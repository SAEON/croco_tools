 function [Lonr,Latr,pm,pn,ang] = ...
           easy_grid(nx,ny,dl,size_x,size_y,tra_lon,tra_lat,rot);
%
%==================================================================== 
%   Easy grid make an rectangular, orthogonal grid with minimal 
%   gridsize variation. It uses a Mercator projection around the 
%   equator and then rotates the sphere around its 3 axis to position 
%   the grid where it is desired.
%   
%   Inputs:
%   nx:      Number of grid point in the x direction
%   ny:      Number of grid point in the y direction
%   size_x:  Domain size in x-direction
%   size_y:  Domain size in y-direction
%   tra_lon: Desired longitude of grid center
%   tra_lat: Desired latitude of grid center
%   rot:     Rotation of grid direction (0: x direction is west-east)
%
%  (c) 2008, Jeroen Molemaker, UCLA
%      2019, P. Marchesiello, modif. for croco_tools
%==================================================================== 
%
R_earth=6367442.76;   % Earth radius
deg2rad=pi/180;
rad2deg=180/pi;

if rot==0

%
% Get the longitude and latitude as in croco_tools
%
dlon=size_x/(R_earth*cos(tra_lat*deg2rad))*rad2deg;
dlat=size_y/R_earth*rad2deg;
lonmin=tra_lon-0.5*dlon;
lonmax=tra_lon+0.5*dlon;
latmin=tra_lat-0.5*dlat;
latmax=tra_lat+0.5*dlat;
lonr=(lonmin:dl:lonmax);
i=1;
latr(i)=latmin;
while latr(i)<=latmax
  i=i+1;
  latr(i)=latr(i-1)+dl*cos(latr(i-1)*pi/180);
end
[Lonr,Latr]=meshgrid(lonr,latr);
[Lonu,Lonv,Lonp]=rho2uvp(Lonr); 
[Latu,Latv,Latp]=rho2uvp(Latr);

else

%
% Perform Mercator projection around the equator
% before rotation and translation to center lat,lon
%
if (size_y>size_x) 
  length = size_y; nl = ny;
  width  = size_x; nw = nx;
else
  length = size_x; nl = nx;
  width  = size_y; nw = ny;
end
   
dlon = length/R_earth;
lonr = dlon*[-0.5:1:nl+0.5]/nl - dlon/2;  

dlat = width/R_earth;
y1 = log(tan(pi/4-dlat/4));
y2 = log(tan(pi/4+dlat/4));
y = (y2-y1)*[-0.5:1:nw+0.5]/nw + y1;  
latr = 2*atan(exp(y)) - pi/2; 
dlat_cen = 0.5*(latr(round(nw/2)+1)-latr(round(nw/2)-1));
dlon_cen = dlon/nl;
mul = dlat_cen/dlon_cen*length/width*nw/nl;
latr = latr/mul;

[Lonr,Latr] = meshgrid(lonr,latr);
Lonu = 0.5*(Lonr(:,1:end-1)+Lonr(:,2:end));
Latu = 0.5*(Latr(:,1:end-1)+Latr(:,2:end));
Lonv = 0.5*(Lonr(1:end-1,:)+Lonr(2:end,:));
Latv = 0.5*(Latr(1:end-1,:)+Latr(2:end,:));

if (size_y>size_x) 
 [Lonr,Latr] = rot_sphere(Lonr,Latr,90);
 [Lonu,Latu] = rot_sphere(Lonu,Latu,90);
 [Lonv,Latv] = rot_sphere(Lonv,Latv,90);
 Lonr = flipdim(Lonr,1)';
 Latr = flipdim(Latr,1)';
 Lonu_tmp = flipdim(Lonv,1)';
 Latu_tmp = flipdim(Latv,1)';
 Lonv = flipdim(Lonu,1)';
 Latv = flipdim(Latu,1)';
 Lonu = Lonu_tmp;
 Latu = Latu_tmp;
end

[Lonr,Latr] = rot_sphere(Lonr,Latr,rot);
[Lonu,Latu] = rot_sphere(Lonu,Latu,rot);
[Lonv,Latv] = rot_sphere(Lonv,Latv,rot);
  
[Lonr,Latr] = tra_sphere(Lonr,Latr,tra_lat);
[Lonu,Latu] = tra_sphere(Lonu,Latu,tra_lat);
[Lonv,Latv] = tra_sphere(Lonv,Latv,tra_lat);

Lonr = Lonr*rad2deg + tra_lon;
Lonu = Lonu*rad2deg + tra_lon;
Lonv = Lonv*rad2deg + tra_lon;
Latr = Latr*rad2deg;
Latu = Latu*rad2deg;
Latv = Latv*rad2deg;

Lonr(Lonr<-180) = Lonr(Lonr<-180) + 360;
Lonu(Lonu<-180) = Lonu(Lonu<-180) + 360;
Lonv(Lonv<-180) = Lonv(Lonv<-180) + 360;

end % rot==0

%
% Compute pn and pm (as in croco_tools)
%
[Mp,L]=size(Latu);
[M,Lp]=size(Latv);
Lm=L-1;
Mm=M-1;
dx=zeros(Mp,Lp);
dy=zeros(Mp,Lp);
dx(:,2:L)=spheric_dist(Latu(:,1:Lm),Latu(:,2:L),...
                       Lonu(:,1:Lm),Lonu(:,2:L));
dx(:,1)=dx(:,2);
dx(:,Lp)=dx(:,L);

dy(2:M,:)=spheric_dist(Latv(1:Mm,:),Latv(2:M,:),...
                       Lonv(1:Mm,:),Lonv(2:M,:));
dy(1,:)=dy(2,:);
dy(Mp,:)=dy(M,:);

pm=1./dx;
pn=1./dy;
%
% Compute angles of local grid positive x-axis relative to east
% (as in croco_tools)
%
ang=get_angle(Latu,Lonu)*rad2deg;

return

%
%==================================================================== 
%  Rotate sphere around its y-axis
%  Part of Easy Grid
%  (c) 2008, Jeroen Molemaker, UCLA
%==================================================================== 
%
function [lon2,lat2] = rot_sphere(lon1,lat1,rot)


[n,m] = size(lon1);
rot = rot*pi/180;
eps = 1.e-20;
%
% Translate into x,y,z
% conventions:  (lon,lat) = (0,0)  corresponds to (x,y,z) = ( 0,-r, 0)
%               (lon,lat) = (0,90) corresponds to (x,y,z) = ( 0, 0, r)
%
x1 = sin(lon1).*cos(lat1);
y1 = cos(lon1).*cos(lat1);
z1 = sin(lat1);
%
% We will rotate these points around the small circle defined by 
% the intersection of the sphere and the plane that
% is orthogonal to the line through (lon,lat) (0,0) and (180,0)
%
% The rotation is in that plane around its intersection with
% aforementioned line.
%
% Since the plane is orthogonal to the y-axis (in my definition at least),
% Rotations in the plane of the small circle maintain constant y and are around
% (x,y,z) = (0,y1,0)
%
rp1 = sqrt(x1.^2+z1.^2);

ap1 = pi/2*ones(n,m);
ap1(abs(x1)>eps) = atan(abs(z1(abs(x1)>eps)./x1(abs(x1)>eps)));
ap1(x1<0) = pi - ap1(x1<0);
ap1(z1<0) = -ap1(z1<0);

ap2 = ap1 + rot;
x2 = rp1.*cos(ap2);
y2 = y1;
z2 = rp1.*sin(ap2);

%
% transform back from (x,y,z) to (lat,lon)
%
lon2 = pi/2*ones(n,m);
lon2(abs(y2)>eps) = atan(abs(x2(abs(y2)>eps)./y2(abs(y2)>eps)));
lon2(y2<0) = pi - lon2(y2<0);
lon2(x2<0) = -lon2(x2<0);

pr2 = sqrt(x2.^2+y2.^2);
lat2 = pi/2*ones(n,m);
lat2(abs(pr2)>eps) = atan(abs(z2(abs(pr2)>eps)./pr2(abs(pr2)>eps)));
lat2(z2<0) = -lat2(z2<0);

return

%
%==================================================================== 
%  Translate rotated grid
%  Part of easy grid
%  (c) 2008, Jeroen Molemaker, UCLA
%==================================================================== 
%
function [lon2,lat2] = tra_sphere(lon1,lat1,tra)


[n,m] = size(lon1);
tra = tra*pi/180;  %% translation in latitude direction
%
% translate into x,y,z
% conventions:  (lon,lat) = (0,0)  corresponds to (x,y,z) = ( 0,-r, 0)
%               (lon,lat) = (0,90) corresponds to (x,y,z) = ( 0, 0, r)
%
x1 = sin(lon1).*cos(lat1);
y1 = cos(lon1).*cos(lat1);
z1 = sin(lat1);

rp1 = sqrt(y1.^2+z1.^2);

ap1 = pi/2*ones(n,m);
ap1(abs(y1)>1e-5) = atan(abs(z1(abs(y1)>1e-5)./y1(abs(y1)>1e-5)));
ap1(y1<0) = pi - ap1(y1<0);
ap1(z1<0) = -ap1(z1<0);

ap2 = ap1 + tra;
x2 = x1;
y2 = rp1.*cos(ap2);
z2 = rp1.*sin(ap2);
%
% transform back from (x,y,z) to (lat,lon)
%
lon2 = pi/2*ones(n,m);
lon2(abs(y2)>1e-5) = atan(abs(x2(abs(y2)>1e-5)./y2(abs(y2)>1e-5)));
lon2(y2<0) = pi - lon2(y2<0);
lon2(x2<0) = -lon2(x2<0);

pr2 = sqrt(x2.^2+y2.^2);
lat2 = pi/2*ones(n,m);
lat2(abs(pr2)>1e-5) = atan(abs(z2(abs(pr2)>1e-5)./pr2(abs(pr2)>1e-5)));
lat2(z2<0) = -lat2(z2<0);

return
