function h=add_topo(grdname,toponame)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% add a topography (here etopo2) to a ROMS grid
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  read grid
%
nc=netcdf(grdname);
lon=nc{'lon_rho'}(:);
lat=nc{'lat_rho'}(:);
pm=nc{'pm'}(:);
pn=nc{'pn'}(:);
result=close(nc);
%
dl=1;
lonmin=min(min(lon))-dl;
lonmax=max(max(lon))+dl;
latmin=min(min(lat))-dl;
latmax=max(max(lat))+dl;
%
%  open the topo file
%
nc=netcdf(toponame);
tlon=nc{'lon'}(:);
tlat=nc{'lat'}(:);
%
%  get a subgrid
%
j=find(tlat>=latmin & tlat<=latmax);
i1=find(tlon-360>=lonmin & tlon-360<=lonmax);
i2=find(tlon>=lonmin & tlon<=lonmax);
i3=find(tlon+360>=lonmin & tlon+360<=lonmax);
x=cat(1,tlon(i1)-360,tlon(i2),tlon(i3)+360);
y=tlat(j);
%
%  Read data
%
if ~isempty(i2)
  topo=-nc{'topo'}(j,i2);
else
  topo=[];
end
if ~isempty(i1)
  topo=cat(2,-nc{'topo'}(j,i1),topo);
end
if ~isempty(i3)
  topo=cat(2,topo,-nc{'topo'}(j,i3));
end
result=close(nc);
%
% Get ROMS mean resolution
%
dx=mean(mean(1./pm));
dy=mean(mean(1./pn));
dx_roms=mean([dx dy]);
disp(['ROMS resolution : ',num2str(dx_roms/1000,2),' km'])
%
% Get TOPO mean resolution
%
R=6367442.76;
deg2rad=pi/180;
dg=mean(x(2:end)-x(1:end-1));
dphi=y(2:end)-y(1:end-1);
dy=R*deg2rad*dphi;
dx=R*deg2rad*dg*cos(deg2rad*y);
dx_topo=mean([dx ;dy]);
disp(['Topography resolution : ',num2str(dx_topo/1000,2),' km'])
%
% Degrade TOPO resolution
%
n=0;
%while dx_roms>(2*dx_topo)
while dx_roms>(dx_topo)
  n=n+1;
%  
  x=0.5*(x(2:end)+x(1:end-1));
  x=x(1:2:end);
  y=0.5*(y(2:end)+y(1:end-1));
  y=y(1:2:end);
%
  topo=0.25*(topo(2:end,1:end-1)  +topo(2:end,2:end)+...
             topo(1:end-1,1:end-1)+topo(1:end-1,2:end));
  topo=topo(1:2:end,1:2:end);   
%  
  dg=mean(x(2:end)-x(1:end-1));
  dphi=y(2:end)-y(1:end-1);
  dy=R*deg2rad*dphi;
  dx=R*deg2rad*dg*cos(deg2rad*y);
  dx_topo=mean([dx ;dy]);
end
disp(['Topography resolution halved ',num2str(n),' times'])
disp(['Topography resolution : ',num2str(dx_topo/1000,2),' km'])
%
%  interpole topo
%
h=interp2(x,y,topo,lon,lat,'cubic');
return
