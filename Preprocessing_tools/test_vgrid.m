%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Test the vertical grid
%
%
%  Pierrick Penven, IRD, 2002.
%
%  Version of 2-Oct-2002
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all
close all
%%%%%%%%%%%%%%%%%%%%% USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%%
%
romstools_param
%
%
%%%%%%%%%%%%%%%%%%% END USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%
%
% Read the grid
%
nc=netcdf(grdname);
h=nc{'h'}(:);
mask=nc{'mask_rho'}(:);
lon=nc{'lon_rho'}(:);
lat=nc{'lat_rho'}(:);
close(nc)
%
% Get the vertical levels
%
hmin=min(min(h(mask==1)));
hmax=max(max(h(mask==1)));
disp(['hmin = ',num2str(hmin)])
if hc > hmin
  error([' hc (',num2str(hc),' m) > hmin (',num2str(hmin),' m)'])
end
zw=zlevs(h,0.*h,theta_s,theta_b,hc,N,'w');
dz=zw(2:end,:,:)-zw(1:end-1,:,:);
dzsurf(:,:)=dz(end,:,:);
dzbot(:,:)=dz(1,:,:);
disp(['Surface : minimum = ',num2str(min(min(dzsurf(mask==1)))),...
      ' - maximum = ',num2str(max(max(dzsurf(mask==1))))])
disp(['Bottom : minimum = ',num2str(min(min(dzbot(mask==1)))),...
      ' - maximum = ',num2str(max(max(dzbot(mask==1))))])
%
% Make  a plot
%
npts=20;
hmax=1000;
h=(hmin:(hmax-hmin)/(npts-1):hmax);
x=(1:npts);
z=(1:N);
[xr,zr]=meshgrid(x,z);
zr=squeeze(zlevs(h,0.*h,theta_s,theta_b,hc,N,'r'));
zw=squeeze(zlevs(h,0.*h,theta_s,theta_b,hc,N,'w'));
dz=zw(2:end,:)-zw(1:end-1,:);
pcolor(xr,zr,dz)
colorbar
%dz(end,end)
