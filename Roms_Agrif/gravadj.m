clear all
close all

tndx=3;
nc=netcdf('roms_his.nc');
h=nc{'h'}(:);
x=squeeze(nc{'x_rho'}(2,:));
zeta=squeeze(nc{'zeta'}(tndx,:,:));
t=squeeze(nc{'temp'}(tndx,:,2,:));
[N,M]=size(t);
theta_s=nc.theta_s(:);
theta_b=nc.theta_b(:);
hc=nc.hc(:);
close(nc);

zr = zlevs(h,zeta,theta_s,theta_b,hc,N,'r');
zr=squeeze(zr(:,2,:));
xr=reshape(x,1,M);
xr=repmat(xr,[N 1])/1000;

contourf(xr,zr,t,[-1:0.5:7])
caxis([0 5])
shading flat
colorbar



