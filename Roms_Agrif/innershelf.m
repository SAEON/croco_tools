clear all
close all

tndx=31;
nc=netcdf('roms_his.nc');
h=nc{'h'}(:);
x=squeeze(nc{'x_rho'}(2,:));
zeta=squeeze(nc{'zeta'}(tndx,:,:));
t=squeeze(nc{'temp'}(tndx,:,2,:));
[N,L]=size(t);
u=squeeze(nc{'u'}(tndx,:,2,:));
v=squeeze(nc{'v'}(tndx,:,2,:));
w=squeeze(nc{'w'}(tndx,:,2,:));
theta_s=nc.theta_s(:);
theta_b=nc.theta_b(:);
hc=nc.hc(:);
close(nc);
zr = zlevs(h,zeta,theta_s,theta_b,hc,N,'r');
zr=squeeze(zr(:,1,:));
zw = zlevs(h,zeta,theta_s,theta_b,hc,N,'w');
zw=squeeze(zw(:,1,:));

xr=reshape(x,1,L);
xr=repmat(xr,[N 1])/1000;
xw=reshape(x,1,L);
xw=repmat(xw,[N+1 1])/1000;

pcolor(xr,zr,t)
shading interp
colorbar

figure
pcolor(xr,zr,w)
shading interp
colorbar


