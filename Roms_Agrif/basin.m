clear all
close all

tndx=11;
j=25;

nc=netcdf('roms_his.nc');
h=nc{'h'}(:);
x1=nc{'x_rho'}(:);
y1=nc{'y_rho'}(:);
x=squeeze(x1(j,:));
zeta=squeeze(nc{'zeta'}(tndx,:,:));
t=squeeze(nc{'temp'}(tndx,:,j,:));
[N,M]=size(t);
theta_s=nc.theta_s(:);
theta_b=nc.theta_b(:);
hc=nc.hc(:);
close(nc);

zr = zlevs(h,zeta,theta_s,theta_b,hc,N,'r');
zr=squeeze(zr(:,j,:));
xr=reshape(x,1,M);
xr=repmat(xr,[N 1])/1000;

figure(1)
pcolor(xr,zr,t)
shading flat
colorbar

figure(2)
pcolor(x1/1000,y1/1000,zeta)
axis image
shading flat
colorbar
hold on
contour(x1/1000,y1/1000,h,'k')
hold off

