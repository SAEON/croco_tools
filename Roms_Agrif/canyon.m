clear all
close all

tndx=3;
i=32;

nc=netcdf('roms_his.nc');
h=nc{'h'}(:);
x1=nc{'x_rho'}(:);
y1=nc{'y_rho'}(:);
y=squeeze(y1(:,i));
zeta=squeeze(nc{'zeta'}(tndx,:,:));
t=squeeze(nc{'rho'}(tndx,:,:,i));
[N,M]=size(t);
sst=squeeze(nc{'temp'}(tndx,N,:,:));
u=squeeze(nc{'u'}(tndx,N,:,:));
v=squeeze(nc{'v'}(tndx,N,:,:));
theta_s=nc.theta_s(:);
theta_b=nc.theta_b(:);
hc=nc.hc(:);
close(nc);

zr = zlevs(h,zeta,theta_s,theta_b,hc,N,'r');
zr=squeeze(zr(:,:,i));
yr=reshape(y,1,M);
yr=repmat(yr,[N 1])/1000;

figure(1)
pcolor(yr,zr,t)
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

