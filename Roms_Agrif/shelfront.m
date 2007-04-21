clear all
close all

tndx=11;
nc=netcdf('roms_his.nc');
h=nc{'h'}(:);
y=squeeze(nc{'y_rho'}(:,2));
zeta=squeeze(nc{'zeta'}(tndx,:,:));
t=squeeze(nc{'temp'}(tndx,:,:,2));
[N,M]=size(t);
theta_s=nc.theta_s(:);
theta_b=nc.theta_b(:);
hc=nc.hc(:);
close(nc);

zr = zlevs(h,zeta,theta_s,theta_b,hc,N,'r');
zr=squeeze(zr(:,:,1));
yr=reshape(y,1,M);
yr=repmat(yr,[N 1])/1000;

contourf(yr,zr,t,(12:0.5:18))
caxis([12 18])
shading flat
colorbar



