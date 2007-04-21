clear all
close all

tndx=1;
nc=netcdf('roms_avg.nc');
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

pcolor(yr,zr,t)
shading flat
colorbar



