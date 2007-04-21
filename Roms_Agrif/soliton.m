clear all
close all

tndx=16;
nc=netcdf('roms_his.nc');
x=nc{'x_rho'}(:);
y=nc{'y_rho'}(:);
zeta=squeeze(nc{'zeta'}(tndx,:,:));
z1=squeeze(nc{'zeta'}(1,:,:));
close(nc);


pcolor(x,y,zeta)
axis image
shading interp
hold on
contour(x,y,z1,'k')
hold off

