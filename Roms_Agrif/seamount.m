clear all
close all

tndx=6;
N=1;
nc=netcdf('roms_his.nc');
h=nc{'h'}(:);
u=squeeze(nc{'u'}(tndx,N,:,:));
v=squeeze(nc{'v'}(tndx,N,:,:));
close(nc);
spd=sqrt((0.5*(u(2:end-1,1:end-1)+u(2:end-1,2:end))).^2+...
         (0.5*(v(1:end-1,2:end-1)+v(2:end,2:end-1))).^2);
pcolor(spd)
axis image
shading interp
hold on
contour(h(2:end-1,2:end-1),'k')
colorbar
hold off


