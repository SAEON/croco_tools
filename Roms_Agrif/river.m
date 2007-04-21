clear all
close all

clear M

nc=netcdf('roms_his.nc');
tis=nc{'scrum_time'}(:);
h=nc{'h'}(:);

tmin=1; tmax=length(tis);
N=20;
vname='salt';

for tndx=tmin:tmax;
s=squeeze(nc{vname}(tndx,N,:,:));
s(s==0)=NaN;
u=squeeze(nc{'u'}(tndx,N,:,:));
v=squeeze(nc{'v'}(tndx,N,:,:));
pcolor(s); hold on;
quiver(u2rho_2d(u),v2rho_2d(v),4);
axis image

%caxis([26 36])
shading flat
hold on
%contour(h(2:end-1,2:end-1),'k')
colorbar('v')
hold off

speed=max(max(sqrt(u2rho_2d(u).^2+v2rho_2d(v).^2)));

M(tndx) = getframe;
end;
close(nc);
movie(M,1,1);


return

nc=netcdf('roms_his.nc');
s=squeeze(nc{vname}(tndx,:,:,:));
s(s==0)=NaN;
maxT=max(max(max(s)));
minT=min(min(min(s)));
disp(['Variable: ',vname,'  min = ',num2str(minT),'  -  max = ',num2str(maxT)])

figure
u=squeeze(nc{'u'}(tndx,:,41,:));
pcolor(u);colorbar;
figure
w=squeeze(nc{'w'}(tndx,:,41,:));
pcolor(w);colorbar;
figure
v=squeeze(nc{'v'}(tndx,:,:,4));
pcolor(v);colorbar;
figure
zeta=squeeze(nc{'zeta'}(tndx,41,:));
plot(zeta);


