clear all
close all
warning off
%%%%%%%%%%%%%%%%%%%%% USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%%
%
% Directory and file names
%
directory='/home/sandrine/RUN_GG_QSCAT_V2014_01_07/SCRATCH/';
model='roms';
filetype='avg';  % 'his' or 'avg'
Ymin=5;
Ymax=10;
Yorig=nan;
endf='.nc.1';
vtransform=1;
H0=-100; % depth of integration
%
% possiblity to compute KmKe for a given season (1 to 4)
% or for the annual mean (season=5).
% 
season=3;
%
Mmin=1+((season-1)*3);
Mmax=Mmin+2;
if season==5
 Mmin=1;
 Mmax=12;
end
%
% Read the grid
%
fname=[directory,model,'_',filetype,'_Y',num2str(Ymin),'M',num2str(Mmin),endf];
nc=netcdf(fname);
theta_s=nc.theta_s(:);
theta_b=nc.theta_b(:);
hc=nc.hc(:);
pm=nc{'pm'}(:);
pn=nc{'pn'}(:);
h=nc{'h'}(:);
maskr=nc{'mask_rho'}(:);
lon=nc{'lon_rho'}(:);
lat=nc{'lat_rho'}(:);
close(nc)
%
[M,L]=size(pm);
[masku,maskv,maskp]=uvp_mask(maskr);
%
% get the seasonal mean
%
Sname=[directory,model,'_Smean',endf]
nc=netcdf(Sname);
N=length(nc('s_rho'))
zetab=squeeze(nc{'zeta'}(season,:,:));
ub=squeeze(nc{'u'}(season,:,:,:));
vb=squeeze(nc{'v'}(season,:,:,:));
wb=squeeze(nc{'omega'}(season,:,:,:));
close(nc)
%
% gradient of ubar and vbar
%
dudx=zeros(N,M,L);
dudx(:,:,2:end-1)=tridim(pm(:,2:end-1),N).*(ub(:,:,2:end)-ub(:,:,1:end-1));
dudy=zeros(N,M,L);
cff=ub(:,2:end,:)-ub(:,1:end-1,:);
dudy(:,2:end-1,2:end-1)=tridim(pn(2:end-1,2:end-1),N).*0.25.*...
             (cff(:,2:end,2:end)  +cff(:,1:end-1,2:end)+...
              cff(:,2:end,1:end-1)+cff(:,1:end-1,1:end-1));
dvdx=zeros(N,M,L);
cff=vb(:,:,2:end)-vb(:,:,1:end-1);
dvdx(:,2:end-1,2:end-1)=tridim(pm(2:end-1,2:end-1),N).*0.25.*...
             (cff(:,2:end,2:end)  +cff(:,1:end-1,2:end)+...
              cff(:,2:end,1:end-1)+cff(:,1:end-1,1:end-1));
dvdy=zeros(N,M,L);
dvdy(:,2:end-1,:)=tridim(pn(2:end-1,:),N).*(vb(:,2:end,:)-vb(:,1:end-1,:));
%
dn_u=tridim(2./(pn(:,1:end-1)+pn(:,2:end)),N);
dm_v=tridim(2./(pm(1:end-1,:)+pm(2:end,:)),N);
omn_w=tridim(1./(pm.*pn),N+1);
%
% Time loop
%
KmKe=zeros(N,M,L);
uu=zeros(N,M,L);
uv=zeros(N,M,L);
vu=zeros(N,M,L);
vv=zeros(N,M,L);
%
nindex=0;
%
for Y=Ymin:Ymax 
  for M=Mmin:Mmax
    fname=[directory,model,'_',filetype,'_Y',num2str(Y),'M',num2str(M),endf];
    disp(['Opening : ',fname])
    nc=netcdf(fname);
    ntime=length(nc('time'));
%
    for tindex=1:ntime
%    for tindex=1:2
tindex
      zeta=squeeze(nc{'zeta'}(tindex,:,:));
      up=squeeze(nc{'u'}(tindex,:,:,:))-ub;
      vp=squeeze(nc{'v'}(tindex,:,:,:))-vb;
      wp=squeeze(nc{'omega'}(tindex,:,:,:))-wb;
%
% Compute Hz
% 
      zw=zlevs(h,zeta,theta_s,theta_b,hc,N,'w',vtransform);
      Hz=zw(2:N+1,:,:)-zw(1:N,:,:); 
      mnoHz=tridim(pm.*pn,N)./Hz;

%
% Compute the terms
%
      upXgradub=mnoHz.*roms_advection(masku,maskv,maskr,Hz,dn_u.*up,dm_v.*vp,omn_w.*wp,u2rho_3d(ub));
      upXgradvb=mnoHz.*roms_advection(masku,maskv,maskr,Hz,dn_u.*up,dm_v.*vp,omn_w.*wp,v2rho_3d(vb));
%
      up=u2rho_3d(up);
      vp=v2rho_3d(vp);
%
      KmKe=KmKe - up.*upXgradub - vp.*upXgradvb;
      nindex=nindex+1;
%
      uu=uu+up.*up;
      uv=uv+up.*vp;
      vu=vu+vp.*up;
      vv=vv+vp.*vp;
%
    end
%
    close(nc)
  end
end
%
KmKe=KmKe/nindex;
%
uu=uu/nindex;
uv=uv/nindex;
vu=vu/nindex;
vv=vv/nindex;
KmKe2=-(uu.*dudx+uv.*dudy+vu.*dvdx+vv.*dvdy);
%
zr=zlevs(h,zeta,theta_s,theta_b,hc,N,'r',vtransform);
zw=zlevs(h,zeta,theta_s,theta_b,hc,N,'w',vtransform);
mask=maskr;
mask(mask==0)=NaN;
[KmKe2D,h0]=vintegr2(KmKe,zw,zr,H0,0);
[KmKe2D2,h0]=vintegr2(KmKe2,zw,zr,H0,0);

save kmke.mat lon lat mask KmKe KmKe2 zr zw  H0 KmKe2D KmKe2D2


figure(1)
pcolor(lon,lat,mask.*1e6.*KmKe2D)
axis image
shading flat
caxis([-20 20])
colorbar
title('KmKe')

figure(2)
pcolor(lon,lat,mask.*1e6.*KmKe2D2)
axis image
shading flat
caxis([-20 20])
colorbar
title('KmKe2')
