%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Build a CROCO grid file
%
%  Further Information:  
%  http://www.croco-ocean.org
%  
%  This file is part of CROCOTOOLS
%
%  CROCOTOOLS is free software; you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published
%  by the Free Software Foundation; either version 2 of the License,
%  or (at your option) any later version.
%
%  CROCOTOOLS is distributed in the hope that it will be useful, but
%  WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program; if not, write to the Free Software
%  Foundation, Inc., 59 Temple Place, Suite 330, Boston,
%  MA  02111-1307  USA
%
%  Copyright (c) 2002-2006 by Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr  
%
%  Contributions of P. Marchesiello (IRD) and X. Capet (UCLA)
%
%  Updated    Aug-2006 by Pierrick Penven
%  Updated    24-Oct-2006 by Pierrick Penven (mask correction)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all
close all
%%%%%%%%%%%%%%%%%%%%% USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%%
%
crocotools_param
% WRF grid file used to create croco grid (input)
wrf_file=[RUN_dir,'/WRF_FILES/WPS_DATA/geo_em.d01.nc'];
% CROCO grid filen (output)
croco_file=[RUN_dir, '/CROCO_FILES/croco_grd.nc'];
% Refinement coefficient:
%  coef=1: CROCO grid is similar to WRF grid
%  coef=N: Each WRF cell is divided into N CROCO cells in x and y
%  Only works with odd number; otherwise rho/M points can not match
coef=1
% number of points to remove close to the boundary to avoid WRF 'sponge'
nbdy=0
%
%%%%%%%%%%%%%%%%%%% END USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%
%
warning off

if rem(coef,2)==0
  error('To have a perfect match coefficient need to be an odd number') 
end
%
% Title
%
disp(' ')
disp([' Origin grid: ', wrf_file])
disp(' ')
disp([' Making the grid: ',croco_file])
disp(' ')
disp([' Title: ',CROCO_title])
disp(' ')
disp([' Refine coefficient from WRF is : ',num2str(coef)])

%
% Get the Longitude
%
nc=netcdf(wrf_file);
Lonp0=squeeze(nc{'XLONG_C'}(1,:,:));
Lonr0=squeeze(nc{'XLONG_M'}(1,:,:));
Latp0=squeeze(nc{'XLAT_C'}(1,:,:));
Latr0=squeeze(nc{'XLAT_M'}(1,:,:));
Lonp0(Lonp0<0)=Lonp0(Lonp0<0)+360;
Lonr0(Lonr0<0)=Lonr0(Lonr0<0)+360;
close(nc)

Lonp=zeros((coef*(size(Lonp0,1)-(nbdy+1)*2)-(coef-1)),(coef*(size(Lonp0,2)-(nbdy+1)*2)-(coef-1)));
dx = (Lonp0(1,2+nbdy:end-1-nbdy)-Lonp0(1,1+nbdy:end-2-nbdy))/coef;

for n = 1:size(Lonp,1)
    Lonp(n,1:coef:end)=Lonp0(1,2+nbdy:end-1-nbdy);
end

cpt=1;
for j = 1:coef:(size(Lonp,2)-2)
    for n = 1:coef-1
        Lonp(:,j+n)=Lonp(:,j)+n*dx(cpt);
    end
    cpt=cpt+1;
end

%
%
Latp=zeros((coef*(size(Latp0,1)-(nbdy+1)*2)-(coef-1)),(coef*(size(Latp0,2)-(nbdy+1)*2)-(coef-1)));

dy=(Latp0(2+nbdy:end-1-nbdy,1)-Latp0(1+nbdy:end-2-nbdy,1))/coef;
for n = 1:size(Latp,2)
    Latp(1:coef:end,n)=Latp0(2+nbdy:end-1-nbdy,1);
end

cpt=1;
for j = 1:coef:(size(Latp,1)-2)
    for n = 1:coef-1
        Latp(j+n,:)=Latp(j,:)+n*dy(cpt);
    end
    cpt=cpt+1;
end
%
% Make rho-grid
%

Lonr=zeros([size(Lonp,1)+1,size(Lonp,2)+1]);
Latr=zeros([size(Latp,1)+1,size(Latp,2)+1]);

[M,L]=size(Lonp);
Mp=M+1;
Lp=L+1;
Mm=M-1;
Lm=L-1;

Lonr(2:M,2:L)=0.25*(Lonp(1:Mm,1:Lm)+Lonp(1:Mm,2:L)+Lonp(2:M,1:Lm)+Lonp(2:M,2:L));
Lonr(1,:)=Lonr(2,:);
Lonr(Mp,:)=Lonr(M,:);
Lonr(:,1)=Lonr(:,2)-dx(1);
Lonr(:,Lp)=Lonr(:,L)+dx(end);

Latr(2:M,2:L)=0.25*(Latp(1:Mm,1:Lm)+Latp(1:Mm,2:L)+Latp(2:M,1:Lm)+Latp(2:M,2:L));
Latr(1,:)=Latr(2,:)-dy(1);
Latr(Mp,:)=Latr(M,:)+dy(end);
Latr(:,1)=Latr(:,2);
Latr(:,Lp)=Latr(:,L);

Lonu=rho2u_2d(Lonr);
Lonv=rho2v_2d(Lonr);
Latu=rho2u_2d(Latr);
Latv=rho2v_2d(Latr);


%
% Create the grid file
%
disp(' ')
disp(' Create the grid file...')
[M,L]=size(Latp);
disp([' LLm = ',num2str(L-1)])
disp([' MMm = ',num2str(M-1)])
create_grid(L,M,croco_file,CROCO_title)
%
% Fill the grid file
%
disp(' ')
disp(' Fill the grid file...')
nc=netcdf(croco_file,'write');
nc{'lat_u'}(:)=Latu;
nc{'lon_u'}(:)=Lonu;
nc{'lat_v'}(:)=Latv;
nc{'lon_v'}(:)=Lonv;
nc{'lat_rho'}(:)=Latr;
nc{'lon_rho'}(:)=Lonr;
nc{'lat_psi'}(:)=Latp;
nc{'lon_psi'}(:)=Lonp;
close(nc)
disp(' ')
disp(' Compute the metrics...')
[pm,pn,dndx,dmde]=get_metrics(croco_file);
xr=0.*pm;
yr=xr;
for i=1:L
  xr(:,i+1)=xr(:,i)+2./(pm(:,i+1)+pm(:,i));
end
for j=1:M
  yr(j+1,:)=yr(j,:)+2./(pn(j+1,:)+pn(j,:));
end
[xu,xv,xp]=rho2uvp(xr);
[yu,yv,yp]=rho2uvp(yr);
dx=1./pm;
dy=1./pn;
dxmax=max(max(dx/1000));
dxmin=min(min(dx/1000));
dymax=max(max(dy/1000));
dymin=min(min(dy/1000));
disp(' ')
disp([' Min dx=',num2str(dxmin),' km - Max dx=',num2str(dxmax),' km'])
disp([' Min dy=',num2str(dymin),' km - Max dy=',num2str(dymax),' km'])
%
%  Angle between XI-axis and the direction
%  to the EAST at RHO-points [radians].
%
angle=get_angle(Latu,Lonu);
%
%  Coriolis parameter
%
f=4*pi*sin(pi*Latr/180)/(24*3600);
%
% Fill the grid file
%
disp(' ')
disp(' Fill the grid file...')
nc=netcdf(croco_file,'write');
nc{'pm'}(:)=pm;
nc{'pn'}(:)=pn;
nc{'dndx'}(:)=dndx;
nc{'dmde'}(:)=dmde;
nc{'x_u'}(:)=xu;
nc{'y_u'}(:)=yu;
nc{'x_v'}(:)=xv;
nc{'y_v'}(:)=yv;
nc{'x_rho'}(:)=xr;
nc{'y_rho'}(:)=yr;
nc{'x_psi'}(:)=xp;
nc{'y_psi'}(:)=yp;
nc{'angle'}(:)=angle;
nc{'f'}(:)=f;
nc{'spherical'}(:)='T';
close(nc);
%
%
%  Add topography from topofile
%
disp(' ')
disp(' Add topography...')
h=add_topo(croco_file,topofile);

%
% Compute the mask
%
%nc=netcdf(wrf_file);
%maskr0=squeeze(nc{'LANDMASK'}(1,:,:));

%maskr=zeros(size(Lonr,1),size(Lonr,2));
%size(maskr)
%size(maskr0)
%[Mp,Lp]=size(maskr0);
%[igrd_r,jgrd_r]=meshgrid((3:1:Lp+2),(3:1:Mp+2));
%irchild=(1:1/coef:Lp+3);
%jrchild=(1:1/coef:Mp+3);
%[ichildgrd_r,jchildgrd_r]=meshgrid(irchild,jrchild);
%maskr_coarse=interp2(igrd_r,jgrd_r,maskr0,ichildgrd_r,jchildgrd_r,'nearest');
%maskr_coarse(1:10,4)
% To avoid some problem at the boundaries
%maskr_coarse(1,:)=maskr_coarse(2,:);
%maskr_coarse(end,:)=maskr_coarse(end-1,:);
%maskr_coarse(:,1)=maskr_coarse(:,2);
%maskr_coarse(:,end)=maskr_coarse(:,end-1);
%
%maskr(2:end-1,2:end-1)=get_embeddedmask(maskr_coarse,h(2:end-1,2:end-1),coef,0);


%maskr(:,1)=maskr(:,2);
%maskr(:,end)=maskr(:,end-1);

%maskr(1,:)=maskr(2,:);
%maskr(end,:)=maskr(end-1,:);
%maskr2=maskr;

%maskr(maskr2==0)=1;
%maskr(maskr2==1)=0;
%clear mask
%clone(nc)


maskr=h>0;
maskr=process_mask(maskr);
%
[masku,maskv,maskp]=uvp_mask(maskr);
%
%  Write it down
%
nc=netcdf(croco_file,'write');
nc{'h'}(:)=h;
nc{'mask_u'}(:)=masku;
nc{'mask_v'}(:)=maskv;
nc{'mask_psi'}(:)=maskp;
nc{'mask_rho'}(:)=maskr;
close(nc);
%
% Create the coastline
%
if ~isempty(coastfileplot)
  make_coast(croco_file,coastfileplot);
end
%
r=input('Do you want to use editmask ? y,[n]','s');
if strcmp(r,'y')
  disp(' Editmask:')
  disp(' Edit manually the land mask.')
  disp(' Press enter when finished.')
  disp(' ')
  if ~isempty(coastfileplot)
    editmask(croco_file,coastfilemask)
  else
    editmask(croco_grd)
  end
  r=input(' Finished with edit mask ? [press enter when finished]','s');
end
%
close all
%
%  Smooth the topography
%
nc=netcdf(croco_file,'write');
h=nc{'h'}(:);
maskr=nc{'mask_rho'}(:);
%
h=smoothgrid(h,maskr,hmin,hmax_coast,hmax,...
                        rtarget,n_filter_deep_topo,n_filter_final);
%
%  Write it down
%
disp(' ')
disp(' Write it down...')
nc{'h'}(:)=h;
close(nc);
%
% make a plot
%
if makeplot==1
  disp(' ')
  disp(' Do a plot...')
  themask=ones(size(maskr));
  themask(maskr==0)=NaN;
  domaxis=[min(min(Lonr)) max(max(Lonr)) min(min(Latr)) max(max(Latr))];
  colaxis=[min(min(h)) max(max(h))];
  fixcolorbar([0.25 0.05 0.5 0.03],colaxis,...
              'Topography',10)
  width=1;
  height=0.8;
  subplot('position',[0. 0.14 width height])
  m_proj('mercator',...
         'lon',[domaxis(1) domaxis(2)],...
         'lat',[domaxis(3) domaxis(4)]);
  m_pcolor(Lonr,Latr,h.*themask);
  shading flat
  caxis(colaxis)
  hold on
  [C1,h1]=m_contour(Lonr,Latr,h,[hmin 100 200 500 1000 2000 4000],'k');
  clabel(C1,h1,'LabelSpacing',1000,'Rotation',0,'Color','r')
  if ~isempty(coastfileplot)
    m_usercoast(coastfileplot,'color','r');
    %m_usercoast(coastfileplot,'speckle','color','r');
  else
    m_gshhs_l('color','r');
    m_gshhs_l('speckle','color','r');
  end
  m_grid('box','fancy',...
         'xtick',5,'ytick',5,'tickdir','out',...
         'fontsize',7);
  hold off
end

