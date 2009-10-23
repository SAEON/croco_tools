function interp_OGCM(OGCM_dir,OGCM_prefix,year,month,Roa,interp_method,...
                     lonU,latU,lonV,latV,lonT,latT,Z,tin,...
		     nc_clm,nc_bry,lon,lat,angle,h,tout)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%
% Read the local OGCM files and perform the interpolations
%
% Ok, I am lazy and I did not do something special for the bry files...
%
%
%  Further Information:  
%  http://www.brest.ird.fr/Roms_tools/
%  
%  This file is part of ROMSTOOLS
%
%  ROMSTOOLS is free software; you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published
%  by the Free Software Foundation; either version 2 of the License,
%  or (at your option) any later version.
%
%  ROMSTOOLS is distributed in the hope that it will be useful, but
%  WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program; if not, write to the Free Software
%  Foundation, Inc., 59 Temple Place, Suite 330, Boston,
%  MA  02111-1307  USA
%
%  Copyright (c) 2005-2006 by Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr 
%
%  Updated    6-Sep-2006 by Pierrick Penven : Nothing special for the bry file 
%  Update    13-Sep-2009 by Gildas Cambon :   Begin treatments case  for the bry
%  file, no to be continued ...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
conserv=1; % same barotropic velocities as the OGCM
%
disp(['  Horizontal interpolation: ',...
      OGCM_prefix,'Y',num2str(year),'M',num2str(month),'.cdf'])
tic
%
%
% ROMS grid angle
%
cosa=cos(angle);
sina=sin(angle);
%
% Open the OGCM file
%
nc=netcdf([OGCM_dir,OGCM_prefix,'Y',num2str(year),'M',num2str(month),'.cdf']);
%
% Interpole data on the OGCM Z grid and ROMS horizontal grid
%
%
% Read and extrapole the 2D variables
%
zeta=ext_data_OGCM(nc,lonT,latT,'ssh',tin,lon,lat,1,Roa,interp_method);
u2d=ext_data_OGCM(nc,lonU,latU,'ubar',tin,lon,lat,1,Roa,interp_method);
v2d=ext_data_OGCM(nc,lonV,latV,'vbar',tin,lon,lat,1,Roa,interp_method);
ubar=rho2u_2d(u2d.*cosa+v2d.*sina);
vbar=rho2v_2d(v2d.*cosa-u2d.*sina);
%
% Read and extrapole the 3D variables
%
NZ=length(Z);
[M,L]=size(lon);
dz=gradient(Z);
temp=zeros(NZ,M,L);
salt=zeros(NZ,M,L);
u=zeros(NZ,M,L-1);
v=zeros(NZ,M-1,L);
for k=1:NZ
  if rem(k,10)==0
    disp(['  Level ',num2str(k),' of ',num2str(NZ)])
  end
  u2d=ext_data_OGCM(nc,lonU,latU,'u',tin,lon,lat,...
                    k,Roa,interp_method);
  v2d=ext_data_OGCM(nc,lonV,latV,'v',tin,lon,lat,...
                    k,Roa,interp_method);
  u(k,:,:)=rho2u_2d(u2d.*cosa+v2d.*sina);
  v(k,:,:)=rho2v_2d(v2d.*cosa-u2d.*sina);
  temp(k,:,:)=ext_data_OGCM(nc,lonT,latT,'temp',tin,lon,lat,...
                            k,Roa,interp_method);
  salt(k,:,:)=ext_data_OGCM(nc,lonT,latT,'salt',tin,lon,lat,...
                            k,Roa,interp_method);
end
%
% Close the OGCM file
%
close(nc)
%
%Initialisation in case of bry files
%
if ~isempty(nc_bry)
 for obcndx=1:4
  if obcndx==1
  zeta_south=squeeze(zeta(1,:));
  ubar_south=squeeze(ubar(1,:));
  vbar_south=squeeze(vbar(1,:));
  u_south=squeeze(u(:,1,:));
  v_south=squeeze(v(:,1,:));
  temp_south=squeeze(temp(:,1,:));
  salt_south=squeeze(salt(:,1,:));
  
  end

  if obcndx==2
  zeta_east=squeeze(zeta(:,end));
  ubar_east=squeeze(ubar(:,end));
  vbar_east=squeeze(vbar(:,end));
  u_east=squeeze(u(:,:,end));
  v_east=squeeze(v(:,:,end));
  temp_east=squeeze(temp(:,:,end));
  salt_east=squeeze(salt(:,:,end));
  end

  if obcndx==3
  zeta_north=squeeze(zeta(end,:));
  ubar_north=squeeze(ubar(end,:));
  vbar_north=squeeze(vbar(end,:));
  u_north=squeeze(u(:,end,:));
  v_north=squeeze(v(:,end,:));
  temp_north=squeeze(temp(:,end,:));
  salt_north=squeeze(salt(:,end,:));
  end

  if obcndx==4
  zeta_west=squeeze(zeta(:,1));
  ubar_west=squeeze(ubar(:,1));
  vbar_west=squeeze(vbar(:,1));
  u_west=squeeze(u(:,:,1));
  v_west=squeeze(v(:,:,1));
  temp_west=squeeze(temp(:,:,1));
  salt_west=squeeze(salt(:,:,1));
  end
 end %for
end %if

%
% Get the ROMS vertical grid
%
disp('  Vertical interpolations')
if ~isempty(nc_clm)
  theta_s=nc_clm{'theta_s'}(:);
  theta_b=nc_clm{'theta_b'}(:);
  hc=nc_clm{'hc'}(:);
  N=length(nc_clm('s_rho'));
end
if ~isempty(nc_bry)
  theta_s=nc_bry{'theta_s'}(:);
  theta_b=nc_bry{'theta_b'}(:);
  hc=nc_bry{'hc'}(:);
  N=length(nc_bry('s_rho'));
end
%
% Vertical interpolation in case of clim file
%

if ~isempty(nc_clm)
zr=zlevs(h,zeta,theta_s,theta_b,hc,N,'r');
zu=rho2u_3d(zr);
zv=rho2v_3d(zr);
zw=zlevs(h,zeta,theta_s,theta_b,hc,N,'w');
dzr=zw(2:end,:,:)-zw(1:end-1,:,:);
dzu=rho2u_3d(dzr);
dzv=rho2v_3d(dzr);
%
% Add an extra bottom layer (-100000m) and an extra surface layer (+100m)
% to prevent vertical extrapolations
%
Z=[100;Z;-100000];
u=cat(1,u(1,:,:),u);
u=cat(1,u,u(end,:,:));
v=cat(1,v(1,:,:),v);
v=cat(1,v,v(end,:,:));
temp=cat(1,temp(1,:,:),temp);
temp=cat(1,temp,temp(end,:,:));
salt=cat(1,salt,salt(end,:,:));
salt=cat(1,salt(1,:,:),salt);
% 
% Perform the vertical interpolations 
%
u=ztosigma(flipdim(u,1),zu,flipud(Z));
v=ztosigma(flipdim(v,1),zv,flipud(Z));
temp=ztosigma(flipdim(temp,1),zr,flipud(Z));
salt=ztosigma(flipdim(salt,1),zr,flipud(Z));
%
% Correct the horizontal transport 
% i.e. remove the interpolated tranport and add 
%      the OGCM transport
%
if conserv==1
  u=u-tridim(squeeze(sum(u.*dzu)./sum(dzu)),N);
  v=v-tridim(squeeze(sum(v.*dzv)./sum(dzv)),N);
  u=u+tridim(ubar,N);
  v=v+tridim(vbar,N);
end
%
% Barotropic velocities
%
ubar=squeeze(sum(u.*dzu)./sum(dzu));
vbar=squeeze(sum(v.*dzv)./sum(dzv));
end   %~isempty(nc_clm)


%
% Vertical interpolation in case of bry files
%

if ~isempty(nc_bry)

zr=zlevs(h,zeta,theta_s,theta_b,hc,N,'r');
zu=rho2u_3d(zr);
zv=rho2v_3d(zr);
zw=zlevs(h,zeta,theta_s,theta_b,hc,N,'w');
dzr=zw(2:end,:,:)-zw(1:end-1,:,:);
dzu=rho2u_3d(dzr);
dzv=rho2v_3d(dzr);

%
zr_south=squeeze(zr(:,1,:));
zr_east=squeeze(zr(:,:,end));
zr_north=squeeze(zr(:,end,:));
zr_west=squeeze(zr(:,:,1));
%
zu_south=squeeze(zu(:,1,:));
zu_east=squeeze(zu(:,:,end));
zu_north=squeeze(zu(:,end,:));
zu_west=squeeze(zu(:,:,1));
%
zv_south=squeeze(zv(:,1,:));
zv_east=squeeze(zv(:,:,end));
zv_north=squeeze(zv(:,end,:));
zv_west=squeeze(zv(:,:,1));
%
zw_south=squeeze(zw(:,1,:));
zw_east=squeeze(zw(:,:,end));
zw_north=squeeze(zw(:,end,:));
zw_east=squeeze(zw(:,:,1));
%
dzr_south=squeeze(dzr(:,1,:));
dzr_east=squeeze(dzr(:,:,end));
dzr_north=squeeze(dzr(:,end,:));
dzr_west=squeeze(dzr(:,:,1));
%
dzu_south=squeeze(dzu(:,1,:));
dzu_east=squeeze(dzu(:,:,end));
dzu_north=squeeze(dzu(:,end,:));
dzu_west=squeeze(dzu(:,:,1));
%
dzv_south=squeeze(dzv(:,1,:));
dzv_east=squeeze(dzv(:,:,end));
dzv_north=squeeze(dzv(:,end,:));
dzv_west=squeeze(dzv(:,:,1));
%


%
% Add an extra bottom layer (-100000m) and an extra surface layer (+100m)
% to prevent vertical extrapolations but for bry files
%
Z=[100;Z;-100000];

%South
u_south=cat(1,u_south(1,:),u_south);
u_south=cat(1,u_south,u_south(end,:));
v_south=cat(1,v_south(1,:),v_south);
v_south=cat(1,v_south,v_south(end,:));
temp_south=cat(1,temp_south(1,:),temp_south);
temp_south=cat(1,temp_south,temp_south(end,:));
salt_south=cat(1,salt_south,salt_south(end,:));
salt_south=cat(1,salt_south(1,:),salt_south);

%East
u_east=cat(1,u_east(1,:),u_east);
u_east=cat(1,u_east,u_east(end,:));
v_east=cat(1,v_east(1,:),v_east);
v_east=cat(1,v_east,v_east(end,:));
temp_east=cat(1,temp_east(1,:),temp_east);
temp_east=cat(1,temp_east,temp_east(end,:));
salt_east=cat(1,salt_east,salt_east(end,:));
salt_east=cat(1,salt_east(1,:),salt_east);

%North
u_north=cat(1,u_north(1,:),u_north);
u_north=cat(1,u_north,u_north(end,:));
v_north=cat(1,v_north(1,:),v_north);
v_north=cat(1,v_north,v_north(end,:));
temp_north=cat(1,temp_north(1,:),temp_north);
temp_north=cat(1,temp_north,temp_north(end,:));
salt_north=cat(1,salt_north,salt_north(end,:));
salt_north=cat(1,salt_north(1,:),salt_north);

%West
u_west=cat(1,u_west(1,:),u_west);
u_west=cat(1,u_west,u_west(end,:));
v_west=cat(1,v_west(1,:),v_west);
v_west=cat(1,v_west,v_west(end,:));
temp_west=cat(1,temp_west(1,:),temp_west);
temp_west=cat(1,temp_west,temp_west(end,:));
salt_west=cat(1,salt_west,salt_west(end,:));
salt_west=cat(1,salt_west(1,:),salt_west);


%
% Perform the vertical interpolations for bry files
%

u_south=squeeze(ztosigma_1d(flipdim(u_south,1),zu_south,flipud(Z)));
u_east=squeeze(ztosigma_1d(flipdim(u_east,1),zu_east,flipud(Z)));
u_north=squeeze(ztosigma_1d(flipdim(u_north,1),zu_north,flipud(Z)));
u_west=squeeze(ztosigma_1d(flipdim(u_west,1),zu_west,flipud(Z)));
%
v_south=squeeze(ztosigma_1d(flipdim(v_south,1),zv_south,flipud(Z)));
v_east=squeeze(ztosigma_1d(flipdim(v_east,1),zv_east,flipud(Z)));
v_north=squeeze(ztosigma_1d(flipdim(v_north,1),zv_north,flipud(Z)));
v_west=squeeze(ztosigma_1d(flipdim(v_west,1),zv_west,flipud(Z)));
%
temp_south=squeeze(ztosigma_1d(flipdim(temp_south,1),zr_south,flipud(Z)));
temp_east=squeeze(ztosigma_1d(flipdim(temp_east,1),zr_east,flipud(Z)));
temp_north=squeeze(ztosigma_1d(flipdim(temp_north,1),zr_north,flipud(Z)));
temp_west=squeeze(ztosigma_1d(flipdim(temp_west,1),zr_west,flipud(Z)));
%
salt_south=squeeze(ztosigma_1d(flipdim(salt_south,1),zr_south,flipud(Z)));
salt_east=squeeze(ztosigma_1d(flipdim(salt_east,1),zr_east,flipud(Z)));
salt_north=squeeze(ztosigma_1d(flipdim(salt_north,1),zr_north,flipud(Z)));
salt_west=squeeze(ztosigma_1d(flipdim(salt_west,1),zr_west,flipud(Z)));
%

%
% Correct the horizontal transport 
% i.e. remove the interpolated tranport and add 
% the OGCM transport for bry files
%
if conserv==1

u_south=u_south-squeeze(tridim(squeeze(sum(u_south.*dzu_south)./sum(dzu_south)),N));
u_east=u_east-squeeze(tridim(squeeze(sum(u_east.*dzu_east)./sum(dzu_east)),N));
u_north=u_north-squeeze(tridim(squeeze(sum(u_north.*dzu_north)./sum(dzu_north)),N));
u_west=u_west-squeeze(tridim(squeeze(sum(u_west.*dzu_west)./sum(dzu_west)),N));
%   
v_south=v_south-squeeze(tridim(squeeze(sum(v_south.*dzv_south)./sum(dzv_south)),N));
v_east=v_east-squeeze(tridim(squeeze(sum(v_east.*dzv_east)./sum(dzv_east)),N));
v_north=v_north-squeeze(tridim(squeeze(sum(v_north.*dzv_north)./sum(dzv_north)),N));
v_west=v_west-squeeze(tridim(squeeze(sum(v_west.*dzv_west)./sum(dzv_west)),N));
%
u_south =u_south + squeeze(tridim(ubar_south,N));
u_east  =u_east  + squeeze(tridim(ubar_east,N));     
u_north =u_north + squeeze(tridim(ubar_north,N));     
u_west  =u_west  + squeeze(tridim(ubar_west,N));
%
v_south = v_south + squeeze(tridim(vbar_south,N));
v_east  = v_east  + squeeze(tridim(vbar_east,N));     
v_north = v_north + squeeze(tridim(vbar_north,N));     
v_west  = v_west  + squeeze(tridim(vbar_west,N));  
  
end

%
% Barotropic velocities for bry outputs
%

ubar_south=squeeze(sum(u_south.*dzu_south)./sum(dzu_south));
ubar_east=squeeze(sum(u_east.*dzu_east)./sum(dzu_east));
ubar_north=squeeze(sum(u_north.*dzu_north)./sum(dzu_north));
ubar_west=squeeze(sum(u_west.*dzu_west)./sum(dzu_west));


vbar_south=squeeze(sum(v_south.*dzv_south)./sum(dzv_south));
vbar_east=squeeze(sum(v_east.*dzv_east)./sum(dzv_east));
vbar_north=squeeze(sum(v_north.*dzv_north)./sum(dzv_north));
vbar_west=squeeze(sum(v_west.*dzv_west)./sum(dzv_west));

end   %~isempty(nc_bry)
%--------------------------------------------------------------

%
%  fill the files
%
if ~isempty(nc_clm)
  nc_clm{'zeta'}(tout,:,:)=zeta;
  nc_clm{'SSH'}(tout,:,:)=zeta;
  nc_clm{'temp'}(tout,:,:,:)=temp;
  nc_clm{'salt'}(tout,:,:,:)=salt;
  nc_clm{'u'}(tout,:,:,:)=u;
  nc_clm{'v'}(tout,:,:,:)=v;
  nc_clm{'ubar'}(tout,:,:,:)=ubar;
  nc_clm{'vbar'}(tout,:,:,:)=vbar;
end
if ~isempty(nc_bry)
for obcndx=1:4
  if obcndx==1
      nc_bry{'zeta_south'}(tout,:)=zeta_south;
      nc_bry{'temp_south'}(tout,:,:)=temp_south;
      nc_bry{'salt_south'}(tout,:,:)=salt_south;
      nc_bry{'u_south'}(tout,:,:)=u_south;
      nc_bry{'v_south'}(tout,:,:)=v_south;
      nc_bry{'ubar_south'}(tout,:,:)=ubar_south;
      nc_bry{'vbar_south'}(tout,:,:)=vbar_south;  
  elseif obcndx==2
      nc_bry{'zeta_east'}(tout,:)=zeta_east;
      nc_bry{'temp_east'}(tout,:,:)=temp_east;
      nc_bry{'salt_east'}(tout,:,:)=salt_east;
      nc_bry{'u_east'}(tout,:,:)=u_east;
      nc_bry{'v_east'}(tout,:,:)=v_east;
      nc_bry{'ubar_east'}(tout,:,:)=ubar_east;
      nc_bry{'vbar_east'}(tout,:,:)=vbar_east;  
  elseif obcndx==3
      nc_bry{'zeta_north'}(tout,:)=zeta_north;
      nc_bry{'temp_north'}(tout,:,:)=temp_north;
      nc_bry{'salt_north'}(tout,:,:)=salt_north;
      nc_bry{'u_north'}(tout,:,:)=u_north;
      nc_bry{'v_north'}(tout,:,:)=v_north;
      nc_bry{'ubar_north'}(tout,:,:)=ubar_north;
      nc_bry{'vbar_north'}(tout,:,:)=vbar_north;  
  elseif obcndx==4
      nc_bry{'zeta_west'}(tout,:)=zeta_west;
      nc_bry{'temp_west'}(tout,:,:)=temp_west;
      nc_bry{'salt_west'}(tout,:,:)=salt_west;
      nc_bry{'u_west'}(tout,:,:)=u_west;
      nc_bry{'v_west'}(tout,:,:)=v_west;
      nc_bry{'ubar_west'}(tout,:,:)=ubar_west;
      nc_bry{'vbar_west'}(tout,:,:)=vbar_west;  
  end
end

end

toc
