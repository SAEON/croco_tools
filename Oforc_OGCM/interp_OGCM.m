function interp_OGCM(OGCM_dir,OGCM_prefix,year,month,Roa,interp_method,...
                     lonU,latU,lonV,latV,lonT,latT,Z,tin,...
		     nc_clm,nc_bry,lon,lat,angle,h,tout,obc)
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
%  Updated    5-Nov-2006 by Pierrick Penven : A bit of cleaning...
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
conserv=1; % same barotropic velocities as the OGCM
%
disp(['  Horizontal interpolation: ',...
      OGCM_prefix,'Y',num2str(year),'M',num2str(month),'.cdf'])
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
  if obc(1)==1
    zeta_south=squeeze(zeta(1,:));
    ubar_south=squeeze(ubar(1,:));
    vbar_south=squeeze(vbar(1,:));
    u_south=squeeze(u(:,1,:));
    v_south=squeeze(v(:,1,:));
    temp_south=squeeze(temp(:,1,:));
    salt_south=squeeze(salt(:,1,:));
  end
  if obc(2)==1
    zeta_east=squeeze(zeta(:,end));
    ubar_east=squeeze(ubar(:,end));
    vbar_east=squeeze(vbar(:,end));
    u_east=squeeze(u(:,:,end));
    v_east=squeeze(v(:,:,end));
    temp_east=squeeze(temp(:,:,end));
    salt_east=squeeze(salt(:,:,end));
  end
  if obc(3)==1
    zeta_north=squeeze(zeta(end,:));
    ubar_north=squeeze(ubar(end,:));
    vbar_north=squeeze(vbar(end,:));
    u_north=squeeze(u(:,end,:));
    v_north=squeeze(v(:,end,:));
    temp_north=squeeze(temp(:,end,:));
    salt_north=squeeze(salt(:,end,:));
  end
  if obc(4)==1
    zeta_west=squeeze(zeta(:,1));
    ubar_west=squeeze(ubar(:,1));
    vbar_west=squeeze(vbar(:,1));
    u_west=squeeze(u(:,:,1));
    v_west=squeeze(v(:,:,1));
    temp_west=squeeze(temp(:,:,1));
    salt_west=squeeze(salt(:,:,1));
  end
end 

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
% Add an extra bottom layer (-100000m) and an extra surface layer (+100m)
% to prevent vertical extrapolations
%
Z=[100;Z;-100000];
%
% ROMS vertical grid
%
zr=zlevs(h,zeta,theta_s,theta_b,hc,N,'r');
zu=rho2u_3d(zr);
zv=rho2v_3d(zr);
zw=zlevs(h,zeta,theta_s,theta_b,hc,N,'w');
dzr=zw(2:end,:,:)-zw(1:end-1,:,:);
dzu=rho2u_3d(dzr);
dzv=rho2v_3d(dzr);
%
%
% Vertical interpolation in case of clim file
%
%
if ~isempty(nc_clm)
%
% Add a level on top and bottom with no-gradient
%
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
%
end   %~isempty(nc_clm)
%
%
% Vertical interpolation in case of bry files
%
%
if ~isempty(nc_bry)
%
%South
%
  if obc(1)==1
    [u_south,v_south,ubar_south,vbar_south,...
     temp_south,salt_south]=vinterp_OGCM_bry(zr(:,1,:),zu(:,1,:),zv(:,1,:),...
                                             dzr(:,1,:),dzu(:,1,:),dzv(:,1,:),...
                                             u_south,v_south,ubar_south,vbar_south,...
                                             temp_south,salt_south,...
                                             N,Z,conserv);
  end
  if obc(2)==1
    [u_east,v_east,ubar_east,vbar_east,...
     temp_east,salt_east]=vinterp_OGCM_bry(zr(:,:,end),zu(:,:,end),zv(:,:,end),...
                                           dzr(:,:,end),dzu(:,:,end),dzv(:,:,end),...
                                           u_east,v_east,ubar_east,vbar_east,...
                                           temp_east,salt_east,...
                                           N,Z,conserv);
  end
  if obc(3)==1
    [u_north,v_north,ubar_north,vbar_north,...
     temp_north,salt_north]=vinterp_OGCM_bry(zr(:,end,:),zu(:,end,:),zv(:,end,:),...
                                             dzr(:,end,:),dzu(:,end,:),dzv(:,end,:),...
                                             u_north,v_north,ubar_north,vbar_north,...
                                             temp_north,salt_north,...
                                             N,Z,conserv);
  end
  if obc(4)==1
    [u_west,v_west,ubar_west,vbar_west,...
     temp_west,salt_west]=vinterp_OGCM_bry(zr(:,:,1),zu(:,:,1),zv(:,:,1),...
                                           dzr(:,:,1),dzu(:,:,1),dzv(:,:,1),...
                                           u_west,v_west,ubar_west,vbar_west,...
                                           temp_west,salt_west,...
                                           N,Z,conserv);
  end
end   %~isempty(nc_bry)
%--------------------------------------------------------------

%
%  fill the files
%
% Climatology file
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
%
% Boundary file
%
if ~isempty(nc_bry)
  if obc(1)==1
    nc_bry{'zeta_south'}(tout,:)=zeta_south;
    nc_bry{'temp_south'}(tout,:,:)=temp_south;
    nc_bry{'salt_south'}(tout,:,:)=salt_south;
    nc_bry{'u_south'}(tout,:,:)=u_south;
    nc_bry{'v_south'}(tout,:,:)=v_south;
    nc_bry{'ubar_south'}(tout,:,:)=ubar_south;
    nc_bry{'vbar_south'}(tout,:,:)=vbar_south;
  end  
  if obc(2)==1
    nc_bry{'zeta_east'}(tout,:)=zeta_east;
    nc_bry{'temp_east'}(tout,:,:)=temp_east;
    nc_bry{'salt_east'}(tout,:,:)=salt_east;
    nc_bry{'u_east'}(tout,:,:)=u_east;
    nc_bry{'v_east'}(tout,:,:)=v_east;
    nc_bry{'ubar_east'}(tout,:,:)=ubar_east;
    nc_bry{'vbar_east'}(tout,:,:)=vbar_east;  
  end  
  if obc(3)==1
    nc_bry{'zeta_north'}(tout,:)=zeta_north;
    nc_bry{'temp_north'}(tout,:,:)=temp_north;
    nc_bry{'salt_north'}(tout,:,:)=salt_north;
    nc_bry{'u_north'}(tout,:,:)=u_north;
    nc_bry{'v_north'}(tout,:,:)=v_north;
    nc_bry{'ubar_north'}(tout,:,:)=ubar_north;
    nc_bry{'vbar_north'}(tout,:,:)=vbar_north;    
  end  
  if obc(4)==1
    nc_bry{'zeta_west'}(tout,:)=zeta_west;
    nc_bry{'temp_west'}(tout,:,:)=temp_west;
    nc_bry{'salt_west'}(tout,:,:)=salt_west;
    nc_bry{'u_west'}(tout,:,:)=u_west;
    nc_bry{'v_west'}(tout,:,:)=v_west;
    nc_bry{'ubar_west'}(tout,:,:)=ubar_west;
    nc_bry{'vbar_west'}(tout,:,:)=vbar_west;  
  end
end
