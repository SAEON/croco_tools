%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Create and fill frc and bulk files with GFS data.
% for a forecast run
%
% The on-line reference to GFS is at
% http://nomad3.ncep.noaa.gov/
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
%  Copyright (c) 2006 by Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr  
%
%  Updated    9-Sep-2006 by Pierrick Penven
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Common parameters
%
romstools_param
%
makeplot = 0;
%
frc_prefix=[frc_prefix,'_GFS_'];
blk_prefix=[blk_prefix,'_GFS_'];
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% end of user input  parameters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% time (in matlab time)
%
today=floor(now);
%
% date in 'Yorig' time
%
rundate=datenum(today)-datenum(Yorig,1,1);
%
% GFS data name
%
gfs_name=[FRCST_dir,'GFS_',num2str(rundate),'.nc'];
%
%
if level==0
  nc_suffix='.nc';
else
  nc_suffix=['.nc.',num2str(level)];
  grdname=[grdname,'.',num2str(level)];
end
%
% Get the model grid
%
nc=netcdf(grdname);
lon=nc{'lon_rho'}(:);
lat=nc{'lat_rho'}(:);
angle=nc{'angle'}(:);
h=nc{'h'}(:);
close(nc)
cosa=cos(angle);
sina=sin(angle);
%
% Extract data over the internet
%
if Download_data==1
%
% Get the model limits
%
  lonmin=min(min(lon));
  lonmax=max(max(lon));
  latmin=min(min(lat));
  latmax=max(max(lat));
%
% Download data with DODS (the download matlab routine depends on the OGCM)
% 
  disp('Download data...')
  download_GFS(today,lonmin,lonmax,latmin,latmax,FRCST_dir,Yorig)
%
end
%
% Get the GFS grid 
% 
nc=netcdf(gfs_name);
lon1=nc{'lon'}(:);
lat1=nc{'lat'}(:);
time=nc{'time'}(:);
mask=nc{'mask'}(:);
tlen=length(time);
%
% bulk and forcing files
%
blkname=[blk_prefix,num2str(rundate),nc_suffix];
disp(['Create a new bulk file: ' blkname])
create_bulk(blkname,grdname,ROMS_title,time,0);
nc_blk=netcdf(blkname,'write');
frcname=[frc_prefix,num2str(rundate),nc_suffix];
disp(['Create a new forcing file: ' frcname])
create_forcing(frcname,grdname,ROMS_title,...
                       time,0,0,...
                       0,0,0,...
  	               0,0,0,0,0,0)
nc_frc=netcdf(frcname,'write');
%
% Loop on time
%
missval=nan;
default=nan;
for l=1:tlen
%for l=1:1
  disp(['time index: ',num2str(l),' of total: ',num2str(tlen)])
  
  var=get_missing_val(lon1,lat1,squeeze(nc{'tair'}(l,:,:)),missval,Roa,default);
  nc_blk{'tair'}(l,:,:)=interp2(lon1,lat1,var,lon,lat,interp_method);
  var=get_missing_val(lon1,lat1,squeeze(nc{'rhum'}(l,:,:)),missval,Roa,default);
  nc_blk{'rhum'}(l,:,:)=interp2(lon1,lat1,var,lon,lat,interp_method);
  var=get_missing_val(lon1,lat1,squeeze(nc{'prate'}(l,:,:)),missval,Roa,default);
  nc_blk{'prate'}(l,:,:)=interp2(lon1,lat1,var,lon,lat,interp_method);
  var=get_missing_val(lon1,lat1,squeeze(nc{'wspd'}(l,:,:)),missval,Roa,default);
  nc_blk{'wspd'}(l,:,:)=interp2(lon1,lat1,var,lon,lat,interp_method);
  var=get_missing_val(lon1,lat1,squeeze(nc{'radlw'}(l,:,:)),missval,Roa,default);
  nc_blk{'radlw'}(l,:,:)=interp2(lon1,lat1,var,lon,lat,interp_method);
  var=get_missing_val(lon1,lat1,squeeze(nc{'radsw'}(l,:,:)),missval,Roa,default);
  nc_blk{'radsw'}(l,:,:)=interp2(lon1,lat1,var,lon,lat,interp_method);
  tx=get_missing_val(lon1,lat1,squeeze(nc{'tx'}(l,:,:)),missval,Roa,default);
  ty=get_missing_val(lon1,lat1,squeeze(nc{'ty'}(l,:,:)),missval,Roa,default);
  tx=interp2(lon1,lat1,tx,lon,lat,interp_method);
  ty=interp2(lon1,lat1,ty,lon,lat,interp_method);
  nc_frc{'sustr'}(l,:,:)=rho2u_2d(tx.*cosa+ty.*sina);
  nc_frc{'svstr'}(l,:,:)=rho2v_2d(ty.*cosa-tx.*sina);
end
%
%  Perform the interpolations
% 
close(nc_frc);
close(nc_blk);
close(nc)
%---------------------------------------------------------------
% Make a few plots
%---------------------------------------------------------------
if makeplot==1
  disp(' ')
  disp(' Make a few plots...')
  slides=[10 12 14 16]; 
  test_forcing(blkname,grdname,'tair',slides,3,coastfileplot)
  figure
  test_forcing(blkname,grdname,'rhum',slides,3,coastfileplot)
  figure
  test_forcing(blkname,grdname,'prate',slides,3,coastfileplot)
  figure
  test_forcing(blkname,grdname,'wspd',slides,3,coastfileplot)
  figure
  test_forcing(blkname,grdname,'radlw',slides,3,coastfileplot)
  figure
  test_forcing(blkname,grdname,'radsw',slides,3,coastfileplot)
end











