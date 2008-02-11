%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Create and fill ROMS clim and bry files with OGCM data.
% for a forecast run
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
%  Copyright (c) 2006 by Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr  
%
%  Updated    8-Sep-2006 by Pierrick Penven
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%clear all
%close all
%%%%%%%%%%%%%%%%%%%%% USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%%
%
% Common parameters
%
romstools_param
%
% Specific to forecast
%
makeplot = 0;
%
OGCM = 'ECCO'; 
bry_prefix  = [ROMS_files_dir,'roms_bry_',OGCM,'_']; % generic boundary file name
clm_prefix  = [ROMS_files_dir,'roms_clm_',OGCM,'_']; % generic climatology file name
ini_prefix  = [ROMS_files_dir,'roms_ini_',OGCM,'_']; % generic initial file name
FRCST_prefix  = [OGCM,'_'];                    % generic OGCM file name 
%
%
if strcmp(OGCM,'ECCO')
%
%  ECCO DODS URL
%
% Kalman filter 
%
  url = 'http://ecco.jpl.nasa.gov/cgi-bin/nph-dods/datasets/kf066b/kf066b_'; 
%
else
%
% I don't know other OGCM available in real time...
%
  error(['Unknown OGCM: ',OGCM])
end
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% end of user input  parameters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%
% Extract data over the internet
%
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
eval(['OGCM_name=download_',OGCM,'_frcst(lonmin,lonmax,latmin,latmax,',...
                'FRCST_dir,FRCST_prefix,url,Yorig);'])
%
% Get the date
%
rundate_str=date;
rundate=datenum(rundate_str)-datenum(Yorig,1,1);
%
% Get the OGCM grid 
% 
nc=netcdf(OGCM_name);
lonT=nc{'lonT'}(:);
latT=nc{'latT'}(:);
lonU=nc{'lonU'}(:);
latU=nc{'latU'}(:);
lonV=nc{'lonV'}(:);
latV=nc{'latV'}(:);
Z=-nc{'depth'}(:);
NZ=length(Z);
NZ=NZ-rmdepth;
Z=Z(1:NZ);
close(nc)
%
% Initial file 
% (the strategy is to start at the begining of a month)
% it is possible to do some temporal interpolation... 
% but I am too lazy. lets start the first day of
% month Mmin of year Ymin... with the first data available.
%
if makeini==1
  ininame=[ini_prefix,num2str(rundate),nc_suffix];
  disp(['Create an initial file for ',num2str(rundate);])
  create_inifile(ininame,grdname,ROMS_title,...
                 theta_s,theta_b,hc,N,...
                 rundate-1,'clobber');
  nc_ini=netcdf(ininame,'write');
  interp_OGCM_frcst(OGCM_name,Roa,interp_method,...
              lonU,latU,lonV,latV,lonT,latT,Z,1,...
              nc_ini,[],lon,lat,angle,h,1)
  close(nc_ini)
  eval(['!cp ',ininame,' ',ini_prefix,'hct',nc_suffix])
end
%
%
% Clim and Bry files
%
if makeclim==1 | makebry==1
  if makebry==1
    bryname=[bry_prefix,num2str(rundate),nc_suffix];
    create_bryfile(bryname,grdname,ROMS_title,[1 1 1 1],...
                   theta_s,theta_b,hc,N,...
                   [90 270],360,'clobber');
    nc_bry=netcdf(bryname,'write');
  else
    nc_bry=[];
  end
  if makeclim==1
    clmname=[clm_prefix,num2str(rundate),nc_suffix];
    create_climfile(clmname,grdname,ROMS_title,...
                    theta_s,theta_b,hc,N,...
                    [90 270],360,'clobber');
    nc_clm=netcdf(clmname,'write');
  else
    nc_clm=[];
  end
%
% Perform the interpolations for the current month
%
  interp_OGCM_frcst(OGCM_name,Roa,interp_method,...
                    lonU,latU,lonV,latV,lonT,latT,Z,1,...
		    nc_clm,nc_bry,lon,lat,angle,h,1)
  interp_OGCM_frcst(OGCM_name,Roa,interp_method,...
                    lonU,latU,lonV,latV,lonT,latT,Z,1,...
		    nc_clm,nc_bry,lon,lat,angle,h,2)
%
% Close the ROMS files
%
  if ~isempty(nc_clm)
    close(nc_clm);
  end
  if ~isempty(nc_bry)
    close(nc_bry);
  end
%
end
%---------------------------------------------------------------
% Make a few plots
%---------------------------------------------------------------
if makeplot==1
  disp(' ')
  disp(' Make a few plots...')
  if makeini==1
    ininame=[ini_prefix,num2str(rundate),nc_suffix];
    figure
    test_clim(ininame,grdname,'temp',1,coastfileplot)
    figure
    test_clim(ininame,grdname,'salt',1,coastfileplot)
  end
  if makeclim==1
    clmname=[clm_prefix,num2str(rundate),nc_suffix];
    figure
    test_clim(clmname,grdname,'temp',1,coastfileplot)
    figure
    test_clim(clmname,grdname,'salt',1,coastfileplot)
  end
  if makebry==1
    bryname=[bry_prefix,num2str(rundate),nc_suffix];
    figure
    test_bry(bryname,grdname,'temp',1,obc)
    figure
    test_bry(bryname,grdname,'salt',1,obc)
    figure
    test_bry(bryname,grdname,'u',1,obc)
    figure
    test_bry(bryname,grdname,'v',1,obc)
  end
end

