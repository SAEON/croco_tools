%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Create and fill CROCO clim and bry files with OGCM data.
% for a forecast run
%
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
%  Copyright (c) 2006 by Pierrick Penven
%  e-mail:Pierrick.Penven@ird.fr
%
%  Updated    8-Sep-2006 by Pierrick Penven
%  Updated   20-Aug-2008 by Matthieu Caillaud & P. Marchesiello
%  Updated   12-Feb-2016 by P. Marchesiello
%  Updated   14-Oct-2020 by P. Marchesiello
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%clear all
%close all
tic
%%%%%%%%%%%%%%%%%%%%% USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%%
%
% Common parameters
%
crocotools_param
%
% Specific to forecast
%
makeplot = 0;
%
% Get date
%
rundate_str=date;
rundate=datenum(rundate_str)-datenum(Yorig,1,1);
%
% Set generic OGCM file name
%
FRCST_prefix=[OGCM,'_'];
OGCM_name=[FRCST_dir,FRCST_prefix,num2str(rundate),'.cdf'];
%
if strcmp(OGCM,'ECCO')
  %
  %  ECCO DODS URL
  %
  % Kalman filter
  %
  url = 'http://ecco.jpl.nasa.gov/thredds/dodsC/las/kf080/kf080_';
  %
elseif strcmp(OGCM,'mercator')
  %
  %  MERCATOR : see get_file_python_mercator.m
  %
  raw_mercator_name=[FRCST_dir,'raw_motu_mercator_',num2str(rundate),'.nc'];
else
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
pm=nc{'pm'}(:);
pn=nc{'pn'}(:);
rmask=nc{'mask_rho'}(:);
close(nc)

%---------------------------------------------------------------
% Extract data from the Internet
%---------------------------------------------------------------
if Download_data
 %
 % Get model limits
 %
 lonmin=min(min(lon)); 
 lonmax=max(max(lon));
 latmin=min(min(lat));
 latmax=max(max(lat));
 %
 % Download data (matlab routine for download depends on OGCM)
 %
 if strcmp(OGCM,'ECCO')
  disp('Download data...')
  eval(['OGCM_name=download_', ...
         OGCM,'_frcst(lonmin,lonmax,latmin,latmax,',...
                     'FRCST_dir,FRCST_prefix,url,Yorig);'])

 elseif strcmp(OGCM,'mercator')
  %
  % Use Motu python
  %
  disp('Download data...')
    eval(['OGCM_name=download_', ...
          OGCM,'_frcst_python(pathMotu,user,password,mercator_type,', ...
                              'motu_url_fcst,service_id_fcst,product_id_fcst,', ...
                              'hdays,fdays,', ...
                              'lonmin,lonmax,latmin,latmax,hmax,', ...
                              'FRCST_dir,FRCST_prefix,raw_mercator_name,Yorig);'])
  end
end

%---------------------------------------------------------------
% Get OGCM grid 
%---------------------------------------------------------------
nc=netcdf(OGCM_name)
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

%---------------------------------------------------------------
% Get time array 
%---------------------------------------------------------------
if strcmp(OGCM,'ECCO')
  time=[90 270];
  time_cycle=360;
  trange=[1 1];
elseif strcmp(OGCM,'mercator')
  OGCM_time=nc{'time'}(:);
  time_cycle=0;
  delta=1; % >1 if subsampling
  trange=[1:delta:length(OGCM_time)];
  time=zeros(length(trange),1);
  for i=1:length(trange)
    time(i)=OGCM_time(trange(i));
  end
end
close(nc)

%---------------------------------------------------------------
% Initial file 
%---------------------------------------------------------------
if makeini==1
  ininame=[ini_prefix,num2str(rundate),nc_suffix];
  disp(['Create an initial file for ',num2str(rundate);])
  create_inifile(ininame,grdname,CROCO_title,...
                 theta_s,theta_b,hc,N,...
                 rundate-hdays,'clobber',vtransform); % starts at 00:00
  nc_ini=netcdf(ininame,'write');
 
  interp_OGCM_frcst(OGCM_name,Roa,interp_method,...
              lonU,latU,lonV,latV,lonT,latT,Z,1,...
              nc_ini,[],lon,lat,angle,h,pm,pn,rmask,...
              1,vtransform,obc)
  close(nc_ini)
  eval(['!cp ',ininame,' ',ini_prefix,'hct',nc_suffix])
end

%---------------------------------------------------------------
% Clim and Bry files
%---------------------------------------------------------------
if makeclim==1 | makebry==1
  if makebry==1
    bryname=[bry_prefix,num2str(rundate),nc_suffix];
    create_bryfile(bryname,grdname,CROCO_title,[1 1 1 1],...
                   theta_s,theta_b,hc,N,...
                   time,time_cycle,'clobber',vtransform);
    nc_bry=netcdf(bryname,'write');
  else
    nc_bry=[];
  end
  if makeclim==1
    clmname=[clm_prefix,num2str(rundate),nc_suffix];
    create_climfile(clmname,grdname,CROCO_title,...
                    theta_s,theta_b,hc,N,...
                    time,time_cycle,'clobber',vtransform);
    nc_clm=netcdf(clmname,'write');
  else
    nc_clm=[];
  end

%---------------------------------------------------------------
% Perform interpolations for all selected records
%---------------------------------------------------------------
for tndx=1:length(time)
  disp([' Time step : ',num2str(tndx),' of ',num2str(length(time)),' :'])
  interp_OGCM_frcst(OGCM_name,Roa,interp_method,...
                    lonU,latU,lonV,latV,lonT,latT,Z,trange(tndx),...
		    nc_clm,nc_bry,lon,lat,angle,h,pm,pn,rmask, ...
                    tndx,vtransform,obc)
end

%
% Close CROCO files
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

toc
