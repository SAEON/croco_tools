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
%  Updated    20-Aug-2008 by Matthieu Caillaud & P. Marchesiello
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%clear all
%close all
%%%%%%%%%%%%%%%%%%%%% USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%%
%
% Common parameters
%
tic
romstools_param
% Get the date
%
rundate_str=date;
rundate=datenum(rundate_str)-datenum(Yorig,1,1);
OGCM_name=[FRCST_dir,FRCST_prefix,num2str(rundate),'.cdf'];
%
%
% Specific to forecast
%
makeplot = 0;
%
%OGCM = 'mercator';
bry_prefix  = [ROMS_files_dir,'roms_bry_',OGCM,'_']; % generic boundary file name
clm_prefix  = [ROMS_files_dir,'roms_clm_',OGCM,'_']; % generic climatology file name
ini_prefix  = [ROMS_files_dir,'roms_ini_',OGCM,'_']; % generic initial file name
FRCST_prefix  = [OGCM,'_'];                          % generic OGCM file name
%
%
if strcmp(OGCM,'ECCO')
  %
  %  ECCO DODS URL
  %
  % Kalman filter
  %
  %%url = 'http://ecco.jpl.nasa.gov/cgi-bin/nph-dods/datasets/kf066b/kf066b_';
  url = 'http://ecco.jpl.nasa.gov/thredds/dodsC/las/kf080/kf080_';
  %
elseif strcmp(OGCM,'mercator')
  %
  %  MERCATOR DODS URL
  %
  %  login/password can be entered here or in romstools params
  %  and should be asked to the MyOCEAN team
  %
  url=[FRCST_dir,'motu_primaryrawformat_mercator_',num2str(rundate),'.nc'];
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

if Download_data
  %
  %
  % Download data with Motu python (the download matlab routine depends on the OGCM)
  %
  if strcmp(OGCM,'ECCO')
	disp('Download data...')
	eval(['OGCM_name=download_',OGCM,'_frcst(lonmin,lonmax,latmin,latmax,',...
	  'FRCST_dir,FRCST_prefix,url,Yorig);'])
  elseif strcmp(OGCM,'mercator')
	disp('Download data...')
	eval(['OGCM_name=download_',OGCM,'_frcst_python(lonmin,lonmax,latmin,latmax,',...
		'FRCST_dir,FRCST_prefix,url,Yorig);'])
% % 	if ~exist(OGCM_name);
% % 	  disp([' '])
% % 	  disp(['  ==> Download the raw motu Mercator file :',url])
% % 	  disp(['  ==> Then create the ROMS format Mercator file :',OGCM_name])
% % 	  disp(['============================'])
% % 	  eval(['OGCM_name=download_',OGCM,'_frcst_python(lonmin,lonmax,latmin,latmax,',...
% % 		'FRCST_dir,FRCST_prefix,url,Yorig);'])
% % 	else
% % 	  disp(['  ==> No processing needed: ROMS Mercator file exists : ', OGCM_name])
% % 	  if ~exist(url)
% % 		disp([char({'  ROMS Mercator file exists';'  but not the Motu Mercator file. TAKE CARE'})])
% % 		disp(['==========================================='])
% % 	  end
% % 	end
  end
end
% Get the OGCM grid 
% 
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
if strcmp(OGCM,'ECCO')
  time=[90 270];
  time_cycle=360;
  trange=[1 1];
  %hdays=1;
elseif strcmp(OGCM,'mercator')
  OGCM_time=nc{'time'}(:);
  ntimes=length(OGCM_time);
  dt=max(gradient(OGCM_time));
  time_cycle=0;
  %hdays=5;
  if OGCM_time(end)~=rundate+6
    if OGCM_time(end)==rundate+5
      roms_time=0*(1:ntimes+1);
      roms_time(1:end-1)=OGCM_time;
      roms_time(end)=roms_time(end-1)+dt;
    else
      roms_time=0*(1:ntimes+2);
      roms_time(1:end-2)=OGCM_time;
      roms_time(end-1)=roms_time(end-2)+dt;
      roms_time(end)=roms_time(end-1)+dt;
    end
  else
    roms_time=OGCM_time;
  end
  nrec=1;
  if nrec==1  % nrec=1 3 records are used
    time=0*(1:3);
    time(1)=roms_time(1);
    time(2)=roms_time(OGCM_time==rundate);
    time(3)=roms_time(end);
  else        % nrec=0 all records are used
    for i=1:delta:length(roms_time)
      time(i)=roms_time(i);
    end
  end
  p=find(OGCM_time==OGCM_time(1));
  q=find(OGCM_time==OGCM_time(5));
  r=find(OGCM_time==OGCM_time(end));
  trange=[p q r];
end   % MERCATOR
close(nc)

%
% Initial file 
%
if makeini==1
  ininame=[ini_prefix,num2str(rundate),nc_suffix];
  disp(['Create an initial file for ',num2str(rundate);])
  create_inifile(ininame,grdname,ROMS_title,...
                 theta_s,theta_b,hc,N,...
                 rundate-hdays,'clobber',vtransform);
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
                   time,time_cycle,'clobber',vtransform);
    nc_bry=netcdf(bryname,'write');
  else
    nc_bry=[];
  end
  if makeclim==1
    clmname=[clm_prefix,num2str(rundate),nc_suffix];
    create_climfile(clmname,grdname,ROMS_title,...
                    theta_s,theta_b,hc,N,...
                    time,time_cycle,'clobber',vtransform);
    nc_clm=netcdf(clmname,'write');
  else
    nc_clm=[];
  end

%
% Perform the interpolations for all selected records
%
for tndx=1:length(time)
  disp([' Time step : ',num2str(tndx),' of ',num2str(length(time)),' :'])
  interp_OGCM_frcst(OGCM_name,Roa,interp_method,...
                    lonU,latU,lonV,latV,lonT,latT,Z,trange(tndx),...
		    nc_clm,nc_bry,lon,lat,angle,h,tndx,vtransform)
end

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

toc
