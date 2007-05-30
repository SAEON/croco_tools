%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  make_NCEP.m
% 
%  Create and fill frc and bulk files with NCEP data.
%  (NCEP Reanalysis)
%
%  The on-line reference to NCEP is at
%  http://www.cdc.noaa.gov/cdc/reanalysis/
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
%  Adapted from a previous verions from
%  Alvaro Peliz (U. Aveiro) & Patrick Marchesiello (IRD) - 2005
%
%  Updated    6-Sep-2006 by Pierrick Penven
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start
clear all
close all
%%%%%%%%%%%%%%%%%%%%% USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%%
%
% Common parameters
%
romstools_param
%
frc_prefix=[frc_prefix,'_NCEP_'];
blk_prefix=[blk_prefix,'_NCEP_'];
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
close(nc)
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
% Download NCEP
% 
  disp('Download NCEP data with OPENDAP')
  download_NCEP(Ymin,Ymax,Mmin,Mmax,lonmin,lonmax,latmin,latmax,...
                NCEP_dir,NCEP_version,Yorig)
end
%
if makefrc==1 | makeblk==1
%
% Get the NCEP horisontal grids (it should be the same for every month)
% in case of NCEP1 there are 2 horizontal grid
% (rhum is not on the gauss grid) 
% 
  nc=netcdf([NCEP_dir,'land.sfc.gauss.nc']);
  lon1=nc{'lon'}(:);
  lat1=nc{'lat'}(:);
  [lon1,lat1]=meshgrid(lon1,lat1);
  mask1=1-squeeze(nc{'land'}(:));
  mask1(mask1==0)=NaN;
  close(nc)
  if NCEP_version==1
    nc=netcdf([NCEP_dir,'land.nc']);
    lon2=nc{'lon'}(:);
    lat2=nc{'lat'}(:);
    [lon2,lat2]=meshgrid(lon2,lat2);
    mask2=1-squeeze(nc{'land'}(:));
    mask2(mask2==0)=NaN;
    close(nc)
  else
    lon2=[];
    lat2=[];
    mask2=[];
  end
%
% Loop on the years and the months
%
  for Y=Ymin:Ymax
    if Y==Ymin 
      mo_min=Mmin;
    else
      mo_min=1;
    end
    if Y==Ymax
      mo_max=Mmax;
    else
      mo_max=12;
    end
    for M=mo_min:mo_max
      disp(' ')
      disp(['Processing  year ',num2str(Y),...
            ' - month ',num2str(M)])
      disp(' ')
%
% Process the time (here in days)
%
      nc=netcdf([NCEP_dir,'air.2m.gauss.Y',num2str(Y),'M',num2str(M),'.nc']);
      NCEP_time=nc{'time'}(:);
      close(nc);
      dt=mean(gradient(NCEP_time));
%
% Add 3 times step in the ROMS files: 1 at the beginning and 2 at the end 
% (otherwise.. ROMS crashes)
%
      tlen=length(NCEP_time)+3;
      time=0*(1:tlen);
      time(2:end-2)=NCEP_time;
      time(1)=time(2)-dt;
      time(end-1)=time(end-2)+dt;
      time(end)=time(end-1)+dt;
%
% Create the ROMS forcing files
%
      blkname=[blk_prefix,'Y',num2str(Y),...
               'M',num2str(M),nc_suffix];
      frcname=[frc_prefix,'Y',num2str(Y),...
               'M',num2str(M),nc_suffix];
      if makeblk==1
        disp(['Create a new bulk file: ' blkname])
        create_bulk(blkname,grdname,ROMS_title,time,0);
      end
      if makefrc==1
        disp(['Create a new forcing file: ' frcname])
        create_forcing(frcname,grdname,ROMS_title,...
                       time,0,0,...
                       0,0,0,...
  	               0,0,0,0,0,0)
      end
%
% Add the tides (needs to be tested for this version of make_NCEP)
%
      if add_tides==1
        add_tidal_data(tidename,grdname,frcname,Y,M)
      end
%
% Open the ROMS forcing files
%
      if makefrc==1
        nc_frc=netcdf(frcname,'write');
      else
        nc_frc=[];
      end
      if makeblk==1
        nc_blk=netcdf(blkname,'write');
      else
        nc_blk=[];
      end
%
% Check if there are NCEP files for the previous Month
%
      Mm=M-1;
      Ym=Y;
      if Mm==0
        Mm=12;
        Ym=Y-1;
      end
      fname=[NCEP_dir,'air.2m.gauss.Y',num2str(Ym),'M',num2str(Mm),'.nc'];
      if exist(fname)==0
        disp(['   No data for the previous month: using current month'])
        tndx=1;
        Mm=M;
        Ym=Y;
      else
        nc=netcdf(fname);
        tndx=length(nc('time'));
        if makefrc==1
          nc_frc{'sms_time'}(1)=nc{'time'}(tndx);
        end
        if makeblk==1
          nc_blk{'bulk_time'}(1)=nc{'time'}(tndx);
        end
        close(nc)
      end
%
% Perform the interpolations for the previous month
%
      disp('First step')
      interp_NCEP(NCEP_version,NCEP_dir,Ym,Mm,Roa,interp_method,...
                  lon1,lat1,mask1,lon2,lat2,mask2,tndx,...
      	  	  nc_frc,nc_blk,lon,lat,angle,1)
%
% Perform the interpolations for the current month
%
      for tndx=1:tlen-3
        if mod(tndx,20)==0
          disp(['  Step: ',num2str(tndx),' of ',num2str(tlen-3)])
        end
        interp_NCEP(NCEP_version,NCEP_dir,Y,M,Roa,interp_method,...
                    lon1,lat1,mask1,lon2,lat2,mask2,tndx,...
		    nc_frc,nc_blk,lon,lat,angle,tndx+1)
      end
%
% Read the NCEP file for the next month
%
      Mp=M+1;
      Yp=Y;
      if Mp==13
        Mp=1;
        Yp=Y+1;
      end
      fname=[NCEP_dir,'air.2m.gauss.Y',num2str(Yp),'M',num2str(Mp),'.nc'];
      if exist(fname)==0
        disp(['   No data for the next month: using current month'])
        tndx=tlen-3;
        Mp=M;
        Yp=Y;
      else
        nc=netcdf(fname);
        tndx=1;
        if makefrc==1
          nc_frc{'sms_time'}(tlen-1)=nc{'time'}(tndx);
          nc_frc{'sms_time'}(tlen)=nc{'time'}(tndx+1);
        end
        if makeblk==1
          nc_blk{'bulk_time'}(tlen-1)=nc{'time'}(tndx);
          nc_blk{'bulk_time'}(tlen)=nc{'time'}(tndx+1);
        end
        close(nc)
      end
%
% Perform the interpolations for the next month
%
      disp('Last step')
      interp_NCEP(NCEP_version,NCEP_dir,Yp,Mp,Roa,interp_method,...
                  lon1,lat1,mask1,lon2,lat2,mask2,tndx,...
		  nc_frc,nc_blk,lon,lat,angle,tlen-1)
      disp('Last step (one more time...)')
      if Mp==M
        interp_NCEP(NCEP_version,NCEP_dir,Yp,Mp,Roa,interp_method,...
                    lon1,lat1,mask1,lon2,lat2,mask2,tndx,...
		    nc_frc,nc_blk,lon,lat,angle,tlen)
      else
        interp_NCEP(NCEP_version,NCEP_dir,Yp,Mp,Roa,interp_method,...
                    lon1,lat1,mask1,lon2,lat2,mask2,tndx+1,...
		    nc_frc,nc_blk,lon,lat,angle,tlen)
      end
%
% Close the ROMS forcing files
%
      if ~isempty(nc_frc)
        close(nc_frc);
      end
      if ~isempty(nc_blk)
        close(nc_blk);
      end
%
    end
  end
%
end
%
% Spin-up: (reproduce the first year 'SPIN_Long' times)
% just copy the files for the first year and change the time
%
if SPIN_Long>0
  disp(['Add a spin-up phase of ',num2str(SPIN_Long)])
  M=Mmin-1;
  Y=Ymin-SPIN_Long;
  for month=1:12*SPIN_Long
    M=M+1;
    if M==13
      M=1; 
      Y=Y+1;
    end
%
% Forcing files
%
    if makefrc==1
%
% Copy the file
%
      frcname=[frc_prefix,'Y',num2str(Ymin),'M',num2str(M),nc_suffix];
      frcname2=[frc_prefix,'Y',num2str(Y),'M',num2str(M),nc_suffix];
      disp(['Create ',frcname2]) 
      eval(['!cp ',frcname,' ',frcname2]) 
%
% Change the time
%
      nc=netcdf(frcname2,'write');
      time=nc{'sms_time'}(:)+datenum(Yorig,1,1);
      [y,m,d,h,mi,s]=datevec(time);
      dy=Ymin-Y;
      y=y-dy;
      time=datenum(y,m,d,h,mi,s)-datenum(Yorig,1,1);
%      disp(datestr(time+datenum(Yorig,1,1)))
      nc{'sms_time'}(:)=time;
      close(nc)
    end
%
% Bulk files
%
    if makeblk==1
%
% Copy the file
%
      blkname=[blk_prefix,'Y',num2str(Ymin),'M',num2str(M),nc_suffix];
      blkname2=[blk_prefix,'Y',num2str(Y),'M',num2str(M),nc_suffix];
      disp(['Create ',blkname2]) 
      eval(['!cp ',blkname,' ',blkname2]) 
%
% Change the time
%
      nc=netcdf(blkname2,'write');
      time=nc{'bulk_time'}(:)+datenum(Yorig,1,1);
      [y,m,d,h,mi,s]=datevec(time);
      dy=Ymin-Y;
      y=y-dy;
      time=datenum(y,m,d,h,mi,s)-datenum(Yorig,1,1);
%      disp(datestr(time+datenum(Yorig,1,1)))
      nc{'bulk_time'}(:)=time;
      close(nc)
    end
  end
end
%---------------------------------------------------------------
% Make a few plots
%---------------------------------------------------------------
if makeplot==1
  disp(' ')
  disp(' Make a few plots...')
  slides=[1 2 3 4]; 
  test_forcing(blkname,grdname,'tair',slides,3,coastfileplot)
  figure
  test_forcing(blkname,grdname,'rhum',slides,3,coastfileplot)
  figure
  test_forcing(blkname,grdname,'prate',slides,3,coastfileplot)
  figure
  test_forcing(blkname,grdname,'uwnd',slides,3,coastfileplot)
  figure
  test_forcing(blkname,grdname,'vwnd',slides,3,coastfileplot)
  figure
  test_forcing(blkname,grdname,'wspd',slides,3,coastfileplot)
  figure
  test_forcing(blkname,grdname,'radlw',slides,3,coastfileplot)
  figure
  test_forcing(blkname,grdname,'radsw',slides,3,coastfileplot)
end
