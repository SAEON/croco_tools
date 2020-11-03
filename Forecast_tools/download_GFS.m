function download_GFS(today,lonmin,lonmax,latmin,latmax,FRCST_dir,Yorig,it)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  download_GFS(today,lonmin,lonmax,latmin,latmax,FRCST_dir,Yorig)
%
%
%  Extract a subgrid from GFS to get a CROCO forcing
%  Store that into monthly files (to limit the problems
%  of bandwith...).
%  Take care of the Greenwitch Meridian.
%  Transform variables in the CROCO format.
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
%  Updated    9-Sep-2006 by Pierrick Penven
%  Updated    20-Aug-2008 by Matthieu Caillaud & P. Marchesiello
%  Updated    12-Feb-2016 by P. Marchesiello
%  Updated    31-Oct-2020 by P. Marchesiello
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
crocotools_param
%
% Put the date in 'Yorig' time
%
rundate=datenum(today)-datenum(Yorig,1,1);
%
% GFS output name
%
gfsftype=1;  % GFS files
gfs_name=[FRCST_dir,'GFS_',num2str(rundate),'.nc'];
%
% start
%
disp([' '])
disp(['Get GFS data for ',datestr(today)])
disp(['Minimum Longitude: ',num2str(lonmin)])
disp(['Maximum Longitude: ',num2str(lonmax)])
disp(['Minimum Latitude:  ',num2str(latmin)])
disp(['Maximum Latitude:  ',num2str(latmax)])
disp([' '])
%
% Create directory if needed
%
disp(['Making output data directory ',FRCST_dir])
eval(['!mkdir ',FRCST_dir])
%
% Get GFS file name (first check if a forecast is available)
%
gfs_run_time=0;
gfs_date=today-0.5;
foundfile=0;
while foundfile==0
  fname=get_GFS_fname(gfs_date,gfs_run_time,gfsftype);
  warning off
  try
    x=loaddap('-A -e +v ', fname);
    foundfile=1;
  catch
    foundfile=0;
  end
  if foundfile==1 & ~isempty(x)
    disp('  File found')
  else
    foundfile=0;
    disp(['  GFS : did not found ',fname])
    gfs_run_time=gfs_run_time-6;
    if gfs_run_time<0
      gfs_date=gfs_date-1;
      gfs_run_time=18;
      if gfs_date<gfs_date0-8;
        error(['  GFS: did not found anything'])
      end
    end 
  end
  warning on
end
%
% Get subgrid for GFS extraction
%
gfs_run_time_GFS=gfs_run_time; 
gfs_date_GFS=gfs_date;
fname=get_GFS_fname(gfs_date_GFS,gfs_run_time_GFS,gfsftype);
[i1min,i1max,i2min,i2max,i3min,i3max,jrange,lon,lat]=...
         get_GFS_subgrid(fname,lonmin,lonmax,latmin,latmax);
%
% Get mask on this grid
%
mask=getdap('',fname,'landsfc','[1:1]','',jrange,...
               i1min,i1max,i2min,i2max,i3min,i3max);
mask(mask==1)=NaN;
mask(isfinite(mask))=1;
%
% Initialize arrays with subgrid dimensions
%
Nrec=(hdays+fdays+1)*8/it;
[M,L]=size(mask);
tx=zeros(Nrec,M,L);
ty=tx;
tair=tx;
rhum=tx;
prate=tx;
wspd=tx;
radlw=tx;
radsw=tx;
radlw_in=tx;
uwnd=tx;
vwnd=tx;
%
n=0;
gfstime=0*(1:Nrec);
%
%==================================================
% 1: Get Hindcast data from past GFS forecast files
%==================================================
%
disp([char(10) ' ... READ HINDCAST FILES ... ' char(10)])
%
% Starting date : (hdays+1) days ago 06Z (12h will be added)
%
gfs_run_time=6;
gfs_date=today-(hdays+1); % (hdays+1) days ago 6Z
%
% Loop on past GFS forecast files 
%     - Starts (hdays+1) days ago 18Z
%     - Increment every 6h
%     - Ends yesterday 12Z
%
for ihind=1:4*hdays    % loop on files until time=yesterday 12Z 
%
  gfs_run_time=gfs_run_time+6;  % add 6 hours
  if gfs_run_time>18
    gfs_date=gfs_date+1;
    gfs_run_time=0;
  end
%
  tndxdap=2;  % take 3rd rec. of each file => add another 6 hours
%
% 1.1: check if GFS file is available.
%
  fname=get_GFS_fname(gfs_date,gfs_run_time,gfsftype);
  warning off
  try
    x=loaddap('-A -e +v ', fname);
    foundfile=1;
  catch
    foundfile=0;
  end
  if foundfile==1 & ~isempty(x)
    disp('  File found')
    gfs_run_time1=gfs_run_time;  % Increment Forecast 
    gfs_date1=gfs_date;          % Start time
    missvalue=x.ugrd10m.missing_value;
  else
    foundfile=0;
    disp(['  GFS: file not found, try next file'])
  end
  warning on
%
% 1.2: read file 
%
  if foundfile, 
   n=n+1;
   disp([char(10) 'N_hindcast=',num2str(n)])

   [gfstime(n),tx(n,:,:),ty(n,:,:),tair(n,:,:),rhum(n,:,:),...
              prate(n,:,:),wspd(n,:,:),uwnd(n,:,:),vwnd(n,:,:),...
              radlw(n,:,:),radlw_in(n,:,:),radsw(n,:,:)]=...
       get_GFS(fname,mask,tndxdap,jrange,i1min,i1max,i2min,i2max, ...
                                         i3min,i3max,missvalue);
  end 
end % ihind (nb of hindcast record)
%
%==================================================================
% 2: Get Forecast data, starting yesterday 18Z
%==================================================================
%
disp([char(10) ' ... READ FORECAST FILES ... ' char(10)])
%
% Get starting GFS date following last hindcast date
%
gfs_run_time=gfs_run_time1+6; % add 6 hours => 12Z
gfs_date=gfs_date1;
if gfs_run_time>18
  gfs_date=gfs_date+1;
  gfs_run_time=0;
end
%
% 2.1: check if GFS forecast is available for this time.
%      if not: take previous one (but increment time index)
%
t1=2;
foundfile=0;
while foundfile==0
  fname=get_GFS_fname(gfs_date,gfs_run_time,gfsftype);
  warning off
  try
    x=loaddap('-A -e +v ', fname);
    foundfile=1;
  catch
    foundfile=0;
  end
  if foundfile==1 & ~isempty(x)
    disp('  File found')
  else
    foundfile=0;
    disp(['  GFS : did not found ',fname])
    t1=t1+2;
    gfs_run_time=gfs_run_time-6;
    if gfs_run_time<0
      gfs_date=gfs_date-1;
      gfs_run_time=18;
      if gfs_date<gfs_date0-8;
        error(['  GFS: did not found anything'])
      end
    end
  end
  warning on
end
%
% 2.2: loop on records in same forecast file (dt = 3h * it)
%
tend=(fdays+1)*8;
for tndx=t1+1:it:tend
  tndxdap=tndx-1;
  n=n+1;
  disp([char(10) 'N_forecast=',num2str(n)])
  [gfstime(n),tx(n,:,:),ty(n,:,:),tair(n,:,:),rhum(n,:,:),...
              prate(n,:,:),wspd(n,:,:),uwnd(n,:,:),vwnd(n,:,:),...
              radlw(n,:,:),radlw_in(n,:,:),radsw(n,:,:)]=...
       get_GFS(fname,mask,tndxdap,jrange,i1min,i1max,i2min,i2max, ...
                                         i3min,i3max,missvalue);
end
%
%==================================================================
% 3: Finalize data processing and write in file
%==================================================================
%
% Reduce the matrices
%
gfstime=gfstime(1:n);
tx=tx(1:n,:,:);
ty=ty(1:n,:,:);
tair=tair(1:n,:,:);
rhum=rhum(1:n,:,:);
prate=prate(1:n,:,:);
wspd=wspd(1:n,:,:);
uwnd=uwnd(1:n,:,:);
vwnd=vwnd(1:n,:,:);
radlw=radlw(1:n,:,:);
radlw_in=radlw_in(1:n,:,:);
radsw=radsw(1:n,:,:);
%
% Set time in Yorig time
%
gfstime=gfstime-datenum(Yorig,1,1);
%
% Create the GFS output file and write everything down
%
mask(isnan(mask))=0;
write_GFS(gfs_name,Yorig,lon,lat,mask,gfstime,...
          tx,ty,tair,rhum,prate,wspd,uwnd,vwnd,radlw,radlw_in,radsw)
%
disp('Download GFS: done')
%
return




