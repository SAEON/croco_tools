function download_GFS(today,lonmin,lonmax,latmin,latmax,FRCST_dir,Yorig)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  download_GFS(today,lonmin,lonmax,latmin,latmax,FRCST_dir,Yorig)
%
%
%  Extract a subgrid from GFS to get a ROMS forcing
%  Store that into monthly files (to limit the problems
%  of bandwith...).
%  Take care of the Greenwitch Meridian.
%  Transform variables in the ROMS format.
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
%  Updated    9-Sep-2006 by Pierrick Penven
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Put the date in 'Yorig' time
%
rundate=datenum(today)-datenum(Yorig,1,1);
%
% GFS output name
%
gfs_name=[FRCST_dir,'GFS_',num2str(rundate),'.nc'];
%
% start
%
disp([' '])
disp(['Get GFS data for ',datestr(today)])
disp(['Minimum Longitude: ',num2str(lonmin)])
disp(['Maximum Longitude: ',num2str(lonmax)])
disp(['Minimum Latitude: ',num2str(latmin)])
disp(['Maximum Latitude: ',num2str(latmax)])
disp([' '])
%
% Create the directory
%
disp(['Making output data directory ',FRCST_dir])
eval(['!mkdir ',FRCST_dir])
%
% Get the GFS file name (first check if there is an availlable forecast)
%
gfs_run_time0=0;
gfs_date0=today;
gfs_run_time=gfs_run_time0;
gfs_date=gfs_date0;
foundfile=0;
while foundfile==0
  fname=get_GFS_fname(gfs_date,gfs_run_time);
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
% Get the grid and the indices of the subgrid.
%
[i1min,i1max,i2min,i2max,i3min,i3max,jrange,lon,lat]=...
         get_GFS_subgrid(fname,lonmin,lonmax,latmin,latmax);
mask=getdap('',fname,'landsfc','[0:0]','',jrange,...
             i1min,i1max,i2min,i2max,i3min,i3max);
mask(mask==1)=NaN;
mask(isfinite(mask))=1;
%
% Get the grid size
%
[M,L]=size(mask);
N=70;
%
% initialise the variables
%
gfstime=0*(1:N);
tx=zeros(N,M,L);
ty=tx;
tair=tx;
rhum=tx;
prate=tx;
wspd=tx;
radlw=tx;
radsw=tx;
%
% Get the variables for the hindcast for the previous day.
% today-2 18Z, today-1 00Z, today-1 06Z, today-1 12Z, today-1 18Z
%
n=0;
%
% Loop on GFS forecasts (starts 2 day ago 18Z, 1 forecast every 6h).
%
gfs_run_time0=12;
gfs_date0=today-2;
for frcst=1:5
  gfs_run_time0=gfs_run_time0+6;
  if gfs_run_time0>18
    gfs_date0=gfs_date0+1;
    gfs_run_time0=0;
  end
%
% 1.1: check if the GFS forecast has been done for this time.
% if not: take the previous one
%
  gfs_run_time=gfs_run_time0;
  gfs_date=gfs_date0;
  t1=1;
  foundfile=0;
  while foundfile==0
    fname=get_GFS_fname(gfs_date,gfs_run_time);
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
% 1.2: read 2 time steps (dt = 3h)
%
  for tndx=t1:t1+1
    n=n+1;
    [gfstime(n),tx(n,:,:),ty(n,:,:),tair(n,:,:),rhum(n,:,:),...
     prate(n,:,:),wspd(n,:,:),radlw(n,:,:),radsw(n,:,:)]=...
     get_GFS(fname,mask,tndx,jrange,i1min,i1max,i2min,i2max,...
             i3min,i3max);
  end
end
%
% Get the variables 2: Forecast 00Z
%
gfs_run_time0=0;
gfs_date0=today;
%
% 2.1: check if the GFS forecast has been done for this time.
% if not: take the previous one
%
gfs_run_time=gfs_run_time0;
gfs_date=gfs_date0;
t1=1;
foundfile=0;
while foundfile==0
  fname=get_GFS_fname(gfs_date,gfs_run_time);
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
% 2.2: read 2 time steps (dt = 3h)
%
for tndx=t1:60
%for tndx=t1:5
  n=n+1;
  [gfstime(n),tx(n,:,:),ty(n,:,:),tair(n,:,:),rhum(n,:,:),...
   prate(n,:,:),wspd(n,:,:),radlw(n,:,:),radsw(n,:,:)]=...
   get_GFS(fname,mask,tndx,jrange,i1min,i1max,i2min,i2max,...
           i3min,i3max);
end
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
radlw=radlw(1:n,:,:);
radsw=radsw(1:n,:,:);
%
% Put the time in Yorig time
%
gfstime=gfstime-datenum(Yorig,1,1);
%
% Create the GFS output file and write everything down
%
mask(isnan(mask))=0;
write_GFS(gfs_name,Yorig,lon,lat,mask,gfstime,tx,ty,tair,rhum,prate,wspd,radlw,radsw)
%
disp('Download GFS: done')
%
return




