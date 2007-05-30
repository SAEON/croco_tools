function download_NCEP(Ymin,Ymax,Mmin,Mmax,lonmin,lonmax,latmin,latmax,...
                       NCEP_dir,NCEP_version,Yorig)
%
% Extract a subgrid from NCEP to get a ROMS forcing
% Store that into monthly files (to limit the problems
% of bandwith...).
% Take care of the Greenwitch Meridian.
% 
% Pierrick 2005
%

%clear all
%close all
%nargin=0;
%
if nargin < 1
  Ymin=2003;
  Ymax=2003;
  Yorig=2003;
  Mmin=1;
  Mmax=1;
  lonmin=-90;
  lonmax=-70;
  latmin=-20;
  latmax=-5;
  NCEP_dir='../Forcing_data/NCEP_Peru/';
  NCEP_version=1;
end
%
if NCEP_version==1
%
% First dataset
%
  url1='http://www.cdc.noaa.gov/cgi-bin/nph-nc/Datasets/ncep.reanalysis/surface_gauss/';
  land1_name='land.sfc.gauss.nc';
  name1={'air.2m.gauss.' 'nlwrs.sfc.gauss.' 'nswrs.sfc.gauss.' 'prate.sfc.gauss.'...
         'uwnd.10m.gauss.' 'vwnd.10m.gauss.'};
  level1={'' '' '' '' '' ''};
%
% Second dataset
%
  url2='http://www.cdc.noaa.gov/cgi-bin/nph-nc/Datasets/ncep.reanalysis/surface/';
  land2_name='land.nc';
  name2={'rhum.sig995.'};
  level2={''};
%
elseif NCEP_version==2
%
% One dataset only...
%
  url1='http://www.cdc.noaa.gov/cgi-bin/nph-nc/Datasets/ncep.reanalysis2/gaussian_grid/';
  land1_name='land.sfc.gauss.nc';
  name1={'air.2m.gauss.' 'dlwrf.sfc.gauss.' 'dswrf.sfc.gauss.' 'prate.sfc.gauss.'...
         'uwnd.10m.gauss.' 'vwnd.10m.gauss.' 'shum.2m.gauss.' 'skt.sfc.gauss.'};
  level1={'[0]' '' '' '' '[0]' '[0]' '[0]' ''};
else
  error('Wrong NCEP version')
end
%
% start
%
disp([' '])
disp(['Get NCEP data from ',num2str(Ymin),' to ',num2str(Ymax)])
disp(['Minimum Longitude: ',num2str(lonmin)])
disp(['Maximum Longitude: ',num2str(lonmax)])
disp(['Minimum Latitude: ',num2str(latmin)])
disp(['Maximum Latitude: ',num2str(latmax)])
disp([' '])
%
% Create the directory
%
disp(['Making output data directory ',NCEP_dir])
eval(['!mkdir ',NCEP_dir])
%
% Start with the first NCEP dataset
%
disp(['Process the first dataset: ',url1])
%
% Find a subset of the NCEP grid
%
[i1min,i1max,i2min,i2max,i3min,i3max,jrange,lon,lat,mask]=...
get_NCEP_grid(url1,land1_name,lonmin,lonmax,latmin,latmax);
%
% Write out the land mask
%
write_NCEP([NCEP_dir,land1_name],'land',lon,lat,0,mask,Yorig)
%
% Loop on the years
%
for Y=Ymin:Ymax
  disp(['Processing year: ',num2str(Y)])
%
% Get the time vector for this year
%
  time=readdap([url1,char(name1(1)),num2str(Y),'.nc'],'time',[]);
%
% Convert the time into "Yorig" time (i.e in days since Yorig/1/1 00:00:0.0)
%
  if NCEP_version==1
    time=365+(time/24)-datenum(Yorig,1,1);
  elseif NCEP_version==2
    time=datenum(1800,1,1)+(time/24)-datenum(Yorig,1,1);
  end
  [year,month,days,hour,min,sec]=datevec(time+datenum(Yorig,1,1));
%
% Loop on the months
%
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
    disp(['  Processing month: ',num2str(M)])
%
% Get the time indices for this month
%
    tndx=find(month==M);
    trange=['[',num2str(tndx(1)-1),':',num2str(tndx(end)-1),']'];
%
% Loop on the names
%
    for i=1:length(name1)
      extract_NCEP(NCEP_dir,url1,char(name1(i)),Y,M,...
                   lon,lat,time(tndx),...
                   trange,char(level1(i)),jrange,...
                   i1min,i1max,i2min,i2max,i3min,i3max,...
                   Yorig)
    end
  end
end
if NCEP_version==1
%
% Start with the second NCEP dataset
%
  disp(['Process the second dataset: ',url2])
%
% Get a subset of the NCEP grid
%
  [i1min,i1max,i2min,i2max,i3min,i3max,jrange,lon,lat,mask]=...
  get_NCEP_grid(url2,land2_name,lonmin,lonmax,latmin,latmax);
%
% Write the land mask
%
  write_NCEP([NCEP_dir,land2_name],'land',lon,lat,0,mask,Yorig)
%
% Loop on the years
%
  for Y=Ymin:Ymax
    disp(['Processing year: ',num2str(Y)])
%
% Get the time vector for this year
%
    time=readdap([url2,char(name2(1)),num2str(Y),'.nc'],'time',[]);
%
% Convert it into "Yorig" time
%
    time=365+(time/24)-datenum(Yorig,1,1);
    [year,month,days,hour,min,sec]=datevec(time+datenum(Yorig,1,1));
%
% Loop on the months
%
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
      disp(['  Processing month: ',num2str(M)])
%
% Get the time indices for this month
%
      tndx=find(month==M);
      trange=['[',num2str(tndx(1)-1),':',num2str(tndx(end)-1),']'];
%
% Loop on the names
%
      for i=1:length(name2)
        extract_NCEP(NCEP_dir,url2,char(name2(i)),Y,M,...
                     lon,lat,time(tndx),...
                     trange,char(level2(i)),jrange,...
                     i1min,i1max,i2min,i2max,i3min,i3max,...
                     Yorig)
      end
    end
  end
end
return
