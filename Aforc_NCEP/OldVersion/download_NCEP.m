function download_NCEP(Ymin,Ymax,Mmin,Mmax,lonmin,lonmax,latmin,latmax,...
                       NCEP_dir,NCEP_version,Yorig)
%
% Extract a subgrid from NCEP to get a ROMS forcing
% Store that into monthly files (to limit the problems
% of bandwith...).
% Take care of the Greenwitch Meridian.
% 
% Pierrick 2005
% Update J. Lefevre, feb-2008 to use Nomads1 server(nomad3 is dead)

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
disp(['latmax=',num2str(latmax)])
if NCEP_version==1
%
% nomad3 server reanalysis-1
  ncep_url='http://nomad1.ncep.noaa.gov:9090/dods/reanalyses/reanalysis-1/';
  catalog={'6hr/grb2d/grb2d' ...
           '6hr/grb2d/grb2d' ...
           '6hr/grb2d/grb2d' ...
	   '6hr/grb2d/grb2d' ...
	   '6hr/grb2d/grb2d' ...
	   '6hr/grb2d/grb2d' ...
	   '6hr/grb2d/grb2d' ...
	   '6hr/grb2d/grb2d' ...
	   '6hr/grb2d/grb2d' ...
	   '6hr/grb2d/grb2d'};  
  vnames={'landsfc' ...      % surface land-sea mask [1=land; 0=sea] 
          'tmp2m' ...        % 2 m temp. [k]
          'dlwrfsfc' ...     % surface downward long wave flux [w/m^2] 
          'tmpsfc' ...       % surface temp. [k]   		  
	  'dswrfsfc' ...     % surface downward solar radiation flux [w/m^2]
	  'uswrfsfc' ...     % surface upward solar radiation flux [w/m^2]  
	  'pratesfc' ...     % surface precipitation rate [kg/m^2/s] 
	  'ugrd10m' ...      % 10 m u wind [m/s]
	  'vgrd10m' ...      % 10 m v wind [m/s]
	  'spfh2m'};         % 2 m specific humidity [kg/kg]
  level ={'' '' '' '' '' '' '' '' '' ''};
%
elseif NCEP_version==2
%http://nomad3.ncep.noaa.gov:9090/dods/reanalyses/reanalysis-2/month/flx/flx
% nomad3 server reanalysis-2
  ncep_url='http://nomad1.ncep.noaa.gov:9090/dods/reanalyses/reanalysis-2/';
  catalog={'6hr/flx/flx' ...
           '6hr/flx/flx' ...
           '6hr/flx/flx' ...
	   '6hr/flx/flx' ...
	   '6hr/flx/flx' ...
	   '6hr/flx/flx' ...
	   '6hr/flx/flx' ...
	   '6hr/flx/flx' ...
	   '6hr/flx/flx' ...
	   '6hr/flx/flx'};  
  vnames={'landsfc' ...    % surface land-sea mask [1=land; 0=sea] 
          'tmp2m' ...      % 2 m temp. [k]
          'dlwrfsfc' ...   % surface downward long wave flux [w/m^2] 
          'tmpsfc' ...     % surface temp. [k]   
	  'dswrfsfc' ...   % surface downward solar radiation flux [w/m^2]
	  'uswrfsfc' ...   % surface upward solar radiation flux [w/m^2] 
	  'pratesfc' ...   % surface precipitation rate [kg/m^2/s] 
	  'ugrd10m' ...    % 10 m u wind [m/s]
	  'vgrd10m' ...    % 10 m v wind [m/s]
	  'spfh2m'};       % 2 m specific humidity [kg/kg]
  level ={'' '' '' '' '' '' '' '' '' ''};

else
  error('Wrong NCEP version')
end

  %-----------------------------------------------------------------------

disp([' '])
disp(['Get NCEP data from ',num2str(Ymin),' to ',num2str(Ymax)])
disp(['From ',ncep_url]);
disp([' '])
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


for k=1:length(vnames)
disp(['-------------------------'])
disp([''])
disp([''])
disp(['VNAME IS ',char(vnames(k))]);
disp(['--------------------------'])
% Get attribute
 x=loaddap('-A -e +v',[ncep_url,char(catalog(k))]);
 if (dods_err==1)
   error(dods_err_msg)
 end

% check time unit (APDRC dataset are daily)
 if isempty(x.time.units)
  error('No time unit found');
 end
 eval(['Tunits=x.time.units;']);

 if findstr('day',Tunits)
   time_scale = 1;
 elseif findstr('hour',Tunits)
   time_scale = 1/24;
 else
   error('Time units is not days or hours. I assume NCEP datasets are all daily ??! ');
 end

% NCEP reanalysis 1 and 2 from NOMADS : time origine is "days since 1-1-1 00:00:00"
startime = [1,1,1,0,0,0]; %[year, month, day, hour, minute, second]
 
 time=readdap([ncep_url,char(catalog(k))],'time',[]);
 time = time*time_scale;

%% time = time - time(1);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
TIME_OFFSET=(mjd(Yorig,1,1,0)-mjd(startime(1),startime(2),startime(3),startime(4))); 
% time = time - TIME_OFFSET; % This is time in days from Yorig
time = time - TIME_OFFSET - 2;    %  This is time in days from Yorig
                                  %  We have to remove at the end to be 
				  %  OK with the date  !!!!				  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[year,month,days,hour,min,sec]=datevec(time+datenum(Yorig,1,1));
%year' ;
%month';
%time(1:4)
%year(1:4)
%month(1:4)
%disp('---------')
%time(end-3:end)
%year(end-3:end)
%month(end-3:end)
%disp('---------')
%size(time)
%year(1:10)
%month(1:10)


% Find a subset of the NCEP grid
 [i1min,i1max,i2min,i2max,i3min,i3max,jrange,lon,lat]=...
 get_NCEP_grid([ncep_url,char(catalog(k))],lonmin,lonmax,latmin,latmax);
 

disp(['latmax=',num2str(latmax)])
% 
if k==1
 disp('  Get the Land Mask tindex = 1');
 tndx = 1;
 trange = '[1:1]';
 extract_NCEP(NCEP_dir,ncep_url,char(catalog(k)),char(vnames(k)),Ymin,Mmin,...
              lon,lat,time(tndx),...
              trange,char(level(k)),jrange,...
              i1min,i1max,i2min,i2max,i3min,i3max,...
              Yorig) 
else
% Loop on the years
 for Y=Ymin:Ymax
  disp(['Processing year: ',num2str(Y)])

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
   recipe_file = [NCEP_dir,char(vnames(k)),'_Y',num2str(Y),'M',num2str(M),'.nc'];
   if ~exist(recipe_file)
   % Get the time indices for this month and year
    tndx=intersect(find(month==M),find(year==Y));

%intersect.m work for matlab7 check for matlab  6 
%----------------------------------------------

trange=['[',num2str(tndx(1)-1),':',num2str(tndx(end)-1),']'];
disp(['TRANGE=',num2str(trange)])
   % Get the subset
    extract_NCEP(NCEP_dir,ncep_url,char(catalog(k)),char(vnames(k)),Y,M,...
              lon,lat,time(tndx),...
              trange,char(level(k)),jrange,...
              i1min,i1max,i2min,i2max,i3min,i3max,...
              Yorig)
   else
    disp(['File ',recipe_file,' already exists, abort.']);
   end % if file exist 
  end % end loop month
 end % end loop year
end % end if k
end % end loop variable

return
