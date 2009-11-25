function download_NCEP(Ymin,Ymax,Mmin,Mmax,lonmin,lonmax,latmin,latmax,...
                       NCEP_dir,NCEP_version,Yorig,Get_My_Data)
%
% Extract a subgrid from NCEP to get a ROMS forcing
% Store that into monthly files (to limit the problems
% of bandwith...).
% Take care of the Greenwitch Meridian.
% 
% Pierrick 2005
%Update J. Lefevre, feb-2008 to use Nomads1 server(nomad3 is dead)
%Update G. Cambon, Oct-2009

if Get_My_Data ~= 1
 disp(['================='])
 disp(['OPENDAP Procedure'])
 disp(['================='])
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

elseif Get_My_Data == 1
 disp(['================='])
 disp(['Direct FTP Procedure'])
 disp(['================='])
 
 if NCEP_version==1
disp('Use my own ncep data NCEP1')
      ncep_url='/data1/gcambon/NCEP_REA1/';
      catalog={'land.sfc.gauss' ...
              'air.2m.gauss' ...
              'dlwrf.sfc.gauss' ...
              'skt.sfc.gauss' ... 
	      'dswrf.sfc.gauss' ...
	      'uswrf.sfc.gauss' ...
	      'prate.sfc.gauss' ...
	      'uwnd.10m.gauss' ...
	      'vwnd.10m.gauss' ...
	      'shum.2m.gauss' ...
	          ''};
     vnames={'land'...    % surface land-sea mask [1=land; 0=sea] 
             'air' ...       % 2 m temp. [k]
             'dlwrf' ...     % surface downward long wave flux [w/m^2] 
             'skt' ...       % surface temp. [k]   		  
	     'dswrf' ...     % surface downward solar radiation flux [w/m^2] 
	     'uswrf' ...     % surface upward solar radiation flux [w/m^2] 
	     'prate' ...     % surface precipitation rate [kg/m^2/s] 
	     'uwnd' ...      % 10 m u wind [m/s]
	     'vwnd' ...      % 10 m v wind [m/s]
	     'shum'};        % 2 m specific humidity [kg/kg]
  level ={'' '' '' '' '' '' '' '' '' ''};
 else
disp('Use my own ncep data NCEP2')
      ncep_url='/data1/gcambon/NCEP_REA2/';
      catalog={'land.sfc.gauss' ...
               'air.2m.gauss' ...
               'dlwrf.sfc.gauss' ...
               'skt.sfc.gauss' ... 
	       'dswrf.sfc.gauss' ...
	       'uswrf.sfc.gauss' ...
	       'prate.sfc.gauss' ...
	       'uwnd.10m.gauss' ...
	       'vwnd.10m.gauss' ...
	       'shum.2m.gauss' ...
	          ''};
     vnames={'land'...       % surface land-sea mask [1=land; 0=sea] 
             'air' ...       % 2 m temp. [k]
             'dlwrf' ...     % surface downward long wave flux [w/m^2] 
             'skt' ...       % surface temp. [k]   		  
	     'dswrf' ...     % surface downward solar radiation flux [w/m^2] 
	     'uswrf' ...     % surface upward solar radiation flux [w/m^2] 
	     'prate' ...     % surface precipitation rate [kg/m^2/s] 
	     'uwnd' ...      % 10 m u wind [m/s]
	     'vwnd' ...      % 10 m v wind [m/s]
	     'shum'};        % 2 m specific humidity [kg/kg]
  level ={'' '' '' '' '' '' '' '' '' ''};  
 end 
end  %endif Get_My_Data

%
%Common OpenDAP FTP
%

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
% End Common OpenDap FTP 
%-----------------------

for k=1:length(vnames)
   disp([''])
   disp(['=========================='])
   disp(['VNAME IS ',char(vnames(k))]);
   disp(['--------------------------'])

%
% Time unit (and time in case of opendap) section
%

if Get_My_Data~=1
disp(['------'])
disp(['Get time units and time:  Get_My_Data is OFF '])
disp(['------'])  
 x=loaddap('-A -e +v',[ncep_url,char(catalog(k))]);
  if (dods_err==1)
        error(dods_err_msg)
  end
% check time unit (APDRC dataset are daily)
  if isempty(x.time.units)
        error('No time unit found');
  end
eval(['Tunits=x.time.units;']);  
time=readdap([ncep_url,char(catalog(k))],'time',[]);
time(1:20)
 if findstr('day',Tunits)
   time_scale = 1;
 elseif findstr('hour',Tunits)
   time_scale = 1/24;
 else
   error('Time units is not days or hours. I assume NCEP datasets are all daily ??! ');
 end

else %Get_My_Data~=1
  if k==1  %Read time only one time
disp(['------'])
disp(['Get time units only :  Get_My_Data is OFF '])
disp(['Read file: ',[char(ncep_url),'prate.sfc.gauss.1995.nc or whatever', ...
		   ' NCEP file']])
disp(['------'])    
nc=netcdf([char(ncep_url),'prate.sfc.gauss.1995.nc']);
xmi= nc{'time'}.units(:);
close(nc)
    if isempty(xmi)
     error('No time unit found');
   end
eval(['Tunits=xmi;'])

   if findstr('day',Tunits)
     time_scale = 1;
   elseif findstr('hour',Tunits)
     time_scale = 1/24;
   else
     error('Time units is not days or hours. I assume NCEP datasets are all daily ??! ');
   end
  
  end  %k==1
end    %Get_My_Data~=1

%
% Subgrid section :Find a subset of the NCEP grid
%

if k==1
disp(['================================='])
disp(['GET  SUBGRID time only k=1']);
disp(['USE VARIABLE: ',char(vnames(k))  ])
disp(['================================='])

[i1min,i1max,i2min,i2max,i3min,i3max,jrange,jmin,jmax,lon,lat]=...
get_NCEP_grid([ncep_url,char(catalog(k))],lonmin,lonmax,latmin,latmax,Get_My_Data);

disp(['================================='])
disp(['i1min=',num2str(i1min)])
disp(['i1max=',num2str(i1max)])
disp(['i2min=',num2str(i2min)])
disp(['i2max=',num2str(i2max)])
disp(['i3min=',num2str(i3min)])
disp(['i3max=',num2str(i3max)])
disp(['jmin=',num2str(jmin)])
disp(['jmax=',num2str(jmax)])
disp(['lon(1)= ',num2str(lon(1))])
disp(['lon(end)= ',num2str(lon(end))])
disp(['lat(1)= ',num2str(lat(1))])
disp(['lat(end)= ',num2str(lat(end))])
disp(['================================='])
end %if k==1

 
if k==1
  if Get_My_Data~=1
 disp('GET LAND MASK');
 tndx = 1;
 trange = '[1:1]';
 extract_NCEP(NCEP_dir,ncep_url,char(catalog(k)),char(vnames(k)),Ymin,Mmin,...
              lon,lat,tndx,time(tndx),...
              trange,char(level(k)),jrange,...
              i1min,i1max,i2min,i2max,i3min,i3max,...
              jmin,jmax,Yorig,Get_My_Data) 
 
  elseif Get_My_Data==1
   disp([' '])
   disp(['============================='])
   disp(['Get the Land Mask tindex = 1']);
   disp(['In case of Get_My_Data ON']);
   disp(['Get the Land Mask by using extract_NCEP_Mask_Mydata']);
   disp(['Execute extract_NCEP_Mask_Mydata'])
   disp([' ']) 
   extract_NCEP_Mask_Mydata(NCEP_dir,ncep_url,char(catalog(k)),...
		      Ymin,Mmin,...
                      lon,lat,level,...
                      i1min,i1max,i2min,i2max,i3min,i3max,...
                      jmin,jmax)

  end  %Get_My_Data
else %if k==1

%
% Loop on the years
%
 for Y=Ymin:Ymax
  disp([' '])
  disp(['============================='])
  disp(['Processing year: ',num2str(Y)])
  disp(['============================='])
  disp([' '])

   if Get_My_Data==1    
%Get the time indice to get into  monthly file
%-------------------
nc=netcdf([ncep_url,char(catalog(k)),'.',num2str(Y),'.nc']);
time=nc{'time'}(:);
close(nc);
%===================================
%in case of Get_My_Data~=1, time is already loaded 
%in the time unit section previoulsly 
%=====================================
   end

%size(time)
%time(1)
%time(1)/24

%
%Convertion of time and Yorig section
%
%==========================================================
%Convert time relative to Yorig
%Here they are relative to start
% NCEP reanalysis 1 and 2 from NOMADS : time origine is "days since 1-1-1 00:00:00"
% NCEP reanalysis 1 from FTP: time origine is "days since 1-1-1 00:00:00"
% NCEP reanalysis 2 from FTP: time origine is "days since 1800-1-1 00:00:00"
%===========================================================

   if (NCEP_version == 1 && Get_My_Data~= 1)   ;             
disp(['NCEP_version is ',num2str(NCEP_version)])
startime = [1,1,1,0,0,0]; %[year, month, day, hour, minute, second]

   elseif (NCEP_version ==  2 && Get_My_Data ~= 1);
disp(['NCEP_version is ',num2str(NCEP_version)])
startime = [1,1,1,0,0,0]; %[year, month, day, hour, minute, second]

   elseif (NCEP_version ==  1 && Get_My_Data == 1);
disp(['NCEP_version is ',num2str(NCEP_version)])
startime = [1,1,1,0,0,0]; %[year, month, day, hour, minute, second]

   elseif (NCEP_version ==  2 && Get_My_Data == 1);
disp(['NCEP_version is ',num2str(NCEP_version)])
startime = [1800,1,1,0,0,0]; %[year, month, day, hour, minute, second]

   end
time = time.*time_scale;
TIME_OFFSET=(mjd(Yorig,1,1,0)-mjd(startime(1),startime(2),startime(3), ...
				  startime(4))); 
  if Get_My_Data~= 1
time = time - TIME_OFFSET -2; 
%                             This is time in days from Yorig
%                             In case of OpenDap DATA
%                             we have to remove at the end to be OK with the date
  else
time = time - TIME_OFFSET  ;
%                             This is time in days from Yorig
%                             In case ofFTP DATA
%                             we do not need to remove at the end
 end
[year,month,days,hour,min,sec]=datevec(time+datenum(Yorig,1,1));
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
   recipe_file = [NCEP_dir,char(vnames(k)),'_Y',num2str(Y),'M',num2str(M),'.nc'];
    if ~exist(recipe_file)

%disp(['month(1:10)= '])
%month(1:10)
%month(end-10:end)
%disp(['day(1:10)= '])
%days(1:10)
%days(end-10:end)
%disp(['year(1:end)= '])
%year(1:10)
%year(end-10:end)
%year(1:end)

%
% Get the time indices for this month and year
%
tndx=intersect(find(month==M),find(year==Y));
%intersect.m work for matlab7 check for matlab 6

%size(month);
%size(year);
%Y;
%tndx;

%trange=['[',num2str(tndx(1)-1),':',num2str(tndx(end)-1),']'];
trange=['[',num2str(tndx(1)),':',num2str(tndx(end)),']'];
disp(['TRANGE=',num2str(trange)]);

%
% Get the subset
%
    extract_NCEP(NCEP_dir,ncep_url,char(catalog(k)),...
	      char(vnames(k)),Y,M,...
              lon,lat,tndx,time(tndx),...
              trange,char(level(k)),jrange,...
              i1min,i1max,i2min,i2max,i3min,i3max,...
              jmin,jmax,Yorig,Get_My_Data)
    else
    disp(['File ',recipe_file,' already exists, abort.']);
    end % if file exist 
  end % end loop month
  
 end % end loop year
 
end % end if k

end % end loop variable    

return
