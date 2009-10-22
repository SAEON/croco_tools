function download_NCEP_Mydata(Ymin,Ymax,Mmin,Mmax,lonmin,lonmax,latmin,latmax,...
                       NCEP_dir,NCEP_version,Yorig)
%-----------------------------------------------------------------------
if NCEP_version==1
disp('Use my own ncep data NCEP1')
      ncep_url='/data1/gcambon/NCEP_REA1/';
      catalog={'land.sfc.gauss' ...
              'air.2m.gauss' ...
              'dlwrf.sfc.gauss' ...
              'skt.sfc.gauss' ... 
	      'dswrf.sfc.gauss' ...
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
	     'prate' ...     % surface precipitation rate [kg/m^2/s] 
	     'uwnd' ...      % 10 m u wind [m/s]
	     'vwnd' ...      % 10 m v wind [m/s]
	     'shum'};        % 2 m specific humidity [kg/kg]
  level ={'' '' '' '' '' '' '' '' ''};
else
disp('Use my own ncep data NCEP2')
      ncep_url='/data1/gcambon/NCEP_REA2/';
      catalog={'land.sfc.gauss' ...
               'air.2m.gauss' ...
               'dlwrf.sfc.gauss' ...
               'skt.sfc.gauss' ... 
	       'dswrf.sfc.gauss' ...
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
	     'prate' ...     % surface precipitation rate [kg/m^2/s] 
	     'uwnd' ...      % 10 m u wind [m/s]
	     'vwnd' ...      % 10 m v wind [m/s]
	     'shum'};        % 2 m specific humidity [kg/kg]
  level ={'' '' '' '' '' '' '' '' ''};  
  
  
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


 
% Get attribute for the time
%----------------------------    
   nc=netcdf([char(ncep_url),'prate.sfc.gauss.1995.nc']);
   xmi= nc{'time'}.units(:)
   close(nc)      
   if isempty(xmi)
     error('No time unit found');
   end
   eval(['Tunits=xmi;'])

if findstr('day',Tunits)
   time_scale = 1
 elseif findstr('hour',Tunits)
   time_scale = 1/24
 else
   error('Time units is not days or hours. I assume NCEP datasets are all daily ??! ');
end


%
%1  Find a subset of the NCEP grid
%---------------------------------
[i1min,i1max,i2min,i2max,i3min,i3max,jmin,jmax,lon,lat]=...
 get_NCEP_grid_Mydata([ncep_url,'land.sfc.gauss.nc'],'land',lonmin,lonmax,latmin,latmax);

%Boucle sur les variable   
for k0=1:length(vnames);
%for k0=1:2;
    k=k0;
    disp(['VNAMES K=',num2str(k)]);
    disp(['VNAME IS ',char(vnames(k))]);

if k0==1
disp('  Get the Land Mask tindex = 1');
tndx = 1;
extract_NCEP_Mask_Mydata(NCEP_dir,ncep_url,char(catalog(k)),...
		              Ymin,Mmin,...
                      lon,lat,level,...
                      i1min,i1max,i2min,i2max,i3min,i3max,...
                      jmin,jmax)
 else     

%Loop over year
%--------------
 for Y=Ymin:Ymax
  disp(['Processing year: ',num2str(Y)]);
  
%2-a Get the time indice to get into  monthly file
%--------------------------------------------------
nc=netcdf([ncep_url,char(catalog(k)),'.',num2str(Y),'.nc']);
time=nc{'time'}(:);
close(nc);
time(1)
time(1)/24
%----------------------------------------------------------------------------------
%Convert time relative to Yorig
%Here they are relative to start
% NCEP reanalysis 1 and 2 from NOMADS : time origine is "days since 1-1-1 00:00:00"
% NCEP reanalysis 1 from FTP: time origine is "days since 1-1-1 00:00:00"
% NCEP reanalysis 2 from FTP: time origine is "days since 1-1-1 00:00:00"
%-----------------------------------------------------------------------------------
if NCEP_version  == 1;             
disp(['NCEP_version is ',num2str(NCEP_version)])
startime = [1,1,1,0,0,0]; %[year, month, day, hour, minute, second]
elseif NCEP_version  ==  2;
disp(['NCEP_version is ',num2str(NCEP_version)])
startime = [1800,1,1,0,0,0]; %[year, month, day, hour, minute, second]
end

%---------------------------------------------------------------------------------
time = time.*time_scale;
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%time = time; %temps en heures
TIME_OFFSET=(mjd(Yorig,1,1,0)-mjd(startime(1),startime(2),startime(3),startime(4))); 
time = time - TIME_OFFSET - 2;    %  This is time in days from Yorig
                                  %  We have to remove at the end to be 
				  %  OK with the date ??!! !!!!
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[year,month,days,hour,min,sec]=datevec(time+datenum(Yorig,1,1)); 
% $$$ year' ;
% $$$ month';
% $$$ disp('time(1:4)')
% $$$ time(1:4)
% $$$ 
% $$$ disp('year(1:4)')
% $$$ year(1:4)
% $$$ 
% $$$ disp('month(1:4)')
% $$$ month(1:4)
% $$$ 
% $$$ 
% $$$ time(end-3:end)
% $$$ year(end-3:end)
% $$$ month(end-3:end)

%disp(['Month=',month])
%disp(['Year=',num2str(year)])
%size(time)
%pause
%year(1:10)
%month(1:10)


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
   disp(['  Processing month: ',num2str(M)]);
   recipe_file = [NCEP_dir,char(vnames(k)),'_Y',num2str(Y),'M', num2str(M),'.nc'];
   disp(['Recipe file=',recipe_file])
%
% Get the time indices for this month and year   
%   
   tndx=intersect(find(month==M),find(year==Y));   

%   disp(['size(tndx)=',num2str(size(tndx))]);
%   disp(['tndx(1)=',num2str(tndx(1))])
%   disp(['tndx(end)=',num2str(tndx(end))])
%   disp(['time(tndx(1))=',num2str(time(tndx(1)))])
%   disp(['time(tndx(end))=',num2str(time(tndx(end)))])

%
% Get the subset  
%
% $$$   NCEP_dir
% $$$   ncep_url
% $$$   char(catalog(k))
% $$$   char(vnames(k))
% $$$   tndx;
% $$$   time(tndx);
  extract_NCEP_Mydata(NCEP_dir,ncep_url,char(catalog(k)),...
		      char(vnames(k)),Y,M,...
                      lon,lat,tndx,time(tndx),level,...
                      i1min,i1max,i2min,i2max,i3min,i3max,...
                      jmin,jmax,Yorig)
  end % end loop month
 end % end loop year
end %for k

end   %land ou les autres variables
