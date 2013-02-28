function mercator_name=download_mercator_frcst_python(lonmin,lonmax,latmin,latmax,...
					       FRCST_dir,FRCST_prefix,url,Yorig)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Extract a subgrid from ECCO to get a ROMS forcing
%   Store that into monthly files.
%   Take care of the Greenwitch Meridian.
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
%Mercator login / password
user = '';
password = '';

%
%Path to file motu-client-python/
%
pathMotu ='../Forecast_tools/';

% Create the directory
%
disp(['Making output data directory ',FRCST_dir])
eval(['!mkdir ',FRCST_dir])
%
% Get the date
%
l=1;
rundate_str=date;
rundate=datenum(rundate_str)-datenum(Yorig,1,1);
lh=5; % length of hindcast for mercator
lf=6; % length of forecast

for i=1:lh
  time1(i)=datenum(rundate_str)-(lh+1-i);
end
time2=datenum(rundate_str);
for j=1:lf
  time3(j)=datenum(rundate_str)+j;
end
time=cat(2,time1,time2,time3);
tiempo_inicial = time1(1);
tiempo_final = time3(end);
if (lonmin > 180)
  lonmin = lonmin - 360;
end
if (lonmax > 180)
  lonmax = lonmax - 360;
end

get_file_python_mercator(pathMotu, ...
	     {'sea_surface_height_above_geoid' ...
	      'sea_water_x_velocity' ...
	      'sea_water_y_velocity' ...
	      'sea_water_potential_temperature' ...
	      'sea_water_salinity'}, ...
	     [lonmin-1 lonmax+1 latmin-1 latmax+1 0 5500], ...
	     {datestr(tiempo_inicial,'yyyy-mm-dd') datestr(tiempo_final,'yyyy-mm-dd')}, ...
	     {user password}, ...
	     url);
	   
nc = netcdf(url);
lon = nc{'longitude'}(:);
lat = nc{'latitude'}(:);
depth = nc{'depth'}(:);
close(nc)
%
% start
%
disp([' '])
disp(['Get data for ',rundate_str])
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
warning on
%
mercator_name=[FRCST_dir,FRCST_prefix,num2str(rundate),'.cdf'];
if ~exist(mercator_name)  
  extract_mercator_frcst_python(FRCST_dir,FRCST_prefix,url,...
			 time,Yorig)
else
  disp('Mercator file already exist')
end

return
