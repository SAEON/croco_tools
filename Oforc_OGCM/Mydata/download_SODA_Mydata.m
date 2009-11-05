function download_SODA_Mydata(Ymin,Ymax,Mmin,Mmax,lonmin,lonmax,latmin,latmax,...
                       OGCM_dir,OGCM_prefix,url_mydata,Yorig)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% In case of specific data, in local, not using the OpenDap procedure
% 
% Extract a subgrid from SODA to get a ROMS forcing
% Store that into monthly files.
% Take care of the Greenwitch Meridian.
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
%  Copyright (c) 2005-2006 by Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr  
%
%  Updated    6-Sep-2006 by Pierrick Penven
%  Updated    13-Sep-2006 by Gildas Cambon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

disp([' '])
disp(['Get (from My data) data from Y',num2str(Ymin),'M',num2str(Mmin),...
      ' to Y',num2str(Ymax),'M',num2str(Mmax)])
disp(['Minimum Longitude: ',num2str(lonmin)])
disp(['Maximum Longitude: ',num2str(lonmax)])
disp(['Minimum Latitude: ',num2str(latmin)])
disp(['Maximum Latitude: ',num2str(latmax)])
disp([' '])
%
% Create the directory
%
disp(['Making output data directory ',OGCM_dir])
eval(['!mkdir ',OGCM_dir])
%
% Start 
%%%%%%%%%
%disp(['URL PATH='])
%url_mydata='/media/disk/LINUX/GALA6/SODA_2.0.1/5dmean/'
%%%%%%%%

disp(['Process the dataset: ',url_mydata])
%
% Find a subset of the SODA grid
[i1min,i1max,i2min,i2max,i3min,i3max,jmin,jmax,jrange,k,krange,lon,lat,depth]=...
get_SODA_subgrid_Mydata(url_mydata,lonmin,lonmax,latmin,latmax);

%
% Build the vectors 35 ans in days since since 1960-01-01)
SODA_time0=[1:5:73*5*55];

% Transform it into Yorig time (i.e days since Yorig-01-01)
%
[year0,month0,day0]=datevec(SODA_time0);
year=1960+year0;
month=month0;
SODA_time=datenum(year,month,day0)-datenum(Yorig,1,1) - 1;

% Loop on the years
%
for Y=Ymin:Ymax
  disp(['Processing year: ',num2str(Y)])
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
% Get the time indice for this year and month
%
    tndx=find(month==M & year==Y);
    tndx2=find(year==Y);
    [year1,month1,day1]=datevec(SODA_time0(tndx2));
    tndx3=find(month1==M);
    
    disp(['TNDX3= index in the month...',num2str(tndx3)])
    disp(['SODA_time associated relative to Yorig time=...',num2str(SODA_time(tndx))])

%-------------------------------------------------------------
% Special treatments of leap year as 2000 for exemple
%-------------------------------------------------------------
lp_year=leap_year(Y);

if (lp_year==1 & M==12)
   disp(['Year',num2str(Y),' is a leap year'])
   disp(['Correct the last month...'])
   tndx=tndx(1:end-1);
   tndx3=tndx3(1:end-1);
   disp(['TNDX3= index in the month...',num2str(tndx3)])
   disp(['SODA_time associated relative to Yorig time=...',num2str(SODA_time(tndx))])
end 
%-------------------------------------------------------    
  
%
% Extract SODA data
%

[u1,v1,temp1,salt1,taux1,tauy1,ssh1]=extract_SODA_Mydata(OGCM_dir,OGCM_prefix,...
		 url_mydata,Y,M,...
                 lon,lat,depth,tndx,tndx3,SODA_time,...
                 krange,jmin,jmax,...
                 i1min,i1max,i2min,i2max,i3min,i3max,...
                 Yorig);

 end
end
return
