function download_QSCAT(Ymin,Ymax,Mmin,Mmax,lonmin,lonmax,latmin,latmax,...
                        QSCAT_dir,Yorig)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Extract a subgrid from QSCAT to get a ROMS forcing
% Store that into monthly files (to limit the problems
% of bandwith...).
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
%  Copyright (c) 2007 by Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr  
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
if nargin < 1
  Ymin=2000;
  Ymax=2000;
  Yorig=1900;
  Mmin=1;
  Mmax=3;
  lonmin=12.3;
  lonmax=20.45;
  latmin=-35.5;
  latmax=-26.5;
  QSCAT_dir='DATA/QSCAT_Benguela/';
end
%
url='http://www.ifremer.fr/dodsG/CERSAT/quikscat_daily';
%
% start
%
disp([' '])
disp(['Get QSCAT wind from ',num2str(Ymin),' to ',num2str(Ymax)])
disp(['Minimum Longitude: ',num2str(lonmin)])
disp(['Maximum Longitude: ',num2str(lonmax)])
disp(['Minimum Latitude: ',num2str(latmin)])
disp(['Maximum Latitude: ',num2str(latmax)])
disp([' '])
%
% Create the directory
%
disp(['Making output data directory ',QSCAT_dir])
eval(['!mkdir ',QSCAT_dir])
%
% Find a subset of the QSCAT grid
%
[i1min,i1max,i2min,i2max,i3min,i3max,jrange,lon,lat]=...
get_QSCAT_grid(url,lonmin,lonmax,latmin,latmax);
%
% Get the time
%
time=readdap(url,'time',[]);
%
% Convert the time into "Yorig" time (i.e in days since Yorig/1/1 00:00:0.0)
%
time=time+datenum(1,1,1)-datenum(Yorig,1,1)-2; %-2 to match with CERSAT dates%
[year,month,days,hour,min,sec]=datevec(time+datenum(Yorig,1,1));
%
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
% Get the time indices for this month
%
    tndx=find(month==M & year==Y);
    taux=zeros(length(tndx),length(lat),length(lon));
    tauy=0*taux;
    n=0;
    for i=tndx(1):tndx(end)   
      disp(['    Processing day: ',num2str(n)])
      trange=['[',num2str(i-1),':',num2str(i-1),']'];
      x=getdap(url,[],'zwst',trange,[],jrange,...
                         i1min,i1max,i2min,i2max,i3min,i3max);
      y=getdap(url,[],'mwst',trange,[],jrange,...
                           i1min,i1max,i2min,i2max,i3min,i3max);
      x(x==-32767)=NaN;
      y(y==-32767)=NaN;
      if (isnan(max(max(x))) | isnan(max(max(y))))
        disp('Warning : nan value')
      elseif ((max(max(x))==0) | (max(max(y))==0))
        disp('Warning : 0 value')
      else
        n=n+1;
        good_time(n)=time(i);
        taux(n,:,:)=x;
        tauy(n,:,:)=y;
      end  
    end
    taux=taux(1:n,:,:);
    tauy=tauy(1:n,:,:);    
    write_NCEP([QSCAT_dir,'taux','Y',num2str(Y),'M',num2str(M),'.nc'],...
                'taux',lon,lat,good_time,taux,Yorig)
    write_NCEP([QSCAT_dir,'tauy','Y',num2str(Y),'M',num2str(M),'.nc'],...
                'tauy',lon,lat,good_time,tauy,Yorig)
  end
end
return
