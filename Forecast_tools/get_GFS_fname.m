function fname=get_GFS_fname(time,gfs_run_time,gfstype)
%
%  fname=get_GFS_fname(time,gfs_run_time)
%
%  Give the GFS url for a given date.
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
%  Updated    20-Aug-2008 by Matthieu Caillaud & P. Marchesiello
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%----------------------------------------------
%
url='http://nomads.ncep.noaa.gov:9090';
%url='http://nomad5.ncep.noaa.gov:9090';
%url='http://nomad1.ncep.noaa.gov:9090';
%
if gfstype==0
  %=== nomads ===
  gfsname='fnl/fnl';
  gfsname1='fnlflx';
  %=== nomad1-5 ===
  %gfsname='gdas/rotating/gdas';
  %gfsname1='gdas';
else
  %=== nomads ===
  gfsname='gfs_hd/gfs_hd';
  gfsname1='gfs_hd';
 %=== nomad1 ===
  %gfsname='gfs_master/gfs';
  %gfsname1='gfs_master';
 %=== nomad5 ===
 % gfsname='gfs/gfs';
 % gfsname1='gfs';
end
%
%----------------------------------------------
%
% Get the date
%
[y,m,d,h,mi,s]=datevec(time);
stry=num2str(y);
if m<10
  strm=['0',num2str(m)];
else
  strm=num2str(m);
end
if d<10
  strd=['0',num2str(d)];
else
  strd=num2str(d);
end
%
gfsdir=[url,'/dods/',gfsname,stry,strm,strd,'/'];
gdasdir=[url,'/dods/',gfsname,stry,strm,strd];
%
% Get the grid
%
if gfs_run_time < 10
  if gfstype==0
    %fname=[gdasdir,'0',num2str(gfs_run_time)];
    fname=[gfsdir,gfsname1,'_0',num2str(gfs_run_time),'z'];
  else
    fname=[gfsdir,gfsname1,'_0',num2str(gfs_run_time),'z'];
  end
else
  if gfstype==0
    %fname=[gdasdir,num2str(gfs_run_time)];
    fname=[gfsdir,gfsname1,'_',num2str(gfs_run_time),'z'];
  else
    fname=[gfsdir,gfsname1,'_',num2str(gfs_run_time),'z'];
  end
end
