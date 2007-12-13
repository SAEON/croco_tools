function fname=get_GFS_fname(time,gfs_run_time)
%
%  fname=get_GFS_fname(time,gfs_run_time)
%
%  Gifve the GFS url for a given date.
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
gfsname='gfs_master';
%url='http://nomads6.ncdc.noaa.gov:9090';
url='http://nomad3.ncep.noaa.gov:9090';
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
gfsdir=[url,'/dods/',gfsname,'/gfs',stry,strm,strd,'/'];
%
% Get the grid
%
if gfs_run_time < 10
  fname=[gfsdir,gfsname,'_0',num2str(gfs_run_time),'z'];
else
  fname=[gfsdir,gfsname,'_',num2str(gfs_run_time),'z'];
end
