function [i1min,i1max,i2min,i2max,i3min,i3max,jrange,krange,lon,lat,depth]=...
    get_SODA2_subgrid(path,lonmin,lonmax,latmin,latmax)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Get the indices for a SODA subgrid 
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
%  Copyright (c) 2011 by Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr  
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%clear all
%close all
if nargin < 1
  path='http://soda.tamu.edu/opendap/SODA_2.1.6/MONTHLY/SODA_2.1.6_195801.cdf';
  lonmin=16;
  lonmax=19;
  latmin=-34;
  latmax=-32;
end
%
dl=1;
lonmin=lonmin-dl;
lonmax=lonmax+dl;
latmin=latmin-dl;
latmax=latmax+dl;
%
% Get the global grid
%
% ! Warning hardcoded for http://soda.tamu.edu/opendap/SODA_2.1.6 !
%
x=readattribute(path);
L=x.temp.lon.DODS_ML_Size;
M=x.temp.lat.DODS_ML_Size;
N=x.temp.depth.DODS_ML_Size;
argdods='-e -v';
var=loaddap(argdods,[path,'?temp[0:0][0:0][0:',num2str(L-1),']']);
lon=var.temp.lon;
var=loaddap(argdods,[path,'?temp[0:0][0:',num2str(M-1),'][0:0]']);
lat=var.temp.lat;
var=loaddap(argdods,[path,'?temp[0:',num2str(N-1),'][0:0][0:0]']);
depth=var.temp.depth;
%
% Get a subgrid
%
%
% 1 Longitude: take care of greenwitch
%
i1=find(lon-360>=lonmin & lon-360<=lonmax);
i2=find(lon>=lonmin & lon<=lonmax);
i3=find(lon+360>=lonmin & lon+360<=lonmax);
%
lon=cat(1,lon(i1)-360,lon(i2),lon(i3)+360);
%
if ~isempty(i1)
  i1min=min(i1)-1;
  i1max=max(i1)-1;
else
  i1min=[];
  i1max=[];
end
if ~isempty(i2)
  i2min=min(i2)-1;
  i2max=max(i2)-1;
else
  i2min=[];
  i2max=[];
end
if ~isempty(i3)
  i3min=min(i3)-1;
  i3max=max(i3)-1;
else
  i3min=[];
  i3max=[];
end
%
% 2 Latitude
%
j=find(lat>=latmin & lat<=latmax);
lat=lat(j);
jmin=min(j)-1;
jmax=max(j)-1;
jrange=['[',num2str(jmin),':',num2str(jmax),']'];
%
% 3 Depth
%
k=length(depth);
krange=['[',num2str(0),':',num2str(k-1),']'];
%
return
