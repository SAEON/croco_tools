function var=get_hslice(fname,gname,vname,tindex,level,type);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% function var=get_hslice(fname,vname,tindex,level,type);
%
% get an horizontal slice of a ROMS variable
%
% input:
%
%  fname    ROMS netcdf file name (average or history) (string)
%  gname    ROMS netcdf grid file name  (string)
%  vname    name of the variable (string)
%  tindex   time index (integer)
%  level    vertical level of the slice (scalar):
%             level =   integer >= 1 and <= N
%                       take a slice along a s level (N=top))
%             level =   0
%                       2D horizontal variable (like zeta)
%             level =   real < 0
%                       interpole a horizontal slice at z=level
%  type    type of the variable (character):
%             r for 'rho' for zeta, temp, salt, w(!)
%             w for 'w'   for AKt
%             u for 'u'   for u, ubar
%             v for 'v'   for v, vbar
%
% output:
%
%  var     horizontal slice (2D matrix)
%
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
%  Copyright (c) 2002-2006 by Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr  
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
nc=netcdf(fname);
if level==0
%
% 2D variable
%
  var=squeeze(nc{vname}(tindex,:,:));
elseif level>0
%
% Get a sigma level of a 3D variable
%
  var=squeeze(nc{vname}(tindex,level,:,:));
else
%
% Get a horizontal level of a 3D variable
%
% Get the depths of the sigma levels
%
  z=get_depths(fname,gname,tindex,type);
%
% Read the 3d matrix and do the interpolation
%
  var_sigma=squeeze(nc{vname}(tindex,:,:,:));
  var = vinterp(var_sigma,z,level);
end
close(nc);
return
