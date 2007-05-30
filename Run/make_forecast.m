%
%  make_forecast.m
%
%  Preparation of the files for the forecast system:
%     - launch make_OGCM_frcst and make_GFS
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
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
start
%
% Get the lateral boundary conditions
%
make_OGCM_frcst
%
% Get the surface forcing
%
make_GFS
%
% Copy the resulting files
%
eval(['!cp ',bry_prefix,num2str(rundate),nc_suffix,' ',bry_prefix,'0',nc_suffix])
eval(['!cp ',blk_prefix,num2str(rundate),nc_suffix,' ',blk_prefix,'0',nc_suffix])
eval(['!cp ',frc_prefix,num2str(rundate),nc_suffix,' ',frc_prefix,'0',nc_suffix])
%
return
