%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Add the paths of the different toolboxes
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
%  Copyright (c) 2005-2006 by Patrick Marchesiello and Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr  
%
%  Updated    10-Sep-2006 by Pierrick Penven
%  Updated    22-Sep-2006 by Pierrick Penven (64 bits test)
%  Updated    24-Oct-2006 by Pierrick Penven (mask added)
%  Updated    16-jan-2007 by Pierrick Penven (quikscat added)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
disp(['Add the paths of the different toolboxes'])
mypath='../';
%
% ROMSTOOLS directories
%
addpath([mypath,'Aforc_NCEP'])
addpath([mypath,'Aforc_QuikSCAT'])
addpath([mypath,'Diagnostic_tools'])
addpath([mypath,'Forecast_tools'])
addpath([mypath,'Nesting_tools'])
addpath([mypath,'Preprocessing_tools'])
addpath([mypath,'Oforc_OGCM'])
addpath([mypath,'Opendap_tools'])
addpath([mypath,'Tides'])
addpath([mypath,'Visualization_tools'])
addpath([mypath,'Run/TEST_CASES'])
%
% Other software directories
%
addpath([mypath,'air_sea'])
addpath([mypath,'mask'])
%
%-------------------------------------------------------
%
% Comment/Add this lines if these directories 
% are already in tour matlab path
addpath([mypath,'m_map'])
addpath([mypath,'netcdf_matlab'])
addpath([mypath,'netcdf_matlab/ncfiles'])
addpath([mypath,'netcdf_matlab/nctype'])
addpath([mypath,'netcdf_matlab/ncutility'])
%
%-------------------------------------------------------
%
% Get the path to the mexcdf (it depends on the architecture)
% Comment/Add all these lines if you don't want to pass in these tests
% $$$ !uname -p > .mysystem
% $$$ fid=fopen('.mysystem');
% $$$ mysystem=fscanf(fid,'%s');
% $$$ fclose(fid); 
% $$$ matversion=version('-release');
% $$$ myversion=str2num(matversion(1:2));
% $$$ !rm -f .mysystem
% $$$ disp(['Arch : ',mysystem,' - Matlab version : ',matversion])
% $$$ if (strcmp(mysystem(end-1:end),'64') & (myversion > 13))
% $$$   disp('Use of mexnc and loaddap in 64 bits.')
% $$$   addpath([mypath,'mexnc']) % 64bits version of mexnc 
% $$$   addpath([mypath,'Opendap_tools/FEDORA_X64']) % 64bits version of loaddap
% $$$ elseif (strcmp(mysystem(end-1:end),'86') | (myversion <= 13))
% $$$   disp('Use of mex60 and loaddap in 32 bits.')
% $$$   addpath([mypath,'mex60']) % older version of mexcdf
% $$$   addpath([mypath,'Opendap_tools/FEDORA']) % tested on matlab6 / fedora4
% $$$ else
% $$$   disp(['Arch : ',mysystem,...
% $$$        ' you should provide the paths of your own loaddap and mexcdf directories'])
% $$$ end
%
%-------------------------------------------------------
%
% Pierrick: a corriger pour que ca marche sur differents systemes
%
% Comment/Add these lines if you wants to 
% use :
% 1 - your own compiled mexnc libraries 
%  addpath([mypath,'mexnc'])
% 2 - your own compiled libdap/loaddap libraries 
%  addpath([mypath,'Opendap_tools/DEBIAN_X64'])
addpath([mypath,'mex60'])
addpath([mypath,'Opendap_tools/FEDORA'])
