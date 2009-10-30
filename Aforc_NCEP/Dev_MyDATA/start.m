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
mypathsvn='/home/gcambon/SVN_3/romsagrif/Roms_tools/'
mypath2='/home/gcambon/ROMS/';
%
% ROMSTOOLS directories
%
addpath([mypathsvn,'/Aforc_NCEP/Dev_MyDATA/'])
addpath([mypathsvn,'Aforc_QuikSCAT'])
addpath([mypathsvn,'Diagnostic_tools'])
addpath([mypathsvn,'Forecast_tools'])
addpath([mypathsvn,'Nesting_tools'])
addpath([mypathsvn,'Preprocessing_tools'])
addpath([mypathsvn,'Oforc_OGCM'])
addpath([mypathsvn,'Opendap_tools'])
addpath([mypathsvn,'Tides'])
addpath([mypathsvn,'Visualization_tools'])
addpath([mypathsvn,'Run/TEST_CASES'])
%
% Other software directories
%
addpath([mypathsvn,'air_sea'])
addpath([mypathsvn,'mask'])
addpath([mypath2,'m_map'])


%Get from internet, Compile for 64 bytes, change in ncdim.m
addpath([mypath2,'mexncGIL/mexcdf/netcdf_toolbox/netcdf'])
addpath([mypath2,'mexncGIL/mexcdf/netcdf_toolbox/netcdf/ncsource'])
addpath([mypath2,'mexncGIL/mexcdf/netcdf_toolbox/netcdf/nctype'])
addpath([mypath2,'mexncGIL/mexcdf/netcdf_toolbox/netcdf/ncutility'])

%addpath([mypath,'netcdf_matlab'])
%addpath([mypath,'netcdf_matlab/ncfiles'])
%addpath([mypath,'netcdf_matlab/nctype'])
%addpath([mypath,'netcdf_matlab/ncutility'])



%
% Get the path to the mexcdf (it depends on the architecture)
%
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
% $$$     addpath([mypath,'mexnc']) % 64bits version of mexnc 
% $$$     addpath([mypath,'Opendap_tools/DEBIAN']) % tested on matlab6 / fedora4
% $$$   
% $$$ end


%--------------------------------------------------------------------------------------
%addpath([mypath,'mexnc_64'])                   %car les biblio netcdf matlab ne sont pas
                                                %bonne sur pc_kuria (32 bits
                                                %au lieu de 64)
%addpath(['/home/gcambon/Desktop/Roms_tools/mexnc'])  
addpath([mypath2,'mexncGIL/mexcdf/mexnc'])
%addpath([mypath,'mexnc_64'])   

					     
%---------------------------------------------------------------------------------------

%addpath([mypath,'Opendap_tools/DEBIAN'])                
%addpath([mypath,'Opendap_tools/CENTOS5.2_X64'])             
addpath(['/opt/loaddap-3.5.2/share/loaddap/'])                                   
addpath(['/opt/loaddap-3.5.2/bin/'])
addpath(['/opt/libdap-3.6.2/bin/'])    
%--------------------------------------------------------------------------------------					 
addpath([mypath2,'Gil_tools/'])   
addpath([mypath2,'Gil_tools/Forcing']) 
addpath([mypath2,'Gil_tools/My_Anim'])
addpath([mypath2,'Gil_tools/Pisces'])
addpath([mypath2,'Gil_tools/Sigma2Z'])
addpath([mypath2,'Gil_tools/editbathymetry'])
addpath([mypath2,'Gil_tools/CompareMPIOMP'])
addpath([mypath2,'Gil_tools/Correlation'])
addpath([mypath2,'Gil_tools/CompareRun'])
addpath([mypath2,'Gil_tools/ROUTINES'])
addpath([mypath2,'Gil_tools/MHR/'])
addpath([mypath2,'Gil_tools/MyNCEP/'])