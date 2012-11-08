clear all
close all
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Add biological parameters to a ROMS climatology file
%
%  Data source : IRI/LDEO Climate Data Library (World Ocean Atlas 1998)
%    http://ingrid.ldgo.columbia.edu/
%    http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NODC/.WOA2001/
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
%  Copyright (c) 2003-2006 by Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr  
%
%  Contributions of P. Marchesiello (IRD)
%
%  Updated    1-Sep-2006 by Pierrick Penven
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all
close all
%%%%%%%%%%%%%%%%%%%%% USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%%
%
% Common parameters
%
romstools_param
%
%  Data climatologies file names:
%
%    no3_seas_data : seasonal NO3 climatology
%    no3_ann_data  : annual NO3 climatology
%    chla_seas_data : seasonal Chlorophylle climatology
%
no3_seas_data=[woa_dir,'no3_seas.cdf'];
no3_ann_data=[woa_dir,'no3_ann.cdf'];
chla_seas_data=[chla_dir,'chla_seas.cdf'];
NO3min=0.01;
%
%
%%%%%%%%%%%%%%%%%%% END USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%
%
% Add variables in the files
%
add_no3(oaname,clmname,ininame,grdname,no3_seas_data,...
        no3_ann_data,woa_cycle,makeoa,makeclim)
%
% Horizontal extrapolation of NO3
%
if (makeoa)
  ext_tracers(oaname,no3_seas_data,no3_ann_data,...
              'nitrate','NO3','no3_time','Zno3',Roa);
end
%
% Vertical interpolations 
%
if (makeclim)
  disp(' ')
  disp(' Vertical interpolations')
  disp(' ')
  disp(' NO3...')
  vinterp_clm(clmname,grdname,oaname,'NO3','no3_time','Zno3',0,'r');
%
% Remove low values for oligotrophic areas
%
  nc=netcdf(clmname,'write');
  tlen=length(nc('no3_time'));
  for l=1:tlen
    NO3=nc{'NO3'}(l,:,:,:);
    NO3(NO3<NO3min)=NO3min;
    nc{'NO3'}(l,:,:,:)=NO3;
  end
  close(nc)
%
%  CHla
%		 
  disp(' ')
  disp(' CHla...')
  add_chla(clmname,grdname,chla_seas_data,woa_cycle,Roa);
%
%  Phyto
%		 
  disp(' ')
  disp(' Phyto...')
  add_phyto(clmname);
%
%  Zoo
%
  disp(' ')
  disp(' Zoo...')
  add_zoo(clmname);
end
%
% Initial file
%
if (makeini)
  disp(' ')
  disp(' Initial')
  disp(' ')
  disp(' NO3...')
  add_ini_no3(ininame,grdname,oaname,woa_cycle,NO3min);
%
%  CHla
%		 
  disp(' ')
  disp(' CHla...')
  add_ini_chla(ininame,grdname,chla_seas_data,woa_cycle,Roa);
%
%  Phyto
%		 
  disp(' ')
  disp(' Phyto...')
  add_ini_phyto(ininame);
%
%  Zoo
%
  disp(' ')
  disp(' Zoo...')
  add_ini_zoo(ininame);
end
%
% Make a few plots
%
disp(' ')
disp(' Make a few plots...')
test_clim(clmname,grdname,'NO3',1,coastfileplot)
figure
test_clim(clmname,grdname,'CHLA',1,coastfileplot)
figure
test_clim(clmname,grdname,'PHYTO',1,coastfileplot)
%
% End
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%











