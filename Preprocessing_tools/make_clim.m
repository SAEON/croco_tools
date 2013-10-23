%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Build a ROMS climatology file
%
%  Extrapole and interpole temperature and salinity from a
%  Climatology to get boundary and initial conditions for
%  ROMS (climatology and initial netcdf files) .
%  Get the velocities and sea surface elevation via a 
%  geostrophic computation.
%
%  Data input format (netcdf):
%     temperature(T, Z, Y, X)
%     T : time [Months]
%     Z : Depth [m]
%     Y : Latitude [degree north]
%     X : Longitude [degree east]
%
%  Data source : IRI/LDEO Climate Data Library (World Ocean Atlas 1998)
%    http://ingrid.ldgo.columbia.edu/
%    http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NODC/.WOA98/
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
%  Contributions of P. Marchesiello (IRD)
%
%  Updated    1-Sep-2006 by Pierrick Penven
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
%    temp_month_data : monthly temperature climatology
%    temp_ann_data   : annual temperature climatology
%    salt_month_data : monthly salinity climatology
%    salt_ann_data   : annual salinity climatology
%
temp_month_data=[climato_dir,'temp_month.cdf'];
temp_ann_data=[climato_dir,'temp_ann.cdf'];
salt_month_data=[climato_dir,'salt_month.cdf'];
salt_ann_data=[climato_dir,'salt_ann.cdf'];
%
%
%%%%%%%%%%%%%%%%%%% END USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%

%
% Title
%
disp(' ')
disp([' Making the clim: ',clmname])
disp(' ')
disp([' Title: ',ROMS_title])

%
% Read in the grid
%
disp(' ')
disp(' Read in the grid...')
nc=netcdf(grdname);
Lp=length(nc('xi_rho'));
Mp=length(nc('eta_rho'));
hmax=max(max(nc{'h'}(:)));
result=close(nc);



%----------------------------------------------------------------------------
% Create the climatology file
%----------------------------------------------------------------------------

if (makeclim)
  disp(' ')
  disp(' Create the climatology file...')
  if  ~exist('vtransform')
      vtransform=1; %Old Vtransform
      disp([' NO VTRANSFORM parameter found'])
      disp([' USE VTRANSFORM default value vtransform = 1'])
  end
  create_climfile(clmname,grdname,ROMS_title,...
                  theta_s,theta_b,hc,N,...
                  woa_time,woa_cycle,'clobber',vtransform);
end


if (makeoa)

  %
  % Create the OA file
  %
  disp(' ')
  disp(' Create the OA file...')
  nc=netcdf(temp_ann_data);
  Z=nc{'Z'}(:);
  kmax=max(find(Z<hmax))-1;
  Z=Z(1:kmax);
  close(nc)
  create_oafile(oaname,grdname,ROMS_title,Z,...
                woa_time,woa_cycle,'clobber');

  %
  % Horizontal extrapolations 
  %
  disp(' ')
  disp(' Horizontal extrapolations')
  disp(' ')
  disp(' Temperature...')
  ext_tracers(oaname,temp_month_data,temp_ann_data,...
              'temperature','temp','tclm_time','Z',Roa);
  disp(' ')
  disp(' Salinity...')
  ext_tracers(oaname,salt_month_data,salt_ann_data,...
              'salinity','salt','sclm_time','Z',Roa);
end


if (makeclim)

  %
  % Vertical interpolations 
  %
  disp(' ')
  disp(' Vertical interpolations')
  disp(' ')
  disp(' Temperature...')
  vinterp_clm(clmname,grdname,oaname,'temp','tclm_time','Z',0,'r');
  disp(' ')
  disp(' Salinity...')
  vinterp_clm(clmname,grdname,oaname,'salt','sclm_time','Z',0,'r');
  if (insitu2pot)
    disp(' ')
    disp(' Compute potential temperature from in-situ...')
    getpot(clmname,grdname)
  end

  %
  % Geostrophy
  %
  disp(' ')
  disp(' Compute geostrophic currents')
  geost_currents(clmname,grdname,oaname,frcname,zref,obc,0)

end



%----------------------------------------------------------------------------
% Initial file
%----------------------------------------------------------------------------
if (makeini)

  disp('======================== ')
  disp('Initial')
  create_inifile(ininame,grdname,ROMS_title,...
                 theta_s,theta_b,hc,N,...
                 tini,'clobber',vtransform);
  disp(' ')
  disp(' Temperature...')
  vinterp_clm(ininame,grdname,oaname,'temp','tclm_time','Z',tini,'r',1);
  disp(' ')
  disp(' Salinity...')
  vinterp_clm(ininame,grdname,oaname,'salt','sclm_time','Z',tini,'r',1);
  if (insitu2pot)
    disp(' ')
    disp(' Compute potential temperature from in-situ...')
    getpot(ininame,grdname)
  end

end



%----------------------------------------------------------------------------
% Make a few plots
%----------------------------------------------------------------------------
if makeplot==1
  disp(' ')
  disp(' Make a few plots...')
  test_clim(clmname,grdname,'temp',1,coastfileplot)
  figure
  test_clim(clmname,grdname,'salt',1,coastfileplot)
  figure
  test_clim(clmname,grdname,'temp',6,coastfileplot)
  figure
  test_clim(clmname,grdname,'salt',6,coastfileplot)
end
%
% End
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
