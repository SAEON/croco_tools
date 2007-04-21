%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Build a ROMS initial file from Levitus Data
%
%  Extrapole and interpole temperature and salinity from a
%  Climatology to get initial conditions for
%  ROMS (initial netcdf files) .
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
%  P. Marchesiello & P. Penven - IRD 2005
%
%  Version of 21-Sep-2005
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all
close all
%%%%%%%%%%%%%%%%%%%%% USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%%
%
%  Title 
%
title='Climatology';
%
%  Switches for selecting what to process (1=ON)
%
makeini=1;  %1: process initial data
%
%  Grid file name - Climatology file name
%  Initial file name - OA file name
%
grdname='roms_grd.nc';
frcname='roms_frc.nc';
ininame='roms_ini.nc';
oaname ='roms_oa.nc';
%
%  Vertical grid parameters
%  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%  !!! WARNING WARNING WARNING WARNING WARNING  !!!
%  !!!   THESE MUST IDENTICAL IN THE .IN FILE   !!!
%  !!! WARNING WARNING WARNING WARNING WARNING  !!!
%  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%
theta_s=6.;
theta_b=0.;
hc=5.;
N=30; % number of vertical levels (rho)
%
%  Open boundaries switches
%  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%  !!! WARNING WARNING WARNING WARNING WARNING  !!!
%  !!!   MUST BE CONFORM TO THE CPP SWITCHES    !!!
%  !!! WARNING WARNING WARNING WARNING WARNING  !!!
%  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
%
obc=[1 1 1 1]; % open boundaries (1=open , [S E N W])
%
%  Level of reference for geostrophy calculation
%
zref=-500; 
%
%  Day of initialisation
%
tini=0;  
%
% Set times and cycles: monthly climatology for all data
%
time=[15:30:345];    % time 
cycle=360;           % cycle 
%
%  Data climatologies file names:
%
%    temp_month_data : monthly temperature climatology
%    temp_ann_data   : annual temperature climatology
%    salt_month_data : monthly salinity climatology
%    salt_ann_data   : annual salinity climatology
%
temp_month_data='../WOA2001/temp_month_corrected.cdf';
temp_ann_data='../WOA2001/temp_ann_corrected.cdf';
insitu2pot=1;   %1: transform in-situ temperature to potential temperature
salt_month_data='../WOA2001/salt_month_corrected.cdf';
salt_ann_data='../WOA2001/salt_ann_corrected.cdf';
%
%
%%%%%%%%%%%%%%%%%%% END USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%
%
% Title
%
disp(' ')
disp([' Making initial file: ',ininame])
disp(' ')
disp([' Title: ',title])
%
% Initial file
%
create_inifile(ininame,grdname,title,...
               theta_s,theta_b,hc,N,...
               tini,'clobber');
%
% Horizontal and vertical interp/extrapolations 
%
disp(' ')
disp(' Interpolations / extrapolations')
disp(' ')
disp(' Temperature...')
ext_tracers_ini(ininame,grdname,temp_month_data,temp_ann_data,...
            'temperature','temp','r',tini);
disp(' ')
disp(' Salinity...')
ext_tracers_ini(ininame,grdname,salt_month_data,salt_ann_data,...
             'salinity','salt','r',tini);
%
% Geostrophy
%
%  disp(' ')
%  disp(' Compute geostrophic currents')
%  geost_currents(ininame,grdname,oaname,frcname,zref,obc,0)
%
% Initial file
%
if (insitu2pot)
  disp(' ')
  disp(' Compute potential temperature from in-situ...')
  getpot(ininame,grdname)
end
%
% Make a few plots
%
disp(' ')
disp(' Make a few plots...')
test_clim(ininame,grdname,'temp',1)
figure
test_clim(ininame,grdname,'salt',1)
%
% End
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
