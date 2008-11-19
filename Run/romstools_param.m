%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% romstools_param: common parameter file for the preprocessing
%                  of ROMS simulations using ROMSTOOLS
%
%                  This file is used by make_grid.m, make_forcing.m, 
%                  make_clim.m, make_biol.m, make_bry.m, make_tides.m,
%                  make_NCEP.m, make_OGCM.m, make_...
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
%  Updated    6-Sep-2006 by Pierrick Penven
%  Updated    2006/10/05 by Pierrick Penven (add tidegauge observations)
%  Updated    24-Oct-2006 by Pierrick Penven (diagnostics, chla etc...)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 1 General parameters
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  ROMS title names and directories
%
ROMS_title  = 'Benguela Test Model';
ROMS_config = 'Benguela';
DATADIR = '../'
ROMSTOOLS_dir = '../';
RUN_dir=[ROMSTOOLS_dir,'Run/'];
ROMS_files_dir=[RUN_dir,'ROMS_FILES/'];
%
% ROMS file names (grid, forcing, bulk, climatology, initial)
%
grdname=[ROMS_files_dir,'roms_grd.nc'];
frcname=[ROMS_files_dir,'roms_frc.nc'];
blkname=[ROMS_files_dir,'roms_blk.nc'];
clmname=[ROMS_files_dir,'roms_clm.nc'];
ininame=[ROMS_files_dir,'roms_ini.nc'];
oaname =[ROMS_files_dir,'roms_oa.nc'];    % oa file  : intermediate file not used
                                          %            in roms simulations
bryname=[ROMS_files_dir,'roms_bry.nc'];
Zbryname=[ROMS_files_dir,'roms_bry_Z.nc'];% Zbry file: intermediate file not used
                                          %            in roms simulations
%
frc_prefix=[ROMS_files_dir,'roms_frc'];   % generic forcing file name 
                                          % for interannual roms simulations (NCEP or GFS)
blk_prefix=[ROMS_files_dir,'roms_blk'];   % generic bulk file name
                                          % for interannual roms simulations (NCEP or GFS)
%
% Objective analysis decorrelation scale [m]
% (if Roa=0: simple extrapolation method; crude but much less costly)
%
%Roa=300e3;
Roa=0;
%
interp_method = 'cubic';           % Interpolation method: 'linear' or 'cubic'
%
makeplot     = 1;                 % 1: create a few graphics after each preprocessing step
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 2 Grid parameters
%   used by make_grid.m (and others..)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Grid dimensions:
%
lonmin =     8;   % Minimum longitude [degree east]
lonmax =    22;   % Maximum longitude [degree east]
latmin =   -38;   % Minimum latitude  [degree north]
latmax =   -26;   % Maximum latitude  [degree north]
%
% Grid resolution [degree]
%
dl = 1/6;
%
% Number of vertical Levels (! should be the same in param.h !)
%
N = 32;
%
%  Vertical grid parameters (! should be the same in roms.in !)
%
theta_s = 6.;
theta_b = 0.;
hc      =10.;
%
% Minimum depth at the shore [m] (depends on the resolution,
% rule of thumb: dl=1, hmin=300, dl=1/4, hmin=150, ...)
% This affect the filtering since it works on grad(h)/h.
%
hmin = 75;
%
% Maximum depth at the shore [m] (to prevent the generation
% of too big walls along the coast)
%
hmax_coast = 500;
%
%  Topography netcdf file name (ETOPO 2 or any other netcdf file
%  in the same format)
%
topofile = [DATADIR,'Topo/etopo2.nc'];
%
% Slope parameter (r=grad(h)/h) maximum value for topography smoothing
%
rtarget = 0.25;
%
% Number of pass of a selective filter to reduce the isolated
% seamounts on the deep ocean.
%
n_filter_deep_topo=4;
%
% Number of pass of a single hanning filter at the end of the
% smooting procedure to ensure that there is no 2DX noise in the 
% topography.
%
n_filter_final=2;
%
%  GSHSS user defined coastline (see m_map) 
%  XXX_f.mat    Full resolution data
%  XXX_h.mat    High resolution data
%  XXX_i.mat    Intermediate resolution data
%  XXX_l.mat    Low resolution data
%  XXX_c.mat    Crude resolution data
%
coastfileplot = 'coastline_l.mat';
coastfilemask = 'coastline_l_mask.mat';
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 3 Surface forcing parameters
%   used by make_forcing.m and by make_bulk.m
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% COADS directory (for climatology runs)
%
coads_dir=[DATADIR,'COADS05/'];
%
% COADS time (for climatology runs)
%
coads_time=(15:30:345); % days: middle of each month
coads_cycle=360;        % repetition of a typical year of 360 days  
%
%coads_time=(15.2188:30.4375:350.0313); % year of 365.25 days in the case
%coads_cycle=365.25;                    % of QSCAT experiments with 
%                                         climatological heat flux.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 3.1 Surface forcing parameters
%   used by pathfinder_sst.m
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
pathfinder_sst_name=[DATADIR,...
                    'SST_pathfinder/climato_pathfinder.nc'];
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 4 Open boundaries and initial conditions parameters
%   used by make_clim.m, make_biol.m, make_bry.m
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Open boundaries switches (! should be consistent with cppdefs.h !)
%
obc = [1 1 1 1]; % open boundaries (1=open , [S E N W])
%
%  Level of reference for geostrophy calculation
%
zref = -1000;
%
%  Switches for selecting what to process in make_clim (1=ON)
%  (and also in make_OGCM.m and make_OGCM_frcst.m)
makeini=1;      %1: process initial data
makeclim=1;     %1: process lateral boundary data
makebry=1;      %1: process boundary data
%
makeoa=1;       %1: process oa data (intermediate file)
insitu2pot=1;   %1: transform in-situ temperature to potential temperature
makeZbry=1;     %1: process data in Z coordinate
%
%  Day of initialisation for climatology experiments (=0 : 1st january 0h)
%
tini=0;  
%
% World Ocean Atlas directory (WOA2001 or WOA2005) 
%
woa_dir=[DATADIR,'WOA2005/'];
%
% Surface chlorophyll seasonal climatology (WOA2001 or SeaWifs)
%
chla_dir=[DATADIR,'SeaWifs/'];
%
%  Set times and cycles for the boundary conditions: 
%   monthly climatology 
%
woa_time=(15:30:345); % days: middle of each month
woa_cycle=360;        % repetition of a typical year of 360 days  
%
%woa_time=(15.2188:30.4375:350.0313); % year of 365.25 days in the case
%woa_cycle=365.25;                    % of QSCAT experiments with 
%                                 climatological boundary conditions
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 5 Parameters for tidal forcing
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% TPXO file name (TPXO6 or TPXO7)
%
tidename=[DATADIR,'TPXO6/TPXO6.nc'];
%
% Number of tides component to process
%
Ntides=10;
%
% Chose order from the rank in the TPXO file :
% "M2 S2 N2 K2 K1 O1 P1 Q1 Mf Mm"
% " 1  2  3  4  5  6  7  8  9 10"
%
tidalrank=[1 2 3 4 5 6 7 8 9 10];
%
% Compare with tidegauge observations
%
%lon0=-4.60;
%lat0=48.42;    % Brest location
%Z0=4;          % Mean depth of the tidegauge in Brest
lon0=18.37;
lat0=-33.91;   % Cape Town location
Z0=1;          % Mean depth of the tidegauge in Cape Town
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 6 Temporal parameters (used for make_tides, make_NCEP, make_OGCM)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
Yorig         = 1900;               % reference time for vector time
                                    % in roms initial and forcing files
%
Ymin          = 2000;               % first forcing year
Ymax          = 2000;               % last  forcing year
Mmin          = 1;                  % first forcing month
Mmax          = 3;                  % last  forcing month
%
Dmin          = 1;                  % Day of initialization
Hmin          = 0;                  % Hour of initialization
Min_min       = 0;                  % Minute of initialization
Smin          = 0;                  % Second of initialization
%
SPIN_Long     = 0;                  % SPIN-UP duration in Years
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 7 Parameters for Interannual forcing (SODA, ECCO, NCEP, ...)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Path to Forcing data
%
FORC_DATA_DIR = [RUN_dir,'DATA/'];
%
Download_data = 1;                            % Get data from the OPENDAP sites 
level         = 0;                            % AGRIF level; 0=parent grid 
%
%  Options for make_NCEP
%
NCEP_dir= [FORC_DATA_DIR,'NCEP_',ROMS_config,'/']; % NCEP data directory
makefrc      = 1;                            % 1: Create forcing files
makeblk      = 1;                            % 1: Create bulk files
QSCAT_blk    = 1;                            % 1: -a) Correct NCEP
                                             %     frc/bulk file with the
                                             %     u,v,wspd fields from
                                             %     QSCAT daily data 
					     %    -b) Download u, v, wspd
                                             %         in the QSCAT frc file
add_tides     = 0;                     % 1: Add the tides (To be done...)
%
NCEP_version  = 1;                            % NCEP version:
%                                 (1: NCEP/NCAR Reanalysis, 1/1/1948 - present
%                                  2: NCEP-DOE Reanalysis, 1/1/1979 - 12/31/2001)
%
%  Options for make_OGCM 
%
OGCM        = 'SODA';                                % Select the OGCM: SODA, ECCO, ...
OGCM_dir    = [FORC_DATA_DIR,OGCM,'_',ROMS_config,'/'];   % OGCM data directory
bry_prefix  = [ROMS_files_dir,'roms_bry_',OGCM,'_']; % generic boundary file name
clm_prefix  = [ROMS_files_dir,'roms_clm_',OGCM,'_']; % generic climatology file name
ini_prefix  = [ROMS_files_dir,'roms_ini_',OGCM,'_']; % generic initial file name
OGCM_prefix = [OGCM,'_'];                            % generic OGCM file name 
rmdepth     = 2;                                     % Number of bottom levels to remove 
%                             (This is usefull when there is no valid data at this level
%                        i.e if the depth in the domain is shallower than the OGCM depth)
%
%  Options for make_QSCAT_daily and make_QSCAT_clim   
%
QSCAT_dir        = [FORC_DATA_DIR,'QSCAT_',ROMS_config,'/']; % QSCAT data directory.
QSCAT_frc_prefix = [frc_prefix,'_QSCAT_']; % generic forcing file name
                                           % for interannual roms simulations with QuickCAT.
QSCAT_clim_file  = [DATADIR,'QuikSCAT_clim/',...   % QuikSCAT climatology
                    'QSCAT_global_clim_jul99_aug06.nc']; % file for make_QSCAT_clim.
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 8 Parameters for the forecast system
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
FRCST_dir = [FORC_DATA_DIR,'Forecast/'];       % path to store local OGCM data
FRCST_prefix  = [OGCM,'_'];                    % generic OGCM file name 

if strcmp(OGCM,'ECCO')                         % nb of hindcast days
  hdays=1;
elseif strcmp(OGCM,'mercator')
  hdays=5
end
timezone = +11                                 % Local time= UTC + timezone
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% 9 Parameters for the diagnostic tools
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
DIAG_dir = [ROMSTOOLS_dir,'Diagnostic_tools/'];
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%











