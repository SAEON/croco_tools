%===========================================
%
% romstools_param
%
% provide configuration parameters
% for building forcing and climatology files
% for ROMS-1D
%
% P. Marchesiello June 2012
%===========================================
%
% Station location
%
lon=220;
lat=30;
%
% Input data location
%
FORDIR = '/Users/pmarches/Roms_tools/COADS05/';
CLMDIR = '/Users/pmarches/Roms_tools/WOA2005/';
%
% Output files
%
OUTDIR='./';
frc_file = 'forces.data';
clm_file = 'clim.data';
%
% Grid parameters: same as in main.F
%
N       = 100;
theta_s = 2;
hmax    = 200;

