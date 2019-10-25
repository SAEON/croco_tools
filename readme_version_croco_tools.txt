CROCO_TOOLS v1.1
---------------
Released date : 25 October 2019

Previous release : CROCO_TOOLS v1.0 (June 2018)

Reminders:
==========
CROCO sources and CROCO_TOOLS (the follow-on of ROMS_TOOLS) are now distributed separately (for croco_tools releases, see associated tab at  https://www.croco-ocean.org/download/croco-project/ ). ROMS_AGRIF  is not maintained anymore and we strongly encourage ROMS_AGRIF users to switch  to CROCO. CROCO version available directly from the git repository is a unstable development  version. Standard users should use the stable one downloaded from the web site.

New in v1.1 :
=============

- Aforc_CFSR : new specific directory dedicated to CFSR atmospheric forcing processing

- croco_pyvisu : a new powerful visualization gui (as croco_gui) in Python (Thanks to S. Le Gentil, LOPS/Ifremer), portable on every supercomputer.

- Preprocessing_tools: 
	- Insert Easy interactive grid maker (adapted from J. Molemaker (UCLA) package) within make_grid 
	       and the croco_tools package (allows grid rotation and parameter adjustment)
	
	- Add new filter used in smoothgrid.m (adapted by P. Penven from A. Shchepetkin) with option set in crocotools_param.h

- Rivers : several update to manage variable flows and/or concentration (temperature, salinity, bigeochemical tracers, ...) from input netCDF files.


CONTENTS:
--------
- start.m: script to set the Matlab paths

- crocotools_param.m: script that contains the necessary parameters for the generation of the CROCO input NetCDF files

- Aforc_ECMWF/: Scripts for the recovery of surface forcing data (based on ECMWF reanalysis) for inter-annual simulations

- Aforc_CFSR/: Scripts for the recovery of surface forcing data (based on CFSR reanalysis) for inter-annual simulations

- Aforc_NCEP/: Scripts for the recovery of surface forcing data (based on NCEP reanalysis) for inter-annual simulations

- Aforc_QuikSCAT/ : Scripts for the recovery of wind stress from satellite scatterometer data (QuickSCAT)

- Coupling_tools/: Scripts for building and running coupled configurations (with atmosphere and/or wave models)

- croco_pytools/ : Preprocessing Python scripts for preparing the grid, forcing, initialization files

- croco_pyvisu/ : A new portable visualization gui (as croco_gui) but in Python

- Diagnostic_tools/ : A few Matlab scripts for animations and basic statistical analysis

- Forecast_tools/ : Scripts for the generation of an operational oceanic forecast system

- Nesting_tools/ : Preprocessing tools used to prepare nested models

- Oforc_OGCM/ : Scripts for the recovery of initial and lateral boundary conditions from global OGCMs (SODA (Carton et al., 2005) or ECCO (Stammer et al., 1999)) for inter-annual simulations

- Opendap_tools/ : LoadDAP mexcdf and several scripts to automatically download data over the Internet

- Opendap_tools_no_loaddap/ : same as Opendap_tools but use the built-in support for OPeNDAP from Matlab >= 2012a. You do not need to install libdap and loaddap library. Note that you can also access to local data.

- Preprocessing_tools/ : Preprocessing Matlab scripts (make_grid.m, make_forcing, etc…)

- Rivers/ : Scripts to prepare time-varying runoff forcing file and compute the runoff location

- RUNOFF_DAI/ : runoff global climatology

- Tides/ : Matlab routines to prepare CROCO tidal simulations. Tidal data are derived from the Oregon State University global models of ocean tides TPXO6 and TPXO7 (Egbert and Erofeeva, 2002): http://www.oce.orst.edu/research/po/research/tide/global.html

- Town/:

- UTILITIES/: Matlab utilities

- Visualization_tools/ : Matlab scripts for the CROCO visualization graphic user interface



 _   _                      __                 _
| | | | __ ___   _____     / _|_   _ _ __     | |
| |_| |/ _` \ \ / / _ \   | |_| | | | '_ \    | |
|  _  | (_| |\ V /  __/   |  _| |_| | | | |   |_|
|_| |_|\__,_| \_/ \___|   |_|  \__,_|_| |_|   (_)


