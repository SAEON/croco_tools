CROCO_TOOLS v1.0
-----------------
Released date : 26 June 2018


New in v1.0 :
=============

– former OASIS directory has been moved to Junk/oasis_old_scripts as new tools are available in Coupling_tools directory
– Coupling_tools scripts: scripts to help you build and run a coupled configuration
– croco_pytools: Python scripts, some are equivalent of croco Preprocessing_tools
– new default s vertical coordinate (vtransform=2, #define NEW_S_COORD) and new values of theta_s, theta_b and hc accordingly
– new topographic filter adapted from ucla tools
– all matlab utilities moved to new UTILITIES directory


CONTENTS:
--------
- start.m: script to set the Matlab paths 
- crocotools_param.m: script that contains the necessary parameters for the generation of the CROCO input NetCDF files 

- Aforc_ECMWF/: Scripts for the recovery of surface forcing data (based on ECMWF reanalysis) for inter-annual simulations
- Aforc_NCEP/: Scripts for the recovery of surface forcing data (based on CFSR reanalysis) for inter-annual simulations
- Aforc_QuikSCAT/ : Scripts for the recovery of wind stress from satellite scatterometer data (QuickSCAT)
- Coupling_tools/: Scripts for building and running coupled configurations (with atmosphere and/or wave models)
- croco_pytools/: Preprocessing Python scripts for preparing the grid, forcing, initialization files
- Diagnostic_tools/ : A few Matlab scripts for animations and basic statistical analysis
- Forecast_tools/ : Scripts for the generation of an operational oceanic forecast system
- Junk/ : old scripts, kept as archive (e.g. former OASIS scripts directory)
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



ROMS_TOOLS  is not maintained anymore and we strongly encourage ROMS_TOOLS users to switch to CROCO_TOOLS. 
