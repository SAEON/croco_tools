CROCO_TOOLS v1.3
----------------
Released date : 28 November 2022

Previous release : CROCO_TOOLS v1.2 (19 January 2022)

Reminders:
==========
CROCO sources and CROCO_TOOLS (the follow-on of ROMS_TOOLS) are now distributed separately (for croco_tools releases, see associated tab at  https://www.croco-ocean.org/download/croco-project/ ). ROMS_AGRIF  is not maintained anymore and we strongly encourage ROMS_AGRIF users to switch  to CROCO. CROCO version available directly from the git repository is a unstable development  version. Standard users should use the stable one downloaded from the web site.

# New in CROCO_TOOLS v1.3

* Improve octave compatibility

* Add global attribute in netcdf files (notably: origin date)

* Tides: add the possibility to create frc files with tides only, and add a routine to create tide-only frc files for interanual simulation

* Add pre-processing scripts for downlading glorys oceanic reanalysis data.

* Coupling: add scripts to create weight files and smoothing for the coupler (interpolation between model grids), some corrections in pre-processing for WWIII for adequations with more recent versions of WWIII, and some other minor corrections in scripts

* Biology: Pre-processing scripts have been moved to Preprocessing_tools/Bio. Update some scripts to handle PISCES-quota

* Nesting_tools: some small corrections

