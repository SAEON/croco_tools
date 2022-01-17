CROCO_TOOLS v1.2
----------------
Released date : 19 January 2022

Previous release : CROCO_TOOLS v1.1 (October 2019)

Reminders:
==========
CROCO sources and CROCO_TOOLS (the follow-on of ROMS_TOOLS) are now distributed separately (for croco_tools releases, see associated tab at  https://www.croco-ocean.org/download/croco-project/ ). ROMS_AGRIF  is not maintained anymore and we strongly encourage ROMS_AGRIF users to switch  to CROCO. CROCO version available directly from the git repository is a unstable development  version. Standard users should use the stable one downloaded from the web site.

# New in CROCO_TOOLS v1.2

## Interannual forcing pre-processing

_full description :_ [_https://croco-ocean.gitlabpages.inria.fr/croco_doc/tutos/tutos.05.prepro.matlab.inter.html_](https://croco-ocean.gitlabpages.inria.fr/croco_doc/tutos/tutos.05.prepro.matlab.inter.html)

* croco_pytools toolbox from Evan Mason has been removed from the repository and is now available on croco website in the documentation section <https://www.croco-ocean.org/documentation/> under "Useful Links". A new python toolbox for preparing CROCO files is under development, and will be available in a future release.
* New option to build interannual forcing with 2 digit month format

### Atmospheric forcing

* Aforc_ERA5/ : new scripts to create atmospheric forcing using the ECMWF-ERA5 hourly reanalysis

### Oceanic forcing

* Oforc_OGCM/ : new scripts to create oceanic lateral forcing and initialisation using the CMEMS GLORYS1/12 daily reanalysis : _make_OGCM_mercator.m_ and _download_mercator_python.m_

## Runoff

_full description :_ [_https://croco-ocean.gitlabpages.inria.fr/croco_doc/tutos/tutos.12.rivers.html_](https://croco-ocean.gitlabpages.inria.fr/croco_doc/tutos/tutos.12.rivers.html)

* Rivers/ : Some bug fixes regarding the runoff positioning

## Coupling

_full description :_ [_https://croco-ocean.gitlabpages.inria.fr/croco_doc/tutos/tutos.16.coupling.advanced.html_](https://croco-ocean.gitlabpages.inria.fr/croco_doc/tutos/tutos.16.coupling.advanced.html)

* Coupling_tools: The coupling toolbox is now splitted into croco_tools/Coupling_tools and croco/SCRIPTS/SCRIPTS_COUPLING. The latter contains the full toolbox to configure and launch coupled simulations. croco_tools/Coupling_tools now contains only useful scripts for pre-processing files for the different coupled models. When using all-prod-cpl in create_config.bash, these pre-processing scripts will be placed under the PREPRO directory in your configuration directory.


 _   _                      __                 _
| | | | __ ___   _____     / _|_   _ _ __     | |
| |_| |/ _` \ \ / / _ \   | |_| | | | '_ \    | |
|  _  | (_| |\ V /  __/   |  _| |_| | | | |   |_|
|_| |_|\__,_| \_/ \___|   |_|  \__,_|_| |_|   (_)


