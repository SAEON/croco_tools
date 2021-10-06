#-------------------------------------------------------------------------------
# OCE
#-------------------------------------------------------------------------------
# namelist [Info: grid size is directly read in oce_compile.sh and cpl_nam.sh ]

# Online Compilation
export ONLINE_COMP=0

# Time steps
export DT_OCE=1200
export NDTFAST=60

# Parameter 
export hmin=75; # minimum water depth in CROCO, delimiting coastline in WW3 

# domains
export AGRIFZ=0

# forcing files
export ini_ext='ini_SODA' # ini extension file (ini_SODA,...)
export bry_ext='bry_SODA' # bry extension file (bry_SODA,...)
export surfrc_flag="FALSE" # Flag if surface forcing is needed (FALSE if cpl)
export interponline=0 # switch (1=on, 0=off) for online surface interpolation
export frc_ext='blk_CFSR' # surface forcing extension(blk_CFSR, frc_CFSR,...). If interponline=1 just precise the type (ECMWF, CFSR,AROME,...)
export tide_flag="FALSE" # the forcing extension must be blk_??? otherwise tide forcing overwrites it 

# output settings
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#                                          WARNING                                       ! 
# When XIOS is activated the following values (for the model) are not taken into account !
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
export oce_nhis=18     # history output interval (in number of timesteps) 
export oce_navg=18     # average output interval (in number of timesteps) 


