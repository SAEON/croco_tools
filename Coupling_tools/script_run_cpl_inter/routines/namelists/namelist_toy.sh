#-------------------------------------------------------------------------------
# TOY
#-------------------------------------------------------------------------------
# type
export toytype=("wav" "atm") #oce,atm,wav

# namelist

# Time steps
### TOY ###
#!!!! WARNING !!!!
# When using toy, please put the output frequency of "toyfile" in the TSP of the model
# example: ww3.200501.nc output_frequency=3600 -----> TSP_WW3=3600
#!!!!!!!!!!!!!!!!!

# forcing files
export toyfile=("$CWORK/toy_files/ww3.frc.200501.nc" "$CWORK/toy_files/wrfout_d01_20050101_20050131.nc")
export timerange=('1,745' '2,125')

