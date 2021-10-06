#-------------------------------------------------------------------------------
# ATM
#-------------------------------------------------------------------------------
# namelist
export atmnamelist=namelist.input.base.complete

# Time steps
export DT_ATM=100 #100   # 100 90 75 72 60 45

# Grid size
#[ Grid size should be already put in the namelist. When coupled it is directly read in cpl_nam.sh ]

# domains
export NB_dom=1 # Number of coupled domains
export wrfcpldom='d01'

# Boundaries interval 
export interval_seconds=21600 # interval ( in sec ) of the latteral input
export auxinput4_interval=360 # interval ( in min ) of bottom input

# output settings
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
#                                          WARNING                                       ! 
# When XIOS is activated the following values (for the model) are not taken into account !
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
export atm_his_h=6                        # output interval (h)
export atm_his_frames=1000 # $((31*24))          # nb of outputs per file
export atm_diag_int_m=$((${atm_his_h}*60))  # diag output interval (m)
export atm_diag_frames=1000     # nb of diag outputs per file

