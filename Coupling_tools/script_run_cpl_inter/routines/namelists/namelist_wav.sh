#-------------------------------------------------------------------------------
# WAV
#-------------------------------------------------------------------------------
# namelist

# Time steps
export DT_WAV=3600     # TMAX = 3*TCFL
export DT_WW_PRO=1200  # TCFL --> ww3.grid to see the definition
export DT_WW_REF=1800  # TMAX / 2
export DT_WW_SRC=10

# Grid size
export wavnx=41 ; export wavny=42

# forcing files
export forcin=() # forcing file(s) list (leave empty if none)
export forcww3=() # name of ww3_prnc.inp extension/input file

# output settings
export flagout="TRUE" # Keep (TRUE) or not (FALSE) ww3 full output binary file (out_grd.ww3)
export wav_int=21600            # output interval (s)
# ww3 file to be used for creating restart file for oasis 
export wavfile=$CWORK/outputs_frc_ww3_CFSR/ww3.200501.nc # Usually done by running a frc mode on the area

