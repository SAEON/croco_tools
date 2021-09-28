#-------------------------------------------------------------------------------
#                                                                      Restart
#-------------------------------------------------------------------------------

if [ ${DATE_BEGIN_JOB} -eq ${DATE_BEGIN_EXP} ]; then

    echo 'WW3 pre-processing before run:'
 # WW3 grid 

    echo "${SERIAL_LAUNCH_WAV}ww3_grid &> grid.out"
    ${SERIAL_LAUNCH_WAV}ww3_grid &> grid.out
    if [ ! -e mod_def.ww3 ]; then
        echo 'ERROR when running ww3_grid, mod_def.ww3 does not exist'
        exit 1
    fi
 # WW3 prnc

    for  k in `seq 0 $(( ${lengthforc} - 1))` ; do
        echo "ln -sf ww3_prnc.inp.${forcww3[$k]} ww3_prnc.inp"
        ${io_getfile} ww3_prnc.inp.${forcww3[$k]}                          ww3_prnc.inp

        echo "${SERIAL_LAUNCH_WAV}ww3_prnc &> prnc.${forcww3[$k]}.out"
        ${SERIAL_LAUNCH_WAV}ww3_prnc &> prnc.${forcww3[$k]}.out
        if [ ! -e ${forcww3[$k]}.ww3 ]; then
            echo 'ERROR when running ww3_prnc for '${forcww3[$k]}
            exit 1
        fi
    done
 # WW3 strt

     echo "${SERIAL_LAUNCH_WAV}ww3_strt &> strt.out"
     ${SERIAL_LAUNCH_WAV}ww3_strt &> strt.out
     if [ ! -e restart.ww3 ]; then
        echo 'ERROR when running ww3_strt, restart.ww3 does not exist'
        exit 1
     fi



else

   rstfile='mod_def.ww3 restart.ww3'
   for k in `seq 0 $(( ${lengthforc} - 1))` ; do
       rstfile="${rstfile} ${forcww3[$k]}.ww3"
   done
   
   for file in ${rstfile} ; do
       cpfile ${RESTDIR_IN}/${file}_${DATE_END_JOBm1} ./${file}
   done

fi

