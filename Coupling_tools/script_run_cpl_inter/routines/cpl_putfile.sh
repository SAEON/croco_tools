
#-------------------------------------------------------------------------------
#                                                                      Restart
#-------------------------------------------------------------------------------
   [ ${USE_ATM} -eq 1 ] && ${io_putfile} atm.nc ${RESTDIR_OUT}/atm_${CEXPER}_${DATE_END_JOB}.nc
   [ ${USE_OCE} -eq 1 ] && ${io_putfile} oce.nc ${RESTDIR_OUT}/oce_${CEXPER}_${DATE_END_JOB}.nc
   [ ${USE_WAV} -eq 1 ] && ${io_putfile} wav.nc ${RESTDIR_OUT}/wav_${CEXPER}_${DATE_END_JOB}.nc

   [[ ${USE_ATM} -eq 1 && ${USE_OCE} -eq 1 ]] && cp *atmt_to_ocnt* ${RESTDIR_OUT}/. && cp *ocnt_to_atmt* ${RESTDIR_OUT}/. 
   [[ ${USE_ATM} -eq 1 && ${USE_WAV} -eq 1 ]] && cp *atmt_to_ww3t* ${RESTDIR_OUT}/. && cp *ww3t_to_atmt* ${RESTDIR_OUT}/.
   [[ ${USE_OCE} -eq 1 && ${USE_WAV} -eq 1 ]] && cp *ocn*_to_ww3t* ${RESTDIR_OUT}/. && cp *ww3t_to_ocnt* ${RESTDIR_OUT}/. 

    if [ ${USE_TOY} -eq 1 ] ; then
        for k in `seq 0 $(( ${nbtoy} - 1 ))`; do
            printf "move ${toytype[$k]}.nc"
            ${io_putfile} ${toytype[$k]}.nc ${RESTDIR_OUT}/${toytype[$k]}_${CEXPER}_${DATE_END_JOB}.nc
        done
        [ ${USE_OCE} -eq 1 ] && cp *toy*_to_ocn* ${RESTDIR_OUT}/. && cp *ocn*_to_toy* ${RESTDIR_OUT}/. 
        [ ${USE_WAV} -eq 1 ] && cp *toy*_to_ww3t* ${RESTDIR_OUT}/. && cp *ww3t_to_toy* ${RESTDIR_OUT}/.
        [ ${USE_ATM} -eq 1 ] && cp *toy*_to_atmt* ${RESTDIR_OUT}/. && cp *atmt_to_toy* ${RESTDIR_OUT}/.
        if [ ${nbtoy} -gt 1 ]; then
            cp *toy*_to_toy*
	fi
    fi


   cpfile2 grids.nc ${RESTDIR_OUT}/. && cpfile2 masks.nc ${RESTDIR_OUT}/. && cpfile2 areas.nc ${RESTDIR_OUT}/.




