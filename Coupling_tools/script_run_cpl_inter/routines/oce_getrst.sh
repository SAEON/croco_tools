#-------------------------------------------------------------------------------
#                                                                      Restart
#-------------------------------------------------------------------------------

if [ ${DATE_BEGIN_JOB} -eq ${DATE_BEGIN_EXP} ]; then
    cur_Y=$( echo $DATE_BEGIN_JOB | cut -c 1-4 )
    cur_M=$( echo $DATE_BEGIN_JOB | cut -c 5-6 )
    for nn in $( seq 0 ${AGRIFZ} ); do
       if [ ${nn} -gt 0 ]; then
           agrif_ext=".${nn}"
       else
           agrif_ext=""
       fi
           ${io_getfile} ${OCE_FILES_DIR}/croco_${ini_ext}_Y${cur_Y}M${cur_M}.nc${agrif_ext}                  croco_ini.nc${agrif_ext}
    done
else
    for nn in $( seq 0 ${AGRIFZ} ); do
       if [ ${nn} -gt 0 ]; then
           agrif_ext=".${nn}"
       else
           agrif_ext=""
       fi
       ${io_getfile} ${RESTDIR_IN}/croco_rst_${DATE_END_JOBm1}.nc${agrif_ext} croco_ini.nc${agrif_ext}
    done
fi
