#-------------------------------------------------------------------------------
#                                                                      Restart
#-------------------------------------------------------------------------------
if [ ${DATE_BEGIN_JOB} -eq ${DATE_BEGIN_EXP} ]
then
    [ ${MONTH_BEGIN_JOB} -le 9 ] && mm=0${MONTH_BEGIN_JOB} || mm=${MONTH_BEGIN_JOB}
    [ ${DAY_BEGIN_JOB}   -le 9 ] && dd=0${DAY_BEGIN_JOB}   || dd=${DAY_BEGIN_JOB}

 me=$( printf "%02d"  ${MONTH_END_EXP} )

 filelist='wrfinput_d01' 
 if [ $NB_dom -ge 2 ] ; then
  filelist="$filelist wrfinput_d02"
  if [ $NB_dom -eq 3 ] ; then
   filelist="$filelist wrfinput_d03"
  fi
 fi
 for file in $filelist
  do
   echo "ln -sf ${ATM_FILES_DIR}${file}_${YEAR_BEGIN_JOB}_${mm}_${YEAR_END_JOB}_${me} ./$file"
   ln -sf ${ATM_FILES_DIR}/${file}_${YEAR_BEGIN_JOB}_${mm}_${YEAR_END_JOB}_${me} ./$file
  done

  for dom in $wrfcpldom ; do
     echo 'set CPLMASK to 1 in coupled domain'$dom
     echo "ncap2 -O -s 'CPLMASK(:,0,:,:)=(LANDMASK-1)*(-1)' ./wrfinput_$dom ./wrfinput_$dom"
     module load $ncomod
     ncap2 -O -s "CPLMASK(:,0,:,:)=(LANDMASK-1)*(-1)" ./wrfinput_$dom ./wrfinput_$dom
     module unload $ncomod

   done

else
    touch ls_l/getfile_atm_restarts.txt
    for file in `${MACHINE_STOCKAGE} ls ${RESTDIR_IN}/wrfrst_d0?_*`
# for i in ${RESTDIR_IN}/wrfrst_d01_*
    do
	${io_getfile} ${file} . >> ls_l/getfile_atm_restarts.txt
    done
fi
