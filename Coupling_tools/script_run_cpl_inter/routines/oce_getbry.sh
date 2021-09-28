#!/bin/bash


for i in `seq 0 $(( ${JOB_DUR_MTH}-1 ))`; do
    if [ ${JOB_DUR_MTH} -eq 1 ]; then
        cur_Y=$( echo $DATE_BEGIN_JOB | cut -c 1-4 )
        cur_M=$( echo $DATE_BEGIN_JOB | cut -c 5-6 )
        ln -sf ${OCE_FILES_DIR}/croco_${bry_ext}_Y${cur_Y}M${cur_M}.nc croco_bry.nc
    elif [ ${i} -eq 0 ]; then
        cur_Y=$( echo $DATE_BEGIN_JOB | cut -c 1-4 )
        cur_M=$( echo $DATE_BEGIN_JOB | cut -c 5-6 )

        varlist='spherical Vtransform Vstretching tstart theta_s theta_b Tcline hc sc_r sc_w Cs_r Cs_w' # One dimension stuffs that don't change 

        for varn in ${varlist} ; do
            ncks -A -v ${varn} ${OCE_FILES_DIR}/croco_${bry_ext}_Y${cur_Y}M${cur_M}.nc croco_bry.nc
        done
        string=$( ncdump -h ${OCE_FILES_DIR}/croco_${bry_ext}_Y${cur_Y}M${cur_M}.nc | grep double )
        ns=$( ncdump -h ${OCE_FILES_DIR}/croco_${bry_ext}_Y${cur_Y}M${cur_M}.nc | grep -c double )

        for j in `seq 21 $ns`; do
            if [ ${j} -eq 21 ] ; then
                var=bry_time
                dimt=bry_time
            else
                end=$( echo $string | cut -d';' -f ${j})
                var1=$( echo $end | cut -d'(' -f 1)
                var=$(echo $var1 | cut -d' ' -f 2)
                dimt1=$( echo $end | cut -d'(' -f 2)
                dimt=$(echo $dimt1 | cut -d',' -f 1)
            fi

            ncks -O -F -v ${var} -d $dimt,1,2 ${OCE_FILES_DIR}/croco_${bry_ext}_Y${cur_Y}M${cur_M}.nc out_${var}_${i}.nc
            ncks -O --mk_rec_dmn $dimt out_${var}_${i}.nc out_${var}.nc
            \rm -f out_${var}_${i}.nc
        done

    else
        mdy=$( valid_date $(( $MONTH_BEGIN_JOB + $i )) $DAY_BEGIN_JOB $YEAR_BEGIN_JOB )
        cur_Y=$( printf "%04d\n"  $( echo $mdy | cut -d " " -f 3) )
        cur_M=$( printf "%02d\n"  $( echo $mdy | cut -d " " -f 1) )

        string=$( ncdump -h ${OCE_FILES_DIR}/croco_${bry_ext}_Y${cur_Y}M${cur_M}.nc | grep double )
        ns=$( ncdump -h ${OCE_FILES_DIR}/croco_${bry_ext}_Y${cur_Y}M${cur_M}.nc | grep -c double )

        for j in `seq 21 $ns`; do
            if [ ${j} -eq 21 ] ; then
                var=bry_time
                dimt=bry_time
            else
                end=$( echo $string | cut -d';' -f ${j})
                var1=$( echo $end | cut -d'(' -f 1)
                var=$(echo $var1 | cut -d' ' -f 2)
                dimt1=$( echo $end | cut -d'(' -f 2)
                dimt=$(echo $dimt1 | cut -d',' -f 1)
            fi

            if [ $i -eq $(( ${JOB_DUR_MTH}-1 )) ]; then
               ncks -O -F -v ${var} -d $dimt,2,3 ${OCE_FILES_DIR}/croco_${bry_ext}_Y${cur_Y}M${cur_M}.nc out_${var}_${i}.nc
               ncrcat -O out_${var}.nc out_${var}_${i}.nc out_${var}.nc
               \rm -f out_${var}_${i}.nc
               ncks -O --fix_rec_dmn $dimt out_${var}.nc out_${var}.nc
               ncks -A out_${var}.nc croco_bry.nc ; \rm -f out_${var}.nc
               [ ${j} -eq 21 ] && ncks -A -v tend ${OCE_FILES_DIR}/croco_${bry_ext}_Y${cur_Y}M${cur_M}.nc croco_bry.nc

            else
                ncks -O -F -v ${var} -d $dimt,2 ${OCE_FILES_DIR}/croco_${bry_ext}_Y${cur_Y}M${cur_M}.nc out_${var}_${i}.nc
                ncrcat -O out_${var}.nc out_${var}_${i}.nc out_${var}.nc
                \rm -f out_${var}_${i}.nc
            fi
        done
    fi
done

if [ ${JOB_DUR_MTH} -eq 0 ] ; then
    printf "Job duration is less than a month ---> Using netcdf of the current month\n"
    cur_Y=$( echo $DATE_BEGIN_JOB | cut -c 1-4 )
    cur_M=$( echo $DATE_BEGIN_JOB | cut -c 5-6 )    

    ln -sf ${OCE_FILES_DIR}/croco_${bry_ext}_Y${cur_Y}M${cur_M}.nc croco_bry.nc
fi	


