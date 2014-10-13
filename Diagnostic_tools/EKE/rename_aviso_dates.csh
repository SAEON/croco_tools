#! /bin/csh
#
# Give a new name to the AVISO SSH files and create a symbolic link
#
#
#
#0        1         2         3         4         5         6         7         8         9
#1234567890123456789012345678901234567890123456789012345678901234567890123456789012345678901234567890
#dt_upd_global_merged_msla_h_19991124_19991124_20051108.nc.gz
#/u/AVISO/madt_global_grid_all_sat_merged/h/1994/dt_global_allsat_madt_h_19941231_20140106.nc
#
#
#
set DIR1=/u/AVISO/madt_global_grid_all_sat_merged/h/
set FNAME1=dt_global_allsat_madt_h_
set DIR2=/u/AVISO/madt_h_all_sat/
set FNAME2=madt_h_

@ YEAR = 1993
while ($YEAR <= 2014)
  echo $YEAR
  echo ${DIR1}${YEAR}/${FNAME1}
  foreach AVISOFILE (`ls ${DIR1}${YEAR}/${FNAME1}*.nc`)
    echo ${AVISOFILE}
    set DATE  = `echo $AVISOFILE | cut -c73-80`
    echo ${DATE} 
    echo ln -s ${AVISOFILE} ${DIR2}${FNAME2}${DATE}.nc
    rm  ${DIR2}${FNAME2}${DATE}.nc
    ln -s ${AVISOFILE} ${DIR2}${FNAME2}${DATE}.nc
  end
  @ YEAR += 1
end
