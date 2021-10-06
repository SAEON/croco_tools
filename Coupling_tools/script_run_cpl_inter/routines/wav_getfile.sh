#!/bin/bash

echo ' '
echo '-- WW3 inputs --------------'
echo 'copy and fill ww3 settings files *.inp'

cp ${WAV_NAM_DIR}/*.inp .

ms=$( printf "%02d"  ${MONTH_BEGIN_JOB} )
me=$( printf "%02d"  ${MONTH_END_JOB} )
ds=$( printf "%02d"  ${DAY_BEGIN_JOB} )
de=$( printf "%02d"  ${DAY_END_JOB} )

 ## - Fill ww3_grid.inp file -##
sed -e "s/<wavdt>/${DT_WAV}/g" \
    -e "s/<wavdtPRO>/${DT_WW_PRO}/g"  -e "s/<wavdtREF>/${DT_WW_REF}/g"  -e "s/<wavdtSRC>/${DT_WW_SRC}/g"  \
    -e "s/<wavnx>/${wavnx}/g"   -e "s/<wavny>/${wavny}/g"  \
    -e "s/<hmin>/${hmin}/g" \
    ${WAV_NAM_DIR}/ww3_grid.inp.base > ./ww3_grid.inp

 ## - Fill ww3_ounf.inp file -##
sed -e "s/<wav_int>/${wav_int}/g" \
    -e "s/<yr1>/${YEAR_BEGIN_JOB}/g"  -e "s/<mo1>/${ms}/g"  -e "s/<dy1>/${ds}/g"  -e "s/<hr1>/00/g" \
    ${WAV_NAM_DIR}/ww3_ounf.inp.base > ./ww3_ounf.inp

## - Fill ww3_shel.inp file -##
sed -e "s/<yr1>/${YEAR_BEGIN_JOB}/g"  -e "s/<mo1>/${ms}/g"  -e "s/<dy1>/${ds}/g"  -e "s/<hr1>/00/g"  \
    -e "s/<yr2>/${YEAR_END_JOB}/g"  -e "s/<mo2>/${me}/g"  -e "s/<dy2>/${de}/g"  -e "s/<hr2>/24/g" \
    -e "s/<wav_int>/${wav_int}/g"  -e "s/<wav_rst>/$(( ${TOTAL_JOB_DUR} * 24 * 3600))/g" \
    -e "s/<wavdt>/${DT_WAV}/g" \
    ${WAV_NAM_DIR}/ww3_shel.inp.base.${RUNtype} > ./ww3_shel.inp


echo 'link ww3 input files and copy associated settings files'
lengthforc=${#forcww3[@]}

for k in `seq 0 $(( ${lengthforc} - 1))` ; do
    echo "ln -sf ${WAV_FILES_DIR}/${forcin[$k]} ./${forcww3[$k]}.nc"
    ${io_getfile} ${WAV_FILES_DIR}/${forcin[$k]} ./${forcww3[$k]}.nc

    echo "cp -f ${WAV_NAM_DIR}/ww3_prnc.inp.${forcww3[$k]} ./"
    cpfile ${WAV_NAM_DIR}/ww3_prnc.inp.${forcww3[$k]} ./
 done

cp ${WAV_FILES_DIR}/*.inp ./.
