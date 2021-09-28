. ${SCRIPTDIR}/routines/caltools.sh
##

## -------------------------- #
## - Copy TOY input files -   #
## -------------------------- #


#-------
## Number of time step per day
#-------


echo "fill TOY model namelist"

if [ ${nbtoy} -eq 1 ] ; then
    #-------
    ## Number of time step per day
    #-------
    TOY_NTSP_DAY=$(( 86400 / ${TSP_TOY[0]} ))
    TOY_NTIMES=$(( ( ${JDAY_END_JOB} - ${JDAY_BEGIN_JOB} + 1 ) * ${TOY_NTSP_DAY}     ))
    #
    sed -e "s/<toydt>/${TSP_TOY[0]}/g" -e "s/<toytimes>/${TOY_NTIMES}/g"   \
    ${TOY_NAM_DIR}/${toynamelist[0]} > ./TOYNAMELIST.nam
#
else 
    for k in `seq 0 $(( ${nbtoy} - 1 ))` ; do
        #-------
	## Number of time step per day
	#-------
        TOY_NTSP_DAY=$(( 86400 / ${TSP_TOY[$k]} ))
	TOY_NTIMES=$(( ( ${JDAY_END_JOB} - ${JDAY_BEGIN_JOB} + 1 ) * ${TOY_NTSP_DAY}     ))
        #
        sed -e "s/<toydt>/${TSP_TOY[$k]}/g" -e "s/<toytimes>/${TOY_NTIMES}/g"   \
    ${TOY_NAM_DIR}/${toynamelist[$k]} > ./TOYNAMELIST.nam.${toytype[$k]}
    done
fi
