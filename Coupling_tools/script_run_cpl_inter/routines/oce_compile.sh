#!/bin/bash
if [ ${DATE_BEGIN_JOB} -eq ${DATE_BEGIN_EXP} ]; then

#-------------------------------------------------------------------------------
#   Get files
#-------------------------------------------------------------------------------

#    CROCOSRC=/ccc/work/cont005/ra0542/massons/now/models/croco
    cd ${JOBDIR_ROOT}
    printf "\n\n CROCO online compilation is on \n\n" 
#    rsync -a ${OCE}/OCEAN/ OCEAN/
    [ ! -d Run_croco ] && rm -rf Run_croco
    mkdir Run_croco
    rsync -a ${OCE}/param.h   Run_croco/.
    rsync -a ${OCE}/cppdefs.h Run_croco/.
    rsync -a ${OCE}/jobcomp   Run_croco/.

#-------------------------------------------------------------------------------
#   sed on files
#-------------------------------------------------------------------------------
    cd Run_croco
    # Option of compilation
#    sed -e "s/-O3 /-O3 -xAVX /" jobcomp > tmp$$i
sed -e "s|SOURCE=.*|SOURCE=${OCE} |g" \
    -e "s|FC=.*|FC=${FC}|g" \
    -e "s|MPIF90=.*|MPIF90=${MPIF90}|g" \
    -e "s|PRISM_ROOT_DIR=.*|PRISM_ROOT_DIR="${CPL}"|g" \
    -e "s|XIOS_ROOT_DIR=.*|XIOS_ROOT_DIR=${XIOS}|g" \
    -e "s|-O3|-O3|g" \
    ./jobcomp > tmp$$
    mv tmp$$ jobcomp

    # MPI and Grid size
    sed -e "s/# define BENGUELA_LR/# define ${CEXPER}/g" \
        -e "s/# undef  MPI/# define  MPI/g" \
        ./cppdefs.h > tmp$$
    mv tmp$$ cppdefs.h
    printf "\n\nReading grid size in ${OCE_FILES_DIR}/croco_grd.nc \n\n"
    dimx=$( ncdump -h  ${OCE_FILES_DIR}/croco_grd.nc  | grep "xi_rho =" | cut -d ' ' -f 3)
    dimy=$( ncdump -h  ${OCE_FILES_DIR}/croco_grd.nc | grep "eta_rho =" | cut -d ' ' -f 3)
    dimz=$( ncdump -h  ${OCE_FILES_DIR}/croco_${ini_ext}*.nc | grep "s_rho =" | cut -d ' ' -f 3)
    printf "\nGrid size is (in Lx X Ly X Nz ) : ${dimx}X${dimy}X${dimz}\n"
    #add new line for new conf in param.h
    sed -i '187i#  elif defined NEWCONFIG' param.h
    sed -i '188i      parameter (LLm0=DIMX,   MMm0=DIMY,   N=DIMZ)' param.h
    # update necessary things
    sed -e "s/NP_XI *= *[0-9]* *,/NP_XI=${NP_OCEX},/g" \
        -e "s/NP_ETA *= *[0-9]* *,/NP_ETA=${NP_OCEY},/g" \
        -e "s/NEWCONFIG/${CEXPER}/g" \
        -e "s/DIMX/$(( ${dimx} - 2 ))/g" \
        -e "s/DIMY/$(( ${dimy} - 2 ))/g" \
        -e "s/DIMZ/${dimz}/g" \
        param.h > tmp$$
    mv tmp$$ param.h
#
    if [ $USE_CPL -ge 1 ]; then
        if [ $USE_ATM -eq 1 ] || [ $USE_TOYATM -eq 1 ]; then 
            sed -e "s/#  *undef  *OA_COUPLING/# define OA_COUPLING/g" cppdefs.h > tmp$$
            printf "\n Coupling with ATM \n"
	    mv tmp$$ cppdefs.h
	else
            sed -e "s/#  *define  *OA_COUPLING/# undef OA_COUPLING/g" cppdefs.h > tmp$$
	    mv tmp$$ cppdefs.h
	fi
        if [ $USE_WAV -eq 1 ] [ $USE_TOYWAV -eq 1 ]; then
            sed -e "s/#  *undef  *OW_COUPLING/# define OW_COUPLING/g" cppdefs.h > tmp$$
            printf "\n Coupling with WAV \n"
	    mv tmp$$ cppdefs.h
        else
            sed -e "s/#  *define  *OW_COUPLING/# undef OW_COUPLING/g" cppdefs.h > tmp$$
	    mv tmp$$ cppdefs.h
        fi
            
    fi
#
    if [[ ${surfrc_flag} == "TRUE" && ${frc_ext} != *'frc'* ]]; then
	sed -e "s/#  *undef  *BULK_FLUX/# define BULK_FLUX/g" cppdefs.h > tmp$$
        mv tmp$$ cppdefs.h
        if [ ${interponline} == 1 ]; then
	    sed -e "s/#  *undef  *ONLINE/# define ONLINE/g" \
		-e "s/#  *undef  *${frc_ext}/# define ${frc_ext}/g" \
	    cppdefs.h > tmp$$
            printf "\n Online bulk activated with ${frc_ext}\n"
            mv tmp$$ cppdefs.h
        else 
            printf "\n Bulk activated\n"
        fi
    fi

    if [[ ${bry_ext} == *'bry'* ]]; then
        sed -e "s/#  *define  *CLIMATOLOGY/# undef CLIMATOLOGY/g" \
	    -e "s/#  *undef *FRC_BRY/# define FRC_BRY/g" \
	cppdefs.h > tmp$$
        printf "\n Lateral forcing is BRY\n"
        mv tmp$$ cppdefs.h
    else
        printf "\n Lateral forcing is CLM\n"
    fi
#
    if [ ${tide_flag} == "TRUE" ]; then
	sed -e "s/#  *undef  *TIDES/# define TIDES/g" cppdefs.h > tmp$$
        mv tmp$$ cppdefs.h
        printf "\n Tides are taken into account\n"
    fi
#
    if [ ${USE_XIOS_OCE} -eq 1 ]; then
	sed -e "s/#  *undef  *XIOS/# define XIOS/g" cppdefs.h > tmp$$
    	mv tmp$$ cppdefs.h
        if [ ${USE_CPL} ] ;then
            sed -e "s/XIOS_withOASIS=.*/XIOS_withOASIS=1/g" cppdefs.h > tmp$$
            mv tmp$$ cppdefs.h
        else
            sed -e "s/XIOS_withOASIS=.*/XIOS_withOASIS=0/g" cppdefs.h > tmp$$
            mv tmp$$ cppdefs.h
        fi
        printf "\n Output will be handled by XIOS\n"
    fi
#
    if [ $AGRIFZ -eq 0 ]; then
        sed -e "s/#  *define  *AGRIF/# undef AGRIF/g" cppdefs.h > tmp$$
        sed -e "s/MAKE  *\-j  *[1-9]/MAKE -j 8/g" jobcomp > tmp2$$
    else
        sed -e "s/#  *undef  *AGRIF/# define AGRIF/g" cppdefs.h > tmp$$
        sed -e "s/MAKE  *\-j  *[1-9]/MAKE -j 1/g" jobcomp > tmp2$$
    fi
#
    mv tmp$$ cppdefs.h
    mv tmp2$$ jobcomp

#-------------------------------------------------------------------------------
#   compile
#-------------------------------------------------------------------------------

    chmod 755 jobcomp
    time ./jobcomp >& log.compil
    mv croco croco.${RUNtype}
# save exe for next jobs
    rsync -av croco.${RUNtype} ${EXEDIR}/crocox
    [ ${USE_XIOS_OCE} -eq 1 ] && { cp *.xml ${XIOS_NAM_DIR}/ ;}
    cd ${EXEDIR}
else
    
    rsync -av ${JOBDIR_ROOT}/Run_croco/croco.${RUNtype} crocox

fi

