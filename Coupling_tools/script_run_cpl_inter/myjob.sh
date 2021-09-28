################################################################################
############################ USER CHANGES ######################################
################################################################################

# Real job duration in sec (converted to MACHINE format in submit job)
export TIMEJOB=1800

# Project Id, on which hours will be taken (No need on datarmor)
export projectid=""

#-------------------------------------------------------------------------------
# Run date settings
#-------------------------------------------------------------------------------
# The experiment is divided into jobs: 1 experiment / n jobs 

# Full Experiment 
#                                                            Start of Experiment
export YEAR_BEGIN_EXP=2005
export MONTH_BEGIN_EXP=1
export DAY_BEGIN_EXP=1
#                                                     Duration of the Experiment
export EXP_DUR_MTH=$(( 3 * 1 ))
export EXP_DUR_DAY=0
#                                                                  Period of Job
export YEAR_BEGIN_JOB=2005
export MONTH_BEGIN_JOB=1
export DAY_BEGIN_JOB=1
#                                                            Duration of the Job
export JOB_DUR_MTH=1
export JOB_DUR_DAY=0

#-------------------------------------------------------------------------------
#  Job submission type
#-------------------------------------------------------------------------------
export CHAINED_JOB="FALSE" #If TRUE  , place all the jobs in the queue at the begining,
                          #Ff FALSE , place job in the queue after the previous one ended

# MODE_TEST: extension du nom de l'experience
#            pour tourner differents tests dans la meme experience
# on peut lancer plusieurs tests en même temps mais pas être en production et lancer des tests
export         MODE_TEST=""    #   mode Production 

#-------------------------------------------------------------------------------
#  Multi-Step
#-------------------------------------------------------------------------------
LOADL_STEP_NAME="XXX" # to access directly to "get_file"/"run_model"/"put_file" in job_base.sh 

# DEBUG for test
export SCRIPT_DEBUG="FALSE"

#-------------------------------------------------------------------------------
# Number of core used
# ------------------------------------------------------------------------------

MPI_LAUNCH_CMD=$MPI_LAUNCH  # ccc_mprun :for irene / $MPI_LAUCH for datarmor (or mpiexec.hydra )
export SERIAL_LAUNCH_WAV="$MPI_LAUNCH -n 1 " # for ww3 prepro getrst_ww3.sh

# nb of CPUs for each model
export NP_OCEX=2
export NP_OCEY=2
export NP_ATM=12
export NP_WAV=14
export NP_TOY=2
export NP_XIOS_ATM=1
export NP_XIOS_OCE=1

# additional MPI Settings for ATM 
export atm_nprocX=-1      # -1 for automatic settings
export atm_nprocY=-1      # -1 for automatic settings
export atm_niotaskpg=0    # 0 for default settings
export atm_niogp=1        # 1 for default settings

################################################################################
############################ END USER CHANGE ###################################
################################################################################

#-------------------------------------------------------------------------------
#  Calendar computation
#-------------------------------------------------------------------------------
cd ${SCRIPTDIR}
   . ./routines/caltools.sh
cd -

#-------------------------------------------------------------------------------
#  Names
#-------------------------------------------------------------------------------
export    ROOT_NAME_1="${CEXPER}_${DATE_BEGIN_JOB}_${DATE_END_JOB}${MODE_TEST}"
export              ROOT_NAME_2="${DATE_BEGIN_JOB}_${DATE_END_JOB}${MODE_TEST}"
export                                ROOT_NAME_3="${DATE_END_JOB}${MODE_TEST}"
export    jobname="job_${ROOT_NAME_1}.sh"  # File submitted. For DATRAMOR the extension is changed by .pbs

#-------------------------------------------------------------------------------
#  define the function to get/put files
#-------------------------------------------------------------------------------
export io_getfile="lnfile"
export io_putfile="mvfile"

#-------------------------------------------------------------------------------
#  Which Computer?
#-------------------------------------------------------------------------------
if [ ${MACHINE} == "IRENE" ]; then
   export QSUB="ccc_msub -m work,store,scratch"
   export COMPUTER="IRENE"
elif [ ${MACHINE} == "JEANZAY" ]; then
   export QSUB="sbatch"
   export COMPUTER="JEANZAY"
elif [ ${MACHINE} == "DATARMOR" ]; then
   export QSUB="qsub"
   export COMPUTER="DATARMOR"
   export jobname="job_${ROOT_NAME_1}.pbs"
else
   printf "\n\n Machine unknown  => EXIT \n\n"; exit;
fi

echo ${COMPUTER}

