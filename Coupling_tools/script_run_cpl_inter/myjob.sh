################################################################################
############################ USER CHANGES ######################################
################################################################################

#-------------------------------------------------------------------------------
# Job submission settings
#-------------------------------------------------------------------------------
# Job walltime 
export TIMEJOB=1800

# Project Id (on which hours are taken, if needed)
export projectid=""

#-------------------------------------------------------------------------------
# Run date settings
#-------------------------------------------------------------------------------
# Your run can be divided into several jobs (e.g.: 1 year run into 12 jobs of 1 month)

# Full Experiment 
# Start of Experiment
export YEAR_BEGIN_EXP=2005
export MONTH_BEGIN_EXP=1
export DAY_BEGIN_EXP=1
# Duration of the Experiment
export EXP_DUR_MTH=$(( 3 * 1 ))
export EXP_DUR_DAY=0

# Start date of the first Job
export YEAR_BEGIN_JOB=2005
export MONTH_BEGIN_JOB=1
export DAY_BEGIN_JOB=1
# Duration of each Job
export JOB_DUR_MTH=1
export JOB_DUR_DAY=0

#-------------------------------------------------------------------------------
# Job submission type
#-------------------------------------------------------------------------------
export CHAINED_JOB="FALSE" #If TRUE , place all the jobs in the queue at the begining,
                           #If FALSE, place job in the queue after the previous one ended

# MODE_TEST: 
# The MODE_TEST env variable is a suffix of the experiment name
# in order to run several tests within one experiment.
# Several tests can be launched simultaneously 
# However, production mode and test mode cannot be used simultaneously
export MODE_TEST=""    #   mode Production 

# DEBUG for test
# To help debug the submit_job. Can be of some use when working on chained_job.sh
export SCRIPT_DEBUG="FALSE"

#-------------------------------------------------------------------------------
# Multi-Step
#-------------------------------------------------------------------------------
LOADL_STEP_NAME="XXX"   # XXX will do all steps
                        # otherwise "get_file"/"run_model"/"put_file" individual steps can be selected 

#-------------------------------------------------------------------------------
# Number of core used
# ------------------------------------------------------------------------------
# mpi launch command: ccc_mprun :for irene / $MPI_LAUNCH for datarmor (or mpiexec.hydra )
MPI_LAUNCH_CMD=$MPI_LAUNCH  
export SERIAL_LAUNCH_WAV="$MPI_LAUNCH -n 1 " # serial launch for WW3 prepro: getrst_ww3.sh

# nb of CPUs for each model
# < insert here CPU > !!! DO NOT REMOVE THIS LIST used in create_config

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
#  Define the functions to get/put files
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
   printf "\n\n Machine unknown  => EXIT \n\n"
   printf "To define your Machine:\n - Define your environement in ./routines/myenv \n - Prepare your header in ./routines \n - Prepare a launch_${MACHINE} in ./routines";  exit;
fi

echo ${COMPUTER}

