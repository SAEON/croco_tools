#!/bin/bash
#MSUB -r Comp_wps       # request name
#MSUB -o Comp_wps.o
#MSUB -e Comp_wps.e
#MSUB -j oe          #  VERIFIER que c'est OK sur Curie XXX
#MSUB -n 16        # Total number of mpi task to use
#MSUB -T 7200        
#MSUB -x
#MSUB -q skylake 
#MSUB -A yourproject
#MSUB -m scratch,store,work
#===============================================================================

#===============================================================================

umask 022
set -u

./run_wps.bash configure.namelist.wps_BENGUELA 16

