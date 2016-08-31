#include "cppdefs.h"


      subroutine check_switches1 (ierr)

!!!!!! WARNING: THIS IS A MACHINE GENERATED CODE, DO NOT EDIT! !!!!!!
!!!!!! This file needs to be updated only if new CPP-switches  !!!!!!
!!!!!! were introduced into "cppdefs.h".  NO ACTION IS NEEDED  !!!!!!
!!!!!! if changes in "cppdefs.h" are limited to activation or  !!!!!!
!!!!!! deactivation of previously known switches.              !!!!!!
!!!!!! To refresh this file compile and execute "cppcheck.F"   !!!!!!
!!!!!! as an independent program, or use commands              !!!!!!
!!!!!! "make checkdefs" or "make depend".                      !!!!!!
!!!!!! Number of Configuration Choices:                     20 !!!!!!
!!!!!! Total number of CPP-switches:                       334 !!!!!!

      implicit none
      integer ierr, is,ie, iexample
#include "param.h"
#include "strings.h"
#ifdef MPI
# include "scalars.h"
#endif
      MPI_master_only write(stdout,'(/1x,A/)')
     &      'Activated C-preprocessing Options:'
      do is=1,max_opt_size
        Coptions(is:is)=' '
      enddo
      iexample=0
      is=1
#ifdef BASIN
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'BASIN'
      ie=is + 4
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='BASIN'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CANYON_A
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'CANYON_A'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='CANYON_A'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CANYON_B
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'CANYON_B'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='CANYON_B'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef EQUATOR
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'EQUATOR'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='EQUATOR'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef GRAV_ADJ
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'GRAV_ADJ'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='GRAV_ADJ'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef INNERSHELF
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'INNERSHELF'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='INNERSHELF'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef RIVER
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'RIVER'
      ie=is + 4
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='RIVER'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OVERFLOW
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'OVERFLOW'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='OVERFLOW'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SEAMOUNT
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'SEAMOUNT'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='SEAMOUNT'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SHELFRONT
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'SHELFRONT'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='SHELFRONT'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SOLITON
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'SOLITON'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='SOLITON'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef UPWELLING
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'UPWELLING'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='UPWELLING'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef VORTEX
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'VORTEX'
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='VORTEX'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef INTERNAL
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'INTERNAL'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='INTERNAL'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef JET
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'JET'
      ie=is + 2
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='JET'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef RIP
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'RIP'
      ie=is + 2
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='RIP'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SHOREFACE
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'SHOREFACE'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='SHOREFACE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef THACKER
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'THACKER'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='THACKER'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TANK
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'TANK'
      ie=is + 3
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='TANK'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef REGIONAL
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'REGIONAL'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='REGIONAL'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef BENGUELA_LR
      MPI_master_only write(stdout,'(10x,A)') 'BENGUELA_LR'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='BENGUELA_LR'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OPENMP
      MPI_master_only write(stdout,'(10x,A)') 'OPENMP'
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='OPENMP'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef MPI
      MPI_master_only write(stdout,'(10x,A)') 'MPI'
      ie=is + 2
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='MPI'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef AGRIF
      MPI_master_only write(stdout,'(10x,A)') 'AGRIF'
      ie=is + 4
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='AGRIF'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef AGRIF_2WAY
      MPI_master_only write(stdout,'(10x,A)') 'AGRIF_2WAY'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='AGRIF_2WAY'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OA_COUPLING
      MPI_master_only write(stdout,'(10x,A)') 'OA_COUPLING'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='OA_COUPLING'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TIDES
      MPI_master_only write(stdout,'(10x,A)') 'TIDES'
      ie=is + 4
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='TIDES'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_EAST
      MPI_master_only write(stdout,'(10x,A)') 'OBC_EAST'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='OBC_EAST'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_WEST
      MPI_master_only write(stdout,'(10x,A)') 'OBC_WEST'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='OBC_WEST'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_NORTH
      MPI_master_only write(stdout,'(10x,A)') 'OBC_NORTH'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='OBC_NORTH'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_SOUTH
      MPI_master_only write(stdout,'(10x,A)') 'OBC_SOUTH'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='OBC_SOUTH'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef BIOLOGY
      MPI_master_only write(stdout,'(10x,A)') 'BIOLOGY'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='BIOLOGY'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef FLOATS
      MPI_master_only write(stdout,'(10x,A)') 'FLOATS'
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='FLOATS'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef STATIONS
      MPI_master_only write(stdout,'(10x,A)') 'STATIONS'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='STATIONS'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef PASSIVE_TRACER
      MPI_master_only write(stdout,'(10x,A)') 'PASSIVE_TRACER'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='PASSIVE_TRACER'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SEDIMENT
      MPI_master_only write(stdout,'(10x,A)') 'SEDIMENT'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='SEDIMENT'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef BBL
      MPI_master_only write(stdout,'(10x,A)') 'BBL'
      ie=is + 2
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='BBL'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef PARALLEL_FILES
      MPI_master_only write(stdout,'(10x,A)') 'PARALLEL_FILES'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='PARALLEL_FILES'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef AUTOTILING
      MPI_master_only write(stdout,'(10x,A)') 'AUTOTILING'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='AUTOTILING'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ETALON_CHECK
      MPI_master_only write(stdout,'(10x,A)') 'ETALON_CHECK'
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ETALON_CHECK'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CURVGRID
      MPI_master_only write(stdout,'(10x,A)') 'CURVGRID'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='CURVGRID'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SPHERICAL
      MPI_master_only write(stdout,'(10x,A)') 'SPHERICAL'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='SPHERICAL'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef MASKING
      MPI_master_only write(stdout,'(10x,A)') 'MASKING'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='MASKING'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef WET_DRY
      MPI_master_only write(stdout,'(10x,A)') 'WET_DRY'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='WET_DRY'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef NEW_S_COORD
      MPI_master_only write(stdout,'(10x,A)') 'NEW_S_COORD'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='NEW_S_COORD'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SOLVE3D
      MPI_master_only write(stdout,'(10x,A)') 'SOLVE3D'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='SOLVE3D'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef UV_COR
      MPI_master_only write(stdout,'(10x,A)') 'UV_COR'
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='UV_COR'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef UV_ADV
      MPI_master_only write(stdout,'(10x,A)') 'UV_ADV'
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='UV_ADV'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SALINITY
      MPI_master_only write(stdout,'(10x,A)') 'SALINITY'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='SALINITY'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef NONLIN_EOS
      MPI_master_only write(stdout,'(10x,A)') 'NONLIN_EOS'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='NONLIN_EOS'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SPLIT_EOS
      MPI_master_only write(stdout,'(10x,A)') 'SPLIT_EOS'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='SPLIT_EOS'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef UV_VIS2
      MPI_master_only write(stdout,'(10x,A)') 'UV_VIS2'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='UV_VIS2'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef UV_MIX_S
      MPI_master_only write(stdout,'(10x,A)') 'UV_MIX_S'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='UV_MIX_S'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef UV_VIS_SMAGO
      MPI_master_only write(stdout,'(10x,A)') 'UV_VIS_SMAGO'
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='UV_VIS_SMAGO'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TS_HADV_RSUP3
      MPI_master_only write(stdout,'(10x,A)') 'TS_HADV_RSUP3'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='TS_HADV_RSUP3'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TS_HADV_UP5
      MPI_master_only write(stdout,'(10x,A)') 'TS_HADV_UP5'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='TS_HADV_UP5'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TS_HADV_C4
      MPI_master_only write(stdout,'(10x,A)') 'TS_HADV_C4'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='TS_HADV_C4'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TS_HADV_WENO5
      MPI_master_only write(stdout,'(10x,A)') 'TS_HADV_WENO5'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='TS_HADV_WENO5'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TS_MIX_S
      MPI_master_only write(stdout,'(10x,A)') 'TS_MIX_S'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='TS_MIX_S'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TS_DIF2
      MPI_master_only write(stdout,'(10x,A)') 'TS_DIF2'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='TS_DIF2'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TS_DIF4
      MPI_master_only write(stdout,'(10x,A)') 'TS_DIF4'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='TS_DIF4'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TS_DIF_SMAGO
      MPI_master_only write(stdout,'(10x,A)') 'TS_DIF_SMAGO'
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='TS_DIF_SMAGO'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TS_MIX_ISO
      MPI_master_only write(stdout,'(10x,A)') 'TS_MIX_ISO'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='TS_MIX_ISO'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SPONGE
      MPI_master_only write(stdout,'(10x,A)') 'SPONGE'
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='SPONGE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef BODYFORCE
      MPI_master_only write(stdout,'(10x,A)') 'BODYFORCE'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='BODYFORCE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef BVF_MIXING
      MPI_master_only write(stdout,'(10x,A)') 'BVF_MIXING'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='BVF_MIXING'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LMD_MIXING
      MPI_master_only write(stdout,'(10x,A)') 'LMD_MIXING'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='LMD_MIXING'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef GLS_MIXING
      MPI_master_only write(stdout,'(10x,A)') 'GLS_MIXING'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='GLS_MIXING'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LMD_SKPP
      MPI_master_only write(stdout,'(10x,A)') 'LMD_SKPP'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='LMD_SKPP'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LMD_BKPP
      MPI_master_only write(stdout,'(10x,A)') 'LMD_BKPP'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='LMD_BKPP'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LMD_RIMIX
      MPI_master_only write(stdout,'(10x,A)') 'LMD_RIMIX'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='LMD_RIMIX'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LMD_CONVEC
      MPI_master_only write(stdout,'(10x,A)') 'LMD_CONVEC'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='LMD_CONVEC'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LMD_DDMIX
      MPI_master_only write(stdout,'(10x,A)') 'LMD_DDMIX'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='LMD_DDMIX'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LMD_NONLOCAL
      MPI_master_only write(stdout,'(10x,A)') 'LMD_NONLOCAL'
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='LMD_NONLOCAL'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef GLS_KKL
      MPI_master_only write(stdout,'(10x,A)') 'GLS_KKL'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='GLS_KKL'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef GLS_KOMEGA
      MPI_master_only write(stdout,'(10x,A)') 'GLS_KOMEGA'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='GLS_KOMEGA'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef GLS_KEPSILON
      MPI_master_only write(stdout,'(10x,A)') 'GLS_KEPSILON'
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='GLS_KEPSILON'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef GLS_GEN
      MPI_master_only write(stdout,'(10x,A)') 'GLS_GEN'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='GLS_GEN'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef KANTHA_CLAYSON
      MPI_master_only write(stdout,'(10x,A)') 'KANTHA_CLAYSON'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='KANTHA_CLAYSON'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CRAIG_BANNER
      MPI_master_only write(stdout,'(10x,A)') 'CRAIG_BANNER'
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='CRAIG_BANNER'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CANUTO_A
      MPI_master_only write(stdout,'(10x,A)') 'CANUTO_A'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='CANUTO_A'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ZOS_HSIG
      MPI_master_only write(stdout,'(10x,A)') 'ZOS_HSIG'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ZOS_HSIG'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef BULK_FLUX
      MPI_master_only write(stdout,'(10x,A)') 'BULK_FLUX'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='BULK_FLUX'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef BULK_FAIRALL
      MPI_master_only write(stdout,'(10x,A)') 'BULK_FAIRALL'
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='BULK_FAIRALL'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef BULK_LW
      MPI_master_only write(stdout,'(10x,A)') 'BULK_LW'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='BULK_LW'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef BULK_EP
      MPI_master_only write(stdout,'(10x,A)') 'BULK_EP'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='BULK_EP'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef BULK_SMFLUX
      MPI_master_only write(stdout,'(10x,A)') 'BULK_SMFLUX'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='BULK_SMFLUX'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SST_SKIN
      MPI_master_only write(stdout,'(10x,A)') 'SST_SKIN'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='SST_SKIN'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_DIURNAL_SW
      MPI_master_only write(stdout,'(10x,A)') 'ANA_DIURNAL_SW'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ANA_DIURNAL_SW'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ONLINE
      MPI_master_only write(stdout,'(10x,A)') 'ONLINE'
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ONLINE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef QCORRECTION
      MPI_master_only write(stdout,'(10x,A)') 'QCORRECTION'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='QCORRECTION'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SFLX_CORR
      MPI_master_only write(stdout,'(10x,A)') 'SFLX_CORR'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='SFLX_CORR'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CLIMATOLOGY
      MPI_master_only write(stdout,'(10x,A)') 'CLIMATOLOGY'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='CLIMATOLOGY'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ZCLIMATOLOGY
      MPI_master_only write(stdout,'(10x,A)') 'ZCLIMATOLOGY'
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ZCLIMATOLOGY'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef M2CLIMATOLOGY
      MPI_master_only write(stdout,'(10x,A)') 'M2CLIMATOLOGY'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='M2CLIMATOLOGY'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef M3CLIMATOLOGY
      MPI_master_only write(stdout,'(10x,A)') 'M3CLIMATOLOGY'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='M3CLIMATOLOGY'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TCLIMATOLOGY
      MPI_master_only write(stdout,'(10x,A)') 'TCLIMATOLOGY'
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='TCLIMATOLOGY'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ZNUDGING
      MPI_master_only write(stdout,'(10x,A)') 'ZNUDGING'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ZNUDGING'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef M2NUDGING
      MPI_master_only write(stdout,'(10x,A)') 'M2NUDGING'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='M2NUDGING'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef M3NUDGING
      MPI_master_only write(stdout,'(10x,A)') 'M3NUDGING'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='M3NUDGING'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TNUDGING
      MPI_master_only write(stdout,'(10x,A)') 'TNUDGING'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='TNUDGING'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ROBUST_DIAG
      MPI_master_only write(stdout,'(10x,A)') 'ROBUST_DIAG'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ROBUST_DIAG'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef FRC_BRY
      MPI_master_only write(stdout,'(10x,A)') 'FRC_BRY'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='FRC_BRY'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef Z_FRC_BRY
      MPI_master_only write(stdout,'(10x,A)') 'Z_FRC_BRY'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='Z_FRC_BRY'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef M2_FRC_BRY
      MPI_master_only write(stdout,'(10x,A)') 'M2_FRC_BRY'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='M2_FRC_BRY'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef M3_FRC_BRY
      MPI_master_only write(stdout,'(10x,A)') 'M3_FRC_BRY'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='M3_FRC_BRY'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef T_FRC_BRY
      MPI_master_only write(stdout,'(10x,A)') 'T_FRC_BRY'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='T_FRC_BRY'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_BSFLUX
      MPI_master_only write(stdout,'(10x,A)') 'ANA_BSFLUX'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ANA_BSFLUX'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_BTFLUX
      MPI_master_only write(stdout,'(10x,A)') 'ANA_BTFLUX'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ANA_BTFLUX'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef PSOURCE
      MPI_master_only write(stdout,'(10x,A)') 'PSOURCE'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='PSOURCE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef PSOURCE_NCFILE
      MPI_master_only write(stdout,'(10x,A)') 'PSOURCE_NCFILE'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='PSOURCE_NCFILE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef PSOURCE_NCFILE_TS
      MPI_master_only write(stdout,'(10x,A)') 'PSOURCE_NCFILE_TS'
      ie=is +16
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='PSOURCE_NCFILE_TS'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SSH_TIDES
      MPI_master_only write(stdout,'(10x,A)') 'SSH_TIDES'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='SSH_TIDES'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef UV_TIDES
      MPI_master_only write(stdout,'(10x,A)') 'UV_TIDES'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='UV_TIDES'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TIDERAMP
      MPI_master_only write(stdout,'(10x,A)') 'TIDERAMP'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='TIDERAMP'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_M2FLATHER
      MPI_master_only write(stdout,'(10x,A)') 'OBC_M2FLATHER'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='OBC_M2FLATHER'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_M2SPECIFIED
      MPI_master_only write(stdout,'(10x,A)') 'OBC_M2SPECIFIED'
      ie=is +14
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='OBC_M2SPECIFIED'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_M2CHARACT
      MPI_master_only write(stdout,'(10x,A)') 'OBC_M2CHARACT'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='OBC_M2CHARACT'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_M2ORLANSKI
      MPI_master_only write(stdout,'(10x,A)') 'OBC_M2ORLANSKI'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='OBC_M2ORLANSKI'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_VOLCONS
      MPI_master_only write(stdout,'(10x,A)') 'OBC_VOLCONS'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='OBC_VOLCONS'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_M3ORLANSKI
      MPI_master_only write(stdout,'(10x,A)') 'OBC_M3ORLANSKI'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='OBC_M3ORLANSKI'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_TORLANSKI
      MPI_master_only write(stdout,'(10x,A)') 'OBC_TORLANSKI'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='OBC_TORLANSKI'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_M3SPECIFIED
      MPI_master_only write(stdout,'(10x,A)') 'OBC_M3SPECIFIED'
      ie=is +14
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='OBC_M3SPECIFIED'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OBC_TSPECIFIED
      MPI_master_only write(stdout,'(10x,A)') 'OBC_TSPECIFIED'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='OBC_TSPECIFIED'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef AVERAGES
      MPI_master_only write(stdout,'(10x,A)') 'AVERAGES'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='AVERAGES'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef AVERAGES_K
      MPI_master_only write(stdout,'(10x,A)') 'AVERAGES_K'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='AVERAGES_K'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef DIAGNOSTICS_TS
      MPI_master_only write(stdout,'(10x,A)') 'DIAGNOSTICS_TS'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='DIAGNOSTICS_TS'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef DIAGNOSTICS_UV
      MPI_master_only write(stdout,'(10x,A)') 'DIAGNOSTICS_UV'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='DIAGNOSTICS_UV'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef DIAGNOSTICS_TS_ADV
      MPI_master_only write(stdout,'(10x,A)') 'DIAGNOSTICS_TS_ADV'
      ie=is +17
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='DIAGNOSTICS_TS_ADV'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef DIAGNOSTICS_TS_MLD
      MPI_master_only write(stdout,'(10x,A)') 'DIAGNOSTICS_TS_MLD'
      ie=is +17
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='DIAGNOSTICS_TS_MLD'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef BIO_HADV_WENO5
      MPI_master_only write(stdout,'(10x,A)') 'BIO_HADV_WENO5'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='BIO_HADV_WENO5'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef PISCES
      MPI_master_only write(stdout,'(10x,A)') 'PISCES'
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='PISCES'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef BIO_NChlPZD
      MPI_master_only write(stdout,'(10x,A)') 'BIO_NChlPZD'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='BIO_NChlPZD'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef BIO_N2ChlPZD2
      MPI_master_only write(stdout,'(10x,A)') 'BIO_N2ChlPZD2'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='BIO_N2ChlPZD2'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef BIO_BioEBUS
      MPI_master_only write(stdout,'(10x,A)') 'BIO_BioEBUS'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='BIO_BioEBUS'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef DIURNAL_INPUT_SRFLX
      MPI_master_only write(stdout,'(10x,A)') 'DIURNAL_INPUT_SRFLX'
      ie=is +18
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='DIURNAL_INPUT_SRFLX'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef key_pisces
      MPI_master_only write(stdout,'(10x,A)') 'key_pisces'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='key_trc_pisces'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OXYGEN
      MPI_master_only write(stdout,'(10x,A)') 'OXYGEN'
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='OXYGEN'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef NITROUS_OXIDE
      MPI_master_only write(stdout,'(10x,A)') 'NITROUS_OXIDE'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='NITROUS_OXIDE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef DIAGNOSTICS_BIO
      MPI_master_only write(stdout,'(10x,A)') 'DIAGNOSTICS_BIO'
      ie=is +14
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='DIAGNOSTICS_BIO'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef key_trc_diaadd
      MPI_master_only write(stdout,'(10x,A)') 'key_trc_diaadd'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='key_trc_diaadd'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef key_trc_dia3d
      MPI_master_only write(stdout,'(10x,A)') 'key_trc_dia3d'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='key_trc_dia3d'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef FLOATS_GLOBAL_ATTRIBUTES
      MPI_master_only write(stdout,'(10x,A)') 'FLOATS_GLOBAL_ATTRIBUTES'
      ie=is +23
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='FLOATS_GLOBAL_ATTRIBUTES'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef IBM
      MPI_master_only write(stdout,'(10x,A)') 'IBM'
      ie=is + 2
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='IBM'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef RANDOM_WALK
      MPI_master_only write(stdout,'(10x,A)') 'RANDOM_WALK'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='RANDOM_WALK'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef DIEL_MIGRATION
      MPI_master_only write(stdout,'(10x,A)') 'DIEL_MIGRATION'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='DIEL_MIGRATION'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef RANDOM_VERTICAL
      MPI_master_only write(stdout,'(10x,A)') 'RANDOM_VERTICAL'
      ie=is +14
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='RANDOM_VERTICAL'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef RANDOM_HORIZONTAL
      MPI_master_only write(stdout,'(10x,A)') 'RANDOM_HORIZONTAL'
      ie=is +16
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='RANDOM_HORIZONTAL'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ALL_SIGMA
      MPI_master_only write(stdout,'(10x,A)') 'ALL_SIGMA'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ALL_SIGMA'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_SEDIMENT
      MPI_master_only write(stdout,'(10x,A)') 'ANA_SEDIMENT'
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ANA_SEDIMENT'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef BED_ARMOR
      MPI_master_only write(stdout,'(10x,A)') 'BED_ARMOR'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='BED_ARMOR'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_SPFLUX
      MPI_master_only write(stdout,'(10x,A)') 'ANA_SPFLUX'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ANA_SPFLUX'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_BPFLUX
      MPI_master_only write(stdout,'(10x,A)') 'ANA_BPFLUX'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ANA_BPFLUX'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_WWAVE
      MPI_master_only write(stdout,'(10x,A)') 'ANA_WWAVE'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ANA_WWAVE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_GRID
      MPI_master_only write(stdout,'(10x,A)') 'ANA_GRID'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ANA_GRID'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_INITIAL
      MPI_master_only write(stdout,'(10x,A)') 'ANA_INITIAL'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ANA_INITIAL'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_SMFLUX
      MPI_master_only write(stdout,'(10x,A)') 'ANA_SMFLUX'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ANA_SMFLUX'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_STFLUX
      MPI_master_only write(stdout,'(10x,A)') 'ANA_STFLUX'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ANA_STFLUX'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef EW_PERIODIC
      MPI_master_only write(stdout,'(10x,A)') 'EW_PERIODIC'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='EW_PERIODIC'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_VMIX
      MPI_master_only write(stdout,'(10x,A)') 'ANA_VMIX'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ANA_VMIX'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_SRFLUX
      MPI_master_only write(stdout,'(10x,A)') 'ANA_SRFLUX'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ANA_SRFLUX'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_SSFLUX
      MPI_master_only write(stdout,'(10x,A)') 'ANA_SSFLUX'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ANA_SSFLUX'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_SST
      MPI_master_only write(stdout,'(10x,A)') 'ANA_SST'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ANA_SST'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef GRAV_ADJ_SOLITON
      MPI_master_only write(stdout,'(10x,A)') 'GRAV_ADJ_SOLITON'
      ie=is +15
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='GRAV_ADJ_SOLITON'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef NBQ
      MPI_master_only write(stdout,'(10x,A)') 'NBQ'
      ie=is + 2
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='NBQ'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef INNERSHELF_EKMAN
      MPI_master_only write(stdout,'(10x,A)') 'INNERSHELF_EKMAN'
      ie=is +15
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='INNERSHELF_EKMAN'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef INNERSHELF_APG
      MPI_master_only write(stdout,'(10x,A)') 'INNERSHELF_APG'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='INNERSHELF_APG'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef NS_PERIODIC
      MPI_master_only write(stdout,'(10x,A)') 'NS_PERIODIC'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='NS_PERIODIC'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef BODYTIDE
      MPI_master_only write(stdout,'(10x,A)') 'BODYTIDE'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='BODYTIDE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef INTERNALSHELF
      MPI_master_only write(stdout,'(10x,A)') 'INTERNALSHELF'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='INTERNALSHELF'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef UV_MIX_GEO
      MPI_master_only write(stdout,'(10x,A)') 'UV_MIX_GEO'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='UV_MIX_GEO'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TS_MIX_GEO
      MPI_master_only write(stdout,'(10x,A)') 'TS_MIX_GEO'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='TS_MIX_GEO'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_SSH
      MPI_master_only write(stdout,'(10x,A)') 'ANA_SSH'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ANA_SSH'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_M2CLIMA
      MPI_master_only write(stdout,'(10x,A)') 'ANA_M2CLIMA'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ANA_M2CLIMA'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_M3CLIMA
      MPI_master_only write(stdout,'(10x,A)') 'ANA_M3CLIMA'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ANA_M3CLIMA'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_TCLIMA
      MPI_master_only write(stdout,'(10x,A)') 'ANA_TCLIMA'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ANA_TCLIMA'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef M2FILTER_FLAT
      MPI_master_only write(stdout,'(10x,A)') 'M2FILTER_FLAT'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='M2FILTER_FLAT'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_PSOURCE
      MPI_master_only write(stdout,'(10x,A)') 'ANA_PSOURCE'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ANA_PSOURCE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_JET
      MPI_master_only write(stdout,'(10x,A)') 'ANA_JET'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ANA_JET'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ZONAL_NUDGING
      MPI_master_only write(stdout,'(10x,A)') 'ZONAL_NUDGING'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ZONAL_NUDGING'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LMD_KPP
      MPI_master_only write(stdout,'(10x,A)') 'LMD_KPP'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='LMD_KPP'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef MRL_WCI
      MPI_master_only write(stdout,'(10x,A)') 'MRL_WCI'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='MRL_WCI'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef WKB_WWAVE
      MPI_master_only write(stdout,'(10x,A)') 'WKB_WWAVE'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='WKB_WWAVE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef WAVE_ROLLER
      MPI_master_only write(stdout,'(10x,A)') 'WAVE_ROLLER'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='WAVE_ROLLER'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef WAVE_STREAMING
      MPI_master_only write(stdout,'(10x,A)') 'WAVE_STREAMING'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='WAVE_STREAMING'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef MRL_CEW
      MPI_master_only write(stdout,'(10x,A)') 'MRL_CEW'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='MRL_CEW'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef WAVE_RAMP
      MPI_master_only write(stdout,'(10x,A)') 'WAVE_RAMP'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='WAVE_RAMP'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef BISCA
      MPI_master_only write(stdout,'(10x,A)') 'BISCA'
      ie=is + 4
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='BISCA'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef RIP_TOPO_2D
      MPI_master_only write(stdout,'(10x,A)') 'RIP_TOPO_2D'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='RIP_TOPO_2D'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef GRANDPOPO
      MPI_master_only write(stdout,'(10x,A)') 'GRANDPOPO'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='GRANDPOPO'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef WKB_OBC_WEST
      MPI_master_only write(stdout,'(10x,A)') 'WKB_OBC_WEST'
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='WKB_OBC_WEST'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef WAVE_FRICTION
      MPI_master_only write(stdout,'(10x,A)') 'WAVE_FRICTION'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='WAVE_FRICTION'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef WKB_KZ_FILTER
      MPI_master_only write(stdout,'(10x,A)') 'WKB_KZ_FILTER'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='WKB_KZ_FILTER'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef WKB_TIME_FILTER
      MPI_master_only write(stdout,'(10x,A)') 'WKB_TIME_FILTER'
      ie=is +14
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='WKB_TIME_FILTER'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ALLOW_SINGLE_BLOCK_MODE
      MPI_master_only write(stdout,'(10x,A)') 'ALLOW_SINGLE_BLOCK_MODE'
      ie=is +22
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ALLOW_SINGLE_BLOCK_MODE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SINGLE
      MPI_master_only write(stdout,'(10x,A)') 'SINGLE'
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='SINGLE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef RVTK_DEBUG
      MPI_master_only write(stdout,'(10x,A)') 'RVTK_DEBUG'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='RVTK_DEBUG'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OA_MCT
      MPI_master_only write(stdout,'(10x,A)') 'OA_MCT'
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='OA_MCT'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef MPI_COMM_WORLD
      MPI_master_only write(stdout,'(10x,A)') 'MPI_COMM_WORLD'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='MPI_COMM_WORLD'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OA_GRID_UV
      MPI_master_only write(stdout,'(10x,A)') 'OA_GRID_UV'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='OA_GRID_UV'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef VAR_RHO_2D
      MPI_master_only write(stdout,'(10x,A)') 'VAR_RHO_2D'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='VAR_RHO_2D'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef NBQ_REINIT
      MPI_master_only write(stdout,'(10x,A)') 'NBQ_REINIT'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='NBQ_REINIT'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TRACETXT
      MPI_master_only write(stdout,'(10x,A)') 'TRACETXT'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='TRACETXT'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CHECK_CROCO
      MPI_master_only write(stdout,'(10x,A)') 'CHECK_CROCO'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='CHECK_CROCO'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef HZR
      MPI_master_only write(stdout,'(10x,A)') 'HZR'
      ie=is + 2
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='HZR'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef RHO0
      MPI_master_only write(stdout,'(10x,A)') 'RHO0'
      ie=is + 3
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='RHO0'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef M2FILTER_NONE
      MPI_master_only write(stdout,'(10x,A)') 'M2FILTER_NONE'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='M2FILTER_NONE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef M2FILTER_POWER
      MPI_master_only write(stdout,'(10x,A)') 'M2FILTER_POWER'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='M2FILTER_POWER'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef M2FILTER_COSINE
      MPI_master_only write(stdout,'(10x,A)') 'M2FILTER_COSINE'
      ie=is +14
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='M2FILTER_COSINE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef UV_HADV_C4
      MPI_master_only write(stdout,'(10x,A)') 'UV_HADV_C4'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='UV_HADV_C4'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef VIS_COEF_3D
      MPI_master_only write(stdout,'(10x,A)') 'VIS_COEF_3D'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='VIS_COEF_3D'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef M2_HADV_UP3
      MPI_master_only write(stdout,'(10x,A)') 'M2_HADV_UP3'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='M2_HADV_UP3'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CLIMAT_UV_MIXH
      MPI_master_only write(stdout,'(10x,A)') 'CLIMAT_UV_MIXH'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='CLIMAT_UV_MIXH'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef UV_VADV_SPLINES
      MPI_master_only write(stdout,'(10x,A)') 'UV_VADV_SPLINES'
      ie=is +14
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='UV_VADV_SPLINES'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef UV_VADV_C2
      MPI_master_only write(stdout,'(10x,A)') 'UV_VADV_C2'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='UV_VADV_C2'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TS_MIX_IMP
      MPI_master_only write(stdout,'(10x,A)') 'TS_MIX_IMP'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='TS_MIX_IMP'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef DIF_COEF_3D
      MPI_master_only write(stdout,'(10x,A)') 'DIF_COEF_3D'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='DIF_COEF_3D'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TS_HADV_RSUP5
      MPI_master_only write(stdout,'(10x,A)') 'TS_HADV_RSUP5'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='TS_HADV_RSUP5'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TS_HADV_C6
      MPI_master_only write(stdout,'(10x,A)') 'TS_HADV_C6'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='TS_HADV_C6'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef NTRA_T3DMIX
      MPI_master_only write(stdout,'(10x,A)') 'NTRA_T3DMIX'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='NTRA_T3DMIX'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CLIMAT_TS_MIXH
      MPI_master_only write(stdout,'(10x,A)') 'CLIMAT_TS_MIXH'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='CLIMAT_TS_MIXH'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CLIMAT_TS_MIXH_FINE
      MPI_master_only write(stdout,'(10x,A)') 'CLIMAT_TS_MIXH_FINE'
      ie=is +18
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='CLIMAT_TS_MIXH_FINE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TS_VADV_SPLINES
      MPI_master_only write(stdout,'(10x,A)') 'TS_VADV_SPLINES'
      ie=is +14
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='TS_VADV_SPLINES'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TS_VADV_AKIMA
      MPI_master_only write(stdout,'(10x,A)') 'TS_VADV_AKIMA'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='TS_VADV_AKIMA'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TS_VADV_WENO5
      MPI_master_only write(stdout,'(10x,A)') 'TS_VADV_WENO5'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='TS_VADV_WENO5'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TS_VADV_C2
      MPI_master_only write(stdout,'(10x,A)') 'TS_VADV_C2'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='TS_VADV_C2'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef TS_VADV_FCT
      MPI_master_only write(stdout,'(10x,A)') 'TS_VADV_FCT'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='TS_VADV_FCT'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SPONGE_GRID
      MPI_master_only write(stdout,'(10x,A)') 'SPONGE_GRID'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='SPONGE_GRID'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SPONGE_DIF2
      MPI_master_only write(stdout,'(10x,A)') 'SPONGE_DIF2'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='SPONGE_DIF2'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SPONGE_VIS2
      MPI_master_only write(stdout,'(10x,A)') 'SPONGE_VIS2'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='SPONGE_VIS2'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CONST_TRACERS
      MPI_master_only write(stdout,'(10x,A)') 'CONST_TRACERS'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='CONST_TRACERS'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef BULK_SM_UPDATE
      MPI_master_only write(stdout,'(10x,A)') 'BULK_SM_UPDATE'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='BULK_SM_UPDATE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CUBIC_INTERP
      MPI_master_only write(stdout,'(10x,A)') 'CUBIC_INTERP'
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='CUBIC_INTERP'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LIMIT_BSTRESS
      MPI_master_only write(stdout,'(10x,A)') 'LIMIT_BSTRESS'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='LIMIT_BSTRESS'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef WAVE_BODY_STREAMING
      MPI_master_only write(stdout,'(10x,A)') 'WAVE_BODY_STREAMING'
      ie=is +18
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='WAVE_BODY_STREAMING'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef WAVE_SFC_BREAK
      MPI_master_only write(stdout,'(10x,A)') 'WAVE_SFC_BREAK'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='WAVE_SFC_BREAK'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef WAVE_BREAK_CT93
      MPI_master_only write(stdout,'(10x,A)') 'WAVE_BREAK_CT93'
      ie=is +14
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='WAVE_BREAK_CT93'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef WAVE_BREAK_TG86
      MPI_master_only write(stdout,'(10x,A)') 'WAVE_BREAK_TG86'
      ie=is +14
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='WAVE_BREAK_TG86'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef WAVE_BREAK_TG86A
      MPI_master_only write(stdout,'(10x,A)') 'WAVE_BREAK_TG86A'
      ie=is +15
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='WAVE_BREAK_TG86A'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef WAVE_OFFLINE
      MPI_master_only write(stdout,'(10x,A)') 'WAVE_OFFLINE'
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='WAVE_OFFLINE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_BRY_WKB
      MPI_master_only write(stdout,'(10x,A)') 'ANA_BRY_WKB'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ANA_BRY_WKB'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LMD_SKPP2005
      MPI_master_only write(stdout,'(10x,A)') 'LMD_SKPP2005'
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='LMD_SKPP2005'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LMD_BKPP2005
      MPI_master_only write(stdout,'(10x,A)') 'LMD_BKPP2005'
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='LMD_BKPP2005'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef DIURNAL_INPUT_SFLX
      MPI_master_only write(stdout,'(10x,A)') 'DIURNAL_INPUT_SFLX'
      ie=is +17
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='DIURNAL_INPUT_SFLX'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef VAR_CHL_C
      MPI_master_only write(stdout,'(10x,A)') 'VAR_CHL_C'
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='VAR_CHL_C'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CARBON
      MPI_master_only write(stdout,'(10x,A)') 'CARBON'
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='CARBON'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef HYDROGEN_SULFIDE
      MPI_master_only write(stdout,'(10x,A)') 'HYDROGEN_SULFIDE'
      ie=is +15
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='HYDROGEN_SULFIDE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LINEAR_CONTINUATION
      MPI_master_only write(stdout,'(10x,A)') 'LINEAR_CONTINUATION'
      ie=is +18
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='LINEAR_CONTINUATION'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef NEUMANN
      MPI_master_only write(stdout,'(10x,A)') 'NEUMANN'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='NEUMANN'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ANA_BSEDIM
      MPI_master_only write(stdout,'(10x,A)') 'ANA_BSEDIM'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ANA_BSEDIM'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef Z0_BL
      MPI_master_only write(stdout,'(10x,A)') 'Z0_BL'
      ie=is + 4
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='Z0_BL'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef Z0_RIP
      MPI_master_only write(stdout,'(10x,A)') 'Z0_RIP'
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='Z0_RIP'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef Z0_BIO
      MPI_master_only write(stdout,'(10x,A)') 'Z0_BIO'
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='Z0_BIO'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef DBLEPREC
      MPI_master_only write(stdout,'(10x,A)') 'DBLEPREC'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='DBLEPREC'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef Linux
      MPI_master_only write(stdout,'(10x,A)') 'Linux'
      ie=is + 4
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='Linux'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef PGI
      MPI_master_only write(stdout,'(10x,A)') 'PGI'
      ie=is + 2
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='PGI'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef __IFC
      MPI_master_only write(stdout,'(10x,A)') '__IFC'
      ie=is + 4
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='__IFC'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef QUAD
      MPI_master_only write(stdout,'(10x,A)') 'QUAD'
      ie=is + 3
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='QUAD'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef QuadZero
      MPI_master_only write(stdout,'(10x,A)') 'QuadZero'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='QuadZero'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef THREE_GHOST_POINTS
      MPI_master_only write(stdout,'(10x,A)') 'THREE_GHOST_POINTS'
      ie=is +17
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='THREE_GHOST_POINTS'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef THREE_GHOST_POINTS_TS
      MPI_master_only write(stdout,'(10x,A)') 'THREE_GHOST_POINTS_TS'
      ie=is +20
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='THREE_GHOST_POINTS_TS'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef THREE_GHOST_POINTS_UV
      MPI_master_only write(stdout,'(10x,A)') 'THREE_GHOST_POINTS_UV'
      ie=is +20
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='THREE_GHOST_POINTS_UV'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef GLOBAL_2D_ARRAY
      MPI_master_only write(stdout,'(10x,A)') 'GLOBAL_2D_ARRAY'
      ie=is +14
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='GLOBAL_2D_ARRAY'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef GLOBAL_1D_ARRAYXI
      MPI_master_only write(stdout,'(10x,A)') 'GLOBAL_1D_ARRAYXI'
      ie=is +16
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='GLOBAL_1D_ARRAYXI'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef GLOBAL_1D_ARRAYETA
      MPI_master_only write(stdout,'(10x,A)') 'GLOBAL_1D_ARRAYETA'
      ie=is +17
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='GLOBAL_1D_ARRAYETA'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef START_2D_ARRAY
      MPI_master_only write(stdout,'(10x,A)') 'START_2D_ARRAY'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='START_2D_ARRAY'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef START_1D_ARRAYXI
      MPI_master_only write(stdout,'(10x,A)') 'START_1D_ARRAYXI'
      ie=is +15
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='START_1D_ARRAYXI'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef START_1D_ARRAYETA
      MPI_master_only write(stdout,'(10x,A)') 'START_1D_ARRAYETA'
      ie=is +16
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='START_1D_ARRAYETA'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef GLOBAL_2D_ARRAY_EXT_NBQ
      MPI_master_only write(stdout,'(10x,A)') 'GLOBAL_2D_ARRAY_EXT_NBQ'
      ie=is +22
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='GLOBAL_2D_ARRAY_EXT_NBQ'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef PRIVATE_1D_SCRATCH_ARRAY
      MPI_master_only write(stdout,'(10x,A)') 'PRIVATE_1D_SCRATCH_ARRAY'
      ie=is +23
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='PRIVATE_1D_SCRATCH_ARRAY'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef PRIVATE_2D_SCRATCH_ARRAY
      MPI_master_only write(stdout,'(10x,A)') 'PRIVATE_2D_SCRATCH_ARRAY'
      ie=is +23
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='PRIVATE_2D_SCRATCH_ARRAY'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef PRIVATE_1DXI_SCRATCH_ARRAY
      MPI_master_only write(stdout,'(10x,A)') 'PRIVATE_1DXI_SCRATCH_ARRAY'
      ie=is +25
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='PRIVATE_1DXI_SCRATCH_ARRAY'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef PRIVATE_1DETA_SCRATCH_ARRAY
      MPI_master_only write(stdout,'(10x,A)') 'PRIVATE_1DETA_SCRATCH_ARRAY'
      ie=is +26
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='PRIVATE_1DETA_SCRATCH_ARRAY'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef WESTERN_EDGE
      MPI_master_only write(stdout,'(10x,A)') 'WESTERN_EDGE'
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='WESTERN_EDGE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef EASTERN_EDGE
      MPI_master_only write(stdout,'(10x,A)') 'EASTERN_EDGE'
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='EASTERN_EDGE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SOUTHERN_EDGE
      MPI_master_only write(stdout,'(10x,A)') 'SOUTHERN_EDGE'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='SOUTHERN_EDGE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef NORTHERN_EDGE
      MPI_master_only write(stdout,'(10x,A)') 'NORTHERN_EDGE'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='NORTHERN_EDGE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef MYID
      MPI_master_only write(stdout,'(10x,A)') 'MYID'
      ie=is + 3
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='MYID'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef MPI_master_only
      MPI_master_only write(stdout,'(10x,A)') 'MPI_master_only'
      ie=is +14
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='MPI_master_only'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef mynode
      MPI_master_only write(stdout,'(10x,A)') 'mynode'
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='mynode'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ZEROTH_TILE
      MPI_master_only write(stdout,'(10x,A)') 'ZEROTH_TILE'
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ZEROTH_TILE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SINGLE_TILE_MODE
      MPI_master_only write(stdout,'(10x,A)') 'SINGLE_TILE_MODE'
      ie=is +15
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='SINGLE_TILE_MODE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LF_AM_STEP
      MPI_master_only write(stdout,'(10x,A)') 'LF_AM_STEP'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='LF_AM_STEP'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef AB_AM_STEP
      MPI_master_only write(stdout,'(10x,A)') 'AB_AM_STEP'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='AB_AM_STEP'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef FIRST_TIME_STEP
      MPI_master_only write(stdout,'(10x,A)') 'FIRST_TIME_STEP'
      ie=is +14
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='FIRST_TIME_STEP'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef FIRST_2D_STEP
      MPI_master_only write(stdout,'(10x,A)') 'FIRST_2D_STEP'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='FIRST_2D_STEP'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef NOT_LAST_2D_STEP
      MPI_master_only write(stdout,'(10x,A)') 'NOT_LAST_2D_STEP'
      ie=is +15
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='NOT_LAST_2D_STEP'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef sgi
      MPI_master_only write(stdout,'(10x,A)') 'sgi'
      ie=is + 2
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='sgi'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SGI
      MPI_master_only write(stdout,'(10x,A)') 'SGI'
      ie=is + 2
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='SGI'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CVECTOR
      MPI_master_only write(stdout,'(10x,A)') 'CVECTOR'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='CVECTOR'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CSDOACROSS
      MPI_master_only write(stdout,'(10x,A)') 'CSDOACROSS'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='CSDOACROSS'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CAND
      MPI_master_only write(stdout,'(10x,A)') 'CAND'
      ie=is + 3
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='CAND'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef ENTER_CRITICAL_REGION
      MPI_master_only write(stdout,'(10x,A)') 'ENTER_CRITICAL_REGION'
      ie=is +20
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ENTER_CRITICAL_REGION'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef EXIT_CRITICAL_REGION
      MPI_master_only write(stdout,'(10x,A)') 'EXIT_CRITICAL_REGION'
      ie=is +19
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='EXIT_CRITICAL_REGION'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CSDISTRIBUTE_RESHAPE
      MPI_master_only write(stdout,'(10x,A)') 'CSDISTRIBUTE_RESHAPE'
      ie=is +19
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='CSDISTRIBUTE_RESHAPE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef BLOCK_PATTERN
      MPI_master_only write(stdout,'(10x,A)') 'BLOCK_PATTERN'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='BLOCK_PATTERN'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef BLOCK_CLAUSE
      MPI_master_only write(stdout,'(10x,A)') 'BLOCK_CLAUSE'
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='BLOCK_CLAUSE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef cray
      MPI_master_only write(stdout,'(10x,A)') 'cray'
      ie=is + 3
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='cray'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef CRAY
      MPI_master_only write(stdout,'(10x,A)') 'CRAY'
      ie=is + 3
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='CRAY'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef SHARE
      MPI_master_only write(stdout,'(10x,A)') 'SHARE'
      ie=is + 4
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='SHARE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef LOCAL
      MPI_master_only write(stdout,'(10x,A)') 'LOCAL'
      ie=is + 4
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='LOCAL'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef PUT_GRID_INTO_RESTART
      MPI_master_only write(stdout,'(10x,A)') 'PUT_GRID_INTO_RESTART'
      ie=is +20
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='PUT_GRID_INTO_RESTART'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef PUT_GRID_INTO_HISTORY
      MPI_master_only write(stdout,'(10x,A)') 'PUT_GRID_INTO_HISTORY'
      ie=is +20
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='PUT_GRID_INTO_HISTORY'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef PUT_GRID_INTO_AVERAGES
      MPI_master_only write(stdout,'(10x,A)') 'PUT_GRID_INTO_AVERAGES'
      ie=is +21
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='PUT_GRID_INTO_AVERAGES'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef NF_FTYPE
      MPI_master_only write(stdout,'(10x,A)') 'NF_FTYPE'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='NF_FTYPE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef nf_get_att_FTYPE
      MPI_master_only write(stdout,'(10x,A)') 'nf_get_att_FTYPE'
      ie=is +15
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='nf_get_att_FTYPE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef nf_put_att_FTYPE
      MPI_master_only write(stdout,'(10x,A)') 'nf_put_att_FTYPE'
      ie=is +15
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='nf_put_att_FTYPE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef nf_get_var1_FTYPE
      MPI_master_only write(stdout,'(10x,A)') 'nf_get_var1_FTYPE'
      ie=is +16
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='nf_get_var1_FTYPE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef nf_put_var1_FTYPE
      MPI_master_only write(stdout,'(10x,A)') 'nf_put_var1_FTYPE'
      ie=is +16
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='nf_put_var1_FTYPE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef nf_get_vara_FTYPE
      MPI_master_only write(stdout,'(10x,A)') 'nf_get_vara_FTYPE'
      ie=is +16
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='nf_get_vara_FTYPE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef nf_put_vara_FTYPE
      MPI_master_only write(stdout,'(10x,A)') 'nf_put_vara_FTYPE'
      ie=is +16
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='nf_put_vara_FTYPE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef nf_put_var_FTYPE
      MPI_master_only write(stdout,'(10x,A)') 'nf_put_var_FTYPE'
      ie=is +15
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='nf_put_var_FTYPE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef FILLVAL
      MPI_master_only write(stdout,'(10x,A)') 'FILLVAL'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='FILLVAL'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef START_DATE
      MPI_master_only write(stdout,'(10x,A)') 'START_DATE'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='START_DATE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef OUT_DOUBLE
      MPI_master_only write(stdout,'(10x,A)') 'OUT_DOUBLE'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='OUT_DOUBLE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef NF_FOUT
      MPI_master_only write(stdout,'(10x,A)') 'NF_FOUT'
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='NF_FOUT'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef fast_indx_out
      MPI_master_only write(stdout,'(10x,A)') 'fast_indx_out'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='fast_indx_out'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef AGRIF_UPDATE_MIX_LOW
      MPI_master_only write(stdout,'(10x,A)') 'AGRIF_UPDATE_MIX_LOW'
      ie=is +19
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='AGRIF_UPDATE_MIX_LOW'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef AGRIF_UPDATE_MIX
      MPI_master_only write(stdout,'(10x,A)') 'AGRIF_UPDATE_MIX'
      ie=is +15
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='AGRIF_UPDATE_MIX'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef AGRIF_UPDATE_DECAL
      MPI_master_only write(stdout,'(10x,A)') 'AGRIF_UPDATE_DECAL'
      ie=is +17
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='AGRIF_UPDATE_DECAL'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef AGRIF_CONSERV_VOL
      MPI_master_only write(stdout,'(10x,A)') 'AGRIF_CONSERV_VOL'
      ie=is +16
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='AGRIF_CONSERV_VOL'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef AGRIF_CONSERV_TRA
      MPI_master_only write(stdout,'(10x,A)') 'AGRIF_CONSERV_TRA'
      ie=is +16
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='AGRIF_CONSERV_TRA'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef AGRIF_SPONGE
      MPI_master_only write(stdout,'(10x,A)') 'AGRIF_SPONGE'
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='AGRIF_SPONGE'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef AGRIF_OBC_EAST
      MPI_master_only write(stdout,'(10x,A)') 'AGRIF_OBC_EAST'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='AGRIF_OBC_EAST'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef AGRIF_OBC_WEST
      MPI_master_only write(stdout,'(10x,A)') 'AGRIF_OBC_WEST'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='AGRIF_OBC_WEST'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef AGRIF_OBC_NORTH
      MPI_master_only write(stdout,'(10x,A)') 'AGRIF_OBC_NORTH'
      ie=is +14
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='AGRIF_OBC_NORTH'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef AGRIF_OBC_SOUTH
      MPI_master_only write(stdout,'(10x,A)') 'AGRIF_OBC_SOUTH'
      ie=is +14
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='AGRIF_OBC_SOUTH'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef AGRIF_FLUX_BC
      MPI_master_only write(stdout,'(10x,A)') 'AGRIF_FLUX_BC'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='AGRIF_FLUX_BC'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef AGRIF_OBC_M2SPECIFIED
      MPI_master_only write(stdout,'(10x,A)') 'AGRIF_OBC_M2SPECIFIED'
      ie=is +20
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='AGRIF_OBC_M2SPECIFIED'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef AGRIF_OBC_M3SPECIFIED
      MPI_master_only write(stdout,'(10x,A)') 'AGRIF_OBC_M3SPECIFIED'
      ie=is +20
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='AGRIF_OBC_M3SPECIFIED'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef AGRIF_OBC_TSPECIFIED
      MPI_master_only write(stdout,'(10x,A)') 'AGRIF_OBC_TSPECIFIED'
      ie=is +19
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='AGRIF_OBC_TSPECIFIED'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef AGRIF_OBC_M3ORLANSKI
      MPI_master_only write(stdout,'(10x,A)') 'AGRIF_OBC_M3ORLANSKI'
      ie=is +19
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='AGRIF_OBC_M3ORLANSKI'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef AGRIF_OBC_TORLANSKI
      MPI_master_only write(stdout,'(10x,A)') 'AGRIF_OBC_TORLANSKI'
      ie=is +18
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='AGRIF_OBC_TORLANSKI'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
      MPI_master_only write(stdout,'(/)')
      if (iexample.eq.0) then
        MPI_master_only write(stdout,'(1x,A)')
     & 'ERROR in "cppdefs.h": no configuration is specified.'
        ierr=ierr+1
      elseif (iexample.gt.1) then
        MPI_master_only write(stdout,'(1x,A)')
     & 'ERROR: more than one configuration in "cppdefs.h".'
        ierr=ierr+1
      endif
      return
  99  MPI_master_only write(stdout,'(/1x,A,A/14x,A)')
     &  'CHECKDEFS -- ERROR: Unsufficient size of string Coptions',
     &  'in file "strings.h".', 'Increase the size it and recompile.'
      ierr=ierr+1
      return
      end
