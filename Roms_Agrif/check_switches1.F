! $Id$
!
!======================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
! 
! ROMS_AGRIF website : http://roms.mpl.ird.fr
!======================================================================
!
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
!!!!!! Number of Configuration Choices:                      1 !!!!!!
!!!!!! Total number of CPP-switches:                        46 !!!!!!

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
#ifdef VORTEX
      iexample=iexample+1
      MPI_master_only write(stdout,'(10x,A)') 'VORTEX'
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='VORTEX'
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
#ifdef M2FILTER_COSINE
      MPI_master_only write(stdout,'(10x,A)') 'M2FILTER_COSINE'
      ie=is +14
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='M2FILTER_COSINE'
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
#ifdef ANA_SMFLUX
      MPI_master_only write(stdout,'(10x,A)') 'ANA_SMFLUX'
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ANA_SMFLUX'
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
#ifdef ANA_VMIX
      MPI_master_only write(stdout,'(10x,A)') 'ANA_VMIX'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='ANA_VMIX'
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
#ifdef MIX_S_UV
      MPI_master_only write(stdout,'(10x,A)') 'MIX_S_UV'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='MIX_S_UV'
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
#ifdef MIX_S_TS
      MPI_master_only write(stdout,'(10x,A)') 'MIX_S_TS'
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='MIX_S_TS'
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
#ifdef OBC_M2FLATHER
      MPI_master_only write(stdout,'(10x,A)') 'OBC_M2FLATHER'
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='OBC_M2FLATHER'
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
#ifdef OBC_M3ORLANSKI
      MPI_master_only write(stdout,'(10x,A)') 'OBC_M3ORLANSKI'
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='OBC_M3ORLANSKI'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef AGRIF_STORE_BAROT_CHILD
      MPI_master_only write(stdout,'(10x,A)') 'AGRIF_STORE_BAROT_CHILD'
      ie=is +22
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='AGRIF_STORE_BAROT_CHILD'
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
#ifdef AGRIF_POLY_DUAVG
      MPI_master_only write(stdout,'(10x,A)') 'AGRIF_POLY_DUAVG'
      ie=is +15
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='AGRIF_POLY_DUAVG'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef AGRIF_LOCAL_VOLCONS
      MPI_master_only write(stdout,'(10x,A)') 'AGRIF_LOCAL_VOLCONS'
      ie=is +18
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='AGRIF_LOCAL_VOLCONS'
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
