! $Id$
#include "cppdefs.h"

      subroutine setup_kwds (ierr)

!!!!!! WARNING: THIS IS A MACHINE GENERATED CODE, DO NOT EDIT! !!!!!!
!!!!!! This file needs to be updated only if new keywords were !!!!!!
!!!!!! introduced into "read_inp.F". To create or refresh this !!!!!!
!!!!!! file use compile and execute "checkkwds.F" as an        !!!!!!
!!!!!! independent program, or use commands "make checkkwds"   !!!!!!
!!!!!! or "make depend".                                       !!!!!!

      implicit none
      integer ierr, is,ie
#include "param.h"
#include "strings.h"
#ifdef MPI
# include "scalars.h"
#endif
      do is=1,max_opt_size
        Coptions(is:is)=' '
      enddo
      is=1
#ifdef FLOATS
#endif
#ifdef STATIONS
#endif
#ifdef PSOURCE
#endif
#ifdef MPI
#endif
#ifdef SOLVE3D
#endif
#ifdef AGRIF
#endif
#if defined SOLITON
#elif defined OVERFLOW
#elif defined SEAMOUNT
#elif defined UPWELLING
#elif defined REGIONAL
#elif defined BASIN
#elif defined INNERSHELF
#elif defined SHELFRONT
#elif defined CANYON_A || defined CANYON_B
#elif defined GRAV_ADJ
#else
#endif
#ifdef MPI
#else
#endif
#ifdef AGRIF
#endif
#ifdef AVERAGES
#endif
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='title'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='time_stepping'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#ifdef SOLVE3D
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='S-coord'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='initial'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#ifdef ANA_INITIAL
#endif
#if defined MPI && defined PARALLEL_FILES
#endif
#ifdef ANA_INITIAL
#endif
#ifndef ANA_GRID
      ie=is + 4
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='grid'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# if defined MPI && defined PARALLEL_FILES
# endif
#endif
#if !defined ANA_SMFLUX  || defined SOLVE3D && (\
        !defined ANA_STFLUX  || !defined ANA_BTFLUX  \
      ||(defined BBL         && !defined ANA_BSEDIM && !defined SEDIMENT) \
      ||(defined BBL         && !defined ANA_WWAVE)  \
      ||(defined QCORRECTION && !defined ANA_SST)    \
      ||(defined SALINITY    && !defined ANA_SSFLUX) \
      ||(defined LMD_SKPP    && !defined ANA_SRFLUX) \
      ||(defined SALINITY    && defined QCORRECTION && \
         defined SFLX_CORR   && !defined ANA_SSS) \
      || defined BULK_FLUX)
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='forcing'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# if defined MPI && defined PARALLEL_FILES
# endif
# if defined BULK_FLUX
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='bulk_forcing'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#  if defined MPI && defined PARALLEL_FILES
#  endif
# endif
#endif
#if (defined TCLIMATOLOGY  && !defined ANA_TCLIMA) || \
      (defined ZCLIMATOLOGY  && !defined ANA_SSH) || \
      (defined M2CLIMATOLOGY && !defined ANA_M2CLIMA) || \
      (defined M3CLIMATOLOGY && !defined ANA_M3CLIMA)
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='climatology'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# ifdef AGRIF
# endif
# if defined MPI && defined PARALLEL_FILES
# endif
# ifdef AGRIF
# endif
#endif
#if defined T_FRC_BRY || defined M2_FRC_BRY || \
      defined M3_FRC_BRY || defined Z_FRC_BRY
# ifndef ANA_BRY
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='boundary'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#  ifdef AGRIF
#  endif
#  if defined MPI && defined PARALLEL_FILES
#  endif
#  ifdef AGRIF
#  endif
# endif
#endif
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='restart'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# if defined MPI && defined PARALLEL_FILES
# endif
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='history'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# if defined MPI && defined PARALLEL_FILES
# endif
#ifdef AVERAGES
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='averages'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# if defined MPI && defined PARALLEL_FILES
# endif
#endif
#if defined DIAGNOSTICS_TS
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='diagnostics'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# if defined MPI && defined PARALLEL_FILES
# endif
# ifdef AVERAGES
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='diag_avg'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# if defined MPI && defined PARALLEL_FILES
# endif
# endif
#endif /* DIAGOSTICS_TS */
#if defined DIAGNOSTICS_UV
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='diagnosticsM'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# if defined MPI && defined PARALLEL_FILES
# endif
# ifdef AVERAGES
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='diagM_avg'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# if defined MPI && defined PARALLEL_FILES
# endif
# endif
#endif /*DIAGNOSTICS_UV */
#ifdef DIAGNOSTICS_BIO
      ie=is +15
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='diagnostics_bio'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# if defined MPI && defined PARALLEL_FILES
# endif
# ifdef AVERAGES
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='diagbio_avg'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# if defined MPI && defined PARALLEL_FILES
# endif
# endif
#endif /* DIAGOSTICS_BIO */
#ifdef FLOATS
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='floats'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#ifdef AGRIF
#endif
# if defined MPI && defined PARALLEL_FILES
# endif
#ifdef AGRIF
#endif
#endif /* FLOATS */
#ifdef STATIONS
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='stations'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# ifdef AGRIF
# endif
# if defined MPI && defined PARALLEL_FILES
# endif
# ifdef AGRIF
# endif
#endif /* STATIONS */
#ifdef ASSIMILATION
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='assimilation'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
      ie=is +22
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='primary_history_fields'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#ifdef SOLVE3D
#endif
#ifdef SOLVE3D
#endif
#ifdef SOLVE3D
      ie=is +24
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='auxiliary_history_fields'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# ifdef SALINITY
# endif
# ifdef LMD_SKPP
# endif
# ifdef LMD_BKPP
# endif
# ifdef BIOLOGY
#  ifdef BIO_NChlPZD
#   ifdef OXYGEN
#   endif
#  elif defined BIO_N2P2Z2D2 && defined VAR_CHL_C
#  endif
# endif
# ifdef SALINITY
# endif
# ifdef LMD_SKPP
# endif
# ifdef LMD_BKPP
# endif
# ifdef BIOLOGY
#  ifdef BIO_NChlPZD
#   ifdef OXYGEN
#   endif
#  elif defined BIO_N2P2Z2D2 && defined VAR_CHL_C
#  endif
# endif
#  if defined UV_VIS2 && defined SMAGORINSKY
#  endif
# ifdef SALINITY
# endif
# ifdef LMD_SKPP
# endif
# ifdef LMD_BKPP
# endif
# ifdef BIOLOGY
#  ifdef BIO_NChlPZD
#   ifdef OXYGEN
#   endif
#  elif defined BIO_N2P2Z2D2 && defined VAR_CHL_C
#  endif
# endif
#  if defined UV_VIS2 && defined SMAGORINSKY
#  endif
#endif /* SOLVE3D */
#ifdef AVERAGES
      ie=is +16
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='primary_averages'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# ifdef SOLVE3D
# endif
# ifdef SOLVE3D
# endif
# ifdef SOLVE3D
      ie=is +18
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='auxiliary_averages'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#  ifdef SALINITY
#  endif
#  ifdef LMD_SKPP
#  endif
#  ifdef LMD_BKPP
#  endif
#  ifdef BIOLOGY
#   ifdef BIO_NChlPZD
#    ifdef OXYGEN
#    endif
#   elif defined BIO_N2P2Z2D2 && defined VAR_CHL_C
#   endif
#  endif
#  ifdef SALINITY
#  endif
#  ifdef LMD_SKPP
#  endif
#  ifdef LMD_BKPP
#  endif
#  ifdef BIOLOGY
#   ifdef BIO_NChlPZD
#    ifdef OXYGEN
#    endif
#   elif defined BIO_N2P2Z2D2 && defined VAR_CHL_C
#   endif
#  endif
#  if defined UV_VIS2 && defined SMAGORINSKY
#  endif
#  ifdef SALINITY
#  endif
#  ifdef LMD_SKPP
#  endif
#  ifdef LMD_BKPP
#  endif
#  ifdef BIOLOGY
#   ifdef BIO_NChlPZD
#    ifdef OXYGEN
#    endif
#   elif defined BIO_N2P2Z2D2 && defined VAR_CHL_C
#   endif
#  endif
#  if defined UV_VIS2 && defined SMAGORINSKY
#  endif
# endif /* SOLVE3D */
#endif /* AVERAGES */
#ifdef FLOATS
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='float_fields'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#ifdef AGRIF
#endif
#ifdef AGRIF
#endif
#endif /* FLOATS */
#if defined DIAGNOSTICS_TS
# ifdef AVERAGES
# endif
#endif /*DIAGNOSTICS_TS */
#if defined DIAGNOSTICS_UV
# ifdef AVERAGES
# endif
#endif /*DIAGNOSTICS_UV */
#ifdef DIAGNOSTICS_BIO
# ifdef AVERAGES
# endif  /* AVERAGES */
#endif /* DIAGNOSTICS_BIO */
#ifdef STATIONS
      ie=is +14
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='station_fields'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# ifdef AGRIF
# endif
# ifdef AGRIF
# endif
#endif /* STATIONS */
      ie=is + 4
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='rho0'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#if defined UV_VIS2 || defined UV_VIS4
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='lateral_visc'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#ifdef UV_VIS2
#endif
#ifdef UV_VIS4
#endif
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='bottom_drag'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='gamma2'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#ifdef SOLVE3D
# ifdef TS_DIF2
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='tracer_diff2'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# endif
# ifdef TS_DIF4
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='tracer_diff4'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# endif
# if !defined LMD_MIXING && !defined BVF_MIXING\
      && !defined MY2_MIXING && !defined MY25_MIXING
      ie=is +15
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='vertical_mixing'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# endif
# ifdef MY25_MIXING
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='MY_bak_mixing'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#  ifdef Q_DIF2
#  endif
#  ifdef Q_DIF4
#  endif
# endif
# ifdef BODYFORCE
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='bodyforce'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# endif
#endif
#if  defined SPONGE || \
     defined TNUDGING   || defined M2NUDGING  || \
     defined M3NUDGING  || defined ZNUDGING
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='sponge'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
#if  defined T_FRC_BRY  || defined M2_FRC_BRY || \
     defined M3_FRC_BRY || defined Z_FRC_BRY  || \
     defined TNUDGING   || defined M2NUDGING  || \
     defined M3NUDGING  || defined ZNUDGING
      ie=is + 8
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='nudg_cof'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# if  defined AGRIF && !defined AGRIF_OBC_M2ORLANSKI && \
     !defined AGRIF_OBC_M3ORLANSKI && !defined AGRIF_OBC_TORLANSKI
# endif
# if defined AGRIF && !defined AGRIF_OBC_M2ORLANSKI && \
     !defined AGRIF_OBC_M3ORLANSKI && !defined AGRIF_OBC_TORLANSKI
# endif
#endif
#ifdef SOLVE3D
# ifndef NONLIN_EOS
      ie=is +11
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='lin_EOS_cff'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# endif
# ifdef SEDIMENT
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='sediments'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
      ie=is +23
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='sediment_history_fields'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# endif /* SEDIMENT */
#endif
#ifdef BBL
      ie=is +18
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='bbl_history_fields'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif /* BBL */
#ifdef ANA_PSOURCE
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
#ifdef PSOURCE_VAR
      Coptions(is:ie)='psource_var'
#else
      Coptions(is:ie)='psource'
#endif /* PSOURCE_VAR */
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif /* ANA_SOURCES */
#ifdef MPI
#endif
      return
  99  MPI_master_only write(stdout,'(/1x,A,A/14x,A)')
     &  'SETUP_KWDS ERROR: Unsufficient size of string Coptions',
     &  'in file "strings.h".', 'Increase the size it and recompile.'
      ierr=ierr+1
      return
      end
