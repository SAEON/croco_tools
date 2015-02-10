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
#if defined PSOURCE || defined PSOURCE_NCFILE
#endif
#ifdef WKB_WWAVE
#endif
#ifdef ONLINE
#endif
#ifdef MPI
#endif
#ifdef NBQ
#endif
#ifdef SOLVE3D
#endif
#if defined BASIN
#elif defined CANYON_A
#elif defined CANYON_B
#elif defined EQUATOR
#elif defined GRAV_ADJ
#elif defined INNERSHELF
#elif defined INTERNAL
#elif defined OVERFLOW
#elif defined RIVER
#elif defined SEAMOUNT
#elif defined SHELFRONT
#elif defined SOLITON
#elif defined UPWELLING
#elif defined VORTEX
#elif defined JET
#elif defined SHOREFACE
#elif defined RIP
#elif defined THACKER
#elif defined REGIONAL
#else
#endif
#ifdef MPI
#else
#endif
#ifdef AGRIF
#ifdef AGRIF_ADAPTIVE
#else
#endif
#endif
#ifdef AVERAGES
#endif
#ifdef DIAGNOSTICS_TS
# ifdef AVERAGES
# endif
# ifdef DIAGNOSTICS_TS_MLD
#  ifdef AVERAGES
#  endif
# endif
#endif
#ifdef DIAGNOSTICS_UV
#ifdef AVERAGES
#endif
#endif
#ifdef DIAGNOSTICS_BIO
#ifdef AVERAGES
#endif
#endif
      ie=is + 5
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='title'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#ifdef START_DATE
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='start_date'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
      ie=is +13
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='time_stepping'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#ifdef NBQ
      ie=is +17
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='time_stepping_nbq'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif
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
#if       (!defined ANA_SMFLUX) \
        || (defined TIDES) \
        || (defined MRL_WCI     && !defined ANA_WWAVE   && !defined WKB_WWAVE) \
        || (defined SOLVE3D     && (\
                                  (!defined ANA_STFLUX  && !defined BULK_FLUX) \
        ||                         !defined ANA_BTFLUX   \
        || (defined BBL         && !defined ANA_BSEDIM  && !defined SEDIMENT) \
        || (defined BBL         && !defined ANA_WWAVE   && !defined WKB_WWAVE) \
        || (defined QCORRECTION && !defined ANA_SST)   \
        || (defined SALINITY    && !defined ANA_SSFLUX  && \
                                   !defined BULK_FLUX   && !defined BULK_EP) \
        || (defined SALINITY    &&  defined QCORRECTION && \
                                    defined SFLX_CORR   && !defined ANA_SSS) \
        || (defined LMD_SKPP    && !defined ANA_SRFLUX  && !defined BULK_FLUX) \
        || (defined BULK_FLUX   && \
                                  (!defined BULK_SMFLUX || !defined BULK_EP)) \
                                   )\
           )\
        && !defined OA_COUPLING
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='forcing'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# if defined MPI && defined PARALLEL_FILES
# endif
#endif
#if defined BIOLOGY && defined PISCES
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='biology'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# if defined MPI && defined PARALLEL_FILES
# endif
#endif
#if defined BULK_FLUX
# ifndef ONLINE /* ONLINE FORCING */
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='bulk_forcing'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#  if defined MPI && defined PARALLEL_FILES
#  endif
# endif  /* ONLINE FORCING */
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
#if !defined ANA_BRY && defined FRC_BRY
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
#endif
#if defined WKB_WWAVE && !defined ANA_BRY_WKB
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='wkb_boundary'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#  ifdef AGRIF
#  endif
#  if defined MPI && defined PARALLEL_FILES
#  endif
#  ifdef AGRIF
#  endif
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
#endif /* DIAGNOSTICS_BIO */
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
#endif
#if !defined SOLVE3D && defined RIP
      ie=is +24
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='auxiliary_history_fields'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#elif defined SOLVE3D
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
# ifdef SALINITY
# endif
# ifdef BULK_FLUX
# endif
# if defined BIOLOGY && !defined PISCES
#  ifdef BIO_NChlPZD
#   ifdef OXYGEN
#   endif
#  elif defined BIO_BioEBUS
#  endif
# endif
# ifdef SALINITY
# endif
# ifdef LMD_SKPP
# endif
# ifdef LMD_BKPP
# endif
# ifdef SALINITY
# endif
# if defined BULK_FLUX
# endif
# if defined BIOLOGY && !defined PISCES
#  ifdef BIO_NChlPZD
#   ifdef OXYGEN
#   endif
#  elif defined BIO_BioEBUS
#  endif
# endif
# ifdef VIS_COEF_3D
# endif
# ifdef DIF_COEF_3D
# endif
# ifdef SALINITY
# endif
# ifdef LMD_SKPP
# endif
# ifdef LMD_BKPP
# endif
# ifdef BULK_FLUX
# endif
# if defined BIOLOGY && !defined PISCES
#  ifdef BIO_NChlPZD
#   ifdef OXYGEN
#   endif
#  elif defined BIO_BioEBUS
#  endif
# endif /* BIOLOGY */
# ifdef SALINITY
# endif
# ifdef VIS_COEF_3D
# endif
# ifdef DIF_COEF_3D
# endif
# ifdef GLS_MIXING
      ie=is +18
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='gls_history_fields'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# endif
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
#  ifdef SALINITY
#  endif
#  ifdef BULK_FLUX
#  endif
#  if defined BIOLOGY && !defined PISCES
#   ifdef BIO_NChlPZD
#    ifdef OXYGEN
#    endif
#   elif defined BIO_BioEBUS
#   endif
#  endif /* BIOLOGY */
#  ifdef SALINITY
#  endif
#  ifdef LMD_SKPP
#  endif
#  ifdef LMD_BKPP
#  endif
#  ifdef SALINITY
#  endif
#  ifdef BULK_FLUX
#  endif
#  if defined BIOLOGY && !defined PISCES
#   ifdef BIO_NChlPZD
#    ifdef OXYGEN
#    endif
#   elif defined BIO_BioEBUS
#   endif
#  endif  /* BIOLOGY */
#  ifdef VIS_COEF_3D
#  endif
#  ifdef DIF_COEF_3D
#  endif
#  ifdef SALINITY
#  endif
#  ifdef LMD_SKPP
#  endif
#  ifdef LMD_BKPP
#  endif
#  ifdef BULK_FLUX
#  endif
#  if defined BIOLOGY && !defined PISCES
#   ifdef BIO_NChlPZD
#    ifdef OXYGEN
#    endif
#   elif defined BIO_BioEBUS
#   endif
#  endif   /* BIOLOGY */
#  ifdef SALINITY
#  endif
#  ifdef VIS_COEF_3D
#  endif
#  ifdef DIF_COEF_3D
#  endif
#  ifdef GLS_MIXING
      ie=is +12
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='gls_averages'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
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
      ie=is +21
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='diag3D_history_fields'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# ifdef DIAGNOSTICS_TS_MLD
      ie=is +21
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='diag2D_history_fields'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# endif  /* DIAGNOSTICS_TS_MLD */
# ifdef AVERAGES
      ie=is +21
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='diag3D_average_fields'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#  ifdef DIAGNOSTICS_TS_MLD
      ie=is +21
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='diag2D_average_fields'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#  endif  /* DIAGNOSTICS_TS_MLD */
# endif  /* AVERAGES */
#endif  /* DIAGNOSTICS_TS */
#if defined DIAGNOSTICS_UV
      ie=is +20
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='diagM_history_fields'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# ifdef AVERAGES
      ie=is +20
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='diagM_average_fields'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# endif /* AVERAGES  */
#endif /*DIAGNOSTICS_UV */
#ifdef DIAGNOSTICS_BIO
      ie=is +26
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='diagbioFlux_history_fields'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
      ie=is +27
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='diagbioVSink_history_fields'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
      ie=is +28
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='diagbioGasExc_history_fields'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# ifdef AVERAGES
      ie=is +26
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='diagbioFlux_average_fields'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
      ie=is +27
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='diagbioVSink_average_fields'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
      ie=is +28
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='diagbioGasExc_average_fields'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
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
#if defined UV_VIS2 || defined SPONGE_VIS2
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
# if defined TS_DIF2 || defined SPONGE_DIF2
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
# if !defined LMD_MIXING && !defined BVF_MIXING && !defined GLS_MIXING
      ie=is +15
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='vertical_mixing'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# endif
# ifdef BODYFORCE
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='bodyforce'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# endif
#endif
#if  (defined SPONGE && !defined SPONGE_GRID)
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='sponge'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#elif  (defined SPONGE && defined SPONGE_GRID)
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
#  ifdef AVERAGES
#  endif
# endif /* SEDIMENT */
#endif
#ifdef BBL
      ie=is +18
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='bbl_history_fields'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif /* BBL */
#ifdef MRL_WCI
      ie=is +18
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='wci_history_fields'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# ifdef SOLVE3D
# endif
# ifdef SOLVE3D
# endif
# ifdef AVERAGES
# ifdef SOLVE3D
# endif
# endif /* AVERAGES */
#endif /* MRL_WCI */
#ifdef WKB_WWAVE
      ie=is +18
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='wkb_history_fields'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# ifdef AVERAGES
#  ifndef WAVE_ROLLER
#  else
#  endif
# endif
      ie=is + 9
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='wkb_wwave'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#  ifdef WAVE_BREAK_TG86
#  elif defined WAVE_BREAK_TG86A
#  elif defined WAVE_BREAK_CT93
#  else
#  endif
# ifdef WAVE_ROLLER
      ie=is +10
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='wkb_roller'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
# endif
#endif  /* WKB_WWAVE */
#ifdef ANA_PSOURCE
# ifdef PSOURCE_NCFILE
      ie=is +14
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='psource_ncfile'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#  ifdef PSOURCE_NCFILE_TS
#  else
#  endif /* PSOURCE_NCFILE_TS  */
#  ifdef MPI
#  endif
#  ifndef PSOURCE_NCFILE_TS
#  endif
# else
      ie=is + 7
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='psource'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#  ifdef MPI
#  endif
# endif   /*  define PSOURCE_NCFILE  */
#endif /* ANA_PSOURCE */
#ifdef ONLINE   /* JG Online */
      ie=is + 6
      if (ie.ge.max_opt_size) goto 99
      Coptions(is:ie)='online'
      Coptions(ie+1:ie+1)=' '
      is=ie+2
#endif /* ONLINE */
#ifdef MPI
#endif
      return
  99  MPI_master_only write(stdout,'(/1x,A,A/14x,A)')
     &  'SETUP_KWDS ERROR: Unsufficient size of string Coptions',
     &  'in file "strings.h".', 'Increase the size it and recompile.'
      ierr=ierr+1
      return
      end
