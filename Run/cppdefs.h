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
/*
   This is "cppdefs.h": MODEL CONFIGURATION FILE
   ==== == ============ ===== ============= ====
*/
#undef  BASIN           /* Basin Example */
#undef  CANYON_A        /* Canyon_A Example */
#undef  CANYON_B        /* Canyon_B Example */
#undef  EQUATOR         /* Equator Example  */
#undef  GRAV_ADJ        /* Graviational Adjustment Example */
#undef  INNERSHELF      /* Inner Shelf Example */
#undef  RIVER           /* River run-off Example */
#undef  OVERFLOW        /* Graviational/Overflow Example */
#undef  SEAMOUNT        /* Seamount Example */
#undef  SHELFRONT       /* Shelf Front Example */
#undef  SOLITON         /* Equatorial Rossby Wave Example */
#undef  UPWELLING       /* Upwelling Example */
#undef  VORTEX          /* Baroclinic Vortex Example */
#undef  INTERNAL        /* Internal Tide Example */
#undef  JET             /* Baroclinic Jet Example */
#define REGIONAL        /* REGIONAL Applications */


#if defined REGIONAL
/*
!====================================================================
!               REGIONAL (realistic) Configurations
!==================================================================== 
!
!----------------------
! BASIC OPTIONS
!----------------------
!
*/
                      /* Configuration Name */
# define BENGUELA_LR
                      /* Parallelization */
# undef  OPENMP
# undef  MPI
                      /* Nesting */
# undef  AGRIF
# undef  AGRIF_2WAY
                      /* Open Boundary Conditions */
# undef  TIDES
# define OBC_EAST
# define OBC_WEST
# define OBC_NORTH
# define OBC_SOUTH
                      /* Applications */
# undef  BIOLOGY
# undef  FLOATS
# undef  STATIONS
# undef  PASSIVE_TRACER
# undef  SEDIMENT
# undef  BBL
/*!
!-------------------------------------------------
! PRE-SELECTED OPTIONS
!
! ADVANCED OPTIONS ARE IN SET_GLOBAL_DEFINITIONS.H
!-------------------------------------------------
*/
                      /* Parallelization */
# ifdef MPI
#  undef  PARALLEL_FILES
# endif
# undef  AUTOTILING
# undef  ETALON_CHECK
                      /* Grid configuration */
# define CURVGRID
# define SPHERICAL
# define MASKING
                      /* Model dynamics */
# define SOLVE3D
# define UV_COR
# define UV_ADV
# ifdef TIDES
#  define SSH_TIDES
#  define UV_TIDES
#  define TIDERAMP
# endif
                      /* Lateral Explicit Momentum Mixing */
# undef  UV_VIS2
# ifdef UV_VIS2
#  define UV_MIX_S
#  define UV_VIS_SMAGO
# endif
                      /* Lateral Tracer Advection (default UP3) */
# define TS_HADV_RSUP3
# undef  TS_HADV_UP5
# undef  TS_HADV_C4
# undef  TS_HADV_WENO5
                      /* Lateral Explicit Tracer Mixing */
# ifdef TS_HADV_C4
#  define  TS_DIF2
#  undef   TS_DIF4
#  define  TS_DIF_SMAGO
#  define  TS_MIX_ISO
# endif
                      /* Sponge layers for UV and TS */
# define SPONGE
# define SPONGE_GRID
                      /* Vertical Mixing */
# undef  BODYFORCE
# undef  BVF_MIXING
# define LMD_MIXING
# ifdef LMD_MIXING
#  define LMD_SKPP
#  define LMD_SKPP2005
#  define LMD_BKPP
#  define LMD_RIMIX
#  define LMD_CONVEC
#  undef  LMD_DDMIX
#  define LMD_NONLOCAL
# endif
                      /* Equation of State */
# define SALINITY
# define NONLIN_EOS
# define SPLIT_EOS
                      /* Surface Forcing */
# undef  BULK_FLUX
# ifdef BULK_FLUX
#  define BULK_FAIRALL
#  define BULK_LW
#  define BULK_EP
#  define BULK_SMFLUX
#  undef  SST_SKIN
#  undef  ANA_DIURNAL_SW
#  undef  ONLINE
# else
#  define QCORRECTION
#  define SFLX_CORR
#  define ANA_DIURNAL_SW
# endif
                      /* Lateral Forcing */
# define CLIMATOLOGY
# ifdef CLIMATOLOGY
#  define ZCLIMATOLOGY
#  define M2CLIMATOLOGY
#  define M3CLIMATOLOGY
#  define TCLIMATOLOGY

#  define ZNUDGING
#  define M2NUDGING
#  define M3NUDGING
#  define TNUDGING
#  undef  ROBUST_DIAG
# endif

# undef  FRC_BRY
# ifdef FRC_BRY
#  define Z_FRC_BRY
#  define M2_FRC_BRY
#  define M3_FRC_BRY
#  define T_FRC_BRY
# endif
                      /* Bottom Forcing */
# define ANA_BSFLUX
# define ANA_BTFLUX
                      /* Point Sources - Rivers */
# undef  PSOURCE
# undef  ANA_PSOURCE
                      /* Open Boundary Conditions */
# ifdef TIDES
#  define OBC_M2FLATHER
# else
#  undef  OBC_M2SPECIFIED
#  undef  OBC_M2FLATHER
#  define OBC_M2CHARACT
#  undef  OBC_M2ORLANSKI
#  ifdef  OBC_M2ORLANSKI
#   define OBC_VOLCONS
#  endif
# endif
# define OBC_M3ORLANSKI
# define OBC_TORLANSKI
# undef  OBC_M3SPECIFIED
# undef  OBC_TSPECIFIED
                      /* Input/Output & Diagnostics */
# define AVERAGES
# define AVERAGES_K
# undef  DIAGNOSTICS_TS
# undef  DIAGNOSTICS_UV
# ifdef DIAGNOSTICS_TS
#  undef DIAGNOSTICS_TS_ADV
#  undef DIAGNOSTICS_TS_MLD
# endif
/*
!           Applications:
!---------------------------------
! Biology, floats, Stations, 
! Passive tracer, Sediments, BBL
!---------------------------------

    Quasi-monotone lateral advection scheme (WENO5)
    for passive/biology/sediment tracers 
*/
# if defined PASSIVE_TRACER || defined BIOLOGY || defined SEDIMENT
#  undef BIO_HADV_WENO5
# endif

                      /*   Choice of Biology models   */
# ifdef BIOLOGY
#  undef  PISCES
#  define BIO_NChlPZD
#  undef  BIO_N2ChlPZD2  
                      /*   Biology options    */
#  ifdef PISCES
#   define key_trc_pisces
#   define key_passivetrc
#   undef  DIAGNOSTICS_BIO
#   ifdef DIAGNOSTICS_BIO
#     define key_trc_diaadd
#     define key_trc_dia3d
#   endif
#  endif
#  ifdef BIO_NChlPZD
#   undef  OXYGEN
#  endif
#  ifdef BIO_NChlPZD
#   define DIAGNOSTICS_BIO
#  endif
#  ifdef BIO_N2P2Z2D2
#   undef  VAR_CHL_C
#  endif
# endif
                      /*   Lagrangian floats model    */
# ifdef FLOATS
#  undef  FLOATS_GLOBAL_ATTRIBUTES
#  undef  IBM
#  undef  RANDOM_WALK
#  ifdef RANDOM_WALK
#   define DIEL_MIGRATION
#   define RANDOM_VERTICAL
#   define RANDOM_HORIZONTAL
#  endif
# endif
                      /*   Stations recording    */
# ifdef STATIONS
#  define ALL_SIGMA
# endif
                      /*   Sediment dynamics model     */
# ifdef SEDIMENT
#  define ANA_SEDIMENT
#  undef  BED_ARMOR
#  undef  ANA_SPFLUX
#  undef  ANA_BPFLUX
#  define LINEAR_CONTINUATION
#  undef  NEUMANN
# endif
                      /*   Bottom Boundary Layer model     */
# ifdef BBL
#  define ANA_WWAVE
#  ifdef SEDIMENT
#   undef  ANA_BSEDIM
#  else
#   define ANA_BSEDIM
#  endif
#  undef  Z0_BL
#  ifdef Z0_BL
#   define Z0_RIP
#  endif
#  undef  Z0_BIO
# endif
/*
!
!==========================================================
!              IDEALIZED CONFIGURATIONS
!==========================================================
!
*/
#elif defined BASIN
/*
!                       Basin Example
!                       ===== =======
*/
# define ETALON_CHECK
# undef OPENMP
# undef MPI
# define UV_ADV
# define UV_COR
# define UV_VIS2
# define UV_MIX_S
# define SOLVE3D
# define TS_DIF2
# define TS_MIX_S
# define BODYFORCE
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_BTFLUX

#elif defined CANYON_A
/*
!                       First Canyon Example
!                       ===== ====== =======
*/
# define ETALON_CHECK
# undef OPENMP
# undef MPI
# define UV_ADV
# define UV_COR
# define SOLVE3D
# define EW_PERIODIC
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_BTFLUX

#elif defined CANYON_B
/*
!                       Second Canyon Example
!                       ====== ====== =======
*/
# define ETALON_CHECK
# undef OPENMP
# undef MPI
# define UV_ADV
# define UV_COR
# define SOLVE3D
# define EW_PERIODIC
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_BTFLUX
# define ANA_VMIX

#elif defined EQUATOR
/*
!                       Equator Example
!                       ======= =======
! Boccaletti, G., R.C. Pacanowski, G.H. Philander and A.V. Fedorov, 2004,
! The Thermal Structure of the Upper Ocean, J.Phys.Oceanogr., 34, 888-902.
*/
# define ETALON_CHECK
# undef OPENMP
# undef MPI
# define UV_ADV
# define UV_COR
# define UV_VIS2
# define UV_MIX_S
# define SOLVE3D
# define SALINITY
# define TS_DIF2
# define TS_MIX_S
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_SRFLUX
# define ANA_SSFLUX
# define ANA_BTFLUX
# define ANA_BSFLUX
# define QCORRECTION
# define ANA_SST
# define LMD_SKPP /* problem with MPI in Xi direction */
# define LMD_MIXING
# define LMD_RIMIX
# define LMD_CONVEC

#elif defined GRAV_ADJ
/*
!                       Gravitational Adjustment Example
!                       ============= ========== =======
*/
# define ETALON_CHECK
# undef OPENMP
# undef MPI
# define SOLVE3D
# define UV_ADV
# define UV_VIS2
# define UV_MIX_S
# undef  TS_HADV_WENO5
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_BTFLUX

#elif defined INNERSHELF
/*
!                       Inner Shelf Example
!                       ===== ===== =======
*/
# define ETALON_CHECK
# undef  OPENMP
# undef  MPI
# define INNERSHELF_EKMAN
# define INNERSHELF_APG
# define SOLVE3D
# define UV_COR
# define ANA_GRID
# define ANA_INITIAL
# define AVERAGES
# define ANA_SSFLUX
# define ANA_SRFLUX
# define ANA_STFLUX
# define ANA_BSFLUX
# define ANA_BTFLUX
# define ANA_SMFLUX
# define NS_PERIODIC
# define OBC_WEST
# define OBC_M2ORLANSKI
# define OBC_VOLCONS
# define OBC_TORLANSKI
# define OBC_M3ORLANSKI
# define SPONGE
# ifndef INNERSHELF_EKMAN
#  define UV_ADV
#  define SALINITY
#  define NONLIN_EOS
#  define SPLIT_EOS
#  define LMD_MIXING
#  define LMD_SKPP
#  define LMD_BKPP
#  define LMD_RIMIX
#  define LMD_CONVEC
# endif

#elif defined INTERNAL
/*
!                       Internal Tide Example
!                       ======== ==== =======
! Di Lorenzo, E, W.R. Young and S.L. Smith, 2006, Numerical and anlytical estimates of M2
! tidal conversion at steep oceanic ridges, J. Phys. Oceanogr., 36, 1072-1084.  
*/
# undef  ETALON_CHECK
# undef  OPENMP
# undef  MPI
# define SOLVE3D
# define UV_COR
# define UV_ADV
# define BODYTIDE
# define ANA_GRID
# undef  INTERNALSHELF
# define ANA_INITIAL
# define ANA_BTFLUX
# define ANA_SMFLUX
# define ANA_SRFLUX
# define ANA_STFLUX
# define ANA_VMIX

# define EW_PERIODIC
# define NS_PERIODIC

# undef  UV_VIS2
# undef  UV_MIX_GEO
# undef  TS_DIF2
# undef  TS_MIX_GEO
# undef  SPONGE
# undef  ANA_SSH
# undef  ANA_M2CLIMA
# undef  ANA_M3CLIMA
# undef  ANA_TCLIMA
# undef  ZCLIMATOLOGY
# undef  M2CLIMATOLOGY
# undef  M3CLIMATOLOGY
# undef  TCLIMATOLOGY
# undef  ZNUDGING
# undef  M2NUDGING
# undef  M3NUDGING
# undef  TNUDGING
# undef  OBC_EAST
# undef  OBC_WEST
# undef  OBC_M2CHARACT
# undef  OBC_M2FLATHER
# undef  OBC_TORLANSKI
# undef  OBC_M3ORLANSKI

#elif defined RIVER
/*
!                       River run-off test problem
!                       ==========================
*/
# define ETALON_CHECK
# undef OPENMP
# undef MPI
# define SOLVE3D
# define UV_ADV
# define UV_COR
# define M2FILTER_FLAT
# define NONLIN_EOS
# define SPLIT_EOS
# define SALINITY
# define ANA_GRID
# define MASKING
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_SSFLUX
# define ANA_SRFLUX
# define ANA_BTFLUX
# define ANA_BSFLUX
# define LMD_MIXING
# define LMD_SKPP
# define LMD_BKPP
# define LMD_RIMIX
# define LMD_CONVEC
# define PSOURCE
# define ANA_PSOURCE
# define NS_PERIODIC
# define FLOATS
# ifdef FLOATS
#   define RANDOM_WALK
#   ifdef RANDOM_WALK
#      define DIEL_MIGRATION
#      define RANDOM_VERTICAL
#      define RANDOM_HORIZONTAL
#   endif
# endif

#elif defined SEAMOUNT
/*
!                       Seamount Example
!                       ======== =======
*/
# define ETALON_CHECK
# undef OPENMP
# undef MPI
# define UV_ADV
# define UV_COR
# define SOLVE3D
# define SALINITY
# define NONLIN_EOS
# define SPLIT_EOS
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_SSFLUX
# define ANA_SRFLUX
# define ANA_BTFLUX
# define ANA_BSFLUX

# elif defined SHELFRONT
/*
!                       Shelf Front Example
!                       ===== ===== =======
*/
# define ETALON_CHECK
# undef OPENMP
# undef MPI
# define UV_ADV
# define UV_COR
# define SOLVE3D
# define SALINITY
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_SSFLUX
# define ANA_SRFLUX
# define ANA_BTFLUX
# define ANA_BSFLUX
# define EW_PERIODIC

#elif defined SOLITON
/*
!                       Equatorial Rossby Wave Example
!                       ========== ====== ==== =======
*/
# define ETALON_CHECK
# undef OPENMP
# undef MPI
# define UV_COR
# define UV_ADV
# define ANA_GRID
# define ANA_INITIAL
# define AVERAGES
# define EW_PERIODIC
# define ANA_SMFLUX

# elif defined OVERFLOW
/*
!                       Gravitational/Overflow Example
!                       ====================== =======
*/
# define ETALON_CHECK
# undef OPENMP
# undef MPI
# define UV_ADV
# define UV_COR
# define UV_VIS2
# define UV_MIX_GEO
# define TS_DIF2
# define TS_MIX_GEO
# define SOLVE3D
# define ANA_GRID
# define ANA_INITIAL
# define ANA_SMFLUX
# define ANA_STFLUX
# define ANA_BTFLUX

#elif defined UPWELLING
/*
!                       Upwelling Example
!                       ========= =======
*/
# define ETALON_CHECK
# undef OPENMP
# undef MPI
# define SOLVE3D
# define UV_COR
# define UV_ADV
# define ANA_GRID
# define ANA_INITIAL
# define AVERAGES
# define SALINITY
# define NONLIN_EOS
# define SPLIT_EOS
# define ANA_SSFLUX
# define ANA_SRFLUX
# define ANA_STFLUX
# define ANA_BSFLUX
# define ANA_BTFLUX
# define ANA_SMFLUX
# define LMD_MIXING
# define LMD_SKPP
# define LMD_BKPP
# define LMD_RIMIX
# define LMD_CONVEC
# define EW_PERIODIC

#elif defined VORTEX
/*
!                       Baroclinic Vortex Example (TEST AGRIF)
!                       ========== ====== ======= ===== ======
*/
# undef  ETALON_CHECK
# undef  OPENMP
# undef  MPI
# define AGRIF
# define AGRIF_2WAY
# define SOLVE3D
# define UV_COR
# define UV_ADV
# define ANA_STFLUX
# define ANA_SMFLUX
# define ANA_BSFLUX
# define ANA_BTFLUX
# define ANA_VMIX
# define SPONGE
# define ZCLIMATOLOGY
# define M2CLIMATOLOGY
# define M3CLIMATOLOGY
# define TCLIMATOLOGY
# define ZNUDGING
# define M2NUDGING
# define M3NUDGING
# define TNUDGING
# define OBC_EAST
# define OBC_WEST
# define OBC_NORTH
# define OBC_SOUTH
# define OBC_M2FLATHER
# define OBC_TORLANSKI
# define OBC_M3ORLANSKI

#elif defined JET
/*
!                       Baroclinic JET Example
!                       ========== === =======
*/
# define ANA_JET
# undef  ETALON_CHECK
# undef  MPI
# undef  AGRIF
# undef  AGRIF_2WAY
# define SOLVE3D
# define UV_COR
# define UV_ADV
# define UV_VIS2
# define UV_MIX_S
# ifdef ANA_JET
#  define ANA_GRID
#  define ANA_INITIAL
# endif
# define ANA_STFLUX
# define ANA_SMFLUX
# define ANA_BSFLUX
# define ANA_BTFLUX
# define ANA_VMIX
# define EW_PERIODIC
# define CLIMATOLOGY
# ifdef CLIMATOLOGY
#  define ZCLIMATOLOGY
#  define M2CLIMATOLOGY
#  define M3CLIMATOLOGY
#  define TCLIMATOLOGY
#  define ZNUDGING
#  define M2NUDGING
#  define M3NUDGING
#  define TNUDGING
#  define ROBUST_DIAG
#  define ZONAL_NUDGING
#  ifdef ANA_JET
#   define ANA_SSH
#   define ANA_M2CLIMA
#   define ANA_M3CLIMA
#   define ANA_TCLIMA
#  endif
# endif
# define LMD_MIXING 
# ifdef  LMD_MIXING
#  undef  ANA_VMIX
#  define ANA_SRFLUX
#  undef  LMD_KPP
#  define LMD_RIMIX
#  define LMD_CONVEC
# endif 

#endif /* END OF CONFIGURATION CHOICE */

#include "set_global_definitions.h"

