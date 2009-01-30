		!
! $Id: cppdefs.h,v 1.28 2005/10/27 09:58:11 pmarches Exp $
!
/*
   This is "cppdefs.h": MODEL CONFIGURATION FILE
   ==== == ============ ===== ============= ====
*/
#undef BASIN           /* Basin Example */
#undef CANYON_A        /* Canyon_A Example */
#undef CANYON_B        /* Canyon_B Example */
#undef EQUATOR         /* Equator Example  */
#undef GRAV_ADJ        /* Graviational Adjustment Example */
#undef INNERSHELF      /* Inner Shelf Example */
#undef RIVER           /* River run-off Example */
#undef OVERFLOW        /* Graviational/Overflow Example */
#undef SEAMOUNT        /* Seamount Example */
#undef SHELFRONT       /* Shelf Front Example */
#undef SOLITON         /* Equatorial Rossby Wave Example */
#undef UPWELLING       /* Upwelling Example */
#undef VORTEX          /* Baroclinic Vortex Example */
#undef INTERNAL        /* Internal Tide Example */
#define REGIONAL       /* REGIONAL Applications */


#if defined REGIONAL
/*
!====================================================================
!               REGIONAL (realistic) Configurations
!==================================================================== 
!
!------------------------
! BASIC OPTIONS
!------------------------
!
*/
                      /* Configuration Namea */
# define PERU
                      /* Parallelization */
# undef OPENMP
# define MPI
                      /* Nesting */
# define AGRIF
# define AGRIF_2WAY
                      /* Open Boundary Conditions */
# undef TIDES
# undef OBC_EAST
# define OBC_WEST
# define OBC_NORTH
# define OBC_SOUTH
                      /* Applications */
# define BIOLOGY
# undef FLOATS
# undef STATIONS
# undef PASSIVE_TRACER
# undef SEDIMENT
# undef BBL
/*!
!------------------------
! PRE-SELECTED OPTIONS
!------------------------
*/
                      /* Parallelization */
# ifdef MPI
#  undef PARALLEL_FILES
# endif
# undef AUTOTILING
                      /* Model dynamics */
# define SOLVE3D
# define UV_COR
# define UV_ADV
# ifdef TIDES
#  define SSH_TIDES
#  define UV_TIDES
#  define TIDERAMP
# endif
                      /* Grid configuration */
# define CURVGRID
# define SPHERICAL
# define MASKING
!
                      /* Lateral Momentum Mixing */
# define UV_VIS2
# define MIX_GP_UV
# undef UV_SPLIT_UP3
# undef VIS_SMAGO
                      /* Lateral Tracer Mixing */
# define MIX_GP_TS
# define TS_DIF2
# undef TS_SPLIT_UP3
                      /* Vertical Mixing */
# undef BODYFORCE
# undef BVF_MIXING
# define LMD_MIXING
# ifdef LMD_MIXING
#  define LMD_SKPP
#  define LMD_BKPP
#  define LMD_RIMIX
#  define LMD_CONVEC
#  undef LMD_DDMIX
#  undef LMD_NONLOCAL
# endif
                      /* Equation of State */
# define SALINITY
# define NONLIN_EOS
# define SPLIT_EOS
                      /* Surface Forcing */
# define QCORRECTION
# define SFLX_CORR
# define DIURNAL_SRFLUX

# undef BULK_FLUX
# ifdef BULK_FLUX
# define LW_ONLINE
#  define BULK_EP
#  undef BULK_SMFLUX
#  define DIURNAL_SRFLUX
# endif
# ifdef BULK_EP
#  undef QCORRECTION
#  undef SFLX_CORR
# endif
# ifdef BULK_SMFLUX
#  undef BULK_WVEC
#  define BULK_WSTR
# endif
                      /* Lateral Forcing */
# define SPONGE

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
#  undef ROBUST_DIAG
# endif

# define FRC_BRY
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
# undef PSOURCE
# undef ANA_PSOURCE
                      /* Open Boundary Conditions */
# ifdef TIDES
#  define OBC_M2FLATHER
# else
#  undef OBC_M2SPECIFIED
#  undef OBC_M2FLATHER
#  define OBC_M2CHARACT
#  undef OBC_M2ORLANSKI
#  ifdef OBC_M2ORLANSKI
#   define OBC_VOLCONS
#  endif
# endif
# define OBC_M3ORLANSKI
# define OBC_TORLANSKI
# undef OBC_M3SPECIFIED
# undef OBC_TSPECIFIED
                      /* Input/Output & Diagnostics */
# define AVERAGES
# define AVERAGES_K
# undef DIAGNOSTICS_TS
# ifdef DIAGNOSTICS_TS
# undef DIAGNOSTICS_TS_ADV
# undef DIAGNOSTICS_TS_MLD
# endif
# undef DIAGNOSTICS_UV
/*
!           Applications:
!---------------------------------
! Biology, floats, Stations, 
! Passive tracer, Sediments, BBL
!---------------------------------
*/
# ifdef BIOLOGY
#  define PISCES
#  undef BIO_NPZD
#  undef BIO_N2P2Z2D2
#  undef BIO_N2PZD2
/*      BIOLOGY OPTIONS  */
#  ifdef PISCES
#   define key_trc_pisces
#   define key_passivetrc
#   undef DIAGNOSTICS_BIO
#   ifdef DIAGNOSTICS_BIO
#     define key_trc_diaadd
#     define key_trc_dia3d
#   endif
#  endif
#  ifdef BIO_NPZD
#   undef OXYGEN      /* Under Development */
#  endif
#  if defined BIO_NPZD || defined BIO_N2P2Z2D2
#   define DIAGNOSTICS_BIO
#  endif
#  ifdef BIO_N2P2Z2D2
#   undef VAR_CHL_C
#  endif
# endif

# ifdef FLOATS
#  undef FLOATS_GLOBAL_ATTRIBUTES
#  undef IBM
#  undef RANDOM_WALK
#  ifdef RANDOM_WALK
#   define DIEL_MIGRATION
#   define RANDOM_VERTICAL
#   define RANDOM_HORIZONTAL
#  endif
# endif

# ifdef STATIONS
#  define ALL_SIGMA
# endif

# ifdef SEDIMENT
#  define ANA_SEDIMENT
#  undef BED_ARMOR
#  undef ANA_SPFLUX
#  undef ANA_BPFLUX
#  define LINEAR_CONTINUATION
#  undef NEUMANN
# endif

# ifdef BBL
#  define ANA_WWAVE
#  ifdef SEDIMENT
#   undef ANA_BSEDIM
#  else
#   define ANA_BSEDIM
#  endif
#  undef Z0_BL
#  ifdef Z0_BL
#   define Z0_RIP
#  endif
#  undef Z0_BIO
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
# define MIX_S_UV
# define SOLVE3D
# define TS_DIF2
# define MIX_S_TS
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
# define MIX_S_UV
# define SOLVE3D
# define SALINITY
# define TS_DIF2
# define MIX_S_TS
# undef BODYFORCE
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
# define LMD_SKPP
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
# define UV_ADV
# define UV_VIS2
# define MIX_S_UV
# define TS_DIF2
# define MIX_S_TS
# define SOLVE3D
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
# define NS_PERIODIC

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
# undef  MIX_GP_UV
# undef  TS_DIF2
# undef  MIX_GP_TS
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
# define MIX_GP_UV
# define TS_DIF2
# define MIX_GP_TS
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
# define ETALON_CHECK
# undef OPENMP
# undef MPI
# define AGRIF
# define AGRIF_2WAY
# define MASKING
# define SOLVE3D
# define UV_COR
# define UV_ADV
# define ANA_STFLUX
# define ANA_SMFLUX
# define ANA_BSFLUX
# define ANA_BTFLUX
# define ANA_VMIX
# define UV_VIS2
# define MIX_S_UV
# define TS_DIF2
# define MIX_S_TS
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
#endif

#include "set_global_definitions.h"

