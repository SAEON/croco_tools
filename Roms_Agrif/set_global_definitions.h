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
/* This is "global_definitions.h": It contains a set of predetermined
 macro definitions which are inserted into the individual files by
 C-preprocessor. General user is strongly discouraged from attempts
 to modify anything below this line.
------------------------------------------------------------------ */

/* Switch to mixed [tiled + single-block] execution. Activation of
 this switch enables special logical branch in "compute_tile_bounds"
 which recognizes tile=NSUB_X*NSUB_E as covering the whole model
 grid, and it increases sizes of arrays declared in "private_scratch"
 to accomodate enough workspace accordingly. This switch is used for
 debugging purposes only and normally should be undefined.
*/
#undef ALLOW_SINGLE_BLOCK_MODE
#ifdef ALLOW_SINGLE_BLOCK_MODE
# define SINGLE NSUB_X*NSUB_E,NSUB_X*NSUB_E !!!
#endif

/*  Activate the RVTK_DEBUG procedure that will compare the results
    serial and multi-processor result by comparing binary file
*/
#undef RVTK_DEBUG
 
/*
   Activate barotropic pressure gradient response to the
   perturbation of free-surface in the presence of stratification
*/
#ifdef SOLVE3D
# define VAR_RHO_2D
#endif

/*
   Set default time-averaging filter for barotropic fields.
*/
# undef M2FILTER_COSINE
# undef M2FILTER_FLAT
# define M2FILTER_POWER
#if defined SSH_TIDES || defined UV_TIDES
# undef M2FILTER_POWER
# define M2FILTER_FLAT
#endif

/*
    Select MOMENTUM LATERAL advection-diffusion scheme:
    (The default is third-order upstream biased)
*/
#undef  UV_HADV_C4        /* 4th-order centered lateral advection */
#ifdef UV_VIS_SMAGO 
# define VIS_COEF_3D
#endif

/*
    if defined apply MOMENTUM LATERAL diffusion in the interior 
    over an anomaly only with respect to a reference frame (climatology)
*/
#if defined M3CLIMATOLOGY
# undef CLIMAT_UV_MIXH
#endif

/*
    Select MOMENTUM VERTICAL advection scheme:
*/
#define UV_VADV_SPLINES   /* splines vertical advection */
#undef  UV_VADV_C2        /* 2nd-order centered vertical advection */

/*
    Select TRACER LATERAL advection-diffusion scheme
    (The default is third-order upstream biased)
*/
/* #undef  TS_HADV_C6     6th-order upstream lateral advection */ 
/* #undef  TS_HADV_UP5    5th-order upstream lateral advection */ 
/* #undef  TS_HADV_C4     4th-order centered lateral advection */
/* #undef  TS_HADV_UP3    3rd-order upstream lateral advection */
                     
# ifdef TS_HADV_C4      /* 4th-order centered advection        */
#  define  TS_DIF2      /*   + Laplacian Diffusion             */
#  undef   TS_DIF4      /*                                     */
#  define  TS_DIF_SMAGO /*   + Smagorinsky diffusivity         */
#  define  TS_MIX_ISO   /*   + Isopycnal rotation              */ 
# endif 
#ifdef TS_HADV_RSUP3   /*  Rotated-Split 3rd-order scheme is:  */
# define TS_HADV_C4    /*    4th-order centered advection      */
# undef  TS_DIF2       /*               +                      */
# define TS_DIF4       /*         Hyperdiffusion  with         */
# define TS_MIX_GEO    /*        Geopotential rotation         */
# undef  TS_MIX_ISO    /*     or Isopycnal    rotation         */
# define TS_MIX_IMP    /*   and  Semi-Implicit Time-Stepping   */
# define DIF_COEF_3D 
#endif
#ifdef TS_HADV_RSUP5   /*    Pseudo RS 5th-order scheme is:    */
# define TS_HADV_C6    /*    6th-order centered advection      */
# undef  TS_DIF2       /*               +                      */
# define TS_DIF4       /*         Hyperdiffusion  with         */
# define TS_MIX_GEO    /*        Geopotential rotation         */
# undef  TS_MIX_ISO    /*     or Isopycnal    rotation         */
# define TS_MIX_IMP    /*   and  Semi-Implicit Time-Stepping   */
# define DIF_COEF_3D 
#endif

#ifdef BIO_HADV_WENO5  /*   WENO5 for passive tracers     */
# define NTRA_T3DMIX 2
#else
# define NTRA_T3DMIX NT
#endif

#ifdef TS_DIF_SMAGO    /*   Smagorinsky diffusivity option     */
# define DIF_COEF_3D
#endif
#if defined TS_MIX_ISO || (defined TS_DIF4 && defined TS_MIX_GEO)
# define TS_MIX_IMP
#endif

/*
   if defined apply interior diffusion over tracer anomalies
   with respect to a reference frame (climatology)
*/
# if defined TCLIMATOLOGY
#  undef CLIMAT_TS_MIXH
#  undef CLIMAT_TS_MIXH_FINE
# endif

/*
    Select model dynamics for TRACER vertical advection
    (The default is 4th-order centered)
*/
#undef  TS_VADV_SPLINES   /* splines vertical advection */
#define TS_VADV_AKIMA     /* 4th-order Akima vertical advection */
#undef  TS_VADV_C2        /* 2nd-order centered vertical advection */

/*
   SPONGE:  
   define SPONGE_GRID, SPONGE_DIF2 
   and SPONGE_VIS2 
*/
#ifdef SPONGE
# define SPONGE_GRID
# define SPONGE_DIF2
# define SPONGE_VIS2
#endif

/*
    Constant tracer option
*/
#undef   CONST_TRACERS

/*
    Psource option
*/
#ifdef PSOURCE
#  define ANA_PSOURCE  /* ON: set vertical profil for qbar */
#endif
/*
    Bulk flux option
*/
#ifdef BULK_FLUX
# ifdef BULK_EP
#  undef QCORRECTION
#  undef SFLX_CORR
# endif
# ifdef BULK_SMFLUX     
#  define BULK_SM_UPDATE /* ON: Compute wind stress via bulk_flux */
# endif
# ifdef ONLINE
#  define CUBIC_INTERP
# endif
#endif

/* Switch ON/OFF double precision for real type variables (since this
 is mostly controlled by mpc and/or compuler options, this CPP-switch
 affects only on the correct choice of netCDF functions, see below)
 and the use QUAD precision for global summation variables, which is
 always desirable, but some compilers do not support it.
*/

#define DBLEPREC

#if defined DBLEPREC && !defined Linux && !defined PGI && !defined __IFC
# define QUAD 16
# define QuadZero 0.Q0
/* # define QuadZero 0.0_16 */
!! Gc remove because incompatibe with AGRIF
!#elif defined DBLEPREC && defined Ifort
!/* Ifort supports QUAD precision */
!# define QUAD 16
!# define QuadZero 0.Q0
!! Gc remove because incompatibe with AGRIF
#else
# define QUAD 8
# define QuadZero 0.D0
#endif

/*
  Define standard dimensions for the model arrays (vertical
 dimensions are inserted explicitly in the code, when needed). The
 periodic and nonperiodic versions may be different by the number of
 ghost points on each edge (2 or 1 respectively). This distinction
 is present only in the purely SHARED MEMORY code. In the case of
 message passing, when array dimensions correspond to a portion of
 the physical domain (as opposite to the whole domain), so two ghost
 zones are always provided on the each side. These data for these
 two ghost zones is then exchanged by message passing. 
*/
#if defined TS_HADV_UP5 || defined TS_HADV_C6 \
    || defined TS_HADV_WENO5 || defined BIO_HADV_WENO5
# define THREE_GHOST_POINTS
# define THREE_GHOST_POINTS_TS
# undef  THREE_GHOST_POINTS_UV
#endif

#ifdef THREE_GHOST_POINTS
# ifdef MPI
#  define GLOBAL_2D_ARRAY -2:Lm+3+padd_X,-2:Mm+3+padd_E
#  define GLOBAL_1D_ARRAYXI -2:Lm+3+padd_X
#  define GLOBAL_1D_ARRAYETA -2:Mm+3+padd_E
#  define START_2D_ARRAY -2,-2
#  define START_1D_ARRAYXI -2
#  define START_1D_ARRAYETA -2
# else
#  ifdef EW_PERIODIC
#   define GLOBAL_1D_ARRAYXI -2:Lm+3+padd_X
#   define START_1D_ARRAYXI -2
#   ifdef NS_PERIODIC
#    define GLOBAL_2D_ARRAY -2:Lm+3+padd_X,-2:Mm+3+padd_E
#    define GLOBAL_1D_ARRAYETA -3:Mm+3+padd_E
#    define START_2D_ARRAY -2,-2
#    define START_1D_ARRAYETA -2
#   else
#    define GLOBAL_2D_ARRAY -2:Lm+3+padd_X,0:Mm+1+padd_E
#    define GLOBAL_1D_ARRAYETA 0:Mm+1+padd_E
#    define START_2D_ARRAY -2,0
#    define START_1D_ARRAYETA 0
#   endif
#  else
#   define GLOBAL_1D_ARRAYXI 0:Lm+1+padd_X
#   define START_1D_ARRAYXI 0
#   ifdef NS_PERIODIC
#    define GLOBAL_2D_ARRAY 0:Lm+1+padd_X,-2:Mm+3+padd_E
#    define GLOBAL_1D_ARRAYETA -2:Mm+3+padd_E
#    define START_2D_ARRAY 0,-2
#    define START_1D_ARRAYETA -2
#   else
#    define GLOBAL_2D_ARRAY 0:Lm+1+padd_X,0:Mm+1+padd_E
#    define GLOBAL_1D_ARRAYETA 0:Mm+1+padd_E
#    define START_2D_ARRAY 0,0
#    define START_1D_ARRAYETA 0
#   endif
#  endif
# endif
#else
# ifdef MPI
#  define GLOBAL_2D_ARRAY -1:Lm+2+padd_X,-1:Mm+2+padd_E
#  define GLOBAL_1D_ARRAYXI -1:Lm+2+padd_X
#  define GLOBAL_1D_ARRAYETA -1:Mm+2+padd_E
#  define START_2D_ARRAY -1,-1
#  define START_1D_ARRAYXI -1
#  define START_1D_ARRAYETA -1
# else
#  ifdef EW_PERIODIC
#   define GLOBAL_1D_ARRAYXI -1:Lm+2+padd_X
#   define START_1D_ARRAYXI -1
#   ifdef NS_PERIODIC
#    define GLOBAL_2D_ARRAY -1:Lm+2+padd_X,-1:Mm+2+padd_E
#    define GLOBAL_1D_ARRAYETA -1:Mm+2+padd_E
#    define START_2D_ARRAY -1,-1
#    define START_1D_ARRAYETA -1
#   else
#    define GLOBAL_2D_ARRAY -1:Lm+2+padd_X,0:Mm+1+padd_E
#    define GLOBAL_1D_ARRAYETA 0:Mm+1+padd_E
#    define START_2D_ARRAY -1,0
#    define START_1D_ARRAYETA 0
#   endif
#  else
#   define GLOBAL_1D_ARRAYXI 0:Lm+1+padd_X
#   define START_1D_ARRAYXI 0
#   ifdef NS_PERIODIC
#    define GLOBAL_2D_ARRAY 0:Lm+1+padd_X,-1:Mm+2+padd_E
#    define GLOBAL_1D_ARRAYETA -1:Mm+2+padd_E
#    define START_2D_ARRAY 0,-1
#    define START_1D_ARRAYETA -1
#   else
#    define GLOBAL_2D_ARRAY 0:Lm+1+padd_X,0:Mm+1+padd_E
#    define GLOBAL_1D_ARRAYETA 0:Mm+1+padd_E
#    define START_2D_ARRAY 0,0
#    define START_1D_ARRAYETA 0
#   endif
#  endif
# endif
#endif

#define PRIVATE_1D_SCRATCH_ARRAY Istr-2:Iend+2
#define PRIVATE_2D_SCRATCH_ARRAY Istr-2:Iend+2,Jstr-2:Jend+2
#define PRIVATE_1DXI_SCRATCH_ARRAY Istr-2:Iend+2
#define PRIVATE_1DETA_SCRATCH_ARRAY Jstr-2:Jend+2

/*
  The following definitions contain fortran logical expressions
 equivalent to the question: ''Am I the thread working on subdomain
 [tile] which is adjacent to the WESTERN [EASTERN/SOUTHERN/NORTHERN]
 edge of the model domain?'' These logical expressions are used to
 control loop bounds over a subdomain [tile], so that boundary points
 are included, if needed and if the subdomain is adjacent to the
 boundary. They are also used to decide which thread is updating the
 segment of the boundary [bcs2d,bcs3d] to avoid mutual overlap. In
 the case when there is only one subdomain all four of these logical
 expressions have value .TRUE.

   Message passing and shared memory versions.
*/
#ifdef MPI
# define WESTERN_EDGE .not.WEST_INTER
# define EASTERN_EDGE .not.EAST_INTER
# define SOUTHERN_EDGE .not.SOUTH_INTER
# define NORTHERN_EDGE .not.NORTH_INTER
#else
# define WESTERN_EDGE istr.eq.1
# define EASTERN_EDGE iend.eq.Lm
# define SOUTHERN_EDGE jstr.eq.1
# define NORTHERN_EDGE jend.eq.Mm
#endif

/*
  Sometimes it is needed to include MPI-node number into printed
 message. To do it conditionally (MPI code only) add MYID (without
 preceeding comma) into the end of the message to be printed.
*/
#ifdef MPI
# define MYID ,' mynode =', mynode
#else
# define MYID !
#endif

/*
  Sometimes an operation needs to be restricted to one MPI process,
 the master process. Typically this occurs when it is desirable to
 avoid redundant write of the same message by all MPI processes into
 stdout. The following switch serves this purpose:
*/
#ifdef MPI
# define MPI_master_only if (mynode.eq.0)
#else
# define MPI_master_only
#endif

/*
  Similarly, if operation needed to be done by one thread only, e.g.
 copy a redundantly computed private scalar into shared scalar, or
 write an error message in situation when the error condition is
 discovered redundantly by every thread (and guaranteed to be the
 same for all) and only one needs to complain. The following flag is
 used to restrict the operation only to thread which is working on
 south-western tile. This switch is the same for MPI/nonMPI code.
*/
#define ZEROTH_TILE Istr+Jstr.eq.2

/*
  Occasinally a subroutine designed to process a tile may be called
 to process the whole domain. If it is necessary to dustinguish
 whether it is being called for the whole domain (SINGLE_TILE_MODE)
 or a tile. This switch is the same for MPI/nonMPI code.
*/
#ifdef MPI
# undef AUTOTILING
# define SINGLE_TILE_MODE  Iend-Istr+Jend-Jstr.eq.Lmmpi+Mmmpi-2
#else
# define SINGLE_TILE_MODE  Iend-Istr+Jend-Jstr.eq.Lm+Mm-2
#endif

/*
  Define logical flags for the first 2D and 3D time steps.
 This affects proper startup procedure for 2D/3D Adams-Bashforth
 [-- Adams-Moulton] time-stepping engines for the model.
*/
#define LF_AM_STEP
#undef  AB_AM_STEP

#define FIRST_TIME_STEP iic.eq.ntstart
#ifdef SOLVE3D
# define FIRST_2D_STEP iif.eq.1
# define NOT_LAST_2D_STEP iif.lt.nfast+1
#else
# define FIRST_2D_STEP iic.eq.ntstart
# define NOT_LAST_2D_STEP iic.lt.ntimes+2
#endif

/*
  The following definitions are machine dependent macros, compiler
 directives, etc. A proper set of definitions is activated by a
 proper choice C-preprocessor flag, i.e. -DSGI for an SGI computer
 or -DCRAY for a Cray shared memory architecture (Y-MP, C-90, J-90).
 Definitions for other shared memory platforms may be appended here.
*/
#if defined sgi || defined SGI

# define CVECTOR CDIR$ IVDEP
# define CSDOACROSS C$DOACROSS
# define CAND C$&
# define ENTER_CRITICAL_REGION SPACE call mp_setlock()
# define EXIT_CRITICAL_REGION  SPACE call mp_unsetlock()
# define CSDISTRIBUTE_RESHAPE !! c$distribute 
/* # define CSDISTRIBUTE_RESHAPE !! c$distribute_reshape */
# define BLOCK_PATTERN block,block
# define BLOCK_CLAUSE !! onto(2,*)

#elif defined cray || defined CRAY
# ifdef  DBLEPREC
#  undef  DBLEPREC
# endif
# define CVECTOR CDIR$ IVDEP
# define CSDOACROSS CMIC$ DO ALL
# define SHARE SHARED
# define LOCAL PRIVATE
# define CAND CMIC$&
# define ENTER_CRITICAL_REGION CMIC$ GUARD
# define EXIT_CRITICAL_REGION CMIC$ END GUARD

#endif

#define PUT_GRID_INTO_RESTART
#define PUT_GRID_INTO_HISTORY
#define PUT_GRID_INTO_AVERAGES

/*
  Choice of double/single precision for real type variables
 and associated intrinsic functions.
*/
#ifdef DBLEPREC

c-# define float dfloat
c-# define FLoaT dfloat
c-# define FLOAT dfloat
c-# define sqrt dsqrt
c-# define SQRT dsqrt
c-# define exp dexp
c-# define EXP dexp
c-# define dtanh dtanh
c-# define TANH dtanh

# define NF_FTYPE NF_DOUBLE
# define nf_get_att_FTYPE nf_get_att_double
# define nf_put_att_FTYPE nf_put_att_double
# define nf_get_var1_FTYPE nf_get_var1_double
# define nf_put_var1_FTYPE nf_put_var1_double
# define nf_get_vara_FTYPE nf_get_vara_double
# define nf_put_vara_FTYPE nf_put_vara_double
# define nf_put_var_FTYPE nf_put_var_double
# define nf_put_att_FTYPE nf_put_att_double
#else
# define NF_FTYPE NF_REAL
# define nf_get_att_FTYPE nf_get_att_real
# define nf_put_att_FTYPE nf_put_att_real
# define nf_get_var1_FTYPE nf_get_var1_real
# define nf_put_var1_FTYPE nf_put_var1_real
# define nf_get_vara_FTYPE nf_get_vara_real
# define nf_put_vara_FTYPE nf_put_vara_real
# define nf_put_var_FTYPE nf_put_var_real
# define nf_put_att_FTYPE nf_put_att_real
#endif
/* 
 Choice of setting land mask value to _FillValue
*/ 
#undef FILLVAL

/* 
 Choice of writing start_date information in netCDF output
 (in roms.in, add the keyword start_date:
  For example, if the simulation starts 1 January of 2000, at 00:00:00
  start_date: 01-JAN-2000 00:00:00) 
*/ 
#undef START_DATE

/*
 Choice of double/single precision for netCDF output.
*/
#ifdef OUT_DOUBLE
# define NF_FOUT NF_DOUBLE
#else
# define NF_FOUT NF_REAL
#endif

/*
 Decide which time step of fast variables zeta, ubar, vbar goes
 to output.
*/ 
#ifdef SOLVE3D
# define fast_indx_out knew
#else
# define fast_indx_out kstp
#endif

/*
    --- AGRIF nesting options ---
*/
#ifdef AGRIF
/*                    Update schemes */
# undef  AGRIF_UPDATE_MIX_LOW
# define AGRIF_UPDATE_MIX
# define AGRIF_UPDATE_DECAL
/*                    Conservation options */
# define AGRIF_CONSERV_VOL
# undef  AGRIF_CONSERV_TRA
/*                    Sponge layer */
# define AGRIF_SPONGE
/*                    Boundary conditions */
# define AGRIF_OBC_EAST
# define AGRIF_OBC_WEST
# define AGRIF_OBC_NORTH
# define AGRIF_OBC_SOUTH

# define AGRIF_FLUX_BC 

# define AGRIF_OBC_M2SPECIFIED
# ifdef AGRIF_2WAY
#  define AGRIF_OBC_M3SPECIFIED
#  define AGRIF_OBC_TSPECIFIED
# else
#  define AGRIF_OBC_M3ORLANSKI
#  define AGRIF_OBC_TORLANSKI
# endif
#endif /* AGRIF */

/*
  Consistency for 2D configurations
*/
#ifndef SOLVE3D
# undef AVERAGES_K
# undef SALINITY
# undef NONLIN_EOS
# undef SPLIT_EOS
# undef QCORRECTION
# undef SFLX_CORR
# undef ANA_DIURNAL_SW
# undef BULK_FLUX
# undef TS_DIF2
# undef TS_DIF4
# undef CLIMAT_TS_MIXH
# undef SPONGE_DIF2
# undef TS_HADV_RSUP3
# undef TS_MIX_GEO
# undef UV_MIX_GEO
# undef UV_VIS_SMAGO
# undef TS_DIF_SMAGO
# undef M3NUDGING
# undef TNUDGING
# undef ROBUST_DIAG
# undef M3CLIMATOLOGY
# undef TCLIMATOLOGY
# undef M3_FRC_BRY
# undef T_FRC_BRY
# undef BODYFORCE
# undef BVF_MIXING
# undef LMD_MIXING
# undef LMD_BKPP
# undef LMD_SKPP
# undef LMD_RIMIX
# undef LMD_CONVEC
# undef OBC_M3ORLANSKI
# undef OBC_M3SPECIFIED
# undef OBC_TORLANSKI
# undef OBC_TSPECIFIED
# undef AGRIF_OBC_M3ORLANSKI
# undef AGRIF_OBC_M3SPECIFIED
# undef AGRIF_OBC_TORLANSKI
# undef AGRIF_OBC_TSPECIFIED
#endif

