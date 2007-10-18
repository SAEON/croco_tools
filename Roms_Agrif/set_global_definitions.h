!
! $Id: set_global_definitions.h,v 1.8 2005/10/10 13:40:19 pmarches Exp $
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
 
/*
 * Activate barotropic pressure gradient response to the
 * perturbation of free-surface in the presence of stratification
*/

#ifdef SOLVE3D
# define VAR_RHO_2D
#endif

/*
 * Set default time-averaging filter for barotropic fields.
*/
 
# undef M2FILTER_COSINE
# undef M2FILTER_FLAT
# define M2FILTER_POWER
#if defined SSH_TIDES || defined UV_TIDES
# undef M2FILTER_POWER
# define M2FILTER_FLAT
#endif

/*
**  Select model dynamics for MOMENTUM equations:
**  (The default horizontal advection is third-order upstream bias)
*/

#undef HADV_C4_UV        /* 4th-order centered horizontal advection */
#define VADV_SPLINES_UV  /* splines vertical advection */
#undef  VADV_C2_UV       /* 2nd-order centered vertical advection */

/*
 *  Select model dynamics for TRACER equations:
 *  (The default horizontal and vertical advection is 4th-order centered)
*/

#define HADV_UPSTREAM_TS  /* 3rd-order upstream horiz. advection */
#ifdef SPLIT_UP3          /* Split 3rd-order scheme into   */
# undef HADV_UPSTREAM_TS  /*    4th order centered         */
# undef TS_DIF2           /*     + hyperdiffusion          */
# define TS_DIF4
# define DIF_COEF_3D 
# define DIF_PECLET
#endif
#undef  HADV_AKIMA_TS     /* 4th-order Akima horiz. advection */
#define VADV_SPLINES_TS   /* splines vertical advection */
#undef  VADV_AKIMA_TS     /* 4th-order Akima vertical advection */
#undef  VADV_C2_TS        /* 2nd-order centered vertical advection */
#undef  CONST_TRACERS


/* Switch ON/OFF double precision for real type variables (since this
 is mostly controlled by mpc and/or compuler options, this CPP-switch
 affects only on the correct choice of netCDF functions, see below)
 and the use QUAD precision for global summation variables, which is
 always desirable, but some compilers do not support it.
*/

#define DBLEPREC

#if defined CLUSTER
# define QUAD 8
# define QuadZero 0.D0
#elif defined Linux
# define QUAD 8
# define QuadZero 0.D0
#elif defined __IFC
# define QUAD 16
# define QuadZero 0.0_16
#else
# define QUAD 16
# define QuadZero 0.q0
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

#ifdef MPI
# define GLOBAL_2D_ARRAY -1:Lm+2+padd_X,-1:Mm+2+padd_E
# define START_2D_ARRAY -1,-1
#else
# ifdef EW_PERIODIC
#  ifdef NS_PERIODIC
#   define GLOBAL_2D_ARRAY -1:Lm+2+padd_X,-1:Mm+2+padd_E
#   define START_2D_ARRAY -1,-1
#  else
#   define GLOBAL_2D_ARRAY -1:Lm+2+padd_X,0:Mm+1+padd_E
#   define START_2D_ARRAY -1,0
#  endif
# else
#  ifdef NS_PERIODIC
#   define GLOBAL_2D_ARRAY 0:Lm+1+padd_X,-1:Mm+2+padd_E
#   define START_2D_ARRAY 0,-1
#  else
#   define GLOBAL_2D_ARRAY 0:Lm+1+padd_X,0:Mm+1+padd_E
#   define START_2D_ARRAY 0,0
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
# define WESTERN_EDGE Istr.eq.1
# define EASTERN_EDGE Iend.eq.Lm
# define SOUTHERN_EDGE Jstr.eq.1
# define NORTHERN_EDGE Jend.eq.Mm
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
#define SINGLE_TILE_MODE  Iend-Istr+Jend-Jstr.eq.Lmmpi+Mmmpi-2
#else
#define SINGLE_TILE_MODE  Iend-Istr+Jend-Jstr.eq.Lm+Mm-2
#endif

/*
  Define logical flags for the first 2D and 3D time steps.
 This affects proper startup procedure for 2D/3D Adams-Bashforth
 [-- Adams-Moulton] time-stepping engines for the model.
*/
#define LF_AM_STEP
#undef AB_AM_STEP

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
!
# define CVECTOR CDIR$ IVDEP
# define CSDOACROSS C$DOACROSS
# define CAND C$&
# define ENTER_CRITICAL_REGION SPACE call mp_setlock()
# define EXIT_CRITICAL_REGION  SPACE call mp_unsetlock()
# define CSDISTRIBUTE_RESHAPE !! c$distribute 
/* # define CSDISTRIBUTE_RESHAPE !! c$distribute_reshape */
# define BLOCK_PATTERN block,block
# define BLOCK_CLAUSE !! onto(2,*)
!
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
!
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
#else
# define NF_FTYPE NF_REAL
# define nf_get_att_FTYPE nf_get_att_real
# define nf_put_att_FTYPE nf_put_att_real
# define nf_get_var1_FTYPE nf_get_var1_real
# define nf_put_var1_FTYPE nf_put_var1_real
# define nf_get_vara_FTYPE nf_get_vara_real
# define nf_put_vara_FTYPE nf_put_vara_real
#endif
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
# define fast_indx_out knew

/*
  Default boundary conditions for nesting 
*/
# ifdef AGRIF
#  define AGRIF_OBC_EAST
#  define AGRIF_OBC_WEST
#  define AGRIF_OBC_NORTH
#  define AGRIF_OBC_SOUTH
#  undef AGRIF_STORE_BAROT_CHILD
#  define AGRIF_FLUX_BC 
#  undef AGRIF_POLY_DUAVG
#  undef AGRIF_LOCAL_VOLCONS
#  define AGRIF_OBC_M2SPECIFIED

#  ifdef AGRIF_2WAY
#   define AGRIF_OBC_M3SPECIFIED
#   define AGRIF_OBC_TSPECIFIED
#  else
#   define AGRIF_OBC_M3ORLANSKI
#   define AGRIF_OBC_TORLANSKI
#  endif
# endif

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
# undef DIURNAL_SRFLUX
# undef TS_DIF2
# undef TS_DIF4
# undef CLIMAT_TS_MIXH
# undef MIX_GP_TS
# undef MIX_GP_UV
# undef SMAGORINSKY
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

