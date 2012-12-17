! $Id$
!======================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
! 
! ROMS_AGRIF website : http://www.romsagrif.org
!======================================================================
/*
   This is "cppdefs.h": MODEL CONFIGURATION FILE
   ==== == ============ ===== ============= ====
*/
#define SALINITY	/* Activate salinity */
#define NONLIN_EOS	/* nonlinear equation of state */

#define LMD_MIXING	/* Activate Large/McWilliams/Doney interior closure */
#define LMD_RIMIX	/* Add diffusivity due to shear instability */
#define LMD_DDMIX	/* Add convective mixing due to shear instability */
#define LMD_CONVEC	/* Add double-diffusive mixing */
#define LMD_KPP		/* turn on surface boundary layer kpp mixing */
#define LMD_NONLOCAL	/* turn on nonlocal transport */

#undef DIURNAL_SW      /* Activate diurnal insolation */

#define  SST_SKIN	/* Activate Zeng/Beljaars model of SST skin */

#define BULK_FLUX	/* Activate Fairall bulk forcing parametrization */
#ifdef BULK_FLUX
# define BULK_LW	/* SST feedback on LW flux */
# define BULK_EP        /* SST feedback on evaporation and salt flux */
# define BULK_SMFLUX    /* SST feedback on wind stress */
#endif

#undef BIOLOGY		/* Activate biological model */
#define BIO_OCEAN	/* oceanic species */
#ifdef  BIOLOGY
# define OXYGEN		/* oxygene */
#endif

#undef UPWELLING	/* Activate parameterized upwelling */
#ifdef  UPWELLING
# undef SINUS_W		/* sinusoidal profile of vertical velocities */
# undef LINEAR_W	/* inear profile of vertical velocities */
# define PARAB_W	/* parabolic profile of vertical velocities */
# define EXPORT 	/* parameterized horizontal advection */
#endif

#define NUDGE		/* Activate restoring to climatology (T,S,NO3) */

#undef NCARG		/* Define if using NCAR graphics libraries */

#define RESTART_OUT	/* Dump a restart file */
#undef  RESTART_IN	/* Restart from a file */
#define BIN_OUTPUT	/* Binary output */
#define ASCII_OUTPUT	/* ASCII output */

#define FIRST_TIME_STEP iic.eq.ntstart
#define LAST_TIME_STEP iic.eq.ntimes+1
#define TIME_TO_OUTPUT tdays.gt.twrite.and.mod(iic-ntstart,noutput).eq.0
#define TIME_TO_WRITE  mod(iic-ntstart,nwrite).eq.0
#define TIME_TO_PLOT tdays.gt.twrite.and.mod(iic-ntstart,nplot).eq.0


