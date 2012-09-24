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
      program main
!     implicit none
      integer tile, subs, trd, ierr
#include "param.h"
#include "private_scratch.h"
#include "scalars.h"
#include "ncscrum.h"
#ifdef FLOATS
# include "grid.h"
# include "floats.h"
# include "ncscrum_floats.h"
#endif
#if defined RVTK_DEBUG || defined RVTK_DEBUG_ADVANCED
# include "grid.h"
#endif
#ifdef STATIONS
# if !defined FLOATS
#  include "grid.h"
# endif
# include "sta.h"
# include "nc_sta.h"
#endif
#ifdef AGRIF
       Type(Agrif_pgrid),pointer  :: parcours     
#endif 
#ifdef MPI
      include 'mpif.h'
C      real*8 start_time2, start_time1, exe_time
#endif 

      integer :: iifroot, iicroot
#ifdef AGRIF
      integer size_XI,size_ETA,se,sse, sz,ssz
      integer iunit, Agrif_lev_sedim
      parameter(iunit=50)
      external :: step
#include "zoom.h"      
#ifdef PISCES
# include "parameter.h"
#endif
# include "dynparam.h"
#endif
     
#ifdef AUTOTILING
! Initial values of NSUB_X, NSUB_E
      NSUB_X = 1
      NSUB_E = NPP
#endif

# include "dynderivparam.h"

#if defined MPI && !defined AGRIF
      call MPI_Init (ierr)
#endif   

#ifdef AGRIF
      call Agrif_Init_Grids()
      call declare_zoom_variables()
      open(iunit,file='AGRIF_FixedGrids.in',form='formatted',
     &                                   status="old",err=70)
      read(iunit,*,err=80) Agrif_lev_sedim
      goto 90
  70  write(stdout,75) 'AGRIF_FixedGrids.in' 
  75  format(/,' MAIN - error opening file: ',a)
  80  write(stdout,85) 'AGRIF_FixedGrids.in'
  85  format(/,' MAIN - error reading variables in: ',a)
  90  close(iunit)
#endif
   
#ifdef AUTOTILING
      call init_auto_tiling
#endif
#if defined RVTK_DEBUG || defined RVTK_DEBUG_ADVANCED
       call debug_ini
#endif       
#ifdef MPI
C     start_time1=PMPI_Wtime()
      call MPI_Setup (ierr)
      if (ierr.ne.0) goto 100        !--> ERROR
#endif

#define CR  !

      call read_inp (ierr)           ! Read in tunable model
      if (ierr.ne.0) goto 100        ! parameters.
      call init_scalars (ierr)       ! Also initialize global
      if (ierr.ne.0) goto 100        ! scalar variables.

#ifdef PISCES
      call trclsm
      call get_dust
#endif

#ifdef SEDIMENT
# ifdef AGRIF
      if (Agrif_lev_sedim.EQ.0) call init_sediment
# else
      call init_sediment             ! Read sediment initial values and
                                     ! parameters from sediment.in file
# endif
#endif
 
C$OMP PARALLEL DO PRIVATE(tile)
      do tile=0,NSUB_X*NSUB_E-1      ! Create parallel threads;
        call start_timers()          ! start timers for each thread;
        call init_arrays (tile)      ! initialize (FIRST-TOUCH) model 
      enddo                          ! global arrays (most of them 
CR      write(*,*) '-11' MYID        ! are just set to to zero).

#ifdef ANA_GRID
C$OMP PARALLEL DO PRIVATE(tile)      ! Set horizontal curvilinear 
      do tile=0,NSUB_X*NSUB_E-1      ! grid and model bathymetry 
        call ana_grid (tile)         ! (analyticaly or read from 
      enddo                          ! GRID NetCDF file).
                                     ! Also read Land/Sea mask
                                     ! from GRID NetCDF file).
# if defined RVTK_DEBUG || defined RVTK_DEBUG_ADVANCED
C$OMP BARRIER
C$OMP MASTER
       call check_tab2d(h(:,:),'h initialisation #1','r')
C$OMP END MASTER

# endif

#else
      call get_grid
      if (may_day_flag.ne.0) goto 99 !-->  EXIT
#endif

C$OMP PARALLEL DO PRIVATE(tile)
      do tile=0,NSUB_X*NSUB_E-1      ! Compute various metric
        call setup_grid1 (tile)      ! term combinations.
      enddo
CR      write(*,*) '-10' MYID
C$OMP PARALLEL DO PRIVATE(tile)
      do tile=0,NSUB_X*NSUB_E-1
        call setup_grid2 (tile)
      enddo
CR      write(*,*) ' -9' MYID

#ifdef SOLVE3D
                                     ! Set up vertical S-coordinate
      call set_scoord                ! and fast-time averaging  
      call set_weights               ! weights for coupling of 
                                     ! split-explicit baroropic mode.
#endif
CR      write(*,*) ' -8' MYID 
#ifdef SOLVE3D
#if defined VIS_GRID || defined DIF_GRID 
C$OMP PARALLEL DO PRIVATE(tile)
      do tile=0,NSUB_X*NSUB_E-1      ! Rescale horizontal mixing
!        call hmix_coef (tile)        ! coefficients according to
      enddo                          ! local grid size.
#endif
C$OMP PARALLEL DO PRIVATE(tile)      ! Create three-dimensional
      do tile=0,NSUB_X*NSUB_E-1      ! S-coordinate system, which
        call set_depth (tile)        ! may be needed by ana_initial
      enddo                          ! (here it is assumed that free
CR      write(*,*) ' -7' MYID        ! surface zeta=0).
C$OMP PARALLEL DO PRIVATE(tile)
      do tile=0,NSUB_X*NSUB_E-1
        call grid_stiffness (tile)   ! Grid diagnostics only
      enddo
#endif

#ifdef ANA_INITIAL
      if (nrrec.eq.0) then
C$OMP PARALLEL DO PRIVATE(tile)      ! Set initial conditions
        do tile=0,NSUB_X*NSUB_E-1    ! for primitive variables
          call ana_initial (tile)    ! (analytically or read
        enddo                        ! from initial conditions
      else                           ! NetCDF file).
#endif
        call get_initial
C$OMP PARALLEL DO PRIVATE(tile)      ! Set analytical initial 
        do tile=0,NSUB_X*NSUB_E-1    ! conditions for tracers
          call ana_initracer (tile)  ! if not found in initial
        enddo                        ! conditions NetCDF file). 
#ifdef ANA_INITIAL
      endif
#endif
#if defined BIOLOGY && defined PISCES
C$OMP PARALLEL DO PRIVATE(tile)      ! Set analytical initial
        do tile=0,NSUB_X*NSUB_E-1    ! conditions for tracers
          call  pisces_ini (tile)  ! if not found in initial
        enddo                        ! conditions NetCDF file).
#endif
CR      write(*,*) ' -6' MYID 
      if (may_day_flag.ne.0) goto 99 !-->  EXIT

#ifdef SOLVE3D
C$OMP PARALLEL DO PRIVATE(tile)      ! Create three-dimensional
      do tile=0,NSUB_X*NSUB_E-1      ! S-coordinate system: at this
        call set_depth (tile)        ! time free surface is set to
      enddo                          ! a non-zero field, either 
CR      write(*,*)  ' -5' MYID       ! analytically or from restart.
C$OMP PARALLEL DO PRIVATE(tile)
      do tile=0,NSUB_X*NSUB_E-1
        call set_HUV (tile)
      enddo
CR      write(*,*)  ' -4' MYID
C$OMP PARALLEL DO PRIVATE(tile)
      do tile=0,NSUB_X*NSUB_E-1
        call omega (tile)
        call rho_eos (tile)
      enddo
CR      write(*,*)  ' -3' MYID
#endif
!
! Create climatological environment:
!-------------------------------------------------------
! Set nudging coefficient for sea surface hight and tracer
!                 climatology;
! Set bottom sediment grain size [m] and density [kg/m^3]
!                 used in bottom boundary layer formulation;
! Set initial analytical tracer climatology fields;
! Set initial sea surface height climatology;
!
#if defined TNUDGING || defined ZNUDGING || defined SPONGE \
  || (defined BBL && defined ANA_BSEDIM)\
  || (defined SEDIMENT && defined ANA_SEDIMENT)\
  || (defined TCLIMATOLOGY)\
  || (defined ZCLIMATOLOGY && defined ANA_SSH)
C$OMP PARALLEL DO PRIVATE(tile)
      do tile=0,NSUB_X*NSUB_E-1
# if defined TNUDGING || defined ZNUDGING || defined SPONGE
        call set_nudgcof (tile)
# endif
# if defined BBL && defined ANA_BSEDIM
#  ifdef AGRIF
        if (Agrif_lev_sedim.EQ.0) call ana_bsedim (tile)
#  else
        call ana_bsedim (tile)
#  endif
# endif
# if defined SEDIMENT && defined ANA_SEDIMENT
#  ifdef AGRIF
        if (Agrif_lev_sedim.EQ.0) call ana_sediment (tile)
#  else
        call ana_sediment (tile)
#  endif
# endif
# if defined TCLIMATOLOGY
        call ana_tclima (tile)
# endif
# if defined M2CLIMATOLOGY && defined ANA_M2CLIMA ||\
    (defined M3CLIMATOLOGY && defined ANA_M3CLIMA)
        call ana_uclima (tile)
# endif
# if defined ZCLIMATOLOGY && defined ANA_SSH
        call ana_ssh (tile)
# endif
      enddo
#endif
CR      write(*,*) ' -2' MYID 
!
!  Read tracer climatology fields from input NetCDF file;
!  Read sea surface height climatology from input NetCDF file;
!       [recall that the CPP-logic here is mutually exclussive
!       for the calls to ana_bsedim_tile -- get_bsedim;
!       ana_tclima_tile -- get_tclima; ana_ssh_tile -- get_ssh]
!  Read initial data from input forcing NetCDF files.
!
#if defined BBL && !defined ANA_BSEDIM && !defined SEDIMENT
# ifdef AGRIF
      if (Agrif_lev_sedim.EQ.0) call get_bsedim
#  else
      call get_bsedim
#  endif
#endif
C#if defined SEDIMENT && !defined ANA_SEDIMENT
C# ifdef AGRIF
C      if (Agrif_lev_sedim.EQ.0) call get_sediment
C#  else
C      call get_sediment
C#  endif
C#endif
C#endif

#if defined SOLVE3D && defined TCLIMATOLOGY && !defined ANA_TCLIMA
      call get_tclima
#endif
#if defined M2CLIMATOLOGY && !defined ANA_M2CLIMA ||\
   (defined M3CLIMATOLOGY && !defined ANA_M3CLIMA)
      call get_uclima
#endif
#if defined ZCLIMATOLOGY && !defined ANA_SSH
      call get_ssh
#endif
#if !defined ANA_BRY && defined FRC_BRY
      call get_bry
#  ifdef BIOLOGY 
      call get_bry_bio
#  endif
# endif

      call get_vbc
#if defined BBL && !defined ANA_WWAVE
# ifdef AGRIF
      if (Agrif_lev_sedim.EQ.0)
     &  call get_wwave
#  else
      call get_wwave
#  endif
#endif
#if defined SSH_TIDES || defined UV_TIDES
      call get_tides
#endif
CR      write(*,*) ' -1' MYID 

      if (may_day_flag.ne.0) goto 99 !-->  EXIT
      if (ldefhis .and. wrthis(indxTime)) call wrt_his
!#if defined DIAGNOSTICS_TS
!      if (ldefdia .and. wrtdia) call wrt_diags
!#endif
!#if defined DIAGNOSTICS_UV
!      if (ldefdiaM .and. wrtdiaM) call wrt_diagsM
!#endif

#ifdef FLOATS
! Initialization for Lagrangian floats
!-------------------------------------------------------
      nrecflt=0    ! initialization done here and not in 
      ncidflt=-1   ! init_scalars since it must be done only 
                   ! once (whether child levels exist or not)
      spval=1.E15  ! spval is the nodata flag for float variables

      deltac2p=2.3 ! distance from the boundary at which a float
                   ! is transferred from child to parent
      deltap2c=2.5 ! same for transfer from parent to child

      call init_arrays_floats
      call init_floats
# ifdef SPHERICAL
      call interp_r2d_type_ini (lonr(START_2D_ARRAY), iflon)
      call interp_r2d_type_ini (latr(START_2D_ARRAY), iflat)
# else
      call interp_r2d_type_ini (  xr(START_2D_ARRAY), iflon)
      call interp_r2d_type_ini (  yr(START_2D_ARRAY), iflat)
# endif
# ifdef SOLVE3D
      call fill_ini ! fills in trackaux for ixgrd,iygrd,izgrd
                    ! and ifld (either izgrd or ifld is meaningful)
# endif
      if (ldefflt) call wrt_floats
#endif /* FLOATS */

#ifdef STATIONS
! Initialization for stations
!-------------------------------------------------------
      nrecsta=0       ! initialization done here and not in
      ncidsta=-1      ! init_scalars since it must be done only
                      ! once (whether child levels exist or not)
      staspval=1.E15  ! nodata flag for float variables.

      stadeltap2c=2.5 ! distance from the boundary at which a
                      ! float is transfered from parent to child

      call init_arrays_sta
      call init_sta
# ifdef SPHERICAL
      call interp_r2d_sta_ini (lonr(START_2D_ARRAY), istalon)
      call interp_r2d_sta_ini (latr(START_2D_ARRAY), istalat)
# else
      call interp_r2d_sta_ini (  xr(START_2D_ARRAY), istalon)
      call interp_r2d_sta_ini (  yr(START_2D_ARRAY), istalat)
# endif
# ifdef SOLVE3D
      call fill_sta_ini ! fills in trackaux for ixgrd,iygrd,izgrd
                        ! and ifld (either izgrd or ifld is meaningful)
# endif
      if (ldefsta) call wrt_sta
#endif /* STATIONS */

CR      write(*,*) '  0' MYID
      if (may_day_flag.ne.0) goto 99     !-->  EXIT
!
!
!          *****   ********   *****   ******  ********
!         **   **  *  **  *  *   **   **   ** *  **  *
!         **          **    **   **   **   **    **
!          *****      **    **   **   **   *     **
!              **     **    *******   *****      **
!         **   **     **    **   **   **  **     **
!          *****      **    **   **   **   **    **
!
#undef CR
      write(stdout,'(/1x,A27/)') 'MAIN: started time-steping.'
      next_kstp=kstp
      time_start=time

#ifdef SOLVE3D      
      iif = -1
      nbstep3d = 0
#endif
      iic = ntstart

#ifdef AGRIF
      iind = -1
      grids_at_level = -1
      sortedint = -1
      call computenbmaxtimes
#endif

      do iicroot=ntstart,ntimes+1


#ifdef SOLVE3D
#if !defined AGRIF
        do iifroot = 0,nfast+2
#else
        nbtimes = 0
        do while (nbtimes.LE.nbmaxtimes)
#endif
#endif

#ifdef AGRIF
          call Agrif_Step(step)
#else
          call step()
#endif

#ifdef SOLVE3D
        enddo
#endif
        if (may_day_flag.ne.0) goto 99     !-->  EXIT
      enddo                          !--> end of time step

  99  continue                       ! SHUTDOWN:
C$OMP PARALLEL DO PRIVATE(tile)      ! Stop timers and 
      do tile=0,NSUB_X*NSUB_E-1      ! close netCDF files.
        call stop_timers()
      enddo
      call closecdf
#ifdef AGRIF
!
!  Close the netcdf files also for the child grids
!
      parcours=>Agrif_Curgrid%child_grids
      do while (associated(parcours))
        Call Agrif_Instance(parcours % gr)
        call closecdf
        parcours => parcours % next             
      enddo
#endif

 100  continue
#ifdef MPI
      call MPI_Barrier(MPI_COMM_WORLD, ierr)
C     start_time2=PMPI_Wtime()
C     exe_time = start_time2 - start_time1
C     if(mynode.eq.0) print*,'exe_time =',exe_time
      call MPI_Finalize (ierr)
#endif
      stop
      end

