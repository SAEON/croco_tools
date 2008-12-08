!
! $Id: zoom.F,v 1.12 2005/10/07 15:32:41 pmarches Exp $
!
#include "cppdefs.h"
#ifdef AGRIF
!
! AGRIF related routines:
!
! Agrif_initworkspace      : initialize the array sizes.
! Agrif_initvalues         : perform the ROMS initial procedures 
!                            (grid, initial conditions, etc...)
! Agrif_transfer_floatsp2c : checks if a float must be transferred 
!                            from one grid to the child grid
! Agrif_laststep           : determines whether this time step is 
!                            the last one before a ROOT time step.
! Agrif_Invloc             : MPI related shit.
! Agrif_update             : Update in the case of 2-way nesting
!
! Agrif_u3dbc_interp_tile  : Perform the interpolations for u3dbc
!                            to get ubry_east or uclm.
! Agrif_v3dbc_interp_tile  : Perform the interpolations for v3dbc
!                            to get vbry_east or vclm.
! Agrif_t3dbc_interp_tile  : Perform the interpolations for t3dbc
!                            to get tbry_east or tclm.
! Agrif_u2dbc_interp_tile  : Perform the interpolations for u2dbc
!                            to get ubarbry_east or ubclm.
! Agrif_v2dbc_interp_tile  : Perform the interpolations for v2dbc
!                            to get vbarbry_east or vbclm.
! Agrif_zetabc_interp_tile : Perform the interpolations for zetabc
!                            to get zetabry_east or zetaclm.
! 
! Agrif_storebaro_tile     : Store the barotropic velocities for 
!                            the child boundary conditions.
! 
!
!====================================================================
!                   subroutine Agrif_initworkspace
!====================================================================
!
      subroutine Agrif_initworkspace()
      implicit none
# ifdef MPI
      integer ierr  
# endif          
# include "param.h"
# include "private_scratch.h"
# include "ncscrum.h"
# include "scalars.h"
#ifdef PISCES
# include "parameter.h"
#endif
#ifdef AUTOTILING
# include "autotiling.h"
#endif
      integer size_XI,size_ETA,se,sse, sz,ssz
!$AGRIF_DO_NOT_TREAT
      logical, SAVE :: MPIisinitialize = .FALSE.
!$AGRIF_END_DO_NOT_TREAT

      IF (Agrif_Root()) THEN
# include "dynparam.h"
#ifdef MPI
        IF (.Not.MPIisinitialize) THEN
           Call MPI_Init(ierr)
           MPIisinitialize = .TRUE.
        ENDIF
#endif
      ENDIF
#ifdef AUTOTILING
      If (Agrif_Nb_Step() == 0) Then
        NSUB_X = 1
        NSUB_E = NPP
      EndIf
#endif
# include "dynderivparam.h"
# ifdef MPI
      Call MPI_Setup(ierr)
# endif      
      return
      end
!
!====================================================================
!                   subroutine Agrif_initvalues
!====================================================================
!
      subroutine Agrif_initvalues()
      use AGRIF_UTIL
      implicit none
      integer tile, ierr, trd, subs
# ifdef SEDIMENT
     & , Agrif_lev_sedim
# endif
# include "param.h"
# include "private_scratch.h"
# include "ncscrum.h"
# include "scalars.h"
# include "grid.h"
# include "ocean2d.h"
# include "zoom.h"
#ifdef SOLVE3D
# include "coupling.h"
# include "ocean3d.h"
#endif
#ifdef AUTOTILING
# include "autotiling.h"
#endif

      integer size_XI,size_ETA,se,sse, sz,ssz
      real res1,res2,res3,res4,res5,res6,cff1,cff2
      integer i,j
      integer ipr,jpr
      real,dimension(:,:),pointer :: hparent,umaskparent,rmaskparent
      real htest(GLOBAL_2D_ARRAY)
      real tind(5)
      integer j1,i1,i2,j2,ipu,jpu,ipv,jpv
      real gradh(GLOBAL_2D_ARRAY)
      real hbis(GLOBAL_2D_ARRAY)
      real hcur(GLOBAL_2D_ARRAY)
      real hmean(GLOBAL_2D_ARRAY)
      real hbis2(GLOBAL_2D_ARRAY)
      real hbis3(GLOBAL_2D_ARRAY)      
      integer nbloop
      integer nbconstraint
      integer n1,n2
      real valconstraints(100000,2)
      integer iprmin,iprmax,jprmin,jprmax
      real,dimension(:,:),allocatable :: hconstraint,constraints,
     &gradconstraints
      external hinterp, updateh   
!$AGRIF_DO_NOT_TREAT
       integer isens
       common/interph/isens
!$AGRIF_END_DO_NOT_TREAT     

# ifdef MPI
#  define LOCALLM Lmmpi
#  define LOCALMM Mmmpi
# else
#  define LOCALLM Lm
#  define LOCALMM Mm
# endif 
      
# include "dynderivparam.h"

#ifdef AUTOTILING
      call init_auto_tiling
#endif

# ifdef SEDIMENT
      Agrif_lev_sedim=Agrif_Nb_Fine_Grids()
# endif
# ifdef MPI
      call MPI_Setup (ierr)
      if (ierr.ne.0) goto 100                            !--> ERROR
# endif

      call read_inp (ierr)           ! Read in tunable model
      if (ierr.ne.0) goto 100        ! parameters.
      call init_scalars (ierr)       ! Also initialize global
      if (ierr.ne.0) goto 100        ! scalar variables.

#ifdef PISCES
      call trclsm
      call get_dust
#endif

# ifdef SEDIMENT
      if (Agrif_Fixed().EQ.Agrif_lev_sedim) then
        call init_sediment           ! Read sediment initial values and
                                     ! parameters from sediment.in file
      endif
# endif

C$OMP PARALLEL DO PRIVATE(tile)
      do tile=0,NSUB_X*NSUB_E-1      ! Create parallel threads;
c        call start_timers()          ! start timers for each thread;
        call init_arrays (tile)      ! initialize (FIRST-TOUCH) model 
      enddo                          ! global arrays (most of them 
CR      write(*,*) '-11' MYID        ! are just set to to zero).

# ifdef ANA_GRID
C$OMP PARALLEL DO PRIVATE(tile)      ! Set horizontal curvilinear 
      do tile=0,NSUB_X*NSUB_E-1      ! grid and model bathymetry 
        call ana_grid (tile)         ! (analyticaly or read from 
      enddo                          ! GRID NetCDF file).
# else
                                     ! Also read Land/Sea mask
      call get_grid                  ! from GRID NetCDF file). 
      if (may_day_flag.ne.0) goto 99     !-->  EXIT
# endif
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
                                     ! Set up vertical S-coordinate
# ifdef SOLVE3D
      call set_scoord                ! and fast-time averaging  
      call set_weights               ! weights for coupling of 
                                     ! split-explicit baroropic mode.
# endif
CR      write(*,*) ' -8' MYID 
# ifdef SOLVE3D
C$OMP PARALLEL DO PRIVATE(tile)      ! Create three-dimensional
      do tile=0,NSUB_X*NSUB_E-1      ! S-coordinate system, which
        call set_depth (tile)        ! may be neded by ana_ninitial
      enddo                          ! (here it is assumed that free
CR      write(*,*) ' -7' MYID        ! surface zeta=0).
C$OMP PARALLEL DO PRIVATE(tile)      ! Create three-dimensional
      do tile=0,NSUB_X*NSUB_E-1      ! S-coordinate system, which
        call grid_stiffness (tile) 
      enddo                          ! (here it is assumed that free
# endif
# ifdef ANA_INITIAL
      if (nrrec.eq.0) then
C$OMP PARALLEL DO PRIVATE(tile)      ! Set initial conditions
        do tile=0,NSUB_X*NSUB_E-1    ! for primitive variables
          call ana_initial (tile)    ! (analytically or read
        enddo                        ! from initial conditions
      else                           ! NetCDF file).
# endif
        call get_initial
C$OMP PARALLEL DO PRIVATE(tile)      ! Set analytical initial 
        do tile=0,NSUB_X*NSUB_E-1    ! conditions for tracers
          call ana_initracer (tile)  ! if not found in initial
        enddo                        ! conditions NetCDF file). 
# ifdef ANA_INITIAL
      endif
# endif
#if defined BIOLOGY && defined PISCES
C$OMP PARALLEL DO PRIVATE(tile)      ! Set analytical initial
        do tile=0,NSUB_X*NSUB_E-1    ! conditions for tracers
          call  pisces_ini (tile)  ! if not found in initial
        enddo                        ! conditions NetCDF file).
#endif
CR      write(*,*) ' -6' MYID 
      if (may_day_flag.ne.0) goto 99     !-->  EXIT

# ifdef SOLVE3D
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
# endif
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
# if defined TNUDGING || defined ZNUDGING || defined SPONGE \
      || (defined BBL && defined ANA_BSEDIM)
C$OMP PARALLEL DO PRIVATE(tile)
      do tile=0,NSUB_X*NSUB_E-1
#  if defined TNUDGING || defined ZNUDGING || defined SPONGE
        call set_nudgcof_fine (tile)
#  endif
#  if defined BBL && defined ANA_BSEDIM
        if (Agrif_Fixed().EQ.Agrif_lev_sedim) then
          call ana_bsedim (tile)
        endif
#  endif
#  if defined SEDIMENT && defined ANA_SEDIMENT
        if (Agrif_Fixed().EQ.Agrif_lev_sedim) then
          call ana_sediment (tile)
        endif
#  endif
      enddo
# endif
CR      write(*,*) ' -2' MYID 
!
!  Read bottom sediment grain size [m] and density [kg/m^3] from
!           input NetCDF file;
!
# if defined BBL && !defined ANA_BSEDIM && !defined SEDIMENT
      if (Agrif_Fixed().EQ.Agrif_lev_sedim) then
          call get_bsedim
      endif
# endif
# if defined SEDIMENT && !defined ANA_SEDIMENT
      if (Agrif_Fixed().EQ.Agrif_lev_sedim) then
          call get_sediment
      endif
# endif
      call get_vbc
# if defined BBL && !defined ANA_WWAVE
      if (Agrif_Fixed().EQ.Agrif_lev_sedim) then
        call get_wwave
      endif
# endif /* BBL && !ANA_WWAVE*/
CR      write(*,*) ' -1' MYID 
      if (may_day_flag.ne.0) goto 99     !-->  EXIT

      if (ldefhis .and. wrthis(indxTime)) call wrt_his
CR      write(*,*) '  0' MYID
      if (may_day_flag.ne.0) goto 99     !-->  EXIT

 99   continue
 100  continue
 
      next_kstp=kstp
      time_start=time

      U2DTimeindex = -1
      V2DTimeindex = -1
      ZetaTimeindex = -1
# ifdef SOLVE3D
      Ttimeindex = -1
      Utimeindex = -1
      Vtimeindex = -1
# endif

      TspongeTimeindex = -1
      UVspongeTimeindex = -1

      Call Agrif_Set_type(zeta,(/2,2,0/),(/1,1,0/))
      Call Agrif_Set_raf(zeta,(/'x','y','N'/))
#ifdef SOLVE3D
      Call Agrif_Set_type(DU_avg1,(/1,2,0/),(/1,1,0/))
      Call Agrif_Set_type(DV_avg1,(/2,1,0/),(/1,1,0/))
      Call Agrif_Set_raf(DU_avg1,(/'x','y','N'/))
      Call Agrif_Set_raf(DV_avg1,(/'x','y','N'/))
      
      Call Agrif_Set_type(DU_avg2,(/1,2/),(/1,1/))
      Call Agrif_Set_type(DV_avg2,(/2,1/),(/1,1/))
      Call Agrif_Set_raf(DU_avg2,(/'x','y'/))
      Call Agrif_Set_raf(DV_avg2,(/'x','y'/))

      Call Agrif_Set_type(u,(/1,2,0,0/),(/1,1,0,0/))
      Call Agrif_Set_type(v,(/2,1,0,0/),(/1,1,0,0/))
      Call Agrif_Set_raf(u,(/'x','y','N','N'/))
      Call Agrif_Set_raf(v,(/'x','y','N','N'/))


      Call Agrif_Set_type(Hz,(/1,2,0,0/),(/1,1,0,0/))
      Call Agrif_Set_type(Hz_bak,(/2,1,0,0/),(/1,1,0,0/))
      Call Agrif_Set_raf(Hz,(/'x','y','N','N'/))
      Call Agrif_Set_raf(Hz_bak,(/'x','y','N','N'/))
      
      Call Agrif_Set_type(t,(/2,2,0,0,0/),(/1,1,0,0,0/))
      Call Agrif_Set_raf(t,(/'x','y','N','N','N'/))
      
      Call Agrif_Set_type(Zt_avg1,(/2,2/),(/1,1/))
      Call Agrif_Set_raf(Zt_avg1,(/'x','y'/))

      Call Agrif_Set_type(Zt_avg2,(/2,2,0/),(/1,1,0/))
      Call Agrif_Set_raf(Zt_avg2,(/'x','y','N'/))
            
      Call Agrif_Set_type(h,(/2,2/),(/1,1/))
      Call Agrif_Set_raf(h,(/'x','y'/))  

#  ifdef MASKING      
      Call Agrif_Set_type(rmask,(/2,2/),(/1,1/))
      Call Agrif_Set_raf(rmask,(/'x','y'/))          
#  endif            

#else
      Call Agrif_Set_type(ubar,(/1,2,0/),(/1,1,0/))
      Call Agrif_Set_type(vbar,(/2,1,0/),(/1,1,0/))
      Call Agrif_Set_raf(ubar,(/'x','y','N'/))
      Call Agrif_Set_raf(vbar,(/'x','y','N'/))
#endif
!
      Call Agrif_Set_bcinterp(zeta,AGRIF_linear)
      Call Agrif_Set_bc(zeta,(/-1,0/))

      Call Agrif_Set_bcinterp(h,AGRIF_linear)
      Call Agrif_Set_interp(h,AGRIF_linear)
      Call Agrif_Set_bc(h,(/0,0/))
      Call Agrif_Set_UpdateType(h,update=
     &   Agrif_Update_Full_Weighting)

#ifdef MASKING      
      Call Agrif_Set_UpdateType(rmask,update=
     &   Agrif_Update_Average)
#endif
                      
#ifdef SOLVE3D

      Call Agrif_Set_bcinterp(Zt_avg1,AGRIF_linear)
      Call Agrif_Set_bc(Zt_avg1,(/-1,0/))
      
      Call Agrif_Set_bcinterp(u,interp1=agrif_linear
     &  ,interp2=agrif_ppm)
      Call Agrif_Set_bcinterp(v,interp1=agrif_ppm
     &  ,interp2=agrif_linear)
      Call Agrif_Set_bc(u,(/0,0/))
      Call Agrif_Set_bc(v,(/0,0/))
      
      
      Call Agrif_Set_bcinterp(t,AGRIF_linear)
      Call Agrif_Set_bc(t,(/-1,0/))

      Call Agrif_Set_bcinterp(DU_avg2,interp1=agrif_linear
     &   ,interp2=agrif_ppm)
      Call Agrif_Set_bcinterp(DV_avg2,interp1=agrif_ppm
     &  ,interp2=agrif_linear)
      Call Agrif_Set_bc(DU_avg2,(/0,0/))
      Call Agrif_Set_bc(DV_avg2,(/0,0/))

      Call Agrif_Set_bcinterp(DU_avg1,interp1=agrif_linear
     &   ,interp2=agrif_ppm)
      Call Agrif_Set_bcinterp(DV_avg1,interp1=agrif_ppm
     &  ,interp2=agrif_linear)
      Call Agrif_Set_bc(DU_avg1,(/0,0/))
      Call Agrif_Set_bc(DV_avg1,(/0,0/))
      Call Agrif_Set_type(updateTprof,(/2,2,0,0/),(/1,1,0,0/))
      Call Agrif_Set_type(tsponge,(/2,2,0,0/),(/1,1,0,0/))      
      Call Agrif_Set_raf(updateTprof,(/'x','y','N','N'/)) 
      Call Agrif_Set_raf(tsponge,(/'x','y','N','N'/))            
      Call Agrif_Set_type(Huon,(/1,2,0/),(/1,1,0/))
      Call Agrif_Set_raf(Huon,(/'x','y','N'/))
      Call Agrif_Set_type(Hvom,(/2,1,0/),(/1,1,0/))
      Call Agrif_Set_raf(Hvom,(/'x','y','N'/))  
      
      Call Agrif_Set_type(usponge,(/1,2,0/),(/1,1,0/))
      Call Agrif_Set_raf(usponge,(/'x','y','N'/))
      Call Agrif_Set_type(vsponge,(/2,1,0/),(/1,1,0/))
      Call Agrif_Set_raf(vsponge,(/'x','y','N'/))      
          
      Call Agrif_Set_bcinterp(updateTprof,agrif_linear)
      Call Agrif_Set_bc(updateTprof,(/-1,0/))     

      Call Agrif_Set_bcinterp(tsponge,agrif_linear)
      Call Agrif_Set_bc(tsponge,(/-1,0/))           
      
      Call Agrif_Set_bcinterp(Huon,interp1=agrif_linear
     &    ,interp2=agrif_ppm)
      Call Agrif_Set_bc(Huon,(/0,0/))     
      
      Call Agrif_Set_bcinterp(Hvom,interp1=agrif_ppm
     &   ,interp2=agrif_linear)
      Call Agrif_Set_bc(Hvom,(/0,0/))

      Call Agrif_Set_bcinterp(usponge,interp1=agrif_linear
     &    ,interp2=agrif_ppm)
      Call Agrif_Set_bc(usponge,(/0,0/))     
      
      Call Agrif_Set_bcinterp(vsponge,interp1=agrif_ppm
     &   ,interp2=agrif_linear)
      Call Agrif_Set_bc(vsponge,(/0,0/))
#else
      Call Agrif_Set_bcinterp(ubar,AGRIF_linearconserv)
      Call Agrif_Set_bcinterp(vbar,AGRIF_linearconserv)
      Call Agrif_Set_bc(ubar,(/0,2/))
      Call Agrif_Set_bc(vbar,(/0,2/))
#endif /* SOLVE3D */

#ifdef AGRIF_2WAY
      Call Agrif_Set_UpdateType(updateTprof,update=
     &   Agrif_Update_full_weighting)

      Call Agrif_Set_UpdateType(tsponge,update=
     &   Agrif_Update_full_weighting)
     
      Call Agrif_Set_UpdateType(Huon,
     &update1=Agrif_Update_full_weighting,
     &update2=
     &Agrif_Update_full_weighting)

      Call Agrif_Set_UpdateType(DU_avg2,update=
     &Agrif_Update_full_weighting)

      Call Agrif_Set_UpdateType(DU_avg1,update=
     &Agrif_Update_full_weighting)

      Call Agrif_Set_UpdateType(Hvom,update1=
     &Agrif_Update_full_weighting,update2=
     &Agrif_Update_full_weighting)

      Call Agrif_Set_UpdateType(DV_avg2,update=
     &Agrif_Update_full_weighting)   

      Call Agrif_Set_UpdateType(DV_avg1,update=
     &Agrif_Update_full_weighting) 
      
      Call Agrif_Set_UpdateType(Zt_avg1,update=
     &Agrif_Update_full_weighting)  

      Call Agrif_Set_UpdateType(Zt_avg2,update=
     &Agrif_Update_full_weighting)

!       Call Agrif_Set_UpdateType(updateTprof,update=
!      &   Agrif_Update_Average)
! 
!       Call Agrif_Set_UpdateType(Huon,
!      &update1=Agrif_Update_Average,
!      &update2=
!      &Agrif_Update_Average)
! 
!       Call Agrif_Set_UpdateType(DU_avg2,update=
!      &Agrif_Update_Average)
! 
!       Call Agrif_Set_UpdateType(DU_avg1,update=
!      &Agrif_Update_Average)
! 
!       Call Agrif_Set_UpdateType(Hvom,update1=
!      &Agrif_Update_Average,update2=
!      &Agrif_Update_Average)
! 
!       Call Agrif_Set_UpdateType(DV_avg2,update=
!      &Agrif_Update_Average)   
! 
!       Call Agrif_Set_UpdateType(DV_avg1,update=
!      &Agrif_Update_Average) 
!       
!       Call Agrif_Set_UpdateType(Zt_avg1,update=
!      &Agrif_Update_Average)  
! 
!       Call Agrif_Set_UpdateType(Zt_avg2,update=
!      &Agrif_Update_Average)


!       Call Agrif_Set_UpdateType(updateTprof,update=
!      &   Agrif_Update_Average)
! 
!       Call Agrif_Set_UpdateType(Huon,
!      &update1=Agrif_Update_Average,
!      &update2=Agrif_Update_Average)
! 
!       Call Agrif_Set_UpdateType(DU_avg2,
!      &update1=Agrif_Update_Average,
!      &update2=Agrif_Update_Average)
! 
!       Call Agrif_Set_UpdateType(DU_avg1,
!      &update1=Agrif_Update_Average,
!      &update2=Agrif_Update_Average)
! 
!       Call Agrif_Set_UpdateType(Hvom,update1=
!      &Agrif_Update_Average,update2=
!      &Agrif_Update_Average)
! 
!       Call Agrif_Set_UpdateType(DV_avg2,update1=
!      &Agrif_Update_Average,update2=
!      &Agrif_Update_Average)
! 
!       Call Agrif_Set_UpdateType(DV_avg1,update1=
!      &Agrif_Update_Average,update2=
!      &Agrif_Update_Average)
! 
!       Call Agrif_Set_UpdateType(Zt_avg1,update=
!      &Agrif_Update_Average)
! 
!       Call Agrif_Set_UpdateType(Zt_avg2,update=
!      &Agrif_Update_Average)


#endif
  
      iif = nfast+1
      iic = ntstart

      nbcoarse = 1
      nbstep3d = 0
      
      TTimesponge = 1
      UVTimesponge = 1

      usponge = 0.
      vsponge = 0.
      
      myfx = 0.
      myfy = 0.


#if defined AGRIF_2WAY   

#ifdef MASKING
      Agrif_UseSpecialValueInUpdate = .TRUE.
      Agrif_SpecialValueFineGrid = 0.
      
      Call Agrif_Update_Variable(rmask,rmask)
      
      Agrif_UseSpecialValueInUpdate = .FALSE.   

      Call Agrif_ChildGrid_To_ParentGrid()
      Call ResetMask()
      Call Agrif_ParentGrid_To_ChildGrid()
#endif 
   
      Agrif_UseSpecialValueInUpdate = .TRUE.
      Agrif_SpecialValueFineGrid = 0.   
      Call Agrif_Update_Variable(h,h,locupdate=(/1,0/),
     &     procname=updateh)
      Agrif_UseSpecialValueInUpdate = .FALSE.

      isens = 1
      Call Agrif_Bc_Variable(hbis,DU_avg2,calledweight=1.,
     &procname=hinterp)

#  ifdef AGRIF_OBC_WEST
#   ifdef MPI           
        if (.not.WEST_INTER) then
#   endif      
      Do j=0,LOCALMM+1
        h(0,j) = 2.*hbis(1,j)-h(1,j)
      enddo
#   ifdef MPI            
        endif
#   endif 
#  endif 

#  ifdef AGRIF_OBC_EAST
#   ifdef MPI           
        if (.not.EAST_INTER) then
#   endif      
      Do j=0,LOCALMM+1
        h(LOCALLM+1,j) = 2.*hbis(LOCALLM+1,j)-h(LOCALLM,j)
      enddo
#   ifdef MPI            
        endif
#   endif 
#  endif       
          
      isens = 2
      Call Agrif_Bc_Variable(hbis,DV_avg2,calledweight=1.,
     &procname=hinterp)
      
#  ifdef AGRIF_OBC_SOUTH
#   ifdef MPI           
        if (.not.SOUTH_INTER) then
#   endif       
      Do i=0,LOCALLM+1
        h(i,0) = 2.*hbis(i,1)-h(i,1)
      enddo
#   ifdef MPI            
        endif
#   endif 
#  endif 

#  ifdef AGRIF_OBC_NORTH
#   ifdef MPI           
        if (.not.NORTH_INTER) then
#   endif       
      Do i=0,LOCALLM+1
        h(i,LOCALMM+1) = 2.*hbis(i,LOCALMM+1)-h(i,LOCALMM)
      enddo
#   ifdef MPI            
        endif
#   endif 
#  endif 

      Agrif_UseSpecialValueInUpdate = .TRUE.
      Agrif_SpecialValueFineGrid = 0.   
      Call Agrif_Update_Variable(h,h,locupdate=(/1,0/),
     &     procname=updateh)
      Agrif_UseSpecialValueInUpdate = .FALSE.           

#endif     <-  AGRIF_2WAY
     
      return
      end
      
#ifdef MASKING
       subroutine ResetMask()
#  include "param.h"
#  include "grid.h"
#  include "scalars.h"
       integer i,j
       
       do j=0,Mm+1
       do i=1,Lm+1
        umask(i,j) = rmask(i,j)*rmask(i-1,j)
       enddo
       enddo

       do j=1,Mm+1
       do i=0,Lm+1
        vmask(i,j) = rmask(i,j)*rmask(i,j-1)
       enddo
       enddo
       
       do j=1,Mm+1
       do i=1,Lm+1
          pmask(i,j)=rmask(i,j)*rmask(i-1,j)*rmask(i,j-1)
     &                                      *rmask(i-1,j-1)
          if (gamma2.lt.0.) pmask(i,j)=2.-pmask(i,j)
       enddo
       enddo
                 
       end subroutine ResetMask
#endif      

      Subroutine Updateh(tabres,i1,i2,j1,j2,before)
      implicit none
#  include "param.h"
#  include "grid.h"
#  include "ocean2d.h"
#  include "coupling.h"
#  include "scalars.h"
#  include "zoom.h"
      integer i1,i2,j1,j2
      real tabres(i1:i2,j1:j2)
      logical before
      
      integer i,j  
       real tabtemp(i1:i2,j1:j2) 
              
      IF (before) THEN
 
           tabres(i1:i2,j1:j2) = h(i1:i2,j1:j2)
#ifdef MASKING
     &       *rmask(i1:i2,j1:j2)
#endif
         
       ELSE

c         h(i1+1:i2-1,j1+1:j2-1) = tabres(i1+1:i2-1,j1+1:j2-1)
#ifdef MASKING
c     &             *rmask(i1+1:i2-1,j1+1:j2-1)
#endif
       do j=j1+1,j2-1
       do i=i1+1,i2-1
       h(i,j)=max(tabres(i,j),hc)
       enddo
       enddo
	 	 
       ENDIF
      
      return
      end
      
      subroutine hinterp(tabres,i1,i2,j1,j2)
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean2d.h"
# include "scalars.h"
!$AGRIF_DO_NOT_TREAT
       integer isens
       common/interph/isens
!$AGRIF_END_DO_NOT_TREAT

       integer i1,i2,j1,j2
       real tabres(i1:i2,j1:j2)
       integer i,j
       
       IF (isens == 1) THEN
       do j=j1,j2
       do i=i1,i2
         tabres(i,j) = 0.5*(h(i-1,j)+h(i,j))
       enddo
       enddo       
       ELSE
       do j=j1,j2
       do i=i1,i2
         tabres(i,j) = 0.5*(h(i,j-1)+h(i,j))
       enddo
       enddo       
       ENDIF  
       
      return
      end      
       
!
!====================================================================
!                   subroutine Agrif_transfer_floatsp2c
!====================================================================
!
# if defined FLOATS || defined STATIONS
        subroutine Agrif_transfer_floatsp2c(rank,xfloat,yfloat,delta)
        use Agrif_Util
!----------------------------------------------
! checks if a float must be transferred from one grid to the child grid
! margin is the sum of the distance between rho and velocity points (0.5)
! plus a safe value to ensure that the float can stay inside the child
! at least coeffreft time steps.
!----------------------------------------------
        type(Agrif_grid), Pointer :: childgrid
        type(Agrif_pgrid), Pointer :: parcours

        real xfloat,yfloat,margin,delta,tmp1,tmp2
        integer rank,timeindex

        parcours=>Agrif_Curgrid%child_grids
        do while (associated(parcours))
          childgrid=>parcours%gr
          margin=0.5+delta
          tmp1=childgrid%spacerefx*(xfloat+0.5-childgrid%ix)+0.5
          if ((tmp1 .ge. margin).and.
     &        (tmp1 .le. (childgrid%nbx+1-margin))) then
            tmp2=childgrid%spacerefy*(yfloat+0.5-childgrid%iy)+0.5
            if ((tmp2 .ge. margin).and.
     &          (tmp2 .le. (childgrid%nby+1-margin))) then
              rank=childgrid%fixedrank
              xfloat=tmp1
              yfloat=tmp2
              exit
            endif
          endif
          parcours=>parcours%next
        enddo
        end subroutine Agrif_transfer_floatsp2c
!
!====================================================================
!                   subroutine Agrif_Invloc
!====================================================================
!
        subroutine Agrif_laststep(logic)
        use Agrif_Util

!----------------------------------------------
! determines whether this time step is the last one before
! a ROOT time step.
!----------------------------------------------
        Implicit none
        logical logic
        type(Agrif_grid), Pointer :: parcours
        integer tmp, tmp2,rhot,stepmod

        logic=.false.
        tmp=0
        tmp2=1
        parcours=>Agrif_Curgrid

        Do While (associated(parcours%parent))
          rhot=1
          rhot=max(rhot,parcours%timerefx,parcours%timerefy)
          stepmod=mod(parcours%ngridstep,int(rhot))
          tmp2=tmp2*rhot
          tmp=stepmod+rhot*tmp
          parcours=>parcours%parent
        End Do
        tmp2=tmp2-1
        if (tmp2 .eq. tmp) logic=.true.
        return
        end subroutine

# endif /* FLOATS || STATIONS */
!
!====================================================================
!                   subroutine Agrif_Invloc
!====================================================================
!
# if defined MPI
        Subroutine Agrif_Invloc(indloc,proc,dir,indglob)
        implicit none
        integer indloc, proc, dir, indglob
# include "param.h"        
        If (dir == 1) Then
           indglob = indloc + iminmpi-1          
        Else If (dir == 2) Then
           indglob = indloc + jminmpi-1
        Else 
           indglob = indloc
        End If
        Return
        End
# endif
!

!



      SUBROUTINE Agrif_detect(g,sizex,sizey,sizez)
!
!     Modules used:
!
      Use Agrif_Types
!
!
!     Declarations:
!
!
!     Variables
!
      Type(Agrif_Grid),pointer   :: g        ! Pointer on the current grid
      Integer :: sizex,sizey,sizez
!
!     Begin
!
!

!
      Return
      End Subroutine Agrif_detect


#else  /* AGRIF */
!
!====================================================================
!                   nothing...
!====================================================================
!
      subroutine zoom_empty
      return
      end
#endif  /* AGRIF */
