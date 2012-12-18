! $Id$
!
!======================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
! 
! This routine belongs to the specific ROMS_AGRIF package.
! 
! ROMS_AGRIF website : http://roms.mpl.ird.fr
!======================================================================
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
#ifdef AGRIF_ADAPTIVE
      If (Agrif_Nb_step() == 0) Then
        Call Agrif_Set_coeffref_x(3)
        Call Agrif_Set_coeffref_y(3)
        Call Agrif_Set_coeffreft_x(3)
        Call Agrif_Set_coeffreft_y(3) 
        Call Agrif_Set_Minwidth(18)
        Call Agrif_Set_Regridding(300*(nfast+2))
      EndIf
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
#ifdef PISCES
# include "parameter.h"
#endif
#ifdef AUTOTILING
# include "autotiling.h"
#endif

      integer size_XI,size_ETA,se,sse, sz,ssz
      real res1,res2,res3,res4,res5,res6,cff1,cff2
      integer i,j
      integer ipr,jpr,itrc
      real,dimension(:,:),pointer :: hparent,umaskparent,rmaskparent
      real htest(GLOBAL_2D_ARRAY)
      real tind(5)
      integer ipu,jpu,ipv,jpv,k
      real gradh(GLOBAL_2D_ARRAY)
      real hbis(GLOBAL_2D_ARRAY)
      real hcur(GLOBAL_2D_ARRAY)
      real hmean(GLOBAL_2D_ARRAY)
      real hbis2(GLOBAL_2D_ARRAY)
      real hbis3(GLOBAL_2D_ARRAY)      
      real CF(GLOBAL_2D_ARRAY,0:N)
      real DC(GLOBAL_2D_ARRAY,0:N)
      integer nbloop
      integer nbconstraint
      integer n1,n2
      real valconstraints(100000,2)
      integer iprmin,iprmax,jprmin,jprmax
      real,dimension(:,:),allocatable :: hconstraint,constraints,
     &gradconstraints
      real,dimension(:,:,:,:),allocatable :: text
      real,dimension(:,:),allocatable :: tabtemp2d
      real,dimension(:,:,:),allocatable :: tabtemp3d   
      integer :: irhot   
      external hinterp, updateh   
      external initzeta,initubar,initvbar
      external initu,initv,initt

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
     
#if defined AGRIF_UPDATE_FULLWEIGHTING || defined AGRIF_UPDATE_MIX        
      Call Agrif_Set_UpdateType(h,update=
     &   Agrif_Update_Full_Weighting)
#endif

#if defined AGRIF_UPDATE_MIX_HIGH
      Call Agrif_Set_UpdateType(h,update=
     &   Agrif_Update_4rdorder)
#endif

#if defined AGRIF_UPDATE_AVERAGE || defined AGRIF_UPDATE_MIX_LOW
      Call Agrif_Set_UpdateType(h,update=
     &   Agrif_Update_Average)
#endif


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

      call declare_zoom_variables()

#ifdef AGRIF_2WAY
      Call Agrif_Set_UpdateType(updateTprof,update=
     &   Agrif_Update_full_weighting)

      Call Agrif_Set_UpdateType(tsponge,update=
     &   Agrif_Update_full_weighting)
     
      Call Agrif_Set_UpdateType(Huon,
     &update1=Agrif_Update_full_weighting,
     &update2=
     &Agrif_Update_full_weighting)

#ifdef AGRIF_UPDATE_FULLWEIGHTING
      Call Agrif_Set_UpdateType(DU_avg2,update=
     &Agrif_Update_full_weighting)
#endif

#ifdef AGRIF_UPDATE_AVERAGE
      Call Agrif_Set_UpdateType(DU_avg2,
     &update=Agrif_Update_Average)
#endif

#ifdef AGRIF_UPDATE_MIX
      Call Agrif_Set_UpdateType(DU_avg2,
     &update1=Agrif_Update_Average,
     &update2=Agrif_Update_Full_Weighting)
#endif

#ifdef AGRIF_UPDATE_MIX_HIGH
      Call Agrif_Set_UpdateType(DU_avg2,
     &update1=Agrif_Update_Full_Weighting,
     &update2=Agrif_Update_4rdorder)
#endif

#ifdef AGRIF_UPDATE_MIX_LOW
      Call Agrif_Set_UpdateType(DU_avg2,
     &update1=Agrif_Update_Copy,
     &update2=Agrif_Update_Average)
#endif

      Call Agrif_Set_UpdateType(DU_avg1,update=
     &Agrif_Update_full_weighting)

      Call Agrif_Set_UpdateType(Hvom,update1=
     &Agrif_Update_full_weighting,update2=
     &Agrif_Update_full_weighting)

#ifdef AGRIF_UPDATE_FULLWEIGHTING
      Call Agrif_Set_UpdateType(DV_avg2,update=
     &Agrif_Update_full_weighting)
#endif

#ifdef AGRIF_UPDATE_FULLWEIGHTING
      Call Agrif_Set_UpdateType(DV_avg2,update=
     &Agrif_Update_Average)
#endif

#ifdef AGRIF_UPDATE_MIX     
      Call Agrif_Set_UpdateType(DV_avg2,
     &update1=Agrif_Update_Full_Weighting,
     &update2=Agrif_Update_Average)
#endif

#ifdef AGRIF_UPDATE_MIX_HIGH    
      Call Agrif_Set_UpdateType(DV_avg2,
     &update1=Agrif_Update_4rdorder,
     &update2=Agrif_Update_Full_Weighting)
#endif

#ifdef AGRIF_UPDATE_MIX_LOW    
      Call Agrif_Set_UpdateType(DV_avg2,
     &update1=Agrif_Update_Average,
     &update2=Agrif_Update_Copy)
#endif

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
#if !defined AGRIF_ADAPTIVE
        call get_initial
#else    
       
        allocate(tabtemp2d(GLOBAL_2D_ARRAY))
        allocate(tabtemp3d(GLOBAL_2D_ARRAY,1:N))

        call Agrif_Declare_Variable((/2,2/),(/1,1/),(/'x','y'/),lbound(Zt_avg1),
     &   ubound(Zt_avg1),zetaid,torestore=.true.)
        call Agrif_Declare_Variable((/1,2/),(/1,1/),(/'x','y'/),lbound(DU_avg2),
     &   ubound(DU_avg2),ubarid,torestore=.true.)
        call Agrif_Declare_Variable((/2,1/),(/1,1/),(/'x','y'/),lbound(DV_avg2),
     &   ubound(DV_avg2),vbarid,torestore=.true.)  
     
        allocate(text(GLOBAL_2D_ARRAY,N,NT))  
             
        call Agrif_Declare_Variable((/2,2,0,0/),(/1,1,0,0/),
     &    (/'x','y','N','N'/),lbound(text),ubound(text),tid,
     &    torestore=.true.)
        call Agrif_Declare_Variable((/1,2,0/),(/1,1,0/),
     &   (/'x','y','N'/),lbound(Huon),ubound(Huon),uid,
     &   torestore=.true.)
        call Agrif_Declare_Variable((/2,1,0/),(/1,1,0/),
     &   (/'x','y','N'/),lbound(Hvom),ubound(Hvom),vid,
     &   torestore=.true.) 
             
        call Agrif_Set_bc(zetaid,(/-1,0/))     
        call Agrif_Set_bc(ubarid,(/0,0/))
        call Agrif_Set_bc(vbarid,(/0,0/))
        call Agrif_Set_bc(tid,(/-1,0/))     
        call Agrif_Set_bc(uid,(/0,0/))
        call Agrif_Set_bc(vid,(/0,0/))
        
        call Agrif_Set_bcinterp(zetaid,interp=Agrif_ppm)        
        call Agrif_Set_bcinterp(ubarid,interp1=Agrif_linear,
     &                                 interp2=Agrif_ppm)
        call Agrif_Set_bcinterp(vbarid,interp1=Agrif_ppm,
     &                                 interp2=Agrif_linear)
        call Agrif_Set_bcinterp(tid,interp=Agrif_ppm)
        call Agrif_Set_bcinterp(uid,interp1=Agrif_linear,
     &                                 interp2=Agrif_ppm)
        call Agrif_Set_bcinterp(vid,interp1=Agrif_ppm,
     &                                 interp2=Agrif_linear)
        
        call Agrif_Set_interp(zetaid,interp=Agrif_ppm)
        call Agrif_Set_interp(ubarid,interp1=Agrif_linear,
     &                                 interp2=Agrif_ppm)
        call Agrif_Set_interp(vbarid,interp1=Agrif_ppm,
     &                                 interp2=Agrif_linear)
        call Agrif_Set_interp(tid,interp=Agrif_ppm)
        call Agrif_Set_interp(uid,interp1=Agrif_linear,
     &                                 interp2=Agrif_ppm)
        call Agrif_Set_interp(vid,interp1=Agrif_ppm,
     &                                 interp2=Agrif_linear)
     
        call Agrif_Init_Variable(tabtemp2d,zetaid,procname=initzeta)
        zeta(:,:,1) = tabtemp2d


        call Agrif_Init_Variable(tabtemp2d,ubarid,procname=initubar)
        DU_avg1(:,:,1) = tabtemp2d/Agrif_irhoy()
        call Agrif_Init_Variable(tabtemp2d,vbarid,procname=initvbar)
        DV_avg1(:,:,1) = tabtemp2d/Agrif_irhox()

        call Agrif_Init_Variable(text,tid,procname=initt)
        do itrc=1,NT
            got_tini(itrc) = .true.
            t(:,:,:,1,itrc)=text(:,:,:,itrc)
        enddo        
        call Agrif_Init_Variable(tabtemp3d,uid,procname=initu)
        u(:,:,:,1) = tabtemp3d
        call Agrif_Init_Variable(tabtemp3d,vid,procname=initv)
        v(:,:,:,1) = tabtemp3d
        
        deallocate(tabtemp2d,tabtemp3d,text)


        if (Agrif_Parent_Nb_Step() == 0) then
        call get_initial
        endif
#endif

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

#if defined AGRIF_ADAPTIVE
      if (Agrif_Parent_Nb_Step() /= 0) then
      do j=0,Mm+1
      do i=1,Lm+1
        DC(i,j,0)=0.
        CF(i,j,0)=0.
      enddo
      enddo
      
      do k=1,N
      do j=0,Mm+1
      do i=1,Lm+1
        DC(i,j,k) = 0.5*(Hz(i,j,k)+Hz(i-1,j,k))*on_u(i,j)
        DC(i,j,0)=DC(i,j,0)+DC(i,j,k)
        CF(i,j,0)=CF(i,j,0)+DC(i,j,k)*u(i,j,k,1)
      enddo
      enddo
      enddo
      
      do j=0,Mm+1
      do i=1,Lm+1
        DC(i,j,0)=1./DC(i,j,0)
        CF(i,j,0)=DC(i,j,0)*(CF(i,j,0)-DU_avg1(i,j,1))
        ubar(i,j,1) = DC(i,j,0)*DU_avg1(i,j,1)
      enddo
      enddo
      
      do k=N,1,-1
      do j=0,Mm+1
      do i=0,Lm+1
        u(i,j,k,1) = (u(i,j,k,1)-CF(i,j,0))
      enddo
      enddo
      enddo
      
      do j=1,Mm+1
      do i=0,Lm+1
        DC(i,j,0)=0.
        CF(i,j,0)=0.
      enddo
      enddo
      
      do k=1,N
      do j=1,Mm+1
      do i=0,Lm+1
        DC(i,j,k) = 0.5*(Hz(i,j,k)+Hz(i,j-1,k))*om_v(i,j)
        DC(i,j,0)=DC(i,j,0)+DC(i,j,k)
        CF(i,j,0)=CF(i,j,0)+DC(i,j,k)*v(i,j,k,1)
      enddo
      enddo
      enddo
      
      do j=1,Mm+1
      do i=0,Lm+1
        DC(i,j,0)=1./DC(i,j,0)
        CF(i,j,0)=DC(i,j,0)*(CF(i,j,0)-DV_avg1(i,j,1))
        vbar(i,j,1) = DC(i,j,0)*DV_avg1(i,j,1)
      enddo
      enddo
      
      do k=N,1,-1
      do j=1,Mm+1
      do i=0,Lm+1
        v(i,j,k,1) = (v(i,j,k,1)-CF(i,j,0))
      enddo
      enddo
      enddo     
          
      endif 
#endif
     
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
#  ifdef AGRIF_OBC_SOUTH
#   ifdef MPI        
            if (.not.SOUTH_INTER) then
#   endif
                U_south(0:LOCALLM+1,0:0,1:N,4)=
     &          u(0:LOCALLM+1,0:0,1:N,1)
     
                V_south(0:LOCALLM+1,1:1,1:N,4)=
     &          v(0:LOCALLM+1,1:1,1:N,1)
     
                T_south(0:LOCALLM+1,0:1,1:N,4,1:NT)=
     &          t(0:LOCALLM+1,0:1,1:N,1,1:NT)     
          
#   ifdef MPI           
            endif          
#   endif            
#  endif            
#  ifdef AGRIF_OBC_NORTH
#   ifdef MPI        
            if (.not.NORTH_INTER) then  
#   endif
                U_north(0:LOCALLM+1,LOCALMM+1:LOCALMM+1,1:N,4)=
     &          u(0:LOCALLM+1,LOCALMM+1:LOCALMM+1,1:N,1)
     
                V_north(0:LOCALLM+1,LOCALMM+1:LOCALMM+1,1:N,4)=
     &          v(0:LOCALLM+1,LOCALMM+1:LOCALMM+1,1:N,1) 
     
                T_north(0:LOCALLM+1,LOCALMM:LOCALMM+1,1:N,4,1:NT)=
     &          t(0:LOCALLM+1,LOCALMM:LOCALMM+1,1:N,1,1:NT)         
          
#   ifdef MPI                    
            endif  
#   endif            
#  endif            
#  ifdef AGRIF_OBC_WEST
#   ifdef MPI        
            if (.not.WEST_INTER) then
#   endif
                U_west(1:1,0:LOCALMM+1,1:N,4)=
     &          u(1:1,0:LOCALMM+1,1:N,1)
     
                V_west(0:0,0:LOCALMM+1,1:N,4)=
     &            v(0:0,0:LOCALMM+1,1:N,1)
     
                T_west(0:1,0:LOCALMM+1,1:N,4,1:NT)=
     &          t(0:1,0:LOCALMM+1,1:N,1,1:NT)     
                    
#   ifdef MPI            
            endif
#   endif            
#  endif            
#  ifdef AGRIF_OBC_EAST
#   ifdef MPI        
            if (.not.EAST_INTER) then
#   endif
                U_east(LOCALLM+1:LOCALLM+1,0:LOCALMM+1,1:N,4)=
     &          u(LOCALLM+1:LOCALLM+1,0:LOCALMM+1,1:N,1)
     
                V_east(LOCALLM+1:LOCALLM+1,0:LOCALMM+1,1:N,4)=
     &          v(LOCALLM+1:LOCALLM+1,0:LOCALMM+1,1:N,1)
     
                T_east(LOCALLM:LOCALLM+1,0:LOCALMM+1,1:N,4,1:NT)=
     &          t(LOCALLM:LOCALLM+1,0:LOCALMM+1,1:N,1,1:NT)     

          
#   ifdef MPI               
            endif  
#   endif                            
#  endif      
# endif

      TspongeTimeindex = -1
      UVspongeTimeindex = -1


  
      iif = -1
      
#ifdef AGRIF_ADAPTIVE
      ntstart = 1 + Agrif_irhot()*
     &   (Agrif_Parent(iic)-1)   
      time_start=Agrif_Parent(time_start)
     &+dt*Agrif_irhot()
     & *float(Agrif_Parent(iic)
     &-Agrif_Parent(ntstart))
#endif      
      iic = ntstart

      nbcoarse = 1
      nbstep3d = 0
      
      TTimesponge = 1
      UVTimesponge = 1

      usponge = 0.
      vsponge = 0.
      
      myfx = 0.
      myfy = 0.
      DU_avg1(:,:,4:5) = 0.
      DV_avg1(:,:,4:5) = 0.
      
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
      
        allocate(tabtemp2d(0:i2u,j1t:j2t))
      Call Agrif_Bc_Variable(tabtemp2d,ubarid,calledweight=1.,
     &procname=hinterp)
        hbis(0:i2u,j1t:j2t)=tabtemp2d
        deallocate(tabtemp2d)  
           
#  ifdef AGRIF_OBC_WEST
#   ifdef MPI           
        if (.not.WEST_INTER) then
#   endif      
      Do j=0,LOCALMM+1
        h(0,j) = 2.*hbis(1,j)-h(1,j)
        hinv(0,j) = 1./h(0,j)
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
        hinv(LOCALLM+1,j) = 1./h(LOCALLM+1,j)
      enddo
#   ifdef MPI            
        endif
#   endif 
#  endif       
          
      isens = 2
        allocate(tabtemp2d(i1t:i2t,0:j2v))
      Call Agrif_Bc_Variable(tabtemp2d,vbarid,calledweight=1.,
     &procname=hinterp)
        hbis(i1t:i2t,0:j2v)=tabtemp2d
        deallocate(tabtemp2d)
              
#  ifdef AGRIF_OBC_SOUTH
#   ifdef MPI           
        if (.not.SOUTH_INTER) then
#   endif       
      Do i=0,LOCALLM+1
        h(i,0) = 2.*hbis(i,1)-h(i,1)
        hinv(i,0) = 1./h(i,0)
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
        hinv(i,LOCALMM+1) = 1./h(i,LOCALMM+1)
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

      Call Agrif_ChildGrid_To_ParentGrid()
      Call UpdateGridhis()
      Call Agrif_ParentGrid_To_ChildGrid()
#endif     /* AGRIF_2WAY */
    
#ifdef AGRIF_ADAPTIVE
      call Agrif_Set_Regridding(300*(nfast+2))
      call Agrif_Set_Minwidth(18)
      call Agrif_Set_Rafmax(2)
#endif
     
      return
      end

      subroutine declare_zoom_variables()
      use AGRIF_UTIL
      implicit none
# include "param.h"
# include "ncscrum.h"
# include "scalars.h"
# include "zoom.h"
      integer :: irhot
      
      i1t = 0
      i2t = Lm+1
      j1t = 0
      j2t = Mm+1

#ifdef MPI
      if (WEST_INTER) then
        i1t = -1
      endif
      if (EAST_INTER) then
        i2t = Lmmpi+2
      else
        i2t = Lmmpi+1
      endif 
      if (SOUTH_INTER) then
        j1t = -1
      endif
      if (NORTH_INTER) then
        j2t = Mmmpi+2
      else
        j2t = Mmmpi+1
      endif           
#endif    

      i1u = i1t
      i2u = i2t
      j1v = j1t
      j2v = j2t
      
      irhot = Agrif_Irhot()
      
      Call Agrif_Declare_Variable((/2,2,0/),(/1,1,0/),
     &    (/'x','y','N'/),(/i1t,j1t,1/),
     &    (/i2t,j2t,irhot+1/),updatezetaid)

#if defined AGRIF_UPDATE_FULLWEIGHTING || defined AGRIF_UPDATE_MIX        
      Call Agrif_Set_UpdateType(updatezetaid,update=
     &Agrif_Update_full_weighting)
#endif

#if defined AGRIF_UPDATE_MIX_HIGH
      Call Agrif_Set_UpdateType(updatezetaid,update=
     &Agrif_Update_4rdorder)
#endif

#if defined AGRIF_UPDATE_AVERAGE || defined AGRIF_UPDATE_MIX_LOW
      Call Agrif_Set_UpdateType(updatezetaid,update=
     &Agrif_Update_Average)
#endif
     
      Call Agrif_Declare_Variable((/1,2,0/),(/1,1,0/),
     &    (/'x','y','N'/),(/0,j1t,1/),
     &    (/i2u,j2t,irhot+1/),updateubarid)

      Call Agrif_Declare_Variable((/1,2/),(/1,1/),
     &    (/'x','y'/),(/0,j1t/),
     &    (/i2u,j2t/),updateduavg2id)
     
#ifdef AGRIF_UPDATE_FULLWEIGHTING         
      Call Agrif_Set_UpdateType(updateubarid,update=
     &Agrif_Update_full_weighting)
      Call Agrif_Set_UpdateType(updateduavg2id,update=
     &Agrif_Update_full_weighting)
#endif

#ifdef AGRIF_UPDATE_MIX
      Call Agrif_Set_UpdateType(updateubarid,
     &update1=Agrif_Update_Average,
     &update2=Agrif_Update_Full_Weighting)
      Call Agrif_Set_UpdateType(updateduavg2id,
     &update1=Agrif_Update_Average,
     &update2=Agrif_Update_Full_Weighting)
#endif

#ifdef AGRIF_UPDATE_MIX_HIGH
      Call Agrif_Set_UpdateType(updateubarid,
     &update1=Agrif_Update_Full_Weighting,
     &update2=Agrif_Update_4rdorder)
      Call Agrif_Set_UpdateType(updateduavg2id,
     &update1=Agrif_Update_Full_Weighting,
     &update2=Agrif_Update_4rdorder)
     
#endif

#ifdef AGRIF_UPDATE_MIX_LOW
      Call Agrif_Set_UpdateType(updateubarid,
     &update1=Agrif_Update_Copy,
     &update2=Agrif_Update_Average)
      Call Agrif_Set_UpdateType(updateduavg2id,
     &update1=Agrif_Update_Copy,
     &update2=Agrif_Update_Average)
#endif

#ifdef AGRIF_UPDATE_AVERAGE
      Call Agrif_Set_UpdateType(updateubarid,
     &update=Agrif_Update_Average)   
      Call Agrif_Set_UpdateType(updateduavg2id,
     &update=Agrif_Update_Average) 
#endif

      Call Agrif_Declare_Variable((/2,1,0/),(/1,1,0/),
     &    (/'x','y','N'/),(/i1t,0,1/),
     &    (/i2t,j2v,irhot+1/),updatevbarid)
     
      Call Agrif_Declare_Variable((/2,1/),(/1,1/),
     &    (/'x','y'/),(/i1t,0/),
     &    (/i2t,j2v/),updatedvavg2id)

#ifdef AGRIF_UPDATE_FULLWEIGHTING          
      Call Agrif_Set_UpdateType(updatevbarid,update=
     &Agrif_Update_full_weighting)  
      Call Agrif_Set_UpdateType(updatedvavg2id,update=
     &Agrif_Update_full_weighting)
#endif

#ifdef AGRIF_UPDATE_MIX
      Call Agrif_Set_UpdateType(updatevbarid,
     &update1=Agrif_Update_Full_Weighting,
     &update2=Agrif_Update_Average)
      Call Agrif_Set_UpdateType(updatedvavg2id,
     &update1=Agrif_Update_Full_Weighting,
     &update2=Agrif_Update_Average)
#endif

#ifdef AGRIF_UPDATE_MIX_HIGH
      Call Agrif_Set_UpdateType(updatevbarid,
     &update1=Agrif_Update_4rdorder,
     &update2=Agrif_Update_Full_Weighting)
      Call Agrif_Set_UpdateType(updatedvavg2id,
     &update1=Agrif_Update_4rdorder,
     &update2=Agrif_Update_Full_Weighting)
#endif

#ifdef AGRIF_UPDATE_MIX_LOW
      Call Agrif_Set_UpdateType(updatevbarid,
     &update1=Agrif_Update_Average,
     &update2=Agrif_Update_Copy)
      Call Agrif_Set_UpdateType(updatedvavg2id,
     &update1=Agrif_Update_Average,
     &update2=Agrif_Update_Copy)
#endif

#ifdef AGRIF_UPDATE_AVERAGE
      Call Agrif_Set_UpdateType(updatevbarid,
     &update=Agrif_Update_Average)   
      Call Agrif_Set_UpdateType(updatedvavg2id,
     &update=Agrif_Update_Average) 
#endif

     
      Call Agrif_Declare_Variable((/2,2,0,0/),(/1,1,0,0/),
     &    (/'x','y','N','N'/),(/i1t,j1t,1,1/),
     &    (/i2t,j2t,N,NT/),updatetid)

#if defined AGRIF_UPDATE_FULLWEIGHTING || defined AGRIF_UPDATE_MIX
      Call Agrif_Set_UpdateType(updatetid,update=
     &Agrif_Update_full_weighting)
#endif

#if defined AGRIF_UPDATE_MIX_HIGH
      Call Agrif_Set_UpdateType(updatetid,update=
     &Agrif_Update_4rdorder)
#endif

#if defined AGRIF_UPDATE_AVERAGE || defined AGRIF_UPDATE_MIX_LOW
      Call Agrif_Set_UpdateType(updatetid,update=
     &Agrif_Update_Average)
#endif

      Call Agrif_Declare_Variable((/1,2,0/),(/1,1,0/),
     &    (/'x','y','N'/),(/i1u,j1t,1/),
     &    (/i2u,j2t,N/),updateuid)

#ifdef AGRIF_UPDATE_FULLWEIGHTING           
      Call Agrif_Set_UpdateType(updateuid,update=
     &Agrif_Update_full_weighting)
#endif

#ifdef AGRIF_UPDATE_MIX
      Call Agrif_Set_UpdateType(updateuid,
     &update1=Agrif_Update_Average,
     &update2=Agrif_Update_Full_Weighting)
#endif

#ifdef AGRIF_UPDATE_MIX_HIGH
      Call Agrif_Set_UpdateType(updateuid,
     &update1=Agrif_Update_Full_Weighting,
     &update2=Agrif_Update_4rdorder)
#endif

#ifdef AGRIF_UPDATE_MIX_LOW
      Call Agrif_Set_UpdateType(updateuid,
     &update1=Agrif_Update_Copy,
     &update2=Agrif_Update_Average)
#endif

#ifdef AGRIF_UPDATE_AVERAGE
      Call Agrif_Set_UpdateType(updateuid,
     &update=Agrif_Update_Average)    
#endif

      Call Agrif_Declare_Variable((/1,2,0/),(/1,1,0/),
     &    (/'x','y','N'/),(/i1u,j1t,1/),
     &    (/i2u,j2t,N/),updatehuonid)

#ifdef AGRIF_UPDATE_FULLWEIGHTING           
      Call Agrif_Set_UpdateType(updatehuonid,update=
     &Agrif_Update_full_weighting)
#endif

#ifdef AGRIF_UPDATE_MIX
      Call Agrif_Set_UpdateType(updatehuonid,
     &update1=Agrif_Update_Average,
     &update2=Agrif_Update_Full_Weighting)
#endif

#ifdef AGRIF_UPDATE_MIX_HIGH
      Call Agrif_Set_UpdateType(updatehuonid,
     &update1=Agrif_Update_Full_Weighting,
     &update2=Agrif_Update_4rdorder)
#endif

#ifdef AGRIF_UPDATE_MIX_LOW
      Call Agrif_Set_UpdateType(updatehuonid,
     &update1=Agrif_Update_Copy,
     &update2=Agrif_Update_Average)
#endif

#ifdef AGRIF_UPDATE_AVERAGE
      Call Agrif_Set_UpdateType(updatehuonid,
     &update=Agrif_Update_Average)    
#endif

      Call Agrif_Declare_Variable((/1,2,0,0/),(/1,1,0,0/),
     &    (/'x','y','N','N'/),(/i1u,j1t,1,1/),
     &    (/i2u,j2t,N,NT/),updatemyfxid)

#ifdef AGRIF_UPDATE_FULLWEIGHTING           
      Call Agrif_Set_UpdateType(updatemyfxid,update=
     &Agrif_Update_full_weighting)
#endif

#ifdef AGRIF_UPDATE_MIX
      Call Agrif_Set_UpdateType(updatemyfxid,
     &update1=Agrif_Update_Average,
     &update2=Agrif_Update_Full_Weighting)
#endif

#ifdef AGRIF_UPDATE_MIX_HIGH
      Call Agrif_Set_UpdateType(updatemyfxid,
     &update1=Agrif_Update_Full_Weighting,
     &update2=Agrif_Update_Average)
#endif

#ifdef AGRIF_UPDATE_MIX_LOW
      Call Agrif_Set_UpdateType(updatemyfxid,
     &update1=Agrif_Update_Copy,
     &update2=Agrif_Update_Average)
#endif

#ifdef AGRIF_UPDATE_AVERAGE
      Call Agrif_Set_UpdateType(updatemyfxid,
     &update=Agrif_Update_Average)    
#endif


      Call Agrif_Declare_Variable((/2,1,0/),(/1,1,0/),
     &    (/'x','y','N'/),(/i1t,j1v,1/),
     &    (/i2t,j2v,N/),updatevid)

#ifdef AGRIF_UPDATE_FULLWEIGHTING           
      Call Agrif_Set_UpdateType(updatevid,update=
     &Agrif_Update_full_weighting)
#endif

#ifdef AGRIF_UPDATE_MIX     
      Call Agrif_Set_UpdateType(updatevid,
     &update1=Agrif_Update_Full_Weighting,
     &update2=Agrif_Update_Average)
#endif

#ifdef AGRIF_UPDATE_MIX_HIGH  
      Call Agrif_Set_UpdateType(updatevid,
     &update1=Agrif_Update_4rdorder,
     &update2=Agrif_Update_Full_Weighting)
#endif

#ifdef AGRIF_UPDATE_MIX_LOW
      Call Agrif_Set_UpdateType(updatevid,
     &update1=Agrif_Update_Average,
     &update2=Agrif_Update_Copy)
#endif

#ifdef AGRIF_UPDATE_AVERAGE     
      Call Agrif_Set_UpdateType(updatevid,
     &update=Agrif_Update_Average)     
#endif

      Call Agrif_Declare_Variable((/2,1,0/),(/1,1,0/),
     &    (/'x','y','N'/),(/i1t,j1v,1/),
     &    (/i2t,j2v,N/),updatehvomid)

#ifdef AGRIF_UPDATE_FULLWEIGHTING           
      Call Agrif_Set_UpdateType(updatehvomid,update=
     &Agrif_Update_full_weighting)
#endif

#ifdef AGRIF_UPDATE_MIX     
      Call Agrif_Set_UpdateType(updatehvomid,
     &update1=Agrif_Update_Full_Weighting,
     &update2=Agrif_Update_Average)
#endif

#ifdef AGRIF_UPDATE_MIX_HIGH 
      Call Agrif_Set_UpdateType(updatehvomid,
     &update1=Agrif_Update_4rdorder,
     &update2=Agrif_Update_Full_Weighting)
#endif

#ifdef AGRIF_UPDATE_MIX_LOW
      Call Agrif_Set_UpdateType(updatehvomid,
     &update1=Agrif_Update_Average,
     &update2=Agrif_Update_Copy)
#endif

#ifdef AGRIF_UPDATE_AVERAGE     
      Call Agrif_Set_UpdateType(updatehvomid,
     &update=Agrif_Update_Average)     
#endif

      Call Agrif_Declare_Variable((/2,1,0,0/),(/1,1,0,0/),
     &    (/'x','y','N','N'/),(/i1t,j1v,1,1/),
     &    (/i2t,j2v,N,NT/),updatemyfyid)

#ifdef AGRIF_UPDATE_FULLWEIGHTING           
      Call Agrif_Set_UpdateType(updatemyfyid,update=
     &Agrif_Update_full_weighting)
#endif

#ifdef AGRIF_UPDATE_MIX     
      Call Agrif_Set_UpdateType(updatemyfyid,
     &update1=Agrif_Update_Full_Weighting,
     &update2=Agrif_Update_Average)
#endif

#ifdef AGRIF_UPDATE_MIX_HIGH    
      Call Agrif_Set_UpdateType(updatemyfyid,
     &update1=Agrif_Update_4rdorder,
     &update2=Agrif_Update_Full_Weighting)
#endif

#ifdef AGRIF_UPDATE_MIX_LOW
      Call Agrif_Set_UpdateType(updatemyfyid,
     &update1=Agrif_Update_Average,
     &update2=Agrif_Update_Copy)
#endif

#ifdef AGRIF_UPDATE_AVERAGE     
      Call Agrif_Set_UpdateType(updatemyfyid,
     &update=Agrif_Update_Average)     
#endif
     
        call Agrif_Declare_Variable((/2,2/),(/1,1/),(/'x','y'/),
     &   (/i1t,j1t/),(/i2t,j2t/),zetaid)
        call Agrif_Declare_Variable((/1,2/),(/1,1/),(/'x','y'/),
     &   (/0,j1t/),(/i2u,j2t/),ubarid)
        call Agrif_Declare_Variable((/2,1/),(/1,1/),(/'x','y'/),
     &   (/i1t,0/),(/i2t,j2v/),vbarid)    
             
        call Agrif_Declare_Variable((/2,2,0,0/),(/1,1,0,0/),
     &    (/'x','y','N','N'/),(/i1t,j1t,1,1/),
     &    (/i2t,j2t,N,NT/),tid)
     
        call Agrif_Declare_Variable((/2,2,0,0/),(/1,1,0,0/),
     &    (/'x','y','N','N'/),(/i1t,j1t,1,1/),
     &    (/i2t,j2t,N,NT/),tspongeid)
          
        call Agrif_Declare_Variable((/1,2,0/),(/1,1,0/),
     &   (/'x','y','N'/),(/i1u,j1t,1/),
     &   (/i2u,j2t,N/),uid)
     
        call Agrif_Declare_Variable((/1,2,0/),(/1,1,0/),
     &   (/'x','y','N'/),(/i1u,j1t,1/),
     &   (/i2u,j2t,N/),uspongeid)
          
        call Agrif_Declare_Variable((/2,1,0/),(/1,1,0/),
     &   (/'x','y','N'/),(/i1t,j1v,1/),
     &   (/i2t,j2v,N/),vid)
     
        call Agrif_Declare_Variable((/2,1,0/),(/1,1,0/),
     &   (/'x','y','N'/),(/i1t,j1v,1/),
     &   (/i2t,j2v,N/),vspongeid)     
             
        call Agrif_Set_bc(zetaid,(/-1,0/))     
        call Agrif_Set_bc(ubarid,(/0,0/))
        call Agrif_Set_bc(vbarid,(/0,0/))
        call Agrif_Set_bc(tid,(/-1,0/))     
        call Agrif_Set_bc(uid,(/0,0/))
        call Agrif_Set_bc(vid,(/0,0/))
        
        call Agrif_Set_bcinterp(zetaid,interp=Agrif_lagrange)        
        call Agrif_Set_bcinterp(ubarid,interp1=Agrif_lagrange,
     &                                 interp2=Agrif_ppm)
        call Agrif_Set_bcinterp(vbarid,interp1=Agrif_ppm,
     &                                 interp2=Agrif_lagrange)
        call Agrif_Set_bcinterp(tid,interp=Agrif_lagrange)
        call Agrif_Set_bcinterp(tspongeid,interp=Agrif_lagrange)
        
        call Agrif_Set_bcinterp(uid,interp1=Agrif_lagrange,
     &                                 interp2=Agrif_ppm)
     
        call Agrif_Set_bcinterp(uspongeid,interp1=Agrif_lagrange,
     &                                 interp2=Agrif_ppm)
          
        call Agrif_Set_bcinterp(vid,interp1=Agrif_ppm,
     &                                 interp2=Agrif_lagrange)
     
        call Agrif_Set_bcinterp(vspongeid,interp1=Agrif_ppm,
     &                                 interp2=Agrif_lagrange) 
           
      end subroutine declare_zoom_variables
            
#ifdef MASKING
       recursive subroutine ResetMask()
       Use Agrif_Util
#  include "param.h"
#  include "grid.h"
#  include "scalars.h"
       integer i,j
       
#ifdef MPI
      call exchange_r2d_tile (1,Lm,1,Mm,  rmask)
#endif

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

#ifdef MPI
      call exchange_u2d_tile (1,Lm,1,Mm,  umask)
      call exchange_v2d_tile (1,Lm,1,Mm,  vmask)
      call exchange_p2d_tile (1,Lm,1,Mm,  pmask)
#endif

      if (.Not.Agrif_Root()) Then
        Agrif_UseSpecialValueInUpdate = .TRUE.
        Agrif_SpecialValueFineGrid = 0.
      
        Call Agrif_Update_Variable(rmask,rmask)
      
        Agrif_UseSpecialValueInUpdate = .FALSE.   

        Call Agrif_ChildGrid_To_ParentGrid()
        Call ResetMask()
        Call Agrif_ParentGrid_To_ChildGrid()
      endif

       end subroutine ResetMask
#endif      

      Subroutine Updateh(tabres,i1,i2,j1,j2,before)
      implicit none
#  include "param.h"
#  include "grid.h"
#  include "ocean2d.h"
#  include "ocean3d.h"
#  include "coupling.h"
#  include "scalars.h"
#  include "zoom.h"
      integer i1,i2,j1,j2
      real tabres(i1:i2,j1:j2)
      logical before
      real cff_r,cff1_r,cff2_r, cff_w,cff1_w,cff2_w, z_r0,z_w0
      
      integer i,j,k  
       real tabtemp(i1:i2,j1:j2) 
              
      IF (before) THEN
 
           tabres(i1:i2,j1:j2) = h(i1:i2,j1:j2)
#ifdef MASKING
     &       *rmask(i1:i2,j1:j2)
#endif
         
       ELSE
       do j=j1,j2
       do i=i1,i2
         h(i,j)=max(tabres(i,j),hc)
         hinv(i,j)=1./h(i,j)
       enddo
       enddo
       
       do j=j1,j2
       do i=i1,i2
         z_w(i,j,0) = -h(i,j)
       enddo
       do k=1,N,+1
          cff_w=hc*(sc_w(k)-Cs_w(k))
          cff1_w=Cs_w(k)
          cff2_w=sc_w(k)+1.

          cff_r=hc*(sc_r(k)-Cs_r(k))
          cff1_r=Cs_r(k)
          cff2_r=sc_r(k)+1.

          do i=i1,i2
            z_w0=cff_w+cff1_w*h(i,j)                               !<
            z_w(i,j,k)=z_w0+Zt_avg1(i,j)*(1.+z_w0*hinv(i,j))       !<

            z_r0=cff_r+cff1_r*h(i,j)                               !<
            z_r(i,j,k)=z_r0+Zt_avg1(i,j)*(1.+z_r0*hinv(i,j))       !<

c**         z_w(i,j,k)=cff_w+cff1_w*h(i,j)+cff2_w*Zt_avg1(i,j)     !>
c**         z_r(i,j,k)=cff_r+cff1_r*h(i,j)+cff2_r*Zt_avg1(i,j)     !>

            Hz_bak(i,j,k)=Hz(i,j,k)
            Hz(i,j,k)=z_w(i,j,k)-z_w(i,j,k-1)
          enddo
        enddo
      enddo
	 	 
       ENDIF
      
      return
      end


      recursive Subroutine UpdateGridhis()
      implicit none
# include "param.h"
# include "grid.h"
# include "scalars.h"
# include "ncscrum.h"

      call wrt_his

      if (.Not.Agrif_Root()) Then
        Call Agrif_ChildGrid_To_ParentGrid()
        Call UpdateGridhis()
        Call Agrif_ParentGrid_To_ChildGrid()
      endif
      
      end subroutine UpdateGridhis


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
       do i=max(i1,lbound(h,1)+1),i2
         tabres(i,j) = 0.5*(h(i-1,j)+h(i,j))
       enddo
       enddo       
       ELSE
       do j=max(j1,lbound(h,2)+1),j2
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



      SUBROUTINE Agrif_detect(taberr,sizexy)
!
!     Modules used:
!
      use AGRIF_types
      implicit none
      integer tile, ierr, trd, subs
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

!
!
!     Declarations:
!
!
!     Variables
!

      Integer, Dimension(2) :: sizexy
      Integer,Dimension(sizexy(1),sizexy(2))   :: taberr        ! Pointer on the current grid      
!
!     Begin
!
!
       real vort(GLOBAL_2D_ARRAY)

      integer i,j
      real crit

      vort = 0.
      do j=1,Mm+1
      do i=1,Lm+1
        vort(i,j) = (v(i,j,N,nnew)-v(i-1,j,N,nnew))
     &     -(u(i,j,N,nnew)-u(i,j-1,N,nnew))
      enddo
      enddo
!      
      crit = maxval(abs(vort))
      taberr=0.
      do j=1,Mm+1
      do i=1,Lm+1
          if (abs(vort(i,j)) > 0.8*crit) then
            taberr(i,j) = 1
          endif
      enddo
      enddo
      
      Return
      End Subroutine Agrif_detect
      
      Subroutine Agrif_Before_Regridding()
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
   
      real tabtemp2d(GLOBAL_2D_ARRAY)
      real tabtemp3d(GLOBAL_2D_ARRAY,N)
      real text(GLOBAL_2D_ARRAY,N,NT)
      integer itrc
  
      tabtemp2d = Zt_avg1
      
      Call Agrif_Save_ForRestore(tabtemp2d,zetaid)
      tabtemp2d = Agrif_irhoy()*DU_avg1(:,:,nnew)
      Call Agrif_Save_ForRestore(tabtemp2d,ubarid)
      tabtemp2d = Agrif_irhox()*DV_avg1(:,:,nnew)      
      Call Agrif_Save_ForRestore(tabtemp2d,vbarid)
      
      tabtemp3d = u(:,:,:,nnew)
      Call Agrif_Save_ForRestore(tabtemp3d,uid)
      tabtemp3d = v(:,:,:,nnew)
      Call Agrif_Save_ForRestore(tabtemp3d,vid)

      do itrc=1,NT
            text(:,:,:,itrc) = t(:,:,:,nnew,itrc)
      enddo
      Call Agrif_Save_ForRestore(text,tid)        
                    
      Return
      End Subroutine Agrif_Before_Regridding

      subroutine initzeta(tabres,i1,i2,j1,j2)
      implicit none
# include "param.h"
# include "ocean2d.h"
# include "scalars.h"
# include "zoom.h"
# include "ocean3d.h"
# include "coupling.h"

       integer i1,i2,j1,j2
       real tabres(i1:i2,j1:j2)

       tabres = Zt_avg1(i1:i2,j1:j2)
             
      return
      end
      
      subroutine initubar(tabres,i1,i2,j1,j2)
      implicit none
# include "param.h"
# include "ocean2d.h"
# include "scalars.h"
# include "zoom.h"
# include "ocean3d.h"
# include "coupling.h"

       integer i1,i2,j1,j2
       real tabres(i1:i2,j1:j2)
       
       tabres = DU_avg1(i1:i2,j1:j2,nnew)
             
      return
      end 
      
      subroutine initvbar(tabres,i1,i2,j1,j2)
      implicit none
# include "param.h"
# include "ocean2d.h"
# include "scalars.h"
# include "zoom.h"
# include "ocean3d.h"
# include "coupling.h"

       integer i1,i2,j1,j2
       real tabres(i1:i2,j1:j2)
       
       tabres = DV_avg1(i1:i2,j1:j2,nnew)
             
      return
      end            
      
      subroutine initu(tabres,i1,i2,j1,j2,k1,k2)
      implicit none
# include "param.h"
# include "ocean3d.h"
# include "scalars.h"
# include "zoom.h"

       integer i1,i2,j1,j2,k1,k2
       real tabres(i1:i2,j1:j2,k1:k2)
       
       tabres = u(i1:i2,j1:j2,k1:k2,nnew)
             
      return
      end 
      
      subroutine initv(tabres,i1,i2,j1,j2,k1,k2)
      implicit none
# include "param.h"
# include "ocean3d.h"
# include "scalars.h"
# include "zoom.h"

       integer i1,i2,j1,j2,k1,k2
       real tabres(i1:i2,j1:j2,k1:k2)
       
       tabres = v(i1:i2,j1:j2,k1:k2,nnew)
             
      return
      end
      
      subroutine initt(tabres,i1,i2,j1,j2,k1,k2,m1,m2)
      implicit none
# include "param.h"
# include "ocean3d.h"
# include "scalars.h"
# include "zoom.h"

       integer i1,i2,j1,j2,k1,k2,m1,m2
       real tabres(i1:i2,j1:j2,k1:k2,m1:m2)
       
       tabres = t(i1:i2,j1:j2,k1:k2,nnew,m1:m2)
             
      return
      end                   

      subroutine computenbmaxtimes()
      implicit none
#include "param.h"
#include "scalars.h"
#include "zoom.h"
      integer nunit
      integer j, jp
      integer :: nb
      
      integer :: nbstep2d(0:20)
      
      nunit = Agrif_Get_Unit()
    
      open(nunit,file='AGRIF_FixedGrids.in',form='formatted')
      j=1
      jp = 0
      call agrif_read_fixed(jp,j,nunit)
      close(nunit)
      
      nbstep2d = 0
      
      call write_number(0,nbstep2d)
      
      nb = 0
      do while (sortedint(nb) /= -1)
        nbmaxtimes = nb
        nb = nb + 1
      enddo
      
      return
      end
      
      recursive subroutine write_number(j,nbstep2d)
      implicit none
#include "param.h"
#include "scalars.h"
#include "zoom.h"
      integer :: j
      integer nb
      integer ngrid
      integer :: nbstep2d(0:20)
      
      call write_prestep3d(j)
      
      do nb=1,nfast
       call write_step2d(j,nbstep2d)
      enddo
      
      end subroutine write_number

      recursive subroutine write_prestep3d(j)
      implicit none
#include "param.h"
#include "scalars.h"
#include "zoom.h"
      integer :: j
      integer nb
      integer ngrid
      integer :: nbstep2d(0:20)
      
      iind = iind + 1
      sortedint(iind) = j
      whichstep(iind) = 0
      
      ngrid = 0
      do while (grids_at_level(j,ngrid)/=-1)
      call write_prestep3d(grids_at_level(j,ngrid))
      ngrid = ngrid + 1
      enddo
      
      end subroutine write_prestep3d
      
      recursive subroutine write_step2d(j,nbstep2d)
      implicit none
#include "param.h"
#include "scalars.h"
#include "zoom.h"
      integer :: j
      integer nb
      integer ngrid
      integer :: nbstep2d(0:20)

      iind = iind + 1
      sortedint(iind) = j
      whichstep(iind) = 1
      nbstep2d(j) = nbstep2d(j)+1
      
      ngrid = 0
      do while (grids_at_level(j,ngrid)/=-1)
      do nb=1,coeff_ref_time(grids_at_level(j,ngrid))
      call write_step2d(grids_at_level(j,ngrid),nbstep2d)
      enddo
      ngrid = ngrid + 1
      enddo
      
      if (nbstep2d(j) == nfast) then
        if (j /= 0) then
          if (nbstep2d(parent_grid(j)) /= nfast) then
            call write_step3d1(j)
            call write_step3d2(j)
            call write_prestep3d(j)
          endif
        endif
        if (j == 0) then
          call write_step3d1(j)
          call write_step3d2(j)
        endif
        nbstep2d(j) = 0
      endif
      
      end subroutine write_step2d
  
      recursive subroutine write_step3d1(j)
      implicit none
#include "param.h"
#include "scalars.h"
#include "zoom.h"
      integer :: j
      integer nb
      integer ngrid
      integer :: nbstep2d(0:20)
      
      iind = iind + 1
      sortedint(iind) = j
      whichstep(iind) = 2
      
      ngrid = 0
      do while (grids_at_level(j,ngrid)/=-1)
      call write_step3d1(grids_at_level(j,ngrid))
      ngrid = ngrid + 1
      enddo
      
      end subroutine write_step3d1
      
      recursive subroutine write_step3d2(j)
      implicit none
#include "param.h"
#include "scalars.h"
#include "zoom.h"
      integer :: j
      integer nb
      integer ngrid
      integer :: nbstep2d(0:20)
      
      iind = iind + 1
      sortedint(iind) = j
      whichstep(iind) = 3
      
      ngrid = 0
      do while (grids_at_level(j,ngrid)/=-1)
      call write_step3d2(grids_at_level(j,ngrid))
      ngrid = ngrid + 1
      enddo
      
      end subroutine write_step3d2
      
      recursive subroutine agrif_read_fixed(jp,j,nunit)
      implicit none
#include "param.h"
#include "scalars.h"
#include "zoom.h"
      integer :: j,nunit,jp
      integer :: i
      integer :: nb_grids
      integer :: grid_number
      integer :: r_imin,r_imax,r_jmin,r_jmax,r_rhox,r_rhoy,r_time
      integer :: gridnum(1000)
      
      read(nunit,*)nb_grids
      
      do i=1,nb_grids
        grid_number = j
        grids_at_level(jp,i-1)=j
        gridnum(i)=j
        parent_grid(j) = jp
        read(nunit,*)r_imin,r_imax,r_jmin,r_jmax,r_rhox,r_rhoy,r_time
        coeff_ref_time(j)=r_time
        j=j+1
      enddo
      
      do i=1,nb_grids
       call agrif_read_fixed(gridnum(i),j,nunit)
      enddo
      
      return
      end

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
