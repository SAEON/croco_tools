! $Id: pre_step3d.F 1568 2014-06-30 15:57:49Z gcambon $
!
!======================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al)
! and Rutgers University (Arango et al) are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
!
! ROMS_AGRIF website : http://www.romsagrif.org
!======================================================================
!
#include "cppdefs.h"
#ifdef SOLVE3D

      subroutine pre_step3d (tile)
!
      implicit none
      integer tile,  trd,omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "ocean3d.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call pre_step3d_tile (Istr,Iend,Jstr,Jend,
     &                A3d(1,1,trd), A3d(1,2,trd), A3d(1,5,trd),
     &                A2d(1,1,trd), A2d(1,2,trd), A2d(1,3,trd),
     &                A2d(1,1,trd), A2d(1,2,trd), A2d(1,3,trd)
# ifndef NBQ
     &                                          , A3d(1,3,trd)
# endif
     &                      )
      return
      end

      subroutine pre_step3d_tile (Istr,Iend,Jstr,Jend, ru,rv,rw,
     &                                     FC,CF,DC, FX,FE,WORK
# ifndef NBQ
     &                                                 ,Hz_half
# endif
     &                           )  
!
!--------------------------------------------------------------------
! Preliminary step: initialize computations of the new time step
! 3D primitive variables.
!
! Since r.h.s. arrays ru,rv,rt(:,:,???[,:]), which at this moment
! contain r.h.s at time step n-2 will be overwritten by the
! subsequent routines within rhs3d driver, both [n-1 and n-2] old-
! time-step r.h.s. term in Adams-Bashforth stepping scheme are
! added at this time to the time step [n] fields and the result is
! stored as u,v,t(:,:,???[,:]).
!
! The actual time step will be completed in step3d, after the
! time step [n] r.h.s. terms and new-time step Hz will be available
! after the completion rhs3d computations and the 2D (barotropic
! mode) computations.
!--------------------------------------------------------------------
!
      implicit none
# include "param.h"
      integer Istr,Iend,Jstr,Jend, itrc, i,j,k, indx
     &       ,imin,imax,jmin,jmax
# ifdef PSOURCE
     &       ,is
# endif
      real   ru(PRIVATE_2D_SCRATCH_ARRAY,N),    cff,
     &       rv(PRIVATE_2D_SCRATCH_ARRAY,N),    cff1,
     &       rw(PRIVATE_2D_SCRATCH_ARRAY,0:N),     
     &       FC(PRIVATE_1D_SCRATCH_ARRAY,0:N),  cff2,
     &       CF(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &       DC(PRIVATE_1D_SCRATCH_ARRAY,0:N),  gamma, 
     &       FX(PRIVATE_2D_SCRATCH_ARRAY),      epsil,
     &       FE(PRIVATE_2D_SCRATCH_ARRAY),
     &     WORK(PRIVATE_2D_SCRATCH_ARRAY)
# ifndef NBQ
      real Hz_half(PRIVATE_2D_SCRATCH_ARRAY,N)
# endif
      parameter (gamma=1./6., epsil=1.E-16)
# include "grid.h"
# include "ocean3d.h"
# include "coupling.h"
# include "ocean2d.h"
# include "forces.h"
# include "mixing.h"
# include "scalars.h"
# ifdef PSOURCE
#  include "sources.h"
# endif
# ifdef NBQ
#  include "nbq.h"
# endif
# ifdef MASKING
#  define SWITCH *
# else
#  define SWITCH !
# endif
!
!--------------------------------------------------------------------
! Definition of flux operators: 1st, 2nd, 3rd, 4th, 5th or 6th order,
! used in UP5 and C6 advection schemes (and order degradation near 
! land masks). cdiff is part of laplacian diffusion in flux1 (used 
! near mask): 
!    0 --> flux1=flux2 (second order C2 advection scheme)
!    1 --> flux1 gives 1st order monotonic UP1 advection scheme
!--------------------------------------------------------------------
!
# if defined TS_HADV_UP5 ||  defined TS_HADV_C6 \
      || defined TS_HADV_WENO5 ||  defined BIO_HADV_WENO5 \
      || defined TS_VADV_WENO5

      REAL    :: q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2
      REAL    :: ua, vel, cdiff, cdif
      REAL    :: flux1, flux2, flux3, flux4, flux5, flux6
      REAL    :: flx2, flx3, flx4, flx5
      REAL    :: mask0, mask1, mask2, mask3

      flux2(q_im1, q_i, ua, cdiff) = 0.5*( q_i + q_im1 )

      flux1(q_im1, q_i, ua, cdiff) = flux2(q_im1, q_i, ua, cdiff) -
     &      0.5*cdiff*sign(1.,ua)*(q_i-q_im1)

      flux4(q_im2, q_im1, q_i, q_ip1, ua) = 
     &      ( 7.*(q_i + q_im1) - (q_ip1 + q_im2) )/12.0

      flux3(q_im2, q_im1, q_i, q_ip1, ua) =
     &      flux4(q_im2, q_im1, q_i, q_ip1, ua) +
     &      sign(1.,ua)*((q_ip1 -
     &      q_im2)-3.*(q_i-q_im1))/12.0

      flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =
     &      ( 37.*(q_i+q_im1) - 8.*(q_ip1+q_im2)
     &      +(q_ip2+q_im3) )/60.0

      flux5(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua) =
     &      flux6(q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua)
     &      -sign(1.,ua)*( 
     &      (q_ip2-q_im3)-5.*(q_ip1-q_im2)+10.*(q_i-q_im1) )/60.0

# endif
!--------------------------------------------------------------------
!
# include "compute_auxiliary_bounds.h"
!
      indx=3-nstp

      if (FIRST_TIME_STEP) then
        cff=0.5*dt
        cff1=1.
        cff2=0.
      else
        cff=(1.-gamma)*dt
        cff1=0.5+gamma
        cff2=0.5-gamma
      endif
      do k=1,N
        do j=JstrV-1,Jend
          do i=IstrU-1,Iend
            Hz_half(i,j,k)=cff1*Hz(i,j,k)+cff2*Hz_bak(i,j,k)
     &        -cff*pm(i,j)*pn(i,j)*( Huon(i+1,j,k)-Huon(i,j,k)
     &                              +Hvom(i,j+1,k)-Hvom(i,j,k)
     &                                    +W(i,j,k)-W(i,j,k-1)
     &                                                       )
          enddo
        enddo
      enddo

# ifdef NBQ
      do k=1,N
        do j=JstrV-1,Jend
          do i=IstrU-1,Iend
            Hzr_half_nbq(i,j,k)=Hz_half(i,j,k)/
     &                          (1.5*rho_nbq_avg1(i,j,k)
     &                          -0.5*rho_nbq_avg2(i,j,k))
          enddo
        enddo
      enddo
!
!  Set lateral boundary conditions for Hz_half
!
#  ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
        do k=0,N
          do j=Jstr,Jend
            Hzr_half_nbq(0,j,k)=Hzr_half_nbq(1,j,k)
          enddo
        enddo
      endif
      if (EASTERN_EDGE) then
        do k=0,N
          do j=Jstr,Jend
            Hzr_half_nbq(LOCALLM+1,j,k)=Hzr_half_nbq(LOCALLM,j,k)
          enddo
        enddo
      endif
#  endif
#  ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        do k=0,N
          do i=Istr,Iend
            Hzr_half_nbq(i,0,k)=Hzr_half_nbq(i,1,k)
          enddo
        enddo
      endif
      if (NORTHERN_EDGE) then
        do k=0,N
          do i=Istr,Iend
            Hzr_half_nbq(i,LOCALMM+1,k)=Hzr_half_nbq(i,LOCALMM,k)
          enddo
        enddo
      endif
#  endif
#  ifndef EW_PERIODIC
      if (WESTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=0,N
          Hzr_half_nbq(0,0,k)=Hzr_half_nbq(1,1,k)
        enddo
      endif
      if (WESTERN_EDGE .and. NORTHERN_EDGE) then
        do k=0,N
          Hzr_half_nbq(0,LOCALMM+1,k)=Hzr_half_nbq(1,LOCALMM,k)
        enddo
      endif
      if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=0,N
          Hzr_half_nbq(LOCALLM+1,0,k)=Hzr_half_nbq(LOCALLM,1,k)
        enddo
      endif
      if (EASTERN_EDGE .and. NORTHERN_EDGE) then
        do k=0,N
          Hzr_half_nbq(LOCALLM+1,LOCALMM+1,k)=
     &                     Hzr_half_nbq(LOCALLM,LOCALMM,k)
        enddo
      endif
#  endif
#  if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                        Hzr_half_nbq(START_2D_ARRAY,1))
#  endif
      do j=Jstr-1,Jend+1
        do i=Istr-1,Iend+1
           zw_half_nbq(i,j,0)=-h(i,j)
           zr_half_nbq(i,j,1)=zw_half_nbq(i,j,0)+0.5*Hzr_half_nbq(i,j,1)
        enddo
      enddo
      do k=1,N
        do j=Jstr-1,Jend+1
          do i=Istr-1,Iend+1
            zw_half_nbq(i,j,k)=zw_half_nbq(i,j,k-1)+Hzr_half_nbq(i,j,k)
          enddo
        enddo
      enddo
      do k=2,N
        do j=Jstr-1,Jend+1
          do i=Istr-1,Iend+1
            zr_half_nbq(i,j,k)=zr_half_nbq(i,j,k-1)
     &                  +0.5*(Hzr_half_nbq(i,j,k)+Hzr_half_nbq(i,j,k-1))
          enddo
        enddo
      enddo
      do k=1,N-1
        do j=JstrV-1,Jend
          do i=IstrU-1,Iend
            Hzw_half_nbq(i,j,k)=z_r(i,j,k+1)-z_r(i,j,k)
        enddo
      enddo
      enddo
      do j=Jstr-1,Jend+1
        do i=Istr-1,Iend+1
          Hzr_half_nbq(i,j,0  )=0.
          Hzr_half_nbq(i,j,N+1)=0.
          Hzw_half_nbq(i,j,0  )=z_r(i,j,1)-z_w(i,j,0)
          Hzw_half_nbq(i,j,N  )=z_w(i,j,N)-z_r(i,j,N)
        enddo
      enddo
# endif /* NBQ */

!
!  Start computation of the auxiliary tracer field.
!----------------------------------------------------
!  Once it will be completed, t(:,:,???,:) is effectively halfway
!  between time steps n and n+1. A high spatial order, centered,
!  non-conservative [but constancy preserving!] scheme is used for
!  this auxiliary step. This is done by introducing an artificial
!  continuity equation [Easter, 1993].
!
!  Since this field will be used exclussively to compute the high-
!  order fluxes during subsequent step3d_t operation, the final
!  values of t(i,j,k,??,itrc) alfer step3d_t will be computed in
!  a flux-conservative manner. The overall time step will be both 
!  conservative and constancy preserving.
!
!  This preliminary step shall be done before field t(:,:,:,???,:)
!  loses its meaningful values during the pre-step operation. 
!
!======================================================================
!
!  Compute horizontal advection
!
!======================================================================
!
      do itrc=1,NT
        do k=1,N

# ifdef BIO_HADV_WENO5
          if (itrc .le. 2) then
# endif

# if defined TS_HADV_UP5 || defined TS_HADV_C6 || defined TS_HADV_WENO5
!
!----------------------------------------------------------
! Sixth order advection scheme
!----------------------------------------------------------
!
#  define FLUX5 flux6
#  define FLUX3 flux4
#  define FLUX2 flux2
#  undef  UP5_MASKING
!
            cdif=0.
#  include "t3dadv_order5.h"
!
#  undef FLUX5
#  undef FLUX3
#  undef FLUX2
#  undef UP5_MASKING

# elif defined TS_HADV_C2
!
!----------------------------------------------------------
! Second order advection scheme
!----------------------------------------------------------
!
          do j=Jstr,Jend
            do i=Istr,Iend+1
              FX(i,j)=0.5*(t(i,j,k,nstp,itrc)+t(i-1,j,k,nstp,itrc))
     &                                                 *Huon(i,j,k)
            enddo
          enddo
          do j=Jstr,Jend+1
            do i=Istr,Iend
              FE(i,j)=0.5*(t(i,j,k,nstp,itrc)+t(i,j-1,k,nstp,itrc))
     &                                                 *Hvom(i,j,k)
            enddo
          enddo

# else  /*  --> default: C4 */
!----------------------------------------------------------
! Fourth order advection scheme
!----------------------------------------------------------
!
#  define grad WORK
#  ifdef EW_PERIODIC
#   define I_EXT_RANGE Istr-1,Iend+2
#  else
#   ifdef MPI
          if (WEST_INTER) then
            imin=Istr-1
          else
            imin=max(Istr-1,1)
          endif
          if (EAST_INTER) then
            imax=Iend+2
          else
            imax=min(Iend+2,Lmmpi+1)
          endif
#    define I_EXT_RANGE imin,imax
#   else
#    define I_EXT_RANGE max(Istr-1,1),min(Iend+2,Lm+1)
#   endif
#  endif
# ifdef NS_PERIODIC
#  define J_EXT_RANGE Jstr-1,Jend+2
# else
#  ifdef MPI
          if (SOUTH_INTER) then
            jmin=Jstr-1
          else
            jmin=max(Jstr-1,1)
          endif
          if (NORTH_INTER) then
            jmax=Jend+2
          else
            jmax=min(Jend+2,Mmmpi+1)
          endif
#   define J_EXT_RANGE jmin,jmax
#  else
#   define J_EXT_RANGE max(Jstr-1,1),min(Jend+2,Mm+1)
#  endif
# endif

          do j=Jstr,Jend
            do i=I_EXT_RANGE
              FX(i,j)=(t(i,j,k,nstp,itrc)-t(i-1,j,k,nstp,itrc))
#  ifdef MASKING
     &                                               *umask(i,j)
#  endif
            enddo
          enddo
#  undef I_EXT_RANGE
#  ifndef EW_PERIODIC 
          if (WESTERN_EDGE) then
            do j=Jstr,Jend
              FX(0,j)=FX(1,j)
            enddo
          endif
          if (EASTERN_EDGE) then
#   ifdef MPI
            do j=Jstr,Jend
              FX(Lmmpi+2,j)=FX(Lmmpi+1,j)
            enddo
#   else
             do j=Jstr,Jend
              FX(Lm+2,j)=FX(Lm+1,j)
            enddo
#   endif
          endif
#  endif
          do j=Jstr,Jend
            do i=Istr-1,Iend+1
              grad(i,j)=0.5*(FX(i+1,j)+FX(i,j))
            enddo
          enddo             !--> discard FX
          do j=Jstr,Jend
            do i=Istr,Iend+1
              FX(i,j)=0.5*( t(i,j,k,nstp,itrc)+t(i-1,j,k,nstp,itrc)
     &                     -0.333333333333*(grad(i,j)-grad(i-1,j))
     &                                               )*Huon(i,j,k)
            enddo
          enddo            !--> discard grad

          do j=J_EXT_RANGE
            do i=Istr,Iend
              FE(i,j)=(t(i,j,k,nstp,itrc)-t(i,j-1,k,nstp,itrc))
#  ifdef MASKING
     &                                               *vmask(i,j)
#  endif
            enddo
          enddo
#  undef J_EXT_RANGE
#  ifndef NS_PERIODIC
          if (SOUTHERN_EDGE) then
            do i=Istr,Iend
              FE(i,0)=FE(i,1)
            enddo
          endif
          if (NORTHERN_EDGE) then
#   ifdef MPI
            do i=Istr,Iend
              FE(i,Mmmpi+2)=FE(i,Mmmpi+1)
            enddo
#   else
            do i=Istr,Iend
              FE(i,Mm+2)=FE(i,Mm+1)
            enddo
#   endif
          endif
#  endif
          do j=Jstr-1,Jend+1
            do i=Istr,Iend
              grad(i,j)=0.5*(FE(i,j+1)+FE(i,j))
            enddo
          enddo            !--> discard FE
          do j=Jstr,Jend+1
            do i=Istr,Iend
              FE(i,j)=0.5*( t(i,j,k,nstp,itrc)+t(i,j-1,k,nstp,itrc)
     &                     -0.333333333333*(grad(i,j)-grad(i,j-1))
     &                                               )*Hvom(i,j,k)
#  undef grad
            enddo
          enddo            !--> discard grad
# endif /* TS_HADV_UP5 */

# ifdef BIO_HADV_WENO5
          else  ! if (itrc .gt. 2)
!
!----------------------------------------------------------
! C6 advection for passive tracers 
!----------------------------------------------------------
!
#  define FLUX5 flux6
#  define FLUX3 flux4
#  define FLUX2 flux2
#  undef  UP5_MASKING
!
            cdif=0.
#  include "t3dadv_order5.h"
!
#  undef FLUX5
#  undef FLUX3
#  undef FLUX2
#  undef UP5_MASKING
          endif
# endif

# ifdef PSOURCE
!
!----------------------------------------------------------
! Apply point sources for river runoff simulations
!----------------------------------------------------------
!
          do is=1,Nsrc
#  ifdef MPI
           i=Isrc_mpi(is,mynode)
           j=Jsrc_mpi(is,mynode)
#  else
           i=Isrc(is)
           j=Jsrc(is)
#  endif
            if (Istr.le.i .and. i.le.Iend+1
     &                   .and. Jstr.le.j .and. j.le.Jend+1) then
              if (Dsrc(is).eq.0) then
                if (Lsrc(is,itrc)) then
                  FX(i,j)=Huon(i,j,k)*Tsrc(is,k,itrc)
#  ifdef MASKING
                else
                  if (rmask(i,j).eq.0 .and. rmask(i-1,j).eq.1) then
                    FX(i,j)=Huon(i,j,k)*t(i-1,j,k,nstp,itrc)
                  elseif (rmask(i,j).eq.1. .and. rmask(i-1,j).eq.0) then
                    FX(i,j)=Huon(i,j,k)*t(i  ,j,k,nstp,itrc)
                  endif
#  endif
                endif
              else
                if (Lsrc(is,itrc)) then
                  FE(i,j)=Hvom(i,j,k)*Tsrc(is,k,itrc)
#  ifdef MASKING
                else
                  if (rmask(i,j).eq.0 .and. rmask(i,j-1).eq.1) then
                    FE(i,j)=Hvom(i,j,k)*t(i,j-1,k,nstp,itrc)
                  elseif (rmask(i,j).eq.1 .and. rmask(i,j-1).eq.0) then
                    FE(i,j)=Hvom(i,j,k)*t(i,j  ,k,nstp,itrc)
                  endif
#  endif
                endif
              endif
            endif
          enddo
# endif /* PSOURCE */
!
!----------------------------------------------------------
! Finalize horizontal advection: compute flux divergences
!----------------------------------------------------------
!
          if (FIRST_TIME_STEP) then
            cff=0.5*dt
            do j=Jstr,Jend
              do i=Istr,Iend
                t(i,j,k,nnew,itrc)=Hz(i,j,k)*t(i,j,k,nstp,itrc)
     &                      -cff*pm(i,j)*pn(i,j)*( FX(i+1,j)-FX(i,j)
     &                                            +FE(i,j+1)-FE(i,j))
              enddo
            enddo
          else
            cff=(1.-gamma)*dt
            cff1=0.5+gamma
            cff2=0.5-gamma
            do j=Jstr,Jend
              do i=Istr,Iend
                t(i,j,k,nnew,itrc)=cff1*Hz(i,j,k)*t(i,j,k,nstp,itrc)
     &                            +cff2*Hz_bak(i,j,k)*t(i,j,k,indx,itrc)
     &                         -cff*pm(i,j)*pn(i,j)*( FX(i+1,j)-FX(i,j)
     &                                               +FE(i,j+1)-FE(i,j))
              enddo
            enddo
          endif

        enddo   ! <-- k
      enddo       ! <-- itrc

!
! Continue computation of the auxiliary tracer field: auxiliary
! continuity equation (the same for all tracers): DC=1/Hz_half_new,
! where Hz_half_new is Hz at time step n+1/2 as it would be computed
! from three-dimensional divergence of volume fluxes Huon,Hvom and W.
!
!======================================================================
!
! Compute vertical advection
!
!======================================================================
!
! Finalize computation of the auxiliary tracer field: compute its
! change due to vertical advection. Computation of vertical advective
! fluxes requires interpolation of tracer values to the verical grid-
! box interfaces (W-points). This can be done by either using
! parabolic spline interpolation or, more simple local cubic
! polynomial [linear interpolation is considered obsolete].
!
      do j=Jstr,Jend
        do k=1,N
          do i=Istr,Iend
            DC(i,k)=1./Hz_half(i,j,k)
          enddo
        enddo
        do itrc=1,NT
# ifdef TS_VADV_SPLINES
!
!----------------------------------------------------------
! Compute vertical advective fluxes using parabolic splines: 
! FC=W*[spline-interpolated tracer]
!----------------------------------------------------------
!
                              ! Construct parabolic splines: here
          do i=Istr,Iend      ! CF is the set of vertical derivatives
            FC(i,0)=0.        ! of the tracer field t(:,:,:,nstp,:),
            CF(i,0)=0.        ! FC is an auxiliary scratch variable. 
          enddo
          do k=1,N-1,+1
            do i=Istr,Iend
              cff=1./(2.*HZR(i,j,k+1)+HZR(i,j,k)*(2.-FC(i,k-1)))
              FC(i,k)=cff*HZR(i,j,k+1)
              CF(i,k)=cff*( 6.*( t(i,j,k+1,nstp,itrc)
     &               -t(i,j,k,nstp,itrc))-HZR(i,j,k)*CF(i,k-1))
            enddo
          enddo
          do i=Istr,Iend
            CF(i,N)=0.
          enddo
          do k=N-1,1,-1       !<-- irreversible
            do i=Istr,Iend
              CF(i,k)=CF(i,k)-FC(i,k)*CF(i,k+1)
            enddo
          enddo               !--> discard FC, keep CF

          cff=1./3.           ! Compute vertical advective fluxes 
          do k=1,N-1          ! FC=W*[spline-interpolated tracer]
            do i=Istr,Iend
              FC(i,k)=W(i,j,k)*( t(i,j,k,nstp,itrc)+cff*HZR(i,j,k)
     &                                  *(CF(i,k)+0.5*CF(i,k-1)) )
            enddo
          enddo               !--> discard CF
          do i=Istr,Iend
            FC(i,N)=0.
            FC(i,0)=0.
          enddo
# elif defined TS_VADV_AKIMA
!
!----------------------------------------------------------
! Compute vertical advective fluxes 
! using 4th-order Akima scheme
!----------------------------------------------------------
!
          do k=1,N-1
            do i=istr,iend
              FC(i,k)=t(i,j,k+1,nstp,itrc)-t(i,j,k,nstp,itrc)
            enddo
          enddo
          do i=istr,iend
            FC(i,0)=FC(i,1)
            FC(i,N)=FC(i,N-1)
          enddo
          do k=1,N
            do i=istr,iend
              cff=2.*FC(i,k)*FC(i,k-1)
              if (cff.gt.epsil) then
                CF(i,k)=cff/(FC(i,k)+FC(i,k-1))
              else
                CF(i,k)=0.
              endif
            enddo
          enddo            !--> discard FC
          do k=1,N-1
            do i=istr,iend
              FC(i,k)=0.5*( t(i,j,k,nstp,itrc)+t(i,j,k+1,nstp,itrc)
     &                -0.333333333333*(CF(i,k+1)-CF(i,k)) )*W(i,j,k)
            enddo
          enddo            !--> discard CF
          do i=istr,iend
            FC(i,0)=0.
            FC(i,N)=0.
          enddo
# elif defined TS_VADV_WENO5
!
!----------------------------------------------------------
! Compute vertical advective fluxes 
! using 5th-order WENO scheme
!----------------------------------------------------------
!
          do k=3,N-3
            do i=Istr,Iend
              FC(i,k)=W(i,j,k)*flux6(
     &             t(i,j,k-2,nstp,itrc), t(i,j,k-1,nstp,itrc), 
     &             t(i,j,k  ,nstp,itrc), t(i,j,k+1,nstp,itrc),
     &             t(i,j,k+2,nstp,itrc), t(i,j,k+3,nstp,itrc), W(i,j,k))
            enddo
          enddo
          do i=Istr,Iend
            FC(i,2)=W(i,j,2)*flux4(
     &           t(i,j,1,nstp,itrc), t(i,j,2,nstp,itrc), 
     &           t(i,j,3,nstp,itrc), t(i,j,4,nstp,itrc), W(i,j,2))
            FC(i,N-2)=W(i,j,N-2)*flux4(
     &           t(i,j,N-3,nstp,itrc), t(i,j,N-2,nstp,itrc), 
     &           t(i,j,N-1,nstp,itrc), t(i,j,N  ,nstp,itrc), W(i,j,N-2))
            FC(i,1)=0.5*W(i,j,1)*(t(i,j,1,nstp,itrc)
     &                           +t(i,j,2,nstp,itrc))
            FC(i,N-1)=0.5*W(i,j,N-1)*(t(i,j,N-1,nstp,itrc)
     &                               +t(i,j,N,  nstp,itrc))
            FC(i, 0)=0.
            FC(i,N )=0.
          enddo
# elif defined TS_VADV_C2
!
!----------------------------------------------------------
! Compute vertical advective fluxes
! using 2th-order centered scheme
!----------------------------------------------------------
!
          do k=1,N-1
           do i=Istr,Iend
             FC(i,k)=0.5*W(i,j,k)*(t( i,j,k,nstp,itrc)
     &                           +t(i,j,k+1,nstp,itrc))
           enddo
         enddo
         do i=Istr,Iend
           FC(i, 0)=0.
           FC(i,N )=0.
         enddo
# else
!
!----------------------------------------------------------
! Compute vertical advective fluxes
! using 4th-order centered scheme
!----------------------------------------------------------
!
          do k=2,N-2
            do i=Istr,Iend
              FC(i,k)=W(i,j,k)*(
     &                     0.58333333333333*( t(i,j,k  ,nstp,itrc)
     &                                       +t(i,j,k+1,nstp,itrc))
     &                    -0.08333333333333*( t(i,j,k-1,nstp,itrc)
     &                                       +t(i,j,k+2,nstp,itrc))
     &                                                            )
            enddo
          enddo
          do i=Istr,Iend
            FC(i, 0)=0.0
            FC(i,  1)=W(i,j,  1)*(     0.5*t(i,j,  1,nstp,itrc)
     &                       +0.58333333333333*t(i,j,  2,nstp,itrc)
     &                       -0.08333333333333*t(i,j,  3,nstp,itrc)
     &                                                            )
            FC(i,N-1)=W(i,j,N-1)*(     0.5*t(i,j,N  ,nstp,itrc)
     &                       +0.58333333333333*t(i,j,N-1,nstp,itrc)
     &                       -0.08333333333333*t(i,j,N-2,nstp,itrc)
     &                                                            )
            FC(i,N )=0.0
          enddo
# endif

          if (FIRST_TIME_STEP) then
            cff=0.5*dt
          else
            cff=(1.-gamma)*dt
          endif
          do k=1,N
            do i=Istr,Iend
              t(i,j,k,nnew,itrc)=DC(i,k)*( t(i,j,k,nnew,itrc)
     &               -cff*pm(i,j)*pn(i,j)*(FC(i,k)-FC(i,k-1)))
#ifdef CONST_TRACERS 
              t(i,j,k,nnew,itrc)=t(i,j,k,nstp,itrc)
#endif
            enddo
          enddo            !--> discard FC
        enddo   !<-- itrc  !--> discard DC

!
!======================================================================
!
! Momentum equations: time stepping to time n+1/2
!
!======================================================================
!
        do i=IstrU,Iend
          DC(i,0)=pm_u(i,j)*pn_u(i,j)
        enddo
        if (FIRST_TIME_STEP) then
          do k=1,N
            do i=IstrU,Iend
              u(i,j,k,nnew)=( u(i,j,k,nstp)*(Hz(i,j,k)+Hz(i-1,j,k))
     &                                        +dt*DC(i,0)*ru(i,j,k)
     &                          )/(Hz_half(i,j,k)+Hz_half(i-1,j,k))

              u(i,j,k,indx)=u(i,j,k,nstp)*0.5*(Hz(i,j,k)+
     &                                         Hz(i-1,j,k))
#ifdef NBQ
              ruint_nbq(i,j,k)=DC(i,0)*( ru(i,j,k) - rufrc(i,j)
     &                                      *(Hz(i,j,k)+Hz(i-1,j,k))
     &                                /(Zt_avg1(i,j)+Zt_avg1(i-1,j)) )
#endif
            enddo
          enddo
        else
          cff=2.*(1.-gamma)*dt
          cff1=0.5+gamma
          cff2=0.5-gamma
          do k=1,N
            do i=IstrU,Iend
#ifdef NBQ
              u(i,j,k,nnew)=( cff1*u(i,j,k,nstp)*(Hz(i,j,k)+
     &                                            Hz(i-1,j,k))
     &                       +cff2*u(i,j,k,indx)*(Hz_bak(i,j,k)+
     &                                            Hz_bak(i-1,j,k))
     &                       +cff*DC(i,0)*(ru(i,j,k)+ru_nbq_avg1(i,j,k))
     &                         )/(Hz_half(i,j,k)+Hz_half(i-1,j,k))
#else
              u(i,j,k,nnew)=( cff1*u(i,j,k,nstp)*(Hz(i,j,k)+
     &                                            Hz(i-1,j,k))
     &                       +cff2*u(i,j,k,indx)*(Hz_bak(i,j,k)+
     &                                            Hz_bak(i-1,j,k))
     &                       +cff*DC(i,0)*ru(i,j,k)
     &                         )/(Hz_half(i,j,k)+Hz_half(i-1,j,k))
#endif

              u(i,j,k,indx)=u(i,j,k,nstp)*0.5*(Hz(i,j,k)+
     &                                         Hz(i-1,j,k))

#ifdef NBQ
              ruint_nbq(i,j,k)=DC(i,0)*( ru(i,j,k) - rufrc(i,j)
     &                                      *(Hz(i,j,k)+Hz(i-1,j,k))
     &                                /(Zt_avg1(i,j)+Zt_avg1(i-1,j)) )
#endif
            enddo
          enddo
         endif

        if (j.ge.JstrV) then
          do i=Istr,Iend
            DC(i,0)=pm_v(i,j)*pn_v(i,j)
          enddo
          if (FIRST_TIME_STEP) then
            do k=1,N
              do i=Istr,Iend
                v(i,j,k,nnew)=(v(i,j,k,nstp)*(Hz(i,j,k)+Hz(i,j-1,k))
     &                                         +dt*DC(i,0)*rv(i,j,k)
     &                           )/(Hz_half(i,j,k)+Hz_half(i,j-1,k))

                v(i,j,k,indx)=v(i,j,k,nstp)*0.5*(Hz(i,j  ,k)+
     &                                           Hz(i,j-1,k))
#ifdef NBQ
                rvint_nbq(i,j,k)=DC(i,0)*( rv(i,j,k) - rvfrc(i,j)
     &                                   *(Hz(i,j,k)+Hz(i,j-1,k))
     &                             /(Zt_avg1(i,j)+Zt_avg1(i,j-1)) )
#endif
              enddo
            enddo
          else
            cff=2.*(1.-gamma)*dt
            cff1=0.5+gamma
            cff2=0.5-gamma
            do k=1,N
              do i=Istr,Iend
#ifdef NBQ
                v(i,j,k,nnew)=( cff1*v(i,j,k,nstp)*(Hz(i,j,k)+
     &                                              Hz(i,j-1,k))
     &                         +cff2*v(i,j,k,indx)*(Hz_bak(i,j,k)+
     &                                              Hz_bak(i,j-1,k))
     &                       +cff*DC(i,0)*(rv(i,j,k)+rv_nbq_avg1(i,j,k))
     &                           )/(Hz_half(i,j,k)+Hz_half(i,j-1,k))
#else
                v(i,j,k,nnew)=( cff1*v(i,j,k,nstp)*(Hz(i,j,k)+
     &                                              Hz(i,j-1,k))
     &                         +cff2*v(i,j,k,indx)*(Hz_bak(i,j,k)+
     &                                              Hz_bak(i,j-1,k))
     &                         +cff*DC(i,0)*rv(i,j,k)
     &                           )/(Hz_half(i,j,k)+Hz_half(i,j-1,k))
#endif

                v(i,j,k,indx)=v(i,j,k,nstp)*0.5*(Hz(i,j,k)+
     &                                           Hz(i,j-1,k))
#ifdef NBQ
                rvint_nbq(i,j,k)=DC(i,0)*( rv(i,j,k) - rvfrc(i,j)
     &                                   *(Hz(i,j,k)+Hz(i,j-1,k))
     &                             /(Zt_avg1(i,j)+Zt_avg1(i,j-1)) )
#endif

              enddo
            enddo               !--> discard DC(:,0)
          endif
        endif
      enddo     !<-- j

#ifdef NBQ
!----------------------------------------------------------
!<-- at this point 
!<-- rw            contains internal 3D advection + Coriolis
!<-- rw_nbq_avg1   contains NBQ pressure gradient + gravity 
!                                        + second viscosity 
!----------------------------------------------------------
      do j=Jstr,Jend
!==
      if (FIRST_TIME_STEP) then
!==        
        do k=1,N-1
          do i=Istr,Iend
            wz(i,j,k,nnew)=( wz(i,j,k,nstp)*(Hz(i,j,k)+Hz(i,j,k+1))
     &                                          +dt*pn(i,j)*pm(i,j)
     &                              *(rw(i,j,k)+rw_nbq_avg1(i,j,k))
     &                     )   /  (Hz_half(i,j,k)+Hz_half(i,j,k+1)) !<-- wz has units m.s-1

            wz(i,j,k,indx)=wz(i,j,k,nstp)*0.5*(Hz(i,j,k  )+
     &                                         Hz(i,j,k+1)) !<-- wz(indx) has units kg.m-1.s-1

            rwint_nbq(i,j,k)=pm(i,j)*pn(i,j)*rw(i,j,k) !<-- rwint_nbq has units kg.m-1.s-2

          enddo
        enddo
!== Special treatment for k=N because the control volume at the top is Hz/2       
        do i=Istr,Iend
            wz(i,j,N,nnew)=( wz(i,j,N,nstp)*Hz(i,j,N)
     &                            +dt*pn(i,j)*pm(i,j)
     &                *(rw(i,j,N)+rw_nbq_avg1(i,j,N))
     &                     )        /  Hz_half(i,j,N) !<-- wz has units m.s-1

            wz(i,j,N,indx)=wz(i,j,N,nstp)*0.5*Hz(i,j,N) !<-- wz(indx) has units kg.m-1.s-1

            rwint_nbq(i,j,N)=pm(i,j)*pn(i,j)*rw(i,j,N) !<-- rwint_nbq has units kg.m-1.s-2
 
        enddo        
!==        
      else
!==      
        cff=2.*(1.-gamma)*dt
        cff1=0.5+gamma
        cff2=0.5-gamma
!==        
        do k=1,N-1
          do i=Istr,Iend
             wz(i,j,k,nnew)=( cff1*wz(i,j,k,nstp)*(Hz(i,j,k  )+
     &                                             Hz(i,j,k+1))
     &                       +cff2*wz(i,j,k,indx)*(Hz_bak(i,j,k  )+
     &                                             Hz_bak(i,j,k+1))
     &                       +cff*pn(i,j)*pm(i,j)
     &                              *(rw(i,j,k)+rw_nbq_avg1(i,j,k))
     &                      ) / (Hz_half(i,j,k)+Hz_half(i,j,k+1))  !<-- wz has units m.s-1

             wz(i,j,k,indx)=wz(i,j,k,nstp)*0.5*(Hz(i,j,k  )+
     &                                          Hz(i,j,k+1)) !<-- wz(indx) has units kg.m-1.s-1

             rwint_nbq(i,j,k)=pm(i,j)*pn(i,j)*rw(i,j,k) !<-- rwint_nbq has units kg.m-1.s-2

          enddo
       enddo              

!== Special treatment for k=N because the control volume at the top is Hz/2             
       do i=Istr,Iend
             wz(i,j,N,nnew)=( cff1*wz(i,j,N,nstp)*Hz    (i,j,N)
     &                       +cff2*wz(i,j,N,indx)*Hz_bak(i,j,N)
     &                       +cff*pn(i,j)*pm(i,j)
     &                          *(rw(i,j,N)+rw_nbq_avg1(i,j,N))
     &                      )/ Hz_half(i,j,N)                !<-- wz has units m.s-1

             wz(i,j,N,indx)=wz(i,j,N,nstp)*0.5*Hz(i,j,N)   !<-- wz(indx) has units kg.m-1.s-1

             rwint_nbq(i,j,N)=pm(i,j)*pn(i,j)*rw(i,j,N) !<-- rwint_nbq has units kg.m-1.s-2
       enddo            
!==                  
      endif
      enddo
!==
#endif      
!
!======================================================================
! Set PHYSICAL lateral boundary conditions.
!======================================================================
!
      do itrc=1,NT
        call t3dbc_tile (Istr,Iend,Jstr,Jend, nnew,itrc, WORK)
# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
#  ifdef THREE_GHOST_POINTS_TS
        call exchange_r3d_3pts_tile (Istr,Iend,Jstr,Jend,
     &                               t(START_2D_ARRAY,1,nnew,itrc))
#  else
        call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                          t(START_2D_ARRAY,1,nnew,itrc))
#  endif
# endif
      enddo

      call u3dbc_tile (Istr,Iend,Jstr,Jend, WORK)
      call v3dbc_tile (Istr,Iend,Jstr,Jend, WORK)
# ifdef NBQ
      call w3dbc_tile (Istr,Iend,Jstr,Jend, WORK)      
# endif
!
!======================================================================
! Coupling, include ghost points associated with PHYSICAL
! boundaries ONLY. Do not touch periodic ghost points or
! internal computational margins (MPI code). 
!======================================================================
!
# ifdef EW_PERIODIC
#  define IU_RANGE Istr,Iend
#  define IV_RANGE Istr,Iend
# else
#  define IU_RANGE Istr,IendR
#  define IV_RANGE IstrR,IendR
# endif

# ifdef NS_PERIODIC
#  define JU_RANGE Jstr,Jend
#  define JV_RANGE Jstr,Jend
# else
#  define JU_RANGE JstrR,JendR
#  define JV_RANGE Jstr,JendR
# endif
!
      do j=JU_RANGE                      ! XI-component
        do i=IU_RANGE
          DC(i,0)=0.
          CF(i,0)=0.
        enddo
        do k=1,N,+1
          do i=IU_RANGE
            DC(i,k)=0.5*(Hz(i,j,k)+Hz(i-1,j,k))*on_u(i,j)
            DC(i,0)=DC(i,0)+DC(i,k)
            CF(i,0)=CF(i,0)+DC(i,k)*u(i,j,k,nnew)
          enddo
        enddo
        if (FIRST_TIME_STEP) then
          cff1=1.
          cff2=0.
        else
          cff1=1.5
          cff2=0.5
        endif
        do i=IU_RANGE
          CF(i,0)=( CF(i,0)-cff1*DU_avg1(i,j,nstp)
     &             +cff2*DU_avg1(i,j,indx) )/DC(i,0)
        enddo
        do k=N,1,-1
          do i=IU_RANGE
            u(i,j,k,nnew)=(u(i,j,k,nnew)-CF(i,0))
# ifdef MASKING
     &                                 *umask(i,j)
# endif
          enddo
        enddo
      enddo

      do j=JV_RANGE
          do i=IV_RANGE                   ! ETA-component
            DC(i,0)=0.
            CF(i,0)=0.
          enddo
          do k=1,N,+1
            do i=IV_RANGE
              DC(i,k)=0.5*(Hz(i,j,k)+Hz(i,j-1,k))*om_v(i,j)
              DC(i,0)=DC(i,0)+DC(i,k)
              CF(i,0)=CF(i,0)+DC(i,k)*v(i,j,k,nnew)
            enddo
          enddo
          if (FIRST_TIME_STEP) then
            cff1=1.
            cff2=0.
          else
            cff1=1.5
            cff2=0.5
          endif
          do i=IV_RANGE
            CF(i,0)=( CF(i,0)-cff1*DV_avg1(i,j,nstp)
     &               +cff2*DV_avg1(i,j,indx) )/DC(i,0)
          enddo
          do k=N,1,-1
            do i=IV_RANGE
              v(i,j,k,nnew)=(v(i,j,k,nnew)-CF(i,0))
# ifdef MASKING
     &                                  *vmask(i,j)
# endif
            enddo
          enddo
      enddo

# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend,
     &                        u(START_2D_ARRAY,1,nnew))
      call exchange_v3d_tile (Istr,Iend,Jstr,Jend,
     &                        v(START_2D_ARRAY,1,nnew))
#  ifdef NBQ    
      call exchange_w3d_tile (Istr,Iend,Jstr,Jend,
     &                       wz(START_2D_ARRAY,1,nnew))     
#  endif
# endif

!
!======================================================================
! Prepare to start two-dimensional time stepping:
! set the initial values of the fast-time-step free-surface
! field to its fast-time-averaged values corresponding
! to the time step n (nstp).
!======================================================================
!
# ifdef MPI
      do j=JU_RANGE
        do i=IV_RANGE
          zeta(i,j,knew)=Zt_avg1(i,j)
        enddo
      enddo
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                          zeta(START_2D_ARRAY,knew))
# else
      do j=JstrR,JendR
        do i=IstrR,IendR
          zeta(i,j,knew)=Zt_avg1(i,j)
        enddo
      enddo
# endif
!
# undef IU_RANGE
# undef JU_RANGE
# undef IV_RANGE
# undef JV_RANGE
#else
      subroutine pre_step3d_empty
#endif
      return
      end
