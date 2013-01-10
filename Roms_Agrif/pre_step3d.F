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
#ifdef SOLVE3D

      subroutine pre_step3d (tile)
!
! version pre_step3d4S
!
      implicit none
      integer tile,  trd,omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "ocean3d.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call pre_step3d_tile (Istr,Iend,Jstr,Jend,
     &                              A3d(1,1,trd), A3d(1,2,trd),
     &                A2d(1,1,trd), A2d(1,2,trd), A2d(1,3,trd),
     &                A2d(1,1,trd), A2d(1,2,trd), A2d(1,3,trd),
     &                                            A3d(1,3,trd))
      return
      end

      subroutine pre_step3d_tile (Istr,Iend,Jstr,Jend, ru,rv,
     &                         FC,CF,DC, FX,FE,WORK, Hz_half)
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
     &       FC(PRIVATE_1D_SCRATCH_ARRAY,0:N),  cff2,
     &       CF(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &       DC(PRIVATE_1D_SCRATCH_ARRAY,0:N),  gamma, 
     &       FX(PRIVATE_2D_SCRATCH_ARRAY),      epsil,
     &       FE(PRIVATE_2D_SCRATCH_ARRAY),
     &     WORK(PRIVATE_2D_SCRATCH_ARRAY),
     &  Hz_half(PRIVATE_2D_SCRATCH_ARRAY,N)
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
# ifdef MASKING
#  define SWITCH *
# else
#  define SWITCH !
# endif
!
# if defined TS_HADV_UP5 ||  defined TS_HADV_C6 \
      || defined TS_HADV_WENO5 ||  defined BIO_HADV_WENO5
!
! definition of flux operators, 3rd, 4th, 5th or 6th order
! for UP5 and C6 advection schemes
!
      REAL    :: q_im3, q_im2, q_im1, q_i, q_ip1, q_ip2, ua, vel
      REAL    :: flux3, flux4, flux5, flux6
      REAL    :: flx2, flx3, flx4, flx5
      REAL    :: mask0, mask1, mask2, mask3

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
!===============================================================
!
! Compute horizontal advection
!
!===============================================================
!
      do itrc=1,NT
        do k=1,N

# ifdef BIO_HADV_WENO5
          if (itrc .le. 2) then
# endif

# if defined TS_HADV_UP5 || defined TS_HADV_C6 || defined TS_HADV_WENO5 
!--------------------------------
! Sixth order advection scheme
!--------------------------------
!
#  define FLUX5 flux6
#  define FLUX4 flux4
#  define FLUX3 flux4
#  undef  UP5_MASKING
!
#  include "t3dadv_order5.h"
!
#  undef FLUX5
#  undef FLUX4
#  undef FLUX3
#  undef UP5_MASKING

# else  /*  --> default: C4 */
!----------------------------------------
! Fourth order advection scheme
!----------------------------------------
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
!----------------------------------------------------
! C6 advection for passive tracers 
!----------------------------------------------------
!
#  define FLUX5 flux6
#  define FLUX4 flux4
#  define FLUX3 flux4
#  undef  UP5_MASKING
!
#  include "t3dadv_order5.h"
!
#  undef FLUX5
#  undef FLUX4
#  undef FLUX3
#  undef UP5_MASKING
          endif
# endif

# ifdef PSOURCE
!
!-------------------------------------------------------------------
! Apply point sources for river runoff simulations
!-------------------------------------------------------------------
!
          do is=1,Nsrc
            i=Isrc(is)
            j=Jsrc(is)
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
!--------------------------------------------------------
! Finalize horizontal advection: compute flux divergences
!--------------------------------------------------------
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
!===============================================================
!
! Compute vertical advection
!
!===============================================================
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
                              ! Construct parabolic splines: here
          do i=Istr,Iend      ! CF is the set of vertical derivatives
            FC(i,0)=0.        ! of the tracer field t(:,:,:,nstp,:),
            CF(i,0)=0.        ! FC is an auxiliary scratch variable. 
          enddo
          do k=1,N-1,+1
            do i=Istr,Iend
              cff=1./(2.*Hz(i,j,k+1)+Hz(i,j,k)*(2.-FC(i,k-1)))
              FC(i,k)=cff*Hz(i,j,k+1)
              CF(i,k)=cff*( 6.*( t(i,j,k+1,nstp,itrc)
     &               -t(i,j,k,nstp,itrc))-Hz(i,j,k)*CF(i,k-1))
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
              FC(i,k)=W(i,j,k)*( t(i,j,k,nstp,itrc)+cff*Hz(i,j,k)
     &                                  *(CF(i,k)+0.5*CF(i,k-1)) )
            enddo
          enddo               !--> discard CF
          do i=Istr,Iend
            FC(i,N)=0.
            FC(i,0)=0.
          enddo
# elif defined TS_VADV_AKIMA
!
! Compute vertical advective fluxes using 4th-order Akima scheme
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
# elif TS_VADV_C2
!
! Compute vertical advective fluxes using 2th-order centered scheme
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
! Compute vertical advective fluxes using 4th-order centered scheme
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
c**
c**   if (itrc.eq.2) t(i,j,k,nnew,2)=1.! CONSTANCY PRESERVATION CHECK
c**
#ifdef CONST_TRACERS 
              t(i,j,k,nnew,itrc)=t(i,j,k,nstp,itrc)
#endif
            enddo
          enddo            !--> discard FC
        enddo   !<-- itrc  !--> discard DC

!
!---------------------------------------------------------------
! Momentum equations: time stepping to time n+1/2
!---------------------------------------------------------------
!
        do i=IstrU,Iend
          DC(i,0)=0.25*(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
        enddo
        if (FIRST_TIME_STEP) then
          do k=1,N
            do i=IstrU,Iend
              u(i,j,k,nnew)=( u(i,j,k,nstp)*(Hz(i,j,k)+Hz(i-1,j,k))
     &                                         +dt*DC(i,0)*ru(i,j,k)
     &                           )/(Hz_half(i,j,k)+Hz_half(i-1,j,k))

              u(i,j,k,indx)=u(i,j,k,nstp)*0.5*(Hz(i,j,k)+
     &                                         Hz(i-1,j,k))
            enddo
          enddo
        else
          cff=2.*(1.-gamma)*dt
          cff1=0.5+gamma
          cff2=0.5-gamma
          do k=1,N
            do i=IstrU,Iend
              u(i,j,k,nnew)=( cff1*u(i,j,k,nstp)*(Hz(i,j,k)+
     &                                            Hz(i-1,j,k))
     &                       +cff2*u(i,j,k,indx)*(Hz_bak(i,j,k)+
     &                                            Hz_bak(i-1,j,k))
     &                       +cff*DC(i,0)*ru(i,j,k)
     &                         )/(Hz_half(i,j,k)+Hz_half(i-1,j,k))

              u(i,j,k,indx)=u(i,j,k,nstp)*0.5*(Hz(i,j,k)+
     &                                         Hz(i-1,j,k))
            enddo
          enddo
        endif

        if (j.ge.JstrV) then
          do i=Istr,Iend
            DC(i,0)=0.25*(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
          enddo
          if (FIRST_TIME_STEP) then
            do k=1,N
              do i=Istr,Iend
                v(i,j,k,nnew)=(v(i,j,k,nstp)*(Hz(i,j,k)+Hz(i,j-1,k))
     &                                          +dt*DC(i,0)*rv(i,j,k)
     &                            )/(Hz_half(i,j,k)+Hz_half(i,j-1,k))

                v(i,j,k,indx)=v(i,j,k,nstp)*0.5*(Hz(i,j,k)+
     &                                           Hz(i,j-1,k))
              enddo
            enddo
          else
            cff=2.*(1.-gamma)*dt
            cff1=0.5+gamma
            cff2=0.5-gamma
            do k=1,N
              do i=Istr,Iend
                v(i,j,k,nnew)=( cff1*v(i,j,k,nstp)*(Hz(i,j,k)+
     &                                              Hz(i,j-1,k))
     &                         +cff2*v(i,j,k,indx)*(Hz_bak(i,j,k)+
     &                                              Hz_bak(i,j-1,k))
     &                         +cff*DC(i,0)*rv(i,j,k)
     &                           )/(Hz_half(i,j,k)+Hz_half(i,j-1,k))

                v(i,j,k,indx)=v(i,j,k,nstp)*0.5*(Hz(i,j,k)+
     &                                           Hz(i,j-1,k))
              enddo
            enddo               !--> discard DC(:,0) 
          endif
        endif
      enddo     !<-- j

!
!---------------------------------------------------------------
! Set PHYSICAL lateral boundary conditions.
!---------------------------------------------------------------
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

!
!---------------------------------------------------------------
! Coupling, include ghost points associated with PHYSICAL
! boundaries ONLY. Do not touch periodic ghost points or
! internal computational margins (MPI code). 
!---------------------------------------------------------------
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
# endif

!
!---------------------------------------------------------------
! Prepare to start two-dimensional time stepping:
! set the initial values of the fast-time-step free-surface
! field to its fast-time-averaged values corresponding
! o the time step n (nstp).
!---------------------------------------------------------------
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
