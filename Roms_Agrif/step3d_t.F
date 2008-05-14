!
! $Id: step3d_t.F,v 1.5 2005/10/26 15:18:53 pmarches Exp $
!
#include "cppdefs.h"
#ifdef SOLVE3D

      subroutine step3d_t (tile)
!
! version step3d_t3S
!
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "ocean3d.h"
# include "compute_tile_bounds.h"

      trd=omp_get_thread_num()
      call step3d_t_tile (Istr,Iend,Jstr,Jend,
     &             A2d(1,1,trd), A2d(1,2,trd), A2d(1,3,trd),
     &             A2d(1,4,trd), A2d(1,5,trd), A2d(1,6,trd),
     &                           A2d(1,7,trd), A3d(1,1,trd),
     &     t(START_2D_ARRAY,1,1,1),    Hz(START_2D_ARRAY,1),
     &    Hz_bak(START_2D_ARRAY,1),   z_r(START_2D_ARRAY,1),
     &       z_w(START_2D_ARRAY,0),  Huon(START_2D_ARRAY,1),
     &      Hvom(START_2D_ARRAY,1),     W(START_2D_ARRAY,0)) 
      return
      end

      subroutine step3d_t_tile (Istr,Iend,Jstr,Jend,
     &                       FX,FE, WORK, FC,CF,BC,DC, swdk,
     &                    t, Hz,Hz_bak,z_r,z_w,Huon,Hvom ,W)
      implicit none
# include "param.h"
# include "grid.h"
# include "mixing.h"
# include "climat.h"
# include "scalars.h"
# include "sources.h"
# include "forces.h"
#ifdef DIAGNOSTICS_TS
# include "diagnostics.h"
#endif
      integer Istr,Iend,Jstr,Jend, itrc, i,j,k, indx, kmld
# ifdef PSOURCE
     &       ,is
# endif
#ifdef MPI
     &       ,imin,imax,jmin,jmax
#endif
#ifdef AGRIF
#include "zoom.h"
      real invrrhot
#endif
      real FX(PRIVATE_2D_SCRATCH_ARRAY),
     &     FE(PRIVATE_2D_SCRATCH_ARRAY),   cff,
     &     WORK(PRIVATE_2D_SCRATCH_ARRAY), epsil,
     &     FC(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     CF(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     BC(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     DC(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &     swdk(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &     t(GLOBAL_2D_ARRAY,N,3,NT),
     &     Hz(GLOBAL_2D_ARRAY,N),
     &     Hz_bak(GLOBAL_2D_ARRAY,N),
     &     z_r(GLOBAL_2D_ARRAY,N),
     &     z_w(GLOBAL_2D_ARRAY,0:N),
     &     Huon(GLOBAL_2D_ARRAY,N),
     &     Hvom(GLOBAL_2D_ARRAY,N),
     &     W(GLOBAL_2D_ARRAY,0:N)
      parameter (epsil=1.E-16)
!
# include "compute_auxiliary_bounds.h"
!
!---------------------------------------------------------------
! Compute horizontal advection
!---------------------------------------------------------------
!
# ifdef HADV_UPSTREAM_TS
#  define curv WORK
# else
#  define grad WORK
# endif
!
      do k=1,N
        do itrc=1,NT
# ifdef EW_PERIODIC
#  define I_EXT_RANGE Istr-1,Iend+2
# else
#  ifdef MPI
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
#   define I_EXT_RANGE imin,imax
#  else
#   define I_EXT_RANGE max(Istr-1,1),min(Iend+2,Lm+1)
#  endif
# endif
          do j=Jstr,Jend
            do i=I_EXT_RANGE
              FX(i,j)=(t(i,j,k,3,itrc)-t(i-1,j,k,3,itrc))
# ifdef MASKING
     &                                        *umask(i,j)
# endif
            enddo
          enddo
# undef I_EXT_RANGE
# ifndef EW_PERIODIC
          if (WESTERN_EDGE) then
            do j=Jstr,Jend
              FX(0,j)=FX(1,j)
            enddo
          endif
          if (EASTERN_EDGE) then
#  ifdef MPI
            do j=Jstr,Jend
              FX(Lmmpi+2,j)=FX(Lmmpi+1,j)
            enddo
#  else
             do j=Jstr,Jend
              FX(Lm+2,j)=FX(Lm+1,j)
            enddo
#  endif
          endif
# endif

          do j=Jstr,Jend
            do i=Istr-1,Iend+1
# if defined HADV_UPSTREAM_TS
              curv(i,j)=FX(i+1,j)-FX(i,j)
# elif defined HADV_AKIMA_TS
              cff=2.*FX(i+1,j)*FX(i,j)
              if (cff.gt.epsil) then
                grad(i,j)=cff/(FX(i+1,j)+FX(i,j))
              else
                grad(i,j)=0.
              endif
# else
              grad(i,j)=0.5*(FX(i+1,j)+FX(i,j))
# endif
            enddo
          enddo             !--> discard FX


          do j=Jstr,Jend
            do i=Istr,Iend+1
# ifdef HADV_UPSTREAM_TS
              if (Huon(i,j,k) .gt. 0.) then
                cff=curv(i-1,j)
              else
                cff=curv(i,j)
              endif
              FX(i,j)=0.5*( t(i,j,k,3,itrc)+t(i-1,j,k,3,itrc)
     &                           -0.333333333333*cff )*Huon(i,j,k)
# else
              FX(i,j)=0.5*( t(i,j,k,3,itrc)+t(i-1,j,k,3,itrc)
     &                     -0.333333333333*(grad(i,j)-grad(i-1,j))
     &                                               )*Huon(i,j,k)
# endif
            enddo          !--> discard grad
          enddo

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

          do j=J_EXT_RANGE
            do i=Istr,Iend
              FE(i,j)=(t(i,j,k,3,itrc)-t(i,j-1,k,3,itrc)) 
# ifdef MASKING
     &                                         *vmask(i,j)
# endif
            enddo
          enddo
# undef J_EXT_RANGE
# ifndef NS_PERIODIC
          if (SOUTHERN_EDGE) then
            do i=Istr,Iend
              FE(i,0)=FE(i,1)
            enddo
          endif
          if (NORTHERN_EDGE) then
#  ifdef MPI
            do i=Istr,Iend
              FE(i,Mmmpi+2)=FE(i,Mmmpi+1)
            enddo
#  else
            do i=Istr,Iend
              FE(i,Mm+2)=FE(i,Mm+1)
            enddo
#  endif
          endif
# endif

          do j=Jstr-1,Jend+1
            do i=Istr,Iend
# if defined HADV_UPSTREAM_TS
              curv(i,j)=FE(i,j+1)-FE(i,j)
# elif defined HADV_AKIMA_TS
              cff=2.*FE(i,j+1)*FE(i,j)
              if (cff.gt.epsil) then
                grad(i,j)=cff/(FE(i,j+1)+FE(i,j))
              else
                grad(i,j)=0.
              endif
# else
              grad(i,j)=0.5*(FE(i,j+1)+FE(i,j))
# endif
            enddo
          enddo            !--> discard FE


          do j=Jstr,Jend+1
            do i=Istr,Iend
# ifdef HADV_UPSTREAM_TS
              if (Hvom(i,j,k) .gt. 0.) then
                cff=curv(i,j-1)
              else
                cff=curv(i,j)
              endif
              FE(i,j)=0.5*( t(i,j,k,3,itrc)+t(i,j-1,k,3,itrc)
     &                          -0.333333333333*cff )*Hvom(i,j,k)
#  undef curv
# else
              FE(i,j)=0.5*( t(i,j,k,3,itrc)+t(i,j-1,k,3,itrc)
     &                     -0.333333333333*(grad(i,j)-grad(i,j-1))
     &                                               )*Hvom(i,j,k)
#  undef grad
# endif
            enddo
          enddo            !--> discard grad

# ifdef PSOURCE
!
! Apply point sources for river runoff simulations
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
                    FX(i,j)=Huon(i,j,k)*t(i-1,j,k,3,itrc)
                  elseif (rmask(i,j).eq.1. .and. rmask(i-1,j).eq.0) then
                    FX(i,j)=Huon(i,j,k)*t(i  ,j,k,3,itrc)
                  endif
#  endif
                endif
              else
                if (Lsrc(is,itrc)) then
                  FE(i,j)=Hvom(i,j,k)*Tsrc(is,k,itrc)
#  ifdef MASKING
                else
                  if (rmask(i,j).eq.0 .and. rmask(i,j-1).eq.1) then
                    FE(i,j)=Hvom(i,j,k)*t(i,j-1,k,3,itrc)
                  elseif (rmask(i,j).eq.1 .and. rmask(i,j-1).eq.0) then
                    FE(i,j)=Hvom(i,j,k)*t(i,j  ,k,3,itrc)
                  endif
#  endif
                endif
              endif
            endif
          enddo
# endif
          do j=Jstr,Jend
            do i=Istr,Iend
              t(i,j,k,nnew,itrc)=Hz_bak(i,j,k)*t(i,j,k,nstp,itrc)
     &                     -dt*pm(i,j)*pn(i,j)*( FX(i+1,j)-FX(i,j)
     &                                          +FE(i,j+1)-FE(i,j)
     &                                                           )
            enddo
          enddo                 !-->  discard FX,FE

#ifdef AGRIF
          IF (Agrif_Root()) Then
          MYFX(Istr:Iend+1,Jstr:Jend,k,itrc)=FX(Istr:Iend+1,Jstr:Jend)
          MYFY(Istr:Iend,Jstr:Jend+1,k,itrc)=FE(Istr:Iend,Jstr:Jend+1)
          Else
          invrrhot=1./real(Agrif_Rhot())
          MYFX(Istr:Iend+1,Jstr:Jend,k,itrc)=
     &       MYFX(Istr:Iend+1,Jstr:Jend,k,itrc)
     &       +invrrhot*FX(Istr:Iend+1,Jstr:Jend)
          MYFY(Istr:Iend,Jstr:Jend+1,k,itrc)=
     &       MYFY(Istr:Iend,Jstr:Jend+1,k,itrc)
     &      +invrrhot*FE(Istr:Iend,Jstr:Jend+1)
          EndIF
#endif

#ifdef DIAGNOSTICS_TS
      do j=Jstr,Jend
        do i=Istr,Iend 
          TXadv(i,j,k,itrc) = -(FX(i+1,j)-FX(i,j))
# ifdef MASKING
     &              * rmask(i,j)
# endif
          TYadv(i,j,k,itrc) = -(FE(i,j+1)-FE(i,j))
# ifdef MASKING
     &              * rmask(i,j)
# endif
          enddo
        enddo
#endif /*DIAGNOSTICS_TS*/

        enddo    ! <-- itrc
      enddo    !<-- k

# if defined LMD_SKPP || defined LMD_BKPP
!
!---------------------------------------------------------------
! Compute fraction of the solar shortwave flux "swdk"
! penetrating to grid level depth (at vertical w-points).
! (swdk is used later in this routine)
!---------------------------------------------------------------
!
#  define wrk1 FX
#  define wrk2 FE
      do k=1,N-1
        do j=Jstr,Jend
          do i=Istr,Iend
            wrk1(i,j)=z_w(i,j,k)-z_w(i,j,N)
          enddo
        enddo
        call lmd_swfrac_tile (Istr,Iend,Jstr,Jend,1.,wrk1,wrk2)
        do j=Jstr,Jend
          do i=Istr,Iend
            swdk(i,j,k)=wrk2(i,j)
          enddo
        enddo
      enddo
#  undef wrk1
#  undef wrk2
# endif
!
!---------------------------------------------------------------
! Compute vertical advection
!---------------------------------------------------------------
!
      do j=Jstr,Jend
        do itrc=1,NT
# ifdef VADV_SPLINES_TS
!
! Compute vertical advective fluxes using parabolic splines: 
! FC=W*[spline-interpolated tracer]
!
! Firts construct parabolic splines:
! here CF is the set of vertical derivatives of the tracer field
! t(:,:,:,3,itrc) at the grid-box interfaces, W-points;
! FC is an auxiliary scratch variable.
!
          do i=Istr,Iend
            FC(i,0)=0.
            CF(i,0)=0.
          enddo
          do k=1,N-1,+1
            do i=Istr,Iend
              cff=1./(2.*Hz(i,j,k+1)+Hz(i,j,k)*(2.-FC(i,k-1)))
              FC(i,k)=cff*Hz(i,j,k+1)
              CF(i,k)=cff*(6.*(t(i,j,k+1,3,itrc)-t(i,j,k,3,itrc))
     &                                      -Hz(i,j,k)*CF(i,k-1))
            enddo
          enddo
          do i=Istr,Iend
            CF(i,N)=0.
          enddo
          do k=N-1,1,-1
            do i=Istr,Iend
              CF(i,k)=CF(i,k)-FC(i,k)*CF(i,k+1)
            enddo
          enddo               !--> discard FC
!
          cff=1./3.
          do k=1,N-1
            do i=Istr,Iend
              FC(i,k)=W(i,j,k)*( t(i,j,k,3,itrc)+cff*Hz(i,j,k)
     &                               *(CF(i,k)+0.5*CF(i,k-1)) )
            enddo
          enddo               !--> discard CF
          do i=Istr,Iend
            FC(i,N)=0.
            FC(i,0)=0.
            CF(i,0)=dt*pm(i,j)*pn(i,j)
          enddo
# elif defined VADV_AKIMA_TS
!
! Compute vertical advective fluxes using 4th-order Akima scheme
!
          do k=1,N-1
            do i=istr,iend
              FC(i,k)=t(i,j,k+1,3,itrc)-t(i,j,k,3,itrc)
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
              FC(i,k)=0.5*( t(i,j,k,3,itrc)+t(i,j,k+1,3,itrc)
     &                -0.333333333333*(CF(i,k+1)-CF(i,k)) )*W(i,j,k)
            enddo
          enddo            !--> discard CF
          do i=istr,iend
            FC(i,0)=0.
            FC(i,N)=0.
            CF(i,0)=dt*pm(i,j)*pn(i,j)
          enddo
# elif VADV_C2_TS
!
! Compute vertical advective fluxes using 2th-order centered scheme
!
          do k=1,N-1
            do i=Istr,Iend
              FC(i,k)=0.5*W(i,j,k)*(t( i,j,k,3,itrc)
     &                             +t(i,j,k+1,3,itrc))
            enddo
          enddo
          do i=Istr,Iend
            FC(i, 0)=0.
            FC(i,N )=0.
            CF(i,0)=dt*pm(i,j)*pn(i,j)
          enddo
# else
!
! Compute vertical advective fluxes using 4th-order centered scheme
!
          do k=2,N-2
            do i=Istr,Iend
              FC(i,k)=W(i,j,k)*(0.58333333333333*( t(i,j,k,3,itrc)
     &                                            +t(i,j,k+1,3,itrc))
     &                         -0.08333333333333*( t(i,j,k-1,3,itrc)
     &                                            +t(i,j,k+2,3,itrc))
     &                                                              )
            enddo
          enddo
          do i=Istr,Iend
            FC(i, 0)=0.
            FC(i,  1)=W(i,j,  1)*( 0.5*t(i,j,  1,3,itrc)
     &                         +0.58333333333333*t(i,j,  2,3,itrc)
     &                         -0.08333333333333*t(i,j,  3,3,itrc)
     &                                                           )
            FC(i,N-1)=W(i,j,N-1)*( 0.5*t(i,j,N  ,3,itrc)
     &                         +0.58333333333333*t(i,j,N-1,3,itrc)
     &                         -0.08333333333333*t(i,j,N-2,3,itrc)
     &                                                           )
            FC(i,N )=0.
            CF(i,0)=dt*pm(i,j)*pn(i,j)
          enddo
# endif
!
! Apply vertical advective fluxes.
!
          do k=1,N
            do i=Istr,Iend
              t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)-CF(i,0)*(FC(i,k)
     &                                                    -FC(i,k-1))
            enddo
          enddo               !--> discard FC

#ifdef DIAGNOSTICS_TS
       do k=1,N 
         do i=Istr,Iend
           TVadv(i,j,k,itrc) = -(FC(i,k)-FC(i,k-1))
#ifdef MASKING
     &         * rmask(i,j)
#endif
         enddo
       enddo
#endif /*DIAGNOSTICS_TS*/  

!
!---------------------------------------------------------------
! Compute surface and Bottom forcing
!---------------------------------------------------------------
!
!
! Add top and bottom fluxes
!
          do i=Istr,Iend
            FC(i,N)=dt*stflx(i,j,itrc)
            FC(i,0)=-dt*btflx(i,j,itrc)            
          enddo
!
! Add solar radiation flux in temperature equation
! Also compute the nonlocal transport flux for unstable
! (convective) forcing conditions into matrix DC when using
! the Large et al. 1994 KPP scheme.
!
          if (itrc.eq.itemp) then
            do k=1,N-1
              do i=Istr,Iend
                FC(i,k)=0.
# if defined LMD_SKPP || defined LMD_BKPP
     &                 +dt*srflx(i,j)*swdk(i,j,k)
#  ifdef LMD_NONLOCAL
     &                 -dt*Akt(i,j,k,itemp)*ghats(i,j,k)
#  endif
# endif
              enddo
            enddo
          elseif (itrc.eq.isalt) then
            do k=1,N-1
              do i=Istr,Iend
                FC(i,k)=0.
# if defined LMD_SKPP || defined LMD_BKPP
#  ifdef LMD_NONLOCAL
     &                 -dt*Akt(i,j,k,isalt)*ghats(i,j,k)
#  endif
# endif
              enddo
            enddo
          endif
          if (itrc.eq.itemp .or. itrc.eq.isalt) then
            do k=1,N
              do i=Istr,Iend
                t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)+FC(i,k )
     &                                               -FC(i,k-1)
#  ifdef DIAGNOSTICS_TS
                Tforc(i,j,k,itrc)=(FC(i,k)-FC(i,k-1))
     &                                    /(dt*pm(i,j)*pn(i,j))
#   ifdef MASKING
     &                                             * rmask(i,j)
#   endif           
#  endif  /* DIAGNOSTICS_TS */
              enddo
            enddo
          endif

!
!---------------------------------------------------------------
! Compute vertical mixing
!---------------------------------------------------------------
!
! The vertical mixing term is not added as an explicit divergence term
! like the other terms due to the semi-implicit Crank-Nicholson scheme. 
! We store first t(i,j,k,nnew,itrc) which contains both the old concentration 
! of tracer (depth is already factored in here) and the right hand 
! side terms of the explicit time stepping. The new concentration is
! added after resolving tridiagonal system to obtain the vertical mixing
! term by substracting the right hand side terms to the time rate of change
! (see scrum manual p16)
!
! Crank-Nicholson scheme:
!
!   dq(k)     1     [         q(k+1)-q(k)             q(k)-q(k-1) ]
!  ------ = ----- * [ Akt(k)* ----------- - Akt(k-1)* ----------- ]
!    dt     Hz(k)   [            dz(k)                   dz(k-1)  ]
!
! where q(k) represents tracer field t(:,:,k,:,itrc). Doing so
! implies solution of a tri-diagonal system
!
!     -FC(k-1)*q_new(k-1) +[Hz(k)+FC(k-1)+FC(k)]*q_new(k)
!                       -FC(k)*q_new(k+1) = Hz(k)*q_old(k)
!
!                dt*Akt(k)
! where FC(k) = ----------- is normalized diffusivity coefficient
!                  dz(k)
!
! defined at W-points; q_new(k) is the new-time-step (unknown) tracer
! field; q_old(k) is old-time-step tracer (known).  As long as
! vertical diffusivity Akt(k) is nonnegative, the tri-diagonal matrix
! is diagonally dominant which guarantees stability of a Gaussian
! elimination procedure, (e.g., Richtmeyer annd  Morton, 1967).
! Top and bottom boundary conditions are assumed to be no-flux,
! effectively Akt(N)=Akt(0)=0, hence FC(N)=FC(1)=0. This leads to
! equations for top and bottom grid boxes;
!
!   -FC(N-1)*q_new(N-1) +[Hz(N)+FC(N-1)]*q_new(N) = Hz(N)*q_old(N)
!
!          [Hz(1)+FC(1)]*q_new(1) -FC(1)*q_new(2) = Hz(1)*q_old(1)
!
! The FC(N)=FC(0)=0 boundary conditions does not mean that physical
! boundary conditions are no flux: the forcing fluxes have been
! applied explicitly above.  Instead, the no-flux condition should
! be interpreted as that the implicit step merely redistributes the
! tracer concentration throughout the water column. At this moment
! the content of array t(:,:,:,nnew,itrc) has meaning of Hz*tracer.
! After the implicit step it becomes just tracer.
!
#ifdef DIAGNOSTICS_TS
          do k=1,N
            do i=Istr,Iend
               TVmix(i, j, k, itrc)=t(i,j,k,nnew,itrc)
            enddo
         enddo
# endif /* DIAGNOSTICS_TS */
!
! Compute off-diagonal matrix coefficients FC [dt*Akt/Hz] for the
! implicit vertical diffusion terms at future time step, located at
! horizontal RHO-points and vertical W-points.
! Also set FC at the top and bottom levels.
!
# ifdef SALINITY
          indx=min(itrc,isalt)
# else
          indx=min(itrc,itemp)
# endif
          
          do k=1,N-1
            do i=Istr,Iend
              FC(i,k)=-dt*Akt(i,j,k,indx)/(z_r(i,j,k+1)-z_r(i,j,k))
            enddo
          enddo
          do i=Istr,Iend
            FC(i,0)=0.
            FC(i,N)=0.
          enddo
!
! Compute diagonal matrix coefficients BC.
! Also load right-hand-side terms for the tracer equation into DC.
!
          do k=1,N
            do i=Istr,Iend
              BC(i,k)=Hz(i,j,k)-FC(i,k)-FC(i,k-1)
              DC(i,k)=t(i,j,k,nnew,itrc)
            enddo  
          enddo
!
! Solve the tridiagonal system. Although it is mathematically
! redundunt to keep the solution in the private array DC, and
! then just write it back to shared target array t [instead of
! just using array t as the workspace/output], it is preferred
! to do it  this way in order to avoid read-write operations
! out/in  to the shared array in order to minimize the effect
! of false cache_line sharing in the case if there is blocking
! in XI-direction.
!
          do i=Istr,Iend
            cff=1./BC(i,1)
            CF(i,1)=cff*FC(i,1)
            DC(i,1)=cff*DC(i,1)
          enddo
          do k=2,N-1
            do i=Istr,Iend
              cff=1./(BC(i,k)-FC(i,k-1)*CF(i,k-1))
              CF(i,k)=cff*FC(i,k)
              DC(i,k)=cff*(DC(i,k)-FC(i,k-1)*DC(i,k-1))
            enddo
          enddo
          do i=Istr,Iend
             DC(i,N)=(DC(i,N)-FC(i,N-1)*DC(i,N-1))/
     &               (BC(i,N)-FC(i,N-1)*CF(i,N-1))
             t(i,j,N,nnew,itrc)=DC(i,N)
          enddo
          do k=N-1,1,-1
            do i=Istr,Iend
              DC(i,k)=DC(i,k)-CF(i,k)*DC(i,k+1)
              t(i,j,k,nnew,itrc)=DC(i,k)
            enddo
          enddo

# ifdef DIAGNOSTICS_TS
          do k=1,N
            do i=Istr,Iend
              TVmix(i,j,k,itrc) = 
     &            -(TVmix(i,j,k,itrc)-t(i,j,k,nnew,itrc)*Hz(i,j,k))
     &                                        /(dt*pm(i,j)*pn(i,j))
#  ifdef MASKING
     &                                                 * rmask(i,j)
#  endif
            enddo
          enddo                 
# endif /* DIAGNOSTICS_TS */

# ifdef CONST_TRACERS
          do k=1,N
            do i=Istr,Iend
              t(i,j,k,nnew,itrc)=t(i,j,k,nstp,itrc)
            enddo
          enddo
#  ifdef DIAGNOSTICS_TS
          do k=1,N
            do i=Istr,Iend
               TVmix(i,j,k,itrc)=0.0
#   ifdef MASKING
     &              * rmask(i,j)
#   endif
            enddo
          enddo
#  endif /* DIAGNOSTICS_TS */
# endif /* CONST_TRACERS */
        enddo      ! <-- itrc

# if defined FLOATS && defined  RANDOM_WALK
!---------------------------------------------------------------
!  Compute vertical gradient in vertical T-diffusion coefficient
!  for floats random walk.
!---------------------------------------------------------------
          do k=1,N
            do i=Istr,Iend
              dAktdz(i,j,k)=(Akt(i,j,k,1)-Akt(i,j,k-1,1))/Hz(i,j,k)
            enddo
          enddo
# endif
      enddo      ! <-- j
!
!---------------------------------------------------------------
! Set lateral boundary conditions; nudge toward tracer climatology;
! apply land-sea mask and exchange periodic boundary conditions.
!---------------------------------------------------------------
!
      do itrc=1,NT
        call t3dbc_tile (Istr,Iend,Jstr,Jend, nnew,itrc, WORK)
      enddo

# ifdef BIOLOGY
!
!---------------------------------------------------------------
! Compute biological fluxes
!---------------------------------------------------------------
!
      call biology_tile (Istr,Iend,Jstr,Jend)
# endif

      do itrc=1,NT
!
# if defined TNUDGING && defined TCLIMATOLOGY
!
!---------------------------------------------------------------
! Compute Nudging terms
!---------------------------------------------------------------
!
        do k=1,N
          do j=Jstr,Jend
            do i=Istr,Iend
              t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)
     &                    +dt*Tnudgcof(i,j,k,itrc)*(tclm(i,j,k,itrc)
     &                                           -t(i,j,k,nnew,itrc))
#  ifdef DIAGNOSTICS_TS
              Tforc(i,j,k,itrc)=(Tforc(i,j,k,itrc)
     &                    +Tnudgcof(i,j,k,itrc)
     &                    *(tclm(i,j,k,itrc)-t(i,j,k,nnew,itrc))
     &                    *(Hz(i,j,k)/(pm(i,j)*pn(i,j))))
#   ifdef MASKING
     &                    *rmask(i,j)
#   endif
#  endif  /* DIAGNOSTICS_TS */
            enddo
          enddo
        enddo
# endif /* TNUDGING && TCLIMATOLOGY */
!
# ifdef DIAGNOSTICS_TS
!
!---------------------------------------------------------------
! Compute the tendency term of tracer diagnostics
! Divide all diagnostic terms by the cell volume
! (Hz(i,j,k,itrc)/(pm(i,j).*pn(i,j)). There after the unit
! of diagnostic terms will be: (unit of tracers)* s-1.
!
! Note: the Horizontal mixing term is computed in t3dmix
! where Trate is updated accordingly
!---------------------------------------------------------------
!
       do k=1,N
         do j=Jstr,Jend
           do i=Istr,Iend
              Trate(i,j,k,itrc)=(Hz(i,j,k)*t(i,j,k,nnew,itrc)
     &                          -Hz_bak(i,j,k)*t(i,j,k,nstp,itrc))
     &                                       /(dt*pm(i,j)*pn(i,j))
#  ifdef MASKING
     &                   *rmask(i,j)
#  endif
!
              TXadv(i,j,k,itrc)=TXadv(i,j,k,itrc)+
     &           t(i,j,k,3,itrc)*(Huon(i+1,j,k)-Huon(i,j,k))
              TYadv(i,j,k,itrc)=TYadv(i,j,k,itrc)+
     &           t(i,j,k,3,itrc)*(Hvom(i,j+1,k)-Hvom(i,j,k))
              TVadv(i,j,k,itrc)=TVadv(i,j,k,itrc)-
     &           t(i,j,k,3,itrc)*(Huon(i+1,j,k)-Huon(i,j,k)+
     &                            Hvom(i,j+1,k)-Hvom(i,j,k))
!
              cff=pm(i,j)*pn(i,j)/Hz(i,j,k)
              TXadv(i,j,k,itrc)=TXadv(i,j,k,itrc)*cff
              TYadv(i,j,k,itrc)=TYadv(i,j,k,itrc)*cff
              TVadv(i,j,k,itrc)=TVadv(i,j,k,itrc)*cff
              TVmix(i,j,k,itrc)=TVmix(i,j,k,itrc)*cff
              Tforc(i,j,k,itrc)=Tforc(i,j,k,itrc)*cff
              Trate(i,j,k,itrc)=Trate(i,j,k,itrc)*cff
           enddo
         enddo
       enddo
!
!---------------------------------------------------------------
! Compute tracer diagnostics averaged over the MLD
!---------------------------------------------------------------
!
       do j=Jstr,Jend
         do i=Istr,Iend
            TXadv_mld(i,j,itrc)=0.
            TYadv_mld(i,j,itrc)=0.
            TVadv_mld(i,j,itrc)=0.
            TVmix_mld(i,j,itrc)=0.
            Tforc_mld(i,j,itrc)=0.
            Trate_mld(i,j,itrc)=0.
         enddo
       enddo
       do j=Jstr,Jend
         do i=Istr,Iend
#  ifdef LMD_SKPP
           kmld=kbl(i,j)
#  else
           kmld=N-5
#  endif
           do k=N,kmld,-1
             cff=Hz(i,j,k)/(z_w(i,j,N)-z_w(i,j,kmld-1))
             TXadv_mld(i,j,itrc)=TXadv_mld(i,j,itrc)+
     &                           TXadv(i,j,k,itrc)*cff
             TYadv_mld(i,j,itrc)=TYadv_mld(i,j,itrc)+
     &                           TYadv(i,j,k,itrc)*cff
             TVadv_mld(i,j,itrc)=TVadv_mld(i,j,itrc)+
     &                           TVadv(i,j,k,itrc)*cff
             TVmix_mld(i,j,itrc)=TVmix_mld(i,j,itrc)+
     &                           TVmix(i,j,k,itrc)*cff
             TForc_mld(i,j,itrc)=Tforc_mld(i,j,itrc)+
     &                           Tforc(i,j,k,itrc)*cff
             Trate_mld(i,j,itrc)=Trate_mld(i,j,itrc)+
     &                           Trate(i,j,k,itrc)*cff
           enddo
         enddo
       enddo
       do j=Jstr,Jend
         do i=Istr,Iend
            Tentr_mld(i,j,itrc)=Trate_mld(i,j,itrc)-
     &                          TXadv_mld(i,j,itrc)-
     &                          TYadv_mld(i,j,itrc)-
     &                          TVadv_mld(i,j,itrc)-
     &                          TVmix_mld(i,j,itrc)-
     &                          Tforc_mld(i,j,itrc)
         enddo
       enddo
# endif /* DIAGNOSTICS_TS */


# if defined MASKING
#  ifdef EW_PERIODIC
#   define I_RANGE Istr,Iend
#  else
#   define I_RANGE IstrR,IendR
#  endif
#  ifdef NS_PERIODIC
#   define J_RANGE Jstr,Jend
#  else
#   define J_RANGE JstrR,JendR
#  endif
        do k=1,N
          do j=J_RANGE
            do i=I_RANGE
              t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)*rmask(i,j)
            enddo
          enddo
        enddo
#  undef I_RANGE
#  undef J_RANGE
# endif /* MASKING */

# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
        call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                          t(START_2D_ARRAY,1,nnew,itrc))
# endif

      enddo  ! <-- itrc

#else
      subroutine step3d_t_empty
#endif  /* SOLVE3D */
      return
      end
