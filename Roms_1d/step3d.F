! $Id$
!======================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
! 
! ROMS_AGRIF website : http://www.romsagrif.org
!======================================================================
#include "cppdefs.h"

      subroutine step3d
      implicit none
#include "param.h"
#include "grid.h"
#include "ocean3d.h"
#include "scalars.h"
#include "mixing.h"
#include "forces.h"

      integer k,itrc
      real Hz_inv(0:N), FC(0:N),BC,CF(0:N), DCu(0:N),DCv(0:N),
     &                  cff,cff1,cff2,cff3,cff6,swdk,lambda,
     &                  cu
      parameter (lambda=0.501)

      real One, Two, Three, Half, ThreeHalfth
      parameter(One=1., Two=2., Three=3., Half=0.5, ThreeHalfth=1.5)
#ifdef UPWELLING
      real zwmax
      parameter(zwmax=-100.)
#  ifdef PARAB_W 
      real cffa1,cffb1,cffa2,cffb2,cffc2
#  endif
#endif
      do k=1,N
        Hz_inv(k)=ods/Hz(k)
      enddo
!
! Set vertical velocity profile:
!
#ifdef UPWELLING
!
! vertical upwelling velocities
! (sinusoidal, linear, or parabolic profile)
!
#  ifdef SINUS_W
      cff1=0.5*pi/zwmax
      cff2=0.5*pi/(zwmax-z_w(0))
      do k=0,N
        if (z_w(k).gt.zwmax) then
          w(k)= upwi*sin(cff1*z_w(k))
        else
          w(k)=upwi*sin(cff2*(z_w(k)-z_w(0)))
        endif
      enddo
#  elif defined LINEAR_W 
      cff1 = upwi/zwmax
      cff2 = upwi/(zwmax-z_w(0))
      do k=0,N
        if (z_w(k).gt.zwmax) then
          w(k)=z_w(k)*cff1
        else
          w(k)=(z_w(k)-z_w(0))*cff2
        endif
      enddo
#  elif defined PARAB_W 
      cffa1=-upwi/(zwmax*zwmax)
      cffb1=2*upwi/zwmax
      cffa2=-upwi/((z_w(0)-zwmax)*(z_w(0)-zwmax))
      cffb2=2*upwi*zwmax/((z_w(0)-zwmax)*(z_w(0)-zwmax))
      cffc2=upwi+cffa2*zwmax*zwmax
      do k=0,N
        if (z_w(k).gt.zwmax) then
          w(k)=cffa1*z_w(k)*z_w(k)+cffb1*z_w(k)
        else
          w(k)=cffa2*z_w(k)*z_w(k)+cffb2*z_w(k)+cffc2
        endif
      enddo
#  endif
#else
      do k=0,N
         w(k)=0.0
      enddo
#endif
      do itrc=1,NT
#define SPLINES
#ifdef SPLINES
!
! Compute vertical advection terms using conservative parabolic
! spline reconstruction of the tracer field.
!
#define NEUMANN
#define r CF
#if defined NEUMANN
          FC(1)=Half
          r(0)=ThreeHalfth*t(1,nstp,itrc)
#elif defined LINEAR_CONTINUATION
          FC(1)=One
          r(0)=Two*t(1,nstp,itrc)*day2sec+
#elif defined PARABOLIC_CONTINUATION
          cff=Hz(1)/Hz(2)
          FC(1)=One+cff
          r(0)=Two*t(1,nstp,itrc)+cff*( t(1,nstp,itrc)
     &                    +cff*t(2,nstp,itrc) )/FC(1)
#endif
          do k=1,N-1,+1
            cff=One/(Two*Hz(k)+Hz(k+1)*(Two-FC(k)))
            FC(k+1)=cff*Hz(k)
            r(k)=cff*( Three*( t(k+1,nstp,itrc)*Hz(k)
     &                               +t(k,nstp,itrc)*Hz(k+1))
     &                                       -Hz(k+1)*r(k-1))
          enddo
#if defined PARABOLIC_CONTINUATION
          cff=Hz(N)/Hz(N-1)
          cff1=One+cff
          r(N)=( cff*(t(N,nstp,itrc)+cff*t(N-1,nstp,itrc))
     &               +cff1*(Two*t(N,nstp,itrc)-cff1*r(N-1))
     &                            )/(cff1*(One-cff1*FC(N)))
#elif defined LINEAR_CONTINUATION
          r(N)=(  Two*t(N,nstp,itrc)-r(N-1))/(One-FC(N))
#elif defined NEUMANN
          r(N)=(Three*t(N,nstp,itrc)-r(N-1))/(Two-FC(N))
#endif
          do k=N-1,0,-1
            r(k)=r(k)-FC(k+1)*r(k+1)
          enddo
                                          !--> discard FC

          do k=1,N-1
            if (w(k).gt.0.) then
              cff=Hz_inv(k)
              cff1=r(k-1) 
              cff2=t(k,nstp,itrc)
            else
              cff=Hz_inv(k+1)
              cff1=r(k+1)
              cff2=t(k+1,nstp,itrc)
            endif
            cu=dt*abs(w(k))*cff
            FC(k)=w(k)*( r(k) -0.5*cu*( r(k)-cff1
     &                  +(3.-2.*cu)*(r(k)+cff1-2.*cff2) 
     &                                               )) 
          enddo                       !--> discard r
          FC(0)=0.
          FC(N)=0.
#undef r
#else
!
! Compute vertical advection terms. QUICKEST [Leonard, 1979]
! flux-integrated scheme is used to compute FCs.
!
        do k=2,N-1
          CF(k)=t(k+1,nstp,itrc)-2.*t(k,nstp,itrc)+t(k-1,nstp,itrc)
        enddo
        CF(1)=t(2,nstp,itrc)-t(1,nstp,itrc)
        CF(N)=t(N-1,nstp,itrc)-t(N,nstp,itrc)
        do k=1,N-1
          if (w(k).gt.0.) then
            cff=CF(k)
          else
            cff=CF(k+1)
          endif
          cu=w(k)*dt*2.*ods/(Hz(k)+Hz(k+1))
          FC(k)=w(k)*0.5*( t(k+1,nstp,itrc)+t(k,nstp,itrc)
     &                -cu*(t(k+1,nstp,itrc)-t(k,nstp,itrc))
     &                 -0.3333333333333333*(1.-cu*cu)*cff )
        enddo
        FC(0)=0.                             !--> discard CF
        FC(N)=0.
#endif
!
! At the final stage the updated tracer field is
! divided by Hz(k)-dt*(W(k+1/2)-W(k-1/2)) instead of just Hz(k)
! to compensate for the effects of vertical divergence/convergence
! of vertical velocity. This is equivalent to the presence of
! compensating horizontal convergence/divergence.
!
        do k=1,N
          t(k,nnew,itrc)=( t(k,nstp,itrc)-dt*Hz_inv(k)
     &                                 *(FC(k)-FC(k-1))
     &                )/( 1.-dt*Hz_inv(k)*(w(k)-w(k-1)))
        enddo
                            !--> discard FC
!
! Vertical diffusivity terms, compute: (i) surface heat forcing
! [short wave radiation flux "srflx" is assumed to be included into
! net surface heat flux forcing "stflx"]; (ii) radiative penetrative
! heat flux attenuated by the fraction "swdk" penetrating to the grid
! level depth (defined at vertical w-points); (iii) nonlocal
! transport flux for unstable (convective) forcing conditions,
! Large et al. 1994 KPP model.
!
#ifdef LMD_KPP
        CF(N)=stflx(itrc)
        if (itrc.eq.itemp) then
          do k=N-1,1,-1
            call lmd_swfrac (+1.,z_w(k),swdk)
            CF(k)=srflx*swdk
# ifdef LMD_NONLOCAL
     &            -ghats(k)*(stflx(itemp)-srflx)
# endif
          enddo
        else
          do k=N-1,1,-1
# ifdef LMD_NONLOCAL
            CF(k)=-ghats(k)*stflx(itrc)
# else
            CF(k)=0.
# endif
          enddo
        endif
        CF(0)=0.
        do k=1,N
          t(k,nnew,itrc)=t(k,nnew,itrc)+dt*Hz_inv(k)*(CF(k)-CF(k-1))
        enddo
#else
        t(N,nnew,itrc)=t(N,nnew,itrc)+dt*Hz_inv(N)*stflx(itrc)
#endif
#define DC DCu
!
! vertical diffusion flux computation
!
        cff6=ds/6.
        do k=1,N-1
          FC(k)=cff6*Hz(k  )-dt*Akt(k-1,itrc)*Hz_inv(k  )
          CF(k)=cff6*Hz(k+1)-dt*Akt(k+1,itrc)*Hz_inv(k+1)
        enddo

        CF(0)=0.
        DC(0)=0.

        cff3=ds/3.
        do k=1,N-1,+1               !<-- irreversible 
          BC=cff3*(Hz(k)+Hz(k+1))+dt*Akt(k,itrc)*(
     &                     Hz_inv(k )+Hz_inv(k+1) )
          cff=1./(BC-FC(k)*CF(k-1))
          CF(k)=cff*CF(k)
          DC(k)=cff*( t(k+1,nnew,itrc)-t(k,nnew,itrc)
     &                                -FC(k)*DC(k-1) )
        enddo

        DC(N)=0.
        do k=N-1,1,-1               !<-- irreversible 
          DC(k)=DC(k)-CF(k)*DC(k+1)
        enddo                       !--> discard CF

        do k=1,N-1,+1                 !<-- irreversible
           FC(k)=Akt(k,itrc)*0.166666666666667*(
     &           5.*DC(k)+(Hz(k+1)*DC(k-1)+Hz(k)*DC(k+1))
     &                                   /(Hz(k)+Hz(k+1))
     &                                                  )
        enddo
        FC(0)=0.
        FC(N)=0.

        do k=1,N
#ifdef NUDGE
           if (itrc.le.3) then ! nudge only T/S/NO3
              cff1=delta(k)*(tnudge(k,itrc)-t(k,nnew,itrc))
           else
              cff1=0.
           endif
           t(k,nnew,itrc)=t(k,nnew,itrc)+dt*(
     &                     cff1+Hz_inv(k)*(FC(k)-FC(k-1)))
#else
           t(k,nnew,itrc)=t(k,nnew,itrc)+dt*Hz_inv(k)*(FC(k)-FC(k-1))
#endif
        enddo
#if defined UPWELLING && defined EXPORT
!
! lateral export term (accounting for horizontal fluxes that
! balance the local forcing away from the area)
! dT/dt = -U/L delta(T)
!
        cff = max(upwi/(-zwmax),0.)   ! term U/L
        do k=1,N
         if (z_r(k).ge.zwmax) then
           t(k,nnew,itrc)=t(k,nnew,itrc)+
     &                    dt*cff*(tnudge(k,itrc)-t(k,nnew,itrc))
          endif  
        enddo 
#endif        
      enddo   ! <-- itrc
#undef DC

!
! Momentum equations: Coriolis terms and vertical viscosity
!-------------------- -------------------------------------
!
!
      cff=1./(1.+(lambda*dt*f)**2)               !< Weighed Crank-Nicholson
      cff1=cff*(1.-lambda*(1.-lambda)*(dt*f)**2) !  lambda=1. ==> Backward Euler
      cff2=cff*dt*f                              !  lambda=0.5 ==> Crank-Nicholson

      do k=1,N
        u(k,nnew)=cff1*u(k,nstp)+cff2*v(k,nstp)
        v(k,nnew)=cff1*v(k,nstp)-cff2*u(k,nstp)
      enddo
!
! Vertical viscosity
!
      u(N,nnew)=u(N,nnew)+dt*Hz_inv(N)*sustr   ! add wind stress
      v(N,nnew)=v(N,nnew)+dt*Hz_inv(N)*svstr   ! forcing at top 

      cff6=ds/6.
      do k=1,N-1
        FC(k)=cff6*Hz(k  )-dt*Kv(k-1)*Hz_inv(k  )
        CF(k)=cff6*Hz(k+1)-dt*Kv(k+1)*Hz_inv(k+1)
      enddo

      CF(0)=0.
      DCu(0)=0.
      DCv(0)=0.

      cff3=ds/3.
      do k=1,N-1,+1               !<-- irreversible
        BC=cff3*(Hz(k)+Hz(k+1))+dt*Kv(k)*(
     &                   Hz_inv(k )+Hz_inv(k+1) )
        cff=1./(BC-FC(k)*CF(k-1))
        CF(k)=cff*CF(k)
        DCu(k)=cff*(u(k+1,nnew)-u(k,nnew) - FC(k)*DCu(k-1))
        DCv(k)=cff*(v(k+1,nnew)-v(k,nnew) - FC(k)*DCv(k-1))
      enddo

      DCu(N)=0.
      DCv(N)=0.
      do k=N-1,1,-1               !<-- irreversible
        DCu(k)=DCu(k)-CF(k)*DCu(k+1)
        DCv(k)=DCv(k)-CF(k)*DCv(k+1)
      enddo                           !--> discard CF

      do k=1,N,+1                 !<-- irreversible
        DCu(k)=DCu(k)*Kv(k)
        u(k,nnew)=u(k,nnew)+dt*Hz_inv(k)*(DCu(k)-DCu(k-1))
        DCv(k)=DCv(k)*Kv(k)
        v(k,nnew)=v(k,nnew)+dt*Hz_inv(k)*(DCv(k)-DCv(k-1))
      enddo

      return
      end




