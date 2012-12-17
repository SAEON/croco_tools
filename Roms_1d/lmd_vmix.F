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
#ifdef LMD_MIXING

      subroutine lmd_vmix
!
!--------------------------------------------------------------------
!  This subroutine computes vertical mixing coefficients for momentum
!  and tracers at the ocean interior using the Large, McWilliams and
!  Doney (1994) mixing scheme.
!
!  On Output:
!     Kv       vertical viscosity coefficient [m^2/s].
!     Kt       vertical diffusion coefficient for potential
!                 temperature [m^2/s].
!     Ks       vertical diffusion coefficient for salinity [m^2/s].
!
!  Reference:
!
!    Large, W.G., J.C. McWilliams, and S.C. Doney, 1994: A Review
!    and model with a nonlocal boundary layer parameterization,
!    Reviews of Geophysics, 32,363-403.
!--------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "mixing.h"
# include "scalars.h"

      real Ri0, nuwm, nuws,  nu0m,   nu0s,  nu0c, nu,   Rrho0,
     &     nuf, fdd,  tdd1,  tdd2,   tdd3,  sdd1, sdd2, sdd3, eps

      parameter (        ! Critical gradient Richardson number
     &     Ri0=0.7,      ! below which turbulent mixing occurs.
                         !
     &     nuwm=1.0e-4,  ! Interior viscosity and diffusivity 
     &     nuws=1.0e-5,  !   due to wave breaking, [m^2/s] 
                         !
     &     nu0m=50.e-4,  ! Maximum interior viscosity and dif-
     &     nu0s=50.e-4,  ! fusivity due to shear instability,
                         !                             [m^2/s];
                         ! Maximum interior convective 
     &     nu0c=0.1,     ! viscosity and  diffusivity due
                         ! to static instability, [m^2/s];
                         !
     &     nu=1.5e-6,    ! Molecular viscosity [m^2/s];
                         !
                         ! Value of double-diffusive density 
     &     Rrho0=1.9,    ! ratio where diffusivity goes to 
                         !              zero in salt fingering.
     &     nuf=10.0e-4,  ! Scaling factors for double diffusion
     &     fdd=0.7,      ! coefficient in salt fingering.
                         !
     &     tdd1=0.909,   !
     &     tdd2=4.6,     ! Double diffusion constants 
     &     tdd3=0.54,    ! for temperature (Marmorino and 
     &     sdd1=0.15,    ! Caldwell, 1976) and salinity 
     &     sdd2=1.85,    ! (Fedorov, 1988).
     &     sdd3=0.85)    !

      integer k
# ifdef LMD_RIMIX
      real nu_sx, RiRi0
# endif

#undef SPLINES

# ifdef SPLINES
      real FC(0:N),dR(0:N),dU(0:N),dV(0:N),cff,cff6
# endif

# ifdef LMD_DDMIX
      real Rrho, ddDS, ddDT, nu_dds, nu_ddt, alfaobeta, Tt, Ts, Tp

      real A0,A1,A2,A3,A4, B0,B1, C0, D0,D1,D2, E0,F0,G0,H0, Smean
      parameter(A0=+0.665157E-01, A1=+0.170907E-01, A2=-0.203814E-03,
     &          A3=+0.298357E-05, A4=-0.255019E-07, B0=+0.378110E-02,
     &          B1=-0.846960E-04, C0=-0.678662E-05, D0=+0.380374E-04,
     &          D1=-0.933746E-06, D2=+0.791325E-08, E0=-0.164759E-06,
     &          F0=-0.251520E-11, G0=+0.512857E-12, H0=-0.302285E-13,
     &          Smean=35.0)
# endif
      parameter (eps=1.E-14)
# define tind nstp

!
! Compute interior viscosities and diffusivities everywhere as the
! superposition of: (i) internal wave breaking, (ii) local Richardson
! number instability due to resolved vertical shear, convection  due
! to static instability, and double diffusion.
!
! For shear instability mixing: Compute ratio of the local gradient
! Richardson number, Ri=Brunt_Vaisala_Frequensy/Velocity_Shear**2,
! to the critical Richardson number, Ri0 (usually Ri0=0.7, see
! parameter above). Restrict this ratio to be between 0 and 1;
! After that set the interior diffusivity due to shear
! instability mixing.
!
c---# undef SPLINES
# ifdef SPLINES
                        ! Construct parabolic splines: after this
      FC(0)=0.          ! complete, dR,dU,dV are the sets of vertical
      dR(0)=0.          ! derivatives of the potential density and
      dU(0)=0.          ! velocity components at the vertical grid-
      dV(0)=0.          ! box interfaces ( W-points)
                        ! FC is just an auxiliary scratch variable.
      cff6=6./ds
      do k=1,N-1,+1
        cff=1./(2.*Hz(k+1)+Hz(k)*(2.-FC(k-1)))
        FC(k)=cff*Hz(k+1)
        dR(k)=cff*(cff6*(rho1(k+1)  -rho1(k))-Hz(k)*dR(k-1))
        dU(k)=cff*(cff6*(u(k+1,tind)-u(k,tind))-Hz(k)*dU(k-1))
        dV(k)=cff*(cff6*(v(k+1,tind)-v(k,tind))-Hz(k)*dV(k-1))
      enddo

      dR(N)=0.
      dU(N)=0.
      dV(N)=0.

      do k=N-1,1,-1
        dR(k)=dR(k)-FC(k)*dR(k+1)
        dU(k)=dU(k)-FC(k)*dU(k+1)
        dV(k)=dV(k)-FC(k)*dV(k+1)
      enddo             !--> discard FC
# endif


      do k=1,N-1
        Kv(k)=nuwm                       ! mixing due to
        Kt(k)=nuws                       ! internal wave breaking 
# ifdef LMD_RIMIX
#  ifdef SPLINES
        RiRi0=min( 1., max( 0., -g*dR(k)/( rho0*Ri0*( dU(k)*dU(k)
     &                                         +dV(k)*dV(k)+eps ))
     &                                                         ))
#  else

        RiRi0=min( 1., max( 0., -g*(rho1(k+1)-rho1(k))
     &                                     *(z_r(k+1)-z_r(k))
     &                             /( rho0*Ri0*(
     &                                 (u(k+1,tind)-u(k,tind))**2
     &                                +(v(k+1,tind)-v(k,tind))**2
     &                                                    +eps ))
     &                                                        ))
#  endif
        nu_sx=(1.-RiRi0**2)**3
        Kv(k)=Kv(k)+nu0m*nu_sx           ! mixing due to vertical
        Kt(k)=Kt(k)+nu0s*nu_sx           ! shear instability
# endif
# if defined LMD_CONVEC && !defined LMD_KPP
#  ifdef SPLINES
        if (dR(k).gt.0.) then
#  else
        if (rho1(k+1).gt.rho1(k)) then
#  endif                                 ! interior convective 
          Kv(k)=Kv(k)+nu0c               ! mixing due to vertical
          Kt(k)=Kt(k)+nu0c               ! static instability
        endif
# endif
# ifdef SALINITY
        Ks(k)=Kt(k)
#  ifdef LMD_DDMIX
!
! Compute double-diffusive mixing: It can occur when vertical
! gradient of density is stable but the vertical gradient of
! salinity (salt figering) or temperature (diffusive convection)
! is unstable.
!
! Compute the ratio of thermal expansion and saline contraction
! coefficients at horizontal and vertical W-points.
!
! Compute double-diffusive density ratio, Rrho.
!
        Tt=0.5*(t(k,tind,itemp)+t(k+1,tind,itemp))
        Ts=0.5*(t(k,tind,isalt)+t(k+1,tind,isalt))-Smean
        Tp=-z_w(k)
        alfaobeta=A0+Tt*(A1+Tt*(A2+Tt*(A3+Tt*A4)))
     &              +Ts*(B0+Tt*B1+Ts*C0)
     &              +Tp*( D0+Tt*(D1+Tt*D2)+Ts*E0
     &                   +Tp*(Ts*F0+Tt*Tt*G0+Tp*H0))

        ddDT=t(k+1,tind,itemp)-t(k,tind,itemp)
        ddDS=t(k+1,tind,isalt)-t(k,tind,isalt)
        ddDS=sign(1.,ddDS)*max(abs(ddDS),eps)
        Rrho=alfaobeta*ddDT/ddDS
        if (Rrho.gt.1. .and. ddDS.gt.0.) then
!
! Salt fingering case.
!---------------------
! Compute interior diffusivity for double diffusive mixing of
! salinity. Upper bound "Rrho" by "Rrho0"; (Rrho0=1.9, nuf=0.001).
! After that set interior diffusivity for double diffusive mixing
! of temperature (fdd=0.7).
!
          Rrho=min(Rrho,Rrho0)
          nu_dds=nuf*( 1.-((Rrho-1.)/(Rrho0-1.))**2 )**3
          nu_ddt=fdd*nu_dds
        elseif (Rrho.lt.1. .and. Rrho.gt.0. .and. ddDS.lt.0.) then
!
! Diffusive convection case.
!---------------------------
! Compute interior diffusivity for double diffusive mixing of
! temperature (Marmorino and Caldwell, 1976); (nu=1.5e-6, tdd1=0.909,
! tdd2=4.6, tdd3=0.54). Compute interior diffusivity for double
! diffusive mixing of salinity (sdd1=0.15, sdd2=1.85, sdd3=0.85).
!
          nu_ddt=nu*tdd1*exp( tdd2*exp( -tdd3*( (1./Rrho)-1. )))
          if (Rrho.lt.0.5) then
            nu_dds=nu_ddt*sdd1*Rrho
          else
            nu_dds=nu_ddt*(sdd2*Rrho-sdd3)
          endif
        else
          nu_ddt=0.
          nu_dds=0.
        endif
                              ! Add double diffusion contribution
        Kt(k)=Kt(k)+nu_ddt    ! to temperature and salinity
        Ks(k)=Ks(k)+nu_dds    ! mixing coefficients.
#  endif /* LMD_DDMIX */
# endif /* SALINITY */
      enddo                   ! <-- k

      Kv(0)=Kv(  1)
      Kv(N)=Kv(N-1)     ! Padd out surface and bottom
      Kt(0)=Kt(  1)     ! values for blmix alculations.
      Kt(N)=Kt(N-1)
# ifdef SALINITY
      Ks(0)=Ks(  1)
      Ks(N)=Ks(N-1)
# endif
#else
      subroutine lmd_vmix_empty
#endif
      return    
      end

