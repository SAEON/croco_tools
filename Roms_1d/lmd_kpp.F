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
#ifdef LMD_KPP

      subroutine lmd_kpp
      implicit none
# include "param.h"
      integer k, kbl
      real
     &    surf_dpth, dpth,
     &         Bo,
     &      Bosol,
     &      Bfsfc(0:N),Bfsfc_bl,
     &     stable,
     &         f1,
     &        Gm1,
     &     dGm1dS,
     &        Gt1,
     &     dGt1dS,
     &        Gs1,
     &     dGs1dS,
     &     Cr(0:N) 

      real alpha,beta, zscale,swdk,wm,ws, ustar
      real hekman,  hmonob, 
     &     cff,cff_up,cff_dn, K_bl,dK_bl, sgm, a1,a2,a3

# include "grid.h"
# include "ocean3d.h" 
# include "forces.h"
# include "mixing.h"
# include "scalars.h"
# define tind nstp

      real nubl, cs, Cv, Ricr, betaT, epsilon, cekman, cmonob,
     &                               Cstar, Cg, Vtc, nu0c, eps

      parameter (
     &   nubl=0.1,     ! Maximum allowed boundary layer
                       ! viscosity and diffusivity [m^2/s].
                       
     &   cs=98.96,     ! see parameter associated with turbulent
                       !     velocity scales in lmd_wscale.F

     &   Cv=1.8,       ! Ratio of interior Brunt-Vaisala
                       !     frequency "N" to that at the
                       !           entrainment depth "he".

     &   Ricr=0.15,    ! Critical bulk Richardson number.

     &   betaT=-0.2,   ! Ratio of entrainment flux to 
                       !       to surface buoyancy flux.

     &   epsilon=0.1,  ! Nondimensional extent of the
                       !                surface layer.

     &   cekman=0.7,   ! Constant used in the computation
                       !           of Ekman depth.

     &   cmonob=1.,    ! Constant used in the computaion
                       ! Monin-Obukhov depth.
                       
     &   Cstar=10.,    ! Proportionality coefficient 
                       ! parameterizing nonlocal transport.

                       ! Maximum interior convective 
     &   nu0c=0.1,     ! viscosity and  diffusivity due
                       ! to static instability, [m^2/s];
                       !

     &   eps=1.E-20 )  ! just a small number

# undef SPLINES

# ifdef SPLINES
      real FC(0:N),dR(0:N),dU(0:N),dV(0:N), cff6
# endif
      real rho_surf,u_surf,v_surf, rho_k,u_k,v_k

      Cg=Cstar*vonKar*(cs*vonKar*epsilon)**(1./3.)
      Vtc=Cv * (-betaT/(cs*epsilon))**0.5 / (Ricr*vonKar**2)

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
!
! Find thickness of boundary layer: compute verical coordinate "z_bl" 
! at which bulk Richardson number reaches its critical value:
!
!                           [Br - B(d)] * d 
! Eq. (1)      Rib(z) = ----------------------- ;    Rib(d)=Ricr
!                       |Vr - V(d)|^2 + Vt(d)^2
!
! where "Br" and "Vr" are the reference boyancy and velocity (at
! surface); while "B(d)" and "V(d)" are the boyancy and velocity at
! depth "d".
!
! In the code below, the criterion "Rib(z_bl)=Ricr" is reformulated
! as follows:
!
!                       Rib(d)       Ritop(d)
! Eq. (2)              ------- = --------------- = 1
!                        Ricr    Ricr * Ribot(d)
!
! where "Ritop" and "Ribot" are numerator and denominator in Eq. (1).
! in its turn, Eq. (2) is rewritten is the following form:
!
! Eq. (3)            Cr(d) = Ritop(d) - Ricr * Ribot(d) = 0
!
! that is, the boundary layer extends to the depth where "Cr(d)"
! changes its sign.
!
! A few preliminary steps needed to be done, before computing
! boundary layer depth:
!
! At first, compute boyoancy forcing "Bfsfc" as a function of depth.
! To do so, compute thermal expansion coefficient "alpha" [kg/m^3
! /decC] and saline contraction coefficient "beta" [kg/m^3/PSU] at
! the surface, then compute surface turbulent buoyancy forcing "Bo"
! [m^2/s^3], with the incoming solar shortwave radiation component
! "Bosol" extracted and stored separately. Finally, the boyoancy
! forcing "Bfsfc" at depth as is computed as a combination of surface
! boyoancy forcing "Bo" (without solar radiation) and the fraction
! "swdk" of surface shortwave radiation forcing "Bosol" penetrating
! to that depth.
!
! Secondly, compute friction velocity "ustar" as a function of
! surface wind stress. Also set an estimate of surface layer
! thickness "surf_dpth" based on the previously known value of
! boundary layer thickness ("surf_dpth" is needed to restrict the 
! verical lenght scale "zscale" used for computation of turbulent
! velocity scales in the case of unstable boyancy forcing [see
! LMD94, Eq. (13) and paragraph above it].
!
!
      call alfabeta (alpha,beta)
      Bo=g*( alpha*(stflx(itemp)-srflx)
# ifdef SALINITY
     &               -beta*stflx(isalt)
# endif
     &                                )
      Bosol=g*alpha*srflx      !--> discard alpha,beta;

      do k=N,1,-1
        call lmd_swfrac (+1.,z_w(k),swdk)
        Bfsfc(k)=Bo+Bosol*(1.-swdk)
      enddo
      Bfsfc(0)=0.

      ustar=sqrt(sqrt(sustr**2+svstr**2))
      surf_dpth=(z_w(N)-z_bl)*epsilon
#ifdef SPLINE
      rho_surf=rho1(N)+ds*Hz(N)*( 0.3333333333333*dR(N  )
     &                           +0.1666666666666*dR(N-1))
      u_surf=u(N,tind)+ds*Hz(N)*( 0.3333333333333*dU(N  )
     &                           +0.1666666666666*dU(N-1))
      v_surf=v(N,tind)+ds*Hz(N)*( 0.3333333333333*dV(N  )
     &                           +0.1666666666666*dV(N-1))
#else
      rho_surf=rho1(N)
      u_surf=u(N,tind)
      v_surf=v(N,tind)
#endif
      Cr(N)=0.
      do k=N,1,-1
        dpth=z_w(N)-z_w(k-1)
        if (Bfsfc(k-1).lt.0) then
          zscale=min(surf_dpth,dpth)
        else
          zscale=dpth
        endif
        call lmd_wscale (Bfsfc(k-1),zscale,ustar, wm,ws)
#ifdef SPLINE
        rho_k=rho1(k)-ds*Hz(k)*( 0.1666666666666*dR(k  )
     &                          +0.3333333333333*dR(k-1))
        u_k=u(k,tind)-ds*Hz(k)*( 0.1666666666666*dU(k  )
     &                          +0.3333333333333*dU(k-1))
        v_k=v(k,tind)-ds*Hz(k)*( 0.1666666666666*dV(k  )
     &                          +0.3333333333333*dV(k-1))
#else
        rho_k=rho1(k)
        u_k=u(k,tind)
        v_k=v(k,tind)
#endif
        Cr(k-1)=-g*(rho_surf-rho_k)*dpth
     &          -Ricr*rho0*( (u_surf-u_k)**2+(v_surf-v_k)**2
#ifdef SPLINE
     &               +Vtc*dpth*ws*sqrt(abs(g*dR(k-1)/rho0)) )
#else
     &               +Vtc*dpth*ws*sqrt(abs(bvf(k))) )
#endif
       enddo
!
! Linearly interpolate to find "z_bl" where Rib/Ricr=1.
!
      kbl=1
      do k=N,2,-1
        if (kbl.eq.1 .and. Cr(k-1).gt.0.) kbl=k
      enddo
      if (kbl.eq.1) then
        z_bl=z_w(kbl)
      else
        z_bl=( z_w(kbl)*Cr(kbl-1)-z_w(kbl-1)*Cr(kbl)
     &                          )/(Cr(kbl-1)-Cr(kbl))
      endif
!
! Find stability and buoyancy forcing "Bfsfc_bl" for boundary layer.
! In the case of stable boyancy forcing compare "z_bl" with physicaly
! limiting cases (Ekman depth and Monin-Obukhov depth) and  restrict
! it if necessary. After that recompute boundary layer index and
! boyancy forcing "Bfsfc_bl". 
!
      call lmd_swfrac (+1.,z_bl,swdk)
      Bfsfc_bl=Bo+Bosol*(1.-swdk)            !--> discard swdk

      if (Bfsfc_bl.gt.0.) then
        hekman=cekman*ustar/max(abs(f),eps)
        z_bl=z_w(N)-min( z_w(N)-z_bl, hekman)
!        hmonob=cmonob*ustar**3/(vonKar*Bfsfc_bl) 
!        z_bl=z_w(N)-min( z_w(N)-z_bl, hekman, hmonob)
      endif

      kbl=1
      do k=N,2,-1
        if (kbl.eq.1 .and. z_w(k-1).lt.z_bl) kbl=k
      enddo

      call lmd_swfrac (+1.,z_bl,swdk)
      Bfsfc_bl=Bo+Bosol*(1.-swdk)
      stable=0.5+sign(0.5,Bfsfc_bl)    !--> discard swdk
!
! Compute tubulent velocity scales (wm,ws) at "z_bl".
!
      if (Bfsfc_bl.gt.0.) then
        cff=1.
      else
        cff=epsilon
      endif
      zscale=cff*(z_w(N)-z_bl)
      call lmd_wscale (Bfsfc_bl,zscale,ustar, wm,ws)

      f1=5.*max(0.,Bfsfc_bl)*vonKar/(ustar**4+eps)
!
! Compute coefficients for nondimensional shape functions Gx(sgm)
! in terms of interior diffusivities at sgm=1 (Gm1, Gt1, Gs1) and
! their vertical derivatives at z_bl, (dGm1dS, dGt1dS, dGs1dS) .
!
      cff=1./(z_w(kbl)-z_w(kbl-1))
      cff_up=cff*(z_bl-z_w(kbl-1))
      cff_dn=cff*(z_w(kbl) -z_bl)

      K_bl=cff_up*Kv(kbl)+cff_dn*Kv(kbl-1)
      dK_bl=cff * (Kv(kbl)   -    Kv(kbl-1))
      Gm1=K_bl/((z_w(N)-z_bl)*wm+eps)
      dGm1dS=min(0., K_bl*f1-dK_bl/(wm+eps))

      K_bl=cff_up*Kt(kbl)+cff_dn*Kt(kbl-1) 
      dK_bl=cff * (Kt(kbl)   -    Kt(kbl-1))
      Gt1=K_bl/((z_w(N)-z_bl)*ws+eps)
      dGt1dS=min(0., K_bl*f1-dK_bl/(ws+eps))

# ifdef SALINITY
      K_bl=cff_up*Ks(kbl)+cff_dn*Ks(kbl-1)
      dK_bl=cff * (Ks(kbl)   -    Ks(kbl-1))
      Gs1=K_bl/((z_w(N)-z_bl)*ws+eps)
      dGs1dS=min(0., K_bl*f1-dK_bl/(ws+eps))
# endif
!
! Set mixing coefficients within boundary layer: to do so, first
! compute turbulent velocity scales, "wm","ws" at depth corresponding
! to the depth of z_w(k), then set the mixing coefficient profiles as
! products of these velocity scales and the nondimensional shape
! functions of nondimensional fractional coordinate "sgm".
!
      surf_dpth=(z_w(N)-z_bl)*epsilon
      do k=N-1,kbl,-1
        dpth=z_w(N)-z_w(k)
        if (Bfsfc(k).lt.0) then
          zscale=min(surf_dpth,dpth)
        else
          zscale=dpth
        endif
        call lmd_wscale (Bfsfc(k),zscale,ustar, wm,ws)

        sgm=dpth/(z_w(N)-z_bl+eps)
        a1=sgm-2.
        a2=3.-2.*sgm
        a3=sgm-1.
        
        Kv(k)=wm*dpth*(1.+sgm*(a1+a2*Gm1+a3*dGm1dS))
        Kt(k)=ws*dpth*(1.+sgm*(a1+a2*Gt1+a3*dGt1dS))
# ifdef SALINITY
        Ks(k)=ws*dpth*(1.+sgm*(a1+a2*Gs1+a3*dGs1dS))
# endif
# ifdef LMD_NONLOCAL
!
!  Compute boundary layer nonlocal transport [m/s^2]
!
        if (Bfsfc(k).le.0.) then
          ghats(k)=Cg * sgm*(1.-sgm)**2
        else
          ghats(k)=0.
        endif
!        ghats(k)=Bfsfc(k)*Cg*(1.-stable)/(dpth*ws+eps)
# endif
      enddo

      do k=kbl-1,0,-1
# ifdef LMD_NONLOCAL
        ghats(k)=0.
# endif
# if defined LMD_CONVEC
#  ifdef SPLINES
        if (dR(k).gt.0.) then
#  else
        if (rho1(k+1).gt.rho1(k)) then
#  endif                                 ! interior convective 
          Kv(k)=Kv(k)+nu0c               ! mixing due to vertical
          Kt(k)=Kt(k)+nu0c               ! static instability
        endif
# endif

      enddo
#else
      subroutine lmd_kpp_empty
#endif /* LMD_KPP */
      return
      end


