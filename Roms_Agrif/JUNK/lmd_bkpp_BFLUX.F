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
#ifdef LMD_BKPP
      subroutine lmd_bkpp_tile (Istr,Iend,Jstr,Jend, Kv,Kt,Ks,
     &                                          bl_dpth,Bflux,
     &                     Gm1,dGm1dS, Gt1,dGt1dS, Gs1,dGs1dS,
     &                                    wrk1,wrk2,wrk3, Rib)
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "forces.h"
# include "mixing.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i,j,k, ka,ku,ksave
      real    Kv(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &        Kt(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &        Ks(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &       bl_dpth(PRIVATE_2D_SCRATCH_ARRAY), 
     &         Bflux(PRIVATE_2D_SCRATCH_ARRAY),
     &           Gm1(PRIVATE_2D_SCRATCH_ARRAY),
     &        dGm1dS(PRIVATE_2D_SCRATCH_ARRAY),
     &           Gt1(PRIVATE_2D_SCRATCH_ARRAY),
     &        dGt1dS(PRIVATE_2D_SCRATCH_ARRAY),
     &           Gs1(PRIVATE_2D_SCRATCH_ARRAY),
     &        dGs1dS(PRIVATE_2D_SCRATCH_ARRAY),
     &          wrk1(PRIVATE_2D_SCRATCH_ARRAY),
     &          wrk2(PRIVATE_2D_SCRATCH_ARRAY),
     &          wrk3(PRIVATE_2D_SCRATCH_ARRAY),
     &         Rib(PRIVATE_2D_SCRATCH_ARRAY,2)
# undef BKPP_BFLUX
# define tind nstp
      real Vtc,    hekman,  hmonob, dVsq,   Vtsq,
     &             bl_dnew, sig,    Kv_bl,  Kt_bl,  Ks_bl,
     &     cff,    lmd_a1, dKv_bl, dKt_bl, dKs_bl,
     &     cff_up, lmd_a2, Gm,     Gt,     Gs,
     &     cff_dn, Ritop,   lmd_a3
      real zbl, zsbl
      real eps
      parameter (eps=1.E-20)
      real lmd_cs, lmd_Cv, Ric, lmd_betaT, lmd_epsilon,
     &     lmd_cekman, lmd_nu0c
      parameter (
     &   lmd_cs=98.96,     ! see parameter associated with turbulent
                           ! velocity scales in lmd_wscale.F
                           !
     &   lmd_Cv=1.8,       ! Ratio of interior Brunt-Vaisala
                           ! frequency "N" to that at the
                           ! entrainment depth "he".
                           !
     &   Ric=0.3,          ! Critical bulk Richardson number.
                           !
     &   lmd_betaT=-0.2,   ! Ratio of entrainment flux to 
                           ! to surface buoyancy flux.
                           !
     &   lmd_epsilon=0.1,  ! Nondimensional extent of the
                           ! bottom layer.
                           !
     &   lmd_cekman=0.7,   ! Constant used in the computation
                           ! of Ekman depth.
                           !
     &   lmd_nu0c=0.1      ! Maximum interior convective
                           ! viscosity and  diffusivity due
                           ! to shear instability, [m^2/s];
     &                                                           )

!
! Initialize relevant parameters
!
      Vtc=lmd_Cv*sqrt(-lmd_betaT)/( sqrt(lmd_cs*lmd_epsilon)
     &                                        *Ric*vonKar*vonKar )
!
!  Get approximation of surface layer depth using "lmd_eps" and
!  boundary layer depth from previous time step.
!
      do j=Jstr,Jend
        do i=Istr,Iend
          bl_dpth(i,j)=lmd_epsilon*hbbl(i,j)
        enddo
      enddo
!
!  Compute turbulent friction velocity [m/s] "ustar" from bottom stress
!  at RHO-points. (Bottom stress includes wave effects if activated in BBL)
!
      do j=Jstr,Jend
        do i=Istr,Iend
          ustar(i,j)=sqrt(sqrt( (0.5*(bustr(i,j)+bustr(i+1,j)))**2
     &                         +(0.5*(bvstr(i,j)+bvstr(i,j+1)))**2))
        enddo
      enddo
!
!  Compute bottom turbulent buoyancy forcing "Bflux" [m^2/s^3]
!  using thermal expansion coefficient "alpha" [kg/m^3/decC] and
!  saline contraction coefficient "beta" [kg/m^3/PSU] at the bottom.
!  Caution: alfabeta is currently only valid for surface coefficients
!
# ifdef BKPP_BFLUX
#  define alpha wrk1
#  define  beta wrk2
      call alfabeta_tile (Istr,Iend,Jstr,Jend, alpha,beta)
      do j=Jstr,Jend
        do i=Istr,Iend
          Bflux(i,j)=g*alpha(i,j)*btflx(i,j,itemp)
#  ifdef SALINITY
          Bflux(i,j)=Bflux(i,j)-g*beta(i,j)*btflx(i,j,isalt)
#  endif
        enddo
      enddo  !--> discard alpha,beta; keep Bflux
#  undef beta
#  undef alpha
# endif /* BKPP_BFLUX */
# define sigma wrk1
# define wm wrk3
# define ws wrk2
!
# if !defined BKPP_BFLUX
      do j=Jstr,Jend
        do i=Istr,Iend
          Bflux(i,j)=0.
          wm(i,j)=vonKar*ustar(i,j)
          ws(i,j)=wm(i,j)          
        enddo
      enddo
# endif /* BKPP_BFLUX */
!
!----------------------------------------------------------------
!  Compute bulk Richardson number "Rib" and then find depth of the
!  oceanic planetary boundary layer "hbbl", such that Rib(hbbl)=Ric.
!----------------------------------------------------------------
!
!  Set indices for array "Rib", the bulk Richardson number.
!
      ka=1 
      ku=2
!
!  Intialize boundary layer depth "hbbl" and index "kbl" of first grid
!  level above "hbbl" to maximum values.
!
      do j=Jstr,Jend
        do i=Istr,Iend
          hbbl(i,j)=z_r(i,j,N)-z_w(i,j,0)
          kbl(i,j)=N
          Rib(i,j,ku)=0.
        enddo
      enddo
!
!  Find bulk Richardson number at every grid level until > Ric.
!
      do k=2,N 
!
!  Compute nondimensional vertical coordinate "sigma".
!  Compute turbulent velocity scales (wm,ws) at "sigma".
!
# ifdef BKPP_BFLUX
        do j=Jstr,Jend
          do i=Istr,Iend
            sigma(i,j)=min(z_r(i,j,k)-z_w(i,j,0),bl_dpth(i,j))
          enddo
        enddo

        call lmd_wscale_tile (Istr,Iend,Jstr,Jend, Bflux,
     &                                       sigma,wm,ws)
# endif /* BKPP_BFLUX */
!
!  Compute bulk Richardson number "Rib" 
!---------------------------------------
!
!                  [Br - B(d)] * d
!     Rib(d) = ----------------------- ;       Rib(hbbl)=Ric
!              |Vr - V(d)|^2 + Vt(d)^2
!
!  To do so, first compute numerator of bluk Richardson number,
!  Ritop=(Br-B)*d, where Br is the near-surface reference buoyancy,
!  B is the mean buoyancy as function of d, and d is the distance
!  coordinate from the boundary.
!
!  Then compute the square of velocity shear relative to reference
!  velocities, dVsq=|Vr-V|^2, at horizontal and vertical RHO-points.
!
!  Then compute Vtsq
!
        cff=g/rho0
        do j=Jstr,Jend
          do i=Istr,Iend
            Ritop=-cff*(rho1(i,j,k)-rho1(i,j,1))
     &                           *(z_r(i,j,k)-z_r(i,j,1))

            dVsq=0.25*( (u(i  ,j,k,tind)-u(i  ,j,1,tind)+
     &                   u(i+1,j,k,tind)-u(i+1,j,1,tind))**2
     &                 +(v(i,j  ,k,tind)-v(i,j  ,1,tind)+
     &                   v(i,j+1,k,tind)-v(i,j+1,1,tind))**2)

            Vtsq=Vtc*(z_r(i,j,k)-z_r(i,j,1))*ws(i,j)
     &          *sqrt(max(0.,0.5*(bvf(i,j,k)+bvf(i,j,k-1))))

            Rib(i,j,ka)=Ritop/(dVsq+Vtsq+eps)
          enddo
        enddo   !--> discard ws
!
!  Linearly interpolate to find "hbbl" where Rib=Ric.
!
        do j=Jstr,Jend
          do i=Istr,Iend
            if (kbl(i,j).eq.N .and. Rib(i,j,ka).gt.Ric) then
              zbl=z_r(i,j,k)-(z_r(i,j,k)-z_r(i,j,k-1))*
     &           (Ric-Rib(i,j,ka))/(Rib(i,j,ku)-Rib(i,j,ka))
              hbbl(i,j)=zbl-z_w(i,j,0)
              kbl(i,j)=k
            endif
          enddo
        enddo
        ksave=ka
        ka=ku
        ku=ksave
      enddo    !<-- k    !--> discard Rr,Zr,Ur,Vr,Rib
!
!  Correct "hbbl" with physically limiting case (Ekman depth)
!
      do j=Jstr,Jend
        do i=Istr,Iend
          hekman=lmd_cekman*ustar(i,j)/max(abs(f(i,j)),eps)
          hbbl(i,j)=min(hbbl(i,j),hekman)
# ifdef MASKING
          hbbl(i,j)=hbbl(i,j)*rmask(i,j)
# endif
          kbl(i,j)=N
        enddo
      enddo
!
!  Find new boundary layer index "kbl".
!
      do k=N,1,-1
        do j=Jstr,Jend
          do i=Istr,Iend
            if (z_r(i,j,k)-z_w(i,j,0).gt.hbbl(i,j)) then
              kbl(i,j)=k
            endif
          enddo
        enddo
      enddo
!
!  Compute tubulent velocity scales (wm,ws) at "hbbl".
!
# ifdef BKPP_BFLUX
      do j=Jstr,Jend
        do i=Istr,Iend
          wm(i,j)=vonKar*ustar(i,j)
          ws(i,j)=wm(i,j)
        enddo
      enddo
# endif
!
!-----------------------------------------------------------------
!  Compute nondimensional shape function Gx(sigma) at "hbbl"
!  (sigma=1) in terms of interior diffusivities (Gx1) and
!  its vertical derivative (dGx1dS) via interpolation.
!-----------------------------------------------------------------
!
      do j=Jstr,Jend
        do i=Istr,Iend
          zbl=z_w(i,j,0)+hbbl(i,j)
          k=kbl(i,j)
          if (zbl.lt.z_w(i,j,k-1)) k=k-1
          cff=1./(z_w(i,j,k)-z_w(i,j,k-1))
          cff_up=cff*(zbl-z_w(i,j,k-1))
          cff_dn=cff*(z_w(i,j,k)-zbl)

          Kv_bl=cff_up*Kv(i,j,k)+cff_dn*Kv(i,j,k-1)
          dKv_bl=-cff*(Kv(i,j,k)-Kv(i,j,k-1))
          Gm1(i,j)=Kv_bl/(hbbl(i,j)*wm(i,j)+eps)
          dGm1dS(i,j)=min(0.,-dKv_bl/(wm(i,j)+eps))

          Kt_bl=cff_up*Kt(i,j,k)+cff_dn*Kt(i,j,k-1)
          dKt_bl=-cff*(Kt(i,j,k)-Kt(i,j,k-1))
          Gt1(i,j)=Kt_bl/(hbbl(i,j)*ws(i,j)+eps)
          dGt1dS(i,j)=min(0.,-dKt_bl/(ws(i,j)+eps))

# ifdef SALINITY
          Ks_bl=cff_up*Ks(i,j,k)+cff_dn*Ks(i,j,k-1)
          dKs_bl=-cff*(Ks(i,j,k)-Ks(i,j,k-1))
          Gs1(i,j)=Ks_bl/(hbbl(i,j)*ws(i,j)+eps)
          dGs1dS(i,j)=min(0.,-dKs_bl/(ws(i,j)+eps))
# endif /* SALINITY */
        enddo
      enddo
!
!-----------------------------------------------------------------
!  Compute boundary layer mixing coefficients.
!-----------------------------------------------------------------
!
      do k=1,N-1
!
!  Compute turbulent velocity scales at vertical W-points.
!
# ifdef BKPP_BFLUX
        do j=Jstr,Jend
          do i=Istr,Iend
            bl_dnew=hbbl(i,j)*lmd_epsilon
            sigma(i,j)=min(z_w(i,j,k)-z_w(i,j,0),bl_dnew)
          enddo
        enddo
        call lmd_wscale_tile (Istr,Iend,Jstr,Jend,
     &                                 Bflux,sigma,wm,ws)
# endif
!
        do j=Jstr,Jend
          do i=Istr,Iend
            if (k.lt.kbl(i,j)) then
!
!  Set polynomial coefficients for shape function.
!
              sig=min((z_w(i,j,k)-z_w(i,j,0))/(hbbl(i,j)+eps),1.)
# ifdef MASKING
              sig=sig*rmask(i,j)
# endif
              lmd_a1=sig-2.
              lmd_a2=3.-2.*sig
              lmd_a3=sig-1.
!
!  Compute nondimensional shape functions.
!
              Gm=lmd_a1+lmd_a2*Gm1(i,j)+lmd_a3*dGm1dS(i,j)
              Gt=lmd_a1+lmd_a2*Gt1(i,j)+lmd_a3*dGt1dS(i,j)
# ifdef SALINITY
              Gs=lmd_a1+lmd_a2*Gs1(i,j)+lmd_a3*dGs1dS(i,j)
# endif
!
!  Compute boundary layer mixing coefficients, combine them
!  with interior mixing coefficients.
!
              Kv(i,j,k)=hbbl(i,j)*wm(i,j)*sig*(1.+sig*Gm)
              Kt(i,j,k)=hbbl(i,j)*ws(i,j)*sig*(1.+sig*Gt)
# ifdef SALINITY
              Ks(i,j,k)=hbbl(i,j)*ws(i,j)*sig*(1.+sig*Gs)
# endif
# ifdef LMD_SKPP
!
! If reach into SBL, take the max of surface and bottom values 
!
              zsbl=z_w(i,j,N)-hbl(i,j)
              if (z_w(i,j,k).gt.zsbl) then
                Kv(i,j,k)=max(Kv(i,j,k),Akv(i,j,k))
                Kt(i,j,k)=max(Kt(i,j,k),Akt(i,j,k,itemp))
#  ifdef SALINITY
                Ks(i,j,k)=max(Ks(i,j,k),Akt(i,j,k,isalt))
#  endif
              endif
# endif /* LMD_SKPP */

            else  !<-- k > kbl(i,j)

# ifdef  LMD_CONVEC
!
!  Add convective adjustment
!
              if (bvf(i,j,k).lt.0.) then
#  ifdef LMD_SKPP
                zsbl=z_w(i,j,N)-hbl(i,j)
                if (z_w(i,j,k).lt.zsbl) then
#  endif
                  Kv(i,j,k)=Kv(i,j,k)+lmd_nu0c
                  Kt(i,j,k)=Kt(i,j,k)+lmd_nu0c
                  Ks(i,j,k)=Ks(i,j,k)+lmd_nu0c
#  ifdef LMD_SKPP
                endif
#  endif
              endif
# endif

            endif  !<-- k > kbl(i,j)
          enddo
        enddo
# undef ws
# undef wm
# undef sigma
!
!-----------------------------------------------------------------
!  Finalize: Set vertical mixing coefficients "Akv" and "Akt" 
!-----------------------------------------------------------------
!
        do j=Jstr,Jend
          do i=Istr,Iend
            Akv(i,j,k)       = Kv(i,j,k) 
            Akt(i,j,k,itemp) = Kt(i,j,k)
# ifdef SALINITY
            Akt(i,j,k,isalt) = Ks(i,j,k) 
# endif
          enddo
        enddo

      enddo ! <-- k

# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_w3d_tile (Istr,Iend,Jstr,Jend, Akv)
      call exchange_w3d_tile (Istr,Iend,Jstr,Jend,
     &                     Akt(START_2D_ARRAY,0,itemp))
      call exchange_w3d_tile (Istr,Iend,Jstr,Jend,
     &                     Akt(START_2D_ARRAY,0,isalt))
# endif
#else
      subroutine lmd_bkpp_empty
#endif /* LMD_BKPP */
      return
      end
