!
! $Id: bulk_flux.F,v 1.1 2005/10/26 15:20:43 pmarches Exp $
!
#include "cppdefs.h"
#ifdef BULK_FLUX

      subroutine bulk_flux_tile (Istr,Iend,Jstr,Jend, aer,cer)
!
!===============================================================
!  This routine computes the bulk parameterization of surface
!  wind stress and surface net heat fluxes.
!
!  The COAMPS formulation is used
!===============================================================
!
      implicit none
#include "param.h"
#include "grid.h"
#include "ocean3d.h"
#include "forces.h"
#include "scalars.h"
      integer i,j, Istr,Iend,Jstr,Jend
      real evap
      real cpair,rho0i,cpi,cpaircpi,CtoK, sigmaSB
      real patm
      real ew,rw,qw,es,rs,ra,qa,emiss_lw
      real vtair,rhoair,a,b,ce,cs,htevap
      real hflat,hfsen,radlw_out,lwup,ts
      real wspd0,szero,ss
      real aer(PRIVATE_2D_SCRATCH_ARRAY),
     &     cer(PRIVATE_2D_SCRATCH_ARRAY)
!
#include "compute_auxiliary_bounds.h"
!
!
!  use mean value for atmospheric pressure in these calculations.
      patm=1013.0
!
!  define specific heat for air in Joules/(kg-degC).
!  note:  to convert from cal/g to Joules/kg, muliply by 4186.8.
      cpair=0.240*4186.8
!
!  define inverse seawater density, use mean value for seawater density.
      rho0i=1.0/rho0
!
!  set inverse of specific heat for seawater (kg-degC/Joule).
!  cp is defined in scalars.h
      cpi=1.0/cp
!
!  define ratio of specific heat for air divided by specific heat for
!  seawater.
      cpaircpi=cpair*cpi
!
      do j=Jstr-1,JendR
        do i=Istr-1,IendR
!
!  calc saturation vapor pressure (ew) at surface of water.
!  use formula of Buck.
!  note:  change due to salinity correction is about 1.7%.
!  simplify for constant atm pressure (1013 mb).
          ew=6.1121*exp(17.502*t(i,j,N,nrhs,itemp)
     &                /(240.97 + t(i,j,N,nrhs,itemp)))
     &        *1.0042*(1.0 - 5.37E-4*t(i,j,N,nrhs,isalt))
!
!  calc saturation mixing ratio at surface of water (rw).
          rw=0.62197*ew/(patm-ew)
!
!  calc saturation specific humidity at surface of water (qw).
          qw=rw/(1.0+rw)
!
!  calc saturation vapor pressure (es) of air at temp tair.
!  simplify for constant atm pressure (1013 mb) and zero salinity.
          es=6.1121
     &      *exp(17.502*tair(i,j)/(240.97 + tair(i,j)))*1.0042
!
!  calc saturation mixing ratio for air (rs).
          rs=0.62197*es/(patm-es)
!
!  calc mixing ratio for air (ra) from relative humidity.
          ra=rhum(i,j)*rs
!
!  calc specific humidity for air.
          qa=ra/(1.0+ra)
!
!  calc air density in kg/m3 (vtair = virtual temperature of air).
!  formula is from Smithsonian Meteor Tables (List, 1951, p 290, 295).
!  note:  the correction for water vapor is ~ 1%, so neglect.
          vtair=(tair(i,j)+273.16)*(1.0+1.60779*ra)/(1.0+ra)
          rhoair=0.34838*patm/vtair
!
!  calc drag coefficients for latent and sensible heat exchange.
!
!  drag coefficient calculation here is from Kondo, 1975.
!  use min windspeed of 0.1 m/s.
!  note:  using neutral values of 0.0014 for ce and 0.0011 for cs
!  results in mean latent and sensible heat flux magnitudes that
!  match the values computed by coamps fairly well.
# ifdef BULK_WVEC
          wspd0=max(0.1,
     &         sqrt(uwnd(i,j)*uwnd(i,j)+vwnd(i,j)*vwnd(i,j)))
# else
          wspd0=max(0.1,wspd(i,j))
# endif
          szero=(t(i,j,N,nrhs,itemp)-tair(i,j))/(wspd0*wspd0)
          ss=szero*abs(szero)/(abs(szero)+0.01)
          ss=max(-3.3,ss)
          if (ss .le. 0.0) then
            a=0.1+0.03*ss+0.9*exp(4.8*ss)
          else
            a=1.0+0.63*sqrt(ss)
          endif
          ce=0.0014*a
          cs=0.0011*a    !--> discard a,ss
!
!  calc latent heat of evaporation in Joules/kg.
!  the formula is  htevap = 597.8-0.573*T(degC) cal/g.
!  to convert from cal/g to Joules/kg, muliply by 4186.8.
          htevap=2.5029e6-2.40e3*t(i,j,N,nrhs,itemp)
!
!  calc evaporation rate (m/s).
!  calc latent and sensible heat fluxes (deg_C-m/s).
!  define latent and sensible heat fluxes (+) downward.
          a=rhoair*wspd0*rho0i
!
! saved for the calc of wind stress
!
          aer(i,j)=a
          cer(i,j)=ce
!
          evap=a*ce*(qw-qa)
          hflat=-evap*htevap*cpi
          hfsen=-a*cs*(t(i,j,N,nrhs,itemp)-tair(i,j))*cpaircpi

#ifdef LW_ONLINE
!
!compute radlw from incoming lonwave from LWHF.m:
! computes net longwave heat flux following Dickey et al (1994).
! qlw=LWHF(Ts,dlw) computes the net longwave heat flux into the ocean.
! Following Dickey et al (1994), J. Atmos. Oceanic Tech., 11, 1057-1078,
! the incident longwave flux can be corrected for sensor heating due to
! insolation if you are using Epply or Kipp & Zonen CG1 pyrgeometers.
! In this case, use qlw=LWHF(Ts,dlw,dsw). Epply is the default
! pyrgeometer; change code for the Kipp & Zonen instrument.

          CtoK        = 273.16    !conversion factor for [C] to [K]

          emiss_lw    = 0.985
! long-wave emissivity of ocean from Dickey et al! (1994),
! J. Atmos. Oceanic Tech., 11, 1057-1076.


          sigmaSB     = 5.6697e-8  ! Stefan-Boltzmann constant [m^2/K^4

! long-wave emissivity of ocean from Dickey et al! (1994),
! J. Atmos. Oceanic Tech., 11, 1057-1076

! compute upward gray-body longwave flux
          lwup=-1*emiss_lw*sigmaSB*((t(i,j,N,nrhs,itemp)+CtoK)**4)
! compute net longwave flux
          radlw_out= lwup + emiss_lw*radlw(i,j)*(rho0*cp)
! scale factor, convert to degC/s
         radlw_out=radlw_out/(rho0*cp)
!  Change the signe of longwave because -1 x -1

!  Compute total surface heat flux.
          stflx(i,j,itemp)=radsw(i,j)+radlw_out+hflat+hfsen
#else 
          stflx(i,j,itemp)=radsw(i,j)-radlw(i,j)+hflat+hfsen
#endif

# ifdef BULK_EP
          stflx(i,j,isalt)=(evap-prate(i,j))*t(i,j,N,nrhs,isalt)
# endif
!
!  Store fluxes for diagnostics
!
          shflx_rsw(i,j)=radsw(i,j)
          shflx_lat(i,j)=hflat
          shflx_sen(i,j)=hfsen
#ifdef LW_ONLINE
          shflx_rlw(i,j)=radlw_out
#else
          shflx_rlw(i,j)=-radlw(i,j)
#endif 
        enddo
      enddo

# ifdef BULK_WVEC
!
!  Compute kinematic, surface wind stress (m2/s2).
        do j=JstrR,JendR
          do i=Istr,IendR
            a=0.5*(aer(i-1,j)+aer(i,j))
            ce=0.5*(cer(i-1,j)+cer(i,j))
            sustr(i,j)=a*ce*uwnd(i,j)
        enddo
      enddo
        do j=Jstr,JendR
          do i=IstrR,IendR
            a=0.5*(aer(i,j-1)+aer(i,j))
            ce=0.5*(cer(i,j-1)+cer(i,j))
            svstr(i,j)=a*ce*vwnd(i,j)
        enddo
      enddo

# endif


#else
      subroutine bulk_flux_empty
#endif
      return
      end
