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
#ifdef BULK_FLUX

      subroutine bulk_flux_tile (Istr,Iend,Jstr,Jend, aer,cer)
#ifndef BULK_FAIRALL
!
!===============================================================
!  This routine computes the bulk parameterization of surface
!  wind stress and surface net heat fluxes.
!
!  The COAMPS formulation is used
!===============================================================
!
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "forces.h"
# include "scalars.h"
      integer i,j, Istr,Iend,Jstr,Jend
      real evap
      real cpair,rho0i,cpi,cpaircpi
      real patm
      real ew,rw,qw,es,rs,ra,qa
      real vtair,rhoair,a,b,ce,cs,htevap
      real hflat,hfsen,ts
      real wspd0,szero,ss
# ifdef BULK_LW
      real CtoK,emiss_lw,SigmaSB
      parameter (CtoK=273.16)       !conversion factor for [C] to [K]
      parameter (emiss_lw=0.985)    !longwave emissivity of Ocean (Dickey et al., 1994)
      parameter (SigmaSB=5.6697e-8) !Stefan-Boltzmann constant [m^2/K^4]
# endif
      real aer(PRIVATE_2D_SCRATCH_ARRAY),
     &     cer(PRIVATE_2D_SCRATCH_ARRAY)
!
# include "compute_auxiliary_bounds.h"
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
      do j=min(Jstr-1,JstrR),JendR
        do i=min(Istr-1,IstrR),IendR
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
!
          wspd0=max(0.1,wspd(i,j))
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
          evap=a*ce*(qw-qa)
          hflat=-evap*htevap*cpi
          hfsen=-a*cs*(t(i,j,N,nrhs,itemp)-tair(i,j))*cpaircpi
!
!  Compute total surface heat flux.
!
          stflx(i,j,itemp)=radsw(i,j)+hflat+hfsen
# ifndef BULK_LW
     &                    -radlw(i,j)
# else
     &                    +emiss_lw*(radlw(i,j)
     &        -rho0i*cpi*sigmaSB*(t(i,j,N,nrhs,itemp)+CtoK)**4)
# endif


# ifdef BULK_EP
          stflx(i,j,isalt)=(evap-prate(i,j))*t(i,j,N,nrhs,isalt)
# endif
!
!  Store fluxes for diagnostics
!
          shflx_rsw(i,j)=radsw(i,j)
          shflx_lat(i,j)=hflat
          shflx_sen(i,j)=hfsen
# ifndef BULK_LW
          shflx_rlw(i,j)=-radlw(i,j)
# else
          shflx_rlw(i,j)=emiss_lw*(radlw(i,j)
     &        -rho0i*cpi*sigmaSB*(t(i,j,N,nrhs,itemp)+CtoK)**4)
# endif
!
!  Save for the calc of wind stress
!
          aer(i,j)=a
          cer(i,j)=ce
        enddo
      enddo

# ifdef BULK_SM_UPDATE
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
      return
      end

#else
!================================================== Hernan G. Arango ===
!  Copyright (c) 2002-2007 The ROMS/TOMS Group                         !
!    Licensed under a MIT/X style license                              !
!    See License_ROMS.txt                                              !
!=======================================================================
!                                                                      !
!  This routine computes the bulk parameterization of surface wind     !
!  stress and surface net heat fluxes.                                 !
!                                                                      !
!  References:                                                         !
!                                                                      !
!    Fairall, C.W., E.F. Bradley, D.P. Rogers, J.B. Edson and G.S.     !
!      Young, 1996:  Bulk parameterization of air-sea fluxes for       !
!      tropical ocean-global atmosphere Coupled-Ocean Atmosphere       !
!      Response Experiment, JGR, 101, 3747-3764.                       !
!                                                                      !
!    Fairall, C.W., E.F. Bradley, J.S. Godfrey, G.A. Wick, J.B.        !
!      Edson, and G.S. Young, 1996:  Cool-skin and warm-layer          !
!      effects on sea surface temperature, JGR, 101, 1295-1308.        !
!                                                                      !
!    Liu, W.T., K.B. Katsaros, and J.A. Businger, 1979:  Bulk          !
!        parameterization of the air-sea exchange of heat and          !
!        water vapor including the molecular constraints at            !
!        the interface, J. Atmos. Sci, 36, 1722-1735.                  !
!                                                                      !
!  Adapted from COARE code written originally by David Rutgers and     !
!  Frank Bradley.                                                      !
!                                                                      !
!  EMINUSP option for equivalent salt fluxes added by Paul Goodman     !
!  (10/2004).                                                          !
!                                                                      !
!  Modified by Kate Hedstrom for COARE version 3.0 (03/2005).          !
!  Modified by Jim Edson to correct specific hunidities.               !
!                                                                      !
!  Reference:                                                          !
!                                                                      !
!     Fairall et al., 2003: J. Climate, 16, 571-591.                   !
!                                                                      !
!     Taylor, P. K., and M. A. Yelland, 2001: The dependence of sea    !
!     surface roughness on the height and steepness of the waves.      !
!     J. Phys. Oceanogr., 31, 572-590.                                 !
!                                                                      !
!     Oost, W. A., G. J. Komen, C. M. J. Jacobs, and C. van Oort, 2002:!
!     New evidence for a relation between wind stress and wave age     !
!     from measurements during ASGAMAGE. Bound.-Layer Meteor., 103,    !
!     409-438.                                                         !
!
!  Modified for ROMS_AGRIF by Fred marin (2008)
!  Modified for ROMS_AGRIF by P. Marchesiello (2009)
!                                                                      !
!=======================================================================
!

      implicit none
#include "param.h"
#include "grid.h"
#include "ocean3d.h"
#include "forces.h"
#include "scalars.h"
      integer i,j,Istr,Iend,Jstr,Jend
      integer Itermax,Iter
      real a,cff
      real rho0i,cpi
      real TseaC,TseaK,rhoSea,Qsea
      real TairC,TairK,rhoAir,Qair
      real Q,RH,VisAir,Hlv
      real delW,delT,delQ
      real u10,Zo10,Cd10,Ch10,Ct10,Cd
      real Ct,CC,Ri,Ribcu,Zetu,L10
      real Wstar,Tstar,Qstar
      real ZoW,ZoT,ZoT10,ZoQ,ZoL,L,Rr,Bf
      real Wpsi,Tpsi,Qpsi
      real wspd0,Wgus,charn

      real bulk_psiu,bulk_psit

      real hfsen,hflat,hflw,upvel,evap

      real blk_Rgas,blk_ZW,blk_ZT,blk_ZQ,blk_beta,blk_Cpa,blk_Zabl
      parameter (blk_Rgas=287.1)
      parameter (blk_ZW=10.0)
      parameter (blk_ZT=10.0)
      parameter (blk_ZQ=10.0)
      parameter (blk_Zabl=600.0)
      parameter (blk_beta=1.2)
      parameter (blk_Cpa=1004.67)

# ifdef BULK_LW
      real emiss_lw,SigmaSB
      parameter (emiss_lw=0.985)
      parameter (SigmaSB=5.6697E-8)
# endif

      real rhow,patm
      parameter (rhow=1000.0)
      parameter (patm=1010.0)

      real eps,r3
      parameter (eps=1.d-20)
      parameter (r3=1.0/3.0)

      real aer(PRIVATE_2D_SCRATCH_ARRAY),
     &     cer(PRIVATE_2D_SCRATCH_ARRAY)
!
#include "compute_auxiliary_bounds.h"
!
!
!  define inverse seawater density, use mean value for seawater density.
      rho0i=1.0/rho0
!
!  set inverse of specific heat for seawater (kg-degC/Joule).
!  cp is defined in scalars.h
      cpi=1.0/cp
!
!
      do j=min(Jstr-1,JstrR),JendR
        do i=min(Istr-1,IstrR),IendR
!
!  Input bulk parameterization fields
!
          wspd0=wspd(i,j)
          TairC=tair(i,j)
          TairK=TairC+273.16
          TseaC=t(i,j,N,nrhs,itemp)
          TseaK=TseaC+273.16
          rhoSea=rho(i,j,N)+rho0
          RH=rhum(i,j)
# ifndef BULK_LW
          hflw=radlw(i,j)
# else
!
!  Treat input longwave data as downwelling radiation only and add
!  outgoing IR from model sea surface temperature.
!
          hflw=emiss_lw*(rho0i*cpi*SigmaSB*TseaK*TseaK*TseaK*TseaK
     &         -radlw(i,j))
# endif
!
!-----------------------------------------------------------------------
!  Compute specific humidities (kg/kg).
!
!    note that Qair is the saturation specific humidity at Tair
!                 Q is the actual specific humidity
!              Qsea is the saturation specific humidity at Tsea

!          Saturation vapor pressure in mb is first computed and then
!          converted to specific humidity in kg/kg
!
!          The saturation vapor pressure is computed from Teten formula
!          using the approach of Buck (1981):
!
!          Esat(mb) = (1.0007+3.46E-6*patm)*6.1121*
!                  EXP(17.502*TairC(C)/(240.97+TairC(C)))
!
!          The ambient vapor is found from the definition of the
!          Relative humidity:
!
!          RH = W/Ws*100 ~ E/Esat*100   E = RH/100*Esat if RH is in %
!                                       E = RH*Esat     if RH fractional
!
!          The specific humidity is then found using the relationship:
!
!          Q = 0.622 E/(P + (0.622-1)e)
!
!          Q(kg/kg) = 0.62197*(E(mb)/(patm(mb)-0.378*E(mb)))
!
!-----------------------------------------------------------------------
!
!  Compute air saturation vapor pressure (mb), using Teten formula.
!
          cff=(1.0007+3.46e-6*patm)*6.1121*
     &        exp(17.502*TairC/(240.97+TairC))
!
!  Compute specific humidity at Saturation, Qair (kg/kg).
!
          Qair=0.62197*(cff/(patm-0.378*cff))
!
!  Compute specific humidity, Q (kg/kg).
!
          if (RH.lt.2.0) then                          !RH fraction
            cff=cff*RH                                 !Vapor pres (mb)
            Q=0.62197*(cff/(patm-0.378*cff))           !Spec hum (kg/kg)
          else          !RH input was actually specific humidity in g/kg
            Q=RH/1000.0                                !Spec Hum (kg/kg)
          endif
!
!  Compute water saturation vapor pressure (mb), using Teten formula.
!
          cff=(1.0007+3.46e-6*patm)*6.1121*
     &            exp(17.502*TseaC/(240.97+TseaC))
!
!  Vapor Pressure reduced for salinity (Kraus & Businger, 1994, pp 42).
!
          cff=cff*0.98
!
!  Compute Qsea (kg/kg) from vapor pressure.
!
          Qsea=0.62197*(cff/(patm-0.378*cff))
!
!-----------------------------------------------------------------------
!  Compute Monin-Obukhov similarity parameters for wind (Wstar),
!  heat (Tstar), and moisture (Qstar), Liu et al. (1979).
!-----------------------------------------------------------------------
!
!  Moist air density (kg/m3).
!
          rhoAir=patm*100.0/(blk_Rgas*TairK*
     &                              (1.0+0.61*Q))
!
!  Kinematic viscosity of dry air (m2/s), Andreas (1989).
!
          VisAir=1.326E-5*(1.0+TairC*(6.542E-3+TairC*
     &               (8.301e-6-4.84e-9*TairC)))
!
!  Compute latent heat of vaporization (J/kg) at sea surface, Hlv.
!
          Hlv=(2.501-0.00237*TseaC)*1.0e+6
!
!  Assume that wind is measured relative to sea surface and include
!  gustiness.
!
          Wgus=0.5
          delW=SQRT(wspd0*wspd0+Wgus*Wgus)
          delQ=Qsea-Q
          delT=TseaC-TairC
!
!  Neutral coefficients.
!
          ZoW=0.0001
          u10=delW*LOG(10.0/ZoW)/LOG(blk_ZW/ZoW)
          Wstar=0.035*u10
          Zo10=0.011*Wstar*Wstar/g+0.11*VisAir/Wstar
          Cd10=(vonKar/LOG(10.0/Zo10))**2
          Ch10=0.00115
          Ct10=Ch10/sqrt(Cd10)
          ZoT10=10.0/exp(vonKar/Ct10)
          Cd=(vonKar/LOG(blk_ZW/Zo10))**2
!
!  Compute Richardson number.
!
          Ct=vonKar/LOG(blk_ZT/ZoT10)  ! T transfer coefficient
          CC=vonKar*Ct/Cd
          Ribcu=-blk_ZW/(blk_Zabl*0.004*blk_beta**3)
          Ri=-g*blk_ZW*(delT+0.61*TairK*delQ)/
     &          (TairK*delW*delW)
          if (Ri.lt.0.0) then
            Zetu=CC*Ri/(1.0+Ri/Ribcu)       ! Unstable
          else
            Zetu=CC*Ri/(1.0+3.0*Ri/CC)      ! Stable
          endif
          L10=blk_ZW/Zetu
          if (Zetu.gt.50.0) then
            IterMax=1
          else
            IterMax=3
          endif
!
!  First guesses for Monon-Obukhov similarity scales.
!
          Wstar=delW*vonKar/(LOG(blk_ZW/Zo10)-
     &                             bulk_psiu(blk_ZW/L10,pi))
          Tstar=-delT*vonKar/(LOG(blk_ZT/ZoT10)-
     &              bulk_psit(blk_ZT/L10,pi))
          Qstar=-delQ*vonKar/(LOG(blk_ZQ/ZoT10)-
     &              bulk_psit(blk_ZQ/L10,pi))
!
!  Modify Charnock for high wind speeds. The 0.125 factor below is for
!  1.0/(18.0-10.0).
!
          if (delW.gt.18.0) then
            charn=0.018
          elseif ((10.0.lt.delW).and.(delW.le.18.0)) then
            charn=0.011+0.125*(0.018-0.011)*(delW-10.)
          else
            charn=0.011
          endif
!
!  Iterate until convergence. It usually converges within four
!  iterations.
!
        do Iter=1,IterMax
          ZoW=charn*Wstar*Wstar/g+0.11*VisAir/(Wstar+eps)
          Rr=ZoW*Wstar/VisAir
!
!  Compute Monin-Obukhov stability parameter, Z/L.
!
          ZoQ=MIN(1.15e-4,5.5e-5/Rr**0.6)
          ZoT=ZoQ
          ZoL=vonKar*g*blk_ZW*
     &             (Tstar*(1.0+0.61*Q)+0.61*TairK*Qstar)/
     &             (TairK*Wstar*Wstar*(1.0+0.61*Q)+eps)
          L=blk_ZW/(ZoL+eps)
!
!  Evaluate stability functions at Z/L.
!
          Wpsi=bulk_psiu(ZoL,pi)
          Tpsi=bulk_psit(blk_ZT/L,pi)
          Qpsi=bulk_psit(blk_ZQ/L,pi)
!
!  Compute wind scaling parameters, Wstar.
!
          Wstar=MAX(eps,delW*vonKar/(LOG(blk_ZW/ZoW)-Wpsi))
          Tstar=-delT*vonKar/(LOG(blk_ZT/ZoT)-Tpsi)
          Qstar=-delQ*vonKar/(LOG(blk_ZQ/ZoQ)-Qpsi)
!
!  Compute gustiness in wind speed.
!
          Bf=-g/TairK*Wstar*(Tstar+0.61*TairK*Qstar)
          if (Bf.gt.0.0) then
            Wgus=blk_beta*(Bf*blk_Zabl)**r3
          else
            Wgus=0.2
          endif
          delW=SQRT(wspd0*wspd0+Wgus*Wgus)
        enddo
!
!-----------------------------------------------------------------------
!  Compute Atmosphere/Ocean fluxes.
!-----------------------------------------------------------------------
!
!
!  Compute transfer coefficients for momentum (Cd).
!
          wspd0=SQRT(wspd0*wspd0+Wgus*Wgus)
          Cd=Wstar*Wstar/(wspd0*wspd0+eps)
!
!  Compute turbulent sensible heat flux (W/m2), Hs.
!
          hfsen=-blk_Cpa*rhoAir*Wstar*Tstar
!
!  Compute turbulent latent heat flux (W/m2), Hl.
!
          hflat=-Hlv*rhoAir*Wstar*Qstar
!
!  Compute Webb correction (Webb effect) to latent heat flux, Hlw.
!
          upvel=-1.61*Wstar*Qstar-(1.0+1.61*Q)*Wstar*Tstar/TairK
          hflat=hflat+rhoAir*Hlv*upvel*Q
!
!=======================================================================
!  Compute surface net heat flux and surface wind stress.
!=======================================================================
!
!  Compute kinematic, surface, net heat flux (degC m/s).  Notice that
!  the signs of latent and sensible fluxes are reversed because fluxes
!  calculated from the bulk formulations above are positive out of the
!  ocean.
!
!  For EMINUSP option,  EVAP = LHeat (W/m2) / Hlv (J/kg) = kg/m2/s
!                       PREC = rain = kg/m2/s
!
!  To convert these rates to m/s divide by freshwater density, rhow.
!
!  Note that when the air is undersaturated in water vapor (Q < Qsea)
!  the model will evaporate and LHeat > 0:
!
!                   LHeat positive out of the ocean
!                    evap positive out of the ocean
!
!  Note that if evaporating, the salt flux is positive
!        and if     raining, the salt flux is negative
!
!  Note that fresh water flux is positive out of the ocean and the
!  salt flux (stflx(isalt)) is positive into the ocean. It is converted
!  to (psu m/s) for stflx(isalt) in "set_vbc.F".
!
          hflat=-hflat*rho0i*cpi
          hfsen=-hfsen*rho0i*cpi
! Compute total surface heat flux
          stflx(i,j,itemp)=radsw(i,j)-hflw+hflat+hfsen
# ifdef BULK_EP
          evap=-cp*hflat/Hlv
          stflx(i,j,isalt)=(evap-prate(i,j))*t(i,j,N,nrhs,isalt)
# endif
!
!  saved for the calculation of wind stress
!
          aer(i,j)=rhoAir*wspd0*rho0i
          cer(i,j)=Cd
!
!  Store fluxes for diagnostics
          shflx_rsw(i,j)=radsw(i,j)
          shflx_lat(i,j)=hflat
          shflx_sen(i,j)=hfsen
          shflx_rlw(i,j)=-hflw
        enddo
      enddo

# ifdef BULK_SM_UPDATE
!
!  Compute kinematic, surface wind stress (m2/s2).
        do j=JstrR,JendR
          do i=Istr,IendR
            a=0.5*(aer(i-1,j)+aer(i,j))
            cff=0.5*(cer(i-1,j)+cer(i,j))
            sustr(i,j)=a*cff*uwnd(i,j)
        enddo
      enddo
        do j=Jstr,JendR
          do i=IstrR,IendR
            a=0.5*(aer(i,j-1)+aer(i,j))
            cff=0.5*(cer(i,j-1)+cer(i,j))
            svstr(i,j)=a*cff*vwnd(i,j)
        enddo
      enddo

# endif

      return
      end

      FUNCTION bulk_psiu (ZoL, pi)
!
!=======================================================================
!                                                                      !
!  This function evaluates the stability function for  wind speed      !
!  by matching Kansas  and free convection forms.  The convective      !
!  form follows Fairall et al. (1996) with profile constants from      !
!  Grachev et al. (2000) BLM.  The  stable  form is from Beljaars      !
!  and Holtslag (1991).                                                !
!                                                                      !
!=======================================================================
!
!
!  Function result
!
      real bulk_psiu
!
!  Imported variable declarations.
!
      real ZoL, pi
!
!  Local variable declarations.
!
      real r3
      parameter (r3=1.0/3.0)
      real Fw, cff, psic, psik, x, y
!
!-----------------------------------------------------------------------
!  Compute stability function, PSI.
!-----------------------------------------------------------------------
!
!  Unstable conditions.
!
      if (ZoL.lt.0.0) then
        x=(1.0-15.0*ZoL)**0.25
        psik=2.0*LOG(0.5*(1.0+x))+LOG(0.5*(1.0+x*x))-
     &       2.0*ATAN(x)+0.5*pi
!
!  For very unstable conditions, use free-convection (Fairall).
!
        cff=SQRT(3.0)
        y=(1.0-10.15*ZoL)**r3
        psic=1.5*LOG(r3*(1.0+y+y*y))-
     &       cff*ATAN((1.0+2.0*y)/cff)+pi/cff
!
!  Match Kansas and free-convection forms with weighting Fw.
!
        cff=ZoL*ZoL
        Fw=cff/(1.0+cff)
        bulk_psiu=(1.0-Fw)*psik+Fw*psic
!
!  Stable conditions.
!
      else
        cff=MIN(50.0,0.35*ZoL)
        bulk_psiu=-((1.0+ZoL)+0.6667*(ZoL-14.28)/
     &            EXP(cff)+8.525)
      endif
      return
      END FUNCTION bulk_psiu

      FUNCTION bulk_psit (ZoL, pi)
!
!=======================================================================
!                                                                      !
!  This function evaluates the  stability function  for moisture and   !
!  heat by matching Kansas and free convection forms. The convective   !
!  form follows Fairall et al. (1996) with  profile  constants  from   !
!  Grachev et al. (2000) BLM.  The stable form is from  Beljaars and   !
!  and Holtslag (1991).                                                !
!
!=======================================================================
!
!  Function result
!
      real bulk_psit
!
!  Imported variable declarations.
!
      real ZoL, pi
!
!  Local variable declarations.
!
      real r3
      parameter (r3=1.0/3.0)
      real Fw, cff, psic, psik, x, y
!
!-----------------------------------------------------------------------
!  Compute stability function, PSI.
!-----------------------------------------------------------------------
!
!  Unstable conditions.
!
      if (ZoL.lt.0.0) then
        x=(1.0-15.0*ZoL)**0.5
        psik=2.0*LOG(0.5*(1.0+x))
!
!  For very unstable conditions, use free-convection (Fairall).
!
        cff=SQRT(3.0)
        y=(1.0-34.15*ZoL)**r3
        psic=1.5*LOG(r3*(1.0+y+y*y))-
     &       cff*ATAN((1.0+2.0*y)/cff)+pi/cff
!
!  Match Kansas and free-convection forms with weighting Fw.
!
        cff=ZoL*ZoL
        Fw=cff/(1.0+cff)
        bulk_psit=(1.0-Fw)*psik+Fw*psic
!
!  Stable conditions.
!
      else
        cff=MIN(50.0,0.35*ZoL)
        bulk_psit=-((1.0+2.0*ZoL)**1.5+
     &            0.6667*(ZoL-14.28)/EXP(cff)+8.525)
      endif
      return
      END FUNCTION bulk_psit
#endif

#else
      subroutine bulk_flux_empty
      return
      end
#endif
