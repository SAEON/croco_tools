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
#ifdef BIOLOGY

      subroutine biology
!
! Compute biological forcing functions as defined by the
! Fasham et al. [JMR, 48, 591-639, 1990]
!
! This routine has been written by John Moisan and adapted 
! to be used in scrum by MANU Sept. 8 98.
! It is used to compute the sources and sinks terms for the
! equation for biological quantity. IN this particular 
! implementation we have: NO3, NH4, Detritus, PHYTOplankton,
! chlorophyll and ZOOplanknton.
!
      implicit none
#include "param.h"
#include "grid.h"
#include "ocean3d.h"
#include "scalars.h"
#include "mixing.h"                     
#include "forces.h"
      real kwater, kChla, palpha,
     &     K_NO3, K_NH4, mu_46, mu_47, gmax, K_Phyt,
     &     beta, mu_62, mu_52, mu_56, mu_21, mu_72, 
     &     CN_Phyt, theta_m, chla_C, wSDet, wLDet, wPhyt, wChla
      integer ITERMAX

#ifdef BIO_OCEAN
      parameter (
     &   ITERMAX = 3,    ! number of small implicit time steps
!
! Parameters as in Table 1; Fasham et al. [JMR, 48, 591-639, 1990]
! An "*" indicates that the parameters and/or their ranges are poorly understood
!
     &   kwater = 0.04,  ! light attenuation due to sea water 
                         ! range:(0.04<==>0.04];    units:[m-1]
     &   kChla  = 0.025, ! light attenuation by Chlor
                         ! range:(0.025<==>0.025);  units:[(m^2 mg Chla)-1]
     &   palpha = 5.00,  !*initial slope of the P-I curve
                         !*range:(1.00<==>1.00);    units:[mg C (mg Chla W m-2 d)-1]
     &   CN_Phyt= 6.625, ! C:N ratio for phyto 
                         ! range:(6.625<==>6.625);  units:[mMol C (mMol N)-1]
     &   theta_m= 0.053, ! maximum Cellular Chlor to C Ratio 
                         ! range:(0.015<==>0.072);  units:[mg Chla:mg C]
     &   chla_C = 1.3538,! mass balance for chla molecule (893.5/660.) 
                         ! range:(NA);              units:[mg Chla (mg C)-1]
     &   K_NO3  = 1./0.5,!*inverse half-saturation for Phyto NO3 uptake 
                         ! range:( <==>  );         units:[1/(mMol N m-3)]
     &   K_NH4  = 1./0.1,!*inverse half-saturation for Phyto NH4 uptake 
                         ! range:( <==>  );         units:[1/(mMol N m-3)]
     &   mu_46  = 0.07,  !*Phyto mortality to SDet rate 
                         ! range:( <==>  );         units:[d-1]
     &   gmax   = 0.75,  !*Zoo maximum growth rate 
                         ! range:( <==>  );         units:[d-1]
     &   beta   = 0.75,  !*Zoo assimilation efficiency 
                         ! range:(0.<==>1.);        units:[n.d.]
     &   K_Phyt = 1.,    !*Zoo half-saturation constant for ingestion 
                         ! range:( <==>  );         units:[mMol N m-3]
     &   mu_52  = 0.10,  !*Zoo specific excretion rate 
                         ! range:( <==>  );         units:[d-1]
     &   mu_56  = 0.10,  !*Zoo quadratic mortality to Detritus 
                         ! range:(0.<==>.2);        units:[d-1 (mMol N m-3)-1]
     &   mu_62  = 0.1,   !*Small Detrital breakdown to NH4 rate 
                         ! range:( <==>   );        units:[d-1]
     &   mu_47  = 0.005, !*Specific (per unit Phyto+SDet) aggregation rate
                         !       SDet+Phyt ==> LDet 
                         ! range:( <==>   );        units:[(mMol N m-3)-1 d-1]
     &   mu_72  = 0.1,   !*Specific rate of LDET recycling to NH4 
                         ! range:( <==>   );        units:[d-1]
     &   wSDet  = 0.1,   !*Sinking velocity for SDET 
                         ! range:( <==>   );        units:[m d-1]
     &   wLDet  = 10.0,  !*Sinking velocity for LDET
                         ! range:( <==>   );        units:[m d-1]
     &   wPhyt  = 0.1,   !*Sinking velocity for Phyt
                         ! range:( <==>   );        units:[m d-1]
     &   wChla  = wPhyt, !^Sinking velocity for Chla
                         ! range:( <==>   );        units:[m d-1]
     &   mu_21  = 0.1)   !*Oxidation of NH4 to NO3 (Nitrification) 
                         ! range:( <==>   );        units:[d-1]
#else
c COASTAL
      parameter (
     &   ITERMAX = 3,    ! number of small implicit time steps
!
! Parameters as in Table 1; Fasham et al. [JMR, 48, 591-639, 1990]
! An "*" indicates that the parameters and/or their ranges are poorly understood
!
     &   kwater = 0.04,  ! light attenuation due to sea water 
                         ! range:(0.04<==>0.04];    units:[m-1]
     &   kChla  = 0.025, ! light attenuation by Chlor
                         ! range:(0.025<==>0.025);  units:[(m^2 mg Chla)-1]
     &   palpha = 1.00,  !*initial slope of the P-I curve
                         ! range:(1.00<==>1.00);    units:[mg C (mg Chla W m-2 d)-1]
     &   CN_Phyt= 6.625, ! C:N ratio for phyto 
                         ! range:(6.625<==>6.625);  units:[mMol C (mMol N)-1]
     &   theta_m= 0.053, ! maximum Cellular Chlor to C Ratio 
                         ! range:(0.015<==>0.072);  units:[mg Chla:mg C]
     &   chla_C = 1.3538,! mass balance for chla molecule (893.5/660.) 
                         ! range:(NA);              units:[mg Chla (mg C)-1]
     &   K_NO3  = 1./0.5,!*inverse half-saturation for Phyto NO3 uptake 
                         ! range:( <==>  );         units:[1/(mMol N m-3)]
     &   K_NH4  = 1./0.5,!*inverse half-saturation for Phyto NH4 uptake 
                         ! range:( <==>  );         units:[1/(mMol N m-3)]
     &   mu_46  = 0.07,  !*Phyto mortality to SDet rate 
                         ! range:( <==>  );         units:[d-1]
     &   gmax   = 0.75,  !*Zoo maximum growth rate 
                         ! range:( <==>  );         units:[d-1]
     &   beta   = 0.75,  !*Zoo assimilation efficiency 
                         ! range:(0.<==>1.);        units:[n.d.]
     &   K_Phyt = 1.,    !*Zoo half-saturation constant for ingestion 
                         ! range:( <==>  );         units:[mMol N m-3]
     &   mu_52  = 0.10,  !*Zoo specific excretion rate 
                         ! range:( <==>  );         units:[d-1]
     &   mu_56  = 0.10,  !*Zoo quadratic mortality to Detritus 
                         ! range:(0.<==>.2);        units:[d-1 (mMol N m-3)-1]
     &   mu_62  = 0.1,   !*Small Detrital breakdown to NH4 rate 
                         ! range:( <==>   );        units:[d-1]
     &   mu_47  = 0.005, !*Specific (per unit Phyto+SDet) aggregation rate
                         !       SDet+Phyt ==> LDet 
                         ! range:( <==>   );        units:[(mMol N m-3)-1 d-1]
     &   mu_72  = 0.05,  !*Specific rate of LDET recycling to NH4 
                         ! range:( <==>   );        units:[d-1]
     &   wSDet  = 1.0,   !*Sinking velocity for SDET 
                         ! range:( <==>   );        units:[m d-1]
     &   wLDet  = 10.0,  !*Sinking velocity for LDET
                         ! range:( <==>   );        units:[m d-1]
     &   wPhyt  = 0.,    !*Sinking velocity for Phyt
                         ! range:( <==>   );        units:[m d-1]
     &   wChla  = wPhyt, !^Sinking velocity for Chla
                         ! range:( <==>   );        units:[m d-1]
     &   mu_21  = 0.1)   !*Oxidation of NH4 to NO3 (Nitrification) 
                         ! range:( <==>   );        units:[d-1]
#endif

      integer k, ITER
      real dtbio, temp(N), salt(N), NO3(N), E_NO3,
     &     cff, PAR,     NH4(N), 
     &     attn,    Phyt(N),
     &     Vp,      Zoo(N),
     &     Epp,     SDet(N),
     &     Q,       LDet(N),
     &     cu, aL,aR, aJ(N),   theta(N),  chla(N),
     &     PAR_var_cff(N),    att_var(N), 
     &     cff1,cff2,cff3,cff6,FC(0:N),dSDet(0:N),dLDet(0:N),dPhyt(0:N),
     &     dChla(0:N)
#ifdef OXYGEN
      real O2(N),bioinit(N),biofinal(N)
      real dissO2,o2satu,satpc,aou
#endif
!
!
!
! Since the following solver is iterative to achieve implicit
! discretization of the biological interaction
!
!           BIO = BIO + dtbio * rhs(BIO)
! 
! where rhs(BIO) is the vector of biological r.h.s. computed at
! the new time step. During the iterative procedure a series of
! fractional time steps is performed in a chained mode (splitting
! by different biological conversion processes) in sequence NO3 -- 
! NH4 -- Phyt -- Zoo -- SDet, that is the main food chain. In all 
! stages the concentration of the component being consumed is
! treated in fully implicit manner, so that the algorithm guarantees
! non-negative values, no matter how strong is the concentration of
! active consuming component (Phyto or Zoo).
!
! The overall algorithm, as well as any stage of it is formulated
! in conservative form (except explicit sinking) in sense that the
! sum of concentration of all five components is conserved.
!
!
! Extract biological variables from tracer arrays; place them into
! scratch variables; restrict their values to be positive definite.
!
      do k=1,N
        temp(k)  = max(t(k,nnew,itemp),0.)         ! temperature
        salt(k)  = max(t(k,nnew,isalt),0.)         ! salinity
        NO3(k)   = max(t(k,nnew,iNO3_),0.)         ! Nitrate
        NH4(k)   = max(t(k,nnew,iNH4_),0.)         ! Ammonium
        Phyt(k)  = max(t(k,nnew,iPhyt),0.)         ! Phytoplankton
        Zoo(k)   = max(t(k,nnew,iZoo_),0.)         ! Zooplankton
        SDet(k)  = max(t(k,nnew,iSDet),0.)         ! Small Detritus
        LDet(k)  = max(t(k,nnew,iLDet),0.)         ! Large Detritus
        chla(k)  = max(t(k,nnew,iChla),0.)         ! Chlor a [mg Chl a m-3]
#ifdef OXYGEN
        O2(k)    = max(t(k,nnew,iOxy_),0.)         ! Oxygen [O2 mMol m-3]
#endif
        if (phyt(k) .gt. 0.001 .and. chla(k) .gt. 0.001) then
           theta(k) = chla(k)/(phyt(k)*CN_Phyt*12.)     ! Chla/Phyto ratio
        else
           theta(k) = theta_m
        endif
#ifdef OXYGEN
        bioinit(k)=Phyt(k)+Zoo(k)+SDet(k)+LDet(k)
#endif
      enddo
!
! Calulate aJ: Set Photosynthetically Available Radiation (PAR) at
! surface from solar radiation x 0.43. Then, within wach grid box 
! compute attenuation coefficient based on the concentration of
! phytoplancton inside the grid box, and attenuate PAR from surface
! down (thus, PAR at certain depth depends on the whole distribution
! of Phytoplankton above. To compute aJ, one needs PAR somewhat in
! the middle of the gridbox, so that attenuation "attn" corresponds
! to half of the grid box height, while PAR is multiplied by it
! twice: once to get it in the middle of grid-box and once the
! compute on the lower grid-box interface;
!
! Set biological time step [i.e. time step per
! iteration measured in days]:
! 
      dtbio=dt/(24.*3600.*float(ITERMAX))


      DO ITER=1,ITERMAX      !--> Start internal iterations to achieve
                             !    nonlinear backward-implicit solution.

        PAR=srflx*rho0*Cp*0.43
        if (PAR.gt.0.) then
!    
!         sun is up
!
          do k=N,1,-1     !<-- irreversible
            attn=exp(-0.5*(kwater+kChla*chla(k))
     &                                 *(z_w(k)-z_w(k-1)))
            att_var(k) = attn
            PAR=PAR*attn
            Vp=0.59*(1.066**t(k,nnew,itemp))   ! From Eppley: [0.59=ln(2.)*.851]
            cff=PAR*palpha*theta(k)            ! Vp=2.9124317 at 
            Epp=Vp/sqrt(Vp*Vp+cff*cff)         ! t=19.25 degrees
            aJ(k)=Epp*cff

            cff=K_NO3*NO3(k)+K_NH4*NH4(k)
            Q=cff/(1.+cff)

            cff=dtbio*aJ(k)*Q
            theta(k)=(theta(k)+theta_m*Chla_c*Epp*Q*cff)/(1.+cff)
            PAR=PAR*attn
            PAR_var_cff(k) = PAR
          enddo
!
! NO3 and NH4 uptake by Phyto
!
          do k=1,N
            E_NO3=K_NO3*(1./(1.+(NH4(k)*K_NH4))) ! Parker 1993 Ecol Mod. 66 113-120
            cff=dtbio*aJ(k)*Phyt(k)/(1.+E_NO3*NO3(k)+K_NH4*NH4(k))
            NO3(k)=NO3(k)/(1.+cff*E_NO3)
            NH4(k)=NH4(k)/(1.+cff*K_NH4)
            Phyt(k)=Phyt(k)+cff*( E_NO3*NO3(k)+K_NH4*NH4(k))
          enddo
        endif
!
! Nitrification of NH4 ==> NO3
!
        do k=1,N
          cff=dtbio*mu_21*NH4(k)
          cff3=cff
          NH4(k)=NH4(k)-cff
          NO3(k)=NO3(k)+cff
        enddo
!
! (1) Phytoplankton grazing by Zooplankton to Zoo and SDet
! (2) Phytoplankton mortality to SDet (mu_46)
!
        do k=1,N
          cff1=dtbio*gmax*Zoo(k)/(K_Phyt+Phyt(k))
          cff2=dtbio*mu_46
          Phyt(k)=Phyt(k)/(1.+cff1+cff2)
          Zoo(k)=Zoo(k)+Phyt(k)*cff1*beta
          SDet(k)=SDet(k)+Phyt(k)*(cff1*(1.-beta)+cff2)
        enddo
!
! (1) Zoo excretion to NH4  (rate mu_52)
! (2) Zoo mortality to SDet (rate mu_56)
!
        do k=1,N
          cff1=dtbio*mu_52
          cff2=dtbio*mu_56*zoo(k)      ! grazing by higher trophic levels
          Zoo(k)=Zoo(k)/(1.+cff1+cff2)
          NH4(k)=NH4(k)+Zoo(k)*cff1
          SDet(k)=SDet(k)+Zoo(k)*cff2
        enddo
!
! (1) Coagulation of Phyt+SDet to LDet
!
        do k=1,N
          cff1=dtbio*mu_47*(SDet(k)+PHyt(k))
          Phyt(k)=Phyt(k)/(1.+cff1)
          SDet(k)=SDet(k)/(1.+cff1)
          LDet(k)=LDet(k)+(SDet(k)+Phyt(k))*cff1
        enddo
!
! (1) SDet breakdown to NH4
!
        do k=1,N
          cff1=dtbio*mu_62
          SDet(k)=SDet(k)/(1.+cff1)
          NH4(k)=NH4(k)+SDet(k)*cff1
        enddo
!
! (1) LDet recycling to NH4 (remineralization)
!
        do k=1,N
          cff1=dtbio*mu_72
          LDet(k)=LDet(k)/(1.+cff1)
          NH4(k)=NH4(k)+LDet(k)*cff1
        enddo
!
#ifdef OXYGEN
        do k=1,N
           biofinal(k)=Phyt(k)+Zoo(k)+SDet(k)+LDet(k)
           cff=(biofinal(k)-bioinit(k))*(138./16.)
!          convert the forcing to [mmol O2 m-3 s-1] to [ml O2 l-1 s-1]
           cff=cff*22.4/1000.
           cff3=cff3*(3./2.)*22.4/1000.  ! denitrification term [NH4==>NO3]
           O2(k)=O2(k)+cff-cff3
        enddo
        dissO2=6.
        call O2sat(dissO2,temp(N),salt(N),rho1(N),O2satu,satpc,AOU)
        O2(N)=O2satu   ! set the surface boundary condition
#endif
! Vertical sinking: Vertical advection algorithm based on monotonic,
! continuous conservative parabolic splines.
!
! Part (i): Construct parabolic splines: compute vertical derivatives
! of the felds SDet,LDet. The derivatives are located at W-points;
! Neumann boundary conditions are assumed on top and bottom.
!
        FC(0)=0.
        dSDet(0)=0.
        dLDet(0)=0.
        dPhyt(0)=0.
        dChla(0)=0.

        cff6=6./ds
        do k=1,N-1,+1
          cff=1./(2.*Hz(k+1)+Hz(k)*(2.-FC(k-1)))
          FC(k)=cff*Hz(k+1)
          dSDet(k)=cff*(cff6*(SDet(k+1)-SDet(k))-Hz(k)*dSDet(k-1))
          dLDet(k)=cff*(cff6*(LDet(k+1)-LDet(k))-Hz(k)*dLDet(k-1))
          dPhyt(k)=cff*(cff6*(Phyt(k+1)-Phyt(k))-Hz(k)*dPhyt(k-1))
          dChla(k)=cff*(cff6*(Chla(k+1)-Chla(k))-Hz(k)*dChla(k-1))
        enddo

        dSDet(N)=0.
        dLDet(N)=0.
        dPhyt(N)=0.
        dChla(N)=0.

        do k=N-1,1,-1     !<-- irreversible
          dSDet(k)=dSDet(k)-FC(k)*dSDet(k+1)
          dLDet(k)=dLDet(k)-FC(k)*dLDet(k+1)
          dPhyt(k)=dPhyt(k)-FC(k)*dPhyt(k+1)
          dChla(k)=dChla(k)-FC(k)*dChla(k+1)
        enddo
!
! Part (ii): Convert dSDet,dLDet [which are now vertical derivatives
! of fields SDet,LDet at the grid box interfaces] into field values
! at these interfaces, assuming parabolic profiles within each grid
! box. Restrict these values to lie between bounds determined from
! box-averaged values of grid boxes adjacent from above and below.
! (This restriction is part of PPM-like monotonization procedure.)
!
        cff=ds/3.
        dSDet(0)=SDet(1)   ! -cff*Hz(1)*(dSDet(0)+0.5*dSDet(1))
        dLDet(0)=LDet(1)   ! -cff*Hz(1)*(dLDet(0)+0.5*dLDet(1))
        dPhyt(0)=Phyt(1)   ! -cff*Hz(1)*(dPhyt(0)+0.5*dPhyt(1))
        dChla(0)=Chla(1)   ! -cff*Hz(1)*(dChla(0)+0.5*dChla(1))

        dSDet(N) =SDet(N)  ! +cff*Hz(N)*(dSDet(N) +0.5*dSDet(N-1))
        dLDet(N) =LDet(N)  ! +cff*Hz(N)*(dLDet(N) +0.5*dLDet(N-1))
        dPhyt(N) =Phyt(N)  ! +cff*Hz(N)*(dPhyt(N) +0.5*dPhyt(N-1))
        dChla(N) =Chla(N)  ! +cff*Hz(N)*(dChla(N) +0.5*dChla(N-1))

        do k=2,N,+1        !<-- irreversible
          dSDet(k-1) =SDet(k) -cff*Hz(k)*(0.5*dSDet(k)+dSDet(k-1))
          dSDet(k-1) =max(dSDet(k-1),   min(SDet(k-1),   SDet(k)))
          dSDet(k-1) =min(dSDet(k-1),   max(SDet(k-1),   SDet(k)))

          dLDet(k-1) =LDet(k) -cff*Hz(k)*(0.5*dLDet(k)+dLDet(k-1))
          dLDet(k-1) =max(dLDet(k-1),   min(LDet(k-1),   LDet(k)))
          dLDet(k-1) =min(dLDet(k-1),   max(LDet(k-1),   LDet(k)))

          dPhyt(k-1) =Phyt(k) -cff*Hz(k)*(0.5*dPhyt(k)+dPhyt(k-1))
          dPhyt(k-1) =max(dPhyt(k-1),   min(Phyt(k-1),   Phyt(k)))
          dPhyt(k-1) =min(dPhyt(k-1),   max(Phyt(k-1),   Phyt(k)))

          dChla(k-1) =Chla(k) -cff*Hz(k)*(0.5*dChla(k)+dChla(k-1))
          dChla(k-1) =max(dChla(k-1),   min(Chla(k-1),   Chla(k)))
          dChla(k-1) =min(dChla(k-1),   max(Chla(k-1),   Chla(k)))
        enddo
!
! Part (iii): Convert dSDet,dLDet into flux-integrated values,
! complete PPM flux limiting. This procedure starts from assigning
! Left and Right (aR,aL) values of the interpolating parabola, then
! monotonicity conditions are checked and aL,aR are modified to fit.
! Overall, from this moment and further on it follows Colella--
! --Woodward, 1984 bombmaking code almost exactly.
!
        do k=1,N,+1        !<-- irreversible
          FC(k)=dtbio/(ds*Hz(k))

          aR=dSDet(k)
          aL=dSDet(k-1)
          cff1=(aR-aL)*6.*(SDet(k)-0.5*(aR+aL))
          cff2=(aR-aL)**2
          if ((aR-SDet(k))*(SDet(k)-aL).lt.0.) then
            aL=SDet(k)
            aR=SDet(k)
          elseif (cff1.gt.cff2) then
            aL=3.*SDet(k)-2.*aR
          elseif (cff1.lt.-cff2) then
            aR=3.*SDet(k)-2.*aL
          endif
          cu=wSDet*FC(k)
          dSDet(k-1)=SDet(k) - (1.-cu)*(0.5*(aR-aL)-( 0.5*(aR+aL)
     &                                      -SDet(k) )*(1.-2.*cu))
          aR=dLDet(k)
          aL=dLDet(k-1)
          cff1=(aR-aL)*6.*(LDet(k)-0.5*(aR+aL))
          cff2=(aR-aL)**2
          if ((aR-LDet(k))*(LDet(k)-aL).lt.0.) then
            aL=LDet(k)
            aR=LDet(k)
          elseif (cff1.gt.cff2) then
            aL=3.*LDet(k)-2.*aR
          elseif (cff1.lt.-cff2) then
            aR=3.*LDet(k)-2.*aL
          endif
          cu=wLDet*FC(k)
          dLDet(k-1)=LDet(k) - (1.-cu)*(0.5*(aR-aL)-( 0.5*(aR+aL)
     &                                      -LDet(k) )*(1.-2.*cu))

          aR=dPhyt(k)
          aL=dPhyt(k-1)
          cff1=(aR-aL)*6.*(Phyt(k)-0.5*(aR+aL))
          cff2=(aR-aL)**2
          if ((aR-Phyt(k))*(Phyt(k)-aL).lt.0.) then
            aL=Phyt(k)
            aR=Phyt(k)
          elseif (cff1.gt.cff2) then
            aL=3.*Phyt(k)-2.*aR
          elseif (cff1.lt.-cff2) then
            aR=3.*Phyt(k)-2.*aL
          endif
          cu=wPhyt*FC(k)
          dPhyt(k-1)=Phyt(k) - (1.-cu)*(0.5*(aR-aL)-( 0.5*(aR+aL)
     &                                      -Phyt(k) )*(1.-2.*cu))

          aR=dChla(k)
          aL=dChla(k-1)
          cff1=(aR-aL)*6.*(Chla(k)-0.5*(aR+aL))
          cff2=(aR-aL)**2
          if ((aR-Chla(k))*(Chla(k)-aL).lt.0.) then
            aL=Chla(k)
            aR=Chla(k)
          elseif (cff1.gt.cff2) then
            aL=3.*Chla(k)-2.*aR
          elseif (cff1.lt.-cff2) then
            aR=3.*Chla(k)-2.*aL
          endif
          cu=wChla*FC(k)
          dChla(k-1)=Chla(k) - (1.-cu)*(0.5*(aR-aL)-( 0.5*(aR+aL)
     &                                      -Chla(k) )*(1.-2.*cu))

        enddo

        dSDet(N)=0.   ! Set no-flux boundary conditions at top.
        dLDet(N)=0.
        dPhyt(N)=0.
        dChla(N)=0.
!

! Apply fluxes:

!
        do k=1,N
          SDet(k)=SDet(k) + wSDet*FC(k)*(dSDet(k)-dSDet(k-1))
          LDet(k)=LDet(k) + wLDet*FC(k)*(dLDet(k)-dLDet(k-1))
          Phyt(k)=Phyt(k) + wPhyt*FC(k)*(dPhyt(k)-dPhyt(k-1))
          Chla(k)=Chla(k) + wChla*FC(k)*(dChla(k)-dChla(k-1))
        enddo

      ENDDO  ! <-- ITER
!
! Write back
!
      do k=1,N
        t(k,nnew,iNO3_)=min(0., t(k,nnew,iNO3_)) +NO3(k)
        t(k,nnew,iNH4_)=min(0., t(k,nnew,iNH4_)) +NH4(k)
        t(k,nnew,iPhyt)=min(0., t(k,nnew,iPhyt)) +Phyt(k)
        t(k,nnew,iZoo_)=min(0., t(k,nnew,iZoo_)) +Zoo(k)
        t(k,nnew,iSDet)=min(0., t(k,nnew,iSDet)) +SDet(k)
        t(k,nnew,iLDet)=min(0., t(k,nnew,iLDet)) +LDet(k)
        t(k,nnew,iChla)=Phyt(k)*CN_Phyt*12.*theta(k)
#ifdef OXYGEN
        t(k,nnew,iOxy_)=min(0., t(k,nnew,iOxy_)) +O2(k)
#endif
      enddo
      return
      end

#else
      subroutine biology_empty
      return
      end
#endif
