!
! $Id$
!
#include "cppdefs.h"
#if defined BIOLOGY && defined BIO_N2ChlPZD2

      subroutine biology_tile (Istr,Iend,Jstr,Jend)
!
! Compute biological forcing functions as defined by the
! Fasham et al. [JMR, 48, 591-639, 1990]
!
! In this particular implementation there is 7 compartments:
! NO3, NH4, Chlorophyl, PHYTOplankton, ZOOplanknton, Small Detritus, 
! and Large Detritus.
!
      implicit none
      integer Istr,Iend,Jstr,Jend
#include "param.h"
#include "grid.h"
#include "ocean3d.h"
#include "scalars.h"
#include "forces.h"
      real kwater, kChla, palpha,
     &     K_NO3, K_NH4, mu_P_Sd, mu_Agg, gmax, K_Phyt,
     &     beta, mu_Sd_A, mu_Z_A, mu_Z_Sd, mu_A_N, mu_Ld_A, 
     &     CN_Phyt, theta_m, chla_C, wPhyt, wSDet, wLDet
      integer ITERMAX
      parameter (
     &  ITERMAX = 3,      ! number of small implicit time steps
!
! Parameters as in Table 1; Fasham et al. [JMR, 48, 591-639, 1990]
!
     &  kwater  = 0.04,   ! light attenuation due to sea water  [m-1]
     &  kChla   = 0.024,  ! light attenuation by Chlorophyl  
                          !                         [(m^2 mg Chla)-1]
     &  palpha  = 1.0,    ! initial slope of the P-I curve
                          !                             [(W m-2 d)-1]
     &  CN_Phyt = 6.625,  ! C:N ratio for phytoplankton
                          !                       [mMol C (mMol N)-1]
     &  theta_m =0.053478,! maximum Cellular Chlorophyll to Carbon
                          !                      Ratio [mg Chla/mg C]
     &  chla_C  = 1.3538, ! mass balance for chla molecule, 893.5/660.
                          !                        [mg Chla (mg C)-1]
     &  K_NO3   = 1./.75, ! inverse half-saturation for Phytoplankton
                          !         NO3 uptake       [1/(mMol N m-3)]
     &  K_NH4   = 1./.5,  ! inverse half-saturation for Phytoplankton
                          !         NH4 uptake       [1/(mMol N m-3)]
     &  mu_A_N  = 0.05,   ! Oxidation of NH4 to NO3 (Nitrification) [d-1]
     &  mu_P_Sd = 0.072,  ! Phyto mortality to SDet rate        [d-1]
     &  gmax    = 0.6,    ! maximum Zooplankton growth rate     [d-1]
     &  beta    = 0.75,   ! Zooplankton assimilation efficiency of
                          !                       Phytoplankton [n.d.]
     &  K_Phyt  = 1.0,    ! Zooplankton half-saturation constant
     &                    !                       for ingestion [d-1]
     &  mu_Z_A  = 0.10,   ! Zooplankton specific excretion rate [d-1]
     &  mu_Z_Sd = 0.025,  ! Zooplankton mortality to Detritus   [d-1] 
     &  mu_Sd_A = 0.03,   ! Small Detrital breakdown to NH4 rate [d-1]
     &  mu_Agg  = 0.005,  ! Specific (per unit Phyto+SDet) aggregation
                          !  rate:   SDet+Phyt ==> LDet [1/(mMol N * d)]
     &  mu_Ld_A = 0.01,   ! Large Detrital recycling to NH4 rate [d-1]
     &  wPhyt   = 0.5,    ! sinking velocities for Phytoplankton [m.d-1]
     &  wSDet   = 1.0,    !                        Small Detritus
     &  wLDet   =10.0  )  !                        Large Detritus
!
      integer i,j,k, ITER, iB
      real    NO3(N), NH4(N), Phyt(N), Zoo(N), 
     &        SDet(N), LDet(N), theta(N), 
     &        aJ(N),FC(0:N),
     &        PAR, attn, Vp, Epp, Q, cu, aL,aR, dtdays,
     &        cff,cff1,cff2,cff6,
     &        SB(N,4),dSB(0:N,4),wSB(4)
!
# include "compute_auxiliary_bounds.h"
!
      dtdays=dt/(24.*3600.*float(ITERMAX))  ! time step as fraction of day.
!
!
! Since the following solver is iterative to achieve implicit
! discretization of the biological interaction, two time slices are
! required, BIO where BIO is understood as vector of
! biological state variables: BIO=[NO3,NH4,Phyt,Zoo,SDet]. Assume
! that the iterations converge, the newly obtained state variables
! satisfy equations
!
!           BIO = BIO + dtdays * rhs(BIO)
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
!#  ifdef EW_PERIODIC
!#   define I_RANGE Istr,Iend
!#  else
!#   define I_RANGE IstrR,IendR
!#  endif
!#  ifdef NS_PERIODIC
!#   define J_RANGE Jstr,Jend
!#  else
!#   define J_RANGE JstrR,JendR
!#  endif
#  define I_RANGE Istr,Iend
#  define J_RANGE Jstr,Jend

      do j=J_RANGE
        do i=I_RANGE
!
! Extract biological variables from tracer arrays; place them into
! scratch variables; restrict their values to be positive definite.
!
      do k=1,N
        NO3(k) =max(t(i,j,k,nnew,iNO3_)  ,0.)  ! Nitrate
        NH4(k) =max(t(i,j,k,nnew,iNH4_)  ,0.)  ! Ammonium
        Phyt(k)=max(t(i,j,k,nnew,iPhy1)  ,0.)  ! Phytoplankton
        Zoo(k) =max(t(i,j,k,nnew,iZoo1)  ,0.)  ! Zooplankton
        SDet(k)=max(t(i,j,k,nnew,iDet1)  ,0.)  ! Small Detritus
        LDet(k)=max(t(i,j,k,nnew,iDet2)  ,0.)  ! Large Detritus
        theta(k)=max(t(i,j,k,nnew,iChla) ,0.)  ! Chla/Phyto ratio
     &        /(Phyt(k)*CN_Phyt*12. +1.E-20)
      enddo


      DO ITER=1,ITERMAX      !--> Start internal iterations to achieve
                             !    nonlinear backward-implicit solution.

        PAR=srflx(i,j)*rho0*Cp*0.43

        if (PAR.gt.0.) then
!
!   *** SUN IS UP ***
!
! Calulate aJ: Set Photosynthetically Available Radiation (PAR) at
! surface from solar radiation x 0.43. Then, within each grid box
! compute attenuation coefficient based on the concentration of
! Phytoplankton inside the grid box, and attenuate PAR from surface
! down (thus, PAR at certain depth depends on the whole distribution
! of Phytoplankton above). To compute aJ, one needs PAR somewhat in
! the middle of the gridbox, so that attenuation "attn" corresponds
! to half of the grid box height, while PAR is multiplied by it
! twice: once to get it in the middle of grid-box and once the
! compute on trhe lower grid-box interface;
!
          do k=N,1,-1     !<-- irreversible

            attn=exp(-0.5*(kwater+kChla *
     &               theta(k)*Phyt(k)*CN_Phyt*12.+1.e-20) *
     &               (z_w(i,j,k)-z_w(i,j,k-1)))

            PAR=PAR*attn
            Vp=0.59*(1.066**t(i,j,k,nnew,itemp))   ! From Eppley
            cff=PAR*palpha*theta(k)        
            Epp=Vp/sqrt(Vp*Vp+cff*cff)
            aJ(k)=Epp*cff

            cff=K_NO3*NO3(k)+K_NH4*NH4(k)
            Q=cff/(1.+cff)

            cff=dtdays*aJ(k)*Q
            theta(k)=(theta(k)+theta_m*Chla_c*Epp*Q*cff)/(1.+cff)
            PAR=PAR*attn
          enddo
!
! (1) NO3 uptake by Phyto
!
          do k=1,N
            cff1=dtdays*Phyt(k)*aJ(k)*K_NO3
     &               /(1.+K_NO3*NO3(k)+K_NH4*NH4(k))
            NO3(k)=NO3(k)/(1.+cff1)
            Phyt(k)=Phyt(k)+cff1*NO3(k) 
          enddo
!
! (1) NH4 uptake by Phyto
! (2) nitrification of NH4 ==> NO3
!
          do k=1,N
            cff1=dtdays*Phyt(k)*aJ(k)*K_NH4
     &               /(1.+K_NO3*NO3(k)+K_NH4*NH4(k))
            cff2=dtdays*mu_A_N
            NH4(k)=NH4(k)/(1.+cff1+cff2)
            Phyt(k)=Phyt(k)+NH4(k)*cff1
            NO3(k)=NO3(k)+NH4(k)*cff2
          enddo
!
        else
!
!   *** SUN IS DOWN ***
!
! (1) nitrification of NH4 ==> NO3
!
          do k=1,N
            cff1=dtdays*mu_A_N
            NH4(k)=NH4(k)/(1.+cff1)
            NO3(k)=NO3(k)+NH4(k)*cff1
          enddo
!
        endif
!
! (1) Phytoplankton grazing by Zooplankton to Zoo and SDet
! (2) Phytoplankton mortality to SDet (mu_P_Sd)
!
        do k=1,N
          cff1=dtdays*gmax*Zoo(k)/(K_Phyt+Phyt(k))
          cff2=dtdays*mu_P_Sd
          Phyt(k)=Phyt(k)/(1.+cff1+cff2)
          Zoo(k)=Zoo(k)+Phyt(k)*cff1*beta
          SDet(k)=SDet(k)+Phyt(k)*(cff1*(1.-beta)+cff2)
        enddo
!
! (1) Zoo excretion to NH4  (rate mu_Z_A)
! (2) Zoo mortality to SDet (rate mu_Z_Sd)
!
        do k=1,N
          cff1=dtdays*mu_Z_A
          cff2=dtdays*mu_Z_Sd
          Zoo(k)=Zoo(k)/(1.+cff1+cff2)
          NH4(k)=NH4(k)+Zoo(k)*cff1
          SDet(k)=SDet(k)+Zoo(k)*cff2
        enddo
!
! (1) Coagulation of Phyt+SDet to LDet
!
        do k=1,N
          cff1=dtdays*mu_Agg*(SDet(k)+PHyt(k))
          cff2=1./(1.+cff1)
          Phyt(k)=Phyt(k)*cff2
          SDet(k)=SDet(k)*cff2
          LDet(k)=LDet(k)+(Phyt(k)+SDet(k))*cff1
        enddo
!
! (1) SDet breakdown to NH4
!
        do k=1,N
          cff1=dtdays*mu_Sd_A
          SDet(k)=SDet(k)/(1.+cff1)
          NH4(k)=NH4(k)+SDet(k)*cff1
        enddo
!
! (1) LDet recycling to NH4 (remineralization)
!
        do k=1,N
          cff1=dtdays*mu_Ld_A
          LDet(k)=LDet(k)/(1.+cff1)
          NH4(k)=NH4(k)+LDet(k)*cff1
        enddo

!
! Vertical sinking: Vertical advection algorithm based on monotonic,
! continuous conservative parabolic splines.
!
        do k=1,N
          SB(k,1)=theta(k)*Phyt(k)*CN_Phyt*12.
          SB(k,2)=Phyt(k)
          SB(k,3)=SDet(k)
          SB(k,4)=LDet(k)
        enddo
        wSB(1)=wPhyt
        wSB(2)=wPhyt
        wSB(3)=wSDet
        wSB(4)=wLDet

        do iB=1,4

! Part (i): Construct parabolic splines: compute vertical derivatives
! of the fields SB. The derivatives are located at W-points;
! Neumann boundary conditions are assumed on top and bottom.
!
          dSB(0,iB)=0.
          FC(0)=0.
          cff6=6.
          do k=1,N-1
            cff=1./(2.*Hz(i,j,k+1)+Hz(i,j,k)*(2.-FC(k-1)))
            FC(k)=cff*Hz(i,j,k+1)
            dSB(k,iB)=cff*(cff6*(SB(k+1,iB)-SB(k,iB))
     &                             -Hz(i,j,k)*dSB(k-1,iB))
          enddo
          dSB(N,iB)=0.
          do k=N-1,1,-1     !<-- irreversible
            dSB(k,iB)=dSB(k,iB)-FC(k)*dSB(k+1,iB)
          enddo
!
! Part (ii): Convert dSB [which are now vertical derivatives
! of fields SB at the grid box interfaces] into field values
! at these interfaces, assuming parabolic profiles within each grid
! box. Restrict these values to lie between bounds determined from
! box-averaged values of grid boxes adjscent from above and below.
! (This restriction is part of PPM-like monotonization procedure.)
!
          cff=1./3.
          dSB(0,iB)=SB(1,iB) !-cff*Hz(1)*(dSB(0,iB)+0.5*dSB(1,iB))
          dSB(N,iB)=SB(N,iB) !+cff*Hz(N)*(dSB(N,iB)+0.5*dSB(N-1,iB))
          do k=2,N          !<-- irreversible
            dSB(k-1,iB)=SB(k,iB)
     &                 -cff*Hz(i,j,k)*(0.5*dSB(k,iB)+dSB(k-1,iB))
            dSB(k-1,iB)=max(dSB(k-1,iB),min(SB(k-1,iB),SB(k,iB)))
            dSB(k-1,iB)=min(dSB(k-1,iB),max(SB(k-1,iB),SB(k,iB)))
          enddo
!  
! Part (iii): Convert dSB into flux-integrated values,
! complete PPM flux limiting. This procedure starts from assigning
! Left and Right (aR,aL) values of the interpolating parabolae, then
! monotonicity conditions are checked and aL,aR are modified to fit.
! Overall, from this moment and further on it follows Colella--
! --Woodward, 1984 bombmaking code almost exactly.
!
          do k=1,N           !<-- irreversible
            FC(k)=dtdays/Hz(i,j,k)
            aR=dSB(k,iB)
            aL=dSB(k-1,iB)
            cff1=(aR-aL)*6.*(SB(k,iB)-.5*(aR+aL))
            cff2=(aR-aL)**2
            if ((aR-SB(k,iB))*(SB(k,iB)-aL).lt.0.) then
              aL=SB(k,iB)
              aR=SB(k,iB)
            elseif (cff1.gt.cff2) then
              aL=3.*SB(k,iB)-2.*aR
            elseif (cff1.lt.-cff2) then
              aR=3.*SB(k,iB)-2.*aL
            endif
            cu=wSB(iB)*FC(k)
            dSB(k-1,iB)=SB(k,iB)-(1.-cu)*(.5*(aR-aL)-(.5*(aR+aL)
     &                                   -SB(k,iB) )*(1.-2.*cu))
          enddo
          dSB(N,iB)=0.   ! Set no-flux boundary conditions at top.
!
! Apply fluxes:
!
          do k=1,N
            SB(k,iB)=SB(k,iB)+wSB(iB)*FC(k)*(dSB(k,iB)-dSB(k-1,iB))
          enddo
        enddo  ! <-- iB

        do k=1,N
          theta(k)=SB(k,1)/(SB(k,2)*CN_Phyt*12.+1.E-20)
          Phyt(k) =SB(k,2)
          SDet(k) =SB(k,3)
          LDet(k) =SB(k,4)
        enddo
      ENDDO  ! <-- ITER
!
! Write back
!
          do k=1,N
           t(i,j,k,nnew,iNO3_)=min(t(i,j,k,nnew,iNO3_),0.) +NO3(k)
           t(i,j,k,nnew,iNH4_)=min(t(i,j,k,nnew,iNH4_),0.) +NH4(k)
           t(i,j,k,nnew,iPhy1)=min(t(i,j,k,nnew,iPhy1),0.) +Phyt(k)
           t(i,j,k,nnew,iZoo1)=min(t(i,j,k,nnew,iZoo1),0.) +Zoo(k)
           t(i,j,k,nnew,iDet1)=min(t(i,j,k,nnew,iDet1),0.) +SDet(k)
           t(i,j,k,nnew,iDet2)=min(t(i,j,k,nnew,iDet2),0.) +LDet(k)
           t(i,j,k,nnew,iChla)=min(t(i,j,k,nnew,iChla),0.) +
     &                                 CN_Phyt*12.*Phyt(k)*theta(k)
          enddo
        enddo
      enddo

#else
      subroutine biology_empty ()
#endif
      return
      end

