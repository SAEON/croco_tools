! $Id: bio_N2P2Z2D2.F 697 2011-04-11 12:35:17Z gcambon $
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
#ifdef BIOLOGY

      subroutine biology_tile (Istr,Iend,Jstr,Jend)
!
! Compute biological forcing functions as defined by the
! Fasham et al. [JMR, 48, 591-639, 1990]
!
! In this particular implementation there is 8 compartments:
! NO3, NH4, Chlorophyl, PHYTOplankton, ZOOplanknton, Small Detritus, 
! and Large Detritus.
!
      implicit none
      integer Istr,Iend,Jstr,Jend
#include "param.h"
#include "grid.h"
#include "ocean3d.h"
#include "diagnostics.h"
#include "scalars.h"
#include "forces.h"
#include "mixing.h"

      real kwater, kChla, palpha1, palpha2,
     &     K1_NO3, K2_NO3, K1_NH4, K2_NH4, 
     &     mu_P1_Sd, mu_P2_Sd, gmax1, gmax2, 
     &     K_Zoo1, K_Zoo2, beta1, beta2,
     &     e_11, e_12, e_21, e_22, e_ZZ,                !mu_agg
     &     mu_Sd_A, mu_Z1_A, mu_Z2_A, mu_Z1_Sd, mu_Z2_Ld,  
     &     mu_A_N, mu_Ld_A, CN_Phyt, wPhy2, wSDet, wLDet, !,chla_C,
     &     ThisFlux(N, NumFluxTerms), opc
#ifdef VAR_CHL_C
     &    , theta1_m, theta2_m
#else
     &    , theta_m
#endif
#ifdef DIAGNOSTICS_BIO
     &    , ThisVSinkFlux(N, NumVSinkTerms)  ! Upward flux is positive
     &    , somme, bilan_no3, bilan_phy1, bilan_zoo1, bilan_det1
     &    , bilan_nh4, bilan_phy2, bilan_zoo2, bilan_det2
#endif
      integer ITERMAX
      integer nsink
      parameter (
     &  ITERMAX = 3,      ! number of small implicit time steps
     &  nsink   = NumVSinkTerms + 1, !-->  add'lly: Chlorophyll
!
! Parameters as in Table 1; Fasham et al. [JMR, 48, 591-639, 1990]
!
     &  kwater   = 0.04,    ! light attenuation due to sea water  [m-1]
     &  kChla    = 0.024,   ! light attenuation by Chlorophyl  
                            !                         [(m^2 mg Chla)-1]
     &  palpha1  = 0.025,   !Small Phyto initial slope of the P-I curve
     &  palpha2  = 0.04,    !Large Phyto initial slope of the P-I curve
                            !                             [(W m-2 d)-1]
     &  CN_Phyt  = 6.625,   ! C:N ratio for phytoplankton
                            !                       [mMol C (mMol N)-1]
#ifdef VAR_CHL_C
     &  theta1_m  =0.02,    ! maximum Cellular Chl/Carbon ratio for Phy1
     &  theta2_m  =0.0535,  ! maximum Cellular Chl/Carbon ratio for Phy2
                            !                             [mg Chla/mg C]
#else
     &  theta_m  = 0.02,    ! maximum Cellular Chl/Carbon ratio for Phy
#endif
     &  K1_NO3   = 1./1.0,  ! inverse half-saturation for Small Phytoplankton
     &  K2_NO3   = 1./2.0,  ! inverse half-saturation for Large Phytoplankton
                            !         NO3 uptake       [1/(mMol N m-3)]
     &  K1_NH4   = 1./0.5,  ! inverse half-saturation for Small Phytoplankton
     &  K2_NH4   = 1./0.7,  ! inverse half-saturation for Large Phytoplankton
                            !         NH4 uptake       [1/(mMol N m-3)]
     &  mu_A_N   = 0.1,     ! Oxidation of NH4 to NO3 (Nitrification) [d-1]
     &  mu_P1_Sd = 0.027,   ! Small Phyto mortality to SDet rate        [d-1]
     &  mu_P2_Sd = 0.030,   ! Large Phyto mortality to SDet rate        [d-1]  
     &  gmax1    = 0.9,     ! maximum Small Zooplankton growth rate     [d-1]
     &  gmax2    = 0.95,    ! maximum Large Zooplankton growth rate     [d-1]
     &  beta1    = 0.75,    ! Small Zooplankton assimilation efficiency of
                            !                       Food [n.d.]
     &  beta2    = 0.70,    ! Large Zooplankton assimilation efficiency of
                            !                       Food [n.d.]
     &  K_Zoo1   = 1.0,     ! Small Zooplankton half-saturation constant
                            !                       for ingestion [d-1]
     &  K_Zoo2   = 2.0,     ! Large Zooplankton half-saturation constant
                            !                       for ingestion [d-1]
     &  e_11     = 1.,      ! Small Zooplankton preference for Small Phytoplankton
     &  e_12     = 0.4,     ! Small Zooplankton preference for Large Phytoplankton
     &  e_21     = 0.5,     ! Large Zooplankton preference for Small Phytoplankton
     &  e_22     = 1.,      ! Large Zooplankton preference for Large Phytoplankton
     &  e_zz     = 0.4,     ! Large Zooplankton preference for Small Zooplankton -->0.5
     &  mu_Z1_A  = 0.10,    ! Small Zooplankton specific excretion rate [d-1]
     &  mu_Z2_A  = 0.05,    ! Large Zooplankton specific excretion rate [d-1]
     &  mu_Z1_Sd = 0.025,   ! Small Zooplankton mortality to Detritus   [d-1]
     &  mu_Z2_Ld = 0.05,    ! Large Zooplankton mortality to Detritus   [d-1] 
     &  mu_Sd_A  = 0.10,    ! Small Detrital breakdown to NH4 rate [d-1]
!      &  mu_Agg  = 0.005,  ! Specific (per unit Phyto+SDet) aggregation
                            !  rate:   SDet+Phyt ==> LDet [1/(mMol N * d)]
     &  mu_Ld_A  = 0.05,    ! Large Detrital recycling to NH4 rate [d-1]
     &  wPhy2    = 0.5,     ! sinking velocities for Large Phytoplankton [m.d-1]
     &  wSDet    = 0.5,     !                        Small Detritus
     &  wLDet    = 5.0  )   !                        Large Detritus
!
      integer i,j,k, ITER, iB
      real    NO3(N), NH4(N), Phyt_1(N), Phyt_2(N),
     &        Zoo_1(N), Zoo_2(N), SDet(N), LDet(N)
# ifdef VAR_CHL_C
     &      , Chla_1(N), Chla_2(N)
# endif
# if defined DIAGNOSTICS_BIO
     &      , iflux, l, m
     &      , dtsec        ! length of time step in seconds (for gas exchange)
# endif /* DIAGNOSTICS_BIO */
  
      real    aJ_1(N), aJ_2(N), FC(0:N), Ft_1(N), Ft_2(N),PAR,
     &        PARsup, attn, Vp1, Vp2, Epp1, Epp2, cu, aL,aR, dtdays,
     &        inh1_NH4, inh2_NH4, L1_NH4, L2_NH4, L1_NO3, L2_NO3,
     &        L1_TOT, L2_TOT, cff11, cff21,
     &        cff, cff1, cff2, cff3, cff6, cffa, cffb,
     &        SB(N,nsink),dSB(0:N,nsink),wSB(nsink)
# ifdef DIAGNOSTICS_BIO
     &        , LastVSinkFlux, ColumnMassOld(0:NumVSinkTerms) 
     &        , ColumnMassNew(0:NumVSinkTerms)
# endif /* DIAGNOSTICS_BIO */
!
# include "compute_auxiliary_bounds.h"
!
      dtdays=dt/(24.*3600.*float(ITERMAX))  ! time step as fraction of day.
# ifdef DIAGNOSTICS_BIO
      dtsec = dt / float(ITERMAX)           ! time step in seconds
# endif /* DIAGNOSTICS_BIO */
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

/*
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
*/
#  define I_RANGE Istr,Iend
#  define J_RANGE Jstr,Jend

      do j=J_RANGE
        do i=I_RANGE
# ifdef DIAGNOSTICS_BIO
! Reset the biogeochemical fluxes. This is necessary because the 
! biological routine uses multiple. time steps for each physical time step.
          do k=1,N
            do l=1,NumFluxTerms
              bioFlux(i,j,k,l) = 0.0
            enddo
            do l=1,NumVSinkTerms
              bioVSink(i,j,k,l) = 0.0
            enddo
          enddo
# endif /* DIAGNOSTICS_BIO */
# ifdef MASKING
        if (rmask(i,j) .eq. 1) then
# endif /* MASKING */
!
! Extract biological variables from tracer arrays; place them into
! scratch variables; restrict their values to be positive definite.
!
      do k=1,N
        NO3(k)   =max(t(i,j,k,nnew,iNO3_)  ,0.)  ! Nitrate
        NH4(k)   =max(t(i,j,k,nnew,iNH4_)  ,0.)  ! Ammonium
        Phyt_1(k)=max(t(i,j,k,nnew,iPhy1)  ,0.)  ! Small Phytoplankton
        Phyt_2(k)=max(t(i,j,k,nnew,iPhy2)  ,0.)  ! Large Phytoplankton
# ifdef VAR_CHL_C
        Chla_1(k)=max(t(i,j,k,nnew,iChl1)  ,0.)  ! Chlor a
        Chla_2(k)=max(t(i,j,k,nnew,iChl2)  ,0.)  ! Chlor a
# endif
        Zoo_1(k) =max(t(i,j,k,nnew,iZoo1)  ,0.)  ! Small Zooplankton
        Zoo_2(k) =max(t(i,j,k,nnew,iZoo2)  ,0.)  ! Large Zooplankton
        SDet(k)  =max(t(i,j,k,nnew,iDet1)  ,0.)  ! Small Detritus
        LDet(k)  =max(t(i,j,k,nnew,iDet2)  ,0.)  ! Large Detritus
!
# ifdef VAR_CHL_C
        if (Phyt_1(k).gt.0.001 .and. Chla_1(k).gt.0.001) then
          theta1(i,j,k) = Chla_1(k)/(Phyt_1(k)*CN_Phyt*12.)       ! Chl1/Phy1 ratio
        else                                                      ! [mg Chla (mg C)-1]
          theta1(i,j,k) = theta1_m
        endif
        if (Phyt_2(k).gt.0.001 .and. Chla_2(k).gt.0.001) then
          theta2(i,j,k) = Chla_2(k)/(Phyt_2(k)*CN_Phyt*12.)       ! Chl2/Phy2 ratio
        else                                                      ! [mg Chla (mg C)-1]
          theta2(i,j,k) = theta2_m
        endif
# endif
      enddo


      DO ITER=1,ITERMAX      !--> Start internal iterations to achieve
                             !    nonlinear backward-implicit solution.

        PAR=srflx(i,j)*rho0*Cp*0.43
        opc=0.01*PAR

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

# ifdef VAR_CHL_C
            attn=exp(-0.5*(kwater+kChla*(Chla_1(k)+Chla_2(k)))
     &                   *(z_w(i,j,k)-z_w(i,j,k-1))  )
# else
            attn=exp(-0.5*(kwater+kChla*
     &               theta_m*CN_Phyt*(Phyt_1(k)+Phyt_2(k))*12.+1.e-20)*
     &               (z_w(i,j,k)-z_w(i,j,k-1))  )
# endif

            PAR=PAR*attn
            PARsup=PAR
            Vp1=0.5570*(1.066**t(i,j,k,nnew,itemp))   ! From Eppley
            Vp2=0.8356*(1.066**t(i,j,k,nnew,itemp))
# ifdef VAR_CHL_C
            cff1=PAR*palpha1*theta1(i,j,k)
            cff2=PAR*palpha2*theta2(i,j,k)
# else
            cff1=PAR*palpha1
            cff2=PAR*palpha2
# endif
            Epp1=Vp1/sqrt(Vp1*Vp1+cff1*cff1)
            Epp2=Vp2/sqrt(Vp2*Vp2+cff2*cff2)
            aJ_1(k)=Epp1*cff1
            aJ_2(k)=Epp2*cff2
!
!           1--> Small Phytoplankton
!           2--> Large Phytoplankton
            inh1_NH4=1./(1.+ NH4(k)*K1_NH4)
            inh2_NH4=1./(1.+ NH4(k)*K2_NH4)
!           theta adaptation
# ifdef VAR_CHL_C 
            L1_NH4=K1_NH4*NH4(k)/(1.+K1_NH4*NH4(k))
            L2_NH4=K2_NH4*NH4(k)/(1.+K2_NH4*NH4(k))
            L1_NO3=K1_NO3*NO3(k)*inh1_NH4/(1.+K1_NO3*NO3(k))
            L2_NO3=K2_NO3*NO3(k)*inh2_NH4/(1.+K2_NO3*NO3(k))
            L1_TOT=L1_NH4+L1_NO3
            L2_TOT=L2_NH4+L2_NO3

            cff1=dtdays*aJ_1(k)*L1_TOT
            cff2=dtdays*aJ_2(k)*L2_TOT

            theta1(i,j,k)=(theta1(i,j,k)
     &                     +theta1_m*Epp1*L1_TOT*cff1)/(1.+cff1)
            theta2(i,j,k)=(theta2(i,j,k)
     &                     +theta2_m*Epp2*L2_TOT*cff2)/(1.+cff2)
!            cff1=dtdays*aJ_1(k)*L1_TOT*Phyt_1(k)
!            cff11=cff1/(Phyt_1(k)+Phyt_2(k))
!            cff2=dtdays*aJ_2(k)*L2_TOT*Phyt_2(k)
!            cff21=cff2/(Phyt_1(k)+Phyt_2(k))
!            theta(i,j,k)=( theta(i,j,k)+theta_m*cff11*Epp1*L1_TOT
!     &                         +theta_m*cff21*Epp2*L2_TOT
!     &               ) / (1+cff1+cff2)
# endif
            
!
! Nutrient limitation terms:--> Parker 1993 Ecol Mod.,66,113-120                 
! (1) NO3 uptake by Phyto (large and small)
!
            cff1=dtdays*aJ_1(k)*Phyt_1(k)*K1_NO3*
     &              inh1_NH4/(1.+K1_NO3*NO3(k))
            NO3(k)=NO3(k)/(1.+cff1)
            ThisFlux(k, NFlux_NewProd1) = cff1*NO3(k)
            Phyt_1(k)=Phyt_1(k)+ThisFlux(k, NFlux_NewProd1) 

            cff2=dtdays*aJ_2(k)*Phyt_2(k)*K2_NO3*
     &              inh2_NH4/(1.+K2_NO3*NO3(k))
            NO3(k)=NO3(k)/(1.+cff2)
            ThisFlux(k, NFlux_NewProd2) = cff2*NO3(k)
            Phyt_2(k)=Phyt_2(k)+ThisFlux(k, NFlux_NewProd2)
!
! (2) NH4 uptake by Phyto (large and small)
! (3) nitrification of NH4 ==> NO3
!
            cff1=dtdays*aJ_1(k)*Phyt_1(k)*K1_NH4*inh1_NH4
            cff3=dtdays*mu_A_N
            NH4(k)=NH4(k)/(1.+cff1+cff3)  
            ThisFlux(k, NFlux_RegProd1) = cff1*NH4(k)
            ThisFlux(k, NFlux_Nitrific)=cff3*NH4(k)
            Phyt_1(k)=Phyt_1(k)+ThisFlux(k, NFlux_RegProd1) 
            NO3(k)=NO3(k)+ ThisFlux(k, NFlux_Nitrific)

            cff2=dtdays*aJ_2(k)*Phyt_2(k)*K2_NH4*inh2_NH4
            NH4(k)=NH4(k)/(1.+cff2)  
            ThisFlux(k, NFlux_RegProd2) = cff2*NH4(k)
            Phyt_2(k)=Phyt_2(k)+ThisFlux(k, NFlux_RegProd2) 

            PAR=PAR*attn
!      Calcul of the euphotic depth
!
            if (PARsup.ge.opc) then
              if (PAR.ge.opc) then
                hel(i,j)=-z_w(i,j,k-1)
              else
                hel(i,j)=-z_r(i,j,k)
              endif
            endif
!
          enddo
        else
!
!   *** SUN IS DOWN ***
!
          do k = N, 1, -1
            ThisFlux(k, NFlux_NewProd1) = 0.0
            ThisFlux(k, NFlux_RegProd1) = 0.0
            ThisFlux(k, NFlux_NewProd2) = 0.0
            ThisFlux(k, NFlux_RegProd2) = 0.0
! (3) Dark nitrification of NH4 ==> NO3
            cff3=dtdays*mu_A_N
            NH4(k)=NH4(k)/(1.+cff3)
            ThisFlux(k, NFlux_Nitrific)=cff3*NH4(k)
            NO3(k)=NO3(k)+ ThisFlux(k, NFlux_Nitrific)
          enddo

!#ifdef AVERAGES
! if the sun is down, set the logical variable "sun_down"
! to true for not taking into account this time step in the averaging
!          if ((ZEROTH_TILE).and.(srflx(Istr,Jstr).eq.0.)) then
!            sun_down=.true.
!          endif
!#endif
          hel(i,j)=0.0
        endif
!
        do k=1,N

!    ---> Ft_1:Total Food available for Small Zooplankton
!    ---> Ft_2:Total Food available for Large Zooplankton
          Ft_1(k)=e_11*Phyt_1(k)+e_12*Phyt_2(k)
          Ft_2(k)=e_21*Phyt_1(k)+e_22*Phyt_2(k)+e_zz*Zoo_1(k)
 
! (1) Small Phytoplankton grazing by Small Zooplankton (to Zoo and SDet)
! (2) Small Phytoplankton grazing by Large Zooplankton (to Zoo and LDet)
! (a) Small Phytoplankton mortality to SDet (mu_P1_Sd)
!
          cff1=dtdays*gmax1*e_11*Zoo_1(k)/(k_Zoo1+Ft_1(k))
          cff2=dtdays*gmax1*e_12*Zoo_1(k)/(k_Zoo1+Ft_1(k))
          cffa=dtdays*mu_P1_Sd
          cffb=dtdays*mu_P2_Sd
          Phyt_1(k)=Phyt_1(k)/(1.+cff1+cffa)
          Phyt_2(k)=Phyt_2(k)/(1.+cff2+cffb)
          ThisFlux(k, NFlux_P1Z1Grazing) = cff1*Phyt_1(k)*beta1
          ThisFlux(k, NFlux_P2Z1Grazing) = cff2*Phyt_2(k)*beta1
          ThisFlux(k, NFlux_P1mort) = Phyt_1(k)*cffa
          ThisFlux(k, NFlux_P2mort) = Phyt_2(k)*cffb
          ThisFlux(k, NFlux_SlopFeedZ1) = cff1*Phyt_1(k)*(1.-beta1)
     &                        + cff2*Phyt_2(k)*(1.-beta1)
          Zoo_1(k)=Zoo_1(k)+ThisFlux(k, NFlux_P1Z1Grazing)
     &                     +ThisFlux(k, NFlux_P2Z1Grazing)
          SDet(k)=SDet(k)+ThisFlux(k, NFlux_P1mort)
     &                   +ThisFlux(k, NFlux_P2mort)
     &                   +ThisFlux(k, NFlux_SlopFeedZ1)

! (1) Large Phytoplankton grazing by Small Zooplankton (to Zoo and SDet)
! (2) Large Phytoplankton grazing by Large Zooplankton (to Zoo and LDet)
! (b) Large Phytoplankton mortality to SDet (mu_P2_Sd)
!
          cff1=dtdays*gmax2*e_21*Zoo_2(k)/(k_Zoo2+Ft_2(k))
          cff2=dtdays*gmax2*e_22*Zoo_2(k)/(k_Zoo2+Ft_2(k))
          cff3=dtdays*gmax2*e_zz*Zoo_2(k)/(k_Zoo2+Ft_2(k))
          Phyt_1(k)=Phyt_1(k)/(1.+cff1)
          Phyt_2(k)=Phyt_2(k)/(1.+cff2)
          Zoo_1(k)=Zoo_1(k)/(1.+cff3)
          ThisFlux(k, NFlux_P1Z2Grazing) = cff1*Phyt_1(k)*beta2
          ThisFlux(k, NFlux_P2Z2Grazing) = cff2*Phyt_2(k)*beta2
          ThisFlux(k, NFlux_Z1Z2Grazing) = cff3*Zoo_1(k)*beta2
          ThisFlux(k, NFlux_SlopFeedZ2) = cff1*Phyt_1(k)*(1.-beta2)
     &                                  + cff2*Phyt_2(k)*(1.-beta2)
     &                                  + cff3*Zoo_1(k)*(1.-beta2)

          Zoo_2(k)=Zoo_2(k)+ThisFlux(k, NFlux_P1Z2Grazing)
     &                     +ThisFlux(k, NFlux_P2Z2Grazing)
     &                     +ThisFlux(k, NFlux_Z1Z2Grazing)
          LDet(k)=LDet(k)+ThisFlux(k, NFlux_SlopFeedZ2)
        
!
! (1) Small Zooplankton grazing by Large Zooplankton (to Zoo and LDet)
! (2) Small Zoo excretion to NH4  (rate mu_Z1_A)
! (3) Small Zoo mortality to SDet (rate mu_Z1_Sd)
!
          cff2=dtdays*mu_Z1_A
          cff3=dtdays*mu_Z1_Sd
          Zoo_1(k)=Zoo_1(k)/(1.+cff2+cff3)
          ThisFlux(k, NFlux_Z1metab) = cff2*Zoo_1(k)
          ThisFlux(k, NFlux_Z1mort) = cff3*Zoo_1(k)

          NH4(k)=NH4(k)+ThisFlux(k, NFlux_Z1metab)
          SDet(k)=SDet(k)+ThisFlux(k, NFlux_Z1mort)
        
!
! (1) Large Zoo excretion to NH4  (rate mu_Z2_A)
! (2) Large Zoo mortality to LDet (rate mu_Z2_Ld)
!
          cff1=dtdays*mu_Z2_A
          cff2=dtdays*mu_Z2_Ld
          Zoo_2(k)=Zoo_2(k)/(1.+cff1+cff2)
          ThisFlux(k, NFlux_Z2metab) = cff1*Zoo_2(k)
          ThisFlux(k, NFlux_Z2mort) = cff2*Zoo_2(k)
          NH4(k)=NH4(k)+ThisFlux(k, NFlux_Z2metab)
          LDet(k)=LDet(k)+ThisFlux(k, NFlux_Z2mort)

!
! (1) SDet breakdown to NH4
!
          cff1=dtdays*mu_Sd_A
          SDet(k)=SDet(k)/(1.+cff1)
          ThisFlux(k, NFlux_ReminD1)=SDet(k)*cff1
          NH4(k)=NH4(k)+ThisFlux(k, NFlux_ReminD1)
        
!
! (1) LDet recycling to NH4 (remineralization)
!
          cff1=dtdays*mu_Ld_A
          LDet(k)=LDet(k)/(1.+cff1)
          ThisFlux(k, NFlux_ReminD2)=LDet(k)*cff1
          NH4(k)=NH4(k)+ThisFlux(k, NFlux_ReminD2)

        enddo

!
! Vertical sinking: Vertical advection algorithm based on monotonic,
! continuous conservative parabolic splines.
!
        do k=1,N
# ifdef VAR_CHL_C
          SB(k,1)=theta2(i,j,k)*Phyt_2(k)*CN_Phyt*12.
# else
          SB(k,1)=theta_m*Phyt_2(k)*CN_Phyt*12.
# endif
          SB(k,2)=Phyt_2(k)
          SB(k,3)=SDet(k)
          SB(k,4)=LDet(k)
        enddo
        wSB(1)=wPhy2
        wSB(2)=wPhy2
        wSB(3)=wSDet
        wSB(4)=wLDet

        do iB=1,nsink

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
#ifdef DIAGNOSTICS_BIO
        do iflux = 1, NumVSinkTerms
          ColumnMassOld(iflux) = 0.0
          ColumnMassNew(iflux) = 0.0
        enddo
#endif /* DIAGNOSTICS_BIO */

        do k=1,N
         ! Chlorophyll
#ifdef VAR_CHL_C
          theta2(i,j,k)= SB(k,1)/(SB(k,2)*CN_Phyt*12.+1.E-20)
#endif
#ifdef DIAGNOSTICS_BIO
! ColumnMassOld and ColumnMassNew are needed to compute the sinking flux
! into the sediment
         ! Large Phytoplankton
          ColumnMassOld(1) = ColumnMassOld(1)+Phyt_2(k)*Hz(i,j,k)
          ThisVSinkFlux(k, NFlux_VSinkP2) = Phyt_2(k)-SB(k,2)
#endif /* DIAGNOSTICS_BIO */
          Phyt_2(k) =SB(k,2)
#ifdef DIAGNOSTICS_BIO
          ColumnMassNew(1)=ColumnMassNew(1)+Phyt_2(k) * Hz(i,j,k)
#endif /* DIAGNOSTICS_BIO */

          ! Small detritus
#ifdef DIAGNOSTICS_BIO
          ColumnMassOld(2) = ColumnMassOld(2)+SDet(k)*Hz(i,j,k)
          ThisVSinkFlux(k, NFlux_VSinkD1) = SDet(k)-SB(k,3)
#endif /* DIAGNOSTICS_BIO */
          SDet(k) =SB(k,3)
#ifdef DIAGNOSTICS_BIO
          ColumnMassNew(2) = ColumnMassNew(2)+SDet(k)*Hz(i,j,k)
#endif /* DIAGNOSTICS_BIO */
          
            ! Large detritus
#ifdef DIAGNOSTICS_BIO
          ColumnMassOld(3) = ColumnMassOld(3)+LDet(k)*Hz(i,j,k)
          ThisVSinkFlux(k, NFlux_VSinkD2) = LDet(k)-SB(k,4)
#endif /* DIAGNOSTICS_BIO */
          LDet(k) =SB(k,4)
#ifdef DIAGNOSTICS_BIO
          ColumnMassNew(3) = ColumnMassNew(3)+LDet(k)*Hz(i,j,k)
#endif /* DIAGNOSTICS_BIO */
        enddo
!
#ifdef DIAGNOSTICS_BIO
! Transfer fluxes to global arrays at the end of each biological time step
! for computational efficiency, divide now by dtsec to get the correct units
        do iflux = 1, NumFluxTerms
           do k = 1, N
! biological Flux in mmolN/s
              bioFlux(i,j,k,iflux) = ( bioFlux(i,j,k,iflux) +
     &            ThisFlux(k, iflux)*Hz(i,j,k) / (pn(i,j)*pm(i,j)*dt) )
# ifdef MASKING
     &             * rmask(i,j)
# endif /* MASKING */
           end do
        end do
        do iflux = 1, NumVSinkTerms
! Compute the vertical sinking flux into the sediment by comparing
! previous and current mass in this (i,j) column
! The flux is positive if upward, so usually it will be negative, i.e.
! into the sediment.
           LastVSinkFlux = ( ColumnMassNew(iflux) -
     &          ColumnMassOld(iflux) ) / dtsec
           bioVSink(i,j,0,iflux) = (  bioVSink(i,j,0,iflux)
     &                              + LastVSinkFlux / float(ITERMAX) )
#  ifdef MASKING
     &          * rmask(i,j)
#  endif /* MASKING */
           do k = 1, N
              LastVSinkFlux = LastVSinkFlux +
     &           ( ThisVSinkFlux(k,iflux)*Hz(i,j,k) 
     &           / (pn(i,j)*pm(i,j)*dtsec) )

              bioVSink(i,j,k,iflux) =(  bioVSink(i,j,k,iflux)
     &                                + LastVSinkFlux / float(ITERMAX) )
#  ifdef MASKING
     &             * rmask(i,j)
#  endif /* MASKING */
           end do
         enddo
#endif /* DIAGNOSTICS_BIO */  
      ENDDO  ! <-- ITER
!
! Write back
!
!EM
!#ifdef DIAGNOSTICS_BIO
!        if ((i.eq.13) .and. (j.eq.15)) then
!        bilan_no3=bioFlux(13,15,32,5)
!     &           -bioFlux(13,15,32,1)-bioFlux(13,15,32,2)
!        bilan_nh4=bioFlux(13,15,32,19)+bioFlux(13,15,32,20)
!     &           +bioFlux(13,15,32,15)+bioFlux(13,15,32,16)
!     &           -bioFlux(13,15,32,3)-bioFlux(13,15,32,4)
!     &           -bioFlux(13,15,32,5)
!        bilan_phy1=bioFlux(13,15,32,1)+bioFlux(13,15,32,3)
!     &            -bioFlux(13,15,32,6)-bioFlux(13,15,32,8)
!     &            -bioFlux(13,15,32,13)
!        bilan_phy2=bioFlux(13,15,32,2)+bioFlux(13,15,32,4)
!     &            -bioFlux(13,15,32,7)-bioFlux(13,15,32,9)
!     &            -bioFlux(13,15,32,14)
!        bilan_zoo1=bioFlux(13,15,32,6)+bioFlux(13,15,32,7)
!     &            -bioFlux(13,15,32,10)-bioFlux(13,15,32,11)
!     &            -bioFlux(13,15,32,15)-bioFlux(13,15,32,17)
!        bilan_zoo2=bioFlux(13,15,32,8)+bioFlux(13,15,32,9)
!     &            +bioFlux(13,15,32,10)-bioFlux(13,15,32,12)
!     &            -bioFlux(13,15,32,16)-bioFlux(13,15,32,18)
!        bilan_det1=bioFlux(13,15,32,13)+bioFlux(13,15,32,14)
!     &            +bioFlux(13,15,32,11)+bioFlux(13,15,32,17)
!     &            -bioFlux(13,15,32,19)
!        bilan_det2=bioFlux(13,15,32,12)+bioFlux(13,15,32,18)
!     &            -bioFlux(13,15,32,20)
!        print*, bilan_no3, bilan_nh4
!        print*, bilan_phy1, bilan_phy2
!        print*, bilan_zoo1, bilan_zoo2
!        print*, bilan_det1, bilan_det2
!        print*, bioVSink(13,15,32,1)
!        print*, bioVSink(13,15,32,2)
!        print*, bioVSink(13,15,32,3)
!        somme=bilan_no3+bilan_nh4+bilan_phy1+bilan_phy2
!     &       +bilan_zoo1+bilan_zoo2+bilan_det1+bilan_det2
!        print*, 'Bilan = ',somme
!        endif
!#endif /* DIAGNOSTICS_BIO */
          do k=1,N
            t(i,j,k,nnew,iNO3_)=min(t(i,j,k,nnew,iNO3_),0.) +NO3(k)
            t(i,j,k,nnew,iNH4_)=min(t(i,j,k,nnew,iNH4_),0.) +NH4(k)
            t(i,j,k,nnew,iPhy1)=min(t(i,j,k,nnew,iPhy1),0.) +Phyt_1(k)
            t(i,j,k,nnew,iPhy2)=min(t(i,j,k,nnew,iPhy2),0.) +Phyt_2(k)
            t(i,j,k,nnew,iZoo1)=min(t(i,j,k,nnew,iZoo1),0.) +Zoo_1(k)
            t(i,j,k,nnew,iZoo2)=min(t(i,j,k,nnew,iZoo2),0.) +Zoo_2(k)
            t(i,j,k,nnew,iDet1)=min(t(i,j,k,nnew,iDet1),0.) +SDet(k)
            t(i,j,k,nnew,iDet2)=min(t(i,j,k,nnew,iDet2),0.) +LDet(k)
# ifdef VAR_CHL_C
            t(i,j,k,nnew,iChl1)=min(t(i,j,k,nnew,iChl1),0.)
     &                          + CN_Phyt*12.*Phyt_1(k)*theta1(i,j,k)
            t(i,j,k,nnew,iChl2)=min(t(i,j,k,nnew,iChl2),0.)
     &                          + CN_Phyt*12.*Phyt_2(k)*theta2(i,j,k)
# endif
          enddo
# ifdef MASKING
        endif  ! rmask if
# endif /* MASKING */
        enddo
      enddo

#else /* BIOLOGY */
      subroutine biology_empty ()
#endif /* BIOLOGY */
      return
      end

