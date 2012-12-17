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

      subroutine rho_eos
!
!--------------------------------------------------------------------
! Compute density anomaly via Equation Of State (EOS) for seawater.
! If so prescribed, non-linear EOS of Jackett and McDougall (1995)
! is used.
!
! Tt potential temperature [deg Celsius].
! Ts salinity [PSU].
! Tz pressure/depth, [depth in meters and negative].
!
! K0, K1 and K2 are the pressure polynomial coefficients for secant
! bulk modulus, so that
!
!               bulk = K0 - K1 * z + K2 * z**2 ;
!
! while rho1 is sea-water density [kg/m^3] at standard pressure
! of 1 Atm, so that the density anomaly at in-sity pressure is
!
!               rho = rho1 / (1 + z / bulk) - 1000
!
! If so prescribed, it also computes the Brunt-Vaisala frequency
! [1/s^2] at horizontal RHO-points and vertical W-points,
!
!                   bvf = - g/rho0 d(rho)/d(z).
!
!  In computation of bvf the density anomaly difference is computed
!  by adiabatically lowering/rising the water parcel from RHO point
!  above/below to the W-point depth at "z_w".
!
!  Reference:
!
!  Jackett, D. R. and T. J. McDougall, 1995, Minimal Adjustment of
!  Hydrostatic Profiles to Achieve Static Stability, Journ of Atmos.
!  and Oceanic Techn., vol. 12, pp. 381-389.
!
! << This equation of state formulation has been derived by Jackett
!    and McDougall (1992), unpublished manuscript, CSIRO, Australia.
!    It computes in-situ density anomaly as a function of potential
!    temperature (Celsius) relative to the surface, salinity (PSU),
!    and depth (meters).  It assumes  no  pressure  variation along
!    geopotential  surfaces,  that  is,  depth  and  pressure  are
!    interchangeable. >>
!                                          John Wilkin, 29 July 92
!--------------------------------------------------------------------
!

      implicit none
#include "param.h"
#include "grid.h"
#include "ocean3d.h"
#include "scalars.h"


#ifdef NONLIN_EOS
      real A00, A01, A02, A03, A04, A10, A11, A12, A13, Tt,
     &     AS0, AS1, AS2, B00, B01, B02, B03, B10, B11,   Ts,
     &     B12, BS1, E00, E01, E02, E10, E11, E12, sqrtTs, cff
      parameter(A00=+19092.56 ,  A01=+209.8925   , A02=-3.041638,
     &         A03=-1.852732e-3, A04=-1.361629e-5, A10=104.4077  ,
     &         A11=-6.500517   , A12=+0.1553190  , A13=2.326469e-4,
     &         AS0=-5.587545   , AS1=+0.7390729  , AS2=-1.909078e-2,
     &         B00=+4.721788e-1, B01=+1.028859e-2, B02=-2.512549e-4,
     &         B03=-5.939910e-7, B10=-1.571896e-2, B11=-2.598241e-4,
     &         B12=+7.267926e-6, BS1=+2.042967e-3,
     &         E00=+1.045941e-5, E01=-5.782165e-10,E02=+1.296821e-7,
     &         E10=-2.595994e-7, E11=-1.248266e-9, E12=-3.508914e-9)
      real QR, Q01, Q02, Q03, Q04, Q05, Q10, Q11,  Q12, Q13, Q14,
     &                                       QS0,  QS1, QS2, Q20
      parameter(QR=+999.842594 , Q01=+6.793952e-2, Q02=-9.095290e-3,
     &         Q03=+1.001685e-4, Q04=-1.120083e-6, Q05=+6.536332e-9,
     &         Q10=+0.824493   , Q11=-4.08990e-3 , Q12=+7.64380e-5,
     &         Q13=-8.24670e-7 , Q14=+5.38750e-9 , QS0=-5.72466e-3,
     &         QS1=+1.02270e-4 , QS2=-1.65460e-6 , Q20=+4.8314e-4)
      integer k
      real K0(N),K1(N),K2(N)
!
      cff=g/rho0
      do k=1,N
        Tt=t(k,nstp,itemp)
# ifdef SALINITY
        Ts=t(k,nstp,isalt)
        sqrtTs=sqrt(Ts)
# else
        Ts=0.
        sqrtTs=0.
# endif
        K0(k)=A00+Tt*(A01+Tt*(A02+Tt*(A03+Tt*A04)))
     &           +Ts*( A10+Tt*(A11+Tt*(A12+Tt*A13))+sqrtTs*(
     &                                      AS0+Tt*(AS1+Tt*AS2) ))

        K1(k)=B00+Tt*(B01+Tt*(B02+Tt*B03))+Ts*( B10+Tt*(B11+Tt*B12)
     &                                                  +sqrtTs*BS1)
        K2(k)=E00+Tt*(E01+Tt*E02)+Ts*(E10+Tt*(E11+Tt*E12))

        rho1(k)=QR+Tt*(Q01+Tt*(Q02+Tt*(Q03+Tt*(Q04+Tt*Q05))))
     &            +Ts*(Q10+Tt*(Q11+Tt*(Q12+Tt*(Q13+Tt*Q14)))
     &                  +sqrtTs*(QS0+Tt*(QS1+Tt*QS2))+Ts*Q20)

        rhop0(k)=rho1(k)-1000.
      enddo
!
!  Compute the Brunt-Vaisala frequency (bvf).
!
      cff=g/rho0
      do k=1,N-1
        bvf(k)=-cff*( rho1(k+1)/( 1.+0.1*z_w(k)/(
     &                   K0(k+1)-z_w(k)*(K1(k+1)-z_w(k)*K2(k+1))
     &                                                         ))
     &               -rho1(k)/( 1.+0.1*z_w(k)/(
     &                          K0(k)-z_w(k)*(K1(k)-z_w(k)*K2(k))
     &                                                         ))
     &                    )/(z_r(k+1)-z_r(k))
      enddo
#else
      real cff
      integer k
      do k=1,N
        rhop0(k)=R0+Tcoef*t(k,nstp,itemp)
#  ifdef SALINITY
     &             +Scoef*t(k,nstp,isalt)
#  endif
      enddo
      cff=g/rho0
      do k=1,N-1
        bvf(k)=-cff*(rhop0(k+1)-rhop0(k))/(z_r(k+1)-z_r(k))
      enddo
#endif
      return
      end




