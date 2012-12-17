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
      subroutine alfabeta (alpha,beta)
!
! Compute thermal expansion and saline contraction coefficients as
! function of potential temperature, salinity, and pressure from a
! polynomial expression (Jackett & McDougall, 1992).
! The polynomial expression was found from fitting to 248 values in
! the oceanographic ranges of salinity, potentialtemperature, and
! pressure. It  assumes no pressure variation along geopotential
! surfaces, that is, depth and pressure are interchangeable.
! The coefficients are evaluated at the surface.
!
! Output:
!        alpha    Thermal expansion coefficient [kg/m^3/Celsius].
!        beta     Saline contraction coefficient [kg/m^3/PSU].
!
!  Adapted from original "rati" and "beta" routines.
!  Copyright (c) 1996 Rutgers University
!
      implicit none
      real alpha, beta
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
# ifdef NONLIN_EOS
      real      Q00, Q01, Q02, Q03, Q04, Q05, U00, U01, U02, U03,
     &          U04, V00, V01, V02, W00,    Tt, Ts, sqrtTs, cff
      parameter(Q00=+999.842594 , Q01=+6.793952E-2, Q02=-9.095290E-3,
     &          Q03=+1.001685E-4, Q04=-1.120083E-6, Q05=+6.536332E-9,
     &          U00=+0.824493   , U01=-4.08990E-3 , U02=+7.64380E-5 ,
     &          U03=-8.24670E-7 , U04=+5.38750E-9 , V00=-5.72466E-3 ,
     &          V01=+1.02270E-4 , V02=-1.65460E-6 , W00=+4.8314E-4  )
# endif
!
! Ts salinity [PSU].
! Tt potential temperature [deg Celsius].
! den(Ts,Tt,0) surface density [kg/m^3]
! rho1(Ts,Tt,0)=den(Ts,Tt,0)-1000.
! alpha(Ts,Tt,0)=-d(rho1(Ts,Tt,0))/d(Tt) / den(Ts,Tt,0)
! beta(Ts,Tt,0) = d(rho1(Ts,Tt,0))/d(Ts) / den(Ts,Tt,0)

# ifdef NONLIN_EOS 
      Tt=t(N,nstp,itemp)
#  ifdef SALINITY
      Ts=t(N,nstp,isalt)
      sqrtTs=sqrt(Ts)
#  else
      Ts=0.
      sqrtTs=0.
#  endif
      cff=1./(1000.+rhop0(N))

      alpha=-cff*(Q01+Tt*(2.*Q02+Tt*(3.*Q03+Tt*(4.*Q04+Tt*5.*Q05)))
     &                    +Ts*(U01+Tt*(2.*U02+Tt*(3.*U03+Tt*4.*U04))
     &                                      +sqrtTs*(V01+Tt*2.*V02))
     &                                                            )
#  ifdef SALINITY
      beta= cff*(U00+Tt*(U01+Tt*(U02+Tt*(U03+Tt*U04)))+1.5*(V00+
     &                        Tt*(V01+Tt*V02) )*sqrtTs+2.*W00*Ts)
#  else
      beta=0.
#  endif
# else     
!
!   Linear Equation of state thermal expansion and saline
!   contraction coefficients:
!
      alpha=abs(Tcoef)
#  ifdef SALINITY
      beta=abs(Scoef)
#  else
      beta=0.
#  endif
# endif /* NONLIN_EOS */
#else
      subroutine alfabeta_empty
#endif /* LMD_KPP */
      return
      end
