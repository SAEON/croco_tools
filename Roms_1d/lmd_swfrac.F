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

      subroutine lmd_swfrac (Zscale,Z, swdk)
!
! Compute fraction of solar shortwave flux penetrating to specified
! depth (times Zscale) due to exponential decay in Jerlov water type.
!
! Input:  Zscale   scale factor to apply to depth array
!         Z        vertical height [meters, negative] for
!                        desired solar short-wave fraction.
! Output: swdk     shortwave (radiation) fractional decay.
!
! Reference:
!
! Paulson, C.A., and J.J. Simpson, 1977: Irradiance meassurements
! in the upper ocean, J. Phys. Oceanogr., 7, 952-956.
!
! This routine was adapted from Bill Large 1995 code.
!
      implicit none
      integer Jwtp
      real Zscale,Z, swdk, r1(5), mu1(5),mu2(5)
# include "param.h"
# include "scalars.h"

      r1(1)=0.58     !  Define fraction of the total radiance
      r1(2)=0.62     !  for wavelength band 1 as a function of
      r1(3)=0.67     !  Jerlov water type. The fraction for
      r1(4)=0.77     !  wavelength band 2 is r2=1-r1.
      r1(5)=0.78

      mu1(1)=0.35    !  Set reciprocal of the absorption
      mu1(2)=0.6     !  coefficient for each of two solar
      mu1(3)=1.      !  wavelength bands as a function 
      mu1(4)=1.5     !  of water type (Ref: Paulson and
      mu1(5)=1.4     !  Simpson, 1977).

      mu2(1)=23.
      mu2(2)=20.
      mu2(3)=17.
      mu2(4)=14.
      mu2(5)=7.9
                     !  Set Jerlov water type
      Jwtp=1         !  (assign an integer from 1 to 5.
! 
! Use Paulson and Simpson (1977) two wavelength bands solar
! absorption model.
!
      swdk=r1(Jwtp)*exp(Z*Zscale/mu1(Jwtp))+(1.-r1(Jwtp))*
     &                              exp(Z*Zscale/mu2(Jwtp))
#else
      subroutine lmd_swfrac
#endif /* LMD_KPP */
      return
      end

