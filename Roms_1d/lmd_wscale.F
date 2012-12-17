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

      subroutine lmd_wscale (Bfsfc,zscale,ustar, wm,ws)
!
! Computes turbulent velocity scale for momentum and tracer
! using a 2D-lookup table as a function of "ustar" and "zetahat".
!
! Input:  Bfsfc
!         zscale   boundary layer depth [m].
!         ustar
!
! Output: wm,ws  turbulent velocity scales [m/s] at zscale
!                for momentum and tracer fields respectively.
!
! This routine was adapted from Bill Large 1995 code.
!
      implicit none
      real Bfsfc,zscale,ustar, wm,ws
# include "param.h"
# include "grid.h"
# include "mixing.h"
      real ustar3, zetahat, zetapar

      real zetam,zetas, am,cm, as,cs, eps, r2, r3, r4 
      parameter (
     &    zetam=-0.2,     ! Maximum stability parameters "zeta"
     &    zetas=-1.0,     ! value of the 1/3 power law regime of 
                          ! flux profile for momentum and tracers
     &    am=1.257,
     &    as=-28.86,      ! Coefficients of flux profile  
     &    cm=8.360,       ! for momentum and tracers in their 
     &    cs=98.96,       ! 1/3 power law regime;

     &    eps=1.E-20,
     &    r2=0.5, r3=1./3., r4=0.25)

      ustar3=ustar**3
      zetahat=vonKar*zscale*Bfsfc
      zetapar=zetahat/(ustar3+eps)

      if (zetahat.ge.0.) then
!
! Stable regime.
!
        wm=vonKar*ustar/(1.+5.*zetapar)
        ws=wm
      else
!
! Unstable regime.
!
        if (zetapar.gt.zetam) then
          wm=vonKar*ustar*(1.-16.*zetapar)**r4
        else
          wm=vonKar*(am*ustar3-cm*zetahat)**r3
        endif
        if (zetapar.gt.zetas) then
          ws=vonKar*ustar*(1.-16.*zetapar)**r2
        else
          ws=vonKar*(as*ustar3-cs*zetahat)**r3
        endif
      endif
#else
      subroutine lmd_wscale_empty
#endif /* LMD_KPP */
      return
      end
