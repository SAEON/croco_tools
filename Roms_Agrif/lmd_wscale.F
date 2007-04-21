!
! $Id: lmd_wscale.F,v 1.2 2003/12/17 13:56:03 pmarches Exp $
!
#include "cppdefs.h"
#if defined LMD_SKPP || defined LMD_BKPP

      subroutine lmd_wscale_tile (Istr,Iend,Jstr,Jend, Bfsfc,sigma,
     &                                                       wm,ws)
!
!-------------------------------------------------------------------
!  This routine computes the turbulent velocity scale for momentum
!  and tracer using a 2D-lookup table as a function of "ustar" and
!  "zetahat".
!
!  Input:  Bfsfc   
!          sigma   boundary layer depth [m].
!
!  Output: wm   turbulent velocity scale [m/s] at sigma for momentum
!          ws   turbulent velocity scale [m/s] at sigma for tracer.
!
!  This routine was adapted from Bill Large 1995 code.
!--------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "grid.h"
# include "mixing.h"
# include "scalars.h"

      integer Istr,Iend,Jstr,Jend, i,j
      real Bfsfc(PRIVATE_2D_SCRATCH_ARRAY),   eps,
     &     sigma(PRIVATE_2D_SCRATCH_ARRAY),   ustar3,   r2,
     &        wm(PRIVATE_2D_SCRATCH_ARRAY),   zetahat,  r3,
     &        ws(PRIVATE_2D_SCRATCH_ARRAY),   zetapar,  r4
      parameter (eps=1.E-20, r2=0.5, r3=1./3., r4=0.25)

      real lmd_zetam,lmd_zetas, lmd_am,lmd_cm, lmd_as,lmd_cs
      parameter (
     &    lmd_zetam=-0.2, ! Maximum stability parameters "zeta"
     &    lmd_zetas=-1.0, ! value of the 1/3 power law regime of 
                          ! flux profile for momentum and tracers
     &    lmd_am=1.257,
     &    lmd_as=-28.86,  ! Coefficients of flux profile  
     &    lmd_cm=8.360,   ! for momentum and tracers in their 
     &    lmd_cs=98.96)   ! 1/3 power law regime;


      do j=Jstr,Jend
        do i=Istr,Iend
          ustar3=ustar(i,j)*ustar(i,j)*ustar(i,j)
          zetahat=vonKar*sigma(i,j)*Bfsfc(i,j)
          zetapar=zetahat/(ustar3+eps)
# ifdef MASKING
          zetahat=zetahat*rmask(i,j)
          zetapar=zetapar*rmask(i,j)
# endif
!
!  Stable regime.
!
          if (zetahat.ge.0.) then
            wm(i,j)=vonKar*ustar(i,j)/(1.+5.*zetapar)
            ws(i,j)=wm(i,j)
!
!  Unstable regime.
!
          else
            if (zetapar.gt.lmd_zetam) then
              wm(i,j)=vonKar*ustar(i,j)*(1.-16.*zetapar)**r4
            else
              wm(i,j)=vonKar*(lmd_am*ustar3-lmd_cm*zetahat)**r3
            endif
            if (zetapar.gt.lmd_zetas) then
              ws(i,j)=vonKar*ustar(i,j)*(1.-16.*zetapar)**r2
            else
              ws(i,j)=vonKar*(lmd_as*ustar3-lmd_cs*zetahat)**r3
            endif
          endif
        enddo       /* !!! This loop wan not pipelined. */
      enddo
#else
      subroutine lmd_wscale
#endif /* LMD_SKPP || LMD_BKPP */
      return
      end
