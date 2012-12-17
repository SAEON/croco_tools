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

      subroutine setup_grid
      implicit none
      integer k
      real cff,sc_w,sc_r

#include "param.h"
#include "grid.h"

      cff=hmax/sinh(theta_s)
      do k=N,1,-1
        sc_w=ds*float(k-N)
        z_w(k)=-cff*sinh(-theta_s*sc_w)
        sc_r=ds*(float(k-N)-0.5)
        z_r(k)=-cff*sinh(-theta_s*sc_r)
      enddo
      z_w(0)=-hmax
      do k=1,N
        Hz(k)=ods*(z_w(k)-z_w(k-1))
      enddo

      return
      end






