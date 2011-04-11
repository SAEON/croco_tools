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
#if defined VIS_GRID || defined DIF_GRID
      subroutine visc_rescale (tile)
      implicit none
      integer tile
# include "param.h"
# include "compute_tile_bounds.h"
      call visc_rescale_tile (Istr,Iend,Jstr,Jend)
      return
      end
!
      subroutine visc_rescale_tile (Istr,Iend,Jstr,Jend)
!
! Rescale variable horizontal viscosity and mixing coefficients
! according to the local grid box size. The maximum grid size
! "grdmax" is determined in metrics and it is assumed known in
! this subroutine.
!

      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,itrc
      real cff
# include "param.h"
# include "grid.h"
# include "mixing.h"
# include "scalars.h"
!
# include "compute_auxiliary_bounds.h"
!
# ifdef VIS_GRID
#  ifdef UV_VIS2
      cff=visc2/grdmax
      do j=JstrR,JendR
        do i=IstrR,IendR
          visc2_r(i,j)=cff*grdscl(i,j)
        enddo
      enddo
      cff=visc2*0.25/grdmax
      do j=Jstr,JendR
        do i=Istr,IendR
          visc2_p(i,j)=cff*(grdscl(i,j  )+grdscl(i-1,j  )+
     &                      grdscl(i,j-1)+grdscl(i-1,j-1))
        enddo
      enddo
#  endif
#  ifdef UV_VIS4
      cff=visc4/(grdmax**3)
      do j=JstrR,JendR
        do i=IstrR,IendR
          visc4_r(i,j)=cff*grdscl(i,j)**3
        enddo
      enddo
      cff=visc4*0.25/(grdmax**3)
      do j=Jstr,JendR
        do i=Istr,IendR
          visc4_p(i,j)=cff*(grdscl(i,j  )**3+grdscl(i-1,j  )**3+
     &                      grdscl(i,j-1)**3+grdscl(i-1,j-1)**3)
        enddo
      enddo
#  endif
# endif
# ifdef DIF_GRID
#  ifdef TS_DIF2
      do itrc=1,NT
        cff=tnu2(itrc)/grdmax
        do j=JstrR,JendR
          do i=IstrR,IendR
            diff2(i,j,itrc)=cff*grdscl(i,j)
          enddo
        enddo
      enddo
#  endif
#  ifdef TS_DIF4
      do itrc=1,NT
        cff=tnu4(itrc)/(grdmax**3)
        do j=JstrR,JendR
          do i=IstrR,IendR
            diff4(i,j,itrc)=cff*grdscl(i,j)**3
          enddo
        enddo
      enddo
#  endif
# endif
#else
      subroutine visc_rescale_empty
#endif
      return
      end
