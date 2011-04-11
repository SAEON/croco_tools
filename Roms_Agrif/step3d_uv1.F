! $Id$
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
#ifdef SOLVE3D
      subroutine step3d_uv1 (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call step3d_uv1_tile (Istr,Iend,Jstr,Jend,  A3d(1,1,trd),
     &                               A3d(1,2,trd), A2d(1,1,trd))
      return
      end

      subroutine step3d_uv1_tile (Istr,Iend,Jstr,Jend, ru,rv,DC)
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k
# include "param.h"
      real ru(PRIVATE_2D_SCRATCH_ARRAY,N),
     &     rv(PRIVATE_2D_SCRATCH_ARRAY,N),
     &     DC(PRIVATE_1D_SCRATCH_ARRAY,0:N),  cff
# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
!
# include "compute_auxiliary_bounds.h"
!
      cff=0.25*dt
      do j=Jstr,Jend
        do i=IstrU,Iend
          DC(i,0)=cff*(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
        enddo
        do k=1,N
          do i=IstrU,Iend
            u(i,j,k,nnew)=u(i,j,k,nnew)+DC(i,0)*ru(i,j,k)
          enddo
        enddo
        if (j.ge.JstrV) then
          do i=Istr,Iend
            DC(i,0)=cff*(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
          enddo
          do k=1,N
            do i=Istr,Iend
              v(i,j,k,nnew)=v(i,j,k,nnew)+DC(i,0)*rv(i,j,k)
            enddo
          enddo
        endif
      enddo 
#else
      subroutine step3d_uv1_empty
#endif
      return
      end
