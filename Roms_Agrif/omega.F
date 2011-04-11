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

      subroutine omega (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call omega_tile (Istr,Iend,Jstr,Jend, A2d(1,1,trd))
      return
      end

      subroutine omega_tile (Istr,Iend,Jstr,Jend, wrk)
!
! Compute S-coordinate vertical velocity, w=[Hz/(m*n)]*omega [m^3/s],
! which has the meaning of FINITE_VOLUME WATER FLUX through MOVING
! grid-box interfaces of RHO-boxes. 
! 
! To do so, integrate divergence of horizontal mass fluxes from
! bottom up, starting with the no-normal flow boundary condition at 
! the bottom (k=0); Once this operation is done, W(:,:,N) contains
! the vertical velocity at the free surface, which is, in its turn,
! the temporal tendency of the free surface, d_zeta/d_t;
! In order to compute the S-coordinate vertical velocity, one
! needs to subtract the vertical velocities of moving S-coordinate
! isosurfaces, which are proportional the product of d_zeta/d_t
! and the fraction of the distance from the point to the bottom 
! divided by the total depth of water column, i.e. the whole
! S-coordinate system is "brethes" by linear in Z-space expansion
! and contraction set by free-surface variation.
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k
# include "param.h"
      real wrk(PRIVATE_1D_SCRATCH_ARRAY)
# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
!

#ifdef MPI
#define LOCALLM Lmmpi
#define LOCALMM Mmmpi
#else
#define LOCALLM Lm
#define LOCALMM Mm
#endif

# include "compute_auxiliary_bounds.h"
!

      do j=Jstr,Jend         !!! WARNING: Setting old-new must be
        do i=Istr,Iend       !!!          consistent with similar  
          W(i,j,0)=0.        !!!          setting in set_depth.F  
        enddo
        do k=1,N,+1
          do i=Istr,Iend
            W(i,j,k)=W(i,j,k-1) -Huon(i+1,j,k)+Huon(i,j,k)
     &                          -Hvom(i,j+1,k)+Hvom(i,j,k)
          enddo
        enddo

        do i=Istr,Iend                                        !< new
          wrk(i)=W(i,j,N)/(z_w(i,j,N)-z_w(i,j,0))             !< new
        enddo                                                 !< new

        do k=N-1,1,-1
          do i=Istr,Iend
            W(i,j,k)=W(i,j,k)-wrk(i)*(z_w(i,j,k)-z_w(i,j,0))  !< new
          enddo
        enddo
        do i=Istr,Iend
          W(i,j,N)=0.
        enddo
      enddo
!
!  Set lateral boundary conditions.
!
# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
        do k=0,N
          do j=Jstr,Jend
            W(0,j,k)=W(1,j,k)
          enddo
        enddo
      endif
      if (EASTERN_EDGE) then
        do k=0,N
          do j=Jstr,Jend
            W(LOCALLM+1,j,k)=W(LOCALLM,j,k)
          enddo
        enddo
      endif
# endif
# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        do k=0,N
          do i=Istr,Iend
            W(i,0,k)=W(i,1,k)
          enddo
        enddo
      endif
      if (NORTHERN_EDGE) then
        do k=0,N
          do i=Istr,Iend
            W(i,LOCALMM+1,k)=W(i,LOCALMM,k)
          enddo
        enddo
      endif
#  ifndef EW_PERIODIC
      if (WESTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=0,N
          W(0,0,k)=W(1,1,k)
        enddo
      endif
      if (WESTERN_EDGE .and. NORTHERN_EDGE) then
        do k=0,N
          W(0,LOCALMM+1,k)=W(1,LOCALMM,k)
        enddo
      endif
      if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
        do k=0,N
          W(LOCALLM+1,0,k)=W(LOCALLM,1,k)
        enddo
      endif
      if (EASTERN_EDGE .and. NORTHERN_EDGE) then
        do k=0,N
          W(LOCALLM+1,LOCALMM+1,k)=W(LOCALLM,LOCALMM,k)
        enddo
      endif
#  endif
# endif

# if defined EW_PERIODIC || defined NS_PERIODIC  || defined MPI
      call exchange_w3d_tile (Istr,Iend,Jstr,Jend, W(START_2D_ARRAY,0))
# endif
      return
      end
#else
      subroutine omega_empty
      return
      end
#endif /* SOLVE3D */
