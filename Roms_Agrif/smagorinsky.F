!
! $Id: smagorinsky.F,v 1.4 2005/10/07 15:32:40 pmarches Exp $
!
#include "cppdefs.h"
#if defined UV_VIS2 && defined SMAGORINSKY

      subroutine smagorinsky (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call smagorinsky_tile (Istr,Iend,Jstr,Jend)
      return
      end
!
      subroutine smagorinsky_tile (Istr,Iend,Jstr,Jend)
      implicit none
      integer ierr
# include "param.h"
# include "grid.h"
# include "mixing.h"
# include "ocean3d.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i, j, k
!
!---------------------------------------------------------------
!  Smagorinsky diffusivity coefficient
!
!  The POM formulation is used:
!    A = C*DX*DY*0.5*sqrt[du/dx^2 +dvdy^2 + 0.5(dvdx+dudy)^2]
!---------------------------------------------------------------
!
      real horcon
      parameter(horcon=0.1) 
!
# include "compute_auxiliary_bounds.h"
!
!  Viscosity at RHO points
!
!
      do k=1,N
        do j=JstrV-1,Jend
          do i=IstrU-1,Iend
            visc3d_r(i,j,k)=(horcon/(pm(i,j)*pn(i,j)))
     &         *sqrt( ((u(i+1,j,k,nrhs)-u(i,j,k,nrhs))*pm(i,j))**2
     &               +((v(i,j+1,k,nrhs)-v(i,j,k,nrhs))*pn(i,j))**2
     &               +0.5*(0.25*pn(i,j)*(
     &                     u(i,j+1,k,nrhs)+u(i+1,j+1,k,nrhs)
     &                    -u(i,j-1,k,nrhs)-u(i+1,j-1,k,nrhs))
     &                   + 0.25*pm(i,j)*(
     &                     v(i+1,j,k,nrhs)+v(i+1,j+1,k,nrhs)
     &                    -v(i-1,j,k,nrhs)-v(i-1,j+1,k,nrhs)) )**2)
          enddo
        enddo
      enddo
!
! Boundary conditions
!
      if (WESTERN_EDGE) then
        do k=1,N
          do j=JstrV-1,Jend
            visc3d_r(Istr-1,j,k)=visc3d_r(Istr,j,k)
          enddo
        enddo
      endif
      if (EASTERN_EDGE) then
        do k=1,N
          do j=JstrV-1,Jend
            visc3d_r(Iend+1,j,k)=visc3d_r(Iend,j,k)
          enddo
        enddo
      endif
      if (SOUTHERN_EDGE) then
        do k=1,N
          do i=IstrU-1,Iend
            visc3d_r(i,Jstr-1,k)=visc3d_r(i,Jstr,k)
          enddo
        enddo
      endif
      if (NORTHERN_EDGE) then
        do k=1,N
          do i=IstrU-1,Iend
            visc3d_r(i,Jend+1,k)=visc3d_r(i,Jend,k)
          enddo
        enddo
      endif
!
! Corners
!
      if (SOUTHERN_EDGE .and. WESTERN_EDGE) then
        do k=1,N
          visc3d_r(Istr-1,Jstr-1,k)=0.5*
     &                          ( visc3d_r(Istr,Jstr-1,k)
     &                           +visc3d_r(Istr-1,Jstr,k))
        enddo
      endif
      if (SOUTHERN_EDGE .and. EASTERN_EDGE) then
        do k=1,N
          visc3d_r(Iend+1,Jstr-1,k)=0.5*
     &                          (visc3d_r(Iend,Jstr-1,k)
     &                          +visc3d_r(Iend+1,Jstr,k))
        enddo
      endif
      if (NORTHERN_EDGE .and. WESTERN_EDGE) then
        do k=1,N
          visc3d_r(Istr-1,Jend+1,k)=0.5*
     &                          ( visc3d_r(Istr,Jend+1,k)
     &                           +visc3d_r(Istr-1,Jend,k))
        enddo
      endif
      if (NORTHERN_EDGE .and. EASTERN_EDGE) then
        do k=1,N
          visc3d_r(Iend+1,Jend+1,k)=0.5*
     &                          ( visc3d_r(Iend,Jend+1,k)
     &                           +visc3d_r(Iend+1,Jend,k))
        enddo
      endif
!
!  Viscosity at PSI points
!
      do k=1,N
        do j=Jstr,JendR
          do i=Istr,IendR
            visc3d_p(i,j,k)=0.25*
     &                  ( visc3d_r(i,j  ,k)+visc3d_r(i-1,j  ,k)
     &                   +visc3d_r(i,j-1,k)+visc3d_r(i-1,j-1,k))
          enddo
        enddo
      enddo

# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_r3d_tile (Istr,Iend,Jstr,Jend,visc3d_r)
      call exchange_p3d_tile (Istr,Iend,Jstr,Jend,visc3d_p)
# endif
#else
      subroutine smagorinsky_empty
#endif  /* UV_VIS2 && SMAGORINSKY */
      return
      end
