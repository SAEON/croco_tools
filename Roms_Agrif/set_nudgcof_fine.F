!
! $Id: set_nudgcof_fine.F,v 1.9 2005/10/26 15:18:52 pmarches Exp $
!
#include "cppdefs.h"
#if defined AGRIF && (defined TNUDGING || defined ZNUDGING || defined SPONGE)

      subroutine set_nudgcof_fine (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call set_nudgcof_fine_tile (Istr,Iend,Jstr,Jend,A2d(1,1,trd))
      return
      end
!
      subroutine set_nudgcof_fine_tile (Istr,Iend,Jstr,Jend, wrk)
!     implicit none
      integer ierr
# if defined MPI
      include 'mpif.h'
# endif
# include "param.h"
# include "grid.h"
# include "climat.h"
# include "mixing.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i, j, k, isp, itrc, ibnd
      real visc2_sponge, visc4_sponge, diff2_sponge, diff4_sponge
      real wrk(PRIVATE_2D_SCRATCH_ARRAY)
# ifdef SPONGE_GRID
      real uscale
      parameter (uscale=0.1)
# endif
!
# include "compute_extended_bounds.h"
!
# undef SPONGE_GRID
!                    if SPONGE_GRID, vsponge is redefined as:
!                    5 times an interior visc2 given by :
!                    A = 1/12 * uscale * grd_scale
!                    B = 1/12 * uscale * grd_scale**3
!
!--------------------------------------------------------------------
!  Set up nudging towards climatology time-scale coeffcients [1/s]
!  and/or increase horizontal mixing in the sponge areas.
!--------------------------------------------------------------------
!
!  
      isp = 2*max(Agrif_Irhox(),Agrif_Irhoy())  
!
      do j=max(-1,JstrR-1),JendR
        do i=max(-1,IstrR-1),IendR
          ibnd=isp
# ifndef MPI
#  ifdef AGRIF_OBC_WEST
          ibnd=min(ibnd,i)
#  endif
#  ifdef AGRIF_OBC_EAST
          ibnd=min(ibnd,Lm+1-i)
#  endif
#  ifdef AGRIF_OBC_SOUTH
          ibnd=min(ibnd,j)
#  endif
#  ifdef AGRIF_OBC_NORTH
          ibnd=min(ibnd,Mm+1-j)
#  endif
# else
#  ifdef AGRIF_OBC_WEST
          ibnd=min(ibnd,i+ii*Lm)
#  endif
#  ifdef AGRIF_OBC_EAST
          ibnd=min(ibnd,LLm+1-i-ii*Lm)
#  endif
#  ifdef AGRIF_OBC_SOUTH
          ibnd=min(ibnd,j+jj*Mm)
#  endif
#  ifdef AGRIF_OBC_NORTH
          ibnd=min(ibnd,MMm+1-j-jj*Mm)
#  endif
# endif
!
          wrk(i,j)=.5*(cos(pi*float(ibnd)/float(isp))+1.)! cosine profile
!          wrk(i,j)=float(isp-ibnd)/float(isp)            ! linear profile
        enddo
      enddo

!
!-------------------------------------------------------------------
! Compute nudging coefficients in nudging layers
!-------------------------------------------------------------------
!
      do j=JstrR,JendR
        do i=IstrR,IendR
# ifdef ZNUDGING
          Znudgcof(i,j)=0.
# endif
# ifdef M2NUDGING
          M2nudgcof(i,j)=0.
# endif
# ifdef M3NUDGING
          M3nudgcof(i,j)=0.
# endif
        enddo
      enddo
!
!  Apply nudging to the tracers 
!
# ifdef TNUDGING
      do itrc=1,NT               ! includes BIOLOGY variables
        do k=1,N
          do j=JstrR,JendR
            do i=IstrR,IendR
              Tnudgcof(i,j,k,itrc)=0.
            enddo
          enddo
        enddo
      enddo
#  ifdef ROBUST_DIAG  
      do itrc=1,2               ! nudging everywhere
        do k=1,N
          do j=JstrR,JendR
            do i=IstrR,IendR
              Tnudgcof(i,j,k,itrc)=1./(360.*86400.)
            enddo
          enddo
        enddo
      enddo
#  endif
# endif

# ifdef SPONGE
!
!-------------------------------------------------------------------
! Add Viscosity and Diffusivity in SPONGE layers
!-------------------------------------------------------------------
!
#  if defined UV_VIS2 || defined UV_VIS4
!
! Add Viscosity at rho points 
!
      do j=JstrR,JendR
        do i=IstrR,IendR
#   ifdef SPONGE_GRID
          visc2_sponge=5*uscale*grdmax/12.
#   else
          visc2_sponge=v_sponge
#   endif
#   ifdef UV_VIS2
          visc2_r(i,j)=visc2_sponge*wrk(i,j) + visc2_r(i,j)
#   elif defined UV_VIS4
          visc4_sponge=visc2_sponge/(pm(i,j)*pn(i,j))
          visc4_r(i,j)=visc4_sponge*wrk(i,j) + visc4_r(i,j)
          visc4_r(i,j)=min(visc4_r(i,j),1.e-2*grdmax**3)
#   endif
        enddo
      enddo
!
!  Interpolate Viscosity at psi points
!
      do j=Jstr,JendR
        do i=Istr,IendR
#   ifdef SPONGE_GRID
          visc2_sponge=5*uscale*grdmax/12.
#   else
          visc2_sponge=v_sponge
#   endif
#   ifdef UV_VIS2
          visc2_p(i,j)=0.25*visc2_sponge*( wrk(i,j  )+wrk(i-1,j  )
     &                                    +wrk(i,j-1)+wrk(i-1,j-1))
     &                                               + visc2_p(i,j)
#   elif defined UV_VIS4
          visc4_sponge=visc2_sponge/(pm(i,j)*pn(i,j))
          visc4_p(i,j)=0.25*visc4_sponge*( wrk(i,j  )+wrk(i-1,j  )
     &                                    +wrk(i,j-1)+wrk(i-1,j-1))
     &                                               + visc4_p(i,j)
#   endif
        enddo
      enddo
#  endif /* UV_VIS2 || UV_VIS4 */

#  if defined TS_DIF2 || defined TS_DIF4
!
!  Add Diffusivity for all tracers
!
      do itrc=1,NT
        do j=JstrR,JendR
          do i=IstrR,IendR
#   ifdef SPONGE_GRID
            diff2_sponge=5*uscale*grdmax/12.
#   else
            diff2_sponge=v_sponge
#   endif
#   ifdef TS_DIF2
            diff2(i,j,itrc)=diff2_sponge*wrk(i,j)+diff2(i,j,itrc)
#   elif defined TS_DIF4
            diff4_sponge=diff2_sponge/(pm(i,j)*pn(i,j))
            diff4(i,j,itrc)=diff4_sponge*wrk(i,j)+diff4(i,j,itrc)
            diff4(i,j,itrc)=min(diff4(i,j,itrc),1.e-2*grdmax**3)
#   endif
          enddo
        enddo
      enddo
#  endif /* TS_DIF2 || TS_DIF4 */
# endif /* SPONGE */

!
!-------------------------------------------------------------------
!  add configuration specific stuff
!-------------------------------------------------------------------
!

#else
      subroutine set_nudgcof_fine_empty
#endif /* AGRIF && (TNUDGING || ZNUDGING || SPONGE) */
      return
      end
