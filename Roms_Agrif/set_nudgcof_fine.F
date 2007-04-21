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
#if defined MPI
      include 'mpif.h'
#endif
# include "param.h"
# include "grid.h"
# include "climat.h"
# include "mixing.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i, j, k, isp, itrc, ibnd
      real wrk(PRIVATE_2D_SCRATCH_ARRAY)
!
# include "compute_extended_bounds.h"
!
!--------------------------------------------------------------------
!  Set up nudging towards climatology time-scale coeffcients [1/s]
!  and/or
!  increase horizontal mixing in the sponge areas.
!--------------------------------------------------------------------
!  

#ifdef SMAGORINSKY
      v_sponge = 0.
#endif

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
# ifdef SPONGE
#  ifdef UV_VIS2
          visc2_r(i,j)=v_sponge*wrk(i,j) + visc2
#  endif
#  ifdef TS_DIF2
          diff2(i,j,itemp)=v_sponge*wrk(i,j)
#  endif
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
!
!  viscosity at psi points
!
# ifdef SPONGE
#  ifdef UV_VIS2
      do j=Jstr,JendR
        do i=Istr,IendR
          visc2_p(i,j)=0.25*v_sponge*( wrk(i,j  )+wrk(i-1,j  )
     &                                +wrk(i,j-1)+wrk(i-1,j-1))
     &               + visc2
        enddo
      enddo
#  endif
!
!  diffusivity for the other tracers
!
#  if defined TS_DIF2
      do itrc=1,NT
        do j=JstrR,JendR
          do i=IstrR,IendR
            diff2(i,j,itrc)=diff2(i,j,itemp)+tnu2(itrc)
          enddo
        enddo
      enddo
#  endif
# endif
!
!  add configuration specific stuff
!
#else
      subroutine set_nudgcof_fine_empty
#endif /* AGRIF && (TNUDGING || ZNUDGING || SPONGE) */
      return
      end
