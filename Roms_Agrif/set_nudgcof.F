!
! $Id: set_nudgcof.F,v 1.12 2005/10/26 15:18:52 pmarches Exp $
!
#include "cppdefs.h"
#if defined ZNUDGING || defined M2NUDGING || defined M3NUDGING\
    || defined TNUDGING || defined SPONGE

      subroutine set_nudgcof (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call set_nudgcof_tile (Istr,Iend,Jstr,Jend,A2d(1,1,trd))
      return
      end
!
      subroutine set_nudgcof_tile (Istr,Iend,Jstr,Jend, wrk)
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
# include "ocean3d.h"
      integer Istr,Iend,Jstr,Jend, i, j, k, isp, itrc, ibnd
      real visc2_sponge, visc4_sponge, diff2_sponge, diff4_sponge
      real wrk(PRIVATE_2D_SCRATCH_ARRAY)
# if defined CANARY  || defined IBERIA
      real lon0,lat0, rsponge, dx,dy,dr,cff,cff1,cff2,
# endif

# undef SPONGE_GRID
! if SPONGE_GRID, vsponge is defined as :
!       A = 1/2 * uscale * grd_scale
!  and  B = A * grd_scale**2
# ifdef SPONGE_GRID
      real uscale
      parameter (uscale=0.1)
# endif
!
# include "compute_extended_bounds.h"
!
!--------------------------------------------------------------------
!  Set up nudging towards climatology time-scale coeffcients [1/s]
!  and/or increase horizontal mixing in the sponge areas.
!--------------------------------------------------------------------
!
# ifdef MPI
      if (ii.eq.0.and.jj.eq.0.and.Istr.eq.1.and.Jstr.eq.1) then
        isp=int(x_sponge*pm(1,1))
      endif      
      call MPI_Bcast(isp, 1, MPI_INTEGER,
     &                    0, MPI_COMM_WORLD, ierr)      
# else
      isp=int(x_sponge*pm(1,1)) ! number of points in layer     
# endif
!
      do j=max(-1,JstrR-1),JendR
        do i=max(-1,IstrR-1),IendR
          ibnd=isp
# ifndef MPI
#  ifdef OBC_WEST
          ibnd=min(ibnd,i)
#  endif
#  ifdef OBC_EAST
          ibnd=min(ibnd,Lm+1-i)
#  endif
#  ifdef OBC_SOUTH
          ibnd=min(ibnd,j)
#  endif
#  ifdef OBC_NORTH
          ibnd=min(ibnd,Mm+1-j)
#  endif
# else
#  ifdef OBC_WEST
          ibnd=min(ibnd,i+ii*Lm)
#  endif
#  ifdef OBC_EAST
          ibnd=min(ibnd,LLm+1-i-ii*Lm)
#  endif
#  ifdef OBC_SOUTH
          ibnd=min(ibnd,j+jj*Mm)
#  endif
#  ifdef OBC_NORTH
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
# ifdef TNUDGING
          Tnudgcof(i,j,N,itemp)=tauT_out*wrk(i,j)
# endif
# ifdef ZNUDGING
          Znudgcof(i,j)=tauM_out*wrk(i,j)
# endif
# ifdef M2NUDGING
          M2nudgcof(i,j)=tauM_out*wrk(i,j)
# endif
# ifdef M3NUDGING
          M3nudgcof(i,j)=tauM_out*wrk(i,j)
# endif
        enddo
      enddo
!
!  Apply nudging to the other tracers and the other vertical levels
!
# ifdef TNUDGING
      do itrc=1,NT               ! includes BIOLOGY variables
        do k=1,N
          do j=JstrR,JendR
            do i=IstrR,IendR
              Tnudgcof(i,j,k,itrc)=Tnudgcof(i,j,N,itemp)
            enddo
          enddo
        enddo
      enddo
#  ifdef ROBUST_DIAG
      do itrc=1,2               ! nudging everywher
        do k=1,N
          do j=JstrR,JendR
            do i=IstrR,IendR
              Tnudgcof(i,j,k,itrc)=tauT_out
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
          visc2_sponge=.5*uscale*grdmax
#   else
          visc2_sponge=v_sponge
#   endif
#   if defined UV_VIS2 || defined SPONGE_VIS2
          visc2_r(i,j)=visc2_sponge*wrk(i,j) + visc2_r(i,j)
#   elif defined UV_VIS4
          visc4_sponge=5*visc2_sponge/(pm(i,j)*pn(i,j))
          visc4_r(i,j)=visc4_sponge*wrk(i,j) + visc4_r(i,j)
#   endif
        enddo
      enddo
!
!  Interpolate Viscosity at psi points
!
      do j=Jstr,JendR
        do i=Istr,IendR
#   ifdef SPONGE_GRID
          visc2_sponge=.5*uscale*grdmax
#   else
          visc2_sponge=v_sponge
#   endif
#   if defined UV_VIS2 || defined SPONGE_VIS2
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
            diff2_sponge=.5*uscale*grdmax
#   else
            diff2_sponge=v_sponge
#   endif
#   if defined TS_DIF2 || defined SPONGE_DIF2
            diff2(i,j,itrc)=diff2_sponge*wrk(i,j)+diff2(i,j,itrc)
#   elif defined TS_DIF4
            diff4_sponge=diff2_sponge/(pm(i,j)*pn(i,j))
            diff4(i,j,itrc)=diff4_sponge*wrk(i,j)+diff4(i,j,itrc)
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
# if defined CANARY || defined IBERIA
      lon0=-8.2
      lat0=35.7
      rsponge=300.e3
      cff=1./(50.*86400.)
      cff1=Eradius*cos(lat0*deg2rad)*deg2rad
      cff2=Eradius*deg2rad
      do j=JstrR,JendR
        do i=IstrR,IendR
          dx=cff1*(lonr(i,j)-lon0)
          dy=cff2*(latr(i,j)-lat0)
          dr=sqrt(dx**2+dy**2)
          if (dr .lt. rsponge) then
            do k=1,N
              Tnudgcof(i,j,k,itemp)=.5*cff*(cos(pi*dr/rsponge)+1)
     &                    *(-atan((z_r(i,j,k)+750.)*2.e-2)/pi+.5)
              Tnudgcof(i,j,k,isalt)=Tnudgcof(i,j,k,itemp)
            enddo
          endif
        enddo
      enddo
# endif
#else
      subroutine set_nudgcof_empty
#endif /* TNUDGING || ZNUDGING || SPONGE */
      return
      end
