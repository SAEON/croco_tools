!
! $Id: set_nudgcof.F,v 1.12 2005/10/26 15:18:52 pmarches Exp $
!
#include "cppdefs.h"
#if defined TNUDGING || defined ZNUDGING || defined SPONGE

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
      real wrk(PRIVATE_2D_SCRATCH_ARRAY)
# if defined CANARY  || defined IBERIA
      real lon0,lat0, rsponge, dx,dy,dr,cff,cff1,cff2
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
     &                + visc2
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
#  if defined CANARY || defined IBERIA
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
#  endif
#else
      subroutine set_nudgcof_empty
#endif /* TNUDGING || ZNUDGING || SPONGE */
      return
      end
