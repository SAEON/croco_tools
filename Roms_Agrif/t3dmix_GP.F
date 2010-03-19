!
#include "cppdefs.h"

#ifndef CHILD_SPG
      subroutine t3dmix (tile)
      implicit none
      integer tile, itrc, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
#ifdef AGRIF
      do itrc=1,NT
       if (AGRIF_Root()) then
        call t3dmix_tile (istr,iend,jstr,jend, itrc, A3d(1,1,trd),
     &                                    A2d(1, 1,trd), A2d(1, 2,trd),
     &                      A2d(1,3,trd), A2d(1, 5,trd), A2d(1, 7,trd),
     &                      A2d(1,9,trd), A2d(1,11,trd), A2d(1,13,trd))
       else
        call t3dmix_child_tile(istr,iend,jstr,jend, itrc, A3d(1,1,trd),
     &                                    A2d(1, 1,trd), A2d(1, 2,trd),
     &                      A2d(1,3,trd), A2d(1, 5,trd), A2d(1, 7,trd),
     &                      A2d(1,9,trd), A2d(1,11,trd), A2d(1,13,trd))

       endif
      enddo
      return
      end
#else 
      do itrc=1,NT
       call t3dmix_tile(istr,iend,jstr,jend, itrc, A3d(1,1,trd),
     &                                    A2d(1, 1,trd), A2d(1, 2,trd),
     &                      A2d(1,3,trd), A2d(1, 5,trd), A2d(1, 7,trd),
     &                      A2d(1,9,trd), A2d(1,11,trd), A2d(1,13,trd))
      enddo
       return
       end
#endif   /* AGRIF */
!
!---------------------------------------------------------------------
!*********************************************************************
!---------------------------------------------------------------------
!
!PARENT
! 
      subroutine t3dmix_tile (istr,iend,jstr,jend, itrc, LapT,
     &                    FX,FE, FC,dTdz, dTdx,dTde,dZdx,dZde)
#undef CLIMAT_TS_MIXH_FINE
!
#else
!
! CHILD
!
      subroutine t3dmix_child_tile(Istr,Iend,Jstr,Jend, itrc,LapT,
     &                     FX,FE, FC,dTdz, dTdx,dTde,dZdx,dZde)

#if !defined TS_SPLIT_UP3
#define CLIMAT_TS_MIXH_FINE
#endif
!
#endif /* CHILD_SPG */
!
!---------------------------------------------------------------------
! ******************************Common Code***************************
!---------------------------------------------------------------------
!!
      implicit none
# include "param.h"
      integer istr,iend,jstr,jend, itrc, i,j,k,k1,k2, kmld
# ifdef TS_DIF4
     &                           , imin,imax,jmin,jmax
# endif
      real LapT(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &       FX(PRIVATE_2D_SCRATCH_ARRAY),
     &       FE(PRIVATE_2D_SCRATCH_ARRAY),
     &       FC(PRIVATE_2D_SCRATCH_ARRAY,2),
     &     dTdz(PRIVATE_2D_SCRATCH_ARRAY,2),   cff,
     &     dTdx(PRIVATE_2D_SCRATCH_ARRAY,2),   cff1,
     &     dTde(PRIVATE_2D_SCRATCH_ARRAY,2),   cff2,
     &     dZdx(PRIVATE_2D_SCRATCH_ARRAY,2),   cff3,
     &     dZde(PRIVATE_2D_SCRATCH_ARRAY,2),   cff4
# include "grid.h"
# include "ocean3d.h"
# include "mixing.h"
# ifdef CLIMAT_TS_MIXH
# include "climat.h"
# endif
# include "scalars.h"
# ifdef DIAGNOSTICS_TS
#  include "diagnostics.h"
# endif
!
#ifdef AGRIF
#include "zoom.h"
#endif
!
#ifdef CHILD_SPG
#define TCLM tsponge
#else
#define TCLM tclm
#endif
!
# include "compute_auxiliary_bounds.h"



# ifdef TS_DIF2
!
!-----------------------------------------------------------------------
! Compute horizontal (along geopotential     !!!   WARNING: RECURSIVE
! surfaces) Laplacian diffusion terms.       !!!   BLOCKING SEQUENCE
!-----------------------------------------------------------------------
!
      k2=1            ! vertical   dTdx,dTde(:,:,k2) k+1   rho-points 
      do k=0,N,+1     ! placement    dTdz,FC(:,:,k2) k+1/2   W-points 
        k1=k2         ! of scratch dTdx,dTde(:,:,k1) k     rho-points 
        k2=3-k1       ! variables    dTdz,FC(:,:,k1) k-1/2   W-points

        if (k.lt.N) then
          do j=jstr,jend
            do i=istr,iend+1
              cff=0.5*(pm(i,j)+pm(i-1,j)) 
#    ifdef MASKING
     &                                     * umask(i,j)
#    endif
              dZdx(i,j,k2)=cff*(z_r(i,j,k+1) -z_r(i-1,j,k+1))
              dTdx(i,j,k2)=cff*(t(i,j,k+1,nrhs,itrc)
     &                               -t(i-1,j,k+1,nrhs,itrc)
#  if defined CLIMAT_TS_MIXH || defined CLIMAT_TS_MIXH_FINE
     &               -TCLM(i,j,k+1,itrc)+TCLM(i-1,j,k+1,itrc)
#  endif
     &                                                      )
            enddo
          enddo
          do j=jstr,jend+1
            do i=istr,iend
              cff=0.5*(pn(i,j)+pn(i,j-1)) 
#    ifdef MASKING
     &                                       * vmask(i,j)
#    endif
              dZde(i,j,k2)=cff*(z_r(i,j,k+1) -z_r(i,j-1,k+1))
              dTde(i,j,k2)=cff*(t(i,j,k+1,nrhs,itrc)
     &                               -t(i,j-1,k+1,nrhs,itrc)
#  if defined CLIMAT_TS_MIXH || defined CLIMAT_TS_MIXH_FINE
     &               -TCLM(i,j,k+1,itrc)+TCLM(i,j-1,k+1,itrc)
#  endif
     &                                                      )
            enddo
          enddo
        endif
 
        if (k.eq.0 .or. k.eq.N) then
          do j=jstr-1,jend+1
            do i=istr-1,iend+1
              dTdz(i,j,k2)=0.0     ! Set no-flux bottom or
              FC(i,j,k2)=0.0       ! top boundary conditions.
            enddo
          enddo
        else
          do j=jstr-1,jend+1
            do i=istr-1,iend+1
              dTdz(i,j,k2)=(t(i,j,k+1,nrhs,itrc)-t(i,j,k,nrhs,itrc)
#  if defined CLIMAT_TS_MIXH || defined CLIMAT_TS_MIXH_FINE
     &                          -TCLM(i,j,k+1,itrc)+TCLM(i,j,k,itrc)
#  endif
     &                                   )/(z_r(i,j,k+1)-z_r(i,j,k))
            enddo
          enddo
        endif
 
        if (k.gt.0) then

          do j=jstr,jend
            do i=istr,iend+1
#  ifdef DIF_COEF_3D
              FX(i,j)=0.25*(diff3d(i,j,k)+diff3d(i-1,j,k))
#  else
              FX(i,j)=0.25*(diff2(i,j,itrc)+diff2(i-1,j,itrc))
#  endif
     &                     *(Hz(i,j,k)+Hz(i-1,j,k))*on_u(i,j)*(
     &         dTdx(i  ,j,k1)-0.5*(
     &             min(dZdx(i,j,k1),0.)*(dTdz(i-1,j,k1)+dTdz(i,j,k2))
     &            +max(dZdx(i,j,k1),0.)*(dTdz(i-1,j,k2)+dTdz(i,j,k1))
     &                                                             ))
            enddo
          enddo
          do j=jstr,jend+1
            do i=istr,iend
#  ifdef DIF_COEF_3D
              FE(i,j)=0.25*(diff3d(i,j,k)+diff3d(i,j-1,k))
#  else
              FE(i,j)=0.25*(diff2(i,j,itrc)+diff2(i,j-1,itrc))
#  endif
     &                     *(Hz(i,j,k)+Hz(i,j-1,k))*om_v(i,j)*(
     &         dTde(i,j,k1)-0.5*(
     &             min(dZde(i,j,k1),0.)*(dTdz(i,j-1,k1)+dTdz(i,j,k2))
     &            +max(dZde(i,j,k1),0.)*(dTdz(i,j-1,k2)+dTdz(i,j,k1))
     &                                                             ))
            enddo
          enddo
          if (k.lt.N) then
            do j=jstr,jend
              do i=istr,iend
                cff1=min(dZdx(i  ,j,k1),0.)
                cff2=min(dZdx(i+1,j,k2),0.)
                cff3=max(dZdx(i  ,j,k2),0.)
                cff4=max(dZdx(i+1,j,k1),0.)
#  ifdef DIF_COEF_3D
                FC(i,j,k2)=0.5*diff3d(i,j,k)*(
#  else
                FC(i,j,k2)=0.5*diff2(i,j,itrc)*(
#  endif
     &            cff1*(cff1*dTdz(i,j,k2)-dTdx(i  ,j,k1))
     &           +cff2*(cff2*dTdz(i,j,k2)-dTdx(i+1,j,k2))
     &           +cff3*(cff3*dTdz(i,j,k2)-dTdx(i  ,j,k2))
     &           +cff4*(cff4*dTdz(i,j,k2)-dTdx(i+1,j,k1))
     &                                         )
                cff1=min(dZde(i,j  ,k1),0.)
                cff2=min(dZde(i,j+1,k2),0.)
                cff3=max(dZde(i,j  ,k2),0.)
                cff4=max(dZde(i,j+1,k1),0.)
 
#  ifdef DIF_COEF_3D
                FC(i,j,k2)=FC(i,j,k2)+0.5*diff3d(i,j,k)*(
#  else
                FC(i,j,k2)=FC(i,j,k2)+0.5*diff2(i,j,itrc)*(
#  endif
     &            cff1*(cff1*dTdz(i,j,k2)-dTde(i,j  ,k1))
     &           +cff2*(cff2*dTdz(i,j,k2)-dTde(i,j+1,k2))
     &           +cff3*(cff3*dTdz(i,j,k2)-dTde(i,j  ,k2))
     &           +cff4*(cff4*dTdz(i,j,k2)-dTde(i,j+1,k1))
     &                                                    )
              enddo
            enddo
          endif
          do j=jstr,jend
            do i=istr,iend
              cff1=pm(i,j)*pn(i,j)
              t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)+dt*(
     &                                   cff1*( FX(i+1,j)-FX(i,j)
     &                                         +FE(i,j+1)-FE(i,j))
     &                                         +FC(i,j,k2)-FC(i,j,k1)
     &                                                   )/Hz(i,j,k)
            enddo
          enddo
        endif  ! <-- k.gt.0
      enddo   ! --> k

# endif /* T_DIF2 */


# ifdef TS_DIF4
!
!-----------------------------------------------------------------------
!  Compute horizontal biharmonic diffusion along geopotential
!  surfaces.  The biharmonic operator is computed by applying
!  the harmonic operator twice. 
!
!  To increase stability range, rotated biharmonic diffusion is 
!  applied to T at time between nrhs (n+1/2) and nstp (n) 
!  with coefficients: (3/4)*T(nrhs)+(1/4)*T(nstp)
!-----------------------------------------------------------------------
!
#  ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
        imin=istr
      else
        imin=istr-1
      endif
      if (EASTERN_EDGE) then
        imax=iend
      else
        imax=iend+1
      endif
#  else
      imin=istr-1
      imax=iend+1
#  endif
#  ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        jmin=jstr
      else
        jmin=jstr-1
      endif
      if (NORTHERN_EDGE) then
        jmax=jend
      else
        jmax=jend+1
      endif
#  else
      jmin=jstr-1
      jmax=jend+1
#  endif
!
! Compute horizontal biharmonic diffusion      !!! WARNING: RECURSIVE
! along geopotential surfaces. The biharmonic  !!! BLOCKING SEQUENCE
! operator is computed by applying the rotated !!!
! Laplacian operator twice.
!
! THE FIRST LAPLACIAN:! vertical   dTdx,dTde(:,:,k2) k+1   rho-points
!                     ! placement    dTdz,FC(:,:,k2) k+1/2   W-points 
      k2=1            ! of scratch dTdx,dTde(:,:,k1) k     rho-points 
      do k=0,N,+1     ! variables    dTdz,FC(:,:,k1) k-1/2   W-points 
        k1=k2
        k2=3-k1
        if (k.lt.N) then
          do j=jmin,jmax
            do i=imin,imax+1
              cff=0.5*(pm(i,j)+pm(i-1,j)) 
#  ifdef MASKING
     &                                        * umask(i,j)
#  endif
              dZdx(i,j,k2)=cff*(z_r(i,j,k+1)-z_r(i-1,j,k+1))
!              dTdx(i,j,k2)=cff*(t(i,j,k+1,nrhs,itrc)-
!     &                                 t(i-1,j,k+1,nrhs,itrc)
              dTdx(i,j,k2)=cff*( 0.75*(t(i,j,k+1,nrhs,itrc)-
     &                                 t(i-1,j,k+1,nrhs,itrc))
     &                          +0.25*(t(i,j,k+1,nstp,itrc)-
     &                                 t(i-1,j,k+1,nstp,itrc))
#  if defined CLIMAT_TS_MIXH || defined CLIMAT_TS_MIXH_FINE
     &                -TCLM(i,j,k+1,itrc)+TCLM(i-1,j,k+1,itrc)
#  endif
     &                         )
            enddo
          enddo
          do j=jmin,jmax+1
            do i=imin,imax
              cff=0.5*(pn(i,j)+pn(i,j-1)) 
#    ifdef MASKING
     &                                         * vmask(i,j)
#    endif
              dZde(i,j,k2)=cff*(z_r(i,j,k+1)-z_r(i,j-1,k+1))
!              dTde(i,j,k2)=cff*(t(i,j,k+1,nrhs,itrc)-
!     &                                 t(i,j-1,k+1,nrhs,itrc)
              dTde(i,j,k2)=cff*( 0.75*(t(i,j,k+1,nrhs,itrc)-
     &                                 t(i,j-1,k+1,nrhs,itrc))
     &                          +0.25*(t(i,j,k+1,nstp,itrc)-
     &                                 t(i,j-1,k+1,nstp,itrc))
#  if defined CLIMAT_TS_MIXH || defined CLIMAT_TS_MIXH_FINE
     &                -TCLM(i,j,k+1,itrc)+TCLM(i,j-1,k+1,itrc)
#  endif
     &                         )
            enddo
          enddo
        endif
 
        if (k.eq.0 .or. k.eq.N) then
          do j=jmin-1,jmax+1
            do i=imin-1,imax+1
              dTdz(i,j,k2)=0.0
              FC(i,j,k2)=0.0
            enddo
          enddo
        else
          do j=jmin-1,jmax+1
            do i=imin-1,imax+1
!              dTdz(i,j,k2)=(t(i,j,k+1,nrhs,itrc)-t(i,j,k,nrhs,itrc)
              dTdz(i,j,k2)=( 0.75*(t(i,j,k+1,nrhs,itrc)
     &                            -t(i,j,k,nrhs,itrc))
     &                      +0.25*(t(i,j,k+1,nstp,itrc)
     &                            -t(i,j,k,nstp,itrc))
#  if defined CLIMAT_TS_MIXH || defined CLIMAT_TS_MIXH_FINE
     &                      -TCLM(i,j,k+1,itrc)+TCLM(i,j,k,itrc)
#  endif
     &                     )/(z_r(i,j,k+1)-z_r(i,j,k))
            enddo
          enddo
        endif
 
        if (k.gt.0) then
          do j=jmin,jmax
            do i=imin,imax+1
              FX(i,j)=0.5*(Hz(i,j,k)+Hz(i-1,j,k))*on_u(i,j)*(
     &                                                dTdx(i  ,j,k1)
     &      -0.5*( min(dZdx(i,j,k1),0.)*(dTdz(i-1,j,k1)+dTdz(i,j,k2))
     &            +max(dZdx(i,j,k1),0.)*(dTdz(i-1,j,k2)+dTdz(i,j,k1))
     &                                                            ))
            enddo
          enddo
          do j=jmin,jmax+1
            do i=imin,imax
              FE(i,j)=0.5*(Hz(i,j,k)+Hz(i,j-1,k))*om_v(i,j)*(
     &         dTde(i,j,k1)-0.5*(
     &             min(dZde(i,j,k1),0.)*(dTdz(i,j-1,k1)+dTdz(i,j,k2))
     &            +max(dZde(i,j,k1),0.)*(dTdz(i,j-1,k2)+dTdz(i,j,k1))
     &                                                             ))
            enddo
          enddo
          if (k.lt.N) then
            do j=jmin,jmax
              do i=imin,imax
                cff1=min(dZdx(i  ,j,k1),0.)
                cff2=min(dZdx(i+1,j,k2),0.)
                cff3=max(dZdx(i  ,j,k2),0.)
                cff4=max(dZdx(i+1,j,k1),0.)

                FC(i,j,k2)=0.5*(
     &            cff1*(cff1*dTdz(i,j,k2)-dTdx(i  ,j,k1))
     &           +cff2*(cff2*dTdz(i,j,k2)-dTdx(i+1,j,k2))
     &           +cff3*(cff3*dTdz(i,j,k2)-dTdx(i  ,j,k2))
     &           +cff4*(cff4*dTdz(i,j,k2)-dTdx(i+1,j,k1))
     &                          )
                cff1=min(dZde(i,j  ,k1),0.)
                cff2=min(dZde(i,j+1,k2),0.)
                cff3=max(dZde(i,j  ,k2),0.)
                cff4=max(dZde(i,j+1,k1),0.)
 
                FC(i,j,k2)=FC(i,j,k2)+0.5*(
     &            cff1*(cff1*dTdz(i,j,k2)-dTde(i,j  ,k1))
     &           +cff2*(cff2*dTdz(i,j,k2)-dTde(i,j+1,k2))
     &           +cff3*(cff3*dTdz(i,j,k2)-dTde(i,j  ,k2))
     &           +cff4*(cff4*dTdz(i,j,k2)-dTde(i,j+1,k1))
     &                                    )
              enddo
            enddo
          endif
          do j=jmin,jmax
            do i=imin,imax
              LapT(i,j,k)=( pm(i,j)*pn(i,j)*( FX(i+1,j)-FX(i,j)
     &                                       +FE(i,j+1)-FE(i,j))
     &                                    +FC(i,j,k2)-FC(i,j,k1)
     &                                              )/Hz(i,j,k)
            enddo
          enddo
        endif   ! <-- k.gt.0
      enddo   ! --> k

#  ifndef EW_PERIODIC
        if (WESTERN_EDGE) then            ! Apply lateral boundary
          do k=1,N                        ! conditions to the first
            do j=jmin,jmax                ! Laplacian (in cases other
#   ifndef OBC_WEST
              LapT(istr-1,j,k)=0.         ! than periodic: closed
#   else
              LapT(istr-1,j,k)=LapT(istr,j,k)      !  or gradient)
#   endif
            enddo
          enddo
        endif
        if (EASTERN_EDGE) then
          do k=1,N
            do j=jmin,jmax
#   ifndef OBC_EAST
              LapT(iend+1,j,k)=0.
#   else
              LapT(iend+1,j,k)=LapT(iend,j,k)
#   endif
            enddo
          enddo
        endif
#  endif /* !EW_PERIODIC */
#  ifndef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do k=1,N
            do i=imin,imax
#   ifndef OBC_SOUTH
              LapT(i,jstr-1,k)=0.
#   else
              LapT(i,jstr-1,k)=LapT(i,jstr,k)
#   endif
            enddo
          enddo
        endif
        if (NORTHERN_EDGE) then
          do k=1,N
            do i=imin,imax
#   ifndef OBC_NORTH
              LapT(i,jend+1,k)=0.
#   else
              LapT(i,jend+1,k)=LapT(i,jend,k)
#   endif
            enddo
          enddo
        endif
#  endif /* !NS_PERIODIC */
!
! THE SECOND LAPLACIAN
!
      k2=1
      do k=0,N,+1
       k1=k2
       k2=3-k1
 
        if (k.lt.N) then
          do j=jstr,jend
            do i=istr,iend+1
              cff=0.5*(pm(i,j)+pm(i-1,j)) 
#    ifdef MASKING
     &                                    * umask(i,j)
#    endif
              dZdx(i,j,k2)=cff*(z_r(i,j,k+1) -z_r(i-1,j,k+1))
              dTdx(i,j,k2)=cff*(LapT(i,j,k+1)-LapT(i-1,j,k+1))
            enddo
          enddo
          do j=jstr,jend+1
            do i=istr,iend
              cff=0.5*(pn(i,j)+pn(i,j-1)) 
#    ifdef MASKING
     &                                     * vmask(i,j)
#    endif
              dZde(i,j,k2)=cff*(z_r(i,j,k+1) -z_r(i,j-1,k+1))
              dTde(i,j,k2)=cff*(LapT(i,j,k+1)-LapT(i,j-1,k+1))
            enddo
          enddo
        endif
 
        if (k.eq.0 .or. k.eq.N) then
          do j=jstr-1,jend+1
            do i=istr-1,iend+1
              dTdz(i,j,k2)=0.0
              FC(i,j,k2)=0.0
            enddo
          enddo
        else
          do j=jstr-1,jend+1
            do i=istr-1,iend+1
              dTdz(i,j,k2)=(LapT(i,j,k+1)-LapT(i,j,k))
     &                       /(z_r(i,j,k+1)-z_r(i,j,k))
            enddo
          enddo
        endif
 
        if (k.gt.0) then

          do j=jstr,jend
            do i=istr,iend+1
#  ifdef DIF_COEF_3D
#   ifdef TS_SPLIT_UP3
              FX(i,j)=-0.5*diff3d_u(i,j,k)
#   else
              FX(i,j)=-0.25*(diff3d(i,j,k)+diff3d(i-1,j,k))
#   endif
#  else
              FX(i,j)=-0.25*(diff4(i,j,itrc)+diff4(i-1,j,itrc))
#  endif
     &                     *(Hz(i,j,k)+Hz(i-1,j,k))*on_u(i,j)*(
     &         dTdx(i  ,j,k1)-0.5*(
     &             min(dZdx(i,j,k1),0.)*(dTdz(i-1,j,k1)+dTdz(i,j,k2))
     &            +max(dZdx(i,j,k1),0.)*(dTdz(i-1,j,k2)+dTdz(i,j,k1))
     &                                                             ))
            enddo
          enddo
          do j=jstr,jend+1
            do i=istr,iend
#  ifdef DIF_COEF_3D
#   ifdef TS_SPLIT_UP3
              FE(i,j)=-0.5*diff3d_v(i,j,k)
#   else
              FE(i,j)=-0.25*(diff3d(i,j,k)+diff3d(i,j-1,k))
#   endif
#  else
              FE(i,j)=-0.25*(diff4(i,j,itrc)+diff4(i,j-1,itrc))
#  endif
     &                     *(Hz(i,j,k)+Hz(i,j-1,k))*om_v(i,j)*(
     &         dTde(i,j,k1)-0.5*(
     &             min(dZde(i,j,k1),0.)*(dTdz(i,j-1,k1)+dTdz(i,j,k2))
     &            +max(dZde(i,j,k1),0.)*(dTdz(i,j-1,k2)+dTdz(i,j,k1))
     &                                                             ))
            enddo
          enddo
          if (k.lt.N) then
            do j=jstr,jend
              do i=istr,iend
                cff1=min(dZdx(i  ,j,k1),0.)
                cff2=min(dZdx(i+1,j,k2),0.)
                cff3=max(dZdx(i  ,j,k2),0.)
                cff4=max(dZdx(i+1,j,k1),0.)
#  ifdef DIF_COEF_3D
#   ifdef TS_SPLIT_UP3
                FC(i,j,k2)=-0.125*(diff3d_u(i,j,k)+diff3d_u(i+1,j,k)
     &                      +diff3d_u(i,j,k+1)+diff3d_u(i+1,j,k+1))*(
#   else
                FC(i,j,k2)=-0.5*diff3d(i,j,k)*(
#   endif
#  else
                FC(i,j,k2)=-0.5*diff4(i,j,itrc)*(
#  endif
     &            cff1*(cff1*dTdz(i,j,k2)-dTdx(i  ,j,k1))
     &           +cff2*(cff2*dTdz(i,j,k2)-dTdx(i+1,j,k2))
     &           +cff3*(cff3*dTdz(i,j,k2)-dTdx(i  ,j,k2))
     &           +cff4*(cff4*dTdz(i,j,k2)-dTdx(i+1,j,k1))
     &                                           )
                cff1=min(dZde(i,j  ,k1),0.)
                cff2=min(dZde(i,j+1,k2),0.)
                cff3=max(dZde(i,j  ,k2),0.)
                cff4=max(dZde(i,j+1,k1),0.)
#  ifdef DIF_COEF_3D
#   ifdef TS_SPLIT_UP3
                FC(i,j,k2)=FC(i,j,k2)
     &                -0.125*(diff3d_v(i,j,k)+diff3d_v(i,j+1,k)
     &                  +diff3d_v(i,j,k+1)+diff3d_v(i,j+1,k+1))*(
#   else
                FC(i,j,k2)=FC(i,j,k2)-0.5*diff3d(i,j,k)*(
#   endif
#  else
                FC(i,j,k2)=FC(i,j,k2)-0.5*diff4(i,j,itrc)*(
#  endif
     &            cff1*(cff1*dTdz(i,j,k2)-dTde(i,j  ,k1))
     &           +cff2*(cff2*dTdz(i,j,k2)-dTde(i,j+1,k2))
     &           +cff3*(cff3*dTdz(i,j,k2)-dTde(i,j  ,k2))
     &           +cff4*(cff4*dTdz(i,j,k2)-dTde(i,j+1,k1))
     &                                                     )
              enddo
            enddo
          endif
          do j=jstr,jend
            do i=istr,iend
              cff1=pm(i,j)*pn(i,j)
              t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc) + dt*(
     &                              cff1*( FX(i+1,j)-FX(i,j)
     &                                    +FE(i,j+1)-FE(i,j))
     &                     +FC(i,j,k2)-FC(i,j,k1) )/Hz(i,j,k)
            enddo
          enddo
        endif  ! <-- k.gt.0
      enddo   ! --> k

# endif /* TS_DIF4 */
!---------------------------------------------------------------------
# ifdef DIAGNOSTICS_TS
!
!==========================================================
! Tracer diagnostics
!==========================================================
!
      do k=1,N
        do j=jstr,jend
          do i=istr,iend
            cff1=pm(i,j)*pn(i,j)
            THmix(i,j,k,itrc)=FX(i+1,j)-FX(i,j)
     &                       +FE(i,j+1)-FE(i,j)
#  ifndef MIX_S_TS
     &                       +(FC(i,j,k2)-FC(i,j,k1))/cff1
# endif
#  ifdef MASKING
     &                                        * rmask(i,j)
#  endif
            Trate(i,j,k,itrc)=(Hz(i,j,k)*t(i,j,k,nnew,itrc)
     &                    -Hz_bak(i,j,k)*t(i,j,k,nstp,itrc))
     &                                            /(dt*cff1)
#  ifdef MASKING
     &                                        * rmask(i,j)
#  endif
!
! Divide all diagnostic terms by the cell volume
! (Hz(i,j,k,itrc)/(pm(i,j).*pn(i,j)). There after the unit
! of diag terms are: (unit of tracers)* s-1.
!
            THmix(i,j,k,itrc)=THmix(i,j,k,itrc)*cff1/Hz(i,j,k)
            Trate(i,j,k,itrc)=Trate(i,j,k,itrc)*cff1/Hz(i,j,k)
          enddo
        enddo
      enddo 
!
! Compute tracer diagnostics averaged over the MLD
!
#  ifdef DIAGNOSTICS_TS_MLD
       do j=Jstr,Jend
         do i=Istr,Iend
            THmix_mld(i,j,itrc)=0.
            Trate_mld(i,j,itrc)=0.
         enddo
       enddo
       do j=Jstr,Jend
         do i=Istr,Iend
#   ifdef LMD_SKPP
           kmld=kbl(i,j)
#   else
           kmld=N-5
#   endif
           do k=N,kmld,-1
             cff=Hz(i,j,k)/(z_w(i,j,N)-z_w(i,j,kmld-1))
             THmix_mld(i,j,itrc)=THmix_mld(i,j,itrc)+
     &                           THmix(i,j,k,itrc)*cff
             Trate_mld(i,j,itrc)=Trate_mld(i,j,itrc)+
     &                           Trate(i,j,k,itrc)*cff
           enddo
         enddo
       enddo
#  endif
# endif /* DIAGNOSTICS_TS */
!---------------------------------------------------------------------
#  if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
        call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                          t(START_2D_ARRAY,1,nnew,itrc))
#  endif
!
       return
      end
 
#ifndef CHILD_SPG
# define CHILD_SPG
# ifdef AGRIF
#  include "t3dmix_GP.F"
# endif
# undef CHILD_SPG
#endif  /* !CHILD_SPG */


 
