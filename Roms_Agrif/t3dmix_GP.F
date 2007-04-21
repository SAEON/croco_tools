!
! $Id: t3dmix_GP.F,v 1.7 2005/10/10 13:40:19 pmarches Exp $
!
#include "cppdefs.h"
#if defined SOLVE3D && defined TS_DIF2 && defined MIX_GP_TS

      subroutine t3dmix (tile)
      implicit none
      integer tile, trd, omp_get_thread_num, itrc
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      do itrc=1,NT
        call t3dmix_tile (Istr,Iend,Jstr,Jend, itrc, A3d(1,1,trd),
     &                                  A2d(1, 1,trd), A2d(1, 2,trd),
     &                    A2d(1,3,trd), A2d(1, 5,trd), A2d(1, 7,trd),
     &                    A2d(1,9,trd), A2d(1,11,trd), A2d(1,13,trd))
      enddo
      return
      end

      subroutine t3dmix_tile (Istr,Iend,Jstr,Jend, itrc, LapT, FX,FE,
     &                                  FC,dTdz, dTdx,dTde,dZdx,dZde)
!
!--------------------------------------------------------------------
!  Compute horizontal mixing terms for tracer equations.
!--------------------------------------------------------------------
!
      implicit none
# include "param.h"
      integer itrc, Istr,Iend,Jstr,Jend, i,j,k,k1,k2
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
# include "climat.h"
# include "scalars.h"
# ifdef DIAGNOSTICS_TS
#  include "diagnostics.h"
# endif
!
# ifdef MPI
#  define LOCALLM Lmmpi
#  define LOCALMM Mmmpi
# else
#  define LOCALLM Lm
#  define LOCALMM Mm
# endif
# include "compute_auxiliary_bounds.h"
!
! Compute and add in horizontal Laplacian     !!! WARNING: RECURSIVE
! diffusion along geopotential surfaces.          BLOCKING SEQUENCE
!--------------------------------------------------------------------
!                   !  Vertical    dTdx,dTde(:,:,k2) k+1   rho-points
      k2=1          !  placement     dTdz,FC(:,:,k2) k+1/2   W-points
      do k=0,N,+1   !  of fields:  dTdx,dTde(:,:,k1) k     pho-points
       k1=k2        !                dTdz,FC(:,:,k1) k-1/2   W-points
       k2=3-k1

        if (k.lt.N) then
          do j=Jstr,Jend
            do i=Istr,Iend+1
              cff=0.5*(pm(i,j)+pm(i-1,j))
# ifdef MASKING
     &           *umask(i,j)
# endif
              dZdx(i,j,k2)=cff*(z_r(i,j,k+1) -z_r(i-1,j,k+1))
              dTdx(i,j,k2)=cff*(t(i,j,k+1,nrhs,itrc)
     &                               -t(i-1,j,k+1,nrhs,itrc)
# ifdef CLIMAT_TS_MIXH
     &               -tclm(i,j,k+1,itrc)+tclm(i-1,j,k+1,itrc)
# endif
     &                                                      )
            enddo
          enddo
          do j=Jstr,Jend+1
            do i=Istr,Iend
              cff=0.5*(pn(i,j)+pn(i,j-1))
# ifdef MASKING
     &           *vmask(i,j)
# endif
              dZde(i,j,k2)=cff*(z_r(i,j,k+1) -z_r(i,j-1,k+1))
              dTde(i,j,k2)=cff*(t(i,j,k+1,nrhs,itrc)
     &                               -t(i,j-1,k+1,nrhs,itrc)
# ifdef CLIMAT_TS_MIXH
     &               -tclm(i,j,k+1,itrc)+tclm(i,j-1,k+1,itrc)
# endif
     &                                                      )
            enddo
          enddo
        endif

        if (k.eq.0 .or. k.eq.N) then
          do j=Jstr-1,Jend+1
            do i=Istr-1,Iend+1
              dTdz(i,j,k2)=0.0     ! Set no-flux bottom or
              FC(i,j,k2)=0.0       ! top boundary conditions.
            enddo
          enddo
        else
          do j=Jstr-1,Jend+1
            do i=Istr-1,Iend+1
              dTdz(i,j,k2)=(t(i,j,k+1,nrhs,itrc)-t(i,j,k,nrhs,itrc)
# ifdef CLIMAT_TS_MIXH
     &                          -tclm(i,j,k+1,itrc)+tclm(i,j,k,itrc)
# endif
     &                                   )/(z_r(i,j,k+1)-z_r(i,j,k))
            enddo
          enddo
        endif

        if (k.gt.0) then
          do j=Jstr,Jend
            do i=Istr,Iend+1
              FX(i,j)=0.25*(diff2(i,j,itrc)+diff2(i-1,j,itrc))
     &                     *(Hz(i,j,k)+Hz(i-1,j,k))*on_u(i,j)*(
     &         dTdx(i  ,j,k1)-0.5*(
     &             min(dZdx(i,j,k1),0.)*(dTdz(i-1,j,k1)+dTdz(i,j,k2))
     &            +max(dZdx(i,j,k1),0.)*(dTdz(i-1,j,k2)+dTdz(i,j,k1))
     &                                                             ))
            enddo
          enddo
          do j=Jstr,Jend+1
            do i=Istr,Iend
              FE(i,j)=0.25*(diff2(i,j,itrc)+diff2(i,j-1,itrc))
     &                     *(Hz(i,j,k)+Hz(i,j-1,k))*om_v(i,j)*(
     &         dTde(i,j,k1)-0.5*(
     &             min(dZde(i,j,k1),0.)*(dTdz(i,j-1,k1)+dTdz(i,j,k2))
     &            +max(dZde(i,j,k1),0.)*(dTdz(i,j-1,k2)+dTdz(i,j,k1))
     &                                                             ))
            enddo
          enddo
          if (k.lt.N) then
            do j=Jstr,Jend
              do i=Istr,Iend
                cff1=min(dZdx(i  ,j,k1),0.)
                cff2=min(dZdx(i+1,j,k2),0.)

                cff3=max(dZdx(i  ,j,k2),0.)
                cff4=max(dZdx(i+1,j,k1),0.)

                FC(i,j,k2)=0.5*diff2(i,j,itrc)*(
     &                       cff1*(cff1*dTdz(i,j,k2)-dTdx(i  ,j,k1))
     &                        +cff2*(cff2*dTdz(i,j,k2)-dTdx(i+1,j,k2))
     &                        +cff3*(cff3*dTdz(i,j,k2)-dTdx(i  ,j,k2))
     &                        +cff4*(cff4*dTdz(i,j,k2)-dTdx(i+1,j,k1))
     &                                                               )
                cff1=min(dZde(i,j  ,k1),0.)
                cff2=min(dZde(i,j+1,k2),0.)

                cff3=max(dZde(i,j  ,k2),0.)
                cff4=max(dZde(i,j+1,k1),0.)

                FC(i,j,k2)=FC(i,j,k2)+0.5*diff2(i,j,itrc)*(
     &                        cff1*(cff1*dTdz(i,j,k2)-dTde(i,j  ,k1))
     &                       +cff2*(cff2*dTdz(i,j,k2)-dTde(i,j+1,k2))
     &                       +cff3*(cff3*dTdz(i,j,k2)-dTde(i,j  ,k2))
     &                       +cff4*(cff4*dTdz(i,j,k2)-dTde(i,j+1,k1))
     &                                                              )
              enddo
            enddo
          endif
          do j=Jstr,Jend
            do i=Istr,Iend
              cff1=pm(i,j)*pn(i,j)
              t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)+dt*(
     &                     cff1*(FX(i+1,j)-FX(i,j)+FE(i,j+1)-FE(i,j))
     &                          +FC(i,j,k2)-FC(i,j,k1) )/Hz(i,j,k)
# ifdef DIAGNOSTICS_TS
              THmix(i,j,k,itrc)=FX(i+1,j)-FX(i,j)
     &                         +FE(i,j+1)-FE(i,j)
     &                         +(FC(i,j,k2)-FC(i,j,k1))/cff1
#  ifdef MASKING
     &              * rmask(i,j)
#  endif
              Trate(i,j,k,itrc)=(Hz(i,j,k)*t(i,j,k,nnew,itrc)
     &                          -Hz_bak(i,j,k)*t(i,j,k,nstp,itrc))
     &                                                  /(dt*cff1)
#  ifdef MASKING
     &              * rmask(i,j)
#  endif
!
! Divide all diagnostic terms by the cell volume 
! (Hz(i,j,k,itrc)/(pm(i,j).*pn(i,j)). There after the unit
! of diag terms are: (unit of tracers)* s-1.
!
              THmix(i,j,k,itrc)=THmix(i,j,k,itrc)*cff1/Hz(i,j,k)
              Trate(i,j,k,itrc)=Trate(i,j,k,itrc)*cff1/Hz(i,j,k)
# endif 
            enddo
          enddo
        endif  ! <-- k.gt.0
      enddo   ! --> k

# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
        call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                          t(START_2D_ARRAY,1,nnew,itrc))
# endif

      return
      end
#else
      subroutine t3dmix_empty
      end
#endif /* SOLVE3D && TS_DIF2 && MIX_GP_TS*/
