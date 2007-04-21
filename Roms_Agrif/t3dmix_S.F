!
! $Id: t3dmix_S.F,v 1.4 2005/10/10 13:40:19 pmarches Exp $
!
#include "cppdefs.h"
#if defined SOLVE3D && defined TS_DIF2 && defined MIX_S_TS

      subroutine t3dmix (tile)
      implicit none
      integer tile, trd, omp_get_thread_num, itrc
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      do itrc=1,NT
        call t3dmix_tile (Istr,Iend,Jstr,Jend, itrc, A2d(1,1,trd),
     &                                 A2d(1,2,trd), A2d(1,3,trd))
      enddo
      return
      end

      subroutine t3dmix_tile (Istr,Iend,Jstr,Jend, itrc, FX,FE,LapT)
!
!  This routine computes horizontal mixing of tracers
!  along S-surfaces.
!
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "mixing.h"
# include "climat.h"
# include "scalars.h"
      integer itrc, Istr,Iend,Jstr,Jend, i,j,k
      real FX(PRIVATE_2D_SCRATCH_ARRAY),     cff1,
     &     FE(PRIVATE_2D_SCRATCH_ARRAY),     cff2,
     &     LapT(PRIVATE_2D_SCRATCH_ARRAY)
!
# ifdef MPI
#  define LOCALLM Lmmpi
#  define LOCALMM Mmmpi
# else
#  define LOCALLM Lm
#  define LOCALMM Mm
# endif
!
# include "compute_auxiliary_bounds.h"
!
      do k=1,N
!
!  Add in horizontal Laplacian diffusion along constant S-surfaces.
!--------------------------------------------------------------------
!  Compute XI- and ETA-components of diffusive tracer flux.
!
        do j=Jstr,Jend
          do i=Istr,Iend+1
            FX(i,j)=0.25*(diff2(i,j,itrc)+diff2(i-1,j,itrc))
     &                   *pmon_u(i,j)*(Hz(i,j,k)+Hz(i-1,j,k))*(
     &                     t(i,j,k,nrhs,itrc)-t(i-1,j,k,nrhs,itrc)
# ifdef CLIMAT_TS_MIXH
     &                          -tclm(i,j,k,itrc)+tclm(i-1,j,k,itrc)
# endif
     &                                                        )
# ifdef MASKING
     &                                                   *umask(i,j)
# endif
          enddo
        enddo
        do j=Jstr,Jend+1
          do i=Istr,Iend
            FE(i,j)=0.25*(diff2(i,j,itrc)+diff2(i,j-1,itrc))
     &                     *pnom_v(i,j)*(Hz(i,j,k)+Hz(i,j-1,k))*(
     &                      t(i,j,k,nrhs,itrc)-t(i,j-1,k,nrhs,itrc)
# ifdef CLIMAT_TS_MIXH
     &                          -tclm(i,j,k,itrc)+tclm(i,j-1,k,itrc)
# endif
     &                                                           )
# ifdef MASKING
     &                                                   *vmask(i,j)
# endif
          enddo
        enddo
!
!  Add in horizontal diffusion of tracer [T m^3/s].
!  Multiply by mixing coefficient.
!
        do j=Jstr,Jend
          do i=Istr,Iend
            cff1=pm(i,j)*pn(i,j)
            t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)+dt*cff1
     &                        *(FX(i+1,j)-FX(i,j)+FE(i,j+1)-FE(i,j))
     &                                                    /Hz(i,j,k)
# ifdef DIAGNOSTICS_TS
            THmix(i,j,k,itrc)=FX(i+1,j)-FX(i,j)
     &                       +FE(i,j+1)-FE(i,j)
     &                       +(FC(i,j,k2)-FC(i,j,k1))/cff1
#  ifdef MASKING
     &              * rmask(i,j)
#  endif
            Trate(i,j,k,itrc)=(Hz(i,j,k)*t(i,j,k,nnew,itrc)
     &                        -Hz_bak(i,j,k)*t(i,j,k,nstp,itrc))
     &                                                 /(dt*cff1)
#  ifdef MASKING
     &              * rmask(i,j)
#  endif
!
!  Divide all diagnostic terms by the cell volume
!  (Hz(i,j,k,itrc)/(pm(i,j).*pn(i,j)). There after the unit
!  of diag terms are: (unit of tracers)* s-1.
!
            THmix(i,j,k,itrc)=THmix(i,j,k,itrc)*cff1/Hz(i,j,k)
            Trate(i,j,k,itrc)=Trate(i,j,k,itrc)*cff1/Hz(i,j,k)
# endif
          enddo
        enddo
      enddo

# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
        call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                          t(START_2D_ARRAY,1,nnew,itrc))
# endif

      return
      end
#else
      subroutine t3dmix_empty
      end
# endif /* SOLVE3D && TS_DIF2 && MIX_S_TS */
