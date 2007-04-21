!
! $Id: t3dmix_S.F,v 1.4 2005/10/10 13:40:19 pmarches Exp $
!
#include "cppdefs.h"
#if defined SOLVE3D && defined TS_DIF2 && defined AGRIF

      subroutine t3dmix_fine (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      
        call t3dmix_fine_tile (Istr,Iend,Jstr,Jend, A2d(1,1,trd),
     &                                 A2d(1,2,trd), A2d(1,3,trd))
      
      return
      end

      subroutine t3dmix_fine_tile (Istr,Iend,Jstr,Jend, FX,FE,LapT)
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
# include "zoom.h"
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
       integer decal
       
       external interpsponget
!
# include "compute_auxiliary_bounds.h"
!

!     IF (mod(nbstep3d,Agrif_Irhot()).NE.0) RETURN

C$OMP BARRIER
C$OMP MASTER
      decal = 2*max(Agrif_Irhox(),Agrif_Irhoy())+1
      Call Agrif_Set_bc(updateTprof,(/-decal,0/),
     &     InterpolationShouldbemade=.TRUE.)
#ifdef MASKING
        Agrif_UseSpecialvalue=.true.
#endif
      Agrif_Specialvalue=0.
      tsponge = 0.
      Call Agrif_Bc_Variable(tsponge,updateTprof,procname=interpsponget)
      Agrif_UseSpecialvalue=.false.

      do itrc=1,NT
      do k=1,N
      do j=0,Mm+1
      do i=0,Lm+1
        tsponge(i,j,k,itrc) = (t(i,j,k,nstp,itrc) - tsponge(i,j,k,itrc))
     &           *rmask(i,j)
      enddo
      enddo
      enddo
      enddo

C$OMP END MASTER
C$OMP BARRIER

!
      do itrc = 1,NT
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
     &                     tsponge(i,j,k,itrc)-tsponge(i-1,j,k,itrc)
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
     &                      tsponge(i,j,k,itrc)-tsponge(i,j-1,k,itrc)
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
      enddo

      return
      end
      
      subroutine interpsponget(tabres,i1,i2,j1,j2,k1,k2,m1,m2)
      implicit none
# include "param.h"
# include "ocean3d.h"
# include "scalars.h"

       integer i1,i2,j1,j2,k1,k2,m1,m2
       real tabres(i1:i2,j1:j2,k1:k2,m1:m2)
       
       tabres(i1:i2,j1:j2,k1:k2,m1:m2) = 
     &               t(i1:i2,j1:j2,k1:k2,nstp,m1:m2)
      
      return
      end  
#else
      subroutine t3dmix_fine_empty
      end
# endif /* SOLVE3D && TS_DIF2 && MIX_S_TS */
