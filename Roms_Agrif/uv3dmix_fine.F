!
! $Id: uv3dmix_S.F,v 1.3 2005/10/10 13:40:19 pmarches Exp $
!
#include "cppdefs.h"
# if defined SOLVE3D && defined AGRIF
 
      subroutine uv3dmix_fine (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call uv3dmix_fine_tile (Istr,Iend,Jstr,Jend, 
     &                        A2d(1,1,trd), A2d(1,2,trd),
     &                        A2d(1,3,trd), A2d(1,4,trd))
      return
      end
 
      subroutine uv3dmix_fine_tile (Istr,Iend,Jstr,Jend, UFx,UFe,VFx,VFe)
!
! Computes harmonic mixing of momentum, along constant S-surfaces
! as horizontal divergence of the stress tensor. Components of the
! stress tensor are:
!                         du     dv
!         s_xx = -s_yy = ---- - -----
!                         dx     dy
!
!                         du     dv
!         s_xy =  s_yx = ---- + ----
!                         dy     dx
!
! Reference:
!
! Wajsowicz, R.C, 1993: A consistent formulation of the anisotropic
!     stress tensor for use in models of the large-scale ocean
!     circulation, JCP, 105, 333-338.
!
! Sadourny, R. and K. Maynard, 1997: Formulations of lateral
!     diffusion in geophysical fluid dynamics models, In "Numerical
!     Methods of Atmospheric and Oceanic Modelling". Lin, Laprise,
!     and Ritchie, Eds., NRC Research Press, 547-556.
!
! Griffies, S.M. and R.W. Hallberg, 2000: Biharmonic friction with
!     a Smagorinsky-like viscosity for use in large-scale eddy-
!     permitting ocean models, Mon. Wea. Rev., 128, 8, 2935-2946.
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k, indx
      real UFe(PRIVATE_2D_SCRATCH_ARRAY),
     &     UFx(PRIVATE_2D_SCRATCH_ARRAY), cff,
     &     VFe(PRIVATE_2D_SCRATCH_ARRAY),
     &     VFx(PRIVATE_2D_SCRATCH_ARRAY)
# include "param.h"
# include "scalars.h"
# include "grid.h"
# include "ocean3d.h"
# include "coupling.h"
# include "mixing.h"
# include "zoom.h"
# ifdef DIAGNOSTICS_UV
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

       integer decal
       
       external interpspongeu, interpspongev
!
# include "compute_auxiliary_bounds.h"
!
!     IF (mod(nbstep3d,Agrif_Irhot()).NE.0) RETURN

C$OMP BARRIER
C$OMP MASTER
      decal = 2*max(Agrif_Irhox(),Agrif_Irhoy())+1
      Call Agrif_Set_bc(Huon,(/-decal,0/),
     &     InterpolationShouldbemade=.TRUE.)
      Call Agrif_Set_bc(Hvom,(/-decal,0/),
     &     InterpolationShouldbemade=.TRUE.)
      usponge = 0.
      vsponge = 0.
      Call Agrif_Bc_Variable(usponge,Huon,procname=interpspongeu)
      Call Agrif_Bc_Variable(vsponge,Hvom,procname=interpspongev)

      
      do k=1,N
      do j=0,Mm+1
      do i=0,Lm+1
        usponge(i,j,k) = (u(i,j,k,nstp) - usponge(i,j,k))*umask(i,j)
      enddo
      enddo
      enddo
      
      do k=1,N
      do j=0,Mm+1
      do i=0,Lm+1
        vsponge(i,j,k) = (v(i,j,k,nstp) - vsponge(i,j,k))*vmask(i,j)
      enddo
      enddo
      enddo

C$OMP END MASTER
C$OMP BARRIER

      indx=3-nstp    !--> time index for target arrays;

            
!
!  Compute flux-components of the horizontal divergence of the stress
!  tensor (m5/s2) in XI- and ETA-directions.
!
      do k=1,N
        do j=JstrV-1,Jend
          do i=IstrU-1,Iend
            cff=0.5*Hz(i,j,k)*visc2_r(i,j)*(
     &      on_r(i,j)*pm(i,j)*( (pn(i  ,j)+pn(i+1,j))*usponge(i+1,j,k)
     &                         -(pn(i-1,j)+pn(i  ,j))*usponge(i  ,j,k)
     &                                                              )
     &     -om_r(i,j)*pn(i,j)*( (pm(i,j  )+pm(i,j+1))*vsponge(i,j+1,k)
     &                         -(pm(i,j-1)+pm(i,j  ))*vsponge(i,j  ,k)
     &                                                             ))
            UFx(i,j)=on_r(i,j)*on_r(i,j)*cff
            VFe(i,j)=om_r(i,j)*om_r(i,j)*cff
          enddo
        enddo
        do j=Jstr,Jend+1
          do i=Istr,Iend+1
            cff=0.125*visc2_p(i,j)*
     &      (Hz(i-1,j,k)+Hz(i,j,k)+Hz(i-1,j-1,k)+Hz(i,j-1,k))*(
     &      0.25*(pm(i-1,j)+pm(i,j)+pm(i-1,j-1)+pm(i,j-1))*on_p(i,j)
     &                     *( (pn(i  ,j-1)+pn(i  ,j))*vsponge(i  ,j,k)
     &                       -(pn(i-1,j-1)+pn(i-1,j))*vsponge(i-1,j,k)
     &                                                              )
     &     +0.25*(pn(i-1,j)+pn(i,j)+pn(i-1,j-1)+pn(i,j-1))*om_p(i,j)
     &                     *( (pm(i-1,j  )+pm(i,j  ))*usponge(i,j  ,k)
     &                       -(pm(i-1,j-1)+pm(i,j-1))*usponge(i,j-1,k)
     &                                                             ))
# ifdef MASKING
     &                                                   *pmask(i,j)
# endif
            UFe(i,j)=om_p(i,j)*om_p(i,j)*cff
            VFx(i,j)=on_p(i,j)*on_p(i,j)*cff
          enddo
        enddo
!
! Apply viscous terms. Note that at this stage arrays u,v(...,3-nstp)
! contain Hz*U and Hz*V with units of [m2/s].   Also compute vertical
! integral of viscous terms and add it into coupling terms for the
! barotropic mode
!
        do j=Jstr,Jend
          do i=IstrU,Iend
            cff=0.125*(pm(i-1,j)+pm(i,j))*(pn(i-1,j) +pn(i,j))
     &             *( (pn(i-1,j)+pn(i,j))*(UFx(i,j)-UFx(i-1,j))
     &               +(pm(i-1,j)+pm(i,j))*(UFe(i,j+1)-UFe(i,j))
     &                                                        )
            rufrc(i,j)=rufrc(i,j) + cff
            u(i,j,k,indx)=u(i,j,k,indx) + dt*cff
# ifdef DIAGNOSTICS_UV
            MHmix(i,j,k,1) = cff*om_u(i,j)*on_u(i,j)
# endif
          enddo
        enddo
        do j=JstrV,Jend
          do i=Istr,Iend
            cff=0.125*(pm(i,j)+pm(i,j-1))*(pn(i,j) +pn(i,j-1))
     &             *( (pn(i,j-1)+pn(i,j))*(VFx(i+1,j)-VFx(i,j))
     &               -(pm(i,j-1)+pm(i,j))*(VFe(i,j)-VFe(i,j-1))
     &                                                        )
            rvfrc(i,j)=rvfrc(i,j) + cff
            v(i,j,k,indx)=v(i,j,k,indx) + dt*cff
# ifdef DIAGNOSTICS_UV
            MHmix(i,j,k,2) = cff*om_v(i,j)*on_v(i,j)
# endif
          enddo
        enddo
      enddo
      return
      end
      
      subroutine interpspongeu(tabres,i1,i2,j1,j2,k1,k2)
      implicit none
# include "param.h"
# include "ocean3d.h"
# include "scalars.h"

       integer i1,i2,j1,j2,k1,k2
       real tabres(i1:i2,j1:j2,k1:k2)
       
       tabres(i1:i2,j1:j2,k1:k2) = u(i1:i2,j1:j2,k1:k2,nstp)
             
      return
      end
      
      subroutine interpspongev(tabres,i1,i2,j1,j2,k1,k2)
      implicit none
# include "param.h"
# include "ocean3d.h"
# include "scalars.h"

       integer i1,i2,j1,j2,k1,k2
       real tabres(i1:i2,j1:j2,k1:k2)
       
       tabres(i1:i2,j1:j2,k1:k2) = v(i1:i2,j1:j2,k1:k2,nstp)
             
      return
      end      
            
#else
      subroutine nestingvisc3d_empty
      end
#endif
 
