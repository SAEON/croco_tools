#include "cppdefs.h"
      subroutine uv3dmix (tile)
!
!=====================================================================
!  Copyright (c) 2000 Rutgers/UCLA                                   !
!================================================ Hernan G. Arango ===
!                                                                    !
!  This subroutine computes biharmonic mixing of momentum, along     !
!  constant  S-surfaces, from the  horizontal divergence  of the     !
!  stress tensor. A transverse isotropy is assumed so the stress     !
!  tensor is splitted into vertical and horizontal subtensors.       !
!                                                                    !
!  Reference:                                                        !
!                                                                    !
!      Wajsowicz, R.C, 1993: A consistent formulation of the         !
!         anisotropic stress tensor for use in models of the         !
!         large-scale ocean circulation, JCP, 105, 333-338.          !
!                                                                    !
!      Sadourny, R. and K. Maynard, 1997: Formulations of            !
!         lateral diffusion in geophysical fluid dynamics            !
!         models, In "Numerical Methods of Atmospheric and           !
!         Oceanic Modelling". Lin, Laprise, and Ritchie,             !
!         Eds., NRC Research Press, 547-556.                         !
!                                                                    !
!      Griffies, S.M. and R.W. Hallberg, 2000: Biharmonic            !
!         friction with a Smagorinsky-like viscosity for             !
!         use in large-scale eddy-permitting ocean models,           !
!         Monthly Weather Rev., 128, 8, 2935-2946.                   !
!                                                                    !
!=====================================================================
!
      implicit none
      integer tile, trd, omp_get_thread_num
#include "param.h"
#include "private_scratch.h"
!
#include "compute_tile_bounds.h"
!
      trd=omp_get_thread_num()
      call uv3dmix_tile (Istr,Iend,Jstr,Jend,
     &                      A2d(1, 1,trd),A2d(1, 3,trd),A2d(1, 5,trd),
     &                      A2d(1, 7,trd),A2d(1, 9,trd),A2d(1,11,trd))
      return
      end
!
!*********************************************************************
      subroutine uv3dmix_tile (Istr,Iend,Jstr,Jend,UFx,UFe,LapU,
     &                            VFx,VFe,LapV)
!*********************************************************************
!
      implicit none
# include "param.h"
# include "grid.h"
# include "mixing.h"
# include "ocean3d.h"
# include "scalars.h"
# ifdef DIAGNOSTICS_UV
#  include "diagnostics.h"
# endif
!
      integer	Iend, Istr, Jend, Jstr, i, j, k, indx
      real	cff
      real
     &         LapU(GLOBAL_2D_ARRAY),
     &         LapV(GLOBAL_2D_ARRAY),
     &          UFe(PRIVATE_2D_SCRATCH_ARRAY),
     &          UFx(PRIVATE_2D_SCRATCH_ARRAY),
     &          VFe(PRIVATE_2D_SCRATCH_ARRAY),
     &          VFx(PRIVATE_2D_SCRATCH_ARRAY)
!
# include "compute_auxiliary_bounds.h"
 
      indx=3-nstp    !--> time index for target arrays;

      do k=1,N
!
!---------------------------------------------------------------------
!  Compute horizontal biharmonic viscosity along constant S-surfaces.
!  The biharmonic operator is computed by applying the harmonic
!  operator twice.
!---------------------------------------------------------------------
!
!  Compute flux-components of the horizontal divergence of the stress
!  tensor (m4 s^-3/2) in XI- and ETA-directions.  It is assumed here
!  that "visc4_r" and "visc4_p" are the squared root of the biharmonic
!  viscosity coefficient.  For momentum balance purposes, the
!  thickness "Hz" appears only when computing the second harmonic
!  operator.
!
        do j=JstrV-1,Jend
          do i=IstrU-1,Iend
!            cff=visc4_r(i,j)*0.5*Hz(i,j,k)*
            cff=0.5*Hz(i,j,k)*
     &          (pmon_r(i,j)*
     &           ((pn(i  ,j)+pn(i+1,j))*u(i+1,j,k,nstp)-
     &            (pn(i-1,j)+pn(i  ,j))*u(i  ,j,k,nstp))-
     &           pnom_r(i,j)*
     &           ((pm(i,j  )+pm(i,j+1))*v(i,j+1,k,nstp)-
     &            (pm(i,j-1)+pm(i,j  ))*v(i,j  ,k,nstp)))
            UFx(i,j)=on_r(i,j)*on_r(i,j)*cff
            VFe(i,j)=om_r(i,j)*om_r(i,j)*cff
          enddo
        enddo
        do j=Jstr,Jend+1
          do i=Istr,Iend+1
!            cff=visc4_p(i,j)*0.125*(Hz(i-1,j  ,k)+Hz(i,j  ,k)+
            cff=0.125*(Hz(i-1,j  ,k)+Hz(i,j  ,k)+
     &                 Hz(i-1,j-1,k)+Hz(i,j-1,k))*
     &          (pmon_p(i,j)*
     &           ((pn(i  ,j-1)+pn(i  ,j))*v(i  ,j,k,nstp)-
     &            (pn(i-1,j-1)+pn(i-1,j))*v(i-1,j,k,nstp))+
     &           pnom_p(i,j)*
     &           ((pm(i-1,j  )+pm(i,j  ))*u(i,j  ,k,nstp)-
     &            (pm(i-1,j-1)+pm(i,j-1))*u(i,j-1,k,nstp)))
# ifdef MASKING
     &         *pmask(i,j)
# endif /* MASKING */
            UFe(i,j)=om_p(i,j)*om_p(i,j)*cff
            VFx(i,j)=on_p(i,j)*on_p(i,j)*cff
          enddo
        enddo
!
!  Compute first harmonic operator (m s^-3/2).
!
        do j=Jstr,Jend
          do i=IstrU,Iend
            LapU(i,j)=0.25*
     &                (pm(i-1,j)+pm(i,j))*(pn(i-1,j)+pn(i,j))*
     &                ((pn(i-1,j)+pn(i,j))*(UFx(i,j  )-UFx(i-1,j))+
     &                 (pm(i-1,j)+pm(i,j))*(UFe(i,j+1)-UFe(i  ,j)))
     &                 /(Hz(i-1,j,k)+Hz(i,j,k))
          enddo
        enddo
        do j=JstrV,Jend
          do i=Istr,Iend 
            LapV(i,j)=0.25*
     &                (pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))*
     &                ((pn(i,j-1)+pn(i,j))*(VFx(i+1,j)-VFx(i,j  ))-
     &                 (pm(i,j-1)+pm(i,j))*(VFe(i  ,j)-VFe(i,j-1)))
     &                 /(Hz(i-1,j,k)+Hz(i,j,k))
          enddo
        enddo
 
# if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,
     &                         LapU(START_2D_ARRAY))
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,
     &                         LapV(START_2D_ARRAY))
# endif     
!
!  Apply boundary conditions (other than periodic) to the first
!  harmonic operator. These are gradient or closed (free slip or
!  no slip) boundary conditions.
!
# ifndef EW_PERIODIC
        if (WESTERN_EDGE) then
          do j=Jstr-1,Jend+1
#  ifdef WESTERN_WALL
            LapU(IstrU-1,j)=0.0
#  else
            LapU(IstrU-1,j)=LapU(IstrU,j)
#  endif
          enddo
          do j=Jstr-1,Jend+1
#  ifdef WESTERN_WALL
            LapV(Istr-1,j)=gamma2*LapV(Istr,j)
#  else
            LapV(Istr-1,j)=0.0
#  endif
          enddo
        endif
        if (EASTERN_EDGE) then
          do j=Jstr-1,Jend+1
#  ifdef EASTERN_WALL
            LapU(Iend+1,j)=0.0
#  else
            LapU(Iend+1,j)=LapU(Iend,j)
#  endif
          enddo
          do j=Jstr-1,Jend+1
#  ifdef EASTERN_WALL
            LapV(Iend+1,j)=gamma2*LapV(Iend,j)
#  else
            LapV(Iend+1,j)=0.0
#  endif
          enddo
        endif
# endif /* !EW_PERIODIC */
# ifndef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do i=Istr-1,Iend+1
#  ifdef SOUTHERN_WALL
            LapU(i,Jstr-1)=gamma2*LapU(i,Jstr)
#  else
            LapU(i,Jstr-1)=0.0
#  endif
          enddo
          do i=Istr-1,Iend+1
#  ifdef SOUTHERN_WALL
            LapV(i,JstrV-1)=0.0
#  else
            LapV(i,JstrV-1)=LapV(i,JstrV)
#  endif
          enddo
        endif
        if (NORTHERN_EDGE) then
          do i=Istr-1,Iend+1
#  ifdef NORTHERN_WALL
            LapU(i,Jend+1)=gamma2*LapU(i,Jend)
#  else
            LapU(i,Jend+1)=0.0
#  endif
          enddo
          do i=Istr-1,Iend+1
#  ifdef NORTHERN_WALL
            LapV(i,Jend+1)=0.0
#  else
            LapV(i,Jend+1)=LapV(i,Jend)
#  endif
          enddo
        endif
# endif /* !NS_PERIODIC */
# if !defined EW_PERIODIC && !defined NS_PERIODIC
        if (SOUTHERN_EDGE .and. WESTERN_EDGE) then
          LapU(1,0)=0.5*(LapU(2,0)+LapU(1,1))
          LapV(0,1)=0.5*(LapV(0,2)+LapV(1,1))
        endif
        if (SOUTHERN_EDGE .and. EASTERN_EDGE) then
          LapU(Lm+1,0)=0.5*(LapU(Lm,0)+LapU(Lm+1,1))
          LapV(Lm+1,1)=0.5*(LapV(Lm,1)+LapV(Lm+1,2))
        endif
        if (NORTHERN_EDGE .and. WESTERN_EDGE) then
          LapU(1,Mm+1)=0.5*(LapU(2,Mm+1)+LapU(1,Mm))
          LapV(0,Mm+1)=0.5*(LapV(1,Mm+1)+LapV(0,Mm))
        endif
        if (NORTHERN_EDGE .and. EASTERN_EDGE) then
          LapU(Lm+1,Mm+1)=0.5*(LapU(Lm,Mm+1)+LapU(Lm+1,Mm))
          LapV(Lm+1,Mm+1)=0.5*(LapV(Lm,Mm+1)+LapV(Lm+1,Mm))
        endif
# endif
!
!  Compute flux-components of the horizontal divergence of the
!  harmonic stress tensor (m4/s2) in XI- and ETA-directions.
!
        do j=JstrV-1,Jend
          do i=IstrU-1,Iend
            cff=Hz(i,j,k)*0.5*
     &          (pmon_r(i,j)*
     &           ((pn(i  ,j)+pn(i+1,j))*LapU(i+1,j)-
     &            (pn(i-1,j)+pn(i  ,j))*LapU(i  ,j))-
     &           pnom_r(i,j)*
     &           ((pm(i,j  )+pm(i,j+1))*LapV(i,j+1)-
     &            (pm(i,j-1)+pm(i,j  ))*LapV(i,j  )))
# ifdef VIS_COEF_3D
#  ifdef UV_SPLIT_UP3
            UFx(i,j)=on_r(i,j)*on_r(i,j)*viscU_r(i,j,k)*cff
            VFe(i,j)=om_r(i,j)*om_r(i,j)*viscV_r(i,j,k)*cff
#  else
            UFx(i,j)=on_r(i,j)*on_r(i,j)*visc3d_r(i,j,k)*cff
            VFe(i,j)=om_r(i,j)*om_r(i,j)*visc3d_r(i,j,k)*cff
#  endif
# else
            UFx(i,j)=on_r(i,j)*on_r(i,j)*visc4_r(i,j)*cff
            VFe(i,j)=om_r(i,j)*om_r(i,j)*visc4_r(i,j)*cff
# endif
          enddo
        enddo
        do j=Jstr,Jend+1
          do i=Istr,Iend+1
            cff=0.125*(Hz(i-1,j  ,k)+Hz(i,j  ,k)+
     &                 Hz(i-1,j-1,k)+Hz(i,j-1,k))*
     &          (pmon_p(i,j)*
     &           ((pn(i  ,j-1)+pn(i  ,j))*LapV(i  ,j)-
     &            (pn(i-1,j-1)+pn(i-1,j))*LapV(i-1,j))+
     &           pnom_p(i,j)*
     &           ((pm(i-1,j  )+pm(i,j  ))*LapU(i,j  )-
     &            (pm(i-1,j-1)+pm(i,j-1))*LapU(i,j-1)))
# ifdef MASKING
     &         *pmask(i,j)
# endif /* MASKING */
# ifdef VIS_COEF_3D
#  ifdef UV_SPLIT_UP3
            UFe(i,j)=om_p(i,j)*om_p(i,j)*viscU_p(i,j,k)*cff
            VFx(i,j)=on_p(i,j)*on_p(i,j)*viscV_p(i,j,k)*cff
#  else
            UFe(i,j)=om_p(i,j)*om_p(i,j)*visc3d_p(i,j,k)*cff
            VFx(i,j)=on_p(i,j)*on_p(i,j)*visc3d_p(i,j,k)*cff
#  endif
# else
            UFe(i,j)=om_p(i,j)*om_p(i,j)*visc4_p(i,j)*cff
            VFx(i,j)=on_p(i,j)*on_p(i,j)*visc4_p(i,j)*cff
# endif
          enddo
        enddo
!
! Time-step biharmonic, S-surfaces viscosity term.  Notice that
! momentum at this stage is HzU and HzV and has units m2/s.
!
        do j=Jstr,Jend
          do i=IstrU,Iend
            cff=0.125*(pm(i-1,j)+pm(i,j))*(pn(i-1,j) +pn(i,j))*
     &                    ((pn(i-1,j)+pn(i,j))*(UFx(i,j  )-UFx(i-1,j))+
     &                     (pm(i-1,j)+pm(i,j))*(UFe(i,j+1)-UFe(i  ,j)))
            u(i,j,k,indx)=u(i,j,k,indx)-dt*cff
# ifdef DIAGNOSTICS_UV
!           MHmix(i,j,k,1) = cff*om_u(i,j)*on_u(i,j)
            MHmix(i,j,k,1) = cff/(0.5*(Hz(i-1,j,k)+Hz(i,j,k)))
# endif
          enddo
        enddo
        do j=JstrV,Jend
          do i=Istr,Iend
            cff=0.125*(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))*
     &                    ((pn(i,j-1)+pn(i,j))*(VFx(i+1,j)-VFx(i,j  ))-
     &                     (pm(i,j-1)+pm(i,j))*(VFe(i  ,j)-VFe(i,j-1)))
            v(i,j,k,indx)=v(i,j,k,indx)-dt*cff
# ifdef DIAGNOSTICS_UV
!           MHmix(i,j,k,2) = cff*om_v(i,j)*on_v(i,j)
            MHmix(i,j,k,2) = cff/(0.5*(Hz(i,j-1,k)+Hz(i,j,k)))
# endif
          enddo
        enddo
      enddo

      return
      end


