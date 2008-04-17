#include "cppdefs.h"
      subroutine uv3dmix (tile)
!
!=====================================================================
!  Copyright (c) 2000 Rutgers/UCLA                                   !
!================================================ Hernan G. Arango ===
!                                                                    !
!  This subroutine computes  biharmonic mixing of momentum,  rotated !
!  along geopotentials, from the horizontal divergence of the stress !
!  tensor.  A transverse isotropy is assumed so the stress tensor is !
!  splitted into vertical and horizontal subtensors.                 !
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
      trd=omp_get_thread_num()
      call uv3dmix_tile (Istr,Iend,Jstr,Jend,
     &                      A3d(1, 1,trd),A3d(1, 2,trd),A2d(1, 1,trd),
     &                      A2d(1, 2,trd),A2d(1, 3,trd),A2d(1, 4,trd),
     &                      A2d(1, 5,trd),A2d(1, 7,trd),A2d(1, 9,trd),
     &                      A2d(1,11,trd),A2d(1,13,trd),A2d(1,15,trd),
     &                      A2d(1,17,trd),A2d(1,19,trd),A2d(1,21,trd),
     &                      A2d(1,23,trd),A2d(1,25,trd),A2d(1,27,trd))
      return
      end
!
!*********************************************************************
      subroutine uv3dmix_tile (Istr,Iend,Jstr,Jend,LapU,LapV,UFx,
     &                              UFe,VFx,VFe,UFs,VFs,dnUdx,dmUde,
     &                              dUdz,dnVdx,dmVde,dVdz,dZdx_r,
     &                              dZdx_p,dZde_r,dZde_p)
!*********************************************************************
!
      implicit none
# include "param.h"
!
      integer   Iend, Istr, Jend, Jstr, i, j, k, k1, k2
      real      cff, cff1, cff2, cff3, cff4, cff5, cff6, cff7, cff8,
     &          cff_vis,
     &          dmUdz, dnUdz, dmVdz, dnVdz
      real      LapU(GLOBAL_2D_ARRAY,0:N),
     &          LapV(GLOBAL_2D_ARRAY,0:N),
     &           UFe(PRIVATE_2D_SCRATCH_ARRAY),
     &           VFe(PRIVATE_2D_SCRATCH_ARRAY),
     &           UFx(PRIVATE_2D_SCRATCH_ARRAY),
     &           VFx(PRIVATE_2D_SCRATCH_ARRAY),
     &           UFs(PRIVATE_2D_SCRATCH_ARRAY,2),
     &           VFs(PRIVATE_2D_SCRATCH_ARRAY,2),
     &         dmUde(PRIVATE_2D_SCRATCH_ARRAY,2),
     &         dmVde(PRIVATE_2D_SCRATCH_ARRAY,2),
     &         dnUdx(PRIVATE_2D_SCRATCH_ARRAY,2),
     &         dnVdx(PRIVATE_2D_SCRATCH_ARRAY,2),
     &          dUdz(PRIVATE_2D_SCRATCH_ARRAY,2),
     &          dVdz(PRIVATE_2D_SCRATCH_ARRAY,2),
     &        dZde_p(PRIVATE_2D_SCRATCH_ARRAY,2),
     &        dZde_r(PRIVATE_2D_SCRATCH_ARRAY,2),
     &        dZdx_p(PRIVATE_2D_SCRATCH_ARRAY,2),
     &        dZdx_r(PRIVATE_2D_SCRATCH_ARRAY,2)
!
# include "grid.h"
# include "mixing.h"
# include "ocean3d.h"
# include "scalars.h"
# include "compute_auxiliary_bounds.h"

!
!--------------------------------------------------------------------
!  Compute horizontal biharmonic viscosity along geopotential
!  surfaces.  The biharmonic operator is computed by applying
!  the harmonic operator twice.
!--------------------------------------------------------------------
!
!  Compute horizontal and vertical gradients.  Notice the recursive
!  blocking sequence.
!  For momentum balance purposes, the thickness "Hz" appears only when
!  computing the second harmonic operator. The vertical placement of
!  the gradients is:
!
!    dZdx_r, dZde_r, dnUdx, dmVde(:,:,k1) k      rho-points
!    dZdx_r, dZde_r, dnUdx, dmVde(:,:,k2) k+1    rho-points
!    dZdx_p, dZde_p, dnVdx, dmUde(:,:,k1) k      psi-points
!    dZdx_p, dZde_p, dnVdx, dmUde(:,:,k2) k+1    psi-points
!                       UFs, dUdz(:,:,k1) k-1/2  WU-points
!                       UFs, dUdz(:,:,k2) k+1/2  WU-points
!                       VFs, dVdz(:,:,k1) k-1/2  WV-points
!                       VFs, dVdz(:,:,k2) k+1/2  WV-points
!
      k2=1
      do k=0,N
        k1=k2
        k2=3-k1
        if (k.lt.N) then
!
!  Compute slopes (nondimensional) at RHO- and PSI-points.
!
          do j=Jstr-1,Jend+1
            do i=IstrU-1,Iend+1
              UFx(i,j)=0.5*(pm(i-1,j)+pm(i,j))*
     &                 (z_r(i,j,k+1)-z_r(i-1,j,k+1))
# ifdef MASKING
     &                *umask(i,j)
# endif
            enddo
          enddo
          do j=JstrV-1,Jend+1
            do i=Istr-1,Iend+1
              VFe(i,j)=0.5*(pn(i,j-1)+pn(i,j))*
     &                 (z_r(i,j,k+1)-z_r(i,j-1,k+1))
# ifdef MASKING
     &                *vmask(i,j)
# endif
            enddo
          enddo
!
!  Compute momentum horizontal (1/m/s) and vertical (1/s) gradients.
!
          do j=JstrV-1,Jend
            do i=IstrU-1,Iend
              dnUdx(i,j,k2)=0.5*pm(i,j)*
     &                      ((pn(i  ,j)+pn(i+1,j))*u(i+1,j,k+1,nrhs)-
     &                       (pn(i-1,j)+pn(i  ,j))*u(i  ,j,k+1,nrhs))
# ifdef MASKING
     &                     *rmask(i,j)
# endif
              dmVde(i,j,k2)=0.5*pn(i,j)*
     &                      ((pm(i,j  )+pm(i,j+1))*v(i,j+1,k+1,nrhs)-
     &                       (pm(i,j-1)+pm(i,j  ))*v(i,j  ,k+1,nrhs))
# ifdef MASKING
     &                     *rmask(i,j)
# endif
              dZdx_r(i,j,k2)=0.5*(UFx(i,j)+UFx(i+1,j))
              dZde_r(i,j,k2)=0.5*(VFe(i,j)+VFe(i,j+1))
            enddo
          enddo
          do j=Jstr,Jend+1
            do i=Istr,Iend+1
              dmUde(i,j,k2)=0.125*(pn(i-1,j  )+pn(i,j  )+
     &                             pn(i-1,j-1)+pn(i,j-1))*
     &                      ((pm(i-1,j  )+pm(i,j  ))*u(i,j  ,k+1,nrhs)-
     &                       (pm(i-1,j-1)+pm(i,j-1))*u(i,j-1,k+1,nrhs))
# ifdef MASKING
     &                     *pmask(i,j)
# endif
              dnVdx(i,j,k2)=0.125*(pm(i-1,j  )+pm(i,j  )+
     &                             pm(i-1,j-1)+pm(i,j-1))*
     &                      ((pn(i  ,j-1)+pn(i  ,j))*v(i  ,j,k+1,nrhs)-
     &                       (pn(i-1,j-1)+pn(i-1,j))*v(i-1,j,k+1,nrhs))
# ifdef MASKING
     &                     *pmask(i,j)
# endif
              dZde_p(i,j,k2)=0.5*(VFe(i-1,j)+VFe(i,j))
              dZdx_p(i,j,k2)=0.5*(UFx(i,j-1)+UFx(i,j))
            enddo
          enddo
        endif
        if ((k.eq.0).or.(k.eq.N)) then
          do j=Jstr-1,Jend+1
            do i=IstrU-1,Iend+1
              dUdz(i,j,k2)=0.0
              UFs(i,j,k2)=0.0
            enddo
          enddo
          do j=JstrV-1,Jend+1
            do i=Istr-1,Iend+1
              dVdz(i,j,k2)=0.0
              VFs(i,j,k2)=0.0
            enddo
          enddo
        else
          do j=Jstr-1,Jend+1
            do i=IstrU-1,Iend+1
              dUdz(i,j,k2)=(u(i,j,k+1,nrhs)-u(i,j,k,nrhs))/
     &                     (0.5*(z_r(i-1,j,k+1)-z_r(i-1,j,k)+
     &                              z_r(i  ,j,k+1)-z_r(i  ,j,k)))
            enddo
          enddo
          do j=JstrV-1,Jend+1
            do i=Istr-1,Iend+1
              dVdz(i,j,k2)=(v(i,j,k+1,nrhs)-v(i,j,k,nrhs))/
     &                     (0.5*(z_r(i,j-1,k+1)-z_r(i,j-1,k)+
     &                              z_r(i,j  ,k+1)-z_r(i,j  ,k)))
            enddo
          enddo
        endif
!
!  Compute components of the rotated viscous flux (m^4 s-^3/2) along
!  geopotential surfaces in the XI- and ETA-directions.
!
        if (k.gt.0) then
          do j=JstrV-1,Jend
            do i=IstrU-1,Iend
              cff= !visc4_r(i,j)*
     &            (on_r(i,j)*(dnUdx(i,j,k1)-0.5*pn(i,j)*
     &                        (MIN(dZdx_r(i,j,k1),0.0)*
     &                             (dUdz(i,j,k1)+dUdz(i+1,j,k2))+
     &                         MAX(dZdx_r(i,j,k1),0.0)*
     &                             (dUdz(i,j,k2)+dUdz(i+1,j,k1))))-
     &             om_r(i,j)*(dmVde(i,j,k1)-0.5*pm(i,j)*
     &                        (MIN(dZde_r(i,j,k1),0.0)*
     &                             (dVdz(i,j,k1)+dVdz(i,j+1,k2))+
     &                         MAX(dZde_r(i,j,k1),0.0)*
     &                             (dVdz(i,j,k2)+dVdz(i,j+1,k1)))))
# ifdef MASKING
     &           *rmask(i,j)
# endif
              UFx(i,j)=on_r(i,j)*on_r(i,j)*cff
              VFe(i,j)=om_r(i,j)*om_r(i,j)*cff
            enddo
          enddo
          do j=Jstr,Jend+1
            do i=Istr,Iend+1
              cff= !visc4_p(i,j)*
     &            (on_p(i,j)*(dnVdx(i,j,k1)-
     &                        0.125*(pn(i-1,j-1)+pn(i-1,j)+
     &                                  pn(i  ,j-1)+pn(i  ,j))*
     &                        (MIN(dZdx_p(i,j,k1),0.0)*
     &                             (dVdz(i-1,j,k1)+dVdz(i,j,k2))+
     &                         MAX(dZdx_p(i,j,k1),0.0)*
     &                             (dVdz(i-1,j,k2)+dVdz(i,j,k1))))+
     &             om_p(i,j)*(dmUde(i,j,k1)-
     &                        0.125*(pm(i-1,j-1)+pm(i-1,j)+
     &                                  pm(i  ,j-1)+pm(i  ,j))*
     &                        (MIN(dZde_p(i,j,k1),0.0)*
     &                             (dUdz(i,j-1,k1)+dUdz(i,j,k2))+
     &                         MAX(dZde_p(i,j,k1),0.0)*
     &                             (dUdz(i,j-1,k2)+dUdz(i,j,k1)))))
# ifdef MASKING
     &           *pmask(i,j)
# endif
              UFe(i,j)=om_p(i,j)*om_p(i,j)*cff
              VFx(i,j)=on_p(i,j)*on_p(i,j)*cff
            enddo
          enddo
!
!  Compute vertical flux (m^2 s^-3/2) due to sloping terrain-following
!  surfaces.
!
          if (k.lt.N) then
            do j=Jstr,Jend
              do i=IstrU,Iend
                cff=0.5*(pn(i-1,j)+pn(i,j))
                dnUdz=cff*dUdz(i,j,k2)
                dnVdz=cff*0.25*(dVdz(i-1,j+1,k2)+dVdz(i,j+1,k2)+
     &                             dVdz(i-1,j  ,k2)+dVdz(i,j  ,k2))
                cff=0.5*(pm(i-1,j)+pm(i,j))
                dmUdz=cff*dUdz(i,j,k2)
                dmVdz=cff*0.25*(dVdz(i-1,j+1,k2)+dVdz(i,j+1,k2)+
     &                             dVdz(i-1,j  ,k2)+dVdz(i,j  ,k2))
                cff1=MIN(dZdx_r(i-1,j,k1),0.0)
                cff2=MIN(dZdx_r(i  ,j,k2),0.0)
                cff3=MAX(dZdx_r(i-1,j,k2),0.0)
                cff4=MAX(dZdx_r(i  ,j,k1),0.0)
                UFs(i,j,k2)=on_u(i,j)*0.5*
!     &                      0.25*(visc4_r(i-1,j)+visc4_r(i,j))*
     &                      (cff1*(cff1*dnUdz-dnUdx(i-1,j,k1))+
     &                       cff2*(cff2*dnUdz-dnUdx(i  ,j,k2))+
     &                       cff3*(cff3*dnUdz-dnUdx(i-1,j,k2))+
     &                       cff4*(cff4*dnUdz-dnUdx(i  ,j,k1)))
                cff1=MIN(dZde_p(i,j  ,k1),0.0)
                cff2=MIN(dZde_p(i,j+1,k2),0.0)
                cff3=MAX(dZde_p(i,j  ,k2),0.0)
                cff4=MAX(dZde_p(i,j+1,k1),0.0)
                UFs(i,j,k2)=UFs(i,j,k2)+om_u(i,j)*0.5*
!     &                      0.25*(visc4_r(i-1,j)+visc4_r(i,j))*
     &                      (cff1*(cff1*dmUdz-dmUde(i,j  ,k1))+
     &                       cff2*(cff2*dmUdz-dmUde(i,j+1,k2))+
     &                       cff3*(cff3*dmUdz-dmUde(i,j  ,k2))+
     &                       cff4*(cff4*dmUdz-dmUde(i,j+1,k1)))
                cff1=MIN(dZde_p(i,j  ,k1),0.0)
                cff2=MIN(dZde_p(i,j+1,k2),0.0)
                cff3=MAX(dZde_p(i,j  ,k2),0.0)
                cff4=MAX(dZde_p(i,j+1,k1),0.0)
                cff5=MIN(dZdx_p(i,j  ,k1),0.0)
                cff6=MIN(dZdx_p(i,j+1,k2),0.0)
                cff7=MAX(dZdx_p(i,j  ,k2),0.0)
                cff8=MAX(dZdx_p(i,j+1,k1),0.0)
                UFs(i,j,k2)=UFs(i,j,k2)+on_u(i,j)*0.5*
!     &                      0.25*(visc4_r(i-1,j)+visc4_r(i,j))*
     &                      (cff1*(cff5*dnVdz-dnVdx(i,j  ,k1))+
     &                       cff2*(cff6*dnVdz-dnVdx(i,j+1,k2))+
     &                       cff3*(cff7*dnVdz-dnVdx(i,j  ,k2))+
     &                       cff4*(cff8*dnVdz-dnVdx(i,j+1,k1)))
                cff1=MIN(dZdx_r(i-1,j,k1),0.0)
                cff2=MIN(dZdx_r(i  ,j,k2),0.0)
                cff3=MAX(dZdx_r(i-1,j,k2),0.0)
                cff4=MAX(dZdx_r(i  ,j,k1),0.0)
                cff5=MIN(dZde_r(i-1,j,k1),0.0)
                cff6=MIN(dZde_r(i  ,j,k2),0.0)
                cff7=MAX(dZde_r(i-1,j,k2),0.0)
                cff8=MAX(dZde_r(i  ,j,k1),0.0)
                UFs(i,j,k2)=UFs(i,j,k2)-om_u(i,j)*0.5*
!     &                      0.25*(visc4_r(i-1,j)+visc4_r(i,j))*
     &                      (cff1*(cff5*dmVdz-dmVde(i-1,j,k1))+
     &                       cff2*(cff6*dmVdz-dmVde(i  ,j,k2))+
     &                       cff3*(cff7*dmVdz-dmVde(i-1,j,k2))+
     &                       cff4*(cff8*dmVdz-dmVde(i  ,j,k1)))
              enddo
            enddo
!
            do j=JstrV,Jend
              do i=Istr,Iend
                cff=0.5*(pn(i,j-1)+pn(i,j))
                dnUdz=cff*0.25*(dUdz(i,j  ,k2)+dUdz(i+1,j  ,k2)+
     &                             dUdz(i,j-1,k2)+dUdz(i+1,j-1,k2))
                dnVdz=cff*dUdz(i,j,k2)
                cff=0.5*(pm(i,j-1)+pm(i,j))
                dmUdz=cff*0.25*(dUdz(i,j  ,k2)+dUdz(i+1,j  ,k2)+
     &                             dUdz(i,j-1,k2)+dUdz(i+1,j-1,k2))
                dmVdz=cff*dUdz(i,j,k2)
                cff1=MIN(dZdx_p(i  ,j,k1),0.0)
                cff2=MIN(dZdx_p(i+1,j,k2),0.0)
                cff3=MAX(dZdx_p(i  ,j,k2),0.0)
                cff4=MAX(dZdx_p(i+1,j,k1),0.0)
                VFs(i,j,k2)=on_v(i,j)*0.5*
!     &                      0.25*(visc4_r(i,j-1)+visc4_r(i,j))*
     &                      (cff1*(cff1*dnVdz-dnVdx(i  ,j,k1))+
     &                       cff2*(cff2*dnVdz-dnVdx(i+1,j,k2))+
     &                       cff3*(cff3*dnVdz-dnVdx(i  ,j,k2))+
     &                       cff4*(cff4*dnVdz-dnVdx(i+1,j,k1)))
                cff1=MIN(dZde_r(i,j-1,k1),0.0)
                cff2=MIN(dZde_r(i,j  ,k2),0.0)
                cff3=MAX(dZde_r(i,j-1,k2),0.0)
                cff4=MAX(dZde_r(i,j  ,k1),0.0)
                VFs(i,j,k2)=VFs(i,j,k2)+om_v(i,j)*0.5*
!     &                      0.25*(visc4_r(i,j-1)+visc4_r(i,j))*
     &                      (cff1*(cff1*dmVdz-dmVde(i,j-1,k1))+
     &                       cff2*(cff2*dmVdz-dmVde(i,j  ,k2))+
     &                       cff3*(cff3*dmVdz-dmVde(i,j-1,k2))+
     &                       cff4*(cff4*dmVdz-dmVde(i,j  ,k1)))
                cff1=MIN(dZde_r(i,j-1,k1),0.0)
                cff2=MIN(dZde_r(i,j  ,k2),0.0)
                cff3=MAX(dZde_r(i,j-1,k2),0.0)
                cff4=MAX(dZde_r(i,j  ,k1),0.0)
                cff5=MIN(dZdx_r(i,j-1,k1),0.0)
                cff6=MIN(dZdx_r(i,j  ,k2),0.0)
                cff7=MAX(dZdx_r(i,j-1,k2),0.0)
                cff8=MAX(dZdx_r(i,j  ,k1),0.0)
                VFs(i,j,k2)=VFs(i,j,k2)-on_v(i,j)*0.5*
!     &                      0.25*(visc4_r(i,j-1)+visc4_r(i,j))*
     &                      (cff1*(cff5*dnUdz-dnUdx(i,j-1,k1))+
     &                       cff2*(cff6*dnUdz-dnUdx(i,j  ,k2))+
     &                       cff3*(cff7*dnUdz-dnUdx(i,j-1,k2))+
     &                       cff4*(cff8*dnUdz-dnUdx(i,j  ,k1)))
                cff1=MIN(dZdx_p(i  ,j,k1),0.0)
                cff2=MIN(dZdx_p(i+1,j,k2),0.0)
                cff3=MAX(dZdx_p(i  ,j,k2),0.0)
                cff4=MAX(dZdx_p(i+1,j,k1),0.0)
                cff5=MIN(dZde_p(i  ,j,k1),0.0)
                cff6=MIN(dZde_p(i+1,j,k2),0.0)
                cff7=MAX(dZde_p(i  ,j,k2),0.0)
                cff8=MAX(dZde_p(i+1,j,k1),0.0)
                VFs(i,j,k2)=VFs(i,j,k2)+om_v(i,j)*0.5*
!     &                      0.25*(visc4_r(i,j-1)+visc4_r(i,j))*
     &                      (cff1*(cff5*dmUdz-dmUde(i  ,j,k1))+
     &                       cff2*(cff6*dmUdz-dmUde(i+1,j,k2))+
     &                       cff3*(cff7*dmUdz-dmUde(i  ,j,k2))+
     &                       cff4*(cff8*dmUdz-dmUde(i+1,j,k1)))
              enddo
            enddo
          endif
!
! Compute first harmonic operator (m s^-3/2).
!
          do j=Jstr,Jend
            do i=IstrU,Iend
              LapU(i,j,k)=(0.25*
     &                     (pm(i-1,j)+pm(i,j))*(pn(i-1,j)+pn(i,j))*
     &                     ((pn(i-1,j)+pn(i,j))*(UFx(i  ,j)-
     &                                           UFx(i-1,j))+
     &                      (pm(i-1,j)+pm(i,j))*(UFe(i,j+1)-
     &                                           UFe(i,j  )))+
     &                     (UFs(i,j,k2)-UFs(i,j,k1))/
     &                     (0.5*(Hz(i-1,j,k)+Hz(i,j,k))))
# ifdef MASKING
     &                   *umask(i,j)
# endif
            enddo
          enddo
          do j=JstrV,Jend
            do i=Istr,Iend
              LapV(i,j,k)=(0.25*
     &                     (pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))*
     &                     ((pn(i,j-1)+pn(i,j))*(VFx(i+1,j)-
     &                                           VFx(i  ,j))-
     &                      (pm(i,j-1)+pm(i,j))*(VFe(i,j  )-
     &                                           VFe(i,j-1)))+
     &                     (VFs(i,j,k2)-VFs(i,j,k1))/
     &                     (0.5*(Hz(i,j-1,k)+Hz(i,j,k))))
# ifdef MASKING
     &                   *vmask(i,j)
# endif
            enddo
          enddo
        endif
      enddo
            
#  if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend,
     &                         LapU(START_2D_ARRAY,1))
      call exchange_v3d_tile (Istr,Iend,Jstr,Jend,
     &                         LapV(START_2D_ARRAY,1))
#endif
          
!
!  Apply boundary conditions (except periodic; closed or gradient)
!  to the first harmonic operator.
!
# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
        do k=1,N
          do j=Jstr-1,Jend+1
#  ifdef WESTERN_WALL
            LapU(IstrU-1,j,k)=0.0
#  else
            LapU(IstrU-1,j,k)=LapU(IstrU,j,k)
#  endif
          enddo
          do j=Jstr-1,Jend+1
#  ifdef WESTERN_WALL
            LapV(Istr-1,j,k)=gamma2*LapV(Istr,j,k)
#  else
            LapV(Istr-1,j,k)=0.0
#  endif
          enddo
        enddo
      endif
      if (EASTERN_EDGE) then
        do k=1,N
          do j=Jstr-1,Jend+1
#  ifdef EASTERN_WALL
            LapU(Iend+1,j,k)=0.0
#  else
            LapU(Iend+1,j,k)=LapU(Iend,j,k)
#  endif
          enddo
          do j=Jstr-1,Jend+1
#  ifdef EASTERN_WALL
            LapV(Iend+1,j,k)=gamma2*LapV(Iend,j,k)
#  else
            LapV(Iend+1,j,k)=0.0
#  endif
          enddo
        enddo
      endif
# endif /* !EW_PERIODIC */
# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        do k=1,N
          do i=Istr-1,Iend+1
#  ifdef SOUTHERN_WALL
            LapU(i,Jstr-1,k)=gamma2*LapU(i,Jstr,k)
#  else
            LapU(i,Jstr-1,k)=0.0
#  endif
          enddo
          do i=Istr-1,Iend+1
#  ifdef SOUTHERN_WALL
            LapV(i,JstrV-1,k)=0.0
#  else
            LapV(i,JstrV-1,k)=LapV(i,JstrV,k)
#  endif
          enddo
        enddo
      endif
      if (NORTHERN_EDGE) then
        do k=1,N
          do i=Istr-1,Iend+1
#  ifdef NORTHERN_WALL
            LapU(i,Jend+1,k)=gamma2*LapU(i,Jend,k)
#  else
            LapU(i,Jend+1,k)=0.0
#  endif
          enddo
          do i=Istr-1,Iend+1
#  ifdef NORTHERN_WALL
            LapV(i,Jend+1,k)=0.0
#  else
            LapV(i,Jend+1,k)=LapV(i,Jend,k)
#  endif
          enddo
        enddo
      endif
# endif /* !NS_PERIODIC */
# if !defined EW_PERIODIC && !defined NS_PERIODIC
      if (SOUTHERN_EDGE .and. WESTERN_EDGE) then
        do k=1,N
          LapU(1,0,k)=0.5*(LapU(2,0,k)+LapU(1,1,k))
          LapV(0,1,k)=0.5*(LapV(0,2,k)+LapV(1,1,k))
        enddo
      endif
      if (SOUTHERN_EDGE .and. EASTERN_EDGE) then
        do k=1,N
          LapU(Lm+1,0,k)=0.5*(LapU(Lm,0,k)+LapU(Lm+1,1,k))
          LapV(Lm+1,1,k)=0.5*(LapV(Lm,1,k)+LapV(Lm+1,2,k))
        enddo
      endif
      if (NORTHERN_EDGE .and. WESTERN_EDGE) then
        do k=1,N
          LapU(1,Mm+1,k)=0.5*(LapU(2,Mm+1,k)+LapU(1,Mm,k))
          LapV(0,Mm+1,k)=0.5*(LapV(1,Mm+1,k)+LapV(0,Mm,k))
        enddo
      endif
      if (NORTHERN_EDGE .and. EASTERN_EDGE) then
        do k=1,N
          LapU(Lm+1,Mm+1,k)=0.5*(LapU(Lm,Mm+1,k)+LapU(Lm+1,Mm,k))
          LapV(Lm+1,Mm+1,k)=0.5*(LapV(Lm,Mm+1,k)+LapV(Lm+1,Mm,k))
        enddo
      endif
# endif /* !EW_PERIODIC && !NS_PERIODIC */
!
!  Compute horizontal and vertical gradients associated with the
!  second rotated harmonic operator.
!
      k2=1
      do k=0,N
        k1=k2
        k2=3-k1
        if (k.lt.N) then
!
!  Compute slopes (nondimensional) at RHO- and PSI-points.
!
          do j=Jstr-1,Jend+1
            do i=IstrU-1,Iend+1
              UFx(i,j)=0.5*(pm(i-1,j)+pm(i,j))*
     &                 (z_r(i,j,k+1)-z_r(i-1,j,k+1))
# ifdef MASKING
     &                *umask(i,j)
# endif
            enddo
          enddo
          do j=JstrV-1,Jend+1
            do i=Istr-1,Iend+1
              VFe(i,j)=0.5*(pn(i,j-1)+pn(i,j))*
     &                 (z_r(i,j,k+1)-z_r(i,j-1,k+1))
# ifdef MASKING
     &                *vmask(i,j)
# endif
            enddo
          enddo
!
!  Compute momentum horizontal (m^-1 s^-3/2) and vertical (s^-3/2)
!  gradients.
!
          do j=JstrV-1,Jend
            do i=IstrU-1,Iend
              dnUdx(i,j,k2)=0.5*pm(i,j)*
     &                      ((pn(i  ,j)+pn(i+1,j))*LapU(i+1,j,k+1)-
     &                       (pn(i-1,j)+pn(i  ,j))*LapU(i  ,j,k+1)) 
# ifdef MASKING
     &                     *rmask(i,j)
# endif
              dmVde(i,j,k2)=0.5*pn(i,j)*
     &                      ((pm(i,j  )+pm(i,j+1))*LapV(i,j+1,k+1)-
     &                       (pm(i,j-1)+pm(i,j  ))*LapV(i,j  ,k+1)) 
# ifdef MASKING
     &                     *rmask(i,j)
# endif
              dZdx_r(i,j,k2)=0.5*(UFx(i,j)+UFx(i+1,j))
              dZde_r(i,j,k2)=0.5*(VFe(i,j)+VFe(i,j+1))

            enddo
          enddo
          do j=Jstr,Jend+1
            do i=Istr,Iend+1
              dmUde(i,j,k2)=0.125*(pn(i-1,j  )+pn(i,j  )+
     &                                pn(i-1,j-1)+pn(i,j-1))*
     &                      ((pm(i-1,j  )+pm(i,j  ))*LapU(i,j  ,k+1)-
     &                       (pm(i-1,j-1)+pm(i,j-1))*LapU(i,j-1,k+1))
# ifdef MASKING
     &                     *pmask(i,j)
# endif
              dnVdx(i,j,k2)=0.125*(pm(i-1,j  )+pm(i,j  )+
     &                                pm(i-1,j-1)+pm(i,j-1))*
     &                      ((pn(i  ,j-1)+pn(i  ,j))*LapV(i  ,j,k+1)-
     &                       (pn(i-1,j-1)+pn(i-1,j))*LapV(i-1,j,k+1))
# ifdef MASKING
     &                     *pmask(i,j)
# endif
              dZde_p(i,j,k2)=0.5*(VFe(i-1,j)+VFe(i,j))
              dZdx_p(i,j,k2)=0.5*(UFx(i,j-1)+UFx(i,j))
            enddo
          enddo
        endif
        if ((k.eq.0).or.(k.eq.N)) then
          do j=Jstr-1,Jend+1
            do i=IstrU-1,Iend+1
              dUdz(i,j,k2)=0.0
              UFs(i,j,k2)=0.0
            enddo
          enddo
          do j=JstrV-1,Jend+1
            do i=Istr-1,Iend+1
              dVdz(i,j,k2)=0.0
              VFs(i,j,k2)=0.0
            enddo
          enddo
        else
          do j=Jstr-1,Jend+1
            do i=IstrU-1,Iend+1
              dUdz(i,j,k2)=(LapU(i,j,k+1)-LapU(i,j,k))/
     &                     (0.5*(z_r(i-1,j,k+1)-z_r(i-1,j,k)+
     &                              z_r(i  ,j,k+1)-z_r(i  ,j,k)))
            enddo
          enddo
          do j=JstrV-1,Jend+1
            do i=Istr-1,Iend+1
              dVdz(i,j,k2)=(LapV(i,j,k+1)-LapV(i,j,k))/
     &                     (0.5*(z_r(i,j-1,k+1)-z_r(i,j-1,k)+
     &                              z_r(i,j  ,k+1)-z_r(i,j  ,k)))
            enddo
          enddo
        endif
!
!  Compute components of the rotated viscous flux (m5/s2) along
!  geopotential surfaces in the XI- and ETA-directions.
!
        if (k.gt.0) then
          do j=JstrV-1,Jend
            do i=IstrU-1,Iend
!              cff=visc4_r(i,j)*Hz(i,j,k)*
              cff=Hz(i,j,k)*
     &            (on_r(i,j)*(dnUdx(i,j,k1)-0.5*pn(i,j)*
     &                        (MIN(dZdx_r(i,j,k1),0.0)*
     &                             (dUdz(i,j,k1)+dUdz(i+1,j,k2))+
     &                         MAX(dZdx_r(i,j,k1),0.0)*
     &                             (dUdz(i,j,k2)+dUdz(i+1,j,k1))))-
     &             om_r(i,j)*(dmVde(i,j,k1)-0.5*pm(i,j)*
     &                        (MIN(dZde_r(i,j,k1),0.0)*
     &                             (dVdz(i,j,k1)+dVdz(i,j+1,k2))+
     &                         MAX(dZde_r(i,j,k1),0.0)*
     &                             (dVdz(i,j,k2)+dVdz(i,j+1,k1)))))
# ifdef MASKING
     &           *rmask(i,j)
# endif
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
!              cff=visc4_p(i,j)*0.25*(Hz(i-1,j  ,k)+Hz(i,j  ,k)+
              cff=0.25*(Hz(i-1,j  ,k)+Hz(i,j  ,k)+
     &                  Hz(i-1,j-1,k)+Hz(i,j-1,k))*
     &            (on_p(i,j)*(dnVdx(i,j,k1)-
     &                        0.125*(pn(i-1,j-1)+pn(i-1,j)+
     &                                  pn(i  ,j-1)+pn(i  ,j))*
     &                        (MIN(dZdx_p(i,j,k1),0.0)*
     &                             (dVdz(i-1,j,k1)+dVdz(i,j,k2))+
     &                         MAX(dZdx_p(i,j,k1),0.0)*
     &                             (dVdz(i-1,j,k2)+dVdz(i,j,k1))))+
     &             om_p(i,j)*(dmUde(i,j,k1)-
     &                        0.125*(pm(i-1,j-1)+pm(i-1,j)+
     &                                  pm(i  ,j-1)+pm(i  ,j))*
     &                        (MIN(dZde_p(i,j,k1),0.0)*
     &                             (dUdz(i,j-1,k1)+dUdz(i,j,k2))+
     &                         MAX(dZde_p(i,j,k1),0.0)*
     &                             (dUdz(i,j-1,k2)+dUdz(i,j,k1)))))
# ifdef MASKING
     &           *pmask(i,j)
# endif
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
!  Compute vertical flux (m2/s2) due to sloping terrain-following
!  surfaces.
!
          if (k.lt.N) then
            do j=Jstr,Jend
              do i=IstrU,Iend
                cff=0.5*(pn(i-1,j)+pn(i,j))
                dnUdz=cff*dUdz(i,j,k2)
                dnVdz=cff*0.25*(dVdz(i-1,j+1,k2)+dVdz(i,j+1,k2)+
     &                             dVdz(i-1,j  ,k2)+dVdz(i,j  ,k2))
                cff=0.5*(pm(i-1,j)+pm(i,j))
                dmUdz=cff*dUdz(i,j,k2)
                dmVdz=cff*0.25*(dVdz(i-1,j+1,k2)+dVdz(i,j+1,k2)+
     &                             dVdz(i-1,j  ,k2)+dVdz(i,j  ,k2))
                cff1=MIN(dZdx_r(i-1,j,k1),0.0)
                cff2=MIN(dZdx_r(i  ,j,k2),0.0)
                cff3=MAX(dZdx_r(i-1,j,k2),0.0)
                cff4=MAX(dZdx_r(i  ,j,k1),0.0)
# ifdef VIS_COEF_3D
#  ifdef UV_SPLIT_UP3
                cff_vis=0.25*(viscU_r(i-1,j,k  )+viscU_r(i,j,k  )+
     &                        viscU_r(i-1,j,k+1)+viscU_r(i,j,k+1))
#  else
                cff_vis=0.25*(visc3d_r(i-1,j,k  )+visc3d_r(i,j,k  )+
     &                        visc3d_r(i-1,j,k+1)+visc3d_r(i,j,k+1))
#  endif
# else
                cff_vis=0.5*(visc4_r(i-1,j)+visc4_r(i,j))
# endif

                UFs(i,j,k2)=0.5*cff_vis*on_u(i,j)*
     &                      (cff1*(cff1*dnUdz-dnUdx(i-1,j,k1))+
     &                       cff2*(cff2*dnUdz-dnUdx(i  ,j,k2))+
     &                       cff3*(cff3*dnUdz-dnUdx(i-1,j,k2))+
     &                       cff4*(cff4*dnUdz-dnUdx(i  ,j,k1)))
                cff1=MIN(dZde_p(i,j  ,k1),0.0)
                cff2=MIN(dZde_p(i,j+1,k2),0.0)
                cff3=MAX(dZde_p(i,j  ,k2),0.0)
                cff4=MAX(dZde_p(i,j+1,k1),0.0)
                UFs(i,j,k2)=UFs(i,j,k2)+0.5*cff_vis*om_u(i,j)*
     &                      (cff1*(cff1*dmUdz-dmUde(i,j  ,k1))+
     &                       cff2*(cff2*dmUdz-dmUde(i,j+1,k2))+
     &                       cff3*(cff3*dmUdz-dmUde(i,j  ,k2))+
     &                       cff4*(cff4*dmUdz-dmUde(i,j+1,k1)))
                cff1=MIN(dZde_p(i,j  ,k1),0.0)
                cff2=MIN(dZde_p(i,j+1,k2),0.0)
                cff3=MAX(dZde_p(i,j  ,k2),0.0)
                cff4=MAX(dZde_p(i,j+1,k1),0.0)
                cff5=MIN(dZdx_p(i,j  ,k1),0.0)
                cff6=MIN(dZdx_p(i,j+1,k2),0.0)
                cff7=MAX(dZdx_p(i,j  ,k2),0.0)
                cff8=MAX(dZdx_p(i,j+1,k1),0.0)
                UFs(i,j,k2)=UFs(i,j,k2)+0.5*cff_vis*on_u(i,j)*
     &                      (cff1*(cff5*dnVdz-dnVdx(i,j  ,k1))+
     &                       cff2*(cff6*dnVdz-dnVdx(i,j+1,k2))+
     &                       cff3*(cff7*dnVdz-dnVdx(i,j  ,k2))+
     &                       cff4*(cff8*dnVdz-dnVdx(i,j+1,k1)))
                cff1=MIN(dZdx_r(i-1,j,k1),0.0)
                cff2=MIN(dZdx_r(i  ,j,k2),0.0)
                cff3=MAX(dZdx_r(i-1,j,k2),0.0)
                cff4=MAX(dZdx_r(i  ,j,k1),0.0)
                cff5=MIN(dZde_r(i-1,j,k1),0.0)
                cff6=MIN(dZde_r(i  ,j,k2),0.0)
                cff7=MAX(dZde_r(i-1,j,k2),0.0)
                cff8=MAX(dZde_r(i  ,j,k1),0.0)
                UFs(i,j,k2)=UFs(i,j,k2)-0.5*cff_vis*om_u(i,j)*
     &                      (cff1*(cff5*dmVdz-dmVde(i-1,j,k1))+
     &                       cff2*(cff6*dmVdz-dmVde(i  ,j,k2))+
     &                       cff3*(cff7*dmVdz-dmVde(i-1,j,k2))+
     &                       cff4*(cff8*dmVdz-dmVde(i  ,j,k1)))
              enddo
            enddo
!
            do j=JstrV,Jend
              do i=Istr,Iend
                cff=0.5*(pn(i,j-1)+pn(i,j))
                dnUdz=cff*0.25*(dUdz(i,j  ,k2)+dUdz(i+1,j  ,k2)+
     &                             dUdz(i,j-1,k2)+dUdz(i+1,j-1,k2))
                dnVdz=cff*dUdz(i,j,k2)
                cff=0.5*(pm(i,j-1)+pm(i,j))
                dmUdz=cff*0.25*(dUdz(i,j  ,k2)+dUdz(i+1,j  ,k2)+
     &                             dUdz(i,j-1,k2)+dUdz(i+1,j-1,k2))
                dmVdz=cff*dUdz(i,j,k2)
                cff1=MIN(dZdx_p(i  ,j,k1),0.0)
                cff2=MIN(dZdx_p(i+1,j,k2),0.0)
                cff3=MAX(dZdx_p(i  ,j,k2),0.0)
                cff4=MAX(dZdx_p(i+1,j,k1),0.0)
# ifdef VIS_COEF_3D
#  ifdef UV_SPLIT_UP3
                cff_vis=0.25*(viscV_r(i,j-1,k  )+viscV_r(i,j,k  )+
     &                        viscV_r(i,j-1,k+1)+viscV_r(i,j,k+1))
#  else
                cff_vis=0.25*(visc3d_r(i,j-1,k  )+visc3d_r(i,j,k  )+
     &                        visc3d_r(i,j-1,k+1)+visc3d_r(i,j,k+1))
#  endif
# else
                cff_vis=0.5*(visc4_r(i,j-1)+visc4_r(i,j))
# endif
                VFs(i,j,k2)=0.5*cff_vis*on_v(i,j)*
     &                      (cff1*(cff1*dnVdz-dnVdx(i  ,j,k1))+
     &                       cff2*(cff2*dnVdz-dnVdx(i+1,j,k2))+
     &                       cff3*(cff3*dnVdz-dnVdx(i  ,j,k2))+
     &                       cff4*(cff4*dnVdz-dnVdx(i+1,j,k1)))
                cff1=MIN(dZde_r(i,j-1,k1),0.0)
                cff2=MIN(dZde_r(i,j  ,k2),0.0)
                cff3=MAX(dZde_r(i,j-1,k2),0.0)
                cff4=MAX(dZde_r(i,j  ,k1),0.0)
                VFs(i,j,k2)=VFs(i,j,k2)+0.5*cff_vis*om_v(i,j)*
     &                      (cff1*(cff1*dmVdz-dmVde(i,j-1,k1))+
     &                       cff2*(cff2*dmVdz-dmVde(i,j  ,k2))+
     &                       cff3*(cff3*dmVdz-dmVde(i,j-1,k2))+
     &                       cff4*(cff4*dmVdz-dmVde(i,j  ,k1)))
                cff1=MIN(dZde_r(i,j-1,k1),0.0)
                cff2=MIN(dZde_r(i,j  ,k2),0.0)
                cff3=MAX(dZde_r(i,j-1,k2),0.0)
                cff4=MAX(dZde_r(i,j  ,k1),0.0)
                cff5=MIN(dZdx_r(i,j-1,k1),0.0)
                cff6=MIN(dZdx_r(i,j  ,k2),0.0)
                cff7=MAX(dZdx_r(i,j-1,k2),0.0)
                cff8=MAX(dZdx_r(i,j  ,k1),0.0)
                VFs(i,j,k2)=VFs(i,j,k2)-0.5*cff_vis*on_v(i,j)*
     &                      (cff1*(cff5*dnUdz-dnUdx(i,j-1,k1))+
     &                       cff2*(cff6*dnUdz-dnUdx(i,j  ,k2))+
     &                       cff3*(cff7*dnUdz-dnUdx(i,j-1,k2))+
     &                       cff4*(cff8*dnUdz-dnUdx(i,j  ,k1)))
                cff1=MIN(dZdx_p(i  ,j,k1),0.0)
                cff2=MIN(dZdx_p(i+1,j,k2),0.0)
                cff3=MAX(dZdx_p(i  ,j,k2),0.0)
                cff4=MAX(dZdx_p(i+1,j,k1),0.0)
                cff5=MIN(dZde_p(i  ,j,k1),0.0)
                cff6=MIN(dZde_p(i+1,j,k2),0.0)
                cff7=MAX(dZde_p(i  ,j,k2),0.0)
                cff8=MAX(dZde_p(i+1,j,k1),0.0)
                VFs(i,j,k2)=VFs(i,j,k2)+0.5*cff_vis*om_v(i,j)*
     &                      (cff1*(cff5*dmUdz-dmUde(i  ,j,k1))+
     &                       cff2*(cff6*dmUdz-dmUde(i+1,j,k2))+
     &                       cff3*(cff7*dmUdz-dmUde(i  ,j,k2))+
     &                       cff4*(cff8*dmUdz-dmUde(i+1,j,k1)))
              enddo
            enddo
          endif
!
! Time-step biharmonic, geopotential viscosity term. Notice that
! momentum at this stage is HzU and HzV and has m2/s units.
!
          cff=dt*0.125
          do j=Jstr,Jend
            do i=IstrU,Iend
              u(i,j,k,nnew)=u(i,j,k,nnew)-
     &                      (cff*(pm(i-1,j)+pm(i,j))*
     &                           (pn(i-1,j)+pn(i,j))*
     &                           ((pn(i-1,j)+pn(i,j))*(UFx(i  ,j)-
     &                                                 UFx(i-1,j))+
     &                            (pm(i-1,j)+pm(i,j))*(UFe(i,j+1)-
     &                                                 UFe(i,j  )))+
     &                       dt*(UFs(i,j,k2)-UFs(i,j,k1)))
# ifdef DIAGNOSTICS_UV
              MHmix(i,j,k,1) = u(i,j,k,nnew)/
     &                          (0.5*(Hz(i-1,j,k)+Hz(i,j,k)))
# endif
            enddo
          enddo

          do j=JstrV,Jend
            do i=Istr,Iend
              v(i,j,k,nnew)=v(i,j,k,nnew)-
     &                      (cff*(pm(i,j)+pm(i,j-1))*
     &                           (pn(i,j)+pn(i,j-1))*
     &                           ((pn(i,j-1)+pn(i,j))*(VFx(i+1,j)-
     &                                                 VFx(i  ,j))-
     &                            (pm(i,j-1)+pm(i,j))*(VFe(i,j  )-
     &                                                 VFe(i,j-1)))+
     &                       dt*(VFs(i,j,k2)-VFs(i,j,k1)))
# ifdef DIAGNOSTICS_UV
              MHmix(i,j,k,2) = v(i,j,k,nnew)/
     &                          (0.5*(Hz(i,j-1,k)+Hz(i,j,k)))
# endif
            enddo
          enddo
        endif
      enddo

      return
      end


