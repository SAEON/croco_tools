!
!======================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
! 
! ROMS_AGRIF website : http://roms.mpl.ird.fr
!======================================================================
!
#include "cppdefs.h"
#ifdef SOLVE3D

      subroutine u3dmix (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call u3dmix_tile (Istr,Iend,Jstr,Jend,   A3d(1,1,trd),
     &                                                  A3d(1,2,trd),
     &                      A2d(1,1,trd), A2d(1,2,trd), A2d(1,3,trd),
     &                      A2d(1,4,trd), A2d(1,5,trd), A2d(1,6,trd))
      return
      end

      subroutine u3dmix_tile (Istr,Iend,Jstr,Jend, LapU,UFs, UFz,
     &                                     UFx,UFe,wrk,Slope,dUdz)
!
!--------------------------------------------------------------------
! Compute lateral mixing terms for XI-component momentum equation.
!--------------------------------------------------------------------
!
      implicit none
# include "param.h"
      integer Istr,Iend,Jstr,Jend, i,j,k,indx
      real LapU(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &      UFs(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &      UFz(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &          UFx(PRIVATE_2D_SCRATCH_ARRAY),
     &          UFe(PRIVATE_2D_SCRATCH_ARRAY),
     &          wrk(PRIVATE_2D_SCRATCH_ARRAY),
     &        Slope(PRIVATE_2D_SCRATCH_ARRAY),
     &         dUdz(PRIVATE_2D_SCRATCH_ARRAY), cff
# include "grid.h"
# include "ocean3d.h"
# include "coupling.h"
# include "mixing.h"
# include "scalars.h"
!
#ifdef MPI
#define LOCALLM Lmmpi
#define LOCALMM Mmmpi
#else
#define LOCALLM Lm
#define LOCALMM Mm
#endif
# include "compute_auxiliary_bounds.h"
!
      indx=3-nstp    !--> time index for target arrays;
#define nnew illegal

# ifdef MIX_S_UV
#  ifdef UV_VIS2
!
! Compute horizontal Laplacian viscosity along constant S-surfaces.
!------------------------------------------------------------------
! Compute horizontal viscous U-component momentum fluxes in XI- and
! ETA-directions, then apply them to compute harmonic viscous term. 
!
      do k=1,N
        do j=Jstr,Jend
          do i=IstrU-1,Iend
            UFx(i,j)=
#   ifdef SMAGORINSKY
     &               (visc2_r(i,j)+visc3d_r(i,j,k))
#   else
     &                visc2_r(i,j)
#   endif
     &                       *pmon_r(i,j)*Hz(i,j,k)
     &                       *(u(i+1,j,k,nrhs)-u(i,j,k,nrhs))
          enddo
        enddo
        do j=Jstr,Jend+1
          do i=IstrU,Iend
            UFe(i,j)=
#   ifdef SMAGORINSKY
     &               (visc2_p(i,j)+visc3d_p(i,j,k))
#   else
     &                visc2_p(i,j)
#   endif
     &                       *pnom_p(i,j)*0.25*( Hz(i,j,k)
     &                  +Hz(i-1,j,k)+Hz(i,j-1,k)+Hz(i-1,j-1,k))
     &                         *(u(i,j,k,nrhs)-u(i,j-1,k,nrhs))
#   ifdef MASKING
     &                                              *pmask(i,j)
#   endif
          enddo
        enddo

        do j=Jstr,Jend
          do i=IstrU,Iend
            cff=0.25*(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
     &             *(UFx(i,j)-UFx(i-1,j)+UFe(i,j+1)-UFe(i,j))
            rufrc(i,j)=rufrc(i,j)+cff
            u(i,j,k,indx)=u(i,j,k,indx)+cff*dt
          enddo
        enddo
      enddo
#  endif /* UV_VIS2 */
#  ifdef UV_VIS4
!
! Compute horizontal biharmonic viscosity along constant S-surfaces.
!-------------------------------------------------------------------
! The biharmonic operator is computed by applying the Laplacian
! operator twice.
!
! Compute horizontal u-momentum flux [m^2/s] in the XI-direction.
!
      do k=1,N
        do j=Jstr-1,Jend+1
          do i=max(IstrU-2,1),min(Iend+1,LOCALLM)
            UFx(i,j)=pmon_r(i,j)*Hz(i,j,k)*( u(i+1,j,k,nrhs)
     &                                        -u(i,j,k,nrhs))
          enddo
        enddo
#   ifdef EW_PERIODIC
        if (WESTERN_EDGE) then
          do j=Jstr-1,Jend+1
            UFx(0,j)=pmon_r(Lm2,j)*Hz(Lm2,j,k)*( u(Lm,j,k,nrhs)-
     &                                               u(Lm2,j,k))
          enddo
        endif
        if (EASTERN_EDGE) then
          do j=Jstr-1,Jend+1
            UFx(L,j)=pmon_r(2,j)*Hz(2,j,k)*( u(3,j,k,nrhs)-
     &                                            u(2,j,k,nrhs))
          enddo
        endif
#   endif /* EW_PERIODIC */
!
!  Compute horizontal u-momentum flux [m^2/s] in the ETA-direction.
!
        do j=max(Jstr-1,1),min(Jend+2,M)
          do i=IstrU-1,Iend+1
            UFe(i,j)=pnom_p(i,j)*(u(i,j,k,nrhs)-u(i,j-1,k,nrhs))
     &                             *0.25*(Hz(i,j  ,k)+Hz(i-1,j  ,k)+
     &                                    Hz(i,j-1,k)+Hz(i-1,j-1,k))
#   ifdef MASKING
            UFe(i,j)=UFe(i,j)*pmask(i,j)
#   endif
          enddo
        enddo
#   ifdef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do i=IstrU-1,Iend+1
            UFe(i,0)=pnom_p(i,Mm2)*(u(i,Mm2,k,nrhs)-u(i,M-3,k,nrhs))
     &                             *0.25*(Hz(i,Mm2,k)+Hz(i-1,Mm2,k)+
     &                                    Hz(i,M-3,k)+Hz(i-1,M-3,k))
#    ifdef MASKING
            UFe(i,0)=UFe(i,0)*pmask(i,Mm2)
#    endif
          enddo
        endif
        if (NORTHERN_EDGE) then
          do i=IstrU-1,Iend+1
            UFe(i,Mp)=pnom_p(i,3)*(u(i,3,k,nrhs)-u(i,2,k,nrhs))
     &                                 *0.25*(Hz(i,3,k)+Hz(i-1,3,k)+
     &                                        Hz(i,2,k)+Hz(i-1,2,k))
#    ifdef MASKING
            UFe(i,Mp)=UFe(i,Mp)*pmask(i,3)
#    endif
          enddo
        endif
#   endif /* NS_PERIODIC */
!
!  Compute first Laplacian, without mixing coefficient.
!  Multiply by the metrics of the second Laplacian, [1/m/s].
!  Save the result into work array "wrk".
!
        do j=Jstr-1,Jend+1
          do i=IstrU-1,Iend+1
            wrk(i,j)=(UFx(i,j)-UFx(i-1,j)+UFe(i,j+1)-UFe(i,j))*
     &               0.5*(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))/
     &               (Hz(i,j,k)+Hz(i-1,j,k))
          enddo
        enddo
!
!   In cases other than periodicity, apply gradient or closed
!  [free-slip (gamma2=1) or no-slip (gamma2=-1)] boundary conditions.
!
#   ifndef EW_PERIODIC
        if (WESTERN_EDGE) then
          do j=Jstr-1,Jend+1
#    ifdef WESTERN_WALL
            wrk(IstrU-1,j)=0.
#    else
            wrk(IstrU-1,j)=wrk(IstrU,j)
#    endif
          enddo
        endif
        if (EASTERN_EDGE) then
          do j=Jstr-1,Jend+1
#    ifdef EASTERN_WALL
            wrk(Iend+1,j)=0.
#    else
            wrk(Iend+1,j)=wrk(Iend,j)
#    endif
          enddo
        endif
#   endif /* !EW_PERIODIC */
#   ifndef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do i=IstrU-1,Iend+1
#    ifdef SOUTHERN_WALL
            wrk(i,Jstr-1)=gamma2*wrk(i,Jstr)
#    else
            wrk(i,Jstr-1)=0.
#    endif
          enddo
        endif
        if (NORTHERN_EDGE) then
          do i=IstrU-1,Iend+1
#    ifdef NORTHERN_WALL
            wrk(i,Jend+1)=gamma2*wrk(i,Jend)
#    else
            wrk(i,Jend+1)=0.
#    endif
          enddo
        endif
#   endif /* !NS_PERIODIC */
!
!  Compute d(LapV)/d(xi) and d(LapV)/d(eta) terms [1/s], then
!  compute and add in biharmonic viscous term to the r.h.s.
!
        do j=Jstr,Jend
          do i=IstrU-1,Iend
            UFx(i,j)=visc4_r(i,j)*pmon_r(i,j)*Hz(i,j,k)
     &                           *(wrk(i+1,j)-wrk(i,j))
          enddo
        enddo
        do j=Jstr,Jend+1
          do i=IstrU,Iend
            UFe(i,j)=visc4_p(i,j)*pnom_p(i,j)
     &                *0.25*(Hz(i,j  ,k)+Hz(i-1,j  ,k)+
     &                       Hz(i,j-1,k)+Hz(i-1,j-1,k))
     &                     *(wrk(i,j)-wrk(i,j-1))
#   ifdef MASKING
            UFe(i,j)=UFe(i,j)*pmask(i,j)
#   endif /* MASKING */
          enddo
        enddo
        cff=dt*0.25D0
        do j=Jstr,Jend
          do i=IstrU,Iend
            u(i,j,k,indx)=u(i,j,k,indx)-cff*(pm(i,j)+pm(i-1,j))
     &                                     *(pn(i,j)+pn(i-1,j))
     &                                *( UFx(i,j  )-UFx(i-1,j)
     &                                  +UFe(i,j+1)-UFe(i  ,j))
          enddo
        enddo
      enddo
#  endif /* UV_VIS4 */
# endif /* MIX_S_UV */
# ifdef MIX_GP_UV
#  ifdef UV_VIS2
!
!  Compute/add_in horizontal Laplacian viscosity along rotated
!  surfaces.
!-------------------------------------------------------------------
!  Compute d(u)/d(z) at horizontal U-points and vertical W-points.
!
      do j=Jstr-1,Jend+1
        do k=1,N-1
          do i=IstrU-1,Iend+1
            UFs(i,j,k)=(u(i,j,k+1,nrhs)-u(i,j,k,nrhs))/
     &                       (0.5*(z_r(i-1,j,k+1)-z_r(i-1,j,k)+
     &                            z_r(i  ,j,k+1)-z_r(i  ,j,k)))
          enddo
        enddo
        do i=IstrU-1,Iend+1
          UFs(i,j,0)=0.
          UFs(i,j,N)=0.
        enddo
      enddo
!-
      do k=N,1,-1
!
!  Compute contribution of the to ETA-component viscous U-fluxes.
!-------------------------------------------------------------------
!  Average d(u)/d(z) to vertical RHO-points [1/s]. Notice that
!  vertical K-loop is reversed to allow recursive use of 3D work
!  array "UFs".
!
        do j=Jstr-1,Jend+1
          do i=IstrU-1,Iend+1
            dUdz(i,j)=0.5*(UFs(i,j,k)+UFs(i,j,k-1))
          enddo
        enddo
!
!  Compute slope (nondimensional) due mixing along geopotential
!  surfaces; if applicable, add in slope due to mixing along
!  isopycnal surfaces and apply land/sea mask.
!
        do j=Jstr,Jend
          do i=IstrU-1,Iend+1
            Slope(i,j)=0.5*(pm(i,j)+pm(i-1,j))*
     &                    (z_r(i,j,k)-z_r(i-1,j,k))
#   ifdef MIX_EN_UV
            Slope(i,j)=Slope(i,j)+rhosx(i,j,k)
#   endif
#   ifdef MASKING
            Slope(i,j)=Slope(i,j)*umask(i,j)
#   endif
          enddo
!
!  Compute flux along along S-surfaces and subtract slope due to
!  rotated surfaces, [m^2/s^2]. Save XI-component of the vertical
!  flux into work array "wrk".
!
          do i=IstrU-1,Iend
            cff=pm(i,j)*(u(i+1,j,k,nrhs)-u(i,j,k,nrhs))
            cff=
#   ifdef SMAGORINSKY
     &          (visc2_r(i,j)+visc3d_r(i,j,k))
#   else
     &           visc2_r(i,j)
#   endif
     &                     *( cff-0.25*(Slope(i,j)+Slope(i+1,j))
     &                                  *(dUdz(i,j)+dUdz(i+1,j)) )
            wrk(i,j)=cff*0.5*(Slope(i,j)+Slope(i+1,j))
            UFx(i,j)=cff*Hz(i,j,k)/pn(i,j)
          enddo
!
!  Add in divergence contribution of the XI-component LapU-fluxes.
!  Also average XI-component of the vertical flux to horizontal
!  U-points and load it back into work array "UFs".
!
          cff=0.25*dt
          do i=IstrU,Iend
            u(i,j,k,indx)=u(i,j,k,indx)+cff*(pm(i,j)+pm(i-1,j))
     &                                     *(pn(i,j)+pn(i-1,j))
     &                                   *(UFx(i,j)-UFx(i-1,j))

            UFs(i,j,k)=0.5*(wrk(i,j)+wrk(i-1,j))
          enddo
        enddo
!
!  Compute contribution of the to ETA-component viscous U-fluxes.
!--------------------------------------------------------------------
!  Compute slope (nondimensional) due mixing along geopotential
!  surfaces; if applicable, add in slope due to mixing along
!  isopycnal surfaces and apply land/sea mask.
!
        do j=Jstr,Jend+1
          do i=IstrU-1,Iend
            Slope(i,j)=0.5*(pn(i,j)+pn(i,j-1))*
     &                    (z_r(i,j,k)-z_r(i,j-1,k))
#   ifdef MIX_EN_UV
            Slope(i,j)=Slope(i,j)+rhose(i,j,k)
#   endif
#   ifdef MASKING
            Slope(i,j)=Slope(i,j)*vmask(i,j)
#   endif
          enddo
!
!  Compute flux along along S-surfaces and subtract slope due
!  to rotated surfaces, [m^2/s^2]. Add in ETA-component of the
!  vertical flux into work array "wrk".
!
          do i=IstrU,Iend
            cff=0.25*(pn(i,j)+pn(i,j-1)+pn(i-1,j)+pn(i-1,j-1))
     &                           *(u(i,j,k,nrhs)-u(i,j-1,k,nrhs))
#   ifdef MASKING
     &                                                 *pmask(i,j)
#   endif
            cff=
#   ifdef SMAGORINSKY
     &          (visc2_p(i,j)+visc3d_p(i,j,k))
#   else
     &           visc2_p(i,j)
#   endif
     &                      *( cff-0.25*(Slope(i-1,j)+Slope(i,j))
     &                                   *(dUdz(i,j)+dUdz(i,j-1)))
            wrk(i,j)=cff*0.5*(Slope(i-1,j)+Slope(i,j))
            UFe(i,j)=cff*(Hz(i,j  ,k)+Hz(i-1,j  ,k)+
     &                    Hz(i,j-1,k)+Hz(i-1,j-1,k))/
     &                   (pm(i,j)+pm(i,j-1)+pm(i-1,j)+pm(i-1,j-1))
          enddo
        enddo
!
!  Compute and add in horizontal divergence (along S-surfaces) of the
!  flux [m^4/s^2] in the ETA-direction. Average ETA-component of the
!  vertical flux to horizontal U-points and add it to array "UFs".
!
        cff=dt*0.25D0
        do j=Jstr,Jend
          do i=IstrU,Iend
            u(i,j,k,indx)=u(i,j,k,indx)+cff*(pm(i,j)+pm(i-1,j))
     &                                     *(pn(i,j)+pn(i-1,j))
     &                                   *(UFe(i,j+1)-UFe(i,j))

            UFs(i,j,k)=UFs(i,j,k)+0.5*(wrk(i,j)+wrk(i,j+1))
          enddo
        enddo
      enddo
!
!  Average vertical flux to vertical W-points. Apply no flux boundary
!  conditions. After that apply the vertical divergence of the flux
!  [m^4/s^2] due to sloping S-surfaces.
 
!
      do j=Jstr,Jend
        do k=1,N-1
          do i=IstrU,Iend
            UFz(i,k)=0.5*(UFs(i,j,k)+UFs(i,j,k+1))
          enddo
        enddo
        do i=IstrU,Iend
          UFz(i,0)=0.
          UFz(i,N)=0.
        enddo
        do k=1,N
          do i=IstrU,Iend
            u(i,j,k,indx)=u(i,j,k,indx)-dt*(UFz(i,k)-UFz(i,k-1))
          enddo
        enddo
      enddo
#  endif /* UV_VIS2 */
#  ifdef UV_VIS4
!
!  Compute/add_in horizontal biharmonic viscosity along rotated
!  surfaces. The biharmonic operator is computed by applying the
!  rotated Laplacian operator twice.
!--------------------------------------------------------------------
!  Evaluate the first rotated Laplacian operator.
!--------------------------------------------------------------------
!  Compute d(u)/d(z) at horizontal U-points and vertical W-points.
!
      do k=1,N-1
        do j=max(Jstr-2,0),min(Jend+2,LOCALMM+1)
          do i=max(IstrU-2,1),min(Iend+2,LOCALLM+1)
            UFs(i,j,k)=(u(i,j,k+1,nrhs)-u(i,j,k,nrhs))/
     &                 (0.5*(z_r(i-1,j,k+1)-z_r(i-1,j,k)+
     &                      z_r(i  ,j,k+1)-z_r(i  ,j,k)))
          enddo
        enddo
#   ifdef EW_PERIODIC
        if (WESTERN_EDGE) then
          do j=Jstr-1,Jend+1
            UFs(0,j,k)=(u(Lm-1,j,k+1,nrhs)-u(Lm-1,j,k,nrhs))/
     &                      (0.5*(z_r(L-3,j,k+1)-z_r(L-3,j,k)+
     &                           z_r(Lm-1,j,k+1)-z_r(Lm-1,j,k)))
          enddo
        endif
        if (EASTERN_EDGE) then
          do j=Jstr-1,Jend+1
            UFs(Lm+2,j,k)=(u(3,j,k+1,nrhs)-u(3,j,k,nrhs))/
     &                      (0.5*(z_r(2,j,k+1)-z_r(2,j,k)+
     &                           z_r(3,j,k+1)-z_r(3,j,k)))
          enddo
        endif
#   endif /* EW_PERIODIC */
#   ifdef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do i=IstrU-1,Iend+1
            UFs(i,-1,k)=(u(i,Mm+1-3,k+1,nrhs)-u(i,Mm+1-3,k,nrhs))/
     &                  (0.5*(z_r(i-1,Mm+1-3,k+1)-z_r(i-1,Mm+1-3,k)+
     &                       z_r(i  ,Mm+1-3,k+1)-z_r(i  ,Mm+1-3,k)))
          enddo
        endif
        if (NORTHERN_EDGE) then
          do i=IstrU-1,Iend+1
            UFs(i,Mm+2,k)=(u(i,3,k+1,nrhs)-u(i,3,k,nrhs))/
     &                  (0.5*(z_r(i-1,3,k+1)-z_r(i-1,3,k)+
     &                       z_r(i  ,3,k+1)-z_r(i  ,3,k)))
          enddo
        endif
#   endif /* NS_PERIODIC */
      enddo
      do j=Jstr-2,Jend+2
        do i=IstrU-2,Iend+2
          UFs(i,j,0)=0.
          UFs(i,j,N)=0.
        enddo
      enddo
!
!  First Laplacian: compute lateral mixing contribution to horizontal
!                   flux in the XI-direction.
!--------------------------------------------------------------------
!
!  Average d(u)/d(z) to vertical RHO-points [1/s]. Notice that the
!  vertical k-loop is reversed to allow recursive use of 3D work
!  array "UFs".
!
      do k=N,1,-1
        do j=Jstr-2,Jend+2
          do i=IstrU-2,Iend+2
            dUdz(i,j)=0.5*(UFs(i,j,k)+UFs(i,j,k-1))
          enddo
        enddo
!
!  Compute slope (nondimensional) due mixing along geopotential
!  surfaces; if applicable, add in slope (nondimensional) due
!  to mixing along isopycnal surfaces and apply land/sea mask.
!
        do j=Jstr-1,Jend+1
          do i=max(IstrU-2,1),min(Iend+2,LOCALLM+1)
            Slope(i,j)=0.5*(pm(i,j)+pm(i-1,j))*
     &                    (z_r(i,j,k)-z_r(i-1,j,k))
#   ifdef MIX_EN_UV
            Slope(i,j)=Slope(i,j)+rhosx(i,j,k)
#   endif
#   ifdef MASKING
            Slope(i,j)=Slope(i,j)*umask(i,j)
#   endif 
          enddo
        enddo
#   ifdef EW_PERIODIC
        if (WESTERN_EDGE) then
          do j=Jstr-1,Jend+1
            Slope(0,j)=0.5*(pm(Lm-1,j)+pm(Lm+1-3,j))*
     &                 (z_r(Lm-1,j,k)-z_r(Lm+1-3,j,k))
#   ifdef MIX_EN_UV
            Slope(0,j)=Slope(0,j)+rhosx(Lm-1,j,k)
#   endif /* MIX_EN_UV */
#   ifdef MASKING
            Slope(0,j)=Slope(0,j)*umask(Lm-1,j)
#   endif /* MASKING */
          enddo
        endif
        if (EASTERN_EDGE) then
          do j=Jstr-1,Jend+1
            Slope(Lm+2,j)=0.5*(pm(3,j)+pm(2,j))*(z_r(3,j,k)-z_r(2,j,k))
#   ifdef MIX_EN_UV
            Slope(Lm+2,j)=Slope(Lm+2,j)+rhosx(3,j,k)
#   endif /* MIX_EN_UV */
#   ifdef MASKING
            Slope(Lm+2,j)=Slope(Lm+2,j)*umask(3,j)
#   endif /* MASKING */
          enddo
        endif
#   endif /* EW_PERIODIC */
!
!  Compute XI-component U viscous flux (without mixing coefficient)
!  along S-surfaces and subtract slope due to rotated surfaces,
!  [m^2/s]. Save XI-component of the vertical flux into array "wrk".
!
        do j=Jstr-1,Jend+1
          do i=max(IstrU-2,1),min(Iend+1,LOCALLM)
            UFx(i,j)=pm(i,j)*(u(i+1,j,k,nrhs)-u(i,j,k,nrhs))
          enddo
        enddo
#   ifdef EW_PERIODIC
        if (WESTERN_EDGE) then
          do j=Jstr-1,Jend+1
            UFx(0,j)=pm(Lm2,j)*(u(Lm,j,k,nrhs)-u(Lm2,j,k,nrhs))
          enddo
        endif
        if (EASTERN_EDGE) then
          do j=Jstr-1,Jend+1
            UFx(L,j)=pm(2,j)*(u(3,j,k,nrhs)-u(2,j,k,nrhs))
          enddo
        endif
#   endif /* EW_PERIODIC */
        do j=Jstr-1,Jend+1
          do i=IstrU-2,Iend+1
            cff=UFx(i,j)-0.25*(Slope(i,j)+Slope(i+1,j))*
     &                          (dUdz(i,j)+dUdz(i+1,j))
            wrk(i,j)=cff*0.5*(Slope(i,j)+Slope(i+1,j))
            UFx(i,j)=cff*Hz(i,j,k)/pn(i,j)
          enddo
!
!  Add in divergence contribution of the XI-component U-fluxes.
!  Also average XI-component of the vertical flux to horizontal
!  U-points and load it back into work array "UFs".
!
          do i=IstrU-1,Iend+1
            LapU(i,j,k)=UFx(i,j)-UFx(i-1,j)
            UFs(i,j,k)=0.5*(wrk(i,j)+wrk(i-1,j))
          enddo
        enddo
!
!  First Laplacian: compute lateral mixing contribution to horizontal
!                   flux in the ETA-direction.
!--------------------------------------------------------------------
!  Compute slope (nondimensional) due mixing along geopotential
!  surfaces; if applicable, add in slope due mixing along isopycnal
!  surfaces and apply land/sea mask.
!
        do j=max(Jstr-1,1),min(Jend+2,LOCALMM+1)
          do i=IstrU-2,Iend+1
            Slope(i,j)=0.5*(pn(i,j)+pn(i,j-1))*
     &                    (z_r(i,j,k)-z_r(i,j-1,k))
#   ifdef MIX_EN_UV
            Slope(i,j)=Slope(i,j)+rhose(i,j,k)
#   endif
#   ifdef MASKING
            Slope(i,j)=Slope(i,j)*vmask(i,j)
#   endif
          enddo
        enddo
#   ifdef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do i=IstrU-2,Iend+1
            Slope(i,0)=0.5*(pn(i,Mm-1)+pn(i,Mm+1-3))*
     &                 (z_r(i,Mm-1,k)-z_r(i,Mm+1-3,k))
#   ifdef MIX_EN_UV
            Slope(i,0)=Slope(i,0)+rhose(i,Mm-1,k)
#   endif
#   ifdef MASKING
            Slope(i,0)=Slope(i,0)*vmask(i,Mm2)
#   endif
          enddo
        endif
        if (NORTHERN_EDGE) then
          do i=IstrU-2,Iend+1
            Slope(i,Mp)=0.5*(pn(i,3)+pn(i,2))*(z_r(i,3,k)-z_r(i,2,k))
#   ifdef MIX_EN_UV
            Slope(i,Mp)=Slope(i,Mp)+rhose(i,3,k)
#   endif
#   ifdef MASKING
            Slope(i,Mp)=Slope(i,Mp)*vmask(i,3)
#   endif
          enddo
        endif
#   endif /* NS_PERIODIC */
!
!  Compute ETA-component horizontal (along S-surfaces) viscous
!  U-momentum fluxes [m^4/s^2]. Then compute contribution of these
!  fluxes into LapU; also average ETA-component of the vertical flux
!  to horizontal U-points and add it into work array "UFs".
!
        do j=max(Jstr-1,1),min(Jend+2,LOCALMM+1)
          do i=IstrU-1,Iend+1
            UFe(i,j)=0.25*(u(i,j,k,nrhs)-u(i,j-1,k,nrhs))*
     &                   (pn(i,j)+pn(i,j-1)+pn(i-1,j)+pn(i-1,j-1))
#   ifdef MASKING
            UFe(i,j)=UFe(i,j)*pmask(i,j)
#   endif
          enddo
        enddo
#   ifdef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do i=IstrU-1,Iend+1
            UFe(i,0)=0.25*( pn(i,Mm2)+pn(i,M-3)+pn(i-1,Mm2)+
     &                pn(i-1,M-3))*(u(i,Mm2,k,nrhs)-u(i,M-3,k,nrhs))
#   ifdef MASKING
            UFe(i,0)=UFe(i,0)*pmask(i,Mm2)
#   endif
          enddo
        endif
        if (NORTHERN_EDGE) then
          do i=IstrU-1,Iend+1
            UFe(i,Mp)=0.25*(pn(i,3)+pn(i,2)+pn(i-1,3)+pn(i-1,2))*
     &                                 (u(i,3,k,nrhs)-u(i,2,k,nrhs))
#   ifdef MASKING
            UFe(i,Mp)=UFe(i,Mp)*pmask(i,3)
#   endif
          enddo
        endif
#   endif /* NS_PERIODIC */
        do j=Jstr-1,Jend+2
          do i=IstrU-1,Iend+1
            cff=UFe(i,j)-0.25*(Slope(i-1,j)+Slope(i,j))*
     &                           (dUdz(i,j)+dUdz(i,j-1))
            wrk(i,j)=cff*0.5*(Slope(i-1,j)+Slope(i,j))
            UFe(i,j)=cff*(Hz(i,j  ,k)+Hz(i-1,j  ,k)+
     &                    Hz(i,j-1,k)+Hz(i-1,j-1,k))/
     &                   (pm(i  ,j)+pm(i  ,j-1)+
     &                    pm(i-1,j)+pm(i-1,j-1))
          enddo
        enddo
        do j=Jstr-1,Jend+1
          do i=IstrU-1,Iend+1
            LapU(i,j,k)=LapU(i,j,k)+UFe(i,j+1)-UFe(i,j)
            UFs(i,j,k)=UFs(i,j,k)+0.5*(wrk(i,j)+wrk(i,j+1))
          enddo
        enddo
      enddo
!
!  Average vertical flux to vertical W-points.
!  Apply no flux top/bottom boundary conditions.
!
      do j=Jstr-1,Jend+1
        do k=1,N-1
          do i=IstrU-1,Iend+1
            UFz(i,k)=0.5*(UFs(i,j,k)+UFs(i,j,k+1))
          enddo
        enddo
        do i=IstrU-1,Iend+1
          UFz(i,0)=0.
          UFz(i,N)=0.
        enddo
!
!  Add in the vertical divergence of the flux due to sloping
!  S-surfaces in the ETA-direction. Multiply the first Laplacian
!  by the metrics of the second Laplacian, [1/m/s].
!
        do k=1,N
          do i=IstrU-1,Iend+1
            LapU(i,j,k)=( LapU(i,j,k)*0.25*(pm(i,j)+pm(i-1,j))*
     &                    (pn(i,j)+pn(i-1,j))-UFz(i,k)+UFz(i,k-1)
     &                            )/(0.5*(Hz(i,j,k)+Hz(i-1,j,k)))
          enddo
        enddo
      enddo      
!
!   In cases other than periodicity, apply gradient or closed
!  [free-slip (gamma2=1) or no-slip (gamma2=-1)] boundary conditions.
!
#   ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
        do k=1,N
          do j=Jstr-1,Jend+1
#    ifdef WESTERN_WALL
            LapU(IstrU-1,j,k)=0.
#    else
            LapU(IstrU-1,j,k)=LapU(IstrU,j,k)
#    endif
          enddo
        enddo
      endif
      if (EASTERN_EDGE) then
        do k=1,N
          do j=Jstr-1,Jend+1
#    ifdef EASTERN_WALL
            LapU(Iend+1,j,k)=0.
#    else
            LapU(Iend+1,j,k)=LapU(Iend,j,k)
#    endif
          enddo
        enddo
      endif
#   endif /* !EW_PERIODIC */
#   ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        do k=1,N
          do i=IstrU-1,Iend+1
#    ifdef SOUTHERN_WALL
            LapU(i,Jstr-1,k)=gamma2*LapU(i,Jstr,k)
#    else
            LapU(i,Jstr-1,k)=0.
#    endif
          enddo
        enddo
      endif
      if (NORTHERN_EDGE) then
        do k=1,N
          do i=IstrU-1,Iend+1
#    ifdef NORTHERN_WALL
            LapU(i,Jend+1,k)=gamma2*LapU(i,Jend,k)
#    else
            LapU(i,Jend+1,k)=0.
#    endif
          enddo
        enddo
      endif
#   endif /* !NS_PERIODIC */
!
!  Evaluate the second rotated Laplacian operator.
!--------------------------------------------------------------------
!  Compute d(LAP(u))/d(z) at horizontal U-points and vertical
!  W-points and store into utility work array "UFs".
!
      do j=Jstr-1,Jend+1
        do k=1,N-1
          do i=IstrU-1,Iend+1
            UFs(i,j,k)=(LapU(i,j,k+1)-LapU(i,j,k))/
     &                 (0.5*(z_r(i-1,j,k+1)-z_r(i-1,j,k)+
     &                      z_r(i  ,j,k+1)-z_r(i  ,j,k)))
          enddo
        enddo
        do i=IstrU-1,Iend+1
          UFs(i,j,0)=0.
          UFs(i,j,N)=0.
        enddo
      enddo
!
!  Compute contribution of the lateral XI-component horizontal
!  viscous LapU-fluxes.
!--------------------------------------------------------------------
!  Average d(LapU)/d(z) to vertical RHO-points [1/m^2/s].
!  Notice that the vertical k-loop is reversed to allow
!  recursive reuse of 3D work array "UFs".
!
      do k=N,1,-1
        do j=Jstr-1,Jend+1
          do i=IstrU-1,Iend+1
            dUdz(i,j)=0.5*(UFs(i,j,k)+UFs(i,j,k-1))
          enddo
        enddo
!
!  Compute slope (nondimensional) due mixing along geopotential
!  surfaces; if applicable, add in slope due to mixing along
!  isopycnal surfaces and apply land/sea mask.
!
        do j=Jstr,Jend
          do i=IstrU-1,Iend+1
            Slope(i,j)=0.5*(pm(i,j)+pm(i-1,j))*
     &                    (z_r(i,j,k)-z_r(i-1,j,k))
#   ifdef MIX_EN_UV
            Slope(i,j)=Slope(i,j)+rhosx(i,j,k)
#   endif
#   ifdef MASKING
            Slope(i,j)=Slope(i,j)*umask(i,j)
#   endif
          enddo
!
!  Compute XI-component viscous LapU-fluxes along S-surfaces
!  and subtract the slope due to rotated surfaces, [m^2/s^2].
!  Save XI-component of the vertical flux in work array "wrk".
!
          do i=IstrU-1,Iend
            cff=pm(i,j)*(LapU(i+1,j,k)-LapU(i,j,k))
            cff=-visc4_r(i,j)*(cff-0.25*(Slope(i,j)+Slope(i+1,j))
     &                                  *(dUdz(i,j)+dUdz(i+1,j)))
            wrk(i,j)=cff*0.5*(Slope(i,j)+Slope(i+1,j))
            UFx(i,j)=cff*Hz(i,j,k)/pn(i,j)
          enddo
!
!  Add in divergence contribution of the XI-component LapU-fluxes.
!  Also average XI-component of the vertical flux to horizontal
!  U-points and load it back into work array "UFs".
!
          cff=dt*0.25D0
          do i=IstrU,Iend
c*          ru(i,j,k,nrhs)=ru(i,j,k,nrhs)+UFx(i,j)-UFx(i-1,j)

            u(i,j,k,indx)=u(i,j,k,indx)+cff*(pm(i,j)+pm(i-1,j))
     &                                     *(pn(i,j)+pn(i-1,j))
     &                                   *(UFx(i,j)-UFx(i-1,j))

            UFs(i,j,k)=0.5*(wrk(i,j)+wrk(i-1,j))
          enddo
        enddo
!
!  Compute contribution of the lateral ETA-component horizontal
!  viscous LapU-fluxes.
!--------------------------------------------------------------------
!  Compute slope (nondimensional) due mixing along geopotential
!  surfaces; if applicable, add in slope due to mixing along
!  isopycnal surfaces; apply land/sea mask.
!
        do j=Jstr,Jend+1
          do i=IstrU-1,Iend
            Slope(i,j)=0.5*(pn(i,j)+pn(i,j-1))*
     &                    (z_r(i,j,k)-z_r(i,j-1,k))
#   ifdef MIX_EN_UV
            Slope(i,j)=Slope(i,j)+rhose(i,j,k)
#   endif
#   ifdef MASKING
            Slope(i,j)=Slope(i,j)*vmask(i,j)
#   endif
          enddo
!
!  Compute ETA-component viscous LapU-fluxes along along S-surfaces
!  and subtract the slope due to rotated surfaces, [m^2/s^2]; Save
!  ETA-component of the vertical flux in work array "wrk".
!
          do i=IstrU,Iend
            cff=0.25*(LapU(i,j,k)-LapU(i,j-1,k))*
     &              (pn(i,j)+pn(i,j-1)+pn(i-1,j)+pn(i-1,j-1))
#   ifdef MASKING
            cff=cff*pmask(i,j)
#   endif /* MASKING */
            cff=-visc4_p(i,j)*(cff-0.25*(Slope(i-1,j)+Slope(i,j))
     &                                  *(dUdz(i,j)+dUdz(i,j-1)))
            wrk(i,j)=cff*0.5*(Slope(i-1,j)+Slope(i,j))
            UFe(i,j)=cff*(Hz(i,j  ,k)+Hz(i-1,j  ,k)+
     &                              Hz(i,j-1,k)+Hz(i-1,j-1,k))/
     &                    (pm(i,j)+pm(i,j-1)+pm(i-1,j)+pm(i-1,j-1))
          enddo
        enddo
!
!  Add in divergence contribution of the ETA-component fluxes.
!  Also average ETA-component of the vertical flux to horizontal
!  U-points and add it in work array "UFs".
!
        cff=dt*0.25D0
        do j=Jstr,Jend
          do i=IstrU,Iend
            u(i,j,k,indx)=u(i,j,k,indx)+cff*(pm(i,j)+pm(i-1,j))
     &                                     *(pn(i,j)+pn(i-1,j))
     &                                   *(UFe(i,j+1)-UFe(i,j))

            UFs(i,j,k)=UFs(i,j,k)+0.5*(wrk(i,j)+wrk(i,j+1))
          enddo
        enddo
      enddo
!
!  Average vertical flux to vertical W-points. Apply no flux boundary
!  conditions. After that compute divergence contribution of vertical
!  component fluxes due to the sloping S-surfaces.

!
      do j=Jstr,Jend
        do k=1,N-1
          do i=IstrU,Iend
            UFz(i,k)=0.5*(UFs(i,j,k)+UFs(i,j,k+1))
          enddo
        enddo
        do i=IstrU,Iend
          UFz(i,0)=0.
          UFz(i,N)=0.
        enddo
      
        do k=1,N
          do i=IstrU,Iend
            u(i,j,k,indx)=u(i,j,k,indx)-dt*(UFz(i,k)-UFz(i,k-1))
          enddo
        enddo
      enddo
#  endif /* UV_VIS4 */
# endif /* MIX_GP_UV */
#else
      subroutine u3dmix_empty
#endif /* SOLVE3D */
      return
      end
