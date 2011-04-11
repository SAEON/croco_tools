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

      subroutine v3dmix (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call v3dmix_tile (Istr,Iend,Jstr,Jend,  A3d(1,1,trd),
     &                                                  A3d(1,2,trd),
     &                      A2d(1,1,trd), A2d(1,2,trd), A2d(1,3,trd),
     &                      A2d(1,4,trd), A2d(1,5,trd), A2d(1,6,trd))
      return
      end

      subroutine v3dmix_tile (Istr,Iend,Jstr,Jend, LapV,VFs, VFz,
     &                                     VFx,VFe,wrk,Slope,dVdz)
!
!--------------------------------------------------------------------
!  Compute lateral mixing terms for ETA-direction momentum equation.
!--------------------------------------------------------------------
!
      implicit none
# include "param.h"
      integer Istr,Iend,Jstr,Jend, i,j,k,indx
      real LapV(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &      VFs(PRIVATE_2D_SCRATCH_ARRAY,0:N),
     &      VFz(PRIVATE_1D_SCRATCH_ARRAY,0:N),
     &          VFx(PRIVATE_2D_SCRATCH_ARRAY),
     &          VFe(PRIVATE_2D_SCRATCH_ARRAY),
     &          wrk(PRIVATE_2D_SCRATCH_ARRAY),
     &        Slope(PRIVATE_2D_SCRATCH_ARRAY),
     &         dVdz(PRIVATE_2D_SCRATCH_ARRAY), cff
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
      indx=3-nstp
# define nnew illegal

# ifdef MIX_S_UV
#  ifdef UV_VIS2
!
! Compute horizontal Laplacian viscosity along constant S-surfaces.
!------------------------------------------------------------------
! Compute horizontal viscous V-component momentum fluxes in XI- and
! ETA-directions, then apply them to compute harmonic viscous term.
!
      do k=1,N
        do j=JstrV,Jend
          do i=Istr,Iend+1
            VFx(i,j)=
#   ifdef SMAGORINSKY
     &               (visc2_p(i,j)+visc3d_p(i,j,k))
#   else
     &                visc2_p(i,j)
#   endif
     &                       *pmon_p(i,j)*0.25*( Hz(i,j,k)
     &                  +Hz(i-1,j,k)+Hz(i,j-1,k)+Hz(i-1,j-1,k))
     &                         *(v(i,j,k,nrhs)-v(i-1,j,k,nrhs))
#   ifdef MASKING
     &                                              *pmask(i,j)
#   endif
          enddo
        enddo
        do j=JstrV-1,Jend
          do i=Istr,Iend
            VFe(i,j)=
#   ifdef SMAGORINSKY
     &               (visc2_r(i,j)+visc3d_r(i,j,k))
#   else
     &                visc2_r(i,j)
#   endif
     &                       *pnom_r(i,j)*Hz(i,j,k)
     &                         *(v(i,j+1,k,nrhs)-v(i,j,k,nrhs))
          enddo
        enddo

        do j=JstrV,Jend
          do i=Istr,Iend
            cff=0.25*(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
     &             *(VFx(i+1,j)-VFx(i,j)+VFe(i,j)-VFe(i,j-1))
            rvfrc(i,j)=rvfrc(i,j)+cff
            v(i,j,k,indx)=v(i,j,k,indx)+cff*dt
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
! Compute horizontal v-momentum flux [m^2/s] in the XI-direction.
!
      do k=1,N
        do j=JstrV-1,Jend+1
          do i=max(Istr-1,1),min(Iend+2,LOCALLM+1)
            VFx(i,j)=pmon_p(i,j)*0.25*( Hz(i,j  ,k)+Hz(i-1,j  ,k)
     &                                 +Hz(i,j-1,k)+Hz(i-1,j-1,k))
     &                            *(v(i,j,k,nrhs)-v(i-1,j,k,nrhs))
#   ifdef MASKING
     &                                                 *pmask(i,j)
#   endif
          enddo
        enddo
#   ifdef EW_PERIODIC
        if (WESTERN_EDGE) then
          do j=JstrV-1,Jend+1
            VFx(0,j)=pmon_p(Lm2,j)*(v(Lm2,j,k,nrhs)-v(L-3,j,k,nrhs))
     &                           *0.25*(Hz(Lm2,j  ,k)+Hz(L-3,j  ,k)+
     &                                  Hz(Lm2,j-1,k)+Hz(L-3,j-1,k))
#    ifdef MASKING
            VFx(0,j)=VFx(0,j)*pmask(Lm2,j)
#    endif /* MASKING */
          enddo
        endif
        if (EASTERN_EDGE) then
          do j=JstrV-1,Jend+1
            VFx(Lp,j)=pmon_p(3,j)*(v(3,j,k,nrhs)-v(2,j,k,nrhs))
     &                               *0.25*(Hz(3,j  ,k)+Hz(2,j  ,k)+
     &                                      Hz(3,j-1,k)+Hz(2,j-1,k))
#    ifdef MASKING
            VFx(Lp,j)=VFx(Lp,j)*pmask(3,j)
#    endif /* MASKING */
          enddo
        endif
#   endif /* EW_PERIODIC */
!
!  Compute horizontal v-momentum flux [m^2/s] in the ETA-direction.
!
        do j=max(JstrV-2,1),min(Jend+1,LOCALMM)
          do i=Istr-1,Iend+1
            VFe(i,j)=pnom_r(i,j)*Hz(i,j,k)*( v(i,j+1,k,nrhs)-
     &                                          v(i,j,k,nrhs))
          enddo
        enddo
#   ifdef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do i=Istr-1,Iend+1
            VFe(i,0)=pnom_r(i,Mm2)*Hz(i,Mm2,k)*( v(i,Mm,k,nrhs)-
     &                                           v(i,Mm2,k,nrhs))
          enddo
        endif
        if (NORTHERN_EDGE) then
          do i=Istr-1,Iend+1
            VFe(i,M)=pnom_r(i,2)*Hz(i,2,k)*( v(i,3,k,nrhs)-
     &                                          v(i,2,k,nrhs))
          enddo
        endif
#   endif /* NS_PERIODIC */
!
!  Compute first Laplacian, without mixing coefficient.
!  Multiply by the metrics of the second Laplacian, [1/m/s].
!  Save the result into work array "wrk".
!
        do j=JstrV-1,Jend+1
          do i=Istr-1,Iend+1
            wrk(i,j)=(VFx(i+1,j)-VFx(i,j)+VFe(i,j)-VFe(i,j-1))*
     &               0.5*(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))/
     &               (Hz(i,j,k)+Hz(i,j-1,k))
          enddo
        enddo
!
!  In cases other than periodicity, apply gradient or closed [free-
!  slip (gamma2=1) or no-slip (gamma2=-1)] boundary conditions.
!
#   ifndef EW_PERIODIC
        if (WESTERN_EDGE) then
          do j=JstrV,Jend
#    ifdef WESTERN_WALL
            wrk(Istr-1,j)=gamma2*wrk(Istr,j)
#    else
            wrk(Istr-1,j)=0.
#    endif
          enddo
        endif
        if (EASTERN_EDGE) then
          do j=JstrV,Jend
#    ifdef EASTERN_WALL
            wrk(Iend+1,j)=gamma2*wrk(Iend,j)
#    else
            wrk(Iend+1,j)=0.
#    endif
          enddo
        endif
#   endif /* !EW_PERIODIC */
#   ifndef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do i=Istr-1,Iend+1
#    ifdef SOUTHERN_WALL
            wrk(i,JstrV-1)=0.
#    else
            wrk(i,JstrV-1)=wrk(i,JstrV)
#    endif
          enddo
        endif
        if (NORTHERN_EDGE) then
          do i=Istr-1,Iend+1
#    ifdef NORTHERN_WALL
            wrk(i,Jend+1)=0.
#    else
            wrk(i,Jend+1)=wrk(i,Jend)
#    endif
          enddo
        endif
#   endif /* !NS_PERIODIC */
!
!  Compute d(LapV)/d(xi) and d(LapV)/d(eta) terms, then
!  compute and add in biharmonic mixing [m^4/s^2] to r.h.s.
!
        do j=JstrV,Jend
          do i=Istr,Iend+1
            VFx(i,j)=visc4_p(i,j)*pmon_p(i,j)
     &                 *0.25*(Hz(i,j  ,k)+Hz(i-1,j  ,k)+
     &                        Hz(i,j-1,k)+Hz(i-1,j-1,k))
     &                      *(wrk(i,j)-wrk(i-1,j))
#   ifdef MASKING
            VFx(i,j)=VFx(i,j)*pmask(i,j)
#   endif /* MASKING */
          enddo
        enddo
        do j=JstrV-1,Jend
          do i=Istr,Iend
            VFe(i,j)=visc4_r(i,j)*pnom_r(i,j)*Hz(i,j,k)
     &                           *(wrk(i,j+1)-wrk(i,j))
          enddo
        enddo
        cff=dt*0.25D0
        do j=JstrV,Jend
          do i=Istr,Iend
            v(i,j,k,indx)=v(i,j,k,indx)-cff*(pm(i,j)+pm(i,j-1))
     &                                     *(pn(i,j)+pn(i,j-1))
     &                                *( VFx(i+1,j)-VFx(i,j  )
     &                                  +VFe(i  ,j)-VFe(i,j-1))
          enddo
        enddo
      enddo
#  endif /* UV_VIS4 */
# endif /* MIX_S_UV */
# ifdef MIX_GP_UV
#  ifdef UV_VIS2
!
!  Add in horizontal Laplacian viscosity along rotated surfaces.
!-----------------------------------------------------------------
!  Compute d(v)/d(z) at horizontal V-points and vertical W-points.
!
      do j=JstrV-1,Jend+1
        do k=1,N-1
          do i=Istr-1,Iend+1
            VFs(i,j,k)=(v(i,j,k+1,nrhs)-v(i,j,k,nrhs))/
     &                    (0.5*(z_r(i,j-1,k+1)-z_r(i,j-1,k)+
     &                          z_r(i,j  ,k+1)-z_r(i,j  ,k)))
          enddo
        enddo
        do i=Istr-1,Iend+1
          VFs(i,j,0)=0.
          VFs(i,j,N)=0.
        enddo
      enddo
!-
      do k=N,1,-1
!
!  Compute contribution of the to XI-component viscous V-fluxes.
!-------------------------------------------------------------------
!  Average d(v)/d(z) to vertical RHO-points [1/s]. Notice that
!  vertical K-loop is reversed to allow recursive use of 3D work
!  array "VFs".
!
        do j=JstrV-1,Jend+1
          do i=Istr-1,Iend+1
            dVdz(i,j)=0.5*(VFs(i,j,k)+VFs(i,j,k-1))
          enddo
        enddo
!
!  Compute slope (nondimensional) due mixing along geopotential
!  surfaces; if applicable, add in slope due to mixing along
!  isopycnal surfaces and apply land/sea mask.
!
        do j=JstrV-1,Jend
          do i=Istr,Iend+1
            Slope(i,j)=0.5*(pm(i,j)+pm(i-1,j))
     &                    *(z_r(i,j,k)-z_r(i-1,j,k))
#   ifdef MIX_EN_UV
            Slope(i,j)=Slope(i,j)+rhosx(i,j,k)
#   endif
#   ifdef MASKING
            Slope(i,j)=Slope(i,j)*umask(i,j)
#   endif
          enddo
        enddo
!
!  Compute flux along along S-surfaces and subtract slope due to
!  rotated surfaces, [m^2/s^2]. Save XI-component of the vertical
!  flux into work array "wrk".
!
        do j=JstrV,Jend
          do i=Istr,Iend+1
            cff=0.25*(pm(i,j)+pm(i,j-1)+pm(i-1,j)+pm(i-1,j-1))
     &                         *(v(i,j,k,nrhs)-v(i-1,j,k,nrhs))
#   ifdef MASKING
     &                                               *pmask(i,j)
#   endif
            cff=
#   ifdef SMAGORINSKY
     &          (visc2_p(i,j)+visc3d_p(i,j,k))
#   else
     &           visc2_p(i,j)
#   endif
     &                     *(cff-0.25*(Slope(i,j)+Slope(i,j-1))
     &                                 *(dVdz(i,j)+dVdz(i-1,j)))
            wrk(i,j)=cff*0.5*(Slope(i,j)+Slope(i,j-1))
            VFx(i,j)=cff*(Hz(i,j  ,k)+Hz(i-1,j  ,k)+
     &                    Hz(i,j-1,k)+Hz(i-1,j-1,k))/
     &                   (pn(i  ,j)+pn(i  ,j-1)+
     &                    pn(i-1,j)+pn(i-1,j-1))
          enddo
!
!  Add in divergence contribution of the XI-component V-fluxes.
!  Also average XI-component of the vertical flux to horizontal
!  V-points and load it back into work array "VFs".
!
          cff=0.25*dt
          do i=Istr,Iend
            v(i,j,k,indx)=v(i,j,k,indx)+cff*(pm(i,j)+pm(i,j-1))
     &                                     *(pn(i,j)+pn(i,j-1))
     &                                   *(VFx(i+1,j)-VFx(i,j))

            VFs(i,j,k)=0.5*(wrk(i,j)+wrk(i+1,j))
          enddo
        enddo
!
!  Compute contribution of the to ETA-component viscous V-fluxes.
!----------------------------------------------------------------
!  Compute slope (nondimensional) due mixing along geopotential
!  surfaces, if so prescribed, add in slope due to mixing along
!  isopycnal surfaces and apply land/sea mask.
!
        do j=JstrV-1,Jend+1
          do i=Istr,Iend
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
!
!  Compute flux along along S-surfaces and subtract slope due
!  to rotated surfaces, [m^2/s^2].
!
        do j=JstrV-1,Jend
          do i=Istr,Iend
            cff=pn(i,j)*(v(i,j+1,k,nrhs)-v(i,j,k,nrhs))
            cff=
#   ifdef SMAGORINSKY
     &          (visc2_r(i,j)+visc3d_r(i,j,k))
#   else
     &           visc2_r(i,j)
#   endif
     &                     *(cff-0.25*(Slope(i,j)+Slope(i,j+1))
     &                                 *(dVdz(i,j)+dVdz(i,j+1)))
            wrk(i,j)=cff*0.5*(Slope(i,j)+Slope(i,j+1))
            VFe(i,j)=cff*Hz(i,j,k)/pm(i,j)
          enddo
        enddo
!
!  Add in divergence contribution of the ETA-component V-fluxes.
!  Also average ETA-component of the vertical flux to horizontal
!  V-points and add it into work array "VFs".
!
        cff=dt*0.25D0
        do j=JstrV,Jend
          do i=Istr,Iend
            v(i,j,k,indx)=v(i,j,k,indx)+cff*(pm(i,j)+pm(i,j-1))
     &                                     *(pn(i,j)+pn(i,j-1))
     &                                   *(VFe(i,j)-VFe(i,j-1))

            VFs(i,j,k)=VFs(i,j,k)+0.5*(wrk(i,j-1)+wrk(i,j))
          enddo
        enddo
      enddo
!
!  Average vertical flux to vertical W-points.
!  Apply no flux boundary conditions.
!
      do j=JstrV,Jend
        do k=1,N-1
          do i=Istr,Iend
            VFz(i,k)=0.5*(VFs(i,j,k)+VFs(i,j,k+1))
          enddo
        enddo
        do i=Istr,Iend
          VFz(i,0)=0.
          VFz(i,N)=0.
        enddo
!
!  Add in the vertical divergence of the flux [m^4/s^2] due
!  to sloping S-surfaces.
!
        do k=1,N
          do i=Istr,Iend
            v(i,j,k,indx)=v(i,j,k,indx)-dt*(VFz(i,k)-VFz(i,k-1))
          enddo
        enddo
      enddo
#  endif /* UV_VIS2 */
#  ifdef UV_VIS4
!
!  Add in horizontal biharmonic viscosity along rotated surfaces.
!  The biharmonic operator is computed by applying the rotated
!  Laplacian operator twice.
!--------------------------------------------------------------------
!  Evaluate the first rotated Laplacian operator.
!--------------------------------------------------------------------
!  Compute d(v)/d(z) at horizontal V-points and vertical W-points.
!
      do k=1,N-1
        do j=max(JstrV-2,1),min(Jend+2,LOCALMM+1)
          do i=max(Istr-2,0),min(Iend+2,LOCALLM+1)
            VFs(i,j,k)=(v(i,j,k+1,nrhs)-v(i,j,k,nrhs))/
     &                 (0.5*(z_r(i,j-1,k+1)-z_r(i,j-1,k)+
     &                      z_r(i,j  ,k+1)-z_r(i,j  ,k)))
          enddo
        enddo
#   ifdef EW_PERIODIC
        if (WESTERN_EDGE) then
          do j=JstrV-1,Jend+1
            VFs(-1,j,k)=(v(L-3,j,k+1,nrhs)-v(L-3,j,k,nrhs))/
     &                  (0.5*(z_r(L-3,j-1,k+1)-z_r(L-3,j-1,k)+
     &                       z_r(L-3,j  ,k+1)-z_r(L-3,j  ,k)))
          enddo
        endif
        if (EASTERN_EDGE) then
          do j=JstrV-1,Jend+1
            VFs(Lp,j,k)=(v(3,j,k+1,nrhs)-v(3,j,k,nrhs))/
     &                  (0.5*(z_r(3,j-1,k+1)-z_r(3,j-1,k)+
     &                       z_r(3,j  ,k+1)-z_r(3,j  ,k)))
          enddo
        endif
#   endif /* EW_PERIODIC */
#   ifdef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do i=Istr-1,Iend+1
            VFs(i,0,k)=(v(i,Mm2,k+1,nrhs)-v(i,Mm2,k,nrhs))/
     &                     (0.5*(z_r(i,M-3,k+1)-z_r(i,M-3,k)+
     &                          z_r(i,Mm2,k+1)-z_r(i,Mm2,k)))
          enddo
        endif
        if (NORTHERN_EDGE) then
          do i=Istr-1,Iend+1
            VFs(i,Mp,k)=(v(i,3,k+1,nrhs)-v(i,3,k,nrhs))/
     &                      (0.5*(z_r(i,2,k+1)-z_r(i,2,k)+
     &                           z_r(i,3,k+1)-z_r(i,3,k)))
          enddo
        endif
#   endif /* NS_PERIODIC */
        do j=JstrV-2,Jend+2
          do i=Istr-2,Iend+2
            VFs(i,j,0)=0.
            VFs(i,j,N)=0.
          enddo
        enddo
      enddo
      do k=N,1,-1
!
!  Average d(v)/d(z) to vertical RHO-points [1/s]. Notice that the
!  vertical k-loop is reversed to allow recursive use of 3D work
!  array "VFs".
!
        do j=JstrV-2,Jend+2
          do i=Istr-2,Iend+2
            dVdz(i,j)=0.5*(VFs(i,j,k)+VFs(i,j,k-1))
          enddo
        enddo
!
!  First Laplacian: compute lateral mixing contribution to horizontal
!                   flux in the XI-direction.
!--------------------------------------------------------------------
!  Compute XI-component of the slope of the S-coordinate isosurfaces  
!  with respect to geopotential surfaces; if applicable, add in slope 
!  due to mixing along isopycnal surfaces and apply land/sea mask.   
!
        do j=JstrV-2,Jend+1
          do i=max(Istr-1,1),min(Iend+2,LOCALLM+1)
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
          do j=JstrV-2,Jend+1
            Slope(0,j)=0.5*(pm(Lm2,j)+pm(L-3,j))*
     &                    (z_r(Lm2,j,k)-z_r(L-3,j,k))
#   ifdef MIX_EN_UV
            Slope(0,j)=Slope(0,j)+rhosx(Lm2,j,k)
#   endif
#   ifdef MASKING
            Slope(0,j)=Slope(0,j)*umask(Lm2,j)
#   endif
          enddo
        endif
        if (EASTERN_EDGE) then
          do j=JstrV-2,Jend+1
            Slope(Lp,j)=0.5*(pm(3,j)+pm(2,j))*
     &                     (z_r(3,j,k)-z_r(2,j,k))
#   ifdef MIX_EN_UV
            Slope(Lp,j)=Slope(Lp,j)+rhosx(3,j,k)
#   endif
#   ifdef MASKING
            Slope(Lp,j)=Slope(Lp,j)*umask(3,j)
#   endif /* MASKING */
          enddo
        endif
#   endif /* EW_PERIODIC */
!
!  Compute XI-component V viscous flux (without mixing coefficient)
!  along S-surfaces and subtract slope due to rotated surfaces,
!  [m^2/s]. Save XI-component of the vertical flux into array "wrk".
!

        do j=JstrV-1,Jend+1
          do i=max(Istr-1,1),min(Iend+2,LOCALLM+1)
            VFx(i,j)=0.25*(v(i,j,k,nrhs)-v(i-1,j,k,nrhs))*
     &                   (pm(i,j)+pm(i,j-1)+pm(i-1,j)+pm(i-1,j-1))
#   ifdef MASKING
            VFx(i,j)=VFx(i,j)*pmask(i,j)
#   endif
          enddo
        enddo
#   ifdef EW_PERIODIC
        if (WESTERN_EDGE) then
          do j=JstrV-1,Jend+1
            VFx(0,j)=0.25*(v(Lm2,j,k,nrhs)-v(L-3,j,k,nrhs))*
     &               (pm(Lm2,j)+pm(Lm2,j-1)+pm(L-3,j)+pm(L-3,j-1))
#   ifdef MASKING
            VFx(0,j)=VFx(0,j)*pmask(Lm2,j)
#   endif
          enddo
        endif
        if (EASTERN_EDGE) then
          do j=JstrV-1,Jend+1
            VFx(Lp,j)=0.25*(v(3,j,k,nrhs)-v(2,j,k,nrhs))*
     &                    (pm(3,j)+pm(3,j-1)+pm(2,j)+pm(2,j-1))
#   ifdef MASKING
            VFx(Lp,j)=VFx(Lp,j)*pmask(3,j)
#   endif
          enddo
        endif
#   endif
        do j=JstrV-1,Jend+1
          do i=Istr-1,Iend+2
            cff=VFx(i,j)-0.25*(Slope(i,j)+Slope(i,j-1))*
     &                          (dVdz(i,j)+dVdz(i-1,j))
            wrk(i,j)=cff*0.5*(Slope(i,j)+Slope(i,j-1))
            VFx(i,j)=cff*(Hz(i,j  ,k)+Hz(i-1,j  ,k)+
     &                    Hz(i,j-1,k)+Hz(i-1,j-1,k))/
     &                   (pn(i  ,j)+pn(i  ,j-1)+
     &                    pn(i-1,j)+pn(i-1,j-1))
          enddo
!
!  Add in divergence contribution of the XI-component V-fluxes.
!  Also average XI-component of the vertical flux to horizontal
!  V-points and load it back into work array "VFs".
!
          do i=Istr-1,Iend+1
            LapV(i,j,k)=VFx(i+1,j)-VFx(i,j)
            VFs(i,j,k)=0.5*(wrk(i,j)+wrk(i+1,j))
          enddo
        enddo
!
!  First Laplacian: compute lateral mixing contribution to horizontal
!                   flux in the ETA-direction.
!--------------------------------------------------------------------
!  Compute ETA-component of the slope of the S-coordinate isosurfaces
!  with respect to geopotential surfaces; if applicable, add in slope
!  due to mixing along isopycnal surfaces and apply land/sea mask.
!
        do j=max(JstrV-2,1),min(Jend+2,LOCALMM+1)
          do i=Istr-1,Iend+1
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
          do i=Istr-1,Iend+1
            Slope(i,0)=0.5*(pn(i,Mm2)+pn(i,M-3))*
     &                 (z_r(i,Mm2,k)-z_r(i,M-3,k))
#    ifdef MIX_EN_UV
            Slope(i,0)=Slope(i,0)+rhose(i,Mm2,k)
#    endif
#    ifdef MASKING
            Slope(i,0)=Slope(i,0)*vmask(i,Mm2)
#    endif
          enddo
        endif
        if (NORTHERN_EDGE) then
          do i=Istr-1,Iend+1
            Slope(i,Mp)=0.5*(pn(i,3)+pn(i,2))*(z_r(i,3,k)-z_r(i,2,k))
#    ifdef MIX_EN_UV
            Slope(i,Mp)=Slope(i,Mp)+rhose(i,3,k)
#    endif
#    ifdef MASKING
            Slope(i,Mp)=Slope(i,Mp)*vmask(i,3)
#    endif
          enddo
        endif
#   endif /* NS_PERIODIC */
!
!  Compute ETA-component horizontal (along S-surfaces) viscous
!  V-momentum fluxes [m^4/s^2].
!
        do j=max(JstrV-2,1),min(Jend+1,LOCALMM)
          do i=Istr-1,Iend+1
            VFe(i,j)=pn(i,j)*(v(i,j+1,k,nrhs)-v(i,j,k,nrhs))
          enddo
        enddo
#   ifdef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do i=Istr-1,Iend+1
            VFe(i,0)=pn(i,Mm2)*(v(i,Mm,k,nrhs)-v(i,Mm2,k,nrhs))
          enddo
        endif
        if (NORTHERN_EDGE) then
          do i=Istr-1,Iend+1
            VFe(i,M)=pn(i,2)*(v(i,3,k,nrhs)-v(i,2,k,nrhs))
          enddo
        endif
#   endif /* NS_PERIODIC */
        do j=JstrV-2,Jend+1
          do i=Istr-1,Iend+1
            cff=VFe(i,j)-0.5*(Slope(i,j)+Slope(i,j+1))*
     &                          (dVdz(i,j)+dVdz(i,j+1))
            wrk(i,j)=cff*0.5*(Slope(i,j)+Slope(i,j+1))
            VFe(i,j)=cff*Hz(i,j,k)/pm(i,j)
          enddo
        enddo
!
!  Add in divergence contribution of the ETA-component V-fluxes.
!  Also average ETA-component of the vertical flux to horizontal
!  V-points and add it in work array "VFs".
!
        do j=JstrV-1,Jend+1
          do i=Istr-1,Iend+1
            LapV(i,j,k)=LapV(i,j,k)+VFe(i,j)-VFe(i,j-1)
            VFs(i,j,k)=VFs(i,j,k)+0.5*(wrk(i,j-1)+wrk(i,j))
          enddo
        enddo
      enddo
!
!  Average vertical flux to vertical W-points.
!  Apply no flux top/bottom boundary conditions.
!
      do j=JstrV-1,Jend+1
        do k=1,N-1
          do i=Istr-1,Iend+1
            VFz(i,k)=0.5*(VFs(i,j,k)+VFs(i,j,k+1))
          enddo
        enddo
        do i=Istr-1,Iend+1
          VFz(i,0)=0.
          VFz(i,N)=0.
        enddo
!
!  Add in the vertical divergence of the flux due to sloping
!  S-surfaces in the ETA-direction. Multiply the first Laplacian
!  by the metrics of the second Laplacian, [1/m/s].
!
        do k=1,N
          do i=Istr-1,Iend+1
            LapV(i,j,k)=( LapV(i,j,k)*0.25*(pn(i,j)+pn(i,j-1))*
     &                    (pm(i,j)+pm(i,j-1))-VFz(i,k)+VFz(i,k-1)
     &                            )/(0.5*(Hz(i,j,k)+Hz(i,j-1,k)))
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
          do j=JstrV-1,Jend+1
#    ifdef WESTERN_WALL
            LapV(Istr-1,j,k)=gamma2*LapV(Istr,j,k)
#    else
            LapV(Istr-1,j,k)=0.
#    endif
          enddo
        enddo
      endif
      if (EASTERN_EDGE) then
        do k=1,N
          do j=JstrV-1,Jend+1
#    ifdef EASTERN_WALL
            LapV(Iend+1,j,k)=gamma2*LapV(Iend,j,k)
#    else
            LapV(Iend+1,j,k)=0.
#    endif
          enddo
        enddo
      endif
#   endif /* !EW_PERIODIC */
#   ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        do k=1,N
          do i=Istr-1,Iend+1
#    ifdef SOUTHERN_WALL
            LapV(i,JstrV-1,k)=0.
#    else
            LapV(i,JstrV-1,k)=LapV(i,JstrV,k)
#    endif
          enddo
        enddo
      endif
      if (NORTHERN_EDGE) then
        do k=1,N
          do i=Istr-1,Iend+1
#    ifdef NORTHERN_WALL
            LapV(i,Jend+1,k)=0.
#    else
            LapV(i,Jend+1,k)=LapV(i,Jend,k)
#    endif
          enddo
        enddo
      endif
#   endif /* !NS_PERIODIC */
!
!  Evaluate the second rotated Laplacian operator.
!--------------------------------------------------------------------
!  Compute VFs=d(LAP(v))/d(z) at horizontal V-points and vertical
!  W-points; set verical boundary conditions for it.
!
      do j=JstrV-1,Jend+1
        do k=1,N-1
          do i=Istr-1,Iend+1
            VFs(i,j,k)=(LapV(i,j,k+1)-LapV(i,j,k))/
     &                 (0.5*((z_r(i,j-1,k+1)-z_r(i,j-1,k))+
     &                      (z_r(i,j  ,k+1)-z_r(i,j  ,k))))
     &                     
          enddo
        enddo
        do i=Istr-1,Iend+1
          VFs(i,j,0)=0.
          VFs(i,j,N)=0.
        enddo
      enddo
      do k=N,1,-1
!
!  Compute contribution of the lateral XI-component horizontal
!  viscous LapU-fluxes.
!--------------------------------------------------------------------
!  Average d(LapV)/d(z) to vertical RHO-points [1/m^2/s]. Notice
!  that the vertical k-loop is reversed to allow recursive reuse
!  of 3D work array "VFs".
!
        do j=JstrV-1,Jend+1
          do i=Istr-1,Iend+1
            dVdz(i,j)=0.5*(VFs(i,j,k)+VFs(i,j,k-1))
          enddo
        enddo
!
!  Compute slope (nondimensional) due mixing along geopotential
!  surfaces; if applicable, add in slope (nondimensional) due
!  to mixing along isopycnal surfaces and apply land/sea mask.
!
        do j=JstrV-1,Jend
          do i=Istr,Iend+1
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
!
!  Compute XI-component viscous LapV-fluxes along S-surfaces
!  and subtract the slope due to rotated surfaces, [m^2/s^2].
!  Save XI-component of the vertical flux in work array "wrk".
!
        do j=JstrV,Jend
          do i=Istr,Iend+1
            cff=0.25*(LapV(i,j,k)-LapV(i-1,j,k))*
     &              (pm(i,j)+pm(i,j-1)+pm(i-1,j)+pm(i-1,j-1))
#   ifdef MASKING
            cff=cff*pmask(i,j)
#   endif
            cff=-visc4_p(i,j)*(cff-0.25*(Slope(i,j)+Slope(i,j-1))
     &                                   *(dVdz(i,j)+dVdz(i-1,j)))
            wrk(i,j)=cff*0.5*(Slope(i,j)+Slope(i,j-1))
            VFx(i,j)=cff*(Hz(i,j  ,k)+Hz(i-1,j  ,k)+
     &                    Hz(i,j-1,k)+Hz(i-1,j-1,k))/
     &                   (pn(i,j)+pn(i,j-1)+pn(i-1,j)+pn(i-1,j-1))
          enddo
!
!  Add in divergence contribution of the XI-component LapV-fluxes.
!  Also average XI-component of the vertical flux to horizontal
!  V-points and load it back into work array "VFs".
!
          cff=dt*0.25D0
          do i=Istr,Iend
c*          rv(i,j,k,nrhs)=rv(i,j,k,nrhs)+VFx(i+1,j)-VFx(i,j)

            v(i,j,k,indx)=v(i,j,k,indx)+cff*(pm(i,j)+pm(i,j-1))
     &                                     *(pn(i,j)+pn(i,j-1))
     &                                   *(VFx(i+1,j)-VFx(i,j))

            VFs(i,j,k)=0.5*(wrk(i,j)+wrk(i+1,j))
          enddo
        enddo
!
!  Compute contribution of the lateral ETA-component horizontal
!  viscous LapV-fluxes.
!--------------------------------------------------------------------
!  Compute slope (nondimensional) due mixing along geopotential
!  surfaces; if applicable, add in slope due to mixing along
!  isopycnal surfaces; apply land/sea mask.
!
        do j=JstrV-1,Jend+1
          do i=Istr,Iend
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
!
!  Compute ETA-component viscous LapV-fluxes along along S-surfaces
!  and subtract the slope due to rotated surfaces, [m^2/s^2]; Save
!  ETA-component of the vertical flux in work array "wrk".
!
        do j=JstrV-1,Jend
          do i=Istr,Iend
            cff=pn(i,j)*(LapV(i,j+1,k)-LapV(i,j,k))
            cff=-visc4_r(i,j)*(cff-0.25*(Slope(i,j)+Slope(i,j+1))*
     &                             (dVdz(i,j)+dVdz(i,j+1)))
            wrk(i,j)=cff*0.5*(Slope(i,j)+Slope(i,j+1))
            VFe(i,j)=cff*Hz(i,j,k)/pm(i,j)
          enddo
        enddo
!
!  Add in divergence contribution of the ETA-component LapV-fluxes.
!  Also average ETA-component of the vertical flux to horizontal
!  V-points and add it in work array "VFs".
!
        cff=dt*0.25D0
        do j=JstrV,Jend
          do i=Istr,Iend
            v(i,j,k,indx)=v(i,j,k,indx)+cff*(pm(i,j)+pm(i,j-1))
     &                                     *(pn(i,j)+pn(i,j-1))
     &                                   *(VFe(i,j)-VFe(i,j-1))

            VFs(i,j,k)=VFs(i,j,k)+0.5*(wrk(i,j-1)+wrk(i,j))
          enddo
        enddo
      enddo
!
!  Average vertical flux to vertical W-points.
!  Apply no flux boundary conditions.
!
      do j=JstrV,Jend
        do k=1,N-1
          do i=Istr,Iend
            VFz(i,k)=0.5*(VFs(i,j,k)+VFs(i,j,k+1))
          enddo
        enddo
        do i=Istr,Iend
          VFz(i,0)=0.
          VFz(i,N)=0.
        enddo
!
!  Add in divergence contribution of vertical component fluxes
!  due to the sloping S-surfaces.
!
        do k=1,N
          do i=Istr,Iend
            v(i,j,k,indx)=v(i,j,k,indx)-dt*(VFz(i,k)-VFz(i,k-1))
          enddo
        enddo
      enddo
#  endif /* UV_VIS4 */
# endif /* MIX_GP_UV */
#else
      subroutine v3dmix_empty
#endif /* SOLVE3D */
      return
      end
