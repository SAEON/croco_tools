! $Id$
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
#define PRED_COUPLED_MODE

      subroutine step2d (tile)
      implicit none
      integer tile, trd
#include "param.h"
#include "private_scratch.h"

C$    integer omp_get_thread_num
#include "compute_tile_bounds.h"
      trd=0
C$    trd=omp_get_thread_num()
      
      call step2D_FB_tile (istr,iend,jstr,jend, A2d(1,1,trd),
     &                    A2d(1,2,trd), A2d(1, 3,trd), A2d(1, 4,trd),
     &                    A2d(1, 5,trd), A2d(1, 6,trd),A2d(1, 7,trd),
     &                    A2d(1, 8,trd), A2d(1, 9,trd), A2d(1,10,trd),
     &                    A2d(1,11,trd), A2d(1,12,trd), A2d(1,13,trd))
      return
      end
 
      subroutine step2D_FB_tile (istr,iend,jstr,jend, zeta_new,Dnew,
     &                           rubar,rvbar, urhs,vrhs, DUon,DVom,
     &                           Drhs, UFx,UFe,VFx,VFe)
!
!======================================================================
! Perform one time step for barotropic mode (free-surface and baro-
! tropic 2D momentum equations) using Generalized Forward-Backward
! AB3-AM4 algorithm. Also calculate fast-time averages to interact
! with baroclinic mode.
!
! Forward-Backward schemes for coupled equations (here free-surface zeta
! and 2D momentum ubar and vbar) are stable for longer time-steps 
! (alpha_max>1) than synchronous schemes while only needing evaluation 
! of each r.h.s. term ones. Rather than the original Euler FB schemes, 
! a generalized FB scheme with an AB3-AM4 step is used here for its added 
! robustness (for Coriolis and advection terms). It is a combination of 
! AB3-like step for zeta and AM4-like step for ubar and vbar but with 
! coefficients chosen for maximum stability (e.g. beta=5/12 in original 
! AB3 is replaced by 0.281105):
!
! AB3 Forward step:
! ----------------
! ubarrhs = (3/2+beta)*ubar(n) + (1/2+2*beta)*ubar(n-1) + beta*ubar(n-2)
! vbarrhs = (3/2+beta)*vbar(n) + (1/2+2*beta)*vbar(n-1) + beta*vbar(n-2)
! zeta(n+1) = zeta(n) + RZETA(ubarhs,vbarrhs)
!
! AM4 Backward step:
! -----------------
! zetarhs = (1/2+gamma+2*epsilon) * zeta(n+1) + 
!           (1/2-2*gamma-3*epsilon) * zeta(n) +
!           gamma * zeta(n-1) + epsilon * zeta(n-2)
! ubar(n+1) = ubar(n) + RUBAR(zetarhs,ubarrhs,vbarrhs)
! vbar(n+1) = vbar(n) + RVBAR(zetarhs,ubarrhs,vbarrhs)
!
! beta=0.281105   gamma=0.0880    epsilon=0.013 
!
! Note that the first 2D step is performed with the original Euler FB 
! scheme and the second 2D step with a AB2-AM3 FB scheme with coefficients 
! again chosen for maximum stability.
!
! Reference:
! ---------
! Shchepetkin, A.F., and J.C. McWilliams, 2009: Computational kernel 
! algorithms for fine-scale, multiprocess, longtime oceanic simulations. 
! Pp. 119–182 in Handbook of Numerical Analysis: Computational Methods 
! for the Atmosphere and Oceans. R.M. Teman and J.J. Tribbia, eds, 
! Elsevier Science.
!
!======================================================================
!
      implicit none
#include "param.h"
      integer istr,iend,jstr,jend, i,j, kbak, kold,
#ifdef PSOURCE
     &                                    is,
#endif
     &        IMAX,JMAX
      real    VMAX,VMAXL
      real zeta_new(PRIVATE_2D_SCRATCH_ARRAY),  cff,
     &         Dnew(PRIVATE_2D_SCRATCH_ARRAY),  cff0,
     &        rubar(PRIVATE_2D_SCRATCH_ARRAY),  cff1,
     &        rvbar(PRIVATE_2D_SCRATCH_ARRAY),  cff2,
     &         urhs(PRIVATE_2D_SCRATCH_ARRAY),  cff3,
     &         vrhs(PRIVATE_2D_SCRATCH_ARRAY),
     &         DUon(PRIVATE_2D_SCRATCH_ARRAY),
     &         DVom(PRIVATE_2D_SCRATCH_ARRAY),
     &         Drhs(PRIVATE_2D_SCRATCH_ARRAY),
     &          UFx(PRIVATE_2D_SCRATCH_ARRAY),
     &          UFe(PRIVATE_2D_SCRATCH_ARRAY),  DUnew,
     &          VFx(PRIVATE_2D_SCRATCH_ARRAY),  DVnew,
     &          VFe(PRIVATE_2D_SCRATCH_ARRAY)
#include "grid.h"
#include "ocean2d.h"
#ifdef SOLVE3D
# include "coupling.h"
#else
# include "forces.h"
#endif
#include "mixing.h"
#include "climat.h"
#include "scalars.h"
#include "sources.h"
#ifdef AGRIF
#include "zoom.h"
        integer irhox, irhoy, irhot
#endif
# if defined INTERNAL || defined BODYTIDE
      real U0, omega
# endif
!
#ifdef MASKING
# define SWITCH *
#else
# define SWITCH !
#endif
!
# ifdef MPI
#  define LOCALLM Lmmpi
#  define LOCALMM Mmmpi
# else
#  define LOCALLM Lm
#  define LOCALMM Mm
# endif   

#include "compute_auxiliary_bounds.h"
!
!======================================================================
! AB3 Forward Step: Computes zeta(n+1) and zetarhs
!======================================================================
!
! Preliminary step: compute total depth (meters) of the water
! ----------------- column and vertically integrated mass fluxes
! which are needed to compute divergence in rhs_zeta and as input
! data to compute nonlinear advection terms for the barotropic
! momentum equations.
!

#ifdef AGRIF
        irhox = Agrif_Irhox()
        irhoy = Agrif_Irhoy()
        irhot = Agrif_Irhot()
#endif
      if (FIRST_2D_STEP) then         ! Meaning of temporal indices
        kbak=kstp                     ! ------- -- -------- -------
        kold=kstp                     ! m-2     m-1      m      m+1
        cff1=1.                       ! kold    kbak    kstp    knew
        cff2=0.
        cff3=0.
      elseif (FIRST_2D_STEP+1) then
        kbak=kstp-1
        if (kbak.lt.1) kbak=4
        kold=kbak
        cff1=1.                  ! Logically AB2-AM3 forward-backward 
        cff2=0.                  ! scheme with coefficients chosen for
        cff3=0.                  ! maximum stability ... (see below)
      else
        kbak=kstp-1
        if (kbak.lt.1) kbak=4
        kold=kstp-2
        if (kold.lt.1) kold=4
        cff1= 1.781105
        cff2=-1.06221
        cff3= 0.281105
      endif
!
      do j=JstrV-2,jend+1
        do i=IstrU-2,iend+1
          Drhs(i,j)=h(i,j)+cff1*zeta(i,j,kstp)+cff2*zeta(i,j,kbak)
     &                                        +cff3*zeta(i,j,kold)
        enddo
      enddo
      do j=jstr-1,jend+1
        do i=IstrU-1,iend+1
          urhs(i,j)=cff1*ubar(i,j,kstp) +cff2*ubar(i,j,kbak)
     &                                  +cff3*ubar(i,j,kold)
          DUon(i,j)=0.5*(Drhs(i,j)+Drhs(i-1,j))*on_u(i,j)*urhs(i,j)
        enddo
      enddo
      do j=JstrV-1,jend+1
        do i=istr-1,iend+1
          vrhs(i,j)=cff1*vbar(i,j,kstp) +cff2*vbar(i,j,kbak)
     &                                  +cff3*vbar(i,j,kold)
          DVom(i,j)=0.5*(Drhs(i,j)+Drhs(i,j-1))*om_v(i,j)*vrhs(i,j)
        enddo
      enddo

#ifdef OBC_VOLCONS
        call set_DUV_bc_tile (Istr,Iend,Jstr,Jend, Drhs, DUon,DVom)
#endif
#ifdef RVTK_DEBUG_ADVANCED
C$OMP BARRIER
C$OMP MASTER
       call check_tab2d(zeta(:,:,kstp),'zeta step2d #1','r')
       call check_tab2d(ubar(:,:,kstp),'ubar step2d #1','u')
       call check_tab2d(vbar(:,:,kstp),'vbar step2d #1','v')
C$OMP END MASTER       
#endif    
!
! Advance free-surface:   Compute zeta_new, which is at new time
!-------- ---- --------   step, and interpolate half-step backward
! for the subsequent computation of barotropic pressure-gradient
! terms. It should be noted that because Forward Euler step is used 
! to update zeta during the first barotropic step, the pressure term  
! must be computed via Backward step to keep it numerically stable.
! However, this would interfere with the computation of forcing terms
! "rufrc,rvfrc" because computation of pressure gradient in 3D mode
! uses exactly the initial value of "zeta", rather than value changed
! by one barotropic time step.  To resolve this conflict, the
! pressure gradient term computation during the first barotropic
! step is computed in two stages: first use just zeta(:,:,kstp) to
! insure exact consistency with 3D mode; then, after "rufrc,rvfrc"
! are finalized, add a correction term based on the difference
! zeta_new(:,:)-zeta(:,:,kstp) to "rubar,rvbar" to make them
! consistent with Backward step for pressure gradient terms.
!
      if (FIRST_2D_STEP) then
#ifdef SOLVE3D
        cff0=0.                !---> Compute pressure-gradient
        cff1=1.                !  terms using just zeta(:,:,kstp)
#else
        cff0=1.
        cff1=0.
#endif
        cff2=0.
        cff3=0.
      elseif (FIRST_2D_STEP+1) then
        cff0= 1.0833333333333    ! Logically AB2-AM3 forward-backward
        cff1=-0.1666666666666    ! scheme with coefficients chosen for
        cff2= 0.0833333333333    ! maximum stability, while maintaining
        cff3=0.                  ! third-accuracy; alpha_max=1.73
      else
        cff0=0.614
        cff1=0.285
        cff2=0.088
        cff3=0.013
      endif
 
#define zwrk UFx
#define rzeta  UFe
#define rzeta2  VFe
#define rzetaSA VFx
!
! Computes zeta(n+1)
!
      do j=JstrV-1,jend
        do i=IstrU-1,iend
          zeta_new(i,j)=zeta(i,j,kstp) + dtfast*pm(i,j)*pn(i,j)
     &                                   *(DUon(i,j)-DUon(i+1,j  )
     &                                    +DVom(i,j)-DVom(i  ,j+1))
#ifdef ZONAL_NUDGING
          zeta(i,j,knew)=zeta_new(i,j)
#endif
        enddo
      enddo
!
! Add nudging terms
!
#ifdef ZNUDGING
# ifdef ZONAL_NUDGING
      if (iic.eq.ntstart .or. mod(iic,10).eq.0) then
        if (FIRST_2D_STEP) then
          call zonavg_2d(istr,iend,jstr,jend,
     &                   zeta(START_2D_ARRAY,knew),zetazon)
        endif
      endif
      if (iic.eq.ntstart) then
        if (FIRST_2D_STEP) then
          call zonavg_2d(istr,iend,jstr,jend,
     &                   ssh(START_2D_ARRAY),sshzon)
        endif
      endif
# endif  /* ZONAL_NUDGING */
      do j=JstrV-1,jend
        do i=IstrU-1,iend
          zeta_new(i,j)=zeta_new(i,j) + dtfast*Znudgcof(i,j)
# ifdef ZONAL_NUDGING
     &                                 *(sshzon(j)-zetazon(j))
# else
     &                                 *(ssh(i,j)-zeta_new(i,j))
# endif /* ZONAL_NUDGING */
        enddo
      enddo
#endif /* ZNUDGING */
!
! Computes zetarhs to use in mometum equations
!
      do j=JstrV-1,jend
        do i=IstrU-1,iend
          zeta_new(i,j)=zeta_new(i,j) SWITCH rmask(i,j)
          Dnew(i,j)=zeta_new(i,j)+h(i,j)
          zwrk(i,j)=cff0*zeta_new(i,j) +cff1*zeta(i,j,kstp)
     &             +cff2*zeta(i,j,kbak)+cff3*zeta(i,j,kold)
#if defined VAR_RHO_2D && defined SOLVE3D
          rzeta(i,j)=(1.+rhoS(i,j))*zwrk(i,j)
          rzeta2(i,j)=rzeta(i,j)*zwrk(i,j)
          rzetaSA(i,j)=zwrk(i,j)*(rhoS(i,j)-rhoA(i,j))
#else
          rzeta(i,j)=zwrk(i,j)
          rzeta2(i,j)=zwrk(i,j)*zwrk(i,j)
#endif
        enddo
      enddo
      do j=JstrV-1,Jend
        do i=IstrU-1,Iend
          zeta(i,j,knew)=zeta_new(i,j)        
        enddo
      enddo 
 
#ifdef AGRIF
      if (FIRST_2D_STEP) then
      do j=Jstr-1,Jend+1
        do i=Istr-1,Iend+1
          Zt_avg3(i,j,0)=zeta(i,j,kstp)       
        enddo
      enddo 
      do j=JstrR,JendR
        do i=Istr,IendR
        du_avg3(i,j,0)  = DUon(i,j)
        enddo
      enddo 
      do j=Jstr,JendR
        do i=IstrR,IendR
        dv_avg3(i,j,0)  = DVom(i,j)
        enddo
      enddo 
      endif

#if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_r2d_tile (istr,iend,jstr,jend,
     &                   Zt_avg3(START_2D_ARRAY,0))
      call exchange_u2d_tile (istr,iend,jstr,jend,
     &                   du_avg3(START_2D_ARRAY,0))
      call exchange_v2d_tile (istr,iend,jstr,jend,
     &                   dv_avg3(START_2D_ARRAY,0))
#endif

#ifdef RVTK_DEBUG_ADVANCED
       if (.not.agrif_Root()) then
C$OMP BARRIER
C$OMP MASTER
       call check_tab2d(Zt_avg3(:,:,0),'Zt_avg3 (index 0) step2d','r')
       call check_tab2d(DU_avg3(:,:,0),'DU_avg3 (index 0) step2d','u')
       call check_tab2d(DV_avg3(:,:,0),'DV_avg3 (index 0) step2d','v')
C$OMP END MASTER  
       endif
#endif  

#endif         

#ifdef RVTK_DEBUG_ADVANCED
C$OMP BARRIER
C$OMP MASTER
       call check_tab2d(zeta(:,:,knew),'zeta step2d #2','rint')
C$OMP END MASTER       
#endif       
!
! Set boundary conditions for the free-surface:
!
      call zetabc_tile (Istr,Iend,Jstr,Jend)
!
!
! Compute time averaged fields over all short timesteps.
!-------- ---- -------- ------ ---- --- ----- ----------
! Reset/initialise arrays for averaged fields during the first
! barotropic time step; Accumulate averages after that. Include
! physical boundary points, but not periodic ghost points or
! computation  MPI computational margins.
!
#ifdef SOLVE3D
        cff1=weight(1,iif)
        cff2=weight(2,iif)
        if (FIRST_2D_STEP) then
          do j=jstrR,jendR
            do i=istrR,iendR
              Zt_avg1(i,j)=cff1*zeta(i,j,knew)
              DU_avg1(i,j,nnew)=0.
              DV_avg1(i,j,nnew)=0.
              DU_avg2(i,j)=cff2*DUon(i,j)
              DV_avg2(i,j)=cff2*DVom(i,j)
            enddo
          enddo 
        else
          do j=jstrR,jendR
            do i=istrR,iendR
              Zt_avg1(i,j)=Zt_avg1(i,j)+cff1*zeta(i,j,knew)
              DU_avg2(i,j)=DU_avg2(i,j)+cff2*DUon(i,j)
              DV_avg2(i,j)=DV_avg2(i,j)+cff2*DVom(i,j)
            enddo
          enddo
        endif
#endif
!
!
!======================================================================
! AM4 Backward Step: Computes ubar,vbar at time n+1
!                    first computes rubar,rvbar
!======================================================================
!
!
! Compute pressure-gradient terms  NOTE that "rubar" and "rvbar"
!-------- -------- -------- -----  are computed within the same
! fused loop despite the fact that their normal index ranges are
! different. Fusing loops causes redundant computation of one
! column of "rubar" on the western physical boundary and one row
! of "rvbar" on the southern, but, at the same time it allows to
! share references to array elements (i,j) which results in an
! increase of computational density by almost a factor of 1.5
! resulting in overall more efficient code pipelined in 26 cycles
! (61% of peak speed) on R10000 vs. 16+16 cycles of separate loop
! version for the case when both CPP switches below are defined.
!
      cff=0.5*g
      do j=jstr,jend
        do i=istr,iend
          rubar(i,j)=cff*on_u(i,j)*( (h(i-1,j)+h(i,j))*(rzeta(i-1,j)
     &                        -rzeta(i,j)) +rzeta2(i-1,j)-rzeta2(i,j)
 
#if defined VAR_RHO_2D && defined SOLVE3D
     &              +(h(i-1,j)-h(i,j))*( rzetaSA(i-1,j)+rzetaSA(i,j)
     &                        +0.333333333333*(rhoA(i-1,j)-rhoA(i,j))
     &                                      *(zwrk(i-1,j)-zwrk(i,j)))
#endif
     &                                                              )
! 
          rvbar(i,j)=cff*om_v(i,j)*( (h(i,j-1)+h(i,j))*(rzeta(i,j-1)
     &                        -rzeta(i,j)) +rzeta2(i,j-1)-rzeta2(i,j)
 
#if defined VAR_RHO_2D && defined SOLVE3D
     &              +(h(i,j-1)-h(i,j))*( rzetaSA(i,j-1)+rzetaSA(i,j)
     &                        +0.333333333333*(rhoA(i,j-1)-rhoA(i,j))
     &                                      *(zwrk(i,j-1)-zwrk(i,j)))
#endif
     &                                                              )
        enddo
      enddo            !--> discard  zwrk, rzeta, rzeta2, rzetaSA

#undef rzetaSA
#undef rzeta2
#undef rzeta
#undef zwrk

!
! Compute horizontal advection terms for momentum equations (2D only)
!-------- ---------- --------- ----- --- -------- --------- --- -----
! NOTE: mathematically necessary (minimal) index ranges for momentum-
! flux components are 
!
!      UFx(IstrU-1:iend,jstr:jend)   VFx(istr:iend+1,JstrV:jend)
!      UFe(IstrU:iend,jstr:jend+1)   VFe(istr,iend,JstrV-1,jend)
!
! however, for the purpose computational efficiency, these ranges are
! unified by suppressing U,V-suffices in order to allow fusion of the
! consecutive loops. This leads to slight increase of the redundant
! computations near western and southern boundaries in non-periodic
! directions. 
!

#ifdef UV_ADV
      do j=jstr,jend
        do i=istr-1,iend
          UFx(i,j)=0.25*(DUon(i,j)+DUon(i+1,j))
     &                 *(urhs(i,j)+urhs(i+1,j))

          VFx(i+1,j)=0.25*(DUon(i+1,j)+DUon(i+1,j-1))
     &                   *(vrhs(i+1,j)+vrhs(i,j))
# ifdef MASKING
     &                                 *pmask(i+1,j)
# endif
        enddo
      enddo
      do j=jstr-1,jend
        do i=istr,iend
          VFe(i,j)=0.25*(DVom(i,j)+DVom(i,j+1))
     &                 *(vrhs(i,j)+vrhs(i,j+1))

          UFe(i,j+1)=0.25*(DVom(i,j+1)+DVom(i-1,j+1))
     &                   *(urhs(i,j+1)+urhs(i,j))
# ifdef MASKING
     &                                 *pmask(i,j+1)
# endif
        enddo
      enddo
      do j=jstr,jend
        do i=istr,iend
          rubar(i,j)=rubar(i,j)-UFx(i,j)+UFx(i-1,j)
     &                         -UFe(i,j+1)+UFe(i,j)

          rvbar(i,j)=rvbar(i,j)-VFx(i+1,j)+VFx(i,j)
     &                         -VFe(i,j)+VFe(i,j-1)
        enddo
      enddo !--> discard UFx,VFe,UFe,VFx, DUon,DVom
#endif
!
! Compute Coriolis (2D and 3D) term and advective curvilinear metric
! terms (2D only).
!
# if defined UV_COR || (defined CURVGRID && defined UV_ADV)
      do j=JstrV-1,jend
        do i=IstrU-1,iend
          cff=Drhs(i,j)*(
#  ifdef UV_COR
     &                   fomn(i,j)
#  endif
#  if (defined CURVGRID && defined UV_ADV)
     &          +0.5*( dndx(i,j)*(vrhs(i,j)+vrhs(i,j+1))
     &                -dmde(i,j)*(urhs(i,j)+urhs(i+1,j)))
#  endif
     &                   )
          UFx(i,j)=cff*(vrhs(i,j)+vrhs(i,j+1))
          VFe(i,j)=cff*(urhs(i,j)+urhs(i+1,j))
        enddo
      enddo
      do j=jstr,jend
        do i=IstrU,iend
          rubar(i,j)=rubar(i,j)+0.25*(UFx(i,j)+UFx(i-1,j))
        enddo
      enddo
      do j=JstrV,jend
        do i=istr,iend
          rvbar(i,j)=rvbar(i,j)-0.25*(VFe(i,j)+VFe(i,j-1))
        enddo
      enddo
# endif
!
! Compute horizontal viscous stress terms (2D only).
!
#ifdef SOLVE3D
#undef UV_VIS2
#endif
# ifdef UV_VIS2
      do j=jstr-1,jend
        do i=istr-1,iend
          cff=2.*Drhs(i,j)*visc2_r(i,j)
          UFx(i,j)=cff*(ubar(i+1,j,kstp)-ubar(i,j,kstp))
     &                                 *pm(i,j)*on_r(i,j)
          VFe(i,j)=cff*(vbar(i,j+1,kstp)-vbar(i,j,kstp))
     &                                 *pn(i,j)*om_r(i,j)

          cff1=0.0625*visc2_p(i+1,j+1)*( Drhs(i,j)
     &       +Drhs(i+1,j)+Drhs(i,j+1)+Drhs(i+1,j+1) )*(
     &          (pn(i+1,j+1)+pn(i,j+1)+pn(i+1,j)+pn(i,j))
     &             *(ubar(i+1,j+1,kstp)-ubar(i+1,j,kstp))
     &         +(pm(i+1,j+1)+pm(i,j+1)+pm(i+1,j)+pm(i,j))
     &             *(vbar(i+1,j+1,kstp)-vbar(i,j+1,kstp))
     &                                                  )
#  ifdef MASKING
     &                     *pmask(i+1,j+1)
#  endif
          UFe(i+1,j+1)=cff1*om_p(i+1,j+1)
          VFx(i+1,j+1)=cff1*on_p(i+1,j+1)
        enddo
      enddo
      do j=jstr,jend
        do i=istr,iend
          rubar(i,j)=rubar(i,j)+UFx(i,j)-UFx(i-1,j)
     &                         +UFe(i,j+1)-UFe(i,j)

          rvbar(i,j)=rvbar(i,j)+VFx(i+1,j)-VFx(i,j)
     &                         +VFe(i,j)-VFe(i,j-1)
        enddo
      enddo
# endif /* UV_VIS2 */
!
! Linear and/or quadratic bottom stress.
!

      if (rdrg2.gt.0.) then
        do j=jstr,jend
          do i=IstrU,iend
            cff=0.25*( vbar(i  ,j,kstp)+vbar(i  ,j+1,kstp)
     &                +vbar(i-1,j,kstp)+vbar(i-1,j+1,kstp))
 
            rubar(i,j)=rubar(i,j)-ubar(i,j,kstp)*( rdrg+rdrg2
     &              *sqrt(ubar(i,j,kstp)*ubar(i,j,kstp)+cff*cff)
     &                                     )*om_u(i,j)*on_u(i,j)
          enddo
        enddo
        do j=JstrV,jend
          do i=istr,iend
            cff=0.25*( ubar(i,j  ,kstp)+ubar(i+1,j  ,kstp)
     &                +ubar(i,j-1,kstp)+ubar(i+1,j-1,kstp))
 
            rvbar(i,j)=rvbar(i,j)-vbar(i,j,kstp)*( rdrg+rdrg2
     &              *sqrt(cff*cff+vbar(i,j,kstp)*vbar(i,j,kstp))
     &                                     )*om_v(i,j)*on_v(i,j)
          enddo
        enddo
      else if (rdrg.gt.0.0) then
        do j=jstr,jend
          do i=IstrU,iend
            rubar(i,j)=rubar(i,j) - rdrg*ubar(i,j,kstp)
     &                             *om_u(i,j)*on_u(i,j)
          enddo
        enddo
        do j=JstrV,jend
          do i=istr,iend
            rvbar(i,j)=rvbar(i,j) - rdrg*vbar(i,j,kstp)
     &                             *om_v(i,j)*on_v(i,j)
          enddo
        enddo
      endif
      
!
! Coupling between 2D and 3D parts.
!--------- ------- -- --- -- ------
! Before the predictor step of the first barotropic time step
! arrays "rufrc" and "rvfrc" contain vertically integrals of the
! 3D right-hand-side terms for the momentum equations (including
! surface and bottom stresses, if so prescribed).
!
! During the first barotropic time step connvert them into forcing
! terms by subtracting the fast-time "rubar" and "rvbar" from them;
! These forcing terms are then extrapolated forward in time using
! optimized Adams-Bashforth weights, so that the resultant rufrc
! and rvfrc are centered effectively at time n+1/2. From now on,
! these newly computed forcing terms will remain constant during
! the fast time stepping and will added to "rubar" and "rvbar"
! during all subsequent barotropic time steps.
!
#ifdef SOLVE3D
      if (FIRST_2D_STEP) then
# ifdef PRED_COUPLED_MODE
        if (FIRST_TIME_STEP) then
          cff3=0.                        ! This version is designed
          cff2=0.                        ! for coupling during 3D
          cff1=1.                        ! predictor sub-step: here
        elseif (FIRST_TIME_STEP+1) then  ! forcing term "rufrc" is
          cff3=0.                        ! computed as instantaneous
          cff2=-0.5                      ! value at 3D time step
          cff1=1.5                       ! "nstp" first, and then
        else                             ! extrapolated half-step
          cff3=0.281105                  ! forward using  AM3-like
          cff2=-0.5-2.*cff3              ! weights optimized for
          cff1=1.5+cff3                  ! maximum stability (with
        endif                            ! special care for startup)
        do j=jstr,jend
          do i=IstrU,iend
            cff=rufrc(i,j)-rubar(i,j)
            rufrc(i,j)=cff1*cff + cff2*rufrc_bak(i,j,3-nstp)
     &                          + cff3*rufrc_bak(i,j,nstp)
            rufrc_bak(i,j,nstp)=cff
          enddo
        enddo
        do j=JstrV,jend
          do i=istr,iend
            cff=rvfrc(i,j)-rvbar(i,j)
            rvfrc(i,j)=cff1*cff + cff2*rvfrc_bak(i,j,3-nstp)
     &                          + cff3*rvfrc_bak(i,j,nstp)
            rvfrc_bak(i,j,nstp)=cff
          enddo
        enddo
# else
        do j=jstr,jend                       ! This version is
          do i=istr,iend                     ! designed for coupling
            rufrc(i,j)=rufrc(i,j)-rubar(i,j) ! during 3D corrector
            rvfrc(i,j)=rvfrc(i,j)-rvbar(i,j) ! sub-step: no forward
          enddo                              ! extrapolation needs
        enddo                                ! to be performed.
# endif
!
! Since coupling requires that pressure gradient term is computed
! using zeta(:,:,kstp) instead of zeta_new(:,:) needed to achieve
! numerical stability, apply compensation to shift pressure gradient
! terms from "kstp" to "knew": in essense, convert the fist 2D step
! from Forward Euler to Forward-Backward].
!
# define zwrk UFx
# define rzeta  UFe
# define rzeta2  VFe
# define rzetaSA VFx

        do j=JstrV-1,jend
          do i=IstrU-1,iend
            zwrk(i,j)=zeta_new(i,j)-zeta(i,j,kstp)
# if defined VAR_RHO_2D && defined SOLVE3D
            rzeta(i,j)=(1.+rhoS(i,j))*zwrk(i,j)
            rzeta2(i,j)=rzeta(i,j)*(zeta_new(i,j)+zeta(i,j,kstp))
            rzetaSA(i,j)=zwrk(i,j)*(rhoS(i,j)-rhoA(i,j))
# else
            rzeta(i,j)=zwrk(i,j)
            rzeta2(i,j)=zwrk(i,j)*(zeta_new(i,j)+zeta(i,j,kstp))
# endif
          enddo
        enddo

        cff=0.5*g
        do j=jstr,jend
          do i=istr,iend
            rubar(i,j)=rubar(i,j) +cff*on_u(i,j)*( (h(i-1,j)+h(i,j))
     &          *(rzeta(i-1,j)-rzeta(i,j)) +rzeta2(i-1,j)-rzeta2(i,j)
# if defined VAR_RHO_2D && defined SOLVE3D
     &              +(h(i-1,j)-h(i,j))*( rzetaSA(i-1,j)+rzetaSA(i,j)
     &                        +0.333333333333*(rhoA(i-1,j)-rhoA(i,j))
     &                                     *(zwrk(i-1,j)-zwrk(i,j)) )
# endif
     &                                                              )
!
            rvbar(i,j)=rvbar(i,j) +cff*om_v(i,j)*( (h(i,j-1)+h(i,j))
     &          *(rzeta(i,j-1)-rzeta(i,j)) +rzeta2(i,j-1)-rzeta2(i,j)

# if defined VAR_RHO_2D && defined SOLVE3D
     &              +(h(i,j-1)-h(i,j))*( rzetaSA(i,j-1)+rzetaSA(i,j)
     &                        +0.333333333333*(rhoA(i,j-1)-rhoA(i,j))
     &                                     *(zwrk(i,j-1)-zwrk(i,j)) )
# endif
     &                                                              )
          enddo
        enddo            !--> discard  zwrk, rzeta, rzeta2, rzetaSA

# undef rzetaSA
# undef rzeta2
# undef rzeta
# undef zwrk
      endif   !<-- FIRST_2D_STEP
#endif
!
! Perform time step for the 2D momentum equations. Also compute
! fast-time averaged barotropic mass fluxes. Doing so on the fly
! yields a more computationally dense code and eliminates repeated   
! multiplication by Dnew (since mass fluxes are actually available
! as volatile variables DUnew,DVnew at this moment. However, as the
! result of this arrangement, a special code is needed to compute
! fast-time averages along the physical boundaries, which is done
! immediately below. 
!

#define Dstp DUon
 
      do j=JstrV-1,jend
        do i=IstrU-1,iend
          Dstp(i,j)=zeta(i,j,kstp)+h(i,j)
        enddo
      enddo

      cff=0.5*dtfast
#ifdef SOLVE3D
      cff1=0.5*weight(1,iif)
#else
      cff2=2.*dtfast
#endif
      do j=jstr,jend
        do i=IstrU,iend
          DUnew=( (Dstp(i,j)+Dstp(i-1,j))*ubar(i,j,kstp)
     &        +cff*(pm(i,j)+pm(i-1,j))*(pn(i,j)+pn(i-1,j))
#ifdef SOLVE3D
     &                             *(rubar(i,j)+rufrc(i,j))
#else
     &                          *rubar(i,j)+cff2*sustr(i,j)
#endif
     &                                                    )
#ifdef MASKING
     &                                         *umask(i,j)
#endif
          ubar(i,j,knew)=DUnew/(Dnew(i,j)+Dnew(i-1,j))
#ifdef SOLVE3D
          DU_avg1(i,j,nnew)=DU_avg1(i,j,nnew) +cff1*DUnew*on_u(i,j)
#endif
        enddo
      enddo
         
      do j=JstrV,jend
        do i=istr,iend
          DVnew=( (Dstp(i,j)+Dstp(i,j-1))*vbar(i,j,kstp)
     &        +cff*(pm(i,j)+pm(i,j-1))*(pn(i,j)+pn(i,j-1))
#ifdef SOLVE3D
     &                             *(rvbar(i,j)+rvfrc(i,j))
#else
     &                          *rvbar(i,j)+cff2*svstr(i,j)
#endif
     &                                                    )
#ifdef MASKING
     &                                         *vmask(i,j)
#endif
          vbar(i,j,knew)=DVnew/(Dnew(i,j)+Dnew(i,j-1))
#ifdef SOLVE3D
          DV_avg1(i,j,nnew)=DV_avg1(i,j,nnew) +cff1*DVnew*om_v(i,j)
#endif
        enddo
      enddo
!
!  Set 2D Momemtum nudging
!
#if defined M2NUDGING && defined M2CLIMATOLOGY

# ifdef ZONAL_NUDGING
      if (iic.eq.ntstart .or. mod(iic,10).eq.0) then
        if (FIRST_2D_STEP) then
          call zonavg_2d(istr,iend,jstr,jend,
     &                   ubar(START_2D_ARRAY,knew),ubzon)
          call zonavg_2d(istr,iend,jstr,jend,
     &                   vbar(START_2D_ARRAY,knew),vbzon)
        endif
      endif
      if (iic.eq.ntstart) then
        if (FIRST_2D_STEP) then
          call zonavg_2d(istr,iend,jstr,jend,
     &                   ubclm(START_2D_ARRAY),ubclmzon)
          call zonavg_2d(istr,iend,jstr,jend,
     &                   vbclm(START_2D_ARRAY),vbclmzon)
        endif
      endif
# endif /* ZONAL_NUDGING */

      do j=Jstr,Jend
        do i=IstrU,Iend
# ifdef ZONAL_NUDGING        
          DUnew = dtfast*M2nudgcof(i,j)*(ubclmzon(j)-ubzon(j))
# else          
          DUnew = dtfast*M2nudgcof(i,j)*(ubclm(i,j)-ubar(i,j,knew))
# endif
# ifdef MASKING
     &                 * umask(i,j)
# endif          
          ubar(i,j,knew)=ubar(i,j,knew) + DUnew
# ifdef SOLVE3D
          DU_avg1(i,j,nnew)=DU_avg1(i,j,nnew) +cff1*DUnew*
     &                         (Dnew(i,j)+Dnew(i-1,j))*on_u(i,j)
# endif
        enddo
      enddo
      do j=JstrV,Jend
        do i=Istr,Iend
# if defined ZONAL_NUDGING  
          DVnew = dtfast*M2nudgcof(i,j)*(vbclmzon(j)-vbzon(j)) 
# else
          DVnew = dtfast*M2nudgcof(i,j)*(vbclm(i,j)-vbar(i,j,knew))    
# endif      
# ifdef MASKING
     &                 * vmask(i,j)
# endif          
          vbar(i,j,knew)=vbar(i,j,knew) + DVnew
# ifdef SOLVE3D
          DV_avg1(i,j,nnew)=DV_avg1(i,j,nnew) +cff1*DVnew*
     &                         (Dnew(i,j)+Dnew(i,j-1))*om_v(i,j)
# endif

        enddo
      enddo
#endif
!
!  Body force for the Internal Tide test case
!
#if defined INTERNAL || defined BODYTIDE
      omega=2.*pi/(12.4*3600.)
      U0=0.02
      do j=Jstr,Jend
        do i=IstrU,Iend
          DUnew = dtfast*omega*U0*cos(omega*time)
          ubar(i,j,knew)=ubar(i,j,knew) + DUnew
#ifdef SOLVE3D
          DU_avg1(i,j,nnew)=DU_avg1(i,j,nnew) +cff1*DUnew*
     &                         (Dnew(i,j)+Dnew(i-1,j))*on_u(i,j)
#endif
        enddo
      enddo
      do j=JstrV,Jend
        do i=Istr,Iend
          DVnew = dtfast*0.5*(f(i,j)+f(i,j-1))*
     &                   U0*sin(omega*time)
          vbar(i,j,knew)=vbar(i,j,knew) + DVnew
#ifdef SOLVE3D
          DV_avg1(i,j,nnew)=DV_avg1(i,j,nnew) +cff1*DVnew*
     &                         (Dnew(i,j)+Dnew(i,j-1))*om_v(i,j)
#endif

        enddo
      enddo
#endif     
!
! Set boundary conditions and compute integral mass flux accross
! all open boundaries, if any.
!

      call    u2dbc_tile (istr,iend,jstr,jend, UFx) 
      call    v2dbc_tile (istr,iend,jstr,jend, UFx)

#ifdef OBC_VOLCONS
      call obc_flux_tile (istr,iend,jstr,jend)
#endif
!
! Compute fast-time averaged barotropic mass fluxes along physical
! boundaries.
!
#ifdef SOLVE3D
# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
        do j=jstr-1,jendR
          Dnew(istr-1,j)=h(istr-1,j)+zeta(istr-1,j,knew)
        enddo
      endif
      if (EASTERN_EDGE) then
        do j=jstr-1,jendR
          Dnew(iend+1,j)=h(iend+1,j)+zeta(iend+1,j,knew)
        enddo
      endif
# endif
# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        do i=istr-1,iendR
          Dnew(i,jstr-1)=h(i,jstr-1)+zeta(i,jstr-1,knew)
        enddo
      endif
      if (NORTHERN_EDGE) then
        do i=istr-1,iendR
          Dnew(i,jend+1)=h(i,jend+1)+zeta(i,jend+1,knew)
        enddo
      endif
# endif
      cff1=0.5*weight(1,iif)
# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
        do j=jstrR,jendR
          DU_avg1(IstrU-1,j,nnew)=DU_avg1(IstrU-1,j,nnew)
     &         +cff1*(Dnew(IstrU-1,j)
     &         +Dnew(IstrU-2,j))*ubar(IstrU-1,j,knew)*on_u(IstrU-1,j)
        enddo
        do j=JstrV,jend
          DV_avg1(istr-1,j,nnew)=DV_avg1(istr-1,j,nnew)
     &       +cff1*(Dnew(istr-1,j)
     &       +Dnew(istr-1,j-1) )*vbar(istr-1,j,knew)*om_v(istr-1,j)
        enddo
      endif
        
      if (EASTERN_EDGE) then
        do j=jstrR,jendR
          DU_avg1(iend+1,j,nnew)=DU_avg1(iend+1,j,nnew)
     &            +cff1*( Dnew(iend+1,j)
     &            +Dnew(iend,j) )*ubar(iend+1,j,knew)*on_u(iend+1,j)
        enddo
        do j=JstrV,jend
          DV_avg1(iend+1,j,nnew)=DV_avg1(iend+1,j,nnew)
     &        +cff1*( Dnew(iend+1,j)
     &        +Dnew(iend+1,j-1) )*vbar(iend+1,j,knew)*om_v(iend+1,j)
        enddo
      endif
# endif
# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        do i=IstrU,iend
          DU_avg1(i,jstr-1,nnew)=DU_avg1(i,jstr-1,nnew)
     &        +cff1*( Dnew(i,jstr-1)
     &        +Dnew(i-1,jstr-1) )*ubar(i,jstr-1,knew)*on_u(i,jstr-1)
        enddo
        do i=istrR,iendR
          DV_avg1(i,JstrV-1,nnew)=DV_avg1(i,JstrV-1,nnew)
     &         +cff1*(Dnew(i,JstrV-1)
     &         +Dnew(i,JstrV-2))*vbar(i,JstrV-1,knew)*om_v(i,JstrV-1)
        enddo
      endif
      if (NORTHERN_EDGE) then
        do i=IstrU,iend
          DU_avg1(i,jend+1,nnew)=DU_avg1(i,jend+1,nnew)
     &        +cff1*( Dnew(i,jend+1)
     &        +Dnew(i-1,jend+1) )*ubar(i,jend+1,knew)*on_u(i,jend+1)
        enddo
        do i=istrR,iendR
          DV_avg1(i,jend+1,nnew)=DV_avg1(i,jend+1,nnew)
     &            +cff1*( Dnew(i,jend+1)
     &            +Dnew(i,jend) )*vbar(i,jend+1,knew)*om_v(i,jend+1)
        enddo
      endif
# endif 
#endif

#ifdef PSOURCE
!
! Apply point sources for river runoff simulations.
!
      do is=1,Nsrc 
# ifdef MPI
        i=Isrc_mpi(is,mynode)
        j=Jsrc_mpi(is,mynode)
# else
        i=Isrc(is)
        j=Jsrc(is)
# endif
        if (istrR.le.i .and. i.le.iendR .and.
     &      jstrR.le.j .and. j.le.jendR) then
          if (Dsrc(is).eq.0) then
            ubar(i,j,knew)=2.*Qbar(is)/( on_u(i,j)
     &                   *(Dnew(i-1,j)+Dnew(i,j)) )
# ifdef SOLVE3D
            DU_avg1(i,j,nnew)=Qbar(is)
# endif
          else
            vbar(i,j,knew)=2.*Qbar(is)/( om_v(i,j)
     &                   *(Dnew(i,j-1)+Dnew(i,j)) )
# ifdef SOLVE3D
            DV_avg1(i,j,nnew)=Qbar(is)
# endif
          endif
        endif
      enddo
#endif
#ifdef SOLVE3D
!      if (iif.eq.nfast) call set_depth_tile (istr,iend,jstr,jend)
#else
      call diag_tile (istr,iend,jstr,jend, UFx,UFe)
#endif
#if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_r2d_tile (istr,iend,jstr,jend,
     &                   zeta(START_2D_ARRAY,knew))
      call exchange_u2d_tile (istr,iend,jstr,jend,
     &                   ubar(START_2D_ARRAY,knew))
      call exchange_v2d_tile (istr,iend,jstr,jend,
     &                   vbar(START_2D_ARRAY,knew))
#endif

#ifdef RVTK_DEBUG_ADVANCED
C$OMP BARRIER
C$OMP MASTER
       call check_tab2d(ubar(:,:,knew),'ubar step2d #2','u')
       call check_tab2d(vbar(:,:,knew),'vbar step2d #2','v')
C$OMP END MASTER       
#endif   

#ifdef AGRIF
      if (.NOT.Agrif_Root()) THEN
        do j=Jstr-1,Jend+1
          do i=Istr-1,Iend+1
            Zt_avg3(i,j,iif)=zeta(i,j,knew)
          enddo
        enddo 
        do j=JstrR,JendR
          do i=Istr,IendR
          du_avg3(i,j,iif) = 0.5*(h(i,j)+zeta(i,j,knew)+
     &          h(i-1,j)+zeta(i-1,j,knew)) *on_u(i,j)*ubar(i,j,knew)
          enddo
        enddo 
        do j=Jstr,JendR
          do i=IstrR,IendR
          dv_avg3(i,j,iif) = 0.5*(h(i,j)+zeta(i,j,knew)+
     &          h(i,j-1)+zeta(i,j-1,knew)) *om_v(i,j)*vbar(i,j,knew)
          enddo
        enddo
        
#if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_r2d_tile (istr,iend,jstr,jend,
     &                   Zt_avg3(START_2D_ARRAY,iif))
      call exchange_u2d_tile (istr,iend,jstr,jend,
     &                   du_avg3(START_2D_ARRAY,iif))
      call exchange_v2d_tile (istr,iend,jstr,jend,
     &                   dv_avg3(START_2D_ARRAY,iif))
#endif

#ifdef RVTK_DEBUG_ADVANCED
C$OMP BARRIER
C$OMP MASTER
       call check_tab2d(Zt_avg3(:,:,iif),'Zt_avg3 step2d','r')
       call check_tab2d(DU_avg3(:,:,iif),'DU_avg3 step2d','u')
       call check_tab2d(DV_avg3(:,:,iif),'DV_avg3 step2d','v')
C$OMP END MASTER       
#endif   
      endif
# ifdef AGRIF_CONSERV_VOL
       if (iif == nfast) then
       if (agrif_root()) then
         do j=jstrR,jendR
           do i=istrR,iendR
             DU_avg1(i,j,5) = dt * DU_avg2(i,j)
             DV_avg1(i,j,5) = dt * DV_avg2(i,j)
           enddo
         enddo
       else
         do j=jstrR,jendR
           do i=istrR,iendR
             DU_avg1(i,j,5) = dt * DU_avg2(i,j)
             DV_avg1(i,j,5) = dt * DV_avg2(i,j)
             DU_avg1(i,j,4) = DU_avg1(i,j,4) + DU_avg1(i,j,5)
             DV_avg1(i,j,4) = DV_avg1(i,j,4) + DV_avg1(i,j,5)
           enddo
         enddo       
       endif
       endif
# endif
#endif
!
!  TEST FOR CFL VIOLATION. IF SO, PRINT AND STOP
!
      VMAXL=100.
      VMAX=0.
      do j=JstrV,Jend
        do i=Istr,Iend
          IF(ABS(vbar(i,j,knew)).GE.VMAX) THEN
            VMAX=ABS(vbar(i,j,knew))
#ifdef MPI
            IMAX=i+iminmpi-1
            JMAX=j+jminmpi-1
#else
            IMAX=i
            JMAX=j
#endif
          ENDIF
        enddo
      enddo
      IF(VMAX.GT.VMAXL) THEN
         write(stdout,'(A)') '                                         '
         write(stdout,'(A)') '                                         '
         write(stdout,'(A)') ' ======================================= '
         write(stdout,'(A)') ' =                                     = '
         write(stdout,'(A)') ' =   STEP2D:   ABNORMAL JOB END        = '
         write(stdout,'(A)') ' =                 BLOW UP             = '
         write(stdout,'(A)') ' =                                     = '
         write(stdout,'(A)') ' ======================================= '
         write(stdout,'(A)') '                                         '
         write(stdout,'(A,F8.2)') '  VMAX (M/S) =',VMAX
         write(stdout,'(A,2I6)')  '  IMAX JMAX  =',IMAX,JMAX
#ifdef SOLVE3D
        WRITE(stdout,'(A,2I6)')   '  IINT IEXT  =',iic,iif
#else
        WRITE(stdout,'(A,I6)')    '  IIC        =',iic
#endif
        may_day_flag=1
        stop                            !-->  EXIT
      ENDIF
        
      return
      end
 
