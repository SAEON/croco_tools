#include "cppdefs.h"
#ifdef NBQ
!------------------------------------------------------------------------------
!                               NHOMS
!                Non Hydrostatic Ocean Modeling System      
!------------------------------------------------------------------------------
!
!> @note <a href="http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm"> Main web documentation </a>
!
! DESCRIPTION: 
!
!> @brief Density anomaly driver 
!
!> @details 
!! - ichoix=0 vertical averaging density to get the external mode density (rhpbar_nbq_t).
!! - ichoix=1 internal mode density (rhpio_nbq_t).
!! - ichoix=5 computes the NH pressure gradient for the internal mode.
!! - ichoix=6 computes the divergence of the momentum (div_nbq_a) 
!!      + used in the momentum equation (second viscosity term with gradient operator)
!!      + used in the continuity equation
!! - ichoix=7 time incrementation of NBQ mode density anomaly.
!
! REVISION HISTORY:
!
!> @authors
!> @date 2015 January
!> @todo
!
!------------------------------------------------------------------------------
      subroutine density_nbq(ichoix)

!__________________________________________________________________________
!
!                               SNH2012.14      
!                 Non-Hydrostatic & Non-Boussinesq Kernel Version  
! Laboratoire d'Aerologie, 14 Avenue Edouard Belin, F-31400 Toulouse 
! http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm  
!
!__________________________________________________________________________


!      use module_principal , only : kount0, iteration3d, rhp_t, rho, mask_t, &
!			    dz_t, iteration2d_max_now, hz_w, iteration2d,    &
!			    imax, jmax, kmax
!      use module_parallele                ! #MPI#
      use module_nh , only : nzq_nh, neqmom_nh, momv_nh, momj_nh, momi_nh, &
                      l2iq_nh, l2jq_nh, l2kq_nh, l2imom_nh, l2jmom_nh,     &
                      l2kmom_nh,contv_nh,conti_nh,contj_nh,nmcont_nh       ! #NH#
      use module_nbq                      ! #NBQ#
      implicit none
      integer :: ichoix, i,j ,k, k1
# include "param_F90.h"
# include "nbq.h"
# include "work.h"
# include "scalars_F90.h"

      real Hzr(GLOBAL_2D_ARRAY,N)
      common /grid_Hzr/Hzr

      if (ichoix.eq.0) then
!
!**********************************************************************
!  reinitialize density fields
!**********************************************************************
!
# if defined NBQ_REINIT || !defined M2FILTER_NONE
!
        do l_nbq = 1 , nzq_nh
          i     = l2iq_nh (l_nbq)
          j     = l2jq_nh (l_nbq)
          k     = l2kq_nh (l_nbq)
#  ifdef NBQ_REINIT
          rhp_nbq_a(l_nbq,0)=rho_nbq_avg1(i,j,k)-rho0
          rhp_nbq_a(l_nbq,1)=rhp_nbq_a(l_nbq,0)
#  else
          rhp_nbq_a(l_nbq,:)=rhp_nbq_a_bak(l_nbq,:)
#  endif
        enddo
# endif

      elseif (ichoix.eq.2) then
!
!**********************************************************************
!  Transfer density field to i,j,k array 
!  and time filter, ready for external mode
!**********************************************************************
!
        rhobar_nbq(:,:)=0.
        work2d(:,:)=0.
        do l_nbq = 1 , nzq_nh
          i     = l2iq_nh (l_nbq)
          j     = l2jq_nh (l_nbq)
          k     = l2kq_nh (l_nbq)
       
          rho_nbq_ext(i,j,k)  = 0.5*(rhp_nbq_a(l_nbq,0)+rhp_nbq_a(l_nbq,1))
          work2d(i,j)         = work2d(i,j)+Hzr(i,j,k)
          rhobar_nbq(i,j)     = rhobar_nbq(i,j)+                      &
                                rho_nbq_ext(i,j,k)*Hzr(i,j,k)
        enddo

        rhobar_nbq(1:Lm,1:Mm)=rhobar_nbq(1:Lm,1:Mm)/work2d(1:Lm,1:Mm)
!
!.......Rho s added subsequently for added precision 
!
        rho_nbq_ext(:,:,:)  = rho_nbq_ext(:,:,:) + rho0
        rhobar_nbq(:,:)     = rhobar_nbq(:,:)    + rho0

      elseif (ichoix.eq.3) then
!
!**********************************************************************
!   Transformation z--> s for time derivative ??? 
!**********************************************************************
!
   
      elseif (ichoix.eq.6) then
!
!**********************************************************************
!     Calcul de la divergence
!**********************************************************************
!
         if(iteration_nbq  > 1) then

            call amux(                                                &
                  nzq_nh                                              &
               !  neqcont_nh(0)                                       &
                 ,qdm_nbq_a(1:neqmom_nh(0),2)                         &
                 ,div_nbq_a(1:nzq_nh,1)                               &
               ! ,div_nbq_a(1:neqcont_nh(0),1)                        &
                 ,contv_nh(1:nmcont_nh)                               &
                 ,contj_nh(1:)                                        &
                 ,conti_nh(1:)       )

         else

            call amux(                                                &
                  nzq_nh                                              &
              !   neqcont_nh(0)                                       &
                 ,qdm_nbq_a(1:neqmom_nh(0),1)                         &
                 ,div_nbq_a(1:nzq_nh,1)                               &
              !  ,div_nbq_a(1:neqcont_nh(0),1)                        &
                 ,contv_nh(1:nmcont_nh)                               &
                 ,contj_nh(1:)                                        &
                 ,conti_nh(1:)    )

         endif

      elseif (ichoix.eq.7) then
!
!*******************************************************************
!......Move forward: Masse
!*******************************************************************
!
        do l_nbq = 1 , nzq_nh
!          rhp_nbq_a(l_nbq,-1) = rhp_nbq_a(l_nbq,0)                                  
          rhp_nbq_a(l_nbq,0)  = rhp_nbq_a(l_nbq,1) 
          rhp_nbq_a(l_nbq,1)  = rhp_nbq_a(l_nbq,2) 
          div_nbq_a(l_nbq,0)  = div_nbq_a(l_nbq,1)  
        enddo

      endif  ! ichoix

        return
        end
#else
        subroutine density_nbq_empty
        return
        end
#endif
