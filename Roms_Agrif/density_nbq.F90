#include "cppdefs.h"
#ifdef NBQ

      subroutine density_nbq(ichoix)

!**********************************************************************
!
!                      Various Computations related to
!                            NBQ density
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
!**********************************************************************
!      use module_principal , only : kount0, iteration3d, rhp_t, rho, mask_t, &
!			    dz_t, iteration2d_max_now, hz_w, iteration2d,    &
!			    imax, jmax, kmax
!      use module_parallele               ! #MPI#
      use module_nh                       ! #NH#
      use module_nbq                      ! #NBQ#
      implicit none
      integer :: ichoix, i,j ,k, k1,indm_d

# include "param_F90.h"
# include "scalars_F90.h"
# include "work.h"
# include "grid.h"
# include "ocean3d.h"
# include "nbq.h"

#ifdef MPI
#define LOCALLM Lmmpi
#define LOCALMM Mmmpi
#else
#define LOCALLM Lm
#define LOCALMM Mm
#endif    

      real :: dist_d

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
          rhp_nbq_a(l_nbq,0)=rho0*(rho_nbq_avg1(i,j,k) - 1.)
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
       
          rho_nbq_ext(i,j,k)  = 0.5*(rhp_nbq_a(l_nbq,0)                &
                                    +rhp_nbq_a(l_nbq,1))
          work2d(i,j)         = work2d(i,j)+Hzr_half_nbq(i,j,k)
          rhobar_nbq(i,j)     = rhobar_nbq(i,j)+                       &
                                rho_nbq_ext(i,j,k)*Hzr_half_nbq(i,j,k)
        enddo
!
!.......Rho0 added subsequently for added precision 
!
        rho_nbq_ext(:,:,:) = (rho_nbq_ext(:,:,:) + rho0) / rho0

        do j=0,LOCALMM+1
        do i=0,LOCALLM+1
           rhobar_nbq(i,j) = (rhobar_nbq(i,j)/work2d(i,j) + rho0) / rho0
        enddo
        enddo
   
      elseif (ichoix.eq.3) then
!
!**********************************************************************
! Store density fields at internal timestep n+1
!**********************************************************************
!
         rhp_nbq_a_bak(1:nzq_nh,0)=rhp_nbq_a(1:nzq_nh,0)
         rhp_nbq_a_bak(1:nzq_nh,1)=rhp_nbq_a(1:nzq_nh,1)

      elseif (ichoix.eq.6) then
!
!**********************************************************************
!     Calcul de la divergence
!**********************************************************************
!
         if(iteration_nbq  > 1) then
            indm_d=conti_nh(neqmom_nh(0)+1)+1
            call amux(                                                &
                  nzq_nh                                              &
                 ,qdm_nbq_a(1:neqmom_nh(0),2)                         &
                 ,div_nbq_a(1:nzq_nh,1)                               &
                 ,contv_nh(1:indm_d)                                  &
                 ,contj_nh(1:indm_d)                                  &
                 ,conti_nh(1:neqmom_nh(0)+1)                          &
               ! ,nmlcont_nh                                          &
                       )

         else

            call amux(                                                &
                  nzq_nh                                              &
                 ,qdm_nbq_a(1:neqmom_nh(0),1)                         &
                 ,div_nbq_a(1:nzq_nh,1)                               &
                 ,contv_nh(1:indm_d)                                  &
                 ,contj_nh(1:indm_d)                                  &
                 ,conti_nh(1:neqmom_nh(0)+1)                          &
             !   ,nmlcont_nh                                          &
                  )

         endif

      elseif (ichoix.eq.7) then
!
!*******************************************************************
!......Move forward: Masse
!*******************************************************************
!          
         rhp_nbq_a(1:nzq_nh,0)  = rhp_nbq_a(1:nzq_nh,1) 
         rhp_nbq_a(1:nzq_nh,1)  = rhp_nbq_a(1:nzq_nh,2) 
         div_nbq_a(1:nzq_nh,0)  = div_nbq_a(1:nzq_nh,1)  ! for second viscosity

# ifdef ACOUSTIC
      elseif (ichoix.eq.10) then
!
!*******************************************************************
!......Acoustic waves: Initialization
!*******************************************************************

          period_exp = 0.025/2.
          for_a_exp  = 2.5
          amp_exp = 1.e-3
          hmax_exp = 128.
          dg_exp = 2.

      elseif (ichoix.eq.11) then
!
!*******************************************************************
!......Acoustic waves: Forcing
!*******************************************************************

        time_nbq = time_nbq + 0.5*dtnbq

        do l_nbq = 1 , nzq_nh 

          i=l2iq_nh(l_nbq)
          j=l2jq_nh(l_nbq)
          k=l2kq_nh(l_nbq)

          dist_d=sqrt((xr(i,j)-xl/2.)**2+(0.*(yr(i,j)-el/2.))**2       &
                              +(abs(z_r(i,j,k))-hmax_exp/2.)**2)
!         if (dist_d.le.for_a_exp(1)) then
             div_nbq_a(l_nbq,1) = div_nbq_a(l_nbq,1)                   &
                        +amp_exp*sin(2*acos(-1.)*time_nbq/period_exp)  &
                                        *exp(-dist_d**2/for_a_exp**2)
!         endif
        enddo
# endif /* ACOUSTIC */

      endif  ! ichoix

        return
        end
#else
        subroutine density_nbq_empty
        return
        end
#endif
