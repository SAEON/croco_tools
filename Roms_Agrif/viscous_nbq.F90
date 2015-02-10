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
!> @brief <a href="http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages/Schemas_Filtrage.htm">
!! Filtering pseudo-acoustic waves</a>.
!! Second viscosity calculation. 
!> @details Divergence operator applied on the momentum, not on velocity.
!! The NH operators are reused.
!
! REVISION HISTORY:
!
!> @authors
!> @date 2015 January
!> @todo
!
!------------------------------------------------------------------------------
      subroutine viscous_nbq(ichoix)

!__________________________________________________________________________________
!
!                               SNH2012.14      
!                 Non-Hydrostatic & Non-Boussinesq Kernel Version  
! Laboratoire d'Aerologie, 14 Avenue Edouard Belin, F-31400 Toulouse
! http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm  
!
!__________________________________________________________________________________

!      use module_principal
!      use module_parallele
      use module_nh
      use module_nbq
!      use module_exp
      implicit none
# include "param_F90.h"
# include "grid.h"

      real dtnbq
      common /time_nbq2/ dtnbq
      real visc2_nbq
      common /nbq_visc2/ visc2_nbq

      real Hzw_half_nbq(GLOBAL_2D_ARRAY,0:N)
      common /grid_Hzw_half_nbq/ Hzw_half_nbq

      integer :: i,j,k,ichoix

      if (ichoix.eq.0) then
!*******************************************************************
!*******************************************************************
!       Initialisation de la seconde viscosite
!       calcul de la viscosite de reference...
!
!       iteration3d = iteration2d = 1
!
!*******************************************************************
!*******************************************************************
       
        do l_nbq=1,neqmom_nh(1)
           i=l2imom_nh(l_nbq)
           j=l2jmom_nh(l_nbq)
           k=l2kmom_nh(l_nbq)
          !visc2_nbq_a(l_nbq) = visc2_nbq * om_u(i,j)
!           visc2_nbq_a(l_nbq) = min(visc2_nbq,om_u(i,j)**2/2./edt_nbq_lp*0.9)  ! Attention x<-->y
           visc2_nbq_a(l_nbq) = min(visc2_nbq,0.9*om_u(i,j)**2/dtnbq)  ! Attention x<-->y
        enddo
        do l_nbq=neqmom_nh(1)+1,neqmom_nh(1)+neqmom_nh(2)
           i=l2imom_nh(l_nbq)
           j=l2jmom_nh(l_nbq)
           k=l2kmom_nh(l_nbq)
          !visc2_nbq_a(l_nbq) = visc2_nbq * don_v(i,j)
!           visc2_nbq_a(l_nbq) = min(visc2_nbq,on_v(i,j)**2/2./edt_nbq_lp*0.9)  ! Attention x<-->y
           visc2_nbq_a(l_nbq) = min(visc2_nbq,0.9*on_v(i,j)**2/dtnbq)  ! Attention x<-->y
        enddo
        do l_nbq=neqmom_nh(1)+neqmom_nh(2)+1,neqmom_nh(0)
           i=l2imom_nh(l_nbq)
           j=l2jmom_nh(l_nbq)
           k=l2kmom_nh(l_nbq)
          !visc2_nbq_a(l_nbq) = visc2_nbq * Hzw_nbq(i,j,k)
!           visc2_nbq_a(l_nbq) = min(visc2_nbq,Hzw_nbq(i,j,k)**2/2./edt_nbq_lp*0.9)
           visc2_nbq_a(l_nbq) = min(visc2_nbq,0.9*Hzw_half_nbq(i,j,k)**2/dtnbq)
        enddo
      !  visc2_nbq_a= visc2_nbq
      endif

      if (ichoix.eq.1) then
!*******************************************************************
!*******************************************************************
!               Seconde viscosite:
!
! Attention: telle que codee actuellement la seconde viscosite
!            est appliquee sur la qdm et non la vitesse comme cela
!            devrait etre le cas pour la version moleculaire !
!            (normalisation par rho dans main_nbq)
!           On "gagne" ainsi un produit MAT*VECT dans la boucle NBQ
!           On conserve toutefois la divergence calculee sur l'ancienne
!           grille... soit probleme potentiel!
!
!*******************************************************************
!*******************************************************************

!    if (mod(int(iic),10).eq.0) then
        !call amux_s(                                               &
        !     neqmom_nh(0)                                          &
        !    ,neqmom_nh(1)+neqmom_nh(2)                             &
        !    ,div_nbq_a(1:nzq_nh,0)                                 &
        !    ,rhsd2_nbq                                             &
        !    ,momv_nh                                               &
        !    ,momj_nh                                               &
        !    ,momi_nh       )   

!..........Mise a jour de la grille:
!           call amux(                                                &
!                 neqcont_nh(0)                                       &
!                ,qdm_nbq_a(1:neqmom_nh(0),1)                         &
!                ,div_nbq_a(1:neqcont_nh(0),0)                        &
!                ,contv_nh(1:nmcont_nh)                               &
!                ,contj_nh                                            &
!                ,conti_nh       )  

         call amux(                                                 &
              neqmom_nh(0)                                          &
             ,div_nbq_a(1:nzq_nh,0)                                 &
             ,rhsd2_nbq(1:neqmom_nh(0))                             &
             ,momv_nh(1:nmcont_nh)                                  &
             ,momj_nh(1:)                                           &
             ,momi_nh(1:)       )   
!    endif
    
      endif


      if (ichoix.eq.2) then
!*******************************************************************
!*******************************************************************
!             Diffusion numerique sur masse volumique:
!*******************************************************************
!*******************************************************************
#ifdef diff_nbq
          call amux(                                                &
              neqmom_nh(0)                                          &
             ,rhp_nbq_a(1:neqcont_nh(0),0)                          &
             ,rhsd1_nbq(1:neqmom_nh(0))                             &
             ,momv_nh                                               &
             ,momj_nh                                               &
             ,momi_nh       )       

          call amux(                                                &
             neqcont_omp                                            &
             ,rhsd1_nbq(1:neqmom_nh(0))                             &
             ,rhsd3_nbq(1:neqmom_nh(0))                             &
             ,ompcontv_nh(1:nmcont_nh)                              &
             ,ompcontj_nh                                           &
             ,ompconti_nh       )   

#endif
       endif


      if (ichoix.eq.10) then
!*******************************************************************
!*******************************************************************
!                Calcul de la seconde viscosite
!
!               iteration3d = iteration2d = 1
!
!*******************************************************************
!*******************************************************************

        return   ! attention passer a visc2ref avant de decommenter!

        do l_nbq=1,neqmom_nh(1)
           i=l2imom_nh(l_nbq)
           j=l2jmom_nh(l_nbq)
           k=l2kmom_nh(l_nbq)
           visc2_nbq_a(l_nbq)=visc2ref_nbq_a(l_nbq) ! &
!             * (1.d0 +  0.d3*abs(diffsum_nbq_a(l_nbq,0)+diffsum_nbq_a(l_nbq,1))/real(2*iteration_nbq_max*iteration2d_max_now,kind=8))
            !* (1.d0 +  5.d2*abs(diffsum2_nbq_a(l_nbq,0)+diffsum2_nbq_a(l_nbq,1))/real(2*iteration_nbq_max,kind=8))
!       write(6,*) iteration3d,'u',i,j,k,visc2_nbq_a(l_nbq),visc2_nbq
        enddo
        do l_nbq=neqmom_nh(1)+1,neqmom_nh(1)+neqmom_nh(2)
           i=l2imom_nh(l_nbq)
           j=l2jmom_nh(l_nbq)
           k=l2kmom_nh(l_nbq)
           visc2_nbq_a(l_nbq)=visc2ref_nbq_a(l_nbq)!  &
!             * (1.d0 +  0.d3*abs(diffsum_nbq_a(l_nbq,0)+diffsum_nbq_a(l_nbq,1))/real(2*iteration_nbq_max*iteration2d_max_now,kind=8))
            !* (1.d0 +  5.d2*abs(diffsum2_nbq_a(l_nbq,0)+diffsum2_nbq_a(l_nbq,1))/real(2*iteration_nbq_max,kind=8))
!       write(6,*) iteration3d,'v',i,j,k,visc2_nbq_a(l_nbq),visc2_nbq
        enddo
        do l_nbq=neqmom_nh(1)+neqmom_nh(2)+1,neqmom_nh(1)+neqmom_nh(2)+neqmom_nh(3)
           i=l2imom_nh(l_nbq)
           j=l2jmom_nh(l_nbq)
           k=l2kmom_nh(l_nbq)
           visc2_nbq_a(l_nbq)=visc2ref_nbq_a(l_nbq)!  &
!             * (1.d0 +  0.d3*abs(diffsum_nbq_a(l_nbq,0)+diffsum_nbq_a(l_nbq,1))/real(2*iteration_nbq_max*iteration2d_max_now,kind=8))
            !* (1.d0 +  5.d2*abs(diffsum2_nbq_a(l_nbq,0)+diffsum2_nbq_a(l_nbq,1))/real(2*iteration_nbq_max,kind=8))
!       write(6,*) iteration3d,'w',i,j,k,visc2_nbq_a(l_nbq),visc2_nbq
        enddo

      
      endif

      end subroutine viscous_nbq

#else
      subroutine viscous_nbq_empty
      end subroutine viscous_nbq_empty
#endif
