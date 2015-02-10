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
!> @brief SNBQ driver : Non-hydrostatic algorithm with the Non-boussinesq solver.
!
!> @details Loops on the NBQ time step. See SNBQ web pages :
!! - <a href="http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages/Algorithme_NBQ.htm">SNBQ algorithm</a>
!! - <a href="http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages/Algebrique_SNBQ.htm">SNBQ algebric representation</a>
! <a href="http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages/Couplage_Numerique.htm">Numerical coupling</a>
! <a href="http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages/Couplage_Modes_SNBQ.htm">Coupling</a>
! <a href="http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages/Couplage_Split_NBQ.htm">Coupling Splitting</a>
! REVISION HISTORY:
!
!> @authors
!> @date 2015 January
!> @todo
!
!------------------------------------------------------------------------------
      subroutine step3d_nbq

!__________________________________________________________________________________
!
!                               SNH2012.14      
!                 Non-Hydrostatic & Non-Boussinesq Version  
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

#     include "param_F90.h"
#     include "scalars_F90.h"
#     include "ocean3d_F90.h"

      double precision :: dist_m    ! Pour forcage onde acoustique.
      double precision :: tmptime1,tmptime2

      real dtnbq
      common /time_nbq2/ dtnbq 

      integer i,j,k

# undef DEBUG
!
!-------------------------------------------------------------------
! Au premier pas de temps externe !
!-------------------------------------------------------------------
!
      if (iif.eq.1) then

!......Set second viscosity coefficient:
        if (iic.eq.1) then
          call viscous_nbq(0)
        endif

!......Schema numerique pour la seconde viscosite:
!       call viscous_nbq(10)

      endif
!
!-------------------------------------------------------------------
!  Reinitialize momentum and density fields
!-------------------------------------------------------------------
!
# if defined NBQ_REINIT || !defined M2FILTER_NONE
      if (iif.eq.1) then
        call ru_nbq(0)
        call density_nbq(0)
      endif
# endif
!
!-------------------------------------------------------------------
!       Store momentum and density fields at internal timestep n+1
!-------------------------------------------------------------------
!
# if !defined NBQ_REINIT && !defined M2FILTER_NONE
      if (iif.eq.ndtfast+1.and. iteration_nbq.eq.1) then
        do l_nbq = 1 , neqmom_nh(1) + neqmom_nh(2) + (1-ifl_imp_nbq) * neqmom_nh(3)
          qdm_nbq_a_bak(l_nbq,0)=qdm_nbq_a(l_nbq,0)
          qdm_nbq_a_bak(l_nbq,1)=qdm_nbq_a(l_nbq,1)
        enddo
        do l_nbq = 1 , nzq_nh
          rhp_nbq_a_bak(l_nbq,0)=rhp_nbq_a(l_nbq,0)
          rhp_nbq_a_bak(l_nbq,1)=rhp_nbq_a(l_nbq,1)
        enddo
      endif
# endif
!
!-------------------------------------------------------------------
!  Get internal and external forcing terms for nbq equation:
!  ru+rubar (or lambda_ext+lambda_int)
!-------------------------------------------------------------------
!
      call ru_nbq(1)
!
!-------------------------------------------------------------------
!    Transformation z--> s for time derivative ???
!-------------------------------------------------------------------
!
!      call density_nbq(3)
!
!-------------------------------------------------------------------
!       Partie implicite: construction du systeme
!-------------------------------------------------------------------
!
# ifdef TOTO
      if (iif.eq.1.and.ifl_imp_nbq.eq.1) call implicit_nbq (1)
# endif
!
!*******************************************************************
!*******************************************************************
!              NBQ mode iteration (main loop)
!*******************************************************************
!*******************************************************************

      do iteration_nbq=1,iteration_nbq_max

# ifdef DEBUG
        print *,'STEP3D_NBQ: it_nbq = ',iteration_nbq
# endif
!
!-------------------------------------------------------------------
!      Compute divergence term (AMUX):
!          qdm_nbq_a ==> div_nbq_a
!-------------------------------------------------------------------
!
        call density_nbq(6)
!
!-------------------------------------------------------------------
!......Emission eventuelle d'une onde acoustique:
!-------------------------------------------------------------------
!
!       if (name_exp.eq.'acous') call state_exp(10)
!       call state_exp(10)
!
!-------------------------------------------------------------------
!      Compute Second viscosity (product mat*vect):
!            div_nbq_a ==> rhsd2_nbq
!-------------------------------------------------------------------
!
        call viscous_nbq (1)
!
!-------------------------------------------------------------------
!      Message passing 
!-------------------------------------------------------------------
!
# ifdef TOTO_PARALLEL
!  Send
        call parallele_nbq(7)

!  Wait/receive
        if(iteration_nbq > 1) then     ! not during first step
          call parallele_nbq(15)
        endif
# endif
!
!-------------------------------------------------------------------
!      Momentum equation: switch time indices (move forward)
!-------------------------------------------------------------------
!
        if(iteration_nbq > 1) then     ! not during first step
          call ru_nbq(7)
        endif
!
!-------------------------------------------------------------------
!......Diffusion numerique sur masse volumique (à éviter!)
!-------------------------------------------------------------------
!
!       call viscous_nbq (2)
!
!-------------------------------------------------------------------
!      Compute pressure gradient and gravity terms (AMUX)
!                rhp_nbq_a  ==> rhs1_nbq
!-------------------------------------------------------------------
!
        call ru_nbq(6)
!
!-------------------------------------------------------------------
!      Momentum equation: leapfrog time stepping
!-------------------------------------------------------------------
!
        do l_nbq = 1 , neqmom_nh(1) + neqmom_nh(2) + (1-ifl_imp_nbq) * neqmom_nh(3)

          qdm_nbq_a(l_nbq,2) = qdm_nbq_a(l_nbq,0) + 2*dtnbq*(          &
                               soundspeed_nbq**2  * rhs1_nbq(l_nbq)    &
                             - visc2_nbq_a(l_nbq) * rhsd2_nbq(l_nbq)   &
                             + dqdmdt_nbq_a(l_nbq) )            !      & 
!                + rhs2r_nbq(l_nbq+neqmom_nh(1) + neqmom_nh(2) )

          rhssum_nbq_a(l_nbq,2) = rhssum_nbq_a(l_nbq,2)  +             &
                               soundspeed_nbq**2  * rhs1_nbq(l_nbq)    &
                             - visc2_nbq_a(l_nbq) * rhsd2_nbq(l_nbq)

        enddo 

# ifdef DEBUG
        l_nbq=110
        i     = l2imom_nh (l_nbq)
        j     = l2jmom_nh (l_nbq)
        k     = l2kmom_nh (l_nbq)
        print *,'STEP3D_NBQ QDM STEP: ',i,j,k,neqmom_nh(1),iic,iif
        print *,'                     ',qdm_nbq_a(l_nbq,0),qdm_nbq_a(l_nbq,1)
        print *,'                     ',wz(i,j,k,nnew)
        print *,'                     ',dqdmdt_nbq_a(l_nbq),rhssum_nbq_a(l_nbq,2)
        print *,'                     ',soundspeed_nbq**2,rhs1_nbq(l_nbq)
        print *,'                     ',visc2_nbq_a(l_nbq),rhsd2_nbq(l_nbq)
        print *,'                     ',dqdmdt_nbq_a(l_nbq)
# endif
!
!-------------------------------------------------------------------
!       Partie implicite
!-------------------------------------------------------------------
!
# ifdef TOTO
        if (ifl_imp_nbq.eq.1) then
          call implicit_nbq (2)
          call implicit_nbq (3)
        endif
# endif
!
!-------------------------------------------------------------------
!......Horizontal momentum boundary conditions
!-------------------------------------------------------------------
!
# ifdef TOTO
        call obc_nbq
# endif
!
!-------------------------------------------------------------------
!......Message passing 
!-------------------------------------------------------------------
!
# ifdef TOTO_PARALLEL
!  Send
        call parallele_nbq(5)

!  Wait/receive
        call parallele_nbq(17)
# endif
!
!-------------------------------------------------------------------
!......Mass equation: leapfrog time stepping:
!-------------------------------------------------------------------
!
        do l_nbq = 1 , nzq_nh 

          rhp_nbq_a(l_nbq,2) = rhp_nbq_a(l_nbq,0)                  &
                             - div_nbq_a(l_nbq,1) * 2*dtnbq      ! &
       !!!!!                 + rhs1_nbq(neqmom_nh(0)+l_nbq)*rhsd3_nbq(l_nbq)
       !!!!! CXA CORRECTION DE PENTE A FINIR DE CODER !!!!!

        enddo

# ifdef DEBUG
        l_nbq=10
        i     = l2imom_nh (l_nbq)
        j     = l2jmom_nh (l_nbq)
        k     = l2kmom_nh (l_nbq)
        print *,'STEP3D_NBQ      RHO STEP: ',i,j,k,nzq_nh
        print *,'                           ',rhp_nbq_a(l_nbq,0),rhp_nbq_a(l_nbq,2)
# endif
!
!-------------------------------------------------------------------
!...... Mass equation: switch time indices (move forward)
!-------------------------------------------------------------------
!
        call density_nbq(7)
!
!*******************************************************************
!*******************************************************************

      enddo    ! NBQ loop

!*******************************************************************
!*******************************************************************
!
!-------------------------------------------------------------------
!......Echanges "boucle NBQ" wait/reception qdm
!-------------------------------------------------------------------
!
# ifdef TOTO_PARALLEL
      call parallele_nbq(15)
# endif
!
!-------------------------------------------------------------------
!......Move forward: momemtum
!-------------------------------------------------------------------
!
      call ru_nbq(7)
!
!-------------------------------------------------------------------
!......Set NBQ/EXT coupling terms
!-------------------------------------------------------------------
!
      call ru_nbq(2)
      call density_nbq(2)


      end subroutine step3d_nbq

#else
      subroutine step3d_nbq_empty
      end subroutine step3d_nbq_empty
#endif
