#include "cppdefs.h"
#ifdef NBQ
!
!======================================================================
!                      NBQ-Mode for NH-modeling
!                            Main Routine
!======================================================================
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
!======================================================================
!
      subroutine step3d_nbq

      use module_nh 
      use module_nbq

      implicit none

# include "param_F90.h"
# include "scalars_F90.h"
# include "ocean3d.h"
# include "grid.h"
# include "nbq.h"

      real :: dum_s

# undef DEBUG
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
      if (iif.eq.ndtfast+1.and.iteration_nbq.eq.1) then
        call ru_nbq(3)
        call density_nbq(3)
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
!*******************************************************************
!*******************************************************************
!              NBQ mode iteration (main loop)
!*******************************************************************
!*******************************************************************

      do iteration_nbq=1,iteration_nbq_max

# ifdef DEBUG
        print *,'STEP3D_NBQ: it_nbq = ',iteration_nbq
# endif


!.......1st iteration NBQ: treats NBQ
        if (iteration_nbq>1) then
!        Receive
         call parallele_nbq(15)
!        Momentum equation: switch time indices (move forward)
         call ru_nbq(7)
        endif
!
!-------------------------------------------------------------------
!.......Output to debug MPI:
!-------------------------------------------------------------------
!
#ifdef DEBUG_NBQ
        call output_nbq(10)
#endif
!
!-------------------------------------------------------------------
!.......Output: qdm in Internal and NBQ modes
!-------------------------------------------------------------------
!
!        call output_nbq(30)
!
!-------------------------------------------------------------------
!      Compute divergence term (AMUX):
!          qdm_nbq_a ==> div_nbq_a
!-------------------------------------------------------------------
!
        call density_nbq(6)
!
!-------------------------------------------------------------------
!      Compute Second viscosity (product mat*vect):
!            div_nbq_a ==> rhsd2_nbq
!-------------------------------------------------------------------
!
        call viscous_nbq (1)
!
!-------------------------------------------------------------------
!......Message passing , indice switch (iif=1)
!-------------------------------------------------------------------
!
!  Send
          call parallele_nbq(7)
!
!-------------------------------------------------------------------
!......Emission eventuelle d'une onde acoustique:
!-------------------------------------------------------------------
!
!       call density_nbq(11)
!
!-------------------------------------------------------------------
!      Compute pressure gradient and gravity terms (AMUX)
!                rhp_nbq_a  ==> rhs1_nbq
!-------------------------------------------------------------------
!
        call ru_nbq(6)

!-------------------------------------------------------------------
!      Momentum equation: leapfrog time stepping
!-------------------------------------------------------------------
!
        do l_nbq = 1 , neqmom_nh(0) 

          dum_s =             soundspeed_nbq**2  * rhs1_nbq(l_nbq)     &
                             - visc2_nbq_a(l_nbq) * rhsd2_nbq(l_nbq)   

          qdm_nbq_a(l_nbq,2) = qdm_nbq_a(l_nbq,0)  + 2.*dtnbq*(        &
                               dum_s                                   &
                             + dqdmdt_nbq_a(l_nbq) )        

          rhssum_nbq_a(l_nbq,2) = rhssum_nbq_a(l_nbq,2)  +  dum_s       

        enddo 
!
!-------------------------------------------------------------------
!......Horizontal momentum boundary conditions
!-------------------------------------------------------------------
!
# ifdef OBC_NBQ
!        call obc_nbq
# endif

!
!-------------------------------------------------------------------
!......Message passing 
!-------------------------------------------------------------------
!
!  Send
        call parallele_nbq(5)

! Receive
        call parallele_nbq(17)
!
!-------------------------------------------------------------------
!......Mass equation: leapfrog time stepping:
!-------------------------------------------------------------------
!
#ifndef NBQ_DRHODT
        do l_nbq = 1 , nzq_nh 
          rhp_nbq_a(l_nbq,2) = rhp_nbq_a(l_nbq,0)                  &
                             - div_nbq_a(l_nbq,1) * 2. * dtnbq 
        enddo
#else
        do l_nbq = 1 , nzq_nh 
          rhp_nbq_a(l_nbq,2) = rhp_nbq_a(l_nbq,0)                  &
                             - div_nbq_a(l_nbq,1) * 2. * dtnbq     &
                             + rhs1_nbq(neqmom_nh(0)+l_nbq)
        enddo
#endif
!
!-------------------------------------------------------------------
!.......Output to debug MPI:
!-------------------------------------------------------------------
!
#ifdef DEBUG_NBQ
         call output_nbq(11)
#endif
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
!
!-------------------------------------------------------------------
!......Message passing 
!-------------------------------------------------------------------
!
! Receive
       call parallele_nbq(15)

!-------------------------------------------------------------------
!......Move forward: momentum
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
