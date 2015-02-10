#include "cppdefs.h"
#ifdef NBQ
!------------------------------------------------------------------------------
!
!> @note <a href="http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm"> Main web documentation </a>
!
! DESCRIPTION: 
!
!> @brief SNBQ momentum routine.
!
!> @details 
!! - ichoix=6 pressure gradient calculation for the SNBQ momentum equation
!
! REVISION HISTORY:
!
!> @authors
!> @date 2015 January
!> @todo
!
!------------------------------------------------------------------------------

      subroutine ru_nbq(ichoix)

!__________________________________________________________________________________
!
!                               SNH2012.14      
!                 Non-Hydrostatic & Non-Boussinesq Version  
! Laboratoire d'Aerologie, 14 Avenue Edouard Belin, F-31400 Toulouse
! http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm  
!
!__________________________________________________________________________________

!      use module_parameter
!      use module_principal
      use module_nh 
!      use module_parallele
      use module_nbq
      implicit none
# include "param_F90.h"
# include "scalars_F90.h"
# include "ocean3d_F90.h"
# include "nbq.h"
# include "grid.h"
      integer i,j,k
      integer :: ichoix

      if (ichoix.eq.0) then
!
!*******************************************************************
!  Fill external and internal forcing terms
!  and reinitialize momentum fields
!*******************************************************************
!
# if defined NBQ_REINIT || !defined M2FILTER_NONE
!
       do l_nbq=1,neqmom_nh(1)
           i=l2imom_nh(l_nbq)
           j=l2jmom_nh(l_nbq)
           k=l2kmom_nh(l_nbq)
#  ifdef NBQ_REINIT
             qdm_nbq_a(l_nbq,0)=u(i,j,k,nnew)  ! contains Hz*u(nstp)
             qdm_nbq_a(l_nbq,1)=u(i,j,k,nnew)
#  else
             qdm_nbq_a(l_nbq,:)=qdm_nbq_a_bak(l_nbq,:)
#  endif
       enddo
       do l_nbq=neqmom_nh(1)+1,neqmom_nh(1)+neqmom_nh(2)
           i=l2imom_nh(l_nbq)
           j=l2jmom_nh(l_nbq)
           k=l2kmom_nh(l_nbq)
#  ifdef NBQ_REINIT
             qdm_nbq_a(l_nbq,0)=v(i,j,k,nnew)   ! contains Hz*v(nstp)
             qdm_nbq_a(l_nbq,1)=v(i,j,k,nnew)
#  else
             qdm_nbq_a(l_nbq,:)=qdm_nbq_a_bak(l_nbq,:)
#  endif
       enddo
       do l_nbq=neqmom_nh(1)+neqmom_nh(2)+1,neqmom_nh(1)+neqmom_nh(2)+neqmom_nh(3)
           i=l2imom_nh(l_nbq)
           j=l2jmom_nh(l_nbq)
           k=l2kmom_nh(l_nbq)
#  ifdef NBQ_REINIT
             qdm_nbq_a(l_nbq,0)=wz(i,j,k,nnew)  ! contains Hz*wz(nstp)
             qdm_nbq_a(l_nbq,1)=wz(i,j,k,nnew)

#  else
             qdm_nbq_a(l_nbq,:)=qdm_nbq_a_bak(l_nbq,:)
#  endif
       enddo
# endif

      elseif (ichoix.eq.1) then
!
!*******************************************************************
!  Fill external and internal forcing terms
!  and reinitialize momentum fields
!*******************************************************************
!
       do l_nbq=1,neqmom_nh(1)
           i=l2imom_nh(l_nbq)
           j=l2jmom_nh(l_nbq)
           k=l2kmom_nh(l_nbq)
           dqdmdt_nbq_a(l_nbq)=ruint_nbq(i,j,k)+ruext_nbq(i,j,k)
       enddo

       do l_nbq=neqmom_nh(1)+1,neqmom_nh(1)+neqmom_nh(2)
           i=l2imom_nh(l_nbq)
           j=l2jmom_nh(l_nbq)
           k=l2kmom_nh(l_nbq)
           dqdmdt_nbq_a(l_nbq)=rvint_nbq(i,j,k)+rvext_nbq(i,j,k)
       enddo

       do l_nbq=neqmom_nh(1)+neqmom_nh(2)+1,neqmom_nh(1)+neqmom_nh(2)+neqmom_nh(3)
           i=l2imom_nh(l_nbq)
           j=l2jmom_nh(l_nbq)
           k=l2kmom_nh(l_nbq)
           dqdmdt_nbq_a(l_nbq)=rwint_nbq(i,j,k)
       enddo

      elseif (ichoix.eq.2) then
!
!*******************************************************************
!  Prepare feedback of NBQ rhs terms to external equations
!*******************************************************************
!
        rubar_nbq(:,:)=0.
        do l_nbq=1,neqmom_nh(1)
            i=l2imom_nh(l_nbq)
            j=l2jmom_nh(l_nbq)
            k=l2kmom_nh(l_nbq)

            ru_nbq_ext(i,j,k) = ( rhssum_nbq_a(l_nbq,1)+rhssum_nbq_a(l_nbq,2)) &
                                  / real(2*ndtnbq)                             &
                                  * on_u(i,j)*om_u(i,j)
 
            rhssum_nbq_a(l_nbq,1) = rhssum_nbq_a(l_nbq,2)
            rhssum_nbq_a(l_nbq,2) = 0.

            rubar_nbq(i,j)        = rubar_nbq(i,j)+ru_nbq_ext(i,j,k)
        enddo

        rvbar_nbq(:,:)=0.
        do l_nbq=neqmom_nh(1)+1,neqmom_nh(1)+neqmom_nh(2)
            i=l2imom_nh(l_nbq)
            j=l2jmom_nh(l_nbq)
            k=l2kmom_nh(l_nbq)

            rv_nbq_ext(i,j,k) = ( rhssum_nbq_a(l_nbq,1)+rhssum_nbq_a(l_nbq,2)) &
                                  / real(2*ndtnbq)                             &
                                  * on_v(i,j)*om_v(i,j)

            rhssum_nbq_a(l_nbq,1) = rhssum_nbq_a(l_nbq,2)
            rhssum_nbq_a(l_nbq,2) = 0.

            rvbar_nbq(i,j)        = rvbar_nbq(i,j)+rv_nbq_ext(i,j,k)
        enddo

        do l_nbq=neqmom_nh(1)+neqmom_nh(2)+1,neqmom_nh(1)+neqmom_nh(2)+neqmom_nh(3)
            i = l2imom_nh (l_nbq)
            j = l2jmom_nh (l_nbq)
            k = l2kmom_nh (l_nbq)

            rw_nbq_ext(i,j,k) = ( rhssum_nbq_a(l_nbq,1)+rhssum_nbq_a(l_nbq,2)) &
                                  / real(2*ndtnbq)                             &
                                  * on_r(i,j)*om_r(i,j)

            rhssum_nbq_a(l_nbq,1) = rhssum_nbq_a(l_nbq,2)
            rhssum_nbq_a(l_nbq,2) = 0.
        enddo


      elseif (ichoix.eq.6) then
!
!*******************************************************************
!  Increment momentum:
!*******************************************************************
!
        call amux(                                                   &
!              neqmom_nh(0)                                          &
               neqcorrt_nbq                                          &
              ,rhp_nbq_a(1:nzq_nh,1)                                 &  
!             ,rhs1_nbq(1:neqmom_nh(0))                              &
              ,rhs1_nbq(1:neqcorrt_nbq)                              &
              ,momvg_nh(1:)                                          &
              ,momj_nh(1:)                                           & 
              ,momi_nh(1:)       ) 
   
      elseif (ichoix.eq.7) then
!
!*******************************************************************
!  Move forward momentum
!*******************************************************************
!
         do l_nbq = 1 , neqmom_nh(0)
!          qdm_nbq_a  (l_nbq,-2) = qdm_nbq_a(l_nbq,-1) 
!          qdm_nbq_a  (l_nbq,-1) = qdm_nbq_a(l_nbq,0) 
          qdm_nbq_a  (l_nbq,0)  = qdm_nbq_a(l_nbq,1) 
          qdm_nbq_a  (l_nbq,1)  = qdm_nbq_a(l_nbq,2)
         enddo


       endif  ! ichoix

       return
       end
#else
      subroutine ru_nbq_empty 
      return
      end
#endif

