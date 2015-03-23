#include "cppdefs.h"
#ifdef NBQ

      subroutine ru_nbq(ichoix)

!**********************************************************************
!
!                      Various Computations related to
!                            NBQ momentum
!
!> @note <a href="http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm"> Main web documentation </a>
!
! DESCRIPTION: 
!
!> @brief NBQ momentum routine.
!
! REVISION HISTORY:
!
!> @authors
!> @date 2015 January
!> @todo
!
!**********************************************************************

      use module_nh 
      use module_nbq
      implicit none

# include "param_F90.h"
# include "scalars_F90.h"
# include "grid.h"
# include "ocean3d.h"
# include "nbq.h"

      integer i,j,k, indm_r
      integer :: ichoix

!      real sum_nbq(GLOBAL_2D_ARRAY,2)

      if (ichoix.eq.0) then
!
!*******************************************************************
!  Fill external and internal forcing terms
!  and reinitialize momentum fields
!  If reinitialization only!
!*******************************************************************
!
# if defined NBQ_REINIT || !defined M2FILTER_NONE
!
       do l_nbq=1,neqmom_nh(1)
           i=l2imom_nh(l_nbq)
           j=l2jmom_nh(l_nbq)
           k=l2kmom_nh(l_nbq)
#  ifdef NBQ_REINIT
             qdm_nbq_a(l_nbq,0)=u(i,j,k,nnew)   & ! WARNING: u(nnew) <-- Hz*u(nstp)
                              *rho0/(1. RHO0)    
             qdm_nbq_a(l_nbq,1)=qdm_nbq_a(l_nbq,0)
#  else
             qdm_nbq_a(l_nbq,:)=qdm_nbq_a_bak(l_nbq,:)
#  endif
       enddo
       do l_nbq=neqmom_nh(1)+1,neqmom_nh(1)+neqmom_nh(2)
           i=l2imom_nh(l_nbq)
           j=l2jmom_nh(l_nbq)
           k=l2kmom_nh(l_nbq)
#  ifdef NBQ_REINIT
             qdm_nbq_a(l_nbq,0)=v(i,j,k,nnew)   & ! WARNING: v(nnew) <-- Hz*v(nstp)
                              *rho0/(1. RHO0)
             qdm_nbq_a(l_nbq,1)=qdm_nbq_a(l_nbq,0)
#  else
             qdm_nbq_a(l_nbq,:)=qdm_nbq_a_bak(l_nbq,:)
#  endif
       enddo
       do l_nbq=neqmom_nh(1)+neqmom_nh(2)+1,neqmom_nh(1)+neqmom_nh(2)+neqmom_nh(3)
           i=l2imom_nh(l_nbq)
           j=l2jmom_nh(l_nbq)
           k=l2kmom_nh(l_nbq)
#  ifdef NBQ_REINIT
             qdm_nbq_a(l_nbq,0)=wz(i,j,k,nnew)  & ! WARNING: wz(nnew) <-- Hz*wz(nstp)
                               *rho0/(1. RHO0)
             qdm_nbq_a(l_nbq,1)=qdm_nbq_a(l_nbq,0)

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
           dqdmdt_nbq_a(l_nbq)=(ruint_nbq(i,j,k)+ruext_nbq(i,j,k))  &
                                                   *rho0/(1. RHO0)
       enddo

       do l_nbq=neqmom_nh(1)+1,neqmom_nh(1)+neqmom_nh(2)
           i=l2imom_nh(l_nbq)
           j=l2jmom_nh(l_nbq)
           k=l2kmom_nh(l_nbq)
           dqdmdt_nbq_a(l_nbq)=(rvint_nbq(i,j,k)+rvext_nbq(i,j,k))  &
                                                   *rho0/(1. RHO0)
       enddo

       do l_nbq=neqmom_nh(1)+neqmom_nh(2)+1,neqmom_nh(1)+neqmom_nh(2)+neqmom_nh(3)
           i=l2imom_nh(l_nbq)
           j=l2jmom_nh(l_nbq)
           k=l2kmom_nh(l_nbq)
           dqdmdt_nbq_a(l_nbq)=rwint_nbq(i,j,k) *rho0/(1. RHO0)

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

            ru_nbq_ext(i,j,k) = rhssum_nbq_a(l_nbq,2)        &
                                  / real(ndtnbq)             &
                                  * on_u(i,j)*om_u(i,j) RHO0/rho0
 
            rhssum_nbq_a(l_nbq,2) = 0.

            rubar_nbq(i,j)        = rubar_nbq(i,j)+ru_nbq_ext(i,j,k)
        enddo
        

        rvbar_nbq(:,:)=0.
        do l_nbq=neqmom_nh(1)+1,neqmom_nh(1)+neqmom_nh(2)
            i=l2imom_nh(l_nbq)
            j=l2jmom_nh(l_nbq)
            k=l2kmom_nh(l_nbq)

            rv_nbq_ext(i,j,k) = rhssum_nbq_a(l_nbq,2)        &
                                  / real(ndtnbq)             &
                                  * on_v(i,j)*om_v(i,j) RHO0/rho0

            rhssum_nbq_a(l_nbq,2) = 0.

            rvbar_nbq(i,j)        = rvbar_nbq(i,j)+rv_nbq_ext(i,j,k)
        enddo

        do l_nbq=neqmom_nh(1)+neqmom_nh(2)+1,neqmom_nh(1)+neqmom_nh(2)+neqmom_nh(3)
            i = l2imom_nh (l_nbq)
            j = l2jmom_nh (l_nbq)
            k = l2kmom_nh (l_nbq)

            rw_nbq_ext(i,j,k) = rhssum_nbq_a(l_nbq,2)        &
                                  / real(ndtnbq)             &
                                  * on_r(i,j)*om_r(i,j) RHO0/rho0

            rhssum_nbq_a(l_nbq,2) = 0.
        enddo

      elseif (ichoix.eq.3) then
!
!**********************************************************************
! Store momentum fields at internal timestep n+1
! If not "flat filter" only !
!**********************************************************************

        qdm_nbq_a_bak(1:neqmom_nh(0),0)=qdm_nbq_a(1:neqmom_nh(0),0)
        qdm_nbq_a_bak(1:neqmom_nh(0),1)=qdm_nbq_a(1:neqmom_nh(0),1)

      elseif (ichoix.eq.6) then
!
!*******************************************************************
!  Increment momentum:
!*******************************************************************
!
        indm_r=momi_nh(nzq_nh+1)+1
        call amux(                                                   &
               neqcorrt_nbq                                          &
              ,rhp_nbq_a(1:nzq_nh,1)                                 &  
              ,rhs1_nbq(1:neqcorrt_nbq)                              &
              ,momvg_nh(1:indm_r)                                    &
              ,momj_nh(1:indm_r)                                     & 
              ,momi_nh(1:nzq_nh+1)                                   &
          !   ,nmlmom_nh                                             &
                      ) 
   
      elseif (ichoix.eq.7) then
!
!*******************************************************************
!  Move forward momentum
!*******************************************************************
!
          qdm_nbq_a  (1:neqmom_nh(0),0)  = qdm_nbq_a(1:neqmom_nh(0),1) 
          qdm_nbq_a  (1:neqmom_nh(0),1)  = qdm_nbq_a(1:neqmom_nh(0),2)

       endif  ! ichoix

       return
       end
#else
      subroutine ru_nbq_empty 
      return
      end
#endif

