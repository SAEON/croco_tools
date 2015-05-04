#include "cppdefs.h"
#ifdef NBQ

      subroutine mat_cont_nh 
!*******************************************************************
!*******************************************************************
!*******************************************************************
!             Matrix CONT for continuity equation:
!
!           Updated at every internal mode time step
!
!*******************************************************************
!*******************************************************************
!*******************************************************************

      use module_nh
      use module_nbq

# ifdef TRACETXT
      use module_tracetxt_out
# endif

      implicit none
# include "param_F90.h"
# include "scalars_F90.h"
# include "grid.h"
# include "ocean3d.h"
# include "nbq.h"

      integer :: i,j,k,kmax

!*******************************************************************
!     Various Initializations:
!*******************************************************************

      kmax=N  !CXA

!*******************************************************************
!     Updates CONT matrix:
!*******************************************************************
      do l_nh = 1,neqcont_nh(0)

!......Equation characteristics:
       i = l2iq_nh (l_nh) 
       j = l2jq_nh (l_nh) 
       k = l2kq_nh (l_nh)      
       l1_nh = cont_nnz_nh(l_nh)

       if (k.eq.1) then
!-----------------------------------------------------------
!                         bottom
!-----------------------------------------------------------

!-----------------------------
!....... u(i,j,k):
!-----------------------------
     
!CXA    contv_nh(l1_nh) = - dy_u(i,j) / dxdy_t(i,j)                    &
!                         - coefb_u(i,j,k) * gdepth_u(i,j,k)           &
!                         / dz_u(i,j,k,1)
        contv_nh(l1_nh) = - on_u(i,j)*pm(i,j)*pn(i,j)                  &
                          - coefb_u(i,j,k) * gdepth_u(i,j,k)           &
                          / (0.5*( Hzr_half_nbq(i-1,j,k) +             &
                                    Hzr_half_nbq(i,j,k) ) )
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,1)

!-----------------------------
!....... v(i,j,k):
!-----------------------------
!CXA    contv_nh(l1_nh) = - dx_v(i,j)  / dxdy_t(i,j)                   &
!                         - coefb_v(i,j,k) * gdepth_v(i,j,k)           &
!                         / dz_v(i,j,k,1)     
        contv_nh(l1_nh) = - om_v(i,j)*pm(i,j)*pn(i,j)                  &
                          - coefb_v(i,j,k) * gdepth_v(i,j,k)           &
                          / (0.5*(Hzr_half_nbq(i,j-1,k)+               &
                                  Hzr_half_nbq(i,j,k)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,2)

!-----------------------------
!....... u(i,j,k+1):
!-----------------------------
!CXA    contv_nh(l1_nh) = - coefa_u(i,j,k+1) * gdepth_u(i,j,k+1)       &
!                         / dz_u(i,j,k+1,1)      
        contv_nh(l1_nh) = - coefa_u(i,j,k+1) * gdepth_u(i,j,k+1)       &
                          / (0.5*(Hzr_half_nbq(i-1,j,k+1)+             &
                                  Hzr_half_nbq(i,j,k+1)))     
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k+1,1)

!-----------------------------
!....... v(i,j,k+1):
!-----------------------------
!CXA    contv_nh(l1_nh) = - coefa_v(i,j,k+1) * gdepth_v(i,j,k+1)       &
!                         / dz_v(i,j,k+1,1)     
        contv_nh(l1_nh) = - coefa_v(i,j,k+1) * gdepth_v(i,j,k+1)       &
                          / (0.5*(Hzr_half_nbq(i,j-1,k+1)+             &
                                  Hzr_half_nbq(i,j,k+1)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k+1,2)

!-----------------------------
!....... u(i+1,j,k):
!-----------------------------
!CXA    contv_nh(l1_nh) =   dy_u(i+1,j) / dxdy_t(i,j)                  &
!                         - coefb_u(i+1,j,k) * gdepth_u(i+1,j,k)       &
!                         / dz_u(i+1,j,k,1)
        contv_nh(l1_nh) =   on_u(i+1,j)*pm(i,j)*pn(i,j)                &
                          - coefb_u(i+1,j,k) * gdepth_u(i+1,j,k)       &
                          / (0.5*(Hzr_half_nbq(i,j,k)+                 &
                                  Hzr_half_nbq(i+1,j,k)))
        l1_nh = l1_nh + mijk2lmom_nh(i+1,j,k,1)

!-----------------------------
!....... v(i,j+1,k):
!-----------------------------
!CXA    contv_nh(l1_nh) =   dx_v(i,j+1)  / dxdy_t(i,j)                 &
!                         - coefb_v(i,j+1,k) * gdepth_v(i,j+1,k)       &
!                         / dz_v(i,j+1,k,1)
        contv_nh(l1_nh) =   om_v(i,j+1)*pm(i,j)*pn(i,j)                &
                          - coefb_v(i,j+1,k) * gdepth_v(i,j+1,k)       &
                          / (0.5*(Hzr_half_nbq(i,j,k)+                 &
                                  Hzr_half_nbq(i,j+1,k)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j+1,k,2)

!-----------------------------
!....... u(i+1,j,k+1):
!-----------------------------
!CXA    contv_nh(l1_nh) = - coefa_u(i+1,j,k+1) * gdepth_u(i+1,j,k+1)   &
!                         / dz_u(i+1,j,k+1,1)  
        contv_nh(l1_nh) = - coefa_u(i+1,j,k+1) * gdepth_u(i+1,j,k+1)   &
                          / (0.5*(Hzr_half_nbq(i,j,k+1)+               &
                                  Hzr_half_nbq(i+1,j,k+1)))
        l1_nh = l1_nh + mijk2lmom_nh(i+1,j,k+1,1)

!-----------------------------
!....... v(i,j+1,k+1):
!-----------------------------
!CXA    contv_nh(l1_nh) = - coefa_v(i,j+1,k+1) * gdepth_v(i,j+1,k+1)   &
!                         / dz_v(i,j+1,k+1,1)
        contv_nh(l1_nh) = - coefa_v(i,j+1,k+1) * gdepth_v(i,j+1,k+1)   &
                          / (0.5*(Hzr_half_nbq(i,j,k+1)+               &
                                  Hzr_half_nbq(i,j+1,k+1)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j+1,k+1,2) 

!-----------------------------
!....... w(i,j,k):
!-----------------------------
!CXA    contv_nh(l1_nh) = 1. / dz_w(i,j,k+1,1)       ! index shift
!CXA    l1_nh = l1_nh + mijk2lmom_nh(i,j,k+1,3)
!CXA    contv_nh(l1_nh) = 1. / dz_w(i,j,k,1)         ! roms conversion
        contv_nh(l1_nh) = 1. / Hzw_half_nbq(i,j,k)
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,3)

!-----------------------------
!....... w(i,j,k-1):  CXA ici on choisit le facteur d échelle pour
!                         verifier une condition de frontiere 
!-----------------------------
!CXA     contv_nh(l1_nh) = -1. / dz_w(i,j,k,1)    ! index shift
!CXA     l1_nh = l1_nh + mijk2lmom_nh(i,j,k,3)
!CXA     contv_nh(l1_nh) = -1. / dz_w(i,j,k-1,1)  ! roms conversion
         contv_nh(l1_nh) = -1. / Hzw_half_nbq(i,j,k-1)
         l1_nh = l1_nh + mijk2lmom_nh(i,j,k-1,3)

       endif ! (k=1)


       if (k.ge.2.and.k.le.kmax-1) then
!-----------------------------------------------------------
!                       Inner Domain
!                      k in [2,kmax-1]
!-----------------------------------------------------------

!-----------------------------
!....... u(i,j,k):
!-----------------------------
!CXA    contv_nh(l1_nh) = - dy_u(i,j) / dxdy_t(i,j)                    &
!                         - ( coefb_u(i,j,k) - coefa_u(i,j,k) )        & 
!                         * gdepth_u(i,j,k) / dz_u(i,j,k,1)                 
        contv_nh(l1_nh) = - on_u(i,j)*pm(i,j)*pn(i,j)                  &
                          - ( coefb_u(i,j,k) - coefa_u(i,j,k) )        &
                          * gdepth_u(i,j,k)                            &
                            / (0.5*(Hzr_half_nbq(i-1,j,k)+             &
                                    Hzr_half_nbq(i,j,k)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,1)

!-----------------------------
!....... v(i,j,k):
!-----------------------------
!CXA    contv_nh(l1_nh) = - dx_v(i,j) / dxdy_t(i,j)                    &
!                         - ( coefb_v(i,j,k) - coefa_v(i,j,k) )        &
!                         * gdepth_v(i,j,k) / dz_v(i,j,k,1)&
        contv_nh(l1_nh) = - om_v(i,j)*pm(i,j)*pn(i,j)                  &
                          - ( coefb_v(i,j,k) - coefa_v(i,j,k) )        &
                          * gdepth_v(i,j,k)                            &
                             / (0.5*(Hzr_half_nbq(i,j-1,k)+            &
                                     Hzr_half_nbq(i,j,k)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,2)

!-----------------------------
!....... u(i,j,k+1):
!-----------------------------
!CXA    contv_nh(l1_nh) = - coefa_u(i,j,k+1) * gdepth_u(i,j,k+1)       &
!                         / dz_u(i,j,k+1,1)  
        contv_nh(l1_nh) = - coefa_u(i,j,k+1) * gdepth_u(i,j,k+1)       &
                          / (0.5*(Hzr_half_nbq(i-1,j,k+1)+             &
                                  Hzr_half_nbq(i,j,k+1)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k+1,1)

!-----------------------------
!....... v(i,j,k+1):
!-----------------------------
!CXA    contv_nh(l1_nh) = - coefa_v(i,j,k+1) * gdepth_v(i,j,k+1)       &
!                         / dz_v(i,j,k+1,1)    
        contv_nh(l1_nh) = - coefa_v(i,j,k+1) * gdepth_v(i,j,k+1)       &
                          / (0.5*(Hzr_half_nbq(i,j-1,k+1)+             &
                                  Hzr_half_nbq(i,j,k+1)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k+1,2)

!-----------------------------
!....... u(i,j,k-1):
!-----------------------------
!CXA    contv_nh(l1_nh) = + coefb_u(i,j,k-1) * gdepth_u(i,j,k-1)       &
!                         / dz_u(i,j,k-1,1) 
        contv_nh(l1_nh) = + coefb_u(i,j,k-1) * gdepth_u(i,j,k-1)       &
                          / (0.5*(Hzr_half_nbq(i-1,j,k-1)+             &
                                  Hzr_half_nbq(i,j,k-1)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k-1,1)

!-----------------------------
!....... v(i,j,k-1):
!-----------------------------
!CXA    contv_nh(l1_nh) = + coefb_v(i,j,k-1) * gdepth_v(i,j,k-1)       &
!                         / dz_v(i,j,k-1,1)  
        contv_nh(l1_nh) = + coefb_v(i,j,k-1) * gdepth_v(i,j,k-1)       &
                          / (0.5*(Hzr_half_nbq(i,j-1,k-1)+             &
                                  Hzr_half_nbq(i,j,k-1)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k-1,2)

!-----------------------------
!....... u(i+1,j,k):
!-----------------------------
!CXA    contv_nh(l1_nh) =   dy_u(i+1,j) / dxdy_t(i,j) 
!                         - ( coefb_u(i+1,j,k) - coefa_u(i+1,j,k) )    &
!                         * gdepth_u(i+1,j,k) / dz_u(i+1,j,k,1)        
        contv_nh(l1_nh) =   on_u(i+1,j)*pm(i,j)*pn(i,j)                &
                          - ( coefb_u(i+1,j,k) - coefa_u(i+1,j,k) )    &
                          * gdepth_u(i+1,j,k)                          &
                          / (0.5*(Hzr_half_nbq(i,j,k)+                 &
                                  Hzr_half_nbq(i+1,j,k)))
        l1_nh = l1_nh + mijk2lmom_nh(i+1,j,k,1)

!-----------------------------
!....... v(i,j+1,k):
!-----------------------------
!CXA    contv_nh(l1_nh) =   dx_v(i,j+1) / dxdy_t(i,j)                  &
!                         - ( coefb_v(i,j+1,k) - coefa_v(i,j+1,k) )    &
!                         * gdepth_v(i,j+1,k) / dz_v(i,j+1,k,1)       
        contv_nh(l1_nh)  =   om_v(i,j+1)*pm(i,j)*pn(i,j)               &
                           - ( coefb_v(i,j+1,k) - coefa_v(i,j+1,k) )   &
                           * gdepth_v(i,j+1,k)                         &
                           / (0.5*(Hzr_half_nbq(i,j,k)+                &
                                   Hzr_half_nbq(i,j+1,k)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j+1,k,2)

!-----------------------------
!....... u(i+1,j,k+1):
!-----------------------------
!CXA    contv_nh(l1_nh) = - coefa_u(i+1,j,k+1) * gdepth_u(i+1,j,k+1)   &
!                         / dz_u(i+1,j,k+1,1) 
        contv_nh(l1_nh) = - coefa_u(i+1,j,k+1) * gdepth_u(i+1,j,k+1)   &
                          / (0.5*(Hzr_half_nbq(i,j,k+1)+               &
                                  Hzr_half_nbq(i+1,j,k+1)))
        l1_nh = l1_nh + mijk2lmom_nh(i+1,j,k+1,1)

!-----------------------------
!....... v(i,j+1,k+1):
!-----------------------------
!CXA    contv_nh(l1_nh) = - coefa_v(i,j+1,k+1) * gdepth_v(i,j+1,k+1)   &
!                         / dz_v(i,j+1,k+1,1)         
        contv_nh(l1_nh) = - coefa_v(i,j+1,k+1) * gdepth_v(i,j+1,k+1)   &
                          / (0.5*(Hzr_half_nbq(i,j,k+1)+               &
                                  Hzr_half_nbq(i,j+1,k+1)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j+1,k+1,2)

!-----------------------------
!....... u(i+1,j,k-1):
!-----------------------------
!CXA    contv_nh(l1_nh) = + coefb_u(i+1,j,k-1) * gdepth_u(i+1,j,k-1)   &
!                          / dz_u(i+1,j,k-1,1)   
        contv_nh(l1_nh) = + coefb_u(i+1,j,k-1) * gdepth_u(i+1,j,k-1)   &
                          / (0.5*(Hzr_half_nbq(i,j,k-1)+               &
                                  Hzr_half_nbq(i+1,j,k-1)))
        l1_nh = l1_nh + mijk2lmom_nh(i+1,j,k-1,1)

!-----------------------------
!....... v(i,j+1,k-1):
!-----------------------------
!CXA    contv_nh(l1_nh) = + coefb_v(i,j+1,k-1) * gdepth_v(i,j+1,k-1)   &
!                         / dz_v(i,j+1,k-1,1)    
        contv_nh(l1_nh) = + coefb_v(i,j+1,k-1) * gdepth_v(i,j+1,k-1)   &
                          / (0.5*(Hzr_half_nbq(i,j,k-1)+               &
                                  Hzr_half_nbq(i,j+1,k-1)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j+1,k-1,2)

!-----------------------------
!....... w(i,j,k):
!-----------------------------
!CXA    contv_nh(l1_nh) = 1. / dz_w(i,j,k+1,1)  
!CXA    l1_nh = l1_nh + mijk2lmom_nh(i,j,k+1,3)
!CXA    contv_nh(l1_nh) = 1. / dz_w(i,j,k,1)         ! roms conversion
        contv_nh(l1_nh) = 1. / Hzw_half_nbq(i,j,k)
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,3)
 
!-----------------------------
!....... w(i,j,k-1):
!-----------------------------
!CXA    contv_nh(l1_nh) = -1. / dz_w(i,j,k,1)
!CXA    l1_nh = l1_nh + mijk2lmom_nh(i,j,k,3)
!CXA    contv_nh(l1_nh) = -1. / dz_w(i,j,k-1,1)   ! roms conversion
        contv_nh(l1_nh) = -1. / Hzw_half_nbq(i,j,k-1)
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k-1,3)

        endif ! (k>1     k<kmax)


        if (k.eq.kmax) then
!-----------------------------------------------------------
!                      surface
!-----------------------------------------------------------

!-----------------------------
!....... u(i,j,k):
!-----------------------------
!CXA    contv_nh(l1_nh) = - dy_u(i,j)  / dxdy_t(i,j)                   &
!                         + ( coefa_u(i,j,k) * gdepth_u(i,j,k)         &
!CXA                      - coefb_u(i,j,k+1) * gdepth_u(i,j,k+1) )     &
!                         / dz_u(i,j,k,1)   
        contv_nh(l1_nh) = - on_u(i,j)*pm(i,j)*pn(i,j)                  &
                          + ( coefa_u(i,j,k) * gdepth_u(i,j,k)         &
                          - coefb_u(i,j,k+1) * gdepth_u(i,j,k+1) )     &
                          / (0.5*(Hzr_half_nbq(i-1,j,k)+               &
                                  Hzr_half_nbq(i,j,k)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,1)
           
!-----------------------------
!....... v(i,j,k):
!-----------------------------
!CXA    contv_nh(l1_nh) = - dx_v(i,j) / dxdy_t(i,j)                    &
!                         + ( coefa_v(i,j,k) * gdepth_v(i,j,k)         &
!CXA                      - coefb_v(i,j,k+1) * gdepth_v(i,j,k+1) )     &
!                         / dz_v(i,j,k,1)  
        contv_nh(l1_nh) = - om_v(i,j)*pm(i,j)*pn(i,j)                  &
                           + ( coefa_v(i,j,k) * gdepth_v(i,j,k)        &
                           - coefb_v(i,j,k+1) * gdepth_v(i,j,k+1) )    &
                           / (0.5*(Hzr_half_nbq(i,j-1,k)+              &
                                   Hzr_half_nbq(i,j,k)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,2)

!-----------------------------
!....... u(i,j,k+1):
!-----------------------------

!-----------------------------
!....... v(i,j,k+1):
!-----------------------------

!-----------------------------
!....... u(i,j,k-1):
!-----------------------------
!CXA    contv_nh(l1_nh) = + coefb_u(i,j,k-1) * gdepth_u(i,j,k-1)       &
!                         / dz_u(i,j,k-1,1)  
        contv_nh(l1_nh) = + coefb_u(i,j,k-1) * gdepth_u(i,j,k-1)       &
                          / (0.5*(Hzr_half_nbq(i-1,j,k-1)+             &
                                  Hzr_half_nbq(i,j,k-1)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k-1,1)

!-----------------------------
!....... v(i,j,k-1):
!-----------------------------
!CXA    contv_nh(l1_nh) = + coefb_v(i,j,k-1) * gdepth_v(i,j,k-1)       &
!                         / dz_v(i,j,k-1,1)
        contv_nh(l1_nh) = + coefb_v(i,j,k-1) * gdepth_v(i,j,k-1)       &
                          / (0.5*(Hzr_half_nbq(i,j-1,k-1)+             &
                                  Hzr_half_nbq(i,j,k-1)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k-1,2)

!-----------------------------
!....... u(i+1,j,k):
!-----------------------------
!CXA    contv_nh(l1_nh) = dy_u(i+1,j) / dxdy_t(i,j)                    &
!                         + ( coefa_u(i+1,j,k) * gdepth_u(i+1,j,k)     &
!CXA                      - coefb_u(i+1,j,k+1) * gdepth_u(i+1,j,k+1) ) &
!                         / dz_u(i+1,j,k,1)   
        contv_nh(l1_nh) =   on_u(i+1,j)*pm(i,j)*pn(i,j) +              &
                            ( coefa_u(i+1,j,k) * gdepth_u(i+1,j,k)     &
                          - coefb_u(i+1,j,k+1) * gdepth_u(i+1,j,k+1) ) &
                          / (0.5*(Hzr_half_nbq(i,j,k)+                 &
                                  Hzr_half_nbq(i+1,j,k)))
        l1_nh = l1_nh + mijk2lmom_nh(i+1,j,k,1)

!-----------------------------
!....... v(i,j+1,k):
!-----------------------------
!CXA    contv_nh(l1_nh) =   dx_v(i,j+1) / dxdy_t(i,j)                  &
!                         + ( coefa_v(i,j+1,k) * gdepth_v(i,j+1,k)     &
!CXA                      -  coefb_v(i,j+1,k+1) * gdepth_v(i,j+1,k+1)) &
!                         / dz_v(i,j+1,k,1)  
        contv_nh(l1_nh) =   om_v(i,j+1)*pm(i,j)*pn(i,j) +              &
                            ( coefa_v(i,j+1,k) * gdepth_v(i,j+1,k)     &
                          - coefb_v(i,j+1,k+1) * gdepth_v(i,j+1,k+1))  &
                          / (0.5*(Hzr_half_nbq(i,j,k)+                 &
                                  Hzr_half_nbq(i,j+1,k)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j+1,k,2)

!-----------------------------
!....... u(i+1,j,k+1):
!-----------------------------

!-----------------------------
!....... v(i,j+1,k+1):
!-----------------------------

!-----------------------------
!....... u(i+1,j,k-1):
!-----------------------------
!CXA    contv_nh(l1_nh) = + coefb_u(i+1,j,k-1) * gdepth_u(i+1,j,k-1)   &
!                         / dz_u(i+1,j,k-1,1)   
        contv_nh(l1_nh) = + coefb_u(i+1,j,k-1) * gdepth_u(i+1,j,k-1)   &
                          / (0.5*(Hzr_half_nbq(i,j,k-1)+               &
                                  Hzr_half_nbq(i+1,j,k-1)))
        l1_nh = l1_nh + mijk2lmom_nh(i+1,j,k-1,1)

!-----------------------------
!....... v(i,j+1,k-1):
!-----------------------------
!CXA    contv_nh(l1_nh) = + coefb_v(i,j+1,k-1) * gdepth_v(i,j+1,k-1)   &
!                         / dz_v(i,j+1,k-1,1)   
        contv_nh(l1_nh) = + coefb_v(i,j+1,k-1) * gdepth_v(i,j+1,k-1)   &
                          / (0.5*(Hzr_half_nbq(i,j,k-1)+               &
                                  Hzr_half_nbq(i,j+1,k-1)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j+1,k-1,2)

!-----------------------------
!.......point w(i,j,k):
!-----------------------------
!CXA    contv_nh(l1_nh) = 1. / dz_w(i,j,k+1,1)
!CXA    l1_nh = l1_nh + mijk2lmom_nh(i,j,k+1,3)
!CXA    contv_nh(l1_nh) = 1. / dz_w(i,j,k,1)     ! roms conversion
        contv_nh(l1_nh) = 1. / Hzw_half_nbq(i,j,k)
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,3)

!-----------------------------
!....... w(i,j,k-1):
!-----------------------------
!CXA    contv_nh(l1_nh) = -1. / dz_w(i,j,k,1)
!CXA    l1_nh = l1_nh + mijk2lmom_nh(i,j,k,3)
!CXA    contv_nh(l1_nh) = -1. / dz_w(i,j,k-1,1)  ! roms conversion
        contv_nh(l1_nh) = -1. / Hzw_half_nbq(i,j,k-1)
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k-1,3)
        endif ! (k=kmax)
        
      enddo

      if (ifl_nbq.eq.1) then
!***********************************************************
!  Normalisation en NBQ / NH  qui tourne avec dz div
!***********************************************************

      do l_nh = 1,neqcont_nh(0)

!......Equation characteristics:
       i = l2iq_nh (l_nh) 
       j = l2jq_nh (l_nh) 
       k = l2kq_nh (l_nh) 

       do l1_nh = conti_nh(l_nh),conti_nh(l_nh+1)-1
!CXA     contv_nh(l1_nh) = contv_nh(l1_nh) / dz_t(i,j,k,1)
         contv_nh(l1_nh) = contv_nh(l1_nh) / Hzr_half_nbq(i,j,k)
       enddo

      enddo

      endif

!***********************************************************
!      Check!
!***********************************************************
#ifdef CHECK_CROCO
!CXA  call set_tracedirectory(iteration3d)
      call set_tracedirectory(iic)
      filetrace='mat_cont_it_'//int2string(iic)//'.'//int2string(mynode)//'.txt'
      call printmat_mm(filetrace,neqcont_nh(0),neqmom_nh(0),     & 
                       nzcont_nh,conti_nh,                       &
                       contj_nh,contv_nh)
#endif

      return
      end subroutine mat_cont_nh
#else
      subroutine mat_cont_nh_empty
      return
      end 
#endif
