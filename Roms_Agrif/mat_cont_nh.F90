#include "cppdefs.h"
#ifdef NBQ
      subroutine mat_cont_nh 

!__________________________________________________________________________
!
!                               SNH2012.14      
!                 Non-Hydrostatic & Non-Boussinesq Kernel Version  
! Laboratoire d Aerologie, 14 Avenue Edouard Belin, F-31400 Toulouse
! http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm  
!
!__________________________________________________________________________
!CXA MODULES A INTEGRER
!CXA      use module_principal
!CXA      use module_parallele !#MPI
      use module_nh
      use module_nbq
# ifdef TRACETXT
      use module_tracetxt_out
# endif
      implicit none
# include "param_F90.h"
# include "scalars_F90.h"
# include "grid_F90.h"
# include "ocean3d_F90.h"
      integer :: i,j,k,kmax
      real tmp

      real Hzr_half_nbq(GLOBAL_2D_ARRAY,0:N+1)
      common /grid_Hzr_half_nbq/ Hzr_half_nbq
      real Hzw_half_nbq(GLOBAL_2D_ARRAY,0:N)
      common /grid_Hzw_half_nbq/ Hzw_half_nbq

!CXA MODULES A INTEGRER

!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!      include 'omp_lib.h'                                                   ! #OMP#
!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!$$$$$$$$$ O M P $$$$$$$$$
!$omp barrier      ! #OMP#
!$$$$$$$$$ O M P $$$$$$$$$

!      if (iteration3d.eq.0) rhs2_nh=0.

!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!!omp do schedule(guided,neqcont_nh(0)/64)     &                            ! #OMP#
!$omp do schedule(runtime)                     &                            ! #OMP#
!$omp private(l1_nh,i,j,k,l_nh)                                             ! #OMP#
!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

      kmax=N  !CXA

      do l_nh = 1,neqcont_nh(0)

!......initialisation du second membre:
!       rhs2_nh(l_nh)=0.

!......caracteristiques de l equation:
       i = l2iq_nh (l_nh) 
       j = l2jq_nh (l_nh) 
       k = l2kq_nh (l_nh)      
       l1_nh = cont_nnz_nh(l_nh)

       if (k.eq.1) then
!-----------------------------------------------------------
!                         bottom
!-----------------------------------------------------------

!-----------------------------
!.......point u(i,j,k):
!-----------------------------
     
!CXA        contv_nh(l1_nh)  = - dy_u(i,j) / dxdy_t(i,j) - coefb_u(i,j,k) * gdepth_u(i,j,k) / dz_u(i,j,k,1)
        contv_nh(l1_nh)  = - on_u(i,j)*pm(i,j)*pn(i,j) &
                             - coefb_u(i,j,k) * gdepth_u(i,j,k)    & 
                             / (0.5*( Hzr_half_nbq(i-1,j,k) + Hzr_half_nbq(i,j,k) ) )
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,1)
!CXA tableau mijk2lmom_nh vaut 0 (terre) ou 1 (mer) de telle sorte que ecrasement si terre
!-----------------------------
!.......point v(i,j,k):
!-----------------------------
!CXA        contv_nh(l1_nh)  =  - dx_v(i,j)  / dxdy_t(i,j) - coefb_v(i,j,k) * gdepth_v(i,j,k) / dz_v(i,j,k,1)     
        contv_nh(l1_nh)  =  - om_v(i,j)*pm(i,j)*pn(i,j) &
                                  - coefb_v(i,j,k) * gdepth_v(i,j,k)   & 
                            / (0.5*(Hzr_half_nbq(i,j-1,k)+Hzr_half_nbq(i,j,k)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,2)

!-----------------------------
!.......point u(i,j,k+1):
!-----------------------------
!CXA        contv_nh(l1_nh) =  - coefa_u(i,j,k+1) * gdepth_u(i,j,k+1) / dz_u(i,j,k+1,1)      
        contv_nh(l1_nh) =  - coefa_u(i,j,k+1) * gdepth_u(i,j,k+1) &
           / (0.5*(Hzr_half_nbq(i-1,j,k+1)+Hzr_half_nbq(i,j,k+1)))     
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k+1,1)

!-----------------------------
!.......point v(i,j,k+1):
!-----------------------------
!CXA        contv_nh(l1_nh) =  - coefa_v(i,j,k+1) * gdepth_v(i,j,k+1) / dz_v(i,j,k+1,1)     
        contv_nh(l1_nh) =  - coefa_v(i,j,k+1) * gdepth_v(i,j,k+1) &
                    / (0.5*(Hzr_half_nbq(i,j-1,k+1)+Hzr_half_nbq(i,j,k+1)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k+1,2)

!-----------------------------
!.......point u(i+1,j,k):
!-----------------------------
!CXA        contv_nh(l1_nh)  =  dy_u(i+1,j) / dxdy_t(i,j)  - coefb_u(i+1,j,k) * gdepth_u(i+1,j,k) / dz_u(i+1,j,k,1)                  
        contv_nh(l1_nh)  =  on_u(i+1,j)*pm(i,j)*pn(i,j) &
                         - coefb_u(i+1,j,k) * gdepth_u(i+1,j,k) &  
                      / (0.5*(Hzr_half_nbq(i,j,k)+Hzr_half_nbq(i+1,j,k)))

        l1_nh = l1_nh + mijk2lmom_nh(i+1,j,k,1)

!-----------------------------
!.......point v(i,j+1,k):
!-----------------------------
!CXA        contv_nh(l1_nh)  = dx_v(i,j+1)  / dxdy_t(i,j) - coefb_v(i,j+1,k) * gdepth_v(i,j+1,k) / dz_v(i,j+1,k,1)          
        contv_nh(l1_nh)  = om_v(i,j+1)*pm(i,j)*pn(i,j) &
                  - coefb_v(i,j+1,k) * gdepth_v(i,j+1,k)  &
                           / (0.5*(Hzr_half_nbq(i,j,k)+Hzr_half_nbq(i,j+1,k)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j+1,k,2)

!-----------------------------
!.......point u(i+1,j,k+1):
!-----------------------------
!CXA        contv_nh(l1_nh)  = - coefa_u(i+1,j,k+1) * gdepth_u(i+1,j,k+1) / dz_u(i+1,j,k+1,1)  
        contv_nh(l1_nh)  = - coefa_u(i+1,j,k+1) * gdepth_u(i+1,j,k+1) & 
                 / (0.5*(Hzr_half_nbq(i,j,k+1)+Hzr_half_nbq(i+1,j,k+1)))
        l1_nh = l1_nh + mijk2lmom_nh(i+1,j,k+1,1)

!-----------------------------
!.......point v(i,j+1,k+1):
!-----------------------------
!CXA        contv_nh(l1_nh)  =  - coefa_v(i,j+1,k+1) * gdepth_v(i,j+1,k+1) / dz_v(i,j+1,k+1,1)
        contv_nh(l1_nh)  =  - coefa_v(i,j+1,k+1) * gdepth_v(i,j+1,k+1) &
                         / (0.5*(Hzr_half_nbq(i,j,k+1)+Hzr_half_nbq(i,j+1,k+1)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j+1,k+1,2) 

!-----------------------------
!.......point w(i,j,k):
!-----------------------------
!CXA        contv_nh(l1_nh) =  1.d0 / dz_w(i,j,k+1,1) ! decalage d indice
!CXA        l1_nh = l1_nh + mijk2lmom_nh(i,j,k+1,3)

!CXA        contv_nh(l1_nh) =  1.d0 / dz_w(i,j,k,1) !traduction roms
        contv_nh(l1_nh) =  1.d0 / Hzw_half_nbq(i,j,k)
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,3)

!-----------------------------
!.......point w(i,j,k-1):  CXA ici on choisit le facteur d échelle pour
!verifier une condition de frontiere 
!-----------------------------
!CXA         contv_nh(l1_nh)  = -1.d0 / dz_w(i,j,k,1)   ! decalage d indice
!CXA         l1_nh = l1_nh + mijk2lmom_nh(i,j,k,3)

!CXA         contv_nh(l1_nh)  = -1.d0 / dz_w(i,j,k-1,1)   !traduction
!roms
         contv_nh(l1_nh)  = -1.d0 / Hzw_half_nbq(i,j,k-1)
         l1_nh = l1_nh + mijk2lmom_nh(i,j,k-1,3)

       endif ! (k=1)


       if (k.ge.2.and.k.le.kmax-1) then
!-----------------------------------------------------------
!                       k in [2,kmax-1]
!-----------------------------------------------------------

!-----------------------------
!.......point u(i,j,k):
!-----------------------------
!CXA        contv_nh(l1_nh)  =  - dy_u(i,j) / dxdy_t(i,j) - ( coefb_u(i,j,k) - coefa_u(i,j,k) ) * gdepth_u(i,j,k) / dz_u(i,j,k,1)                 
        contv_nh(l1_nh)  =  - on_u(i,j)*pm(i,j)*pn(i,j) &
                   - ( coefb_u(i,j,k) - coefa_u(i,j,k) ) * gdepth_u(i,j,k) & 
                              / (0.5*(Hzr_half_nbq(i-1,j,k)+Hzr_half_nbq(i,j,k)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,1)

        tmp= - ( coefb_u(i,j,k) - coefa_u(i,j,k) ) * gdepth_u(i,j,k)     & 
                     / (0.5*(Hzr_half_nbq(i-1,j,k)+Hzr_half_nbq(i,j,k)))
!       write(*,*) 'ca devrait faire 25 ',  - on_u(i,j)*pm(i,j)*pn(i,j)
!       if (tmp.ne.0) then 
!         write(*,*) 'ca merde dans z_thickness 3 ',i,' ',j,' ',k,' ',  &
!           coefb_u(i,j,k), ' ',coefa_u(i,j,k), ' ',gdepth_u(i,j,k), ' ', 0.5*(Hzr_half_nbq(i-1,j,k)+Hzr_half_nbq(i,j,k))
!       endif

!-----------------------------
!.......point v(i,j,k):
!-----------------------------
!CXA        contv_nh(l1_nh)  = - dx_v(i,j) / dxdy_t(i,j) - ( coefb_v(i,j,k) - coefa_v(i,j,k) ) * gdepth_v(i,j,k) / dz_v(i,j,k,1)      
        contv_nh(l1_nh)  = - om_v(i,j)*pm(i,j)*pn(i,j) &
             - ( coefb_v(i,j,k) - coefa_v(i,j,k) ) * gdepth_v(i,j,k) &
                             / (0.5*(Hzr_half_nbq(i,j-1,k)+Hzr_half_nbq(i,j,k)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,2)

!-----------------------------
!.......point u(i,j,k+1):
!-----------------------------
!CXA        contv_nh(l1_nh)  =  -  coefa_u(i,j,k+1) * gdepth_u(i,j,k+1) / dz_u(i,j,k+1,1)  
        contv_nh(l1_nh)  =  -  coefa_u(i,j,k+1) * gdepth_u(i,j,k+1) / (0.5*(Hzr_half_nbq(i-1,j,k+1)+Hzr_half_nbq(i,j,k+1)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k+1,1)

!-----------------------------
!.......point v(i,j,k+1):
!-----------------------------
!CXA        contv_nh(l1_nh)  =  -  coefa_v(i,j,k+1) * gdepth_v(i,j,k+1) / dz_v(i,j,k+1,1)    
        contv_nh(l1_nh)  =  -  coefa_v(i,j,k+1) * gdepth_v(i,j,k+1) / (0.5*(Hzr_half_nbq(i,j-1,k+1)+Hzr_half_nbq(i,j,k+1)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k+1,2)

!-----------------------------
!.......point u(i,j,k-1):
!-----------------------------
!CXA        contv_nh(l1_nh)  =  +  coefb_u(i,j,k-1) * gdepth_u(i,j,k-1) / dz_u(i,j,k-1,1) 
        contv_nh(l1_nh)  =  +  coefb_u(i,j,k-1) * gdepth_u(i,j,k-1) / (0.5*(Hzr_half_nbq(i-1,j,k-1)+Hzr_half_nbq(i,j,k-1)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k-1,1)

!-----------------------------
!.......point v(i,j,k-1):
!-----------------------------
!CXA        contv_nh(l1_nh)  =  +  coefb_v(i,j,k-1) * gdepth_v(i,j,k-1) / dz_v(i,j,k-1,1)  
        contv_nh(l1_nh)  =  +  coefb_v(i,j,k-1) * gdepth_v(i,j,k-1) / (0.5*(Hzr_half_nbq(i,j-1,k-1)+Hzr_half_nbq(i,j,k-1)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k-1,2)

!-----------------------------
!.......point u(i+1,j,k):
!-----------------------------
!CXA        contv_nh(l1_nh)  = dy_u(i+1,j) / dxdy_t(i,j) - ( coefb_u(i+1,j,k) - coefa_u(i+1,j,k) ) * gdepth_u(i+1,j,k) / dz_u(i+1,j,k,1)        
        contv_nh(l1_nh)  = on_u(i+1,j)*pm(i,j)*pn(i,j) &
         - ( coefb_u(i+1,j,k) - coefa_u(i+1,j,k) ) * gdepth_u(i+1,j,k) & 
                   / (0.5*(Hzr_half_nbq(i,j,k)+Hzr_half_nbq(i+1,j,k)))
        l1_nh = l1_nh + mijk2lmom_nh(i+1,j,k,1)

!-----------------------------
!.......point v(i,j+1,k):
!-----------------------------
!CXA        contv_nh(l1_nh)  = dx_v(i,j+1) / dxdy_t(i,j) - ( coefb_v(i,j+1,k) - coefa_v(i,j+1,k) ) * gdepth_v(i,j+1,k) / dz_v(i,j+1,k,1)       
        contv_nh(l1_nh)  = om_v(i,j+1)*pm(i,j)*pn(i,j) &
             - ( coefb_v(i,j+1,k) - coefa_v(i,j+1,k) ) * gdepth_v(i,j+1,k) &
                       / (0.5*(Hzr_half_nbq(i,j,k)+Hzr_half_nbq(i,j+1,k)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j+1,k,2)

!-----------------------------
!.......point u(i+1,j,k+1):
!-----------------------------
!CXA        contv_nh(l1_nh)  =  -  coefa_u(i+1,j,k+1) * gdepth_u(i+1,j,k+1) / dz_u(i+1,j,k+1,1) 
        contv_nh(l1_nh)  =  -  coefa_u(i+1,j,k+1) * gdepth_u(i+1,j,k+1) &
                 / (0.5*(Hzr_half_nbq(i,j,k+1)+Hzr_half_nbq(i+1,j,k+1)))
        l1_nh = l1_nh + mijk2lmom_nh(i+1,j,k+1,1)

!-----------------------------
!.......point v(i,j+1,k+1):
!-----------------------------
!CXA        contv_nh(l1_nh)  = -  coefa_v(i,j+1,k+1) * gdepth_v(i,j+1,k+1) / dz_v(i,j+1,k+1,1)         
        contv_nh(l1_nh)  = -  coefa_v(i,j+1,k+1) * gdepth_v(i,j+1,k+1) &
                / (0.5*(Hzr_half_nbq(i,j,k+1)+Hzr_half_nbq(i,j+1,k+1)))

        l1_nh = l1_nh + mijk2lmom_nh(i,j+1,k+1,2)

!-----------------------------
!.......point u(i+1,j,k-1):
!-----------------------------
!CXA        contv_nh(l1_nh)  = + coefb_u(i+1,j,k-1) * gdepth_u(i+1,j,k-1) / dz_u(i+1,j,k-1,1)   
        contv_nh(l1_nh)  = + coefb_u(i+1,j,k-1) * gdepth_u(i+1,j,k-1) &
               / (0.5*(Hzr_half_nbq(i,j,k-1)+Hzr_half_nbq(i+1,j,k-1)))
        l1_nh = l1_nh + mijk2lmom_nh(i+1,j,k-1,1)

!-----------------------------
!.......point v(i,j+1,k-1):
!-----------------------------
!CXA        contv_nh(l1_nh)  = + coefb_v(i,j+1,k-1) * gdepth_v(i,j+1,k-1) / dz_v(i,j+1,k-1,1)    
        contv_nh(l1_nh)  = + coefb_v(i,j+1,k-1) * gdepth_v(i,j+1,k-1) &
                  / (0.5*(Hzr_half_nbq(i,j,k-1)+Hzr_half_nbq(i,j+1,k-1)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j+1,k-1,2)

!-----------------------------
!.......point w(i,j,k):
!-----------------------------
!CXA        contv_nh(l1_nh)  =  1.d0 / dz_w(i,j,k+1,1)  
!CXA        l1_nh = l1_nh + mijk2lmom_nh(i,j,k+1,3)
!CXA        contv_nh(l1_nh)  =  1.d0 / dz_w(i,j,k,1)   !traduction roms
        contv_nh(l1_nh)  =  1.d0 / Hzw_half_nbq(i,j,k)
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,3)
 
!-----------------------------
!.......point w(i,j,k-1):
!-----------------------------
!CXA        contv_nh(l1_nh)  = -1.d0 / dz_w(i,j,k,1)
!CXA        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,3)
!CXA        contv_nh(l1_nh)  = -1.d0 / dz_w(i,j,k-1,1) !traduction roms
        contv_nh(l1_nh)  = -1.d0 / Hzw_half_nbq(i,j,k-1)
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k-1,3)

        endif ! (k>1     k<kmax)


        if (k.eq.kmax) then
!-----------------------------------------------------------
!                      surface
!-----------------------------------------------------------

!-----------------------------
!.......point u(i,j,k):
!-----------------------------
!CXA        contv_nh(l1_nh)  =  - dy_u(i,j)  / dxdy_t(i,j) + ( coefa_u(i,j,k) * gdepth_u(i,j,k)     &
!CXA                            - coefb_u(i,j,k+1) * gdepth_u(i,j,k+1) ) / dz_u(i,j,k,1)   
        contv_nh(l1_nh)  =  - on_u(i,j)*pm(i,j)*pn(i,j) &
                    + ( coefa_u(i,j,k) * gdepth_u(i,j,k)     &
                            - coefb_u(i,j,k+1) * gdepth_u(i,j,k+1) ) &
                     / (0.5*(Hzr_half_nbq(i-1,j,k)+Hzr_half_nbq(i,j,k)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,1)
           
!-----------------------------
!.......point v(i,j,k):
!-----------------------------
!CXA        contv_nh(l1_nh)  =  - dx_v(i,j)  / dxdy_t(i,j) + ( coefa_v(i,j,k) * gdepth_v(i,j,k)     &
!CXA                            - coefb_v(i,j,k+1) * gdepth_v(i,j,k+1) ) / dz_v(i,j,k,1)  
        contv_nh(l1_nh)  =  - om_v(i,j)*pm(i,j)*pn(i,j) &
                           + ( coefa_v(i,j,k) * gdepth_v(i,j,k)     &
                            - coefb_v(i,j,k+1) * gdepth_v(i,j,k+1) ) &
                    / (0.5*(Hzr_half_nbq(i,j-1,k)+Hzr_half_nbq(i,j,k)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,2)

!-----------------------------
!.......point u(i,j,k+1):
!-----------------------------

!-----------------------------
!.......point v(i,j,k+1):
!-----------------------------

!-----------------------------
!.......point u(i,j,k-1):
!-----------------------------
!CXA        contv_nh(l1_nh)  = + coefb_u(i,j,k-1) * gdepth_u(i,j,k-1)  / dz_u(i,j,k-1,1)  
        contv_nh(l1_nh)  = + coefb_u(i,j,k-1) * gdepth_u(i,j,k-1)  / (0.5*(Hzr_half_nbq(i-1,j,k-1)+Hzr_half_nbq(i,j,k-1)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k-1,1)

!-----------------------------
!.......point v(i,j,k-1):
!-----------------------------
!CXA        contv_nh(l1_nh)  = + coefb_v(i,j,k-1) * gdepth_v(i,j,k-1) / dz_v(i,j,k-1,1)  
        contv_nh(l1_nh)  = + coefb_v(i,j,k-1) * gdepth_v(i,j,k-1) &
                / (0.5*(Hzr_half_nbq(i,j-1,k-1)+Hzr_half_nbq(i,j,k-1)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k-1,2)

!-----------------------------
!.......point u(i+1,j,k):
!-----------------------------
!CXA        contv_nh(l1_nh)  = dy_u(i+1,j) / dxdy_t(i,j) + ( coefa_u(i+1,j,k) * gdepth_u(i+1,j,k)  &
!CXA                                     - coefb_u(i+1,j,k+1) * gdepth_u(i+1,j,k+1) ) / dz_u(i+1,j,k,1)   
        contv_nh(l1_nh)  = on_u(i+1,j)*pm(i,j)*pn(i,j) +    &
                    ( coefa_u(i+1,j,k) * gdepth_u(i+1,j,k)  &
               - coefb_u(i+1,j,k+1) * gdepth_u(i+1,j,k+1) ) &
             / (0.5*(Hzr_half_nbq(i,j,k)+Hzr_half_nbq(i+1,j,k)))

        l1_nh = l1_nh + mijk2lmom_nh(i+1,j,k,1)

!-----------------------------
!.......point v(i,j+1,k):
!-----------------------------
!CXA        contv_nh(l1_nh)  = dx_v(i,j+1) / dxdy_t(i,j) + ( coefa_v(i,j+1,k) * gdepth_v(i,j+1,k)  &
!CXA                                    -  coefb_v(i,j+1,k+1) * gdepth_v(i,j+1,k+1) ) / dz_v(i,j+1,k,1)  
        contv_nh(l1_nh)  = om_v(i,j+1)*pm(i,j)*pn(i,j) +      &
                      ( coefa_v(i,j+1,k) * gdepth_v(i,j+1,k)  &
                -  coefb_v(i,j+1,k+1) * gdepth_v(i,j+1,k+1) ) &
                  / (0.5*(Hzr_half_nbq(i,j,k)+Hzr_half_nbq(i,j+1,k)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j+1,k,2)

!-----------------------------
!.......point u(i+1,j,k+1):
!-----------------------------

!-----------------------------
!.......point v(i,j+1,k+1):
!-----------------------------

!-----------------------------
!.......point u(i+1,j,k-1):
!-----------------------------
!CXA        contv_nh(l1_nh)  =  +  coefb_u(i+1,j,k-1) * gdepth_u(i+1,j,k-1) / dz_u(i+1,j,k-1,1)   
        contv_nh(l1_nh)  =  +  coefb_u(i+1,j,k-1) * gdepth_u(i+1,j,k-1) &
                 / (0.5*(Hzr_half_nbq(i,j,k-1)+Hzr_half_nbq(i+1,j,k-1)))
        l1_nh = l1_nh + mijk2lmom_nh(i+1,j,k-1,1)

!-----------------------------
!.......point v(i,j+1,k-1):
!-----------------------------
!CXA        contv_nh(l1_nh)  =  + coefb_v(i,j+1,k-1) * gdepth_v(i,j+1,k-1) / dz_v(i,j+1,k-1,1)   
        contv_nh(l1_nh)  =  + coefb_v(i,j+1,k-1) * gdepth_v(i,j+1,k-1) &
                 / (0.5*(Hzr_half_nbq(i,j,k-1)+Hzr_half_nbq(i,j+1,k-1)))
        l1_nh = l1_nh + mijk2lmom_nh(i,j+1,k-1,2)

!-----------------------------
!.......point w(i,j,k):
!-----------------------------
!CXA        contv_nh(l1_nh)  =  1.d0 / dz_w(i,j,k+1,1)
!CXA        l1_nh = l1_nh + mijk2lmom_nh(i,j,k+1,3)
!CXA        contv_nh(l1_nh)  =  1.d0 / dz_w(i,j,k,1) !traduction roms
        contv_nh(l1_nh)  =  1.d0 / Hzw_half_nbq(i,j,k)
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,3)

!-----------------------------
!.......point w(i,j,k-1):
!-----------------------------
!CXA        contv_nh(l1_nh)  =  -1.d0 / dz_w(i,j,k,1)
!CXA        l1_nh = l1_nh + mijk2lmom_nh(i,j,k,3)
!CXA        contv_nh(l1_nh)  =  -1.d0 / dz_w(i,j,k-1,1) !traduction roms
        contv_nh(l1_nh)  =  -1.d0 / Hzw_half_nbq(i,j,k-1)
        l1_nh = l1_nh + mijk2lmom_nh(i,j,k-1,3)
        endif ! (k=kmax)
        
      enddo

#ifdef TOTO_IMP
      if (ifl_nbq.eq.1) then
!***********************************************************
!***********************************************************
!     Matrice implicite:    
!          points interieurs
!        & points aux frontieres 
!***********************************************************
!***********************************************************

      l1imp_nbq = 1

      do l_nh = 1,nzq_nh

       i = l2iq_nh (l_nh) 
       j = l2jq_nh (l_nh) 
       k = l2kq_nh (l_nh)      

!-----------------------------
!.......point w(i,j,k+1):
!-----------------------------
        cimpv_nbq(l1imp_nbq) =  -1.d0 / dz_w(i,j,k+1,1) / dz_t(i,j,k,1) * edt_nbq_bk 
        l1imp_nbq = l1imp_nbq + mijk2lmom_nh(i,j,k+1,3)

!-----------------------------
!.......point w(i,j,k):
!-----------------------------
        cimpv_nbq(l1imp_nbq) = 1.d0 / dz_w(i,j,k,1) / dz_t(i,j,k,1) * edt_nbq_bk  
        l1imp_nbq = l1imp_nbq + mijk2lmom_nh(i,j,k,3)

      enddo
      endif

#endif

      if (ifl_nbq.eq.1) then
!***********************************************************
!  Normalisation en NBQ / NH  qui tourne avec dz div
!***********************************************************

      do l_nh = 1,neqcont_nh(0)

!......caracteristiques de l equation:
       i = l2iq_nh (l_nh) 
       j = l2jq_nh (l_nh) 
       k = l2kq_nh (l_nh) 

       do l1_nh = conti_nh(l_nh),conti_nh(l_nh+1)-1
!CXA          contv_nh(l1_nh) = contv_nh(l1_nh) / dz_t(i,j,k,1)
          contv_nh(l1_nh) = contv_nh(l1_nh) / Hzr_half_nbq(i,j,k)
       enddo
      enddo

      endif

!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$omp end do                                                                ! #OMP#
!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!$$$$$$$$$ O M P $$$$$$$$$
!$omp barrier      ! #OMP#
!$$$$$$$$$ O M P $$$$$$$$$
!        write(*,*) ' neqmom_nh(1) ',neqmom_nh(1)
!        write(*,*)' qui t es NBQ 1',l2imom_nh(1), ' ', l2jmom_nh(1), ' ', l2kmom_nh(1)
!        write(*,*)' qui t es NBQ 2451',l2imom_nh(2451), ' ', l2jmom_nh(2451), ' ', l2kmom_nh(2451)
#ifdef CHECK_CROCO
!CXA       call set_tracedirectory(iteration3d)
        call set_tracedirectory(iic)
        filetrace='mat_cont_it_'//int2string(iic)//'.txt'
        call printmat_mm(filetrace,neqcont_nh(0),neqmom_nh(0),     & 
                                       nzcont_nh,conti_nh,         &
                                         contj_nh,contv_nh)

#endif

      return
      end subroutine mat_cont_nh
#else
        subroutine mat_cont_nh_empty
        return
        end 
#endif
