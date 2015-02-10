#include "cppdefs.h"
#ifdef NBQ
      subroutine mat_mom_nh 

!__________________________________________________________________________
!
!                               SNH2012.14      
!                 Non-Hydrostatic & Non-Boussinesq Kernel Version  
! Laboratoire d'Aerologie, 14 Avenue Edouard Belin, F-31400 Toulouse
! http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm  
!
!__________________________________________________________________________
!CXA MODULES A INTEGRER
!CXA      use module_principal
!CXA      use module_parallele !#MPI
      use module_nh
      use module_nbq
#ifdef TRACETXT
      use module_tracetxt_out
#endif
      implicit none
#include "param_F90.h"
#include "scalars_F90.h"
#include "grid_F90.h"
#include "ocean3d_F90.h"

      real Hzr_half_nbq(GLOBAL_2D_ARRAY,0:N+1)
      common /grid_Hzr_half_nbq/ Hzr_half_nbq
      real Hzw_half_nbq(GLOBAL_2D_ARRAY,0:N)
      common /grid_Hzw_half_nbq/ Hzw_half_nbq

      real :: dti_lp
      integer :: i,j,k,kmax,imax,jmax

!CXA MODULES A INTEGRER

!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!      include 'omp_lib.h'                                                   ! #OMP#
!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

      double precision  a_m,b_m
      integer  c_m

!$$$$$$$$$ O M P $$$$$$$$$
!$omp barrier      ! #OMP#
!$$$$$$$$$ O M P $$$$$$$$$

!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$omp sections private(i,j,k,l_nh,l1_nh,a_m,b_m,c_m)                        ! #OMP#  
!                                                                           ! #OMP#
!$omp section                                                               ! #OMP#
!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
#ifdef MPI
#define LOCALLM Lmmpi
#define LOCALMM Mmmpi
#else
#define LOCALLM Lm
#define LOCALMM Mm
#endif    

      kmax=N  !CXA
      imax=LOCALLM
      jmax=LOCALMM
      dti_lp=2*dt   !CXA

      rhs2r_nbq =0.

!---->equation du mouvement suivant x:
      do l_nh=1,neqmom_nh(1)

!.......caracteristiques de l'equation:
        l1_nh = momi_nh   (l_nh) - 1
        i     = l2imom_nh (l_nh)
        j     = l2jmom_nh (l_nh)
        k     = l2kmom_nh (l_nh)

        c_m = 1

!CXA     if je suis sur le bord est
        if (i.eq.imax+1) then ! cdl est FLATHER avec vitesse acoustique
!!!                                            ou interne
!.......point p(i-1,j,k):
!        if (ifl_nbq.eq.0) then 
!CXA          momv_nh(l1_nh+c_m) = dz_u(i,j,k,2) / cwj_int_u(j,2) / dti_lp             
          momv_nh(l1_nh+c_m) = (0.5*(Hzr_half_nbq(i-1,j,k)+Hzr_half_nbq(i,j,k))) / cw_int_nbq / dti_lp
          c_m = c_m + mijk2lq_nh(i-1,j,k) 
!        else
!         momv_nh(l1_nh+c_m) = dz_u(i,j,k,2) / soundspeed_nbq / edt_nbq_lp         
!         c_m = c_m + mijk2lq_nh(i-1,j,k) 
!        endif

!CXA     if je suis sur le bord ouest
        elseif (i.eq.1) then ! cdl ouest
!.......point p(i,j,k):
!        if (ifl_nbq.eq.0) then 
!CXA          momv_nh(l1_nh+c_m) = -dz_u(i,j,k,2) / cwj_int_u(j,1) / dti_lp
          momv_nh(l1_nh+c_m) = -(0.5*(Hzr_half_nbq(i-1,j,k)+Hzr_half_nbq(i,j,k))) / cw_int_nbq / dti_lp
          c_m = c_m + mijk2lq_nh(i,j,k)
!        else
!         momv_nh(l1_nh+c_m) = -dz_u(i,j,k,2) / soundspeed_nbq / edt_nbq_lp   
!         c_m = c_m + mijk2lq_nh(i,j,k)
!        endif

        else ! point courant
!.......precalculs:
!CXA dz_u au pas de temps 2 cad le + recent
!CXA        a_m  = - dz_u(i,j,k,2) / dx_u(i,j)  
        a_m  = - (0.5*(Hzr_half_nbq(i-1,j,k)+Hzr_half_nbq(i,j,k))) * pm_u(i,j)  
        b_m  =  gdepth_u(i,j,k) 

!.......point p(i,j,k+1):
        momv_nh(l1_nh+c_m) = b_m * coefb_u(i,j,k)
        c_m = c_m + mijk2lq_nh(i,j,k+1)

!.......point p(i,j,k):
        if (k.ne.1.and.k.ne.kmax) then
           momv_nh(l1_nh+c_m) =  a_m + b_m * ( coefa_u(i,j,k) - coefb_u(i,j,k))
        elseif (k.eq.kmax) then
           momv_nh(l1_nh+c_m) =  a_m + b_m * coefa_u(i,j,k)   - gdepth_u(i,j,k+1) * coefb_u(i,j,k+1) 
        else
           momv_nh(l1_nh+c_m) =  a_m  - b_m * coefb_u(i,j,k)
        endif
        c_m = c_m + mijk2lq_nh(i,j,k)

!.......point p(i,j,k-1):
        momv_nh(l1_nh+c_m) = - b_m * coefa_u(i,j,k)
        c_m = c_m + mijk2lq_nh(i,j,k-1)

!.......point p(i-1,j,k+1):
        momv_nh(l1_nh+c_m) = b_m * coefb_u(i-1,j,k)
        c_m = c_m + mijk2lq_nh(i-1,j,k+1)

!.......point p(i-1,j,k):
        if (k.ne.1.and.k.ne.kmax) then
           momv_nh(l1_nh+c_m) = - a_m  + b_m * ( coefa_u(i-1,j,k) - coefb_u(i-1,j,k))
        elseif (k.eq.kmax) then
           momv_nh(l1_nh+c_m) = - a_m  + b_m * coefa_u(i-1,j,k)   - gdepth_u(i,j,k+1) * coefb_u(i-1,j,k+1) 
        else
           momv_nh(l1_nh+c_m) = - a_m  - b_m * coefb_u(i-1,j,k)
        endif
        c_m = c_m + mijk2lq_nh(i-1,j,k)

!.......point p(i-1,j,k-1):
        momv_nh(l1_nh+c_m) = - b_m * coefa_u(i-1,j,k)
        c_m = c_m + mijk2lq_nh(i-1,j,k-1)
        endif ! point courant: fin
      enddo


!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$omp section                                                               ! #OMP#
!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!---->equation du mouvement suivant y:

      do l_nh=neqmom_nh(1)+1,neqmom_nh(1)+neqmom_nh(2)

!.......caracteristiques de l'equation:
        l1_nh = momi_nh   (l_nh) - 1
        i     = l2imom_nh (l_nh)
        j     = l2jmom_nh (l_nh)
        k     = l2kmom_nh (l_nh)

        c_m = 1
!CXA     if je suis sur le bord nord
        if (j.eq.jmax+1) then  ! cdl nord
!.......point p(i,j-1,k):
!        if (ifl_nbq.eq.0) then 
!CXA          momv_nh(l1_nh+c_m) = dz_v(i,j,k,2)  / cwi_int_v(i,2) / dti_lp    
          momv_nh(l1_nh+c_m) = (0.5*(Hzr_half_nbq(i,j-1,k)+Hzr_half_nbq(i,j,k)))  / cw_int_nbq / dti_lp    
          c_m = c_m + mijk2lq_nh(i,j-1,k)
!        else
!         momv_nh(l1_nh+c_m) = dz_v(i,j,k,2) / soundspeed_nbq / edt_nbq_lp   
!         c_m = c_m + mijk2lq_nh(i,j-1,k)
!        endif
 
!CXA     if je suis sur le bord sud
        elseif (j.eq.1) then   ! cdl sud
!.......point p(i,j,k):
!        if (ifl_nbq.eq.0) then 
!CXA           momv_nh(l1_nh+c_m) = -dz_v(i,j,k,2) / cwi_int_v(i,1) / dti_lp   
           momv_nh(l1_nh+c_m) = -(0.5*(Hzr_half_nbq(i,j-1,k)+Hzr_half_nbq(i,j,k))) / cw_int_nbq / dti_lp   
           c_m = c_m + mijk2lq_nh(i,j,k)
!        else
!          momv_nh(l1_nh+c_m) = -dz_v(i,j,k,2) /soundspeed_nbq / edt_nbq_lp  
!          c_m = c_m + mijk2lq_nh(i,j,k)
!        endif

        else ! point courant
!.......precalculs:

!CXA        a_m  = - dz_v(i,j,k,2) / dy_v(i,j) 
        a_m  = - (0.5*(Hzr_half_nbq(i,j-1,k)+Hzr_half_nbq(i,j,k))) * pn_v(i,j) 
        b_m  =  gdepth_v(i,j,k) 

!.......point p(i,j,k+1):
        momv_nh(l1_nh+c_m) =  b_m * coefb_v(i,j,k) 
        c_m = c_m + mijk2lq_nh(i,j,k+1)

!.......point p(i,j,k):
        if (k.ne.1.and.k.ne.kmax) then
           momv_nh(l1_nh+c_m) =  a_m  + b_m * ( coefa_v(i,j,k) - coefb_v(i,j,k))
        elseif (k.eq.kmax) then
           momv_nh(l1_nh+c_m) =  a_m + b_m * coefa_v(i,j,k)    - gdepth_v(i,j,k+1) * coefb_v(i,j,k+1) 
        else
           momv_nh(l1_nh+c_m) =  a_m - b_m * coefb_v(i,j,k)
        endif
        c_m = c_m + mijk2lq_nh(i,j,k)

!.......point p(i,j,k-1):
        momv_nh(l1_nh+c_m) = - b_m * coefa_v(i,j,k)
        c_m = c_m + mijk2lq_nh(i,j,k-1)

!.......point p(i,j-1,k+1):
        momv_nh(l1_nh+c_m) = b_m * coefb_v(i,j-1,k)
        c_m = c_m + mijk2lq_nh(i,j-1,k+1)

!.......point p(i,j-1,k):
        if (k.ne.1.and.k.ne.kmax) then
           momv_nh(l1_nh+c_m) = - a_m + b_m * ( coefa_v(i,j-1,k) - coefb_v(i,j-1,k))
        elseif (k.eq.kmax) then
           momv_nh(l1_nh+c_m) = - a_m + b_m * coefa_v(i,j-1,k)   - gdepth_v(i,j,k+1) * coefb_v(i,j-1,k+1)
        else
           momv_nh(l1_nh+c_m) = - a_m - b_m * coefb_v(i,j-1,k)
        endif
        c_m = c_m + mijk2lq_nh(i,j-1,k)

!.......point p(i,j-1,k-1):
        momv_nh(l1_nh+c_m) = - b_m * coefa_v(i,j-1,k)
        c_m = c_m + mijk2lq_nh(i,j-1,k-1)
        endif ! point courant: fin

      enddo


!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!!omp section                                                               ! #OMP#
!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

      if (ifl_nbq.eq.1) then

      momvg_nh(1:l1_nh) = momv_nh(1:l1_nh)

!---->equation du mouvement suivant z:

      do l_nh=neqmom_nh(1)+neqmom_nh(2)+1,                &
                 neqmom_nh(1)+neqmom_nh(2)+neqmom_nh(3)
         
!.......caracteristiques de l'equation:
        l1_nh = momi_nh   (l_nh) - 1
        i     = l2imom_nh (l_nh)
        j     = l2jmom_nh (l_nh)
        k     = l2kmom_nh (l_nh)

        c_m = 1
        
!CXA TBC
!.......point p(i,j,k+1):
!CXA        if (k.ne.1) then  !! no equation for the lowest w
        if (k.ne.0) then
          momv_nh (l1_nh+c_m) =  -1.      

          momvg_nh(l1_nh+c_m) =  -1.     &
!CXA           -  dz_w(i,j,k,2)*grav/soundspeed_nbq**2   ! Signe: -MOM
           -  Hzw_half_nbq(i,j,k)*g/soundspeed_nbq**2   ! Signe: -MOM
        endif
!CXA     c_m = c_m + mijk2lq_nh(i,j,k)
        c_m = c_m + mijk2lq_nh(i,j,k+1)

!CXA        if (k.ne.1) then
        if (k.ne.0) then
!.......point p(i,j,k):
         momv_nh (l1_nh+c_m) =  1.       
         momvg_nh(l1_nh+c_m) =  1.     &
!CXA            -  dz_w(i,j,k,2)*grav/soundspeed_nbq**2  ! Signe: -MOM
            - Hzw_half_nbq(i,j,k)*g/soundspeed_nbq**2  ! Signe: -MOM
        endif


!.......point p(i,j,k):
!       momv_nh(l1_nh+c_m) = -1.
!       c_m = c_m + mijk2lq_nh(i,j,k)
        
!.......point p(i,j,k-1):
!       momv_nh(l1_nh+c_m) = 1.
!       c_m = c_m + mijk2lq_nh(i,j,k-1)

      enddo

      endif

!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$omp end sections                                                          ! #OMP#
!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
!$$$$$$$$$$$$$$$$$$$$$$$$ O M P $$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

!$$$$$$$$$ O M P $$$$$$$$$
!$omp barrier      ! #OMP#
!$$$$$$$$$ O M P $$$$$$$$$

#ifdef CHECK_CROCO
!CXA       call set_tracedirectory(iteration3d)
        call set_tracedirectory(iic)
        filetrace='mat_mom_it_'//int2string(iic)//'.txt'
        call printmat_mm(filetrace,neqmom_nh(0),neqcont_nh(0),     &
                                             nzmom_nh,momi_nh,     &
                                              momj_nh,momv_nh)

#endif

      return
      end subroutine mat_mom_nh
#else
        subroutine mat_mom_nh_empty
        return
        end 
#endif
