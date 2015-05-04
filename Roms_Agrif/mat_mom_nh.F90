#include "cppdefs.h"
#ifdef NBQ

      subroutine mat_mom_nh 
!*******************************************************************
!*******************************************************************
!*******************************************************************
!        Matrix MOM for momentum equations: computation 
!
!          updated at every internal mode time step
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

      real :: dti_lp
      integer :: i,j,k,kmax,imax,jmax

!CXA MODULES A INTEGRER

      double precision  a_m,b_m
      integer  c_m

#ifdef MPI
#define LOCALLM Lmmpi
#define LOCALMM Mmmpi
#else
#define LOCALLM Lm
#define LOCALMM Mm
#endif    

!*******************************************************************
!     Various Initializations:
!*******************************************************************

      kmax=N  
      imax=LOCALLM
      jmax=LOCALMM
      dti_lp=2*dt   !CXA

!*******************************************************************
!     Momentum Equation: X-direction
!******************************************************************* 

      do l_nh=1,neqmom_nh(1)

!.......caracteristiques de l'equation:
        l1_nh = momi_nh   (l_nh) - 1
        i     = l2imom_nh (l_nh)
        j     = l2jmom_nh (l_nh)
        k     = l2kmom_nh (l_nh)
        c_m = 1

        if (i.eq.imax+1) then ! cdl est FLATHER avec vitesse acoustique
                              ! ou interne
!.......point p(i-1,j,k):
!CXA      momv_nh(l1_nh+c_m) = dz_u(i,j,k,2) / cwj_int_u(j,2) / dti_lp             
          momv_nh(l1_nh+c_m) = (0.5*(Hzr_half_nbq(i-1,j,k)+            &
                                     Hzr_half_nbq(i,j,k)))             &
                               / cw_int_nbq / dti_lp
          c_m = c_m + mijk2lq_nh(i-1,j,k) 

        elseif (i.eq.0) then ! cdl ouest
!.......point p(i,j,k):
!CXA      momv_nh(l1_nh+c_m) = -dz_u(i,j,k,2) / cwj_int_u(j,1) / dti_lp
          momv_nh(l1_nh+c_m) = -(0.5*(Hzr_half_nbq(i-1,j,k)+           &
                                      Hzr_half_nbq(i,j,k)))            &
                               / cw_int_nbq / dti_lp
          c_m = c_m + mijk2lq_nh(i,j,k)

        else ! point courant
!.......precalculs:
        a_m  = - (0.5*(Hzr_half_nbq(i-1,j,k)+Hzr_half_nbq(i,j,k)))     &
                                                       * pm_u(i,j)  
        b_m  =  gdepth_u(i,j,k) 

!.......point p(i,j,k+1):
        momv_nh(l1_nh+c_m) = b_m * coefb_u(i,j,k)
        c_m = c_m + mijk2lq_nh(i,j,k+1)

!.......point p(i,j,k):
        if (k.ne.1.and.k.ne.kmax) then
           momv_nh(l1_nh+c_m) =   a_m                                 &
                                + b_m * (coefa_u(i,j,k)-coefb_u(i,j,k))
        elseif (k.eq.kmax) then
           momv_nh(l1_nh+c_m) =  a_m +                                 &
                                 b_m * coefa_u(i,j,k)                  &
                                 - gdepth_u(i,j,k+1) * coefb_u(i,j,k+1) 
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
          momv_nh(l1_nh+c_m) = - a_m  + b_m *                          &
                                 (coefa_u(i-1,j,k) - coefb_u(i-1,j,k))
        elseif (k.eq.kmax) then
           momv_nh(l1_nh+c_m) = - a_m                                  &
                                + b_m * coefa_u(i-1,j,k)               &
                                - gdepth_u(i,j,k+1) * coefb_u(i-1,j,k+1)
        else
           momv_nh(l1_nh+c_m) = - a_m  - b_m * coefb_u(i-1,j,k)
        endif
        c_m = c_m + mijk2lq_nh(i-1,j,k)

!.......point p(i-1,j,k-1):
        momv_nh(l1_nh+c_m) = - b_m * coefa_u(i-1,j,k)
        c_m = c_m + mijk2lq_nh(i-1,j,k-1)
        endif ! point courant: fin
      enddo

!*******************************************************************
!     Momentum Equation: Y-direction
!******************************************************************* 

      do l_nh=neqmom_nh(1)+1,neqmom_nh(1)+neqmom_nh(2)

!.......caracteristiques de l'equation:
        l1_nh = momi_nh   (l_nh) - 1
        i     = l2imom_nh (l_nh)
        j     = l2jmom_nh (l_nh)
        k     = l2kmom_nh (l_nh)

        c_m = 1
        if (j.eq.jmax+1) then  ! cdl nord

!.......point p(i,j-1,k):
!CXA      momv_nh(l1_nh+c_m) = dz_v(i,j,k,2)  / cwi_int_v(i,2) / dti_lp    
          momv_nh(l1_nh+c_m) = (0.5*(Hzr_half_nbq(i,j-1,k)+            &
                                     Hzr_half_nbq(i,j,k)))             &
                               / cw_int_nbq / dti_lp    
          c_m = c_m + mijk2lq_nh(i,j-1,k)
 
        elseif (j.eq.0) then   ! cdl sud
!.......point p(i,j,k):
!CXA      momv_nh(l1_nh+c_m) = -dz_v(i,j,k,2) / cwi_int_v(i,1) / dti_lp   
          momv_nh(l1_nh+c_m) = -(0.5*(Hzr_half_nbq(i,j-1,k)+           &
                                      Hzr_half_nbq(i,j,k)))            &
                                / cw_int_nbq / dti_lp   
          c_m = c_m + mijk2lq_nh(i,j,k)
        else ! point courant
!.......precalculs:
!CXA    a_m  = - dz_v(i,j,k,2) / dy_v(i,j) 
        a_m  = - (0.5*(Hzr_half_nbq(i,j-1,k)+Hzr_half_nbq(i,j,k)))     &
               * pn_v(i,j) 
        b_m  =  gdepth_v(i,j,k) 

!.......point p(i,j,k+1):
        momv_nh(l1_nh+c_m) =  b_m * coefb_v(i,j,k) 
        c_m = c_m + mijk2lq_nh(i,j,k+1)

!.......point p(i,j,k):
        if (k.ne.1.and.k.ne.kmax) then
           momv_nh(l1_nh+c_m) =  a_m  + b_m                            &
                                 * ( coefa_v(i,j,k) - coefb_v(i,j,k))
        elseif (k.eq.kmax) then
           momv_nh(l1_nh+c_m) =  a_m + b_m * coefa_v(i,j,k)            &
                                 - gdepth_v(i,j,k+1) * coefb_v(i,j,k+1) 
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
           momv_nh(l1_nh+c_m) = - a_m + b_m                            &
                                * ( coefa_v(i,j-1,k) - coefb_v(i,j-1,k))
        elseif (k.eq.kmax) then
           momv_nh(l1_nh+c_m) = - a_m + b_m * coefa_v(i,j-1,k)         &
                                - gdepth_v(i,j,k+1) * coefb_v(i,j-1,k+1)
        else
           momv_nh(l1_nh+c_m) = - a_m - b_m * coefb_v(i,j-1,k)
        endif
        c_m = c_m + mijk2lq_nh(i,j-1,k)

!.......point p(i,j-1,k-1):
        momv_nh(l1_nh+c_m) = - b_m * coefa_v(i,j-1,k)
        c_m = c_m + mijk2lq_nh(i,j-1,k-1)
        endif ! point courant: fin

      enddo

      if (ifl_nbq.eq.1) then

      momvg_nh(1:l1_nh) = momv_nh(1:l1_nh)

!*******************************************************************
! Momentum Equation: Z-direction
!******************************************************************* 

      do l_nh=neqmom_nh(1)+neqmom_nh(2)+1,                &
                 neqmom_nh(1)+neqmom_nh(2)+neqmom_nh(3)
         
!.......caracteristiques de l'equation:
        l1_nh = momi_nh   (l_nh) - 1
        i     = l2imom_nh (l_nh)
        j     = l2jmom_nh (l_nh)
        k     = l2kmom_nh (l_nh)

        c_m = 1
        
!.......point p(i,j,k+1):
        if (k.ne.0) then
          momv_nh (l1_nh+c_m) =  -1.      

          momvg_nh(l1_nh+c_m) =  -1.     &
!CXA       -  dz_w(i,j,k,2)*grav/soundspeed_nbq**2      ! Signe: -MOM
           -  Hzw_half_nbq(i,j,k)*g/soundspeed_nbq**2   ! Signe: -MOM
        endif
        c_m = c_m + mijk2lq_nh(i,j,k+1)

        if (k.ne.0) then
!.......point p(i,j,k):
         momv_nh (l1_nh+c_m) =  1.       
         momvg_nh(l1_nh+c_m) =  1.     &
!CXA        - dz_w(i,j,k,2)*grav/soundspeed_nbq**2     ! Signe: -MOM
            - Hzw_half_nbq(i,j,k)*g/soundspeed_nbq**2  ! Signe: -MOM
        endif

      enddo

#ifdef NBQ_DRHODT
!*******************************************************************
!---->Correction derivee temporelle de rho:
!*******************************************************************
      do l_nh=neqmom_nh(0)+1,neqcorrt_nbq

!......caracteristiques de l'equation:
       l1_nh = momi_nh   (l_nh) - 1
       i = l2iq_nh (l_nh-neqmom_nh(0))
       j = l2jq_nh (l_nh-neqmom_nh(0))
       k = l2kq_nh (l_nh-neqmom_nh(0))

       c_m = 1

!      (depth_nbq_t(i,j,k,2)-depth_nbq_t(i,j,k,0)) / 2. / dti_fw / 2. / dz_t(i,j,k,1)* edt_nbq_lp
       a_m = 0.25*dtnbq*(zr_half_nbq(i,j,k,2)-zr_half_nbq(i,j,k,0))    &
                                       / (dt * Hzr_half_nbq(i,j,k))

!......Point rh(i,j,k+1)
       momvg_nh(l1_nh+c_m) = a_m

       if (k.eq.0.or.k.eq.N)                                           &
                            momvg_nh(l1_nh+c_m) = 2.*momvg_nh(l1_nh+c_m)
       c_m = c_m + mijk2lq_nh(i,j,k+1)

!......Point rh(i,j,k-1)
       momvg_nh(l1_nh+c_m) = -a_m
       if (k.eq.0.or.k.eq.N)                                           &
                            momvg_nh(l1_nh+c_m) = 2.*momvg_nh(l1_nh+c_m)
!      c_m = c_m + mijk2lq_nh(i,j,k-1)

      enddo
#endif

      endif

!*******************************************************************
!     Check!
!*******************************************************************
#ifdef CHECK_CROCO
!CXA       call set_tracedirectory(iteration3d)
        call set_tracedirectory(iic)
        filetrace='mat_mom_it_'//int2string(iic)//'.'//int2string(mynode)//'.txt'
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
