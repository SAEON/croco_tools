#include "cppdefs.h"
#ifdef NBQ
      subroutine  z_thickness_nh(ichoix)

!__________________________________________________________________________
!
!                               SNH2012.14      
!                 Non-Hydrostatic & Non-Boussinesq Kernel Version  
! Laboratoire d'Aerologie, 14 Avenue Edouard Belin, F-31400 Toulouse
! http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm  
! !!!XA a verifier que l'update des epaisseurs de couche 
! !!! est ok avec omega et la surface libre
!  http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages/Coord_Conservation_Masse_Volume.htm
!__________________________________________________________________________

!CXA      use module_parallele !#mpi
      use module_nh
      use module_nbq
      implicit none
#include "param_F90.h"
#include "grid_F90.h"
#include "ocean3d_F90.h"

      integer :: i,j,k,kmax,imax,jmax
      integer :: ichoix

      real Hzr_half_nbq(GLOBAL_2D_ARRAY,0:N+1)
      common /grid_Hzr_half_nbq/ Hzr_half_nbq
      real Hzw_half_nbq(GLOBAL_2D_ARRAY,0:N)
      common /grid_Hzw_half_nbq/ Hzw_half_nbq

      real zr_half_nbq(GLOBAL_2D_ARRAY,N)
      common /grid_zr_half_nbq/ zr_half_nbq
      real zw_half_nbq(GLOBAL_2D_ARRAY,0:N)
      common /grid_zw_half_nbq/ zw_half_nbq

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

      if (ichoix.eq.0 .or. ichoix.eq.1) then  !!CXA simplification
         !write(*,*)'je passe dans z_thickness '
!**********************************************************************
!    Initialisations
!**********************************************************************
!CXA !!!!!! aux indices qui debordent dans toutes les directions. 
!CXA      do k=0,kmax+1
        do k=1,kmax
          do j=1,jmax+1
            do i=1,imax+1
              gdepth_u(i,j,k) = zr_half_nbq(i,j,k)-zr_half_nbq(i-1,j  ,k)
              gdepth_v(i,j,k) = zr_half_nbq(i,j,k)-zr_half_nbq(i  ,j-1,k)
              coefa_u(i,j,k)  = 0.25*pm_u(i,j)*                        &
                     (Hzr_half_nbq(i,j,k  )+Hzr_half_nbq(i-1,j,k  ))/  &
                     (Hzw_half_nbq(i,j,k-1)+Hzw_half_nbq(i-1,j,k-1))
              coefb_u(i,j,k)  = 0.25*pm_u(i,j)*                        &
                     (Hzr_half_nbq(i,j,k  )+Hzr_half_nbq(i-1,j,k  ))/  &
                     (Hzw_half_nbq(i,j,k  )+Hzw_half_nbq(i-1,j,k  ))
              coefa_v(i,j,k)  = 0.25*pn_v(i,j)*                        &
                     (Hzr_half_nbq(i,j,k  )+Hzr_half_nbq(i-1,j,k  ))/  &
                     (Hzw_half_nbq(i,j,k-1)+Hzw_half_nbq(i-1,j,k-1))
              coefb_u(i,j,k)  = 0.25*pn_v(i,j)*                        &
                     (Hzr_half_nbq(i,j,k  )+Hzr_half_nbq(i-1,j,k  ))/  &
                     (Hzw_half_nbq(i,j,k  )+Hzw_half_nbq(i-1,j,k  ))
!         if (abs(gdepth_u(i,j,k)).gt.1d-10) then 
!            write(*,*) 'ca merde dans z_thickness 1 ',i,' ',j,' ',k,' ', &
!               gdepth_u(i,j,k), ' ',z_r(i,j,k), ' ',z_r(i-1,j  ,k) 
!         endif
!         if (abs(gdepth_v(i,j,k)).gt.1d-10) then 
!            write(*,*) 'ca merde dans z_thickness 2 ',i,' ',j,' ',k,' ', & 
!               gdepth_v(i,j,k), ' ',z_r(i,j,k), ' ',z_r(i-1,j  ,k)
!         endif
            enddo
          enddo
        enddo

        do i = 1 , imax + 1
          do j = 1 , jmax + 1
!CXA          coefa_u(i,j,0)    = 0.5 / dx_u(i,j) * real (slip_exp)   ! BESOIN ???????
!CXA          coefa_v(i,j,0)    = 0.5 / dy_v(i,j) * real (slip_exp)   ! BESOIN ???????
            coefa_u(i,j,0)    = 0.5 * pm_u(i,j) * real (slip_nbq)   ! BESOIN ???????
            coefa_v(i,j,0)    = 0.5 * pn_v(i,j) * real (slip_nbq)   ! BESOIN ???????
            coefa_u(i,j,1)    = 0. 
            coefa_v(i,j,1)    = 0.
            coefb_u(i,j,kmax) = 0.                                 
            coefb_v(i,j,kmax) = 0.                                 
!CXA          coefb_u(i,j,kmax+1)   = 0.5 / dx_u(i,j)  
!CXA          coefb_v(i,j,kmax+1)   = 0.5 / dy_v(i,j)  
            coefb_u(i,j,kmax+1)   = 0.5 * pm_u(i,j)  
            coefb_v(i,j,kmax+1)   = 0.5 * pn_v(i,j)  
          enddo
        enddo

!CXA surface and bottom coefa and b already treated in initial_nh.F90
!CXA we do not touch anything

!CXA treatment of bottom
        k=0
        do j=1,jmax+1
          do i=1,imax+1
            gdepth_u(i,j,k) = zw_half_nbq(i,j,k)-zw_half_nbq(i-1,j  ,k)
            gdepth_v(i,j,k) = zw_half_nbq(i,j,k)-zw_half_nbq(i  ,j-1,k)
!        if (abs(gdepth_u(i,j,k)).gt.1d-10) then 
!            write(*,*) 'ca merde dans z_thickness 3 ',i,' ',j,' ',k,' ', &
!               gdepth_u(i,j,k), ' ',z_r(i,j,k), ' ',z_r(i-1,j  ,k) 
!         endif
!         if (abs(gdepth_v(i,j,k)).gt.1d-10) then 
!            write(*,*) 'ca merde dans z_thickness 4 ',i,' ',j,' ',k,' ', &
!               gdepth_v(i,j,k), ' ',z_r(i,j,k), ' ',z_r(i,j-1,k)
!
!         endif
          enddo
        enddo
!CXA treatment of surface
        k=N+1
        do j=1,jmax+1
          do i=1,imax+1
            gdepth_u(i,j,k) = zw_half_nbq(i,j,k-1)-zw_half_nbq(i-1,j  ,k-1)
!        write(*,*)'gdepth_u',i,j,k,gdepth_u(i,j,k)
            gdepth_v(i,j,k) = zw_half_nbq(i,j,k-1)-zw_half_nbq(i  ,j-1,k-1)
!        if (abs(gdepth_u(i,j,k)).gt.1d-10) then 
!           write(*,*) 'ca merde dans z_thickness 5 ',i,' ',j,' ',k,' ', & 
!              gdepth_u(i,j,k), ' ',z_r(i,j,k), ' ',z_r(i,j-1,k) 
!        endif
!        if (abs(gdepth_v(i,j,k)).gt.1d-10) then 
!            write(*,*) 'ca merde dans z_thickness 6 ',i,' ',j,' ',k,' ', &
!               gdepth_v(i,j,k), ' ',z_r(i,j,k), ' ',z_r(i,j-1,k)
!        endif
          enddo
        enddo

      endif ! ichoix
     
#ifdef TOTO
      if (ichoix.eq.1) then
!**********************************************************************
!    Mises a jour
!**********************************************************************
!CXA !!!!!! aux indices qui debordent dans toutes les directions. 

        do k=0,kmax+1
          do j=1,jmax+1
            do i=1,imax+1
              gdepth_u(i,j,k) =                                           &
                 sigma_t(i,j,k)*(h_w(i,j)+ssh_int_w(i,j,2))-h_w(i,j)      &
!MVBXA          - bathy_mvb_t(i,j,2) * real(ifl_mvbat_exp)                &  ! #MVB#
          - (                                                             &
             sigma_t(i-1,j,k)*(h_w(i-1,j)+ssh_int_w(i-1,j,2))-h_w(i-1,j)  &  ! #MVB#
!MVBXA          - bathy_mvb_t(i-1,j,2) * real(ifl_mvbat_exp)              &
            )                                                      
          
              gdepth_v(i,j,k) =                                           &
             sigma_t(i,j,k)*(h_w(i,j)+ssh_int_w(i,j,2))-h_w(i,j)          &
!MVBXA          - bathy_mvb_t(i,j,2) * real(ifl_mvbat_exp)                &  ! #MVB#
          - (                                                             &
             sigma_t(i,j-1,k)*(h_w(i,j-1)+ssh_int_w(i,j-1,2))-h_w(i,j-1)  &
!MVBXA          -  bathy_mvb_t(i,j-1,2) * real(ifl_mvbat_exp)             &  ! #MVB#
            )
            enddo
          enddo
        enddo
      
      endif
#endif
      return
      end subroutine z_thickness_nh

#else
      subroutine z_thickness_nh_empty
      return
      end 
#endif


