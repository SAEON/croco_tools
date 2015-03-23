#include "cppdefs.h"
#ifdef NBQ
      subroutine initial_nh (ichoix)
!**********************************************************************
!
!                 NH and NBQ initialization
!
!**********************************************************************
!CXA MODULES TO ADD 

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

      integer :: i,j,k,kmax,imax,jmax
      integer ichoix
      integer :: lunit1
      double precision :: nb_simu,alpha_simu

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

      if (ichoix.eq.1) then
!
!----------------------------------------------------------------------
!  Initialize density perturbation and momentum arrays
!----------------------------------------------------------------------
!
        rhp_nbq_a(:,:) = 0.0
        qdm_nbq_a(:,:) = 0.0
!
!----------------------------------------------------------------------
!  Initialize parameters: should be done in a NH-namelist
!----------------------------------------------------------------------
!
        ifl_nbq  = 1        !CXA put elswhere or replace by cpp keys
        slip_nbq = 0
        slip_nbq = 0

        iteration_nbq_max=ndtnbq
        soundspeed_nbq=csound_nbq !!! pseudoacoustic speed for tank
        cw_int_nbq=soundspeed_nbq !!! ~ 2-10 sqrt(gH)_max

!       MPI_master_only write(stdout,'3(A,I4/)')
!                       'NBQ: INITIALIZING ifl_nbq      =',ifl_nbq
!                       '     INITIALIZING slip_nbq     =',slip_nbq
!                       '     INITIALIZING ifl_imp_nbq  =',ifl_imp_nbq
!
!----------------------------------------------------------------------
!  Compute vertical grid step at rho point
!----------------------------------------------------------------------
!
      do k=1,kmax
        do j=0,jmax+1
          do i=0,imax+1
            dsig_t_nh(i,j,k)=Cs_w(k)-Cs_w(k-1)
          enddo
        enddo
      enddo
!
!----------------------------------------------------------------------
!  Extend at top and bottom
!----------------------------------------------------------------------
!
      do j=0,jmax+1
        do i=0,imax+1
          dsig_t_nh(i,j,kmax+1) =   dsig_t_nh(i,j,kmax)
          dsig_t_nh(i,j,0)      =   dsig_t_nh(i,j,1) 
        enddo
      enddo
!
!----------------------------------------------------------------------
!  Same for dsig_w
!----------------------------------------------------------------------
!
      do i = 0 , imax + 1
        do j = 0 , jmax + 1
          do k = 1 , kmax-1
            dsig_w_nh(i,j,k)    = Cs_r(k+1)-Cs_r(k)
          enddo   
          dsig_w_nh(i,j,0     ) = Cs_r(1)-Cs_w(0)
          dsig_w_nh(i,j,kmax)   = Cs_w(kmax)-Cs_r(kmax)
        enddo     
      enddo   
!
!----------------------------------------------------------------------
!  Pre-numbering of grid points and matrices:
!----------------------------------------------------------------------
!

!... Numbering of pressure points:
      call nump_nh
!CXA      if (ifl_file_out.eq.1) call file_out('nh  ',1)

!... Initialize momentum equations matrix (mom):
      call mat_mom_init_nh

!... Initialize density equation matrix (cont):
      call mat_cont_init_nh

!
!----------------------------------------------------------------------
!  Check!
!----------------------------------------------------------------------
!
#ifdef CHECK_CROCO_0
!CXA       call set_tracedirectory(iteration3d)
      call set_tracedirectory(iic)
      filetrace=                                                 &
        'mat_mom_it_'//int2string(iic)//'.'//int2string(mynode)//'.txt'
      call printmat_mm(filetrace,neqmom_nh(0),neqcont_nh(0),     &
                       nzmom_nh,momi_nh,momj_nh,momv_nh)
      filetrace=                                                 &
        'mat_cont_it_'//int2string(iic)//'.'//int2string(mynode)//'.txt'
      call printmat_mm(filetrace,neqcont_nh(0),neqmom_nh(0),     &
                       nzcont_nh,conti_nh,contj_nh,contv_nh)
#endif
!
!----------------------------------------------------------------------
!  Pre-compute coefficients for sigma correction
!
!http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages/Schemas_Divergence.htm
!http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages/Schemas_GP.htm
!http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages/Schemas_Conservation_Energie.htm
!
!  Separation of static and dynamic terms (dt dependent)
!   --> one multiplication remains to be done in mat_cont and mat_mom
!----------------------------------------------------------------------
!
!... General case:
!    CXA overwritten every time step in z_thickness.F90
!
         do i = 0 , imax + 1                ! 0 ==> 1
         do j = 0 , jmax + 1
         do k = 1 , kmax
!CXA        coefa_u(i,j,k) = 0.25 / dx_u(i,j) * dsig_t (i,j,k) / dsig_w(i,j,k  ) 
!CXA        coefa_v(i,j,k) = 0.25 / dy_v(i,j) * dsig_t (i,j,k) / dsig_w(i,j,k  ) 
!CXA        coefb_u(i,j,k) = 0.25 / dx_u(i,j) * dsig_t (i,j,k) / dsig_w(i,j,k+1) 
!CXA        coefb_v(i,j,k) = 0.25 / dy_v(i,j) * dsig_t (i,j,k) / dsig_w(i,j,k+1) 
            coefa_u(i,j,k) = 0.25 / om_u(i,j) * dsig_t_nh(i,j,k) &
                                              / dsig_w_nh(i,j,k-1) 
            coefa_v(i,j,k) = 0.25 / on_v(i,j) * dsig_t_nh(i,j,k) &
                                              / dsig_w_nh(i,j,k-1) 
            coefb_u(i,j,k) = 0.25 / om_u(i,j) * dsig_t_nh(i,j,k) &
                                              / dsig_w_nh(i,j,k) 
            coefb_v(i,j,k) = 0.25 / on_v(i,j) * dsig_t_nh(i,j,k) &
                                              / dsig_w_nh(i,j,k) 
         enddo
         enddo
         enddo
!
!... Lower (k = 1) et upper (k=kmax+1) layers:
!
         do i = 0 , imax + 1
         do j = 0 , jmax + 1
!CXA        coefa_u(i,j,0)    = 0.5 / dx_u(i,j) * real (slip_exp)   ! BESOIN ???????
!CXA        coefa_v(i,j,0)    = 0.5 / dy_v(i,j) * real (slip_exp)   ! BESOIN ???????
            coefa_u(i,j,0)    = 0.5 / om_u(i,j) * real (slip_nbq)   ! BESOIN ???????
            coefa_v(i,j,0)    = 0.5 / on_v(i,j) * real (slip_nbq)   ! BESOIN ???????
            coefa_u(i,j,1)    = 0. 
            coefa_v(i,j,1)    = 0.
            coefb_u(i,j,kmax) = 0.                                 
            coefb_v(i,j,kmax) = 0.                                 
!CXA        coefb_u(i,j,kmax+1)   = 0.5 / dx_u(i,j)  
!CXA        coefb_v(i,j,kmax+1)   = 0.5 / dy_v(i,j)  
            coefb_u(i,j,kmax+1)   = 0.5 / om_u(i,j)  
            coefb_v(i,j,kmax+1)   = 0.5 / on_v(i,j)  
         enddo
         enddo
!
!----------------------------------------------------------------------
!... Initialize matrix products and HIPS:
!----------------------------------------------------------------------
!
#ifndef NOHIPS
!CXA      call poisson_nh(0)
#endif
!
!----------------------------------------------------------------------
!... Set second viscosity coefficient:
!----------------------------------------------------------------------
!
         call viscous_nbq(0)
!
!----------------------------------------------------------------------
!... Check: ASCII output of 3 time steps:
!----------------------------------------------------------------------
!
         call output_nbq(0)
!
!----------------------------------------------------------------------
!... Initialize Acoustic waves:
!----------------------------------------------------------------------
!
!        call density_nbq(10)

       endif     ! ichoix == 1
!
!
#ifdef MPI 
      ! Initialise nqd exchange (ikl2l_xxx is needed)
      call MPI_nbq_Setup(Lmmpi,Mmmpi,N)
#endif

      return
      end subroutine initial_nh
#else
      subroutine initial_nh_empty
      return
      end
#endif
