#include "cppdefs.h"
#ifdef NBQ
      subroutine initial_nh (ichoix)
!__________________________________________________________________________
!
!                               SNH2012.14      
!                 Non-Hydrostatic & Non-Boussinesq Kernel Version  
! Laboratoire d'Aerologie, 14 Avenue Edouard Belin, F-31400 Toulouse
! http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm  
!
!__________________________________________________________________________
! il reste des finalisations a faire dessous
!CXA MODULES TO ADD 

!CXA      use module_parallele !#mpi
      use module_nh
      use module_nbq
!CXA      use module_out
# ifdef TRACETXT
      use module_tracetxt_out
# endif
      implicit none
# include "param_F90.h"
# include "scalars_F90.h"
# include "grid_F90.h"
# include "ocean3d_F90.h"
      integer ndtnbq
      common /time_nbq1/ ndtnbq
      real csound_nbq
      common /nbq_csound/ csound_nbq

!CXA MODULES TO ADD 
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

#ifdef TOTO
      lunit1 = 3 + par%rank_mpicores

      if (ichoix.eq.0) then
!*****************************************************************
! Lecture du notebook_nh
! & initialisation de quelques flags.
!*****************************************************************

      open(unit=lunit1,file=nomfichier_default(23)) ! namelist_nh_default
      read (lunit1,list_nh) 
      close(lunit1)

      open(unit=lunit1,file=nomfichier(23)) ! namelist_nh
      read (lunit1,list_nh) 
      close(lunit1)

!......sorties ASCII
       if (ifl_nh.eq.1.and.par%rank_mpicores.eq.0) then
        write(6,*)
        write(6,*)
        write (6,*) '------------------------------'
        write (6,*) '   Noyau non-hydrostatique   '
        write (6,*) '------------------------------'
        write(6,*)
       elseif (par%rank_mpicores.eq.0) then
        write(6,*)
        write(6,*)
        write (6,*) '------------------------------'
        write (6,*) '   Noyau hydrostatique   '
        write (6,*) '------------------------------'
        write(6,*)
       endif
       endif


       if (ifl_nh.eq.1) then
!*******************************************************************   
! flag pour le calcul de la divergence:
!
!   ifl_div_p = 0 : pas de calcul,
!               1 : calcul avant inversion,
!               2 : calcul apres inversion,
!              -1 : calcul avant et apres inversion.
! 
!*******************************************************************
      ifl_div_nh = 1 

!*******************************************************************
! flag pour l'evaluation du temps de resolution (0/1):
!*******************************************************************
      ifl_time_nh = 0

!*******************************************************************
! flag pour la sortie ascii dans des fichiers (0/1):
!     ifl_file_out dans namelist_output
!*******************************************************************

!......sorties dans le fichier history:

      call history_nh(1)

      endif       ! ichoix == 0

#endif /* TOTO */

      if (ichoix.eq.1) then
        ifl_nbq=1                  !CXA to put elswhere or replace by cpp keys
        slip_nbq=0
        slip_nbq=0
        iteration_nbq_max=ndtnbq
        soundspeed_nbq=csound_nbq !!! pseudoacoustic speed for tank (~ 2-10 sqrt(gH)_max
        cw_int_nbq=soundspeed_nbq 

        write(6,*) 'INITIALISATION EN DUR ifl_nbq      =',ifl_nbq
        write(6,*) 'INITIALISATION LU iteration_nbq_max=',ndtnbq
        write(6,*) 'INITIALISATION LU csound           =',soundspeed_nbq
        write(6,*) 'INITIALISATION EN DUR slip_nbq     =',slip_nbq
        write(6,*) 'INITIALISATION EN DUR ifl_imp_nbq  =',ifl_imp_nbq
        write(6,*) ' '

        rhp_nbq_a(:,:) = 0.0
        qdm_nbq_a(:,:) = 0.0

!...........................................................
! on calcule l espacement sigma
!...........................................................
      do k=1,kmax
        do j=0,jmax+1
          do i=0,imax+1
            dsig_t_nh(i,j,k)=Cs_w(k)-Cs_w(k-1)
          enddo
        enddo
      enddo
!...........................................................
! on etend en haut et en bas
!...........................................................
      do j=0,jmax+1
        do i=0,imax+1
          dsig_t_nh(i,j,kmax+1) =   dsig_t_nh(i,j,kmax)
          dsig_t_nh(i,j,0)    =   dsig_t_nh(i,j,1) 
        enddo
      enddo
      

!...........................................................
! idem pour dsig_w
!...........................................................
      do i = 0 , imax + 1
        do j = 0 , jmax + 1
          do k = 1 , kmax-1
            dsig_w_nh(i,j,k)    = Cs_r(k+1)-Cs_r(k)
          enddo   
          dsig_w_nh(i,j,0     ) = Cs_r(1)-Cs_w(0)
          dsig_w_nh(i,j,kmax)   = Cs_w(kmax)-Cs_r(kmax)
        enddo     
      enddo   


!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  ! #OMP#
! #OMP#  : entre univers  par%comm2d                                  ! #OMP#
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  ! #OMP#
!CXA      if (par%rank.ne.-1) then                                        ! #OMP#

!*****************************************************************
!.....prenumerotation des points de grille et des matrices:
!*****************************************************************
!.....numerotation des points de pressions:
      call nump_nh
!CXA      if (ifl_file_out.eq.1) call file_out('nh  ',1)

!.....initialisation de la matrice des equations du mouvement (mom):
      call mat_mom_init_nh

!.....initialisation de la matrice de l equation de continuite:
      call mat_cont_init_nh
!-------------------------------------------------------------
! Trace : Print de matrice cont et mom
!-------------------------------------------------------------
#ifdef CHECK_CROCO_0
!CXA       call set_tracedirectory(iteration3d)
        call set_tracedirectory(iic)
	filetrace='mat_mom_it_'//int2string(iic)//'.txt'
	call printmat_mm(filetrace,neqmom_nh(0),neqcont_nh(0),     &
			    nzmom_nh,momi_nh,         &
	    momj_nh,momv_nh)
	filetrace='mat_cont_it_'//int2string(iic)//'.txt'
	call printmat_mm(filetrace,neqcont_nh(0),neqmom_nh(0),     &
			    nzcont_nh,conti_nh,         &
	    contj_nh,contv_nh)

#endif
!*****************************************************************
! precalcul des coef pour la correction sigma
!http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages/Schemas_Divergence.htm
!http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages/Schemas_GP.htm
!http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages/Schemas_Conservation_Energie.htm
!!! separation des termes qui dependent du pas de temps de ceux qui n'en
!   dependent pas. -> il reste encore un terme multiplicatif a faire
!   dans mat_cont et mat_mom
!*****************************************************************

!.......cas general:
!CXA overwritten every time step in z_thickness.F90
         do i = 1 , imax + 1                ! 0 ==> 1
         do j = 1 , jmax + 1
         do k = 1 , kmax
!CXA            coefa_u(i,j,k) = 0.25 / dx_u(i,j) * dsig_t (i,j,k) / dsig_w(i,j,k  ) 
!CXA            coefa_v(i,j,k) = 0.25 / dy_v(i,j) * dsig_t (i,j,k) / dsig_w(i,j,k  ) 
!CXA            coefb_u(i,j,k) = 0.25 / dx_u(i,j) * dsig_t (i,j,k) / dsig_w(i,j,k+1) 
!CXA            coefb_v(i,j,k) = 0.25 / dy_v(i,j) * dsig_t (i,j,k) / dsig_w(i,j,k+1) 
            coefa_u(i,j,k) = 0.25 / om_u(i,j) * dsig_t_nh (i,j,k) / dsig_w_nh(i,j,k-1  ) 
            coefa_v(i,j,k) = 0.25 / on_v(i,j) * dsig_t_nh (i,j,k) / dsig_w_nh(i,j,k-1  ) 
            coefb_u(i,j,k) = 0.25 / om_u(i,j) * dsig_t_nh (i,j,k) / dsig_w_nh(i,j,k) 
            coefb_v(i,j,k) = 0.25 / on_v(i,j) * dsig_t_nh (i,j,k) / dsig_w_nh(i,j,k) 
         enddo
         enddo
         enddo
         
!.......couches inferieure (k = 1) et superieure (k=kmax+1):
         do i = 1 , imax + 1
         do j = 1 , jmax + 1

!CXA            coefa_u(i,j,0)    = 0.5 / dx_u(i,j) * real (slip_exp)   ! BESOIN ???????
!CXA            coefa_v(i,j,0)    = 0.5 / dy_v(i,j) * real (slip_exp)   ! BESOIN ???????
            coefa_u(i,j,0)    = 0.5 / om_u(i,j) * real (slip_nbq)   ! BESOIN ???????
            coefa_v(i,j,0)    = 0.5 / on_v(i,j) * real (slip_nbq)   ! BESOIN ???????
            coefa_u(i,j,1)    = 0. 
            coefa_v(i,j,1)    = 0.
            coefb_u(i,j,kmax) = 0.                                 
            coefb_v(i,j,kmax) = 0.                                 
!CXA            coefb_u(i,j,kmax+1)   = 0.5 / dx_u(i,j)  
!CXA            coefb_v(i,j,kmax+1)   = 0.5 / dy_v(i,j)  
            coefb_u(i,j,kmax+1)   = 0.5 / om_u(i,j)  
            coefb_v(i,j,kmax+1)   = 0.5 / on_v(i,j)  

         enddo
         enddo

!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  ! #OMP#
! #OMP#  : entre univers  par%comm2d                                  ! #OMP#
!@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@  ! #OMP#
!CXA      endif         ! endif ==> par%rank .ne. -1                      ! #OMP#

!.....initialisation des produits matriciels et de HIPS:
#ifndef NOHIPS
!CXA      call poisson_nh(0)
#endif
       endif     ! ichoix == 1


      return
      end subroutine initial_nh
#else
      subroutine initial_nh_empty
      return
      end
#endif
