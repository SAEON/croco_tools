#include "cppdefs.h"
#ifdef NBQ
      module module_nh


!__________________________________________________________________________
!
!                               SNH2012.14      
!                 Non-Hydrostatic & Non-Boussinesq Kernel Version  
! Laboratoire d Aerologie, 14 Avenue Edouard Belin, F-31400 Toulouse
! http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm  
!
!__________________________________________________________________________


! debut module ne pas toucher a cette ligne

!.....nombre moyen de termes par ligne pour les matrices :
      integer, parameter  :: ntcont_nh=14,ntmom_nh=6,ntw_nh=5   ! A indexer sur la largeur de bande...
!.....calcul de profiles:
      integer, parameter  :: nbpmax_o = 100
      integer, parameter  :: nbvmax_o = 4
!.....taille min et max des variables obc:
      integer, parameter  :: nobcmin=-1,nobcmax=3

!.....taille du vecteur q ==> lignes de cont:
!     frontieres necessaires pour l utilisation parallele.
      integer  :: nmq_nh

!.....taille du vecteur v ==> lignes de mom:
      integer  :: nmv_nh

!.....taille max de la matrice cont:
      integer  :: nmcont_nh

!.....taille max de la matrice mom:
      integer  :: nmmom_nh

!.....taille max de la matrice prod:
      integer  :: nmw_nh

!**********************************************************************
!.....Flags et dimensions "integer"
!**********************************************************************
      integer                                                         &
          diffx_nh                                                    &
        ,diffy_nh                                                     &
        ,diffz_nh                                                     &
        ,grenzp_nh                                                    &
        ,grng                                                         &
        ,grnzg_nh                                                     &
        ,grnzp_nh                                                     &
        ,grnzgp_nh                                                    &
        ,groffset                                                     &
        ,iflg_nh        (4)                                           &
        ,ifl_diff_cox                                                 &
        ,ifl_div_nh                                                   &
        ,ifl_nh                                                       &
        ,ifl_solv_nh                                                  &
        ,ifl_time_nh                                                  &
        ,l_nh                                                         &
        ,l0_nh                                                        &
        ,l1_nh                                                        &
        ,nnz_nh         (10)                                          &
        ,ompnnz_nh         (10)                                       &
        ,nzeq_nh                                                      &
        ,nzq_nh                                                       &
        ,nzcont_nh                                                    &             
        ,nzmom_nh                                                     &     
        ,offsetloc                                                    &
        ,offsetvoisin                                                 &      
        ,trunc_nh                                                     &
        ,omp_l0_nh                                                    &
        ,neqcont_nh     (0:7)                                         &
        ,neqmom_nh      (0:10)                                        &
        ,offset_mpicores (2)                                          & ! norestart
        ,neqcont_omp                                                  & 
        ,neqcont2_omp                                                 & 
        ,neqmom_omp                                                   & 
        ,nnz_omp                                                      &
        ,nnz_cont_omp                                                 &
        ,nnz_mom_omp                                                  &
        ,k1_omp                                                       &
        ,k2_omp                                                       &
        ,nmlmom_nh                                                    &  
        ,nmlcont_nh
       
      integer,dimension(:),allocatable ::                          &        
         conti_nh     & !(nmq_nh)                                      & ! norestart
        ,ompconti_nh  & ! (nmq_nh)                                   & ! norestart
        ,cont_nnz_nh  & ! (nmq_nh)                                      & 
        ,contj_nh     & ! (nmcont_nh)                                   & ! norestart    
        ,ompcontj_nh  & ! (nmcont_nh)                                & ! norestart    
        ,neqint_nh    & ! (0:nbdom_world-1)                             &
        ,nnzint_nh      ! (0:nbdom_world-1)                                   

      integer*8      ::                                                 &
         dkpred_nh

!**********************************************************************
!.....Variables Real
!**********************************************************************
      real ::      &
       period_exp  &
      ,for_a_exp   &
      ,dg_exp      &
      ,hmax_exp    &
      ,amp_exp

!**********************************************************************
!.....Tableaux "integer"
!**********************************************************************
      integer, dimension(:), allocatable     ::                           &
         fqg_nh         &  !(nmq_nh)                                      &   ! bcp trop grand... dim. a diminuer.
        ,greip_nh       &  !(0:nbdom_world+1)                             & ! norestart 
        ,grep_nh        &  !(1:nbdom_world+1)                             &
        ,grepci_nh      &  !(int(nmq_nh*1.5))                             & ! norestart
        ,grepgi_nh      &  !(0:nbdom_world+1)                             & ! norestart
        ,grepgv_nh      &  !(int(nmq_nh*1.5))                             & ! norestart
        ,grip_nh        &  !(0:nbdom_world+1)                             & ! norestart
        ,grlg_nh        &  !(int(nmq_nh*1.2))                             & ! norestart 
        ,grlp_nh        &  !(int(nmq_nh*1.2))                             & ! norestart
        ,grneqint_nh    &  !(0:nbdom_world-1)                             &
        ,grp_nh         &  !(1:nbdom_world+1)                             &
        ,grpci_nh       &  !(int(nmq_nh*1.2))                             & ! norestart
        ,grpgi_nh       &  !(0:nbdom_world+1)                             & ! norestart
        ,grpgv_nh       &  !(int(nmq_nh*1.2))                             & ! norestart
        ,grplv_nh       &  !(int(nmq_nh*1.2))                             & ! norestart
        ,grworki_nh     &  !(int(nmq_nh*1.2))                             & ! norestart
        ,grworkj_nh     &  !(int(nmw_nh*1.2))                             & ! norestart
        ,iw1_nh         &  !(2*nbdom_world*nmq_nh+1)                      &
        ,iw2_nh         &  !(nbdom_world*nmq_nh+1)                        &
        ,iw3_nh         &  !(0:nbdom_world+1)                             & ! norestart
        ,iw4_nh         &  !(0:int(nmq_nh*1.5))                           & ! norestart
        ,iw5_nh         &  !(int(nmq_nh*1.2))                             & ! norestart
        ,iw6_nh         &  !(int(nmw_nh*1.5))                             & ! norestart
        ,iw7_nh         &  !(int(nmw_nh*1.5))                             & ! norestart
        ,l2iq_nh        &  !(nmq_nh)                                      & ! norestart
        ,l2jq_nh        &  !(nmq_nh)                                      & ! norestart
        ,l2kq_nh        &  !(nmq_nh)                                      & ! norestart
        ,l2imom_nh      &  !(nmv_nh)                                      & ! norestart
        ,l2jmom_nh      &  !(nmv_nh)                                      & ! norestart
        ,l2kmom_nh      &  !(nmv_nh)                                      & ! norestart
        ,momi_nh        &  !(nmv_nh)                                      &
        ,momj_nh        &  !(nmmom_nh)                                    & ! norestart
        ,ompmomi_nh     &  !(nmv_nh)                                      &
        ,ompmomj_nh     &  !(nmmom_nh)                                    & ! norestart
        ,ompmomjg_nh    &  !(nmmom_nh)                                    & ! norestart
        ,momjg_nh       &  !(nmmom_nh)                                    & ! norestart
        ,worki_nh       &  !(nmq_nh)                                      & ! norestart
        ,workj_nh       &  !(nmw_nh)                                      & ! norestart
        ,ompworki_nh    &  !(nmq_nh)                                      & ! norestart
        ,ompworkj_nh    &  !(nmw_nh)                                      & ! norestart
        ,goffset_mpicores  !(0:nbdom_world-1)                            & ! norestart
        
      integer, dimension(:,:,:), allocatable    ::                      &
         ijk2lq_nh      &  !(GLOBAL_2D_ARRAY,0:kmax+2)                  & ! norestart
        ,ijk2lqg_nh     &  !(GLOBAL_2D_ARRAY,0:kmax+2)                  &       
        ,mijk2lq_nh        !(GLOBAL_2D_ARRAY,0:kmax+2)                   ! norestart

      integer, dimension(:,:,:,:), allocatable  ::                      &
         ijk2lg_nh      &  !(GLOBAL_2D_ARRAY,0:kmax+2,0:nbdom_world-1)  & ! norestart
        ,ijk2lmom_nh    &  !(GLOBAL_2D_ARRAY,0:kmax+3,3)                  & ! norestartw
        ,mijk2lmom_nh      !(GLOBAL_2D_ARRAY,0:kmax+3,3)                   ! norestart
!CXA les deux tableaux ci dessus ont leur taille augmentee sur z avec
!indice 0



!**********************************************************************
!.....double precision
!**********************************************************************
!#ifdef simple_precision 
!      real*4                                                         &
!#else
      double precision                                                &
!#endif
         dxb_nh                                                       &
        ,time_step_nh                                                 

!**********************************************************************
!.....Tableaux: double precision
!**********************************************************************  
!#ifdef simple_precision 
!      real*4                                                         &
!#else
      double precision                                                &
!#endif
         t1_p                                                         &
        ,t2_p                                                         &
        ,t3_p                                                         &
        ,t4_p                                                         &
        ,t5_p                                                         
        
      double precision, dimension(:), allocatable      ::              &
         workv_nh       &  !(nmw_nh)                                      & ! norestart
        ,ompworkv_nh    &  !(nmw_nh)                                      & ! norestart
        ,contv_nh       &  !(0:nmcont_nh)                                 &
        ,ompcontv_nh    &  !(0:nmcont_nh)                                 &
        ,grrhs2_nh      &  !(int(nmq_nh*1.2))                             & ! norestart
        ,grworkv_nh     &  !(int(nmw_nh*1.2))                             & ! norestart
        ,grworkv0_nh    &  !(int(nmw_nh*1.2))                             &
        ,momv_nh        &  !(nmmom_nh)                                    &
        ,momvg_nh       &  !(nmmom_nh)                                    &
        ,ompmomv_nh     &  !(nmmom_nh)                                    &
        ,dw6_nh         &  !(int(nmw_nh*1.5))                             & ! norestart
        ,dw7_nh         &  !(int(nmw_nh*1.5))                             & ! norestart
        ,rhs1_nh        &  !(nmv_nh)                                      &
        ,rhs2_nh        &  !(nmq_nh)                                      &
        ,rhs2b_nh        &  !(nmq_nh)                                      &
        ,omprhs2_nh     &  !(nmq_nh)                                      &
        ,rhs3_nh        &  !(nmq_nh)                                      &
        ,omprhs3_nh     &  !(nmq_nh)                                      &
        ,rhs4_nh        &  !(nmv_nh)                                      &
        ,rhs5_nh        &  !(nmv_nh)                                      &
        ,rhs6_nh        &  !(nmv_nh)                                      &
        ,rhs7_nh           !(nmv_nh)                                      

      double precision, dimension(:,:), allocatable    ::              &
         advx_u         &  !(GLOBAL_2D_ARRAY)                           &
        ,advy_v         &  !(GLOBAL_2D_ARRAY)                           &
        ,coriolis_nh_t  &  !(GLOBAL_2D_ARRAY)                           &
        ,xadvf_u        &  !(GLOBAL_2D_ARRAY)                           &
        ,yadvf_v        &  !(GLOBAL_2D_ARRAY)                           &
        ,velb_u         &  !(GLOBAL_2D_ARRAY)                           &
        ,velb_v         &  !(GLOBAL_2D_ARRAY)                           &
        ,dvbar_nh_u     &  !(GLOBAL_2D_ARRAY)                           &
        ,dvbar_nh_v        !(GLOBAL_2D_ARRAY)                           &
        
      double precision, dimension(:,:,:), allocatable   ::            &
         coefa_u        &  !(GLOBAL_2D_ARRAY,0:kmax+1)                  &
        ,coefb_u        &  !(GLOBAL_2D_ARRAY,0:kmax+1)                  &
        ,coefa_v        &  !(GLOBAL_2D_ARRAY,0:kmax+1)                  &
        ,coefb_v        &  !(GLOBAL_2D_ARRAY,0:kmax+1)                  & 
        ,dsig_w_nh      &  !(GLOBAL_2D_ARRAY,0:kmax)                  &
        ,dsig_t_nh      &  !(GLOBAL_2D_ARRAY,0:kmax+1)                  &
        ,coef2d_diff_nh &  !(GLOBAL_2D_ARRAY,4)                         &
        ,dzta_nh        &  !(GLOBAL_2D_ARRAY,0:2)                       &
        ,flux_ext_ass_nh &  !(GLOBAL_2D_ARRAY,2)                        &
        ,gdepth_u       &  !(GLOBAL_2D_ARRAY,0:kmax+1)                  &
        ,gdepth_v       &  !(GLOBAL_2D_ARRAY,0:kmax+1)                  &
        ,pnh_t          &  !(GLOBAL_2D_ARRAY,kmax)                      &
        ,profnh_t       &  !(GLOBAL_2D_ARRAY,0:kmax)                    & 
        ,sigma_t        &  !(GLOBAL_2D_ARRAY,0:kmax+1)                  &
        ,sponge_w       &  !(GLOBAL_2D_ARRAY,2)                         &
        ,xynh_t         &  !(GLOBAL_2D_ARRAY,0:2)                       &
        ,dsig_w         &  !(GLOBAL_2D_ARRAY,kmax+1)                    &
        ,xdiff_u        &  !(GLOBAL_2D_ARRAY,2)                         &
        ,ydiff_v           !(GLOBAL_2D_ARRAY,2)                         

        
      double precision, dimension(:,:,:,:), allocatable     ::          &
         coef_diff_nh   &  !(GLOBAL_2D_ARRAY,1:kmax,6)                   &
        ,diff_div_nh    &  !(GLOBAL_2D_ARRAY,1:kmax,2)                   &
        ,div_nh_t       &  !(GLOBAL_2D_ARRAY,0:kmax,2)                   & ! norestart
        ,dz_w           &  !(GLOBAL_2D_ARRAY,0:kmax+1,-1:2)              &
        ,flux_int_ass_nh &  !(GLOBAL_2D_ARRAY,0:kmax+1,3)                &       
        ,gpnh_u         &  !(GLOBAL_2D_ARRAY,0:kmax+1,-1:1)              &
        ,gpnh_v         &  !(GLOBAL_2D_ARRAY,0:kmax+1,-1:1)              &
        ,gpnh_w         &  !(GLOBAL_2D_ARRAY,0:kmax+1,-1:1)              &
        ,veldxdz_w      &  !(GLOBAL_2D_ARRAY,0:kmax+1,1)                 & 
        ,veldydz_w      &  !(GLOBAL_2D_ARRAY,0:kmax+1,1)                 & 
        ,velobc_w       &  !(GLOBAL_2D_ARRAY,0:kmax+1,nobcmin:nobcmax)   &
        ,vel_w             !(GLOBAL_2D_ARRAY,0:kmax+1,-1:2)             


      real,dimension(:,:),allocatable     ::                                                        &
         vg2d_nh           ! (1:max(imax,6),1:max(jmax,6))                 &
      real,dimension(:,:,:),allocatable   ::							&                   
         vg3d_nh            ! (1:max(imax,6),1:max(jmax,6),1:kmax+1)       

      double precision                                                     &
         time_omp_nh    (100)                                         

      integer         &
         gl_mpi_nh      (2*8)  

! fin module ne pas toucher a cette ligne
         
         contains!---------------------------------------------------------------------
!CXA         subroutine alloc_module_nh(imax,jmax,kmax,nbdom_world)
         subroutine alloc_module_nh()
         implicit none
# include "param_F90.h"
!CXA         integer,intent(in) :: imax,jmax,kmax,nbdom_world
         integer :: imax,jmax,kmax,nbdom_world
#ifdef MPI
!#define LOCALLM Lmmpi
!#define LOCALMM Mmmpi
#define LOCALLM Lm
#define LOCALMM Mm
#else
#define LOCALLM Lm
#define LOCALMM Mm
#endif    
      kmax=N  !CXA
      imax=LOCALLM
      jmax=LOCALMM

#ifdef MPI
         nbdom_world=NNODES
#else
         nbdom_world=1
#endif

#ifndef NOHIPS
         nmq_nh=(imax+2)*(jmax+2)*(kmax+1)
!        nmv_nh=(imax+2)*(jmax+2)*(kmax+1)*4
         nmv_nh=(imax+2)*(jmax+2)*kmax  &
                                   +(imax+2)*(jmax+2)*kmax  &
                                   +(imax+2)*(jmax+2)*(kmax+1)      *2.
         nmw_nh=nmv_nh*ntw_nh
#endif
#ifdef NOHIPS
         nmq_nh=(imax+2)*(jmax+2)*kmax*5
         nmv_nh=(imax+2)*(jmax+2)*kmax  &
                                   +(imax+2)*(jmax+2)*kmax  &
                                   +(imax+2)*(jmax+2)*(kmax+1)      *2.
         nmw_nh=1.
#endif

         nmcont_nh=nmq_nh*ntcont_nh
         nmmom_nh=nmv_nh*ntmom_nh

! Variables communes SNH / SNBQ
         allocate(conti_nh          (nmq_nh)                                 )   !  ! norestart
         allocate(cont_nnz_nh       (nmq_nh)                                 )   !  
         allocate(contj_nh          (nmcont_nh)                              )   !  ! norestart    
         allocate(neqint_nh         (0:nbdom_world-1)                        )   ! 
         allocate(nnzint_nh         (0:nbdom_world-1)                        )
         allocate(fqg_nh             (nmq_nh)                                       )   !    ! bcp trop grand... dim. a diminuer.
         allocate(l2iq_nh            (nmq_nh)                                       )   !  ! norestart
         allocate(l2jq_nh            (nmq_nh)                                       )   !  ! norestart
         allocate(l2kq_nh            (nmq_nh)                                       )   !  ! norestart
         allocate(l2imom_nh          (nmv_nh)                                       )   !  ! norestart
         allocate(l2jmom_nh          (nmv_nh)                                       )   !  ! norestart
         allocate(l2kmom_nh          (nmv_nh)                                       )   !  ! norestart
         allocate(momi_nh            (nmv_nh)                                       )   ! 
         allocate(momj_nh            (nmmom_nh)                                     )   !  ! norestart
         allocate(momjg_nh           (nmmom_nh)                                     )   !  ! norestart
         allocate(worki_nh           (nmq_nh)                                       )   !  ! norestart
         allocate(workj_nh           (nmw_nh)                                       )   !  ! norestart
         allocate(ijk2lq_nh          (GLOBAL_2D_ARRAY,0:kmax+2)                   )   !  ! norestart
         allocate(ijk2lqg_nh         (GLOBAL_2D_ARRAY,0:kmax+2)                   )   !        
         allocate(mijk2lq_nh        (GLOBAL_2D_ARRAY,0:kmax+2)                    )! norestart
         allocate(ijk2lg_nh          (GLOBAL_2D_ARRAY,0:kmax+2,0:nbdom_world-1)   )   !  ! norestart
         allocate(ijk2lmom_nh        (GLOBAL_2D_ARRAY,0:kmax+3,4)                   )   !  ! norestartw
         allocate(mijk2lmom_nh      (GLOBAL_2D_ARRAY,0:kmax+3,4)                    )! norestart
         allocate(contv_nh          (0:nmcont_nh)                               )   ! 
         allocate(momv_nh        (nmmom_nh)                                     )   ! 
         allocate(momvg_nh       (nmmom_nh)                                     )   ! 
         allocate(rhs1_nh        (nmv_nh)                                       )   ! 
         allocate(rhs2_nh        (nmq_nh)                                       )   ! 
         allocate(rhs2b_nh        (nmq_nh)                                       )   ! 
         allocate(coriolis_nh_t  (GLOBAL_2D_ARRAY)                            )   ! 
         allocate(velb_u         (GLOBAL_2D_ARRAY)                            )   ! 
         allocate(velb_v         (GLOBAL_2D_ARRAY)                            )   ! 
         allocate(coefa_u        (GLOBAL_2D_ARRAY,0:N+1)                   )   ! 
         allocate(coefb_u        (GLOBAL_2D_ARRAY,0:N+1)                   )   ! 
         allocate(coefa_v        (GLOBAL_2D_ARRAY,0:N+1)                   )   ! 
         allocate(coefb_v        (GLOBAL_2D_ARRAY,0:N+1)                   )   !  
         allocate(dsig_w_nh      (GLOBAL_2D_ARRAY,0:kmax)                   )   !
         allocate(dsig_t_nh      (GLOBAL_2D_ARRAY,0:kmax+1)                   )   !
         allocate(flux_ext_ass_nh (GLOBAL_2D_ARRAY,2)                         )   ! 
         allocate(gdepth_u       (GLOBAL_2D_ARRAY,0:N+1)                   )   ! 
         allocate(gdepth_v       (GLOBAL_2D_ARRAY,0:N+1)                   )   ! 
         allocate(profnh_t       (GLOBAL_2D_ARRAY,0:N)                     )   !  
         allocate(sigma_t        (GLOBAL_2D_ARRAY,0:N+1)                   )   ! 
         allocate(sponge_w       (GLOBAL_2D_ARRAY,2)                          )   ! 
         allocate(xynh_t         (GLOBAL_2D_ARRAY,0:2)                        )   ! 
         allocate(dsig_w         (GLOBAL_2D_ARRAY,N+1)                     )   ! 
         allocate(xdiff_u        (GLOBAL_2D_ARRAY,2)                          )   ! 
         allocate(ydiff_v        (GLOBAL_2D_ARRAY,2)                          )   ! 
         allocate(coef_diff_nh   (GLOBAL_2D_ARRAY,1:N,6)                   )   ! 
         allocate(diff_div_nh    (GLOBAL_2D_ARRAY,1:N,2)                   )   ! 
         allocate(div_nh_t       (GLOBAL_2D_ARRAY,0:N,2)                   )   !  ! norestart
            allocate(dz_w           (GLOBAL_2D_ARRAY,-1:kmax+1,-2:2)              )   ! 
         allocate(flux_int_ass_nh (GLOBAL_2D_ARRAY,0:N+1,3)                )   !        
         allocate(veldxdz_w      (GLOBAL_2D_ARRAY,-1:N+1,1)                 )   !  
         allocate(veldydz_w      (GLOBAL_2D_ARRAY,-1:N+1,1)                 )   !  
         allocate(velobc_w       (GLOBAL_2D_ARRAY,-1:N+1,nobcmin:nobcmax)   )   ! 
         allocate(vel_w          (GLOBAL_2D_ARRAY,-1:N+1,-1:2)    		)         
         allocate(vg2d_nh        (1:max(imax,6),1:max(jmax,6))                  )   ! 
         allocate(vg3d_nh        (1:max(imax,6),1:max(jmax,6),1:kmax+1)         )
         allocate(gpnh_u         (GLOBAL_2D_ARRAY,0:N+1,-1:1)              )   ! 
         allocate(gpnh_v         (GLOBAL_2D_ARRAY,0:N+1,-1:1)              )   ! 
         allocate(gpnh_w         (GLOBAL_2D_ARRAY,0:N+1,-1:1)              )   ! 
         allocate(advx_u         (GLOBAL_2D_ARRAY)                            )   ! 
         allocate(advy_v         (GLOBAL_2D_ARRAY)                            )   ! 
         allocate(xadvf_u        (GLOBAL_2D_ARRAY)                            )   ! 
         allocate(yadvf_v        (GLOBAL_2D_ARRAY)                            )   ! 

! Variables specifiques SNH 
#ifndef NOHIPS
         allocate(ompconti_nh       (nmq_nh)                                 )   !  ! norestart
         allocate(ompcontj_nh       (nmcont_nh)                              )   !  ! norestart    
         allocate(greip_nh           (0:nbdom_world+1)                              )   !  ! norestart 
         allocate(grep_nh            (1:nbdom_world+1)                              )   ! 
         allocate(grepci_nh          (int(nmq_nh*1.5))                              )   !  ! norestart
         allocate(grepgi_nh          (0:nbdom_world+1)                              )   !  ! norestart
         allocate(grepgv_nh          (int(nmq_nh*1.5))                              )   !  ! norestart
         allocate(grip_nh            (0:nbdom_world+1)                              )   !  ! norestart
         allocate(grlg_nh            (int(nmq_nh*1.2))                              )   !  ! norestart 
         allocate(grlp_nh            (int(nmq_nh*1.2))                              )   !  ! norestart
         allocate(grneqint_nh        (0:nbdom_world-1)                              )   ! 
         allocate(grp_nh             (1:nbdom_world+1)                              )   ! 
         allocate(grpci_nh           (int(nmq_nh*1.2))                              )   !  ! norestart
         allocate(grpgi_nh           (0:nbdom_world+1)                              )   !  ! norestart
         allocate(grpgv_nh           (int(nmq_nh*1.2))                              )   !  ! norestart
         allocate(grplv_nh           (int(nmq_nh*1.2))                              )   !  ! norestart
         allocate(grworki_nh         (int(nmq_nh*1.2))                              )   !  ! norestart
         allocate(grworkj_nh         (int(nmw_nh*1.2))                              )   !  ! norestart

         allocate(iw1_nh             (2*nbdom_world*nmq_nh+1)                       )   ! 
         allocate(iw2_nh             (nbdom_world*nmq_nh+1)                         )   ! 
         allocate(iw3_nh             (0:nbdom_world+1)                              )   !  ! norestart
         allocate(iw4_nh             (0:int(nmq_nh*1.5))                            )   !  ! norestart
         allocate(iw5_nh             (int(nmq_nh*1.2))                              )   !  ! norestart
         allocate(iw6_nh             (int(nmw_nh*1.5))                              )   !  ! norestart
         allocate(iw7_nh             (int(nmw_nh*1.5))                              )   !  ! norestart
         allocate(ompmomi_nh         (nmv_nh)                                       )   ! 
         allocate(ompmomj_nh         (nmmom_nh)                                     )   !  ! norestart
         allocate(ompmomjg_nh        (nmmom_nh)                                     )   !  ! norestart
         allocate(ompworki_nh        (nmq_nh)                                       )   !  ! norestart
         allocate(ompworkj_nh        (nmw_nh)                                       )   !  ! norestart
         allocate(goffset_mpicores  (0:nbdom_world-1)                               )   !  ! norestart
         allocate(workv_nh          (nmw_nh)                                    )   !  ! norestart
         allocate(ompworkv_nh       (nmw_nh)                                    )   !  ! norestart
         allocate(ompcontv_nh       (0:nmcont_nh)                               )   ! 
         allocate(grrhs2_nh      (int(nmq_nh*1.2))                              )   !  ! norestart
         allocate(grworkv_nh     (int(nmw_nh*1.2))                              )   !  ! norestart
         allocate(grworkv0_nh    (int(nmw_nh*1.2))                              )   ! 
         allocate(ompmomv_nh        (nmmom_nh)                                  )   ! 
         allocate(dw6_nh         (int(nmw_nh*1.5))                              )   !  ! norestart
         allocate(dw7_nh         (int(nmw_nh*1.5))                              )   !  ! norestart
         allocate(omprhs2_nh        (nmq_nh)                                    )   ! 
         allocate(omprhs3_nh        (nmq_nh)                                    )   ! 
         allocate(rhs3_nh        (nmq_nh)                                       )   ! 
         allocate(rhs4_nh        (nmv_nh)                                       )   ! 
         allocate(rhs5_nh        (nmv_nh)                                       )   ! 
         allocate(rhs6_nh        (nmv_nh)                                       )   ! 
         allocate(rhs7_nh        (nmv_nh)                                       )   ! 
         allocate(dvbar_nh_u     (GLOBAL_2D_ARRAY)                            )   ! 
         allocate(dvbar_nh_v     (GLOBAL_2D_ARRAY)                            )   ! 
         allocate(dzta_nh        (GLOBAL_2D_ARRAY,0:2)                        )   ! 
         allocate(pnh_t          (GLOBAL_2D_ARRAY,kmax)                       )   ! 
#endif

!        allocate(coef2d_diff_nh (GLOBAL_2D_ARRAY,4)                          )   ! 
! 
!        
         
         end subroutine alloc_module_nh

        end module module_nh  
#else
        module module_nh_empty
        end module
#endif
