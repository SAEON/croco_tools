#include "cppdefs.h"
#ifdef NBQ
      module module_nbq

      implicit none

!__________________________________________________________________________________
!
!                               SNH2012.14      
!                 Non-Hydrostatic & Non-Boussinesq Kernel Version  
! Laboratoire d Aerologie, 14 Avenue Edouard Belin, F-31400 Toulouse
! http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm  
!
!__________________________________________________________________________________


! debut module ne pas toucher à cette ligne
      integer  :: nmq_nbq,nmcont_nbq
      integer  :: nmv_nbq,nmmom_nbq,nmprod_nbq

!**********************************************************************
!.....Flags et dimensions
!**********************************************************************
      integer             ::                                          &
       iteration_nbq_max                                              &
      ,iteration_nbq                                                  &
      ,l_nbq                                                          &
      ,l0_nbq                                                         &
      ,l1_nbq                                                         &
      ,ifl_nbq                                                        &
      ,slip_nbq          !CXA

      integer             ::                                          &
       neqcorrt_nbq
                                
      double precision    ::                                          &
       ifl_filtre_nbq                                                 &
      ,fl_nbq                                                         &
      ,cw_int_nbq                                      !CXA

!**********************************************************************
!.....double precision
!**********************************************************************

!.....Variables 0D:
      double precision    ::                                          &
       assel1_nbq                                                     &
      ,assel2_nbq                                                     &
      ,edt_nbq_bk                                                     &
      ,edt_nbq_lp                                                     &
      ,soundspeed_nbq                                                 &
      ,soundspeed2_nbq                                                &
      ,visc2_nbq0                                                     &
      ,time_nbq           
      double precision, dimension(:,:),allocatable   ::               &
       soundspeed2v_nbq						          !(GLOBAL_2D_ARRAY)                         &
      double precision, dimension(: ),allocatable   ::                &
       soundspeed2q_nbq						      &    !(0:nmq_nbq)                                &
      ,soundspeed2mom_nbq					           ! (0:nmv_nbq)                                  &

 !....Variables algébriques mode NBQ:
      double precision,dimension(:),allocatable     ::                                          &
       dqdmdt_nbq_a      					   !    &         ! (nmv_nbq)                                    &
      double precision,dimension(:,:),allocatable     ::		&
       flux_ass_nbq_a      						&         ! (nmv_nbq,2)                                  &    ! Pour filtre d Asselin "Boucle NBQ"
      ,qdm_nbq_a           						&         ! (0:nmv_nbq,-1:2)                             &
      ,rhp_nbq_a           						&         ! (0:nmq_nbq,0:2)                              &
      ,qdm_nbq_a_bak           						&         ! (0:nmv_nbq,-1:2)                             &
      ,rhp_nbq_a_bak           						&         ! (0:nmq_nbq,0:2)                              &
      ,rhpsum_nbq_a        						&         ! (0:nmq_nbq,1:2)                              &   
      ,rhpsum2_nbq_a       						&         ! (0:nmq_nbq,1:2)                              &   
      ,diffsum_nbq_a       						&         ! (0:nmv_nbq,1:2)                              &
      ,diffsum2_nbq_a       						&         ! (0:nmv_nbq,1:2)                              &
      ,rhssum_nbq_a       						&         ! (0:nmv_nbq,1:2)                              &
      ,div_nbq_a          						          ! (0:nmq_nbq,0:2)                  

 !....Tableaux de travail NBQ:
      double precision,dimension(:),allocatable    ::                   &
       rhs1_nbq            						&         ! (nmv_nbq)                                    &
      ,rhs2_nbq            						&         ! (nmv_nbq)                                    &
      ,rhsd1_nbq           						&         ! (nmv_nbq)                                    &    ! Pour Diffusion(s) "Boucle NBQ"
      ,rhsd3_nbq           						&         ! (nmq_nbq)                                    &    ! Pour Diffusion(s) "Boucle NBQ"
      ,rhsd2_nbq                   		   		        &  	  ! (nmv_nbq)  ! Pour Diffusion(s) "Boucle NBQ"
      ,visc2_nbq_a                 					&         ! (nmq_nbq) 
      ,visc2ref_nbq_a

!.....Couplage:
      double precision,dimension(:,:),allocatable  :: 			 &
       qdmsum_nbq_a      						 &         ! (nmv_nbq,0:2)                                &
      ,qdmsum2_nbq_a     						           ! (nmv_nbq,0:2)

!.....Variables mode EXT:  
      double precision,dimension(:,:,:),allocatable   ::                            &
       fluxbar_nbq_u      &         ! (GLOBAL_2D_ARRAY,0:2)                       &
      ,fluxbar_nbq_v       &         ! (GLOBAL_2D_ARRAY,0:2)                      &
      ,dqdmdtext_nbq_u       &         ! (GLOBAL_2D_ARRAY,1)                        &
      ,dqdmdtext_nbq_v       &         ! (GLOBAL_2D_ARRAY,1)                        &
      ,qdmbar_nbq_u        &         ! (GLOBAL_2D_ARRAY,0:2)                      &
      ,qdmbar_nbq_v        &         ! (GLOBAL_2D_ARRAY,0:2)                      &
      ,rhpbar_nbq_t        &         ! (GLOBAL_2D_ARRAY,0:2)                      &
      ,rhsext_nbq_u        &         ! (GLOBAL_2D_ARRAY,1:2)                      &
      ,rhsext_nbq_v        &         ! (GLOBAL_2D_ARRAY,1:2)                      &
      ,qdmbarobc_u         &         ! (GLOBAL_2D_ARRAY,0:2)                      &
      ,qdmbarobc_v                  ! (GLOBAL_2D_ARRAY,0:2)                       

      double precision,dimension(:,:),allocatable   ::                              &
!      qdmavr_nbq_u        &         ! (GLOBAL_2D_ARRAY)                          &
!     ,qdmavr_nbq_v        &         ! (GLOBAL_2D_ARRAY)                          &
       rhsavr_nbq_u        &         ! (GLOBAL_2D_ARRAY)                          &
      ,rhsavr_nbq_v                 ! (GLOBAL_2D_ARRAY)	       	      

      double precision,dimension(:),allocatable   :: 				    &
      !qdmref_nbq_a       							    &         ! (0:nmv_nbq)                                    &
       rhpref_nbq_a       							    &         ! (0:nmv_nbq)                                    &
      ,velfrs_nbq_a      							    &         ! (0:nmv_nbq)                                    &
      ,spgfrs_nbq_a                ! (0:nmv_nbq)

!.....Variables mode INT:
      double precision,dimension(:,:,:,:),allocatable   ::                         &
       rhpts_nbq_t        							    &         ! (GLOBAL_2D_ARRAY,0:kmax+1,0:2)             &
      ,dqdmdtint_nbq_u          							    &         ! (GLOBAL_2D_ARRAY,0:kmax+1,1)               &
      ,dqdmdtint_nbq_v          							    &         ! (GLOBAL_2D_ARRAY,0:kmax+1,1)               &
      ,dqdmdtint_nbq_w          							    &         ! (GLOBAL_2D_ARRAY,0:kmax+1,1)               &
      ,qdm_u               							    &         ! (GLOBAL_2D_ARRAY,0:kmax+1,-1:2)               &
      ,qdm_v               							    &         ! (GLOBAL_2D_ARRAY,0:kmax+1,-1:2)               &
      ,qdm_w               							    &         ! (GLOBAL_2D_ARRAY,0:kmax+1,-1:2)               &
      ,rhpio_nbq_t         							    &         ! (-1:imax+2,-1:jmax+2,0:kmax+1,-1:2)          &
      ,rhpio_nbq_u         							    &         ! (GLOBAL_2D_ARRAY,0:kmax+1,-1:2)            &
      ,rhpio_nbq_v         							    &         ! (GLOBAL_2D_ARRAY,0:kmax+1,-1:2)            &
      ,rhpio_nbq_w         							    &         ! (GLOBAL_2D_ARRAY,0:kmax+1,-1:2)            &
      ,veldydz_nbq_u       							    &         ! (0:imax+2,0:jmax+1,0:kmax+1,1)               &
      ,veldxdz_nbq_v       							    &         ! (0:imax+1,0:jmax+2,0:kmax+1,1)               &
      ,veldxdz_nbq_w       							    &         ! (GLOBAL_2D_ARRAY,0:kmax+1,1)               & 
      ,veldydz_nbq_w       							    &         ! (GLOBAL_2D_ARRAY,0:kmax+1,1)               &
      ,rhsint_nbq_u        							    &         ! (0:imax+1,0:jmax+2,0:kmax+1,1)             &
      ,rhsint_nbq_v        							    &         ! (0:imax+1,0:jmax+2,0:kmax+1,1)             &
      ,rhsint_nbq_w      							              ! (0:imax+1,0:jmax+2,0:kmax+1,1)    

      double precision,dimension(:,:),allocatable   ::                             &
       qdmu_eo_nbq                						    &                 
      ,qdmv_eo_nbq                						    &          
      ,qdmu_ns_nbq                						    &                 
      ,qdmv_ns_nbq                				!		    &

!.....Pour echanges "parallele":
      double precision ,dimension(:,:,:),allocatable    ::                           &
      qdm_nbq_u     							    	    &         ! (GLOBAL_2D_ARRAY,0:kmax+1)                      &     
     ,qdm_nbq_v     							    	    &         ! (GLOBAL_2D_ARRAY,0:kmax+1)                      &     
     ,qdm_nbq_w     							    	    &         ! (GLOBAL_2D_ARRAY,0:kmax+1)                      &      
     ,rhp_nbq_t     							    	    &         ! (GLOBAL_2D_ARRAY,0:kmax+1)                      &  
     ,rhpsum_nbq_t  					!		    	    &         ! (GLOBAL_2D_ARRAY,0:kmax+1)                      &


!.....Pour partie implicite:
      integer::   &
      nzmimp_nbq  &
     ,nzcimp_nbq  &
     ,l1imp_nbq   &
     ,neqmimp_nbq &
     ,neqcimp_nbq &
     ,nnz_nbq     &
     ,nindkun_nbq &
     ,ifl_imp_nbq

      double precision ::   &
      coefimp_nbq           &
     ,tsratio_nbq

      double precision,dimension(:), allocatable :: &
      cimpv_nbq   &
     ,mimpv_nbq   &
     ,rhsimp_nbq  &
     ,rhsimp2_nbq &
     ,wimp_nbq    &
     ,qdmimp_nbq  &
     ,pdv_nbq     &
     ,puv_nbq     &
     ,plv_nbq     &
     ,puvsum_nbq  &
     ,plvsum_nbq  &
     ,pdvsum_nbq  &
     ,rhssum_nbq

      integer,dimension(:), allocatable :: &
      cimpi_nbq      &
     ,cimpj_nbq      &
     ,mimpi_nbq      &
     ,mimpj_nbq      &
     ,pimpi_nbq      &
     ,pimpj_nbq      &
     ,iw1_nbq        &
     ,iw2_nbq        &
     ,indkun_nbq

      integer ::     &
      ptri_nbq

! Pour convection:
      double precision,dimension(:,:,:), allocatable :: &
      f1conv_nbq &
     ,f2conv_nbq


! Pour les echanges :: qdm et mv 
      integer,dimension(18) :: tabreq_qdm,tabreq_mv,tabreq_rhs, tabreq
      integer,dimension(:,:), allocatable :: tstatus
      double precision,dimension(:), allocatable :: ftrOUEST, ftrEST
      double precision,dimension(:), allocatable :: ftrNORD, ftrSUD
      double precision,dimension(:), allocatable :: ftrOUEST_qdm, ftrEST_qdm
      double precision,dimension(:), allocatable :: ftrNORD_qdm, ftrSUD_qdm
      double precision,dimension(:), allocatable :: ftrOUEST_mv, ftrEST_mv
      double precision,dimension(:), allocatable :: ftrNORD_mv, ftrSUD_mv
      double precision,dimension(:), allocatable :: ftrSUDOUEST, ftrNORDEST
      double precision,dimension(:), allocatable :: ftrNORDOUEST, ftrSUDEST
      double precision,dimension(:), allocatable :: ftrSUDOUEST_qdm, ftrNORDEST_qdm
      double precision,dimension(:), allocatable :: ftrNORDOUEST_qdm, ftrSUDEST_qdm
      double precision,dimension(:), allocatable :: ftrSUDOUEST_mv, ftrNORDEST_mv
      double precision,dimension(:), allocatable :: ftrNORDOUEST_mv, ftrSUDEST_mv

      double precision,dimension(:), allocatable :: ftrOUESTout_qdm, ftrESTout_qdm
      double precision,dimension(:), allocatable :: ftrNORDout_qdm, ftrSUDout_qdm
      double precision,dimension(:), allocatable :: ftrSUDOUESTout_qdm, ftrNORDESTout_qdm
      double precision,dimension(:) ,allocatable :: ftrNORDOUESTout_qdm, ftrSUDESTout_qdm

      double precision,dimension(:), allocatable :: ftrOUESTout_mv, ftrESTout_mv
      double precision,dimension(:), allocatable :: ftrNORDout_mv, ftrSUDout_mv
      double precision,dimension(:), allocatable :: ftrSUDOUESTout_mv, ftrNORDESTout_mv
      double precision,dimension(:) ,allocatable :: ftrNORDOUESTout_mv, ftrSUDESTout_mv

      double precision,dimension(:), allocatable :: ftrOUESTout_rhs, ftrESTout_rhs
      double precision,dimension(:), allocatable :: ftrNORDout_rhs, ftrSUDout_rhs
      double precision,dimension(:), allocatable :: ftrSUDOUESTout_rhs, ftrNORDESTout_rhs
      double precision,dimension(:) ,allocatable :: ftrNORDOUESTout_rhs, ftrSUDESTout_rhs

      integer :: nbsendOUEST, idiOUEST=1
      integer :: nbsendEST, idiEST=1
      integer :: nbsendNORD, idiNORD=1
      integer :: nbsendSUD, idiSUD=1
      integer :: nbsendOUEST_qdm, idiOUEST_qdm=1
      integer :: nbsendEST_qdm, idiEST_qdm=1
      integer :: nbsendNORD_qdm, idiNORD_qdm=1
      integer :: nbsendSUD_qdm, idiSUD_qdm=1
      integer :: nbsendOUEST_mv, idiOUEST_mv=1
      integer :: nbsendEST_mv, idiEST_mv=1
      integer :: nbsendNORD_mv, idiNORD_mv=1
      integer :: nbsendSUD_mv, idiSUD_mv=1

      integer :: nbsendSUDOUEST, idiSUDOUEST=1
      integer :: nbsendSUDEST, idiSUDEST=1
      integer :: nbsendNORDEST, idiNORDEST=1
      integer :: nbsendNORDOUEST, idiNORDOUEST=1

      integer :: nbsendSUDOUEST_qdm, idiSUDOUEST_qdm=1
      integer :: nbsendSUDEST_qdm, idiSUDEST_qdm=1
      integer :: nbsendNORDEST_qdm, idiNORDEST_qdm=1
      integer :: nbsendNORDOUEST_qdm, idiNORDOUEST_qdm=1

      integer :: nbsendSUDOUEST_mv, idiSUDOUEST_mv=1
      integer :: nbsendSUDEST_mv, idiSUDEST_mv=1
      integer :: nbsendNORDEST_mv, idiNORDEST_mv=1
      integer :: nbsendNORDOUEST_mv, idiNORDOUEST_mv=1


      integer :: nbreq=1,nbreq_qdm=1,nbreq_mv=1,nbreq_rhs=1
      integer,parameter :: TAGOUEST_qdm=55000, TAGEST_qdm=55010, TAGSUD_qdm=56000, TAGNORD_qdm=56010
      integer,parameter :: TAGSUDOUEST_qdm=155000, TAGSUDEST_qdm=515010, TAGNORDOUEST_qdm=516000, TAGNORDEST_qdm=516010
      integer,parameter :: TAGOUEST_mv=75000, TAGEST_mv=75010, TAGSUD_mv=76000, TAGNORD_mv=76010
      integer,parameter :: TAGSUDOUEST_mv=715000, TAGSUDEST_mv=715010, TAGNORDOUEST_mv=716000, TAGNORDEST_mv=716010
      integer,parameter :: TAGOUEST_rhs=85000, TAGEST_rhs=85010, TAGSUD_rhs=86000, TAGNORD_rhs=86010
      integer,parameter :: TAGSUDOUEST_rhs=815000, TAGSUDEST_rhs=815010, TAGNORDOUEST_rhs=816000, TAGNORDEST_rhs=816010
! fin module ne pas toucher à cette ligne                        
     
      
      contains   !--------------------------------------------------------------------
      !--------------------------------------------------------------------
      subroutine alloc_module_nbq(MPI_STATUS_SIZE)
      implicit none
# include "param_F90.h"
      integer,intent(in) :: MPI_STATUS_SIZE
      integer :: imax,jmax,kmax

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

      !
      nmq_nbq    = (imax+2)*(jmax+2)*(kmax+1)
      nmv_nbq    = (imax+2)*(jmax+2)*(kmax+1)*4
      nmprod_nbq = (imax+2)*(jmax+2)*(kmax+1)*10
      nmcont_nbq = nmq_nbq*20
      nmmom_nbq  = nmv_nbq*20

!.....Variables 0D:
       allocate(  soundspeed2v_nbq   (GLOBAL_2D_ARRAY)                         )
       allocate(  soundspeed2q_nbq   (0:nmq_nbq)                                 )
       allocate(  soundspeed2mom_nbq (0:nmv_nbq)                                 )
 !....Variables algébriques mode NBQ:
       allocate(  dqdmdt_nbq_a      (0:nmv_nbq)                                    )
       allocate(  flux_ass_nbq_a    (0:nmv_nbq,2)                                  )    ! Pour filtre d Asselin "Boucle NBQ"
       allocate(  qdm_nbq_a         (0:nmv_nbq,-2:2)                             )
       allocate(  rhp_nbq_a         (0:nmq_nbq,-1:2)                              )
       allocate(  qdm_nbq_a_bak     (0:nmv_nbq,-2:2)                             )
       allocate(  rhp_nbq_a_bak     (0:nmq_nbq,-1:2)                              )
       allocate(  rhpsum_nbq_a      (0:nmq_nbq,1:2)                              )   
       allocate(  rhpsum2_nbq_a     (0:nmq_nbq,1:2)                              )   
       allocate(  diffsum_nbq_a     (0:nmv_nbq,0:2)                              )
       allocate(  diffsum2_nbq_a    (0:nmv_nbq,0:2)                              )
       allocate(  rhssum_nbq_a      (0:nmv_nbq,2:2)                                  )
       allocate(  div_nbq_a         (0:nmq_nbq,0:1) 				 )                 
!....Tableaux de travail NBQ:
       allocate(  rhs1_nbq          (0:nmv_nbq)                                    )
       allocate(  rhs2_nbq          (0:nmv_nbq)                                    )
       allocate(  rhsd1_nbq         (0:nmv_nbq)                                    )    ! Pour Diffusion(s) "Boucle NBQ"
       allocate(  rhsd2_nbq         (0:nmv_nbq)                                    )    ! Pour Diffusion(s) "Boucle NBQ"
       allocate(  rhsd3_nbq         (0:nmq_nbq)                                    )    ! Pour Diffusion(s) "Boucle NBQ"
       allocate(  visc2_nbq_a       (0:nmv_nbq)					 )  
       allocate(  visc2ref_nbq_a    (0:nmv_nbq)					 )  

!.....Couplage:
       allocate(  qdmsum_nbq_a      (0:nmv_nbq,0:2)                                )
       allocate(  qdmsum2_nbq_a     (0:nmv_nbq,0:2)				 )
!.....Variables mode EXT:
       allocate(  fluxbar_nbq_u     (GLOBAL_2D_ARRAY,0:2)                      )
       allocate(  fluxbar_nbq_v     (GLOBAL_2D_ARRAY,0:2)                      )
       allocate(  dqdmdtext_nbq_u     (GLOBAL_2D_ARRAY,1)                        )
       allocate(  dqdmdtext_nbq_v     (GLOBAL_2D_ARRAY,1)                        )
       allocate(  qdmbar_nbq_u      (GLOBAL_2D_ARRAY,0:2)                      )
       allocate(  qdmbar_nbq_v      (GLOBAL_2D_ARRAY,0:2)                      )
       allocate(  rhsavr_nbq_u      (GLOBAL_2D_ARRAY)                          )
       allocate(  rhsavr_nbq_v      (GLOBAL_2D_ARRAY)                          )
       allocate(  rhpbar_nbq_t      (GLOBAL_2D_ARRAY,0:2)                      )
       allocate(  rhsext_nbq_u      (GLOBAL_2D_ARRAY,1:2)                      )
       allocate(  rhsext_nbq_v      (GLOBAL_2D_ARRAY,1:2)                      )
!      allocate(  qdmavr_nbq_u      (GLOBAL_2D_ARRAY)                          )

       allocate(  qdmu_eo_nbq         (GLOBAL_1D_ARRAYETA,1:2)                        )
       allocate(  qdmv_eo_nbq         (GLOBAL_1D_ARRAYETA,1:2)                        )           			        
       allocate(  qdmu_ns_nbq         (GLOBAL_1D_ARRAYXI,1:2)                        )         			              
       allocate(  qdmv_ns_nbq         (GLOBAL_1D_ARRAYXI,1:2)                        )   
       			          
!      allocate(  qdmavr_nbq_v      (GLOBAL_2D_ARRAY)                          )
       allocate(  qdmbarobc_u       (GLOBAL_2D_ARRAY,0:2)                      )
       allocate(  qdmbarobc_v       (GLOBAL_2D_ARRAY,0:2)                      )
      !allocate(  qdmref_nbq_a      (0:nmv_nbq)                                  )
       allocate(  rhpref_nbq_a      (0:nmv_nbq)                                  )
       allocate(  velfrs_nbq_a      (0:nmv_nbq)                                  )
       allocate(  spgfrs_nbq_a      (0:nmv_nbq)					 )
!.....Variables mode INT:
       allocate(  rhpts_nbq_t       (GLOBAL_2D_ARRAY,0:kmax+1,0:2)             )
       allocate(  dqdmdtint_nbq_u   (GLOBAL_2D_ARRAY,0:kmax+1,1)               )
       allocate(  dqdmdtint_nbq_v   (GLOBAL_2D_ARRAY,0:kmax+1,1)               )
       allocate(  dqdmdtint_nbq_w   (GLOBAL_2D_ARRAY,-1:kmax+1,1)               )
       allocate(  qdm_u             (GLOBAL_2D_ARRAY,0:kmax+1,0:2)            )
       allocate(  qdm_v             (GLOBAL_2D_ARRAY,0:kmax+1,0:2)            )
       allocate(  qdm_w             (GLOBAL_2D_ARRAY,-1:kmax+1,0:2)            )
#ifdef MPI
       allocate(  rhpio_nbq_t       (GLOBAL_2D_ARRAY,-1:kmax+1,0:2)          ) 
#else 
       allocate(  rhpio_nbq_t       (GLOBAL_2D_ARRAY_EXT_NBQ,-1:kmax+1,0:2)          )
#endif
       allocate(  rhpio_nbq_u       (GLOBAL_2D_ARRAY,0:kmax+1,0:2)            )
       allocate(  rhpio_nbq_v       (GLOBAL_2D_ARRAY,0:kmax+1,0:2)            )
       allocate(  rhpio_nbq_w       (GLOBAL_2D_ARRAY,-1:kmax+1,0:2)            )
#ifdef MPI
       allocate(  veldydz_nbq_u     (GLOBAL_2D_ARRAY,0:kmax+1,1)               )
       allocate(  veldxdz_nbq_v     (GLOBAL_2D_ARRAY,0:kmax+1,1)               )
#else
       allocate(  veldydz_nbq_u     (GLOBAL_2D_ARRAY_EXT_NBQ,0:kmax+1,1)               )
       allocate(  veldxdz_nbq_v     (GLOBAL_2D_ARRAY_EXT_NBQ,0:kmax+1,1)               )
#endif

       allocate(  veldxdz_nbq_w     (GLOBAL_2D_ARRAY,-1:kmax+1,1)               ) 
       allocate(  veldydz_nbq_w     (GLOBAL_2D_ARRAY,-1:kmax+1,1)               )
       allocate(  rhsint_nbq_u      (GLOBAL_2D_ARRAY,0:kmax+1,1:2)             )
       allocate(  rhsint_nbq_v      (GLOBAL_2D_ARRAY,0:kmax+1,1:2)             )
       allocate(  rhsint_nbq_w      (GLOBAL_2D_ARRAY,-1:kmax+1,1:2)	 	 )    
!.....Pour echanges "parallele":
       allocate(  qdm_nbq_u     (GLOBAL_2D_ARRAY,0:kmax+1)                      )     
       allocate(  qdm_nbq_v     (GLOBAL_2D_ARRAY,0:kmax+1)                      )     
       allocate(  qdm_nbq_w     (GLOBAL_2D_ARRAY,-1:kmax+1)                      )      
       allocate(  rhp_nbq_t     (GLOBAL_2D_ARRAY,0:kmax+1)                      )  
       allocate(  rhpsum_nbq_t  (GLOBAL_2D_ARRAY,0:kmax+1)                      )

!......Pour partie implicit:
!      allocate(rhssave_nbq      (nmv_nbq,0:2)                                )   ! 
       allocate(rhssum_nbq       (nmv_nbq)                                )   ! 
       allocate(rhsimp_nbq       (nmv_nbq)                                )   ! 
       allocate(rhsimp2_nbq      (nmv_nbq)                                )   ! 
       allocate(qdmimp_nbq       (nmv_nbq)                                )   ! 
       allocate(wimp_nbq         (nmv_nbq)                                )   ! 
       allocate(cimpv_nbq        (0:nmcont_nbq)                           )   ! 
       allocate(mimpv_nbq        (nmmom_nbq)                              )   ! 
!      allocate(pimpv_nbq        (nmprod_nbq)                             )   ! 
       allocate(cimpi_nbq        (nmq_nbq)                                )   !  ! norestart
       allocate(cimpj_nbq        (nmcont_nbq)                             )   !  ! norestart    
       allocate(mimpi_nbq        (nmv_nbq)                                )   ! 
       allocate(mimpj_nbq        (nmmom_nbq)                              )   !  ! norestart
       allocate(pimpi_nbq        (nmv_nbq)                                )   ! 
       allocate(pimpj_nbq        (nmprod_nbq)                             )   !  ! norestart
       allocate(iw1_nbq          (nmv_nbq)                                )   ! 
       allocate(iw2_nbq          (nmv_nbq)                                )   ! 
       allocate(indkun_nbq       (nmv_nbq)                                )   ! 

       allocate(pdv_nbq          (nmv_nbq) )
       allocate(puv_nbq          (nmv_nbq) )
       allocate(plv_nbq          (nmv_nbq) )

       allocate(pdvsum_nbq       (nmv_nbq) )
       allocate(puvsum_nbq       (nmv_nbq) )
       allocate(plvsum_nbq       (nmv_nbq) )

       allocate(f1conv_nbq(GLOBAL_2D_ARRAY,1))
       allocate(f2conv_nbq(GLOBAL_2D_ARRAY,1))


       ! Pour les echanges :: qdm et mv et rhs
      allocate( ftrNORD(        		(jmax+2)*(kmax+2)*4)	)
      allocate( ftrOUEST_qdm(   		(jmax+2)*(kmax+2)*4)	)
      allocate( ftrEST_qdm(     		(jmax+2)*(kmax+2)*4)	)
      allocate( ftrOUEST_mv(    		(jmax+2)*(kmax+2)*4)	)
      allocate( ftrEST_mv(      		(jmax+2)*(kmax+2)*4)	)
      allocate( ftrSUDOUEST(    		(jmax+2)*(kmax+2)*4)	)
      allocate( ftrNORDEST(     		(jmax+2)*(kmax+2)*4)	)
      allocate( ftrSUDOUEST_qdm(		(jmax+2)*(kmax+2)*4)	)
      allocate( ftrNORDEST_qdm(			(jmax+2)*(kmax+2)*4)	)
      allocate( ftrSUDOUEST_mv(			(jmax+2)*(kmax+2)*4)	)
      allocate( ftrNORDEST_mv(			(jmax+2)*(kmax+2)*4)	)
      allocate( ftrSUD(				(imax+2)*(kmax+2)*4)	)
      allocate( ftrNORD_qdm(			(imax+2)*(kmax+2)*4)	)
      allocate( ftrSUD_qdm(			(imax+2)*(kmax+2)*4)	)
      allocate( ftrNORD_mv(			(imax+2)*(kmax+2)*4)	)
      allocate( ftrSUD_mv(			(imax+2)*(kmax+2)*4)	)
     allocate( ftrNORDOUEST(			(imax+2)*(kmax+2)*4)	)
      allocate( ftrSUDEST(			(imax+2)*(kmax+2)*4)	)
      allocate( ftrNORDOUEST_qdm(		(imax+2)*(kmax+2)*4)	)
      allocate( ftrSUDEST_qdm(			(imax+2)*(kmax+2)*4)	)
      allocate( ftrNORDOUEST_mv(		(imax+2)*(kmax+2)*4)	)
      allocate( ftrSUDEST_mv(			(imax+2)*(kmax+2)*4)	)

      allocate( ftrOUESTout_qdm(    		(jmax+2)*(kmax+2)*4)	)
      allocate( ftrESTout_qdm(      		(jmax+2)*(kmax+2)*4)	)
      allocate( ftrNORDout_qdm(			(imax+2)*(kmax+2)*4)	)
      allocate( ftrSUDout_qdm(			(imax+2)*(kmax+2)*4)	)
      allocate( ftrSUDOUESTout_qdm(			(jmax+2)*(kmax+2)*4)	)
      allocate( ftrNORDESTout_qdm(			(jmax+2)*(kmax+2)*4)	)!!!!!!!!!!!!!!!!!!
      allocate( ftrNORDOUESTout_qdm(		(imax+2)*(kmax+2)*4)	)
      allocate( ftrSUDESTout_qdm(			(imax+2)*(kmax+2)*4)	)

      allocate( ftrOUESTout_mv(    		(jmax+2)*(kmax+2)*4)	)
      allocate( ftrESTout_mv(      		(jmax+2)*(kmax+2)*4)	)
      allocate( ftrNORDout_mv(			(imax+2)*(kmax+2)*4)	)
      allocate( ftrSUDout_mv(			(imax+2)*(kmax+2)*4)	)
      allocate( ftrSUDOUESTout_mv(			(jmax+2)*(kmax+2)*4)	)
      allocate( ftrNORDESTout_mv(			(jmax+2)*(kmax+2)*4)	)!!!!!!!!!!!!!!!!!!
      allocate( ftrNORDOUESTout_mv(		(imax+2)*(kmax+2)*4)	)
      allocate( ftrSUDESTout_mv(			(imax+2)*(kmax+2)*4)	)

      allocate( ftrOUESTout_rhs(    		(jmax+2)*(kmax+2)*4)	)
      allocate( ftrESTout_rhs(      		(jmax+2)*(kmax+2)*4)	)
      allocate( ftrNORDout_rhs(			(imax+2)*(kmax+2)*4)	)
      allocate( ftrSUDout_rhs(			(imax+2)*(kmax+2)*4)	)
      allocate( ftrSUDOUESTout_rhs(			(jmax+2)*(kmax+2)*4)	)
      allocate( ftrNORDESTout_rhs(			(jmax+2)*(kmax+2)*4)	)!!!!!!!!!!!!!!!!!!
      allocate( ftrNORDOUESTout_rhs(		(imax+2)*(kmax+2)*4)	)
      allocate( ftrSUDESTout_rhs(			(imax+2)*(kmax+2)*4)	)

      allocate( tstatus(MPI_STATUS_SIZE,18)	)
      
      end subroutine alloc_module_nbq

      end module module_nbq

#else
      module module_nbq_empty
      end module
#endif      
