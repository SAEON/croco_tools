#include "cppdefs.h"
#if defined NBQ && defined MPI
!------------------------------------------------------------------------------
!                               NHOMS
!                Non Hydrostatic Ocean Modeling System      
!------------------------------------------------------------------------------
!
!> @note <a href="http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm"> Main web documentation </a>
!
! DESCRIPTION: 
!
!> @brief
!
!> @details 
!
!
! REVISION HISTORY:
!
!> @authors
!> @date 2015 January
!> @todo
!
!------------------------------------------------------------------------------

      subroutine parallele_nbq(ichoix)

!__________________________________________________________________________________
!
!                               SNH2012.14      
!                 Non-Hydrostatic & Non-Boussinesq Version  
! Laboratoire d'Aerologie, 14 Avenue Edouard Belin, F-31400 Toulouse
! http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm  
!
!__________________________________________________________________________________


!      use module_parameter
!      use module_parameter_nh
!      use module_parallele
!      use module_principal , only :  mask_u, mask_v, mask_t, imax, jmax, kmax &
!			  , iteration3d, iteration2d
      use module_nh , only : ijk2lmom_nh, time_omp_nh, ijk2lq_nh, rhs2_nh
      use module_nbq
      use module_parallel_nbq
!      use module_profile
      implicit none 
      integer,intent(in) ::ichoix
      integer,dimension(8),parameter ::  lvoisin  =    (/ ouest,   est,   nord,  sud,        sudouest, sudest, nordouest, nordest /)
      integer,dimension(10),parameter :: tagqdm_Send = (/ 55000, 55010, 56010, 56000, 0, 0,  155000, 515010,    516000, 516010  /)
      integer,dimension(10),parameter :: tagqdm_Recv = (/ 55010, 55000, 56000, 56010, 0, 0,  516010, 516000,    515010, 155000  /)
      integer,dimension(10),parameter :: tagdiv_Send = (/ 75000, 75010, 76010, 76000, 0, 0,  255000, 715010,    716000, 716010  /)
      integer,dimension(10),parameter :: tagdiv_Recv = (/ 75010, 75000, 76000, 76010, 0, 0,  716010, 716000,    715010, 255000  /)
      integer,dimension(10),parameter :: tagrhs_Send = (/ 85000, 85010, 86010, 86000, 0, 0,  355000, 815010,    816000, 816010  /)
      integer,dimension(10),parameter :: tagrhs_Recv = (/ 85010, 85000, 86000, 86010, 0, 0,  816010, 816000,    815010, 355000  /)
      integer :: i, j, k, bcl
      double precision :: tmptime1,tmptime2
      logical,save :: firstpass=.true.
      integer :: vois
      integer(MPI_ADDRESS_KIND) :: addr1
!       double precision,dimension(0:size(qdm_nbq_a(:,2))) :: qdm_tmp1,qdm_tmp2
      integer :: sizeqdm, szsend, szrecv, debsend,debrecv
      integer :: kmax,imax,jmax
#define LOCALLM Lmmpi
#define LOCALMM Mmmpi
       
      imax=LOCALLM+1
      jmax=LOCALMM+1
     
!       print *,"iteration_nbq=",iteration_nbq,"ichoix=",ichoix
!       print *,"size(qdm_nbq_a(:,2))=",size(qdm_nbq_a(:,2))
!       if(iteration_nbq == 2) then
!        call mpi_finalize(ierr)
!        stop !'fin test...'
!       endif
!       sizeqdm=size(qdm_nbq_a(:,2))
! !       qdm_nbq_a(:,2)  = par%rank
!       qdm_nbq_a(:,1)  = par%rank+10
!       qdm_nbq_a(:,0)  = par%rank+20
!       qdm_nbq_a(:,-1) = par%rank+30

! Moce in  MPI_nbq_Setup routine, called in main.F
!       if (firstpass) then
! !****************************************************************************************
! ! Preparation des nouvraux type d'echange
! !****************************************************************************************
! 	call create_echange_qdm_nbq_a(imax,jmax,kmax)
! 	call create_echange_div_nbq_a(imax,jmax,kmax)
! 	call create_echange_rhs2_nh(imax,jmax,kmax)
! 	!call create_echange_rhs2_nh(imax,jmax,kmax)
! 	
! 	firstpass=.false.
! ! 	write(200+par%rank,*) ech_qdm_nbq(:)%send
! ! 	write(200+par%rank,*) ech_qdm_nbq(:)%recv
! !       else
! 
!          !call mpi_barrier(par%comm2d,ierr)
!       endif

!       qdm_tmp1 = par%rank*10
!       qdm_tmp2 = par%rank*10

      if (ichoix.eq.5) then
!****************************************************************************************
! Echanges des variables "boucle NBQ" qunatité de mouvement :: ENVOI
!****************************************************************************************
	  nbreq_qdm=1
	  do bcl=1,8
	   vois=lvoisin(bcl)
	   if (par%tvoisin(vois) /= mpi_proc_null) then 
! 	   write(200+par%rank,*) "envoie/recoit : ",par%tvoisin(vois)," --- ",vois
! 	   write(200+par%rank,*) " recv,send=",ech_qdm_nbq(vois)%recv,ech_qdm_nbq(vois)%send
! 	   write(200+par%rank,*) " recv a :",par%tvoisin(vois)," tag=",tagqdm_Recv(vois)
! 	   write(200+par%rank,*) " send a :",par%tvoisin(vois)," tag=",tagqdm_Send(vois)
	   
	   call MPI_TYPE_SIZE(ech_qdm_nbq(vois)%send, szsend,ierr)
	   call MPI_TYPE_SIZE(ech_qdm_nbq(vois)%recv, szrecv,ierr) 
!            call MPI_TYPE_GET_EXTENT(ech_qdm_nbq(vois)%send, debsend, szsend, ierr)
!            call MPI_TYPE_GET_EXTENT(ech_qdm_nbq(vois)%recv, debrecv, szrecv, ierr)
            call MPI_IRECV(qdm_nbq_a(1,2),  1, ech_qdm_nbq(vois)%recv, par%tvoisin(vois), &
	      tagqdm_Recv(vois), par%comm2d, tabreq_qdm(nbreq_qdm), ierr)
 	     nbreq_qdm=nbreq_qdm+1
 	     call MPI_IRSEND(qdm_nbq_a(1,2),  1, ech_qdm_nbq(vois)%send, par%tvoisin(vois), &
               tagqdm_Send(vois), par%comm2d, tabreq_qdm(nbreq_qdm), ierr)
  	     nbreq_qdm=nbreq_qdm+1	
!  	      write(200+par%rank,*) "send num=",ech_qdm_nbq(:)%send
!  	      write(200+par%rank,*) "recv num=",ech_qdm_nbq(:)%recv
!  	     write(200+par%rank,*) "envoie/recoit ok : ",bcl,ierr
!  	     write(200+par%rank,*) "Envoie taille=",szsend,debsend
!  	     write(200+par%rank,*) "Recv   taille=",szrecv,debrecv
           endif
	  enddo
      endif
      if (ichoix.eq.15) then
!****************************************************************************************
! Echanges des variables "boucle NBQ" qunatité de mouvement :: RECEPTION/WAIT
!****************************************************************************************
!----------------------------------------------------------------------------------------
!  Salle d'attente
!----------------------------------------------------------------------------------------
	nbreq_qdm = nbreq_qdm-1
	CALL MPI_WAITALL(nbreq_qdm, tabreq_qdm(1:nbreq_qdm), tstatus(:,1:nbreq_qdm), IERR)   
        nbreq_qdm=0
!       do i=0,imax+1
!       do j=0,jmax+1
!       do k=1,kmax+1
!         l_nbq=ijk2lmom_nh(i,j,k,1)
!!         write(23000+par%rank,*) i,j,k,"=",qdm_nbq_a(l_nbq,2)
!       enddo
!       enddo
!       enddo
       endif

!****************************************************************************************
!****************************************************************************************
      if (ichoix.eq.7) then  
!****************************************************************************************
! Echanges des variables "boucle NBQ" divergence :: ENVOIE
!****************************************************************************************
	  nbreq_mv=1
	  do bcl=1,8
	   vois=lvoisin(bcl)
	   if (par%tvoisin(vois) /= mpi_proc_null) then 
! 	    print *,par%rank," vois=",vois," id=",par%tvoisin(vois)
! 	    write(1500+par%rank,*) " vois=",vois," id=",par%tvoisin(vois)
! 	    write(1500+par%rank,*) "ech_div_nbq(vois)%recv=",ech_div_nbq(vois)%recv
! 	    write(1500+par%rank,*) "tagdiv_Recv(vois)=",tagdiv_Recv(vois)
! 	    write(1500+par%rank,*) "tabreq_mv(nbreq_mv)=",tabreq_mv(nbreq_mv)
! 	    write(1500+par%rank,*)
! 	    write(500+par%rank,*)  "ech_div_nbq(vois)%send=",ech_div_nbq(vois)%send
! 	    write(500+par%rank,*)  "tagdiv_Send(vois)=",tagdiv_Send(vois)
! 	    write(500+par%rank,*)  "tabreq_mv(nbreq_mv)=",tabreq_mv(nbreq_mv)
! 	    write(500+par%rank,*)

            call MPI_IRECV(div_nbq_a(1,1),  1, ech_div_nbq(vois)%recv, par%tvoisin(vois), &
	      tagdiv_Recv(vois), par%comm2d, tabreq_mv(nbreq_mv), ierr)
 	     nbreq_mv=nbreq_mv+1
 	     call MPI_IRSEND(div_nbq_a(1,1),  1, ech_div_nbq(vois)%send, par%tvoisin(vois), &
               tagdiv_Send(vois), par%comm2d, tabreq_mv(nbreq_mv), ierr)
  	     nbreq_mv=nbreq_mv+1
! 	    write(500+par%rank,*)     ierr
! 	    write(1500+par%rank,*)    ierr
  	     
           endif  
	  enddo
      endif
      if (ichoix.eq.17) then
!****************************************************************************************
! Echanges des variables "boucle NBQ" divergence :: ENVOIE
!****************************************************************************************
!----------------------------------------------------------------------------------------
!  Salle d'attente
!----------------------------------------------------------------------------------------
	nbreq_mv = nbreq_mv-1
	CALL MPI_WAITALL(nbreq_mv, tabreq_mv(1:nbreq_mv), tstatus(:,1:nbreq_mv), IERR)   
        nbreq_mv=0
      endif
!****************************************************************************************
!****************************************************************************************

!****************************************************************************************
!****************************************************************************************
      if (ichoix.eq.8) then  
!****************************************************************************************
! !.....Echanges de RHS2_NH contenant la condition cinematique au fond:
!****************************************************************************************
	  nbreq_rhs=1
	  do bcl=1,8
	   vois=lvoisin(bcl)
	   if (par%tvoisin(vois) /= mpi_proc_null) then 
            call MPI_IRECV(rhs2_nh(1),  1, ech_rhs2_nh(vois)%recv, par%tvoisin(vois), &
	      tagrhs_Recv(vois), par%comm2d, tabreq_rhs(nbreq_rhs), ierr)
 	     nbreq_rhs=nbreq_rhs+1
 	     call MPI_IRSEND(rhs2_nh(1),  1, ech_rhs2_nh(vois)%send, par%tvoisin(vois), &
               tagrhs_Send(vois), par%comm2d, tabreq_rhs(nbreq_rhs), ierr)
  	     nbreq_rhs=nbreq_rhs+1	
           endif  
	  enddo
      endif
      if (ichoix.eq.18) then
!****************************************************************************************
! !.....Echanges de RHS2_NH contenant la condition cinematique au fond:
!****************************************************************************************
!----------------------------------------------------------------------------------------
!  Salle d'attente
!----------------------------------------------------------------------------------------
	nbreq_rhs = nbreq_rhs-1
	CALL MPI_WAITALL(nbreq_rhs, tabreq_rhs(1:nbreq_rhs), tstatus(:,1:nbreq_rhs), IERR)   
        nbreq_rhs=0
      endif
!****************************************************************************************
!****************************************************************************************



!****************************************************************************************
!****************************************************************************************
!****************************************************************************************
!****************************************************************************************
!****************************************************************************************
!****************************************************************************************
!****************************************************************************************
!****************************************************************************************
!
!                                  OLD
!
!****************************************************************************************
!****************************************************************************************
!****************************************************************************************

       
       
      if (ichoix.eq.557) then
!****************************************************************************************
! Echanges des variables "boucle NBQ" divergence :: ENVOIE
!****************************************************************************************

        nbreq_mv        = 1
        idiOUEST_mv     = 1
        idiEST_mv       = 1 
        idiSUD_mv       = 1
        idiNORD_mv      = 1 
        idiSUDOUEST_mv  = 1
        idiSUDEST_mv    = 1 
        idiNORDEST_mv   = 1
        idiNORDOUEST_mv = 1 

!----------------------------------------------------------------------------------------
!  OUEST
!----------------------------------------------------------------------------------------
        if (par%tvoisin(ouest) /= mpi_proc_null) then
           tmptime1 = mpi_wtime()
           i=2
           do k=1,kmax
           do j=1,jmax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                ftrOUESTout_mv(idiOUEST_mv) = div_nbq_a(l_nbq,1)  !* mask_t(i,j,k)
                idiOUEST_mv=idiOUEST_mv+1
              endif
           enddo
           enddo
           tmptime2 = mpi_wtime()
           time_omp_nh(20)=time_omp_nh(20)+(tmptime2-tmptime1)

           ! Envoie
           tmptime1 = mpi_wtime()
           nbsendOUEST_mv=idiOUEST_mv-1
           call MPI_IRSEND(ftrouestout_mv,  nbsendOUEST_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(ouest), &
              TAGOUEST_mv, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1
           call MPI_IRECV(ftrouest_mv,  nbsendOUEST_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(ouest), &
              TAGEST_mv, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1
           tmptime2 = mpi_wtime()
           time_omp_nh(21)=time_omp_nh(21)+(tmptime2-tmptime1)
        endif

!----------------------------------------------------------------------------------------
!  EST
!-----------------------------------------------------------------------      
        if (par%tvoisin(est) /= mpi_proc_null) then
           tmptime1 = mpi_wtime()
           i = imax-1
           do k=1,kmax
           do j=1,jmax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                ftrESTout_mv(idiEST_mv) = div_nbq_a(l_nbq,1)  !* mask_t(i,j,k)
                idiEST_mv=idiEST_mv+1
              endif
           enddo
           enddo
           tmptime2 = mpi_wtime()
           time_omp_nh(20)=time_omp_nh(20)+(tmptime2-tmptime1)

           ! Envoie
           tmptime1 = mpi_wtime()
           nbsendEST_mv=idiEST_mv-1
           call MPI_IRSEND(ftrestout_mv,  nbsendEST_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(est), &
              TAGEST_mv, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1
           call MPI_IRECV(ftrest_mv,  nbsendEST_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(est), &
              TAGOUEST_mv, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1
           tmptime2 = mpi_wtime()
           time_omp_nh(21)=time_omp_nh(21)+(tmptime2-tmptime1)
        endif

!----------------------------------------------------------------------------------------
!  SUD
!----------------------------------------------------------------------------------------
        if (par%tvoisin(sud) /= mpi_proc_null) then
           tmptime1 = mpi_wtime()
           j=2
           do k=1,kmax
           do i=1,imax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                ftrSUDout_mv(idiSUD_mv) = div_nbq_a(l_nbq,1)  !* mask_t(i,j,k)
                idiSUD_mv=idiSUD_mv+1
              endif
           enddo
           enddo
           tmptime2 = mpi_wtime()
           time_omp_nh(20)=time_omp_nh(20)+(tmptime2-tmptime1)

           ! Envoie
           tmptime1 = mpi_wtime()
           nbsendSUD_mv=idiSUD_mv-1
           call MPI_IRSEND(ftrsudout_mv,  nbsendSUD_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(sud), &
              TAGSUD_mv, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1
           call MPI_IRECV(ftrsud_mv,  nbsendSUD_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(sud), &
              TAGNORD_mv, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1
           tmptime2 = mpi_wtime()
           time_omp_nh(21)=time_omp_nh(21)+(tmptime2-tmptime1)
        endif

!----------------------------------------------------------------------------------------
!  NORD
!----------------------------------------------------------------------------------------
        if (par%tvoisin(nord) /= mpi_proc_null) then
           tmptime1 = mpi_wtime()
           j = jmax-1
           do k=1,kmax
           do i=1,imax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                ftrNORDout_mv(idiNORD_mv) = div_nbq_a(l_nbq,1)  !* mask_t(i,j,k)
!               ftrNORDout(idiNORD) = 3
                idiNORD_mv=idiNORD_mv+1
              endif
           enddo
           enddo
           tmptime2 = mpi_wtime()
           time_omp_nh(20)=time_omp_nh(20)+(tmptime2-tmptime1)

           ! Envoie
           tmptime1 = mpi_wtime()
           nbsendNORD_mv=idiNORD_mv-1
           call MPI_IRSEND(ftrnordout_mv,  nbsendNORD_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(nord), &
              TAGNORD_mv, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1
           call MPI_IRECV(ftrnord_mv,  nbsendNORD_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(nord), &
              TAGSUD_mv, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1
           tmptime2 = mpi_wtime()
           time_omp_nh(21)=time_omp_nh(21)+(tmptime2-tmptime1)
        endif

!----------------------------------------------------------------------------------------
!  SUD-OUEST
!----------------------------------------------------------------------------------------
     !  if (par%tvoisin(sudouest) /= mpi_proc_null) then
        if ( (par%tvoisin(sud) /= mpi_proc_null).and. (par%tvoisin(ouest) /= mpi_proc_null)) then
           i=2
           j=2
           do k=1,kmax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                ftrSUDOUESTout_mv(idiSUDOUEST_mv) = div_nbq_a(l_nbq,1)  !* mask_t(i,j,k)
                idiSUDOUEST_mv=idiSUDOUEST_mv+1
              endif
           enddo
           ! Envoie
           nbsendSUDOUEST_mv=idiSUDOUEST_mv-1
           call MPI_IRSEND(ftrsudouestout_mv,  nbsendSUDOUEST_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(sudouest), &
              TAGSUDOUEST_mv, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1        
           call MPI_IRECV(ftrsudouest_mv,  nbsendSUDOUEST_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(sudouest), &
              TAGNORDEST_mv, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1

        endif

!----------------------------------------------------------------------------------------
!  SUD-EST
!----------------------------------------------------------------------------------------
  !     if (par%tvoisin(sudest) /= mpi_proc_null) then
        if ( (par%tvoisin(sud) /= mpi_proc_null).and. (par%tvoisin(est) /= mpi_proc_null)) then
           i=imax-1
           j=2
           do k=1,kmax
              l_nbq=ijk2lq_nh(i,j,k)             
              if (l_nbq.gt.0) then
                ftrSUDESTout_mv(idiSUDEST_mv) = div_nbq_a(l_nbq,1)  !* mask_t(i,j,k)
                idiSUDEST_mv=idiSUDEST_mv+1
              endif
           enddo

           ! Envoie
           nbsendSUDEST_mv=idiSUDEST_mv-1
           call MPI_IRSEND(ftrsudestout_mv,  nbsendSUDEST_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(sudest), &
              TAGSUDEST_mv, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1
           call MPI_IRECV(ftrsudest_mv,  nbsendSUDEST_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(sudest), &
              TAGNORDOUEST_mv, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1
        endif

!----------------------------------------------------------------------------------------
!  NORD-EST
!----------------------------------------------------------------------------------------
   !    if (par%tvoisin(nordest) /= mpi_proc_null) then
        if ( (par%tvoisin(nord) /= mpi_proc_null).and. (par%tvoisin(est) /= mpi_proc_null)) then  
           i=imax-1
           j=jmax-1
           do k=1,kmax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                ftrNORDESTout_mv(idiNORDEST_mv) = div_nbq_a(l_nbq,1)  !* mask_t(i,j,k)
                idiNORDEST_mv=idiNORDEST_mv+1
              endif
           enddo

           ! Envoie
           nbsendNORDEST_mv=idiNORDEST_mv-1
           call MPI_IRSEND(ftrnordestout_mv,  nbsendNORDEST_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(nordest), &
              TAGNORDEST_mv, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1
           call MPI_IRECV(ftrnordest_mv,  nbsendNORDEST_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(nordest), &
              TAGSUDOUEST_mv, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1
        endif

!----------------------------------------------------------------------------------------
!  NORD-OUEST
!----------------------------------------------------------------------------------------
   !    if (par%tvoisin(nordouest) /= mpi_proc_null) then
        if ( (par%tvoisin(nord) /= mpi_proc_null).and. (par%tvoisin(ouest) /= mpi_proc_null)) then
           i=2
           j=jmax-1           
           do k=1,kmax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                ftrNORDOUESTout_mv(idiNORDOUEST_mv) = div_nbq_a(l_nbq,1)  !* mask_t(i,j,k)
                idiNORDOUEST_mv=idiNORDOUEST_mv+1
              endif
           enddo

           ! Envoie
           nbsendNORDOUEST_mv=idiNORDOUEST_mv-1
           call MPI_IRSEND(ftrnordouestout_mv,  nbsendNORDOUEST_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(nordouest), &
              TAGNORDOUEST_mv, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1
           call MPI_IRECV(ftrnordouest_mv,  nbsendNORDOUEST_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(nordouest), &
              TAGSUDEST_mv, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1

        endif

     endif ! if (ichoix.eq.7) 
     
      if (ichoix.eq.5517) then
!****************************************************************************************
! Echanges des variables "boucle NBQ" qunatité de mouvement :: RECEPTION/WAIT
!****************************************************************************************
 
!----------------------------------------------------------------------------------------
! Salle d'attente
!----------------------------------------------------------------------------------------

       nbreq_mv = nbreq_mv-1
       CALL MPI_WAITALL(nbreq_mv, tabreq_mv(1:nbreq_mv), tstatus(:,1:nbreq_mv), IERR)   

!----------------------------------------------------------------------------------------
!  OUEST
!----------------------------------------------------------------------------------------
        tmptime1 = mpi_wtime()
        if (par%tvoisin(ouest) /= mpi_proc_null) then
           idiOUEST_mv=1
           i=1
           do k=1,kmax
           do j=1,jmax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                div_nbq_a(l_nbq,1) = ftrOUEST_mv(idiOUEST_mv)
                idiOUEST_mv=idiOUEST_mv+1
              endif
           enddo
           enddo
        endif
!----------------------------------------------------------------------------------------
!  EST
!----------------------------------------------------------------------------------------
        if (par%tvoisin(est) /= mpi_proc_null) then
           idiEST_mv=1
           i=imax
           do k=1,kmax
           do j=1,jmax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                div_nbq_a(l_nbq,1) = ftrEST_mv(idiEST_mv)
                idiEST_mv=idiEST_mv+1
              endif
           enddo
           enddo
        endif
!----------------------------------------------------------------------------------------
!  SUD
!----------------------------------------------------------------------------------------
        tmptime1 = mpi_wtime()
        if (par%tvoisin(sud) /= mpi_proc_null) then
           idiSUD_mv=1
           j=1
           do k=1,kmax
           do i=1,imax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                div_nbq_a(l_nbq,1) = ftrSUD_mv(idiSUD_mv)
                idiSUD_mv=idiSUD_mv+1
              endif
           enddo
           enddo
        endif
!----------------------------------------------------------------------------------------
!  NORD
!----------------------------------------------------------------------------------------
        if (par%tvoisin(nord) /= mpi_proc_null) then
           idiNORD_mv=1
           j=jmax
           do k=1,kmax
           do i=1,imax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                div_nbq_a(l_nbq,1) = ftrNORD_mv(idiNORD_mv)
                idiNORD_mv=idiNORD_mv+1
              endif
           enddo
           enddo
        endif
        tmptime2 = mpi_wtime()
        time_omp_nh(20)=time_omp_nh(20)+(tmptime2-tmptime1)

!****************************************************************************************
! Echanges des coins: reception
!****************************************************************************************
!----------------------------------------------------------------------------------------
!  SUD-OUEST
!----------------------------------------------------------------------------------------
   !    if (par%tvoisin(sudouest) /= mpi_proc_null) then
        if ( (par%tvoisin(sud) /= mpi_proc_null).and. (par%tvoisin(ouest) /= mpi_proc_null)) then
           idiSUDOUEST_mv=1
           i=1
           j=1
           do k=1,kmax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                div_nbq_a(l_nbq,1) = ftrSUDOUEST_mv(idiSUDOUEST_mv)
                idiSUDOUEST_mv=idiSUDOUEST_mv+1
              endif
           enddo
        endif
!----------------------------------------------------------------------------------------
!  SUD-EST
!----------------------------------------------------------------------------------------
   !    if (par%tvoisin(sudest) /= mpi_proc_null) then
        if ( (par%tvoisin(sud) /= mpi_proc_null).and. (par%tvoisin(est) /= mpi_proc_null)) then
           idiSUDEST_mv=1
           i=imax
           j=1
           do k=1,kmax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                div_nbq_a(l_nbq,1) = ftrSUDEST_mv(idiSUDEST_mv)
                idiSUDEST_mv=idiSUDEST_mv+1
              endif
           enddo
        endif

!----------------------------------------------------------------------------------------
!  NORD-EST
!----------------------------------------------------------------------------------------
    !   if (par%tvoisin(nordest) /= mpi_proc_null) then
        if ( (par%tvoisin(nord) /= mpi_proc_null).and. (par%tvoisin(est) /= mpi_proc_null)) then  
           idiNORDEST_mv=1
           i=imax
           j=jmax
           do k=1,kmax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                div_nbq_a(l_nbq,1) = ftrNORDEST_mv(idiNORDEST_mv)
                idiNORDEST_mv=idiNORDEST_mv+1
              endif
           enddo
        endif

!----------------------------------------------------------------------------------------
!  NORD-OUEST
!----------------------------------------------------------------------------------------
    !   if (par%tvoisin(nordouest) /= mpi_proc_null) then
        if ( (par%tvoisin(nord) /= mpi_proc_null).and. (par%tvoisin(ouest) /= mpi_proc_null)) then
           idiNORDOUEST_mv=1
           i=1
           j=jmax
           do k=1,kmax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                div_nbq_a(l_nbq,1) = ftrNORDOUEST_mv(idiNORDOUEST_mv)
                idiNORDOUEST_mv=idiNORDOUEST_mv+1
              endif
           enddo
        endif
       endif ! if (ichoix.eq.17) 

      if (ichoix.eq.558) then
!****************************************************************************************
! Echanges des variables "boucle NBQ" divergence :: ENVOIE
!****************************************************************************************

        nbreq_mv        = 1
        idiOUEST_mv     = 1
        idiEST_mv       = 1 
        idiSUD_mv       = 1
        idiNORD_mv      = 1 
        idiSUDOUEST_mv  = 1
        idiSUDEST_mv    = 1 
        idiNORDEST_mv   = 1
        idiNORDOUEST_mv = 1 

!----------------------------------------------------------------------------------------
!  OUEST
!----------------------------------------------------------------------------------------
        if (par%tvoisin(ouest) /= mpi_proc_null) then
           tmptime1 = mpi_wtime()
           i=2
           do k=1,kmax
           do j=1,jmax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                 ftrOUESTout_rhs(idiOUEST_mv) = rhs2_nh(l_nbq)  !* mask_t(i,j,k)
                 idiOUEST_mv=idiOUEST_mv+1
              endif
           enddo
           enddo
           tmptime2 = mpi_wtime()
           time_omp_nh(20)=time_omp_nh(20)+(tmptime2-tmptime1)

           ! Envoie
           tmptime1 = mpi_wtime()
           nbsendOUEST_mv=idiOUEST_mv-1
           call MPI_IRSEND(ftrouestout_rhs,  nbsendOUEST_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(ouest), &
              TAGOUEST_rhs, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1
           call MPI_IRECV(ftrouest_mv,  nbsendOUEST_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(ouest), &
              TAGEST_rhs, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1
           tmptime2 = mpi_wtime()
           time_omp_nh(21)=time_omp_nh(21)+(tmptime2-tmptime1)
        endif

!----------------------------------------------------------------------------------------
!  EST
!----------------------------------------------------------------------------------------
        if (par%tvoisin(est) /= mpi_proc_null) then
           tmptime1 = mpi_wtime()
           i = imax-1
           do k=1,kmax
           do j=1,jmax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                ftrESTout_rhs(idiEST_mv) = rhs2_nh(l_nbq)  !* mask_t(i,j,k)
                idiEST_mv=idiEST_mv+1
        !	if (par%rank.eq.0) write(6,*) iteration3d,iteration2d,iteration_nbq,par%rank,"--->",i,j,k,rhs2_nh(l_nbq),mask_t(i,j,k) 
              endif
           enddo
           enddo
           tmptime2 = mpi_wtime()
           time_omp_nh(20)=time_omp_nh(20)+(tmptime2-tmptime1)

           ! Envoie
           tmptime1 = mpi_wtime()
           nbsendEST_mv=idiEST_mv-1
           call MPI_IRSEND(ftrestout_rhs,  nbsendEST_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(est), &
              TAGEST_rhs, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1
           call MPI_IRECV(ftrest_mv,  nbsendEST_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(est), &
              TAGOUEST_rhs, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1
           tmptime2 = mpi_wtime()
           time_omp_nh(21)=time_omp_nh(21)+(tmptime2-tmptime1)
        endif

!----------------------------------------------------------------------------------------
!  SUD
!----------------------------------------------------------------------------------------
        if (par%tvoisin(sud) /= mpi_proc_null) then
           tmptime1 = mpi_wtime()
           j=2
           do k=1,kmax
           do i=1,imax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                ftrSUDout_rhs(idiSUD_mv) = rhs2_nh(l_nbq)  !* mask_t(i,j,k)
                idiSUD_mv=idiSUD_mv+1
              endif
           enddo
           enddo
           tmptime2 = mpi_wtime()
           time_omp_nh(20)=time_omp_nh(20)+(tmptime2-tmptime1)

           ! Envoie
           tmptime1 = mpi_wtime()
           nbsendSUD_mv=idiSUD_mv-1
           call MPI_IRSEND(ftrsudout_rhs,  nbsendSUD_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(sud), &
              TAGSUD_rhs, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1
           call MPI_IRECV(ftrsud_mv,  nbsendSUD_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(sud), &
              TAGNORD_rhs, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1
           tmptime2 = mpi_wtime()
           time_omp_nh(21)=time_omp_nh(21)+(tmptime2-tmptime1)
        endif

!----------------------------------------------------------------------------------------
!  NORD
!----------------------------------------------------------------------------------------
        if (par%tvoisin(nord) /= mpi_proc_null) then
           tmptime1 = mpi_wtime()
           j = jmax-1
           do k=1,kmax
           do i=1,imax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                ftrNORDout_rhs(idiNORD_mv) = rhs2_nh(l_nbq)  !* mask_t(i,j,k)
!               ftrNORDout(idiNORD) = 3
                idiNORD_mv=idiNORD_mv+1
              endif
           enddo
           enddo
           tmptime2 = mpi_wtime()
           time_omp_nh(20)=time_omp_nh(20)+(tmptime2-tmptime1)

           ! Envoie
           tmptime1 = mpi_wtime()
           nbsendNORD_mv=idiNORD_mv-1
           call MPI_IRSEND(ftrnordout_rhs,  nbsendNORD_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(nord), &
              TAGNORD_rhs, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1
           call MPI_IRECV(ftrnord_mv,  nbsendNORD_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(nord), &
              TAGSUD_rhs, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1
           tmptime2 = mpi_wtime()
           time_omp_nh(21)=time_omp_nh(21)+(tmptime2-tmptime1)
        endif

!----------------------------------------------------------------------------------------
!  SUD-OUEST
!----------------------------------------------------------------------------------------
     !  if (par%tvoisin(sudouest) /= mpi_proc_null) then
        if ( (par%tvoisin(sud) /= mpi_proc_null).and. (par%tvoisin(ouest) /= mpi_proc_null)) then
           i=2
           j=2
           do k=1,kmax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then 
                ftrSUDOUESTout_rhs(idiSUDOUEST_mv) = rhs2_nh(l_nbq)  !* mask_t(i,j,k)
                idiSUDOUEST_mv=idiSUDOUEST_mv+1
              endif
           enddo
           ! Envoie
           nbsendSUDOUEST_mv=idiSUDOUEST_mv-1
           call MPI_IRSEND(ftrsudouestout_rhs,  nbsendSUDOUEST_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(sudouest), &
              TAGSUDOUEST_rhs, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1        
           call MPI_IRECV(ftrsudouest_mv,  nbsendSUDOUEST_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(sudouest), &
              TAGNORDEST_rhs, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1

        endif

!----------------------------------------------------------------------------------------
!  SUD-EST
!----------------------------------------------------------------------------------------
  !     if (par%tvoisin(sudest) /= mpi_proc_null) then
        if ( (par%tvoisin(sud) /= mpi_proc_null).and. (par%tvoisin(est) /= mpi_proc_null)) then
           i=imax-1
           j=2
           do k=1,kmax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                ftrSUDESTout_rhs(idiSUDEST_mv) = rhs2_nh(l_nbq)  !* mask_t(i,j,k)
                idiSUDEST_mv=idiSUDEST_mv+1
              endif
           enddo

           ! Envoie
           nbsendSUDEST_mv=idiSUDEST_mv-1
           call MPI_IRSEND(ftrsudestout_rhs,  nbsendSUDEST_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(sudest), &
              TAGSUDEST_rhs, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1
           call MPI_IRECV(ftrsudest_mv,  nbsendSUDEST_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(sudest), &
              TAGNORDOUEST_rhs, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1
        endif

!----------------------------------------------------------------------------------------
!  NORD-EST
!----------------------------------------------------------------------------------------
   !    if (par%tvoisin(nordest) /= mpi_proc_null) then
        if ( (par%tvoisin(nord) /= mpi_proc_null).and. (par%tvoisin(est) /= mpi_proc_null)) then  
           i=imax-1
           j=jmax-1
           do k=1,kmax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                ftrNORDESTout_rhs(idiNORDEST_mv) = rhs2_nh(l_nbq)  !* mask_t(i,j,k)
                idiNORDEST_mv=idiNORDEST_mv+1
              endif
           enddo

           ! Envoie
           nbsendNORDEST_mv=idiNORDEST_mv-1
           call MPI_IRSEND(ftrnordestout_rhs,  nbsendNORDEST_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(nordest), &
              TAGNORDEST_rhs, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1
           call MPI_IRECV(ftrnordest_mv,  nbsendNORDEST_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(nordest), &
              TAGSUDOUEST_rhs, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1
        endif

!----------------------------------------------------------------------------------------
!  NORD-OUEST
!----------------------------------------------------------------------------------------
   !    if (par%tvoisin(nordouest) /= mpi_proc_null) then
        if ( (par%tvoisin(nord) /= mpi_proc_null).and. (par%tvoisin(ouest) /= mpi_proc_null)) then
           i=2
           j=jmax-1           
           do k=1,kmax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                ftrNORDOUESTout_rhs(idiNORDOUEST_mv) = rhs2_nh(l_nbq)  !* mask_t(i,j,k)
                idiNORDOUEST_mv=idiNORDOUEST_mv+1
              endif
           enddo

           ! Envoie
           nbsendNORDOUEST_mv=idiNORDOUEST_mv-1
           call MPI_IRSEND(ftrnordouestout_rhs,  nbsendNORDOUEST_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(nordouest), &
              TAGNORDOUEST_rhs, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1
           call MPI_IRECV(ftrnordouest_mv,  nbsendNORDOUEST_mv, MPI_DOUBLE_PRECISION, PAR%tvoisin(nordouest), &
              TAGSUDEST_rhs, par%comm2d, tabreq_mv(nbreq_mv), ierr)
           nbreq_mv=nbreq_mv+1

        endif

     endif ! if (ichoix.eq.8) 
     
      if (ichoix.eq.5518) then
!****************************************************************************************
! Echanges des variables "boucle NBQ" qunatité de mouvement :: RECEPTION/WAIT
!****************************************************************************************

!----------------------------------------------------------------------------------------
! Salle d'attente
!----------------------------------------------------------------------------------------

       nbreq_mv = nbreq_mv-1
       CALL MPI_WAITALL(nbreq_mv, tabreq_mv(1:nbreq_mv), tstatus(:,1:nbreq_mv), IERR)   

!----------------------------------------------------------------------------------------
!  OUEST
!----------------------------------------------------------------------------------------
        tmptime1 = mpi_wtime()
        if (par%tvoisin(ouest) /= mpi_proc_null) then
           idiOUEST_mv=1
           i=1
           do k=1,kmax
           do j=1,jmax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                rhs2_nh(l_nbq) = ftrOUEST_mv(idiOUEST_mv)
                idiOUEST_mv=idiOUEST_mv+1
              endif
           enddo
           enddo
        endif
!----------------------------------------------------------------------------------------
!  EST
!----------------------------------------------------------------------------------------
        if (par%tvoisin(est) /= mpi_proc_null) then
           idiEST_mv=1
           i=imax
           do k=1,kmax
           do j=1,jmax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                rhs2_nh(l_nbq) = ftrEST_mv(idiEST_mv)
                idiEST_mv=idiEST_mv+1
              endif
           enddo
           enddo
        endif
!----------------------------------------------------------------------------------------
!  SUD
!----------------------------------------------------------------------------------------
        tmptime1 = mpi_wtime()
        if (par%tvoisin(sud) /= mpi_proc_null) then
           idiSUD_mv=1
           j=1
           do k=1,kmax
           do i=1,imax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                rhs2_nh(l_nbq) = ftrSUD_mv(idiSUD_mv)
                idiSUD_mv=idiSUD_mv+1
              endif
           enddo
           enddo
        endif
!----------------------------------------------------------------------------------------
!  NORD
!----------------------------------------------------------------------------------------
        if (par%tvoisin(nord) /= mpi_proc_null) then
           idiNORD_mv=1
           j=jmax
           do k=1,kmax
           do i=1,imax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                rhs2_nh(l_nbq) = ftrNORD_mv(idiNORD_mv)
                idiNORD_mv=idiNORD_mv+1
              endif
           enddo
           enddo
        endif
        tmptime2 = mpi_wtime()
        time_omp_nh(20)=time_omp_nh(20)+(tmptime2-tmptime1)

!****************************************************************************************
! Echanges des coins: reception
!****************************************************************************************
!----------------------------------------------------------------------------------------
!  SUD-OUEST
!----------------------------------------------------------------------------------------
   !    if (par%tvoisin(sudouest) /= mpi_proc_null) then
        if ( (par%tvoisin(sud) /= mpi_proc_null).and. (par%tvoisin(ouest) /= mpi_proc_null)) then
           idiSUDOUEST_mv=1
           i=1
           j=1
           do k=1,kmax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                rhs2_nh(l_nbq) = ftrSUDOUEST_mv(idiSUDOUEST_mv)
                idiSUDOUEST_mv=idiSUDOUEST_mv+1
              endif  
           enddo
        endif
!----------------------------------------------------------------------------------------
!  SUD-EST
!----------------------------------------------------------------------------------------
   !    if (par%tvoisin(sudest) /= mpi_proc_null) then
        if ( (par%tvoisin(sud) /= mpi_proc_null).and. (par%tvoisin(est) /= mpi_proc_null)) then
           idiSUDEST_mv=1
           i=imax
           j=1
           do k=1,kmax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                rhs2_nh(l_nbq) = ftrSUDEST_mv(idiSUDEST_mv)
                idiSUDEST_mv=idiSUDEST_mv+1
              endif
           enddo
        endif

!----------------------------------------------------------------------------------------
!  NORD-EST
!----------------------------------------------------------------------------------------
    !   if (par%tvoisin(nordest) /= mpi_proc_null) then
        if ( (par%tvoisin(nord) /= mpi_proc_null).and. (par%tvoisin(est) /= mpi_proc_null)) then  
           idiNORDEST_mv=1
           i=imax
           j=jmax
           do k=1,kmax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                rhs2_nh(l_nbq) = ftrNORDEST_mv(idiNORDEST_mv)
                idiNORDEST_mv=idiNORDEST_mv+1
              endif
           enddo
        endif

!----------------------------------------------------------------------------------------
!  NORD-OUEST
!----------------------------------------------------------------------------------------
    !   if (par%tvoisin(nordouest) /= mpi_proc_null) then
        if ( (par%tvoisin(nord) /= mpi_proc_null).and. (par%tvoisin(ouest) /= mpi_proc_null)) then
           idiNORDOUEST_mv=1
           i=1
           j=jmax
           do k=1,kmax
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq.gt.0) then
                rhs2_nh(l_nbq) = ftrNORDOUEST_mv(idiNORDOUEST_mv)
                idiNORDOUEST_mv=idiNORDOUEST_mv+1
              endif
           enddo
        endif
       endif ! if (ichoix.eq.18) 

       end subroutine parallele_nbq
#else
      subroutine parallele_nbq(ichoix)
      implicit none
      integer,intent(in) :: ichoix
      end subroutine parallele_nbq
#endif
