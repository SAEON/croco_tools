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
      integer,dimension(10),parameter :: tagqdmU_Send= (/ 55001, 55011, 56011, 56001, 0, 0,  155001, 515011,    516001, 516011  /)
      integer,dimension(10),parameter :: tagqdmU_Recv= (/ 55011, 55001, 56001, 56011, 0, 0,  516011, 516001,    515011, 155001  /)
      integer,dimension(10),parameter :: tagqdmV_Send= (/ 55002, 55012, 56012, 56002, 0, 0,  155002, 515012,    516002, 516012  /)
      integer,dimension(10),parameter :: tagqdmV_Recv= (/ 55012, 55002, 56002, 56012, 0, 0,  516012, 516002,    515012, 155002  /)
      integer,dimension(10),parameter :: tagqdmW_Send= (/ 55003, 55013, 56013, 56003, 0, 0,  155003, 515013,    516003, 516013  /)
      integer,dimension(10),parameter :: tagqdmW_Recv= (/ 55013, 55003, 56003, 56013, 0, 0,  516013, 516003,    515013, 155003  /)
      integer :: i, j, k, bcl
      double precision :: tmptime1,tmptime2
      logical,save :: firstpass=.true.
      integer :: vois
      integer(MPI_ADDRESS_KIND) :: addr1
!       double precision,dimension(0:size(qdm_nbq_a(:,vnnew_nbq))) :: qdm_tmp1,qdm_tmp2
      integer :: sizeqdm, szsend, szrecv, debsend,debrecv
      integer :: kmax,imax,jmax
#define LOCALLM Lmmpi
#define LOCALMM Mmmpi
       
      imax=LOCALLM+1
      jmax=LOCALMM+1
     


      if (ichoix.eq.5) then
!****************************************************************************************
! Echanges des variables "boucle NBQ" qunatité de mouvement :: ENVOI
!****************************************************************************************
	  nbreq_qdm=1
	  do bcl=1, 8
	   vois=lvoisin(bcl)
	   if (par%tvoisin(vois) /= mpi_proc_null) then 
	   call MPI_TYPE_SIZE(ech_qdm_nbq(vois)%send, szsend,ierr)
	   call MPI_TYPE_SIZE(ech_qdm_nbq(vois)%recv, szrecv,ierr) 
           call MPI_TYPE_GET_EXTENT(ech_qdm_nbq(vois)%send, debsend, szsend, ierr)
           call MPI_TYPE_GET_EXTENT(ech_qdm_nbq(vois)%recv, debrecv, szrecv, ierr)
            call MPI_IRECV(qdm_nbq_a(1,vnnew_nbq),  1, ech_qdm_nbq(vois)%recv, par%tvoisin(vois), &
	      tagqdm_Recv(vois), par%comm2d, tabreq_qdm(nbreq_qdm), ierr)
 	     nbreq_qdm=nbreq_qdm+1
 	     call MPI_IRSEND(qdm_nbq_a(1,vnnew_nbq),  1, ech_qdm_nbq(vois)%send, par%tvoisin(vois), &
               tagqdm_Send(vois), par%comm2d, tabreq_qdm(nbreq_qdm), ierr)
  	     nbreq_qdm=nbreq_qdm+1	
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
       endif
       
      if (ichoix.eq.51) then ! U only
!****************************************************************************************
! Echanges des variables "boucle NBQ" qunatité de mouvement U :: ENVOI
!****************************************************************************************
	  nbreq_qdmU=1
	  do bcl=1, 8
	   vois=lvoisin(bcl)
	   if (par%tvoisin(vois) /= mpi_proc_null) then 
	   call MPI_TYPE_SIZE(ech_qdmU_nbq(vois)%send, szsend,ierr)
	   call MPI_TYPE_SIZE(ech_qdmU_nbq(vois)%recv, szrecv,ierr) 
	   if (szsend >0) then
	      call MPI_IRECV(qdm_nbq_a(1,vnnew_nbq),  1, ech_qdmU_nbq(vois)%recv, par%tvoisin(vois), &
		tagqdmU_Recv(vois), par%comm2d, tabreq_qdmU(nbreq_qdmU), ierr)
		nbreq_qdmU=nbreq_qdmU+1
	      call MPI_IRSEND(qdm_nbq_a(1,vnnew_nbq),  1, ech_qdmU_nbq(vois)%send, par%tvoisin(vois), &
		tagqdmU_Send(vois), par%comm2d, tabreq_qdmU(nbreq_qdmU), ierr)
	      nbreq_qdmU=nbreq_qdmU+1
  	   endif
           endif
	  enddo
      endif
      if (ichoix.eq.151) then
!****************************************************************************************
! Echanges des variables "boucle NBQ" qunatité de mouvement :: RECEPTION/WAIT
!****************************************************************************************
!----------------------------------------------------------------------------------------
!  Salle d'attente
!----------------------------------------------------------------------------------------
	nbreq_qdmU = nbreq_qdmU-1
	if (nbreq_qdmU > 0 ) CALL MPI_WAITALL(nbreq_qdmU, tabreq_qdmU(1:nbreq_qdmU), tstatus(:,1:nbreq_qdmU), IERR)   
        nbreq_qdmV=0
       endif
       
      if (ichoix.eq.52) then ! V only
!****************************************************************************************
! Echanges des variables "boucle NBQ" qunatité de mouvement U :: ENVOI
!****************************************************************************************
	  nbreq_qdmV=1
	  do bcl=1, 8
	   vois=lvoisin(bcl)
	   if (par%tvoisin(vois) /= mpi_proc_null) then 
	   call MPI_TYPE_SIZE(ech_qdmV_nbq(vois)%send, szsend,ierr)
	   call MPI_TYPE_SIZE(ech_qdmV_nbq(vois)%recv, szrecv,ierr) 
	   if (szsend >0) then
	    call MPI_IRECV(qdm_nbq_a(1,vnnew_nbq),  1, ech_qdmV_nbq(vois)%recv, par%tvoisin(vois), &
		tagqdmV_Recv(vois), par%comm2d, tabreq_qdmV(nbreq_qdmV), ierr)
	    nbreq_qdmV=nbreq_qdmV+1
 	    call MPI_IRSEND(qdm_nbq_a(1,vnnew_nbq),  1, ech_qdmV_nbq(vois)%send, par%tvoisin(vois), &
               tagqdmV_Send(vois), par%comm2d, tabreq_qdmV(nbreq_qdmV), ierr)
	      nbreq_qdmV=nbreq_qdmV+1	
	   endif
           endif
	  enddo 
      endif
      if (ichoix.eq.152) then
!****************************************************************************************
! Echanges des variables "boucle NBQ" qunatité de mouvement :: RECEPTION/WAIT
!****************************************************************************************
!----------------------------------------------------------------------------------------
!  Salle d'attente
!----------------------------------------------------------------------------------------
	nbreq_qdmV = nbreq_qdmV-1
	if (nbreq_qdmV > 0 ) CALL MPI_WAITALL(nbreq_qdmV, tabreq_qdmV(1:nbreq_qdmV), tstatus(:,1:nbreq_qdmV), IERR)   
        nbreq_qdmV=0
       endif

      if (ichoix.eq.53) then ! W only
!****************************************************************************************
! Echanges des variables "boucle NBQ" qunatité de mouvement U :: ENVOI
!****************************************************************************************
	  nbreq_qdmW=1
	  do bcl=1, 8
	   vois=lvoisin(bcl)
	   if (par%tvoisin(vois) /= mpi_proc_null) then 
	   call MPI_TYPE_SIZE(ech_qdmW_nbq(vois)%send, szsend,ierr)
	   call MPI_TYPE_SIZE(ech_qdmW_nbq(vois)%recv, szrecv,ierr) 
	   if (szsend >0) then
	    call MPI_IRECV(qdm_nbq_a(1,vnnew_nbq),  1, ech_qdmW_nbq(vois)%recv, par%tvoisin(vois), &
		tagqdmW_Recv(vois), par%comm2d, tabreq_qdmW(nbreq_qdmW), ierr)
	    nbreq_qdmW=nbreq_qdmW+1
	    call MPI_IRSEND(qdm_nbq_a(1,vnnew_nbq),  1, ech_qdmW_nbq(vois)%send, par%tvoisin(vois), &
               tagqdmW_Send(vois), par%comm2d, tabreq_qdmW(nbreq_qdmW), ierr)
  	    nbreq_qdmW=nbreq_qdmW+1	
  	   endif
           endif
	  enddo
      endif
      if (ichoix.eq.153) then
!****************************************************************************************
! Echanges des variables "boucle NBQ" qunatité de mouvement :: RECEPTION/WAIT
!****************************************************************************************
!----------------------------------------------------------------------------------------
!  Salle d'attente
!----------------------------------------------------------------------------------------
	nbreq_qdmW = nbreq_qdmW-1
	if (nbreq_qdmW > 0 ) CALL MPI_WAITALL(nbreq_qdmW, tabreq_qdmW(1:nbreq_qdmW), tstatus(:,1:nbreq_qdmW), IERR)   
        nbreq_qdmW=0
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

            call MPI_IRECV(div_nbq_a(1,dnrhs_nbq),  1, ech_div_nbq(vois)%recv, par%tvoisin(vois), &
	      tagdiv_Recv(vois), par%comm2d, tabreq_mv(nbreq_mv), ierr)
 	     nbreq_mv=nbreq_mv+1
 	     call MPI_IRSEND(div_nbq_a(1,dnrhs_nbq),  1, ech_div_nbq(vois)%send, par%tvoisin(vois), &
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


       end subroutine parallele_nbq
#else
      subroutine parallele_nbq(ichoix)
      implicit none
      integer,intent(in) :: ichoix
      end subroutine parallele_nbq
#endif
