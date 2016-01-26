#include "cppdefs.h"
#if defined NBQ && defined MPI

module module_parallel_nbq
!------------------------------------------------------------------------------
!                       NBQ Module for MPI-exchanges
!------------------------------------------------------------------------------

      implicit none
      integer, parameter :: ouest=1,est=2,nord=3,sud=4,haut=5,bas=6
      integer, parameter :: sudouest=7,sudest=8,nordouest=9,nordest=10
      integer, parameter :: ouestest=1,nordsud=2
      include 'mpif.h'    
# include "param_F90.h"
# include "scalars_F90.h"

  type echblock
	  integer :: send
	  integer :: recv
  end type echblock
  type infopar_croco
         integer ::  comm2d                 !COMMUNICATEUR GLOBAL
         integer ::  rank
         integer,dimension(10)                      ::  tvoisin         !LE NUMERO DES VOISINS DANS L'ORDRE(O,E,S,N)
  end type infopar_croco

  
  type (infopar_croco) :: par
  type (echblock),dimension(10) :: ech_qdm_nbq, ech_div_nbq, ech_rhs2_nh
  type (echblock),dimension(10) :: ech_qdmU_nbq, ech_qdmV_nbq, ech_qdmW_nbq
  integer,dimension(8),parameter :: liste_voisin = &
      (/ ouest, est, nord, sud, sudouest, sudest, nordouest, nordest /)
  integer :: ierr

  !! For 3D arrays (grid varibles exchanges) 
  type boundarybuff
	double precision,dimension(:,:),allocatable :: ouest,est,sud,nord,nordest, &
			sudest,sudouest,norouest
  end type boundarybuff
  
  type (boundarybuff) 			:: gdepth_uS, gdepth_uR
  type (boundarybuff) 			:: gdepth_vS, gdepth_vR
  type (boundarybuff) 			:: coefa_uS,  coefa_uR
  type (boundarybuff) 			:: coefa_vS,  coefa_vR
  type (boundarybuff) 			:: coefb_uS,  coefb_uR
  type (boundarybuff) 			:: coefb_vS,  coefb_vR
  integer 				:: sz1, sz2, sz3, sz4, sz5, sz6
  logical,save 				:: doalloc=.TRUE.
 
 
contains 

!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!                 ROUTINES QDM_NBQ
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!--------------------------------------------------------------------------
subroutine borne_echange_qdm_nbq_a(voisin,imax,jmax,kmax, &
	  ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s, &
	  ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r )
  use module_nh , only : istr_nh,iend_nh,jstr_nh,jend_nh,istru_nh,iendu_nh,jstrv_nh,jendv_nh
!  use module_parallele 	  
  implicit none
      integer, parameter :: ouest=1,est=2,nord=3,sud=4,haut=5,bas=6
      integer, parameter :: sudouest=7,sudest=8,nordouest=9,nordest=10
      integer, parameter :: ouestest=1,nordsud=2
  integer,intent(in) :: voisin	  
  integer,intent(in) :: imax,jmax,kmax
  integer,dimension(3),intent(out) :: ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s
  integer,dimension(3),intent(out) :: ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r
!   write(6000+par%rank,*) "borne_echange_qdm_nbq_a voisin:",voisin

!   write(200+par%rank,*) "istr_nh,iend_nh  =",istr_nh,iend_nh
!   write(200+par%rank,*) "istru_nh,iendu_nh=",istru_nh,iendu_nh
!   write(200+par%rank,*) "jstr_nh,jend_nh  =",jstr_nh,jend_nh
!   write(200+par%rank,*) "jstrv_nh,jendv_nh=",jstrv_nh,jendv_nh
!   write(300+par%rank,*) "istr_nh,iend_nh  =",istr_nh,iend_nh
!   write(300+par%rank,*) "istru_nh,iendu_nh=",istru_nh,iendu_nh
!   write(300+par%rank,*) "jstr_nh,jend_nh  =",jstr_nh,jend_nh
!   write(300+par%rank,*) "jstrv_nh,jendv_nh=",jstrv_nh,jendv_nh
  select case(voisin)
  !!! (/ U, V, W /)
  case(ouest) !........................
      ! Envoie
      ideb_s=(/ istru_nh,      istr_nh,   istr_nh    /)
      ifin_s=(/ istru_nh,      istr_nh,   istr_nh    /)
      jdeb_s=(/  jstr_nh,     jstrv_nh,   jstr_nh    /)
      jfin_s=(/  jend_nh,     jendv_nh,   jend_nh    /)
      kdeb_s=(/ 1,    1,    0    /)
      kfin_s=(/ kmax, kmax, kmax /)
      ! Reception
      ideb_r=(/ istru_nh-1,     istr_nh-1,    istr_nh-1  /)
      ifin_r=(/ istru_nh-1,     istr_nh-1,    istr_nh-1  /)
      jdeb_r=(/  jstr_nh,        jstrv_nh,      jstr_nh  /)
      jfin_r=(/  jend_nh,        jendv_nh,      jend_nh  /)  
      kdeb_r=(/ 1,    1,    0    /)
      kfin_r=(/ kmax, kmax, kmax /)

      case(est)
      ! Envoie
      ideb_s=(/ iendu_nh,  iend_nh, iend_nh /)
      ifin_s=(/ iendu_nh,  iend_nh, iend_nh /)
      jdeb_s=(/  jstr_nh, jstrv_nh, jstr_nh /)
      jfin_s=(/  jend_nh, jendv_nh, jend_nh /)
      kdeb_s=(/ 1,    1,    0    /)
      kfin_s=(/ kmax, kmax, kmax /)
      ! Reception
      ideb_r=(/ iendu_nh+1,  iend_nh+1, iend_nh+1  /)
      ifin_r=(/ iendu_nh+1,  iend_nh+1, iend_nh+1  /)
      jdeb_r=(/  jstr_nh,     jstrv_nh,   jstr_nh  /)
      jfin_r=(/  jend_nh,     jendv_nh,   jend_nh  /)
      kdeb_r=(/ 1,      1,      0       /)
      kfin_r=(/ kmax,   kmax,   kmax    /)

  case(nord) !........................
      ! Envoie
      ideb_s=(/ istru_nh,     istr_nh,    istr_nh    /)
      ifin_s=(/ iendu_nh,     iend_nh,    iend_nh    /)
      jdeb_s=(/  jend_nh,    jendv_nh,    jend_nh    /)
      jfin_s=(/  jend_nh,    jendv_nh,    jend_nh   /)
      kdeb_s=(/ 1,    1,    0    /)
      kfin_s=(/ kmax, kmax, kmax /)
      ! Reception
      ideb_r=(/ istru_nh,       istr_nh,      istr_nh       /)
      ifin_r=(/ iendu_nh,       iend_nh,      iend_nh       /)
      jdeb_r=(/  jend_nh+1,     jendv_nh+1,   jend_nh+1     /)
      jfin_r=(/  jend_nh+1,     jendv_nh+1,   jend_nh+1     /)
      kdeb_r=(/ 1,      1,      0       /)
      kfin_r=(/ kmax,   kmax,   kmax    /)

      
   case(sud) !........................ 
      ideb_s=(/ istru_nh,    istr_nh,      istr_nh  /)
      ifin_s=(/ iendu_nh,    iend_nh,      iend_nh  /)
      jdeb_s=(/  jstr_nh,   jstrv_nh,      jstr_nh  /)
      jfin_s=(/  jstr_nh,   jstrv_nh,      jstr_nh  /)
      kdeb_s=(/ 1,    1,       0 /)
      kfin_s=(/ kmax, kmax, kmax /)
      ! Reception
      ideb_r=(/ istru_nh,     istr_nh,      istr_nh      /)
      ifin_r=(/ iendu_nh,     iend_nh,      iend_nh      /)
      jdeb_r=(/  jstr_nh-1,  jstrv_nh-1,    jstr_nh-1    /)
      jfin_r=(/  jstr_nh-1,  jstrv_nh-1,    jstr_nh-1    /)
      kdeb_r=(/ 1,    1,    0    /)
      kfin_r=(/ kmax, kmax, kmax /)

   
      
      !!! Les Coins..............................
   case(sudouest)
!       ! Envoie
      ideb_s=(/ istru_nh,     istr_nh,      istr_nh    /) 
      ifin_s=(/ istru_nh,     istr_nh,      istr_nh    /) 
      jdeb_s=(/  jstr_nh,    jstrv_nh,      jstr_nh    /) 
      jfin_s=(/  jstr_nh,    jstrv_nh,      jstr_nh    /) 
      kdeb_s=(/ 1,    1,    0    /)
      kfin_s=(/ kmax, kmax, kmax /)
!       ! Reception
      ideb_r=(/ istru_nh-1,     istr_nh-1,      istr_nh-1    /) 
      ifin_r=(/ istru_nh-1,     istr_nh-1,      istr_nh-1    /) 
      jdeb_r=(/  jstr_nh-1,    jstrv_nh-1,      jstr_nh-1    /) 
      jfin_r=(/  jstr_nh-1,    jstrv_nh-1,      jstr_nh-1    /) 
      kdeb_r=(/ 1,    1,    0    /)
      kfin_r=(/ kmax, kmax, kmax /)
!      stop 'sudouest'
   case(sudest)
       ! Envoie
      ideb_s=(/ iendu_nh,  iend_nh, iend_nh /) 
      ifin_s=(/ iendu_nh,  iend_nh, iend_nh /) 
      jdeb_s=(/  jstr_nh,    jstrv_nh,      jstr_nh    /) 
      jfin_s=(/  jstr_nh,    jstrv_nh,      jstr_nh    /) 
      kdeb_s=(/ 1,    1,    0    /)
      kfin_s=(/ kmax, kmax, kmax /)
!       ! Reception
      ideb_r=(/ iendu_nh+1,     iend_nh+1,      iend_nh+1      /) 
      ifin_r=(/ iendu_nh+1,     iend_nh+1,      iend_nh+1      /) 
      jdeb_r=(/  jstr_nh-1,    jstrv_nh-1,      jstr_nh-1      /) 
      jfin_r=(/  jstr_nh-1,    jstrv_nh-1,      jstr_nh-1      /) 
      kdeb_r=(/ 1,      1,      0      /)
      kfin_r=(/ kmax,   kmax,   kmax   /)
!      stop 'sudest'
   case(nordouest)
!       ! Envoie
      ideb_s=(/ istru_nh,     istr_nh,      istr_nh     /) 
      ifin_s=(/ istru_nh,     istr_nh,      istr_nh     /) 
      jdeb_s=(/  jend_nh,    jendv_nh,      jend_nh     /) 
      jfin_s=(/  jend_nh,    jendv_nh,      jend_nh     /) 
      kdeb_s=(/ 1,    1,    0     /)
      kfin_s=(/ kmax, kmax, kmax  /)
!       ! Reception
      ideb_r=(/ istru_nh-1,     istr_nh-1,      istr_nh-1      /) 
      ifin_r=(/ istru_nh-1,     istr_nh-1,      istr_nh-1      /) 
      jdeb_r=(/  jend_nh+1,    jendv_nh+1,      jend_nh+1      /) 
      jfin_r=(/  jend_nh+1,    jendv_nh+1,      jend_nh+1      /) 
      kdeb_r=(/ 1,      1,      0      /)
      kfin_r=(/ kmax,   kmax,   kmax   /)
!      stop 'nordouest'
   case(nordest)
      ! Envoie
      ideb_s=(/ iendu_nh,  iend_nh, iend_nh    /) 
      ifin_s=(/ iendu_nh,  iend_nh, iend_nh    /) 
      jdeb_s=(/  jend_nh, jendv_nh, jend_nh     /) 
      jfin_s=(/  jend_nh, jendv_nh, jend_nh     /) 
      kdeb_s=(/ 1,    1,    0       /)
      kfin_s=(/ kmax, kmax, kmax    /)
      ! Reception
      ideb_r=(/ iendu_nh+1,  iend_nh+1, iend_nh+1 /) 
      ifin_r=(/ iendu_nh+1,  iend_nh+1, iend_nh+1 /) 
      jdeb_r=(/  jend_nh+1, jendv_nh+1, jend_nh+1 /) 
      jfin_r=(/  jend_nh+1, jendv_nh+1, jend_nh+1 /) 
      kdeb_r=(/ 1,      1,      0      /)
      kfin_r=(/ kmax,   kmax,   kmax   /)
!      stop 'nordest'
   case default
      call mpi_finalize(ierr)
      stop 'borne_echange_qdm_nbq_a voisin inconu'
   end select
    
     ! Si pas de voisins on incremente 
     if ( (WESTERN_EDGE).and.((voisin == nord).or.(voisin == sud)) ) then
	   ideb_s = (/ istru_nh-1,     istr_nh-1,      istr_nh-1 /)
	   ideb_r = (/ istru_nh-1,     istr_nh-1,      istr_nh-1 /)
     endif
     if ( (EASTERN_EDGE).and.((voisin == nord).or.(voisin == sud)) ) then
           ifin_s = (/ iendu_nh+1,  iend_nh+1, iend_nh+1 /)
           ifin_r = (/ iendu_nh+1,  iend_nh+1, iend_nh+1 /)
     endif
     if ( (SOUTHERN_EDGE).and.((voisin == ouest).or.(voisin == est)) ) then
           jdeb_s=(/  jstr_nh-1,    jstrv_nh-1,      jstr_nh-1  /)
           jdeb_r=(/  jstr_nh-1,    jstrv_nh-1,      jstr_nh-1  /)
     endif
     if ( (NORTHERN_EDGE).and.((voisin == ouest).or.(voisin == est)) ) then
           jfin_s=(/  jend_nh+1, jendv_nh+1, jend_nh+1 /)
           jfin_r=(/  jend_nh+1, jendv_nh+1, jend_nh+1 /)
     endif

end subroutine 	borne_echange_qdm_nbq_a  
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
subroutine get_index_ghost_qdm_nbq_a(ideb, ifin, jdeb, jfin, kdeb, kfin, &
			    l_index, ii)
  use module_nh , only : ijk2lmom_nh
  implicit none
  integer,dimension(3),intent(in) :: ideb,ifin,jdeb,jfin,kdeb,kfin
  integer,dimension(:),intent(inout) :: l_index
  integer,intent(out) :: ii
  integer :: var, i, j, k, l_nbq
     ii=0
     do var=1,3 ! u,v,w ==> i=3,2,2
      do i=ideb(var),ifin(var)
	do k=kdeb(var),kfin(var)
        do j=jdeb(var),jfin(var)
           l_nbq=ijk2lmom_nh(i,j,k,var)
!              write(200+par%rank,*) i,j,k," ---",ii," -- ",l_nbq
          if (l_nbq > 0) then
            ii=ii+1
            l_index(ii)=l_nbq
           endif
         enddo    
	 enddo
	 enddo
     enddo	 
end subroutine get_index_ghost_qdm_nbq_a
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
subroutine getblocks_qdm_nbq_a(voisin,imax,jmax,kmax,nbelt,nbblock,blockdeb,blocklength,sendrecv)
  use module_qsort
  implicit none
  integer, intent(in) :: voisin,nbelt,imax,jmax,kmax,sendrecv
  integer, intent(out) :: nbblock
  integer,dimension(nbelt), intent(out) :: blockdeb
  integer,dimension(nbelt), intent(out) :: blocklength
  !
  integer,dimension(nbelt) :: idi, idi_tri
  integer,dimension(3) :: ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s
  integer,dimension(3) :: ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r
  integer :: ii, bcl,bcl2
  integer :: intex
  !
  call borne_echange_qdm_nbq_a(voisin,imax,jmax,kmax, &
	  ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s, &
	  ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r )
	  
  ii=0
  if(sendrecv == 1) then !! sendrecv==1 => send
    call get_index_ghost_qdm_nbq_a(ideb_s, ifin_s,  &
                                  jdeb_s, jfin_s, kdeb_s, kfin_s, &
				  idi, ii)
   else
   call get_index_ghost_qdm_nbq_a(ideb_r, ifin_r,  &
                                  jdeb_r, jfin_r, kdeb_r, kfin_r, &
				  idi, ii)
  endif

  idi_tri(1:ii)=qsort(idi(1:ii)) !.......
!  write(200+par%rank,*) " ds  getblocks_qdm_nbq_a ii,nbelt=",ii,nbelt
!  do bcl=1,ii
!     write(300+par%Rank,*) bcl,idi_tri(bcl)
!  enddo
  call getblocks(ii,idi_tri,nbblock,blockdeb,blocklength)
!  write(200+par%rank,*) " ds  getblocks_qdm_nbq_a nbblock=",nbblock
  blockdeb(1:nbblock)=idi_tri(blockdeb(1:nbblock))-1
  blocklength = blocklength+1
!  write(200+par%rank,*) " ds  getblocks_qdm_nbq_a nbblock=",nbblock
end subroutine getblocks_qdm_nbq_a
!--------------------------------------------------------------------------


!--------------------------------------------------------------------------
subroutine create_echange_qdm_nbq_a(imax,jmax,kmax)
!
! Echanges des variables "boucle NBQ" quantité de mouvement :: 
! creation d'un type mpi d'echange
!
  use module_nh , only : ijk2lmom_nh, ijk2lq_nh, rhs2_nh,l2imom_nh,l2jmom_nh,l2kmom_nh, &
			 istr_nh,iend_nh,jstr_nh
!  use module_nbq
  use module_qsort
  implicit none
  integer,intent(in) :: imax,jmax,kmax
  integer :: bcl, bcl2
  integer :: nbblock
  integer,dimension((max(imax,jmax)+2)*(kmax+2)*4) :: blocklength
  integer,dimension((max(imax,jmax)+2)*(kmax+2)*4) :: blockdeb, blocktype
  integer :: ftrouestnb
  integer :: ftrnordnb
  integer,dimension(8) :: szmax 

  ftrouestnb=(max(imax,jmax+2))*(kmax+2)*4
  ftrnordnb =(max(imax,jmax)+2)*(kmax+2)*4
  szmax = (/ ftrouestnb, ftrouestnb, ftrnordnb, ftrnordnb, &
             ftrouestnb, ftrouestnb, ftrnordnb, ftrnordnb /)  
             
   do bcl=1, 8
!!Type Envoie
      nbblock=0
      call getblocks_qdm_nbq_a(liste_voisin(bcl), imax, jmax, kmax, &
 	        	      szmax(bcl),nbblock,blockdeb,blocklength,1)
!      if (par%tvoisin(bcl) /= MPI_PROC_NULL) then
!       write(200+par%rank,*) " ENVOI ds create_echange_qdm_nbq_a nbblock=",nbblock," voisin=",par%tvoisin(bcl)
!       do bcl2=1,nbblock
! 	  write(200+par%rank,*) "   deb,len=",blockdeb(bcl2),blocklength(bcl2)
!       enddo
!      endif
        call MPI_Type_indexed( nbblock, blocklength(1:nbblock),	blockdeb(1:nbblock), MPI_DOUBLE_PRECISION, &
		      ech_qdm_nbq(liste_voisin(bcl))%send,ierr)                        
      call mpi_type_commit(ech_qdm_nbq(liste_voisin(bcl))%send,ierr)
!!Type Reception
     call getblocks_qdm_nbq_a(liste_voisin(bcl), imax, jmax, kmax, &
	        	      szmax(bcl),nbblock,blockdeb,blocklength,0)
!      if (par%tvoisin(bcl) /= MPI_PROC_NULL) then
!       write(300+par%rank,*) " RECEPTION ds create_echange_qdm_nbq_a nbblock=",nbblock," voisin=",par%tvoisin(bcl)
!       do bcl2=1,nbblock
! 	  write(300+par%rank,*) "   deb,len=",blockdeb(bcl2),blocklength(bcl2)
!       enddo
!      endif
      call MPI_Type_indexed( nbblock, blocklength,	blockdeb, MPI_DOUBLE_PRECISION, &
		      ech_qdm_nbq(liste_voisin(bcl))%recv,ierr)                        
      call mpi_type_commit(ech_qdm_nbq(liste_voisin(bcl))%recv,ierr)
   enddo	
   
end subroutine create_echange_qdm_nbq_a
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
!     Echange U
!--------------------------------------------------------------------------
subroutine get_index_ghost_qdmU_nbq_a(ideb, ifin, jdeb, jfin, kdeb, kfin, &
			    l_index, ii)
  use module_nh , only : ijk2lmom_nh
  implicit none
  integer,dimension(3),intent(in) :: ideb,ifin,jdeb,jfin,kdeb,kfin
  integer,dimension(:),intent(inout) :: l_index
  integer,intent(out) :: ii
  integer :: var, i, j, k, l_nbq
     ii=0
     var=1 !,3 ! u,v,w ==> i=3,2,2
      do i=ideb(var),ifin(var)
	do k=kdeb(var),kfin(var)
        do j=jdeb(var),jfin(var)
           l_nbq=ijk2lmom_nh(i,j,k,var)
!              write(200+par%rank,*) i,j,k," ---",ii," -- ",l_nbq
          if (l_nbq > 0) then
            ii=ii+1
            l_index(ii)=l_nbq
           endif
         enddo    
	 enddo
      enddo	 
end subroutine get_index_ghost_qdmU_nbq_a
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
subroutine getblocks_qdmU_nbq_a(voisin,imax,jmax,kmax,nbelt,nbblock,blockdeb,blocklength,sendrecv)
  use module_qsort
  implicit none
  integer, intent(in) :: voisin,nbelt,imax,jmax,kmax,sendrecv
  integer, intent(out) :: nbblock
  integer,dimension(nbelt), intent(out) :: blockdeb
  integer,dimension(nbelt), intent(out) :: blocklength
  !
  integer,dimension(nbelt) :: idi, idi_tri
  integer,dimension(3) :: ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s
  integer,dimension(3) :: ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r
  integer :: ii, bcl,bcl2
  integer :: intex
  !
  call borne_echange_qdm_nbq_a(voisin,imax,jmax,kmax, &
	  ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s, &
	  ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r )
	  
  ii=0
  if(sendrecv == 1) then !! sendrecv==1 => send
    call get_index_ghost_qdmU_nbq_a(ideb_s, ifin_s,  &
                                  jdeb_s, jfin_s, kdeb_s, kfin_s, &
				  idi, ii)
   else
   call get_index_ghost_qdmU_nbq_a(ideb_r, ifin_r,  &
                                  jdeb_r, jfin_r, kdeb_r, kfin_r, &
				  idi, ii)
  endif

  idi_tri(1:ii)=qsort(idi(1:ii)) !.......
!  write(200+par%rank,*) " ds  getblocks_qdm_nbq_a ii,nbelt=",ii,nbelt
!  do bcl=1,ii
!     write(300+par%Rank,*) bcl,idi_tri(bcl)
!  enddo
  call getblocks(ii,idi_tri,nbblock,blockdeb,blocklength)
!  write(200+par%rank,*) " ds  getblocks_qdm_nbq_a nbblock=",nbblock
  blockdeb(1:nbblock)=idi_tri(blockdeb(1:nbblock))-1
  blocklength = blocklength+1
!  write(200+par%rank,*) " ds  getblocks_qdm_nbq_a nbblock=",nbblock
end subroutine getblocks_qdmU_nbq_a
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
subroutine create_echange_qdmU_nbq_a(imax,jmax,kmax)
!
! Echanges des variables "boucle NBQ" quantité de mouvement :: 
! creation d'un type mpi d'echange
!
  use module_nh , only : ijk2lmom_nh, ijk2lq_nh, rhs2_nh,l2imom_nh,l2jmom_nh,l2kmom_nh, &
			 istr_nh,iend_nh,jstr_nh
!  use module_nbq
  use module_qsort
  implicit none
  integer,intent(in) :: imax,jmax,kmax
  integer :: bcl, bcl2
  integer :: nbblock
  integer,dimension((max(imax,jmax)+2)*(kmax+2)*4) :: blocklength
  integer,dimension((max(imax,jmax)+2)*(kmax+2)*4) :: blockdeb, blocktype
  integer :: ftrouestnb
  integer :: ftrnordnb
  integer,dimension(8) :: szmax 

  ftrouestnb=(max(imax,jmax+2))*(kmax+2)*4
  ftrnordnb =(max(imax,jmax)+2)*(kmax+2)*4
  szmax = (/ ftrouestnb, ftrouestnb, ftrnordnb, ftrnordnb, &
             ftrouestnb, ftrouestnb, ftrnordnb, ftrnordnb /)  
             
   do bcl=1, 8
!!Type Envoie
      nbblock=0
      call getblocks_qdmU_nbq_a(liste_voisin(bcl), imax, jmax, kmax, &
 	        	      szmax(bcl),nbblock,blockdeb,blocklength,1)
!      if (par%tvoisin(bcl) /= MPI_PROC_NULL) then
!       write(300+par%rank,*) " ENVOI ds create_echange_qdmU_nbq_a nbblock=",nbblock," voisin=",par%tvoisin(bcl)
!       do bcl2=1,nbblock
! 	  write(300+par%rank,*) "   deb,len=",blockdeb(bcl2),blocklength(bcl2)
!       enddo
!      endif
        call MPI_Type_indexed( nbblock, blocklength(1:nbblock),	blockdeb(1:nbblock), MPI_DOUBLE_PRECISION, &
		      ech_qdmU_nbq(liste_voisin(bcl))%send,ierr)                        
      call mpi_type_commit(ech_qdmU_nbq(liste_voisin(bcl))%send,ierr)
!!Type Reception
     call getblocks_qdmU_nbq_a(liste_voisin(bcl), imax, jmax, kmax, &
	        	      szmax(bcl),nbblock,blockdeb,blocklength,0)
!      if (par%tvoisin(bcl) /= MPI_PROC_NULL) then
!       write(300+par%rank,*) " RECEPTION ds create_echange_qdm_nbq_a nbblock=",nbblock," voisin=",par%tvoisin(bcl)
!       do bcl2=1,nbblock
! 	  write(300+par%rank,*) "   deb,len=",blockdeb(bcl2),blocklength(bcl2)
!       enddo
!      endif
      call MPI_Type_indexed( nbblock, blocklength,	blockdeb, MPI_DOUBLE_PRECISION, &
		      ech_qdmU_nbq(liste_voisin(bcl))%recv,ierr)                        
      call mpi_type_commit(ech_qdmU_nbq(liste_voisin(bcl))%recv,ierr)
   enddo	
   
end subroutine create_echange_qdmU_nbq_a
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
!     Echange V
!--------------------------------------------------------------------------
subroutine get_index_ghost_qdmV_nbq_a(ideb, ifin, jdeb, jfin, kdeb, kfin, &
			    l_index, ii)
  use module_nh , only : ijk2lmom_nh
  implicit none
  integer,dimension(3),intent(in) :: ideb,ifin,jdeb,jfin,kdeb,kfin
  integer,dimension(:),intent(inout) :: l_index
  integer,intent(out) :: ii
  integer :: var, i, j, k, l_nbq
     ii=0
     var=2 !,3 ! u,v,w ==> i=3,2,2
      do i=ideb(var),ifin(var)
	do k=kdeb(var),kfin(var)
        do j=jdeb(var),jfin(var)
           l_nbq=ijk2lmom_nh(i,j,k,var)
!              write(200+par%rank,*) i,j,k," ---",ii," -- ",l_nbq
          if (l_nbq > 0) then
            ii=ii+1
            l_index(ii)=l_nbq
           endif
         enddo    
	 enddo
      enddo	 
end subroutine get_index_ghost_qdmV_nbq_a
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
subroutine getblocks_qdmV_nbq_a(voisin,imax,jmax,kmax,nbelt,nbblock,blockdeb,blocklength,sendrecv)
  use module_qsort
  implicit none
  integer, intent(in) :: voisin,nbelt,imax,jmax,kmax,sendrecv
  integer, intent(out) :: nbblock
  integer,dimension(nbelt), intent(out) :: blockdeb
  integer,dimension(nbelt), intent(out) :: blocklength
  !
  integer,dimension(nbelt) :: idi, idi_tri
  integer,dimension(3) :: ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s
  integer,dimension(3) :: ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r
  integer :: ii, bcl,bcl2
  integer :: intex
  !
  call borne_echange_qdm_nbq_a(voisin,imax,jmax,kmax, &
	  ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s, &
	  ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r )
	  
  ii=0
  if(sendrecv == 1) then !! sendrecv==1 => send
    call get_index_ghost_qdmV_nbq_a(ideb_s, ifin_s,  &
                                  jdeb_s, jfin_s, kdeb_s, kfin_s, &
				  idi, ii)
   else
   call get_index_ghost_qdmV_nbq_a(ideb_r, ifin_r,  &
                                  jdeb_r, jfin_r, kdeb_r, kfin_r, &
				  idi, ii)
  endif

  idi_tri(1:ii)=qsort(idi(1:ii)) !.......
!  write(200+par%rank,*) " ds  getblocks_qdm_nbq_a ii,nbelt=",ii,nbelt
!  do bcl=1,ii
!     write(300+par%Rank,*) bcl,idi_tri(bcl)
!  enddo
  call getblocks(ii,idi_tri,nbblock,blockdeb,blocklength)
!  write(200+par%rank,*) " ds  getblocks_qdm_nbq_a nbblock=",nbblock
  blockdeb(1:nbblock)=idi_tri(blockdeb(1:nbblock))-1
  blocklength = blocklength+1
!  write(200+par%rank,*) " ds  getblocks_qdm_nbq_a nbblock=",nbblock
end subroutine getblocks_qdmV_nbq_a
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
subroutine create_echange_qdmV_nbq_a(imax,jmax,kmax)
!
! Echanges des variables "boucle NBQ" quantité de mouvement :: 
! creation d'un type mpi d'echange
!
  use module_nh , only : ijk2lmom_nh, ijk2lq_nh, rhs2_nh,l2imom_nh,l2jmom_nh,l2kmom_nh, &
			 istr_nh,iend_nh,jstr_nh
!  use module_nbq
  use module_qsort
  implicit none
  integer,intent(in) :: imax,jmax,kmax
  integer :: bcl, bcl2
  integer :: nbblock
  integer,dimension((max(imax,jmax)+2)*(kmax+2)*4) :: blocklength
  integer,dimension((max(imax,jmax)+2)*(kmax+2)*4) :: blockdeb, blocktype
  integer :: ftrouestnb
  integer :: ftrnordnb
  integer,dimension(8) :: szmax 

  ftrouestnb=(max(imax,jmax+2))*(kmax+2)*4
  ftrnordnb =(max(imax,jmax)+2)*(kmax+2)*4
  szmax = (/ ftrouestnb, ftrouestnb, ftrnordnb, ftrnordnb, &
             ftrouestnb, ftrouestnb, ftrnordnb, ftrnordnb /)  
             
   do bcl=1, 8
!!Type Envoie
      nbblock=0
      call getblocks_qdmV_nbq_a(liste_voisin(bcl), imax, jmax, kmax, &
 	        	      szmax(bcl),nbblock,blockdeb,blocklength,1)
!      if (par%tvoisin(bcl) /= MPI_PROC_NULL) then
!       write(400+par%rank,*) " ENVOI ds create_echange_qdmV_nbq_a nbblock=",nbblock," voisin=",par%tvoisin(bcl)
!       do bcl2=1,nbblock
! 	  write(400+par%rank,*) "   deb,len=",blockdeb(bcl2),blocklength(bcl2)
!       enddo
!      endif
        call MPI_Type_indexed( nbblock, blocklength(1:nbblock),	blockdeb(1:nbblock), MPI_DOUBLE_PRECISION, &
		      ech_qdmV_nbq(liste_voisin(bcl))%send,ierr)                        
      call mpi_type_commit(ech_qdmV_nbq(liste_voisin(bcl))%send,ierr)
!!Type Reception
     call getblocks_qdmV_nbq_a(liste_voisin(bcl), imax, jmax, kmax, &
	        	      szmax(bcl),nbblock,blockdeb,blocklength,0)
!      if (par%tvoisin(bcl) /= MPI_PROC_NULL) then
!       write(300+par%rank,*) " RECEPTION ds create_echange_qdm_nbq_a nbblock=",nbblock," voisin=",par%tvoisin(bcl)
!       do bcl2=1,nbblock
! 	  write(300+par%rank,*) "   deb,len=",blockdeb(bcl2),blocklength(bcl2)
!       enddo
!      endif
      call MPI_Type_indexed( nbblock, blocklength,	blockdeb, MPI_DOUBLE_PRECISION, &
		      ech_qdmV_nbq(liste_voisin(bcl))%recv,ierr)                        
      call mpi_type_commit(ech_qdmV_nbq(liste_voisin(bcl))%recv,ierr)
   enddo	
   
end subroutine create_echange_qdmV_nbq_a
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
!     Echange W
!--------------------------------------------------------------------------
subroutine get_index_ghost_qdmW_nbq_a(ideb, ifin, jdeb, jfin, kdeb, kfin, &
			    l_index, ii)
  use module_nh , only : ijk2lmom_nh
  implicit none
  integer,dimension(3),intent(in) :: ideb,ifin,jdeb,jfin,kdeb,kfin
  integer,dimension(:),intent(inout) :: l_index
  integer,intent(out) :: ii
  integer :: var, i, j, k, l_nbq
     ii=0
     var=3 !,3 ! u,v,w ==> i=3,2,2
      do i=ideb(var),ifin(var)
	do k=kdeb(var),kfin(var)
        do j=jdeb(var),jfin(var)
           l_nbq=ijk2lmom_nh(i,j,k,var)
!              write(200+par%rank,*) i,j,k," ---",ii," -- ",l_nbq
          if (l_nbq > 0) then
            ii=ii+1
            l_index(ii)=l_nbq
           endif
         enddo    
	 enddo
      enddo 
end subroutine get_index_ghost_qdmW_nbq_a
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
subroutine getblocks_qdmW_nbq_a(voisin,imax,jmax,kmax,nbelt,nbblock,blockdeb,blocklength,sendrecv)
  use module_qsort
  implicit none
  integer, intent(in) :: voisin,nbelt,imax,jmax,kmax,sendrecv
  integer, intent(out) :: nbblock
  integer,dimension(nbelt), intent(out) :: blockdeb
  integer,dimension(nbelt), intent(out) :: blocklength
  !
  integer,dimension(nbelt) :: idi, idi_tri
  integer,dimension(3) :: ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s
  integer,dimension(3) :: ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r
  integer :: ii, bcl,bcl2
  integer :: intex
  !
  call borne_echange_qdm_nbq_a(voisin,imax,jmax,kmax, &
	  ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s, &
	  ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r )
	  
  ii=0
  if(sendrecv == 1) then !! sendrecv==1 => send
    call get_index_ghost_qdmW_nbq_a(ideb_s, ifin_s,  &
                                  jdeb_s, jfin_s, kdeb_s, kfin_s, &
				  idi, ii)
   else
   call get_index_ghost_qdmW_nbq_a(ideb_r, ifin_r,  &
                                  jdeb_r, jfin_r, kdeb_r, kfin_r, &
				  idi, ii)
  endif

  idi_tri(1:ii)=qsort(idi(1:ii)) !.......
!  write(200+par%rank,*) " ds  getblocks_qdm_nbq_a ii,nbelt=",ii,nbelt
!  do bcl=1,ii
!     write(300+par%Rank,*) bcl,idi_tri(bcl)
!  enddo
  call getblocks(ii,idi_tri,nbblock,blockdeb,blocklength)
!  write(200+par%rank,*) " ds  getblocks_qdm_nbq_a nbblock=",nbblock
  blockdeb(1:nbblock)=idi_tri(blockdeb(1:nbblock))-1
  blocklength = blocklength+1
!  write(200+par%rank,*) " ds  getblocks_qdm_nbq_a nbblock=",nbblock
end subroutine getblocks_qdmW_nbq_a
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
subroutine create_echange_qdmW_nbq_a(imax,jmax,kmax)
!
! Echanges des variables "boucle NBQ" quantité de mouvement :: 
! creation d'un type mpi d'echange
!
  use module_nh , only : ijk2lmom_nh, ijk2lq_nh, rhs2_nh,l2imom_nh,l2jmom_nh,l2kmom_nh, &
			 istr_nh,iend_nh,jstr_nh
!  use module_nbq
  use module_qsort
  implicit none
  integer,intent(in) :: imax,jmax,kmax
  integer :: bcl, bcl2
  integer :: nbblock
  integer,dimension((max(imax,jmax)+2)*(kmax+2)*4) :: blocklength
  integer,dimension((max(imax,jmax)+2)*(kmax+2)*4) :: blockdeb, blocktype
  integer :: ftrouestnb
  integer :: ftrnordnb
  integer,dimension(8) :: szmax 

  ftrouestnb=(max(imax,jmax+2))*(kmax+2)*4
  ftrnordnb =(max(imax,jmax)+2)*(kmax+2)*4
  szmax = (/ ftrouestnb, ftrouestnb, ftrnordnb, ftrnordnb, &
             ftrouestnb, ftrouestnb, ftrnordnb, ftrnordnb /)  
             
   do bcl=1, 8
!!Type Envoie
      nbblock=0
      call getblocks_qdmW_nbq_a(liste_voisin(bcl), imax, jmax, kmax, &
 	        	      szmax(bcl),nbblock,blockdeb,blocklength,1)
!      if (par%tvoisin(bcl) /= MPI_PROC_NULL) then
!       write(500+par%rank,*) " ENVOI ds create_echange_qdmW_nbq_a nbblock=",nbblock," voisin=",par%tvoisin(bcl)
!       do bcl2=1,nbblock
! 	  write(500+par%rank,*) "   deb,len=",blockdeb(bcl2),blocklength(bcl2)
!       enddo
!      endif
        call MPI_Type_indexed( nbblock, blocklength(1:nbblock),	blockdeb(1:nbblock), MPI_DOUBLE_PRECISION, &
		      ech_qdmW_nbq(liste_voisin(bcl))%send,ierr)                        
      call mpi_type_commit(ech_qdmW_nbq(liste_voisin(bcl))%send,ierr)
!!Type Reception
     call getblocks_qdmW_nbq_a(liste_voisin(bcl), imax, jmax, kmax, &
	        	      szmax(bcl),nbblock,blockdeb,blocklength,0)
!      if (par%tvoisin(bcl) /= MPI_PROC_NULL) then
!       write(300+par%rank,*) " RECEPTION ds create_echange_qdm_nbq_a nbblock=",nbblock," voisin=",par%tvoisin(bcl)
!       do bcl2=1,nbblock
! 	  write(300+par%rank,*) "   deb,len=",blockdeb(bcl2),blocklength(bcl2)
!       enddo
!      endif
      call MPI_Type_indexed( nbblock, blocklength,	blockdeb, MPI_DOUBLE_PRECISION, &
		      ech_qdmW_nbq(liste_voisin(bcl))%recv,ierr)                        
      call mpi_type_commit(ech_qdmW_nbq(liste_voisin(bcl))%recv,ierr)
   enddo	
   
end subroutine create_echange_qdmW_nbq_a
!--------------------------------------------------------------------------



!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!                 ROUTINES DIV_NBQ
!-------------------------------------------------------------------------------------
!-------------------------------------------------------------------------------------
!--------------------------------------------------------------------------
subroutine borne_echange_div_nbq_a(voisin,imax,jmax,kmax, &
	  ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s, &
	  ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r )
  use module_nh , only : istr_nh,iend_nh,jstr_nh,jend_nh, &
	    istru_nh,iendu_nh,jstrv_nh,jendv_nh
!  use module_parallele 	  
  implicit none
  integer,intent(in) :: voisin	  
  integer,intent(in) :: imax,jmax,kmax
  integer,dimension(3),intent(out) :: ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s
  integer,dimension(3),intent(out) :: ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r
  
!     write(3200+par%rank,*) "------------------------------------------------"
!     write(3200+par%rank,*) "---- ",voisin," ---"
!     write(3200+par%rank,*) "------------------------------------------------"
!   
!     write(3200+par%rank,*) "borne_echange_div_nbq_a"
!     write(3200+par%rank,*) "istr_nh,iend_nh=",istr_nh,iend_nh
!     write(3200+par%rank,*) "jstr_nh,jend_nh=",jstr_nh,jend_nh
!     write(3200+par%rank,*) "istru_nh,iendu_nh=",istru_nh,iendu_nh
!     write(3200+par%rank,*) "jstrv_nh,jendv_nh=",jstrv_nh,jendv_nh
! !   write(6000+par%rank,*) "borne_echange_qdm_nbq_a voisin:",voisin
  select case(voisin)
   case(ouest) !........................
      ! Envoie
      ideb_s=(/ istr_nh, 0, 0 /); jdeb_s=(/ jstr_nh, 0, 0 /);     kdeb_s=(/ 1   , 0, 0 /) 
      ifin_s=(/ istr_nh, 0, 0 /); jfin_s=(/ jend_nh, 0, 0 /);     kfin_s=(/ kmax, 0, 0 /)
      
      ! Reception
      ideb_r=(/ istr_nh-1, 0, 0 /); jdeb_r=(/ jstr_nh, 0, 0 /);     kdeb_r=(/ 1 ,   0, 0 /)
      ifin_r=(/ istr_nh-1, 0, 0 /); jfin_r=(/ jend_nh, 0, 0 /);     kfin_r=(/ kmax, 0, 0 /)

   case(est)
      ! Envoie
      ideb_s=(/ iend_nh,   0, 0  /); jdeb_s=(/ jstr_nh,    0, 0 /);   kdeb_s=(/ 1,    0, 0 /)
      ifin_s=(/ iend_nh,   0, 0  /); jfin_s=(/ jend_nh, 0, 0 /);   kfin_s=(/ kmax, 0, 0 /)
      ! Reception
      ideb_r=(/ iend_nh+1, 0, 0  /); jdeb_r=(/ jstr_nh,    0, 0 /);   kdeb_r=(/ 1,    0, 0 /) 
      ifin_r=(/ iend_nh+1, 0, 0  /); jfin_r=(/ jend_nh, 0, 0 /);   kfin_r=(/ kmax, 0, 0 /)
           
  case(sud)
      ! Envoie
      ideb_s=(/ istr_nh,    0, 0 /);  jdeb_s=(/ jstr_nh, 0, 0  /);kdeb_s=(/ 1,    0, 0 /)
      ifin_s=(/ iend_nh,    0, 0 /);  jfin_s=(/ jstr_nh, 0, 0  /);kfin_s=(/ kmax, 0, 0 /)    
      ! Reception
      ideb_r=(/ istr_nh,    0, 0 /);  jdeb_r=(/ jstr_nh-1, 0, 0  /);kdeb_r=(/ 1,    0, 0 /)
      ifin_r=(/ iend_nh,    0, 0 /);  jfin_r=(/ jstr_nh-1, 0, 0  /);kfin_r=(/ kmax, 0, 0 /)

  case(nord)
      ! Envoie
      ideb_s=(/ istr_nh,    0, 0 /); jdeb_s=(/ jend_nh,    0, 0 /);kdeb_s=(/ 1,    0, 0 /)
      ifin_s=(/ iend_nh,    0, 0 /); jfin_s=(/ jend_nh,    0, 0 /);kfin_s=(/ kmax, 0, 0 /)
!       ! Reception
      ideb_r=(/ istr_nh,    0, 0 /);  jdeb_r=(/ jend_nh+1, 0, 0 /); kdeb_r=(/ 1,    0, 0 /)
      ifin_r=(/ iend_nh,    0, 0 /);  jfin_r=(/ jend_nh+1, 0, 0 /); kfin_r=(/ kmax, 0, 0 /)
      
       
  case(sudouest) 
!       ! Envoie
      ideb_s=(/ istr_nh,   0, 0 /);jdeb_s=(/ jstr_nh, 0, 0 /);kdeb_s=(/ 1   , 0, 0 /)
      ifin_s=(/ istr_nh,   0, 0 /);jfin_s=(/ jstr_nh, 0, 0 /);kfin_s=(/ kmax, 0, 0 /)
!       ! Reception
      ideb_r=(/ istr_nh-1, 0, 0 /);jdeb_r=(/ jstr_nh-1, 0, 0 /);kdeb_r=(/ 1,    0, 0 /)
      ifin_r=(/ istr_nh-1, 0, 0 /);jfin_r=(/ jstr_nh-1, 0, 0 /);kfin_r=(/ kmax, 0, 0 /)
      
   case(sudest)
       ! Envoie
      ideb_s=(/ iend_nh,   0, 0 /);  jdeb_s=(/ jstr_nh, 0, 0 /); kdeb_s=(/    1, 0, 0 /)
      ifin_s=(/ iend_nh,   0, 0 /);  jfin_s=(/ jstr_nh, 0, 0 /); kfin_s=(/ kmax, 0, 0 /)   
!      ! Reception
      ideb_r=(/ iend_nh+1, 0, 0 /);jdeb_r=(/ jstr_nh-1, 0, 0 /); kdeb_r=(/ 1,    0, 0 /)
      ifin_r=(/ iend_nh+1, 0, 0 /);jfin_r=(/ jstr_nh-1, 0, 0 /); kfin_r=(/ kmax, 0, 0 /)
 
   case(nordouest)
!       ! Envoie
      ideb_s=(/ istr_nh,   0, 0 /);jdeb_s=(/ jend_nh,   0, 0 /);kdeb_s=(/ 1,    0, 0 /)
      ifin_s=(/ istr_nh,   0, 0 /);jfin_s=(/ jend_nh,   0, 0 /);kfin_s=(/ kmax, 0, 0 /)     
!       ! Reception
      ideb_r=(/ istr_nh-1, 0, 0 /);jdeb_r=(/ jend_nh+1, 0, 0 /);kdeb_r=(/ 1,    0, 0 /)
      ifin_r=(/ istr_nh-1, 0, 0 /);jfin_r=(/ jend_nh+1, 0, 0 /);kfin_r=(/ kmax, 0, 0 /)

   case(nordest)
      ! Envoie
      ideb_s=(/ iend_nh, 0,   0 /);jdeb_s=(/ jend_nh,   0, 0 /);kdeb_s=(/ 1,     0, 0 /)
      ifin_s=(/ iend_nh, 0,   0 /);jfin_s=(/ jend_nh,   0, 0 /);kfin_s=(/ kmax,  0, 0 /)
!     ! Reception
      ideb_r=(/ iend_nh+1, 0, 0 /);jdeb_r=(/ jend_nh+1, 0, 0 /);kdeb_r=(/ 1,     0, 0 /)
      ifin_r=(/ iend_nh+1, 0, 0 /);jfin_r=(/ jend_nh+1, 0, 0 /); kfin_r=(/ kmax, 0, 0 /)

   case default
      call mpi_finalize(ierr)
      stop 'borne_echange_qdm_nbq_a voisin inconu'
   end select
     ! Si pas de voisins on incremente pour CDL
     if ( (WESTERN_EDGE).and.((voisin == nord).or.(voisin == sud)) ) then
	  ideb_s = (/ istr_nh-1, 0, 0 /)
	  ideb_r = (/ istr_nh-1, 0, 0 /)
     endif
     if ( (EASTERN_EDGE).and.((voisin == nord).or.(voisin == sud)) ) then
           ifin_s = (/ istr_nh+1, 0, 0 /)
           ifin_r = (/ istr_nh+1, 0, 0 /)
     endif
     if ( (SOUTHERN_EDGE).and.((voisin == ouest).or.(voisin == est)) ) then
         jdeb_s=(/ jstr_nh-1, 0, 0 /)
         jdeb_r=(/ jstr_nh-1, 0, 0 /)
     endif
     if ( (NORTHERN_EDGE).and.((voisin == ouest).or.(voisin == est)) ) then
         jfin_s=(/ jend_nh+1, 0, 0 /)
         jfin_r=(/ jend_nh+1, 0, 0 /)
     endif
end subroutine 	borne_echange_div_nbq_a  
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
subroutine get_index_ghost_div_nbq_a(ideb, ifin, jdeb, jfin, kdeb, kfin, &
			    l_index, ii)
  use module_nh , only : ijk2lq_nh
  implicit none
  integer,dimension(3),intent(in) :: ideb,ifin,jdeb,jfin,kdeb,kfin
  integer,dimension(:),intent(inout) :: l_index
  integer,intent(out) :: ii
  integer :: var, i, j, k, l_nbq
     ii=0
     var=1 !! inutile mais pour garder la me syntax que get_index_ghost_qdm_nbq_a
!     write(3200+par%rank,*) "ideb(var),ifin(var)=",ideb(var),ifin(var)
!     write(3200+par%rank,*) "jdeb(var),jfin(var)=",jdeb(var),jfin(var)
!     write(3200+par%rank,*) "kdeb(var),kfin(var)=",kdeb(var),kfin(var)
     do i=ideb(var),ifin(var)
        do k=kdeb(var),kfin(var)
           do j=jdeb(var),jfin(var)
              l_nbq=ijk2lq_nh(i,j,k)
              if (l_nbq > 0) then
                  ii=ii+1
                  l_index(ii)=l_nbq  
!                    write(3200+par%rank,*) i,j,k,":",ii,l_index(ii)
!                else
!  		   write(3200+par%rank,*) i,j,k,":",l_nbq,"--------------------------"
              endif
           enddo    
        enddo
      enddo
!      write(3200+par%rank,*)
end subroutine get_index_ghost_div_nbq_a
!--------------------------------------------------------------------------

!--------------------------------------------------------------------------
subroutine getblocks_div_nbq_a(voisin,imax,jmax,kmax,nbelt,nbblock,blockdeb,blocklength,sendrecv)
  use module_qsort
  implicit none
  integer, intent(in) :: voisin,nbelt,imax,jmax,kmax,sendrecv
  integer, intent(out) :: nbblock
  integer,dimension(nbelt), intent(out) :: blockdeb
  integer,dimension(nbelt), intent(out) :: blocklength
  !
  integer,dimension(nbelt) :: idi, idi_tri
  integer,dimension(3) :: ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s
  integer,dimension(3) :: ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r
  integer :: ii, bcl,bcl2
  integer :: intex
  !
  nbblock=0
  call borne_echange_div_nbq_a(voisin,imax,jmax,kmax, &
	  ideb_s,ifin_s,jdeb_s,jfin_s,kdeb_s,kfin_s, &
	  ideb_r,ifin_r,jdeb_r,jfin_r,kdeb_r,kfin_r )
	  
  ii=0
  if(sendrecv == 1) then !! sendrecv==1 => send
!    write(800+par%rank,*) "Send"
!     write(3200+par%rank,*) "ENVOI.........................."
    call get_index_ghost_div_nbq_a(ideb_s, ifin_s,  &
                                  jdeb_s, jfin_s, kdeb_s, kfin_s, &
				  idi, ii)  
   else
!    write(800+par%rank,*) "Recv"
!     write(3200+par%rank,*) "RECEPTION........................."
   call get_index_ghost_div_nbq_a(ideb_r, ifin_r,  &
                                  jdeb_r, jfin_r, kdeb_r, kfin_r, &
				  idi, ii)
  endif

  idi_tri(1:ii)=qsort(idi(1:ii)) !.......

  call getblocks(ii,idi_tri,nbblock,blockdeb,blocklength)				 
  blockdeb(1:nbblock)=(idi_tri(blockdeb(1:nbblock))-1)
  blocklength = (blocklength+1)
end subroutine getblocks_div_nbq_a

!--------------------------------------------------------------------------
subroutine create_echange_div_nbq_a(imax,jmax,kmax)
!
! Echanges des variables "boucle NBQ" divergence :: 
! creation d'un type mpi d'echange
!
  use module_nh , only : ijk2lmom_nh, ijk2lq_nh, rhs2_nh,l2imom_nh,l2jmom_nh,l2kmom_nh
!  use module_nbq
  use module_qsort
  implicit none
  integer,intent(in) :: imax,jmax,kmax
  integer :: bcl,bcl2
  integer :: nbblock
  integer,dimension((max(imax,jmax)+2)*(kmax+2)*4) :: blocklength
  integer,dimension((max(imax,jmax)+2)*(kmax+2)*4) :: blockdeb, blocktype
  integer :: ftrouestnb
  integer :: ftrnordnb
  integer,dimension(8) :: szmax 

  ftrouestnb=(max(imax,jmax+2))*(kmax+2)*4
  ftrnordnb =(max(imax,jmax)+2)*(kmax+2)*4
  szmax = (/ ftrouestnb, ftrouestnb, ftrnordnb, ftrnordnb, &
             ftrouestnb, ftrouestnb, ftrnordnb, ftrnordnb /)  
             
   do bcl=1, 8 
!!Type Envoie
      call getblocks_div_nbq_a(liste_voisin(bcl), imax, jmax, kmax, &
 	        	      szmax(bcl),nbblock,blockdeb,blocklength,1)
     if (par%tvoisin(bcl) /= MPI_PROC_NULL) then
!       write(200+par%rank,*) " ds create_echange_div_nbq_a nbblock=",nbblock," voisin=",bcl
!       do bcl2=1,nbblock
! 	  write(200+par%rank,*) "   deb,len=",blockdeb(bcl2),blocklength(bcl2)
!       enddo
      endif
!       write(200+par%rank,*) "Send:"
!       write(200+par%rank,*) "nbblock=",nbblock," voisin=",bcl," par%tvoisin(bcl)=",par%tvoisin(bcl)
!       do bcl2=1,nbblock
!          write(200+par%rank,*) "   deb,len=",blockdeb(bcl2),blocklength(bcl2)
!       enddo
      
      call MPI_Type_indexed( nbblock, blocklength(1:nbblock),	blockdeb(1:nbblock), MPI_DOUBLE_PRECISION, &
		      ech_div_nbq(liste_voisin(bcl))%send,ierr)                        
      call mpi_type_commit(ech_div_nbq(liste_voisin(bcl))%send,ierr)
!       write(200+par%rank,*) ech_div_nbq(liste_voisin(bcl))%send
!       write(200+par%rank,*) 
      
!      write(200+par%rank,*) "Envoie ech_div_nbq, liste_voisin(bcl)=",liste_voisin(bcl),ech_div_nbq(liste_voisin(bcl))%send
!!Type Reception
     call getblocks_div_nbq_a(liste_voisin(bcl), imax, jmax, kmax, &
	        	      szmax(bcl),nbblock,blockdeb,blocklength,0)
!       write(200+par%rank,*) "Recv:"
!       write(200+par%rank,*) "nbblock=",nbblock," voisin=",bcl
!       do bcl2=1,nbblock
!          write(200+par%rank,*) "   deb,len=",blockdeb(bcl),blocklength(bcl)
!       enddo
      if (par%tvoisin(bcl) /= MPI_PROC_NULL) then
!       write(300+par%rank,*) " ds create_echange_div_nbq_a nbblock=",nbblock," voisin=",bcl
      do bcl2=1,nbblock
! 	  write(300+par%rank,*) "   deb,len=",blockdeb(bcl2),blocklength(bcl2)
      enddo
      endif
     call MPI_Type_indexed( nbblock, blocklength,	blockdeb, MPI_DOUBLE_PRECISION, &
		      ech_div_nbq(liste_voisin(bcl))%recv,ierr)                        
      call mpi_type_commit(ech_div_nbq(liste_voisin(bcl))%recv,ierr)
!       write(200+par%rank,*) "RECV ech_div_nbq, liste_voisin(bcl)=",liste_voisin(bcl),ech_div_nbq(liste_voisin(bcl))%recv
!       write(200+par%rank,*) ech_div_nbq(liste_voisin(bcl))%recv
!       write(200+par%rank,*) 
   enddo	
   
end subroutine create_echange_div_nbq_a
!--------------------------------------------------------------------------


end module module_parallel_nbq

#else
 module module_parallel_nbq
 end module module_parallel_nbq
#endif
