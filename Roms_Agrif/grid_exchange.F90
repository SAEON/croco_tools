#include "cppdefs.h"
subroutine grid_exchange
#if defined NBQ  && defined MPI
  use module_parallel_nbq
  use module_nh
  implicit none
  integer,parameter :: TAG1=1000, TAG2=2000,   TAG3=3000,  TAG4=4000
  integer,parameter :: TAG5=5000, TAG6=6000,   TAG7=7000,   TAG8=8000
  integer,parameter :: TAG9=9000, TAG10=10000

  integer,parameter :: TAG11=11000, TAG12=12000,   TAG13=13000,  TAG14=14000
  integer,parameter :: TAG15=15000, TAG16=16000,   TAG17=17000,  TAG18=18000
  integer,parameter :: TAG19=19000, TAG20=20000

  integer,parameter :: TAG21=21000, TAG22=22000,   TAG23=23000,  TAG24=24000
  integer,parameter :: TAG25=25000, TAG26=26000,   TAG27=27000,  TAG28=28000
  integer,parameter :: TAG29=29000, TAG30=30000
  double precision,dimension(istru_nh-1:iend_nh+1) :: ftr_ouest
  integer,dimension(256) 		:: tabreq
  integer,dimension(MPI_STATUS_SIZE,256) :: tstatus
  integer 				:: nbreq, ierror
  
  
!**********************************************************************
!
! MPI NEEDS:
!
! gdepth_u, coefa_u and coefb_u: 
!                we need here ( istru_nh-1,[jstr_nh:jend_nh],[0:N+1] )
!                             ( iendu_nh+1,[jstr_nh:jend_nh],[0:N+1] )               
! gdepth_v, coefa_v and coefb_v: 
!                we need here ( [istr_nh:iend_nh],jstrv_nh-1,[0:N+1] )
!                             ( [istr_nh:iend_nh],jendv_nh+1,[0:N+1] ) 
!  
!**********************************************************************
if (doalloc) then
    ! U variables
    ! ---  gdepth_u --- ouest-est
    allocate(gdepth_uS%ouest(jend_nh-jstr_nh+1,N+2))
    allocate(  gdepth_uS%est(jend_nh-jstr_nh+1,N+2))
    allocate(gdepth_uR%ouest(jend_nh-jstr_nh+1,N+2))
    allocate(  gdepth_uR%est(jend_nh-jstr_nh+1,N+2))
    ! ---  gdepth_u --- sud-nord
    allocate(gdepth_uS%sud(iendu_nh-istru_nh+1,N+2))
    allocate(  gdepth_uS%nord(iendu_nh-istru_nh+1,N+2))
    allocate(gdepth_uR%sud(iendu_nh-istru_nh+1,N+2))
    allocate(  gdepth_uR%nord(iendu_nh-istru_nh+1,N+2))
    ! ---  coefa_u --- ouest-est
    allocate(coefa_uS%ouest(jend_nh-jstr_nh+1,N+2))
    allocate( coefa_uS%est (jend_nh-jstr_nh+1,N+2))
    allocate(coefa_uR%ouest(jend_nh-jstr_nh+1,N+2))
    allocate(  coefa_uR%est(jend_nh-jstr_nh+1,N+2))
    ! ---  coefa_u --- sud-nord
    allocate(coefa_uS%sud(iendu_nh-istru_nh+1,N+2))
    allocate( coefa_uS%nord (iendu_nh-istru_nh+1,N+2))
    allocate(coefa_uR%sud(iendu_nh-istru_nh+1,N+2))
    allocate(  coefa_uR%nord(iendu_nh-istru_nh+1,N+2))
    ! ---  coefb_u --- ouest-est
    allocate(coefb_uS%ouest(jend_nh-jstr_nh+1,N+2))
    allocate(  coefb_uS%est(jend_nh-jstr_nh+1,N+2))
    allocate(coefb_uR%ouest(jend_nh-jstr_nh+1,N+2))
    allocate(  coefb_uR%est(jend_nh-jstr_nh+1,N+2))
    ! ---  coefb_u --- sud-nord
    allocate(coefb_uS%sud(iendu_nh-istru_nh+1,N+2))
    allocate(  coefb_uS%nord(iendu_nh-istru_nh+1,N+2))
    allocate(coefb_uR%sud(iendu_nh-istru_nh+1,N+2))
    allocate(  coefb_uR%nord(iendu_nh-istru_nh+1,N+2))
    sz1 = (jend_nh-jstr_nh+1)*(N+2)   ! WEST-EST boundary
    sz3 = (iendu_nh-istru_nh+1)*(N+2) ! SUD-NORTH boundary
    ! V variables
    ! ---  gdepth_v --- ouest-est
    allocate(gdepth_vS%ouest(jendv_nh-jstrv_nh+1,N+2))
    allocate(  gdepth_vS%est(jendv_nh-jstrv_nh+1,N+2))
    allocate(gdepth_vR%ouest(jendv_nh-jstrv_nh+1,N+2))
    allocate(  gdepth_vR%est(jendv_nh-jstrv_nh+1,N+2))
    ! ---  gdepth_v --- sud-nord
    allocate(gdepth_vS%sud(iend_nh-istr_nh+1,N+2))
    allocate(  gdepth_vS%nord(iend_nh-istr_nh+1,N+2))
    allocate(gdepth_vR%sud(iend_nh-istr_nh+1,N+2))
    allocate(  gdepth_vR%nord(iend_nh-istr_nh+1,N+2))
    ! ---  coefa_u --- ouest-est
    allocate(coefa_vS%ouest(jendv_nh-jstrv_nh+1,N+2))
    allocate(  coefa_vS%est(jendv_nh-jstrv_nh+1,N+2))
    allocate(coefa_vR%ouest(jendv_nh-jstrv_nh+1,N+2))
    allocate(  coefa_vR%est(jendv_nh-jstrv_nh+1,N+2))
    ! ---  coefa_u --- sud-nord
    allocate(coefa_vS%sud(iend_nh-istr_nh+1,N+2))
    allocate(  coefa_vS%nord(iend_nh-istr_nh+1,N+2))
    allocate(coefa_vR%sud(iend_nh-istr_nh+1,N+2))
    allocate(  coefa_vR%nord(iend_nh-istr_nh+1,N+2))
    ! ---  coefb_u --- ouest-est
    allocate(coefb_vS%ouest(jendv_nh-jstrv_nh+1,N+2))
    allocate(  coefb_vS%est(jendv_nh-jstrv_nh+1,N+2))
    allocate(coefb_vR%ouest(jendv_nh-jstrv_nh+1,N+2))
    allocate(  coefb_vR%est(jendv_nh-jstrv_nh+1,N+2))
    ! ---  coefb_u --- sud-nord
    allocate(coefb_vS%sud(iend_nh-istr_nh+1,N+2))
    allocate(  coefb_vS%nord(iend_nh-istr_nh+1,N+2))
    allocate(coefb_vR%sud(iend_nh-istr_nh+1,N+2))
    allocate(  coefb_vR%nord(iend_nh-istr_nh+1,N+2))
    sz2 = (jendv_nh-jstrv_nh+1)*(N+2) ! WEST-EST boundary
    sz4 = (iend_nh-istr_nh+1)*(N+2)   ! SUD-NORTH boundary
    doalloc=.FALSE.
  endif

!    print *,par%tvoisin(:)
!    print *,"N=",N
! !   ! Number of exchange
!    call mpi_finalize(ierr)
!    stop 'ici...'
  nbreq=0

  !******************************************************************
  !		 U point variables
  !******************************************************************
  
  !******************************************************************
  !		 WEST direction
  !******************************************************************
  if (par%tvoisin(ouest) /= MPI_PROC_NULL) then
  !-------- gdepth_u exchange ---------------------------------------
    gdepth_uS%ouest = gdepth_u(istru_nh,jstr_nh:jend_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(gdepth_uS%ouest, sz1, MPI_DOUBLE_PRECISION, &
      par%tvoisin(ouest), TAG1, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(gdepth_uR%ouest, sz1, MPI_DOUBLE_PRECISION, &
      par%tvoisin(ouest), TAG2, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefa_u exchange ---------------------------------------
    coefa_uS%ouest = coefa_u(istru_nh,jstr_nh:jend_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefa_uS%ouest, sz1, MPI_DOUBLE_PRECISION, &
      par%tvoisin(ouest), TAG3, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefa_uR%ouest, sz1, MPI_DOUBLE_PRECISION, &
      par%tvoisin(ouest), TAG4, par%comm2d, tabreq(nbreq), ierror)
   !-------- coefb_u exchange ---------------------------------------     
    coefb_uS%ouest = coefb_u(istru_nh,jstr_nh:jend_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefb_uS%ouest, sz1, MPI_DOUBLE_PRECISION, &
      par%tvoisin(ouest), TAG5, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefb_uR%ouest, sz1, MPI_DOUBLE_PRECISION, &
      par%tvoisin(ouest), TAG6, par%comm2d, tabreq(nbreq), ierror)
  endif
  
  
  !******************************************************************
  !		 EAST direction
  !******************************************************************
   if (par%tvoisin(est) /= MPI_PROC_NULL) then
  !-------- gdepth_u exchange ---------------------------------------
    gdepth_uS%est   = gdepth_u(iendu_nh,jstr_nh:jend_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(gdepth_uS%est, sz1, MPI_DOUBLE_PRECISION, &
      par%tvoisin(est), TAG2, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(gdepth_uR%est, sz1, MPI_DOUBLE_PRECISION, &
      par%tvoisin(est), TAG1, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefa_u exchange ---------------------------------------
    coefa_uS%est   = coefa_u(iendu_nh,jstr_nh:jend_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefa_uS%est, sz1, MPI_DOUBLE_PRECISION, &
      par%tvoisin(est), TAG4, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefa_uR%est, sz1, MPI_DOUBLE_PRECISION, &
      par%tvoisin(est), TAG3, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefb_u exchange ---------------------------------------     
    coefb_uS%est   = coefb_u(iendu_nh,jstr_nh:jend_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefb_uS%est, sz1, MPI_DOUBLE_PRECISION, &
      par%tvoisin(est), TAG6, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefb_uR%est, sz1, MPI_DOUBLE_PRECISION, &
      par%tvoisin(est), TAG5, par%comm2d, tabreq(nbreq), ierror)
  endif
  
  
  !******************************************************************
  !		 NORTH direction
  !******************************************************************
  if (par%tvoisin(nord) /= MPI_PROC_NULL) then
  !-------- gdepth_u exchange ---------------------------------------
    gdepth_uS%nord   = gdepth_u(istru_nh:iendu_nh,jend_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(gdepth_uS%nord, sz3, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nord), TAG14, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(gdepth_uR%nord, sz3, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nord), TAG13, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefa_u exchange ---------------------------------------
    coefa_uS%nord = coefa_u(istru_nh:iendu_nh,jend_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefa_uS%nord, sz3, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nord), TAG16, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefa_uR%nord, sz3, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nord), TAG15, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefb_u exchange ---------------------------------------     
    coefb_uS%nord = coefb_u(istru_nh:iendu_nh,jend_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefb_uS%nord, sz3, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nord), TAG18, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefb_uR%nord, sz3, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nord), TAG17, par%comm2d, tabreq(nbreq), ierror)
  endif

  !******************************************************************
  !		 SOUTH direction
  !******************************************************************
  if (par%tvoisin(sud) /= MPI_PROC_NULL) then
  !-------- gdepth_u exchange ---------------------------------------
    gdepth_uS%sud = gdepth_u(istru_nh:iendu_nh,jstr_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(gdepth_uS%sud, sz3, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sud), TAG13, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(gdepth_uR%sud, sz3, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sud), TAG14, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefa_u exchange ---------------------------------------
    coefa_uS%sud   = coefa_u(istru_nh:iendu_nh,jstr_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefa_uS%sud, sz3, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sud), TAG15, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefa_uR%sud, sz3, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sud), TAG16, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefb_u exchange ---------------------------------------     
    coefb_uS%sud   = coefb_u(istru_nh:iendu_nh,jstr_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefb_uS%sud, sz3, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sud), TAG17, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefb_uR%sud, sz3, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sud), TAG18, par%comm2d, tabreq(nbreq), ierror)
  endif

  !******************************************************************
  !		 CORNER direction
  !******************************************************************
  if (par%tvoisin(sudest) /= MPI_PROC_NULL) then
      stop 'TBD....'
  endif
  if (par%tvoisin(sudouest) /= MPI_PROC_NULL) then
      stop 'TBD....'
  endif
  if (par%tvoisin(nordouest) /= MPI_PROC_NULL) then
      stop 'TBD....'
  endif
  if (par%tvoisin(nordest) /= MPI_PROC_NULL) then
      stop 'TBD....'
  endif

  
  

  !******************************************************************
  !********************* V Point variables    ***********************
  
  !******************************************************************
  !		 WEST direction
  !******************************************************************
  if (par%tvoisin(ouest) /= MPI_PROC_NULL) then
  !-------- gdepth_v exchange ---------------------------------------
    gdepth_vS%ouest = gdepth_v(istr_nh,jstrv_nh:jendv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(gdepth_vS%ouest, sz2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(ouest), TAG19, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(gdepth_vR%ouest, sz2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(ouest), TAG20, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefa_v exchange ---------------------------------------
    coefa_vS%ouest = coefa_v(istr_nh,jstrv_nh:jendv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefa_vS%ouest, sz2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(ouest), TAG21, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefa_vR%ouest, sz2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(ouest), TAG22, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefb_v exchange ---------------------------------------
    coefb_vS%ouest = coefb_v(istr_nh,jstrv_nh:jendv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefb_vS%ouest, sz2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(ouest), TAG23, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefb_vR%ouest, sz2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(ouest), TAG24, par%comm2d, tabreq(nbreq), ierror)
  endif

  !******************************************************************
  !		 EAST direction
  !******************************************************************
  if (par%tvoisin(est) /= MPI_PROC_NULL) then
  !-------- gdepth_v exchange ---------------------------------------
    gdepth_vS%est   = gdepth_v(iend_nh,jstrv_nh:jendv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(gdepth_vS%est, sz2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(est), TAG20, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(gdepth_vR%est, sz2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(est), TAG19, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefa_v exchange ---------------------------------------
    coefa_vS%est   = coefa_v(iend_nh,jstrv_nh:jendv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefa_vS%est, sz2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(est), TAG22, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefa_vR%est, sz2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(est), TAG21, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefb_v exchange ---------------------------------------
    coefb_vS%est   = coefb_v(iend_nh,jstrv_nh:jendv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefb_vS%est, sz2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(est), TAG24, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefb_vR%est, sz2, MPI_DOUBLE_PRECISION, &
      par%tvoisin(est), TAG23, par%comm2d, tabreq(nbreq), ierror)
  endif

  !******************************************************************
  !		 NORTH direction
  !******************************************************************
  if (par%tvoisin(nord) /= MPI_PROC_NULL) then
  !-------- gdepth_v exchange ---------------------------------------
    gdepth_vS%nord   = gdepth_v(istr_nh:iend_nh,jendv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(gdepth_vS%nord, sz4, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nord), TAG26, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(gdepth_vR%nord, sz4, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nord), TAG25, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefa_v exchange ---------------------------------------
    coefa_vS%nord   = coefa_v(istr_nh:iend_nh,jendv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefa_vS%nord, sz4, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nord), TAG28, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefa_vR%nord, sz4, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nord), TAG27, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefb_v exchange ---------------------------------------
    coefb_vS%nord   = coefb_v(istr_nh:iend_nh,jendv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefb_vS%nord, sz4, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nord), TAG30, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefb_vR%nord, sz4, MPI_DOUBLE_PRECISION, &
      par%tvoisin(nord), TAG29, par%comm2d, tabreq(nbreq), ierror)
  endif

  !******************************************************************
  !		 SOUTH direction
  !******************************************************************
  if (par%tvoisin(sud) /= MPI_PROC_NULL) then
  !-------- gdepth_v exchange ---------------------------------------
    gdepth_vS%sud   = gdepth_v(istr_nh:iend_nh,jstrv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(gdepth_vS%sud, sz4, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sud), TAG25, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(gdepth_vR%sud, sz4, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sud), TAG26, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefa_v exchange ---------------------------------------
    coefa_vS%sud   = coefa_v(istr_nh:iend_nh,jstrv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefa_vS%sud, sz4, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sud), TAG27, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefa_vR%sud, sz4, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sud), TAG28, par%comm2d, tabreq(nbreq), ierror)
  !-------- coefb_v exchange ---------------------------------------
    coefb_vS%sud   = coefb_v(istr_nh:iend_nh,jstrv_nh,0:N+1)
    nbreq=nbreq+1
    call MPI_ISEND(coefb_vS%sud, sz4, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sud), TAG29, par%comm2d, tabreq(nbreq), ierror)
    nbreq=nbreq+1
    call MPI_IRECV(coefb_vR%sud, sz4, MPI_DOUBLE_PRECISION, &
      par%tvoisin(sud), TAG30, par%comm2d, tabreq(nbreq), ierror)
  endif

  
  !******************************************************************
  !		 CORNER direction
  !******************************************************************
  if (par%tvoisin(sudest) /= MPI_PROC_NULL) then
  !-------- gdepth_v exchange ---------------------------------------
  !-------- coefa_v exchange ---------------------------------------
  !-------- coefb_v exchange ---------------------------------------
      stop 'TBD....'
  endif
  if (par%tvoisin(sudouest) /= MPI_PROC_NULL) then
  !-------- gdepth_v exchange ---------------------------------------
  !-------- coefa_v exchange ---------------------------------------
  !-------- coefb_v exchange ---------------------------------------
      stop 'TBD....'
  endif
  if (par%tvoisin(nordouest) /= MPI_PROC_NULL) then
  !-------- gdepth_v exchange ---------------------------------------
  !-------- coefa_v exchange ---------------------------------------
  !-------- coefb_v exchange ---------------------------------------
      stop 'TBD....'
  endif
  if (par%tvoisin(nordest) /= MPI_PROC_NULL) then
  !-------- gdepth_v exchange ---------------------------------------
  !-------- coefa_v exchange ---------------------------------------
  !-------- coefb_v exchange ---------------------------------------
      stop 'TBD....'
  endif
 

  !! Waiting ROOM
  CALL MPI_WAITALL(nbreq, tabreq(1:nbreq), tstatus(:,1:nbreq), ierror)   
!  write(100+par%rank,*) "ierror=",ierror
!  write(100+par%rank,*) "tstatus=",tstatus
  
  if (par%tvoisin(ouest) /= MPI_PROC_NULL) then
      gdepth_u(istru_nh-1,jstr_nh:jend_nh,0:N+1)  = gdepth_uR%ouest
       coefa_u(istru_nh-1,jstr_nh:jend_nh,0:N+1)  = coefa_uR%ouest
       coefb_u(istru_nh-1,jstr_nh:jend_nh,0:N+1)  = coefb_uR%ouest
      
      gdepth_v(istr_nh-1,jstrv_nh:jendv_nh,0:N+1)  = gdepth_vR%ouest
       coefa_v(istr_nh-1,jstrv_nh:jendv_nh,0:N+1)  = coefa_vR%ouest
       coefb_v(istr_nh-1,jstrv_nh:jendv_nh,0:N+1)  = coefb_vR%ouest
  endif
  if (par%tvoisin(  est) /= MPI_PROC_NULL) then
      gdepth_u(iendu_nh+1,jstr_nh:jend_nh,0:N+1)  = gdepth_uR%est
       coefa_u(iendu_nh+1,jstr_nh:jend_nh,0:N+1)  = coefa_uR%est
       coefb_u(iendu_nh+1,jstr_nh:jend_nh,0:N+1)  = coefb_uR%est
      
      gdepth_v(iend_nh+1,jstrv_nh:jendv_nh,0:N+1)  = gdepth_vR%est
       coefa_v(iend_nh+1,jstrv_nh:jendv_nh,0:N+1)  = coefa_vR%est
       coefb_v(iend_nh+1,jstrv_nh:jendv_nh,0:N+1)  = coefb_vR%est
  endif

  
  if (par%tvoisin(nord) /= MPI_PROC_NULL) then
      gdepth_u(istru_nh:iendu_nh,jend_nh+1,0:N+1)  = gdepth_uR%nord
       coefa_u(istru_nh:iendu_nh,jend_nh+1,0:N+1)  = coefa_uR%nord
       coefb_u(istru_nh:iendu_nh,jend_nh+1,0:N+1)  = coefb_uR%nord
      
       gdepth_v(istr_nh:iend_nh,jendv_nh+1,0:N+1)  = gdepth_vR%nord
        coefa_v(istr_nh:iend_nh,jendv_nh+1,0:N+1)  = coefa_vR%nord
        coefb_v(istr_nh:iend_nh,jendv_nh+1,0:N+1)  = coefb_vR%nord
  endif
  if (par%tvoisin(  sud) /= MPI_PROC_NULL) then
      gdepth_u(istru_nh:iendu_nh,jstr_nh-1,0:N+1)  = gdepth_uR%sud
       coefa_u(istru_nh:iendu_nh,jstr_nh-1,0:N+1)  = coefa_uR%sud
       coefb_u(istru_nh:iendu_nh,jstr_nh-1,0:N+1)  = coefb_uR%sud
      
      gdepth_v(istr_nh:iend_nh,jstrv_nh-1,0:N+1)  = gdepth_vR%sud
       coefa_v(istr_nh:iend_nh,jstrv_nh-1,0:N+1)  = coefa_vR%sud
       coefb_v(istr_nh:iend_nh,jstrv_nh-1,0:N+1)  = coefb_vR%sud
  endif
  
!  print *,"istru_nh,iendu_nh,jstr_nh,jend_nh,N=",istru_nh,iendu_nh,jstr_nh,jend_nh,N
#endif  
end subroutine grid_exchange
