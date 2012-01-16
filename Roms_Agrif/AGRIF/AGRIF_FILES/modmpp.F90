!
! $Id: modmpp.F 779 2007-12-22 17:04:17Z rblod $
!
!     AGRIF (Adaptive Grid Refinement In Fortran)
!
!     Copyright (C) 2003 Laurent Debreu (Laurent.Debreu@imag.fr)
!                        Christophe Vouland (Christophe.Vouland@imag.fr)
!
!     This program is free software; you can redistribute it and/or modify
!     it under the terms of the GNU General Public License as published by
!     the Free Software Foundation; either version 2 of the License, or
!     (at your option) any later version.
!
!     This program is distributed in the hope that it will be useful,
!     but WITHOUT ANY WARRANTY; without even the implied warranty of
!     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!     GNU General Public License for more details.
!
!     You should have received a copy of the GNU General Public License
!     along with this program; if not, write to the Free Software
!     Foundation, Inc., 59 Temple Place- Suite 330, Boston, MA 02111-1307, USA.
!
!
module Agrif_Mpp
!
    use Agrif_Types
    use Agrif_Arrays
!
contains
!
#if defined AGRIF_MPI
!
!===================================================================================================
!  subroutine Get_External_Data_first
!---------------------------------------------------------------------------------------------------
subroutine Get_External_Data_first ( pttruetab, cetruetab, pttruetabwhole, cetruetabwhole,  &
                                     nbdim, memberoutall, sendtoproc, recvfromproc,         &
                                     imin, imax, imin_recv, imax_recv )
!---------------------------------------------------------------------------------------------------
    include 'mpif.h'
!
    integer,                                     intent(in)  :: nbdim
    integer, dimension(nbdim,0:Agrif_NbProcs-1), intent(in)  :: pttruetab,     cetruetab
    integer, dimension(nbdim,0:Agrif_NbProcs-1), intent(in)  :: pttruetabwhole,cetruetabwhole
    logical, dimension(0:Agrif_Nbprocs-1),       intent(in)  :: memberoutall
    logical, dimension(0:Agrif_Nbprocs-1),       intent(out) :: sendtoproc
    logical, dimension(0:Agrif_Nbprocs-1),       intent(out) :: recvfromproc
    integer, dimension(nbdim,0:Agrif_NbProcs-1), intent(out) :: imin,imax
    integer, dimension(nbdim,0:Agrif_NbProcs-1), intent(out) :: imin_recv,imax_recv
!
    integer :: imintmp, imaxtmp, i, j, k, i1
    integer :: imin1,imax1
    logical :: tochange,tochangebis
    integer, dimension(nbdim,0:Agrif_NbProcs-1)    :: pttruetab2,cetruetab2
!
! pttruetab2 and cetruetab2 are modified arraysin order to always
! send the most inner points
!
    pttruetab2(:,Agrif_Procrank) = pttruetab(:,Agrif_Procrank)
    cetruetab2(:,Agrif_Procrank) = cetruetab(:,Agrif_Procrank)
!
    do k = 0,Agrif_Nbprocs-1
    do i = 1,nbdim
        tochangebis=.TRUE.
        DO i1 = 1,nbdim
            IF (i /= i1) THEN
                IF ( (pttruetab(i1,Agrif_Procrank) /= pttruetab(i1,k)).OR. &
                     (cetruetab(i1,Agrif_Procrank) /= cetruetab(i1,k))) THEN
                    tochangebis = .FALSE.
                    EXIT
                ENDIF
            ENDIF
        ENDDO
        IF (tochangebis) THEN
            imin1 = max(pttruetab(i,Agrif_Procrank), pttruetab(i,k))
            imax1 = min(cetruetab(i,Agrif_Procrank), cetruetab(i,k))
! Always send the most interior points

            tochange = .false.
            IF (cetruetab(i,Agrif_Procrank) > cetruetab(i,k)) THEN
                DO j=imin1,imax1
                    IF ((cetruetab(i,k)-j) > (j-pttruetab(i,Agrif_Procrank))) THEN
                        imintmp = j+1
                        tochange = .TRUE.
                    ELSE
                        EXIT
                    ENDIF
                ENDDO
            ENDIF

            if (tochange) then
                pttruetab2(i,Agrif_Procrank) = imintmp
            endif

            tochange = .FALSE.
            imaxtmp=0
            IF (pttruetab(i,Agrif_Procrank) < pttruetab(i,k)) THEN
                DO j=imax1,imin1,-1
                    IF ((j-pttruetab(i,k)) > (cetruetab(i,Agrif_Procrank)-j)) THEN
                        imaxtmp = j-1
                        tochange = .TRUE.
                    ELSE
                        EXIT
                    ENDIF
                ENDDO
            ENDIF

            if (tochange) then
                cetruetab2(i,Agrif_Procrank) = imaxtmp
            endif
        ENDIF
    enddo
    enddo

    do k = 0,Agrif_NbProcs-1
!
        sendtoproc(k) = .true.
!
!CDIR SHORTLOOP
        do i = 1,nbdim
            imin(i,k) = max(pttruetab2(i,Agrif_Procrank), pttruetabwhole(i,k))
            imax(i,k) = min(cetruetab2(i,Agrif_Procrank), cetruetabwhole(i,k))
!
            if (imin(i,k) > imax(i,k)) then
                sendtoproc(k) = .false.
            endif
        enddo
        IF (.NOT.memberoutall(k)) THEN
            sendtoproc(k) = .FALSE.
        ENDIF
    enddo
!
    call Exchangesamelevel_first(sendtoproc,nbdim,imin,imax,recvfromproc,imin_recv,imax_recv)
!---------------------------------------------------------------------------------------------------
end subroutine Get_External_Data_first
!===================================================================================================
!
! no more used ???
#if 0
!===================================================================================================
!  subroutine Get_External_Data
!---------------------------------------------------------------------------------------------------
subroutine Get_External_Data ( tempC, tempCextend, pttruetab, cetruetab,    &
                               pttruetabwhole, cetruetabwhole,              &
                               nbdim, memberin, memberout, memberoutall1 )
!---------------------------------------------------------------------------------------------------
    include 'mpif.h'
!
    INTEGER :: nbdim
    TYPE(Agrif_PVariable) :: tempC, tempCextend
    INTEGER,DIMENSION(nbdim,0:Agrif_NbProcs-1)    :: pttruetab,     cetruetab
    INTEGER,DIMENSION(nbdim,0:Agrif_NbProcs-1)    :: pttruetabwhole,cetruetabwhole
    INTEGER :: k,i,k2
    LOGICAL :: sendtoproc(0:Agrif_Nbprocs-1)
    INTEGER,DIMENSION(nbdim,0:Agrif_NbProcs-1)    :: imin,imax
    LOGICAL :: memberin, memberout
    INTEGER :: imintmp, imaxtmp,j,i1
    INTEGER :: imin1,imax1
    LOGICAL :: tochange,tochangebis
    INTEGER,DIMENSION(nbdim,0:Agrif_NbProcs-1)    :: pttruetab2, cetruetab2
    LOGICAL :: memberout1(1),memberoutall(0:Agrif_Nbprocs-1)
    LOGICAL, OPTIONAL :: memberoutall1(0:Agrif_Nbprocs-1)
    INTEGER :: code

! pttruetab2 and cetruetab2 are modified arraysin order to always
! send the most inner points

    IF (present(memberoutall1)) THEN
        memberoutall = memberoutall1
    ELSE
         memberout1(1) = memberout
        call MPI_ALLGATHER(memberout1,1,MPI_LOGICAL,memberoutall,1,MPI_LOGICAL,MPI_COMM_WORLD,code)
    ENDIF
    
    pttruetab2(:,Agrif_Procrank) = pttruetab(:,Agrif_Procrank)
    cetruetab2(:,Agrif_Procrank) = cetruetab(:,Agrif_Procrank)
    
    do k2=0,Agrif_Nbprocs-1
    do i=1,nbdim
!
        tochangebis=.TRUE.
        DO i1=1,nbdim
            IF (i /= i1) THEN
                IF ((pttruetab(i1,Agrif_Procrank) /= pttruetab(i1,k2)) .OR. &
                    (cetruetab(i1,Agrif_Procrank) /= cetruetab(i1,k2))) THEN
                    tochangebis = .FALSE.
                    EXIT
                ENDIF
            ENDIF
        ENDDO

        IF (tochangebis) THEN

            imin1 = max(pttruetab(i,Agrif_Procrank),pttruetab(i,k2))
            imax1 = min(cetruetab(i,Agrif_Procrank),cetruetab(i,k2))

! Always send the most interior points

            tochange = .false.
            IF (cetruetab(i,Agrif_Procrank)> cetruetab(i,k2)) THEN
                DO j=imin1,imax1
                    IF ((cetruetab(i,k2)-j) > (j-pttruetab(i,Agrif_Procrank))) THEN
                        imintmp = j+1
                        tochange = .TRUE.
                    ELSE
                        EXIT
                    ENDIF
                ENDDO
            ENDIF

            if (tochange) then
                pttruetab2(i,Agrif_Procrank) = imintmp
            endif

            tochange = .FALSE.
            imaxtmp=0
            IF (pttruetab(i,Agrif_Procrank) < pttruetab(i,k2)) THEN
                DO j=imax1,imin1,-1
                    IF ((j-pttruetab(i,k2)) > (cetruetab(i,Agrif_Procrank)-j)) THEN
                        imaxtmp = j-1
                        tochange = .TRUE.
                    ELSE
                        EXIT
                    ENDIF
                ENDDO
            ENDIF

            if (tochange) then
                cetruetab2(i,Agrif_Procrank) = imaxtmp
            endif
!
        ENDIF
    enddo
    enddo

    do k = 0,Agrif_NbProcs-1
!
        sendtoproc(k) = .true.
!
!CDIR SHORTLOOP
        do i = 1,nbdim
            imin(i,k) = max(pttruetab2(i,Agrif_Procrank),pttruetabwhole(i,k))
            imax(i,k) = min(cetruetab2(i,Agrif_Procrank),cetruetabwhole(i,k))
!
            if (imin(i,k) > imax(i,k)) then
                sendtoproc(k) = .false.
            endif
!
        enddo
        IF (.NOT.memberoutall(k)) THEN
            sendtoproc(k) = .FALSE.
        ENDIF
!
    enddo


!    IF (.NOT.memberin) sendtoproc = .FALSE.

    IF (memberout) THEN
        call Agrif_nbdim_allocation(tempCextend%var,        &
                        pttruetabwhole(:,Agrif_ProcRank),   &
                        cetruetabwhole(:,Agrif_ProcRank),nbdim)
        call Agrif_nbdim_Full_VarEQreal(tempCextend%var,0.,nbdim)
    ENDIF

    IF (sendtoproc(Agrif_ProcRank)) THEN
        call Agrif_nbdim_VarEQvar(  &
                tempCextend%var, imin(:,Agrif_Procrank), imax(:,Agrif_Procrank), &
                tempC%var,       imin(:,Agrif_Procrank), imax(:,Agrif_Procrank), nbdim)
    ENDIF

    call Exchangesamelevel(sendtoproc,nbdim,imin,imax,tempC,tempCextend)
!---------------------------------------------------------------------------------------------------
end subroutine Get_External_Data
!===================================================================================================
!endif 0 : Get_External_Data is no more used
#endif
!
! no more used ???
#if 0
!===================================================================================================
!  subroutine ExchangeSameLevel
!---------------------------------------------------------------------------------------------------
subroutine ExchangeSameLevel ( sendtoproc, nbdim, imin, imax, tempC, tempCextend )
!---------------------------------------------------------------------------------------------------
    INTEGER :: nbdim
    INTEGER,DIMENSION(nbdim,0:Agrif_Nbprocs-1)      :: imin,imax
    INTEGER,DIMENSION(nbdim,2,0:Agrif_Nbprocs-1)    :: iminmax_temp
    INTEGER,DIMENSION(nbdim,0:Agrif_Nbprocs-1)      :: imin_recv,imax_recv
    TYPE(Agrif_PVariable)                           :: tempC,tempCextend
    LOGICAL,DIMENSION(0:Agrif_Nbprocs-1)            :: sendtoproc
    LOGICAL,DIMENSION(0:Agrif_Nbprocs-1)            :: recvfromproc
    LOGICAL                                         :: res
    TYPE(Agrif_PVariable), SAVE                     :: temprecv

    include 'mpif.h'
    INTEGER :: k
    INTEGER :: etiquette = 100
    INTEGER :: code, datasize
    INTEGER,DIMENSION(MPI_STATUS_SIZE)   :: statut

    do k = 0,Agrif_ProcRank-1
!
        call MPI_SEND(sendtoproc(k),1,MPI_LOGICAL,k,etiquette,MPI_COMM_WORLD,code)
!
        if (sendtoproc(k)) then
!
            iminmax_temp(:,1,k) = imin(:,k)
            iminmax_temp(:,2,k) = imax(:,k)

            call MPI_SEND(iminmax_temp(:,:,k),2*nbdim,MPI_INTEGER,k,etiquette,MPI_COMM_WORLD,code)
!
            datasize = 1
!
!CDIR SHORTLOOP
            do i = 1,nbdim
                datasize = datasize * (imax(i,k)-imin(i,k)+1)
            enddo
!
            SELECT CASE(nbdim)
            CASE(1)
                call MPI_SEND(tempC%var%array1(imin(1,k):imax(1,k)),    &
                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,      &
                        MPI_COMM_WORLD,code)
            CASE(2)
                call MPI_SEND(tempC%var%array2(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k)),    &
                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,      &
                        MPI_COMM_WORLD,code)
            CASE(3)
                call Agrif_Send_3Darray(tempC%var%array3,lbound(tempC%var%array3),imin(:,k),imax(:,k),k)
            CASE(4)
                call MPI_SEND(tempC%var%array4(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k),     &
                                               imin(4,k):imax(4,k)),    &
                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,      &
                        MPI_COMM_WORLD,code)
            CASE(5)
                call MPI_SEND(tempC%var%array5(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k),     &
                                               imin(4,k):imax(4,k),     &
                                               imin(5,k):imax(5,k)),    &
                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,      &
                        MPI_COMM_WORLD,code)
            CASE(6)
                call MPI_SEND(tempC%var%array6(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k),     &
                                               imin(4,k):imax(4,k),     &
                                               imin(5,k):imax(5,k),     &
                                               imin(6,k):imax(6,k)),    &
                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,      &
                        MPI_COMM_WORLD,code)
            END SELECT
        endif
    enddo
!
!   Reception from others processors of the necessary part of the parent grid
    do k = Agrif_ProcRank+1,Agrif_Nbprocs-1
!
        call MPI_RECV(res,1,MPI_LOGICAL,k,etiquette,MPI_COMM_WORLD,statut,code)
!
        recvfromproc(k) = res
!
        if (recvfromproc(k)) then
!
            call MPI_RECV(iminmax_temp(:,:,k),2*nbdim,MPI_INTEGER,k,etiquette,MPI_COMM_WORLD,statut,code)

            imin_recv(:,k) = iminmax_temp(:,1,k)
            imax_recv(:,k) = iminmax_temp(:,2,k)
            datasize = 1
!
!CDIR SHORTLOOP
            do i = 1,nbdim
                datasize = datasize * (imax_recv(i,k)-imin_recv(i,k)+1)
            enddo

            IF (.Not.Associated(temprecv%var)) allocate(temprecv%var)
            call Agrif_nbdim_allocation(temprecv%var,imin_recv(:,k),imax_recv(:,k),nbdim)
            
            SELECT CASE(nbdim)
            CASE(1)
                call MPI_RECV(temprecv%var%array1,datasize,MPI_DOUBLE_PRECISION,k,etiquette, &
                        MPI_COMM_WORLD,statut,code)
            CASE(2)
                call MPI_RECV(temprecv%var%array2,datasize,MPI_DOUBLE_PRECISION,k,etiquette, &
                        MPI_COMM_WORLD,statut,code)
            CASE(3)
                call MPI_RECV(temprecv%var%array3,datasize,MPI_DOUBLE_PRECISION,k,etiquette, &
                        MPI_COMM_WORLD,statut,code)
            CASE(4)
                call MPI_RECV(temprecv%var%array4,datasize,MPI_DOUBLE_PRECISION,k,etiquette, &
                        MPI_COMM_WORLD,statut,code)
            CASE(5)
                call MPI_RECV(temprecv%var%array5,datasize,MPI_DOUBLE_PRECISION,k,etiquette, &
                        MPI_COMM_WORLD,statut,code)
            CASE(6)
                call MPI_RECV(temprecv%var%array6,datasize,MPI_DOUBLE_PRECISION,k,etiquette, &
                        MPI_COMM_WORLD,statut,code)
            END SELECT

            call where_valtabtotab_mpi(tempCextend%var,temprecv%var,imin_recv(:,k),imax_recv(:,k),0.,nbdim)
            call Agrif_nbdim_deallocation(temprecv%var,nbdim)
!                deallocate(temprecv%var)
        endif
    enddo

!   Reception from others processors of the necessary part of the parent grid
    do k = Agrif_ProcRank+1,Agrif_Nbprocs-1
!
        call MPI_SEND(sendtoproc(k),1,MPI_LOGICAL,k,etiquette,MPI_COMM_WORLD,code)
!
        if (sendtoproc(k)) then
!
            iminmax_temp(:,1,k) = imin(:,k)
            iminmax_temp(:,2,k) = imax(:,k)

            call MPI_SEND(iminmax_temp(:,:,k),2*nbdim,MPI_INTEGER,k,etiquette,MPI_COMM_WORLD,code)
!
            SELECT CASE(nbdim)
            CASE(1)
                datasize=SIZE(tempC%var%array1(imin(1,k):imax(1,k)))
                call MPI_SEND(tempC%var%array1(imin(1,k):imax(1,k)),    &
                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,      &
                        MPI_COMM_WORLD,code)
            CASE(2)
                datasize=SIZE(tempC%var%array2(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k)))
                call MPI_SEND(tempC%var%array2(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k)),    &
                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,      &
                        MPI_COMM_WORLD,code)
            CASE(3)
                datasize=SIZE(tempC%var%array3(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k)))
                call MPI_SEND(tempC%var%array3(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k)),    &
                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,      &
                        MPI_COMM_WORLD,code)
            CASE(4)
                datasize=SIZE(tempC%var%array4(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k),     &
                                               imin(4,k):imax(4,k)))
                call MPI_SEND(tempC%var%array4(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k),     &
                                               imin(4,k):imax(4,k)),    &
                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,      &
                        MPI_COMM_WORLD,code)
            CASE(5)
                datasize=SIZE(tempC%var%array5(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k),     &
                                               imin(4,k):imax(4,k),     &
                                               imin(5,k):imax(5,k)))
                call MPI_SEND(tempC%var%array5(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k),     &
                                               imin(4,k):imax(4,k),     &
                                               imin(5,k):imax(5,k)),    &
                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,      &
                        MPI_COMM_WORLD,code)
            CASE(6)
                datasize=SIZE(tempC%var%array6(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k),     &
                                               imin(4,k):imax(4,k),     &
                                               imin(5,k):imax(5,k),     &
                                               imin(6,k):imax(6,k)))
                call MPI_SEND(tempC%var%array6(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k),     &
                                               imin(4,k):imax(4,k),     &
                                               imin(5,k):imax(5,k),     &
                                               imin(6,k):imax(6,k)),    &
                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,      &
                        MPI_COMM_WORLD,code)
            END SELECT
!
        endif

!
    enddo
!
!
!   Reception from others processors of the necessary part of the parent grid
    do k = Agrif_ProcRank-1,0,-1
!
        call MPI_RECV(res,1,MPI_LOGICAL,k,etiquette,MPI_COMM_WORLD,statut,code)
!
        recvfromproc(k) = res

        if (recvfromproc(k)) then
!
            call MPI_RECV(iminmax_temp(:,:,k),2*nbdim,MPI_INTEGER,k,etiquette,MPI_COMM_WORLD,statut,code)

!           imin_recv(:,k) = iminmax_temp(:,1,k)
!           imax_recv(:,k) = iminmax_temp(:,2,k)

!           datasize = 1
!
!           do i = 1,nbdim
!               datasize = datasize * (imax_recv(i,k)-imin_recv(i,k)+1)
!           enddo
            IF (.Not.Associated(temprecv%var)) allocate(temprecv%var)
            call Agrif_nbdim_allocation(temprecv%var,iminmax_temp(:,1,k),iminmax_temp(:,2,k),nbdim)

            SELECT CASE(nbdim)
            CASE(1)
                datasize=SIZE(temprecv%var%array1)
                call MPI_RECV(temprecv%var%array1,datasize,MPI_DOUBLE_PRECISION,k,etiquette,&
                        MPI_COMM_WORLD,statut,code)
            CASE(2)
                datasize=SIZE(temprecv%var%array2)
                call MPI_RECV(temprecv%var%array2,datasize,MPI_DOUBLE_PRECISION,k,etiquette,&
                        MPI_COMM_WORLD,statut,code)
            CASE(3)
                datasize=SIZE(temprecv%var%array3)
                call MPI_RECV(temprecv%var%array3,datasize,MPI_DOUBLE_PRECISION,k,etiquette,&
                        MPI_COMM_WORLD,statut,code)

            CASE(4)
                datasize=SIZE(temprecv%var%array4)
                call MPI_RECV(temprecv%var%array4,datasize,MPI_DOUBLE_PRECISION,k,etiquette,&
                        MPI_COMM_WORLD,statut,code)
            CASE(5)
                datasize=SIZE(temprecv%var%array5)
                call MPI_RECV(temprecv%var%array5,datasize,MPI_DOUBLE_PRECISION,k,etiquette,&
                        MPI_COMM_WORLD,statut,code)
            CASE(6)
                datasize=SIZE(temprecv%var%array6)
                call MPI_RECV(temprecv%var%array6,datasize,MPI_DOUBLE_PRECISION,k,etiquette,&
                        MPI_COMM_WORLD,statut,code)
          END SELECT

            call where_valtabtotab_mpi(tempCextend%var,temprecv%var, &
                   iminmax_temp(:,1,k),iminmax_temp(:,2,k),0.,nbdim)

            call Agrif_nbdim_deallocation(temprecv%var,nbdim)
!                deallocate(temprecv%var)
        endif
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine ExchangeSamelevel
!===================================================================================================
! endif 0 : ExchangeSamelevel is no more used
#endif
!
!===================================================================================================
!  subroutine ExchangeSameLevel_first
!---------------------------------------------------------------------------------------------------
subroutine ExchangeSameLevel_first ( sendtoproc, nbdim, imin, imax, recvfromproc, &
                                     imin_recv, imax_recv )
!---------------------------------------------------------------------------------------------------
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1),       intent(in)  :: sendtoproc
    INTEGER,                                     intent(in)  :: nbdim
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1), intent(in)  :: imin
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1), intent(in)  :: imax
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1),       intent(out) :: recvfromproc
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1), intent(out) :: imin_recv
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1), intent(out) :: imax_recv
!
    include 'mpif.h'
    INTEGER :: i,k
    INTEGER :: etiquette = 100
    INTEGER :: code
    LOGICAL :: res
    INTEGER, DIMENSION(MPI_STATUS_SIZE)   :: statut
    INTEGER, DIMENSION(nbdim,2,0:Agrif_Nbprocs-1)    :: iminmax_temp

    do k = 0,Agrif_ProcRank-1
!
        call MPI_SEND(sendtoproc(k),1,MPI_LOGICAL,k,etiquette,MPI_COMM_WORLD,code)
!
        if (sendtoproc(k)) then
            iminmax_temp(:,1,k) = imin(:,k)
            iminmax_temp(:,2,k) = imax(:,k)
            call MPI_SEND(iminmax_temp(:,:,k),2*nbdim,MPI_INTEGER,k,etiquette,MPI_COMM_WORLD,code)
        endif
!
    enddo
!
!   Reception from others processors of the necessary part of the parent grid
    do k = Agrif_ProcRank+1,Agrif_Nbprocs-1
!
        call MPI_RECV(res,1,MPI_LOGICAL,k,etiquette,MPI_COMM_WORLD,statut,code)
        recvfromproc(k) = res
!
        if (recvfromproc(k)) then
            call MPI_RECV(iminmax_temp(:,:,k),2*nbdim,MPI_INTEGER,k,etiquette, &
                    MPI_COMM_WORLD,statut,code)
            imin_recv(:,k) = iminmax_temp(:,1,k)
            imax_recv(:,k) = iminmax_temp(:,2,k)
        endif
!
    enddo

!   Reception from others processors of the necessary part of the parent grid
    do k = Agrif_ProcRank+1,Agrif_Nbprocs-1
!
        call MPI_SEND(sendtoproc(k),1,MPI_LOGICAL,k,etiquette,MPI_COMM_WORLD,code)
!
        if (sendtoproc(k)) then
!
            iminmax_temp(:,1,k) = imin(:,k)
            iminmax_temp(:,2,k) = imax(:,k)

            call MPI_SEND(iminmax_temp(:,:,k),2*nbdim,MPI_INTEGER,k,etiquette, &
                    MPI_COMM_WORLD,code)
        endif
!
    enddo
!
!
!   Reception from others processors of the necessary part of the parent grid
    do k = Agrif_ProcRank-1,0,-1
!
        call MPI_RECV(res,1,MPI_LOGICAL,k,etiquette,MPI_COMM_WORLD,statut,code)
        recvfromproc(k) = res
!
        if (recvfromproc(k)) then
!
            call MPI_RECV(iminmax_temp(:,:,k),2*nbdim,MPI_INTEGER,k,etiquette, &
                    MPI_COMM_WORLD,statut,code)

            imin_recv(:,k) = iminmax_temp(:,1,k)
            imax_recv(:,k) = iminmax_temp(:,2,k)
        endif
!
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine ExchangeSamelevel_first
!===================================================================================================
!
!===================================================================================================
!  subroutine ExchangeSameLevel2
!---------------------------------------------------------------------------------------------------
subroutine ExchangeSameLevel2 ( sendtoproc, recvfromproc, nbdim,    &
                                pttruetabwhole, cetruetabwhole,     &
                                imin, imax, imin_recv, imax_recv,   &
                                memberout, tempC, tempCextend )
!---------------------------------------------------------------------------------------------------
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1),       intent(in)    :: sendtoproc
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1),       intent(in)    :: recvfromproc
    INTEGER,                                     intent(in)    :: nbdim
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1), intent(in)    :: pttruetabwhole
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1), intent(in)    :: cetruetabwhole
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1), intent(in)    :: imin,      imax
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1), intent(in)    :: imin_recv, imax_recv
    LOGICAL,                                     intent(in)    :: memberout
    TYPE(Agrif_PVariable),                       intent(inout) :: tempC, tempCextend
!
    include 'mpif.h'
    INTEGER :: i,k
    INTEGER :: etiquette = 100
    INTEGER :: code, datasize
    INTEGER, DIMENSION(MPI_STATUS_SIZE)   :: statut
    TYPE(Agrif_PVariable), SAVE :: temprecv
!
    IF (memberout) THEN
        call Agrif_nbdim_allocation(tempCextend%var, pttruetabwhole(:,Agrif_ProcRank),  &
                                                     cetruetabwhole(:,Agrif_ProcRank),nbdim)
        call Agrif_nbdim_Full_VarEQreal(tempCextend%var,0.,nbdim)
    ENDIF
!
    IF (sendtoproc(Agrif_ProcRank)) THEN
        call Agrif_nbdim_VarEQvar(tempCextend%var,imin(:,Agrif_Procrank),imax(:,Agrif_Procrank), &
                                  tempC%var,      imin(:,Agrif_Procrank),imax(:,Agrif_Procrank), &
                                  nbdim)
    ENDIF
!
    do k = 0,Agrif_ProcRank-1
!
        if (sendtoproc(k)) then
!
            datasize = 1
!
!CDIR SHORTLOOP
            do i = 1,nbdim
                datasize = datasize * (imax(i,k)-imin(i,k)+1)
            enddo
!
            SELECT CASE(nbdim)
            CASE(1)
                call MPI_SEND(tempC%var%array1(imin(1,k):imax(1,k)),    &
                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,      &
                        MPI_COMM_WORLD,code)
            CASE(2)
                call MPI_SEND(tempC%var%array2(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k)),    &
                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,      &
                        MPI_COMM_WORLD,code)
            CASE(3)
                call Agrif_Send_3Darray(tempC%var%array3,lbound(tempC%var%array3),imin(:,k),imax(:,k),k)
            CASE(4)
                call MPI_SEND(tempC%var%array4(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k),     &
                                               imin(4,k):imax(4,k)),    &
                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,      &
                        MPI_COMM_WORLD,code)
            CASE(5)
                call MPI_SEND(tempC%var%array5(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k),     &
                                               imin(4,k):imax(4,k),     &
                                               imin(5,k):imax(5,k)),    &
                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,      &
                        MPI_COMM_WORLD,code)
            CASE(6)
                call MPI_SEND(tempC%var%array6(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k),     &
                                               imin(4,k):imax(4,k),     &
                                               imin(5,k):imax(5,k),     &
                                               imin(6,k):imax(6,k)),    &
                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,      &
                        MPI_COMM_WORLD,code)
            END SELECT
!
        endif
    enddo
!
!   Reception from others processors of the necessary part of the parent grid
    do k = Agrif_ProcRank+1,Agrif_Nbprocs-1
!
        if (recvfromproc(k)) then
!
            datasize = 1
!
!CDIR SHORTLOOP
            do i = 1,nbdim
                datasize = datasize * (imax_recv(i,k)-imin_recv(i,k)+1)
            enddo

            IF (.Not.Associated(temprecv%var)) allocate(temprecv%var)
            call Agrif_nbdim_allocation(temprecv%var,imin_recv(:,k),imax_recv(:,k),nbdim)

            SELECT CASE(nbdim)
            CASE(1)
                call MPI_RECV(temprecv%var%array1,datasize,MPI_DOUBLE_PRECISION,k,etiquette, &
                        MPI_COMM_WORLD,statut,code)
            CASE(2)
                call MPI_RECV(temprecv%var%array2,datasize,MPI_DOUBLE_PRECISION,k,etiquette, &
                        MPI_COMM_WORLD,statut,code)
            CASE(3)
                call MPI_RECV(temprecv%var%array3,datasize,MPI_DOUBLE_PRECISION,k,etiquette, &
                        MPI_COMM_WORLD,statut,code)
            CASE(4)
                call MPI_RECV(temprecv%var%array4,datasize,MPI_DOUBLE_PRECISION,k,etiquette, &
                        MPI_COMM_WORLD,statut,code)
            CASE(5)
                call MPI_RECV(temprecv%var%array5,datasize,MPI_DOUBLE_PRECISION,k,etiquette, &
                        MPI_COMM_WORLD,statut,code)
            CASE(6)
                call MPI_RECV(temprecv%var%array6,datasize,MPI_DOUBLE_PRECISION,k,etiquette, &
                        MPI_COMM_WORLD,statut,code)
            END SELECT

            call where_valtabtotab_mpi(tempCextend%var,temprecv%var, &
                    imin_recv(:,k),imax_recv(:,k),0.,nbdim)

            call Agrif_nbdim_deallocation(temprecv%var,nbdim)
!            deallocate(temprecv%var)
        endif
    enddo

!   Reception from others processors of the necessary part of the parent grid
    do k = Agrif_ProcRank+1,Agrif_Nbprocs-1
!
        if (sendtoproc(k)) then
!
            SELECT CASE(nbdim)
            CASE(1)
                datasize=SIZE(tempC%var%array1(imin(1,k):imax(1,k)))
                call MPI_SEND(tempC%var%array1(imin(1,k):imax(1,k)),    &
                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,      &
                        MPI_COMM_WORLD,code)
            CASE(2)
                datasize=SIZE(tempC%var%array2(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k)))
                call MPI_SEND(tempC%var%array2(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k)),    &
                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,      &
                        MPI_COMM_WORLD,code)
            CASE(3)
                datasize=SIZE(tempC%var%array3(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k)))
                call MPI_SEND(tempC%var%array3(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k)),    &
                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,      &
                        MPI_COMM_WORLD,code)
            CASE(4)
                datasize=SIZE(tempC%var%array4(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k),     &
                                               imin(4,k):imax(4,k)))
                call MPI_SEND(tempC%var%array4(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k),     &
                                               imin(4,k):imax(4,k)),    &
                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,      &
                        MPI_COMM_WORLD,code)
            CASE(5)
                datasize=SIZE(tempC%var%array5(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k),     &
                                               imin(4,k):imax(4,k),     &
                                               imin(5,k):imax(5,k)))
                call MPI_SEND(tempC%var%array5(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k),     &
                                               imin(4,k):imax(4,k),     &
                                               imin(5,k):imax(5,k)),    &
                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,      &
                        MPI_COMM_WORLD,code)
            CASE(6)
                datasize=SIZE(tempC%var%array6(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k),     &
                                               imin(4,k):imax(4,k),     &
                                               imin(5,k):imax(5,k),     &
                                               imin(6,k):imax(6,k)))
                call MPI_SEND(tempC%var%array6(imin(1,k):imax(1,k),     &
                                               imin(2,k):imax(2,k),     &
                                               imin(3,k):imax(3,k),     &
                                               imin(4,k):imax(4,k),     &
                                               imin(5,k):imax(5,k),     &
                                               imin(6,k):imax(6,k)),    &
                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,      &
                        MPI_COMM_WORLD,code)
            END SELECT
!
        endif
!
    enddo
!
!   Reception from others processors of the necessary part of the parent grid
    do k = Agrif_ProcRank-1,0,-1
!
        if (recvfromproc(k)) then
!
            IF (.Not.Associated(temprecv%var)) allocate(temprecv%var)
            call Agrif_nbdim_allocation(temprecv%var,imin_recv(:,k),imax_recv(:,k),nbdim)

            SELECT CASE(nbdim)
            CASE(1)
                datasize=SIZE(temprecv%var%array1)
                call MPI_RECV(temprecv%var%array1,datasize,MPI_DOUBLE_PRECISION,k,etiquette,&
                        MPI_COMM_WORLD,statut,code)
            CASE(2)
                datasize=SIZE(temprecv%var%array2)
                call MPI_RECV(temprecv%var%array2,datasize,MPI_DOUBLE_PRECISION,k,etiquette,&
                        MPI_COMM_WORLD,statut,code)
            CASE(3)
                datasize=SIZE(temprecv%var%array3)
                call MPI_RECV(temprecv%var%array3,datasize,MPI_DOUBLE_PRECISION,k,etiquette,&
                        MPI_COMM_WORLD,statut,code)
            CASE(4)
                datasize=SIZE(temprecv%var%array4)
                call MPI_RECV(temprecv%var%array4,datasize,MPI_DOUBLE_PRECISION,k,etiquette,&
                          MPI_COMM_WORLD,statut,code)
            CASE(5)
                datasize=SIZE(temprecv%var%array5)
                call MPI_RECV(temprecv%var%array5,datasize,MPI_DOUBLE_PRECISION,k,etiquette,&
                         MPI_COMM_WORLD,statut,code)
            CASE(6)
                datasize=SIZE(temprecv%var%array6)
                call MPI_RECV(temprecv%var%array6,datasize,MPI_DOUBLE_PRECISION,k,etiquette,&
                        MPI_COMM_WORLD,statut,code)
            END SELECT

            call where_valtabtotab_mpi(tempCextend%var,temprecv%var, &
                                       imin_recv(:,k),imax_recv(:,k),0.,nbdim)

            call Agrif_nbdim_deallocation(temprecv%var,nbdim)
!            deallocate(temprecv%var)
        endif
!
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine ExchangeSamelevel2
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Send_3Darray
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Send_3Darray ( tab3D, bounds, imin, imax, k )
!---------------------------------------------------------------------------------------------------
    integer, dimension(3),                                     intent(in) :: bounds
    real, dimension(bounds(1):,bounds(2):,bounds(3):), target, intent(in) :: tab3D
    integer, dimension(3),                                     intent(in) :: imin, imax
    integer,                                                   intent(in) :: k
!
    integer :: etiquette = 100
    integer :: datasize, code
    include 'mpif.h'

    datasize = SIZE(tab3D(imin(1):imax(1),  &
                          imin(2):imax(2),  &
                          imin(3):imax(3)))

    call MPI_SEND( tab3D( imin(1):imax(1),  &
                          imin(2):imax(2),  &
                          imin(3):imax(3)), &
                          datasize,MPI_DOUBLE_PRECISION,k,etiquette,MPI_COMM_WORLD,code)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Send_3Darray
!===================================================================================================
!
#else
!===================================================================================================
    subroutine Agrif_mpp_empty()
    end subroutine Agrif_mpp_empty
!===================================================================================================
#endif

end Module Agrif_Mpp
