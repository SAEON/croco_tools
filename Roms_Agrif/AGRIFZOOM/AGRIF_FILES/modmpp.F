!
! $Id: modcurgridfunctions.F,v 1.4 2005/08/22 15:11:29 agrif Exp $
!
C     AGRIF (Adaptive Grid Refinement In Fortran)
C
C     Copyright (C) 2003 Laurent Debreu (Laurent.Debreu@imag.fr)
C                        Christophe Vouland (Christophe.Vouland@imag.fr)
C
C     This program is free software; you can redistribute it and/or modify
C     it under the terms of the GNU General Public License as published by
C     the Free Software Foundation; either version 2 of the License, or
C     (at your option) any later version.
C
C     This program is distributed in the hope that it will be useful,
C     but WITHOUT ANY WARRANTY; without even the implied warranty of
C     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C     GNU General Public License for more details.
C
C     You should have received a copy of the GNU General Public License
C     along with this program; if not, write to the Free Software
C     Foundation, Inc., 59 Temple Place- Suite 330, Boston, MA 02111-1307, USA.
C
C
C
CCC   Module Agrif_mpp
C
      Module Agrif_mpp
      Use Agrif_Types
      Use Agrif_Arrays

      Contains
#ifdef AGRIF_MPI
C
      Subroutine Get_External_Data(tempC,tempCextend,pttruetab,
     &   cetruetab,pttruetabwhole,cetruetabwhole,nbdim,memberin,
     &   memberout)

      IMPLICIT NONE
#include "mpif.h"
      INTEGER :: nbdim
      TYPE(Agrif_PVariable) :: tempC, tempCextend
      INTEGER,DIMENSION(nbdim,0:Agrif_NbProcs-1)    :: pttruetab,
     &                                                 cetruetab
      INTEGER,DIMENSION(nbdim,0:Agrif_NbProcs-1)    :: pttruetabwhole,
     &                                                 cetruetabwhole
      INTEGER :: k,i,k2
      LOGICAL :: sendtoproc(0:Agrif_Nbprocs-1)
      INTEGER,DIMENSION(nbdim,0:Agrif_NbProcs-1)    :: imin,imax
      LOGICAL :: memberin, memberout
      INTEGER :: imintmp, imaxtmp,j,i1
      INTEGER :: imin1,imax1
      LOGICAL :: tochange,tochangebis
      INTEGER,DIMENSION(nbdim,0:Agrif_NbProcs-1)    :: pttruetab2,
     &                                                 cetruetab2
      LOGICAL :: memberout1(1),memberoutall(0:Agrif_Nbprocs-1)
      INTEGER :: code

C pttruetab2 and cetruetab2 are modified arraysin order to always
C send the most inner points

        
         memberout1(1) = memberout

         CALL MPI_ALLGATHER(memberout1,1,MPI_LOGICAL,memberoutall,
     &                  1,MPI_LOGICAL,MPI_COMM_WORLD,code)

         pttruetab2(:,Agrif_Procrank) = pttruetab(:,Agrif_Procrank)
         cetruetab2(:,Agrif_Procrank) = cetruetab(:,Agrif_Procrank)
         do k2=0,Agrif_Nbprocs-1
            do i=1,nbdim

           tochangebis=.TRUE.
           DO i1=1,nbdim
            IF (i .NE. i1) THEN
              IF ((pttruetab(i1,Agrif_Procrank).NE.pttruetab(i1,k2)).OR.
     &          (cetruetab(i1,Agrif_Procrank).NE.cetruetab(i1,k2))) THEN
                   tochangebis = .FALSE.
               EXIT
              ENDIF
             ENDIF
           ENDDO

           IF (tochangebis) THEN


          imin1 = max(pttruetab(i,Agrif_Procrank),
     &                    pttruetab(i,k2))
          imax1 = min(cetruetab(i,Agrif_Procrank),
     &                    cetruetab(i,k2))

C Always send the most interior points

           tochange = .false.
           IF (cetruetab(i,Agrif_Procrank)> cetruetab(i,k2)) THEN

           DO j=imin1,imax1
             IF ((cetruetab(i,k2)-j) >
     &             (j-pttruetab(i,Agrif_Procrank))) THEN
             imintmp = j+1
             tochange = .TRUE.
             ELSE
              EXIT
             ENDIF
          ENDDO
          ENDIF

           if (tochange) then
C
              pttruetab2(i,Agrif_Procrank) = imintmp
C
          endif

           tochange = .FALSE.
           imaxtmp=0
           IF (pttruetab(i,Agrif_Procrank) < pttruetab(i,k2)) THEN
          DO j=imax1,imin1,-1
            IF ((j-pttruetab(i,k2)) >
     &             (cetruetab(i,Agrif_Procrank)-j)) THEN
             imaxtmp = j-1
             tochange = .TRUE.
            ELSE
             EXIT
            ENDIF
          ENDDO

          ENDIF

                    if (tochange) then
C
              cetruetab2(i,Agrif_Procrank) = imaxtmp
C
          endif
C

          ENDIF
           enddo
         enddo


       do k = 0,Agrif_NbProcs-1
C
        sendtoproc(k) = .true.
C
        do i = 1,nbdim
C
          imin(i,k) = max(pttruetab2(i,Agrif_Procrank),
     &                    pttruetabwhole(i,k))
          imax(i,k) = min(cetruetab2(i,Agrif_Procrank),
     &                    cetruetabwhole(i,k))
C
          if (imin(i,k) > imax(i,k)) then
C
              sendtoproc(k) = .false.
C
          endif
C
        enddo
        IF (.NOT.memberoutall(k)) THEN
           sendtoproc(k) = .FALSE.
        ENDIF
C
      enddo


c       IF (.NOT.memberin) sendtoproc = .FALSE.

      IF (memberout) THEN
      Call Agrif_nbdim_allocation(tempCextend%var,
     &                 pttruetabwhole(:,Agrif_ProcRank),
     &                 cetruetabwhole(:,Agrif_ProcRank),nbdim)
      call Agrif_nbdim_Full_VarEQreal(tempCextend%var,0.,nbdim)
      ENDIF

      IF (sendtoproc(Agrif_ProcRank)) THEN
           Call Agrif_nbdim_VarEQvar(tempCextend%var,
     &                               imin(:,Agrif_Procrank),
     &                               imax(:,Agrif_Procrank),
     &                               tempC%var,
     &                               imin(:,Agrif_Procrank),
     &                               imax(:,Agrif_Procrank),
     &                               nbdim)
      ENDIF

      Call Exchangesamelevel(sendtoproc,nbdim,imin,imax,tempC,
     &     tempCextend)

      End Subroutine Get_External_Data

       Subroutine ExchangeSameLevel(sendtoproc,nbdim,imin,imax,
     &          tempC,tempCextend)
      Implicit None
      INTEGER :: nbdim
      INTEGER,DIMENSION(nbdim,0:Agrif_Nbprocs-1) :: imin,imax
      INTEGER,DIMENSION(nbdim,2,0:Agrif_Nbprocs-1) :: iminmax_temp
      INTEGER,DIMENSION(nbdim,0:Agrif_Nbprocs-1) :: imin_recv,imax_recv
      TYPE(Agrif_PVARIABLE) :: tempC,tempCextend
      LOGICAL,DIMENSION(0:Agrif_Nbprocs-1)       :: sendtoproc
      LOGICAL,DIMENSION(0:Agrif_Nbprocs-1)       :: recvfromproc
      LOGICAL                                    :: res
      TYPE(AGRIF_PVARIABLE)                      :: temprecv

#include "mpif.h"
          INTEGER :: i,k
          INTEGER :: etiquette = 100
          INTEGER :: code, datasize
          INTEGER,DIMENSION(MPI_STATUS_SIZE)   :: statut


      do k = 0,Agrif_ProcRank-1
C
C
            Call MPI_SEND(sendtoproc(k),1,MPI_LOGICAL,k,etiquette,
     &                    MPI_COMM_WORLD,code)
C
            if (sendtoproc(k)) then
C
                iminmax_temp(:,1,k) = imin(:,k)
                iminmax_temp(:,2,k) = imax(:,k)

                Call MPI_SEND(iminmax_temp(:,:,k),
     &                        2*nbdim,MPI_INTEGER,k,etiquette,
     &                        MPI_COMM_WORLD,code)
C
                datasize = 1
C
                do i = 1,nbdim
C
                  datasize = datasize * (imax(i,k)-imin(i,k)+1)
C
                enddo
C
                SELECT CASE(nbdim)
                CASE(1)
                   Call MPI_SEND(tempC%var%array1(
     &                        imin(1,k):imax(1,k)),
     &                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,
     &                        MPI_COMM_WORLD,code)
                CASE(2)
                   Call MPI_SEND(tempC%var%array2(
     &                        imin(1,k):imax(1,k),
     &                        imin(2,k):imax(2,k)),
     &                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,
     &                        MPI_COMM_WORLD,code)
                CASE(3)
                   Call MPI_SEND(tempC%var%array3(
     &                        imin(1,k):imax(1,k),
     &                        imin(2,k):imax(2,k),
     &                        imin(3,k):imax(3,k)),
     &                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,
     &                        MPI_COMM_WORLD,code)
                CASE(4)
                   Call MPI_SEND(tempC%var%array4(
     &                        imin(1,k):imax(1,k),
     &                        imin(2,k):imax(2,k),
     &                        imin(3,k):imax(3,k),
     &                        imin(4,k):imax(4,k)),
     &                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,
     &                        MPI_COMM_WORLD,code)
                CASE(5)
                   Call MPI_SEND(tempC%var%array5(
     &                        imin(1,k):imax(1,k),
     &                        imin(2,k):imax(2,k),
     &                        imin(3,k):imax(3,k),
     &                        imin(4,k):imax(4,k),
     &                        imin(5,k):imax(5,k)),
     &                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,
     &                        MPI_COMM_WORLD,code)
                CASE(6)
                   Call MPI_SEND(tempC%var%array6(
     &                        imin(1,k):imax(1,k),
     &                        imin(2,k):imax(2,k),
     &                        imin(3,k):imax(3,k),
     &                        imin(4,k):imax(4,k),
     &                        imin(5,k):imax(5,k),
     &                        imin(6,k):imax(6,k)),
     &                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,
     &                        MPI_COMM_WORLD,code)
                END SELECT
C
            endif

C
      enddo
C
C
C     Reception from others processors of the necessary part of the parent grid
      do k = Agrif_ProcRank+1,Agrif_Nbprocs-1
C
C
            Call MPI_RECV(res,1,MPI_LOGICAL,k,etiquette,
     &                    MPI_COMM_WORLD,statut,code)
C
            recvfromproc(k) = res

C
            if (recvfromproc(k)) then
C
                Call MPI_RECV(iminmax_temp(:,:,k),
     &                        2*nbdim,MPI_INTEGER,k,etiquette,
     &                        MPI_COMM_WORLD,statut,code)

                imin_recv(:,k) = iminmax_temp(:,1,k)
                imax_recv(:,k) = iminmax_temp(:,2,k)

                datasize = 1
C
                do i = 1,nbdim
C
                datasize = datasize * (imax_recv(i,k)-imin_recv(i,k)+1)
C
                enddo

                allocate(temprecv%var)
             call Agrif_nbdim_allocation(temprecv%var,imin_recv(:,k),
     &   imax_recv(:,k),nbdim)
            SELECT CASE(nbdim)
            CASE(1)
              Call MPI_RECV(temprecv%var%array1,
     &               datasize,MPI_DOUBLE_PRECISION,k,etiquette,
     &               MPI_COMM_WORLD,statut,code)
            CASE(2)
              Call MPI_RECV(temprecv%var%array2,
     &               datasize,MPI_DOUBLE_PRECISION,k,etiquette,
     &               MPI_COMM_WORLD,statut,code)
            CASE(3)
              Call MPI_RECV(temprecv%var%array3,
     &               datasize,MPI_DOUBLE_PRECISION,k,etiquette,
     &               MPI_COMM_WORLD,statut,code)

            CASE(4)
              Call MPI_RECV(temprecv%var%array4,
     &               datasize,MPI_DOUBLE_PRECISION,k,etiquette,
     &               MPI_COMM_WORLD,statut,code)
            CASE(5)
              Call MPI_RECV(temprecv%var%array5,
     &               datasize,MPI_DOUBLE_PRECISION,k,etiquette,
     &               MPI_COMM_WORLD,statut,code)
            CASE(6)
              Call MPI_RECV(temprecv%var%array6,
     &               datasize,MPI_DOUBLE_PRECISION,k,etiquette,
     &               MPI_COMM_WORLD,statut,code)
       END SELECT
            
            Call where_valtabtotab_mpi(tempCextend%var,
     &             temprecv%var,imin_recv(:,k),imax_recv(:,k),0.,nbdim)
     
                Call Agrif_nbdim_deallocation(temprecv%var,nbdim)
                deallocate(temprecv%var)

            endif

C
      enddo


      do k = Agrif_ProcRank+1,Agrif_Nbprocs-1
C
C
            Call MPI_SEND(sendtoproc(k),1,MPI_LOGICAL,k,etiquette,
     &                    MPI_COMM_WORLD,code)
C
            if (sendtoproc(k)) then
C
                iminmax_temp(:,1,k) = imin(:,k)
                iminmax_temp(:,2,k) = imax(:,k)

                Call MPI_SEND(iminmax_temp(:,:,k),
     &                        2*nbdim,MPI_INTEGER,k,etiquette,
     &                        MPI_COMM_WORLD,code)
C
                datasize = 1
C
                do i = 1,nbdim
C
                  datasize = datasize * (imax(i,k)-imin(i,k)+1)
C
                enddo
C
                SELECT CASE(nbdim)
                CASE(1)
                   Call MPI_SEND(tempC%var%array1(
     &                        imin(1,k):imax(1,k)),
     &                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,
     &                        MPI_COMM_WORLD,code)
                CASE(2)
                   Call MPI_SEND(tempC%var%array2(
     &                        imin(1,k):imax(1,k),
     &                        imin(2,k):imax(2,k)),
     &                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,
     &                        MPI_COMM_WORLD,code)
                CASE(3)
                   Call MPI_SEND(tempC%var%array3(
     &                        imin(1,k):imax(1,k),
     &                        imin(2,k):imax(2,k),
     &                        imin(3,k):imax(3,k)),
     &                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,
     &                        MPI_COMM_WORLD,code)
                CASE(4)
                   Call MPI_SEND(tempC%var%array4(
     &                        imin(1,k):imax(1,k),
     &                        imin(2,k):imax(2,k),
     &                        imin(3,k):imax(3,k),
     &                        imin(4,k):imax(4,k)),
     &                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,
     &                        MPI_COMM_WORLD,code)
                CASE(5)
                   Call MPI_SEND(tempC%var%array5(
     &                        imin(1,k):imax(1,k),
     &                        imin(2,k):imax(2,k),
     &                        imin(3,k):imax(3,k),
     &                        imin(4,k):imax(4,k),
     &                        imin(5,k):imax(5,k)),
     &                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,
     &                        MPI_COMM_WORLD,code)
                CASE(6)
                   Call MPI_SEND(tempC%var%array6(
     &                        imin(1,k):imax(1,k),
     &                        imin(2,k):imax(2,k),
     &                        imin(3,k):imax(3,k),
     &                        imin(4,k):imax(4,k),
     &                        imin(5,k):imax(5,k),
     &                        imin(6,k):imax(6,k)),
     &                        datasize,MPI_DOUBLE_PRECISION,k,etiquette,
     &                        MPI_COMM_WORLD,code)
                END SELECT
C
            endif

C
      enddo

C
C
C     Reception from others processors of the necessary part of the parent grid
      do k = Agrif_ProcRank-1,0,-1
C
C
            Call MPI_RECV(res,1,MPI_LOGICAL,k,etiquette,
     &                    MPI_COMM_WORLD,statut,code)
C
            recvfromproc(k) = res

C
            if (recvfromproc(k)) then
C
                Call MPI_RECV(iminmax_temp(:,:,k),
     &                        2*nbdim,MPI_INTEGER,k,etiquette,
     &                        MPI_COMM_WORLD,statut,code)

                imin_recv(:,k) = iminmax_temp(:,1,k)
                imax_recv(:,k) = iminmax_temp(:,2,k)

                datasize = 1
C
                do i = 1,nbdim
C
                datasize = datasize * (imax_recv(i,k)-imin_recv(i,k)+1)
C
                enddo
                allocate(temprecv%var)
             call Agrif_nbdim_allocation(temprecv%var,imin_recv(:,k),
     &   imax_recv(:,k),nbdim)
            SELECT CASE(nbdim)
            CASE(1)
              Call MPI_RECV(temprecv%var%array1,
     &               datasize,MPI_DOUBLE_PRECISION,k,etiquette,
     &               MPI_COMM_WORLD,statut,code)
            CASE(2)
              Call MPI_RECV(temprecv%var%array2,
     &               datasize,MPI_DOUBLE_PRECISION,k,etiquette,
     &               MPI_COMM_WORLD,statut,code)
            CASE(3)
              Call MPI_RECV(temprecv%var%array3,
     &               datasize,MPI_DOUBLE_PRECISION,k,etiquette,
     &               MPI_COMM_WORLD,statut,code)

            CASE(4)
              Call MPI_RECV(temprecv%var%array4,
     &               datasize,MPI_DOUBLE_PRECISION,k,etiquette,
     &               MPI_COMM_WORLD,statut,code)
            CASE(5)
              Call MPI_RECV(temprecv%var%array5,
     &               datasize,MPI_DOUBLE_PRECISION,k,etiquette,
     &               MPI_COMM_WORLD,statut,code)
            CASE(6)
              Call MPI_RECV(temprecv%var%array6,
     &               datasize,MPI_DOUBLE_PRECISION,k,etiquette,
     &               MPI_COMM_WORLD,statut,code)
          END SELECT
            
            Call where_valtabtotab_mpi(tempCextend%var,
     &             temprecv%var,imin_recv(:,k),imax_recv(:,k),0.,nbdim)
     
                Call Agrif_nbdim_deallocation(temprecv%var,nbdim)
                deallocate(temprecv%var)
            endif

C
      enddo

          End Subroutine ExchangeSamelevel

#else
      Subroutine Agrif_mpp_empty()
      End Subroutine Agrif_mpp_empty
#endif

      End Module Agrif_mpp
