!
! $Id$
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
C     Foundation, Inc., 59 Temple Place -  Suite 330, Boston, MA 02111-1307, USA.
C
C
C
CCC   Module Agrif_Mask
C
      Module Agrif_Mask
C
CCC   Description:
CCC   Module for masks
C
C     Modules used: 
C
      Use Agrif_Types       
C
      IMPLICIT NONE
C
      CONTAINS
C     Define procedures contained in this module
C
C     **************************************************************************
C     Subroutine Agrif_CheckMasknD
C     **************************************************************************
C       
      Subroutine Agrif_CheckMasknD(tempP,parent,pbtab,petab,ppbtab,
     &               ppetab,noraftab,nbdim)
C
CCC   Description:
CCC   Subroutine called in the procedure Agrif_InterpnD to recalculate the value 
CCC   of the parent grid variable when this one is equal to Agrif_SpecialValue. 
C
C     Declarations:
C
       
C
C     Arrays arguments      
      INTEGER :: nbdim
      INTEGER,DIMENSION(nbdim) :: pbtab  ! Limits of the parent grid used  
      INTEGER,DIMENSION(nbdim) :: petab  ! interpolation of the child grid
      LOGICAL,DIMENSION(nbdim) :: noraftab
      INTEGER,DIMENSION(nbdim) :: ppbtab,ppetab
C
C     Pointer argument
      TYPE(AGRIF_PVARIABLE) :: tempP  ! Part of the parent grid used for 
                                      ! the interpolation of the child grid                     
C     Data TYPE argument                                   
      TYPE(AGRIF_PVARIABLE) :: parent      ! The parent grid
C
C     Local scalar
      INTEGER                   :: i0,j0,k0,l0,m0,n0
C      
C     Local arrays
C
C      
      SELECT CASE (nbdim)
      CASE (1)
         do i0 = pbtab(1),petab(1)
         IF (tempP%var%array1(i0)
     &                        == Agrif_SpecialValue) Then
            Call CalculNewValTempP((/i0/),
     &                             tempP,parent,
     &                             ppbtab,ppetab,
     &                             noraftab,nbdim)
         ENDIF
         enddo
      CASE (2)
         do j0 = pbtab(2),petab(2)
         do i0 = pbtab(1),petab(1)
         IF (tempP%var%array2(i0,j0)
     &                        == Agrif_SpecialValue) Then
            Call CalculNewValTempP((/i0,j0/),
     &                             tempP,parent,
     &                             ppbtab,ppetab,
     &                             noraftab,nbdim)
         ENDIF
         enddo  
         enddo
      CASE (3)
         do k0 = pbtab(3),petab(3)
         do j0 = pbtab(2),petab(2)
         do i0 = pbtab(1),petab(1)
         IF (tempP%var%array3(i0,j0,k0)
     &                        == Agrif_SpecialValue) Then
!------CDIR NEXPAND
            Call CalculNewValTempP3D((/i0,j0,k0/),
     &      tempP%var%array3(ppbtab(1),ppbtab(2),ppbtab(3)),
     &      parent%var%array3(ppbtab(1),ppbtab(2),ppbtab(3)),
     &                           ppbtab,ppetab,
     &                           noraftab,MaxSearch,Agrif_SpecialValue)
     
c            Call CalculNewValTempP((/i0,j0,k0/),
c     &                             tempP,parent,
c     &                             ppbtab,ppetab,
c     &                             noraftab,nbdim)
          
         ENDIF
         enddo
         enddo  
         enddo
      CASE (4)
         do l0 = pbtab(4),petab(4)
         do k0 = pbtab(3),petab(3)
         do j0 = pbtab(2),petab(2)
         do i0 = pbtab(1),petab(1)
         IF (tempP%var%array4(i0,j0,k0,l0) 
     &                        == Agrif_SpecialValue) Then
            Call CalculNewValTempP4D((/i0,j0,k0,l0/),
     &      tempP%var%array4(ppbtab(1),ppbtab(2),ppbtab(3),ppbtab(4)),
     &      parent%var%array4(ppbtab(1),ppbtab(2),ppbtab(3),ppbtab(4)),
     &                           ppbtab,ppetab,
     &                           noraftab,MaxSearch,Agrif_SpecialValue)
         ENDIF
         enddo
         enddo
         enddo  
         enddo
      CASE (5)
         do m0 = pbtab(5),petab(5)
         do l0 = pbtab(4),petab(4)
         do k0 = pbtab(3),petab(3)
         do j0 = pbtab(2),petab(2)
         do i0 = pbtab(1),petab(1)
         IF (tempP%var%array5(i0,j0,k0,l0,m0) 
     &                       == Agrif_SpecialValue) Then
            Call CalculNewValTempP((/i0,j0,k0,l0,m0/),
     &                             tempP,parent,
     &                             ppbtab,ppetab,
     &                             noraftab,nbdim)
         ENDIF
         enddo
         enddo
         enddo  
         enddo
         enddo
      CASE (6)
         do n0 = pbtab(6),petab(6)
         do m0 = pbtab(5),petab(5)
         do l0 = pbtab(4),petab(4)
         do k0 = pbtab(3),petab(3)
         do j0 = pbtab(2),petab(2)
         do i0 = pbtab(1),petab(1)
         IF (tempP%var%array6(i0,j0,k0,l0,m0,n0) 
     &                       == Agrif_SpecialValue) Then
            Call CalculNewValTempP((/i0,j0,k0,l0,m0,n0/),
     &                             tempP,parent,
     &                             ppbtab,ppetab,
     &                             noraftab,nbdim)
         ENDIF
         enddo
         enddo
         enddo
         enddo  
         enddo
         enddo
      END SELECT
C
C      
C     
      End Subroutine Agrif_CheckMasknD
C
C
C     **************************************************************************
C     Subroutine CalculNewValTempP
C     **************************************************************************
C       
      Subroutine CalculNewValTempP(indic,
     &               tempP,parent,ppbtab,
     &               ppetab,noraftab,nbdim)
C
CCC   Description:
CCC   Subroutine called in the procedure Agrif_InterpnD to recalculate the value 
CCC   of the parent grid variable when this one is equal to Agrif_SpecialValue. 
C
C     Declarations:
C
       
C
C     Arrays arguments      
      INTEGER :: nbdim
      INTEGER,DIMENSION(nbdim) :: indic
      LOGICAL,DIMENSION(nbdim) :: noraftab
      INTEGER,DIMENSION(nbdim) :: ppbtab,ppetab
C
C     Pointer argument
      TYPE(AGRIF_PVARIABLE) :: tempP  ! Part of the parent grid used for 
                                      ! the interpolation of the child grid                     
C     Data TYPE argument                                   
      TYPE(AGRIF_PVARIABLE) :: parent      ! The parent grid
C
C     Local scalar
      INTEGER                  :: i,ii,iii,jj,kk,ll,mm,nn 
      INTEGER,DIMENSION(nbdim) :: imin,imax,idecal
      INTEGER                  :: Nbvals
      REAL                     :: Res
      REAL                     :: ValParent
      INTEGER                  :: ValMax
      LOGICAL                  :: firsttest
C      
C     Local arrays      
C
      ValMax = 1
      do iii = 1 , nbdim
         IF (.NOT.noraftab(iii)) THEN
         ValMax = max(ValMax,ppetab(iii)-indic(iii))
         ValMax = max(ValMax,indic(iii)-ppbtab(iii))
         ENDIF
      enddo
C
      Valmax = min(Valmax,MaxSearch)
C
!CDIR NOVECTOR
      imin = indic
!CDIR NOVECTOR
      imax = indic
C
         i = 1
         firsttest = .TRUE.
C
         do While(i <= ValMax)
C
         IF ((i == 1).AND.(firsttest)) i = Valmax

            do iii = 1 , nbdim
               if (.NOT.noraftab(iii)) then
                  imin(iii) = max(indic(iii) - i,ppbtab(iii))
                  imax(iii) = min(indic(iii) + i,ppetab(iii))
                  if (firsttest) then               
                  if (indic(iii).GT.ppbtab(iii)) then

!CDIR NOVECTOR
                     idecal = indic
                     idecal(iii) = idecal(iii)-1
                     SELECT CASE(nbdim)
                     CASE (1)
                        if (tempP%var%array1(idecal(1)
     &                            ) == Agrif_SpecialValue) then
                           imin(iii) = imax(iii)
                        endif                     
                     CASE (2)
                        if (tempP%var%array2(idecal(1),
     &            idecal(2)) == Agrif_SpecialValue) then
                           imin(iii) = imax(iii)
                        endif
                     CASE (3)
                        if (tempP%var%array3(idecal(1),
     &            idecal(2),idecal(3)) 
     &                               == Agrif_SpecialValue) then
                           imin(iii) = imax(iii)
                        endif  
                     CASE (4)
                        if (tempP%var%array4(idecal(1),
     &            idecal(2),idecal(3),idecal(4)) 
     &                               == Agrif_SpecialValue) then
                           imin(iii) = imax(iii)
                        endif     
                     CASE (5)
                        if (tempP%var%array5(idecal(1),
     &            idecal(2),idecal(3),idecal(4),idecal(5)) 
     &                               == Agrif_SpecialValue) then
                           imin(iii) = imax(iii)
                        endif  
                     CASE (6)
                        if (tempP%var%array6(idecal(1),
     &            idecal(2),idecal(3),idecal(4),idecal(5),idecal(6)) 
     &                               == Agrif_SpecialValue) then
                           imin(iii) = imax(iii)
                        endif                                                                                           
                     END SELECT
                  endif
                  endif
               endif            
            enddo
C
            Res = 0.
            Nbvals = 0
C
            SELECT CASE(nbdim)
            CASE (1)
!CDIR ALTCODE
!CDIR SHORTLOOP
               do ii = imin(1),imax(1)
                    ValParent = parent%var%array1(ii)
                    if ( ValParent .NE. Agrif_SpecialValue) then
                        Res = Res + ValParent
                        Nbvals = Nbvals + 1
                    endif 
               enddo
C
            CASE (2)
               do jj = imin(2),imax(2)
!CDIR ALTCODE
!CDIR SHORTLOOP
               do ii = imin(1),imax(1)
                    ValParent = parent%var%array2(ii,jj)
                    if ( ValParent .NE. Agrif_SpecialValue) then
                        Res = Res + ValParent
                        Nbvals = Nbvals + 1
                    endif 
               enddo  
               enddo
               
            CASE (3)
               do kk = imin(3),imax(3)
               do jj = imin(2),imax(2)
!CDIR ALTCODE
!CDIR SHORTLOOP
               do ii = imin(1),imax(1)
                    ValParent = parent%var%array3(ii,jj,kk)
                    if ( ValParent .NE. Agrif_SpecialValue) then
                        Res = Res + ValParent
                        Nbvals = Nbvals + 1
                    endif 
                        enddo
                  enddo  
               enddo

            CASE (4)
               do ll = imin(4),imax(4)
               do kk = imin(3),imax(3)
               do jj = imin(2),imax(2)
!CDIR ALTCODE
!CDIR SHORTLOOP
               do ii = imin(1),imax(1)
                    ValParent = parent%var%array4(ii,jj,kk,ll)
                    if ( ValParent .NE. Agrif_SpecialValue) then
                        Res = Res + ValParent
                        Nbvals = Nbvals + 1
                    endif 
                              enddo
                        enddo
                  enddo  
               enddo

            CASE (5)
               do mm = imin(5),imax(5)
               do ll = imin(4),imax(4)
               do kk = imin(3),imax(3)
               do jj = imin(2),imax(2)
!CDIR ALTCODE
!CDIR SHORTLOOP
               do ii = imin(1),imax(1)
                    ValParent = parent%var%array5(ii,jj,kk,ll,mm)
                    if ( ValParent .NE. Agrif_SpecialValue) then
                        Res = Res + ValParent
                        Nbvals = Nbvals + 1
                    endif 
                                    enddo
                              enddo
                        enddo
                  enddo  
               enddo

            CASE (6)
               do nn = imin(6),imax(6)
               do mm = imin(5),imax(5)
               do ll = imin(4),imax(4)
               do kk = imin(3),imax(3)
               do jj = imin(2),imax(2)
!CDIR ALTCODE
!CDIR SHORTLOOP
               do ii = imin(1),imax(1)
                    ValParent = parent%var%array6(ii,jj,kk,ll,mm,nn)
                    if ( ValParent .NE. Agrif_SpecialValue) then
                        Res = Res + ValParent
                        Nbvals = Nbvals + 1
                    endif 
                                          enddo
                                    enddo
                              enddo
                        enddo
                  enddo  
               enddo

            END SELECT
C
C
            
            if (Nbvals.GT.0) then
              if (firsttest) then
                   firsttest = .FALSE.
                   i=1
                   cycle
              endif
            SELECT CASE(nbdim)
            CASE (1)              
              tempP%var%array1(indic(1)) 
     &           = Res/Nbvals
            CASE (2)
              tempP%var%array2(indic(1),
     &                            indic(2)) = Res/Nbvals
            CASE (3)
              tempP%var%array3(indic(1),
     &                            indic(2),indic(3)) = Res/Nbvals
            CASE (4)
              tempP%var%array4(indic(1),
     &                            indic(2),indic(3),indic(4))
     &                = Res/Nbvals
            CASE (5)
              tempP%var%array5(indic(1),
     &                            indic(2),indic(3),indic(4),
     &                   indic(5)) = Res/Nbvals
            CASE (6)
              tempP%var%array6(indic(1),
     &                            indic(2),indic(3),indic(4),
     &                           indic(5),indic(6)) = Res/Nbvals
            END SELECT
              exit
            else
               if (firsttest) exit
               i = i + 1                      
            endif
          enddo            
C     
      End Subroutine CalculNewValTempP
C
C
      End Module Agrif_Mask     
      
      Subroutine CalculNewValTempP3D(indic,
     &               tempP,parent,ppbtab,
     &               ppetab,noraftab,MaxSearch,Agrif_SpecialValue)
C
CCC   Description:
CCC   Subroutine called in the procedure Agrif_InterpnD to recalculate the value 
CCC   of the parent grid variable when this one is equal to Agrif_SpecialValue. 
C
C     Declarations:
C
       
C
C     Arrays arguments      
      INTEGER, PARAMETER :: nbdim = 3
      INTEGER,DIMENSION(nbdim) :: indic
      LOGICAL,DIMENSION(nbdim) :: noraftab
      INTEGER,DIMENSION(nbdim) :: ppbtab,ppetab
      REAL :: Agrif_SpecialValue
C
C     Pointer argument
      REAL,DIMENSION(ppbtab(1):ppetab(1),ppbtab(2):ppetab(2),
     &    ppbtab(3):ppetab(3)) :: tempP, parent  ! Part of the parent grid used for 
                                      ! the interpolation of the child grid                     
C
C     Local scalar
      INTEGER                  :: i,ii,iii,jj,kk,ll,mm,nn 
      INTEGER,DIMENSION(nbdim) :: imin,imax,idecal
      INTEGER                  :: Nbvals
      REAL                     :: Res
      REAL                     :: ValParent
      INTEGER                  :: ValMax
      INTEGER :: MaxSearch
      LOGICAL :: Existunmasked
C      
C     Local arrays      
C
      ValMax = 1
!CDIR NOVECTOR
      do iii = 1 , nbdim
         IF (.NOT.noraftab(iii)) THEN
         ValMax = max(ValMax,ppetab(iii)-indic(iii))
         ValMax = max(ValMax,indic(iii)-ppbtab(iii))
         ENDIF
      enddo
C
      Valmax = min(Valmax,MaxSearch)
C
!CDIR NOVECTOR
      imin = indic
!CDIR NOVECTOR
      imax = indic
      
!CDIR NOVECTOR
         idecal = indic	 
C
         i = Valmax

            do iii = 1 , nbdim
               if (.NOT.noraftab(iii)) then
                  imin(iii) = max(indic(iii) - i,ppbtab(iii))
                  imax(iii) = min(indic(iii) + i,ppetab(iii))
         
                  if (indic(iii).GT.ppbtab(iii)) then

                     idecal(iii) = idecal(iii)-1

                        if (tempP(idecal(1),idecal(2),idecal(3)) 
     &                               == Agrif_SpecialValue) then
                           imin(iii) = imax(iii)
                        endif

                     idecal(iii) = idecal(iii)+1
                  endif

               endif            
            enddo
C
            Existunmasked = .FALSE.
C
               do kk = imin(3),imax(3)
               do jj = imin(2),imax(2)
!CDIR NOVECTOR
               do ii = imin(1),imax(1)
                    if ( parent(ii,jj,kk) .NE. Agrif_SpecialValue) then
                        Existunmasked = .TRUE.
                       exit
                    endif 
                        enddo
                  enddo  
               enddo
C
C
          if (.Not.Existunmasked) return
C
         i = 1
C
         do While(i <= ValMax)
C

            do iii = 1 , nbdim
               if (.NOT.noraftab(iii)) then
                  imin(iii) = max(indic(iii) - i,ppbtab(iii))
                  imax(iii) = min(indic(iii) + i,ppetab(iii))
               endif            
            enddo
C
            Res = 0.
            Nbvals = 0
C
               do kk = imin(3),imax(3)
               do jj = imin(2),imax(2)
!CDIR NOVECTOR
               do ii = imin(1),imax(1)
                    ValParent = parent(ii,jj,kk)
                    if ( ValParent .NE. Agrif_SpecialValue) then
                        Res = Res + ValParent
                        Nbvals = Nbvals + 1
                    endif 
                        enddo
                  enddo  
               enddo
C
C
            
            if (Nbvals.GT.0) then
              tempP(indic(1),indic(2),indic(3)) = Res/Nbvals
              exit
            else
               i = i + 1                      
            endif
          enddo            
C     
      End Subroutine CalculNewValTempP3D        

      Subroutine CalculNewValTempP4D(indic,
     &               tempP,parent,ppbtab,
     &               ppetab,noraftab,MaxSearch,Agrif_SpecialValue)
C
CCC   Description:
CCC   Subroutine called in the procedure Agrif_InterpnD to recalculate the value 
CCC   of the parent grid variable when this one is equal to Agrif_SpecialValue. 
C
C     Declarations:
C
       
C
C     Arrays arguments      
      INTEGER, PARAMETER :: nbdim = 4
      INTEGER,DIMENSION(nbdim) :: indic
      LOGICAL,DIMENSION(nbdim) :: noraftab
      INTEGER,DIMENSION(nbdim) :: ppbtab,ppetab
      INTEGER :: MaxSearch      
      REAL :: Agrif_SpecialValue      
C
C     Pointer argument
      REAL,DIMENSION(ppbtab(1):ppetab(1),ppbtab(2):ppetab(2),
     &    ppbtab(3):ppetab(3),
     &    ppbtab(4):ppetab(4)) :: tempP, parent  ! Part of the parent grid used for 
                                      ! the interpolation of the child grid                     
C
C     Local scalar
      INTEGER                  :: i,ii,iii,jj,kk,ll,mm,nn 
      INTEGER,DIMENSION(nbdim) :: imin,imax,idecal
      INTEGER                  :: Nbvals
      REAL                     :: Res
      REAL                     :: ValParent
      INTEGER                  :: ValMax
      LOGICAL                  :: firsttest
C      
C     Local arrays      
C
      ValMax = 1
      do iii = 1 , nbdim
         IF (.NOT.noraftab(iii)) THEN
         ValMax = max(ValMax,ppetab(iii)-indic(iii))
         ValMax = max(ValMax,indic(iii)-ppbtab(iii))
         ENDIF
      enddo
C
      Valmax = min(Valmax,MaxSearch)
C
      imin = indic
      imax = indic
C
         i = 1
         firsttest = .TRUE.
         idecal = indic	 
	 
C
         do While(i <= ValMax)
C
         IF ((i == 1).AND.(firsttest)) i = Valmax

            do iii = 1 , nbdim
               if (.NOT.noraftab(iii)) then
                  imin(iii) = max(indic(iii) - i,ppbtab(iii))
                  imax(iii) = min(indic(iii) + i,ppetab(iii))
                  if (firsttest) then               
                  if (indic(iii).GT.ppbtab(iii)) then


                     idecal(iii) = idecal(iii)-1
		     
                     if (tempP(idecal(1),idecal(2),idecal(3),idecal(4))
     &                               == Agrif_SpecialValue) then
                           imin(iii) = imax(iii)
                     endif
		     
		     idecal(iii) = idecal(iii)+1
                  endif
                  endif
               endif            
            enddo
C
            Res = 0.
            Nbvals = 0
C
               do ll = imin(4),imax(4)
               do kk = imin(3),imax(3)
               do jj = imin(2),imax(2)
               do ii = imin(1),imax(1)
                    ValParent = parent(ii,jj,kk,ll)
                    if ( ValParent .NE. Agrif_SpecialValue) then
                        Res = Res + ValParent
                        Nbvals = Nbvals + 1
                    endif 
                              enddo
                        enddo
                  enddo  
               enddo
C
C
            
            if (Nbvals.GT.0) then
              if (firsttest) then
                   firsttest = .FALSE.
                   i=1
                   cycle
              endif
	      
              tempP(indic(1),indic(2),indic(3),indic(4)) = Res/Nbvals
	    
              exit
            else
               if (firsttest) exit
               i = i + 1                      
            endif
          enddo            
C     
      End Subroutine CalculNewValTempP4D        
