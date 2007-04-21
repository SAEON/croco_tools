C     Agrif (Adaptive Grid Refinement In Fortran)
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
C     Foundation, Inc., 59 Temple Place-Suite 330, Boston, MA 02111-1307, USA.
C
C
C
CCC   Module Agrif_fluxmod
C
      Module Agrif_fluxmod
      Use Agrif_types
      Use Agrif_Arrays
      Use Agrif_Curgridfunctions
      
      CONTAINS  
      
      
      Subroutine Agrif_AllocateFlux(Flux,fluxtab)
      Type(Agrif_Flux), Pointer :: Flux
      Real, Dimension(:,:) :: fluxtab
      Type(Agrif_Profile), Pointer :: Profile
      Integer :: dimensio,n,n2
      INTEGER, DIMENSION(:,:), Pointer :: normalsizes
      INTEGER, DIMENSION(6) :: unitarray
      Type(Agrif_Variable), Pointer :: fluxtabvar
      Integer :: nbout
      
      Profile => Flux%profile
      dimensio = Profile%nbdim
      
      unitarray = 1
      
      do n=1,dimensio
        IF (Profile%posvar(n) == 1) THEN
          IF (Profile%interptab(n) == 'x') THEN
            Allocate(Flux%fluxtabx)
            fluxtabvar => Flux%fluxtabx
          ELSE IF (Profile%interptab(n) == 'y') THEN
            Allocate(Flux%fluxtaby)
            fluxtabvar => Flux%fluxtaby
          ELSE IF (Profile%interptab(n) == 'z') THEN
            Allocate(Flux%fluxtabz)
            fluxtabvar => Flux%fluxtabz
          ENDIF
          ALLOCATE(fluxtabvar%iarray2(2,6))
          normalsizes=>fluxtabvar%iarray2
          normalsizes(1,1) = 2
          nbout = 1
          DO n2 = 1,dimensio
            IF (n2 .NE. n) THEN
              nbout = nbout + 1
              If ((Profile%posvar(n2) == 1)
     &            .OR.(Profile%posvar(n2) == 2)) THEN
                IF (Profile%interptab(n2) == 'x') THEN
                  normalsizes(2,n2) = 
     &                Agrif_Curgrid%nb(1)/agrif_Curgrid%spaceref(1)
                  normalsizes(1,nbout) = 
     &                Agrif_Curgrid%nb(1)/agrif_Curgrid%spaceref(1)
                ELSE IF (Profile%interptab(n2) == 'y') THEN
                  normalsizes(2,n2) = 
     &                Agrif_Curgrid%nb(2)/agrif_Curgrid%spaceref(2)
                  normalsizes(1,nbout) = 
     &                Agrif_Curgrid%nb(2)/agrif_Curgrid%spaceref(2)
                ELSE IF (Profile%interptab(n2) == 'z') THEN
                  normalsizes(2,n2) = 
     &                Agrif_Curgrid%nb(3)/agrif_Curgrid%spaceref(3)
                  normalsizes(1,nbout) = 
     &                Agrif_Curgrid%nb(3)/agrif_Curgrid%spaceref(3)
                ENDIF
              ELSE
                normalsizes(2,n2) = SIZE(fluxtab,n2)
                normalsizes(1,nbout) = SIZE(fluxtab,n2)
              ENDIF
            ENDIF
          ENDDO          
        ENDIF
      enddo
      
      
      do n=1,dimensio
        IF (Profile%posvar(n) == 1) THEN
          IF (Profile%interptab(n) == 'x') THEN
            fluxtabvar => Flux%fluxtabx
          ELSE IF (Profile%interptab(n) == 'y') THEN
            fluxtabvar => Flux%fluxtaby
          ELSE IF (Profile%interptab(n) == 'z') THEN
            fluxtabvar => Flux%fluxtabz
          ENDIF
          Call Agrif_nbdim_allocation(fluxtabvar,unitarray(1:dimensio),
     &          fluxtabvar%iarray2(1,1:dimensio),dimensio)
        ENDIF
      enddo
      
      Flux%fluxallocated = .TRUE.
      
      End Subroutine Agrif_AllocateFlux
      
      FUNCTION Agrif_Search_Flux(fluxname)
      character*(*) fluxname
      Type(Agrif_Flux), Pointer :: Agrif_Search_Flux
      
      Type(Agrif_Flux), pointer :: parcours
      Logical :: foundflux
      
      foundflux = .FALSE.
      parcours => Agrif_Curgrid%fluxes
      
      Do While (Associated(parcours))
         IF (parcours % fluxname == fluxname) THEN
           foundflux = .TRUE.
           EXIT
         ENDIF
         parcours => parcours%nextflux
      End Do
      
      IF (.NOT.foundflux) THEN
      write(*,*) 'The array flux '''
     &           //TRIM(fluxname)//''' has not been declared'
      stop
      ENDIF      
      
      Agrif_Search_Flux => parcours
      
      End Function Agrif_Search_Flux
      
      Subroutine Agrif_Save_Fluxtab(Flux,Fluxtab)
      Type(Agrif_Flux), Pointer :: Flux
      Real, Dimension(:,:) :: Fluxtab
      INTEGER, DIMENSION(:,:), Pointer :: normalsizes
      INTEGER, DIMENSION(6) :: normalsizes2, normalsizes3
      INTEGER, DIMENSION(6) :: unitarray2, unitarray3
      Type(Agrif_Variable), Pointer :: fluxtabvar
      Type(Agrif_Profile), Pointer :: Profile
      Integer :: dimensio,n,n2,j,j1,j2
      
      Profile => Flux%profile
      dimensio = Profile%nbdim
            
      do n=1,dimensio
        IF (Profile%posvar(n) == 1) THEN
          IF (Profile%interptab(n) == 'x') THEN
            fluxtabvar => Flux%fluxtabx
          ELSE IF (Profile%interptab(n) == 'y') THEN
            fluxtabvar => Flux%fluxtaby
          ELSE IF (Profile%interptab(n) == 'z') THEN
            fluxtabvar => Flux%fluxtabz
          ENDIF 
          normalsizes => fluxtabvar%iarray2
          unitarray2 = 1
          unitarray3 = 1
          normalsizes2 = normalsizes(1,:)
          normalsizes3 = normalsizes(2,:)
          
          unitarray3(n) = Profile%point(n)
          normalsizes3(n) = Profile%point(n) 
                              
          SELECT CASE(dimensio)
          CASE(1)
          CASE(2)
            j1 = unitarray3(2)
            Do j=unitarray3(2),normalsizes3(2)
            do j2 = j1,j1+Agrif_curgrid%spaceref(2)
!            print *,'flux stocke fiun = ',j2,fluxtab(unitarray3(1),j2)
            enddo
            fluxtabvar%array2(1:1,j) = 
     &      fluxtabvar%array2(1:1,j) +
     &             SUM(fluxtab(unitarray3(1):normalsizes3(1),
     &                        j1:j1+Agrif_Curgrid%spaceref(2)))
             j1 = j1+Agrif_Curgrid%spaceref(2)
            EndDo
          END SELECT
          
          unitarray3(n) = Profile%point(n)+Agrif_Curgrid%nb(n)
          normalsizes3(n) = Profile%point(n)+Agrif_Curgrid%nb(n) 
          SELECT CASE(dimensio)
          CASE(1)
          CASE(2)
            j1 = unitarray3(2)
            Do j=unitarray3(2),normalsizes3(2)
            fluxtabvar%array2(2:2,j) = 
     &      fluxtabvar%array2(2:2,j) +
     &             SUM(fluxtab(unitarray3(1):normalsizes3(1),
     &                        j1:j1+Agrif_Curgrid%spaceref(2)))
             j1 = j1+Agrif_Curgrid%spaceref(2)
            EndDo
         END SELECT
        ENDIF
      enddo
                  
      End Subroutine Agrif_Save_Fluxtab

      Subroutine Agrif_Save_Fluxtab_child(Flux,Fluxtab)
      Type(Agrif_Flux), Pointer :: Flux
      Real, Dimension(:,:) :: Fluxtab
      INTEGER, DIMENSION(:,:), Pointer :: normalsizes
      INTEGER, DIMENSION(6) :: normalsizes2, normalsizes3
      INTEGER, DIMENSION(6) :: unitarray2, unitarray3
      Type(Agrif_Variable), Pointer :: fluxtabvar
      Type(Agrif_Profile), Pointer :: Profile
      Integer :: dimensio,n,n2
      
      Profile => Flux%profile
      dimensio = Profile%nbdim
            
      do n=1,dimensio
        IF (Profile%posvar(n) == 1) THEN
          IF (Profile%interptab(n) == 'x') THEN
            fluxtabvar => Flux%fluxtabx
          ELSE IF (Profile%interptab(n) == 'y') THEN
            fluxtabvar => Flux%fluxtaby
          ELSE IF (Profile%interptab(n) == 'z') THEN
            fluxtabvar => Flux%fluxtabz
          ENDIF 
          normalsizes => fluxtabvar%iarray2
          unitarray2 = 1
          unitarray3 = 1
          normalsizes2 = normalsizes(1,:)
          normalsizes3 = normalsizes(2,:)
          
          unitarray3(n) = Profile%point(n)+Agrif_Curgrid%ix(n)
          normalsizes3(n) = unitarray3(n)
                              
          SELECT CASE(dimensio)
          CASE(1)
          CASE(2)
            fluxtabvar%array2(1:1,
     &                        unitarray2(2):normalsizes2(2)) = 
     &      - fluxtab(unitarray3(1):normalsizes3(1),
     &                        unitarray3(2):normalsizes3(2))
!        print *,'flux stocke = ',fluxtab(unitarray3(1):normalsizes3(1),
!     &                        unitarray3(2):normalsizes3(2))
          END SELECT
          
          unitarray3(n) = unitarray3(n)+
     &         Agrif_Curgrid%nb(n)/Agrif_Curgrid%spaceref(n)
          normalsizes3(n) = unitarray3(n)
          SELECT CASE(dimensio)
          CASE(1)
          CASE(2)
            fluxtabvar%array2(2:2,
     &                        unitarray2(2):normalsizes2(2)) = 
     &      - fluxtab(unitarray3(1):normalsizes3(1),
     &                        unitarray3(2):normalsizes3(2))    
         END SELECT
        ENDIF
      enddo
                  
      End Subroutine Agrif_Save_Fluxtab_child
      
      Subroutine Agrif_Cancel_Fluxarray(Flux)
      Type(Agrif_Flux), Pointer :: Flux
      Type(Agrif_Variable), Pointer :: fluxtabvar
      Type(Agrif_Profile), Pointer :: Profile
      Integer :: dimensio,n,n2
      
      Profile => Flux%profile
      dimensio = Profile%nbdim
            
      do n=1,dimensio
        IF (Profile%posvar(n) == 1) THEN
          IF (Profile%interptab(n) == 'x') THEN
            fluxtabvar => Flux%fluxtabx
          ELSE IF (Profile%interptab(n) == 'y') THEN
            fluxtabvar => Flux%fluxtaby
          ELSE IF (Profile%interptab(n) == 'z') THEN
            fluxtabvar => Flux%fluxtabz
          ENDIF 
                              
          SELECT CASE(dimensio)
          CASE(1)
          CASE(2)
            fluxtabvar%array2 = 0.
          END SELECT
        ENDIF
      enddo
                  
      End Subroutine Agrif_Cancel_Fluxarray  
      
      Subroutine Agrif_FluxCorrect(Flux, procname)
      Type(Agrif_Flux), Pointer :: Flux
      External :: procname
      Type(Agrif_Variable), Pointer :: fluxtabvar
      Type(Agrif_Profile), Pointer :: Profile
      Integer :: dimensio,n,n2,j1,j2
      Integer, Dimension(:), Allocatable :: Loctab
      Integer :: locind
      
      Profile => Flux%profile
      dimensio = Profile%nbdim

      do n=1,dimensio
        IF (Profile%posvar(n) == 1) THEN
          IF (Profile%interptab(n) == 'x') THEN
            fluxtabvar => Flux%fluxtabx
            locind = 1
          ELSE IF (Profile%interptab(n) == 'y') THEN
            fluxtabvar => Flux%fluxtaby
            locind = 2
          ELSE IF (Profile%interptab(n) == 'z') THEN
            fluxtabvar => Flux%fluxtabz
            locind = 3
          ENDIF 
                              
          SELECT CASE(dimensio)
          CASE(1)
          CASE(2)
           Allocate(Loctab(2))
           Loctab(1) = Agrif_Curgrid%ix(locind)
           Loctab(2) = Agrif_Curgrid%ix(locind)+
     &         Agrif_Curgrid%nb(locind)/Agrif_Curgrid%spaceref(locind)
           j1 = agrif_curgrid%ix(2)
           j2 = agrif_curgrid%ix(2)+
     &        agrif_curgrid%nb(2)/Agrif_curgrid%spaceref(2)
           Call Agrif_ChildGrid_to_ParentGrid()
            Call procname(fluxtabvar%array2,Loctab(1),Loctab(2),j1,j2)
           Call Agrif_ParentGrid_to_ChildGrid()
          END SELECT
        ENDIF
      enddo
      
      If (Allocated(Loctab)) Deallocate(Loctab)
                  
      End Subroutine Agrif_FluxCorrect 
          
      End Module Agrif_fluxmod
