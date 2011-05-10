

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
C     Foundation, Inc., 59 Temple Place- Suite 330, Boston, MA 02111-1307, USA.
C
C
C
CCC   Module Agrif_Update
C
      Module Agrif_Update
C
CCC   Description:
CCC   Module to update a parent grid from its child grids
C
C     Modules used:
C   
      Use Agrif_Updatebasic
c      Use Agrif_Boundary
      Use Agrif_Arrays
      Use Agrif_CurgridFunctions
      Use Agrif_Mask



C
      IMPLICIT NONE
C      
      CONTAINS
C     Define procedures contained in this module
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Update_1d
C     **************************************************************************
C 
      Subroutine Agrif_Update_1d(TypeUpdate,parent,child,tab,deb,fin,
     &                           procname)
C
CCC   Description:
CCC   Subroutine to update a 1D grid variable on the parent grid.
C
C     Declarations:
C
      
C
C     Arguments      
      INTEGER, DIMENSION(6) :: TypeUpdate                  ! TYPE of update (copy or average)
      TYPE(AGRIF_PVariable) :: parent        ! Variable on the parent grid
      TYPE(AGRIF_PVariable) :: child         ! Variable on the child grid
      TYPE(AGRIF_PVariable) :: childtemp     ! Temporary variable on the child
      INTEGER :: deb,fin                      ! Positions where interpolations 
                                              ! are done on the fine grid       
      External :: procname
      Optional ::  procname      
      REAL, DIMENSION(lbound(child%var%array1,1):
     &                ubound(child%var%array1,1)), Target :: tab  ! Results
C
C
C     Definition of a temporary AGRIF_PVariable data TYPE  
      allocate(childtemp % var)
C
C     Pointer on the root variable
      childtemp % var % root_var => child % var %root_var
C      
C     Number of dimensions of the grid variable
      childtemp % var % nbdim = 1  
C      
C     Values on the current grid used for the update
      childtemp % var % array1 => tab      
C
     
      IF (present(procname)) THEN
      CALL Agrif_UpdateVariable
     &     (TypeUpdate,parent,child,deb,fin,procname)
      ELSE
      CALL Agrif_UpdateVariable
     &     (TypeUpdate,parent,child,deb,fin)
      ENDIF     
C      
      deallocate(childtemp % var)
C
C       
      End Subroutine Agrif_Update_1D
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Update_2d
C     **************************************************************************
C 

      Subroutine Agrif_Update_2d(TypeUpdate,parent,child,tab,deb,fin,
     &                           procname)
C
CCC   Description:
CCC   Subroutine to update a 2D grid variable on the parent grid.
C
C     Declarations:
C
      
C
C     Arguments      
      INTEGER, DIMENSION(6) :: TypeUpdate                  ! TYPE of update (copy or average)
      TYPE(AGRIF_PVariable) :: parent        ! Variable on the parent grid
      TYPE(AGRIF_PVariable) :: child         ! Variable on the child grid
      TYPE(AGRIF_PVariable) :: childtemp     ! Temporary variable on the child
      INTEGER :: deb,fin                      ! Positions where interpolations 
                                              ! are done on the fine grid 
                                              
      External :: procname
      Optional ::  procname
                                                          
      REAL, DIMENSION(
     &      lbound(child%var%array2,1):ubound(child%var%array2,1),
     &      lbound(child%var%array2,2):ubound(child%var%array2,2)),
     &      Target :: tab  ! Results
C
C
C     Definition of a temporary AGRIF_PVariable data TYPE
      allocate(childtemp % var)
C
C     Pointer on the root variable
      childtemp % var % root_var => child % var %root_var
C      
C     Number of dimensions of the grid variable
      childtemp % var % nbdim = 2  
C      
C     Values on the current grid used for the update
      childtemp % var % array2 => tab      
C
      IF (present(procname)) THEN
      CALL Agrif_UpdateVariable
     &     (TypeUpdate,parent,child,deb,fin,procname)
      ELSE
      CALL Agrif_UpdateVariable
     &     (TypeUpdate,parent,child,deb,fin)
      ENDIF
C      
      deallocate(childtemp % var)
C
C       
      End Subroutine Agrif_Update_2D
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Update_3d
C     **************************************************************************
C 
      Subroutine Agrif_Update_3d(TypeUpdate,parent,child,tab,deb,fin,
     &                           procname)
C
CCC   Description:
CCC   Subroutine to update a 3D grid variable on the parent grid.
C
C     Declarations:
C
      
C
C     Arguments      
      INTEGER, DIMENSION(6) :: TypeUpdate                  ! TYPE of update (copy or average)
      TYPE(AGRIF_PVariable) :: parent        ! Variable on the parent grid
      TYPE(AGRIF_PVariable) :: child         ! Variable on the child grid
      TYPE(AGRIF_PVariable) :: childtemp     ! Temporary variable on the child
      INTEGER :: deb,fin                      ! Positions where interpolations 
                                              ! are done on the fine grid    
      External :: procname
      Optional ::  procname
                                                       
      REAL, DIMENSION(
     &      lbound(child%var%array3,1):ubound(child%var%array3,1),
     &      lbound(child%var%array3,2):ubound(child%var%array3,2),
     &      lbound(child%var%array3,3):ubound(child%var%array3,3)),
     &      Target :: tab  ! Results    
C
C
C     Definition of a temporary AGRIF_PVariable data TYPE  
      allocate(childtemp % var)
C
C     Pointer on the root variable
      childtemp % var % root_var => child % var %root_var
C      
C     Number of dimensions of the grid variable
      childtemp % var % nbdim = 3  
C      
C     Values on the current grid used for the update
      childtemp % var % array3 => tab     
C
      IF (present(procname)) THEN
      CALL Agrif_UpdateVariable
     &     (TypeUpdate,parent,child,deb,fin,procname)
      ELSE
      CALL Agrif_UpdateVariable
     &     (TypeUpdate,parent,child,deb,fin)
      ENDIF
C      
      DEALLOCATE(childtemp % var)
C
C       
      End Subroutine Agrif_Update_3D
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Update_4d
C     **************************************************************************
C 
      Subroutine Agrif_Update_4d(TypeUpdate,parent,child,tab,deb,fin,
     &                           procname)
C
CCC   Description:
CCC   Subroutine to update a 4D grid variable on the parent grid.
C
C     Declarations:
C
      
C
C     Arguments      
      INTEGER, DIMENSION(6) :: TypeUpdate                  ! TYPE of update (copy or average)
      TYPE(AGRIF_PVariable) :: parent        ! Variable on the parent grid
      TYPE(AGRIF_PVariable) :: child         ! Variable on the child grid
      TYPE(AGRIF_PVariable) :: childtemp     ! Temporary variable on the child
      INTEGER :: deb,fin                      ! Positions where interpolations 
                                              ! are done on the fine grid     
      External :: procname
      Optional ::  procname        
      REAL, DIMENSION(
     &      lbound(child%var%array4,1):ubound(child%var%array4,1),
     &      lbound(child%var%array4,2):ubound(child%var%array4,2),
     &      lbound(child%var%array4,3):ubound(child%var%array4,3),
     &      lbound(child%var%array4,4):ubound(child%var%array4,4)),
     &      Target :: tab  ! Results
C
C
C     Definition of a temporary AGRIF_PVariable data TYPE  
      allocate(childtemp % var)
C
C     Pointer on the root variable
      childtemp % var % root_var => child % var %root_var
C      
C     Number of dimensions of the grid variable
      childtemp % var % nbdim = 4  
C      
C     Values on the current grid used for the update
      childtemp % var % array4 => tab     
C
      IF (present(procname)) THEN
      CALL Agrif_UpdateVariable
     &     (TypeUpdate,parent,child,deb,fin,procname)
      ELSE
      CALL Agrif_UpdateVariable
     &     (TypeUpdate,parent,child,deb,fin)
      ENDIF
C
      deallocate(childtemp % var)
C
C       
      End Subroutine Agrif_Update_4D
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Update_5d
C     **************************************************************************
C 
      Subroutine Agrif_Update_5d(TypeUpdate,parent,child,tab,deb,fin,
     &                           procname)
C
CCC   Description:
CCC   Subroutine to update a 5D grid variable on the parent grid.
C
C     Declarations:
C
      
C
C     Arguments      
      INTEGER, DIMENSION(6) :: TypeUpdate                  ! TYPE of update (copy or average)
      TYPE(AGRIF_PVariable) :: parent        ! Variable on the parent grid
      TYPE(AGRIF_PVariable) :: child         ! Variable on the child grid
      TYPE(AGRIF_PVariable) :: childtemp     ! Temporary variable on the child
      INTEGER :: deb,fin                      ! Positions where interpolations 
                                              ! are done on the fine grid     
      External :: procname
      Optional ::  procname
              
      REAL, DIMENSION(
     &      lbound(child%var%array5,1):ubound(child%var%array5,1),
     &      lbound(child%var%array5,2):ubound(child%var%array5,2),
     &      lbound(child%var%array5,3):ubound(child%var%array5,3),
     &      lbound(child%var%array5,4):ubound(child%var%array5,4),
     &      lbound(child%var%array5,5):ubound(child%var%array5,5)),
     &      Target :: tab  ! Results
C
C
C     Definition of a temporary AGRIF_PVariable data TYPE  
      allocate(childtemp % var)
C
C     Pointer on the root variable
      childtemp % var % root_var => child % var %root_var
C
C     Number of dimensions of the grid variable
      childtemp % var % nbdim = 5  
C      
C     Values on the current grid used for the update
      childtemp % var % array5 => tab      
C
      IF (present(procname)) THEN
      CALL Agrif_UpdateVariable
     &     (TypeUpdate,parent,child,deb,fin,procname)
      ELSE
      CALL Agrif_UpdateVariable
     &     (TypeUpdate,parent,child,deb,fin)
      ENDIF
C      
      deallocate(childtemp % var)
C
C       
      End Subroutine Agrif_Update_5D
C
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Update_6d
C     **************************************************************************
C 
      Subroutine Agrif_Update_6d(TypeUpdate,parent,child,tab,deb,fin)
C
CCC   Description:
CCC   Subroutine to update a 6D grid variable on the parent grid.
C
C     Declarations:
C
      
C
C     Arguments      
      INTEGER, DIMENSION(6) :: TypeUpdate                  ! TYPE of update (copy or average)
      TYPE(AGRIF_PVariable) :: parent        ! Variable on the parent grid
      TYPE(AGRIF_PVariable) :: child         ! Variable on the child grid
      TYPE(AGRIF_PVariable) :: childtemp     ! Temporary variable on the child
      INTEGER :: deb,fin                      ! Positions where interpolations 
                                              ! are done on the fine grid       
      REAL, DIMENSION(
     &      lbound(child%var%array6,1):ubound(child%var%array6,1),
     &      lbound(child%var%array6,2):ubound(child%var%array6,2),
     &      lbound(child%var%array6,3):ubound(child%var%array6,3),
     &      lbound(child%var%array6,4):ubound(child%var%array6,4),
     &      lbound(child%var%array6,5):ubound(child%var%array6,5),
     &      lbound(child%var%array6,6):ubound(child%var%array6,6)),
     &      Target :: tab  ! Results
C
C
C     Definition of a temporary AGRIF_PVariable data TYPE  
      allocate(childtemp % var)
C
C     Pointer on the root variable
      childtemp % var % root_var => child % var %root_var
C      
C     Number of dimensions of the grid variable
      childtemp % var % nbdim = 6  
C      
C     Values on the current grid used for the update
      childtemp % var % array6 => tab      
C
      Call Agrif_UpdateVariable
     &     (TypeUpdate,parent,child,deb,fin)
C      
      deallocate(childtemp % var)
C
C       
      End Subroutine Agrif_Update_6D
C
C
C
C     **************************************************************************  
C     Subroutine Agrif_UpdateVariable    
C     **************************************************************************  
C   
      Subroutine Agrif_UpdateVariable(TypeUpdate,parent,child,deb,fin,
     &                   procname)   
C
CCC   Description:
CCC   Subroutine to set arguments of Agrif_UpdatenD, n being the number of
C         dimensions of the grid variable.
C
CC    Declarations:
C      
c      
C      
C     Scalar argument
      INTEGER, DIMENSION(6) :: TypeUpdate                  ! TYPE of update (copy or average)
C     Data TYPE arguments
      TYPE(AGRIF_PVariable) :: parent   ! Variable on the parent grid
      TYPE(AGRIF_PVariable) :: child    ! Variable on the child grid 
      INTEGER               :: deb,fin  ! Positions where boundary conditions
                                        !    are calculated     
      External :: procname
      Optional ::  procname
      
C
C     Local scalars      
      INTEGER :: nbdim                  ! Number of dimensions of the current
                                        !    grid
      INTEGER ,DIMENSION(6) :: pttab_child  
      INTEGER ,DIMENSION(6) :: petab_child      
      INTEGER ,DIMENSION(6) :: pttab_parent  
      REAL    ,DIMENSION(6) :: s_child,s_parent
      REAL    ,DIMENSION(6) :: ds_child,ds_parent
      INTEGER,DIMENSION(6)          :: loctab_Child ! Indicates if the child
                                        !    grid has a common border with 
                                        !    the root grid            
      TYPE(AGRIF_Variable), Pointer :: root               ! Variable on the root grid
      INTEGER,DIMENSION(6)          :: posvartab_Child    ! Position of the
                                        !    variable on the cell
      INTEGER,DIMENSION(6)          :: nbtab_Child        ! Number of the cells    
      INTEGER :: n              
      LOGICAL :: wholeupdate
C
C 

      loctab_child(:) = 0
C
      root => child % var % root_var 
      nbdim = root % nbdim
C
      do n = 1,nbdim
        posvartab_child(n) = root % posvar(n)
      enddo
C      
      
      Call PreProcessToInterpOrUpdate(parent,child,
     &             petab_Child(1:nbdim),
     &             pttab_Child(1:nbdim),pttab_Parent(1:nbdim),
     &             s_Child(1:nbdim),s_Parent(1:nbdim),
     &             ds_Child(1:nbdim),ds_Parent(1:nbdim),
     &             nbdim)
C
C
      do n = 1,nbdim
C
        Select case(root % interptab(n))
C
          case('x') ! x DIMENSION
C
            nbtab_Child(n) = Agrif_Curgrid % nb(1)
C
          case('y') ! y DIMENSION      
C
            nbtab_Child(n) = Agrif_Curgrid % nb(2)
C
          case('z') ! z DIMENSION
C
            nbtab_Child(n) = Agrif_Curgrid % nb(3)
C
          case('N') ! No space DIMENSION      
C
            select case (nbdim) 
C      
              case(1)
                nbtab_Child(n) = SIZE(child % var % array1,n) - 1
              case(2)
                nbtab_Child(n) = SIZE(child % var % array2,n) - 1
              case(3)
                nbtab_Child(n) = SIZE(child % var % array3,n) - 1
              case(4)
                nbtab_Child(n) = SIZE(child % var % array4,n) - 1
              case(5)
                nbtab_Child(n) = SIZE(child % var % array5,n) - 1  
              case(6)
                nbtab_Child(n) = SIZE(child % var % array6,n) - 1 
C
            end select
C
C           No interpolation but only a copy of the values of the grid variable      
C      
            posvartab_child(n) = 1
            
            loctab_child(n) = -3
C
        End select
C
      enddo
      
C     Call to a procedure of update according to the number of dimensions of
C     the grid variable

      wholeupdate = .FALSE.

      IF ((deb == -99) .AND. (deb == fin)) THEN
       wholeupdate = .TRUE.
      ENDIF

      IF ((deb > fin)) THEN
       wholeupdate = .TRUE.
      ENDIF
     
       IF (present(procname)) THEN

          IF (wholeupdate) THEN

          Call AGRIF_UpdateWhole
     &         (TypeUpdate,parent,child,deb,fin,
     &          pttab_Child(1:nbdim),pttab_Parent(1:nbdim),
     &          nbtab_Child(1:nbdim),posvartab_Child(1:nbdim),
     &          loctab_Child(1:nbdim),
     &          s_Child(1:nbdim),s_Parent(1:nbdim),
     &          ds_Child(1:nbdim),ds_Parent(1:nbdim),nbdim,procname)  
         ELSE
          Call AGRIF_UpdateBcnD
     &         (TypeUpdate,parent,child,deb,fin,
     &          pttab_Child(1:nbdim),pttab_Parent(1:nbdim),
     &          nbtab_Child(1:nbdim),posvartab_Child(1:nbdim),
     &          loctab_Child(1:nbdim),
     &          s_Child(1:nbdim),s_Parent(1:nbdim),
     &          ds_Child(1:nbdim),ds_Parent(1:nbdim),nbdim,procname)  
         ENDIF
       ELSE
         IF (wholeupdate) THEN
          Call AGRIF_UpdateWhole
     &         (TypeUpdate,parent,child,deb,fin,
     &          pttab_Child(1:nbdim),pttab_Parent(1:nbdim),
     &          nbtab_Child(1:nbdim),posvartab_Child(1:nbdim),
     &          loctab_Child(1:nbdim),
     &          s_Child(1:nbdim),s_Parent(1:nbdim),
     &          ds_Child(1:nbdim),ds_Parent(1:nbdim),nbdim)
         ELSE
          Call AGRIF_UpdateBcnD
     &         (TypeUpdate,parent,child,deb,fin,
     &          pttab_Child(1:nbdim),pttab_Parent(1:nbdim),
     &          nbtab_Child(1:nbdim),posvartab_Child(1:nbdim),
     &          loctab_Child(1:nbdim),
     &          s_Child(1:nbdim),s_Parent(1:nbdim),
     &          ds_Child(1:nbdim),ds_Parent(1:nbdim),nbdim)
         ENDIF
       ENDIF
C
      Return
C
C
      End subroutine Agrif_UpdateVariable
C
C     **************************************************************************
CCC   Subroutine Agrif_UpdateWhole
C     **************************************************************************               
C
      Subroutine AGRIF_UpdateWhole(TypeUpdate,parent,child,deb,fin,
     &                           pttab_child,pttab_Parent,
     &                           nbtab_Child,posvartab_Child,
     &                           loctab_Child,
     &                           s_Child,s_Parent,
     &                           ds_Child,ds_Parent,nbdim,procname)
C
CCC   Description:
CCC   Subroutine to calculate the boundary conditions for a nD grid variable on 
CCC   a fine grid by using a space and time interpolations; it is called by the 
CCC   Agrif_CorrectVariable procedure.
C
C
C     Declarations:
C
      
C





C
C     Arguments
      INTEGER, DIMENSION(6) :: TypeUpdate            ! TYPE of update (copy or
                                                     !    average) 
      TYPE(AGRIF_PVariable)    :: parent             ! Variable on the parent
                                                     !    grid
      TYPE(AGRIF_PVariable)    :: child              ! Variable on the child
                                                     !    grid 
      INTEGER :: deb, fin
      INTEGER                  :: nbdim              ! Number of dimensions of
                                                     !    the grid variable
      INTEGER,DIMENSION(nbdim) :: pttab_child        ! Index of the first point
                                                     !    inside the domain for
                                                     !    the parent grid
                                                     !    variable
      INTEGER,DIMENSION(nbdim) :: pttab_Parent       ! Index of the first point
                                                     !    inside the domain for
                                                     !    the child grid
                                                     !    variable
      INTEGER,DIMENSION(nbdim) :: nbtab_Child        ! Number of cells of the
                                                     !    child grid
      INTEGER,DIMENSION(nbdim) :: posvartab_Child    ! Position of the grid
                                                     !    variable (1 or 2)
      INTEGER,DIMENSION(nbdim) :: loctab_Child       ! Indicates if the child
                                                     !    grid has a common
                                                     !    border with the root
                                                     !    grid
      REAL   ,DIMENSION(nbdim) :: s_Child,s_Parent   ! Positions of the parent
                                                     !    and child grids 
      REAL   ,DIMENSION(nbdim) :: ds_Child,ds_Parent ! Space steps of the parent
                                                     !    and child grids
      External :: procname
      Optional ::  procname      
C
C     Local variables     
      INTEGER,DIMENSION(nbdim,2)   :: lubglob
      INTEGER                      :: i                 
      INTEGER,DIMENSION(nbdim,2,2) :: indtab         ! Arrays indicating the
                                                     !    limits of the child
      INTEGER,DIMENSION(nbdim,2,2) :: indtruetab     ! grid variable where
                                                     !   boundary conditions are
      integer :: coeffraf
      INTEGER :: debloc, finloc
C







C      
C
C indtab contains the limits for the fine grid points that will be used
C in the update scheme

      DO i = 1, nbdim
        coeffraf = nint(ds_Parent(i)/ds_Child(i))
        debloc = 0
        finloc = nbtab_Child(i)/coeffraf - 1

        IF (posvartab_child(i) == 1) THEN
           finloc = finloc - 1
        ENDIF

        IF (deb > fin) THEN
          debloc = deb
          finloc = finloc - deb
        ENDIF

        indtab(i,1,1) = pttab_child(i) + (debloc + 1) * coeffraf
        indtab(i,1,2) = pttab_child(i) + (finloc + 1) * coeffraf

        IF (posvartab_child(i) == 1) THEN
          IF (TypeUpdate(i) .NE. Agrif_Update_Copy) THEN
            indtab(i,1,1) = indtab(i,1,1) - coeffraf / 2
            indtab(i,1,2) = indtab(i,1,2) + coeffraf / 2
          ENDIF
        ELSE
          indtab(i,1,1) = indtab(i,1,1) - coeffraf
          indtab(i,1,2) = indtab(i,1,2) - 1
        ENDIF
        IF (loctab_child(i) == -3) THEN
           indtab(i,1,1) = pttab_child(i)
C
               if (posvartab_child(i) == 1) then
C
               indtab(i,1,2) = pttab_child(i) + nbtab_child(i) 
C
               else
C
               indtab(i,1,2) = pttab_child(i) + nbtab_child(i) - 1
               ENDIF
        ENDIF
      ENDDO

C lubglob contains the global lbound and ubound of the child array
C lubglob(:,1) : global lbound for each dimension
C lubglob(:,2) : global lbound for each dimension


        Call Agrif_nbdim_Get_bound_dimension(child % var,lubglob(:,1),
     &               lubglob(:,2),nbdim)
C
C

      indtruetab(1:nbdim,1,1) = max(indtab(1:nbdim,1,1),
     &     lubglob(1:nbdim,1))
      indtruetab(1:nbdim,1,2) = min(indtab(1:nbdim,1,2),
     &     lubglob(1:nbdim,2))

C
C

           IF (present(procname)) THEN
              Call Agrif_UpdatenD              
     &             (TypeUpdate,parent,child,
     &              indtruetab(1:nbdim,1,1),indtruetab(1:nbdim,1,2),
     &              pttab_child(1:nbdim),pttab_Parent(1:nbdim),
     &              s_Child(1:nbdim),s_Parent(1:nbdim),
     &              ds_Child(1:nbdim),ds_Parent(1:nbdim),
     &              posvartab_child,loctab_Child,
     &              nbdim,procname)
           ELSE
              Call Agrif_UpdatenD              
     &             (TypeUpdate,parent,child,
     &              indtruetab(1:nbdim,1,1),indtruetab(1:nbdim,1,2),
     &              pttab_child(1:nbdim),pttab_Parent(1:nbdim),
     &              s_Child(1:nbdim),s_Parent(1:nbdim),
     &              ds_Child(1:nbdim),ds_Parent(1:nbdim),
     &              posvartab_child,loctab_Child,
     &              nbdim)           
           ENDIF
C
C      
C  
      End Subroutine Agrif_UpdateWhole
C
C     **************************************************************************
CCC   Subroutine Agrif_UpdateBcnd
C     **************************************************************************               
C
      Subroutine AGRIF_UpdateBcnd(TypeUpdate,parent,child,deb,fin,
     &                           pttab_child,pttab_Parent,
     &                           nbtab_Child,posvartab_Child,
     &                           loctab_Child,
     &                           s_Child,s_Parent,
     &                           ds_Child,ds_Parent,nbdim,procname)
C
CCC   Description:
CCC   Subroutine to calculate the boundary conditions for a nD grid variable on
CCC   a fine grid by using a space and time interpolations; it is called by the 
CCC   Agrif_CorrectVariable procedure.
C
C
C     Declarations:
C
      
C
C
C     Arguments
      INTEGER, DIMENSION(6) :: TypeUpdate            ! TYPE of update 
                                                     !   (copy or average) 
      TYPE(AGRIF_PVariable)    :: parent             ! Variable on the parent
                                                     !   grid
      TYPE(AGRIF_PVariable)    :: child              ! Variable on the child
                                                     !   grid 
      INTEGER                  :: deb,fin            ! Positions where
                                                     !   interpolations are done
      INTEGER                  :: nbdim              ! Number of dimensions of
                                                     !   the grid variable
      INTEGER,DIMENSION(nbdim) :: pttab_child        ! Index of the first point
                                                     !   inside the domain for
                                                     !   the parent grid
                                                     !   variable
      INTEGER,DIMENSION(nbdim) :: pttab_Parent       ! Index of the first point
                                                     !   inside the domain for
                                                     !   the child grid variable
      INTEGER,DIMENSION(nbdim) :: nbtab_Child        ! Number of cells of the
                                                     !   child grid
      INTEGER,DIMENSION(nbdim) :: posvartab_Child    ! Position of the grid
                                                     !   variable (1 or 2)
      INTEGER,DIMENSION(nbdim) :: loctab_Child       ! Indicates if the child
                                                     !   grid has a common
                                                     !   border with the root
                                                     !   grid
      REAL   ,DIMENSION(nbdim) :: s_Child,s_Parent   ! Positions of the parent
                                                     !   and child grids 
      REAL   ,DIMENSION(nbdim) :: ds_Child,ds_Parent ! Space steps of the parent
                                                     !   and child grids
      External :: procname
      Optional ::  procname      
C
C     Local variables
      INTEGER,DIMENSION(nbdim,2)   :: lubglob
      INTEGER                      :: i                 
      INTEGER,DIMENSION(nbdim,2,2) :: indtab         ! Arrays indicating the
                                                     !   limits of the child
      INTEGER,DIMENSION(nbdim,2,2) :: indtruetab     ! grid variable where
                                                     !  boundary conditions are 
      INTEGER,DIMENSION(nbdim,2,2,nbdim)   :: ptres      ! calculated
      INTEGER                      :: nb,ndir,n
      integer :: coeffraf
C
C      
C

      DO i = 1, nbdim
        coeffraf = nint(ds_Parent(i)/ds_Child(i))
        indtab(i,1,1) = pttab_child(i) + (deb + 1) * coeffraf
        indtab(i,1,2) = pttab_child(i) + (fin + 1) * coeffraf

        indtab(i,2,1) = pttab_child(i) + nbtab_child(i)
     &    - (fin + 1) *  coeffraf
        indtab(i,2,2) = pttab_child(i) + nbtab_child(i)
     &    - (deb + 1) *  coeffraf

        IF (posvartab_child(i) == 1) THEN
          IF (TypeUpdate(i) .NE. Agrif_Update_Copy) THEN
            indtab(i,:,1) = indtab(i,:,1) - coeffraf / 2
            indtab(i,:,2) = indtab(i,:,2) + coeffraf / 2
          ENDIF
        ELSE
          indtab(i,1,1) = indtab(i,1,1) - coeffraf
          indtab(i,1,2) = indtab(i,1,2) - 1
          indtab(i,2,2) = indtab(i,2,2) + coeffraf - 1
        ENDIF
      ENDDO

        Call Agrif_nbdim_Get_bound_dimension(child % var,lubglob(:,1),
     &               lubglob(:,2),nbdim)

C
C     
      indtruetab(1:nbdim,1,1) = max(indtab(1:nbdim,1,1),
     &     lubglob(1:nbdim,1))
      indtruetab(1:nbdim,1,2) = max(indtab(1:nbdim,1,2),
     &     lubglob(1:nbdim,1))
      indtruetab(1:nbdim,2,1) = min(indtab(1:nbdim,2,1),
     &     lubglob(1:nbdim,2))
      indtruetab(1:nbdim,2,2) = min(indtab(1:nbdim,2,2),
     &     lubglob(1:nbdim,2))
                        
C 
C
      do nb = 1,nbdim
C
        do ndir = 1,2
C
          if (loctab_child(nb) /= -3) then
C           
              do n = 1,2
C
                ptres(nb,n,ndir,nb) = indtruetab(nb,ndir,n)
C
              enddo              
C
              do i = 1,nbdim
C     
                if (i .NE. nb) then      
C
                    if (loctab_child(i) == -3) then
C
                        ptres(i,1,ndir,nb) = pttab_child(i)
C
                      else
C
                        ptres(i,1,ndir,nb) = indtruetab(i,1,1)
C
                    endif
C
                    if (loctab_child(i) == -3) then
C
                        if (posvartab_child(i) == 1) then
C
                            ptres(i,2,ndir,nb) = pttab_child(i) 
     &                                + nbtab_child(i)
C
                          else
C
                            ptres(i,2,ndir,nb) = pttab_child(i) 
     &                             + nbtab_child(i) - 1
C
                        endif                             
C
                      else
C
                        ptres(i,2,ndir,nb) = indtruetab(i,2,2)
C
                    endif                        
C      
                endif
C      
              enddo
      
C
            
        endif
      
        enddo
       enddo
C

C

      do nb = 1,nbdim
C
        do ndir = 1,2                
C
          if (loctab_child(nb) /= -3) then
C
           IF (present(procname)) THEN
              Call Agrif_UpdatenD              
     &             (TypeUpdate,parent,child,
     &              ptres(1:nbdim,1,ndir,nb),ptres(1:nbdim,2,ndir,nb),
     &              pttab_child(1:nbdim),pttab_Parent(1:nbdim),
     &              s_Child(1:nbdim),s_Parent(1:nbdim),
     &              ds_Child(1:nbdim),ds_Parent(1:nbdim),
     &              posvartab_Child,loctab_Child,
     &              nbdim,procname)
           ELSE
              Call Agrif_UpdatenD              
     &             (TypeUpdate,parent,child,
     &              ptres(1:nbdim,1,ndir,nb),ptres(1:nbdim,2,ndir,nb),
     &              pttab_child(1:nbdim),pttab_Parent(1:nbdim),
     &              s_Child(1:nbdim),s_Parent(1:nbdim),
     &              ds_Child(1:nbdim),ds_Parent(1:nbdim),
     &              posvartab_Child,loctab_Child,
     &              nbdim)           
           ENDIF
C
          endif
          
C
        enddo       
C
      enddo
C
C      
C  
      End Subroutine Agrif_UpdateBcnd
C
C     **************************************************************************  
CCC   Subroutine Agrif_UpdatenD  
C     **************************************************************************  
C  
      Subroutine Agrif_UpdatenD(TypeUpdate,parent,child,
     &                          pttab,petab,
     &                          pttab_Child,pttab_Parent,
     &                          s_Child,s_Parent,
     &                          ds_Child,ds_Parent,
     &                          posvartab_Child,loctab_Child,
     &                          nbdim,procname)  
C
C     Description:
C     Subroutine to update a 2D grid variable on the parent grid of 
C        the current grid. 
C
C     Declarations:
C
      
C
C
C     Arguments
      INTEGER                    :: nbdim
      INTEGER, DIMENSION(6) :: TypeUpdate              ! TYPE of update 
                                                       !  (copy or average)
      TYPE(AGRIF_PVARIABLE)      :: parent             ! Variable of the parent
                                                       !   grid   
      TYPE(AGRIF_PVARIABLE)      :: child              ! Variable of the child
                                                       !   grid
      INTEGER,DIMENSION(nbdim)   :: pttab              ! Index of the first
                                                       !   point inside the
                                                       !   domain
      INTEGER,DIMENSION(nbdim)   :: petab              ! Index of the first
                                                       !   point inside the
                                                       !   domain
      INTEGER,DIMENSION(nbdim)   :: pttab_Child        ! Index of the first
                                                       !   point inside the
                                                       !   domain for the child
                                                       !   grid variable
      INTEGER,DIMENSION(nbdim)   :: pttab_Parent       ! Index of the first
                                                       !   point inside the
                                                       !   domain for the parent
                                                       !   grid variable
      REAL,DIMENSION(nbdim)      :: s_Child,s_Parent   ! Positions of the parent
                                                       !   and child grids
      REAL,DIMENSION(nbdim)      :: ds_Child,ds_Parent ! Space steps of the
                                                       !   parent and child
                                                       !   grids 
      External :: procname
      Optional ::  procname
C
C     Local pointers
      TYPE(AGRIF_PVARIABLE)      :: tempP      ! Temporary parent grid variable
      TYPE(AGRIF_PVARIABLE)      :: tempC      ! Temporary child grid variable
C
C     Local scalars
      INTEGER,DIMENSION(nbdim)    :: pttruetab,cetruetab
      INTEGER,DIMENSION(nbdim)    :: posvartab_Child,loctab_Child
      INTEGER,DIMENSION(nbdim)    :: indmin,indmax
      INTEGER,DIMENSION(nbdim)    :: indminglob,indmaxglob
      REAL   ,DIMENSION(nbdim)    :: s_Child_temp,s_Parent_temp
cccccccc      LOGICAL,DIMENSION(nbdim)    :: noraftab
      INTEGER,DIMENSION(nbdim)    :: lowerbound,upperbound
      LOGICAL :: memberin, member
      INTEGER,DIMENSION(nbdim)    :: pttruetabwhole,cetruetabwhole
      INTEGER,DIMENSION(nbdim,2,2) :: childarray
      INTEGER,DIMENSION(nbdim,2,2) :: parentarray
      TYPE(AGRIF_PVARIABLE)      :: tempCextend,tempPextend ! Temporary child
                                                            !    grid
C
C
  
C
C     local lbound and ubound of the child array

      Call Agrif_nbdim_Get_bound_dimension(child%var,
     &                              lowerbound,upperbound,nbdim)

C here pttab and petab corresponds to the (global) indices of the points needed
C in the update
C pttruetab and cetruetab contains only indices that are present
C on the local processor

      Call Agrif_Childbounds(nbdim,
     &                       lowerbound,upperbound,
     &                       pttab,petab,
     &                       pttruetab,cetruetab,memberin)

       Call Agrif_Prtbounds(nbdim,indminglob,indmaxglob,s_Parent_temp,
     &                     s_Child_temp,s_Child,ds_Child,
     &                     s_Parent,ds_Parent,
     &                     pttab,petab,pttab_Child,
     &                     pttab_Parent,
     &                     posvartab_Child,TypeUpdate,loctab_Child
     &     )

       indmin = indminglob
       indmax = indmaxglob
       pttruetabwhole = pttruetab
       cetruetabwhole = cetruetab
       childarray(:,1,2) = pttruetab
       childarray(:,2,2) = cetruetab


      IF (memberin) THEN
      allocate(tempC%var)

C
      Call Agrif_nbdim_allocation(tempC%var,
     &                 pttruetab,cetruetab,nbdim)

      Call Agrif_nbdim_Full_VarEQreal(tempC%var,0.,nbdim)



      IF (present(procname)) THEN
        SELECT CASE (nbdim)
        CASE(1)
          CALL procname(tempC%var%array1,
     &                          childarray(1,1,2),childarray(1,2,2),
     &                                   .TRUE.)
        CASE(2)
          CALL procname(tempC%var%array2,
     &                          childarray(1,1,2),childarray(1,2,2),
     &                          childarray(2,1,2),childarray(2,2,2),
     &                                   .TRUE.)
        CASE(3)
          CALL procname(tempC%var%array3,
     &                          childarray(1,1,2),childarray(1,2,2),
     &                          childarray(2,1,2),childarray(2,2,2),
     &                          childarray(3,1,2),childarray(3,2,2),
     &                                   .TRUE.)
        CASE(4)
          CALL procname(tempC%var%array4,
     &                          childarray(1,1,2),childarray(1,2,2),
     &                          childarray(2,1,2),childarray(2,2,2),
     &                          childarray(3,1,2),childarray(3,2,2),
     &                          childarray(4,1,2),childarray(4,2,2),
     &                                   .TRUE.)
        CASE(5)
          CALL procname(tempC%var%array5,
     &                          childarray(1,1,2),childarray(1,2,2),
     &                          childarray(2,1,2),childarray(2,2,2),
     &                          childarray(3,1,2),childarray(3,2,2),
     &                          childarray(4,1,2),childarray(4,2,2),
     &                          childarray(5,1,2),childarray(5,2,2),
     &                                   .TRUE.)
        CASE(6)
          CALL procname(tempC%var%array6,
     &                          childarray(1,1,2),childarray(1,2,2),
     &                          childarray(2,1,2),childarray(2,2,2),
     &                          childarray(3,1,2),childarray(3,2,2),
     &                          childarray(4,1,2),childarray(4,2,2),
     &                          childarray(5,1,2),childarray(5,2,2),
     &                          childarray(6,1,2),childarray(6,2,2),
     &                                   .TRUE.)
        END SELECT
      ELSE
      Call Agrif_nbdim_VarEQvar(tempC%var,pttruetab,cetruetab,
     &          child%var,childarray(:,1,2),childarray(:,2,2),
     &                          nbdim)
      ENDIF

      ENDIF



C
C
      tempCextend%var => tempC%var

C
C
C     Update of the parent grid (tempP) from the child grid (tempC)


      IF (memberin) THEN

      allocate(tempP%var)
      Call Agrif_nbdim_allocation(tempP%var,
     &                 indmin,indmax,nbdim)

      if ( nbdim .EQ. 1 ) then
         Call Agrif_Update_1D_recursive(TypeUpdate,
     &           tempP%var%array1,tempCextend%var%array1,
     &           indmin,indmax,
     &           pttruetabwhole,cetruetabwhole,
     &           s_Child_temp,s_Parent_temp,
     &           ds_Child,ds_Parent,nbdim)
      endif
      if ( nbdim .EQ. 2 ) then
         Call Agrif_Update_2D_recursive(TypeUpdate,
     &           tempP%var%array2,tempCextend%var%array2,
     &           indmin,indmax,
     &           pttruetabwhole,cetruetabwhole,
     &           s_Child_temp,s_Parent_temp,
     &           ds_Child,ds_Parent,nbdim)
      endif

      if ( nbdim .EQ. 3 ) then
         Call Agrif_Update_3D_recursive(TypeUpdate,
     &           tempP%var%array3,tempCextend%var%array3,
     &           indmin,indmax,
     &           pttruetabwhole,cetruetabwhole,
     &           s_Child_temp,s_Parent_temp,
     &           ds_Child,ds_Parent,nbdim)
      endif
      if ( nbdim .EQ. 4 ) then
         Call Agrif_Update_4D_recursive(TypeUpdate,
     &           tempP%var%array4,tempCextend%var%array4,
     &           indmin,indmax,
     &           pttruetabwhole,cetruetabwhole,
     &           s_Child_temp,s_Parent_temp,
     &           ds_Child,ds_Parent,nbdim)
      endif
      if ( nbdim .EQ. 5 ) then
         Call Agrif_Update_5D_recursive(TypeUpdate,
     &           tempP%var%array5,tempCextend%var%array5,
     &           indmin,indmax,
     &           pttruetabwhole,cetruetabwhole,
     &           s_Child_temp,s_Parent_temp,
     &           ds_Child,ds_Parent,nbdim)
      endif
      if ( nbdim .EQ. 6 ) then
         Call Agrif_Update_6D_recursive(TypeUpdate,
     &           tempP%var%array6,tempCextend%var%array6,
     &           indmin,indmax,
     &           pttruetabwhole,cetruetabwhole,
     &           s_Child_temp,s_Parent_temp,
     &           ds_Child,ds_Parent,nbdim)
      endif

      Call Agrif_nbdim_deallocation(tempCextend%var,nbdim)
      Deallocate(tempCextend%var)

      ENDIF

      tempPextend%var => tempP%var
      parentarray(:,1,1) = indmin
      parentarray(:,2,1) = indmax
      parentarray(:,1,2) = indmin
      parentarray(:,2,2) = indmax
      member = .TRUE.

C
C
C
C     Special values on the child grid
      if (Agrif_UseSpecialValueFineGrid) then
C
ccc         noraftab(1:nbdim) =
ccc     &    child % var % root_var % interptab(1:nbdim) .EQ. 'N'
C
C
c          Call Agrif_nbdim_Get_bound_dimension(child%var,
c     &                              lowerbound,upperbound,nbdim)
c          Call Agrif_CheckMasknD(tempC,child,
c     &                           pttruetab(1:nbdim),cetruetab(1:nbdim),
c     &                           lowerbound,
c     &                           upperbound,
c     &                           noraftab(1:nbdim),nbdim)
C
C
      endif


C
C
C
C
C     Special values on the parent grid
      if (Agrif_UseSpecialValue) then
C
C
c          Call GiveAgrif_SpecialValueToTab(parent%var,tempP%var,
c     &                  indmin,indmax,
c     &                  Agrif_SpecialValue,nbdim)
C
C
C
      endif   
C
C
        IF (member) THEN

          IF (present(procname)) THEN
            CALL Agrif_ChildGrid_to_ParentGrid()
            SELECT CASE(nbdim)
            CASE(1)
            CALL procname(
     &      tempPextend%var%array1(
     &                      parentarray(1,1,1):parentarray(1,2,1)),
     &                      parentarray(1,1,2),parentarray(1,2,2),
     &                                   .FALSE.
     &                      )
            CASE(2)
            CALL procname(
     &      tempPextend%var%array2(
     &                      parentarray(1,1,1):parentarray(1,2,1),
     &                      parentarray(2,1,1):parentarray(2,2,1)),
     &                      parentarray(1,1,2),parentarray(1,2,2),
     &                      parentarray(2,1,2),parentarray(2,2,2),
     &                                   .FALSE.
     &                      )
            CASE(3)
            CALL procname(
     &      tempPextend%var%array3(
     &                      parentarray(1,1,1):parentarray(1,2,1),
     &                      parentarray(2,1,1):parentarray(2,2,1),
     &                      parentarray(3,1,1):parentarray(3,2,1)),
     &                      parentarray(1,1,2),parentarray(1,2,2),
     &                      parentarray(2,1,2),parentarray(2,2,2),
     &                      parentarray(3,1,2),parentarray(3,2,2),
     &                                   .FALSE.
     &                      )
            CASE(4)
            CALL procname(
     &      tempPextend%var%array4(
     &                      parentarray(1,1,1):parentarray(1,2,1),
     &                      parentarray(2,1,1):parentarray(2,2,1),
     &                      parentarray(3,1,1):parentarray(3,2,1),
     &                      parentarray(4,1,1):parentarray(4,2,1)),
     &                      parentarray(1,1,2),parentarray(1,2,2),
     &                      parentarray(2,1,2),parentarray(2,2,2),
     &                      parentarray(3,1,2),parentarray(3,2,2),
     &                      parentarray(4,1,2),parentarray(4,2,2),
     &                                   .FALSE.
     &                      )
            CASE(5)
            CALL procname(
     &      tempPextend%var%array5(
     &                      parentarray(1,1,1):parentarray(1,2,1),
     &                      parentarray(2,1,1):parentarray(2,2,1),
     &                      parentarray(3,1,1):parentarray(3,2,1),
     &                      parentarray(4,1,1):parentarray(4,2,1),
     &                      parentarray(5,1,1):parentarray(5,2,1)),
     &                      parentarray(1,1,2),parentarray(1,2,2),
     &                      parentarray(2,1,2),parentarray(2,2,2),
     &                      parentarray(3,1,2),parentarray(3,2,2),
     &                      parentarray(4,1,2),parentarray(4,2,2),
     &                      parentarray(5,1,2),parentarray(5,2,2),
     &                                   .FALSE.
     &                      )
            CASE(6)
            CALL procname(
     &      tempPextend%var%array6(
     &                      parentarray(1,1,1):parentarray(1,2,1),
     &                      parentarray(2,1,1):parentarray(2,2,1),
     &                      parentarray(3,1,1):parentarray(3,2,1),
     &                      parentarray(4,1,1):parentarray(4,2,1),
     &                      parentarray(5,1,1):parentarray(5,2,1),
     &                      parentarray(6,1,1):parentarray(6,2,1)),
     &                      parentarray(1,1,2),parentarray(1,2,2),
     &                      parentarray(2,1,2),parentarray(2,2,2),
     &                      parentarray(3,1,2),parentarray(3,2,2),
     &                      parentarray(4,1,2),parentarray(4,2,2),
     &                      parentarray(5,1,2),parentarray(5,2,2),
     &                      parentarray(6,1,2),parentarray(6,2,2),
     &                                   .FALSE.
     &                      )
            END SELECT
            CALL Agrif_ParentGrid_to_ChildGrid()
          ELSE
            SELECT CASE(nbdim)
            CASE(1)
            parent%var%array1(parentarray(1,1,2):parentarray(1,2,2)) =
     &      tempPextend%var%array1(
     &                      parentarray(1,1,1):parentarray(1,2,1))
            CASE(2)
            parent%var%array2(parentarray(1,1,2):parentarray(1,2,2),
     &                      parentarray(2,1,2):parentarray(2,2,2)) =
     &      tempPextend%var%array2(
     &                      parentarray(1,1,1):parentarray(1,2,1),
     &                      parentarray(2,1,1):parentarray(2,2,1))
            CASE(3)
            parent%var%array3(parentarray(1,1,2):parentarray(1,2,2),
     &                      parentarray(2,1,2):parentarray(2,2,2),
     &                      parentarray(3,1,2):parentarray(3,2,2)) =
     &      tempPextend%var%array3(
     &                      parentarray(1,1,1):parentarray(1,2,1),
     &                      parentarray(2,1,1):parentarray(2,2,1),
     &                      parentarray(3,1,1):parentarray(3,2,1))
            CASE(4)
            parent%var%array4(parentarray(1,1,2):parentarray(1,2,2),
     &                      parentarray(2,1,2):parentarray(2,2,2),
     &                      parentarray(3,1,2):parentarray(3,2,2),
     &                      parentarray(4,1,2):parentarray(4,2,2)) =
     &      tempPextend%var%array4(
     &                      parentarray(1,1,1):parentarray(1,2,1),
     &                      parentarray(2,1,1):parentarray(2,2,1),
     &                      parentarray(3,1,1):parentarray(3,2,1),
     &                      parentarray(4,1,1):parentarray(4,2,1))
            CASE(5)
            parent%var%array5(parentarray(1,1,2):parentarray(1,2,2),
     &                      parentarray(2,1,2):parentarray(2,2,2),
     &                      parentarray(3,1,2):parentarray(3,2,2),
     &                      parentarray(4,1,2):parentarray(4,2,2),
     &                      parentarray(5,1,2):parentarray(5,2,2)) =
     &      tempPextend%var%array5(
     &                      parentarray(1,1,1):parentarray(1,2,1),
     &                      parentarray(2,1,1):parentarray(2,2,1),
     &                      parentarray(3,1,1):parentarray(3,2,1),
     &                      parentarray(4,1,1):parentarray(4,2,1),
     &                      parentarray(5,1,1):parentarray(5,2,1))
            CASE(6)
            parent%var%array6(parentarray(1,1,2):parentarray(1,2,2),
     &                      parentarray(2,1,2):parentarray(2,2,2),
     &                      parentarray(3,1,2):parentarray(3,2,2),
     &                      parentarray(4,1,2):parentarray(4,2,2),
     &                      parentarray(5,1,2):parentarray(5,2,2),
     &                      parentarray(6,1,2):parentarray(6,2,2)) =
     &      tempPextend%var%array6(
     &                      parentarray(1,1,1):parentarray(1,2,1),
     &                      parentarray(2,1,1):parentarray(2,2,1),
     &                      parentarray(3,1,1):parentarray(3,2,1),
     &                      parentarray(4,1,1):parentarray(4,2,1),
     &                      parentarray(5,1,1):parentarray(5,2,1),
     &                      parentarray(6,1,1):parentarray(6,2,1))
            END SELECT
          ENDIF

        Call Agrif_nbdim_deallocation(tempPextend%var,nbdim)
       ENDIF
C
C
C     Deallocations

      IF (memberin) THEN
      Deallocate(tempP % var)
      ENDIF

C
C
      End Subroutine Agrif_UpdatenD
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Prtbounds
C     **************************************************************************
C
      Subroutine Agrif_Prtbounds(nbdim,indmin,indmax,s_Parent_temp,
     &                              s_Child_temp,s_Child,ds_Child,
     &                              s_Parent,ds_Parent,
     &                              pttruetab,cetruetab,pttab_Child,
     &                              pttab_Parent,
     &                              posvartab_child,TypeUpdate,
     &                              loctab_Child
     &                 )
C
CCC   Description:
CCC   Subroutine calculating the bounds of the parent grid to be updated
CCC   by the child grid     
C
C
C     Declarations:
C
      
C
C
C     Arguments
      INTEGER :: nbdim
      INTEGER,DIMENSION(nbdim) :: indmin,indmax
      REAL,DIMENSION(nbdim) :: s_Parent_temp,s_child_temp
      REAL,DIMENSION(nbdim) :: s_Child,ds_child
      REAL,DIMENSION(nbdim) :: s_Parent,ds_Parent
      INTEGER,DIMENSION(nbdim) :: pttruetab,cetruetab
      INTEGER,DIMENSION(nbdim) :: posvartab_child,TypeUpdate
      INTEGER,DIMENSION(nbdim) :: loctab_Child
      INTEGER,DIMENSION(nbdim) :: pttab_Child,pttab_Parent
C
C     Local variables
      INTEGER :: i
      REAL,DIMENSION(nbdim) :: dim_newmin,dim_newmax      
C
C
      do i = 1,nbdim
C
        dim_newmin(i) = s_Child(i) + (pttruetab(i) -
     &                                pttab_Child(i)) * ds_Child(i)
C
        dim_newmax(i) = s_Child(i) + (cetruetab(i) - 
     &                                pttab_Child(i)) * ds_Child(i)
C     
        indmin(i) = pttab_Parent(i) +
     &        agrif_ceiling((dim_newmin(i)-s_Parent(i))/ds_Parent(i))
C
        indmax(i) = pttab_Parent(i) +
     &        agrif_int((dim_newmax(i)-s_Parent(i))/ds_Parent(i))
C
C
        s_Parent_temp(i) = s_Parent(i) + 
     &                     (indmin(i) - pttab_Parent(i)) * 
     &                      ds_Parent(i) 
C     
        s_Child_temp(i) = dim_newmin(i)

C
      enddo
C
      Return
C
C
      End Subroutine Agrif_Prtbounds
C
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Update_1D_Recursive
C     **************************************************************************
C
      Subroutine Agrif_Update_1D_recursive(TypeUpdate,tempP,tempC,
     &                                     indmin,indmax,
     &                                     pttab_child,petab_child,
     &                                     s_child,s_parent,
     &                                     ds_child,ds_parent,nbdim)
C
CCC   Description:
CCC   Subroutine to update a 1D grid variable on the parent grid.
C
CC    Method:
C
C     Declarations:
C
      
C
C     Arguments
      INTEGER                   :: nbdim
      INTEGER, DIMENSION(nbdim) :: TypeUpdate            ! TYPE of update (copy or average)
      INTEGER, DIMENSION(nbdim) :: indmin,indmax
      INTEGER, DIMENSION(nbdim) :: pttab_child,petab_child
      REAL, DIMENSION(nbdim)    :: s_child,s_parent
      REAL, DIMENSION(nbdim)    :: ds_child,ds_parent
      REAL, DIMENSION(indmin(nbdim):indmax(nbdim))           :: tempP
      REAL, DIMENSION(pttab_child(nbdim):petab_child(nbdim)) :: tempC
C
C
      Call Agrif_UpdateBase(TypeUpdate(1),
     &                  tempP(indmin(nbdim):indmax(nbdim)),
     &                  tempC(pttab_child(nbdim):petab_child(nbdim)),
     &                  indmin(nbdim),indmax(nbdim),           
     &                  pttab_child(nbdim),petab_child(nbdim),
     &                  s_parent(nbdim),s_child(nbdim),
     &                  ds_parent(nbdim),ds_child(nbdim))
C
      Return
C
C
      End Subroutine Agrif_Update_1D_recursive
C
C
C
C     **************************************************************************  
CCC   Subroutine Agrif_Update_2D_Recursive 
C     **************************************************************************
C
      Subroutine Agrif_Update_2D_recursive(TypeUpdate,tempP,tempC,
     &                                     indmin,indmax,   
     &                                     pttab_child,petab_child,
     &                                     s_child,s_parent,
     &                                     ds_child,ds_parent,nbdim)
C
CCC   Description:
CCC   Subroutine to update a 2D grid variable on the parent grid. 
CCC   It calls Agrif_Update_1D_Recursive and Agrif_UpdateBase.   
C
CC    Method:
C
C     Declarations:
C
      
C     
      INTEGER                   :: nbdim
      INTEGER, DIMENSION(nbdim) :: TypeUpdate            ! TYPE of update (copy or average)
      INTEGER, DIMENSION(nbdim) :: indmin,indmax
      INTEGER, DIMENSION(nbdim) :: pttab_child,petab_child
      REAL, DIMENSION(nbdim)    :: s_child,s_parent
      REAL, DIMENSION(nbdim)    :: ds_child,ds_parent
      REAL, DIMENSION(indmin(1):indmax(1),
     &                indmin(2):indmax(2))           :: tempP
      REAL, DIMENSION(pttab_child(1):petab_child(1),
     &                pttab_child(2):petab_child(2)) :: tempC
C
C     Local variables      
      REAL, DIMENSION(:,:), Allocatable :: tabtemp
      INTEGER :: i,j
C
C
      Allocate(tabtemp(indmin(1):indmax(1),
     &                 pttab_child(2):petab_child(2)))
C
      do j = pttab_child(nbdim),petab_child(nbdim)
C
        Call Agrif_Update_1D_recursive(TypeUpdate,    
     &         tabtemp(indmin(nbdim-1):indmax(nbdim-1),j),
     &         tempC(pttab_child(nbdim-1):petab_child(nbdim-1),j),
     &         indmin(1:nbdim-1),indmax(1:nbdim-1),
     &         pttab_child(1:nbdim-1),petab_child(1:nbdim-1),
     &         s_child(1:nbdim-1),s_parent(1:nbdim-1),
     &         ds_child(1:nbdim-1),ds_parent(1:nbdim-1),nbdim-1)
C
      enddo
C
      do i = indmin(1),indmax(1)
C
        Call Agrif_UpdateBase(TypeUpdate(2),
     &           tempP(i,indmin(nbdim):indmax(nbdim)),
     &          tabtemp(i,pttab_child(nbdim):petab_child(nbdim)),
     &           indmin(nbdim),indmax(nbdim),
     &           pttab_child(nbdim),petab_child(nbdim),
     &           s_parent(nbdim),s_child(nbdim),
     &           ds_parent(nbdim),ds_child(nbdim))
C        
      enddo
C
      Deallocate(tabtemp)
C
      Return
C
C
      End Subroutine Agrif_Update_2D_recursive
C
C
C
C     **************************************************************************  
CCC   Subroutine Agrif_Update_3D_Recursive 
C     **************************************************************************
C
      Subroutine Agrif_Update_3D_recursive(TypeUpdate,tempP,tempC,
     &                                     indmin,indmax,   
     &                                     pttab_child,petab_child,
     &                                     s_child,s_parent,
     &                                     ds_child,ds_parent,nbdim)
C
CCC   Description:
CCC   Subroutine to update a 3D grid variable on the parent grid. 
CCC   It calls Agrif_Update_2D_Recursive and Agrif_UpdateBase.   
C
CC    Method:
C
C     Declarations:
C
      
C     
      INTEGER                   :: nbdim
      INTEGER, DIMENSION(nbdim) :: TypeUpdate            ! TYPE of update (copy or average)
      INTEGER, DIMENSION(nbdim) :: indmin,indmax
      INTEGER, DIMENSION(nbdim) :: pttab_child,petab_child
      REAL, DIMENSION(nbdim)    :: s_child,s_parent
      REAL, DIMENSION(nbdim)    :: ds_child,ds_parent
      REAL, DIMENSION(indmin(1):indmax(1),
     &                indmin(2):indmax(2),
     &                indmin(3):indmax(3))           :: tempP
      REAL, DIMENSION(pttab_child(1):petab_child(1),
     &                pttab_child(2):petab_child(2),
     &                pttab_child(3):petab_child(3)) :: tempC
C
C     Local variables      
      REAL, DIMENSION(:,:,:), Allocatable :: tabtemp
      INTEGER :: i,j,k
C
C
      Allocate(tabtemp(indmin(1):indmax(1),
     &                 indmin(2):indmax(2), 
     &                 pttab_child(3):petab_child(3)))
C
      do k = pttab_child(nbdim),petab_child(nbdim)
C
        Call Agrif_Update_2D_recursive(TypeUpdate,    
     &         tabtemp(indmin(nbdim-2):indmax(nbdim-2),
     &                 indmin(nbdim-1):indmax(nbdim-1),k),
     &         tempC(pttab_child(nbdim-2):petab_child(nbdim-2),
     &               pttab_child(nbdim-1):petab_child(nbdim-1),k),
     &         indmin(1:nbdim-1),indmax(1:nbdim-1),
     &         pttab_child(1:nbdim-1),petab_child(1:nbdim-1),
     &         s_child(1:nbdim-1),s_parent(1:nbdim-1),
     &         ds_child(1:nbdim-1),ds_parent(1:nbdim-1),nbdim-1)
C
      enddo
C
C
      do j = indmin(2),indmax(2)
C
        do i = indmin(1),indmax(1)
C
          Call Agrif_UpdateBase(TypeUpdate(3),
     &           tempP(i,j,indmin(nbdim):indmax(nbdim)),
     &          tabtemp(i,j,pttab_child(nbdim):petab_child(nbdim)),
     &           indmin(nbdim),indmax(nbdim),
     &           pttab_child(nbdim),petab_child(nbdim),
     &           s_parent(nbdim),s_child(nbdim),
     &           ds_parent(nbdim),ds_child(nbdim))
C
        enddo 
C        
      enddo
C
      Deallocate(tabtemp)
C
      Return
C
C
      End Subroutine Agrif_Update_3D_recursive
C
C
C
C     **************************************************************************  
CCC   Subroutine Agrif_Update_4D_Recursive 
C     **************************************************************************
C
      Subroutine Agrif_Update_4D_recursive(TypeUpdate,tempP,tempC,
     &                                     indmin,indmax,   
     &                                     pttab_child,petab_child,
     &                                     s_child,s_parent,
     &                                     ds_child,ds_parent,nbdim)
C
CCC   Description:
CCC   Subroutine to update a 4D grid variable on the parent grid. 
CCC   It calls Agrif_Update_3D_Recursive and Agrif_UpdateBase.    
C
CC    Method:
C
C     Declarations:
C
      
C     
      INTEGER                   :: nbdim
      INTEGER, DIMENSION(nbdim) :: TypeUpdate            ! TYPE of update (copy or average)
      INTEGER, DIMENSION(nbdim) :: indmin,indmax
      INTEGER, DIMENSION(nbdim) :: pttab_child,petab_child
      REAL, DIMENSION(nbdim)    :: s_child,s_parent
      REAL, DIMENSION(nbdim)    :: ds_child,ds_parent
      REAL, DIMENSION(indmin(1):indmax(1),
     &                indmin(2):indmax(2),
     &                indmin(3):indmax(3),
     &                indmin(4):indmax(4))           :: tempP
      REAL, DIMENSION(pttab_child(1):petab_child(1),
     &                pttab_child(2):petab_child(2),
     &                pttab_child(3):petab_child(3),
     &                pttab_child(4):petab_child(4)) :: tempC
C
C     Local variables      
      REAL, DIMENSION(:,:,:,:), Allocatable :: tabtemp
      INTEGER :: i,j,k,l
C
C
      Allocate(tabtemp(indmin(1):indmax(1),
     &                 indmin(2):indmax(2),
     &                 indmin(3):indmax(3), 
     &                 pttab_child(4):petab_child(4)))
C
      do l = pttab_child(nbdim),petab_child(nbdim)
C
        Call Agrif_Update_3D_recursive(TypeUpdate,    
     &         tabtemp(indmin(nbdim-3):indmax(nbdim-3),
     &                 indmin(nbdim-2):indmax(nbdim-2),
     &                 indmin(nbdim-1):indmax(nbdim-1),l),
     &         tempC(pttab_child(nbdim-3):petab_child(nbdim-3),
     &               pttab_child(nbdim-2):petab_child(nbdim-2),
     &               pttab_child(nbdim-1):petab_child(nbdim-1),l),
     &         indmin(1:nbdim-1),indmax(1:nbdim-1),
     &         pttab_child(1:nbdim-1),petab_child(1:nbdim-1),
     &         s_child(1:nbdim-1),s_parent(1:nbdim-1),
     &         ds_child(1:nbdim-1),ds_parent(1:nbdim-1),nbdim-1)
C
      enddo
C
      do k = indmin(3),indmax(3)
C
        do j = indmin(2),indmax(2)
C
          do i = indmin(1),indmax(1)
C
            Call Agrif_UpdateBase(TypeUpdate(4),
     &           tempP(i,j,k,indmin(nbdim):indmax(nbdim)),
     &          tabtemp(i,j,k,pttab_child(nbdim):petab_child(nbdim)),
     &           indmin(nbdim),indmax(nbdim),
     &           pttab_child(nbdim),petab_child(nbdim),
     &           s_parent(nbdim),s_child(nbdim),
     &           ds_parent(nbdim),ds_child(nbdim))
C
          enddo 
C
        enddo 
C        
      enddo
C
      Deallocate(tabtemp)
C
      Return
C
C
      End Subroutine Agrif_Update_4D_recursive
C
C
C
C     **************************************************************************  
CCC   Subroutine Agrif_Update_5D_Recursive 
C     **************************************************************************
C
      Subroutine Agrif_Update_5D_recursive(TypeUpdate,tempP,tempC,
     &                                     indmin,indmax,   
     &                                     pttab_child,petab_child,
     &                                     s_child,s_parent,
     &                                     ds_child,ds_parent,nbdim)
C
CCC   Description:
CCC   Subroutine to update a 5D grid variable on the parent grid. 
CCC   It calls Agrif_Update_4D_Recursive and Agrif_UpdateBase.    
C
CC    Method:
C
C     Declarations:
C
      
C     
      INTEGER                   :: nbdim
      INTEGER, DIMENSION(nbdim) :: TypeUpdate            ! TYPE of update (copy or average)
      INTEGER, DIMENSION(nbdim) :: indmin,indmax
      INTEGER, DIMENSION(nbdim) :: pttab_child,petab_child
      REAL, DIMENSION(nbdim)    :: s_child,s_parent
      REAL, DIMENSION(nbdim)    :: ds_child,ds_parent
      REAL, DIMENSION(indmin(1):indmax(1),
     &                indmin(2):indmax(2),
     &                indmin(3):indmax(3),
     &                indmin(4):indmax(4),
     &                indmin(5):indmax(5))           :: tempP
      REAL, DIMENSION(pttab_child(1):petab_child(1),
     &                pttab_child(2):petab_child(2),
     &                pttab_child(3):petab_child(3),
     &                pttab_child(4):petab_child(4),
     &                pttab_child(5):petab_child(5)) :: tempC
C
C     Local variables      
      REAL, DIMENSION(:,:,:,:,:), Allocatable :: tabtemp
      INTEGER :: i,j,k,l,m
C
C
      Allocate(tabtemp(indmin(1):indmax(1),
     &                 indmin(2):indmax(2),
     &                 indmin(3):indmax(3),
     &                 indmin(4):indmax(4),   
     &                 pttab_child(5):petab_child(5)))
C
      do m = pttab_child(nbdim),petab_child(nbdim)
C
        Call Agrif_Update_4D_recursive(TypeUpdate,    
     &         tabtemp(indmin(nbdim-4):indmax(nbdim-4),
     &                 indmin(nbdim-3):indmax(nbdim-3),
     &                 indmin(nbdim-2):indmax(nbdim-2),
     &                 indmin(nbdim-1):indmax(nbdim-1),m),
     &         tempC(pttab_child(nbdim-4):petab_child(nbdim-4),
     &               pttab_child(nbdim-3):petab_child(nbdim-3),
     &               pttab_child(nbdim-2):petab_child(nbdim-2),
     &               pttab_child(nbdim-1):petab_child(nbdim-1),m),
     &         indmin(1:nbdim-1),indmax(1:nbdim-1),
     &         pttab_child(1:nbdim-1),petab_child(1:nbdim-1),
     &         s_child(1:nbdim-1),s_parent(1:nbdim-1),
     &         ds_child(1:nbdim-1),ds_parent(1:nbdim-1),nbdim-1)
C
      enddo
C
      do l = indmin(4),indmax(4)
C
        do k = indmin(3),indmax(3)
C
          do j = indmin(2),indmax(2)
C
            do i = indmin(1),indmax(1)
C
              Call Agrif_UpdateBase(TypeUpdate(5),
     &           tempP(i,j,k,l,indmin(nbdim):indmax(nbdim)),
     &          tabtemp(i,j,k,l,
     &                   pttab_child(nbdim):petab_child(nbdim)),
     &           indmin(nbdim),indmax(nbdim),
     &           pttab_child(nbdim),petab_child(nbdim),
     &           s_parent(nbdim),s_child(nbdim),
     &           ds_parent(nbdim),ds_child(nbdim))
C
            enddo
C
          enddo 
C
        enddo 
C        
      enddo
C
      Deallocate(tabtemp)
C
      Return
C
C
      End Subroutine Agrif_Update_5D_recursive
C
C
C
C
C     **************************************************************************  
CCC   Subroutine Agrif_Update_6D_Recursive 
C     **************************************************************************
C
      Subroutine Agrif_Update_6D_recursive(TypeUpdate,tempP,tempC,
     &                                     indmin,indmax,   
     &                                     pttab_child,petab_child,
     &                                     s_child,s_parent,
     &                                     ds_child,ds_parent,nbdim)
C
CCC   Description:
CCC   Subroutine to update a 6D grid variable on the parent grid. 
CCC   It calls Agrif_Update_5D_Recursive and Agrif_UpdateBase.    
C
CC    Method:
C
C     Declarations:
C
      
C     
      INTEGER                   :: nbdim
      INTEGER, DIMENSION(nbdim) :: TypeUpdate            ! TYPE of update (copy or average)
      INTEGER, DIMENSION(nbdim) :: indmin,indmax
      INTEGER, DIMENSION(nbdim) :: pttab_child,petab_child
      REAL, DIMENSION(nbdim)    :: s_child,s_parent
      REAL, DIMENSION(nbdim)    :: ds_child,ds_parent
      REAL, DIMENSION(indmin(1):indmax(1),
     &                indmin(2):indmax(2),
     &                indmin(3):indmax(3),
     &                indmin(4):indmax(4),
     &                indmin(5):indmax(5),
     &                indmin(6):indmax(6))           :: tempP
      REAL, DIMENSION(pttab_child(1):petab_child(1),
     &                pttab_child(2):petab_child(2),
     &                pttab_child(3):petab_child(3),
     &                pttab_child(4):petab_child(4),
     &                pttab_child(5):petab_child(5),
     &                pttab_child(6):petab_child(6)) :: tempC
C
C     Local variables      
      REAL, DIMENSION(:,:,:,:,:,:), Allocatable :: tabtemp
      INTEGER :: i,j,k,l,m,n
C
C
      Allocate(tabtemp(indmin(1):indmax(1),
     &                 indmin(2):indmax(2),
     &                 indmin(3):indmax(3),
     &                 indmin(4):indmax(4),   
     &                 indmin(5):indmax(5),   
     &                 pttab_child(6):petab_child(6)))
C
      do n = pttab_child(nbdim),petab_child(nbdim)
C
        Call Agrif_Update_5D_recursive(TypeUpdate,    
     &         tabtemp(indmin(nbdim-5):indmax(nbdim-5),
     &                 indmin(nbdim-4):indmax(nbdim-4),
     &                 indmin(nbdim-3):indmax(nbdim-3),
     &                 indmin(nbdim-2):indmax(nbdim-2),
     &                 indmin(nbdim-1):indmax(nbdim-1),n),
     &         tempC(pttab_child(nbdim-5):petab_child(nbdim-5),
     &               pttab_child(nbdim-4):petab_child(nbdim-4),
     &               pttab_child(nbdim-3):petab_child(nbdim-3),
     &               pttab_child(nbdim-2):petab_child(nbdim-2),
     &               pttab_child(nbdim-1):petab_child(nbdim-1),n),
     &         indmin(1:nbdim-1),indmax(1:nbdim-1),
     &         pttab_child(1:nbdim-1),petab_child(1:nbdim-1),
     &         s_child(1:nbdim-1),s_parent(1:nbdim-1),
     &         ds_child(1:nbdim-1),ds_parent(1:nbdim-1),nbdim-1)
C
      enddo
C
      do m = indmin(5),indmax(5)
      do l = indmin(4),indmax(4)
C
        do k = indmin(3),indmax(3)
C
          do j = indmin(2),indmax(2)
C
            do i = indmin(1),indmax(1)
C
              Call Agrif_UpdateBase(TypeUpdate(6),
     &           tempP(i,j,k,l,m,indmin(nbdim):indmax(nbdim)),
     &          tabtemp(i,j,k,l,m,
     &                   pttab_child(nbdim):petab_child(nbdim)),
     &           indmin(nbdim),indmax(nbdim),
     &           pttab_child(nbdim),petab_child(nbdim),
     &           s_parent(nbdim),s_child(nbdim),
     &           ds_parent(nbdim),ds_child(nbdim))
C
            enddo
C
          enddo 
C
        enddo 
C        
      enddo
      enddo
C
      Deallocate(tabtemp)
C
      Return
C
C
      End Subroutine Agrif_Update_6D_recursive
C
C
C
C     **************************************************************************  
CCC   Subroutine Agrif_UpdateBase  
C     **************************************************************************  
C  
      Subroutine Agrif_UpdateBase(TypeUpdate,
     &                            parenttab,childtab,
     &                            indmin,indmax,pttab_child,petab_child,
     &                            s_parent,s_child,ds_parent,ds_child)
C
CCC   Description:
CCC   Subroutine calling the updating method chosen by the user (copy, average
CCC   or full-weighting).    
C
CC    Method:
C
C     Declarations:
C
      
C
      INTEGER :: TypeUpdate
      INTEGER :: indmin,indmax
      INTEGER :: pttab_child,petab_child
      REAL,DIMENSION(indmin:indmax)           :: parenttab       
      REAL,DIMENSION(pttab_child:petab_child) :: childtab      
      REAL    :: s_parent,s_child
      REAL    :: ds_parent,ds_child       
C
C
      if (TypeUpdate == AGRIF_Update_copy) then
C             
          Call copy1D
     &       (parenttab,childtab,
     &          indmax-indmin+1,petab_child-pttab_child+1,
     &          s_parent,s_child,ds_parent,ds_child)     
C
        elseif (TypeUpdate == AGRIF_Update_average) then
C             
          Call average1D
     &       (parenttab,childtab,
     &          indmax-indmin+1,petab_child-pttab_child+1,
     &          s_parent,s_child,ds_parent,ds_child)    
C
        elseif (TypeUpdate == AGRIF_Update_full_weighting) then
C             
          Call full_weighting1D
     &       (parenttab,childtab,
     &          indmax-indmin+1,petab_child-pttab_child+1,
     &          s_parent,s_child,ds_parent,ds_child)
C
      endif 
C
      Return                
C
C      
      End Subroutine Agrif_UpdateBase
C
C
      End Module Agrif_Update



      
