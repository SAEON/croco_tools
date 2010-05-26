!
! $Id: modupdate.F 779 2007-12-22 17:04:17Z rblod $
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
#ifdef AGRIF_MPI
      Use Agrif_mpp
#endif
C
      IMPLICIT NONE
      logical, private :: precomputedone(7) = .FALSE.      
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
      INTEGER, DIMENSION(6) :: deb,fin       ! Positions where interpolations 
                                              ! are done on the fine grid       
      External :: procname
      Optional ::  procname      
      REAL, DIMENSION(
     &         child%var%lb(1):child%var%ub(1)
     &         ), Target :: tab    ! Result
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
      
      childtemp % var % lb = child % var % lb
      childtemp % var % ub = child % var % ub             
      
C      childtemp % var % list_update => child%var%list_update
             
C
     
      IF (present(procname)) THEN
      CALL Agrif_UpdateVariable
     &     (TypeUpdate,parent,child,deb,fin,procname)
      ELSE
      CALL Agrif_UpdateVariable
     &     (TypeUpdate,parent,child,deb,fin)
      ENDIF     
C      
C      child % var % list_update => childtemp%var%list_update
      
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
      INTEGER, DIMENSION(6) :: deb,fin       ! Positions where interpolations  
                                              ! are done on the fine grid 
                                              
      External :: procname
      Optional ::  procname
                                                          
      REAL, DIMENSION(
     &         child%var%lb(1):child%var%ub(1),
     &         child%var%lb(2):child%var%ub(2)
     &    ), Target :: tab    ! Result
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
      
      childtemp % var % lb = child % var % lb
      childtemp % var % ub = child % var % ub             
      
C      childtemp % var % list_update => child%var%list_update      
C
      IF (present(procname)) THEN
      CALL Agrif_UpdateVariable
     &     (TypeUpdate,parent,child,deb,fin,procname)
      ELSE
      CALL Agrif_UpdateVariable
     &     (TypeUpdate,parent,child,deb,fin)
      ENDIF
C      
C      child % var % list_update => childtemp%var%list_update
      
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
      INTEGER, DIMENSION(6) :: deb,fin       ! Positions where interpolations 
                                              ! are done on the fine grid    
      External :: procname
      Optional ::  procname
                                                       
      REAL, DIMENSION(
     &         child%var%lb(1):child%var%ub(1),
     &         child%var%lb(2):child%var%ub(2),
     &         child%var%lb(3):child%var%ub(3)
     &      ), Target :: tab  ! Results  
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
      
      childtemp % var % lb = child % var % lb
      childtemp % var % ub = child % var % ub  
            
      
C      childtemp % var % list_update => child%var%list_update      
C
      IF (present(procname)) THEN
      CALL Agrif_UpdateVariable
     &     (TypeUpdate,parent,child,deb,fin,procname)
      ELSE
      CALL Agrif_UpdateVariable
     &     (TypeUpdate,parent,child,deb,fin)
      ENDIF
C      
C      child % var % list_update => childtemp%var%list_update
      
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
      INTEGER, DIMENSION(6) :: deb,fin       ! Positions where interpolations 
                                              ! are done on the fine grid     
      External :: procname
      Optional ::  procname        
      REAL, DIMENSION(
     &         child%var%lb(1):child%var%ub(1),
     &         child%var%lb(2):child%var%ub(2),
     &         child%var%lb(3):child%var%ub(3),
     &         child%var%lb(4):child%var%ub(4)
     &      ), Target :: tab  ! Results
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
      
      childtemp % var % lb = child % var % lb
      childtemp % var % ub = child % var % ub  
            
      
C      childtemp % var % list_update => child%var%list_update
            
C
      IF (present(procname)) THEN
      CALL Agrif_UpdateVariable
     &     (TypeUpdate,parent,child,deb,fin,procname)
      ELSE
      CALL Agrif_UpdateVariable
     &     (TypeUpdate,parent,child,deb,fin)
      ENDIF

C      child % var % list_update => childtemp%var%list_update      
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
      INTEGER, DIMENSION(6) :: deb,fin       ! Positions where interpolations 
                                              ! are done on the fine grid     
      External :: procname
      Optional ::  procname
              
      REAL, DIMENSION(
     &         child%var%lb(1):child%var%ub(1),
     &         child%var%lb(2):child%var%ub(2),
     &         child%var%lb(3):child%var%ub(3),
     &         child%var%lb(4):child%var%ub(4),
     &         child%var%lb(5):child%var%ub(5)
     &      ),  Target :: tab  ! Results
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
      
      childtemp % var % lb = child % var % lb
      childtemp % var % ub = child % var % ub        
      
C      childtemp % var % list_update => child%var%list_update      
C
      IF (present(procname)) THEN
      CALL Agrif_UpdateVariable
     &     (TypeUpdate,parent,child,deb,fin,procname)
      ELSE
      CALL Agrif_UpdateVariable
     &     (TypeUpdate,parent,child,deb,fin)
      ENDIF
      
C      child % var % list_update => childtemp%var%list_update
            
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
      INTEGER, DIMENSION(6) :: deb,fin       ! Positions where interpolations 
                                              ! are done on the fine grid       
      REAL, DIMENSION(
     &         child%var%lb(1):child%var%ub(1),
     &         child%var%lb(2):child%var%ub(2),
     &         child%var%lb(3):child%var%ub(3),
     &         child%var%lb(4):child%var%ub(4),
     &         child%var%lb(5):child%var%ub(5),
     &         child%var%lb(6):child%var%ub(6)
     &      ),  Target :: tab  ! Results
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
      
      childtemp % var % lb = child % var % lb
      childtemp % var % ub = child % var % ub  
            
C      childtemp % var % list_update => child%var%list_update
C
      Call Agrif_UpdateVariable
     &     (TypeUpdate,parent,child,deb,fin)
     
C      child % var % list_update => childtemp%var%list_update
           
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
      INTEGER, DIMENSION(6) :: deb,fin       ! Positions where interpolations 
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
            
            nbtab_Child(n) = child % var % ub(n) - child % var % lb(n)
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
      
      do n=1,nbdim
      if (loctab_child(n) /= -3) then
        if (deb(n)>fin(n)) wholeupdate = .TRUE.
        if ((deb(n) == -99).AND.(deb(n)==fin(n))) wholeupdate=.TRUE.
      endif
      enddo

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
#ifdef AGRIF_MPI
C
#include "mpif.h"
C
#endif
C
C     Arguments
      INTEGER, DIMENSION(6) :: TypeUpdate            ! TYPE of update (copy or
                                                     !    average) 
      TYPE(AGRIF_PVariable)    :: parent             ! Variable on the parent
                                                     !    grid
      TYPE(AGRIF_PVariable)    :: child              ! Variable on the child
                                                     !    grid 
      INTEGER, DIMENSION(6) :: deb, fin
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
#ifdef AGRIF_MPI
C
      INTEGER,DIMENSION(nbdim)   :: lb,ub
      INTEGER,DIMENSION(nbdim,2) :: iminmaxg
      INTEGER                    :: code
C
#endif
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

        IF (deb(i) > fin(i)) THEN
          debloc = deb(i)
          finloc = finloc - deb(i)
        ENDIF

        indtab(i,1,1) = pttab_child(i) + (debloc + 1) * coeffraf
        indtab(i,1,2) = pttab_child(i) + (finloc + 1) * coeffraf

        IF (posvartab_child(i) == 1) THEN
          IF (TypeUpdate(i) .EQ. Agrif_Update_Full_Weighting) THEN
            indtab(i,1,1) = indtab(i,1,1) - (coeffraf - 1)
            indtab(i,1,2) = indtab(i,1,2) + (coeffraf - 1)        
          ELSE IF (TypeUpdate(i) .NE. Agrif_Update_Copy) THEN
            indtab(i,1,1) = indtab(i,1,1) - coeffraf / 2
            indtab(i,1,2) = indtab(i,1,2) + coeffraf / 2
          ENDIF
        ELSE
          indtab(i,1,1) = indtab(i,1,1) - coeffraf
          indtab(i,1,2) = indtab(i,1,2) - 1
C at this point, indices are OK for an average          
          IF ((TypeUpdate(i) .EQ. Agrif_Update_Full_Weighting)) THEN
            indtab(i,1,1) = indtab(i,1,1) - coeffraf/2
            indtab(i,1,2) = indtab(i,1,2) + coeffraf/2
          ENDIF
          IF ((TypeUpdate(i) .EQ. Agrif_Update_4rdorder)
     &                            .AND.(mod(coeffraf,2) == 1)) THEN
            indtab(i,1,1) = indtab(i,1,1) - coeffraf/2-3
            indtab(i,1,2) = indtab(i,1,2) + coeffraf/2+3
          ENDIF          
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

#if !defined AGRIF_MPI
        Call Agrif_nbdim_Get_bound_dimension(child % var,lubglob(:,1),
     &               lubglob(:,2),nbdim)
C
#else
C     
        Call Agrif_nbdim_Get_bound_dimension(child % var,lb,ub,nbdim)
      DO i = 1,nbdim
C
        Call Agrif_Invloc(lb(i),Agrif_Procrank,i,iminmaxg(i,1))
        Call Agrif_Invloc(ub(i),Agrif_Procrank,i,iminmaxg(i,2))
C
      ENDDO
C
      iminmaxg(1:nbdim,2) = - iminmaxg(1:nbdim,2)
      
      CALL MPI_ALLREDUCE(iminmaxg,lubglob,2*nbdim,MPI_INTEGER,MPI_MIN,
     &                     MPI_COMM_WORLD,code)

      lubglob(1:nbdim,2) = - lubglob(1:nbdim,2)      
C
#endif 
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
#ifdef AGRIF_MPI
C
#include "mpif.h"
C
#endif
C
C     Arguments
      INTEGER, DIMENSION(6) :: TypeUpdate            ! TYPE of update 
                                                     !   (copy or average) 
      TYPE(AGRIF_PVariable)    :: parent             ! Variable on the parent
                                                     !   grid
      TYPE(AGRIF_PVariable)    :: child              ! Variable on the child
                                                     !   grid 
      INTEGER, DIMENSION(6) :: deb, fin
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
#ifdef AGRIF_MPI
C
      INTEGER,DIMENSION(nbdim)   :: lb,ub
      INTEGER,DIMENSION(nbdim,2) :: iminmaxg
      INTEGER                    :: code
C
#endif
C      
C

      DO i = 1, nbdim
        coeffraf = nint(ds_Parent(i)/ds_Child(i))
        indtab(i,1,1) = pttab_child(i) + (deb(i) + 1) * coeffraf
        indtab(i,1,2) = pttab_child(i) + (fin(i) + 1) * coeffraf

        indtab(i,2,1) = pttab_child(i) + nbtab_child(i)
     &    - (fin(i) + 1) *  coeffraf
        indtab(i,2,2) = pttab_child(i) + nbtab_child(i)
     &    - (deb(i) + 1) *  coeffraf

        IF (posvartab_child(i) == 1) THEN
          IF (TypeUpdate(i) .EQ. Agrif_Update_Full_Weighting) THEN
            indtab(i,:,1) = indtab(i,:,1) - (coeffraf - 1)
            indtab(i,:,2) = indtab(i,:,2) + (coeffraf - 1)        
          ELSE IF (TypeUpdate(i) .NE. Agrif_Update_Copy) THEN
            indtab(i,:,1) = indtab(i,:,1) - coeffraf / 2
            indtab(i,:,2) = indtab(i,:,2) + coeffraf / 2
          ENDIF
        ELSE
          indtab(i,1,1) = indtab(i,1,1) - coeffraf
          indtab(i,1,2) = indtab(i,1,2) - 1
          indtab(i,2,2) = indtab(i,2,2) + coeffraf - 1
          IF (TypeUpdate(i) .EQ. Agrif_Update_Full_Weighting) THEN
            indtab(i,1,1) = indtab(i,1,1) - coeffraf/2
            indtab(i,1,2) = indtab(i,1,2) + coeffraf/2
            indtab(i,2,1) = indtab(i,2,1) - coeffraf/2
            indtab(i,2,2) = indtab(i,2,2) + coeffraf/2
          ENDIF     
          IF (TypeUpdate(i) .EQ. Agrif_Update_4rdorder) THEN
            indtab(i,1,1) = indtab(i,1,1) - coeffraf/2-3
            indtab(i,1,2) = indtab(i,1,2) + coeffraf/2+3
            indtab(i,2,1) = indtab(i,2,1) - coeffraf/2-3
            indtab(i,2,2) = indtab(i,2,2) + coeffraf/2+3
          ENDIF           
        ENDIF
      ENDDO

#if !defined AGRIF_MPI      
        Call Agrif_nbdim_Get_bound_dimension(child % var,lubglob(:,1),
     &               lubglob(:,2),nbdim)

C
#else
C     
        Call Agrif_nbdim_Get_bound_dimension(child % var,lb,ub,nbdim)
      DO i = 1,nbdim
C
        Call Agrif_Invloc(lb(i),Agrif_Procrank,i,iminmaxg(i,1))
        Call Agrif_Invloc(ub(i),Agrif_Procrank,i,iminmaxg(i,2))
C
      ENDDO
C
      iminmaxg(1:nbdim,2) = - iminmaxg(1:nbdim,2)
      
      CALL MPI_ALLREDUCE(iminmaxg,lubglob,2*nbdim,MPI_INTEGER,MPI_MIN,
     &                     MPI_COMM_WORLD,code)  
     
      lubglob(1:nbdim,2) = - lubglob(1:nbdim,2)      
C
#endif 
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
     &              nbdim,procname,nb,ndir)
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
     &                          nbdim,procname,nb,ndir)  
C
C     Description:
C     Subroutine to update a 2D grid variable on the parent grid of 
C        the current grid. 
C
C     Declarations:
C
      
C
#ifdef AGRIF_MPI
C
#include "mpif.h"
C
#endif
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
      Integer :: nb,ndir
      Optional :: nb,ndir
      
C
C     Local pointers
      TYPE(AGRIF_PVARIABLE), SAVE      :: tempP      ! Temporary parent grid variable
      TYPE(AGRIF_PVARIABLE), SAVE      :: tempC      ! Temporary child grid variable
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
      TYPE(AGRIF_PVARIABLE), SAVE      :: tempCextend,tempPextend ! Temporary child
      INTEGER :: nbin, ndirin
C
#ifdef AGRIF_MPI
C
      INTEGER,DIMENSION(nbdim)    :: indminglob2,indmaxglob2
      LOGICAL, DIMENSION(0:Agrif_Nbprocs-1) :: sendtoproc1,recvfromproc1
      LOGICAL, DIMENSION(0:Agrif_Nbprocs-1) :: sendtoproc2,recvfromproc2      
      INTEGER                              :: code
      INTEGER                              :: i,j,k
      INTEGER,DIMENSION(nbdim,4)           :: tab3
      INTEGER,DIMENSION(nbdim,4,0:Agrif_Nbprocs-1) :: tab4
      INTEGER,DIMENSION(nbdim,0:Agrif_Nbprocs-1,8) :: tab4t
      INTEGER,DIMENSION(nbdim,0:Agrif_Nbprocs-1,8) :: tab5t      
      LOGICAL :: find_list_update
      LOGICAL, DIMENSION(0:Agrif_Nbprocs-1) :: memberinall, memberinall2
      LOGICAL, DIMENSION(1) :: memberin1      
C
#endif
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
#ifdef AGRIF_MPI
     &                 ,pttruetabwhole,cetruetabwhole
#endif
     &     )

#ifdef AGRIF_MPI
      IF (memberin) THEN
       Call Agrif_GlobtoLocInd2(childarray,
     &                     lowerbound,upperbound,
     &                     pttruetab,cetruetab,
     &                     nbdim,Agrif_Procrank,
     &                     member)

      ENDIF


         Call Agrif_Prtbounds(nbdim,indmin,indmax,s_Parent_temp,
     &                     s_Child_temp,s_Child,ds_Child,
     &                     s_Parent,ds_Parent,
     &                     pttruetab,cetruetab,pttab_Child,
     &                     pttab_Parent,
     &                     posvartab_Child,TypeUpdate,loctab_Child
     &                 ,pttruetabwhole,cetruetabwhole
     &     )

#else
       indmin = indminglob
       indmax = indmaxglob
       pttruetabwhole = pttruetab
       cetruetabwhole = cetruetab
       childarray(:,1,2) = pttruetab
       childarray(:,2,2) = cetruetab
#endif

       IF (present(procname)) THEN
       IF (.Not.present(nb)) THEN
       nbin=0
       ndirin=0
       ELSE
       nbin = nb
       ndirin = ndir
       ENDIF
       ENDIF
       
      IF (memberin) THEN
      IF (.not.associated(tempC%var)) allocate(tempC%var)

C
      Call Agrif_nbdim_allocation(tempC%var,
     &                 pttruetab,cetruetab,nbdim)

      Call Agrif_nbdim_Full_VarEQreal(tempC%var,0.,nbdim)

      IF (present(procname)) THEN
        SELECT CASE (nbdim)
        CASE(1)
          CALL procname(tempC%var%array1,
     &                          childarray(1,1,2),childarray(1,2,2),
     &                                   .TRUE.,nbin,ndirin)
        CASE(2)
          CALL procname(tempC%var%array2,
     &                          childarray(1,1,2),childarray(1,2,2),
     &                          childarray(2,1,2),childarray(2,2,2),
     &                                   .TRUE.,nbin,ndirin)
        CASE(3)
          CALL procname(tempC%var%array3,
     &                          childarray(1,1,2),childarray(1,2,2),
     &                          childarray(2,1,2),childarray(2,2,2),
     &                          childarray(3,1,2),childarray(3,2,2),
     &                                   .TRUE.,nbin,ndirin)
        CASE(4)
          CALL procname(tempC%var%array4,
     &                          childarray(1,1,2),childarray(1,2,2),
     &                          childarray(2,1,2),childarray(2,2,2),
     &                          childarray(3,1,2),childarray(3,2,2),
     &                          childarray(4,1,2),childarray(4,2,2),
     &                                   .TRUE.,nbin,ndirin)
        CASE(5)
          CALL procname(tempC%var%array5,
     &                          childarray(1,1,2),childarray(1,2,2),
     &                          childarray(2,1,2),childarray(2,2,2),
     &                          childarray(3,1,2),childarray(3,2,2),
     &                          childarray(4,1,2),childarray(4,2,2),
     &                          childarray(5,1,2),childarray(5,2,2),
     &                                   .TRUE.,nbin,ndirin)
        CASE(6)
          CALL procname(tempC%var%array6,
     &                          childarray(1,1,2),childarray(1,2,2),
     &                          childarray(2,1,2),childarray(2,2,2),
     &                          childarray(3,1,2),childarray(3,2,2),
     &                          childarray(4,1,2),childarray(4,2,2),
     &                          childarray(5,1,2),childarray(5,2,2),
     &                          childarray(6,1,2),childarray(6,2,2),
     &                                   .TRUE.,nbin,ndirin)
        END SELECT
      ELSE
      Call Agrif_nbdim_VarEQvar(tempC%var,pttruetab,cetruetab,
     &          child%var,childarray(:,1,2),childarray(:,2,2),
     &                          nbdim)
      ENDIF

      ENDIF



C
C
#ifdef AGRIF_MPI
C
C     tab2 contains the necessary limits of the parent grid for each processor

      IF (Associated(child%var%list_update)) THEN
      Call Agrif_Find_list_update(child%var%list_update,pttab,petab,
     &                          pttab_Child,pttab_Parent,nbdim,
     &       find_list_update,tab4t,tab5t,memberinall,memberinall2,
     &     sendtoproc1,recvfromproc1,sendtoproc2,recvfromproc2)
      ELSE
      find_list_update = .FALSE.
      ENDIF
      
      if (.not.find_list_update) then      
      tab3(:,1) = pttruetab(:)
      tab3(:,2) = cetruetab(:)
      tab3(:,3) = pttruetabwhole(:)
      tab3(:,4) = cetruetabwhole(:)
C
C
      Call MPI_ALLGATHER(tab3,4*nbdim,MPI_INTEGER,tab4,4*nbdim,
     &                   MPI_INTEGER,MPI_COMM_WORLD,code)

      IF (.not.associated(tempCextend%var)) Allocate(tempCextend%var)
      DO k=0,Agrif_Nbprocs-1
       do j=1,4
         do i=1,nbdim
         tab4t(i,k,j) = tab4(i,j,k)
         enddo
       enddo
      enddo
      
      memberin1(1) = memberin
      CALL MPI_ALLGATHER(memberin1,1,MPI_LOGICAL,memberinall,
     &                  1,MPI_LOGICAL,MPI_COMM_WORLD,code)
 
       Call Get_External_Data_first(tab4t(:,:,1),
     &            tab4t(:,:,2),
     &            tab4t(:,:,3),tab4t(:,:,4),nbdim,memberin,memberin,
     &            memberinall,sendtoproc1,recvfromproc1,tab4t(:,:,5),
     & tab4t(:,:,6),tab4t(:,:,7),tab4t(:,:,8))
               
      endif

      Call ExchangeSameLevel2(sendtoproc1,recvfromproc1,nbdim,
     &            tab4t(:,:,3),tab4t(:,:,4),tab4t(:,:,5),tab4t(:,:,6),
     &            tab4t(:,:,7),tab4t(:,:,8),memberin,tempC,
     &            tempCextend) 
           
!      Call Get_External_Data(tempC,tempCextend,tab4t(:,:,1),
!     &            tab4t(:,:,2),
!     &            tab4t(:,:,3),tab4t(:,:,4),nbdim,memberin,memberin,
!     &            memberinall)

#else
      tempCextend%var => tempC%var
#endif

C
C
C     Update of the parent grid (tempP) from the child grid (tempC)


      IF (memberin) THEN

      IF (.not.associated(tempP%var)) allocate(tempP%var)
      Call Agrif_nbdim_allocation(tempP%var,
     &                 indmin,indmax,nbdim)

      if ( nbdim .EQ. 1 ) then
         tempP%var%array1 = 0.
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
C      Deallocate(tempCextend%var)

      ENDIF

#ifdef AGRIF_MPI
      Call Agrif_nbdim_Get_bound_dimension(parent%var,
     &                              lowerbound,upperbound,nbdim)

      Call Agrif_ChildGrid_to_ParentGrid()
C
      Call Agrif_Childbounds(nbdim,
     &                       lowerbound,upperbound,
     &                       indminglob,indmaxglob,
     &                       indminglob2,indmaxglob2,member)
C
      IF (member) THEN
      Call Agrif_GlobtoLocInd2(parentarray,
     &                     lowerbound,upperbound,
     &                     indminglob2,indmaxglob2,
     &                     nbdim,Agrif_Procrank,
     &                     member)
      ENDIF

      Call Agrif_ParentGrid_to_ChildGrid()

      if (.not.find_list_update) then
      tab3(:,1) = indmin(:)
      tab3(:,2) = indmax(:)
      tab3(:,3) = indminglob2(:)
      tab3(:,4) = indmaxglob2(:)
C
      Call MPI_ALLGATHER(tab3,4*nbdim,MPI_INTEGER,tab4,4*nbdim,
     &                   MPI_INTEGER,MPI_COMM_WORLD,code)

      IF (.not.associated(tempPextend%var)) Allocate(tempPextend%var)
      DO k=0,Agrif_Nbprocs-1
       do j=1,4
         do i=1,nbdim
         tab5t(i,k,j) = tab4(i,j,k)
         enddo
       enddo
      enddo
 
      memberin1(1) = member
      CALL MPI_ALLGATHER(memberin1,1,MPI_LOGICAL,memberinall2,
     &                  1,MPI_LOGICAL,MPI_COMM_WORLD,code)
 
      Call Get_External_Data_first(tab5t(:,:,1),
     &            tab5t(:,:,2),
     &            tab5t(:,:,3),tab5t(:,:,4),nbdim,memberin,member,
     &            memberinall2,sendtoproc2,recvfromproc2,tab5t(:,:,5),
     & tab5t(:,:,6),tab5t(:,:,7),tab5t(:,:,8))
          
      Call Agrif_Addto_list_update(child%var%list_update,pttab,petab,
     &                          pttab_Child,pttab_Parent,nbdim
     &   ,tab4t,tab5t,memberinall,memberinall2,
     &   sendtoproc1,recvfromproc1,sendtoproc2,recvfromproc2) 
          
      endif
      
c      Call Get_External_Data(tempP,tempPextend,tab5t(:,:,1),
c     &            tab5t(:,:,2),
c     &            tab5t(:,:,3),tab5t(:,:,4),nbdim,memberin,member,
c     &            memberinall2)
     
      Call ExchangeSameLevel2(sendtoproc2,recvfromproc2,nbdim,
     &            tab5t(:,:,3),tab5t(:,:,4),tab5t(:,:,5),tab5t(:,:,6),
     &            tab5t(:,:,7),tab5t(:,:,8),member,tempP,
     &            tempPextend)     

#else
      tempPextend%var => tempP%var
      parentarray(:,1,1) = indmin
      parentarray(:,2,1) = indmax
      parentarray(:,1,2) = indmin
      parentarray(:,2,2) = indmax
      member = .TRUE.
#endif

C
C
C
C     Special values on the child grid
      if (Agrif_UseSpecialValueFineGrid) then
C
ccc         noraftab(1:nbdim) =
ccc     &    child % var % root_var % interptab(1:nbdim) .EQ. 'N'
C
#ifdef AGRIF_MPI
C
c          Allocate(childvalues% var)
C
c          Call Agrif_nbdim_allocation(childvalues%var,
c     &                      pttruetab,cetruetab,nbdim)
c          Call Agrif_nbdim_Full_VarEQvar(childvalues% var,
c     &                                tempC% var,
c     &                                nbdim)
c          Call Agrif_CheckMasknD(tempC,childvalues,
c     &                           pttruetab(1:nbdim),cetruetab(1:nbdim),
c     &                           pttruetab(1:nbdim),cetruetab(1:nbdim),
c     &                           noraftab(1:nbdim),nbdim)
c          Call Agrif_nbdim_deallocation(childvalues% var,nbdim)
c         Deallocate(childvalues % var)
C
#else
C
c          Call Agrif_nbdim_Get_bound_dimension(child%var,
c     &                              lowerbound,upperbound,nbdim)
c          Call Agrif_CheckMasknD(tempC,child,
c     &                           pttruetab(1:nbdim),cetruetab(1:nbdim),
c     &                           lowerbound,
c     &                           upperbound,
c     &                           noraftab(1:nbdim),nbdim)
C
#endif
C
      endif


C
C
C
C
C     Special values on the parent grid
      if (Agrif_UseSpecialValue) then
C
#ifdef AGRIF_MPI
C
c          Call GiveAgrif_SpecialValueToTab_mpi(parent%var,tempP%var,
c     &                 parentarray,
c     &                 indmin,indmax,
c     &                 Agrif_SpecialValue,nbdim)
C
C
#else
C
c          Call GiveAgrif_SpecialValueToTab(parent%var,tempP%var,
c     &                  indmin,indmax,
c     &                  Agrif_SpecialValue,nbdim)
C
#endif
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
     &                                   .FALSE.,nbin,ndirin
     &                      )
            CASE(2)
            CALL procname(
     &      tempPextend%var%array2(
     &                      parentarray(1,1,1):parentarray(1,2,1),
     &                      parentarray(2,1,1):parentarray(2,2,1)),
     &                      parentarray(1,1,2),parentarray(1,2,2),
     &                      parentarray(2,1,2),parentarray(2,2,2),
     &                                   .FALSE.,nbin,ndirin
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
     &                                   .FALSE.,nbin,ndirin
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
     &                                   .FALSE.,nbin,ndirin
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
     &                                   .FALSE.,nbin,ndirin
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
     &                                   .FALSE.,nbin,ndirin
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
#ifdef AGRIF_MPI
      Call Agrif_nbdim_deallocation(tempP%var,nbdim)
      Call Agrif_nbdim_deallocation(tempC%var,nbdim)
!      Deallocate(tempC % var)
#endif
!      Deallocate(tempP % var)
      ENDIF
#ifdef AGRIF_MPI
!      Deallocate(tempPextend%var)
!      IF (.Not.memberin) Deallocate(tempCextend%var)
#endif

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
#ifdef AGRIF_MPI
     &                 ,pttruetabwhole,cetruetabwhole
#endif
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
#ifdef AGRIF_MPI
cccccccccccccccccccccccccc#include "mpif.h"
#endif
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
#ifdef AGRIF_MPI
      INTEGER,DIMENSION(nbdim)    :: pttruetabwhole,cetruetabwhole
      REAL :: positionmin,positionmax
      INTEGER :: imin,imax
      INTEGER :: coeffraf
#endif
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
        IF (loctab_Child(i) .NE. -3) THEN
        IF (posvartab_child(i) == 1) THEN
        ELSE
        IF (TypeUpdate(i) .EQ. Agrif_Update_4rdorder) THEN
          indmin(i) = indmin(i)+1
          indmax(i) = indmax(i)-1
        ENDIF        
        ENDIF        
        ENDIF
        
#ifdef AGRIF_MPI
        positionmin = s_Parent(i) + (indmin(i)-
     &                          pttab_Parent(i))*ds_Parent(i)
        IF (loctab_Child(i) .NE. -3) THEN
        IF (posvartab_child(i) == 1) THEN
          IF (TypeUpdate(i) .EQ. Agrif_Update_Average) THEN
          positionmin = positionmin - ds_Parent(i)/2.
          ELSE IF (TypeUpdate(i) .EQ. Agrif_Update_Full_Weighting) THEN
          positionmin = positionmin - (ds_Parent(i)-ds_Child(i))
          ENDIF
        ELSE
        IF (TypeUpdate(i) .EQ. Agrif_Update_4rdorder) THEN
          positionmin = positionmin - ds_Parent(i)/2.        
          positionmin = positionmin - ds_Child(i)-3*ds_Child(i)
        ELSE
          IF (TypeUpdate(i).NE.Agrif_Update_Full_Weighting) THEN
            positionmin = positionmin - ds_Parent(i)/2.
          ELSE
          coeffraf = nint(ds_Parent(i)/ds_Child(i))
          if (mod(coeffraf,2) == 1) then
            positionmin = positionmin - (ds_Parent(i)-ds_Child(i))
          else
            positionmin = positionmin - (ds_Parent(i)-ds_Child(i))-ds_Child(i)/2.
          endif
          ENDIF
        ENDIF
        ENDIF
        ENDIF
        imin = pttab_Child(i) +
     &        agrif_ceiling((positionmin-s_Child(i))/ds_Child(i))

       positionmin = s_Child(i) + (imin -
     &                                pttab_Child(i)) * ds_Child(i)

        pttruetabwhole(i) = imin

        positionmax = s_Parent(i) + (indmax(i)-
     &                          pttab_Parent(i))*ds_Parent(i)
        IF (loctab_Child(i) .NE. -3) THEN
        IF (posvartab_child(i) == 1) THEN
          IF (TypeUpdate(i) .EQ. Agrif_Update_Average) THEN
        positionmax = positionmax  + ds_Parent(i)/2.
          ELSE IF (TypeUpdate(i) .EQ. Agrif_Update_Full_Weighting) THEN
          positionmax = positionmax  + (ds_Parent(i)-ds_Child(i))
          ENDIF
        ELSE
        IF (TypeUpdate(i) .EQ. Agrif_Update_4rdorder) THEN
          positionmax = positionmax + ds_Child(i)+3*ds_Child(i)
        ELSE
          IF (TypeUpdate(i).NE.Agrif_Update_Full_Weighting) THEN        
            positionmax = positionmax  + ds_Parent(i)/2.
          ELSE
          coeffraf = nint(ds_Parent(i)/ds_Child(i))
          if (mod(coeffraf,2) == 1) then
            positionmax = positionmax + (ds_Parent(i)-ds_Child(i))
          else
            positionmax = positionmax + (ds_Parent(i)-ds_Child(i)) + ds_Child(i)/2.
          endif

          ENDIF    
        ENDIF
        ENDIF
        ENDIF
        imax = pttab_Child(i) +
     &        agrif_int((positionmax-s_Child(i))/ds_Child(i))

        positionmax = s_Child(i) + (imax -
     &                                pttab_Child(i)) * ds_Child(i)

        cetruetabwhole(i) = imax

#endif
C
        s_Parent_temp(i) = s_Parent(i) + 
     &                     (indmin(i) - pttab_Parent(i)) * 
     &                      ds_Parent(i) 
C     
        s_Child_temp(i) = dim_newmin(i)

#ifdef AGRIF_MPI
        s_Child_temp(i) = positionmin
#endif
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
C      REAL, DIMENSION(pttab_child(1):petab_child(1),
C     &                pttab_child(2):petab_child(2)) :: tempC
     
      REAL, DIMENSION(:,:) :: tempC     
C
C     Local variables      
      REAL, DIMENSION(indmin(1):indmax(1),
     &                 pttab_child(2):petab_child(2)) :: tabtemp
      REAL, DIMENSION(indmin(2):indmax(2),
     &                indmin(1):indmax(1))           :: tempP_trsp
      REAL, DIMENSION(pttab_child(2):petab_child(2),
     &                 indmin(1):indmax(1)) :: tabtemp_trsp
      INTEGER :: i,j
      INTEGER :: coeffraf,locind_child_left
C
      tabtemp = 0.


      coeffraf = nint ( ds_parent(1) / ds_child(1) ) 
      IF((TypeUpdate(1) == AGRIF_Update_average) 
     &                 .AND. (coeffraf /= 1 ))THEN
!---CDIR NEXPAND
         IF(.NOT. precomputedone(1) ) Call average1Dprecompute2D
     &       (petab_child(2)-pttab_child(2)+1,
     &        indmax(1)-indmin(1)+1,
     &        petab_child(1)-pttab_child(1)+1,
     &        s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
!---CDIR NEXPAND
          Call average1Daftercompute
     &       (  tabtemp, tempC,
     &          size(tabtemp), size(tempC),
     &          s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
 
      ELSEIF((TypeUpdate(1) == AGRIF_Update_copy)
     &                 .AND. (coeffraf /= 1 ))THEN
!---CDIR NEXPAND
         IF(.NOT. precomputedone(1) ) Call copy1Dprecompute2D
     &       (petab_child(2)-pttab_child(2)+1,
     &        indmax(1)-indmin(1)+1,
     &        petab_child(1)-pttab_child(1)+1,
     &        s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
!---CDIR NEXPAND
          Call copy1Daftercompute
     &       (  tabtemp, tempC,
     &          size(tabtemp), size(tempC),
     &          s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)

      ELSE
      do j = pttab_child(nbdim),petab_child(nbdim)
C
!---CDIR NEXPAND
        Call Agrif_Update_1D_recursive(TypeUpdate(1:nbdim-1),    
     &         tabtemp(:,j),
     &         tempC(:,j-pttab_child(nbdim)+1),
     &         indmin(1:nbdim-1),indmax(1:nbdim-1),
     &         pttab_child(1:nbdim-1),petab_child(1:nbdim-1),
     &         s_child(1:nbdim-1),s_parent(1:nbdim-1),
     &         ds_child(1:nbdim-1),ds_parent(1:nbdim-1),nbdim-1)
C
      enddo
      ENDIF
      tabtemp_trsp = TRANSPOSE(tabtemp) 
      coeffraf = nint(ds_parent(nbdim)/ds_child(nbdim))
    
!---CDIR NEXPAND
      Call Agrif_Compute_nbdim_update(s_parent(nbdim),s_child(nbdim),
     &  ds_parent(nbdim),ds_child(nbdim),coeffraf,locind_child_left)
C
      
      tempP_trsp = 0. 

      IF((TypeUpdate(2) == AGRIF_Update_average)
     &                .AND. (coeffraf /= 1 ))THEN
!---CDIR NEXPAND
         IF(.NOT. precomputedone(2) ) Call average1Dprecompute2D
     &      ( indmax(1)-indmin(1)+1,
     &        indmax(2)-indmin(2)+1,
     &        petab_child(2)-pttab_child(2)+1,
     &        s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)
!---CDIR NEXPAND
          Call average1Daftercompute
     &       (  tempP_trsp, tabtemp_trsp,
     &          size(tempP_trsp), size(tabtemp_trsp),
     &          s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)

      ELSEIF((TypeUpdate(2) == AGRIF_Update_copy)
     &                .AND. (coeffraf /= 1 ))THEN
!---CDIR NEXPAND
         IF(.NOT. precomputedone(2) ) Call copy1Dprecompute2D
     &      ( indmax(1)-indmin(1)+1,
     &        indmax(2)-indmin(2)+1,
     &        petab_child(2)-pttab_child(2)+1,
     &        s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)
!---CDIR NEXPAND
          Call copy1Daftercompute
     &       (  tempP_trsp, tabtemp_trsp,
     &          size(tempP_trsp), size(tabtemp_trsp),
     &          s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)

      ELSE

      do i = indmin(1),indmax(1)
C
!---CDIR NEXPAND
        Call Agrif_UpdateBase(TypeUpdate(2),
     &           tempP_trsp(indmin(nbdim):indmax(nbdim),i),
     &          tabtemp_trsp(pttab_child(nbdim):petab_child(nbdim),i),
     &           indmin(nbdim),indmax(nbdim),
     &           pttab_child(nbdim),petab_child(nbdim),
     &           s_parent(nbdim),s_child(nbdim),
     &           ds_parent(nbdim),ds_child(nbdim),
     &                  coeffraf,locind_child_left)
C        
      enddo
      
      ENDIF

      tempP = TRANSPOSE(tempP_trsp)
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
      REAL, DIMENSION(indmin(1):indmax(1),
     &                 indmin(2):indmax(2), 
     &                 pttab_child(3):petab_child(3)) :: tabtemp
      INTEGER :: i,j,k
      INTEGER :: coeffraf,locind_child_left
      INTEGER :: kdeb
C
C
      coeffraf = nint ( ds_parent(1) / ds_child(1) )
      IF((TypeUpdate(1) == AGRIF_Update_average)
     &                 .AND. (coeffraf /= 1 ))THEN
!---CDIR NEXPAND
         Call average1Dprecompute2D
     &       (petab_child(2)-pttab_child(2)+1,
     &        indmax(1)-indmin(1)+1,
     &        petab_child(1)-pttab_child(1)+1,
     &        s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
      precomputedone(1) = .TRUE.
      ELSEIF((TypeUpdate(1) == AGRIF_Update_copy)
     &                 .AND. (coeffraf /= 1 ))THEN
!---CDIR NEXPAND
         Call copy1Dprecompute2D
     &       (petab_child(2)-pttab_child(2)+1,
     &        indmax(1)-indmin(1)+1,
     &        petab_child(1)-pttab_child(1)+1,
     &        s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
      precomputedone(1) = .TRUE.
      ENDIF

      coeffraf = nint ( ds_parent(2) / ds_child(2) )
      IF((TypeUpdate(2) == AGRIF_Update_average)
     &                .AND. (coeffraf /= 1 ))THEN
!---CDIR NEXPAND
         Call average1Dprecompute2D
     &      ( indmax(1)-indmin(1)+1,
     &        indmax(2)-indmin(2)+1,
     &        petab_child(2)-pttab_child(2)+1,
     &        s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)
      precomputedone(2) = .TRUE.
      ELSEIF((TypeUpdate(2) == AGRIF_Update_copy)
     &                .AND. (coeffraf /= 1 ))THEN
!---CDIR NEXPAND
         Call copy1Dprecompute2D
     &      ( indmax(1)-indmin(1)+1,
     &        indmax(2)-indmin(2)+1,
     &        petab_child(2)-pttab_child(2)+1,
     &        s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)
      precomputedone(2) = .TRUE.
      ENDIF


      do k = pttab_child(nbdim),petab_child(nbdim)
C
        Call Agrif_Update_2D_recursive(TypeUpdate(1:nbdim-1),    
     &         tabtemp(:,:,k),
     &         tempC(:,:,k),
     &         indmin(1:nbdim-1),indmax(1:nbdim-1),
     &         pttab_child(1:nbdim-1),petab_child(1:nbdim-1),
     &         s_child(1:nbdim-1),s_parent(1:nbdim-1),
     &         ds_child(1:nbdim-1),ds_parent(1:nbdim-1),nbdim-1)
C
      enddo
C
      precomputedone(1) = .FALSE.
      precomputedone(2) = .FALSE.
      coeffraf = nint ( ds_parent(3) / ds_child(3) )

      Call Agrif_Compute_nbdim_update(s_parent(nbdim),s_child(nbdim),
     &  ds_parent(nbdim),ds_child(nbdim),coeffraf,locind_child_left)
      
      IF (coeffraf == 1) THEN
      
      kdeb = pttab_child(3)+locind_child_left-2
      do k=indmin(3),indmax(3)
      kdeb = kdeb + 1
      do j = indmin(2),indmax(2)
        do i = indmin(1),indmax(1)
        tempP(i,j,k) = tabtemp(i,j,kdeb)
      enddo
      enddo
      enddo
              
      ELSE
      tempP = 0.
C
      do j = indmin(2),indmax(2)
C
        do i = indmin(1),indmax(1)
C
          Call Agrif_UpdateBase(TypeUpdate(3),
     &           tempP(i,j,:),
     &          tabtemp(i,j,:),
     &           indmin(nbdim),indmax(nbdim),
     &           pttab_child(nbdim),petab_child(nbdim),
     &           s_parent(nbdim),s_child(nbdim),
     &           ds_parent(nbdim),ds_child(nbdim),
     &                  coeffraf,locind_child_left)
C
        enddo 
C        
      enddo
      ENDIF
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
      INTEGER :: coeffraf,locind_child_left
C
C
      Allocate(tabtemp(indmin(1):indmax(1),
     &                 indmin(2):indmax(2),
     &                 indmin(3):indmax(3), 
     &                 pttab_child(4):petab_child(4)))
C
      do l = pttab_child(nbdim),petab_child(nbdim)
C
        Call Agrif_Update_3D_recursive(TypeUpdate(1:nbdim-1),    
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
      
      Call Agrif_Compute_nbdim_update(s_parent(nbdim),s_child(nbdim),
     &  ds_parent(nbdim),ds_child(nbdim),coeffraf,locind_child_left)
C
      tempP = 0.

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
     &           ds_parent(nbdim),ds_child(nbdim),
     &                  coeffraf,locind_child_left)
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
      INTEGER :: coeffraf,locind_child_left
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
        Call Agrif_Update_4D_recursive(TypeUpdate(1:nbdim-1),    
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
      
      Call Agrif_Compute_nbdim_update(s_parent(nbdim),s_child(nbdim),
     &  ds_parent(nbdim),ds_child(nbdim),coeffraf,locind_child_left)
      tempP = 0.
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
     &           ds_parent(nbdim),ds_child(nbdim),
     &                  coeffraf,locind_child_left)
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
      INTEGER :: coeffraf,locind_child_left
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
        Call Agrif_Update_5D_recursive(TypeUpdate(1:nbdim-1),    
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
      
      Call Agrif_Compute_nbdim_update(s_parent(nbdim),s_child(nbdim),
     &  ds_parent(nbdim),ds_child(nbdim),coeffraf,locind_child_left)
C
      tempP = 0.

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
     &           ds_parent(nbdim),ds_child(nbdim),
     &                  coeffraf,locind_child_left)
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
     &                            s_parent,s_child,ds_parent,ds_child,
     &                            coeffraf,locind_child_left)
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
      INTEGER :: coeffraf,locind_child_left
C
C
      if (TypeUpdate == AGRIF_Update_copy) then
C             
          Call agrif_copy1D
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
     &          s_parent,s_child,ds_parent,ds_child,
     &          coeffraf,locind_child_left)
C
        elseif (TypeUpdate == Agrif_Update_4rdorder) then
C             
          Call Fourthorder1d
     &       (parenttab,childtab,
     &          indmax-indmin+1,petab_child-pttab_child+1,
     &          s_parent,s_child,ds_parent,ds_child,
     &          coeffraf,locind_child_left)     
C
      endif 
C
      Return                
C
C      
      End Subroutine Agrif_UpdateBase
C
C

      Subroutine Agrif_Compute_nbdim_update(s_parent,s_child,
     &  ds_parent,ds_child,coeffraf,locind_child_left)
      real :: s_parent,s_child,ds_parent,ds_child
      integer :: coeffraf,locind_child_left
     
      coeffraf = nint(ds_parent/ds_child)
      locind_child_left = 1 + agrif_int((s_parent-s_child)/ds_child)
      
      End Subroutine Agrif_Compute_nbdim_update
 
#if defined AGRIF_MPI      
      Subroutine Agrif_Find_list_update(list_update,pttab,petab,
     &                          pttab_Child,pttab_Parent,nbdim,
     &     find_list_update,tab4t,tab5t,memberinall,memberinall2,
     &     sendtoproc1,recvfromproc1,sendtoproc2,recvfromproc2)     
      TYPE(Agrif_List_Interp_Loc), Pointer :: list_update
      INTEGER :: nbdim
      INTEGER,DIMENSION(nbdim)   :: pttab,petab,pttab_Child,pttab_Parent
      LOGICAL :: find_list_update
      Type(Agrif_List_Interp_loc), Pointer :: parcours
      INTEGER :: i
C
      INTEGER,DIMENSION(nbdim,0:Agrif_Nbprocs-1,8) :: tab4t
      INTEGER,DIMENSION(nbdim,0:Agrif_Nbprocs-1,8) :: tab5t      
      LOGICAL, DIMENSION(0:Agrif_Nbprocs-1) :: memberinall,memberinall2
      LOGICAL, DIMENSION(0:Agrif_Nbprocs-1) :: sendtoproc1,recvfromproc1      
      LOGICAL, DIMENSION(0:Agrif_Nbprocs-1) :: sendtoproc2,recvfromproc2      
                    
      find_list_update = .FALSE.
      
      parcours => list_update
      
      Find_loop :   Do While (associated(parcours))
        Do i=1,nbdim
          IF ((pttab(i) /= parcours%interp_loc%pttab(i)).OR.
     &        (petab(i) /= parcours%interp_loc%petab(i)).OR.
     &        (pttab_child(i) /= parcours%interp_loc%pttab_child(i)).OR.
     &        (pttab_parent(i) /= parcours%interp_loc%pttab_parent(i)))
     &               THEN
            parcours=>parcours%suiv
            Cycle Find_loop
          ENDIF
        EndDo
        
        tab4t = parcours%interp_loc%tab4t(1:nbdim,0:Agrif_Nbprocs-1,1:8)
        memberinall = parcours%interp_loc%memberinall(0:Agrif_Nbprocs-1)
        
        tab5t = parcours%interp_loc%tab5t(1:nbdim,0:Agrif_Nbprocs-1,1:8)
        memberinall2 = 
     &       parcours%interp_loc%memberinall2(0:Agrif_Nbprocs-1)
        sendtoproc1 = 
     &       parcours%interp_loc%sendtoproc1(0:Agrif_Nbprocs-1)   
        recvfromproc1 = 
     &       parcours%interp_loc%recvfromproc1(0:Agrif_Nbprocs-1)     
        sendtoproc2 = 
     &       parcours%interp_loc%sendtoproc2(0:Agrif_Nbprocs-1)   
        recvfromproc2 = 
     &       parcours%interp_loc%recvfromproc2(0:Agrif_Nbprocs-1)          
        
        find_list_update = .TRUE.   
        Exit Find_loop
      End Do Find_loop  
                              
      End Subroutine Agrif_Find_list_update  
      
      Subroutine Agrif_AddTo_list_update(list_update,pttab,petab,
     &                          pttab_Child,pttab_Parent,nbdim
     &      ,tab4t,tab5t,memberinall,memberinall2,
     &      sendtoproc1,recvfromproc1,sendtoproc2,recvfromproc2)
          
      TYPE(Agrif_List_Interp_Loc), Pointer :: list_update
      INTEGER :: nbdim
      INTEGER,DIMENSION(nbdim)   :: pttab,petab,pttab_Child,pttab_Parent
      INTEGER,DIMENSION(nbdim,0:Agrif_Nbprocs-1,8) :: tab4t
      INTEGER,DIMENSION(nbdim,0:Agrif_Nbprocs-1,8) :: tab5t      
      LOGICAL,DIMENSION(0:Agrif_Nbprocs-1) :: memberinall, memberinall2
      LOGICAL,DIMENSION(0:Agrif_Nbprocs-1) :: sendtoproc1, recvfromproc1      
      LOGICAL,DIMENSION(0:Agrif_Nbprocs-1) :: sendtoproc2, recvfromproc2

      Type(Agrif_List_Interp_loc), Pointer :: parcours
            
       Allocate(parcours)
      Allocate(parcours%interp_loc)
      
      parcours%interp_loc%pttab(1:nbdim) = pttab(1:nbdim)
      parcours%interp_loc%petab(1:nbdim) = petab(1:nbdim)
      parcours%interp_loc%pttab_child(1:nbdim) = pttab_child(1:nbdim)
      parcours%interp_loc%pttab_parent(1:nbdim) = pttab_parent(1:nbdim)
      Allocate(parcours%interp_loc%tab4t(nbdim,0:Agrif_Nbprocs-1,8))
      Allocate(parcours%interp_loc%memberinall(0:Agrif_Nbprocs-1))
      
      Allocate(parcours%interp_loc%tab5t(nbdim,0:Agrif_Nbprocs-1,8))
      Allocate(parcours%interp_loc%memberinall2(0:Agrif_Nbprocs-1))
      Allocate(parcours%interp_loc%sendtoproc1(0:Agrif_Nbprocs-1))
      Allocate(parcours%interp_loc%recvfromproc1(0:Agrif_Nbprocs-1))      
      Allocate(parcours%interp_loc%sendtoproc2(0:Agrif_Nbprocs-1))
      Allocate(parcours%interp_loc%recvfromproc2(0:Agrif_Nbprocs-1))            
            
      parcours%interp_loc%tab4t=tab4t   
      parcours%interp_loc%memberinall=memberinall
 
      parcours%interp_loc%tab5t=tab5t   
      parcours%interp_loc%memberinall2=memberinall2
      parcours%interp_loc%sendtoproc1=sendtoproc1   
      parcours%interp_loc%recvfromproc1=recvfromproc1      
      parcours%interp_loc%sendtoproc2=sendtoproc2       
      parcours%interp_loc%recvfromproc2=recvfromproc2      
            
      parcours%suiv => list_update
      
      list_update => parcours
      
      End Subroutine Agrif_Addto_list_update
#endif
           

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
      INTEGER :: coeffraf,locind_child_left
C
C
      Call Agrif_Compute_nbdim_update(s_parent(nbdim),s_child(nbdim),
     &  ds_parent(nbdim),ds_child(nbdim),coeffraf,locind_child_left)
      
      Call Agrif_UpdateBase(TypeUpdate(1),
     &                  tempP(indmin(nbdim):indmax(nbdim)),
     &                  tempC(pttab_child(nbdim):petab_child(nbdim)),
     &                  indmin(nbdim),indmax(nbdim),           
     &                  pttab_child(nbdim),petab_child(nbdim),
     &                  s_parent(nbdim),s_child(nbdim),
     &                  ds_parent(nbdim),ds_child(nbdim),
     &                  coeffraf,locind_child_left)

C
      Return
C
C
      End Subroutine Agrif_Update_1D_recursive      

      End Module Agrif_Update
