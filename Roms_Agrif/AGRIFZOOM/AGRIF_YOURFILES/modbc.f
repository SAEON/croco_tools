

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
CCC   Module Agrif_Boundary 
C 
      Module Agrif_Boundary  
C
CCC   Description:
CCC   Module to calculate the boundary conditions on the child grids from their
CCC   parent grids.
C
C     Modules used:  
C   
      Use Agrif_Interpolation        
C
      IMPLICIT NONE
C        
      CONTAINS   
C     Define procedures contained in this module
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_bc_1d
C     **************************************************************************
C 
      Subroutine Agrif_Interp_bc_1d(TypeInterp,parent,child,tab,deb,fin,
     &                              weight,pweight)            
C
CCC   Description:
CCC   Subroutine to calculate the boundary conditions on a fine grid for a 1D 
CCC   grid variable.
C
C     Declarations:
C      
      
C
C     Arguments      
      INTEGER,DIMENSION(6) :: TypeInterp    ! TYPE of interpolation
                                            ! (linear,...)
      TYPE(AGRIF_PVariable) :: parent       ! Variable on the parent grid
      TYPE(AGRIF_PVariable) :: child        ! Variable on the child grid
      TYPE(AGRIF_PVariable) :: childtemp    ! Temporary variable on the child 
                                            ! grid
      INTEGER :: deb,fin                    ! Positions of the  interpolations 
      REAL, DIMENSION(
     &    lbound(child%var%array1,1):ubound(child%var%array1,1)
     &    ), Target :: tab ! Values of the grid variable
      LOGICAL :: pweight                    ! Indicates if weight is used for  
                                            ! the temporal interpolation 
      REAL :: weight                        ! Coefficient for the time 
                                            ! interpolation
C
C
C     Definition of a temporary AGRIF_PVariable data TYPE representing the grid
C     variable.  
C
      allocate(childtemp % var)  
C
      childtemp % var % root_var => child % var % root_var
C      
C     Values of the grid variable
      childtemp % var % array1 => tab  
C
C     Temporary results for the time interpolation before and after the space 
C     interpolation 
      childtemp % var % oldvalues2D => child % var % oldvalues2D
C 
C     Index indicating if a space interpolation is necessary
      childtemp % var % interpIndex => child % var % interpIndex       
      childtemp % var % Interpolationshouldbemade = 
     &                 child % var % Interpolationshouldbemade       
C
C     Call to the procedure for the calculations of the boundary conditions
      Call Agrif_CorrectVariable
     &     (TypeInterp,parent,childtemp,deb,fin,pweight,weight)
C
      child % var % oldvalues2D => childtemp % var % oldvalues2D
C      
      deallocate(childtemp % var)
C
C       
      End Subroutine Agrif_Interp_bc_1D
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_bc_2d
C     **************************************************************************
C 
      Subroutine Agrif_Interp_bc_2d(TypeInterp,parent,child,tab,deb,fin,
     &                              weight,pweight,procname)
C
CCC   Description:
CCC   Subroutine to calculate the boundary conditions on a fine grid for a 2D 
CCC   grid variable.
C
C     Declarations:
C      
      
C
C     Arguments      
      External :: procname
      Optional :: procname
      INTEGER,DIMENSION(6) :: TypeInterp    ! TYPE of interpolation (linear, 
                                            ! lagrange, spline, ... )
      TYPE(AGRIF_PVariable) :: parent       ! Variable on the parent grid
      TYPE(AGRIF_PVariable) :: child        ! Variable on the child grid
      TYPE(AGRIF_PVariable) :: childtemp    ! Temporary variable on the child 
                                            ! grid
      INTEGER :: deb,fin                    ! Positions where interpolations are
                                            ! done on the fine grid 
      REAL, DIMENSION(
     &         lbound(child%var%array2,1): ubound(child%var%array2,1),
     &         lbound(child%var%array2,2): ubound(child%var%array2,2)), 
     &         Target :: tab ! Values of the grid variable
      LOGICAL :: pweight                    ! Indicates if weight is used for  
                                            ! the temporal interpolation 
      REAL :: weight                        ! Coefficient for the time 
                                            ! interpolation
C
C
C     Definition of a temporary AGRIF_PVariable data TYPE representing the grid
C     variable.   
C
      allocate(childtemp % var)  
C
      childtemp % var % root_var => child % var % root_var
C      
C     Values of the grid variable
      childtemp % var % array2 => tab  
C
C     Temporary results for the time interpolation before and after the space 
C     interpolation 
      childtemp % var % oldvalues2D => child % var % oldvalues2D
C 
C     Index indicating if a space interpolation is necessary
      childtemp % var % interpIndex => child % var % interpIndex       
      childtemp % var % Interpolationshouldbemade = 
     &                 child % var % Interpolationshouldbemade       
C
C     Call to the procedure for the calculations of the boundary conditions
      IF (present(procname)) THEN
      Call Agrif_CorrectVariable
     &     (TypeInterp,parent,childtemp,deb,fin,pweight,weight,procname)
      ELSE
      Call Agrif_CorrectVariable
     &     (TypeInterp,parent,childtemp,deb,fin,pweight,weight)
      ENDIF

C   
      child % var % oldvalues2D => childtemp % var % oldvalues2D
C         
      deallocate(childtemp % var)
C
C       
      End Subroutine Agrif_Interp_bc_2D
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_bc_3d
C     **************************************************************************
C 
      Subroutine Agrif_Interp_bc_3d(TypeInterp,parent,child,tab,deb,fin,
     &                              weight,pweight,procname)            
C
CCC   Description:
CCC   Subroutine to calculate the boundary conditions on a fine grid for a 3D 
CCC   variable.
C
C     Declarations:
C      
      
C
C     Arguments 
      External :: procname
      Optional :: procname     
      INTEGER,DIMENSION(6) :: TypeInterp    ! TYPE of interpolation (linear, 
                                            ! lagrange, spline, ... )
      TYPE(AGRIF_PVariable) :: parent       ! Variable on the parent grid
      TYPE(AGRIF_PVariable) :: child        ! Variable on the child grid
      TYPE(AGRIF_PVariable) :: childtemp    ! Temporary variable on the child
                                            ! grid
      INTEGER :: deb,fin                    ! Positions where interpolations 
                                            ! are done on the fine grid 
      REAL, DIMENSION(
     &         lbound(child%var%array3,1):ubound(child%var%array3,1),
     &         lbound(child%var%array3,2):ubound(child%var%array3,2),
     &         lbound(child%var%array3,3):ubound(child%var%array3,3)
     &         ), Target :: tab ! Values of the grid variable
      LOGICAL :: pweight                    ! Indicates if weight is used for 
                                            ! the temporal interpolation 
      REAL :: weight                        ! Coefficient for the time 
                                            ! interpolation
C
C
C     Definition of a temporary AGRIF_PVariable data TYPE representing the grid 
C     variable.  
C
      allocate(childtemp % var)  
C
      childtemp % var % root_var => child % var % root_var       
C      
C     Values of the grid variable
      childtemp % var % array3 => tab
C
C     Temporary results for the time interpolation before and after the space 
C     interpolation  
      childtemp % var % oldvalues2D => child % var % oldvalues2D
C 
C     Index indicating if a space interpolation is necessary
      childtemp % var % interpIndex => child % var % interpIndex
      childtemp % var % Interpolationshouldbemade = 
     &                 child % var % Interpolationshouldbemade       
C
C     Call to the procedure for the calculations of the boundary conditions     
      IF (present(procname)) THEN
      Call Agrif_CorrectVariable
     &     (TypeInterp,parent,childtemp,deb,fin,pweight,weight,procname)
      ELSE
      Call Agrif_CorrectVariable
     &     (TypeInterp,parent,childtemp,deb,fin,pweight,weight)
      ENDIF
C
      child % var % oldvalues2D => childtemp % var % oldvalues2D
C      
      deallocate(childtemp % var)
C
C       
      End Subroutine Agrif_Interp_bc_3D               
C
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_bc_4d
C     **************************************************************************
C 
      Subroutine Agrif_Interp_bc_4d(TypeInterp,parent,child,tab,deb,fin,
     &                             weight,pweight,procname)            
C
CCC   Description:
CCC   Subroutine to calculate the boundary conditions on a fine grid for a 4D 
CCC   grid variable.
C
C     Declarations:
C      
      
C
C     Arguments      
      External :: procname
      Optional :: procname     
      INTEGER,DIMENSION(6) :: TypeInterp      ! TYPE of interpolation (linear, 
                                              ! lagrange, spline, ... )
      TYPE(AGRIF_PVariable) :: parent         ! Variable on the parent grid
      TYPE(AGRIF_PVariable) :: child          ! Variable on the child grid
      TYPE(AGRIF_PVariable) :: childtemp      ! Temporary varaiable on the child
                                              ! grid
      INTEGER :: deb,fin                      ! Positions where interpolations 
                                              ! are done on the fine grid 
      REAL, DIMENSION(
     &        lbound(child%var%array4,1):ubound(child%var%array4,1),
     &        lbound(child%var%array4,2):ubound(child%var%array4,2),
     &        lbound(child%var%array4,3):ubound(child%var%array4,3),
     &        lbound(child%var%array4,4):ubound(child%var%array4,4)
     &        ), Target :: tab ! Values of the grid variable
      LOGICAL :: pweight                      ! Indicates if weight is used for 
                                              ! the temporal interpolation 
      REAL :: weight                          ! Coefficient for the time 
                                              ! interpolation
C
C
C     Definition of a temporary AGRIF_PVariable data TYPE representing the grid 
C     variable.  
C
      allocate(childtemp % var)  
C
      childtemp % var % root_var => child % var % root_var 
C      
C     Values of the grid variable
      childtemp % var % array4 => tab  
C
C     Temporary results for the time interpolation before and after the space 
C     interpolation       
      childtemp % var % oldvalues2D => child % var % oldvalues2D
C 
C     Index indicating if a space interpolation is necessary
      childtemp % var % interpIndex => child % var % interpIndex       
      childtemp % var % Interpolationshouldbemade = 
     &                 child % var % Interpolationshouldbemade       
C
C     Call to the procedure for the calculations of the boundary conditions
      IF (present(procname)) THEN
      Call Agrif_CorrectVariable
     &     (TypeInterp,parent,childtemp,deb,fin,pweight,weight,procname)
      ELSE
      Call Agrif_CorrectVariable
     &     (TypeInterp,parent,childtemp,deb,fin,pweight,weight)
      ENDIF
C
      child % var % oldvalues2D => childtemp % var % oldvalues2D
C      
      deallocate(childtemp % var)
C
C       
      End Subroutine Agrif_Interp_bc_4D
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_bc_5d
C     **************************************************************************
C 
      Subroutine Agrif_Interp_bc_5d(TypeInterp,parent,child,tab,deb,fin,
     &                             weight,pweight,procname)            
C
CCC   Description:
CCC   Subroutine to calculate the boundary conditions on a fine grid for a 5D 
CCC   grid variable.
C
C     Declarations:
C      
      
C
C     Arguments      
      External :: procname
      Optional :: procname     
      INTEGER,DIMENSION(6) :: TypeInterp      ! TYPE of interpolation (linear, 
                                              ! lagrange, spline, ... )
      TYPE(AGRIF_PVariable) :: parent         ! Variable on the parent grid
      TYPE(AGRIF_PVariable) :: child          ! Variable on the child grid
      TYPE(AGRIF_PVariable) :: childtemp      ! Temporary varaiable on the child
                                              ! grid
      INTEGER :: deb,fin                      ! Positions where interpolations 
                                              ! are done on the fine grid 
      REAL, DIMENSION(
     &         lbound(child%var%array5,1):ubound(child%var%array5,1),
     &         lbound(child%var%array5,2):ubound(child%var%array5,2),
     &         lbound(child%var%array5,3):ubound(child%var%array5,3),
     &         lbound(child%var%array5,4):ubound(child%var%array5,4),
     &         lbound(child%var%array5,5):ubound(child%var%array5,5)
     &         ), Target :: tab ! Values of the grid variable
      LOGICAL :: pweight                      ! Indicates if weight is used for 
                                              ! the temporal interpolation 
      REAL :: weight                          ! Coefficient for the time 
                                              ! interpolation
C
C
C     Definition of a temporary AGRIF_PVariable data TYPE representing the grid 
C     variable.  
C
      allocate(childtemp % var)  
C
      childtemp % var % root_var => child % var % root_var 
C      
C     Values of the grid variable
      childtemp % var % array5 => tab  
C
C     Temporary results for the time interpolation before and after the space 
C     interpolation       
      childtemp % var % oldvalues2D => child % var % oldvalues2D
C 
C     Index indicating if a space interpolation is necessary
      childtemp % var % interpIndex => child % var % interpIndex       
      childtemp % var % Interpolationshouldbemade = 
     &                 child % var % Interpolationshouldbemade       
C
C     Call to the procedure for the calculations of the boundary conditions  
      IF (present(procname)) THEN
      Call Agrif_CorrectVariable
     &     (TypeInterp,parent,childtemp,deb,fin,pweight,weight,procname)
      ELSE
      Call Agrif_CorrectVariable
     &     (TypeInterp,parent,childtemp,deb,fin,pweight,weight)
      ENDIF
C
      child % var % oldvalues2D => childtemp % var % oldvalues2D
C      
      deallocate(childtemp % var)
C
C       
      End Subroutine Agrif_Interp_bc_5D
C
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_bc_6d
C     **************************************************************************
C 
      Subroutine Agrif_Interp_bc_6d(TypeInterp,parent,child,tab,deb,fin,
     &                             weight,pweight)            
C
CCC   Description:
CCC   Subroutine to calculate the boundary conditions on a fine grid for a 6D 
CCC   grid variable.
C
C     Declarations:
C      
      
C
C     Arguments      
      INTEGER,DIMENSION(6) :: TypeInterp      ! TYPE of interpolation (linear, 
                                              ! lagrange, spline, ... )
      TYPE(AGRIF_PVariable) :: parent         ! Variable on the parent grid
      TYPE(AGRIF_PVariable) :: child          ! Variable on the child grid
      TYPE(AGRIF_PVariable) :: childtemp      ! Temporary varaiable on the child
                                              ! grid
      INTEGER :: deb,fin                      ! Positions where interpolations 
                                              ! are done on the fine grid 
      REAL, DIMENSION(
     &         lbound(child%var%array6,1):ubound(child%var%array6,1),
     &         lbound(child%var%array6,2):ubound(child%var%array6,2),
     &         lbound(child%var%array6,3):ubound(child%var%array6,3),
     &         lbound(child%var%array6,4):ubound(child%var%array6,4),
     &         lbound(child%var%array6,5):ubound(child%var%array6,5),
     &         lbound(child%var%array6,6):ubound(child%var%array6,6)
     &         ), Target :: tab ! Values of the grid variable
      LOGICAL :: pweight                      ! Indicates if weight is used for 
                                              ! the temporal interpolation 
      REAL :: weight                          ! Coefficient for the time 
                                              ! interpolation
C
C
C     Definition of a temporary AGRIF_PVariable data TYPE representing the grid 
C     variable.  
C
      allocate(childtemp % var)  
C
      childtemp % var % root_var => child % var % root_var 
C      
C     Values of the grid variable
      childtemp % var % array6 => tab  
C
C     Temporary results for the time interpolation before and after the space 
C     interpolation       
      childtemp % var % oldvalues2D => child % var % oldvalues2D
C 
C     Index indicating if a space interpolation is necessary
      childtemp % var % interpIndex => child % var % interpIndex       
      childtemp % var % Interpolationshouldbemade = 
     &                 child % var % Interpolationshouldbemade       
C
C     Call to the procedure for the calculations of the boundary conditions
      Call Agrif_CorrectVariable
     &     (TypeInterp,parent,childtemp,deb,fin,pweight,weight)
C
      child % var % oldvalues2D => childtemp % var % oldvalues2D
C      
      deallocate(childtemp % var)
C
C       
      End Subroutine Agrif_Interp_bc_6D
C
C
C     **************************************************************************
CCC   Subroutine Agrif_CorrectVariable
C     **************************************************************************
C
      Subroutine AGRIF_CorrectVariable(TypeInterp,parent,child,deb,fin,
     &                                 pweight,weight,procname)
C
CCC   Description:
CCC   Subroutine to calculate the boundary conditions on a fine grid.
C
C     Declarations:
C      
      
C
C     Arguments
      External :: procname
      Optional :: procname
      TYPE(AGRIF_PVariable) :: parent         ! Variable on the parent grid
      TYPE(AGRIF_PVariable) :: child          ! Variable on the child grid
      INTEGER,DIMENSION(6)  :: TypeInterp     ! TYPE of interpolation
                                              !    (linear,lagrange,...)
      INTEGER               :: deb,fin        ! Positions where boundary
                                              !    conditions are calculated
      LOGICAL               :: pweight        ! Indicates if weight is used 
                                              !    for the time interpolation 
      REAL                  :: weight         ! Coefficient for the time
                                              !    interpolation
C
C     Local scalars
      TYPE(Agrif_Grid)    , Pointer :: Agrif_Child_Gr,Agrif_Parent_Gr
      TYPE(AGRIF_Variable), Pointer :: root   ! Variable on the root grid
      INTEGER                       :: nbdim  ! Number of dimensions of 
                                              !    the grid variable 
      INTEGER                       :: n
      INTEGER,DIMENSION(6)          :: pttab_child  ! Index of the first point
                                                    !    inside the domain for
                                                    !    the child grid variable
      INTEGER,DIMENSION(6)          :: pttab_parent ! Index of the first point
                                                    !    inside the domain for
                                                    !    the parent grid
                                                    !    variable
      INTEGER,DIMENSION(6)          :: nbtab_Child  ! Number of the cells
      INTEGER,DIMENSION(6)          :: posvartab_Child    ! Position of the
                                                    !    variable on the cell 
      INTEGER,DIMENSION(6)          :: loctab_Child ! Indicates if the child
                                                    !    grid has a common
                                                    !    border with the root
                                                    !    grid      
      REAL, DIMENSION(6)            :: s_child,s_parent   ! Positions of the
                                                    !    parent and child grids
      REAL, DIMENSION(6)            :: ds_child,ds_parent ! Space steps of the
                                                    !    parent and child grids 
C
C
      loctab_child(:) = 0
C
      Agrif_Child_Gr => Agrif_Curgrid
      Agrif_Parent_Gr => Agrif_Curgrid % parent
      root => child % var % root_var 
      nbdim = root % nbdim
C
      do n = 1,nbdim
        posvartab_child(n) = root % posvar(n)      
      enddo
C
C
      do n = 1,nbdim
C
        Select case(root % interptab(n))
C
          case('x') ! x DIMENSION
C
            nbtab_Child(n) = Agrif_Child_Gr % nb(1)
            pttab_Child(n) = root % point(1)
            pttab_Parent(n) = root % point(1)
            s_Child(n) = Agrif_Child_Gr % Agrif_x(1)
            s_Parent(n) = Agrif_Parent_Gr % Agrif_x(1)
            ds_Child(n) = Agrif_Child_Gr % Agrif_d(1)
            ds_Parent(n) = Agrif_Parent_Gr % Agrif_d(1)
            if (root % posvar(n) == 2) then
                s_Child(n) = s_Child(n) + ds_Child(n)/2.
                s_Parent(n) = s_Parent(n) + ds_Parent(n)/2.
            endif
C
            if (Agrif_CURGRID % NearRootBorder(1)) 
     &         loctab_child(n) = -1
            if (Agrif_CURGRID % DistantRootBorder(1))
     &         loctab_child(n) = -2
            if ((Agrif_CURGRID % NearRootBorder(1)) .AND. 
     &          (Agrif_CURGRID % DistantRootBorder(1))) 
     &         loctab_child(n) = -3
C
          case('y') ! y DIMENSION      
C
            nbtab_Child(n) = Agrif_Child_Gr % nb(2)
            pttab_Child(n) = root % point(2)
            pttab_Parent(n) = root % point(2)
            s_Child(n) = Agrif_Child_Gr % Agrif_x(2)
            s_Parent(n) = Agrif_Parent_Gr % Agrif_x(2) 
            ds_Child(n) = Agrif_Child_Gr % Agrif_d(2)
            ds_Parent(n) = Agrif_Parent_Gr % Agrif_d(2)
            if (root % posvar(n) == 2) then        
                s_Child(n) = s_Child(n) + ds_Child(n)/2.
                s_Parent(n) = s_Parent(n) + ds_Parent(n)/2.
            endif        
C
            if (Agrif_CURGRID % NearRootBorder(2)) 
     &         loctab_child(n) = -1
            if (Agrif_CURGRID % DistantRootBorder(2)) 
     &         loctab_child(n) = -2
            if ((Agrif_CURGRID % NearRootBorder(2)) .AND. 
     &          (Agrif_CURGRID % DistantRootBorder(2))) 
     &         loctab_child(n) = -3
C
          case('z') ! z DIMENSION
C
            nbtab_Child(n) = Agrif_Child_Gr % nb(3)
            pttab_Child(n) = root % point(3)
            pttab_Parent(n) = root % point(3)
            s_Child(n) = Agrif_Child_Gr % Agrif_x(3)
            s_Parent(n) = Agrif_Parent_Gr % Agrif_x(3)
            ds_Child(n) = Agrif_Child_Gr % Agrif_d(3)
            ds_Parent(n) = Agrif_Parent_Gr % Agrif_d(3)
            if (root % posvar(n) == 2) then        
                s_Child(n) = s_Child(n) + ds_Child(n)/2.
                s_Parent(n) = s_Parent(n) + ds_Parent(n)/2.
            endif        
C
            if (Agrif_CURGRID % NearRootBorder(3)) 
     &         loctab_child(n) = -1
            if (Agrif_CURGRID % DistantRootBorder(3)) 
     &         loctab_child(n) = -2
            if ((Agrif_CURGRID % NearRootBorder(3)) .AND. 
     &          (Agrif_CURGRID % DistantRootBorder(3))) 
     &         loctab_child(n) = -3
C
          case('N') ! No space DIMENSION      
C
            select case (nbdim) 
C      
              case(1)
                nbtab_Child(n) = SIZE(child % var % array1,n) - 1
                pttab_Child(n) = lbound(child % var % array1,n)
              case(2)
                nbtab_Child(n) = SIZE(child % var % array2,n) - 1
                pttab_Child(n) = lbound(child % var % array2,n)
              case(3)
                nbtab_Child(n) = SIZE(child % var % array3,n) - 1
                pttab_Child(n) = lbound(child % var % array3,n)  
              case(4)
                nbtab_Child(n) = SIZE(child % var % array4,n) - 1
                pttab_Child(n) = lbound(child % var % array4,n)
              case(5)
                nbtab_Child(n) = SIZE(child % var % array5,n) - 1
                pttab_Child(n) = lbound(child % var % array5,n)      
              case(6)
                nbtab_Child(n) = SIZE(child % var % array6,n) - 1
                pttab_Child(n) = lbound(child % var % array6,n)      
C
            end select
C
C           No interpolation but only a copy of the values of the grid variable
C      
            posvartab_child(n) = 1
            pttab_Parent(n)= pttab_Child(n)
            s_Child(n)=0.
            s_Parent(n)=0. 
            ds_Child(n)=1.
            ds_Parent(n)=1.
            loctab_child(n) = -3
C
        End select
C
      enddo
C
         IF (present(procname)) THEN
          Call AGRIF_CorrectnD
     &         (TypeInterp,parent,child,deb,fin,pweight,weight,
     &          pttab_Child(1:nbdim),pttab_Parent(1:nbdim),
     &          nbtab_Child(1:nbdim),posvartab_Child(1:nbdim),
     &          loctab_Child(1:nbdim),
     &          s_Child(1:nbdim),s_Parent(1:nbdim),
     &          ds_Child(1:nbdim),ds_Parent(1:nbdim),nbdim,procname)
        ELSE
          Call AGRIF_CorrectnD
     &         (TypeInterp,parent,child,deb,fin,pweight,weight,
     &          pttab_Child(1:nbdim),pttab_Parent(1:nbdim),
     &          nbtab_Child(1:nbdim),posvartab_Child(1:nbdim),
     &          loctab_Child(1:nbdim),
     &          s_Child(1:nbdim),s_Parent(1:nbdim),
     &          ds_Child(1:nbdim),ds_Parent(1:nbdim),nbdim)
         ENDIF
C
C
      End subroutine AGRIF_CorrectVariable
C
C     **************************************************************************
CCC   Subroutine Agrif_Correctnd
C     **************************************************************************
C
      Subroutine AGRIF_Correctnd(TypeInterp,parent,child,deb,fin,
     &                           pweight,weight,
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
      External :: procname
      Optional :: procname
      INTEGER,DIMENSION(6) :: TypeInterp ! TYPE of interpolation (linear, 
                                         !   spline,...)  
      TYPE(AGRIF_PVariable)    :: parent ! Variable on the parent grid
      TYPE(AGRIF_PVariable)    :: child  ! Variable on the child grid 
      INTEGER                  :: deb,fin ! Positions where interpolations 
                                         !    are done
      LOGICAL                  :: pweight ! Indicates if weight is used for 
                                         !    the temporal interpolation
      REAL                     :: weight ! Coefficient for the temporal
                                         !    interpolation
      INTEGER                  :: nbdim  ! Number of dimensions of the grid
                                         !    variable
      INTEGER,DIMENSION(nbdim) :: pttab_child ! Index of the first point inside
                                         !    the domain for the parent 
                                         !    grid variable
      INTEGER,DIMENSION(nbdim) :: pttab_Parent ! Index of the first point 
                                         !   inside the domain for the 
                                         !   child grid variable
      INTEGER,DIMENSION(nbdim) :: nbtab_Child ! Number of cells of the child
                                         !    grid
      INTEGER,DIMENSION(nbdim) :: posvartab_Child ! Position of the grid
                                         !    variable (1 or 2)
      INTEGER,DIMENSION(nbdim) :: loctab_Child ! Indicates if the child 
                                        !    grid has a common border with 
                                        !    the root grid
      REAL   ,DIMENSION(nbdim) :: s_Child,s_Parent ! Positions of the parent 
                                        !   and child grids 
      REAL   ,DIMENSION(nbdim) :: ds_Child,ds_Parent ! Space steps of the 
                                        !    parent and child grids
C
C     Local variables
      TYPE(AGRIF_PVariable)        :: restore ! Variable on the parent     
      INTEGER,DIMENSION(nbdim,2)   :: lubglob
      INTEGER                      :: i          
      INTEGER                      :: kindex  ! Index used for safeguard 
                                       !    and time interpolation
      INTEGER,DIMENSION(nbdim,2,2) :: indtab ! Arrays indicating the limits 
                                       !    of the child     
      INTEGER,DIMENSION(nbdim,2,2) :: indtruetab ! grid variable where 
                                       !   boundary conditions are 
      INTEGER,DIMENSION(nbdim,2,2,nbdim)   :: ptres,ptres2 ! calculated
      INTEGER                      :: nb,ndir,n,sizetab
      REAL, DIMENSION(:), Allocatable :: tab ! Array used for the interpolation
      REAL    :: c1t,c2t               ! Coefficients for the time interpolation
                                       !    (c2t=1-c1t) 
C
C      
C
      indtab(1:nbdim,2,1) = pttab_child(1:nbdim) + nbtab_child(1:nbdim)
     &          + deb
      indtab(1:nbdim,2,2) = indtab(1:nbdim,2,1) + ( fin - deb )
       
      indtab(1:nbdim,1,1) = pttab_child(1:nbdim) - fin
      indtab(1:nbdim,1,2) = pttab_child(1:nbdim) - deb
                  
      WHERE (posvartab_child(1:nbdim) == 2)
        indtab(1:nbdim,1,1) = indtab(1:nbdim,1,1) - 1
        indtab(1:nbdim,1,2) = indtab(1:nbdim,1,2) - 1
      END WHERE
      

      Call Agrif_nbdim_Get_bound_dimension(child%var,lubglob(:,1),
     &              lubglob(:,2),nbdim)
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
          if (loctab_child(nb) /= (-ndir) 
     &        .AND. loctab_child(nb) /= -3) then
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
                    if (loctab_child(i) == -1 
     &                                 .OR. loctab_child(i) == -3) then
C
                        ptres(i,1,ndir,nb) = pttab_child(i)
C
                      else
C
                        ptres(i,1,ndir,nb) = indtruetab(i,1,1)
C
                    endif
C
                    if (loctab_child(i) == -2 
     &                                 .OR. loctab_child(i) == -3) then
C
                        if (posvartab_child(i) == 1) then
C
                            ptres(i,2,ndir,nb) = pttab_child(i)
     &                                                 + nbtab_child(i)
C
                          else
C
                            ptres(i,2,ndir,nb) = pttab_child(i)
     &                                             + nbtab_child(i) - 1
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
              ptres2(:,:,ndir,nb) = ptres(:,:,ndir,nb)
            
        endif
      
        enddo
       enddo
C
      if (child % var % interpIndex 
     &        /= Agrif_Curgrid % parent % ngridstep .OR.
     &    child%var%Interpolationshouldbemade ) then
C
C     Space interpolation 
C      
      kindex = 1    
C
      do nb = 1,nbdim
C
        do ndir = 1,2                
C
          if (loctab_child(nb) /= (-ndir) 
     &        .AND. loctab_child(nb) /= -3) then  
C
              IF (present(procname)) THEN
              Call Agrif_InterpnD
     &             (TYPEInterp,parent,child,
     &              ptres(1:nbdim,1,ndir,nb),ptres(1:nbdim,2,ndir,nb),
     &              pttab_child(1:nbdim),pttab_Parent(1:nbdim),
     &              s_Child(1:nbdim),s_Parent(1:nbdim),
     &              ds_Child(1:nbdim),ds_Parent(1:nbdim),
     &              restore,.FALSE.,nbdim,procname)
              ELSE
              Call Agrif_InterpnD              
     &             (TYPEInterp,parent,child,
     &              ptres(1:nbdim,1,ndir,nb),ptres(1:nbdim,2,ndir,nb),
     &              pttab_child(1:nbdim),pttab_Parent(1:nbdim),
     &              s_Child(1:nbdim),s_Parent(1:nbdim),
     &              ds_Child(1:nbdim),ds_Parent(1:nbdim),
     &              restore,.FALSE.,nbdim)
              ENDIF
     
              IF (.NOT. child%var%interpolationshouldbemade) THEN
C     
C             Safeguard of the values of the grid variable (at times n and n+1
C                on the parent grid)
C      
              sizetab = 1
C
              do i = 1,nbdim
C          
                sizetab = sizetab 
     &              * (ptres2(i,2,ndir,nb)-ptres2(i,1,ndir,nb)+1)
C      
              enddo
C              
              allocate(tab(sizetab))
C
             Call Agrif_vector2array(
     &                      tab,child%var,
     &                      ptres2(:,:,ndir,nb),
     &                      nbdim)

C     
              Call saveAfterInterp
     &             (child,tab,kindex)
C
C
              deallocate(tab)     
           ENDIF
C
          endif
C
        enddo       
C
      enddo     
C
C
      child % var % interpIndex = Agrif_Curgrid % parent % ngridstep
C
C
      endif  
      
              IF (.NOT. child%var%interpolationshouldbemade) THEN
C
C
C     Calculation of the coefficients c1t and c2t for the temporary
C        interpolation
      if (pweight) then
C
          c1t = weight
C
        else
C
          c1t = (REAL(AGRIF_Nbstepint()) + 1.) / Agrif_Rhot()
C
      endif
C                                  
      c2t = 1. - c1t
C           
C     Time interpolation
C
      kindex = 1
C
      do nb = 1,nbdim
C
        do ndir = 1,2      
C
          if (loctab_child(nb) /= (-ndir) 
     &        .AND. loctab_child(nb) /= -3) then
C      
              sizetab=1   
C
              do i = 1,nbdim
C          
                sizetab = sizetab
     &             * (ptres2(i,2,ndir,nb)-ptres2(i,1,ndir,nb)+1)
C      
              enddo
C              
              allocate(tab(sizetab))
C
             Call Agrif_vector2array(
     &                      tab,child%var,
     &                      ptres2(:,:,ndir,nb),
     &                      nbdim)
C    
C                  
              Call timeInterpolation
     &             (child,tab,kindex,c1t,c2t)           
C
C
          Call Agrif_array2vector(
     &                   child%var,
     &                   ptres2(:,:,ndir,nb),
     &                   tab,nbdim)
C
C         
              deallocate(tab)
C 
          endif
C
        enddo
C      
      enddo
C      

       ENDIF
C  
      End Subroutine Agrif_Correctnd
C
C
C     **************************************************************************
CCC   Subroutine saveAfterInterp
C     **************************************************************************
C
      Subroutine saveAfterInterp(child,tab,kindex)
C
CCC   Descritpion:
CCC   Subroutine used to save the values of the grid variable on the fine grid 
CCC   after the space interpolation. 
C
C     Declarations:
C
      
C
C     argument
      TYPE (AGRIF_PVariable) :: child   ! The fine grid variable
      REAL, DIMENSION(:)     :: tab     ! Values on the fine grid variable 
                                        !   after the space interpolation  
      INTEGER                :: kindex  ! Index indicating where this safeguard 
                                        ! is done on the fine grid
C
C     Local scalars
      INTEGER :: newsize                ! Size of the domain where boundary
                                        !    conditions are calculated
      INTEGER :: i
C
C
C     Allocation of the array oldvalues2d
      newsize = size(tab)
C
      if (newsize .LE. 0) return
C
      Call checkSize
     &     (child,kindex+newsize)  
C
C
C     Safeguard in the oldvalues2d array       
C            
      if (child % var % interpIndex 
     &        /= Agrif_Curgrid % parent % ngridstep ) then
         do i = 1,newsize
           child % var % oldvalues2d(kindex,1) = child % var %
     &                                        oldvalues2d(kindex,2) 
           child % var % oldvalues2d(kindex,2) = tab(i)
           kindex = kindex + 1
         enddo
      else
        do i = 1,newsize
           child % var % oldvalues2d(kindex,2) = tab(i)
           kindex = kindex + 1
         enddo
      endif
     

C
C                                                  
      End subroutine saveAfterInterp
C
C
C
C     **************************************************************************
CCC   Subroutine timeInterpolation
C     **************************************************************************
C
      Subroutine timeInterpolation(child,tab,kindex,c1t,c2t) 
C
CCC   Descritpion:
CCC   Subroutine for a linear time interpolation on the child grid. 
C
C     Declarations:
C
      
C
C     argument
      TYPE (AGRIF_PVariable) :: child  ! The fine grid variable
      REAL, DIMENSION(:)     :: tab
      INTEGER                :: kindex ! Index indicating the values of the fine
                                       ! grid got before and after the space 
                                       ! interpolation and used for the time 
                                       ! interpolation
      REAL                   :: c1t,c2t! coefficients for the time interpolation
                                       ! (c2t=1-c1t)  
C
C     Local aruments      
      INTEGER :: i
C
C
      do i = 1,size(tab)
        tab(i) = c2t*child % var % oldvalues2d(kindex,1)   
     &         + c1t*child % var % oldvalues2d(kindex,2)        
        kindex = kindex + 1        
      enddo                                             
C
C
      End subroutine timeInterpolation      
C
C
C
C     **************************************************************************
CCC   Subroutine checkSize
C     **************************************************************************
C
      Subroutine checkSize(child,newsize)
C
CCC   Descritpion:
CCC   Subroutine used in the saveAfterInterp procedure to allocate the 
CCC   oldvalues2d array. 
C
C     Declarations:
C
      
C      
C     TYPE argument
      TYPE (AGRIF_PVariable) :: child  ! The fine grid variable
C 
C     Scalar arguments
      INTEGER :: newsize               ! Size of the domains where the boundary 
                                       ! conditions are calculated  
C
C     Local arrays
      REAL, DIMENSION(:,:), Allocatable :: tempoldvalues ! Temporary array
C                
C
      if (.NOT. associated(child % var % oldvalues2d)) then
C
          allocate(child % var % oldvalues2d(newsize,2))
C  
          child % var % oldvalues2d=0.
C
        else
C
          if (SIZE(child % var % oldvalues2d,1) < newsize) then   
C
              allocate(tempoldvalues(SIZE(child % var %
     &                                    oldvalues2d,1),2))
C
              tempoldvalues = child % var % oldvalues2d
C
              deallocate(child % var % oldvalues2d)
C
              allocate(child % var % oldvalues2d(newsize,2))
C            
              child%var%oldvalues2d=0.
C
              child % var % oldvalues2d(1:SIZE(tempoldvalues,1),:) = 
     &        tempoldvalues(:,:)
C
              deallocate(tempoldvalues)
C
          endif 
C
      endif                                                    
C
C
      End  Subroutine checkSize
C
C
C
C      
      End Module AGRIF_boundary

