!
! $Id: modbc.F 779 2007-12-22 17:04:17Z rblod $
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
!> Module Agrif_Boundary.
!>
!> Contains subroutines to calculate the boundary conditions on the child grids from their
!> parent grids.
!
module Agrif_Boundary
!
    use Agrif_Interpolation
!
    implicit none
!
contains
!
!===================================================================================================
!  subroutine Agrif_Interp_bc_1d
!
!> calculates the boundary conditions on a fine grid for a 1D grid variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_bc_1d ( TypeInterp, parent, child, tab, deb, fin, &
                                weight, pweight, procname )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(6,6) :: TypeInterp   !< Type of interpolation (linear, lagrange, spline, ... )
    TYPE(Agrif_PVariable)   :: parent       !< Variable on the parent grid
    TYPE(Agrif_PVariable)   :: child        !< Variable on the child grid
    REAL, DIMENSION(                    &
        child%var%lb(1):child%var%ub(1) &
    ), target               :: tab          !< Values of the grid variable
    INTEGER :: deb,fin                      !< Positions where interpolations are done on the fine grid
    External :: procname
    Optional :: procname
    
    TYPE(Agrif_PVariable) :: childtemp      ! Temporary variable on the child grid
    LOGICAL :: pweight                      ! Indicates if weight is used for the temporal interpolation
    REAL :: weight                          ! Coefficient for the time
                                            ! interpolation
!
!   Definition of a temporary Agrif_PVariable data type representing the grid  variable.
!
    allocate(childtemp % var)
!
    childtemp % var % root_var => child % var % root_var
!
!   Values of the grid variable
    childtemp % var % parray1 => tab
!
!   Temporary results for the time interpolation before and after the space interpolation
    childtemp % var % oldvalues2D => child % var % oldvalues2D
!
!   Index indicating if a space interpolation is necessary
    childtemp % var % interpIndex => child % var % interpIndex
    childtemp % var % list_interp => child % var % list_interp
    childtemp % var % Interpolationshouldbemade = child % var % Interpolationshouldbemade

    childtemp % var % lb = child % var % lb
    childtemp % var % ub = child % var % ub
!
!   Call to the procedure for the calculations of the boundary conditions
    IF (present(procname)) THEN
        Call Agrif_CorrectVariable(TypeInterp,parent,childtemp,deb,fin,pweight,weight,procname)
    ELSE
        Call Agrif_CorrectVariable(TypeInterp,parent,childtemp,deb,fin,pweight,weight)
    ENDIF
!
    child % var % oldvalues2D => childtemp % var % oldvalues2D
    child % var % list_interp => childtemp % var % list_interp
!
    deallocate(childtemp % var)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_bc_1D
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_bc_2d
!
!> calculates the boundary conditions on a fine grid for a 2D grid variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_bc_2d ( TypeInterp, parent, child, tab, deb, fin, &
                                weight, pweight, procname )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(6,6) :: TypeInterp   !< Type of interpolation (linear, lagrange, spline, ... )
    TYPE(Agrif_PVariable)   :: parent       !< Variable on the parent grid
    TYPE(Agrif_PVariable)   :: child        !< Variable on the child grid
    REAL, DIMENSION(                    &
        child%var%lb(1):child%var%ub(1),&
        child%var%lb(2):child%var%ub(2) &
    ), target               :: tab          !< Values of the grid variable
    INTEGER :: deb,fin                      !< Positions where interpolations are done on the fine grid
    External :: procname
    Optional :: procname
    
    TYPE(Agrif_PVariable) :: childtemp      ! Temporary variable on the child grid
    LOGICAL :: pweight                      ! Indicates if weight is used for the temporal interpolation
    REAL :: weight                          ! Coefficient for the time
                                            ! interpolation
!
!   Definition of a temporary Agrif_PVariable data type representing the grid  variable.
!
    allocate(childtemp % var)
!
    childtemp % var % root_var => child % var % root_var
!
!   Values of the grid variable
    childtemp % var % parray2 => tab
!
!   Temporary results for the time interpolation before and after the space interpolation
    childtemp % var % oldvalues2D => child % var % oldvalues2D
!
!   Index indicating if a space interpolation is necessary
    childtemp % var % interpIndex => child % var % interpIndex
    childtemp % var % list_interp => child % var % list_interp
    childtemp % var % Interpolationshouldbemade = child % var % Interpolationshouldbemade

    childtemp % var % lb = child % var % lb
    childtemp % var % ub = child % var % ub
!
!   Call to the procedure for the calculations of the boundary conditions
    IF (present(procname)) THEN
        Call Agrif_CorrectVariable(TypeInterp,parent,childtemp,deb,fin,pweight,weight,procname)
    ELSE
        Call Agrif_CorrectVariable(TypeInterp,parent,childtemp,deb,fin,pweight,weight)
    ENDIF
!
    child % var % oldvalues2D => childtemp % var % oldvalues2D
    child % var % list_interp => childtemp % var % list_interp
!
    deallocate(childtemp % var)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_bc_2D
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_bc_3d
!
!> calculates the boundary conditions on a fine grid for a 3D grid variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_bc_3d ( TypeInterp, parent, child, tab, deb, fin, &
                                weight, pweight, procname )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(6,6) :: TypeInterp   !< Type of interpolation (linear, lagrange, spline, ... )
    TYPE(Agrif_PVariable)   :: parent       !< Variable on the parent grid
    TYPE(Agrif_PVariable)   :: child        !< Variable on the child grid
    REAL, DIMENSION(                    &
        child%var%lb(1):child%var%ub(1),&
        child%var%lb(2):child%var%ub(2),&
        child%var%lb(3):child%var%ub(3) &
    ), target               :: tab          !< Values of the grid variable
    INTEGER :: deb,fin                      !< Positions where interpolations are done on the fine grid
    External :: procname
    Optional :: procname
    
    TYPE(Agrif_PVariable) :: childtemp      ! Temporary variable on the child grid
    LOGICAL :: pweight                      ! Indicates if weight is used for the temporal interpolation
    REAL :: weight                          ! Coefficient for the time
                                            ! interpolation
!
!   Definition of a temporary Agrif_PVariable data type representing the grid  variable.
!
    allocate(childtemp % var)
!
    childtemp % var % root_var => child % var % root_var
!
!   Values of the grid variable
    childtemp % var % parray3 => tab
!
!   Temporary results for the time interpolation before and after the space interpolation
    childtemp % var % oldvalues2D => child % var % oldvalues2D
!
!   Index indicating if a space interpolation is necessary
    childtemp % var % interpIndex => child % var % interpIndex
    childtemp % var % list_interp => child % var % list_interp
    childtemp % var % Interpolationshouldbemade = child % var % Interpolationshouldbemade

    childtemp % var % lb = child % var % lb
    childtemp % var % ub = child % var % ub
!
!   Call to the procedure for the calculations of the boundary conditions
    IF (present(procname)) THEN
        Call Agrif_CorrectVariable(TypeInterp,parent,childtemp,deb,fin,pweight,weight,procname)
    ELSE
        Call Agrif_CorrectVariable(TypeInterp,parent,childtemp,deb,fin,pweight,weight)
    ENDIF
!
    child % var % oldvalues2D => childtemp % var % oldvalues2D
    child % var % list_interp => childtemp % var % list_interp
!
    deallocate(childtemp % var)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_bc_3D
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_bc_4d
!
!> calculates the boundary conditions on a fine grid for a 4D grid variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_bc_4d ( TypeInterp, parent, child, tab, deb, fin, &
                                weight, pweight, procname )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(6,6) :: TypeInterp   !< Type of interpolation (linear, lagrange, spline, ... )
    TYPE(Agrif_PVariable)   :: parent       !< Variable on the parent grid
    TYPE(Agrif_PVariable)   :: child        !< Variable on the child grid
    REAL, DIMENSION(                    &
        child%var%lb(1):child%var%ub(1),&
        child%var%lb(2):child%var%ub(2),&
        child%var%lb(3):child%var%ub(3),&
        child%var%lb(4):child%var%ub(4) &
    ), target               :: tab          !< Values of the grid variable
    INTEGER :: deb,fin                      !< Positions where interpolations are done on the fine grid
    External :: procname
    Optional :: procname
    
    TYPE(Agrif_PVariable) :: childtemp      ! Temporary variable on the child grid
    LOGICAL :: pweight                      ! Indicates if weight is used for the temporal interpolation
    REAL :: weight                          ! Coefficient for the time
                                            ! interpolation
!
!   Definition of a temporary Agrif_PVariable data type representing the grid  variable.
!
    allocate(childtemp % var)
!
    childtemp % var % root_var => child % var % root_var
!
!   Values of the grid variable
    childtemp % var % parray4 => tab
!
!   Temporary results for the time interpolation before and after the space interpolation
    childtemp % var % oldvalues2D => child % var % oldvalues2D
!
!   Index indicating if a space interpolation is necessary
    childtemp % var % interpIndex => child % var % interpIndex
    childtemp % var % list_interp => child % var % list_interp
    childtemp % var % Interpolationshouldbemade = child % var % Interpolationshouldbemade

    childtemp % var % lb = child % var % lb
    childtemp % var % ub = child % var % ub
!
!   Call to the procedure for the calculations of the boundary conditions
    IF (present(procname)) THEN
        Call Agrif_CorrectVariable(TypeInterp,parent,childtemp,deb,fin,pweight,weight,procname)
    ELSE
        Call Agrif_CorrectVariable(TypeInterp,parent,childtemp,deb,fin,pweight,weight)
    ENDIF
!
    child % var % oldvalues2D => childtemp % var % oldvalues2D
    child % var % list_interp => childtemp % var % list_interp
!
    deallocate(childtemp % var)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_bc_4D
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_bc_5d
!
!> calculates the boundary conditions on a fine grid for a 5D grid variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_bc_5d ( TypeInterp, parent, child, tab, deb, fin, &
                                weight, pweight, procname )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(6,6) :: TypeInterp   !< Type of interpolation (linear, lagrange, spline, ... )
    TYPE(Agrif_PVariable)   :: parent       !< Variable on the parent grid
    TYPE(Agrif_PVariable)   :: child        !< Variable on the child grid
    REAL, DIMENSION(                    &
        child%var%lb(1):child%var%ub(1),&
        child%var%lb(2):child%var%ub(2),&
        child%var%lb(3):child%var%ub(3),&
        child%var%lb(4):child%var%ub(4),&
        child%var%lb(5):child%var%ub(5) &
    ), target               :: tab          !< Values of the grid variable
    INTEGER :: deb,fin                      !< Positions where interpolations are done on the fine grid
    External :: procname
    Optional :: procname
    
    TYPE(Agrif_PVariable) :: childtemp      ! Temporary variable on the child grid
    LOGICAL :: pweight                      ! Indicates if weight is used for the temporal interpolation
    REAL :: weight                          ! Coefficient for the time
                                            ! interpolation
!
!   Definition of a temporary Agrif_PVariable data type representing the grid  variable.
!
    allocate(childtemp % var)
!
    childtemp % var % root_var => child % var % root_var
!
!   Values of the grid variable
    childtemp % var % parray5 => tab
!
!   Temporary results for the time interpolation before and after the space interpolation
    childtemp % var % oldvalues2D => child % var % oldvalues2D
!
!   Index indicating if a space interpolation is necessary
    childtemp % var % interpIndex => child % var % interpIndex
    childtemp % var % list_interp => child % var % list_interp
    childtemp % var % Interpolationshouldbemade = child % var % Interpolationshouldbemade

    childtemp % var % lb = child % var % lb
    childtemp % var % ub = child % var % ub
!
!   Call to the procedure for the calculations of the boundary conditions
    IF (present(procname)) THEN
        Call Agrif_CorrectVariable(TypeInterp,parent,childtemp,deb,fin,pweight,weight,procname)
    ELSE
        Call Agrif_CorrectVariable(TypeInterp,parent,childtemp,deb,fin,pweight,weight)
    ENDIF
!
    child % var % oldvalues2D => childtemp % var % oldvalues2D
    child % var % list_interp => childtemp % var % list_interp
!
    deallocate(childtemp % var)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_bc_5D
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_bc_6d
!
!> calculates the boundary conditions on a fine grid for a 6D grid variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_bc_6d ( TypeInterp, parent, child, tab, deb, fin, &
                                weight, pweight, procname )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(6,6) :: TypeInterp   !< Type of interpolation (linear, lagrange, spline, ... )
    TYPE(Agrif_PVariable)   :: parent       !< Variable on the parent grid
    TYPE(Agrif_PVariable)   :: child        !< Variable on the child grid
    REAL, DIMENSION(                    &
        child%var%lb(1):child%var%ub(1),&
        child%var%lb(2):child%var%ub(2),&
        child%var%lb(3):child%var%ub(3),&
        child%var%lb(4):child%var%ub(4),&
        child%var%lb(5):child%var%ub(5),&
        child%var%lb(6):child%var%ub(6) &
    ), target               :: tab          !< Values of the grid variable
    INTEGER :: deb,fin                      !< Positions where interpolations are done on the fine grid
    External :: procname
    Optional :: procname
    
    TYPE(Agrif_PVariable) :: childtemp      ! Temporary variable on the child grid
    LOGICAL :: pweight                      ! Indicates if weight is used for the temporal interpolation
    REAL :: weight                          ! Coefficient for the time
                                            ! interpolation
!
!   Definition of a temporary Agrif_PVariable data type representing the grid  variable.
!
    allocate(childtemp % var)
!
    childtemp % var % root_var => child % var % root_var
!
!   Values of the grid variable
    childtemp % var % parray6 => tab
!
!   Temporary results for the time interpolation before and after the space interpolation
    childtemp % var % oldvalues2D => child % var % oldvalues2D
!
!   Index indicating if a space interpolation is necessary
    childtemp % var % interpIndex => child % var % interpIndex
    childtemp % var % list_interp => child % var % list_interp
    childtemp % var % Interpolationshouldbemade = child % var % Interpolationshouldbemade

    childtemp % var % lb = child % var % lb
    childtemp % var % ub = child % var % ub
!
!   Call to the procedure for the calculations of the boundary conditions
    IF (present(procname)) THEN
        Call Agrif_CorrectVariable(TypeInterp,parent,childtemp,deb,fin,pweight,weight,procname)
    ELSE
        Call Agrif_CorrectVariable(TypeInterp,parent,childtemp,deb,fin,pweight,weight)
    ENDIF
!
    child % var % oldvalues2D => childtemp % var % oldvalues2D
    child % var % list_interp => childtemp % var % list_interp
!
    deallocate(childtemp % var)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_bc_6D
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_CorrectVariable
!
!> subroutine to calculate the boundary conditions on a fine grid
!---------------------------------------------------------------------------------------------------
subroutine Agrif_CorrectVariable ( TypeInterp, parent, child, deb, fin, &
                                   pweight, weight, procname )
!---------------------------------------------------------------------------------------------------
    INTEGER,DIMENSION(6,6), intent(in)  :: TypeInterp     !< Type of interpolation (linear,lagrange,...)
    TYPE(Agrif_PVariable)   :: parent         !< Variable on the parent grid
    TYPE(Agrif_PVariable)   :: child          !< Variable on the child grid
    INTEGER                 :: deb,fin        !< Positions where boundary conditions are calculated
    LOGICAL                 :: pweight        !< Indicates if weight is used for the time interpolation
    REAL                    :: weight         !< Coefficient for the time interpolation
    External :: procname
    Optional :: procname
!
    TYPE(Agrif_Grid)    , pointer :: Agrif_Child_Gr, Agrif_Parent_Gr
    TYPE(Agrif_Variable), pointer :: root   ! Variable on the root grid
    INTEGER                :: nbdim  ! Number of dimensions of the grid variable
    INTEGER                :: n
    INTEGER,DIMENSION(6)   :: pttab_child       ! Index of the first point inside the domain for
                                                !    the child grid variable
    INTEGER,DIMENSION(6)   :: pttab_parent      ! Index of the first point inside the domain for
                                                !    the parent grid variable
    INTEGER,DIMENSION(6)   :: nbtab_Child       ! Number of cells for child
    INTEGER,DIMENSION(6)   :: posvartab_Child   ! Position of the variable on the cell
    INTEGER,DIMENSION(6)   :: loctab_Child      ! Indicates if the child grid has a common border
                                                !    with the root grid
    REAL, DIMENSION(6)     :: s_child, s_parent   ! Positions of the parent and child grids
    REAL, DIMENSION(6)     :: ds_child, ds_parent ! Space steps of the parent and child grids
!
    loctab_child(:) = 0
!
    Agrif_Child_Gr => Agrif_Curgrid
    Agrif_Parent_Gr => Agrif_Curgrid % parent
    root => child % var % root_var
    nbdim = root % nbdim
!
    do n = 1,nbdim
        posvartab_child(n) = root % posvar(n)
    enddo
!
    do n = 1,nbdim
!
        select case(root % interptab(n))
!
        case('x') ! x DIMENSION
!
            nbtab_Child(n) = Agrif_Child_Gr % nb(1)
            pttab_Child(n) = root % point(1)
            pttab_Parent(n) = root % point(1)
            s_Child(n) = Agrif_Child_Gr % Agrif_x(1)
            s_Parent(n) = Agrif_Parent_Gr % Agrif_x(1)
            ds_Child(n) = Agrif_Child_Gr % Agrif_d(1)
            ds_Parent(n) = Agrif_Parent_Gr % Agrif_d(1)
            if (root % posvar(n) == 2) then
                s_Child(n)  = s_Child(n)  + ds_Child(n)/2.
                s_Parent(n) = s_Parent(n) + ds_Parent(n)/2.
            endif
            if (Agrif_Curgrid % NearRootBorder(1))      loctab_child(n) = -1
            if (Agrif_Curgrid % DistantRootBorder(1))   loctab_child(n) = -2
            if ((Agrif_Curgrid % NearRootBorder(1)) .AND. &
                (Agrif_Curgrid % DistantRootBorder(1))) loctab_child(n) = -3
!
        case('y') ! y DIMENSION
!
            nbtab_Child(n) = Agrif_Child_Gr % nb(2)
            pttab_Child(n) = root % point(2)
            pttab_Parent(n) = root % point(2)
            s_Child(n) = Agrif_Child_Gr % Agrif_x(2)
            s_Parent(n) = Agrif_Parent_Gr % Agrif_x(2)
            ds_Child(n) = Agrif_Child_Gr % Agrif_d(2)
            ds_Parent(n) = Agrif_Parent_Gr % Agrif_d(2)
            if (root % posvar(n) == 2) then
                s_Child(n)  = s_Child(n) + ds_Child(n)/2.
                s_Parent(n) = s_Parent(n) + ds_Parent(n)/2.
            endif
!
            if (Agrif_Curgrid % NearRootBorder(2))      loctab_child(n) = -1
            if (Agrif_Curgrid % DistantRootBorder(2))   loctab_child(n) = -2
            if ((Agrif_Curgrid % NearRootBorder(2)) .AND. &
                (Agrif_Curgrid % DistantRootBorder(2))) loctab_child(n) = -3
!
        case('z') ! z DIMENSION
!
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
!
            if (Agrif_Curgrid % NearRootBorder(3))      loctab_child(n) = -1
            if (Agrif_Curgrid % DistantRootBorder(3))   loctab_child(n) = -2
            if ((Agrif_Curgrid % NearRootBorder(3)) .AND. &
                (Agrif_Curgrid % DistantRootBorder(3))) loctab_child(n) = -3
!
        case('N') ! No space DIMENSION
!
            nbtab_Child(n) = parent % var % ub(n) - parent % var % lb(n)
            pttab_Child(n) = parent % var % lb(n)
!
!           No interpolation but only a copy of the values of the grid variable
            posvartab_child(n) = 1
            pttab_Parent(n) = pttab_Child(n)
            s_Child(n) = 0.
            s_Parent(n) = 0.
            ds_Child(n) = 1.
            ds_Parent(n) = 1.
            loctab_child(n) = -3
!
        end select
!
    enddo
!
    IF (present(procname)) THEN
        Call Agrif_Correctnd(TypeInterp,parent,child,deb,fin,pweight,weight,    &
                             pttab_Child(1:nbdim), pttab_Parent(1:nbdim),       &
                             nbtab_Child(1:nbdim), posvartab_Child(1:nbdim),    &
                             loctab_Child(1:nbdim),                             &
                             s_Child(1:nbdim), s_Parent(1:nbdim),               &
                             ds_Child(1:nbdim),ds_Parent(1:nbdim), nbdim, procname )
    ELSE
        Call Agrif_Correctnd(TypeInterp,parent,child,deb,fin,pweight,weight,    &
                             pttab_Child(1:nbdim), pttab_Parent(1:nbdim),       &
                             nbtab_Child(1:nbdim), posvartab_Child(1:nbdim),    &
                             loctab_Child(1:nbdim),                             &
                             s_Child(1:nbdim), s_Parent(1:nbdim),               &
                             ds_Child(1:nbdim),ds_Parent(1:nbdim), nbdim )
    ENDIF
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_CorrectVariable
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Correctnd
!
!> calculates the boundary conditions for a nD grid variable on a fine grid by using
!> a space and time interpolations; it is called by the #Agrif_CorrectVariable procedure
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Correctnd ( TypeInterp, parent, child, deb, fin, pweight, weight,  &
                             pttab_child, pttab_Parent,                             &
                             nbtab_Child, posvartab_Child, loctab_Child,            &
                             s_Child, s_Parent, ds_Child, ds_Parent,                &
                             nbdim, procname )
!---------------------------------------------------------------------------------------------------
#if defined AGRIF_MPI
    include 'mpif.h'
#endif
!
    INTEGER, DIMENSION(6,6), intent(in) :: TypeInterp   !< Type of interpolation (linear, spline,...)
    TYPE(Agrif_PVariable)       :: parent           !< Variable on the parent grid
    TYPE(Agrif_PVariable)       :: child            !< Variable on the child grid
    INTEGER                     :: deb, fin         !< Positions where interpolations are done
    LOGICAL                     :: pweight          !< Indicates if weight is used for the temporal
                                                    !<    interpolation
    REAL                        :: weight           !< Coefficient for the temporal interpolation
    INTEGER, DIMENSION(nbdim)   :: pttab_child      !< Index of the first point inside the domain
                                                    !<     for the parent grid variable
    INTEGER, DIMENSION(nbdim)   :: pttab_Parent     !< Index of the first point inside the domain
                                                    !<    for the child grid variable
    INTEGER, DIMENSION(nbdim)   :: nbtab_Child      !< Number of cells of the child grid
    INTEGER, DIMENSION(nbdim)   :: posvartab_Child  !< Position of the grid variable (1 or 2)
    INTEGER, DIMENSION(nbdim)   :: loctab_Child     !< Indicates if the child grid has a common
                                                    !<    border with the root grid
    REAL   , DIMENSION(nbdim)   :: s_Child,  s_Parent   !< Positions of the parent and child grids
    REAL   , DIMENSION(nbdim)   :: ds_Child, ds_Parent  !< Space steps of the parent and child grids
    INTEGER                     :: nbdim            !< Number of dimensions of the grid variable
    External :: procname
    Optional :: procname
!
    TYPE(Agrif_PVariable)               :: restore      ! Variable on the parent
    INTEGER,DIMENSION(nbdim,2)          :: lubglob
    INTEGER                             :: kindex       ! Index used for safeguard and time interpolation
    INTEGER,DIMENSION(nbdim,2,2)        :: indtab       ! Arrays indicating the limits of the child
    INTEGER,DIMENSION(nbdim,2,2)        :: indtruetab   ! grid variable where boundary conditions are
    INTEGER,DIMENSION(nbdim,2,2,nbdim)  :: ptres,ptres2 ! calculated
    INTEGER                             :: i, nb,ndir,n,sizetab(1)
    REAL, DIMENSION(:), Allocatable     :: tab          ! Array used for the interpolation
    REAL                                :: c1t,c2t      ! Coefficients for the time interpolation (c2t=1-c1t)
!
#if defined AGRIF_MPI
!
    INTEGER, DIMENSION(nbdim)   :: lower, upper
    INTEGER, DIMENSION(nbdim)   :: ltab, utab
    INTEGER, DIMENSION(nbdim)   :: lb, ub
    INTEGER, DIMENSION(nbdim,2) :: iminmaxg
    INTEGER                     :: code
!
#endif
!
    indtab(1:nbdim,2,1) = pttab_child(1:nbdim) + nbtab_child(1:nbdim) + deb
    indtab(1:nbdim,2,2) = indtab(1:nbdim,2,1) + ( fin - deb )

    indtab(1:nbdim,1,1) = pttab_child(1:nbdim) - fin
    indtab(1:nbdim,1,2) = pttab_child(1:nbdim) - deb

    WHERE (posvartab_child(1:nbdim) == 2)
        indtab(1:nbdim,1,1) = indtab(1:nbdim,1,1) - 1
        indtab(1:nbdim,1,2) = indtab(1:nbdim,1,2) - 1
    END WHERE

#if !defined AGRIF_MPI
    Call Agrif_nbdim_Get_bound_dimension(child%var,lubglob(:,1), lubglob(:,2),nbdim)
#else
    Call Agrif_nbdim_Get_bound_dimension(child%var,lb,ub,nbdim)

    DO i = 1,nbdim
        Call Agrif_Invloc(lb(i),Agrif_Procrank,i,iminmaxg(i,1))
        Call Agrif_Invloc(ub(i),Agrif_Procrank,i,iminmaxg(i,2))
    ENDDO
!
    iminmaxg(1:nbdim,2) = - iminmaxg(1:nbdim,2)

    CALL MPI_ALLREDUCE(iminmaxg,lubglob,2*nbdim,MPI_INTEGER,MPI_MIN,MPI_COMM_WORLD,code)

    lubglob(1:nbdim,2) = - lubglob(1:nbdim,2)
#endif
!
    indtruetab(1:nbdim,1,1) = max(indtab(1:nbdim,1,1), lubglob(1:nbdim,1))
    indtruetab(1:nbdim,1,2) = max(indtab(1:nbdim,1,2), lubglob(1:nbdim,1))
    indtruetab(1:nbdim,2,1) = min(indtab(1:nbdim,2,1), lubglob(1:nbdim,2))
    indtruetab(1:nbdim,2,2) = min(indtab(1:nbdim,2,2), lubglob(1:nbdim,2))
!
    do nb = 1,nbdim
        do ndir = 1,2
!
            if (loctab_child(nb) /= (-ndir) .AND. loctab_child(nb) /= -3) then
!
                do n = 1,2
                    ptres(nb,n,ndir,nb) = indtruetab(nb,ndir,n)
                enddo
!
                do i = 1,nbdim
!
                    if (i /= nb) then
!
                        if (loctab_child(i) == -1 .OR. loctab_child(i) == -3) then
                            ptres(i,1,ndir,nb) = pttab_child(i)
                        else
                            ptres(i,1,ndir,nb) = indtruetab(i,1,1)
                        endif
                        if (loctab_child(i) == -2 .OR. loctab_child(i) == -3) then
                            if (posvartab_child(i) == 1) then
                                ptres(i,2,ndir,nb) = pttab_child(i) + nbtab_child(i)
                            else
                                ptres(i,2,ndir,nb) = pttab_child(i) + nbtab_child(i) - 1
                            endif
                        else
                            ptres(i,2,ndir,nb) = indtruetab(i,2,2)
                        endif
!
                    endif
!
                enddo

!
#if defined AGRIF_MPI
                Call Agrif_nbdim_Get_bound_dimension(child%var,lower,upper,nbdim)

                do i = 1,nbdim
!
                    Call GetLocalBoundaries(ptres(i,1,ndir,nb), ptres(i,2,ndir,nb), i,  &
                                            lower(i), upper(i), ltab(i), utab(i) )
                    ptres2(i,1,ndir,nb) = max(ltab(i),lower(i))
                    ptres2(i,2,ndir,nb) = min(utab(i),upper(i))
                    if ((i == nb) .AND. (ndir == 1)) then
                        ptres2(i,2,ndir,nb) = max(utab(i),lower(i))
                    elseif ((i == nb) .AND. (ndir == 2)) then
                        ptres2(i,1,ndir,nb) = min(ltab(i),upper(i))
                    endif
!
                enddo
#else
                ptres2(:,:,ndir,nb) = ptres(:,:,ndir,nb)
#endif
            endif
!
        enddo   ! ndir = 1,2
    enddo       ! nb = 1,nbdim
!
    if ( child % var % interpIndex /= Agrif_Curgrid % parent % ngridstep .OR. &
         child % var % Interpolationshouldbemade ) then
!
!     Space interpolation
!
        kindex = 1
!
        do nb = 1,nbdim
            do ndir = 1,2
!
                if (loctab_child(nb) /= (-ndir) .AND. loctab_child(nb) /= -3) then
!
                    IF (present(procname)) THEN
                        Call Agrif_InterpnD(TYPEInterp(nb,:), parent, child,        &
                                            ptres(1:nbdim,1,ndir,nb),               &
                                            ptres(1:nbdim,2,ndir,nb),               &
                                            pttab_child(1:nbdim),                   &
                                            pttab_Parent(1:nbdim),                  &
                                            s_Child(1:nbdim), s_Parent(1:nbdim),    &
                                            ds_Child(1:nbdim),ds_Parent(1:nbdim),   &
                                            restore, .FALSE., nbdim, procname )
                    ELSE
                        Call Agrif_InterpnD(TYPEInterp(nb,:), parent, child,        &
                                            ptres(1:nbdim,1,ndir,nb),               &
                                            ptres(1:nbdim,2,ndir,nb),               &
                                            pttab_child(1:nbdim),                   &
                                            pttab_Parent(1:nbdim),                  &
                                            s_Child(1:nbdim), s_Parent(1:nbdim),    &
                                            ds_Child(1:nbdim),ds_Parent(1:nbdim),   &
                                            restore, .FALSE., nbdim )
                    ENDIF

                    IF (.NOT. child%var%interpolationshouldbemade) THEN
!
!                       Safeguard of the values of the grid variable (at times n and n+1 on the
!                       parent grid)
!
                        sizetab(1) = 1
                        do i = 1,nbdim
                            sizetab(1) = sizetab(1) * (ptres2(i,2,ndir,nb)-ptres2(i,1,ndir,nb)+1)
                        enddo

                        Call saveAfterInterp(child%var,ptres2(:,:,ndir,nb),kindex,sizetab(1),nbdim)
!
                    endif
!
                endif
!
            enddo   ! ndir = 1,2
        enddo       ! nb = 1,nbdim
!
        child % var % interpIndex = Agrif_Curgrid % parent % ngridstep
!
    endif

    IF (.NOT. child%var%interpolationshouldbemade) THEN
!
!       Calculation of the coefficients c1t and c2t for the temporary interpolation
!
        if (pweight) then
            c1t = weight
        else
            c1t = (REAL(Agrif_Nbstepint()) + 1.) / Agrif_Rhot()
        endif
        c2t = 1. - c1t
!
!       Time interpolation
!
        kindex = 1
!
        do nb = 1,nbdim
            do ndir = 1,2
                if (loctab_child(nb) /= (-ndir) .AND. loctab_child(nb) /= -3) then
                    Call timeInterpolation(child%var,ptres2(:,:,ndir,nb),kindex,c1t,c2t,nbdim)
                endif
            enddo
        enddo
!
    ENDIF
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Correctnd
!===================================================================================================
!
!===================================================================================================
!  subroutine saveAfterInterp
!
!> saves the values of the grid variable on the fine grid after the space interpolation
!---------------------------------------------------------------------------------------------------
subroutine saveAfterInterp ( child_var, bounds, kindex, newsize, nbdim )
!---------------------------------------------------------------------------------------------------
    TYPE (Agrif_Variable),       INTENT(inout)  :: child_var !< The fine grid variable
    INTEGER, DIMENSION(nbdim,2), INTENT(in)     :: bounds
    INTEGER,                     INTENT(inout)  :: kindex    !< Index indicating where this safeguard
                                                             !<     is done on the fine grid
    INTEGER,                     INTENT(in)     :: newsize
    INTEGER,                     INTENT(in)     :: nbdim
!
    INTEGER     :: ir,jr,kr,lr,mr,nr
!
!    Allocation of the array oldvalues2d
!
    if (newsize .LE. 0) return
!
    Call Agrif_Checksize(child_var,kindex+newsize)

    if (child_var % interpIndex /= Agrif_Curgrid % parent % ngridstep ) then
        child_var % oldvalues2d(1,kindex:kindex+newsize-1) = &
        child_var % oldvalues2d(2,kindex:kindex+newsize-1)
    endif

    SELECT CASE (nbdim)
    CASE (1)
!CDIR ALTCODE
        do ir = bounds(1,1), bounds(1,2)
            child_var % oldvalues2d(2,kindex) = child_var % parray1(ir)
            kindex = kindex + 1
        enddo
!
    CASE (2)
        do jr = bounds(2,1),bounds(2,2)
!CDIR ALTCODE
        do ir = bounds(1,1),bounds(1,2)
            child_var % oldvalues2d(2,kindex) = child_var % parray2(ir,jr)
            kindex = kindex + 1
        enddo
        enddo
!
    CASE (3)
        do kr = bounds(3,1),bounds(3,2)
        do jr = bounds(2,1),bounds(2,2)
!CDIR ALTCODE
        do ir = bounds(1,1),bounds(1,2)
            child_var % oldvalues2d(2,kindex) = child_var % parray3(ir,jr,kr)
            kindex = kindex + 1
        enddo
        enddo
        enddo
!
      CASE (4)
        do lr = bounds(4,1),bounds(4,2)
        do kr = bounds(3,1),bounds(3,2)
        do jr = bounds(2,1),bounds(2,2)
!CDIR ALTCODE
        do ir = bounds(1,1),bounds(1,2)
            child_var % oldvalues2d(2,kindex) = child_var % parray4(ir,jr,kr,lr)
            kindex = kindex + 1
        enddo
        enddo
        enddo
        enddo
!
    CASE (5)
        do mr = bounds(5,1),bounds(5,2)
        do lr = bounds(4,1),bounds(4,2)
        do kr = bounds(3,1),bounds(3,2)
        do jr = bounds(2,1),bounds(2,2)
!CDIR ALTCODE
        do ir = bounds(1,1),bounds(1,2)
            child_var % oldvalues2d(2,kindex) = child_var % parray5(ir,jr,kr,lr,mr)
            kindex = kindex + 1
        enddo
        enddo
        enddo
        enddo
        enddo
!
    CASE (6)
        do nr = bounds(6,1),bounds(6,2)
        do mr = bounds(5,1),bounds(5,2)
        do lr = bounds(4,1),bounds(4,2)
        do kr = bounds(3,1),bounds(3,2)
        do jr = bounds(2,1),bounds(2,2)
!CDIR ALTCODE
        do ir = bounds(1,1),bounds(1,2)
            child_var % oldvalues2d(2,kindex) = child_var % parray6(ir,jr,kr,lr,mr,nr)
            kindex = kindex + 1
        enddo
        enddo
        enddo
        enddo
        enddo
        enddo
    END SELECT
!---------------------------------------------------------------------------------------------------
end subroutine saveAfterInterp
!===================================================================================================
!
!===================================================================================================
!  subroutine timeInterpolation
!
!> subroutine for a linear time interpolation on the child grid
!---------------------------------------------------------------------------------------------------
subroutine timeInterpolation ( child_var, bounds, kindex, c1t, c2t, nbdim )
!---------------------------------------------------------------------------------------------------
    TYPE (Agrif_Variable)       :: child_var !< The fine grid variable
    INTEGER, DIMENSION(nbdim,2) :: bounds
    INTEGER                     :: kindex    !< Index indicating the values of the fine grid got
                                             !<    before and after the space interpolation and
                                             !<    used for the time interpolation
    REAL                        :: c1t, c2t  !< Coefficients for the time interpolation (c2t=1-c1t)
    INTEGER                     :: nbdim
!
    INTEGER :: ir,jr,kr,lr,mr,nr
!
    SELECT CASE (nbdim)
    CASE (1)
!CDIR ALTCODE
        do ir = bounds(1,1),bounds(1,2)
            child_var % parray1(ir) = c2t*child_var % oldvalues2d(1,kindex) + &
                                      c1t*child_var % oldvalues2d(2,kindex)
            kindex = kindex + 1
        enddo
!
    CASE (2)
        do jr = bounds(2,1),bounds(2,2)
!CDIR ALTCODE
        do ir = bounds(1,1),bounds(1,2)
            child_var % parray2(ir,jr) = c2t*child_var % oldvalues2d(1,kindex) + &
                                         c1t*child_var % oldvalues2d(2,kindex)
            kindex = kindex + 1
        enddo
        enddo
!
    CASE (3)
        do kr = bounds(3,1),bounds(3,2)
        do jr = bounds(2,1),bounds(2,2)
!CDIR ALTCODE
        do ir = bounds(1,1),bounds(1,2)
            child_var % parray3(ir,jr,kr) = c2t*child_var % oldvalues2d(1,kindex) + &
                                            c1t*child_var % oldvalues2d(2,kindex)
            kindex = kindex + 1
        enddo
        enddo
        enddo
!
    CASE (4)
        do lr = bounds(4,1),bounds(4,2)
        do kr = bounds(3,1),bounds(3,2)
        do jr = bounds(2,1),bounds(2,2)
!CDIR ALTCODE
        do ir = bounds(1,1),bounds(1,2)
            child_var % parray4(ir,jr,kr,lr) = c2t*child_var % oldvalues2d(1,kindex) + &
                                               c1t*child_var % oldvalues2d(2,kindex)
            kindex = kindex + 1
        enddo
        enddo
        enddo
        enddo
!
    CASE (5)
        do mr=bounds(5,1),bounds(5,2)
        do lr=bounds(4,1),bounds(4,2)
        do kr=bounds(3,1),bounds(3,2)
        do jr=bounds(2,1),bounds(2,2)
!CDIR ALTCODE
        do ir=bounds(1,1),bounds(1,2)
            child_var % parray5(ir,jr,kr,lr,mr) = c2t*child_var % oldvalues2d(1,kindex) + &
                                                  c1t*child_var % oldvalues2d(2,kindex)
            kindex = kindex + 1
        enddo
        enddo
        enddo
        enddo
        enddo
!
    CASE (6)
        do nr=bounds(6,1),bounds(6,2)
        do mr=bounds(5,1),bounds(5,2)
        do lr=bounds(4,1),bounds(4,2)
        do kr=bounds(3,1),bounds(3,2)
        do jr=bounds(2,1),bounds(2,2)
!CDIR ALTCODE
        do ir=bounds(1,1),bounds(1,2)
            child_var % parray6(ir,jr,kr,lr,mr,nr) = c2t*child_var % oldvalues2d(1,kindex) + &
                                                     c1t*child_var % oldvalues2d(2,kindex)
            kindex = kindex + 1
        enddo
        enddo
        enddo
        enddo
        enddo
        enddo
    END SELECT
!---------------------------------------------------------------------------------------------------
end subroutine timeInterpolation
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Checksize
!
!> subroutine used in the saveAfterInterp procedure to allocate the oldvalues2d array
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Checksize ( child_var, newsize )
!---------------------------------------------------------------------------------------------------
    TYPE (Agrif_Variable), INTENT(inout)  :: child_var !< The fine grid variable
    INTEGER              , INTENT(in)     :: newsize   !< Size of the domains where the boundary
                                                       !<    conditions are calculated
!
    REAL, DIMENSION(:,:), Allocatable :: tempoldvalues ! Temporary array
!
    if (.NOT. associated(child_var % oldvalues2d)) then
!
        allocate(child_var % oldvalues2d(2,newsize))
        child_var % oldvalues2d = 0.
!
    else
!
        if (SIZE(child_var % oldvalues2d,2) < newsize) then
!
            allocate(tempoldvalues(2,SIZE(child_var % oldvalues2d,2)))
            tempoldvalues = child_var % oldvalues2d
            deallocate(child_var % oldvalues2d)
            allocate(  child_var % oldvalues2d(2,newsize))
            child_var % oldvalues2d = 0.
            child_var % oldvalues2d(:,1:SIZE(tempoldvalues,2)) = tempoldvalues(:,:)
            deallocate(tempoldvalues)
!
        endif
!
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Checksize
!===================================================================================================
!
end module Agrif_Boundary

