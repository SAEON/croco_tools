!
! $Id: modinterp.F 779 2007-12-22 17:04:17Z rblod $
!
!     AGRif (Adaptive Grid Refinement In Fortran)
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
!> Module to initialize a fine grid from its parent grid, by using a space interpolation
!
module Agrif_Interpolation
!
    use Agrif_InterpBasic
    use Agrif_Arrays
    use Agrif_Mask
    use Agrif_CurgridFunctions
#if defined AGRIF_MPI
    use Agrif_Mpp
#endif
!
    implicit none
!
    logical, private:: precomputedone(7) = .FALSE.
!
contains
!
!===================================================================================================
!  subroutine Agrif_Interp_1d
!
!> Calculates the boundary conditions of a fine grid for a 1D grid variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_1d ( TypeInterp, parent, child, tab, torestore, nbdim, procname )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(6), INTENT(in)   :: TypeInterp   !< Kind of interpolation (linear,lagrange,spline)
    TYPE(Agrif_PVariable)               :: parent       !< Variable on the parent grid
    TYPE(Agrif_PVariable)               :: child        !< Variable on the child grid
    REAL, DIMENSION(                    &
        child%var%lb(1):child%var%ub(1) &
    ), target                           :: tab          !< Result
    LOGICAL, INTENT(in)                 :: torestore
    INTEGER, INTENT(in)                 :: nbdim
    External :: procname
    Optional :: procname
!
    TYPE(Agrif_PVariable) :: childtemp   ! Temporary variable on the child grid
!
    allocate(childtemp % var)
!
!   Pointer on the root variable
    childtemp % var % root_var => child % var % root_var
!
!   Number of dimensions of the grid variable
    childtemp % var % nbdim = nbdim
!
!   Tab is the result of the interpolation
    childtemp % var % parray1 => tab
    childtemp % var % lb = child % var % lb
    childtemp % var % ub = child % var % ub
!
    if (torestore) then
        childtemp % var % parray1 = child % var % array1
        childtemp % var % restore1D => child % var % restore1D
    endif
!
!   Index indicating (in the Agrif_Interp1D procedure) if a space interpolation is necessary
    childtemp % var % interpIndex => child % var % interpIndex
    childtemp % var % Interpolationshouldbemade = child % var % Interpolationshouldbemade
    childtemp % var % list_interp => child % var% list_interp
!
    if (present(procname)) then
        call Agrif_InterpVariable(TypeInterp,parent,childtemp,torestore,procname)
    else
        call Agrif_InterpVariable(TypeInterp,parent,childtemp,torestore)
    endif
    child % var % list_interp => childtemp % var %list_interp
!
    deallocate(childtemp % var)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_1D
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_2d
!
!> Calculates the boundary conditions of a fine grid for a 2D grid variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_2d ( TypeInterp, parent, child, tab, torestore, nbdim, procname )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(6), INTENT(in)   :: TypeInterp   !< Kind of interpolation (linear,lagrange,spline)
    TYPE(Agrif_PVariable)               :: parent       !< Variable on the parent grid
    TYPE(Agrif_PVariable)               :: child        !< Variable on the child grid
    REAL, DIMENSION(                     &
        child%var%lb(1):child%var%ub(1), &
        child%var%lb(2):child%var%ub(2)  &
    ), target                           :: tab          !< Result
    LOGICAL, INTENT(in)                 :: torestore
    INTEGER, INTENT(in)                 :: nbdim
    External :: procname
    Optional :: procname
!
    TYPE(Agrif_PVariable) :: childtemp   ! Temporary variable on the child grid
!
    allocate(childtemp % var)
!
!   Pointer on the root variable
    childtemp % var % root_var => child % var % root_var
!
!   Number of dimensions of the grid variable
    childtemp % var % nbdim = nbdim
!
!   Tab is the result of the interpolation
    childtemp % var % parray2 => tab
    childtemp % var % lb = child % var % lb
    childtemp % var % ub = child % var % ub
!
    if (torestore) then
        childtemp % var % parray2 = child % var % array2
        childtemp % var % restore2D => child % var % restore2D
    endif
!
!   Index indicating (in the Agrif_Interp2D procedure) if a space interpolation is necessary
    childtemp % var % interpIndex => child % var % interpIndex
    childtemp % var % Interpolationshouldbemade = child % var % Interpolationshouldbemade
    childtemp % var % list_interp => child % var% list_interp
!
    if (present(procname)) then
        call Agrif_InterpVariable(TypeInterp,parent,childtemp,torestore,procname)
    else
        call Agrif_InterpVariable(TypeInterp,parent,childtemp,torestore)
    endif
    child % var % list_interp => childtemp % var %list_interp
!
    deallocate(childtemp % var)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_2D
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_3d
!
!> Calculates the boundary conditions of a fine grid for a 3D grid variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_3d ( TypeInterp, parent, child, tab, torestore, nbdim, procname )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(6), INTENT(in)   :: TypeInterp   !< Kind of interpolation (linear,lagrange,spline)
    TYPE(Agrif_PVariable)               :: parent       !< Variable on the parent grid
    TYPE(Agrif_PVariable)               :: child        !< Variable on the child grid
    REAL, DIMENSION(                     &
        child%var%lb(1):child%var%ub(1), &
        child%var%lb(2):child%var%ub(2), &
        child%var%lb(3):child%var%ub(3)  &
    ), target                           :: tab          !< Result
    LOGICAL, INTENT(in)                 :: torestore
    INTEGER, INTENT(in)                 :: nbdim
    External :: procname
    Optional :: procname
!
    TYPE(Agrif_PVariable) :: childtemp   ! Temporary variable on the child grid
!
    allocate(childtemp % var)
!
!   Pointer on the root variable
    childtemp % var % root_var => child % var % root_var
!
!   Number of dimensions of the grid variable
    childtemp % var % nbdim = nbdim
!
!   Tab is the result of the interpolation
    childtemp % var % parray3 => tab
    childtemp % var % lb = child % var % lb
    childtemp % var % ub = child % var % ub
!
    if (torestore) then
        childtemp % var % parray3 = child % var % array3
        childtemp % var % restore3D => child % var % restore3D
    endif
!
!   Index indicating (in the Agrif_Interp3D procedure) if a space interpolation is necessary
    childtemp % var % interpIndex => child % var % interpIndex
    childtemp % var % Interpolationshouldbemade = child % var % Interpolationshouldbemade
    childtemp % var % list_interp => child % var % list_interp
!
    if (present(procname)) then
        call Agrif_InterpVariable(TypeInterp,parent,childtemp,torestore,procname)
    else
        call Agrif_InterpVariable(TypeInterp,parent,childtemp,torestore)
    endif
    child % var % list_interp => childtemp % var %list_interp
!
    deallocate(childtemp % var)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_3D
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_4d
!
!> Calculates the boundary conditions of a fine grid for a 4D grid variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_4d ( TypeInterp, parent, child, tab, torestore, nbdim, procname )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(6), INTENT(in)   :: TypeInterp   !< Kind of interpolation (linear,lagrange,spline)
    TYPE(Agrif_PVariable)               :: parent       !< Variable on the parent grid
    TYPE(Agrif_PVariable)               :: child        !< Variable on the child grid
    REAL, DIMENSION(                     &
        child%var%lb(1):child%var%ub(1), &
        child%var%lb(2):child%var%ub(2), &
        child%var%lb(3):child%var%ub(3), &
        child%var%lb(4):child%var%ub(4)  &
    ), target                           :: tab          !< Result
    LOGICAL, INTENT(in)                 :: torestore
    INTEGER, INTENT(in)                 :: nbdim
    External :: procname
    Optional :: procname
!
    TYPE(Agrif_PVariable) :: childtemp   ! Temporary variable on the child grid
!
    allocate(childtemp % var)
!
!   Pointer on the root variable
    childtemp % var % root_var => child % var % root_var
!
!   Number of dimensions of the grid variable
    childtemp % var % nbdim = nbdim
!
!   Tab is the result of the interpolation
    childtemp % var % parray4 => tab
    childtemp % var % lb = child % var % lb
    childtemp % var % ub = child % var % ub
!
    if (torestore) then
        childtemp % var % parray4 = child % var % array4
        childtemp % var % restore4D => child % var % restore4D
    endif
!
!   Index indicating (in the Agrif_Interp4D procedure) if a space interpolation is necessary
    childtemp % var % interpIndex => child % var % interpIndex
    childtemp % var % Interpolationshouldbemade = child % var % Interpolationshouldbemade
    childtemp % var % list_interp => child % var% list_interp
!
    if (present(procname)) then
        call Agrif_InterpVariable(TypeInterp,parent,childtemp,torestore,procname)
    else
        call Agrif_InterpVariable(TypeInterp,parent,childtemp,torestore)
    endif
    child % var % list_interp => childtemp % var %list_interp
!
    deallocate(childtemp % var)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_4D
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_5d
!
!> Calculates the boundary conditions of a fine grid for a 5D grid variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_5d ( TypeInterp, parent, child, tab, torestore, nbdim, procname )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(6), INTENT(in)   :: TypeInterp   !< Kind of interpolation (linear,lagrange,spline)
    TYPE(Agrif_PVariable)               :: parent       !< Variable on the parent grid
    TYPE(Agrif_PVariable)               :: child        !< Variable on the child grid
    REAL, DIMENSION(                     &
        child%var%lb(1):child%var%ub(1), &
        child%var%lb(2):child%var%ub(2), &
        child%var%lb(3):child%var%ub(3), &
        child%var%lb(4):child%var%ub(4), &
        child%var%lb(5):child%var%ub(5)  &
    ), target                           :: tab          !< Result
    LOGICAL, INTENT(in)                 :: torestore
    INTEGER, INTENT(in)                 :: nbdim
    External :: procname
    Optional :: procname
!
    TYPE(Agrif_PVariable) :: childtemp   ! Temporary variable on the child grid
!
    allocate(childtemp % var)
!
!   Pointer on the root variable
    childtemp % var % root_var => child % var % root_var
!
!   Number of dimensions of the grid variable
    childtemp % var % nbdim = nbdim
!
!   Tab is the result of the interpolation
    childtemp % var % parray5 => tab
    childtemp % var % lb = child % var % lb
    childtemp % var % ub = child % var % ub
!
    if (torestore) then
        childtemp % var % parray5 = child % var % array5
        childtemp % var % restore5D => child % var % restore5D
    endif
!
!   Index indicating (in the Agrif_Interp5D procedure) if a space interpolation is necessary
    childtemp % var % interpIndex => child % var % interpIndex
    childtemp % var % Interpolationshouldbemade = child % var % Interpolationshouldbemade
    childtemp % var % list_interp => child % var% list_interp
!
    if (present(procname)) then
        call Agrif_InterpVariable(TypeInterp,parent,childtemp,torestore,procname)
    else
        call Agrif_InterpVariable(TypeInterp,parent,childtemp,torestore)
    endif
    child % var % list_interp => childtemp % var %list_interp
!
    deallocate(childtemp % var)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_5D
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_6d
!
!> Calculates the boundary conditions of a fine grid for a 6D grid variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_6d ( TypeInterp, parent, child, tab, torestore, nbdim, procname )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(6), INTENT(in)   :: TypeInterp   !< Kind of interpolation (linear,lagrange,spline)
    TYPE(Agrif_PVariable)               :: parent       !< Variable on the parent grid
    TYPE(Agrif_PVariable)               :: child        !< Variable on the child grid
    REAL, DIMENSION(                     &
        child%var%lb(1):child%var%ub(1), &
        child%var%lb(2):child%var%ub(2), &
        child%var%lb(3):child%var%ub(3), &
        child%var%lb(4):child%var%ub(4), &
        child%var%lb(5):child%var%ub(5), &
        child%var%lb(6):child%var%ub(6)  &
    ), target                           :: tab          !< Result
    LOGICAL, INTENT(in)                 :: torestore
    INTEGER, INTENT(in)                 :: nbdim
    External :: procname
    Optional :: procname
!
    TYPE(Agrif_PVariable) :: childtemp   ! Temporary variable on the child grid
!
    allocate(childtemp % var)
!
!   Pointer on the root variable
    childtemp % var % root_var => child % var % root_var
!
!   Number of dimensions of the grid variable
    childtemp % var % nbdim = nbdim
!
!   Tab is the result of the interpolation
    childtemp % var % parray6 => tab
    childtemp % var % lb = child % var % lb
    childtemp % var % ub = child % var % ub
!
    if (torestore) then
        childtemp % var % parray6 = child % var % array6
        childtemp % var % restore6D => child % var % restore6D
    endif
!
!   Index indicating (in the Agrif_Interp6D procedure) if a space interpolation is necessary
    childtemp % var % interpIndex => child % var % interpIndex
    childtemp % var % Interpolationshouldbemade = child % var % Interpolationshouldbemade
    childtemp % var % list_interp => child % var% list_interp
!
    if (present(procname)) then
        call Agrif_InterpVariable(TypeInterp,parent,childtemp,torestore,procname)
    else
        call Agrif_InterpVariable(TypeInterp,parent,childtemp,torestore)
    endif
    child % var % list_interp => childtemp % var %list_interp
!
    deallocate(childtemp % var)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_6D
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_InterpVariable
!
!> Sets some arguments of subroutine Agrif_InterpnD, n being the dimension of the grid variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_InterpVariable ( TypeInterp, parent, child, torestore, procname )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(6), INTENT(in)   :: TypeInterp   !< TYPE of interpolation (linear,spline,...)
    TYPE(Agrif_PVariable)               :: parent       !< Variable on the parent grid
    TYPE(Agrif_PVariable)               :: child        !< Variable on the child grid
    LOGICAL,               INTENT(in)   :: torestore    !< .false. indicates that the results of the
                                                        !< interpolation are applied on the whole current grid
    External :: procname
    Optional :: procname
!
    INTEGER               :: nbdim        ! Number of dimensions of the current grid
    INTEGER, DIMENSION(6) :: pttab_child
    INTEGER, DIMENSION(6) :: petab_child
    INTEGER, DIMENSION(6) :: pttab_parent
    REAL   , DIMENSION(6) :: s_child,s_parent
    REAL   , DIMENSION(6) :: ds_child,ds_parent
!
    nbdim = child % var % root_var % nbdim
!
    call PreProcessToInterpOrUpdate(parent,child,petab_Child(1:nbdim),pttab_Child(1:nbdim),     &
                                    pttab_Parent(1:nbdim),s_Child(1:nbdim),s_Parent(1:nbdim),   &
                                    ds_Child(1:nbdim),ds_Parent(1:nbdim),nbdim,interp=.true.)
!
!   Call to a procedure of interpolation against the number of dimensions of the grid variable
!
    if (present(procname)) then
        call Agrif_InterpnD(TypeInterp,parent,child,                        &
                            pttab_Child(1:nbdim),petab_Child(1:nbdim),      &
                            pttab_Child(1:nbdim),pttab_Parent(1:nbdim),     &
                            s_Child(1:nbdim),s_Parent(1:nbdim),             &
                            ds_Child(1:nbdim),ds_Parent(1:nbdim),           &
                            child,torestore,nbdim,procname)
    else
        call Agrif_InterpnD(TypeInterp,parent,child,                        &
                            pttab_Child(1:nbdim),petab_Child(1:nbdim),      &
                            pttab_Child(1:nbdim),pttab_Parent(1:nbdim),     &
                            s_Child(1:nbdim),s_Parent(1:nbdim),             &
                            ds_Child(1:nbdim),ds_Parent(1:nbdim),           &
                            child,torestore,nbdim)
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_InterpVariable
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_InterpnD
!
!> Interpolates a nD grid variable from its parent grid, by using a space interpolation
!---------------------------------------------------------------------------------------------------
subroutine Agrif_InterpnD ( TypeInterp, parent, child, pttab, petab, pttab_Child, pttab_Parent, &
                            s_Child, s_Parent, ds_Child, ds_Parent, restore, torestore,         &
                            nbdim, procname )
!---------------------------------------------------------------------------------------------------
#if defined AGRIF_MPI
    include 'mpif.h'
#endif
!
    INTEGER, DIMENSION(6), INTENT(in)   :: TypeInterp   !< Type of interpolation !    (linear,...)
    TYPE(Agrif_PVariable)      :: parent             !< Variable of the parent grid
    TYPE(Agrif_PVariable)      :: child              !< Variable of the child grid
    INTEGER, DIMENSION(nbdim)  :: pttab              !< Index of the first point inside the domain
    INTEGER, DIMENSION(nbdim)  :: petab              !< Index of the first point inside the domain
    INTEGER, DIMENSION(nbdim)  :: pttab_Child        !< Index of the first point inside the domain 
                                                     !<    for the child grid variable
    INTEGER, DIMENSION(nbdim)  :: pttab_Parent       !< Index of the first point inside the domain
                                                     !<    for the parent grid variable
    REAL,    DIMENSION(nbdim)  :: s_Child,s_Parent   !< Positions of the parent and child grids
    REAL,    DIMENSION(nbdim)  :: ds_Child,ds_Parent !< Space steps of the parent and child grids
    TYPE(Agrif_PVariable)      :: restore            !< Indicates points where interpolation
    LOGICAL                    :: torestore          !< Indicates if the array restore is used
    INTEGER                    :: nbdim
    External :: procname
    Optional :: procname
!
    INTEGER                       :: i,j,k,l,m,n
    INTEGER, DIMENSION(nbdim)     :: pttruetab,cetruetab
    INTEGER, DIMENSION(nbdim)     :: indmin,indmax
    INTEGER, DIMENSION(nbdim)     :: indminglob, indmaxglob
    INTEGER, DIMENSION(nbdim)     :: indminglob2,indmaxglob2
    LOGICAL, DIMENSION(nbdim)     :: noraftab
    REAL   , DIMENSION(nbdim)     :: s_Child_temp,s_Parent_temp
    INTEGER, DIMENSION(nbdim)     :: lowerbound,upperbound
    INTEGER, DIMENSION(nbdim,2,2) :: childarray
    INTEGER, DIMENSION(nbdim,2,2) :: parentarray
    LOGICAL :: memberin,member
    LOGICAL :: find_list_interp
!
#if defined AGRIF_MPI
!
    INTEGER, PARAMETER          :: etiquette = 100
    INTEGER                     :: code
    INTEGER, DIMENSION(nbdim,4)                     :: tab3
    INTEGER, DIMENSION(nbdim,4,0:Agrif_Nbprocs-1)   :: tab4
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1,8)   :: tab4t
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1)           :: memberinall
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1)           :: sendtoproc1, recvfromproc1
    LOGICAL, DIMENSION(1)                           :: memberin1
    LOGICAL                                         :: memberout
!
#endif
!
    TYPE(Agrif_PVariable), SAVE :: tempP, tempPextend   ! Temporary parent grid variable
    TYPE(Agrif_PVariable), SAVE :: tempC                ! Temporary child grid variable
    TYPE(Agrif_PVariable), SAVE :: parentvalues
!
!   Boundaries of the current grid where interpolation is done
    if (Associated(child%var%list_interp)) then
        call Agrif_Find_list_interp(child%var%list_interp,pttab,petab,  &
                                    pttab_Child,pttab_Parent,nbdim,     &
                                    indmin,indmax,indminglob,           &
                                    indmaxglob,indminglob2,indmaxglob2, &
                                    parentarray,pttruetab,cetruetab,    &
                                    member,memberin,find_list_interp    &
#if defined AGRIF_MPI
                           ,tab4t,memberinall,sendtoproc1,recvfromproc1 &
#endif
        )
    else
        find_list_interp = .FALSE.
    endif

    if (.not.find_list_interp) then
!
        call Agrif_nbdim_Get_bound_dimension(child%var,lowerbound,upperbound,nbdim)
        call Agrif_Childbounds(nbdim,lowerbound,upperbound,pttab,petab, &
                               pttruetab,cetruetab,memberin)
        call Agrif_Parentbounds(TypeInterp,nbdim,indminglob,indmaxglob, &
                                s_Parent_temp,s_Child_temp,             &
                                s_Child,ds_Child,                       &
                                s_Parent,ds_Parent,                     &
                                pttab,petab,                            &
                                pttab_Child,pttab_Parent,               &
                                child%var%root_var % posvar,            &
                                child%var%root_var % interptab)
#if defined AGRIF_MPI
        if (memberin) then
            call Agrif_Parentbounds(TypeInterp,nbdim,indmin,indmax,     &
                                    s_Parent_temp,s_Child_temp,         &
                                    s_Child,ds_Child,                   &
                                    s_Parent,ds_Parent,                 &
                                    pttruetab,cetruetab,                &
                                    pttab_Child,pttab_Parent,           &
                                    child%var%root_var % posvar,        &
                                    child%var%root_var % interptab)
        endif

        call Agrif_nbdim_Get_bound_dimension(parent%var,lowerbound,upperbound,nbdim)
        call Agrif_ChildGrid_to_ParentGrid()
!
        call Agrif_Childbounds(nbdim,lowerbound,upperbound, &
                               indminglob,indmaxglob,       &
                               indminglob2,indmaxglob2,member)
!
        if (member) then
            call Agrif_GlobtoLocInd2(parentarray,lowerbound,upperbound, &
                                     indminglob2,indmaxglob2,           &
                                     nbdim,Agrif_Procrank,member)
        endif

        call Agrif_ParentGrid_to_ChildGrid()
#else
        parentarray(:,1,1) = indminglob
        parentarray(:,2,1) = indmaxglob
        parentarray(:,1,2) = indminglob
        parentarray(:,2,2) = indmaxglob
        indmin = indminglob
        indmax = indmaxglob
        member = .TRUE.
#endif

    else

#if defined AGRIF_MPI
        s_Parent_temp = s_Parent + (indmin - pttab_Parent) * ds_Parent
        s_Child_temp  = s_Child + (pttruetab - pttab_Child) * ds_Child
#else
        parentarray(:,1,1) = indminglob
        parentarray(:,2,1) = indmaxglob
        parentarray(:,1,2) = indminglob
        parentarray(:,2,2) = indmaxglob
        indmin = indminglob
        indmax = indmaxglob
        member = .TRUE.
        s_Parent_temp = s_Parent + (indminglob - pttab_Parent) * ds_Parent
        s_Child_temp  = s_Child + (pttab - pttab_Child) * ds_Child
#endif

    endif

    if (member) then
        if (.not.associated(tempP%var)) allocate(tempP%var)
!
        call Agrif_nbdim_allocation(tempP%var,parentarray(:,1,1),parentarray(:,2,1),nbdim)
        call Agrif_nbdim_Full_VarEQreal(tempP%var,0.,nbdim)

        if (present(procname)) then
!
            call Agrif_ChildGrid_to_ParentGrid()
!
            SELECT CASE (nbdim)
            CASE(1)
                CALL procname(tempP%var%array1,                     &
                          parentarray(1,1,2),parentarray(1,2,2))
            CASE(2)
                CALL procname(tempP%var%array2,                     &
                          parentarray(1,1,2),parentarray(1,2,2),    &
                          parentarray(2,1,2),parentarray(2,2,2))
            CASE(3)
                CALL procname(tempP%var%array3,                     &
                          parentarray(1,1,2),parentarray(1,2,2),    &
                          parentarray(2,1,2),parentarray(2,2,2),    &
                          parentarray(3,1,2),parentarray(3,2,2))
            CASE(4)
                CALL procname(tempP%var%array4,                     &
                          parentarray(1,1,2),parentarray(1,2,2),    &
                          parentarray(2,1,2),parentarray(2,2,2),    &
                          parentarray(3,1,2),parentarray(3,2,2),    &
                          parentarray(4,1,2),parentarray(4,2,2))
            CASE(5)
                CALL procname(tempP%var%array5,                     &
                          parentarray(1,1,2),parentarray(1,2,2),    &
                          parentarray(2,1,2),parentarray(2,2,2),    &
                          parentarray(3,1,2),parentarray(3,2,2),    &
                          parentarray(4,1,2),parentarray(4,2,2),    &
                          parentarray(5,1,2),parentarray(5,2,2))
            CASE(6)
                CALL procname(tempP%var%array6,                     &
                          parentarray(1,1,2),parentarray(1,2,2),    &
                          parentarray(2,1,2),parentarray(2,2,2),    &
                          parentarray(3,1,2),parentarray(3,2,2),    &
                          parentarray(4,1,2),parentarray(4,2,2),    &
                          parentarray(5,1,2),parentarray(5,2,2),    &
                          parentarray(6,1,2),parentarray(6,2,2))
            END SELECT
!
            call Agrif_ParentGrid_to_ChildGrid()
!
        else
            call Agrif_nbdim_VarEQvar(tempP%var,  parentarray(:,1,1), parentarray(:,2,1), &
                                      parent%var, parentarray(:,1,2), parentarray(:,2,2), &
                                      nbdim)
        endif
    endif

#if defined AGRIF_MPI
    if (.not.find_list_interp) then
!
        tab3(:,1) = indminglob2(:)
        tab3(:,2) = indmaxglob2(:)
        tab3(:,3) = indmin(:)
        tab3(:,4) = indmax(:)
!
        call MPI_ALLGATHER(tab3,4*nbdim,MPI_INTEGER,tab4,4*nbdim,MPI_INTEGER,MPI_COMM_WORLD,code)

        if (.not.associated(tempPextend%var))   Allocate(tempPextend%var)

        DO k=0,Agrif_Nbprocs-1
            do j=1,4
                do i=1,nbdim
                    tab4t(i,k,j) = tab4(i,j,k)
                enddo
            enddo
        enddo

        memberin1(1) = memberin
        CALL MPI_ALLGATHER(memberin1,1,MPI_LOGICAL,memberinall,1,MPI_LOGICAL,MPI_COMM_WORLD,code)

        call Get_External_Data_first(tab4t(:,:,1),tab4t(:,:,2),         &
                                     tab4t(:,:,3),tab4t(:,:,4),         &
                                     nbdim,memberinall, &
                                     sendtoproc1,recvfromproc1,         &
                                     tab4t(:,:,5),tab4t(:,:,6),         &
                                     tab4t(:,:,7),tab4t(:,:,8) )
    endif

!   call Get_External_Data(tempP,tempPextend,tab4t(:,:,1), &
!                 tab4t(:,:,2), &
!                 tab4t(:,:,3),tab4t(:,:,4),nbdim,member,memberin, &
!                 memberinall)
!
    call ExchangeSameLevel2(sendtoproc1,recvfromproc1,nbdim,        &
            tab4t(:,:,3),tab4t(:,:,4),tab4t(:,:,5),tab4t(:,:,6),    &
            tab4t(:,:,7),tab4t(:,:,8),memberin,tempP,tempPextend)
#else
    tempPextend%var => tempP%var
#endif

    if (.not.find_list_interp) then
        call Agrif_Addto_list_interp(                           &
                child%var%list_interp,pttab,petab,              &
                pttab_Child,pttab_Parent,indmin,indmax,         &
                indminglob,indmaxglob,indminglob2,indmaxglob2,  &
                parentarray,pttruetab,cetruetab,                &
                member,memberin,nbdim                           &
#if defined AGRIF_MPI
               ,tab4t,memberinall,sendtoproc1,recvfromproc1     &
#endif
        )
    endif
!
    if (memberin) then
!
        if (.not.associated(tempC%var)) allocate(tempC%var)
!
        call Agrif_nbdim_allocation(tempC%var,pttruetab,cetruetab,nbdim)
!
!       Special values on the parent grid
        if (Agrif_UseSpecialValue) then
!
            noraftab(1:nbdim) = child % var % root_var % interptab(1:nbdim) == 'N'
!
            if (.not.associated(parentvalues%var))  Allocate(parentvalues%var)
!
            call Agrif_nbdim_allocation(parentvalues%var,indmin,indmax,nbdim)
            call Agrif_nbdim_Full_VarEQvar(parentvalues%var,tempPextend%var,nbdim)
!
            call Agrif_CheckMasknD(tempPextend,parentvalues,    &
                    indmin(1:nbdim),indmax(1:nbdim),            &
                    indmin(1:nbdim),indmax(1:nbdim),            &
                    noraftab(1:nbdim),nbdim)
!
            call Agrif_nbdim_deallocation(parentvalues%var,nbdim)
!           Deallocate(parentvalues%var)
!
        endif
!
!       Interpolation of the current grid

        if (memberin) then
            if ( nbdim == 1 ) then
                call Agrif_Interp_1D_recursive(TypeInterp,          &
                        tempPextend%var%array1,tempC%var%array1,    &
                        indmin,indmax,pttruetab,cetruetab,          &
                        s_Child_temp,s_Parent_temp,ds_Child,ds_Parent,nbdim)
            elseif ( nbdim == 2 ) then
                call Agrif_Interp_2D_recursive(TypeInterp,          &
                        tempPextend%var%array2,tempC%var%array2,    &
                        indmin,indmax,pttruetab,cetruetab,          &
                        s_Child_temp,s_Parent_temp,ds_Child,ds_Parent,nbdim)
            elseif ( nbdim == 3 ) then
                call Agrif_Interp_3D_recursive(TypeInterp,          &
                        tempPextend%var%array3,tempC%var%array3,    &
                        indmin,indmax,pttruetab,cetruetab,          &
                        s_Child_temp,s_Parent_temp,ds_Child,ds_Parent,nbdim)
            elseif ( nbdim == 4 ) then
                call Agrif_Interp_4D_recursive(TypeInterp,          &
                        tempPextend%var%array4,tempC%var%array4,    &
                        indmin,indmax,pttruetab,cetruetab,          &
                        s_Child_temp,s_Parent_temp,ds_Child,ds_Parent,nbdim)
            elseif ( nbdim == 5 ) then
                call Agrif_Interp_5D_recursive(TypeInterp,          &
                        tempPextend%var%array5,tempC%var%array5,    &
                        indmin,indmax,pttruetab,cetruetab,          &
                        s_Child_temp,s_Parent_temp,ds_Child,ds_Parent,nbdim)
            elseif ( nbdim == 6 ) then
                call Agrif_Interp_6D_recursive(TypeInterp,          &
                        tempPextend%var%array6,tempC%var%array6,    &
                        indmin,indmax,pttruetab,cetruetab,          &
                        s_Child_temp,s_Parent_temp,ds_Child,ds_Parent,nbdim)
            endif
!
            call Agrif_nbdim_Get_bound_dimension(child % var,lowerbound,upperbound,nbdim)

#if defined AGRIF_MPI
            call Agrif_GlobtoLocInd2(childarray,lowerbound,upperbound,  &
                     pttruetab,cetruetab,nbdim,Agrif_Procrank,memberout)
#else
            childarray(:,1,1) = pttruetab
            childarray(:,2,1) = cetruetab
            childarray(:,1,2) = pttruetab
            childarray(:,2,2) = cetruetab
!cccccccccccccc       memberout = .TRUE.
#endif
!
!           Special values on the child grid
            if (Agrif_UseSpecialValueFineGrid) then
!
                call GiveAgrif_SpecialValueToTab_mpi(       &
                        child%var,tempC%var, childarray,    &
                        Agrif_SpecialValueFineGrid,nbdim)
!
            endif
        endif
!
        if (torestore) then
!
#if defined AGRIF_MPI
!
            SELECT CASE (nbdim)
            CASE (1)
                do i = pttruetab(1),cetruetab(1)
!hildarrayAModifier     if (restore%var%restore1D(i) == 0)      &
!hildarrayAModifier         child%var%array1(childarray(i,1,2)) = tempC%var%array1(i)
                enddo
            CASE (2)
                do i = pttruetab(1),cetruetab(1)
                do j = pttruetab(2),cetruetab(2)
!hildarrayAModifier     if (restore%var%restore2D(i,j) == 0)    &
!hildarrayAModifier         child%var%array2(childarray(i,1,2), &
!hildarrayAModifier                          childarray(j,2,2)) = tempC%var%array2(i,j)
                enddo
                enddo
            CASE (3)
                do i = pttruetab(1),cetruetab(1)
                do j = pttruetab(2),cetruetab(2)
                do k = pttruetab(3),cetruetab(3)
!hildarrayAModifier     if (restore%var%restore3D(i,j,k) == 0)  &
!hildarrayAModifier         child%var%array3(childarray(i,1,2), &
!hildarrayAModifier                          childarray(j,2,2), &
!hildarrayAModifier                          childarray(k,3,2)) = tempC%var%array3(i,j,k)
                enddo
                enddo
                enddo
            CASE (4)
                do i = pttruetab(1),cetruetab(1)
                do j = pttruetab(2),cetruetab(2)
                do k = pttruetab(3),cetruetab(3)
                do l = pttruetab(4),cetruetab(4)
!hildarrayAModifier     if (restore%var%restore4D(i,j,k,l) == 0)    &
!hildarrayAModifier         child%var%array4(childarray(i,1,2),     &
!hildarrayAModifier                          childarray(j,2,2),     &
!hildarrayAModifier                          childarray(k,3,2),     &
!hildarrayAModifier                          childarray(l,4,2)) = tempC%var%array4(i,j,k,l)
                enddo
                enddo
                enddo
                enddo
            CASE (5)
                do i = pttruetab(1),cetruetab(1)
                do j = pttruetab(2),cetruetab(2)
                do k = pttruetab(3),cetruetab(3)
                do l = pttruetab(4),cetruetab(4)
                do m = pttruetab(5),cetruetab(5)
!hildarrayAModifier     if (restore%var%restore5D(i,j,k,l,m) == 0)  &
!hildarrayAModifier         child%var%array5(childarray(i,1,2),     &
!hildarrayAModifier                          childarray(j,2,2),     &
!hildarrayAModifier                          childarray(k,3,2),     &
!hildarrayAModifier                          childarray(l,4,2),     &
!hildarrayAModifier                          childarray(m,5,2)) = tempC%var%array5(i,j,k,l,m)
                enddo
                enddo
                enddo
                enddo
                enddo
            CASE (6)
                do i = pttruetab(1),cetruetab(1)
                do j = pttruetab(2),cetruetab(2)
                do k = pttruetab(3),cetruetab(3)
                do l = pttruetab(4),cetruetab(4)
                do m = pttruetab(5),cetruetab(5)
                do n = pttruetab(6),cetruetab(6)
!hildarrayAModifier     if (restore%var%restore6D(i,j,k,l,m,n) == 0)    &
!hildarrayAModifier         child%var%array6(childarray(i,1,2),         &
!hildarrayAModifier                          childarray(j,2,2),         &
!hildarrayAModifier                          childarray(k,3,2),         &
!hildarrayAModifier                          childarray(l,4,2),         &
!hildarrayAModifier                          childarray(m,5,2),         &
!hildarrayAModifier                          childarray(n,6,2)) = tempC%var%array6(i,j,k,l,m,n)
                enddo
                enddo
                enddo
                enddo
                enddo
                enddo
            END SELECT
!
#else
            SELECT CASE (nbdim)
            CASE (1)
                do i = pttruetab(1),cetruetab(1)
                    if (restore%var%restore1D(i) == 0)          &
                        child % var % parray1(i) = tempC % var % array1(i)
                enddo
            CASE (2)
                do j = pttruetab(2),cetruetab(2)
                do i = pttruetab(1),cetruetab(1)
                    if (restore%var%restore2D(i,j) == 0)        &
                        child % var % parray2(i,j) = tempC % var % array2(i,j)
                enddo
                enddo
            CASE (3)
                do k = pttruetab(3),cetruetab(3)
                do j = pttruetab(2),cetruetab(2)
                do i = pttruetab(1),cetruetab(1)
                    if (restore%var%restore3D(i,j,k) == 0)      &
                        child % var % parray3(i,j,k) = tempC % var % array3(i,j,k)
                enddo
                enddo
                enddo
            CASE (4)
                do l = pttruetab(4),cetruetab(4)
                do k = pttruetab(3),cetruetab(3)
                do j = pttruetab(2),cetruetab(2)
                do i = pttruetab(1),cetruetab(1)
                    if (restore%var%restore4D(i,j,k,l) == 0)    &
                        child % var % parray4(i,j,k,l) = tempC % var % array4(i,j,k,l)
                enddo
                enddo
                enddo
                enddo
            CASE (5)
                do m = pttruetab(5),cetruetab(5)
                do l = pttruetab(4),cetruetab(4)
                do k = pttruetab(3),cetruetab(3)
                do j = pttruetab(2),cetruetab(2)
                do i = pttruetab(1),cetruetab(1)
                    if (restore%var%restore5D(i,j,k,l,m) == 0)  &
                        child % var % parray5(i,j,k,l,m) = tempC % var % array5(i,j,k,l,m)
                enddo
                enddo
                enddo
                enddo
                enddo
            CASE (6)
                do n = pttruetab(6),cetruetab(6)
                do m = pttruetab(5),cetruetab(5)
                do l = pttruetab(4),cetruetab(4)
                do k = pttruetab(3),cetruetab(3)
                do j = pttruetab(2),cetruetab(2)
                do i = pttruetab(1),cetruetab(1)
                    if (restore%var%restore6D(i,j,k,l,m,n) == 0)    &
                        child % var % parray6(i,j,k,l,m,n) = tempC % var % array6(i,j,k,l,m,n)
                enddo
                enddo
                enddo
                enddo
                enddo
                enddo
            END SELECT
!
#endif
!
        else
!
            if (memberin) then
!
                SELECT CASE (nbdim)
                CASE (1)
                    child%var%parray1(childarray(1,1,2):childarray(1,2,2)) =    &
                     tempC%var%array1(childarray(1,1,1):childarray(1,2,1))
                CASE (2)
                    child%var%parray2(childarray(1,1,2):childarray(1,2,2),      &
                                      childarray(2,1,2):childarray(2,2,2)) =    &
                     tempC%var%array2(childarray(1,1,1):childarray(1,2,1),      &
                                      childarray(2,1,1):childarray(2,2,1))
                CASE (3)
                    child%var%parray3(childarray(1,1,2):childarray(1,2,2),      &
                                      childarray(2,1,2):childarray(2,2,2),      &
                                      childarray(3,1,2):childarray(3,2,2)) =    &
                     tempC%var%array3(childarray(1,1,1):childarray(1,2,1),      &
                                      childarray(2,1,1):childarray(2,2,1),      &
                                      childarray(3,1,1):childarray(3,2,1))
                CASE (4)
                    child%var%parray4(childarray(1,1,2):childarray(1,2,2),      &
                                      childarray(2,1,2):childarray(2,2,2),      &
                                      childarray(3,1,2):childarray(3,2,2),      &
                                      childarray(4,1,2):childarray(4,2,2)) =    &
                     tempC%var%array4(childarray(1,1,1):childarray(1,2,1),      &
                                      childarray(2,1,1):childarray(2,2,1),      &
                                      childarray(3,1,1):childarray(3,2,1),      &
                                      childarray(4,1,1):childarray(4,2,1))
                CASE (5)
                    child%var%parray5(childarray(1,1,2):childarray(1,2,2),      &
                                      childarray(2,1,2):childarray(2,2,2),      &
                                      childarray(3,1,2):childarray(3,2,2),      &
                                      childarray(4,1,2):childarray(4,2,2),      &
                                      childarray(5,1,2):childarray(5,2,2)) =    &
                     tempC%var%array5(childarray(1,1,1):childarray(1,2,1),      &
                                      childarray(2,1,1):childarray(2,2,1),      &
                                      childarray(3,1,1):childarray(3,2,1),      &
                                      childarray(4,1,1):childarray(4,2,1),      &
                                      childarray(5,1,1):childarray(5,2,1))
                CASE (6)
                    child%var%parray6(childarray(1,1,2):childarray(1,2,2),      &
                                      childarray(2,1,2):childarray(2,2,2),      &
                                      childarray(3,1,2):childarray(3,2,2),      &
                                      childarray(4,1,2):childarray(4,2,2),      &
                                      childarray(5,1,2):childarray(5,2,2),      &
                                      childarray(6,1,2):childarray(6,2,2)) =    &
                     tempC%var%array6(childarray(1,1,1):childarray(1,2,1),      &
                                      childarray(2,1,1):childarray(2,2,1),      &
                                      childarray(3,1,1):childarray(3,2,1),      &
                                      childarray(4,1,1):childarray(4,2,1),      &
                                      childarray(5,1,1):childarray(5,2,1),      &
                                      childarray(6,1,1):childarray(6,2,1))
                END SELECT
            endif
!
        endif

        call Agrif_nbdim_deallocation(tempPextend%var,nbdim)
!        deallocate(tempPextend%var)

        call Agrif_nbdim_deallocation(tempC%var,nbdim)
!        deallocate(tempC % var)
    else

!      deallocate(tempPextend%var)

    endif
!
!   Deallocations
#if defined AGRIF_MPI
    if (member) then
        call Agrif_nbdim_deallocation(tempP%var,nbdim)
!        deallocate(tempP % var)
    endif
#endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_InterpnD
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Parentbounds
!
!> Calculates the bounds of the parent grid for the interpolation of the child grid
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Parentbounds ( TypeInterp, nbdim, indmin, indmax,  &
                                s_Parent_temp, s_Child_temp,        &
                                s_Child, ds_Child,                  &
                                s_Parent,ds_Parent,                 &
                                pttruetab, cetruetab,               &
                                pttab_Child, pttab_Parent, posvar, interptab )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(6),     intent(in)   :: TypeInterp
    INTEGER,                   intent(in)   :: nbdim
    INTEGER, DIMENSION(nbdim), intent(out)  :: indmin, indmax
    REAL,    DIMENSION(nbdim), intent(out)  :: s_Parent_temp, s_child_temp
    REAL,    DIMENSION(nbdim), intent(in)   :: s_Child, ds_child
    REAL,    DIMENSION(nbdim), intent(in)   :: s_Parent,ds_Parent
    INTEGER, DIMENSION(nbdim), intent(in)   :: pttruetab, cetruetab
    INTEGER, DIMENSION(nbdim), intent(in)   :: pttab_Child, pttab_Parent
    INTEGER, DIMENSION(nbdim), intent(in)   :: posvar
    CHARACTER(6), DIMENSION(nbdim), intent(in)  :: interptab
!
    INTEGER :: i
    REAL,DIMENSION(nbdim) :: dim_newmin, dim_newmax
!
    dim_newmin = s_Child + (pttruetab - pttab_Child) * ds_Child
    dim_newmax = s_Child + (cetruetab - pttab_Child) * ds_Child
!
    do i = 1,nbdim
!
        indmin(i) = pttab_Parent(i) + agrif_int((dim_newmin(i)-s_Parent(i))/ds_Parent(i))
        indmax(i) = pttab_Parent(i) + agrif_ceiling((dim_newmax(i)-s_Parent(i))/ds_Parent(i))
!
!       Necessary for the Quadratic interpolation
!
        if ( (pttruetab(i) == cetruetab(i)) .and. (posvar(i) == 1) ) then
        elseif (interptab(i) == 'N') then
        elseif ( (TypeInterp(i) == Agrif_ppm) .or.    &
                 (TypeInterp(i) == Agrif_eno) .or.    &
                 (TypeInterp(i) == Agrif_weno) ) then
            indmin(i) = indmin(i) - 2
            indmax(i) = indmax(i) + 2
        elseif ( (TypeInterp(i) /= Agrif_constant) .and.  &
                 (TypeInterp(i) /= Agrif_linear) ) then
            indmin(i) = indmin(i) - 1
            indmax(i) = indmax(i) + 1
        endif
!
    enddo
!
    s_Parent_temp = s_Parent + (indmin - pttab_Parent) * ds_Parent
    s_Child_temp  = s_Child + (pttruetab - pttab_Child) * ds_Child
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Parentbounds
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_1D_Recursive
!
!> Subroutine for the interpolation of a 1D grid variable.
!> It calls Agrif_InterpBase.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_1D_recursive ( TypeInterp, tabin, tabout, indmin, indmax,   &
                                       pttab_child, petab_child,                    &
                                       s_child, s_parent, ds_child, ds_parent, nbdim )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(1)                           :: TypeInterp
    INTEGER                                         :: nbdim
    INTEGER, DIMENSION(nbdim)                       :: indmin, indmax
    INTEGER, DIMENSION(nbdim)                       :: pttab_child, petab_child
    REAL, DIMENSION(nbdim)                          :: s_child, s_parent
    REAL, DIMENSION(nbdim)                          :: ds_child, ds_parent
    REAL, DIMENSION(                                &
        indmin(nbdim):indmax(nbdim)                 &
    ), INTENT(IN)                                   :: tabin
    REAL, DIMENSION(                                &
        pttab_child(nbdim):petab_child(nbdim)       &
    ), INTENT(OUT)                                  :: tabout
!
    call Agrif_InterpBase(TypeInterp(1),                    &
            tabin(indmin(nbdim):indmax(nbdim)),             &
            tabout(pttab_child(nbdim):petab_child(nbdim)),  &
            indmin(nbdim),indmax(nbdim),                    &
            pttab_child(nbdim),petab_child(nbdim),          &
            s_parent(nbdim),s_child(nbdim),                 &
            ds_parent(nbdim),ds_child(nbdim))
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_1D_recursive
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_2D_Recursive
!
!> Subroutine for the interpolation of a 2D grid variable.
!> It calls Agrif_Interp_1D_recursive and Agrif_InterpBase.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_2D_recursive ( TypeInterp, tabin, tabout, indmin, indmax,   &
                                       pttab_child, petab_child,                    &
                                       s_child, s_parent, ds_child, ds_parent, nbdim )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(2)                           :: TypeInterp
    INTEGER                                         :: nbdim
    INTEGER, DIMENSION(nbdim)                       :: indmin, indmax
    INTEGER, DIMENSION(nbdim)                       :: pttab_child, petab_child
    REAL, DIMENSION(nbdim)                          :: s_child, s_parent
    REAL, DIMENSION(nbdim)                          :: ds_child, ds_parent
    REAL, DIMENSION(                                &
        indmin(nbdim-1):indmax(nbdim-1),            &
        indmin(nbdim):indmax(nbdim)                 &
    ), INTENT(IN)                                   :: tabin
    REAL, DIMENSION(                                &
        pttab_child(nbdim-1):petab_child(nbdim-1),  &
        pttab_child(nbdim):petab_child(nbdim)       &
    ), INTENT(OUT)                                  :: tabout
!
    REAL, DIMENSION(                                &
        pttab_child(nbdim-1):petab_child(nbdim-1),  &
        indmin(nbdim):indmax(nbdim))                :: tabtemp
    REAL, DIMENSION(                                &
        pttab_child(nbdim):petab_child(nbdim),      &
        pttab_child(nbdim-1):petab_child(nbdim-1))  :: tabout_trsp
    REAL, DIMENSION(                                &
        indmin(nbdim):indmax(nbdim),                &
        pttab_child(nbdim-1):petab_child(nbdim-1))  :: tabtemp_trsp
    INTEGER :: i,j,coeffraf
!
!     Commentaire perso : nbdim vaut toujours 2 ici.
!
    coeffraf = nint ( ds_parent(1) / ds_child(1) )
!
    if ((TypeInterp(1) == Agrif_Linear) .and. (coeffraf /= 1)) then
!---CDIR NEXPAND
        if(.NOT. precomputedone(1))     &
            call Linear1dPrecompute2d(                  &
                    indmax(2)-indmin(2)+1,              &
                    indmax(1)-indmin(1)+1,              &
                    petab_child(1)-pttab_child(1)+1,    &
                    s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
!---CDIR NEXPAND
        call Linear1dAfterCompute(tabin,tabtemp,size(tabin),size(tabtemp),1)
!
    elseif ((TypeInterp(1) == Agrif_PPM) .and. (coeffraf /= 1)) then
!---CDIR NEXPAND
        if(.NOT. precomputedone(1))     &
            call PPM1dPrecompute2d(                     &
                    indmax(2)-indmin(2)+1,              &
                    indmax(1)-indmin(1)+1,              &
                    petab_child(1)-pttab_child(1)+1,        &
                    s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
!---CDIR NEXPAND
        call PPM1dAfterCompute(tabin,tabtemp,size(tabin),size(tabtemp),1)
    else
        do j = indmin(nbdim),indmax(nbdim)
!
!---CDIR NEXPAND
            call Agrif_Interp_1D_recursive(TypeInterp(1),                   &
                    tabin(indmin(nbdim-1):indmax(nbdim-1),j),               &
                    tabtemp(pttab_child(nbdim-1):petab_child(nbdim-1),j),   &
                    indmin(1:nbdim-1),indmax(1:nbdim-1),                    &
                    pttab_child(1:nbdim-1),petab_child(1:nbdim-1),          &
                    s_child(1:nbdim-1), s_parent(1:nbdim-1),                &
                    ds_child(1:nbdim-1),ds_parent(1:nbdim-1),nbdim-1)
!
        enddo
    endif

    coeffraf = nint(ds_parent(nbdim)/ds_child(nbdim))
    tabtemp_trsp = TRANSPOSE(tabtemp)

    if ((TypeInterp(2) == Agrif_Linear) .and. (coeffraf /= 1)) then
!---CDIR NEXPAND
        if(.NOT. precomputedone(2))     &
            call Linear1dPrecompute2d(                  &
                    petab_child(1)-pttab_child(1)+1,    &
                    indmax(2)-indmin(2)+1,              &
                    petab_child(2)-pttab_child(2)+1,    &
                    s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)
!---CDIR NEXPAND
        call Linear1dAfterCompute(tabtemp_trsp,tabout_trsp, &
                size(tabtemp_trsp),size(tabout_trsp),2)

    elseif ((TypeInterp(2) == Agrif_PPM) .and. (coeffraf /= 1)) then
!---CDIR NEXPAND
        if(.NOT. precomputedone(2))     &
            call PPM1dPrecompute2d(                     &
                    petab_child(1)-pttab_child(1)+1,    &
                    indmax(2)-indmin(2)+1,              &
                    petab_child(2)-pttab_child(2)+1,    &
                    s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)
!---CDIR NEXPAND
        call PPM1dAfterCompute(tabtemp_trsp, tabout_trsp,    &
                               size(tabtemp_trsp), size(tabout_trsp), 2)
    else
        do i=pttab_child(nbdim-1),petab_child(nbdim-1)
!
!---CDIR NEXPAND
            call Agrif_InterpBase(TypeInterp(2),                            &
                    tabtemp_trsp(indmin(nbdim):indmax(nbdim),i),            &
                    tabout_trsp(pttab_child(nbdim):petab_child(nbdim),i),   &
                    indmin(nbdim),indmax(nbdim),                            &
                    pttab_child(nbdim),petab_child(nbdim),                  &
                    s_parent(nbdim), s_child(nbdim),                        &
                   ds_parent(nbdim),ds_child(nbdim))

        enddo
    endif
!
    tabout = TRANSPOSE(tabout_trsp)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_2D_recursive
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_3D_Recursive
!
!> Subroutine for the interpolation of a 3D grid variable.
!> It calls #Agrif_Interp_2D_recursive and #Agrif_InterpBase.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_3D_recursive ( TypeInterp, tabin, tabout, indmin, indmax, &
                                       pttab_child, petab_child,                  &
                                       s_child, s_parent, ds_child, ds_parent, nbdim )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(3)                           :: TypeInterp
    INTEGER                                         :: nbdim
    INTEGER, DIMENSION(nbdim)                       :: indmin, indmax
    INTEGER, DIMENSION(nbdim)                       :: pttab_child, petab_child
    REAL, DIMENSION(nbdim)                          :: s_child, s_parent
    REAL, DIMENSION(nbdim)                          :: ds_child, ds_parent
    REAL, DIMENSION(                                &
        indmin(nbdim-2):indmax(nbdim-2),            &
        indmin(nbdim-1):indmax(nbdim-1),            &
        indmin(nbdim):indmax(nbdim)                 &
    ), INTENT(IN)                                   :: tabin
    REAL, DIMENSION(                                &
        pttab_child(nbdim-2):petab_child(nbdim-2),  &
        pttab_child(nbdim-1):petab_child(nbdim-1),  &
        pttab_child(nbdim):petab_child(nbdim)       &
    ), INTENT(OUT)                                  :: tabout
!
    REAL, DIMENSION(                                &
        pttab_child(nbdim-2):petab_child(nbdim-2),  &
        pttab_child(nbdim-1):petab_child(nbdim-1),  &
        indmin(nbdim):indmax(nbdim))                :: tabtemp
    INTEGER :: i, j, k, coeffraf
    INTEGER :: locind_child_left, kdeb
!
    coeffraf = nint ( ds_parent(1) / ds_child(1) )
    if ( (TypeInterp(1) == Agrif_Linear) .and. (coeffraf/=1) ) then
        call Linear1dPrecompute2d(indmax(2)-indmin(2)+1,            &
                                  indmax(1)-indmin(1)+1,            &
                                  petab_child(1)-pttab_child(1)+1,  &
                                  s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
        precomputedone(1) = .TRUE.
    elseif ( (TypeInterp(1) == Agrif_PPM) .and. (coeffraf/=1) ) then
        call PPM1dPrecompute2d(indmax(2)-indmin(2)+1,           &
                               indmax(1)-indmin(1)+1,           &
                               petab_child(1)-pttab_child(1)+1, &
                               s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
        precomputedone(1) = .TRUE.
    endif

    coeffraf = nint ( ds_parent(2) / ds_child(2) )
    if ( (TypeInterp(2) == Agrif_Linear) .and. (coeffraf/=1) ) then
        call Linear1dPrecompute2d(petab_child(1)-pttab_child(1)+1,  &
                                  indmax(2)-indmin(2)+1,            &
                                  petab_child(2)-pttab_child(2)+1,  &
                                  s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)
        precomputedone(2) = .TRUE.
    elseif ( (TypeInterp(2) == Agrif_PPM) .and. (coeffraf/=1) ) then
        call PPM1dPrecompute2d(petab_child(1)-pttab_child(1)+1, &
                               indmax(2)-indmin(2)+1,           &
                               petab_child(2)-pttab_child(2)+1, &
                               s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)
        precomputedone(2) = .TRUE.
    endif
!
    do k = indmin(nbdim),indmax(nbdim)
        call Agrif_Interp_2D_recursive(TypeInterp(1:2),                                     &
                                       tabin(indmin(nbdim-2):indmax(nbdim-2),               &
                                             indmin(nbdim-1):indmax(nbdim-1),k),            &
                                       tabtemp(pttab_child(nbdim-2):petab_child(nbdim-2),   &
                                               pttab_child(nbdim-1):petab_child(nbdim-1),k),&
                                       indmin(1:nbdim-1),indmax(1:nbdim-1),                 &
                                       pttab_child(1:nbdim-1),petab_child(1:nbdim-1),       &
                                       s_child(1:nbdim-1),s_parent(1:nbdim-1),              &
                                       ds_child(1:nbdim-1),ds_parent(1:nbdim-1),nbdim-1)
    enddo
!
    precomputedone(1) = .FALSE.
    precomputedone(2) = .FALSE.
    coeffraf = nint ( ds_parent(3) / ds_child(3) )
!
    call Agrif_Compute_nbdim_interp(s_parent(nbdim), s_child(nbdim),    &
                                   ds_parent(nbdim),ds_child(nbdim),    &
                                   coeffraf,locind_child_left)
    if (coeffraf == 1) then
        kdeb = indmin(3)+locind_child_left-2
        do k = pttab_child(3),petab_child(3)
            kdeb = kdeb + 1
            do j = pttab_child(2),petab_child(2)
            do i = pttab_child(1),petab_child(1)
                tabout(i,j,k) = tabtemp(i,j,kdeb)
            enddo
            enddo
        enddo
    else
        do j = pttab_child(nbdim-1),petab_child(nbdim-1)
        do i = pttab_child(nbdim-2),petab_child(nbdim-2)
            call Agrif_InterpBase(TypeInterp(3),                                    &
                                  tabtemp(i,j,indmin(nbdim):indmax(nbdim)),         &
                                  tabout(i,j,pttab_child(nbdim):petab_child(nbdim)),&
                                  indmin(nbdim),indmax(nbdim),                      &
                                  pttab_child(nbdim),petab_child(nbdim),            &
                                  s_parent(nbdim), s_child(nbdim),                  &
                                 ds_parent(nbdim),ds_child(nbdim))
        enddo
        enddo
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_3D_recursive
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_4D_Recursive
!
!> Subroutine for the interpolation of a 4D grid variable.
!> It calls #Agrif_Interp_3D_recursive and #Agrif_InterpBase.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_4D_recursive ( TypeInterp, tabin, tabout, indmin, indmax, &
                                       pttab_child, petab_child,                  &
                                       s_child, s_parent, ds_child, ds_parent, nbdim )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(4)                           :: TypeInterp
    INTEGER                                         :: nbdim
    INTEGER, DIMENSION(nbdim)                       :: indmin, indmax
    INTEGER, DIMENSION(nbdim)                       :: pttab_child, petab_child
    REAL, DIMENSION(nbdim)                          :: s_child, s_parent
    REAL, DIMENSION(nbdim)                          :: ds_child, ds_parent
    REAL, DIMENSION(                                &
        indmin(nbdim-3):indmax(nbdim-3),            &
        indmin(nbdim-2):indmax(nbdim-2),            &
        indmin(nbdim-1):indmax(nbdim-1),            &
        indmin(nbdim):indmax(nbdim)                 &
    ), INTENT(IN)                                   :: tabin
    REAL, DIMENSION(                                &
        pttab_child(nbdim-3):petab_child(nbdim-3),  &
        pttab_child(nbdim-2):petab_child(nbdim-2),  &
        pttab_child(nbdim-1):petab_child(nbdim-1),  &
        pttab_child(nbdim):petab_child(nbdim)       &
    ), INTENT(OUT)                                  :: tabout
!
    REAL, DIMENSION(                                &
        pttab_child(nbdim-3):petab_child(nbdim-3),  &
        pttab_child(nbdim-2):petab_child(nbdim-2),  &
        pttab_child(nbdim-1):petab_child(nbdim-1),  &
        indmin(nbdim):indmax(nbdim))                :: tabtemp
    INTEGER :: i, j, k, l
!
    do l = indmin(nbdim),indmax(nbdim)
        call Agrif_Interp_3D_recursive(TypeInterp(1:3),                                     &
                                       tabin(indmin(nbdim-3):indmax(nbdim-3),               &
                                             indmin(nbdim-2):indmax(nbdim-2),               &
                                             indmin(nbdim-1):indmax(nbdim-1),l),            &
                                       tabtemp(pttab_child(nbdim-3):petab_child(nbdim-3),   &
                                               pttab_child(nbdim-2):petab_child(nbdim-2),   &
                                               pttab_child(nbdim-1):petab_child(nbdim-1),l),&
                                       indmin(1:nbdim-1),indmax(1:nbdim-1),                 &
                                       pttab_child(1:nbdim-1),petab_child(1:nbdim-1),       &
                                       s_child(1:nbdim-1),s_parent(1:nbdim-1),              &
                                       ds_child(1:nbdim-1),ds_parent(1:nbdim-1),nbdim-1)
    enddo
!
    do k = pttab_child(nbdim-1),petab_child(nbdim-1)
    do j = pttab_child(nbdim-2),petab_child(nbdim-2)
    do i = pttab_child(nbdim-3),petab_child(nbdim-3)
        call Agrif_InterpBase(TypeInterp(4),                                        &
                              tabtemp(i,j,k,indmin(nbdim):indmax(nbdim)),           &
                              tabout(i,j,k,pttab_child(nbdim):petab_child(nbdim)),  &
                              indmin(nbdim),indmax(nbdim),                          &
                              pttab_child(nbdim),petab_child(nbdim),                &
                              s_parent(nbdim), s_child(nbdim),                      &
                             ds_parent(nbdim),ds_child(nbdim))
    enddo
    enddo
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_4D_recursive
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_5D_Recursive
!
!> Subroutine for the interpolation of a 5D grid variable.
!> It calls #Agrif_Interp_4D_recursive and #Agrif_InterpBase.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_5D_recursive ( TypeInterp, tabin, tabout, indmin, indmax, &
                                       pttab_child, petab_child,                  &
                                       s_child, s_parent, ds_child, ds_parent, nbdim )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(5)                           :: TypeInterp
    INTEGER                                         :: nbdim
    INTEGER, DIMENSION(nbdim)                       :: indmin, indmax
    INTEGER, DIMENSION(nbdim)                       :: pttab_child, petab_child
    REAL, DIMENSION(nbdim)                          :: s_child, s_parent
    REAL, DIMENSION(nbdim)                          :: ds_child, ds_parent
    REAL, DIMENSION(                                &
        indmin(nbdim-4):indmax(nbdim-4),            &
        indmin(nbdim-3):indmax(nbdim-3),            &
        indmin(nbdim-2):indmax(nbdim-2),            &
        indmin(nbdim-1):indmax(nbdim-1),            &
        indmin(nbdim):indmax(nbdim)                 &
    ), INTENT(IN)                                   :: tabin
    REAL, DIMENSION(                                &
        pttab_child(nbdim-4):petab_child(nbdim-4),  &
        pttab_child(nbdim-3):petab_child(nbdim-3),  &
        pttab_child(nbdim-2):petab_child(nbdim-2),  &
        pttab_child(nbdim-1):petab_child(nbdim-1),  &
        pttab_child(nbdim):petab_child(nbdim)       &
    ), INTENT(OUT)                                  :: tabout
!
    REAL, DIMENSION(                                &
        pttab_child(nbdim-4):petab_child(nbdim-4),  &
        pttab_child(nbdim-3):petab_child(nbdim-3),  &
        pttab_child(nbdim-2):petab_child(nbdim-2),  &
        pttab_child(nbdim-1):petab_child(nbdim-1),  &
        indmin(nbdim):indmax(nbdim))                :: tabtemp
    INTEGER :: i, j, k, l, m
!
    do m = indmin(nbdim),indmax(nbdim)
        call Agrif_Interp_4D_recursive(TypeInterp(1:4),                                     &
                                       tabin(indmin(nbdim-4):indmax(nbdim-4),               &
                                             indmin(nbdim-3):indmax(nbdim-3),               &
                                             indmin(nbdim-2):indmax(nbdim-2),               &
                                             indmin(nbdim-1):indmax(nbdim-1),m),            &
                                       tabtemp(pttab_child(nbdim-4):petab_child(nbdim-4),   &
                                               pttab_child(nbdim-3):petab_child(nbdim-3),   &
                                               pttab_child(nbdim-2):petab_child(nbdim-2),   &
                                               pttab_child(nbdim-1):petab_child(nbdim-1),m),&
                                       indmin(1:nbdim-1),indmax(1:nbdim-1),                 &
                                       pttab_child(1:nbdim-1),petab_child(1:nbdim-1),       &
                                       s_child(1:nbdim-1),s_parent(1:nbdim-1),              &
                                       ds_child(1:nbdim-1),ds_parent(1:nbdim-1),nbdim-1)
    enddo
!
    do l = pttab_child(nbdim-1),petab_child(nbdim-1)
    do k = pttab_child(nbdim-2),petab_child(nbdim-2)
    do j = pttab_child(nbdim-3),petab_child(nbdim-3)
    do i = pttab_child(nbdim-4),petab_child(nbdim-4)
        call Agrif_InterpBase(TypeInterp(5),                                        &
                              tabtemp(i,j,k,l,indmin(nbdim):indmax(nbdim)),         &
                              tabout(i,j,k,l,pttab_child(nbdim):petab_child(nbdim)),&
                              indmin(nbdim),indmax(nbdim),                          &
                              pttab_child(nbdim),petab_child(nbdim),                &
                              s_parent(nbdim),s_child(nbdim),                       &
                             ds_parent(nbdim),ds_child(nbdim))
    enddo
    enddo
    enddo
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_5D_recursive
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_6D_Recursive
!
!> Subroutine for the interpolation of a 6D grid variable.
!> It calls #Agrif_Interp_5D_recursive and Agrif_InterpBase.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_6D_recursive(TypeInterp,tabin,tabout,&
           indmin,indmax,&
           pttab_child,petab_child,&
           s_child,s_parent,ds_child,ds_parent,nbdim)
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(6)                           :: TypeInterp
    INTEGER                                         :: nbdim
    INTEGER, DIMENSION(nbdim)                       :: indmin, indmax
    INTEGER, DIMENSION(nbdim)                       :: pttab_child, petab_child
    REAL, DIMENSION(nbdim)                          :: s_child, s_parent
    REAL, DIMENSION(nbdim)                          :: ds_child, ds_parent
    REAL, DIMENSION(                                &
        indmin(nbdim-5):indmax(nbdim-5),            &
        indmin(nbdim-4):indmax(nbdim-4),            &
        indmin(nbdim-3):indmax(nbdim-3),            &
        indmin(nbdim-2):indmax(nbdim-2),            &
        indmin(nbdim-1):indmax(nbdim-1),            &
        indmin(nbdim):indmax(nbdim)                 &
    ), INTENT(IN)                                   :: tabin
    REAL, DIMENSION(                                &
        pttab_child(nbdim-5):petab_child(nbdim-5),  &
        pttab_child(nbdim-4):petab_child(nbdim-4),  &
        pttab_child(nbdim-3):petab_child(nbdim-3),  &
        pttab_child(nbdim-2):petab_child(nbdim-2),  &
        pttab_child(nbdim-1):petab_child(nbdim-1),  &
        pttab_child(nbdim):petab_child(nbdim)       &
    ), INTENT(OUT)                                  :: tabout
!
    REAL, DIMENSION(                                &
        pttab_child(nbdim-5):petab_child(nbdim-5),  &
        pttab_child(nbdim-4):petab_child(nbdim-4),  &
        pttab_child(nbdim-3):petab_child(nbdim-3),  &
        pttab_child(nbdim-2):petab_child(nbdim-2),  &
        pttab_child(nbdim-1):petab_child(nbdim-1),  &
        indmin(nbdim):indmax(nbdim))                :: tabtemp
    INTEGER :: i, j, k, l, m, n
!
    do n = indmin(nbdim),indmax(nbdim)
        call Agrif_Interp_5D_recursive(TypeInterp(1:5),                                 &
                                       tabin(indmin(nbdim-5):indmax(nbdim-5),           &
                                        indmin(nbdim-4):indmax(nbdim-4),                &
                                        indmin(nbdim-3):indmax(nbdim-3),                &
                                        indmin(nbdim-2):indmax(nbdim-2),                &
                                        indmin(nbdim-1):indmax(nbdim-1),n),             &
                                  tabtemp(pttab_child(nbdim-5):petab_child(nbdim-5),    &
                                          pttab_child(nbdim-4):petab_child(nbdim-4),    &
                                          pttab_child(nbdim-3):petab_child(nbdim-3),    &
                                          pttab_child(nbdim-2):petab_child(nbdim-2),    &
                                          pttab_child(nbdim-1):petab_child(nbdim-1),n), &
                                  indmin(1:nbdim-1),indmax(1:nbdim-1),                  &
                                  pttab_child(1:nbdim-1),petab_child(1:nbdim-1),        &
                                  s_child(1:nbdim-1), s_parent(1:nbdim-1),              &
                                 ds_child(1:nbdim-1),ds_parent(1:nbdim-1),nbdim-1)
    enddo
!
    do m = pttab_child(nbdim-1),petab_child(nbdim-1)
    do l = pttab_child(nbdim-2),petab_child(nbdim-2)
    do k = pttab_child(nbdim-3),petab_child(nbdim-3)
    do j = pttab_child(nbdim-4),petab_child(nbdim-4)
    do i = pttab_child(nbdim-5),petab_child(nbdim-5)
        call Agrif_InterpBase(TypeInterp(6),                                            &
                              tabtemp(i,j,k,l,m,indmin(nbdim):indmax(nbdim)),           &
                              tabout(i,j,k,l,m,pttab_child(nbdim):petab_child(nbdim)),  &
                              indmin(nbdim),indmax(nbdim),                              &
                              pttab_child(nbdim),petab_child(nbdim),                    &
                              s_parent(nbdim), s_child(nbdim),                          &
                             ds_parent(nbdim),ds_child(nbdim))
    enddo
    enddo
    enddo
    enddo
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_6D_recursive
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_InterpBase
!
!> Calls the interpolation method chosen by the user (linear, lagrange, spline, etc.).
!---------------------------------------------------------------------------------------------------
subroutine Agrif_InterpBase ( TypeInterp, parenttab, childtab, indmin, indmax,  &
                              pttab_child, petab_child,                         &
                              s_parent, s_child, ds_parent, ds_child )
!---------------------------------------------------------------------------------------------------
    INTEGER                                                 :: TypeInterp
    INTEGER                                                 :: indmin, indmax
    INTEGER                                                 :: pttab_child, petab_child
    REAL, DIMENSION(indmin:indmax),           INTENT(IN)    :: parenttab
    REAL, DIMENSION(pttab_child:petab_child), INTENT(OUT)   :: childtab
    REAL                                                    :: s_parent, s_child
    REAL                                                    :: ds_parent,ds_child
!
    if ( (indmin == indmax) .and. (pttab_child == petab_child) ) then
!
        childtab(pttab_child) = parenttab(indmin)
!
    elseif (TypeInterp == Agrif_LINEAR) then    !       Linear interpolation
!
        call linear1D(parenttab,childtab,                   &
                indmax-indmin+1,petab_child-pttab_child+1,  &
                s_parent,s_child,ds_parent,ds_child)
!
    elseif ( TypeInterp == Agrif_PPM ) then     !       PPM interpolation

        call PPM1d(parenttab,childtab,                      &
                indmax-indmin+1,petab_child-pttab_child+1,  &
                s_parent,s_child,ds_parent,ds_child)
!
    elseif (TypeInterp == Agrif_LAGRANGE) then  !       Lagrange interpolation
!
        call lagrange1D(parenttab,childtab,                 &
                indmax-indmin+1,petab_child-pttab_child+1,  &
                s_parent,s_child,ds_parent,ds_child)
!
    elseif (TypeInterp == Agrif_ENO) then       !       Eno interpolation
!
        call ENO1d(parenttab,childtab,                      &
                indmax-indmin+1,petab_child-pttab_child+1,  &
                s_parent,s_child,ds_parent,ds_child)
!
    elseif (TypeInterp == Agrif_WENO) then      !       Weno interpolation
!
        call WENO1d(parenttab,childtab,                     &
                indmax-indmin+1,petab_child-pttab_child+1,  &
                s_parent,s_child,ds_parent,ds_child)
!
    elseif (TypeInterp == Agrif_LINEARCONSERV) then !   Linear conservative interpolation
!
        call Linear1dConserv(parenttab,childtab,            &
                indmax-indmin+1,petab_child-pttab_child+1,  &
                s_parent,s_child,ds_parent,ds_child)
!
    elseif (TypeInterp == Agrif_LINEARCONSERVLIM) then !Linear conservative interpolation
!
        call Linear1dConservLim(parenttab,childtab,         &
                indmax-indmin+1,petab_child-pttab_child+1,  &
                s_parent,s_child,ds_parent,ds_child)
!
    elseif (TypeInterp == Agrif_CONSTANT) then
!
        call Constant1d(parenttab,childtab,                 &
                indmax-indmin+1,petab_child-pttab_child+1,  &
                s_parent,s_child,ds_parent,ds_child)
!
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_InterpBase
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Compute_nbdim_interp
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Compute_nbdim_interp ( s_parent, s_child, ds_parent, ds_child,     &
                                        coeffraf, locind_child_left )
!---------------------------------------------------------------------------------------------------
    real,    intent(in)     ::  s_parent, s_child
    real,    intent(in)     :: ds_parent,ds_child
    integer, intent(out)    :: coeffraf
    integer, intent(out)    :: locind_child_left
!
    coeffraf = nint(ds_parent/ds_child)
    locind_child_left = 1 + agrif_int((s_child-s_parent)/ds_parent)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Compute_nbdim_interp
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Find_list_interp
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Find_list_interp ( list_interp, pttab, petab, pttab_Child, pttab_Parent,   &
                                    nbdim, indmin, indmax, indminglob,  indmaxglob,         &
                                                           indminglob2, indmaxglob2,        &
                                    parentarray, pttruetab, cetruetab, member, memberin,    &
                                    find_list_interp                                        &
#if defined AGRIF_MPI
                                    ,tab4t,memberinall,sendtoproc1,recvfromproc1            &
#endif
    )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_List_Interp_Loc),   pointer     :: list_interp
    INTEGER,                       intent(in)  :: nbdim
    INTEGER, DIMENSION(nbdim),     intent(in)  :: pttab, petab, pttab_Child, pttab_Parent
    INTEGER, DIMENSION(nbdim),     intent(out) :: indmin, indmax
    INTEGER, DIMENSION(nbdim),     intent(out) :: indminglob, indmaxglob
    INTEGER, DIMENSION(nbdim),     intent(out) :: indminglob2, indmaxglob2
    INTEGER, DIMENSION(nbdim,2,2), intent(out) :: parentarray
    INTEGER, DIMENSION(nbdim),     intent(out) :: pttruetab, cetruetab
    LOGICAL,                       intent(out) :: member, memberin
    LOGICAL,                       intent(out) :: find_list_interp
#if defined AGRIF_MPI
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1,8), intent(out) :: tab4t
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1),         intent(out) :: memberinall
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1),         intent(out) :: sendtoproc1, recvfromproc1
#endif
!
    INTEGER :: i
    Type(Agrif_List_Interp_Loc), pointer :: parcours

    find_list_interp = .FALSE.

    parcours => list_interp
    Find_loop : do while (associated(parcours))
        do i=1,nbdim
            if ((pttab(i) /= parcours%interp_loc%pttab(i)) .OR. &
                (petab(i) /= parcours%interp_loc%petab(i)) .OR. &
                (pttab_child(i)  /= parcours%interp_loc%pttab_child(i)).OR. &
                (pttab_parent(i) /= parcours%interp_loc%pttab_parent(i))) then
                parcours => parcours%suiv
                cycle Find_loop
            endif
        enddo

        indmin = parcours%interp_loc%indmin(1:nbdim)
        indmax = parcours%interp_loc%indmax(1:nbdim)

        pttruetab = parcours%interp_loc%pttruetab(1:nbdim)
        cetruetab = parcours%interp_loc%cetruetab(1:nbdim)

#if !defined AGRIF_MPI
        indminglob = parcours%interp_loc%indminglob(1:nbdim)
        indmaxglob = parcours%interp_loc%indmaxglob(1:nbdim)
#else
        indminglob = parcours%interp_loc%indminglob2(1:nbdim)
        indmaxglob = parcours%interp_loc%indmaxglob2(1:nbdim)
        indminglob2 = parcours%interp_loc%indminglob2(1:nbdim)
        indmaxglob2 = parcours%interp_loc%indmaxglob2(1:nbdim)
        parentarray = parcours%interp_loc%parentarray(1:nbdim,:,:)
        member = parcours%interp_loc%member
        tab4t = parcours%interp_loc%tab4t(1:nbdim,0:Agrif_Nbprocs-1,1:8)
        memberinall = parcours%interp_loc%memberinall(0:Agrif_Nbprocs-1)
        sendtoproc1 = parcours%interp_loc%sendtoproc1(0:Agrif_Nbprocs-1)
        recvfromproc1 = parcours%interp_loc%recvfromproc1(0:Agrif_Nbprocs-1)
#endif
        memberin = parcours%interp_loc%memberin
        find_list_interp = .TRUE.
        exit Find_loop
    enddo Find_loop
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Find_list_interp
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_AddTo_list_interp
!---------------------------------------------------------------------------------------------------
subroutine Agrif_AddTo_list_interp ( list_interp, pttab, petab, pttab_Child, pttab_Parent,  &
                                     indmin, indmax, indminglob, indmaxglob,                &
                                     indminglob2, indmaxglob2, parentarray,                 &
                                     pttruetab, cetruetab, member, memberin, nbdim          &
#if defined AGRIF_MPI
                                    ,tab4t, memberinall, sendtoproc1, recvfromproc1         &
#endif
    )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_List_Interp_Loc), pointer    :: list_interp
    INTEGER                                 :: nbdim
    INTEGER, DIMENSION(nbdim)               :: pttab, petab, pttab_Child, pttab_Parent
    INTEGER, DIMENSION(nbdim)               :: indmin,indmax
    INTEGER, DIMENSION(nbdim)               :: indminglob, indmaxglob
    INTEGER, DIMENSION(nbdim)               :: indminglob2,indmaxglob2
    INTEGER, DIMENSION(nbdim)               :: pttruetab, cetruetab
    INTEGER, DIMENSION(nbdim,2,2)           :: parentarray
    LOGICAL                                 :: member, memberin
#if defined AGRIF_MPI
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1,8)   :: tab4t
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1)           :: memberinall
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1)           :: sendtoproc1
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1)           :: recvfromproc1
#endif
!
    Type(Agrif_List_Interp_Loc), Pointer :: parcours
!
    Allocate(parcours)
    Allocate(parcours%interp_loc)

    parcours%interp_loc%pttab(1:nbdim) = pttab(1:nbdim)
    parcours%interp_loc%petab(1:nbdim) = petab(1:nbdim)
    parcours%interp_loc%pttab_child(1:nbdim) = pttab_child(1:nbdim)
    parcours%interp_loc%pttab_parent(1:nbdim) = pttab_parent(1:nbdim)

    parcours%interp_loc%indmin(1:nbdim) = indmin(1:nbdim)
    parcours%interp_loc%indmax(1:nbdim) = indmax(1:nbdim)

    parcours%interp_loc%memberin = memberin
#if !defined AGRIF_MPI
    parcours%interp_loc%indminglob(1:nbdim) = indminglob(1:nbdim)
    parcours%interp_loc%indmaxglob(1:nbdim) = indmaxglob(1:nbdim)
#else
    parcours%interp_loc%indminglob2(1:nbdim) = indminglob2(1:nbdim)
    parcours%interp_loc%indmaxglob2(1:nbdim) = indmaxglob2(1:nbdim)
    parcours%interp_loc%parentarray(1:nbdim,:,:) = parentarray(1:nbdim,:,:)
    parcours%interp_loc%member = member
    Allocate(parcours%interp_loc%tab4t(nbdim,0:Agrif_Nbprocs-1,8))
    Allocate(parcours%interp_loc%memberinall(0:Agrif_Nbprocs-1))
    Allocate(parcours%interp_loc%sendtoproc1(0:Agrif_Nbprocs-1))
    Allocate(parcours%interp_loc%recvfromproc1(0:Agrif_Nbprocs-1))
    parcours%interp_loc%tab4t=tab4t
    parcours%interp_loc%memberinall=memberinall
    parcours%interp_loc%sendtoproc1=sendtoproc1
    parcours%interp_loc%recvfromproc1=recvfromproc1
#endif

    parcours%interp_loc%pttruetab(1:nbdim) = pttruetab(1:nbdim)
    parcours%interp_loc%cetruetab(1:nbdim) = cetruetab(1:nbdim)

    parcours%suiv => list_interp
    list_interp => parcours
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Addto_list_interp
!===================================================================================================
!
end module Agrif_Interpolation
