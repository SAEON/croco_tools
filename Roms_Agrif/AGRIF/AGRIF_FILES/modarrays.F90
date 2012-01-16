!
! $Id: modarrays.F 662 2007-05-25 15:58:52Z opalod $
!
!     AGRIF (Adaptive Grid Refinement In Fortran)
!
!     Copyright (C) 2003 Laurent Debreu (Laurent.Debreu@imag.fr)
!                Christophe Vouland (Christophe.Vouland@imag.fr)
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
!> Module Agrif_Arrays
!
module Agrif_Arrays
!
    use Agrif_Types
!
    implicit none
!
contains
!
!===================================================================================================
! subroutine Agrif_Childbounds
!
!> Subroutine calculating the global indices of the child grid
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Childbounds ( nbdim, lboundloc, uboundloc, &
                               pttab, petab, pttruetab, cetruetab, memberin )
!---------------------------------------------------------------------------------------------------
    INTEGER,                  intent(in)  :: nbdim
    INTEGER,DIMENSION(nbdim), intent(in)  :: lboundloc, uboundloc
    INTEGER,DIMENSION(nbdim), intent(in)  :: pttab, petab
    INTEGER,DIMENSION(nbdim), intent(out) :: pttruetab, cetruetab
    LOGICAL,                  intent(out) :: memberin
!
    INTEGER :: i,lbglob,ubglob
!
#if defined AGRIF_MPI
    INTEGER :: indglob1, indglob2
#endif
!
    do i = 1,nbdim
!
        lbglob = lboundloc(i)
        ubglob = uboundloc(i)
!
#if defined AGRIF_MPI
        call Agrif_InvLoc(lbglob,Agrif_ProcRank,i,indglob1)
        call Agrif_InvLoc(ubglob,Agrif_ProcRank,i,indglob2)
!
        pttruetab(i) = max(pttab(i),indglob1)
        cetruetab(i) = min(petab(i),indglob2)
#else
        pttruetab(i) = max(pttab(i),lbglob)
        cetruetab(i) = min(petab(i),ubglob)
#endif
!
    enddo

    memberin = .TRUE.

    do i=1,nbdim
        if (cetruetab(i) < pttruetab(i)) then
            memberin = .FALSE.
            exit
        endif
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Childbounds
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_nbdim_Get_bound
!
!> Gets the lower and the upper boundaries of a table.
!> Output datas are scalar.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_nbdim_Get_bound ( Variable, lower, upper, indice )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_Variable), pointer :: Variable   !< we want extract boundaries of this table
    INTEGER, INTENT(out)          :: lower      !< output data
    INTEGER, INTENT(out)          :: upper      !< output data
    INTEGER, INTENT(in)           :: indice     !< direction in wich we want to know the dimension
!
    lower = Variable % lb(indice)
    upper = Variable % ub(indice)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_nbdim_Get_bound
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_nbdim_Get_bound_dimension
!
!> Gets the lower and the upper boundaries of a table.
!> Output datas are scalar.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_nbdim_Get_bound_dimension ( Variable, lower, upper, nbdim )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_Variable), pointer           :: Variable   !< we want extract boundaries of this table
    INTEGER,DIMENSION(nbdim), INTENT(out)   :: lower      !< output data
    INTEGER,DIMENSION(nbdim), INTENT(out)   :: upper      !< output data
    INTEGER, INTENT(in)                     :: nbdim      !< dimension of the table
!
    lower = Variable % lb(1:nbdim)
    upper = Variable % ub(1:nbdim)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_nbdim_Get_bound_dimension
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_nbdim_allocation
!
!> Allocates the table Variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_nbdim_allocation ( Variable, inf, sup, nbdim )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_Variable),     pointer       :: Variable
    INTEGER,DIMENSION(nbdim), intent(in)    :: inf, sup
    INTEGER,                  intent(in)    :: nbdim     !< dimension of the table
!
    select case (nbdim)
    case (1)
        allocate(Variable%array1(inf(1):sup(1)))
    case (2)
        allocate(Variable%array2(inf(1):sup(1),    &
                                 inf(2):sup(2)))
    case (3)
        allocate(Variable%array3(inf(1):sup(1),    &
                                 inf(2):sup(2),    &
                                 inf(3):sup(3)))
    case (4)
        allocate(Variable%array4(inf(1):sup(1),    &
                                 inf(2):sup(2),    &
                                 inf(3):sup(3),    &
                                 inf(4):sup(4)))
    case (5)
        allocate(Variable%array5(inf(1):sup(1),    &
                                 inf(2):sup(2),    &
                                 inf(3):sup(3),    &
                                 inf(4):sup(4),    &
                                 inf(5):sup(5)))
    case (6)
        allocate(Variable%array6(inf(1):sup(1),    &
                                 inf(2):sup(2),    &
                                 inf(3):sup(3),    &
                                 inf(4):sup(4),    &
                                 inf(5):sup(5),    &
                                 inf(6):sup(6)))
    end select
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_nbdim_allocation
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_nbdim_deallocation
!
!> Dellocates the table Variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_nbdim_deallocation ( Variable, nbdim )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_Variable), pointer   :: Variable
    INTEGER, INTENT(in)             :: nbdim     !< dimension of the table
!
    select case (nbdim)
    case (1)
        deallocate(Variable%array1)
    case (2)
        deallocate(Variable%array2)
    case (3)
        deallocate(Variable%array3)
    case (4)
        deallocate(Variable%array4)
    case (5)
        deallocate(Variable%array5)
    case (6)
        deallocate(Variable%array6)
    end select
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_nbdim_deallocation
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_nbdim_Full_VarEQreal
!
!> This subroutine is used to give the same value to the table Variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_nbdim_Full_VarEQreal ( Variable, Value, nbdim )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_Variable), pointer   :: Variable
    REAL,    INTENT(in)             :: Value    !< input value
    INTEGER, INTENT(in)             :: nbdim    !< dimension of the table
!
    select case (nbdim)
    case (1)
        Variable%array1 = Value
    case (2)
        Variable%array2 = Value
    case (3)
        call Agrif_set_tozero3D(Variable%array3)
!      Variable%array3 = Value
    case (4)
        Variable%array4 = Value
    case (5)
        Variable%array5 = Value
    case (6)
        Variable%array6 = Value
    end select
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_nbdim_Full_VarEQreal
!===================================================================================================
!
!===================================================================================================
    subroutine Agrif_set_tozero3D(tab3D)
!---------------------------------------------------------------------------------------------------
    real, dimension(:,:,:), target :: tab3D
!
    tab3D = 0.
!---------------------------------------------------------------------------------------------------
end subroutine agrif_set_tozero3D
!===================================================================================================
!
#if !defined AGRIF_MPI
!===================================================================================================
!  subroutine Agrif_nbdim_VarEQreal
!
!> Gives the same value to a part of the table Variable
!---------------------------------------------------------------------------------------------------
    subroutine Agrif_nbdim_VarEQreal ( Variable, inf, sup, Value, nbdim )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_Variable), pointer           :: Variable
    INTEGER,                   intent(in)   :: nbdim     !< dimension of the table
    INTEGER, DIMENSION(nbdim), intent(in)   :: inf,sup
    REAL,                      intent(in)   :: Value
!
    select case (nbdim)
    case (1)
       Variable%array1( inf(1):sup(1) )  = Value
    case (2)
       Variable%array2( inf(1):sup(1),   &
                        inf(2):sup(2) )  = Value
    case (3)
       Variable%array3( inf(1):sup(1),   &
                        inf(2):sup(2),   &
                        inf(3):sup(3) )  = Value
    case (4)
       Variable%array4( inf(1):sup(1),   &
                        inf(2):sup(2),   &
                        inf(3):sup(3),   &
                        inf(4):sup(4) )  = Value
    case (5)
       Variable%array5( inf(1):sup(1),   &
                        inf(2):sup(2),   &
                        inf(3):sup(3),   &
                        inf(4):sup(4),   &
                        inf(5):sup(5) )  = Value
    case (6)
       Variable%array6( inf(1):sup(1),   &
                        inf(2):sup(2),   &
                        inf(3):sup(3),   &
                        inf(4):sup(4),   &
                        inf(5):sup(5),   &
                        inf(6):sup(6) )  = Value
    end select
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_nbdim_VarEQreal
!===================================================================================================
#endif
!
!===================================================================================================
!  subroutine Agrif_nbdim_VarEQvar
!
!> Gives the value of a part of the table Variable2 to the table Variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_nbdim_VarEQvar ( Variable, inf, sup,   &
                                  Variable2,inf2,sup2, nbdim )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_Variable), pointer           :: Variable
    TYPE(Agrif_Variable), pointer           :: Variable2
    INTEGER,                   intent(in)   :: nbdim     !< dimension of the table
    INTEGER, DIMENSION(nbdim), intent(in)   :: inf, sup
    INTEGER, DIMENSION(nbdim), intent(in)   :: inf2,sup2
!
    select case (nbdim)
    case (1)
        Variable%array1(inf(1):sup(1)) = Variable2%array1(inf2(1):sup2(1))
    case (2)
        call Agrif_Copy_2d(Variable%array2,Variable2%array2, &
                lbound(Variable%array2), &
                lbound(Variable2%array2), &
                inf,sup,inf2,sup2)
    case (3)
        call Agrif_Copy_3d(Variable%array3,Variable2%array3, &
                lbound(Variable%array3),  &
                lbound(Variable2%array3), &
                inf,sup,inf2,sup2)
    case (4)
        call Agrif_Copy_4d(Variable%array4,Variable2%array4, &
                lbound(Variable%array4),  &
                lbound(Variable2%array4), &
                inf,sup,inf2,sup2)
    case (5)
        Variable%array5(inf(1):sup(1),   &
                        inf(2):sup(2),   &
                        inf(3):sup(3),   &
                        inf(4):sup(4),   &
                        inf(5):sup(5)) = &
            Variable2%array5(inf2(1):sup2(1), &
                             inf2(2):sup2(2), &
                             inf2(3):sup2(3), &
                             inf2(4):sup2(4), &
                             inf2(5):sup2(5))
    case (6)
        Variable%array6(inf(1):sup(1),   &
                        inf(2):sup(2),   &
                        inf(3):sup(3),   &
                        inf(4):sup(4),   &
                        inf(5):sup(5),   &
                        inf(6):sup(6)) = &
            Variable2%array6(inf2(1):sup2(1), &
                             inf2(2):sup2(2), &
                             inf2(3):sup2(3), &
                             inf2(4):sup2(4), &
                             inf2(5):sup2(5), &
                             inf2(6):sup2(6))
    end select
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_nbdim_VarEQvar
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_nbdim_Full_VarEQvar
!
!> give the value of the table Variable2 to the table Variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_nbdim_Full_VarEQvar ( Variable, Variable2, nbdim )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_Variable), pointer     :: Variable
    TYPE(Agrif_Variable), pointer     :: Variable2
    INTEGER,              intent(in)  :: nbdim     !< dimension of the table
!
    SELECT CASE (nbdim)
    CASE (1) ; Variable%array1 = Variable2%array1
    CASE (2) ; Variable%array2 = Variable2%array2
    CASE (3) ; Variable%array3 = Variable2%array3
    CASE (4) ; Variable%array4 = Variable2%array4
    CASE (5) ; Variable%array5 = Variable2%array5
    CASE (6) ; Variable%array6 = Variable2%array6
    end SELECT
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_nbdim_Full_VarEQvar
!===================================================================================================
!
!===================================================================================================
!  subroutine GiveAgrif_SpecialValueToTab_mpi
!---------------------------------------------------------------------------------------------------
subroutine GiveAgrif_SpecialValueToTab_mpi ( Variable1, Variable2, bounds, Value, nbdim )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_Variable),      pointer      :: Variable1
    TYPE(Agrif_Variable),      pointer      :: Variable2
    INTEGER,                   intent(in)   :: nbdim
    INTEGER, DIMENSION(:,:,:), intent(in)   :: bounds
    REAL,                      intent(in)   :: Value
!
    select case (nbdim)
    case (1)
        where (Variable1 % array1( bounds(1,1,2):bounds(1,2,2)) == Value )
            Variable2 % array1(bounds(1,1,1):bounds(1,2,1)) = Value
        end where
    case (2)
        where (Variable1 % array2( bounds(1,1,2):bounds(1,2,2), &
                                   bounds(2,1,2):bounds(2,2,2)) == Value)
            Variable2 % array2(bounds(1,1,1):bounds(1,2,1), &
                               bounds(2,1,1):bounds(2,2,1)) = Value
        end where
    case (3)
        where (Variable1 % array3( bounds(1,1,2):bounds(1,2,2), &
                                   bounds(2,1,2):bounds(2,2,2), &
                                   bounds(3,1,2):bounds(3,2,2)) == Value)
            Variable2 % array3(bounds(1,1,1):bounds(1,2,1), &
                               bounds(2,1,1):bounds(2,2,1), &
                               bounds(3,1,1):bounds(3,2,1)) = Value
        end where
    case (4)
        where (Variable1 % array4( bounds(1,1,2):bounds(1,2,2), &
                                   bounds(2,1,2):bounds(2,2,2), &
                                   bounds(3,1,2):bounds(3,2,2), &
                                   bounds(4,1,2):bounds(4,2,2)) == Value)
            Variable2 % array4(bounds(1,1,1):bounds(1,2,1), &
                               bounds(2,1,1):bounds(2,2,1), &
                               bounds(3,1,1):bounds(3,2,1), &
                               bounds(4,1,1):bounds(4,2,1)) = Value
        end where
    case (5)
        where (Variable1 % array5( bounds(1,1,2):bounds(1,2,2), &
                                   bounds(2,1,2):bounds(2,2,2), &
                                   bounds(3,1,2):bounds(3,2,2), &
                                   bounds(4,1,2):bounds(4,2,2), &
                                   bounds(5,1,2):bounds(5,2,2)) == Value)
            Variable2 % array5(bounds(1,1,1):bounds(1,2,1), &
                               bounds(2,1,1):bounds(2,2,1), &
                               bounds(3,1,1):bounds(3,2,1), &
                               bounds(4,1,1):bounds(4,2,1), &
                               bounds(5,1,1):bounds(5,2,1)) = Value
        end where
    case (6)
        where (Variable1 % array6( bounds(1,1,2):bounds(1,2,2), &
                                   bounds(2,1,2):bounds(2,2,2), &
                                   bounds(3,1,2):bounds(3,2,2), &
                                   bounds(4,1,2):bounds(4,2,2), &
                                   bounds(5,1,2):bounds(5,2,2), &
                                   bounds(6,1,2):bounds(6,2,2)) == Value)
            Variable2 % array6(bounds(1,1,1):bounds(1,2,1), &
                               bounds(2,1,1):bounds(2,2,1), &
                               bounds(3,1,1):bounds(3,2,1), &
                               bounds(4,1,1):bounds(4,2,1), &
                               bounds(5,1,1):bounds(5,2,1), &
                               bounds(6,1,1):bounds(6,2,1)) = Value
        end where
    end select
!---------------------------------------------------------------------------------------------------
end subroutine GiveAgrif_SpecialValueToTab_mpi
!===================================================================================================
!
!===================================================================================================
!  subroutine GiveAgrif_SpecialValueToTab
!---------------------------------------------------------------------------------------------------
subroutine GiveAgrif_SpecialValueToTab ( Variable1, Variable2, &
                                         lower, upper, Value, nbdim )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_Variable),      pointer      :: Variable1
    TYPE(Agrif_Variable),      pointer      :: Variable2
    INTEGER,                   intent(in)   :: nbdim
    INTEGER, DIMENSION(nbdim), intent(in)   :: lower, upper
    REAL,                      intent(in)   :: Value
!
    select case (nbdim)
    case (1)
        where (Variable1 % array1( lower(1):upper(1)) == Value)
            Variable2 % array1(lower(1):upper(1)) = Value
        end where
    case (2)
        where (Variable1 % array2( lower(1):upper(1), &
                                   lower(2):upper(2)) == Value)
            Variable2 % array2(lower(1):upper(1), &
                               lower(2):upper(2)) = Value
        end where
    case (3)
        where (Variable1 % array3( lower(1):upper(1), &
                                   lower(2):upper(2), &
                                   lower(3):upper(3)) == Value)
            Variable2 % array3(lower(1):upper(1), &
                               lower(2):upper(2), &
                               lower(3):upper(3)) = Value
        end where
    case (4)
        where (Variable1 % array4( lower(1):upper(1), &
                                   lower(2):upper(2), &
                                   lower(3):upper(3), &
                                   lower(4):upper(4)) == Value)
            Variable2 % array4(lower(1):upper(1), &
                               lower(2):upper(2), &
                               lower(3):upper(3), &
                               lower(4):upper(4)) = Value
        end where
    case (5)
        where (Variable1 % array5( lower(1):upper(1), &
                                   lower(2):upper(2), &
                                   lower(3):upper(3), &
                                   lower(4):upper(4), &
                                   lower(5):upper(5)) == Value)
            Variable2 % array5(lower(1):upper(1), &
                               lower(2):upper(2), &
                               lower(3):upper(3), &
                               lower(4):upper(4), &
                               lower(5):upper(5)) = Value
        end where
    case (6)
        where (Variable1 % array6( lower(1):upper(1), &
                                   lower(2):upper(2), &
                                   lower(3):upper(3), &
                                   lower(4):upper(4), &
                                   lower(5):upper(5), &
                                   lower(6):upper(6)) == Value)
            Variable2 % array6(lower(1):upper(1), &
                               lower(2):upper(2), &
                               lower(3):upper(3), &
                               lower(4):upper(4), &
                               lower(5):upper(5), &
                               lower(6):upper(6)) = Value
        end where
    end select
!---------------------------------------------------------------------------------------------------
end subroutine GiveAgrif_SpecialValueToTab
!===================================================================================================
!
#if defined AGRIF_MPI
!===================================================================================================
!  subroutine Where_ValTabToTab_mpi
!---------------------------------------------------------------------------------------------------
subroutine Where_ValTabToTab_mpi ( Variable1, Variable2, lower, upper, Value, nbdim )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_Variable),     pointer       :: Variable1
    TYPE(Agrif_Variable),     pointer       :: Variable2
    INTEGER,                  intent(in)    :: nbdim
    INTEGER,DIMENSION(nbdim), intent(in)    :: lower,upper
    REAL,                     intent(in)    :: Value
!
    integer :: i,j,k,l,m,n
!
    select case (nbdim)
    case (1)
        do i = lower(1),upper(1)
            if (variable1%array1(i) == Value) then
                variable1%array1(i) = Variable2%array1(i)
            endif
        enddo
    case (2)
        do j = lower(2),upper(2)
        do i = lower(1),upper(1)
            if (variable1%array2(i,j) == Value) then
                variable1%array2(i,j) = Variable2%array2(i,j)
            endif
        enddo
        enddo
    case (3)
        do k = lower(3),upper(3)
        do j = lower(2),upper(2)
        do i = lower(1),upper(1)
            if (variable1%array3(i,j,k) == Value) then
                variable1%array3(i,j,k) = Variable2%array3(i,j,k)
            endif
        enddo
        enddo
        enddo
    case (4)
        do l = lower(4),upper(4)
        do k = lower(3),upper(3)
        do j = lower(2),upper(2)
        do i = lower(1),upper(1)
            if (variable1%array4(i,j,k,l) == Value) then
                variable1%array4(i,j,k,l) = Variable2%array4(i,j,k,l)
            endif
        enddo
        enddo
        enddo
        enddo
    case (5)
        do m = lower(5),upper(5)
        do l = lower(4),upper(4)
        do k = lower(3),upper(3)
        do j = lower(2),upper(2)
        do i = lower(1),upper(1)
            if (variable1%array5(i,j,k,l,m) == Value) then
                variable1%array5(i,j,k,l,m) = Variable2%array5(i,j,k,l,m)
            endif
        enddo
        enddo
        enddo
        enddo
        enddo
    case (6)
        do n = lower(6),upper(6)
        do m = lower(5),upper(5)
        do l = lower(4),upper(4)
        do k = lower(3),upper(3)
        do j = lower(2),upper(2)
        do i = lower(1),upper(1)
            if (variable1%array6(i,j,k,l,m,n) == Value) then
                variable1%array6(i,j,k,l,m,n) = Variable2%array6(i,j,k,l,m,n)
            endif
        enddo
        enddo
        enddo
        enddo
        enddo
        enddo
    end select
!---------------------------------------------------------------------------------------------------
end subroutine Where_ValTabToTab_mpi
!===================================================================================================
#endif
!
!===================================================================================================
!  subroutine PreProcessToInterpOrUpdate
!---------------------------------------------------------------------------------------------------
subroutine PreProcessToInterpOrUpdate ( parent, child, petab_Child,     &
                                        pttab_Child, pttab_Parent,      &
                                        s_Child,  s_Parent,             &
                                        ds_Child, ds_Parent, nbdim, interp )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_PVariable), intent(in)   :: parent   !< Variable on the parent grid
    TYPE(Agrif_PVariable), intent(in)   :: child    !< Variable on the child grid
    INTEGER, DIMENSION(6), intent(out)  :: petab_child
    INTEGER, DIMENSION(6), intent(out)  :: pttab_child
    INTEGER, DIMENSION(6), intent(out)  :: pttab_parent
    REAL, DIMENSION(6),    intent(out)  :: s_child, s_parent
    REAL, DIMENSION(6),    intent(out)  :: ds_child,ds_parent
    INTEGER                 :: nbdim
    LOGICAL                 :: interp
!
    TYPE(Agrif_Variable), pointer :: root ! pointer on the variable of the root grid
    TYPE(Agrif_Grid), pointer   :: Agrif_Child_Gr
    TYPE(Agrif_Grid), pointer   :: Agrif_Parent_Gr
    INTEGER :: n
!
    Agrif_Child_Gr  => Agrif_Curgrid
    Agrif_Parent_Gr => Agrif_Curgrid % parent
!
    root => child % var % root_var
!
!   Number of dimensions of the current grid
    nbdim = root % nbdim
!
    do n = 1,nbdim
!
!       Value of interptab(n) can be either x,y,z or N for a no space dimension
        select case(root % interptab(n))
!
!       The dimension is 'x'
        case('x')
!
            pttab_Child(n)  = root % point(1)
            pttab_Parent(n) = root % point(1)
            s_Child(n)  = Agrif_Child_Gr  % Agrif_x(1)
            s_Parent(n) = Agrif_Parent_Gr % Agrif_x(1)
            ds_Child(n)  = Agrif_Child_Gr  % Agrif_d(1)
            ds_Parent(n) = Agrif_Parent_Gr % Agrif_d(1)
!
            if ( root % posvar(n) == 1 ) then
                petab_Child(n) = pttab_Child(n) + Agrif_Child_Gr%nb(1)
            else
                petab_Child(n) = pttab_Child(n) + Agrif_Child_Gr%nb(1) - 1
                s_Child(n)  = s_Child(n) + ds_Child(n)/2.
                s_Parent(n) = s_Parent(n) + ds_Parent(n)/2.
            endif
!
!       The dimension is 'y'
        case('y')
!
            pttab_Child(n)  = root % point(2)
            pttab_Parent(n) = root % point(2)
            s_Child(n)  = Agrif_Child_Gr  % Agrif_x(2)
            s_Parent(n) = Agrif_Parent_Gr % Agrif_x(2)
            ds_Child(n)  = Agrif_Child_Gr  % Agrif_d(2)
            ds_Parent(n) = Agrif_Parent_Gr % Agrif_d(2)
!
            if (root % posvar(n)==1) then
                petab_Child(n) = pttab_Child(n) + Agrif_Child_Gr%nb(2)
            else
                petab_Child(n) = pttab_Child(n) + Agrif_Child_Gr%nb(2) - 1
                s_Child(n)  = s_Child(n) + ds_Child(n)/2.
                s_Parent(n) = s_Parent(n) + ds_Parent(n)/2.
            endif
!
!       The DIMENSION is 'z'
        case('z')
!
            pttab_Child(n)  = root % point(3)
            pttab_Parent(n) = root % point(3)
            s_Child(n)  = Agrif_Child_Gr  % Agrif_x(3)
            s_Parent(n) = Agrif_Parent_Gr % Agrif_x(3)
            ds_Child(n)  = Agrif_Child_Gr  % Agrif_d(3)
            ds_Parent(n) = Agrif_Parent_Gr % Agrif_d(3)
!
            if (root % posvar(n)==1) then
                petab_Child(n) = pttab_Child(n) + Agrif_Child_Gr%nb(3)
            else
                petab_Child(n) = pttab_Child(n) + Agrif_Child_Gr%nb(3) - 1
                s_Child(n)  = s_Child(n) + ds_Child(n)/2.
                s_Parent(n) = s_Parent(n) + ds_Parent(n)/2.
            endif
!
!       The DIMENSION is not space
        case('N')
!
!       The next coefficients are calculated in order to do a simple copy of
!       values of the grid variable when the procedure of interpolation is
!       called for this DIMENSION
!
            if (interp) then
                call Agrif_nbdim_Get_bound(parent % var, pttab_Child(n),petab_Child(n),n)
            else
                call Agrif_nbdim_Get_bound(child % var, pttab_Child(n),petab_Child(n),n)
            endif
!
!           No interpolation but only a copy of the values of the grid variable
            pttab_Parent(n) = pttab_Child(n)
            s_Child(n)=0.
            s_Parent(n)=0.
            ds_Child(n)=1.
            ds_Parent(n)=1.
!
        end select
!
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine PreProcessToInterpOrUpdate
!===================================================================================================
!
#if defined AGRIF_MPI
!===================================================================================================
!   subroutine GetLocalBoundaries
!---------------------------------------------------------------------------------------------------
subroutine GetLocalBoundaries ( tab1, tab2, i, lb, ub, deb, fin )
!---------------------------------------------------------------------------------------------------
    INTEGER  ::  tab1,tab2
    INTEGER  ::  i
    INTEGER  ::  lb,ub
    INTEGER  ::  deb,fin
!
    INTEGER  ::  imin,imax
    INTEGER  ::  i1,i2
!
    call Agrif_InvLoc(lb,AGRIF_ProcRank,i,imin)
    call Agrif_InvLoc(ub,AGRIF_ProcRank,i,imax)
!
    if ( imin > tab2 ) then
        i1 = imax - imin
    else
        i1 = max(tab1 - imin,0)
    endif
!
    if (imax < tab1) then
        i2 = -(imax - imin)
    else
        i2 = min(tab2 - imax,0)
    endif
!
    deb = lb + i1
    fin = ub + i2
!---------------------------------------------------------------------------------------------------
end subroutine GetLocalBoundaries
!===================================================================================================
#endif
!
#if defined AGRIF_MPI
!===================================================================================================
!  subroutine Agrif_GlobtoLocInd2
!
!> For a global index located on the current processor, tabarray gives the corresponding local index
!---------------------------------------------------------------------------------------------------
subroutine Agrif_GlobtoLocInd2 ( tabarray, lboundl, uboundl, tab1, tab2,    &
                                 nbdim, rank, member )
!---------------------------------------------------------------------------------------------------
    INTEGER,                      intent(in)    :: nbdim
    INTEGER,DIMENSION(nbdim),     intent(in)    :: tab1, tab2
    INTEGER,DIMENSION(nbdim,2,2), intent(inout) :: tabarray
    INTEGER,DIMENSION(nbdim),     intent(in)    :: lboundl, uboundl
    INTEGER,                      intent(in)    :: rank
    LOGICAL,                      intent(out)   :: member
!
    INTEGER     :: i,i1,k
    INTEGER     :: nbloc(nbdim)
!
    tabarray(:,1,:) = HUGE(1)
    tabarray(:,2,:) = -HUGE(1)
!
    nbloc = 0
!
    do i = 1,nbdim
!
        call Agrif_Invloc(lboundl(i),rank,i,i1)

        do k=tab1(i)+lboundl(i)-i1,tab2(i)+lboundl(i)-i1
!
            if ( (k >= lboundl(i)) .AND. (k.LE.uboundl(i)) ) then
                nbloc(i) = 1
                tabarray(i,1,1) = min(tabarray(i,1,1),k-lboundl(i)+i1)
                tabarray(i,2,1) = max(tabarray(i,2,1),k-lboundl(i)+i1)

                tabarray(i,1,2) = min(tabarray(i,1,2),k)
                tabarray(i,2,2) = max(tabarray(i,2,2),k)
            endif
        enddo
    enddo

    member = .FALSE.
    if (sum(nbloc) == nbdim)  member = .TRUE.
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_GlobtoLocInd2
!===================================================================================================
#endif
!
!===================================================================================================
!  subroutine Agrif_Copy_2d
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Copy_2d ( tabout, tabin, l, m, inf, sup, inf2, sup2 )
!---------------------------------------------------------------------------------------------------
    integer, dimension(2), intent(in)   :: l, m
    integer, dimension(2), intent(in)   :: inf, sup
    integer, dimension(2), intent(in)   :: inf2,sup2
    real, target, dimension(l(1):,l(2):) :: tabout
    real, target, dimension(m(1):,m(2):) :: tabin
!
    tabout( inf(1):sup(1), &
            inf(2):sup(2)) = tabin(inf2(1):sup2(1), &
                                   inf2(2):sup2(2))
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Copy_2d
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Copy_3d
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Copy_3d ( tabout, tabin, l, m, inf, sup, inf2, sup2 )
!---------------------------------------------------------------------------------------------------
    integer, dimension(3), intent(in)   :: l, m
    integer, dimension(3), intent(in)   :: inf, sup
    integer, dimension(3), intent(in)   :: inf2,sup2
    real, target, dimension(l(1):,l(2):,l(3):) :: tabout
    real, target, dimension(m(1):,m(2):,m(3):) :: tabin
!
    tabout(inf(1):sup(1), &
           inf(2):sup(2), &
           inf(3):sup(3)) = tabin(inf2(1):sup2(1), &
                                  inf2(2):sup2(2), &
                                  inf2(3):sup2(3))
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Copy_3d
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Copy_4d
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Copy_4d ( tabout, tabin, l, m, inf, sup, inf2, sup2 )
!---------------------------------------------------------------------------------------------------
    integer, dimension(4), intent(in)   :: l, m
    integer, dimension(4), intent(in)   :: inf, sup
    integer, dimension(4), intent(in)   :: inf2,sup2
    real, target, dimension(l(1):,l(2):,l(3):,l(4):) :: tabout
    real, target, dimension(m(1):,m(2):,m(3):,m(4):) :: tabin
!
    tabout(inf(1):sup(1), &
           inf(2):sup(2), &
           inf(3):sup(3), &
           inf(4):sup(4)) = tabin(inf2(1):sup2(1), &
                                  inf2(2):sup2(2), &
                                  inf2(3):sup2(3), &
                                  inf2(4):sup2(4))
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Copy_4d
!===================================================================================================
!
end module Agrif_Arrays
