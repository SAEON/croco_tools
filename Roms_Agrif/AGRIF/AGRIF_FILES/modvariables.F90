module Agrif_Variables
    
    use Agrif_CurgridFunctions
!
    implicit none

contains
!
!===================================================================================================
!  subroutine Agrif_Declare_Variable
!
!> Declare a new variable profile
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Declare_Variable ( posvar, firstpoint, raf, lb, ub, varid, torestore )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(:),      INTENT(in)      :: posvar     !< position of the variable on the cell
                                                              !< (1 for the border of the edge, 2 for the center)
    INTEGER, DIMENSION(:),      INTENT(in)      :: firstpoint !< index of the first point in the real domain
    CHARACTER(*), DIMENSION(:), INTENT(in)      :: raf        !< Array indicating the type of dimension (space or not)
                                                              !<   for each of them
    INTEGER, DIMENSION(:),      INTENT(in)      :: lb
    INTEGER, DIMENSION(:),      INTENT(in)      :: ub
    INTEGER,                    INTENT(inout)   :: varid
    LOGICAL, OPTIONAL,          INTENT(in)      :: torestore
!
    character*(80) :: variablename
    Type(Agrif_List_Variables), pointer :: newvariable
    TYPE(Agrif_PVariable), pointer      :: parent_var, root_var
    TYPE(Agrif_Variable),  pointer      :: var
    INTEGER :: i, dimensio
    LOGICAL :: restaure

    variablename = 'xxx'

    restaure = .FALSE.
    if ( Agrif_Mygrid % ngridstep /= 0 ) then
        if (present(torestore)) restaure = torestore
    endif
!
    dimensio = SIZE(posvar)
!
    allocate(newvariable)
    allocate(newvariable % pvar)
    allocate(newvariable % pvar % var)
    
    var => newvariable % pvar % var
    
    allocate(var % posvar(dimensio))
    allocate(var % interptab(dimensio))
!
    var % variablename = variablename
    var % interptab = raf
    var % nbdim = dimensio
    var % posvar = posvar
    var % point(1:dimensio) = firstpoint
    var % restaure = restaure

    var % lb(1:dimensio) = lb(1:dimensio)
    var % ub(1:dimensio) = ub(1:dimensio)

    if (restaure) then
        select case(dimensio)
        case(1)
            allocate(var%Restore1D(lb(1):ub(1)))
            var%Restore1D = 0
        case(2)
            allocate(var%Restore2D(lb(1):ub(1),lb(2):ub(2)))
            var%Restore2D = 0
        case(3)
            allocate(var%Restore3D(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)))
            var%Restore3D = 0
        case(4)
            allocate(var%Restore4D(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)))
            var%Restore4D = 0
        case(5)
            allocate(var%Restore5D(lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),lb(5):ub(5)))
            var%Restore5D = 0
        end select
    endif

    newvariable % nextvariable => Agrif_Curgrid % variables

    Agrif_Curgrid % variables => newvariable
    Agrif_Curgrid % Nbvariables = Agrif_Curgrid % Nbvariables + 1

    varid = -Agrif_Curgrid % Nbvariables

!       if (agrif_curgrid%parent%nbvariables < agrif_curgrid%nbvariables)
!     &       then
!      Allocate(newvariablep)
!      Allocate(newvariablep%pvar)
!      Allocate(newvariablep%pvar%var)
!      Allocate(newvariablep%pvar%var%posvar(dimensio))
!      Allocate(newvariablep%pvar%var%interptab(dimensio))
!      newvariablep%pvar%var%variablename = variablename
!      newvariablep%pvar%var%interptab = raf
!      newvariablep%pvar%var%nbdim = dimensio
!      newvariablep%pvar%var%posvar = posvar
!      newvariablep%pvar%var%point(1:dimensio) = firstpoint
!      newvariablep%pvar%var%restaure = restaure
!
!      newvariablep%pvar%var%lb(1:dimensio) = lb(1:dimensio)
!      newvariablep%pvar%var%ub(1:dimensio) = ub(1:dimensio)
!
!      newvariablep % nextvariable => Agrif_Curgrid%parent%variables
!
!      Agrif_Curgrid%parent%variables => newvariablep
!
!      Agrif_Curgrid%parent%Nbvariables =
!     &    Agrif_Curgrid%parent%Nbvariables + 1
!      parent_var=>newvariablep%pvar
!      else
!      parent_var=>Agrif_Search_Variable
!     &              (Agrif_Curgrid%parent,Agrif_Curgrid%nbvariables)
!       endif

    if (Agrif_Root()) then
        parent_var => NULL()
    else
        parent_var => Agrif_Search_Variable(Agrif_Curgrid%parent, Agrif_Curgrid%nbvariables)
    endif
!
    newvariable % pvar % parent_var => parent_var
    root_var => Agrif_Search_Variable(Agrif_Mygrid,Agrif_Curgrid%nbvariables)
    var%root_var => root_var%var
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Declare_Variable
!===================================================================================================
!
!===================================================================================================
!  function Agrif_Search_Variable
!---------------------------------------------------------------------------------------------------
function Agrif_Search_Variable ( grid, varid ) result(outvar)
!---------------------------------------------------------------------------------------------------
    Type(Agrif_Grid), pointer       :: grid
    integer,          intent(in)    :: varid
!
    Type(Agrif_PVariable),      pointer :: outvar
    Type(Agrif_List_Variables), pointer :: parcours
    integer :: nb, varidinv
!
    parcours => grid % variables
    varidinv = 1 + grid % nbvariables - varid

    do nb = 1,varidinv-1
        parcours => parcours % nextvariable
    enddo

    outvar => parcours % pvar
!---------------------------------------------------------------------------------------------------
end function Agrif_Search_variable
!===================================================================================================
!
end module Agrif_Variables