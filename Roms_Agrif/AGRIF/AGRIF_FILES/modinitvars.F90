!
! $Id: modinitvars.F 662 2007-05-25 15:58:52Z opalod $
!
!     Agrif (Adaptive Grid Refinement In Fortran)
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
!     Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
!
!
!> Module Agrif_Init_Vars
!>
!> Initialization of the variables of the current grid.
!
module Agrif_Init_Vars
!
    use Agrif_Types
    use Agrif_Link
!
    implicit none
!
contains
!
!===================================================================================================
!  subroutine Agrif_Create_Var
!
!> Allocation of the list of grid variables for grid Agrif_Gr.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Create_Var ( Agrif_Gr )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid), pointer   :: Agrif_Gr  !< Pointer on the current grid
!
    integer :: nb
!
    allocate(Agrif_Gr % tabvars(Agrif_NbVariables))
!
    do nb = 1,Agrif_NbVariables
        allocate(Agrif_Gr % tabvars(nb) % var)
    enddo
!
    if ( Agrif_Gr % fixedrank /= 0 ) then
        do nb = 1, Agrif_NbVariables
            Agrif_Gr % tabvars(nb) % parent_var     => Agrif_Gr % parent %tabvars(nb)
            Agrif_Gr % tabvars(nb) % var % nbdim    =  Agrif_Mygrid % tabvars(nb) % var % nbdim
            Agrif_Gr % tabvars(nb) % var % root_var => Agrif_Mygrid % tabvars(nb) % var
        enddo
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Create_Var
!===================================================================================================
!
end module Agrif_Init_Vars
