!
! $Id: modbcfunction.F 779 2007-12-22 17:04:17Z rblod $
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
!     Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
!
!> Module Agrif_BcFunction.
!
module Agrif_BcFunction
!
!     Modules used:
!
    use Agrif_Boundary
    use Agrif_Update
    use Agrif_FluxMod
    use Agrif_Save
!
    implicit none
!
    interface Agrif_Bc_variable
        module procedure Agrif_Bc_variable0d, &
                         Agrif_Bc_variable1d, &
                         Agrif_Bc_variable2d, &
                         Agrif_Bc_variable3d, &
                         Agrif_Bc_variable4d, &
                         Agrif_Bc_variable5d
    end interface
!
    interface Agrif_Set_Parent
        module procedure Agrif_Set_Parent_int, &
                         Agrif_Set_Parent_real
    end interface
!
    interface Agrif_Interp_variable
        module procedure Agrif_Interp_var0d, &
                         Agrif_Interp_var1d, &
                         Agrif_Interp_var2d, &
                         Agrif_Interp_var3d, &
                         Agrif_Interp_var4d, &
                         Agrif_Interp_var5d
    end interface
!
    interface Agrif_Init_variable
        module procedure Agrif_Init_variable0d, &
                         Agrif_Init_variable1d, &
                         Agrif_Init_variable2d, &
                         Agrif_Init_variable3d, &
                         Agrif_Init_variable4d
    end interface
!
    interface Agrif_update_variable
        module procedure Agrif_update_var0d, &
                         Agrif_update_var1d, &
                         Agrif_update_var2d, &
                         Agrif_update_var3d, &
                         Agrif_update_var4d, &
                         Agrif_update_var5d
    end interface

    interface Agrif_Save_Forrestore
        module procedure Agrif_Save_Forrestore0d, &
                         Agrif_Save_Forrestore2d, &
                         Agrif_Save_Forrestore3d, &
                         Agrif_Save_Forrestore4d
    end interface
!
contains
!
!===================================================================================================
! subroutine Agrif_Set_type
!
!> To set the type of the variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_type ( tabvarsindic, posvar, point )
!---------------------------------------------------------------------------------------------------
    INTEGER,               INTENT(in) :: tabvarsindic   !< indice of the variable in tabvars
    INTEGER, DIMENSION(:), INTENT(in) :: posvar         !< position of the variable on the cell
                                                        !< (1 for the border of the edge, 2 for the center)
    INTEGER, DIMENSION(:), INTENT(in) :: point          !< index of the first point in the REAL domain
!
    INTEGER :: dimensio !< Dimension of the variable
    INTEGER :: i
    TYPE(Agrif_Variable), pointer :: var => NULL()
    
    var => Agrif_Mygrid%tabvars(tabvarsindic) % var
!
    dimensio = var % nbdim
!
    if (.not.associated(var % posvar))  allocate(var % posvar(dimensio))
!
    var % posvar(1:dimensio) = posvar(1:dimensio)
    var % point(1:dimensio)  = point(1:dimensio)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_type
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_parent_int
!
!> To set the TYPE of the variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_parent_int(tabvarsindic,value)
!---------------------------------------------------------------------------------------------------
    INTEGER, INTENT(in)     :: tabvarsindic !< indice of the variable in tabvars
    INTEGER, INTENT(in)     :: value        !< input value
!
    Agrif_Curgrid % parent % tabvars(tabvarsindic) % var % iarray0 = value
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_parent_int
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_parent_real
!---------------------------------------------------------------------------------------------------
!> To set the TYPE of the variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_parent_real ( tabvarsindic, value )
!---------------------------------------------------------------------------------------------------
    INTEGER, INTENT(in)     :: tabvarsindic !< indice of the variable in tabvars
    REAL,    INTENT(in)     :: value        !< input value
!
    Agrif_Curgrid % parent % tabvars(tabvarsindic) % var % array0 = value
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_parent_real
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_raf
!
!> To set the rafinment ratio of variable
!>
!> Attention tabraf est de taille trois si on ne raffine pas suivant z la troisieme entree du
!> tableau tabraf est 'N'
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_raf ( tabvarsindic, tabraf )
!---------------------------------------------------------------------------------------------------
    INTEGER,                    INTENT(in) :: tabvarsindic !< indice of the variable in tabvars
    CHARACTER(*), DIMENSION(:), INTENT(in) :: tabraf
!
    INTEGER :: dimensio ! Dimension of the variable
    INTEGER :: i

    TYPE(Agrif_Variable), pointer :: var => NULL()
    
    var => Agrif_Mygrid%tabvars(tabvarsindic) % var
!
    dimensio = var % nbdim
!
    if (.not.associated(var % interptab)) allocate( var% interptab(dimensio) )

    do i = 1 , dimensio
        var % interptab(i) = TRIM(tabraf(i))
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_raf
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_bc
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_bc ( tabvarsindic, point, Interpolationshouldbemade )
!---------------------------------------------------------------------------------------------------
    INTEGER,               intent(in)   :: tabvarsindic !< indice of the variable in tabvars
    INTEGER, DIMENSION(2), intent(in)   :: point        !< point
    LOGICAL, OPTIONAL,     intent(in)   :: Interpolationshouldbemade !< interpolation should be made
!
    INTEGER                        :: indic ! indice of the variable in tabvars
    TYPE(Agrif_PVariable), pointer :: tabvars
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif

    if (indic <=0) then
        tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
    else
        tabvars => Agrif_Curgrid % tabvars(indic)
    endif

    if (Agrif_Curgrid % fixedrank /= 0) then
        if (.Not.Associated(tabvars%var% interpIndex)) then
            allocate(tabvars%var % interpIndex)
            tabvars%var % interpIndex = -1
            allocate(tabvars%var % oldvalues2D(2,1))
            tabvars%var % oldvalues2D = 0.
        endif
        if ( PRESENT(Interpolationshouldbemade) ) then
            tabvars%var%Interpolationshouldbemade = Interpolationshouldbemade
        endif
    endif
!
    tabvars%var % bcinf = point(1)
    tabvars%var % bcsup = point(2)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_bc
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_interp
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_interp ( tabvarsindic, interp, interp1, interp2, interp3 )
!---------------------------------------------------------------------------------------------------
    INTEGER,           intent(in)   :: tabvarsindic !< indice of the variable in tabvars
    INTEGER, OPTIONAL, intent(in)   :: interp, interp1, interp2, interp3
!
    INTEGER :: indic ! indice of the variable in tabvars
    TYPE(Agrif_PVariable), pointer :: tabvars
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif

    if (indic <=0) then
        tabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
    else
        tabvars => Agrif_Mygrid % tabvars(indic)
    endif
!
    tabvars % var % Typeinterp = Agrif_Constant
    if (present(interp))    tabvars % var % Typeinterp = interp
    if (present(interp1))   tabvars % var % Typeinterp(1) = interp1
    if (present(interp2))   tabvars % var % Typeinterp(2) = interp2
    if (present(interp3))   tabvars % var % Typeinterp(3) = interp3
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_interp
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_bcinterp
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_bcinterp ( tabvarsindic, interp,   interp1,  interp2,  interp3, &
                                              interp11, interp12, interp21, interp22 )
!---------------------------------------------------------------------------------------------------
    INTEGER,           intent(in)   :: tabvarsindic !< indice of the variable in tabvars
    INTEGER, OPTIONAL, intent(in)   :: interp,   interp1,  interp2,  interp3
    INTEGER, OPTIONAL, intent(in)   :: interp11, interp12, interp21, interp22
!
    INTEGER                         :: indic ! indice of the variable in tabvars
    TYPE(Agrif_PVariable), pointer  :: tabvars
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif

    if (indic <=0) then
        tabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
    else
        tabvars => Agrif_Mygrid % tabvars(indic)
    endif
!
    tabvars% var % bctypeinterp = Agrif_Constant
    if (present(interp))    tabvars% var % bctypeinterp = interp
    if (present(interp1))   tabvars% var % bctypeinterp(1:2,1) = interp1
    if (present(interp11))  tabvars% var % bctypeinterp(1,1) = interp11
    if (present(interp12))  tabvars% var % bctypeinterp(1,2) = interp12
    if (present(interp2))   tabvars% var % bctypeinterp(1:2,2) = interp2
    if (present(interp21))  tabvars% var % bctypeinterp(2,1) = interp21
    if (present(interp22))  tabvars% var % bctypeinterp(2,2) = interp22
    if (present(interp3))   tabvars% var % bctypeinterp(1:2,3) = interp3
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_bcinterp
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_Update
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_Update ( tabvarsindic, point )
!---------------------------------------------------------------------------------------------------
    INTEGER,               intent(in)   :: tabvarsindic !< indice of the variable in tabvars
    INTEGER, DIMENSION(2), intent(in)   :: point
!
    Agrif_Curgrid%tabvars(tabvarsindic)%var % updateinf = point(1)
    Agrif_Curgrid%tabvars(tabvarsindic)%var % updatesup = point(2)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_Update
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_UpdateType
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_UpdateType ( tabvarsindic, update,  update1, update2, &
                                                update3, update4, update5 )
!---------------------------------------------------------------------------------------------------
    INTEGER,           intent(in) :: tabvarsindic     !< indice of the variable in tabvars
    INTEGER, OPTIONAL, intent(in) :: update, update1, update2, update3, update4, update5
!
    INTEGER                         :: indic ! indice of the variable in tabvars
    TYPE(Agrif_PVariable), pointer  :: roottabvars
!
    indic = tabvarsindic

    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif

    if (indic <=0) then
        roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
    else
        roottabvars => Agrif_Mygrid % tabvars(indic)
    endif
!
    roottabvars% var % typeupdate = Agrif_Update_Copy
    if (present(update))    roottabvars% var % typeupdate    = update
    if (present(update1))   roottabvars% var % typeupdate(1) = update1
    if (present(update2))   roottabvars% var % typeupdate(2) = update2
    if (present(update3))   roottabvars% var % typeupdate(3) = update3
    if (present(update4))   roottabvars% var % typeupdate(4) = update4
    if (present(update5))   roottabvars% var % typeupdate(5) = update5
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_UpdateType
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Set_restore
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Set_restore ( tabvarsindic )
!---------------------------------------------------------------------------------------------------
    INTEGER, intent(in) :: tabvarsindic     !< indice of the variable in tabvars
!
    INTEGER :: indic  !  indice of the variable in tabvars
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif
!
    Agrif_Mygrid%tabvars(indic)%var % restaure = .TRUE.
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Set_restore
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Init_variable0d
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Init_variable0d ( tabvarsindic0, tabvarsindic, procname )
!---------------------------------------------------------------------------------------------------
    INTEGER, intent(in) :: tabvarsindic0    !< indice of the variable in tabvars
    INTEGER, intent(in) :: tabvarsindic     !< indice of the variable in tabvars
    INTEGER :: indic ! indice of the variable in tabvars
    Optional :: procname
    External :: procname
!
    if (Agrif_Root()) return
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif

    if (present(procname)) then
        call Agrif_Interp_variable(tabvarsindic0,indic,procname)
        call Agrif_Bc_variable(tabvarsindic0,indic,1.,procname)
    else
        call Agrif_Interp_variable(tabvarsindic0,indic)
        call Agrif_Bc_variable(tabvarsindic0,indic,1.)
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Init_variable0d
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Init_variable1d
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Init_variable1d ( q, tabvarsindic, procname )
!---------------------------------------------------------------------------------------------------
    REAL, DIMENSION(:)              :: q
    INTEGER, intent(in)             :: tabvarsindic !< indice of the variable in tabvars
    Optional :: procname
    External :: procname
!
    INTEGER :: indic ! indice of the variable in tabvars
!
    if (Agrif_Root()) return
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif
!
    if (present(procname)) then
        call Agrif_Interp_variable(q,indic,procname)
        call Agrif_Bc_variable(q,indic,1.,procname)
    else
        call Agrif_Interp_variable(q,indic)
        call Agrif_Bc_variable(q,indic,1.)
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Init_variable1d
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Init_variable2d
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Init_variable2d ( q, tabvarsindic, procname )
!---------------------------------------------------------------------------------------------------
    REAL, DIMENSION(:,:)            :: q
    INTEGER, intent(in)             :: tabvarsindic !< indice of the variable in tabvars
    Optional :: procname
    External :: procname
!
    INTEGER :: indic ! indice of the variable in tabvars
!
    if (Agrif_Root()) return
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif
!
    if (present(procname)) then
        call Agrif_Interp_variable(q,indic,procname)
        call Agrif_Bc_variable(q,indic,1.,procname)
    else
        call Agrif_Interp_variable(q,indic)
        call Agrif_Bc_variable(q,indic,1.)
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Init_variable2d
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Init_variable3d
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Init_variable3d ( q, tabvarsindic, procname )
!---------------------------------------------------------------------------------------------------
    REAL, DIMENSION(:,:,:)          :: q
    INTEGER, intent(in)             :: tabvarsindic !< indice of the variable in tabvars
    Optional :: procname
    External :: procname
!
    INTEGER :: indic ! indice of the variable in tabvars
!
    if (Agrif_Root()) return
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif
!
    if (present(procname)) then
        call Agrif_Interp_variable(q,indic,procname)
        call Agrif_Bc_variable(q,indic,1.,procname)
    else
        call Agrif_Interp_variable(q,indic)
        call Agrif_Bc_variable(q,indic,1.)
    endif
!
end subroutine Agrif_Init_variable3d
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Init_variable4d
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Init_variable4d ( q, tabvarsindic, procname )
!---------------------------------------------------------------------------------------------------
    REAL, DIMENSION(:,:,:,:)        :: q
    INTEGER, intent(in)             :: tabvarsindic !< indice of the variable in tabvars
    Optional :: procname
    External :: procname
!
    INTEGER :: indic ! indice of the variable in tabvars
!
    if (Agrif_Root()) return
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif
!
    if (present(procname)) then
        call Agrif_Interp_variable(q,indic,procname)
        call Agrif_Bc_variable(q,indic,1.,procname)
    else
        call Agrif_Interp_variable(q,indic)
        call Agrif_Bc_variable(q,indic,1.)
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Init_variable4d
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Bc_variable0d
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Bc_variable0d ( tabvarsindic0, tabvarsindic, calledweight, procname )
!---------------------------------------------------------------------------------------------------
    INTEGER, intent(in)         :: tabvarsindic0    !< indice of the variable in tabvars
    INTEGER, intent(in)         :: tabvarsindic     !< indice of the variable in tabvars
    REAL, OPTIONAL, intent(in)  :: calledweight
    Optional :: procname
    External :: procname
!
    REAL    :: weight
    LOGICAL :: pweight
    INTEGER :: indic, dimensio
    TYPE(Agrif_PVariable), pointer :: tabvars, parenttabvars, roottabvars
!
    if (Agrif_Root()) return
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif

    if ( PRESENT(calledweight) ) then
        weight = calledweight
        pweight = .TRUE.
    else
        weight = 0.
        pweight = .FALSE.
    endif
    
    if (indic <=0) then
        tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
        parenttabvars => tabvars%parent_var
        roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
    else
        tabvars => Agrif_Curgrid % tabvars(indic)
        parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
        roottabvars => Agrif_Mygrid % tabvars(indic)
    endif

    dimensio =  roottabvars % var % nbdim
!
    if ( dimensio == 1 ) then
        call Agrif_Interp_Bc_1D( roottabvars % var % bctypeinterp,          &
                    parenttabvars, tabvars,                                 &
                    Agrif_Curgrid % tabvars(tabvarsindic0) %var % array1,   &
                    tabvars % var % bcinf,                                  &
                    tabvars % var % bcsup,                                  &
                    weight, pweight)
    endif
!
    if ( dimensio == 2 ) then
        if (present(procname)) then
            call Agrif_Interp_Bc_2D( roottabvars % var % bctypeinterp,      &
                    parenttabvars, tabvars,                                 &
                    Agrif_Curgrid % tabvars(tabvarsindic0) %var % array2,   &
                    tabvars % var % bcinf,                                  &
                    tabvars % var % bcsup,                                  &
                    weight, pweight, procname)
        else
            call Agrif_Interp_Bc_2D( roottabvars % var % bctypeinterp,      &
                    parenttabvars, tabvars,                                 &
                    Agrif_Curgrid % tabvars(tabvarsindic0) %var % array2,   &
                    tabvars % var % bcinf,                                  &
                    tabvars % var % bcsup,                                  &
                    weight, pweight)
        endif
    endif
!
    if ( dimensio == 3 ) then
        if (present(procname)) then
            call Agrif_Interp_Bc_3D( roottabvars % var % bctypeinterp,      &
                    parenttabvars, tabvars,                                 &
                    Agrif_Curgrid % tabvars(tabvarsindic0) %var % array3,   &
                    tabvars % var % bcinf,                                  &
                    tabvars % var % bcsup,                                  &
                    weight, pweight, procname)
        else
            call Agrif_Interp_Bc_3D( roottabvars % var % bctypeinterp,      &
                    parenttabvars, tabvars,                                 &
                    Agrif_Curgrid % tabvars(tabvarsindic0) %var % array3,   &
                    tabvars % var % bcinf,                                  &
                    tabvars % var % bcsup,                                  &
                    weight, pweight)
        endif
    endif
!
    if ( dimensio == 4 ) then
        if (present(procname)) then
            call Agrif_Interp_Bc_4D( roottabvars % var % bctypeinterp,      &
                    parenttabvars, tabvars,                                 &
                    Agrif_Curgrid % tabvars(tabvarsindic0) %var % array4,   &
                    tabvars % var % bcinf,                                  &
                    tabvars % var % bcsup,                                  &
                    weight, pweight, procname)
        else
            call Agrif_Interp_Bc_4D( roottabvars % var % bctypeinterp,      &
                    parenttabvars, tabvars,                                 &
                    Agrif_Curgrid % tabvars(tabvarsindic0) %var % array4,   &
                    tabvars % var % bcinf,                                  &
                    tabvars % var % bcsup,                                  &
                    weight, pweight)
        endif
    endif
!
    if ( dimensio == 5 ) then
        if (present(procname)) then
            call Agrif_Interp_Bc_5D( roottabvars % var % bctypeinterp,      &
                    parenttabvars, tabvars,                                 &
                    Agrif_Curgrid % tabvars(tabvarsindic0) %var % array5,   &
                    tabvars % var % bcinf,                                  &
                    tabvars % var % bcsup,                                  &
                    weight, pweight, procname)
        else
            call Agrif_Interp_Bc_5D( roottabvars % var % bctypeinterp,      &
                    parenttabvars, tabvars,                                 &
                    Agrif_Curgrid % tabvars(tabvarsindic0) %var % array5,   &
                    tabvars % var % bcinf,                                  &
                    tabvars % var % bcsup,                                  &
                    weight, pweight)
        endif
    endif
!
    if ( dimensio == 6 ) then
        call Agrif_Interp_Bc_6D( roottabvars % var % bctypeinterp,          &
                    parenttabvars, tabvars,                                 &
                    Agrif_Curgrid % tabvars(tabvarsindic0) %var % array6,   &
                    tabvars % var % bcinf,                                  &
                    tabvars % var % bcsup,                                  &
                    weight,pweight)
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Bc_variable0d
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Bc_variable1d
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Bc_variable1d ( q, tabvarsindic, calledweight, procname )
!---------------------------------------------------------------------------------------------------
    REAL, Dimension(:)              :: q
    INTEGER,        INTENT(in)      :: tabvarsindic ! indice of the variable in tabvars
    REAL, OPTIONAL, INTENT(in)      :: calledweight
    External                        :: procname
    Optional                        :: procname
!
    INTEGER :: indic ! indice of the variable in tabvars
!
    REAL    :: weight
    LOGICAL :: pweight
    TYPE(Agrif_PVariable),Pointer ::tabvars,parenttabvars,roottabvars
!
    if (Agrif_Root()) return
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif

    if ( PRESENT(calledweight) ) then
        weight = calledweight
        pweight = .TRUE.
    else
        weight = 0.
        pweight = .FALSE.
    endif

    if (indic <=0) then
        tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
        parenttabvars => tabvars%parent_var
        roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
    else
        tabvars=>Agrif_Curgrid % tabvars(indic)
        parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
        roottabvars => Agrif_Mygrid % tabvars(indic)
    endif

    if (present(procname)) then
        call Agrif_Interp_Bc_1D(roottabvars % var % bctypeinterp,   &
                                parenttabvars, tabvars, q,          &
                                tabvars % var % bcinf,              &
                                tabvars % var % bcsup,              &
                                weight, pweight, procname)
    else
        call Agrif_Interp_Bc_1D(roottabvars % var % bctypeinterp,   &
                                parenttabvars, tabvars, q,          &
                                tabvars % var % bcinf,              &
                                tabvars % var % bcsup,              &
                                weight, pweight)
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Bc_variable1d
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Bc_variable2d
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Bc_variable2d(q,tabvarsindic,calledweight, procname)
!---------------------------------------------------------------------------------------------------
    REAL, Dimension(:,:)            :: q
    INTEGER,        INTENT(in)      :: tabvarsindic ! indice of the variable in tabvars
    REAL, OPTIONAL, INTENT(in)      :: calledweight
    External                        :: procname
    Optional                        :: procname
!
    INTEGER :: indic ! indice of the variable in tabvars
!
    REAL    :: weight
    LOGICAL :: pweight
    TYPE(Agrif_PVariable),Pointer ::tabvars,parenttabvars,roottabvars
!
    if (Agrif_Root()) return
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif

    if ( PRESENT(calledweight) ) then
        weight = calledweight
        pweight = .TRUE.
    else
        weight = 0.
        pweight = .FALSE.
    endif

    if (indic <=0) then
        tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
        parenttabvars => tabvars%parent_var
        roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
    else
        tabvars => Agrif_Curgrid % tabvars(indic)
        parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
        roottabvars => Agrif_Mygrid % tabvars(indic)
    endif

    if (present(procname)) then
        call Agrif_Interp_Bc_2D(roottabvars % var % bctypeinterp,   &
                                parenttabvars, tabvars, q,          &
                                tabvars % var % bcinf,              &
                                tabvars % var % bcsup,              &
                                weight, pweight, procname)
    else
        call Agrif_Interp_Bc_2D(roottabvars % var % bctypeinterp,   &
                                parenttabvars, tabvars, q,          &
                                tabvars % var % bcinf,              &
                                tabvars % var % bcsup,              &
                                weight, pweight)
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Bc_variable2d
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Bc_variable3d
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Bc_variable3d ( q, tabvarsindic, calledweight, procname )
!---------------------------------------------------------------------------------------------------
    REAL, Dimension(:,:,:)          :: q
    INTEGER,        INTENT(in)      :: tabvarsindic ! indice of the variable in tabvars
    REAL, OPTIONAL, INTENT(in)      :: calledweight
    External                        :: procname
    Optional                        :: procname
!
    INTEGER :: indic ! indice of the variable in tabvars
!
    REAL    :: weight
    LOGICAL :: pweight
    TYPE(Agrif_PVariable),Pointer ::tabvars,parenttabvars,roottabvars
!
    if (Agrif_Root()) return
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif

    if ( PRESENT(calledweight) ) then
        weight = calledweight
        pweight = .TRUE.
    else
        weight = 0.
        pweight = .FALSE.
    endif

    if (indic <=0) then
        tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
        parenttabvars => tabvars%parent_var
        roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
    else
        tabvars=>Agrif_Curgrid % tabvars(indic)
        parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
        roottabvars => Agrif_Mygrid % tabvars(indic)
    endif

    if (present(procname)) then
        call Agrif_Interp_Bc_3D(roottabvars % var % bctypeinterp,   &
                                parenttabvars, tabvars, q,          &
                                tabvars % var % bcinf,              &
                                tabvars % var % bcsup,              &
                                weight, pweight, procname)
    else
        call Agrif_Interp_Bc_3D(roottabvars % var % bctypeinterp,   &
                                parenttabvars, tabvars, q,          &
                                tabvars % var % bcinf,              &
                                tabvars % var % bcsup,              &
                                weight, pweight)
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Bc_variable3d
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Bc_variable4d
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Bc_variable4d ( q, tabvarsindic, calledweight, procname )
!---------------------------------------------------------------------------------------------------
    REAL, Dimension(:,:,:,:)        :: q
    INTEGER,        INTENT(in)      :: tabvarsindic ! indice of the variable in tabvars
    REAL, OPTIONAL, INTENT(in)      :: calledweight
    External                        :: procname
    Optional                        :: procname
!
    INTEGER :: indic ! indice of the variable in tabvars
!
    REAL    :: weight
    LOGICAL :: pweight
    TYPE(Agrif_PVariable),Pointer ::tabvars,parenttabvars,roottabvars
!
    if (Agrif_Root()) return
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif

    if ( PRESENT(calledweight) ) then
        weight = calledweight
        pweight = .TRUE.
    else
        weight = 0.
        pweight = .FALSE.
    endif

    if (indic <=0) then
        tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
        parenttabvars => tabvars%parent_var
        roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
    else
        tabvars=>Agrif_Curgrid % tabvars(indic)
        parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
        roottabvars => Agrif_Mygrid % tabvars(indic)
    endif

    if (present(procname)) then
        call Agrif_Interp_Bc_4D(roottabvars % var % bctypeinterp,   &
                                parenttabvars, tabvars, q,          &
                                tabvars % var % bcinf,              &
                                tabvars % var % bcsup,              &
                                weight, pweight, procname)
    else
        call Agrif_Interp_Bc_4D(roottabvars % var % bctypeinterp,   &
                                parenttabvars, tabvars, q,          &
                                tabvars % var % bcinf,              &
                                tabvars % var % bcsup,              &
                                weight, pweight)
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Bc_variable4d
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Bc_variable5d
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Bc_variable5d ( q, tabvarsindic, calledweight, procname )
!---------------------------------------------------------------------------------------------------
    REAL, Dimension(:,:,:,:,:)      :: q
    INTEGER,        INTENT(in)      :: tabvarsindic ! indice of the variable in tabvars
    REAL, OPTIONAL, INTENT(in)      :: calledweight
    External                        :: procname
    Optional                        :: procname
!
    INTEGER :: indic ! indice of the variable in tabvars
!
    REAL    :: weight
    LOGICAL :: pweight
    TYPE(Agrif_PVariable),Pointer ::tabvars,parenttabvars,roottabvars
!
    if (Agrif_Root()) return
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif

    if ( PRESENT(calledweight) ) then
        weight = calledweight
        pweight = .TRUE.
    else
        weight = 0.
        pweight = .FALSE.
    endif

    if (indic <=0) then
        tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
        parenttabvars => tabvars%parent_var
        roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
    else
        tabvars=>Agrif_Curgrid % tabvars(indic)
        parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
        roottabvars => Agrif_Mygrid % tabvars(indic)
    endif

    if (present(procname)) then
        call Agrif_Interp_Bc_5D(roottabvars % var % bctypeinterp,   &
                                parenttabvars, tabvars, q,          &
                                tabvars % var % bcinf,              &
                                tabvars % var % bcsup,              &
                                weight, pweight, procname)
    else
        call Agrif_Interp_Bc_5D(roottabvars % var % bctypeinterp,   &
                                parenttabvars, tabvars, q,          &
                                tabvars % var % bcinf,              &
                                tabvars % var % bcsup,              &
                                weight, pweight)
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Bc_variable5d
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_var0D
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_var0d ( tabvarsindic0, tabvarsindic, procname )
!---------------------------------------------------------------------------------------------------
    INTEGER,  intent(in) :: tabvarsindic0 !< indice of the variable in tabvars
    INTEGER,  intent(in) :: tabvarsindic  !< indice of the variable in tabvars
    External :: procname
    Optional ::  procname
!
    INTEGER :: dimensio
    INTEGER :: indic  ! indice of the variable in tabvars
!
    if (Agrif_Root()) return
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif
!
    dimensio = Agrif_Mygrid % tabvars(indic) % var % nbdim
!
    if ( dimensio == 1 ) then
        if (present(procname)) then
            call Agrif_Interp_1D( Agrif_Mygrid % tabvars(indic) % var %  TypeInterp,        &
                                  Agrif_Curgrid % parent % tabvars(indic),                  &
                                  Agrif_Curgrid % tabvars(indic),                           &
                                  Agrif_Curgrid % tabvars(tabvarsindic0) % var % array1 ,   &
                                  Agrif_Mygrid % tabvars(indic) % var % restaure,           &
                                  Agrif_Mygrid % tabvars(indic) %var % nbdim, procname )
        else
            call Agrif_Interp_1D( Agrif_Mygrid % tabvars(indic) % var %  TypeInterp,        &
                                  Agrif_Curgrid % parent % tabvars(indic),                  &
                                  Agrif_Curgrid % tabvars(indic),                           &
                                  Agrif_Curgrid % tabvars(tabvarsindic0) % var % array1 ,   &
                                  Agrif_Mygrid % tabvars(indic) % var % restaure,           &
                                  Agrif_Mygrid % tabvars(indic) %var % nbdim )
        endif
    endif
!
    if ( dimensio == 2 ) then
        if (present(procname)) then
            call Agrif_Interp_2D( Agrif_Mygrid % tabvars(indic) % var %  TypeInterp,        &
                                  Agrif_Curgrid % parent % tabvars(indic),                  &
                                  Agrif_Curgrid % tabvars(indic),                           &
                                  Agrif_Curgrid % tabvars(tabvarsindic0) % var % array2 ,   &
                                  Agrif_Mygrid % tabvars(indic) % var % restaure,           &
                                  Agrif_Mygrid % tabvars(indic) %var % nbdim, procname )
        else
            call Agrif_Interp_2D( Agrif_Mygrid % tabvars(indic) % var %  TypeInterp,        &
                                  Agrif_Curgrid % parent % tabvars(indic),                  &
                                  Agrif_Curgrid % tabvars(indic),                           &
                                  Agrif_Curgrid % tabvars(tabvarsindic0) % var % array2 ,   &
                                  Agrif_Mygrid % tabvars(indic) % var % restaure,           &
                                  Agrif_Mygrid % tabvars(indic) %var % nbdim )
        endif
    endif
!
    if ( dimensio == 3 ) then
        if (present(procname)) then
            call Agrif_Interp_3D( Agrif_Mygrid % tabvars(indic) % var %  TypeInterp,        &
                                  Agrif_Curgrid % parent % tabvars(indic),                  &
                                  Agrif_Curgrid % tabvars(indic),                           &
                                  Agrif_Curgrid % tabvars(tabvarsindic0) % var % array3 ,   &
                                  Agrif_Mygrid % tabvars(indic) % var % restaure,           &
                                  Agrif_Mygrid % tabvars(indic) %var % nbdim, procname )
        else
            call Agrif_Interp_3D( Agrif_Mygrid % tabvars(indic) % var %  TypeInterp,        &
                                  Agrif_Curgrid % parent % tabvars(indic),                  &
                                  Agrif_Curgrid % tabvars(indic),                           &
                                  Agrif_Curgrid % tabvars(tabvarsindic0) % var % array3 ,   &
                                  Agrif_Mygrid % tabvars(indic) % var % restaure,           &
                                  Agrif_Mygrid % tabvars(indic) %var % nbdim )
        endif
    endif
!
    if ( dimensio == 4 ) then
        if (present(procname)) then
            call Agrif_Interp_4D( Agrif_Mygrid % tabvars(indic) % var %  TypeInterp,        &
                                  Agrif_Curgrid % parent % tabvars(indic),                  &
                                  Agrif_Curgrid % tabvars(indic),                           &
                                  Agrif_Curgrid % tabvars(tabvarsindic0) % var % array4 ,   &
                                  Agrif_Mygrid % tabvars(indic) % var % restaure,           &
                                  Agrif_Mygrid % tabvars(indic) %var % nbdim, procname )
        else
            call Agrif_Interp_4D( Agrif_Mygrid % tabvars(indic) % var %  TypeInterp,        &
                                  Agrif_Curgrid % parent % tabvars(indic),                  &
                                  Agrif_Curgrid % tabvars(indic),                           &
                                  Agrif_Curgrid % tabvars(tabvarsindic0) % var % array4 ,   &
                                  Agrif_Mygrid % tabvars(indic) % var % restaure,           &
                                  Agrif_Mygrid % tabvars(indic) %var % nbdim )
        endif
    endif
!
    if ( dimensio == 5 ) then
        if (present(procname)) then
            call Agrif_Interp_5D( Agrif_Mygrid % tabvars(indic) % var %  TypeInterp,        &
                                  Agrif_Curgrid % parent % tabvars(indic),                  &
                                  Agrif_Curgrid % tabvars(indic),                           &
                                  Agrif_Curgrid % tabvars(tabvarsindic0) % var % array5 ,   &
                                  Agrif_Mygrid % tabvars(indic) % var % restaure,           &
                                  Agrif_Mygrid % tabvars(indic) %var % nbdim, procname )
        else
            call Agrif_Interp_5D( Agrif_Mygrid % tabvars(indic) % var %  TypeInterp,        &
                                  Agrif_Curgrid % parent % tabvars(indic),                  &
                                  Agrif_Curgrid % tabvars(indic),                           &
                                  Agrif_Curgrid % tabvars(tabvarsindic0) % var % array5 ,   &
                                  Agrif_Mygrid % tabvars(indic) % var % restaure,           &
                                  Agrif_Mygrid % tabvars(indic) %var % nbdim )
        endif
    endif
!
    if ( dimensio == 6 ) then
        if (present(procname)) then
            call Agrif_Interp_6D( Agrif_Mygrid % tabvars(indic) % var %  TypeInterp,        &
                                  Agrif_Curgrid % parent % tabvars(indic),                  &
                                  Agrif_Curgrid % tabvars(indic),                           &
                                  Agrif_Curgrid % tabvars(tabvarsindic0) % var % array6 ,   &
                                  Agrif_Mygrid % tabvars(indic) % var % restaure,           &
                                  Agrif_Mygrid % tabvars(indic) %var % nbdim, procname )
        else
            call Agrif_Interp_6D( Agrif_Mygrid % tabvars(indic) % var %  TypeInterp,        &
                                  Agrif_Curgrid % parent % tabvars(indic),                  &
                                  Agrif_Curgrid % tabvars(indic),                           &
                                  Agrif_Curgrid % tabvars(tabvarsindic0) % var % array6 ,   &
                                  Agrif_Mygrid % tabvars(indic) % var % restaure,           &
                                  Agrif_Mygrid % tabvars(indic) %var % nbdim )
        endif
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_var0d
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_var1d
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_var1d ( q, tabvarsindic, procname )
!---------------------------------------------------------------------------------------------------
    REAL, DIMENSION(:)  :: q
    INTEGER, INTENT(in) :: tabvarsindic !< indice of the variable in tabvars
    External :: procname
    Optional :: procname
!
    TYPE(Agrif_PVariable), pointer ::tabvars, parenttabvars, roottabvars
    INTEGER :: indic
!
    if (Agrif_Root()) return
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif

    if (indic <=0) then
        tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
        parenttabvars => tabvars%parent_var
        roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
    else
        tabvars => Agrif_Curgrid % tabvars(indic)
        parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
        roottabvars => Agrif_Mygrid % tabvars(indic)
    endif

    if (present(procname)) then
        call Agrif_Interp_1D(roottabvars % var % TypeInterp, parenttabvars, tabvars, q, &
                             roottabvars % var % restaure, roottabvars %var % nbdim, procname)
    else
        call Agrif_Interp_1D(roottabvars % var % TypeInterp, parenttabvars, tabvars, q, &
                             roottabvars % var % restaure, roottabvars %var % nbdim)
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_var1d
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_var2d
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_var2d ( q, tabvarsindic, procname )
!---------------------------------------------------------------------------------------------------
    REAL, DIMENSION(:,:)    :: q
    INTEGER, INTENT(in)     :: tabvarsindic !< indice of the variable in tabvars
    External :: procname
    Optional :: procname
!
    TYPE(Agrif_PVariable), pointer ::tabvars, parenttabvars, roottabvars
    INTEGER :: indic
!
    if (Agrif_Root()) return
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif

    if (indic <=0) then
        tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
        parenttabvars => tabvars%parent_var
        roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
        if (tabvars%var%restaure) then
            if (Agrif_Curgrid%ngridstep == 0) then
                call Agrif_CopyFromOld_AllOneVar(Agrif_Curgrid,Agrif_OldMygrid,indic)
            endif
        endif
    else
        tabvars => Agrif_Curgrid % tabvars(indic)
        parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
        roottabvars => Agrif_Mygrid % tabvars(indic)
    endif

    if (present(procname)) then
        call Agrif_Interp_2D(roottabvars % var % TypeInterp, parenttabvars, tabvars, q, &
                             roottabvars % var % restaure, roottabvars %var % nbdim, procname)
    else
        call Agrif_Interp_2D(roottabvars % var % TypeInterp, parenttabvars, tabvars, q, &
                             roottabvars % var % restaure, roottabvars %var % nbdim)
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_var2d
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_var3d
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_var3d(q,tabvarsindic,procname)
!---------------------------------------------------------------------------------------------------
    REAL, DIMENSION(:,:,:)      :: q
    INTEGER, INTENT(in)         :: tabvarsindic !< indice of the variable in tabvars
    External :: procname
    Optional :: procname
!
    TYPE(Agrif_PVariable), pointer :: tabvars, parenttabvars, roottabvars
    INTEGER :: indic
!
    if (Agrif_Root()) return
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif

    if (indic <=0) then
        tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
        parenttabvars => tabvars%parent_var
        roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
        if (tabvars%var%restaure) then
            if (Agrif_Curgrid%ngridstep == 0) then
                call Agrif_CopyFromOld_AllOneVar(Agrif_Curgrid,Agrif_OldMygrid,indic)
            endif
        endif
    else
        tabvars => Agrif_Curgrid % tabvars(indic)
        parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
        roottabvars => Agrif_Mygrid % tabvars(indic)
    endif

    if (present(procname)) then
        call Agrif_Interp_3D(roottabvars % var % TypeInterp, parenttabvars, tabvars, q, &
                             roottabvars % var % restaure, roottabvars %var % nbdim, procname)
    else
        call Agrif_Interp_3D(roottabvars % var % TypeInterp, parenttabvars, tabvars, q, &
                             roottabvars % var % restaure, roottabvars %var % nbdim)
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_var3d
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_var4d
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_var4d(q,tabvarsindic,procname)
!---------------------------------------------------------------------------------------------------
    REAL, DIMENSION(:,:,:,:)    :: q
    INTEGER, INTENT(in)         :: tabvarsindic !< indice of the variable in tabvars
    External :: procname
    Optional :: procname
!
    TYPE(Agrif_PVariable), pointer :: tabvars, parenttabvars, roottabvars
    INTEGER :: indic
!
    if (Agrif_Root()) return
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif

    if (indic <=0) then
        tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
        parenttabvars => tabvars%parent_var
        roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
        if (tabvars%var%restaure) then
            if (Agrif_Curgrid%ngridstep == 0) then
                call Agrif_CopyFromOld_AllOneVar(Agrif_Curgrid,Agrif_OldMygrid,indic)
            endif
        endif
    else
        tabvars => Agrif_Curgrid % tabvars(indic)
        parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
        roottabvars => Agrif_Mygrid % tabvars(indic)
    endif

    if (present(procname)) then
        call Agrif_Interp_4D(roottabvars % var % TypeInterp, parenttabvars, tabvars, q, &
                             roottabvars % var % restaure, roottabvars %var % nbdim, procname)
    else
        call Agrif_Interp_4D(roottabvars % var % TypeInterp, parenttabvars, tabvars, q, &
                             roottabvars % var % restaure, roottabvars %var % nbdim)
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_var4d
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Interp_var5d
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Interp_var5d(q,tabvarsindic,procname)
!---------------------------------------------------------------------------------------------------
    REAL, DIMENSION(:,:,:,:,:)      :: q
    INTEGER, INTENT(in)             :: tabvarsindic !< indice of the variable in tabvars
    External :: procname
    Optional :: procname
!
    TYPE(Agrif_PVariable), pointer :: tabvars, parenttabvars, roottabvars
    INTEGER :: indic
!
    if (Agrif_Root()) return
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif

    if (indic <=0) then
        tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
        parenttabvars => tabvars%parent_var
        roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
    else
        tabvars => Agrif_Curgrid % tabvars(indic)
        parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
        roottabvars => Agrif_Mygrid % tabvars(indic)
    endif

    if (present(procname)) then
        call Agrif_Interp_5D(roottabvars % var % TypeInterp, parenttabvars, tabvars, q, &
                             roottabvars % var % restaure, roottabvars %var % nbdim, procname)
    else
        call Agrif_Interp_5D(roottabvars % var % TypeInterp, parenttabvars, tabvars, q, &
                             roottabvars % var % restaure, roottabvars %var % nbdim)
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Interp_var5d
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_update_var0d
!---------------------------------------------------------------------------------------------------
subroutine Agrif_update_var0d ( tabvarsindic0, tabvarsindic, &
                                locupdate, locupdate1, locupdate2, procname )
!---------------------------------------------------------------------------------------------------
    INTEGER, INTENT(in) :: tabvarsindic     ! indice of the variable in tabvars
    INTEGER, INTENT(in) :: tabvarsindic0    ! indice of the variable in tabvars
    INTEGER, DIMENSION(2), INTENT(in), OPTIONAL :: locupdate
    INTEGER, DIMENSION(2), INTENT(in), OPTIONAL :: locupdate1
    INTEGER, DIMENSION(2), INTENT(in), OPTIONAL :: locupdate2
    External :: procname
    Optional :: procname
!
    INTEGER :: indic
    INTEGER :: dimensio
    TYPE(Agrif_PVariable), pointer :: tabvars, parenttabvars, roottabvars
!
    if (Agrif_Root()) return
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif

    if (indic <=0) then
        tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
        parenttabvars => tabvars%parent_var
        roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
    else
        tabvars => Agrif_Curgrid % tabvars(indic)
        parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
        roottabvars => Agrif_Mygrid % tabvars(indic)
    endif
!
    dimensio = roottabvars % var % nbdim
!
    if (present(locupdate)) then
        tabvars % var % updateinf(1:dimensio) = locupdate(1)
        tabvars % var % updatesup(1:dimensio) = locupdate(2)
    else
        tabvars % var % updateinf(1:dimensio) = -99
        tabvars % var % updatesup(1:dimensio) = -99
    endif

    if (present(locupdate1)) then
        tabvars%var % updateinf(1) = locupdate1(1)
        tabvars%var % updatesup(1) = locupdate1(2)
    endif

    if (present(locupdate2)) then
        tabvars%var % updateinf(2) = locupdate2(1)
        tabvars%var % updatesup(2) = locupdate2(2)
    endif

    if (present(procname)) then
        call Agrif_UpdateVariable( roottabvars % var % typeupdate,      &
                                   parenttabvars, tabvars,              &
                                   tabvars % var % updateinf,           &
                                   tabvars % var % updatesup, procname )
    else
        call Agrif_UpdateVariable( roottabvars % var % typeupdate,      &
                                   parenttabvars, tabvars,              &
                                   tabvars % var % updateinf,           &
                                   tabvars % var % updatesup )
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_update_var0d
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_update_var1d
!---------------------------------------------------------------------------------------------------
subroutine Agrif_update_var1d ( q, tabvarsindic, locupdate, locupdate1, locupdate2, procname )
!---------------------------------------------------------------------------------------------------
    REAL, DIMENSION(:)      :: q
    INTEGER, INTENT(in)     :: tabvarsindic     ! indice of the variable in tabvars
    INTEGER, DIMENSION(2), INTENT(in), OPTIONAL :: locupdate
    INTEGER, DIMENSION(2), INTENT(in), OPTIONAL :: locupdate1
    INTEGER, DIMENSION(2), INTENT(in), OPTIONAL :: locupdate2
    External :: procname
    Optional :: procname
!
    TYPE(Agrif_PVariable), pointer :: tabvars, parenttabvars, roottabvars
    INTEGER :: indic ! indice of the variable in tabvars
    INTEGER :: dimensio
!
    if (Agrif_Root()) return
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif

    if (indic <=0) then
        tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
        parenttabvars => tabvars%parent_var
        roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
    else
        tabvars => Agrif_Curgrid % tabvars(indic)
        parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
        roottabvars => Agrif_Mygrid % tabvars(indic)
    endif
!
    dimensio = roottabvars % var % nbdim
!
    if (present(locupdate)) then
        tabvars % var % updateinf(1:dimensio) = locupdate(1)
        tabvars % var % updatesup(1:dimensio) = locupdate(2)
    else
        tabvars % var % updateinf(1:dimensio) = -99
        tabvars % var % updatesup(1:dimensio) = -99
    endif

    if (present(locupdate1)) then
        tabvars%var % updateinf(1) = locupdate1(1)
        tabvars%var % updatesup(1) = locupdate1(2)
    endif

    if (present(locupdate2)) then
        tabvars%var % updateinf(2) = locupdate2(1)
        tabvars%var % updatesup(2) = locupdate2(2)
    endif

    if (present(procname)) then
        call Agrif_UpdateVariable( roottabvars % var % typeupdate, parenttabvars, tabvars,  &
                                   tabvars % var % updateinf,                               &
                                   tabvars % var % updatesup, procname )
    else
        call Agrif_UpdateVariable( roottabvars % var % typeupdate, parenttabvars, tabvars,  &
                                   tabvars % var % updateinf,                               &
                                   tabvars % var % updatesup )
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_update_var1d
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_update_var2d
!---------------------------------------------------------------------------------------------------
subroutine Agrif_update_var2d(q,tabvarsindic,locupdate, locupdate1,locupdate2,procname)
!---------------------------------------------------------------------------------------------------
    REAL, DIMENSION(:,:)    :: q
    INTEGER, INTENT(in)     :: tabvarsindic     ! indice of the variable in tabvars
    INTEGER, DIMENSION(2), INTENT(in), OPTIONAL :: locupdate
    INTEGER, DIMENSION(2), INTENT(in), OPTIONAL :: locupdate1
    INTEGER, DIMENSION(2), INTENT(in), OPTIONAL :: locupdate2
    External :: procname
    Optional :: procname
!
    TYPE(Agrif_PVariable), pointer :: tabvars, parenttabvars, roottabvars
    INTEGER :: indic ! indice of the variable in tabvars
    INTEGER :: dimensio
!
    if (Agrif_Root()) return
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif

    if (indic <=0) then
        tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
        parenttabvars => tabvars%parent_var
        roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
    else
        tabvars => Agrif_Curgrid % tabvars(indic)
        parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
        roottabvars => Agrif_Mygrid % tabvars(indic)
    endif
!
    dimensio = roottabvars % var % nbdim
!
    if (present(locupdate)) then
        tabvars % var % updateinf(1:dimensio) = locupdate(1)
        tabvars % var % updatesup(1:dimensio) = locupdate(2)
    else
        tabvars % var % updateinf(1:dimensio) = -99
        tabvars % var % updatesup(1:dimensio) = -99
    endif

    if (present(locupdate1)) then
        tabvars%var % updateinf(1) = locupdate1(1)
        tabvars%var % updatesup(1) = locupdate1(2)
    endif

    if (present(locupdate2)) then
        tabvars%var % updateinf(2) = locupdate2(1)
        tabvars%var % updatesup(2) = locupdate2(2)
    endif

    if (present(procname)) then
        call Agrif_UpdateVariable( roottabvars % var % typeupdate, parenttabvars, tabvars,  &
                                   tabvars % var % updateinf,                               &
                                   tabvars % var % updatesup, procname )
    else
        call Agrif_UpdateVariable( roottabvars % var % typeupdate, parenttabvars, tabvars,  &
                                   tabvars % var % updateinf,                               &
                                   tabvars % var % updatesup )
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_update_var2d
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_update_var3d
!---------------------------------------------------------------------------------------------------
subroutine Agrif_update_var3d ( q, tabvarsindic, locupdate, locupdate1, locupdate2, procname )
!---------------------------------------------------------------------------------------------------
    REAL, DIMENSION(:,:,:)  :: q
    INTEGER, INTENT(in)     :: tabvarsindic     ! indice of the variable in tabvars
    INTEGER, DIMENSION(2), INTENT(in), OPTIONAL :: locupdate
    INTEGER, DIMENSION(2), INTENT(in), OPTIONAL :: locupdate1
    INTEGER, DIMENSION(2), INTENT(in), OPTIONAL :: locupdate2
    External :: procname
    Optional :: procname
!
    TYPE(Agrif_PVariable), pointer :: tabvars, parenttabvars, roottabvars
    INTEGER :: indic ! indice of the variable in tabvars
    INTEGER :: dimensio
!
    if (Agrif_Root()) return
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif

    if (indic <=0) then
        tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
        parenttabvars => tabvars%parent_var
        roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
    else
        tabvars => Agrif_Curgrid % tabvars(indic)
        parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
        roottabvars => Agrif_Mygrid % tabvars(indic)
    endif
!
    dimensio = roottabvars % var % nbdim
!
    if (present(locupdate)) then
        tabvars % var % updateinf(1:dimensio) = locupdate(1)
        tabvars % var % updatesup(1:dimensio) = locupdate(2)
    else
        tabvars % var % updateinf(1:dimensio) = -99
        tabvars % var % updatesup(1:dimensio) = -99
    endif

    if (present(locupdate1)) then
        tabvars%var % updateinf(1) = locupdate1(1)
        tabvars%var % updatesup(1) = locupdate1(2)
    endif

    if (present(locupdate2)) then
        tabvars%var % updateinf(2) = locupdate2(1)
        tabvars%var % updatesup(2) = locupdate2(2)
    endif

    if (present(procname)) then
        call Agrif_UpdateVariable( roottabvars % var % typeupdate, parenttabvars, tabvars,  &
                                   tabvars % var % updateinf,                               &
                                   tabvars % var % updatesup, procname )
    else
        call Agrif_UpdateVariable( roottabvars % var % typeupdate, parenttabvars, tabvars,  &
                                   tabvars % var % updateinf,                               &
                                   tabvars % var % updatesup )
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_update_var3d
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_update_var4d
!---------------------------------------------------------------------------------------------------
subroutine Agrif_update_var4d ( q, tabvarsindic, locupdate, locupdate1, locupdate2, procname )
!---------------------------------------------------------------------------------------------------
    REAL, DIMENSION(:,:,:,:)    :: q
    INTEGER, INTENT(in)         :: tabvarsindic     ! indice of the variable in tabvars
    INTEGER, DIMENSION(2), INTENT(in), OPTIONAL :: locupdate
    INTEGER, DIMENSION(2), INTENT(in), OPTIONAL :: locupdate1
    INTEGER, DIMENSION(2), INTENT(in), OPTIONAL :: locupdate2
    External :: procname
    Optional :: procname
!
    TYPE(Agrif_PVariable), pointer :: tabvars, parenttabvars, roottabvars
    INTEGER :: indic ! indice of the variable in tabvars
    INTEGER :: dimensio
!
    if (Agrif_Root()) return
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif

    if (indic <=0) then
        tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
        parenttabvars => tabvars%parent_var
        roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
    else
        tabvars => Agrif_Curgrid % tabvars(indic)
        parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
        roottabvars => Agrif_Mygrid % tabvars(indic)
    endif
!
    dimensio = roottabvars % var % nbdim
!
    if (present(locupdate)) then
        tabvars % var % updateinf(1:dimensio) = locupdate(1)
        tabvars % var % updatesup(1:dimensio) = locupdate(2)
    else
        tabvars % var % updateinf(1:dimensio) = -99
        tabvars % var % updatesup(1:dimensio) = -99
    endif

    if (present(locupdate1)) then
        tabvars%var % updateinf(1) = locupdate1(1)
        tabvars%var % updatesup(1) = locupdate1(2)
    endif

    if (present(locupdate2)) then
        tabvars%var % updateinf(2) = locupdate2(1)
        tabvars%var % updatesup(2) = locupdate2(2)
    endif

    if (present(procname)) then
        call Agrif_UpdateVariable( roottabvars % var % typeupdate, parenttabvars, tabvars,  &
                                   tabvars % var % updateinf,                               &
                                   tabvars % var % updatesup, procname )
    else
        call Agrif_UpdateVariable( roottabvars % var % typeupdate, parenttabvars, tabvars,  &
                                   tabvars % var % updateinf,                               &
                                   tabvars % var % updatesup )
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_update_var4d
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_update_var5d
!---------------------------------------------------------------------------------------------------
subroutine Agrif_update_var5d ( q, tabvarsindic, locupdate, locupdate1, locupdate2, procname )
!---------------------------------------------------------------------------------------------------
    REAL, DIMENSION(:,:,:,:,:)  :: q
    INTEGER, INTENT(in)         :: tabvarsindic     ! indice of the variable in tabvars
    INTEGER, DIMENSION(2), INTENT(in), OPTIONAL :: locupdate
    INTEGER, DIMENSION(2), INTENT(in), OPTIONAL :: locupdate1
    INTEGER, DIMENSION(2), INTENT(in), OPTIONAL :: locupdate2
    External :: procname
    Optional :: procname
!
    TYPE(Agrif_PVariable), pointer :: tabvars, parenttabvars, roottabvars
    INTEGER :: indic ! indice of the variable in tabvars
    INTEGER :: dimensio
!
    if (Agrif_Root()) return
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif

    if (indic <=0) then
        tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
        parenttabvars => tabvars%parent_var
        roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
    else
        tabvars => Agrif_Curgrid % tabvars(indic)
        parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
        roottabvars => Agrif_Mygrid % tabvars(indic)
    endif
!
    dimensio = roottabvars % var % nbdim
!
    if (present(locupdate)) then
        tabvars % var % updateinf(1:dimensio) = locupdate(1)
        tabvars % var % updatesup(1:dimensio) = locupdate(2)
    else
        tabvars % var % updateinf(1:dimensio) = -99
        tabvars % var % updatesup(1:dimensio) = -99
    endif

    if (present(locupdate1)) then
        tabvars%var % updateinf(1) = locupdate1(1)
        tabvars%var % updatesup(1) = locupdate1(2)
    endif

    if (present(locupdate2)) then
        tabvars%var % updateinf(2) = locupdate2(1)
        tabvars%var % updatesup(2) = locupdate2(2)
    endif

    if (present(procname)) then
        call Agrif_UpdateVariable( roottabvars % var % typeupdate, parenttabvars, tabvars,  &
                                   tabvars % var % updateinf,                               &
                                   tabvars % var % updatesup, procname )
    else
        call Agrif_UpdateVariable( roottabvars % var % typeupdate, parenttabvars, tabvars,  &
                                   tabvars % var % updateinf,                               &
                                   tabvars % var % updatesup )
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_update_var5d
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Declare_Flux
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Declare_Flux ( fluxname, profilename )
!---------------------------------------------------------------------------------------------------
    character*(*) :: fluxname, profilename
!
    Type(Agrif_Flux), pointer :: newflux
    Type(Agrif_Profile), pointer  :: parcours
    logical :: foundprofile

    foundprofile = .FALSE.
    parcours => Agrif_Myprofiles

    Do While (Associated(parcours))
        if (parcours % profilename == profilename) then
            foundprofile = .TRUE.
            EXIT
        endif
        parcours => parcours%nextprofile
    end Do

    if (.NOT.foundprofile) then
        write(*,*) "The profile "//TRIM(profilename)//" has not been declared"
        stop
    endif

    allocate(Newflux)

    Newflux % fluxname = fluxname
    Newflux % profile => parcours
    Newflux % nextflux => Agrif_Curgrid % fluxes

    Agrif_Curgrid % fluxes => Newflux
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Declare_Flux
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Save_Flux
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Save_Flux ( fluxname, fluxtab )
!---------------------------------------------------------------------------------------------------
    character*(*) :: fluxname
    REAL, DIMENSION(:,:) :: fluxtab

    Type(Agrif_Flux),  pointer :: Flux
    Type(Agrif_PGrid), pointer :: parcours_child
    Type(Agrif_Grid),  pointer :: currentgrid,oldcurgrid

    if (.Not.Agrif_Root()) then
        Flux => Agrif_Search_Flux(fluxname)
        if (.NOT.Flux%fluxallocated) then
            call Agrif_allocateFlux(Flux,fluxtab)
        endif

        call Agrif_Save_Fluxtab(Flux,fluxtab)
    endif

    oldcurgrid => Agrif_Curgrid
    parcours_child => Agrif_Curgrid%child_grids

    Do While (Associated(parcours_child))
        currentgrid => parcours_child%gr
        Agrif_Curgrid => parcours_child%gr
        Flux => Agrif_Search_Flux(fluxname)
        if (.NOT.Flux%fluxallocated) then
            call Agrif_allocateFlux(Flux,fluxtab)
        endif
        call Agrif_Save_Fluxtab_child(Flux,fluxtab)
        parcours_child=> parcours_child%next
    end Do

    Agrif_Curgrid => oldcurgrid
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Save_Flux
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Cancel_Flux
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Cancel_Flux(fluxname)
!---------------------------------------------------------------------------------------------------
    character*(*) :: fluxname

    Type(Agrif_Flux), pointer :: Flux

    Flux => Agrif_Search_Flux(fluxname)

    if (Flux%Fluxallocated) call Agrif_Cancel_Fluxarray(Flux)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Cancel_Flux
!===================================================================================================
!
!===================================================================================================
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Flux_Correction(fluxname, procname)
!---------------------------------------------------------------------------------------------------
    character*(*) :: fluxname
    external :: procname

    Type(Agrif_Flux), pointer :: Flux

    Flux => Agrif_Search_Flux(fluxname)

    call Agrif_FluxCorrect(Flux, procname)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Flux_Correction
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Declare_Profile_flux
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Declare_Profile_flux(profilename,posvar,firstpoint,raf)
!---------------------------------------------------------------------------------------------------
    character*(*) :: profilename
    Type(Agrif_Profile), Pointer :: newprofile
    INTEGER, DIMENSION(:) :: posvar
    INTEGER, DIMENSION(:) :: firstpoint
    CHARACTER(*) ,DIMENSION(:) :: raf
    INTEGER :: dimensio
!
    dimensio = SIZE(posvar)
!
    allocate(newprofile)
    allocate(newprofile%posvar(dimensio))
    allocate(newprofile%interptab(dimensio))
    newprofile % profilename = profilename
    newprofile % interptab = raf
    newprofile % nbdim = dimensio
    newprofile % posvar = posvar
    newprofile % point(1:dimensio) = firstpoint
    newprofile % nextprofile => Agrif_myprofiles
    Agrif_myprofiles => newprofile
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Declare_Profile_flux
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Save_ForRestore0D
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Save_ForRestore0D ( tabvarsindic0, tabvarsindic )
!---------------------------------------------------------------------------------------------------
    integer, intent(in) :: tabvarsindic0, tabvarsindic
!
    integer :: dimensio
!
    dimensio =  Agrif_Mygrid % tabvars(tabvarsindic0) % var % nbdim
!
    select case(dimensio)
    case(2)
        call Agrif_Save_ForRestore2D(Agrif_Curgrid % tabvars(tabvarsindic0) % var % array2,tabvarsindic)
    case(3)
        call Agrif_Save_ForRestore3D(Agrif_Curgrid % tabvars(tabvarsindic0) % var % array3,tabvarsindic)
    case(4)
        call Agrif_Save_ForRestore4D(Agrif_Curgrid % tabvars(tabvarsindic0) % var % array4,tabvarsindic)
    end select
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Save_ForRestore0D
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Save_ForRestore2D
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Save_ForRestore2D ( q, tabvarsindic )
!---------------------------------------------------------------------------------------------------
    real, dimension(:,:), intent(in)    :: q
    integer,              intent(in)    :: tabvarsindic
!
    type(Agrif_PVariable), pointer  :: tabvars, roottabvars
    integer                         :: indic
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif
!
    if (indic <=0) then
        tabvars     => Agrif_Search_Variable(Agrif_Curgrid,-indic)
        roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
    else
        tabvars     => Agrif_Curgrid % tabvars(indic)
        roottabvars => Agrif_Mygrid % tabvars(indic)
    endif
!
    if (.not.allocated(tabvars%var%array2)) then
        allocate(tabvars%var%array2(tabvars%var%lb(1):tabvars%var%ub(1),  &
                 tabvars%var%lb(2):tabvars%var%ub(2)))
    endif
!
    tabvars%var%array2 = q
    roottabvars%var%restaure = .true.
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Save_ForRestore2D
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Save_ForRestore3D
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Save_ForRestore3D ( q, tabvarsindic )
!---------------------------------------------------------------------------------------------------
    real, dimension(:,:,:), intent(in)  :: q
    integer,                intent(in)  :: tabvarsindic
!
    type(Agrif_PVariable), pointer  :: tabvars, roottabvars
    integer                         :: indic
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif
!
    if (indic <=0) then
        tabvars     => Agrif_Search_Variable(Agrif_Curgrid,-indic)
        roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
    else
        tabvars     => Agrif_Curgrid % tabvars(indic)
        roottabvars => Agrif_Mygrid % tabvars(indic)
    endif
!
    if (.not.allocated(tabvars%var%array3)) then
        allocate(tabvars%var%array3(                     &
                    tabvars%var%lb(1):tabvars%var%ub(1), &
                    tabvars%var%lb(2):tabvars%var%ub(2), &
                    tabvars%var%lb(3):tabvars%var%ub(3)))
    endif
!
    tabvars%var%array3 = q
    roottabvars%var%restaure = .true.
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Save_ForRestore3D
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Save_ForRestore4D
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Save_ForRestore4D ( q, tabvarsindic )
!---------------------------------------------------------------------------------------------------
    real, dimension(:,:,:,:), intent(in)    :: q
    integer,                  intent(in)    :: tabvarsindic
!
    type(Agrif_PVariable), pointer  :: tabvars, roottabvars
    integer                         :: indic
!
    indic = tabvarsindic
    if (tabvarsindic >=0) then
        if (Agrif_Curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
            indic = Agrif_Curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
    endif
!
    if (indic <=0) then
        tabvars     => Agrif_Search_Variable(Agrif_Curgrid,-indic)
        roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
    else
        tabvars     => Agrif_Curgrid % tabvars(indic)
        roottabvars => Agrif_Mygrid % tabvars(indic)
    endif
!
    if (.not.allocated(tabvars%var%array4)) then
        allocate(tabvars%var%array4(                    &
                    tabvars%var%lb(1):tabvars%var%ub(1),&
                    tabvars%var%lb(2):tabvars%var%ub(2),&
                    tabvars%var%lb(3):tabvars%var%ub(3),&
                    tabvars%var%lb(4):tabvars%var%ub(4)))
    endif
!
    tabvars%var%array4 = q
    roottabvars%var%restaure = .true.
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Save_ForRestore4D
!===================================================================================================
!
end module Agrif_BcFunction
