!
! $Id: modutil.F 662 2007-05-25 15:58:52Z opalod $
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
!     Foundation, Inc., 59 Temple Place-  Suite 330, Boston, MA 02111-1307, USA.
!
!> Module Agrif_Util
!!
!! This module contains the two procedures called in the main program :
!! - #Agrif_Init_Grids allows the initialization of the root coarse grid
!! - #Agrif_step allows the creation of the grid hierarchy and the management of the time integration.
!
module Agrif_Util
!
    use Agrif_Clustering
    use Agrif_BcFunction
!
    implicit none
!
contains
!
!===================================================================================================
!  subroutine Agrif_Step
!
!> creates the grid hierarchy and manages the time integration procedure.
!> It is called in the main program.
!> calls subroutines #Agrif_Regrid and #Agrif_Integrate.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Step ( procname )
!---------------------------------------------------------------------------------------------------
    external :: procname
!
#if defined AGRIF_MPI
    integer      :: code
    include 'mpif.h'
    if ( Agrif_Mygrid % ngridstep == 0 ) then
        call MPI_COMM_SIZE(MPI_COMM_WORLD,Agrif_Nbprocs,code)
        call MPI_COMM_RANK(MPI_COMM_WORLD,Agrif_ProcRank,code)
        call MPI_COMM_GROUP(MPI_COMM_WORLD,Agrif_Group,code)
    endif
#endif
!
!   Creation and initialization of the grid hierarchy
!
!   Set the clustering variables
    call Agrif_clustering_def()
!
    if ( Agrif_USE_ONLY_FIXED_GRIDS == 1 ) then
!
        if (Agrif_Mygrid % ngridstep == 0) then
            call Agrif_Regrid()
            call Agrif_Instance(Agrif_Mygrid)
        endif
!
    else
!
        if (mod(Agrif_Mygrid % ngridstep,Agrif_Regridding) == 0) then
            call Agrif_Regrid()
            call Agrif_Instance(Agrif_Mygrid)
        endif
!
    endif
!
!   Time integration of the grid hierarchy
!
    call Agrif_Integrate(Agrif_Mygrid,procname)
!
    if (associated(Agrif_Mygrid%child_grids)) call Agrif_Instance(Agrif_Mygrid)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Step
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Step_Child
!
!> Apply 'procname' to each grid of the hierarchy
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Step_Child ( procname )
!---------------------------------------------------------------------------------------------------
    external :: procname
!
    call Agrif_Integrate_Child(Agrif_Mygrid,procname)
!
    if (associated(Agrif_Mygrid%child_grids)) call Agrif_Instance(Agrif_Mygrid)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Step_Child
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Regrid
!
!> Creates the grid hierarchy from fixed grids and adaptive mesh refinement.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Regrid
!---------------------------------------------------------------------------------------------------
    Type(Agrif_Rectangle), pointer     :: coarsegrid_fixed
    Type(Agrif_Rectangle), pointer     :: coarsegrid_moving
    integer                            :: j
    integer :: nunit
    integer                            :: iii
    logical :: BEXIST
!
    if ( Agrif_USE_ONLY_FIXED_GRIDS == 0 ) &
        call Agrif_detect_all(Agrif_Mygrid)     ! Detection of areas to be refined
!
    allocate(coarsegrid_fixed)
    allocate(coarsegrid_moving)
!
    if ( Agrif_USE_ONLY_FIXED_GRIDS == 0 ) &
        call Agrif_Cluster_All(Agrif_Mygrid,coarsegrid_moving) ! Clustering
!
    if ( Agrif_USE_FIXED_GRIDS == 1 .OR. Agrif_USE_ONLY_FIXED_GRIDS == 1 ) then
!
        if (Agrif_Mygrid % ngridstep == 0) then
!
            nunit = Agrif_Get_Unit()
            open(nunit, file='AGRIF_FixedGrids.in', form='formatted', status="old", ERR=99)
            j = 1
!           Creation of the grid hierarchy from the Agrif_FixedGrids.in file
            do iii = 1,Agrif_Probdim
                coarsegrid_fixed % imin(iii) = 1
                coarsegrid_fixed % imax(iii) = Agrif_Mygrid % nb(iii) + 1
            enddo
            call Agrif_Read_Fix_Grd(coarsegrid_fixed,j,nunit)
            close(nunit)
!
            nullify(Agrif_oldmygrid)
            nullify(Agrif_Mygrid  % child_grids)
!
!           Creation of the grid hierarchy from coarsegrid_fixed
            call Agrif_Create_Grids(Agrif_Mygrid,coarsegrid_fixed)
        else
            Agrif_oldmygrid => Agrif_Mygrid % child_grids
        endif
    else
        Agrif_oldmygrid => Agrif_Mygrid % child_grids
        nullify(Agrif_Mygrid % child_grids)
    endif
!
    if ( Agrif_USE_ONLY_FIXED_GRIDS == 0 ) then
!
        call Agrif_Save_All(Agrif_oldmygrid)
        call Agrif_Free_before_All(Agrif_oldmygrid)
!
!       Creation of the grid hierarchy from coarsegrid_moving
        call Agrif_Create_Grids(Agrif_Mygrid,coarsegrid_moving)
!
    endif
!
!   Initialization of the grid hierarchy by copy or interpolation
!
    call Agrif_Init_Hierarchy(Agrif_Mygrid)
!
    if ( Agrif_USE_ONLY_FIXED_GRIDS == 0 ) call Agrif_Free_after_All(Agrif_oldmygrid)
!
    deallocate(coarsegrid_fixed)
    deallocate(coarsegrid_moving)
!
    return
!
!     Opening error
!
99  INQUIRE(FILE='AGRIF_FixedGrids.in',EXIST=BEXIST)
    if (.not. BEXIST) then
        print*,'ERROR : File AGRIF_FixedGrids.in not found.'
        STOP
    else
        print*,'Error opening file AGRIF_FixedGrids.in'
        STOP
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Regrid
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_detect_All
!
!> Detects areas to be refined.
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_detect_all ( g )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_Grid),  pointer  :: g        !< Pointer on the current grid
!
    Type(Agrif_PGrid), pointer  :: parcours ! Pointer for the recursive procedure
    integer, DIMENSION(3)       :: size
    integer                     :: iii
    real                        :: g_eps
!
    parcours => g % child_grids
!
!   To be positioned on the finer grids of the grid hierarchy
!
    do while (associated(parcours))
        call Agrif_detect_all(parcours % gr)
        parcours => parcours % next
    enddo
!
    g_eps = huge(1.)
    do iii = 1,Agrif_Probdim
        g_eps=min(g_eps, g%Agrif_d(iii))
    enddo
!
    g_eps = g_eps/100.
!
    if ( Agrif_Probdim == 1 ) g%tabpoint1D = 0
    if ( Agrif_Probdim == 2 ) g%tabpoint2D = 0
    if ( Agrif_Probdim == 3 ) g%tabpoint3D = 0
!
    do iii = 1 , Agrif_Probdim
        if (g%Agrif_d(iii)/Agrif_coeffref(iii) < (Agrif_mind(iii)-g_eps)) Return
    enddo
!
    call Agrif_instance(g)
!
!   Detection (Agrif_detect is a users routine)
!
    do iii = 1,Agrif_Probdim
        size(iii) = g%nb(iii) + 1
    enddo
!
    SELECT CASE (Agrif_Probdim)
    CASE (1)
        call Agrif_detect(g%tabpoint1D,size)
    CASE (2)
        call Agrif_detect(g%tabpoint2D,size)
    CASE (3)
        call Agrif_detect(g%tabpoint3D,size)
    END SELECT
!
!   Addition of the areas detected on the child grids
!
    parcours => g % child_grids
!
    do while (associated(parcours))
        call Agrif_Add_detected_areas(g,parcours % gr)
        parcours => parcours % next
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_detect_all
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Add_detected_areas
!
!> Adds on the parent grid the areas detected on its child grids
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Add_detected_areas ( parentgrid, childgrid )
!---------------------------------------------------------------------------------------------------
    Type(Agrif_Grid), pointer   :: parentgrid
    Type(Agrif_Grid), pointer   :: childgrid
!
    integer :: i,j,k
!
    do i = 1,childgrid%nb(1)+1
        if ( Agrif_Probdim == 1 ) then
            if (childgrid%tabpoint1D(i)==1) then
                parentgrid%tabpoint1D(childgrid%ix(1)+(i-1)/Agrif_Coeffref(1)) = 1
            endif
        else
            do j=1,childgrid%nb(2)+1
                if (Agrif_Probdim==2) then
                    if (childgrid%tabpoint2D(i,j)==1) then
                        parentgrid%tabpoint2D(                       &
                            childgrid%ix(1)+(i-1)/Agrif_Coeffref(1), &
                            childgrid%ix(2)+(j-1)/Agrif_Coeffref(2)) = 1
                    endif
                else
                    do k=1,childgrid%nb(3)+1
                        if (childgrid%tabpoint3D(i,j,k)==1) then
                            parentgrid%tabpoint3D(                       &
                                childgrid%ix(1)+(i-1)/Agrif_Coeffref(1), &
                                childgrid%ix(2)+(j-1)/Agrif_Coeffref(2), &
                                childgrid%ix(3)+(k-1)/Agrif_Coeffref(3)) = 1
                        endif
                    enddo
                endif
            enddo
        endif
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Add_detected_areas
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Free_before_All
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_Free_before_All ( g )
!---------------------------------------------------------------------------------------------------
    Type(Agrif_PGrid), pointer   :: g        !< Pointer on the current grid
!
    Type(Agrif_PGrid), pointer   :: parcours ! Pointer for the recursive procedure
!
    parcours => g
!
    do while (associated(parcours))
!
        if (.not. parcours%gr%fixed) then
            call Agrif_Free_data_before(parcours%gr)
            parcours % gr % oldgrid = .TRUE.
        endif
!
        call Agrif_Free_before_all (parcours % gr % child_grids)
!
        parcours => parcours % next
!
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Free_before_All
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Save_All
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_Save_All ( g )
!---------------------------------------------------------------------------------------------------
    Type(Agrif_PGrid), pointer   :: g        !< Pointer on the current grid
!
    Type(Agrif_PGrid), pointer   :: parcours ! Pointer for the recursive procedure
!
    parcours => g
!
    do while (associated(parcours))
!
        if (.not. parcours%gr%fixed) then
            call Agrif_Instance(parcours%gr)
            call Agrif_Before_Regridding()
            parcours % gr % oldgrid = .TRUE.
        endif
!
        call Agrif_Save_All (parcours % gr % child_grids)
!
        parcours => parcours % next
!
      enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Save_All
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Free_after_All
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_Free_after_All ( g )
!---------------------------------------------------------------------------------------------------
    Type(Agrif_PGrid), pointer   :: g       !< Pointer on the current grid
!
    Type(Agrif_PGrid), pointer  :: parcours ! Pointer for the recursive proced
    Type(Agrif_PGrid), pointer  :: preparcours
    Type(Agrif_PGrid), pointer  :: preparcoursini
!
    allocate(preparcours)
!
    preparcoursini => preparcours
!
    nullify(preparcours % gr)
!
    preparcours % next => g
    parcours => g
!
    do while (associated(parcours))
!
        if ( (.NOT. parcours%gr % fixed) .AND. (parcours%gr % oldgrid) ) then
            call Agrif_Free_data_after(parcours%gr)
        endif
!
        call Agrif_Free_after_all (parcours%gr % child_grids)
!
        if (parcours % gr % oldgrid) then
            deallocate(parcours % gr)
            preparcours % next => parcours % next
            deallocate(parcours)
            parcours => preparcours % next
        else
            preparcours => preparcours % next
            parcours => parcours % next
        endif
!
    enddo
!
    deallocate(preparcoursini)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Free_after_All
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Integrate
!
!> Manages the time integration of the grid hierarchy.
!! Recursive subroutine and call on subroutines #Agrif_Instance and #Agrif_Step
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_Integrate ( g, procname )
!---------------------------------------------------------------------------------------------------
    Type(Agrif_Grid), pointer   :: g        !< Pointer on the current grid
    External                    :: procname
!
    Type(Agrif_PGrid), pointer  :: parcours ! Pointer for the recursive procedure
    integer                     :: nbt      ! Number of time steps of the current grid
    integer                     :: k, iii
!
!   Instanciation of the variables of the current grid
    if (g%fixedrank /= 0) then
        call Agrif_Instance(g)
    endif
!
!   One step on the current grid
!
    call procname ()
!
!   Number of time steps on the current grid
!
    g%ngridstep = g % ngridstep + 1
    parcours => g % child_grids
!
!   Recursive procedure for the time integration of the grid hierarchy
    do while (associated(parcours))
!
!       Instanciation of the variables of the current grid
        call Agrif_Instance(parcours % gr)
!
!       Number of time steps
        nbt = 1
        do iii = 1,Agrif_Probdim
            nbt = max(nbt, parcours % gr % timeref(iii))
        enddo
!
        do k = 1,nbt
            call Agrif_Integrate(parcours % gr, procname)
        enddo
!
        parcours => parcours % next
!
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Integrate
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Integrate_Child
!
!> Manages the time integration of the grid hierarchy.
!! Recursive subroutine and call on subroutines Agrif_Instance & Agrif_Step.
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_Integrate_Child ( g, procname )
!---------------------------------------------------------------------------------------------------
    Type(Agrif_Grid), pointer   :: g        !< Pointer on the current grid
    External                    :: procname
!
    Type(Agrif_PGrid), pointer  :: parcours ! Pointer for the recursive procedure
!
!   One step on the current grid
!
    call procname ()
!
!   Number of time steps on the current grid
!
    parcours => g % child_grids
!
!   Recursive procedure for the time integration of the grid hierarchy
    do while (associated(parcours))
!
!       Instanciation of the variables of the current grid
        call Agrif_Instance(parcours % gr)
        call Agrif_Integrate_Child (parcours % gr, procname)
        parcours => parcours % next
!
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Integrate_Child
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Init_Grids
!
!> Initializes the root coarse grid pointed by Agrif_Mygrid. It is called in the main program.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Init_Grids
!---------------------------------------------------------------------------------------------------
    integer :: iii
!
#ifdef AGRIF_MPI
    include 'mpif.h'
    Agrif_MPIPREC = MPI_DOUBLE_PRECISION
#endif
!
    call Agrif_probdim_modtype_def()
!
    Agrif_UseSpecialValue = .FALSE.
    Agrif_UseSpecialValueFineGrid = .FALSE.
    Agrif_SpecialValue = 0.
    Agrif_SpecialValueFineGrid = 0.
!
    allocate(Agrif_Mygrid)
!
!   Space and time refinement factors are set to 1 on the root grid
!
    do iii = 1,Agrif_Probdim
        Agrif_Mygrid % spaceref(iii) = 1
        Agrif_Mygrid % timeref(iii) = 1
    enddo
!
!   Initialization of the number of time steps
    Agrif_Mygrid % ngridstep = 0
    Agrif_Mygrid % grid_id = 0
!
!   No parent grid for the root coarse grid
    nullify(Agrif_Mygrid % parent)
!
!   Initialization of the minimum positions, global abscissa and space steps
    do iii = 1, Agrif_Probdim
        Agrif_Mygrid % ix(iii) = 1
        Agrif_Mygrid % Agrif_x(iii) = 0.
        Agrif_Mygrid % Agrif_d(iii) = 1.
!       Borders of the root coarse grid
        Agrif_Mygrid % NearRootBorder(iii) = .true.
        Agrif_Mygrid % DistantRootBorder(iii) = .true.
    enddo
!
!   The root coarse grid is a fixed grid
    Agrif_Mygrid % fixed = .TRUE.
!   Level of the root grid
    Agrif_Mygrid % level = 0
!   Maximum level in the hierarchy
    Agrif_MaxLevelLoc = 0
!
!   Number of the grid pointed by Agrif_Mygrid (root coarse grid)
    Agrif_Mygrid % rank = 1
!
!   Number of the root grid as a fixed grid
    Agrif_Mygrid % fixedrank = 0
!
!   Initialization of some fields of the root grid variables
    call Agrif_Create_Var(Agrif_Mygrid)
!
!   Initialization of the other fields of the root grid variables (number of
!   cells, positions, number and type of its dimensions, ...)
    call Agrif_Set_numberofcells(Agrif_Mygrid)
    call Agrif_Instance (Agrif_Mygrid)
    call Agrif_Set_numberofcells(Agrif_Mygrid)
!
!   Allocation of the array containing the values of the grid variables
    call Agrif_Allocation (Agrif_Mygrid)
    call Agrif_initialisations(Agrif_Mygrid)
!
    nullify(Agrif_Mygrid % child_grids)
!
!   Total number of fixed grids
    Agrif_nbfixedgrids = 0
!
    call Agrif_Instance (Agrif_Mygrid)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Init_Grids
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Deallocation
!
!> Deallocates all data arrays.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Deallocation
!---------------------------------------------------------------------------------------------------
    integer                         :: nb
    TYPE(Agrif_Variable), pointer   :: var
!
    do nb = 1,Agrif_NbVariables
!
        var => Agrif_Mygrid % tabvars(nb) % var
!
        if ( allocated(var % array1) ) deallocate(var % array1)
        if ( allocated(var % array2) ) deallocate(var % array2)
        if ( allocated(var % array3) ) deallocate(var % array3)
        if ( allocated(var % array4) ) deallocate(var % array4)
        if ( allocated(var % array5) ) deallocate(var % array5)
        if ( allocated(var % array6) ) deallocate(var % array6)
!
        if ( allocated(var % iarray1) ) deallocate(var % iarray1)
        if ( allocated(var % iarray2) ) deallocate(var % iarray2)
        if ( allocated(var % iarray3) ) deallocate(var % iarray3)
        if ( allocated(var % iarray4) ) deallocate(var % iarray4)
        if ( allocated(var % iarray5) ) deallocate(var % iarray5)
        if ( allocated(var % iarray6) ) deallocate(var % iarray6)
!
        if ( allocated(var % sarray1) ) deallocate(var % sarray1)
        if ( allocated(var % sarray2) ) deallocate(var % sarray2)
        if ( allocated(var % sarray3) ) deallocate(var % sarray3)
        if ( allocated(var % sarray4) ) deallocate(var % sarray4)
        if ( allocated(var % sarray5) ) deallocate(var % sarray5)
        if ( allocated(var % sarray6) ) deallocate(var % sarray6)
!
        if ( allocated(var % darray1) ) deallocate(var % darray1)
        if ( allocated(var % darray2) ) deallocate(var % darray2)
        if ( allocated(var % darray3) ) deallocate(var % darray3)
        if ( allocated(var % darray4) ) deallocate(var % darray4)
        if ( allocated(var % darray5) ) deallocate(var % darray5)
        if ( allocated(var % darray6) ) deallocate(var % darray6)
!
        if ( allocated(var % larray1) ) deallocate(var % larray1)
        if ( allocated(var % larray2) ) deallocate(var % larray2)
        if ( allocated(var % larray3) ) deallocate(var % larray3)
        if ( allocated(var % larray4) ) deallocate(var % larray4)
        if ( allocated(var % larray5) ) deallocate(var % larray5)
        if ( allocated(var % larray6) ) deallocate(var % larray6)
!
        if ( allocated(var % carray1) ) deallocate(var % carray1)
        if ( allocated(var % carray2) ) deallocate(var % carray2)
!
        deallocate(var)
!
    enddo
!
    deallocate(Agrif_Mygrid % tabvars)
    deallocate(Agrif_Mygrid)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Deallocation
!===================================================================================================
!
end module Agrif_Util
