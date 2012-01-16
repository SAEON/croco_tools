!
! $Id: modsauv.F 662 2007-05-25 15:58:52Z opalod $
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
!> Module Agrif_Save
!!
!! Module for the initialization by copy of the grids created by clustering.
!
module Agrif_Save
!
    use Agrif_Types
    use Agrif_Link
    use Agrif_Arrays
    use Agrif_Variables
!
    implicit none
!
contains
!
!===================================================================================================
! subroutine Agrif_deallocate_Arrays
!---------------------------------------------------------------------------------------------------
subroutine Agrif_deallocate_Arrays ( Var )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Variable), pointer :: Var
!
    if (allocated(var%array1))  deallocate(var%array1)
    if (allocated(var%array2))  deallocate(var%array2)
    if (allocated(var%array3))  deallocate(var%array3)
    if (allocated(var%array4))  deallocate(var%array4)
    if (allocated(var%array5))  deallocate(var%array5)
    if (allocated(var%array6))  deallocate(var%array6)
!
    if (allocated(var%darray1)) deallocate(var%darray1)
    if (allocated(var%darray2)) deallocate(var%darray2)
    if (allocated(var%darray3)) deallocate(var%darray3)
    if (allocated(var%darray4)) deallocate(var%darray4)
    if (allocated(var%darray5)) deallocate(var%darray5)
    if (allocated(var%darray6)) deallocate(var%darray6)
!
    if (allocated(var%sarray1)) deallocate(var%sarray1)
    if (allocated(var%sarray2)) deallocate(var%sarray2)
    if (allocated(var%sarray3)) deallocate(var%sarray3)
    if (allocated(var%sarray4)) deallocate(var%sarray4)
    if (allocated(var%sarray5)) deallocate(var%sarray5)
    if (allocated(var%sarray6)) deallocate(var%sarray6)
!
    if (allocated(var%larray1)) deallocate(var%larray1)
    if (allocated(var%larray2)) deallocate(var%larray2)
    if (allocated(var%larray3)) deallocate(var%larray3)
    if (allocated(var%larray4)) deallocate(var%larray4)
    if (allocated(var%larray5)) deallocate(var%larray5)
    if (allocated(var%larray6)) deallocate(var%larray6)
!
    if (allocated(var%iarray1)) deallocate(var%iarray1)
    if (allocated(var%iarray2)) deallocate(var%iarray2)
    if (allocated(var%iarray3)) deallocate(var%iarray3)
    if (allocated(var%iarray4)) deallocate(var%iarray4)
    if (allocated(var%iarray5)) deallocate(var%iarray5)
    if (allocated(var%iarray6)) deallocate(var%iarray6)
!
    if (allocated(var%carray1)) deallocate(var%carray1)
    if (allocated(var%carray2)) deallocate(var%carray2)
!
    if (associated(var%oldvalues2D))    deallocate(var%oldvalues2D)
    if (associated(var%interpIndex))    deallocate(var%interpIndex)
!
    if (associated(var%posvar))     deallocate(var%posvar)
    if (associated(var%interptab))  deallocate(var%interptab)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_deallocate_Arrays
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Free_data_before
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Free_data_before ( Agrif_Gr )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid),pointer   :: Agrif_Gr ! Pointer on the current grid
!
    integer :: i
    type(Agrif_List_Variables), pointer :: parcours
!
    do i = 1,Agrif_NbVariables
!
        if ( .NOT. Agrif_Mygrid % tabvars(i) % var % restaure ) then
            call Agrif_deallocate_Arrays(Agrif_Gr%tabvars(i)%var)
        endif
!
        if (associated(Agrif_Gr%tabvars(i)%var%list_interp)) then
            call Agrif_Free_list_interp(Agrif_Gr%tabvars(i)%var%list_interp)
        endif
!
        if ( .NOT. Agrif_Mygrid % tabvars(i) % var % restaure ) then
            deallocate(Agrif_Gr%tabvars(i)%var)
        endif
!
    enddo

    parcours => Agrif_Gr%variables

    do i = 1,Agrif_Gr%NbVariables
!
        if ( .NOT. parcours%pvar%var%root_var%restaure ) then
            call Agrif_deallocate_Arrays(parcours%pvar%var)
        endif
!
        if (associated(parcours%pvar%var%list_interp)) then
            call Agrif_Free_list_interp(parcours%pvar%var%list_interp)
        endif
!
        if ( .NOT. parcours%pvar%var%root_var % restaure ) then
            deallocate(parcours%pvar%var)
        endif
!
        parcours => parcours%nextvariable
!
    enddo
!
    if ( Agrif_USE_ONLY_FIXED_GRIDS == 0 ) then
        if ( Agrif_Probdim == 1 ) deallocate(Agrif_Gr%tabpoint1D)
        if ( Agrif_Probdim == 2 ) deallocate(Agrif_Gr%tabpoint2D)
        if ( Agrif_Probdim == 3 ) deallocate(Agrif_Gr%tabpoint3D)
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Free_data_before
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Free_list_interp
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_Free_list_interp ( list_interp )
!---------------------------------------------------------------------------------------------------
      type(Agrif_List_Interp_Loc), pointer :: list_interp
!
      if (associated(list_interp%suiv)) call Agrif_Free_list_interp(list_interp%suiv)

#if defined AGRIF_MPI
       deallocate(list_interp%interp_loc%tab4t)
       deallocate(list_interp%interp_loc%memberinall)
       deallocate(list_interp%interp_loc%sendtoproc1)
       deallocate(list_interp%interp_loc%recvfromproc1)
#endif
       deallocate(list_interp%interp_loc)
       deallocate(list_interp)
       nullify(list_interp)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Free_list_interp
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Free_data_after
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Free_data_after ( Agrif_Gr )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid), pointer   :: Agrif_Gr  !< Pointer on the current grid
!
    integer :: i
    type(Agrif_List_Variables), pointer :: parcours, rootparcours
!
    do i = 1,Agrif_NbVariables
        if ( Agrif_Mygrid % tabvars(i) % var % restaure) then
            call Agrif_deallocate_Arrays(Agrif_Gr%tabvars(i)%var)
            deallocate(Agrif_Gr%tabvars(i)%var)
        endif
    enddo
!
    parcours => Agrif_Gr%variables
    rootparcours => Agrif_Mygrid%variables
!
    do i = 1,Agrif_Gr%NbVariables
        if (rootparcours%pvar%var%restaure) then
            call Agrif_deallocate_Arrays(parcours%pvar%var)
            deallocate(parcours%pvar%var)
        endif
        parcours => parcours%nextvariable
        rootparcours => rootparcours%nextvariable
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Free_data_after
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_CopyFromOld_All
!
!> Called in Agrif_Clustering#Agrif_Init_Hierarchy.
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_CopyFromOld_All ( g, oldchildgrids )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid),  pointer  :: g   !< Pointer on the current grid
    type(Agrif_PGrid), pointer  :: oldchildgrids
!
    type(Agrif_PGrid), pointer  :: parcours ! Pointer for the recursive procedure
    real    :: g_eps, eps, oldgrid_eps
    integer :: out
    integer :: iii
!
    out = 0
!
    parcours => oldchildgrids
!
    do while (associated(parcours))
!
        if ((.NOT. g % fixed) .AND. (parcours % gr %oldgrid)) then
!
            g_eps = huge(1.)
            oldgrid_eps = huge(1.)
            do iii = 1,Agrif_Probdim
                g_eps = min(g_eps,g % Agrif_d(iii))
                oldgrid_eps = min(oldgrid_eps, parcours % gr % Agrif_d(iii))
            enddo
!
            eps = min(g_eps,oldgrid_eps)/100.
!
            do iii = 1,Agrif_Probdim
                if (g % Agrif_d(iii) < (parcours % gr % Agrif_d(iii) - eps)) then
                    parcours => parcours % next
                    out = 1
                    exit
                endif
            enddo
!
            if ( out == 1 ) cycle
!
            call Agrif_CopyFromOld(g,parcours%gr)
!
        endif
!
        call Agrif_CopyFromOld_All(g,parcours % gr % child_grids)
!
        parcours => parcours % next
!
      enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_CopyFromOld_All
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_CopyFromOld
!
!> Call to the Agrif_Copy procedure.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_CopyFromOld ( Agrif_New_Gr, Agrif_Old_Gr )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid), pointer   :: Agrif_New_Gr  !< Pointer on the current grid
    type(Agrif_Grid), pointer   :: Agrif_Old_Gr  !< Pointer on an old grid
!
    integer :: i
!
    do i = 1,Agrif_NbVariables
        if ( Agrif_Mygrid % tabvars(i) % var % restaure ) then
            call Agrif_Copy(Agrif_New_Gr,Agrif_Old_Gr,Agrif_New_Gr % tabvars(i), &
                                                      Agrif_Old_Gr % tabvars(i))
        endif
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_CopyFromOld
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_CopyFromOld_AllOneVar
!
!> Called in Agrif_Clustering#Agrif_Init_Hierarchy.
!---------------------------------------------------------------------------------------------------
recursive subroutine Agrif_CopyFromOld_AllOneVar ( g, oldchildgrids, indic )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid),  pointer      :: g                !< Pointer on the current grid
    type(Agrif_PGrid), pointer      :: oldchildgrids
    integer,           intent(in)   :: indic
!
    type(Agrif_PGrid), pointer  :: parcours ! Pointer for the recursive procedure
    real    :: g_eps,eps,oldgrid_eps
    integer :: out
    integer :: iii
!
    out = 0
!
    parcours => oldchildgrids
!
    do while (associated(parcours))
!
        if ((.NOT. g % fixed) .AND. (parcours % gr %oldgrid)) then
!
            g_eps = huge(1.)
            oldgrid_eps = huge(1.)
            do iii = 1,Agrif_Probdim
                g_eps = min(g_eps,g % Agrif_d(iii))
                oldgrid_eps = min(oldgrid_eps,parcours % gr % Agrif_d(iii))
            enddo
!
            eps = min(g_eps,oldgrid_eps)/100.
!
            do iii = 1,Agrif_Probdim
                if (g % Agrif_d(iii) < (parcours % gr % Agrif_d(iii) - eps)) then
                    parcours => parcours % next
                    out = 1
                    exit
                endif
            enddo
        
            if ( out == 1 ) cycle
!
            call Agrif_CopyFromOldOneVar(g,parcours%gr,indic)
!
        endif
!
        call Agrif_CopyFromOld_AllOneVar(g,parcours%gr % child_grids,indic)
!
        parcours => parcours % next
!
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_CopyFromOld_AllOneVar
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_CopyFromOldOneVar
!
!> Call to Agrif_Copy
!---------------------------------------------------------------------------------------------------
subroutine Agrif_CopyFromOldOneVar ( Agrif_New_Gr, Agrif_Old_Gr, indic )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid), pointer       :: Agrif_New_Gr     !< Pointer on the current grid
    type(Agrif_Grid), pointer       :: Agrif_Old_Gr     !< Pointer on an old grid
    integer,          intent(in)    :: indic
!
    type(Agrif_PVariable), pointer  :: tabvars,oldtabvars
!
    tabvars    => Agrif_Search_Variable(Agrif_New_Gr,-indic)
    oldtabvars => Agrif_Search_Variable(Agrif_Old_Gr,-indic)
!
    call Agrif_Nbdim_Allocation(tabvars%var,tabvars%var%lb,tabvars%var%ub,tabvars%var%nbdim)
    call Agrif_Copy(Agrif_New_Gr,Agrif_Old_Gr,tabvars,oldtabvars)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_CopyFromOldOneVar
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Copy
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Copy ( Agrif_New_Gr, Agrif_Old_Gr, New_Var, Old_Var )
!---------------------------------------------------------------------------------------------------
    type(Agrif_Grid), pointer   :: Agrif_New_Gr   ! Pointer on the current grid
    type(Agrif_Grid), pointer :: Agrif_Old_Gr    ! Pointer on an old grid
    type(Agrif_PVariable) :: New_Var
    type(Agrif_PVariable) :: Old_Var
!
    type(Agrif_Variable), pointer :: root ! Pointer on the variable of the root grid
    integer               :: nbdim     ! Number of dimensions of the current grid
    integer, dimension(6) :: pttabnew  ! Indexes of the first point in the domain
    integer, dimension(6) :: petabnew  ! Indexes of the first point in the domain
    integer, dimension(6) :: pttabold  ! Indexes of the first point in the domain
    integer, dimension(6) :: petabold  ! Indexes of the first point in the domain
    integer, dimension(6) :: nbtabold  ! Number of cells in each direction
    integer, dimension(6) :: nbtabnew  ! Number of cells in each direction
    real,    dimension(6) :: snew,sold
    real,    dimension(6) :: dsnew,dsold
    real    :: eps
    integer :: n
!
    root => New_Var % var % root_var
    nbdim = root % nbdim
!
    do n = 1,nbdim
!
        select case(root % interptab(n))
!
        case('x')
!
            pttabnew(n) = root % point(1)
            pttabold(n) = root % point(1)
            snew(n) = Agrif_New_Gr % Agrif_x(1)
            sold(n) = Agrif_Old_Gr % Agrif_x(1)
            dsnew(n) = Agrif_New_Gr % Agrif_d(1)
            dsold(n) = Agrif_Old_Gr % Agrif_d(1)
!
            if (root % posvar(n) == 1) then
                petabnew(n) = pttabnew(n) + Agrif_New_Gr % nb(1)
                petabold(n) = pttabold(n) + Agrif_Old_Gr % nb(1)
            else
                petabnew(n) = pttabnew(n) + Agrif_New_Gr % nb(1) - 1
                petabold(n) = pttabold(n) + Agrif_Old_Gr % nb(1) - 1
                snew(n) = snew(n) + dsnew(n)/2.
                sold(n) = sold(n) + dsold(n)/2.
            endif
!
        case('y')
!
            pttabnew(n) = root % point(2)
            pttabold(n) = root % point(2)
            snew(n) = Agrif_New_Gr % Agrif_x(2)
            sold(n) = Agrif_Old_Gr % Agrif_x(2)
            dsnew(n) = Agrif_New_Gr % Agrif_d(2)
            dsold(n) = Agrif_Old_Gr % Agrif_d(2)
!
            if (root % posvar(n) == 1) then
                petabnew(n) = pttabnew(n) + Agrif_New_Gr % nb(2)
                petabold(n) = pttabold(n) + Agrif_Old_Gr % nb(2)
            else
                petabnew(n) = pttabnew(n) + Agrif_New_Gr % nb(2) - 1
                petabold(n) = pttabold(n) + Agrif_Old_Gr % nb(2) - 1
                snew(n) = snew(n) + dsnew(n)/2.
                sold(n) = sold(n) + dsold(n)/2.
            endif
!
        case('z')
!
            pttabnew(n) = root % point(3)
            pttabold(n) = root % point(3)
            snew(n) = Agrif_New_Gr % Agrif_x(3)
            sold(n) = Agrif_Old_Gr % Agrif_x(3)
            dsnew(n) = Agrif_New_Gr % Agrif_d(3)
            dsold(n) = Agrif_Old_Gr % Agrif_d(3)
!
            if (root % posvar(n) == 1) then
                petabnew(n) = pttabnew(n) + Agrif_New_Gr % nb(3)
                petabold(n) = pttabold(n) + Agrif_Old_Gr % nb(3)
            else
                petabnew(n) = pttabnew(n) + Agrif_New_Gr % nb(3) - 1
                petabold(n) = pttabold(n) + Agrif_Old_Gr % nb(3) - 1
                snew(n) = snew(n) + dsnew(n)/2.
                sold(n) = sold(n) + dsold(n)/2.
            endif
!
        case('N')
!
            call Agrif_nbdim_Get_bound(New_Var%var,pttabnew(n),petabnew(n),n)
!
            pttabold(n) = pttabnew(n)
            petabold(n) = petabnew(n)
            snew(n) = 0.
            sold(n) = 0.
            dsnew(n) = 1.
            dsold(n) = 1.
!
        end select
!
    enddo
!
    do n = 1,nbdim
        nbtabnew(n) = petabnew(n) - pttabnew(n)
        nbtabold(n) = petabold(n) - pttabold(n)
    enddo
!
    eps = min(minval(dsnew(1:nbdim)),minval(dsold(1:nbdim))) / 100.
!
    do n = 1,nbdim
        if (snew(n) > (sold(n)+dsold(n)*nbtabold(n)+eps)) return
        if ((snew(n)+dsnew(n)*nbtabnew(n)-eps) < sold(n)) return
    enddo
!
    call Agrif_CopynD(New_Var,Old_Var,pttabold,petabold,pttabnew,petabnew,  &
                      sold,snew,dsold,dsnew,nbdim)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Copy
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_CopynD
!
!> Copy from the nD variable Old_Var the nD variable New_Var
!---------------------------------------------------------------------------------------------------
subroutine Agrif_CopynD ( New_Var, Old_Var, pttabold, petabold, pttabnew, petabnew, &
                          sold, snew, dsold, dsnew, nbdim )
!---------------------------------------------------------------------------------------------------
    integer :: nbdim
    type(Agrif_PVariable)       :: New_Var, Old_Var
    integer,dimension(nbdim)    :: pttabnew
    integer,dimension(nbdim)    :: petabnew
    integer,dimension(nbdim)    :: pttabold
    integer,dimension(nbdim)    :: petabold
    real, dimension(nbdim)      :: snew, sold
    real, dimension(nbdim)      :: dsnew,dsold
!
    integer :: i,j,k,l,m,n,i0,j0,k0,l0,m0,n0
!
    real,    dimension(nbdim) :: dim_gmin,   dim_gmax
    real,    dimension(nbdim) :: dim_newmin, dim_newmax
    real,    dimension(nbdim) :: dim_min
    integer, dimension(nbdim) :: ind_gmin,ind_newmin, ind_newmax
!
    do i = 1,nbdim
!
        dim_gmin(i) = sold(i)
        dim_gmax(i) = sold(i) + (petabold(i)-pttabold(i)) * dsold(i)
!
        dim_newmin(i) = snew(i)
        dim_newmax(i) = snew(i) + (petabnew(i)-pttabnew(i)) * dsnew(i)
!
    enddo
!
    do i = 1,nbdim
        if (dim_gmax(i) < dim_newmin(i)) return
        if (dim_gmin(i) > dim_newmax(i)) return
    enddo
!
    do i = 1,nbdim
!
        ind_newmin(i) = pttabnew(i) - floor(-(max(dim_gmin(i),dim_newmin(i))-dim_newmin(i))/dsnew(i))
        dim_min(i) = snew(i) + (ind_newmin(i)-pttabnew(i))*dsnew(i)
        ind_gmin(i)   = pttabold(i) + nint((dim_min(i)-dim_gmin(i))/dsold(i))
        ind_newmax(i) = pttabnew(i) + int((min(dim_gmax(i),dim_newmax(i))-dim_newmin(i))/dsnew(i))
!
    enddo
!
    select case (nbdim)
!
    case (1)
        i0 = ind_gmin(1)
        do i = ind_newmin(1),ind_newmax(1)
            New_Var % var % array1(i)    = Old_Var % var % array1(i0)
            New_Var % var % restore1D(i) = 1
            i0 = i0 + int(dsnew(1)/dsold(1))
        enddo
!
    case (2)
        i0 = ind_gmin(1) ; do i = ind_newmin(1),ind_newmax(1)
        j0 = ind_gmin(2) ; do j = ind_newmin(2),ind_newmax(2)
            New_Var % var % array2(i,j)    = Old_Var % var % array2(i0,j0)
            New_Var % var % restore2D(i,j) = 1
            j0 = j0 + int(dsnew(2)/dsold(2))
        enddo
        i0 = i0 + int(dsnew(1)/dsold(1))
        enddo
!
    case (3)
        i0 = ind_gmin(1) ; do i = ind_newmin(1),ind_newmax(1)
        j0 = ind_gmin(2) ; do j = ind_newmin(2),ind_newmax(2)
        k0 = ind_gmin(3) ; do k = ind_newmin(3),ind_newmax(3)
            New_Var % var % array3(i,j,k)    =  Old_Var % var % array3(i0,j0,k0)
            New_Var % var % restore3D(i,j,k) = 1
            k0 = k0 + int(dsnew(3)/dsold(3))
        enddo
        j0 = j0 + int(dsnew(2)/dsold(2))
        enddo
        i0 = i0 + int(dsnew(1)/dsold(1))
        enddo
!
    case (4)
        i0 = ind_gmin(1) ; do i = ind_newmin(1),ind_newmax(1)
        j0 = ind_gmin(2) ; do j = ind_newmin(2),ind_newmax(2)
        k0 = ind_gmin(3) ; do k = ind_newmin(3),ind_newmax(3)
        l0 = ind_gmin(4) ; do l = ind_newmin(4),ind_newmax(4)
            New_Var % var % array4(i,j,k,l)    = Old_Var % var % array4(i0,j0,k0,l0)
            New_Var % var % restore4D(i,j,k,l) = 1
            l0 = l0 + int(dsnew(4)/dsold(4))
        enddo
        k0 = k0 + int(dsnew(3)/dsold(3))
        enddo
        j0 = j0 + int(dsnew(2)/dsold(2))
        enddo
        i0 = i0 + int(dsnew(1)/dsold(1))
        enddo
!
    case (5)
        i0 = ind_gmin(1) ; do i = ind_newmin(1),ind_newmax(1)
        j0 = ind_gmin(2) ; do j = ind_newmin(2),ind_newmax(2)
        k0 = ind_gmin(3) ; do k = ind_newmin(3),ind_newmax(3)
        l0 = ind_gmin(4) ; do l = ind_newmin(4),ind_newmax(4)
        m0 = ind_gmin(5) ; do m = ind_newmin(5),ind_newmax(5)
            New_Var % var % array5(i,j,k,l,m)    = Old_Var % var % array5(i0,j0,k0,l0,m0)
            New_Var % var % restore5D(i,j,k,l,m) = 1
            m0 = m0 + int(dsnew(5)/dsold(5))
        enddo
        l0 = l0 + int(dsnew(4)/dsold(4))
        enddo
        k0 = k0 + int(dsnew(3)/dsold(3))
        enddo
        j0 = j0 + int(dsnew(2)/dsold(2))
        enddo
        i0 = i0 + int(dsnew(1)/dsold(1))
        enddo
!
    case (6)
        i0 = ind_gmin(1) ; do i = ind_newmin(1),ind_newmax(1)
        j0 = ind_gmin(2) ; do j = ind_newmin(2),ind_newmax(2)
        k0 = ind_gmin(3) ; do k = ind_newmin(3),ind_newmax(3)
        l0 = ind_gmin(4) ; do l = ind_newmin(4),ind_newmax(4)
        m0 = ind_gmin(5) ; do m = ind_newmin(5),ind_newmax(5)
        n0 = ind_gmin(6) ; do n = ind_newmin(6),ind_newmax(6)
            New_Var % var % array6(i,j,k,l,m,n)    = Old_Var % var % array6(i0,j0,k0,l0,m0,n0)
            New_Var % var % restore6D(i,j,k,l,m,n) = 1
            n0 = n0 + int(dsnew(6)/dsold(6))
        enddo
        m0 = m0 + int(dsnew(5)/dsold(5))
        enddo
        l0 = l0 + int(dsnew(4)/dsold(4))
        enddo
        k0 = k0 + int(dsnew(3)/dsold(3))
        enddo
        j0 = j0 + int(dsnew(2)/dsold(2))
        enddo
        i0 = i0 + int(dsnew(1)/dsold(1))
        enddo
!
      end select
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_CopynD
!===================================================================================================
!
end module Agrif_Save
