!
! $Id: modupdate.F 779 2007-12-22 17:04:17Z rblod $
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
!> Module Agrif_Update
!>
!> This module  contains procedures to update a parent grid from its child grids.
!
module Agrif_Update
!
    use Agrif_UpdateBasic
    use Agrif_Arrays
    use Agrif_CurgridFunctions
    use Agrif_Mask
#if defined AGRIF_MPI
    use Agrif_Mpp
#endif
!
    implicit none
!
    logical, private :: precomputedone(7) = .FALSE.
!
contains
!
!===================================================================================================
!  subroutine Agrif_UpdateVariable
!
!> subroutine to set arguments of Agrif_UpdatenD, n being the number of dimensions of the
!! grid variable
!---------------------------------------------------------------------------------------------------
subroutine Agrif_UpdateVariable ( TypeUpdate, parent, child, deb, fin, procname )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(6), intent(in)   :: TypeUpdate  !< Type of update (copy or average)
    TYPE(Agrif_PVariable)               :: parent      !< Variable on the parent grid
    TYPE(Agrif_PVariable)               :: child       !< Variable on the child grid
    INTEGER, DIMENSION(6), intent(in)   :: deb         !< Positions where interpolations are calculated
    INTEGER, DIMENSION(6), intent(in)   :: fin         !< Positions where interpolations are calculated
    External :: procname
    Optional ::  procname
!
    INTEGER               :: nbdim    ! Number of dimensions of the current grid
    INTEGER, DIMENSION(6) :: pttab_child
    INTEGER, DIMENSION(6) :: petab_child
    INTEGER, DIMENSION(6) :: pttab_parent
    REAL   , DIMENSION(6) :: s_child, s_parent
    REAL   , DIMENSION(6) :: ds_child,ds_parent
    INTEGER, DIMENSION(6) :: loctab_Child ! Indicates if the child grid has a common border with the root grid
    TYPE(Agrif_Variable), pointer :: root               ! Variable on the root grid
    INTEGER,DIMENSION(6)          :: posvartab_Child    ! Position of the variable on the cell
    INTEGER,DIMENSION(6)          :: nbtab_Child        ! Number of the cells
    INTEGER :: n
    LOGICAL :: wholeupdate
    INTEGER, DIMENSION(6) :: oldparentlbound, oldparentubound
!
    loctab_child(:) = 0
!
    root => child % var % root_var
    nbdim = root % nbdim
!
    do n = 1,nbdim
        posvartab_child(n) = root % posvar(n)
    enddo
!
    call PreProcessToInterpOrUpdate(parent,child,petab_Child(1:nbdim),              &
                                    pttab_Child(1:nbdim), pttab_Parent(1:nbdim),    &
                                    s_Child(1:nbdim), s_Parent(1:nbdim),            &
                                    ds_Child(1:nbdim),ds_Parent(1:nbdim),           &
                                    nbdim, interp=.FALSE.)
!
    do n = 1,nbdim
!
        select case(root % interptab(n))
!
        case('x') ! x DIMENSION
            nbtab_Child(n) = Agrif_Curgrid % nb(1)
!
        case('y') ! y DIMENSION
            nbtab_Child(n) = Agrif_Curgrid % nb(2)
!
        case('z') ! z DIMENSION
            nbtab_Child(n) = Agrif_Curgrid % nb(3)
!
        case('N') ! No space DIMENSION
            nbtab_Child(n) = child % var % ub(n) - child % var % lb(n)
!
!           No interpolation but only a copy of the values of the grid variable
            posvartab_child(n) = 1
            loctab_child(n) = -3
            oldparentlbound(n) = parent%var%lb(n)
            oldparentubound(n) = parent%var%ub(n)
            parent%var%lb(n) = child%var%lb(n)
            parent%var%ub(n) = child%var%ub(n)
        end select
!
    enddo

!   Call to a procedure of update according to the number of dimensions of the grid variable
!
    wholeupdate = .FALSE.
!
    do n=1,nbdim
        if (loctab_child(n) /= -3) then
            if ( deb(n) > fin(n) ) wholeupdate = .TRUE.
            if ((deb(n) == -99).AND.(deb(n)==fin(n))) wholeupdate=.TRUE.
        endif
    enddo
!
    IF (present(procname)) THEN
        IF (wholeupdate) THEN
            call Agrif_UpdateWhole(TypeUpdate,parent,child,deb,fin,     &
                    pttab_Child(1:nbdim),pttab_Parent(1:nbdim),         &
                    nbtab_Child(1:nbdim),posvartab_Child(1:nbdim),      &
                    loctab_Child(1:nbdim),                              &
                    s_Child(1:nbdim),s_Parent(1:nbdim),                 &
                    ds_Child(1:nbdim),ds_Parent(1:nbdim),nbdim,procname)
        ELSE
            call Agrif_UpdateBcnD(TypeUpdate,parent,child,deb,fin,      &
                    pttab_Child(1:nbdim),pttab_Parent(1:nbdim),         &
                    nbtab_Child(1:nbdim),posvartab_Child(1:nbdim),      &
                    loctab_Child(1:nbdim),                              &
                    s_Child(1:nbdim),s_Parent(1:nbdim),                 &
                    ds_Child(1:nbdim),ds_Parent(1:nbdim),nbdim,procname)
        ENDIF
    ELSE
        IF (wholeupdate) THEN
            call Agrif_UpdateWhole(TypeUpdate,parent,child,deb,fin,     &
                    pttab_Child(1:nbdim),pttab_Parent(1:nbdim),         &
                    nbtab_Child(1:nbdim),posvartab_Child(1:nbdim),      &
                    loctab_Child(1:nbdim),                              &
                    s_Child(1:nbdim),s_Parent(1:nbdim),                 &
                    ds_Child(1:nbdim),ds_Parent(1:nbdim),nbdim)
        ELSE
            call Agrif_UpdateBcnD(TypeUpdate,parent,child,deb,fin,      &
                    pttab_Child(1:nbdim),pttab_Parent(1:nbdim),         &
                    nbtab_Child(1:nbdim),posvartab_Child(1:nbdim),      &
                    loctab_Child(1:nbdim),                              &
                    s_Child(1:nbdim),s_Parent(1:nbdim),                 &
                    ds_Child(1:nbdim),ds_Parent(1:nbdim),nbdim)
        ENDIF
    ENDIF
!
    do n = 1,nbdim
!
        select case(root % interptab(n))
        case('N') ! No space DIMENSION
            parent%var%lb(n) = oldparentlbound(n)
            parent%var%ub(n) = oldparentubound(n)
        end select
!
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_UpdateVariable
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_UpdateWhole
!
!> calculates the boundary conditions for a nD grid variable on a fine grid by using a space and
!! time interpolations; it is called by Agrif_Boundary#Agrif_CorrectVariable.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_UpdateWhole ( TypeUpdate, parent, child, deb, fin, &
                               pttab_child, pttab_Parent,           &
                               nbtab_Child, posvartab_Child,        &
                               loctab_Child,                        &
                               s_Child,  s_Parent,                  &
                               ds_Child, ds_Parent, nbdim, procname )
!---------------------------------------------------------------------------------------------------
#if defined AGRIF_MPI
      include 'mpif.h'
#endif
!
    INTEGER, DIMENSION(6), intent(in)   :: TypeUpdate   !< Type of update (copy or average)
    TYPE(Agrif_PVariable)               :: parent       !< Variable on the parent grid
    TYPE(Agrif_PVariable)               :: child        !< Variable on the child grid
    INTEGER, DIMENSION(6), intent(in)   :: deb, fin
    INTEGER,               intent(in)   :: nbdim        !< Number of dimensions of the grid variable
    INTEGER, DIMENSION(nbdim) :: pttab_child        !< Index of the first point inside the domain for
                                                     !    the parent grid variable
    INTEGER, DIMENSION(nbdim) :: pttab_Parent       !< Index of the first point
                                                     !    inside the domain for
                                                     !    the child grid variable
    INTEGER, DIMENSION(nbdim) :: nbtab_Child        !< Number of cells of the child grid
    INTEGER, DIMENSION(nbdim) :: posvartab_Child    !< Position of the grid variable (1 or 2)
    INTEGER, DIMENSION(nbdim) :: loctab_Child       !< Indicates if the child
                                                     !    grid has a common
                                                     !    border with the root grid
    REAL,    DIMENSION(nbdim) :: s_Child   !< Positions of the child grid
    REAL,    DIMENSION(nbdim) :: s_Parent   !< Positions of the parent grid
    REAL,    DIMENSION(nbdim) :: ds_Child !< Space steps of the child grid
    REAL,    DIMENSION(nbdim) :: ds_Parent !< Space steps of the parent grid
    External :: procname
    Optional :: procname
!
    INTEGER,DIMENSION(nbdim,2)   :: lubglob
    INTEGER                      :: i
    INTEGER,DIMENSION(nbdim,2,2) :: indtab      ! limits of the child grid that will be used in the update scheme
    INTEGER,DIMENSION(nbdim,2,2) :: indtruetab  ! grid variable where boundary conditions are
    integer :: coeffraf
    INTEGER :: debloc, finloc
!
#if defined AGRIF_MPI
    INTEGER,DIMENSION(nbdim)   :: lb,ub
    INTEGER,DIMENSION(nbdim,2) :: iminmaxg
    INTEGER                    :: code
#endif
!
    DO i = 1, nbdim
!
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
            IF (TypeUpdate(i) == Agrif_Update_Full_Weighting) THEN
                indtab(i,1,1) = indtab(i,1,1) - (coeffraf - 1)
                indtab(i,1,2) = indtab(i,1,2) + (coeffraf - 1)
            ELSE IF (TypeUpdate(i) /= Agrif_Update_Copy) THEN
                indtab(i,1,1) = indtab(i,1,1) - coeffraf / 2
                indtab(i,1,2) = indtab(i,1,2) + coeffraf / 2
            ENDIF
        ELSE
            indtab(i,1,1) = indtab(i,1,1) - coeffraf
            indtab(i,1,2) = indtab(i,1,2) - 1
! at this point, indices are OK for an average
            IF ((TypeUpdate(i) == Agrif_Update_Full_Weighting)) THEN
                indtab(i,1,1) = indtab(i,1,1) - coeffraf/2
                indtab(i,1,2) = indtab(i,1,2) + coeffraf/2
            ENDIF
        ENDIF
        IF (loctab_child(i) == -3) THEN
!
            indtab(i,1,1) = pttab_child(i)
!
            if (posvartab_child(i) == 1) then
                indtab(i,1,2) = pttab_child(i) + nbtab_child(i)
            else
                indtab(i,1,2) = pttab_child(i) + nbtab_child(i) - 1
            endif
        ENDIF
    ENDDO

! lubglob contains the global lbound and ubound of the child array
! lubglob(:,1) : global lbound for each dimension
! lubglob(:,2) : global lbound for each dimension

#if !defined AGRIF_MPI
!
    call Agrif_nbdim_Get_bound_dimension(child % var,lubglob(:,1),lubglob(:,2),nbdim)
!
#else
!
    call Agrif_nbdim_Get_bound_dimension(child % var,lb,ub,nbdim)
!
    DO i = 1,nbdim
        call Agrif_Invloc(lb(i),Agrif_Procrank,i,iminmaxg(i,1))
        call Agrif_Invloc(ub(i),Agrif_Procrank,i,iminmaxg(i,2))
    ENDDO
!
    iminmaxg(1:nbdim,2) = - iminmaxg(1:nbdim,2)
    CALL MPI_ALLREDUCE(iminmaxg,lubglob,2*nbdim,MPI_INTEGER,MPI_MIN,MPI_COMM_WORLD,code)
    lubglob(1:nbdim,2)  = - lubglob(1:nbdim,2)
!
#endif
!
    indtruetab(1:nbdim,1,1) = max(indtab(1:nbdim,1,1),lubglob(1:nbdim,1))
    indtruetab(1:nbdim,1,2) = min(indtab(1:nbdim,1,2),lubglob(1:nbdim,2))
!
    IF (present(procname)) THEN
        call Agrif_UpdatenD(TypeUpdate,parent,child,                        &
                            indtruetab(1:nbdim,1,1),indtruetab(1:nbdim,1,2),&
                            pttab_child(1:nbdim),pttab_Parent(1:nbdim),     &
                            s_Child(1:nbdim),s_Parent(1:nbdim),             &
                            ds_Child(1:nbdim),ds_Parent(1:nbdim),           &
                            posvartab_child,loctab_Child,nbdim,procname)
    ELSE
        call Agrif_UpdatenD(TypeUpdate,parent,child,                        &
                            indtruetab(1:nbdim,1,1),indtruetab(1:nbdim,1,2),&
                            pttab_child(1:nbdim),pttab_Parent(1:nbdim),     &
                            s_Child(1:nbdim),s_Parent(1:nbdim),             &
                            ds_Child(1:nbdim),ds_Parent(1:nbdim),           &
                            posvartab_child,loctab_Child,nbdim)
    ENDIF
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_UpdateWhole
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_UpdateBcnd
!
!> Calculates the boundary conditions for a nD grid variable on a fine grid by using a space
!! and time interpolations; it is called by Agrif_Boundary#Agrif_CorrectVariable.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_UpdateBcnd ( TypeUpdate, parent, child, deb, fin,  &
                              pttab_child, pttab_Parent,            &
                              nbtab_Child, posvartab_Child,         &
                              loctab_Child,                         &
                              s_Child,  s_Parent,                   &
                              ds_Child, ds_Parent, nbdim, procname )
!---------------------------------------------------------------------------------------------------
#if defined AGRIF_MPI
    include 'mpif.h'
#endif
!
    INTEGER, DIMENSION(6),     intent(in)   :: TypeUpdate   !< Type of update (copy or average)
    TYPE(Agrif_PVariable)                   :: parent       !< Variable on the parent grid
    TYPE(Agrif_PVariable)                   :: child        !< Variable on the child grid
    INTEGER, DIMENSION(6),     intent(in)   :: deb, fin
    INTEGER                                 :: nbdim        !< Number of dimensions of the grid variable
    INTEGER, DIMENSION(nbdim), intent(in)   :: pttab_child  !< Index of the first point inside the domain for
                                                            !!   the parent grid variable
    INTEGER, DIMENSION(nbdim), intent(in)   :: pttab_Parent !< Index of the first point inside the domain for
                                                            !!   the child grid variable
    INTEGER, DIMENSION(nbdim), intent(in)   :: nbtab_Child  !< Number of cells of the child grid
    INTEGER, DIMENSION(nbdim), intent(in)   :: posvartab_Child   !< Position of the grid variable (1 or 2)
    INTEGER, DIMENSION(nbdim), intent(in)   :: loctab_Child      !< Indicates if the child grid has a common
                                                                 !!    border with the root grid
    REAL,    DIMENSION(nbdim), intent(in)   :: s_Child      !< Positions of the child grid
    REAL,    DIMENSION(nbdim), intent(in)   :: s_Parent     !< Positions of the parent grid
    REAL,    DIMENSION(nbdim), intent(in)   :: ds_Child     !< Space steps of the child grid
    REAL,    DIMENSION(nbdim), intent(in)   :: ds_Parent    !< Space steps of the parent grid
    External :: procname
    Optional :: procname
!
    INTEGER,DIMENSION(nbdim,2)   :: lubglob
    INTEGER                      :: i
    INTEGER,DIMENSION(nbdim,2,2) :: indtab         ! Arrays indicating the limits of the child
    INTEGER,DIMENSION(nbdim,2,2) :: indtruetab     ! grid variable where boundary conditions are
    INTEGER,DIMENSION(nbdim,2,2,nbdim)   :: ptres  ! calculated
    INTEGER                      :: nb,ndir,n
    integer :: coeffraf
!
#if defined AGRIF_MPI
    INTEGER,DIMENSION(nbdim)   :: lb,ub
    INTEGER,DIMENSION(nbdim,2) :: iminmaxg
    INTEGER                    :: code
#endif
!
    DO i = 1, nbdim
        coeffraf = nint(ds_Parent(i)/ds_Child(i))
        indtab(i,1,1) = pttab_child(i) + (deb(i) + 1) * coeffraf
        indtab(i,1,2) = pttab_child(i) + (fin(i) + 1) * coeffraf

        indtab(i,2,1) = pttab_child(i) + nbtab_child(i) - (fin(i)+1) *  coeffraf
        indtab(i,2,2) = pttab_child(i) + nbtab_child(i) - (deb(i)+1) *  coeffraf

        IF (posvartab_child(i) == 1) THEN
            IF (TypeUpdate(i) == Agrif_Update_Full_Weighting) THEN
                indtab(i,:,1) = indtab(i,:,1) - (coeffraf - 1)
                indtab(i,:,2) = indtab(i,:,2) + (coeffraf - 1)
            ELSE IF (TypeUpdate(i) /= Agrif_Update_Copy) THEN
                indtab(i,:,1) = indtab(i,:,1) - coeffraf / 2
                indtab(i,:,2) = indtab(i,:,2) + coeffraf / 2
            ENDIF
        ELSE
            indtab(i,1,1) = indtab(i,1,1) - coeffraf
            indtab(i,1,2) = indtab(i,1,2) - 1
            indtab(i,2,2) = indtab(i,2,2) + coeffraf - 1
            IF (TypeUpdate(i) == Agrif_Update_Full_Weighting) THEN
                indtab(i,1,1) = indtab(i,1,1) - coeffraf/2
                indtab(i,1,2) = indtab(i,1,2) + coeffraf/2
                indtab(i,2,1) = indtab(i,2,1) - coeffraf/2
                indtab(i,2,2) = indtab(i,2,2) + coeffraf/2
            ENDIF
        ENDIF
    ENDDO

#if !defined AGRIF_MPI
!
    Call Agrif_nbdim_Get_bound_dimension(child % var,lubglob(:,1),lubglob(:,2),nbdim)
!
#else
!
    Call Agrif_nbdim_Get_bound_dimension(child % var,lb,ub,nbdim)
!
    DO i = 1,nbdim
        call Agrif_Invloc(lb(i),Agrif_Procrank,i,iminmaxg(i,1))
        call Agrif_Invloc(ub(i),Agrif_Procrank,i,iminmaxg(i,2))
    ENDDO
!
    iminmaxg(1:nbdim,2) = - iminmaxg(1:nbdim,2)
    CALL MPI_ALLREDUCE(iminmaxg,lubglob,2*nbdim,MPI_INTEGER,MPI_MIN,MPI_COMM_WORLD,code)
    lubglob(1:nbdim,2) = - lubglob(1:nbdim,2)
!
#endif
!
    indtruetab(1:nbdim,1,1) = max(indtab(1:nbdim,1,1),lubglob(1:nbdim,1))
    indtruetab(1:nbdim,1,2) = max(indtab(1:nbdim,1,2),lubglob(1:nbdim,1))
    indtruetab(1:nbdim,2,1) = min(indtab(1:nbdim,2,1),lubglob(1:nbdim,2))
    indtruetab(1:nbdim,2,2) = min(indtab(1:nbdim,2,2),lubglob(1:nbdim,2))
!
    do nb = 1,nbdim
    do ndir = 1,2
        if (loctab_child(nb) /= -3) then
            do n = 1,2
                ptres(nb,n,ndir,nb) = indtruetab(nb,ndir,n)
            enddo
            do i = 1,nbdim
                if (i /= nb) then
                    if (loctab_child(i) == -3) then
                        ptres(i,1,ndir,nb) = pttab_child(i)
                    else
                        ptres(i,1,ndir,nb) = indtruetab(i,1,1)
                    endif
                    if (loctab_child(i) == -3) then
                        if (posvartab_child(i) == 1) then
                            ptres(i,2,ndir,nb) = pttab_child(i) + nbtab_child(i)
                        else
                            ptres(i,2,ndir,nb) = pttab_child(i) + nbtab_child(i) - 1
                        endif
                    else
                        ptres(i,2,ndir,nb) = indtruetab(i,2,2)
                    endif
                endif
            enddo
        endif
    enddo
    enddo
!
    do nb = 1,nbdim
    do ndir = 1,2
!
        if (loctab_child(nb) /= -3) then
!
            IF (present(procname)) THEN
                call Agrif_UpdatenD(TypeUpdate,parent,child,                &
                        ptres(1:nbdim,1,ndir,nb),ptres(1:nbdim,2,ndir,nb),  &
                        pttab_child(1:nbdim),pttab_Parent(1:nbdim),         &
                        s_Child(1:nbdim),s_Parent(1:nbdim),                 &
                        ds_Child(1:nbdim),ds_Parent(1:nbdim),               &
                        posvartab_Child,loctab_Child,nbdim,procname,nb,ndir)
            ELSE
                call Agrif_UpdatenD(TypeUpdate,parent,child,                &
                        ptres(1:nbdim,1,ndir,nb),ptres(1:nbdim,2,ndir,nb),  &
                        pttab_child(1:nbdim),pttab_Parent(1:nbdim),         &
                        s_Child(1:nbdim),s_Parent(1:nbdim),                 &
                        ds_Child(1:nbdim),ds_Parent(1:nbdim),               &
                        posvartab_Child,loctab_Child,nbdim)
            ENDIF
!
        endif
!
    enddo
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_UpdateBcnd
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_UpdatenD
!
!> updates a 2D grid variable on the parent grid of the current grid
!---------------------------------------------------------------------------------------------------
subroutine Agrif_UpdatenD ( TypeUpdate, parent, child, pttab, petab,    &
                            pttab_Child, pttab_Parent,                  &
                            s_Child,  s_Parent,                         &
                            ds_Child, ds_Parent,                        &
                            posvartab_Child, loctab_Child, nbdim, procname, nb, ndir )
!---------------------------------------------------------------------------------------------------
#if defined AGRIF_MPI
    include 'mpif.h'
#endif
!
    INTEGER, DIMENSION(6),     intent(in)   :: TypeUpdate   !< Type of update (copy or average)
    TYPE(Agrif_PVariable)                   :: parent       !< Variable of the parent grid
    TYPE(Agrif_PVariable)                   :: child        !< Variable of the child grid
    INTEGER,                   intent(in)   :: nbdim
    INTEGER, DIMENSION(nbdim), intent(in)   :: pttab        !< Index of the first point inside the domain
    INTEGER, DIMENSION(nbdim), intent(in)   :: petab        !< Index of the first point inside the domain
    INTEGER, DIMENSION(nbdim), intent(in)   :: pttab_Child  !< Index of the first point inside the domain for the child
                                                            !!    grid variable
    INTEGER, DIMENSION(nbdim), intent(in)   :: pttab_Parent !< Index of the first point inside the domain for the parent
                                                            !!    grid variable
    REAL,    DIMENSION(nbdim), intent(in)   :: s_Child      !< Positions of the child grid
    REAL,    DIMENSION(nbdim), intent(in)   :: s_Parent     !< Positions of the parent grid
    REAL,    DIMENSION(nbdim), intent(in)   :: ds_Child     !< Space steps of the child grid
    REAL,    DIMENSION(nbdim), intent(in)   :: ds_Parent    !< Space steps of the parent grid
    External :: procname
    Optional ::  procname
    Integer, optional,         intent(in)   :: nb, ndir
!
    INTEGER, DIMENSION(nbdim)    :: pttruetab,cetruetab
    INTEGER, DIMENSION(nbdim)    :: posvartab_Child,loctab_Child
    INTEGER, DIMENSION(nbdim)    :: indmin,indmax
    INTEGER, DIMENSION(nbdim)    :: indminglob,indmaxglob
    REAL   , DIMENSION(nbdim)    :: s_Child_temp,s_Parent_temp
!ccc LOGICAL, DIMENSION(nbdim)    :: noraftab
    INTEGER, DIMENSION(nbdim)    :: lowerbound,upperbound
    LOGICAL :: memberin, member
    INTEGER, DIMENSION(nbdim)    :: pttruetabwhole,cetruetabwhole
    INTEGER, DIMENSION(nbdim,2,2) :: childarray
    INTEGER, DIMENSION(nbdim,2,2) :: parentarray
    INTEGER :: nbin, ndirin
!
#if defined AGRIF_MPI
!
    INTEGER,DIMENSION(nbdim)    :: indminglob2,indmaxglob2
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1) :: sendtoproc1,recvfromproc1
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1) :: sendtoproc2,recvfromproc2
    INTEGER                               :: code
    INTEGER                               :: i,j,k
    INTEGER, DIMENSION(nbdim,4)           :: tab3
    INTEGER, DIMENSION(nbdim,4,0:Agrif_Nbprocs-1) :: tab4
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1,8) :: tab4t
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1,8) :: tab5t
    LOGICAL :: find_list_update
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1) :: memberinall, memberinall2
    LOGICAL, DIMENSION(1) :: memberin1
!
#endif
!
    TYPE(Agrif_PVariable), SAVE :: tempP      ! Temporary parent grid variable
    TYPE(Agrif_PVariable), SAVE :: tempC      ! Temporary child grid variable
    TYPE(Agrif_PVariable), SAVE :: tempCextend, tempPextend ! Temporary child
!
!   local lbound and ubound of the child array
    call Agrif_nbdim_Get_bound_dimension(child%var,lowerbound,upperbound,nbdim)

! here pttab and petab corresponds to the (global) indices of the points needed in the update
! pttruetab and cetruetab contains only indices that are present on the local processor

    call Agrif_Childbounds(nbdim,lowerbound,upperbound,pttab,petab,pttruetab,cetruetab,memberin)
    call Agrif_Prtbounds(nbdim,indminglob,indmaxglob,s_Parent_temp,s_Child_temp,    &
                         s_Child,ds_Child,s_Parent,ds_Parent,                       &
                         pttab,petab,pttab_Child, pttab_Parent,                     &
                         posvartab_Child,TypeUpdate,loctab_Child                    &
#if defined AGRIF_MPI
                        ,pttruetabwhole,cetruetabwhole                              &
#endif
            )

#if defined AGRIF_MPI
!
    IF (memberin) THEN
        call Agrif_GlobtoLocInd2(childarray,lowerbound,upperbound,  &
                                 pttruetab,cetruetab,nbdim,Agrif_Procrank,member)
    ENDIF

    call Agrif_Prtbounds(nbdim,indmin,indmax,s_Parent_temp,                 &
                         s_Child_temp,s_Child,ds_Child,s_Parent,ds_Parent,  &
                         pttruetab,cetruetab,pttab_Child,pttab_Parent,      &
                         posvartab_Child,TypeUpdate,loctab_Child,           &
                         pttruetabwhole,cetruetabwhole)
!
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
!
        IF (.not.associated(tempC%var))  allocate(tempC%var)
!
        call Agrif_nbdim_allocation(tempC%var,pttruetab,cetruetab,nbdim)
        call Agrif_nbdim_Full_VarEQreal(tempC%var,0.,nbdim)

        IF (present(procname)) THEN
            SELECT CASE (nbdim)
            CASE(1)
                CALL procname(tempC%var%array1,                 &
                          childarray(1,1,2),childarray(1,2,2),.TRUE.,nbin,ndirin)
            CASE(2)
                CALL procname(tempC%var%array2,                 &
                          childarray(1,1,2),childarray(1,2,2),  &
                          childarray(2,1,2),childarray(2,2,2),.TRUE.,nbin,ndirin)
            CASE(3)
                CALL procname(tempC%var%array3,                 &
                          childarray(1,1,2),childarray(1,2,2),  &
                          childarray(2,1,2),childarray(2,2,2),  &
                          childarray(3,1,2),childarray(3,2,2),.TRUE.,nbin,ndirin)
            CASE(4)
                CALL procname(tempC%var%array4,                 &
                          childarray(1,1,2),childarray(1,2,2),  &
                          childarray(2,1,2),childarray(2,2,2),  &
                          childarray(3,1,2),childarray(3,2,2),  &
                          childarray(4,1,2),childarray(4,2,2),.TRUE.,nbin,ndirin)
            CASE(5)
                CALL procname(tempC%var%array5,                 &
                          childarray(1,1,2),childarray(1,2,2),  &
                          childarray(2,1,2),childarray(2,2,2),  &
                          childarray(3,1,2),childarray(3,2,2),  &
                          childarray(4,1,2),childarray(4,2,2),  &
                          childarray(5,1,2),childarray(5,2,2),.TRUE.,nbin,ndirin)
            CASE(6)
                CALL procname(tempC%var%array6,                 &
                          childarray(1,1,2),childarray(1,2,2),  &
                          childarray(2,1,2),childarray(2,2,2),  &
                          childarray(3,1,2),childarray(3,2,2),  &
                          childarray(4,1,2),childarray(4,2,2),  &
                          childarray(5,1,2),childarray(5,2,2),  &
                          childarray(6,1,2),childarray(6,2,2),.TRUE.,nbin,ndirin)
            END SELECT
        ELSE
            call Agrif_nbdim_VarEQvar(tempC%var,pttruetab,cetruetab,    &
                    child%var,childarray(:,1,2),childarray(:,2,2),nbdim)
        ENDIF
    ENDIF
!
#if defined AGRIF_MPI
!
!     tab2 contains the necessary limits of the parent grid for each processor

    if (Associated(child%var%list_update)) then
        call Agrif_Find_list_update(child%var%list_update,pttab,petab,                      &
                                    pttab_Child,pttab_Parent,nbdim,                         &
                                    find_list_update,tab4t,tab5t,memberinall,memberinall2,  &
                                    sendtoproc1,recvfromproc1,sendtoproc2,recvfromproc2)
    else
        find_list_update = .FALSE.
    endif

    if (.not.find_list_update) then
        tab3(:,1) = pttruetab(:)
        tab3(:,2) = cetruetab(:)
        tab3(:,3) = pttruetabwhole(:)
        tab3(:,4) = cetruetabwhole(:)
!
        call MPI_ALLGATHER(tab3,4*nbdim,MPI_INTEGER,tab4,4*nbdim,MPI_INTEGER,MPI_COMM_WORLD,code)

        if (.not.associated(tempCextend%var)) allocate(tempCextend%var)
        do k=0,Agrif_Nbprocs-1
            do j=1,4
                do i=1,nbdim
                    tab4t(i,k,j) = tab4(i,j,k)
                enddo
            enddo
        enddo

        memberin1(1) = memberin
        call MPI_ALLGATHER(memberin1,1,MPI_LOGICAL,memberinall,1,MPI_LOGICAL,MPI_COMM_WORLD,code)

        call Get_External_Data_first(tab4t(:,:,1),tab4t(:,:,2),tab4t(:,:,3),tab4t(:,:,4),   &
                                     nbdim,memberinall,                   &
                                     sendtoproc1,recvfromproc1,                             &
                                     tab4t(:,:,5),tab4t(:,:,6),tab4t(:,:,7),tab4t(:,:,8))
    endif

    call ExchangeSameLevel2(sendtoproc1,recvfromproc1,nbdim,        &
            tab4t(:,:,3),tab4t(:,:,4),tab4t(:,:,5),tab4t(:,:,6),    &
            tab4t(:,:,7),tab4t(:,:,8),memberin,tempC,tempCextend)

!      Call Get_External_Data(tempC,tempCextend,tab4t(:,:,1),
!     &            tab4t(:,:,2),
!     &            tab4t(:,:,3),tab4t(:,:,4),nbdim,memberin,memberin,
!     &            memberinall)

#else
    tempCextend%var => tempC%var
#endif
!
!     Update of the parent grid (tempP) from the child grid (tempC)
!
    IF (memberin) THEN
!
        IF (.not.associated(tempP%var)) allocate(tempP%var)
!
        call Agrif_nbdim_allocation(tempP%var,indmin,indmax,nbdim)
!
        if ( nbdim == 1 ) then
            tempP%var%array1 = 0.
            call Agrif_Update_1D_Recursive(TypeUpdate,tempP%var%array1,tempCextend%var%array1,  &
                    indmin,indmax,pttruetabwhole,cetruetabwhole,                                &
                    s_Child_temp,s_Parent_temp,ds_Child,ds_Parent,nbdim)
        endif
        if ( nbdim == 2 ) then
            call Agrif_Update_2D_Recursive(TypeUpdate,tempP%var%array2,tempCextend%var%array2,  &
                    indmin,indmax,pttruetabwhole,cetruetabwhole,                                &
                    s_Child_temp,s_Parent_temp,ds_Child,ds_Parent,nbdim)
        endif
        if ( nbdim == 3 ) then
            call Agrif_Update_3D_Recursive(TypeUpdate,tempP%var%array3,tempCextend%var%array3,  &
                    indmin,indmax,pttruetabwhole,cetruetabwhole,                                &
                    s_Child_temp,s_Parent_temp,ds_Child,ds_Parent,nbdim)
        endif
        if ( nbdim == 4 ) then
            call Agrif_Update_4D_Recursive(TypeUpdate,tempP%var%array4,tempCextend%var%array4,  &
                    indmin,indmax,pttruetabwhole,cetruetabwhole,                                &
                    s_Child_temp,s_Parent_temp,ds_Child,ds_Parent,nbdim)
        endif
        if ( nbdim == 5 ) then
            call Agrif_Update_5D_Recursive(TypeUpdate,tempP%var%array5,tempCextend%var%array5,  &
                    indmin,indmax,pttruetabwhole,cetruetabwhole,                                &
                    s_Child_temp,s_Parent_temp,ds_Child,ds_Parent,nbdim)
        endif
        if ( nbdim == 6 ) then
            call Agrif_Update_6D_Recursive(TypeUpdate,tempP%var%array6,tempCextend%var%array6,  &
                    indmin,indmax,pttruetabwhole,cetruetabwhole,                                &
                    s_Child_temp,s_Parent_temp,ds_Child,ds_Parent,nbdim)
        endif

        call Agrif_nbdim_deallocation(tempCextend%var,nbdim)
!
    ENDIF

#if defined AGRIF_MPI
    call Agrif_nbdim_Get_bound_dimension(parent%var,lowerbound,upperbound,nbdim)
    call Agrif_ChildGrid_to_ParentGrid()
    call Agrif_Childbounds(nbdim,lowerbound,upperbound, &
                           indminglob,indmaxglob,       &
                           indminglob2,indmaxglob2,member)
!
    IF (member) THEN
        call Agrif_GlobtoLocInd2(parentarray,lowerbound,upperbound,&
                                 indminglob2,indmaxglob2,nbdim,Agrif_Procrank,member)
    ENDIF

    call Agrif_ParentGrid_to_ChildGrid()

    if (.not.find_list_update) then
        tab3(:,1) = indmin(:)
        tab3(:,2) = indmax(:)
        tab3(:,3) = indminglob2(:)
        tab3(:,4) = indmaxglob2(:)
!
        call MPI_ALLGATHER(tab3,4*nbdim,MPI_INTEGER,tab4,4*nbdim,MPI_INTEGER,MPI_COMM_WORLD,code)

        IF (.not.associated(tempPextend%var)) allocate(tempPextend%var)
        DO k=0,Agrif_Nbprocs-1
            do j=1,4
                do i=1,nbdim
                    tab5t(i,k,j) = tab4(i,j,k)
                enddo
            enddo
        enddo

        memberin1(1) = member
        call MPI_ALLGATHER(memberin1,1,MPI_LOGICAL,memberinall2,1,MPI_LOGICAL,MPI_COMM_WORLD,code)
        call Get_External_Data_first(tab5t(:,:,1),tab5t(:,:,2),tab5t(:,:,3),tab5t(:,:,4),   &
                                     nbdim,memberinall2,sendtoproc2,recvfromproc2,          &
                                     tab5t(:,:,5),tab5t(:,:,6),tab5t(:,:,7),tab5t(:,:,8))

        call Agrif_Addto_list_update(child%var%list_update,pttab,petab,pttab_Child,pttab_Parent,    &
                                     nbdim,tab4t,tab5t,memberinall,memberinall2,                    &
                                     sendtoproc1,recvfromproc1,sendtoproc2,recvfromproc2)

    endif

    call ExchangeSameLevel2(sendtoproc2,recvfromproc2,nbdim,                    &
                            tab5t(:,:,3),tab5t(:,:,4),tab5t(:,:,5),tab5t(:,:,6),&
                            tab5t(:,:,7),tab5t(:,:,8),member,tempP,tempPextend)
#else
    tempPextend%var => tempP%var
    parentarray(:,1,1) = indmin
    parentarray(:,2,1) = indmax
    parentarray(:,1,2) = indmin
    parentarray(:,2,2) = indmax
    member = .TRUE.
#endif
!
!   Special values on the child grid
    if ( Agrif_UseSpecialValueFineGrid ) then
!
!cc         noraftab(1:nbdim) =
!cc     &    child % var % root_var % interptab(1:nbdim) == 'N'
!
#if defined AGRIF_MPI
!
!          allocate(childvalues% var)
!
!          Call Agrif_nbdim_allocation(childvalues%var,
!     &                      pttruetab,cetruetab,nbdim)
!          Call Agrif_nbdim_Full_VarEQvar(childvalues% var,
!     &                                tempC% var,
!     &                                nbdim)
!          Call Agrif_CheckMasknD(tempC,childvalues,
!     &                           pttruetab(1:nbdim),cetruetab(1:nbdim),
!     &                           pttruetab(1:nbdim),cetruetab(1:nbdim),
!     &                           noraftab(1:nbdim),nbdim)
!          Call Agrif_nbdim_deallocation(childvalues% var,nbdim)
!         Deallocate(childvalues % var)
!
#else
!
!          Call Agrif_nbdim_Get_bound_dimension(child%var,
!     &                              lowerbound,upperbound,nbdim)
!          Call Agrif_CheckMasknD(tempC,child,
!     &                           pttruetab(1:nbdim),cetruetab(1:nbdim),
!     &                           lowerbound,
!     &                           upperbound,
!     &                           noraftab(1:nbdim),nbdim)
!
#endif
!
    endif
!
!   Special values on the parent grid
    if (Agrif_UseSpecialValue) then
!
#if defined AGRIF_MPI
!
!          Call GiveAgrif_SpecialValueToTab_mpi(parent%var,tempP%var,
!     &                 parentarray,
!     &                 Agrif_SpecialValue,nbdim)
!
!
#else
!
!          Call GiveAgrif_SpecialValueToTab(parent%var,tempP%var,
!     &                  indmin,indmax,
!     &                  Agrif_SpecialValue,nbdim)
!
#endif
!
    endif
!
    IF (member) THEN

        IF (present(procname)) THEN
!
            call Agrif_ChildGrid_to_ParentGrid()
!
            SELECT CASE(nbdim)
            CASE(1)
                call procname( tempPextend%var%array1(          &
                        parentarray(1,1,1):parentarray(1,2,1)), &
                        parentarray(1,1,2),parentarray(1,2,2),.FALSE.,nbin,ndirin)
            CASE(2)
                call procname( tempPextend%var%array2(          &
                        parentarray(1,1,1):parentarray(1,2,1),  &
                        parentarray(2,1,1):parentarray(2,2,1)), &
                        parentarray(1,1,2),parentarray(1,2,2),  &
                        parentarray(2,1,2),parentarray(2,2,2),.FALSE.,nbin,ndirin)
            CASE(3)
                call procname( tempPextend%var%array3(          &
                        parentarray(1,1,1):parentarray(1,2,1),  &
                        parentarray(2,1,1):parentarray(2,2,1),  &
                        parentarray(3,1,1):parentarray(3,2,1)), &
                        parentarray(1,1,2),parentarray(1,2,2),  &
                        parentarray(2,1,2),parentarray(2,2,2),  &
                        parentarray(3,1,2),parentarray(3,2,2),.FALSE.,nbin,ndirin)
            CASE(4)
                call procname( tempPextend%var%array4(          &
                        parentarray(1,1,1):parentarray(1,2,1),  &
                        parentarray(2,1,1):parentarray(2,2,1),  &
                        parentarray(3,1,1):parentarray(3,2,1),  &
                        parentarray(4,1,1):parentarray(4,2,1)), &
                        parentarray(1,1,2),parentarray(1,2,2),  &
                        parentarray(2,1,2),parentarray(2,2,2),  &
                        parentarray(3,1,2),parentarray(3,2,2),  &
                        parentarray(4,1,2),parentarray(4,2,2),.FALSE.,nbin,ndirin)
            CASE(5)
                call procname( tempPextend%var%array5(          &
                        parentarray(1,1,1):parentarray(1,2,1),  &
                        parentarray(2,1,1):parentarray(2,2,1),  &
                        parentarray(3,1,1):parentarray(3,2,1),  &
                        parentarray(4,1,1):parentarray(4,2,1),  &
                        parentarray(5,1,1):parentarray(5,2,1)), &
                        parentarray(1,1,2),parentarray(1,2,2),  &
                        parentarray(2,1,2),parentarray(2,2,2),  &
                        parentarray(3,1,2),parentarray(3,2,2),  &
                        parentarray(4,1,2),parentarray(4,2,2),  &
                        parentarray(5,1,2),parentarray(5,2,2),.FALSE.,nbin,ndirin)
            CASE(6)
                call procname( tempPextend%var%array6(          &
                        parentarray(1,1,1):parentarray(1,2,1),  &
                        parentarray(2,1,1):parentarray(2,2,1),  &
                        parentarray(3,1,1):parentarray(3,2,1),  &
                        parentarray(4,1,1):parentarray(4,2,1),  &
                        parentarray(5,1,1):parentarray(5,2,1),  &
                        parentarray(6,1,1):parentarray(6,2,1)), &
                        parentarray(1,1,2),parentarray(1,2,2),  &
                        parentarray(2,1,2),parentarray(2,2,2),  &
                        parentarray(3,1,2),parentarray(3,2,2),  &
                        parentarray(4,1,2),parentarray(4,2,2),  &
                        parentarray(5,1,2),parentarray(5,2,2),  &
                        parentarray(6,1,2),parentarray(6,2,2),.FALSE.,nbin,ndirin)
            END SELECT
!
            call Agrif_ParentGrid_to_ChildGrid()
!
        ELSE
            SELECT CASE(nbdim)
            CASE(1)
                parent%var%array1(                                      &
                            parentarray(1,1,2):parentarray(1,2,2)) =    &
                    tempPextend%var%array1(                             &
                            parentarray(1,1,1):parentarray(1,2,1))
            CASE(2)
                parent%var%array2(                                      &
                            parentarray(1,1,2):parentarray(1,2,2),      &
                            parentarray(2,1,2):parentarray(2,2,2)) =    &
                    tempPextend%var%array2(                             &
                            parentarray(1,1,1):parentarray(1,2,1),      &
                            parentarray(2,1,1):parentarray(2,2,1))
            CASE(3)
                parent%var%array3(                                      &
                            parentarray(1,1,2):parentarray(1,2,2),      &
                            parentarray(2,1,2):parentarray(2,2,2),      &
                            parentarray(3,1,2):parentarray(3,2,2)) =    &
                    tempPextend%var%array3(                             &
                            parentarray(1,1,1):parentarray(1,2,1),      &
                            parentarray(2,1,1):parentarray(2,2,1),      &
                            parentarray(3,1,1):parentarray(3,2,1))
            CASE(4)
                parent%var%array4(                                      &
                            parentarray(1,1,2):parentarray(1,2,2),      &
                            parentarray(2,1,2):parentarray(2,2,2),      &
                            parentarray(3,1,2):parentarray(3,2,2),      &
                            parentarray(4,1,2):parentarray(4,2,2)) =    &
                    tempPextend%var%array4(                             &
                        parentarray(1,1,1):parentarray(1,2,1),          &
                        parentarray(2,1,1):parentarray(2,2,1),          &
                        parentarray(3,1,1):parentarray(3,2,1),          &
                        parentarray(4,1,1):parentarray(4,2,1))
            CASE(5)
                parent%var%array5(                                      &
                            parentarray(1,1,2):parentarray(1,2,2),      &
                            parentarray(2,1,2):parentarray(2,2,2),      &
                            parentarray(3,1,2):parentarray(3,2,2),      &
                            parentarray(4,1,2):parentarray(4,2,2),      &
                            parentarray(5,1,2):parentarray(5,2,2)) =    &
                    tempPextend%var%array5(                             &
                            parentarray(1,1,1):parentarray(1,2,1),      &
                            parentarray(2,1,1):parentarray(2,2,1),      &
                            parentarray(3,1,1):parentarray(3,2,1),      &
                            parentarray(4,1,1):parentarray(4,2,1),      &
                            parentarray(5,1,1):parentarray(5,2,1))
            CASE(6)
                parent%var%array6(                                      &
                            parentarray(1,1,2):parentarray(1,2,2),      &
                            parentarray(2,1,2):parentarray(2,2,2),      &
                            parentarray(3,1,2):parentarray(3,2,2),      &
                            parentarray(4,1,2):parentarray(4,2,2),      &
                            parentarray(5,1,2):parentarray(5,2,2),      &
                            parentarray(6,1,2):parentarray(6,2,2)) =    &
                    tempPextend%var%array6(                             &
                            parentarray(1,1,1):parentarray(1,2,1),      &
                            parentarray(2,1,1):parentarray(2,2,1),      &
                            parentarray(3,1,1):parentarray(3,2,1),      &
                            parentarray(4,1,1):parentarray(4,2,1),      &
                            parentarray(5,1,1):parentarray(5,2,1),      &
                            parentarray(6,1,1):parentarray(6,2,1))
            END SELECT
        ENDIF
!
        call Agrif_nbdim_deallocation(tempPextend%var,nbdim)
!
    ENDIF
!
#if defined AGRIF_MPI
    IF (memberin) THEN
        call Agrif_nbdim_deallocation(tempP%var,nbdim)
        call Agrif_nbdim_deallocation(tempC%var,nbdim)
    ENDIF
#endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_UpdatenD
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Prtbounds
!
!> calculates the bounds of the parent grid to be updated by the child grid
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Prtbounds ( nbdim, indmin, indmax, s_Parent_temp, s_Child_temp,        &
                             s_Child, ds_Child, s_Parent, ds_Parent,                    &
                             pttruetab, cetruetab, pttab_Child, pttab_Parent,           &
                             posvartab_child, TypeUpdate, loctab_Child                  &
#if defined AGRIF_MPI
                            ,pttruetabwhole, cetruetabwhole                             &
#endif
                )
!---------------------------------------------------------------------------------------------------
    INTEGER,                   intent(in)   :: nbdim
    INTEGER, DIMENSION(nbdim), intent(out)  :: indmin, indmax
    REAL,    DIMENSION(nbdim), intent(out)  :: s_Parent_temp, s_Child_temp
    REAL,    DIMENSION(nbdim), intent(in)   :: s_Child,  ds_child
    REAL,    DIMENSION(nbdim), intent(in)   :: s_Parent, ds_Parent
    INTEGER, DIMENSION(nbdim), intent(in)   :: pttruetab, cetruetab
    INTEGER, DIMENSION(nbdim), intent(in)   :: pttab_Child, pttab_Parent
    INTEGER, DIMENSION(nbdim), intent(in)   :: posvartab_child, TypeUpdate
    INTEGER, DIMENSION(nbdim), intent(in)   :: loctab_Child
#if defined AGRIF_MPI
    INTEGER,DIMENSION(nbdim), intent(out)   :: pttruetabwhole, cetruetabwhole
#endif
!
    REAL,DIMENSION(nbdim) :: dim_newmin,dim_newmax
    INTEGER :: i
#if defined AGRIF_MPI
    REAL    :: positionmin, positionmax
    INTEGER :: imin, imax
    INTEGER :: coeffraf
#endif
!
    do i = 1,nbdim
!
        dim_newmin(i) = s_Child(i) + (pttruetab(i) - pttab_Child(i)) * ds_Child(i)
        dim_newmax(i) = s_Child(i) + (cetruetab(i) - pttab_Child(i)) * ds_Child(i)
!
        indmin(i) = pttab_Parent(i) + agrif_ceiling((dim_newmin(i)-s_Parent(i))/ds_Parent(i))
        indmax(i) = pttab_Parent(i) + agrif_int(    (dim_newmax(i)-s_Parent(i))/ds_Parent(i))
!
#if defined AGRIF_MPI
        positionmin = s_Parent(i) + (indmin(i)-pttab_Parent(i))*ds_Parent(i)
        IF (loctab_Child(i) /= -3) THEN
            IF (posvartab_child(i) == 1) THEN
                IF      (TypeUpdate(i) == Agrif_Update_Average) THEN
                    positionmin = positionmin - ds_Parent(i)/2.
                ELSE IF (TypeUpdate(i) == Agrif_Update_Full_Weighting) THEN
                    positionmin = positionmin - (ds_Parent(i)-ds_Child(i))
                ENDIF
            ELSE
                IF (TypeUpdate(i) /= Agrif_Update_Full_Weighting) THEN
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
!
        imin = pttab_Child(i) + agrif_ceiling((positionmin-s_Child(i))/ds_Child(i))
        positionmin = s_Child(i)  + (imin - pttab_Child(i)) * ds_Child(i)
        positionmax = s_Parent(i) + (indmax(i)-pttab_Parent(i))*ds_Parent(i)
        pttruetabwhole(i) = imin
        
        IF (loctab_Child(i) /= -3) THEN
            IF (posvartab_child(i) == 1) THEN
                IF      (TypeUpdate(i) == Agrif_Update_Average) THEN
                    positionmax = positionmax  + ds_Parent(i)/2.
                ELSE IF (TypeUpdate(i) == Agrif_Update_Full_Weighting) THEN
                    positionmax = positionmax  + (ds_Parent(i)-ds_Child(i))
                ENDIF
            ELSE
                IF (TypeUpdate(i) /= Agrif_Update_Full_Weighting) THEN
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
        
        imax = pttab_Child(i) +agrif_int((positionmax-s_Child(i))/ds_Child(i))
        positionmax = s_Child(i) + (imax - pttab_Child(i)) * ds_Child(i)
        cetruetabwhole(i) = imax
#endif
!
        s_Parent_temp(i) = s_Parent(i) + (indmin(i) - pttab_Parent(i)) * ds_Parent(i)
        s_Child_temp(i)  = dim_newmin(i)

#if defined AGRIF_MPI
        s_Child_temp(i) = positionmin
#endif
!
    enddo
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Prtbounds
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Update_2D_Recursive
!
!> updates a 2D grid variable on the parent grid.
!! Calls #Agrif_Update_1D_Recursive and #Agrif_UpdateBase
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Update_2D_Recursive ( TypeUpdate, tempP, tempC, indmin, indmax,    &
                                       pttab_child, petab_child,                    &
                                       s_child,  s_parent,                          &
                                       ds_child, ds_parent, nbdim )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(nbdim), intent(in)   :: TypeUpdate       !< Type of update (copy or average)
    INTEGER, DIMENSION(nbdim), intent(in)   :: indmin, indmax
    INTEGER, DIMENSION(nbdim), intent(in)   :: pttab_child, petab_child
    REAL,    DIMENSION(nbdim), intent(in)   :: s_child,  s_parent
    REAL,    DIMENSION(nbdim), intent(in)   :: ds_child, ds_parent
    REAL,    DIMENSION(                 &
                indmin(1):indmax(1),    &
                indmin(2):indmax(2)),   &
                               intent(out)  :: tempP
!      REAL, DIMENSION(pttab_child(1):petab_child(1),&
!                     pttab_child(2):petab_child(2)) :: tempC
    REAL,    DIMENSION(:,:),   intent(in)   :: tempC
    INTEGER,                   intent(in)   :: nbdim
!
    REAL, DIMENSION(indmin(1):indmax(1),pttab_child(2):petab_child(2)) :: tabtemp
    REAL, DIMENSION(indmin(2):indmax(2),indmin(1):indmax(1))           :: tempP_trsp
    REAL, DIMENSION(pttab_child(2):petab_child(2),indmin(1):indmax(1)) :: tabtemp_trsp
    INTEGER :: i, j
    INTEGER :: coeffraf, locind_child_left
!
    tabtemp = 0.
    coeffraf = nint ( ds_parent(1) / ds_child(1) )
!
    IF((TypeUpdate(1) == AGRIF_Update_average) .AND. (coeffraf /= 1 )) THEN
!---CDIR NEXPAND
        if ( .NOT. precomputedone(1) ) then
            call Average1dPrecompute( petab_child(2)-pttab_child(2)+1,    &
                                      indmax(1)-indmin(1)+1,              &
                                      petab_child(1)-pttab_child(1)+1,    &
                                      s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
!            precomputedone(1) = .TRUE.
        endif
!---CDIR NEXPAND
        call Average1dAfterCompute( tabtemp, tempC, size(tabtemp), size(tempC), &
                    s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
!
    ELSE IF ((TypeUpdate(1) == AGRIF_Update_copy) .AND. (coeffraf /= 1 ))THEN
!---CDIR NEXPAND
         if ( .NOT. precomputedone(1) ) then
            call Copy1dPrecompute( petab_child(2)-pttab_child(2)+1, &
                                   indmax(1)-indmin(1)+1,           &
                                   petab_child(1)-pttab_child(1)+1, &
                                   s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
!            precomputedone(1) = .TRUE.
        endif
!---CDIR NEXPAND
        call Copy1dAfterCompute(tabtemp,tempC,size(tabtemp),size(tempC),1)
!
    ELSE
        do j = pttab_child(nbdim),petab_child(nbdim)
!
!---CDIR NEXPAND
            call Agrif_Update_1D_Recursive( TypeUpdate(1:nbdim-1),tabtemp(:,j),             &
                                            tempC(:,j-pttab_child(nbdim)+1),                &
                                            indmin(1:nbdim-1),indmax(1:nbdim-1),            &
                                            pttab_child(1:nbdim-1),petab_child(1:nbdim-1),  &
                                            s_child(1:nbdim-1),s_parent(1:nbdim-1),         &
                                            ds_child(1:nbdim-1),ds_parent(1:nbdim-1),nbdim-1)
        enddo
    ENDIF
!
    tabtemp_trsp = TRANSPOSE(tabtemp)
    coeffraf = nint(ds_parent(nbdim)/ds_child(nbdim))

!---CDIR NEXPAND
    call Agrif_Compute_nbdim_update(s_parent(nbdim),s_child(nbdim),ds_parent(nbdim),ds_child(nbdim),coeffraf,locind_child_left)
!
    tempP_trsp = 0.
!
    IF((TypeUpdate(2) == AGRIF_Update_average) .AND. (coeffraf /= 1 )) THEN
!---CDIR NEXPAND
        if ( .NOT. precomputedone(2) ) then
            call Average1dPrecompute( indmax(1)-indmin(1)+1,          &
                                      indmax(2)-indmin(2)+1,          &
                                      petab_child(2)-pttab_child(2)+1,&
                                      s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)
!            precomputedone(2) = .TRUE.
        endif
!---CDIR NEXPAND
        call Average1dAfterCompute( tempP_trsp, tabtemp_trsp, size(tempP_trsp), size(tabtemp_trsp),&
                s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)
!
    ELSE IF ((TypeUpdate(2) == AGRIF_Update_copy) .AND. (coeffraf /= 1 )) THEN
!---CDIR NEXPAND
        if ( .NOT. precomputedone(2) ) then
            call Copy1dPrecompute( indmax(1)-indmin(1)+1,             &
                                   indmax(2)-indmin(2)+1,             &
                                   petab_child(2)-pttab_child(2)+1,   &
                                   s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)
!            precomputedone(2) = .TRUE.
        endif
!---CDIR NEXPAND
        call Copy1dAfterCompute( tempP_trsp, tabtemp_trsp, size(tempP_trsp), size(tabtemp_trsp),2)
!
    ELSE
        do i = indmin(1),indmax(1)
!
!---CDIR NEXPAND
            call Agrif_UpdateBase(TypeUpdate(2),                                        &
                                  tempP_trsp(indmin(nbdim):indmax(nbdim),i),            &
                                  tabtemp_trsp(pttab_child(nbdim):petab_child(nbdim),i),&
                                  indmin(nbdim),indmax(nbdim),                          &
                                  pttab_child(nbdim),petab_child(nbdim),                &
                                  s_parent(nbdim),s_child(nbdim),                       &
                                  ds_parent(nbdim),ds_child(nbdim),                     &
                                  coeffraf,locind_child_left)
!
        enddo
    ENDIF
!
    tempP = TRANSPOSE(tempP_trsp)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Update_2D_Recursive
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Update_3D_Recursive
!
!> Updates a 3D grid variable on the parent grid.
!! Calls #Agrif_Update_2D_Recursive and #Agrif_UpdateBase.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Update_3D_Recursive ( TypeUpdate, tempP, tempC, indmin, indmax,    &
                                       pttab_child, petab_child,                    &
                                       s_child, s_parent, ds_child, ds_parent, nbdim )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(nbdim), intent(in)   :: TypeUpdate            ! TYPE of update (copy or average)
    INTEGER, DIMENSION(nbdim), intent(in)   :: indmin, indmax
    INTEGER, DIMENSION(nbdim), intent(in)   :: pttab_child, petab_child
    REAL,    DIMENSION(nbdim), intent(in)   :: s_child,  s_parent
    REAL,    DIMENSION(nbdim), intent(in)   :: ds_child, ds_parent
    REAL,    DIMENSION(                 &
                indmin(1):indmax(1),    &
                indmin(2):indmax(2),    &
                indmin(3):indmax(3)),           intent(out) :: tempP
    REAL, DIMENSION(                            &
                pttab_child(1):petab_child(1),  &
                pttab_child(2):petab_child(2),  &
                pttab_child(3):petab_child(3)), intent(in)  :: tempC
    INTEGER,                                    intent(in)  :: nbdim
!
    REAL, DIMENSION(                    &
                indmin(1):indmax(1),    &
                indmin(2):indmax(2),    &
                pttab_child(3):petab_child(3)) :: tabtemp
    INTEGER :: i,j,k
    INTEGER :: coeffraf,locind_child_left
    INTEGER :: kdeb
!
    coeffraf = nint ( ds_parent(1) / ds_child(1) )
!
    if ((TypeUpdate(1) == AGRIF_Update_average) .AND. (coeffraf /= 1 )) then
!---CDIR NEXPAND
        call Average1dPrecompute(petab_child(2)-pttab_child(2)+1,&
                                 indmax(1)-indmin(1)+1,&
                                 petab_child(1)-pttab_child(1)+1,&
                                 s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
        precomputedone(1) = .TRUE.
    else if ((TypeUpdate(1) == AGRIF_Update_copy) .AND. (coeffraf /= 1 )) then
!---CDIR NEXPAND
         call Copy1dPrecompute(petab_child(2)-pttab_child(2)+1, &
                               indmax(1)-indmin(1)+1,           &
                               petab_child(1)-pttab_child(1)+1, &
                               s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
        precomputedone(1) = .TRUE.
    endif
!
    coeffraf = nint ( ds_parent(2) / ds_child(2) )
!
    if ((TypeUpdate(2) == AGRIF_Update_average) .AND. (coeffraf /= 1 )) then
!---CDIR NEXPAND
        call Average1dPrecompute(indmax(1)-indmin(1)+1,&
                                 indmax(2)-indmin(2)+1,&
                                 petab_child(2)-pttab_child(2)+1,&
                                 s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)
        precomputedone(2) = .TRUE.
    else if ((TypeUpdate(2) == AGRIF_Update_copy) .AND. (coeffraf /= 1 )) then
!---CDIR NEXPAND
        call Copy1dPrecompute( indmax(1)-indmin(1)+1,           &
                               indmax(2)-indmin(2)+1,           &
                               petab_child(2)-pttab_child(2)+1, &
                               s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)
        precomputedone(2) = .TRUE.
    endif
!
    do k = pttab_child(nbdim),petab_child(nbdim)
        call Agrif_Update_2D_Recursive( TypeUpdate(1:nbdim-1),tabtemp(:,:,k),tempC(:,:,k),  &
                                        indmin(1:nbdim-1),indmax(1:nbdim-1),                &
                                        pttab_child(1:nbdim-1),petab_child(1:nbdim-1),      &
                                        s_child(1:nbdim-1),s_parent(1:nbdim-1),             &
                                        ds_child(1:nbdim-1),ds_parent(1:nbdim-1),nbdim-1)
    enddo
!
    precomputedone(1) = .FALSE.
    precomputedone(2) = .FALSE.
!
    coeffraf = nint ( ds_parent(3) / ds_child(3) )

    call Agrif_Compute_nbdim_update(s_parent(nbdim),s_child(nbdim), &
            ds_parent(nbdim),ds_child(nbdim),coeffraf,locind_child_left)
!
    if (coeffraf == 1) then
        kdeb = pttab_child(3)+locind_child_left-2
        do k=indmin(3),indmax(3)
            kdeb = kdeb + 1
            do j = indmin(2),indmax(2)
            do i = indmin(1),indmax(1)
                tempP(i,j,k) = tabtemp(i,j,kdeb)
            enddo
            enddo
        enddo
    else
        tempP = 0.
        do j = indmin(2),indmax(2)
        do i = indmin(1),indmax(1)
            call Agrif_UpdateBase(TypeUpdate(3),tempP(i,j,:),tabtemp(i,j,:),    &
                                  indmin(nbdim),indmax(nbdim),                  &
                                  pttab_child(nbdim),petab_child(nbdim),        &
                                  s_parent(nbdim),s_child(nbdim),               &
                                  ds_parent(nbdim),ds_child(nbdim),             &
                                  coeffraf,locind_child_left)
!
        enddo
        enddo
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Update_3D_Recursive
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Update_4D_Recursive
!
!> Updates a 4D grid variable on the parent grid.
!! Calls #Agrif_Update_3D_Recursive and #Agrif_UpdateBase.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Update_4D_Recursive ( TypeUpdate, tempP, tempC, indmin, indmax, &
                                       pttab_child, petab_child,                 &
                                       s_child, s_parent, ds_child, ds_parent, nbdim )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(nbdim), intent(in)   :: TypeUpdate            !< Type of update (copy or average)
    INTEGER, DIMENSION(nbdim), intent(in)   :: indmin, indmax
    INTEGER, DIMENSION(nbdim), intent(in)   :: pttab_child, petab_child
    REAL,    DIMENSION(nbdim), intent(in)   :: s_child,  s_parent
    REAL,    DIMENSION(nbdim), intent(in)   :: ds_child, ds_parent
    REAL,    DIMENSION(                 &
                indmin(1):indmax(1),    &
                indmin(2):indmax(2),    &
                indmin(3):indmax(3),    &
                indmin(4):indmax(4)),           intent(out) :: tempP
    REAL, DIMENSION(                            &
                pttab_child(1):petab_child(1),  &
                pttab_child(2):petab_child(2),  &
                pttab_child(3):petab_child(3),  &
                pttab_child(4):petab_child(4)), intent(in)  :: tempC
    INTEGER,                                    intent(in)  :: nbdim
!
    REAL, DIMENSION(:,:,:,:), allocatable   :: tabtemp
    INTEGER :: i,j,k,l
    INTEGER :: coeffraf,locind_child_left
!
    allocate(tabtemp(indmin(1):indmax(1), &
                     indmin(2):indmax(2), &
                     indmin(3):indmax(3), &
                     pttab_child(4):petab_child(4)))
!
    do l = pttab_child(nbdim),petab_child(nbdim)
        call Agrif_Update_3D_Recursive(TypeUpdate(1:nbdim-1),                               &
                                       tabtemp(indmin(nbdim-3):indmax(nbdim-3),             &
                                               indmin(nbdim-2):indmax(nbdim-2),             &
                                               indmin(nbdim-1):indmax(nbdim-1),l),          &
                                       tempC(pttab_child(nbdim-3):petab_child(nbdim-3),     &
                                             pttab_child(nbdim-2):petab_child(nbdim-2),     &
                                             pttab_child(nbdim-1):petab_child(nbdim-1),l),  &
                                       indmin(1:nbdim-1),indmax(1:nbdim-1),                 &
                                       pttab_child(1:nbdim-1),petab_child(1:nbdim-1),       &
                                       s_child(1:nbdim-1), s_parent(1:nbdim-1),             &
                                      ds_child(1:nbdim-1),ds_parent(1:nbdim-1),nbdim-1)
    enddo
!
    call Agrif_Compute_nbdim_update(s_parent(nbdim), s_child(nbdim),    &
                                   ds_parent(nbdim),ds_child(nbdim),coeffraf,locind_child_left)
    tempP = 0.
!
    do k = indmin(3),indmax(3)
    do j = indmin(2),indmax(2)
    do i = indmin(1),indmax(1)
        call Agrif_UpdateBase(TypeUpdate(4),                                        &
                              tempP(i,j,k,indmin(nbdim):indmax(nbdim)),             &
                              tabtemp(i,j,k,pttab_child(nbdim):petab_child(nbdim)), &
                              indmin(nbdim),indmax(nbdim),                          &
                              pttab_child(nbdim),petab_child(nbdim),                &
                              s_parent(nbdim),s_child(nbdim),                       &
                             ds_parent(nbdim),ds_child(nbdim),coeffraf,locind_child_left)
    enddo
    enddo
    enddo
!
    deallocate(tabtemp)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Update_4D_Recursive
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Update_5D_Recursive
!
!> Updates a 5D grid variable on the parent grid.
!! Calls #Agrif_Update_4D_Recursive and #Agrif_UpdateBase.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Update_5D_Recursive ( TypeUpdate, tempP, tempC, indmin, indmax, &
                                       pttab_child, petab_child,                 &
                                       s_child, s_parent, ds_child, ds_parent, nbdim )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(nbdim), intent(in)   :: TypeUpdate            !< Type of update (copy or average)
    INTEGER, DIMENSION(nbdim), intent(in)   :: indmin, indmax
    INTEGER, DIMENSION(nbdim), intent(in)   :: pttab_child, petab_child
    REAL,    DIMENSION(nbdim), intent(in)   :: s_child,  s_parent
    REAL,    DIMENSION(nbdim), intent(in)   :: ds_child, ds_parent
    REAL,    DIMENSION(                 &
                indmin(1):indmax(1),    &
                indmin(2):indmax(2),    &
                indmin(3):indmax(3),    &
                indmin(4):indmax(4),    &
                indmin(5):indmax(5)),           intent(out) :: tempP
    REAL, DIMENSION(                            &
                pttab_child(1):petab_child(1),  &
                pttab_child(2):petab_child(2),  &
                pttab_child(3):petab_child(3),  &
                pttab_child(4):petab_child(4),  &
                pttab_child(5):petab_child(5)), intent(in)  :: tempC
    INTEGER,                                    intent(in)  :: nbdim
!
    REAL, DIMENSION(:,:,:,:,:), allocatable   :: tabtemp
    INTEGER :: i,j,k,l,m
    INTEGER :: coeffraf,locind_child_left
!
    allocate(tabtemp(indmin(1):indmax(1), &
                     indmin(2):indmax(2), &
                     indmin(3):indmax(3), &
                     indmin(4):indmax(4), &
                     pttab_child(5):petab_child(5)))
!
    do m = pttab_child(nbdim),petab_child(nbdim)
        call Agrif_Update_4D_Recursive(TypeUpdate(1:nbdim-1),                               &
                                       tabtemp(indmin(nbdim-4):indmax(nbdim-4),             &
                                               indmin(nbdim-3):indmax(nbdim-3),             &
                                               indmin(nbdim-2):indmax(nbdim-2),             &
                                               indmin(nbdim-1):indmax(nbdim-1),m),          &
                                       tempC(pttab_child(nbdim-4):petab_child(nbdim-4),     &
                                             pttab_child(nbdim-3):petab_child(nbdim-3),     &
                                             pttab_child(nbdim-2):petab_child(nbdim-2),     &
                                             pttab_child(nbdim-1):petab_child(nbdim-1),m),  &
                                       indmin(1:nbdim-1),indmax(1:nbdim-1),                 &
                                       pttab_child(1:nbdim-1),petab_child(1:nbdim-1),       &
                                       s_child(1:nbdim-1), s_parent(1:nbdim-1),             &
                                      ds_child(1:nbdim-1),ds_parent(1:nbdim-1),nbdim-1)
    enddo
!
    call Agrif_Compute_nbdim_update(s_parent(nbdim), s_child(nbdim), &
                                   ds_parent(nbdim),ds_child(nbdim),coeffraf,locind_child_left)
    tempP = 0.
!
    do l = indmin(4),indmax(4)
    do k = indmin(3),indmax(3)
    do j = indmin(2),indmax(2)
    do i = indmin(1),indmax(1)
        call Agrif_UpdateBase(TypeUpdate(5),                                            &
                              tempP(i,j,k,l,indmin(nbdim):indmax(nbdim)),               &
                              tabtemp(i,j,k,l,pttab_child(nbdim):petab_child(nbdim)),   &
                              indmin(nbdim),indmax(nbdim),                              &
                              pttab_child(nbdim),petab_child(nbdim),                    &
                              s_parent(nbdim), s_child(nbdim),                          &
                             ds_parent(nbdim),ds_child(nbdim),coeffraf,locind_child_left)
    enddo
    enddo
    enddo
    enddo
!
    deallocate(tabtemp)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Update_5D_Recursive
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Update_6D_Recursive
!
!> Updates a 6D grid variable on the parent grid.
!! Calls #Agrif_Update_5D_Recursive and #Agrif_UpdateBase.
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Update_6D_Recursive ( TypeUpdate, tempP, tempC, indmin, indmax, &
                                       pttab_child, petab_child,                 &
                                       s_child, s_parent, ds_child, ds_parent, nbdim )
!---------------------------------------------------------------------------------------------------
    INTEGER, DIMENSION(nbdim), intent(in)   :: TypeUpdate            !< Type of update (copy or average)
    INTEGER, DIMENSION(nbdim), intent(in)   :: indmin, indmax
    INTEGER, DIMENSION(nbdim), intent(in)   :: pttab_child, petab_child
    REAL,    DIMENSION(nbdim), intent(in)   :: s_child,  s_parent
    REAL,    DIMENSION(nbdim), intent(in)   :: ds_child, ds_parent
    REAL,    DIMENSION(                 &
                indmin(1):indmax(1),    &
                indmin(2):indmax(2),    &
                indmin(3):indmax(3),    &
                indmin(4):indmax(4),    &
                indmin(5):indmax(5),    &
                indmin(6):indmax(6)),           intent(out) :: tempP
    REAL, DIMENSION(                            &
                pttab_child(1):petab_child(1),  &
                pttab_child(2):petab_child(2),  &
                pttab_child(3):petab_child(3),  &
                pttab_child(4):petab_child(4),  &
                pttab_child(5):petab_child(5),  &
                pttab_child(6):petab_child(6)), intent(in)  :: tempC
    INTEGER,                                    intent(in)  :: nbdim
!
    REAL, DIMENSION(:,:,:,:,:,:), allocatable   :: tabtemp
    INTEGER :: i,j,k,l,m,n
    INTEGER :: coeffraf,locind_child_left
!
    allocate(tabtemp(indmin(1):indmax(1), &
                     indmin(2):indmax(2), &
                     indmin(3):indmax(3), &
                     indmin(4):indmax(4), &
                     indmin(5):indmax(5), &
                     pttab_child(6):petab_child(6)))
!
    do n = pttab_child(nbdim),petab_child(nbdim)
        call Agrif_Update_5D_Recursive(TypeUpdate(1:nbdim-1),                               &
                                       tabtemp(indmin(nbdim-5):indmax(nbdim-5),             &
                                               indmin(nbdim-4):indmax(nbdim-4),             &
                                               indmin(nbdim-3):indmax(nbdim-3),             &
                                               indmin(nbdim-2):indmax(nbdim-2),             &
                                               indmin(nbdim-1):indmax(nbdim-1),n),          &
                                       tempC(pttab_child(nbdim-5):petab_child(nbdim-5),     &
                                             pttab_child(nbdim-4):petab_child(nbdim-4),     &
                                             pttab_child(nbdim-3):petab_child(nbdim-3),     &
                                             pttab_child(nbdim-2):petab_child(nbdim-2),     &
                                             pttab_child(nbdim-1):petab_child(nbdim-1),n),  &
                                       indmin(1:nbdim-1),indmax(1:nbdim-1),                 &
                                       pttab_child(1:nbdim-1),petab_child(1:nbdim-1),       &
                                       s_child(1:nbdim-1), s_parent(1:nbdim-1),             &
                                      ds_child(1:nbdim-1),ds_parent(1:nbdim-1),nbdim-1)
    enddo
!
    call Agrif_Compute_nbdim_update(s_parent(nbdim), s_child(nbdim), &
                                   ds_parent(nbdim),ds_child(nbdim),coeffraf,locind_child_left)
!
    tempP = 0.
!
    do m = indmin(5),indmax(5)
    do l = indmin(4),indmax(4)
    do k = indmin(3),indmax(3)
    do j = indmin(2),indmax(2)
    do i = indmin(1),indmax(1)
        call Agrif_UpdateBase(TypeUpdate(6),                                            &
                              tempP(i,j,k,l,m,indmin(nbdim):indmax(nbdim)),             &
                              tabtemp(i,j,k,l,m,pttab_child(nbdim):petab_child(nbdim)), &
                              indmin(nbdim),indmax(nbdim),                              &
                              pttab_child(nbdim),petab_child(nbdim),                    &
                              s_parent(nbdim),s_child(nbdim),                           &
                             ds_parent(nbdim),ds_child(nbdim),coeffraf,locind_child_left)
    enddo
    enddo
    enddo
    enddo
    enddo
!
    deallocate(tabtemp)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Update_6D_Recursive
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_UpdateBase
!
!> Calls the updating method chosen by the user (copy, average or full-weighting).
!---------------------------------------------------------------------------------------------------
subroutine Agrif_UpdateBase ( TypeUpdate, parenttab, childtab, indmin, indmax,  &
                              pttab_child, petab_child,                         &
                              s_parent, s_child, ds_parent, ds_child,           &
                              coeffraf, locind_child_left )
!---------------------------------------------------------------------------------------------------
    INTEGER,                                  intent(in) :: TypeUpdate
    INTEGER,                                  intent(in) :: indmin, indmax
    INTEGER,                                  intent(in) :: pttab_child,petab_child
    REAL, DIMENSION(indmin:indmax),           intent(out):: parenttab
    REAL, DIMENSION(pttab_child:petab_child), intent(in) :: childtab
    REAL,                                     intent(in) :: s_parent,  s_child
    REAL,                                     intent(in) :: ds_parent, ds_child
    INTEGER,                                  intent(in) :: coeffraf, locind_child_left
!
    if     ( TypeUpdate == AGRIF_Update_copy ) then
!
        call Agrif_Copy1D(parenttab,childtab,indmax-indmin+1,petab_child-pttab_child+1, &
                s_parent,s_child,ds_parent,ds_child)
!
    elseif ( TypeUpdate == AGRIF_Update_average ) then
!
        call average1D(parenttab,childtab,indmax-indmin+1,petab_child-pttab_child+1,    &
                s_parent,s_child,ds_parent,ds_child)
!
    elseif ( TypeUpdate == AGRIF_Update_full_weighting ) then
!
        call full_weighting1D(parenttab,childtab,indmax-indmin+1,petab_child-pttab_child+1, &
                s_parent,s_child,ds_parent,ds_child,coeffraf,locind_child_left)
!
    endif
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_UpdateBase
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_Compute_nbdim_update
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Compute_nbdim_update ( s_parent, s_child, ds_parent, ds_child, &
                                        coeffraf, locind_child_left )
!---------------------------------------------------------------------------------------------------
    real,    intent(in)     ::  s_parent,  s_child
    real,    intent(in)     :: ds_parent, ds_child
    integer, intent(out)    :: coeffraf, locind_child_left
!
    coeffraf = nint(ds_parent/ds_child)
    locind_child_left = 1 + agrif_int((s_parent-s_child)/ds_child)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Compute_nbdim_update
!===================================================================================================
!
#if defined AGRIF_MPI
!===================================================================================================
!  subroutine Agrif_Find_list_update
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Find_list_update ( list_update, pttab, petab, pttab_Child, pttab_Parent, nbdim, &
                                    find_list_update, tab4t, tab5t, memberinall, memberinall2,   &
                                    sendtoproc1, recvfromproc1, sendtoproc2, recvfromproc2 )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_List_Interp_Loc),                   pointer     :: list_update
    INTEGER,                                       intent(in)  :: nbdim
    INTEGER, DIMENSION(nbdim),                     intent(in)  :: pttab, petab
    INTEGER, DIMENSION(nbdim),                     intent(in)  :: pttab_Child, pttab_Parent
    LOGICAL,                                       intent(out) :: find_list_update
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1,8), intent(out) :: tab4t
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1,8), intent(out) :: tab5t
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1), intent(out) :: memberinall,memberinall2
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1), intent(out) :: sendtoproc1,recvfromproc1
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1), intent(out) :: sendtoproc2,recvfromproc2
!
    Type(Agrif_List_Interp_Loc), Pointer :: parcours
    INTEGER :: i
!
    find_list_update = .FALSE.
!
    parcours => list_update

    Find_loop :  do while ( associated(parcours) )
        do i = 1,nbdim
            IF ((pttab(i) /= parcours%interp_loc%pttab(i)) .OR. &
                (petab(i) /= parcours%interp_loc%petab(i)) .OR. &
                (pttab_child(i)  /= parcours%interp_loc%pttab_child(i)) .OR. &
                (pttab_parent(i) /= parcours%interp_loc%pttab_parent(i))) THEN
                parcours => parcours%suiv
                cycle Find_loop
            ENDIF
        enddo
!
        tab4t = parcours%interp_loc%tab4t(1:nbdim,0:Agrif_Nbprocs-1,1:8)
        tab5t = parcours%interp_loc%tab5t(1:nbdim,0:Agrif_Nbprocs-1,1:8)
        memberinall  =  parcours%interp_loc%memberinall(0:Agrif_Nbprocs-1)
        memberinall2 =  parcours%interp_loc%memberinall2(0:Agrif_Nbprocs-1)
        sendtoproc1 =   parcours%interp_loc%sendtoproc1(0:Agrif_Nbprocs-1)
        sendtoproc2 =   parcours%interp_loc%sendtoproc2(0:Agrif_Nbprocs-1)
        recvfromproc1 = parcours%interp_loc%recvfromproc1(0:Agrif_Nbprocs-1)
        recvfromproc2 = parcours%interp_loc%recvfromproc2(0:Agrif_Nbprocs-1)
!
        find_list_update = .TRUE.
        exit Find_loop
!
    enddo Find_loop
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Find_list_update
!===================================================================================================
!
!===================================================================================================
!  subroutine Agrif_AddTo_list_update
!---------------------------------------------------------------------------------------------------
subroutine Agrif_AddTo_list_update ( list_update, pttab, petab, pttab_Child, pttab_Parent,  &
                                     nbdim, tab4t, tab5t, memberinall, memberinall2,        &
                                     sendtoproc1, recvfromproc1, sendtoproc2, recvfromproc2 )
!---------------------------------------------------------------------------------------------------
    TYPE(Agrif_List_Interp_Loc), pointer :: list_update
    INTEGER,                                        intent(in) :: nbdim
    INTEGER, DIMENSION(nbdim),                      intent(in) :: pttab, petab
    INTEGER, DIMENSION(nbdim),                      intent(in) :: pttab_Child, pttab_Parent
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1,8),  intent(in) :: tab4t
    INTEGER, DIMENSION(nbdim,0:Agrif_Nbprocs-1,8),  intent(in) :: tab5t
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1),          intent(in) :: memberinall, memberinall2
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1),          intent(in) :: sendtoproc1, recvfromproc1
    LOGICAL, DIMENSION(0:Agrif_Nbprocs-1),          intent(in) :: sendtoproc2, recvfromproc2
!
    Type(Agrif_List_Interp_Loc), pointer :: parcours
!
    allocate(parcours)
    allocate(parcours%interp_loc)

    parcours%interp_loc%pttab(1:nbdim) = pttab(1:nbdim)
    parcours%interp_loc%petab(1:nbdim) = petab(1:nbdim)
    parcours%interp_loc%pttab_child(1:nbdim)  = pttab_child(1:nbdim)
    parcours%interp_loc%pttab_parent(1:nbdim) = pttab_parent(1:nbdim)
    
    allocate(parcours%interp_loc%tab4t(nbdim,0:Agrif_Nbprocs-1,8))
    allocate(parcours%interp_loc%tab5t(nbdim,0:Agrif_Nbprocs-1,8))

    allocate(parcours%interp_loc%memberinall (0:Agrif_Nbprocs-1))
    allocate(parcours%interp_loc%memberinall2(0:Agrif_Nbprocs-1))

    allocate(parcours%interp_loc%recvfromproc1(0:Agrif_Nbprocs-1))
    allocate(parcours%interp_loc%recvfromproc2(0:Agrif_Nbprocs-1))
    allocate(parcours%interp_loc%sendtoproc1(0:Agrif_Nbprocs-1))
    allocate(parcours%interp_loc%sendtoproc2(0:Agrif_Nbprocs-1))

    parcours%interp_loc%tab4t = tab4t
    parcours%interp_loc%tab5t = tab5t
    parcours%interp_loc%memberinall   = memberinall
    parcours%interp_loc%memberinall2  = memberinall2
    parcours%interp_loc%sendtoproc1   = sendtoproc1
    parcours%interp_loc%sendtoproc2   = sendtoproc2
    parcours%interp_loc%recvfromproc1 = recvfromproc1
    parcours%interp_loc%recvfromproc2 = recvfromproc2

    parcours%suiv => list_update
    list_update => parcours
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Addto_list_update
!===================================================================================================
#endif
!
!===================================================================================================
!  subroutine Agrif_Update_1D_Recursive
!
!> Updates a 1D grid variable on the parent grid
!---------------------------------------------------------------------------------------------------
subroutine Agrif_Update_1D_Recursive ( TypeUpdate, tempP, tempC, indmin, indmax,    &
                                       pttab_child, petab_child,                    &
                                       s_child,  s_parent,                          &
                                       ds_child, ds_parent, nbdim )
!---------------------------------------------------------------------------------------------------
    INTEGER                   :: nbdim
    INTEGER, DIMENSION(nbdim) :: TypeUpdate            ! TYPE of update (copy or average)
    INTEGER, DIMENSION(nbdim) :: indmin,indmax
    INTEGER, DIMENSION(nbdim) :: pttab_child,petab_child
    REAL, DIMENSION(nbdim)    :: s_child,s_parent
    REAL, DIMENSION(nbdim)    :: ds_child,ds_parent
    REAL, DIMENSION(indmin(nbdim):indmax(nbdim))           :: tempP
    REAL, DIMENSION(pttab_child(nbdim):petab_child(nbdim)) :: tempC
    INTEGER :: coeffraf,locind_child_left
!
    call Agrif_Compute_nbdim_update(s_parent(nbdim),s_child(nbdim),&
            ds_parent(nbdim),ds_child(nbdim),coeffraf,locind_child_left)
!
    call Agrif_UpdateBase(TypeUpdate(1),&
                  tempP(indmin(nbdim):indmax(nbdim)),&
                  tempC(pttab_child(nbdim):petab_child(nbdim)),&
                  indmin(nbdim),indmax(nbdim),&
                  pttab_child(nbdim),petab_child(nbdim),&
                  s_parent(nbdim),s_child(nbdim),&
                  ds_parent(nbdim),ds_child(nbdim),&
                  coeffraf,locind_child_left)
!---------------------------------------------------------------------------------------------------
end subroutine Agrif_Update_1D_Recursive
!===================================================================================================
!
end module Agrif_Update
