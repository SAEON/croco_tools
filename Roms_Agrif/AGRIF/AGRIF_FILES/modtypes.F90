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
!     Foundation, Inc., 59 Temple Place-Suite 330, Boston, MA 02111-1307, USA.
!
!
!
!
!> Definition of data types used in AGRIF, of several variables and parameters
!
module Agrif_Types
!
implicit none

integer, parameter :: Agrif_MaxRaff = 7       !< Maximum refinement ratio
integer, parameter :: Agrif_NbMaxGrids = 10   !< Maximum number of grids of the hierarchy
!
!===================================================================================================
type Agrif_LRectangle
!---------------------------------------------------------------------------------------------------
!<  Data type allowing a grid to reach a grid on the same level or its child grids
!
    type(Agrif_Rectangle) , pointer :: r    => NULL()   !< to reach a child grid
    type(Agrif_LRectangle), pointer :: next => NULL()   !< to reach a grid on the same level
!
!---------------------------------------------------------------------------------------------------
end type Agrif_LRectangle
!===================================================================================================
!
!===================================================================================================
type Agrif_Rectangle
!---------------------------------------------------------------------------------------------------
!<  Data type to define several characteristics of a grid (number, position, time and space
!<  refinement factors, etc).
!
    integer                         :: number       !< Number of the grid
    integer                         :: nbgridchild  !< Number of child grids
    integer, dimension(3)           :: imin         !< Minimal position in the x,y and z direction
    integer, dimension(3)           :: imax         !< Maximal position in the x,y and z direction
    integer, dimension(3)           :: spaceref     !< Space refinement factor in the x,y and z direction
    integer, dimension(3)           :: timeref      !< Time refinement factor in the x,y and z direction
    type(Agrif_LRectangle), pointer :: childgrids => NULL()   !< Pointer to reach a grid on the same level or a child grid
!
!---------------------------------------------------------------------------------------------------
end type Agrif_Rectangle
!===================================================================================================
!
!===================================================================================================
type Agrif_PGrid
!---------------------------------------------------------------------------------------------------
!<  Data type to go over the grid hierarchy (used for the creation of this grid hierarchy
!<  and during the time integration).
!
    type(Agrif_Grid) , pointer :: gr   => NULL()  !< allows to reach a child grid
    type(Agrif_PGrid), pointer :: next => NULL()  !< allows to reach the grids of the same level
!
!---------------------------------------------------------------------------------------------------
end type Agrif_PGrid
!===================================================================================================
!
!===================================================================================================
type Agrif_PVariable
!---------------------------------------------------------------------------------------------------
!<  Data type to define a grid variable.
!
    type(Agrif_Variable) , pointer  :: var        => NULL() !< Allows to get various characteristics of the variable
                                                            !<   (defined by the Agrif_Variable data type)
    type(Agrif_PVariable), pointer  :: parent_var => NULL() !< Pointer on the parent grid
!---------------------------------------------------------------------------------------------------
end type Agrif_PVariable
!===================================================================================================
!
!===================================================================================================
type Agrif_Grid
!---------------------------------------------------------------------------------------------------
!<  Data type to define a grid (position, space and time refinement factors).
!
    type(Agrif_Grid)                   , pointer :: parent      => NULL() !< pointer on the parent grid
    type(Agrif_PGrid)                  , pointer :: child_grids => NULL() !< pointer on the child grids
    type(Agrif_PVariable), dimension(:), pointer :: tabvars     => NULL() !< list of the grid variables
    type(Agrif_Grid)                   , pointer :: save_grid   => NULL() !< pointer on the save grid
!
    real   , dimension(3)              :: Agrif_x   !< global x,y and z position
    real   , dimension(3)              :: Agrif_d   !< global space step in the x,y and z direction
    integer, dimension(3)              :: nb        !< number of cells in the x,y and z direction
    integer, dimension(3)              :: ix        !< minimal position in the x,y and z direction
    integer, dimension(3)              :: spaceref  !< space refinement factor in the x,y and z direction
    integer, dimension(3)              :: timeref   !< Time refinement factor in the x,y and z direction
    integer                            :: ngridstep !< number of time steps
    integer                            :: rank
    integer                            :: grid_id   !< moving grid id
    integer                            :: fixedrank !< number of the grid
    logical                            :: fixed     !< fixed or moving grid ?
    logical                            :: oldgrid
!> \name logicals indicating if the current grid has a common border with the root coarse grid
!> @{
    logical, dimension(3)              :: NearRootBorder
    logical, dimension(3)              :: DistantRootBorder
!> @}
!> \name Arrays for adaptive grid refinement
!> @{
    integer, dimension(:)    ,   allocatable :: tabpoint1D
    integer, dimension(:,:)  ,   allocatable :: tabpoint2D
    integer, dimension(:,:,:),   allocatable :: tabpoint3D
!> @}
    type(Agrif_Flux)               , pointer :: fluxes    => NULL()
    type(Agrif_List_Variables)     , pointer :: variables => NULL()
    integer                                  :: NbVariables = 0
    integer                                  :: level    !< level of the grid in the hierarchy
!---------------------------------------------------------------------------------------------------
end type Agrif_Grid
!===================================================================================================
!
!===================================================================================================
type Agrif_Variable
!---------------------------------------------------------------------------------------------------
!<  Data type to characterize a grid variable.
!
    character*80 :: variablename
!
    type(Agrif_Variable), pointer  :: root_var => NULL() !< pointer on the variable of the root grid
!
    integer, dimension(6)              :: point                 !< index of the first point in the
                                                                !<    real domain (x,y and z direction)
    integer, dimension(:)    , pointer :: posvar      => NULL() !< position of the variable on the cell
                                                                !<   (1 for the boarder of the edge, 2 for the center)
    integer                  , pointer :: interpIndex => NULL() !< Indication for the space interpolation (module Agrif_Boundary)
    integer                            :: nbdim = 0             !< number of dimensions of the grid variable
    character(6),dimension(:), pointer :: interptab   => NULL() !< Array indicating the type of dimension (space or not)
                                                                !<   for each of them
!> \name Pointers to real Arrays containing the values of the grid variables
!> @{
    real  , dimension(:)          , pointer :: parray1 => NULL()
    real  , dimension(:,:)        , pointer :: parray2 => NULL()
    real  , dimension(:,:,:)      , pointer :: parray3 => NULL()
    real  , dimension(:,:,:,:)    , pointer :: parray4 => NULL()
    real  , dimension(:,:,:,:,:)  , pointer :: parray5 => NULL()
    real  , dimension(:,:,:,:,:,:), pointer :: parray6 => NULL()
!> @}
!> \name Arrays containing the values of the grid variables (real)
!> @{
    real                                        :: array0
    real  , dimension(:)          , allocatable :: array1
    real  , dimension(:,:)        , allocatable :: array2
    real  , dimension(:,:,:)      , allocatable :: array3
    real  , dimension(:,:,:,:)    , allocatable :: array4
    real  , dimension(:,:,:,:,:)  , allocatable :: array5
    real  , dimension(:,:,:,:,:,:), allocatable :: array6
!> @}
!> \name Arrays containing the values of the grid variables (real*8)
!> @{
    real*8                                      :: darray0
    real*8, dimension(:)          , allocatable :: darray1
    real*8, dimension(:,:)        , allocatable :: darray2
    real*8, dimension(:,:,:)      , allocatable :: darray3
    real*8, dimension(:,:,:,:)    , allocatable :: darray4
    real*8, dimension(:,:,:,:,:)  , allocatable :: darray5
    real*8, dimension(:,:,:,:,:,:), allocatable :: darray6
!> @}
!> \name Arrays containing the values of the grid variables (real*4)
!> @{
    real*4                                      :: sarray0
    real*4, dimension(:)          , allocatable :: sarray1
    real*4, dimension(:,:)        , allocatable :: sarray2
    real*4, dimension(:,:,:)      , allocatable :: sarray3
    real*4, dimension(:,:,:,:)    , allocatable :: sarray4
    real*4, dimension(:,:,:,:,:)  , allocatable :: sarray5
    real*4, dimension(:,:,:,:,:,:), allocatable :: sarray6
!> @}
!> \name Arrays containing the values of the grid variables (logical)
!> @{
    logical                                     :: larray0
    logical, dimension(:)          ,allocatable :: larray1
    logical, dimension(:,:)        ,allocatable :: larray2
    logical, dimension(:,:,:)      ,allocatable :: larray3
    logical, dimension(:,:,:,:)    ,allocatable :: larray4
    logical, dimension(:,:,:,:,:)  ,allocatable :: larray5
    logical, dimension(:,:,:,:,:,:),allocatable :: larray6
!> @}
!> \name Arrays containing the values of the grid variables (integer)
!> @{
    integer                                     :: iarray0
    integer, dimension(:)          ,allocatable :: iarray1
    integer, dimension(:,:)        ,allocatable :: iarray2
    integer, dimension(:,:,:)      ,allocatable :: iarray3
    integer, dimension(:,:,:,:)    ,allocatable :: iarray4
    integer, dimension(:,:,:,:,:)  ,allocatable :: iarray5
    integer, dimension(:,:,:,:,:,:),allocatable :: iarray6
!> @}
!> \name Arrays used to restore the values
!> @{
    integer, dimension(:)          , pointer :: restore1D => NULL()
    integer, dimension(:,:)        , pointer :: restore2D => NULL()
    integer, dimension(:,:,:)      , pointer :: restore3D => NULL()
    integer, dimension(:,:,:,:)    , pointer :: restore4D => NULL()
    integer, dimension(:,:,:,:,:)  , pointer :: restore5D => NULL()
    integer, dimension(:,:,:,:,:,:), pointer :: restore6D => NULL()
!> @}
!> \name Arrays containing the values of the grid variables (character)
!> @{
    character(2400)                             :: carray0
    character(2400), dimension(:)  , allocatable :: carray1
    character(2400), dimension(:,:), allocatable :: carray2
!> @}

    real, dimension(:,:), pointer :: oldvalues2D => NULL() !< Array used for the time interpolation

    logical :: restaure = .FALSE. !< =1 if the variable should be restored
    logical :: Interpolationshouldbemade = .FALSE. !< TRUE if the interpolation should be made in any case
    integer :: bcinf !< option bc
    integer :: bcsup !< option bc
    integer, dimension(6) :: updateinf      !< option update
    integer, dimension(6) :: updatesup      !< option update
    integer, dimension(6,6) :: BCTypeInterp !< option bcinterp
    integer, dimension(6) :: TypeInterp     !< option interp
    integer, dimension(6) :: TypeUpdate     !< option update

    integer, dimension(6) :: lb
    integer, dimension(6) :: ub

    type(Agrif_List_Interp_Loc), pointer :: list_interp => NULL()
    type(Agrif_List_Interp_Loc), pointer :: list_update => NULL()
!---------------------------------------------------------------------------------------------------
end type Agrif_Variable
!===================================================================================================
!
!===================================================================================================
type Agrif_Interp_Loc
!---------------------------------------------------------------------------------------------------
    integer,dimension(6)              :: pttab,petab, pttab_Child, pttab_Parent = -99
    integer,dimension(6)              :: indmin, indmax
    integer,dimension(6)              :: pttruetab,cetruetab
    logical :: member, memberin
#if !defined AGRIF_MPI
    integer,dimension(6)              :: indminglob,indmaxglob
#else
    integer,dimension(6)              :: indminglob2,indmaxglob2
    integer,dimension(6,2,2)          :: parentarray
    integer,dimension(:,:,:), pointer :: tab4t          => NULL()
    integer,dimension(:,:,:), pointer :: tab5t          => NULL()
    logical, dimension(:),    pointer :: memberinall    => NULL()
    logical, dimension(:),    pointer :: memberinall2   => NULL()
    logical, dimension(:),    pointer :: sendtoproc1    => NULL()
    logical, dimension(:),    pointer :: sendtoproc2    => NULL()
    logical, dimension(:),    pointer :: recvfromproc1  => NULL()
    logical, dimension(:),    pointer :: recvfromproc2  => NULL()
#endif
!---------------------------------------------------------------------------------------------------
end type Agrif_Interp_Loc
!===================================================================================================

!===================================================================================================
type Agrif_List_Interp_Loc
!---------------------------------------------------------------------------------------------------
    type(Agrif_Interp_Loc),      pointer :: interp_loc => NULL()
    type(Agrif_List_Interp_Loc), pointer :: suiv       => NULL()
!---------------------------------------------------------------------------------------------------
end type Agrif_List_Interp_Loc
!===================================================================================================

!===================================================================================================
type Agrif_List_Variables
!---------------------------------------------------------------------------------------------------
    type(Agrif_PVariable),      pointer :: pvar          => NULL()
    type(Agrif_List_Variables), pointer :: nextvariable  => NULL()
!---------------------------------------------------------------------------------------------------
end type Agrif_List_Variables
!===================================================================================================

!===================================================================================================
type Agrif_Profile
!---------------------------------------------------------------------------------------------------
    character*80 :: profilename
!
    integer :: nbdim = 0       !< number of dimensions of the grid variable

    !> index of the first point in the real domain (x,y and z direction)
    integer, dimension(6)          :: point

    !> Position of the variable on the cell (1 for the boarder of the edge, 2 for the center)
    integer, dimension(:), pointer :: posvar      => NULL()

    !> Indication for the space interpolation (module Agrif_Boundary)
    integer              , pointer :: interpIndex => NULL()

    !> Array indicating the type of dimension (space or not) for each of them
    character(6), dimension(:), pointer :: interptab   => NULL()

    type(Agrif_Variable), pointer   :: var          => NULL()
    type(Agrif_Profile),  pointer   :: nextprofile  => NULL()
!---------------------------------------------------------------------------------------------------
end type Agrif_Profile
!===================================================================================================

type(Agrif_Profile), pointer :: Agrif_MyProfiles => NULL()

!  Boundaries Fluxes

!===================================================================================================
type Agrif_Flux
!---------------------------------------------------------------------------------------------------
    character*80 fluxname
    type(Agrif_Variable), pointer :: fluxtabx => NULL()
    type(Agrif_Variable), pointer :: fluxtaby => NULL()
    type(Agrif_Variable), pointer :: fluxtabz => NULL()
    type(Agrif_Profile),  pointer :: profile  => NULL()
    logical :: Fluxallocated = .FALSE.
    type(Agrif_Flux), pointer     :: nextflux => NULL()
!---------------------------------------------------------------------------------------------------
end type Agrif_Flux
!===================================================================================================
!
!===================================================================================================
!> Different parameters
!
    type(Agrif_PVariable), dimension(:), pointer :: Agrif_tabvars => NULL()
!
    !> this pointer always points on the root grid of the grid hierarchy
    type(Agrif_Grid) , pointer :: Agrif_Mygrid => NULL()

    !> Pointer used in the \link Agrif_Util::Agrif_Regrid() Agrif_regrid \endlink subroutine.
    !> It contains  the safeguard of the grid hierarchy.
    type(Agrif_PGrid), pointer :: Agrif_oldmygrid => NULL()

    !> Pointer to the current grid (the link is done by using the Agrif_Instance procedure (\see module Agrif_Init))
    type(Agrif_Grid) , pointer :: Agrif_Curgrid => NULL()

    integer               :: Agrif_Probdim          !< Problem dimension
    integer               :: Agrif_NbVariables      !< Number of variables
    integer               :: Agrif_nbfixedgrids     !< Number of fixed grids in the grid hierarchy
    integer, dimension(3) :: Agrif_coeffref         !< Space refinement factor
    integer, dimension(3) :: Agrif_coeffreft        !< Time refinement factor
    logical               :: Agrif_UseSpecialValue          !< T if use special values on the parent grid
    logical               :: Agrif_UseSpecialValueInUpdate  !< T if use special values on the parent grid
    logical               :: Agrif_UseSpecialValueFineGrid  !< T if use special values on the current grid
    real                  :: Agrif_SpecialValue             !< Special value on the parent grid
    real                  :: Agrif_SpecialValueFineGrid     !< Special value on the current grid
!>
!> \name Clustering parameters
!> @{
    integer               :: Agrif_Regridding = 10
    integer               :: Agrif_Minwidth
    real                  :: Agrif_Efficiency = 0.7
    integer               :: MaxSearch = 5
    real, dimension(3)    :: Agrif_mind
!> @}
!> \name parameters for the interpolation of the child grids
!> @{
    integer, parameter    :: Agrif_linear = 1           !< linear interpolation
    integer, parameter    :: Agrif_lagrange = 2         !< lagrange interpolation
    integer, parameter    :: Agrif_eno = 3              !< spline interpolation
    integer, parameter    :: Agrif_user_interp = 4      !< user defined interpolation
    integer, parameter    :: Agrif_constant = 5         !< constant interpolation
    integer, parameter    :: Agrif_linearconserv = 6    !< linear conservative interpolation
    integer, parameter    :: Agrif_linearconservlim = 7 !< linear conservative interpolation
    integer, parameter    :: Agrif_ppm = 8              !< PPM interpolation
    integer, parameter    :: Agrif_weno = 9             !< WENO5 interpolation
!> @}
!> \name parameters for the update of the parent grids
!> @{
    integer, parameter    :: Agrif_Update_Copy = 1              !< copy
    integer, parameter    :: Agrif_Update_Average = 2           !< average
    integer, parameter    :: Agrif_Update_Full_Weighting = 3    !< full-weighting
!> @}
!> \name Raffinement grid switches
!> @{
    integer               :: Agrif_USE_ONLY_FIXED_GRIDS   !< = 1 if fixed grid mode
    integer               :: Agrif_USE_FIXED_GRIDS        !< = 1 if AMR mode + fixed grid else only AMR mode
!> @}
    integer               :: Agrif_Maxlevelloc
!
#if defined AGRIF_MPI
    integer :: Agrif_Nbprocs  !< Number of processors
    integer :: Agrif_ProcRank !< Rank of the current processor
    integer :: Agrif_Group    !< Group associated to MPI_COMM_WORLD
    integer :: Agrif_MPIPREC
#endif
!
contains
!
!===================================================================================================
!  function Agrif_Ceiling
!---------------------------------------------------------------------------------------------------
integer function Agrif_Ceiling ( x )
!---------------------------------------------------------------------------------------------------
    real,   intent(in) :: x
!
    integer   :: i
!
    i = FLOOR(x)
!
    if( ABS(x - i) <= 0.0001 )then
        Agrif_Ceiling = i
    else
        Agrif_Ceiling = i+1
    endif
!---------------------------------------------------------------------------------------------------
end function Agrif_Ceiling
!===================================================================================================
!
!===================================================================================================
!  function Agrif_Int
!---------------------------------------------------------------------------------------------------
    integer function Agrif_Int(x)
!---------------------------------------------------------------------------------------------------
    real,   intent(in) :: x
!
    integer :: i
!
    i = FLOOR(x) + 1
!
    if( ABS(x - i) <= 0.0001 )then
        Agrif_Int = i
    else
        Agrif_Int = i-1
    endif
!---------------------------------------------------------------------------------------------------
end function Agrif_Int
!===================================================================================================
!
end module Agrif_Types
