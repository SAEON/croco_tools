C     Agrif (Adaptive Grid Refinement In Fortran)
C
C     Copyright (C) 2003 Laurent Debreu (Laurent.Debreu@imag.fr)
C                        Christophe Vouland (Christophe.Vouland@imag.fr)    
C
C     This program is free software; you can redistribute it and/or modify
C     it under the terms of the GNU General Public License as published by
C     the Free Software Foundation; either version 2 of the License, or
C     (at your option) any later version.
C
C     This program is distributed in the hope that it will be useful,
C     but WITHOUT ANY WARRANTY; without even the implied warranty of
C     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C     GNU General Public License for more details.
C
C     You should have received a copy of the GNU General Public License
C     along with this program; if not, write to the Free Software
C     Foundation, Inc., 59 Temple Place-Suite 330, Boston, MA 02111-1307, USA.
C
C
C
CCC   Module Agrif_types  
C
      Module Agrif_types 
C
CCC   Description:
CCC   Definition of data types used in Agrif, of several variables and
C        PARAMETERs.     
C
      IMPLICIT NONE
      
C     Maximum refinement ratio

      INTEGER, PARAMETER :: Agrif_MaxRaff = 7
      
C     Maximum number of grids of the hierarchy
      INTEGER, PARAMETER :: Agrif_NbMaxGrids = 10
                  
C
C     **************************************************************************
CCC   TYPE Agrif_LRECTANGLE
C     **************************************************************************
C
CCC   Description:
CCC   Data TYPE allowing a grid to reach a grid on the same level or its child
CCC   grids.
C
      TYPE Agrif_lrectangle  
           TYPE(Agrif_rectangle) , Pointer :: r    ! to reach a child grid
           TYPE(Agrif_lrectangle), Pointer :: next ! to reach a grid on the 
                                                   !    same level
      End TYPE Agrif_lrectangle  
C
C     **************************************************************************
CCC   TYPE Agrif_RECTANGLE: 
C     **************************************************************************
C
CCC   Description:
CCC   Data TYPE to define several CHARACTERistics of a grid (number,position,
CCC   time and space refinement factors,...). 
C
      TYPE Agrif_rectangle
           INTEGER                         :: number      ! Number of the grid
           ! Number of child grids 
           INTEGER                         :: nbgridchild 
           ! Minimal position in the x,y and z direction
           INTEGER, DIMENSION(3)           :: imin        
           ! Maximal position in the x,y and z direction 
           INTEGER, DIMENSION(3)           :: imax 
           ! Space refinement factor in the x,y and z direction  
           INTEGER, DIMENSION(3)           :: spaceref    
           ! Time refinement factor in the x,y and z direction
           INTEGER, DIMENSION(3)           :: timeref     
           ! Pointer to reach a grid on  the same level or a child grid
           TYPE(Agrif_lrectangle), Pointer :: childgrids  
      End TYPE Agrif_rectangle  
C
C
C
C     **************************************************************************
CCC   TYPE Agrif_PGrid
C     **************************************************************************
C
CCC   Description:
CCC   Data TYPE to go over the grid hierarchy (used for the creation of this 
CCC   grid hierarchy and during the time integration).
C 
      TYPE Agrif_pgrid  
           ! allows to reach a child grid  
           TYPE(Agrif_grid) , Pointer :: gr
           ! allows to reach the grids of the same level 
           TYPE(Agrif_pgrid), Pointer :: next 
      End TYPE Agrif_pgrid 
C
C
C
C     **************************************************************************
CCC   TYPE Agrif_PVariable
C     **************************************************************************
C
CCC   Description:
CCC   Data TYPE to define a grid variable.
C
      TYPE Agrif_PVariable 
           ! This field allows to get the various CHARACTERistics 
           !   of the variable (defined by the Agrif_Variable data TYPE)
           TYPE(Agrif_Variable) , Pointer :: var  
           ! Pointer on the parent grid
           TYPE(Agrif_PVariable), Pointer :: parent_var 
      End TYPE Agrif_PVariable 
C
C     **************************************************************************
CCC   TYPE Agrif_Grid
C     **************************************************************************
C
CCC   Description:
CCC   Data TYPE to define a grid (position, space and time refinement factors).
C
      TYPE Agrif_grid  
           ! pointer on the parent grid 
           TYPE(Agrif_grid)                    ,Pointer :: parent      
           ! pointer on the child grids 
           TYPE(Agrif_pgrid)                   ,Pointer :: child_grids 
           ! List of the grid variables   
           TYPE(Agrif_PVariable), DIMENSION(:) ,Pointer :: tabvars  
           ! pointer on the save grid 
           TYPE(Agrif_grid)                    ,Pointer :: save_grid                
C
           ! Global x,y and z position
           REAL    ,DIMENSION(3)                   :: Agrif_x   
           ! Global space step in the x,y and z direction
           REAL    ,DIMENSION(3)                   :: Agrif_d
           ! number of cells in the x,y and z direction
           INTEGER ,DIMENSION(3)                   :: nb        
           ! minimal position in the x,y and z direction
           INTEGER ,DIMENSION(3)                   :: ix        
           ! Space refinement factor in the x,y and z direction
           INTEGER ,DIMENSION(3)                   :: spaceref  
           ! Time refinement factor in the x,y and z direction 
           INTEGER ,DIMENSION(3)                   :: timeref   
           INTEGER ,DIMENSION(:)          ,Pointer :: restore1D   
           INTEGER ,DIMENSION(:,:)        ,Pointer :: restore2D   
           INTEGER ,DIMENSION(:,:,:)      ,Pointer :: restore3D 
           INTEGER ,DIMENSION(:,:,:,:)    ,Pointer :: restore4D
           INTEGER ,DIMENSION(:,:,:,:,:)  ,Pointer :: restore5D
           INTEGER ,DIMENSION(:,:,:,:,:,:),Pointer :: restore6D
           ! number of time step  
           INTEGER                                 :: ngridstep 
           INTEGER                                 :: rank  
           !moving grid id
           INTEGER                                 :: grid_id
           ! number of the grid 
           INTEGER                                 :: fixedrank 
           ! fixed or moving grid ?  
           LOGICAL                                 :: fixed     
           LOGICAL                                 :: oldgrid   
C          LOGICALs indicating if the current grid has a common border 
C             with the root coarse grid
           LOGICAL ,DIMENSION(3)                   :: NearRootBorder
           LOGICAL ,DIMENSION(3)                   :: DistantRootBorder
C          Arrays for adaptive grid refinement
           REAL    ,DIMENSION(:)          ,Pointer :: valtabpoint1D
           REAL    ,DIMENSION(:,:)        ,Pointer :: valtabpoint2D
           REAL    ,DIMENSION(:,:,:)      ,Pointer :: valtabpoint3D
           INTEGER ,DIMENSION(:)          ,Pointer :: tabpoint1D
           INTEGER ,DIMENSION(:,:)        ,Pointer :: tabpoint2D
           INTEGER ,DIMENSION(:,:,:)      ,Pointer :: tabpoint3D
           Type(Agrif_List_Variables), Pointer     :: variables=>NULL()
           INTEGER                                 :: NbVariables = 0
           Type(Agrif_Flux), Pointer               :: fluxes => NULL()
           INTEGER                                 :: level
           ! level of the grid in the hierarchy
      End TYPE Agrif_grid
C
C     **************************************************************************
CCC   TYPE Agrif_VARIABLE
C     **************************************************************************
C     
CCC   Description:
CCC   Data TYPE to CHARACTERize a grid variable.   
C
      TYPE Agrif_Variable 
         CHARACTER*80 :: variablename
C       
         ! Pointer on the variable of the root grid 
         TYPE(Agrif_Variable), Pointer :: root_var 
C
         ! index of the first point in the REAL domain (x,y and z direction)
         INTEGER     ,DIMENSION(6)          :: point
         ! position of the variable on the cell (1 for the boarder of 
         !    the edge, 2 for the center)
         INTEGER     ,DIMENSION(:) ,Pointer :: posvar      => NULL() 
         ! Indication for the space interpolation (module Agrif_Boundary)
         INTEGER                   ,Pointer :: interpIndex => NULL() 
         ! number of DIMENSIONs of the grid variable
         INTEGER                            :: nbdim = 0             
         ! Array indicating the TYPE of DIMENSION (space or not) for 
         !    each of them 
         CHARACTER(6),DIMENSION(:) ,Pointer :: interptab   => NULL() 
C        Arrays containing the values of the grid variables (REAL)
         REAL                                    :: array0 
         REAL   , DIMENSION(:)          ,Pointer :: array1    => NULL()
         REAL   , DIMENSION(:,:)        ,Pointer :: array2    => NULL()
         REAL   , DIMENSION(:,:,:)      ,Pointer :: array3    => NULL()
         REAL   , DIMENSION(:,:,:,:)    ,Pointer :: array4    => NULL()
         REAL   , DIMENSION(:,:,:,:,:)  ,Pointer :: array5    => NULL()
         REAL   , DIMENSION(:,:,:,:,:,:),Pointer :: array6    => NULL()
C        Arrays containing the values of the grid variables (REAL*8)
         REAL*8                                 :: darray0
         REAL*8, DIMENSION(:)          ,Pointer :: darray1   => NULL()
         REAL*8, DIMENSION(:,:)        ,Pointer :: darray2   => NULL()
         REAL*8, DIMENSION(:,:,:)      ,Pointer :: darray3   => NULL()
         REAL*8, DIMENSION(:,:,:,:)    ,Pointer :: darray4   => NULL()
         REAL*8, DIMENSION(:,:,:,:,:)  ,Pointer :: darray5   => NULL()
         REAL*8, DIMENSION(:,:,:,:,:,:),Pointer :: darray6   => NULL()
C        Arrays containing the values of the grid variables (REAL*4)
         REAL*4                                 :: sarray0
         REAL*4, DIMENSION(:)          ,Pointer :: sarray1   => NULL()
         REAL*4, DIMENSION(:,:)        ,Pointer :: sarray2   => NULL()
         REAL*4, DIMENSION(:,:,:)      ,Pointer :: sarray3   => NULL()
         REAL*4, DIMENSION(:,:,:,:)    ,Pointer :: sarray4   => NULL()
         REAL*4, DIMENSION(:,:,:,:,:)  ,Pointer :: sarray5   => NULL()
         REAL*4, DIMENSION(:,:,:,:,:,:),Pointer :: sarray6   => NULL()
C        Arrays containing the values of the grid variables (LOGICAL)
         LOGICAL                                 :: larray0
         LOGICAL, DIMENSION(:)          ,Pointer :: larray1   => NULL()
         LOGICAL, DIMENSION(:,:)        ,Pointer :: larray2   => NULL()
         LOGICAL, DIMENSION(:,:,:)      ,Pointer :: larray3   => NULL()
         LOGICAL, DIMENSION(:,:,:,:)    ,Pointer :: larray4   => NULL()
         LOGICAL, DIMENSION(:,:,:,:,:)  ,Pointer :: larray5   => NULL()
         LOGICAL, DIMENSION(:,:,:,:,:,:),Pointer :: larray6   => NULL()
C         Arrays containing the values of the grid variables (INTEGER)   
         INTEGER                                 :: iarray0
         INTEGER, DIMENSION(:)          ,Pointer :: iarray1   => NULL()
         INTEGER, DIMENSION(:,:)        ,Pointer :: iarray2   => NULL()
         INTEGER, DIMENSION(:,:,:)      ,Pointer :: iarray3   => NULL()
         INTEGER, DIMENSION(:,:,:,:)    ,Pointer :: iarray4   => NULL()
         INTEGER, DIMENSION(:,:,:,:,:)  ,Pointer :: iarray5   => NULL()
         INTEGER, DIMENSION(:,:,:,:,:,:),Pointer :: iarray6   => NULL()
C
         INTEGER, DIMENSION(:)          ,Pointer :: restore1D => NULL()
         INTEGER, DIMENSION(:,:)        ,Pointer :: restore2D => NULL()
         INTEGER, DIMENSION(:,:,:)      ,Pointer :: restore3D => NULL()
         INTEGER, DIMENSION(:,:,:,:)    ,Pointer :: restore4D => NULL()
         INTEGER, DIMENSION(:,:,:,:,:)  ,Pointer :: restore5D => NULL()
         INTEGER, DIMENSION(:,:,:,:,:,:),Pointer :: restore6D => NULL()
C   
         CHARACTER(2050)                          :: carray0
         CHARACTER(200), DIMENSION(:)    ,Pointer :: carray1   => NULL()
         CHARACTER(200), DIMENSION(:,:)  ,Pointer :: carray2   => NULL()
C
         ! Array used for the time interpolation
         REAL   , DIMENSION(:,:)      ,Pointer :: oldvalues2D => NULL()

         ! if the variable should be restore -> =1
         LOGICAL :: restaure = .FALSE.
         ! the interpolation should be made in any case
         LOGICAL :: Interpolationshouldbemade = .FALSE. 
         INTEGER :: bcinf ! option bc
         INTEGER :: bcsup ! option bc
         INTEGER, DIMENSION(6) :: updateinf ! option update
         INTEGER, DIMENSION(6) :: updatesup ! option update         
         INTEGER, DIMENSION(6,6) :: bcTYPEinterp ! option bcinterp
         INTEGER, DIMENSION(6) :: TYPEinterp ! option interp
         INTEGER, DIMENSION(6) :: TYPEupdate ! option update
         
         INTEGER, DIMENSION(6) :: lb, ub
         
         Type(Agrif_List_Interp_Loc), Pointer :: list_interp => NULL()
         Type(Agrif_List_Interp_Loc), Pointer :: list_update => NULL()
C
      End TYPE Agrif_Variable 
      
      Type Agrif_Interp_Loc
      integer,dimension(6) :: pttab,petab,
     &                          pttab_Child,pttab_Parent = -99
      integer,dimension(6) :: indmin, indmax
      INTEGER,DIMENSION(6)    :: pttruetab,cetruetab
      logical :: member, memberin      
#if !defined AGRIF_MPI      
      integer,dimension(6) :: indminglob,indmaxglob
#else
      integer,dimension(6) :: indminglob2,indmaxglob2
      INTEGER,DIMENSION(6,2,2) :: parentarray
      INTEGER,DIMENSION(:,:,:), POINTER :: tab4t
      LOGICAL, DIMENSION(:), POINTER :: memberinall
      INTEGER,DIMENSION(:,:,:), POINTER :: tab5t
      LOGICAL, DIMENSION(:), POINTER :: memberinall2
      LOGICAL, DIMENSION(:), POINTER :: sendtoproc1    
      LOGICAL, DIMENSION(:), POINTER :: recvfromproc1      
      LOGICAL, DIMENSION(:), POINTER :: sendtoproc2      
      LOGICAL, DIMENSION(:), POINTER :: recvfromproc2      
#endif      
      End Type Agrif_Interp_Loc
      
      Type Agrif_List_Interp_Loc
      Type(Agrif_Interp_Loc), Pointer :: interp_loc
      Type(Agrif_List_Interp_Loc), Pointer :: suiv
      End Type Agrif_List_Interp_Loc

       TYPE Agrif_List_Variables
         Type(Agrif_PVariable), Pointer :: pvar
         Type(Agrif_List_Variables), Pointer :: nextvariable  => NULL()
       END TYPE Agrif_List_Variables
               
       TYPE Agrif_Profile
          character*80 :: profilename
C
         ! index of the first point in the REAL domain (x,y and z direction)
         INTEGER     ,DIMENSION(6)          :: point
         ! position of the variable on the cell (1 for the boarder of 
         !    the edge, 2 for the center)
         INTEGER     ,DIMENSION(:) ,Pointer :: posvar      => NULL() 
         ! Indication for the space interpolation (module Agrif_Boundary)
         INTEGER                   ,Pointer :: interpIndex => NULL() 
         ! number of DIMENSIONs of the grid variable
         INTEGER                            :: nbdim = 0             
         ! Array indicating the TYPE of DIMENSION (space or not) for 
         !    each of them 
         CHARACTER(6),DIMENSION(:) ,Pointer :: interptab   => NULL() 
         Type(Agrif_Variable), Pointer :: var
         Type(Agrif_Profile), Pointer :: nextprofile  => NULL()
       END TYPE Agrif_Profile
       
       Type(Agrif_Profile), Pointer :: Agrif_MyProfiles => NULL()
           
C  Boundaries Fluxes

      Type Agrif_Flux
        Character*80 fluxname
        Type(Agrif_Variable), Pointer :: fluxtabx
        Type(Agrif_Variable), Pointer :: fluxtaby
        Type(Agrif_Variable), Pointer :: fluxtabz        
        Type(Agrif_Profile), Pointer  :: profile
        Logical :: Fluxallocated = .FALSE.
        Type(Agrif_Flux), Pointer     :: nextflux => NULL()
      End Type Agrif_Flux      
C
C     **************************************************************************
CCC   Different PARAMETERs
C     **************************************************************************
      TYPE(Agrif_PVariable), DIMENSION(:) ,Pointer :: Agrif_tabvars
C
      ! this pointer always points on the root grid of the grid hierarchy
      TYPE(Agrif_grid)  ,Pointer :: Agrif_Mygrid      
      ! Pointer used in the Agrif_regrid subroutine (Agrif_Util module). 
      !    It contains  the safeguard of the grid hierarchy.
      TYPE(Agrif_pgrid) ,Pointer :: Agrif_oldmygrid   
      ! pointer to the current grid (the link is done by using the
      !    Agrif_Instance   procedure (module Agrif_Init)) 
      TYPE(Agrif_grid)  ,Pointer :: Agrif_Curgrid
      ! Pointer used in the Agrif_ChildGrid_to_ParentGrid and 
      !    Agrif_ParentGrid_to_ChildGrid subroutines 
      !    (Agrif_CurgridFunctions module). It contains the 
      !    safeguard of the current grid hierarchy.
      TYPE(Agrif_grid)  ,Pointer :: Agrif_saveCURGRID
C
      ! Problem DIMENSION
      INTEGER               :: Agrif_Probdim
      ! number of variables
      INTEGER               :: Agrif_NbVariables
      ! number of fixed grids in the grid hierarchy
      INTEGER               :: Agrif_nbfixedgrids            
      ! space refinement factor
      INTEGER ,DIMENSION(3) :: Agrif_coeffref
      ! time refinement factor
      INTEGER ,DIMENSION(3) :: Agrif_coeffreft
      ! LOGICAL to use special values on the parent grid
      LOGICAL               :: Agrif_UseSpecialValue
      ! LOGICAL to use special values on the parent grid
      LOGICAL               :: Agrif_UseSpecialValueInUpdate
      ! LOGICAL to use special values on the current grid
      LOGICAL               :: Agrif_UseSpecialValueFineGrid
      ! Special values on the parent grid
      REAL                  :: Agrif_SpecialValue
      ! Special values on the current grid
      REAL                  :: Agrif_SpecialValueFineGrid
C   clustering PARAMETERs
      INTEGER               :: Agrif_Regridding = 10
      INTEGER               :: Agrif_Minwidth
      REAL                  :: Agrif_Efficiency = 0.7
      INTEGER               :: MaxSearch = 5
      REAL    ,DIMENSION(3) :: Agrif_mind
C     PARAMETERs for the interpolation of the child grids
      ! linear interpolation
      INTEGER ,PARAMETER    :: Agrif_linear=1
      ! lagrange interpolation
      INTEGER ,PARAMETER    :: Agrif_lagrange=2
      ! spline interpolation
      INTEGER ,PARAMETER    :: Agrif_eno=3
      ! user s interpolation
      INTEGER ,PARAMETER    :: Agrif_user_interp=4
      ! constant interpolation
      INTEGER ,PARAMETER    :: Agrif_constant=5
      ! linear conservative interpolation 
      INTEGER ,PARAMETER    :: Agrif_linearconserv=6
      ! linear conservative interpolation       
      INTEGER ,PARAMETER    :: Agrif_linearconservlim=7
      INTEGER ,PARAMETER    :: Agrif_ppm=8 
      INTEGER ,PARAMETER    :: Agrif_weno=9         
C     PARAMETERs for the update of the parent grids      
      INTEGER ,PARAMETER    :: Agrif_Update_Copy=1           ! copy
      INTEGER ,PARAMETER    :: Agrif_Update_Average=2        ! average
      INTEGER ,PARAMETER    :: Agrif_Update_Full_Weighting=3 ! full-weighting 
      INTEGER ,PARAMETER    :: Agrif_Update_4rdorder=4 ! 4rd order
C     Raffinement grid switch definition
      ! Agrif_USE_ONLY_FIXED_GRIDS =1 if fixed grid mode
      INTEGER               :: Agrif_USE_ONLY_FIXED_GRIDS    
      ! Agrif_USE_FIXED_GRIDS = 1 if AMR mode + fixed grid 
      !    else only AMR mode
      INTEGER               :: Agrif_USE_FIXED_GRIDS
      INTEGER               :: Agrif_Maxlevelloc
C
#ifdef AGRIF_MPI
      INTEGER :: Agrif_Nbprocs  ! Number of processors
      INTEGER :: Agrif_ProcRank ! Rank of the current processor 
      INTEGER :: Agrif_Group    ! Group associated to MPI_COMM_WORLD
      INTEGER :: Agrif_MPIPREC
#endif
C
      contains
C   
      Integer Function agrif_ceiling(x)
C             
          Real :: x
          Integer ::i 
C
          i = floor(x)
C      
          if( abs(x - i).le.0.0001 )then
             agrif_ceiling = i
          else
             agrif_ceiling = i+1
          endif      
C
      End Function
C
      Integer Function agrif_int(x)
C             
          Real :: x
          Integer ::i 
C
          i = floor(x) + 1
C      
          if( abs(x - i).le.0.0001 )then
             agrif_int = i
          else
             agrif_int = i-1
          endif      
C
      End Function
      End Module Agrif_TYPEs  
