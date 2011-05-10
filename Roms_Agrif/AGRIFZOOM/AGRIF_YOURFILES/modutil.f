

!
! $Id$
!
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
C     Foundation, Inc., 59 Temple Place-  Suite 330, Boston, MA 02111-1307, USA.
C
C
C
CCC   Module Agrif_Util 
C 
      Module Agrif_Util 
C
C
CCC   Description:   
CCC   This module contains the two procedures called in the main program : 
CCC   Agrif_Init_Grids allowing the initialization of the root coarse grid, and
CCC   Agrif_Step allowing the creation of the grid hierarchy and the management
CCC   of the time integration. 
C
C     Modules used:
C
      Use Agrif_Clustering    
      Use Agrif_bcfunction
C      
      IMPLICIT NONE
C
      Contains
C     Definition of procedures contained in this module.
C
C     **************************************************************************
CCC   Subroutine Agrif_Step
C     **************************************************************************
C 
      Subroutine Agrif_Step(procname) 
C
CCC   Description:
CCC   Subroutine to create the grid hierarchy and to manage the time integration 
CCC   procedure. It is called in the main program.
C
CC    Method:
CC    Call on subroutines Agrif_Regrid and Agrif_Integrate.
C
C     Declarations:
C
      Optional :: procname
      External :: procname
C
C
C     Creation and initialization of the grid hierarchy 
C
C  
C    Set the clustering variables
C
      Call Agrif_clustering_def()
C      
      if ( Agrif_USE_ONLY_FIXED_GRIDS .EQ. 1 ) then
C     
         If (Agrif_Mygrid % ngridstep == 0) then
C
            Call Agrif_Regrid
C 
            Call Agrif_Instance           
     &          (Agrif_Mygrid)           
         endif
C
      else
C    
         If (mod(Agrif_Mygrid % ngridstep, Agrif_Regridding) == 0) then
C
            Call Agrif_Regrid
C  
            Call Agrif_Instance           
     &          (Agrif_Mygrid)  
C            
         endif
C
      endif
C
C     Time integration of the grid hierarchy
C
      If (present(procname)) Then
         Call Agrif_Integrate (Agrif_Mygrid,procname)
      Else
         Call Agrif_Integrate (Agrif_Mygrid)      
      endif
C    
      If (associated(Agrif_Mygrid%child_grids)) 
     &   Call Agrif_Instance (Agrif_Mygrid)
C
      Return
C             
      End Subroutine Agrif_Step      

C     **************************************************************************
CCC   Subroutine Agrif_Step_Child
C     **************************************************************************
C
      Subroutine Agrif_Step_Child(procname)
C
CCC   Description:
CCC   Subroutine to create the grid hierarchy and to manage the time integration
CCC   procedure. It is called in the main program.
C
CC    Method:
CC    Call on subroutines Agrif_Regrid and Agrif_Integrate.
C
C     Declarations:
C
      Optional :: procname
      External :: procname
C
C     Time integration of the grid hierarchy
C
      If (present(procname)) Then
         Call Agrif_Integrate_Child (Agrif_Mygrid,procname)
      Else
         Call Agrif_Integrate_Child (Agrif_Mygrid)
      endif
C    
      If (associated(Agrif_Mygrid%child_grids))
     &   Call Agrif_Instance (Agrif_Mygrid)
C
      Return
C
      End Subroutine Agrif_Step_Child

C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Regrid
C     **************************************************************************
C
      Subroutine Agrif_Regrid
C
CCC   Description:
CCC   Subroutine to create the grid hierarchy from fixed grids and 
CC       adaptive mesh refinement.
C
CC    Method:        
C
C     Declarations:
C
C     Local variables      
      Type(Agrif_Rectangle), Pointer     :: coarsegrid_fixed
      Type(Agrif_Rectangle), Pointer     :: coarsegrid_moving  
      INTEGER                            :: j
      INTEGER :: nunit
      INTEGER                            :: iii
      Logical :: BEXIST
C 
      if ( Agrif_USE_ONLY_FIXED_GRIDS .EQ. 0 )
     &   Call Agrif_detect_all(Agrif_Mygrid) ! Detection of areas to be refined
C
      Allocate(coarsegrid_fixed)
      Allocate(coarsegrid_moving)
C
      if ( Agrif_USE_ONLY_FIXED_GRIDS .EQ. 0 ) 
     &    Call Agrif_Cluster_All(Agrif_Mygrid,coarsegrid_moving) ! Clustering
C
      if ( Agrif_USE_FIXED_GRIDS .EQ. 1 .OR. 
     &     Agrif_USE_ONLY_FIXED_GRIDS .EQ. 1 ) then
C
      If (Agrif_Mygrid % ngridstep == 0) Then
          nunit = Agrif_Get_Unit()
          open(nunit,file='AGRIF_FixedGrids.in',form='formatted',
     &           status="old",ERR=99) ! Opening of the Agrif_FixedGrids.in file
          j = 1
C         Creation of the grid hierarchy from the Agrif_FixedGrids.in file 
            do iii = 1 , Agrif_Probdim
               coarsegrid_fixed % imin(iii) = 1
               coarsegrid_fixed % imax(iii) = Agrif_Mygrid % nb(iii) + 1
            enddo
C      
            Call Agrif_Read_Fix_Grd (coarsegrid_fixed,j,nunit)     
            close(nunit) ! Closing of the Agrif_FixedGrids.in file 
C
          Nullify(Agrif_oldmygrid)
          Nullify(Agrif_Mygrid  % child_grids)
C        
C         Creation of the grid hierarchy from coarsegrid_fixed
            Call Agrif_Create_Grids (Agrif_Mygrid,coarsegrid_fixed)
        Else        
          Agrif_oldmygrid => Agrif_Mygrid % child_grids
         endif
      else
      Agrif_oldmygrid => Agrif_Mygrid % child_grids
      Nullify(Agrif_Mygrid  % child_grids)
      endif
C
      if ( Agrif_USE_ONLY_FIXED_GRIDS .EQ. 0 ) then
C      
      Call Agrif_Free_before_All(Agrif_oldmygrid)
C 
C     Creation of the grid hierarchy from coarsegrid_moving    
      Call Agrif_Create_Grids
     &     (Agrif_Mygrid,coarsegrid_moving)
C
      endif
C
C     Initialization of the grid hierarchy by copy or interpolation
C
      Call Agrif_Init_Hierarchy(Agrif_Mygrid)
C
      if ( Agrif_USE_ONLY_FIXED_GRIDS .EQ. 0 ) 
     &        Call Agrif_Free_after_All(Agrif_oldmygrid)
C
      Deallocate(coarsegrid_fixed)
      Deallocate(coarsegrid_moving)
C      
      Return
C
C     Opening error
C
   99 INQUIRE(FILE='AGRIF_FixedGrids.in',EXIST=BEXIST)
      If (.not. BEXIST) Then
          print*,'ERROR : File AGRIF_FixedGrids.in not found.'
          STOP 
        Else
          print*,'Error opening file AGRIF_FixedGrids.in'
          STOP         
      endif     
C      
      End Subroutine Agrif_Regrid
C
C     **************************************************************************
CCC   Subroutine Agrif_detect_All
C     **************************************************************************
C
      Recursive Subroutine Agrif_detect_all(g)
C
CCC   Description:
CCC   Subroutine to detect areas to be refined.
C
CC    Method:        
C
C     Declarations:
C
      
C      
C     Pointer argument    
      TYPE(Agrif_Grid) ,pointer  :: g        ! Pointer on the current grid
C      
C     Local variables
      Type(Agrif_pgrid),pointer  :: parcours ! Pointer for the recursive 
                                             ! procedure      
      INTEGER, DIMENSION(3)      :: size
      INTEGER                    :: iii
      Real :: g_eps     
C
      parcours => g % child_grids
C  
C     To be positioned on the finer grids of the grid hierarchy          
C  
      do while (associated(parcours))       
         Call Agrif_detect_all (parcours % gr)  
        parcours => parcours % next             
      enddo
C
      g_eps = huge(1.)
      do iii = 1 , Agrif_Probdim
         g_eps=min(g_eps,g%Agrif_d(iii))
      enddo
C
      g_eps = g_eps/100.
C          
      if ( Agrif_Probdim .EQ. 1 ) g%tabpoint1D=0
      if ( Agrif_Probdim .EQ. 2 ) g%tabpoint2D=0
      if ( Agrif_Probdim .EQ. 3 ) g%tabpoint3D=0
C      
      do iii = 1 , Agrif_Probdim
         if (g%Agrif_d(iii)/Agrif_coeffref(iii).LT.
     &                     (Agrif_mind(iii)-g_eps)) Return
      enddo
C                                       
      Call Agrif_instance(g)
C  
C     Detection (Agrif_detect is a user s routine)
C      
      do iii = 1 , Agrif_Probdim
         size(iii) = g%nb(iii) + 1
      enddo
C
      SELECT CASE (Agrif_Probdim)
      CASE (1) 
         Call Agrif_detect(g%tabpoint1D,size)
      CASE (2) 
         Call Agrif_detect(g%tabpoint2D,size)
      CASE (3) 
         Call Agrif_detect(g%tabpoint3D,size)
      END SELECT
C
C     Addition of the areas detected on the child grids
C      
      parcours => g % child_grids 
C           
      Do while (associated(parcours))
        Call Agrif_Add_detected_areas (g,parcours % gr)
        parcours => parcours % next 
      enddo
C     
      Return      
C
      End Subroutine Agrif_detect_all
C      
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Add_detected_areas
C     **************************************************************************
C      
      Subroutine Agrif_Add_detected_areas(parentgrid,childgrid)
C
CCC   Description:
CCC   Subroutine to add on the parent grid the areas detected 
CC       on its child grids.
C
CC    Method:        
C
C     Declarations:
C
      
C 
      Type(Agrif_Grid),pointer   :: parentgrid,childgrid
C      
      Integer :: i,j,k
C
      do i = 1,childgrid%nb(1)+1
         if ( Agrif_Probdim .EQ. 1 ) then
            If (childgrid%tabpoint1D(i).EQ.1) Then
                parentgrid%tabpoint1D(childgrid%ix(1)+
     &                 (i-1)/Agrif_Coeffref(1)) = 1
            endif
         else
            do j=1,childgrid%nb(2)+1
               if (Agrif_Probdim.EQ.2) then
              If (childgrid%tabpoint2D(i,j).EQ.1) Then
                  parentgrid%tabpoint2D(
     &                  childgrid%ix(1)+(i-1)/Agrif_Coeffref(1),
     &                  childgrid%ix(2)+(j-1)/Agrif_Coeffref(2)) = 1
                  endif
               else
                  do k=1,childgrid%nb(3)+1
                If (childgrid%tabpoint3D(i,j,k).EQ.1) Then
                    parentgrid%tabpoint3D(
     &                     childgrid%ix(1)+(i-1)/Agrif_Coeffref(1),
     &                     childgrid%ix(2)+(j-1)/Agrif_Coeffref(2),
     &                     childgrid%ix(3)+(k-1)/Agrif_Coeffref(3)) = 1
                     endif
                  enddo
               endif
            enddo
         endif
      enddo
C      
      Return
C
      End Subroutine Agrif_Add_detected_areas   
C      
C
C     **************************************************************************
CCC   Subroutine Agrif_Free_before_All
C     **************************************************************************
C
      Recursive Subroutine Agrif_Free_before_All(g)
C
CCC   Description:
C
CC    Method:        
C
C     Declarations:
C
C     Pointer argument    
      Type(Agrif_pgrid),pointer   :: g        ! Pointer on the current grid
C
C     Local pointer
      Type(Agrif_pgrid),pointer  :: parcours ! Pointer for the recursive 
                                             ! procedure      
C
C
      parcours => g        
C      
      Do while (associated(parcours))     
        If (.not. parcours%gr%fixed) Then
            Call Agrif_Free_data_before(parcours%gr)
            parcours % gr % oldgrid = .TRUE.        
        endif
C
        Call Agrif_Free_before_all (parcours % gr % child_grids)
C  
        parcours => parcours % next             
      enddo
C     
      Return      
C
C
      End Subroutine Agrif_Free_before_All
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Free_after_All
C     **************************************************************************
C
      Recursive Subroutine Agrif_Free_after_All(g)
C
CCC   Description:
C
CC    Method:        
C
C     Declarations:
C
      
C      
C     Pointer argument    
      Type(Agrif_pgrid),pointer   :: g       ! Pointer on the current grid
C
C     Local pointers
      TYPE(Agrif_pgrid),pointer  :: parcours ! Pointer for the recursive proced
      Type(Agrif_pgrid),pointer  :: preparcours   
      Type(Agrif_pgrid),pointer  :: preparcoursini 
C
C
      Allocate(preparcours)
C 
      preparcoursini => preparcours
C 
      Nullify(preparcours % gr)
C 
      preparcours % next => g
C
      parcours => g
C      
      Do while (associated(parcours))
C
         if ( (.NOT. parcours% gr% fixed) .AND.
     &        (parcours% gr% oldgrid    ) ) then
          Call Agrif_Free_data_after(parcours%gr)
         endif
C
         Call Agrif_Free_after_all (parcours % gr % child_grids)
C
      If (parcours % gr % oldgrid) Then
          Deallocate(parcours % gr)
          preparcours % next => parcours % next
          Deallocate(parcours)
          parcours => preparcours % next
        Else
          preparcours => preparcours % next
          parcours => parcours % next 
         endif       
      enddo
C
      Deallocate(preparcoursini)
C     
      Return      
C
      End Subroutine Agrif_Free_after_All
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Integrate
C     **************************************************************************
C
      Recursive Subroutine Agrif_Integrate(g, procname)         
C
CCC   Description:
CCC   Subroutine to manage the time integration of the grid hierarchy.
C
CC    Method:
CC    Recursive subroutine and call on subroutines Agrif_Instance & Agrif_Step
C
C     Declarations:
C
      
C      
C     Pointer argument    
      Type(Agrif_Grid),pointer   :: g        ! Pointer on the current grid
C
C     main procedure name
      Optional :: procname
      External :: procname      
C      
C     Local pointer
      Type(Agrif_pgrid),pointer  :: parcours ! Pointer for the recursive 
                                             ! procedure
C
C     Local scalars
      INTEGER                    :: nbt      ! Number of time steps 
                                             ! of the current grid
      INTEGER                    :: k        
      INTEGER                    :: iii
C  
C     Instanciation of the variables of the current grid
      If (g%fixedrank .NE.0) Then      
      Call Agrif_Instance
     &     (g)
      End If
C       
C     One step on the current grid      
C
      If (present(procname)) Then
         Call procname ()      
      Else
        write(*,*) 'The name of the step subroutine has not '
        write(*,*) 'been given in the subroutine Agrif_Integrate'
        stop
      endif
C
C     Number of time steps on the current grid      
C
      g%ngridstep = g % ngridstep + 1
C      
      parcours => g % child_grids
C    
C     Recursive procedure for the time integration of the grid hierarchy      
      Do while (associated(parcours))
C
C       Instanciation of the variables of the current grid            
        Call Agrif_Instance
     &           (parcours % gr)
C       
C       Number of time steps
        nbt = 1
        do iii = 1 , Agrif_Probdim
           nbt = max(nbt, parcours % gr % timeref(iii))
        enddo
C       
        Do k = 1,nbt
C       
           If (present(procname)) Then
                 Call Agrif_Integrate (parcours % gr, procname)
           Else
                 Call Agrif_Integrate (parcours % gr)      
           endif
C       
        enddo     
C  
        parcours => parcours % next
C     
      enddo
C
C      
      End Subroutine Agrif_Integrate 

C     **************************************************************************
CCC   Subroutine Agrif_Integrate_Child
C     **************************************************************************
C
      Recursive Subroutine Agrif_Integrate_Child(g,procname)
C
CCC   Description:
CCC   Subroutine to manage the time integration of the grid hierarchy.
C
CC    Method:
CC    Recursive subroutine and call on subroutines Agrif_Instance & Agrif_Step.
C
C     Declarations:
C

C
C     Pointer argument
      Type(Agrif_Grid),pointer   :: g        ! Pointer on the current grid
C
C     main procedure name
      Optional :: procname
      External :: procname
C
C     Local pointer
      Type(Agrif_pgrid),pointer  :: parcours ! Pointer for the recursive
                                             ! procedure
C
C     One step on the current grid
C
      If (present(procname)) Then
         Call procname ()      
      Else
        write(*,*) 'The name of the step subroutine has not '
        write(*,*) 'been given in the subroutine Agrif_Integrate'
        stop
      endif
C
C     Number of time steps on the current grid
C
C
      parcours => g % child_grids
C
C     Recursive procedure for the time integration of the grid hierarchy      
      Do while (associated(parcours))
C
C       Instanciation of the variables of the current grid
        Call Agrif_Instance
     &           (parcours % gr)

C       
           If (present(procname)) Then
                 Call Agrif_Integrate_Child (parcours % gr, procname)
           Else
                 Call Agrif_Integrate_Child (parcours % gr)
           endif
C  
        parcours => parcours % next
C     
      enddo
C
C
      End Subroutine Agrif_Integrate_Child

C
C
C     **************************************************************************
CCC   Subroutine Agrif_Init_Grids
C     **************************************************************************
C      
      Subroutine Agrif_Init_Grids
C
CCC   Description:
CCC   Subroutine to initialize the root coarse grid pointed by Agrif_Mygrid. 
CCC   It is called in the main program.
C
C     Declarations:
C
C
      INTEGER :: iii
C
C             definition of the probdim and modtypes variables
C
      Call Agrif_probdim_modtype_def()
C      
      Agrif_UseSpecialValue = .FALSE.
      Agrif_UseSpecialValueFineGrid = .FALSE.
      Agrif_SpecialValue = 0.
      Agrif_SpecialValueFineGrid = 0.
C      
C     Allocation of Agrif_Mygrid
      allocate(Agrif_Mygrid)
C      
C     Space and time refinement factors are set to 1 on the root grid
C
      do iii = 1 , Agrif_Probdim
         Agrif_Mygrid % spaceref(iii) = 1
         Agrif_Mygrid % timeref(iii) = 1
      enddo
C      
C     Initialization of the number of time steps    
      Agrif_Mygrid % ngridstep = 0
      Agrif_Mygrid % grid_id = 0
C
C     No parent grid for the root coarse grid
      Nullify(Agrif_Mygrid % parent)
C      
C     Initialization of the minimum positions, global abscissa and space steps  
      do iii= 1 ,  Agrif_Probdim
         Agrif_Mygrid % ix(iii) = 1            
         Agrif_Mygrid % Agrif_x(iii) = 0.
         Agrif_Mygrid % Agrif_d(iii) = 1. 
C     Borders of the root coarse grid 
         Agrif_Mygrid % NearRootBorder(iii) = .true.  
         Agrif_Mygrid % DistantRootBorder(iii) = .true.      
      enddo
C  
C     The root coarse grid is a fixed grid
      Agrif_Mygrid % fixed = .TRUE.
C      
C     Number of the grid pointed by Agrif_Mygrid (root coarse grid)
      Agrif_Mygrid % rank = 1      
C 
C     Number of the root grid as a fixed grid   
      Agrif_Mygrid % fixedrank = 0  
C 
C     Initialization of some fields of the root grid variables     
      Call Agrif_Create_Var (Agrif_Mygrid)
C      
C     Initialization of the other fields of the root grid variables (number of 
C     cells, positions, number and type of its dimensions, ...)  
      Call Agrif_Set_numberofcells(Agrif_Mygrid)
C      
      Call Agrif_Instance (Agrif_Mygrid)        
C             
C     Allocation of the array containing the values of the grid variables
      Call Agrif_Allocation (Agrif_Mygrid)
C        
      Call Agrif_initialisations(Agrif_Mygrid)
C      
      nullify(Agrif_Mygrid % child_grids)
C 
C     Total number of fixed grids   
      Agrif_nbfixedgrids = 0         
C      
      Call Agrif_Instance (Agrif_Mygrid)
C        
      End Subroutine Agrif_Init_Grids  
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Deallocation
C     **************************************************************************
C      
      Subroutine Agrif_Deallocation
C
CCC   Description:
CCC   Subroutine to initialize the root coarse grid pointed by Agrif_Mygrid. 
CCC   It is called in the main program.
C
C     Declarations:
C
C
      INTEGER :: nb
C
C             definition of the probdim and modtypes variables
C
      do nb = 1, Agrif_NbVariables
          if ( associated(Agrif_Mygrid % tabvars(nb) % var % array1) ) 
     &       Deallocate(Agrif_Mygrid % tabvars(nb) % var % array1)
          if ( associated(Agrif_Mygrid % tabvars(nb) % var % array2) ) 
     &       Deallocate(Agrif_Mygrid % tabvars(nb) % var % array2)
          if ( associated(Agrif_Mygrid % tabvars(nb) % var % array3) ) 
     &       Deallocate(Agrif_Mygrid % tabvars(nb) % var % array3)
          if ( associated(Agrif_Mygrid % tabvars(nb) % var % array4) ) 
     &       Deallocate(Agrif_Mygrid % tabvars(nb) % var % array4)
          if ( associated(Agrif_Mygrid % tabvars(nb) % var % array5) ) 
     &       Deallocate(Agrif_Mygrid % tabvars(nb) % var % array5)
          if ( associated(Agrif_Mygrid % tabvars(nb) % var % array6) ) 
     &       Deallocate(Agrif_Mygrid % tabvars(nb) % var % array6)
C
          if ( associated(Agrif_Mygrid % tabvars(nb) % var % iarray1) ) 
     &       Deallocate(Agrif_Mygrid % tabvars(nb) % var % iarray1)
          if ( associated(Agrif_Mygrid % tabvars(nb) % var % iarray2) ) 
     &       Deallocate(Agrif_Mygrid % tabvars(nb) % var % iarray2)
          if ( associated(Agrif_Mygrid % tabvars(nb) % var % iarray3) ) 
     &       Deallocate(Agrif_Mygrid % tabvars(nb) % var % iarray3)
          if ( associated(Agrif_Mygrid % tabvars(nb) % var % iarray4) ) 
     &       Deallocate(Agrif_Mygrid % tabvars(nb) % var % iarray4)
          if ( associated(Agrif_Mygrid % tabvars(nb) % var % iarray5) ) 
     &       Deallocate(Agrif_Mygrid % tabvars(nb) % var % iarray5)
          if ( associated(Agrif_Mygrid % tabvars(nb) % var % iarray6) ) 
     &       Deallocate(Agrif_Mygrid % tabvars(nb) % var % iarray6)
C
          if ( associated(Agrif_Mygrid % tabvars(nb) % var % larray1) ) 
     &       Deallocate(Agrif_Mygrid % tabvars(nb) % var % larray1)
          if ( associated(Agrif_Mygrid % tabvars(nb) % var % larray2) ) 
     &       Deallocate(Agrif_Mygrid % tabvars(nb) % var % larray2)
          if ( associated(Agrif_Mygrid % tabvars(nb) % var % larray3) ) 
     &       Deallocate(Agrif_Mygrid % tabvars(nb) % var % larray3)
          if ( associated(Agrif_Mygrid % tabvars(nb) % var % larray4) ) 
     &       Deallocate(Agrif_Mygrid % tabvars(nb) % var % larray4)
          if ( associated(Agrif_Mygrid % tabvars(nb) % var % larray5) ) 
     &       Deallocate(Agrif_Mygrid % tabvars(nb) % var % larray5)
          if ( associated(Agrif_Mygrid % tabvars(nb) % var % larray6) ) 
     &       Deallocate(Agrif_Mygrid % tabvars(nb) % var % larray6)
C
          if ( associated(Agrif_Mygrid % tabvars(nb) % var % carray1) ) 
     &       Deallocate(Agrif_Mygrid % tabvars(nb) % var % carray1)
          if ( associated(Agrif_Mygrid % tabvars(nb) % var % carray2) ) 
     &       Deallocate(Agrif_Mygrid % tabvars(nb) % var % carray2)
      enddo
C
      do nb = 1, Agrif_NbVariables
        Deallocate(Agrif_Mygrid % tabvars(nb) % var)
      enddo
C
      Deallocate(Agrif_Mygrid % tabvars)
C
      Deallocate(Agrif_Mygrid)
C
      End Subroutine  Agrif_Deallocation 
C
      End module Agrif_Util 
