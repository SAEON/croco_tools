C     AGRIF (Adaptive Grid Refinement In Fortran)
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
CCC   Module Agrif_Init
C
      Module Agrif_Init 
C
CCC   Description:
CCC   Several operations on the variables of the current grid (creation,
CCC   instanciation, ...) used during the creation of the grid hierarchy and
CCC   during the time integration.
C
C     Modules used: 
C
      Use Agrif_Types 
      Use Agrif_link
C   
      IMPLICIT NONE
C
      Contains
C     Defininition of the procedures contained in this module
C
C     **************************************************************************
CCC   Subroutine Agrif_Allocation  
C     **************************************************************************
C  
      Subroutine Agrif_Allocation(Agrif_Gr)
C
CCC   Description:
CCC   Subroutine to allocate the arrays containing the values of the variables 
CCC   of the current grd.
C
CC    Method:
CC    Use of the allocate function.  
C
C     Declarations:
C      
C     Pointer argument:
      TYPE(AGRIF_grid), Pointer :: Agrif_Gr  ! Pointer on the current grid
C 
C     Allocation of the arrays of the variables  
C     We cut this in several files to avoid long compilation timings
C
      Call Agrif_Allocationcalls(Agrif_Gr)
C
      if ( Agrif_USE_ONLY_FIXED_GRIDS .EQ. 0 ) then
C
         if ( Agrif_Probdim .EQ. 1 ) 
     &      Allocate(Agrif_Gr%tabpoint1D(Agrif_Gr%nb(1)+1))
         if ( Agrif_Probdim .EQ. 2 ) 
     &      Allocate(Agrif_Gr%tabpoint2D(Agrif_Gr%nb(1)+1,
     &       Agrif_Gr%nb(2)+1))      
         if ( Agrif_Probdim .EQ. 3 ) 
     &      Allocate(Agrif_Gr%tabpoint3D(Agrif_Gr%nb(1)+1,
     &       Agrif_Gr%nb(2)+1,Agrif_Gr%nb(3)+1))      
C
      endif
C 
      End Subroutine Agrif_Allocation
C
C     **************************************************************************
CCC   Subroutine Agrif_Instance 
C     **************************************************************************
C 
      Subroutine Agrif_Instance(Agrif_Gr)
C
CCC   Description:
CCC   Subroutine to do an instance of the common variables to the variables of  
CCC   the current grid.
C
CC    Method:
CC    Pointing the common variables on these of the current grid.  
C
C     Declarations:
C        

C
C

C
C     Pointer argument: 
      Type(Agrif_Grid), Pointer :: Agrif_Gr   ! Pointer on the current grid  
C 
C
      Agrif_Curgrid => Agrif_Gr
      Agrif_tabvars => Agrif_Curgrid % tabvars
C
      Call Agrif_Get_numberofcells(Agrif_Gr)
C
C
C     Calculation of isf,jsf,nzsf and of the index of the output file 
      Call Agrif_InitWorkSpace() 
C
C 
      End Subroutine Agrif_Instance
C
C
C     **************************************************************************
CCC   Subroutine Agrif_initialisations  
C     **************************************************************************
      Subroutine Agrif_initialisations(Agrif_Gr)
C
CCC   Description:
C
CC    Method:
C
C     Declarations:
C        
      INTEGER :: i
C
C     Pointer argument: 
      Type(Agrif_Grid), Pointer  :: Agrif_Gr
C
      do i = 1 , Agrif_NbVariables
C 
         Agrif_Gr % tabvars(i) % var % nbdim = 0
C
         if (associated(Agrif_Gr%tabvars(i)%var%array1)) then
             Agrif_Gr % tabvars(i) % var % nbdim = 1
         endif
         if (associated(Agrif_Gr%tabvars(i)%var%array2)) then
             Agrif_Gr % tabvars(i) % var % nbdim = 2
         endif
         if (associated(Agrif_Gr%tabvars(i)%var%array3)) then
             Agrif_Gr % tabvars(i) % var % nbdim = 3
         endif
         if (associated(Agrif_Gr%tabvars(i)%var%array4)) then
             Agrif_Gr % tabvars(i) % var % nbdim = 4
         endif
         if (associated(Agrif_Gr%tabvars(i)%var%array5)) then
             Agrif_Gr % tabvars(i) % var % nbdim = 5
         endif
         if (associated(Agrif_Gr%tabvars(i)%var%array6)) then
             Agrif_Gr % tabvars(i) % var % nbdim = 6
         endif
C
         if (associated(Agrif_Gr%tabvars(i)%var%darray1)) then
             Agrif_Gr % tabvars(i) % var % nbdim = 1
         endif
         if (associated(Agrif_Gr%tabvars(i)%var%darray2)) then
             Agrif_Gr % tabvars(i) % var % nbdim = 2
         endif
         if (associated(Agrif_Gr%tabvars(i)%var%darray3)) then
             Agrif_Gr % tabvars(i) % var % nbdim = 3
         endif
         if (associated(Agrif_Gr%tabvars(i)%var%darray4)) then
             Agrif_Gr % tabvars(i) % var % nbdim = 4
         endif
         if (associated(Agrif_Gr%tabvars(i)%var%darray5)) then
             Agrif_Gr % tabvars(i) % var % nbdim = 5
         endif
         if (associated(Agrif_Gr%tabvars(i)%var%darray6)) then
             Agrif_Gr % tabvars(i) % var % nbdim = 6
         endif
C
         if (associated(Agrif_Gr%tabvars(i)%var%larray1)) then
             Agrif_Gr % tabvars(i) % var % nbdim = 1
         endif
         if (associated(Agrif_Gr%tabvars(i)%var%larray2)) then
             Agrif_Gr % tabvars(i) % var % nbdim = 2
         endif
         if (associated(Agrif_Gr%tabvars(i)%var%larray3)) then
             Agrif_Gr % tabvars(i) % var % nbdim = 3
         endif
         if (associated(Agrif_Gr%tabvars(i)%var%larray4)) then
             Agrif_Gr % tabvars(i) % var % nbdim = 4
         endif
         if (associated(Agrif_Gr%tabvars(i)%var%larray5)) then
             Agrif_Gr % tabvars(i) % var % nbdim = 5
         endif
         if (associated(Agrif_Gr%tabvars(i)%var%larray6)) then
             Agrif_Gr % tabvars(i) % var % nbdim = 6
         endif
C
         if (associated(Agrif_Gr%tabvars(i)%var%iarray1)) then
             Agrif_Gr % tabvars(i) % var % nbdim = 1
         endif
         if (associated(Agrif_Gr%tabvars(i)%var%iarray2)) then
             Agrif_Gr % tabvars(i) % var % nbdim = 2
         endif
         if (associated(Agrif_Gr%tabvars(i)%var%iarray3)) then
             Agrif_Gr % tabvars(i) % var % nbdim = 3
         endif
         if (associated(Agrif_Gr%tabvars(i)%var%iarray4)) then
             Agrif_Gr % tabvars(i) % var % nbdim = 4
         endif
         if (associated(Agrif_Gr%tabvars(i)%var%iarray5)) then
             Agrif_Gr % tabvars(i) % var % nbdim = 5
         endif
         if (associated(Agrif_Gr%tabvars(i)%var%iarray6)) then
             Agrif_Gr % tabvars(i) % var % nbdim = 6
         endif
C
         if (associated(Agrif_Gr%tabvars(i)%var%carray1)) then
             Agrif_Gr % tabvars(i) % var % nbdim = 1
         endif
         if (associated(Agrif_Gr%tabvars(i)%var%carray2)) then
             Agrif_Gr % tabvars(i) % var % nbdim = 2
         endif
C 
      enddo
C
      Return
C 
C
      End Subroutine Agrif_initialisations
C 
C
      End Module AGRIF_Init 
