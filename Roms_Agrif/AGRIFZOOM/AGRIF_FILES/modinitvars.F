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
C     Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
C
C
C
CCC   Module Agrif_Init_Vars
C
      Module Agrif_Init_Vars
C
CCC   Description:
CCC   Initialization of the variables of the current grid.
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
C 
C     **************************************************************************
CCC   Subroutine Agrif_Create_var 
C     **************************************************************************
C  
      Subroutine Agrif_Create_Var(Agrif_Gr)
C
CCC   Description:
CCC   Allocation of the field "var" of the grid variables.
CCC   Fields "root_var", "nbdim" and "parent_var" of the grid variables.
CCC   Array "tabvars" of the current grid. 
C
CC    Method:                  
C 
C     Declarations:
C
      
C
C     Pointer argument:
      TYPE(Agrif_Grid),pointer   :: Agrif_Gr  ! Pointer on the current grid
      INTEGER :: nb
C
C     Array "tabvars" of the current grid. 
C
      Allocate(Agrif_Gr % tabvars(Agrif_NbVariables))
C      
      do nb = 1, Agrif_NbVariables
        Allocate(Agrif_Gr % tabvars(nb) % var)
      enddo

C     Fields "root_var", "nbdim" and "parent_var" of the grid variables.     
      if (Agrif_Gr % fixedrank .NE.0) then
         do nb = 1, Agrif_NbVariables     
            Agrif_Gr % tabvars(nb) % parent_var => 
     &      Agrif_Gr % parent %tabvars(nb)
            Agrif_Gr % tabvars(nb) % var % nbdim =
     &      Agrif_Mygrid % tabvars(nb) % var % nbdim
            Agrif_Gr % tabvars(nb) % var % root_var =>
     &      Agrif_Mygrid % tabvars(nb) % var          
         enddo
      endif
C           
      End Subroutine Agrif_Create_Var
C
C
C     **************************************************************************
CCC   Subroutine Agrif_CreateVar
C     **************************************************************************
C 
      Subroutine Agrif_CreateVar(Agrif_Gr)            
C
CCC   Description:
CCC   This subroutine allows to make a link between Mygrid and the current grid 
C
C     Modules used:
C      
C
C     Declarations:
C      
C
C
C     Arguments      
C
      TYPE(Agrif_Grid),pointer   :: Agrif_Gr    ! Pointer on the current grid
      INTEGER                    :: i           ! Loop
C
      if (Agrif_Gr % fixedrank .NE. 0) then
C  
         do i = 1 , Agrif_NbVariables 
            Agrif_Gr % tabvars(i) % parent_var => 
     &      Agrif_Gr % parent % tabvars(i) 
            Agrif_Gr % tabvars(i) % var % root_var => 
     &      Agrif_Mygrid % tabvars(i) % var
            Agrif_Gr % tabvars(i) % var % nbdim  = 
     &      Agrif_Gr % parent % tabvars(i) % var % nbdim
         enddo 
C	 
      endif 
C       
      End Subroutine Agrif_CreateVar
C
C
C
      End Module Agrif_Init_Vars
