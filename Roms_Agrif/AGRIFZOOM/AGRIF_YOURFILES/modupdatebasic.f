

!
! $Id$
!
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
C     Foundation, Inc., 59 Temple Place -  Suite 330, Boston, MA 02111-1307, USA.
C
C
C
CCC   Module Agrif_Updatebasic
C      
C
      Module Agrif_Updatebasic
C
CCC   Description:
CCC   Module containing different procedures of update (copy,average,
CCC   full_weighting) used in the Agrif_Update module.
C
C     Modules used:
C
      USE Agrif_types
      
      IMPLICIT NONE
C             

      CONTAINS
C     Define procedures contained in this module
C
C
C
C     **************************************************************************  
CCC   Subroutine Copy1d  
C     ************************************************************************** 
C
      Subroutine copy1d(x,y,np,nc,
     &                  s_parent,s_child,ds_parent,ds_child) 
C
CCC   Description:
CCC   Subroutine to do a copy on a parent grid (vector x) from its child grid 
CCC   (vector y).  
C
CC    Method:
C
C     Declarations:
C
      
C        
C     Arguments
      INTEGER             :: np,nc      
      REAL, DIMENSION(np) :: x      
      REAL, DIMENSION(nc) :: y  
      REAL                :: s_parent,s_child
      REAL                :: ds_parent,ds_child
C
C     Local variables
      INTEGER :: i,locind_child_left,coeffraf
C 
C
      coeffraf = nint(ds_parent/ds_child)
C
      if (coeffraf == 1) then
C
          locind_child_left = 1 + nint((s_parent - s_child)/ds_child)
C        
          x(1:np) = y(locind_child_left:locind_child_left+np-1)
C
          return
C
      endif
C
      
      locind_child_left = 1 + nint((s_parent - s_child)/ds_child)
      
      do i = 1,np
C      
         x(i) = y(locind_child_left)
C
         locind_child_left = locind_child_left + coeffraf
C         
      enddo    
       
C
      Return 
C
C
      End Subroutine copy1d
C
C
C
C     **************************************************************************  
CCC   Subroutine Average1d  
C     ************************************************************************** 
C   
      Subroutine average1d(x,y,np,nc,
     &                     s_parent,s_child,ds_parent,ds_child) 
C
CCC   Description:
CCC   Subroutine to do an update by average on a parent grid (vector x)from its 
CCC   child grid (vector y).
C
C     Arguments
      INTEGER             :: np,nc      
      REAL, DIMENSION(np) :: x      
      REAL, DIMENSION(nc) :: y  
      REAL                :: s_parent,s_child
      REAL                :: ds_parent,ds_child
C
C     Local variables
      INTEGER :: i,locind_child_left,coeffraf,ii
      REAL    :: xpos 
      INTEGER :: nbnonnuls
      INTEGER :: diffmod
C 
C
      coeffraf = nint(ds_parent/ds_child)
C
      if (coeffraf == 1) then
C
          locind_child_left = 1 + nint((s_parent - s_child)/ds_child)
C        
          x(1:np) = y(locind_child_left:locind_child_left+np-1)
C
          return
C
      endif
C
      xpos = s_parent      
      
      x = 0.
C
      diffmod = 0
      
      IF ( mod(coeffraf,2) == 0 ) diffmod = 1
      
      do i = 1,np
C
        locind_child_left = 1 + agrif_int((xpos - s_child)/ds_child)
C
        if ((locind_child_left-1 < 1) 
     &      .OR. (locind_child_left+1 > nc)) then 
C
            x(i) = y(locind_child_left)                
C
          else 
          nbnonnuls = 0
          Do ii = -coeffraf/2+locind_child_left+diffmod,
     &                coeffraf/2+locind_child_left
C 
            IF (Agrif_UseSpecialValueInUpdate) THEN
            IF (y(ii) .NE. Agrif_SpecialValueFineGrid) THEN
               nbnonnuls = nbnonnuls + 1
               x(i) = x(i) + y(ii)
            ENDIF
            ELSE
               x(i) = x(i) + y(ii)
            ENDIF
          End Do
            IF (Agrif_UseSpecialValueInUpdate) THEN
                 IF (nbnonnuls .NE. 0) THEN
                    x(i) = x(i)/nbnonnuls
                 ELSE
                    x(i) = Agrif_SpecialValueFineGrid
                 ENDIF
            ELSE
                 x(i) = x(i)/coeffraf
            ENDIF
C
        endif
C
        xpos = xpos + ds_parent
C
      enddo
C
      Return 
C            
C     
      End Subroutine average1d
C
C
C
C     **************************************************************************  
CCC   Subroutine Full_weighting1d  
C     **************************************************************************  
C
      Subroutine full_weighting1D(x,y,np,nc,
     &                            s_parent,s_child,ds_parent,ds_child) 
C
CCC   Description:
CCC   Subroutine to do an update by full_weighting on a parent grid (vector x) 
CCC   from its child grid (vector y).
C 
C     Arguments
      INTEGER             :: np,nc      
      REAL, DIMENSION(np) :: x      
      REAL, DIMENSION(nc) :: y  
      REAL                :: s_parent,s_child
      REAL                :: ds_parent,ds_child
C
C     Local variables
      INTEGER :: i,locind_child_left,coeffraf
      REAL    :: xpos 
C 
C
      coeffraf = nint(ds_parent/ds_child)
C
      if (coeffraf == 1) then
C
          locind_child_left = 1 + nint((s_parent - s_child)/ds_child)
C        
          x(1:np) = y(locind_child_left:locind_child_left+np-1)
C
          return
C
      endif
C
      IF (coeffraf .NE. 3) THEN
        print *,'FULL WEIGHTING NOT READY FOR COEFFRAF = 3'
	STOP
      ENDIF
      xpos = s_parent      
C
      do i = 1,np
C
        locind_child_left = 1 + nint((xpos - s_child)/ds_child)
C
        if ((locind_child_left-1 < 1) 
     &      .OR. (locind_child_left+1 > nc)) then 
C      Agrif_UseSpecialValueInUpdate = .TRUE.
            x(i) = y(locind_child_left)                
C
          else
C        
            x(i) = (y(locind_child_left-1)+2.*y(locind_child_left)+
     &              y(locind_child_left+1))/4.                
C
        endif
C
        xpos = xpos + ds_parent
C
      enddo
C
      Return 
C            
C
      End Subroutine full_weighting1D      
C
C
C
      End module AGRIF_updatebasic
