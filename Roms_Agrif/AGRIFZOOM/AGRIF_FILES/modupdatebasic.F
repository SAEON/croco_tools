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
      
      integer,dimension(:,:),allocatable :: indchildcopy
      integer,dimension(:,:),allocatable :: indchildaverage
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
      Subroutine agrif_copy1d(x,y,np,nc,
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
!CDIR ALTCODE
          x(1:np) = y(locind_child_left:locind_child_left+np-1)
C
          return
C
      endif
C
      
      locind_child_left = 1 + nint((s_parent - s_child)/ds_child)
      
!CDIR ALTCODE
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
      End Subroutine agrif_copy1d

C     **************************************************************************  
CCC   Subroutine Copy1dprecompute  
C     ************************************************************************** 
C
      Subroutine copy1dprecompute2d(nc2,np,nc,
     &                  s_parent,s_child,ds_parent,ds_child,dir)
C
CCC   Description:
CCC   Subroutine to precompute index for a copy on a parent grid (vector x) from
CCC   its child grid (vector y).
C
CC    Method:
C
C     Declarations:
C

C        
C     Arguments
      INTEGER             :: nc2,np,nc
      INTEGER             :: dir   
      REAL                :: s_parent,s_child
      REAL                :: ds_parent,ds_child
C
C     Local variables
      INTEGER,DIMENSION(:,:),ALLOCATABLE :: indchildcopy_tmp
      INTEGER :: i,locind_child_left,coeffraf
C 
C
      coeffraf = nint(ds_parent/ds_child)
C
      locind_child_left = 1 + nint((s_parent - s_child)/ds_child)

      if (.not.allocated(indchildcopy)) then
      allocate(indchildcopy(np*nc2,3))
      else
      if (size(indchildcopy,1)<np*nc2) then
      allocate( indchildcopy_tmp(
     &         size(indchildcopy,1),size(indchildcopy,2)))
      indchildcopy_tmp = indchildcopy
      deallocate(indchildcopy)
      allocate(indchildcopy(np*nc2,3))
      indchildcopy(1:size(indchildcopy_tmp,1),
     &              1:size(indchildcopy_tmp,2)) = indchildcopy_tmp
      deallocate(indchildcopy_tmp)
      endif
      endif


      do i = 1,np
C      
         indchildcopy(i,dir) = locind_child_left
         locind_child_left = locind_child_left + coeffraf
C         
      enddo

      do i =2, nc2
        indchildcopy(1+(i-1)*np:i*np,dir)=
     &       indchildcopy(1+(i-2)*np:(i-1)*np,dir) + nc 
      enddo

C
      Return
C
C
      End Subroutine copy1dprecompute2d

C
C
C     **************************************************************************  
CCC   Subroutine Copy1dprecompute  
C     ************************************************************************** 
C
      Subroutine copy1dprecompute(np,nc,
     &                  s_parent,s_child,ds_parent,ds_child,dir)
C
CCC   Description:
CCC   Subroutine to precompute index for a copy on a parent grid (vector x) from
CCC   its child grid (vector y).
C
CC    Method:
C
C     Declarations:
C

C        
C     Arguments
      INTEGER             :: np,nc
      INTEGER             :: dir
      REAL                :: s_parent,s_child
      REAL                :: ds_parent,ds_child
C
C     Local variables
      INTEGER :: i,locind_child_left,coeffraf
C 
C
      coeffraf = nint(ds_parent/ds_child)
C
      locind_child_left = 1 + nint((s_parent - s_child)/ds_child)

      if (.not.allocated(indchildcopy)) then
      allocate(indchildcopy(np,1))
      else
      if (size(indchildcopy)<np) then
      deallocate(indchildcopy)
      allocate(indchildcopy(np,1))
      endif
      endif

!CDIR ALTCODE
      do i = 1,np
C      
         indchildcopy(i,1) = locind_child_left
         locind_child_left = locind_child_left + coeffraf
C         
      enddo

C
      Return
C
C
      End Subroutine copy1dprecompute
C
C
C     **************************************************************************  
CCC   Subroutine Copy1daftercompute  
C     ************************************************************************** 
C
      Subroutine copy1daftercompute(x,y,np,nc,
     &                  s_parent,s_child,ds_parent,ds_child,dir)
C
CCC   Description:
CCC   Subroutine to do a copy on a parent grid (vector x) from its child grid 
CCC   (vector y) using precomputed index.  
C
CC    Method:
C
C     Declarations:
C
  
C        
C     Arguments
      INTEGER             :: np,nc
      INTEGER             :: dir
      REAL, DIMENSION(np) :: x
      REAL, DIMENSION(nc) :: y
      REAL                :: s_parent,s_child
      REAL                :: ds_parent,ds_child
C
C     Local variables
      INTEGER :: i
C 
C
      
!CDIR ALTCODE
      do i = 1,np
C      
         x(i) = y(indchildcopy(i,dir))
C
      enddo
  
C
      Return
C
C
      End Subroutine copy1daftercompute


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
      REAL    :: xpos, invcoeffraf 
      INTEGER :: nbnonnuls
      INTEGER :: diffmod
C 
C
      coeffraf = nint(ds_parent/ds_child)
      invcoeffraf = 1./coeffraf
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

        locind_child_left = 1 + agrif_int((xpos - s_child)/ds_child)
C
      IF (Agrif_UseSpecialValueInUpdate) THEN

      do i = 1,np
          nbnonnuls = 0
!CDIR NOVECTOR
          Do ii = -coeffraf/2+locind_child_left+diffmod,
     &                coeffraf/2+locind_child_left
C 
               IF (y(ii) .NE. Agrif_SpecialValueFineGrid) THEN
               nbnonnuls = nbnonnuls + 1
               x(i) = x(i) + y(ii)
               ENDIF
          End Do
               IF (nbnonnuls .NE. 0) THEN
                  x(i) = x(i)/nbnonnuls
               ELSE
                  x(i) = Agrif_SpecialValueFineGrid
               ENDIF
        locind_child_left = locind_child_left + coeffraf
      enddo

      ELSE 

!CDIR ALTCODE
      do i = 1,np
!CDIR NOVECTOR
          Do ii = -coeffraf/2+locind_child_left+diffmod,
     &                coeffraf/2+locind_child_left
C 
               x(i) = x(i) + y(ii)
          End Do
                 x(i) = x(i)*invcoeffraf
        locind_child_left = locind_child_left + coeffraf
      enddo

      ENDIF

      Return 
C            
C     
      End Subroutine average1d
      
      Subroutine average1dprecompute2d(nc2,np,nc,
     &                     s_parent,s_child,ds_parent,ds_child,dir)
C
CCC   Description:
CCC   Subroutine to do an update by average on a parent grid (vector x)from its 
CCC   child grid (vector y).
C
C     Arguments
      INTEGER             :: nc2,np,nc,dir
      REAL                :: s_parent,s_child
      REAL                :: ds_parent,ds_child
C
C     Local variables
      INTEGER :: i,locind_child_left,coeffraf,ii
      INTEGER,DIMENSION(:,:),ALLOCATABLE :: indchildaverage_tmp
      REAL    :: xpos, invcoeffraf
      INTEGER :: nbnonnuls
      INTEGER :: diffmod
C 
C
      coeffraf = nint(ds_parent/ds_child)
C
      xpos = s_parent
C
      diffmod = 0

      IF ( mod(coeffraf,2) == 0 ) diffmod = 1

      locind_child_left = 1 + agrif_int((xpos - s_child)/ds_child)

      if (.not.allocated(indchildaverage)) then
      allocate(indchildaverage(np*nc2,3))
      else
      if (size(indchildaverage,1)<np*nc2) then
      allocate( indchildaverage_tmp(
     &         size(indchildaverage,1),size(indchildaverage,2)))
      indchildaverage_tmp = indchildaverage
      deallocate(indchildaverage)
      allocate(indchildaverage(np*nc2,3))
      indchildaverage(1:size(indchildaverage_tmp,1),
     &              1:size(indchildaverage_tmp,2)) = indchildaverage_tmp
      deallocate(indchildaverage_tmp)
      endif
      endif

      do i = 1,np
        indchildaverage(i,dir)= -coeffraf/2+locind_child_left
     &                                    +diffmod
        locind_child_left = locind_child_left + coeffraf
      enddo
      do i = 2,nc2
        indchildaverage(1+(i-1)*np:i*np,dir)= 
     &                 indchildaverage(1+(i-2)*np:(i-1)*np,dir) + nc 
      enddo

      Return 
C            
C     
      End Subroutine average1dprecompute2d
      

      Subroutine average1dprecompute(np,nc,
     &                     s_parent,s_child,ds_parent,ds_child) 
C
CCC   Description:
CCC   Subroutine to do an update by average on a parent grid (vector x)from its 
CCC   child grid (vector y).
C
C     Arguments
      INTEGER             :: np,nc 
      REAL                :: s_parent,s_child
      REAL                :: ds_parent,ds_child
C
C     Local variables
      INTEGER :: i,locind_child_left,coeffraf,ii
      REAL    :: xpos, invcoeffraf 
      INTEGER :: nbnonnuls
      INTEGER :: diffmod
C 
C
      coeffraf = nint(ds_parent/ds_child)

C
      if (coeffraf == 1) then
C
          return
C
      endif
            
C
      xpos = s_parent
C
      diffmod = 0
      
      IF ( mod(coeffraf,2) == 0 ) diffmod = 1

        locind_child_left = 1 + agrif_int((xpos - s_child)/ds_child)

      if (.not.allocated(indchildaverage)) then
      allocate(indchildaverage(np,1))
      else
      if (size(indchildaverage,1)<np) then
      deallocate(indchildaverage)
      allocate(indchildaverage(np,1))
      endif
      endif
      
!CDIR ALTCODE
      do i = 1,np
C 
        indchildaverage(i,1)=-coeffraf/2+locind_child_left+diffmod
        locind_child_left = locind_child_left + coeffraf
      enddo

      Return 
C            
C     
      End Subroutine average1dprecompute
      
      Subroutine average1daftercompute(x,y,np,nc,
     &                     s_parent,s_child,ds_parent,ds_child,dir) 
C
CCC   Description:
CCC   Subroutine to do an update by average on a parent grid (vector x)from its 
CCC   child grid (vector y).
C
C     Arguments
      INTEGER             :: np,nc,dir      
      REAL, DIMENSION(np) :: x      
      REAL, DIMENSION(nc) :: y  
      REAL                :: s_parent,s_child
      REAL                :: ds_parent,ds_child
C
C     Local variables
      INTEGER :: i,locind_child_left,coeffraf,ii,j
      REAL    :: xpos, invcoeffraf 
      REAL, PARAMETER :: one_third=1./3.
      INTEGER, DIMENSION(np) :: nbnonnuls
      INTEGER :: diffmod
      REAL, DIMENSION(0:5) :: invcoeff=(/1.,1.,0.5,one_third,0.25,0.2/)
C 
C
      coeffraf = nint(ds_parent/ds_child)
      invcoeffraf = 1./coeffraf      
C      
C
      IF (Agrif_UseSpecialValueInUpdate) THEN

      nbnonnuls = 0
      do  j =1, coeffraf
          do i=1, np
               IF (y(indchildaverage(i,dir) + j -1) .NE. 
     &               Agrif_SpecialValueFineGrid) THEN
               nbnonnuls(i) = nbnonnuls(i) + 1
               x(i) = x(i) +  y(indchildaverage(i,dir) + j -1 )
               ENDIF
          End Do
      enddo
      do i=1,np
      x(i)= x(i)*invcoeff(nbnonnuls(i)) 
      enddo

      ELSE
!CDIR NOLOOPCHG
      do  j =1, coeffraf
!CDIR VECTOR
          do i=1, np
             x(i) = x(i) + y(indchildaverage(i,dir) + j -1 )
          enddo
      enddo
       x = x * invcoeffraf
      ENDIF

      Return 
C            
C     
      End Subroutine average1daftercompute            
C
C
C
C     **************************************************************************  
CCC   Subroutine Full_weighting1d  
C     **************************************************************************  
C
      Subroutine full_weighting1D(x,y,np,nc,
     &                            s_parent,s_child,ds_parent,ds_child,
     &                            coeffraf,locind_child_left) 
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
      REAL    :: xpos,sumweight,weight
      INTEGER :: ii,diffmod
      REAL :: xposfin
      INTEGER :: it1,it2
      INTEGER :: i1,i2
      REAL :: invsumweight
      REAL :: weights(-(coeffraf):coeffraf)
      
C
C
      if (coeffraf == 1) then
C        
          x(1:np) = y(locind_child_left:locind_child_left+np-1)
C
          return
C
      endif
C
      xpos = s_parent      
      
       x = 0.

       xposfin = s_child + ds_child * (locind_child_left - 1)
       IF (abs(xposfin - xpos).LT.0.001) THEN
          diffmod = 0
       ELSE
          diffmod = 1
       ENDIF
C
       
      if (diffmod == 1) THEN
        invsumweight=1./(2.*coeffraf**2)
        do i=-coeffraf,-1
          weights(i) = invsumweight*(2*(coeffraf+i)+1)
        enddo
        do i=0,coeffraf-1
          weights(i)=weights(-(i+1))
        enddo
        it1 = -coeffraf
        i1 = -(coeffraf-1)+locind_child_left
        i2 = 2*coeffraf - 1
      else
        invsumweight=1./coeffraf**2
        do i=-(coeffraf-1),0
          weights(i) = invsumweight*(coeffraf + i)
        enddo
        do i=1,coeffraf-1
          weights(i) = invsumweight*(coeffraf - i)
        enddo
        it1 = -(coeffraf-1)
        i1 = -(coeffraf-1)+locind_child_left
        i2 = 2*coeffraf - 2
      endif

      sumweight = 0                    
      do i = 1,np
C
          it2 = it1
          Do ii = i1,i1+i2
C
           IF (Agrif_UseSpecialValueInUpdate) THEN
            IF (y(ii) .NE. Agrif_SpecialValueFineGrid) THEN
               x(i) = x(i) + weights(it2)*y(ii)
               sumweight = sumweight+weights(it2)
            ENDIF
           ELSE           
               x(i) = x(i) + weights(it2)*y(ii)
           ENDIF           
           
          it2 = it2+1
          End Do

           IF (Agrif_UseSpecialValueInUpdate) THEN          
                 IF (sumweight .NE. 0.) THEN
                    x(i) = x(i)/sumweight
                    sumweight = 0
                 ELSE
                    x(i) = Agrif_SpecialValueFineGrid
                 ENDIF
           ENDIF
        
        i1 = i1 + coeffraf
C
      enddo    
C

      Return 
C            
C
      End Subroutine full_weighting1D 

C     **************************************************************************  
CCC   Subroutine Fourthorder1d  
C     **************************************************************************  
C
      Subroutine Fourthorder1d(x,y,np,nc,
     &                            s_parent,s_child,ds_parent,ds_child,
     &                            coeffraf,locind_child_left) 
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
      REAL    :: xpos,sumweight,weight
      INTEGER :: ii,diffmod
      REAL :: xposfin
      INTEGER :: it1,it2
      INTEGER :: i1,i2
      REAL :: invsumweight
!      REAL :: weights(-(coeffraf-1):coeffraf-1)
      REAL weights(-5:5)
      
C
C
      if (coeffraf == 1) then
C        
          x(1:np) = y(locind_child_left:locind_child_left+np-1)
C
          return
C
      endif
C
      xpos = s_parent      
      
       x = 0.

       xposfin = s_child + ds_child * (locind_child_left - 1)
       IF (abs(xposfin - xpos).LT.0.001) THEN
          diffmod = 0
       ELSE
          diffmod = 1
       ENDIF
C
             
       it1 = -(coeffraf-1)
       i1 = -(coeffraf-1)+locind_child_left+diffmod
       i2 = 2*coeffraf - 2
       
      invsumweight=1./coeffraf**2
      do i=-(coeffraf-1),0
        weights(i) = invsumweight*(coeffraf + i)
      enddo
      do i=1,coeffraf-1
        weights(i) = invsumweight*(coeffraf - i)
      enddo
      
      weights(-5) = -4./243.
      weights(-4) = -5./243.
      weights(-3) = 0.
      weights(-2) = 10./81.
      weights(-1) = 20./81.
      weights(0) = 1./3.
      weights(5) = -4./243.
      weights(4) = -5./243.
      weights(3) = 0.
      weights(2) = 10./81.
      weights(1) = 20./81.   
      
!      weights=0.
!      weights(-2)=1./9.
!      weights(-1)=2./9.
!      weights(0)=3./9.
!      weights(1)=2./9.
!      weights(2)=1./9.
      
      i2 = 10
      i1 = locind_child_left-5
      it1 = -5  
      
      sumweight = 0                    
      do i = 1,np
C
          it2 = it1
          Do ii = i1,i1+i2
C
           IF (Agrif_UseSpecialValueInUpdate) THEN
            IF (y(ii) .NE. Agrif_SpecialValueFineGrid) THEN
               x(i) = x(i) + weights(it2)*y(ii)
               sumweight = sumweight+weights(it2)
            ENDIF
           ELSE           
               x(i) = x(i) + weights(it2)*y(ii)
           ENDIF           
           
          it2 = it2+1
          End Do

           IF (Agrif_UseSpecialValueInUpdate) THEN          
                 IF (sumweight .NE. 0.) THEN
                    x(i) = x(i)/sumweight
                    sumweight = 0
                 ELSE
                    x(i) = Agrif_SpecialValueFineGrid
                 ENDIF
           ENDIF
        
        i1 = i1 + coeffraf
C
      enddo    
C
      Return 
C            
C
      End Subroutine Fourthorder1d 
      
C
      End module AGRIF_updatebasic
