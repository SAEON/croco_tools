

!
! $Id: modinterpbasic.F,v 1.4 2005/08/22 15:11:29 agrif Exp $
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
C     Foundation, Inc., 59 Temple Place- Suite 330, Boston, MA 02111-1307, USA.
C
C
C
CCC   Module Agrif_Interpbasic
C
      Module Agrif_Interpbasic
C
CCC   Description:
CCC   Module containing different procedures of interpolation (linear,lagrange,
CCC   spline,...) used in the Agrif_Interpolation module.
C
C     Modules used:
      USE Agrif_types
C
      IMPLICIT NONE
C             
      CONTAINS
C     Define procedures contained in this module
C 
C
C     **************************************************************************  
CCC   Subroutine Linear1d  
C     ************************************************************************** 
C 
      Subroutine Linear1d(x,y,np,nc,
     &                    s_parent,s_child,ds_parent,ds_child) 
C
CCC   Description:
CCC   Subroutine to do a linear 1D interpolation on a child grid (vector y) from
CCC   its parent grid (vector x).  
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
      REAL                :: s_parent,s_child,ds_parent,ds_child
C
C     Local scalars
      INTEGER :: i,coeffraf,locind_parent_left
      REAL    :: ypos,globind_parent_left
C
C

      coeffraf = nint(ds_parent/ds_child)
C
      if (coeffraf == 1) then
C
          locind_parent_left = 1 + nint((s_child - s_parent)/ds_parent)
C        
          y(1:nc) = x(locind_parent_left:locind_parent_left+nc-1)
C
          return
C
      endif                          
C
      ypos = s_child      
C
      do i = 1,nc-1
C
        locind_parent_left = 1 + agrif_int((ypos - s_parent)/ds_parent)
C
        globind_parent_left = s_parent 
     &                        + (locind_parent_left - 1)*ds_parent
C        
        y(i) = ((globind_parent_left + ds_parent - ypos)
     &          *x(locind_parent_left)
     &        + (ypos - globind_parent_left)
     &          *x(locind_parent_left+1))
     &         / ds_parent                
C
        ypos = ypos + ds_child
C
      enddo
C
      locind_parent_left = 1 + agrif_int((ypos - s_parent)/ds_parent)
C
      if (locind_parent_left == np) then
C
          y(nc) = x(np)
C
        else
C
          globind_parent_left = s_parent 
     &                        + (locind_parent_left - 1)*ds_parent  
C      
          y(nc) = ((globind_parent_left + ds_parent - ypos)
     &            *x(locind_parent_left)
     &          + (ypos - globind_parent_left)
     &            *x(locind_parent_left+1))
     &           / ds_parent     
C
      endif                                          
C           
      Return
C
C       
      End Subroutine Linear1d   
C
C
C
C     **************************************************************************  
CCC   Subroutine Lagrange1d  
C     **************************************************************************
C
      Subroutine Lagrange1d(x,y,np,nc,
     &                      s_parent,s_child,ds_parent,ds_child)
C
CCC   Description:
CCC   Subroutine to do a lagrange 1D interpolation on a child grid (vector y) 
CCC   from its parent grid (vector x).  
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
      REAL                :: s_parent,s_child,ds_parent,ds_child 
C
C     Local scalars
      INTEGER :: i,coeffraf,locind_parent_left
      REAL    :: ypos,globind_parent_left
      REAL    :: X1,X2,X3 
C
C 
      if (np <= 2) then
C      
          Call Linear1D(x,y,np,nc,
     &                  s_parent,s_child,ds_parent,ds_child)
C         
         Return
C 
      endif
C
      coeffraf = nint(ds_parent/ds_child)
C
      if (coeffraf == 1) then
C
          locind_parent_left = 1 + nint((s_child - s_parent)/ds_parent)
C        
          y(1:nc) = x(locind_parent_left:locind_parent_left+nc-1)
C
          return
C
      endif
C
      ypos = s_child      
C
      do i = 1,nc
C
        locind_parent_left = 1 + agrif_int((ypos - s_parent)/ds_parent)
C
        globind_parent_left = s_parent 
     &                        + (locind_parent_left - 1)*ds_parent 
C
        if (locind_parent_left+2 <= np) then            
C
            X1 = (x(locind_parent_left+1)-x(locind_parent_left))
     &           /ds_parent
C 
            X2 = (x(locind_parent_left+2)-x(locind_parent_left+1))
     &           /ds_parent
C
            X3 = (X2 - X1)/(2.*ds_parent)                 
C
            y(i) = x(locind_parent_left) + 
     &             (ypos - globind_parent_left)*X1 +
     &             (ypos - globind_parent_left)*
     &             (ypos - globind_parent_left - ds_parent)*X3 
C
          elseif (locind_parent_left+1 <= np) then 
C
            X1 = (x(locind_parent_left)-x(locind_parent_left-1))
     &           /ds_parent
C     
            X2 = (x(locind_parent_left+1)-x(locind_parent_left))
     &           /ds_parent
C 
            X3 = (X2 - X1)/(2.*ds_parent)                 
C
            y(i) = x(locind_parent_left-1) + 
     &             (ypos - globind_parent_left - ds_parent)*X1 +
     &             (ypos - globind_parent_left - ds_parent)*
     &             (ypos - globind_parent_left)*X3
C
          else
C
            X1 = (x(locind_parent_left-1)-x(locind_parent_left-2))
     &           /ds_parent
C     
            X2 = (x(locind_parent_left)-x(locind_parent_left-1))
     &           /ds_parent
C 
            X3 = (X2 - X1)/(2.*ds_parent)                 
C
            y(i) = x(locind_parent_left-2) + 
     &             (ypos - globind_parent_left - 2.*ds_parent)*X1 +
     &             (ypos - globind_parent_left - 2.*ds_parent)*
     &             (ypos - globind_parent_left - ds_parent)*X3
C
        endif   
C       
        ypos = ypos + ds_child 
C
      enddo
C
      return
C
C       
      End Subroutine Lagrange1d 
C
C
C     **************************************************************************  
CCC   Subroutine Constant1d  
C     ************************************************************************** 
C 
      Subroutine constant1d(x,y,np,nc,
     &                    s_parent,s_child,ds_parent,ds_child) 
C
CCC   Description:
CCC   Subroutine to do a linear 1D interpolation on a child grid (vector y) from
CCC   its parent grid (vector x).  
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
      REAL                :: s_parent,s_child,ds_parent,ds_child
C
C     Local scalars
      INTEGER :: i,coeffraf,locind_parent
      REAL    :: ypos
C
C

      coeffraf = nint(ds_parent/ds_child)
C
      if (coeffraf == 1) then
C
          locind_parent = 1 + nint((s_child - s_parent)/ds_parent)
C        
          y(1:nc) = x(locind_parent:locind_parent+nc-1)
C
          return
C
      endif                          
C
      ypos = s_child      
C
      do i = 1,nc
C
        locind_parent = 1 + nint((ypos - s_parent)/ds_parent)
C
        y(i) = x(locind_parent)
C
        ypos = ypos + ds_child
C
      enddo
C           
      Return
C
C       
      End Subroutine constant1d   
C
C     **************************************************************************  
CCC   Subroutine Linear1dconserv
C     ************************************************************************** 
C 
      Subroutine Linear1dconserv(x,y,np,nc,
     &                    s_parent,s_child,ds_parent,ds_child) 
C
CCC   Description:
CCC   Subroutine to do a linear 1D interpolation on a child grid (vector y) from
CCC   its parent grid (vector x).  
C
CC    Method:
C
C     Declarations:
C
      Implicit none
C        
C     Arguments
      Integer             :: np,nc      
      Real, Dimension(np) :: x      
      Real, Dimension(nc) :: y
      Real, Dimension(:),Allocatable :: ytemp
      Real                :: s_parent,s_child,ds_parent,ds_child
C
C     Local scalars
      Integer :: i,coeffraf,locind_parent_left,locind_parent_last
      Real    :: ypos
      integer :: i1,i2,ii
      real :: xpmin,xpmax,slope
      INTEGER :: diffmod
      REAL :: xdiffmod

C
C

      coeffraf = nint(ds_parent/ds_child)
C
      If (coeffraf == 1) Then
C
          locind_parent_left = 1 + nint((s_child - s_parent)/ds_parent)
C        
          y(1:nc) = x(locind_parent_left:locind_parent_left+nc-1)
C
          return
C
      End If
C              
      diffmod = 0
      IF (mod(coeffraf,2) == 0) diffmod = 1  

      xdiffmod = real(diffmod)/2.
                         
      allocate(ytemp(-2*coeffraf:nc+2*coeffraf))
C
      ypos = s_child  
C      
      locind_parent_left = 1 + agrif_int((ypos - s_parent)/ds_parent) 

      locind_parent_last = 1 +
     &  agrif_ceiling((ypos +(nc - 1) *ds_child - s_parent)/ds_parent)
     
      xpmin = s_parent + (locind_parent_left-1)*ds_parent 
      xpmax = s_parent + (locind_parent_last-1)*ds_parent          
      
      i1 = 1+agrif_int((xpmin-s_child)/ds_child)
      i2 = 1+agrif_int((xpmax-s_child)/ds_child)

      i = i1
      
      if (locind_parent_left == 1) then
        slope=
     &   (x(locind_parent_left+1)-x(locind_parent_left))/(coeffraf)
      else
         slope=
     &   (x(locind_parent_left+1)-x(locind_parent_left-1))/(2.*coeffraf)
      endif
      
        do ii=i-coeffraf/2+diffmod,i+coeffraf/2
          ytemp(ii) = x(locind_parent_left)+(ii-i-xdiffmod/2.)*slope
        enddo 

        locind_parent_left = locind_parent_left + 1
                      
      do i=i1 +  coeffraf, i2 - coeffraf,coeffraf
        slope=
     &   (x(locind_parent_left+1)-x(locind_parent_left-1))/(2.*coeffraf)
        do ii=i-coeffraf/2+diffmod,i+coeffraf/2
          ytemp(ii) = x(locind_parent_left)+(ii-i-xdiffmod/2.)*slope
        enddo
        locind_parent_left = locind_parent_left + 1
      enddo
      
      i = i2
      
      if (locind_parent_left == np) then
        slope=
     &   (x(locind_parent_left)-x(locind_parent_left-1))/(coeffraf)
      else
         slope=
     &   (x(locind_parent_left+1)-x(locind_parent_left-1))/(2.*coeffraf)
      endif
      
        do ii=i-coeffraf/2+diffmod,nc
          ytemp(ii) = x(locind_parent_left)+(ii-i-xdiffmod/2.)*slope
        enddo      
C
      y(1:nc)=ytemp(1:nc)                                   
C           
      deallocate(ytemp)
      Return
C       
      End Subroutine Linear1dconserv
      
C
C     **************************************************************************  
CCC   Subroutine Linear1dconservlim
C     ************************************************************************** 
C 
      Subroutine Linear1dconservlim(x,y,np,nc,
     &                    s_parent,s_child,ds_parent,ds_child) 
C
CCC   Description:
CCC   Subroutine to do a linear 1D interpolation on a child grid (vector y) from
CCC   its parent grid (vector x).  
C
CC    Method:
C
C     Declarations:
C
      Implicit none
C        
C     Arguments
      Integer             :: np,nc      
      Real, Dimension(np) :: x      
      Real, Dimension(nc) :: y
      Real, Dimension(:),Allocatable :: ytemp
      Real                :: s_parent,s_child,ds_parent,ds_child
C
C     Local scalars
      Integer :: i,coeffraf,locind_parent_left,locind_parent_last
      Real    :: ypos
      integer :: i1,i2,ii
      real :: xpmin,xpmax,slope
      INTEGER :: diffmod
      real :: xdiffmod
C
C

      coeffraf = nint(ds_parent/ds_child)
C
      If (coeffraf == 1) Then
C
          locind_parent_left = 1 + nint((s_child - s_parent)/ds_parent)
C        
          y(1:nc) = x(locind_parent_left:locind_parent_left+nc-1)
C
          return
C
      End If
C
      IF (coeffraf .NE.3) THEN
      print *,'LINEARCONSERVLIM not ready for refinement ratio = ',
     &   coeffraf
      stop
      ENDIF   
      
      diffmod = 0
      IF (mod(coeffraf,2) == 0) diffmod = 1        

      xdiffmod = real(diffmod)/2.
                         
      allocate(ytemp(-2*coeffraf:nc+2*coeffraf))
C
      ypos = s_child  
C      
      locind_parent_left = 1 + agrif_int((ypos - s_parent)/ds_parent) 

      locind_parent_last = 1 +
     &  agrif_ceiling((ypos +(nc - 1) *ds_child - s_parent)/ds_parent)
     
      xpmin = s_parent + (locind_parent_left-1)*ds_parent 
      xpmax = s_parent + (locind_parent_last-1)*ds_parent          
      
      i1 = 1+agrif_int((xpmin-s_child)/ds_child)
      i2 = 1+agrif_int((xpmax-s_child)/ds_child)

      i = i1
      
      if (locind_parent_left == 1) then
        slope=0.       
      else
        slope = vanleer(x(locind_parent_left-1:locind_parent_left+1))
        slope = slope / coeffraf    
      endif
      
        do ii=i-coeffraf/2+diffmod,i+coeffraf/2
          ytemp(ii) = x(locind_parent_left)+(ii-i-xdiffmod/2.)*slope
        enddo 

        locind_parent_left = locind_parent_left + 1
                      
      do i=i1 +  coeffraf, i2 - coeffraf,coeffraf     
        slope = vanleer(x(locind_parent_left-1:locind_parent_left+1))
        slope = slope / coeffraf

        do ii=i-coeffraf/2+diffmod,i+coeffraf/2
          ytemp(ii) = x(locind_parent_left)+(ii-i-xdiffmod/2.)*slope
        enddo
        locind_parent_left = locind_parent_left + 1
      enddo
      
      i = i2
      
      if (locind_parent_left == np) then
        slope=0.     
      else
        slope = vanleer(x(locind_parent_left-1:locind_parent_left+1))
        slope = slope / coeffraf      
      endif
      
        do ii=i-coeffraf/2+diffmod,nc
          ytemp(ii) = x(locind_parent_left)+(ii-i-xdiffmod/2.)*slope
        enddo      
C
      y(1:nc)=ytemp(1:nc)                                   
C           
      deallocate(ytemp)
      Return
C       
      End Subroutine Linear1dconservlim      
C         

C     **************************************************************************  
CCC   Subroutine ppm1d
C     ************************************************************************** 
C 
      Subroutine ppm1d(x,y,np,nc,
     &                    s_parent,s_child,ds_parent,ds_child) 
C
CCC   Description:
CCC   Subroutine to do a 1D interpolation and apply monotonicity constraints
CCC   using piecewise parabolic method  
CCC   on a child grid (vector y) from its parent grid (vector x).
CC    Method:
C
C     Declarations:
C
      Implicit none
C        
C     Arguments
      Integer             :: np,nc      
      Real, Dimension(np) :: x      
      Real, Dimension(nc) :: y
      Real, Dimension(:),Allocatable :: ytemp
      Real                :: s_parent,s_child,ds_parent,ds_child
C
C     Local scalars
      Integer :: i,coeffraf,locind_parent_left,locind_parent_last
      Integer :: iparent,ipos,pos,nmin,nmax
      Real    :: ypos
      integer :: i1,jj
      Real :: xpmin,cavg,a,b
C      
      Real :: xrmin,xrmax,am3,s2,s1  
      Real, Dimension(np) :: dela,xr,xl,delta,a6,slope,slope2
      Real, Dimension(:),Allocatable  :: diff,diff2,diff3    
      INTEGER :: diffmod
C      
      coeffraf = nint(ds_parent/ds_child)
C
      If (coeffraf == 1) Then
          locind_parent_left = 1 + nint((s_child - s_parent)/ds_parent)
          y(1:nc) = x(locind_parent_left:locind_parent_left+nc-1)
          return
      End If
C      
      Allocate(ytemp(-2*coeffraf:nc+2*coeffraf))
      ypos = s_child  
C
      locind_parent_left = 1 + agrif_int((ypos - s_parent)/ds_parent) 
      locind_parent_last = 1 +
     &      agrif_ceiling((ypos +(nc - 1) 
     &      *ds_child - s_parent)/ds_parent)  
C
      xpmin = s_parent + (locind_parent_left-1)*ds_parent       
      i1 = 1+agrif_int((xpmin-s_child)/ds_child)        
C     
      Allocate( diff(coeffraf),diff2(coeffraf),diff3(coeffraf) )
C      
         diff(:) = ds_child/ds_parent
C      
      Do i=1,coeffraf
         a = real(i-1)*ds_child/ds_parent
         b = real(i)*ds_child/ds_parent
         diff2(i) = 0.5*(b*b - a*a)  
         diff3(i) = (1./3.)*(b*b*b - a*a*a)
      End do
C
      if( locind_parent_last+2 <= np ) then
           nmax = locind_parent_last+2    
      else if( locind_parent_last+1 <= np ) then
           nmax = locind_parent_last+1
      else
           nmax = locind_parent_last 
      endif     
C      
      if(locind_parent_left-1 >= 1) then
          nmin = locind_parent_left-1
      else 
          nmin = locind_parent_left
      endif    
C 
      Do i = nmin,nmax
         slope(i) = x(i) - x(i-1)
         slope2(i) = 2.*abs(slope(i))
      Enddo
C
      Do i = nmin,nmax-1
         dela(i) = 0.5 * ( slope(i) + slope(i+1) )
C Van Leer slope limiter
         dela(i) = min( abs(dela(i)),slope2(i),
     &                  slope2(i+1) )*sign(1.,dela(i))
         IF( slope(i)*slope(i+1) <= 0. ) dela(i) = 0.
      Enddo
C
      Do i = nmin,nmax-2
         xr(i) = x(i) + (1./2.)*slope(i+1) + (-1./6.)*dela(i+1)
     &                                     + ( 1./6. )*dela(i)
      Enddo
C
      Do i = nmin,nmax-2
         xrmin = min(x(i),x(i+1))
         xrmax = max(x(i),x(i+1))
         xr(i) = min(xr(i),xrmax)
         xr(i) = max(xr(i),xrmin)
         xl(i+1) = xr(i)         
      Enddo
C apply parabolic monotonicity
       Do i = locind_parent_left,locind_parent_last
          If( ( (xr(i)-x(i))* (x(i)-xl(i)) ) .le. 0. ) then
             xl(i) = x(i) 
             xr(i) = x(i)
          Endif          
          delta(i) = xr(i) - xl(i)
          am3 = 3. * x(i)
          s1  = am3 - 2. * xr(i)
          s2  = am3 - 2. * xl(i)
          IF( delta(i) * (xl(i) - s1) .le. 0. ) xl(i) = s1
          IF( delta(i) * (s2 - xr(i)) .le. 0. ) xr(i) = s2
          delta(i) = xr(i) - xl(i)
          a6(i) = 6.*x(i)-3.*(xl(i) +xr(i))
C
       End do   
C
        diffmod = 0
	IF (mod(coeffraf,2) == 0) diffmod = 1           
C
        ipos = i1
C               
        Do iparent = locind_parent_left,locind_parent_last       
             pos=1
             cavg = 0.
             Do jj = ipos - coeffraf/2+diffmod,ipos + coeffraf/2
C
               ytemp(jj) = (diff(pos)*xl(iparent)   
     &             + diff2(pos)
     &             *  (delta(iparent)+a6(iparent))
     &             - diff3(pos)*a6(iparent))*coeffraf
                              
               cavg = cavg + ytemp(jj)
               pos = pos+1 
             End do 
             ipos = ipos + coeffraf
C
        End do     
C
C
        y(1:nc)=ytemp(1:nc)                                 
        deallocate(ytemp)                
        deallocate(diff, diff2, diff3)
      Return
      End Subroutine ppm1d
C                              
C     **************************************************************************  
CCC   Subroutine eno1d
C     ************************************************************************** 
C 
      Subroutine eno1d(x,y,np,nc,
     &                    s_parent,s_child,ds_parent,ds_child) 
C
CCC   Description:
CCC   ---- p 163-164 Computational gasdynamics ----
CCC   Subroutine to do a 1D interpolation 
CCC   using piecewise polynomial ENO reconstruction technique  
CCC   on a child grid (vector y) from its parent grid (vector x).
CC    Method:
C
C     Declarations:
C
      Implicit none
C        
C     Arguments
      Integer             :: np,nc      
      Real, Dimension(np) :: x      
      Real, Dimension(nc) :: y
      Real, Dimension(:),Allocatable :: ytemp
      Real                :: s_parent,s_child,ds_parent,ds_child
C
C     Local scalars
      Integer :: i,coeffraf,locind_parent_left,locind_parent_last
      Integer :: ipos,pos
      Real    :: ypos,xi
      integer :: i1,jj
      Real :: xpmin,cavg
C 
      Real, Dimension(3,np) :: dd,c
      Integer :: left
C           
      Real, DImension(1:np+1) :: xhalf
      Real, Dimension(:,:),Allocatable  :: Xbar 
      INTEGER :: diffmod     
C
      coeffraf = nint(ds_parent/ds_child)
C      
      If (coeffraf == 1) Then
          locind_parent_left = 1 + nint((s_child - s_parent)/ds_parent)
          y(1:nc) = x(locind_parent_left:locind_parent_left+nc-1)
          return
      End If     
      
      diffmod = 0
      IF (mod(coeffraf,2) == 0) diffmod = 1  
C      
      Allocate(ytemp(-2*coeffraf:nc+2*coeffraf))
      ypos = s_child  
      locind_parent_left = 1 + agrif_int((ypos - s_parent)/ds_parent) 
      locind_parent_last = 1 +
     &      agrif_ceiling((ypos +(nc - 1) *ds_child - 
     &      s_parent)/ds_parent)       
      xpmin = s_parent + (locind_parent_left-1)*ds_parent      
      i1 = 1+agrif_int((xpmin-s_child)/ds_child)           
C      
      xhalf(np+1) = np + 0.5
      Do i = 1,np
          xhalf(i) = i - 0.5
      Enddo
C
C compute divided differences
C
      dd(1,1:np) = x(1:np)
      dd(2,1:np-1) = 0.5*( dd(1,2:np) - dd(1,1:np-1) )
      dd(3,1:np-2) = (1./3.)*( dd(2,2:np-1) - dd(2,1:np-2) )
C
      Allocate( Xbar( coeffraf,2 ) )
      xi = 0.5
      Do i = 1,coeffraf
        Xbar(i,1) = (i-1)*ds_child/ds_parent - xi
        Xbar(i,2) = i*ds_child/ds_parent - xi
      Enddo
C
      ipos = i1
C           
      DO i = locind_parent_left,locind_parent_last           
         left = i           
         do jj = 2,3
             If(abs(dd(jj,left)) .gt. abs(dd(jj,left-1)))
     &               left = left-1            
         enddo
C           
C  convert to Taylor series form
C
         Call Taylor(i,xhalf(left:left+2),dd(1:3,left),c(1:3,i))
      ENDDO     
C
C evaluate the reconstruction on each cell
C
       DO i = locind_parent_left,locind_parent_last  
C
         cavg = 0.
         pos = 1.            
C         
         Do jj = ipos - coeffraf/2+diffmod,ipos + coeffraf/2
           ytemp(jj) =(c(1,i)*(Xbar(pos,2)-Xbar(pos,1))
     &                +c(2,i)*(Xbar(pos,2)*Xbar(pos,2)-
     &                         Xbar(pos,1)*Xbar(pos,1))
     &                +c(3,i)*(Xbar(pos,2)*Xbar(pos,2)*Xbar(pos,2)-
     &                         Xbar(pos,1)*Xbar(pos,1)*Xbar(pos,1)))
     &                         *coeffraf
           cavg = cavg + ytemp(jj)
           pos = pos+1
         Enddo                
         ipos = ipos + coeffraf                 
      ENDDO
C
      y(1:nc)=ytemp(1:nc)                                            
      deallocate(ytemp,Xbar)                 
C      
      Return       
      End Subroutine eno1d
C 
C      
C     **************************************************************************  
CCC   Subroutine taylor
C     ************************************************************************** 
C       
      subroutine taylor(ind,xhalf,dd,c)      
C      
      Integer :: ind
      real,dimension(3) :: dd,c     
      real,dimension(0:3,0:3) :: d 
      real,dimension(3) :: xhalf    
      integer ::i,j
C      
C
      d(0,0:3)=1.
      do i = 1,3
         d(i,0)=(ind-xhalf(i))*d(i-1,0)
      enddo  
C      
      do i = 1,3
         do j = 1,3-i
           d(i,j) = d(i,j-1) + (ind-xhalf(i+j))*d(i-1,j)
         enddo
      enddo
C         
      do j = 1,3        
         c(j) = 0.
         do i=0,3-j
            c(j) = c(j) + d(i,j)*dd(i+j)         
         enddo
      enddo
C      
      end subroutine taylor    
      
      
      REAL FUNCTION vanleer(tab)
      REAL, DIMENSION(3) :: tab
       real res1
       real p1,p2,p3
       
       p1=(tab(3)-tab(1))/2.
       p2=2.*(tab(2)-tab(1))
       p3=2.*(tab(3)-tab(2))
       
       if ((p1>0.).AND.(p2>0.).AND.(p3>0)) then
          res1=minval((/p1,p2,p3/))
       elseif ((p1<0.).AND.(p2<0.).AND.(p3<0)) then
          res1=maxval((/p1,p2,p3/))
       else
          res1=0.
       endif
          
          vanleer = res1   
      
      
      END FUNCTION vanleer 

C      
      End Module Agrif_Interpbasic
