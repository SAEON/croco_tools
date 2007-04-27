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
      Real,Dimension(Agrif_MaxRaff) :: tabdiff2, tabdiff3
      Real,Dimension(:),Allocatable::tabtest4      
       
      CONTAINS
C     Define procedures contained in this module
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
      REAL,INTENT(IN), DIMENSION(np) :: x      
      REAL,INTENT(OUT), DIMENSION(nc) :: y  
      REAL                :: s_parent,s_child,ds_parent,ds_child
C
C     Local scalars
      INTEGER :: i,coeffraf,locind_parent_left
      REAL    :: ypos,globind_parent_left,globind_parent_right
      REAL    :: invds, invds2
      REAL :: ypos2,diff
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

      locind_parent_left = 1 + agrif_int((ypos - s_parent)/ds_parent)

        globind_parent_left = s_parent 
     &                        + (locind_parent_left - 1)*ds_parent

        globind_parent_right = globind_parent_left + ds_parent

C
      invds = 1./ds_parent
      invds2 = ds_child/ds_parent
      
      ypos2 = ypos*invds
      globind_parent_right=globind_parent_right*invds
      
      do i = 1,nc-1
C
        if (ypos2 > globind_parent_right) then
           locind_parent_left = locind_parent_left + 1.
           globind_parent_right = globind_parent_right + 1.
        endif
        
        diff=(globind_parent_right - ypos2)
        
        y(i) = (diff*x(locind_parent_left)
     &        + (1.-diff)*x(locind_parent_left+1))
C
        ypos2 = ypos2 + invds2
C
      enddo
C
      ypos = s_child + (nc-1)*ds_child
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
     &            *x(locind_parent_left+1))*invds
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
      REAL,INTENT(IN), DIMENSION(np) :: x      
      REAL,INTENT(OUT), DIMENSION(nc) :: y  
      REAL                :: s_parent,s_child,ds_parent,ds_child 
C
C     Local scalars
      INTEGER :: i,coeffraf,locind_parent_left
      REAL    :: ypos,globind_parent_left
      REAL    :: X1,X2,X3 
      real :: deltax,invdsparent
      real t1,t2,t3,t4,t5,t6,t7,t8
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
      
      invdsparent=1./ds_parent
C
      ypos = s_child      
C
      do i = 1,nc
C
        locind_parent_left = 1 + agrif_int((ypos - s_parent)/ds_parent)
C

        globind_parent_left = s_parent 
     &                        + (locind_parent_left - 1)*ds_parent 
     
        deltax = invdsparent*(ypos-globind_parent_left)
        ypos = ypos + ds_child
         if (abs(deltax).LE.0.0001) then
           y(i)=x(locind_parent_left)
           
           cycle
         endif
C           
C
        t2 = deltax - 2.
        t3 = deltax - 1.
        t4 = deltax + 1.
        
        t5 = -(1./6.)*deltax*t2*t3
        t6 = 0.5*t2*t3*t4
        t7 = -0.5*deltax*t2*t4
        t8 = (1./6.)*deltax*t3*t4
        
        y(i)=t5*x(locind_parent_left-1)+t6*x(locind_parent_left)
     &  +t7*x(locind_parent_left+1)+t8*x(locind_parent_left+2)
C
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
      REAL,INTENT(IN), DIMENSION(np) :: x      
      REAL,INTENT(OUT), DIMENSION(nc) :: y  
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
      Real, INTENT(IN),Dimension(np) :: x      
      Real, INTENT(OUT),Dimension(nc) :: y
C      Real, Dimension(:),Allocatable :: ytemp
      Real                :: s_parent,s_child,ds_parent,ds_child
C
C     Local scalars
      Integer :: i,coeffraf,locind_parent_left,locind_parent_last
      Integer :: iparent,ipos,pos,nmin,nmax
      Real    :: ypos
      integer :: i1,jj
      Real :: xpmin,a
C      
      Real :: xrmin,xrmax,am3,s2,s1  
      Real, Dimension(np) :: xl,delta,a6,slope
C      Real, Dimension(:),Allocatable  :: diff,diff2,diff3    
      INTEGER :: diffmod
      REAL :: invcoeffraf
C      
      coeffraf = nint(ds_parent/ds_child)
C
      If (coeffraf == 1) Then
          locind_parent_left = 1 + nint((s_child - s_parent)/ds_parent)
          y(1:nc) = x(locind_parent_left:locind_parent_left+nc-1)
          return
      End If
      invcoeffraf = ds_child/ds_parent
C      

      IF( .NOT. allocated(tabtest4) ) THEN    
      Allocate(tabtest4(-2*coeffraf:nc+2*coeffraf))
      ELSE
         IF (size(tabtest4) .LT. nc+4*coeffraf+1)THEN
         deallocate( tabtest4 )
         Allocate(tabtest4(-2*coeffraf:nc+2*coeffraf))
         ENDIF
      ENDIF  
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
C
      
      Do i=1,coeffraf
        tabdiff2(i)=(real(i)-0.5)*invcoeffraf
      EndDo

      a = invcoeffraf**2 
      tabdiff3(1) = (1./3.)*a
      a=2.*a
      Do i=2,coeffraf
        tabdiff3(i) = tabdiff3(i-1)+(real(i)-1)*a
      EndDo
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
C
      Do i = nmin,nmax
         slope(i) = x(i) - x(i-1)
      Enddo

      Do i = nmin+1,nmax-1
         xl(i)= 0.5*(x(i-1)+x(i))
     &      -0.08333333333333*(slope(i+1)-slope(i-1)) 
      Enddo
C
C apply parabolic monotonicity
       Do i = locind_parent_left,locind_parent_last
          delta(i) = xl(i+1) - xl(i)
          a6(i) = 6.*x(i)-3.*(xl(i) +xl(i+1))
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
             Do jj = ipos - coeffraf/2+diffmod,ipos + coeffraf/2
C
               tabtest4(jj) = xl(iparent)   
     &             + tabdiff2(pos) *  (delta(iparent)+a6(iparent))
     &             - tabdiff3(pos) *  a6(iparent)
               pos = pos+1 
             End do 
             ipos = ipos + coeffraf
C
        End do     
C
C
        y(1:nc)=tabtest4(1:nc)
        
      Return
      End Subroutine ppm1d
      
C     **************************************************************************  
CCC   Subroutine weno1d
C     ************************************************************************** 
C 
      Subroutine weno1dnew(x,y,np,nc,
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
      Real, Dimension(np) :: xr,xl,delta,a6,slope,slope2,smooth
      Real, Dimension(:),Allocatable  :: diff,diff2,diff3
      INTEGER :: diffmod
      REAL :: invcoeffraf
      integer :: s,l,k
      integer :: etan, etap
      real :: delta0, delta1, delta2
      real :: epsilon
      parameter (epsilon = 1.D-8)
      real, dimension(:,:), allocatable :: ak, ck
C      
      coeffraf = nint(ds_parent/ds_child)
C
      If (coeffraf == 1) Then
          locind_parent_left = 1 + nint((s_child - s_parent)/ds_parent)
          y(1:nc) = x(locind_parent_left:locind_parent_left+nc-1)
          return
      End If
      invcoeffraf = ds_child/ds_parent
      Allocate(ak(0:1,coeffraf))
      Allocate(ck(0:1,coeffraf))
            
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
      diff(1)=0.5*invcoeffraf
      do i=2,coeffraf
         diff(i) = diff(i-1)+invcoeffraf
      enddo
      
      ak = 0.
      ck = 0.
      
      do i=1,coeffraf
         do k=0,1
         do s=0,2
         do l=0,2
           if (l /= s) then
            ak(k,i) = ak(k,i)+(diff(i)-(k-l+1.))
           endif
         enddo
         enddo
         enddo
               
         etap = 0
         etan = 0
         do k=0,1
          if (ak(k,i) > 0) then
            etap = etap+1
          else if (ak(k,i) < 0) then
            etan = etan + 1
          endif
         enddo
               
         do k=0,1
           if (ak(k,i) == 0) then
            Ck(k,i) = 1.
           else if (ak(k,i) > 0) then
            Ck(k,i) = 1./(etap * ak(k,i))
           else
            Ck(k,i) = -1./(etan * ak(k,i))
           endif
         enddo
      enddo
                     
C      
      a = 0.
      b = invcoeffraf
      Do i=1,coeffraf
         diff2(i) = 0.5*(b*b - a*a)  
         diff3(i) = (1./3.)*(b*b*b - a*a*a)
         a = a + invcoeffraf
         b = b + invcoeffraf
      End do
C
      if( locind_parent_last+2 <= np ) then
           nmax = locind_parent_last+2    
      elseif( locind_parent_last+1 <= np ) then
           nmax = locind_parent_last+1
      else
           nmax = locind_parent_last 
      endif     
C      
      if(locind_parent_left-2 >= 1) then
          nmin = locind_parent_left-2
      elseif(locind_parent_left-1 >= 1) then
          nmin = locind_parent_left-1
      else 
          nmin = locind_parent_left
      endif    
C 
      Do i = nmin+1,nmax
         slope(i) = (x(i) - x(i-1))
      Enddo 
      DO i=nmin+2,nmax
        smooth(i) = 0.5*(slope(i)**2+slope(i-1)**2)
     &       +(slope(i)-slope(i-1))**2
      enddo
C
        diffmod = 0
        IF (mod(coeffraf,2) == 0) diffmod = 1           
C
        ipos = i1
C               
        Do iparent = locind_parent_left,locind_parent_last       
             pos=1
             
            delta0=1./(epsilon+smooth(iparent  ))**3
            delta1=1./(epsilon+smooth(iparent+1))**3
            delta2=1./(epsilon+smooth(iparent+2))**3   
                      
             Do jj = ipos - coeffraf/2+diffmod,ipos + coeffraf/2
C
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
        
        deallocate(ak,ck)
        
      Return
      End Subroutine weno1dnew
      
C     **************************************************************************  
CCC   Subroutine weno1d
C     ************************************************************************** 
C 
      Subroutine weno1d(x,y,np,nc,
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
      Real, Dimension(np) :: xr,xl,delta,a6,slope,slope2
      Real, Dimension(:),Allocatable  :: diff,diff2,diff3
      INTEGER :: diffmod
      REAL :: invcoeffraf
      integer :: s,l,k
      integer :: etan, etap
      real :: delta0, delta1,sumdelta
      real :: epsilon
      parameter (epsilon = 1.D-8)
C      
      coeffraf = nint(ds_parent/ds_child)
C
      If (coeffraf == 1) Then
          locind_parent_left = 1 + nint((s_child - s_parent)/ds_parent)
          y(1:nc) = x(locind_parent_left:locind_parent_left+nc-1)
          return
      End If
      invcoeffraf = ds_child/ds_parent
            
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
      Allocate( diff(coeffraf))
C      
      diff(1)=0.5*invcoeffraf
      do i=2,coeffraf
         diff(i) = diff(i-1)+invcoeffraf
      enddo
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
      Do i = nmin+1,nmax
         slope(i) = (x(i) - x(i-1))
      Enddo 
C
        diffmod = 0
        IF (mod(coeffraf,2) == 0) diffmod = 1           
C
        ipos = i1
C               
        Do iparent = locind_parent_left,locind_parent_last       
             pos=1
            delta0=1./(epsilon+slope(iparent  )**2)**2
            delta1=1./(epsilon+slope(iparent+1)**2)**2
            sumdelta = 1./(delta0+delta1)             
             Do jj = ipos - coeffraf/2+diffmod,ipos + coeffraf/2
C                              
          ytemp(jj) = x(iparent)+(diff(pos)-0.5)*(
     &                delta0*slope(iparent)+
     &                delta1*slope(iparent+1))*sumdelta
               pos = pos+1 
             End do 
             ipos = ipos + coeffraf
C
        End do     
C
C
        y(1:nc)=ytemp(1:nc)                                 
        deallocate(ytemp)                
        deallocate(diff)
        
      Return
      End Subroutine weno1d
                 
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
