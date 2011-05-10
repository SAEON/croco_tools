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
C     Foundation, Inc., 59 Temple Place- Suite 330, Boston, MA 02111-1307, USA.
C
C
C
CCC   Module AGRIF_Clustering  
C
C
      Module Agrif_Clustering 
C
CCC   Description:
CCC   Module to create and initialize the grid hierarchy from the 
CCC   AGRIF_FixedGrids.in file.
C
C     Modules used:
C  
      Use Agrif_Curgridfunctions 
      Use Agrif_Init_Vars
      Use Agrif_Save
C             
      IMPLICIT NONE
C        
      Contains 
C     Define procedures contained in this module
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Cluster_All  
C     **************************************************************************
C
      Recursive Subroutine Agrif_Cluster_All(g,coarsegrid)
C
CCC   Description:
CCC   Subroutine for the clustering. A temporary grid hierarchy, pointed by 
CCC   coarsegrid, is created.
C
CC    Method:
CC            
C
C     Declarations:
C
C     Pointer arguments    
      TYPE(AGRIF_grid)     ,pointer   :: g        ! Pointer on the current grid
      TYPE(AGRIF_rectangle),pointer   :: coarsegrid
C      
C     Local pointer         
      TYPE(AGRIF_lrectangle),pointer  :: parcours 
      TYPE(AGRIF_grid)      ,pointer  :: newgrid 
      REAL                            :: g_eps
      INTEGER                         :: iii
C      
      g_eps = huge(1.)
      do iii = 1 , Agrif_Probdim
         g_eps = min(g_eps,g%Agrif_d(iii))
      enddo
C
      g_eps = g_eps/100.                  
C
C     Necessary condition for clustering 
      do iii = 1 , Agrif_Probdim
         if (g%Agrif_d(iii)/Agrif_coeffref(iii).LT.
     &                   (Agrif_mind(iii)-g_eps)) Return
      enddo
C
      Nullify(coarsegrid%childgrids)
C
      Call Agrif_ClusterGridnD(g,coarsegrid) 
C
      parcours => coarsegrid % childgrids                   
C      
      do while (associated(parcours))
C      
C       Newgrid is created. It is a copy of a fine grid created previously by
C          clustering.      
        Allocate(newgrid)
C      
        Nullify(newgrid%child_grids)
C
        do iii = 1 , Agrif_Probdim
           newgrid % nb(iii) = (parcours % r % imax(iii) - 
     &                          parcours % r % imin(iii)) *
     &                          Agrif_Coeffref(iii)
C        
           newgrid % Agrif_x(iii) = g%Agrif_x(iii) + 
     &                         (parcours %r % imin(iii) -1) 
     &                         *g%Agrif_d(iii)
C 
           newgrid % Agrif_d(iii) = g%Agrif_d(iii) / Agrif_Coeffref(iii)
C
        enddo
C
        if ( Agrif_Probdim .EQ. 1 ) then
           allocate(newgrid%tabpoint1D(newgrid%nb(1)+1))
           newgrid%tabpoint1D = 0
        endif
C
        if ( Agrif_Probdim .EQ. 2 ) then
           allocate(newgrid%tabpoint2D(newgrid%nb(1)+1,
     &                                 newgrid%nb(2)+1))
           newgrid%tabpoint2D = 0      
        endif
C
        if ( Agrif_Probdim .EQ. 3 ) then
           allocate(newgrid%tabpoint3D(newgrid%nb(1)+1,
     &                                 newgrid%nb(2)+1,
     &                                 newgrid%nb(3)+1))
           newgrid%tabpoint3D = 0
        endif
C       Points detection on newgrid
        Call Agrif_TabpointsnD(Agrif_mygrid,newgrid) 
C
C       Recursive call to Agrif_Cluster_All            
        Call Agrif_Cluster_All (newgrid, parcours % r)     
C
        parcours => parcours % next 
C      
        if ( Agrif_Probdim .EQ. 1 ) Deallocate(newgrid%tabpoint1D)
        if ( Agrif_Probdim .EQ. 2 ) Deallocate(newgrid%tabpoint2D)
        if ( Agrif_Probdim .EQ. 3 ) Deallocate(newgrid%tabpoint3D)
C
        Deallocate(newgrid)   
C
      enddo
C
C      
      End Subroutine Agrif_Cluster_All
C
C     **************************************************************************
CCC   Subroutine Agrif_TabpointsnD
C     **************************************************************************
C
      Recursive Subroutine Agrif_TabpointsnD(g,newgrid)
C
CCC   Description:
CCC   Copy on newgrid of points detected on the grid hierarchy pointed by g.   
C
CC    Method:
CC            
C
C     Declarations:
C
C     Arguments
      TYPE(Agrif_Grid),pointer   :: g,newgrid
C
C     Local variables      
      TYPE(Agrif_Pgrid),pointer  :: parcours
C
      REAL                  :: g_eps,newgrid_eps,eps
      REAL   , DIMENSION(3) :: newmin,newmax
      REAL   , DIMENSION(3) :: gmin,gmax
      REAL   , DIMENSION(3) :: xmin
      INTEGER, DIMENSION(3) :: igmin,inewmin
      INTEGER, DIMENSION(3) :: inewmax
      INTEGER               :: iii
      INTEGER               :: i,j,k
      INTEGER               :: i0,j0,k0
C
C     
      parcours => g % child_grids
C
      do While(associated(parcours))
        Call Agrif_TabpointsnD(parcours%gr,newgrid)
        parcours => parcours % next
      enddo      
C
      g_eps = 10.
      newgrid_eps = 10.
C      
      do iii = 1 , Agrif_probdim
         g_eps = min( g_eps , g % Agrif_d(iii) )
         newgrid_eps = min(newgrid_eps,newgrid%Agrif_d(iii))
      enddo
C
      eps = min(g_eps,newgrid_eps)/100.                  
C                   
      do iii = 1 , Agrif_probdim
         if (newgrid%Agrif_d(iii) .LT. (g%Agrif_d(iii)-eps)) Return
C                   
         gmin(iii) = g%Agrif_x(iii)
         gmax(iii) = g%Agrif_x(iii) + g%nb(iii) * g%Agrif_d(iii)
C 
         newmin(iii) = newgrid%Agrif_x(iii)
         newmax(iii) = newgrid%Agrif_x(iii) + newgrid%nb(iii) *
     &                 newgrid%Agrif_d(iii)
C      
         if (gmax(iii) .LT. newmin(iii)) Return
C
         if (gmin(iii) .GT. newmax(iii)) Return
C           
         inewmin(iii) = 1 - floor(-(max(gmin(iii),newmin(iii))-
     &                    newmin(iii))
     &                   /newgrid%Agrif_d(iii)) 
C          
         xmin(iii) = newgrid%Agrif_x(iii) + (inewmin(iii)-1)*
     &              newgrid%Agrif_d(iii)          
C        
         igmin(iii) = 1 + nint((xmin(iii)-gmin(iii))/g%Agrif_d(iii))
C
         inewmax(iii) = 1 + int((min(gmax(iii),newmax(iii))-
     &                  newmin(iii))/newgrid%Agrif_d(iii))
      enddo
C                
      if ( Agrif_probdim .EQ. 1 ) then
         i0 = igmin(1)
         do i = inewmin(1),inewmax(1)
            newgrid%tabpoint1D(i) = max(
     &                           newgrid%tabpoint1D(i),
     &                           g%tabpoint1D(i0))
         enddo
         i0 = i0 + int(newgrid%Agrif_d(1)/g%Agrif_d(1))
      endif
C
      if ( Agrif_probdim .EQ. 2 ) then
         i0 = igmin(1)
         do i = inewmin(1),inewmax(1)
            j0 = igmin(2)
            do j = inewmin(2),inewmax(2)
               newgrid%tabpoint2D(i,j) = max(
     &                           newgrid%tabpoint2D(i,j),
     &                           g%tabpoint2D(i0,j0))
               j0 = j0 + int(newgrid%Agrif_d(2)/g%Agrif_d(2))
            enddo
            i0 = i0 + int(newgrid%Agrif_d(1)/g%Agrif_d(1))
         enddo
      endif
C
      if ( Agrif_probdim .EQ. 3 ) then
         i0 = igmin(1)
         do i = inewmin(1),inewmax(1)
            j0 = igmin(2)
            do j = inewmin(2),inewmax(2)
               k0 = igmin(3)
               do k = inewmin(3),inewmax(3)
                  newgrid%tabpoint3D(i,j,k) = max(
     &                           newgrid%tabpoint3D(i,j,k),
     &                           g%tabpoint3D(i0,j0,k0))
                  k0 = k0 + int(newgrid%Agrif_d(3)/g%Agrif_d(3))
               enddo                                        
               j0 = j0 + int(newgrid%Agrif_d(2)/g%Agrif_d(2))
            enddo
            i0 = i0 + int(newgrid%Agrif_d(1)/g%Agrif_d(1))
         enddo
      endif
C
      Return
C
C
      End Subroutine Agrif_TabpointsnD
C
C     **************************************************************************
CCC   Subroutine Agrif_ClusterGridnD
C     **************************************************************************
C      
      Subroutine Agrif_ClusterGridnD(g,coarsegrid)
C
CCC   Description:
CCC   Clustering on the grid pointed by g.
C
CC    Method:
CC            
C
C     Declarations:
C
C     Pointer arguments
      TYPE(AGRIF_grid)     ,pointer  :: g          ! Pointer on the current grid
      TYPE(AGRIF_rectangle),pointer  :: coarsegrid
C
C     Local variables      
      TYPE(Agrif_rectangle) :: newrect
      TYPE(Agrif_Variable)  :: newflag
C
      INTEGER               :: i,j,k
      INTEGER ,DIMENSION(3) :: sx
      INTEGER               :: bufferwidth,flagpoints
      INTEGER               :: n1,n2,m1,m2,o1,o2
      INTEGER               :: iii
C
C      
      bufferwidth = int(Agrif_Minwidth/5.)
C      
      do iii = 1 , Agrif_probdim
         sx(iii) = g % nb(iii) + 1
      enddo
C                  
      if ( Agrif_probdim .EQ. 1 ) then
         allocate(newflag%iarray1(sx(1)))
         newflag%iarray1 = 0      
      endif
      if ( Agrif_probdim .EQ. 2 ) then
         allocate(newflag%iarray2(sx(1),sx(2)))
         newflag%iarray2 = 0      
      endif
      if ( Agrif_probdim .EQ. 3 ) then
         allocate(newflag%iarray3(sx(1),sx(2),sx(3)))
         newflag%iarray3 = 0      
      endif
C
      flagpoints = 0 
C      
      if (bufferwidth>0) then 
C      
         if ( Agrif_probdim .EQ. 1 ) then
            do i = bufferwidth,sx(1)-bufferwidth+1      
               if (g % tabpoint1D(i) .EQ. 1) then
                  m1 = i - bufferwidth + 1
                  m2 = i + bufferwidth - 1                  
                  flagpoints = flagpoints + 1
                  newflag%iarray1(m1:m2)=1 
               endif 
            enddo    
         endif
C
        if ( Agrif_probdim .EQ. 2 ) then
           do i = bufferwidth,sx(1)-bufferwidth+1      
               do j = bufferwidth,sx(2)-bufferwidth+1
                  if (g % tabpoint2D(i,j) .EQ. 1) then
                     n1 = j - bufferwidth + 1
                     n2 = j + bufferwidth - 1
                     m1 = i - bufferwidth + 1
                     m2 = i + bufferwidth - 1                  
                     flagpoints = flagpoints + 1
                     newflag%iarray2(m1:m2,n1:n2)=1 
                  endif  
               enddo    
            enddo
         endif
C
        if ( Agrif_probdim .EQ. 3 ) then
            do i = bufferwidth,sx(1)-bufferwidth+1      
               do j = bufferwidth,sx(2)-bufferwidth+1
                  do k = bufferwidth,sx(3)-bufferwidth+1
                     if (g % tabpoint3D(i,j,k) .EQ. 1) then
                        o1 = k - bufferwidth + 1
                        o2 = k + bufferwidth - 1
                        n1 = j - bufferwidth + 1
                        n2 = j + bufferwidth - 1
                        m1 = i - bufferwidth + 1
                        m2 = i + bufferwidth - 1                  
                        flagpoints = flagpoints + 1
                        newflag%iarray3(m1:m2,n1:n2,o1:o2)=1 
                     endif 
                  enddo  
               enddo    
            enddo
        endif
      else
          flagpoints = 1       
C
          if ( Agrif_probdim .EQ. 1 ) then
          newflag%iarray1 = g % tabpoint1D
          endif
          if ( Agrif_probdim .EQ. 2 ) then
          newflag%iarray2 = g % tabpoint2D
          endif
          if ( Agrif_probdim .EQ. 3 ) then
          newflag%iarray3 = g % tabpoint3D
          endif
      endif
C       
      if (flagpoints .EQ. 0) then       
          if ( Agrif_probdim .EQ. 1 ) deallocate(newflag%iarray1) 
          if ( Agrif_probdim .EQ. 2 ) deallocate(newflag%iarray2) 
          if ( Agrif_probdim .EQ. 3 ) deallocate(newflag%iarray3) 
          Return 
      endif       
C      
      do iii = 1 , Agrif_probdim
         newrect % imin(iii) = 1
         newrect % imax(iii) = sx(iii)
      enddo
C      
      Call Agrif_Clusternd(newflag,
     &                     coarsegrid%childgrids,newrect) 
C      
      if ( Agrif_probdim .EQ. 1 ) deallocate(newflag%iarray1)
      if ( Agrif_probdim .EQ. 2 ) deallocate(newflag%iarray2)
      if ( Agrif_probdim .EQ. 3 ) deallocate(newflag%iarray3)
C  
C    
      End Subroutine Agrif_ClusterGridnD
C
C     **************************************************************************
CCC   Subroutine Agrif_ClusternD
C     **************************************************************************
C
      Recursive subroutine Agrif_Clusternd(flag,boxlib,oldB) 
C
CCC   Description:
CCC   Clustering on the grid pointed by oldB.
C
CC    Method:
CC            
C
C     Declarations:
C
C     Arguments  
      TYPE(Agrif_rectangle) ::  oldB    
      TYPE(Agrif_Variable)  :: flag
c      INTEGER,DIMENSION(oldB%imin(1):oldB%imax(1),
c     &                  oldB%imin(2):oldB%imax(2)) :: flag
      TYPE(Agrif_lrectangle),pointer :: boxlib      
C
C     Local variables      
      TYPE(Agrif_lrectangle),pointer :: tempbox,parcbox,parcbox2       
      TYPE(Agrif_rectangle) :: newB,newB2
      INTEGER :: i,j,k,iii
      INTEGER,DIMENSION(:),allocatable :: i_sig 
      INTEGER,DIMENSION(:),allocatable :: j_sig       
      INTEGER,DIMENSION(:),allocatable :: k_sig       
      INTEGER,DIMENSION(3) :: ipu,ipl
      INTEGER,DIMENSION(3) :: istr,islice
      REAL :: cureff
      REAL :: neweff
      INTEGER :: ValMax,ValSum,TailleTab
      INTEGER :: nbpoints,nbpointsflag  
      LOGICAL :: test
C          
      allocate(i_sig(oldB%imin(1):oldB%imax(1)))
      if ( Agrif_probdim .GE. 2 ) 
     & allocate(j_sig(oldB%imin(2):oldB%imax(2)))
      if ( Agrif_probdim .EQ. 3 ) 
     & allocate(k_sig(oldB%imin(3):oldB%imax(3)))
C
      test = .FALSE.
      do iii = 1 , Agrif_probdim
         test = test .OR. ( (oldB%imax(iii)-oldB%imin(iii)+1) 
     &                    .LT. Agrif_Minwidth)
      enddo
      if ( test ) Return
C     
      if ( Agrif_probdim .EQ. 1 ) i_sig = flag%iarray1
      if ( Agrif_probdim .EQ. 2 ) then
         do i = oldB%imin(1),oldB%imax(1)
           i_sig(i) = SUM(flag%iarray2(i,
     &                            oldB%imin(2):oldB%imax(2)))
         enddo
         do j = oldB%imin(2),oldB%imax(2)
           j_sig(j) = SUM(flag%iarray2(
     &                            oldB%imin(1):oldB%imax(1),j))
         enddo      
      endif
      if ( Agrif_probdim .EQ. 3 ) then
         do i = oldB%imin(1),oldB%imax(1)
            i_sig(i) = SUM(flag%iarray3(i,
     &                            oldB%imin(2):oldB%imax(2),
     &                            oldB%imin(3):oldB%imax(3)))
         enddo
         do j = oldB%imin(2),oldB%imax(2)
            j_sig(j) = SUM(flag%iarray3(
     &                          oldB%imin(1):oldB%imax(1),j,
     &                          oldB%imin(3):oldB%imax(3)))
         enddo
         do k = oldB%imin(3),oldB%imax(3)
            k_sig(k) = SUM(flag%iarray3(
     &                          oldB%imin(1):oldB%imax(1),
     &                          oldB%imin(2):oldB%imax(2),k))
         enddo      
      endif
C                    
      do iii = 1 , Agrif_probdim
         ipl(iii) = oldB%imin(iii)
         ipu(iii) = oldB%imax(iii)
      enddo
C           
      Call Agrif_Clusterprune(i_sig,ipl(1),ipu(1)) 
      if ( Agrif_probdim .GE. 2 ) 
     &   Call Agrif_Clusterprune(j_sig,ipl(2),ipu(2))      
      if ( Agrif_probdim .EQ. 3 ) 
     &   Call Agrif_Clusterprune(k_sig,ipl(3),ipu(3))      
C     
      test = .TRUE.
      do iii = 1 , Agrif_probdim
         test = test .AND. (ipl(iii).EQ.oldB%imin(iii))
         test = test .AND. (ipu(iii).EQ.oldB%imax(iii))
      enddo
      
      if (.NOT. test) then 
          do iii = 1 , Agrif_probdim
             newB%imin(iii) = ipl(iii)
             newB%imax(iii) = ipu(iii)
          enddo
C        
          if ( Agrif_probdim .EQ. 1 ) 
     &       nbpoints = SUM(flag%iarray1(newB%imin(1):newB%imax(1)))   
          if ( Agrif_probdim .EQ. 2 ) 
     &       nbpoints = SUM(flag%iarray2(newB%imin(1):newB%imax(1),
     &                           newB%imin(2):newB%imax(2)))   
          if ( Agrif_probdim .EQ. 3 ) 
     &       nbpoints = SUM(flag%iarray3(newB%imin(1):newB%imax(1),
     &                           newB%imin(2):newB%imax(2),   
     &                           newB%imin(3):newB%imax(3)))
C   
          if ( Agrif_probdim .EQ. 1 ) 
     &       TailleTab = newB%imax(1)-newB%imin(1)+1
          if ( Agrif_probdim .EQ. 2 ) 
     &       TailleTab = (newB%imax(1)-newB%imin(1)+1)*
     &                   (newB%imax(2)-newB%imin(2)+1)
          if ( Agrif_probdim .EQ. 3 ) 
     &       TailleTab = (newB%imax(1)-newB%imin(1)+1)*
     &                   (newB%imax(2)-newB%imin(2)+1)*
     &                   (newB%imax(3)-newB%imin(3)+1)
C
          neweff = REAL(nbpoints)/TailleTab
C     
          if (nbpoints.GT.0) then
C        
              if ((neweff .GT .Agrif_efficiency)) then
                  Call Agrif_Add_Rectangle(newB,boxlib)
                  Return
              else
C            
                  tempbox => boxlib
                  newB2 = newB
                  Call Agrif_Clusternd(flag,
     &                      boxlib,newB)
C     
C                 Compute new efficiency
C
                  cureff = neweff
                  parcbox2 => boxlib
                  nbpoints = 0
                  nbpointsflag = 0
C
                  do While (associated(parcbox2))
                    if (associated(parcbox2,tempbox)) Exit
                    newB = parcbox2%r
C
                    if ( Agrif_probdim .EQ. 1 ) Valsum = 
     &                                 SUM(flag%iarray1(
     &                                 newB%imin(1):newB%imax(1)))
                    if ( Agrif_probdim .EQ. 2 ) Valsum = 
     &                                 SUM(flag%iarray2(
     &                                 newB%imin(1):newB%imax(1),
     &                                 newB%imin(2):newB%imax(2)))
                    if ( Agrif_probdim .EQ. 3 ) Valsum = 
     &                                 SUM(flag%iarray3(
     &                                 newB%imin(1):newB%imax(1),
     &                                 newB%imin(2):newB%imax(2),
     &                                 newB%imin(3):newB%imax(3)))
C
                    nbpointsflag = nbpointsflag + ValSum
                    if ( Agrif_probdim .EQ. 1 ) 
     &                 TailleTab = newB%imax(1)-newB%imin(1)+1
                    if ( Agrif_probdim .EQ. 2 ) 
     &                 TailleTab = (newB%imax(1)-newB%imin(1)+1)*
     &                             (newB%imax(2)-newB%imin(2)+1)
                    if ( Agrif_probdim .EQ. 3 ) 
     &                 TailleTab = (newB%imax(1)-newB%imin(1)+1)*
     &                             (newB%imax(2)-newB%imin(2)+1)*
     &                             (newB%imax(3)-newB%imin(3)+1)
                    nbpoints = nbpoints + TailleTab
                    parcbox2 => parcbox2%next  
                  enddo  
C coefficient 1.05 avant 1.15 possibilité de laisser choix à l utilisateur
                  if  (REAL(nbpointsflag)/REAL(nbpoints)
     &                 .LT.(1.0001*cureff)) then
                      parcbox2 => boxlib            
                      do While (associated(parcbox2))
                        if (associated(parcbox2,tempbox)) Exit
                        deallocate(parcbox2%r)
                        parcbox2 => parcbox2%next             
                      enddo      
                      boxlib => tempbox
                      Call Agrif_Add_Rectangle(newB2,boxlib)
                      Return
                  endif
              endif         
          endif
          Return
      endif 
C       
      do iii = 1 , Agrif_Probdim
         istr(iii) = oldB%imax(iii)
         islice(iii) = oldB%imin(iii)
      enddo     
C      
      Call Agrif_Clusterslice(i_sig,islice(1),istr(1)) 
      if ( Agrif_probdim .GE. 2 ) 
     & Call Agrif_Clusterslice(j_sig,islice(2),istr(2)) 
      if ( Agrif_probdim .EQ. 3 ) 
     & Call Agrif_Clusterslice(k_sig,islice(3),istr(3)) 
C      
      ValSum = 0
      do iii = 1 , Agrif_Probdim
         Valsum = valSum + islice(iii)
      enddo
C      
      if ( Valsum .EQ. -Agrif_Probdim ) then 
          Call Agrif_Add_Rectangle(oldB,boxlib) 
          Return 
      endif 
C       
      nullify(tempbox)
      tempbox => boxlib
      if ( Agrif_probdim .EQ. 1 ) 
     &      cureff  = oldB%imax(1)-oldB%imin(1)+1
      if ( Agrif_probdim .EQ. 2 ) 
     &      cureff  = (oldB%imax(1)-oldB%imin(1)+1)*
     &                (oldB%imax(2)-oldB%imin(2)+1)
      if ( Agrif_probdim .EQ. 3 ) 
     &       cureff = (oldB%imax(1)-oldB%imin(1)+1)*
     &                (oldB%imax(2)-oldB%imin(2)+1)*
     &                (oldB%imax(3)-oldB%imin(3)+1)
      Nullify(parcbox)
C 
      do iii = 1 , Agrif_Probdim
          newB%imax(iii) = oldB%imax(iii)            
          newB%imin(iii) = oldB%imin(iii)
      enddo     
C
      ValMax = 0
      do iii = 1 , Agrif_Probdim
         ValMax = Max(ValMax,istr(iii))
      enddo
C
      if (istr(1) .EQ. ValMax ) then
          newB%imax(1) = islice(1)
          Call Agrif_Add_Rectangle(newB,parcbox)                   
          newB%imin(1) = islice(1)+1
          newB%imax(1) = oldB%imax(1)
          Call Agrif_Add_Rectangle(newB,parcbox)
      elseif ( Agrif_probdim .GE. 2 ) then
         if (istr(2) .EQ. ValMax ) then
            newB%imax(2) = islice(2)
            Call Agrif_Add_Rectangle(newB,parcbox)
            newB%imin(2) = islice(2)+1
            newB%imax(2) = oldB%imax(2)
            Call Agrif_Add_Rectangle(newB,parcbox)
         elseif ( Agrif_probdim .EQ. 3 ) then
            newB%imax(3) = islice(3)
            Call Agrif_Add_Rectangle(newB,parcbox)
            newB%imin(3) = islice(3)+1
            newB%imax(3) = oldB%imax(3)
            Call Agrif_Add_Rectangle(newB,parcbox)
         endif
      endif
C      
      do While (associated(parcbox))
        newB = parcbox%r            
C
        if ( Agrif_probdim .EQ. 1 ) nbpoints = 
     &                                 SUM(flag%iarray1(
     &                                 newB%imin(1):newB%imax(1)))
        if ( Agrif_probdim .EQ. 2 ) nbpoints = 
     &                                 SUM(flag%iarray2(
     &                                 newB%imin(1):newB%imax(1),
     &                                 newB%imin(2):newB%imax(2)))
        if ( Agrif_probdim .EQ. 3 ) nbpoints = 
     &                                 SUM(flag%iarray3(
     &                                 newB%imin(1):newB%imax(1),
     &                                 newB%imin(2):newB%imax(2),
     &                                 newB%imin(3):newB%imax(3)))
C      
       if ( Agrif_probdim .EQ. 1 ) 
     &                 TailleTab = newB%imax(1)-newB%imin(1)+1
       if ( Agrif_probdim .EQ. 2 ) 
     &                 TailleTab = (newB%imax(1)-newB%imin(1)+1)*
     &                             (newB%imax(2)-newB%imin(2)+1)
       if ( Agrif_probdim .EQ. 3 ) 
     &                 TailleTab = (newB%imax(1)-newB%imin(1)+1)*
     &                             (newB%imax(2)-newB%imin(2)+1)*
     &                             (newB%imax(3)-newB%imin(3)+1)

        neweff = REAL(nbpoints) / TailleTab
C     
        if (nbpoints .GT. 0) then
C      
            if ((neweff .GT .Agrif_efficiency)) then
                Call Agrif_Add_Rectangle(newB,boxlib)
            else
                tempbox => boxlib
                newB2 = newB
                Call Agrif_Clusternd(flag,
     &                      boxlib,newB)
C
C               Compute new efficiency
C
                cureff = neweff
                parcbox2 => boxlib
                nbpoints = 0
                nbpointsflag = 0
C
                do While (associated(parcbox2))
                  if (associated(parcbox2,tempbox)) Exit
                  newB = parcbox2%r
C
                  if ( Agrif_probdim .EQ. 1 ) ValSum = 
     &                                 SUM(flag%iarray1(
     &                                 newB%imin(1):newB%imax(1)))
                  if ( Agrif_probdim .EQ. 2 ) ValSum = 
     &                                 SUM(flag%iarray2(
     &                                 newB%imin(1):newB%imax(1),
     &                                 newB%imin(2):newB%imax(2)))
                  if ( Agrif_probdim .EQ. 3 ) ValSum = 
     &                                 SUM(flag%iarray3(
     &                                 newB%imin(1):newB%imax(1),
     &                                 newB%imin(2):newB%imax(2),
     &                                 newB%imin(3):newB%imax(3)))
C
                  nbpointsflag = nbpointsflag + ValSum
C
                    if ( Agrif_probdim .EQ. 1 ) 
     &                 TailleTab = newB%imax(1)-newB%imin(1)+1
                    if ( Agrif_probdim .EQ. 2 ) 
     &                 TailleTab = (newB%imax(1)-newB%imin(1)+1)*
     &                             (newB%imax(2)-newB%imin(2)+1)
                    if ( Agrif_probdim .EQ. 3 ) 
     &                 TailleTab = (newB%imax(1)-newB%imin(1)+1)*
     &                             (newB%imax(2)-newB%imin(2)+1)*
     &                             (newB%imax(3)-newB%imin(3)+1)

                  nbpoints = nbpoints + TailleTab
C              
                  parcbox2 => parcbox2%next  
                enddo   
C            
                if  (REAL(nbpointsflag)/REAL(nbpoints)
     &               .LT.(1.15*cureff)) then
                    parcbox2 => boxlib            
                    do While (associated(parcbox2))
                      if (associated(parcbox2,tempbox)) Exit
                      deallocate(parcbox2%r)
                      parcbox2 => parcbox2%next             
                    enddo
                    boxlib => tempbox
                    Call Agrif_Add_Rectangle(newB2,boxlib)
                endif
            endif
        endif
        parcbox => parcbox%next
      enddo
C      
C      
      Return
C      
      End Subroutine Agrif_Clusternd 
C
C     **************************************************************************
CCC   Subroutine Agrif_Clusterslice
C     **************************************************************************
C                           
      Subroutine Agrif_Clusterslice(sig,slice,str) 
C
C
CCC   Description:
CCC   
C
CC    Method:
CC            
C
C     Declarations:
C
C     Arguments 
      INTEGER                      :: slice,str
      INTEGER,DIMENSION(slice:str) :: sig 
C
C     Local variables      
      INTEGER                      :: ideb,ifin
      INTEGER                      :: i,t,a,di,ts 
      INTEGER,DIMENSION(slice:str) :: lap 
C
C    
      ideb = slice
      ifin = str
C    
      if (SIZE(sig) <= 2*Agrif_Minwidth) then
          str = -1 
          slice = -1 
          Return
      endif
C      
      t = -1 
      a = -1 
C
      do i = ideb+Agrif_Minwidth-1,ifin-Agrif_Minwidth
          if (sig(i) .EQ. 0) then
              if ((i-ideb) < (ifin-i)) then
                  di = i - ideb
                else 
                  di = ifin - i 
              endif
C
              if (di > t) then
                a = i 
                t = di 
              endif
         endif
      enddo
C      
      if (a .NE. (-1)) then
          slice = a 
          str = t 
          Return
      endif
C      
      t = -1 
      a = -1 
C      
      do i = ideb+1,ifin-1 
        lap(i) = sig(i+1) + sig(i-1) - 2*sig(i) 
      enddo
C      
      do i = ideb + Agrif_Minwidth-1,ifin-Agrif_Minwidth
        if ((lap(i+1)*lap(i)) .LE. 0) then
            ts = ABS(lap(i+1) - lap(i)) 
            if (ts > t) then
                t = ts 
                a = i
            endif
       endif
      enddo
C      
      if (a .EQ. (ideb + Agrif_Minwidth - 1)) then
          a = -1
          t = -1
      endif
C      
      slice = a 
      str = t 
C
C      
      End Subroutine Agrif_Clusterslice       
C
C
C    
C     **************************************************************************
CCC   Subroutine Agrif_Clusterprune
C     **************************************************************************
C      
      Subroutine Agrif_Clusterprune(sig,pl,pu) 
C
C
CCC   Description:
CCC   
C
CC    Method:
CC            
C
C     Declarations:
C
C     Arguments
      INTEGER                  :: pl,pu 
      INTEGER,DIMENSION(pl:pu) :: sig
C
C     Local variables      
      INTEGER :: ideb,ifin       
      INTEGER :: diff,addl,addu,udist,ldist  
C
C      
      ideb = pl
      ifin = pu
C      
      if (SIZE(sig) <= Agrif_Minwidth) then
          return 
      endif 
C       
      do While ((sig(pl) .EQ. 0) .AND. (pl < ifin)) 
        pl = pl + 1 
      enddo 
C       
      do While ((sig(pu) .EQ. 0) .AND. (pu > ideb)) 
        pu = pu - 1 
      enddo 
C       
      if ((pu-pl) < Agrif_Minwidth) then
          diff = Agrif_Minwidth - (pu - pl + 1) 
          udist = ifin - pu 
          ldist = pl - ideb
          addl = diff / 2 
          addu = diff - addl 
          if (addu > udist) then
              addu = udist 
              addl = diff - addu 
          endif
C         
          if (addl > ldist) then
              addl = ldist 
              addu = diff - addl
          endif
C         
          pu = pu + addu 
          pl = pl - addl         
C      
      endif 
C       
C
      End Subroutine Agrif_Clusterprune
C
C
C              
C     **************************************************************************
CCC   Subroutine Agrif_Add_Rectangle 
C     **************************************************************************
C        
      Subroutine Agrif_Add_Rectangle(R,LR)
C
CCC   Description:
CCC   Subroutine to add the Agrif_Rectangle R in a list managed by LR.
C
C     Declarations:
C
C     Arguments               
      TYPE(AGRIF_rectangle)           :: R  
      TYPE(AGRIF_lrectangle), Pointer :: LR
C
C     Local variable        
      TYPE(AGRIF_lrectangle), Pointer :: newrect  
C
      INTEGER                         :: iii 
C
C        
      allocate(newrect)
      allocate(newrect % r)
C
      newrect % r = R
C      
      do iii = 1 , Agrif_Probdim
         newrect % r % spaceref(iii) = Agrif_Coeffref(iii)
         newrect % r % timeref(iii) = Agrif_Coeffreft(iii)
      enddo
C              
      newrect % r % number = -1            
      Nullify(newrect % r % childgrids)
      newrect % next => LR  
      LR => newrect  
C
C      
      End Subroutine Agrif_Add_Rectangle   
C  
C
C  
C     **************************************************************************
CCC   Subroutine Agrif_Read_Fix_Grd  
C     **************************************************************************
C      
      Recursive Subroutine Agrif_Read_Fix_Grd(coarsegrid,j,nunit)
C
CCC   Description:
CCC   Subroutine to create the grid hierarchy from the reading of the 
CCC   AGRIF_FixedGrids.in file.
C
CC    Method:
CC    Recursive subroutine and creation of a first grid hierarchy from the
CC    reading of the AGRIF_FixedGrids.in file.        
C
C     Declarations:
C
C     Pointer argument      
      TYPE(AGRIF_rectangle), Pointer   :: coarsegrid ! Pointer on the first grid
                                                     ! of the grid hierarchy
C                                    
C     Scalar arguments                                    
      INTEGER                          :: j          ! Number of the new grid
      INTEGER                          :: nunit      ! unit associated with file
C      
C     Local variables      
      TYPE(AGRIF_rectangle)            :: newrect    ! Pointer on a new grid   
      TYPE(AGRIF_lrectangle), Pointer  :: parcours   ! Pointer for the recursive
                                                     !    procedure
      TYPE(AGRIF_lrectangle), Pointer  :: newlrect
      TYPE(AGRIF_lrectangle), Pointer  :: end_list       
      INTEGER                          :: i          ! for each child grid
      INTEGER                          :: nb_grids   ! Number of child grids
      INTEGER                          :: iii
C          
C
      Nullify(newrect%childgrids)
C      
C     Reading of the number of child grids        
      read(nunit,*) nb_grids
C      
C     coarsegrid%nbgridchild = nb_grids
C      
      allocate(end_list)
C
      nullify(end_list % r)
      nullify(end_list % next)
C      
      coarsegrid % childgrids => end_list
C
C     Reading of imin(1),imax(1),imin(2),imax(2),imin(3),imax(3), and space and
C        time refinement factors for each child grid.
C     Creation and addition of the new grid to the grid hierarchy. 
C
      do i = 1,nb_grids
        allocate(newlrect)      
        newrect % number = j   ! Number of the grid
C
        if ( Agrif_USE_ONLY_FIXED_GRIDS .EQ. 0 ) then
           if (Agrif_Probdim == 3) then  
            read(nunit,*) newrect % imin(1), newrect % imax(1),
     &                    newrect % imin(2), newrect % imax(2),
     &                    newrect % imin(3), newrect % imax(3),
     &                    newrect % spaceref(1),newrect % spaceref(2),
     &                    newrect % spaceref(3),
     &                    newrect % timeref(1),newrect % timeref(2),
     &                    newrect % timeref(3)
           elseif (Agrif_Probdim == 2) then            
            read(nunit,*) newrect % imin(1),newrect % imax(1),
     &                  newrect % imin(2),newrect % imax(2),
     &                    newrect % spaceref(1),newrect % spaceref(2),
     &                    newrect % timeref(1),newrect % timeref(2)
           elseif (Agrif_Probdim == 1) then
            read(nunit,*) newrect % imin(1), newrect % imax(1),
     &                    newrect % spaceref(1),
     &                    newrect % timeref(1)
           endif
        else
           if (Agrif_Probdim == 3) then  
            read(nunit,*) newrect % imin(1), newrect % imax(1),
     &                    newrect % imin(2), newrect % imax(2),
     &                    newrect % imin(3), newrect % imax(3),
     &                    newrect % spaceref(1),newrect % spaceref(2),
     &                    newrect % spaceref(3),
     &                    newrect % timeref(1)
           elseif (Agrif_Probdim == 2) then            
            read(nunit,*) newrect % imin(1),newrect % imax(1),
     &                  newrect % imin(2),newrect % imax(2),
     &                    newrect % spaceref(1),newrect % spaceref(2),
     &                    newrect % timeref(1)
           elseif (Agrif_Probdim == 1) then
            read(nunit,*) newrect % imin(1), newrect % imax(1),
     &                    newrect % spaceref(1),
     &                    newrect % timeref(1)
           endif
C
           if ( Agrif_probdim .GE. 2 ) then
              do iii = 2 , Agrif_probdim
                 newrect % timeref(iii) = newrect % timeref(1) 
              enddo
           endif
C
        endif
C
C       Addition to the grid hierarchy
C
        nullify(newrect % childgrids)
        j = j + 1
        Allocate(newlrect%r)
        newlrect % r = newrect 
        nullify(newlrect % next)
        end_list % next => newlrect
        end_list => end_list % next
      enddo
C      
      coarsegrid % childgrids => coarsegrid % childgrids % next
      parcours => coarsegrid % childgrids
C
C     Recursive operation to create the grid hierarchy branch by branch
C
      do while (associated(parcours))
        call Agrif_Read_Fix_Grd (parcours % r,j,nunit)
        parcours => parcours % next
      enddo
C      
C
      End Subroutine Agrif_Read_Fix_Grd        
C        
C 
C
C     **************************************************************************
CCC   Subroutine Agrif_Create_Grids  
C     **************************************************************************
C
      Recursive Subroutine Agrif_Create_Grids(g,coarsegrid)
C
CCC   Description:
CCC   Subroutine to create the grid hierarchy (g) from the one created with the
CCC   Agrif_Read_Fix_Grd or Agrif_Cluster_All procedures (coarsegrid).
C
CC    Method:
CC    Recursive subroutine.        
C
C     Declarations:
C
C     Pointer arguments        
      TYPE(AGRIF_grid)     , Pointer  :: g          ! Pointer on the root coarse
                                                    ! grid
      TYPE(AGRIF_rectangle), Pointer  :: coarsegrid ! Pointer on the root coarse
                                                    ! grid of the grid hierarchy
                                                    ! created with the
                                                    ! Agrif_Read_Fix_Grd
                                                    ! subroutine
C
C     Local pointers
      TYPE(Agrif_grid)      , Pointer :: newgrid    ! New grid
      TYPE(Agrif_pgrid)     , Pointer :: newpgrid
      TYPE(Agrif_pgrid)     , Pointer :: parcours2
      TYPE(Agrif_lrectangle), Pointer :: parcours
      TYPE(Agrif_pgrid)     , Pointer :: end_list
      TYPE(Agrif_pgrid)     , Pointer :: parcours3
C
C     Local scalars      
      LOGICAL                         :: nullliste
      INTEGER                         :: iii
      INTEGER                         :: moving_grid_id = 0      

C
      parcours3 => g % child_grids
C     
      if (associated(parcours3)) then
          do While (associated(parcours3 % next))
            parcours3 => parcours3 % next
          enddo
          end_list => parcours3
          nullliste=.FALSE.
        else
          allocate(end_list)
          nullify(end_list % gr)
          nullify(end_list % next)      
          g % child_grids => end_list 
          parcours3 => end_list      
          nullliste=.TRUE.
      endif
C      
      parcours => coarsegrid % childgrids
C      
C     Creation of the grid hierarchy from the one created by using the 
C     Agrif_Read_Fix_Grd subroutine 
C
      do while (associated(parcours))
        allocate(newgrid)        
        moving_grid_id=moving_grid_id+1
        newgrid % grid_id = moving_grid_id
        do iii = 1 , Agrif_Probdim
           newgrid % spaceref(iii) = parcours % r % spaceref(iii)
           newgrid % timeref(iii) = parcours % r % timeref(iii)
        enddo
C
        do iii = 1 , Agrif_Probdim
          newgrid % nb(iii) = (parcours % r % imax(iii) 
     &                       - parcours % r % imin(iii)) 
     &                       * parcours % r % spaceref(iii)
C     
          newgrid % ix(iii) =  parcours % r % imin(iii)
C
          newgrid % Agrif_d(iii) = g % Agrif_d(iii) 
     &                  / REAL(newgrid % spaceref(iii))
C
          newgrid % Agrif_x(iii) = g % Agrif_x(iii) +
     &      (parcours % r % imin(iii) - 1)* g % Agrif_d(iii)
C      
        enddo
C
C       Pointer on the parent grid                        
C
        newgrid % parent => g      

C       Level of the current grid
        newgrid % level = newgrid % parent % level + 1
        if (newgrid % level > Agrif_MaxLevelLoc) then
          Agrif_MaxLevelLoc = newgrid%level
        endif

C      
C       Grid pointed by newgrid is a fixed grid      
C
        if (parcours % r % number .GT. 0) then      
            newgrid % fixed = .true.  
          else
            newgrid % fixed = .false.
        endif
C
C       Number of the grid pointed by newgrid
        newgrid % fixedrank = parcours % r % number
C           
C       No time calculation on this grid         
        newgrid % ngridstep = 0      
C
C       Test indicating if the current grid has a common border with the root 
C       coarse grid in the x direction
        do iii = 1 , Agrif_Probdim
           newgrid % NearRootBorder(iii) = .FALSE.
C        
           if ((newgrid % parent % NearRootBorder(iii)) .AND. 
     &         (newgrid % ix(iii) == 1)) then
               newgrid % NearRootBorder(iii) = .TRUE.
           endif
C
           newgrid % DistantRootBorder(iii) = .FALSE.
C 
           if ((newgrid % parent % DistantRootBorder(iii)) .AND. 
     &         (newgrid % ix(iii) + 
     &         (newgrid % nb(iii)/newgrid % spaceref(iii))  
     &          - 1  == newgrid % parent % nb(iii))) then
               newgrid % DistantRootBorder(iii) = .TRUE.
           endif
        enddo
C
C       Writing in output files
C
        newgrid % oldgrid = .FALSE.      
C
C     
C       Definition of the CHARACTERistics of the variable of the grid pointed by
C       newgrid 
        Call Agrif_Create_Var (newgrid)
C      
C       Instanciation of the grid pointed by newgrid and its variables
        Call Agrif_Instance (newgrid)
C       
C       Nullify the variable of the grid pointed by newgrid
C
C
C       Addition of this grid to the grid hierarchy 
C      
        nullify(newgrid % child_grids)
        allocate(newpgrid)  
        newpgrid % gr => newgrid
        nullify(newpgrid % next)
        end_list % next => newpgrid
        end_list => end_list % next               
        parcours => parcours % next
C
C       Updating the total number of fixed grids
        if (newgrid % fixed) then
            AGRIF_nbfixedgrids = AGRIF_nbfixedgrids + 1
        endif
C                  
      enddo
C
C
      if (nullliste) then
          g % child_grids => g % child_grids % next
          parcours2 => g % child_grids
          deallocate(parcours3)
        else
          parcours2 => parcours3 % next
      endif
C      
      parcours => coarsegrid % childgrids
C
C     Recursive call to the subroutine Agrif_Create_Fixed_Grids to create the
C     grid hierarchy
C      
      do while (associated(parcours))
        Call Agrif_Create_Grids (parcours2 % gr,parcours % r)
        parcours => parcours % next
        parcours2 => parcours2 % next
      enddo 
C      
      Return      
C                  
      End Subroutine Agrif_Create_Grids
C
C      
C
C     **************************************************************************
CCC   Subroutine Agrif_Init_Hierarchy  
C     **************************************************************************
C
      Recursive Subroutine Agrif_Init_Hierarchy(g) 
C
CCC   Description:
CCC   Subroutine to initialize all the grids except the root coarse grid (this 
CCC   one, pointed by AGRIF_mygrid, is initialized by the subroutine 
CCC   Agrif_Init_Grids defi ned in the module Agrif_Util and called in the main 
CCC   program ). 
C
CC    Method:
CC    Recursive subroutine.        
C
C     Declarations:
C
C     Pointer argument      
      TYPE(AGRIF_grid), Pointer  :: g         ! Pointer on the root coarse grid 
C      
C     Local variables      
      TYPE(AGRIF_pgrid), Pointer :: parcours  ! Pointer for the recursive call
      LOGICAL                    :: Init_Hierarchy
C      
C
      parcours=>g%child_grids
C      
      do while (associated(parcours))
        Init_Hierarchy = .false.
        if ( AGRIF_USE_FIXED_GRIDS .EQ. 1 .OR. 
     &       AGRIF_USE_ONLY_FIXED_GRIDS .EQ. 1 ) then
           if ((parcours%gr%fixed) 
     &         .AND. (Agrif_mygrid%ngridstep == 0)) then
              Init_Hierarchy = .true.
           endif      
       endif
C
       if (.NOT. parcours%gr%fixed) Init_Hierarchy = .true.
       if (parcours % gr % oldgrid) Init_Hierarchy = .false.
C
       if (Init_Hierarchy) then 
C
C           Instanciation of the grid pointed by parcours%gr and its variables 
            Call Agrif_Instance (parcours % gr)
C    
C           Allocation of the arrays containing values of the variables of the
C           grid pointed by parcours%gr      
C      
            Call Agrif_Allocation (parcours % gr)   
C            
            Call Agrif_initialisations(parcours % gr)              
C       
            Call Agrif_Instance(parcours % gr)
C
            if ( Agrif_USE_ONLY_FIXED_GRIDS .EQ. 0 ) then
              Call Agrif_Allocate_Restore (parcours % gr)
            endif
C
            if ( Agrif_USE_ONLY_FIXED_GRIDS .EQ. 0 ) then
C              Initialization by copy of the grids created by clustering 
               Call AGRIF_CopyFromold_All (parcours%gr,
     &                                     Agrif_oldmygrid)
            endif
C
C           Initialization by interpolation 
C           (this routine is written by the user) 
            Call Agrif_InitValues()
C
            if ( Agrif_USE_ONLY_FIXED_GRIDS .EQ. 0 ) then
               Call Agrif_Free_Restore (parcours % gr)
            endif
C
       endif                                  
       parcours => parcours % next 
C         
      enddo
C
      parcours => g % child_grids
C      
C     Recursive operation to initialize all the grids
      do while (associated(parcours))
        Call Agrif_Init_Hierarchy (parcours%gr)
        parcours => parcours%next
      enddo
C      
      End Subroutine Agrif_Init_Hierarchy 
C
C     **************************************************************************
CCC   Subroutine Agrif_Allocate_Restore  
C     **************************************************************************
C
      Subroutine Agrif_Allocate_Restore(Agrif_Gr)
C      
C      
C     Modules used:
C
      TYPE(AGRIF_grid), Pointer  :: Agrif_Gr   ! Pointer on the root coarse grid
C     
      INTEGER                    :: i
C
        do i = 1 , Agrif_NbVariables
            if ( Agrif_Mygrid%tabvars(i)%var % restaure ) then
            if ( Agrif_Gr%tabvars(i)%var % nbdim .EQ. 1 ) then    
            Allocate( Agrif_Gr%tabvars(i)%var%  
     &    Restore1D(lbound(Agrif_Gr%tabvars(i)%var%array1,1) 
     &    :ubound(Agrif_Gr%tabvars(i)%var%array1,1))) 
            Agrif_Gr%tabvars(i)%var%Restore1D = 0
C
            endif
            if ( Agrif_Gr%tabvars(i)%var % nbdim .EQ. 2 ) then
            Allocate( Agrif_Gr%tabvars(i)%var%Restore2D(  
     &      lbound(Agrif_Gr%tabvars(i)%var%array2,1):         
     &      ubound(Agrif_Gr%tabvars(i)%var%array2,1),         
     &      lbound(Agrif_Gr%tabvars(i)%var%array2,2)          
     &      :ubound(Agrif_Gr%tabvars(i)%var%array2,2)))         
            Agrif_Gr%tabvars(i)%var%Restore2D = 0
C
            endif
            if ( Agrif_Mygrid%tabvars(i)%var % nbdim .EQ. 3 ) then
C     
            Allocate( Agrif_Gr%tabvars(i)%var%Restore3D( 
     &      lbound(Agrif_Gr%tabvars(i)%var%array3,1):        
     &      ubound(Agrif_Gr%tabvars(i)%var%array3,1),        
     &      lbound(Agrif_Gr%tabvars(i)%var%array3,2):        
     &      ubound(Agrif_Gr%tabvars(i)%var%array3,2),        
     &      lbound(Agrif_Gr%tabvars(i)%var%array3,3):        
     &      ubound(Agrif_Gr%tabvars(i)%var%array3,3)))  
            Agrif_Gr%tabvars(i)%var%Restore3D = 0
            endif
C 
            endif
          enddo
C
      Return
C
C      
      End Subroutine Agrif_Allocate_Restore      
C
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Free_Restore  
C     **************************************************************************
C
      Subroutine Agrif_Free_Restore(Agrif_Gr)
C
C      
C     Pointer argument      
      TYPE(AGRIF_grid), Pointer  :: Agrif_Gr   ! Pointer on the root coarse grid
      INTEGER :: i     
C
      do i = 1 , Agrif_NbVariables
         if ( Agrif_Mygrid % tabvars(i) % var % restaure) then
C  
            if (associated(Agrif_Gr%tabvars(i)%var%Restore1D)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%Restore1D)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%Restore2D)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%Restore2D)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%Restore3D)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%Restore3D)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%Restore4D)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%Restore4D)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%Restore5D)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%Restore5D)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%Restore6D)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%Restore6D)
            endif
C 
        endif
      enddo
C 
      Return
C
C      
      End Subroutine Agrif_Free_Restore
C
C
C
      End Module Agrif_Clustering
