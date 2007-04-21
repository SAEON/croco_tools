!
! $Id: modcurgridfunctions.F,v 1.4 2005/08/22 15:11:29 agrif Exp $
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
C     Foundation, Inc., 59 Temple Place-  Suite 330, Boston, MA 02111-1307, USA.
C
C
C
CCC   Module Agrif_CurgridFunctions      
C      
      Module Agrif_CurgridFunctions    
C  
CCC   Description:
CCC   Module to define some procedures concerning the current grid
C
C     Modules used: 
C
      Use Agrif_Init
C
      IMPLICIT NONE
C
C
      Contains
C     Define procedures contained in this module
C
C     **************************************************************************
CCC   Function Agrif_Rhot
C     **************************************************************************
C
      Function Agrif_Rhot()
C  
CCC   Description:
CCC   Function returning the time refinement factor of the current grid.
C
C     Declarations:
C  
      
C
      REAL :: Agrif_Rhot  ! Result
C
C     Local scalar      
      INTEGER :: res         ! Intermediate result
      INTEGER :: iii
C
C
      res=1
C
      do iii = 1 , Agrif_Probdim
         res = max(res, AGRIF_Curgrid % timeref(iii))
      enddo
C
      Agrif_Rhot = float(res)
C
C
      End function Agrif_rhot
C
C
C
C
C     **************************************************************************
CCC   Function Agrif_IRhot
C     **************************************************************************
C
      Function Agrif_IRhot()
C  
CCC   Description:
CCC   Function returning the time refinement factor of the current grid.
C
C     Declarations:
C  
      
C
      INTEGER :: Agrif_IRhot  ! Result
C
C     Local scalar      
      INTEGER :: res         ! Intermediate result
      INTEGER :: iii
C
C
      res=1
C
      do iii = 1 , Agrif_Probdim
         res = max(res, AGRIF_Curgrid % timeref(iii))
      enddo
C
      Agrif_IRhot = res
C
C
      End function Agrif_IRhot
C
C
C
C     **************************************************************************
CCC   Function Agrif_Parent_Rhot
C     **************************************************************************
C
      Function Agrif_Parent_Rhot()
C  
CCC   Description:
CCC   Function returning the time refinement factor of the parent grid of the 
CCC   current grid.
C
C     Declarations:
C  
      
C
      REAL :: Agrif_Parent_Rhot  ! Result
C
C     Local scalar      
      INTEGER :: res         ! Intermediate result
      INTEGER :: iii
C
C
      res=1
C
      do iii = 1 , Agrif_Probdim
         res = max(res, AGRIF_Curgrid % parent % timeref(iii))
      enddo
C
      Agrif_Parent_Rhot = float(res)
C
C
      End function Agrif_Parent_Rhot      
C
C
C     **************************************************************************
CCC   Function Agrif_Parent_IRhot
C     **************************************************************************
C
      Function Agrif_Parent_IRhot()
C  
CCC   Description:
CCC   Function returning the time refinement factor of the parent grid of the 
CCC   current grid.
C
C     Declarations:
C  
      
C
      INTEGER :: Agrif_Parent_IRhot  ! Result
C
C     Local scalar      
      INTEGER :: res         ! Intermediate result
      INTEGER :: iii
C
C
      res=1
C
      do iii = 1 , Agrif_Probdim
         res = max(res, AGRIF_Curgrid % parent % timeref(iii))
      enddo
C
      Agrif_Parent_IRhot = res
C
C
      End function Agrif_Parent_IRhot      
C
C
C     **************************************************************************
CCC   Function Agrif_Nbstepint
C     **************************************************************************
C
      Function Agrif_Nbstepint()
C  
CCC   Description:
CCC   Function for the calculation of the coefficients used for the time 
CCC   interpolation (module Agrif_Boundary).
C
C     Declarations:
C  
      
C            
      INTEGER :: Agrif_nbstepint  ! result
C
C
      Agrif_nbstepint = mod(AGRIF_CURGRID % ngridstep,
     &                      int(AGRIF_rhot()))
C
C
      End function Agrif_Nbstepint
C
C
C
CC    **************************************************************************
CCC   Function Agrif_Parent_Nbstepint
C     **************************************************************************
C
      Function Agrif_Parent_Nbstepint()
C  
CCC   Description:
CCC   Function for the calculation of the coefficients used for the time 
CCC   interpolation (module Agrif_Boundary).
C
C     Declarations:
C  
      
C            
      INTEGER :: Agrif_Parent_Nbstepint  ! result
C
C
      Agrif_Parent_Nbstepint = mod(AGRIF_CURGRID % parent % ngridstep, 
     &                             int(AGRIF_Parent_Rhot()))
C
C
      End function Agrif_Parent_Nbstepint
C
C     **************************************************************************
CCC   Subroutine Agrif_InterpNearBorderX
C     **************************************************************************
C
      Subroutine Agrif_InterpNearBorderX()
C  
CCC   Description:
CCC   Subroutine allowing to interpole (in the x direction) on a near border of 
CCC   the current grid if this one has a common border with the root coarse 
CCC   grid.   
C
C     Declarations:
C  
      
C
C
      AGRIF_CURGRID % NearRootBorder(1) = .FALSE.             
C
C
      End Subroutine Agrif_InterpNearBorderX 
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_InterpDistantBorderX
C     **************************************************************************
C
      Subroutine Agrif_InterpDistantBorderX()
C  
CCC   Description:
CCC   Subroutine allowing to interpole (in the x direction) on a distant border 
CCC   of the current grid if this one has a common border with the root coarse  
CCC   grid.
C
C     Declarations:
C  

C
C
      AGRIF_CURGRID % DistantRootBorder(1) = .FALSE.             
C
C
      End Subroutine Agrif_InterpDistantBorderX                 
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_InterpNearBorderY
C     **************************************************************************
C
      Subroutine Agrif_InterpNearBorderY()
C  
CCC   Description:
CCC   Subroutine allowing to interpole (in the y direction) on a near border of 
CCC   the current grid if this one has a common border with the root coarse 
CCC   grid.
C
C     Declarations:
C  
      
C
C
      AGRIF_CURGRID % NearRootBorder(2) = .FALSE.             
C
C
      End Subroutine Agrif_InterpNearBorderY
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_InterpDistantBorderY
C     **************************************************************************
C
      Subroutine Agrif_InterpDistantBorderY()
C  
CCC   Description:
CCC   Subroutine allowing to interpole (in the y direction) on a distant border 
CCC   of the current grid if this one has a common border with the root coarse  
CCC   grid.
C
C     Declarations:
C  
      
C
C
      AGRIF_CURGRID % DistantRootBorder(2) = .FALSE.             
C
C
      End Subroutine Agrif_InterpDistantBorderY      
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_InterpNearBorderZ
C     **************************************************************************
C
      Subroutine Agrif_InterpNearBorderZ()
C  
CCC   Description:
CCC   Subroutine allowing to interpole (in the z direction) on a near border of 
CCC   the current grid if this one has a common border with the root coarse 
CCC   grid.
C
C     Declarations:
C  
      
C
C
      AGRIF_CURGRID % NearRootBorder(3) = .FALSE.             
C
C
      End Subroutine Agrif_InterpNearBorderZ
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_InterpDistantBorderZ
C     **************************************************************************
C
      Subroutine Agrif_InterpDistantBorderZ()
C  
CCC   Description:
CCC   Subroutine allowing to interpole (in the z direction) on a distant border 
CCC   of the current grid if this one has a common border with the root coarse  
CCC   grid.
C
C     Declarations:
C  
      
C
C
      AGRIF_CURGRID % DistantRootBorder(3) = .FALSE.             
C
C
      End Subroutine Agrif_InterpDistantBorderZ
C 
C     **************************************************************************
CCC   Function Agrif_Parent_Nb_Step
C     ************************************************************************** 
C 
      Function AGRIF_Parent_Nb_Step() 
C  
CCC   Description:
CCC   Function returning the number of time steps of the parent grid of the
CCC   current grid.
C
C     Declarations:
C    
      
C
      INTEGER :: AGRIF_Parent_Nb_Step ! Result 
C
C
      if (Agrif_Root()) then  
C     
          Agrif_Parent_Nb_Step = -1 
C       
        else
C       
          Agrif_Parent_Nb_Step = Agrif_Curgrid % parent % ngridstep
C     
      endif
C
C     
      End function Agrif_Parent_Nb_Step    
C 
C
C 
C     **************************************************************************
CCC   Function Agrif_Root
C     **************************************************************************
C
      Function Agrif_Root()
C     
CCC   Description:
CCC   Function indicating if the current grid is or not the root grid.
C
C     Declarations:
C
      
C
      LOGICAL :: Agrif_Root   ! Result
C    
C
      if (AGRIF_CURGRID % fixedrank .EQ. 0) then
C          
          Agrif_Root = .TRUE.
C    
        else
C    
          Agrif_Root = .FALSE.
C     
      endif
C
C    
      End function Agrif_Root
C
C
C
C     **************************************************************************
CCC   Function Agrif_Parent_Root
C     **************************************************************************
C
      Function Agrif_Parent_Root()
C     
CCC   Description:
CCC   Function indicating if the parent grid of the current grid is or not the 
CCC   root grid.
C
C     Declarations:
C
      
C
      LOGICAL :: Agrif_Parent_Root   ! Result
C    
C
      if (AGRIF_CURGRID % parent % fixedrank .EQ. 0) then
C          
          Agrif_Parent_Root = .TRUE.
C    
        else
C    
          Agrif_Parent_Root = .FALSE.
C     
      endif
C
C    
      End function Agrif_Parent_Root     
C 
C
C 
C     **************************************************************************
CCC   Function Agrif_Fixed
C     **************************************************************************
C 
      Function Agrif_Fixed()
C  
CCC   Description:
CCC   Function returning the number of the current grid.
C
C     Declarations:
C   
      
C
      INTEGER Agrif_Fixed   ! Result 
C 
C
      if (Agrif_Curgrid % fixed) then
C     
          Agrif_Fixed = Agrif_Curgrid % fixedrank 
C
        else
C     
         Agrif_Fixed = -1
C        
      endif
C
C     
      End function Agrif_Fixed 
C
C
C
C     **************************************************************************
CCC   Function Agrif_Parent_Fixed
C     **************************************************************************
 
      Function Agrif_Parent_Fixed()
C  
CCC   Description:
CCC   Function returning the number of the parent grid of the current grid.
C
C     Declarations:
C   
      
C
      INTEGER Agrif_Parent_Fixed   ! Result 
C 
C
      if (Agrif_Curgrid % parent % fixed) then
C     
          Agrif_Parent_Fixed = AGRIF_CURGRID % parent % fixedrank 
C
        else
C     
         Agrif_Parent_Fixed = 0
C        
      endif
C
C     
      End function Agrif_Parent_Fixed
C
C
C
C     **************************************************************************
CCC   Function Agrif_Is_Fixed
C     **************************************************************************
 
      Function Agrif_Is_Fixed()
C  
CCC   Description:
CCC   Function returning true if the current grid is fixed.
C
C     Declarations:
C   
      
C
      LOGICAL Agrif_Is_Fixed   ! Result 
C 
C
      if (Agrif_Curgrid % fixed) then
C     
          Agrif_Is_Fixed = .true.
C
        else
C     
          Agrif_Is_Fixed = .false.
C        
      endif
C
C     
      End function Agrif_Is_Fixed
C
C
C
C     **************************************************************************
CCC   Function Agrif_Parent_Is_Fixed
C     **************************************************************************
 
      Function Agrif_Parent_Is_Fixed()
C  
CCC   Description:
CCC   Function returning true if the parent grid of the current grid is fixed.
C
C     Declarations:
C   
      
C
      LOGICAL Agrif_Parent_Is_Fixed   ! Result 
C 
C
      if (Agrif_Curgrid % parent % fixed) then
C     
          Agrif_Parent_Is_Fixed = .true.
C
        else
C     
          Agrif_Parent_Is_Fixed = .false.
C        
      endif
C
C     
      End function Agrif_Parent_Is_Fixed            
C
C 
C
C     **************************************************************************
CCC   Function AGRIF_CFixed
C     **************************************************************************
 
            Function AGRIF_CFixed()
C  
CCC   Description:
CCC   Function returning the number of the current grid.
C
C     Declarations:
C   
      
C
      CHARACTER(3) AGRIF_CFixed   ! Result 
C
C     Local variables      
      CHARACTER(3) cfixed
      INTEGER fixed
C 
C
      fixed = Agrif_Fixed()
C      
      if(fixed.NE.-1) then
C
           if (fixed .LE. 9) then
C
               write(cfixed,'(i1)')fixed
C 
           else
C   
               write(cfixed,'(i2)')fixed
C      
           endif
C
           AGrif_Cfixed=cfixed
C      
      else
C
      print*,'Call to AGRIF_CFixed() on a moving grid'
      stop
C
      endif     

      End function AGRIF_CFixed
C
C
C
C     **************************************************************************
CCC   Function AGRIF_Parent_CFixed
C     **************************************************************************
 
      Function AGRIF_Parent_CFixed()
C  
CCC   Description:
CCC   Function returning the number of the parent grid of the current grid.
C
C     Declarations:
C   
      
C
      CHARACTER(3) AGRIF_Parent_CFixed   ! Result 
C
C     Local variables      
      CHARACTER(3) cfixed
      INTEGER fixed
C 
C
      fixed = Agrif_Parent_Fixed()
C      
      if(fixed.NE.-1) then
C
          if (fixed .LE. 9) then
C
              write(cfixed,'(i1)')fixed
C      
          else
C        
              write(cfixed,'(i2)')fixed
C      
          endif
C
          AGrif_Parent_Cfixed=cfixed
C
      else
C
      print*,'Illegal call to AGRIF_Parent_CFixed()'
      stop     
C
      endif

      End function AGRIF_Parent_CFixed      
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_ChildGrid_to_ParentGrid
C     **************************************************************************
C
      Subroutine Agrif_ChildGrid_to_ParentGrid()
C  
CCC   Description:
CCC   Subroutine allowing to make the pointer AGRIF_CURGRID point on the parent
CCC   grid of the current grid.
C
C     Declarations:
C  
      
C
C
      AGRIF_saveCURGRID => AGRIF_CURGRID
C
      Call AGRIF_INSTANCE(AGRIF_CURGRID%parent)            
C
C
      End Subroutine Agrif_ChildGrid_to_ParentGrid
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_ParentGrid_to_ChildGrid
C     **************************************************************************
C
      Subroutine Agrif_ParentGrid_to_ChildGrid()
C  
CCC   Description:
CCC   Subroutine allowing to make the pointer AGRIF_CURGRID point on the child 
CCC   grid after having called the Agrif_ChildGrid_to_ParentGrid subroutine.
C
C     Declarations:
C  
      
C
C
      Call AGRIF_INSTANCE(AGRIF_saveCURGRID)            
C
C
      End Subroutine Agrif_ParentGrid_to_ChildGrid
C
C
C
C     **************************************************************************
CCC   Function Agrif_Get_Unit
C     **************************************************************************
C
      Function Agrif_Get_Unit()

CCC   Description : return a unit not connected to any file
C
C     Declarations
C
      
C
      INTEGER Agrif_Get_Unit
C
C     Local scalars
      INTEGER n
      LOGICAL op
C
      INTEGER :: nunit
      INTEGER :: iii,out,iiimax
      Logical :: BEXIST
      INTEGER,DIMENSION(1:10) :: ForbiddenUnit
C
C
C     Load forbidden Unit if the file Agrif_forbidenUnit exist
C      

      INQUIRE(FILE='Agrif_forbiddenUnit.txt',EXIST=BEXIST)
      If (.not. BEXIST) Then
c          File Agrif_forbiddenUnit.txt not found
      Else
           nunit = 777
           open(nunit,file='Agrif_forbiddenUnit.txt',form='formatted',
     &          status="old")
              iii = 1
              do while ( .TRUE. )
                 read(nunit,*,END = 99) ForbiddenUnit(iii)
                 iii = iii + 1
              enddo
   99         CONTINUE
              iiimax = iii
           close(nunit)
      endif     
C 
      do n = 7,1000
C 
        Inquire(Unit=n,Opened=op)
C
        out = 0
        if ( BEXIST .AND. .NOT.op) then
           do iii = 1 , iiimax
              if ( n .EQ. ForbiddenUnit(iii) ) out = 1
           enddo
        endif
C
        if (.NOT.op .AND. out .EQ. 0) exit
C      
      enddo
C
      Agrif_Get_Unit=n
C
C
      End Function Agrif_Get_Unit
      
      Subroutine Agrif_Set_Efficiency(eff)
      REAL :: eff
      
      IF ((eff.LT.0.).OR.(eff.GT.1)) THEN
      write(*,*)'Error Efficiency should be between 0 and 1'
      stop
      ELSE
      Agrif_efficiency = eff
      ENDIF
      End Subroutine Agrif_Set_Efficiency
      
      Subroutine Agrif_Set_Regridding(regfreq)
      INTEGER :: regfreq
      
      IF (regfreq.LT.0) THEN
      write(*,*)'Regridding frequency should be positive'
      stop
      ELSE
      Agrif_regridding = regfreq
      ENDIF
      End Subroutine Agrif_Set_Regridding
C
      End Module Agrif_CurgridFunctions 
