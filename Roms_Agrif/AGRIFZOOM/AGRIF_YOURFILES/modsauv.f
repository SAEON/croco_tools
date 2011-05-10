

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
CCC   Module Agrif_Save  
C
      Module Agrif_Save 
C
CCC   Description:   
CCC   Module for the initialization by copy of the grids created by clustering.
C
C     Modules used:
C      
      Use Agrif_Types
      Use Agrif_Link
      Use Agrif_Arrays
C
      IMPLICIT NONE
C
      Contains
C     Definition of procedures contained in this module.
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Free_data_before
C     **************************************************************************
C
      Subroutine Agrif_Free_data_before(Agrif_Gr)
C
CCC   Description:
CCC   
C
CC    Method:
CC            
C
C     Declarations:
C
      
C      
C     Pointer argument    
      TYPE(Agrif_Grid),pointer   :: Agrif_Gr ! Pointer on the current grid
      INTEGER i
C
C   
      do i = 1 , AGRIF_NbVariables
         if ( .NOT. Agrif_Mygrid % tabvars(i) % var % restaure) then
C 
            if (associated(Agrif_Gr%tabvars(i)%var%array1)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%array1)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%array2)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%array2)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%array3)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%array3)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%array4)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%array4)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%array5)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%array5)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%array6)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%array6)
            endif
C
            if (associated(Agrif_Gr%tabvars(i)%var%darray1)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%darray1)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%darray2)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%darray2)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%darray3)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%darray3)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%darray4)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%darray4)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%darray5)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%darray5)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%darray6)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%darray6)
            endif
C
            if (associated(Agrif_Gr%tabvars(i)%var%larray1)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%larray1)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%larray2)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%larray2)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%larray3)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%larray3)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%larray4)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%larray4)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%larray5)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%larray5)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%larray6)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%larray6)
            endif
C
            if (associated(Agrif_Gr%tabvars(i)%var%iarray1)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%iarray1)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%iarray2)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%iarray2)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%iarray3)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%iarray3)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%iarray4)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%iarray4)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%iarray5)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%iarray5)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%iarray6)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%iarray6)
            endif
C
            if (associated(Agrif_Gr%tabvars(i)%var%carray1)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%carray1)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%carray2)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%carray2)
            endif
C
            if (associated(Agrif_Gr%tabvars(i)%var%oldvalues2D)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%oldvalues2D)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%interpIndex)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%interpIndex)
            endif
            
            if (associated(Agrif_Gr%tabvars(i)%var%posvar)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%posvar)
            endif     
            
            if (associated(Agrif_Gr%tabvars(i)%var%interptab)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%interptab)
            endif  
                             
C
            Deallocate(Agrif_Gr%tabvars(i)%var)
C 
        endif
      enddo
C
C 
C
      if ( Agrif_USE_ONLY_FIXED_GRIDS .EQ. 0 ) then
         if ( Agrif_Probdim .EQ. 1 ) Deallocate(Agrif_Gr%tabpoint1D)
         if ( Agrif_Probdim .EQ. 2 ) Deallocate(Agrif_Gr%tabpoint2D)
         if ( Agrif_Probdim .EQ. 3 ) Deallocate(Agrif_Gr%tabpoint3D)
      endif
C          
      Return
C
C
      End Subroutine Agrif_Free_data_before
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Free_data_after
C     **************************************************************************
C      
      Subroutine Agrif_Free_data_after(Agrif_Gr)
C
CCC   Description:
CCC   
C
CC    Method:
CC            
C
C     Declarations:
C
C
      
C
C     Pointer argument    
      TYPE(Agrif_Grid),pointer   :: Agrif_Gr  ! Pointer on the current grid
      INTEGER i
C
C      
      do i = 1 , AGRIF_NbVariables
         if ( Agrif_Mygrid % tabvars(i) % var % restaure) then
C 
            if (associated(Agrif_Gr%tabvars(i)%var%array1)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%array1)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%array2)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%array2)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%array3)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%array3)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%array4)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%array4)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%array5)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%array5)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%array6)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%array6)
            endif
!
            if (associated(Agrif_Gr%tabvars(i)%var%darray1)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%darray1)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%darray2)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%darray2)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%darray3)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%darray3)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%darray4)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%darray4)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%darray5)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%darray5)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%darray6)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%darray6)
            endif
!
            if (associated(Agrif_Gr%tabvars(i)%var%larray1)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%larray1)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%larray2)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%larray2)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%larray3)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%larray3)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%larray4)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%larray4)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%larray5)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%larray5)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%larray6)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%larray6)
            endif
!
            if (associated(Agrif_Gr%tabvars(i)%var%iarray1)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%iarray1)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%iarray2)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%iarray2)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%iarray3)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%iarray3)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%iarray4)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%iarray4)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%iarray5)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%iarray5)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%iarray6)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%iarray6)
            endif
!
            if (associated(Agrif_Gr%tabvars(i)%var%carray1)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%carray1)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%carray2)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%carray2)
            endif
!
            if (associated(Agrif_Gr%tabvars(i)%var%oldvalues2D)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%oldvalues2D)
            endif
            if (associated(Agrif_Gr%tabvars(i)%var%interpIndex)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%interpIndex)
            endif
            
            if (associated(Agrif_Gr%tabvars(i)%var%posvar)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%posvar)
            endif     
            
            if (associated(Agrif_Gr%tabvars(i)%var%interptab)) then
               Deallocate(Agrif_Gr%tabvars(i)%var%interptab)
            endif              
!
            Deallocate(Agrif_Gr%tabvars(i)%var)
! 
         endif
      enddo
C
C      
      Return
C
C
      End Subroutine Agrif_Free_data_after      
C
C
CC    **************************************************************************
CCC   Subroutine AGRIF_CopyFromold_All
C     **************************************************************************
C
      Recursive Subroutine AGRIF_CopyFromold_All(g,oldchildgrids)
C
CCC   Description:
CCC   Routine called in the Agrif_Init_Hierarchy procedure 
C       (Agrif_Clustering module). 
C
CC    Method:        
C
C     Declarations:
C
      
C      
C     Pointer argument    
      TYPE(AGRIF_grid),pointer   :: g ! Pointer on the current grid
      TYPE(AGRIF_pgrid),pointer   :: oldchildgrids
C
C     Local pointer
      TYPE(AGRIF_pgrid),pointer  :: parcours ! Pointer for the recursive 
                                             ! procedure 
      REAL g_eps,eps,oldgrid_eps
      INTEGER :: out
      INTEGER :: iii
C
      out = 0
C                                                                           
      parcours => oldchildgrids  
C
      do while (associated(parcours))
C 
        if ((.NOT. g % fixed) .AND. (parcours % gr %oldgrid)) then
C        
            g_eps = huge(1.)
            oldgrid_eps = huge(1.)
            do iii = 1 , Agrif_Probdim
               g_eps = min(g_eps,g % Agrif_d(iii))
               oldgrid_eps = min(oldgrid_eps,
     &                       parcours % gr % Agrif_d(iii))
            enddo
C
            eps = min(g_eps,oldgrid_eps)/100.                  
C
            do iii = 1 , Agrif_Probdim

               if (g % Agrif_d(iii) .LT. 
     &             (parcours % gr % Agrif_d(iii) - eps)) then
C            
                   parcours => parcours % next
C            
                   out = 1
C   
                   Exit
C             
               endif
C      
            enddo
        if ( out .EQ. 1 ) Cycle
C
            Call AGRIF_CopyFromOld(g,parcours%gr)
C
        endif
C                     
        Call Agrif_CopyFromold_All
     &             (g, parcours % gr % child_grids)
C        
        parcours => parcours % next
C        
      enddo
C
C     
      Return      
C
C
      End Subroutine AGRIF_CopyFromold_All
C
C
C      
C     **************************************************************************
CCC   Subroutine Agrif_CopyFromold
C     **************************************************************************
C      
      Subroutine Agrif_CopyFromold(Agrif_New_Gr,Agrif_Old_Gr)
C
CCC   Description:
CCC   Call to the Agrif_Copy procedure.
C
CC    Method:
CC            
C
C     Declarations:
C
      
C      
C     Pointer argument    
      TYPE(Agrif_Grid),Pointer   :: Agrif_New_Gr  ! Pointer on the current grid
      TYPE(Agrif_Grid),Pointer :: Agrif_Old_Gr    ! Pointer on an old grid
      INTEGER :: i
C
C      
      do i = 1 , AGRIF_NbVariables
         if ( Agrif_Mygrid % tabvars(i) % var % restaure ) then
C
            Call Agrif_Copy(Agrif_New_Gr,Agrif_Old_Gr,
     &           Agrif_New_Gr % tabvars(i),
     &           Agrif_Old_Gr % tabvars(i))
C
        endif
      enddo

C      
C            
      Return
C
C      
      End Subroutine Agrif_CopyFromold
C
C
CC
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Copy
C     **************************************************************************
C      
      Subroutine Agrif_Copy(Agrif_New_Gr,Agrif_Old_Gr,New_Var,Old_Var)
C     
CCC   Description:
CCC   Sets arguments of the Agrif_UpdatenD procedures, n being the number of 
CCC   DIMENSION of the grid variable.
C
CC    Method:
CC            
C
C     Declarations:
C
      
C
C     Pointer argument    
      TYPE(Agrif_Grid),Pointer   :: Agrif_New_Gr   ! Pointer on the current grid
      TYPE(Agrif_Grid), Pointer :: Agrif_Old_Gr    ! Pointer on an old grid
      TYPE(Agrif_Pvariable) :: New_Var, Old_Var
      INTEGER :: nbdim                  ! Number of dimensions of the current 
                                        ! grid      
      INTEGER,DIMENSION(6) :: pttabnew  ! Indexes of the first point in the 
                                        ! domain
      INTEGER,DIMENSION(6) :: petabnew  ! Indexes of the first point in the 
                                        ! domain
      INTEGER,DIMENSION(6) :: pttabold  ! Indexes of the first point in the 
                                        ! domain
      INTEGER,DIMENSION(6) :: petabold  ! Indexes of the first point in the 
                                        ! domain
      INTEGER,DIMENSION(6) :: nbtabold  ! Number of cells in each direction
      
      INTEGER,DIMENSION(6) :: nbtabnew  ! Number of cells in each direction   
      TYPE(AGRIF_Variable), Pointer :: root ! Pointer on the variable of the 
                                            ! root grid
      REAL, DIMENSION(6) :: snew,sold
      REAL, DIMENSION(6) :: dsnew,dsold                              
      REAL :: eps
      INTEGER :: n
C
C  
      root => New_Var % var % root_var 
C      
      nbdim = root % nbdim
C         
      do n=1,nbdim
C  
        select case(root % interptab(n))
C        
        case('x')                
C
          pttabnew(n) = root % point(1)
C        
          pttabold(n) = root % point(1)
C        
          snew(n) = Agrif_New_Gr % Agrif_x(1)
C
          sold(n) = Agrif_Old_Gr % Agrif_x(1) 
C        
          dsnew(n) = Agrif_New_Gr % Agrif_d(1)
C
          dsold(n) = Agrif_Old_Gr % Agrif_d(1)
C                      
          if (root % posvar(n) .EQ. 1) then
C          
              petabnew(n) = pttabnew(n) + Agrif_New_Gr % nb(1)
C          
              petabold(n) = pttabold(n) + Agrif_Old_Gr % nb(1)         
C        
            else
C          
              petabnew(n) = pttabnew(n) + Agrif_New_Gr % nb(1) - 1
C          
              petabold(n) = pttabold(n) + Agrif_Old_Gr % nb(1) - 1
C
              snew(n) = snew(n) + dsnew(n)/2.
C
              sold(n) = sold(n) + dsold(n)/2.
C        
          endif           
C        
        case('y')
C        
          pttabnew(n) = root % point(2)
C        
          pttabold(n) = root % point(2)
C        
          snew(n) = Agrif_New_Gr % Agrif_x(2)
C        
          sold(n) = Agrif_Old_Gr % Agrif_x(2)
C        
          dsnew(n) = Agrif_New_Gr % Agrif_d(2)
C        
          dsold(n) = Agrif_Old_Gr % Agrif_d(2)
C                
          if (root % posvar(n) .EQ. 1) then
C        
              petabnew(n) = pttabnew(n) + Agrif_New_Gr % nb(2)
C          
              petabold(n) = pttabold(n) + Agrif_Old_Gr % nb(2)         
C        
            else
C          
              petabnew(n) = pttabnew(n) + Agrif_New_Gr % nb(2) - 1
C          
              petabold(n) = pttabold(n) + Agrif_Old_Gr % nb(2) - 1
C
              snew(n) = snew(n) + dsnew(n)/2.
C
              sold(n) = sold(n) + dsold(n)/2.                    
C        
          endif 
C        
        case('z')
C        
          pttabnew(n) = root % point(3)
C        
          pttabold(n) = root % point(3)
C        
          snew(n) = Agrif_New_Gr % Agrif_x(3)
C        
          sold(n) = Agrif_Old_Gr % Agrif_x(3)
C        
          dsnew(n) = Agrif_New_Gr % Agrif_d(3)
C        
          dsold(n) = Agrif_Old_Gr % Agrif_d(3)
C                
          if (root % posvar(n) .EQ. 1) then
C        
              petabnew(n) = pttabnew(n) + Agrif_New_Gr % nb(3)
C          
              petabold(n) = pttabold(n) + Agrif_Old_Gr % nb(3)         
C        
            else
C          
             petabnew(n) = pttabnew(n) + Agrif_New_Gr % nb(3) - 1
C          
             petabold(n) = pttabold(n) + Agrif_Old_Gr % nb(3) - 1
C
             snew(n) = snew(n) + dsnew(n)/2.
C
             sold(n) = sold(n) + dsold(n)/2.                   
C        
          endif
C          
        case('N')
C        
          Call Agrif_nbdim_Get_bound(New_Var%var,
     &                           pttabnew(n),petabnew(n),
     &                           n,nbdim)
C        
          pttabold(n) = pttabnew(n)
C      
          petabold(n) = petabnew(n)
C              
          snew(n) = 0.
C      
          sold(n) = 0. 
C      
          dsnew(n) = 1.
C      
          dsold(n) = 1.
C        
        end select
C      
      enddo
C      
      do n = 1,nbdim
C      
        nbtabnew(n) = petabnew(n) - pttabnew(n)
C      
        nbtabold(n) = petabold(n) - pttabold(n)      
C 
      enddo      
C
      eps = min(minval(dsnew(1:nbdim)),minval(dsold(1:nbdim)))
C     
      eps = eps/100.      
C      
      do n = 1,nbdim
C      
        if (snew(n) .GT. (sold(n)+dsold(n)*nbtabold(n)+eps)) Return
C      
        if ((snew(n)+dsnew(n)*nbtabnew(n)-eps) .LT. sold(n)) Return
C      
      enddo
C            
C
      Call AGRIF_CopynD
     &        (New_Var,Old_Var,pttabold,petabold,pttabnew,petabnew,
     &         sold,snew,dsold,dsnew,nbdim) 
C      
C 
      Return
C      
C
      End Subroutine Agrif_Copy
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_CopynD
C     **************************************************************************
C      
      Subroutine Agrif_CopynD(New_Var,Old_Var,pttabold,petabold,
     &                        pttabnew,petabnew,sold,snew,dsold,
     &                        dsnew,nbdim)
C     
CCC   Description:
CCC   Copy of the nD New_Var variable from the nD Old_Var variable.
C
CC    Method:
CC            
C
C     Declarations:
C
      
C
      INTEGER :: nbdim
      TYPE(Agrif_Pvariable) :: New_Var, Old_Var     
      INTEGER,DIMENSION(nbdim) :: pttabnew 
      INTEGER,DIMENSION(nbdim) :: petabnew
      INTEGER,DIMENSION(nbdim) :: pttabold
      INTEGER,DIMENSION(nbdim) :: petabold      
      REAL, DIMENSION(nbdim) :: snew,sold
      REAL, DIMENSION(nbdim) :: dsnew,dsold 
C
      INTEGER :: i,j,k,l,m,n,i0,j0,k0,l0,m0,n0
C
      REAL, DIMENSION(nbdim) :: dim_gmin,dim_gmax
      REAL, DIMENSION(nbdim) :: dim_newmin,dim_newmax
      REAL, DIMENSION(nbdim) :: dim_min
      INTEGER, DIMENSION(nbdim) :: ind_gmin,ind_newmin
      INTEGER, DIMENSION(nbdim) ::  ind_newmax
C               
C
      do i = 1,nbdim
C      
        dim_gmin(i) = sold(i)
        dim_gmax(i) = sold(i) + (petabold(i)-pttabold(i)) * dsold(i)
C 
        dim_newmin(i) = snew(i)
        dim_newmax(i) = snew(i) + (petabnew(i)-pttabnew(i)) * dsnew(i)
C
      enddo         
C
      do i = 1,nbdim
C  
        if (dim_gmax(i) .LT. dim_newmin(i)) Return
C      
        if (dim_gmin(i) .GT. dim_newmax(i)) Return
C
      enddo
C
C
      do i = 1,nbdim
C 
        ind_newmin(i) = pttabnew(i) - floor(-(max(dim_gmin(i),
     &                      dim_newmin(i))-dim_newmin(i))/dsnew(i)) 
C      
        dim_min(i) = snew(i) + (ind_newmin(i)-pttabnew(i))*dsnew(i)
C      
        ind_gmin(i) = pttabold(i) + nint((dim_min(i)-
     &                dim_gmin(i))/dsold(i))      
C
        ind_newmax(i) = pttabnew(i) 
     &                  + int((min(dim_gmax(i),dim_newmax(i))
     &                         -dim_newmin(i))/dsnew(i))
C
      enddo 
C
C
C
      SELECT CASE (nbdim)
      CASE (1)
         i0 = ind_gmin(1)
         do i = ind_newmin(1),ind_newmax(1)
            New_Var % var % array1(i) =
     &      Old_Var % var % array1(i0)
            New_Var % var % restore1D(i) = 1   
         i0 = i0 + int(dsnew(1)/dsold(1))
         enddo
      CASE (2)
        i0 = ind_gmin(1)
        do i = ind_newmin(1),ind_newmax(1)
        j0 = ind_gmin(2)
        do j = ind_newmin(2),ind_newmax(2)
           New_Var % var % array2(i,j) =
     &            Old_Var % var % array2(i0,j0)
           New_Var % var % restore2D(i,j) = 1
               j0 = j0 + int(dsnew(2)/dsold(2))
        enddo
        i0 = i0 + int(dsnew(1)/dsold(1))
        enddo
      CASE (3)
        i0 = ind_gmin(1)
        do i = ind_newmin(1),ind_newmax(1)
        j0 = ind_gmin(2)
        do j = ind_newmin(2),ind_newmax(2)
        k0 = ind_gmin(3)
        do k = ind_newmin(3),ind_newmax(3)
           New_Var % var % array3(i,j,k) =
     &                  Old_Var % var % array3(i0,j0,k0)
           New_Var % var % restore3D(i,j,k) = 1   
                     k0 = k0 + int(dsnew(3)/dsold(3))
        enddo
        j0 = j0 + int(dsnew(2)/dsold(2))
        enddo
        i0 = i0 + int(dsnew(1)/dsold(1))
        enddo
      CASE (4)
        i0 = ind_gmin(1)
        do i = ind_newmin(1),ind_newmax(1)
        j0 = ind_gmin(2)
        do j = ind_newmin(2),ind_newmax(2)
        k0 = ind_gmin(3)
        do k = ind_newmin(3),ind_newmax(3)
        l0 = ind_gmin(4)
        do l = ind_newmin(4),ind_newmax(4) 
           New_Var % var % array4(i,j,k,l) =
     &                        Old_Var % var % array4(i0,j0,k0,l0)
           New_Var % var % restore4D(i,j,k,l) = 1   
        l0 = l0 + int(dsnew(4)/dsold(4))
        enddo
        k0 = k0 + int(dsnew(3)/dsold(3))
        enddo
        j0 = j0 + int(dsnew(2)/dsold(2))
        enddo
        i0 = i0 + int(dsnew(1)/dsold(1))
        enddo
      CASE (5)
        i0 = ind_gmin(1)
        do i = ind_newmin(1),ind_newmax(1)
        j0 = ind_gmin(2)
        do j = ind_newmin(2),ind_newmax(2)
        k0 = ind_gmin(3)
        do k = ind_newmin(3),ind_newmax(3)
        l0 = ind_gmin(4)
        do l = ind_newmin(4),ind_newmax(4) 
        m0 = ind_gmin(5)
        do m = ind_newmin(5),ind_newmax(5)
           New_Var % var % array5(i,j,k,l,m) =
     &                        Old_Var % var % array5(i0,j0,k0,l0,m0)
           New_Var % var % restore5D(i,j,k,l,m) = 1
        m0 = m0 + int(dsnew(5)/dsold(5)) 
        enddo
        l0 = l0 + int(dsnew(4)/dsold(4))
        enddo
        k0 = k0 + int(dsnew(3)/dsold(3))
        enddo
        j0 = j0 + int(dsnew(2)/dsold(2))
        enddo
        i0 = i0 + int(dsnew(1)/dsold(1))
        enddo
      CASE (6)
        i0 = ind_gmin(1)
        do i = ind_newmin(1),ind_newmax(1)
        j0 = ind_gmin(2)
        do j = ind_newmin(2),ind_newmax(2)
        k0 = ind_gmin(3)
        do k = ind_newmin(3),ind_newmax(3)
        l0 = ind_gmin(4)
        do l = ind_newmin(4),ind_newmax(4) 
        m0 = ind_gmin(5)
        do m = ind_newmin(5),ind_newmax(5)
        n0 = ind_gmin(6)
        do n = ind_newmin(6),ind_newmax(6)
           New_Var % var % array6(i,j,k,l,m,n) =
     &                        Old_Var % var % array6(i0,j0,k0,l0,m0,n0)
           New_Var % var % restore6D(i,j,k,l,m,n) = 1
        n0 = n0 + int(dsnew(6)/dsold(6)) 
        enddo
        m0 = m0 + int(dsnew(5)/dsold(5)) 
        enddo
        l0 = l0 + int(dsnew(4)/dsold(4))
        enddo
        k0 = k0 + int(dsnew(3)/dsold(3))
        enddo
        j0 = j0 + int(dsnew(2)/dsold(2))
        enddo
        i0 = i0 + int(dsnew(1)/dsold(1))
        enddo
      END SELECT
C           
      Return
C
C
      End Subroutine Agrif_CopynD                    
C
C
C
      End module Agrif_Save
