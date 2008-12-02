!
! $Id: modinterp.F 779 2007-12-22 17:04:17Z rblod $
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
CCC   Module Agrif_Interpolation
C
      Module Agrif_Interpolation
C
CCC   Description:
CCC   Module to initialize a fine grid from its parent grid, by using a space 
CCC   interpolation
C
C     Modules used:
C  
      Use Agrif_Interpbasic
      Use Agrif_Arrays
      Use Agrif_Mask 
      Use Agrif_CurgridFunctions
#if defined AGRIF_MPI
      Use Agrif_mpp
#endif
C      
      IMPLICIT NONE
      logical,  private:: precomputedone(7) = .FALSE.
C
      CONTAINS
C     Define procedures contained in this module        
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_1d
C     **************************************************************************
C 
      Subroutine Agrif_Interp_1d(TypeInterp,parent,child,tab,
     &    torestore,nbdim,procname)            
C
CCC   Description:
CCC   Subroutine to calculate the boundary conditions of a fine grid for a 1D
C        grid variable.
C
C     Declarations:
C      
      
C
C     Arguments      
      INTEGER :: nbdim
      INTEGER,DIMENSION(6) :: TypeInterp     ! Kind of interpolation
                                             !    (linear,lagrange,spline)
      TYPE(AGRIF_PVariable) :: parent        ! Variable on the parent grid
      TYPE(AGRIF_PVariable) :: child         ! Variable on the child grid
      TYPE(AGRIF_PVariable) :: childtemp     ! Temporary variable on the child
                                             !    grid
      LOGICAL :: torestore
      REAL, DIMENSION(
     &         child%var%lb(1):child%var%ub(1)
     &         ), Target :: tab    ! Result
      External :: procname
      Optional ::  procname
C
C
      allocate(childtemp % var)  
C      
C     Pointer on the root variable
      childtemp % var % root_var => child % var %root_var
C      
C     Number of dimensions of the grid variable
      childtemp % var % nbdim = nbdim
C
C     Tab is the result of the interpolation
      childtemp % var % array1 => tab 
      
      childtemp % var % lb = child % var % lb
      childtemp % var % ub = child % var % ub  
            
C      
      if (torestore) then
C 
          childtemp % var % array1 = child % var % array1
C          
          childtemp % var % restore1D => child % var % restore1D
C      
        else
C        
          Nullify(childtemp % var % restore1D)
C      
      endif     
C 
C     Index indicating (in the Agrif_Interp1D procedure) if a space
C        interpolation is necessary
      childtemp % var % interpIndex => child % var % interpIndex       
      childtemp % var % Interpolationshouldbemade = 
     &    child % var % Interpolationshouldbemade 
      childtemp % var % list_interp => child % var% list_interp            
C      
      if (present(procname)) then
      Call Agrif_InterpVariable
     &     (TypeInterp,parent,childtemp,torestore,procname)
      else
      Call Agrif_InterpVariable
     &     (TypeInterp,parent,childtemp,torestore)
      endif
      child % var % list_interp => childtemp % var %list_interp     
C      
      deallocate(childtemp % var)
C
C       
      End Subroutine Agrif_Interp_1D
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_2d
C     **************************************************************************
C 
      Subroutine Agrif_Interp_2d(TypeInterp,parent,child,tab,
     &                           torestore,nbdim,procname)            
C
CCC   Description:
CCC   Subroutine to calculate the boundary conditions of a fine grid for a 2D
C        grid variable.
C
C     Declarations:
C      
      
C
C     Arguments      
      INTEGER :: nbdim
      INTEGER,DIMENSION(6) :: TypeInterp     ! Kind of interpolation
                                             !    (linear,lagrange,spline)
      TYPE(AGRIF_PVariable) :: parent        ! Variable on the parent grid
      TYPE(AGRIF_PVariable) :: child         ! Variable on the child grid
      TYPE(AGRIF_PVariable) :: childtemp     ! Temporary variable on the child
                                             !    grid
      LOGICAL :: torestore
      REAL, DIMENSION(
     &         child%var%lb(1):child%var%ub(1),
     &         child%var%lb(2):child%var%ub(2)
     &    ), Target :: tab    ! Result
      External :: procname
      Optional ::  procname
C
C
      allocate(childtemp % var)  
C      
C     Pointer on the root variable
      childtemp % var % root_var => child % var %root_var
C      
C     Number of dimensions of the grid variable
      childtemp % var % nbdim = nbdim
C
C     Tab is the result of the interpolation
      childtemp % var % array2 => tab  
      
      childtemp % var % lb = child % var % lb
      childtemp % var % ub = child % var % ub
      
C
      if (torestore) then      
C 
          childtemp % var % array2 = child % var % array2          
C 
          childtemp % var % restore2D => child % var % restore2D        
C      
        else
C        
          Nullify(childtemp % var % restore2D)
C      
      endif       
C 
C     Index indicating (in the Agrif_Interp2D procedure) if a space
C        interpolation is necessary
      childtemp % var % interpIndex => child % var % interpIndex       
      childtemp % var % Interpolationshouldbemade = 
     &    child % var % Interpolationshouldbemade   
      childtemp % var % list_interp => child % var% list_interp          
C     
      if (present(procname)) then
      Call Agrif_InterpVariable
     &     (TypeInterp,parent,childtemp,torestore,procname)
      else
      Call Agrif_InterpVariable
     &     (TypeInterp,parent,childtemp,torestore)
      endif

      child % var % list_interp => childtemp % var %list_interp     
C      
      deallocate(childtemp % var)
C
C       
      End Subroutine Agrif_Interp_2D
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_3d
C     **************************************************************************
C 
      Subroutine Agrif_Interp_3d(TypeInterp,parent,child,tab,
     &   torestore,nbdim,procname)            
C
CCC   Description:
CCC   Subroutine to calculate the boundary conditions of a fine grid for a 3D
C        grid variable.
C
C     Declarations:
C      
      
C
C     Arguments      
      INTEGER :: nbdim
      INTEGER,DIMENSION(6) :: TypeInterp     ! Kind of interpolation
                                             !    (linear,lagrange,spline)
      TYPE(AGRIF_PVariable) :: parent        ! Variable on the parent grid
      TYPE(AGRIF_PVariable) :: child         ! Variable on the child grid
      TYPE(AGRIF_PVariable) :: childtemp     ! Temporary variable on the child
                                             !    grid
      LOGICAL :: torestore
      REAL, DIMENSION(
     &         child%var%lb(1):child%var%ub(1),
     &         child%var%lb(2):child%var%ub(2),
     &         child%var%lb(3):child%var%ub(3)
     &      ), Target :: tab  ! Results
      External :: procname
      Optional ::  procname
C
C
      allocate(childtemp % var)  
C
C     Pointer on the root variable
      childtemp % var % root_var => child % var %root_var
C      
C     Number of dimensions of the grid variable
      childtemp % var % nbdim = nbdim  
C     
C     Tab is the result of the interpolation 
      childtemp % var % array3 => tab 
      
      childtemp % var % lb = child % var % lb
      childtemp % var % ub = child % var % ub        
C      
      if (torestore) then
C     
          childtemp % var % array3 = child % var % array3
C
          childtemp % var % restore3D => child % var % restore3D
C      
        else
C        
          Nullify(childtemp % var % restore3D)
C      
      endif
C 
C     Index indicating (in the Agrif_Interp3D procedure) if a space
C        interpolation is necessary
      childtemp % var % interpIndex => child % var % interpIndex    
      childtemp % var % Interpolationshouldbemade = 
     &    child % var % Interpolationshouldbemade   
      childtemp % var % list_interp => child % var% list_interp          
C     

      if (present(procname)) then
      Call Agrif_InterpVariable
     &     (TypeInterp,parent,childtemp,torestore,procname)
      else
      Call Agrif_InterpVariable
     &     (TypeInterp,parent,childtemp,torestore)
      endif


      child % var % list_interp => childtemp % var %list_interp     
C      
      deallocate(childtemp % var)
C
C       
      End Subroutine Agrif_Interp_3D               
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_4d
C     **************************************************************************
C 
      Subroutine Agrif_Interp_4d(TypeInterp,parent,child,tab,
     &   torestore,nbdim,procname)            
C
CCC   Description:
CCC   Subroutine to calculate the boundary conditions of a fine grid for a 4D
C        grid variable.
C
C     Declarations:
C      
      
C
C     Arguments      
      INTEGER :: nbdim
      INTEGER,DIMENSION(6) :: TypeInterp     ! Kind of interpolation
                                             !    (linear,lagrange,spline)
      TYPE(AGRIF_PVariable) :: parent        ! Variable on the parent grid
      TYPE(AGRIF_PVariable) :: child         ! Variable on the child grid
      TYPE(AGRIF_PVariable) :: childtemp     ! Temporary variable on the child
                                             !    grid
      LOGICAL :: torestore
      REAL, DIMENSION(
     &         child%var%lb(1):child%var%ub(1),
     &         child%var%lb(2):child%var%ub(2),
     &         child%var%lb(3):child%var%ub(3),
     &         child%var%lb(4):child%var%ub(4)
     &      ), Target :: tab  ! Results
      External :: procname
      Optional ::  procname
C
C
      allocate(childtemp % var)  
C
C     Pointer on the root variable
      childtemp % var % root_var => child % var %root_var
C      
C     Number of dimensions of the grid variable
      childtemp % var % nbdim = nbdim  
C      
C     Tab is the result of the interpolation
      childtemp % var % array4 => tab 
      
      childtemp % var % lb = child % var % lb
      childtemp % var % ub = child % var % ub  
            
C 
      if (torestore) then
C 
          childtemp % var % array4 = child % var % array4
C
          childtemp % var % restore4D => child % var % restore4D
C      
        else
C        
          Nullify(childtemp % var % restore4D)
C      
      endif        
C 
C     Index indicating (in the Agrif_Interp4D procedure) if a space
C        interpolation is necessary
      childtemp % var % interpIndex => child % var % interpIndex    
      childtemp % var % Interpolationshouldbemade = 
     &    child % var % Interpolationshouldbemade  
      childtemp % var % list_interp => child % var% list_interp           
C     
      if (present(procname)) then
      Call Agrif_InterpVariable
     &     (TypeInterp,parent,childtemp,torestore,procname)
      else
      Call Agrif_InterpVariable
     &     (TypeInterp,parent,childtemp,torestore)
      endif


      child % var % list_interp => childtemp % var %list_interp
C      
      deallocate(childtemp % var)
C
C       
      End Subroutine Agrif_Interp_4D
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_5d
C     **************************************************************************
C 
      Subroutine Agrif_Interp_5d(TypeInterp,parent,child,tab,
     &   torestore,nbdim,procname)            
C
CCC   Description:
CCC   Subroutine to calculate the boundary conditions of a fine grid for a 5D
C        grid variable.
C
C     Declarations:
C      
      
C
C     Arguments      
      INTEGER :: nbdim
      INTEGER,DIMENSION(6) :: TypeInterp     ! Kind of interpolation
                                             !    (linear,lagrange,spline)
      TYPE(AGRIF_PVariable) :: parent        ! Variable on the parent grid
      TYPE(AGRIF_PVariable) :: child         ! Variable on the child grid
      TYPE(AGRIF_PVariable) :: childtemp     ! Temporary variable on the child
                                             !    grid
      LOGICAL :: torestore
      REAL, DIMENSION(
     &         child%var%lb(1):child%var%ub(1),
     &         child%var%lb(2):child%var%ub(2),
     &         child%var%lb(3):child%var%ub(3),
     &         child%var%lb(4):child%var%ub(4),
     &         child%var%lb(5):child%var%ub(5)
     &      ),  Target :: tab  ! Results
      External :: procname
      Optional ::  procname
C
C
      allocate(childtemp % var)  
C
C     Pointer on the root variable
      childtemp % var % root_var => child % var %root_var
C      
C     Number of dimensions of the grid variable
      childtemp % var % nbdim = nbdim  
C      
C     Tab is the result of the interpolation
      childtemp % var % array5 => tab  
      
      childtemp % var % lb = child % var % lb
      childtemp % var % ub = child % var % ub  
            
C      
      if (torestore) then
C 
          childtemp % var % array5 = child % var % array5
C
          childtemp % var % restore5D => child % var % restore5D
C      
        else
C        
          Nullify(childtemp % var % restore5D)
C      
      endif       
C 
C     Index indicating (in the Agrif_Interp5D procedure) if a space
C        interpolation is necessary
      childtemp % var % interpIndex => child % var % interpIndex    
      childtemp % var % Interpolationshouldbemade = 
     &    child % var % Interpolationshouldbemade   
      childtemp % var % list_interp => child % var% list_interp          
C      
      if (present(procname)) then
      Call Agrif_InterpVariable
     &     (TypeInterp,parent,childtemp,torestore,procname)
      else
      Call Agrif_InterpVariable
     &     (TypeInterp,parent,childtemp,torestore)
      endif

     
      child % var % list_interp => childtemp % var %list_interp
C      
      deallocate(childtemp % var)
C
C       
      End Subroutine Agrif_Interp_5D
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_6d
C     **************************************************************************
C 
      Subroutine Agrif_Interp_6d(TypeInterp,parent,child,tab,
     &  torestore,nbdim,procname)            
C
CCC   Description:
CCC   Subroutine to calculate the boundary conditions of a fine grid for a 6D
C        grid variable.
C
C     Declarations:
C      
      
C
C     Arguments      
      INTEGER :: nbdim
      INTEGER,DIMENSION(6) :: TypeInterp     ! Kind of interpolation
                                             !    (linear,lagrange,spline)
      TYPE(AGRIF_PVariable) :: parent        ! Variable on the parent grid
      TYPE(AGRIF_PVariable) :: child         ! Variable on the child grid
      TYPE(AGRIF_PVariable) :: childtemp     ! Temporary variable on the child
                                             !    grid
      LOGICAL :: torestore
      REAL, DIMENSION(
     &         child%var%lb(1):child%var%ub(1),
     &         child%var%lb(2):child%var%ub(2),
     &         child%var%lb(3):child%var%ub(3),
     &         child%var%lb(4):child%var%ub(4),
     &         child%var%lb(5):child%var%ub(5),
     &         child%var%lb(6):child%var%ub(6)
     &      ),  Target :: tab  ! Results
      External :: procname
      Optional ::  procname
C
C
      allocate(childtemp % var)  
C
C     Pointer on the root variable
      childtemp % var % root_var => child % var %root_var
C      
C     Number of dimensions of the grid variable
      childtemp % var % nbdim = nbdim  
C      
C     Tab is the result of the interpolation
      childtemp % var % array6 => tab  
      
      childtemp % var % lb = child % var % lb
      childtemp % var % ub = child % var % ub  
            
C      
      if (torestore) then
C 
          childtemp % var % array6 = child % var % array6
C
          childtemp % var % restore6D => child % var % restore6D
C      
        else
C        
          Nullify(childtemp % var % restore6D)
C      
      endif       
C 
C     Index indicating (in the Agrif_Interp6D procedure) if a space
C        interpolation is necessary
      childtemp % var % interpIndex => child % var % interpIndex    
      childtemp % var % Interpolationshouldbemade = 
     &    child % var % Interpolationshouldbemade  
     
      childtemp % var % list_interp => child % var% list_interp           
C      

      if (present(procname)) then
      Call Agrif_InterpVariable
     &     (TypeInterp,parent,childtemp,torestore,procname)
      else
      Call Agrif_InterpVariable
     &     (TypeInterp,parent,childtemp,torestore)
      endif


C      
      child % var % list_interp => childtemp % var %list_interp
      deallocate(childtemp % var)
C
C       
      End Subroutine Agrif_Interp_6D
C
C
C
C     **************************************************************************
C     Subroutine Agrif_InterpVariable    
C     **************************************************************************
C   
      Subroutine Agrif_InterpVariable(TYPEinterp,parent,child,torestore,
     &            procname)
C
CCC   Description:
CCC   Subroutine to set some arguments of subroutine Agrif_InterpnD, n being the
CCC   DIMENSION of the grid variable.
C
CC    Declarations:
C      
c      
C      
C
C     Scalar argument
      INTEGER,DIMENSION(6) :: TYPEinterp! TYPE of interpolation
                                        !    (linear,spline,...)
C     Data TYPE arguments                                   
      TYPE(AGRIF_PVariable) :: parent   ! Variable on the parent grid
      TYPE(AGRIF_PVariable) :: child    ! Variable on the child grid
C
C     LOGICAL argument      
      LOGICAL:: torestore               ! Its value is .false., it indicates the
                                        ! results of the interpolation are    
                                        ! applied on the whole current grid   
C
C     Local scalars      
      INTEGER               :: nbdim          ! Number of dimensions of the
                                              !    current grid
      INTEGER ,DIMENSION(6) :: pttab_child  
      INTEGER ,DIMENSION(6) :: petab_child      
      INTEGER ,DIMENSION(6) :: pttab_parent  
      REAL    ,DIMENSION(6) :: s_child,s_parent
      REAL    ,DIMENSION(6) :: ds_child,ds_parent
      External :: procname
      Optional ::  procname

C
      Call PreProcessToInterpOrUpdate(parent,child,
     &             petab_Child(1:nbdim),
     &             pttab_Child(1:nbdim),pttab_Parent(1:nbdim),
     &             s_Child(1:nbdim),s_Parent(1:nbdim),
     &             ds_Child(1:nbdim),ds_Parent(1:nbdim),
     &             nbdim)
C
C
C     Call to a procedure of interpolation against the number of dimensions of 
C     the grid variable
C

      if (present(procname)) then
      call Agrif_InterpnD
     &            (TYPEinterp,parent,child,
     &             pttab_Child(1:nbdim),petab_Child(1:nbdim),
     &             pttab_Child(1:nbdim),pttab_Parent(1:nbdim),
     &             s_Child(1:nbdim),s_Parent(1:nbdim),
     &             ds_Child(1:nbdim),ds_Parent(1:nbdim),
     &             child,torestore,nbdim,procname)
      else
      call Agrif_InterpnD
     &            (TYPEinterp,parent,child,
     &             pttab_Child(1:nbdim),petab_Child(1:nbdim),
     &             pttab_Child(1:nbdim),pttab_Parent(1:nbdim),
     &             s_Child(1:nbdim),s_Parent(1:nbdim),
     &             ds_Child(1:nbdim),ds_Parent(1:nbdim),
     &             child,torestore,nbdim)

      endif
C
      Return
C
C
      End subroutine Agrif_InterpVariable       
C
C
C     **************************************************************************
C     Subroutine Agrif_InterpnD  
C     **************************************************************************
C  
      Subroutine Agrif_InterpnD(TYPEinterp,parent,child,
     &                          pttab,petab,
     &                          pttab_Child,pttab_Parent,
     &                          s_Child,s_Parent,ds_Child,ds_Parent,
     &                          restore,torestore,nbdim,procname)
C
C     Description:
C     Subroutine to interpolate a nD grid variable from its parent grid,
C     by using a space interpolation. 
C
C     Declarations:
C
      
C
#ifdef AGRIF_MPI
C
#include "mpif.h"
C
#endif
C
C     Arguments
      External :: procname
      Optional :: procname
      INTEGER                    :: nbdim
      INTEGER,DIMENSION(6)       :: TYPEinterp         ! TYPE of interpolation
                                                       !    (linear,...)
      TYPE(AGRIF_PVARIABLE)      :: parent             ! Variable of the parent
                                                       !    grid
      TYPE(AGRIF_PVARIABLE)      :: child              ! Variable of the child
                                                       !    grid
      INTEGER,DIMENSION(nbdim)   :: pttab              ! Index of the first
                                                       !    point inside the
                                                       !    domain
      INTEGER,DIMENSION(nbdim)   :: petab              ! Index of the first
                                                       !    point inside the
                                                       !    domain
      INTEGER,DIMENSION(nbdim)   :: pttab_Child        ! Index of the first
                                                       !    point inside the
                                                       !    domain for the child
                                                       !    grid variable
      INTEGER,DIMENSION(nbdim)   :: pttab_Parent       ! Index of the first
                                                       !    point inside the
                                                       !    domain for the
                                                       !    parent grid variable
      TYPE(AGRIF_PVARIABLE)      :: restore            ! Indicates points where
                                                       !    interpolation
      REAL,DIMENSION(nbdim)      :: s_Child,s_Parent   ! Positions of the parent
                                                       !    and child grids
      REAL,DIMENSION(nbdim)      :: ds_Child,ds_Parent ! Space steps of the
                                                       !    parent and child
                                                       !    grids
      LOGICAL                        :: torestore      ! Indicates if the array
                                                       !    restore is used
C
C     Local pointers
      TYPE(AGRIF_PVARIABLE),SAVE      :: tempP,tempPextend  ! Temporary parent grid variable
      TYPE(AGRIF_PVARIABLE),SAVE      :: tempC      ! Temporary child grid variable
C
C     Local scalars        
      INTEGER                     :: i,j,k,l,m,n
      INTEGER,DIMENSION(nbdim)    :: pttruetab,cetruetab
      INTEGER,DIMENSION(nbdim)    :: indmin,indmax
      LOGICAL,DIMENSION(nbdim)    :: noraftab
      REAL   ,DIMENSION(nbdim)    :: s_Child_temp,s_Parent_temp
      INTEGER,DIMENSION(nbdim)    :: lowerbound,upperbound
      INTEGER,DIMENSION(nbdim)    :: indminglob,indmaxglob
      INTEGER,DIMENSION(nbdim,2,2) :: childarray
      INTEGER,DIMENSION(nbdim,2,2) :: parentarray
      LOGICAL :: memberin,member
      TYPE(AGRIF_PVARIABLE),SAVE                      ::  parentvalues
      LOGICAL :: find_list_interp
      INTEGER,DIMENSION(nbdim)    :: indminglob2,indmaxglob2      
C
#ifdef AGRIF_MPI
C
      LOGICAL :: memberout
      INTEGER,PARAMETER                          :: etiquette = 100
      INTEGER                                    :: code
      INTEGER,DIMENSION(nbdim,4)           :: tab3
      INTEGER,DIMENSION(nbdim,4,0:Agrif_Nbprocs-1) :: tab4
      INTEGER,DIMENSION(nbdim,0:Agrif_Nbprocs-1,8) :: tab4t
      LOGICAL, DIMENSION(0:Agrif_Nbprocs-1) :: memberinall
      LOGICAL, DIMENSION(0:Agrif_Nbprocs-1) :: sendtoproc1,recvfromproc1
      LOGICAL, DIMENSION(1) :: memberin1
C
#endif      
C     

C   
C     Boundaries of the current grid where interpolation is done

      IF (Associated(child%var%list_interp)) THEN
      Call Agrif_Find_list_interp(child%var%list_interp,pttab,petab,
     &                          pttab_Child,pttab_Parent,nbdim,
     &                          indmin,indmax,indminglob,
     &       indmaxglob,indminglob2,indmaxglob2,parentarray,
     &       pttruetab,cetruetab,member,memberin,find_list_interp
#if defined AGRIF_MPI
     &       ,tab4t,memberinall,sendtoproc1,recvfromproc1
#endif
     &    )
      ELSE
      find_list_interp = .FALSE.
      ENDIF
      
      IF (.not.find_list_interp) THEN

      Call Agrif_nbdim_Get_bound_dimension(child % var,
     &                               lowerbound,upperbound,nbdim)
      
      Call Agrif_Childbounds(nbdim,lowerbound,upperbound,
     &                                   pttab,petab,
     &                                   pttruetab,cetruetab,memberin)
     
C
      Call Agrif_Parentbounds(TYPEinterp,nbdim,indminglob,indmaxglob,
     &                        s_Parent_temp,s_Child_temp,
     &                        s_Child,ds_Child,
     &                        s_Parent,ds_Parent,
     &                        pttab,petab,
     &                        pttab_Child,pttab_Parent,
     &                        child%var%root_var%posvar,
     &                        child % var % root_var % interptab)
       
#ifdef AGRIF_MPI
       IF (memberin) THEN
        Call Agrif_Parentbounds(TYPEinterp,nbdim,indmin,indmax,
     &                        s_Parent_temp,s_Child_temp,
     &                        s_Child,ds_Child,
     &                        s_Parent,ds_Parent,
     &                        pttruetab,cetruetab,
     &                        pttab_Child,pttab_Parent,
     &                        child%var%root_var%posvar,
     &                        child % var % root_var % interptab)
      ENDIF
       
      Call Agrif_nbdim_Get_bound_dimension(parent%var,
     &                              lowerbound,upperbound,nbdim)
       
      Call Agrif_ChildGrid_to_ParentGrid()
C
      Call Agrif_Childbounds(nbdim,
     &                       lowerbound,upperbound,
     &                       indminglob,indmaxglob,
     &                       indminglob2,indmaxglob2,member)

C
      IF (member) THEN
      Call Agrif_GlobtoLocInd2(parentarray,
     &                     lowerbound,upperbound,
     &                     indminglob2,indmaxglob2,
     &                     nbdim,Agrif_Procrank,
     &                     member)
       endif
       
      Call Agrif_ParentGrid_to_ChildGrid()
#else
      parentarray(:,1,1) = indminglob
      parentarray(:,2,1) = indmaxglob
      parentarray(:,1,2) = indminglob
      parentarray(:,2,2) = indmaxglob
      indmin = indminglob
      indmax = indmaxglob
      member = .TRUE.
#endif

      ELSE
      
#if !defined AGRIF_MPI
      parentarray(:,1,1) = indminglob
      parentarray(:,2,1) = indmaxglob
      parentarray(:,1,2) = indminglob
      parentarray(:,2,2) = indmaxglob
      indmin = indminglob
      indmax = indmaxglob
      member = .TRUE.
      s_Parent_temp = s_Parent + (indminglob - pttab_Parent)*ds_Parent
      s_Child_temp = s_Child + (pttab - pttab_Child) * ds_Child
#else      
      s_Parent_temp = s_Parent + (indmin - pttab_Parent)*ds_Parent
      s_Child_temp = s_Child + (pttruetab - pttab_Child) * ds_Child
#endif      
           
      ENDIF

      IF (member) THEN
      IF (.not.associated(tempP%var)) allocate(tempP%var)

C
      Call Agrif_nbdim_allocation(tempP%var,
     &     parentarray(:,1,1),parentarray(:,2,1),nbdim)

      Call Agrif_nbdim_Full_VarEQreal(tempP%var,0.,nbdim)



      IF (present(procname)) THEN
      Call Agrif_ChildGrid_to_ParentGrid()
            SELECT CASE (nbdim)
        CASE(1)
          CALL procname(tempP%var%array1,
     &                          parentarray(1,1,2),parentarray(1,2,2))
        CASE(2)
          CALL procname(tempP%var%array2,
     &                          parentarray(1,1,2),parentarray(1,2,2),
     &                          parentarray(2,1,2),parentarray(2,2,2))
        CASE(3)
          CALL procname(tempP%var%array3,
     &                          parentarray(1,1,2),parentarray(1,2,2),
     &                          parentarray(2,1,2),parentarray(2,2,2),
     &                          parentarray(3,1,2),parentarray(3,2,2))
        CASE(4)
          CALL procname(tempP%var%array4,
     &                          parentarray(1,1,2),parentarray(1,2,2),
     &                          parentarray(2,1,2),parentarray(2,2,2),
     &                          parentarray(3,1,2),parentarray(3,2,2),
     &                          parentarray(4,1,2),parentarray(4,2,2))
        CASE(5)
          CALL procname(tempP%var%array5,
     &                          parentarray(1,1,2),parentarray(1,2,2),
     &                          parentarray(2,1,2),parentarray(2,2,2),
     &                          parentarray(3,1,2),parentarray(3,2,2),
     &                          parentarray(4,1,2),parentarray(4,2,2),
     &                          parentarray(5,1,2),parentarray(5,2,2))
        CASE(6)
          CALL procname(tempP%var%array6,
     &                          parentarray(1,1,2),parentarray(1,2,2),
     &                          parentarray(2,1,2),parentarray(2,2,2),
     &                          parentarray(3,1,2),parentarray(3,2,2),
     &                          parentarray(4,1,2),parentarray(4,2,2),
     &                          parentarray(5,1,2),parentarray(5,2,2),
     &                          parentarray(6,1,2),parentarray(6,2,2))
            END SELECT
      Call Agrif_ParentGrid_to_ChildGrid()
      ELSE

      Call Agrif_nbdim_VarEQvar(tempP%var,
     &        parentarray(:,1,1),parentarray(:,2,1),
     &        parent%var,parentarray(:,1,2),parentarray(:,2,2),
     &        nbdim)
      ENDIF
            endif

#ifdef AGRIF_MPI
      if (.not.find_list_interp) then
      tab3(:,1) = indminglob2(:)
      tab3(:,2) = indmaxglob2(:)
      tab3(:,3) = indmin(:)
      tab3(:,4) = indmax(:)
C
C
      Call MPI_ALLGATHER(tab3,4*nbdim,MPI_INTEGER,tab4,4*nbdim,
     &                   MPI_INTEGER,MPI_COMM_WORLD,code)

      IF (.not.associated(tempPextend%var)) Allocate(tempPextend%var)

      DO k=0,Agrif_Nbprocs-1
       do j=1,4
         do i=1,nbdim
         tab4t(i,k,j) = tab4(i,j,k)
                enddo
      enddo
      enddo
      
      memberin1(1) = memberin
      CALL MPI_ALLGATHER(memberin1,1,MPI_LOGICAL,memberinall,
     &                  1,MPI_LOGICAL,MPI_COMM_WORLD,code)

       Call Get_External_Data_first(tab4t(:,:,1),
     &            tab4t(:,:,2),
     &            tab4t(:,:,3),tab4t(:,:,4),nbdim,member,memberin,
     &            memberinall,sendtoproc1,recvfromproc1,tab4t(:,:,5),
     & tab4t(:,:,6),tab4t(:,:,7),tab4t(:,:,8))
          
      endif     
           
!      Call Get_External_Data(tempP,tempPextend,tab4t(:,:,1),
!     &            tab4t(:,:,2),
!     &            tab4t(:,:,3),tab4t(:,:,4),nbdim,member,memberin,
!     &            memberinall)
     
      Call ExchangeSameLevel2(sendtoproc1,recvfromproc1,nbdim,
     &            tab4t(:,:,3),tab4t(:,:,4),tab4t(:,:,5),tab4t(:,:,6),
     &            tab4t(:,:,7),tab4t(:,:,8),memberin,tempP,
     &            tempPextend)
#else
      tempPextend%var => tempP%var
#endif

      if (.not.find_list_interp) then
      Call Agrif_Addto_list_interp(child%var%list_interp,pttab,petab,
     &                          pttab_Child,pttab_Parent,indmin,indmax,
     &   indminglob,indmaxglob,indminglob2,indmaxglob2,parentarray,
     &   pttruetab,cetruetab,member,memberin,nbdim
#if defined AGRIF_MPI
     &   ,tab4t,memberinall,sendtoproc1,recvfromproc1
#endif
     &    )
      endif   
C
C
      IF (memberin) THEN
      IF (.not.associated(tempC%var)) allocate(tempC%var)
C

      Call Agrif_nbdim_allocation(tempC%var,pttruetab,cetruetab,nbdim)

C
C
C     Special values on the parent grid
      if (Agrif_UseSpecialValue) then
C
          noraftab(1:nbdim) =
     &         child % var % root_var % interptab(1:nbdim) .EQ. 'N'
C
          IF (.not.associated(parentvalues%var))
     &            Allocate(parentvalues%var)
C
          Call Agrif_nbdim_allocation
     &               (parentvalues%var,indmin,indmax,nbdim)
          Call Agrif_nbdim_Full_VarEQvar
     &               (parentvalues%var,tempPextend%var,nbdim)
C
          Call Agrif_CheckMasknD(tempPextend,
     &                           parentvalues,
     &                           indmin(1:nbdim),indmax(1:nbdim),
     &                           indmin(1:nbdim),indmax(1:nbdim),
     &                           noraftab(1:nbdim),nbdim)
C
          Call Agrif_nbdim_deallocation(parentvalues%var,nbdim)
C          Deallocate(parentvalues%var)
C
C
      endif      

C
C
C     Interpolation of the current grid

      IF (memberin) THEN
      if ( nbdim .EQ. 1 ) then
         Call Agrif_Interp_1D_recursive(TypeInterp,
     &           tempPextend%var%array1,tempC%var%array1,
     &           indmin,indmax,
     &           pttruetab,cetruetab,
     &           s_Child_temp,s_Parent_temp,
     &           ds_Child,ds_Parent,nbdim)
      elseif ( nbdim .EQ. 2 ) then

         Call Agrif_Interp_2D_recursive(TypeInterp,
     &           tempPextend%var%array2,tempC%var%array2,
     &           indmin,indmax,
     &           pttruetab,cetruetab,
     &           s_Child_temp,s_Parent_temp,
     &           ds_Child,ds_Parent,nbdim)
      elseif ( nbdim .EQ. 3 ) then

         Call Agrif_Interp_3D_recursive(TypeInterp,
     &           tempPextend%var%array3,tempC%var%array3,
     &           indmin,indmax,
     &           pttruetab,cetruetab,
     &           s_Child_temp,s_Parent_temp,
     &           ds_Child,ds_Parent,nbdim)
      elseif ( nbdim .EQ. 4 ) then
         Call Agrif_Interp_4D_recursive(TypeInterp,
     &           tempPextend%var%array4,tempC%var%array4,
     &           indmin,indmax,
     &           pttruetab,cetruetab,
     &           s_Child_temp,s_Parent_temp,
     &           ds_Child,ds_Parent,nbdim)
      elseif ( nbdim .EQ. 5 ) then
         Call Agrif_Interp_5D_recursive(TypeInterp,
     &           tempPextend%var%array5,tempC%var%array5,
     &           indmin,indmax,
     &           pttruetab,cetruetab,
     &           s_Child_temp,s_Parent_temp,
     &           ds_Child,ds_Parent,nbdim)
      elseif ( nbdim .EQ. 6 ) then
         Call Agrif_Interp_6D_recursive(TypeInterp,
     &           tempPextend%var%array6,tempC%var%array6,
     &           indmin,indmax,
     &           pttruetab,cetruetab,
     &           s_Child_temp,s_Parent_temp,
     &           ds_Child,ds_Parent,nbdim)
       endif


C  

      Call Agrif_nbdim_Get_bound_dimension(child % var,
     &                               lowerbound,upperbound,nbdim)

#ifdef AGRIF_MPI
      Call Agrif_GlobtoLocInd2(childarray,
     &                     lowerbound,upperbound,
     &                     pttruetab,cetruetab,
     &                     nbdim,Agrif_Procrank,
     &                     memberout)

#else
       childarray(:,1,1) = pttruetab
       childarray(:,2,1) = cetruetab
       childarray(:,1,2) = pttruetab
       childarray(:,2,2) = cetruetab
ccccccccccccccc       memberout = .TRUE.
#endif


C
C
C     Special values on the child grid  
      if (Agrif_UseSpecialValueFineGrid) then
C

          Call GiveAgrif_SpecialValueToTab_mpi(child%var,tempC%var,
     &                 childarray,
     &                 pttruetab,cetruetab,
     &                 Agrif_SpecialValueFineGrid,nbdim)

C        
      endif
      
      endif

C
      if (torestore) then
C
#ifdef AGRIF_MPI
C
        SELECT CASE (nbdim)
        CASE (1)
             do i = pttruetab(1),cetruetab(1)         
ChildarrayAModifier                if (restore%var%restore1D(i) == 0) 
ChildarrayAModifier     &                child%var%array1(childarray(i,1,2)
ChildarrayAModifier     &                                 ) = 
ChildarrayAModifier     &                tempC%var%array1(i)
             enddo
        CASE (2)
             do i = pttruetab(1),cetruetab(1)
             do j = pttruetab(2),cetruetab(2)
ChildarrayAModifier                   if (restore%var%restore2D(i,j) == 0) 
ChildarrayAModifier     &                child%var%array2(childarray(i,1,2),
ChildarrayAModifier     &                                 childarray(j,2,2)) = 
ChildarrayAModifier     &                tempC%var%array2(i,j)
             enddo
             enddo
        CASE (3)
             do i = pttruetab(1),cetruetab(1)
             do j = pttruetab(2),cetruetab(2)  
             do k = pttruetab(3),cetruetab(3)
ChildarrayAModifier                      if (restore%var%restore3D(i,j,k) == 0) 
ChildarrayAModifier     &                child%var%array3(childarray(i,1,2),
ChildarrayAModifier     &                                 childarray(j,2,2),
ChildarrayAModifier     &                                 childarray(k,3,2)) = 
ChildarrayAModifier     &                tempC%var%array3(i,j,k)
             enddo
             enddo
             enddo
        CASE (4)
             do i = pttruetab(1),cetruetab(1)
             do j = pttruetab(2),cetruetab(2)
             do k = pttruetab(3),cetruetab(3)
             do l = pttruetab(4),cetruetab(4)
ChildarrayAModifier                         if (restore%var%restore4D(i,j,k,l) == 0) 
ChildarrayAModifier     &                      child%var%array4(childarray(i,1,2),
ChildarrayAModifier     &                                       childarray(j,2,2),
ChildarrayAModifier     &                                       childarray(k,3,2),
ChildarrayAModifier     &                                       childarray(l,4,2)) = 
ChildarrayAModifier     &                      tempC%var%array4(i,j,k,l)
             enddo
             enddo
             enddo
             enddo
        CASE (5)
             do i = pttruetab(1),cetruetab(1)
             do j = pttruetab(2),cetruetab(2)
             do k = pttruetab(3),cetruetab(3)
             do l = pttruetab(4),cetruetab(4)
             do m = pttruetab(5),cetruetab(5)
ChildarrayAModifier              if (restore%var%restore5D(i,j,k,l,m) == 0) 
ChildarrayAModifier     &                child%var%array5(childarray(i,1,2),
ChildarrayAModifier     &                                 childarray(j,2,2),
ChildarrayAModifier     &                                 childarray(k,3,2),
ChildarrayAModifier     &                                 childarray(l,4,2),
ChildarrayAModifier     &                                 childarray(m,5,2)) = 
ChildarrayAModifier     &                tempC%var%array5(i,j,k,l,m)
             enddo
             enddo
             enddo
             enddo
             enddo
        CASE (6)
             do i = pttruetab(1),cetruetab(1)
             do j = pttruetab(2),cetruetab(2)
             do k = pttruetab(3),cetruetab(3)
             do l = pttruetab(4),cetruetab(4)
             do m = pttruetab(5),cetruetab(5)
             do n = pttruetab(6),cetruetab(6)
ChildarrayAModifier              if (restore%var%restore6D(i,j,k,l,m,n) == 0) 
ChildarrayAModifier     &                child%var%array6(childarray(i,1,2),
ChildarrayAModifier     &                                 childarray(j,2,2),
ChildarrayAModifier     &                                 childarray(k,3,2),
ChildarrayAModifier     &                                 childarray(l,4,2),
ChildarrayAModifier     &                                 childarray(m,5,2),
ChildarrayAModifier     &                                 childarray(n,6,2)) = 
ChildarrayAModifier     &                tempC%var%array6(i,j,k,l,m,n)
             enddo
             enddo
             enddo
             enddo
             enddo
             enddo
        END SELECT
C
#else
        SELECT CASE (nbdim)
        CASE (1)
           do i = pttruetab(1),cetruetab(1)         
            if (restore%var%restore1D(i) == 0)
     &            child % var % array1(i) = 
     &            tempC % var % array1(i)    
          enddo
        CASE (2)
           do j = pttruetab(2),cetruetab(2)
             do i = pttruetab(1),cetruetab(1) 
              if (restore%var%restore2D(i,j) == 0)     
     &              child % var % array2(i,j) = 
     &              tempC % var % array2(i,j)    
              enddo
             enddo
        CASE (3)
           do k = pttruetab(3),cetruetab(3)
           do j = pttruetab(2),cetruetab(2)
             do i = pttruetab(1),cetruetab(1) 
              if (restore%var%restore3D(i,j,k) == 0)
     &                  child % var % array3(i,j,k) =
     &                  tempC % var % array3(i,j,k)    
                  enddo
              enddo
             enddo
        CASE (4)
           do l = pttruetab(4),cetruetab(4)
           do k = pttruetab(3),cetruetab(3)
          do j = pttruetab(2),cetruetab(2)
             do i = pttruetab(1),cetruetab(1)
                if (restore%var%restore4D(i,j,k,l) == 0)
     &                 child % var % array4(i,j,k,l) = 
     &                 tempC % var % array4(i,j,k,l)    
             enddo
             enddo
              enddo
             enddo
        CASE (5)
           do m = pttruetab(5),cetruetab(5)
          do l = pttruetab(4),cetruetab(4)
         do k = pttruetab(3),cetruetab(3)
           do j = pttruetab(2),cetruetab(2)
             do i = pttruetab(1),cetruetab(1)
                if (restore%var%restore5D(i,j,k,l,m) == 0)
     &                  child % var % array5(i,j,k,l,m) = 
     &                  tempC % var % array5(i,j,k,l,m)    
             enddo
             enddo
                  enddo
              enddo
             enddo
        CASE (6)
           do n = pttruetab(6),cetruetab(6)
          do m = pttruetab(5),cetruetab(5)
          do l = pttruetab(4),cetruetab(4)
         do k = pttruetab(3),cetruetab(3)
          do j = pttruetab(2),cetruetab(2)
             do i = pttruetab(1),cetruetab(1)
                if (restore%var%restore6D(i,j,k,l,m,n) == 0)
     &                      child % var % array6(i,j,k,l,m,n) = 
     &                      tempC % var % array6(i,j,k,l,m,n)    
             enddo
            enddo
                      enddo
                  enddo
              enddo
             enddo
        END SELECT
C
#endif
C        
        else
C
C
          IF (memberin) THEN
          SELECT CASE (nbdim)
          CASE (1)
            child%var%array1(childarray(1,1,2):childarray(1,2,2)) =
     &       tempC%var%array1(childarray(1,1,1):childarray(1,2,1))
          CASE (2)
            child%var%array2(childarray(1,1,2):childarray(1,2,2),
     &                       childarray(2,1,2):childarray(2,2,2)) =
     &      tempC%var%array2(childarray(1,1,1):childarray(1,2,1),
     &                       childarray(2,1,1):childarray(2,2,1))
          CASE (3)
            child%var%array3(childarray(1,1,2):childarray(1,2,2),
     &                       childarray(2,1,2):childarray(2,2,2),
     &                       childarray(3,1,2):childarray(3,2,2)) =
     &      tempC%var%array3(childarray(1,1,1):childarray(1,2,1),
     &                       childarray(2,1,1):childarray(2,2,1),
     &                       childarray(3,1,1):childarray(3,2,1))
          CASE (4)
            child%var%array4(childarray(1,1,2):childarray(1,2,2),
     &                       childarray(2,1,2):childarray(2,2,2),
     &                       childarray(3,1,2):childarray(3,2,2),
     &                       childarray(4,1,2):childarray(4,2,2)) =
     &      tempC%var%array4(childarray(1,1,1):childarray(1,2,1),
     &                       childarray(2,1,1):childarray(2,2,1),
     &                       childarray(3,1,1):childarray(3,2,1),
     &                       childarray(4,1,1):childarray(4,2,1))
          CASE (5)
            child%var%array5(childarray(1,1,2):childarray(1,2,2),
     &                       childarray(2,1,2):childarray(2,2,2),
     &                       childarray(3,1,2):childarray(3,2,2),
     &                       childarray(4,1,2):childarray(4,2,2),
     &                       childarray(5,1,2):childarray(5,2,2)) =
     &      tempC%var%array5(childarray(1,1,1):childarray(1,2,1),
     &                       childarray(2,1,1):childarray(2,2,1),
     &                       childarray(3,1,1):childarray(3,2,1),
     &                       childarray(4,1,1):childarray(4,2,1),
     &                       childarray(5,1,1):childarray(5,2,1))
          CASE (6)
            child%var%array6(childarray(1,1,2):childarray(1,2,2),
     &                       childarray(2,1,2):childarray(2,2,2),
     &                       childarray(3,1,2):childarray(3,2,2),
     &                       childarray(4,1,2):childarray(4,2,2),
     &                       childarray(5,1,2):childarray(5,2,2),
     &                       childarray(6,1,2):childarray(6,2,2)) =
     &      tempC%var%array6(childarray(1,1,1):childarray(1,2,1),
     &                       childarray(2,1,1):childarray(2,2,1),
     &                       childarray(3,1,1):childarray(3,2,1),
     &                       childarray(4,1,1):childarray(4,2,1),
     &                       childarray(5,1,1):childarray(5,2,1),
     &                       childarray(6,1,1):childarray(6,2,1))
          END SELECT
          ENDIF
C
C       
      endif

        Call Agrif_nbdim_deallocation(tempPextend%var,nbdim)
C        deallocate(tempPextend%var)

      Call Agrif_nbdim_deallocation(tempC%var,nbdim)
      
C      Deallocate(tempC % var)
      ELSE
      
C      deallocate(tempPextend%var)

      ENDIF
C
C             
C     Deallocations
#ifdef AGRIF_MPI        
      IF (member) THEN
      Call Agrif_nbdim_deallocation(tempP%var,nbdim)
C      Deallocate(tempP % var)
      endif
#endif
C
C
      
C
C
      End Subroutine Agrif_InterpnD 
C
C
C
C                  
C
C     **************************************************************************
CCC   Subroutine Agrif_Parentbounds
C     **************************************************************************
C
      Subroutine Agrif_Parentbounds(TYPEinterp,nbdim,indmin,indmax,
     &                              s_Parent_temp,
     &                              s_Child_temp,s_Child,ds_Child,
     &                              s_Parent,ds_Parent,
     &                              pttruetab,cetruetab,pttab_Child,
     &                              pttab_Parent,posvar,interptab)
C
CCC   Description:
CCC   Subroutine calculating the bounds of the parent grid for the interpolation
CCC   of the child grid     
C
C
C     Declarations:
C
C
C     Arguments
      INTEGER :: nbdim
      INTEGER, DIMENSION(6) :: TypeInterp
      INTEGER,DIMENSION(nbdim) :: indmin,indmax
      REAL,DIMENSION(nbdim) :: s_Parent_temp,s_child_temp
      REAL,DIMENSION(nbdim) :: s_Child,ds_child
      REAL,DIMENSION(nbdim) :: s_Parent,ds_Parent
      INTEGER,DIMENSION(nbdim) :: pttruetab,cetruetab
      INTEGER,DIMENSION(nbdim) :: pttab_Child,pttab_Parent
      INTEGER,DIMENSION(nbdim) :: posvar
      CHARACTER(6), DIMENSION(nbdim) :: interptab
C
C     Local variables
      INTEGER :: i
      REAL,DIMENSION(nbdim) :: dim_newmin,dim_newmax      
C
      dim_newmin = s_Child + (pttruetab - pttab_Child) * ds_Child
      dim_newmax = s_Child + (cetruetab - pttab_Child) * ds_Child
      
      DO i = 1,nbdim         
C     
        indmin(i) = pttab_Parent(i) + 
     &         agrif_int((dim_newmin(i)-s_Parent(i))/ds_Parent(i))
C
        indmax(i) = pttab_Parent(i) + 
     &                agrif_ceiling((dim_newmax(i)-
     &                s_Parent(i))/ds_Parent(i))
     
C
C
C       Necessary for the Quadratic interpolation
C  

        IF ((pttruetab(i) == cetruetab(i)) .AND. 
     &                           (posvar(i) == 1)) THEN
        ELSEIF (interptab(i) .EQ. 'N') THEN
        ELSEIF ( TYPEinterp(i) .eq. Agrif_ppm .or.
     &      TYPEinterp(i) .eq. Agrif_eno  .or.
     &      TYPEinterp(i) .eq. Agrif_weno) THEN            
           indmin(i) = indmin(i) - 2  
           indmax(i) = indmax(i) + 2                  
        ELSE IF (( TYPEinterp(i) .ne. Agrif_constant )
     &        .AND.( TYPEinterp(i) .ne. Agrif_linear )) THEN
           indmin(i) = indmin(i) - 1  
           indmax(i) = indmax(i) + 1
        ENDIF
        

C        
       ENDDO 
C
        s_Parent_temp = s_Parent + (indmin - pttab_Parent) * ds_Parent
C     
        s_Child_temp = s_Child + (pttruetab - pttab_Child) * ds_Child
C
C
      Return
C
C
      End Subroutine Agrif_Parentbounds
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_1D_Recursive 
C     **************************************************************************
C
      Subroutine Agrif_Interp_1D_recursive(TypeInterp,tabin,tabout,
     &           indmin,indmax, 
     &           pttab_child,petab_child,
     &           s_child,s_parent,ds_child,ds_parent,nbdim)     
C
CCC   Description:
CCC   Subroutine for the interpolation of a 1D grid variable. 
CCC   It calls Agrif_InterpBase. 
C
C     Declarations:
C
      
C
C     Arguments
      INTEGER :: nbdim
      INTEGER,DIMENSION(1) :: TypeInterp
      INTEGER, DIMENSION(nbdim) :: indmin,indmax
      INTEGER, DIMENSION(nbdim) :: pttab_child,petab_child
      REAL, DIMENSION(nbdim) :: s_child,s_parent
      REAL, DIMENSION(nbdim) :: ds_child,ds_parent
      REAL, INTENT(IN),DIMENSION(indmin(nbdim):indmax(nbdim)) :: tabin        
      REAL, INTENT(OUT),
     &      DIMENSION(pttab_child(nbdim):petab_child(nbdim)) :: tabout
      INTEGER :: coeffraf
C
C
C     Commentaire perso : nbdim vaut toujours 1 ici. 
C
      coeffraf = nint(ds_parent(nbdim)/ds_child(nbdim))
      
      Call Agrif_InterpBase(TypeInterp(1),
     &                  tabin(indmin(nbdim):indmax(nbdim)),
     &                  tabout(pttab_child(nbdim):petab_child(nbdim)),
     &                  indmin(nbdim),indmax(nbdim),            
     &                  pttab_child(nbdim),petab_child(nbdim),
     &                  s_parent(nbdim),s_child(nbdim),
     &                  ds_parent(nbdim),ds_child(nbdim),coeffraf)
                
C                
      Return
C
C
      End Subroutine Agrif_Interp_1D_recursive
C
C
C      
C     **************************************************************************
CCC   Subroutine Agrif_Interp_2D_Recursive 
C     **************************************************************************
C
      Subroutine Agrif_Interp_2D_recursive(TypeInterp,
     &           tabin,tabout,
     &           indmin,indmax,   
     &           pttab_child,petab_child,
     &            s_child, s_parent,
     &           ds_child,ds_parent,
     &           nbdim)
C
CCC   Description:
CCC   Subroutine for the interpolation of a 2D grid variable. 
CCC   It calls Agrif_Interp_1D_recursive and Agrif_InterpBase.    
C
C     Declarations:
C
      
C     
      INTEGER                   :: nbdim
      INTEGER,DIMENSION(2)      :: TypeInterp
      INTEGER, DIMENSION(nbdim) :: indmin,indmax
      INTEGER, DIMENSION(nbdim) :: pttab_child,petab_child
      REAL   , DIMENSION(nbdim) ::  s_child, s_parent
      REAL   , DIMENSION(nbdim) :: ds_child,ds_parent
      REAL   ,INTENT(IN), DIMENSION(
     &                indmin(nbdim-1):indmax(nbdim-1),
     &                indmin(nbdim):indmax(nbdim)
     &                ) :: tabin        
      REAL   ,INTENT(OUT), DIMENSION(
     &                pttab_child(nbdim-1):petab_child(nbdim-1),
     &                pttab_child(nbdim):petab_child(nbdim)
     &                ) :: tabout
C
C     Local variables      
      REAL, DIMENSION(pttab_child(nbdim-1):petab_child(nbdim-1),
     &                 indmin(nbdim):indmax(nbdim)) :: tabtemp
      INTEGER i,j
      INTEGER :: coeffraf
      REAL   , DIMENSION(
     &                pttab_child(nbdim):petab_child(nbdim),
     &                pttab_child(nbdim-1):petab_child(nbdim-1)
     &                ) :: tabout_trsp
      REAL, DIMENSION(indmin(nbdim):indmax(nbdim),
     &        pttab_child(nbdim-1):petab_child(nbdim-1)) :: tabtemp_trsp

C
C
C
C
C     Commentaire perso : nbdim vaut toujours 2 ici.
C
      coeffraf = nint ( ds_parent(1) / ds_child(1) )
      IF((TypeInterp(1) == Agrif_Linear) .AND. (coeffraf /= 1 ) )THEN

!---CDIR NEXPAND
          IF(.NOT. precomputedone(1) ) call linear1Dprecompute2D(
     &          indmax(2)-indmin(2)+1,   
     &          indmax(1)-indmin(1)+1,   
     &          petab_child(1)-pttab_child(1)+1,
     &          s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
!---CDIR NEXPAND
           call linear1daftercompute(tabin,tabtemp,
     &          size(tabin), size(tabtemp),  
     &          s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
 
      ELSEIF((TypeInterp(1) == Agrif_PPM) .AND. (coeffraf /= 1 ) )THEN
!---CDIR NEXPAND
          IF(.NOT. precomputedone(1) ) call ppm1Dprecompute2D(
     &          indmax(2)-indmin(2)+1,   
     &          indmax(1)-indmin(1)+1,   
     &          petab_child(1)-pttab_child(1)+1,
     &          s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
!---CDIR NEXPAND
           call ppm1daftercompute(tabin,tabtemp,
     &          size(tabin), size(tabtemp),  
     &          s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)

      ELSE

      do j = indmin(nbdim),indmax(nbdim)
C        
!---CDIR NEXPAND
        Call Agrif_Interp_1D_recursive(TypeInterp(1),
     &         tabin(indmin(nbdim-1):indmax(nbdim-1),j),
     &         tabtemp(pttab_child(nbdim-1):petab_child(nbdim-1),j),
     &         indmin(1:nbdim-1),indmax(1:nbdim-1),
     &         pttab_child(1:nbdim-1),petab_child(1:nbdim-1),
     &         s_child(1:nbdim-1),s_parent(1:nbdim-1),
     &         ds_child(1:nbdim-1),ds_parent(1:nbdim-1),nbdim-1)
C        
      enddo
      ENDIF   

      coeffraf = nint(ds_parent(nbdim)/ds_child(nbdim))
      
      tabtemp_trsp = TRANSPOSE(tabtemp)

      IF((TypeInterp(2) == Agrif_Linear) .AND. (coeffraf /= 1 ) )THEN

!---CDIR NEXPAND
          IF(.NOT. precomputedone(2) ) call linear1Dprecompute2D(
     &          petab_child(1)-pttab_child(1)+1,
     &          indmax(2)-indmin(2)+1,
     &          petab_child(2)-pttab_child(2)+1,
     &          s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)
!---CDIR NEXPAND
           call linear1daftercompute(tabtemp_trsp,tabout_trsp,
     &          size(tabtemp_trsp), size(tabout_trsp),
     &          s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)

      ELSEIF((TypeInterp(2) == Agrif_PPM) .AND. (coeffraf /= 1 ) )THEN

!---CDIR NEXPAND
           IF(.NOT. precomputedone(1) )call ppm1Dprecompute2D(
     &          petab_child(1)-pttab_child(1)+1,
     &          indmax(2)-indmin(2)+1,
     &          petab_child(2)-pttab_child(2)+1,
     &          s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)
!---CDIR NEXPAND
           call ppm1daftercompute(tabtemp_trsp,tabout_trsp,
     &          size(tabtemp_trsp), size(tabout_trsp),
     &          s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)

      ELSE
      do i=pttab_child(nbdim-1),petab_child(nbdim-1)
C
!---CDIR NEXPAND
        Call Agrif_InterpBase(TypeInterp(2),
     &           tabtemp_trsp(indmin(nbdim):indmax(nbdim),i),
     &           tabout_trsp(pttab_child(nbdim):petab_child(nbdim),i),
     &           indmin(nbdim),indmax(nbdim),
     &           pttab_child(nbdim),petab_child(nbdim),
     &           s_parent(nbdim),s_child(nbdim),
     &           ds_parent(nbdim),ds_child(nbdim),coeffraf)

C        
      enddo
      ENDIF
      
      tabout = TRANSPOSE(tabout_trsp)
C
      Return
C
C
      End Subroutine Agrif_Interp_2D_recursive
C
C
C      
C     **************************************************************************
CCC   Subroutine Agrif_Interp_3D_Recursive 
C     **************************************************************************
C
      Subroutine Agrif_Interp_3D_recursive(TypeInterp,tabin,tabout,
     &           indmin,indmax,   
     &           pttab_child,petab_child,
     &           s_child,s_parent,ds_child,ds_parent,nbdim)
C
CCC   Description:
CCC   Subroutine for the interpolation of a 3D grid variable. 
CCC   It calls Agrif_Interp_2D_recursive and Agrif_InterpBase.    
C
C     Declarations:
C
      
C     
      INTEGER :: nbdim
      INTEGER,DIMENSION(3) :: TypeInterp
      INTEGER, DIMENSION(nbdim) :: indmin,indmax
      INTEGER, DIMENSION(nbdim) :: pttab_child,petab_child
      REAL, DIMENSION(nbdim) :: s_child,s_parent,ds_child,ds_parent
      REAL,INTENT(IN), DIMENSION(indmin(nbdim-2):indmax(nbdim-2),
     &                indmin(nbdim-1):indmax(nbdim-1),
     &                indmin(nbdim)  :indmax(nbdim)) :: tabin        
      REAL,INTENT(OUT),
     &        DIMENSION(pttab_child(nbdim-2):petab_child(nbdim-2),
     &                pttab_child(nbdim-1):petab_child(nbdim-1),
     &                pttab_child(nbdim):petab_child(nbdim)) :: tabout
C
C     Local variables      
      REAL, DIMENSION(pttab_child(nbdim-2):petab_child(nbdim-2),
     &                 pttab_child(nbdim-1):petab_child(nbdim-1),
     &                 indmin(nbdim):indmax(nbdim)) :: tabtemp
      INTEGER i,j,k
      INTEGER :: coeffraf, locind_child_left, kdeb
C
C
      coeffraf = nint ( ds_parent(1) / ds_child(1) )
      IF((TypeInterp(1) == Agrif_Linear) .AND. (coeffraf/=1))THEN 
        Call linear1Dprecompute2D(
     &          indmax(2)-indmin(2)+1,
     &          indmax(1)-indmin(1)+1,
     &          petab_child(1)-pttab_child(1)+1,
     &          s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
      precomputedone(1) = .TRUE. 
      ELSEIF((TypeInterp(1) == Agrif_PPM) .AND. (coeffraf/=1))THEN
        Call ppm1Dprecompute2D(
     &          indmax(2)-indmin(2)+1,
     &          indmax(1)-indmin(1)+1,
     &          petab_child(1)-pttab_child(1)+1,
     &          s_parent(1),s_child(1),ds_parent(1),ds_child(1),1)
      precomputedone(1) = .TRUE.
      ENDIF

      coeffraf = nint ( ds_parent(2) / ds_child(2) )
      IF((TypeInterp(2) == Agrif_Linear) .AND. (coeffraf/=1)) THEN
         Call linear1Dprecompute2D(
     &          petab_child(1)-pttab_child(1)+1,
     &          indmax(2)-indmin(2)+1,
     &          petab_child(2)-pttab_child(2)+1,
     &          s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)
      precomputedone(2) = .TRUE. 
      ELSEIF((TypeInterp(2) == Agrif_PPM) .AND. (coeffraf/=1)) THEN
         Call ppm1Dprecompute2D(
     &          petab_child(1)-pttab_child(1)+1,
     &          indmax(2)-indmin(2)+1,
     &          petab_child(2)-pttab_child(2)+1,
     &          s_parent(2),s_child(2),ds_parent(2),ds_child(2),2)
      precomputedone(2) = .TRUE.
      ENDIF

      do k = indmin(nbdim),indmax(nbdim)
C        
        Call Agrif_Interp_2D_recursive(TypeInterp(1:2),
     &         tabin(indmin(nbdim-2):indmax(nbdim-2),
     &         indmin(nbdim-1):indmax(nbdim-1),k),
     &         tabtemp(pttab_child(nbdim-2):petab_child(nbdim-2),
     &         pttab_child(nbdim-1):petab_child(nbdim-1),k),
     &         indmin(1:nbdim-1),indmax(1:nbdim-1),
     &         pttab_child(1:nbdim-1),petab_child(1:nbdim-1),
     &         s_child(1:nbdim-1),s_parent(1:nbdim-1),
     &         ds_child(1:nbdim-1),ds_parent(1:nbdim-1),nbdim-1)
C        
      enddo
      
      precomputedone(1) = .FALSE.
      precomputedone(2) = .FALSE.
      coeffraf = nint ( ds_parent(3) / ds_child(3) )

      Call Agrif_Compute_nbdim_interp(s_parent(nbdim),s_child(nbdim),
     &  ds_parent(nbdim),ds_child(nbdim),coeffraf,locind_child_left)
           
      IF (coeffraf == 1) THEN
      
      kdeb = indmin(3)+locind_child_left-2
      do k=pttab_child(3),petab_child(3)
      kdeb = kdeb + 1
      do j = pttab_child(2),petab_child(2)
        do i = pttab_child(1),petab_child(1)
        tabout(i,j,k) = tabtemp(i,j,kdeb)
      enddo
      enddo
      enddo
              
      ELSE      
C
      do j=pttab_child(nbdim-1),petab_child(nbdim-1) 
C        
        do i=pttab_child(nbdim-2),petab_child(nbdim-2)
C
          Call Agrif_InterpBase(TypeInterp(3),
     &           tabtemp(i,j,indmin(nbdim):indmax(nbdim)),
     &           tabout(i,j,pttab_child(nbdim):petab_child(nbdim)),
     &           indmin(nbdim),indmax(nbdim),
     &           pttab_child(nbdim),petab_child(nbdim),
     &           s_parent(nbdim),s_child(nbdim),
     &           ds_parent(nbdim),ds_child(nbdim),coeffraf)
C
        enddo 
C       
      enddo
      ENDIF
C
      Return
C        
C
      End Subroutine Agrif_Interp_3D_recursive
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_4D_Recursive 
C     **************************************************************************
C
      Subroutine Agrif_Interp_4D_recursive(TypeInterp,tabin,tabout,
     &           indmin,indmax,   
     &           pttab_child,petab_child,
     &           s_child,s_parent,ds_child,ds_parent,nbdim)
C
CCC   Description:
CCC   Subroutine for the interpolation of a 4D grid variable. 
CCC   It calls Agrif_Interp_3D_recursive and Agrif_InterpBase.    
C
C     Declarations:
C
      
C     
      INTEGER :: nbdim
      INTEGER,DIMENSION(4) :: TypeInterp
      INTEGER, DIMENSION(nbdim) :: indmin,indmax
      INTEGER, DIMENSION(nbdim) :: pttab_child,petab_child
      REAL, DIMENSION(nbdim) :: s_child,s_parent,ds_child,ds_parent
      REAL,INTENT(IN), DIMENSION(indmin(nbdim-3):indmax(nbdim-3),
     &                indmin(nbdim-2):indmax(nbdim-2),
     &                indmin(nbdim-1):indmax(nbdim-1),
     &                indmin(nbdim):indmax(nbdim)) :: tabin        
      REAL,INTENT(OUT),
     &       DIMENSION(pttab_child(nbdim-3):petab_child(nbdim-3),
     &                pttab_child(nbdim-2):petab_child(nbdim-2),
     &                pttab_child(nbdim-1):petab_child(nbdim-1),
     &                pttab_child(nbdim):petab_child(nbdim)) :: tabout
C
C     Local variables      
      REAL, DIMENSION(pttab_child(nbdim-3):petab_child(nbdim-3),
     &                 pttab_child(nbdim-2):petab_child(nbdim-2),
     &                 pttab_child(nbdim-1):petab_child(nbdim-1),  
     &                 indmin(nbdim):indmax(nbdim)) :: tabtemp
      INTEGER i,j,k,l
      INTEGER :: coeffraf
C
C
      do l = indmin(nbdim),indmax(nbdim)
C        
        Call Agrif_Interp_3D_recursive(TypeInterp(1:3),
     &         tabin(indmin(nbdim-3):indmax(nbdim-3),
     &               indmin(nbdim-2):indmax(nbdim-2),
     &               indmin(nbdim-1):indmax(nbdim-1),l),
     &         tabtemp(pttab_child(nbdim-3):petab_child(nbdim-3),
     &         pttab_child(nbdim-2):petab_child(nbdim-2),
     &         pttab_child(nbdim-1):petab_child(nbdim-1),l),
     &         indmin(1:nbdim-1),indmax(1:nbdim-1),
     &         pttab_child(1:nbdim-1),petab_child(1:nbdim-1),
     &         s_child(1:nbdim-1),s_parent(1:nbdim-1),
     &         ds_child(1:nbdim-1),ds_parent(1:nbdim-1),nbdim-1)
C        
      enddo
C
      coeffraf = nint(ds_parent(nbdim)/ds_child(nbdim))
      
      do k = pttab_child(nbdim-1),petab_child(nbdim-1)
C
        do j = pttab_child(nbdim-2),petab_child(nbdim-2) 
C        
          do i = pttab_child(nbdim-3),petab_child(nbdim-3)
C
            Call Agrif_InterpBase(TypeInterp(4),
     &           tabtemp(i,j,k,indmin(nbdim):indmax(nbdim)),
     &           tabout(i,j,k,pttab_child(nbdim):petab_child(nbdim)),
     &           indmin(nbdim),indmax(nbdim),
     &           pttab_child(nbdim),petab_child(nbdim),
     &           s_parent(nbdim),s_child(nbdim),
     &           ds_parent(nbdim),ds_child(nbdim),coeffraf)
C
          enddo
C
        enddo 
C       
      enddo
C
      Return
C
C        
      End Subroutine Agrif_Interp_4D_recursive
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_5D_Recursive 
C     **************************************************************************
C
      Subroutine Agrif_Interp_5D_recursive(TypeInterp,tabin,tabout,
     &           indmin,indmax,   
     &           pttab_child,petab_child,
     &           s_child,s_parent,ds_child,ds_parent,nbdim)
C
CCC   Description:
CCC   Subroutine for the interpolation of a 5D grid variable. 
CCC   It calls Agrif_Interp_4D_recursive and Agrif_InterpBase.    
C
C     Declarations:
C
      
C     
      INTEGER :: nbdim
      INTEGER,DIMENSION(5) :: TypeInterp
      INTEGER, DIMENSION(nbdim) :: indmin,indmax
      INTEGER, DIMENSION(nbdim) :: pttab_child,petab_child
      REAL, DIMENSION(nbdim) :: s_child,s_parent,ds_child,ds_parent
      REAL,INTENT(IN), DIMENSION(indmin(nbdim-4):indmax(nbdim-4),
     &                indmin(nbdim-3):indmax(nbdim-3),
     &                indmin(nbdim-2):indmax(nbdim-2),
     &                indmin(nbdim-1):indmax(nbdim-1),
     &                indmin(nbdim):indmax(nbdim)) :: tabin  
      REAL,INTENT(OUT),
     &    DIMENSION(pttab_child(nbdim-4):petab_child(nbdim-4),
     &                pttab_child(nbdim-3):petab_child(nbdim-3),
     &                pttab_child(nbdim-2):petab_child(nbdim-2),
     &                pttab_child(nbdim-1):petab_child(nbdim-1),
     &                pttab_child(nbdim):petab_child(nbdim)) :: tabout
C
C     Local variables      
      REAL, DIMENSION(pttab_child(nbdim-4):petab_child(nbdim-4),
     &                 pttab_child(nbdim-3):petab_child(nbdim-3),
     &                 pttab_child(nbdim-2):petab_child(nbdim-2),
     &                 pttab_child(nbdim-1):petab_child(nbdim-1),    
     &                 indmin(nbdim):indmax(nbdim)) :: tabtemp
      INTEGER i,j,k,l,m
      INTEGER :: coeffraf
C
C
      do m = indmin(nbdim),indmax(nbdim)
C        
        Call Agrif_Interp_4D_recursive(TypeInterp(1:4),
     &         tabin(indmin(nbdim-4):indmax(nbdim-4),
     &               indmin(nbdim-3):indmax(nbdim-3),
     &               indmin(nbdim-2):indmax(nbdim-2),
     &               indmin(nbdim-1):indmax(nbdim-1),m),
     &         tabtemp(pttab_child(nbdim-4):petab_child(nbdim-4),
     &                 pttab_child(nbdim-3):petab_child(nbdim-3),
     &                 pttab_child(nbdim-2):petab_child(nbdim-2),
     &                 pttab_child(nbdim-1):petab_child(nbdim-1),m),
     &         indmin(1:nbdim-1),indmax(1:nbdim-1),
     &         pttab_child(1:nbdim-1),petab_child(1:nbdim-1),
     &         s_child(1:nbdim-1),s_parent(1:nbdim-1),
     &         ds_child(1:nbdim-1),ds_parent(1:nbdim-1),nbdim-1)
C        
      enddo
      
      coeffraf = nint(ds_parent(nbdim)/ds_child(nbdim))
C
      do l = pttab_child(nbdim-1),petab_child(nbdim-1) 
C
        do k = pttab_child(nbdim-2),petab_child(nbdim-2)
C
          do j = pttab_child(nbdim-3),petab_child(nbdim-3) 
C        
            do i = pttab_child(nbdim-4),petab_child(nbdim-4)
C
              Call Agrif_InterpBase(TypeInterp(5),
     &             tabtemp(i,j,k,l,indmin(nbdim):indmax(nbdim)),
     &                    tabout(i,j,k,l,
     &             pttab_child(nbdim):petab_child(nbdim)),
     &             indmin(nbdim),indmax(nbdim),
     &             pttab_child(nbdim),petab_child(nbdim),
     &             s_parent(nbdim),s_child(nbdim),
     &             ds_parent(nbdim),ds_child(nbdim),coeffraf)
C
            enddo
C
          enddo
C
        enddo 
C       
      enddo
C
C
      Return
C
C        
      End Subroutine Agrif_Interp_5D_recursive
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_6D_Recursive 
C     **************************************************************************
C
      Subroutine Agrif_Interp_6D_recursive(TypeInterp,tabin,tabout,
     &           indmin,indmax,   
     &           pttab_child,petab_child,
     &           s_child,s_parent,ds_child,ds_parent,nbdim)
C
CCC   Description:
CCC   Subroutine for the interpolation of a 6D grid variable. 
CCC   It calls Agrif_Interp_4D_recursive and Agrif_InterpBase.    
C
C     Declarations:
C
      
C     
      INTEGER :: nbdim
      INTEGER,DIMENSION(6) :: TypeInterp
      INTEGER, DIMENSION(nbdim) :: indmin,indmax
      INTEGER, DIMENSION(nbdim) :: pttab_child,petab_child
      REAL, DIMENSION(nbdim) :: s_child,s_parent,ds_child,ds_parent
      REAL,INTENT(IN), DIMENSION(indmin(nbdim-5):indmax(nbdim-5),
     &                indmin(nbdim-4):indmax(nbdim-4),
     &                indmin(nbdim-3):indmax(nbdim-3), 
     &                indmin(nbdim-2):indmax(nbdim-2),
     &                indmin(nbdim-1):indmax(nbdim-1),
     &                indmin(nbdim):indmax(nbdim)) :: tabin        
      REAL,INTENT(OUT),
     &    DIMENSION(pttab_child(nbdim-5):petab_child(nbdim-5),
     &                pttab_child(nbdim-4):petab_child(nbdim-4),
     &                pttab_child(nbdim-3):petab_child(nbdim-3),
     &                pttab_child(nbdim-2):petab_child(nbdim-2),
     &                pttab_child(nbdim-1):petab_child(nbdim-1),
     &                pttab_child(nbdim):petab_child(nbdim)) :: tabout
C
C     Local variables      
      REAL, DIMENSION(pttab_child(nbdim-5):petab_child(nbdim-5),
     &                 pttab_child(nbdim-4):petab_child(nbdim-4),
     &                 pttab_child(nbdim-3):petab_child(nbdim-3),
     &                 pttab_child(nbdim-2):petab_child(nbdim-2),    
     &                 pttab_child(nbdim-1):petab_child(nbdim-1),    
     &                 indmin(nbdim):indmax(nbdim)) :: tabtemp
      INTEGER i,j,k,l,m,n
      INTEGER :: coeffraf
C
C        
C
      do n = indmin(nbdim),indmax(nbdim)
C        
        Call Agrif_Interp_5D_recursive(TypeInterp(1:5),
     &         tabin(indmin(nbdim-5):indmax(nbdim-5),
     &               indmin(nbdim-4):indmax(nbdim-4),
     &               indmin(nbdim-3):indmax(nbdim-3),
     &               indmin(nbdim-2):indmax(nbdim-2),
     &               indmin(nbdim-1):indmax(nbdim-1),n),
     &         tabtemp(pttab_child(nbdim-5):petab_child(nbdim-5),
     &                 pttab_child(nbdim-4):petab_child(nbdim-4),
     &                 pttab_child(nbdim-3):petab_child(nbdim-3),
     &                 pttab_child(nbdim-2):petab_child(nbdim-2),
     &                 pttab_child(nbdim-1):petab_child(nbdim-1),n),
     &         indmin(1:nbdim-1),indmax(1:nbdim-1),
     &         pttab_child(1:nbdim-1),petab_child(1:nbdim-1),
     &         s_child(1:nbdim-1),s_parent(1:nbdim-1),
     &         ds_child(1:nbdim-1),ds_parent(1:nbdim-1),nbdim-1)
C        
      enddo
      
      coeffraf = nint(ds_parent(nbdim)/ds_child(nbdim))
C
      do m = pttab_child(nbdim-1),petab_child(nbdim-1) 
      do l = pttab_child(nbdim-2),petab_child(nbdim-2) 
C
        do k = pttab_child(nbdim-3),petab_child(nbdim-3)
C
          do j = pttab_child(nbdim-4),petab_child(nbdim-4) 
C        
            do i = pttab_child(nbdim-5),petab_child(nbdim-5)
C
              Call Agrif_InterpBase(TypeInterp(6),
     &             tabtemp(i,j,k,l,m,indmin(nbdim):indmax(nbdim)),
     &                    tabout(i,j,k,l,m,
     &                    pttab_child(nbdim):petab_child(nbdim)),
     &             indmin(nbdim),indmax(nbdim),
     &             pttab_child(nbdim),petab_child(nbdim),
     &             s_parent(nbdim),s_child(nbdim),
     &             ds_parent(nbdim),ds_child(nbdim),coeffraf)
C
            enddo
C
          enddo
C
        enddo 
C       
      enddo
      enddo
C                
C
      Return
C
C        
      End Subroutine Agrif_Interp_6D_recursive
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_InterpBase  
C     **************************************************************************
C  
      Subroutine Agrif_InterpBase(TypeInterp,
     &                           parenttab,childtab,
     &                           indmin,indmax,pttab_child,petab_child,
     &                           s_parent,s_child,ds_parent,ds_child,
     &                           coeffraf)   
C
CCC   Description:
CCC   Subroutine calling the interpolation method chosen by the user (linear, 
CCC   lagrange or spline). 
C
C     Declarations:
C
      
C
      INTEGER                :: TypeInterp
      INTEGER :: indmin,indmax
      INTEGER :: pttab_child,petab_child
      REAL,INTENT(IN),DIMENSION(indmin:indmax)           :: parenttab       
      REAL,INTENT(OUT),DIMENSION(pttab_child:petab_child) :: childtab        
      REAL    :: s_parent,s_child,ds_parent,ds_child 
      INTEGER :: coeffraf
C 
C
       IF ((indmin == indmax).AND.(pttab_child == petab_child)) THEN
         childtab(pttab_child) = parenttab(indmin)
       ELSEIF (TYPEinterp .EQ. AGRIF_LINEAR) then    
C
C         Linear interpolation 
  
          Call linear1D
     &         (parenttab,childtab,
     &          indmax-indmin+1,petab_child-pttab_child+1,
     &          s_parent,s_child,ds_parent,ds_child)
C          
      elseif ( TYPEinterp .EQ. AGRIF_PPM ) then

          Call ppm1D
     &         (parenttab,childtab,
     &         indmax-indmin+1,petab_child-pttab_child+1,
     &         s_parent,s_child,ds_parent,ds_child)
C

        elseif (TYPEinterp .EQ. AGRIF_LAGRANGE) then
C          
C         Lagrange interpolation    
          Call lagrange1D
     &        (parenttab,childtab,
     &         indmax-indmin+1,petab_child-pttab_child+1,
     &         s_parent,s_child,ds_parent,ds_child)
C            
        elseif (TYPEinterp .EQ. AGRIF_ENO) then
C          
C         Eno interpolation
          Call eno1D
     &         (parenttab,childtab,
     &         indmax-indmin+1,petab_child-pttab_child+1,
     &         s_parent,s_child,ds_parent,ds_child)
C        
        elseif (TYPEinterp .EQ. AGRIF_WENO) then
C          
C         Eno interpolation
          Call weno1D
     &         (parenttab,childtab,
     &         indmax-indmin+1,petab_child-pttab_child+1,
     &         s_parent,s_child,ds_parent,ds_child)
C            
        Else if (TYPEinterp .EQ. AGRIF_LINEARCONSERV) then
C          
C         Linear conservative interpolation
          
          Call linear1Dconserv
     &         (parenttab,childtab,
     &         indmax-indmin+1,petab_child-pttab_child+1,
     &         s_parent,s_child,ds_parent,ds_child)   
C              
        Else if (TYPEinterp .EQ. AGRIF_LINEARCONSERVLIM) then
C          
C         Linear conservative interpolation
          
          Call linear1Dconservlim
     &         (parenttab,childtab,
     &         indmax-indmin+1,petab_child-pttab_child+1,
     &         s_parent,s_child,ds_parent,ds_child)         
C              
        elseif (TYPEinterp .EQ. AGRIF_CONSTANT) then
C          
          Call constant1D
     &         (parenttab,childtab,
     &         indmax-indmin+1,petab_child-pttab_child+1,
     &         s_parent,s_child,ds_parent,ds_child)
C              
      endif 
C
C      
      End Subroutine Agrif_InterpBase 
C

      Subroutine Agrif_Compute_nbdim_interp(s_parent,s_child,
     &  ds_parent,ds_child,coeffraf,locind_child_left)
      real :: s_parent,s_child,ds_parent,ds_child
      integer :: coeffraf,locind_child_left
     
      coeffraf = nint(ds_parent/ds_child)
      locind_child_left = 1 + agrif_int((s_child-s_parent)/ds_parent)
      End Subroutine Agrif_Compute_nbdim_interp
C          

      Subroutine Agrif_Find_list_interp(list_interp,pttab,petab,
     &                          pttab_Child,pttab_Parent,nbdim,
     &                          indmin,indmax,indminglob,
     &      indmaxglob,indminglob2,indmaxglob2,parentarray,
     &       pttruetab,cetruetab,member,memberin,
     &      find_list_interp
#if defined AGRIF_MPI
     &     ,tab4t,memberinall,sendtoproc1,recvfromproc1
#endif
     &    )     
      TYPE(Agrif_List_Interp_Loc), Pointer :: list_interp
      INTEGER :: nbdim
      INTEGER,DIMENSION(nbdim)   :: pttab,petab,pttab_Child,pttab_Parent
      LOGICAL :: find_list_interp
      Type(Agrif_List_Interp_loc), Pointer :: parcours
      INTEGER,DIMENSION(nbdim)   :: indmin,indmax  
      INTEGER,DIMENSION(nbdim)   :: indminglob,indmaxglob
      INTEGER,DIMENSION(nbdim)   :: pttruetab,cetruetab      
      INTEGER,DIMENSION(nbdim)   :: indminglob2,indmaxglob2  
      INTEGER,DIMENSION(nbdim,2,2) :: parentarray
      LOGICAL :: member, memberin
      INTEGER :: i
#ifdef AGRIF_MPI
C
      INTEGER,DIMENSION(nbdim,0:Agrif_Nbprocs-1,8) :: tab4t
      LOGICAL, DIMENSION(0:Agrif_Nbprocs-1) :: memberinall
      LOGICAL, DIMENSION(0:Agrif_Nbprocs-1) :: sendtoproc1,recvfromproc1
#endif 
                    
      find_list_interp = .FALSE.
      
      parcours => list_interp
      Find_loop :   Do While (associated(parcours))
        Do i=1,nbdim
          IF ((pttab(i) /= parcours%interp_loc%pttab(i)).OR.
     &        (petab(i) /= parcours%interp_loc%petab(i)).OR.
     &        (pttab_child(i) /= parcours%interp_loc%pttab_child(i)).OR.
     &        (pttab_parent(i) /= parcours%interp_loc%pttab_parent(i)))
     &               THEN
            parcours=>parcours%suiv
            Cycle Find_loop
          ENDIF
        EndDo

        indmin = parcours%interp_loc%indmin(1:nbdim)
        indmax = parcours%interp_loc%indmax(1:nbdim)
        
        pttruetab = parcours%interp_loc%pttruetab(1:nbdim)
        cetruetab = parcours%interp_loc%cetruetab(1:nbdim)
                
#if !defined AGRIF_MPI
        indminglob = parcours%interp_loc%indminglob(1:nbdim)
        indmaxglob = parcours%interp_loc%indmaxglob(1:nbdim)
#else
        indminglob2 = parcours%interp_loc%indminglob2(1:nbdim)
        indmaxglob2 = parcours%interp_loc%indmaxglob2(1:nbdim)
        parentarray = parcours%interp_loc%parentarray(1:nbdim,:,:)
        member = parcours%interp_loc%member
        tab4t = parcours%interp_loc%tab4t(1:nbdim,0:Agrif_Nbprocs-1,1:8)
        memberinall = parcours%interp_loc%memberinall(0:Agrif_Nbprocs-1)
        sendtoproc1 = parcours%interp_loc%sendtoproc1(0:Agrif_Nbprocs-1)
        recvfromproc1 = 
     &    parcours%interp_loc%recvfromproc1(0:Agrif_Nbprocs-1)
#endif        
        memberin = parcours%interp_loc%memberin
        find_list_interp = .TRUE.   
        Exit Find_loop
      End Do Find_loop  
                              
      End Subroutine Agrif_Find_list_interp   
      
      Subroutine Agrif_AddTo_list_interp(list_interp,pttab,petab,
     &                          pttab_Child,pttab_Parent,indmin,indmax,
     &                          indminglob,indmaxglob,
     &                          indminglob2,indmaxglob2,
     &                          parentarray,pttruetab,cetruetab,
     &                          member,memberin,nbdim
#if defined AGRIF_MPI
     &      ,tab4t,memberinall,sendtoproc1,recvfromproc1
#endif
     &    )
          
      TYPE(Agrif_List_Interp_Loc), Pointer :: list_interp
      INTEGER :: nbdim
      INTEGER,DIMENSION(nbdim)   :: pttab,petab,pttab_Child,pttab_Parent
      INTEGER,DIMENSION(nbdim)   :: indmin,indmax
      INTEGER,DIMENSION(nbdim)   :: indminglob,indmaxglob
      INTEGER,DIMENSION(nbdim)   :: indminglob2,indmaxglob2
      INTEGER,DIMENSION(nbdim)   :: pttruetab,cetruetab
      INTEGER,DIMENSION(nbdim,2,2) :: parentarray
      LOGICAL :: member, memberin
#ifdef AGRIF_MPI
C
      INTEGER,DIMENSION(nbdim,0:Agrif_Nbprocs-1,8) :: tab4t
      LOGICAL,DIMENSION(0:Agrif_Nbprocs-1) :: memberinall
      LOGICAL,DIMENSION(0:Agrif_Nbprocs-1) :: sendtoproc1
      LOGICAL,DIMENSION(0:Agrif_Nbprocs-1) :: recvfromproc1
#endif                   
      Type(Agrif_List_Interp_loc), Pointer :: parcours
            
       Allocate(parcours)
      Allocate(parcours%interp_loc)
      
      parcours%interp_loc%pttab(1:nbdim) = pttab(1:nbdim)
      parcours%interp_loc%petab(1:nbdim) = petab(1:nbdim)
      parcours%interp_loc%pttab_child(1:nbdim) = pttab_child(1:nbdim)
      parcours%interp_loc%pttab_parent(1:nbdim) = pttab_parent(1:nbdim)
  
  
      parcours%interp_loc%indmin(1:nbdim) = indmin(1:nbdim)
      parcours%interp_loc%indmax(1:nbdim) = indmax(1:nbdim)

      parcours%interp_loc%memberin = memberin
#if !defined AGRIF_MPI
      parcours%interp_loc%indminglob(1:nbdim) = indminglob(1:nbdim)
      parcours%interp_loc%indmaxglob(1:nbdim) = indmaxglob(1:nbdim)
#else
      parcours%interp_loc%indminglob2(1:nbdim) = indminglob2(1:nbdim)
      parcours%interp_loc%indmaxglob2(1:nbdim) = indmaxglob2(1:nbdim)
      parcours%interp_loc%parentarray(1:nbdim,:,:) 
     &       = parentarray(1:nbdim,:,:)
      parcours%interp_loc%member = member
      Allocate(parcours%interp_loc%tab4t(nbdim,0:Agrif_Nbprocs-1,8))
      Allocate(parcours%interp_loc%memberinall(0:Agrif_Nbprocs-1))
      Allocate(parcours%interp_loc%sendtoproc1(0:Agrif_Nbprocs-1))
      Allocate(parcours%interp_loc%recvfromproc1(0:Agrif_Nbprocs-1))                  
      parcours%interp_loc%tab4t=tab4t   
      parcours%interp_loc%memberinall=memberinall   
      parcours%interp_loc%sendtoproc1=sendtoproc1
      parcours%interp_loc%recvfromproc1=recvfromproc1            
#endif      

      parcours%interp_loc%pttruetab(1:nbdim) = pttruetab(1:nbdim)
      parcours%interp_loc%cetruetab(1:nbdim) = cetruetab(1:nbdim)
      
      parcours%suiv => list_interp
      
      list_interp => parcours
      End Subroutine Agrif_Addto_list_interp 
                
      End Module Agrif_Interpolation
