!
! $Id: modbcfunction.F 779 2007-12-22 17:04:17Z rblod $
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
C     Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
C
C
C
CCC   Module AGRIF_bcfunction
C
C 
      Module  Agrif_bcfunction
CCC   Description:
CCC   
C
C     Modules used:
C  
      Use Agrif_Boundary
      Use Agrif_Update
      Use Agrif_fluxmod
      Use Agrif_Save
C             
      IMPLICIT NONE
C
      interface Agrif_Bc_variable
          module procedure Agrif_Bc_variable0d,
     &                     Agrif_Bc_variable1d,
     &                     Agrif_Bc_variable2d,
     &                     Agrif_Bc_variable3d,
     &                     Agrif_Bc_variable4d,
     &                     Agrif_Bc_variable5d
      end interface       
C
      interface Agrif_Set_Parent
          module procedure Agrif_Set_Parent_int,
     &                     Agrif_Set_Parent_real
      end interface       
C
      interface Agrif_Interp_variable
          module procedure Agrif_Interp_var0d,
     &                     Agrif_Interp_var1d,
     &                     Agrif_Interp_var2d,
     &                     Agrif_Interp_var3d,
     &                     Agrif_Interp_var4d,
     &                     Agrif_Interp_var5d
      end interface       
C
      interface Agrif_Init_variable
          module procedure Agrif_Init_variable0d,
     &                     Agrif_Init_variable1d,
     &                     Agrif_Init_variable2d,
     &                     Agrif_Init_variable3d,
     &                     Agrif_Init_variable4d
      end interface       
C
      interface Agrif_update_variable
          module procedure Agrif_update_var0d,
     &                     Agrif_update_var1d,
     &                     Agrif_update_var2d,
     &                     Agrif_update_var3d,
     &                     Agrif_update_var4d,
     &                     Agrif_update_var5d
      end interface      
      
      interface Agrif_Save_Forrestore
         module procedure Agrif_Save_Forrestore0d,    
     &                    Agrif_Save_Forrestore2d,    
     &                    Agrif_Save_Forrestore3d,    
     &                    Agrif_Save_Forrestore4d
      end interface
C
      Contains 
C
C     **************************************************************************
CCC   Subroutine Agrif_Set_type
C     **************************************************************************
C 
      Subroutine Agrif_Set_type(tabvarsindic,posvar,point)
C
CCC   Description:
CCC   To set the TYPE of the variable.
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
      INTEGER, DIMENSION(:) :: posvar
      INTEGER, DIMENSION(:) :: point
C
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
      INTEGER :: dimensio ! DIMENSION of the variable
      INTEGER :: i
C
C
C     Begin 
C
      dimensio = Agrif_Mygrid % tabvars(tabvarsindic) %var % nbdim
C
      if (.not.associated(Agrif_Mygrid % tabvars(tabvarsindic)
     &                                 %var % posvar)) then
      Allocate( 
     & Agrif_Mygrid % tabvars(tabvarsindic)%var % posvar(dimensio))
      endif 
            
      do i = 1 , dimensio
         Agrif_Mygrid % tabvars(tabvarsindic) %var % posvar(i)
     &                       = posvar(i)
         Agrif_Mygrid % tabvars(tabvarsindic) %var % point(i) 
     &                       = point(i)
      enddo
C
C
      End Subroutine Agrif_Set_type
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Set_parent_int
C     **************************************************************************
C 
      Subroutine Agrif_Set_parent_int(tabvarsindic,value)
C
CCC   Description:
CCC   To set the TYPE of the variable.
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
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
      INTEGER :: Value
C
C     Begin 
C
      Agrif_Curgrid % parent % tabvars(tabvarsindic) % 
     &         var % iarray0 = value
C
C
      End Subroutine Agrif_Set_parent_int
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Set_parent_real
C     **************************************************************************
C 
      Subroutine Agrif_Set_parent_real(tabvarsindic,value)
C
CCC   Description:
CCC   To set the TYPE of the variable.
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
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
      REAL :: Value
C
C     Begin 
C
      Agrif_Curgrid % parent % tabvars(tabvarsindic) % 
     &          var % array0 = value
C
C
      End Subroutine Agrif_Set_parent_real
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Set_raf
C     **************************************************************************
C 
      Subroutine Agrif_Set_raf(tabvarsindic,tabraf)
C
CCC   Description:
CCC   Attention tabraf est de taille trois si on ne raffine pas suivant z la
CCC             troisieme entree du tableau tabraf est 'N'
C
C     Modules used:
C      

C
C     Declarations:
C      
C     Arguments      
C
      CHARACTER(*) ,DIMENSION(:) :: tabraf
C
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
      INTEGER :: dimensio ! DIMENSION of the variable
      INTEGER :: i
C
C
C     Begin 
C
      dimensio = Agrif_Mygrid % tabvars(tabvarsindic) %var % nbdim
C        
      if (.not.associated(Agrif_Mygrid % tabvars(tabvarsindic)
     &                                 %var % interptab)) then 
      Allocate(
     & Agrif_Mygrid % tabvars(tabvarsindic)%var% interptab(dimensio))
      endif

      do i = 1 , dimensio
         Agrif_Mygrid % tabvars(tabvarsindic) %var % interptab(i) 
     &                 = TRIM(tabraf(i))
      enddo
C
      End Subroutine Agrif_Set_raf
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Set_bc
C     **************************************************************************
C 
      Subroutine Agrif_Set_bc(tabvarsindic,point,
     &          Interpolationshouldbemade)
C
CCC   Description:
CCC
C
C     Modules used:
C      

C
C     Declarations:
C      
C     Arguments      
C
      INTEGER, DIMENSION(2) :: point
      LOGICAL, OPTIONAL :: Interpolationshouldbemade
C
      INTEGER :: tabvarsindic, indic ! indice of the variable in tabvars
      TYPE(Agrif_PVariable),Pointer ::tabvars
      
    
C
C
C     Begin 
C
C     

      indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif
      
      if (indic <=0) then
      tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
      else
      tabvars=>Agrif_Curgrid % tabvars(indic)
      endif  
      
      if (Agrif_Curgrid % fixedrank .NE. 0) then  
       IF (.Not.Associated(tabvars%var% interpIndex)) THEN
        Allocate(tabvars%var % interpIndex)
          tabvars%var % interpIndex = -1

        Allocate(tabvars%var % oldvalues2D(2,1))
          tabvars%var % oldvalues2D = 0. 
       ENDIF      
       if ( PRESENT(Interpolationshouldbemade) ) then
         tabvars%var %
     &     Interpolationshouldbemade = Interpolationshouldbemade
       endif

      endif
C
      tabvars%var % bcinf = point(1)
      tabvars%var % bcsup = point(2)
C
      End Subroutine Agrif_Set_bc
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Set_interp
C     **************************************************************************
C 
      Subroutine Agrif_Set_interp(tabvarsindic,interp,interp1,interp2,
     &                interp3)
C
CCC   Description:
C
C     Declarations:
C      
C     Arguments      
C
      INTEGER, OPTIONAL      :: interp,interp1,interp2,interp3
C
      INTEGER :: tabvarsindic, indic ! indice of the variable in tabvars
      TYPE(Agrif_PVariable),Pointer ::tabvars
      
    
C
C
C     Begin 
C
C     
      indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif 
      
      if (indic <=0) then
      tabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
      else
      tabvars=>Agrif_Mygrid % tabvars(indic)
      endif      
C
C     Begin 
C
      tabvars % var % Typeinterp = 
     &    Agrif_Constant
      IF (present(interp)) THEN
      tabvars % var % Typeinterp = 
     &           interp
      ENDIF
      IF (present(interp1)) THEN
      tabvars % var % Typeinterp(1) = 
     &           interp1
      ENDIF
      IF (present(interp2)) THEN
      tabvars % var % Typeinterp(2) = 
     &           interp2
      ENDIF
      IF (present(interp3)) THEN
      tabvars % var % Typeinterp(3) = 
     &           interp3
      ENDIF
C
      End Subroutine Agrif_Set_interp
C
C     **************************************************************************
CCC   Subroutine Agrif_Set_bcinterp
C     **************************************************************************
C 
      Subroutine Agrif_Set_bcinterp(tabvarsindic,interp,interp1,
     &      interp2,interp3,interp11,interp12,interp21,interp22)
C
CCC   Description:

C
C     Modules used:
C      

C
C     Declarations:
C      
C     Arguments      
C
      INTEGER, OPTIONAL      :: interp,interp1,interp2,interp3
      INTEGER, OPTIONAL      :: interp11,interp12,interp21,interp22
C
      INTEGER :: tabvarsindic, indic ! indice of the variable in tabvars
      TYPE(Agrif_PVariable),Pointer ::tabvars
      
    
C
C
C     Begin 
C
C     

      indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif 
      
      if (indic <=0) then
      tabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
      else
      tabvars=>Agrif_Mygrid % tabvars(indic)
      endif 
C
      tabvars% var % bctypeinterp = 
     &           Agrif_Constant   
      IF (present(interp)) THEN
      tabvars% var % bctypeinterp = 
     &           interp
      ENDIF       
      IF (present(interp1)) THEN
      tabvars% var % bctypeinterp(1:2,1) = 
     &           interp1
      ENDIF       
      IF (present(interp11)) THEN
      tabvars% var % bctypeinterp(1,1) = 
     &           interp11
      ENDIF
      IF (present(interp12)) THEN
      tabvars% var % bctypeinterp(1,2) = 
     &           interp12
      ENDIF          
      IF (present(interp2)) THEN
      tabvars% var % bctypeinterp(1:2,2) = 
     &           interp2
      ENDIF
      IF (present(interp21)) THEN
      tabvars% var % bctypeinterp(2,1) = 
     &           interp21
      ENDIF      
      IF (present(interp22)) THEN
      tabvars% var % bctypeinterp(2,2) = 
     &           interp22
      ENDIF            
      IF (present(interp3)) THEN
      tabvars% var % bctypeinterp(1:2,3) =
     &           interp3
      ENDIF
C
      End Subroutine Agrif_Set_bcinterp
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Set_Update
C     **************************************************************************
C 
      Subroutine Agrif_Set_Update(tabvarsindic,point)
C
CCC   Description:
CCC
C
C     Modules used:
C      

C
C     Declarations:
C      
C     Arguments      
C
      INTEGER, DIMENSION(2) :: point
C
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
C
C
C     Begin 
C
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updateinf = point(1)
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updatesup = point(2)
C
      End Subroutine Agrif_Set_Update
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Set_UpdateType
C     **************************************************************************
C 
      Subroutine Agrif_Set_UpdateType(tabvarsindic,
     &                                  update,update1,update2,
     &                                  update3,update4,update5)
C
CCC   Description:

C
C     Modules used:
C      

C
C     Declarations:
C      
C     Arguments      
C
      INTEGER, OPTIONAL           :: update, update1,
     &       update2, update3,update4,update5
C
      INTEGER :: tabvarsindic,indic ! indice of the variable in tabvars
      TYPE(Agrif_PVariable),Pointer :: roottabvars      
C
C
C     Begin 

      indic = tabvarsindic

      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif 
      
      if (indic <=0) then
      roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)      
      else
      roottabvars => Agrif_Mygrid % tabvars(indic)
      endif
      
C
      roottabvars% var % typeupdate = 
     &                   Agrif_Update_Copy
      
      IF (present(update)) THEN
        roottabvars% var % typeupdate = 
     &           update
      ENDIF
      IF (present(update1)) THEN
        roottabvars% var % typeupdate(1) = 
     &           update1
      ENDIF  
      IF (present(update2)) THEN
        roottabvars% var % typeupdate(2) = 
     &           update2
      ENDIF  
      IF (present(update3)) THEN
        roottabvars% var % typeupdate(3) = 
     &           update3
      ENDIF 
      IF (present(update4)) THEN
        roottabvars% var % typeupdate(4) = 
     &           update4
      ENDIF       
      IF (present(update5)) THEN
        roottabvars% var % typeupdate(5) = 
     &           update5
      ENDIF                  
C
      End Subroutine Agrif_Set_UpdateType           
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Set_restore
C     **************************************************************************
C 
      Subroutine Agrif_Set_restore(tabvarsindic)
C
CCC   Description:
CCC   
C
C     Modules used:
C      

C
C     Declarations:
C      
C     Arguments      
C
      INTEGER :: tabvarsindic,indic ! indice of the variable in tabvars
C
C     Begin 
C
      indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif  
C
      Agrif_Mygrid%tabvars(indic)%var % restaure = .TRUE.
C
      End Subroutine Agrif_Set_restore
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Init_variable0d
C     **************************************************************************
      Subroutine Agrif_Init_variable0d(tabvarsindic0,tabvarsindic,
     &        procname)

      INTEGER :: tabvarsindic0 ! indice of the variable in tabvars
      INTEGER :: tabvarsindic,indic ! indice of the variable in tabvars
      External :: procname
      Optional ::  procname
C
      if (Agrif_Root()) Return
C      
      indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif 
      
      if (present(procname)) then
      CALL Agrif_Interp_variable(tabvarsindic0,indic,procname)
      CALL Agrif_Bc_variable(tabvarsindic0,indic,1.,procname)
      else
      CALL Agrif_Interp_variable(tabvarsindic0,indic)
      CALL Agrif_Bc_variable(tabvarsindic0,indic,1.)
      endif

      End Subroutine Agrif_Init_variable0d
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Init_variable1d
C     **************************************************************************
      Subroutine Agrif_Init_variable1d(q,tabvarsindic,procname)

      REAL, DIMENSION(:) :: q
      INTEGER :: tabvarsindic, indic ! indice of the variable in tabvars
      External :: procname
      Optional ::  procname

C
      if (Agrif_Root()) Return
C      
      indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif      
C
      if (present(procname)) then
      CALL Agrif_Interp_variable(q,indic,procname)
      CALL Agrif_Bc_variable(q,indic,1.,procname)
      else
      CALL Agrif_Interp_variable(q,indic)
      CALL Agrif_Bc_variable(q,indic,1.)
      endif

      End Subroutine Agrif_Init_variable1d
C
C     **************************************************************************
CCC   Subroutine Agrif_Init_variable2d
C     **************************************************************************
      Subroutine Agrif_Init_variable2d(q,tabvarsindic,procname)

      REAL,  DIMENSION(:,:) :: q
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
      External :: procname
      Optional ::  procname
      integer :: indic

C
      if (Agrif_Root()) Return
C
      indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif 
      
      if (present(procname)) then
      CALL Agrif_Interp_variable(q,indic,procname)
      CALL Agrif_Bc_variable(q,indic,1.,procname)
      else
      CALL Agrif_Interp_variable(q,indic)
      CALL Agrif_Bc_variable(q,indic,1.)
      endif


      End Subroutine Agrif_Init_variable2d
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Init_variable3d
C     **************************************************************************
      Subroutine Agrif_Init_variable3d(q,tabvarsindic,procname)

      REAL,  DIMENSION(:,:,:) :: q
      INTEGER :: tabvarsindic,indic ! indice of the variable in tabvars
      External :: procname
      Optional ::  procname
C
      if (Agrif_Root()) Return
C      
      indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif      
C
      if (present(procname)) then
      CALL Agrif_Interp_variable(q,indic,procname)
      CALL Agrif_Bc_variable(q,indic,1.,procname)
      else
      CALL Agrif_Interp_variable(q,indic)
      CALL Agrif_Bc_variable(q,indic,1.)
      endif

C
      End Subroutine Agrif_Init_variable3d
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Init_variable4d
C     **************************************************************************
      Subroutine Agrif_Init_variable4d(q,tabvarsindic,procname)

      REAL,  DIMENSION(:,:,:,:) :: q
      INTEGER :: tabvarsindic, indic ! indice of the variable in tabvars
      External :: procname
      Optional ::  procname
C
      if (Agrif_Root()) Return
C      
      indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif       
C
      if (present(procname)) then
      CALL Agrif_Interp_variable(q,indic,procname)
      CALL Agrif_Bc_variable(q,indic,1.,procname)
      else
      CALL Agrif_Interp_variable(q,indic)
      CALL Agrif_Bc_variable(q,indic,1.)
      endif

C
      End Subroutine Agrif_Init_variable4d      
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Bc_variable0d
C     **************************************************************************
      Subroutine Agrif_Bc_variable0d(tabvarsindic0,tabvarsindic,
     &                               calledweight,procname)

      INTEGER :: tabvarsindic0 ! indice of the variable in tabvars
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
C        
      External :: procname
      Optional ::  procname
      REAL, OPTIONAL :: calledweight
      REAL    :: weight
      LOGICAL :: pweight
C
      INTEGER :: dimensio      

      if (Agrif_Root()) Return
C
      dimensio =  Agrif_Mygrid % tabvars(tabvarsindic) % var % nbdim    
C
      if ( PRESENT(calledweight) ) then
        weight=calledweight      
        pweight = .TRUE.
      else
        weight = 0.
        pweight = .FALSE.
      endif
C      
C

      
      if ( dimensio .EQ. 1 ) Call Agrif_Interp_Bc_1D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % bctypeinterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) %var % array1,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcsup,
     & weight,
     & pweight)
C
      if ( dimensio .EQ. 2 ) then
      IF (present(procname)) THEN
      Call Agrif_Interp_Bc_2D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % bctypeinterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) %var % array2,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcsup,
     & weight,pweight,procname)
      ELSE
          
      Call Agrif_Interp_Bc_2D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % bctypeinterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) %var % array2,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcsup,
     & weight,pweight)
      ENDIF
      endif
C
      if ( dimensio .EQ. 3 ) then
      IF (present(procname)) THEN

      Call Agrif_Interp_Bc_3D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % bctypeinterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) %var % array3,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcsup,
     & weight,pweight,procname)      
      ELSE
      Call Agrif_Interp_Bc_3D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % bctypeinterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) %var % array3,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcsup,
     & weight,pweight)
      ENDIF
      endif
C
      if ( dimensio .EQ. 4 ) then
      IF (present(procname)) THEN
      Call Agrif_Interp_Bc_4D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % bctypeinterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) %var % array4,     
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcsup,
     & weight,pweight,procname)      
      ELSE
      Call Agrif_Interp_Bc_4D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % bctypeinterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) %var % array4,     
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcsup,
     & weight,pweight)
      ENDIF
      endif
C
      if ( dimensio .EQ. 5 ) then
      IF (present(procname)) THEN
      Call Agrif_Interp_Bc_5D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % bctypeinterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) %var % array5,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcsup,
     & weight,pweight,procname)      
      ELSE
      Call Agrif_Interp_Bc_5D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % bctypeinterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) %var % array5,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcsup,
     & weight,pweight)
      ENDIF
      endif
C
      if ( dimensio .EQ. 6 ) Call Agrif_Interp_Bc_6D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % bctypeinterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) %var % array6,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcsup,
     & weight,
     & pweight)
C
C
      End Subroutine Agrif_Bc_variable0d
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Bc_variable1d
C     **************************************************************************
      Subroutine Agrif_Bc_variable1d(q,tabvarsindic,calledweight,
     &                               procname)

      REAL   , Dimension(:)          :: q
      External :: procname
      Optional ::  procname
      INTEGER :: tabvarsindic, indic ! indice of the variable in tabvars
C        
      REAL, OPTIONAL :: calledweight
      REAL    :: weight
      LOGICAL :: pweight
      TYPE(Agrif_PVariable),Pointer ::tabvars,parenttabvars,roottabvars
C
C      
C      
      If (Agrif_Root()) Return
C      
      indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif         
      
      if ( PRESENT(calledweight) ) then
        weight=calledweight      
        pweight = .TRUE.
      else
        weight = 0.
        pweight = .FALSE.
      endif
      
      if (indic <=0) then
      tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
      parenttabvars => tabvars%parent_var
      roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
      else
      tabvars=>Agrif_Curgrid % tabvars(indic)
      parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
      roottabvars => Agrif_Mygrid % tabvars(indic)
      endif
            
      IF (present(procname)) THEN
      Call Agrif_Interp_Bc_1D(
     & roottabvars % var % bctypeinterp,
     & parenttabvars,
     & tabvars,q,
     & tabvars % var % bcinf,
     & tabvars % var % bcsup,
     & weight,pweight,procname)      
      ELSE
      Call Agrif_Interp_Bc_1D(
     & roottabvars % var % bctypeinterp,
     & parenttabvars,
     & tabvars,q,
     & tabvars % var % bcinf,
     & tabvars % var % bcsup,
     & weight,pweight)
      ENDIF
      End Subroutine Agrif_Bc_variable1d 
      
C
C     **************************************************************************
CCC   Subroutine Agrif_Bc_variable2d
C     **************************************************************************
      Subroutine Agrif_Bc_variable2d(q,tabvarsindic,calledweight,
     &                               procname)

      REAL   , Dimension(:,:)          :: q
      External :: procname
      Optional ::  procname
      INTEGER :: tabvarsindic, indic ! indice of the variable in tabvars
C        
      REAL, OPTIONAL :: calledweight
      REAL    :: weight
      LOGICAL :: pweight
      TYPE(Agrif_PVariable),Pointer ::tabvars,parenttabvars,roottabvars
C
C      
C      
      If (Agrif_Root()) Return
C      
      indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif       
      
      if ( PRESENT(calledweight) ) then
        weight=calledweight      
        pweight = .TRUE.
      else
        weight = 0.
        pweight = .FALSE.
      endif
      
      if (indic <=0) then
      tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
      parenttabvars => tabvars%parent_var
      roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
      else
      tabvars=>Agrif_Curgrid % tabvars(indic)
      parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
      roottabvars => Agrif_Mygrid % tabvars(indic)
      endif
            
      IF (present(procname)) THEN
      Call Agrif_Interp_Bc_2D(
     & roottabvars % var % bctypeinterp,
     & parenttabvars,
     & tabvars,q,
     & tabvars % var % bcinf,
     & tabvars % var % bcsup,
     & weight,pweight,procname)      
      ELSE
      Call Agrif_Interp_Bc_2D(
     & roottabvars % var % bctypeinterp,
     & parenttabvars,
     & tabvars,q,
     & tabvars % var % bcinf,
     & tabvars % var % bcsup,
     & weight,pweight)
      ENDIF
      End Subroutine Agrif_Bc_variable2d
            
C
C     **************************************************************************
CCC   Subroutine Agrif_Bc_variable3d
C     **************************************************************************
      Subroutine Agrif_Bc_variable3d(q,tabvarsindic,calledweight,
     &                               procname)

      REAL   , Dimension(:,:,:)          :: q
      External :: procname
      Optional ::  procname
      INTEGER :: tabvarsindic, indic ! indice of the variable in tabvars
C        
      REAL, OPTIONAL :: calledweight
      REAL    :: weight
      LOGICAL :: pweight
      TYPE(Agrif_PVariable),Pointer ::tabvars,parenttabvars,roottabvars
C
C      
C      
      If (Agrif_Root()) Return
C      
      indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif       
      
      if ( PRESENT(calledweight) ) then
        weight=calledweight      
        pweight = .TRUE.
      else
        weight = 0.
        pweight = .FALSE.
      endif
      
      if (indic <=0) then
      tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
      parenttabvars => tabvars%parent_var
      roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
      else
      tabvars=>Agrif_Curgrid % tabvars(indic)
      parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
      roottabvars => Agrif_Mygrid % tabvars(indic)
      endif
            
      IF (present(procname)) THEN
      Call Agrif_Interp_Bc_3D(
     & roottabvars % var % bctypeinterp,
     & parenttabvars,
     & tabvars,q,
     & tabvars % var % bcinf,
     & tabvars % var % bcsup,
     & weight,pweight,procname)      
      ELSE
      Call Agrif_Interp_Bc_3D(
     & roottabvars % var % bctypeinterp,
     & parenttabvars,
     & tabvars,q,
     & tabvars % var % bcinf,
     & tabvars % var % bcsup,
     & weight,pweight)
      ENDIF
      End Subroutine Agrif_Bc_variable3d
      
C
C     **************************************************************************
CCC   Subroutine Agrif_Bc_variable4d
C     **************************************************************************
      Subroutine Agrif_Bc_variable4d(q,tabvarsindic,calledweight,
     &                               procname)

      REAL   , Dimension(:,:,:,:)          :: q
      External :: procname
      Optional ::  procname
      INTEGER :: tabvarsindic, indic ! indice of the variable in tabvars
C        
      REAL, OPTIONAL :: calledweight
      REAL    :: weight
      LOGICAL :: pweight
      TYPE(Agrif_PVariable),Pointer ::tabvars,parenttabvars,roottabvars
C
C      
C      
      If (Agrif_Root()) Return
      
C      
      indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif      
      
      if ( PRESENT(calledweight) ) then
        weight=calledweight      
        pweight = .TRUE.
      else
        weight = 0.
        pweight = .FALSE.
      endif
      
      if (indic <=0) then
      tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
      parenttabvars => tabvars%parent_var
      roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
      else
      tabvars=>Agrif_Curgrid % tabvars(indic)
      parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
      roottabvars => Agrif_Mygrid % tabvars(indic)
      endif
            
      IF (present(procname)) THEN
      Call Agrif_Interp_Bc_4D(
     & roottabvars % var % bctypeinterp,
     & parenttabvars,
     & tabvars,q,
     & tabvars % var % bcinf,
     & tabvars % var % bcsup,
     & weight,pweight,procname)      
      ELSE
      Call Agrif_Interp_Bc_4D(
     & roottabvars % var % bctypeinterp,
     & parenttabvars,
     & tabvars,q,
     & tabvars % var % bcinf,
     & tabvars % var % bcsup,
     & weight,pweight)
      ENDIF
      End Subroutine Agrif_Bc_variable4d
            
C
C     **************************************************************************
CCC   Subroutine Agrif_Bc_variable5d
C     **************************************************************************
      Subroutine Agrif_Bc_variable5d(q,tabvarsindic,calledweight,
     &                               procname)

      REAL   , Dimension(:,:,:,:,:)          :: q
      External :: procname
      Optional ::  procname
      INTEGER :: tabvarsindic, indic ! indice of the variable in tabvars
C        
      REAL, OPTIONAL :: calledweight
      REAL    :: weight
      LOGICAL :: pweight
      TYPE(Agrif_PVariable),Pointer ::tabvars,parenttabvars,roottabvars
C
C      
C      
      If (Agrif_Root()) Return
C      
      indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif      
      
      if ( PRESENT(calledweight) ) then
        weight=calledweight      
        pweight = .TRUE.
      else
        weight = 0.
        pweight = .FALSE.
      endif
      
      if (indic <=0) then
      tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
      parenttabvars => tabvars%parent_var
      roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
      else
      tabvars=>Agrif_Curgrid % tabvars(indic)
      parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
      roottabvars => Agrif_Mygrid % tabvars(indic)
      endif
            
      IF (present(procname)) THEN
      Call Agrif_Interp_Bc_5d(
     & roottabvars % var % bctypeinterp,
     & parenttabvars,
     & tabvars,q,
     & tabvars % var % bcinf,
     & tabvars % var % bcsup,
     & weight,pweight,procname)      
      ELSE
      Call Agrif_Interp_Bc_5d(
     & roottabvars % var % bctypeinterp,
     & parenttabvars,
     & tabvars,q,
     & tabvars % var % bcinf,
     & tabvars % var % bcsup,
     & weight,pweight)
      ENDIF
      End Subroutine Agrif_Bc_variable5d
      
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_var0D
C     **************************************************************************
C 
      Subroutine Agrif_Interp_var0d(tabvarsindic0,tabvarsindic,procname)

      INTEGER :: tabvarsindic0 ! indice of the variable in tabvars
      INTEGER :: tabvarsindic, indic  ! indice of the variable in tabvars
      INTEGER :: dimensio  ! indice of the variable in tabvars
      External :: procname
      Optional ::  procname
C      
      if (Agrif_Root()) Return
C      
      indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif       
C     
      dimensio = Agrif_Mygrid % tabvars(indic) % var % nbdim 
C
      if ( dimensio .EQ. 1 ) then
       if (present(procname)) then
       Call Agrif_Interp_1D(
     & Agrif_Mygrid % tabvars(indic) % var %  TypeInterp,
     & Agrif_Curgrid % parent % tabvars(indic),
     & Agrif_Curgrid % tabvars(indic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array1 ,     
     & Agrif_Mygrid % tabvars(indic) % var % restaure,
     & Agrif_Mygrid % tabvars(indic) %var % nbdim,procname)
       else
       Call Agrif_Interp_1D(
     & Agrif_Mygrid % tabvars(indic) % var %  TypeInterp,
     & Agrif_Curgrid % parent % tabvars(indic),
     & Agrif_Curgrid % tabvars(indic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array1 ,     
     & Agrif_Mygrid % tabvars(indic) % var % restaure,
     & Agrif_Mygrid % tabvars(indic) %var % nbdim)
       endif
       endif
C
      if ( dimensio .EQ. 2 ) then
      if (present(procname)) then
       Call Agrif_Interp_2D(
     & Agrif_Mygrid % tabvars(indic) % var %  TypeInterp,
     & Agrif_Curgrid % parent % tabvars(indic),
     & Agrif_Curgrid % tabvars(indic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array2 ,     
     & Agrif_Mygrid % tabvars(indic) % var % restaure,
     & Agrif_Mygrid % tabvars(indic) %var % nbdim,procname)
      else
       Call Agrif_Interp_2D(
     & Agrif_Mygrid % tabvars(indic) % var %  TypeInterp,
     & Agrif_Curgrid % parent % tabvars(indic),
     & Agrif_Curgrid % tabvars(indic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array2 ,     
     & Agrif_Mygrid % tabvars(indic) % var % restaure,
     & Agrif_Mygrid % tabvars(indic) %var % nbdim)
      endif
      endif
C
      if ( dimensio .EQ. 3 ) then
      if (present(procname)) then
       Call Agrif_Interp_3D(
     & Agrif_Mygrid % tabvars(indic) % var %  TypeInterp,
     & Agrif_Curgrid % parent % tabvars(indic),
     & Agrif_Curgrid % tabvars(indic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array3 ,     
     & Agrif_Mygrid % tabvars(indic) % var % restaure,
     & Agrif_Mygrid % tabvars(indic) %var % nbdim,procname)
      else
       Call Agrif_Interp_3D(
     & Agrif_Mygrid % tabvars(indic) % var %  TypeInterp,
     & Agrif_Curgrid % parent % tabvars(indic),
     & Agrif_Curgrid % tabvars(indic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array3 ,     
     & Agrif_Mygrid % tabvars(indic) % var % restaure,
     & Agrif_Mygrid % tabvars(indic) %var % nbdim)
      endif
      endif
C
      if ( dimensio .EQ. 4 ) then
      if (present(procname)) then
       Call Agrif_Interp_4D(
     & Agrif_Mygrid % tabvars(indic) % var %  TypeInterp,
     & Agrif_Curgrid % parent % tabvars(indic),
     & Agrif_Curgrid % tabvars(indic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array4 ,     
     & Agrif_Mygrid % tabvars(indic) % var % restaure,
     & Agrif_Mygrid % tabvars(indic) %var % nbdim,procname)
      else
       Call Agrif_Interp_4D(
     & Agrif_Mygrid % tabvars(indic) % var %  TypeInterp,
     & Agrif_Curgrid % parent % tabvars(indic),
     & Agrif_Curgrid % tabvars(indic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array4 ,     
     & Agrif_Mygrid % tabvars(indic) % var % restaure,
     & Agrif_Mygrid % tabvars(indic) %var % nbdim)
      endif
      endif
C
      if ( dimensio .EQ. 5 ) then
      if (present(procname)) then
       Call Agrif_Interp_5D(
     & Agrif_Mygrid % tabvars(indic) % var %  TypeInterp,
     & Agrif_Curgrid % parent % tabvars(indic),
     & Agrif_Curgrid % tabvars(indic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array5 ,     
     & Agrif_Mygrid % tabvars(indic) % var % restaure,
     & Agrif_Mygrid % tabvars(indic) %var % nbdim,procname)
      else
       Call Agrif_Interp_5D(
     & Agrif_Mygrid % tabvars(indic) % var %  TypeInterp,
     & Agrif_Curgrid % parent % tabvars(indic),
     & Agrif_Curgrid % tabvars(indic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array5 ,     
     & Agrif_Mygrid % tabvars(indic) % var % restaure,
     & Agrif_Mygrid % tabvars(indic) %var % nbdim)
       endif
       endif
C
      if ( dimensio .EQ. 6 ) then
      if (present(procname)) then
       Call Agrif_Interp_6D(
     & Agrif_Mygrid % tabvars(indic) % var %  TypeInterp,
     & Agrif_Curgrid % parent % tabvars(indic),
     & Agrif_Curgrid % tabvars(indic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array6 ,     
     & Agrif_Mygrid % tabvars(indic) % var % restaure,
     & Agrif_Mygrid % tabvars(indic) %var % nbdim,procname)
      else
       Call Agrif_Interp_6D(
     & Agrif_Mygrid % tabvars(indic) % var %  TypeInterp,
     & Agrif_Curgrid % parent % tabvars(indic),
     & Agrif_Curgrid % tabvars(indic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array6 ,     
     & Agrif_Mygrid % tabvars(indic) % var % restaure,
     & Agrif_Mygrid % tabvars(indic) %var % nbdim)
      endif
      endif
C
      Return
      End Subroutine Agrif_Interp_var0d
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_var1d
C     **************************************************************************
C 
      Subroutine Agrif_Interp_var1d(q,tabvarsindic,procname)

      REAL, DIMENSION(:) :: q
      INTEGER :: tabvarsindic, indic ! indice of the variable in tabvars
      External :: procname
      Optional ::  procname
      TYPE(Agrif_PVariable),Pointer ::tabvars,parenttabvars,roottabvars      
C
      if (Agrif_Root()) Return
C      
C      
      indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif 
      
      if (indic <=0) then
      tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
      parenttabvars => tabvars%parent_var
      roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
      else
      tabvars=>Agrif_Curgrid % tabvars(indic)
      parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
      roottabvars => Agrif_Mygrid % tabvars(indic)
      endif
      
      if (present(procname)) then
      Call Agrif_Interp_1D(
     & roottabvars % var %  TypeInterp,
     & parenttabvars,
     & tabvars,q,
     & roottabvars % var % restaure,
     & roottabvars %var % nbdim,procname)
      else
      Call Agrif_Interp_1D(
     & roottabvars % var %  TypeInterp,
     & parenttabvars,
     & tabvars,q,
     & roottabvars % var % restaure,
     & roottabvars %var % nbdim)
     
      endif
      Return
      End Subroutine Agrif_Interp_var1d
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_var2d
C     **************************************************************************
C 
      Subroutine Agrif_Interp_var2d(q,tabvarsindic,procname)

      REAL,  DIMENSION(:,:) :: q
      INTEGER :: tabvarsindic, indic ! indice of the variable in tabvars
      External :: procname
      Optional ::  procname
      TYPE(Agrif_PVariable),Pointer ::tabvars,parenttabvars,roottabvars      
C
      if (Agrif_Root()) Return
C      
      indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif 
      
      if (indic <=0) then
      tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
      parenttabvars => tabvars%parent_var
      roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
      if (tabvars%var%restaure) then
        if (agrif_curgrid%ngridstep == 0) then
          call AGRIF_CopyFromold_AllOneVar
     &            (Agrif_Curgrid,Agrif_OldMygrid,indic)
        endif
      endif
      else
      tabvars=>Agrif_Curgrid % tabvars(indic)
      parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
      roottabvars => Agrif_Mygrid % tabvars(indic)
      endif

            
      if (present(procname)) then
      Call Agrif_Interp_2D(
     & roottabvars % var %  TypeInterp,
     & parenttabvars,
     & tabvars,q,
     & roottabvars % var % restaure,
     & roottabvars %var % nbdim,procname)
      else
      Call Agrif_Interp_2D(
     & roottabvars % var %  TypeInterp,
     & parenttabvars,
     & tabvars,q,
     & roottabvars % var % restaure,
     & roottabvars %var % nbdim)
     
      endif 
      Return
      End Subroutine Agrif_Interp_var2d
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_var3d
C     **************************************************************************
C 
      Subroutine Agrif_Interp_var3d(q,tabvarsindic,procname)

      REAL,  DIMENSION(:,:,:) :: q
      INTEGER :: tabvarsindic, indic ! indice of the variable in tabvars
      External :: procname
      Optional ::  procname
      TYPE(Agrif_PVariable),Pointer ::tabvars,parenttabvars,roottabvars      
C
      if (Agrif_Root()) Return
C      

      indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif 
      
      if (indic <=0) then
      tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
      parenttabvars => tabvars%parent_var
      roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
      if (tabvars%var%restaure) then
        if (agrif_curgrid%ngridstep == 0) then
          call AGRIF_CopyFromold_AllOneVar
     &            (Agrif_Curgrid,Agrif_OldMygrid,indic)
        endif
      endif      
      else
      tabvars=>Agrif_Curgrid % tabvars(indic)
      parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
      roottabvars => Agrif_Mygrid % tabvars(indic)
      endif

      if (present(procname)) then
      Call Agrif_Interp_3D(
     & roottabvars % var %  TypeInterp,
     & parenttabvars,
     & tabvars,q,
     & roottabvars % var % restaure,
     & roottabvars %var % nbdim,procname)
      else
      Call Agrif_Interp_3D(
     & roottabvars % var %  TypeInterp,
     & parenttabvars,
     & tabvars,q,
     & roottabvars % var % restaure,
     & roottabvars %var % nbdim)
     
      endif   
      Return
      End Subroutine Agrif_Interp_var3d
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_var4d
C     **************************************************************************
C 
      Subroutine Agrif_Interp_var4d(q,tabvarsindic,procname)

      REAL,  DIMENSION(:,:,:,:) :: q
      INTEGER :: tabvarsindic, indic ! indice of the variable in tabvars
      External :: procname
      Optional ::  procname
      TYPE(Agrif_PVariable),Pointer ::tabvars,parenttabvars,roottabvars  
C
      if (Agrif_Root()) Return
C      
      indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif 
      
      if (indic <=0) then
      tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
      parenttabvars => tabvars%parent_var
      roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
      if (tabvars%var%restaure) then
        if (agrif_curgrid%ngridstep == 0) then
          call AGRIF_CopyFromold_AllOneVar
     &            (Agrif_Curgrid,Agrif_OldMygrid,indic)
        endif
      endif      
      else
      tabvars=>Agrif_Curgrid % tabvars(indic)
      parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
      roottabvars => Agrif_Mygrid % tabvars(indic)
      endif
            
      if (present(procname)) then
      Call Agrif_Interp_4D(
     & roottabvars % var %  TypeInterp,
     & parenttabvars,
     & tabvars,q,
     & roottabvars % var % restaure,
     & roottabvars %var % nbdim,procname)
      else
      Call Agrif_Interp_4D(
     & roottabvars % var %  TypeInterp,
     & parenttabvars,
     & tabvars,q,
     & roottabvars % var % restaure,
     & roottabvars %var % nbdim)
     
      endif      

      Return
      End Subroutine Agrif_Interp_var4d     
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_var5d
C     **************************************************************************
C 
      Subroutine Agrif_Interp_var5d(q,tabvarsindic,procname)

      REAL,  DIMENSION(:,:,:,:,:) :: q
      INTEGER :: tabvarsindic, indic ! indice of the variable in tabvars
      External :: procname
      Optional ::  procname
      TYPE(Agrif_PVariable),Pointer ::tabvars,parenttabvars,roottabvars      
C
      if (Agrif_Root()) Return
C      

      indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif 
      
      if (indic <=0) then
      tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
      parenttabvars => tabvars%parent_var
      roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
      else
      tabvars=>Agrif_Curgrid % tabvars(indic)
      parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
      roottabvars => Agrif_Mygrid % tabvars(indic)
      endif
      
      if (present(procname)) then
      Call Agrif_Interp_5D(
     & roottabvars % var %  TypeInterp,
     & parenttabvars,
     & tabvars,q,
     & roottabvars % var % restaure,
     & roottabvars %var % nbdim,procname)
      else
      Call Agrif_Interp_5D(
     & roottabvars % var %  TypeInterp,
     & parenttabvars,
     & tabvars,q,
     & roottabvars % var % restaure,
     & roottabvars %var % nbdim)
     
      endif
      Return
      End Subroutine Agrif_Interp_var5d       
C
C     **************************************************************************
CCC   Subroutine Agrif_update_var0d
C     **************************************************************************
C 
      Subroutine Agrif_update_var0d(tabvarsindic0,tabvarsindic,
     &                              locupdate,locupdate1,
     &                  locupdate2,procname)

      INTEGER :: tabvarsindic ! indice of the variable in tabvars
      INTEGER :: tabvarsindic0 ! indice of the variable in tabvars
      External :: procname
      Optional ::  procname      
      INTEGER :: dimensio
      INTEGER, DIMENSION(2), OPTIONAL :: locupdate
      INTEGER, DIMENSION(2), OPTIONAL :: locupdate1
      INTEGER, DIMENSION(2), OPTIONAL :: locupdate2            
C
      dimensio = Agrif_Mygrid % tabvars(tabvarsindic) % var % nbdim 
C      
      if (Agrif_Root()) Return
      
C     
      IF (present(locupdate)) THEN
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updateinf(1:dimensio)
     &      = locupdate(1)
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updatesup(1:dimensio) 
     &      = locupdate(2)
      ELSE
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updateinf(1:dimensio) 
     &      = -99
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updatesup(1:dimensio) 
     &      = -99
      ENDIF
      
      IF (present(locupdate1)) THEN
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updateinf(1) 
     &      = locupdate1(1)
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updatesup(1) 
     &      = locupdate1(2)
      ENDIF  
      
      IF (present(locupdate2)) THEN
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updateinf(2) 
     &      = locupdate2(1)
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updatesup(2) 
     &      = locupdate2(2)
      ENDIF             
 
      if ( dimensio .EQ. 1 ) then
      IF (present(procname)) THEN
      Call Agrif_Update_1D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % typeupdate,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array1 ,     
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updateinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updatesup,
     & procname)
      ELSE
      Call Agrif_Update_1D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % typeupdate,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array1 ,     
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updateinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updatesup)      
      ENDIF
      endif
      if ( dimensio .EQ. 2 ) then
      IF (present(procname)) THEN
      Call Agrif_Update_2D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % typeupdate,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array2 ,     
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updateinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updatesup,
     & procname)
      ELSE
      Call Agrif_Update_2D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % typeupdate,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array2 ,     
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updateinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updatesup)      
      ENDIF
      endif
      if ( dimensio .EQ. 3 ) then
      IF (present(procname)) THEN
      Call Agrif_Update_3D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % typeupdate,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array3 ,     
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updateinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updatesup,
     & procname)
      ELSE
      Call Agrif_Update_3D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % typeupdate,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array3 ,     
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updateinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updatesup)      
      ENDIF
      endif
      if ( dimensio .EQ. 4 ) then
      IF (present(procname)) THEN
      Call Agrif_Update_4D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % typeupdate,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array4 ,     
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updateinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updatesup,
     & procname)
      ELSE
      Call Agrif_Update_4D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % typeupdate,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array4 ,     
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updateinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updatesup)      
      ENDIF
      endif
      if ( dimensio .EQ. 5 ) then
      IF (present(procname)) THEN
      Call Agrif_Update_5D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % typeupdate,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array5 ,     
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updateinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updatesup,
     & procname)
      ELSE
      Call Agrif_Update_5D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % typeupdate,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array5 ,     
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updateinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updatesup)      
      ENDIF
      endif

      Return
      End Subroutine Agrif_update_var0d
C
C
C     **************************************************************************
CCC   Subroutine Agrif_update_var1d
C     **************************************************************************
C 
      Subroutine Agrif_update_var1d(q,tabvarsindic,locupdate,
     &  locupdate1,locupdate2,procname)

      REAL,  DIMENSION(:) :: q
      INTEGER :: tabvarsindic,indic ! indice of the variable in tabvars
      External :: procname
      Optional ::  procname      
      INTEGER, DIMENSION(2), OPTIONAL :: locupdate
      INTEGER, DIMENSION(2), OPTIONAL :: locupdate1
      INTEGER, DIMENSION(2), OPTIONAL :: locupdate2        
      TYPE(Agrif_PVariable),Pointer ::tabvars,parenttabvars,roottabvars   
C      
      if (Agrif_Root()) Return
C     

      indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif 
      
      if (indic <=0) then
      tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
      parenttabvars => tabvars%parent_var
      roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)      
      else
      tabvars=>Agrif_Curgrid % tabvars(indic)
      parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
      roottabvars => Agrif_Mygrid % tabvars(indic)
      endif
      
      IF (present(locupdate)) THEN
      tabvars%var % updateinf(1:1) 
     &      = locupdate(1)
      tabvars%var % updatesup(1:1) 
     &      = locupdate(2)
      ELSE
      tabvars%var % updateinf(1:1) 
     &      = -99
      tabvars%var % updatesup(1:1) 
     &      = -99
      ENDIF
      
      IF (present(locupdate1)) THEN
      tabvars%var % updateinf(1) 
     &      = locupdate1(1)
      tabvars%var % updatesup(1) 
     &      = locupdate1(2)
      ENDIF  
      
      IF (present(locupdate2)) THEN
      tabvars%var % updateinf(2) 
     &      = locupdate2(1)
      tabvars%var % updatesup(2) 
     &      = locupdate2(2)
      ENDIF       
 
      IF (present(procname)) THEN
      Call Agrif_Update_1D(
     & roottabvars % var % typeupdate,
     & parenttabvars,
     & tabvars,q,
     & tabvars % var % updateinf,
     & tabvars % var % updatesup,
     & procname)
      ELSE 
      Call Agrif_Update_1D(
     & roottabvars % var % typeupdate,
     & parenttabvars,
     & tabvars,q,
     & tabvars % var % updateinf,
     & tabvars % var % updatesup)       
      ENDIF

      Return
      End Subroutine Agrif_update_var1d
C
C
C     **************************************************************************
CCC   Subroutine Agrif_update_var2d
C     **************************************************************************
C 
      Subroutine Agrif_update_var2d(q,tabvarsindic,locupdate,
     &  locupdate1,locupdate2,procname)

      REAL,  DIMENSION(:,:) :: q
      External :: procname
      Optional ::  procname
      INTEGER, DIMENSION(2), OPTIONAL :: locupdate 
      INTEGER, DIMENSION(2), OPTIONAL :: locupdate1
      INTEGER, DIMENSION(2), OPTIONAL :: locupdate2        
      INTEGER :: tabvarsindic,indic ! indice of the variable in tabvars
      TYPE(Agrif_PVariable),Pointer ::tabvars,parenttabvars,roottabvars       
C      
      IF (Agrif_Root()) RETURN
      
C 
      indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif 
      
      if (indic <=0) then
      tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
      parenttabvars => tabvars%parent_var
      roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)      
      else
      tabvars=>Agrif_Curgrid % tabvars(indic)
      parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
      roottabvars => Agrif_Mygrid % tabvars(indic)
      endif
      
      IF (present(locupdate)) THEN
      tabvars%var % updateinf(1:2) 
     &      = locupdate(1)
      tabvars%var % updatesup(1:2) 
     &      = locupdate(2)
      ELSE
      tabvars%var % updateinf(1:2) 
     &      = -99
      tabvars%var % updatesup(1:2) 
     &      = -99
      ENDIF
      
      IF (present(locupdate1)) THEN
      tabvars%var % updateinf(1) 
     &      = locupdate1(1)
      tabvars%var % updatesup(1) 
     &      = locupdate1(2)
      ENDIF  
      
      IF (present(locupdate2)) THEN
      tabvars%var % updateinf(2) 
     &      = locupdate2(1)
      tabvars%var % updatesup(2) 
     &      = locupdate2(2)
      ENDIF
 
      IF (present(procname)) THEN
      Call Agrif_Update_2D(
     & roottabvars % var % typeupdate,
     & parenttabvars,
     & tabvars,q,
     & tabvars % var % updateinf,
     & tabvars % var % updatesup,
     & procname)
      ELSE 
      Call Agrif_Update_2D(
     & roottabvars % var % typeupdate,
     & parenttabvars,
     & tabvars,q,
     & tabvars % var % updateinf,
     & tabvars % var % updatesup)       
      ENDIF

      Return
      End Subroutine Agrif_update_var2d
C 
C
C     **************************************************************************
CCC   Subroutine Agrif_update_var3d
C     **************************************************************************
C 
      Subroutine Agrif_update_var3d(q,tabvarsindic,locupdate,
     &  locupdate1,locupdate2,procname)

      REAL,  DIMENSION(:,:,:) :: q
      External :: procname
      Optional ::  procname
      INTEGER, DIMENSION(2), OPTIONAL :: locupdate
      INTEGER, DIMENSION(2), OPTIONAL :: locupdate1
      INTEGER, DIMENSION(2), OPTIONAL :: locupdate2        
      INTEGER :: tabvarsindic,indic ! indice of the variable in tabvars
      TYPE(Agrif_PVariable),Pointer ::tabvars,parenttabvars,roottabvars       
C      
      IF (Agrif_Root()) RETURN 
C      
      indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif 
      
      if (indic <=0) then
      tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
      parenttabvars => tabvars%parent_var
      roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)      
      else
      tabvars=>Agrif_Curgrid % tabvars(indic)
      parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
      roottabvars => Agrif_Mygrid % tabvars(indic)
      endif
      
      IF (present(locupdate)) THEN
      tabvars%var % updateinf(1:3) 
     &      = locupdate(1)
      tabvars%var % updatesup(1:3) 
     &      = locupdate(2)
      ELSE
      tabvars%var % updateinf(1:3) 
     &      = -99
      tabvars%var % updatesup(1:3) 
     &      = -99
      ENDIF      
      
      IF (present(locupdate1)) THEN
      tabvars%var % updateinf(1) 
     &      = locupdate1(1)
      tabvars%var % updatesup(1) 
     &      = locupdate1(2)
      ENDIF  
      
      IF (present(locupdate2)) THEN
      tabvars%var % updateinf(2) 
     &      = locupdate2(1)
      tabvars%var % updatesup(2) 
     &      = locupdate2(2)
      ENDIF

      IF (present(procname)) THEN
      Call Agrif_Update_3D(
     & roottabvars % var % typeupdate,
     & parenttabvars,
     & tabvars,q,
     & tabvars % var % updateinf,
     & tabvars % var % updatesup,
     & procname)
      ELSE 
      Call Agrif_Update_3D(
     & roottabvars % var % typeupdate,
     & parenttabvars,
     & tabvars,q,
     & tabvars % var % updateinf,
     & tabvars % var % updatesup)       
      ENDIF

      Return
      End Subroutine Agrif_update_var3d
C 
C
C     **************************************************************************
CCC   Subroutine Agrif_update_var4d
C     **************************************************************************
C 
      Subroutine Agrif_update_var4d(q,tabvarsindic,locupdate,
     &  locupdate1,locupdate2,procname)

      REAL,  DIMENSION(:,:,:,:) :: q
      External :: procname
      Optional ::  procname
      INTEGER, DIMENSION(2), OPTIONAL :: locupdate
      INTEGER, DIMENSION(2), OPTIONAL :: locupdate1
      INTEGER, DIMENSION(2), OPTIONAL :: locupdate2        
      INTEGER :: tabvarsindic,indic ! indice of the variable in tabvars
      TYPE(Agrif_PVariable),Pointer ::tabvars,parenttabvars,roottabvars       
C      
      IF (Agrif_Root()) RETURN
      indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif 
      
      if (indic <=0) then
      tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
      parenttabvars => tabvars%parent_var
      roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)     
      else
      tabvars=>Agrif_Curgrid % tabvars(indic)
      parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
      roottabvars => Agrif_Mygrid % tabvars(indic)
      endif      
C      
      IF (present(locupdate)) THEN
      tabvars%var % updateinf(1:4) 
     &      = locupdate(1)
      tabvars%var % updatesup(1:4) 
     &      = locupdate(2)
      ELSE
      tabvars%var % updateinf(1:4) 
     &      = -99
      tabvars%var % updatesup(1:4) 
     &      = -99
      ENDIF
      
      IF (present(locupdate1)) THEN
      tabvars%var % updateinf(1) 
     &      = locupdate1(1)
      tabvars%var % updatesup(1) 
     &      = locupdate1(2)
      ENDIF  
      
      IF (present(locupdate2)) THEN
      tabvars%var % updateinf(2) 
     &      = locupdate2(1)
      tabvars%var % updatesup(2) 
     &      = locupdate2(2)
      ENDIF

      IF (present(procname)) THEN
      Call Agrif_Update_4D(
     & roottabvars % var % typeupdate,
     & parenttabvars,
     & tabvars,q,
     & tabvars % var % updateinf,
     & tabvars % var % updatesup,
     & procname)
      ELSE 
      Call Agrif_Update_4D(
     & roottabvars % var % typeupdate,
     & parenttabvars,
     & tabvars,q,
     & tabvars % var % updateinf,
     & tabvars % var % updatesup)       
      ENDIF

      Return
      End Subroutine Agrif_update_var4d  
C 
C
C     **************************************************************************
CCC   Subroutine Agrif_update_var5d
C     **************************************************************************
C 
      Subroutine Agrif_update_var5d(q,tabvarsindic,locupdate,
     &  locupdate1,locupdate2,procname)

      REAL,  DIMENSION(:,:,:,:,:) :: q
      External :: procname
      Optional ::  procname
      INTEGER, DIMENSION(2), OPTIONAL :: locupdate
      INTEGER, DIMENSION(2), OPTIONAL :: locupdate1
      INTEGER, DIMENSION(2), OPTIONAL :: locupdate2        
      INTEGER :: tabvarsindic,indic ! indice of the variable in tabvars
      TYPE(Agrif_PVariable),Pointer ::tabvars,parenttabvars,roottabvars       
C
      IF (Agrif_Root()) RETURN
C     
      indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif 
      
      if (indic <=0) then
      tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
      parenttabvars => tabvars%parent_var
      roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)      
      else
      tabvars=>Agrif_Curgrid % tabvars(indic)
      parenttabvars => Agrif_Curgrid % parent % tabvars(indic)
      roottabvars => Agrif_Mygrid % tabvars(indic)
      endif
       
      IF (present(locupdate)) THEN
      tabvars%var % updateinf(1:5) 
     &      = locupdate(1)
      tabvars%var % updatesup(1:5) 
     &      = locupdate(2)
      ELSE
      tabvars%var % updateinf(1:5) 
     &      = -99
      tabvars%var % updatesup(1:5) 
     &      = -99
      ENDIF
      
      IF (present(locupdate1)) THEN
      tabvars%var % updateinf(1) 
     &      = locupdate1(1)
      tabvars%var % updatesup(1) 
     &      = locupdate1(2)
      ENDIF  
      
      IF (present(locupdate2)) THEN
      tabvars%var % updateinf(2) 
     &      = locupdate2(1)
      tabvars%var % updatesup(2) 
     &      = locupdate2(2)
      ENDIF

      IF (present(procname)) THEN
      Call Agrif_Update_5D(
     & roottabvars % var % typeupdate,
     & parenttabvars,
     & tabvars,q,
     & tabvars % var % updateinf,
     & tabvars % var % updatesup,
     & procname)
      ELSE 
      Call Agrif_Update_5D(
     & roottabvars % var % typeupdate,
     & parenttabvars,
     & tabvars,q,
     & tabvars % var % updateinf,
     & tabvars % var % updatesup)       
      ENDIF

      Return
      End Subroutine Agrif_update_var5d  
          
      Subroutine Agrif_Declare_Flux(fluxname,profilename) 
      character*(*) :: fluxname, profilename
      Type(Agrif_Flux), pointer :: newflux
      Type(Agrif_Profile), pointer  :: parcours
      logical :: foundprofile
      integer :: i,j,n
            
      foundprofile = .FALSE.
      parcours => Agrif_Myprofiles
      
      Do While (Associated(parcours))
         IF (parcours % profilename == profilename) THEN
           foundprofile = .TRUE.
           EXIT
         ENDIF
         parcours => parcours%nextprofile
      End Do      
      
      IF (.NOT.foundprofile) THEN
      write(*,*) 'The profile '''
     &           //TRIM(profilename)//''' has not been declared'  
      stop    
      ENDIF
      
      Allocate(Newflux)
      
      Newflux % fluxname = fluxname
      
      Newflux % profile => parcours
      
      Newflux % nextflux => Agrif_Curgrid % fluxes
      
      Agrif_Curgrid % fluxes => Newflux
      
      End Subroutine Agrif_Declare_Flux  
       
      Subroutine Agrif_Save_Flux(fluxname, fluxtab)
      character*(*) :: fluxname
      REAL, DIMENSION(:,:) :: fluxtab
      
      
      Type(Agrif_Flux), pointer :: Flux
      
      Type(Agrif_pgrid), pointer :: parcours_child
      
      Type(Agrif_grid), Pointer :: currentgrid,oldcurgrid
      
      IF (.Not.Agrif_Root()) THEN
      Flux => Agrif_Search_Flux(fluxname)

      IF (.NOT.Flux%fluxallocated) THEN
        CALL Agrif_AllocateFlux(Flux,fluxtab)
      ENDIF
      
      Call Agrif_Save_Fluxtab(Flux,fluxtab)
      
      ENDIF
      
      oldcurgrid=> Agrif_Curgrid
      
      parcours_child => Agrif_Curgrid%child_grids
      
      Do While (Associated(parcours_child))
        currentgrid => parcours_child%gr
        Agrif_Curgrid => parcours_child%gr
        Flux => Agrif_Search_Flux(fluxname)
        IF (.NOT.Flux%fluxallocated) THEN
          CALL Agrif_AllocateFlux(Flux,fluxtab)
        ENDIF        
        Call Agrif_Save_Fluxtab_child(Flux,fluxtab)
        parcours_child=> parcours_child%next
      End Do
      
      Agrif_Curgrid=>oldcurgrid
      
      End Subroutine Agrif_Save_Flux

      Subroutine Agrif_Cancel_Flux(fluxname)
      character*(*) :: fluxname
      
      Type(Agrif_Flux), pointer :: Flux
      
      Flux => Agrif_Search_Flux(fluxname)

      IF (Flux%FluxAllocated) Call Agrif_Cancel_Fluxarray(Flux)
      
      End Subroutine Agrif_Cancel_Flux
 
      Subroutine Agrif_Flux_Correction(fluxname, procname)
      character*(*) :: fluxname
      external :: procname
      
      Type(Agrif_Flux), pointer :: Flux
      
      Flux => Agrif_Search_Flux(fluxname)
      
      Call Agrif_FluxCorrect(Flux, procname)

      
      End Subroutine Agrif_Flux_Correction


                              
      Subroutine Agrif_Declare_Profile_flux(profilename,posvar,
     &    firstpoint,raf)
      character*(*) :: profilename
      Type(Agrif_Profile), Pointer :: newprofile
      INTEGER, DIMENSION(:) :: posvar
      INTEGER, DIMENSION(:) :: firstpoint
      CHARACTER(*) ,DIMENSION(:) :: raf      
      INTEGER :: dimensio
            
      dimensio = SIZE(posvar)
C
C    
      Allocate(newprofile)
      Allocate(newprofile%posvar(dimensio))
      Allocate(newprofile%interptab(dimensio))
      newprofile%profilename = profilename
      newprofile%interptab = raf
      newprofile%nbdim = dimensio
      newprofile%posvar = posvar
      newprofile%point(1:dimensio) = firstpoint
      
      newprofile % nextprofile => Agrif_myprofiles
      
      Agrif_myprofiles => newprofile
      
      End Subroutine Agrif_Declare_Profile_flux

      Subroutine Agrif_Save_ForRestore0D(tabvarsindic0,tabvarsindic)
      integer :: tabvarsindic0, tabvarsindic
      integer :: dimensio
      
      dimensio =  Agrif_Mygrid % tabvars(tabvarsindic0) % var % nbdim
      
      select case(dimensio)
      case(2)
          call Agrif_Save_ForRestore2D(
     &      Agrif_Curgrid % tabvars(tabvarsindic0) % var % array2,
     &      tabvarsindic)
      case(3)
          call Agrif_Save_ForRestore3D(
     &      Agrif_Curgrid % tabvars(tabvarsindic0) % var % array3,
     &      tabvarsindic)
      case(4)
          call Agrif_Save_ForRestore4D(
     &      Agrif_Curgrid % tabvars(tabvarsindic0) % var % array4,
     &      tabvarsindic)     
      end select

      Return
      End Subroutine Agrif_Save_ForRestore0D 
      
    
      
      Subroutine Agrif_Save_ForRestore2D(q,tabvarsindic)
      real,dimension(:,:) :: q
      integer :: tabvarsindic, indic
      TYPE(Agrif_PVariable),Pointer ::tabvars, roottabvars    
 
       indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif
      
      if (indic <=0) then
      tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
      roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
      else
      tabvars=>Agrif_Curgrid % tabvars(indic)
      roottabvars => Agrif_Mygrid % tabvars(indic)
      endif      
      if (.not.associated(tabvars%var%array2)) then      
      allocate(tabvars%var%array2(tabvars%var%lb(1):tabvars%var%ub(1),
     &                            tabvars%var%lb(2):tabvars%var%ub(2)))
      endif
      tabvars%var%array2 = q
      roottabvars%var%restaure = .true.
      
      Return
      End Subroutine Agrif_Save_ForRestore2D  
      
      Subroutine Agrif_Save_ForRestore3D(q,tabvarsindic)
      real,dimension(:,:,:) :: q
      integer :: tabvarsindic, indic
      TYPE(Agrif_PVariable),Pointer ::tabvars, roottabvars    
 
       indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif
      
      if (indic <=0) then
      tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
      roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
      else
      tabvars=>Agrif_Curgrid % tabvars(indic)
      roottabvars => Agrif_Mygrid % tabvars(indic)
      endif      

      if (.not.associated(tabvars%var%array3)) then
      allocate(tabvars%var%array3(tabvars%var%lb(1):tabvars%var%ub(1),
     &                            tabvars%var%lb(2):tabvars%var%ub(2),
     &                            tabvars%var%lb(3):tabvars%var%ub(3)))
      endif
      tabvars%var%array3 = q
      roottabvars%var%restaure = .true.
      
      Return
      End Subroutine Agrif_Save_ForRestore3D
      
      Subroutine Agrif_Save_ForRestore4D(q,tabvarsindic)
      real,dimension(:,:,:,:) :: q
      integer :: tabvarsindic, indic
      TYPE(Agrif_PVariable),Pointer ::tabvars, roottabvars    
 
       indic = tabvarsindic
      if (tabvarsindic >=0) then
        if (agrif_curgrid%tabvars(tabvarsindic)%var%nbdim == 0) then
          indic = agrif_curgrid%tabvars(tabvarsindic)%var%iarray0
        endif
      endif
      
      if (indic <=0) then
      tabvars => Agrif_Search_Variable(Agrif_Curgrid,-indic)
      roottabvars => Agrif_Search_Variable(Agrif_Mygrid,-indic)
      else
      tabvars=>Agrif_Curgrid % tabvars(indic)
      roottabvars => Agrif_Mygrid % tabvars(indic)
      endif

      if (.not.associated(tabvars%var%array4)) then         
      allocate(tabvars%var%array4(tabvars%var%lb(1):tabvars%var%ub(1),
     &                            tabvars%var%lb(2):tabvars%var%ub(2),
     &                            tabvars%var%lb(3):tabvars%var%ub(3),
     &                            tabvars%var%lb(4):tabvars%var%ub(4)))
      endif
      tabvars%var%array4 = q
      roottabvars%var%restaure = .true.
      
      Return
      End Subroutine Agrif_Save_ForRestore4D                   
C
      End module Agrif_bcfunction
