!
! $Id: modbcfunction.F,v 1.5 2005/08/22 15:11:29 agrif Exp $
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
     &                     Agrif_Init_variable3d
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
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
C
C
C     Begin 
C
C     
      if (Agrif_Curgrid % fixedrank .NE. 0) then  
       IF (.Not.Associated(Agrif_Curgrid%tabvars(tabvarsindic)%var
     &                % interpIndex)) THEN
        Allocate(Agrif_Curgrid%tabvars(tabvarsindic)%var % interpIndex)
          Agrif_Curgrid%tabvars(tabvarsindic)%var % interpIndex = -1

        Allocate(
     &    Agrif_Curgrid%tabvars(tabvarsindic)%var % oldvalues2D(1,2))
          Agrif_Curgrid%tabvars(tabvarsindic)%var % oldvalues2D = 0. 
       ENDIF      
       if ( PRESENT(Interpolationshouldbemade) ) then
         Agrif_Curgrid%tabvars(tabvarsindic)%var %
     &     Interpolationshouldbemade = Interpolationshouldbemade
       endif

      endif
C
      Agrif_Curgrid%tabvars(tabvarsindic)%var % bcinf = point(1)
      Agrif_Curgrid%tabvars(tabvarsindic)%var % bcsup = point(2)
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
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
C
C     Begin 
C
      Agrif_Mygrid % tabvars(tabvarsindic)% var % Typeinterp = 
     &    Agrif_Constant
      IF (present(interp)) THEN
      Agrif_Mygrid % tabvars(tabvarsindic)% var % Typeinterp = 
     &           interp
      ENDIF
      IF (present(interp1)) THEN
      Agrif_Mygrid % tabvars(tabvarsindic)% var % Typeinterp(1) = 
     &           interp1
      ENDIF
      IF (present(interp2)) THEN
      Agrif_Mygrid % tabvars(tabvarsindic)% var % Typeinterp(2) = 
     &           interp2
      ENDIF
      IF (present(interp3)) THEN
      Agrif_Mygrid % tabvars(tabvarsindic)% var % Typeinterp(3) = 
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
     &      interp2,interp3)
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
C
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
C
C
C     Begin 
C
      Agrif_Mygrid % tabvars(tabvarsindic)% var % bctypeinterp = 
     &           Agrif_Constant
      IF (present(interp)) THEN
      Agrif_Mygrid % tabvars(tabvarsindic)% var % bctypeinterp = 
     &           interp
      ENDIF
      IF (present(interp1)) THEN
      Agrif_Mygrid % tabvars(tabvarsindic)% var % bctypeinterp(1) = 
     &           interp1
      ENDIF
      IF (present(interp2)) THEN
      Agrif_Mygrid % tabvars(tabvarsindic)% var % bctypeinterp(2) = 
     &           interp2
      ENDIF
      IF (present(interp3)) THEN
      Agrif_Mygrid % tabvars(tabvarsindic)% var % bctypeinterp(3) = 
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
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
C
C
C     Begin 
C
      Agrif_Mygrid % tabvars(tabvarsindic) % var % typeupdate = 
     &                   Agrif_Update_Copy
      
      IF (present(update)) THEN
        Agrif_Mygrid % tabvars(tabvarsindic)% var % typeupdate = 
     &           update
      ENDIF
      IF (present(update1)) THEN
        Agrif_Mygrid % tabvars(tabvarsindic)% var % typeupdate(1) = 
     &           update1
      ENDIF  
      IF (present(update2)) THEN
        Agrif_Mygrid % tabvars(tabvarsindic)% var % typeupdate(2) = 
     &           update2
      ENDIF  
      IF (present(update3)) THEN
        Agrif_Mygrid % tabvars(tabvarsindic)% var % typeupdate(3) = 
     &           update3
      ENDIF 
      IF (present(update4)) THEN
        Agrif_Mygrid % tabvars(tabvarsindic)% var % typeupdate(4) = 
     &           update4
      ENDIF       
      IF (present(update5)) THEN
        Agrif_Mygrid % tabvars(tabvarsindic)% var % typeupdate(5) = 
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
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
C
C     Begin 
C
C
      Agrif_Mygrid%tabvars(tabvarsindic)%var % restaure = .TRUE.
C
      End Subroutine Agrif_Set_restore
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Init_variable0d
C     **************************************************************************
      Subroutine Agrif_Init_variable0d(tabvarsindic0,tabvarsindic)

      INTEGER :: tabvarsindic0 ! indice of the variable in tabvars
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
C
      if (Agrif_Root()) Return
C      
      CALL Agrif_Interp_variable(tabvarsindic0,tabvarsindic)
      CALL Agrif_Bc_variable(tabvarsindic0,tabvarsindic,1.)

      End Subroutine Agrif_Init_variable0d
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Init_variable1d
C     **************************************************************************
      Subroutine Agrif_Init_variable1d(q,tabvarsindic)

      REAL, DIMENSION(:) :: q
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
C
      if (Agrif_Root()) Return
C
      CALL Agrif_Interp_variable(q,tabvarsindic)
      CALL Agrif_Bc_variable(q,tabvarsindic,1.)

      End Subroutine Agrif_Init_variable1d
C
C     **************************************************************************
CCC   Subroutine Agrif_Init_variable2d
C     **************************************************************************
      Subroutine Agrif_Init_variable2d(q,tabvarsindic)

      REAL,  DIMENSION(:,:) :: q
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
C
      if (Agrif_Root()) Return
C
      CALL Agrif_Interp_variable(q,tabvarsindic)
      CALL Agrif_Bc_variable(q,tabvarsindic,1.)

      End Subroutine Agrif_Init_variable2d
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Init_variable3d
C     **************************************************************************
      Subroutine Agrif_Init_variable3d(q,tabvarsindic)

      REAL,  DIMENSION(:,:,:) :: q
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
C
      if (Agrif_Root()) Return
C
      CALL Agrif_Interp_variable(q,tabvarsindic)
      CALL Agrif_Bc_variable(q,tabvarsindic,1.)
C
      End Subroutine Agrif_Init_variable3d
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
C
C     **************************************************************************
CCC   Subroutine Agrif_Bc_variable1d
C     **************************************************************************
      Subroutine Agrif_Bc_variable1d(q,tabvarsindic,calledweight)

      REAL   , DIMENSION(:)          :: q
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
C        
      REAL, OPTIONAL :: calledweight
      REAL    :: weight
      LOGICAL :: pweight
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
      if (Agrif_Root()) Return
      
      Call Agrif_Interp_Bc_1D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % bctypeinterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & q,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcsup,
     & weight,
     & pweight)
      End Subroutine Agrif_Bc_variable1d
C
C
CC
C
C     **************************************************************************
CCC   Subroutine Agrif_Bc_variable2d
C     **************************************************************************
      Subroutine Agrif_Bc_variable2d(q,tabvarsindic,calledweight,
     &                                 procname)

      REAL   , DIMENSION(:,:)          :: q
      External :: procname
      Optional ::  procname
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
C        
      REAL, OPTIONAL :: calledweight
      REAL    :: weight
      LOGICAL :: pweight
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

      if (Agrif_Root()) Return
      IF (present(procname)) THEN
      Call Agrif_Interp_Bc_2D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % bctypeinterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),q,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcsup,
     & weight,pweight,procname)      
      ELSE
       Call Agrif_Interp_Bc_2D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % bctypeinterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),q,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcsup,
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
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
C        
      REAL, OPTIONAL :: calledweight
      REAL    :: weight
      LOGICAL :: pweight
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
      If (Agrif_Root()) Return
      IF (present(procname)) THEN
      Call Agrif_Interp_Bc_3D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % bctypeinterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),q,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcsup,
     & weight,pweight,procname)      
      ELSE
      Call Agrif_Interp_Bc_3D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % bctypeinterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),q,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcsup,
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
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
C        
      REAL, OPTIONAL :: calledweight
      REAL    :: weight
      LOGICAL :: pweight
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
      If (Agrif_Root()) Return
      IF (present(procname)) THEN
      Call Agrif_Interp_Bc_4D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % bctypeinterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),q,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcsup,
     & weight,pweight,procname)      
      ELSE
      Call Agrif_Interp_Bc_4D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % bctypeinterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),q,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcsup,
     & weight,pweight)
      ENDIF
      End Subroutine Agrif_Bc_variable4d
C
C     **************************************************************************
CCC   Subroutine Agrif_Bc_variable5d
C     **************************************************************************
      Subroutine Agrif_Bc_variable5d(q,tabvarsindic,calledweight,
     &                              procname)

      REAL   , Dimension(:,:,:,:,:)          :: q
      External :: procname
      Optional ::  procname
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
C        
      REAL, OPTIONAL :: calledweight
      REAL    :: weight
      LOGICAL :: pweight
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
      If (Agrif_Root()) Return
      IF (present(procname)) THEN
      Call Agrif_Interp_Bc_5D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % bctypeinterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),q,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcsup,
     & weight,pweight,procname)      
      ELSE
      Call Agrif_Interp_Bc_5D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % bctypeinterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),q,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % bcsup,
     & weight,pweight)
      ENDIF
      End Subroutine Agrif_Bc_variable5d
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_var0D
C     **************************************************************************
C 
      Subroutine Agrif_Interp_var0d(tabvarsindic0,tabvarsindic)

      INTEGER :: tabvarsindic0 ! indice of the variable in tabvars
      INTEGER :: tabvarsindic  ! indice of the variable in tabvars
      INTEGER :: dimensio  ! indice of the variable in tabvars
C      
      if (Agrif_Root()) Return
C     
      dimensio = Agrif_Mygrid % tabvars(tabvarsindic) % var % nbdim 
C
      if ( dimensio .EQ. 1 )
     & Call Agrif_Interp_1D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var %  TypeInterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array1 ,     
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % restaure,
     & Agrif_Mygrid % tabvars(tabvarsindic) %var % nbdim)
C
      if ( dimensio .EQ. 2 )
     & Call Agrif_Interp_2D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var %  TypeInterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array2 ,     
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % restaure,
     & Agrif_Mygrid % tabvars(tabvarsindic) %var % nbdim)
C
      if ( dimensio .EQ. 3 )
     & Call Agrif_Interp_3D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var %  TypeInterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array3 ,     
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % restaure,
     & Agrif_Mygrid % tabvars(tabvarsindic) %var % nbdim)
C
      if ( dimensio .EQ. 4 )
     & Call Agrif_Interp_4D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var %  TypeInterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array4 ,     
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % restaure,
     & Agrif_Mygrid % tabvars(tabvarsindic) %var % nbdim)
C
      if ( dimensio .EQ. 5 )
     & Call Agrif_Interp_5D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var %  TypeInterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array5 ,     
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % restaure,
     & Agrif_Mygrid % tabvars(tabvarsindic) %var % nbdim)
C
      if ( dimensio .EQ. 6 )
     & Call Agrif_Interp_6D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var %  TypeInterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic0) % var % array6 ,     
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % restaure,
     & Agrif_Mygrid % tabvars(tabvarsindic) %var % nbdim)
C
      Return
      End Subroutine Agrif_Interp_var0d
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_var1d
C     **************************************************************************
C 
      Subroutine Agrif_Interp_var1d(q,tabvarsindic)

      REAL, DIMENSION(:) :: q
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
C
      if (Agrif_Root()) Return
C      
      Call Agrif_Interp_1D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var %  TypeInterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),q,
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % restaure,
     & Agrif_Mygrid % tabvars(tabvarsindic) %var % nbdim)

      Return
      End Subroutine Agrif_Interp_var1d
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_var2d
C     **************************************************************************
C 
      Subroutine Agrif_Interp_var2d(q,tabvarsindic)

      REAL,  DIMENSION(:,:) :: q
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
C
       if (Agrif_Root()) Return
C
       Call Agrif_Interp_2D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var %  TypeInterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),q,
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % restaure,
     & Agrif_Mygrid % tabvars(tabvarsindic) %var % nbdim)

      Return
      End Subroutine Agrif_Interp_var2d
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_var3d
C     **************************************************************************
C 
      Subroutine Agrif_Interp_var3d(q,tabvarsindic)

      REAL,  DIMENSION(:,:,:) :: q
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
C
      if (Agrif_Root()) Return
C
      Call Agrif_Interp_3D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var %  TypeInterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),q,
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % restaure,
     & Agrif_Mygrid % tabvars(tabvarsindic) %var % nbdim)

      Return
      End Subroutine Agrif_Interp_var3d
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_var4d
C     **************************************************************************
C 
      Subroutine Agrif_Interp_var4d(q,tabvarsindic)

      REAL,  DIMENSION(:,:,:,:) :: q
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
C
      if (Agrif_Root()) Return
C
      Call Agrif_Interp_4D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var %  TypeInterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),q,
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % restaure,
     & Agrif_Mygrid % tabvars(tabvarsindic) %var % nbdim)

      Return
      End Subroutine Agrif_Interp_var4d     
C
C     **************************************************************************
CCC   Subroutine Agrif_Interp_var5d
C     **************************************************************************
C 
      Subroutine Agrif_Interp_var5d(q,tabvarsindic)

      REAL,  DIMENSION(:,:,:,:,:) :: q
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
C
      if (Agrif_Root()) Return
C
      Call Agrif_Interp_5D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var %  TypeInterp,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),q,
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % restaure,
     & Agrif_Mygrid % tabvars(tabvarsindic) %var % nbdim)

      Return
      End Subroutine Agrif_Interp_var5d       
C
C     **************************************************************************
CCC   Subroutine Agrif_update_var0d
C     **************************************************************************
C 
      Subroutine Agrif_update_var0d(tabvarsindic0,tabvarsindic,
     &                              locupdate,procname)

      INTEGER :: tabvarsindic ! indice of the variable in tabvars
      INTEGER :: tabvarsindic0 ! indice of the variable in tabvars
      External :: procname
      Optional ::  procname      
      INTEGER :: dimensio
      INTEGER, DIMENSION(2), OPTIONAL :: locupdate
C
      dimensio = Agrif_Mygrid % tabvars(tabvarsindic) % var % nbdim 
C      
      if (Agrif_Root()) Return
C     
      IF (present(locupdate)) THEN
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updateinf = locupdate(1)
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updatesup = locupdate(2)
      ELSE
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updateinf = -99
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updatesup = -99
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
      Subroutine Agrif_update_var1d(q,tabvarsindic,locupdate,procname)

      REAL,  DIMENSION(:) :: q
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
      External :: procname
      Optional ::  procname      
      INTEGER, DIMENSION(2), OPTIONAL :: locupdate
C      
      if (Agrif_Root()) Return
C     
      IF (present(locupdate)) THEN
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updateinf = locupdate(1)
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updatesup = locupdate(2)
      ELSE
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updateinf = -99
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updatesup = -99
      ENDIF
 
      IF (present(procname)) THEN
      Call Agrif_Update_1D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % typeupdate,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),q,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updateinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updatesup,
     & procname)
      ELSE
      Call Agrif_Update_1D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % typeupdate,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),q,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updateinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updatesup)      
      ENDIF

      Return
      End Subroutine Agrif_update_var1d
C
C
C     **************************************************************************
CCC   Subroutine Agrif_update_var2d
C     **************************************************************************
C 
      Subroutine Agrif_update_var2d(q,tabvarsindic,locupdate,procname)

      REAL,  DIMENSION(:,:) :: q
      External :: procname
      Optional ::  procname
      INTEGER, DIMENSION(2), OPTIONAL :: locupdate 
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
C      
      IF (Agrif_Root()) RETURN
C 
      IF (present(locupdate)) THEN
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updateinf = locupdate(1)
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updatesup = locupdate(2)
      ELSE
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updateinf = -99
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updatesup = -99
      ENDIF
 
      IF (present(procname)) THEN
      Call Agrif_Update_2D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % typeupdate,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),q,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updateinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updatesup,
     & procname)
      ELSE
      Call Agrif_Update_2D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % typeupdate,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),q,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updateinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updatesup)      
      ENDIF

      Return
      End Subroutine Agrif_update_var2d
C 
C
C     **************************************************************************
CCC   Subroutine Agrif_update_var3d
C     **************************************************************************
C 
      Subroutine Agrif_update_var3d(q,tabvarsindic,locupdate,procname)

      REAL,  DIMENSION(:,:,:) :: q
      External :: procname
      Optional ::  procname
      INTEGER, DIMENSION(2), OPTIONAL :: locupdate
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
C      
      IF (Agrif_Root()) RETURN
C      

      IF (present(locupdate)) THEN
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updateinf = locupdate(1)
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updatesup = locupdate(2)
      ELSE
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updateinf = -99
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updatesup = -99
      ENDIF

      IF (present(procname)) THEN
      Call Agrif_Update_3D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % typeupdate,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),q,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updateinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updatesup,
     & procname)
      ELSE
      Call Agrif_Update_3D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % typeupdate,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),q,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updateinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updatesup)      
      ENDIF

      Return
      End Subroutine Agrif_update_var3d
C 
C
C     **************************************************************************
CCC   Subroutine Agrif_update_var4d
C     **************************************************************************
C 
      Subroutine Agrif_update_var4d(q,tabvarsindic,locupdate,procname)

      REAL,  DIMENSION(:,:,:,:) :: q
      External :: procname
      Optional ::  procname
      INTEGER, DIMENSION(2), OPTIONAL :: locupdate
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
C      
      IF (Agrif_Root()) RETURN
C      
      IF (present(locupdate)) THEN
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updateinf = locupdate(1)
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updatesup = locupdate(2)
      ELSE
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updateinf = -99
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updatesup = -99
      ENDIF

      IF (present(procname)) THEN
      Call Agrif_Update_4D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % typeupdate,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),q,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updateinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updatesup,
     & procname)
      ELSE
      Call Agrif_Update_4D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % typeupdate,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),q,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updateinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updatesup)      
      ENDIF

      Return
      End Subroutine Agrif_update_var4d  
C 
C
C     **************************************************************************
CCC   Subroutine Agrif_update_var5d
C     **************************************************************************
C 
      Subroutine Agrif_update_var5d(q,tabvarsindic,locupdate,procname)

      REAL,  DIMENSION(:,:,:,:,:) :: q
      External :: procname
      Optional ::  procname
      INTEGER, DIMENSION(2), OPTIONAL :: locupdate
      INTEGER :: tabvarsindic ! indice of the variable in tabvars
C
      IF (Agrif_Root()) RETURN
C      
      IF (present(locupdate)) THEN
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updateinf = locupdate(1)
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updatesup = locupdate(2)
      ELSE
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updateinf = -99
      Agrif_Curgrid%tabvars(tabvarsindic)%var % updatesup = -99
      ENDIF

      IF (present(procname)) THEN
      Call Agrif_Update_5D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % typeupdate,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),q,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updateinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updatesup,
     & procname)
      ELSE
      Call Agrif_Update_5D(
     & Agrif_Mygrid % tabvars(tabvarsindic) % var % typeupdate,
     & Agrif_Curgrid % parent % tabvars(tabvarsindic),
     & Agrif_Curgrid % tabvars(tabvarsindic),q,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updateinf,
     & Agrif_Curgrid % tabvars(tabvarsindic) % var % updatesup)      
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
      
      print *,'ici'
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
                  
      Subroutine Agrif_Declare_Profile(profilename,posvar,firstpoint,
     &    raf)
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
      
      End Subroutine Agrif_Declare_Profile
              
C
      End module Agrif_bcfunction
