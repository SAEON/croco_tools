

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
C     Foundation, Inc., 59 Temple Place- Suite 330, Boston, MA 02111-1307, USA.
C
C
C
CCC   Module Agrif_Arrays
C      
      Module Agrif_Arrays
      Use Agrif_Types
C
      implicit none
C      
      Contains
C     **************************************************************************
CCC   Subroutine Agrif_Childbounds
C     **************************************************************************
C
      Subroutine Agrif_Childbounds(nbdim,lboundloc,uboundloc,
     &                 pttab,petab,pttruetab,cetruetab,memberin)
C
CCC   Description:
CCC   Subroutine calculating the global indices of the child grid
C
C
C     Declarations:
C

C
C     Arguments
      INTEGER :: nbdim
      INTEGER,DIMENSION(nbdim) :: lboundloc,uboundloc
      INTEGER,DIMENSION(nbdim) :: pttab,petab,pttruetab,cetruetab
      LOGICAL :: memberin
C
C     Local variables
      INTEGER :: i,lbglob,ubglob
C



C
C
      do i = 1,nbdim
C
        lbglob = lboundloc(i)
        ubglob = uboundloc(i)
C
C
        pttruetab(i) = max(pttab(i),lbglob)        
C
        cetruetab(i) = min(petab(i),ubglob)
C
C
      enddo

      memberin = .TRUE.

      do i=1,nbdim
        IF (cetruetab(i) < pttruetab(i)) THEN
          memberin = .FALSE.
          EXIT
        ENDIF
      enddo
C
      Return
C
C
      End Subroutine Agrif_Childbounds
C
C
C     **************************************************************************
CCC   Subroutine Agrif_nbdim_Get_bound
C     **************************************************************************
C
      Subroutine Agrif_nbdim_Get_bound(Variable,
     &                           lower,upper,indice,nbdim)
C
CCC   Description:
CCC   This subroutine is used to get the lower and the upper boundaries of a
C     table. Output datas are scalar.
C
C     Declarations:
C      
      
C
C     Arguments      
C
      ! we want extract boundaries of this table
      TYPE(AGRIF_Variable), Pointer :: Variable    
      INTEGER              :: lower,upper ! output data
      ! direction in wich we want to know the dimension
      INTEGER              :: indice      
      INTEGER              :: nbdim       ! dimension of the table
C
C     Local variables
C
      SELECT CASE (nbdim)
      CASE (1)
           lower = lbound(Variable % array1,indice)
           upper = ubound(Variable % array1,indice)
      CASE (2)
           lower = lbound(Variable % array2,indice)
           upper = ubound(Variable % array2,indice)
      CASE (3)
           lower = lbound(Variable % array3,indice)
           upper = ubound(Variable % array3,indice)
      CASE (4)
           lower = lbound(Variable % array4,indice)
           upper = ubound(Variable % array4,indice)
      CASE (5)
           lower = lbound(Variable % array5,indice)
           upper = ubound(Variable % array5,indice)
      CASE (6)
           lower = lbound(Variable % array6,indice)
           upper = ubound(Variable % array6,indice)
      END SELECT
C
      return
C
      End Subroutine Agrif_nbdim_Get_bound       
C
C
C     **************************************************************************
CCC   Subroutine Agrif_Get_bound_dimension
C     **************************************************************************
C
      Subroutine Agrif_nbdim_Get_bound_dimension(Variable,
     &                              lower,upper,nbdim)
C
CCC   Description:
CCC   This subroutine is used to get the lower and the upper boundaries of a
C        table. Output datas are scalar.
C
C     Declarations:
C      
      
C
C     Arguments      
C
      ! we want extract boundaries of this table
      TYPE(AGRIF_Variable), Pointer     :: Variable    
      INTEGER                  :: nbdim       ! dimension of the table
      INTEGER,DIMENSION(nbdim) :: lower,upper ! output data
C
C     Local variables       
C
      SELECT CASE (nbdim)
      CASE (1)
           lower = lbound(Variable % array1)
           upper = ubound(Variable % array1)
      CASE (2)
           lower = lbound(Variable % array2)
           upper = ubound(Variable % array2)
      CASE (3)
           lower = lbound(Variable % array3)
           upper = ubound(Variable % array3)
      CASE (4)
           lower = lbound(Variable % array4)
           upper = ubound(Variable % array4)
      CASE (5)
           lower = lbound(Variable % array5)
           upper = ubound(Variable % array5)
      CASE (6)
           lower = lbound(Variable % array6)
           upper = ubound(Variable % array6)
      END SELECT
C
      return
C
      End Subroutine Agrif_nbdim_Get_bound_dimension     
      
C     **************************************************************************
CCC   Subroutine Agrif_nbdim_allocation
C     **************************************************************************
C
      Subroutine Agrif_nbdim_allocation(Variable,inf,sup,nbdim)
C
CCC   Description:
CCC   This subroutine is used to Allocate the table Variable
C
C     Declarations:
C      
      
C
C     Arguments      
C
      TYPE(AGRIF_Variable), Pointer     :: Variable    
      INTEGER                  :: nbdim       ! dimension of the table
      INTEGER,DIMENSION(nbdim) :: inf,sup
C
C     Local variables       
C
      SELECT CASE (nbdim)
      CASE (1)
         allocate(Variable%array1(
     &            inf(1):sup(1)))
      CASE (2)
          allocate(Variable%array2(
     &            inf(1):sup(1),
     &            inf(2):sup(2)))
      CASE (3)
         allocate(Variable%array3(
     &            inf(1):sup(1),
     &            inf(2):sup(2),
     &            inf(3):sup(3)))
      CASE (4)
         allocate(Variable%array4(
     &            inf(1):sup(1),
     &            inf(2):sup(2),
     &            inf(3):sup(3),
     &            inf(4):sup(4)))
      CASE (5)
         allocate(Variable%array5(
     &            inf(1):sup(1),
     &            inf(2):sup(2),
     &            inf(3):sup(3),
     &            inf(4):sup(4),
     &            inf(5):sup(5)))
      CASE (6)
         allocate(Variable%array6(
     &            inf(1):sup(1),
     &            inf(2):sup(2),
     &            inf(3):sup(3),
     &            inf(4):sup(4),
     &            inf(5):sup(5),
     &            inf(6):sup(6)))
      END SELECT
C
      return
C
      End Subroutine Agrif_nbdim_allocation
C
C
C     **************************************************************************
CCC   Subroutine Agrif_nbdim_deallocation
C     **************************************************************************
C
      Subroutine Agrif_nbdim_deallocation(Variable,nbdim)
C
CCC   Description:
CCC   This subroutine is used to give the same value to the table Variable
C
C     Declarations:
C      
      
C
C     Arguments      
C
      TYPE(AGRIF_Variable), Pointer     :: Variable    
      INTEGER                  :: nbdim       ! dimension of the table
C
C     Local variables       
C

      SELECT CASE (nbdim)
      CASE (1)
         Deallocate(Variable%array1)
      CASE (2)
         Deallocate(Variable%array2)
      CASE (3)
         Deallocate(Variable%array3)
      CASE (4)
         Deallocate(Variable%array4)
      CASE (5)
         Deallocate(Variable%array5)
      CASE (6)
         Deallocate(Variable%array6)
      END SELECT
C
      return
C
      End Subroutine Agrif_nbdim_deallocation
C
C
C     **************************************************************************
CCC   Subroutine Agrif_nbdim_Full_VarEQreal
C     **************************************************************************
C
      Subroutine Agrif_nbdim_Full_VarEQreal(Variable,Value,nbdim)
C
CCC   Description:
CCC   This subroutine is used to get the lower and the upper boundaries of a
C        table. Output datas are scalar.
C
C     Declarations:
C      
      
C
C     Arguments      
C
      TYPE(AGRIF_Variable), Pointer :: Variable    
      REAL                 :: Value       
      INTEGER              :: nbdim       ! dimension of the table
C
C     Local variables       
C
      SELECT CASE (nbdim)
      CASE (1)
        Variable%array1 = Value      
      CASE (2)
        Variable%array2 = Value
      CASE (3)
        Variable%array3 = Value
      CASE (4)
        Variable%array4 = Value
      CASE (5)
        Variable%array5 = Value
      CASE (6)
        Variable%array6 = Value
      END SELECT
C
      return
C
      End Subroutine Agrif_nbdim_Full_VarEQreal       
C
C
C     **************************************************************************
CCC   Subroutine Agrif_nbdim_VarEQreal
C     **************************************************************************
C
      Subroutine Agrif_nbdim_VarEQreal(Variable,inf,sup,Value,nbdim)
C
CCC   Description:
CCC   This subroutine is used to give the same value to a part of 
C        the table Variable
C
C     Declarations:
C      
      
C
C     Arguments      
C
      TYPE(AGRIF_Variable), Pointer :: Variable    
      REAL                 :: Value       
      INTEGER              :: nbdim       ! dimension of the table
      INTEGER,DIMENSION(nbdim) :: inf,sup
C
C     Local variables       
C
      SELECT CASE (nbdim)
      CASE (1)
         Variable%array1(
     &             inf(1):sup(1)
     &             )  = Value
      CASE (2)
         Variable%array2(
     &             inf(1):sup(1),
     &             inf(2):sup(2)
     &             )  = Value
      CASE (3)
         Variable%array3(
     &             inf(1):sup(1),
     &             inf(2):sup(2),
     &             inf(3):sup(3)
     &             )  = Value
      CASE (4)
         Variable%array4(
     &             inf(1):sup(1),
     &             inf(2):sup(2),
     &             inf(3):sup(3),
     &             inf(4):sup(4)
     &             )  = Value
      CASE (5)
         Variable%array5(
     &             inf(1):sup(1),
     &             inf(2):sup(2),
     &             inf(3):sup(3),
     &             inf(4):sup(4),
     &             inf(5):sup(5)
     &             )  = Value
      CASE (6)
         Variable%array6(
     &             inf(1):sup(1),
     &             inf(2):sup(2),
     &             inf(3):sup(3),
     &             inf(4):sup(4),
     &             inf(5):sup(5),
     &             inf(6):sup(6)
     &             )  = Value
      END SELECT
C
      return
C
      End Subroutine Agrif_nbdim_VarEQreal       
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_nbdim_VarEQvar
C     **************************************************************************
C
      Subroutine Agrif_nbdim_VarEQvar(Variable,inf,sup,
     &                                Variable2,inf2,sup2,
     &                                nbdim)
C
CCC   Description:
CCC   This subroutine is used to give the value of a part of the table 
C        Variable2 to the table Variable
C
C     Declarations:
C      
      
C
C     Arguments      
C
      TYPE(AGRIF_Variable), Pointer     :: Variable
      TYPE(AGRIF_Variable), Pointer     :: Variable2
      INTEGER                  :: nbdim       ! dimension of the table
      INTEGER,DIMENSION(nbdim) :: inf,sup
      INTEGER,DIMENSION(nbdim) :: inf2,sup2
C
C     Local variables       
C
      SELECT CASE (nbdim)
      CASE (1)
         Variable%array1(inf(1):sup(1)) = 
     &         Variable2%array1(inf2(1):sup2(1))
      CASE (2)
         Variable%array2(inf(1):sup(1),
     &                         inf(2):sup(2)) = 
     &         Variable2%array2(inf2(1):sup2(1),
     &                          inf2(2):sup2(2))
      CASE (3)
         Variable%array3(inf(1):sup(1),
     &                         inf(2):sup(2), 
     &                         inf(3):sup(3)) = 
     &         Variable2%array3(inf2(1):sup2(1),
     &                          inf2(2):sup2(2),
     &                          inf2(3):sup2(3))
      CASE (4)
        Variable%array4(inf(1):sup(1),
     &                         inf(2):sup(2), 
     &                         inf(3):sup(3),
     &                         inf(4):sup(4)) = 
     &         Variable2%array4(inf2(1):sup2(1),
     &                          inf2(2):sup2(2),
     &                          inf2(3):sup2(3),
     &                          inf2(4):sup2(4))
      CASE (5)
        Variable%array5(inf(1):sup(1),
     &                         inf(2):sup(2), 
     &                         inf(3):sup(3),
     &                         inf(4):sup(4), 
     &                         inf(5):sup(5)) = 
     &         Variable2%array5(inf2(1):sup2(1),
     &                          inf2(2):sup2(2),
     &                          inf2(3):sup2(3),
     &                          inf2(4):sup2(4),
     &                          inf2(5):sup2(5))
      CASE (6)
        Variable%array6(inf(1):sup(1),
     &                         inf(2):sup(2),
     &                         inf(3):sup(3),
     &                         inf(4):sup(4),
     &                         inf(5):sup(5),
     &                         inf(6):sup(6)) = 
     &         Variable2%array6(inf2(1):sup2(1),
     &                          inf2(2):sup2(2),
     &                          inf2(3):sup2(3),
     &                          inf2(4):sup2(4),
     &                          inf2(5):sup2(5),
     &                          inf2(6):sup2(6))
      END SELECT
C
      return
C
      End Subroutine Agrif_nbdim_VarEQvar
C
C     **************************************************************************
CCC   Subroutine Agrif_nbdim_Full_VarEQvar
C     **************************************************************************
C
      Subroutine Agrif_nbdim_Full_VarEQvar(Variable,Variable2,
     &                                nbdim)
C
CCC   Description:
CCC   This subroutine is used to give the value of the table Variable2 
C        to the table Variable
C
C     Declarations:
C      
      
C
C     Arguments      
C
      TYPE(AGRIF_Variable), Pointer     :: Variable
      TYPE(AGRIF_Variable), Pointer     :: Variable2
      INTEGER                  :: nbdim       ! dimension of the table
C
C     Local variables       
C
      SELECT CASE (nbdim)
      CASE (1)
          Variable%array1 = Variable2%array1
      CASE (2)
          Variable%array2 = Variable2%array2
      CASE (3)
          Variable%array3 = Variable2%array3
      CASE (4)
          Variable%array4 = Variable2%array4
      CASE (5)
          Variable%array5 = Variable2%array5
      CASE (6)
          Variable%array6 = Variable2%array6
      END SELECT
C
      return
C
      End Subroutine Agrif_nbdim_Full_VarEQvar
C
C
C     **************************************************************************
CCC   Subroutine Agrif_array2vector
C     **************************************************************************
C
      Subroutine Agrif_array2vector(array,bounds,vector,nbdim)
C
CCC   Description:
CCC   This subroutine is used to record the array into the vector
C
C     Declarations:
C      
      
C
C     Arguments      
C
      TYPE(AGRIF_Variable), Pointer       :: array
      REAL, DIMENSION(:)         :: vector      ! Array used for the time 
      INTEGER                    :: nbdim       ! dimension of the table
      INTEGER,DIMENSION(nbdim,2) :: bounds
C
C     Local variables       
C
      INTEGER                      :: nind,ir,jr,kr,lr,mr,nr
C
      SELECT CASE (nbdim)
      CASE (1)
         nind=0
         do ir=bounds(1,1),bounds(1,2)
            nind=nind+1
            array%array1(ir) = vector(nind)
         enddo       
C
      CASE (2)
         nind=0
          do jr=bounds(2,1),bounds(2,2)
           do ir=bounds(1,1),bounds(1,2)
               nind=nind+1
               array%array2(ir,jr) = vector(nind)
           enddo
         enddo        
C
      CASE (3)
         nind=0
        do kr=bounds(3,1),bounds(3,2)
           do jr=bounds(2,1),bounds(2,2)
             do ir=bounds(1,1),bounds(1,2)
                  nind=nind+1
                  array%array3(ir,jr,kr) = vector(nind)
             enddo
           enddo
         enddo      
C
      CASE (4)
         nind=0
        do lr=bounds(4,1),bounds(4,2)
          do kr=bounds(3,1),bounds(3,2)
             do jr=bounds(2,1),bounds(2,2)
               do ir=bounds(1,1),bounds(1,2)
                     nind=nind+1
                     array%array4(ir,jr,kr,lr) = vector(nind)
               enddo
             enddo
           enddo
         enddo          
C
      CASE (5)
         nind=0
         do mr=bounds(5,1),bounds(5,2)
         do lr=bounds(4,1),bounds(4,2)
          do kr=bounds(3,1),bounds(3,2) 
              do jr=bounds(2,1),bounds(2,2)
                 do ir=bounds(1,1),bounds(1,2)
                     nind=nind+1
                     array%array5(ir,jr,kr,lr,mr) = vector(nind)
                 enddo
               enddo
             enddo
           enddo
         enddo        
C
      CASE (6)
         nind=0
        do nr=bounds(6,1),bounds(6,2)
          do mr=bounds(5,1),bounds(5,2)
          do lr=bounds(4,1),bounds(4,2)
           do kr=bounds(3,1),bounds(3,2)
             do jr=bounds(2,1),bounds(2,2)
                   do ir=bounds(1,1),bounds(1,2)
                     nind=nind+1
                     array%array6(ir,jr,kr,lr,mr,nr) = vector(nind)
                   enddo
                 enddo
               enddo
             enddo
           enddo
         enddo       
        END SELECT
C
      return
C
      End Subroutine Agrif_array2vector
C
C
C
C     **************************************************************************
CCC   Subroutine Agrif_vector2array
C     **************************************************************************
C
      Subroutine Agrif_vector2array(vector,array,bounds,nbdim)
C
CCC   Description:
CCC   This subroutine is used to record the array into the vector
C
C     Declarations:
C      
      
C
C     Arguments      
C
      TYPE(AGRIF_Variable), Pointer       :: array
      REAL, DIMENSION(:)         :: vector      ! Array used for the time 
      INTEGER                    :: nbdim       ! dimension of the table
      INTEGER,DIMENSION(nbdim,2) :: bounds
C
C     Local variables       
C
      INTEGER                      :: nind,ir,jr,kr,lr,mr,nr
C
      SELECT CASE (nbdim)
      CASE (1)
         nind=0
         do ir=bounds(1,1),bounds(1,2)
            nind=nind+1
            vector(nind) = array%array1(ir)
         enddo       
C
      CASE (2)
         nind=0
        do jr=bounds(2,1),bounds(2,2)
           do ir=bounds(1,1),bounds(1,2)
               nind=nind+1
               vector(nind) = array%array2(ir,jr)
           enddo
         enddo      
C
      CASE (3)
         nind=0
        do kr=bounds(3,1),bounds(3,2)
          do jr=bounds(2,1),bounds(2,2)
             do ir=bounds(1,1),bounds(1,2)
                  nind=nind+1
                  vector(nind) = array%array3(ir,jr,kr)
             enddo
           enddo
         enddo       
C
      CASE (4)
         nind=0
          do lr=bounds(4,1),bounds(4,2)
           do kr=bounds(3,1),bounds(3,2)
             do jr=bounds(2,1),bounds(2,2)
               do ir=bounds(1,1),bounds(1,2)
                     nind=nind+1
                     vector(nind) = array%array4(ir,jr,kr,lr)
               enddo
             enddo
           enddo
         enddo         
C
      CASE (5)
         nind=0
         do mr=bounds(5,1),bounds(5,2)
           do lr=bounds(4,1),bounds(4,2)
            do kr=bounds(3,1),bounds(3,2) 
              do jr=bounds(2,1),bounds(2,2)
                 do ir=bounds(1,1),bounds(1,2)
                     nind=nind+1
                     vector(nind) = array%array5(ir,jr,kr,lr,mr)
                 enddo
               enddo
             enddo
           enddo
         enddo   
C
      CASE (6)
         nind=0
        do nr=bounds(6,1),bounds(6,2)
           do mr=bounds(5,1),bounds(5,2)
             do lr=bounds(4,1),bounds(4,2)
               do kr=bounds(3,1),bounds(3,2)
                do jr=bounds(2,1),bounds(2,2)
                   do ir=bounds(1,1),bounds(1,2)
                     nind=nind+1
                     vector(nind) = array%array6(ir,jr,kr,lr,mr,nr)
                   enddo
                 enddo
               enddo
             enddo
           enddo
         enddo       
      END SELECT
C
      return
C
      End Subroutine Agrif_vector2array

C     **************************************************************************
CCC   Subroutine GiveAgrif_SpecialValueToTab
C     **************************************************************************
C
      Subroutine GiveAgrif_SpecialValueToTab(Variable1,Variable2,
     &                  lower,upper,Value,nbdim)
C
CCC   Description:
CCC   
C
C     Declarations:
C      
      
C
C     Arguments      
C
      TYPE(AGRIF_VARIABLE), Pointer    :: Variable1
      TYPE(AGRIF_VARIABLE), Pointer    :: Variable2
      INTEGER                  :: nbdim
      INTEGER,DIMENSION(nbdim) :: lower,upper
      REAL                     :: Value
C
C     Local variables       
C
      SELECT CASE (nbdim)
      CASE (1)
             Where (Variable1 % array1(
     &           lower(1):upper(1))
     &            == Value)
             Variable2 % array1(lower(1):upper(1))
     &                        = Value
C      
              End Where
      CASE (2)
             Where (Variable1 % array2(
     &           lower(1):upper(1),
     &           lower(2):upper(2)) 
     &            == Value)
             Variable2 % array2(lower(1):upper(1),
     &                          lower(2):upper(2))
     &                        = Value
C      
              End Where
      CASE (3)
             Where (Variable1 % array3(
     &           lower(1):upper(1),
     &           lower(2):upper(2), 
     &           lower(3):upper(3)) 
     &            == Value)
             Variable2 % array3(lower(1):upper(1),
     &                          lower(2):upper(2),
     &                          lower(3):upper(3))
     &                         = Value
C      
              End Where
      CASE (4)
             Where (Variable1 % array4(
     &           lower(1):upper(1),
     &           lower(2):upper(2), 
     &           lower(3):upper(3),
     &           lower(4):upper(4)) 
     &            == Value)
             Variable2 % array4(lower(1):upper(1),
     &                          lower(2):upper(2),
     &                          lower(3):upper(3),
     &                          lower(4):upper(4))
     &                        = Value
C      
              End Where
      CASE (5)
             Where (Variable1 % array5(
     &           lower(1):upper(1),
     &           lower(2):upper(2),
     &           lower(3):upper(3),
     &           lower(4):upper(4),
     &           lower(5):upper(5)) 
     &            == Value)
             Variable2 % array5(lower(1):upper(1),
     &                          lower(2):upper(2),
     &                          lower(3):upper(3),
     &                          lower(4):upper(4),
     &                          lower(5):upper(5))
     &                        = Value
C      
              End Where
      CASE (6)
             Where (Variable1 % array6(
     &           lower(1):upper(1),
     &           lower(2):upper(2),
     &           lower(2):upper(3),
     &           lower(4):upper(4),
     &           lower(5):upper(5),
     &           lower(6):upper(6)) 
     &            == Value)
             Variable2 % array6(lower(1):upper(1),
     &                          lower(2):upper(2),
     &                          lower(3):upper(3),
     &                          lower(4):upper(4),
     &                          lower(5):upper(5),
     &                          lower(6):upper(6))
     &                        = Value
C      
              End Where
      END SELECT
C
      return
C
      End Subroutine GiveAgrif_SpecialValueToTab   
C
C

C     **************************************************************************
CCC   Subroutine PreProcessToInterpOrUpdate
C     **************************************************************************
C
      Subroutine PreProcessToInterpOrUpdate(parent,child,
     &             petab_Child,
     &             pttab_Child,pttab_Parent,
     &             s_Child,s_Parent,
     &             ds_Child,ds_Parent,
     &             nbdim)
C
CCC   Description:
CCC   
C
C     Declarations:
C      
C     arguments                                   
      TYPE(AGRIF_PVariable) :: parent   ! Variable on the parent grid
      TYPE(AGRIF_PVariable) :: child    ! Variable on the child grid
      INTEGER :: nbdim                  
      INTEGER,DIMENSION(6) :: pttab_child  
      INTEGER,DIMENSION(6) :: petab_child      
      INTEGER,DIMENSION(6) :: pttab_parent  
      TYPE(AGRIF_Variable), Pointer :: root ! Pointer on the variable of the 
                                            ! root grid
      TYPE(Agrif_Grid), Pointer :: Agrif_Child_Gr,Agrif_Parent_Gr
      REAL, DIMENSION(6) :: s_child,s_parent
      REAL, DIMENSION(6) :: ds_child,ds_parent
C     locals variables
      INTEGER :: n
      
C
C     Arguments      
C

C
C     Local variables       
C
      Agrif_Child_Gr => Agrif_Curgrid
      Agrif_Parent_Gr => Agrif_Curgrid % parent
C
      root => child % var % root_var 
C
C     Number of dimensions of the current grid
      nbdim = root % nbdim
C      
      do n=1,nbdim
C            
        Select case(root % interptab(n))
C
C       Value of interptab(n) can be either x,y,z or N for a no space 
C       DIMENSION            
C
C         The DIMENSION is 'x'
          case('x')
C
            pttab_Child(n) = root % point(1)
C            
            pttab_Parent(n) = root % point(1)
C        
            s_Child(n) = Agrif_Child_Gr % Agrif_x(1)
C
            s_Parent(n) = Agrif_Parent_Gr % Agrif_x(1)
C        
            ds_Child(n) = Agrif_Child_Gr % Agrif_d(1)
C
            ds_Parent(n) = Agrif_Parent_Gr % Agrif_d(1)
C                      
            if (root % posvar(n).EQ.1) then
C          
              petab_Child(n) = pttab_Child(n) + Agrif_Child_Gr%nb(1)
C        
              else
C          
                petab_Child(n) = pttab_Child(n) + 
     &                              Agrif_Child_Gr%nb(1) - 1
C          
                s_Child(n) = s_Child(n) + ds_Child(n)/2.
C
                s_Parent(n) = s_Parent(n) + ds_Parent(n)/2.
C        
            endif                  
C
C         The DIMENSION is 'y'
          case('y')
C
            pttab_Child(n) = root % point(2)
C            
            pttab_Parent(n) = root % point(2)
C        
            s_Child(n) = Agrif_Child_Gr % Agrif_x(2)
C
            s_Parent(n) = Agrif_Parent_Gr % Agrif_x(2) 
C        
            ds_Child(n) = Agrif_Child_Gr % Agrif_d(2)
C
            ds_Parent(n) = Agrif_Parent_Gr % Agrif_d(2)
C                      
            if (root % posvar(n).EQ.1) then
C          
             petab_Child(n) = pttab_Child(n) + Agrif_Child_Gr%nb(2)
C          
              else
C          
                petab_Child(n) = pttab_Child(n) + 
     &                       Agrif_Child_Gr%nb(2) - 1
C          
                s_Child(n) = s_Child(n) + ds_Child(n)/2.
C
                s_Parent(n) = s_Parent(n) + ds_Parent(n)/2.
C        
            endif
            
C
C         The DIMENSION is 'z'                            
          case('z')
C
            pttab_Child(n) = root % point(3)
C            
            pttab_Parent(n) = root % point(3)
C        
            s_Child(n) = Agrif_Child_Gr % Agrif_x(3)
C
            s_Parent(n) = Agrif_Parent_Gr % Agrif_x(3)
C        
            ds_Child(n) = Agrif_Child_Gr % Agrif_d(3)
C
            ds_Parent(n) = Agrif_Parent_Gr % Agrif_d(3)
C                      
            if (root % posvar(n).EQ.1) then
C          
             petab_Child(n) = pttab_Child(n) + Agrif_Child_Gr%nb(3)
C          
              else
C          
                petab_Child(n) = pttab_Child(n) + 
     &                      Agrif_Child_Gr%nb(3) - 1
C
                s_Child(n) = s_Child(n) + ds_Child(n)/2.
C
                s_Parent(n) = s_Parent(n) + ds_Parent(n)/2.
C        
            endif       
C 
C         The DIMENSION is not space                            
          case('N')
C
C         The next coefficients are calculated in order to do a simple copy of 
C         values of the grid variable when the procedure of interpolation is 
C         called for this DIMENSION 
C      
            Call Agrif_nbdim_Get_bound(child % var,
     &                           pttab_Child(n),petab_Child(n),n,nbdim)
C
C           No interpolation but only a copy of the values of the grid variable
C
            pttab_Parent(n) = pttab_Child(n)
C              
            s_Child(n)=0.
C      
            s_Parent(n)=0. 
C      
            ds_Child(n)=1.
C      
            ds_Parent(n)=1.
C
        End select
C            
      enddo     
C
      return
C
      End Subroutine PreProcessToInterpOrUpdate     

C
C

      End Module Agrif_Arrays
