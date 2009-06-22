      Module Agrif_Variables 
      Use Agrif_CurgridFunctions
      
      Contains
      Subroutine Agrif_Declare_Variable(posvar,firstpoint,
     &    raf,lb,ub,varid,torestore)
      character*(80) :: variablename
      Type(Agrif_List_Variables), Pointer :: newvariable,newvariablep
      INTEGER, DIMENSION(:) :: posvar
      INTEGER, DIMENSION(:) :: lb,ub
      INTEGER, DIMENSION(:) :: firstpoint
      CHARACTER(*) ,DIMENSION(:) :: raf        
      TYPE(Agrif_Pvariable), Pointer :: parent_var,root_var
      INTEGER :: dimensio
      INTEGER :: varid
      LOGICAL, OPTIONAL :: torestore
      LOGICAL :: restaure
            
!      if (agrif_root()) return

      variablename = 'xxx'
      
      restaure = .FALSE.
      if (agrif_mygrid%ngridstep /= 0) then
        if (present(torestore)) restaure = torestore
      endif
      
      dimensio = SIZE(posvar)
C
C    
      Allocate(newvariable)
      Allocate(newvariable%pvar)
      Allocate(newvariable%pvar%var)
      Allocate(newvariable%pvar%var%posvar(dimensio))
      Allocate(newvariable%pvar%var%interptab(dimensio))
      newvariable%pvar%var%variablename = variablename
      newvariable%pvar%var%interptab = raf
      newvariable%pvar%var%nbdim = dimensio
      newvariable%pvar%var%posvar = posvar
      newvariable%pvar%var%point(1:dimensio) = firstpoint
      newvariable%pvar%var%restaure = restaure

      newvariable%pvar%var%lb(1:dimensio) = lb(1:dimensio)
      newvariable%pvar%var%ub(1:dimensio) = ub(1:dimensio)
      
      if (restaure) then
      select case(dimensio)
      case(1)
                  Allocate( newvariable%pvar%var%Restore1D(  
     &      lb(1):ub(1)))         
            newvariable%pvar%var%Restore1D = 0      
      case(2)
                  Allocate( newvariable%pvar%var%Restore2D(  
     &      lb(1):ub(1),lb(2):ub(2)))         
            newvariable%pvar%var%Restore2D = 0
      case(3)
                  Allocate( newvariable%pvar%var%Restore3D(  
     &      lb(1):ub(1),lb(2):ub(2),lb(3):ub(3)))         
            newvariable%pvar%var%Restore3D = 0
      case(4)
                  Allocate( newvariable%pvar%var%Restore4D(  
     &      lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4)))         
            newvariable%pvar%var%Restore4D = 0
      case(5)
                  Allocate( newvariable%pvar%var%Restore5D(  
     &      lb(1):ub(1),lb(2):ub(2),lb(3):ub(3),lb(4):ub(4),
     &      lb(5):ub(5)))         
            newvariable%pvar%var%Restore5D = 0            
      end select
      endif
      
      newvariable % nextvariable => Agrif_Curgrid%variables
      
      Agrif_Curgrid%variables => newvariable
      Agrif_Curgrid%Nbvariables = Agrif_Curgrid%Nbvariables + 1

      varid = -Agrif_Curgrid%Nbvariables

!       if (agrif_curgrid%parent%nbvariables < agrif_curgrid%nbvariables)
!     &       then
!      Allocate(newvariablep)
!      Allocate(newvariablep%pvar)
!      Allocate(newvariablep%pvar%var)      
!      Allocate(newvariablep%pvar%var%posvar(dimensio))
!      Allocate(newvariablep%pvar%var%interptab(dimensio))
!      newvariablep%pvar%var%variablename = variablename
!      newvariablep%pvar%var%interptab = raf
!      newvariablep%pvar%var%nbdim = dimensio
!      newvariablep%pvar%var%posvar = posvar
!      newvariablep%pvar%var%point(1:dimensio) = firstpoint
!      newvariablep%pvar%var%restaure = restaure      
! 
!      newvariablep%pvar%var%lb(1:dimensio) = lb(1:dimensio)
!      newvariablep%pvar%var%ub(1:dimensio) = ub(1:dimensio)
!           
!      newvariablep % nextvariable => Agrif_Curgrid%parent%variables
!      
!      Agrif_Curgrid%parent%variables => newvariablep       
!      
!      Agrif_Curgrid%parent%Nbvariables = 
!     &    Agrif_Curgrid%parent%Nbvariables + 1
!      parent_var=>newvariablep%pvar
!      else
!      parent_var=>Agrif_Search_Variable
!     &              (Agrif_Curgrid%parent,Agrif_Curgrid%nbvariables)
!       endif
       
       if (.not.agrif_root()) then
      parent_var=>Agrif_Search_Variable
     &              (Agrif_Curgrid%parent,Agrif_Curgrid%nbvariables)       
       
       newvariable%pvar%parent_var=>parent_var
       do i=1,dimensio
         if (parent_var%var%interptab(i)=='N') then
           parent_var%var%lb(i)=lb(i)
           parent_var%var%ub(i)=ub(i)
         endif
       enddo
       endif       
      
      root_var=>Agrif_Search_Variable
     &              (Agrif_Mygrid,Agrif_Curgrid%nbvariables)
     
      newvariable%pvar%var%root_var=>root_var%var
      
           
      End Subroutine Agrif_Declare_Variable

      FUNCTION Agrif_Search_Variable(grid,varid)
      integer :: varid
      Type(Agrif_Pvariable), Pointer :: Agrif_Search_variable
      Type(Agrif_grid), Pointer :: grid
      
      Type(Agrif_List_Variables), pointer :: parcours
      Logical :: foundvariable
      integer nb
      integer :: varidinv
      
      foundvariable = .FALSE.
      parcours => grid%variables
      varidinv = 1 + grid%nbvariables - varid

      do nb=1,varidinv-1
         parcours => parcours%nextvariable    
      End Do
      
      Agrif_Search_variable => parcours%pvar
      
      
      End Function Agrif_Search_variable
      
      End Module Agrif_Variables      