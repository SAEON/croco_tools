!====================================================================
!                   subroutine Agrif_u2dbc_interp_tile
!====================================================================
!   
#include "cppdefs.h"     
#ifdef AGRIF     
      subroutine u2dbc_interp_tile(Istr,Iend,Jstr,Jend)
      use AGRIF_Util
!     implicit none
# include "param.h"
# include "grid.h"
# include "ocean2d.h"   
# include "scalars.h"
# include "climat.h"
# include "boundary.h"
# include "zoom.h"
# include "coupling.h"
      integer Istr,Iend,Jstr,Jend, i,j
      real t1,t2,t3,t7,t8,t9,t10,t11,t12,t13,t14,t15,t16,t17,t18,t19
      real, dimension(:,:), Allocatable :: duinterp
      logical ptinterp
      real t4,t5,t6,tfin,c1,c2,c3,cff1,cff2,cff3
      external :: u2dinterp
      integer :: irhox, irhoy, irhot
      real :: rrhox, rrhoy, rrhot
      real :: tinterp, onemtinterp
      integer :: iifparent
      integer :: iter
      real,dimension(:,:,:),pointer :: parentdu_avg1
      integer :: ipu, jpu, parentnnew
      integer :: parentnbstep
      real :: cff, cffx, cffy
#ifdef MPI
       include 'mpif.h'
#endif
  
# ifdef MPI
#  define LOCALLM Lmmpi
#  define LOCALMM Mmmpi
# else
#  define LOCALLM Lm
#  define LOCALMM Mm
# endif   
!          
# include "compute_auxiliary_bounds.h"
!     return
      irhox=Agrif_Irhox()
      irhoy=Agrif_Irhoy()
      irhot=Agrif_Irhot()

      rrhox = real(irhox)
      rrhoy = real(irhoy)
      rrhot = real(irhot)

C$OMP BARRIER
C$OMP MASTER
      parentnbstep=Agrif_Parent_Nb_Step()
      if (U2DTimeindex .NE. parentnbstep) then
        tinterp=1.
        Allocate(duinterp(GLOBAL_2D_ARRAY))
#ifdef MASKING
        Agrif_UseSpecialValue = .true.
#endif
        Agrif_SpecialValue = 0.
        duinterp=0. 
   
        Call Agrif_Bc_variable(duinterp,DU_avg2,calledweight=tinterp,
     &    procname = u2dinterp)
                  
        Agrif_UseSpecialvalue=.false. 
        
        
        iifparent=
     &Agrif_Parent(iif)
     
#  ifdef AGRIF_OBC_WEST
#   ifdef MPI           
        if (.not.WEST_INTER) then
#   endif
          if (iifparent == 1) then
          i=1
          do j=0,LOCALMM+1
            DU_west2(j,0) = 0.5*(h(i-1,j)+h(i,j)+zeta(i-1,j,kstp)
     & + zeta(i,j,kstp))*on_u(i,j)*ubar(i,j,kstp)
            DU_west1(j,2) = DU_west2(j,0)
          enddo
          endif
          
          do j=0,LOCALMM+1
          DU_west1(j,1) = DU_west1(j,2)
          DU_west2(j,iifparent) = (duinterp(1,j)/rrhoy)
#  ifdef MASKING
     &                               * umask(1,j)
#   endif           
          DU_west1(j,2) = 0.
          enddo
          
          do iter=0,iifparent
          cff1 = weight2(iifparent,iter)
          do j=0,LOCALMM+1
	    DU_west1(j,2) = DU_west1(j,2) + cff1 * DU_west2(j,iter)
          enddo     
          enddo

#   ifdef MPI            
        endif
#   endif 
#  endif 
#  ifdef AGRIF_OBC_EAST
#   ifdef MPI            
        if (.not.EAST_INTER) then
#   endif       
          
          if (iifparent == 1) then
          i=LOCALLM+1
          do j=0,LOCALMM+1
            DU_east2(j,0) = 0.5*(h(i-1,j)+h(i,j)+zeta(i-1,j,kstp)
     & + zeta(i,j,kstp))*on_u(i,j)*ubar(i,j,kstp)
            DU_east1(j,2) = DU_east2(j,0)
          enddo
          endif
          
          do j=0,LOCALMM+1
          DU_east1(j,1) = DU_east1(j,2)
          DU_east2(j,iifparent) = (duinterp(LOCALLM+1,j)/rrhoy)
#  ifdef MASKING
     &                               * umask(LOCALLM+1,j)
#   endif           
          DU_east1(j,2) = 0.
          enddo
          
          do iter=0,iifparent
          cff1 = weight2(iifparent,iter)
          do j=0,LOCALMM+1
	    DU_east1(j,2) = DU_east1(j,2) + cff1 * DU_east2(j,iter)
          enddo     
          enddo
                    
#   ifdef MPI                       
        endif
#   endif 
#  endif 

#  ifdef AGRIF_OBC_SOUTH
#   ifdef MPI            
        if (.not.SOUTH_INTER) then
#   endif
          
          if (iifparent == 1) then
          j=0
          do i=1,LOCALLM+1
            DU_south2(i,0) = 0.5*(h(i-1,j)+h(i,j)+zeta(i-1,j,kstp)
     & + zeta(i,j,kstp))*on_u(i,j)*ubar(i,j,kstp)
            DU_south1(i,2) = DU_south2(i,0)
          enddo
          endif
          
          do i=0,LOCALLM+1
          DU_south1(i,1) = DU_south1(i,2)
          DU_south2(i,iifparent) = (duinterp(i,0)/rrhox)
#  ifdef MASKING
     &                               * umask(i,0)
#   endif           
          DU_south1(i,2) = 0.
          enddo
          
          do iter=0,iifparent
          cff1 = weight2(iifparent,iter)
          do i=0,LOCALLM+1
	    DU_south1(i,2) = DU_south1(i,2) + cff1 * DU_south2(i,iter)
          enddo     
          enddo
                            
#   ifdef MPI           
        endif          
#   endif 
#  endif 
#  ifdef AGRIF_OBC_NORTH
#   ifdef MPI            
        if (.not.NORTH_INTER) then  
#   endif
          
          if (iifparent == 1) then
          j = LOCALMM+1
          do i=1,LOCALLM+1
            DU_north2(i,0) = 0.5*(h(i-1,j)+h(i,j)+zeta(i-1,j,kstp)
     & + zeta(i,j,kstp))*on_u(i,j)*ubar(i,j,kstp)
            DU_north1(i,2) = DU_north2(i,0)
          enddo
          endif
          
          do i=0,LOCALLM+1
          DU_north1(i,1) = DU_north1(i,2)
          DU_north2(i,iifparent) = (duinterp(i,LOCALMM+1)/rrhox)
#  ifdef MASKING
     &                               * umask(i,LOCALMM+1)
#   endif           
          DU_north1(i,2) = 0.
          enddo
          
          do iter=0,iifparent
          cff1 = weight2(iifparent,iter)
          do i=0,LOCALLM+1
	    DU_north1(i,2) = DU_north1(i,2) + cff1 * DU_north2(i,iter)
          enddo     
          enddo
                          
#   ifdef MPI               
        endif  
#   endif                    
#  endif
        U2DTimeindex = parentnbstep
        deallocate(duinterp)
      endif   
      
      tinterp = (1+mod(iif-1+(nbcoarse-1)*mod(nfast,irhot),irhot))/rrhot
      onemtinterp = 1.-tinterp

          cff1 = 1./weight2(iif,iif)
          onemtinterp = cff1 * onemtinterp
          tinterp     = cff1 * tinterp
                            
#  ifdef AGRIF_OBC_WEST
#   ifdef MPI
      if (.not.WEST_INTER) then
#   endif
          
          do j=0,LOCALMM+1
          du_west(j) = onemtinterp*DU_west1(j,1)+tinterp*DU_west1(j,2)
          enddo
          
          do j=0,LOCALMM+1
          DU_west4(j,iif-1) = DU_west3(1,j,iif-1)
          enddo
          
          do iter=0,iif-1
            cff2 = cff1 * weight2(iif,iter)
            do j=0,LOCALMM+1
             du_west(j) = du_west(j) - cff2*DU_west4(j,iter)
            enddo
          enddo
            
#   ifdef MPI            
      endif
#   endif 
#  endif
#  ifdef AGRIF_OBC_EAST
#   ifdef MPI            
      if (.not.EAST_INTER) then
#   endif
          
          do j=0,LOCALMM+1
          DU_east(j) = onemtinterp*DU_east1(j,1)+tinterp*DU_east1(j,2)
          enddo

          do j=0,LOCALMM+1
          DU_east4(j,iif-1) = DU_east3(LOCALLM+1,j,iif-1)
          enddo
                    
          do iter=0,iif-1
            cff2 = cff1 * weight2(iif,iter)
            do j=0,LOCALMM+1
             DU_east(j) = DU_east(j) - cff2*DU_east4(j,iter)
            enddo
          enddo
                            
#   ifdef MPI                       
      endif
#   endif 
#  endif
#  ifdef AGRIF_OBC_SOUTH
#   ifdef MPI      
      if (.not.SOUTH_INTER) Then
#   endif
         
          do i=0,LOCALLM+1
          DU_south(i) = onemtinterp*DU_south1(i,1)+tinterp*DU_south1(i,2)
          enddo
          
          do i=0,LOCALLM+1
          DU_south4(i,iif-1) = DU_south3(i,0,iif-1)
          enddo
          
          do iter=0,iif-1
            cff2 = cff1 * weight2(iif,iter)
            do i=0,LOCALLM+1
             DU_south(i) = DU_south(i) - cff2*DU_south4(i,iter)
            enddo
          enddo
                  
#   ifdef MPI           
      endif          
#   endif 
#  endif
#  ifdef AGRIF_OBC_NORTH
#   ifdef MPI            
      if (.not.NORTH_INTER) then  
#   endif
          
          do i=0,LOCALLM+1
          DU_north(i) = onemtinterp*DU_north1(i,1)+tinterp*DU_north1(i,2)
          enddo
          
          do i=0,LOCALLM+1
          DU_north4(i,iif-1) = DU_north3(i,LOCALMM+1,iif-1)
          enddo          
          
          do iter=0,iif-1
            cff2 = cff1 * weight2(iif,iter)
            do i=0,LOCALLM+1
             DU_north(i) = DU_north(i) - cff2*DU_north4(i,iter)
            enddo
          enddo         
            
#   ifdef MPI               
      endif  
#   endif 
#  endif  
!
C$OMP END MASTER
C$OMP BARRIER
          
!
! Apply the value to ubclm or ubarbry
!
      cffx = g*dtfast*2./(1.+rrhox)

#ifdef AGRIF_2WAY
      cffx = 0.
#endif

# ifdef AGRIF_OBC_WEST
      if (WESTERN_EDGE) then
        do j=JstrR,JendR
          cff = (cffx/om_u(Istr,j))*
#  ifdef Z_FRC_BRY
     &          (zetabry_west(j)-zeta(Istr+1,j,knew))
#  else
     &          (SSH(Istr+1,j)-zeta(Istr+1,j,knew))
#  endif
#  ifdef M2_FRC_BRY
          ubarbry_west(j) = cff +
#  else
          ubclm(Istr,j) = cff +
#  endif
#  ifdef AGRIF_FLUX_BC
     &              (2.*DU_west(j)/((h(Istr-1,j)+zeta(Istr-1,j,knew)
     &                                 +h(Istr,j)+zeta(Istr,j,knew))
     &                                                *on_u(Istr,j)))
#  else
     &                  DU_west(j)
#  endif
#  ifdef MASKING
     &                  *umask(Istr,j)
#  endif
        enddo   

      endif
# endif
# ifdef AGRIF_OBC_EAST
      if (EASTERN_EDGE) then
        do j=JstrR,JendR
          cff = -(cffx/om_u(Iend+1,j))*
#  ifdef Z_FRC_BRY
     &          (zetabry_east(j)-zeta(Iend,j,knew))
#  else
     &          (SSH(Iend,j)-zeta(Iend,j,knew))
#  endif
#  ifdef M2_FRC_BRY
          ubarbry_east(j) = cff +
#  else
          ubclm(Iend+1,j) = cff +
#  endif
#  ifdef AGRIF_FLUX_BC
     &                  (2.*DU_east(j)/(( h(Iend,j)+zeta(Iend,j,knew)
     &                              +h(Iend+1,j)+zeta(Iend+1,j,knew))
     &                                              *on_u(Iend+1,j)))
#  else
     &                      DU_east(j)
#  endif
#  ifdef MASKING
     &                      *umask(Iend+1,j)
#  endif
        enddo       
      endif
# endif
# ifdef AGRIF_OBC_SOUTH
      if (SOUTHERN_EDGE) then       
        do i=IstrU,IendR
#  ifdef M2_FRC_BRY
          ubarbry_south(i)=
#  else
          ubclm(i,Jstr-1)=
#  endif
#  ifdef AGRIF_FLUX_BC
     &              (2.*DU_south(i)/(( h(i,Jstr-1)+zeta(i,Jstr-1,knew)
     &                           +h(i-1,Jstr-1)+zeta(i-1,Jstr-1,knew))
     &                                                *on_u(i,Jstr-1)))
#  else
     &                  DU_south(i)
#  endif
#  ifdef MASKING
     &                  *umask(i,Jstr-1)
#  endif
      enddo
      endif
# endif
# ifdef AGRIF_OBC_NORTH
      if (NORTHERN_EDGE) then
        do i=IstrU,IendR
#  ifdef M2_FRC_BRY
          ubarbry_north(i)=
#  else
          ubclm(i,Jend+1)=
#  endif
#  ifdef AGRIF_FLUX_BC
     &            (2.*DU_north(i)/(( h(i,Jend+1)+zeta(i,Jend+1,knew)
     &                         +h(i-1,Jend+1)+zeta(i-1,Jend+1,knew))
     &                                             *on_u(i,Jend+1)))
#  else
     &                DU_north(i)
#  endif
#  ifdef MASKING
     &                *umask(i,Jend+1)
#  endif
        enddo

      endif
#endif      
      return
      end
      
      subroutine u2Dinterp(tabres,i1,i2,j1,j2)
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean2d.h"
# include "scalars.h"

       integer i1,i2,j1,j2
       real tabres(i1:i2,j1:j2)
       integer i,j
       
       do j=j1,j2
       do i=i1,i2
         tabres(i,j) = 0.5*(h(i-1,j)+zeta(i-1,j,knew)+h(i,j)+
     &  zeta(i,j,knew))*on_u(i,j)*ubar(i,j,knew)
       enddo
       enddo         
       
      return
      end      
!
!====================================================================
!                   subroutine Agrif_v2dbc_interp_tile
!====================================================================
!
      subroutine v2dbc_interp_tile(Istr,Iend,Jstr,Jend)
      use AGRIF_Util
!     implicit none
# include "param.h"
# include "grid.h"
# include "ocean2d.h"
# include "scalars.h"
# include "climat.h"
# include "boundary.h"
# include "zoom.h"
# include "coupling.h"
      integer Istr,Iend,Jstr,Jend, i,j
      real t1,t2,t3,t4,t5,t6,dv1,dv1np1,dv2,tfin,c1,c2,c3
      real t7,t8,t9,t10,t11
      real, Dimension(:,:), Allocatable :: dvinterp
      logical ptinterp
      external :: v2dinterp
      integer :: irhox, irhoy, irhot
      real :: rrhox, rrhoy, rrhot
      real :: tinterp, onemtinterp
      integer :: iifparent
      integer :: iter  
      integer :: parentnbstep    
      real :: cff, cffy
#ifdef MPI
       include 'mpif.h'
#endif
  
# ifdef MPI
#  define LOCALLM Lmmpi
#  define LOCALMM Mmmpi
# else
#  define LOCALLM Lm
#  define LOCALMM Mm
# endif   
!          
# include "compute_auxiliary_bounds.h"
!
!      return
      irhox=Agrif_Irhox()
      irhoy=Agrif_Irhoy()
      irhot=Agrif_Irhot()
      
      rrhox = real(irhox)
      rrhoy = real(irhoy)
      rrhot = real(irhot)

C$OMP BARRIER
C$OMP MASTER
      parentnbstep=Agrif_Parent_Nb_Step()
      if (V2DTimeindex .NE. parentnbstep) then
        ptinterp = .TRUE.
        tinterp=1.
        Allocate(dvinterp(GLOBAL_2D_ARRAY))
#ifdef MASKING        
        Agrif_UseSpecialValue = .true.
#endif        
        Agrif_SpecialValue = 0.
        dvinterp=0.

        Call Agrif_Bc_variable(dvinterp,DV_avg2,calledweight=tinterp,
     &   procname = v2dinterp)
                  
        Agrif_UseSpecialvalue=.false. 

        iifparent=
     &Agrif_Parent(iif)
             
#  ifdef AGRIF_OBC_SOUTH
#   ifdef MPI     
        If (.not.SOUTH_INTER) Then
#   endif
          
          if (iifparent == 1) then
          j=1
          do i=0,LOCALLM+1
            DV_south2(i,0) = 0.5*(h(i,j-1)+h(i,j)+zeta(i,j-1,kstp)
     & + zeta(i,j,kstp))*om_v(i,j)*vbar(i,j,kstp)
            DV_south1(i,2) = DV_south2(i,0)
          enddo
          endif
          
          do i=0,LOCALLM+1
          DV_south1(i,1) = DV_south1(i,2)
          DV_south2(i,iifparent) = (dvinterp(i,1)/rrhox)
#  ifdef MASKING
     &                               * vmask(i,1)
#   endif           
          DV_south1(i,2) = 0.
          enddo
          
          do iter=0,iifparent
          cff1 = weight2(iifparent,iter)
          do i=0,LOCALLM+1
	    DV_south1(i,2) = DV_south1(i,2) + cff1 * DV_south2(i,iter)
          enddo     
          enddo
                    
#   ifdef MPI           
        endif          
#   endif
#  endif
#  ifdef AGRIF_OBC_NORTH
#   ifdef MPI     
        if (.not.NORTH_INTER) then  
#   endif
          
          if (iifparent == 1) then
          j=LOCALMM+1
          do i=0,LOCALLM+1
            DV_north2(i,0) = 0.5*(h(i,j-1)+h(i,j)+zeta(i,j-1,kstp)
     & + zeta(i,j,kstp))*om_v(i,j)*vbar(i,j,kstp)
            DV_north1(i,2) = DV_north2(i,0)
          enddo
          endif
          
          do i=0,LOCALLM+1
          DV_north1(i,1) = DV_north1(i,2)
          DV_north2(i,iifparent) = (dvinterp(i,LOCALMM+1)/rrhox)
#  ifdef MASKING
     &                               * vmask(i,LOCALMM+1)
#   endif           
          DV_north1(i,2) = 0.
          enddo
          
          do iter=0,iifparent
          cff1 = weight2(iifparent,iter)
          do i=0,LOCALLM+1
	    DV_north1(i,2) = DV_north1(i,2) + cff1 * DV_north2(i,iter)
          enddo     
          enddo
                    
#   ifdef MPI
        endif
#   endif
#  endif
#  ifdef AGRIF_OBC_WEST
#   ifdef MPI     
        if (.not.WEST_INTER) then
#   endif
          
          if (iifparent == 1) then
          i = 0
          do j=1,LOCALMM+1
            DV_west2(j,0) = 0.5*(h(i,j-1)+h(i,j)+zeta(i,j-1,kstp)
     & + zeta(i,j,kstp))*om_v(i,j)*vbar(i,j,kstp)
            DV_west1(j,2) = DV_west2(j,0)
          enddo
          endif
          
          do j=0,LOCALMM+1
          DV_west1(j,1) = DV_west1(j,2)
          DV_west2(j,iifparent) = (dvinterp(0,j)/rrhoy)
#  ifdef MASKING
     &                               * vmask(0,j)
#   endif           
          DV_west1(j,2) = 0.
          enddo
          
          do iter=0,iifparent
          cff1 = weight2(iifparent,iter)
          do j=0,LOCALMM+1
	    DV_west1(j,2) = DV_west1(j,2) + cff1 * DV_west2(j,iter)
          enddo     
          enddo
                    
#   ifdef MPI            
        endif
#   endif
#  endif
#  ifdef AGRIF_OBC_EAST
#   ifdef MPI     
        if (.not.EAST_INTER) then
#   endif
          
          if (iifparent == 1) then
          i=LOCALLM+1
          do j=1,LOCALMM+1
            DV_east2(j,0) = 0.5*(h(i,j-1)+h(i,j)+zeta(i,j-1,kstp)
     & + zeta(i,j,kstp))*om_v(i,j)*vbar(i,j,kstp)
            DV_east1(j,2) = DV_east2(j,0)
          enddo
          endif
          
          do j=0,LOCALMM+1
          DV_east1(j,1) = DV_east1(j,2)
          DV_east2(j,iifparent) = (dvinterp(LOCALLM+1,j)/rrhoy)
#  ifdef MASKING
     &                               * vmask(LOCALLM+1,j)
#   endif           
          DV_east1(j,2) = 0.
          enddo
          
          do iter=0,iifparent
          cff1 = weight2(iifparent,iter)
          do j=0,LOCALMM+1
	    DV_east1(j,2) = DV_east1(j,2) + cff1 * DV_east2(j,iter)
          enddo     
          enddo
                     
#   ifdef MPI               
        endif  
#   endif
#  endif
        V2DTimeindex = parentnbstep
        deallocate(dvinterp)
      endif 

      tinterp = (1+mod(iif-1+(nbcoarse-1)*mod(nfast,irhot),irhot))/rrhot
      onemtinterp = 1.-tinterp
      
          cff1 = 1./weight2(iif,iif)
          onemtinterp = cff1 * onemtinterp
          tinterp     = cff1 * tinterp      
             
#  ifdef AGRIF_OBC_SOUTH
#   ifdef MPI     
      If (.not.SOUTH_INTER) Then
#   endif

          do i=0,LOCALLM+1
          DV_south(i) = onemtinterp*DV_south1(i,1)+tinterp*DV_south1(i,2)
          enddo

          do i=0,LOCALLM+1
          DV_south4(i,iif-1) = DV_south3(i,1,iif-1)
          enddo
                    
          do iter=0,iif-1
            cff2 = cff1 * weight2(iif,iter)
            do i=0,LOCALLM+1
             DV_south(i) = DV_south(i) - cff2*DV_south4(i,iter)
            enddo
          enddo
        
#   ifdef MPI           
      endif          
#   endif
#  endif
#  ifdef AGRIF_OBC_NORTH
#   ifdef MPI           
      if (.not.NORTH_INTER) then  
#   endif
        
          do i=0,LOCALLM+1
          DV_north(i) = onemtinterp*DV_north1(i,1)+tinterp*DV_north1(i,2)
          enddo

          do i=0,LOCALLM+1
          DV_north4(i,iif-1) = DV_north3(i,LOCALMM+1,iif-1)
          enddo
                    
          do iter=0,iif-1
            cff2 = cff1 * weight2(iif,iter)
            do i=0,LOCALLM+1
             DV_north(i) = DV_north(i) - cff2*DV_north4(i,iter)
            enddo
          enddo           

#   ifdef MPI
      endif
#   endif
#  endif
#  ifdef AGRIF_OBC_WEST
#   ifdef MPI           
      if (.not.WEST_INTER) then
#   endif
        
          do j=0,LOCALMM+1
          DV_west(j) = onemtinterp*DV_west1(j,1)+tinterp*DV_west1(j,2)
          enddo

          do j=0,LOCALMM+1
          DV_west4(j,iif-1) = DV_west3(0,j,iif-1)
          enddo
                    
          do iter=0,iif-1
            cff2 = cff1 * weight2(iif,iter)
            do j=0,LOCALMM+1
             DV_west(j) = DV_west(j) - cff2*DV_west4(j,iter)
            enddo
          enddo 
                  
#   ifdef MPI            
      endif
#   endif
#  endif
#  ifdef AGRIF_OBC_EAST
#   ifdef MPI           
      if (.not.EAST_INTER) then
#   endif

          do j=0,LOCALMM+1
          DV_east(j) = onemtinterp*DV_east1(j,1)+tinterp*DV_east1(j,2)
          enddo

          do j=0,LOCALMM+1
          DV_east4(j,iif-1) = DV_east3(LOCALLM+1,j,iif-1)
          enddo
                    
          do iter=0,iif-1
            cff2 = cff1 * weight2(iif,iter)
            do j=0,LOCALMM+1
             DV_east(j) = DV_east(j) - cff2*DV_east4(j,iter)
            enddo
          enddo
          
#   ifdef MPI               
      endif  
#   endif
#  endif

C$OMP END MASTER
C$OMP BARRIER
               
!
! Apply the value to vbclm or vbarbry
!
      cffy = g*dtfast*2./(1.+rrhoy)

#ifdef AGRIF_2WAY
      cffy = 0.
#endif

# ifdef AGRIF_OBC_SOUTH
      if (SOUTHERN_EDGE) then
         do i=IstrR,IendR
           cff = (cffy/on_v(i,Jstr))*
#  ifdef Z_FRC_BRY
     &           (zetabry_south(i)-zeta(i,Jstr+1,knew))
#  else
     &           (SSH(i,Jstr+1)-zeta(i,Jstr+1,knew))
#  endif
#  ifdef M2_FRC_BRY
           vbarbry_south(i) = cff +
#  else
           vbclm(i,Jstr) = cff +
#  endif
#  ifdef AGRIF_FLUX_BC
     &              (2.*DV_south(i)/((h(i,Jstr-1)+zeta(i,Jstr-1,knew)
     &                                  +h(i,Jstr)+zeta(i,Jstr,knew))
     &                                                 *om_v(i,Jstr)))
#  else
     &                  DV_south(i)
#  endif
#  ifdef MASKING
     &                  *vmask(i,Jstr)
#  endif 
       enddo     

      endif
# endif 
# ifdef AGRIF_OBC_NORTH
      if (NORTHERN_EDGE) then
        do i=IstrR,IendR
          cff = -(cffy/on_v(i,Jend+1))*
#  ifdef Z_FRC_BRY
     &         (zetabry_north(i)-zeta(i,Jend,knew))
#  else
     &         (SSH(i,Jend)-zeta(i,Jend,knew))
#  endif

#  ifdef M2_FRC_BRY
          vbarbry_north(i) = cff +
#  else
          vbclm(i,Jend+1) = cff +
#  endif
#  ifdef AGRIF_FLUX_BC
     &                  (2.*DV_north(i)/(( h(i,Jend)+zeta(i,Jend,knew)
     &                               +h(i,Jend+1)+zeta(i,Jend+1,knew))
     &                                               *om_v(i,Jend+1)))
#  else
     &                  DV_north(i)
#  endif
#  ifdef MASKING
     &                  *vmask(i,Jend+1)
#  endif 
        enddo        

	
      endif
# endif 
# ifdef AGRIF_OBC_EAST       
      if (EASTERN_EDGE) then
        do j=JstrV,JendR
#  ifdef M2_FRC_BRY
          vbarbry_east(j)=
#  else
          vbclm(Iend+1,j)=
#  endif
#  ifdef AGRIF_FLUX_BC
     &             (2.*DV_east(j)/(( h(Iend+1,j)+zeta(Iend+1,j,knew)
     &                           +h(Iend+1,j-1)+zeta(Iend+1,j-1,knew))
     &                                             *om_v(Iend+1,j)))
#  else
     &                 DV_east(j)
#  endif
#  ifdef MASKING
     &                 *vmask(Iend+1,j)
#  endif
        enddo
      endif       
# endif 
# ifdef AGRIF_OBC_WEST             
      if (WESTERN_EDGE) then
        do j=JstrV,JendR
#  ifdef M2_FRC_BRY
          vbarbry_west(j)=
#  else
          vbclm(Istr-1,j)=
#  endif
#  ifdef AGRIF_FLUX_BC
     &                (2.*DV_west(j)/((h(Istr-1,j)+zeta(Istr-1,j,knew)
     &                           +h(Istr-1,j-1)+zeta(Istr-1,j-1,knew))
     &                                                *om_v(Istr-1,j)))
#  else
     &                  DV_west(j)
#  endif
#  ifdef MASKING
     &                  *vmask(Istr-1,j)
#  endif
        enddo
      endif
# endif
      return
      end

      subroutine v2Dinterp(tabres,i1,i2,j1,j2)
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean2d.h"
# include "scalars.h"

       integer i1,i2,j1,j2
       real tabres(i1:i2,j1:j2)
       integer i,j    
       
       do j=j1,j2
       do i=i1,i2
         tabres(i,j) = 0.5*(h(i,j-1)+zeta(i,j-1,knew)+h(i,j)+
     &  zeta(i,j,knew))*om_v(i,j)*vbar(i,j,knew)
       enddo
       enddo
      
      return
      end   

!
                  
!
!====================================================================
!                   subroutine Agrif_zetabc_interp_tile
!====================================================================
!
      subroutine zetabc_interp_tile(Istr,Iend,Jstr,Jend)
      use AGRIF_Util
!     implicit none
# include "param.h"
# include "boundary.h"
# include "climat.h"
# include "grid.h"
# include "ocean2d.h"
# include "scalars.h"
# include "zoom.h"
      integer Istr,Iend,Jstr,Jend, i,j, i1, j1
      real tinterp
      real,Dimension(:,:), Allocatable :: zext
      Integer itrcind
      INTEGER :: parentknew, parentkstp,nbstep3dparent
      real t1,t2,t3,t4,t5,t6,t9,t10,t11,t7
      real tin(2), tout(2)
      real cff1
      real zeta1,zeta2
      external zetainterp
      integer :: irhot, irhox, irhoy
      real :: rrhot
      integer :: iifparent
      integer :: iter
      real :: onemtinterp
      integer :: parentnbstep
            
#  ifdef MPI
#   define LOCALLM Lmmpi
#   define LOCALMM Mmmpi
#  else
#   define LOCALLM Lm
#   define LOCALMM Mm
#  endif
!
# include "compute_auxiliary_bounds.h"

!
!       return      
       irhot = Agrif_Irhot()
       irhox = Agrif_Irhox()
       irhoy = Agrif_Irhoy()
       rrhot = real(irhot)

C$OMP BARRIER
C$OMP MASTER

      parentnbstep=Agrif_Parent_Nb_Step()

      if (ZetaTimeindex .NE. parentnbstep) then
            Allocate(zext(GLOBAL_2D_ARRAY))
#ifdef MASKING            
            Agrif_UseSpecialvalue=.true.
#endif            
            Agrif_Specialvalue=0.
            tinterp=1.
            zext=0.
            Call Agrif_Bc_variable(zext,Zt_avg1,
     &                                        calledweight=tinterp,
     &                                        procname=zetainterp)
            Agrif_UseSpecialvalue=.false. 
            
        iifparent=
     &Agrif_Parent(iif)
     
#  ifdef AGRIF_OBC_SOUTH             
#   ifdef MPI        
              if (.not.SOUTH_INTER) then
#   endif
                
             if (iifparent == 1) then
              do j=0,1
              do i=0,LOCALLM+1
               Zeta_south2(i,j,0) = zeta(i,j,kstp)
               Zeta_south1(i,j,2) = Zeta_south2(i,j,0)
              enddo
              enddo
             endif
             
             do j=0,1
             do i=0,LOCALLM+1
               Zeta_south1(i,j,1)=Zeta_south1(i,j,2)
               Zeta_south2(i,j,iifparent)=zext(i,j)
               Zeta_south1(i,j,2) = 0.               
             enddo
             enddo
             
             do iter=0,iifparent
             cff1=weight2(iifparent,iter)
             do j=0,1 
             do i=0,LOCALLM+1             
               Zeta_south1(i,j,2) = Zeta_south1(i,j,2)+
     &                                cff1*Zeta_south2(i,j,iter)
             enddo
             enddo
             enddo
                                
#   ifdef MPI         
              endif          
#   endif
#  endif
#  ifdef AGRIF_OBC_NORTH             
#   ifdef MPI         
              if (.not.NORTH_INTER) then
#   endif                
             if (iifparent == 1) then
              do j=LOCALMM,LOCALMM+1
              do i=0,LOCALLM+1
               Zeta_north2(i,j,0) = zeta(i,j,kstp)
               Zeta_north1(i,j,2) = Zeta_north2(i,j,0)
              enddo
              enddo
             endif
             
             do j=LOCALMM,LOCALMM+1
             do i=0,LOCALLM+1
               Zeta_north1(i,j,1)=Zeta_north1(i,j,2)
               Zeta_north2(i,j,iifparent)=zext(i,j)
               Zeta_north1(i,j,2) = 0.               
             enddo
             enddo
             
             do iter=0,iifparent
             cff1=weight2(iifparent,iter)
             do j=LOCALMM,LOCALMM+1
             do i=0,LOCALLM+1             
               Zeta_north1(i,j,2) = Zeta_north1(i,j,2)+
     &                                cff1*Zeta_north2(i,j,iter)
             enddo
             enddo
             enddo                 
#   ifdef MPI                    
              endif  
#   endif
#  endif
#  ifdef AGRIF_OBC_WEST            
#   ifdef MPI         
              if (.not.WEST_INTER) then
#   endif
                
             if (iifparent == 1) then
                do j=0,LOCALMM+1
                do i=0,1
               Zeta_west2(i,j,0) = zeta(i,j,kstp)
               Zeta_west1(i,j,2) = Zeta_west2(i,j,0)
              enddo
              enddo
             endif
             
                do j=0,LOCALMM+1
                do i=0,1
               Zeta_west1(i,j,1)=Zeta_west1(i,j,2)
               Zeta_west2(i,j,iifparent)=zext(i,j)
               Zeta_west1(i,j,2) = 0.               
             enddo
             enddo
             
             do iter=0,iifparent
             cff1=weight2(iifparent,iter)
                do j=0,LOCALMM+1
                do i=0,1
               Zeta_west1(i,j,2) = Zeta_west1(i,j,2)+
     &                                cff1*Zeta_west2(i,j,iter)
             enddo
             enddo
             enddo                
                                
#   ifdef MPI            
              endif
#   endif
#  endif
#  ifdef AGRIF_OBC_EAST             
#   ifdef MPI         
              if (.not.EAST_INTER) then
#   endif
                
             if (iifparent == 1) then
                do j=0,LOCALMM+1
                do i=LOCALLM,LOCALLM+1
               Zeta_east2(i,j,0) = zeta(i,j,kstp)
               Zeta_east1(i,j,2) = Zeta_east2(i,j,0)
              enddo
              enddo
             endif
             
                do j=0,LOCALMM+1
                do i=LOCALLM,LOCALLM+1
               Zeta_east1(i,j,1)=Zeta_east1(i,j,2)
               Zeta_east2(i,j,iifparent)=zext(i,j)
               Zeta_east1(i,j,2) = 0.               
             enddo
             enddo
             
             do iter=0,iifparent
             cff1=weight2(iifparent,iter)
                do j=0,LOCALMM+1
                do i=LOCALLM,LOCALLM+1
               Zeta_east1(i,j,2) = Zeta_east1(i,j,2)+
     &                                cff1*Zeta_east2(i,j,iter)
             enddo
             enddo
             enddo                 
                                
#   ifdef MPI               
              endif
#   endif                   
#  endif
            ZetaTimeindex = parentnbstep
            Deallocate(zext)
          endif
C$OMP END MASTER
C$OMP BARRIER

          tinterp = (1+mod(iif-1+(nbcoarse-1)*
     &               mod(nfast,irhot),irhot))/rrhot
          onemtinterp = 1.-tinterp

          cff1 = 1./weight2(iif,iif)
          onemtinterp = cff1 * onemtinterp
          tinterp     = cff1 * tinterp

#  ifdef AGRIF_OBC_SOUTH             
          if (SOUTHERN_EDGE) then              
#    ifdef Z_FRC_BRY
            do j = 0,0
#    else
            do j = 0,1
#    endif
              do i = Istr,Iend
#    ifdef Z_FRC_BRY
                zetabry_south(i)= 
#    else
                SSH(i,j)= 
#    endif
     &                   onemtinterp*Zeta_south1(i,j,1)+
     &                   tinterp*Zeta_south1(i,j,2)
              enddo
          
              do i=Istr,Iend
                Zeta_south4(i,j,iif-1) = Zeta_south3(i,j,iif-1)
              enddo
          
              do iter = 0,iif-1
                cff2 = cff1 * weight2(iif,iter)
                do i=Istr,Iend
#    ifdef Z_FRC_BRY
                  zetabry_south(i) = zetabry_south(i)
#    else
                  SSH(i,j) = SSH(i,j)
#    endif
     &                      - cff2*Zeta_south4(i,j,iter)
                enddo
              enddo
            enddo
          endif
#  endif
#  ifdef AGRIF_OBC_NORTH 
          if (NORTHERN_EDGE) then              
#    ifdef Z_FRC_BRY
            do j = Jend+1,Jend+1      
#    else
            do j = Jend,Jend+1      
#    endif
              do i=Istr,Iend
#    ifdef Z_FRC_BRY
                zetabry_north(i)=
#    else
                SSH(i,j)= 
#    endif
     &                   onemtinterp*Zeta_north1(i,j,1)+
     &                   tinterp*Zeta_north1(i,j,2)
              enddo
       
              do i=Istr,Iend
                Zeta_north4(i,j,iif-1)=Zeta_north3(i,j,iif-1)
              enddo
          
              do iter = 0,iif-1
                cff2 = cff1 * weight2(iif,iter)
                do i=Istr,Iend
#    ifdef Z_FRC_BRY
                  zetabry_north(i) = zetabry_north(i)
#    else
                  SSH(i,j) = SSH(i,j)
#    endif
     &                       - cff2*Zeta_north4(i,j,iter)
                enddo
              enddo
            enddo
          endif
#  endif
#  ifdef AGRIF_OBC_WEST 
          if (WESTERN_EDGE) then              
#    ifdef Z_FRC_BRY
            do i = 0,0
#    else
            do i = 0,1
#    endif
              do j=Jstr,Jend
#    ifdef Z_FRC_BRY
                zetabry_west(j)=
#    else
                SSH(i,j)= 
#    endif
     &                   onemtinterp*Zeta_west1(i,j,1)+
     &                   tinterp*Zeta_west1(i,j,2)
              enddo         
         
              do j=Jstr,Jend
                Zeta_west4(i,j,iif-1)=Zeta_west3(i,j,iif-1)
              enddo

              do iter = 0,iif-1
                cff2 = cff1 * weight2(iif,iter)
                do j=Jstr,Jend
#    ifdef Z_FRC_BRY
                  zetabry_west(j) = zetabry_west(j)
#    else
                  SSH(i,j) = SSH(i,j)
#    endif
     &                       - cff2*Zeta_west4(i,j,iter)
                enddo
              enddo
            enddo
          endif
#  endif
#  ifdef AGRIF_OBC_EAST
          if (EASTERN_EDGE) then
#    ifdef Z_FRC_BRY
            do i =  Iend+1,Iend+1
#    else
            do i =  Iend,Iend+1
#    endif
              do j=Jstr,Jend
#    ifdef Z_FRC_BRY
                zetabry_east(j)=  
#    else
                SSH(i,j)= 
#    endif
     &                   onemtinterp*Zeta_east1(i,j,1)+
     &                   tinterp*Zeta_east1(i,j,2)
              enddo
        
              do j=Jstr,Jend
                Zeta_east4(i,j,iif-1)=Zeta_east3(i,j,iif-1)
              enddo
          
              do iter = 0,iif-1
                cff2 = cff1 * weight2(iif,iter)
                do j=Jstr,Jend
#    ifdef Z_FRC_BRY
                  zetabry_east(j) = zetabry_east(j)
#    else
                  SSH(i,j) = SSH(i,j)
#    endif
     &                       - cff2*Zeta_east4(i,j,iter)
                enddo
              enddo
            enddo
          endif
#  endif
     
      return
      end
      
      subroutine zetainterp(tabres,i1,i2,j1,j2)
      implicit none
# include "param.h"
# include "ocean2d.h"
# include "scalars.h"

      integer i1,i2,j1,j2
      real tabres(i1:i2,j1:j2)
       
      tabres(i1:i2,j1:j2) = zeta(i1:i2,j1:j2,knew)
      
      return
      end      
#else
      subroutine zoombc_2D_empty()
      return
      end
#endif
