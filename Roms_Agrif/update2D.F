#include "cppdefs.h"
!====================================================================
!                   subroutine Agrif_update
!====================================================================
!
# if defined AGRIF && defined SOLVE3D && defined AGRIF_2WAY
      subroutine update2d
!
! Update in case of 2-way nesting
!
      use Agrif_Util
      implicit none
#  include "param.h"
#  include "grid.h"
#  include "ocean3d.h"
#  include "ocean2d.h"
#  include "scalars.h"
#  include "zoom.h"	
#  include "coupling.h"
      real tabtemp(GLOBAL_2D_ARRAY,4)
      external updateubar,updatevbar,updatezeta
      external updateduavg2, updatedvavg2
      real,dimension(:,:),pointer :: ztavg1_parent,duavg2parent
      real,dimension(:,:),pointer :: parenth,parenton_u,parentom_v
      real,dimension(:,:),pointer ::dvavg2parent
      integer i,j,ipu,jpu,ipv,jpv,ipr,jpr
      real :: dtparent
      real,dimension(:,:),pointer :: pmparent,pnparent,rmaskparent
      real,dimension(:,:,:),pointer :: zetaparent,parentzeta
      real,dimension(:,:,:),pointer :: parentubar,parentdu_avg1
      real,dimension(:,:,:),pointer :: parentdv_avg1,parentvbar
      real :: t1,t2,t3,t4,t5,t6,tind(5)
      integer j1
      integer :: parentknew,parentiif,parentnnew,parentkstp
      integer :: iter, irhox, irhoy, irhot
      real :: cff
      real :: invrrhot
      integer :: pp

      
!$AGRIF_DO_NOT_TREAT
      logical global_update_2d
      common/globalupdate/global_update_2d
!$AGRIF_END_DO_NOT_TREAT      
# ifdef MPI
#  define LOCALLM Lmmpi
#  define LOCALMM Mmmpi
# else
#  define LOCALLM Lm
#  define LOCALMM Mm
# endif 
      
!       return

      irhot = Agrif_Irhot()
      
!      IF (iif == nfast) THEN
!      IF (nbcoarse == 1) THEN
!      DU_avg1(:,:,4) = 0.
!      DV_avg1(:,:,4) = 0.     
!      ENDIF
      
!      invrrhot = 1./real(irhot)
!      DU_avg1(:,:,4) = DU_avg1(:,:,4) + invrrhot * DU_avg2(:,:)
!      DV_avg1(:,:,4) = DV_avg1(:,:,4) + invrrhot * DV_avg2(:,:)
!      ENDIF
      

      
      IF ((mod(iif+(nbcoarse-1)*mod(nfast,irhot),irhot).NE.0)) RETURN

C$OMP BARRIER
C$OMP SINGLE
        irhox = Agrif_Irhox()
        irhoy = Agrif_Irhoy()
             


     
      Call Agrif_Set_Parent(indupdate,0) 
        
#ifdef MASKING
      Agrif_UseSpecialValueInUpdate = .TRUE.
#endif
      Agrif_SpecialValueFineGrid = 0.
      
      indupdate = 0
      
      global_update_2d = .FALSE.
           
      Call Agrif_Update_Variable(tabtemp,Zt_avg3,
     &         locupdate=(/0,0/),procname = updatezeta)

      Agrif_UseSpecialValueInUpdate = .FALSE.
            
      Call Agrif_Update_Variable(tabtemp,DU_avg1,
     &         locupdate=(/0,0/),procname = updateubar)
     
      Call Agrif_Update_Variable(tabtemp,DV_avg1,
     &         locupdate=(/0,0/),procname = updatevbar) 


          
      
      IF (iif == nfast) THEN
        
      IF (nbcoarse == irhot) THEN

      global_update_2d = .TRUE.

#ifdef MASKING
      Agrif_UseSpecialValueInUpdate = .TRUE.
#endif
      Agrif_SpecialValueFineGrid = 0.
      
      Call Agrif_Update_Variable(tabtemp,Zt_avg3,
     &         procname = updatezeta)
      
      Call Agrif_Update_Variable(tabtemp,DU_avg1,
     &         procname = updateubar)
     
      Call Agrif_Update_Variable(tabtemp,DV_avg1,
     &         procname = updatevbar)      
      
!      Call Agrif_Update_Variable(tabtemp(:,:,1),DU_avg2,
!     &         procname = updateduavg2)
     
!      Call Agrif_Update_Variable(tabtemp(:,:,1),DV_avg2,
!     &         procname = updatedvavg2)  

      Agrif_UseSpecialValueInUpdate = .FALSE.

             
      ENDIF
      
      ENDIF
C$OMP END SINGLE

      return

      ztavg1_parent=>
     &Agrif_Parent(Zt_avg1)
      zetaparent=>
     &Agrif_Parent(zeta)
      parentknew=
     &Agrif_Parent(knew)
      parentkstp=
     &Agrif_Parent(kstp)     
      parentiif=
     &Agrif_Parent(iif)
     
      parentzeta=>
     &Agrif_Parent(zeta)
      parenton_u=>
     &Agrif_Parent(on_u)
      parentom_v=>
     &Agrif_Parent(om_v)     
      parentubar=>
     &Agrif_Parent(ubar)  
      parentvbar=>
     &Agrif_Parent(vbar)      
      parentdu_avg1=>
     &Agrif_Parent(DU_avg1)
      parentdv_avg1=>
     &Agrif_Parent(DV_avg1)     
      parentnnew=
     &Agrif_Parent(nnew)
      parenth=>
     &Agrif_Parent(h)
           
      ztavg1_parent=>
     &Agrif_Parent(Zt_avg1)
      duavg2parent=>
     &Agrif_Parent(DU_avg2) 
      dvavg2parent=>
     &Agrif_Parent(DV_avg2)       
      pmparent=>
     &Agrif_parent(pm)
      pnparent=>
     &Agrif_parent(pn)     
      dtparent=
     &Agrif_Parent(dt)
      
      i=1
      ipu=Agrif_ix()+1-1+(i-1)/3
      Do j=2,Mm-1,3
      jpu=Agrif_iy()+1-1+(j-2)/3
      
      t3 = sum(du_avg1(i,j-1:j+1,4))
      t1=-DUavg2parent(ipu,jpu)+t3
      t2=dtparent*t1*pmparent(ipu-1,jpu)*pnparent(ipu-1,jpu)
      Ztavg1_parent(ipu-1,jpu)=Ztavg1_parent(ipu-1,jpu)-t2
      EndDo 
      
      i=Lm+1
      ipu=Agrif_ix()+1-1+(i-1)/3
      Do j=2,Mm-1,3
      jpu=Agrif_iy()+1-1+(j-2)/3
      t3 = sum(du_avg1(i,j-1:j+1,4))

      t1=-DUavg2parent(ipu,jpu)+t3
      t2=dtparent*t1*pmparent(ipu,jpu)*pnparent(ipu,jpu)
      Ztavg1_parent(ipu,jpu)=Ztavg1_parent(ipu,jpu)+t2
      EndDo 
      
      j=1
      jpv=Agrif_iy()+1-1+(j-1)/3
      Do i=2,Lm-1,3
      ipv=Agrif_ix()+1-1+(i-2)/3
      
      t3 = sum(dv_avg1(i-1:i+1,j,4))
      
            
      t1=-DVavg2parent(ipv,jpv)+t3
      t2=dtparent*t1*pmparent(ipv,jpv-1)*pnparent(ipv,jpv-1)
      Ztavg1_parent(ipv,jpv-1)=Ztavg1_parent(ipv,jpv-1)-t2
      EndDo  
      
      j=Mm+1
      jpv=Agrif_iy()+1-1+(j-1)/3
      Do i=2,Lm-1,3
      ipv=Agrif_ix()+1-1+(i-2)/3
      t3 = sum(dv_avg1(i-1:i+1,j,4))
                 
      t1=-DVavg2parent(ipv,jpv)+t3
      t2=dtparent*t1*pmparent(ipv,jpv)*pnparent(ipv,jpv)
      Ztavg1_parent(ipv,jpv)=Ztavg1_parent(ipv,jpv)+t2
      EndDo    
      
      return
      end
      
      Subroutine Updateubar(tabres,i1,i2,j1,j2,k1,k2,before,nb,ndir)
      implicit none
#  include "param.h"
#  include "grid.h"
#  include "ocean2d.h"
#  include "coupling.h"
#  include "scalars.h"
#  include "zoom.h"
      integer i1,i2,j1,j2,k1,k2
      real tabres(i1:i2,j1:j2,k1:k2)
      logical before
      integer nb, ndir
      real hzu
      real t1,t2,t3
      
      integer i,j
       real t4,t8
      integer :: iter, iifparent
      integer :: oldindupdate
      real :: rrhoy
      real :: cff, cff1, cff2
      logical :: western_side, eastern_side,northern_side,southern_side
!$AGRIF_DO_NOT_TREAT
      logical global_update_2d
      common/globalupdate/global_update_2d
      real,dimension(:,:,:),pointer :: finevalues
      real,dimension(:,:,:),pointer :: coarsevalues
      integer :: iiffine, irhotfine, nbgrid
      common/updateubar2val/finevalues,coarsevalues, iiffine,irhotfine,
     &   nbgrid
!$AGRIF_END_DO_NOT_TREAT  
       integer irhot, ibegin,isize  
       real tabtemp(i1:i2,j1:j2) 
       integer :: n1,n2,n3 
              
              
      IF (before) THEN
         rrhoy = real(Agrif_Irhoy())
 
         IF (global_update_2d) THEN
           tabres(i1:i2,j1:j2,1) = rrhoy * DU_avg1(i1:i2,j1:j2,nnew)
           RETURN
         ENDIF
                  
         irhot=Agrif_Irhot()
         
         iiffine = iif
         irhotfine = irhot
         nbgrid = Agrif_Fixed()
         
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)
         southern_side = (nb == 2).AND.(ndir == 1) 
         northern_side = (nb == 2).AND.(ndir == 2)                                   

         ibegin = min(irhot,iif+1)
	 IF (iif .LE. irhot) THEN
	 ibegin = iif + 1
	 ENDIF
	                 
         IF (western_side) THEN      
          do iter=1,ibegin
          do j=j1,j2  
          do i=i1,i2                 
          tabres(i,j,iter) = du_west3(i,j,iif-iter+1)
          enddo
          enddo          
          enddo
         ELSE IF (eastern_side) THEN
          do iter=1,ibegin
          do j=j1,j2  
          do i=i1,i2                 
          tabres(i,j,iter) = du_east3(i,j,iif-iter+1)
          enddo
          enddo           
          enddo
         ELSE IF (northern_side) THEN
          do iter=1,ibegin
          do j=j1,j2  
          do i=i1,i2                 
          tabres(i,j,iter) = du_north3(i,j,iif-iter+1)
          enddo
          enddo           
          enddo
         ELSE IF (southern_side) THEN
          do iter=1,ibegin
          do j=j1,j2  
          do i=i1,i2                 
          tabres(i,j,iter) = du_south3(i,j,iif-iter+1)
          enddo
          enddo           
          enddo
         ENDIF
         
         tabres = rrhoy * tabres
         
       ELSE

       IF (global_update_2d) THEN
         DU_avg1(i1:i2,j1:j2,nnew) = tabres(i1:i2,j1:j2,1)
#ifdef MASKING
     &             *umask(i1:i2,j1:j2)
#endif         
         RETURN
       ENDIF
              
       ibegin = min(irhotfine,iiffine+1)

	 IF (iiffine .LE. irhotfine) THEN
	 ibegin = iiffine + 1
	 ENDIF
	        
       isize = (j2-j1+1)*(i2-i1+1)
       
       IF ((nbstep3d == 0).AND.(iif == 1)) THEN
       IF (.NOT.Associated(finevalues)) THEN
       Allocate(finevalues(isize,0:nfast,1))
       Allocate(coarsevalues(isize,0:nfast,1))
       ELSE       
       CALL checksize(indupdate+isize,nfast)
       ENDIF
       ENDIF
       
          do iter=1,ibegin
          oldindupdate = indupdate
          do j=j1,j2  
          do i=i1,i2
          oldindupdate = oldindupdate + 1
          finevalues(oldindupdate,iiffine-iter+1,nbgrid)
     &         = tabres(i,j,iter)
          enddo
          enddo          
          enddo
          
          IF (iif == 1) THEN
          oldindupdate = indupdate
          do j=j1,j2
          do i=i1,i2
          oldindupdate = oldindupdate + 1
          coarsevalues(oldindupdate,0,nbgrid) = DU_avg1(i,j,nstp)
          enddo
          enddo        
          ENDIF   
          
          
         tabtemp = 0.
         do iter=0,iif-1
         cff = -weight2(iif,iter)
         call copy1d(tabtemp,coarsevalues(indupdate+1,iter,nbgrid),
     &               cff,isize)
         enddo    
         
         do iter=0,iiffine
         cff=weight2(iiffine,iter)
         call copy1d(tabtemp,finevalues(indupdate+1,iter,nbgrid),
     &               cff,isize)
         enddo
         
         tabtemp = tabtemp/weight2(iif,iif)
                               
         DO j=j1,j2
           DO i=i1,i2
	   t1 = 0.5*(zeta(i,j,knew)+h(i,j)+
     &  zeta(i-1,j,knew)+h(i-1,j))*on_u(i,j)
            
           t2 = tabtemp(i,j)
#ifdef MASKING
     &           * umask(i,j)
#endif
          ubar(i,j,knew) = t2/t1
         indupdate = indupdate + 1
          coarsevalues(indupdate,iif,nbgrid) = tabtemp(i,j)
              
          ENDDO
         ENDDO 
       ENDIF
      
      return
      end 
      
      Subroutine Updatezeta(tabres,i1,i2,j1,j2,k1,k2,before,nb,ndir)
      implicit none
#  include "param.h"
#  include "grid.h"
#  include "ocean2d.h"
#  include "coupling.h"
#  include "scalars.h"
#  include "zoom.h"
      integer i1,i2,j1,j2,k1,k2
      real tabres(i1:i2,j1:j2,k1:k2)
      logical before
      integer nb, ndir
      real hzu
      real t1,t2,t3
      
      integer i,j
       real t4,t8
      integer :: iter, iifparent
      integer :: oldindupdate
      real :: cff, cff1, cff2
      logical :: western_side, eastern_side,northern_side,southern_side
!$AGRIF_DO_NOT_TREAT
      logical global_update_2d
      common/globalupdate/global_update_2d
      real,dimension(:,:,:),pointer :: finevalues
      real,dimension(:,:,:),pointer :: coarsevalues
      integer :: iiffine, irhotfine, nbgrid
      common/updateubar2val/finevalues,coarsevalues, iiffine,irhotfine,
     &   nbgrid
!$AGRIF_END_DO_NOT_TREAT  
       integer irhot, ibegin,isize  
       real tabtemp(i1:i2,j1:j2) 
       integer :: n1,n2,n3 
              
      IF (before) THEN
 
         IF (global_update_2d) THEN
           tabres(i1:i2,j1:j2,1) = Zt_avg1(i1:i2,j1:j2)
           RETURN
         ENDIF
                 
         irhot=Agrif_Irhot()
         
         iiffine = iif
         irhotfine = irhot
         nbgrid = Agrif_Fixed()
         
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)
         southern_side = (nb == 2).AND.(ndir == 1) 
         northern_side = (nb == 2).AND.(ndir == 2)                                   

         ibegin = min(irhot,iif+1)
	 
	 IF (iif .LE. irhot) THEN
	 ibegin = iif + 1
	 ENDIF
                
         IF (western_side) THEN   
	   
          do iter=1,ibegin
          do j=j1,j2  
          do i=i1,i2                 
          tabres(i,j,iter) = Zeta_west3(i,j,iif-iter+1)
          enddo
          enddo          
          enddo
         ELSE IF (eastern_side) THEN
          do iter=1,ibegin
          do j=j1,j2  
          do i=i1,i2                 
          tabres(i,j,iter) = Zeta_east3(i,j,iif-iter+1)
          enddo
          enddo           
          enddo
         ELSE IF (northern_side) THEN
          do iter=1,ibegin
          do j=j1,j2  
          do i=i1,i2                 
          tabres(i,j,iter) = Zeta_north3(i,j,iif-iter+1)
          enddo
          enddo           
          enddo
         ELSE IF (southern_side) THEN
          do iter=1,ibegin
          do j=j1,j2  
          do i=i1,i2                 
          tabres(i,j,iter) = Zeta_south3(i,j,iif-iter+1)
          enddo
          enddo           
          enddo
         ENDIF
         
       ELSE

       IF (global_update_2d) THEN
         Zt_avg1(i1:i2,j1:j2) = tabres(i1:i2,j1:j2,1)
#ifdef MASKING
     &             *rmask(i1:i2,j1:j2)
#endif         
         RETURN
       ENDIF
              
       ibegin = min(irhotfine,iiffine+1)
       
	 IF (iiffine .LE. irhotfine) THEN
	 ibegin = iiffine + 1
	 ENDIF
       
       isize = (j2-j1+1)*(i2-i1+1)
       
       IF ((nbstep3d == 0).AND.(iif == 1)) THEN
       IF (.NOT.Associated(finevalues)) THEN
       Allocate(finevalues(isize,0:nfast,1))
       Allocate(coarsevalues(isize,0:nfast,1))
       ELSE       
       CALL checksize(indupdate+isize,nfast)
       ENDIF
       ENDIF
       
          do iter=1,ibegin
          oldindupdate = indupdate
          do j=j1,j2  
          do i=i1,i2
          oldindupdate = oldindupdate + 1
          finevalues(oldindupdate,iiffine-iter+1,nbgrid)
     &         = tabres(i,j,iter)
          enddo
          enddo          
          enddo
          
          IF (iif == 1) THEN
          oldindupdate = indupdate
          do j=j1,j2
          do i=i1,i2
          oldindupdate = oldindupdate + 1
          coarsevalues(oldindupdate,0,nbgrid) = zeta(i,j,kstp)
          enddo
          enddo        
          ENDIF   
          
          
         tabtemp = 0.
         do iter=0,iif-1
         cff = -weight2(iif,iter)
         call copy1d(tabtemp,coarsevalues(indupdate+1,iter,nbgrid),
     &               cff,isize)
         enddo    
         
         do iter=0,iiffine
         cff=weight2(iiffine,iter)
         call copy1d(tabtemp,finevalues(indupdate+1,iter,nbgrid),
     &               cff,isize)        
         enddo
         
         tabtemp = tabtemp/weight2(iif,iif)
                               
         DO j=j1,j2
           DO i=i1,i2
            
           t2 = tabtemp(i,j)
#ifdef MASKING
     &           * rmask(i,j)
#endif
          zeta(i,j,knew) = t2
         indupdate = indupdate + 1
          coarsevalues(indupdate,iif,nbgrid) = tabtemp(i,j)
              
          ENDDO
         ENDDO 
       ENDIF
      
      return
      end           
  
      Subroutine Updatevbar(tabres,i1,i2,j1,j2,k1,k2,before,nb,ndir)
      implicit none
#  include "param.h"
#  include "grid.h"
#  include "ocean2d.h"
#  include "coupling.h"
#  include "scalars.h"
#  include "zoom.h"
      integer i1,i2,j1,j2,k1,k2
      real tabres(i1:i2,j1:j2,k1:k2)
      logical before
      integer nb, ndir
      real hzu
      real t1,t2,t3
      
      integer i,j
       real t4,t8
      integer :: iter, iifparent
      integer :: oldindupdate
      real :: rrhox
      real :: cff, cff1, cff2
      logical :: western_side, eastern_side,northern_side,southern_side
!$AGRIF_DO_NOT_TREAT
      logical global_update_2d
      common/globalupdate/global_update_2d
      real,dimension(:,:,:),pointer :: finevalues
      real,dimension(:,:,:),pointer :: coarsevalues
      integer :: iiffine, irhotfine, nbgrid
      common/updateubar2val/finevalues,coarsevalues, iiffine,irhotfine,
     &   nbgrid
!$AGRIF_END_DO_NOT_TREAT  
       integer irhot, ibegin,isize  
       real tabtemp(i1:i2,j1:j2) 
       integer :: n1,n2,n3 
              
              
      IF (before) THEN
         rrhox = real(Agrif_Irhox())

         IF (global_update_2d) THEN
           tabres(i1:i2,j1:j2,1) = rrhox * DV_avg1(i1:i2,j1:j2,nnew)
           RETURN
         ENDIF
                  
         irhot=Agrif_Irhot()
         
         iiffine = iif
         irhotfine = irhot
         nbgrid = Agrif_Fixed()
         
         western_side  = (nb == 1).AND.(ndir == 1)
         eastern_side  = (nb == 1).AND.(ndir == 2)
         southern_side = (nb == 2).AND.(ndir == 1) 
         northern_side = (nb == 2).AND.(ndir == 2)

         ibegin = min(irhot,iif+1)
	 IF (iif .LE. irhot) THEN
	 ibegin = iif + 1
	 ENDIF

         IF (western_side) THEN      
          do iter=1,ibegin
          do j=j1,j2  
          do i=i1,i2                 
          tabres(i,j,iter) = dv_west3(i,j,iif-iter+1)
          enddo
          enddo          
          enddo
         ELSE IF (eastern_side) THEN
          do iter=1,ibegin
          do j=j1,j2
          do i=i1,i2
          tabres(i,j,iter) = dv_east3(i,j,iif-iter+1)
          enddo
          enddo           
          enddo
         ELSE IF (northern_side) THEN
          do iter=1,ibegin
          do j=j1,j2
          do i=i1,i2
          tabres(i,j,iter) = dv_north3(i,j,iif-iter+1)
          enddo
          enddo
          enddo
         ELSE IF (southern_side) THEN
          do iter=1,ibegin
          do j=j1,j2
          do i=i1,i2
          tabres(i,j,iter) = dv_south3(i,j,iif-iter+1)
          enddo
          enddo
          enddo
         ENDIF

         tabres = rrhox * tabres

       ELSE

       IF (global_update_2d) THEN
         DV_avg1(i1:i2,j1:j2,nnew) = tabres(i1:i2,j1:j2,1)
#ifdef MASKING
     &             *vmask(i1:i2,j1:j2)
#endif
         RETURN
       ENDIF

       ibegin = min(irhotfine,iiffine+1)

	 IF (iiffine .LE. irhotfine) THEN
	 ibegin = iiffine + 1
	 ENDIF
	        
       isize = (j2-j1+1)*(i2-i1+1)
       
       IF ((nbstep3d == 0).AND.(iif == 1)) THEN
       IF (.NOT.Associated(finevalues)) THEN
       Allocate(finevalues(isize,0:nfast,1))
       Allocate(coarsevalues(isize,0:nfast,1))
       ELSE       
       CALL checksize(indupdate+isize,nfast)
       ENDIF
       ENDIF
       
          do iter=1,ibegin
          oldindupdate = indupdate
          do j=j1,j2  
          do i=i1,i2
          oldindupdate = oldindupdate + 1
          finevalues(oldindupdate,iiffine-iter+1,nbgrid)
     &         = tabres(i,j,iter)
          enddo
          enddo          
          enddo
          
          IF (iif == 1) THEN
          oldindupdate = indupdate
          do j=j1,j2
          do i=i1,i2
          oldindupdate = oldindupdate + 1
          coarsevalues(oldindupdate,0,nbgrid) = DV_avg1(i,j,nstp)
          enddo
          enddo        
          ENDIF   
          
          
         tabtemp = 0.
         do iter=0,iif-1
         cff = -weight2(iif,iter)
         call copy1d(tabtemp,coarsevalues(indupdate+1,iter,nbgrid),
     &               cff,isize)
         enddo    
         
         do iter=0,iiffine
         cff=weight2(iiffine,iter)
         call copy1d(tabtemp,finevalues(indupdate+1,iter,nbgrid),
     &               cff,isize)
         enddo
         
         tabtemp = tabtemp/weight2(iif,iif)
                               
         DO j=j1,j2
           DO i=i1,i2
	   t1 = 0.5*(zeta(i,j,knew)+h(i,j)+
     &  zeta(i,j-1,knew)+h(i,j-1))*om_v(i,j)
            
           t2 = tabtemp(i,j)
#ifdef MASKING
     &           * vmask(i,j)
#endif
          vbar(i,j,knew) = t2/t1
         indupdate = indupdate + 1
          coarsevalues(indupdate,iif,nbgrid) = tabtemp(i,j)
              
          ENDDO
         ENDDO 
       ENDIF
      
      return
      end 
            
      Subroutine Updateduavg2(tabres,i1,i2,j1,j2,before)
      implicit none
#  include "param.h"
#  include "grid.h"
#  include "ocean2d.h"
#  include "coupling.h"
#  include "scalars.h"
      integer i1,i2,j1,j2
      real tabres(i1:i2,j1:j2)
      logical before
      real :: rrhoy
                    
      IF (before) THEN
      
         rrhoy = real(Agrif_Irhoy())
	   
         tabres(i1:i2,j1:j2) = rrhoy * DU_avg1(i1:i2,j1:j2,4) 
         
       ELSE
          
          DU_avg2(i1:i2,j1:j2) = tabres(i1:i2,j1:j2)
#ifdef MASKING
     &                * umask(i1:i2,j1:j2)
#endif

       ENDIF
      
      return
      end      

      Subroutine Updatedvavg2(tabres,i1,i2,j1,j2,before)
      implicit none
#  include "param.h"
#  include "grid.h"
#  include "ocean2d.h"
#  include "coupling.h"
#  include "scalars.h"
#  include "coupling.h"
      integer i1,i2,j1,j2
      real tabres(i1:i2,j1:j2)
      logical before
      real :: rrhox
                    
      IF (before) THEN
      
         rrhox = real(Agrif_Irhox())
	   
         tabres(i1:i2,j1:j2) = rrhox * DV_avg1(i1:i2,j1:j2,4) 
         
       ELSE
          
          DV_avg2(i1:i2,j1:j2) = tabres(i1:i2,j1:j2)
#ifdef MASKING
     &                * vmask(i1:i2,j1:j2)
#endif

       ENDIF
             
      return
      end
            

      subroutine checksize(isize,nfast)
      integer :: isize,nfast
      real,dimension(:,:,:),allocatable :: tempvalues
      integer :: n1,n3
!$AGRIF_DO_NOT_TREAT
      logical global_update_2d
      common/globalupdate/global_update_2d
      real,dimension(:,:,:),pointer :: finevalues
      real,dimension(:,:,:),pointer :: coarsevalues
      integer :: iiffine, irhotfine, nbgrid
      common/updateubar2val/finevalues,coarsevalues, iiffine,irhotfine,
     &  nbgrid
!$AGRIF_END_DO_NOT_TREAT

       IF (size(finevalues,1).LT.(isize)) THEN
       n1 = size(finevalues,1)
       n3 = size(finevalues,3)
       allocate(tempvalues(n1,0:nfast,n3))
       tempvalues=finevalues(1:n1,0:nfast,1:n3)
       deallocate(finevalues)
       allocate(finevalues(isize,0:nfast,n3))
       finevalues(1:n1,0:nfast,1:n3) = tempvalues
       
       tempvalues=coarsevalues(1:n1,0:nfast,1:n3)
       deallocate(coarsevalues)
       allocate(coarsevalues(isize,0:nfast,n3))
       coarsevalues(1:n1,0:nfast,1:n3) = tempvalues
              
       deallocate(tempvalues)
       ELSE IF (nbgrid.GT.size(finevalues,3)) THEN
       n1 = size(finevalues,1)
       n3 = size(finevalues,3)
       allocate(tempvalues(n1,0:nfast,n3))
       tempvalues=finevalues(1:n1,0:nfast,1:n3)
       deallocate(finevalues)
       allocate(finevalues(n1,0:nfast,nbgrid))
       finevalues(1:n1,0:nfast,1:n3) = tempvalues
       
       tempvalues=coarsevalues(1:n1,0:nfast,1:n3)
       deallocate(coarsevalues)
       allocate(coarsevalues(n1,0:nfast,nbgrid))
       coarsevalues(1:n1,0:nfast,1:n3) = tempvalues
              
       deallocate(tempvalues)       
       ENDIF      
      return
      end
#else
        subroutine update2D_empty
        return
        end
#endif
