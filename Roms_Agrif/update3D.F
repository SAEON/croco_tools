#include "cppdefs.h"
!====================================================================
!                   subroutine Agrif_update
!====================================================================
!
#if defined AGRIF && defined SOLVE3D && defined AGRIF_2WAY

      subroutine Agrif_update_np1pre
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
      integer NNO,NNOX,NNOY,ich,jch,k,itrc,ipr,jpr,nnewpr
      integer irmin,irmax,jrmin,jrmax,iumin,iumax,jvmin,jvmax
      integer ii,jj,iint,jint
      real eps,cff,fxc,surfc 
      parameter (eps=1.E-20)
      real tabtemp(GLOBAL_2D_ARRAY,N,NT)
      External UpdateTranp1pre
      External Updateunp1pre
      External Updatevnp1pre
      real,dimension(:,:,:,:,:),pointer ::tparent
      integer :: i,j
!$AGRIF_DO_NOT_TREAT
      integer :: nbcoarsechild
      common/updateprestepint/nbcoarsechild      
!$AGRIF_END_DO_NOT_TREAT
      
      
!
       return

      nbcoarsechild = nbcoarse

      Call Agrif_ChildGrid_To_ParentGrid()
      Call ResetAlready()
      Call Agrif_ParentGrid_To_ChildGrid()
     
#ifdef MASKING
      Agrif_UseSpecialValueInUpdate = .TRUE.
#endif
      Agrif_SpecialValueFineGrid = 0.
            

      
      Call Agrif_Update_Variable(tabtemp,updateTprof,
     &         locupdate=(/0,2/),procname = updateTranp1pre)  

      Agrif_UseSpecialValueInUpdate = .FALSE.
           
      Call Agrif_Update_Variable(tabtemp(:,:,:,1),Huon,
     &         locupdate=(/0,2/),procname = updateunp1pre)

      Call Agrif_Update_Variable(tabtemp(:,:,:,1),Hvom,
     &         locupdate=(/0,2/),procname = updatevnp1pre) 
      
      return
      end  
      
      subroutine ResetAlready()
#include "zoom.h"
      Alreadyupdated = .FALSE.
      end subroutine ResetAlready    

# define UPDATE_UV
      subroutine Agrif_update_np1
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
#  include "coupling.h"
#  include "zoom.h"
      integer NNO,NNOX,NNOY,ich,jch,k,itrc,ipr,jpr,nnewpr
      integer irmin,irmax,jrmin,jrmax,iumin,iumax,jvmin,jvmax
      integer ii,jj,iint,jint
      real eps,cff,fxc,surfc 
      parameter (eps=1.E-20)
      real tabtemp(GLOBAL_2D_ARRAY,N,NT)
      External UpdateTranp1, Updateunp1, Updatevnp1
      real,dimension(:,:,:,:), pointer :: myfxparent,myfyparent,uparent
      real,dimension(:,:,:,:), pointer :: vparent
      real,dimension(:,:),pointer :: pmparent,pnparent,rmaskparent
      real,dimension(:,:,:),pointer :: Hzparent
      real,dimension(:,:,:,:,:),pointer ::tparent
      integer ipu,jpu,ipv,jpv
      real HTEMP(GLOBAL_2D_ARRAY,N)
      integer j,i
      real t1,t2,t3,t4,t5
      real dtparent
      integer nnewparent,parentnnew
      real tind(5)
      integer j1
      integer irhot
!

!      return

#ifdef MASKING
      Agrif_UseSpecialValueInUpdate = .TRUE.
#endif
      Agrif_SpecialValueFineGrid = 0.
      
      irhot=Agrif_Irhot()
      IF (mod(nbstep3d,5) == (irhot-1)) THEN
      Call Agrif_Update_Variable(tabtemp,updateTprof,
     &         procname = updateTranp1)
     
      Agrif_UseSpecialValueInUpdate = .FALSE.
           
      Call Agrif_Update_Variable(tabtemp(:,:,:,1),Huon,
     &         procname = updateunp1)
      Call Agrif_Update_Variable(tabtemp(:,:,:,1),Hvom,
     &         procname = updatevnp1)
      ELSE
      Call Agrif_Update_Variable(tabtemp,updateTprof,
     &         locupdate=(/0,2/),procname = updateTranp1)
     
      Agrif_UseSpecialValueInUpdate = .FALSE.
           
      Call Agrif_Update_Variable(tabtemp(:,:,:,1),Huon,
     &         locupdate=(/0,2/),procname = updateunp1)
      Call Agrif_Update_Variable(tabtemp(:,:,:,1),Hvom,
     &         locupdate=(/0,2/),procname = updatevnp1)     
      ENDIF

       RETURN
      
      tparent=>
     &Agrif_Parent(t)
      uparent=>
     &Agrif_Parent(u) 
      vparent=>
     &Agrif_Parent(v)          
      parentnnew=
     &Agrif_Parent(nnew)
      Hzparent=>
     &Agrif_Parent(Hz)
#ifdef MASKING
      rmaskparent=>
     &Agrif_Parent(rmask)
#endif          
     
 

        
       myfxparent=>
     &Agrif_Parent(myfx)
       myfyparent=>
     &Agrif_Parent(myfy)
      pmparent=>
     &Agrif_Parent(pm)
      pnparent=>
     &Agrif_Parent(pn)
      Hzparent=>
     &Agrif_Parent(Hz) 
      tparent=>
     &Agrif_Parent(t)    
      dtparent=
     &Agrif_Parent(dt)
      nnewparent=
     &Agrif_Parent(nnew)
               
      i=1
      ipu=Agrif_ix()+1-1+(i-1)/3
      
      do itrc=1,NT
      do k=1,N
      do j=2,Mm-1,3
      jpu=Agrif_iy()+1-1+(j-2)/3
      t1=myfxparent(ipu,jpu,k,itrc)
      
      t3 = sum(myfx(i,j-1:j+1,k,itrc))
      
#define CONSERVTRA      
#ifdef CONSERVTRA    
!      print *,ipu,jpu,t3,t1        
      tparent(ipu-1,jpu,k,nnewparent,itrc)=
     & tparent(ipu-1,jpu,k,nnewparent,itrc)-
     & (t3-t1)*dtparent*
     &  pmparent(ipu-1,jpu)*pnparent(ipu-1,jpu)/Hzparent(ipu-1,jpu,k)
#endif
      enddo
      enddo
      enddo
      
      i=Lm+1
      ipu=Agrif_ix()+1-1+(i-1)/3
      
      do itrc=1,NT
      do k=1,N
      do j=2,Mm-1,3
      jpu=Agrif_iy()+1-1+(j-2)/3
      t1=myfxparent(ipu,jpu,k,itrc)
      
      t3 = sum(myfx(i,j-1:j+1,k,itrc))
#ifdef CONSERVTRA 
!      print *,ipu,jpu,t3,t1           
      tparent(ipu,jpu,k,nnewparent,itrc)=
     & tparent(ipu,jpu,k,nnewparent,itrc)+
     & (t3-t1)*dtparent*
     &  pmparent(ipu,jpu)*pnparent(ipu,jpu)/Hzparent(ipu,jpu,k)
#endif     
      enddo
      enddo
      enddo      
      
      j=1
      jpv=Agrif_iy()+1-1+(j-1)/3
      
      do itrc=1,NT
      do k=1,N
      do i=2,Lm-1,3
      ipv=Agrif_ix()+1-1+(i-2)/3
      t1=myfyparent(ipv,jpv,k,itrc)
      
      t3 = sum(myfy(i-1:i+1,j,k,itrc))
#ifdef CONSERVTRA        
      t4 = tparent(ipv,jpv-1,k,nnewparent,itrc)
      tparent(ipv,jpv-1,k,nnewparent,itrc)=
     & tparent(ipv,jpv-1,k,nnewparent,itrc)-
     & (t3-t1)*dtparent*
     &  pmparent(ipv,jpv-1)*pnparent(ipv,jpv-1)/Hzparent(ipv,jpv-1,k)
      
#endif     
      enddo
      enddo
      enddo

      j=Mm+1
      jpv=Agrif_iy()+1-1+(j-1)/3
      
      do itrc=1,NT
      do k=1,N
      do i=2,Lm-1,3
      ipv=Agrif_ix()+1-1+(i-2)/3
      t1=myfyparent(ipv,jpv,k,itrc)
      
      t3 = sum(myfy(i-1:i+1,j,k,itrc))

#ifdef CONSERVTRA 
!      print *,ipv,jpv,t3,t1            
      tparent(ipv,jpv,k,nnewparent,itrc)=
     & tparent(ipv,jpv,k,nnewparent,itrc)+
     & (t3-t1)*dtparent*
     &  pmparent(ipv,jpv)*pnparent(ipv,jpv)/Hzparent(ipv,jpv,k)
#endif  
      enddo
      enddo
      enddo
                  
      myfx = 0.
      myfy = 0.
      
      return
      end
      
      Subroutine UpdateTranp1pre(tabres,i1,i2,j1,j2,k1,k2,l1,l2,before)
      implicit none
#  include "param.h"
#  include "ocean3d.h"
#  include "scalars.h"
#  include "zoom.h"
!$AGRIF_DO_NOT_TREAT
      integer :: nbcoarsechild
      common/updateprestepint/nbcoarsechild
!$AGRIF_END_DO_NOT_TREAT
      integer i1,i2,j1,j2,k1,k2,l1,l2
      real tabres(i1:i2,j1:j2,k1:k2,l1:l2)
      logical before
      
      integer i,j,k,l
      real :: invrrhot
      
      IF (before) THEN
      
        invrrhot = 1./real(Agrif_Irhot())
        DO l=l1,l2
        DO k=k1,k2
        DO j=j1,j2
        DO i=i1,i2
          tabres(i,j,k,l) = invrrhot * t(i,j,k,nrhs,l)
#ifdef MASKING
     &                       *rmask(i,j)
#endif          
        ENDDO
        ENDDO
        ENDDO
        ENDDO
       
       ELSE
       
         IF (nbcoarsechild == 1) THEN
           DO l=l1,l2
           DO k=k1,k2
           DO j=j1,j2
           DO i=i1,i2
             t(i,j,k,nrhs,l) = tabres(i,j,k,l)
#ifdef MASKING             
     &                         * rmask(i,j)
#endif
           ENDDO
           ENDDO
           ENDDO 
           ENDDO
        
         ELSE
       

           DO j=j1,j2
           DO i=i1,i2
             IF (.Not.Alreadyupdated(i,j,1)) THEN
               DO l=l1,l2
               DO k=k1,k2             
                 t(i,j,k,nrhs,l) = (t(i,j,k,nrhs,l)+tabres(i,j,k,l))
#ifdef MASKING             
     &                              * rmask(i,j)
#endif    
               ENDDO
               ENDDO 
               Alreadyupdated(i,j,1) = .TRUE.
             ENDIF
           ENDDO 
           ENDDO 
        
          ENDIF        
        ENDIF
	
      return
      end 
    
      
      Subroutine Updateunp1pre(tabres,i1,i2,j1,j2,k1,k2,before)
      implicit none
#  include "param.h"
#  include "ocean3d.h"
#  include "grid.h"
#  include "scalars.h"
#  include "zoom.h"
      integer i1,i2,j1,j2,k1,k2
      real tabres(i1:i2,j1:j2,k1:k2)
      logical before
      real :: hzu
!$AGRIF_DO_NOT_TREAT
      integer :: nbcoarsechild
      common/updateprestepint/nbcoarsechild
!$AGRIF_END_DO_NOT_TREAT      
      
      integer i,j,k
      real :: invrrhot

      IF (before) THEN
      
        invrrhot = 1./real(Agrif_Irhot())
        DO k=k1,k2
        DO j=j1,j2
        DO i=i1,i2
          tabres(i,j,k) = invrrhot * u(i,j,k,nrhs)
#ifdef MASKING
     &                       *umask(i,j)
#endif          
        ENDDO
        ENDDO
        ENDDO
       
       ELSE
       
         IF (nbcoarsechild == 1) THEN
           DO k=k1,k2
           DO j=j1,j2
           DO i=i1,i2
             u(i,j,k,nrhs) = tabres(i,j,k)
#ifdef MASKING             
     &                         * umask(i,j)
#endif
           ENDDO
           ENDDO
           ENDDO 
        
         ELSE
       

           DO j=j1,j2
           DO i=i1,i2
             IF (.Not.Alreadyupdated(i,j,2)) THEN
               DO k=k1,k2             
                 u(i,j,k,nrhs) = (u(i,j,k,nrhs)+tabres(i,j,k))
#ifdef MASKING             
     &                              * umask(i,j)
#endif    
               ENDDO
               Alreadyupdated(i,j,2) = .TRUE.
             ENDIF
           ENDDO 
           ENDDO 
        
          ENDIF        
        ENDIF
              
      return
      end

      
      
      Subroutine Updatevnp1pre(tabres,i1,i2,j1,j2,k1,k2,before)
      implicit none
#  include "param.h"
#  include "ocean3d.h"
#  include "grid.h"
#  include "scalars.h"
#  include "zoom.h"
      integer i1,i2,j1,j2,k1,k2
      real tabres(i1:i2,j1:j2,k1:k2)
      logical before
      real :: hzu
!$AGRIF_DO_NOT_TREAT
      integer :: nbcoarsechild
      common/updateprestepint/nbcoarsechild
!$AGRIF_END_DO_NOT_TREAT      
      
      integer i,j,k
      
      real :: invrrhot

      IF (before) THEN
      
        invrrhot = 1./real(Agrif_Irhot())
        DO k=k1,k2
        DO j=j1,j2
        DO i=i1,i2
          tabres(i,j,k) = invrrhot * v(i,j,k,nrhs)
#ifdef MASKING
     &                       *vmask(i,j)
#endif          
        ENDDO
        ENDDO
        ENDDO
       
       ELSE
       
         IF (nbcoarsechild == 1) THEN
           DO k=k1,k2
           DO j=j1,j2
           DO i=i1,i2
             v(i,j,k,nrhs) = tabres(i,j,k)
#ifdef MASKING             
     &                         * vmask(i,j)
#endif
           ENDDO
           ENDDO
           ENDDO 
        
         ELSE
       

           DO j=j1,j2
           DO i=i1,i2
             IF (.Not.Alreadyupdated(i,j,3)) THEN
               DO k=k1,k2             
                 v(i,j,k,nrhs) = (v(i,j,k,nrhs)+tabres(i,j,k))
#ifdef MASKING
     &                              * vmask(i,j)
#endif    
               ENDDO
               Alreadyupdated(i,j,3) = .TRUE.
             ENDIF
           ENDDO 
           ENDDO 
        
          ENDIF        
        ENDIF
      
      return
      end      

                  
      Subroutine UpdateTranp1(tabres,i1,i2,j1,j2,k1,k2,l1,l2,before)
      implicit none
#  include "param.h"
#  include "ocean3d.h"
#  include "scalars.h"
      integer i1,i2,j1,j2,k1,k2,l1,l2
      real tabres(i1:i2,j1:j2,k1:k2,l1:l2)
      logical before
      
      integer i,j,k,l

!      print *,'je rentre ',before
            
      IF (before) THEN
      DO l=l1,l2
        DO k=k1,k2
         DO j=j1,j2
           DO i=i1,i2
          !  tabres(i,j,k,l) = Hz(i,j,k)*t(i,j,k,nnew,l)
	    tabres(i,j,k,l) = t(i,j,k,nnew,l)
            ENDDO
         ENDDO
        ENDDO
       ENDDO
       ELSE
      DO l=l1,l2
        DO k=k1,k2
         DO j=j1,j2
          DO i=i1,i2
!           IF (Hz(i,j,k).NE.0.) THEN
!              t(i,j,k,nnew,l) = tabres(i,j,k,l)/Hz(i,j,k)
!           ENDIF
	      t(i,j,k,nnew,l) = tabres(i,j,k,l)
#ifdef MASKING
     &                         *rmask(i,j)
#endif          
          ENDDO
         ENDDO
       ENDDO 
        ENDDO

       ENDIF
      return
      end
 
      
      Subroutine Updateunp1(tabres,i1,i2,j1,j2,k1,k2,before)
      implicit none
#  include "param.h"
#  include "ocean3d.h"
#  include "grid.h"
#  include "scalars.h"
      integer i1,i2,j1,j2,k1,k2
      real tabres(i1:i2,j1:j2,k1:k2)
      logical before
      real :: hzu
      
      integer i,j,k
      
      IF (before) THEN
        DO k=k1,k2
         DO j=j1,j2
           DO i=max(lbound(Hz,1)+1,i1),i2
!            tabres(i,j,k) = 0.5*(Hz(i,j,k)+Hz(i-1,j,k))
!     &                     *on_u(i,j)*u(i,j,k,nnew)
            tabres(i,j,k) = u(i,j,k,nnew)
            ENDDO
         ENDDO
        ENDDO
       ELSE
        DO k=k1,k2
         DO j=j1,j2
           DO i=max(lbound(Hz,1)+1,i1),i2
!          hzu = 0.5*(Hz(i,j,k)+Hz(i-1,j,k))*on_u(i,j)
!          IF (hzu .NE.0.) THEN
!              u(i,j,k,nnew) = 3.*tabres(i,j,k)/hzu
!              u(i,j,k,nnew) = u(i,j,k,nnew)*umask(i,j)
!          ENDIF
          u(i,j,k,nnew) = tabres(i,j,k)
#ifdef MASKING
     &           * umask(i,j)
#endif     
          ENDDO
         ENDDO
       ENDDO        
       ENDIF
      
      return
      end
               
      
      Subroutine Updatevnp1(tabres,i1,i2,j1,j2,k1,k2,before)
      implicit none
#  include "param.h"
#  include "ocean3d.h"
#  include "grid.h"
#  include "scalars.h"
      integer i1,i2,j1,j2,k1,k2
      real tabres(i1:i2,j1:j2,k1:k2)
      logical before
      real hzv
      
      integer i,j,k
      
      IF (before) THEN
        DO k=k1,k2
         DO j=max(lbound(Hz,2)+1,j1),j2
           DO i=i1,i2
!            tabres(i,j,k) = 0.5*(Hz(i,j,k)+Hz(i,j-1,k))
!     &                     *om_v(i,j)*v(i,j,k,nnew)
             tabres(i,j,k) = v(i,j,k,nnew)
            ENDDO
         ENDDO
        ENDDO
       ELSE
        DO k=k1,k2
         DO j=max(lbound(Hz,2)+1,j1),j2
          DO i=i1,i2
!          hzv = 0.5*(Hz(i,j,k)+Hz(i,j-1,k))*om_v(i,j)
!          IF (hzv .NE.0.) THEN
!              v(i,j,k,nnew) = 3.*tabres(i,j,k)/hzv
!              v(i,j,k,nnew) = v(i,j,k,nnew)*vmask(i,j)
!          ENDIF
          v(i,j,k,nnew) = tabres(i,j,k)
#ifdef MASKING
     &              * vmask(i,j)
#endif     
          ENDDO
         ENDDO
       ENDDO        
       ENDIF
      
      return
      end
#else
        subroutine update3D_empty
        return
        end
#endif
