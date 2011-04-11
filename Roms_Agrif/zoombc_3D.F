! $Id$
!
!======================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
! 
! This routine belongs to the specific ROMS_AGRIF package.
! 
! ROMS_AGRIF website : http://roms.mpl.ird.fr
!======================================================================
!
#include "cppdefs.h"
!====================================================================
!                   subroutine Agrif_u3dbc_interp_tile
!====================================================================
!
#ifdef AGRIF
# if defined SOLVE3D
      subroutine u3dbc_interp_tile(Istr,Iend,Jstr,Jend)
      use AGRIF_Util
      implicit none
#  include "param.h"
#  include "boundary.h"  
#  include "climat.h"
#  include "grid.h"
#  include "scalars.h"
#  include "zoom.h"
#  include "ocean3d.h"
      integer Istr,Iend,Jstr,Jend, i,j,k
      real, dimension(:,:,:), allocatable :: uext
      integer itrcind
      real tinterp
      INTEGER :: nbstep3dparent
      External :: u3dinterp
      real :: ainterp(6)
      integer :: irhot
      real :: rrhot
      integer :: nsource, ndest
      integer :: parentnbstep
      
      real :: gamma
!
#  include "compute_auxiliary_bounds.h"
!
#  ifdef MPI
#   define LOCALLM Lmmpi
#   define LOCALMM Mmmpi
#  else
#   define LOCALLM Lm
#   define LOCALMM Mm
#  endif
!       return
      irhot=Agrif_Irhot()
      rrhot=real(irhot)

C$OMP BARRIER
C$OMP MASTER

      parentnbstep=Agrif_Parent_Nb_Step()

       IF ((((nbcoarse == 1).AND.(nnew == 3)).OR.
     &   ((nbcoarse == irhot).AND.(nnew /=3))).AND.
     &    (UTimeindex.NE.parentnbstep)) THEN

        Allocate(uext(i1u:i2u,j1t:j2t,N))
        tinterp=1. 
#ifdef MASKING      
        Agrif_UseSpecialvalue=.true.
#endif        
        Agrif_Specialvalue=0.
        uext=0.   
        Call Agrif_Set_bc(uid,(/0,0/),InterpolationShouldbemade=.TRUE.)
        Call Agrif_Bc_variable(uext,uid,calledweight=tinterp,
     &procname=u3dinterp)
        Agrif_UseSpecialValue=.false.

        IF (nnew == 3) THEN
        nsource = 2
        ELSE
        nsource = 4
        ENDIF
        ndest = nsource - 1
             
#  ifdef AGRIF_OBC_SOUTH
#   ifdef MPI        
            if (.not.SOUTH_INTER) then
#   endif
                U_south(0:LOCALLM+1,0:0,1:N,ndest)=
     &          U_south(0:LOCALLM+1,0:0,1:N,nsource)
     
                U_south(0:LOCALLM+1,0:0,1:N,nsource)=
     &             uext(0:LOCALLM+1,0:0,1:N)
          
#   ifdef MPI           
            endif          
#   endif            
#  endif            
#  ifdef AGRIF_OBC_NORTH
#   ifdef MPI        
            if (.not.NORTH_INTER) then  
#   endif
                U_north(0:LOCALLM+1,LOCALMM+1:LOCALMM+1,1:N,ndest)=
     &          U_north(0:LOCALLM+1,LOCALMM+1:LOCALMM+1,1:N,nsource)
     
                U_north(0:LOCALLM+1,LOCALMM+1:LOCALMM+1,1:N,nsource)=
     &             uext(0:LOCALLM+1,LOCALMM+1:LOCALMM+1,1:N)
          
#   ifdef MPI                    
            endif  
#   endif            
#  endif            
#  ifdef AGRIF_OBC_WEST
#   ifdef MPI        
            if (.not.WEST_INTER) then
#   endif
                U_west(1:1,0:LOCALMM+1,1:N,ndest)=
     &          U_west(1:1,0:LOCALMM+1,1:N,nsource)
     
                U_west(1:1,0:LOCALMM+1,1:N,nsource)=
     &            uext(1:1,0:LOCALMM+1,1:N)
          
#   ifdef MPI            
            endif
#   endif            
#  endif            
#  ifdef AGRIF_OBC_EAST
#   ifdef MPI        
            if (.not.EAST_INTER) then
#   endif
                U_east(LOCALLM+1:LOCALLM+1,0:LOCALMM+1,1:N,ndest)=
     &          U_east(LOCALLM+1:LOCALLM+1,0:LOCALMM+1,1:N,nsource)

                U_east(LOCALLM+1:LOCALLM+1,0:LOCALMM+1,1:N,nsource)=
     &            uext(LOCALLM+1:LOCALLM+1,0:LOCALMM+1,1:N)     
#   ifdef MPI               
            endif  
#   endif                            
#  endif
        deallocate(uext)

        UTimeindex = parentnbstep

      endif
C$OMP END MASTER
C$OMP BARRIER
      
       nbstep3dparent=
     &Agrif_Parent(nbstep3d)
                    
       tinterp = real(nbcoarse)/rrhot
       IF (nnew == 3) tinterp = tinterp - 1./(2.*rrhot)

       gamma = 1./12.
       
       
       ainterp(3)=tinterp*4*(1+tinterp)
       ainterp(2)=-3.*(-1+tinterp+2*tinterp**2)
       ainterp(1)=tinterp*(-1+2*tinterp)
       
       ainterp = ainterp/3.

       ainterp(1) = 0.
       ainterp(2) = 1.-2*tinterp
       ainterp(3) = 2.*tinterp
       
       ainterp = 0.
       
       IF (nnew == 3) then  
         ainterp(1) = 0.5-tinterp
         ainterp(2) = 0.5+tinterp
       else 
         ainterp(3) = -tinterp
         ainterp(4) = 1.+tinterp
       endif
       
!       ainterp(1)=-2*tinterp+2*tinterp**2+4*tinterp**3
!       ainterp(2)=(2./3.)*tinterp+2*tinterp**2+(4./3.)*tinterp**3
!       ainterp(3)=(1./3.)*tinterp-(4./3.)*tinterp**3
!       ainterp(4)=1+tinterp-4.*tinterp**2-4.*tinterp**3
       
       IF (nbstep3dparent .LE. 1) THEN
       ainterp = 0.
       ainterp(2) = 1.
       ENDIF
       
       if (nbstep3d .LE. (Agrif_irhot()-1)) then
        ainterp = 0.
        ainterp(2) = 1.
       endif

      IF ((nbstep3dparent .NE. 0) .AND. (nnew ==3)) THEN
!      ainterp(1)=0.
!      ainterp(2)=-(1.-2*gamma)*(1./(-2.+4*gamma))*(-2.)/rrhot
!      ainterp(3)=-(1.-2*gamma)*(1./(-2.+4*gamma))*(1.-4*gamma)/rrhot
!      ainterp(4)=-(1.-2*gamma)*(1./(-2.+4*gamma))*(1.+4*gamma)/rrhot
!      ainterp(5)=(0.5-2.*gamma)
!      ainterp(6)=(0.5+2.*gamma)      
      ENDIF
      
      IF ((nbstep3dparent .NE. 0) .AND. (nnew /=3)) THEN
!      ainterp(1)=0.
!      ainterp(2)=0.
!      ainterp(3)=-1./rrhot
!      ainterp(4)=1./rrhot
!      ainterp(5)=0.
!      ainterp(6)=1.      
      ENDIF
                           
       IF ((nnew/=3).AND.(nbcoarse == irhot)) THEN
         ainterp    = 0.
         ainterp(4) = 1.
       ENDIF
              
#  ifdef AGRIF_OBC_SOUTH
      if (SOUTHERN_EDGE) then
        do k=1,N      
          do i=Istr,IendR
#   if defined M3_FRC_BRY     
            ubry_south(i,k)=
#   else
            uclm(i,Jstr-1,k)=
#   endif
     &  (ainterp(1)*U_south(i,Jstr-1,k,1)+
     &   ainterp(2)*U_south(i,Jstr-1,k,2)+
     &   ainterp(3)*U_south(i,Jstr-1,k,3)+
     &   ainterp(4)*U_south(i,Jstr-1,k,4)+
     &   ainterp(5)*u(i,Jstr-1,k,3-nstp)+
     &   ainterp(6)*u(i,Jstr-1,k,nstp))
#   ifdef MASKING     
     &    *umask(i,Jstr-1)
#   endif

          enddo
        enddo
      endif
      
      
#  endif
#  ifdef AGRIF_OBC_NORTH
      if (NORTHERN_EDGE) then      
        do k=1,N      
          do i=Istr,IendR
#   if defined M3_FRC_BRY     
            ubry_north(i,k)=
#   else   
            uclm(i,Jend+1,k)=
#   endif
     &  (ainterp(1)*U_north(i,Jend+1,k,1)+
     &   ainterp(2)*U_north(i,Jend+1,k,2)+
     &   ainterp(3)*U_north(i,Jend+1,k,3)+
     &   ainterp(4)*U_north(i,Jend+1,k,4)+
     &   ainterp(5)*u(i,Jend+1,k,3-nstp)+
     &   ainterp(6)*u(i,Jend+1,k,nstp))     
#   ifdef MASKING     
     &    *umask(i,Jend+1)
#   endif
          enddo
        enddo
      endif      
#  endif
 
#  ifdef AGRIF_OBC_WEST
      if (WESTERN_EDGE) then      
        do k=1,N      
          do j=JstrR,JendR
#   if defined M3_FRC_BRY     
            ubry_west(j,k)=
#   else     
            uclm(Istr,j,k)=
#   endif
     &  (ainterp(1)*U_west(Istr,j,k,1)+
     &   ainterp(2)*U_west(Istr,j,k,2)+
     &   ainterp(3)*U_west(Istr,j,k,3)+
     &   ainterp(4)*U_west(Istr,j,k,4)+
     &   ainterp(5)*u(Istr,j,k,3-nstp)+
     &   ainterp(6)*u(Istr,j,k,nstp))
#   ifdef MASKING     
     &    *umask(Istr,j)
#   endif
          enddo
        enddo
      endif      
#  endif
      
#  ifdef AGRIF_OBC_EAST
      if (EASTERN_EDGE) then      
        do k=1,N      
          do j=JstrR,JendR
#   if defined M3_FRC_BRY  
            ubry_east(j,k)=
#   else
            uclm(Iend+1,j,k)=
#   endif
     &  (ainterp(1)*U_east(Iend+1,j,k,1)+
     &   ainterp(2)*U_east(Iend+1,j,k,2)+
     &   ainterp(3)*U_east(Iend+1,j,k,3)+
     &   ainterp(4)*U_east(Iend+1,j,k,4)+
     &   ainterp(5)*u(Iend+1,j,k,3-nstp)+
     &   ainterp(6)*u(Iend+1,j,k,nstp))         
#   ifdef MASKING     
     &    *umask(Iend+1,j)
#   endif
          enddo
        enddo
      endif
#  endif
    
      return
      end
# endif /* SOLVE3D */
!

      subroutine u3Dinterp(tabres,i1,i2,j1,j2,k1,k2)
      implicit none
# include "param.h"
# include "ocean3d.h"
# include "scalars.h"

       integer i1,i2,j1,j2,k1,k2
       real tabres(i1:i2,j1:j2,k1:k2)
       integer nparent
       
       IF ((iif == 0)) THEN  
         nparent = 3
       ELSE
         nparent = nnew
       ENDIF
       
       tabres(i1:i2,j1:j2,k1:k2) = u(i1:i2,j1:j2,k1:k2,nparent)
             
      return
      end
      
      subroutine v3Dinterp(tabres,i1,i2,j1,j2,k1,k2)
      implicit none
# include "param.h"
# include "ocean3d.h"
# include "scalars.h"

       integer i1,i2,j1,j2,k1,k2
       real tabres(i1:i2,j1:j2,k1:k2)
       integer nparent       
       
       IF ((iif == 0)) THEN  
         nparent = 3
       ELSE
         nparent = nnew
       ENDIF
       
       tabres(i1:i2,j1:j2,k1:k2) = v(i1:i2,j1:j2,k1:k2,nparent)
             
      return
      end       
      
!====================================================================
!                   subroutine Agrif_v3dbc_interp_tile
!====================================================================
!
# if defined SOLVE3D
      subroutine v3dbc_interp_tile(Istr,Iend,Jstr,Jend)
      use AGRIF_Util
      implicit none
#  include "param.h"
#  include "boundary.h"  
#  include "climat.h"
#  include "grid.h"
#  include "scalars.h"
#  include "zoom.h"
#  include "ocean3d.h"
      integer Istr,Iend,Jstr,Jend, i,j,k
      real, dimension(:,:,:), allocatable :: vext
      integer itrcind
      real tinterp
      INTEGER :: nbstep3dparent
      external :: v3dinterp
      real ainterp(6)
      integer :: irhot
      real :: rrhot
      integer :: nsource, ndest
      real :: gamma
      integer :: parentnbstep
    
!
#  include "compute_auxiliary_bounds.h"
!
#  ifdef MPI
#   define LOCALLM Lmmpi
#   define LOCALMM Mmmpi
#  else
#   define LOCALLM Lm
#   define LOCALMM Mm
#  endif 
!      return
      irhot=Agrif_Irhot()
      rrhot=real(irhot) 

C$OMP BARRIER
C$OMP MASTER
      
      parentnbstep=Agrif_Parent_Nb_Step()
      
       IF ((((nbcoarse == 1).AND.(nnew == 3)).OR.
     &   ((nbcoarse == irhot).AND.(nnew /=3))).AND.
     &    (VTimeindex.NE.parentnbstep)) THEN 

        allocate(vext(i1t:i2t,j1v:j2v,N))
        tinterp=1.

#ifdef MASKING
        AGRIF_UseSpecialvalue=.true.
#endif        
        AGRIF_Specialvalue=0.
        vext=0.
        Call Agrif_Set_bc(vid,(/0,0/),InterpolationShouldbemade=.TRUE.)
        Call Agrif_Bc_variable(vext,vid,calledweight=tinterp,
     &procname=v3dinterp)     
        AGRIF_UseSpecialValue=.false.

        IF (nnew == 3) THEN
        nsource = 2
        ELSE
        nsource = 4
        ENDIF
        ndest = nsource - 1
        
#  ifdef AGRIF_OBC_SOUTH
#   ifdef MPI  
            if (.not.SOUTH_INTER) then
#   endif
                V_south(0:LOCALLM+1,1:1,1:N,ndest)=
     &          V_south(0:LOCALLM+1,1:1,1:N,nsource)
     
                V_south(0:LOCALLM+1,1:1,1:N,nsource)=
     &             vext(0:LOCALLM+1,1:1,1:N)
#   ifdef MPI           
            endif          
#   endif
#  endif
#  ifdef AGRIF_OBC_NORTH
#   ifdef MPI  
            if (.not.NORTH_INTER) then  
#   endif
                V_north(0:LOCALLM+1,LOCALMM+1:LOCALMM+1,1:N,ndest)=
     &          V_north(0:LOCALLM+1,LOCALMM+1:LOCALMM+1,1:N,nsource)
     
                V_north(0:LOCALLM+1,LOCALMM+1:LOCALMM+1,1:N,nsource)=
     &             vext(0:LOCALLM+1,LOCALMM+1:LOCALMM+1,1:N)
#   ifdef MPI                    
            endif  
#   endif
#  endif
#  ifdef AGRIF_OBC_WEST
#   ifdef MPI           
            if (.not.WEST_INTER) then
#   endif
                V_west(0:0,0:LOCALMM+1,1:N,ndest)=
     &          V_west(0:0,0:LOCALMM+1,1:N,nsource)
     
                V_west(0:0,0:LOCALMM+1,1:N,nsource)=
     &            vext(0:0,0:LOCALMM+1,1:N)
#   ifdef MPI            
            endif
#   endif
#  endif
#  ifdef AGRIF_OBC_EAST
#   ifdef MPI           
            if (.not.EAST_INTER) then
#   endif
                V_east(LOCALLM+1:LOCALLM+1,0:LOCALMM+1,1:N,ndest)=
     &          V_east(LOCALLM+1:LOCALLM+1,0:LOCALMM+1,1:N,nsource)
     
                V_east(LOCALLM+1:LOCALLM+1,0:LOCALMM+1,1:N,nsource)=
     &            vext(LOCALLM+1:LOCALLM+1,0:LOCALMM+1,1:N)
#   ifdef MPI               
            endif  
#   endif                               
#  endif
        deallocate(vext)

        VTimeindex=parentnbstep

      endif
C$OMP END MASTER
C$OMP BARRIER

       nbstep3dparent=
     &Agrif_Parent(nbstep3d)
               
       gamma = 1./12.
            
       tinterp = real(nbcoarse)/rrhot
       IF (nnew == 3) tinterp = tinterp - 1./(2.*rrhot)

       ainterp(3)=tinterp*4*(1+tinterp)
       ainterp(2)=-3.*(-1+tinterp+2*tinterp**2)
       ainterp(1)=tinterp*(-1+2*tinterp)
       
       ainterp = ainterp/3.

       ainterp(1) = 0.
       ainterp(2) = 1.-2*tinterp
       ainterp(3) = 2.*tinterp
       
       ainterp = 0.
       
       IF (nnew == 3) then  
         ainterp(1) = 0.5-tinterp
         ainterp(2) = 0.5+tinterp
       else 
         ainterp(3) = -tinterp
         ainterp(4) = 1.+tinterp
       endif

!       ainterp(1)=-2*tinterp+2*tinterp**2+4*tinterp**3
!       ainterp(2)=(2./3.)*tinterp+2*tinterp**2+(4./3.)*tinterp**3
!       ainterp(3)=(1./3.)*tinterp-(4./3.)*tinterp**3
!       ainterp(4)=1+tinterp-4.*tinterp**2-4.*tinterp**3
              
       IF (nbstep3dparent .LE. 1) THEN
       ainterp = 0.
       ainterp(2) = 1.
       ENDIF

       if (nbstep3d .LE. (Agrif_irhot()-1)) then
        ainterp = 0.
        ainterp(2) = 1.
       endif
       
      IF ((nbstep3dparent .NE. 0) .AND. (nnew ==3)) THEN
!      ainterp(1)=0.
!      ainterp(2)=-(1.-2*gamma)*(1./(-2.+4*gamma))*(-2.)/rrhot
!      ainterp(3)=-(1.-2*gamma)*(1./(-2.+4*gamma))*(1.-4*gamma)/rrhot
!      ainterp(4)=-(1.-2*gamma)*(1./(-2.+4*gamma))*(1.+4*gamma)/rrhot
!      ainterp(5)=(0.5-2.*gamma)
!      ainterp(6)=(0.5+2.*gamma)      
      ENDIF
      
      IF ((nbstep3dparent .NE. 0) .AND. (nnew /=3)) THEN
!      ainterp(1)=0.
!      ainterp(2)=0.
!      ainterp(3)=-1./rrhot
!      ainterp(4)=1./rrhot
!      ainterp(5)=0.
!      ainterp(6)=1.      
      ENDIF
                                 
       IF ((nnew/=3).AND.(nbcoarse == irhot)) THEN
         ainterp    = 0.
         ainterp(4) = 1.
       ENDIF
       

                  
#  ifdef AGRIF_OBC_SOUTH
      if (SOUTHERN_EDGE) then
        do k=1,N      
          do i=IstrR,IendR
#   if defined M3_FRC_BRY   
            vbry_south(i,k)=
#   else
            vclm(i,Jstr,k)=
#   endif
     &    (ainterp(1)*V_south(i,Jstr,k,1)+
     &     ainterp(2)*V_south(i,Jstr,k,2)+
     &     ainterp(3)*V_south(i,Jstr,k,3)+
     &     ainterp(4)*V_south(i,Jstr,k,4)+
     &     ainterp(5)*v(i,Jstr,k,3-nstp)+
     &     ainterp(6)*v(i,Jstr,k,nstp))
#   ifdef MASKING     
     &    *vmask(i,Jstr)
#   endif
          enddo
        enddo
      endif
#  endif
#  ifdef AGRIF_OBC_NORTH
      if (NORTHERN_EDGE) then      
        do k=1,N      
          do i=IstrR,IendR
#   if defined M3_FRC_BRY
            vbry_north(i,k)=
#   else
            vclm(i,Jend+1,k)=
#   endif
     &    (ainterp(1)*V_north(i,Jend+1,k,1)+
     &     ainterp(2)*V_north(i,Jend+1,k,2)+
     &     ainterp(3)*V_north(i,Jend+1,k,3)+
     &     ainterp(4)*V_north(i,Jend+1,k,4)+
     &     ainterp(5)*v(i,Jend+1,k,3-nstp)+
     &     ainterp(6)*v(i,Jend+1,k,nstp))     
#   ifdef MASKING     
     &    *vmask(i,Jend+1)
#   endif 
          enddo
        enddo
      endif      
#  endif
#  ifdef AGRIF_OBC_WEST
      if (WESTERN_EDGE) then      
        do k=1,N      
          do j=Jstr,JendR
#   if defined M3_FRC_BRY
            vbry_west(j,k)=
#   else
            vclm(Istr-1,j,k)=
#   endif
     &    (ainterp(1)*V_west(Istr-1,j,k,1)+
     &     ainterp(2)*V_west(Istr-1,j,k,2)+
     &     ainterp(3)*V_west(Istr-1,j,k,3)+
     &     ainterp(4)*V_west(Istr-1,j,k,4)+
     &     ainterp(5)*v(Istr-1,j,k,3-nstp)+
     &     ainterp(6)*v(Istr-1,j,k,nstp))     
#   ifdef MASKING     
     &    *vmask(Istr-1,j)
#   endif
          enddo
        enddo
      endif      
#  endif
#  ifdef AGRIF_OBC_EAST
      if (EASTERN_EDGE) then      
        do k=1,N      
          do j=Jstr,JendR
#   if defined M3_FRC_BRY  
            vbry_east(j,k)=
#   else
            vclm(Iend+1,j,k)=
#   endif
     &    (ainterp(1)*V_east(Iend+1,j,k,1)+
     &     ainterp(2)*V_east(Iend+1,j,k,2)+
     &     ainterp(3)*V_east(Iend+1,j,k,3)+
     &     ainterp(4)*V_east(Iend+1,j,k,4)+
     &     ainterp(5)*v(Iend+1,j,k,3-nstp)+
     &     ainterp(6)*v(Iend+1,j,k,nstp))      
#   ifdef MASKING     
     &    *vmask(Iend+1,j)
#   endif
          enddo
        enddo
      endif
#  endif    

       
      return
      end
# endif /* SOLVE 3D */
!
!====================================================================
!                   subroutine Agrif_t3dbc_interp_tile
!====================================================================
!
# if defined SOLVE3D
      subroutine t3dbc_interp_tile(Istr,Iend,Jstr,Jend,indx,itrc)
      use AGRIF_Util
      implicit none
#  include "param.h"
#  include "boundary.h"  
#  include "climat.h"
#  include "grid.h"
#  include "scalars.h"
#  include "zoom.h"
#  include "ocean3d.h"
      integer Istr,Iend,Jstr,Jend,itrc,i,j,k
      real,dimension(:,:,:,:),allocatable :: text
      integer itrcind, ntind, indx
      real tinterp
      INTEGER :: nbstep3dparent
      external t3dinterp
      real ainterp(4)
      integer :: irhot
      real :: rrhot
      integer :: nsource, ndest
      integer :: parentnbstep

!
#  include "compute_auxiliary_bounds.h"
!
#  ifdef MPI
#   define LOCALLM Lmmpi
#   define LOCALMM Mmmpi
#  else
#   define LOCALLM Lm
#   define LOCALMM Mm
#  endif   
!      return
      irhot = Agrif_Irhot()
      rrhot = real(irhot) 

       nbstep3dparent=
     &Agrif_Parent(nbstep3d)

C$OMP BARRIER
C$OMP MASTER

      parentnbstep=Agrif_Parent_Nb_Step()

       IF ((((nbcoarse == 1).AND.(nnew == 3)).OR.
     &   ((nbcoarse == irhot).AND.(nnew /=3))).AND.
     &    (TTimeindex.NE.parentnbstep)) THEN 

        allocate(text(i1t:i2t,j1t:j2t,N,NT))
        tinterp=1.
#ifdef MASKING        
        Agrif_UseSpecialvalue=.true.
#endif        
        Agrif_Specialvalue=0.
        text=0. 
        Call Agrif_Set_bc(tid,(/-1,0/),
     &     InterpolationShouldbemade=.TRUE.)        
        Call Agrif_Bc_variable(text,tid,calledweight=tinterp,
     & procname = t3dinterp)
      
        Agrif_UseSpecialValue=.false.

        IF (nnew == 3) THEN
        nsource = 2
        ELSE
        nsource = 4
        ENDIF
        ndest = nsource - 1
        
#  ifdef AGRIF_OBC_SOUTH
#   ifdef MPI        
              if (.not.SOUTH_INTER) Then
#   endif           
                T_south(0:LOCALLM+1,0:1,1:N,ndest,1:NT)=
     &          T_south(0:LOCALLM+1,0:1,1:N,nsource,1:NT)
     
                T_south(0:LOCALLM+1,0:1,1:N,nsource,1:NT)=
     &              text(0:LOCALLM+1,0:1,1:N,1:NT)
          
#   ifdef MPI           
              endif          
#   endif               
#  endif               
#  ifdef AGRIF_OBC_NORTH
#   ifdef MPI                    
              if (.not.NORTH_INTER) then  
#   endif 
                T_north(0:LOCALLM+1,LOCALMM:LOCALMM+1,1:N,ndest,1:NT)=
     &          T_north(0:LOCALLM+1,LOCALMM:LOCALMM+1,1:N,nsource,1:NT)
     
                T_north(0:LOCALLM+1,LOCALMM:LOCALMM+1,1:N,nsource,1:NT)=
     &              text(0:LOCALLM+1,LOCALMM:LOCALMM+1,1:N,1:NT)
          
#   ifdef MPI                    
              endif  
#   endif               
#  endif               
#  ifdef AGRIF_OBC_WEST
#   ifdef MPI                    
              if (.not.WEST_INTER) then
#   endif               
                  T_west(0:1,0:LOCALMM+1,1:N,ndest,1:NT)=
     &            T_west(0:1,0:LOCALMM+1,1:N,nsource,1:NT)
     
                  T_west(0:1,0:LOCALMM+1,1:N,nsource,1:NT)=
     &                text(0:1,0:LOCALMM+1,1:N,1:NT)     
#   ifdef MPI            
              endif
#   endif               
#  endif               
#  ifdef AGRIF_OBC_EAST
#   ifdef MPI                    
              if (.not.EAST_INTER) then
#   endif
                  T_east(LOCALLM:LOCALLM+1,0:LOCALMM+1,1:N,ndest,1:NT)=
     &            T_east(LOCALLM:LOCALLM+1,0:LOCALMM+1,1:N,nsource,1:NT)
     
                  T_east(LOCALLM:LOCALLM+1,0:LOCALMM+1,1:N,nsource,1:NT)=
     &                text(LOCALLM:LOCALLM+1,0:LOCALMM+1,1:N,1:NT)     
#   ifdef MPI               
              endif  
#   endif                           
#  endif
        
        deallocate(text)

         TTimeindex=parentnbstep 

      endif
C$OMP END MASTER
C$OMP BARRIER
                    
       tinterp = real(nbcoarse)/rrhot
       IF (nnew == 3) tinterp = tinterp - 1./(2.*rrhot)

       ainterp(3)=tinterp*4*(1+tinterp)
       ainterp(2)=-3.*(-1+tinterp+2*tinterp**2)
       ainterp(1)=tinterp*(-1+2*tinterp)
       
       ainterp = ainterp/3.

       ainterp(1) = 0.
       ainterp(2) = 1.-2*tinterp
       ainterp(3) = 2.*tinterp
       
       ainterp = 0.
       
       IF (nnew == 3) then  
         ainterp(1) = 0.5-tinterp
         ainterp(2) = 0.5+tinterp
       else 
         ainterp(3) = -tinterp
         ainterp(4) = 1.+tinterp
       endif
       
!       ainterp(1)=-2*tinterp+2*tinterp**2+4*tinterp**3
!       ainterp(2)=(2./3.)*tinterp+2*tinterp**2+(4./3.)*tinterp**3
!       ainterp(3)=(1./3.)*tinterp-(4./3.)*tinterp**3
!       ainterp(4)=1+tinterp-4.*tinterp**2-4.*tinterp**3
       
       IF (nbstep3dparent .LE. 1) THEN
       ainterp = 0.
       ainterp(2) = 1.
       ENDIF

       if (nbstep3d .LE. (Agrif_irhot()-1)) then
        ainterp = 0.
        ainterp(2) = 1.
       endif
                           
       IF ((nnew/=3).AND.(nbcoarse == irhot)) THEN
         ainterp    = 0.
         ainterp(4) = 1.
       ENDIF
!      
#  ifdef AGRIF_OBC_SOUTH
       if (SOUTHERN_EDGE) then
         do k=1,N      
           do i=IstrR,IendR
#   ifdef T_FRC_BRY
            tbry_south(i,k,itrc)=
#   else
            tclm(i,Jstr-1,k,itrc)=
#   endif
     &     (ainterp(1)*T_south(i,Jstr-1,k,1,itrc)+
     &      ainterp(2)*T_south(i,Jstr-1,k,2,itrc)+
     &      ainterp(3)*T_south(i,Jstr-1,k,3,itrc)+
     &      ainterp(4)*T_south(i,Jstr-1,k,4,itrc))
#   ifdef MASKING     
     &        *rmask(i,Jstr-1)
#   endif
          enddo
        enddo
      endif
#  endif
#  ifdef AGRIF_OBC_NORTH
      if (NORTHERN_EDGE) then      
        do k=1,N      
          do i=IstrR,IendR
#   ifdef T_FRC_BRY
            tbry_north(i,k,itrc)=
#   else
            tclm(i,Jend+1,k,itrc)=
#   endif
     &     (ainterp(1)*T_north(i,Jend+1,k,1,itrc)+
     &      ainterp(2)*T_north(i,Jend+1,k,2,itrc)+
     &      ainterp(3)*T_north(i,Jend+1,k,3,itrc)+
     &      ainterp(4)*T_north(i,Jend+1,k,4,itrc))     
#   ifdef MASKING     
     &          *rmask(i,Jend+1)
#   endif
          enddo
        enddo
      endif      
#  endif
#  ifdef AGRIF_OBC_WEST
      if (WESTERN_EDGE) then      
        do k=1,N      
          do j=JstrR,JendR
#   ifdef T_FRC_BRY
            tbry_west(j,k,itrc)=
#   else
            tclm(Istr-1,j,k,itrc)=
#   endif
     &     (ainterp(1)*T_west(Istr-1,j,k,1,itrc)+
     &      ainterp(2)*T_west(Istr-1,j,k,2,itrc)+
     &      ainterp(3)*T_west(Istr-1,j,k,3,itrc)+
     &      ainterp(4)*T_west(Istr-1,j,k,4,itrc))      
#   ifdef MASKING     
     &         *rmask(Istr-1,j)
#   endif
          enddo
        enddo
      endif      
#  endif
#  ifdef AGRIF_OBC_EAST
      if (EASTERN_EDGE) then      
        do k=1,N      
          do j=JstrR,JendR
#   ifdef T_FRC_BRY
            tbry_east(j,k,itrc)= 
#   else
            tclm(Iend+1,j,k,itrc)=
#   endif
     &     (ainterp(1)*T_east(Iend+1,j,k,1,itrc)+
     &      ainterp(2)*T_east(Iend+1,j,k,2,itrc)+
     &      ainterp(3)*T_east(Iend+1,j,k,3,itrc)+
     &      ainterp(4)*T_east(Iend+1,j,k,4,itrc))       
#   ifdef MASKING     
     &        *rmask(Iend+1,j)
#   endif
           enddo
        enddo
      endif
#  endif     
      return
      end
      
      subroutine t3Dinterp(tabres,i1,i2,j1,j2,k1,k2,m1,m2)
      implicit none
# include "param.h"
# include "ocean3d.h"
# include "scalars.h"

       integer i1,i2,j1,j2,k1,k2,m1,m2
       real tabres(i1:i2,j1:j2,k1:k2,m1:m2)
       integer nparent
       
       IF ((iif == 0)) THEN  
         nparent = 3
       ELSE
         nparent = nnew
       ENDIF
       
       tabres(i1:i2,j1:j2,k1:k2,m1:m2) = 
     &               t(i1:i2,j1:j2,k1:k2,nparent,m1:m2)
      
      return
      end       
# endif /* SOLVE 3D */
#else
      subroutine zoombc_3D_empty()
      return
      end
#endif
