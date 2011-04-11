! $Id$
!
!======================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
! 
! ROMS_AGRIF website : http://roms.mpl.ird.fr
!======================================================================
!
#include "cppdefs.h"
#ifdef SOLVE3D

      subroutine set_scoord     ! Input:   hmin,   Tcline,
!                               !          theta_s, theta_b 
! Define S-coordinate system.   !
!                               ! Output:  hc, sc_w(0:N), Cs_w(0:N)
      implicit none             !              sc_r(1:N), Cs_r(1:N)
# include "param.h"
# include "scalars.h"
      integer k
      real cff,cff1,cff2,cff3
!
! Set S-Curves in domain [-1 < sc < 0] at vertical W- and RHO-points.
!
      hc=min(hmin,Tcline)
      cff1=1./sinh(theta_s)
      cff2=0.5/tanh(0.5*theta_s)

      sc_w(0)=-1.0
      Cs_w(0)=-1.0

      cff=1./float(N)
      do k=1,N,+1
        sc_w(k)=cff*float(k-N)
        Cs_w(k)=(1.-theta_b)*cff1*sinh(theta_s*sc_w(k))
     &             +theta_b*(cff2*tanh(theta_s*(sc_w(k)+0.5))-0.5)

        sc_r(k)=cff*(float(k-N)-0.5)
        Cs_r(k)=(1.-theta_b)*cff1*sinh(theta_s*sc_r(k))
     &             +theta_b*(cff2*tanh(theta_s*(sc_r(k)+0.5))-0.5)
      enddo
!
! Report information about vertical S-levels.
!
      MPI_master_only write(stdout,'(/1x,A/,/1x,A,10x,A/)')
     &                       'Vertical S-coordinate System:',
     &                       'level   S-coord     Cs-curve',
     &                       'at_hmin  over_slope     at_hmax' 
      do k=N,0,-1
        cff1=sc_w(k)*hc+(hmin-hc)*Cs_w(k)
        cff2=sc_w(k)*hc+(0.5*(hmin+hmax)-hc)*Cs_w(k)
        cff3=sc_w(k)*hc+(hmax-hc)*Cs_w(k)
        MPI_master_only write(stdout,'(I6,2F12.7,4x,3F12.3)')
     &                     k, sc_w(k),Cs_w(k), cff1,cff2,cff3
      enddo
      return
      end
#else
      subroutine set_scoord_empty
      return
      end
#endif /* SOLVE3D */
