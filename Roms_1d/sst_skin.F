! $Id$
!======================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
! 
! ROMS_AGRIF website : http://www.romsagrif.org
!======================================================================
#include "cppdefs.h"
#ifdef SST_SKIN

      SUBROUTINE sst_skin
      IMPLICIT NONE
# include "param.h"
# include "grid.h"
# include "ocean3d.h" 
# include "forces.h"
# include "mixing.h"
# include "scalars.h"
# define tind nstp

      REAL      z1, an, zk, rhow, cw, znuw, zkw, sdate
      PARAMETER(z1=3.)
      PARAMETER(an=.3,zk=.4, rhow=1025., cw=4190.,
     &          znuw=1.e-6, zkw=1.4e-7, sdate=1201.6667)
      REAL    sw, q, qn, zeta, dep, dtw3, skinmax, skinmin,
     &        fs, con1, con2, con3, con4, con5, zlan, q2, ts, phi, qn1,
     &        usw, qo, swo, us, tb, dtc, dtw, alw, dtwo, delt, f1
!
!     Input arguments
!     (all fluxes are positive downwards)
!     real q       ! LH + SH + LW (degC m/s), + down
!     real sw      ! Net shortwave flux (degC m/s), + down
!     real u       ! Wind speed (m/s)
!     real us      ! Atmospheric friction velocity (m/s)
!     real tb      ! Bulk temperature (deg C)
!     real dtwo    ! Warm layer temp. diff. from previous time (deg C)
!     Local variables
!     real qn      ! Q + R_s - R(-d)
!     real zeta    ! -z / L
!     real dep     ! Skin layer depth (m)
!     real dtw3
!     Output variables
!     real dtw     ! Warm layer temp. diff. (deg C)
!     real dtc     ! Cool skin temp. diff. (deg C)
!     real ts      ! Skin temperature (deg C)
!
      skinmax=-9999.
      skinmin=9999.
!
      q=stflx(itemp)-srflx 
      sw=srflx 
      tb=t(N,tind,itemp) 
      !z1=z_w(N)-z_r(N-5) 
      usw=max(0.01,sqrt(sqrt(sustr**2+svstr**2)))
      dtwo=dtw1
      delt=dt
!
! TEMPORARY KLUDGE
      f1=1.-0.27*exp(-2.8*z1)-0.45*exp(-0.07*z1)
! cool skin
      dtc=0.0
! tb in C
      alw=1.e-5*max(tb,1.)
      con4=16.*g*alw*znuw**3/zkw**2
      con5=con4/usw**4
! otherwise, iterations would be needed for the computation of fs
! iteration impact is less than 0.03C
      q2=max(1./(rhow*cw),-q)
      zlan=6./(1.+(con5*q2)**0.75)**0.333
      dep=zlan*znuw/usw      ! skin layer depth (m)
      fs=0.065+11.*dep-(6.6e-5/dep)*(1.-exp(-dep/8.e-4))
      fs=max(fs,0.01)        ! fract. of solar rad. absorbed in sublayer
      dtc=dep*(q+sw*fs)/zkw  ! cool skin temp. diff (deg C)
      dtc=min(dtc,0.)
! warm layer (X. Zeng)
      dtw=0.0
! tb in C
      alw=1.e-5*max(tb,1.)
      con1=sqrt(5.*z1*g*alw/an)
      con2=zk*g*alw
      qn=q+sw*f1
!  does not change when qn is positive
      if(dtwo.gt.0..and.qn.lt.0.) then
         qn1=sqrt(dtwo)*usw**2/con1
         qn=max(qn,qn1)
      endif
      zeta=z1*con2*qn/usw**3
      if(zeta.gt.0.) then
         phi=1.+5.*zeta
      else
         phi=1./sqrt(1.-16.*zeta)
      endif
      con3=zk*usw/(z1*phi)
! use all SW flux
      dtw=(dtwo+(an+1.)/an*(q+sw*f1)* 
     &                    delt/z1)/(1.+(an+1.)*con3*delt)
      dtw=max(0.,dtw)
      dtwo=dtw
      ts = tb + dtw + dtc
!
      skinmax=amax1(skinmax,ts-tb)
      skinmin=amin1(skinmin,ts-tb)
      sstsk=ts  ! +273.15      ! convert ts (in C) to sstsk (in K)
      dtw1=dtw             ! dtw always in C


!      write(6,999)  ts,tb,tb+dtw
! 999  format('TS TB TC = ',3f15.4)
!     print *, 'check skin sst skinmax = ', skinmax, '  skinmin = ', skinmin
!
#else
      subroutine sst_skin_empty
#endif /* SST_SKIN */
      return
      end

