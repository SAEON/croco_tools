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


      subroutine init_forces
      implicit none
#include "param.h"
#include "scalars.h"
#include "forces.h"
      real ss_ave,st_ave
      integer i,iocheck,ierr
!
!  Read in the forcing file
!
      ierr=0
      open(11,file='forces.data', status='old', form='formatted')
      read(11,*)
      read(11,*)
      read(11,*)
      read(11,*,iostat=iocheck) lonr,latr
      if (iocheck.ne.0) ierr=ierr+1
      if(iocheck.ne.0) write(*,*) 
     &          'SETUP_INITIAL: Cannot read LON LAT'
      read(11,*)
      read(11,*)
      read(11,*,iostat=iocheck) (sustr_m(i),i=1, 6)
      if (iocheck.ne.0) ierr=ierr+1
      read(11,*,iostat=iocheck) (sustr_m(i),i=7,12)
      if (iocheck.ne.0) ierr=ierr+1
      if(iocheck.ne.0) write(*,*) 'Cannot read SUSTR'
      read(11,*)
      read(11,*)
      read(11,*,iostat=iocheck) (svstr_m(i),i=1, 6)
      if (iocheck.ne.0) ierr=ierr+1
      read(11,*,iostat=iocheck) (svstr_m(i),i=7,12)
      if (iocheck.ne.0) ierr=ierr+1
      if(iocheck.ne.0) write(*,*) 'Cannot read SVSTR'
      read(11,*)
      read(11,*)
      read(11,*,iostat=iocheck) (stflx_m(i),i=1, 6)
      if (iocheck.ne.0) ierr=ierr+1
      read(11,*,iostat=iocheck) (stflx_m(i),i=7,12)
      if (iocheck.ne.0) ierr=ierr+1
      if(iocheck.ne.0) write(*,*) 'Cannot read STFLX'
      read(11,*)
      read(11,*)
      read(11,*,iostat=iocheck) (ssflx_m(i),i=1, 6)
      if (iocheck.ne.0) ierr=ierr+1
      read(11,*,iostat=iocheck) (ssflx_m(i),i=7,12)
      if (iocheck.ne.0) ierr=ierr+1
      if(iocheck.ne.0) write(*,*) 'Cannot read SSFLX'
      read(11,*)
      read(11,*)
      read(11,*,iostat=iocheck)   (sst_m(i),i=1, 6)
      if (iocheck.ne.0) ierr=ierr+1
      read(11,*,iostat=iocheck)   (sst_m(i),i=7,12)
      if (iocheck.ne.0) ierr=ierr+1
      if(iocheck.ne.0) write(*,*) 'Cannot read SST'
      read(11,*)
      read(11,*)
      read(11,*,iostat=iocheck) (dqdt_m(i),i=1, 6)
      if (iocheck.ne.0) ierr=ierr+1
      read(11,*,iostat=iocheck) (dqdt_m(i),i=7,12)
      if (iocheck.ne.0) ierr=ierr+1
      if(iocheck.ne.0) write(*,*) 'Cannot read DQDT'
      read(11,*)
      read(11,*)
      read(11,*,iostat=iocheck) (srflx_m(i),i=1, 6)
      if (iocheck.ne.0) ierr=ierr+1
      read(11,*,iostat=iocheck) (srflx_m(i),i=7,12)
      if (iocheck.ne.0) ierr=ierr+1
      if(iocheck.ne.0) write(*,*) 'Cannot read SRFLX'
      read(11,*)
      read(11,*)
      read(11,*,iostat=iocheck) (upwi_m(i),i=1, 6)
      if (iocheck.ne.0) ierr=ierr+1
      read(11,*,iostat=iocheck) (upwi_m(i),i=7,12)
      if (iocheck.ne.0) ierr=ierr+1
      if(iocheck.ne.0) write(*,*) 'Cannot read UPWI'
#ifdef BULK_FLUX
      read(11,*)
      read(11,*)
      read(11,*,iostat=iocheck) (radsw_m(i),i=1, 6)
      if (iocheck.ne.0) ierr=ierr+1
      read(11,*,iostat=iocheck) (radsw_m(i),i=7,12)
      if (iocheck.ne.0) ierr=ierr+1
      if(iocheck.ne.0) write(*,*) 'Cannot read RADSW'
      read(11,*)
      read(11,*)
      read(11,*,iostat=iocheck) (radlw_m(i),i=1, 6)
      if (iocheck.ne.0) ierr=ierr+1
      read(11,*,iostat=iocheck) (radlw_m(i),i=7,12)
      if (iocheck.ne.0) ierr=ierr+1
      if(iocheck.ne.0) write(*,*) 'Cannot read RADLW'
      read(11,*)
      read(11,*)
      read(11,*,iostat=iocheck) (prate_m(i),i=1, 6)
      if (iocheck.ne.0) ierr=ierr+1
      read(11,*,iostat=iocheck) (prate_m(i),i=7,12)
      if (iocheck.ne.0) ierr=ierr+1
      if(iocheck.ne.0) write(*,*) 'Cannot read PRATE'
      read(11,*)
      read(11,*)
      read(11,*,iostat=iocheck) (tair_m(i),i=1, 6)
      if (iocheck.ne.0) ierr=ierr+1
      read(11,*,iostat=iocheck) (tair_m(i),i=7,12)
      if (iocheck.ne.0) ierr=ierr+1
      if(iocheck.ne.0) write(*,*) 'Cannot read TAIR'
      read(11,*)
      read(11,*)
      read(11,*,iostat=iocheck) (rhum_m(i),i=1, 6)
      if (iocheck.ne.0) ierr=ierr+1
      read(11,*,iostat=iocheck) (rhum_m(i),i=7,12)
      if (iocheck.ne.0) ierr=ierr+1
      if(iocheck.ne.0) write(*,*) 'Cannot read RHUM'
      read(11,*)
      read(11,*)
      read(11,*,iostat=iocheck) (uwnd_m(i),i=1, 6)
      if (iocheck.ne.0) ierr=ierr+1
      read(11,*,iostat=iocheck) (uwnd_m(i),i=7,12)
      if (iocheck.ne.0) ierr=ierr+1
      if(iocheck.ne.0) write(*,*) 'Cannot read UWND'
      read(11,*)
      read(11,*)
      read(11,*,iostat=iocheck) (vwnd_m(i),i=1, 6)
      if (iocheck.ne.0) ierr=ierr+1
      read(11,*,iostat=iocheck) (vwnd_m(i),i=7,12)
      if (iocheck.ne.0) ierr=ierr+1
      if(iocheck.ne.0) write(*,*) 'Cannot read VWND'
#endif
      close(11)
      if(ierr.eq.0) write(*,*) 'Read forces from datafile OK'
!
!  Demean sea surface net heat flux
!
      st_ave=0.
      do i=1,12
        st_ave=st_ave+stflx_m(i)
      enddo
      st_ave=st_ave/12.
      do i=1,12
        stflx_m(i)=stflx_m(i)-st_ave
      enddo
!
!  Demean sea surface freshwater flux
!
      ss_ave=0.
      do i=1,12
        ss_ave=ss_ave+ssflx_m(i)
      enddo
      ss_ave=ss_ave/12.
      do i=1,12
        ssflx_m(i)=ssflx_m(i)-ss_ave
      enddo

!
! Coriolis frequency
!
      f=4*pi/86400*sin(latr*pi/180)

      return
      end
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      subroutine set_forces
      implicit none
#include "param.h"
#include "scalars.h"
#include "forces.h"
#include "ocean3d.h"
      integer indx, i,j, itrc
      real sec2month, tau,cff1,cff2, cffM, cffT,cffS, cffW
      real cff,fac1,fac2,cos_h,dec,cos_d,sin_d,tan_d,phi,h0 
      parameter (sec2month=1./2592000., cffM=1./rho0,
     &           cffT=1./(rho0*Cp), cffS=0.01/86400.,
     &           cffW=1./(100.*300.e3) )

      tau=time*sec2month+0.5
      indx=int(tau)
      i=1+mod(indx+11,12)
      j=1+mod(indx   ,12)
      cff2=tau-float(indx)
      cff1=1.-cff2

      sustr=cffM*(cff1*sustr_m(i)+cff2*sustr_m(j)) 
      svstr=cffM*(cff1*svstr_m(i)+cff2*svstr_m(j))
      srflx=cffT*(cff1*srflx_m(i)+cff2*srflx_m(j))
      stflx(itemp)=cffT*( cff1*stflx_m(i)+cff2*stflx_m(j)
     &                  +(cff1*dqdt_m(i)+cff2*dqdt_m(j))
     &                    *(t(N,nstp,itemp)-cff1*sst_m(i)
     &                                     -cff2*sst_m(j))
     &                                                   )
      stflx(isalt)=cffS*t(N,nstp,isalt)*( cff1*ssflx_m(i)
     &                                   +cff2*ssflx_m(j))
#ifdef BULK_FLUX
      radsw=cffT*(cff1*radsw_m(i)+cff2*radsw_m(j))
      radlw=cffT*(cff1*radlw_m(i)+cff2*radlw_m(j))
      prate=cffS*(cff1*prate_m(i)+cff2*prate_m(j))
      tair=cff1*tair_m(i)+cff2*tair_m(j)
      rhum=cff1*rhum_m(i)+cff2*rhum_m(j)
      uwnd=cff1*uwnd_m(i)+cff2*uwnd_m(j)
      vwnd=cff1*vwnd_m(i)+cff2*vwnd_m(j)
      wspd=sqrt(uwnd*uwnd+vwnd*vwnd)
#endif
#ifdef BIOLOGY
      do itrc=isalt+1,NT
        stflx(itrc)=0.
      enddo
#endif
      upwi=cffW*(cff1*upwi_m(i)+cff2*upwi_m(j))

#ifdef DIURNAL_SW
!
!  Modulate average dayly insolation to get diurnal cycle
!  by:  pi*( cos(h)*cos(d)*cos(phi)+   sin(d)sin(phi))/
!          (sin(h0)*cos(d)*cos(phi)+h0*sin(d)sin(phi))
!  h, d, phi are hour, declination, latitude angles
!  h0 is hour angle at sunset and sunrise
!
      cos_h=cos(2.*pi*(tdays-int(tdays)))
      dec=-0.406*cos(deg2rad*(tdays-int(tdays/360.)*360.))
      cos_d=cos(dec)
      sin_d=sin(dec)
      tan_d=tan(dec)
      phi=latr*deg2rad
      h0=acos(-tan(phi)*tan_d)
      fac1=cos_d*cos(phi)
      fac2=sin_d*sin(phi)
      cff=pi*(cos_h*fac1+fac2)/(sin(h0)*fac1+h0*fac2)
!
      stflx(itemp)=stflx(itemp)-srflx
      srflx=max(0.,cff*srflx)
      stflx(itemp)=stflx(itemp)+srflx
# ifdef BULK_FLUX
      radsw=max(0.,cff*radsw)
# endif
#endif

      return
      end



