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

      subroutine setup_initial    
      implicit none
#include "param.h"
#include "scalars.h"
#include "grid.h"
#include "ocean3d.h"
#include "mixing.h"
#include "forces.h"
      integer k,i,iocheck,ierr
      real coef1,coef2,coef3,coef4,coef5,coef6,cff
      real Zwoa(24),Twoa(24),Swoa(24),Nwoa(24)
#ifdef RESTART_IN
      real duout(N),dvout(N),dtout(N,NT)
#endif
#ifdef BIOLOGY
      real NO3
# ifdef OXYGEN
      real O2
# endif
#endif

#ifdef RESTART_IN
      read(60) duout,dvout,dtout
#endif

#ifndef ANA_TCLIMA
!
! READ CLIM FILES
!
      ierr=0
      open(11,file='clim.data', status='old', form='formatted')
      read(11,*)
      read(11,*)
      read(11,*)
      read(11,*)
      read(11,*,iostat=iocheck) (Zwoa(i),i=1,12)
      if (iocheck.ne.0) ierr=ierr+1
      read(11,*,iostat=iocheck) (Zwoa(i),i=13,24)
      if (iocheck.ne.0) ierr=ierr+1
      if (iocheck.ne.0) write(*,*) 'Cannot read Zwoa'
      read(11,*)
      read(11,*)
      read(11,*,iostat=iocheck) (Twoa(i),i=1,12)
      if (iocheck.ne.0) ierr=ierr+1
      read(11,*,iostat=iocheck) (Twoa(i),i=13,24)
      if (iocheck.ne.0) ierr=ierr+1
      if (iocheck.ne.0) write(*,*) 'Cannot read Twoa'
      read(11,*)
      read(11,*)
      read(11,*,iostat=iocheck) (Swoa(i),i=1,12)
      if (iocheck.ne.0) ierr=ierr+1
      read(11,*,iostat=iocheck) (Swoa(i),i=13,24)
      if (iocheck.ne.0) ierr=ierr+1
      if (iocheck.ne.0) write(*,*) 'Cannot read Swoa'
# ifdef BIOLOGY
      read(11,*)
      read(11,*)
      read(11,*,iostat=iocheck) (Nwoa(i),i=1,12)
      if (iocheck.ne.0) ierr=ierr+1
      read(11,*,iostat=iocheck) (Nwoa(i),i=13,24)
      if (iocheck.ne.0) ierr=ierr+1
      if (iocheck.ne.0) write(*,*) 'Cannot read Nwoa'
      close(11)
      if (ierr.eq.0) write(*,*) 'Read T,S,N profiles from datafile OK'
# else
      close(11)
      if (ierr.eq.0) write(*,*) 'Read T,S profiles from datafile OK'
# endif
#endif

      do k=1,N
        u(k,nstp)=0.
        v(k,nstp)=0.
#ifdef ANA_TCLIMA
!
! THESE ARE FITS TO THE CalCOFI DATA SET MEAN PROFILE
!
!       Temperature
!
        coef1=5.
        coef2=1.475e-3
        coef3=5.65
        coef4=100.
        coef5=5.
        coef6=500.
        t(k,nstp,itemp)=coef1+(coef2*z_r(k))+
     &     (coef3*exp(z_r(k)/coef4))+
     &     (coef5*exp(z_r(k)/coef6))
!
!       Salinity
!
        coef1=34.28
        coef2=-1.75e-4
        coef3=0.8
        coef4=200.
        coef5=0.2
        coef6=400.
        t(k,nstp,isalt)=coef1+(coef2*z_r(k))-
     &     (coef3*exp(z_r(k)/coef4))-
     &     (coef5*exp(z_r(k)/coef6))
#else
!
! INTERPOLATE CLIM DATA
!
        do i=1,23
          if (z_r(k).ge.Zwoa(i) .and. z_r(k).lt.Zwoa(i+1)) then
            t(k,nstp,itemp)=( Twoa(i  )*(Zwoa(i+1)-z_r(k))+
     &                        Twoa(i+1)*(z_r(k)-Zwoa(i  )) )/
     &                        (Zwoa(i+1)-Zwoa(i))
            t(k,nstp,isalt)=( Swoa(i  )*(Zwoa(i+1)-z_r(k))+
     &                        Swoa(i+1)*(z_r(k)-Zwoa(i  )) )/
     &                        (Zwoa(i+1)-Zwoa(i))
#ifdef BIOLOGY
            t(k,nstp,iNO3_)=( Nwoa(i  )*(Zwoa(i+1)-z_r(k))+
     &                        Nwoa(i+1)*(z_r(k)-Zwoa(i  )) )/
     &                        (Zwoa(i+1)-Zwoa(i))
#endif
!            t(k,nstp,itemp)=0.3*t(k,nstp,itemp)+
!     &                          0.7*(2.*20.*exp(z_r(k)/70.))
          endif
        enddo
       !t(k,nstp,itemp)=4.*20.*exp(z_r(k)/100.)
#endif
!
#ifdef BIOLOGY
!
! This relationship is fit to the CalCOFI data down to 2500m
!
!        coef1=48.
!        coef2=3.2e-3
!        coef3=48.
!        coef4=250.
!        NO3=coef1+(coef2*z_r(k))-(coef3*exp(z_r(k)/coef4))
!        t(k,nstp,iNO3_)=NO3
!
        t(k,nstp,iNH4_)=0.1
        t(k,nstp,iChla)=0.1                    ! from the CalCOFI data set
        t(k,nstp,iPhyt)=0.1/(6.625*12.*0.053)  ! using the maximum chlorophyll ratio
        t(k,nstp,iZoo_)=0.06
        t(k,nstp,iSDet)=0.04
        t(k,nstp,iLDet)=0.02
#  ifdef OXYGEN
        coef1=-0.767
        coef2=-1.23e-3
        coef3=5.
        coef4=230.
        coef5=1.
        coef6=220.
        cff=coef1+(coef2*z_r(k))
        O2=max(cff,0.)+(coef3*exp(z_r(k)/coef4))+
     &     (coef5*exp(z_r(k)/coef6))
        t(k,nstp,iOxy_)=O2
#  endif
#endif
#ifdef NUDGE
         tnudge(k,itemp)=t(k,nstp,itemp)
         tnudge(k,isalt)=t(k,nstp,isalt)
#  ifdef BIOLOGY
         tnudge(k,iNO3_)=t(k,nstp,iNO3_)
         tnudge(k,iNH4_)=0.
         tnudge(k,iPhyt)=0.
         tnudge(k,iZoo_)=0.
         tnudge(k,iSDet)=0.
         tnudge(k,iLDet)=0.
         tnudge(k,iChla)=0.
#    ifdef OXYGEN
         tnudge(k,iOxy_)=0.
#    endif
#  endif
#endif
#ifdef RESTART_IN
        u(k,nstp)=duout(k)
        v(k,nstp)=dvout(k)
        t(k,nstp,itemp)=dtout(k,itemp)
        t(k,nstp,isalt)=dtout(k,isalt)
#  ifdef BIOLOGY
        t(k,nstp,iNO3_)=dtout(k,iNO3_)
        t(k,nstp,iNH4_)=dtout(k,iNH4_)
        t(k,nstp,iPhyt)=dtout(k,iPhyt)
        t(k,nstp,iZoo_)=dtout(k,iZoo_)
        t(k,nstp,iSDet)=dtout(k,iSDet)
        t(k,nstp,iLDet)=dtout(k,iLDet)
        t(k,nstp,iChla)=dtout(k,iChla)
#    ifdef OXYGEN
        t(k,nstp,iOxy_)=dtout(k,iOxy_)
#    endif
#  endif
#endif
!
      enddo  ! k loop
!
#ifdef SST_SKIN
      sstsk=t(N,nstp,itemp)
      dtw1=0.
#endif
      do k=1,N
        Kv(k)=0.01
        Akt(k,itemp)=0.001
        Akt(k,isalt)=0.001
      enddo

      return
      end


