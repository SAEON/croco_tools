! $Id$
!======================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
! 
! ROMS_AGRIF website : http://www.romsagrif.org
!======================================================================
      implicit none
#include "cppdefs.h"
#include "param.h"
#include "grid.h"
#include "ocean3d.h"
#include "forces.h"
#include "mixing.h"
#include "scalars.h"
      integer k,itrc,nrestart,itout,irestart
      real time_start,cff,nudg_days, 
     &     run_days,write_days,plot_days,output_days,restart_days
#ifdef BIN_OUTPUT ||  ASCII_OUTPUT
      real*4 zout(N),uout(N),vout(N),tout(N,NT),aktout(N),zblout
#ifdef SST_SKIN
      real*4 tskout
#endif
#ifdef BULK_FLUX
      real*4 rswout,rlwout,latout,senout,susout,svsout
#endif
#ifdef UPWELLING
      real*4 wout(N)
#endif
#endif
#ifdef ASCII_OUTPUT
      integer idiz,iuni
      character*7 tfile
      character*16 form
#endif
#ifdef RESTART_OUT
      real duout(N),dvout(N),dtout(N,NT)
#endif
!
!=============================================================
!
!                   RUNNING PARAMETERS
!
!=============================================================
!

      dt       = 30.        ! timestep "seconds"
      hmax     = 200.       ! max depth "meters"
      theta_s  = 2.         ! vertical grid stretching parameter
      tdays    = 0.         ! start time "days"
!
      run_days     = 720.   ! simulation time
      nudg_days    = 360.    ! nudging time scale
      write_days   = 1.     ! write every ...
      plot_days    = 1.     ! plot every ...
      output_days  = 1./8.  ! dump history every ...
      restart_days = 30.    ! dump restart every ...
      twrite       = 1.     ! start writing every ...
!
!
!--------------------------------------------------------------------
!
!	Initialisation
!
!--------------------------------------------------------------------
!
      time     = tdays*day2sec
      ntimes   = int((run_days*day2sec+0.001)/dt) 
      nwrite   = int((write_days*day2sec+0.001)/dt) 
      nplot    = int((plot_days*day2sec+0.001)/dt) 
      noutput  = int((output_days*day2sec+0.001)/dt) 
      nrestart = int((restart_days*day2sec+0.001)/dt) 
      iic=0
      ntstart=1
      nstp=1
      nnew=1
      call setup_grid
      call init_forces
      call setup_initial
#ifdef NUDGE
        do k=1,N
           delta(k)=1./(nudg_days*86400.)          ! [s-1]
        enddo
#endif
!
!------------------------------------------------------------------
!                          I/O
!------------------------------------------------------------------
#ifdef ASCII_OUTPUT
       open(21,file='u.dat',form='formatted',status='unknown')
       open(22,file='v.dat',form='formatted',status='unknown')
#  ifdef UPWELLING
       open(23,file='w.dat',form='formatted',status='unknown')
#  endif
       open(24,file='z.dat',form='formatted',status='unknown')
       open(25,file='days.dat',form='formatted',status='unknown')
       open(26,file='zbl.dat',form='formatted',status='unknown')
       do itrc=1,NT
          idiz=int(itrc/10)
          iuni=itrc-10*idiz
          write(tfile,1000) idiz,iuni
          open(26+itrc,file=tfile,form='formatted',status='unknown')
       enddo
 1000  format('t',i1,i1,'.dat')
       write(form,3000) N
 3000  format('(',i4,'(1x,g12.6))')
       open(26+NT+1,file='akt.dat',form='formatted',status='unknown')
#ifdef SST_SKIN
       open(26+NT+2,file='tsk.dat',form='formatted',status='unknown')
#endif
#ifdef BULK_FLUX
       open(26+NT+3,file='rsw.dat',form='formatted',status='unknown')
       open(26+NT+4,file='rlw.dat',form='formatted',status='unknown')
       open(26+NT+5,file='lat.dat',form='formatted',status='unknown')
       open(26+NT+6,file='sen.dat',form='formatted',status='unknown')
       open(26+NT+7,file='sus.dat',form='formatted',status='unknown')
       open(26+NT+8,file='svs.dat',form='formatted',status='unknown')
#endif
#endif
#ifdef BIN_OUTPUT ||  ASCII_OUTPUT
       do k=1,N
         zout(k)=sngl(z_r(k))
         uout(k)=sngl(u(k,nstp))
         vout(k)=sngl(v(k,nstp))
# ifdef UPWELLING
         wout(k)=sngl(0.5*(w(k-1)+w(k)))
# endif
         do itrc=1,NT
           tout(k,itrc)=sngl(t(k,nstp,itrc))
         enddo
         aktout(k)=sngl(Kt(k))
       enddo
       zblout=sngl(z_bl)
# ifdef SST_SKIN
       tskout=sngl(sstsk)
# endif
# ifdef BULK_FLUX
       rswout=sngl(shflx_rsw)
       rlwout=sngl(shflx_rlw)
       latout=sngl(shflx_lat)
       senout=sngl(shflx_sen)
       susout=sngl(rho0*sustr)
       svsout=sngl(rho0*svstr)
# endif
       itout=1
# ifdef BIN_OUTPUT 
       write(70) zout
       write(70) uout,vout,tout,zblout
# endif
# ifdef ASCII_OUTPUT
       write(21,form) (uout(k),k=1,N)
       write(22,form) (vout(k),k=1,N)
#  ifdef UPWELLING
       write(23,form) (wout(k),k=1,N)
#  endif
       write(24,form) (zout(k),k=1,n)
       write(25,*) 0.
       write(26,*) zblout
       do itrc=1,NT
          write(26+itrc,form) (tout(k,itrc),k=1,N)
       enddo
       write(26+NT+1,form) (1.e4*aktout(k),k=1,N)
#  ifdef SST_SKIN
       write(26+NT+2,form) tskout
#  endif
#  ifdef BULK_FLUX
       write(26+NT+3,form) rswout
       write(26+NT+4,form) rlwout
       write(26+NT+5,form) latout
       write(26+NT+6,form) senout
       write(26+NT+7,form) susout
       write(26+NT+8,form) svsout
#  endif
# endif
#endif
#ifdef RESTART_OUT
       irestart=1
       do k=1,N
         duout(k)=u(k,nnew)
         dvout(k)=v(k,nnew)
         do itrc=1,NT
           dtout(k,itrc)=t(k,nnew,itrc)
         enddo
       enddo
       itout=itout+1
       write(50) duout,dvout,dtout
#endif
#ifdef NCARG
      call opngks
      call set_color
#endif
!
!=============================================================
!
!	MAIN LOOP
!
!=============================================================
!
      time_start=time
      do iic=ntstart,ntimes+1

        time=time_start+dt*float(iic-ntstart)
        tdays=time*sec2day
        if (TIME_TO_WRITE) write(6,999) iic,tdays
 999    format('IIC - TDAYS = ',i10,f10.2)


        nstp=1+mod(iic-ntstart,2)
        nnew=3-nstp

        call set_forces
#ifdef BULK_FLUX
        call bulk_flux
#endif
#ifdef SST_SKIN
        call sst_skin
#endif
        call rho_eos
#ifdef LMD_MIXING
        call lmd_vmix
#endif
#ifdef LMD_KPP
        call lmd_kpp
#endif
#ifdef BIOLOGY
        do k=0,N
          Akt(k,iNO3_)=Ks(k)
          Akt(k,iNH4_)=Ks(k)
          Akt(k,iPhyt)=Ks(k)
          Akt(k,iChla)=Akt(k,iPhyt)
          Akt(k,iZoo_)=Ks(k)
          Akt(k,iSDet)=Ks(k)
          Akt(k,iLDet)=Ks(k)
          Akt(k,iOxy_)=Ks(k)
        enddo
#endif
        call step3d
#ifdef BIOLOGY
        call biology
#endif
!
!------------------------------------------------------------------
!                          I/O
!------------------------------------------------------------------
!
#ifdef NCARG
        if (TIME_TO_PLOT) then
          call plot
        endif
#endif
#if defined BIN_OUTPUT || defined ASCII_OUTPUT
        if (TIME_TO_OUTPUT) then
          do k=1,N
            uout(k)=sngl(u(k,nnew))
            vout(k)=sngl(v(k,nnew))
# ifdef UPWELLING
            wout(k)=sngl(w(k))
# endif
            do itrc=1,NT
              tout(k,itrc)=sngl(t(k,nnew,itrc))
            enddo
            aktout(k)=sngl(Kt(k))
          enddo
          zblout=sngl(z_bl)
# ifdef SST_SKIN
          tskout=sngl(sstsk)
# endif
# ifdef BULK_FLUX
          rswout=sngl(shflx_rsw)
          rlwout=sngl(shflx_rlw)
          latout=sngl(shflx_lat)
          senout=sngl(shflx_sen)
          susout=sngl(rho0*sustr)
          svsout=sngl(rho0*svstr)
# endif
          itout=itout+1
# if defined BIN_OUTPUT
          write(70) uout,vout,tout,zblout
# endif
# ifdef ASCII_OUTPUT
          write(21,form) (uout(k),k=1,N)
          write(22,form) (vout(k),k=1,N)
#  ifdef UPWELLING
          write(23,form) (wout(k),k=1,N)
#  endif
          write(25,*) tdays
          write(26,*) zblout
          do itrc=1,NT
             write(26+itrc,form) (tout(k,itrc),k=1,N)
          enddo
          write(26+NT+1,form) (1.e4*aktout(k),k=1,N)
#  ifdef SST_SKIN
          write(26+NT+2,form) tskout
#  endif
#  ifdef BULK_FLUX
          write(26+NT+3,form) rswout
          write(26+NT+4,form) rlwout
          write(26+NT+5,form) latout
          write(26+NT+6,form) senout
          write(26+NT+7,form) susout
          write(26+NT+8,form) svsout
#  endif
# endif
        endif
#endif
#ifdef RESTART_OUT
        if (mod(iic-ntstart,nrestart).eq.0) then
          irestart=irestart+1
          do k=1,N
            duout(k)=u(k,nnew)
            dvout(k)=v(k,nnew)
            do itrc=1,NT
              dtout(k,itrc)=t(k,nnew,itrc)
            enddo
          enddo
          write(*,*) 'WRITING RESTART FILE NUMBER: ',irestart
          write(50) duout,dvout,dtout
          write(51,*) irestart
        endif
#endif
        nnew=3-nstp
!
!-------------------------------------------------------------
!
      enddo       ! end temporal loop
!
!-------------------------------------------------------------
!
!	close everything
!      
#ifdef BIN_OUTPUT || ASCII_OUTPUT
      write(*,*) 'TOTAL NUMBER OF BINARY OUTPUTS = ',itout
#endif
#ifdef ASCII_OUTPUT
       close(21)
       close(22)
# ifdef UPWELLING
       close(23)
# endif
       close(24)
       close(25)
       close(26)
       do itrc=1,NT
          close(26+itrc)
       enddo
       close(26+itrc+1)
# ifdef SST_SKIN
       close(26+itrc+2)
# endif
# ifdef BULK_FLUX
       close(26+itrc+3)
       close(26+itrc+4)
       close(26+itrc+5)
       close(26+itrc+6)
       close(26+itrc+7)
       close(26+itrc+8)
# endif
#endif
#ifdef NCARG
      call clsgks
#endif
      stop
      end
