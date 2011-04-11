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
                               ! Writes requested model
      subroutine wrt_his       ! fields at requested levels 
                               ! into history netCDF file.
!     implicit none
      integer ierr,  record, lstr, lvar, lenstr
     &   , start(2), count(2), ibuff(4), nf_fwrite
#if defined MPI & !defined PARALLEL_FILES
      include 'mpif.h'
      integer status(MPI_STATUS_SIZE), blank
#endif
#include "param.h"
#include "scalars.h"
#include "ncscrum.h"
#include "forces.h"
#include "grid.h"
#include "ocean2d.h"
#include "ocean3d.h"
#include "mixing.h"
#ifdef SEDIMENT
# include "sediment.h"
#endif
#ifdef BBL
# include "bbl.h"
#endif
#ifdef SOLVE3D
      integer tile, itrc,i,j,k
#  ifdef SEDIMENT
     &      , indxWrk
#  endif
#endif
#include "work.h"
#include "netcdf.inc"

#if defined MPI & !defined PARALLEL_FILES
      if (mynode.gt.0) then
        call MPI_Recv (blank, 1, MPI_INTEGER, mynode-1, 
     &                 1, MPI_COMM_WORLD, status, ierr) 
      endif
#endif
!
! Create/open history file; write grid arrays, if so needed.
!
      call def_his (ncidhis, nrechis, ierr)
      if (ierr .ne. nf_noerr) goto 99
      lstr=lenstr(hisname)
!                                          !!! WARNING: Once time  
! Set record within the file.              !!! stepping has been 
!                                          !!! started, it is assumed
      nrechis=max(1,nrechis)               !!! that global history
      if (nrpfhis.eq.0) then               !!! record index is
        record=nrechis                     !!! advanced by main.  
      else
        record=1+mod(nrechis-1, nrpfhis)
      endif


!#define CR  
CR      write(*,*) 'wrt_his: Entry  ' MYID 
!
! Write out evolving model variables:
! ----- --- -------- ----- ----------
!
! Time step number and record numbers.
!
      ibuff(1)=iic
      ibuff(2)=nrecrst
      ibuff(3)=nrechis
#ifdef AVERAGES
      ibuff(4)=nrecavg
#else
      ibuff(4)=0
#endif
      start(1)=1
      start(2)=record
      count(1)=4
      count(2)=1
      ierr=nf_put_vara_int (ncidhis, hisTstep, start, count, ibuff)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'time_step', record, ierr
     &                  MYID
        goto 99                                           !--> ERROR
      endif
!
! Time
!
      ierr=nf_put_var1_FTYPE (ncidhis, hisTime, record, time)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxTime))
        write(stdout,1) vname(1,indxTime)(1:lvar), record, ierr
     &                  MYID
        goto 99                                           !--> ERROR
      endif

CR      write(*,*) 'wrt_his:  time  ' MYID
!
! Barotropic mode variables: free-surface and 2D momentum
! components in XI-,ETA-directions.
! 
      if (wrthis(indxZ)) then
        ierr=nf_fwrite(zeta(START_2D_ARRAY,fast_indx_out), ncidhis,
     &                                        hisZ, record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxZ))
          write(stdout,1) vname(1,indxZ)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtZ  ' MYID
      endif
      if (wrthis(indxUb)) then
        ierr=nf_fwrite(ubar(START_2D_ARRAY,fast_indx_out), ncidhis,
     &                                       hisUb, record, u2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxUb))
          write(stdout,1) vname(1,indxUb)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his: wrtUBAR' MYID
      endif
      if (wrthis(indxVb)) then
        ierr=nf_fwrite(vbar(START_2D_ARRAY,fast_indx_out), ncidhis,
     &                                       hisVb, record, v2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxVb))
          write(stdout,1) vname(1,indxVb)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his: wrtVBAR' MYID
      endif
      if (wrthis(indxBostr)) then
        do j=0,Mm
          do i=0,Lm
            work2d(i,j)=0.5*sqrt((bustr(i,j)+bustr(i+1,j))**2
     &                          +(bvstr(i,j)+bvstr(i,j+1))**2)
     &                                                   *rho0
          enddo
        enddo
        ierr=nf_fwrite(work2d, ncidhis, hisBostr, record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxBostr))
          write(stdout,1) vname(1,indxBostr)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  bostr' MYID
      endif
      if (wrthis(indxWstr)) then
        do j=1,Mm
         do i=1,Lm
            work2d2(i,j)=0.5*sqrt((sustr(i,j)+sustr(i+1,j))**2
     &                          +(svstr(i,j)+svstr(i,j+1))**2)
     &                                                   *rho0
          enddo
        enddo
        ierr=nf_fwrite(work2d2, ncidhis, hisWstr, record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxWstr))
          write(stdout,1) vname(1,indxWstr)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  Wstr' MYID
      endif
      if (wrthis(indxUWstr)) then
        ierr=nf_fwrite(sustr*rho0, ncidhis, hisUWstr, record, u2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxUWstr))
          write(stdout,1) vname(1,indxUWstr)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  UWstr' MYID
      endif
      if (wrthis(indxVWstr)) then
        ierr=nf_fwrite(svstr*rho0, ncidhis, hisVWstr, record, v2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxVWstr))
          write(stdout,1) vname(1,indxVWstr)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  VWstr' MYID
      endif
#ifdef SOLVE3D
!
! 3D momentum components in XI- and ETA-directions.
!
      if (wrthis(indxU)) then
        ierr=nf_fwrite(u(START_2D_ARRAY,1,nstp), ncidhis,
     &                            hisU,  record,  u3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxU))
          write(stdout,1) vname(1,indxU)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtU  ' MYID
      endif
      if (wrthis(indxV)) then
        ierr=nf_fwrite(v(START_2D_ARRAY,1,nstp), ncidhis,
     &                            hisV,  record,  v3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxV))
          write(stdout,1) vname(1,indxV)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtV  ' MYID
      endif
!
! Tracer variables.
!
      do itrc=1,NT
        if (wrthis(indxT+itrc-1)) then
          ierr=nf_fwrite(t(START_2D_ARRAY,1,nstp,itrc), ncidhis,
     &                               hisT(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxT+itrc-1))
            write(stdout,1) vname(1,indxT+itrc-1)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
CR      write(*,*) 'wrt_his:  wrtT  ' MYID
        endif
      enddo
!
! Density anomaly.
!
      if (wrthis(indxR)) then
        ierr=nf_fwrite(rho, ncidhis, hisR, record, r3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxR))
          write(stdout,1) vname(1,indxR)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtRHO' MYID
      endif
!
! S-coordinate omega vertical velocity (m/s).
!
      if (wrthis(indxO)) then
        do k=0,N
          do j=0,Mm+1
            do i=0,Lm+1
              work(i,j,k)=w(i,j,k)*pm(i,j)*pn(i,j)
            enddo
          enddo
        enddo
        ierr=nf_fwrite(work, ncidhis, hisO, record, w3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxO))
          write(stdout,1) vname(1,indxO)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtO  ' MYID
      endif
!
! Write out true vertical velocity (m/s).
!
      if (wrthis(indxW)) then
        do tile=0,NSUB_X*NSUB_E-1
          call Wvlcty (tile, work) 
        enddo
        ierr=nf_fwrite(work, ncidhis, hisW, record, r3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxW))
          write(stdout,1) vname(1,indxW)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtW  ' MYID
      endif
#  ifdef VIS_COEF_3D
!
! Write out Horizontal viscosity coefficient.
!
      if (wrthis(indxVisc)) then
        ierr=nf_fwrite(visc3d_r(START_2D_ARRAY,1), ncidhis, 
     &                 hisVisc, record, r3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxVisc))
          write(stdout,1) vname(1,indxVisc)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
#  endif
#  ifdef DIF_COEF_3D
!
! Write out Horizontal Diffusivity coefficient.
!
      if (wrthis(indxDiff)) then
        ierr=nf_fwrite(diff3d(START_2D_ARRAY,1), ncidhis,
     &                 hisDiff, record, r3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxDiff))
          write(stdout,1) vname(1,indxDiff)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
#  endif
!
! Write out vertical viscosity coefficient.
!
      if (wrthis(indxAkv)) then
        ierr=nf_fwrite(Akv, ncidhis, hisAkv, record, w3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxAkv))
          write(stdout,1) vname(1,indxAkv)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtAkv' MYID
      endif
!
!  Write out vertical diffusion coefficient for potential temperature.
!
      if (wrthis(indxAkt)) then
        ierr=nf_fwrite(Akt(START_2D_ARRAY,0,itemp), ncidhis,
     &                               hisAkt, record, w3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxAkt))
          write(stdout,1) vname(1,indxAkt)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtAkt' MYID
      endif
# ifdef SALINITY
!
!  Write out vertical diffusion coefficient for salinity.
!
      if (wrthis(indxAks)) then
        ierr=nf_fwrite(Akt(START_2D_ARRAY,0,isalt), ncidhis,
     &                               hisAks, record, w3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxAks))
          write(stdout,1) vname(1,indxAks)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtAks' MYID
      endif
# endif
# ifdef LMD_SKPP
!
!  Write out depth of planetary boundary layer (m).
!
      if (wrthis(indxHbl)) then
        ierr=nf_fwrite(hbl, ncidhis, hisHbl, record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxHbl))
          write(stdout,1) vname(1,indxHbl)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtHBL' MYID
      endif
# endif
# ifdef LMD_BKPP
!
!  Write out depth of bottom planetary boundary layer (m).
!
      if (wrthis(indxHbbl)) then
        ierr=nf_fwrite(hbbl, ncidhis, hisHbbl, record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxHbbl))
          write(stdout,1) vname(1,indxHbbl)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
CR      write(*,*) 'wrt_his:  wrtHBBL' MYID
      endif
# endif
# ifdef BULK_FLUX
!
!  Write out surface heat flux component (degC/s)
!
       if (wrthis(indxShflx_rsw)) then
        ierr=nf_fwrite(shflx_rsw, ncidhis, hisShflx_rsw, record,
     &                                                     r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxShflx_rsw))
          write(stdout,1) vname(1,indxShflx_rsw)(1:lvar), record, ierr
     &                  MYID
          goto 99                                         !--> ERROR
        endif
       endif

       if (wrthis(indxShflx_rlw)) then
        ierr=nf_fwrite(shflx_rlw, ncidhis, hisShflx_rlw, record,
     &                                                     r2dvar)
         if (ierr .ne. nf_noerr) then
           lvar=lenstr(vname(1,indxShflx_rlw))
           write(stdout,1) vname(1,indxShflx_rlw)(1:lvar), record, ierr
     &                  MYID
           goto 99                                         !--> ERROR
         endif
       endif

      if (wrthis(indxShflx_lat)) then
         ierr=nf_fwrite(shflx_lat, ncidhis, hisShflx_lat, record,
     &                                                     r2dvar)
         if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxShflx_lat))
          write(stdout,1) vname(1,indxShflx_lat)(1:lvar), record, ierr
     &                  MYID
          goto 99                                         !--> ERROR
         endif
      endif

      if (wrthis(indxShflx_sen)) then
        ierr=nf_fwrite(shflx_sen, ncidhis, hisShflx_sen, record,
     &                                                     r2dvar)
          if (ierr .ne. nf_noerr) then
           lvar=lenstr(vname(1,indxShflx_sen))
           write(stdout,1) vname(1,indxShflx_sen)(1:lvar), record, ierr
     &                  MYID
           goto 99                                         !--> ERROR
           endif
      endif
# endif
# ifdef BIOLOGY
!
!  Write out depth of the euphotic layer (m).
!
      if (wrthis(indxHel)) then
        ierr=nf_fwrite(hel, ncidhis, hisHel, record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxHel))
          write(stdout,1) vname(1,indxHel)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
!
!  Write out depth of the euphotic layer (m).
!
#  ifdef BIO_NChlPZD
      if (wrthis(indxChC)) then
        ierr=nf_fwrite(theta, ncidhis, hisChC, record, r3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxChC))
          write(stdout,1) vname(1,indxChC)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
#     ifdef OXYGEN
      if (wrthis(indxU10)) then
        ierr=nf_fwrite (u10, ncidhis, hisU10, record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxU10))
          write(stdout,1) vname(1,indxU10)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
      if (wrthis(indxKvO2)) then
        ierr=nf_fwrite (Kv_O2, ncidhis, hisKvO2, record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxKvO2))
          write(stdout,1) vname(1,indxKvO2)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
      if (wrthis(indxO2sat)) then
        ierr=nf_fwrite (O2satu, ncidhis, hisO2sat, record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxO2sat))
          write(stdout,1) vname(1,indxO2sat)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
#     endif /* OXYGEN */
#  elif defined BIO_N2P2Z2D2 && defined VAR_CHL_C
      if (wrthis(indxChC1)) then
        ierr=nf_fwrite(theta1, ncidhis, hisChC1, record, r3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxChC1))
          write(stdout,1) vname(1,indxChC1)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif

      if (wrthis(indxChC2)) then
        ierr=nf_fwrite(theta2, ncidhis, hisChC2, record, r3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxChC2))
          write(stdout,1) vname(1,indxChC2)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
#  endif
# endif
# ifdef SEDIMENT
!
! Write out sediment bed layer thickness, porosity, volume 
! fraction of size class in sediment bed (2+2*NST b3dgrd variables)
!
      if (wrthis(indxBTHK)) then
        ierr=nf_fwrite(bed_thick, ncidhis, hisSed(1), 
     &                                         record, b3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxBTHK))
          write(stdout,1) vname(1,indxBTHK)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
      if (wrthis(indxBPOR)) then
        ierr=nf_fwrite(bed_poros, ncidhis, hisSed(2), 
     &                                         record, b3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxBPOR))
          write(stdout,1) vname(1,indxBPOR)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
      do itrc=1,NST
        indxWrk=indxBFRA+itrc-1
        if (wrthis(indxWrk)) then
          ierr=nf_fwrite(bed_frac(START_2D_ARRAY,1,itrc), 
     &               ncidhis, hisSed(itrc+2), record, b3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxWrk))
            write(stdout,1) vname(1,indxWrk)(1:lvar), record, 
     &                  ierr
     &                  MYID
            goto 99                                         !--> ERROR
          endif
        endif
      enddo
# endif /* SEDIMENT */
# ifdef BBL
        if (wrthis(indxAbed)) then
          ierr=nf_fwrite(Abed, ncidhis, hisBBL(1), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxAbed))
            write(stdout,1) vname(1,indxAbed)(1:lvar), record, ierr
     &                      MYID
            goto 99                                         !--> ERROR
          endif
CR        write(*,*) 'wrt_his:  Abed', MYID
        endif
        if (wrthis(indxHrip)) then
          ierr=nf_fwrite(Hripple, ncidhis, hisBBL(2), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxHrip))
            write(stdout,1) vname(1,indxHrip)(1:lvar), record, ierr
     &                      MYID
            goto 99                                         !--> ERROR
          endif
CR        write(*,*) 'wrt_his:  Hripple' MYID
        endif
        if (wrthis(indxLrip)) then
          ierr=nf_fwrite(Lripple, ncidhis, hisBBL(3), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxLrip))
            write(stdout,1) vname(1,indxLrip)(1:lvar), record, ierr
     &                      MYID
            goto 99                                         !--> ERROR
          endif
CR        write(*,*) 'wrt_his:  Lripple' MYID
        endif
        if (wrthis(indxZbnot)) then
          ierr=nf_fwrite(Zbnot, ncidhis, hisBBL(4), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxZbnot))
            write(stdout,1) vname(1,indxZbnot)(1:lvar), record, ierr
     &                      MYID
            goto 99                                         !--> ERROR
          endif
CR        write(*,*) 'wrt_his:  Zbnot' MYID
        endif
        if (wrthis(indxZbapp)) then
          ierr=nf_fwrite(Zbapp, ncidhis, hisBBL(5), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxZbapp))
            write(stdout,1) vname(1,indxZbapp)(1:lvar), record, ierr
     &                      MYID
            goto 99                                         !--> ERROR
          endif
CR        write(*,*) 'wrt_his:  Zbapp', MYID
        endif
        if (wrthis(indxBostrw)) then
          do j=0,Mm
            do i=0,Lm
              work2d(i,j)=0.5*sqrt((bustrw(i,j)+bustrw(i+1,j))**2
     &                            +(bvstrw(i,j)+bvstrw(i,j+1))**2)
     &                                                       *rho0
            enddo
          enddo
          ierr=nf_fwrite(work2d, ncidhis, hisBBL(6), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxBostrw))
            write(stdout,1) vname(1,indxBostrw)(1:lvar), record, ierr
     &                      MYID
            goto 99                                         !--> ERROR
          endif
CR        write(*,*) 'wrt_his:  bostrw' MYID
        endif
# endif /* BBL */
#endif /* SOLVE3D */

  1   format(/1x,'WRT_HIS ERROR while writing variable ''', A,
     &          ''' into history file.', /11x, 'Time record:',
     &                   I6,3x,'netCDF error code',i4,3x,a,i4)
      goto 100 
  99  may_day_flag=3
 100  continue

!
! Synchronize netCDF file to disk to allow other processes
! to access data immediately after it is written.
!
#if defined MPI & !defined PARALLEL_FILES
      ierr=nf_close (ncidhis)
      if (nrpfhis.gt.0 .and. record.ge.nrpfhis) ncidhis=-1
#else
      if (nrpfhis.gt.0 .and. record.ge.nrpfhis) then
        ierr=nf_close (ncidhis)
        ncidhis=-1
      else
        ierr=nf_sync(ncidhis)
      endif
#endif
      if (ierr .eq. nf_noerr) then
        write(stdout,'(6x,A,2(A,I4,1x),A,I3)') 'WRT_HIS -- wrote ',
     &            'history fields into time record =', record, '/',
     &             nrechis  MYID
      else
        write(stdout,'(/1x,2A/)') 'WRT_HIS ERROR: Cannot ',
     &             'synchronize/close history netCDF file.'
        may_day_flag=3
      endif

#if defined MPI & !defined PARALLEL_FILES
      if (mynode .lt. NNODES-1) then
        call MPI_Send (blank, 1, MPI_INTEGER, mynode+1,
     &                        1, MPI_COMM_WORLD,  ierr)
      endif
#endif
      return
      end
