!
! $Id: wrt_avg.F,v 1.9 2005/09/23 15:56:31 pmarches Exp $
!
#include "cppdefs.h"
#ifdef AVERAGES
                                     ! Write time-averaged 
      subroutine wrt_avg             ! fields into averages
                                     ! netCDF file.
!     implicit none
      integer ierr, record, lstr, lvar, lenstr
     &  , start(2), count(2), ibuff(4), nf_fwrite
# ifdef SOLVE3D
     &            , itrc            
# endif
# if defined MPI & !defined PARALLEL_FILES
      include 'mpif.h'
      integer status(MPI_STATUS_SIZE), blank
# endif
# include "param.h"
# include "scalars.h"
# include "averages.h"
# include "ncscrum.h"
# include "netcdf.inc"

#if defined MPI & !defined PARALLEL_FILES
      if (mynode.gt.0) then
        call MPI_Recv (blank, 1, MPI_INTEGER, mynode-1,
     &                 1, MPI_COMM_WORLD, status, ierr)
      endif
#endif
!
! Create/open averages file; write grid arrays, if so needed.
!
      call def_avg (ncidavg, nrecavg, ierr)
      if (ierr .ne. nf_noerr) goto 99
      lstr=lenstr(avgname)
!                                            !!! WARNING: Here it is
! Set record within the file.                !!! assumed that global
!                                            !!! restart record index
      nrecavg=max(nrecavg,1)                 !!! nrecrst is already
      if (nrpfavg.eq.0) then                 !!! advanced by main.
        record=nrecavg
      else
        record=1+mod(nrecavg-1, nrpfavg)
      endif
!
! Write out time-averaged variables:
! ----- --- ------------- ----------
!
! Time step and record indices.
!
      ibuff(1)=iic
      ibuff(2)=nrecrst
      ibuff(3)=nrechis
      ibuff(4)=nrecavg

      start(1)=1
      start(2)=record
      count(1)=4
      count(2)=1
      ierr=nf_put_vara_int (ncidavg, avgTstep, start, count, ibuff)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'time_step', record,ierr MYID
        goto 99                                           !--> ERROR
      endif
!
! Averaged time
!
      ierr=nf_put_var1_FTYPE (ncidavg, avgTime, record, time_avg)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxTime))
        write(stdout,1) vname(1,indxTime)(1:lvar), record, ierr
     &                  MYID
        goto 99                                           !--> ERROR
      endif
!
! Barotropic mode variables: free-surface and 2D momentum
! components in XI-,ETA-directions.
!
      if (wrtavg(indxZ)) then
        ierr=nf_fwrite (zeta_avg(START_2D_ARRAY), ncidavg, avgZ,
     &                                            record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxZ))
          write(stdout,1) vname(1,indxTime)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif

      if (wrtavg(indxUb)) then
        ierr=nf_fwrite (ubar_avg(START_2D_ARRAY), ncidavg, avgUb,
     &                                            record, u2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxUb))
          write(stdout,1) vname(1,indxUb)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif

      if (wrtavg(indxVb)) then
        ierr=nf_fwrite (vbar_avg(START_2D_ARRAY), ncidavg, avgVb,
     &                                            record, v2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxVb))
          write(stdout,1) vname(1,indxVb)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
!
!  Write out kinematic bottom stress (N/m2).
!
      if (wrtavg(indxBostr)) then
        ierr=nf_fwrite(bostr_avg, ncidavg, avgBostr, record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxBostr))
          write(stdout,1) vname(1,indxBostr)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif

# ifdef SOLVE3D
!
! 3D momentum components in XI- and ETA-directions.
!
      if (wrtavg(indxU)) then
        ierr=nf_fwrite (u_avg(START_2D_ARRAY,1), ncidavg, avgU,
     &                                           record, u3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxU))
          write(stdout,1) vname(1,indxU)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif 
      endif

      if (wrtavg(indxV)) then
        ierr=nf_fwrite (v_avg(START_2D_ARRAY,1), ncidavg, avgV,
     &                                           record, v3dvar)
        if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxV))
          write(stdout,1) vname(1,indxV)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
!
! Tracer variables.
!
      do itrc=1,NT
        if (wrtavg(indxT+itrc-1)) then
          ierr=nf_fwrite (t_avg(START_2D_ARRAY,1,itrc), ncidavg,
     &                              avgT(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxT+itrc-1))
            write(stdout,1) vname(1,indxT+itrc-1)(1:lvar),
     &                      record, ierr MYID
            goto 99                                       !--> ERROR
          endif
        endif
      enddo
!
! Density anomaly.
!
      if (wrtavg(indxR)) then
        ierr=nf_fwrite (rho_avg(START_2D_ARRAY,1), ncidavg, avgR,
     &                                             record, r3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxR))
          write(stdout,1) vname(1,indxR)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
!
!  Write out S-coordinate omega vertical velocity (m/s).
!
      if (wrtavg(indxO)) then
        ierr=nf_fwrite(omega_avg(START_2D_ARRAY,0),ncidavg,avgO,
     &                                             record, w3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxO))
          write(stdout,1) vname(1,indxO)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
!
! Write out true vertical velocity (m/s).
!
      if (wrtavg(indxW)) then
        ierr=nf_fwrite(w_avg(START_2D_ARRAY,1),ncidavg,avgW,
     &                                         record, r3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxW))
          write(stdout,1) vname(1,indxW)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
#  ifdef VIS_COEF_3D
!
! Write out Horizontal viscosity coefficient.
!
      if (wrtavg(indxVisc)) then
        ierr=nf_fwrite(visc3d_avg(START_2D_ARRAY,1), ncidavg, avgVisc, 
     &                                              record, r3dvar)
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
! Write out Horizontal diffusivity coefficient.
!
      if (wrtavg(indxDiff)) then
        ierr=nf_fwrite(diff3d_avg(START_2D_ARRAY,1), ncidavg, avgDiff,
     &                                                 record, r3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxDiff))
          write(stdout,1) vname(1,indxDiff)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
#  endif
#  ifdef AVERAGES_K
!
! Write out vertical viscosity coefficient.
!
      if (wrtavg(indxAkv)) then
        ierr=nf_fwrite(Akv_avg(START_2D_ARRAY,0), ncidavg, avgAkv, 
     &                                              record, w3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxAkv))
          write(stdout,1) vname(1,indxAkv)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
!
!  Write out vertical diffusion coefficient for potential temperature.
!
      if (wrtavg(indxAkt)) then
        ierr=nf_fwrite(Akt_avg(START_2D_ARRAY,0,itemp), ncidavg,
     &                                    avgAkt, record, w3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxAkt))
          write(stdout,1) vname(1,indxAkt)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
!# ifdef SALINITY
!
!  Write out vertical diffusion coefficient for salinity.
!
      if (wrtavg(indxAks)) then
        ierr=nf_fwrite(Akt_avg(START_2D_ARRAY,0,isalt), ncidavg,
     &                                   avgAks, record, w3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxAks))
          write(stdout,1) vname(1,indxAks)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
#  endif
#  ifdef LMD_SKPP
!
!  Write out depth of planetary boundary layer (m).
!
      if (wrtavg(indxHbl)) then
        ierr=nf_fwrite(hbl_avg, ncidavg, avgHbl, record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxHbl))
          write(stdout,1) vname(1,indxHbl)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
#  endif
#  ifdef LMD_BKPP
!
!  Write out depth of planetary boundary layer (m).
!
      if (wrtavg(indxHbbl)) then
        ierr=nf_fwrite(hbbl_avg, ncidavg, avgHbbl, record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxHbbl))
          write(stdout,1) vname(1,indxHbbl)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
#  endif
#  ifdef BIOLOGY
!
!  Write out depth of the euphotic layer (m).
!
      if (wrtavg(indxHel)) then
        ierr=nf_fwrite(hel_avg, ncidavg, avgHel, record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxHel))
          write(stdout,1) vname(1,indxHel)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
!
!  Write out Chlorophyll to Carbon ratio (m).
!
#   ifdef BIO_NPZD
      if (wrtavg(indxChC)) then
        ierr=nf_fwrite(theta_avg, ncidavg, avgChC, record, r3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxChC))
          write(stdout,1) vname(1,indxChC)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
#     ifdef OXYGEN
      if (wrtavg(indxU10)) then
        ierr=nf_fwrite (u10_avg, ncidavg, avgU10, record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxU10))
          write(stdout,1) vname(1,indxU10)(1:lvar), record, ierr,
     &                    nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
      endif
      if (wrtavg(indxKvO2)) then
        ierr=nf_fwrite (Kv_O2_avg, ncidavg, avgKvO2, record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxKvO2))
          write(stdout,1) vname(1,indxKvO2)(1:lvar), record, ierr,
     &                    nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
      endif
      if (wrtavg(indxO2sat)) then
        ierr=nf_fwrite (O2satu_avg, ncidavg, avgO2sat, record, r2dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxO2sat))
          write(stdout,1) vname(1,indxO2sat)(1:lvar), record, ierr,
     &                    nf_strerror(ierr) MYID
          goto 99                                         !--> ERROR
        endif
      endif
#     endif /* OXYGEN */
#   elif defined BIO_N2P2Z2D2 && defined VAR_CHL_C
      if (wrtavg(indxChC1)) then
        ierr=nf_fwrite(theta1_avg, ncidavg, avgChC1, record, r3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxChC1))
          write(stdout,1) vname(1,indxChC1)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif

      if (wrtavg(indxChC2)) then
        ierr=nf_fwrite(theta2_avg, ncidavg, avgChC2, record, r3dvar)
        if (ierr .ne. nf_noerr) then
          lvar=lenstr(vname(1,indxChC2))
          write(stdout,1) vname(1,indxChC2)(1:lvar), record, ierr
     &                    MYID
          goto 99                                         !--> ERROR
        endif
      endif
#   endif
#  endif /* BIOLOGY */
# endif /* SOLVE3D */
  1   format(/' WRT_AVG - ERROR while writing variable(',1x,a,1x,
     &               ')into averages file.',/,11x,'Time record:',
     &                      i6,3x,'netCDF error code',i4,3x,a,i4)
      goto 100
  99  may_day_flag=3
 100  continue

!
! Synchronize netCDF file to disk to allow other processes
! to access data immediately after it is written.
!
# if defined MPI & !defined PARALLEL_FILES
      ierr=nf_close(ncidavg)
      if (nrpfavg.gt.0 .and. record.ge.nrpfavg) ncidavg=-1
# else
      if (nrpfavg.gt.0 .and. record.ge.nrpfavg) then
        ierr=nf_close(ncidavg)
        ncidavg=-1
      else
        ierr=nf_sync(ncidavg)
      endif
# endif
      if (ierr .eq. nf_noerr) then
        write(stdout,'(6x,A,2(A,I4,1x),A,I3)') 'WRT_AVG -- wrote ',
     &            'averaged fields into time record =', record, '/',
     &             nrecavg  MYID
      else
        write(stdout,'(/1x,2A/)') 'WRT_AVG ERROR: Cannot ',
     &             'synchronize/close averages netCDF file.'
        may_day_flag=3
      endif

# if defined MPI & !defined PARALLEL_FILES
      if (mynode .lt. NNODES-1) then
        call MPI_Send (blank, 1, MPI_INTEGER, mynode+1,
     &                        1, MPI_COMM_WORLD,  ierr)
      endif
# endif
      return
      end
#else
      subroutine wrt_avg_empty
      end
#endif /* AVERAGES */
