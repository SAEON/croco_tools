!
! $Id$
!
#include "cppdefs.h"
#if defined DIAGNOSTICS_UV && defined AVERAGES
!
!---------------------------------------------------------------
! Write time-averaged diags flux fields into averages netCDF file
! Writes requested model fields at requested levels into
! diagnostics netCDF file.
!---------------------------------------------------------------
!
      subroutine wrt_diagsM_avg 

      implicit none
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
#include "diagnostics.h"
#ifdef SEDIMENT
# include "sediment.h"
#endif
#ifdef BBL
# include "bbl.h"
#endif
#ifdef SOLVE3D
      integer tile, itrc,i,j,k,ivar
# ifdef SEDIMENT
     &      , indxWrk
# endif
# include "work.h"
#endif
#include "netcdf.inc"
#if defined MPI & !defined PARALLEL_FILES
      if (mynode.gt.0) then
        call MPI_Recv (blank, 1, MPI_INTEGER, mynode-1, 
     &                 1, MPI_COMM_WORLD, status, ierr) 
      endif
#endif
!
# undef DEBUG
!
!
! Create/open diagnostic file; write grid arrays, if so needed.
!
      call def_diagsM_avg (nciddiaM_avg, nrecdiaM_avg, ierr)
      if (ierr .ne. nf_noerr) goto 99
      lstr=lenstr(dianameM_avg)
!                                          !!! WARNING: Once time  
! Set record within the file.              !!! stepping has been 
!                                          !!! started, it is assumed
      nrecdiaM_avg=max(nrecdiaM_avg,1)     !!! that global history
      if (nrpfdiaM_avg.eq.0) then          !!! record index is
        record=nrecdiaM_avg                !!! advanced by main.  
      else
        record=1+mod(nrecdiaM_avg-1, nrpfdiaM_avg)
      endif
!
! Write out evolving model variables:
! ----- --- -------- ----- ----------
!
! Time step number and record numbers.
!
      ibuff(1)=iic
      ibuff(2)=nrecrst
      ibuff(3)=nrechis
      ibuff(4)=nrecdiaM_avg
      start(1)=1
      start(2)=record
      count(1)=4
      count(2)=1
      ierr=nf_put_vara_int (nciddiaM_avg, diaTstepM_avg, 
     &                                             start, count, ibuff)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'time_step_avg', record, ierr
     &                  MYID
        goto 99                                           !--> ERROR
      endif
!
! Averaged diag Time
!
      ierr=nf_put_var1_FTYPE (nciddiaM_avg, diaTimeM_avg, record,
     &                                              timediaM_avg)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxTime))
        write(stdout,1) vname(1,indxTime)(1:lvar), record, ierr
     &                  MYID
        goto 99                                           !--> ERROR
      endif

!
! Momentum diag variables.
! -------- ---- ----------
!
!
      do itrc=1,2
!
! indxMXadv
!
          if (itrc.eq.1) then
            ivar=u3dvar
          else
            ivar=v3dvar
          endif

          ierr=nf_fwrite(MXadv_avg(START_2D_ARRAY,1,itrc), nciddiaM_avg,
     &                              diaMXadv_avg(itrc), record, ivar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxMXadv+itrc-1))
            write(stdout,1) vname(1,indxMXadv+itrc-1)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
!
! indxMYadv
!
          ierr=nf_fwrite(MYadv_avg(START_2D_ARRAY,1,itrc), nciddiaM_avg,
     &                               diaMYadv_avg(itrc), record, ivar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxMYadv+itrc-1))
            write(stdout,1) vname(1,indxMYadv+itrc-1)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
!
! indxMVadv
!
          ierr=nf_fwrite(MVadv_avg(START_2D_ARRAY,1,itrc), nciddiaM_avg,
     &                               diaMVadv_avg(itrc), record, ivar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxMVadv+itrc-1))
            write(stdout,1) vname(1,indxMVadv+itrc-1)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
!
! indxMCor
!
          ierr=nf_fwrite(MCor_avg(START_2D_ARRAY,1,itrc), nciddiaM_avg,
     &                               diaMCor_avg(itrc), record, ivar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxMCor+itrc-1))
            write(stdout,1) vname(1,indxMCor+itrc-1)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
!
! indxMPrsgrd
!
          ierr=nf_fwrite(MPrsgrd_avg(START_2D_ARRAY,1,itrc),nciddiaM_avg,
     &                               diaMPrsgrd_avg(itrc), record, ivar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxMPrsgrd+itrc-1))
            write(stdout,1) vname(1,indxMPrsgrd+itrc-1)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
!
! indxMHmix
!
          ierr=nf_fwrite(MHmix_avg(START_2D_ARRAY,1,itrc), nciddiaM_avg,
     &                               diaMHmix_avg(itrc), record, ivar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxMHmix+itrc-1))
            write(stdout,1) vname(1,indxMHmix+itrc-1)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
!
! indxMVmix
!
          ierr=nf_fwrite(MVmix_avg(START_2D_ARRAY,1,itrc), nciddiaM_avg,
     &                               diaMVmix_avg(itrc), record, ivar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxMVmix+itrc-1))
            write(stdout,1) vname(1,indxMVmix+itrc-1)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
!
! indxMrate
!
          ierr = nf_fwrite(Mrate_avg(START_2D_ARRAY,1,itrc), 
     &        nciddiaM_avg, diaMrate_avg(itrc), record, ivar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxMrate+itrc-1))
            write(stdout,1) vname(1,indxMrate+itrc-1)(1:lvar),
     &           record, ierr MYID 
            goto 99
          endif
!
      enddo
!
#  ifdef DEBUG
      i=20
      j=2
      k=2
      itrc=1
      write(*,*) 'Write diag UV avg at : i='
     &                   ,i,' j=',j,' k=',k,' iUV=',itrc
      write(*,*) 'MXadv_avg(i,j,k,itrc)   ',MXadv_avg(i,j,k,itrc)
      write(*,*) 'MYadv_avg(i,j,k,itrc)   ',MYadv_avg(i,j,k,itrc)
      write(*,*) 'MVadv_avg(i,j,k,itrc)   ',MVadv_avg(i,j,k,itrc)
      write(*,*) 'MCor_avg(i,j,k,itrc)    ',MCor_avg(i,j,k,itrc)
      write(*,*) 'MPrsgrd_avg(i,j,k,itrc) ',MPrsgrd_avg(i,j,k,itrc)
      write(*,*) 'MHmix_avg(i,j,k,itrc)   ',MHmix_avg(i,j,k,itrc)
      write(*,*) 'MVmix_avg(i,j,k,itrc)   ',MVmix_avg(i,j,k,itrc)
      write(*,*) 'Mrate_avg(i,j,k,itrc)   ',Mrate_avg(i,j,k,itrc)

      write(*,*) 'SumUVavg(i,j,k,itrc) = ',MXadv_avg(i,j,k,itrc)
     &                                  +  MYadv_avg(i,j,k,itrc)
     &                                  +  MVadv_avg(i,j,k,itrc)
     &                                  +  MCor_avg(i,j,k,itrc)
     &                                  +  MPrsgrd_avg(i,j,k,itrc)
     &                                  +  MHmix_avg(i,j,k,itrc)
     &                                  +  MVmix_avg(i,j,k,itrc)
     &                                  -  Mrate_avg(i,j,k,itrc)
              write(*,*) '----------------------------------'
#  endif /* DEBUG */


  1   format(/1x,'WRT_DIAG_AVG ERROR while writing variable ''', A,
     &          ''' into diag_avg file.', /11x, 'Time record:',
     &                   I6,3x,'netCDF error code',i4,3x,a,i4)
      goto 100 
  99  may_day_flag=3
 100  continue

!
! Synchronize netCDF file to disk to allow other processes
! to access data immediately after it is written.
!

#if defined MPI & !defined PARALLEL_FILES
      ierr=nf_close (nciddiaM_avg)
      if (nrpfdiaM_avg.gt.0 .and. record.ge.nrpfdiaM_avg) nciddiaM_avg=-1
#else
      if (nrpfdiaM_avg.gt.0 .and. record.ge.nrpfdiaM_avg) then
        ierr=nf_close (nciddiaM_avg)
        nciddiaM_avg=-1
      else
        ierr=nf_sync(nciddiaM_avg)
      endif
#endif


      if (ierr .eq. nf_noerr) then
       write(stdout,'(6x,A,2(A,I4,1x),A,I3)') 'WRT_DIAGM_AVG -- wrote ',
     &            'diagM_avg fields into time record =', record, '/',
     &             nrecdiaM_avg  MYID
      else
        write(stdout,'(/1x,2A/)') 'WRT_DIAGM_AVG ERROR: Cannot ',
     &             'synchronize/close diag_avg netCDF file.'
        may_day_flag=3
      endif

#if defined MPI & !defined PARALLEL_FILES
      if (mynode .lt. NNODES-1) then
        call MPI_Send (blank, 1, MPI_INTEGER, mynode+1,
     &                        1, MPI_COMM_WORLD,  ierr)
      endif
#endif
#undef DEBUG
      return
      end

#else
      subroutine wrt_diagsM_avg_empty
      end
#endif /* (DIAGNOSTICS_UV) && AVERAGES */
