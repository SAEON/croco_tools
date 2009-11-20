!
! $Id$
!
#include "cppdefs.h"
#if defined DIAGNOSTICS_UV 
!
!---------------------------------------------------------------
! Write diagnostics fields at requested levels into diagnostics 
! netCDF file.

!---------------------------------------------------------------
!
      subroutine wrt_diagsM

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
      integer tile,itrc,i,j,k,ivar
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
#undef DEBUG
!
!
! Create/open diagnostic file; write grid arrays, if so needed.
!
      call def_diagsM (nciddiaM, nrecdiaM, ierr)
      if (ierr .ne. nf_noerr) goto 99
      lstr=lenstr(dianameM)
!                                          !!! WARNING: Once time  
! Set record within the file.              !!! stepping has been 
!     
                                           !!! started, it is assumed
      if (iic.eq.0) nrecdiaM=nrecdiaM+1      !!! that global history
      if (nrpfdiaM.eq.0) then               !!! record index is
        record=nrecdiaM                     !!! advanced by main.  
      else
        record=1+mod(nrecdiaM-1, nrpfdiaM)
      endif
!
!---------------------------------------------------------------
! Write out evolving model variables:
!---------------------------------------------------------------
!
! Time step number and record numbers.
!
      ibuff(1)=iic
      ibuff(2)=nrecrst
      ibuff(3)=nrechis
      ibuff(4)=nrecdiaM
      start(1)=1
      start(2)=record
      count(1)=4
      count(2)=1
      ierr=nf_put_vara_int (nciddiaM, diaTstepM, start, count, ibuff)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'time_step', record, ierr
     &                  MYID
        goto 99                                           !--> ERROR
      endif
!
! Time
!
      ierr=nf_put_var1_FTYPE (nciddiaM, diaTimeM, record, time)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxTime))
        write(stdout,1) vname(1,indxTime)(1:lvar), record, ierr
     &                  MYID
        goto 99                                           !--> ERROR
      endif

!---------------------------------------------------------------
! Momentum diagnostic variables.
!---------------------------------------------------------------
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
        if (wrtdiaM(indxMXadv+itrc-1)) then
          ierr=nf_fwrite(MXadv(START_2D_ARRAY,1,itrc), nciddiaM,
     &                               diaMXadv(itrc), record, ivar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxMXadv+itrc-1))
            write(stdout,1) vname(1,indxMXadv+itrc-1)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
        endif       
!
! indxMYadv
!
        if (wrtdiaM(indxMYadv+itrc-1)) then
          ierr=nf_fwrite(MYadv(START_2D_ARRAY,1,itrc), nciddiaM,
     &                               diaMYadv(itrc), record, ivar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxMYadv+itrc-1))
            write(stdout,1) vname(1,indxMYadv+itrc-1)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
         endif
!
! indxMVadv
!
        if (wrtdiaM(indxMVadv+itrc-1)) then
          ierr=nf_fwrite(MVadv(START_2D_ARRAY,1,itrc), nciddiaM,
     &                               diaMVadv(itrc), record, ivar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxMVadv+itrc-1))
            write(stdout,1) vname(1,indxMVadv+itrc-1)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
         endif
!
! indxMCor
!
        if (wrtdiaM(indxMCor+itrc-1)) then
          ierr=nf_fwrite(MCor(START_2D_ARRAY,1,itrc), nciddiaM,
     &                               diaMCor(itrc), record, ivar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxMCor+itrc-1))
            write(stdout,1) vname(1,indxMCor+itrc-1)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
         endif
!
! indxMPrsgrd
!
        if (wrtdiaM(indxMPrsgrd+itrc-1)) then
          ierr=nf_fwrite(MPrsgrd(START_2D_ARRAY,1,itrc), nciddiaM,
     &                               diaMPrsgrd(itrc), record, ivar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxMPrsgrd+itrc-1))
            write(stdout,1) vname(1,indxMPrsgrd+itrc-1)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
         endif
!
! indxMHmix
!
        if (wrtdiaM(indxMHmix+itrc-1)) then
          ierr=nf_fwrite(MHmix(START_2D_ARRAY,1,itrc), nciddiaM,
     &                               diaMHmix(itrc), record, ivar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxMHmix+itrc-1))
            write(stdout,1) vname(1,indxMHmix+itrc-1)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
         endif
!
! indxMVmix
!
        if (wrtdiaM(indxMVmix+itrc-1)) then
          ierr=nf_fwrite(MVmix(START_2D_ARRAY,1,itrc), nciddiaM,
     &                               diaMVmix(itrc), record, ivar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxMVmix+itrc-1))
            write(stdout,1) vname(1,indxMVmix+itrc-1)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
         endif
!
! indxMrate
!
        if (wrtdiaM(indxMrate+itrc-1)) then
          ierr = nf_fwrite(Mrate(START_2D_ARRAY,1,itrc), 
     &        nciddiaM, diaMrate(itrc), record, ivar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxMrate+itrc-1))
            write(stdout,1) vname(1,indxMrate+itrc-1)(1:lvar),
     &           record, ierr MYID 
            goto 99
          endif
         endif
!
      enddo
!
#  ifdef DEBUG
      i=20
      j=2
      k=2
      itrc=1
      write(*,*) 'Write diag UV his at : i='
     &                      ,i,' j=',j,' k=',k,' iUV=',itrc
      write(*,*) 'MXadv(i,j,k,itrc)   ',MXadv(i,j,k,itrc)
      write(*,*) 'MYadv(i,j,k,itrc)   ',MYadv(i,j,k,itrc)
      write(*,*) 'MVadv(i,j,k,itrc)   ',MVadv(i,j,k,itrc)
      write(*,*) 'MCor(i,j,k,itrc)    ',MCor(i,j,k,itrc)
      write(*,*) 'MPrsgrd(i,j,k,itrc) ',MPrsgrd(i,j,k,itrc)
      write(*,*) 'MHmix(i,j,k,itrc)   ',MHmix(i,j,k,itrc)
      write(*,*) 'MVmix(i,j,k,itrc)   ',MVmix(i,j,k,itrc)
      write(*,*) 'Mrate(i,j,k,itrc)   ',Mrate(i,j,k,itrc)

      write(*,*) 'SumUVhis(i,j,k,itrc) = ',MXadv(i,j,k,itrc)
     &                                  +  MYadv(i,j,k,itrc)
     &                                  +  MVadv(i,j,k,itrc)
     &                                  +  MCor(i,j,k,itrc)
     &                                  +  MPrsgrd(i,j,k,itrc)
     &                                  +  MHmix(i,j,k,itrc)
     &                                  +  MVmix(i,j,k,itrc)
     &                                  -  Mrate(i,j,k,itrc)
      write(*,*) '-----------------------------------------'
#  endif /* DEBUG */

  1   format(/1x,'WRT_DIAGM ERROR while writing variable ''', A,
     &          ''' into diag file.', /11x, 'Time record:',
     &                   I6,3x,'netCDF error code',i4,3x,a,i4)
      goto 100 
  99  may_day_flag=3
 100  continue

!
!---------------------------------------------------------------
! Synchronize netCDF file to disk to allow other processes
! to access data immediately after it is written.
!---------------------------------------------------------------
!
#if defined MPI & !defined PARALLEL_FILES
      ierr=nf_close (nciddiaM)
      if (nrpfdiaM.gt.0 .and. record.ge.nrpfdiaM) nciddiaM=-1
#else
      if (nrpfdiaM.gt.0 .and. record.ge.nrpfdiaM) then
        ierr=nf_close (nciddiaM)
        nciddiaM=-1
      else
        ierr=nf_sync(nciddiaM)
      endif
#endif


      if (ierr .eq. nf_noerr) then
        MPI_master_only write(stdout,'(6x,A,2(A,I4,1x),A,I3)')
     &            'WRT_DIAGM -- wrote',
     &            ' diag fields into time record =', record, '/',
     &             nrecdiaM  MYID
      else
        MPI_master_only write(stdout,'(/1x,2A/)') 
     &             'WRT_DIAGM ERROR: Cannot ',
     &             'synchronize/close diag netCDF file.'
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

#else
      subroutine wrt_diagsM_empty
      end
#endif /* (DIAGNOSTICS_UV) */
