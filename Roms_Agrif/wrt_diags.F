!
! $Id: wrt_diags.F,v 1.3 2005/10/10 13:40:19 pmarches Exp $
!
#include "cppdefs.h"
#if defined DIAGNOSTICS_TS
!
!---------------------------------------------------------------
! Write diagnostics fields at requested levels into diagnostics 
! netCDF file.
!---------------------------------------------------------------
!
      subroutine wrt_diags

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
      call def_diags (nciddia, nrecdia, ierr)
      if (ierr .ne. nf_noerr) goto 99
      lstr=lenstr(dianame)
!                                          !!! WARNING: Once time  
! Set record within the file.              !!! stepping has been 
!     
                                           !!! started, it is assumed
      if (iic.eq.0) nrecdia=nrecdia+1      !!! that global history
      if (nrpfdia.eq.0) then               !!! record index is
        record=nrecdia                     !!! advanced by main.  
      else
        record=1+mod(nrecdia-1, nrpfdia)
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
      ibuff(4)=nrecdia
      start(1)=1
      start(2)=record
      count(1)=4
      count(2)=1
      ierr=nf_put_vara_int (nciddia, diaTstep, start, count, ibuff)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'time_step', record, ierr
     &                  MYID
        goto 99                                           !--> ERROR
      endif
!
! Time
!
      ierr=nf_put_var1_FTYPE (nciddia, diaTime, record, time)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxTime))
        write(stdout,1) vname(1,indxTime)(1:lvar), record, ierr
     &                  MYID
        goto 99                                           !--> ERROR
      endif
!
!---------------------------------------------------------------
! Tracer diagnostic variables.
!---------------------------------------------------------------
!
!
      do itrc=1,NT
!
! indxTXadv
!
          ierr=nf_fwrite(TXadv(START_2D_ARRAY,1,itrc), nciddia,
     &                               diaTXadv(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxTXadv+itrc-1))
            write(stdout,1) vname(1,indxTXadv+itrc-1)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
!
! indxTYadv
!
          ierr=nf_fwrite(TYadv(START_2D_ARRAY,1,itrc), nciddia,
     &                               diaTYadv(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxTYadv+itrc-1))
            write(stdout,1) vname(1,indxTYadv+itrc-1)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
!
! indxTVadv
!
          ierr=nf_fwrite(TVadv(START_2D_ARRAY,1,itrc), nciddia,
     &                               diaTVadv(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxTVadv+itrc-1))
            write(stdout,1) vname(1,indxTVadv+itrc-1)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
!
! indxTHmix
!
          ierr=nf_fwrite(THmix(START_2D_ARRAY,1,itrc), nciddia,
     &                               diaTHmix(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxTHmix+itrc-1))
            write(stdout,1) vname(1,indxTHmix+itrc-1)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
!
! indxTVmix
!
          ierr=nf_fwrite(TVmix(START_2D_ARRAY,1,itrc), nciddia,
     &                               diaTVmix(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxTVmix+itrc-1))
            write(stdout,1) vname(1,indxTVmix+itrc-1)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
!
! indxTbody 
!
         ierr = nf_fwrite(Tbody(START_2D_ARRAY,1,itrc), 
     &               nciddia, diaTbody(itrc), record, r3dvar)
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxTbody+itrc-1))
            write(stdout,1) vname(1,indxTbody+itrc-1)(1:lvar),
     &           record, ierr, nf_strerror(ierr) MYID 
            goto 99
         endif
!
! indxTrate
!
          ierr = nf_fwrite(Trate(START_2D_ARRAY,1,itrc), 
     &        nciddia, diaTrate(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxTrate+itrc-1))
            write(stdout,1) vname(1,indxTrate+itrc-1)(1:lvar),
     &           record, ierr, nf_strerror(ierr) MYID 
            goto 99
          endif
!
      enddo
!
#  ifdef DEBUG
      i=20
      j=2
      k=2
      itrc=2
      write(*,*) 'Write diag TS his at : i='
     &                ,i,' j=',j,' k=',k,' iTS=',itrc
      write(*,*) 'TXadv(i,j,k,itrc) ',TXadv(i,j,k,itrc)
      write(*,*) 'TYadv(i,j,k,itrc) ',TYadv(i,j,k,itrc)
      write(*,*) 'TVadv(i,j,k,itrc) ',TVadv(i,j,k,itrc)
      write(*,*) 'THmix(i,j,k,itrc) ',THmix(i,j,k,itrc)
      write(*,*) 'TVmix(i,j,k,itrc) ',TVmix(i,j,k,itrc)
      write(*,*) 'Tbody(i,j,k,itrc) ',Tbody(i,j,k,itrc)
      write(*,*) 'Trate(i,j,k,itrc) ',Trate(i,j,k,itrc)

      write(*,*) 'SumTShis(i,j,k,itrc) = ',TXadv(i,j,k,itrc)
     &                                  +  TYadv(i,j,k,itrc)
     &                                  +  TVadv(i,j,k,itrc)
     &                                  +  THmix(i,j,k,itrc)
     &                                  +  TVmix(i,j,k,itrc)
     &                                  +  Tbody(i,j,k,itrc)
     &                                  -  Trate(i,j,k,itrc)
      write(*,*) '-----------------------------------------'
#  endif /* DEBUG */


  1   format(/1x,'WRT_DIAG ERROR while writing variable ''', A,
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
      ierr=nf_close (nciddia)
      if (nrpfdia.gt.0 .and. record.ge.nrpfdia) nciddia=-1
#else
      if (nrpfdia.gt.0 .and. record.ge.nrpfdia) then
        ierr=nf_close (nciddia)
        nciddia=-1
      else
        ierr=nf_sync(nciddia)
      endif
#endif


      if (ierr .eq. nf_noerr) then
        write(stdout,'(6x,A,2(A,I4,1x),A,I3)') 'WRT_DIAG -- wrote ',
     &            'diag fields into time record =', record, '/',
     &             nrecdia  MYID
      else
        write(stdout,'(/1x,2A/)') 'WRT_DIAG ERROR: Cannot ',
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
      subroutine wrt_diag_empty
      end
#endif /*DIAGNOSTICS_TS */
