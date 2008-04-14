!
! $Id: wrt_diags_avg.F,v 1.3 2005/10/10 13:40:20 pmarches Exp $
!
#include "cppdefs.h"
#if defined DIAGNOSTICS_TS && defined AVERAGES
!
!---------------------------------------------------------------
! Write time-averaged diags flux fields into averages netCDF file
! Writes requested model fields at requested levels into
! diagnostics netCDF file.
!---------------------------------------------------------------
!
      subroutine wrt_diags_avg 

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
      call def_diags_avg (nciddia_avg, nrecdia_avg, ierr)
      if (ierr .ne. nf_noerr) goto 99
      lstr=lenstr(dianame_avg)
!                                          !!! WARNING: Once time  
! Set record within the file.              !!! stepping has been 
!                                          !!! started, it is assumed
      nrecdia_avg=max(nrecdia_avg,1)       !!! that global history
      if (nrpfdia_avg.eq.0) then           !!! record index is
        record=nrecdia_avg                 !!! advanced by main.  
      else
        record=1+mod(nrecdia_avg-1, nrpfdia_avg)
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
      ibuff(4)=nrecdia_avg
      start(1)=1
      start(2)=record
      count(1)=4
      count(2)=1
      ierr=nf_put_vara_int (nciddia_avg, diaTstep_avg, 
     &                                             start, count, ibuff)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'time_step_avg', record, ierr
     &                  MYID
        goto 99                                           !--> ERROR
      endif
!
! Averaged diag Time
!
      ierr=nf_put_var1_FTYPE (nciddia_avg, diaTime_avg, record,
     &                                              timedia_avg)
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
      do itrc=1,NT
!
! indxTXadv
!
          ierr=nf_fwrite(TXadv_avg(START_2D_ARRAY,1,itrc), nciddia_avg,
     &                              diaTXadv_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxTXadv+itrc-1))
            write(stdout,1) vname(1,indxTXadv+itrc-1)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
!
! indxTYadv
!
          ierr=nf_fwrite(TYadv_avg(START_2D_ARRAY,1,itrc), nciddia_avg,
     &                               diaTYadv_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxTYadv+itrc-1))
            write(stdout,1) vname(1,indxTYadv+itrc-1)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
!
! indxTVadv
!
          ierr=nf_fwrite(TVadv_avg(START_2D_ARRAY,1,itrc), nciddia_avg,
     &                               diaTVadv_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxTVadv+itrc-1))
            write(stdout,1) vname(1,indxTVadv+itrc-1)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
!
! indxTHmix
!
          ierr=nf_fwrite(THmix_avg(START_2D_ARRAY,1,itrc), nciddia_avg,
     &                               diaTHmix_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxTHmix+itrc-1))
            write(stdout,1) vname(1,indxTHmix+itrc-1)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
!
! indxTVmix
!
          ierr=nf_fwrite(TVmix_avg(START_2D_ARRAY,1,itrc), nciddia_avg,
     &                               diaTVmix_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxTVmix+itrc-1))
            write(stdout,1) vname(1,indxTVmix+itrc-1)(1:lvar), record,
     &                      ierr MYID
            goto 99                                       !--> ERROR
          endif
!
! indxTForc  
!
         ierr = nf_fwrite(TForc_avg(START_2D_ARRAY,1,itrc), 
     &        nciddia_avg, diaTForc_avg(itrc), record, r3dvar)
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxTForc+itrc-1))
            write(stdout,1) vname(1,indxTForc+itrc-1)(1:lvar),
     &           record, ierr MYID 
            goto 99
         endif
!
! indxTrate
!
          ierr = nf_fwrite(Trate_avg(START_2D_ARRAY,1,itrc), 
     &        nciddia_avg, diaTrate_avg(itrc), record, r3dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxTrate+itrc-1))
            write(stdout,1) vname(1,indxTrate+itrc-1)(1:lvar),
     &           record, ierr MYID 
            goto 99
          endif
!
!
!---------------------------------------------------------------
! Tracer diagnostic variables averaged over the MLD
!---------------------------------------------------------------
!
! indxTXadv
!
          ierr=nf_fwrite(TXadv_mld_avg(START_2D_ARRAY,itrc),nciddia_avg,
     &                           diaTXadv_mld_avg(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxTXadv_mld+itrc-1))
            write(stdout,1) vname(1,indxTXadv_mld+itrc-1)(1:lvar), 
     &                            record, ierr MYID
            goto 99                                       !--> ERROR
          endif
!
! indxTYadv
!
          ierr=nf_fwrite(TYadv_mld_avg(START_2D_ARRAY,itrc),nciddia_avg,
     &                           diaTYadv_mld_avg(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxTYadv_mld+itrc-1))
            write(stdout,1) vname(1,indxTYadv_mld+itrc-1)(1:lvar),
     &                            record, ierr MYID
            goto 99                                       !--> ERROR
          endif
!
! indxTVadv
!
          ierr=nf_fwrite(TVadv_mld_avg(START_2D_ARRAY,itrc),nciddia_avg,
     &                           diaTVadv_mld_avg(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxTVadv_mld+itrc-1))
            write(stdout,1) vname(1,indxTVadv_mld+itrc-1)(1:lvar),
     &                            record,  ierr MYID
            goto 99                                       !--> ERROR
          endif
!
! indxTHmix
!
          ierr=nf_fwrite(THmix_mld_avg(START_2D_ARRAY,itrc),nciddia_avg,
     &                           diaTHmix_mld_avg(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxTHmix_mld+itrc-1))
            write(stdout,1) vname(1,indxTHmix_mld+itrc-1)(1:lvar),
     &                            record, ierr MYID
            goto 99                                       !--> ERROR
          endif
!
! indxTVmix
!
          ierr=nf_fwrite(TVmix_mld_avg(START_2D_ARRAY,itrc),nciddia_avg,
     &                           diaTVmix_mld_avg(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxTVmix_mld+itrc-1))
            write(stdout,1) vname(1,indxTVmix_mld+itrc-1)(1:lvar), 
     &                            record, ierr MYID
            goto 99                                       !--> ERROR
          endif
!
! indxTForc 
!
         ierr = nf_fwrite(TForc_mld_avg(START_2D_ARRAY,itrc), 
     &             nciddia_avg, diaTForc_mld_avg(itrc), record, r2dvar)
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxTForc_mld+itrc-1))
            write(stdout,1) vname(1,indxTForc_mld+itrc-1)(1:lvar),
     &                            record, ierr MYID 
            goto 99
         endif
!
! indxTrate
!
          ierr = nf_fwrite(Trate_mld_avg(START_2D_ARRAY,itrc), 
     &        nciddia_avg, diaTrate_mld_avg(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxTrate_mld+itrc-1))
            write(stdout,1) vname(1,indxTrate_mld+itrc-1)(1:lvar),
     &                            record, ierr MYID 
            goto 99
          endif
!
! indxTentr
!
          ierr = nf_fwrite(Tentr_mld_avg(START_2D_ARRAY,itrc),
     &        nciddia_avg, diaTentr_mld_avg(itrc), record, r2dvar)
          if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxTentr_mld+itrc-1))
            write(stdout,1) vname(1,indxTentr_mld+itrc-1)(1:lvar),
     &                            record, ierr MYID
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
      write(*,*) 'Write diag TS avg at : i='
     &                ,i,' j=',j,' k=',k,' iTS=',itrc
      write(*,*) 'TXadv_avg(i,j,k,itrc) ',TXadv_avg(i,j,k,itrc)
      write(*,*) 'TYadv_avg(i,j,k,itrc) ',TYadv_avg(i,j,k,itrc)
      write(*,*) 'TVadv_avg(i,j,k,itrc) ',TVadv_avg(i,j,k,itrc)
      write(*,*) 'THmix_avg(i,j,k,itrc) ',THmix_avg(i,j,k,itrc)
      write(*,*) 'TVmix_avg(i,j,k,itrc) ',TVmix_avg(i,j,k,itrc)
      write(*,*) 'TForc_avg(i,j,k,itrc) ',TForc_avg(i,j,k,itrc)
      write(*,*) 'Trate_avg(i,j,k,itrc) ',Trate_avg(i,j,k,itrc)

      write(*,*) 'SumTSavg(i,j,k,itrc) = ',TXadv_avg(i,j,k,itrc)
     &                                  +  TYadv_avg(i,j,k,itrc)
     &                                  +  TVadv_avg(i,j,k,itrc)
     &                                  +  THmix_avg(i,j,k,itrc)
     &                                  +  TVmix_avg(i,j,k,itrc)
     &                                  +  TForc_avg(i,j,k,itrc)
     &                                  -  Trate_avg(i,j,k,itrc)
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
      ierr=nf_close (nciddia_avg)
      if (nrpfdia_avg.gt.0 .and. record.ge.nrpfdia_avg) nciddia_avg=-1
#else
      if (nrpfdia_avg.gt.0 .and. record.ge.nrpfdia_avg) then
        ierr=nf_close (nciddia_avg)
        nciddia_avg=-1
      else
        ierr=nf_sync(nciddia_avg)
      endif
#endif


      if (ierr .eq. nf_noerr) then
        write(stdout,'(6x,A,2(A,I4,1x),A,I3)') 'WRT_DIAG_AVG -- wrote ',
     &            'diag_avg fields into time record =', record, '/',
     &             nrecdia_avg  MYID
      else
        write(stdout,'(/1x,2A/)') 'WRT_DIAG_AVG ERROR: Cannot ',
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
      subroutine wrt_diag_avg_empty
      end
#endif /* (DIAGNOSTICS_TS && AVERAGES */
