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
#ifdef STATIONS
                            ! Writes requested model
      subroutine wrt_sta    ! fields at requested levels 
                            ! into station netCDF file.
# ifdef AGRIF
      USE Agrif_Util 
# endif
      implicit none
# include "param.h"
# include "scalars.h"
# include "ncscrum.h"
# include "sta.h"
# include "nc_sta.h"
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "mixing.h"
# include "netcdf.inc"

      integer ierr,  record, lvar, lenstr, ista, id
     &   , indxunrel(Msta),Toutint(Msta)
     &   , start(2), count(2), ibuff(2), nf_fwrite
      logical newf
      character*65 vinfo
      real Tout(Msta)
# ifdef ALL_SIGMA
      integer k, startN(3), countN(3)
      real ToutN(N,Msta)
# endif

# if defined MPI & !defined PARALLEL_FILES
      include 'mpif.h'
      integer status(MPI_STATUS_SIZE), blank
# endif
# if defined MPI & !defined PARALLEL_FILES
      if (mynode.gt.0) then
        call MPI_Recv (blank, 1, MPI_INTEGER, mynode-1, 
     &                 1, MPI_COMM_WORLD, status, ierr) 
      endif
# endif
!
! Create/open history file; write grid arrays, if so needed.
!
      newf=.false.
      call def_sta (ncidsta, nrecsta, ierr, newf)
      if (ierr .ne. nf_noerr) goto 99
!                                       !!! WARNING: Once time  
! Set record within the file.           !!! stepping has been
!                                       !!! started, it is assumed
      if (iic.eq.0) nrecsta=nrecsta+1   !!! that the global station
      if (nrpfsta.eq.0) then            !!!  history record index 
        record=nrecsta                  !!! "nrecsta" is advanced
      else                              !!! by main.
        record=1+mod(nrecsta-1, nrpfsta)
      endif
c     write(*,*) 'nrecsta = ', nrecsta
c     write(*,*) 'nrpfsta = ', nrpfsta
c     write(*,*) 'record = ', record

!
! Write out evolving model variables:
! ----- --- -------- ----- ----------
!
! Time step number and record numbers.
!
      ibuff(1)=iic
      ibuff(2)=nrecsta
      start(1)=1
      start(2)=record
      count(1)=2
      count(2)=1

      ierr=nf_put_vara_int (ncidsta, staTstep, start, count, ibuff)
      if (ierr .ne. nf_noerr) then
        write(stdout,1) 'time_step', record, ierr, nf_strerror(ierr)
     &                   MYID
        goto 99                                           !--> ERROR
      endif
!
! Time
!

      ierr=nf_put_var1_FTYPE (ncidsta, staTime, record, time)
      if (ierr .ne. nf_noerr) then
        lvar=lenstr(vname(1,indxTime))
        write(stdout,1) vname(1,indxTime)(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif

!
!    define position in nc file to write station data
!
      start(1)=1
      count(1)=nstas
      start(2)=record
      count(2)=1
# ifdef ALL_SIGMA
      startN(1)=1
      countN(1)=N
      startN(2)=1
      countN(2)=nstas
      startN(3)=record
      countN(3)=1
# endif

!
! Store information about station horizontal and vertical 
! location if required
!
      if (wrtsta(indxstaGrd)) then
c       if (iic.eq.0) then
# ifdef AGRIF
!
! Grid level
!
        do id=1,nstas
          Toutint(id)=stainfo(istagrd,id)
        enddo
        ierr=nf_put_vara_int (ncidsta, staGlevel, start,count, Toutint)
        if (ierr .ne. nf_noerr) then
          vinfo='grid level'
          lvar=lenstr(vinfo)
          write(stdout,1) vinfo(1:lvar), record, ierr,
     &                    nf_strerror(ierr) MYID
          goto 99                                           !--> ERROR
        endif
# endif /* AGRIF */
# ifdef SPHERICAL
!
! Sations Lat,Lon
!
        do ista=1,nstas
          Tout(ista)=stadata(istalat,ista)
        enddo
        ierr=nf_put_vara_FTYPE(ncidsta,staLat,start,count,
     &                           Tout)
        if (ierr .ne. nf_noerr) then
          vinfo='Lat'
          lvar=lenstr(vinfo)
          write(stdout,1) vinfo(1:lvar), record, ierr,
     &                    nf_strerror(ierr) MYID
          goto 99                                           !--> ERROR
        endif
        do ista=1,nstas
          Tout(ista)=stadata(istalon,ista)
        enddo
        ierr=nf_put_vara_FTYPE(ncidsta,staLon,start,count,
     &                           Tout)
        if (ierr .ne. nf_noerr) then
          vinfo='Lon'
          lvar=lenstr(vinfo)
          write(stdout,1) vinfo(1:lvar), record, ierr,
     &                    nf_strerror(ierr) MYID
          goto 99                                           !--> ERROR
        endif
# endif /* SPHERICAL */
!
! Sations X,Y
!
        do ista=1,nstas
          Tout(ista)=stadata(istaxgrd,ista)
        enddo
        ierr=nf_put_vara_FTYPE(ncidsta,staXgrd,start,count,
     &                         Tout)
        if (ierr .ne. nf_noerr) then
          vinfo='Xgrid'
          lvar=lenstr(vinfo)
          write(stdout,1) vinfo(1:lvar), record, ierr,
     &                   nf_strerror(ierr) MYID
          goto 99                                           !--> ERROR
        endif
        do ista=1,nstas
          Tout(ista)=stadata(istaygrd,ista)
        enddo
        ierr=nf_put_vara_FTYPE(ncidsta,staYgrd,start,count,
     &                           Tout)
        if (ierr .ne. nf_noerr) then
          vinfo='Ygrid'
          lvar=lenstr(vinfo)
          write(stdout,1) vinfo(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
          goto 99                                           !--> ERROR
        endif
c       endif ! iic=0
# ifdef SOLVE3D
!
!  Stations S or depth
!
#  ifdef ALL_SIGMA
      do ista=1,nstas
       do k=1,N
        ToutN(k,ista)=staSigm(istadpt,ista,k)
       enddo
      enddo
      ierr=nf_put_vara_FTYPE(ncidsta,staDepth,startN,countN,
     &                         ToutN)
      if (ierr .ne. nf_noerr) then
        vinfo='Depth'
        lvar=lenstr(vinfo)
        write(stdout,1) vinfo(1:lvar), record, ierr,
     &                  nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif
#  else
c      if(iic.eq.0) then      ! store only first value
       do ista=1,nstas
         Tout(ista)=stadata(istadpt,ista)
       enddo
       ierr=nf_put_vara_FTYPE(ncidsta,staDepth,start,count,
     &                          Tout)
       if (ierr .ne. nf_noerr) then
         vinfo='Depth'
         lvar=lenstr(vinfo)
         write(stdout,1) vinfo(1:lvar), record, ierr,
     &                   nf_strerror(ierr) MYID
         goto 99                                           !--> ERROR
       endif
c      endif ! iic=0
      do ista=1,nstas
        Tout(ista)=stainfo(istazgrd,ista)
      enddo
      ierr=nf_put_vara_FTYPE(ncidsta,staZgrd,start,count,
     &                         Tout)
      if (ierr .ne. nf_noerr) then
        vinfo='Zgrid'
        lvar=lenstr(vinfo)
        write(stdout,1) vinfo(1:lvar), record, ierr,
     &                nf_strerror(ierr) MYID
        goto 99                                           !--> ERROR
      endif
#  endif
# endif /* SOLVE3D */
      endif ! indxstaGrd
!
! Output station data
!
# ifdef SOLVE3D
      if (wrtsta(indxstaTemp)) then
!
! Station temperature data
!
#  ifdef ALL_SIGMA
      do ista=1,nstas
       do k=1,N
        ToutN(k,ista)=staSigm(istatem,ista,k)
       enddo
      enddo
      ierr=nf_put_vara_FTYPE(ncidsta,staTemp,startN,countN,
     &                         ToutN)
#  else
       do ista=1,nstas
         Tout(ista)=stadata(istatem,ista)
       enddo
       ierr=nf_put_vara_FTYPE(ncidsta,staTemp,start,count,
     &                          Tout)
#  endif
       if (ierr .ne. nf_noerr) then
         vinfo='Temp'
         lvar=lenstr(vinfo)
         write(stdout,1) vinfo(1:lvar), record, ierr,
     &                   nf_strerror(ierr) MYID
         goto 99                                           !--> ERROR
       endif
      endif
#  ifdef SALINITY
      if (wrtsta(indxstaSalt)) then
!
! salinity at station position
!
#  ifdef ALL_SIGMA
      do ista=1,nstas
       do k=1,N
        ToutN(k,ista)=staSigm(istasal,ista,k)
       enddo
      enddo
      ierr=nf_put_vara_FTYPE(ncidsta,staSal,startN,countN,
     &                         ToutN)
#  else
       do ista=1,nstas
         Tout(ista)=stadata(istasal,ista)
       enddo
       ierr=nf_put_vara_FTYPE(ncidsta,staSal,start,count,
     &                          Tout)
#  endif
       if (ierr .ne. nf_noerr) then
         vinfo='Salt'
         lvar=lenstr(vinfo)
         write(stdout,1) vinfo(1:lvar), record, ierr,
     &                   nf_strerror(ierr) MYID
         goto 99                                           !--> ERROR
       endif
      endif
#  endif /* SALINITY */
      if (wrtsta(indxstaRho)) then
!
! density at station position
!
#  ifdef ALL_SIGMA
      do ista=1,nstas
       do k=1,N
        ToutN(k,ista)=staSigm(istaden,ista,k)
       enddo
      enddo
      ierr=nf_put_vara_FTYPE(ncidsta,staDen,startN,countN,
     &                         ToutN)
#  else
       do ista=1,nstas
         Tout(ista)=stadata(istaden,ista)
       enddo
       ierr=nf_put_vara_FTYPE(ncidsta,staDen,start,count,
     &                          Tout)
#  endif
       if (ierr .ne. nf_noerr) then
         vinfo='Den'
         lvar=lenstr(vinfo)
         write(stdout,1) vinfo(1:lvar), record, ierr,
     &                   nf_strerror(ierr) MYID
         goto 99                                           !--> ERROR
       endif
      endif
# endif /* SOLVE3D */
      if (wrtsta(indxstaVel)) then
!
! write velocity components
!
#  ifdef ALL_SIGMA
      do ista=1,nstas
       do k=1,N
        ToutN(k,ista)=staSigm(istau,ista,k)
       enddo
      enddo
      ierr=nf_put_vara_FTYPE(ncidsta,staU,startN,countN,
     &                         ToutN)
#  else
       do ista=1,nstas
         Tout(ista)=stadata(istau,ista)
       enddo
       ierr=nf_put_vara_FTYPE(ncidsta,staU,start,count,
     &                          Tout)
#  endif
       if (ierr .ne. nf_noerr) then
         vinfo='u'
         lvar=lenstr(vinfo)
         write(stdout,1) vinfo(1:lvar), record, ierr,
     &                   nf_strerror(ierr) MYID
         goto 99                                           !--> ERROR
       endif
#  ifdef ALL_SIGMA
      do ista=1,nstas
       do k=1,N
        ToutN(k,ista)=staSigm(istav,ista,k)
       enddo
      enddo
      ierr=nf_put_vara_FTYPE(ncidsta,staV,startN,countN,
     &                         ToutN)
#  else
       do ista=1,nstas
         Tout(ista)=stadata(istav,ista)
       enddo
       ierr=nf_put_vara_FTYPE(ncidsta,staV,start,count,
     &                          Tout)
#  endif
       if (ierr .ne. nf_noerr) then
         vinfo='v'
         lvar=lenstr(vinfo)
         write(stdout,1) vinfo(1:lvar), record, ierr,
     &                   nf_strerror(ierr) MYID
         goto 99                                           !--> ERROR
       endif
      endif ! output velocity

!
!  PUT HERE OTHE VARIABES LIKE 2D ETC....
!
!


  1   format(/1x, 'WRT_STA ERROR while writing variable ''', A,
     &        ''' into station file.'  /11x, 'Time record:', I6,
     &        3x,'netCDF error code',i4 /11x,'Cause of error: ',
     &                                            A, 3x, A, i4)
      goto 100 
  99  may_day_flag=3
 100  continue


!
! Synchronize netCDF file to disk to allow other processes
! to access data immediately after it is written.
!
# if defined MPI & !defined PARALLEL_FILES
      ierr=nf_close (ncidsta)
      if (nrpfsta.gt.0 .and. record.ge.nrpfsta) ncidsta=-1
# else
      if (nrpfsta.gt.0 .and. record.ge.nrpfsta) then
        ierr=nf_close (ncidsta)
!        write(*,*) 'FLOAT FILE IS CLOSED (XA) '
        ncidsta=-1
      else
        ierr=nf_sync(ncidsta)
      endif
# endif
      if (ierr .eq. nf_noerr) then
        write(stdout,'(6x,A,2(A,I4,1x),A,I3)') 'WRT_STA -- wrote ',
     &            'station history fields into time record =', record, 
     &            '/' ,nrecsta  MYID
      else
        write(stdout,'(/1x,2A/)') 'WRT_STA ERROR: Cannot ',
     &             'synchronize/close station netCDF file.'
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
      subroutine wrt_sta_empty
      return
      end
#endif /* FLOATS */

