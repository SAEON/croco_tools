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
#if defined PSOURCE_NCFILE

      subroutine get_psource
!
!  Read in point or grided sea river runoff the appropriate
!  time from runoff NetCDF file.
!
!
# define PSOURCE_DATA
      implicit none
# include "param.h"
# include "forces.h"
# include "scalars.h"
# include "netcdf.inc"
# include "ncscrum.h"
# include "sources.h"
      real cff
      integer is,i,ierr, lstr,lvar,lenstr, nf_fread, advance_cycle
      integer s(2),c(2)
!
! Initialization: Inquire about the contents of forcing NetCDF file:
!================  variables and dimensions. Check for consistency.
!
      if (may_day_flag.ne.0) return      !-->  EXIT
      if (itriv.eq.0 .or. iic.eq.0) then
        lstr=lenstr(rivname)

c*        call opencdf (rivname,N)
c*        if (may_day_flag.ne.0) return    !-->  EXIT
!
! If not opened yet, open forcing NetCDF file for reading.
! Find and save IDs for relevant variables
!
        if (ncidriv.eq.-1) then
          ierr=nf_open(rivname(1:lstr), nf_nowrite, ncidriv)
          if (ierr. ne. nf_noerr) goto 4                  !--> ERROR
        endif

        ierr=nf_inq_varid (ncidriv, 'riv_time', riv_tid)
        if (ierr .ne. nf_noerr) then
          write(stdout,3) 'riv_time', rivname(1:lstr)
          goto 99                                         !--> ERROR
        endif

        lvar=lenstr(vname(1,indxRIV))
        ierr=nf_inq_varid (ncidriv, vname(1,indxRIV)(1:lvar), riv_id)
        if (ierr .eq. nf_noerr) then
          ierr=nf_inq_varndims (ncidriv, riv_id, i) 
        endif
        if (ierr .ne. nf_noerr) then
          write(stdout,3) vname(1,indxRIV)(1:lvar), rivname(1:lstr)
          goto 99                                         !--> ERROR
        endif

!
! Determine whether there is cycling to reuse the input data and
! find cycling period "riv_cycle", set initial cycling index
! "riv_ncycle" and record index "riv_rec".
! Set initial value for time index "itriv" and both time record
! bounds to large negative artificial values, so that it will
! trigger the logic in reading part below.
!
        call set_cycle (ncidriv,   riv_tid,    ntriv,
     &                  riv_cycle, riv_ncycle, riv_rec)
        if (may_day_flag.ne.0) return    !-->  EXIT
        itriv=2
        riv_time(1)=-1.E+20
        riv_time(2)=-1.E+20
      endif
!
! Reading data from the runoff file:  Get out, if model time is
!======== ==== ==== === ======= =====  already within the interval
! set by the past and future data times. Otherwise flip the time
! index, increment record and cyclin indices and read a new portion
! of data. Repeat it until model time is between the two times from
! data.
!
  1   i=3-itriv
       cff=time+0.5*dt
        if (riv_time(i).le.cff .and. cff.lt.riv_time(itriv)) return
        ierr=advance_cycle (riv_cycle, ntriv, riv_ncycle, riv_rec)
        if (ierr .ne. 0) goto 7                           !--> ERROR

        ierr=nf_get_var1_FTYPE (ncidriv, riv_tid, riv_rec, cff)

        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'riv_time', riv_rec
          goto 99                                         !--> ERROR
        endif
        riv_time(i)=cff*day2sec+riv_cycle*riv_ncycle
        if (riv_time(itriv).eq.-1.E+20) riv_time(itriv)=riv_time(i)

          s=(/ riv_rec,1 /)
          c=(/ 1,Nsrc /)
          ierr=nf_get_vara_FTYPE(ncidriv,riv_id,s,c,rivg(1:Nsrc,i))
        if (ierr .ne. nf_noerr) then
          write(stdout,6) 'RIV', riv_rec
          goto 99                                         !--> ERROR
        endif     
        
       itriv=i
       MPI_master_only write(stdout,'(6x,A,1x,A,1x,g12.4,1x,I4)') 
     &              'GET_PSOURCE--',
     &              'Read RIV fields                for time =', cff 

!        OPEN(UNIT=114,file='roms_runoff.out',FORM='FORMATTED',
!     &      access='append',status='OLD')
!	WRITE(114,FMT=110)cff,riv_rec,(rivg(is,i),is=1,Nsrc)
!110	FORMAT(g12.4,' ',I4,5(' ',F10.3)/)
!	CLOSE(114)

#ifdef MPI
     &                                                      , mynode
#endif
      if (ntriv.gt.1) goto 1
      if (ntriv.eq.1) return
!
! Sort out error messages: The following portion of the code is
!===== === ===== ========= not accessed unless something goes wrong.
!
  3   format(/,' GET_PSOURCE - ERROR: unable to find forcing variable',
     &                   ': ',a,/,11x,'in runoff NetCDF file: ',a)
  4   write(stdout,5) rivname(1:lstr)
  5   format(/,' GET_PSOURCE - ERROR: unable to open runoff NetCDF ',
     &                                                  'file: ',a)
      goto 99
  6   format(/,' GET_PSOURCE - ERROR while reading variable: ',a,2x,
     &                                      ' at TIME index = ',i4)
  7   write(stdout,8) riv_rec, ntriv, rivname(1:lstr), tdays,
     &                                      riv_time(itriv)*sec2day
  8   format(/,' GET_PSOURCE - ERROR: requested time record ',I4,
     &       1x,'exeeds the last available',  /,  11x,'record ',I4,
     &       1x,'in runoff NetCDF file: ', a, /,   11x,'TDAYS = ',
     &                g12.4,2x,'last available RIV_TIME = ',g12.4)
  99  may_day_flag=2
      return
      end



      subroutine set_psource_tile(Istr,Iend,Jstr,Jend,Nsrc)
!
! Set-up river runoff data for current tile.
!
      implicit none
      integer Nsrc,Istr,Iend,Jstr,Jend,i, it1,it2
      real cff, cff1,cff2
# define RIV_DATA
# include "param.h"
# include "forces.h"
# include "scalars.h"
# include "compute_extended_bounds.h"
!
      it1=3-itriv
      it2=itriv
      cff=time+0.5*dt
      cff1=riv_time(it2)-cff
      cff2=cff-riv_time(it1)
!
!  Load time invariant RIV data.
!
      if (riv_cycle.lt.0.) then
        if (FIRST_TIME_STEP) then
            do i=1,Nsrc
                riv(i)=rivg(i,itriv)
            enddo
        endif
!
!  Time-interpolate RIV from point data.
!  Check that for the next time step [when time=time+dt] time+dt 
!  is still between riv_time(it1) and riv_time(it2); and if not,
!  set synchro_flag top signal that the new forcing data should be
!  read from the netCDF input file (master thread only).
!
      elseif (cff1.ge.0. .and. cff2.ge.0.) then
        if (ZEROTH_TILE .and. cff1.lt.dt) synchro_flag=.TRUE.
        cff=1./(cff1+cff2)
        cff1=cff1*cff
        cff2=cff2*cff
            do i=1,Nsrc
              riv(i)=cff1*rivg(i,it1)+cff2*rivg(i,it2)
            enddo
!
!  Unable to set-up RIV:
!  Complain about the error and signal to quit.      
!
      else
        if (ZEROTH_TILE) then
          write(stdout,1) 'riv_time', tdays, riv_time(it2)*sec2day
  1       format(/,' SET_RIV - current model time exceeds ending',
     &           1x,'value for variable: ',a,/,11x,'TDAYS = ',g12.4,
     &           2x,'TEND = ',g12.4)
          may_day_flag=2
        endif
      endif

      return
      end
#else
      subroutine get_riv_empty
      return
      end
#endif /* PSOURCE_NCFILE */
