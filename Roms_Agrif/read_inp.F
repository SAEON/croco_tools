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
                                        ! Read/report model input
      subroutine read_inp (ierr)        ! parameters  from  startup 
                                        ! script file using keywords
                                        ! to recognize variables.
!     implicit none
#include "param.h"
#include "scalars.h"
#include "ncscrum.h"
#include "sediment.h"
#ifdef FLOATS
# include "ncscrum_floats.h"
#endif
#ifdef STATIONS
# include "nc_sta.h"
#endif
#ifdef PSOURCE
# include "sources.h"
#endif
#ifdef ONLINE
# include "online.h"
#endif
#ifdef MPI
      include 'mpif.h'
#endif

      integer kwsize, testunit, input
      parameter (kwsize=32, testunit=40, input=15)
      character end_signal*3, keyword*32, fname*64
      parameter (end_signal='end')
      integer ierr, iargc, is,ie, kwlen, lstr, lenstr
#ifdef SOLVE3D
     &                                       , itrc
#endif
#ifdef AGRIF
      integer Agrif_lev_sedim
#endif
!
! Use pre-set default startup filename for known applications,
! or get it as an argument from command line via iargc-getarg

! (override default). NOTE: The usage of the executable should
! be either 
!           roms
!        or
!           roms startup_file_name
!
! WITHOUT the UNIX redirection (<): roms<startup_file like it
! used to be.
!
#if defined BASIN
      fname='TEST_CASES/roms.in.Basin'
#elif defined CANYON_A
      fname='TEST_CASES/roms.in.Canyon_A'
#elif defined CANYON_B
      fname='TEST_CASES/roms.in.Canyon_B'
#elif defined EQUATOR
      fname='TEST_CASES/roms.in.Equator'
#elif defined GRAV_ADJ
      fname='TEST_CASES/roms.in.Grav_adj'
#elif defined INNERSHELF
      fname='TEST_CASES/roms.in.Innershelf'
#elif defined INTERNAL
      fname='TEST_CASES/roms.in.Internal'
#elif defined OVERFLOW
      fname='TEST_CASES/roms.in.Overflow'
#elif defined RIVER
      fname='TEST_CASES/roms.in.River'
#elif defined SEAMOUNT
      fname='TEST_CASES/roms.in.Seamount'
#elif defined SHELFRONT
      fname='TEST_CASES/roms.in.Shelfront'
#elif defined SOLITON
      fname='TEST_CASES/roms.in.Soliton'
#elif defined UPWELLING
      fname='TEST_CASES/roms.in.Upwelling'
#elif defined VORTEX
      fname='TEST_CASES/roms.in.Vortex'
#elif defined JET
      fname='TEST_CASES/roms.in.Jet'
#elif defined REGIONAL
      fname='roms.in'
#else
      fname='no_startup_file'
#endif
#ifdef MPI
      if (mynode.eq.0 .and. iargc().GT.0) call getarg(1,fname) 
      call MPI_Bcast(fname,64,MPI_BYTE, 0, MPI_COMM_WORLD,ierr)
#else
      if (iargc().eq.1) call getarg(1,fname)
#endif
!
! if child grid, use an input name: fname.1, .2, .3, ...
!
#ifdef AGRIF
      if (.Not.Agrif_Root()) then
        lstr=lenstr(fname)
#ifdef AGRIF_ADAPTIVE
        fname=fname(1:lstr) / / '.1'  
#else        
        fname=fname(1:lstr) / / '.' / / Agrif_Cfixed()
#endif 
        lstr=lenstr(fname)
      endif
#endif
!
      wrthis(indxTime)=.false.
#ifdef AVERAGES
      wrtavg(indxTime)=.false.
#endif

#ifdef DIAGNOSTICS_TS
      wrtdia3D(NT+1)=.false.
# ifdef AVERAGES
      wrtdia3D_avg(NT+1)=.false.
# endif
# ifdef DIAGNOSTICS_TS_MLD
      wrtdia2D(NT+1)=.false.
#  ifdef AVERAGES
      wrtdia2D_avg(NT+1)=.false.
#  endif
# endif
#endif
#ifdef DIAGNOSTICS_UV
      wrtdiaM(3)=.false.
#ifdef AVERAGES
      wrtdiaM_avg(3)=.false.
#endif
#endif
!
! Read in keyword: keep trying, until keyword is found.
! ==== == ======== ==== ======= ===== ======= == ======
!
      ierr=0    ! <-- reset error counter
      call setup_kwds (ierr)
      open (input,file=fname,status='old',form='formatted',err=97)
   1  keyword='                                '
      read(input,'(A)',err=1,end=99) keyword
      if (ichar(keyword(1:1)).eq.33) goto 1
      is=1
   2  if (is.eq.kwsize) then
        goto 1
      elseif (keyword(is:is).eq.' ') then
        is=is+1
        goto 2
      endif
      ie=is
   3  if (keyword(ie:ie).eq.':') then
        keyword(ie:ie)=' '
        goto 4           !--> recognized keyword.               
      elseif (keyword(ie:ie).ne.' ' .and. ie.lt.kwsize) then 
        ie=ie+1
        goto 3
      endif
      goto 1
   4  kwlen=ie-is
      if (is.gt.1) keyword(1:kwlen)=keyword(is:is+kwlen-1)
!
! Read input parameters according to the keyword:
! ==== ===== ========== ========= == === ========
!
! Title
!
      if (keyword(1:kwlen).eq.'title') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,'(A)',err=95) title
        lstr=lenstr(title)
        MPI_master_only write(stdout,'(/1x,A)') title(1:lstr)
!
#ifdef START_DATE
!
! Start date
!
      elseif (keyword(1:kwlen).eq.'start_date') then
         call cancel_kwd (keyword(1:kwlen), ierr)
         read(input,'(A)',err=95) start_date
         lstr=lenstr(start_date)
         MPI_master_only write(stdout,'(/2x,A,1x,A/)') 
     &   'Start date:', start_date(1:lstr)
#endif
!     
! Time-stepping parameters
!
      elseif (keyword(1:kwlen).eq.'time_stepping') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) ntimes,dt,ndtfast, ninfo
        MPI_master_only write(stdout,
     & '(I10,2x,A,1x,A /F10.2,2x,A,2(/I10,2x,A,1x,A)/F10.4,2x,A)'
     &  ) ntimes,  'ntimes   Total number of timesteps for',
     &                                            '3D equations.',
     &    dt,      'dt       Timestep [sec] for 3D equations',
     &    ndtfast, 'ndtfast  Number of 2D timesteps within each',
     &                                                 '3D step.',
     &    ninfo,   'ninfo    Number of timesteps between',
     &                                     'runtime diagnostics.'
        dtfast=dt/float(ndtfast)     ! set barotropic time step.

        if (NWEIGHT.lt.(2*ndtfast-1)) then
          write(stdout,'(a,i3)')
     &    ' Error - Number of 2D timesteps (2*ndtfast-1): ',
     &    2*ndtfast-1
          write(stdout,'(a,i3)')
     &    '           exceeds barotopic weight dimension: ',NWEIGHT
          goto 95
        endif

#ifdef SOLVE3D
!
! Vertical S-coordinates parameters.
!
      elseif (keyword(1:kwlen).eq.'S-coord') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) theta_s, theta_b, Tcline
        MPI_master_only write(stdout,
     &                        '(3(1pe10.3,2x,A,1x,A/),32x,A)')
     &    theta_s, 'theta_s  S-coordinate surface control',
     &                                               'parameter.',
     &    theta_b, 'theta_b  S-coordinate bottom control',
     &                                               'parameter.',
     &    Tcline,  'Tcline   S-coordinate surface/bottom layer',
     &  'width used in', 'vertical coordinate stretching, meters.'
#endif
!
! Initial conditions file name. Check its availability (in the case
! of analytical initial conditions and nrrec=0 initial conditions are
! created internally and no file is needed).
!
      elseif (keyword(1:kwlen).eq.'initial') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) nrrec
#ifdef ANA_INITIAL
        if (nrrec.gt.0) then 
#endif
          read(input,'(A)',err=95) fname
          lstr=lenstr(fname)
#if defined MPI && defined PARALLEL_FILES
          call insert_node (fname, lstr, mynode, NNODES, ierr)
#endif
          open (testunit, file=fname(1:lstr), status='old', err=97)
          close(testunit)
          ininame=fname(1:lstr)
          MPI_master_only write(stdout,'(1x,A,2x,A,4x,A,I3)')
     &     'Initial State File:', ininame(1:lstr), 'Record:',nrrec
#ifdef ANA_INITIAL
        endif
#endif
#ifndef ANA_GRID
!
! Grid file name. Check its availability.
!
      elseif (keyword(1:kwlen).eq.'grid') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,'(A)',err=95) fname 
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
        open(testunit,file=fname(1:lstr), status='old', err=97)
        close(testunit)
        grdname=fname(1:lstr)
        MPI_master_only write(stdout,'(10x,A,2x,A)')
     &                   'Grid File:', grdname(1:lstr)
#endif
#if      (!defined SOLVE3D && !defined ANA_SMFLUX) \
      || (defined TIDES) \
      || (defined SOLVE3D && (\
           (!defined ANA_STFLUX && !defined BULK_FLUX) \
        ||  !defined ANA_BTFLUX   \
        || (defined BBL  && !defined ANA_BSEDIM && !defined SEDIMENT) \
        || (defined QCORRECTION && !defined ANA_SST)   \
        || (defined BBL         && !defined ANA_WWAVE) \
        || (defined SALINITY    && !defined ANA_SSFLUX && \
            !defined BULK_FLUX  && !defined BULK_EP) \
        || (defined SALINITY    && defined QCORRECTION && \
             defined SFLX_CORR   && !defined ANA_SSS) \
        || (defined LMD_SKPP    && \
            !defined ANA_SRFLUX && !defined BULK_FLUX) \
        || (defined BULK_FLUX && \
            (!defined BULK_SMFLUX || !defined BULK_EP)) \
                               )\
         )
!
!
! Forcing file name. Check its availability.
!
      elseif (keyword(1:kwlen).eq.'forcing') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,'(A)',err=95) fname
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
        open (testunit, file=fname(1:lstr), status='old', err=97)
        close(testunit)
        frcname=fname(1:lstr)
        MPI_master_only write(stdout,'(2x,A,2x,A)')
     &             'Forcing Data File:', frcname(1:lstr)
#endif
!
! Biology forcing: iron dust deposition. 
!
#if defined BIOLOGY && defined PISCES
      elseif (keyword(1:kwlen).eq.'biology') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,'(A)',err=95) fname
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
c        open (testunit, file=fname(1:lstr), status='old', err=97)
c        close(testunit)
        bioname=fname(1:lstr)
        MPI_master_only write(stdout,'(2x,A,2x,A)')
     &             'Biology Forcing Data File:', bioname(1:lstr)
#endif
!
! Bulk file name. Check its availability.
!
#if defined BULK_FLUX
# ifndef ONLINE /* ONLINE FORCING */
        elseif (keyword(1:kwlen).eq.'bulk_forcing') then
          call cancel_kwd (keyword(1:kwlen), ierr)
          read(input,'(A)',err=95) fname
          lstr=lenstr(fname)
#  if defined MPI && defined PARALLEL_FILES
          call insert_node (fname, lstr, mynode, NNODES, ierr)
#  endif
          open (testunit, file=fname(1:lstr), status='old', err=97)
          close(testunit)
          bulkname=fname(1:lstr)
          MPI_master_only write(stdout,'(2x,A,2x,A)')
     &               '   Bulk Data File:', bulkname(1:lstr)
# endif  /* ONLINE FORCING */
#endif
#if (defined TCLIMATOLOGY  && !defined ANA_TCLIMA) || \
      (defined ZCLIMATOLOGY  && !defined ANA_SSH) || \
      (defined M2CLIMATOLOGY && !defined ANA_M2CLIMA) || \
      (defined M3CLIMATOLOGY && !defined ANA_M3CLIMA)
!
! Climatology file name. Check availability.
!
      elseif (keyword(1:kwlen).eq.'climatology') then
        call cancel_kwd (keyword(1:kwlen), ierr)
# ifdef AGRIF
       if (Agrif_Root()) then
# endif
        read(input,'(A)',err=95) fname 
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
        open (testunit, file=fname(1:lstr), status='old', err=97)
        close(testunit)
        clmname=fname(1:lstr)
        MPI_master_only write(stdout,'(3x,A,2x,A)')
     &            'Climatology File:', clmname(1:lstr)
# ifdef AGRIF
       endif
# endif
#endif
#if !defined ANA_BRY && defined FRC_BRY
!
! Boundary file name. Check availability.
!
        elseif (keyword(1:kwlen).eq.'boundary') then
          call cancel_kwd (keyword(1:kwlen), ierr)
#  ifdef AGRIF
       if (Agrif_Root()) then
#  endif
          read(input,'(A)',err=95) fname
          lstr=lenstr(fname)
#  if defined MPI && defined PARALLEL_FILES
          call insert_node (fname, lstr, mynode, NNODES, ierr)
#  endif
          open (testunit, file=fname(1:lstr), status='old', err=97)
          close(testunit)
          bry_file=fname(1:lstr)
          MPI_master_only write(stdout,'(6x,A,2x,A)')
     &          'Boundary File:', bry_file(1:lstr)
#  ifdef AGRIF
       endif
#  endif
#endif
!
! Restart file name.
!
      elseif (keyword(1:kwlen).eq.'restart') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) nrst, nrpfrst
        read(input,'(A)',err=95)  fname 
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
        rstname=fname(1:lstr)
        MPI_master_only write(stdout,
     &             '(7x,A,2x,A,4x,A,I6,4x,A,I4)')
     &             'Restart File:', rstname(1:lstr),
     &             'nrst =', nrst, 'rec/file: ', nrpfrst 
!
! History file name.
!
      elseif (keyword(1:kwlen).eq.'history') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) ldefhis, nwrt, nrpfhis
        read(input,'(A)',err=95) fname
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
        hisname=fname(1:lstr)
        MPI_master_only write(stdout,
     &             '(7x,A,2x,A,2x,A,1x,L1,2x,A,I4,2x,A,I3)')
     &       'History File:', hisname(1:lstr),  'Create new:',
     &       ldefhis, 'nwrt =', nwrt, 'rec/file =', nrpfhis
#ifdef AVERAGES
!
! Averages file name.
!
      elseif (keyword(1:kwlen).eq.'averages') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) ntsavg, navg, nrpfavg
        read(input,'(A)',err=95) fname
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
        avgname=fname(1:lstr)
        MPI_master_only write(stdout,
     &         '(2(I10,2x,A,1x,A/32x,A/),6x,A,2x,A,1x,A,I3)')
     &      ntsavg, 'ntsavg      Starting timestep for the',
     &         'accumulation of output', 'time-averaged data.',
     &      navg,   'navg        Number of timesteps between',
     &     'writing of time-averaged','data into averages file.',
     &     'Averages File:', avgname(1:lstr),
     &     'rec/file =', nrpfavg
#endif

#if defined DIAGNOSTICS_TS
!
! Diagnostics file name.
!
      elseif (keyword(1:kwlen).eq.'diagnostics') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) ldefdia, nwrtdia, nrpfdia
        read(input,'(A)',err=95) fname
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
        dianame=fname(1:lstr)
        MPI_master_only write(stdout,
     &    '(9x,A,2x,A/,32x,A,1x,L1,2x,A,I4,2x,A,I3)')
     &    'Tracer Diag File:',dianame(1:lstr),'Create new:',
     &    ldefdia,'nwrt =',nwrtdia,'rec/file =',nrpfdia
!
# ifdef AVERAGES
      elseif (keyword(1:kwlen).eq.'diag_avg') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) ldefdia_avg, ntsdia_avg, nwrtdia_avg,
     &                                                nrpfdia_avg
        read(input,'(A)',err=95) fname
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
        dianame_avg=fname(1:lstr)
        MPI_master_only write(stdout,
     &    '(5x,A,2x,A/,32x,A,1x,L1,2x,A,I4,2x,A,I3,/32x,A,I10)')
     &    'Tracer AVG Diag File:',dianame_avg(1:lstr),'Create new:',
     &    ldefdia_avg,'nwrt =',nwrtdia_avg,'rec/file =',nrpfdia_avg,
     &    'Starting timestep = ',ntsdia_avg
# endif
#endif /* DIAGOSTICS_TS */
#if defined DIAGNOSTICS_UV
!
! Diagnostics Momentum file name.
!
      elseif (keyword(1:kwlen).eq.'diagnosticsM') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) ldefdiaM, nwrtdiaM, nrpfdiaM
        read(input,'(A)',err=95) fname
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
        dianameM=fname(1:lstr)
        MPI_master_only write(stdout,
     &    '(5x,A,2x,A/,32x,A,1x,L1,2x,A,I4,2x,A,I3)')
     &    'Momentum Diag File:', dianameM(1:lstr),  'Create new:',
     &    ldefdiaM, 'nwrt =', nwrtdiaM, 'rec/file =', nrpfdiaM
!
# ifdef AVERAGES
      elseif (keyword(1:kwlen).eq.'diagM_avg') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) ldefdiaM_avg, ntsdiaM_avg, nwrtdiaM_avg,
     &                                                  nrpfdiaM_avg
        read(input,'(A)',err=95) fname
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
        dianameM_avg=fname(1:lstr)
        MPI_master_only write(stdout,
     &    '(5x,A,2x,A/,32x,A,1x,L1,2x,A,I4,2x,A,I3,/32x,A,I10)')
     &    'Momentum AVG Diag File:',dianameM_avg(1:lstr),'Create new:',
     &    ldefdiaM_avg,'nwrt =',nwrtdiaM_avg,'rec/file =',nrpfdiaM_avg,
     &    'Starting timestep = ',ntsdiaM_avg

# endif
#endif /*DIAGNOSTICS_UV */
#ifdef DIAGNOSTICS_BIO
!
! Diagnostics Biology file name.
!
      elseif (keyword(1:kwlen).eq.'diagnostics_bio') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) ldefdiabio, nwrtdiabio, nrpfdiabio
        read(input,'(A)',err=95) fname
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
        dianamebio=fname(1:lstr)
        MPI_master_only write(stdout,
     &    '(8x,A,2x,A/,32x,A,1x,L1,2x,A,I4,2x,A,I3)') 
     &    'Biology Diag File:', dianamebio(1:lstr),  'Create new:',
     &    ldefdiabio, 'nwrt =', nwrtdiabio, 'rec/file =', nrpfdiabio
!
# ifdef AVERAGES
      elseif (keyword(1:kwlen).eq.'diagbio_avg') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) ldefdiabio_avg, ntsdiabio_avg,
     &                       nwrtdiabio_avg, nrpfdiabio_avg
        read(input,'(A)',err=95) fname
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
        dianamebio_avg=fname(1:lstr)
        MPI_master_only write(stdout,
     &    '(4x,A,2x,A/,32x,A,1x,L1,2x,A,I4,2x,A,I3,/32x,A,I10)')
     &    'Biology AVG Diag File:',dianamebio_avg(1:lstr),'Create new:',
     &    ldefdiabio_avg,'nwrt =',nwrtdiabio_avg,'rec/file =',
     &    nrpfdiabio_avg,'Starting timestep = ',ntsdiabio_avg
# endif
#endif /* DIAGOSTICS_BIO */

#ifdef FLOATS
!
! Floats file name.
!
      elseif (keyword(1:kwlen).eq.'floats') then
        call cancel_kwd (keyword(1:kwlen), ierr)
#ifdef AGRIF
        if (Agrif_Root()) then
#endif
          read(input,*,err=95) ldefflt, nflt, nrpfflt
          read(input,'(A)',err=95) fposnam
          read(input,'(A)',err=95) fname
          lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
          call insert_node (fposnam, lstr, mynode, NNODES, ierr)
# endif
          fltname=fname(1:lstr)
          MPI_master_only write(stdout,
     &              '(9x,A,2x,A,2x,A,1x,L1,2x,A,I4,2x,A,I3)')
     &        'Float File:',fltname(1:lstr),  'Create new:',
     &        ldefflt, 'nflt =', nflt, 'rec/file =', nrpfflt
#ifdef AGRIF
        else
          ldefflt=Agrif_Parent("ldefflt",ldefflt)
          nflt=Agrif_Parent("nflt",nflt)
          nrpfflt=Agrif_Parent("nrpfflt",nrpfflt)
        endif
#endif
#endif /* FLOATS */

#ifdef STATIONS
!
! Stations file name.
!
      elseif (keyword(1:kwlen).eq.'stations') then
        call cancel_kwd (keyword(1:kwlen), ierr)
# ifdef AGRIF
        if (Agrif_Root()) then
# endif
          read(input,*,err=95) ldefsta, nsta, nrpfsta
          read(input,'(A)',err=95) staposname
          read(input,'(A)',err=95) fname
          lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
          call insert_node (staposname, lstr, mynode, NNODES, ierr)
# endif
          staname=fname(1:lstr)
          MPI_master_only write(stdout,
     &              '(9x,A,2x,A,2x,A,1x,L1,2x,A,I4,2x,A,I3)')
     &        'Station File:',staname(1:lstr),  'Create new:',
     &        ldefsta, 'nsta =', nsta, 'rec/file =', nrpfsta
# ifdef AGRIF
        else
          ldefsta=Agrif_Parent("ldefsta",ldefsta)
          nsta=Agrif_Parent("nsta",nsta)
          nrpfsta=Agrif_Parent("nrpfsta",nrpfsta)
        endif
# endif
#endif /* STATIONS */

#ifdef ASSIMILATION
!
! Assimilation input/output file names.
!
      elseif (keyword(1:kwlen).eq.'assimilation') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,'(A)',err=95) aparnam
        read(input,'(A)',err=95) assname
        fname=aparnam
        lstr=lenstr(aparnam)
        open (testunit,file=aparnam(1:lstr),status='old',err=97)
        close(testunit)
        MPI_master_only write(stdout,'(1x,A,2x,A)')
     &         'Assimilation Parameters File:', aparnam(1:lstr)
        fname=assname
        lstr=lenstr(assname)
        open (testunit,file=assname(1:lstr),status='old',err=97)
        close(testunit)
        MPI_master_only write(stdout,'(12x,A,2x,A)')
     &                    'Assimilation File:', assname(1:lstr)
#endif
!
! Switches for fields to be saved into history file.
!
      elseif (keyword(1:kwlen).eq.'primary_history_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) wrthis(indxZ),  wrthis(indxUb)
     &                                       ,  wrthis(indxVb)
#ifdef SOLVE3D
     &                    ,  wrthis(indxU),  wrthis(indxV)
     &                    , (wrthis(itrc), itrc=indxT,indxT+NT-1) 
#endif
        if ( wrthis(indxZ) .or. wrthis(indxUb) .or. wrthis(indxVb)
#ifdef SOLVE3D
     &                        .or. wrthis(indxU) .or. wrthis(indxV)
#endif
     &     ) wrthis(indxTime)=.true.

        MPI_master_only write(stdout,'(/1x,A,5(/6x,l1,2x,A,1x,A))')
     &    'Fields to be saved in history file: (T/F)'
     &    , wrthis(indxZ),  'write zeta ', 'free-surface.'
     &    , wrthis(indxUb), 'write UBAR ', '2D U-momentum component.'
     &    , wrthis(indxVb), 'write VBAR ', '2D V-momentum component.'
#ifdef SOLVE3D
     &    , wrthis(indxU),  'write U    ', '3D U-momentum component.'
     &    , wrthis(indxV),  'write V    ', '3D V-momentum component.'
        do itrc=1,NT
          if (wrthis(indxT+itrc-1)) wrthis(indxTime)=.true.
          MPI_master_only write(stdout, '(6x,L1,2x,A,I2,A,I2,A)')
     &                     wrthis(indxT+itrc-1), 'write T(', itrc,
     &                              ')  Tracer of index', itrc,'.'
        enddo

      elseif (keyword(1:kwlen).eq.'auxiliary_history_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) 
     &           wrthis(indxR),  wrthis(indxO)
     &        ,  wrthis(indxW),  wrthis(indxAkv),  wrthis(indxAkt)
# ifdef SALINITY
     &                                          ,  wrthis(indxAks)
# endif
# ifdef LMD_SKPP
     &                                          ,  wrthis(indxHbl)
# endif
# ifdef LMD_BKPP
     &                                          ,  wrthis(indxHbbl)
# endif
     &                                          ,  wrthis(indxBostr)
     &                                          ,  wrthis(indxWstr)
     &                                          ,  wrthis(indxUWstr)
     &                                          ,  wrthis(indxVWstr)
     &                                          ,  wrthis(indxShflx)
# ifdef SALINITY
     &                                          ,  wrthis(indxSwflx)
# endif
     &                                          ,  wrthis(indxShflx_rsw)
# ifdef BULK_FLUX
     &                                          , wrthis(indxShflx_rlw)
     &                                          , wrthis(indxShflx_lat)
     &                                          , wrthis(indxShflx_sen)
# endif
# if defined BIOLOGY && !defined PISCES
     &                                          ,  wrthis(indxHel)
#  ifdef BIO_NChlPZD
     &                                          ,  wrthis(indxChC)
#   ifdef OXYGEN
     &                                          ,  wrthis(indxU10)
     &                                          ,  wrthis(indxKvO2)
     &                                          ,  wrthis(indxO2sat)
#   endif
#  endif
# endif



        if ( wrthis(indxR) .or. wrthis(indxO) .or. wrthis(indxW)
     &                   .or. wrthis(indxAkv) .or. wrthis(indxAkt)
# ifdef SALINITY
     &                                        .or. wrthis(indxAks)
# endif
# ifdef LMD_SKPP
     &                                        .or. wrthis(indxHbl)
# endif
# ifdef LMD_BKPP
     &                                        .or. wrthis(indxHbbl)
# endif
     &                                        .or. wrthis(indxBostr)
     &                                        .or. wrthis(indxWstr)
     &                                        .or. wrthis(indxUWstr)
     &                                        .or. wrthis(indxVWstr)
     &                                        .or. wrthis(indxShflx)
# ifdef SALINITY
     &                                        .or. wrthis(indxSwflx)
# endif
     &                                        .or. wrthis(indxShflx_rsw)
# if defined BULK_FLUX
     &                                        .or. wrthis(indxShflx_rlw)
     &                                        .or. wrthis(indxShflx_lat)
     &                                        .or. wrthis(indxShflx_sen)
# endif
# if defined BIOLOGY && !defined PISCES
     &                                        .or. wrthis(indxHel)
#  ifdef BIO_NChlPZD
     &                                        .or. wrthis(indxChC)
#   ifdef OXYGEN
     &                                        .or. wrthis(indxU10)
     &                                        .or. wrthis(indxKvO2)
     &                                        .or. wrthis(indxO2sat)
#   endif
#  endif
# endif
     &     ) wrthis(indxTime)=.true.

#  ifdef VIS_COEF_3D
        wrthis(indxVisc)=.true.
#  endif
#  ifdef DIF_COEF_3D
        wrthis(indxDiff)=.true.
#  endif

        MPI_master_only write(stdout,'(8(/6x,l1,2x,A,1x,A))')
     &    wrthis(indxR),     'write RHO  ', 'Density anomaly.'
     &  , wrthis(indxO),    'write Omega', 'Omega vertical velocity.'
     &  , wrthis(indxW),    'write W    ', 'True vertical velocity.'
     &  , wrthis(indxAkv), 'write Akv  ', 'Vertical viscosity.'
     &  , wrthis(indxAkt),  'write Akt  ',
     &                      'Vertical diffusivity for temperature.'
# ifdef SALINITY
     &  , wrthis(indxAks), 'write Aks  ',
     &                         'Vertical diffusivity for salinity.'
# endif
# ifdef LMD_SKPP
     &  , wrthis(indxHbl),  'write Hbl  ',
     &                         'Depth of KPP-model boundary layer.'
# endif
# ifdef LMD_BKPP
     &  , wrthis(indxHbbl),  'write Hbbl  ',
     &                       'Depth of bottom planetary boundary layer.'
# endif
# ifdef BULK_FLUX
     &  , wrthis(indxShflx_rlw), 'write shflx_rlw [W/m2]', 
     &                                 'Long Wave heat flux.'
     &  , wrthis(indxShflx_lat), 'write shflx_lat [W/m2]',
     &                                 'Latent heat flux.'
     &  , wrthis(indxShflx_sen), 'write shflx_sen [W/m2]',
     &                                 'Sensible heat flux'
# endif
# if defined BIOLOGY && !defined PISCES
     &  , wrthis(indxHel),   'write Hel  ',
     &                          'Depth of the euphotic layer'
#  ifdef BIO_NChlPZD
     &  , wrthis(indxChC),   'write ChC  ',
     &                          'Chlorophyll to Carbon ratio'
#   ifdef OXYGEN
     &  , wrthis(indxU10),   'write u10 ',
     &                          'Wind speed at 10 m height'
     &  , wrthis(indxKvO2),  'write Kv_O2 ',
     &                          'Gas transfer coefficient for O2'
     &  , wrthis(indxO2sat), 'write O2sat ',
     &                          'Saturation concentration of O2'
#   endif
#  endif
# endif
     &  , wrthis(indxBostr), 'write Bostr', 'Bottom Stress.'
     &  , wrthis(indxWstr), 'write Wstress', 'Wind Stress.'
     &  , wrthis(indxUWstr), 'write U-Wstress comp.', 'U-Wind Stress.'
     &  , wrthis(indxVWstr), 'write V-Wstress comp.', 'V-Wind Stress.'
     &  , wrthis(indxShflx), 'write Shflx [W/m2]', 
     &                       'Surface net heat flux'
# ifdef SALINITY
     &  , wrthis(indxSwflx), 'write Swflx [cm/day]', 
     &                       'Surface freshwater flux (E-P)'
# endif
     &  , wrthis(indxShflx_rsw),'write Shflx_rsw [W/m2]', 
     &                          'Short-wave surface radiation'
# ifdef VIS_COEF_3D
     &  , wrthis(indxVisc),  'write Visc3d', 'Horizontal viscosity.'
#  endif
#  ifdef DIF_COEF_3D
     &  , wrthis(indxDiff),  'write Visc3d', 'Horizontal diffusivity.'
#  endif

#endif /* SOLVE3D */
#ifdef AVERAGES
!
! Switches for fields to be saved into averages file.
!
      elseif (keyword(1:kwlen).eq.'primary_averages') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) wrtavg(indxZ),  wrtavg(indxUb)
     &                                    ,  wrtavg(indxVb)
# ifdef SOLVE3D
     &                    ,  wrtavg(indxU),  wrtavg(indxV)
     &                    , (wrtavg(itrc), itrc=indxT,indxT+NT-1)
# endif
        if ( wrtavg(indxZ) .or. wrtavg(indxUb) .or. wrtavg(indxVb)
# ifdef SOLVE3D
     &                     .or. wrtavg(indxU)  .or. wrtavg(indxV)
# endif
     &     ) wrtavg(indxTime)=.true.

        MPI_master_only write(stdout,'(/1x,A,5(/6x,l1,2x,A,1x,A))')
     &  'Fields to be saved in averages file: (T/F)'
     &  , wrtavg(indxZ),  'write zeta ', 'free-surface.'
     &  , wrtavg(indxUb), 'write UBAR ', '2D U-momentum component.'
     &  , wrtavg(indxVb), 'write VBAR ', '2D V-momentum component.'
# ifdef SOLVE3D
     &  , wrtavg(indxU),  'write U    ', '3D U-momentum component.'
     &  , wrtavg(indxV),  'write V    ', '3D V-momentum component.'
        do itrc=1,NT
          if (wrtavg(indxT+itrc-1)) wrtavg(indxTime)=.true.
          MPI_master_only write(stdout,
     &                     '(6x,L1,2x,A,I2,A,2x,A,I2,A)')
     &                      wrtavg(indxT+itrc-1), 'write T(',
     &                      itrc,')', 'Tracer of index', itrc,'.'
        enddo

      elseif (keyword(1:kwlen).eq.'auxiliary_averages') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) wrtavg(indxR), wrtavg(indxO)
     &        ,  wrtavg(indxW),  wrtavg(indxAkv),  wrtavg(indxAkt)
#  ifdef SALINITY
     &                                          ,  wrtavg(indxAks)
#  endif
#  ifdef LMD_SKPP
     &                                          ,  wrtavg(indxHbl)
#  endif
#  ifdef LMD_BKPP
     &                                          ,  wrtavg(indxHbbl)
#  endif
     &                                          ,  wrtavg(indxBostr)
     &                                          ,  wrtavg(indxWstr)
     &                                          ,  wrtavg(indxUWstr)
     &                                          ,  wrtavg(indxVWstr)
     &                                          ,  wrtavg(indxShflx)
# ifdef SALINITY
     &                                          ,  wrtavg(indxSwflx)
# endif
     &                                          ,  wrtavg(indxShflx_rsw)
#  ifdef BULK_FLUX
     &                                          ,  wrtavg(indxShflx_rlw)
     &                                          ,  wrtavg(indxShflx_lat)
     &                                          ,  wrtavg(indxShflx_sen)
#  endif
#  if defined BIOLOGY && !defined PISCES
     &                                          ,  wrtavg(indxHel)
#   ifdef BIO_NChlPZD
     &                                          ,  wrtavg(indxChC)
#    ifdef OXYGEN
     &                                          ,  wrtavg(indxU10)
     &                                          ,  wrtavg(indxKvO2)
     &                                          ,  wrtavg(indxO2sat)
#    endif
#   endif
#  endif

        if ( wrtavg(indxR) .or. wrtavg(indxO) .or. wrtavg(indxW)
     &                   .or. wrtavg(indxAkv) .or. wrtavg(indxAkt)
#  ifdef SALINITY
     &                                        .or. wrtavg(indxAks)
#  endif
#  ifdef LMD_SKPP
     &                                        .or. wrtavg(indxHbl)
#  endif
#  ifdef LMD_BKPP
     &                                        .or. wrtavg(indxHbbl)
#  endif
     &                                        .or. wrtavg(indxBostr)
     &                                        .or. wrtavg(indxWstr)
     &                                        .or. wrtavg(indxUWstr)
     &                                        .or. wrtavg(indxVWstr)
     &                                        .or. wrtavg(indxShflx)
# ifdef SALINITY
     &                                        .or. wrtavg(indxSwflx)
# endif
     &                                        .or. wrtavg(indxShflx_rsw)
     &     
#  if defined BULK_FLUX
     &                                        .or. wrtavg(indxShflx_rlw)
     &                                        .or. wrtavg(indxShflx_lat)
     &                                        .or. wrtavg(indxShflx_sen)
#  endif 
#  if defined BIOLOGY && !defined PISCES
     &                                        .or. wrtavg(indxHel)
#   ifdef BIO_NChlPZD
     &                                        .or. wrtavg(indxChC)
#    ifdef OXYGEN
     &                                        .or. wrtavg(indxU10)
     &                                        .or. wrtavg(indxKvO2)
     &                                        .or. wrtavg(indxO2sat)
#    endif
#   endif
#  endif
     &     ) wrtavg(indxTime)=.true.

#  ifdef VIS_COEF_3D
        wrtavg(indxVisc)=.true.
#  endif
#  ifdef DIF_COEF_3D
        wrtavg(indxDiff)=.true.
#  endif

        MPI_master_only write(stdout,'(8(/6x,l1,2x,A,1x,A))')
     &    wrtavg(indxR),    'write RHO  ', 'Density anomaly'
     &  , wrtavg(indxO),    'write Omega', 'Omega vertical velocity.'
     &  , wrtavg(indxW),    'write W    ', 'True vertical velocity.'
     &  , wrtavg(indxAkv),  'write Akv  ', 'Vertical viscosity'
     &  , wrtavg(indxAkt),  'write Akt  ',
     &                      'Vertical diffusivity for temperature.'
#  ifdef SALINITY
     &  , wrtavg(indxAks),  'write Aks  ',
     &                         'Vertical diffusivity for salinity.'
#  endif
#  ifdef LMD_SKPP
     &  , wrtavg(indxHbl),  'write Hbl  ',
     &                          'Depth of KPP-model boundary layer'
#  endif
#  ifdef LMD_BKPP
     &  , wrtavg(indxHbbl),  'write Hbbl  ',
     &                    'Depth of the bottom planetary boundary layer'
#  endif
#  ifdef BULK_FLUX
     &  , wrtavg(indxShflx_rlw), 'write shflx_rlw [W/m2]', 
     &                                 'Long Wave heat flux.'
     &  , wrtavg(indxShflx_lat), 'write shflx_lat[W/m2] ',
     &                                 'Latente heat flux.'
     &  , wrtavg(indxShflx_sen), 'write shflx_sen [W/m2]',
     &                                 'Sensible heat flux.'
#  endif
# if defined BIOLOGY && !defined PISCES
     &  , wrtavg(indxHel),'write Hel  ',
     &                          'Depth of the euphotic layer'
#   ifdef BIO_NChlPZD
     &  , wrtavg(indxChC),'write ChC  ',
     &                          'Chlorophyll to Carbon ratio'
#    ifdef OXYGEN
     &  , wrtavg(indxU10),'write u10 ',
     &         'Wind speed at 10 m height'
     &  , wrtavg(indxKvO2),'write Kv_O2 ',
     &         'Gas transfer coefficient for O2'
     &  , wrtavg(indxO2sat),'write O2sat ',
     &         'Saturation concentration of O2'
#    endif
#   endif
#  endif
     &  , wrtavg(indxBostr),'write Bostr', 'Bottom Stress.'
     &  , wrtavg(indxWstr),'write Wstr', 'Wind Stress.'
     &  , wrtavg(indxUWstr),'write U-Wstress comp.', 'U-Wind Stress.'
     &  , wrtavg(indxVWstr),'write V-Wstress comp.', 'V-Wind Stress.'
     &  , wrtavg(indxShflx),'write Shflx [W/m2]', 
     &                      'Surface net heat flux.'
#  ifdef SALINITY
     &  , wrtavg(indxSwflx),'write Swflx [cm/day]', 
     &                      'Surface freshwater flux (E-P)'
#  endif
     &  , wrtavg(indxShflx_rsw),'write Shflx_rsw [W/m2]', 
     &                      'Short-wave surface radiation.'
#  ifdef VIS_COEF_3D
     &  , wrtavg(indxVisc),'write visc3d', 'Horizontal viscosity'
#  endif
#  ifdef DIF_COEF_3D
     &  , wrtavg(indxDiff),'write diff3d', 'Horizontal diffusivity'
#  endif
# endif /* SOLVE3D */
#endif /* AVERAGES */


#ifdef FLOATS
!
! Switches for fields to be saved into floats output file.
!
      elseif (keyword(1:kwlen).eq.'float_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
#ifdef AGRIF
        if (Agrif_Root()) then
#endif
          read(input,*,err=95) wrtflt(indxfltGrd), 
     &       wrtflt(indxfltTemp), wrtflt(indxfltSalt), 
     &       wrtflt(indxfltRho), wrtflt(indxfltVel)
           MPI_master_only write(stdout,'(/1x,A,5(/6x,l1,2x,A))')
     &      'Fields to be saved in floats output file: (T/F)'
     &     , wrtflt(indxfltGrd),   'write Grid location variables'
     &     , wrtflt(indxfltTemp),  'write temperature.'
     &     , wrtflt(indxfltSalt),  'write salinity.'
     &     , wrtflt(indxfltRho),   'write density.'
     &     , wrtflt(indxfltVel),   'write mean float velocity' 
#ifdef AGRIF
        endif
#endif
#endif /* FLOATS */

#if defined DIAGNOSTICS_TS
!
! Switches for fields to be saved into tracer diagnostics file.
!
      elseif (keyword(1:kwlen).eq.'diag3D_history_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95)  (wrtdia3D(itrc), itrc=1, NT)
        do itrc=1,NT
          if (wrtdia3D(itrc)) wrtdia3D(NT+1)=.true.
          MPI_master_only write(stdout, '(6x,L1,2x,A,2x,A,I2,A,I4)')
     &        wrtdia3D(itrc), 
     &        'write Tracer equation terms ',
     &        ' Tracer of index', itrc,'/',NT
        enddo
# ifdef DIAGNOSTICS_TS_MLD
      elseif (keyword(1:kwlen).eq.'diag2D_history_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95)  (wrtdia2D(itrc), itrc=1, NT)
        do itrc=1,NT
          if (wrtdia2D(itrc)) wrtdia2D(NT+1)=.true.
          MPI_master_only write(stdout, '(6x,L1,2x,A,2x,A,I2,A,I4)')
     &        wrtdia2D(itrc), 
     &        'write Hbl Integrated Tracer equation terms ',
     &        ' Tracer of index', itrc,'/',NT
        enddo
# endif  /* DIAGNOSTICS_TS_MLD */

# ifdef AVERAGES
!
! Switches for fields to be saved into tracer diagnostics average file.
!
      elseif (keyword(1:kwlen).eq.'diag3D_average_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) (wrtdia3D_avg(itrc),itrc=1,NT)
        do itrc=1,NT
          if (wrtdia3D_avg(itrc)) wrtdia3D_avg(NT+1)=.true.
          MPI_master_only write(stdout, '(6x,L1,2x,A,2x,A,I2,A,I4)')
     &        wrtdia3D_avg(itrc), 
     &        'write Avg Tracer equation terms ',
     &        ' Tracer of index', itrc,'/',NT
        enddo
!
#  ifdef DIAGNOSTICS_TS_MLD
      elseif (keyword(1:kwlen).eq.'diag2D_average_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) (wrtdia2D_avg(itrc),itrc=1,NT)
        do itrc=1,NT
          if (wrtdia2D_avg(itrc)) wrtdia2D_avg(NT+1)=.true.
          MPI_master_only write(stdout, '(6x,L1,2x,A,2x,A,I2,A,I4)')
     &        wrtdia2D_avg(itrc), 
     &        'write Avg Hbl Integrated  Tracer equation terms ',
     &        ' Tracer of index', itrc,'/',NT
        enddo
#  endif  /* DIAGNOSTICS_TS_MLD */
# endif  /* AVERAGES */

#endif  /* DIAGNOSTICS_TS */


#if defined DIAGNOSTICS_UV
!
! Switches for fields to be saved into momentum diagnostics file.
!
!!          wrtdiaM=.true.
      elseif (keyword(1:kwlen).eq.'diagM_history_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95)  (wrtdiaM(itrc), itrc=1,2)
         do itrc=1,2
           if (wrtdiaM(itrc)) wrtdiaM(3)=.true.
           MPI_master_only write(stdout, '(6x,L1,2x,A,2x,A,I2)')
     &         wrtdiaM(itrc), 
     &         'write momentum equation terms ',
     &         ' Momentum of index', itrc
        enddo

# ifdef AVERAGES
!!         wrtdiaM_avg=.true.
      elseif (keyword(1:kwlen).eq.'diagM_average_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) (wrtdiaM_avg(itrc),itrc=1,2)  
        do itrc=1,2
          if (wrtdiaM(itrc)) wrtdiaM_avg(3)=.true.
          MPI_master_only write(stdout, '(6x,L1,2x,A,2x,A,I2)')
     &        wrtdiaM_avg(itrc), 
     &        'write averaged momentum  equation terms ',
     &        ' Momentum of index', itrc
       enddo
# endif /* AVERAGES  */

#endif /*DIAGNOSTICS_UV */

#ifdef DIAGNOSTICS_BIO
!
! Switches for fields to be saved into biology diagnostics file.
!
          wrtdiabio=.true.
# ifdef AVERAGES
         wrtdiabio_avg=.true.
# endif  /* AVERAGES */
#endif /* DIAGNOSTICS_BIO */

#ifdef STATIONS
!
! Switches for fields to be saved into stations output file.
!
      elseif (keyword(1:kwlen).eq.'station_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
# ifdef AGRIF
        if (Agrif_Root()) then
# endif
          read(input,*,err=95) wrtsta(indxstaGrd),
     &       wrtsta(indxstaTemp), wrtsta(indxstaSalt),
     &       wrtsta(indxstaRho), wrtsta(indxstaVel)
           MPI_master_only write(stdout,'(/1x,A,5(/6x,l1,2x,A))')
     &      'Fields to be saved in stations output  (T/F)'
     &     , wrtsta(indxstaGrd),   'write Grid location variables'
     &     , wrtsta(indxstaTemp),  'write temperature.'
     &     , wrtsta(indxstaSalt),  'write salinity.'
     &     , wrtsta(indxstaRho),   'write density.'
     &     , wrtsta(indxstaVel),   'write mean station velocity'
# ifdef AGRIF
        endif
# endif
#endif /* STATIONS */

!
! Boussinesq Approximation mean density.
!
      elseif (keyword(1:kwlen).eq.'rho0') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) rho0
        MPI_master_only write(stdout,'(F10.4,2x,A,1x,A)')
     &        rho0, 'rho0     Boussinesq approximation',
     &                           'mean density, kg/m3.'
#if defined UV_VIS2 || defined UV_VIS4
!
! Horizontal viscosity coefficients.
!
      elseif (keyword(1:kwlen).eq.'lateral_visc') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) visc2, visc4
#endif
#if defined UV_VIS2 || defined SPONGE_VIS2
        MPI_master_only write(stdout,9) visc2
   9    format(1pe10.3,2x,'visc2    Horizontal Laplacian ',
     &       'mixing coefficient [m2/s]',/,32x,'for momentum.')
#endif
#ifdef UV_VIS4
        MPI_master_only write(stdout,10) visc4
  10    format(1pe10.3,2x,'visc4    Horizontal biharmonic ',
     &       'mixing coefficient [m4/s]',/,32x,'for momentum.')
#endif
!
! Bottom drag coefficients.
!
      elseif (keyword(1:kwlen).eq.'bottom_drag') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) rdrg, rdrg2, Zob, Cdb_min, Cdb_max
        MPI_master_only write(stdout,'(5(1pe10.3,2x,A/))')
     &     rdrg, 'rdrg     Linear bottom drag coefficient (m/si).',
     &    rdrg2, 'rdrg2    Quadratic bottom drag coefficient.',
     &      Zob, 'Zob      Bottom roughness for logarithmic law (m).',
     &  Cdb_min, 'Cdb_min  Minimum bottom drag coefficient.',
     &  Cdb_max, 'Cdb_max  Maximum bottom drag coefficient.'
!
! Lateral boundary slipperness.
!
      elseif (keyword(1:kwlen).eq.'gamma2') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) gamma2
        MPI_master_only write(stdout,'(f10.2,2x,A,1x,A)')
     &     gamma2, 'gamma2   Slipperiness parameter:',
     &                     'free-slip +1, or no-slip -1.'
#ifdef SOLVE3D
# if defined TS_DIF2 || defined SPONGE_DIF2
!
! Horizontal Laplacian mixing coefficients for tracers.
!
      elseif (keyword(1:kwlen).eq.'tracer_diff2') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) (tnu2(itrc),itrc=1,NT)
        do itrc=1,NT
          MPI_master_only write(stdout,7) tnu2(itrc), itrc, itrc
   7      format(1pe10.3,'  tnu2(',i2,')  Horizontal Laplacian '
     &     ,'mixing coefficient (m2/s)',/,32x,'for tracer ',i2,'.')
        enddo
# endif
# ifdef TS_DIF4
!
! Horizontal biharmonic mixing coefficients for tracer.
!
      elseif (keyword(1:kwlen).eq.'tracer_diff4') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) (tnu4(itrc),itrc=1,NT)
        do itrc=1,NT
          MPI_master_only write(stdout,8) tnu4(itrc), itrc, itrc
   8      format(1pe10.3,'  tnu4(',i2,')  Horizontal biharmonic'
     &    ,' mixing coefficient [m4/s]',/,32x,'for tracer ',i2,'.')
        enddo

# endif
# if !defined LMD_MIXING && !defined BVF_MIXING\
      && !defined MY2_MIXING && !defined MY25_MIXING
!
! Background vertical viscosity and mixing coefficients for tracers.
!
      elseif (keyword(1:kwlen).eq.'vertical_mixing') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) Akv_bak,(Akt_bak(itrc),itrc=1,NT)
        MPI_master_only write(stdout,'(1pe10.3,2x,A,1x,A)')
     &      Akv_bak, 'Akv_bak    Background vertical viscosity',
     &                                     'coefficient, m2/s.'
        do itrc=1,NT
          MPI_master_only write(stdout,
     &           '(1pe10.3,2x,A,I2,A,1x,A/32x,A,I2,A)')
     &            Akt_bak(itrc), 'Akt_bak(', itrc, ')',
     &           'Background vertical mixing coefficient, m2/s,',
     &                                  'for tracer ', itrc, '.' 
        enddo
# endif
# ifdef MY25_MIXING
!
! Mellor-Yamada Level 2.5 turbulent closure parameters.
!
      elseif (keyword(1:kwlen).eq.'MY_bak_mixing') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) Akq_bak, q2nu2, q2nu4
        MPI_master_only write(stdout,13) Akq_bak
  13    format(1pe10.3,2x,'Akq_bak     Background vertical mixing',
     &        ' coefficient [m2/s]',/,32x,'for turbulent energy.')
#  ifdef Q_DIF2
        MPI_master_only write(stdout,14) q2nu2
  14    format(1pe10.3,2x,'q2nu2       Horizontal Laplacian ',
     &  'mixing coefficient [m2/s]',/,32x,'for turbulent energy.')
#  endif
#  ifdef Q_DIF4
        MPI_master_only write(stdout,15) q2nu4
  15    format(1pe10.3,2x,'q2nu4       Horizontal, biharmonic ',
     &  'mixing coefficient, m2/s,',/,32x,'for turbulent energy.')
#  endif
# endif
# ifdef BODYFORCE
      elseif (keyword(1:kwlen).eq.'bodyforce') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) levsfrc,levbfrc
        if (levsfrc.lt.1 .or. levsfrc.gt.N) then
          MPI_master_only write(stdout,19) 'LEVSFRC = ',levsfrc
  19      format(' READ_INP - Illegal bodyforce level, ',A,i4)
          ierr=ierr+1
        endif
        if (levbfrc.lt.1 .or. levbfrc.gt.N) then
          MPI_master_only write(stdout,19) 'LEVBFRC = ',levbfrc
          ierr=ierr+1
        endif
        MPI_master_only write(stdout,20) levsfrc, levbfrc
  20    format(4x,i6,2x,'levsfrc     ',
     &           'Deepest level to apply surface stress as a ',
     &           'bodyforce.',/,
     &         4x,i6,2x,'levbfrc     ',
     &           'Shallowest level to apply bottom stress as a ',
     &           'bodyforce.')
# endif
#endif
#if  (defined SPONGE && !defined SPONGE_GRID) 
!
! Parameters for sponge layers
!
!  if SPONGE_GRID is defined, they are set generically
!  in set_nudgcof routine
!
      elseif (keyword(1:kwlen).eq.'sponge') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) x_sponge, v_sponge
        MPI_master_only write(stdout,'(1pe10.2,2x,A,1x,A)')
     &     x_sponge,'x_sponge Thickness of sponge',
     &     'and/or nudging layer (m)'
        MPI_master_only write(stdout,'(f10.2,2x,A)')
     &     v_sponge,'v_sponge Viscosity in sponge layer (m2/s)'
!
#elif  (defined SPONGE && defined SPONGE_GRID) 
!
!  if SPONGE_GRID is defined, they are set generically
!  in set_nudgcof routine
!
      elseif (keyword(1:kwlen).eq.'sponge') then
         call cancel_kwd (keyword(1:kwlen), ierr)
         MPI_master_only write(stdout,'(/,1x,A,/,25x,A,/)')
     &   'SPONGE_GRID is defined: parameters for sponge layers',
     &   'are set generically in set_nudgcof.F routine'
#endif
!
#if  defined T_FRC_BRY  || defined M2_FRC_BRY || \
     defined M3_FRC_BRY || defined Z_FRC_BRY  || \
     defined TNUDGING   || defined M2NUDGING  || \
     defined M3NUDGING  || defined ZNUDGING
!
! Nudging parameters for OBC and nudging layers 
! (converted from [days] to [sec^-1]
!
      elseif (keyword(1:kwlen).eq.'nudg_cof') then
        call cancel_kwd (keyword(1:kwlen), ierr)
# if  defined AGRIF && !defined AGRIF_OBC_M2ORLANSKI && \
     !defined AGRIF_OBC_M3ORLANSKI && !defined AGRIF_OBC_TORLANSKI
        if (Agrif_Root()) then
# endif
          read(input,*,err=95) tauT_in,tauT_out,tauM_in,tauM_out
          tauT_in =1./(tauT_in *86400.)
          tauT_out=1./(tauT_out*86400.)
          tauM_in =1./(tauM_in *86400.)
          tauM_out=1./(tauM_out*86400.)
          MPI_master_only write(stdout,'(1pe10.3,2x,A)') 
     &        tauT_in,'tauT_in  Nudging coefficients [sec^-1]'
          MPI_master_only write(stdout,'(1pe10.3,2x,A)') 
     &       tauT_out,'tauT_out Nudging coefficients [sec^-1]'
          MPI_master_only write(stdout,'(1pe10.3,2x,A)') 
     &        tauM_in,'tauM_in  Nudging coefficients [sec^-1]'
          MPI_master_only write(stdout,'(1pe10.3,2x,A/)') 
     &       tauM_out,'tauM_out Nudging coefficients [sec^-1]'
# if defined AGRIF && !defined AGRIF_OBC_M2ORLANSKI && \
     !defined AGRIF_OBC_M3ORLANSKI && !defined AGRIF_OBC_TORLANSKI
        endif
# endif
#endif
#ifdef SOLVE3D
# ifndef NONLIN_EOS
!
! Parameters for linear equations of state.
!
      elseif (keyword(1:kwlen).eq.'lin_EOS_cff') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) R0, T0, S0, Tcoef, Scoef
        MPI_master_only write(stdout,'(5(f10.4,2x,A,1x,A/))')
     &       T0, 'T0       Background value for potential',
     &                                     'temperature (Celsius).',
     &       S0, 'S0       Background salinity (PSU),', 'constant.',
     &       R0, 'R0       Background density (kg/m3) used in',
     &                                                'linear EOS.',
     &    Tcoef, 'Tcoef    Thermal expansion coefficient',
     &                                           '(kg/m3/Celsius).',
     &    Scoef, 'Scoef    Saline contraction coefficient',
     &                                                '(kg/m3/PSU).'
# endif
# ifdef SEDIMENT
!
! Sediments input file name.
!
      elseif (keyword(1:kwlen).eq.'sediments') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,'(A)',err=95) sedname
        lstr=lenstr(sedname)
    
        MPI_master_only write(stdout,
     &              '(/9x,A,2x,A)')
     &        'Sediment input file:',sedname

      elseif (keyword(1:kwlen).eq.'sediment_history_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) 
     &                   (wrthis(itrc), itrc=indxSed,indxSed+NST+1) 
#  ifdef AVERAGES
        do itrc=indxSed,indxSed+NST+1
          wrtavg(itrc)=wrthis(itrc)
        enddo
#  endif
        MPI_master_only write(stdout,'(2(/6x,L1,2x,A,1x,A))')
     &    wrthis(indxBTHK), 'write bed_thick ', 
     &                            'thickness of sediment bed layer.'
     &  , wrthis(indxBPOR), 'write bed_poros ', 
     &                             'porosity of sediment bed layer.'
        do itrc=1,NST
            MPI_master_only write(stdout, '(6x,L1,2x,A,I2,A,I2,A)')
     &          wrthis(indxBFRA+itrc-1), 'write bed_frac(',itrc,
     &                   ') Sediment fraction of index ',itrc,'.'
        enddo
# endif /* SEDIMENT */
#endif
#ifdef BBL
      elseif (keyword(1:kwlen).eq.'bbl_history_fields') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) 
     &                    (wrthis(itrc), itrc=indxBBL,indxBBL+5) 

        MPI_master_only write(stdout,'(6(/6x,L1,2x,A,1x,A))')
     &    wrthis(indxAbed), 'write Abed ', 
     &                            'bed wave excursion amplitude.'
     &  , wrthis(indxHrip), 'write Hripple ', 
     &                                       'Bed ripple height.'
     &  , wrthis(indxLrip), 'write Lripple ', 
     &                                       'Bed ripple length.'
     &  , wrthis(indxZbnot), 'write Zbnot ', 
     &                              'Physical bottom roughness.'
     &  , wrthis(indxZbapp), 'write Zbapp ', 
     &                              'Apparent bottom roughness.'
     &  , wrthis(indxBostrw), 'write Bostrw ', 
     &                              'Wave-induced bottom stress.'

#endif /* BBL */
#ifdef ANA_PSOURCE
!
! Set-up point Sources/Sink number (Nsrc), direction (Dsrc), I- and
! J-grid locations (Isrc,Jsrc), and logical switch for type of tracer
! to apply (Lsrc). Currently, the direction can be along XI-direction
! (Dsrc = 0) or along ETA-direction (Dsrc > 0).  The mass sources are
! located at U- or V-points so the grid locations should range from
! 1 =< Isrc =< L  and  1 =< Jsrc =< M.
!
#ifdef PSOURCE_NCFILE
      elseif (keyword(1:kwlen).eq.'psource_ncfile') then
        call cancel_kwd (keyword(1:kwlen), ierr)
! river runoff file name. Check its availability.
        read(input,'(A)',err=95) fname
        lstr=lenstr(fname)
# if defined MPI && defined PARALLEL_FILES
        call insert_node (fname, lstr, mynode, NNODES, ierr)
# endif
        open (testunit, file=fname(1:lstr), status='old', err=97)
        close(testunit)
        rivname=fname(1:lstr)
        MPI_master_only write(stdout,'(2x,A,2x,A)')
     &             'Runoff Data File:', rivname(1:lstr)
        read(input,*,err=95) Nsrc
        MPI_master_only write(stdout,'(/6x,i6,2x,A,1x,A)')
     &                               Nsrc, 'Number of point sources'
        do is=1,Nsrc
          read(input,*,err=95) Isrc(is),Jsrc(is),Dsrc(is),rivdir(is),
     &                                    (Lsrc(is,itrc), itrc=1,NT),
     &                                   (Tsrc0(is,itrc), itrc=1,NT)
        MPI_master_only write(stdout,'(3(/6x,i6,2x,A),(/6x,F6.0,2x,A))')
     &    Isrc(is), 'I point source indice'
     &  , Jsrc(is), 'J point source indice'
     &  , Dsrc(is), 'Orientation of point source flow'
!         MPI_master_only write(stdout,'(/6x,F6.0,2x,A)')
     &  ,  rivdir(is), 'Direction of point source flow'

#else   
        elseif (keyword(1:kwlen).eq.'psource') then
        call cancel_kwd (keyword(1:kwlen), ierr)

        read(input,*,err=95) Nsrc
        MPI_master_only write(stdout,'(/6x,i6,2x,A,1x,A)')
     &                               Nsrc, 'Number of point sources'
        do is=1,Nsrc
          read(input,*,err=95) Isrc(is), Jsrc(is), Dsrc(is), Qbar(is),
     &                                     (Lsrc(is,itrc), itrc=1,NT),
     &                                    (Tsrc0(is,itrc), itrc=1,NT)
          MPI_master_only write(stdout,'(3(/6x,i6,2x,A))')
     &    Isrc(is), 'I point source indice'
     &  , Jsrc(is), 'J point source indice'
     &  , Dsrc(is), 'Direction of point source flow'
          MPI_master_only write(stdout,'(1pe10.3,2x,A)')
     &    Qbar(is), 'Total transport at point source'
#endif         
#ifdef MPI
          if (iminmpi.LE.Isrc(is) .AND. Isrc(is).LE.imaxmpi .AND.
     &        jminmpi.LE.Jsrc(is) .AND. Jsrc(is).LE.jmaxmpi) then
           Isrc_mpi(is,mynode)=Isrc(is)-iminmpi+1
           Jsrc_mpi(is,mynode)=Jsrc(is)-jminmpi+1
          else
           Isrc_mpi(is,mynode)=-1
           Jsrc_mpi(is,mynode)=-1
          endif
#endif
          do itrc=1,NT
            MPI_master_only write(stdout,
     &                     '(6x,L1,2x,A,I2,A,2x,A,I2,A)')
     &                      Lsrc(is,itrc), 'write Lsrc(',
     &                      itrc,')', 'Tracer of index', itrc,'.'
          enddo
          do itrc=1,NT
            MPI_master_only write(stdout,
     &                     '(6x,1pe10.3,2x,A,I2,A,2x,A,I2,A)')
     &                      Tsrc0(is,itrc), 'write Tsrc(',
     &                      itrc,')', 'Tracer of index', itrc,'.'
          enddo
        enddo
#endif /* ANA_SOURCES */
!
! Online forcing with CFSR
!
#ifdef ONLINE   /* JG Online */
      elseif (keyword(1:kwlen).eq.'online') then
        call cancel_kwd (keyword(1:kwlen), ierr)
        read(input,*,err=95) yearnum, monthnum, recordsperday, yearend, monthend
        read(input,'(A)',err=95)  fname 
        lstr=lenstr(fname)
        pathbulk=fname(1:lstr)
	! Print variables
        MPI_master_only write(stdout,
     &         '(7x,A,1x,I4,A,1x,I2,A)')
     &         'Online forcing: first forcing, year',yearnum,
     &         ', month', monthnum,'.'
        MPI_master_only write(stdout,
     &         '(7x,A,1x,I4,A,1x,I2,A)')
     &         'Online forcing: last forcing year',yearend,
     &         ', month', monthend,'.'
        MPI_master_only write(stdout,
     &         '(7x,A,A,1x,A4,1x,I1,A)')
     &         'Online forcing: datasets in ',fname(1:lstr),
     &         'with',recordsperday, ' records per day.'
#endif /* ONLINE */

      else
        MPI_master_only write(stdout,'(/3(1x,A)/)')
     &                  'WARNING: Unrecognized keyword:',
     &                   keyword(1:kwlen),' --> DISREGARDED.'
      endif
      if (keyword(1:kwlen) .eq. end_signal) goto 99
      goto 1
!
! Error while reading input parameters.
!
  95  write(stdout,'(/1x,4A/)') 'READ_INP ERROR while reading block',
     &                    ' with keyword ''', keyword(1:kwlen), '''.' 
      ierr=ierr+1
      goto 99 
  97  write(stdout,'(/1x,4A/)') 'READ_INP ERROR: Cannot find input ',
     &                                'file ''', fname(1:lstr), '''.'
      ierr=ierr+1
  99  close (input) 
!
! Check that all keywords were canceled
! Complain if some of them are left
!
      if (ierr.eq.0) then
        call check_kwds (ierr)
!
! Check CPP-switches for consistency. This operation is split into
! two stages because the first subroutine, "check_switches1", is
! generated by special program cppcheck (file cppcheck.F) by
! examining and documention all available switches in cppdefs.h.
! This subroutine creates log of all switches defined in "cppdefs.h",
! as well as traps multiply defined global configurations (project
! switches, such as REGIONAL, etc).
! The second routine, "check_switches2" is hand written and it
! contains traps for mutually exclussive definition of all other
! CPP-switches (i.e. those which are NOT project selection switches,
! for example, it traps multiply defined vertical mixing schemes or
! lateral boundary conditions).
!
! Both codes are written in transparent mode: they assumed that error
! variable (ierr) is initialized at entry and they add 1 for each
! error discovered. 
!
        call check_srcs
        call check_switches1 (ierr)
        call check_switches2 (ierr)
      endif
      if (ierr.ne.0) then
        write(stdout,'(/1x,2A,I3,1x,A/)') 'READ_INP ERROR: ',
     & 'A total of', ierr, 'configuration errors discovered.'
        return
      endif
#ifdef MPI
!      call MPI_Barrier (MPI_COMM_WORLD, ierr)
#endif
      return
      end

c
