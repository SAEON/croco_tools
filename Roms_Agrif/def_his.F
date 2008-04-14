!
! $Id: def_his.F,v 1.9 2005/09/22 14:51:38 pmarches Exp $
!
#ifndef AVRH
# include "cppdefs.h"

      subroutine def_his (ncid, total_rec, ierr)
#else
      subroutine def_avg (ncid, total_rec, ierr)
#endif
!
! Create/open averages/history netCDF file. In the case when a new
! netCDF file is created, define all variables, their dimensions and
! attributes. In the case when a previously existing netCDF file is
! to be opened for addition of new data, verify that all dimensions
! of the file are consistent with the present model configuration
! and all necessary variables exist. Save netCDF IDs for all needed
! variables. Also determine size of the unlimited dimension.
!
! The difference between def_his and def_avg is as follows: they
! have different netCDF file name (hisname/avgname); netCDF file ID
! (passed as argument); time record index (hisindx/avgindx); array
! of switches which variables to write (wrthis/wrtavg); and different
! sets of netCDF variable IDs (hisTime...hisHbl/avgTime...avgHbl);
! and the first attribute of each variable, long_name, has prefix
! 'averaged'. Because most of the code is identical for both
! routines, the second one is generated from the first entirely
! by CPP.
!
      implicit none
      logical create_new_file
      integer ncid, total_rec, ierr, rec, lstr,lvar,lenstr, timedim
     &      , r2dgrd(3),  u2dgrd(3), v2dgrd(3), auxil(2), checkdims
     &      , b3dgrd(4)
#if defined SEDIMENT || defined BBL
     &      , indxWrk
#endif
#ifdef SOLVE3D
     &      , r3dgrd(4),  u3dgrd(4), v3dgrd(4), w3dgrd(4), itrc
#endif
#include "param.h"
#include "scalars.h"
#include "ncscrum.h"
#include "netcdf.inc"

#ifndef AVRH
# define ncname hisname
# define rec_per_file nrpfhis
# define wrt wrthis
# define vidTime hisTime
# define vidTstep hisTstep
# define vidZ hisZ
# define vidUb hisUb
# define vidVb hisVb
# define vidU hisU
# define vidV hisV
# define vidT hisT
# define vidR hisR
# define vidO hisO
# define vidW hisW
# define vidVisc hisVisc
# define vidDiff hisDiff
# define vidAkv hisAkv
# define vidAkt hisAkt
# define vidAks hisAks
# define vidHbl hisHbl
# define vidHbbl hisHbbl
# ifdef BIOLOGY
#  define vidHel hisHel
#  ifdef BIO_NPZD
#   define vidChC hisChC
#   ifdef OXYGEN
#     define vidU10 hisU10
#     define vidKvO2 hisKvO2
#     define vidO2sat hisO2sat
#   endif
#  elif defined BIO_N2P2Z2D2 && defined VAR_CHL_C
#   define vidChC1 hisChC1
#   define vidChC2 hisChC2
#  endif
# endif
# define vidSed hisSed
# define vidBBL hisBBL
# define vidBostr hisBostr
# define vidShflx_rsw hisShflx_rsw
# define vidShflx_rlw hisShflx_rlw
# define vidShflx_lat hisShflx_lat
# define vidShflx_sen hisShflx_sen
#else
# define ncname avgname
# define rec_per_file nrpfavg
# define wrt wrtavg
# define vidTime avgTime
# define vidTstep avgTstep
# define vidZ avgZ
# define vidUb avgUb
# define vidVb avgVb
# define vidU avgU
# define vidV avgV
# define vidT avgT
# define vidR avgR
# define vidO avgO
# define vidW avgW
# define vidVisc avgVisc
# define vidDiff avgDiff
# define vidAkv avgAkv
# define vidAkt avgAkt
# define vidAks avgAks
# define vidHbl avgHbl
# define vidHbbl avgHbbl
# ifdef BIOLOGY
#  define vidHel avgHel
#  ifdef BIO_NPZD
#   define vidChC avgChC
#   ifdef OXYGEN
#    define vidU10 avgU10
#    define vidKvO2 avgKvO2
#    define vidO2sat avgO2sat
#   endif
#  elif defined BIO_N2P2Z2D2 && defined VAR_CHL_C
#   define vidChC1 avgChC1
#   define vidChC2 avgChC2
#  endif
# endif
# define vidSed avgSed
# define vidBBL avgBBL
# define vidBostr avgBostr
# define vidShflx_rsw avgShflx_rsw
# define vidShflx_rlw avgShflx_rlw
# define vidShflx_lat avgShflx_lat
# define vidShflx_sen avgShflx_sen

      character*70 text
#endif
!
! Put time record index into file name. In  the case when model 
! output is to be arranged into sequence of named files, the naming
! convention is as follows: 'his_root.INDEX.[MPI_node.]nc', where
! INDEX is an integer number such that (i) it is divisible by the
! specified number of records per file; and (ii)
!
!      INDEX + record_within_the_file = total_record
!
! where, 1 =< record_within_the_file =< records_per_file, so that
! total_record changes continuously throughout the sequence of files.
!
      ierr=0
      lstr=lenstr(ncname)
      if (rec_per_file.gt.0) then
        lvar=total_rec-(1+mod(total_rec-1, rec_per_file))
        call insert_time_index (ncname, lstr, lvar, ierr)
        if (ierr .ne. 0) goto 99
      endif
!
! Decide whether to create a new file, or open existing one.
! Overall the whole code below is organized into 3-way switch,
!
! 10  if (create_new_file) then
!        .... create new file, save netCDF ids for all variables;
!     elseif (ncid.eq.-1) then
!        .... try to open existing file and check its dimensions
!       if (cannot be opened or rejected) then
!         create_new_file=.true.
!         goto 10
!       endif   and prepare
!        .... prepare the file for adding new data,
!        .... find and save netCDF ids for all variables
!     else
!        .... just open, no checking, all ids are assumed to be
!        .... already known (MPI single file output only).
!     endif
!
! which is designed to implement flexible opening policy: 
! if ldefhis=.true., it forces creation of a new file [if the
! file already exists, it will be overwritten]; on the other hand,
! ldefhis=.false., it is assumed that the file already exists and
! an attempt to open it is made; if the attempt is successful, the
! file is prepared for appending hew data; if it fails, a new file
! is created.
!
      create_new_file=ldefhis
      if (ncid.ne.-1) create_new_file=.false.
#if defined MPI & !defined PARALLEL_FILES
      if (mynode.gt.0) create_new_file=.false.
#endif
!
!=================================================================
!
! Create new history/averages file:    Put global attributes
! ====== === ======= ======== =====    and define all variables.
!
!=================================================================
!
  10  if (create_new_file) then
        ierr=nf_create(ncname(1:lstr), nf_clobber, ncid)
        if (ierr .ne. nf_noerr) then
          write(stdout,'(/3(1x,A)/)') 'ERROR in def_his/avg:',
     &           'Cannot create netCDF file:', ncname(1:lstr)
          goto 99                                         !--> ERROR
        endif
        if (rec_per_file.eq.0) total_rec=0  
!
! Put global attributes.
! --- ------ -----------
!
        call put_global_atts (ncid, ierr)
!
! Define dimensions of staggered fields.
! ------ ---------- -- --------- -------
!
        ierr=nf_def_dim (ncid, 'xi_rho',   xi_rho,   r2dgrd(1))
        ierr=nf_def_dim (ncid, 'xi_u',     xi_u,     u2dgrd(1))
        ierr=nf_def_dim (ncid, 'eta_rho',  eta_rho,  r2dgrd(2))
        ierr=nf_def_dim (ncid, 'eta_v',    eta_v,    v2dgrd(2))
#ifdef SEDIMENT
        ierr=nf_def_dim (ncid, 's_b',      NLAY,     b3dgrd(3))
#endif
#ifdef SOLVE3D
        ierr=nf_def_dim (ncid, 's_rho',    N,        r3dgrd(3))
        ierr=nf_def_dim (ncid, 's_w',      N+1,      w3dgrd(3))
#endif
        ierr=nf_def_dim (ncid, 'time', nf_unlimited, timedim)
        ierr=nf_def_dim (ncid, 'auxil',    4,        auxil(1))

        auxil(2)=timedim

        r2dgrd(3)=timedim           ! Free surface

        u2dgrd(2)=r2dgrd(2)         ! 2D UBAR-type
        u2dgrd(3)=timedim

        v2dgrd(1)=r2dgrd(1)         ! 2D VBAR-type
        v2dgrd(3)=timedim

        b3dgrd(1)=r2dgrd(1)         !
        b3dgrd(2)=r2dgrd(2)         ! 3D BED-type
        b3dgrd(4)=timedim           !

#ifdef SOLVE3D
        r3dgrd(1)=r2dgrd(1)         !
        r3dgrd(2)=r2dgrd(2)         ! 3D RHO-type
        r3dgrd(4)=timedim           !

        u3dgrd(1)=u2dgrd(1)         !
        u3dgrd(2)=r2dgrd(2)         ! 3D U-type
        u3dgrd(3)=r3dgrd(3)         !
        u3dgrd(4)=timedim

        v3dgrd(1)=r2dgrd(1)         !
        v3dgrd(2)=v2dgrd(2)         ! 3D V-type
        v3dgrd(3)=r3dgrd(3)         !
        v3dgrd(4)=timedim

        w3dgrd(1)=r2dgrd(1)         !
        w3dgrd(2)=r2dgrd(2)         ! 3D W-type
        w3dgrd(4)=timedim           !
#endif
#if (defined PUT_GRID_INTO_HISTORY && !defined AVRH)\
 || (defined PUT_GRID_INTO_AVERAGES && defined AVRH)
!
! Define grid variables.
! ------ ---- ----------
!
        if (total_rec.le.1) call def_grid (ncid, r2dgrd)
#endif

!
! Define evolving model variables.
! ------ -------- ----- ----------
!
! Time step number and time record indices:
!
        ierr=nf_def_var (ncid, 'time_step', nf_int, 2, auxil,
     &                                                 vidTstep)
        ierr=nf_put_att_text (ncid, vidTstep, 'long_name', 48,
     &       'time step and record numbers from initialization')
!
! Time.
!
        lvar=lenstr(vname(1,indxTime))
        ierr=nf_def_var (ncid, vname(1,indxTime)(1:lvar),
     &                            NF_FOUT, 1, timedim, vidTime)
#ifndef AVRH
        lvar=lenstr(vname(2,indxTime))
        ierr=nf_put_att_text (ncid, vidTime, 'long_name', lvar,
     &                                vname(2,indxTime)(1:lvar))
#else
        text='averaged '/ /vname(2,indxTime)
        lvar=lenstr(text)
        ierr=nf_put_att_text (ncid, vidTime, 'long_name', lvar,
     &                                             text(1:lvar))
#endif
        lvar=lenstr(vname(3,indxTime))
        ierr=nf_put_att_text (ncid, vidTime, 'units',  lvar,
     &                                vname(3,indxTime)(1:lvar))
        lvar=lenstr(vname(4,indxTime))
        ierr=nf_put_att_text (ncid, vidTime, 'field',  lvar,
     &                                vname(4,indxTime)(1:lvar))

!===============================================================
!
! Define primary variables
!
!===============================================================

!
! Free-surface.
!
        if (wrt(indxZ)) then
          lvar=lenstr(vname(1,indxZ))
          ierr=nf_def_var (ncid, vname(1,indxZ)(1:lvar),
     &                              NF_FOUT, 3, r2dgrd, vidZ)
#ifndef AVRH
          lvar=lenstr(vname(2,indxZ))
          ierr=nf_put_att_text (ncid, vidZ, 'long_name', lvar,
     &                                  vname(2,indxZ)(1:lvar))
#else
          text='averaged '/ /vname(2,indxZ)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidZ, 'long_name', lvar,
     &                                            text(1:lvar))
#endif
          lvar=lenstr(vname(3,indxZ))
          ierr=nf_put_att_text (ncid, vidZ, 'units',     lvar,
     &                                  vname(3,indxZ)(1:lvar))
          lvar=lenstr(vname(4,indxZ))
          ierr=nf_put_att_text (ncid, vidZ, 'field',     lvar,
     &                                  vname(4,indxZ)(1:lvar))
        endif
!
! 2D momenta in XI- and ETA-directions.
!
        if (wrt(indxUb)) then
          lvar=lenstr(vname(1,indxUb))
          ierr=nf_def_var (ncid, vname(1,indxUb)(1:lvar),
     &                             NF_FOUT, 3, u2dgrd, vidUb)
#ifndef AVRH
          lvar=lenstr(vname(2,indxUb))
          ierr=nf_put_att_text (ncid, vidUb, 'long_name', lvar,
     &                                  vname(2,indxUb)(1:lvar))
#else
          text='averaged '/ /vname(2,indxUb)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidUb, 'long_name', lvar,
     &                                             text(1:lvar))
#endif
          lvar=lenstr(vname(3,indxUb))
          ierr=nf_put_att_text (ncid, vidUb, 'units',     lvar,
     &                                  vname(3,indxUb)(1:lvar))
          lvar=lenstr(vname(4,indxUb))
          ierr=nf_put_att_text (ncid, vidUb, 'field',    lvar,
     &                                  vname(4,indxUb)(1:lvar))
        endif

        if (wrt(indxVb)) then
          lvar=lenstr(vname(1,indxVb))
          ierr=nf_def_var (ncid, vname(1,indxVb)(1:lvar),
     &                              NF_FOUT, 3, v2dgrd, vidVb)
#ifndef AVRH
          lvar=lenstr(vname(2,indxVb))
          ierr=nf_put_att_text (ncid, vidVb, 'long_name', lvar,
     &                                  vname(2,indxVb)(1:lvar))
#else
          text='averaged '/ /vname(2,indxVb)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidVb, 'long_name', lvar,
     &                                             text(1:lvar))
#endif
          lvar=lenstr(vname(3,indxVb))
          ierr=nf_put_att_text (ncid, vidVb, 'units',     lvar,
     &                                  vname(3,indxVb)(1:lvar))
          lvar=lenstr(vname(4,indxVb))
          ierr=nf_put_att_text (ncid, vidVb, 'field',     lvar,
     &                                  vname(4,indxVb)(1:lvar))
        endif

#ifdef SOLVE3D
!
! 3D momenta in XI- and ETA-directions.
!
        if (wrt(indxU)) then
          lvar=lenstr(vname(1,indxU))
          ierr=nf_def_var (ncid, vname(1,indxU)(1:lvar), 
     &                             NF_FOUT, 4, u3dgrd, vidU)
# ifndef AVRH
          lvar=lenstr(vname(2,indxU))
          ierr=nf_put_att_text (ncid, vidU, 'long_name', lvar,
     &                                  vname(2,indxU)(1:lvar))
# else
          text='averaged '/ /vname(2,indxU)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidU, 'long_name', lvar,
     &                                            text(1:lvar))
# endif
          lvar=lenstr(vname(3,indxU))
          ierr=nf_put_att_text (ncid, vidU, 'units',     lvar,
     &                                  vname(3,indxU)(1:lvar))
          lvar=lenstr(vname(4,indxU))
          ierr=nf_put_att_text (ncid, vidU, 'field',     lvar,
     &                                  vname(4,indxU)(1:lvar))
        endif

        if (wrt(indxV)) then
          lvar=lenstr(vname(1,indxV))
          ierr=nf_def_var (ncid, vname(1,indxV)(1:lvar), 
     &                             NF_FOUT, 4, v3dgrd, vidV)
# ifndef AVRH
          lvar=lenstr(vname(2,indxV))
          ierr=nf_put_att_text (ncid, vidV, 'long_name', lvar,
     &                                  vname(2,indxV)(1:lvar))
# else
          text='averaged '/ /vname(2,indxV)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidV, 'long_name', lvar,
     &                                            text(1:lvar))
# endif
          lvar=lenstr(vname(3,indxV))
          ierr=nf_put_att_text (ncid, vidV, 'units',     lvar,
     &                                  vname(3,indxV)(1:lvar))
          lvar=lenstr(vname(4,indxV))
          ierr=nf_put_att_text (ncid, vidV, 'field',     lvar,
     &                                  vname(4,indxV)(1:lvar))
        endif
!
! Tracer variables.
!
        do itrc=1,NT
          if (wrt(indxT+itrc-1)) then
            lvar=lenstr(vname(1,indxT+itrc-1))
            ierr=nf_def_var (ncid, vname(1,indxT+itrc-1)(1:lvar),
     &                             NF_FOUT, 4, r3dgrd, vidT(itrc))
# ifndef AVRH
            lvar=lenstr(vname(2,indxT+itrc-1))
            ierr=nf_put_att_text (ncid, vidT(itrc), 'long_name',
     &                         lvar, vname(2,indxT+itrc-1)(1:lvar))
# else
            text='averaged '/ /vname(2,indxT+itrc-1)
            lvar=lenstr(text)
            ierr=nf_put_att_text (ncid, vidT(itrc), 'long_name',
     &                                          lvar, text(1:lvar))
# endif
            lvar=lenstr(vname(3,indxT+itrc-1))
            ierr=nf_put_att_text (ncid, vidT(itrc), 'units', lvar,
     &                               vname(3,indxT+itrc-1)(1:lvar))
            lvar=lenstr(vname(4,indxT+itrc-1))
            ierr=nf_put_att_text (ncid, vidT(itrc), 'field', lvar,
     &                               vname(4,indxT+itrc-1)(1:lvar))
          endif
        enddo
!
! Density anomaly.
!
        if (wrt(indxR)) then
          lvar=lenstr(vname(1,indxR))
          ierr=nf_def_var (ncid, vname(1,indxR)(1:lvar), 
     &                             NF_FOUT, 4, r3dgrd, vidR)
# ifndef AVRH
          lvar=lenstr(vname(2,indxR))
          ierr=nf_put_att_text (ncid, vidR, 'long_name', lvar,
     &                                  vname(2,indxR)(1:lvar))
# else
          text='averaged '/ /vname(2,indxR)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidR, 'long_name', lvar,
     &                                            text(1:lvar))
# endif
          lvar=lenstr(vname(3,indxR))
          ierr=nf_put_att_text (ncid, vidR, 'units',     lvar,
     &                                  vname(3,indxR)(1:lvar))
          lvar=lenstr(vname(4,indxR))
          ierr=nf_put_att_text (ncid, vidR, 'field',     lvar,
     &                                  vname(4,indxR)(1:lvar))
        endif
!
! S-coordinate "omega" vertical velocity.
!
        if (wrt(indxO)) then
          lvar=lenstr(vname(1,indxO))
          ierr=nf_def_var (ncid, vname(1,indxO)(1:lvar),
     &                             NF_FOUT, 4, w3dgrd, vidO)
# ifndef AVRH
          lvar=lenstr(vname(2,indxO))
          ierr=nf_put_att_text (ncid, vidO, 'long_name', lvar,
     &                                  vname(2,indxO)(1:lvar))
# else
          text='averaged '/ /vname(2,indxO)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidO, 'long_name', lvar,
     &                                            text(1:lvar))
# endif
          lvar=lenstr(vname(3,indxO))
          ierr=nf_put_att_text (ncid, vidO, 'units',     lvar,
     &                                  vname(3,indxO)(1:lvar))
          lvar=lenstr(vname(4,indxO))
          ierr=nf_put_att_text (ncid, vidO, 'field',     lvar,
     &                                  vname(4,indxO)(1:lvar))
        endif
!
! True vertical velocity.
!
        if (wrt(indxW)) then
          lvar=lenstr(vname(1,indxW))
          ierr=nf_def_var (ncid, vname(1,indxW)(1:lvar),
     &                               NF_FOUT, 4, r3dgrd, vidW)
# ifndef AVRH
          lvar=lenstr(vname(2,indxW))
          ierr=nf_put_att_text (ncid, vidW, 'long_name', lvar,
     &                                  vname(2,indxW)(1:lvar))
# else
          text='averaged '/ /vname(2,indxW)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidW, 'long_name', lvar,
     &                                            text(1:lvar))
# endif
          lvar=lenstr(vname(3,indxW))
          ierr=nf_put_att_text (ncid, vidW, 'units',     lvar,
     &                                  vname(3,indxW)(1:lvar))
          lvar=lenstr(vname(4,indxW))
          ierr=nf_put_att_text (ncid, vidW, 'field',     lvar,
     &                                  vname(4,indxW)(1:lvar))
        endif
#endif /* SOLVE3D */

!================================================================
! 
! Define secondary variables
!
!================================================================

        if (wrt(indxBostr)) then
          lvar=lenstr(vname(1,indxBostr))
          ierr=nf_def_var (ncid, vname(1,indxBostr)(1:lvar),
     &                             NF_FOUT, 3, r2dgrd, vidBostr)
#  ifndef AVRH
          lvar=lenstr(vname(2,indxBostr))
          ierr=nf_put_att_text (ncid, vidBostr, 'long_name', lvar,
     &                                 vname(2,indxBostr)(1:lvar))
#  else
          text='averaged '/ /vname(2,indxBostr)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidBostr, 'long_name', lvar,
     &                                               text(1:lvar))
#  endif
          lvar=lenstr(vname(3,indxBostr))
          ierr=nf_put_att_text (ncid, vidBostr, 'units',     lvar,
     &                                 vname(3,indxBostr)(1:lvar))
        endif
#ifdef SOLVE3D
# ifdef VIS_COEF_3D
!
! Horizontal viscosity coefficient.
!
        if (wrt(indxVisc)) then
          lvar=lenstr(vname(1,indxVisc))
          ierr=nf_def_var (ncid, vname(1,indxVisc)(1:lvar),
     &                             NF_FOUT, 4, r3dgrd, vidVisc)
#  ifndef AVRH
          lvar=lenstr(vname(2,indxVisc))
          ierr=nf_put_att_text (ncid, vidVisc, 'long_name', lvar,
     &                                  vname(2,indxVisc)(1:lvar))
#  else
          text='averaged '/ /vname(2,indxVisc)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidVisc, 'long_name', lvar,
     &                                              text(1:lvar))
#  endif
          lvar=lenstr(vname(3,indxVisc))
          ierr=nf_put_att_text (ncid, vidVisc, 'units',     lvar,
     &                                  vname(3,indxVisc)(1:lvar))
          lvar=lenstr(vname(4,indxVisc))
          ierr=nf_put_att_text (ncid, vidVisc, 'field',     lvar,
     &                                  vname(4,indxVisc)(1:lvar))
        endif
# endif
# ifdef DIF_COEF_3D
!
! Horizontal diffusivity coefficient.
!
        if (wrt(indxDiff)) then
          lvar=lenstr(vname(1,indxDiff))
          ierr=nf_def_var (ncid, vname(1,indxDiff)(1:lvar),
     &                             NF_FOUT, 4, r3dgrd, vidDiff)
#  ifndef AVRH
          lvar=lenstr(vname(2,indxDiff))
          ierr=nf_put_att_text (ncid, vidDiff, 'long_name', lvar,
     &                                  vname(2,indxDiff)(1:lvar))
#  else
          text='averaged '/ /vname(2,indxDiff)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidDiff, 'long_name', lvar,
     &                                              text(1:lvar))
#  endif
          lvar=lenstr(vname(3,indxDiff))
          ierr=nf_put_att_text (ncid, vidDiff, 'units',     lvar,
     &                                  vname(3,indxDiff)(1:lvar))
          lvar=lenstr(vname(4,indxDiff))
          ierr=nf_put_att_text (ncid, vidDiff, 'field',     lvar,
     &                                  vname(4,indxDiff)(1:lvar))
        endif
# endif
!
! Vertical viscosity coefficient.
!
        if (wrt(indxAkv)) then
          lvar=lenstr(vname(1,indxAkv))
          ierr=nf_def_var (ncid, vname(1,indxAkv)(1:lvar),
     &                             NF_FOUT, 4, w3dgrd, vidAkv)
# ifndef AVRH
          lvar=lenstr(vname(2,indxAkv))
          ierr=nf_put_att_text (ncid, vidAkv, 'long_name', lvar,
     &                                  vname(2,indxAkv)(1:lvar))
# else
          text='averaged '/ /vname(2,indxAkv)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidAkv, 'long_name', lvar,
     &                                              text(1:lvar))
# endif
          lvar=lenstr(vname(3,indxAkv))
          ierr=nf_put_att_text (ncid, vidAkv, 'units',     lvar,
     &                                  vname(3,indxAkv)(1:lvar))
          lvar=lenstr(vname(4,indxAkv))
          ierr=nf_put_att_text (ncid, vidAkv, 'field',     lvar,
     &                                  vname(4,indxAkv)(1:lvar))
        endif
!
! Vertical diffusion coefficient for potential temperature.
!
        if (wrt(indxAkt)) then
          lvar=lenstr(vname(1,indxAkt))
          ierr=nf_def_var (ncid, vname(1,indxAkt)(1:lvar),
     &                              NF_FOUT, 4, w3dgrd, vidAkt)
# ifndef AVRH
          lvar=lenstr(vname(2,indxAkt))
          ierr=nf_put_att_text (ncid, vidAkt, 'long_name', lvar,
     &                                  vname(2,indxAkt)(1:lvar))
# else
          text='averaged '/ /vname(2,indxAkt)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidAkt, 'long_name', lvar,
     &                                              text(1:lvar))
# endif
          lvar=lenstr(vname(3,indxAkt))
          ierr=nf_put_att_text (ncid, vidAkt, 'units',     lvar,
     &                                  vname(3,indxAkt)(1:lvar))
          lvar=lenstr(vname(4,indxAkt))
          ierr=nf_put_att_text (ncid, vidAkt, 'field',     lvar,
     &                                  vname(4,indxAkt)(1:lvar))
        endif
# ifdef SALINITY
!
! Vertical diffusion coefficient for salinity.
!
        if (wrt(indxAks)) then
          lvar=lenstr(vname(1,indxAks))
          ierr=nf_def_var (ncid, vname(1,indxAks)(1:lvar),
     &                              NF_FOUT, 4, w3dgrd, vidAks)
#  ifndef AVRH
          lvar=lenstr(vname(2,indxAks))
          ierr=nf_put_att_text (ncid, vidAks, 'long_name', lvar,
     &                                  vname(2,indxAks)(1:lvar))
#  else
          text='averaged '/ /vname(2,indxAks)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidAks, 'long_name', lvar,
     &                                              text(1:lvar))
#  endif
          lvar=lenstr(vname(3,indxAks))
          ierr=nf_put_att_text (ncid, vidAks, 'units',     lvar,
     &                                  vname(3,indxAks)(1:lvar))
          lvar=lenstr(vname(4,indxAks))
          ierr=nf_put_att_text (ncid, vidAks, 'field',     lvar,
     &                                  vname(4,indxAks)(1:lvar))
        endif
# endif
# ifdef LMD_SKPP
!
! Depth of planetary boundary layer.
!
        if (wrt(indxHbl)) then
          lvar=lenstr(vname(1,indxHbl))
          ierr=nf_def_var (ncid, vname(1,indxHbl)(1:lvar),
     &                             NF_FOUT, 3, r2dgrd, vidHbl)
#  ifndef AVRH
          lvar=lenstr(vname(2,indxHbl))
          ierr=nf_put_att_text (ncid, vidHbl, 'long_name', lvar,
     &                                  vname(2,indxHbl)(1:lvar))
#  else
          text='averaged '/ /vname(2,indxHbl)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidHbl, 'long_name', lvar,
     &                                              text(1:lvar))
#  endif
          lvar=lenstr(vname(3,indxHbl))
          ierr=nf_put_att_text (ncid, vidHbl, 'units',     lvar,
     &                                  vname(3,indxHbl)(1:lvar))
          lvar=lenstr(vname(4,indxHbl))
          ierr=nf_put_att_text (ncid, vidHbl, 'field',     lvar,
     &                                  vname(4,indxHbl)(1:lvar))
        endif
# endif
# ifdef LMD_BKPP
!
! Depth of bottom planetary boundary layer.
!
        if (wrt(indxHbbl)) then
          lvar=lenstr(vname(1,indxHbbl))
          ierr=nf_def_var (ncid, vname(1,indxHbbl)(1:lvar),
     &                             NF_FOUT, 3, r2dgrd, vidHbbl)
#  ifndef AVRH
          lvar=lenstr(vname(2,indxHbbl))
          ierr=nf_put_att_text (ncid, vidHbbl, 'long_name', lvar,
     &                                  vname(2,indxHbbl)(1:lvar))
#  else
          text='averaged '/ /vname(2,indxHbbl)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidHbbl, 'long_name', lvar,
     &                                              text(1:lvar))
#  endif
          lvar=lenstr(vname(3,indxHbbl))
          ierr=nf_put_att_text (ncid, vidHbbl, 'units',     lvar,
     &                                  vname(3,indxHbbl)(1:lvar))
          lvar=lenstr(vname(4,indxHbbl))
          ierr=nf_put_att_text (ncid, vidHbbl, 'field',     lvar,
     &                                  vname(4,indxHbbl)(1:lvar))
        endif
# endif
# ifdef BIOLOGY
!
! Depth of the euphotic layer.
!
        if (wrt(indxHel)) then
          lvar=lenstr(vname(1,indxHel))
          ierr=nf_def_var (ncid, vname(1,indxHel)(1:lvar),
     &                             NF_FOUT, 3, r2dgrd, vidHel)
#  ifndef AVRH
          lvar=lenstr(vname(2,indxHel))
          ierr=nf_put_att_text (ncid, vidHel, 'long_name', lvar,
     &                                  vname(2,indxHel)(1:lvar))
#  else
          text='averaged '/ /vname(2,indxHel)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidHel, 'long_name', lvar,
     &                                              text(1:lvar))
#  endif
          lvar=lenstr(vname(3,indxHel))
          ierr=nf_put_att_text (ncid, vidHel, 'units',     lvar,
     &                                  vname(3,indxHel)(1:lvar))
          lvar=lenstr(vname(4,indxHel))
          ierr=nf_put_att_text (ncid, vidHel, 'field',     lvar,
     &                                  vname(4,indxHel)(1:lvar))
        endif
!
! Chlorophyll/Carbon ratio
!
#  ifdef BIO_NPZD
        if (wrt(indxChC)) then
          lvar=lenstr(vname(1,indxChC))
          ierr=nf_def_var (ncid, vname(1,indxChC)(1:lvar),
     &                             NF_FOUT, 4, r3dgrd, vidChC)
#   ifndef AVRH
          lvar=lenstr(vname(2,indxChC))
          ierr=nf_put_att_text (ncid, vidChC, 'long_name', lvar,
     &                                  vname(2,indxChC)(1:lvar))
#   else
          text='averaged '/ /vname(2,indxChC)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidChC, 'long_name', lvar,
     &                                              text(1:lvar))
#   endif
          lvar=lenstr(vname(3,indxChC))
          ierr=nf_put_att_text (ncid, vidChC, 'units',     lvar,
     &                                  vname(3,indxChC)(1:lvar))
          lvar=lenstr(vname(4,indxChC))
          ierr=nf_put_att_text (ncid, vidChC, 'field',     lvar,
     &                                  vname(4,indxChC)(1:lvar))
        endif
#   ifdef OXYGEN
!
! Wind speed at 10 m
!
        if (wrt(indxU10)) then
          lvar=lenstr(vname(1,indxU10))
          ierr=nf_def_var (ncid, vname(1,indxU10)(1:lvar),
     &                             NF_FOUT, 3, r2dgrd, vidU10)
#  ifndef AVRH
          lvar=lenstr(vname(2,indxU10))
          ierr=nf_put_att_text (ncid, vidU10, 'long_name', lvar,
     &                                  vname(2,indxU10)(1:lvar))
#  else
          text='averaged '/ /vname(2,indxU10)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidU10, 'long_name', lvar,
     &                                              text(1:lvar))
#  endif
          lvar=lenstr(vname(3,indxU10))
          ierr=nf_put_att_text (ncid, vidU10, 'units',     lvar,
     &                                  vname(3,indxU10)(1:lvar))
          lvar=lenstr(vname(4,indxU10))
          ierr=nf_put_att_text (ncid, vidU10, 'field',     lvar,
     &                                  vname(4,indxU10)(1:lvar))
        endif
!
! Gas exchange coefficient of O2
!
        if (wrt(indxKvO2)) then
          lvar=lenstr(vname(1,indxKvO2))
          ierr=nf_def_var (ncid, vname(1,indxKvO2)(1:lvar),
     &                             NF_FOUT, 3, r2dgrd, vidKvO2)
#  ifndef AVRH
          lvar=lenstr(vname(2,indxKvO2))
          ierr=nf_put_att_text (ncid, vidKvO2, 'long_name', lvar,
     &                                  vname(2,indxKvO2)(1:lvar))
#  else
          text='averaged '/ /vname(2,indxKvO2)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidKvO2, 'long_name', lvar,
     &                                              text(1:lvar))
#  endif
          lvar=lenstr(vname(3,indxKvO2))
          ierr=nf_put_att_text (ncid, vidKvO2, 'units',     lvar,
     &                                  vname(3,indxKvO2)(1:lvar))
          lvar=lenstr(vname(4,indxKvO2))
          ierr=nf_put_att_text (ncid, vidKvO2, 'field',     lvar,
     &                                  vname(4,indxKvO2)(1:lvar))
        endif
!
! Saturation concentration of O2
!
        if (wrt(indxO2sat)) then
          lvar=lenstr(vname(1,indxO2sat))
          ierr=nf_def_var (ncid, vname(1,indxO2sat)(1:lvar),
     &                             NF_FOUT, 3, r2dgrd, vidO2sat)
#  ifndef AVRH
          lvar=lenstr(vname(2,indxO2sat))
          ierr=nf_put_att_text (ncid, vidO2sat, 'long_name', lvar,
     &                                  vname(2,indxO2sat)(1:lvar))
#  else
          text='averaged '/ /vname(2,indxO2sat)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidO2sat, 'long_name', lvar,
     &                                              text(1:lvar))
#  endif
          lvar=lenstr(vname(3,indxO2sat))
          ierr=nf_put_att_text (ncid, vidO2sat, 'units',     lvar,
     &                                  vname(3,indxO2sat)(1:lvar))
          lvar=lenstr(vname(4,indxO2sat))
          ierr=nf_put_att_text (ncid, vidO2sat, 'field',     lvar,
     &                                  vname(4,indxO2sat)(1:lvar))
        endif
#   endif /* OXYGEN */
#  elif defined BIO_N2P2Z2D2 && defined VAR_CHL_C
        if (wrt(indxChC1)) then
          lvar=lenstr(vname(1,indxChC1))
          ierr=nf_def_var (ncid, vname(1,indxChC1)(1:lvar),
     &                             NF_FOUT, 4, r3dgrd, vidChC1)
#   ifndef AVRH
          lvar=lenstr(vname(2,indxChC1))
          ierr=nf_put_att_text (ncid, vidChC1, 'long_name', lvar,
     &                                  vname(2,indxChC1)(1:lvar))
#   else
          text='averaged '/ /vname(2,indxChC1)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidChC1, 'long_name', lvar,
     &                                              text(1:lvar))
#   endif
          lvar=lenstr(vname(3,indxChC1))
          ierr=nf_put_att_text (ncid, vidChC1, 'units',     lvar,
     &                                  vname(3,indxChC1)(1:lvar))
          lvar=lenstr(vname(4,indxChC1))
          ierr=nf_put_att_text (ncid, vidChC1, 'field',     lvar,
     &                                  vname(4,indxChC1)(1:lvar))
        endif

        if (wrt(indxChC2)) then
          lvar=lenstr(vname(1,indxChC2))
          ierr=nf_def_var (ncid, vname(1,indxChC2)(1:lvar),
     &                             NF_FOUT, 4, r3dgrd, vidChC2)
#   ifndef AVRH
          lvar=lenstr(vname(2,indxChC2))
          ierr=nf_put_att_text (ncid, vidChC2, 'long_name', lvar,
     &                                  vname(2,indxChC2)(1:lvar))
#   else
          text='averaged '/ /vname(2,indxChC2)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidChC2, 'long_name', lvar,
     &                                              text(1:lvar))
#   endif
          lvar=lenstr(vname(3,indxChC2))
          ierr=nf_put_att_text (ncid, vidChC2, 'units',     lvar,
     &                                  vname(3,indxChC2)(1:lvar))
          lvar=lenstr(vname(4,indxChC2))
          ierr=nf_put_att_text (ncid, vidChC2, 'field',     lvar,
     &                                  vname(4,indxChC2)(1:lvar))
        endif
#  endif
# endif
!
# ifdef SEDIMENT
!
! bed_thick  Sediment bed layer thickness (m)
! bed_poros  Bed porosity (-)
! bed_frac_sand/silt
!           volume fraction of size class ised in each bed layer (-)
!
        do itrc=1,NST+2
         indxWrk=indxSed+itrc-1
         if (wrt(indxWrk)) then
          lvar=lenstr(vname(1,indxWrk))
          ierr=nf_def_var (ncid, vname(1,indxWrk)(1:lvar),
     &                             NF_FOUT, 4, b3dgrd, vidSed(itrc))
#  ifndef AVRH
          lvar=lenstr(vname(2,indxWrk))
          ierr=nf_put_att_text (ncid, vidSed(itrc), 'long_name', lvar,
     &                                  vname(2,indxWrk)(1:lvar))
#  else
          text='averaged '/ /vname(2,indxWrk)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidSed(itrc), 'long_name', lvar,
     &                                              text(1:lvar))
#  endif
          lvar=lenstr(vname(3,indxWrk))
          ierr=nf_put_att_text (ncid, vidSed(itrc), 'units',     lvar,
     &                                  vname(3,indxWrk)(1:lvar))
         endif
        enddo
!
# endif /* SEDIMENT */
# if defined BBL && !defined AVRH
!
! Abed       Wind-induced, bed wave excursion amplitude (m)
! Hripple    Bed ripple height (m)
! Lripple    Bed ripple length (m)
! Zbnot      Physical hydraulic bottom roughness (grain roughness) (m)
! Zbapp      Effective apparent hydraulic bottom roughness
! bostrw     Wave-induced kinematic bottom stress (N/m2)
!
        do itrc=1,6
         indxWrk=indxBBL+itrc-1
         if (wrt(indxWrk)) then
          lvar=lenstr(vname(1,indxWrk))
          ierr=nf_def_var (ncid, vname(1,indxWrk)(1:lvar),
     &                             NF_FOUT, 3, r2dgrd, vidBBL(itrc))
#  ifndef AVRH
          lvar=lenstr(vname(2,indxWrk))
          ierr=nf_put_att_text (ncid, vidBBL(itrc), 'long_name', lvar,
     &                                  vname(2,indxWrk)(1:lvar))
#  else
          text='averaged '/ /vname(2,indxWrk)
          lvar=lenstr(text)
          ierr=nf_put_att_text (ncid, vidBBL(itrc), 'long_name', lvar,
     &                                              text(1:lvar))
#  endif
          lvar=lenstr(vname(3,indxWrk))
          ierr=nf_put_att_text (ncid, vidBBL(itrc), 'units',     lvar,
     &                                  vname(3,indxWrk)(1:lvar))
         endif
        enddo
# endif /* BBL */
# ifdef BULK_FLUX
! Short wave SHFLX
        lvar=lenstr(vname(1,indxShflx_rsw))
        ierr=nf_def_var (ncid, vname(1,indxShflx_rsw)(1:lvar),
     &                           NF_FOUT, 3, r2dgrd, vidShflx_rsw)
#  ifndef AVRH
        lvar=lenstr(vname(2,indxShflx_rsw))
        ierr=nf_put_att_text (ncid, vidShflx_rsw, 'long_name', lvar,
     &                               vname(2,indxShflx_rsw)(1:lvar))
#  else
        text='averaged '/ /vname(2,indxShflx_rsw)
        lvar=lenstr(text)
        ierr=nf_put_att_text (ncid, vidShflx_rsw, 'long_name', lvar,
     &                                               text(1:lvar))
#  endif
        lvar=lenstr(vname(3,indxShflx_rsw))
        ierr=nf_put_att_text (ncid, vidShflx_rsw, 'units',     lvar,
     &                               vname(3,indxShflx_rsw)(1:lvar))
! Long wave SHFLX
        lvar=lenstr(vname(1,indxShflx_rlw))
        ierr=nf_def_var (ncid, vname(1,indxShflx_rlw)(1:lvar),
     &                           NF_FOUT, 3, r2dgrd, vidShflx_rlw)
#  ifndef AVRH
        lvar=lenstr(vname(2,indxShflx_rlw))
        ierr=nf_put_att_text (ncid, vidShflx_rlw, 'long_name', lvar,
     &                               vname(2,indxShflx_rlw)(1:lvar))
#  else
        text='averaged '/ /vname(2,indxShflx_rlw)
        lvar=lenstr(text)
        ierr=nf_put_att_text (ncid, vidShflx_rlw, 'long_name', lvar,
     &                                             text(1:lvar))
#  endif
        lvar=lenstr(vname(3,indxShflx_rlw))
        ierr=nf_put_att_text (ncid, vidShflx_rlw, 'units',     lvar,
     &                               vname(3,indxShflx_rlw)(1:lvar))
! Latent SHFLX
        lvar=lenstr(vname(1,indxShflx_lat))
        ierr=nf_def_var (ncid, vname(1,indxShflx_lat)(1:lvar),
     &                           NF_FOUT, 3, r2dgrd, vidShflx_lat)
#  ifndef AVRH
        lvar=lenstr(vname(2,indxShflx_lat))
        ierr=nf_put_att_text (ncid, vidShflx_lat, 'long_name', lvar,
     &                               vname(2,indxShflx_lat)(1:lvar))
#  else
        text='averaged '/ /vname(2,indxShflx_lat)
        lvar=lenstr(text)
        ierr=nf_put_att_text (ncid, vidShflx_lat, 'long_name', lvar,
     &                                             text(1:lvar))
#  endif
        lvar=lenstr(vname(3,indxShflx_lat))
        ierr=nf_put_att_text (ncid, vidShflx_lat, 'units',     lvar,
     &                               vname(3,indxShflx_lat)(1:lvar))
! Sensible SHFLX
        lvar=lenstr(vname(1,indxShflx_sen))
        ierr=nf_def_var (ncid, vname(1,indxShflx_sen)(1:lvar),
     &                           NF_FOUT, 3, r2dgrd, vidShflx_sen)
#  ifndef AVRH
        lvar=lenstr(vname(2,indxShflx_sen))
        ierr=nf_put_att_text (ncid, vidShflx_sen, 'long_name', lvar,
     &                               vname(2,indxShflx_sen)(1:lvar))
#  else
        text='averaged '/ /vname(2,indxShflx_sen)
        lvar=lenstr(text)
        ierr=nf_put_att_text (ncid, vidShflx_sen, 'long_name', lvar,
     &                                             text(1:lvar))
#  endif
        lvar=lenstr(vname(3,indxShflx_sen))
        ierr=nf_put_att_text (ncid, vidShflx_sen, 'units',     lvar,
     &                               vname(3,indxShflx_sen)(1:lvar))
# endif
!
#endif /* SOLVE3D */

!
! Leave definition mode.
!
!
        ierr=nf_enddef(ncid)
        write(stdout,'(6x,4A,1x,A,i4)') 'DEF_HIS/AVG - Created ',
     &                'new netCDF file ''', ncname(1:lstr), '''.'
     &                 MYID
!
!===================================================================
! Open an existing file and prepare for appending data.
! ==== == ======== ==== === ======= === ========= =====
! Inquire about the dimensions and variables. Check for
! consistency with model dimensions. In the case when file 
! is rejected (whether it cannot be opened, or something
! is wrong with its dimensions) create a new file.
!
! After that verify that all necessary variables are already
! defined there and find their netCDF IDs.
!===================================================================
!
      elseif (ncid.eq.-1) then
        ierr=nf_open (ncname(1:lstr), nf_write, ncid)
        if (ierr. eq. nf_noerr) then
          ierr=checkdims (ncid, ncname(1:lstr), lstr, rec) 
          if (ierr .eq. nf_noerr) then
            if (rec_per_file.eq.0) then
              ierr=rec+1 - total_rec
            else
              ierr=rec+1 - (1+mod(total_rec-1, rec_per_file))
            endif
            if (ierr.gt.0) then
              MPI_master_only write( stdout,
     &                 '(/1x,A,I5,1x,A/8x,3A,I5,/8x,A,I5,1x,A/)'
     &            ) 'DEF_HIS/AVG WARNING: Actual number of records',
     &               rec,  'in netCDF file',  '''',  ncname(1:lstr),
     &             ''' exceeds the record number from restart data',
     &             rec+1-ierr,'/', total_rec,', restart is assumed.'
              rec=rec-ierr
            elseif (rec_per_file.eq.0) then
              total_rec=rec+1           ! <-- set to the next record
#if defined MPI & !defined PARALLEL_FILES
              if (mynode.gt.0) total_rec=total_rec-1
#endif
            endif
            ierr=nf_noerr
          endif
        endif

        if (ierr. ne. nf_noerr) then
#if defined MPI & !defined PARALLEL_FILES
          if (mynode.eq.0) then
            create_new_file=.true.
            goto 10
          else
            write(stdout,'(/1x,4A,2x,A,I4/)') 'DEF_HIS/AVG ERROR: ',
     &                  'Cannot open file ''', ncname(1:lstr), '''.'
     &                   MYID
            goto 99                                       !--> ERROR
          endif
#else
          create_new_file=.true.
          goto 10
#endif
        endif
!
! Find netCDF IDs of evolving model variables:
! ---- ------ --- -- -------- ----- ----------
!
! Time step indices:
!
        ierr=nf_inq_varid (ncid, 'time_step', vidTstep)
        if (ierr .ne. nf_noerr) then
          write(stdout,1) 'time_step', ncname(1:lstr)
          goto 99                                         !--> ERROR
        endif
!
! Time.
!
        lvar=lenstr(vname(1,indxTime))
        ierr=nf_inq_varid (ncid,vname(1,indxTime)(1:lvar),vidTime)
        if (ierr .ne. nf_noerr) then
          write(stdout,1) vname(1,indxTime)(1:lvar), ncname(1:lstr)
          goto 99                                         !--> ERROR 
        endif
!
! Free-surface.
!
        if (wrt(indxZ)) then
          lvar=lenstr(vname(1,indxZ))
          ierr=nf_inq_varid (ncid, vname(1,indxZ)(1:lvar), vidZ)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxZ)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
!
! 2D momenta in XI- and ETA-directions.
!
        if (wrt(indxUb)) then
          lvar=lenstr(vname(1,indxUb))
          ierr=nf_inq_varid (ncid, vname(1,indxUb)(1:lvar), vidUb)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxUb)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif

        if (wrt(indxVb)) then
          lvar=lenstr(vname(1,indxVb))
          ierr=nf_inq_varid (ncid, vname(1,indxVb)(1:lvar), vidVb)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxVb)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
!
! bostr     Kinematic bottom stress (N/m2)
!
        if (wrt(indxBostr)) then
          lvar=lenstr(vname(1,indxBostr))
          ierr=nf_inq_varid (ncid,vname(1,indxBostr)(1:lvar),
     &                                                   vidBostr)
          if (ierr .ne. nf_noerr) then
          write(stdout,1) vname(1,indxBostr)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif

#ifdef SOLVE3D
!
! 3D momenta in XI- and ETA-directions.
!
        if (wrt(indxU)) then
          lvar=lenstr(vname(1,indxU))
          ierr=nf_inq_varid (ncid, vname(1,indxU)(1:lvar), vidU)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxU)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif

        if (wrt(indxV)) then
          lvar=lenstr(vname(1,indxV))
          ierr=nf_inq_varid (ncid, vname(1,indxV)(1:lvar), vidV)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxV)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
!
! Tracer variables.
!
        do itrc=1,NT
          if (wrt(indxT+itrc-1)) then
            lvar=lenstr(vname(1,indxT+itrc-1))
            ierr=nf_inq_varid (ncid, vname(1,indxT+itrc-1)(1:lvar),
     &                                                 vidT(itrc))
            if (ierr .ne. nf_noerr) then
              write(stdout,1) vname(1,indxT+itrc-1)(1:lvar),
     &                                       ncname(1:lstr) 
              goto 99                                     !--> ERROR
            endif
          endif
        enddo
!
! Density anomaly.
!
        if (wrt(indxR)) then
          lvar=lenstr(vname(1,indxR))
          ierr=nf_inq_varid (ncid, vname(1,indxR)(1:lvar), vidR)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxR)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
!
! S-coordinate "omega" vertical velocity.
!
        if (wrt(indxO)) then
          lvar=lenstr(vname(1,indxO))
          ierr=nf_inq_varid (ncid, vname(1,indxO)(1:lvar), vidO)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxO)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
!
! True vertical velocity.
!
        if (wrt(indxW)) then
          lvar=lenstr(vname(1,indxW))
          ierr=nf_inq_varid (ncid, vname(1,indxW)(1:lvar), vidW)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxW)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
# ifdef VIS_COEF_3D
!
! Horizontal viscosity coefficient.
!
        if (wrt(indxVisc)) then
          lvar=lenstr(vname(1,indxVisc))
          ierr=nf_inq_varid (ncid, vname(1,indxVisc)(1:lvar), vidVisc)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxVisc)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
# endif
# ifdef DIF_COEF_3D
!
! Horizontal diffusivity coefficient.
!
        if (wrt(indxDiff)) then
          lvar=lenstr(vname(1,indxDiff))
          ierr=nf_inq_varid (ncid, vname(1,indxDiff)(1:lvar), vidDiff)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxDiff)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
# endif
!
! Vertical viscosity coefficient.
!
        if (wrt(indxAkv)) then
          lvar=lenstr(vname(1,indxAkv))
          ierr=nf_inq_varid (ncid, vname(1,indxAkv)(1:lvar), vidAkv)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxAkv)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
!
! Vertical diffusion coefficient for potential temperature.
!
        if (wrt(indxAkt)) then
          lvar=lenstr(vname(1,indxAkt))
          ierr=nf_inq_varid (ncid,vname(1,indxAkt)(1:lvar), vidAkt)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxAkt)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif

# ifdef SALINITY
!
! Vertical diffusion coefficient for salinity.
!
        if (wrt(indxAks)) then
          lvar=lenstr(vname(1,indxAks))
          ierr=nf_inq_varid (ncid,vname(1,indxAks)(1:lvar), hisAks)
          if (ierr .ne. nf_noerr) then
            write(stdout,1) vname(1,indxAks)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
# endif
# ifdef LMD_SKPP
!
! Depth of surface planetary boundary layer.
!
        if (wrt(indxHbl)) then
          lvar=lenstr(vname(1,indxHbl))
          ierr=nf_inq_varid (ncid,vname(1,indxHbl)(1:lvar), vidHbl)
          if (ierr .ne. nf_noerr) then
          write(stdout,1) vname(1,indxHbl)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
# endif
# ifdef BIOLOGY
!
! Depth of the euphotic layer.
!
        if (wrt(indxHel)) then
          lvar=lenstr(vname(1,indxHel))
          ierr=nf_inq_varid (ncid,vname(1,indxHel)(1:lvar), vidHel)
          if (ierr .ne. nf_noerr) then
          write(stdout,1) vname(1,indxHel)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
!
! Chlorophyll/Carbon ratio.
!
#  ifdef BIO_NPZD
        if (wrt(indxChC)) then
          lvar=lenstr(vname(1,indxChC))
          ierr=nf_inq_varid (ncid,vname(1,indxChC)(1:lvar), vidChC)
          if (ierr .ne. nf_noerr) then
          write(stdout,1) vname(1,indxChC)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
#   ifdef OXYGEN
!
! Wind speed at 10 m
        if (wrt(indxU10)) then
          lvar=lenstr(vname(1,indxU10))
          ierr=nf_inq_varid (ncid,vname(1,indxU10)(1:lvar), vidU10)
          if (ierr .ne. nf_noerr) then
          write(stdout,1) vname(1,indxU10)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
!
! Gas exchange coefficient of O2
        if (wrt(indxKvO2)) then
          lvar=lenstr(vname(1,indxKvO2))
          ierr=nf_inq_varid (ncid,vname(1,indxKvO2)(1:lvar), vidChC)
          if (ierr .ne. nf_noerr) then
          write(stdout,1) vname(1,indxKvO2)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
!
! Saturation concentration of O2
        if (wrt(indxO2sat)) then
          lvar=lenstr(vname(1,indxO2sat))
          ierr=nf_inq_varid (ncid,vname(1,indxO2sat)(1:lvar), vidChC)
          if (ierr .ne. nf_noerr) then
          write(stdout,1) vname(1,indxO2sat)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
#   endif /* OXYGEN */
#  elif defined BIO_N2P2Z2D2 && defined VAR_CHL_C
        if (wrt(indxChC1)) then
          lvar=lenstr(vname(1,indxChC1))
          ierr=nf_inq_varid (ncid,vname(1,indxChC1)(1:lvar), vidChC1)
          if (ierr .ne. nf_noerr) then
          write(stdout,1) vname(1,indxChC1)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif

        if (wrt(indxChC2)) then
          lvar=lenstr(vname(1,indxChC2))
          ierr=nf_inq_varid (ncid,vname(1,indxChC2)(1:lvar), vidChC2)
          if (ierr .ne. nf_noerr) then
          write(stdout,1) vname(1,indxChC2)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
#  endif
# endif
# ifdef LMD_BKPP
!
! Depth of bottom planetary boundary layer.
!
        if (wrt(indxHbbl)) then
          lvar=lenstr(vname(1,indxHbbl))
          ierr=nf_inq_varid (ncid,vname(1,indxHbbl)(1:lvar), vidHbbl)
          if (ierr .ne. nf_noerr) then
          write(stdout,1) vname(1,indxHbbl)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
        endif
# endif
# ifdef SEDIMENT
!
! Sediment bed layer thickness, porosity, and volume fraction of 
! size class ised in sediment layer (2+NST r2dgrd variables)
!
        do itrc=1,NST+2
         indxWrk=indxSed+itrc-1
         if (wrt(indxWrk)) then
          lvar=lenstr(vname(1,indxWrk))
          ierr=nf_inq_varid (ncid,vname(1,indxWrk)(1:lvar), 
     &                                                vidSed(itrc))
          if (ierr .ne. nf_noerr) then
          write(stdout,1) vname(1,indxWrk)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
         endif
        enddo
# endif /*SEDIMENT */
# ifdef BBL
!
! Abed       Wind-induced, bed wave excursion amplitude (m)  
! Hripple    Bed ripple height (m)                           
! Lripple    Bed ripple length (m)                             
! Zbnot      Physical hydraulic bottom roughness (grain roughness) (m)                 
! Zbapp      Effective apparent hydraulic bottom roughness         
! bostrw     Wave-induced kinematic bottom stress (N/m2)    
!
        do itrc=1,6
         indxWrk=indxBBL+itrc-1
         if (wrt(indxWrk)) then
          lvar=lenstr(vname(1,indxWrk))
          ierr=nf_inq_varid (ncid,vname(1,indxWrk)(1:lvar), 
     &                                                vidBBL(itrc))
          if (ierr .ne. nf_noerr) then
          write(stdout,1) vname(1,indxWrk)(1:lvar), ncname(1:lstr)
            goto 99                                       !--> ERROR
          endif
         endif
        enddo
# endif /* BBL */
# ifdef BULK_FLUX
!
! Shflx     Surface Heat Flux components (degC/s)
!
        lvar=lenstr(vname(1,indxShflx_rsw))
        ierr=nf_inq_varid (ncid,vname(1,indxShflx_rsw)(1:lvar),
     &                                               vidShflx_rsw)
        if (ierr .ne. nf_noerr) then
        write(stdout,1) vname(1,indxShflx_rsw)(1:lvar), ncname(1:lstr)
          goto 99                                       !--> ERROR
        endif
        lvar=lenstr(vname(1,indxShflx_rlw))
        ierr=nf_inq_varid (ncid,vname(1,indxShflx_rlw)(1:lvar),
     &                                               vidShflx_rlw)
        if (ierr .ne. nf_noerr) then
        write(stdout,1) vname(1,indxShflx_rlw)(1:lvar), ncname(1:lstr)
          goto 99                                       !--> ERROR
        endif
        lvar=lenstr(vname(1,indxShflx_lat))
        ierr=nf_inq_varid (ncid,vname(1,indxShflx_lat)(1:lvar),
     &                                               vidShflx_lat)
        if (ierr .ne. nf_noerr) then
        write(stdout,1) vname(1,indxShflx_lat)(1:lvar), ncname(1:lstr)
          goto 99                                       !--> ERROR
        endif
        lvar=lenstr(vname(1,indxShflx_sen))
        ierr=nf_inq_varid (ncid,vname(1,indxShflx_sen)(1:lvar),
     &                                               vidShflx_sen)
        if (ierr .ne. nf_noerr) then
        write(stdout,1) vname(1,indxShflx_sen)(1:lvar), ncname(1:lstr)
          goto 99                                       !--> ERROR
        endif
# endif

#endif /* SOLVE3D */
        write(*,'(6x,2A,i4,1x,A,i4)') 'DEF_HIS/AVG -- Opened ',
     &                     'existing file  from record =', rec
     &                      MYID

#if defined MPI & !defined PARALLEL_FILES
      else
        ierr=nf_open (ncname(1:lstr), nf_write, ncid)
        if (ierr .ne. nf_noerr) then
          write(stdout,'(/1x,4A,2x,A,I4/)') 'DEF_HIS/AVG ERROR: ',
     &                'Cannot open file ''', ncname(1:lstr), '''.'
     &                 MYID
          goto 99                                         !--> ERROR
        endif
#endif
      endif             !<--  create_new_file  
      
      ierr=nf_set_fill (ncid, nf_nofill, lvar)
      if (ierr .ne. nf_noerr) then
        write(*,'(6x,2A,i4,1x,A,i4)') 'DEF_HIS/AVG ERROR: Cannot ', 
     &    'switch to ''nf_nofill'' more; netCDF error code =', ierr 
      endif
   1  format(/1x,'DEF_HIS/AVG ERROR: Cannot find variable ''',
     &                   A, ''' in netCDF file ''', A, '''.'/)
#if (defined PUT_GRID_INTO_HISTORY && !defined AVRH)\
 || (defined PUT_GRID_INTO_AVERAGES && defined AVRH)
!
! Write grid variables.
! ----- ---- ----------
!
        if (total_rec.le.1) call wrt_grid (ncid, ncname, lstr)
#endif
#if defined AVRH
!
! Set initial average time.
! --- ------- ------- -----
!
      if (ntsavg.eq.1) then
        time_avg=time-0.5*float(navg)*dt
      else
        time_avg=time-0.5*float(navg)*dt+float(ntsavg)*dt
      endif
#endif

  99  return
      end

#undef ncname
#undef rec_per_file
#undef wrt 
#undef vidTime
#undef vidTstep
#undef vidZ
#undef vidUb
#undef vidVb
#undef vidU
#undef vidV
#undef vidT
#undef vidR
#undef vidO
#undef vidW
#undef vidW
#undef vidVisc
#undef vidDiff
#undef vidAkv
#undef vidAkt
#undef vidAks
#undef vidHbl
#undef vidHbbl
#ifdef BIOLOGY
# undef vidHel
# ifdef BIO_NPZD
#  undef vidChC
#  ifdef OXYGEN
#   undef vidU10
#   undef vidKvO2
#   undef vidO2sat
#  endif
# elif defined BIO_N2P2Z2D2 && defined VAR_CHL_C
#  undef vidChC1
#  undef vidChC2
# endif
#endif
#undef vidSed
#undef vidBBL
#undef vidBostr
#undef vidShflx_rsw
#undef vidShflx_rlw
#undef vidShflx_lat
#undef vidShflx_sen

#ifdef AVERAGES
# ifndef AVRH
#  define AVRH
#  include "def_his.F"
# endif
#endif
