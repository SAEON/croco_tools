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
                                 ! Create STATIONS data NetCDF file:
                                 ! Define its variables, dimensions,
                                 ! and attributes
      subroutine def_sta(ncid, total_rec, ierr,res) 
      
          implicit none
# include "param.h"
# include "mixing.h"
# include "ncscrum.h"
# include "nc_sta.h"
# include "sta.h"
# include "scalars.h"
# include "strings.h"
# include "netcdf.inc"
!
      logical create_new_file, res
      integer ncid, total_rec, ierr
      integer lstr, lvar, stadim, ftimedim, twodim
      integer pgrd(2), temp(2)
      integer lenstr
# ifdef SOLVE3D
      integer trcdim
#  ifdef ALL_SIGMA
      integer srho, allSigma(3)
#  endif
# endif /* SOLVE3D */
      character*65 vinfo(4)
      if (may_day_flag.ne.0) return      !-->  EXIT

      ierr=0
      lstr=lenstr(staname)
      if (nrpfsta.gt.0) then
        lvar=total_rec-(1+mod(total_rec-1, nrpfsta))
        call insert_time_index (staname, lstr, lvar, ierr)
        if (ierr .ne. 0) goto 99
      endif

!  Create a new station data file.
!---------------------------------
!
      create_new_file=ldefsta
      if (ncid.ne.-1) create_new_file=.false.

#if defined MPI & !defined PARALLEL_FILES
      if (mynode.gt.0) create_new_file=.false.
#endif
 10   if (create_new_file) then

        lstr=lenstr(staname)
        ierr=nf_create(staname(1:lstr),nf_clobber,ncid)
        if (ierr.ne.nf_noerr) then
          write(stdout,11) staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif

!
! Put global attributes.
! --- ------ -----------
!
        call put_global_atts (ncid, ierr)
        if (ierr.ne.nf_noerr) then
          write(stdout,11) staname(1:lstr)
          may_day_flag=3
          return                         !-->  EXIT
        endif

!
!  Define the dimensions of staggered fields.
!--------------------------------------------
!
        ierr=nf_def_dim(ncid,'stanum',nstas,stadim)
# ifdef SOLVE3D
        ierr=nf_def_dim(ncid,'tracer',NT,trcdim)
#  ifdef ALL_SIGMA
        ierr=nf_def_dim(ncid,'s_rho',N,srho)
#  endif
# endif /* SOLVE3D */
        ierr=nf_def_dim(ncid,'ftime',nf_unlimited,ftimedim)
        ierr=nf_def_dim(ncid,'two',2,twodim)
!
!  Define dimension vectors for point variables.
!
        pgrd(1)=stadim
        pgrd(2)=ftimedim
!
        temp(1)=twodim
        temp(2)=ftimedim
!
# ifdef ALL_SIGMA
        allSigma(1)=srho
        allSigma(2)=stadim
        allSigma(3)=ftimedim
# endif
!
!
!  Define variables and their attributes.
!-----------------------------------------------------------
!
!  Define time step and model time.

        vinfo(1)='time_step'
        vinfo(2)='time step and record numbers from initialization'
        vinfo(3)='nondimensionnal'
        vinfo(4)='time step/record number, vector, series'
        lvar=lenstr(vinfo(1))
        ierr=nf_def_var(ncid,vinfo(1)(1:lvar),nf_int,
     &                    2,temp,staTstep)
        lvar=lenstr(vinfo(2))
        ierr=nf_put_att_text(ncid,staTstep,'long_name',lvar,
     &                         vinfo(2)(1:lvar))
        lvar=lenstr(vinfo(3))
        ierr=nf_put_att_text(ncid,staTstep,'units',lvar,
     &                         vinfo(3)(1:lvar))
        lvar=lenstr(vinfo(4))
        ierr=nf_put_att_text(ncid,staTstep,'field',lvar,
     &                         vinfo(4)(1:lvar))

!
!  Define time
!
        lvar=lenstr(vname(1,indxTime))
        ierr=nf_def_var(ncid,vname(1,indxTime)(1:lvar),NF_FTYPE,
     &                    1,ftimedim,staTime)
        lvar=lenstr(vname(2,indxTime))
        ierr=nf_put_att_text(ncid,staTime,'long_name',lvar,
     &                         vname(2,indxTime)(1:lvar))
        lvar=lenstr(vname(3,indxTime))
        ierr=nf_put_att_text(ncid,staTime,'units',lvar,
     &                         vname(3,indxTime)(1:lvar))
        lvar=lenstr(vname(4,indxTime))
        ierr=nf_put_att_text(ncid,staTime,'field',lvar,
     &                         vname(4,indxTime)(1:lvar))


       if (wrtsta(indxstaGrd)) then
!
!  Define stations (lon,lat) or (x,y) and z locations.
!
# ifdef AGRIF
        vinfo(1)='grid_level'
        vinfo(2)='grid level in nested grid hierarchy'
        vinfo(3)='nondimensionnal'
        lvar=lenstr(vinfo(1))
        ierr=nf_def_var(ncid,vinfo(1)(1:lvar),nf_int,
     &                    1,stadim,staGlevel)
        lvar=lenstr(vinfo(2))
        ierr=nf_put_att_text(ncid,staGlevel,'long_name',lvar,
     &                         vinfo(2)(1:lvar))
        lvar=lenstr(vinfo(3))
        ierr=nf_put_att_text(ncid,staGlevel,'units',lvar,
     &                         vinfo(3)(1:lvar))
# endif /*AGRIF*/
# ifdef SPHERICAL
        vinfo(1)='lon'
        vinfo(2)='longitude of model control points'
        vinfo(3)='degree_east'
        vinfo(4)='lon, scalar, series'
        lvar=lenstr(vinfo(1))
        ierr=nf_def_var(ncid,vinfo(1)(1:lvar),NF_FTYPE,
     &                    1,stadim,staLon)
        lvar=lenstr(vinfo(2))
        ierr=nf_put_att_text(ncid,staLon,'long_name',lvar,
     &                         vinfo(2)(1:lvar))
        lvar=lenstr(vinfo(3))
        ierr=nf_put_att_text(ncid,staLon,'units',lvar,
     &                         vinfo(3)(1:lvar))
        lvar=lenstr(vinfo(4))
        ierr=nf_put_att_text(ncid,staLon,'field',lvar,
     &                         vinfo(4)(1:lvar))
        vinfo(1)='lat'
        vinfo(2)='latitude of model control points'
        vinfo(3)='degree_north'
        vinfo(4)='lat, scalar, series'
        lvar=lenstr(vinfo(1))
        ierr=nf_def_var(ncid,vinfo(1)(1:lvar),NF_FTYPE,
     &                    1,stadim,staLat)
        lvar=lenstr(vinfo(2))
        ierr=nf_put_att_text(ncid,staLat,'long_name',lvar,
     &                         vinfo(2)(1:lvar))
        lvar=lenstr(vinfo(3))
        ierr=nf_put_att_text(ncid,staLat,'units',lvar,
     &                         vinfo(3)(1:lvar))
        lvar=lenstr(vinfo(4))
        ierr=nf_put_att_text(ncid,staLat,'field',lvar,
     &                         vinfo(4)(1:lvar))
# endif  /* SPHERICAL */
!
! Define Station X-position in the grid
!
        vinfo(1)='Xgrid'
        vinfo(2)='x-grid sta locations'
        vinfo(3)='nondimensional'
        vinfo(4)='Xgrid, scalar, series'
        lvar=lenstr(vinfo(1))
        ierr=nf_def_var(ncid,vinfo(1)(1:lvar),NF_FTYPE,
     &                    1,stadim,staXgrd)
        lvar=lenstr(vinfo(2))
        ierr=nf_put_att_text(ncid,staXgrd,'long_name',lvar,
     &                         vinfo(2)(1:lvar))
        lvar=lenstr(vinfo(3))
        ierr=nf_put_att_text(ncid,staXgrd,'units',lvar,
     &                         vinfo(3)(1:lvar))
        lvar=lenstr(vinfo(4))
        ierr=nf_put_att_text(ncid,staXgrd,'field',lvar,
     &                         vinfo(4)(1:lvar))
!
! Define float Y-position in the grid
!
        vinfo(1)='Ygrid'
        vinfo(2)='y-grid sta locations'
        vinfo(3)='nondimensional'
        vinfo(4)='Ygrid, scalar, series'
        lvar=lenstr(vinfo(1))
        ierr=nf_def_var(ncid,vinfo(1)(1:lvar),NF_FTYPE,
     &                    1,stadim,staYgrd)
        lvar=lenstr(vinfo(2))
        ierr=nf_put_att_text(ncid,staYgrd,'long_name',lvar,
     &                         vinfo(2)(1:lvar))
        lvar=lenstr(vinfo(3))
        ierr=nf_put_att_text(ncid,staYgrd,'units',lvar,
     &                         vinfo(3)(1:lvar))
        lvar=lenstr(vinfo(4))
        ierr=nf_put_att_text(ncid,staYgrd,'field',lvar,
     &                         vinfo(4)(1:lvar))
#  ifdef SOLVE3D
!
!  Define depth.
!
#   ifdef ALL_SIGMA
        vinfo(1)='depth'
        vinfo(2)='depth of stations levels'
        vinfo(3)='meter'
        lvar=lenstr(vinfo(1))
        ierr=nf_def_var(ncid,vinfo(1)(1:lvar),NF_FTYPE,
     &                    3,allSigma,staDepth)
        lvar=lenstr(vinfo(2))
        ierr=nf_put_att_text(ncid,staDepth,'long_name',lvar,
     &                         vinfo(2)(1:lvar))
        lvar=lenstr(vinfo(3))
        ierr=nf_put_att_text(ncid,staDepth,'units',lvar,
     &                         vinfo(3)(1:lvar))
#   else
        vinfo(1)='depth'
        vinfo(2)='depth of point stations'
        vinfo(3)='meter'
        lvar=lenstr(vinfo(1))
        ierr=nf_def_var(ncid,vinfo(1)(1:lvar),NF_FTYPE,
     &                    1,pgrd,staDepth)
        lvar=lenstr(vinfo(2))
        ierr=nf_put_att_text(ncid,staDepth,'long_name',lvar,
     &                         vinfo(2)(1:lvar))
        lvar=lenstr(vinfo(3))
        ierr=nf_put_att_text(ncid,staDepth,'units',lvar,
     &                         vinfo(3)(1:lvar))
#   endif
!
! Define  vertical positions S or depth(m)
!
#   ifdef ALL_SIGMA 
! Do nothing in this case
! Zgird is allways 1:N
#   else
        vinfo(1)='Zgrid'
        vinfo(2)='time varing sigma level of point stations'
        vinfo(3)='nondimensional'
        vinfo(4)='Zgrid, scalar, series'
        lvar=lenstr(vinfo(1))
        ierr=nf_def_var(ncid,vinfo(1)(1:lvar),NF_FTYPE,
     &                    2,pgrd,staZgrd)
        lvar=lenstr(vinfo(2))
        ierr=nf_put_att_text(ncid,staZgrd,'long_name',lvar,
     &                         vinfo(2)(1:lvar))
        lvar=lenstr(vinfo(3))
        ierr=nf_put_att_text(ncid,staZgrd,'units',lvar,
     &                         vinfo(3)(1:lvar))
        lvar=lenstr(vinfo(4))
        ierr=nf_put_att_text(ncid,staZgrd,'field',lvar,
     &                         vinfo(4)(1:lvar))
#   endif
#  endif /* SOLVE3D */
       endif ! define output of grid parameters
!
! Define veriables for station data
!
# ifdef SOLVE3D
       if (wrtsta(indxstaTemp)) then 
!
!  Define temperature
!
#  ifdef ALL_SIGMA
        vinfo(1)='temp'
        vinfo(2)='temperature'
        vinfo(3)='degrees Celsius'
        lvar=lenstr(vinfo(1))
        ierr=nf_def_var(ncid,vinfo(1)(1:lvar),NF_FTYPE,
     &                    3,AllSigma,staTemp)
        lvar=lenstr(vinfo(2))
        ierr=nf_put_att_text(ncid,staTemp,'long_name',lvar,
     &                         vinfo(2)(1:lvar))
        lvar=lenstr(vinfo(3))
        ierr=nf_put_att_text(ncid,staTemp,'units',lvar,
     &                         vinfo(3)(1:lvar))
#  else
        vinfo(1)='temp'
        vinfo(2)='temperature'
        vinfo(3)='degrees Celsius'
        lvar=lenstr(vinfo(1))
        ierr=nf_def_var(ncid,vinfo(1)(1:lvar),NF_FTYPE,
     &                    2,pgrd,staTemp)
        lvar=lenstr(vinfo(2))
        ierr=nf_put_att_text(ncid,staTemp,'long_name',lvar,
     &                         vinfo(2)(1:lvar))
        lvar=lenstr(vinfo(3))
        ierr=nf_put_att_text(ncid,staTemp,'units',lvar,
     &                         vinfo(3)(1:lvar))
#  endif
       endif ! def temperature
#  ifdef SALINITY
       if (wrtsta(indxstaSalt)) then
!
!  Define salinity
!
#   ifdef ALL_SIGMA
        vinfo(1)='salt'
        vinfo(2)='salinity'
        vinfo(3)='PSU'
        lvar=lenstr(vinfo(1))
        ierr=nf_def_var(ncid,vinfo(1)(1:lvar),NF_FTYPE,
     &                    3,allSigma,staSal)
        lvar=lenstr(vinfo(2))
        ierr=nf_put_att_text(ncid,staSal,'long_name',lvar,
     &                         vinfo(2)(1:lvar))
        lvar=lenstr(vinfo(3))
        ierr=nf_put_att_text(ncid,staSal,'units',lvar,
     &                         vinfo(3)(1:lvar))
#   else
        vinfo(1)='salt'
        vinfo(2)='salinity'
        vinfo(3)='PSU'
        lvar=lenstr(vinfo(1))
        ierr=nf_def_var(ncid,vinfo(1)(1:lvar),NF_FTYPE,
     &                    2,pgrd,staSal)
        lvar=lenstr(vinfo(2))
        ierr=nf_put_att_text(ncid,staSal,'long_name',lvar,
     &                         vinfo(2)(1:lvar))
        lvar=lenstr(vinfo(3))
        ierr=nf_put_att_text(ncid,staSal,'units',lvar,
     &                         vinfo(3)(1:lvar))
#   endif
       endif ! def salinity
#  endif  /* SALINITY */
       if (wrtsta(indxstaRho)) then 
!
!  Define density anomaly.
!
#  ifdef ALL_SIGMA
        vinfo(1)='rho'
        vinfo(2)='density anomaly'
        vinfo(3)='kilogram meter-3'
        lvar=lenstr(vinfo(1))
        ierr=nf_def_var(ncid,vinfo(1)(1:lvar),NF_FTYPE,
     &                    3,allSigma,staDen)
        lvar=lenstr(vinfo(2))
        ierr=nf_put_att_text(ncid,staDen,'long_name',lvar,
     &                         vinfo(2)(1:lvar))
        lvar=lenstr(vinfo(3))
        ierr=nf_put_att_text(ncid,staDen,'units',lvar,
     &                         vinfo(3)(1:lvar))
#  else
        vinfo(1)='rho'
        vinfo(2)='density anomaly'
        vinfo(3)='kilogram meter-3'
        lvar=lenstr(vinfo(1))
        ierr=nf_def_var(ncid,vinfo(1)(1:lvar),NF_FTYPE,
     &                    2,pgrd,staDen)
        lvar=lenstr(vinfo(2))
        ierr=nf_put_att_text(ncid,staDen,'long_name',lvar,
     &                         vinfo(2)(1:lvar))
        lvar=lenstr(vinfo(3))
        ierr=nf_put_att_text(ncid,staDen,'units',lvar,
     &                         vinfo(3)(1:lvar))
#  endif
       endif
       if (wrtsta(indxstaVel)) then
!
!  Velocity components
!
#  ifdef ALL_SIGMA
        vinfo(1)='v'
        vinfo(2)='V component velocity'
        vinfo(3)='meter/s'
        lvar=lenstr(vinfo(1))
        ierr=nf_def_var(ncid,vinfo(1)(1:lvar),NF_FTYPE,
     &                    3,allSigma,staV)
        lvar=lenstr(vinfo(2))
        ierr=nf_put_att_text(ncid,staV,'long_name',lvar,
     &                         vinfo(2)(1:lvar))
        lvar=lenstr(vinfo(3))
        ierr=nf_put_att_text(ncid,staV,'units',lvar,
     &                         vinfo(3)(1:lvar))
        vinfo(1)='u'
        vinfo(2)='U component velocity'
        vinfo(3)='meter/s'
        lvar=lenstr(vinfo(1))
        ierr=nf_def_var(ncid,vinfo(1)(1:lvar),NF_FTYPE,
     &                   3,allSigma,staU)
        lvar=lenstr(vinfo(2))
        ierr=nf_put_att_text(ncid,staU,'long_name',lvar,
     &                         vinfo(2)(1:lvar))
        lvar=lenstr(vinfo(3))
        ierr=nf_put_att_text(ncid,staU,'units',lvar,
     &                         vinfo(3)(1:lvar))
#  else
        vinfo(1)='v'
        vinfo(2)='V component velocity'
        vinfo(3)='meter/s'
        lvar=lenstr(vinfo(1))
        ierr=nf_def_var(ncid,vinfo(1)(1:lvar),NF_FTYPE,
     &                    2,pgrd,staV)
        lvar=lenstr(vinfo(2))
        ierr=nf_put_att_text(ncid,staV,'long_name',lvar,
     &                         vinfo(2)(1:lvar))
        lvar=lenstr(vinfo(3))
        ierr=nf_put_att_text(ncid,staV,'units',lvar,
     &                         vinfo(3)(1:lvar))
        vinfo(1)='u'
        vinfo(2)='U component velocity'
        vinfo(3)='meter/s'
        lvar=lenstr(vinfo(1))
        ierr=nf_def_var(ncid,vinfo(1)(1:lvar),NF_FTYPE,
     &                   2,pgrd,staU)
        lvar=lenstr(vinfo(2))
        ierr=nf_put_att_text(ncid,staU,'long_name',lvar,
     &                         vinfo(2)(1:lvar))
        lvar=lenstr(vinfo(3))
        ierr=nf_put_att_text(ncid,staU,'units',lvar,
     &                         vinfo(3)(1:lvar))
#  endif
       endif ! def velocity

# endif  /* SOLVE3D */
 
 
 
!  Leave definition mode.
!---------------------------
!
        ierr=nf_enddef(ncid)
        res=.true. ! marker that a file has been created
        write(stdout,'(6x,4A,1x,A,i4)') 'DEF_STATIONS - Created ',
     &                'new netCDF file ''', staname(1:lstr), '''.'
     &                 MYID



      elseif (ncid.eq.-1) then

! at this stage DO NOTHING

      endif  ! create or open

   1  format(/1x,'DEF_HIS/AVG ERROR: Cannot find variable ''',
     &                   A, ''' in netCDF file ''', A, '''.'/)

  11  format(/' DEF_STATIONS - unable to create floats file: ',a)
  20  format(/' DEF_STATIONS - error while writing variable: ',a,
     &        /,15x,'into floats file: ',a)

 99   return
      end
#else
      subroutine def_sta_empty
      return
      end
#endif /* STATIONS */
