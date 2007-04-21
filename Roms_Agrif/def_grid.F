!
! $Id: def_grid.F,v 1.2 2003/12/17 13:56:00 pmarches Exp $
!
#include "cppdefs.h"

      subroutine def_grid (ncid, r2dgrd)
!
! Define grid variables in output NetCDF file, which may be
! restart, history, averages, etc...  
!
! Arguments: ncid    NetCDF unit-ID of NetCDF file, which must be
!                            already opened and in definition mode; 
!            r2dgrd  integer array of size 2, which contains NetCDF
!                        IDs for dimensions 'xi_rho' and 'eta_rho'.
!
      implicit none
      integer ncid, r2dgrd(2), nf_ftype, varid, ierr
#include "param.h"
#include "ncscrum.h"
#include "netcdf.inc"
!
! Decide about precision:
!
      if (ncid.eq.ncidrst) then
        nf_ftype=NF_FTYPE
      else
        nf_ftype=NF_FOUT
      endif
!   
!
! Grid type switch: Spherical or Cartesian.
!
      ierr=nf_def_var (ncid, 'spherical', nf_char, 0, 0, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',24,
     &                                   'grid type logical switch')
      ierr=nf_put_att_text (ncid, varid, 'option_T', 9, 'spherical')
      ierr=nf_put_att_text (ncid, varid, 'option_F', 9, 'cartesian')
!
! Physical Dimensions of Model Domain, xl,el.
!
      ierr=nf_def_var (ncid, 'xl', nf_ftype, 0, 0, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',   33,
     &                   'domain length in the XI-direction')
      ierr=nf_put_att_text (ncid, varid, 'units', 5, 'meter')

      ierr=nf_def_var (ncid, 'el', nf_ftype, 0, 0, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',   34,
     &                  'domain length in the ETA-direction')
      ierr=nf_put_att_text (ncid, varid, 'units', 5, 'meter')
!
! Bathymetry.
!
      ierr=nf_def_var (ncid, 'h', nf_ftype, 2, r2dgrd, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',  24,
     &                            'bathymetry at RHO-points')
      ierr=nf_put_att_text (ncid, varid, 'units', 5, 'meter')
      ierr=nf_put_att_text (ncid, varid, 'field', 12,
     &                                        'bath, scalar')
!
! Coriolis Parameter.
!
      ierr=nf_def_var (ncid,'f', nf_ftype, 2, r2dgrd, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',   32,
     &                        'Coriolis parameter at RHO-points')
      ierr=nf_put_att_text (ncid, varid, 'units',  8, 'second-1')
      ierr=nf_put_att_text (ncid, varid, 'field', 16,
     &                                        'coriolis, scalar')
!
! Curvilinear coordinate metric coefficients pm,pn.
!
      ierr=nf_def_var (ncid, 'pm', nf_ftype, 2, r2dgrd, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name', 35,
     &                       'curvilinear coordinate metric in XI')
      ierr=nf_put_att_text (ncid, varid, 'units',  7,    'meter-1')
      ierr=nf_put_att_text (ncid, varid, 'field', 10, 'pm, scalar')

      ierr=nf_def_var (ncid, 'pn', nf_ftype, 2, r2dgrd, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name', 36,
     &                      'curvilinear coordinate metric in ETA')
      ierr=nf_put_att_text (ncid, varid, 'units',  7,    'meter-1')
      ierr=nf_put_att_text (ncid, varid, 'field', 10, 'pn, scalar')
!
! Longitude/latitude or cartezian coordinates of RHO-points.
!
#ifdef SPHERICAL
      ierr=nf_def_var (ncid, 'lon_rho', nf_ftype, 2, r2dgrd, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name', 23,
     &                                     'longitude of RHO-points')
      ierr=nf_put_att_text (ncid, varid, 'units', 11,  'degree_east')
      ierr=nf_put_att_text (ncid, varid, 'field', 15,
     &                                             'lon_rho, scalar')

      ierr=nf_def_var (ncid, 'lat_rho', nf_ftype, 2, r2dgrd, varid)
      ierr=nf_put_att_text (ncid,varid,'long_name',22,
     &                                      'latitude of RHO-points')
      ierr=nf_put_att_text (ncid, varid, 'units', 12,
     &                                                'degree_north')
      ierr=nf_put_att_text (ncid, varid, 'field', 15,
     &                                             'lat_rho, scalar')
#else
      ierr=nf_def_var (ncid, 'x_rho', nf_ftype, 2, r2dgrd, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name', 25,
     &                                   'x-locations of RHO-points')
      ierr=nf_put_att_text (ncid, varid, 'units', 5, 'meter')
      ierr=nf_put_att_text (ncid, varid, 'field',13, 'x_rho, scalar')

      ierr=nf_def_var (ncid, 'y_rho', nf_ftype, 2, r2dgrd, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',25,
     &                                   'y-locations of RHO-points')
      ierr=nf_put_att_text (ncid, varid, 'units', 5, 'meter')
      ierr=nf_put_att_text (ncid, varid, 'field',13, 'y_rho, scalar')
#endif
#ifdef CURVGRID
!
! Angle between XI-axis and EAST at RHO-points
!
      ierr=nf_def_var (ncid, 'angle', nf_ftype, 2, r2dgrd,varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',30,
     &                              'angle between XI-axis and EAST')
      ierr=nf_put_att_text (ncid, varid, 'units', 7, 'radians')
      ierr=nf_put_att_text (ncid, varid, 'field',13, 'angle, scalar')
#endif
#ifdef MASKING
!
! Land-Sea mask at RHO-points.
!
      ierr=nf_def_var (ncid, 'mask_rho', nf_ftype, 2, r2dgrd, varid)
      ierr=nf_put_att_text (ncid, varid, 'long_name',18,
     &                                          'mask on RHO-points')
      ierr=nf_put_att_text (ncid, varid, 'option_0',    4,   'land' )
      ierr=nf_put_att_text (ncid, varid, 'option_1',    5,   'water')
#endif
      return
      end

