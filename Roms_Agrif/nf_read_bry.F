!
! $Id: nf_read_bry.F,v 1.2 2005/10/06 12:56:21 pmarches Exp $
!
#ifndef EAST_WEST 
# include "cppdefs.h"
      integer function nf_read_bry_NS (A, ncid, varid, record, type)
#else
      integer function nf_read_bry_EW (A, ncid, varid, record, type)
#endif
!
! Read side boundary from an input netCDF file.
!
! Arguments: A       real array of standard horizontal dimensions
!                             which is to be read or written.
!            ncid    NetCDF ID of in the file.
!            varid   variable ID of that variable in NetCDF file.
!            record  record number.
!            type    type of the grid (RHO-, U, V, W, PSI etc.)
!
! Because significant portion of the code calculates starting and
! stopping indices for the subarray (which are exactly the same for
! both EAST_WEST and NORTH_SOUTH boundaries) the code has been
! unified
!
      implicit none
#include "param.h"
#ifdef EAST_WEST 
      real A(0:Mm+1,N)
#else
      real A(0:Lm+1,N) 
#endif
      integer ncid, varid, record, type,  i,j,k, shft, ierr
      integer xtype, ndims, dimid(8), natts
      character*12 vname
#include "netcdf.inc"
#include "buffer.h"
#if defined MPI
# include "scalars.h"
#endif
#include "compute_starts_counts.h"

      if (ierr.eq.0) then
!
!---------------------------------------------------------------
! Find subdomain indices for extracting data from input files
!---------------------------------------------------------------
!
        start(1)=1
        start(2)=1
#ifdef MPI
# ifdef PARALLEL_FILES
#  ifdef EW_PERIODIC
        imin=1
        imax=Lm
#  else
        if (ii.gt.0) imin=1
        if (ii.eq.NP_XI-1) then
          imax=Lm+1
        else
          imax=Lm
        endif
#  endif
#  ifdef NS_PERIODIC
        jmin=1
        jmax=Mm
#  else
        if (jj.gt.0) jmin=1
        if (jj.eq.NP_ETA-1) then
          jmax=Mm+1
        else
          jmax=Mm
        endif
#  endif
# else
        if (ii.gt.0) then
          start(1)=1-imin+iminmpi
          imin=1
        endif
        if (ii.eq.NP_XI-1) then
          imax=Lmmpi+1
        else
          imax=Lmmpi
        endif
        if (jj.gt.0) then
          start(2)=1-jmin+jminmpi
          jmin=1
        endif
        if (jj.eq.NP_ETA-1) then
          jmax=Mmmpi+1
        else
          jmax=Mmmpi
        endif
# endif
#else
        imax=Lm+1
        jmax=Mm+1
#endif
        count(1)=imax-imin+1
        count(2)=jmax-jmin+1
!
!---------------------------------------------------------------
! Collapse one of the two horizontal dimensions
!---------------------------------------------------------------
!
#ifdef EAST_WEST 
        start(1)=start(2)     ! Collapse one of the two horizontal
        count(1)=count(2)     ! dimensions: after this step updated
#endif
        start(2)=start(3)     ! start/count(1) corresponds to either
        count(2)=count(3)     ! XI- or ETA-dimensions; index (2) to
        start(3)=start(4)     ! vertical dimension (if any) or time
        count(3)=count(4)     ! record; (3) to time record.
!
!---------------------------------------------------------------
! Read array from the disk.
!---------------------------------------------------------------
!
        ierr=nf_get_vara_FTYPE (ncid, varid, start, count, buff)
        if (ierr .eq. nf_noerr) then
          do k=1,count(2)
#ifdef EAST_WEST
            shft=1-jmin + (k-1)*count(1)
            do j=jmin,jmax
              A(j,k)=buff(j+shft)
            enddo
#else
            shft=1-imin + (k-1)*count(1)
            do i=imin,imax
              A(i,k)=buff(i+shft)
            enddo
#endif
          enddo
#if defined EW_PERIODIC || defined NS_PERIODIC  || defined MPI
!...
!... Exchange periodic and computational margins (not implemented yet)
!...
#endif
        else
          write(*,'(/1x,2A,I5/1x,3A,I4/)') 'ERROR in nf_read_bry: ',
     &                     'nf_get_vara netCDF error code =', ierr,
     &                     'Cause of error: ', nf_strerror(ierr)
     &                     MYID
        endif

      else
#ifdef EAST_WEST
        write(*,'(/1x,2A,I4/)') 'ERROR in nf_read_bry_EW: ',
#else
        write(*,'(/1x,2A,I4/)') 'ERROR in nf_read_bry_NS: ',
#endif
     &                          'illegal grid type', type
      endif
#ifdef EAST_WEST 
      nf_read_bry_EW=ierr
#else
      nf_read_bry_NS=ierr
#endif
      return
      end
 
#ifndef EAST_WEST 
# define EAST_WEST 
# include "nf_read_bry.F"
#endif
