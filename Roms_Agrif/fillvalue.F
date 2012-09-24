! $Id: $
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
      subroutine fillvalue2d(work2d_tmp,ncid,vid,indx,
     &                       record,typevar2d)
      implicit none
      
#include "param.h"
!     Declaration
      real work2d_tmp(GLOBAL_2D_ARRAY)
      logical mask(GLOBAL_2D_ARRAY) 
      real mask2(GLOBAL_2D_ARRAY)
      integer ierr, record, lstr, lvar, lenstr,
     &     nf_fwrite, typevar2d, vid, ncid, indx

      
!     Include the common      
#include "scalars.h"
#include "ncscrum.h"
#include "grid.h"
#include "netcdf.inc"
      if (typevar2d.eq.r2dvar) then
         mask2=rmask
      elseif (typevar2d.eq.u2dvar) then
         mask2=vmask
      elseif (typevar2d.eq.v2dvar)then
         mask2=vmask
      endif     
#ifdef FILLVAL
! Ici il faut mettre spval sur les indice ou rmask==0 .
      mask = (work2d_tmp.eq.mask2)
      WHERE (mask.eqv.mask_val)
          work2d_tmp(:,:) = spval
      END WHERE
#endif
!      print *,'ncid=',ncid
!      print *,'vid=',vid
!      print *,'typevar2d=',typevar2d
      ierr=nf_fwrite(work2d_tmp, ncid, vid, record, typevar2d)
      if (ierr .ne. nf_noerr) then
         lvar=lenstr(vname(1,indx))
         write(stdout,1) vname(1,indx)(1:lvar), record, ierr
     &        MYID
         goto 99                !--> ERROR
      endif
      
 1    format(/1x,'WRT_HIS ERROR while writing variable ''', A,
     &     ''' into history file.', /11x, 'Time record:',
     &     I6,3x,'netCDF error code',i4,3x,a,i4)
      goto 100 
  99  may_day_flag=3
 100  continue      
      
      return
      end

!====================================================================
      
      subroutine fillvalue3d(work3d_tmp,ncid,vid,indx,
     &                       record,typevar3d)
      implicit none
      
#include "param.h"
!     Declaration
      real work3d_tmp(GLOBAL_2D_ARRAY,N)
      real work2d_tmp(GLOBAL_2D_ARRAY)
      integer ierr, record, lstr, lvar, lenstr,
     &     nf_fwrite, typevar3d, vid, ncid, indx, k
      logical mask(GLOBAL_2D_ARRAY)
      real mask2(GLOBAL_2D_ARRAY)

!     Include the common      
#include "scalars.h"
#include "ncscrum.h"
#include "grid.h"
#include "netcdf.inc"
!     
      if (typevar3d.eq.r3dvar) then
         mask2=rmask
      elseif (typevar3d.eq.u3dvar) then
         mask2=vmask
      elseif (typevar3d.eq.v3dvar) then
         mask2=vmask
      endif      
!     
#ifdef FILLVAL
      work2d_tmp = work3d_tmp(:,:,1)
      mask = (work2d_tmp.eq.mask2)
        do k=1,N
          WHERE (mask.eqv.mask_val)
            work3d_tmp(:,:,k) = spval
          END WHERE
      enddo
#endif
!     print *,'ncid=',ncid
!     print *,'vid=',vid
!     print *,'typevar3d=',typevar3d
      ierr=nf_fwrite(work3d_tmp, ncid, vid, record,typevar3d)
      if (ierr .ne. nf_noerr) then
         lvar=lenstr(vname(1,indx))
         write(stdout,1) vname(1,indx)(1:lvar), record, ierr
     &        MYID
         goto 99                !--> ERROR
      endif
 
 1    format(/1x,'WRT_HIS ERROR while writing variable ''', A,
     &     ''' into history file.', /11x, 'Time record:',
     &     I6,3x,'netCDF error code',i4,3x,a,i4)
      goto 100 
  99  may_day_flag=3
 100  continue      
      
      return
      end

!====================================================================

      subroutine fillvalue3d_w(work3d_tmp,ncid,vid,indx,
     &                       record,typevar3d)
      implicit none
      
#include "param.h"
!     Declaration
      real work3d_tmp(GLOBAL_2D_ARRAY,0:N)
      real work2d_tmp(GLOBAL_2D_ARRAY)
      integer ierr, record, lstr, lvar, lenstr,
     &     nf_fwrite, typevar3d, vid, ncid, indx, k, nn
      logical mask(GLOBAL_2D_ARRAY)
      real mask2(GLOBAL_2D_ARRAY)

!     Include the common      
#include "scalars.h"
#include "ncscrum.h"
#include "grid.h"
#include "netcdf.inc"
!     
      mask2=rmask     
!     
#ifdef FILLVAL
      work2d_tmp = work3d_tmp(:,:,1)
      mask = (work2d_tmp.eq.mask2)
      do k=0,N
        WHERE (mask.eqv.mask_val)
          work3d_tmp(:,:,k) = spval
        END WHERE
      enddo
#endif
!     print *,'ncid=',ncid
!     print *,'vid=',vid
!     print *,'typevar3d=',typevar3d
      ierr=nf_fwrite(work3d_tmp, ncid, vid, record,typevar3d)
      if (ierr .ne. nf_noerr) then
         lvar=lenstr(vname(1,indx))
         write(stdout,1) vname(1,indx)(1:lvar), record, ierr
     &        MYID
         goto 99                !--> ERROR
      endif
      
 1    format(/1x,'WRT_HIS ERROR while writing variable ''', A,
     &     ''' into history file.', /11x, 'Time record:',
     &     I6,3x,'netCDF error code',i4,3x,a,i4)
      goto 100 
  99  may_day_flag=3
 100  continue      
      
      return
      end
