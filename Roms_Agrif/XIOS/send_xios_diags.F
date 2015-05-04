! $Id: init_sta.F 697 2011-04-11 12:35:17Z gcambon $
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

#ifdef XIOS

      subroutine send_xios_diags (tile)
      implicit none
# include "param.h"
# ifdef SOLVE3D
#  include "work.h"
#  include "ncscrum.h"
# endif
      integer tile
# include "compute_tile_bounds.h"

      call send_xios_diags_tile (tile,Istr,Iend,Jstr,Jend)

      return
      end


      subroutine send_xios_diags_tile (tile,Istr,Iend,Jstr,Jend)

!
!================================================== John M. Klinck ===
!  Copyright (c) 2000 Rutgers/UCLA                                   !
!================================================ Hernan G. Arango ===
!                                                                    !
!  This routine sends every diagnostics fields to Xios server                                              !
!                                                                    !
!=====================================================================
!

          USE xios     ! XIOS io

      implicit none
# include "param.h"
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "scalars.h"
# include "work.h"
# include "private_scratch.h"
!
      integer tile
      integer Istr,Iend,Jstr,Jend
      integer i,j,k
      integer ilc
!
# include "compute_auxiliary_bounds.h"
!
#  define I_EXT_RANGE Istr-1,Iend+1
#  define J_EXT_RANGE Jstr-1,Jend+1


!       ilc=iic   ! number of time step since start
       ilc=1+iic-ntstart   ! number of time step since restart
       CALL xios_update_calendar(ilc)

! ------ 2D variables

      work2d(:,:) = zeta(:,:,fast_indx_out)
      call xios_send_field("ssh",work2d)
      
      work2d(:,:) = ubar(:,:,fast_indx_out)
      call xios_send_field("ubar",work2d)

      work2d(:,:) = vbar(:,:,fast_indx_out)
      call xios_send_field("vbar",work2d)

! wind stresses

! ------ 3D variables
! nstp vs nnew : present choice follows set_avg.F

      workr(:,:,:) = u(:,:,:,nstp)
      call xios_send_field("u",workr)
 
      workr(:,:,:) = v(:,:,:,nstp)
      call xios_send_field("v",workr)
 
      call Wvlcty (tile, work)
      call xios_send_field("w",work)
 
      workr(:,:,:) = t(:,:,:,nstp,itemp)
      call xios_send_field("T",workr)
 
# ifdef SALINITY
      workr(:,:,:) = t(:,:,:,nstp,isalt)
      call xios_send_field("S",workr)
# endif /* SALINITY */


      return
      end

#else   /* XIOS */

      subroutine send_xios_empty
      return
      end

#endif /* XIOS */

