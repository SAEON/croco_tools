! $Id$
!
!=========================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France.
! The two other branches, from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al), are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
!
! ROMS_AGRIF website : http://roms.mpl.ird.fr
!=========================================================================
!
#include "cppdefs.h"

      subroutine biology_tile (Istr,Iend,Jstr,Jend)
!------------------------------------------------------------------
!
!   ROUTINE biology_pisces : PISCES MODEL
!   *************************************
!
!
!     PURPOSE.
!     --------
!          *ROMS_PISCES ECOSYSTEM MODEL FOR THE WHOLE OCEAN
!                       THIS ROUTINE COMPUTES INTERACTIONS
!                       BETWEEN THE DIFFERENT COMPARTMENTS OF THE
!                       MODEL
!----------------------------------------------------------------
      implicit none
      INTEGER Istr,Iend,Jstr,Jend
# include "param.h"
# include "scalars.h"
      INTEGER ktask

      ktask=1

      CALL ropistep(Istr,Iend,Jstr,Jend)
      CALL p4zprg(ktask,iic,Istr,Iend,Jstr,Jend)
      CALL pirostep(Istr,Iend,Jstr,Jend)

      RETURN       
      END
