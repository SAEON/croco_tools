!
! $Id$
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
