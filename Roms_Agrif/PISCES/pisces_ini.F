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
#if defined key_passivetrc && defined key_trc_pisces

      subroutine pisces_ini(tile)

      implicit none
      integer tile
#include "param.h"
#include "compute_tile_bounds.h"
      call pisces_ini_tile (Istr,Iend,Jstr,Jend)
      return
      end

      subroutine pisces_ini_tile (Istr,Iend,Jstr,Jend)
CC
CC -----------------------------------------------------------------
CC
CC   ROUTINE romspisces : PISCES MODEL
CC   *****************************
CC
CC
CC     PURPOSE.
CC     --------
CC          *ROMSPISCES* ECOSYSTEM MODEL IN THE WHOLE OCEAN
CC                       THIS ROUTINE COMPUTES THE DIFFERENT
CC                       INTERACTIONS
CC                       BETWEEN THE DIFFERENT COMPARTMENTS OF THE
CC                       MODEL
CC ----------------------------------------------------------------
CC parameters and commons
CC ======================
      implicit none
      integer Istr,Iend,Jstr,Jend

      CALL ropigrid(Istr,Iend,Jstr,Jend)
      CALL ropistep(Istr,Iend,Jstr,Jend)
!      CALL trclsm
      CALL trcini(Istr,Iend,Jstr,Jend)

      return
      end
#else
      subroutine pisces_ini_empty
      end
#endif /* key_passivetrc && key_trc_pisces */

