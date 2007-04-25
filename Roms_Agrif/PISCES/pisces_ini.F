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

      CALL ropigrid(Istr,Iend,Jstr,Jend)
      CALL ropistep(Istr,Iend,Jstr,Jend)
      CALL trclsm
      CALL trcini(Istr,Iend,Jstr,Jend)

      return
      end
#else
      subroutine pisces_ini_empty
      end
#endif /* key_passivetrc && key_trc_pisces */

