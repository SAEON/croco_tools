#include "cppdefs.h"

      SUBROUTINE pisces_ini(tile)

      implicit none
#include "param.h"
      integer tile
#include "compute_tile_bounds.h"
C
#if defined key_passivetrc && defined key_trc_pisces
CC   ------------------------------------------------------------------
CCC
CCC   ROUTINE romspisces : PISCES MODEL
CCC   *****************************
CCC
CC
CC     PURPOSE.
CC     --------
CC          *ROMSPISCES* ECOSYSTEM MODEL IN THE WHOLE OCEAN
CC                       THIS ROUTINE COMPUTES THE DIFFERENT
C                        INTERACTIONS
CC                       BETWEEN THE DIFFERENT COMPARTMENTS OF THE
C                        MODEL
CC ----------------------------------------------------------------
CC parameters and commons
CC ======================

      CALL ropigrid(Istr,Iend,Jstr,Jend)
      CALL ropistep(Istr,Iend,Jstr,Jend)
      CALL trclsm
      CALL trcini(Istr,Iend,Jstr,Jend)

#endif
       END
