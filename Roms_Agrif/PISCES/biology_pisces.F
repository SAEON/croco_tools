      SUBROUTINE biology_pisces(kt,Istr,Iend,Jstr,Jend)
C
#include "cppdefs.h"
      INTEGER kt,Istr,Iend,Jstr,Jend
#if defined PISCES
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
      INTEGER ktask

      ktask=1

      CALL ropistep(Istr,Iend,Jstr,Jend)
      CALL p4zprg(ktask,kt,Istr,Iend,Jstr,Jend)
      CALL pirostep(Istr,Iend,Jstr,Jend)
#endif
       
      END
