      SUBROUTINE trclsm
#include "cppdefs.h"
#ifdef PISCES
CCC---------------------------------------------------------------------
CCC
CCC                       ROUTINE trclsm
CCC                     ******************
CCC
CCC  PURPOSE :
CCC  ---------
CCC     READS specific NAMELIST for sms terms 
CCC
CC   METHOD :                   : no
CC   -------
CC
CC   INPUT :
CC   -----
CC
CC   OUTPUT :
CC   ------
CC
CC   WORKSPACE :                : no
CC   ---------
CC
CC   MODIFICATIONS:
CC   --------------
CC      original  : 99-10 (M.A. Foujols, M. Levy) passive tracer
CC                  00-04 (O. Aumont, M.A. Foujols) HAMOCC3 and P3ZD
CC                  02-05 (O. Aumont) AGE TRACER
CC----------------------------------------------------------------------
CC parameters and commons
CC ======================
# include "param.h"
# include "parameter.h"
# include "common.h"

# if defined key_passivetrc

#    if defined key_trc_pisces
#    include "trclsm.pisces.h"
#    else
C      user can define here his own sms namelist
#    endif
# else
C
C no passive tracers
C
# endif
#endif /* PISCES */
      RETURN
      END
