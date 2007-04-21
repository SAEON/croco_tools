      SUBROUTINE p4zslow(kt,Istr,Iend,Jstr,Jend)
#include "cppdefs.h"
      Implicit NONE
      Integer KT,Istr,Iend,Jstr,Jend
#if defined key_passivetrc && defined key_trc_pisces
CCC---------------------------------------------------------------------
CCC
CCC          ROUTINE p4zslow : PISCES MODEL
CCC          *****************************
CCC
CCC     PURPOSE.
CCC     --------
CCC          *P4ZSLOW* CALL VARIOUS ROUTINES ON A DAILY BASIS
CCC
CCC
CC     EXTERNALS.
CC     ----------
CC          p4zche, p4zint
CC
CC   MODIFICATIONS:
CC   --------------
CC      NEW : 2004 O. Aumont
CC----------------------------------------------------------------------
CC parameters and commons
CC ======================
#include "param.h"
#include "parameter.h"
#include "common.h"
CC----------------------------------------------------------------------
CC local declarations
CC ==================
C
C
C* 1. COMPUTATION OF THE CHEMICAL CONSTANTS
C  ----------------------------------------
C
       CALL p4zche(Istr,Iend,Jstr,Jend)
C
C  2. COMPUTATION OF VARIOUS RATES FOR BIOGEOCHEMISTRY
C  ---------------------------------------------------
C
       CALL p4zint(kt,Istr,Iend,Jstr,Jend)
C
C      
#endif
C
      RETURN
      END

