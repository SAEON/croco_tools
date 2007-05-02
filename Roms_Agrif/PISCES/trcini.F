      SUBROUTINE trcini(Istr,Iend,Jstr,Jend)
#include "cppdefs.h"
      Implicit NONE
      INTEGER Istr,Jstr,Iend,Jend

#ifdef PISCES
CCC---------------------------------------------------------------------
CCC
CCC                       ROUTINE trcini
CCC                     ******************
CCC
CCC  PURPOSE :
CCC  ---------
CCC     initialization for passive tracer
CCC     for restart or not
CCC
CC   METHOD :
CC   -------
CC
CC   INPUT :
CC   -----
CC      argument		: no
CC      COMMON
CC
CC   OUTPUT :
CC   ------
CC      argument                : no
CC      COMMON
CC
CC   WORKSPACE :
CC   ---------
CC      i j k
CC
CC
CC   EXTERNAL :
CC   --------
CC
CC   MODIFICATIONS:
CC   --------------
CC                 04/00 (O. Aumont, M.A. Foujols) HAMOCC3 and P3ZD
CC                 04/06 (C. Menkes) HAMOCC3 and P3ZD
CC----------------------------------------------------------------------
CC parameters and commons
CC ======================
# include "param.h"
# include "parameter.h"
# include "common.h"
CC----------------------------------------------------------------------
CC local declarations
CC ==================
# if defined key_passivetrc

      INTEGER i,j,k,itrc
CCC---------------------------------------------------------------------
CCC  OPA8, LODYC (15/11/96)
CCC---------------------------------------------------------------------
#    if defined key_trc_pisces
#    include "trcini.pisces.h"
#    else
C
C Specific initialization to do
C
#    endif
C
C 1. initialization of passives tracers field
C -------------------------------------------
      DO itrc=1,jptra
        DO k=1,jpk
          DO j=Jstr,Jend
            DO i=Istr,Iend
              trn(i,j,k,itrc)=0.e0
            END DO
          END DO
        END DO
      END DO
      WRITE(numout,*) ' '
      WRITE(numout,*) ' trcini: generic initialisation done '
      WRITE(numout,*) ' '
# else
C
C no passive tracers
C
# endif
C
#endif /* PISCES */
      RETURN
      END
