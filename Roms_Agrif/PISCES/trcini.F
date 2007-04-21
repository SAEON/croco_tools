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
CC      ji jj jk
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

      INTEGER ji,jj,jk,jn,jl
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
      DO jn=1,jptra
        DO jk=1,jpk
          DO jj=Jstr,Jend
            DO ji=Istr,Iend
              trn(ji,jj,jk,jn)=0.e0
            END DO
          END DO
        END DO
      END DO

CCC Christophe Menkes. This is already done in init_array.F in ROMS
c$$$#    if defined key_trc_diaadd
c$$$C initialization of output 2d and 3d arrays
c$$$      DO jn=1,jpdia2d
c$$$          DO jj=Jstr,Jend
c$$$            DO ji=Istr,Iend
c$$$            bioVSink(ji,jj,jn)=0.e0
c$$$          END DO
c$$$        END DO
c$$$      END DO
c$$$      DO jn=1,jpdia3d
c$$$        DO jk=1,jpk
c$$$          DO jj=Jstr,Jend
c$$$            DO ji=Istr,Iend
c$$$              bioFlux(ji,jj,jk,jn)=0.e0
c$$$            END DO
c$$$          END DO
c$$$        END DO
c$$$      END DO
c$$$#    endif

          WRITE(numout,*) ' '
          WRITE(numout,*) ' trcini: generic initialisation done '
          WRITE(numout,*) ' '
C
# else
C
C no passive tracers
C
# endif
C
#endif /* PISCES */
      RETURN
      END
