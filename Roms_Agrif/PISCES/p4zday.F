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
      SUBROUTINE p4zday(Istr,Iend,Jstr,Jend)
#include "cppdefs.h"
      Implicit NONE
      INTEGER Istr,Jstr,Iend,Jend

#if defined key_passivetrc && defined key_trc_pisces
CCC---------------------------------------------------------------------
CCC
CCC          ROUTINE p4zday : PISCES MODEL
CCC          *****************************
CCC
CCC  PURPOSE :
CCC  ---------
CCC        PISCES : compute the day length depending on latitude
CCC                 and the day
CCC
CC   INPUT :
CC   -----
CC      argument
CC              ktask           : task identificator
CC      common
CC              all the common defined in opa
CC
CC
CC   OUTPUT :                   : no
CC   ------
CC
CC   EXTERNAL :
CC   --------
CC            None
CC
CC   MODIFICATIONS:
CC   --------------
CC      original  : E. Maier-Reimer (GBC 1993)
CC      additions : C. Le Quere (1999)
CC      modifications : O. Aumont (2004)
CC----------------------------------------------------------------------
CC parameters and commons
CC ======================
#include "param.h"
#include "parameter.h"
#include "common.h"
CC----------------------------------------------------------------------
CC local declarations
CC ==================
      INTEGER ji, jj
      REAL rum, delta, codel, phi, argu
C
C Get year
C --------
C
c       write(numout,*) 'jour ',njulian
C
          rum = FLOAT(njulian-80)/365.
C
      delta = SIN(rum*rpi*2.)*sin(rpi*23.5/180.)
      codel = asin(delta)

      DO jj = Jstr, Jend
        DO ji = Istr, Iend
          phi = gphit(ji,jj)*rpi/180.
          argu = tan(codel)*tan(phi)
          strn(ji,jj) = 0.
          argu=min(1.,argu)
          argu=max(-1.,argu)
          strn(ji,jj)=24.-2.*acos(argu)*180./rpi/15.
          strn(ji,jj)=max(strn(ji,jj),0.)
        END DO
      END DO
C
#endif
      RETURN
      END

