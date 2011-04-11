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
      SUBROUTINE p4zint(kt,Istr,Iend,Jstr,Jend)
#include "cppdefs.h"
      Implicit NONE
      Integer kt,Istr,Iend,Jstr,Jend
#if defined key_passivetrc && defined key_trc_pisces
CCC
CCC
CCC       ROUTINE p4zint : PISCES MODEL
CCC       *****************************
CCC
CC
CC   PURPOSE :
CC   ---------
CC           *P4ZINT* INTERPOLATION AND COMPUTATION OF 
CC                    VARIOUS ACCESSORY FIELDS
CC   INPUT :
CC   -----
CC      argument
CC              kt              : time step
CC
CC   EXTERNAL :
CC   ----------
CC          NONE
CC
CC   MODIFICATIONS:
CC   --------------
CC      original      : 2004    O. Aumont
CC ----------------------------------------------------------------
CC parameters and commons
CC ======================
#include "param.h"
#include "parameter.h"
#include "common.h"
CC-----------------------------------------------------------------
CC------
CC local declarations
CC ==================
C
      INTEGER ji, jj, jk
      INTEGER nspyr,nvit1t,nvit2t, iman
      REAL zpdtan, zdum, zpdtmo, zdemi, zt
C
C
      zpdtan = raass / rdt
      nspyr  = nint(zpdtan)
      iman   = 12
      zpdtmo = zpdtan / float(iman)
      zdemi  = zpdtmo / 2.
      zt     = ( float ( kt) + zdemi) / zpdtmo
      
C  recherche de l'indice des enregistrements
C  du modele dynamique encadrant le pas de temps kt.
C  --------------------------------------------------
C
      xtvit = zt - float(int ( zt))
      nvit1t = int (zt)
      nvit2t = nvit1t+1
      nvit1t = MOD ( nvit1t, iman)
      IF ( nvit1t .EQ. 0 ) nvit1t = iman
      nvit2t = MOD ( nvit2t, iman)
      IF ( nvit2t .EQ. 0 ) nvit2t = iman
C
C Interpolation of dust deposition
C --------------------------------
C
      DO jj = Jstr, Jend
        DO ji = Istr, Iend
      dust(ji,jj) = (1.-xtvit)*dustmo(ji,jj,nvit1t)
     &    +xtvit*dustmo(ji,jj,nvit2t)
        END DO
      END DO
C
C Computation of phyto and zoo metabolic rate
C -------------------------------------------
C
      DO jk = 1, jpk
        DO jj = Jstr, Jend
          DO ji = Istr, Iend
      Tgfunc(ji,jj,jk) = exp(0.063913*tn(ji,jj,jk))
      Tgfunc2(ji,jj,jk) = exp(0.07608*tn(ji,jj,jk))
          END DO
        END DO
      END DO
C
#endif
C
      RETURN
      END


