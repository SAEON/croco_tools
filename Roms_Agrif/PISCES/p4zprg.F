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
       SUBROUTINE p4zprg(ktask,kt,Istr,Iend,Jstr,Jend)
#include "cppdefs.h"
      Implicit none
      INTEGER ktask, kt, Istr, Jstr, Iend, Jend
#if defined key_passivetrc && defined key_trc_pisces
CCC---------------------------------------------------------------------
CCC
CCC           ROUTINE p4zprg : PISCES MODEL
CCC           *****************************
CCC
CCC  PURPOSE :
CCC  ---------
CCC     Call Biological sources and sinks subroutines
CCC
CC   INPUT :
CC   -----
CC      argument
CC              ktask           : task identificator
CC              kt              : time step
CC      common
CC              all the common defined in opa
CC
CC
CC   OUTPUT :                   : no
CC   ------
CC
CC   WORKSPACE :
CC   ---------
CC
CC   EXTERNAL :
CC   --------
CC      p4zslow, p4zlys, p4zbio, p4zsed, p4zflx
CC
CC   MODIFICATIONS:
CC   --------------
CC      original  : O. AUMONT (2004)
CC----------------------------------------------------------------------
CC parameters and commons
CC ======================
#include "param.h"
#include "parameter.h"
#include "common.h"
CC----------------------------------------------------------------------
CC local declarations
CC ==================
      INTEGER ji, jj, jk, jn, jnt
C
C this part is without macrotasking coding
C
      IF (ktask.EQ.1) THEN
C
C Call an intermediate routine that in turns, calls chemistry
C and another routine on a daily basis
C -----------------------------------------------------------
C
      CALL p4zslow(kt,Istr,Iend,Jstr,Jend)
C
C Begin loop for biology with a possible shorter time step
C than for transport
C --------------------------------------------------------
C
       do jnt=1,nrdttrc
C
C Compute biological sources and sinks
C ------------------------------------
C
          CALL p4zbio(Istr,Iend,Jstr,Jend)
C
C Compute loss of nutrients in sediments
C --------------------------------------
C
         CALL p4zsed(Istr,Iend,Jstr,Jend)
C
         DO jn=1,jptra
           DO jk=1,jpk
             DO jj=Jstr, Jend
               DO ji=Istr, Iend
               trb(ji,jj,jk,jn)=trn(ji,jj,jk,jn)
               END DO
             END DO
           END DO
         END DO
       end do
C
C Compute CaCO3 saturation
C ------------------------
C
      CALL p4zlys(Istr,Iend,Jstr,Jend)
C
C Compute surface fluxes
C ----------------------
C
      CALL p4zflx(kt,Istr,Iend,Jstr,Jend)

      ENDIF 
C
C......................................................................
C
#endif
C
      RETURN
      END

