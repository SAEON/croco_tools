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
      SUBROUTINE p4znano(Istr,Iend,Jstr,Jend)
#include "cppdefs.h"
      Implicit NONE
      INTEGER Istr,Jstr,Iend,Jend

#if defined key_passivetrc && defined key_trc_pisces
CCC---------------------------------------------------------------------
CCC
CCC          ROUTINE p4znano : PISCES MODEL
CCC          ******************************
CCC
CCC  PURPOSE :
CCC  ---------
CCC         Compute the mortality terms for nanophytoplankton
CCC
CC   INPUT :
CC   -----
CC      argument
CC              None
CC      common
CC              all the common defined in opa
CC
CC
CC   OUTPUT :                   : no
CC   ------
CC
CC   EXTERNAL :
CC   --------
CC          None
CC
CC   MODIFICATIONS:
CC   --------------
CC      original  : O. Aumont (2002)
CC----------------------------------------------------------------------
CC parameters and commons
CC ======================
#include "param.h"
#include "parameter.h"
#include "common.h"
CC----------------------------------------------------------------------
CC local declarations
CC ==================
      INTEGER ji, jj, jk
      REAL zfact,zstep,compaph
C
C      Time step duration for biology
C      ------------------------------
C
        zstep=rfact2/rjjss
C
      DO jk = 1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C
        compaph = max((trn(ji,jj,jk,jpphy)-1E-8),0.)
        zfact=1./(trn(ji,jj,jk,jpphy)+rtrn)
C
C     Squared mortality of Phyto similar to a sedimentation term during
C     blooms (Doney et al. 1996)
C     -----------------------------------------------------------------
C
        respp(ji,jj,jk) = wchl*1e6*zstep*zdiss(ji,jj,jk)
     &    *compaph*trn(ji,jj,jk,jpphy)
                                                                               
        respnf(ji,jj,jk) = respp(ji,jj,jk)
     &    *trn(ji,jj,jk,jpnfe)*zfact
                                                                               
        respnch(ji,jj,jk) = respp(ji,jj,jk)
     &    *trn(ji,jj,jk,jpnch)*zfact
C
C     Phytoplankton mortality. This mortality loss is slightly
C     increased when nutrients are limiting phytoplankton growth
C     as observed for instance in case of iron limitation.
C     ----------------------------------------------------------
C
        tortp(ji,jj,jk) = mprat*zstep*trn(ji,jj,jk,jpphy)
     $    /(xkmort+trn(ji,jj,jk,jpphy))*compaph
                                                                               
        tortnf(ji,jj,jk)=tortp(ji,jj,jk)
     &    *trn(ji,jj,jk,jpnfe)*zfact
                                                                               
        tortnch(ji,jj,jk)=tortp(ji,jj,jk)
     &    *trn(ji,jj,jk,jpnch)*zfact
C
          END DO
        END DO
      END DO
C
#endif
      RETURN
      END

