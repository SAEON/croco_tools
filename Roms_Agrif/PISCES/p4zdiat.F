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
      SUBROUTINE p4zdiat(Istr,Iend,Jstr,Jend)
#include "cppdefs.h"
      Implicit none
      INTEGER Istr,Jstr,Iend,Jend

#if defined key_passivetrc && defined key_trc_pisces
CCC---------------------------------------------------------------------
CCC
CCC           ROUTINE p4zdiat : PISCES MODEL
CCC           ******************************
CCC
CCC  PURPOSE :
CCC  ---------
CCC         Compute the mortality terms for diatoms
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
CC           None
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
#define PRIV_3D_BIOARRAY Istr:Iend,Jstr:Jend,1:jpk
CC----------------------------------------------------------------------
CC local declarations
CC ==================
      INTEGER ji, jj, jk
      REAL zfact,zstep,compadi
      REAL wchl2n(PRIV_3D_BIOARRAY)
C
C      Time step duration for biology
C      ------------------------------
C
        zstep=rfact2/rjjss
C
C    Aggregation term for diatoms is increased in case of nutrient
C    stress as observed in reality. The stressed cells become more
C    sticky and coagulate to sink quickly out of the euphotic zone
C     ------------------------------------------------------------
C
      DO jk=1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
        wchl2n(ji,jj,jk)=wchl+wchld*(1.-xlimdia(ji,jj,jk))
          END DO
        END DO
      END DO

      DO jk = 1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C
        compadi = max((trn(ji,jj,jk,jpdia)-1E-8),0.)
        zfact=1./(trn(ji,jj,jk,jpdia)+rtrn)
C
C    Aggregation term for diatoms is increased in case of nutrient
C    stress as observed in reality. The stressed cells become more
C    sticky and coagulate to sink quickly out of the euphotic zone
C     ------------------------------------------------------------
C
        respp2(ji,jj,jk) = 1E6*zstep*wchl2n(ji,jj,jk)
     &    *zdiss(ji,jj,jk)*compadi*trn(ji,jj,jk,jpdia)
                                                                               
        respds(ji,jj,jk) = respp2(ji,jj,jk)
     &    *trn(ji,jj,jk,jpbsi)*zfact

        respdf(ji,jj,jk) = respp2(ji,jj,jk)
     &    *trn(ji,jj,jk,jpdfe)*zfact
                                                                               
        respdch(ji,jj,jk)=respp2(ji,jj,jk)
     &    *trn(ji,jj,jk,jpdch)*zfact
C
C     Phytoplankton mortality. 
C     ------------------------
C
        tortp2(ji,jj,jk) = mprat2*zstep*trn(ji,jj,jk,jpdia)
     &    /(xkmort+trn(ji,jj,jk,jpdia))*compadi
                                                                               
        tortds(ji,jj,jk) = tortp2(ji,jj,jk)
     &    *trn(ji,jj,jk,jpbsi)*zfact
                                                                               
        tortdf(ji,jj,jk)=tortp2(ji,jj,jk)
     &    *trn(ji,jj,jk,jpdfe)*zfact
                                                                               
        tortdch(ji,jj,jk)=tortp2(ji,jj,jk)
     &    *trn(ji,jj,jk,jpdch)*zfact
C
          END DO
        END DO
      END DO
C
#endif
      RETURN
      END

