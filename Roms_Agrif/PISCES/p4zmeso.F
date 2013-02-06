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
      SUBROUTINE p4zmeso(Istr,Iend,Jstr,Jend)
#include "cppdefs.h"
      Implicit NONE
      INTEGER Istr,Jstr,Iend,Jend

#if defined key_passivetrc && defined key_trc_pisces
CCC---------------------------------------------------------------------
CCC
CCC            ROUTINE p4zmeso : PISCES MODEL
CCC            ******************************
CCC
CCC  PURPOSE :
CCC  ---------
CCC         Compute the sources/sinks for mesozooplankton
CCC
CC   METHOD :
CC   -------
CC      
CC
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
      REAL compadi,compaph,compapoc,compaz
      REAL zfact,zstep,compam,zdenom,graze2
C
C
C
C     Time step duration for biology
C     ------------------------------
C
        zstep=rfact2/rjjss
C
      DO jk = 1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C
        compam=max((trn(ji,jj,jk,jpmes)-1.E-9),0.)
        zfact=zstep*tgfunc(ji,jj,jk)*compam
C
C     Respiration rates of both zooplankton
C     -------------------------------------
C
        respz2(ji,jj,jk) = resrat2*zfact
     &    *(1.+3.*nitrfac(ji,jj,jk))
     &    *trn(ji,jj,jk,jpmes)/(xkmort+trn(ji,jj,jk,jpmes))
C
C     Zooplankton mortality. A square function has been selected with
C     no real reason except that it seems to be more stable and may
C     mimic predation.
C     ---------------------------------------------------------------
C
        tortz2(ji,jj,jk) = mzrat2*1E6*zfact*trn(ji,jj,jk,jpmes)
C
          END DO
        END DO
      END DO

      DO jk = 1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C
        compadi = max((trn(ji,jj,jk,jpdia)-1E-8),0.)
        compaz = max((trn(ji,jj,jk,jpzoo)-1.E-8),0.)
        compaph = max((trn(ji,jj,jk,jpphy)-2E-7),0.)
        compapoc=max((trn(ji,jj,jk,jppoc)-1E-8),0.)
C
C     Microzooplankton grazing
C     ------------------------
C
        zdenom=1./(xkgraz2+xprefc*trn(ji,jj,jk,jpdia)
     &    +xprefz*trn(ji,jj,jk,jpzoo)
     &    +xprefp*trn(ji,jj,jk,jpphy)
     &    +xprefpoc*trn(ji,jj,jk,jppoc))

        graze2 = grazrat2*zstep*Tgfunc2(ji,jj,jk)*zdenom
     &    *trn(ji,jj,jk,jpmes)

        grazd(ji,jj,jk) = graze2*xprefc*compadi
        grazz(ji,jj,jk) = graze2*xprefz*compaz
        grazn(ji,jj,jk) = graze2*xprefp*compaph
        grazpoc(ji,jj,jk) = graze2*xprefpoc*compapoc

        graznf(ji,jj,jk) = grazn(ji,jj,jk)
     &    *trn(ji,jj,jk,jpnfe)/(trn(ji,jj,jk,jpphy)+rtrn)

        graznch(ji,jj,jk) = grazn(ji,jj,jk)
     &    *trn(ji,jj,jk,jpnch)/(trn(ji,jj,jk,jpphy)+rtrn)

        grazs(ji,jj,jk) = grazd(ji,jj,jk)
     &    *trn(ji,jj,jk,jpbsi)/(trn(ji,jj,jk,jpdia)+rtrn)

        grazf(ji,jj,jk) = grazd(ji,jj,jk)
     &    *trn(ji,jj,jk,jpdfe)/(trn(ji,jj,jk,jpdia)+rtrn)

        grazdch(ji,jj,jk) = grazd(ji,jj,jk)
     &    *trn(ji,jj,jk,jpdch)/(trn(ji,jj,jk,jpdia)+rtrn)

        grazpof(ji,jj,jk) = grazpoc(ji,jj,jk)
     &    *trn(ji,jj,jk,jpsfe)/(trn(ji,jj,jk,jppoc)+rtrn)
C
          END DO
        END DO
      END DO

      DO jk = 1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C
C    Mesozooplankton flux feeding on GOC
C    ----------------------------------
C
        grazffe(ji,jj,jk) = 5E3*zstep*wsbio4(ji,jj,jk)
     &    *tgfunc2(ji,jj,jk)*trn(ji,jj,jk,jpgoc)*trn(ji,jj,jk,jpmes)

        grazfff(ji,jj,jk) = grazffe(ji,jj,jk)
     &    *trn(ji,jj,jk,jpbfe)/(trn(ji,jj,jk,jpgoc)+rtrn)
C
          END DO
        END DO
      END DO

      DO jk = 1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C
C    Mesozooplankton efficiency
C    --------------------------
C
        grarem2(ji,jj,jk)=(grazd(ji,jj,jk)+grazz(ji,jj,jk)
     &    +grazn(ji,jj,jk)+grazpoc(ji,jj,jk)+grazffe(ji,jj,jk))
     &    *(1.-epsher2-unass2)

        grafer2(ji,jj,jk)=(grazf(ji,jj,jk)+graznf(ji,jj,jk)
     &    +grazz(ji,jj,jk)*ferat3+grazpof(ji,jj,jk)
     &    +grazfff(ji,jj,jk))*(1.-epsher2-unass2)
     &    +epsher2*(grazd(ji,jj,jk)*max(
     &    (trn(ji,jj,jk,jpdfe)/(trn(ji,jj,jk,jpdia)+rtrn)
     &    -ferat3),0.)+grazn(ji,jj,jk)*max(
     &    (trn(ji,jj,jk,jpnfe)/(trn(ji,jj,jk,jpphy)+rtrn)
     &    -ferat3),0.)+grazpoc(ji,jj,jk)*max(
     &    (trn(ji,jj,jk,jpsfe)/(trn(ji,jj,jk,jppoc)+rtrn)
     &    -ferat3),0.)+grazffe(ji,jj,jk)*max(
     &    (trn(ji,jj,jk,jpbfe)/(trn(ji,jj,jk,jpgoc)+rtrn)
     &    -ferat3),0.))

        grapoc2(ji,jj,jk)=(grazd(ji,jj,jk)+grazz(ji,jj,jk)
     &    +grazn(ji,jj,jk)+grazpoc(ji,jj,jk)+grazffe(ji,jj,jk))*unass2

#    if defined key_trc_dia3d
        bioFlux(ji,jj,jk,13) = grapoc2(ji,jj,jk)
     &          *tmask(ji,jj,jk)*1e3*rfact2r
#    endif

          END DO
        END DO
      END DO
C
#endif
      RETURN
      END

