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
      SUBROUTINE p4zmicro(Istr,Iend,Jstr,Jend)
#include "cppdefs.h"
      Implicit NONE
      INTEGER Istr,Jstr,Iend,Jend

#if defined key_passivetrc && defined key_trc_pisces
CCC---------------------------------------------------------------------
CCC
CCC           ROUTINE p4zmicro : PISCES MODEL
CCC           *******************************
CCC
CCC  PURPOSE :
CCC  ---------
CCC         Compute the sources/sinks for microzooplankton
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
CC              None
CC
CC   MODIFICATIONS:
CC   --------------
CC      original  : O. Aumont (2004)
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
      REAL compadi,compadi2,compaz,compaph,compapoc
      REAL graze,zdenom,zdenom2
      REAL zfact,zstep,zinano,zidiat,zipoc
C
C    Time step duration for biology
C    ------------------------------
C
        zstep=rfact2/rjjss
C
      DO jk = 1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C
        compaz = max((trn(ji,jj,jk,jpzoo)-1.E-9),0.)
        zfact=zstep*tgfunc(ji,jj,jk)*compaz
C
C     Respiration rates of both zooplankton
C     -------------------------------------
C
        respz(ji,jj,jk) = resrat*zfact
     &    *(1.+3.*nitrfac(ji,jj,jk))
     &    *trn(ji,jj,jk,jpzoo)/(xkmort+trn(ji,jj,jk,jpzoo))
C
C     Zooplankton mortality. A square function has been selected with
C     no real reason except that it seems to be more stable and may
C     mimic predation.
C     ---------------------------------------------------------------
C
          tortz(ji,jj,jk) = mzrat*1E6*zfact*trn(ji,jj,jk,jpzoo)
C
          END DO
        END DO
      END DO
C
      DO jk = 1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C
        compadi = max((trn(ji,jj,jk,jpdia)-1E-8),0.)
        compadi2=min(compadi,5.E-7)
        compaph = max((trn(ji,jj,jk,jpphy)-2E-7),0.)
        compapoc=max((trn(ji,jj,jk,jppoc)-1E-8),0.)
C
C     Microzooplankton grazing
C     ------------------------
C
          zdenom2 = 1./(zprefp*compaph
     &      +zprefc*compapoc+zprefd*compadi2+rtrn)

          graze = grazrat*zstep*tgfunc(ji,jj,jk)
     &      *trn(ji,jj,jk,jpzoo)

          zinano=zprefp*compaph*zdenom2
          zipoc=zprefc*compapoc*zdenom2
          zidiat=zprefd*compadi2*zdenom2

          zdenom = 1./(xkgraz+zinano*compaph
     &      +zipoc*compapoc+zidiat*compadi2)

          grazp(ji,jj,jk) = graze*zinano*compaph*zdenom
          grazm(ji,jj,jk) = graze*zipoc*compapoc*zdenom
          grazsd(ji,jj,jk) = graze*zidiat*compadi2*zdenom

          grazpf(ji,jj,jk) = grazp(ji,jj,jk)*
     &      trn(ji,jj,jk,jpnfe)/(trn(ji,jj,jk,jpphy)+rtrn)

          grazpch(ji,jj,jk) = grazp(ji,jj,jk)*
     &      trn(ji,jj,jk,jpnch)/(trn(ji,jj,jk,jpphy)+rtrn)

          grazmf(ji,jj,jk) = grazm(ji,jj,jk)
     &      *trn(ji,jj,jk,jpsfe)/(trn(ji,jj,jk,jppoc)+rtrn)

          grazsf(ji,jj,jk) = grazsd(ji,jj,jk)
     &      *trn(ji,jj,jk,jpdfe)/(trn(ji,jj,jk,jpdia)+rtrn)

          grazss(ji,jj,jk) = grazsd(ji,jj,jk)
     &      *trn(ji,jj,jk,jpbsi)/(trn(ji,jj,jk,jpdia)+rtrn)

          grazsch(ji,jj,jk) = grazsd(ji,jj,jk)
     &      *trn(ji,jj,jk,jpdch)/(trn(ji,jj,jk,jpdia)+rtrn)
C
          END DO
        END DO
      END DO
C
      DO jk = 1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C
C    Various remineralization and excretion terms
C    --------------------------------------------
C
          grarem(ji,jj,jk)=(grazp(ji,jj,jk)+grazm(ji,jj,jk)
     &      +grazsd(ji,jj,jk))*(1.-epsher-unass)

          grafer(ji,jj,jk)=(grazpf(ji,jj,jk)+grazsf(ji,jj,jk)
     &      +grazmf(ji,jj,jk))*(1.-epsher-unass)
     &      +(grazm(ji,jj,jk)*max((trn(ji,jj,jk,jpsfe)/
     &      (trn(ji,jj,jk,jppoc)+rtrn)-ferat3),0.)
     &      +grazp(ji,jj,jk)*max((trn(ji,jj,jk,jpnfe)/
     &      (trn(ji,jj,jk,jpphy)+rtrn)-ferat3),0.)
     &      +grazsd(ji,jj,jk)*max((trn(ji,jj,jk,jpdfe)/
     &      (trn(ji,jj,jk,jpdia)+rtrn)-ferat3),0.))*epsher

          grapoc(ji,jj,jk)=(grazp(ji,jj,jk)+grazm(ji,jj,jk)
     &      +grazsd(ji,jj,jk))*unass
C
#    if defined key_trc_dia3d
        bioFlux(ji,jj,jk,12) = grapoc(ji,jj,jk)
     &      *tmask(ji,jj,jk)*1e3*rfact2r
#    endif
            END DO
          END DO
        END DO
C
#endif
      RETURN
      END

