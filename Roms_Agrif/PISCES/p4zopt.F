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
      SUBROUTINE p4zopt(Istr,Iend,Jstr,Jend)
#include "cppdefs.h"
      Implicit NONE
      INTEGER Istr,Jstr,Iend,Jend

#if defined key_passivetrc && defined key_trc_pisces
CC---------------------------------------------------------------------
CC
CC             ROUTINE p4zopt : PISCES MODEL
CC             *****************************
CC
CC   PURPOSE :
CC   ---------
CC         Compute the light availability in the water column
CC         depending on the depth and the chlorophyll concentration
CC
CC   INPUT :
CC   -----
CC      argument
CC              None
CC      common
CC              all the common defined in ROMS
CC
CC
CC   OUTPUT :                   : no
CC   ------
CC
CC   MODIFICATIONS:
CC   --------------
CC      original  : O. Aumont (2004)
CC      modified  : P. Marchesiello, C. Menkes (2009)
CC                  (zmeu interpolation, code structure)
CC
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
      INTEGER ji, jj, jk, mrgb
      INTEGER kmld(Istr:Iend,Jstr:Jend)
      REAL xchl, cff1
      REAL ekg(PRIV_3D_BIOARRAY)
      REAL ekr(PRIV_3D_BIOARRAY)
      REAL ekb(PRIV_3D_BIOARRAY)
      REAL parlux
      REAL e1(PRIV_3D_BIOARRAY)
      REAL e2(PRIV_3D_BIOARRAY)
      REAL e3(PRIV_3D_BIOARRAY)
      REAL zdepmoy(Istr:Iend,Jstr:Jend)
      REAL etmp(Istr:Iend,Jstr:Jend)
      REAL zrlight,zblight,zglight
C
C     Initialisation of variables used to compute PAR
C     -----------------------------------------------
C
      parlux = 0.43/3.
      DO jk=1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
            e1(ji,jj,jk) = 0.
            e2(ji,jj,jk) = 0.
            e3(ji,jj,jk) = 0.
            etot(ji,jj,jk)=0.
          END DO
        END DO
      END DO
C
C     Separation in three light bands: red, green, blue
C     -------------------------------------------------
C
      DO jk=1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C
            xchl=(trn(ji,jj,jk,jpnch)+trn(ji,jj,jk,jpdch)+rtrn)*1.E6
            xchl=max(0.03,xchl)
            xchl=min(10.,xchl)
C
            mrgb = max(1,int(41+20.*log10(xchl)+rtrn))
C
            ekb(ji,jj,jk)=xkrgb(1,mrgb)
            ekg(ji,jj,jk)=xkrgb(2,mrgb)
            ekr(ji,jj,jk)=xkrgb(3,mrgb)
C
          END DO
        END DO
      END DO
C
C     Separation in three light bands: red, green, blue
C     -------------------------------------------------
C
C     Surface
C
      DO jj=Jstr, Jend
        DO ji=Istr, Iend
C
          zblight=0.5*ekb(ji,jj,1)*fse3t(ji,jj,1)
          zglight=0.5*ekg(ji,jj,1)*fse3t(ji,jj,1)
          zrlight=0.5*ekr(ji,jj,1)*fse3t(ji,jj,1)
C
          e1(ji,jj,1) = parlux*qsr(ji,jj)*exp(-zblight)
          e2(ji,jj,1) = parlux*qsr(ji,jj)*exp(-zglight)
          e3(ji,jj,1) = parlux*qsr(ji,jj)*exp(-zrlight)
C
        END DO
      END DO
C
C     Sub-surface
C
      DO jk = 2,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C
            zblight=0.5*(ekb(ji,jj,jk-1)*fse3t(ji,jj,jk-1)
     &                  +ekb(ji,jj,jk  )*fse3t(ji,jj,jk  ))
            zglight=0.5*(ekg(ji,jj,jk-1)*fse3t(ji,jj,jk-1)
     &                  +ekg(ji,jj,jk  )*fse3t(ji,jj,jk  ))
            zrlight=0.5*(ekr(ji,jj,jk-1)*fse3t(ji,jj,jk-1)
     &                  +ekr(ji,jj,jk  )*fse3t(ji,jj,jk  ))
C
            e1(ji,jj,jk) = e1(ji,jj,jk-1)*exp(-zblight)
            e2(ji,jj,jk) = e2(ji,jj,jk-1)*exp(-zglight)
            e3(ji,jj,jk) = e3(ji,jj,jk-1)*exp(-zrlight)
C
          END DO
        END DO
      END DO
C
      DO jk = 1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
            etot(ji,jj,jk) = e1(ji,jj,jk)+e2(ji,jj,jk)+e3(ji,jj,jk)
          END DO
        END DO
      END DO
C
C     Computation of the euphotic depth
C     ---------------------------------
C
      DO jj=Jstr, Jend
         DO ji=Istr, Iend
            zmeu(ji,jj) = fsdept(ji,jj,jpk)-fsdepw(ji,jj,1)
            zmeuindex(ji,jj) = jpk
         END DO
      END DO
      DO jk = 2,jpk
         DO jj=Jstr, Jend
            DO ji=Istr, Iend
               cff1=0.0043*qsr(ji,jj)
               IF (zmeuindex(ji,jj).eq.jpk .and. cff1.eq.0) THEN
                  zmeu(ji,jj)=0.
                  zmeuindex(ji,jj)=1
               ELSEIF (zmeuindex(ji,jj).eq.jpk .and.
     &                              etot(ji,jj,jk).lt.cff1) THEN
                  zmeu(ji,jj) = fsdept(ji,jj,jk)-fsdepw(ji,jj,1)-
     &                          fse3w(ji,jj,jk)*
     &                          (cff1-etot(ji,jj,jk))/
     &                          (etot(ji,jj,jk-1)-etot(ji,jj,jk))
                  zmeuindex(ji,jj)=jk-1
               ENDIF
            END DO
         END DO
      END DO
C
C    Computation of mean light in the mixed layer
C    --------------------------------------------
C
      DO jj=Jstr, Jend
         DO ji=Istr, Iend
            etmp(ji,jj) = 0.
            zdepmoy(ji,jj) = 0
         END DO
      END DO
      DO jk = 1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
             IF (fsdepw(ji,jj,jk+1).le.hmld(ji,jj)) THEN
                etmp(ji,jj) = etmp(ji,jj)+etot(ji,jj,jk)*fse3t(ji,jj,jk)
                zdepmoy(ji,jj)=zdepmoy(ji,jj)+fse3t(ji,jj,jk)
             ENDIF
          END DO
       END DO
      END DO
      DO jk = 1,jpk
         DO jj=Jstr, Jend
            DO ji=Istr, Iend
               IF (fsdepw(ji,jj,jk+1).le.hmld(ji,jj)) THEN
                  emoy(ji,jj,jk) = etmp(ji,jj)/(zdepmoy(ji,jj)+rtrn)
               ELSE
                  emoy(ji,jj,jk) = etot(ji,jj,jk)
               ENDIF
            END DO
         END DO
      END DO
C
C     Store diagnostic terms
C     ----------------------
C
#   if defined key_trc_diaadd
      DO jj=Jstr, Jend
         DO ji=Istr, Iend
            bioVSink(ji,jj,11) = zmeu(ji,jj)*tmask(ji,jj,1)
         END DO
      END DO
#    endif
#    if defined key_trc_dia3d
      DO jk = 1,jpk
         DO jj=Jstr, Jend
            DO ji=Istr, Iend
               bioFlux(ji,jj,jk,4)=etot(ji,jj,jk)*tmask(ji,jj,jk)
            END DO
         END DO
      ENDDO
#    endif
C
#endif
      RETURN
      END

