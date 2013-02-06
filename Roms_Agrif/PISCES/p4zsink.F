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
      SUBROUTINE p4zsink(Istr,Iend,Jstr,Jend)
#include "cppdefs.h"
      Implicit NONE
      INTEGER Istr,Jstr,Iend,Jend

#if defined key_passivetrc && defined key_trc_pisces
CCC---------------------------------------------------------------------
CCC
CCC          ROUTINE p4zsink : PISCES MODEL
CCC          ******************************
CCC
CCC  PURPOSE :
CCC  ---------
CCC         Compute vertical flux of particulate matter due to
CCC         gravitational sinking
CCC
CC   INPUT :
CC   -----
CC      common
CC              all the common defined in opa
CC
CC
CC   OUTPUT :                   : no
CC   ------
CC
CC   EXTERNAL :
CC   --------
CC            p4zsink2
CC
CC   MODIFICATIONS:
CC   --------------
CC      original  : 2004 - O. Aumont 
CC      revisions: 2007 - C. Menkes
CC                        P. Marchesiello
CC----------------------------------------------------------------------
CC parameters and commons
CC ======================
# include "param.h"
# include "parameter.h"
# include "common.h"
CC----------------------------------------------------------------------
CC local declarations
CC ==================
      INTEGER jksed, ji, jj, jk, jnt, iter1, iter2
      REAL xagg1,xagg2,xagg3,xagg4
      REAL zfact,zstep,wsmax,rfacts1,rfacts2,cfl1,cfl2
C
C
C    Time step duration for biology
C    ------------------------------
C
       zstep=rfact2/rjjss
C
C    Sinking speeds of detritus is increased with depth as shown
C    by data and from the coagulation theory
C    -----------------------------------------------------------
C
      DO jk=1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
            zfact=max(0.,fsdepw(ji,jj,jk+1)-hmld(ji,jj))/2000.
c            zfact=min(1.,zfact)
            wsbio4(ji,jj,jk)=wsbio2+(200.-wsbio2)*zfact
            wsbio3(ji,jj,jk)=wsbio
            wscal(ji,jj,jk)=wsbio4(ji,jj,jk)
          END DO
        END DO
      END DO
C
C   INITIALIZE TO ZERO ALL THE SINKING ARRAYS
C   -----------------------------------------
C
      DO jk=1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
            sinking(ji,jj,jk)=0.
            sinking2(ji,jj,jk)=0.
            sinkcal(ji,jj,jk)=0.
            sinkfer(ji,jj,jk)=0.
            sinksil(ji,jj,jk)=0.
            sinkfer2(ji,jj,jk)=0.
          END DO
        END DO
      END DO
C
C   Compute the sedimentation term using p4zsink2 for all
C   sinking particles. Apply time-splitting for CFL conditions
C   -----------------------------------------------------
C
      wsmax=0.1/zstep
      iter1=max(1,int(wsbio/wsmax))
      iter2=max(1,int(wsbio2/wsmax))
      ! Check CFL condition
      DO jk=1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
            wsmax=fse3t(ji,jj,jk)/zstep
            cfl1=wsbio3(ji,jj,jk)/wsmax/iter1
            cfl2=wsbio4(ji,jj,jk)/wsmax/iter2
            if (cfl1.gt.1 .or. cfl2.gt.1) then
              print *,'WARNING PISCES/P4ZSINK: CFL CONDITION VIOLATED'
     &               ,cfl1,cfl2
            endif 
          END DO
        END DO
      END DO

      DO jnt=1,iter1
        rfacts1=rfact/float(iter1)
        CALL p4zsink2(Istr,Iend,Jstr,Jend,wsbio3,sinking,jppoc,rfacts1)
        CALL p4zsink2(Istr,Iend,Jstr,Jend,wsbio3,sinkfer,jpsfe,rfacts1)
      END DO
      DO jnt=1,iter2
        rfacts2=rfact2/float(iter2)
        CALL p4zsink2(Istr,Iend,Jstr,Jend,wsbio4,sinking2,jpgoc,rfacts2)
        CALL p4zsink2(Istr,Iend,Jstr,Jend,wsbio4,sinkfer2,jpbfe,rfacts2)
        CALL p4zsink2(Istr,Iend,Jstr,Jend,wsbio4,sinksil,jpdsi,rfacts2)
        CALL p4zsink2(Istr,Iend,Jstr,Jend,wscal,sinkcal,jpcal,rfacts2)
      END DO
C
C  Exchange between organic matter compartments due to
C  coagulation/disaggregation
C  ---------------------------------------------------
C
      DO jk=1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C
            zfact=zstep*zdiss(ji,jj,jk)
C
C    Part I : Coagulation dependent on turbulence
C    ----------------------------------------------
C
            xagg1=940.*zfact*trn(ji,jj,jk,jppoc)*trn(ji,jj,jk,jppoc)
C
            xagg2=1.054E4*zfact*trn(ji,jj,jk,jppoc)*trn(ji,jj,jk,jpgoc)
C
C    Aggregation of small into large particles
C    Part II : Differential settling
C    ----------------------------------------------
C
            xagg3=0.66*zstep*trn(ji,jj,jk,jppoc)*trn(ji,jj,jk,jpgoc)
C
            xagg(ji,jj,jk)=xagg1+xagg2+xagg3
            xaggfe(ji,jj,jk)=xagg(ji,jj,jk)*trn(ji,jj,jk,jpsfe)/
     &                                (trn(ji,jj,jk,jppoc)+rtrn)
C
C     Aggregation of DOC to small particles
C     --------------------------------------
C
            xaggdoc(ji,jj,jk)=(80*trn(ji,jj,jk,jpdoc)+698.
     &           *trn(ji,jj,jk,jppoc))*zfact*trn(ji,jj,jk,jpdoc)
C
            xaggdoc2(ji,jj,jk)=1.05E4*trn(ji,jj,jk,jpgoc)
     &                                *zfact*trn(ji,jj,jk,jpdoc)
C
          END DO
        END DO
      END DO

C#    if defined key_trc_dia3d
# if defined key_trc_diaadd
      DO jj=Jstr, Jend
        DO ji=Istr, Iend
CEM           jksed=zmeuindex(ji,jj)
           jksed=jpk-1
           bioVSink(ji,jj,5) = sinking(ji,jj,jksed+1)*1.e3
     &                            *rfact2r*tmask(ji,jj,jksed+1)
           bioVSink(ji,jj,6) = sinking2(ji,jj,jksed+1)*1.e3
     &                            *rfact2r*tmask(ji,jj,jksed+1)
           bioVSink(ji,jj,7) = sinkfer(ji,jj,jksed+1)*1.e3
     &                            *rfact2r*tmask(ji,jj,jksed+1)
           bioVSink(ji,jj,8) = sinkfer2(ji,jj,jksed+1)*1.e3
     &                            *rfact2r*tmask(ji,jj,jksed+1)
           bioVSink(ji,jj,9) = sinksil(ji,jj,jksed+1)*1.e3
     &                            *rfact2r*tmask(ji,jj,jksed+1)
           bioVSink(ji,jj,10) = sinkcal(ji,jj,jksed+1)*1.e3
     &                            *rfact2r*tmask(ji,jj,jksed+1)
        END DO
      END DO
# endif
C
#endif
      RETURN
      END

