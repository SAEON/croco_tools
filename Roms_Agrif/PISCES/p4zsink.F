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
CC----------------------------------------------------------------------
CC parameters and commons
CC ======================
#include "param.h"
#include "parameter.h"
#include "common.h"
CC----------------------------------------------------------------------
CC local declarations
CC ==================
      INTEGER iterm1,iterm2,jksed, ji, jj, jk, jnt
      REAL xagg1,xagg2,xagg3,xagg4
      REAL zfact,zstep,rfacts1,rfacts2
      REAL zmin(jpk),zminw(jpk)
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
C Christophe Menkes, we replace the arbitrary jksed=10 by zmeuindex
       jksed=10
C
      DO jk=1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
       zfact=max(0.,fsdepw(ji,jj,jk+1)-hmld(ji,jj))/2000.
       zfact=min(1.,zfact)
       wsbio4(ji,jj,jk)=wsbio2+(200.-wsbio2)*zfact
          END DO
        END DO
      END DO
C
      DO jk=1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
       wsbio3(ji,jj,jk)=wsbio
       wscal(ji,jj,jk)=wsbio4(ji,jj,jk)
          END DO
        END DO
      END DO
C
C   SEARCH FOR THE OPTIMAL TIME SPLITTING FOR SINKING
C   -------------------------------------------------
C
        zmin(:)=1E6
        zminw(:)=1E6
      DO jk=1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
        if (tmask(ji,jj,jk).gt.0.) then
         zmin(jk)=min(zmin(jk),fse3t(ji,jj,jk))
         zminw(jk)=min(zminw(jk),fse3w(ji,jj,jk))
        endif
          END DO
        END DO
      END DO

      iterm1=1
      iterm2=1
      DO jk=1,jpk
         iterm1=max(iterm1,int(rfact2/(0.5*min(zmin(jk),
     &   zminw(jk))*24*3600/wsbio)))
         iterm2=max(iterm2,int(rfact2/(0.5*min(zmin(jk),
     &   zminw(jk))*24*3600/wsbio2)))
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
C   the sinking particles
C   -----------------------------------------------------
C
      DO jnt=1,iterm1
        rfacts1=rfact/float(iterm1)
        CALL p4zsink2(Istr,Iend,Jstr,Jend,wsbio3,sinking,jppoc,rfacts1)
        CALL p4zsink2(Istr,Iend,Jstr,Jend,wsbio3,sinkfer,jpsfe,rfacts1)
      END DO

      DO jnt=1,iterm2
        rfacts2=rfact/float(iterm2)
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
     &     (trn(ji,jj,jk,jppoc)+rtrn)
C
C     Aggregation of DOC to small particles
C     --------------------------------------
C
         xaggdoc(ji,jj,jk)=(80*trn(ji,jj,jk,jpdoc)+698.
     &     *trn(ji,jj,jk,jppoc))*zfact*trn(ji,jj,jk,jpdoc)
                                                                                
         xaggdoc2(ji,jj,jk)=1.05E4*trn(ji,jj,jk,jpgoc)
     &     *zfact*trn(ji,jj,jk,jpdoc)
C
          END DO
        END DO
      END DO

C#    if defined key_trc_dia3d
#    if defined key_trc_diaadd
      DO jj=Jstr, Jend
        DO ji=Istr, Iend
           bioVSink(ji,jj,5) = sinking(ji,jj,zmeuindex(ji,jj)+1)*1.e3
     &                      *rfact2r*tmask(ji,jj,zmeuindex(ji,jj)+1)
           bioVSink(ji,jj,6) = sinking2(ji,jj,zmeuindex(ji,jj)+1)*1.e3
     &                      *rfact2r*tmask(ji,jj,zmeuindex(ji,jj)+1)
           bioVSink(ji,jj,7) = sinkfer(ji,jj,zmeuindex(ji,jj)+1)*1.e3
     &                      *rfact2r*tmask(ji,jj,zmeuindex(ji,jj)+1)
           bioVSink(ji,jj,8) = sinkfer2(ji,jj,zmeuindex(ji,jj)+1)*1.e3
     &                      *rfact2r*tmask(ji,jj,zmeuindex(ji,jj)+1)
           bioVSink(ji,jj,9) = sinksil(ji,jj,zmeuindex(ji,jj)+1)*1.e3
     &                      *rfact2r*tmask(ji,jj,zmeuindex(ji,jj)+1)
           bioVSink(ji,jj,10) = sinkcal(ji,jj,zmeuindex(ji,jj)+1)*1.e3
     &                      *rfact2r*tmask(ji,jj,zmeuindex(ji,jj)+1)
c$$$           bioVSink(ji,jj,5) = sinking(ji,jj,jksed+1)*1.e3
c$$$     &                      *rfact2r*tmask(ji,jj,jksed+1)
c$$$           bioVSink(ji,jj,6) = sinking2(ji,jj,jksed+1)*1.e3
c$$$     &                      *rfact2r*tmask(ji,jj,jksed+1)
c$$$           bioVSink(ji,jj,7) = sinkfer(ji,jj,jksed+1)*1.e3
c$$$     &                      *rfact2r*tmask(ji,jj,jksed+1)
c$$$           bioVSink(ji,jj,8) = sinkfer2(ji,jj,jksed+1)*1.e3
c$$$     &                      *rfact2r*tmask(ji,jj,jksed+1)
c$$$           bioVSink(ji,jj,9) = sinksil(ji,jj,jksed+1)*1.e3
c$$$     &                      *rfact2r*tmask(ji,jj,jksed+1)
c$$$           bioVSink(ji,jj,10) = sinkcal(ji,jj,jksed+1)*1.e3
c$$$     &                      *rfact2r*tmask(ji,jj,jksed+1)
        END DO
      END DO
#    endif
C
#endif
      RETURN
      END

