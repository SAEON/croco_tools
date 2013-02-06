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
      SUBROUTINE p4zsed(Istr,Iend,Jstr,Jend)
#include "cppdefs.h"
      Implicit NONE
      INTEGER Istr,Jstr,Iend,Jend

#if defined key_passivetrc && defined key_trc_pisces
CCC---------------------------------------------------------------------
CCC
CCC          ROUTINE p4zsed : PISCES MODEL
CCC          *****************************
CCC
CCC  PURPOSE :
CCC  ---------
CCC         Compute loss of organic matter in the sediments. This
CCC         is by no way a sediment model. The loss is simply 
CCC         computed to balance the inout from rivers and dust
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
CC             None
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
#define PRIV_3D_BIOARRAY Istr:Iend,Jstr:Jend,1:jpk
CC----------------------------------------------------------------------
CC local declarations
CC ==================
      INTEGER ji, jj, jk, ikt
      REAL zlosscal,sumsedsi,sumsedpo4,sumsedcal
      REAL xconctmp,nitrpot(Istr:Iend,Jstr:Jend,jpk)
      REAL xconctmp2,zstep,zfact
      REAL irondep(PRIV_3D_BIOARRAY),sidep(Istr:Iend,Jstr:Jend)
      REAL xlim
C
C     Time step duration for the biology
C     ----------------------------------
C
        zstep=rfact2/rjjss
C
C
C     Initialisation of variables used to compute deposition
C     ------------------------------------------------------
C
      irondep     = 0.
      sidep       = 0.
C
C     Iron and Si deposition at the surface
C     -------------------------------------
C
      DO jj=Jstr, Jend
        DO ji=Istr, Iend
         irondep(ji,jj,1)=(0.01*dust(ji,jj)/(55.85*rmoss)
     &      +3E-10/raass)*rfact2/fse3t(ji,jj,1)
         sidep(ji,jj)=8.8*0.075*dust(ji,jj)*rfact2
     &      /(fse3t(ji,jj,1)*28.1*rmoss)
        END DO
      END DO
C
C     Iron solubilization of particles in the water column
C     ----------------------------------------------------
C
      DO jk=2,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
          irondep(ji,jj,jk)=dust(ji,jj)/(10.*55.85*rmoss)*rfact2
     &      *0.0001
          END DO
        END DO
      END DO
C
C    Add the external input of nutrients, carbon and alkalinity
C    ----------------------------------------------------------
C
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
          trn(ji,jj,1,jppo4) = trn(ji,jj,1,jppo4)
     &      +rivinp(ji,jj)*rfact2
          trn(ji,jj,1,jpno3) = trn(ji,jj,1,jpno3)
     &      +(rivinp(ji,jj)+nitdep(ji,jj))*rfact2
          trn(ji,jj,1,jpfer) = trn(ji,jj,1,jpfer)
     &      +rivinp(ji,jj)*9E-5*rfact2
          trn(ji,jj,1,jpsil) = trn(ji,jj,1,jpsil)
     &      +sidep(ji,jj)+cotdep(ji,jj)*rfact2/6.
          trn(ji,jj,1,jpdic) = trn(ji,jj,1,jpdic)
     &      +rivinp(ji,jj)*rfact2*2.631
          trn(ji,jj,1,jptal) = trn(ji,jj,1,jptal)
     &      +(cotdep(ji,jj)-rno3*(rivinp(ji,jj)
     &      +nitdep(ji,jj)))*rfact2
          END DO
        END DO
C
C     Add the external input of iron which is 3D distributed
C     (dust, river and sediment mobilization)
C     ------------------------------------------------------
C
        DO jk=1,jpk
          DO jj=Jstr, Jend
            DO ji=Istr, Iend
          trn(ji,jj,jk,jpfer) = trn(ji,jj,jk,jpfer)
     &      +irondep(ji,jj,jk)+ironsed(ji,jj,jk)*rfact2
            END DO
          END DO
        END DO
C
C    Then this loss is scaled at each bottom grid cell for
C    equilibrating the total budget of silica in the ocean.
C    Thus, the amount of silica lost in the sediments equal
C    the supply at the surface (dust+rivers)
C    ------------------------------------------------------
C
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
        xconctmp=trn(ji,jj,jpk,jpdsi)*wsbio4(ji,jj,jpk)*zstep
     &    /fse3t(ji,jj,jpk)
        trn(ji,jj,jpk,jpdsi)=trn(ji,jj,jpk,jpdsi)-xconctmp
        trn(ji,jj,jpk,jpsil)=trn(ji,jj,jpk,jpsil)+xconctmp
     &    *0.98
          END DO
        END DO

        DO jj=Jstr, Jend
          DO ji=Istr, Iend
        xconctmp=trn(ji,jj,jpk,jpcal)*wscal(ji,jj,jpk)*zstep
     &    /fse3t(ji,jj,jpk)
        trn(ji,jj,jpk,jpcal)=trn(ji,jj,jpk,jpcal)-xconctmp
        trn(ji,jj,jpk,jptal)=trn(ji,jj,jpk,jptal)+xconctmp
     &    *0.85*2.
        trn(ji,jj,jpk,jpdic)=trn(ji,jj,jpk,jpdic)+xconctmp
     &    *0.85
         END DO
       END DO

        DO jj=Jstr, Jend
          DO ji=Istr, Iend
        xconctmp=trn(ji,jj,jpk,jpgoc)
        xconctmp2=trn(ji,jj,jpk,jppoc)
        trn(ji,jj,jpk,jpgoc)=trn(ji,jj,jpk,jpgoc)
     &    -xconctmp*wsbio4(ji,jj,jpk)*zstep/fse3t(ji,jj,jpk)
        trn(ji,jj,jpk,jppoc)=trn(ji,jj,jpk,jppoc)
     &    -xconctmp2*wsbio3(ji,jj,jpk)*zstep/fse3t(ji,jj,jpk)
        trn(ji,jj,jpk,jpdoc)=trn(ji,jj,jpk,jpdoc)
     &    +(xconctmp*wsbio4(ji,jj,jpk)+xconctmp2*wsbio3(ji,jj,jpk))
     &    *zstep/fse3t(ji,jj,jpk)*0.92
        trn(ji,jj,jpk,jpbfe)=trn(ji,jj,jpk,jpbfe)
     &    -trn(ji,jj,jpk,jpbfe)*wsbio4(ji,jj,jpk)*zstep
     &    /fse3t(ji,jj,jpk)
        trn(ji,jj,jpk,jpsfe)=trn(ji,jj,jpk,jpsfe)
     &    -trn(ji,jj,jpk,jpsfe)*wsbio3(ji,jj,jpk)*zstep
     &    /fse3t(ji,jj,jpk)
          END DO
        END DO
C
C  Potential nitrogen fication dependant on temperature
C  and iron
C  ----------------------------------------------------
C
      DO jk=1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
        xlim=(1.-xnanono3(ji,jj,jk)-xnanonh4(ji,jj,jk))
        if (xlim.le.0.2) xlim=0.01
        nitrpot(ji,jj,jk)=max(0.,(prmax(ji,jj,jk)-2.15/rjjss))
     &    *xlim*rfact2*trn(ji,jj,jk,jpfer)/(conc3
     &    +trn(ji,jj,jk,jpfer))*(1.-exp(-etot(ji,jj,jk)/50.))
          END DO
        END DO 
      END DO
C
C  Nitrogen change due to nitrogen fixation
C  ----------------------------------------
C
      DO jk=1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
        zfact=nitrpot(ji,jj,jk)*5.E-8
        trn(ji,jj,jk,jpnh4)=trn(ji,jj,jk,jpnh4)+zfact
        trn(ji,jj,jk,jpoxy)=trn(ji,jj,jk,jpoxy)+zfact*o2nit
          END DO
        END DO
      END DO
C
#    if defined key_trc_diaadd
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
        bioVSink(ji,jj,13) = nitrpot(ji,jj,1)*1E-7/rfact2*fse3t(ji,jj,1)
     &                       *tmask(ji,jj,1)
        bioVSink(ji,jj,12) = irondep(ji,jj,1)*1e3*rfact2r*tmask(ji,jj,1)
     &    *fse3t(ji,jj,1)
          END DO
        END DO
#    endif

! tomflag
#    if defined key_trc_dia3d
      DO jk=1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
         bioFlux(ji,jj,jk,20) = nitrpot(ji,jj,jk)*5.E-8*o2nit
     &        *1e-3*rfact2r*tmask(ji,jj,jk)
          END DO
        END DO
      END DO
#    endif
C
#endif
      RETURN
      END

