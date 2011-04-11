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
      SUBROUTINE p4zrem(Istr,Iend,Jstr,Jend)
#include "cppdefs.h"
      Implicit none
      INTEGER Istr,Jstr,Iend,Jend

#if defined key_passivetrc && defined key_trc_pisces
CCC---------------------------------------------------------------------
CCC
CCC          ROUTINE p4zrem : PISCES MODEL
CCC          *****************************
CCC
CCC  PURPOSE :
CCC  ---------
CCC         Compute remineralization/scavenging of organic compounds
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
CC            None
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
      INTEGER ji, jj, jk, iz
      REAL remip,remik,xlam1b
      REAL xkeq,xfeequi,siremin
      REAL zsatur,zsatur2,znusil
      REAL zdepbac(PRIV_3D_BIOARRAY)
      REAL zlamfac,zstep,fesatur(PRIV_3D_BIOARRAY)
C
C      Time step duration for the biology
C
       zstep=rfact2/rjjss
C
C      Computation of the mean phytoplankton concentration as
C      a crude estimate of the bacterial biomass
C      --------------------------------------------------
C
c$$$       write(*,*) 'OKOKKOKOKOK','Zmeu',zmeu(30,30)
c$$$      DO jk=1,12
c$$$        DO jj=Jstr, Jend
c$$$          DO ji=Istr, Iend
c$$$         zdepbac(ji,jj,jk)=min(0.7*(trn(ji,jj,jk,jpzoo)
c$$$     &     +2*trn(ji,jj,jk,jpmes)),4E-6)
c$$$          END DO
c$$$        END DO
c$$$      END 
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
            DO jk=1,zmeuindex(ji, jj)
         zdepbac(ji,jj,jk)=min(0.7*(trn(ji,jj,jk,jpzoo)
     &     +2*trn(ji,jj,jk,jpmes)),4E-6)
          END DO
        END DO
      END DO
C
C      Vertical decay of the bacterial activity
C      ----------------------------------------
C
C Christophe Menkes. WE want to do that under the Euphotic Layer
c      DO jk=13,jpk
c        DO jj=Jstr, Jend
c          DO ji=Istr, Iend
c        zdepbac(ji,jj,jk)=min(1.,fsdept(ji,jj,12)/fsdept(ji,jj,jk))
c     &      *zdepbac(ji,jj,12)
c          END DO
c        END DO
c      END DO
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
             iz=zmeuindex(ji,jj)
             DO jk=iz+1,jpk
         zdepbac(ji,jj,jk)=min(1.,fsdept(ji,jj,iz)/fsdept(ji,jj,jk))
     &                                            *zdepbac(ji,jj,iz)
          END DO
        END DO
      END DO

      DO jk=1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C
C    DENITRIFICATION FACTOR COMPUTED FROM O2 LEVELS
C    ----------------------------------------------
C
          nitrfac(ji,jj,jk)=
     &      max(0.,0.4*(6.E-6-trn(ji,jj,jk,jpoxy))/(oxymin+
     &      trn(ji,jj,jk,jpoxy)))
          nitrfac(ji,jj,jk)=min(1.,nitrfac(ji,jj,jk))
          END DO
        END DO
      END DO

      DO jk=1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C
C     DOC ammonification. Depends on depth, phytoplankton biomass
C     and a limitation term which is supposed to be a parameterization
C     of the bacterial activity. 
C     ----------------------------------------------------------------
C
         remik = xremik*zstep/1E-6*xlimbac(ji,jj,jk)
     &     *zdepbac(ji,jj,jk)
         remik=max(remik,5.5E-4*zstep)
C
C     Ammonification in oxic waters with oxygen consumption
C     -----------------------------------------------------
C
         olimi(ji,jj,jk)=min((trn(ji,jj,jk,jpoxy)-rtrn)/o2ut,
     &     remik*(1.-nitrfac(ji,jj,jk))*trn(ji,jj,jk,jpdoc)) 
C
C     Ammonification in suboxic waters with denitrification
C     -------------------------------------------------------
C
         denitr(ji,jj,jk)=min((trn(ji,jj,jk,jpno3)-rtrn)/rdenit,
     &     remik*nitrfac(ji,jj,jk)*trn(ji,jj,jk,jpdoc))
          END DO
        END DO
      END DO
C
      DO jk=1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
         olimi(ji,jj,jk)=max(0.,olimi(ji,jj,jk))
         denitr(ji,jj,jk)=max(0.,denitr(ji,jj,jk))
          END DO
        END DO
      END DO
C
      DO jk=1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C
C    NH4 nitrification to NO3. Ceased for oxygen concentrations
C    below 2 umol/L. Inhibited at strong light 
C    ----------------------------------------------------------
C
         onitr(ji,jj,jk)=nitrif*zstep*trn(ji,jj,jk,jpnh4)/(1.
     &     +emoy(ji,jj,jk))*(1.-nitrfac(ji,jj,jk))
          END DO
        END DO
      END DO

      DO jk=1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C
C    Bacterial uptake of iron. No iron is available in DOC. So
C    Bacteries are obliged to take up iron from the water. Some
C    studies (especially at Papa) have shown this uptake to be
C    significant
C    ----------------------------------------------------------
C
         xbactfer(ji,jj,jk)=15E-6*rfact2*4.*0.4*prmax(ji,jj,jk)
     &     *(xlimphy(ji,jj,jk)*zdepbac(ji,jj,jk))**2
     &     /(xkgraz2+zdepbac(ji,jj,jk))
     &     *(0.5+sign(0.5,trn(ji,jj,jk,jpfer)-2E-11))
C
          END DO
        END DO
      END DO
C
      DO jk=1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C
C    POC disaggregation by turbulence and bacterial activity. 
C    -------------------------------------------------------------
C
         remip=xremip*zstep*tgfunc(ji,jj,jk)*(1.-0.5*nitrfac(ji,jj,jk))
C
C    POC disaggregation rate is reduced in anoxic zone as shown by
C    sediment traps data. In oxic area, the exponent of the martin's
C    law is around -0.87. In anoxic zone, it is around -0.35. This
C    means a disaggregation constant about 0.5 the value in oxic zones
C    -----------------------------------------------------------------
C
         orem(ji,jj,jk)=remip*trn(ji,jj,jk,jppoc)
         orem2(ji,jj,jk)=remip*trn(ji,jj,jk,jpgoc)
         ofer(ji,jj,jk)=remip*trn(ji,jj,jk,jpsfe)
         ofer2(ji,jj,jk)=remip*trn(ji,jj,jk,jpbfe)
C
          END DO
        END DO
      END DO

      DO jk=1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C
C     Remineralization rate of BSi depedant on T and saturation
C     ---------------------------------------------------------
C
         zsatur=(sio3eq(ji,jj,jk)-trn(ji,jj,jk,jpsil))/
     &     (sio3eq(ji,jj,jk)+rtrn)
         zsatur=max(rtrn,zsatur)
         zsatur2=zsatur*(1.+tn(ji,jj,jk)/400.)**4
         znusil=0.225*(1.+tn(ji,jj,jk)/15.)*zsatur+0.775*zsatur2**9

         siremin=xsirem*zstep*znusil
C
         osil(ji,jj,jk)=siremin*trn(ji,jj,jk,jpdsi)
          END DO
        END DO
      END DO
C
      DO jk=1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
         fesatur(ji,jj,jk)=0.6E-9
          END DO
        END DO
      END DO
C
      DO jk=1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C
C     scavenging rate of iron. this scavenging rate depends on the
C     load in particles on which they are adsorbed. The
C     parameterization has been taken from studies on Th
C     ------------------------------------------------------------
C
         xkeq=fekeq(ji,jj,jk)
         xfeequi=(-(1.+fesatur(ji,jj,jk)*xkeq-xkeq*trn(ji,jj,jk,jpfer))+
     &     sqrt((1.+fesatur(ji,jj,jk)*xkeq-xkeq*trn(ji,jj,jk,jpfer))**2
     &     +4.*trn(ji,jj,jk,jpfer)*xkeq))/(2.*xkeq)

         xlam1b=3E-5+xlam1*(trn(ji,jj,jk,jppoc)
     &     +trn(ji,jj,jk,jpgoc)+trn(ji,jj,jk,jpcal)+
     &      trn(ji,jj,jk,jpdsi))*1E6

         xscave(ji,jj,jk)=xfeequi*xlam1b*zstep
C
C  Increased scavenging for very high iron concentrations
C  found near the coasts due to increased lithogenic particles
C  and let's say it unknown processes (precipitation, ...)
C  -----------------------------------------------------------
C
         zlamfac=max(0.,(gphit(ji,jj)+50.)/10.)
         zlamfac=min(1.,zlamfac)
         xlam1b=(80.*(trn(ji,jj,jk,jpdoc)+35E-6)+698.
     &    *trn(ji,jj,jk,jppoc)+1.05E4*trn(ji,jj,jk,jpgoc))
     &    *zdiss(ji,jj,jk)+1E-4*(1.-zlamfac)+xlam1*max(0.,
     &    (trn(ji,jj,jk,jpfer)*1E9-1.))
C
         xaggdfe(ji,jj,jk)=xlam1b*zstep*0.5*(trn(ji,jj,jk,jpfer)
     &     -xfeequi)
C
          END DO
        END DO
      END DO
C
#endif
      RETURN
      END

