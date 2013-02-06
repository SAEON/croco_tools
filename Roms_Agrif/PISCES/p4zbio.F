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
      SUBROUTINE p4zbio(Istr,Iend,Jstr,Jend)
#include "cppdefs.h"
      Implicit NONE
      INTEGER Istr,Jstr,Iend,Jend

#if defined key_passivetrc && defined key_trc_pisces
CCC   ------------------------------------------------------------------
CCC   
CCC   ROUTINE p4zbio : PISCES MODEL
CCC   *****************************
CCC   
CC
CC     PURPOSE.
CC     --------
CC          *P4ZBIO* ECOSYSTEM MODEL IN THE WHOLE OCEAN
CC                   THIS ROUTINE COMPUTES THE DIFFERENT INTERACTIONS
CC                   BETWEEN THE DIFFERENT COMPARTMENTS OF THE MODEL
CC     EXTERNAL :
CC     ----------
CC          p4zopt, p4zprod, p4znano, p4zdiat, p4zmicro, p4zmeso
CC          p4zsink, p4zrem
CC
CC   MODIFICATIONS:
CC   --------------
CC      original      : 2004    O. Aumont
CC ----------------------------------------------------------------
CC parameters and commons
CC ======================
#include "param.h"
#include "parameter.h"
#include "common.h"
#define PRIV_3D_BIOARRAY Istr:Iend,Jstr:Jend,1:jpk
CC-----------------------------------------------------------------
CC local declarations
CC ==================
C     
      INTEGER ji, jj, jk, jn

      REAL zdenom1(PRIV_3D_BIOARRAY),zdenom2(PRIV_3D_BIOARRAY)
      REAL prodca,ztemp,zdenom
C     
      REAL prodt
      REAL zfracal(PRIV_3D_BIOARRAY)
C
C     ASSIGN THE SHEAR RATE THAT IS USED FOR AGGREGATION
C     OF PHYTOPLANKTON AND DETRITUS
C
      DO jk=1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
       if (fsdepw(ji,jj,jk+1).le.hmld(ji,jj)) then 
          zdiss(ji,jj,jk)=1.
       else
          zdiss(ji,jj,jk)=0.01
       endif
          END DO 
        END DO
      END DO
C
C      Compute de different ratios for scavenging of iron
C      --------------------------------------------------
C
       DO jk=1,jpk
         DO jj=Jstr, Jend
           DO ji=Istr, Iend
         zdenom=1./(trn(ji,jj,jk,jppoc)+trn(ji,jj,jk,jpgoc)
     $     +trn(ji,jj,jk,jpdsi)+trn(ji,jj,jk,jpcal)+rtrn)
C
         zdenom1(ji,jj,jk)=trn(ji,jj,jk,jppoc)*zdenom
         zdenom2(ji,jj,jk)=trn(ji,jj,jk,jpgoc)*zdenom
           END DO
         END DO
       END DO
C
C     Compute the fraction of nanophytoplankton that is made
C     of calcifiers
C     ------------------------------------------------------
C
       DO jk=1,jpk
         DO jj=Jstr, Jend
           DO ji=Istr, Iend
       ztemp=max(0.,tn(ji,jj,jk))
       zfracal(ji,jj,jk)=caco3r*xlimphy(ji,jj,jk)*max(0.0001
     &   ,ztemp/(2.+ztemp))*max(1.,trn(ji,jj,jk,jpphy)*1E6/2.)
       zfracal(ji,jj,jk)=min(0.8,zfracal(ji,jj,jk))
       zfracal(ji,jj,jk)=max(0.01,zfracal(ji,jj,jk))
           END DO
         END DO
       END DO

C
C  Call optical routine to compute the PAR in the water column
C  -----------------------------------------------------------
C
      CALL p4zopt(Istr,Iend,Jstr,Jend)
C
C  Call routine to compute the co-limitations by the various
C  nutrients
C  ---------------------------------------------------------
C
      CALL p4zlim(Istr,Iend,Jstr,Jend)
C
C  Call production routine to compute phytoplankton growth rate
C  over the global ocean. Growth rates for each element is 
C  computed (C, Si, Fe, Chl)
C  ------------------------------------------------------------
C
      CALL p4zprod(Istr,Iend,Jstr,Jend)
C
C  Call phytoplankton mortality routines. Mortality losses for 
C  Each elements are computed (C, Fe, Si, Chl)
C  -----------------------------------------------------------
C
      CALL p4znano(Istr,Iend,Jstr,Jend)
      CALL p4zdiat(Istr,Iend,Jstr,Jend)
C
C  Call zooplankton sources/sinks routines. 
C  Each elements are computed (C, Fe, Si, Chl)
C  -----------------------------------------------------------
C
      CALL p4zmicro(Istr,Iend,Jstr,Jend)
      CALL p4zmeso(Istr,Iend,Jstr,Jend)
C
C     Call subroutine for computation of the vertical flux 
C     of particulate organic matter
C     ----------------------------------------------------
      CALL p4zsink(Istr,Iend,Jstr,Jend)
C
C     Call subroutine for computation of remineralization
C     terms of organic matter+scavenging of Fe
C     ----------------------------------------------------
c$$$      write(*,*) 'P4ZBIO ZMEU', zmeu(30,30)
      CALL p4zrem(Istr,Iend,Jstr,Jend)
C     
C     Determination of tracers concentration as a function of 
C     biological sources and sinks
C     --------------------------------------------------------
C     
      DO jk = 1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C     
C     Evolution of PO4
C     ----------------
C     
          trn(ji,jj,jk,jppo4) = trn(ji,jj,jk,jppo4)
     &      -prorca(ji,jj,jk)-prorca2(ji,jj,jk)
     &      +olimi(ji,jj,jk)+grarem(ji,jj,jk)*sigma1+denitr(ji,jj,jk)
     &      +grarem2(ji,jj,jk)*sigma2
C
C     Evolution of NO3 and NH4
C     ------------------------
C
          trn(ji,jj,jk,jpno3) = trn(ji,jj,jk,jpno3)
     &      -pronew(ji,jj,jk)-pronew2(ji,jj,jk)+onitr(ji,jj,jk)
     &      -denitr(ji,jj,jk)*rdenit

          trn(ji,jj,jk,jpnh4) = trn(ji,jj,jk,jpnh4)
     &      -proreg(ji,jj,jk)-proreg2(ji,jj,jk)+olimi(ji,jj,jk)
     &      +grarem(ji,jj,jk)*sigma1+grarem2(ji,jj,jk)*sigma2
     &      -onitr(ji,jj,jk)+denitr(ji,jj,jk)

          END DO
        END DO
      END DO

      DO jk = 1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C    
C     Evolution of Phytoplankton
C     --------------------------
C     
          trn(ji,jj,jk,jpphy) = trn(ji,jj,jk,jpphy)
     &      +prorca(ji,jj,jk)*(1.-excret)-tortp(ji,jj,jk)
     &      -grazp(ji,jj,jk)-grazn(ji,jj,jk)-respp(ji,jj,jk)

          trn(ji,jj,jk,jpnch) = trn(ji,jj,jk,jpnch)
     &      +prorca6(ji,jj,jk)*(1.-excret)-tortnch(ji,jj,jk)
     &      -grazpch(ji,jj,jk)-graznch(ji,jj,jk)-respnch(ji,jj,jk)
C
C     Evolution of Diatoms
C     ------------------
C
          trn(ji,jj,jk,jpdia) = trn(ji,jj,jk,jpdia)
     &      +prorca2(ji,jj,jk)*(1.-excret2)-tortp2(ji,jj,jk)
     &      -respp2(ji,jj,jk)-grazd(ji,jj,jk)-grazsd(ji,jj,jk)

          trn(ji,jj,jk,jpdch) = trn(ji,jj,jk,jpdch)
     &      +prorca7(ji,jj,jk)*(1.-excret2)-tortdch(ji,jj,jk)
     &      -respdch(ji,jj,jk)-grazdch(ji,jj,jk)-grazsch(ji,jj,jk)
          END DO
        END DO
      END DO

      DO jk = 1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C    
C     Evolution of Zooplankton
C     ------------------------
C     
          trn(ji,jj,jk,jpzoo) = trn(ji,jj,jk,jpzoo)
     &      +epsher*(grazp(ji,jj,jk)+grazm(ji,jj,jk)+grazsd(ji,jj,jk))
     &      -grazz(ji,jj,jk)-tortz(ji,jj,jk)-respz(ji,jj,jk)
C
C     Evolution of Mesozooplankton
C     ------------------------
C
          trn(ji,jj,jk,jpmes) = trn(ji,jj,jk,jpmes)
     &      +epsher2*(grazd(ji,jj,jk)+grazz(ji,jj,jk)+grazn(ji,jj,jk)
     &      +grazpoc(ji,jj,jk)+grazffe(ji,jj,jk))-tortz2(ji,jj,jk)
     &      -respz2(ji,jj,jk)
          END DO
        END DO
      END DO

      DO jk = 1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C    
C     Evolution of DOC
C     ----------------
C     
          trn(ji,jj,jk,jpdoc) = trn(ji,jj,jk,jpdoc)
     &      +orem(ji,jj,jk)+excret2*prorca2(ji,jj,jk)
     &      +excret*prorca(ji,jj,jk)-olimi(ji,jj,jk)-denitr(ji,jj,jk)
     &      +grarem(ji,jj,jk)*(1.-sigma1)+grarem2(ji,jj,jk)
     &      *(1.-sigma2)-xaggdoc(ji,jj,jk)-xaggdoc2(ji,jj,jk)
          END DO
        END DO
      END DO

      DO jk = 1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C     
C     Evolution of Detritus
C     ---------------------
C     
          trn(ji,jj,jk,jppoc) = trn(ji,jj,jk,jppoc)
     &     -grazpoc(ji,jj,jk)+grapoc(ji,jj,jk)-grazm(ji,jj,jk)
     &     +respz(ji,jj,jk)-xagg(ji,jj,jk)+xaggdoc(ji,jj,jk)
     &     +(1.-0.5*zfracal(ji,jj,jk))*(tortp(ji,jj,jk)
     &     +respp(ji,jj,jk))+0.5*tortp2(ji,jj,jk)
     &     +orem2(ji,jj,jk)+tortz(ji,jj,jk)-orem(ji,jj,jk)
C    
C     Evolution of rapid Detritus
C     ---------------------
C    
          trn(ji,jj,jk,jpgoc) = trn(ji,jj,jk,jpgoc)
     &     +grapoc2(ji,jj,jk)+respp2(ji,jj,jk)+xagg(ji,jj,jk)
     &     +tortz2(ji,jj,jk)+respz2(ji,jj,jk)-orem2(ji,jj,jk)
     &     +0.5*zfracal(ji,jj,jk)*(respp(ji,jj,jk)+tortp(ji,jj,jk))
     &     +0.5*tortp2(ji,jj,jk)+xaggdoc2(ji,jj,jk)-grazffe(ji,jj,jk)
C
          END DO
        END DO
      END DO

      DO jk = 1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C    
C     Evolution of O2
C     ---------------
C     
         trn(ji,jj,jk,jpoxy) = trn(ji,jj,jk,jpoxy)
     &     +o2ut*(proreg(ji,jj,jk)+proreg2(ji,jj,jk)-olimi(ji,jj,jk)
     &     -grarem(ji,jj,jk)*sigma1-grarem2(ji,jj,jk)*sigma2)
     &     +(o2ut+o2nit)*( pronew(ji,jj,jk)+pronew2(ji,jj,jk))
     &     -o2nit*onitr(ji,jj,jk)
C
          END DO
        END DO
      END DO

      DO jk = 1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C
C     Evolution of IRON
C     -----------------
C
          trn(ji,jj,jk,jpfer) = trn(ji,jj,jk,jpfer)
     &      +(excret-1.)*prorca5(ji,jj,jk)-xaggdfe(ji,jj,jk)
     &      +(excret2-1.)*prorca4(ji,jj,jk)-xbactfer(ji,jj,jk)
     &      +grafer(ji,jj,jk)+grafer2(ji,jj,jk)
     &      +ofer(ji,jj,jk)-xscave(ji,jj,jk)
C
          END DO
        END DO
      END DO

      DO jk = 1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C
C     Evolution of small biogenic Iron
C     --------------------------
C
          trn(ji,jj,jk,jpsfe) = trn(ji,jj,jk,jpsfe)
     &     +unass*(grazpf(ji,jj,jk)+grazsf(ji,jj,jk))
     &     -grazpof(ji,jj,jk)-(1.-unass)*grazmf(ji,jj,jk)
     &     +(1.-0.5*zfracal(ji,jj,jk))*(tortnf(ji,jj,jk)
     &     +respnf(ji,jj,jk))+0.5*tortdf(ji,jj,jk)+ferat3*
     &     (tortz(ji,jj,jk)+respz(ji,jj,jk))-ofer(ji,jj,jk)
     &     +ofer2(ji,jj,jk)-xaggfe(ji,jj,jk)
     &     +xscave(ji,jj,jk)*zdenom1(ji,jj,jk)
C
C     Evolution of big biogenic Iron
C     --------------------------
C
          trn(ji,jj,jk,jpbfe) = trn(ji,jj,jk,jpbfe)
     &     +unass2*(graznf(ji,jj,jk)+grazf(ji,jj,jk)+grazfff(ji,jj,jk)
     &     +grazpof(ji,jj,jk)+ferat3*grazz(ji,jj,jk))+ferat3*
     &     (tortz2(ji,jj,jk)+respz2(ji,jj,jk))-ofer2(ji,jj,jk)
     &     +0.5*zfracal(ji,jj,jk)*(respnf(ji,jj,jk)+tortnf(ji,jj,jk))
     &     +0.5*tortdf(ji,jj,jk)+respdf(ji,jj,jk)+xaggfe(ji,jj,jk)
     &     +xbactfer(ji,jj,jk)-grazfff(ji,jj,jk)+xscave(ji,jj,jk)
     &     *zdenom2(ji,jj,jk)
          END DO
        END DO
      END DO

      DO jk = 1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C
C     Evolution of biogenic Silica
C     ----------------------------
C
          trn(ji,jj,jk,jpbsi) = trn(ji,jj,jk,jpbsi)
     &      +prorca3(ji,jj,jk)*(1.-excret2)-grazss(ji,jj,jk)
     &      -tortds(ji,jj,jk)-respds(ji,jj,jk)-grazs(ji,jj,jk)
C
          END DO
        END DO
      END DO

      DO jk = 1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C
C     Evolution of sinking biogenic silica
C     ------------------------------------
C
          trn(ji,jj,jk,jpdsi)=trn(ji,jj,jk,jpdsi)
     &      +tortds(ji,jj,jk)+respds(ji,jj,jk)+grazs(ji,jj,jk)
     &      -osil(ji,jj,jk)+grazss(ji,jj,jk)
C
          END DO
        END DO
      END DO

      DO jk = 1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C
C     Evolution of biogenic diatom Iron
C     ---------------------------------
C
          trn(ji,jj,jk,jpdfe) = trn(ji,jj,jk,jpdfe)
     &      +prorca4(ji,jj,jk)*(1.-excret2)-grazsf(ji,jj,jk)
     &      -tortdf(ji,jj,jk)-respdf(ji,jj,jk)-grazf(ji,jj,jk)
C
C     Evolution of biogenic nanophytoplankton Iron
C     --------------------------------------------
C
          trn(ji,jj,jk,jpnfe) = trn(ji,jj,jk,jpnfe)
     &      +prorca5(ji,jj,jk)*(1.-excret)-graznf(ji,jj,jk)
     &      -tortnf(ji,jj,jk)-respnf(ji,jj,jk)-grazpf(ji,jj,jk)
C
C     Evolution of dissolved Silica
C     -----------------------------
C
          trn(ji,jj,jk,jpsil) = trn(ji,jj,jk,jpsil)
     &      -(1.-excret2)*prorca3(ji,jj,jk)+osil(ji,jj,jk)
C
          END DO
        END DO
      END DO
C     
C     Evolution of calcite and silicates as a function of the two tracers
C     -------------------------------------------------------------------
C     
      DO  jk = 1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C
          prodt = prorca(ji,jj,jk)+prorca2(ji,jj,jk)
     &      -olimi(ji,jj,jk)-grarem(ji,jj,jk)*sigma1
     &      -grarem2(ji,jj,jk)*sigma2-denitr(ji,jj,jk)

          prodca = pronew(ji,jj,jk)+pronew2(ji,jj,jk)
     &      -onitr(ji,jj,jk)+rdenit*denitr(ji,jj,jk)
C     
C     potential production of calcite and biogenic silicate
C     ------------------------------------------------------
C     
          prcaca(ji,jj,jk)=
     &      zfracal(ji,jj,jk)*0.5*(0.5*(unass*grazp(ji,jj,jk)+
     &      unass2*grazn(ji,jj,jk))+tortp(ji,jj,jk)+respp(ji,jj,jk))
C     
C     Consumption of Total (12C)O2
C     ----------------------------
C     
          trn(ji,jj,jk,jpdic) = trn(ji,jj,jk,jpdic)
     &      -prodt-prcaca(ji,jj,jk)
C     
C     Consumption of alkalinity due to ca++ uptake and increase 
C     of alkalinity due to nitrate consumption during organic 
C     soft tissue production
C     ---------------------------------------------------------
C     
          trn(ji,jj,jk,jptal) = trn(ji,jj,jk,jptal)
     &      +rno3*prodca-2.*prcaca(ji,jj,jk)
          END DO
        END DO
      END DO
C
      DO  jk = 1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
C
C     Production of calcite due to biological production
C     --------------------------------------------------
C     
           trn(ji,jj,jk,jpcal) = trn(ji,jj,jk,jpcal)
     &        +prcaca(ji,jj,jk)
          END DO
        END DO
      ENDDO
C
C     Loop to test if tracers concentrations fall below 0.
C     ----------------------------------------------------
C
C
      DO  jk = 1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
      znegtr(ji,jj,jk) = 1.
          END DO
        END DO
      ENDDO
C
      DO jn = 1,jptra
        DO jk = 1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
              if (trn(ji,jj,jk,jn).lt.0.) then
               znegtr(ji,jj,jk)=0.
              endif
            END DO
          END DO
        END DO
      END DO
C
      DO jn = 1,jptra
        DO jk = 1,jpk
          DO jj=Jstr, Jend
            DO ji=Istr, Iend
         trn(ji,jj,jk,jn) = trb(ji,jj,jk,jn)+
     &     znegtr(ji,jj,jk)*(trn(ji,jj,jk,jn)-trb(ji,jj,jk,jn))
            END DO
          END DO
        END DO
      END DO
C
#    if defined key_trc_dia3d
        DO jk = 1,jpk
          DO jj=Jstr, Jend
            DO ji=Istr, Iend
          bioFlux(ji,jj,jk,4)=etot(ji,jj,jk)* tmask(ji,jj,jk)
          bioFlux(ji,jj,jk,5)=prorca(ji,jj,jk)
     &      *znegtr(ji,jj,jk)*1e3*rfact2r* tmask(ji,jj,jk)
          bioFlux(ji,jj,jk,6)=prorca2(ji,jj,jk)
     &      *znegtr(ji,jj,jk)*1e3*rfact2r* tmask(ji,jj,jk)
          bioFlux(ji,jj,jk,7)=pronew(ji,jj,jk)
     &      *znegtr(ji,jj,jk)*1e3*rfact2r* tmask(ji,jj,jk)
          bioFlux(ji,jj,jk,8)=pronew2(ji,jj,jk)
     &      *znegtr(ji,jj,jk)*1e3*rfact2r* tmask(ji,jj,jk)
          bioFlux(ji,jj,jk,9)=prorca3(ji,jj,jk)
     &      *znegtr(ji,jj,jk)*1e3*rfact2r* tmask(ji,jj,jk)
          bioFlux(ji,jj,jk,10)=prorca4(ji,jj,jk)
     &      *znegtr(ji,jj,jk)*1e3*rfact2r* tmask(ji,jj,jk)
          bioFlux(ji,jj,jk,11)=prorca5(ji,jj,jk)
     &      *znegtr(ji,jj,jk)*1e3*rfact2r* tmask(ji,jj,jk)
          bioFlux(ji,jj,jk,14)=((-o2nit)*onitr(ji,jj,jk))
     &      *tmask(ji,jj,jk)*1e3*rfact2r
          bioFlux(ji,jj,jk,15)=(o2ut+o2nit)*( pronew(ji,jj,jk)
     &      +pronew2(ji,jj,jk))*1e3*tmask(ji,jj,jk)*rfact2r
          bioFlux(ji,jj,jk,16)=o2ut*(proreg(ji,jj,jk)
     &      +proreg2(ji,jj,jk))*1e3*tmask(ji,jj,jk)*rfact2r
          bioFlux(ji,jj,jk,17)=(-o2ut)*olimi(ji,jj,jk)
     &      *1e3*tmask(ji,jj,jk)*rfact2r
          bioFlux(ji,jj,jk,18)=(-o2ut)*grarem(ji,jj,jk)*sigma1
     &      *1e3*tmask(ji,jj,jk)*rfact2r
          bioFlux(ji,jj,jk,19)=(-o2ut)*grarem2(ji,jj,jk)*sigma2
     &      *1e3*tmask(ji,jj,jk)*rfact2r
            END DO
          END DO
        END DO
#    endif
C     
#endif
C     
      RETURN
      END



