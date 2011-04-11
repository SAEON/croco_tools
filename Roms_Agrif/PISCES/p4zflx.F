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
      SUBROUTINE p4zflx(kt,Istr,Iend,Jstr,Jend)
#include "cppdefs.h"
      Implicit NONE
      INTEGER kt, Istr, Jstr, Iend, Jend
#if defined key_passivetrc && defined key_trc_pisces
CCC---------------------------------------------------------------------
CCC
CCC          ROUTINE p4zflx : PISCES MODEL
CCC          *****************************
CCC
CCC
CC     PURPOSE.
CC     --------
CC          *P4ZFLX* CALCULATES GAS EXCHANGE AND CHEMISTRY AT SEA SURFACE
CC
CC     EXTERNALS.
CC     ----------
CC          NONE.
CC
CC   MODIFICATIONS:
CC   --------------
CC      original      : 1988-07 E. MAIER-REIMER      MPI HAMBURG
CC      additions     : 1998    O. Aumont
CC      modifications : 1999    C. Le Quere
CC      modifications : 2004    O. Aumont
CC     -----------------------------------------------------------------
CC  parameters and commons
CC ======================
#include "param.h"
#include "parameter.h"
#include "common.h"
CC----------------------------------------------------------------------
CC local declarations
CC ==================
C
      INTEGER nspyr, ji, jj, jk, krorr
      REAL zexp1, zexp2, zpdtan
      REAL a1, a2, a3, b2, b3, ttc, ws
      REAL kgco2, kgo2
      REAL fld, flu, oxy16, flu16, zfact
      REAL zph,ah2,zbot,zdic,zalk,schmitto2, zalka
      REAL schmittco2
C
C  1. ASSIGNATION TO EXPONENTS IN THE LISS AND MERLIVAT
C     FORMULATION OF THE GAS EXCHANGE RATE
c -----------------------------------------------------
C
      zexp1 = -2./3.
      zexp2 = -1./2.  
      a1    = 0.17
      a2    = 2.85
      a3    = 5.90
      b2    = 9.65
      b3    = 49.3      
      zpdtan = raass / rdt
      nspyr  = nint(zpdtan)
C
C* 1.1 SURFACE CHEMISTRY (PCO2 AND [H+] IN
C     SURFACE LAYER); THE RESULT OF THIS CALCULATION
C     IS USED TO COMPUTE AIR-SEA FLUX OF CO2
C ---------------------------------------------------
C
      DO krorr = 1,10
        DO jj = Jstr, Jend
          DO ji = Istr, Iend
C
C* 1.2 DUMMY VARIABLES FOR DIC, H+, AND BORATE
C --------------------------------------------
C
            zbot  = borat(ji,jj,1)
            zfact = rhopn(ji,jj,1)/1000.+rtrn
            zdic  = trn(ji,jj,1,jpdic)/zfact
            zph   = max(hi(ji,jj,1),1.E-10)/zfact
            zalka = trn(ji,jj,1,jptal)/zfact
C
C* 1.3 CALCULATE [ALK]([CO3--], [HCO3-])
C ------------------------------------
C
            zalk  = zalka-
     &        (akw3(ji,jj,1)/zph-zph+zbot/(1.+zph/akb3(ji,jj,1)))
C
C* 1.4 CALCULATE [H+] AND [H2CO3]
C -----------------------------------------
C
            ah2 = sqrt((zdic-zalk)**2+4*(zalk*ak23(ji,jj,1)
     &                 /ak13(ji,jj,1))*(2*zdic-zalk))
            ah2 = 0.5*ak13(ji,jj,1)/zalk*((zdic-zalk)+ah2)
            h2co3(ji,jj) = (2*zdic-zalk)/(2.+ak13(ji,jj,1)/ah2)*zfact
            hi(ji,jj,1)  = ah2*zfact
          END DO
        END DO
      END DO
C
C
C 2. COMPUTE FLUXES
C --------------
C
C FIRST COMPUTE GAS EXCHANGE COEFFICIENTS
C -------------------------------------------
C
      DO jj = Jstr, Jend
        DO ji = Istr, Iend
C
          ttc = min(35.,tn(ji,jj,1))
          schmittco2=2073.1-125.62*ttc+3.6276*ttc**2
     &      -0.043126*ttc**3
          ws = vatm(ji,jj)
C
C COMPUTE GAS EXCHANGE FOR CO2
C --------------------------------
C
          kgco2 = (0.3*ws*ws + 2.5*(0.5246+ttc*(0.016256+
     &            ttc*0.00049946)))*sqrt(660./schmittco2)
C
C CONVERT TO mol/m2/s/uatm, and apply sea-ice cover
C -----------------------------------------------------
C
          kgco2 = kgco2/(100.*3600.)
     &      *(1-freeze(ji,jj))*tmask(ji,jj,1)
C
C COMPUTE GAS EXCHANGE COEFFICIENT FO O2 FROM LISS AND 
C MERLIVAT EQUATIONS
C ---------------------------------------------------------
C
          ws = vatm(ji,jj)
          schmitto2 = 1953.4-128.0*ttc+3.9918*ttc**2
     &      -0.050091*ttc**3
C
          kgo2 = (0.3*ws*ws + 2.5*(0.5246+ttc*(0.016256+
     &      ttc*0.00049946)))*sqrt(660./schmitto2)

C
C CONVERT TO CM AND APPLY SEA ICE COVER
C -------------------------------------
C
          kgo2 = kgo2/(100.*3600.)
     $      *(1-freeze(ji,jj))*tmask(ji,jj,1)
C
C Compute CO2 flux for the sea and air
C ------------------------------------
C
          fld = atcco2*tmask(ji,jj,1)*chemc(ji,jj,3)*kgco2
          flu = h2co3(ji,jj)*tmask(ji,jj,1)*kgco2

          trn(ji,jj,1,jpdic)= trn(ji,jj,1,jpdic)+(fld-flu)
     &      /fse3t(ji,jj,1)*rfact
C
C Compute O2 flux 
C ---------------
C
          oxy16 = trn(ji,jj,1,jpoxy)
          flu16 = (atcox*chemc(ji,jj,2)-oxy16)*kgo2
          trn(ji,jj,1,jpoxy) = trn(ji,jj,1,jpoxy)+flu16
     &      /fse3t(ji,jj,1)*rfact
C
C Save diagnostics
C ----------------
C
#    if defined key_trc_diaadd
          bioVSink(ji,jj,1) = (fld-flu)*1000.*tmask(ji,jj,1)
          bioVSink(ji,jj,2) = flu16*1000.*tmask(ji,jj,1)
          bioVSink(ji,jj,3) = kgco2*tmask(ji,jj,1)
          bioVSink(ji,jj,4) = (atcco2-h2co3(ji,jj)/
     &                        (chemc(ji,jj,1)+rtrn))
     &                         *tmask(ji,jj,1)
#    endif
C
        END DO
      END DO
C
#endif
      RETURN
      END

