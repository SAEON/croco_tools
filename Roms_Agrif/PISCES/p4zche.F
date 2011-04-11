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
      SUBROUTINE p4zche(Istr,Iend,Jstr,Jend)
#include "cppdefs.h"
      Implicit NONE
      INTEGER Istr,Jstr,Iend,Jend
#if defined key_passivetrc && defined key_trc_pisces
CCC---------------------------------------------------------------------
CCC
CCC          ROUTINE p4zche : PISCES MODEL
CCC          *****************************
CCC
CCC     PURPOSE.
CCC     --------
CCC          *P4ZCHE* SETS CHEMICAL CONSTANTS 
CCC
CCC
CC     EXTERNALS.
CC     ----------
CC          rhopn
CC
CC   MODIFICATIONS:
CC   --------------
CC      original :      1988 E. Maier-Reimer
CC      additions :     1998 O. Aumont
CC      modifications : 1999 C. Le Quere
CC      modifications : 2004 O. Aumont
CC----------------------------------------------------------------------
CC parameters and commons
CC ======================
#include "param.h"
#include "parameter.h"
#include "common.h"
CC----------------------------------------------------------------------
CC local declarations
CC ==================
C
      INTEGER ji, jj, jk
      REAL tkel, sal,  qtt, zbuf1, zbuf2
      REAL pres, tc, cl, cpexp, cek0, oxy, cpexp2
      REAL zsqrt, ztr, zlogt, cek1
      REAL zqtt, qtt2, sal15, zis, zis2
      REAL ckb, ck1, ck2, ckw, ak1, ak2, akb, aksp0, akw
      REAL ckp1, ckp2, ckp3, cksi, akp1, akp2, akp3, aksi
C
C* 1. CHEMICAL CONSTANTS - SURFACE LAYER
C ---------------------------------------
C
      DO jj = Jstr,Jend
        DO ji = Istr,Iend
C
C* 1.1 SET ABSOLUTE TEMPERATURE
C ------------------------------
C
          tkel = tn(ji,jj,1)+273.16
          qtt = tkel*0.01
          qtt2=qtt*qtt
          sal = sn(ji,jj,1) + (1.-tmask(ji,jj,1))*35.
          zqtt=log(qtt)
C
C* 1.2 LN(K0) OF SOLUBILITY OF CO2 (EQ. 12, WEISS, 1980)
C      AND FOR THE ATMOSPHERE FOR NON IDEAL GAS
C -------------------------------------------------------
C
          cek0 = c00+c01/qtt+c02*zqtt+sal*(c03+c04*qtt+c05*qtt2)
          cek1 = ca0+ca1/qtt+ca2*zqtt+ca3*qtt2+sal*(ca4
     &      +ca5*qtt+ca6*qtt2)
C
C* 1.3 LN(K0) OF SOLUBILITY OF O2 and N2 (EQ. 4, WEISS, 1970)
C ------------------------------------------------------------
C
          oxy = ox0+ox1/qtt+ox2*zqtt+sal*(ox3+ox4*qtt+ox5*qtt2)
C
C* 1.4 SET SOLUBILITIES OF O2 AND CO2
C -----------------------------------
C
          chemc(ji,jj,1) = exp(cek0)*1.E-6*rhopn(ji,jj,1)/1000.
          chemc(ji,jj,2) = exp(oxy)*oxyco
          chemc(ji,jj,3) = exp(cek1)*1.E-6*rhopn(ji,jj,1)/1000.
C
        ENDDO
      END DO
C
C* 2 CHEMICAL CONSTANTS - DEEP OCEAN
C -------------------------------------
C
      DO jk = 1,jpk
        DO jj = Jstr,Jend
          DO ji = Istr,Iend
C
C* 2.1 SET PRESSION
C -----------------
C
            pres = 1.025e-1*fsdept(ji,jj,jk)
C
C* 2.2 SET ABSOLUTE TEMPERATURE
C ------------------------------
C
            tkel   = tn(ji,jj,jk)+273.16
            qtt    = tkel*0.01
            sal    = sn(ji,jj,jk) + (1.-tmask(ji,jj,jk))*35.
            zsqrt  = sqrt(sal)
            sal15  = zsqrt*sal
            zlogt  = log(tkel)
            ztr    = 1./tkel
            zis    = 19.924*sal/(1000.-1.005*sal)
            zis2   = zis*zis
            tc = tn(ji,jj,jk) + (1.-tmask(ji,jj,jk))*20.
C
C* 2.3 CHLORINITY (WOOSTER ET AL., 1969)
C ---------------------------------------
C
            cl = sal*salchl
C
C* 2.4 DISSOCIATION CONSTANT FOR CARBONATE AND BORATE
C -------------------------------------------------------
C
            ckb = (cb0+cb1*zsqrt+cb2*sal+cb3*sal15+cb4*sal*sal)*ztr
     &          +(cb5+cb6*zsqrt+cb7*sal)+
     &          (cb8+cb9*zsqrt+cb10*sal)*zlogt+cb11*zsqrt*tkel
            ck1 = c10*ztr+c11+c12*zlogt+c13*sal+c14*sal**2
            ck2 = c20*ztr+c21+c22*sal+c23*sal**2
C
C* 2.5 PKW (H2O) (DICKSON AND RILEY, 1979)
C -----------------------------------------
C
            ckw = cw0*ztr+cw1+cw2*zlogt+(cw3*ztr+cw4+cw5*zlogt)*
     &          zsqrt+cw6*sal
C
C  2.6 DISSOCIATION CONSTANT FOR PHOSPHATE AND SILICATE
C  ----------------------------------------------------
C
            ckp1 = cp10+cp11*ztr+cp12*zlogt+zsqrt*(cp13*ztr
     &           +cp14)+sal*(cp15*ztr+cp16)
            ckp2 = cp20+cp21*ztr+cp22*zlogt+zsqrt*(cp23*ztr
     &           +cp24)+sal*(cp25*ztr+cp26) 
            ckp3 = cp30+cp31*ztr+zsqrt*(cp32*ztr 
     &           +cp33)+sal*(cp34*ztr+cp35)
            cksi = cs10+cs11*ztr+cs12*zlogt+zsqrt*(cs13*ztr 
     &           +cs14)+zis*(cs15*ztr+cs16)+zis2*(cs17*ztr
     &           +cs18)+log(1.+cs18*sal)
C
C* 2.6 K1, K2 OF CARBONIC ACID, KB OF BORIC ACID, KW (H2O) (LIT.?)
C -----------------------------------------------------------------
C
            ak1 = 10**(ck1)
            ak2 = 10**(ck2)
            akb = exp(ckb)
            akp1 = exp(ckp1)
            akp2 = exp(ckp2)
            akp3 = exp(ckp3)
            aksi = exp(cksi)
            akw = exp(ckw)
C
C*2.7 APPARENT SOLUBILITY PRODUCT K'SP OF CALCITE IN SEAWATER
C       (S=27-43, T=2-25 DEG C) AT pres =0 (ATMOSPH. PRESSURE)
C       (INGLE, 1800, EQ. 6)
C -------------------------------------------------------------
C
            aksp0 = 1.E-7*(akcc1+akcc2*sal**(1./3.)+akcc3*log10(sal)
     &          +akcc4*tkel*tkel)
C
C* 2.8 FORMULA FOR CPEXP AFTER EDMOND AND GIESKES (1970)
C        (REFERENCE TO CULBERSON AND PYTKOQICZ (1968) AS MADE
C        IN BROECKER ET AL. (1982) IS INCORRECT; HERE RGAS IS
C        TAKEN TENFOLD TO CORRECT FOR THE NOTATION OF pres  IN
C        DBAR INSTEAD OF BAR AND THE EXPRESSION FOR CPEXP IS
C        MULTIPLIED BY LN(10.) TO ALLOW USE OF EXP-FUNCTION
C        WITH BASIS E IN THE FORMULA FOR AKSPP (CF. EDMOND
C        AND GIESKES (1970), P. 1285 AND P. 1286 (THE SMALL
C        FORMULA ON P. 1286 IS RIGHT AND CONSISTENT WITH THE
C        SIGN IN PARTIAL MOLAR VOLUME CHANGE AS SHOWN ON
C        P. 1285))
C -----------------------------------------------------------
C
            cpexp = pres /(rgas*tkel)
            cpexp2 = pres * pres/(rgas*tkel)
C
C* 2.9 KB OF BORIC ACID, K1,K2 OF CARBONIC ACID PRESSURE
C        CORRECTION AFTER CULBERSON AND PYTKOWICZ (1968)
C        (CF. BROECKER ET AL., 1982)
C --------------------------------------------------------
C
            zbuf1 = -(devk1(3)+devk2(3)*tc+devk3(3)*tc*tc)
            zbuf2 = 0.5*(devk4(3)+devk5(3)*tc)
            akb3(ji,jj,jk) = akb*exp(zbuf1*cpexp+zbuf2*cpexp2)

            zbuf1 = -(devk1(1)+devk2(1)*tc+devk3(1)*tc*tc)
            zbuf2 = 0.5*(devk4(1)+devk5(1)*tc)
            ak13(ji,jj,jk) = ak1*exp(zbuf1*cpexp+zbuf2*cpexp2)

            zbuf1 = -(devk1(2)+devk2(2)*tc+devk3(2)*tc*tc)
            zbuf2 = 0.5*(devk4(2)+devk5(2)*tc)
            ak23(ji,jj,jk) = ak2*exp(zbuf1*cpexp+zbuf2*cpexp2)

            zbuf1 = -(devk1(4)+devk2(4)*tc+devk3(4)*tc*tc)
            zbuf2 = 0.5*(devk4(4)+devk5(4)*tc)
            akp13(ji,jj,jk) = akp1*exp(zbuf1*cpexp+zbuf2*cpexp2)

            zbuf1 = -(devk1(5)+devk2(5)*tc+devk3(5)*tc*tc)
            zbuf2 = 0.5*(devk4(5)+devk5(5)*tc)
            akp23(ji,jj,jk) = akp2*exp(zbuf1*cpexp+zbuf2*cpexp2)

            zbuf1 = -(devk1(6)+devk2(6)*tc+devk3(6)*tc*tc)
            zbuf2 = 0.5*(devk4(6)+devk5(6)*tc)
            akp33(ji,jj,jk) = akp3

            zbuf1 = -(devk1(7)+devk2(7)*tc+devk3(7)*tc*tc)
            zbuf2 = 0.5*(devk4(7)+devk5(7)*tc)
            akw3(ji,jj,jk) = akw*exp(zbuf1*cpexp+zbuf2*cpexp2)

            aksi3(ji,jj,jk) = aksi
C
C  2.10 APPARENT SOLUBILITY PRODUCT K'SP OF CALCITE (OR ARAGONITE)
C        AS FUNCTION OF PRESSURE FOLLWING EDMOND AND GIESKES (1970)
C        (P. 1285) AND BERNER (1976)
C -----------------------------------------------------------------
C
            aksp(ji,jj,jk) = aksp0*exp(cpexp*(devks-devkst*tc))
C
C* 2.11 TOTAL BORATE CONCENTR. [MOLES/L]
C --------------------------------------
C
            borat(ji,jj,jk) = bor1*cl*bor2
C
C  2.12 Iron and SIO3 saturation concentration from ...
C  ----------------------------------------------------
C
         sio3eq(ji,jj,jk)=exp(log(10.)*(6.44-968./tkel))*1E-6
         fekeq(ji,jj,jk)=10**(17.27-1565.7/(273.15+tc))
C
          ENDDO
        ENDDO
      END DO
C      
#endif
C
      RETURN
      END

