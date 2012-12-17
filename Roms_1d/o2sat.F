! $Id$
!======================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
! 
! ROMS_AGRIF website : http://www.romsagrif.org
!======================================================================
      SUBROUTINE O2SAT(DISSO2,THETA,SALT,RHOST0,O2SATU,SATPC,AOU)
C
C     TITLE:
C     *****
C
C       O2SAT  -- CALCULATE O2 SATURATION, %SATURATION AND APPARENT
C                 O2 UTILIZATION (AOU)
C
C     PARAMETERS:
C     **********
C
C       DISSO2 -> DISSOLVED O2 (ML/L)
C       THETA  -> POTENTIAL TEMPERATURE (CELSIUS DEGREES)
C       SALT   -> SALINITY (0/00)
C       RHOST0 -> INSITU DENSITY (S,T,0)
C       O2SATU <- O2 SATURATION (ML/L)
C       SATPC  <- % SATURATION
C       AOU    <- APPARENT O2 UTILIZATION (ML/L)
C
        REAL DISSO2,THETA,SALT,RHOST0,O2SATU,SATPC,AOU
C
C     VARIABLES:
C     *********
C
        REAL*4 A,KELVIN,K100
C
C     CODE:
C     ****
C
      KELVIN = THETA +273.15
      K100   = KELVIN/100.
      A      = -177.7888+255.5907/K100+146.4813*LOG(K100)-22.204*K100
      A      = A + SALT*(-0.037362+K100*(0.016504-0.0020564*K100))
      O2SATU = EXP(A)*RHOST0/1000.0
C
      SATPC  = DISSO2/O2SATU*100.0
      AOU    = O2SATU-DISSO2
C
      RETURN
C
C     END O2SAT
C
      END
