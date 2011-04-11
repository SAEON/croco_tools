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
      SUBROUTINE pirostep(Istr,Iend,Jstr,Jend)
C
#include "cppdefs.h"
      Implicit NONE
      INTEGER Istr,Iend,Jstr,Jend
#if defined key_passivetrc && defined key_trc_pisces
CC   ------------------------------------------------------------------
CCC
CCC   ROUTINE romspisces : PISCES MODEL
CCC   *****************************
CCC
CC
CC     PURPOSE.
CC     --------
CC          *ROMSPISCES* ECOSYSTEM MODEL IN THE WHOLE OCEAN
CC                       THIS ROUTINE COMPUTES THE DIFFERENT
C                        INTERACTIONS
CC                       BETWEEN THE DIFFERENT COMPARTMENTS OF THE
C                        MODEL
CC ----------------------------------------------------------------
CC parameters and commons
CC ======================
#include "param.h"
#include "parameter.h"
#include "common.h"
#include "grid.h"
#include "ocean3d.h"
#include "scalars.h"
#define PRIV_3D_BIOARRAY Istr:Iend,Jstr:Jend,1:jpk

      INTEGER i,j,k,itrc
      REAL zex(PRIV_3D_BIOARRAY)
C
C         Passage de variables biologiques
C         --------------------------------
      DO itrc=1,jptra
        DO k=1,jpk
          DO j=Jstr,Jend
            DO i=Istr,Iend
              trn(i,j,k,itrc)=trn(i,j,k,itrc)*1E6
            END DO
          END DO
        END DO
      END DO

      DO k=1,jpk
        DO j=Jstr,Jend
          DO i=Istr,Iend
            trn(i,j,k,jpno3)=trn(i,j,k,jpno3)/7.6
            trn(i,j,k,jpnh4)=trn(i,j,k,jpnh4)/7.6
            trn(i,j,k,jppo4)=trn(i,j,k,jppo4)/122.
          END DO
        END DO
      END DO


      DO itrc=1,jptra
        DO k=1,jpk
          DO j=Jstr,Jend
            DO i=Istr,Iend
              t(i,j,k,nnew,itrc+2)=trn(i,j,jpk+1-k,itrc)
            END DO
          END DO
        END DO
      END DO
# ifdef key_trc_dia3d
      DO itrc=1,jpdia3d
        DO k=1,jpk
          DO j=Jstr,Jend
            DO i=Istr,Iend
               zex(i,j,k)=bioFlux(i,j,jpk+1-k,itrc)
            END DO
          END DO
        END DO
        DO k=1,jpk
          DO j=Jstr,Jend
            DO i=Istr,Iend
               bioFlux(i,j,k,itrc)=zex(i,j,k)
            END DO
          END DO
        END DO
      END DO
# endif


#endif
       END
