      SUBROUTINE ropistep(Istr,Iend,Jstr,Jend)
C
#include "cppdefs.h"
      Implicit NONE
      INTEGER Istr,Jstr,Iend,Jend

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
#include "forces.h"
#include "mixing.h"

      INTEGER i,j,k,itrc

      njulian=tdays
C
C        Passage de la grille verticale
C        ------------------------------
C
       DO k=1,jpk
         DO j=Jstr,Jend
           DO i=Istr,Iend
       fsdept(i,j,jpk+1-k)=-z_r(i,j,k)
       fse3t(i,j,jpk+1-k)=Hz(i,j,k)
           END DO
         END DO
       END DO
 
       DO k=1,jpk+1
         DO j=Jstr,Jend
           DO i=Istr,Iend
       fsdepw(i,j,jpk+2-k)=-z_w(i,j,k-1)
           END DO
         END DO
       END DO

       DO k=2,jpk
         DO j=Jstr,Jend
           DO i=Istr,Iend
       fse3w(i,j,k)=fsdept(i,j,k)-fsdept(i,j,k-1)
           END DO
         END DO
       END DO
                                                                                
         DO j=Jstr,Jend
           DO i=Istr,Iend
       fse3w(i,j,1)=2*fsdept(i,j,1)
       fse3w(i,j,jpk+1)=2*(fsdepw(i,j,jpk+1)-fsdept(i,j,jpk))
           END DO
         END DO
C
C      Passage des masques
C      -------------------
C
       DO k=1,jpk
         DO j=Jstr,Jend
           DO i=Istr,Iend
           tmask(i,j,jpk+1-k)=rmask(i,j)
           END DO
         END DO
       END DO
C
C        Passage de la dynamique
C        -----------------------
C
      DO k=1,jpk
        DO j=Jstr,Jend
          DO i=Istr,Iend
       tn(i,j,jpk+1-k)=t(i,j,k,nnew,itemp)
       sn(i,j,jpk+1-k)=t(i,j,k,nnew,isalt)
       avs(i,j,jpk+1-k)=akt(i,j,k,isalt)
       rhopn(i,j,jpk+1-k)=rho0+rho1(i,j,k)
          END DO
        END DO
      END DO  
C
C         Passage de variables 2D dynamiques
C         ----------------------------------
C     
      DO j=Jstr,Jend
        DO i=Istr,Iend
!      qsr(i,j)=rho0*Cp*srflxg(i,j,2)
      qsr(i,j)=rho0*Cp*srflx(i,j)
      hmld(i,j)=hbl(i,j)
      vatm(i,j)=sqrt(sqrt((sustr(i,j)*rho0)**2+(svstr(i,j)*rho0)
     &   **2)/1.25e-3)
        END DO
      END DO
C
C         Passage de variables biologiques
C         --------------------------------
      DO itrc=1,jptra
        DO k=1,jpk
          DO j=Jstr,Jend
            DO i=Istr,Iend
      trn(i,j,jpk+1-k,itrc)=max(0.,t(i,j,k,nnew,itrc+2))
            END DO
          END DO
        END DO
      END DO

        DO k=1,jpk
          DO j=Jstr,Jend
            DO i=Istr,Iend
      trn(i,j,k,jpno3)=trn(i,j,k,jpno3)*7.6
      trn(i,j,k,jpnh4)=trn(i,j,k,jpnh4)*7.6
      trn(i,j,k,jppo4)=trn(i,j,k,jppo4)*122.
            END DO
          END DO
        END DO

      DO itrc=1,jptra
        DO k=1,jpk
          DO j=Jstr,Jend
            DO i=Istr,Iend
      trn(i,j,k,itrc)=trn(i,j,k,itrc)*1E-6
      trb(i,j,k,itrc)=trn(i,j,k,itrc)
            END DO
          END DO
        END DO
      END DO
#endif
       END
