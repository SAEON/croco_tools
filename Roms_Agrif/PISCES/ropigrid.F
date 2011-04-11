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
      SUBROUTINE ropigrid(Istr,Iend,Jstr,Jend)
C
#include "cppdefs.h"
      Implicit none
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

      INTEGER i,j,k
C
C        Passage des valeurs de run
C        --------------------------
C
       nit000=ntstart
       nwrite=nrst
       rdt=dt
       rpi=pi
       rday=day2sec
       raass=year2day*day2sec
       rmoss=raass/12.
       rjjss=day2sec
       jpib=Istr
       jpie=Iend
       jpjb=Jstr
       jpje=Jend
C
C        Facteurs de forme
C        -----------------
C
       DO j=Jstr,Jend
         DO i=Istr,Iend
       glamt(i,j)=lonr(i,j)
       gphit(i,j)=latr(i,j)
       e1t(i,j)=1./pm(i,j)
       e2t(i,j)=1./pn(i,j)
         END DO
       END DO
        
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
#endif

       END
