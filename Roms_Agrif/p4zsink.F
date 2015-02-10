! $Id: p4zsink.F 1503 2014-03-19 14:54:00Z gcambon $
!
!=========================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France.
! The two other branches, from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al), are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
!
! ROMS_AGRIF website : http://www.romsagrif.org
!=========================================================================
!
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
CC      revisions : 2007 - C. Menkes
CC                         P. Marchesiello
CC                  2013 - T. Gorgues
CC----------------------------------------------------------------------
CC parameters and commons
CC ======================
# include "param.h"
# include "parameter.h"
# include "common.h"
CC----------------------------------------------------------------------
CC local declarations
CC ==================
      INTEGER jksed, ji, jj, jk, jnt, iiter1, iiter2
      REAL xagg1,xagg2,xagg3,xagg4,zmax
      REAL zfact,zstep,wsmax,rfacts1,rfacts2
# if defined MPI
      include 'mpif.h'
      integer ierr,iiter1tmp,iiter2tmp
# endif
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
      DO jk=1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
c           Here we use now the max of the euphotic zone
c           and the mld instead of just the mld
            zmax = MAX( zmeu(ji,jj), hmld(ji,jj) )
c           Slight modification of the parameterization
c           of the increased velocity of the GOC
            zfact= MAX(0.,fsdepw(ji,jj,jk+1) - zmax )/5000.
c           zfact= MAX(0.,fsdepw(ji,jj,jk+1) - zmax )/2000.
            wsbio4(ji,jj,jk)=wsbio2+(200.-wsbio2)*zfact
      ! limit the values of the sinking speeds to avoid numerical instabilities
            wsbio3(ji,jj,jk)=wsbio
            wscal(ji,jj,jk)=wsbio4(ji,jj,jk)
          END DO
        END DO
      END DO

      ! OA This is (I hope) a temporary solution for the problem that may 
      ! OA arise in specific situation where the CFL criterion is broken 
      ! OA for vertical sedimentation of particles. To avoid this, a time
      ! OA splitting algorithm has been coded. A specific maximum
      ! OA iteration number is provided and may be specified in the namelist 
      ! OA This is to avoid very large iteration number when explicit free
      ! OA surface is used (for instance). When niter?max is set to 1, 
      ! OA this computation is skipped. The crude old threshold method is 
      ! OA then applied. This also happens when niter exceeds nitermax.


      IF( MAX( niter1max, niter2max ) == 1 ) THEN
        iiter1 = 1
        iiter2 = 1
      ELSE
        iiter1 = 1
        iiter2 = 1
        DO jk = 1, jpkm1
          DO jj = Jstr, Jend
             DO ji = Istr, Iend
                IF( tmask(ji,jj,jk) == 1) THEN
                   wsmax =  0.4 * fse3t(ji,jj,jk) / zstep
                   iiter1 =  MAX( iiter1, INT( wsbio3(ji,jj,jk) 
     &                    / wsmax ) )
                   iiter2 =  MAX( iiter2, INT( wsbio4(ji,jj,jk) 
     &                    / wsmax ) )
                ENDIF
             END DO
          END DO
        END DO
# if defined MPI
      call MPI_ALLREDUCE(iiter1,iiter1tmp,1,MPI_INTEGER,
     &                   MPI_MAX,
     &                   MPI_COMM_WORLD,ierr)
      call MPI_ALLREDUCE(iiter2,iiter2tmp,1,MPI_INTEGER,
     &                   MPI_MAX,
     &                   MPI_COMM_WORLD,ierr)
        iiter1=iiter1tmp
        iiter2=iiter2tmp
# endif 

        iiter1 = MIN( iiter1, niter1max )
        iiter2 = MIN( iiter2, niter2max )
      ENDIF

      DO jk = 1,jpkm1
         DO jj = Jstr, Jend
            DO ji = Istr, Iend
               IF( tmask(ji,jj,jk) == 1 ) THEN
                 wsmax = 0.4 * fse3t(ji,jj,jk) / zstep
                 wsbio3(ji,jj,jk) = MIN( wsbio3(ji,jj,jk), wsmax 
     &                            * FLOAT( iiter1 ) )
                 wsbio4(ji,jj,jk) = MIN( wsbio4(ji,jj,jk), wsmax 
     &                            * FLOAT( iiter2 ) )
               ENDIF
            END DO
         END DO
      END DO


C
C   INITIALIZE TO ZERO ALL THE SINKING ARRAYS
C   -----------------------------------------
C
      DO jk=1,jpk
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
            sinking(ji,jj,jk)  = 0.
            sinking2(ji,jj,jk) = 0.
            sinkcal(ji,jj,jk)  = 0.
            sinkfer(ji,jj,jk)  = 0.
            sinksil(ji,jj,jk)  = 0.
            sinkfer2(ji,jj,jk) = 0.
          END DO
        END DO
      END DO

      DO jnt=1,iiter1
        CALL p4zsink2(Istr,Iend,Jstr,Jend,wsbio3,sinking,jppoc,iiter1)
        CALL p4zsink2(Istr,Iend,Jstr,Jend,wsbio3,sinkfer,jpsfe,iiter1)
      END DO
      DO jnt=1,iiter2
        CALL p4zsink2(Istr,Iend,Jstr,Jend,wsbio4,sinking2,jpgoc,iiter2)
        CALL p4zsink2(Istr,Iend,Jstr,Jend,wsbio4,sinkfer2,jpbfe,iiter2)
        CALL p4zsink2(Istr,Iend,Jstr,Jend,wsbio4,sinksil ,jpdsi,iiter2)
        CALL p4zsink2(Istr,Iend,Jstr,Jend,wscal ,sinkcal ,jpcal,iiter2)
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
            xagg1=25.9*zfact*trn(ji,jj,jk,jppoc)*trn(ji,jj,jk,jppoc)
C
            xagg2=4452*zfact*trn(ji,jj,jk,jppoc)*trn(ji,jj,jk,jpgoc)
C
C    Aggregation of small into large particles
C    Part II : Differential settling
C    ----------------------------------------------
C
            xagg3=47.1*zstep*trn(ji,jj,jk,jppoc)*trn(ji,jj,jk,jpgoc)
            xagg4=3.3*zstep*trn(ji,jj,jk,jppoc)*trn(ji,jj,jk,jppoc)
C
            xagg(ji,jj,jk)=xagg1+xagg2+xagg3+xagg4
            xaggfe(ji,jj,jk)=xagg(ji,jj,jk)*trn(ji,jj,jk,jpsfe)/
     &                                (trn(ji,jj,jk,jppoc)+rtrn)
C
C     Aggregation of DOC to small particles
C     --------------------------------------
C
c     Old parameterization
c            xaggdoc(ji,jj,jk)=(80*trn(ji,jj,jk,jpdoc)+698.
c     &           *trn(ji,jj,jk,jppoc))*zfact*trn(ji,jj,jk,jpdoc)

            xaggdoc(ji,jj,jk) = ( ( 0.369 * 0.3 * trn(ji,jj,jk,jpdoc) 
     &           + 102.4 * trn(ji,jj,jk,jppoc) ) * zfact + 2.4 * zstep 
     &           * trn(ji,jj,jk,jppoc) ) * 0.3 * trn(ji,jj,jk,jpdoc)
C
c     Old parameterization
c            xaggdoc2(ji,jj,jk)=1.05E4*trn(ji,jj,jk,jpgoc)
c     &                                *zfact*trn(ji,jj,jk,jpdoc)

            xaggdoc2(ji,jj,jk) = ( 3.53E3 * zfact + 0.1 * zstep ) 
     &           * trn(ji,jj,jk,jpgoc) * 0.3 * trn(ji,jj,jk,jpdoc)
C
c    addition of agregation of doc into poc
            xaggdoc3(ji,jj,jk) = ( 5095. * trn(ji,jj,jk,jppoc) + 114. 
     &           * 0.3 * trn(ji,jj,jk,jpdoc) ) *zstep * 0.3 
     &           * trn(ji,jj,jk,jpdoc)

          END DO
        END DO
      END DO

C#    if defined key_trc_dia3d
# if defined key_trc_diaadd
      DO jj=Jstr, Jend
        DO ji=Istr, Iend
C           jksed=zmeuindex(ji,jj)
C
           jksed=jpk-1
           bioVSink(ji,jj,5) = sinking(ji,jj,jksed+1)*1.e3
     &                            *rfact2r*tmask(ji,jj,jksed+1)
           bioVSink(ji,jj,6) = sinking2(ji,jj,jksed+1)*1.e3
     &                            *rfact2r*tmask(ji,jj,jksed+1)
           bioVSink(ji,jj,7) = sinkfer(ji,jj,jksed+1)*1.e3
     &                            *rfact2r*tmask(ji,jj,jksed+1)
           bioVSink(ji,jj,8) = sinkfer2(ji,jj,jksed+1)*1.e3
     &                            *rfact2r*tmask(ji,jj,jksed+1)
           bioVSink(ji,jj,9) = sinksil(ji,jj,jksed+1)*1.e3
     &                            *rfact2r*tmask(ji,jj,jksed+1)
           bioVSink(ji,jj,10) = sinkcal(ji,jj,jksed+1)*1.e3
     &                            *rfact2r*tmask(ji,jj,jksed+1)
        END DO
      END DO
# endif
C
#endif
      RETURN
      END

