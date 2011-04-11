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
      SUBROUTINE p4zsink2(Istr,Iend,Jstr,Jend,wstmp,sinktemp,jn,rfacts)
#include "cppdefs.h"
      Implicit NONE
      INTEGER Istr,Jstr,Iend,Jend,jn
      REAL rfacts

#if defined key_passivetrc && defined key_trc_pisces
!!!
!!!       p4zsink2 : PISCES model
!!!       ***********************
!!!
!!
!!  PURPOSE :
!!  ---------
!!     Compute the sedimentation terms for the various sinking
!!     particles. The scheme used to compute the trends is based
!!     on MUSCL. 
!!
!!   METHOD :
!!   -------
!!      this ROUTINE compute not exactly the advection but the
!!      transport term, i.e.  div(u*tra).
!!
!!
!!   REFERENCES :                
!!   ----------                  
!!
!!   References :
!!      Estubier, A., and M. Levy, Notes Techn. Pole de Modelisation
!!      IPSL, Sept. 2000 (http://www.lodyc.jussieu.fr/opa)
!!
!!
!!   MODIFICATIONS:
!!   --------------
!!       original :  06-00 (A.Estublier)
!!       modifications : 2004 (O. Aumont)
!!       
!!----------------------------------------------------------------------
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
      INTEGER ji,jj,jk,jnt
      REAL ztraz(Istr:Iend,Jstr:Jend,jpk+1)
      REAL zakz(Istr:Iend,Jstr:Jend,jpk+1)
      REAL zkz(PRIV_3D_BIOARRAY)
      REAL zigma,zew,zstep,zign
      REAL wstmp(jpi,jpj,jpk)
      REAL sinktemp(jpi,jpj,jpk+1)

      REAL wstmp2(Istr:Iend,Jstr:Jend,jpk+1)

!!!---------------------------------------------------------------------
!!!  OPA8, LODYC (01/00)
!!!---------------------------------------------------------------------
! 1. Initialization
! --------------

        zstep=rfacts

        ztraz  = 0
        zkz    = 0
        zakz   = 0.

      DO jk=1,jpkm1
        DO jj=Jstr, Jend
          DO ji=Istr, Iend
         wstmp2(ji,jj,jk+1)=-wstmp(ji,jj,jk)/rjjss*tmask(ji,jj,jk+1)
          end do
         end do
      end do
 
      DO jj=Jstr, Jend
        DO ji=Istr, Iend
        wstmp2(ji,jj,1)=0.
        wstmp2(ji,jj,jpk+1)=0.
        END DO
      END DO
!
! 3. Vertical advective flux
!-------------------------------
! ... first guess of the slopes
!   ... interior values
!
       do jnt=1,2
        DO jk=2,jpk
          DO jj=Jstr, Jend
            DO ji=Istr, Iend
              ztraz(ji,jj,jk) =(trn(ji,jj,jk-1,jn)-trn(ji,jj,jk,jn))
     $                          *tmask(ji,jj,jk)
            END DO
          END DO
        END DO
!
      DO jj=Jstr, Jend
        DO ji=Istr, Iend
        ztraz(ji,jj,jpk+1)=0.
        END DO
      END DO
!
! slopes
!
        DO jk=2,jpk
          DO jj=Jstr, Jend
            DO ji=Istr, Iend
            zign = 0.5*(sign(1.,ztraz(ji,jj,jk)*ztraz(ji,jj,jk+1))+1)
            zakz(ji,jj,jk) = 0.5*(ztraz(ji,jj,jk)
     $                          +ztraz(ji,jj,jk+1))*zign
            ENDDO
          ENDDO
        ENDDO        
!
! Slopes limitation
!
        DO jk=2,jpk
          DO jj=Jstr, Jend
            DO ji=Istr, Iend
              zakz(ji,jj,jk) = sign(1.,zakz(ji,jj,jk)) * 
     $                        min(abs(zakz(ji,jj,jk)),
     $                        2.*abs(ztraz(ji,jj,jk+1)),
     $                        2.*abs(ztraz(ji,jj,jk)))
            ENDDO
          ENDDO
        ENDDO        

! vertical advective flux
        DO jk=1,jpk
          DO jj=Jstr, Jend
            DO ji=Istr, Iend
              zigma = wstmp2(ji,jj,jk+1)*zstep/fse3w(ji,jj,jk+1)
              zew   = wstmp2(ji,jj,jk+1)
              sinktemp(ji,jj,jk+1) = -zew*(trn(ji,jj,jk,jn)
     $           -0.5*(1+zigma)*zakz(ji,jj,jk))*zstep
            ENDDO
          ENDDO
        ENDDO 
!
! Boundary conditions
!
      DO jj=Jstr, Jend
        DO ji=Istr, Iend
         sinktemp(ji,jj,1)=0.
         sinktemp(ji,jj,jpk+1)=0.
        END DO
      END DO
!
       DO jk=1,jpk
         DO jj=Jstr, Jend
           DO ji=Istr, Iend
!
            trn(ji,jj,jk,jn) = trn(ji,jj,jk,jn)
     &        + (sinktemp(ji,jj,jk)-sinktemp(ji,jj,jk+1))
     &        /fse3t(ji,jj,jk)
!
            ENDDO
          ENDDO
        ENDDO
        ENDDO
!
        DO jk=1,jpk
          DO jj=Jstr, Jend
            DO ji=Istr, Iend
!
            trb(ji,jj,jk,jn) = trb(ji,jj,jk,jn)
     &        + 2.*(sinktemp(ji,jj,jk)-sinktemp(ji,jj,jk+1))
     &        /fse3t(ji,jj,jk)
!
            ENDDO
          ENDDO
        ENDDO

       DO jk=1,jpk
         DO jj=Jstr, Jend
           DO ji=Istr, Iend
        trn(ji,jj,jk,jn)=trb(ji,jj,jk,jn)
           ENDDO
         ENDDO
       ENDDO
!
#else 

      REAL wstmp
      REAL sinktemp

#endif
C
      RETURN
      END

