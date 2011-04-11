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
      SUBROUTINE p4zlim(Istr,Iend,Jstr,Jend)
#include "cppdefs.h"
      Implicit NONE
      INTEGER Istr,Jstr,Iend,Jend

#if defined key_passivetrc && defined key_trc_pisces
CCC---------------------------------------------------------------------
CCC
CCC             ROUTINE p4zlim : PISCES MODEL
CCC             *****************************
CCC
CCC  PURPOSE :
CCC  ---------
CCC         Compute the co-limitations by the various nutrients
CCC         for the various phytoplankton species
CCC
CC   INPUT :
CC   -----
CC      argument
CC              None
CC      common
CC              all the common defined in opa
CC
CC
CC   OUTPUT :                   : no
CC   ------
CC
CC   MODIFICATIONS:
CC   --------------
CC      original  : O. Aumont (2004)
CC----------------------------------------------------------------------
CC parameters and commons
CC ======================
#include "param.h"
#include "parameter.h"
#include "common.h"
CC----------------------------------------------------------------------
CC local declarations
CC ==================
      INTEGER ji, jj, jk
      REAL xlim1,xlim2,xlim3,xlim4,zferlim,zno3
      REAL xconctemp,xconctemp2,xconctempn,xconctempn2
C
C  Tuning of the iron concentration to a minimum
C  level that is set to the detection limit
C  -------------------------------------
C
      DO jk = 1, jpk
        DO jj = Jstr, Jend
          DO ji = Istr, Iend
        zno3=trn(ji,jj,jk,jpno3)*1E6
        zferlim=max(1.5E-11*(zno3/40)**2,3E-12)
        zferlim=min(zferlim,1.5E-11)
        trn(ji,jj,jk,jpfer)=max(trn(ji,jj,jk,jpfer),zferlim)
          END DO
        END DO
      END DO
C
C  Computation of a variable Ks for iron on diatoms
C  taking into account that increasing biomass is
C  made of generally bigger cells
C  ------------------------------------------------
C
      DO jk=1,jpk
        DO jj = Jstr, Jend
          DO ji = Istr, Iend
        xconctemp=max(0.,trn(ji,jj,jk,jpdia)-5E-7)
        xconctemp2=min(5.E-7,trn(ji,jj,jk,jpdia))
        xconctempn=max(0.,trn(ji,jj,jk,jpphy)-1E-6)
        xconctempn2=min(1.E-6,trn(ji,jj,jk,jpphy))
        concdfe(ji,jj,jk)=(xconctemp2*conc3+0.4E-9*
     &    xconctemp)/(xconctemp2+xconctemp+rtrn)
        concdfe(ji,jj,jk)=max(conc3,concdfe(ji,jj,jk))
        concnfe(ji,jj,jk)=(xconctempn2*conc2+0.08E-9*
     &    xconctempn)/(xconctempn2+xconctempn+rtrn)
        concnfe(ji,jj,jk)=max(conc2,concnfe(ji,jj,jk))
          END DO
        END DO
      END DO
C
      DO jk = 1,jpk
        DO jj = Jstr, Jend
          DO ji = Istr, Iend
C    
C      Michaelis-Menten Limitation term for nutrients
C      Small flagellates
C      -----------------------------------------------
C
        xnanono3(ji,jj,jk)=trn(ji,jj,jk,jpno3)*concnnh4
     &      /(conc0*concnnh4+concnnh4*trn(ji,jj,jk,jpno3)+
     &        conc0*trn(ji,jj,jk,jpnh4))
        xnanonh4(ji,jj,jk)=trn(ji,jj,jk,jpnh4)*conc0
     &      /(conc0*concnnh4+concnnh4*trn(ji,jj,jk,jpno3)+
     &        conc0*trn(ji,jj,jk,jpnh4))
        xlim1=xnanono3(ji,jj,jk)+xnanonh4(ji,jj,jk)
        xlim2=trn(ji,jj,jk,jppo4)/(trn(ji,jj,jk,jppo4)+concnnh4)
        xlim3=trn(ji,jj,jk,jpfer)/(trn(ji,jj,jk,jpfer)
     &     +concnfe(ji,jj,jk))
        xlimphy(ji,jj,jk)=min(xlim1,xlim2,xlim3)
        xlim1=trn(ji,jj,jk,jpnh4)/(concnnh4+trn(ji,jj,jk,jpnh4))
        xlim3=trn(ji,jj,jk,jpfer)/(trn(ji,jj,jk,jpfer)+conc2)
        xlim4=trn(ji,jj,jk,jpdoc)/(trn(ji,jj,jk,jpdoc)+xkdoc2)
        xlimbac(ji,jj,jk)=min(xlim1,xlim2,xlim3)*xlim4
C
          END DO
        END DO
      END DO
C
      DO jk = 1,jpk
        DO jj = Jstr, Jend
          DO ji = Istr, Iend
C
C   Michaelis-Menten Limitation term for nutrients
C   Diatoms
C   ----------------------------------------------
C
        xdiatno3(ji,jj,jk)=trn(ji,jj,jk,jpno3)*concdnh4
     &      /(conc1*concdnh4+concdnh4*trn(ji,jj,jk,jpno3)+
     &        conc1*trn(ji,jj,jk,jpnh4))
        xdiatnh4(ji,jj,jk)=trn(ji,jj,jk,jpnh4)*conc1
     &      /(conc1*concdnh4+concdnh4*trn(ji,jj,jk,jpno3)+
     &        conc1*trn(ji,jj,jk,jpnh4))

        xlim1=xdiatno3(ji,jj,jk)+xdiatnh4(ji,jj,jk)
        xlim2=trn(ji,jj,jk,jppo4)/(trn(ji,jj,jk,jppo4)+concdnh4)
        xlim3=trn(ji,jj,jk,jpsil)/(trn(ji,jj,jk,jpsil)+xksi2)
        xlim4=trn(ji,jj,jk,jpfer)/(trn(ji,jj,jk,jpfer)
     &      +concdfe(ji,jj,jk))
        xlimdia(ji,jj,jk)=min(xlim1,xlim2,xlim3,xlim4)
        xlim4=trn(ji,jj,jk,jpfer)/(trn(ji,jj,jk,jpfer)+conc3)
        xlimdia2(ji,jj,jk)=min(xlim1,xlim2,xlim3,xlim4)
C
          END DO
        END DO
      END DO
C
#endif
      RETURN
      END

