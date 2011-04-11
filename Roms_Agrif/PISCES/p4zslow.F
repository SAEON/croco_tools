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
      SUBROUTINE p4zslow(kt,Istr,Iend,Jstr,Jend)
#include "cppdefs.h"
      Implicit NONE
      Integer KT,Istr,Iend,Jstr,Jend
#if defined key_passivetrc && defined key_trc_pisces
CCC---------------------------------------------------------------------
CCC
CCC          ROUTINE p4zslow : PISCES MODEL
CCC          *****************************
CCC
CCC     PURPOSE.
CCC     --------
CCC          *P4ZSLOW* CALL VARIOUS ROUTINES ON A DAILY BASIS
CCC
CCC
CC     EXTERNALS.
CC     ----------
CC          p4zche, p4zint
CC
CC   MODIFICATIONS:
CC   --------------
CC      NEW : 2004 O. Aumont
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
C
C* 1. COMPUTATION OF THE CHEMICAL CONSTANTS
C  ----------------------------------------
C
       CALL p4zche(Istr,Iend,Jstr,Jend)
C
C  2. COMPUTATION OF VARIOUS RATES FOR BIOGEOCHEMISTRY
C  ---------------------------------------------------
C
       CALL p4zint(kt,Istr,Iend,Jstr,Jend)
C
C      
#endif
C
      RETURN
      END

