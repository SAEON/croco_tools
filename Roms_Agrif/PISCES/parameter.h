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
CCC---------------------------------------------------------------------
CCC
CCC                         PARAMETER
CCC                       *************
CCC
CCC  GLOBAL VERSION
CCC
CCC  PURPOSE :
CCC  ---------
CCC     Include parameter file
CCC
CC   MODIFICATIONS :
CC   -------------
CC      original : 91 (Imbard, Levy, Madec)
CC
CCC---------------------------------------------------------------------
CCC  OPA8, LODYC (15/11/96)
CCC---------------------------------------------------------------------
CC
CC Several domain sizes are parameterized
CC                                  global      domain   (jpiglo,jpjglo)
CC                                  computation domain   ( jpi  , jpj  )
CC                                  data        domain   (jpidta,jpjdta)
CC
CC
CC Large domain matrix size
CC ------------------------
CC
CC      jpiglo  : first  dimension of global domain --> i
CC      jpjglo  : second dimension of global domain --> j
CC      jpk     : number of vertical levels
CC
      INTEGER jpiglo,jpjglo,jpk
CC
CC Domain characteristics
CC ----------------------
CC      jperio  : lateral cond. type for the global domain (4, 3, 2, 1
CC                or 0)
CC
      PARAMETER(jpk=N)
#if !defined AGRIF
      PARAMETER(jpiglo=LLm0,jpjglo=MMm0)
#endif

C
CC
CC Matrix size
CC -----------
CC      jpi     : first  dimension of grid --> i
CC      jpi     : first  dimension of grid --> i
CC
      INTEGER jpi,jpj
#if !defined AGRIF
      PARAMETER(jpi=Lm)
      PARAMETER(jpj=Mm)
#endif
C
CC
CC Other dimension parameters
CC --------------------------
CC      jpim1   :  jpi - 1
CC      jpjm1   :  jpj - 1
CC      jpkm1   :  jpk - 1
CC
      INTEGER jpim1,jpjm1,jpkm1
      PARAMETER(jpkm1=jpk-1)
#if !defined AGRIF
      PARAMETER(jpim1=jpi-1,jpjm1=jpj-1)
#endif

#ifdef AGRIF
      common/compisces/jpiglo,jpjglo,jpi,jpj,jpim1,jpjm1
#endif

CC
CC Standard output
CC ---------------
CC
      INTEGER numout
      PARAMETER(numout=6)
C
CC Passive tracers parameter
CC -------------------------
CC
#if defined key_passivetrc
#    include "parameter.passivetrc.h"
#endif

