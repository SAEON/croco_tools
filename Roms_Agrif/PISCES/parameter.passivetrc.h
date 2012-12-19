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
#if defined key_passivetrc
CCC---------------------------------------------------------------------
CCC
CCC                         PARAMETER passivetrc
CCC                       ************************
CCC
CCC  PURPOSE :
CCC  ---------
CCC     Include parameter FILE for passive tracer
CCC
CC   MODIFICATIONS :
CC   -------------
CC	original : 96 (M. Levy)
CC                 07/99 (M. Levy for LOBSTER1 or NPZD model)
CC                 04/00 (O. Aumont, M.A. Foujols) HAMOCC3 and P3ZD
CC
CCC---------------------------------------------------------------------
CCC  OPA8, LODYC (15/11/96)
CCC---------------------------------------------------------------------
CC
CC passive tracers
CC ---------------
CC       jptra  : number of passive tracers
CC
#    if defined key_trc_pisces
#    include "parameter.passivetrc.pisces.h"
#    else
CC    default CASE : temperature and salinity as passive tracers
      INTEGER jptra
      PARAMETER (jptra = 2)
#    endif
#    if defined key_trc_diaadd || defined key_trc_dia3d
CC
CC possibility for additional 3d and 2d output
CC -------------------------------------------
CC
      INTEGER jpdia3d, jpdia2d

#       if defined key_trc_pisces
      PARAMETER (jpdia3d = NumFluxTerms, jpdia2d = NumVSinkTerms)
#       else
      PARAMETER (jpdia3d = 1, jpdia2d = 1)
#       endif

#    endif
#else
CC
CC no passive tracer 
CC
#endif
