! $Id: lmd_skpp.F 1477 2014-02-17 16:00:43Z gcambon $
!
!======================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al)
! and Rutgers University (Arango et al) are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
!
! ROMS_AGRIF website : http://www.romsagrif.org
!======================================================================
!
#include "cppdefs.h"

#ifdef LMD_SKPP
# if defined LMD_SKPP2005
#  include "lmd_skpp2005.F"
#else
#  include "lmd_skpp1994.F"
#endif
#else
!========================
      subroutine lmd_skpp_empty
      return
      end  
!========================          
#endif
