! $Id: lmd_bkpp.F 1526 2014-04-16 14:09:11Z marchesiello $
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

#ifdef LMD_BKPP
# if defined LMD_BKPP2005
#  include "lmd_bkpp2005.F"
# else
#  include "lmd_bkpp1994.F"
# endif
#else
!========================
      subroutine lmd_bkpp_empty
      return
      end  
!========================          
#endif
