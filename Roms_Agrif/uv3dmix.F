! $Id: uv3dmix.F 1458 2014-02-03 15:01:25Z gcambon $
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
#if defined SOLVE3D && defined UV_VIS2 && defined UV_MIX_S
# include "uv3dmix_S.F"
#elif defined SOLVE3D && defined UV_VIS2 && defined UV_MIX_GEO
# include "uv3dmix_GP.F"
#elif defined SOLVE3D && defined UV_VIS4 && defined UV_MIX_S
# include "uv3dmix4_S.F"
#elif defined SOLVE3D && defined UV_VIS4 && defined UV_MIX_GEO
# include "uv3dmix4_GP.F"
#else
      subroutine uv3dmix_empty
      end
#endif

