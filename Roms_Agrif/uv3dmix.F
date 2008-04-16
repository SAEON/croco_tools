!
! $Id: uv3dmix.F,v 1.1 2005/09/30 15:11:47 pmarches Exp $
!
#include "cppdefs.h"
#if defined SOLVE3D && defined UV_VIS2 && defined MIX_S_UV
# include "uv3dmix_S.F"
#elif defined SOLVE3D && defined UV_VIS2 && defined MIX_GP_UV
# include "uv3dmix_GP.F"
#elif defined SOLVE3D && defined UV_VIS4 && defined MIX_S_UV
# include "uv3dmix4_S.F"
#elif defined SOLVE3D && defined UV_VIS4 && defined MIX_GP_UV
# include "uv3dmix4_GP.F"
#else
      subroutine uv3dmix_empty
      end
#endif

