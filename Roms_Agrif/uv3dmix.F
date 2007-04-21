!
! $Id: uv3dmix.F,v 1.1 2005/09/30 15:11:47 pmarches Exp $
!
#include "cppdefs.h"
#if defined SOLVE3D && defined MIX_GP_UV
# include "uv3dmix_GP.F"
#elif defined SOLVE3D && defined MIX_S_UV
# include "uv3dmix_S.F"
#else
      subroutine uv3dmix_empty
      end
#endif

