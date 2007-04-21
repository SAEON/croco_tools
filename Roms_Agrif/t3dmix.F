!
! $Id: t3dmix.F,v 1.3 2005/09/30 15:11:47 pmarches Exp $
!
#include "cppdefs.h"
#if defined SOLVE3D && defined MIX_GP_TS
# include "t3dmix_GP.F"
#elif defined SOLVE3D && defined MIX_S_TS
# include "t3dmix_S.F"
#else
      subroutine t3dmix_empty
      end
#endif

