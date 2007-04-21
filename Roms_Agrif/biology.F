!
! $Id: biology.F,v 1.5 2005/01/19 15:03:28 pmarches Exp $
!
#include "cppdefs.h"
#if defined SOLVE3D && defined BIOLOGY && defined BIO_NPZD
# include "bio_NPZD.F"
#elif defined SOLVE3D && defined BIOLOGY && defined BIO_N2PZD2
# include "bio_N2PZD2.F"
#elif defined SOLVE3D && defined BIOLOGY && defined BIO_N2P2Z2D2
# include "bio_N2P2Z2D2.F"
#else
      subroutine biology_empty
      end
#endif

