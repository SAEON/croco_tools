!
! $Id: biology.F,v 1.5 2005/01/19 15:03:28 pmarches Exp $
!
#include "cppdefs.h"
#if defined SOLVE3D && defined BIOLOGY && defined PISCES
# include "biology_pisces.F"
#elif defined SOLVE3D && defined BIOLOGY && defined BIO_NChlPZD
# include "bio_NChlPZD.F"
#elif defined SOLVE3D && defined BIOLOGY && defined BIO_N2ChlPZD2
# include "bio_N2ChlPZD2.F"
#elif defined SOLVE3D && defined BIOLOGY && defined BIO_N2P2Z2D2
# include "bio_N2P2Z2D2.F"
#else
      subroutine biology_empty
      end
#endif

