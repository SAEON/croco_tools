!
! $Id: work.h,v 1.2 2003/12/17 13:56:08 pmarches Exp $
!
!
! This is "work.h": declaration of utility work array.
!
#ifdef SOLVE3D
      real work(GLOBAL_2D_ARRAY,0:N)
      common /work3d/ work
#endif

      real work2d(GLOBAL_2D_ARRAY)
      common /work2d/ work2d

      real work2d2(GLOBAL_2D_ARRAY)
      common /work2d2/ work2d2
