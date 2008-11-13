!
! $Id: private_scratch.h,v 1.3 2004/03/26 15:26:08 pmarches Exp $
!
#if defined AUTOTILING
      real,dimension(:,:,:), pointer :: A2d, A3d
#ifdef AGRIF
      real,dimension(:,:,:), pointer :: A1dXI, A1dETA
#endif
#else
      real A2d(N2d,NSA,0:NPP-1), A3d(N3d,4,0:NPP-1)
#ifdef AGRIF
      real A1dXI(N1dXI,10*NWEIGHT,0:NPP-1),
     &     A1dETA(N1dETA,10*NWEIGHT,0:NPP-1)
#endif      
#endif
#  ifdef SEDIMENT
      integer B2d(N2d,0:NPP-1)
#  endif
      common /private_scratch/ A2d,A3d
#  ifdef SEDIMENT
     &       /private_scratch_bis/ B2d 
#  endif
#  ifdef AGRIF
      common/private_scratch_agrif/A1dXI,A1dETA
#  endif
