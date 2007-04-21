!
! $Id: init_arrays_sta.F,v 1.1 2005/01/21 17:02:10 pmarches Exp $
!
#include "cppdefs.h"
#ifdef STATIONS
      subroutine init_arrays_sta

      implicit none
#include "param.h"
#include "sta.h" 
#include "scalars.h"

      integer iflt, j, i, k
!
!
!  Initialize model control points
!
      do iflt=1,Msta
c       stagrd(iflt)=-1 ! -1 is the dead float value. 
        do j=istagrd,istazgrd  
           stainfo(j,iflt)=staspval
        enddo
        do j=1,NSTAVARS
          stadata(j,iflt)=0.
        enddo
# ifdef ALL_SIGMA
        do j=istadpt,istau  
         do k=1,N
           staSigm(j,iflt,k)=staspval
         enddo
        enddo
# endif
      enddo

      return
      end

#else
      subroutine init_arrays_sta_empty
      return
      end

#endif /* FLOATS */

