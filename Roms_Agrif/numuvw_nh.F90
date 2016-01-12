#include "cppdefs.h"
#ifdef NBQ

      subroutine numuvw_nh

!*******************************************************************
!
!                   Numbering of Mass Points
!
!*******************************************************************

      use module_nh
      implicit none
# include "param_F90.h"
# include "scalars_F90.h"
# include "grid.h"
# include "def_bounds.h"

      integer   :: i,j,k,nzuvw_n   
      integer   :: jstr_n,jend_n


!*******************************************************************
!*******************************************************************
!     Initializations
!*******************************************************************
!*******************************************************************

      nzuvw_n      = 0
      mijk2lmom_nh = 0
      nequ_nh      = 0
      neqv_nh      = 0
      neqmom_nh    = 0

!*******************************************************************
!*******************************************************************
!     Momentum Equation: X-direction
!******************************************************************* 
!*******************************************************************

# ifdef MPI
!-------------------------------------------------------------------
!     WEST MPI Interface
!-------------------------------------------------------------------
      if (WEST_INTER) then
         i = istru_nh-1

!........Corner (South-West):
         if (SOUTH_INTER) then
            jstr_n = jstr_nh-1
         else
            jstr_n = jstr_nh
         endif
!........Corner (North-West):
         if (NORTH_INTER) then
            jend_n = jend_nh+1
         else
            jend_n = jend_nh
         endif

         do k=1,N    
         do j=jstr_n,jend_n
#  ifdef MASKING
            if (umask(i,j).ne.0) then
#  endif
               nzuvw_n               = nzuvw_n + 1 
               ijk2lmom_nh(i,j,k,1)  = nzuvw_n
               mijk2lmom_nh(i,j,k,1) = 1 
               l2imom_nh(nzuvw_n)    = i
               l2jmom_nh(nzuvw_n)    = j
               l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
            endif
#  endif
         enddo
         enddo

      endif

!-------------------------------------------------------------------
!     SOUTH MPI Interface
!-------------------------------------------------------------------
      if (SOUTH_INTER) then
         j = jstr_nh-1

         do i=istru_nh,iendu_nh
         do k=1,N    
#  ifdef MASKING
            if (umask(i,j).ne.0) then
#  endif
               nzuvw_n               = nzuvw_n + 1 
               ijk2lmom_nh(i,j,k,1)  = nzuvw_n
               mijk2lmom_nh(i,j,k,1) = 1 
               l2imom_nh(nzuvw_n)    = i
               l2jmom_nh(nzuvw_n)    = j
               l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
            endif
#  endif
         enddo
         enddo

      endif
# endif /* MPI */

      nequ_nh(1) = nzuvw_n

!-------------------------------------------------------------------
!     WESTERN EDGE
!-------------------------------------------------------------------
# ifdef OBC_NBQ_XXX
      if (WESTERN_EDGE) then
        i = istru_nh-1

!........Corner (South-West):
         if (SOUTH_INTER) then
            jstr_n = jstr_nh-1
         else
            jstr_n = jstr_nh
         endif
!........Corner (North-West):
         if (NORTH_INTER) then
            jend_n = jend_nh+1
         else
            jend_n = jend_nh
         endif

        do k=1,N    
        do j=jstr_n,jend_n
#  ifdef MASKING
          if (umask(i,j).ne.0) then
#  endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,1)  = nzuvw_n
            mijk2lmom_nh(i,j,k,1) = 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
          endif
#  endif
        enddo
        enddo

      endif
# endif /* OBC_NBQ_XXX */

!-------------------------------------------------------------------
!     SOUTHERN EDGE (tangential velocity)
!-------------------------------------------------------------------
# ifdef OBC_NBQ_XXX
      if (SOUTHERN_EDGE) then
        j = jstr_nh-1

        do k=1,N    
        do i=istru_nh-1,iendu_nh+1
#  ifdef MASKING
          if (umask(i,j).ne.0) then
#  endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,1)  = nzuvw_n
            mijk2lmom_nh(i,j,k,1) = 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
          endif
#  endif
        enddo
        enddo

      endif
# endif /* OBC_NBQ_XXX */

      nequ_nh(2) = nzuvw_n
      
!-------------------------------------------------------------------
!     Inner domain, bottom layer: (i,j,k=1)
!-------------------------------------------------------------------
      k=1
      do i=istru_nh,iendu_nh
      do j=jstr_nh,jend_nh          
# ifdef MASKING
         if (umask(i,j).ne.0) then
# endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,1)  = nzuvw_n
            mijk2lmom_nh(i,j,k,1) = 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
# ifdef MASKING
         endif
# endif
      enddo
      enddo

      nequ_nh(3) = nzuvw_n

!-------------------------------------------------------------------
!     Inner domain, inner layers: (i,j, 1<k<N )
!-------------------------------------------------------------------
      do i=istru_nh,iendu_nh
      do j=jstr_nh,jend_nh
      do k=2,N-1      
# ifdef MASKING
         if (umask(i,j).ne.0) then
# endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,1)  = nzuvw_n
            mijk2lmom_nh(i,j,k,1) = 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
# ifdef MASKING
         endif
# endif
      enddo
      enddo
      enddo

      nequ_nh(4) = nzuvw_n

!-------------------------------------------------------------------
!     Inner domain, surface layer: (i,j,k=N )
!-------------------------------------------------------------------
      k=N
      do i=istru_nh,iendu_nh
      do j=jstr_nh,jend_nh           
# ifdef MASKING
         if (umask(i,j).ne.0) then
# endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,1)  = nzuvw_n
            mijk2lmom_nh(i,j,k,1) = 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
# ifdef MASKING
         endif
# endif
      enddo
      enddo

      nequ_nh(5) = nzuvw_n

!-------------------------------------------------------------------
!     EASTERN EDGE
!-------------------------------------------------------------------
# ifdef OBC_NBQ_XXX
      if (EASTERN_EDGE) then
        i = iendu_nh+1

!........Corner (South-East):
         if (SOUTH_INTER) then
            jstr_n = jstr_nh-1
         else
            jstr_n = jstr_nh
         endif
!........Corner (North-East):
         if (NORTH_INTER) then
            jend_n = jend_nh+1
         else
            jend_n = jend_nh
         endif

        do j=jstr_n,jend_n
        do k=1,N                 
#  ifdef MASKING
          if (umask(i,j).ne.0) then
#  endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,1)  = nzuvw_n
            mijk2lmom_nh(i,j,k,1) = 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
          endif
#  endif
        enddo
        enddo

      endif
# endif /* OBC_NBQ_XXX */

!-------------------------------------------------------------------
!     NORTHERN EDGE (tangential velocity)
!-------------------------------------------------------------------
# ifdef OBC_NBQ_XXX
      if (NORTHERN_EDGE) then
        j = jend_nh+1

        do k=1,N    
        do i=istru_nh-1,iendu_nh+1
#  ifdef MASKING
          if (umask(i,j).ne.0) then
#  endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,1)  = nzuvw_n
            mijk2lmom_nh(i,j,k,1) = 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
          endif
#  endif
        enddo
        enddo

      endif
# endif /* OBC_NBQ_XXX */

      nequ_nh(6) = nzuvw_n

# ifdef MPI
!-------------------------------------------------------------------
!     EAST MPI Interface
!-------------------------------------------------------------------
      if (EAST_INTER) then
         i = iendu_nh+1

!........Corner (South-East):
         if (SOUTH_INTER) then
            jstr_n = jstr_nh-1
         else
            jstr_n = jstr_nh
         endif
!........Corner (North-East):
         if (NORTH_INTER) then
            jend_n = jend_nh+1
         else
            jend_n = jend_nh
         endif

         do j=jstr_n,jend_n
         do k=1,N                 
#  ifdef MASKING
            if (umask(i,j).ne.0) then
#  endif
               nzuvw_n               = nzuvw_n + 1 
               ijk2lmom_nh(i,j,k,1)  = nzuvw_n
               mijk2lmom_nh(i,j,k,1) = 1 
               l2imom_nh(nzuvw_n)    = i
               l2jmom_nh(nzuvw_n)    = j
               l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
            endif
#  endif
         enddo
         enddo
       endif

!-------------------------------------------------------------------
!     NORTH MPI Interface
!-------------------------------------------------------------------
      if (NORTH_INTER) then
         j = jend_nh+1

         do i=istru_nh,iendu_nh
         do k=1,N    
#  ifdef MASKING
            if (umask(i,j).ne.0) then
#  endif
               nzuvw_n               = nzuvw_n + 1 
               ijk2lmom_nh(i,j,k,1)  = nzuvw_n
               mijk2lmom_nh(i,j,k,1) = 1 
               l2imom_nh(nzuvw_n)    = i
               l2jmom_nh(nzuvw_n)    = j
               l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
            endif
#  endif
         enddo
         enddo
      endif
# endif /* MPI */

      nequ_nh(7)   = nzuvw_n

!
!.....Number of equations in X-direction:
!
      neqmom_nh(1) = nzuvw_n

!*******************************************************************
!*******************************************************************
!     Momentum Equation: Y-direction
!******************************************************************* 
!*******************************************************************

# ifdef MPI
!-------------------------------------------------------------------
!     WEST MPI Interface
!-------------------------------------------------------------------
      if (WEST_INTER) then

         i = istr_nh-1

!........Corner (South-West):
         if (SOUTH_INTER) then
            jstr_n = jstrv_nh-1
         else
            jstr_n = jstrv_nh
         endif
!........Corner (North-West):
         if (NORTH_INTER) then
            jend_n = jendv_nh+1
         else
            jend_n = jendv_nh
         endif

         do j=jstr_n,jend_n
         do k=1,N    
#  ifdef MASKING
            if (vmask(i,j).ne.0) then
#  endif
               nzuvw_n               = nzuvw_n + 1 
               ijk2lmom_nh(i,j,k,2)  = nzuvw_n
               mijk2lmom_nh(i,j,k,2) = 1 
               l2imom_nh(nzuvw_n)    = i
               l2jmom_nh(nzuvw_n)    = j
               l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
            endif
#  endif
         enddo
         enddo

      endif

!-------------------------------------------------------------------
!     SOUTH MPI Interface
!-------------------------------------------------------------------
      if (SOUTH_INTER) then
         j=jstrv_nh-1

         do i=istr_nh,iend_nh
         do k=1,N 
#  ifdef MASKING
            if (vmask(i,j).ne.0) then
#  endif
               nzuvw_n               = nzuvw_n + 1 
               ijk2lmom_nh(i,j,k,2)  = nzuvw_n
               mijk2lmom_nh(i,j,k,2) = 1 
               l2imom_nh(nzuvw_n)    = i
               l2jmom_nh(nzuvw_n)    = j
               l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
            endif
#  endif
         enddo
         enddo

      endif

# endif /* MPI */

      neqv_nh(1) = nzuvw_n

!-------------------------------------------------------------------
!     WESTERN EDGE (tangential velocity)
!-------------------------------------------------------------------
# ifdef OBC_NBQ_XXX
      if (WESTERN_EDGE) then
         i = istr_nh-1

!........Corner (South-West):
         if (SOUTH_INTER) then
            jstr_n = jstrv_nh-1
         else
            jstr_n = jstrv_nh
         endif
!........Corner (North-West):
         if (NORTH_INTER) then
            jend_n = jendv_nh+1
         else
            jend_n = jendv_nh
         endif

         do j=jstr_n,jend_n
         do k=1,N    
#  ifdef MASKING
            if (vmask(i,j).ne.0) then
#  endif
               nzuvw_n               = nzuvw_n + 1 
               ijk2lmom_nh(i,j,k,2)  = nzuvw_n
               mijk2lmom_nh(i,j,k,2) = 1 
               l2imom_nh(nzuvw_n)    = i
               l2jmom_nh(nzuvw_n)    = j
               l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
            endif
#  endif
         enddo
         enddo

      endif
# endif /* OBC_NBQ_XXX */

!-------------------------------------------------------------------
!     SOUTHERN EDGE
!-------------------------------------------------------------------
# ifdef OBC_NBQ_XXX
      if (SOUTHERN_EDGE) then
        j=jstrv_nh-1

        do i=istr_nh-1,iend_nh+1
        do k=1,N 
#  ifdef MASKING
          if (vmask(i,j).ne.0) then
#  endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,2)  = nzuvw_n
            mijk2lmom_nh(i,j,k,2) = 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
          endif
#  endif
        enddo
        enddo

      endif
# endif /* OBC_SOUTH */

      neqv_nh(2) = nzuvw_n

!-------------------------------------------------------------------
!     Inner domain, bottom layer: (i,j,k=1)
!-------------------------------------------------------------------
      k=1
      do j=jstrv_nh,jendv_nh
      do i=istr_nh,iend_nh
# ifdef MASKING
         if (vmask(i,j).ne.0) then
# endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,2)  = nzuvw_n
            mijk2lmom_nh(i,j,k,2) = 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
# ifdef MASKING
         endif
# endif
      enddo
      enddo

      neqv_nh(3) = nzuvw_n

!-------------------------------------------------------------------
!     Inner domain, inner layers: (i,j, 1<k<N )
!-------------------------------------------------------------------
      do j=jstrv_nh,jendv_nh
      do i=istr_nh,iend_nh
      do k=2,N-1 
#ifdef MASKING
         if (vmask(i,j).ne.0) then
#endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,2)  = nzuvw_n
            mijk2lmom_nh(i,j,k,2) = 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
#ifdef MASKING
         endif
#endif
      enddo
      enddo
      enddo

      neqv_nh(4) = nzuvw_n

!-------------------------------------------------------------------
!     Inner domain, surface layer: (i,j,k=N )
!-------------------------------------------------------------------
      k=N
      do j=jstrv_nh,jendv_nh
      do i=istr_nh,iend_nh
#ifdef MASKING
         if (vmask(i,j).ne.0) then
#endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,2)  = nzuvw_n
            mijk2lmom_nh(i,j,k,2) = 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
#ifdef MASKING
         endif
#endif
      enddo
      enddo

      neqv_nh(5) = nzuvw_n

!-------------------------------------------------------------------
!     EASTERN EDGE
!-------------------------------------------------------------------
# ifdef OBC_NBQ_XXX
      if (EASTERN_EDGE) then
         i = iend_nh+1

!........Corner (South-East):
         if (SOUTH_INTER) then
            jstr_n = jstrv_nh-1
         else
            jstr_n = jstrv_nh
         endif
!........Corner (North-East):
         if (NORTH_INTER) then
            jend_n = jendv_nh+1
         else
            jend_n = jendv_nh
         endif

         do j=jstr_n,jend_n
         do k=1,N    
#  ifdef MASKING
            if (vmask(i,j).ne.0) then
#  endif
               nzuvw_n               = nzuvw_n + 1 
               ijk2lmom_nh(i,j,k,2)  = nzuvw_n
               mijk2lmom_nh(i,j,k,2) = 1 
               l2imom_nh(nzuvw_n)    = i
               l2jmom_nh(nzuvw_n)    = j
               l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
            endif
#  endif
         enddo
         enddo

      endif
# endif /* OBC_NBQ_XXX */

!-------------------------------------------------------------------
!     NORTHERN EDGE
!-------------------------------------------------------------------
# ifdef OBC_NBQ_XXX
      if (NORTHERN_EDGE) then
        j=jendv_nh+1

        do i=istr_nh-1,iend_nh+1
        do k=1,N
#  ifdef MASKING
          if (vmask(i,j).ne.0) then
#  endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,2)  = nzuvw_n
            mijk2lmom_nh(i,j,k,2) = 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
          endif
#  endif
        enddo
        enddo

      endif
# endif /* OBC_NBQ_XXX */

      neqv_nh(6) = nzuvw_n

# ifdef MPI
!-------------------------------------------------------------------
!     EAST MPI Interface
!-------------------------------------------------------------------
      if (EAST_INTER) then
         i = iend_nh+1

!........Corner (South-East):
         if (SOUTH_INTER) then
            jstr_n = jstrv_nh-1
         else
            jstr_n = jstrv_nh
         endif
!........Corner (North-East):
         if (NORTH_INTER) then
            jend_n = jendv_nh+1
         else
            jend_n = jendv_nh
         endif

         do j=jstr_n,jend_n
         do k=1,N    
#  ifdef MASKING
            if (vmask(i,j).ne.0) then
#  endif
               nzuvw_n               = nzuvw_n + 1 
               ijk2lmom_nh(i,j,k,2)  = nzuvw_n
               mijk2lmom_nh(i,j,k,2) = 1 
               l2imom_nh(nzuvw_n)    = i
               l2jmom_nh(nzuvw_n)    = j
               l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
            endif
#  endif
         enddo
         enddo

      endif

!-------------------------------------------------------------------
!     NORTH MPI Interface
!-------------------------------------------------------------------
      if (NORTH_INTER) then
         j=jendv_nh+1

         do i=istr_nh,iend_nh
         do k=1,N

#  ifdef MASKING
            if (vmask(i,j).ne.0) then
#  endif
               nzuvw_n               = nzuvw_n + 1 
               ijk2lmom_nh(i,j,k,2)  = nzuvw_n
               mijk2lmom_nh(i,j,k,2) = 1 
               l2imom_nh(nzuvw_n)    = i
               l2jmom_nh(nzuvw_n)    = j
               l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
            endif
#  endif

         enddo
         enddo

      endif
# endif /* MPI */

      neqv_nh(7)   = nzuvw_n

!
!.....Number of equations in Y-direction:
!
      neqmom_nh(2) = nzuvw_n - neqmom_nh(1)

!*******************************************************************
!*******************************************************************
! Momentum Equation: Z-direction
!******************************************************************* 
!*******************************************************************

# ifdef MPI
!-------------------------------------------------------------------
!     WEST MPI Interface
!-------------------------------------------------------------------
      if (WEST_INTER) then
         i = istr_nh-1

!........Corner (South-West):
         if (SOUTH_INTER) then
            jstr_n = jstr_nh-1
         else
            jstr_n = jstr_nh
         endif
!........Corner (North-West):
         if (NORTH_INTER) then
            jend_n = jend_nh+1
         else
            jend_n = jend_nh
         endif

         do j=jstr_n,jend_n
         do k=0,N    
#  ifdef MASKING
            if (rmask(i,j).ne.0) then
#  endif
             nzuvw_n               = nzuvw_n + 1 
             ijk2lmom_nh(i,j,k,3)  = nzuvw_n
             mijk2lmom_nh(i,j,k,3) = 1 
             l2imom_nh(nzuvw_n)    = i
             l2jmom_nh(nzuvw_n)    = j
             l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
            endif
#  endif
         enddo
         enddo

      endif

!-------------------------------------------------------------------
!     SOUTH MPI Interface
!-------------------------------------------------------------------
      if (SOUTH_INTER) then
         j = jstr_nh-1

         do i=istr_nh,iend_nh
         do k=0,N    
#  ifdef MASKING
            if (rmask(i,j).ne.0) then
#  endif
             nzuvw_n               = nzuvw_n + 1 
             ijk2lmom_nh(i,j,k,3)  = nzuvw_n
             mijk2lmom_nh(i,j,k,3) = 1 
             l2imom_nh(nzuvw_n)    = i
             l2jmom_nh(nzuvw_n)    = j
             l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
            endif
#  endif
         enddo
         enddo

      endif
# endif /* MPI */

      neqw_nh(1)   = nzuvw_n

!-------------------------------------------------------------------
!     Inner domain: (i,j, 0<=k<=N )
!
!       Caution: do not change the order of the loops,
!                k-loop must be insiderid
!-------------------------------------------------------------------
      do j=jstr_nh,jend_nh
      do i=istr_nh,iend_nh
      do k=0,N

# ifdef MASKING
        if (rmask(i,j).ne.0) then
# endif
            nzuvw_n               = nzuvw_n + 1 
            ijk2lmom_nh(i,j,k,3)  = nzuvw_n
            mijk2lmom_nh(i,j,k,3) = 1 
            l2imom_nh(nzuvw_n)    = i
            l2jmom_nh(nzuvw_n)    = j
            l2kmom_nh(nzuvw_n)    = k
# ifdef MASKING
        endif
# endif

      enddo
      enddo
      enddo

      neqw_nh(2)   = nzuvw_n

# ifdef MPI
!-------------------------------------------------------------------
!     East MPI Interface
!-------------------------------------------------------------------
      if (EAST_INTER) then
         i = iend_nh+1

!........Corner (South-East):
         if (SOUTH_INTER) then
            jstr_n = jstr_nh-1
         else
            jstr_n = jstr_nh
         endif
!........Corner (North-East):
         if (NORTH_INTER) then
            jend_n = jend_nh+1
         else
            jend_n = jend_nh
         endif

         do j=jstr_n,jend_n
         do k=0,N    
#  ifdef MASKING
            if (rmask(i,j).ne.0) then
#  endif
             nzuvw_n               = nzuvw_n + 1 
             ijk2lmom_nh(i,j,k,3)  = nzuvw_n
             mijk2lmom_nh(i,j,k,3) = 1 
             l2imom_nh(nzuvw_n)    = i
             l2jmom_nh(nzuvw_n)    = j
             l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
            endif
#  endif
         enddo
         enddo

      endif

!-------------------------------------------------------------------
!     NORTH MPI Interface
!-------------------------------------------------------------------
      if (NORTH_INTER) then
         j = jend_nh+1

         do i=istr_nh,iend_nh
         do k=0,N    
#  ifdef MASKING
            if (rmask(i,j).ne.0) then
#  endif
             nzuvw_n               = nzuvw_n + 1 
             ijk2lmom_nh(i,j,k,3)  = nzuvw_n
             mijk2lmom_nh(i,j,k,3) = 1 
             l2imom_nh(nzuvw_n)    = i
             l2jmom_nh(nzuvw_n)    = j
             l2kmom_nh(nzuvw_n)    = k
#  ifdef MASKING
            endif
#  endif
         enddo
         enddo

      endif
# endif /* MPI */

      neqw_nh(3)   = nzuvw_n

      neqmom_nh(3) = nzuvw_n -(neqmom_nh(1)+neqmom_nh(2)) 
      neqmom_nh(0) = nzuvw_n

      return

      end subroutine numuvw_nh
 
#else
        subroutine numuvw_nh_empty
        return
        end 
#endif
