!
! $Id: exchange_2d_tile.h,v 1.2 2003/12/17 13:56:01 pmarches Exp $
!
      subroutine exchange_2d_tile (Istr,Iend,Jstr,Jend, A)
!
! Set periodic boundary conditions (if any) for a two-dimensional
! field A of ZETA-, U-, V- or PSI-type. This file is designed to
! generate four different subroutines, by redefining (via CPP) the
! name of the subroutine exchange_2d_tile above and the starting
! indices ISTART = [Istr for U-,PSI-type; IstrR for V-,ZETA-type] 
! and JSTART = [Jstr for V-,PSI-type; JstrR for U-,ZETA-type]
! below. See also mounting file exchange.F  
!
      implicit none
#include "param.h"
#include "scalars.h"
      real A(GLOBAL_2D_ARRAY)
      integer Istr,Iend,Jstr,Jend, i,j
!
#include "compute_auxiliary_bounds.h"
!

#ifdef EW_PERIODIC
# ifdef NS_PERIODIC
#  define J_RANGE Jstr,Jend
# else
#  define J_RANGE JSTART,JendR
# endif
# ifdef MPI
      if (NP_XI.eq.1) then
# endif
        if (WESTERN_EDGE) then
          do j=J_RANGE
            A(Lm+1,j)=A(1,j)
            A(Lm+2,j)=A(2,j)
          enddo
        endif
        if (EASTERN_EDGE) then
          do j=J_RANGE
            A(-1,j)=A(Lm-1,j)
            A( 0,j)=A(Lm  ,j)
          enddo
        endif
# ifdef MPI
      endif
# endif
# undef J_RANGE
#endif

#ifdef NS_PERIODIC
# ifdef EW_PERIODIC
#  define I_RANGE Istr,Iend
# else
#  define I_RANGE ISTART,IendR
# endif
# ifdef MPI
      if (NP_ETA.eq.1) then
# endif
        if (SOUTHERN_EDGE) then
          do i=I_RANGE
            A(i,Mm+1)=A(i,1)
            A(i,Mm+2)=A(i,2)
          enddo
        endif
        if (NORTHERN_EDGE) then
          do i=I_RANGE
            A(i,-1)=A(i,Mm-1)
            A(i, 0)=A(i,Mm  )
          enddo
        endif
# ifdef MPI
      endif
# endif
# undef I_RANGE
#endif

#if defined EW_PERIODIC && defined NS_PERIODIC
# ifdef MPI
      if (NP_XI.eq.1 .and. NP_ETA.eq.1) then
# endif
        if (WESTERN_EDGE .and. SOUTHERN_EDGE) then
          A(Lm+1,Mm+1)=A(1,1)
          A(Lm+1,Mm+2)=A(1,2)
          A(Lm+2,Mm+1)=A(2,1)
          A(Lm+2,Mm+2)=A(2,2)
        endif
        if (EASTERN_EDGE .and. SOUTHERN_EDGE) then
          A(-1,Mm+1)=A(Lm-1,1)
          A( 0,Mm+1)=A(Lm  ,1)
          A(-1,Mm+2)=A(Lm-1,2)
          A( 0,Mm+2)=A(Lm  ,2)
        endif
        if (WESTERN_EDGE .and. NORTHERN_EDGE) then
          A(Lm+1,-1)=A(1,Mm-1)
          A(Lm+1, 0)=A(1,Mm  )
          A(Lm+2,-1)=A(2,Mm-1)
          A(Lm+2, 0)=A(2,Mm  )
        endif
        if (EASTERN_EDGE .and. NORTHERN_EDGE) then
          A(-1,-1)=A(Lm-1,Mm-1)
          A( 0,-1)=A(Lm  ,Mm-1)
          A(-1, 0)=A(Lm-1,Mm  )
          A( 0, 0)=A(Lm  ,Mm  )
        endif
# ifdef MPI
      endif
# endif
#endif
#ifdef MPI
      call MessPass2D_tile (Istr,Iend,Jstr,Jend,  A)
#endif
      return
      end


