!
! $Id$
!
#include "cppdefs.h"
                                            ! Setting up curvilinear
      subroutine setup_grid1 (tile)         ! grid: Compute various
      implicit none                         ! combinations of metric
      integer tile, trd                     ! terms.
#include "param.h"
C$    integer omp_get_thread_num
#include "compute_tile_bounds.h"
      call setup_grid1_tile (Istr,Iend,Jstr,Jend)
      return
      end

      subroutine setup_grid1_tile (Istr,Iend,Jstr,Jend)
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j
#include "param.h"
#include "scalars.h"
#include "grid.h"
!
#include "compute_extended_bounds.h"
!
!  Set f/mn,at horizontal RHO-points.
!

      do j=JstrR,JendR                             ! This array
        do i=IstrR,IendR                           ! is NOT to be 
          fomn(i,j)=f(i,j)/(pm(i,j)*pn(i,j))       ! communicated 
        enddo                                      ! in MPI code; 
      enddo                                        ! others are...

#ifdef EW_PERIODIC
# define IR_RANGE IstrR,IendR
# define IU_RANGE  Istr,IendR
#else
# define IR_RANGE IstrR,IendR
# define IU_RANGE  Istr,IendR
# ifdef MPI
                                          ! Ghost points along
      if (WEST_INTER) IstrR=Istr          ! computational boundary 
      if (EAST_INTER) IendR=Iend          ! are filled during
                                          ! subsequent communication;
                                          ! see also below...
# endif
#endif
#ifdef NS_PERIODIC
# define JR_RANGE Jstr,Jend
# define JV_RANGE Jstr,Jend
#else
# define JR_RANGE JstrR,JendR
# define JV_RANGE  Jstr,JendR
# ifdef MPI
      if (SOUTH_INTER) JstrR=Jstr         ! same as above.  
      if (NORTH_INTER) JendR=Jend         !
# endif
#endif
!
!  Compute 1/n, 1/m, n/m and m/n all at horizontal RHO-points.
!
      do j=JR_RANGE
        do i=IR_RANGE
          om_r(i,j)=1./pm(i,j)
          on_r(i,j)=1./pn(i,j)
          pnom_r(i,j)=pn(i,j)/pm(i,j)
          pmon_r(i,j)=pm(i,j)/pn(i,j)
        enddo
      enddo

#if (defined CURVGRID && defined UV_ADV)
!
!  Compute d(1/n)/d(xi) and d(1/m)/d(eta) tems, both at RHO-points.
!
      do j=Jstr,Jend
        do i=Istr,Iend
          dndx(i,j)=0.5/pn(i+1,j)-0.5/pn(i-1,j)
          dmde(i,j)=0.5/pm(i,j+1)-0.5/pm(i,j-1)
        enddo
      enddo

#endif
!
!  Compute m/n at horizontal U-points.
!
      do j=JR_RANGE
        do i=IU_RANGE
           pmon_u(i,j)=(pm(i,j)+pm(i-1,j))
     &                 /(pn(i,j)+pn(i-1,j))
           om_u(i,j)=2./(pm(i,j)+pm(i-1,j))
           on_u(i,j)=2./(pn(i,j)+pn(i-1,j))
           pn_u(i,j)=0.5*(pn(i,j)+pn(i-1,j))
           pm_u(i,j)=0.5*(pm(i,j)+pm(i-1,j))
#ifdef MASKING
           umask(i,j)=rmask(i,j)*rmask(i-1,j)
#endif
        enddo
      enddo
!
!  Compute n/m at horizontal V-points.
!
      do j=JV_RANGE
        do i=IR_RANGE
          pnom_v(i,j)=(pn(i,j)+pn(i,j-1))
     &                /(pm(i,j)+pm(i,j-1))
          om_v(i,j)=2./(pm(i,j)+pm(i,j-1))
          on_v(i,j)=2./(pn(i,j)+pn(i,j-1))
          pm_v(i,j)=0.5*(pm(i,j)+pm(i,j-1))
          pn_v(i,j)=0.5*(pn(i,j)+pn(i,j-1))
#ifdef MASKING
          vmask(i,j)=rmask(i,j)*rmask(i,j-1)
#endif
        enddo
      enddo
!
! Compute n/m and m/n at horizontal PSI-points.
! Set mask according to slipperness parameter gamma.
!
      do j=JV_RANGE
        do i=IU_RANGE
          pnom_p(i,j)=(pn(i,j)+pn(i,j-1)+pn(i-1,j)+pn(i-1,j-1))
     &               /(pm(i,j)+pm(i,j-1)+pm(i-1,j)+pm(i-1,j-1))
          pmon_p(i,j)=(pm(i,j)+pm(i,j-1)+pm(i-1,j)+pm(i-1,j-1))
     &               /(pn(i,j)+pn(i,j-1)+pn(i-1,j)+pn(i-1,j-1))
          om_p(i,j)=4./(pm(i-1,j-1)+pm(i-1,j)+pm(i,j-1)+pm(i,j))
          on_p(i,j)=4./(pn(i-1,j-1)+pn(i-1,j)+pn(i,j-1)+pn(i,j))
#ifdef MASKING
          pmask(i,j)=rmask(i,j)*rmask(i-1,j)*rmask(i,j-1)
     &                                      *rmask(i-1,j-1)
          if (gamma2.lt.0.) pmask(i,j)=2.-pmask(i,j)
#endif
        enddo
      enddo

#undef IR_RANGE
#undef IU_RANGE
#undef JR_RANGE
#undef JV_RANGE

#if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,   om_r)
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,   on_r)
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend, pnom_r)
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend, pmon_r)
# if defined CURVGRID && defined UV_ADV
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,   dndx)
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,   dmde)
# endif
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend, pmon_u)
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,   om_u)
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,   on_u)
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,   pn_u)
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,   pm_u)

      call exchange_v2d_tile (Istr,Iend,Jstr,Jend, pnom_v)
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,   om_v)
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,   on_v)
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,   pm_v)
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,   pn_v)

      call exchange_p2d_tile (Istr,Iend,Jstr,Jend, pnom_p)
      call exchange_p2d_tile (Istr,Iend,Jstr,Jend, pmon_p)
      call exchange_p2d_tile (Istr,Iend,Jstr,Jend,   om_p)
      call exchange_p2d_tile (Istr,Iend,Jstr,Jend,   on_p)
# ifdef MASKING
#  ifdef THREE_GHOST_POINTS_TS 
      call exchange_r2d_3pts_tile (Istr,Iend,Jstr,Jend, rmask)
#  else
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,  rmask)
#  endif
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,  umask)
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,  vmask)
      call exchange_p2d_tile (Istr,Iend,Jstr,Jend,  pmask)
# endif
#endif
      return
      end

