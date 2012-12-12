! $Id$
!
!======================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
! 
! ROMS_AGRIF website : http://roms.mpl.ird.fr
!======================================================================
!
#include "cppdefs.h"
#if defined SSH_TIDES || defined UV_TIDES
      subroutine clm_tides (tile)
!
!================================================== Robert Hetland ===
!  Copyright (c) 2000 Rutgers/UCLA                                   !
!================================================ Hernan G. Arango ===
!                                                                    !
!  This routine adds tidal elevation (m) and tidal currents (m/s) to !
!  sea surface height and 2D momentum climatologies, respectively.   !
!                                                                    !
!=====================================================================
!
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
!
      trd=omp_get_thread_num()
      call clm_tides_tile (Istr,Iend,Jstr,Jend,
     &                     A2d(1,1,trd),A2d(1,2,trd),A2d(1,3,trd),
     &                     A2d(1,4,trd),A2d(1,5,trd),A2d(1,6,trd),
     &                                               A2d(1,7,trd))
      return
      end
!
!*********************************************************************
      subroutine clm_tides_tile (Istr,Iend,Jstr,Jend,Cangle,Sangle,
     &                           Cphase,Sphase,Etide,Utide,Vtide)
!*********************************************************************
!
      implicit none
# include "param.h"
# include "climat.h"
# include "grid.h"
# include "scalars.h"
# include "tides.h"
# include "boundary.h"
!
      integer
     &     Iend, Istr, Jend, Jstr, i, itide, j
      real
     &     angle, phase, omega, ramp
      real
     &        Cangle(PRIVATE_2D_SCRATCH_ARRAY),
     &        Cphase(PRIVATE_2D_SCRATCH_ARRAY),
     &        Sangle(PRIVATE_2D_SCRATCH_ARRAY),
     &        Sphase(PRIVATE_2D_SCRATCH_ARRAY),
     &        Etide(PRIVATE_2D_SCRATCH_ARRAY),
     &        Utide(PRIVATE_2D_SCRATCH_ARRAY),
     &        Vtide(PRIVATE_2D_SCRATCH_ARRAY)
!
# include "compute_auxiliary_bounds.h"
# ifdef MPI
#  define IRANGE Istr-1,Iend+1
#  define JRANGE Jstr-1,Jend+1
# else
#  define IRANGE IstrR,IendR
#  define JRANGE JstrR,JendR
# endif
!
      angle=0.
      phase=0.
      ramp=1.
# ifdef TIDERAMP
      ramp=TANH(dt*sec2day*float(iic-ntstart)) 
# endif
!
# if defined SSH_TIDES
!---------------------------------------------------------------------
!  Add tidal elevation (m) to sea surface height climatology.
!---------------------------------------------------------------------
!
      do j=JRANGE
        do i=IRANGE
          Etide(i,j)=0.
        enddo
      enddo
      do itide=1,Ntides
        if (Tperiod(itide).gt.0.) then
          omega=2.*pi*time/Tperiod(itide)
          do j=JRANGE
            do i=IRANGE
              Etide(i,j)=Etide(i,j)+ramp*
     &                   SSH_Tamp(i,j,itide)*
     &                   COS(omega-SSH_Tphase(i,j,itide))
#  ifdef MASKING
              Etide(i,j)=Etide(i,j)*rmask(i,j)
#  endif
            enddo
          enddo
        endif
      enddo
!
!  Add sub-tidal forcing and adjust climatology to include tides.
!
#  if defined ZCLIMATOLOGY
      do j=JRANGE
        do i=IRANGE
          ssh(i,j)=ssh(i,j)+Etide(i,j)
        enddo
      enddo
#  endif /* ZCLIMATOLOGY */
!
!  If appropriate, load tidal forcing into boundary arrays.
!  Note: Interpolation is done at normal U point in preparation 
!        for FLATHER conditions
!
#  ifdef Z_FRC_BRY
#   if defined OBC_WEST
      if (WESTERN_EDGE) then
        do j=JstrR,JendR
          zetabry_west(j)=zetabry_west(j)+
     &                 0.5*(Etide(Istr-1,j)+Etide(Istr,j))
        enddo
      endif
#   endif
#   if defined OBC_EAST
      if (EASTERN_EDGE) then
        do j=JstrR,JendR
          zetabry_east(j)=zetabry_east(j)+
     &                 0.5*(Etide(Iend,j)+Etide(Iend+1,j))
        enddo
      endif
#   endif
#  if defined OBC_SOUTH
      if (SOUTHERN_EDGE) then
        do i=IstrR,IendR
          zetabry_south(i)=zetabry_south(i)+
     &                  0.5*(Etide(i,Jstr-1)+Etide(i,Jstr))
        enddo
      endif
#   endif
#  if defined OBC_NORTH
      if (NORTHERN_EDGE) then
        do i=IstrR,IendR
          zetabry_north(i)=zetabry_north(i)+
     &                  0.5*(Etide(i,Jend)+Etide(i,Jend+1))
        enddo
      endif
#   endif
#  endif /* Z_FRC_BRY */
# endif /* SSH_TIDES */
!
# if defined UV_TIDES
!
!---------------------------------------------------------------------
!  Add tidal currents (m/s) to 2D momentum climatologies.
!---------------------------------------------------------------------
!
      do j=JstrR,JendR
        do i=Istr,IendR
          Utide(i,j)=0.
        enddo
      enddo
      do j=Jstr,JendR
        do i=IstrR,IendR
          Vtide(i,j)=0.
        enddo
      enddo
      do itide=1,Ntides
        if (Tperiod(itide).gt.0.) then
          omega=2.*pi*time/Tperiod(itide)
          do j=Jstr-1,JendR
            do i=Istr-1,IendR
              angle=UV_Tangle(i,j,itide)-angler(i,j)
              phase=omega-UV_Tphase(i,j,itide)
              Cangle(i,j)=COS(angle)
              Cphase(i,j)=COS(phase)
              Sangle(i,j)=SIN(angle)
              Sphase(i,j)=SIN(phase)
            enddo
          enddo
          do j=JstrR,JendR
            do i=Istr,IendR
              Utide(i,j)=Utide(i,j)+ramp*
     &                        0.125*((UV_Tmajor(i-1,j,itide)+
     &                                UV_Tmajor(i  ,j,itide))*
     &                               (Cangle(i-1,j)+Cangle(i,j))*
     &                               (Cphase(i-1,j)+Cphase(i,j))-
     &                               (UV_Tminor(i-1,j,itide)+
     &                                UV_Tminor(i  ,j,itide))*
     &                               (Sangle(i-1,j)+Sangle(i,j))*
     &                               (Sphase(i-1,j)+Sphase(i,j)))
#  ifdef MASKING
             Utide(i,j)=Utide(i,j)*umask(i,j)
#  endif
            enddo
          enddo
          do j=Jstr,JendR
            do i=IstrR,IendR
              Vtide(i,j)=Vtide(i,j)+ramp*
     &                        0.125*((UV_Tmajor(i,j-1,itide)+
     &                                UV_Tmajor(i,j  ,itide))*
     &                               (Sangle(i,j-1)+Sangle(i,j))*
     &                               (Cphase(i,j-1)+Cphase(i,j))+
     &                               (UV_Tminor(i,j-1,itide)+
     &                                UV_Tminor(i,j  ,itide))*
     &                               (Cangle(i,j-1)+Cangle(i,j))*
     &                               (Sphase(i,j-1)+Sphase(i,j)))
#  ifdef MASKING
              Vtide(i,j)=Vtide(i,j)*vmask(i,j)
#  endif
            enddo
          enddo
        endif
      enddo
#  if defined M2CLIMATOLOGY
!
!  Add sub-tidal forcing and adjust climatology to include tides.
!
      do j=JstrR,JendR
        do i=Istr,IendR
          ubclm(i,j)=ubclm(i,j)+Utide(i,j)
        enddo
      enddo
      do j=Jstr,JendR
        do i=IstrR,IendR
          vbclm(i,j)=vbclm(i,j)+Vtide(i,j)
        enddo
      enddo
#  endif
!
!  If appropriate, load tidal forcing into boundary arrays.
!
#  ifdef M2_FRC_BRY
#   ifdef OBC_WEST
      if (WESTERN_EDGE) THEN
        do j=JstrR,JendR
          ubarbry_west(j)=ubarbry_west(j)+Utide(Istr,j)
        enddo
        do j=Jstr,JendR
          vbarbry_west(j)=vbarbry_west(j)+Vtide(Istr-1,j)
        enddo
      endif
#   endif
#   ifdef OBC_EAST
      if (EASTERN_EDGE) THEN
        do j=JstrR,JendR
          ubarbry_east(j)=ubarbry_east(j)+Utide(Iend+1,j)
        enddo
        do j=Jstr,JendR
          vbarbry_east(j)=vbarbry_east(j)+Vtide(Iend+1,j)
        enddo
      endif
#   endif
#   ifdef OBC_SOUTH
      if (SOUTHERN_EDGE) THEN
        do i=Istr,IendR
          ubarbry_south(i)=ubarbry_south(i)+Utide(i,Jstr-1)
        enddo
        do i=IstrR,IendR
          vbarbry_south(i)=vbarbry_south(i)+Vtide(i,Jstr)
        enddo
      endif
#   endif
#   ifdef OBC_NORTH
      if (NORTHERN_EDGE) THEN
        do i=Istr,IendR
          ubarbry_north(i)=ubarbry_north(i)+Utide(i,Jend+1)
        enddo
        do i=IstrR,IendR
          vbarbry_north(i)=vbarbry_north(i)+Vtide(i,Jend+1)
        enddo
      endif
#   endif
# endif /* M2_FRC_BRY */
# endif /* UV_TIDES */
#else
      subroutine clm_tides_empty
#endif /* SSH_TIDES || UV_TIDES */
      return
      end
