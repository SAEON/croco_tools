! $Id: trcctl.pisces.h 1460 2014-02-03 15:02:02Z gcambon $
!
!=========================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France.
! The two other branches, from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al), are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
!
! ROMS_AGRIF website : http://www.romsagrif.org
!=========================================================================
!
#if defined key_trc_pisces
      IF(lwp) THEN
          WRITE(numout,*) ' use PISCES biological model '
          WRITE(numout,*) ' '
      ENDIF
#endif
