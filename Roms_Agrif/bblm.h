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
!  This is include file "bblm.h"
!--------------------------------------------------------------------
!  BOTTOM BOUNDARY LAYER PARAMETERISATION
!--------------------------------------------------------------------
! Ab      |   Wave bottom excursion amplitude (m).
! Awave   |   Wind induced wave amplitude (m) at RHO-points.
! Cr      |   Nondimentional function that determines the importance
!         |      of currents and wind induced waves on bottom stress
!         |      at RHO-points.
! Dwave   |   Wind induced wave direction (radians) at RHO-points.
! Pwave   |   Wind induced wave period (s) at RHO-points.
! Sdens   |   Sediment grain density (kg/m^3) at RHO-points.
! Ssize   |   Sediment grain diameter size (m) at RHO-points.
! Ub      |   Wave maximum bottom horizontal velocity (m/s).
! UstarC  |   Time-averaged near-bottom friction current magnitude
!         |          (m/s) at RHO-points.
!--------------------------------------------------------------------
      real Ab(GLOBAL_2D_ARRAY)
      common /fbblm_Ab/Ab

      real Awave(GLOBAL_2D_ARRAY)
      common /fbblm_Awave/Awave

      real Cr(GLOBAL_2D_ARRAY)
      common /fbblm_Cr/Cr

      real Dwave(GLOBAL_2D_ARRAY)
      common /fbblm_Dwave/Dwave

      real Pwave(GLOBAL_2D_ARRAY)
      common /fbblm_Pwave/Pwave

      real Sdens(GLOBAL_2D_ARRAY)
      common /fbblm_Sdens/Sdens

      real Ssize(GLOBAL_2D_ARRAY)
      common /fbblm_Ssize/Ssize

      real Ub(GLOBAL_2D_ARRAY)
      common /fbblm_Ub/Ub

      real UstarC(GLOBAL_2D_ARRAY)
      common /fbblm_UstarC/UstarC


