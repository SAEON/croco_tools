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
/* This is include file "coupling.h":
  ----------------------------------------------------
  Variables responsible for communication between two-
  and three-dimensional parts of the model.
*/
#ifdef SOLVE3D
# ifdef VAR_RHO_2D
      real rhoA(GLOBAL_2D_ARRAY)
      real rhoS(GLOBAL_2D_ARRAY)
      common /coup_rhoA/rhoA           /coup_rhoS/rhoS
# endif
      real rufrc(GLOBAL_2D_ARRAY)
      real rvfrc(GLOBAL_2D_ARRAY)
      real rufrc_bak(GLOBAL_2D_ARRAY,2)
      real rvfrc_bak(GLOBAL_2D_ARRAY,2)
      common /coup_rufrc/rufrc         /coup_rvfrc/rvfrc
     &       /coup_rufrc_bak/rufrc_bak /coup_rvfrc_bak/rvfrc_bak

      real Zt_avg1(GLOBAL_2D_ARRAY)
      real DU_avg1(GLOBAL_2D_ARRAY,5)
      real DV_avg1(GLOBAL_2D_ARRAY,5)
      real DU_avg2(GLOBAL_2D_ARRAY)
      real DV_avg2(GLOBAL_2D_ARRAY)
      common /ocean_Zt_avg1/Zt_avg1
     &     /coup_DU_avg1/DU_avg1 /coup_DV_avg1/DV_avg1
     &     /coup_DU_avg2/DU_avg2 /coup_DV_avg2/DV_avg2
#endif
