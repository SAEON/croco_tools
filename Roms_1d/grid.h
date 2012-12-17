! $Id$
!======================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
! 
! ROMS_AGRIF website : http://www.romsagrif.org
!======================================================================
!
! Grid parameters
!
! z_r        Actual depths (m) at vertical RHO-points.                                **
! z_w        Actual depths (m) at vertical W-points.                                  **
! Hz         Thicknesses (m) of vertical RHO-points.
      real z_r(N),z_w(0:N),Hz(N), hmax, theta_s
      common /grid/ z_r,z_w,Hz,hmax,theta_s
