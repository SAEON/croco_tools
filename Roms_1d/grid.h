! $Id$
!
! Grid parameters
!
! z_r        Actual depths (m) at vertical RHO-points.                                **
! z_w        Actual depths (m) at vertical W-points.                                  **
! Hz         Thicknesses (m) of vertical RHO-points.
      real z_r(N),z_w(0:N),Hz(N), hmax, theta_s
      common /grid/ z_r,z_w,Hz,hmax,theta_s
