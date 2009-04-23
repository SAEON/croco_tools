!
! $Id: ocean3d.h,v 1.3 2004/03/26 15:26:07 pmarches Exp $
!
/* This is include file "ocean3.h". 
  --------------------------------------------
*/
#ifdef SOLVE3D
      real u(GLOBAL_2D_ARRAY,N,3)
      real v(GLOBAL_2D_ARRAY,N,3)
      real t(GLOBAL_2D_ARRAY,N,3,NT)
      common /ocean_u/u /ocean_v/v /ocean_t/t

      real Hz(GLOBAL_2D_ARRAY,N)
      real Hz_bak(GLOBAL_2D_ARRAY,N)
      real z_r(GLOBAL_2D_ARRAY,N)
      real z_w(GLOBAL_2D_ARRAY,0:N)
      real Huon(GLOBAL_2D_ARRAY,N)
      real Hvom(GLOBAL_2D_ARRAY,N)
      real W(GLOBAL_2D_ARRAY,0:N)
      common /grid_Hz/Hz    /grid_zr/z_r  /grid_W/W
     &  /grid_Hz_bak/Hz_bak /grid_zw/z_w  /grid_Huon/Huon
     &                                    /grid_Hvom/Hvom

# if defined UV_VIS4 && defined MIX_GP_UV
      real z_u(GLOBAL_2D_ARRAY,N)
      real z_v(GLOBAL_2D_ARRAY,N)
      real dz_u(GLOBAL_2D_ARRAY,N)
      real dz_v(GLOBAL_2D_ARRAY,N)
      common /grid_zu/z_u /grid_zv/z_v
     &       /grid_dz_u/dz_u /grid_dz_v/dz_v
# endif

      real rho1(GLOBAL_2D_ARRAY,N)
      real rho(GLOBAL_2D_ARRAY,N)
      common /ocean_rho1/rho1 /ocean_rho/rho
# ifdef BIOLOGY
#  ifdef BIO_NChlPZD
      real theta(GLOBAL_2D_ARRAY,N)
      common /ocean_theta/theta
#  elif defined BIO_N2P2Z2D2 && defined AVG_CHL_C
      real theta1(GLOBAL_2D_ARRAY,N)
     &   , theta2(GLOBAL_2D_ARRAY,N)
      common /ocean_theta1/theta1 /ocean_theta2/theta2
#  endif
# endif
# if defined NONLIN_EOS && defined SPLIT_EOS
       real qp1(GLOBAL_2D_ARRAY,N)
      common /ocean_qp1/qp1
# endif
#endif  /* SOLVE3D */

