!
! $Id: averages.h,v 1.7 2005/09/23 15:56:30 pmarches Exp $
!
/* This is include file "averages.h": time-averaged fields
 for output purposes:
*/
#ifdef AVERAGES
      real zeta_avg(GLOBAL_2D_ARRAY)
      real ubar_avg(GLOBAL_2D_ARRAY)
      real vbar_avg(GLOBAL_2D_ARRAY)
      common /avg_zeta/zeta_avg 
     &       /avg_ubar/ubar_avg
     &       /avg_vbar/vbar_avg
      real bostr_avg(GLOBAL_2D_ARRAY)
      common /avg_bostr/bostr_avg
      real wstr_avg(GLOBAL_2D_ARRAY)
      common /avg_wstr/wstr_avg
# ifdef SOLVE3D
      real u_avg(GLOBAL_2D_ARRAY,N)
      real v_avg(GLOBAL_2D_ARRAY,N)
      real t_avg(GLOBAL_2D_ARRAY,N,NT)
      real rho_avg(GLOBAL_2D_ARRAY,N)
      real omega_avg(GLOBAL_2D_ARRAY,0:N)
      real w_avg(GLOBAL_2D_ARRAY,N)
      common /avg_u/u_avg /avg_v/v_avg /avg_t/t_avg
     &       /avg_rho/rho_avg /avg_omega/omega_avg
     &       /avg_w/w_avg
#  ifdef LMD_SKPP
      real hbl_avg(GLOBAL_2D_ARRAY)
      common /avg_hbl/hbl_avg
#  endif
#  ifdef LMD_BKPP
      real hbbl_avg(GLOBAL_2D_ARRAY)
      common /avg_hbbl/hbbl_avg
#  endif
#  ifdef BULK_FLUX
      real shflx_rsw_avg(GLOBAL_2D_ARRAY)
      real shflx_rlw_avg(GLOBAL_2D_ARRAY)
      real shflx_lat_avg(GLOBAL_2D_ARRAY)
      real shflx_sen_avg(GLOBAL_2D_ARRAY)
      common /avg_shflx_rsw/shflx_rsw_avg
      common /avg_shflx_rlw/shflx_rlw_avg
      common /avg_shflx_lat/shflx_lat_avg
      common /avg_shflx_sen/shflx_sen_avg
#  endif
#  ifdef BIOLOGY
      real hel_avg(GLOBAL_2D_ARRAY)
      common /avg_hel/hel_avg
#   ifdef BIO_NPZD
      real theta_avg(GLOBAL_2D_ARRAY,N)
      common /avg_theta/theta_avg
#    ifdef OXYGEN
      real u10_avg(GLOBAL_2D_ARRAY)
      real Kv_O2_avg(GLOBAL_2D_ARRAY)
      real O2satu_avg(GLOBAL_2D_ARRAY)
      common /avg_O2_GE/ u10_avg, Kv_O2_avg, O2satu_avg
#    endif
#   elif defined BIO_N2P2Z2D2 && defined AVG_CHL_C
      real theta1_avg(GLOBAL_2D_ARRAY,N)
     &   , theta2_avg(GLOBAL_2D_ARRAY,N)
      common /avg_theta1/theta1_avg /avg_theta2/theta2_avg
#   endif
#  endif /* BIOLOGY */
#  ifdef VIS_COEF_3D
      real visc3d_avg(GLOBAL_2D_ARRAY,N)
      common /avg_visc3d/visc3d_avg
#  endif
#  ifdef DIF_COEF_3D
      real diff3d_avg(GLOBAL_2D_ARRAY,N)
      common /avg_diff3d/diff3d_avg
#  endif
#  ifdef AVERAGES_K
      real Akv_avg(GLOBAL_2D_ARRAY,0:N)
      real Akt_avg(GLOBAL_2D_ARRAY,0:N,2)
      common /avg_Akv/Akv_avg /avg_Akt/Akt_avg
#  endif
# endif
#endif /* AVERAGES */
