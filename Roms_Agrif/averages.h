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
      common /avg_zeta/zeta_avg /avg_ubar/ubar_avg
     &                          /avg_vbar/vbar_avg
# ifdef LMD_SKPP
      real hbl_avg(GLOBAL_2D_ARRAY)
      common /avg_hbl/hbl_avg
# endif
# ifdef LMD_BKPP
      real hbbl_avg(GLOBAL_2D_ARRAY)
      common /avg_hbbl/hbbl_avg
# endif
      real bostr_avg(GLOBAL_2D_ARRAY)
      common /avg_bostr/bostr_avg
# ifdef BIOLOGY
      real hel_avg(GLOBAL_2D_ARRAY)
      common /avg_hel/hel_avg
# endif
# ifdef SOLVE3D
      real u_avg(GLOBAL_2D_ARRAY,N)
      real v_avg(GLOBAL_2D_ARRAY,N)
      real t_avg(GLOBAL_2D_ARRAY,N,NT)
#  ifdef BIOLOGY
#   ifdef BIO_NPZD
      real theta_avg(GLOBAL_2D_ARRAY,N)
#    ifdef OXYGEN
      real u10_avg(GLOBAL_2D_ARRAY)
      real Kv_O2_avg(GLOBAL_2D_ARRAY)
      real O2satu_avg(GLOBAL_2D_ARRAY)
#    endif /* OXYGEN */
#   elif defined BIO_N2P2Z2D2 && defined AVG_CHL_C
      real theta1_avg(GLOBAL_2D_ARRAY,N)
     &   , theta2_avg(GLOBAL_2D_ARRAY,N)
#   endif
#  endif
      real rho_avg(GLOBAL_2D_ARRAY,N)
      real omega_avg(GLOBAL_2D_ARRAY,0:N)
      real w_avg(GLOBAL_2D_ARRAY,N)
      common /avg_u/u_avg /avg_v/v_avg /avg_t/t_avg
     &       /avg_rho/rho_avg /avg_omega/omega_avg
     &       /avg_w/w_avg
#  ifdef BIOLOGY
#   ifdef BIO_NPZD
     &       /avg_theta/theta_avg
#    ifdef OXYGEN
     &       /avg_O2_GE/ u10_avg, Kv_O2_avg, O2satu_avg
#    endif
#   elif defined BIO_N2P2Z2D2 && defined AVG_CHL_C
     &       /avg_theta1/theta1_avg /avg_theta2/theta2_avg
#   endif
#  endif /* BIOLOGY */
#  if (defined UV_VIS2 || defined TS_DIF2) && defined SMAGORINSKY
      real visc3d_avg(GLOBAL_2D_ARRAY,N)
      common /avg_visc3d/visc3d_avg
#  endif
#  ifdef AVERAGES_K
      real Akv_avg(GLOBAL_2D_ARRAY,0:N)
      real Akt_avg(GLOBAL_2D_ARRAY,0:N,2)
      common /avg_Akv/Akv_avg /avg_Akt/Akt_avg
#  endif
# endif
#endif /* AVERAGES */
