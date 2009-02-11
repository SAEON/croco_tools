!
! $Id: mixing.h,v 1.6 2005/10/10 13:40:18 pmarches Exp $
!
! This is include file "mixing.h"
!  ==== == ======= ==== ==========
!
# if defined UV_VIS2 || defined SPONGE_VIS2
      real visc2_r(GLOBAL_2D_ARRAY)
      real visc2_p(GLOBAL_2D_ARRAY)
      real visc2_sponge_r(GLOBAL_2D_ARRAY)
      real visc2_sponge_p(GLOBAL_2D_ARRAY)
      common /mixing_visc2_r/visc2_r /mixing_visc2_p/visc2_p
      common /mixing_visc2_sponge_r/visc2_sponge_r 
     &       /mixing_visc2_sponge_p/visc2_sponge_p
# endif
#if defined UV_VIS4 
# if !defined SPONGE_VIS2
      real visc2_sponge_r(GLOBAL_2D_ARRAY)
      real visc2_sponge_p(GLOBAL_2D_ARRAY)
      common /mixing_visc2_sponge_r/visc2_sponge_r 
     &       /mixing_visc2_sponge_p/visc2_sponge_p
#endif
      real visc4_sponge_r(GLOBAL_2D_ARRAY)
      real visc4_sponge_p(GLOBAL_2D_ARRAY)
      real visc4_r(GLOBAL_2D_ARRAY)
      real visc4_p(GLOBAL_2D_ARRAY)
      common /mixing_visc4_sponge_r/visc4_sponge_r
      common /mixing_visc4_sponge_p/visc4_sponge_p
      common /mixing_visc4_r/visc4_r /mixing_visc4_p/visc4_p
#endif
# if defined TS_DIF2 || defined SPONGE_DIF2
      real diff2_sponge(GLOBAL_2D_ARRAY)
      real diff2(GLOBAL_2D_ARRAY,NT)
      common /mixing_diff2_sponge/diff2_sponge
      common /mixing_diff2/diff2
# endif
#if defined TS_DIF4 
# if !defined SPONGE_DIF2
      real diff2_sponge(GLOBAL_2D_ARRAY)
      common /mixing_diff2_sponge/diff2_sponge
# endif
      real diff4_sponge(GLOBAL_2D_ARRAY)
      real diff4(GLOBAL_2D_ARRAY,NT)
      common /mixing_diff4_sponge/diff4_sponge
      common /mixing_diff4/diff4
#endif
#ifdef VIS_COEF_3D
      real visc3d_r(GLOBAL_2D_ARRAY,N)
      common /mixing_visc3d_r/visc3d_r
# ifdef UV_SPLIT_UP3
      real viscU_r(GLOBAL_2D_ARRAY,N)
      real viscV_r(GLOBAL_2D_ARRAY,N)
      real viscU_p(GLOBAL_2D_ARRAY,N)
      real viscV_p(GLOBAL_2D_ARRAY,N)
      common /mixing_viscU_r/viscU_r
     &       /mixing_viscV_r/viscV_r
      common /mixing_viscU_p/viscU_p
     &       /mixing_viscV_p/viscV_p
# else
      real visc3d_p(GLOBAL_2D_ARRAY,N)
      common /mixing_visc3d_p/visc3d_p
# endif
#endif
#ifdef DIF_COEF_3D
      real diff3d(GLOBAL_2D_ARRAY,N)
      common /mixing_diff3d/diff3d
# ifdef TS_SPLIT_UP3
      real diff3d_u(GLOBAL_2D_ARRAY,N)
      real diff3d_v(GLOBAL_2D_ARRAY,N)
      common /mixing_diff3d_u/diff3d_u
     &       /mixing_diff3d_v/diff3d_v
# endif
#endif

#ifdef SOLVE3D
      real Akv(GLOBAL_2D_ARRAY,0:N)
      real Akt(GLOBAL_2D_ARRAY,0:N,2)
      common /mixing_Akv/Akv /mixing_Akt/Akt
#ifdef RANDOM_WALK
      real dAktdz(GLOBAL_2D_ARRAY,0:N)
      common /mixing_dAktdz/dAktdz
#endif

# if defined ANA_VMIX || defined BVF_MIXING \
  || defined LMD_MIXING || defined LMD_SKPP || defined LMD_BKPP \
  || defined MY2_MIXING || defined MY25_MIXING
      real bvf(GLOBAL_2D_ARRAY,0:N)
      common /mixing_bvf/ bvf
# endif

# ifdef MY25_MIXING
!
!  Mellor-Yamada (1982) Level 2.5 vertical mixing variables.
! Akq     Vertical mixing coefficient (m^2/s) for turbulent energy.
! Lscale  Turbulent length scale (m).
! q2      Turbulent energy squared (m^2/s^2) at horizontal 
!             RHO-points and vertical W-points.
! q2l     Turbulent energy squared times turbulent length scale
!             [m^3/s^2] at horizontal RHO-points and vertical
!             W-points.
!
      real Akq(GLOBAL_2D_ARRAY,0:N)
      real Lscale(GLOBAL_2D_ARRAY,N)
      real q2(GLOBAL_2D_ARRAY,0:N,2)
      real q2l(GLOBAL_2D_ARRAY,0:N,2)
      common /my25_mix_Akq/Akq /my25_mix_Lscale/Lscale
     &       /my25_mix_q2/q2   /my25_mix_q2l/q2l
#endif /* MY25_MIXING */

# if defined LMD_SKPP || defined LMD_BKPP
!
! Large/McWilliams/Doney oceanic planetary boundary layer variables.
! ghats       Boundary layer nonlocal transport (m/s^2).
! hbl         Depth of oceanic surface boundary layer (m).
! hbbl        Depth of oceanic bottom boundary layer (m).
! kbl         Index of first grid level below "hbl".
! ustar       Turbulent friction velocity (m/s).
!
      integer kbl(GLOBAL_2D_ARRAY)
#  ifdef BIOLOGY
      real hel(GLOBAL_2D_ARRAY)
      common /lmd_hel/hel
#  endif
      real hbl(GLOBAL_2D_ARRAY)
      real hbbl(GLOBAL_2D_ARRAY)
      common /lmd_kpp_kbl/kbl     /lmd_kpp_hbl/hbl
     &       /lmd_kpp_hbbl/hbbl  
#  ifdef LMD_NONLOCAL
      real ghats(GLOBAL_2D_ARRAY,0:N)
      common /lmd_kpp_ghats/ghats
#  endif
# endif /* LMD_SKPP || LMD_BKPP */

# ifdef LMD_MIXING
      real ustar(GLOBAL_2D_ARRAY)
      common /lmd_kpp_ustar/ustar
# endif /* LMD_MIXING */

#endif /* SOLVE3D */

