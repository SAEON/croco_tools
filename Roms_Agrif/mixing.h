!
! $Id: mixing.h,v 1.6 2005/10/10 13:40:18 pmarches Exp $
!
! This is include file "mixing.h"
!  ==== == ======= ==== ==========
!
#ifdef UV_VIS2
      real visc2_r(GLOBAL_2D_ARRAY)
      real visc2_p(GLOBAL_2D_ARRAY)
      common /mixing_visc2_r/visc2_r /mixing_visc2_p/visc2_p
# ifdef SMAGORINSKY
      real visc3d_r(GLOBAL_2D_ARRAY,N)
      real visc3d_p(GLOBAL_2D_ARRAY,N)
      common /mixing_visc3d_r/visc3d_r /mixing_visc3d_p/visc3d_p
# endif    
#endif
#ifdef UV_VIS4
      real visc4_r(GLOBAL_2D_ARRAY)
      real visc4_p(GLOBAL_2D_ARRAY)
      common /mixing_visc4_r/visc4_r /mixing_visc4_p/visc4_p
#endif
#ifdef TS_DIF2
      real diff2(GLOBAL_2D_ARRAY,NT)
      common /mixing_diff2/diff2
#endif
#ifdef TS_DIF4
      real diff4(GLOBAL_2D_ARRAY,NT)
      common /mixing_diff4/diff4
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

