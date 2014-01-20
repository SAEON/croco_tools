#ifdef WKB_WWAVE
!---------------------------------------------------------
! wkx, wke  xi- and eta-dir components of wavenumber vector
! wac       wave action density (m^3/s^3)
! hrm       RMS wave height (m)
! frq       wave frequency (rad/s)
! wvn       wave number(rad/m)
! wsb       breaking dissipation, epsilon_b/rho/sigma (m3/s2)
! wfc       frictional dissipation, epsilon_d/rho/sigma (m3/s2)
!
      real wkb_btg, wkb_gam, wkb_rsb, wkb_roller,
     &     wkb_amp, wkb_ang, wkb_prd, wkb_tide
      common /wkb_par/ wkb_btg, wkb_gam, wkb_rsb, wkb_roller, 
     &                 wkb_amp, wkb_ang, wkb_prd, wkb_tide

      real wkx(GLOBAL_2D_ARRAY,2)
      real wke(GLOBAL_2D_ARRAY,2)
      real wac(GLOBAL_2D_ARRAY,2)
      real hrm(GLOBAL_2D_ARRAY,2)
      real frq(GLOBAL_2D_ARRAY,2)
      real wcg(GLOBAL_2D_ARRAY,2)
      real wsb(GLOBAL_2D_ARRAY,2)
      real wvn(GLOBAL_2D_ARRAY,2)
      real wfc(GLOBAL_2D_ARRAY,2)
      common /wkb_wkx/wkx /wkb_wke/wke /wkb_wac/wac
     &       /wkb_hrm/hrm /wkb_frq/frq /wkb_wcg/wcg
     &       /wkb_wsb/wsb /wkb_wvn/wvn /wkb_wfc/wfc
# ifdef WAVE_ROLLER
      real war(GLOBAL_2D_ARRAY,2)
      real wcr(GLOBAL_2D_ARRAY,2)
      real wsr(GLOBAL_2D_ARRAY,2)
      common /wkb_war/war /wkb_wcr/wcr /wkb_wsr/wsr
# endif

! for diagnostics
      integer iwave, winfo
      real*QUAD  av_wac, av_wkn, thwave
      common /wkb_diag_comm/ winfo, iwave, av_wac, av_wkn, thwave

! for CEW
# ifdef MRL_CEW
      integer wint, winterp, iif_wave, interp_max, wavg, cewavg, wcew
      parameter (interp_max = 5)
      parameter (wavg = 1)
      parameter (cewavg = 10)
      parameter (wcew = 3)
#  ifdef WKB_TIME_FILTER
      real uwave(GLOBAL_2D_ARRAY,5)
      real vwave(GLOBAL_2D_ARRAY,5)
      real zwave(GLOBAL_2D_ARRAY,5)
#  else
      real uwave(GLOBAL_2D_ARRAY,2)
      real vwave(GLOBAL_2D_ARRAY,2)
      real zwave(GLOBAL_2D_ARRAY,2)
#  endif
      common /wkb_cew/ wint, winterp,iif_wave
      common /wkb_cewt/  uwave,  vwave,  zwave
# endif

! for boundaries
# ifdef WKB_OBC_WEST
      real wac_west(0:Mm+1), 
     &     wkx_west(0:Mm+1),
     &     wke_west(0:Mm+1)
      common /wbry_west/ wac_west, 
     &                   wkx_west,
     &                   wke_west
# endif
# ifdef WKB_OBC_EAST
      real wac_east(0:Mm+1), 
     &     wkx_east(0:Mm+1),
     &     wke_east(0:Mm+1)
      common /wbry_east/ wac_east, 
     &                   wkx_east,
     &                   wke_east
# endif
# ifdef WKB_OBC_SOUTH
      real wac_south(0:Lm+1), 
     &     wkx_south(0:Lm+1),
     &     wke_south(0:Lm+1)
      common /wbry_south/ wac_south, 
     &                    wkx_south,
     &                    wke_south
# endif
# ifdef WKB_OBC_NORTH
      real wac_north(0:Lm+1), 
     &     wkx_north(0:Lm+1),
     &     wke_north(0:Lm+1)
      common /wbry_north/ wac_north, 
     &                    wkx_north,
     &                    wke_north
# endif

#endif /* WKB_WWAVE */
