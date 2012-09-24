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
! This is include file "grid.h": Environmental two-dimensional
! arrays associated with curvilinear horizontal coordinate system.
! 
! h       Model topography (bottom depth [m] at RHO-points.)
! f       Coriolis parameter [1/s].
! fomn    Compound term, f/[pm*pn] at RHO points.
!
! angler  Angle [radians] between XI-axis and the direction 
!             to the EAST at RHO-points.
!
! latr    Latitude (degrees_north) at RHO-, U-, and V-points.
! latu
! latv
! lonr    Longitude (degrees_east) at RHO-, U-, and V-points.
! lonu
! lonv
!
! xp      XI-coordinates [m] at PSI-points.
! xr      XI-coordinates (m] at RHO-points.
! yp      ETA-coordinates [m] at PSI-points.
! yr      ETA-coordinates [m] at RHO-points.
!
! pm      Coordinate transformation metric "m" [1/meters]
!              associated with the differential distances in XI.
! pn      Coordinate transformation metric "n" [1/meters]
!               associated with the differential distances in ETA.
! om_u    Grid spacing [meters] in the XI -direction at U-points.
! om_v    Grid spacing [meters] in the XI -direction at V-points.
! on_u    Grid spacing [meters] in the ETA-direction at U-points.
! on_v    Grid spacing [meters] in the ETA-direction at V-points.
!
! dmde    ETA-derivative of inverse metric factor "m", d(1/M)/d(ETA).
! dndx     XI-derivative  of inverse metric factor "n", d(1/N)/d(XI).
!
! pmon_p  Compound term, pm/pn at PSI-points.
! pmon_r  Compound term, pm/pn at RHO-points.
! pmon_u  Compound term, pm/pn at U-points.
! pnom_p  Compound term, pn/pm at PSI-points.
! pnom_r  Compound term, pn/pm at RHO-points.
! pnom_v  Compound term, pn/pm at V-points.
!
! rmask   Land-sea masking arrays at RHO-,U-,V- and PSI-points.
! umask   (rmask,umask,vmask) = (0=Land, 1=Sea);
! vmask
! pmask    pmask=(0=Land, 1=Sea, 1-gamma2 =boundary).
!
      real h(GLOBAL_2D_ARRAY)
      real hinv(GLOBAL_2D_ARRAY)
      real f(GLOBAL_2D_ARRAY)
      real fomn(GLOBAL_2D_ARRAY)
      common /grid_h/h /grid_hinv/hinv /grid_f/f /grid_fomn/fomn

# ifdef CURVGRID
      real angler(GLOBAL_2D_ARRAY)
      common /grid_angler/angler
# endif

#ifdef SPHERICAL
      real latr(GLOBAL_2D_ARRAY)
      real lonr(GLOBAL_2D_ARRAY)
      real latu(GLOBAL_2D_ARRAY)
      real lonu(GLOBAL_2D_ARRAY)
      real latv(GLOBAL_2D_ARRAY)
      real lonv(GLOBAL_2D_ARRAY)

      common /grid_latr/latr /grid_lonr/lonr
      common /grid_latu/latu /grid_lonu/lonu
      common /grid_latv/latv /grid_lonv/lonv
#else
      real xp(GLOBAL_2D_ARRAY)
      real xr(GLOBAL_2D_ARRAY)
      real yp(GLOBAL_2D_ARRAY)
      real yr(GLOBAL_2D_ARRAY)
      common /grid_xr/xr /grid_xp/xp /grid_yp/yp /grid_yr/yr
#endif

      real pm(GLOBAL_2D_ARRAY)
      real pn(GLOBAL_2D_ARRAY)
      real om_r(GLOBAL_2D_ARRAY)
      real on_r(GLOBAL_2D_ARRAY)
      real om_u(GLOBAL_2D_ARRAY)
      real on_u(GLOBAL_2D_ARRAY)
      real om_v(GLOBAL_2D_ARRAY)
      real on_v(GLOBAL_2D_ARRAY)
      real om_p(GLOBAL_2D_ARRAY)
      real on_p(GLOBAL_2D_ARRAY)
      real pn_u(GLOBAL_2D_ARRAY)
      real pm_v(GLOBAL_2D_ARRAY)
      real pm_u(GLOBAL_2D_ARRAY)
      real pn_v(GLOBAL_2D_ARRAY)
      common /metrics_pm/pm    /metrics_pn/pn
     &       /metrics_omr/om_r /metrics_on_r/on_r
     &       /metrics_omu/om_u /metrics_on_u/on_u
     &       /metrics_omv/om_v /metrics_on_v/on_v
     &       /metrics_omp/om_p /metrics_on_p/on_p
     &       /metrics_pnu/pn_u /metrics_pmv/pm_v
     &       /metrics_pmu/pm_u /metrics_pnv/pn_v

#if (defined CURVGRID && defined UV_ADV)
      real dmde(GLOBAL_2D_ARRAY)
      real dndx(GLOBAL_2D_ARRAY)
      common /metrics_dmde/dmde    /metrics_dndx/dndx
#endif

      real pmon_p(GLOBAL_2D_ARRAY)
      real pmon_r(GLOBAL_2D_ARRAY)
      real pmon_u(GLOBAL_2D_ARRAY)
      real pnom_p(GLOBAL_2D_ARRAY)
      real pnom_r(GLOBAL_2D_ARRAY)
      real pnom_v(GLOBAL_2D_ARRAY)
      real grdscl(GLOBAL_2D_ARRAY)
      common /metrics_pmon_p/pmon_p /metrics_pnom_p/pnom_p
     &       /metrics_pmon_r/pmon_r /metrics_pnom_r/pnom_r
     &       /metrics_pmon_u/pmon_u /metrics_pnom_v/pnom_v
     &                              /metrics_grdscl/grdscl

#ifdef MASKING
      real rmask(GLOBAL_2D_ARRAY)
      real pmask(GLOBAL_2D_ARRAY)
      real umask(GLOBAL_2D_ARRAY)
      real vmask(GLOBAL_2D_ARRAY)
      common /mask_r/rmask /mask_p/pmask
     &       /mask_u/umask /mask_v/vmask
#endif

