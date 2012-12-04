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
!  This is include file "forces.h"
!--------------------------------------------------------------------
!  SURFACE MOMENTUM FLUX (WIND STRESS):
!--------------------------------------------------------------------
!  sustr |  XI- and ETA-components of kinematic surface momentum flux
!  svstr |  (wind stresses) defined at horizontal U- and V-points.
!            dimensioned as [m^2/s^2].
!
      real sustr(GLOBAL_2D_ARRAY)
      real svstr(GLOBAL_2D_ARRAY)
      common /forces_sustr/sustr /forces_svstr/svstr
#ifndef ANA_SMFLUX
!
!  tsms      Time of surface momentum stresses.
!
!  sustrg |  Two-time level gridded data for XI- and ETA-componets
!  svstrg |  of kinematic surface momentum flux (wind stess).
!
!  sustrp |  Two-time level point data for XI- and ETA-componets 
!  svstrp |  of kinematic surface momentum flux (wind stess).
!
      real sustrg(GLOBAL_2D_ARRAY,2)
      real svstrg(GLOBAL_2D_ARRAY,2)
      common /smsdat_sustrg/sustrg /smsdat_svstrg/svstrg

      real    sustrp(2), svstrp(2), sms_time(2)
      real    sms_cycle, sms_scale 
      integer itsms, sms_ncycle, sms_rec, lsusgrd,    
     &        lsvsgrd,sms_tid, susid, svsid
       common /smsdat1/ sustrp, svstrp, sms_time
       common /smsdat2/ sms_cycle, sms_scale
       common /smsdat3/ itsms, sms_ncycle, sms_rec, lsusgrd,
     &                  lsvsgrd,sms_tid, susid, svsid

#  undef SMFLUX_DATA
#endif /* !ANA_SMFLUX */
!
!  BOTTOM MOMENTUM FLUX:
!--------------------------------------------------------------------
!  bustr |  XI- and ETA-components of kinematic bottom momentum flux
!  bvstr |  (drag) defined at horizontal U- and V-points [m^2/s^2].
!
      real bustr(GLOBAL_2D_ARRAY)
      real bvstr(GLOBAL_2D_ARRAY)
      common /forces_bustr/bustr /forces_bvstr/bvstr
#ifndef ANA_BMFLUX
!
!  tbms      Time of surface momentum stresses.
!
!  bustrg |  Two-time level gridded data for XI- and ETA-componets 
!  bvstrg |  of kinematic bottom momentum flux.
!
!  bustrp |  Two-time level point data for XI- and ETA-componets   
!  bvstrp |  of kinematic bottom momentum flux.
!
      real bustrg(GLOBAL_2D_ARRAY,2)
      real bvstrg(GLOBAL_2D_ARRAY,2)
      common /bmsdat_bustrg/bustrg /bmsdat_bvstrg/bvstrg

      real bms_tintrp(2), bustrp(2),    bvstrp(2), tbms(2)
      real bmsclen, bms_tstart, bms_tend,  tsbms, sclbms
      integer itbms,      bmstid,busid, bvsid,     tbmsindx
      logical bmscycle,   bms_onerec,   lbusgrd,   lbvsgrd
      common /bmsdat/
     &        bms_tintrp, bustrp,       bvstrp,    tbms,
     &        bmsclen,    bms_tstart,   bms_tend,  tsbms,   sclbms,
     &        itbms,      bmstid,busid, bvsid,     tbmsindx,
     &        bmscycle,   bms_onerec,   lbusgrd,   lbvsgrd

#  undef BMFLUX_DATA
#endif /* !ANA_BMFLUX */
#ifdef SOLVE3D
!
!  SURFACE TRACER FLUXES: 
!--------------------------------------------------------------------
!  stflx   Kinematic surface fluxes of tracer type variables at
!          horizontal RHO-points. Physical dimensions [degC m/s] -
!          temperature; [PSU m/s] - salinity.
!
      real stflx(GLOBAL_2D_ARRAY,NT)
      common /forces_stflx/stflx
# ifdef BULK_FLUX
      real shflx_rsw(GLOBAL_2D_ARRAY)
      common /frc_shflx_rsw/shflx_rsw
      real shflx_rlw(GLOBAL_2D_ARRAY)
      common /frc_shflx_rlw/shflx_rlw
      real shflx_lat(GLOBAL_2D_ARRAY)
      common /frc_shflx_lat/shflx_lat
      real shflx_sen(GLOBAL_2D_ARRAY)
      common /frc_shflx_sen/shflx_sen
# endif
# ifdef SST_SKIN
      real sst_skin(GLOBAL_2D_ARRAY)
      common /frc_sst_skin/ sst_skin
      real dT_skin(GLOBAL_2D_ARRAY)
      common /frc_dT_skin/ dT_skin
# endif
# if !defined ANA_STFLUX || !defined ANA_SSFLUX
!
!  stflxg   Two-time level surface tracer flux grided data.
!  stflxp   Two-time level surface tracer flux point  data.
!  tstflx   Time of surface tracer flux.
!
      real stflxg(GLOBAL_2D_ARRAY,2,NT)
      common /stfdat_stflxg/stflxg

      real stflxp(2,NT), stf_time(2,NT)
      real stf_cycle(NT), stf_scale(NT)
      integer itstf(NT), stf_ncycle(NT), stf_rec(NT),   
     &        lstfgrd(NT), stf_tid(NT), stf_id(NT)
      common /stfdat/ stflxp,  stf_time, stf_cycle,     stf_scale,
     &        itstf,     stf_ncycle,     stf_rec,       lstfgrd,
     &                                   stf_tid,       stf_id
#   undef STFLUX_DATA
# endif /* !ANA_STFLUX || !ANA_SSFLUX */
!
!  BOTTOM TRACER FLUXES:
!--------------------------------------------------------------------
!  btflx  Kinematic bottom fluxes of tracer type variables at
!         horizontal RHO-points. Physical dimensions [degC m/s] -
!         temperature; [PSU m/s] - salinity.
!
      real btflx(GLOBAL_2D_ARRAY,NT)
      common /forces_btflx/btflx
# ifndef ANA_BTFLUX
!
!  btflxg   Two-time level bottom tracer flux grided data.
!  btflxp   Two-time level bottom tracer flux point data.
!  tbtflx   Time of bottom tracer flux.
!
      real btflxg(GLOBAL_2D_ARRAY,2,NT)
      common /btfdat_btflxg/btflxg

      real sclbtf(NT), btf_tstart(NT), btf_tend(NT), 
     &     btfclen(NT), tsbtf(NT)
      real btf_tintrp(2,NT), btflxp(2,NT),  tbtflx(2,NT)
      integer itbtf(NT), btfid(NT), btftid(NT),tbtfindx(NT)
      logical lbtfgrd(NT), btfcycle(NT), btf_onerec(NT)
      common /btfdat/
     &        sclbtf,      btf_tstart,   btf_tend,      btfclen,
     &        tsbtf,       btf_tintrp,   btflxp,        tbtflx,
     &        itbtf,       btfid,        btftid,        tbtfindx,
     &        lbtfgrd,     btfcycle,     btf_onerec

#   undef BTFLUX_DATA
# endif /* !ANA_BTFLUX */
#ifdef QCORRECTION
!
!  HEAT FLUX CORRECTION
!--------------------------------------------------------------------
!  dqdt     Kinematic surface net heat flux sensitivity to SST [m/s].
!  sst      Current sea surface temperature [degree Celsius].
!
      real dqdt(GLOBAL_2D_ARRAY)
      real sst(GLOBAL_2D_ARRAY)
      common /forces_dqdt/dqdt /forces_sst/sst
#  ifndef ANA_SST
!
!  dqdtg |  Two-time-level grided data for net surface heat flux
!  sstg  |  sensitivity to SST grided data [Watts/m^2/Celsius] and
!              sea surface temperature [degree Celsius].
!  dqdtp |  Two-time-level point data for net surface heat flux
!  sstp  |  sensitivity to SST grided data [Watts/m^2/Celsius] and
!              sea surface temperature [degree Celsius].
!  tsst     Time of sea surface temperature data.
!
      real dqdtg(GLOBAL_2D_ARRAY,2)
      real sstg(GLOBAL_2D_ARRAY,2)
      common /sstdat_dqdtg/dqdtg /sstdat_sstg/sstg

      real    sstp(2), dqdtp(2), sst_time(2)
      real    sst_cycle, scldqdt
      integer itsst, sst_ncycle, sst_rec,  sst_tid,  sst_id,
     &        dqdt_id,     lsstgrd,   sstunused
      common /sstdat1/ sstp, dqdtp, sst_time
      common /sstdat2/ sst_cycle, scldqdt
      common /sstdat3/ itsst, sst_ncycle, sst_rec, sst_tid, sst_id,
     &                 dqdt_id, lsstgrd, sstunused

#    undef SST_DATA
#  endif /* !ANA_SST */
# endif /* QCORRECTION */
#ifdef PSOURCE_NCFILE
!
!  RIVER RUNOFF
!--------------------------------------------------------------------
!  riv      river runoff [m3/s].
!
      real riv(Msrc)
      common /forces_riv/riv
!
!  rivg  |  Two-time-level grided data for river runoff [m3/s].
!  triv     Time of river runoff data.
!

      real rivg(Msrc,2)
      common /rivdat_rivg/rivg

      real    riv_time(2)
      real    riv_cycle
      integer itriv, riv_ncycle, riv_rec,  riv_tid,  riv_id
      common /rivdat1/ riv_time
      common /rivdat2/ riv_cycle
      common /rivdat3/ itriv, riv_ncycle, riv_rec, riv_tid, riv_id

#    undef RIV_DATA
# endif /* PSOURCE_NCFILE */
# if defined SALINITY && defined SFLX_CORR
!
!  SALT FLUX CORRECTION
!--------------------------------------------------------------------
!  sss      Current sea surface salinity [PSU].
!
      real sss(GLOBAL_2D_ARRAY)
      common /forces_sss/sss
#  if !defined QCORRECTION
      real dqdt(GLOBAL_2D_ARRAY)
      common /forces_dqdt/dqdt 
#  endif
#  ifndef ANA_SSS
!
!  dqdtg |  Two-time-level grided data for net surface heat flux
!  sssg  |  Two-time-level grided data for 
!              sea surface salinity [PSU].
!  dqdtp |  Two-time-level point data for net surface heat flux
!  sssp  |  Two-time-level point data for
!              sea surface salinity [PSU].
!  tsss     Time of sea surface salinity data.
!
      real sssg(GLOBAL_2D_ARRAY,2)
      common /sssdat_sssg/sssg

      real sssp(2),  sss_time(2)
      real sss_cycle
      integer itsss, sss_ncycle, sss_rec,  sss_tid,  sss_id,
     &                          lsssgrd,   sssunused
      common /sssdat/
     &        sssp,  sss_time,    sss_cycle,
     &        itsss, sss_ncycle, sss_rec,     sss_tid,   sss_id,
     &                           lsssgrd,   sssunused
#   if !defined QCORRECTION
      real dqdtg(GLOBAL_2D_ARRAY,2)
      real    dqdtp(2)
      real    scldqdt
      integer dqdt_id
      common /sstdat_dqdtg/dqdtg
      common /sssdat1/ dqdtp
      common /sstdat2/ scldqdt
      common /sstdat3/ dqdt_id
#   endif
#   undef SSS_DATA
#  endif /* !ANA_SSS */
# endif /* SALINITY && SFLX_CORR */
!
!
#ifdef BULK_FLUX
!
!  HEAT FLUX BULK FORMULATION
!--------------------------------------------------------------------
!  tair     surface air temperature at 2m [degree Celsius].
!  wsp      wind speed at 10m [degree Celsius].
!  rhum     surface air relative humidity 2m [fraction]
!  prate    surface precipitation rate [cm day-1]
!  radlw    net terrestrial longwave radiation [Watts meter-2]
!  radsw    net solar shortwave radiation [Watts meter-2]
!
      real tair(GLOBAL_2D_ARRAY)
      real rhum(GLOBAL_2D_ARRAY)
      real prate(GLOBAL_2D_ARRAY)
      real radlw(GLOBAL_2D_ARRAY)
      real radsw(GLOBAL_2D_ARRAY)
      real wspd(GLOBAL_2D_ARRAY)
# ifdef BULK_SM_UPDATE
      real uwnd(GLOBAL_2D_ARRAY)
      real vwnd(GLOBAL_2D_ARRAY)
# endif

      common /bulk_tair/tair
     &       /bulk_rhum/rhum 
     &       /bulk_prate/prate
     &       /bulk_radlw/radlw 
     &       /bulk_radsw/radsw
     &       /bulk_wspd/wspd
# ifdef BULK_SM_UPDATE
     &       /bulk_uwnd/uwnd 
     &       /bulk_vwnd/vwnd
# endif

      real tairg(GLOBAL_2D_ARRAY,2)
      real rhumg(GLOBAL_2D_ARRAY,2)
      real prateg(GLOBAL_2D_ARRAY,2)
      real radlwg(GLOBAL_2D_ARRAY,2)
      real radswg(GLOBAL_2D_ARRAY,2)
      real wspdg(GLOBAL_2D_ARRAY,2)
# ifdef BULK_SM_UPDATE
      real uwndg(GLOBAL_2D_ARRAY,2)
      real vwndg(GLOBAL_2D_ARRAY,2)
# endif

      common /bulkdat_tairg/tairg
     &       /bulkdat_rhumg/rhumg
     &       /bulkdat_prateg/prateg
     &       /bulkdat_radlwg/radlwg
     &       /bulkdat_radswg/radswg
     &       /bulkdat_wspdg/wspdg
# ifdef BULK_SM_UPDATE
     &       /bulk_uwndg/uwndg
     &       /bulk_vwndg/vwndg
# endif

      real    tairp(2),rhump(2),pratep(2),radlwp(2),radswp(2)
     &       ,wspdp(2)
# ifdef BULK_SM_UPDATE
     &       ,uwndp(2),vwndp(2)
# endif
      real    bulk_time(2), bulk_cycle
      integer tair_id,rhum_id,prate_id,radlw_id,radsw_id,
     &        ltairgrd,lrhumgrd,lprategrd,lradlwgrd,lradswgrd
     &       ,wspd_id,lwspdgrd
# ifdef BULK_SM_UPDATE
     &       ,uwnd_id,vwnd_id,luwndgrd,lvwndgrd
# endif
      integer itbulk,bulk_ncycle,bulk_rec,bulk_tid,
     &        bulkunused

      common /bulkdat1/
     &        tair_id,rhum_id,prate_id,radlw_id,radsw_id,
     &        ltairgrd,lrhumgrd,lprategrd,lradlwgrd,lradswgrd, 
     &        itbulk, bulk_ncycle, bulk_rec, bulk_tid, 
     &        bulkunused
     &       ,wspd_id,lwspdgrd
# ifdef BULK_SM_UPDATE
     &       ,uwnd_id,vwnd_id,luwndgrd,lvwndgrd
# endif
      common /bulkdat2/
     &        tairp,rhump,pratep,radlwp,radswp,
     &        bulk_time, bulk_cycle
     &       ,wspdp
# ifdef BULK_SM_UPDATE
     &       ,uwndp,vwndp
# endif
#endif /* BULK_FLUX */
!
!  SOLAR SHORT WAVE RADIATION FLUX.
!--------------------------------------------------------------------
!  srflx  Kinematic surface shortwave solar radiation flux
!         [degC m/s] at horizontal RHO-points
!
      real srflx(GLOBAL_2D_ARRAY)
      common /forces_srflx/srflx
# ifdef ANA_DIURNAL_SW
      real sin_phi(GLOBAL_2D_ARRAY),  
     &     cos_phi(GLOBAL_2D_ARRAY), 
     &     tan_phi(GLOBAL_2D_ARRAY)
      common /diu_srflx/ sin_phi, cos_phi, tan_phi
# endif
# ifndef ANA_SRFLUX
!
!  srflxg | Two-time-level grided and point data for surface 
!  srflxp |      solar shortwave radiation flux grided data.
!  tsrflx   Time of solar shortwave radiation flux.
!
      real srflxg(GLOBAL_2D_ARRAY,2)
      common /srfdat_srflxg/srflxg

      real srflxp(2),srf_time(2)
      real srf_cycle, srf_scale
      integer itsrf, srf_ncycle, srf_rec, 
     &        lsrfgrd, srf_tid, srf_id 
      common /srfdat/
     &        srflxp,srf_time,   srf_cycle, srf_scale,
     &        itsrf, srf_ncycle, srf_rec,   lsrfgrd, srf_tid, srf_id

#   undef SRFLUX_DATA
# endif /* !ANA_SRFLUX */

# ifdef BBL
!
!  WIND INDUCED WAVES:
!--------------------------------------------------------------------
! Awave   |   Wind induced wave amplitude [m] at RHO-points.
! Dwave   |   Wind induced wave direction [radians] at RHO-points.
! Pwave   |   Wind induced wave period [s] at RHO-points.
!
      real Awave(GLOBAL_2D_ARRAY)
      real Dwave(GLOBAL_2D_ARRAY)
      real Pwave(GLOBAL_2D_ARRAY)
      
      common /bbl_Awave/Awave /bbl_Dwave/Dwave /bbl_Pwave/Pwave

#  ifndef ANA_WWAVE
!
!  tww      Time of wind induced waves.
!
!  wwag  |  Two-time-level       | wave amplitude [m]
!  wwdg  |  gridded data         | wave direction [radians]
!  wwpg  |  for wind induced     ! wave period [s]
!
!  wwap  |  Two-time-level       | wave amplitude [m]
!  wwdp  |  point data           | wave direction [radians]
!  wwpp  |  for wind induced     ! wave period [s]
!
      real wwag(GLOBAL_2D_ARRAY,2)
      real wwdg(GLOBAL_2D_ARRAY,2)
      real wwpg(GLOBAL_2D_ARRAY,2)
      
      common /wwf_wwag/wwag /wwf_wwdg/wwdg /wwf_wwpg/wwpg

      real wwap(2), wwdp(2),  wwpp(2), wwv_time(2)
      real ww_cycle, wwa_scale, wwd_scale, wwp_scale,
     &                wwagrd,   wwdgrd,    wwpgrd
      integer itww,   ww_ncycle, ww_rec,    ww_tid,     
     &        wwa_id, wwd_id,    wwp_id
      common /wwdat/
     &        wwap,      wwdp,      wwpp,      wwv_time, ww_cycle, 
     &        wwa_scale, wwd_scale, wwp_scale, itww,    ww_ncycle,     
     &        wwagrd,    wwdgrd,    wwpgrd,    ww_tid,  ww_rec,
     &        wwa_id,    wwd_id,    wwp_id
     

#   undef WWAVE_DATA
#  endif /* !ANA_WAVE */
# endif /* BBL */
#endif /* SOLVE3D */



