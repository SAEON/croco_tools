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
! Surface forcing variables.                                      
!                                                                 
! sustr        Kinematic surface momentum flux (wind stress) in   
!                the U-direction (m2/s2).                    
! svstr        Kinematic surface momentum flux (wind stress) in   
!                the V-direction (m2/s2).                   
! srflx        Kinematic surface shortwave solar radiation flux 
!                (Celsius m/s).
! stflx        Kinematic surface flux of tracer type variables 
!                (temperature: degC m/s; salinity: PSU m/s).
! upwi         Upwelling indice (m3 s-1).
!                                                                
      real sustr,svstr,srflx,stflx(NT),upwi
      common /forces/ sustr,svstr,srflx,stflx,upwi
!
! *_m		Monthly values
!
      real sustr_m(12), svstr_m(12), stflx_m(12),
     &     ssflx_m(12), sst_m(12), dqdt_m(12), srflx_m(12), upwi_m(12)
      common /forces_m/ sustr_m, svstr_m, stflx_m,
     &                  ssflx_m, sst_m, dqdt_m, srflx_m, upwi_m

#ifdef SST_SKIN
      real sstsk, dtw1
      common /forces_sstsk/ sstsk, dtw1
#endif

#ifdef BULK_FLUX
      real lonr,latr,
     &     radsw,radlw,prate,tair,rhum,uwnd,vwnd,wspd
      common /forces_bulk/ lonr,latr,
     &     radsw,radlw,prate,tair,rhum,uwnd,vwnd,wspd

      real radsw_m(12),radlw_m(12),prate_m(12),
     &     tair_m(12),rhum_m(12),uwnd_m(12),vwnd_m(12)
      common /forces_bulk_m/ radsw_m,radlw_m,prate_m,
     &                       tair_m,rhum_m,uwnd_m,vwnd_m

      real shflx_rsw,shflx_lat,shflx_sen,shflx_rlw
      common /forces_bulk_out/ shflx_rsw,shflx_lat,shflx_sen,shflx_rlw
#endif
