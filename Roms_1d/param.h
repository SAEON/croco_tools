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
! model parameters
!
! N           Number of vertical levels.        
! NT          Total number of tracer type variables.
!
! Tracer identification indices:
!
!    itemp    Potential temperature.
!    isalt    Salinity.
!    iNO3_    Nitrate concentration.
!    iNH4_    Ammonium concentration.
!    iChla    Chlor a [mg Chl a m-3].
!    iPhyt    Phytoplankton concentration.
!    iZoo     Zooplankton concentration.
!    iSDet    Small detritus concentration.
!    iLDet    Large detritus concentration.
!    iOxy_    Oxygen [O2 mMol m-3]
!
! pi          Ratio of circumference to diameter.
! g           Acceleration due to gravity (m/s^2). 
! rho0        Mean density (kg/m^3). 
! Cp          Specific heat for seawater (Joules/Kg/degC).
! f           Coriolis parameter (1/s).
! latr        Latitude in degrees.
!
      integer N
      parameter (N=100)
      integer NT, itemp, isalt
      parameter (itemp=1, isalt=2)
#ifdef BIOLOGY
#ifdef OXYGEN
      integer iNO3_, iNH4_, iChla, iPhyt, iZoo_, iSDet, iLDet, iOxy_
      parameter (NT=10,    iNO3_=3,   iNH4_=4,   iChla=5,
     &           iPhyt=6, iZoo_=7,   iSDet=8,   iLDet=9, iOxy_=10)
#else
      integer iNO3_, iNH4_, iChla, iPhyt, iZoo_, iSDet, iLDet
      parameter (NT=9,    iNO3_=3,   iNH4_=4,   iChla=5,
     &           iPhyt=6, iZoo_=7,   iSDet=8,   iLDet=9)
#endif
#else
      parameter (NT=2)
#endif 
      real pi, g, rho0, Cp, ods, ds, day2sec, sec2day,
     &                                  deg2rad, rad2deg
      parameter (pi=3.14159265358979323846, 
     &           g=9.81, rho0=1025., Cp=3985.0,  
     &           ods=N, ds=1./ods,  
     &           day2sec=86400., sec2day=1./86400.,
     &           deg2rad=pi/180., rad2deg=180./pi)

