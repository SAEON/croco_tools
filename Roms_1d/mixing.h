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
! mixing variables and parameters
!
! vonKar       von Karman constant.
! ghats        Boundary layer nonlocal transport (T units/m).
! z_bl         Depth of surface oceanic boundary layer (m).
! Kv           Vertical mixing coefficient (m2/s) for momentum. 
! Kt           Vertical mixing coefficient (m2/s) for temperature.   
! Ks           Vertical mixing coefficient (m2/s) for salinity.   
! Akt          Vertical mixing coefficient (m2/s) for tracers.   
!
      real vonKar
      parameter (vonKar=0.41)
      real Akt(0:N,NT)
#ifdef LMD_NONLOCAL
     &                ,ghats(0:N)
#endif
      common /trac_mixing/ Akt
#ifdef LMD_NONLOCAL
     &                    ,ghats
#endif
      real z_bl
      common /kpp_scalars/ z_bl
      real Kv(0:N)
      common /moment_mixing/ Kv
      real Kt(0:N)
      equivalence (Kt(0),Akt(0,itemp))
#ifdef SALINITY
      real Ks(0:N)
      equivalence (Ks(0),Akt(0,isalt))
#endif

