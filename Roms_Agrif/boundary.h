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
#ifndef ANA_BRY
      real bry_time(2)
      common /bry_indices_array/ bry_time
      real bry_cycle
      common /bry_indices_real/ bry_cycle
      integer bry_id, bry_time_id, bry_ncycle, bry_rec, itbry, ntbry
      common /bry_indices_integer/ bry_id, bry_time_id, bry_ncycle,
     &                             bry_rec, itbry, ntbry

#if defined BIOLOGY || defined PISCES
      logical got_tbry(NT)
      real bry_time1(2,NT)
      common /bry_indices_array1/ bry_time1
      real bry_cycle1(NT)
      common /bry_indices_real1/ bry_cycle1
      integer bry_tid(NT), bry_ncycle1(NT),
     $        bry_rec1(NT), itbry1(NT), ntbry1(NT)
      common /bry_indices_integer1/ bry_tid, bry_ncycle1,
     &                              bry_rec1, itbry1, ntbry1
      common /bry_logical/ got_tbry
  
#endif

# if defined OBC_WEST || defined AGRIF_OBC_WEST
#  ifdef Z_FRC_BRY
      integer zetabry_west_id
      common /zeta_west_id/ zetabry_west_id
#  endif
#  ifdef M2_FRC_BRY
      integer ubarbry_west_id, vbarbry_west_id
      common /ubar_west_id/ ubarbry_west_id, vbarbry_west_id
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
      integer ubry_west_id, vbry_west_id
      common /u_west_id/ ubry_west_id, vbry_west_id
#   endif
#   ifdef T_FRC_BRY
      integer tbry_west_id(NT)
      common /t_west_id/ tbry_west_id
#   endif
#  endif
# endif

# if defined OBC_EAST || defined AGRIF_OBC_EAST
#  ifdef Z_FRC_BRY
      integer zetabry_east_id
      common /zeta_east_id/ zetabry_east_id
#  endif
#  ifdef M2_FRC_BRY
      integer ubarbry_east_id, vbarbry_east_id
      common /ubar_east_id/ ubarbry_east_id, vbarbry_east_id
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
      integer ubry_east_id, vbry_east_id
      common /u_east_id/ ubry_east_id, vbry_east_id
#   endif
#   ifdef T_FRC_BRY
      integer tbry_east_id(NT)
      common /t_east_id/ tbry_east_id
#   endif
#  endif
# endif

# if defined OBC_SOUTH || defined AGRIF_OBC_SOUTH
#  ifdef Z_FRC_BRY
      integer zetabry_south_id
      common /zeta_south_id/ zetabry_south_id
#  endif
#  ifdef M2_FRC_BRY
      integer ubarbry_south_id, vbarbry_south_id
      common /ubar_south_id/ ubarbry_south_id, vbarbry_south_id
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
      integer ubry_south_id, vbry_south_id
      common /u_south_id/ ubry_south_id, vbry_south_id
#   endif
#   ifdef T_FRC_BRY
      integer tbry_south_id(NT)
      common /t_south_id/ tbry_south_id
#   endif
#  endif
# endif

# if defined OBC_NORTH || defined AGRIF_OBC_NORTH
#  ifdef Z_FRC_BRY
      integer zetabry_north_id
      common /zeta_north_id/ zetabry_north_id
#  endif
#  ifdef M2_FRC_BRY
      integer ubarbry_north_id, vbarbry_north_id
      common /ubar_north_id/ ubarbry_north_id, vbarbry_north_id
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
      integer ubry_north_id, vbry_north_id
      common /u_north_id/ ubry_north_id, vbry_north_id
#   endif
#   ifdef T_FRC_BRY
      integer tbry_north_id(NT)
      common /t_north_id/ tbry_north_id
#   endif
#  endif
# endif
#endif  /* ANA_BRY */

# if defined OBC_WEST || defined AGRIF_OBC_WEST
#  ifdef Z_FRC_BRY
      real zetabry_west(GLOBAL_1D_ARRAYETA),
     &    zetabry_west_dt(GLOBAL_1D_ARRAYETA,2)
      common /bry_zeta_west/ zetabry_west, zetabry_west_dt
#  endif
#  ifdef M2_FRC_BRY
      real ubarbry_west(GLOBAL_1D_ARRAYETA),
     &    ubarbry_west_dt(GLOBAL_1D_ARRAYETA,2)
     &    ,vbarbry_west(GLOBAL_1D_ARRAYETA),
     &    vbarbry_west_dt(GLOBAL_1D_ARRAYETA,2)
      common /bry_ubar_west/ ubarbry_west, ubarbry_west_dt,
     &                       vbarbry_west, vbarbry_west_dt
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
      real ubry_west(GLOBAL_1D_ARRAYETA,N),
     &    ubry_west_dt(GLOBAL_1D_ARRAYETA,N,2)
     &    ,vbry_west(GLOBAL_1D_ARRAYETA,N),
     &    vbry_west_dt(GLOBAL_1D_ARRAYETA,N,2)
      common /bry_u_west/ ubry_west, ubry_west_dt,
     &                    vbry_west, vbry_west_dt
#   endif
#   ifdef T_FRC_BRY
      real tbry_west(GLOBAL_1D_ARRAYETA,N,NT),
     &    tbry_west_dt(GLOBAL_1D_ARRAYETA,N,2,NT)
      common /bry_t_west/ tbry_west, tbry_west_dt
#   endif
#  endif
# endif

# if defined OBC_EAST || defined AGRIF_OBC_EAST
#  ifdef Z_FRC_BRY
      real zetabry_east(GLOBAL_1D_ARRAYETA),
     &    zetabry_east_dt(GLOBAL_1D_ARRAYETA,2)
      common /bry_zeta_east/ zetabry_east, zetabry_east_dt
#  endif
#  ifdef M2_FRC_BRY
      real ubarbry_east(GLOBAL_1D_ARRAYETA),
     &    ubarbry_east_dt(GLOBAL_1D_ARRAYETA,2)
     &    ,vbarbry_east(GLOBAL_1D_ARRAYETA),
     &    vbarbry_east_dt(GLOBAL_1D_ARRAYETA,2)
      common /bry_ubar_east/ ubarbry_east, ubarbry_east_dt,
     &                       vbarbry_east, vbarbry_east_dt
#  endif
#  ifdef SOLVE3D 
#   ifdef M3_FRC_BRY
      real ubry_east(GLOBAL_1D_ARRAYETA,N),
     &    ubry_east_dt(GLOBAL_1D_ARRAYETA,N,2)
     &    ,vbry_east(GLOBAL_1D_ARRAYETA,N),
     &    vbry_east_dt(GLOBAL_1D_ARRAYETA,N,2)
      common /bry_u_east/ ubry_east, ubry_east_dt,
     &                    vbry_east, vbry_east_dt
#   endif
#   ifdef T_FRC_BRY
      real tbry_east(GLOBAL_1D_ARRAYETA,N,NT),
     &    tbry_east_dt(GLOBAL_1D_ARRAYETA,N,2,NT)
      common /bry_t_east/ tbry_east, tbry_east_dt
#   endif
#  endif
# endif

# if defined OBC_SOUTH || defined AGRIF_OBC_SOUTH
#  ifdef Z_FRC_BRY 
      real zetabry_south(GLOBAL_1D_ARRAYXI),
     &    zetabry_south_dt(GLOBAL_1D_ARRAYXI,2)
      common /bry_zeta_south/ zetabry_south, zetabry_south_dt
#  endif
#  ifdef M2_FRC_BRY
      real ubarbry_south(GLOBAL_1D_ARRAYXI),
     &    ubarbry_south_dt(GLOBAL_1D_ARRAYXI,2)
     &    ,vbarbry_south(GLOBAL_1D_ARRAYXI),
     &    vbarbry_south_dt(GLOBAL_1D_ARRAYXI,2)
      common /bry_ubar_south/ ubarbry_south, ubarbry_south_dt,
     &                        vbarbry_south, vbarbry_south_dt
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
      real ubry_south(GLOBAL_1D_ARRAYXI,N),
     &    ubry_south_dt(GLOBAL_1D_ARRAYXI,N,2)
     &    ,vbry_south(GLOBAL_1D_ARRAYXI,N),
     &    vbry_south_dt(GLOBAL_1D_ARRAYXI,N,2)
      common /bry_u_south/ ubry_south, ubry_south_dt,
     &                     vbry_south, vbry_south_dt
#   endif
#   ifdef T_FRC_BRY
      real tbry_south(GLOBAL_1D_ARRAYXI,N,NT),
     &    tbry_south_dt(GLOBAL_1D_ARRAYXI,N,2,NT)
      common /bry_t_south/ tbry_south, tbry_south_dt
#   endif
#  endif
# endif

# if defined OBC_NORTH || defined AGRIF_OBC_NORTH
#  ifdef Z_FRC_BRY
      real zetabry_north(GLOBAL_1D_ARRAYXI),
     &    zetabry_north_dt(GLOBAL_1D_ARRAYXI,2)
      common /bry_zeta_north/ zetabry_north, zetabry_north_dt
#  endif
#  ifdef M2_FRC_BRY
      real ubarbry_north(GLOBAL_1D_ARRAYXI),
     &    ubarbry_north_dt(GLOBAL_1D_ARRAYXI,2)
     &    ,vbarbry_north(GLOBAL_1D_ARRAYXI),
     &    vbarbry_north_dt(GLOBAL_1D_ARRAYXI,2)
      common /bry_ubar_north/ ubarbry_north, ubarbry_north_dt,
     &                        vbarbry_north, vbarbry_north_dt
#  endif
#  ifdef SOLVE3D
#   ifdef M3_FRC_BRY
      real ubry_north(GLOBAL_1D_ARRAYXI,N),
     &    ubry_north_dt(GLOBAL_1D_ARRAYXI,N,2)
     &    ,vbry_north(GLOBAL_1D_ARRAYXI,N),
     &    vbry_north_dt(GLOBAL_1D_ARRAYXI,N,2)
      common /bry_u_north/ ubry_north, ubry_north_dt,
     &                     vbry_north, vbry_north_dt
#   endif
#   ifdef T_FRC_BRY
      real tbry_north(GLOBAL_1D_ARRAYXI,N,NT),
     &    tbry_north_dt(GLOBAL_1D_ARRAYXI,N,2,NT)
      common /bry_t_north/ tbry_north, tbry_north_dt
#   endif
#  endif
# endif

