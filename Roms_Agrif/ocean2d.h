!
! $Id: ocean2d.h,v 1.2 2003/12/17 13:56:04 pmarches Exp $
!
/* This is include file "ocean2d.h".
--------------------------------------------------------------------
 zeta,rheta     Free surface elevation [m] and its time tendency;
 ubar,rubar     Vertically integrated  2D velocity components in 
 vbar,rvbar     XI- and ETA-directions and their time tendencies;
*/
      real zeta(GLOBAL_2D_ARRAY,4)
      real ubar(GLOBAL_2D_ARRAY,4)
      real vbar(GLOBAL_2D_ARRAY,4)
      common /ocean_zeta/zeta
     &       /ocean_ubar/ubar
     &       /ocean_vbar/vbar

#ifdef OXYGEN
      real u10(GLOBAL_2D_ARRAY)
      real Kv_O2(GLOBAL_2D_ARRAY)
      real O2satu(GLOBAL_2D_ARRAY)
      common /gasexc_o2/ u10, Kv_O2, O2satu
#endif /* OXYGEN */

