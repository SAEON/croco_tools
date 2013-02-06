! $Id$
!
!=========================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France.
! The two other branches, from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al), are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
!
! ROMS_AGRIF website : http://roms.mpl.ird.fr
!=========================================================================
!
#if defined key_passivetrc
CCC---------------------------------------------------------------------
CCC
CCC                         COMMON passivetrc
CCC                       *********************
CCC
CCC  purpose :
CCC  ---------
CCC     INCLUDE COMMON FILE for passive tracer
CCC
CC
CC      cotxxx for REAL variables or REAL arrays
CC      citxxx for INTEGER or other
CC
CC   modifications :
CC   -------------
CC      original    : 95-02 (M. Levy) passive tracers
CC      opa8        : 96-11
CC      opa8.1      : 98-03
CC                    99-07 (M. Levy) LOBSTER1 model
CC                    00-04 (M.A. Foujols, O. Aumont) HAMOCC3 et P3ZD 
CC                    00-11 (MA Foujols E Kestenare) add trcrat, ahtrc0
CC                            and aeivtr0 for passive tracer diffusion
CC
CCC---------------------------------------------------------------------
CCC  opa8, ipsl (11/96)
CCC---------------------------------------------------------------------
CC
CC----------------------------------------------------------------------
CC
CC COMMON/cittrc/ : passive tracers names and units
CC ------------------------------------------------
CC      ctrcnm    : tracer name (NAMELIST)
CC      ctrcun    : tracer unit (NAMELIST)
CC      ctrcnl    : tracer long name (NAMELIST)
CC
      CHARACTER*8 ctrcnm(jptra),ctrcun(jptra)
      CHARACTER*80 ctrcnl(jptra)
CC
      COMMON/cittrc/ctrcnm,ctrcun, ctrcnl
CC
CC Common/citctl/ : parameters for the control of passive tracers
CC --------------------------------------------------------------
CC
CC      numnat    : the number of the passive tracer NAMELIST
CC      lutini    : initialisation from FILE or not (NAMELIST)
CC      nutini    : FORTRAN LOGICAL UNIT for initialisation file
CC
      INTEGER numnat
      LOGICAL lutini(jptra)
      INTEGER nutini(jptra)
      COMMON/citctl/numnat,lutini,nutini
CC
CC----------------------------------------------------------------------
CC
CC COMMON/cottrc/ : passive tracers fields (before,now,after)
CC ---------------------------------------
CC      trai      : initial total tracer
CC      trn()     : traceur concentration for actual time step
CC      trb()     : traceur concentration for before time step
CC

      REAL trn(jpi,jpj,jpk,jptra)
      REAL trb (jpi,jpj,jpk,jptra)
CC
      COMMON/cottrc/trn,trb
CC
CC----------------------------------------------------------------------
CC
CC COMMON /cot3ad/ non-centered advection scheme (smolarkiewicz)
CC -------------------------------------------------------------
CC      rsc         : tuning coefficient for anti-diffusion (NAMELIST)
CC      rtrn        : value for truncation (NAMELIST)
CC      ugmt, vgmt, wgmt : G&M velocities
CC
      REAL rsc,rtrn
CC
      COMMON/cot3ad/ rsc,rtrn
CC----------------------------------------------------------------------
CC
CC COMMON /cit3ad/ non-centered advection scheme (smolarkiewicz)
CC -------------------------------------------------------------
CC      ndttrc      : frequency of step on passive tracers (NAMELIST)
CC
      INTEGER ndttrc
CC
      COMMON/cit3ad/ ndttrc
CC
CC
CC----------------------------------------------------------------------
CC
CC COMMON/citrst/ : passive tracers restart (input and output)
CC -----------------------------------------------------------
CC      nutwrs    : output FILE for passive tracers restart
CC      lrsttr    : boolean term for restart i/o for passive tracers 
CC                  (NAMELIST)
CC      nutrst    : logical unit for restart FILE for passive tracers
CC      nrsttr    : control of the time step ( 0 or 1 ) for pass. tr.
CC                  (NAMELIST)
CC
      LOGICAL lrsttr
      CHARACTER*48 trestart
      INTEGER nutwrs,nutrst,nrsttr
CC
      COMMON/citrst/nutwrs, lrsttr, nutrst, nrsttr, trestart
CC
CC----------------------------------------------------------------------
CC
CC COMMON/citcdf/ : information for outputs
CC ------------------------------------------------------------------
CC
CC      nwritetrc: time step frequency for concentration outputs (NAMELIST)
CC      nit5     : id for tracer output FILE
CC      ndepit5  : id for depth mesh
CC      nhorit5  : id for horizontal mesh
CC      ndext50   : INTEGER arrays for ocean 3D INDEX 
CC      ndimt50   : number of ocean points in INDEX array
CC      ndext51   : INTEGER arrays for ocean surface INDEX 
CC      ndimt51   : number of ocean points in INDEX array
CC
CC    netcdf files and index common
CC
      INTEGER nwritetrc,nit5,ndepit5,nhorit5
     $       ,ndext50(jpi*jpj*jpk),ndimt50
     $       ,ndext51(jpi*jpj),ndimt51
      COMMON/citcdf/nwritetrc,nit5,ndepit5,nhorit5
     $             ,ndext50,ndimt50
     $             ,ndext51,ndimt51

      INTEGER ijulian
      COMMON/cotcdfi/ijulian

#    if defined key_trc_diaadd || defined key_trc_dia3d
#include "diagnostics.h"
CC----------------------------------------------------------------------
CC
CC COMMON/cot23d/ : additional 2D/3D outputs
CC ------------------------------------------------------------------
CC
CC      ctrc3d    : 3d output field name (NAMELIST)
CC      ctrc3l    : 3d output field long name (NAMELIST)
CC      ctrc3u    : 3d output field unit (NAMELIST)
CC      ctrc2d    : 2d output field name (NAMELIST)
CC      ctrc2l    : 2d output field long name (NAMELIST)
CC      ctrc2u    : 2d output field unit (NAMELIST)
CC      trc3d     : additional 3d outputs
CC      trc2d     : additional 2d outputs
CC
CC Christophe Menkes. This is already defined in ROMS elsewhere
CC      CHARACTER*8 ctrc3d(jpdia3d),ctrc2d(jpdia2d)
CC      CHARACTER*8 ctrc3u(jpdia3d),ctrc2u(jpdia2d)
CC      CHARACTER*80 ctrc3l(jpdia3d),ctrc2l(jpdia2d)
CC      REAL trc3d(jpi,jpj,jpk,jpdia3d), trc2d(jpi,jpj,jpdia2d)

CC      COMMON/cit23d/ctrc3d, ctrc2d, ctrc3l, ctrc2l, ctrc3u, ctrc2u  
CC      COMMON/cot23d/trc3d, trc2d
CC
CC    netcdf files and index common
CC
CC      nwriteadd: frequency of additional arrays outputs (NAMELIST)
CC      nitd     : id for additional array output FILE
CC      ndepitd  : id for depth mesh
CC      nhoritd  : id for horizontal mesh
CC
CC      INTEGER nwriteadd,nitd,ndepitd,nhoritd
CC      COMMON/citcdd/nwriteadd,nitd,ndepitd,nhoritd
CC      SAVE/citcdd/
#    endif
CC
CC----------------------------------------------------------------------
CC
CC COMMON/cotrda/ : passive tracers DATA READ and at given time_step
CC -----------------------------------------------------------------
CC      numtr1    : LOGICAL UNIT for passive tracers DATA
CC      numtr2    : LOGICAL UNIT for passive tracers DATA created IF
CC                  interpolation is needed (ninttr=1)
CC      nlectr    : switch for reading once
CC      ninttr    : switch for interpolation on model grid
CC      nmldmptr  : : = 0/1/2 type of damping in the mixed layer
CC      trcdat()  : passive tracers DATA array for two value
CC                  needed for time interpolation
CC
CC      trdta()   : passive tracers DATA at given time-step
CC      restotr() : array of restoring coeff. for passive tracers
CC
      INTEGER numtr(jptra),nlectr,ninttr,nmldmptr
     &     ,ntrc1,ntrc2,ndmptr
      REAL sdmptr,hdmptr,bdmptr
CC
      COMMON/citrda/numtr,nlectr,ninttr,nmldmptr
     &     ,ntrc1,ntrc2,ndmptr,sdmptr,hdmptr,bdmptr
CC
#    if defined key_trc_dta

      REAL trdta(jpi,jpj,jpk,jptra),tracdta(jpi,jpj,jpk,jptra,2)
      COMMON/cotrda/trdta,tracdta
#    else
CC      no passive tracers DATA at given time step
#    endif
CC
#    if defined key_trc_dmp
      REAL restotr(jpi,jpj,jpk,jptra)
CC
      COMMON/cottdp/restotr
#    endif
CC
#   if defined key_trc_pisces
#    include "common.passivetrc.pisces.h"
#   endif
#else
CC
CC no passive tracer COMMON specification
CC
#endif
