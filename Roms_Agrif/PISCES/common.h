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
CCC---------------------------------------------------------------------
CCC
CCC                         O R C A model
CCC                       ****************
CCC
CCC                            common
CCC
CCC  Purpose :
CCC  ---------
CCC     Include common file
CCC
CC   
CC
CC      comxxx for real variables or real arrays
CC      cimxxx for integer
CC
CC   Modifications :
CC   -------------
CC      original  : 91 (Delecluse, Imbard, Levy, Madec)
CC      additions : 93 (Imbard, Madec) release 7.1
CC                : 95 (Imbard) first release 8.1
CC                : 95 (M.A.Filiberti) ice
CC                : 96 (M. Imbard & G. Madec) release 8.0
CC                : 96 (A. Weaver) correction to preconditioning
CC                : 97 (M. Imbard & G. Madec) release 8.1
CC                : 98 (M. Guyon) FETI method
CC                : 98-01 (G.Madec & M. ioualalen) adding COMSPR for
CC                                                surface pressure
CC                : 98-10 (G. Roullet) free surface
CC                : 99-09 (E. Guilyardi) trends
CC                : 00-03 (G. Madec) no slip accurate
CC                : 00-07 Open Bondary Conditions
CC                  (Jean-Marc Molines, M. Imbard, CLIPPER project)
CC                : 00-08 (G. Madec) double diffusive mixing
CCC---------------------------------------------------------------------
CCC  OPA8.2, LODYC-IPSL (2001)
CCC---------------------------------------------------------------------
CC
C>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
C
C I. MODEL SET UP
C ===============
C
C<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
CC
CC----------------------------------------------------------------------
CC Common/comnam/  : namelist parameters
CC -------------------------------------
CC namrun:  parameters of the run
CC	no     	: job number
CC	cexper 	: experience name for vairmer format
CC	nrstdt 	: control of the time step (0, 1 or 2)
CC	nit000 	: number of the first time step
CC	nitend 	: number of the last time step
CC	ndate0 	: initial calendar date aammjj
CC	nbisex 	: Leap year calendar (0/1)
CC	nprint 	: level of print (0 no print)
CC	nstock 	: frequency of restart file
CC	nwrite 	: frequency of OUTPUT file
CC	nizoom 	: index i for the sub domain left bottom
CC	njzoom 	: index j for the sub domain left bottom
CC      rdt    	: time step for the dynamics (and tracer if nacc=0)
CC	eps  	: absolute precision of the solver
C
      CHARACTER (len=16) cexper      
      CHARACTER (len=48) crestart
C
      LOGICAL lrstar
C
      INTEGER no, nrstdt, nit000, nitend, ndate0, nbisex, nstock,
     $        nprint, nwrite ,
     $        nizoom, njzoom

      INTEGER jpib,jpie,jpjb,jpje
C
      REAL rdt, eps
CC
      COMMON/clmnam/ cexper, crestart, lrstar
      COMMON/cimnam/ no, nrstdt, nit000, nitend, ndate0, nbisex, nstock,
     $        nprint, nwrite , nizoom, njzoom,jpib,jpie,jpjb,jpje
C
      COMMON/comnam/rdt, eps
CC
CC----------------------------------------------------------------------
CC Common/comcst/ : physical constants
CC -----------------------------------
CC      rpi              : pi
CC      rday             : day
CC      raajj            : number of days in one year
CC      raamo            : number of months in one year
CC      rjjhh            : number of hours in one day
CC      rhhmm            : number of minutes in one hour
CC      rmmss            : number of seconds in one minute
CC      raass            : number of seconds in one year
CC      rmoss            : number of seconds in one month
CC      rjjss            : number of seconds in one day
C
      REAL rpi, rday, raajj, raamo, rjjhh, rhhmm, rmmss,
     $     raass, rmoss, rjjss
CC
      COMMON/comcst/ rpi, rday, raajj, raamo, rjjhh, rhhmm, rmmss,
     $     raass, rmoss, rjjss
CC
CC----------------------------------------------------------------------
CC Common/comstp/ : time step
CC --------------------------
CC      rdttra()         : vertical profile of tracer time step
CC
      REAL rdttra(jpk)
CC
      COMMON/comstp/ rdttra
CC
CC----------------------------------------------------------------------
CC Common/comcoh/  : horizontal curvilinear coordinate and scale factors
CC ---------------------------------------------------------------------
CC      glamt()          : longitude of t-point (degre)
CC      gphit()          : latitude  of t-point (degre)
CC      e1t,e2t()        : horizontal scale factors at t-point (m)
C
      REAL glamt(jpi,jpj)
      REAL gphit(jpi,jpj)
      REAL e1t(jpi,jpj)
      REAL e2t(jpi,jpj)
C
      COMMON/comcoh/ 
     $       glamt, gphit, 
     $       e1t, e2t
CC
CC                  z-coordinate (default option)
CC                  ------------------------------
CC      gdept(), gdepw() : depth of t- and w-points (m)
CC      e3t(), e3w()     : vertical scale factors at t- and w-points (m)
CC
      REAL fsdept(GLOBAL_2D_ARRAY,jpk),
     &     fsdepw(GLOBAL_2D_ARRAY,jpk+1),
     &     fse3t(GLOBAL_2D_ARRAY,jpk),
     &     fse3w(GLOBAL_2D_ARRAY,jpk+1)
C
      COMMON/comcoz/ fsdept, fsdepw, fse3t, fse3w
CC
CC----------------------------------------------------------------------
CC Common/comask/  : masks, bathymetry
CC -----------------------------------
CC      mbathy()         : number of ocean level (=0, 1, ... , jpk-1)
CC      tmask(), umask() : land/ocean mask at t-, u-, v- and f-points
CC      vmask()
CC      bmask()          : land/ocean mask of barotropic stream function
CC
      INTEGER mbathy(jpi,jpj)
C
      COMMON/cimask/ mbathy
C
      REAL tmask(GLOBAL_2D_ARRAY,jpk)
C
      COMMON/comask/ tmask
C
CC
CC>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
CC
CC II. DYNAMICS AND TRACERS
CC ========================
CC
CC----------------------------------------------------------------------
CC Common/comnow/  : present fields (now)
CC -------------------------------------
CC	tn(),   sn()     : pot. temperature (celsius), salinity (psu)
CC			   (no units)
CC	rhopn()          : potential volumic mass (kg m-3)
C
      REAL tn(jpi,jpj,jpk), sn(jpi,jpj,jpk)
      REAL rhopn(jpi,jpj,jpk)
C
      COMMON/comnow/ tn, sn, rhopn
CC
CC
CC>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
CC
CC III. OCEAN PHYSICS
CC ==================
CC
CC----------------------------------------------------------------------
CC Common/comzdf/ : vertical diffusion
CC -----------------------------------
CC	avs()	         : vertical diffusivity coeff. at w-point
CC			   for salinity ('key_ddmixing' defined)
C
      REAL avs(jpi,jpj,jpk)
C
      COMMON/comzdf/ avs
CC
CC----------------------------------------------------------------------
CC Common/comtau/ : surface wind stress at givem time_step
CC -------------------------------------------------------
CC	taux(), tauy()   : wind stress components in (i,j) referential
CC	tauxg(), tauyg() : zonal and meridian wind stress component used
CC	                   in output (geographical referential)
C
      REAL taux(jpi,jpj), tauy(jpi,jpj)
C
      COMMON/comtau/ taux, tauy
C
      REAL vatm(GLOBAL_2D_ARRAY)
C
      COMMON/comflr2/vatm
CC
CC----------------------------------------------------------------------
CC Common/comflx/ : surface fluxes
CC -------------------------------
CC      emp()            : evaporation minus precipitation (kg m-2 s-2)
CC      freeze()         : after and now ice mask (0 or 1)
C
      REAL emp(jpi,jpj)
      REAL freeze(jpi,jpj)
C
      COMMON/comflx/ emp, freeze
CC
CC----------------------------------------------------------------------
CC Common/comqsr/ : penetrative solar radiation
CC --------------------------------------------
CC	qsr()	         : solar radiation (w m-2)
C
      REAL qsr(GLOBAL_2D_ARRAY)
CC
      COMMON/comqsr/ qsr
CC
CC      njulian          : julian day (number of day from 1jan)
CC      nday0            : integer of previous fraction of day 
CC      nday1            : integer of current fraction of day
CC      nday             : number of iterations explain in day quanta          
CC      rdate            : model date in year/month/day/hour aammjj.hh
CC      gday0            : calandar previous fraction of day
CC      gday1            : calandar current fraction of day
CC      gday             : number of iterations explain in day quanta

C
      INTEGER njulian, njulian0
      INTEGER nday, nday0, nday1
C
      COMMON/cimrun/ njulian, njulian0, nday, nday0, nday1
CC
      REAL rdate, gday, gday0, gday1
C
      COMMON/comrun/ rdate, gday, gday0, gday1
c
c    netcdf files and index common
c
c for diawri and diabort
      INTEGER nid1,ndepid1,nhorid1
     $       ,ndex10(jpi*jpj*jpk),ndim10
     $       ,ndex11(jpi*jpj),ndim11
     $       ,nid2,ndepid2,nhorid2
     $       ,ndex20(jpi*jpj*jpk),ndim20
     $       ,ndex21(jpi*jpj),ndim21
     $       ,nid3,ndepid3,nhorid3
     $       ,ndex30(jpi*jpj*jpk),ndim30
     $       ,ndex31(jpi*jpj),ndim31
     $       ,nid4,ndepid4,nhorid4
     $       ,ndex40(jpi*jpj*jpk),ndim40
     $       ,ndex41(jpi*jpj),ndim41
      INTEGER nida,ndepida,nhorida
     $       ,ndexa(jpi*jpj*jpk),ndima
     $       ,ndexa1(jpi*jpj),ndima1
     $       ,ndex(1)
CC
CC
CC----------------------------------------------------------------------
CC Common/comdia/ : Additional diagnostics
CC ----------------------------------------
CC    hmld()             : mixing layer depth (turbocline)
CC    hmlp()             : mixed layer depth  (rho=rho0+zdcrit)
CC    hmln()             : mixed layer depth  (bn2 criteria)
C
      REAL hmld(GLOBAL_2D_ARRAY)
C
      COMMON/comdia/ hmld
C
      LOGICAL lwp
C
      COMMON/cimmpp/ lwp
CC
CC>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
CC
CC PASSIVE TRACER MODEL
CC ====================
CC
CC<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<
CC
CC----------------------------------------------------------------------
CC passive tracers Common
CC ----------------------
CC
#if defined key_passivetrc
#    include "common.passivetrc.h"
#endif
CC----------------------------------------------------------------------
