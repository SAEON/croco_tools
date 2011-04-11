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
CCC                         COMMON passivetrc.pisces.h
CCC                       ******************************
CCC
CCC  purpose :
CCC  ---------
CCC     INCLUDE COMMON FILE for PISCES biological model
CCC
CCC  modifications :
CC   -------------
CC      original    : 00-02 (O. Aumont)
CC
CCC---------------------------------------------------------------------
CCC  opa8, ipsl (11/96)
CCC---------------------------------------------------------------------
CC
#if defined key_trc_pisces
CC
CC----------------------------------------------------------------------
CC
CC COMMON/cchem1/ : Variable for chemistry of the CO2 cycle
CC
CC ---------------------------------------------------------------------
CC
      REAL akb3(jpi,jpj,jpk), ak13(jpi,jpj,jpk), ak23(jpi,jpj,jpk)
      REAL aksp(jpi,jpj,jpk), co3(jpi,jpj,jpk), hi(jpi,jpj,jpk)
      REAL borat(jpi,jpj,jpk), akw3(jpi,jpj,jpk), h2co3(jpi,jpj)
      REAL akp13(jpi,jpj,jpk), akp23(jpi,jpj,jpk), akp33(jpi,jpj,jpk)
      REAL aksi3(jpi,jpj,jpk)
      REAL fugaci(jpi,jpj), atcco2, atcox
C
      COMMON/cchem1/ akb3,ak13,ak23,aksp,co3,hi,borat,akw3,h2co3,
     &      fugaci, atcco2, atcox, akp13, akp23, akp33, aksi3
CC
CC----------------------------------------------------------------------
CC
CC COMMON/cchem2/ : Variable for chemistry of the CO2 cycle
CC
CC ---------------------------------------------------------------------
CC
      REAL akcc1, akcc2, akcc3, akcc4, devk1(7), devk2(7)
      REAL devk3(7), devk4(7), devk5(7), devkst, devks
      REAL bor1, bor2, c00, c01, c02, c03, c04, c05, c10, c11
      REAL c12, c13, c20, c21, c22, c23, cb0, cb1, cb2, cb3
      REAL cb4, cb5, cb6, cb7, cb8, cb9, cb10, cb11, c14
      REAL cw3, cw4, cw5, cw6, cw0, cw1, cw2, ox0, ox1
      REAL ox2, ox3, ox4, salchl, rgas, oxyco, ox5
      REAL ca0, ca1, ca2, ca3, ca4, ca5, ca6, chemc(jpi,jpj,3)
      REAL cp10, cp11, cp12, cp13, cp14, cp15, cp16, cp20, cp21
      REAL cp22, cp23, cp24, cp25, cp26, cp30, cp31, cp32, cp33
      REAL cp34, cp35, cs10, cs11, cs12, cs13, cs14, cs15, cs16
      REAL cs17, cs18, cs19
C
      COMMON/cchem2/ akcc1, akcc2, akcc3, akcc4, devk1, devk2
     &, devk3, devk4, devk5, devkst, devks
     &, bor1, bor2, c00, c01, c02, c03, c04, c05, c10, c11
     &, c12, c13, c20, c21, c22, c23, cb0, cb1, cb2, cb3
     &, c14, cb4, cb5, ca0, ca1, ca2, ca3, ca4, ca5, ca6
     &, cb6, cb7, cb8, cb9, cb10, cb11,  cw3, cw4, cw5, cw6
     &, cw0, cw1, cw2, ox0, ox1, ox2, ox3, ox4, ox5, salchl
     &, cp10, cp11, cp12, cp13, cp14, cp15, cp16, cp20, cp21
     &, cp22, cp23, cp24, cp25, cp26, cp30, cp31, cp32, cp33
     &, cp34, cp35, cs10, cs11, cs12, cs13, cs14, cs15, cs16
     &, cs17, cs18, cs19, rgas, oxyco, chemc
CC
CC ---------------------------------------------------------------------
CC COMMON/cchem3/ : Variable for chemistry of Fe and SIO3
CC
CC ---------------------------------------------------------------------
CC
      REAL sio3eq(jpi,jpj,jpk),fekeq(jpi,jpj,jpk)
C
      COMMON/cchem3/ sio3eq, fekeq
CC
CC----------------------------------------------------------------------
CC
CC COMMON/cotsedim/ : Variable for simplified sediments
CC
CC ---------------------------------------------------------------------
CC
      REAL sedpoc(jpi,jpj),sedcal(jpi,jpj),sedsil(jpi,jpj)
      REAL sedlam,sedlostpoc, sedlostsil,sedlostcal
C
      COMMON/cotsedim/sedpoc,sedcal,sedsil,sedlam,
     &                sedlostpoc, sedlostsil,sedlostcal
CC
CC----------------------------------------------------------------------
CC
CC COMMON/cotcon/ : Time variables
CC
CC ---------------------------------------------------------------------
CC
C
      INTEGER       iabsyr, nrdttrc, ndayflxtr
      REAL          tspyr, absyr, xtvit
      REAL rfact, rfactr, rfact2, rfact2r
C
      COMMON/cottim_int/ iabsyr, nrdttrc, ndayflxtr
      COMMON/cottim/ tspyr, absyr
     &, rfact, rfactr, xtvit, rfact2, rfact2r
C
CC
CC----------------------------------------------------------------------
CC
CC COMMON/cotgas/ : Gas exchange
CC
CC ---------------------------------------------------------------------
CC
      INTEGER igaswind,icice
      REAL gasfac, strn(jpi,jpj)
      REAL kgwanin(jpi,jpj)
      REAL wsmo(jpi,jpj,12), kgwanmo(jpi,jpj,12)
      REAL cicemo(jpi,jpj,12),qcumul(jptra)
      REAL patm(jpi,jpj),ppres(jpi,jpj,12)
C
      COMMON/cotgas/ gasfac, igaswind, icice
     & ,kgwanin, wsmo, kgwanmo, cicemo
     & ,patm,ppres,strn,qcumul
C
CC---------------------------------------
CC
CC COMMON/cotham/ : biological parameters 
CC
CC --------------------------------------
CC 
      REAL caco3r, rno3, o2ut, po4r, xsirem
      REAL sco2, dispo0, conc0,sumdepsi,rivalkinput,sedfeinput
      REAL calcon, rivpo4input,nitdepinput,oxymin
      REAL nitrif,rdenit,o2nit,concnnh4,concdnh4
      REAL pislope,excret,wsbio,wchl,resrat,mprat, wchld
      REAL mzrat,grazrat,xprefc,xprefp,unass,xkgraz,xkmort
      REAL xksi1,xksi2,xremik,xremip,xkdoc1
      REAL xkdoc2,grosip,resrat2,excret2,mprat2,mzrat2,xprefz
      REAL xkgraz2,grazrat2,xlam1,conc1,conc2,conc3
      REAL unass2,xprefpoc,epsher,epsher2,pislope2,mpratm
      REAL sigma1, sigma2, zprefc, zprefp, zprefd, ferat3
      REAL fecnm, fecdm, chlcnm, chlcdm
C
      COMMON/cotham/caco3r, rno3, o2ut, po4r
     &  ,sco2, dispo0, conc0,sumdepsi,rivalkinput,sedfeinput
     &  ,calcon, rivpo4input,nitdepinput,oxymin
     &  ,nitrif,rdenit,o2nit,concnnh4,concdnh4,pislope
     &  ,excret,wsbio,wchl,resrat,mprat, xsirem, wchld
     &  ,mzrat,grazrat,xprefc,xprefp,unass,xkgraz,xkmort
     &  ,xksi1,xksi2,xremik,xremip,xkdoc1
     &  ,xkdoc2,grosip,resrat2,excret2,mprat2,mzrat2,xprefz
     &  ,xkgraz2,grazrat2,xlam1,conc1,conc2,conc3,unass2
     &  ,xprefpoc,epsher,epsher2,pislope2,mpratm,sigma1
     &  ,sigma2, zprefc, zprefp, zprefd, ferat3, fecnm
     &  , fecdm, chlcnm, chlcdm
CC
CC---------------------------------------------
CC
CC COMMON/cotpar/ : Biological fluxes for light
CC
CC---------------------------------------------
CC
      REAL etot(jpi,jpj,jpk),emoy(jpi,jpj,jpk)
      REAL xkrgb(3,61),zmeu(jpi,jpj)
      INTEGER zmeuindex(jpi,jpj)

      COMMON/cotpar/etot,emoy,xkrgb,zmeu,zmeuindex
CC
CC----------------------------------------------------------
CC
CC COMMON/cotppp/ : Biological fluxes for primary production
CC
CC----------------------------------------------------------
CC
      REAL prmax(jpi,jpj,jpk),tgfunc(jpi,jpj,jpk)
      REAL prcaca(jpi,jpj,jpk), prorca(jpi,jpj,jpk)
      REAL prorca2(jpi,jpj,jpk),prorca3(jpi,jpj,jpk)
      REAL prorca4(jpi,jpj,jpk),prorca5(jpi,jpj,jpk)
      REAL prorca6(jpi,jpj,jpk),prorca7(jpi,jpj,jpk)
      REAL pronew(jpi,jpj,jpk),pronew2(jpi,jpj,jpk)
      REAL proreg(jpi,jpj,jpk),proreg2(jpi,jpj,jpk)
      REAL xnanono3(jpi,jpj,jpk),xdiatno3(jpi,jpj,jpk)
      REAL xnanonh4(jpi,jpj,jpk),xdiatnh4(jpi,jpj,jpk)
      REAL xlimphy(jpi,jpj,jpk),xlimdia(jpi,jpj,jpk)
      REAL xksimax(jpi,jpj), xksi(jpi,jpj)
      REAL concdfe(jpi,jpj,jpk),xlimdia2(jpi,jpj,jpk)
      REAL concnfe(jpi,jpj,jpk),znegtr(jpi,jpj,jpk)
      REAL tgfunc2(jpi,jpj,jpk)
C
      common/cotppp/prmax,tgfunc,prcaca,prorca,prorca2
     &   ,prorca3,prorca4,prorca5,prorca6,prorca7,pronew
     &   ,pronew2,proreg,proreg2,xnanono3,xdiatno3
     &   ,xnanonh4,xdiatnh4,xlimphy,xlimdia,xksimax,xksi
     &   ,concdfe,xlimdia2,concnfe,znegtr
     &   ,tgfunc2
CC
CC------------------------------------------
CC
CC COMMON/cotmorp/ : sinks for phytoplankton
CC
CC------------------------------------------
CC
      REAL tortp(jpi,jpj,jpk),tortnf(jpi,jpj,jpk)
      REAL tortnch(jpi,jpj,jpk),respp(jpi,jpj,jpk)
      REAL respnch(jpi,jpj,jpk),respdch(jpi,jpj,jpk)
      REAL tortp2(jpi,jpj,jpk),tortdf(jpi,jpj,jpk)
      REAL tortdch(jpi,jpj,jpk),respp2(jpi,jpj,jpk)
      REAL tortds(jpi,jpj,jpk),respds(jpi,jpj,jpk)
      REAL respdf(jpi,jpj,jpk),respnf(jpi,jpj,jpk)
C
      COMMON/cotmorp/tortp,tortnf,tortnch,respp,respnch
     &   ,respdch,tortp2,tortdf,tortdch,respp2,tortds
     &   ,respds,respdf,respnf
CC
CC------------------------------------
CC
CC COMMON/cotzoo/ : SMS for zooplankton
CC
CC-------------------------------------
CC
      REAL respz(jpi,jpj,jpk),tortz(jpi,jpj,jpk)
      REAL grazp(jpi,jpj,jpk),grazpf(jpi,jpj,jpk)
      REAL grazpch(jpi,jpj,jpk),grazm(jpi,jpj,jpk)
      REAL grazmf(jpi,jpj,jpk),grazsd(jpi,jpj,jpk)
      REAL grazsf(jpi,jpj,jpk),grazss(jpi,jpj,jpk)
      REAL grazsch(jpi,jpj,jpk),grarem(jpi,jpj,jpk)
      REAL grafer(jpi,jpj,jpk),respz2(jpi,jpj,jpk)
      REAL tortz2(jpi,jpj,jpk),grazd(jpi,jpj,jpk)
      REAL grazz(jpi,jpj,jpk),grazn(jpi,jpj,jpk)
      REAL grazpoc(jpi,jpj,jpk),graznf(jpi,jpj,jpk)
      REAL graznch(jpi,jpj,jpk),grazs(jpi,jpj,jpk)
      REAL grazf(jpi,jpj,jpk),grazdch(jpi,jpj,jpk)
      REAL grazpof(jpi,jpj,jpk),grarem2(jpi,jpj,jpk)
      REAL grafer2(jpi,jpj,jpk),grapoc2(jpi,jpj,jpk)
      REAL grapoc(jpi,jpj,jpk)
      REAL grazffe(jpi,jpj,jpk),grazfff(jpi,jpj,jpk)
C
      COMMON/cotzoo/respz,tortz,grazp,grazpf,grazpch,grazm
     &   ,grazmf,grazsd,grazsf,grazss,grazsch,grarem,grafer
     &   ,respz2,tortz2,grazd,grazz,grazn,grazpoc,graznf
     &   ,graznch,grazs,grazf,grazdch,grazpof,grarem2
     &   ,grafer2,grapoc2,grapoc,grazffe,grazfff
CC
CC---------------------------------------------
CC
CC COMMON/cotpdom/ : SMS for the organic matter
CC
CC---------------------------------------------
CC
      REAL sinking2(jpi,jpj,jpk+1),phymoy(jpi,jpj)
      REAL sinking(jpi,jpj,jpk+1),sinkfer(jpi,jpj,jpk+1)
      REAL sinkfer2(jpi,jpj,jpk+1),zdiss(jpi,jpj,jpk)
      REAL xagg(jpi,jpj,jpk),xaggfe(jpi,jpj,jpk)
      REAL xaggdoc(jpi,jpj,jpk)
      REAL xaggdfe(jpi,jpj,jpk),xbactfer(jpi,jpj,jpk)
      REAL xscave(jpi,jpj,jpk),olimi(jpi,jpj,jpk)
      REAL orem(jpi,jpj,jpk),orem2(jpi,jpj,jpk)
      REAL ofer(jpi,jpj,jpk),ofer2(jpi,jpj,jpk)
      REAL osil(jpi,jpj,jpk),xaggdoc2(jpi,jpj,jpk)
      REAL wsbio4(jpi,jpj,jpk),wsbio3(jpi,jpj,jpk),wsbio2
      REAL sinksil(jpi,jpj,jpk+1),sinkcal(jpi,jpj,jpk+1)
      REAL nitrfac(jpi,jpj,jpk),xlimbac(jpi,jpj,jpk)
      REAL wscal(jpi,jpj,jpk)
C
      COMMON/cotpdom/sinking2,phymoy,sinking,sinkfer,sinkfer2
     &   ,xagg,xaggfe,xaggdoc,xaggdfe,xbactfer,xscave
     &   ,olimi,orem,orem2,ofer,ofer2,osil,xaggdoc2,wsbio4
     &   ,wsbio3,wsbio2,sinksil,sinkcal,nitrfac,xlimbac,zdiss
     &   ,wscal
CC
CC---------------------------------------------------------
CC
CC COMMON/cotesms/ : external sources of nutrients in ocean
CC
CC---------------------------------------------------------
CC
      REAL onitr(jpi,jpj,jpk),denitr(jpi,jpj,jpk)
      REAL dust(jpi,jpj),dustmo(jpi,jpj,12)
      REAL cmask(jpi,jpj,jpk),areacot,cotdep(jpi,jpj)
      REAL ironsed(jpi,jpj,jpk),rivinp(jpi,jpj)
      REAL nitdep(jpi,jpj)
      LOGICAL bdustfer, briver, bndepo, bsedinput
C
      COMMON/cotesms/onitr,denitr,dust,dustmo,cmask,areacot
     &   ,cotdep,nitdep,ironsed,rivinp,bdustfer,briver
     &   ,bndepo,bsedinput
C
#endif
C
