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
CCC
CCC                       trclsm.pisces.h
CCC                       ****************
CCC
CCC  PURPOSE :
CCC  ---------
CCC     READs and PRINT options for PISCES namelist
CCC
CC   METHOD :                   : no
CC   -------
CC
CC   INPUT :
CC   -----
CC            &natgas
CC            &natext
CC            &natbio           : biological parameters (#ifdef key_npzd_aumont)
CC
CC   OUTPUT :
CC   ------
CC      COMMON                  : (#ifdef "key_passivetrc")
CC           /cotgas/
CC           /cotcon/
CC           /cotbio/           :(#ifdef "key_npzd_aumont")
CC
CC   WORKSPACE :                : no
CC   ---------
CC
CC   MODIFICATIONS:
CC   --------------
CC      original  : 99-10 (M.A. Foujols, M. Levy) passive tracer
CC      addition  : 00-01 (L. Bopp) hamocc3,p3zd
CC     
CC----------------------------------------------------------------------
CC----------------------------------------------------------------------
CC local declarations
CC ==================

#if defined key_passivetrc && defined key_trc_pisces
      INTEGER iost,ilu,iused(1,100),ilseq,iband,ichl
      REAL    xtoto
      CHARACTER (len=21) :: clold,clfor,clseq,clnew,cldir,clunf
      CHARACTER (len=32) :: clname
CC
CCC---------------------------------------------------------------------
CCC  OPA8, LODYC (15/11/96)
CCC---------------------------------------------------------------------
C
C 0. initializations
C ------------------
C
       namelist/natgas/ gasfac, igaswind, icice
       namelist/natext/ atcco2
       namelist/natbio/caco3r,dispo0,conc0,oxymin,grosip
     &   ,sedlam,sedlostpoc,sedlostcal,sedlostsil,nrdttrc
     &   ,pislope, excret,wsbio,wchl,wchld,resrat,mprat,mzrat
     &   ,grazrat,xprefc,xprefp,unass,xkgraz,xkmort,xksi1,xksi2
     &   ,xremip,xremik,xsirem,xkdoc1,xkdoc2,excret2,resrat2
     &   ,mprat2,mpratm,mzrat2,grazrat2,xprefz,xprefpoc,unass2
     &   ,xkgraz2,xlam1,ferat3,conc1,conc2,conc3,concnnh4
     &   ,concdnh4,nitrif,epsher,epsher2,pislope2,wsbio2,sigma1
     &   ,sigma2,zprefc,zprefp,zprefd,fecnm,fecdm,chlcnm,chlcdm
     &   ,sedfeinput
       namelist/natsms/bdustfer, briver, bndepo, bsedinput
C
C OPEN specifier
C
      clold='old'
      clnew='new'
      clfor='formatted'
      clunf='unformatted'
      clseq='SEQUENTIAL'
      cldir='direct'
C
C SEQUENTIAL value
C
      ilseq=1
C
C initialize the number of LOGICAL UNIT used
C
      ilu=0
          WRITE(numout,*) ' '
          WRITE(numout,*) ' ROUTINE trclec'
          WRITE(numout,*) ' **************'
          WRITE(numout,*) ' '
          WRITE(numout,*) ' namelist for PISCES model'
          WRITE(numout,*) ' ***********************'
          WRITE(numout,*) ' '
C
      numnat=80
C
      clname='namelist.trc.sms'
!#ifdef AGRIF
!      IF (.Not.Agrif_Root()) clname=TRIM(clname)//'.'//Agrif_CFixed()
!#endif      
      OPEN(unit=numnat, FILE=clname, FORM=clfor, ACCESS=clseq,
     $    STATUS=clold, IOSTAT=iost)
      REWIND ( numnat )
C
C
C 1 Namelist natgas :
C
      READ(numnat,natgas)
C
          WRITE(numout,*) ' '
          WRITE(numout,*) 'natgas'
          write(numout,*) 'gasfac = ',gasfac
          WRITE(numout,*) ' '
          write(numout,*) 'igaswind = ',igaswind
          WRITE(numout,*) ' '
          write(numout,*) 'icice = ',icice
          WRITE(numout,*) ' '
C
C 2 Namelist natext :
C
      READ(numnat,natext)
C
          WRITE(numout,*) ' '
          WRITE(numout,*) 'natext'
          WRITE(numout,*) ' '
          WRITE(numout,*) 'atmospheric pCO2= ',atcco2
          WRITE(numout,*) ' '

C
      READ(numnat,natbio)
          WRITE(numout,*) 'natbio'
          WRITE(numout,*) ' '
          WRITE(numout,*)
     $        ' mean rainratio                             =', caco3r
          WRITE(numout,*)
     $        ' mean Si/C ratio                            =', grosip
          WRITE(numout,*)
     $        ' Calcite dissolution half saturation        =', dispo0
          WRITE(numout,*)
     $        ' Phosphate half saturation                  =', conc0
          WRITE(numout,*)
     $        ' Sediment bioturbation factor               =', sedlam
          WRITE(numout,*)
     $        ' Sediment burying ratio for POC         =', sedlostpoc
          WRITE(numout,*)
     $        ' Sediment burying ratio for CACO3       =', sedlostcal
          WRITE(numout,*)
     $        ' Sediment burying ratio for SI          =', sedlostsil
          WRITE(numout,*)
     $        ' frequence pour la biologie                 =', nrdttrc
          WRITE(numout,*)
     $        ' P-I slope                                  =', pislope
          WRITE(numout,*)
     $        ' excretion ratio of phytoplankton           =', excret
          WRITE(numout,*)
     $        ' POC sinking speed                          =', wsbio
          WRITE(numout,*)
     $        ' exsudation rate of zooplankton             =', resrat
          WRITE(numout,*)
     $        ' phytoplankton mortality rate               =', mprat
          WRITE(numout,*)
     $        ' zooplankton mortality rate                 =', mzrat
          WRITE(numout,*)
     $        ' zoo preference for phyto                   =', xprefc
          WRITE(numout,*)
     $        ' zoo preference for POC                     =', xprefp
          WRITE(numout,*)
     $        ' maximal zoo grazing rate                   =', grazrat
          WRITE(numout,*)
     $        ' non assimilated fraction of phyto by zoo   =', unass
          WRITE(numout,*)
     $        ' half sturation constant for grazing        =', xkgraz
          WRITE(numout,*)
     $        ' half saturation constant for mortality     =', xkmort
          WRITE(numout,*)
     $        ' half saturation constant for Si uptake     =', xksi1
          WRITE(numout,*)
     $        ' half saturation constant for Si/C          =', xksi2
          WRITE(numout,*)
     $        ' remineralisation rate of POC               =', xremip
          WRITE(numout,*)
     $        ' remineralization rate of DOC               =', xremik
          WRITE(numout,*)
     $        ' remineralization rate of Si                =', xsirem 
          WRITE(numout,*)
     $        ' 1st half-sat. of DOC remineralization      =', xkdoc1
          WRITE(numout,*)
     $        ' 2nd half-sat. of DOC remineralization      =', xkdoc2
          WRITE(numout,*)
     $        ' excretion ratio of diatoms                 =', excret2
          WRITE(numout,*)
     $        ' exsudation rate of mesozooplankton         =', resrat2
          WRITE(numout,*)
     $        ' Diatoms mortality rate                     =', mprat2
          WRITE(numout,*)
     $        ' Phytoplankton minimum mortality rate       =', mpratm
          WRITE(numout,*)
     $        ' mesozooplankton mortality rate             =', mzrat2
          WRITE(numout,*)
     $        ' zoo preference for zoo                     =', xprefz
          WRITE(numout,*)
     $        ' zoo preference for poc                   =', xprefpoc
          WRITE(numout,*)
     $        ' maximal mesozoo grazing rate               =', grazrat2
          WRITE(numout,*)
     $        ' non assimilated fraction of P by mesozoo   =', unass2
          WRITE(numout,*)
     $        ' Efficicency of Mesozoo growth              =', epsher2 
          WRITE(numout,*)
     $        ' Efficiency of microzoo growth              =', epsher
          WRITE(numout,*)
     $        ' half sturation constant for grazing 2      =', xkgraz2
          WRITE(numout,*)
     $        ' Maximum aggregation rate for diatoms       =', wchld
          WRITE(numout,*)
     $        ' scavenging rate of Iron                    =', xlam1
          WRITE(numout,*)
     $        ' Fe/C in zooplankton                        =', ferat3
          WRITE(numout,*)
     $        ' Phosphate half saturation for diatoms      =', conc1
          WRITE(numout,*)
     $        ' Iron half saturation for phyto             =', conc2
          WRITE(numout,*)
     $        ' Iron half saturation for diatoms           =', conc3
          WRITE(numout,*)
     $        ' NH4 half saturation for phyto              =', concnnh4
          WRITE(numout,*)
     $        ' NH4 half saturation for diatoms            =', concdnh4
          WRITE(numout,*)
     $        ' NH4 nitrification rate                     =', nitrif
          WRITE(numout,*)
     $        ' P-I slope  for diatoms                     =', pislope2
          WRITE(numout,*)
     $        ' Big particles sinking speed                =', wsbio2
          WRITE(numout,*)
     $        ' Fraction of microzoo excretion as DOM      =', sigma1
          WRITE(numout,*)
     $        ' Fraction of mesozoo excretion as DOM       =', sigma2
          WRITE(numout,*)
     $        ' Microzoo preference for POM                =', zprefc
          WRITE(numout,*)
     $        ' Microzoo preference for Nanophyto          =', zprefp
          WRITE(numout,*)
     $        ' Microzoo preference for Diatoms            =', zprefd
          WRITE(numout,*)
     $        ' Minimum Chl/C in nanophytoplankton         =', chlcnm
          WRITE(numout,*)
     $        ' Minimum Chl/C in diatoms                   =', chlcdm
          WRITE(numout,*)
     $        ' Maximum Fe/C in nanophytoplankton          =', fecnm
          WRITE(numout,*)
     $        ' Minimum Fe/C in diatoms                    =', fecdm
          WRITE(numout,*)
     $        ' Coastal release of Iron                 =', sedfeinput

      READ(numnat,natsms)
          WRITE(numout,*) ' '
          WRITE(numout,*) 'natsms'
          WRITE(numout,*) ' '
          WRITE(numout,*) 'Dust input from the atmosphere : ', bdustfer
          WRITE(numout,*) ' '
          WRITE(numout,*) 'River input of nutrients : ', briver
          WRITE(numout,*) ' '
          WRITE(numout,*) 'Atmospheric deposition of N : ', bndepo
          WRITE(numout,*) ' '
          WRITE(numout,*) 'Fe input from sediments : ', bsedinput
          WRITE(numout,*) ' '
       CLOSE(numnat)

C
C  FROM THE NEW BIOOPTIC MODEL PROPOSED JM ANDRE, WE READ HERE
C  A PRECOMPUTED ARRAY CORRESPONDING TO THE ATTENUATION COEFFICIENT
C  ----------------------------------------------------------------
C
         open(49,file='kRGB61.txt',form='formatted')
         do ichl=1,61
           READ(49,*) xtoto,(xkrgb(iband,ichl),iband = 1,3)
         end do
         close(49)

#else
C
C no passive tracers
C
#endif
