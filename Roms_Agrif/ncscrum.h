!
! $Id: ncscrum.h,v 1.17 2005/10/11 12:37:02 pmarches Exp $
!
! This is include file "ncscrum.h".
! ==== == ======= ==== ============
! indices in character array "vname", which holds variable names
!                                                and attributes.
! indxTime        time
! indxZ           free-surface
! indxUb,indxVb   vertically integrated 2D U,V-momentum components
!
! indxU,indxV     3D U- and V-momenta.
! indxT,indxS,.., indxZoo  tracers (temperature, salinity,
!                 biological tracers.
! indxsand,silt   sand & silt sediment tracers
! indxO,indeW     omega vertical mass flux and true vertical velocity
! indxR           density anomaly
!
! indxVisc        Horizontal viscosity coefficients
! indxAkv,indxAkt,indxAks  vertical viscosity/diffusivity coefficients
! indxHbl         depth of planetary boundary layer in KPP model
! indxHbbl        depth of bottom planetary boundary layer in KPP model
! indxHel         depth of euphotic layer
! indxChC         Chlorophyll/Carbon ratio
!
! indxSSH         observed sea surface height (from climatology)
! indxSUSTR,indxSVSTR  surface U-, V-momentum stress (wind forcing)
! indxSHFl        net surface heat flux.
! indxSWRad       shortwave radiation flux
! indxSST         sea surface temperature
! indxdQdSST      Q-correction coefficient dQdSST
! indxSSS         sea surface salinity
! indxSSFl        surface fresh water flux
! indxSSFl        surface fresh water flux
!
! indxAi          fraction of cell covered by ice
! indxUi,indxVi   U,V-components of sea ice velocity
! indxHi,indxHS   depth of ice cover and depth of snow cover
! indxTIsrf       temperature of ice surface
!
! indxBSD,indxBSS bottom sediment grain Density and Size 
!                 to be read from file if(!defined ANA_BSEDIM, 
!                 && !defined SEDIMENT) 
!
! indxBTHK,       sediment bed thickness, porosity, size class fractions 
! indxBPOR,indxBFRA
!
! indxWWA,indxWWD,indxWWP   wind induced wave Amplitude,
!                 Direction and Period
!
      integer indxTime, indxZ, indxUb, indxVb
      parameter (indxTime=1, indxZ=2, indxUb=3, indxVb=4)
#ifdef SOLVE3D
      integer indxU, indxV, indxT
      parameter (indxU=5, indxV=6, indxT=7)

# ifdef SALINITY
      integer indxS
      parameter (indxS=indxT+1)
# endif
# ifdef PASSIVE_TRACER
      integer indxTPAS
      parameter (indxTPAS=indxT+ntrc_salt+1)
# endif
# ifdef BIOLOGY
#  ifdef PISCES
      integer indxDIC, indxTAL, indxOXY, indxCAL, indxPO4,
     &        indxPOC, indxSIL, indxPHY, indxZOO, indxDOC,
     &        indxDIA, indxMES, indxBSI, indxFER, indxBFE,
     &        indxGOC, indxSFE, indxDFE, indxDSI, indxNFE,
     &        indxNCH, indxDCH, indxNO3, indxNH4
      parameter (indxDIC =indxT+ntrc_salt+ntrc_pas+1,
     &           indxTAL =indxDIC+1, indxOXY=indxDIC+2,
     &           indxCAL=indxDIC+3, indxPO4=indxDIC+4,
     &           indxPOC=indxDIC+5, indxSIL=indxDIC+6,
     &           indxPHY =indxDIC+7, indxZOO=indxDIC+8,
     &           indxDOC =indxDIC+9, indxDIA=indxDIC+10,
     &           indxMES =indxDIC+11, indxBSI=indxDIC+12,
     &           indxFER =indxDIC+13, indxBFE=indxDIC+14,
     &           indxGOC =indxDIC+15, indxSFE=indxDIC+16,
     &           indxDFE =indxDIC+17, indxDSI=indxDIC+18,
     &           indxNFE =indxDIC+19, indxNCH=indxDIC+20,
     &           indxDCH =indxDIC+21, indxNO3=indxDIC+22,
     &           indxNH4 =indxDIC+23)
#  elif defined BIO_NChlPZD
      integer indxNO3, indxChla,
     &        indxPhy1,indxZoo1,
     &        indxDet1
#   ifdef OXYGEN
     &      , indxO2
#   endif
      parameter (indxNO3 =indxT+ntrc_salt+ntrc_pas+1,
     &           indxChla=indxNO3+1,
     &           indxPhy1=indxNO3+2,
     &           indxZoo1=indxNO3+3,
     &           indxDet1=indxNO3+4)
#   ifdef OXYGEN
      parameter (indxO2=indxNO3+5)
#   endif
#  elif defined BIO_N2PZD2
      integer indxNO3, indxNH4, indxChla,
     &        indxPhy1, indxZoo1,
     &        indxDet1, indxDet2
      parameter (indxNO3 =indxT+ntrc_salt+ntrc_pas+1,
     &           indxNH4 =indxNO3+1, indxChla=indxNO3+2,
     &           indxPhy1=indxNO3+3,
     &           indxZoo1=indxNO3+4,
     &           indxDet1=indxNO3+5, indxDet2=indxNO3+6)
#  elif defined BIO_N2P2Z2D2
      integer indxNO3, indxNH4,
     &        indxPhy1, indxPhy2, indxZoo1,
     &        indxZoo2, indxDet1, indxDet2
#   ifdef VAR_CHL_C
     &      , indxChl1, indxChl2
#   endif
      parameter (indxNO3 =indxT+ntrc_salt+ntrc_pas+1,
     &           indxNH4 =indxNO3+1,
#   ifdef VAR_CHL_C
     &           indxChl1=indxNO3+2, indxChl2=indxNO3+3,
     &           indxPhy1=indxNO3+4, indxPhy2=indxNO3+5,
     &           indxZoo1=indxNO3+6, indxZoo2=indxNO3+7,
     &           indxDet1=indxNO3+8, indxDet2=indxNO3+9)
#   else
     &           indxPhy1=indxNO3+2, indxPhy2=indxNO3+3,
     &           indxZoo1=indxNO3+4, indxZoo2=indxNO3+5,
     &           indxDet1=indxNO3+6, indxDet2=indxNO3+7)
#   endif
#  endif
# endif /* BIOLOGY */
# ifdef SEDIMENT
      integer indxsand, indxsilt
      parameter (indxsand=indxT+ntrc_salt+ntrc_pas+ntrc_bio+1, 
     &           indxsilt=indxsand+1)
# endif

# ifdef DIAGNOSTICS_TS
      integer indxTXadv,indxTYadv,indxTVadv, 
     &        indxTHmix,indxTVmix,indxTbody,indxTrate
      parameter (indxTXadv=indxT+ntrc_salt+ntrc_pas+ntrc_bio+ntrc_sed+1,
     &           indxTYadv=indxTXadv+NT,
     &           indxTVadv=indxTYadv+NT,
     &           indxTHmix=indxTVadv+NT,
     &           indxTVmix=indxTHmix+NT, 
     &           indxTbody=indxTVmix+NT,
     &           indxTrate=indxTbody+NT)
# endif
# ifdef DIAGNOSTICS_UV
      integer indxMXadv,indxMYadv,indxMVadv,indxMCor,
     &        indxMPrsgrd,indxMHmix,indxMVmix,indxMrate
      parameter (indxMXadv=indxT+ntrc_salt+ntrc_pas+ntrc_bio+ntrc_sed
     &                                                  +ntrc_diats+1,
     &           indxMYadv=indxMXadv+2,
     &           indxMVadv=indxMYadv+2,
     &           indxMCor=indxMVadv+2,
     &           indxMPrsgrd=indxMCor+2,
     &           indxMHmix=indxMPrsgrd+2,
     &           indxMVmix=indxMHmix+2, 
     &           indxMrate=indxMVmix+2)
# endif
# if defined BIOLOGY && defined DIAGNOSTICS_BIO
      integer indxbioFlux, indxbioVSink
#  ifdef OXYGEN
     &        , indxGasExcFlux
#  endif
      parameter (indxbioFlux=indxT+ntrc_salt+ntrc_pas+ntrc_bio+ntrc_sed
     &                                        +ntrc_diats+ntrc_diauv+1)
      parameter (indxbioVSink=indxbioFlux+NumFluxTerms)
#  ifdef OXYGEN
      parameter (indxGasExcFlux=indxbioFlux+NumFluxTerms+NumGasExcTerms)
#  endif
# endif /* BIOLOGY && DIAGNOSTICS_BIO */

      integer indxO, indxW, indxR, indxVisc, indxAkv, indxAkt
      parameter (indxO=indxT+ntrc_salt+ntrc_pas+ntrc_bio+ntrc_sed
     &                       +ntrc_diats+ntrc_diauv+ntrc_diabio+1,
     &           indxW=indxO+1, indxR=indxO+2, indxVisc=indxR+1,
     &           indxAkv=indxR+2, indxAkt=indxAkv+1)

# ifdef SALINITY
      integer indxAks
      parameter (indxAks=indxAkt+1)
# endif
# ifdef LMD_SKPP
      integer indxHbl
      parameter (indxHbl=indxAkt+2)
# endif
# ifdef LMD_BKPP
      integer indxHbbl
      parameter (indxHbbl=indxAkt+3)
# endif
#endif

      integer indxSSH
#ifdef BIOLOGY
      integer indxHel
# ifdef BIO_NPZD
     &      , indxChC
#  ifdef OXYGEN
     &      , indxU10, indxKvO2, indxO2sat
#  endif
# elif defined BIO_N2P2Z2D2 && defined VAR_CHL_C
     &      , indxChC1, indxChC2
# endif
#endif /* BIOLOGY*/
#ifdef SOLVE3D
# ifdef BIOLOGY
      parameter (indxHel=indxAkt+4)
#  ifdef BIO_NPZD
      parameter (indxChC=indxHel+1)
#   ifdef OXYGEN
      parameter (indxU10=indxChC+1)
      parameter (indxKvO2=indxU10+1)
      parameter (indxO2sat=indxKvO2+1)
      parameter (indxSSH=indxO2sat+1)
#   else
      parameter (indxSSH=indxChC+1)
#   endif
#  elif defined BIO_N2P2Z2D2
#   if defined VAR_CHL_C
      parameter (indxChC1=indxHel+1)
      parameter (indxChC2=indxChC1+1)
      parameter (indxSSH=indxChC2+1)
#   else
      parameter (indxSSH=indxHel+1)
#   endif
#  else
      parameter (indxSSH=indxHel+1)
#  endif
# else
      parameter (indxSSH=indxAkt+4)
# endif
#else
# ifdef BIOLOGY
      parameter (indxHel=indxVb+1)
#  ifdef BIO_NPZD
      parameter (indxChC=indxHel+1)
#   ifdef OXYGEN
      parameter (indxU10=indxChC+1)
      parameter (indxKvO2=indxU10+1)
      parameter (indxO2sat=indxKvO2+1)
      parameter (indxSSH=indxO2sat+1)
#   else
      parameter (indxSSH=indxChC+1)
#   endif
#  elif defined BIO_N2P2Z2D2
#   ifdef VAR_CHL_C
      parameter (indxChC1=indxHel+1)
      parameter (indxChC2=indxChC1+1)
      parameter (indxSSH=indxChC2+1)
#   else
      parameter (indxSSH=indxHel+1)
#   endif
#  endif
# else
      parameter (indxSSH=indxVb+1)
# endif
#endif /* SOLVE3D */
      integer indxSUSTR, indxSVSTR
      parameter (indxSUSTR=indxSSH+1, indxSVSTR=indxSSH+2)
#ifdef SOLVE3D
      integer indxSHFl, indxSWRad
      parameter (indxSHFl=indxSSH+3)
# ifdef SALINITY
      integer indxSSFl
      parameter (indxSSFl=indxSHFl+1, indxSWRad=indxSHFl+2)
# else
      parameter (indxSWRad=indxSHFl+1)
# endif
      integer indxSST, indxdQdSST
      parameter (indxSST=indxSWRad+1, indxdQdSST=indxSWRad+2)
# if defined SALINITY && defined SFLX_CORR
      integer indxSSS
      parameter (indxSSS=indxSST+2)
# endif
# if defined BULK_FLUX
      integer indxWSPD,indxTAIR,indxRHUM,indxRADLW,indxRADSW,
     &        indxPRATE,indxUWND,indxVWND
      parameter (indxWSPD=indxSST+3,  indxTAIR=indxSST+4,
     &           indxRHUM=indxSST+5,  indxRADLW=indxSST+6,
     &           indxRADSW=indxSST+7, indxPRATE=indxSST+8,
     &           indxUWND=indxSST+9,  indxVWND=indxSST+10)
# endif
#endif
      integer indxBostr
      parameter (indxBostr=indxSUSTR+18)
#ifdef SOLVE3D
# ifdef SEDIMENT
      integer indxSed, indxBTHK, indxBPOR, indxBFRA
      parameter (indxSed=indxSUSTR+19,
     &           indxBTHK=indxSed, indxBPOR=indxSed+1,
     &           indxBFRA=indxSed+2)
# endif
# ifdef BBL
      integer indxBBL, indxAbed, indxHrip, indxLrip, indxZbnot, 
     &        indxZbapp, indxBostrw
#  ifdef SEDIMENT
      parameter (indxBBL=indxSUSTR+21+NST,
#  else
      parameter (indxBBL=indxSUSTR+21, 
#  endif
     &           indxAbed  =indxBBL,   indxHrip  =indxBBL+1,
     &           indxLrip  =indxBBL+2, indxZbnot =indxBBL+3, 
     &           indxZbapp =indxBBL+4, indxBostrw=indxBBL+5)
#  ifndef ANA_WWAVE
      integer indxWWA,indxWWD,indxWWP
      parameter (indxWWA=indxBBL+6, indxWWD=indxWWA+1, 
     &           indxWWP=indxWWA+2)
#  endif
#  ifndef ANA_BSEDIM
#  endif
# endif /* BBL */
#endif /* SOLVE3D */
#ifdef ICE
      integer indxAi
      parameter (indxAi=????)
      integer indxUi, indxVi, indxHi, indxHS, indxTIsrf
      parameter (indxUi=indxAi+1, indxVi=indxAi+2, indxHi=indxAi+3,
     &                         indxHS=indxAi+4, indxTIsrf=indxAi+5)
#endif


!
! Grid Type Codes:  r2dvar....w3hvar are codes for array types.
! ==== ==== ======  The codes are set according to the rule:
!                     horiz_grid_type+4*vert_grid_type
!    where horiz_grid_type=0,1,2,3 for RHO-,U-,V-,PSI-points
!    respectively and vert_grid_type=0 for 2D fields; 1,2 for
!    3D-RHO- and W-vertical points.
!
      integer r2dvar, u2dvar, v2dvar, p2dvar, r3dvar,
     &                u3dvar, v3dvar, p3dvar, w3dvar, b3dvar
      parameter (r2dvar=0, u2dvar=1, v2dvar=2, p2dvar=3,
     & r3dvar=4, u3dvar=5, v3dvar=6, p3dvar=7, w3dvar=8,b3dvar=12) 

!            Horizontal array dimensions in netCDF files.
! xi_rho     WARNING!!! In MPI code in the case of PARALLEL_FILES 
! xi_u       _and_ NON-Periodicity in either XI- or ETA-direction,
! eta_rho    these depend on corresonding MPI-node indices ii,jj
! eta_v      and therefore become live variables, which are placed
!            into common block below rather than defined here as
!            parameters. 

      integer xi_rho,xi_u, eta_rho,eta_v    
#ifndef AGRIF
# if defined MPI && defined PARALLEL_FILES
#  ifdef EW_PERIODIC
      parameter (xi_rho=Lm,     xi_u=Lm)
#  endif
#  ifdef NS_PERIODIC
      parameter (eta_rho=Mm,    eta_v=Mm)
#  endif
# else
      parameter (xi_rho=LLm+2,  xi_u=xi_rho-1,
     &           eta_rho=MMm+2, eta_v=eta_rho-1)
# endif
#else
# if defined MPI && defined PARALLEL_FILES
#  ifdef EW_PERIODIC
      common/netCDFhorizdim1/xi_rho,xi_u
#  endif
#  ifdef NS_PERIODIC
      common/netCDFhorizdim2/eta_rho,eta_v
#  endif
# else
      common/netCDFhorizdim/xi_rho,xi_u, eta_rho,eta_v
# endif
#endif /* AGRIF */
!
! Naming conventions for indices, variable IDs, etc...
!
! prefix ncid_  means netCDF ID for netCDF file
!        nrec_  record number in netCDF file since initialization
!        nrpf_  maximum number of records per file  (output netCDF
!                                                       files only)
! prefix/ending rst_/_rst refers to restart  netCDF file
!               his_/_his           history
!               avg_/_avg           averages
!                    _frc           forcing
!                    _clm           climatology
!
! endings refer to:  ___Time  time [in seconds]
!                    ___Tstep time step numbers and record numbers
!   all objects      ___Z     free-surface
!   with these       ___Ub    vertically integrated 2D U-momentum
!   endings are      ___Vb    vertically integrated 2D V-momentum
!   either
!     netCDF IDs,    ___U     3D U-momentum
!     if occur with  ___V     3D V-momentum
!     prefices rst/  ___T(NT) tracers
!     /his/avg       ___R     density anomaly
!   or               ___O     omega vertical velocity
!     parameter      ___W     true vertical velocity
!     indices, if
!     occur with     ___Akv   vertical viscosity coefficient
!     prefix indx    ___Akt   vertical T-diffusion coefficient
!     (see above).   ___Aks   vertical S-diffusion coefficient
!                    ___Hbl   depth of mixed layer LMD_SKPP.
!
! Sizes of unlimited time dimensions in netCDF files:
!
!   ntsms   surface momentum stress in current forcing file.
!   ntbulk   bulk formulation in current forcing file.
!   ntsrf   shortwave radiation flux in current forcing file.
!   ntssh   sea surface height in current climatology file.
!   ntsst   sea surface temperature in current forcing file.
!   ntsss   sea surface salinity in current forcing file.
!   ntstf   surface flux of tracers in current forcing file.
!   nttclm  tracer variables in current climatology file.
!   ntuclm  momentum variables in current climatology file.
!   ntww    wind induced wave data in current forcing file.
!   ntbulkn bulk formula variables in current forcing file.
!
! vname    character array for variable names and attributes;
!
      integer ncidfrc, ncidbulk, ncidclm,  ntsms
     &      , ntsrf,  ntssh,  ntsst, ntsss, ntuclm, ntww,
     &        ntbulk
#ifdef SOLVE3D
      integer nttclm(NT),    ntstf(NT)
#endif
      integer ncidrst, nrecrst,  nrpfrst
     &      , rstTime, rstTstep, rstZ,    rstUb,  rstVb
#ifdef SOLVE3D
     &                         , rstU,    rstV
      integer rstT(NT)
# ifdef SEDIMENT
      integer rstSed(NST+2)
# endif	
#endif
#ifdef BBL
      integer rstBBL(2)
#endif
      integer  ncidhis, nrechis,  nrpfhis
     &      , hisTime, hisTstep, hisZ,    hisUb,  hisVb
#ifdef SOLVE3D
     &      , hisU,  hisV,  hisR,    hisHbl, hisHbbl
     &      , hisO,  hisW,  hisVisc,  hisAkv,  hisAkt, hisAks
     &      , hisBostr
# ifdef BIOLOGY
     &      , hisHel
#  ifdef BIO_NPZD
     &      , hisChC
#   ifdef OXYGEN
     &      , hisU10, hisKvO2, hisO2sat
#   endif
#  elif defined BIO_N2P2Z2D2 && defined VAR_CHL_C
     &      , hisChC1, hisChC2
#  endif
# endif
      integer hisT(NT)
# ifdef SEDIMENT
      integer hisSed(NST+2)
# endif
# ifdef BBL
      integer hisBBL(6)
# endif
# if defined DIAGNOSTICS_TS 
      integer nciddia, nrecdia, nrpfdia
     &      , diaTime, diaTstep
     &      , diaTXadv(NT), diaTYadv(NT), diaTVadv(NT)
     &      , diaTHmix(NT), diaTVmix(NT)
     &      , diaTbody(NT), diaTrate(NT)
# endif
# ifdef DIAGNOSTICS_UV
        integer nciddiaM, nrecdiaM, nrpfdiaM
     &      , diaTimeM, diaTstepM
     &      , diaMXadv(2), diaMYadv(2), diaMVadv(2)
     &      , diaMCor(2), diaMPrsgrd(2), diaMHmix(2)
     &      , diaMVmix(2), diaMrate(2)
# endif
# ifdef DIAGNOSTICS_BIO
      integer nciddiabio, nrecdiabio, nrpfdiabio
     &      , diaTimebio, diaTstepbio
     &      , diabioFlux(NumFluxTerms)
     &      , diabioVSink(NumVSinkTerms)
     &      , diabioGasExc(NumGasExcTerms)
# endif
#endif
#ifdef AVERAGES
      integer   ncidavg, nrecavg,  nrpfavg
     &      , avgTime, avgTstep, avgZ,    avgUb,  avgVb
# ifdef SOLVE3D
     &      , avgU,  avgV,  avgR,    avgHbl, avgHbbl
     &      , avgO,  avgW,  avgVisc, avgAkv,  avgAkt, avgAks
# ifdef BIOLOGY
     &      , avgHel
#  ifdef BIO_NPZD
     &      , avgChC
#   ifdef OXYGEN
     &      , avgU10, avgKvO2, avgO2sat
#   endif
#  elif defined BIO_N2P2Z2D2 && defined VAR_CHL_C
     &      , avgChC1, avgChC2
#  endif
# endif
      integer avgT(NT)
# endif
      integer avgBostr
# ifdef SOLVE3D
#  ifdef SEDIMENT
      integer avgSed(NST+2)
#  endif
#  ifdef BBL
      integer avgBBL(6)
#  endif
#  if defined DIAGNOSTICS_TS
      integer nciddia_avg, nrecdia_avg, nrpfdia_avg
     &      , diaTime_avg, diaTstep_avg
     &      , diaTXadv_avg(NT), diaTYadv_avg(NT), diaTVadv_avg(NT)
     &      , diaTHmix_avg(NT), diaTVmix_avg(NT)
     &      , diaTbody_avg(NT), diaTrate_avg(NT)
#  endif
#  ifdef DIAGNOSTICS_UV
       integer nciddiaM_avg, nrecdiaM_avg, nrpfdiaM_avg
     &      , diaTimeM_avg, diaTstepM_avg
     &      , diaMXadv_avg(2), diaMYadv_avg(2), diaMVadv_avg(2)
     &      , diaMCor_avg(2), diaMPrsgrd_avg(2), diaMHmix_avg(2)
     &      , diaMVmix_avg(2), diaMrate_avg(2)
#  endif
#  ifdef DIAGNOSTICS_BIO
      integer nciddiabio_avg, nrecdiabio_avg, nrpfdiabio_avg
     &      , diaTimebio_avg, diaTstepbio_avg
     &      , diabioFlux_avg(NumFluxTerms)
     &      , diabioVSink_avg(NumVSinkTerms)
     &      , diabioGasExc_avg(NumGasExcTerms)
#  endif
# endif /* SOLVE3D */
#endif /* AVERAGES */
 
#ifdef SOLVE3D
# define NWRTHIS 130+NT
#else
# define NWRTHIS 14
#endif
      logical wrthis(NWRTHIS)
#ifdef AVERAGES
     &      , wrtavg(NWRTHIS)
#endif
#if defined DIAGNOSTICS_TS 
     &      , wrtdia
# ifdef AVERAGES
     &      , wrtdia_avg
# endif
#endif
#if defined DIAGNOSTICS_UV
     &      , wrtdiaM
# ifdef AVERAGES
     &      , wrtdiaM_avg
# endif
#endif
#ifdef DIAGNOSTICS_BIO
     &      , wrtdiabio
# ifdef AVERAGES
     &      , wrtdiabio_avg
# endif
#endif
	
      common/incscrum/
     &        ncidfrc, ncidbulk,ncidclm, ntsms, ntsrf, ntssh, ntsst
     &      , ntuclm, ntsss, ntww, ntbulk
#if defined MPI && defined PARALLEL_FILES
# ifndef EW_PERIODIC
     &      , xi_rho,  xi_u
# endif
# ifndef NS_PERIODIC
     &      , eta_rho, eta_v
# endif
#endif
#ifdef SOLVE3D
     &                        ,  nttclm,          ntstf
#endif
     &      , ncidrst, nrecrst,  nrpfrst
     &      , rstTime, rstTstep, rstZ,    rstUb,  rstVb
#ifdef SOLVE3D
     &                         , rstU,    rstV,   rstT
# ifdef SEDIMENT
     &                         , rstSed
# endif
#endif
#ifdef BBL
     &                         , rstBBL
#endif
     &      , ncidhis, nrechis,  nrpfhis
     &      , hisTime, hisTstep, hisZ,    hisUb,  hisVb
#ifdef SOLVE3D
     &      , hisU,    hisV,     hisT,    hisR
     &      , hisO,    hisW,     hisVisc, hisAkv,  hisAkt, hisAks
     &      , hisHbl,  hisHbbl,  hisBostr
# ifdef BIOLOGY
     &      , hisHel
#  ifdef BIO_NPZD
     &      , hisChC
#   ifdef OXYGEN
     &      , hisU10, hisKvO2, hisO2sat
#   endif
#  elif defined BIO_N2P2Z2D2 && defined VAR_CHL_C
     &      , hisChC1, hisChC2
#  endif
# endif
# ifdef SEDIMENT
     &      , hisSed
# endif
#endif
#ifdef BBL
     &      , hisBBL
#endif
#if defined DIAGNOSTICS_TS
     &      , nciddia, nrecdia, nrpfdia
     &      , diaTime, diaTstep
     &      , diaTXadv, diaTYadv, diaTVadv, diaTHmix
     &      , diaTVmix, diaTbody, diaTrate
# ifdef AVERAGES
     &      , nciddia_avg, nrecdia_avg, nrpfdia_avg
     &      , diaTime_avg, diaTstep_avg
     &      , diaTXadv_avg, diaTYadv_avg, diaTVadv_avg
     &      , diaTHmix_avg, diaTVmix_avg, diaTbody_avg
     &      , diaTrate_avg
# endif
#endif
#ifdef DIAGNOSTICS_UV
     &      , nciddiaM, nrecdiaM, nrpfdiaM
     &      , diaTimeM, diaTstepM
     &      , diaMXadv, diaMYadv, diaMVadv, diaMCor
     &      , diaMPrsgrd, diaMHmix, diaMVmix, diaMrate
# ifdef AVERAGES
     &      , nciddiaM_avg, nrecdiaM_avg, nrpfdiaM_avg
     &      , diaTimeM_avg, diaTstepM_avg
     &      , diaMXadv_avg, diaMYadv_avg, diaMVadv_avg
     &      , diaMCor_avg, diaMPrsgrd_avg, diaMHmix_avg
     &      , diaMVmix_avg, diaMrate_avg
# endif
#endif
#ifdef DIAGNOSTICS_BIO
     &      , nciddiabio, nrecdiabio, nrpfdiabio
     &      , diaTimebio, diaTstepbio, diabioFlux
     &      , diabioVSink
     &      , diabioGasExc
# ifdef AVERAGES
     &      , nciddiabio_avg, nrecdiabio_avg, nrpfdiabio_avg
     &      , diaTimebio_avg, diaTstepbio_avg, diabioFlux_avg
     &      , diabioVSink_avg
     &      , diabioGasExc_avg
# endif
#endif 
#ifdef AVERAGES
     &      , ncidavg,  nrecavg,  nrpfavg
     &      , avgTime, avgTstep, avgZ,    avgUb,  avgVb
# ifdef SOLVE3D
     &      , avgU,    avgV,     avgT,    avgR
     &      , avgO,    avgW,     avgVisc,  avgAkv,  avgAkt, avgAks
     &      , avgHbl,  avgHbbl
#  ifdef BIOLOGY
     &      , avgHel
#   ifdef BIO_NPZD
     &      , avgChC
#    ifdef OXYGEN
     &      , avgU10, avgKvO2, avgO2sat
#    endif
#   elif defined BIO_N2P2Z2D2 && defined VAR_CHL_C
     &      , avgChC1, avgChC2
#   endif
#  endif
# endif
     &      , avgBostr
# ifdef SOLVE3D
#  ifdef SEDIMENT
     &      , avgSed
#  endif
# endif
# ifdef BBL
     &      , avgBBL
# endif
#endif
     &      , wrthis
#ifdef AVERAGES
     &      , wrtavg
#endif
#if defined DIAGNOSTICS_TS
     &      , wrtdia
# ifdef AVERAGES
     &      , wrtdia_avg
# endif
#endif
#if defined DIAGNOSTICS_UV
     &      , wrtdiaM
# ifdef AVERAGES
     &      , wrtdiaM_avg
# endif
#endif
#ifdef DIAGNOSTICS_BIO
     &      , wrtdiabio
# ifdef AVERAGES
     &      , wrtdiabio_avg
# endif
#endif

      character*80 date_str, title
      character*80 ininame,  grdname,  hisname
     &         ,   rstname,  frcname,  bulkname,  usrname
#ifdef AVERAGES
     &                                ,   avgname
#endif
#ifdef DIAGNOSTICS_TS 
     &                                ,  dianame
# ifdef AVERAGES
     &                                ,  dianame_avg
# endif
#endif
#ifdef DIAGNOSTICS_UV
     &                                ,  dianameM
# ifdef AVERAGES
     &                                ,  dianameM_avg
# endif
#endif
#ifdef DIAGNOSTICS_BIO
     &                                ,  dianamebio
# ifdef AVERAGES
     &                                ,  dianamebio_avg
# endif
#endif
#if (defined TCLIMATOLOGY  && !defined ANA_TCLIMA)\
 || (defined ZCLIMATOLOGY  && !defined ANA_SSH)\
 || (defined M2CLIMATOLOGY && !defined ANA_M2CLIMA)\
 || (defined M3CLIMATOLOGY && !defined ANA_M3CLIMA)
     &                                ,   clmname
#endif
#ifdef FRC_BRY 
     &                                ,   bry_file
#endif
#ifdef ASSIMILATION
     &                      ,  aparnam,   assname
#endif
#ifdef BIOLOGY
     &                                ,   bioname
#endif
#ifdef SEDIMENT
     &                                ,   sedname
#endif

#ifdef SOLVE3D
      character*52  vname(4, 300)
#else
      character*52  vname(4, 39)
#endif

      common /cncscrum/       date_str,   title
     &         ,   ininame,  grdname, hisname
     &         ,   rstname,  frcname, bulkname,  usrname
#ifdef AVERAGES
     &                                ,  avgname
#endif
#if defined DIAGNOSTICS_TS
     &                                ,  dianame
# ifdef AVERAGES
     &                                ,  dianame_avg
# endif
#endif
#if defined DIAGNOSTICS_UV
     &                                ,  dianameM
# ifdef AVERAGES
     &                                ,  dianameM_avg
# endif
#endif
#ifdef DIAGNOSTICS_BIO
     &                                ,  dianamebio
# ifdef AVERAGES
     &                                ,  dianamebio_avg
# endif
#endif
#if (defined TCLIMATOLOGY  && !defined ANA_TCLIMA)\
 || (defined ZCLIMATOLOGY  && !defined ANA_SSH)\
 || (defined M2CLIMATOLOGY && !defined ANA_M2CLIMA)\
 || (defined M3CLIMATOLOGY && !defined ANA_M3CLIMA)
     &                                ,   clmname
#endif
#ifdef FRC_BRY
     &                                ,   bry_file
#endif
#ifdef ASSIMILATION
     &                      ,  aparnam,   assname
#endif
#ifdef SEDIMENT
     &                      ,  sedname
#endif
#ifdef BIOLOGY
     &                      ,  bioname
#endif
     &                      ,  vname

