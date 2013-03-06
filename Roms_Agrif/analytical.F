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
#include "cppdefs.h"
!
!  ANALYTICAL PACKAGE:
!--------------------------------------------------------------------
!
!  This package is used to provide various analytical fields to the
!  model when appropriate.
!
!  Routines:
!
!  ana_bmflux_tile   Analytical kinematic bottom momentum flux.
!  ana_btflux_tile   Analytical kinematic bottom flux of tracer
!                          type variables.
!  ana_bsedim_tile   Analytical bottom sediment grain size
!                          and density.
!  ana_smflux_tile   Analytical kinematic surface momentum flux
!                          (wind stress).
!  ana_srflux_tile   Analytical kinematic surface shortwave
!                          radiation.
!  ana_ssh_tile      Analytical sea surface height climatology.      
!  ana_sst_tile      Analytical sea surface temperature and dQdSST  
!                         which are used during heat flux correction.
!  ana_sss_tile      Analytical sea surface salinity which is used 
!                         during salt flux correction.
!  ana_stflux_tile   Analytical kinematic surface flux of tracer type
!                          variables.
!  ana_tclima_tile   Analytical tracer climatology fields.     
!  ana_uclima_tile   Analytical tracer climatology fields.  
!  ana_wwave_tile    Analytical wind induced wave amplitude,
!                         direction and period.
!  ana_sediment_tile Analytical sediment
!  ana_psource_tile  Analytical point source
!  ana_bry_tile      Analytical boundary forcing.
!
!-------------------------------------------------------------------
!

# if !defined OPENMP
      integer function omp_get_thread_num()
      omp_get_thread_num=0
      return
      end
      integer function omp_get_num_threads()
      omp_get_num_threads=1
      return
      end
# endif

#ifdef ANA_BMFLUX
      subroutine ana_bmflux_tile (Istr,Iend,Jstr,Jend)
!
!---------------------------------------------------------------------
!  This routine sets kinematic bottom momentum flux (bottom stress)
! "bustr" and "bvstr" [m^2/s^2] using an analytical expression.
!---------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "grid.h"
# include "forces.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i,j
!
# include "compute_auxiliary_bounds.h"
!
      do j=JstrR,JendR
        do i=Istr,IendR
          bustr(i,j)=???
        enddo
      enddo
      do j=Jstr,JendR
        do i=IstrR,IendR
          bvstr(i,j)=???
        enddo
      enddo
      return
      end
#endif /* ANA_BMFLUX */ 
#ifdef SOLVE3D
# if defined ANA_BTFLUX || defined ANA_BSFLUX || defined ANA_BPFLUX 
      subroutine ana_btflux_tile (Istr,Iend,Jstr,Jend, itrc)
!
!---------------------------------------------------------------------
!  This routine sets kinematic bottom flux of tracer type variables
!  [tracer units m/s].
!
!  On Input:
!     itrc      Tracer type array index.
!---------------------------------------------------------------------
!
      implicit none
#  include "param.h"
#  include "grid.h"
#  include "forces.h"
#  include "scalars.h"
      integer itrc, Istr,Iend,Jstr,Jend, i,j
!
#  include "compute_auxiliary_bounds.h"
!
      if (itrc.eq.itemp) then
!
! Set kinematic bottom heat flux [degC m/s] at horizontal RHO-points.
!--------------------------------------------------------------------
!
#  if defined BASIN     || defined CANYON_A  || defined CANYON_B   \
   || defined EQUATOR   || defined GRAV_ADJ  || defined INNERSHELF \
   || defined OVERFLOW  || defined REGIONAL  || defined RIVER      \
   || defined SEAMOUNT  || defined SED_TEST2 || defined SHELFRONT  \
   || defined UPWELLING || defined VORTEX    || defined INTERNAL   \
   || defined JET
        do j=JstrR,JendR
          do i=IstrR,IendR
            btflx(i,j,itemp)=0.
          enddo
        enddo
#  else
        do j=JstrR,JendR
          do i=IstrR,IendR
            btflx(i,j,itemp)=???
          enddo
        enddo
#  endif
#  ifdef SALINITY
      elseif (itrc.eq.isalt) then
!
!  Set kinematic bottom salt flux (m/s) at horizontal RHO-points,
!  scaling by bottom salinity is done in STEP3D.
!---------------------------------------------------------------------
!
#   if  defined EQUATOR || defined INNERSHELF || defined REGIONAL  \
   || defined RIVER     || defined SED_TEST2  || defined SHELFRONT \
   || defined UPWELLING || defined SEAMOUNT   || defined VORTEX    \
   || defined JET

        do j=JstrR,JendR
          do i=IstrR,IendR
            btflx(i,j,isalt)=0.
          enddo
        enddo
#   else
        do j=JstrR,JendR
          do i=IstrR,IendR
            btflx(i,j,isalt)=???
          enddo
        enddo
#   endif
#  endif /* SALINITY */
      else
!
!---------------------------------------------------------------------
!  Set kinematic surface flux of additional tracers, 
! for example sediments, bio..., to zero
!---------------------------------------------------------------------
!
        do j=JstrR,JendR
          do i=IstrR,IendR
            btflx(i,j,itrc)=0.
          enddo
        enddo
      endif
      return
      end
# endif /* ANA_BTFLUX */
#endif /* SOLVE3D */
!
#if defined ANA_BSEDIM && defined BBL
      subroutine ana_bsedim (tile)
      implicit none
# include "param.h"
      integer tile
#ifdef  ALLOW_SINGLE_BLOCK_MODE
C$    integer  trd, omp_get_thread_num
#endif
# include "compute_tile_bounds.h"
      call ana_bsedim_tile   (Istr,Iend,Jstr,Jend)
      return
      end
      subroutine ana_bsedim_tile (Istr,Iend,Jstr,Jend)
!
!---------------------------------------------------------------------
!  This routine sets initial bottom sediment grain diameter size [m]
!  and density used in the bottom boundary formulation [kg/m^3].
!---------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "bbl.h"
# include "grid.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i,j
!
# include "compute_auxiliary_bounds.h"
!
# if defined SED_TEST2 || defined REGIONAL
!
! taucb=critical threshold stress for initiation of motion 
! (=bedload for coarse grains). 
! critical suspension stress: ustar_crit=0.8*w_set 
!
! determine taucb from Shields curve, fit provided by 
! Soulsby & Whitehouse 1997, Threshold of sediment motion 
! in coastal environments, Proc. Pacific Coasts and Ports 
! '97 Conf., pp 149--154, Univ Canterbury, Nw Zealand.
!
! visk=1.3e-3/rhow; (kinem. visc., nu=mu/rhow)
! D=d50*(g*(Sdens/rhow-1)/(visk^2))^0.33333 
! thetcr=0.3./(1+1.2*D) + 0.055*(1-exp(-0.02*D))
! taucb=thetcr.*(g*(sdens-rhow).*d50);
!
! Souslby's (1997) estimate of settling velocity
!   w_set = visk*(sqrt(10.36^2+1.049*D^3)-10.36)/d50 [m/s]
! with D as above 
!
      do j=JstrR,JendR
        do i=IstrR,IendR
          Ssize(i,j)=1.5e-4  ! d50 [m]
          Sdens(i,j)=2650.0  ! rho sediment [kg/m^3]
          taucb(i,j)=0.16    ! critical bedload stress [N/m^2]
          w_set(i,j)=0.013   ! analytical settling velocity [m/s]
          Hripple(i,j)=0.01   ! analytical initial ripple height [m]
          Lripple(i,j)=0.10   ! analytical initial ripple length [m]
        enddo
      enddo
# else
      ANA_BSEDIM: no values provided for SSIZE and SDENS.
# endif
      return
      end
#endif /* ANA_BSEDIM && BBL */

#ifdef ANA_SMFLUX
      subroutine ana_smflux_tile (Istr,Iend,Jstr,Jend)
!
!  Sets kinematic surface momentum flux (wind stress) "sustr" and "svstr"
!  [m^2/s^2] using an analytical expression.
!
# ifdef AGRIF
      use Agrif_UTIL
# endif
      implicit none
# include "param.h"
# include "grid.h"
# include "forces.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i,j
      real Ewind, Nwind, dircoef, windamp
      real cff1, cff2
!     data windamp /0./ 
      data Ewind, Nwind, dircoef /0., 0., 0./
!     save windamp
!
#include "compute_extended_bounds.h"
!
!  Set kinematic surface momentum flux (wind stress) component in the
!  XI-direction (m^2/s^2) at horizontal U-points.
!

      windamp = 0.

# ifdef BASIN
      cff1=0.0001 * 0.5*(1.+tanh((time-6.*86400.)/(3.*86400.)))
      cff2=2.*pi/el
      do j=JstrR,JendR
        do i=IstrR,IendR
          sustr(i,j)=-cff1*cos(cff2*yr(i,j))
       enddo
      enddo
# elif defined CANYON_A || defined CANYON_B
      do j=JstrR,JendR
        do i=IstrR,IendR
          sustr(i,j)=0.0001*0.5*sin(2.*pi*tdays/10.)*
     &               (1.-tanh((yr(i,j)-0.5*el)/10000.))
        enddo
      enddo
# elif defined EQUATOR
      do j=JstrR,JendR
        do i=IstrR,IendR
          sustr(i,j)=-0.05/rho0
        enddo
      enddo
# elif defined SED_TEST2
      do j=JstrR,JendR
        do i=IstrR,IendR
          windamp=0.5+
     &            0.5*TANH((time-user(9))/user(10))
          sustr(i,j)=windamp*user(1)
        enddo
      enddo
# elif defined UPWELLING
      if (tdays.le.2.) then
        windamp=-0.1*sin(pi*tdays/4.)/rho0
      else
        windamp=-0.1/rho0
      endif
      do j=JstrR,JendR
        do i=IstrR,IendR
          sustr(i,j)=windamp
        enddo
      enddo
# elif defined GRAV_ADJ  || defined OVERFLOW || defined SEAMOUNT   \
    || defined SHELFRONT || defined SOLITON  || defined INNERSHELF \
    || defined RIVER     || defined VORTEX   || defined REGIONAL   \
    || defined INTERNAL  || defined JET
      do j=JstrR,JendR
        do i=IstrR,IendR
          sustr(i,j)=0.
        enddo
      enddo
# else
      do j=JstrR,JendR
        do i=IstrR,IendR
          sustr(i,j)=???
        enddo
      enddo
# endif
!
!  Set kinematic surface momentum flux (wind stress) component in the
!  ETA-direction (m^2/s^2) at horizontal V-points.
!
# if defined BASIN     || defined CANYON_A  || defined CANYON_B  \
  || defined EQUATOR   || defined GRAV_ADJ  || defined OVERFLOW  \
  || defined REGIONAL  || defined RIVER     || defined SEAMOUNT  \
  || defined SHELFRONT || defined SOLITON   || defined UPWELLING \
  || defined VORTEX    || defined INTERNAL  || defined JET
      do j=JstrR,JendR
        do i=IstrR,IendR
          svstr(i,j)=0.
        enddo
      enddo
# elif defined SED_TEST2
      do j=JstrR,JendR
        do i=IstrR,IendR
          windamp=0.5+
     &            0.5*TANH((time-user(9))/user(10))
          svstr(i,j)= windamp*user(2)
        enddo
      enddo
# elif defined INNERSHELF
      do j=JstrR,JendR
        do i=IstrR,IendR
          svstr(i,j)=0.07/rho0
        enddo
      enddo
# else
      do j=JstrR,JendR
        do i=IstrR,IendR
          svstr(i,j)=???
        enddo
      enddo
# endif
      return
      end
#endif /* ANA_SMFLUX */
#ifdef ANA_SRFLUX
      subroutine ana_srflux_tile (Istr,Iend,Jstr,Jend)
!
!---------------------------------------------------------------------
!  This subroutine sets kinematic surface solar shortwave radiation
!  flux "srflx" (degC m/s) using an analytical expression.
!---------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "grid.h"
# include "forces.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i,j
!
# include "compute_auxiliary_bounds.h"
!
!  Set kinematic surface solar shortwave radiation [degC m/s] at
!  horizontal RHO-points.
!
# if defined EQUATOR   || defined INNERSHELF || defined OVERFLOW \
  || defined REGIONAL  || defined RIVER      || defined SEAMOUNT \
  || defined SHELFRONT || defined UPWELLING  || defined VORTEX \
  || defined INTERNAL  || defined JET
      do j=JstrR,JendR
        do i=IstrR,IendR
          srflx(i,j)=0.
        enddo
      enddo
# else
      do j=JstrR,JendR
        do i=IstrR,IendR
          srflx(i,j)=???
        enddo
      enddo
# endif
      return
      end
#endif /* ANA_SRFLUX */

#if defined ANA_SSH && defined ZCLIMATOLOGY
      subroutine ana_ssh (tile)
      implicit none
# include "param.h"
      integer tile
#ifdef  ALLOW_SINGLE_BLOCK_MODE
C$    integer  trd, omp_get_thread_num
#endif
# include "compute_tile_bounds.h"
      call ana_ssh_tile (Istr,Iend,Jstr,Jend)
      return
      end 
!
      subroutine ana_ssh_tile (Istr,Iend,Jstr,Jend)
!
!---------------------------------------------------------------------
!  This routine sets analytical sea surface height climatology [m].
!---------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean2d.h"
# include "climat.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i,j
# ifdef INTERNAL
      real U0,omega,kwave,ETA0
# endif
!
# include "compute_auxiliary_bounds.h"
!
!  Set sea surface height (meters).
!
# if defined REGIONAL
      do j=JstrR,JendR
        do i=IstrR,IendR
          ssh(i,j)=0.
        enddo
      enddo
# elif defined INTERNAL
      U0=0.02
      omega=2.*pi/(12.4*3600)
      kwave=((omega*omega)-(f(1,1)*f(1,1)))/(g*h(1,1))
      ETA0=kwave*h(1,1)*U0/omega
      do j=JstrR,JendR
        do i=IstrR,IendR
          ssh(i,j)=ETA0*sin(omega*time-kwave*(xr(i,j)))
        enddo
      enddo
# elif defined JET
           ! climatology computed in ana_initial
# else
      do j=JstrR,JendR
        do i=IstrR,IendR
          ssh(i,j)=zeta(i,j,1)
        enddo
      enddo
# endif
# if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_r2d_tile (istr,iend,jstr,jend, ssh(START_2D_ARRAY))
# endif
      return
      end
#endif /* ANA_SSH && ZCLIMATOLOGY */

#if defined ANA_SST && defined QCORRECTION
      subroutine ana_sst_tile (Istr,Iend,Jstr,Jend)
!
!--------------------------------------------------------------------
!  This routine sets sea surface temperature SST[Celsius] and surface
!  net heat flux sensitivity dQdSTT to sea surface temperature using
!  analytical expressions. dQdSTT is usually computed in units of
!  [Watts/m^2/degC]. It needs to be scaled to [m/s] by dividing by
!  rho0*Cp.  These forcing fields are used when the heat flux
!  correction is activated:
!
!       Q_model ~ Q + dQdSST * (T_model - SST)
!--------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "grid.h"
# include "forces.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i,j
# if defined EQUATOR
      real y1,y2,sst1,sst2
# endif
!
# include "compute_auxiliary_bounds.h"
!
# if defined EQUATOR
! SST = 25C and lineraly decreases to 10C 1200km from the Equator.
      y1=1200.E+3
      y2=1500.E+3
      sst1=10.
      sst2=25.
      do j=JstrR,JendR
        do i=IstrR,IendR
          sst(i,j)=sst1
          if ((yr(i,j).gt.-y1).and.(yr(i,j).lt.y1)) then
            sst(i,j)=sst2
          else
            if ((yr(i,j).gt.-y2).and.(yr(i,j).lt.-y1)) then
              sst(i,j)=((sst2-sst1)*yr(i,j)-sst1*y1+y2*sst2)/(y2-y1)
            endif
            if ((yr(i,j).gt.y1).and.(yr(i,j).lt.y2)) then
              sst(i,j)=((sst2-sst1)*yr(i,j)+sst1*y1-y2*sst2)/(y1-y2)
            endif
          endif
          dqdt(i,j)=-50.0/(rho0*Cp)
        enddo
      enddo
# else
      do j=JstrR,JendR
        do i=IstrR,IendR
          sst(i,j)=???
          dqdt(i,j)=???
        enddo
      enddo
# endif
      return
      end
#endif /* ANA_SST && QCORRECTION */
#if defined SALINITY && defined SFLX_CORR && defined ANA_SSS
      subroutine ana_sss_tile (Istr,Iend,Jstr,Jend)
!
!--------------------------------------------------------------------
!  This routine sets sea surface salinity SSS[PSU] using
!  analytical expressions. This forcing field is used when the
!  salt flux correction is activated:
!
!    SSSFLX_model ~ SSS*(E-P) + CST * (SSS_model - SSS)
!
!  we use DQDSST for CST.... 
!
!--------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "grid.h"
# include "forces.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i,j
!
# include "compute_auxiliary_bounds.h"
!
      do j=JstrR,JendR
        do i=IstrR,IendR
          sss(i,j)=???
        enddo
      enddo
      return
      end
#endif /* SALINITY && SFLX_CORR && ANA_SSS */
#if defined ANA_STFLUX || defined ANA_SSFLUX
      subroutine ana_stflux_tile (Istr,Iend,Jstr,Jend, itrc)
!
!--------------------------------------------------------------------
!  This routine sets kinematic surface flux of tracer type variables
!  "stflx" (tracer units m/s) using analytical expressions.
!
!  On Input:
!     itrc      Tracer type array index.
!--------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "grid.h"
# include "forces.h"
# include "scalars.h"
      integer itrc, Istr,Iend,Jstr,Jend, i,j 
c
# include "compute_auxiliary_bounds.h"
c
      if (itrc.eq.itemp) then
!
!  Set kinematic surface heat flux [degC m/s] at horizontal
!  RHO-points.
!
#if defined BASIN    || defined CANYON_A  || defined CANYON_B   \
 || defined EQUATOR  || defined GRAV_ADJ  || defined INNERSHELF \
 || defined OVERFLOW || defined REGIONAL  || defined RIVER      \
 || defined SEAMOUNT || defined SHELFRONT || defined UPWELLING  \
 || defined VORTEX   || defined INTERNAL  || defined JET

        do j=JstrR,JendR
          do i=IstrR,IendR
            stflx(i,j,itemp)=0.
          enddo
        enddo
#else
        do j=JstrR,JendR
          do i=IstrR,IendR
            stflx(i,j,itemp)=???
          enddo
        enddo
#endif
#ifdef SALINITY
      elseif (itrc.eq.isalt) then
!
!  Set kinematic surface freshwater flux (m/s) at horizontal
!  RHO-points, scaling by surface salinity is done in STEP3D.
!
# if defined EQUATOR || defined INNERSHELF || defined REGIONAL  \
  || defined RIVER   || defined SEAMOUNT   || defined SHELFRONT \
  || defined UPWELLING

        do j=JstrR,JendR
          do i=IstrR,IendR
            stflx(i,j,isalt)=0.
          enddo
        enddo
# else
        do j=JstrR,JendR
          do i=IstrR,IendR
            stflx(i,j,isalt)=???
          enddo
        enddo
# endif
#endif /* SALINITY */
      else
!
!  Set kinematic surface flux of additional tracers, if any.
!
        do j=JstrR,JendR
          do i=IstrR,IendR
            stflx(i,j,itrc)=0.
          enddo
        enddo
      endif
      return
      end
#endif /* ANA_STFLUX || ANA_SSFLUX */
!---------------------------------------------------------------------
#if defined TCLIMATOLOGY
      subroutine ana_tclima (tile)
      implicit none
# include"param.h"
      integer tile
#ifdef  ALLOW_SINGLE_BLOCK_MODE
C$    integer  trd, omp_get_thread_num
#endif
#include "compute_tile_bounds.h"
      call ana_tclima_tile   (Istr,Iend,Jstr,Jend)
      return
      end
      subroutine ana_tclima_tile (Istr,Iend,Jstr,Jend)
!
!---------------------------------------------------------------------
!  This routine sets analytical ACTIVE (T&S) tracer climatology fields.
!---------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "grid.h"
# include "climat.h"
# include "ocean3d.h"
# include "scalars.h"
# include "sediment.h"
      integer Istr,Iend,Jstr,Jend, i,j,k, itrc
      real    cff,cff1
!
# include "compute_auxiliary_bounds.h"
!
!  Set climatology fields for tracer type variables.
!---------------------------------------------------------------------
!
# ifdef ANA_TCLIMA
#  ifdef JET
           ! climatology computed in ana_initial
#  else
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            tclm(i,j,k,itemp)=t(i,j,k,1,itemp)
#  ifdef SALINITY
            tclm(i,j,k,isalt)=t(i,j,k,1,isalt)
#  endif /* SALINITY */
          enddo
        enddo
      enddo
#  endif /* JET */
# endif

# ifdef BIOLOGY
#  define temp cff
#  define SiO4 cff1
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
#  ifdef ANA_TCLIMA
            temp=t(i,j,k,1,itemp)
            if (temp.lt.8.) then
               SiO4=30.
            elseif (temp.ge.8. .and. temp.le.11.) then
               SiO4=30.-((temp-8.)*(20./3.))
            elseif (temp.gt.11. .and. temp.le.13.) then
                SiO4=10.-((temp-11.)*(8./2.))
            elseif (temp.gt.13. .and. temp.le.16.) then
               SiO4=2.-((temp-13.)*(2./3.))
            elseif (temp.gt.16.) then
              SiO4=0.
            endif
            tclm(i,j,k,iNO3_)=1.67+0.5873*SiO4+0.0144*SiO4**2
     &                            +0.0003099*SiO4**3
#   ifdef PISCES
            tclm(i,j,k,iDIC_)=2150.
            tclm(i,j,k,iTAL_)=2350.
            tclm(i,j,k,iOXY_)=200.
            tclm(i,j,k,iCAL_)=0.01
            tclm(i,j,k,iPO4_)=tclm(i,j,k,iNO3_)/16.
            tclm(i,j,k,iPOC_)=0.01
            tclm(i,j,k,iSIL_)=91.51
            tclm(i,j,k,iPHY_)=0.01
            tclm(i,j,k,iZOO_)=0.01
            tclm(i,j,k,iDOC_)=5.
            tclm(i,j,k,iDIA_)=0.01
            tclm(i,j,k,iMES_)=0.01
            tclm(i,j,k,iBSI_)=1.5e-3
            tclm(i,j,k,iFER_)=6.e-4
            tclm(i,j,k,iBFE_)=1.e-2*5.e-6
            tclm(i,j,k,iGOC_)=0.01
            tclm(i,j,k,iSFE_)=1.e-2*5.e-6
            tclm(i,j,k,iDFE_)=1.e-2*5.e-6
            tclm(i,j,k,iDSI_)=1.e-2*0.15
            tclm(i,j,k,iNFE_)=1.e-2*5.e-6
            tclm(i,j,k,iNCH_)=1.e-2*12./55.
            tclm(i,j,k,iDCH_)=1.e-2*12./55.
            tclm(i,j,k,iNH4_)=1.e-2
#   elif defined BIO_NChlPZD
            tclm(i,j,k,iChla)=0.08
            tclm(i,j,k,iPhy1)=0.1
            tclm(i,j,k,iZoo1)=0.06
            tclm(i,j,k,iDet1)=0.02
#   elif defined BIO_N2ChlPZD2
            tclm(i,j,k,iNH4_)=0.1
            tclm(i,j,k,iChla)=0.08
            tclm(i,j,k,iPhy1)=0.06
            tclm(i,j,k,iZoo1)=0.04
            tclm(i,j,k,iDet1)=0.02
            tclm(i,j,k,iDet2)=0.02
#   endif
#  else
            if (.not.got_tclm(iNO3_)) then
              temp=t(i,j,k,1,itemp)
              if (temp.lt.8.) then
                 SiO4=30.
              elseif (temp.ge.8. .and. temp.le.11.) then
                 SiO4=30.-((temp-8.)*(20./3.))
              elseif (temp.gt.11. .and. temp.le.13.) then
                 SiO4=10.-((temp-11.)*(8./2.))
              elseif (temp.gt.13. .and. temp.le.16.) then
                 SiO4=2.-((temp-13.)*(2./3.))
              elseif (temp.gt.16.) then
                SiO4=0.
              endif
              tclm(i,j,k,iNO3_)=1.67+0.5873*SiO4+0.0144*SiO4**2
     &                              +0.0003099*SiO4**3
            endif
#   ifdef PISCES
            if (.not.got_tclm(iDIC_)) tclm(i,j,k,iDIC_)=2150.
            if (.not.got_tclm(iTAL_)) tclm(i,j,k,iTAL_)=2350.
            if (.not.got_tclm(iOXY_)) tclm(i,j,k,iOXY_)=200.
            if (.not.got_tclm(iCAL_)) tclm(i,j,k,iCAL_)=0.01
            if (.not.got_tclm(iPO4_)) then
              temp=t(i,j,k,1,itemp)
              if (temp.lt.8.) then
                 SiO4=30.
              elseif (temp.ge.8. .and. temp.le.11.) then
                 SiO4=30.-((temp-8.)*(20./3.))
              elseif (temp.gt.11. .and. temp.le.13.) then
                 SiO4=10.-((temp-11.)*(8./2.))
              elseif (temp.gt.13. .and. temp.le.16.) then
                 SiO4=2.-((temp-13.)*(2./3.))
              elseif (temp.gt.16.) then
                SiO4=0.
              endif
              tclm(i,j,k,iPO4_)=(1.67+0.5873*SiO4+0.0144*SiO4**2
     &                               +0.0003099*SiO4**3)/16.
            endif
            if (.not.got_tclm(iPOC_)) tclm(i,j,k,iPOC_)=0.01
            if (.not.got_tclm(iSIL_)) tclm(i,j,k,iSIL_)=91.51
            if (.not.got_tclm(iPHY_)) tclm(i,j,k,iPHY_)=0.01
            if (.not.got_tclm(iZOO_)) tclm(i,j,k,iZOO_)=0.01
            if (.not.got_tclm(iDOC_)) tclm(i,j,k,iDOC_)=5.
            if (.not.got_tclm(iDIA_)) tclm(i,j,k,iDIA_)=0.01
            if (.not.got_tclm(iMES_)) tclm(i,j,k,iMES_)=0.01
            if (.not.got_tclm(iBSI_)) tclm(i,j,k,iBSI_)=0.0015
            if (.not.got_tclm(iFER_)) tclm(i,j,k,iFER_)=6.e-4
            if (.not.got_tclm(iBFE_)) tclm(i,j,k,iBFE_)=5.e-8
            if (.not.got_tclm(iGOC_)) tclm(i,j,k,iGOC_)=0.01
            if (.not.got_tclm(iSFE_)) tclm(i,j,k,iSFE_)=5.e-8
            if (.not.got_tclm(iDFE_)) tclm(i,j,k,iDFE_)=5.e-8
            if (.not.got_tclm(iDSI_)) tclm(i,j,k,iDSI_)=0.0015
            if (.not.got_tclm(iNFE_)) tclm(i,j,k,iNFE_)=5.e-8
            if (.not.got_tclm(iNCH_)) tclm(i,j,k,iNCH_)=1.e-2*12./55.
            if (.not.got_tclm(iDCH_)) tclm(i,j,k,iDCH_)=1.e-2*12./55.
            if (.not.got_tclm(iNH4_)) tclm(i,j,k,iNH4_)=0.01
#   elif defined BIO_NChlPZD
            if (.not.got_tclm(iChla)) tclm(i,j,k,iChla)=0.08
            if (.not.got_tclm(iPhy1)) tclm(i,j,k,iPhy1)=0.1
            if (.not.got_tclm(iZoo1)) tclm(i,j,k,iZoo1)=0.06
            if (.not.got_tclm(iDet1)) tclm(i,j,k,iDet1)=0.02
#   elif defined BIO_N2ChlPZD2
            if (.not.got_tclm(iNH4_)) tclm(i,j,k,iNH4_)=0.1
            if (.not.got_tclm(iChla)) tclm(i,j,k,iChla)=0.08
            if (.not.got_tclm(iPhy1)) tclm(i,j,k,iPhy1)=0.06
            if (.not.got_tclm(iZoo1)) tclm(i,j,k,iZoo1)=0.04
            if (.not.got_tclm(iDet1)) tclm(i,j,k,iDet1)=0.02
            if (.not.got_tclm(iDet2)) tclm(i,j,k,iDet2)=0.02
#   endif
#  endif /* ANA_TCLIMA */
          enddo
        enddo
      enddo
#  undef SiO4
#  undef temp
# endif /* BIOLOGY */

# ifdef SEDIMENT
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
#  ifdef ANA_TCLIMA
            tclm(i,j,k,isand)=Csed(1)
            tclm(i,j,k,isilt)=Csed(2)
#  else   
            if (.not.got_tclm(isand)) then
              tclm(i,j,k,isand)=Csed(1)
            endif
            if (.not.got_tclm(isilt)) then
              tclm(i,j,k,isilt)=Csed(2)
            endif
#  endif
          enddo
        enddo
      enddo
# endif /* SEDIMENT */

# ifdef PASSIVE_TRACER
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
#  ifdef ANA_TCLIMA
            tclm(i,j,k,itpas)=0.0
#  else
            if (.not.got_tclm(itpas)) then
              tclm(i,j,k,itpas)=0.0
            endif
#  endif
          enddo
        enddo
      enddo
# endif

# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      do itrc=1,NT
#  ifndef ANA_TCLIMA
        if (.not.got_tclm(itrc)) then
#  endif
          call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                            tclm(START_2D_ARRAY,1,itrc))
#  ifndef ANA_TCLIMA
        endif
#  endif
      enddo
# endif

      return
      end
#endif /* TCLIMATOLOGY */
!
!====================================================================
!                   subroutine ana_uclima
!====================================================================
!
#if defined ANA_M2CLIMA && defined M2CLIMATOLOGY ||\
   (defined ANA_M3CLIMA && defined M3CLIMATOLOGY)
      subroutine ana_uclima (tile)
      implicit none
# include "param.h"
      integer tile
#ifdef  ALLOW_SINGLE_BLOCK_MODE
C$    integer  trd, omp_get_thread_num
#endif
# include "compute_tile_bounds.h"
      call ana_uclima_tile   (Istr,Iend,Jstr,Jend)
      return
      end
!---------------------------------------------------------------------
!
      subroutine ana_uclima_tile (Istr,Iend,Jstr,Jend)
!
!---------------------------------------------------------------------
!  This routine sets analytical momentum climatology fields.
!---------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "climat.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i,j,k
# ifdef INTERNAL
      real U0,omega,kwave,V0
# endif
!
# include "compute_auxiliary_bounds.h"
!
# ifdef EW_PERIODIC
#  define IU_RANGE Istr,Iend
#  define IV_RANGE Istr,Iend
# else
#  define IU_RANGE Istr,IendR
#  define IV_RANGE IstrR,IendR
# endif

# ifdef NS_PERIODIC
#  define JU_RANGE Jstr,Jend
#  define JV_RANGE Jstr,Jend
# else
#  define JU_RANGE JstrR,JendR
#  define JV_RANGE Jstr,JendR
# endif
!
# if defined ANA_M2CLIMA && defined M2CLIMATOLOGY
#  if defined REGIONAL
      do j=JstrR,JendR
        do i=IstrR,IendR
          ubclm(i,j)=0.
          vbclm(i,j)=0.
        enddo
      enddo
#  elif defined INTERNAL
      U0=0.02
      omega=2.*pi/(12.4*3600)
      kwave=sqrt(((omega*omega)-(f(1,1)*f(1,1)))/(g*h(1,1)))
      V0=f(1,1)*U0/omega
      do j=JU_RANGE
        do i=IU_RANGE
          ubclm(i,j)=U0*sin(omega*time-kwave*0.5*(xr(i,j)+xr(i-1,j)))
        enddo
      enddo
      do j=JV_RANGE
        do i=IV_RANGE
          vbclm(i,j)=V0*cos(omega*time-kwave*0.5*(xr(i,j)+xr(i,j-1)))
        enddo
      enddo
#  elif defined JET
           ! climatology computed in ana_initial
#  else
      do j=JstrR,JendR
        do i=IstrR,IendR
          ubclm(i,j)=ubar(i,j,1)
          vbclm(i,j)=vbar(i,j,1)
        enddo
      enddo
#  endif
#  if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,  ubclm)
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,  vbclm)
#  endif
# endif
# if defined ANA_M3CLIMA && defined M3CLIMATOLOGY && defined SOLVE3D
#  if defined REGIONAL
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            uclm(i,j,k)=0.
            vclm(i,j,k)=0.
          enddo
        enddo
      enddo
#  elif defined JET
           ! climatology computed in ana_initial
#  else
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            uclm(i,j,k)=u(i,j,k,1)
            vclm(i,j,k)=v(i,j,k,1)
          enddo
        enddo
      enddo
#  endif
#  if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend,  uclm)
      call exchange_v3d_tile (Istr,Iend,Jstr,Jend,  vclm)
#  endif
# endif
# undef IU_RANGE
# undef JU_RANGE
# undef IV_RANGE
# undef JV_RANGE
      return
      end
#endif /* ANA_M2CLIMA && M2CLIMATOLOGY || (ANA_M3CLIMA && M3CLIMATOLOGY) */
!
!====================================================================
!                   subroutine ana_wwave
!====================================================================
!
#if defined ANA_WWAVE && defined BBL
      subroutine ana_wwave (tile)
      implicit none
# include "param.h"
      integer tile
#ifdef  ALLOW_SINGLE_BLOCK_MODE
C$    integer  trd, omp_get_thread_num
#endif
# include "compute_tile_bounds.h"
      call ana_wwave_tile   (Istr,Iend,Jstr,Jend)
      return
      end
!
      subroutine ana_wwave_tile (Istr,Iend,Jstr,Jend)
!
!---------------------------------------------------------------------
!  This routine sets wind induced wave amplitude, direction
!  and period used in the bottom boundary layer formulation.
!---------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "grid.h"
# include "forces.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i,j
!
# include "compute_auxiliary_bounds.h" 
!
!  Set wind induced wave amplitude (m), direction (radians) and
!  period (s) at RHO-points.
!
# if defined SED_TEST2 || defined REGIONAL
      do j=JstrR,JendR
        do i=IstrR,IendR
          Awave(i,j)=1.0
          Dwave(i,j)=270.*deg2rad
          Pwave(i,j)=10.
        enddo
      enddo
# else
      ANA_WWAVE: no values provided for AWAVE, DWAVE, and PWAVE.
       do j=JstrR,JendR
        do i=IstrR,IendR
          Awave(i,j)=???
          Dwave(i,j)=???
          Pwave(i,j)=???
        enddo
      enddo
# endif
      return
      end
#endif /* ANA_WWAVE && BBL */

#if defined SEDIMENT
      subroutine ana_sediment (tile)
      implicit none
# include "param.h"
      integer tile
#ifdef  ALLOW_SINGLE_BLOCK_MODE
C$    integer  trd, omp_get_thread_num
#endif
# include "compute_tile_bounds.h"
      call ana_sediment_tile (Istr,Iend,Jstr,Jend)
      return
      end
!
      subroutine ana_sediment_tile (Istr,Iend,Jstr,Jend)
!
!---------------------------------------------------------------------
!  This routine sets sediment ripple and bed parameters
!---------------------------------------------------------------------
!
      implicit none
# include "param.h"
# include "grid.h"
# include "scalars.h"
# include "sediment.h"
# include "bbl.h"
      integer Istr,Iend,Jstr,Jend, i,j, ilay, itrc
!
#undef DEBUG 
#ifdef DEBUG
      integer ick,jck
      parameter (ick=60, jck=35)
#endif
# include "compute_auxiliary_bounds.h" 
# if defined SED_TEST2 || defined REGIONAL
      do j=JstrR,JendR
        do i=IstrR,IendR
#ifdef BBL 
# ifndef ANA_SEDIMENT 
            if(.not.got_inibed(1)) then 
# endif                    
             Hripple(i,j)=Hrip   ! initial ripple height [m] from .in file
# ifndef ANA_SEDIMENT 
            endif            
            if(.not.got_inibed(2)) then 
# endif                    
             Lripple(i,j)=Lrip   ! initial ripple length [m] from .in file
# ifndef ANA_SEDIMENT 
            endif            
# endif                    
#endif                    
          do ilay=1,NLAY
#ifndef ANA_SEDIMENT
            if(.not.got_inised(1)) then 
#endif                    
              bed_thick(i,j,ilay)=Bthk(ilay) 
#ifndef ANA_SEDIMENT
            endif            
            if(.not.got_inised(2)) then 
#endif                    
              bed_poros(i,j,ilay)=Bpor(ilay)
#ifndef ANA_SEDIMENT
            endif            
            if(.not.got_inised(3)) then 
#endif                    
             do itrc=1,NST
              bed_frac(i,j,ilay,itrc)=Bfr(ilay,itrc)
             enddo
#ifndef ANA_SEDIMENT
            endif            
#endif                    
          enddo

#ifdef DEBUG
         if(j.eq.jck.and.i.eq.ick) then
           write(6,*) '********** ANA_SEDIMENT ***********'
           do itrc=1,NST
             write(6,*) 'Sd(itrc)',Sd(itrc)
             do ilay=1,NLAY
               write(6,*) 'i,j,ilay,itrc',i,j,ilay,itrc
               write(6,*) 'bed_frac',bed_frac(i,j,ilay,itrc)
             enddo
           enddo
           do ilay=1,NLAY
             write(6,*) 'bed_thick',bed_thick(i,j,ilay)
             write(6,*) 'bed_por',bed_poros(i,j,ilay)
           enddo
         endif
#endif
        enddo
      enddo
# else
      ana_sediment: no values provided
       do j=JstrR,JendR
        do i=IstrR,IendR
          do ilay=1,NLAY
            bed_thick(i,j,ilay)=???
            bed_poros(i,j,ilay)=???
            do itrc=1,NST
              bed_frac(i,j,ilay,itrc)=???
            enddo
          enddo
        enddo
      enddo
# endif
      return
      end
#endif /* SEDIMENT */

#if defined PSOURCE && defined ANA_PSOURCE && defined SOLVE3D
!
!-----------------------------------------------------------
!  Set analytical tracer and mass point sources and sinks
!-----------------------------------------------------------
!
      subroutine ana_psource_tile (Istr,Iend,Jstr,Jend) 
      implicit none          
# include "param.h"
# include "scalars.h"
# include "sources.h"
# include "ocean3d.h"
# include "grid.h"
!
      integer is, k, Istr,Iend,Jstr,Jend, i,j
      real cff, cff1, cff2, ramp, Hs
# include "compute_auxiliary_bounds.h" 

      if (iic.eq.ntstart) then

!
! Set-up nondimensional shape Qshape, must add to unity
!
# if defined RIVER
#  define EXP_SHAPE
#  ifdef CST_SHAPE
       cff=1./float(N)
       do k=1,N                         ! Uniform vertical
         do is=1,Nsrc                   ! distribution
           Qshape(is,k)=cff
         enddo
       enddo
#  elif defined EXP_SHAPE
        do is=1,Nsrc                   ! Exponential vertical
          Hs=h(Isrc(is),Jsrc(is))      ! distribution
          cff=5.            ! Hs/z0  (z0 surface layer depth)
          cff1=cff/(1-exp(-cff))
          cff2=0.
          do k=1,N
            Qshape(is,k)=cff1*exp(z_r(Isrc(is),Jsrc(is),k)*cff/Hs)*
     &        (z_w(Isrc(is),Jsrc(is),k)-z_w(Isrc(is),Jsrc(is),k-1))/Hs
            cff2=cff2+Qshape(is,k)
          enddo
          do k=1,N
            Qshape(is,k)=Qshape(is,k)/cff2
          enddo
        enddo
#  elif defined AL_SHAPE
        do is=1,Nsrc                   ! Set-up nondimensional shape
          do k=1,10
            Qshape(is,k)=0.0
          enddo
          do k=11,14
            Qshape(is,k)=0.05
          enddo
          Qshape(is,15)=0.1           ! These most add to unity!
          Qshape(is,16)=0.1           ! These most add to unity!
          Qshape(is,17)=0.1           ! These most add to unity!
          Qshape(is,18)=0.1           ! These most add to unity!
          Qshape(is,19)=0.2
          Qshape(is,20)=0.2
        enddo
#  endif

# elif defined REGIONAL
        do is=1,Nsrc                   ! Exponential vertical
#  ifdef MPI
         i=Isrc_mpi(is,mynode)
         j=Jsrc_mpi(is,mynode)
#  else
         i=Isrc(is)
         j=Jsrc(is)
#  endif
          Hs=h(i,j)      ! distribution
          cff=5.            ! Hs/z0  (z0 surface layer depth)
          cff1=cff/(1-exp(-cff))
          cff2=0.
          do k=1,N
            Qshape(is,k)=cff1*exp(z_r(Isrc(is),Jsrc(is),k)*cff/Hs)*
     &        (z_w(Isrc(is),Jsrc(is),k)-z_w(Isrc(is),Jsrc(is),k-1))/Hs
            cff2=cff2+Qshape(is,k)
          enddo
          do k=1,N
            Qshape(is,k)=Qshape(is,k)/cff2
          enddo
        enddo

# else
      ERROR ###  CPP-key 'ANA_PSOURCE' is defined, but no code
      ERROR ###  is provided to set up Dsrc, Isrc, Jsrc, Lsrc.
# endif /* REGIONAL */

      endif   ! iic.eq.ntstart

!
! Set-up vertically integrated mass transport [m3/s] of point
! sources (these may be time-dependent; positive in the positive U-
! or V-direction and vice-versa) and vertically distribute them
! according to mass transport profile chosen above.
!
# if defined RIVER
      ramp=1 !TANH(dt*sec2day*float(iic-ntstart))
      do is=1,Nsrc
        Qbar(is)=ramp*Qbar(is)
      enddo
# elif defined REGIONAL
      ramp=1 !TANH(dt*sec2day*float(iic-ntstart))
      do is=1,Nsrc
        Qbar(is)=ramp*Qbar(is)
      enddo
# else
      ERROR ###  CPP-key 'ANA_PSOURCE' is defined, but no code
      ERROR ###  is provided to set up Qbar(is) analytically.
# endif
      do is=1,Nsrc
        do k=1,N
          Qsrc(is,k)=Qbar(is)*Qshape(is,k)
        enddo
      enddo
!
!  Set-up tracer (tracer units) point Sources/Sinks.
!
# if defined RIVER
      do k=1,N
        do is=1,Nsrc
          Tsrc(is,k,itemp)=Tsrc0(is,itemp)
!          Tsrc(is,k,itemp)=4.+10.*exp(z_r(Isrc(is),Jsrc(is),k)/50.)
          Tsrc(is,k,isalt)=Tsrc0(is,isalt)
#  if defined PASSIVE_TRACER
          Tsrc(is,k,ipas)=Tsrc0(is,itpas)
#  endif
        enddo
      enddo
# elif defined REGIONAL
      do k=1,N
        do is=1,Nsrc
          Tsrc(is,k,itemp)=Tsrc0(is,itemp)
          Tsrc(is,k,isalt)=Tsrc0(is,isalt)
#  if defined PASSIVE_TRACER
          Tsrc(is,k,itpas)=Tsrc0(is,itpas)
#  endif
#  if defined BIOLOGY
          Tsrc(is,k,iNO3_)=Tsrc0(is,iNO3_)
#  endif
        enddo
      enddo
# else
      ERROR ###  CPP-key 'ANA_PSOURCE' is defined, but no code
      ERROR ###  is provided to set up Tsrc(is) analytically.
# endif
      return
      end
#endif /* PSOURCE && ANA_PSOURCE SOLVE3D*/

#ifdef ANA_BRY
!
!---------------------------------------------------------------------
!  Set analytical boundary forcing
!---------------------------------------------------------------------
!
      subroutine ana_bry (tile)
      implicit none
# include "param.h"
      integer tile
#ifdef  ALLOW_SINGLE_BLOCK_MODE
C$    integer  trd, omp_get_thread_num
#endif
# include "compute_tile_bounds.h"
      call ana_bry_tile   (Istr,Iend,Jstr,Jend)
      return
      end
!
      subroutine ana_bry_tile (Istr,Iend,Jstr,Jend) 
      implicit none          
      integer Istr,Iend,Jstr,Jend, i,j,k, itrc
# include "param.h"
# include "boundary.h"
!
# include "compute_auxiliary_bounds.h"
!
# ifdef OBC_WEST
      if (WESTERN_EDGE) then
#  ifdef Z_FRC_BRY
        do j=JstrR,JendR
          zetabry_west(j)=0.
        enddo
#  endif
#  ifdef M2_FRC_BRY
        do j=JstrR,JendR
          ubarbry_west(j)=0.
          vbarbry_west(j)=0.
        enddo
#  endif
#  ifdef SOLVE3D && (defined M3_FRC_BRY || defined T_FRC_BRY)
        do k=1,N
          do j=JstrR,JendR
#   ifdef M3_FRC_BRY
            ubry_west(j,k)=0.
            vbry_west(j,k)=0.
#   endif
#   ifdef T_FRC_BRY
            do itrc=1,NT
              tbry_west(j,k,itrc)=0.
            enddo
#   endif
          enddo
        enddo
#  endif    /* SOLVE3D && (M3_FRC_BRY || T_FRC_BRY)*/
      endif
# endif /* OBC_WEST */
!
# ifdef OBC_EAST
      if (EASTERN_EDGE) then
#  ifdef Z_FRC_BRY
        do j=JstrR,JendR
          zetabry_east(j)=0.
        enddo
#  endif
#  ifdef M2_FRC_BRY
        do j=JstrR,JendR
          ubarbry_east(j)=0.
          vbarbry_east(j)=0.
        enddo
#  endif
#  ifdef SOLVE3D && (defined M3_FRC_BRY || defined T_FRC_BRY)
        do k=1,N
          do j=JstrR,JendR
#   ifdef M3_FRC_BRY
            ubry_east(j,k)=0.
            vbry_east(j,k)=0.
#   endif
#   ifdef T_FRC_BRY
            do itrc=1,NT
              tbry_east(j,k,itrc)=0.
            enddo
#   endif
          enddo
        enddo
#  endif    /* SOLVE3D && (M3_FRC_BRY || T_FRC_BRY)*/
      endif
# endif /* OBC_EAST */
!
# ifdef OBC_SOUTH
      if (SOUTHERN_EDGE) then
#  ifdef Z_FRC_BRY
        do i=IstrR,IendR
          zetabry_south(i)=0.
        enddo
#  endif
#  ifdef M2_FRC_BRY
        do i=IstrR,IendR
          ubarbry_south(i)=0.
          vbarbry_south(i)=0.
        enddo
#  endif
#  ifdef SOLVE3D && (defined M3_FRC_BRY || defined T_FRC_BRY)
        do k=1,N
          do i=IstrR,IendR
#   ifdef M3_FRC_BRY
            ubry_south(i,k)=0.
            vbry_south(i,k)=0.
#   endif
#   ifdef T_FRC_BRY
            do itrc=1,NT
              tbry_south(i,k,itrc)=0.
            enddo
#   endif
          enddo
        enddo
#  endif    /* SOLVE3D && (M3_FRC_BRY || T_FRC_BRY)*/
      endif
# endif /* OBC_SOUTH */
!
# ifdef OBC_NORTH
      if (NORTHERN_EDGE) then
#  ifdef Z_FRC_BRY
        do i=IstrR,IendR
          zetabry_north(i)=0.
        enddo
#  endif
#  ifdef M2_FRC_BRY
        do i=IstrR,IendR
          ubarbry_north(i)=0.
          vbarbry_north(i)=0.
        enddo
#  endif
#  ifdef SOLVE3D && (defined M3_FRC_BRY || defined T_FRC_BRY)
        do k=1,N
          do i=IstrR,IendR
#   ifdef M3_FRC_BRY
            ubry_north(i,k)=0.
            vbry_north(i,k)=0.
#   endif
#   ifdef T_FRC_BRY
            do itrc=1,NT
              tbry_north(i,k,itrc)=0.
            enddo
#   endif
          enddo
        enddo
#  endif    /* SOLVE3D && (M3_FRC_BRY || T_FRC_BRY)*/
      endif
# endif /* OBC_NORTH */
!
      return
      end
#endif /* defined ANA_BRY */

#if defined BIOLOGY && defined T_FRC_BRY
!
!---------------------------------------------------------------------
!  Set analytical boundary forcing for biological tracers
!---------------------------------------------------------------------
!
      subroutine ana_bry_bio (tile)
      implicit none
      integer tile
# include "param.h"
#ifdef  ALLOW_SINGLE_BLOCK_MODE
C$    integer  trd, omp_get_thread_num
#endif
# include "compute_tile_bounds.h"
      call ana_bry_bio_tile (Istr,Iend,Jstr,Jend)
      return
      end
!
      subroutine ana_bry_bio_tile (Istr,Iend,Jstr,Jend)
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k, itrc
      real    xno3,temp,SiO4
# include "param.h"
# include "boundary.h"
# include "scalars.h"
!
# include "compute_auxiliary_bounds.h"
!
#  ifdef OBC_WEST
      if (WESTERN_EDGE) then
        do k=1,N
          do j=JstrR,JendR
            temp=tbry_west(j,k,itemp)
            if (temp.lt.8.) then
              SiO4=30.
            elseif (temp.ge.8. .and. temp.le.11.) then
              SiO4=30.-((temp-8.)*(20./3.))
            elseif (temp.gt.11. .and. temp.le.13.) then
              SiO4=10.-((temp-11.)*(8./2.))
            elseif (temp.gt.13. .and. temp.le.16.) then
              SiO4=2.-((temp-13.)*(2./3.))
            elseif (temp.gt.16.) then
              SiO4=0.
            endif
            xno3=1.67+0.5873*SiO4+0.0144*SiO4**2
     &           +0.0003099*SiO4**3
            if(.not.got_tbry(iNO3_)) tbry_west(j,k,iNO3_)=xno3
#    ifdef PISCES
            if(.not.got_tbry(iCAL_)) tbry_west(j,k,iCAL_)=0.01   
            if(.not.got_tbry(iPOC_)) tbry_west(j,k,iPOC_)=0.01
            if(.not.got_tbry(iPHY_)) tbry_west(j,k,iPHY_)=0.01
            if(.not.got_tbry(iZOO_)) tbry_west(j,k,iZOO_)=0.01
            if(.not.got_tbry(iDIA_)) tbry_west(j,k,iDIA_)=0.01
            if(.not.got_tbry(iBSI_)) tbry_west(j,k,iBSI_)=1.5e-3
            if(.not.got_tbry(iBFE_)) tbry_west(j,k,iBFE_)=1.e-2*5.e-6
            if(.not.got_tbry(iGOC_)) tbry_west(j,k,iGOC_)=0.01
            if(.not.got_tbry(iSFE_)) tbry_west(j,k,iSFE_)=1.e-2*5.e-6
            if(.not.got_tbry(iDFE_)) tbry_west(j,k,iDFE_)=1.e-2*5.e-6
            if(.not.got_tbry(iDSI_)) tbry_west(j,k,iDSI_)=1.e-2*0.15
            if(.not.got_tbry(iNFE_)) tbry_west(j,k,iNFE_)=1.e-2*5e-6
            if(.not.got_tbry(iNCH_)) tbry_west(j,k,iNCH_)=1.e-2*12./55.
            if(.not.got_tbry(iDCH_)) tbry_west(j,k,iDCH_)=1.e-2*12./55.
            if(.not.got_tbry(iNH4_)) tbry_west(j,k,iNH4_)=1.e-2
#    elif defined BIO_NChlPZD
            if(.not.got_tbry(iChla)) tbry_west(j,k,iChla)=0.08  
            if(.not.got_tbry(iPhy1)) tbry_west(j,k,iPhy1)=0.1
            if(.not.got_tbry(iZoo1)) tbry_west(j,k,iZoo1)=0.06
            if(.not.got_tbry(iDet1)) tbry_west(j,k,iDet1)=0.02
#    elif defined BIO_N2ChlPZD2
            if(.not.got_tbry(iNH4_)) tbry_west(j,k,iNH4_)=0.1  
            if(.not.got_tbry(iChla)) tbry_west(j,k,iChla)=0.08
            if(.not.got_tbry(iPhy1)) tbry_west(j,k,iPhy1)=0.06
            if(.not.got_tbry(iZoo1)) tbry_west(j,k,iZoo1)=0.04
            if(.not.got_tbry(iDet1)) tbry_west(j,k,iDet1)=0.02
            if(.not.got_tbry(iDet2)) tbry_west(j,k,iDet2)=0.02
#    endif /* PISCES or BIO_NChlPZD or BIO_N2ChlPZD2 */
          enddo
        enddo
      endif
#  endif /* OBC_WEST */   
!
#  ifdef OBC_EAST
      if (EASTERN_EDGE) then
        do k=1,N
          do j=JstrR,JendR
            temp=tbry_east(j,k,itemp)
            if (temp.lt.8.) then
              SiO4=30.
            elseif (temp.ge.8. .and. temp.le.11.) then
              SiO4=30.-((temp-8.)*(20./3.))
            elseif (temp.gt.11. .and. temp.le.13.) then
              SiO4=10.-((temp-11.)*(8./2.))
            elseif (temp.gt.13. .and. temp.le.16.) then
              SiO4=2.-((temp-13.)*(2./3.))
            elseif (temp.gt.16.) then
              SiO4=0.
            endif
            xno3=1.67+0.5873*SiO4+0.0144*SiO4**2
     &           +0.0003099*SiO4**3
            if(.not.got_tbry(iNO3_)) tbry_east(j,k,iNO3_)=xno3
#    ifdef PISCES
            if(.not.got_tbry(iCAL_)) tbry_east(j,k,iCAL_)=0.01   
            if(.not.got_tbry(iPOC_)) tbry_east(j,k,iPOC_)=0.01
            if(.not.got_tbry(iPHY_)) tbry_east(j,k,iPHY_)=0.01
            if(.not.got_tbry(iZOO_)) tbry_east(j,k,iZOO_)=0.01
            if(.not.got_tbry(iDIA_)) tbry_east(j,k,iDIA_)=0.01
            if(.not.got_tbry(iBSI_)) tbry_east(j,k,iBSI_)=1.5e-3
            if(.not.got_tbry(iBFE_)) tbry_east(j,k,iBFE_)=1.e-2*5.e-6
            if(.not.got_tbry(iGOC_)) tbry_east(j,k,iGOC_)=0.01
            if(.not.got_tbry(iSFE_)) tbry_east(j,k,iSFE_)=1.e-2*5.e-6
            if(.not.got_tbry(iDFE_)) tbry_east(j,k,iDFE_)=1.e-2*5.e-6
            if(.not.got_tbry(iDSI_)) tbry_east(j,k,iDSI_)=1.e-2*0.15
            if(.not.got_tbry(iNFE_)) tbry_east(j,k,iNFE_)=1.e-2*5e-6
            if(.not.got_tbry(iNCH_)) tbry_east(j,k,iNCH_)=1.e-2*12./55.
            if(.not.got_tbry(iDCH_)) tbry_east(j,k,iDCH_)=1.e-2*12./55.
            if(.not.got_tbry(iNH4_)) tbry_east(j,k,iNH4_)=1.e-2
#    elif defined BIO_NChlPZD
            if(.not.got_tbry(iChla)) tbry_east(j,k,iChla)=0.08   
            if(.not.got_tbry(iPhy1)) tbry_east(j,k,iPhy1)=0.1
            if(.not.got_tbry(iZoo1)) tbry_east(j,k,iZoo1)=0.06
            if(.not.got_tbry(iDet1)) tbry_east(j,k,iDet1)=0.02
#    elif defined BIO_N2ChlPZD2
            if(.not.got_tbry(iNH4_)) tbry_east(j,k,iNH4_)=0.1   
            if(.not.got_tbry(iChla)) tbry_east(j,k,iChla)=0.08
            if(.not.got_tbry(iPhy1)) tbry_east(j,k,iPhy1)=0.06
            if(.not.got_tbry(iZoo1)) tbry_east(j,k,iZoo1)=0.04
            if(.not.got_tbry(iDet1)) tbry_east(j,k,iDet1)=0.02
            if(.not.got_tbry(iDet2)) tbry_east(j,k,iDet2)=0.02
#    endif /* PISCES or BIO_NChlPZD or BIO_N2ChlPZD2 */
           enddo
         enddo
      endif
#  endif /* OBC_EAST */   
!
#  ifdef OBC_NORTH
       if (NORTHERN_EDGE) then
         do k=1,N
           do j=IstrR,IendR
             temp=tbry_north(j,k,itemp)
             if (temp.lt.8.) then
               SiO4=30.
             elseif (temp.ge.8. .and. temp.le.11.) then
               SiO4=30.-((temp-8.)*(20./3.))
             elseif (temp.gt.11. .and. temp.le.13.) then
               SiO4=10.-((temp-11.)*(8./2.))
             elseif (temp.gt.13. .and. temp.le.16.) then
               SiO4=2.-((temp-13.)*(2./3.))
             elseif (temp.gt.16.) then
               SiO4=0.
             endif
             xno3=1.67+0.5873*SiO4+0.0144*SiO4**2
     &            +0.0003099*SiO4**3
             if(.not.got_tbry(iNO3_)) tbry_north(j,k,iNO3_)=xno3
#    ifdef PISCES
             if(.not.got_tbry(iCAL_)) tbry_north(j,k,iCAL_)=0.01   
             if(.not.got_tbry(iPOC_)) tbry_north(j,k,iPOC_)=0.01
             if(.not.got_tbry(iPHY_)) tbry_north(j,k,iPHY_)=0.01
             if(.not.got_tbry(iZOO_)) tbry_north(j,k,iZOO_)=0.01
             if(.not.got_tbry(iDIA_)) tbry_north(j,k,iDIA_)=0.01
             if(.not.got_tbry(iBSI_)) tbry_north(j,k,iBSI_)=1.5e-3
             if(.not.got_tbry(iBFE_)) tbry_north(j,k,iBFE_)=1.e-2*5.e-6
             if(.not.got_tbry(iGOC_)) tbry_north(j,k,iGOC_)=0.01
             if(.not.got_tbry(iSFE_)) tbry_north(j,k,iSFE_)=1.e-2*5.e-6
             if(.not.got_tbry(iDFE_)) tbry_north(j,k,iDFE_)=1.e-2*5.e-6
             if(.not.got_tbry(iDSI_)) tbry_north(j,k,iDSI_)=1.e-2*0.15
             if(.not.got_tbry(iNFE_)) tbry_north(j,k,iNFE_)=1.e-2*5e-6
            if(.not.got_tbry(iNCH_)) tbry_north(j,k,iNCH_)=1.e-2*12./55.
            if(.not.got_tbry(iDCH_)) tbry_north(j,k,iDCH_)=1.e-2*12./55.
             if(.not.got_tbry(iNH4_)) tbry_north(j,k,iNH4_)=1.e-2
#    elif defined BIO_NChlPZD
             if(.not.got_tbry(iChla)) tbry_north(j,k,iChla)=0.08   
             if(.not.got_tbry(iPhy1)) tbry_north(j,k,iPhy1)=0.1
             if(.not.got_tbry(iZoo1)) tbry_north(j,k,iZoo1)=0.06
             if(.not.got_tbry(iDet1)) tbry_north(j,k,iDet1)=0.02
#    elif defined BIO_N2ChlPZD2
             if(.not.got_tbry(iNH4_)) tbry_north(j,k,iNH4_)=0.1   
             if(.not.got_tbry(iChla)) tbry_north(j,k,iChla)=0.08
             if(.not.got_tbry(iPhy1)) tbry_north(j,k,iPhy1)=0.06
             if(.not.got_tbry(iZoo1)) tbry_north(j,k,iZoo1)=0.04
             if(.not.got_tbry(iDet1)) tbry_north(j,k,iDet1)=0.02
             if(.not.got_tbry(iDet2)) tbry_north(j,k,iDet2)=0.02
#    endif /* PISCES or BIO_NChlPZD or BIO_N2ChlPZD2 */
           enddo
         enddo
      endif
#  endif /* OBC_NORTH */   

#  ifdef OBC_SOUTH
       if (SOUTHERN_EDGE) then
         do k=1,N
           do j=IstrR,IendR
             temp=tbry_south(j,k,itemp)
             if (temp.lt.8.) then
               SiO4=30.
             elseif (temp.ge.8. .and. temp.le.11.) then
               SiO4=30.-((temp-8.)*(20./3.))
             elseif (temp.gt.11. .and. temp.le.13.) then
               SiO4=10.-((temp-11.)*(8./2.))
             elseif (temp.gt.13. .and. temp.le.16.) then
               SiO4=2.-((temp-13.)*(2./3.))
             elseif (temp.gt.16.) then
               SiO4=0.
             endif
             xno3=1.67+0.5873*SiO4+0.0144*SiO4**2
     &            +0.0003099*SiO4**3 
             if(.not.got_tbry(iNO3_)) tbry_south(j,k,iNO3_)=xno3
#    ifdef PISCES
             if(.not.got_tbry(iCAL_)) tbry_south(j,k,iCAL_)=0.01   
             if(.not.got_tbry(iPOC_)) tbry_south(j,k,iPOC_)=0.01
             if(.not.got_tbry(iPHY_)) tbry_south(j,k,iPHY_)=0.01
             if(.not.got_tbry(iZOO_)) tbry_south(j,k,iZOO_)=0.01
             if(.not.got_tbry(iDIA_)) tbry_south(j,k,iDIA_)=0.01
             if(.not.got_tbry(iBSI_)) tbry_south(j,k,iBSI_)=1.5e-3
             if(.not.got_tbry(iBFE_)) tbry_south(j,k,iBFE_)=1.e-2*5.e-6
             if(.not.got_tbry(iGOC_)) tbry_south(j,k,iGOC_)=0.01
             if(.not.got_tbry(iSFE_)) tbry_south(j,k,iSFE_)=1.e-2*5.e-6
             if(.not.got_tbry(iDFE_)) tbry_south(j,k,iDFE_)=1.e-2*5.e-6
             if(.not.got_tbry(iDSI_)) tbry_south(j,k,iDSI_)=1.e-2*0.15
             if(.not.got_tbry(iNFE_)) tbry_south(j,k,iNFE_)=1.e-2*5e-6
            if(.not.got_tbry(iNCH_)) tbry_south(j,k,iNCH_)=1.e-2*12./55.
            if(.not.got_tbry(iDCH_)) tbry_south(j,k,iDCH_)=1.e-2*12./55.
             if(.not.got_tbry(iNH4_)) tbry_south(j,k,iNH4_)=1.e-2
#    elif defined BIO_NChlPZD
             if(.not.got_tbry(iChla)) tbry_south(j,k,iChla)=0.08   
             if(.not.got_tbry(iPhy1)) tbry_south(j,k,iPhy1)=0.1
             if(.not.got_tbry(iZoo1)) tbry_south(j,k,iZoo1)=0.06
             if(.not.got_tbry(iDet1)) tbry_south(j,k,iDet1)=0.02
#    elif defined BIO_N2ChlPZD2
             if(.not.got_tbry(iNH4_)) tbry_south(j,k,iNH4_)=0.1 
             if(.not.got_tbry(iChla)) tbry_south(j,k,iChla)=0.08
             if(.not.got_tbry(iPhy1)) tbry_south(j,k,iPhy1)=0.06
             if(.not.got_tbry(iZoo1)) tbry_south(j,k,iZoo1)=0.04
             if(.not.got_tbry(iDet1)) tbry_south(j,k,iDet1)=0.02
             if(.not.got_tbry(iDet2)) tbry_south(j,k,iDet2)=0.02
#    endif /* PISCES or BIO_NChlPZD or BIO_N2ChlPZD2 */
           enddo
         enddo
      endif
#  endif /* OBC_SOUTH */   
      return
      end
#else 
      subroutine empty_analytical
      return
      end
#endif /* BIOLOGY && defined T_FRC_BRY */


