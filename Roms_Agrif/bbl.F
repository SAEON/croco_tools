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
#ifdef BBL
      subroutine bblm (tile)
!====================================================================!
!  Copyright (c) 2002 Rutgers/UCLA                                   !
!=============================================== Meinte Blaas 2002 ==!
!                                                                    !
!  Compute bottom stresses for combined waves & currents             !
!  using the parametric approximation by Soulsby 1997:               !
!  t_cw = t_c[1+1.2(t_w/(t_c+t_w))^3.2]                              !
!  in which                                                          !
!     t_cw = the combined wave-averaged stress (in current dir)      !
!     t_c  = stress due to currents if waves would be absent         ! 
!     t_w  = amplitude of stress due to waves without currents       !
!  and                                                               !
!     t_cw_max = SQRT([t_cw+t_w cos(phi_cw)]^2 + [t_w sin(phi_cw)]^2)!
!     in which                                                       !
!       t_cw_max ~ the maximum combined wave-averaged stress         !
!       phi_cw   = the angle between current and waves               !
!--------------------------------------------------------------------!
! References:
!
! Dyer 1986, Coastal & Estuarine Sediment Dynamics, Wiley, 342 pp.
! Harris & Wiberg 2001, Comp. & Geosci. 27, 675-690
! Li & Amos 2001, Comp. & Geosci. 27, 619-645
! Soulsby 1997, Dynamics of Marine Sands, Telford  Publ., 249 pp.
! Soulsby 1995, Bed shear-stresses due to combined waves and currents,
!  in: Stive et al: Advances in Coastal Morphodynamics, Wiley, 4.20-4.23
! Wiberg & Harris 1994, J. Geophys. Res. 99(C4), 775-789
!====================================================================!
      implicit none
      integer  tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
!
# include "compute_tile_bounds.h"
!
      trd=omp_get_thread_num()
      call bblm_tile (Istr,Iend,Jstr,Jend,
     &                  A2d(1, 1,trd),A2d(1, 2,trd),A2d(1, 3,trd),
     &                  A2d(1, 4,trd),A2d(1, 5,trd),A2d(1, 6,trd),
     &                  A2d(1, 7,trd))
      return
      end

!********************************************************************!
      SUBROUTINE bblm_tile (Istr,Iend,Jstr,Jend,Ub,Zr,Ur,Vr,Umag,
     &                      tauc,tauw)
!********************************************************************!
!  output: bu/vstr = tauc = t_cw 
!     effective wave-averaged bottom stress
!     (after applying apparent roughness)  
!     bu/vstrw = tauw = t_cw_max
!     maximum skin-frictional bottom stress
!     rheight = ripple height (m)
!     rlength = ripple length (m)
!                                
!  input: Awave, Pwave, Dwave, h, Zob, d50, u,v
!         NB! Dwave is assumed to be direction waves are coming from
!             positive clockwise wrt North      
!********************************************************************!
      implicit none
# include "param.h"
# include "bbl.h"
# include "forces.h"
# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
# include "sediment.h"

      logical
     &        SAND, SILT

      integer
     &        Iend, Istr, Jend, Jstr, i, ised, j

      real
     &  Ab, anglec, anglew, cff, cff1, cff2, d50,
     &  Fwave, Kbh, Kbh2, Kdh, phic, phicw, 
     &  rhbio, rhbiomax, rhgt, rhmax, rhmin, rhfac, 
     &  rlbio, rlen, rlmin, rhosed, rhow,
     &  tau_bf, tau_c, tau_cb, tau_ex, tau_up,tau_w,
     &  tau_wb, tau_cs, tau_cw, tau_cwb, tau_cws, tau_en,
     &  thetw, twopi, Ucur, Vcur, Ubc, visk, wset, 
     &  znot, znotc, znot_bl, znot_rip
      parameter (Ubc = 0.01)               

      real
     &            tauc(PRIVATE_2D_SCRATCH_ARRAY),
     &            tauw(PRIVATE_2D_SCRATCH_ARRAY),
     &              Ub(PRIVATE_2D_SCRATCH_ARRAY),
     &            Umag(PRIVATE_2D_SCRATCH_ARRAY),
     &              Ur(PRIVATE_2D_SCRATCH_ARRAY),
     &              Vr(PRIVATE_2D_SCRATCH_ARRAY),
     &              Zr(PRIVATE_2D_SCRATCH_ARRAY)
     
      real
     &        K1, K2, K3, K4, K5, K6
      parameter (K1=0.6666666666, K2=0.3555555555,
     &           K3=0.1608465608, K4=0.0632098765,
     &           K5=0.0217540484, K6=0.0065407983)

      real
     &           scf1, scf2, scf3, scf4, scf5
      parameter (scf1 = 0.5 * 1.39, scf2 = 0.52, 
     &           scf3 = 2.0 - scf2, scf4 = 1.2,
     &           scf5 = 3.2)

      
# include "compute_auxiliary_bounds.h"
!
!---------------------------------------------------------------------
!  Initalize to default values + set b(u,v)str to old time level value.
!---------------------------------------------------------------------
!
      twopi=2.0*pi
      rhbiomax = 6.0d-3 ! maximum biogenic ripple height [m]  
      rhmin = 1.0d-3    ! user-defined minimum ripple height [m]  
      rlmin = 1.0d-2    ! user-defined minimum ripple length [m]  
      rhfac = 1.0d0/EXP(4.11d0)

      do j=JstrV-1,Jend
        do i=IstrU-1,Iend
          tauc(i,j)=0.0       ! [m^2/s^2]
          tauw(i,j)=0.0
        enddo
      enddo
!---------------------------------------------------------------------
!  Set currents above bed.
!---------------------------------------------------------------------
      do j=JstrV-1,Jend+1
        do i=IstrU-1,Iend+1
          Zr(i,j)=z_r(i,j,1)-z_w(i,j,0)
          Ur(i,j)=u(i,j,1,nrhs)
          Vr(i,j)=v(i,j,1,nrhs)
        enddo
      enddo
    
! start main jV, iU loop

      do j=JstrV-1,Jend
        do i=IstrU-1,Iend
          rhbio = 0.0d0
          rlbio = 0.0d0
          rlen = Lripple(i,j)
          rhgt = Hripple(i,j)
          rhow = rho(i,j,1)+1000.0
          visk = 1.3e-3/rhow         ! kinem. viscosity          
!---------------------------------------------------------------------
!  Compute bed wave orbital velocity (m/s) and excursion amplitude
!  (m) from wind-induced waves.  Use Dean & Dalrymple 1991 6th degree
!  polynomial expr. to approximate wave number on shoaling water
!---------------------------------------------------------------------
          Fwave=twopi/Pwave(i,j)
          Kdh=h(i,j)*Fwave*Fwave/g
          Kbh2=Kdh*Kdh+
     &         Kdh/(1.0+Kdh*(K1+Kdh*(K2+Kdh*(K3+Kdh*(K4+
     &              Kdh*(K5+K6*Kdh))))))
          Kbh = SQRT(Kbh2)

!  Compute bed wave orbital velocity and excursion amplitude.
!
          Ab=Awave(i,j)/SINH(Kbh)
          Ub(i,j)=Fwave*Ab
# ifdef MASKING
     &                                   *rmask(i,j)
# endif
!
!  Compute bottom current magnitude at RHO-points.
!
          Ucur=0.5*(Ur(i,j)+Ur(i+1,j))
          Vcur=0.5*(Vr(i,j)+Vr(i,j+1))
          Umag(i,j)=SQRT(Ucur*Ucur+Vcur*Vcur)
# ifdef MASKING
     &                                   *rmask(i,j)
# endif
!
!  Compute angle between currents and waves (radians)
!
          if (Ucur.ne.0.0) then
            phic=ATAN2(Vcur,Ucur)
          else
            phic=(pi/2.0)*SIGN(1.0,Vcur)
          endif
          phicw=(3.0*pi)/2.0 -Dwave(i,j)-phic
# ifdef CURVGRID
     &                                  -angler(i,j)
#endif
! if Dwave is dir waves travel TO:
!         phicw=pi/2.0 +Dwave(i,j)-angler(i,j)-phic
!---------------------------------------------------------------------
!  Determine skin roughness from sediment size and
!  set default logarithmic profile consistent with current-only case
!  
! Establish local median grain size for all calculations in this subroutine 
! and determine local values of critical stresses.
! Since most parameterizations have been derived ignoring multiple grain sizes, 
! we apply this single d50 also in the case of mixed beds. 
!---------------------------------------------------------------------
# ifdef ANA_BSEDIM

          d50 = Ssize(i,j)             ! [m]
          tau_cb = taucb(i,j)/rhow     ! [m^2/s^2]
                                       ! Critical stress for bedload transport

          wset = w_set(i,j)            ! [m/s]
          rhosed = Sdens(i,j)/rhow     ! [ ] relative density
  
 

# elif defined SEDIMENT
! Compute representative median d50, rho_grain, tau_cb, w_settle using bed_frac 
! For local use only, assume log-normal distribution of size classes

          d50   = 1.0
          tau_cb= 1.0
          wset  = 1.0
          rhosed= 1.0
       
          do ised=1,NST
            d50    = d50   *(Sd  (ised))**bed_frac(i,j,1,ised)
            tau_cb = tau_cb*(tau_ce(ised))**bed_frac(i,j,1,ised) ! [m2/s2] CHECK tau(d50)
            wset   = wset  *(wsed  (ised))**bed_frac(i,j,1,ised) ! [m/s]    
            rhosed = rhosed*(Srho  (ised))**bed_frac(i,j,1,ised)
          enddo 

          rhosed= rhosed/rhow
       
# endif /* ANA_BSEDIM || SEDIMENT */ 

! critical stress for transition to sheet flow  (Li&Amos'01, eq.11)
          tau_up = 0.172*(rhosed-1.)*g*(d50**0.624) ! [m^2/s^2]

! critical stress for initiation to suspended load (Bagnold '66|Li&Amos'01, eq.9)
!         tau_crs = max(0.64*wset*wset,ustr_shields)              ! [m^2/s^2]

! threshold stress for break off (Grant & Madsen,1982)   
          tau_bf = 0.79*(visk**(-0.6))*(((rhosed-1)*g)**0.3)
     &                                  *(d50**0.9)*tau_cb        ! [m^2/s^2]

            znotc= d50/12.0
            znot = MAX(Zob,znotc) 
             
!---------------------------------------------------------------------
!  set tauc acc. to current-only case (max znot & skin friction) [m/s]
!---------------------------------------------------------------------
          cff1 = vonKar/LOG(Zr(i,j)/znot)
          cff2 = MIN(Cdb_max,MAX(Cdb_min,cff1*cff1))
          tauc(i,j)= cff2*Umag(i,j)*Umag(i,j)        ! [m^2/s^2] 
      
          cff1 = vonKar/LOG(Zr(i,j)/znotc)
          tau_cs = cff1*cff1*Umag(i,j)*Umag(i,j)     ! [m^2/s^2] 
 
!---------------------------------------------------------------------
! If significant waves (Ub > Ubc= 0.01 m/s):
!  Wave-current interaction case according to Soulsby 1995.  
!  Otherwise: tauw = tauc for sediment purposes
!---------------------------------------------------------------------

          if(Ub(i,j).gt.Ubc) then 

! Determine skin stresses for pure waves and combined flow (m^2/s^2)
! using Soulsby approximation of the wave friction factor
!  fw = 2*scf1*(znotc/Ab)**scf2;  tauw = fw*Ub^2/2
 
           tau_w = scf1*((znotc*Fwave)**scf2)*(Ub(i,j)**scf3)
 
!  combined wave-current skin stress 
            tau_cw= tau_cs*
     &          (1.+scf4*((tau_w/(tau_w+tau_cs))**scf5))
 
!  Maximum of combined wave-current skin stress component for sediment
            tau_cws = SQRT((tau_cw+tau_w*COS(phicw))**2
     &                     + (tau_w*SIN(phicw))**2) 
            
            tauw(i,j)=tau_cws             ! [m^2/s^2]
                       
!  combined wave-current stress using znot
            tau_w = scf1*((znot*Fwave)**scf2)*(Ub(i,j)**scf3)
    
!  combined wave-current stress 
            tau_cw= tauc(i,j)*
     &          (1.+scf4*((tau_w/(tau_w+tauc(i,j)))**scf5))

# ifdef Z0_BL
!---------------------------------------------------------------------
!  Compute bedload roughness for ripple predictor & sediment purposes.
!  At high transport stages, friction depends on thickness of bedload layer.
!  Shear stress due to combined grain & bedload roughness is to be used 
!  to predict ripples and onset of suspension (Li & Amos 2001)
!---------------------------------------------------------------------

            tau_ex=max((tau_cws-tau_cb),0.0)
            cff=(1./((rhosed-1.)*g*d50))
            znot_bl=17.4*d50*(cff*tau_ex)**0.75
            znotc = znotc + znot_bl

!---------------------------------------------------------------------
!  Compute stresses for sediment purposes [m^2/s^2], 
!  using grain + bedload roughness.
!---------------------------------------------------------------------
            cff1=vonKar/LOG(Zr(i,j)/znotc)
            tau_c = cff1*cff1*Umag(i,j)*Umag(i,j)    
            tau_wb = scf1*((znotc*Fwave)**scf2)*(Ub(i,j)**scf3)
            tau_cw= tau_c*(1.+scf4*((tau_wb/(tau_wb+tau_c))**scf5))
        
!  Maximum of combined wave-current stress component for sediment purposes

            tau_cwb = SQRT((tau_cw+tau_wb*COS(phicw))**2 +
     &                   (tau_wb*SIN(phicw))**2)
            tauw(i,j) = tau_cwb                 ! [m^2/s^2]
    

#  ifdef Z0_RIP        
!---------------------------------------------------------------------
!  Determine bedform roughness  ripple height (m)
!  and ripple length (m) for sandy beds.
!
!  Use structure according to Li & Amos 2001
!  See Li & Amos 2001 for further references
!---------------------------------------------------------------------

! check median grain diameter

             if (d50.ge.0.063d-3) then
    
!enhanced skin stress if pre-exisiting ripples (Nielsen, 1986)
              rhmax=0.8*rlen/pi
              rhgt=max(min(rhmax,rhgt),rhmin)
          
              tau_en=max(tau_cws,tau_cws*(rlen/(rlen-pi*rhgt))**2) 
          
              if ((tau_cws.lt.tau_cb).and.(tau_en.ge.tau_cb)) then 
! local transp.
                rhgt = (19.6*(SQRT(tau_cws/tau_cb))+20.9)*d50
                rlen = rhgt/0.12
              else 
     &          if ((tau_cws.ge.tau_cb).and.(tau_cwb.lt.tau_bf)) then 
! bed load in eq. range
                  rhgt = (22.15*(SQRT(tau_cwb/tau_cb))+6.38)*d50
                  rlen = rhgt/0.12
              else 
     &          if ((tau_cwb.ge.tau_bf).and.(tau_cwb.lt.tau_up)) then 
! break off regime
                  rlen = 535.*d50
                  rhgt = 0.15*rlen*(SQRT(tau_up)-SQRT(tau_cwb))/
     &                          (SQRT(tau_up)-SQRT(tau_bf )) 
              else if  (tau_cwb.ge.tau_up) then 
! sheet flow, plane bed
                rlen = 0.0
                rhgt = 0.0
              else 
! tau_en < tau_cb: no transport, pre-existing conditions
                rhgt=Hripple(i,j) 
                rlen=Lripple(i,j) 
              endif !tau_cws
            endif !d50
       
#  endif /* Z0_BL */
# endif /* Z0_RIP */       

# ifdef Z0_BIO
!---------------------------------------------------------------------
!  Determine (biogenic) bedform roughness  ripple height (m)
!  and ripple length (m) for silty beds 
!  using Harris & Wiberg 2001.
!---------------------------------------------------------------------
!   rlbio -> user defined or input field...
            if (d50.lt.0.063d-3) then
              rlbio = 0.1d0      ! 10 cm default biogenic r.length (Wheatcroft 1994) 
              thetw = tau_cws*(1./((rhosed-1.)*g*d50))
              rhbio = (thetw**(-1.67d0))*rlbio*rhfac
              rhgt = min(rhbio,rhbiomax)
              rlen = rlbio
            endif

# endif /* Z0_BIO */

# if defined Z0_RIP || defined Z0_BIO
!ripple roughness using Grant & Madsen (1982) roughness length

            znot_rip = 0.92*rhgt*rhgt/(max(rlen,rlmin))
            znotc = znotc+znot_rip
!---------------------------------------------------------------------
! Compute bottom stress components based on total roughnes [m/s]^2
!---------------------------------------------------------------------
            cff1  = vonKar/LOG(Zr(i,j)/znotc)
            tau_c = cff1*cff1*Umag(i,j)*Umag(i,j)  
            tau_w = scf1*((znotc*Fwave)**scf2)*(Ub(i,j)**scf3)
            tau_cw= tau_c*(1.+scf4*((tau_w/(tau_w+tau_c))**scf5))

# endif /* Z0_RIP || Z0_BIO */
!---------------------------------------------------------------------
!  Compute effective bottom shear velocity [m/s]
!  relevant for flow and eddy-diffusivities/viscosity
!---------------------------------------------------------------------

            tauc(i,j) = tau_cw
          
          else ! (Ub < Ubc)   

! If current-only: tauw = tauc(skin) for use in sediment.F (ifdef BBL)
! tauc for current still depending on roughness due to current ripples (ifdef Z0_RIP)
            
            tauw(i,j) = tau_cs

# ifdef Z0_RIP    
            if(tau_cs.gt.tau_up) then
              rhgt=0.0
              rlen=0.0
            else if(tau_cs.lt.tau_cb) then
              rhgt=Hripple(i,j) 
              rlen=Lripple(i,j) 
            else
              rlen=1000.*d50               ! Yalin (1964)
              rhgt=7.4*(rlen/100.)**1.19 ! Allen (1970)
            endif
 
            znotc = znotc+0.92*rhgt*rhgt/(max(rlen,rlmin))
        
            cff1=vonKar/LOG(Zr(i,j)/znotc)
            cff2=MIN(Cdb_max,MAX(Cdb_min,cff1*cff1))
            tauc(i,j) = cff2*Umag(i,j)*Umag(i,j)
# endif 
          endif 
!---------------------------------------------------------------------
!  Load variables for output purposes.
!---------------------------------------------------------------------

          Abed(i,j)   = Ab
# ifdef MASKING
     &                                   *rmask(i,j)
# endif
          Hripple(i,j)= rhgt
# ifdef MASKING
     &                                   *rmask(i,j)
# endif
          Lripple(i,j)= rlen
# ifdef MASKING
     &                                   *rmask(i,j)
# endif
         Zbnot(i,j)  = znot
# ifdef MASKING
     &                                   *rmask(i,j)
# endif
         Zbapp(i,j) = znotc
# ifdef MASKING
     &                                   *rmask(i,j)
# endif
       enddo
      enddo  ! end main loop

!---------------------------------------------------------------------
!  Compute kinematic bottom stress components for flow
!  due to combined current and wind-induced waves.
!---------------------------------------------------------------------
!
      do j=Jstr,Jend
        do i=IstrU,Iend
          cff1=0.5*(tauc(i-1,j)+tauc(i,j))
          cff2=0.5*(tauw(i-1,j)+tauw(i,j))
          anglec=Ur(i,j)/(0.5*(Umag(i-1,j)+Umag(i,j)+1.e-10))
          anglew=COS(0.5*(Dwave(i-1,j)+Dwave(i,j)))
          bustr(i,j)=cff1*anglec
# ifdef MASKING
     &                                   *umask(i,j)
# endif
          bustrw(i,j)=cff2*anglew
# ifdef MASKING
     &                                   *umask(i,j)
# endif
          Ubed(i,j)=Ub(i,j)*anglew
# ifdef MASKING
     &                                   *umask(i,j)
# endif

        enddo
      enddo
      do j=JstrV,Jend
        do i=Istr,Iend
          cff1=0.5*(tauc(i,j-1)+tauc(i,j))
          cff2=0.5*(tauw(i,j-1)+tauw(i,j))
          anglec=Vr(i,j)/(0.5*(Umag(i,j-1)+Umag(i,j)+1.e-10))
          anglew=SIN(0.5*(Dwave(i,j-1)+Dwave(i,j)))
          bvstr(i,j)=cff1*anglec
# ifdef MASKING
     &                                   *vmask(i,j)
# endif
          bvstrw(i,j)=cff2*anglew
# ifdef MASKING
     &                                   *vmask(i,j)
# endif
          Vbed(i,j)=Ub(i,j)*anglew
# ifdef MASKING
     &                                   *vmask(i,j)
# endif
        enddo
      enddo
!
!
      return
      end
!====================================================================!
#else
      subroutine bblm_empty
      end
#endif

