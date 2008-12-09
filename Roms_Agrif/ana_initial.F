!
! $Id: ana_initial.F,v 1.6 2005/09/14 13:00:10 pmarches Exp $
!
#include "cppdefs.h"
#ifdef ANA_INITIAL
      subroutine ana_initial (tile)
      implicit none
# include "param.h"
      integer tile
# include "compute_tile_bounds.h"
      call ana_initial_tile (Istr,Iend,Jstr,Jend)
      return
      end
!*********************************************************************
      subroutine ana_initial_tile (Istr,Iend,Jstr,Jend)
!*********************************************************************
!
!--------------------------------------------------------------------
!  Set initial conditions for momentum and tracer variables using
!  analytical expressions.
!--------------------------------------------------------------------
!
# ifdef AGRIF
      use Agrif_Util
# endif
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i,j,k, itrc
# ifdef SOLITON
      real fac, x,y, x0,y0, cff1,cff2,cff3
# endif
# ifdef BASIN
      real cff1,cff2
# endif
# ifdef INTERNAL
      real cff
# endif
# ifdef BIOLOGY
      real temp, SiO4
# endif
!
# include "compute_auxiliary_bounds.h"
!
# define IR_RANGE IstrR,IendR
# define IU_RANGE  Istr,IendR
# define JR_RANGE JstrR,JendR
# define JV_RANGE  Jstr,JendR
!
!  Initial conditions for free surface and 2D momentum components.
!------------------------------------------------------------------
!
# if defined BASIN      || defined CANYON_A  || defined CANYON_B  \
   || defined EQUATOR   || defined GRAV_ADJ  || defined OVERFLOW  \
   || defined SEAMOUNT  || defined SHELFRONT || defined UPWELLING \
   || defined INNERSHELF || defined INTERNAL || defined REGIONAL  \
   || defined RIVER
      do j=JR_RANGE
        do i=IR_RANGE
          zeta(i,j,1)=0.
          ubar(i,j,1)=0.
          vbar(i,j,1)=0.
        enddo
      enddo
# elif defined SOLITON
#  ifdef AGRIF
      if (Agrif_Root()) then
#  endif
        x0=2.*xl/3.
        y0=el/2.
#  ifdef AGRIF
      else
        x0=2.*Agrif_Parent_xl/3.
        y0=Agrif_Parent_el/2.
      endif
#  endif
      cff1=0.395
      cff2=0.771*(cff1*cff1)
      do j=JR_RANGE
        do i=IR_RANGE
          x=xr(i,j)-x0
          y=yr(i,j)-y0
          cff3=exp(-cff1*x)
          fac=cff2*(2.*cff3/(1.+cff3*cff3))**2
          zeta(i,j,1)=0.25*fac*(6.*y*y+3.)*exp(-0.5*y*y)
        enddo
      enddo
      do j=JR_RANGE
        do i=IU_RANGE
          x=0.5*(xr(i-1,j)+xr(i,j))-x0
          y=0.5*(yr(i-1,j)+yr(i,j))-y0
          cff3=exp(-cff1*x)
          fac=cff2 * (2.*cff3/(1.+cff3*cff3))**2
          ubar(i,j,1)=0.25*fac*(6.*y*y-9.)*exp(-0.5*y*y)
        enddo
      enddo
      do j=JV_RANGE
        do i=IR_RANGE
          x=0.5*(xr(i,j-1)+xr(i,j))-x0
          y=0.5*(yr(i,j-1)+yr(i,j))-y0
          cff3=exp(-cff1*x)
          fac=cff2 * (2.*cff3/(1.+cff3*cff3))**2
          vbar(i,j,1)=2.*fac*y*(-2.*cff1*tanh(cff1*x))
     &                                  *exp(-0.5*y*y)
        enddo
      enddo
# else
      ERROR: ANA_INITIAL: no initials for ubar,vbar,zeta
# endif

# if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                        zeta(START_2D_ARRAY,1))
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,
     &                        ubar(START_2D_ARRAY,1))
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,
     &                        vbar(START_2D_ARRAY,1))
# endif

# ifdef SOLVE3D
!
!  Initial conditions for momentum components [m/s].
!--------------------------------------------------------------------
!
#  if defined BASIN     || defined CANYON_A  || defined CANYON_B  \
   || defined EQUATOR   || defined GRAV_ADJ  || defined OVERFLOW  \
   || defined SEAMOUNT  || defined SHELFRONT || defined UPWELLING \
   || defined INNERSHELF  || defined INTERNAL || defined REGIONAL \
   || defined RIVER
      do k=1,N
        do j=JR_RANGE
          do i=IR_RANGE
            u(i,j,k,1)=0.
            u(i,j,k,2)=0.
            v(i,j,k,1)=0.
            v(i,j,k,2)=0.
          enddo
        enddo
      enddo
#  else
        ERROR: ANA_INITIAL: no initial conditions for u,v.
#  endif
!
!  Initial conditions for tracer type variables.
!--------------------------------------------------------------------
!  Set initial conditions for potential temperature [degC] and
!  salinity [PSU].
!
#  ifdef BASIN
      cff1=(44.690/39.382)**2
      cff2=cff1*(rho0*800./g)*(5.0e-5/((42.689/44.690)**2))
      do k=1,N
        do j=JR_RANGE
          do i=IR_RANGE
            t(i,j,k,1,itemp)=cff2*exp(z_r(i,j,k)/800.)
     &                 *(0.6-0.4*tanh(z_r(i,j,k)/800.))
            t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
          enddo
        enddo
      enddo
#  elif defined CANYON_A
      do k=1,N
        do j=JR_RANGE
          do i=IR_RANGE
            t(i,j,k,1,itemp)=T0
            t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
          enddo
        enddo
      enddo
#  elif defined CANYON_B
      do k=1,N
        do j=JR_RANGE
          do i=IR_RANGE
            t(i,j,k,1,itemp)=3.488*exp(z_r(i,j,k)/800.)
     &        *(1.-0.666666666666*tanh(z_r(i,j,k)/800.))
            t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
          enddo
        enddo
      enddo
#  elif defined EQUATOR
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            t(i,j,k,1,itemp)=10.0
            t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
#    ifdef SALINITY
            t(i,j,k,1,isalt)=35.0
            t(i,j,k,2,isalt)=t(i,j,k,1,isalt)
#    endif
          enddo
        enddo
      enddo
#  elif defined GRAV_ADJ
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
#ifdef MPI
           if (i+iminmpi-1 .LE. LLm/2) then
#else
           if (i .LE. LLm/2) then
#endif            
            t(i,j,k,1,itemp)=T0+5.
            t(i,j,k,2,itemp)=T0+5
            else
            t(i,j,k,1,itemp)=T0
            t(i,j,k,2,itemp)=T0            
            endif
          enddo
        enddo
      enddo
#  elif defined INNERSHELF
      do k=1,N
        do j=JR_RANGE
          do i=IR_RANGE
#   define Z0 (-80.)
#   define THKNSS 50.
#   define Z1 0.
#   define STRAT 1000.
           t(i,j,k,1,itemp)=14.+5.*THKNSS*log(
     &                            cosh((z_w(i,j,k )-Z0)/THKNSS)
     &                           /cosh((z_w(i,j,k-1)-Z0)/THKNSS)
     &                               )/(z_w(i,j,k)-z_w(i,j,k-1))
     &               +((z_w(i,j,k)+z_w(i,j,k-1))/2.-Z1)/STRAT
#   undef Z0
#   undef THKNSS
#   undef Z1
#   undef STRAT
            t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
#   ifdef SALINITY
            t(i,j,k,1,isalt)=35.
            t(i,j,k,2,isalt)=t(i,j,k,1,isalt)
#   endif
          enddo
        enddo
      enddo
#  elif defined  INTERNAL
#   define BVFREQ 2.e-3
      cff=BVFREQ*BVFREQ/g
      do k=1,N
        do j=JR_RANGE
          do i=IR_RANGE
            t(i,j,k,1,itemp)=rho0*(1-cff*z_r(i,j,k))
            t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
          enddo
        enddo
      enddo
#  elif defined OVERFLOW
      do k=1,N
        do j=JR_RANGE
          do i=IR_RANGE

            t(i,j,k,1,itemp)=T0*(0.5-0.5*tanh( yr(i,j)/1000.-25.))

            t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
          enddo
        enddo
      enddo
#  elif defined  RIVER
      do k=1,N
        do j=JR_RANGE
          do i=IR_RANGE
            t(i,j,k,1,itemp)=10.  !4.+10.*exp(z_r(i,j,k)/50.)
            t(i,j,k,1,isalt)=36.
            t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
            t(i,j,k,2,isalt)=t(i,j,k,1,isalt)
          enddo
        enddo
      enddo
#  elif defined SEAMOUNT
      do k=1,N
        do j=JR_RANGE
          do i=IR_RANGE
            t(i,j,k,1,itemp)=5.*15.*exp(z_r(i,j,k)/100.)
            t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
#   ifdef SALINITY
            t(i,j,k,1,isalt)=35.
            t(i,j,k,2,isalt)=t(i,j,k,1,isalt)
#   endif
          enddo
        enddo
      enddo
#  elif defined SHELFRONT
      do k=1,N
        do j=JR_RANGE
          do i=IR_RANGE
            t(i,j,k,1,itemp)=T0+2.5*tanh((yr(i,j)-50000.0)/20000.0)
            t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
            t(i,j,k,1,isalt)=S0  +  tanh((yr(i,j)-50000.0)/20000.0)
            t(i,j,k,2,isalt)=t(i,j,k,1,isalt)
          enddo
        enddo
      enddo
#  elif  defined SED_TEST2 
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
            t(i,j,k,1,itemp)=20.0
            t(i,j,k,1,isalt)=0.0
            t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
            t(i,j,k,2,isalt)=t(i,j,k,1,isalt)
          enddo
        enddo
      enddo
#  elif defined UPWELLING
      do k=1,N
        do j=JR_RANGE
          do i=IR_RANGE
#   define Z0 (-35.)
#   define THKNSS 6.5
#   define Z1 (-75.)
#   define STRAT 150.
           t(i,j,k,1,itemp)=14.+4.*THKNSS*log(
     &                          cosh((z_w(i,j,k )-Z0)/THKNSS)
     &                         /cosh((z_w(i,j,k-1)-Z0)/THKNSS)
     &                             )/(z_w(i,j,k)-z_w(i,j,k-1))
     &               +((z_w(i,j,k)+z_w(i,j,k-1))/2.-Z1)/STRAT
#   undef Z0
#   undef THKNSS
#   undef Z1
#   undef STRAT
            t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
#   ifdef SALINITY
            t(i,j,k,1,isalt)=35.
            t(i,j,k,2,isalt)=t(i,j,k,1,isalt)
#   endif
          enddo
        enddo
      enddo
#  elif defined REGIONAL
      do k=1,N
        do j=JR_RANGE
          do i=IR_RANGE
#   define Z0 (-80.)
#   define THKNSS 50.
#   define Z1 0.
#   define STRAT 1000.
           t(i,j,k,1,itemp)=14.+5.*THKNSS*log(
     &                            cosh((z_w(i,j,k )-Z0)/THKNSS)
     &                           /cosh((z_w(i,j,k-1)-Z0)/THKNSS)
     &                               )/(z_w(i,j,k)-z_w(i,j,k-1))
     &               +((z_w(i,j,k)+z_w(i,j,k-1))/2.-Z1)/STRAT
#   undef Z0
#   undef THKNSS
#   undef Z1
#   undef STRAT
            t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
#   ifdef SALINITY
            t(i,j,k,1,isalt)=35.
            t(i,j,k,2,isalt)=t(i,j,k,1,isalt)
#   endif
#   ifdef BIOLOGY
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
            t(i,j,k,1,iNO3_)=1.67+0.5873*SiO4+0.0144*SiO4**2
     &                               +0.0003099*SiO4**3
            t(i,j,k,2,iNO3_)=t(i,j,k,1,iNO3_)
#  ifdef PISCES
            t(i,j,k,1,iDIC_)=2150.
            t(i,j,k,2,iDIC_)=t(i,j,k,1,iDIC_)
            t(i,j,k,1,iTAL_)=2350.
            t(i,j,k,2,iTAL_)=t(i,j,k,1,iTAL_)
            t(i,j,k,1,iOXY_)=200.
            t(i,j,k,2,iOXY_)=t(i,j,k,1,iOXY_)
            t(i,j,k,1,iCAL_)=0.01
            t(i,j,k,2,iCAL_)=t(i,j,k,1,iCAL_)
            t(i,j,k,1,iPO4_)=t(i,j,k,1,iNO3_)/16.
            t(i,j,k,2,iPO4_)=t(i,j,k,1,iPO4_)
            t(i,j,k,1,iPOC_)=0.01
            t(i,j,k,2,iPOC_)=t(i,j,k,1,iPOC_)
            t(i,j,k,1,iSIL_)=91.51
            t(i,j,k,2,iSIL_)=t(i,j,k,1,iSIL_)
            t(i,j,k,1,iPHY_)=0.01
            t(i,j,k,2,iPHY_)=t(i,j,k,1,iPHY_)
            t(i,j,k,1,iZOO_)=0.01
            t(i,j,k,2,iZOO_)=t(i,j,k,1,iZOO_)
            t(i,j,k,1,iDOC_)=5.
            t(i,j,k,2,iDOC_)=t(i,j,k,1,iDOC_)
            t(i,j,k,1,iDIA_)=0.01
            t(i,j,k,2,iDIA_)=t(i,j,k,1,iDIA_)
            t(i,j,k,1,iMES_)=0.01
            t(i,j,k,2,iMES_)=t(i,j,k,1,iMES_)
            t(i,j,k,1,iBSI_)=1.5e-3
            t(i,j,k,2,iBSI_)=t(i,j,k,1,iBSI_)
            t(i,j,k,1,iFER_)=6.e-4
            t(i,j,k,2,iFER_)=t(i,j,k,1,iFER_)
            t(i,j,k,1,iBFE_)=1.E-2*5E-6
            t(i,j,k,2,iBFE_)=t(i,j,k,1,iBFE_)
            t(i,j,k,1,iGOC_)=0.01
            t(i,j,k,2,iGOC_)=t(i,j,k,1,iGOC_)
            t(i,j,k,1,iSFE_)=0.01*5.E-6
            t(i,j,k,2,iSFE_)=t(i,j,k,1,iSFE_)
            t(i,j,k,1,iDFE_)=0.01*5.E-6
            t(i,j,k,2,iDFE_)=t(i,j,k,1,iDFE_)
            t(i,j,k,1,iDSI_)=0.01*0.15
            t(i,j,k,2,iDSI_)=t(i,j,k,1,iDSI_)
            t(i,j,k,1,iNFE_)=0.01*5.E-6
            t(i,j,k,2,iNFE_)=t(i,j,k,1,iNFE_)
            t(i,j,k,1,iNCH_)=0.01*12./55.
            t(i,j,k,2,iNCH_)=t(i,j,k,1,iNCH_)
            t(i,j,k,1,iDCH_)=0.01*12./55.
            t(i,j,k,2,iDCH_)=t(i,j,k,1,iDCH_)
            t(i,j,k,1,iNH4_)=0.01
            t(i,j,k,2,iNH4_)=t(i,j,k,1,iNH4_)
#  elif defined BIO_NChlPZD
            t(i,j,k,1,iChla)=0.08
            t(i,j,k,2,iChla)=t(i,j,k,1,iChla)
            t(i,j,k,1,iPhy1)=0.1
            t(i,j,k,2,iPhy1)=t(i,j,k,1,iPhy1)
            t(i,j,k,1,iZoo1)=0.06
            t(i,j,k,2,iZoo1)=t(i,j,k,1,iZoo1)
            t(i,j,k,1,iDet1)=0.02
            t(i,j,k,2,iDet1)=t(i,j,k,1,iDet1)
#     ifdef OXYGEN
            t(i,j,k,1,iO2)=250.0
            t(i,j,k,2,iO2)=t(i,j,k,1,iO2)
#     endif
#    elif defined BIO_N2PZD2
            t(i,j,k,1,iNH4_)=0.1
            t(i,j,k,2,iNH4_)=t(i,j,k,1,iNH4_)
            t(i,j,k,1,iPhy1)=0.06
            t(i,j,k,2,iPhy1)=t(i,j,k,1,iPhy1)
            t(i,j,k,1,iZoo1)=0.04
            t(i,j,k,2,iZoo1)=t(i,j,k,1,iZoo1)
            t(i,j,k,1,iDet1)=0.02
            t(i,j,k,2,iDet1)=t(i,j,k,1,iDet1)
            t(i,j,k,1,iDet2)=0.02
            t(i,j,k,2,iDet2)=t(i,j,k,1,iDet2)
#    elif defined BIO_N2P2Z2D2
            t(i,j,k,1,iNH4_)=0.1
            t(i,j,k,2,iNH4_)=t(i,j,k,1,iNH4_)
#     if defined VAR_CHL_C
            t(i,j,k,1,iChl1)=0.06
            t(i,j,k,2,iChl1)=t(i,j,k,1,iChl1)
            t(i,j,k,1,iChl2)=0.09
            t(i,j,k,2,iChl2)=t(i,j,k,1,iChl2)
#     endif
            t(i,j,k,1,iPhy1)=0.04
            t(i,j,k,2,iPhy1)=t(i,j,k,1,iPhy1)
            t(i,j,k,1,iPhy2)=0.06
            t(i,j,k,2,iPhy2)=t(i,j,k,1,iPhy2)
            t(i,j,k,1,iZoo1)=0.04
            t(i,j,k,2,iZoo1)=t(i,j,k,1,iZoo1)
            t(i,j,k,1,iZoo2)=0.04
            t(i,j,k,2,iZoo2)=t(i,j,k,1,iZoo2)
            t(i,j,k,1,iDet1)=0.02
            t(i,j,k,2,iDet1)=t(i,j,k,1,iDet1)
            t(i,j,k,1,iDet2)=0.02
            t(i,j,k,2,iDet2)=t(i,j,k,1,iDet2)
#    endif
#   endif /* BIOLOGY */
          enddo
        enddo
      enddo
#  else
      do k=1,N
        do j=JR_RANGE
          do i=IR_RANGE
            t(i,j,k,1,itemp)=???
            t(i,j,k,2,itemp)=???
            t(i,j,k,1,isalt)=???
            t(i,j,k,2,isalt)=???
          enddo
        enddo
      enddo
#  endif

#  if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend,
     &                         u(START_2D_ARRAY,1,1))
      call exchange_v3d_tile (Istr,Iend,Jstr,Jend,
     &                         v(START_2D_ARRAY,1,1))
      do itrc=1,NT
        call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                   t(START_2D_ARRAY,1,1,itrc))
      enddo
#  endif
# endif /* SOLVE3D */
# undef IR_RANGE
# undef IU_RANGE
# undef JR_RANGE
# undef JV_RANGE
      return
      end

#else
      subroutine ana_initial_empty
      return
      end
#endif /* ANA_INITIAL */
