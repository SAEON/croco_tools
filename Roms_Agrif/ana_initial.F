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
#ifdef ANA_INITIAL
      subroutine ana_initial (tile)
      implicit none
# include "param.h"
       integer tile, trd
C$    integer omp_get_thread_num
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
# ifdef JET
#  include "private_scratch.h"
# endif
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
# ifdef JET
      integer j0,j1,j2,j3,j4,j5,rhoinv
      real dy,cff,cff1,cff2,cff3,x,y,Fyv,
     &     rhomaxs,bgdrhos,zs1,dzs,drhos,drhosfs,
     &     rhomaxn,bgdrhon,zn1,dzn,drhon,drhosfn,
     &     drhosf,z0,z0p,dzs_a,dzn_a,zk,
     &     Ljet,ywidth,ydist,jmidjet,cff_perturb,
     &     mssh,ierr,tanh
      real rhoprof(0:N,2)
      real ru(PRIVATE_2D_SCRATCH_ARRAY,N),   
     &     rv(PRIVATE_2D_SCRATCH_ARRAY,N),   
     &      P(PRIVATE_2D_SCRATCH_ARRAY,N), 
     &     FC(PRIVATE_2D_SCRATCH_ARRAY),
     &    dZx(PRIVATE_2D_SCRATCH_ARRAY),
     &     rx(PRIVATE_2D_SCRATCH_ARRAY),
     &    dRx(PRIVATE_2D_SCRATCH_ARRAY)
      real tempy(0:MMm+1),fthy(0:MMm+1)
#  ifdef MPI
      real allmssh(1,NNODES)
#   include "mpif.h"
#  endif
      real asymth,zz,zasym,dzasym
      asymth(zz,zasym,dzasym) = 
     &         (zz-zasym)
     &         *sqrt(1+0.5*((zz-zasym)+abs(zz-zasym))**2/dzasym**2)
     &         +zasym
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
   || defined RIVER     || defined INTERNAL  || defined REGIONAL  \
   || defined JET       || defined INNERSHELF 
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
          fac=cff2*(2.*cff3/(1.+cff3*cff3))**2 zeta(i,j,1)
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
   || defined RIVER     || defined INTERNAL  || defined REGIONAL  \
   || defined JET       || defined INNERSHELF
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
#   ifdef INNERSHELF_EKMAN
            dy=Length_ETA/float(MMm) 
            t(i,j,k,1,itemp)=15.
#   else
#    define Z0 (-80.)
#    define THKNSS 50.
#    define Z1 0.
#    define STRAT 1000.
            t(i,j,k,1,itemp)=14.+5.*THKNSS*log(
     &                         cosh((z_w(i,j,k  )-Z0)/THKNSS)
     &                        /cosh((z_w(i,j,k-1)-Z0)/THKNSS)
     &                            )/(z_w(i,j,k)-z_w(i,j,k-1))
     &               +((z_w(i,j,k)+z_w(i,j,k-1))/2.-Z1)/STRAT
#    undef Z0
#    undef THKNSS
#    undef Z1
#    undef STRAT
#   endif
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
               SiO4=10.-((tdy=Length_ETA/float(MMm) emp-11.)*(8./2.))
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
            t(i,j,k,2,iBSI_)=t(i,j,k,1, zeta(i,j,1)iBSI_)
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
#    elif defined BIO_N2ChlPZD2
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
#    endif
#   endif /* BIOLOGY */
          enddo
        enddo
      enddo

# elif defined JET
!
! ---------------------------------------------------------------------
! BAROCLINIC JET TEST CASE
! ---------------------------------------------------------------------
!
! --- Jet and density profile parameters ---
!
      Ljet        = 1600.e3;  ! Jet width in km
      cff_perturb = 0.02      ! zonal perturbation coefficient
!
! Xavier's SETUP1 parameters  (Phillips > Charney mode)
!
! southern profile
      rhomaxs=27.75     ! max density
      bgdrhos=9.8e-6    ! background stratification
      zs1=-1000
      dzs=700
      drhos=1.4         ! gives strength of Phillips mode (0 to 1.4)
! northern profile 
      rhomaxn=27.7573   ! max density
      bgdrhon=9.8e-6    ! background stratification
      zn1=-400 
      dzn=300
! surface Charney mode 
      drhosf=0.00       ! strength of Charney mode (canceled for 1.05, max for 0)
      drhosfs=1.5       ! northern anomaly  (0 for Phillips mode only)
      drhosfn=0.0       ! southern anomaly  (0 for Phillips mode only)
      z0=-300.          ! vertical penetration (-600 for strong Charney mode)
      z0p=-110.
!
! --- Build northern and southern density profiles ---
!
! First compute background density profiles associated with
! a gentle stratification that does not depend 
! on jet side. It is there to ensure static stability. 
      do k=1,N
        zk = z_r(istr,jstrR,k)
        rhoprof(k,1)=rhomaxs-bgdrhos*(zk+h(istr,jstrR))
        rhoprof(k,2)=rhomaxn-bgdrhon*(zk+h(istr,jstrR))
      enddo
!
! Second, get main north/south contrast with a distorded 
! tanh shape of variable amplitude.Distorsion of the tanh 
! is done with the asym function that increases 
! stratification in the upper ocean
      dzs_a=1.3*dzs
      do k=1,N
        zk=asymth(z_r(istr,jstrR,k),zs1,dzs_a)
        rhoprof(k,1)=rhoprof(k,1)-drhos*(0.5+0.5*tanh((zk-zs1)/dzs))
      enddo
      dzn_a=1.3*dzn
      drhon=-(rhoprof(N,1)-rhoprof(N,2))/(0.5+0.5*tanh((zk-zn1)/dzn))
      do k=1,N
        zk=asymth(z_r(istr,jstrR,k),zn1,dzn_a)
        rhoprof(k,2)=rhoprof(k,2)-drhon*(0.5+0.5*tanh((zk-zn1)/dzn))
      enddo
      do k=1,N
        zk = z_r(istr,jstrR,k)
        rhoprof(k,1)=rhoprof(k,1)
     &             -drhosf*(exp((zk-z0p)/abs(z0p)))/(exp(1.))
        rhoprof(k,1)=rhoprof(k,1)
     &             -drhosfs*0.5*(1+tanh((zk-z0)/abs(z0)))/tanh(1.)
        rhoprof(k,2)=rhoprof(k,2)
     &             -drhosf*(exp((zk-z0p)/abs(z0p)))/(exp(1.))
        rhoprof(k,2)=rhoprof(k,2)
     &             -drhosfn*0.5*(1+tanh((zk-z0)/abs(z0)))/tanh(1.)
      enddo
!
! --- Fit southern and northern profiles ---
!
!     use linear and sin^2 functions and add perturbation 
!     fthy is the shape integral in y
!
      ywidth = Ljet/2
      ydist  = el-ywidth
      dy     = el/float(MMm)
!
      j0 = 0
      j1 = j0 + int(ydist/dy)
      j2 = j1 + int(ywidth/dy)
      j3 = j2
      j4 = j3 + int(ywidth/dy)
      j5 = MMm+1
      jmidjet=j2
      cff1=pi*0.5/float(j2-j1)
      cff3=pi*0.5/float(j4-j3)
       do j=j0,j1
          tempy(j)=0.
       enddo
       do j=j4,j5
          tempy(j)=0.
       enddo
       do j=j1+1,j2
          tempy(j)= sin(float(j-j1)*cff1)**2
       enddo
       do j=j2+1,j3
          tempy(j)=1.
       enddo
       do j=j3+1,j4
          tempy(j)= cos(float(j-j3)*cff3)**2
       enddo
! Integrate in y to find fthy.
! tmpy is at rho (u) points, fthy at psi (v) points.
       fthy(0)=0.0
       do j=1,MMm+1
          fthy(j)=fthy(j-1)+(tempy(j)+tempy(j-1))*0.5
       enddo
       cff3=fthy(MMm+1)
       do j=0,MMm+1
          fthy(j)=fthy(j)/cff3
       enddo
! fill in rho and add XI perturbation
       do k=1,N
         do j=0,Mm+1
           do i=0,Lm+1
#  ifdef MPI
             y=j+jj*Mm
             x=i+ii*Lm
#  else
             y=j
             x=i
#  endif
             y=y/real(MMm)-0.5
             x=x/real(LLm-2)
             y= y + cff_perturb*exp(z_r(i,j,k)/1000.)
     &                         *( 0.5*sin(2.*pi*x+pi/7.)
     &                           +0.3*sin(4.*pi*x+pi/13.) )
             y=y*pi*el/Ljet+pi/2 ! mean jet center at y=pi/2
             if(y.lt.0)then
               Fyv=0.5
             elseif(y.gt.pi)then
               Fyv=-0.5
             else
               Fyv=0.5-(y-sin(y)*cos(y))/pi
             endif
             Fyv=Fyv+0.5
             rho(i,j,k)=Fyv*rhoprof(k,1)+(1.-Fyv)*rhoprof(k,2)
           enddo
         enddo
       enddo
       do k=1,N
         do j=1,Mm
           do i=1,Lm
             t(i,j,k,1,1)=rho(i,j,k)
           enddo
         enddo
       enddo
#  if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_r3d_tile (istr,iend,jstr,jend,
     &                        t(START_2D_ARRAY,1,1,1))
#  endif
      rho(:,:,:)=t(:,:,:,1,1)

!
! --- Get surface pressure (and zeta) using pressure gradient routine ---
!
      call prsgrd_tile (istr,iend,jstr,jend, ru,rv, P,
     &                  A2d(1,3,0),A2d(1,4,0), FC,dZx,rx,dRx)
      do j=jstr,jend
        do i=istr,iend
          zeta(i,j,1)=-P(i,j,1)/g
        enddo
      enddo
!
! removes mean ssh level
!
      mssh=0.
      do j=jstr,jend
        do i=istr,iend
           mssh=mssh+zeta(i,j,1)
        enddo
      enddo
      mssh=mssh/(jend-jstr+1)/(iend-istr+1)
#  ifdef MPI
      call MPI_ALLGATHER(mssh,1,MPI_DOUBLE_PRECISION,
     &                   allmssh,1,MPI_DOUBLE_PRECISION,
     &                   MPI_COMM_WORLD,ierr)
      mssh=0.
      do i=1,NNODES
        mssh=mssh+allmssh(1,i)
      enddo
      mssh=mssh/real(NNODES)
#  endif
      do j=jstr,jend
        do i=istr,iend
          zeta(i,j,1)=zeta(i,j,1)-mssh
        enddo
      enddo
!
! Apply zeta boundary conditions
!
      if (SOUTHERN_EDGE) then
        do i=istr,iend                     ! Southern edge gradient BC
          zeta(i,jstr-1,1)=zeta(i,jstr,1)
          enddo
      endif
      if (NORTHERN_EDGE) then
        do i=istr,iend                     ! Northern edge gradient BC
          zeta(i,jend+1,1)=zeta(i,jend,1)
        enddo
      endif
#  if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_r2d_tile (istr,iend,jstr,jend,
     &                        zeta(START_2D_ARRAY,1))
#  endif
!
! --- Get gressure gradient using updated zeta field ---
!
      call set_depth_tile (istr,iend,jstr,jend)
      call prsgrd_tile (istr,iend,jstr,jend, ru,rv, P,
     &                  A2d(1,3,0),A2d(1,4,0), FC,dZx,rx,dRx)
#  ifndef MPI
      P(:,0,:)=P(:,1,:)
      P(:,Mm+1,:)=P(:,Mm,:)
      rv(:,0,:)=rv(:,1,:)
      rv(:,Mm+1,:)=rv(:,Mm,:)
      ru(:,0,:)=0.
      ru(:,Mm+1,:)=0.
#  else
      rho1(PRIVATE_2D_SCRATCH_ARRAY,:)=rv
      call exchange_r3d_tile (istr,iend,jstr,jend,
     &                        rho1(START_2D_ARRAY,1))
      if (jj.eq.0) then
        rho1(:,0,:)=rho1(:,1,:)
      endif
      if (jj.eq.NP_ETA-1) then
        rho1(:,Mm+1,:)=rho1(:,Mm,:)
      endif
      rv=rho1(PRIVATE_2D_SCRATCH_ARRAY,:)
!
      rho1(PRIVATE_2D_SCRATCH_ARRAY,:)=ru
      call exchange_r3d_tile (istr,iend,jstr,jend,
     &                        rho1(START_2D_ARRAY,1))
      if (jj.eq.0) then
        rho1(:,0,:)=0.
      endif
      if (jj.eq.NP_ETA-1) then
        rho1(:,Mm+1,:)=0.
      endif
      ru=rho1(PRIVATE_2D_SCRATCH_ARRAY,:)
#  endif    
!
! --- Compute geostrophic u,v velocities ---
!
      do k=1,N
        do j=jstr,jend
          do i=istr,iend
            cff=0.5*(pm(i,j)+pm(i,j-1))
     &             *(pn(i,j)+pn(i,j-1))
     &            /(Hz(i,j,k)+Hz(i,j-1,k))/f(i,j)
            u(i,j,k,1) = cff*rv(i,j,k)
          enddo
        enddo
      enddo
      do k=1,N
        do j=jstr,jend
          do i=istr,iend
            cff=0.5*(pm(i,j)+pm(i-1,j))
     &                *(pn(i,j)+pn(i-1,j))
     &                /(Hz(i,j,k)+Hz(i-1,j,k))/f(i,j)
            v(i,j,k,1) =-cff*ru(i,j,k)
          enddo
        enddo
      enddo
!
! --- Compute barotropic u velocity ---
!
      do k=1,N
        do j=JR_RANGE
          do i=IU_RANGE
            ubar(i,j,1)=ubar(i,j,1)+u(i,j,k,1)
     &                   *0.5*(Hz(i-1,j,k)+Hz(i,j,k))
          enddo
        enddo
      enddo
      do j=JR_RANGE
        do i=IU_RANGE
          ubar(i,j,1) = ubar(i,j,1) /
     &                  ( 0.5*(zeta(i,j,1)+zeta(i-1,j,1))
     &                  + 0.5*(h(i,j)+h(i-1,j)) )
        enddo
      enddo
!
! --- Compute barotropic u velocity ---
!
      do k=1,N
         do j=JV_RANGE
            do i=IR_RANGE
               vbar(i,j,1)=vbar(i,j,1)+v(i,j,k,1)
     &                   *0.5*(Hz(i,j-1,k)+Hz(i,j,k))
            enddo
         enddo
      enddo
      do j=JV_RANGE
         do i=IR_RANGE
            vbar(i,j,1) = vbar(i,j,1) / 
     &                ( 0.5*(zeta(i,j,1)+zeta(i,j-1,1))
     &                + 0.5*(h(i,j)+h(i,j-1)) )
         enddo
      enddo
# if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,
     &                        ubar(START_2D_ARRAY,1))
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,
     &                        vbar(START_2D_ARRAY,1))
# endif

!
! -------- END OF JET INITIALISATION ---------
!

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
