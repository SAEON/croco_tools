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
   || defined RIVER     || defined INTERNAL  || defined REGIONAL  \
   || defined JET       || defined INNERSHELF 
      do j=JR_RANGE
        do i=IR_RANGE
          zeta(i,j,1)=0.
          zeta(i,j,2)=0.
          ubar(i,j,1)=0.
          vbar(i,j,1)=0.
          ubar(i,j,2)=0.
          vbar(i,j,2)=0.
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
            t(i,j,k,1,itemp)=5/Tcoef
            t(i,j,k,2,itemp)=5/Tcoef
            else
            t(i,j,k,1,itemp)=10/Tcoef
            t(i,j,k,2,itemp)=10/Tcoef           
            endif
          enddo
        enddo
      enddo
#  elif defined INNERSHELF
      do k=1,N
        do j=JR_RANGE
          do i=IR_RANGE
#   ifdef INNERSHELF_EKMAN
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

#  elif defined JET
!
! Compute analytical t and geostrophically adjusted zeta,ub,vb,u,v fields
! with no zonal perturbation first
!  --> store in climatological arrays
!
      call ana_jet_tile (Istr,Iend,Jstr,Jend,0) 
!
! Compute same fields with zonal perturbation 
! --> store in initial arrays
!
      call ana_jet_tile (Istr,Iend,Jstr,Jend,1)

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
     &                        u(START_2D_ARRAY,1,1))
      call exchange_v3d_tile (Istr,Iend,Jstr,Jend,
     &                        v(START_2D_ARRAY,1,1))
      do itrc=1,NT
        call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                          t(START_2D_ARRAY,1,1,itrc))
      enddo
#  endif

# endif /* SOLVE3D */

# if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                        zeta(START_2D_ARRAY,1))
      call exchange_u2d_tile (Istr,Iend,Jstr,Jend,
     &                        ubar(START_2D_ARRAY,1))
      call exchange_v2d_tile (Istr,Iend,Jstr,Jend,
     &                        vbar(START_2D_ARRAY,1))
# endif

# undef IR_RANGE
# undef IU_RANGE
# undef JR_RANGE
# undef JV_RANGE
      return
      end

# ifdef JET
!*********************************************************************
      subroutine ana_jet_tile (Istr,Iend,Jstr,Jend, jet_perturb)
!*********************************************************************
!
!--------------------------------------------------------------------
!  Set initial conditions for momentum and tracer variables using
!  analytical expressions for the JET test case
!--------------------------------------------------------------------
!
#  ifdef AGRIF
      use Agrif_Util
#  endif
      implicit none
#  include "param.h"
#  include "grid.h"
#  include "ocean2d.h"
#  include "ocean3d.h"
#  include "scalars.h"
#  include "private_scratch.h"
#  ifdef CLIMATOLOGY
#   include "climat.h"
#  endif
      integer Istr,Iend,Jstr,Jend, i,j,k, itrc, 
     &        jet_perturb, tndx
      real cff,x,y,Fyz,
     &     rhomaxs,bgdrhos,zs1,dzs,drhos,drhosfs,
     &     rhomaxn,bgdrhon,zn1,dzn,drhon,drhosfn,
     &     drhosf,z0,z0p,dzs_a,dzn_a,zk,
     &     Ljet,cff_perturb,ierr,tanh
      real*QUAD mssh,area,cff1
      real rhoprof(0:N,2)
      real ru(PRIVATE_2D_SCRATCH_ARRAY,N),   
     &     rv(PRIVATE_2D_SCRATCH_ARRAY,N),   
     &      P(PRIVATE_2D_SCRATCH_ARRAY,N), 
     &     FC(PRIVATE_2D_SCRATCH_ARRAY),
     &    dZx(PRIVATE_2D_SCRATCH_ARRAY),
     &     rx(PRIVATE_2D_SCRATCH_ARRAY),
     &    dRx(PRIVATE_2D_SCRATCH_ARRAY)
#  ifdef MPI
      real*QUAD allmssh(1,NNODES),allarea(1,NNODES)
#   include "mpif.h"
#  endif
      real asymth,zz,zasym,dzasym
      asymth(zz,zasym,dzasym) = 
     &         (zz-zasym)
     &         *sqrt(1+0.5*((zz-zasym)+abs(zz-zasym))**2/dzasym**2)
     &         +zasym
!
#  include "compute_auxiliary_bounds.h"
!
! ---------------------------------------------------------------------
! BAROCLINIC JET TEST CASE
! ---------------------------------------------------------------------
!
! --- Jet and density profile parameters ---
!
      Ljet        = 1600.e3;  ! Jet width in km
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
      drhosfs=1.5       ! northern anomaly  (0 for Phillips mode only)
      drhosfn=0.0       ! southern anomaly  (0 for Phillips mode only)
      z0=-300.          ! vertical penetration (-600 for strong Charney mode)
      drhosf=0.00       ! weakens Charney mode (canceled for 1.05)
      z0p=-110.         ! only used for drhosf non zero
!
! zonal perturbation
!
      if (jet_perturb .eq. 0) then
        cff_perturb = 0.0    ! zonal perturbation coefficient
        tndx=2
      else
        cff_perturb = 0.02
        tndx=1
        zeta(:,:,tndx)=0.
        call set_depth_tile (Istr,Iend,Jstr,Jend)
      endif
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
      do k=1,N
        do j=JstrR,JendR
          do i=IstrR,IendR
#  ifdef MPI
            y=j+jj*Mm
            x=i+ii*Lm
#  else
            y=j
            x=i
#  endif
            y=y/real(MMm)-0.5
            x=x/real(LLm-2)
!
! Add vertical attenuation and XI perturbation
!
            y = y + cff_perturb*exp(z_r(i,j,k)/1000.)
     &                          *exp(-(x-0.5)**2/0.05)
     &                          *( 0.5*sin(2.*pi*x)
     &                            +0.5*sin(6.*pi*x) )
!
! Set Jet structure and place its center at y=pi/2
!
             y = y*pi*el/Ljet + pi/2 

             if (y.lt.0) then
               Fyz=1.0
             elseif (y.gt.pi) then
               Fyz=0.0
             else
               Fyz=1.0-(y-sin(y)*cos(y))/pi
             endif
             rho(i,j,k)=Fyz*rhoprof(k,1)+(1.-Fyz)*rhoprof(k,2)
           enddo
         enddo
       enddo

       do k=1,N
         do j=JstrR,JendR
           do i=IstrR,IendR
             t(i,j,k,tndx,1)=rho(i,j,k)
           enddo
         enddo
       enddo

#  if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_r3d_tile (istr,iend,jstr,jend,
     &                        t(START_2D_ARRAY,1,tndx,1))
#  endif
      rho(:,:,:)=t(:,:,:,tndx,1)
!
! --- Compute zeta ---
!
! Get pressure using pressure gradient routine
!
      call prsgrd_tile (Istr,Iend,Jstr,Jend, ru,rv, P,
     &                  A2d(1,3,0),A2d(1,4,0), FC,dZx,rx,dRx)
!
! Compute zeta so as to cancel bottom pressure perturbation
!
      do j=Jstr,Jend
        do i=Istr,Iend
          zeta(i,j,tndx)=-P(i,j,1)/g
        enddo
      enddo
!
! Remove mean sea level (mssh)  !!! MPI DEBUG problems here !!!
!
      mssh=0.
      area=0.
      do j=Jstr,Jend
        do i=Istr,Iend
           cff1=1./(pm(i,j)*pn(i,j))
           mssh=mssh+cff1*zeta(i,j,tndx)
           area=area+cff1
        enddo
      enddo
      mssh=mssh/area
#  ifdef MPI
      call MPI_ALLGATHER(mssh,1,MPI_DOUBLE_PRECISION,
     &                allmssh,1,MPI_DOUBLE_PRECISION,
     &                          MPI_COMM_WORLD,ierr)
      call MPI_ALLGATHER(area,1,MPI_DOUBLE_PRECISION,
     &                allarea,1,MPI_DOUBLE_PRECISION,
     &                          MPI_COMM_WORLD,ierr)
      mssh=0.
      area=0.
      do i=1,NNODES
        mssh=mssh+allmssh(1,i)*allarea(1,i)
        area=area+allarea(1,i)
      enddo
      mssh=mssh/area
#  endif
      do j=Jstr,Jend
        do i=Istr,Iend
          zeta(i,j,tndx)=zeta(i,j,tndx)-mssh
        enddo
      enddo
!
! Apply zeta boundary conditions
!
      if (SOUTHERN_EDGE) then
        do i=Istr,Iend                     ! Southern edge gradient BC
          zeta(i,Jstr-1,tndx)=zeta(i,Jstr,tndx)
          enddo
      endif
      if (NORTHERN_EDGE) then
        do i=istr,iend                     ! Northern edge gradient BC
          zeta(i,Jend+1,tndx)=zeta(i,Jend,tndx)
        enddo
      endif
#  if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_r2d_tile (istr,iend,jstr,jend,
     &                        zeta(START_2D_ARRAY,tndx))
#  endif
!
! --- Get gressure gradient (ru,rv) using updated zeta field ---
!
      call set_depth_tile (Istr,Iend,Jstr,Jend)
      call prsgrd_tile (Istr,Iend,Jstr,Jend, ru,rv, P,
     &                  A2d(1,3,0),A2d(1,4,0), FC,dZx,rx,dRx)

#  define WORK rho1
      WORK(PRIVATE_2D_SCRATCH_ARRAY,:) = rv
#  if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                        WORK(START_2D_ARRAY,1))
#  endif
      if (SOUTHERN_EDGE) then
        WORK(:,0,:)=WORK(:,1,:)
      endif
      if (NORTHERN_EDGE) then
        WORK(:,Mm+1,:)=WORK(:,Mm,:)
      endif
      rv = WORK(PRIVATE_2D_SCRATCH_ARRAY,:)
!
      WORK(PRIVATE_2D_SCRATCH_ARRAY,:) = ru
#  if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      call exchange_r3d_tile (istr,iend,jstr,jend,
     &                        WORK(START_2D_ARRAY,1))
#  endif
      if (SOUTHERN_EDGE) then
        WORK(:,0,:)=0.
      endif
      if (NORTHERN_EDGE) then
        WORK(:,Mm+1,:)=0.
      endif
      ru = WORK(PRIVATE_2D_SCRATCH_ARRAY,:)
#  undef WORK
!
! --- Compute geostrophic u,v velocities ---
!
      do k=1,N
        do j=Jstr,Jend
          do i=Istr,Iend
            cff=0.5*(pm(i,j)+pm(i,j-1))
     &             *(pn(i,j)+pn(i,j-1))
     &            /(Hz(i,j,k)+Hz(i,j-1,k))/f(i,j)
            u(i,j,k,tndx) = cff*rv(i,j,k)
          enddo
        enddo
      enddo
      do k=1,N
        do j=Jstr,Jend
          do i=Istr,Iend
            cff=0.5*(pm(i,j)+pm(i-1,j))
     &                *(pn(i,j)+pn(i-1,j))
     &                /(Hz(i,j,k)+Hz(i-1,j,k))/f(i,j)
            v(i,j,k,tndx) =-cff*ru(i,j,k)
          enddo
        enddo
      enddo
!
! --- Compute barotropic u velocity ---
!
      ubar(:,:,tndx)=0.
      do k=1,N
        do j=Jstr,Jend
          do i=Istr,Iend
            ubar(i,j,tndx)=ubar(i,j,tndx)+u(i,j,k,tndx)
     &                             *0.5*(Hz(i-1,j,k)+Hz(i,j,k))
          enddo
        enddo
      enddo
      do j=Jstr,Jend
        do i=Istr,Iend
          ubar(i,j,tndx) = ubar(i,j,tndx) /
     &                  ( 0.5*(zeta(i,j,tndx)+zeta(i-1,j,tndx))
     &                  + 0.5*(h(i,j)+h(i-1,j)) )
        enddo
      enddo
!
! --- Compute barotropic u velocity ---
!
      vbar(:,:,tndx)=0.
      do k=1,N
        do j=Jstr,Jend
          do i=Istr,Iend
            vbar(i,j,tndx)=vbar(i,j,tndx)+v(i,j,k,tndx)
     &                           *0.5*(Hz(i,j-1,k)+Hz(i,j,k))
          enddo
        enddo
      enddo
      do j=Jstr,Jend
        do i=Istr,Iend
          vbar(i,j,tndx) = vbar(i,j,tndx) / 
     &                ( 0.5*(zeta(i,j,tndx)+zeta(i,j-1,tndx))
     &                + 0.5*(h(i,j)+h(i,j-1)) )
        enddo
      enddo

#  ifdef CLIMATOLOGY
      if (jet_perturb .eq. 0) then
!
! Fill climatological arrays. MPI exchanges are done in analytical
!
        do k=1,N
          do j=Jstr,Jend
            do i=Istr,Iend
              uclm(i,j,k) = u(i,j,k,tndx)
              vclm(i,j,k) = v(i,j,k,tndx)
              tclm(i,j,k,1) = t(i,j,k,tndx,1)
            enddo
          enddo
        enddo
        do j=Jstr,Jend
          do i=Istr,Iend
            ssh(i,j) = zeta(i,j,tndx)
          enddo
        enddo
        do j=Jstr,Jend
          do i=Istr,Iend
            ubclm(i,j) = ubar(i,j,tndx)
          enddo
        enddo
        do j=Jstr,Jend
          do i=Istr,Iend
            vbclm(i,j) = vbar(i,j,tndx)
          enddo
        enddo
      endif
#  endif

      return
      end
# endif /* JET */

#else
      subroutine ana_initial_empty
      return
      end
#endif /* ANA_INITIAL */
