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

      subroutine ana_initracer (tile)
      implicit none
#include "param.h"
      integer tile, trd
C$    integer omp_get_thread_num
#include "compute_tile_bounds.h"
      call ana_initracer_tile (Istr,Iend,Jstr,Jend)
      return
      end

      subroutine ana_initracer_tile (Istr,Iend,Jstr,Jend)
!
!--------------------------------------------------------------------
!  Set initial conditions for tracer variables using
!  analytical expressions if not find in netcdf file.
!--------------------------------------------------------------------
!
      implicit none
#include "param.h"
#include "grid.h"
#include "ocean2d.h"
#include "ocean3d.h"
#include "scalars.h"
#include "sediment.h"
#ifdef SST_SKIN
# include "forces.h"
#endif
      integer Istr,Iend,Jstr,Jend, i,j,k
     &      , itrc
#ifdef BIOLOGY
      real temp, SiO4
#endif
!
# include "compute_auxiliary_bounds.h"
!
#ifdef EW_PERIODIC
#  define IR_RANGE IstrR,IendR
#else
#  define IR_RANGE IstrR,IendR
#endif

#ifdef NS_PERIODIC
#  define JR_RANGE Jstr,Jend
#else
#  define JR_RANGE JstrR,JendR
#endif

#ifdef SOLVE3D

# if defined INNERSHELF || defined UPWELLING || defined BASIN    \
  || defined CANYON_A   || defined CANYON_B  || defined EQUATOR  \
  || defined GRAV_ADJ   || defined OVERFLOW  || defined SEAMOUNT \
  || defined SOLITON    || defined SHELFRONT || defined RIVER    \
  || defined VORTEX     || defined INTERNAL  || defined JET
      do k=1,N
        do j=JR_RANGE
          do i=IR_RANGE
            if (.not.got_tini(itemp)) then
              t(i,j,k,1,itemp)=1.e30
              t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
            endif
#  ifdef SALINITY
            if (.not.got_tini(isalt)) then
              t(i,j,k,1,isalt)=1.e30
              t(i,j,k,2,isalt)=t(i,j,k,1,isalt)
            endif
#  endif
#  if defined PASSIVE_TRACER && !defined ANA_PSOURCE
            if (.not.got_tini(itpas)) then
              if (h(i,j).lt.300) then
                t(i,j,k,1,itpas)=1.
              else
                t(i,j,k,1,itpas)=0
              endif
            endif
#  endif

          enddo
        enddo
      enddo
# elif defined REGIONAL
      do k=1,N
        do j=JR_RANGE
          do i=IR_RANGE
            if (.not.got_tini(itemp)) then
#define Z0 (-80.)
#define THKNSS 50.
#define Z1 0.
#define STRAT 1000.
              t(i,j,k,1,itemp)=14.+5.*THKNSS*log(
     &                            cosh((z_w(i,j,k )-Z0)/THKNSS)
     &                           /cosh((z_w(i,j,k-1)-Z0)/THKNSS)
     &                               )/(z_w(i,j,k)-z_w(i,j,k-1))
     &               +((z_w(i,j,k)+z_w(i,j,k-1))/2.-Z1)/STRAT
#undef Z0
#undef THKNSS
#undef Z1
#undef STRAT
              t(i,j,k,2,itemp)=t(i,j,k,1,itemp)
            endif
#  ifdef SALINITY
            if (.not.got_tini(isalt)) then
              t(i,j,k,1,isalt)=35.
              t(i,j,k,2,isalt)=t(i,j,k,1,isalt)
            endif
#  endif
#  if defined PASSIVE_TRACER && !defined ANA_PSOURCE
            if (.not.got_tini(itpas)) then
c              if (h(i,j).lt.300) then
             if (z_r(i,j,k).lt.-800 .and.
     &           z_r(i,j,k).gt.-1200 ) then
                t(i,j,k,1,itpas)=1
              else
                 t(i,j,k,1,itpas)=0
              endif
            endif
#  endif
#  ifdef BIOLOGY
            if (.not.got_tini(iNO3_)) then
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
            endif
#   ifdef PISCES
            if (.not.got_tini(iDIC_))  then
              t(i,j,k,1,iDIC_)=2150.
              t(i,j,k,2,iDIC_)=t(i,j,k,1,iDIC_)
            endif
            if (.not.got_tini(iTAL_))  then
              t(i,j,k,1,iTAL_)=2350.
              t(i,j,k,2,iTAL_)=t(i,j,k,1,iTAL_)
            endif
            if (.not.got_tini(iOXY_))  then
              t(i,j,k,1,iOXY_)=200.
              t(i,j,k,2,iOXY_)=t(i,j,k,1,iOXY_)
            endif
            if (.not.got_tini(iCAL_))  then
              t(i,j,k,1,iCAL_)=0.01
              t(i,j,k,2,iCAL_)=t(i,j,k,1,iCAL_)
            endif
            if (.not.got_tini(iPO4_))  then
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
               t(i,j,k,1,iPO4_)=(1.67+0.5873*SiO4+0.0144*SiO4**2
     &                               +0.0003099*SiO4**3)/16.
               t(i,j,k,2,iPO4_)=t(i,j,k,1,iPO4_)
            endif
            if (.not.got_tini(iPOC_))  then
              t(i,j,k,1,iPOC_)=1.e-3
              t(i,j,k,2,iPOC_)=t(i,j,k,1,iPOC_)
            endif
            if (.not.got_tini(iSIL_))  then
              t(i,j,k,1,iSIL_)=91.51
              t(i,j,k,2,iSIL_)=t(i,j,k,1,iSIL_)
            endif
            if (.not.got_tini(iPHY_))  then
              t(i,j,k,1,iPHY_)=0.01
              t(i,j,k,2,iPHY_)=t(i,j,k,1,iPHY_)
            endif
            if (.not.got_tini(iZOO_))  then
              t(i,j,k,1,iZOO_)=0.01
              t(i,j,k,2,iZOO_)=t(i,j,k,1,iZOO_)
            endif
            if (.not.got_tini(iDOC_))  then
              t(i,j,k,1,iDOC_)=5.
              t(i,j,k,2,iDOC_)=t(i,j,k,1,iDOC_)
            endif
            if (.not.got_tini(iDIA_))  then
              t(i,j,k,1,iDIA_)=0.01
              t(i,j,k,2,iDIA_)=t(i,j,k,1,iDIA_)
            endif
            if (.not.got_tini(iMES_))  then
              t(i,j,k,1,iMES_)=0.01
              t(i,j,k,2,iMES_)=t(i,j,k,1,iMES_)
            endif
            if (.not.got_tini(iBSI_))  then
              t(i,j,k,1,iBSI_)=1.5e-3
              t(i,j,k,2,iBSI_)=t(i,j,k,1,iBSI_)
            endif
            if (.not.got_tini(iFER_))  then
              t(i,j,k,1,iFER_)=6.e-4
              t(i,j,k,2,iFER_)=t(i,j,k,1,iFER_)
            endif
            if (.not.got_tini(iBFE_))  then
              t(i,j,k,1,iBFE_)=1.E-2*5E-6
              t(i,j,k,2,iBFE_)=t(i,j,k,1,iBFE_)
            endif
            if (.not.got_tini(iGOC_))  then
              t(i,j,k,1,iGOC_)=1.e-2
              t(i,j,k,2,iGOC_)=t(i,j,k,1,iGOC_)
            endif
            if (.not.got_tini(iSFE_))  then
              t(i,j,k,1,iSFE_)=1.e-2*5.E-6
              t(i,j,k,2,iSFE_)=t(i,j,k,1,iSFE_)
            endif
            if (.not.got_tini(iDFE_))  then
              t(i,j,k,1,iDFE_)=1.e-2*5.E-6
              t(i,j,k,2,iDFE_)=t(i,j,k,1,iDFE_)
            endif
            if (.not.got_tini(iDSI_))  then
              t(i,j,k,1,iDSI_)=1.e-2*0.15
              t(i,j,k,2,iDSI_)=t(i,j,k,1,iDSI_)
            endif
            if (.not.got_tini(iNFE_))  then
              t(i,j,k,1,iNFE_)=1.e-2*5.E-6
              t(i,j,k,2,iNFE_)=t(i,j,k,1,iNFE_)
            endif
            if (.not.got_tini(iNCH_))  then
              t(i,j,k,1,iNCH_)=1.e-2*12./55.
              t(i,j,k,2,iNCH_)=t(i,j,k,1,iNCH_)
            endif
            if (.not.got_tini(iDCH_))  then
              t(i,j,k,1,iDCH_)=1.e-2*12./55.
              t(i,j,k,2,iDCH_)=t(i,j,k,1,iDCH_)
            endif
            if (.not.got_tini(iNH4_))  then
              t(i,j,k,1,iNH4_)=1.e-2
              t(i,j,k,2,iNH4_)=t(i,j,k,1,iNH4_)
            endif
#   elif defined BIO_NChlPZD
            if (.not.got_tini(iChla)) then
               t(i,j,k,1,iChla)=0.08
               t(i,j,k,2,iChla)=t(i,j,k,1,iChla)
            endif
            if (.not.got_tini(iPhy1)) then
               t(i,j,k,1,iPhy1)=0.1
               t(i,j,k,2,iPhy1)=t(i,j,k,1,iPhy1)
            endif
            if (.not.got_tini(iZoo1)) then
               t(i,j,k,1,iZoo1)=0.06
               t(i,j,k,2,iZoo1)=t(i,j,k,1,iZoo1)
            endif
            if (.not.got_tini(iDet1)) then
               t(i,j,k,1,iDet1)=0.02
               t(i,j,k,2,iDet1)=t(i,j,k,1,iDet1)
            endif
#    ifdef OXYGEN
             if (.not.got_tini(iO2))  then
              t(i,j,k,1,iO2)=250.0
              t(i,j,k,2,iO2)=t(i,j,k,1,iO2)
            endif
#    endif
#   elif defined BIO_N2ChlPZD2
            if (.not.got_tini(iNH4_))  then
               t(i,j,k,1,iNH4_)=0.1
               t(i,j,k,2,iNH4_)=t(i,j,k,1,iNH4_)
            endif
            if (.not.got_tini(iChla)) then
               t(i,j,k,1,iChla)=0.08
               t(i,j,k,2,iChla)=t(i,j,k,1,iChla)
            endif
            if (.not.got_tini(iPhy1)) then
               t(i,j,k,1,iPhy1)=0.06
               t(i,j,k,2,iPhy1)=t(i,j,k,1,iPhy1)
            endif
            if (.not.got_tini(iZoo1)) then
               t(i,j,k,1,iZoo1)=0.04
               t(i,j,k,2,iZoo1)=t(i,j,k,1,iZoo1)
            endif
            if (.not.got_tini(iDet1)) then
               t(i,j,k,1,iDet1)=0.02
               t(i,j,k,2,iDet1)=t(i,j,k,1,iDet1)
            endif
            if (.not.got_tini(iDet2)) then
               t(i,j,k,1,iDet2)=0.02
               t(i,j,k,2,iDet2)=t(i,j,k,1,iDet2)
            endif
#   elif defined BIO_N2P2Z2D2
            if (.not.got_tini(iNH4_))  then
               t(i,j,k,1,iNH4_)=0.1
               t(i,j,k,2,iNH4_)=t(i,j,k,1,iNH4_)
            endif
#    ifdef VAR_CHL_C
            if (.not.got_tini(iChl1)) then
               t(i,j,k,1,iChl1)=0.06
               t(i,j,k,2,iChl1)=t(i,j,k,1,iChl1)
            endif
            if (.not.got_tini(iChl2)) then
               t(i,j,k,1,iChl2)=0.09
               t(i,j,k,2,iChl2)=t(i,j,k,1,iChl2)
            endif
#    endif
            if (.not.got_tini(iPhy1)) then
               t(i,j,k,1,iPhy1)=0.04
               t(i,j,k,2,iPhy1)=t(i,j,k,1,iPhy1)
            endif
            if (.not.got_tini(iPhy2)) then
               t(i,j,k,1,iPhy2)=0.06
               t(i,j,k,2,iPhy2)=t(i,j,k,1,iPhy2)
            endif
            if (.not.got_tini(iZoo1)) then
               t(i,j,k,1,iZoo1)=0.04
               t(i,j,k,2,iZoo1)=t(i,j,k,1,iZoo1)
            endif
            if (.not.got_tini(iZoo2)) then
               t(i,j,k,1,iZoo2)=0.04
               t(i,j,k,2,iZoo2)=t(i,j,k,1,iZoo2)
            endif
            if (.not.got_tini(iDet1)) then
               t(i,j,k,1,iDet1)=0.02
               t(i,j,k,2,iDet1)=t(i,j,k,1,iDet1)
            endif
            if (.not.got_tini(iDet2)) then
               t(i,j,k,1,iDet2)=0.02
               t(i,j,k,2,iDet2)=t(i,j,k,1,iDet2)
            endif
#   endif
#  endif
#  ifdef SEDIMENT
            if (.not.got_tini(isand)) then
              t(i,j,k,1,isand)=Csed(1)
              t(i,j,k,2,isand)=t(i,j,k,1,isand)
            endif
            if (.not.got_tini(isilt)) then
              t(i,j,k,1,isilt)=Csed(2)
              t(i,j,k,2,isilt)=t(i,j,k,1,isilt)
            endif
#  endif
          enddo
        enddo
      enddo
# else
      do itrc=1,NT
        do k=1,N
          do j=JR_RANGE
            do i=IR_RANGE
              t(i,j,k,1,itrc)=???
              t(i,j,k,2,itrc)=???
            enddo
          enddo
        enddo
      enddo
# endif

# ifdef SST_SKIN
          do j=JR_RANGE
            do i=IR_RANGE
              sst_skin(i,j)=t(i,j,N,1,itemp)
              dT_skin(i,j)=0.
            enddo
          enddo
# endif

# if defined EW_PERIODIC || defined NS_PERIODIC || defined  MPI
      do itrc=1,NT
        call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                          t(START_2D_ARRAY,1,1,itrc))
      enddo
#  ifdef SST_SKIN
        call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                          sst_skin(START_2D_ARRAY))
        call exchange_r2d_tile (Istr,Iend,Jstr,Jend,
     &                          dT_skin(START_2D_ARRAY))
#  endif
# endif

#endif /* SOLVE3D */
#undef IR_RANGE
#undef JR_RANGE
      return
      end


