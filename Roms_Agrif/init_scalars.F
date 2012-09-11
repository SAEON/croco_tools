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

       subroutine init_scalars (ierr)
!
! Set initial values for  globally accessable (stored in common
! blocks) scalar variables. Typically this is associated with
! model utility switches, i/o contlol, time indices, global
! summation counters, etc; NONE of the physical parameters of
! the model is set here.
!
      implicit none
      integer ierr, i,j,itrc, lvar, lenstr
#if defined ONLINE
      integer blkvar_id
#endif /* ONLINE */
      character*20 nametrc, unitt
      character*60 vname1, vname3      
      integer omp_get_num_threads
#include "param.h"
#include "scalars.h"
#include "mixing.h"
#include "ncscrum.h"
#include "diagnostics.h"
#include "boundary.h"
#if defined ONLINE
#  include "online.h"
#endif /* ONLINE */
#if defined TCLIMATOLOGY
#  include "climat.h"
#endif /* TCLIMATOLOGY */

!      call system('uname -prs')
      
!
! Find out how many threads are created and check whether the number
! of threads exceeds maximum allowed, as well as check if the number
! of tiles is divisible by the number of threads, so the job can be
! evenly partitioned, complain and signal to terminate, if something
! is wrong. 
! 
C$OMP PARALLEL
C$OMP CRITICAL (isca_cr_rgn)
      numthreads=omp_get_num_threads()
C$OMP END CRITICAL (isca_cr_rgn)
C$OMP END PARALLEL
      MPI_master_only write(stdout,'(1x,A,3(1x,A,I3),A)') 'NUMBER',
     &    'OF THREADS:',numthreads,'BLOCKING:',NSUB_X,'x',NSUB_E,'.'

      if (numthreads.gt.NPP) then
        MPI_master_only write(stdout,'(/1x,A,I3/)')
     &    'ERROR: Requested number of threads exceeds NPP =', NPP
        ierr=ierr+1                                      !--> ERROR
      elseif (mod(NSUB_X*NSUB_E,numthreads).ne.0) then
        MPI_master_only write(stdout,
     &                '(/1x,A,1x,A,I3,4x,A,I3,4x,A,I4,A)') 'ERROR:',
     &                'wrong choice of numthreads =', numthreads,
     &                'NSUB_X =', NSUB_X, 'NSUB_E =', NSUB_E, '.'
        ierr=ierr+1                                      !--> ERROR
      endif
!
! Set evolving time indices to their default initial values.
!
      time=0.
      tdays=0.
      PREDICTOR_2D_STEP=.FALSE.
      iic=0
      kstp=1
      krhs=1
      knew=1

      ntstart=1
#ifdef SOLVE3D
      nstp=1
      nrhs=1
      nnew=1
#endif
      nfast=1

#ifdef FLOATS
      nfp1=0     ! set time step indices for
      nf=3       ! 5th order AB4AM4 corrected
      nfm1=2     ! scheme
      nfm2=1
      nfm3=0
#endif
!
! Reset intra-thread communication flags and counters.
!
      synchro_flag=.true.
      first_time=0
      may_day_flag=0
!
! Reset process ID and CPU_time clocks (needed by timers).
!
      do j=0,NPP
        do i=0,31
          proc(i,j)=0
          CPU_time(i,j)=0.E0
        enddo
      enddo
      trd_count=0
!
! Reset record counters for averages/history output.
!
      nrecrst=0
      nrechis=0
#ifdef AVERAGES
      nrecavg=0
#endif
#if defined DIAGNOSTICS_TS
      nrecdia=0
# ifdef AVERAGES
      nrecdia_avg=0
# endif
#endif 
#if defined DIAGNOSTICS_UV
      nrecdiaM=0
# ifdef AVERAGES
      nrecdiaM_avg=0
# endif
#endif
!
! Reset global sums and counters needed by volume/energy
! diagnostics and volume conservation constraint.
!
      tile_count=0
      bc_count=0
      avgke=0.
      avgpe=0.
      avgkp=0.
      volume=0.

      hmin=+1.E+20      ! Set these
      hmax=-1.E+20      ! to large,
      grdmin=+1.E+20    ! positive or 
      grdmax=-1.E+20    ! negative
      Cu_min=+1.E+20    ! unrealistic
      Cu_max=-1.E+20    ! values
#ifdef SPHERICAL
      lonmin=+1.E+20
      lonmax=-1.E+20
      latmin=+1.E+20
      latmax=-1.E+20
#endif

      bc_crss=QuadZero
#ifdef OBC_VOLCONS
      bc_flux=QuadZero
      ubar_xs=QuadZero
#endif
#ifdef SOLVE3D
      rx0=-1.E+20
      rx1=-1.E+20
#endif
#ifdef BIOLOGY
      bio_count=0
      do i=0,2*NT+1
        global_sum(i)=QuadZero
      enddo
#endif
!
! Initialize netCDF files IDs to closed status.
!
      ncidrst=-1
      ncidhis=-1
#ifdef AVERAGES
      ncidavg=-1
#endif
      ncidfrc=-1
      ncidbulk=-1
      ncidclm=-1
#ifdef DIAGNOSTICS_TS
      nciddia=-1
# ifdef AVERAGES
      nciddia_avg=-1
# endif 
#endif 
#ifdef DIAGNOSTICS_UV
      nciddiaM=-1
# ifdef AVERAGES
      nciddiaM_avg=-1
# endif
#endif
#ifdef DIAGNOSTICS_BIO
      nciddiabio=-1
# ifdef AVERAGES
      nciddiabio_avg=-1
# endif
#endif
#ifndef ANA_BRY
       bry_id=-1
#endif
      call get_date (date_str)
!
!---------------------------------------------------------------
! Define names of variables in NetCDF output files.
! Inner dimension is for variable type and outer is for variable
! attributes (name, long-name, units, field).
!---------------------------------------------------------------
!
!--#define CR  
CR      write(stdout,'(1x,A,I3)') 'indxTime =',indxTime
      vname(1,indxTime)='scrum_time                               '
      vname(2,indxTime)='time since initialization                '
      vname(3,indxTime)='second                                   '
      vname(4,indxTime)='time, scalar, series                     '

CR      write(stdout,'(1x,A,I3)') 'indxZ =',indxZ
      vname(1,indxZ)='zeta                                        '
      vname(2,indxZ)='free-surface                                '
      vname(3,indxZ)='meter                                       '
      vname(4,indxZ)='free-surface, scalar, series                '

CR      write(stdout,'(1x,A,I3)') 'indxUb =',indxUb
      vname(1,indxUb)='ubar                                       '
      vname(2,indxUb)='vertically integrated u-momentum component '
      vname(3,indxUb)='meter second-1                             '
      vname(4,indxUb)='ubar-velocity, scalar, series              '

CR      write(stdout,'(1x,A,I3)') 'indxVb = ',indxVb
      vname(1,indxVb)='vbar                                       '
      vname(2,indxVb)='vertically integrated v-momentum component '
      vname(3,indxVb)='meter second-1                             '
      vname(4,indxVb)='vbar-velocity, scalar, series              '

CR      write(stdout,'(1x,A,I3)') 'indxBostr =',indxBostr
      vname(1,indxBostr)='bostr                                   '
      vname(2,indxBostr)='Kinematic bottom stress                 '
      vname(3,indxBostr)='N/m2                                    '

CR      write(stdout,'(1x,A,I3)') 'indxWstr =',indxWstr
      vname(1,indxWstr)='wstr                                   '
      vname(2,indxWstr)='Kinematic wind stress                  '
      vname(3,indxWstr)='N/m2                                   '
                               
CR      write(stdout,'(1x,A,I3)') 'indxUWstr =',indxUWstr
      vname(1,indxUWstr)='sustr                                  '
      vname(2,indxUWstr)='Kinematic u wind stress component      '
      vname(3,indxUWstr)='N/m2                                   '

CR      write(stdout,'(1x,A,I3)') 'indxVWstr =',indxVWstr
      vname(1,indxVWstr)='svstr                                  '
      vname(2,indxVWstr)='Kinematic v wind stress component      '
      vname(3,indxVWstr)='N/m2                                   '

#ifdef SOLVE3D
CR      write(stdout,'(1x,A,I3)') 'indxU = ',indxU
      vname(1,indxU)='u                                           '
      vname(2,indxU)='u-momentum component                        '
      vname(3,indxU)='meter second-1                              '
      vname(4,indxU)='u-velocity, scalar, series                  '

CR      write(stdout,'(1x,A,I3)') 'indxV = ',indxV
      vname(1,indxV)='v                                           '
      vname(2,indxV)='v-momentum component                        '
      vname(3,indxV)='meter second-1                              '
      vname(4,indxV)='v-velocity, scalar, series                  '

CR      write(stdout,'(1x,A,I3)') 'indxT = ',indxT
      vname(1,indxT)='temp                                        '
      vname(2,indxT)='potential temperature                       '
      vname(3,indxT)='Celsius                                     '
      vname(4,indxT)='temperature, scalar, series                 '

# ifdef SALINITY
CR      write(stdout,'(1x,A,I3)') 'indxS = ',indxS
      vname(1,indxS)='salt                                        '
      vname(2,indxS)='salinity                                    '
      vname(3,indxS)='PSU                                         '
      vname(4,indxS)='salinity, scalar, series                    '
# endif

# ifdef PASSIVE_TRACER
CR      write(stdout,'(1x,A,I3)') 'indxTPAS = ',indxTPAS
      vname(1,indxTPAS)='tpas                                     '
      vname(2,indxTPAS)='passive tracer                           '
      vname(3,indxTPAS)='no unit                                  '
      vname(4,indxTPAS)='passive tracer, scalar, series           '
# endif

# ifdef BIOLOGY
#  ifdef PISCES
      vname(1,indxDIC)='DIC                                       '
      vname(2,indxDIC)='Dissolved inorganic carbon                '
      vname(3,indxDIC)='umol C L-1                                '
      vname(4,indxDIC)='DIC, scalar, series                       '

      vname(1,indxTAL)='TALK                                      '
      vname(2,indxTAL)='Total Alkalinity                          '
      vname(3,indxTAL)='umol C L-1                                '
      vname(4,indxTAL)='Alkalinity, scalar, series                '

      vname(1,indxOXY)='O2                                        '
      vname(2,indxOXY)='Oxygen                                    '
      vname(3,indxOXY)='umol L-1                                  '
      vname(4,indxOXY)='Oxygen, scalar, series                    '

      vname(1,indxCAL)='CACO3                                     '
      vname(2,indxCAL)='Calcite                                   '
      vname(3,indxCAL)='umol C L-1                                '
      vname(4,indxCAL)='Calcite, scalar, series                   '

      vname(1,indxPO4)='PO4                                       '
      vname(2,indxPO4)='Phosphate                                 '
      vname(3,indxPO4)='umol P L-1                                '
      vname(4,indxPO4)='Phosphate, scalar, series                 '

      vname(1,indxPOC)='POC                                       '
      vname(2,indxPOC)='Small particulate organic carbon          '
      vname(3,indxPOC)='umol C L-1                                '
      vname(4,indxPOC)='Small particles, scalar, series           '

      vname(1,indxSIL)='Si                                        '
      vname(2,indxSIL)='Silicate                                  '
      vname(3,indxSIL)='umol Si L-1                               '
      vname(4,indxSIL)='Silicate, scalar, series                  '

      vname(1,indxPHY)='NANO                                      '
      vname(2,indxPHY)='Nanophytoplankton                         '
      vname(3,indxPHY)='umol C L-1                                '
      vname(4,indxPHY)='Nanophytoplankton, scalar, series         '

      vname(1,indxZOO)='ZOO                                       '
      vname(2,indxZOO)='Microzooplankton                          '
      vname(3,indxZOO)='umol C L-1                                '
      vname(4,indxZOO)='Microzooplankton, scalar, series          '

      vname(1,indxDOC)='DOC                                       '
      vname(2,indxDOC)='Dissolved organic carbon                  '
      vname(3,indxDOC)='umol C L-1                                '
      vname(4,indxDOC)='DOC, scalar, series                       '

      vname(1,indxDIA)='DIA                                       '
      vname(2,indxDIA)='Diatoms                                   '
      vname(3,indxDIA)='umol C L-1                                '
      vname(4,indxDIA)='Diatoms, scalar, series                   '

      vname(1,indxMES)='MESO                                      '
      vname(2,indxMES)='Mesozooplankton                           '
      vname(3,indxMES)='umol C L-1                                '
      vname(4,indxMES)='Mesozooplankton, scalar, series           '

      vname(1,indxBSI)='BSI                                       '
      vname(2,indxBSI)='Diatoms silicon                           '
      vname(3,indxBSI)='umol Si L-1                               '
      vname(4,indxBSI)='BSi, scalar, series                       '

      vname(1,indxFER)='FER                                       '
      vname(2,indxFER)='Dissolved iron                            '
      vname(3,indxFER)='umol Fe L-1                               '
      vname(4,indxFER)='Iron, scalar, series                      '

      vname(1,indxBFE)='BFE                                       '
      vname(2,indxBFE)='Iron in big particles                     '
      vname(3,indxBFE)='umol Fe L-1                               '
      vname(4,indxBFE)='BFE, scalar, series                       '

      vname(1,indxGOC)='GOC                                       '
      vname(2,indxGOC)='Big particulate organic carbon            '
      vname(3,indxGOC)='umol C L-1                                '
      vname(4,indxGOC)='GOC, scalar, series                       '

      vname(1,indxSFE)='SFE                                       '
      vname(2,indxSFE)='Iron in nanophytoplankton                 '
      vname(3,indxSFE)='umol Fe L-1                               '
      vname(4,indxSFE)='SFE, scalar, series                       '

      vname(1,indxDFE)='DFE                                       '
      vname(2,indxDFE)='Iron in diatoms                           '
      vname(3,indxDFE)='umol Fe L-1                               '
      vname(4,indxDFE)='DFE, scalar, series                       '

      vname(1,indxDSI)='DSI                                       '
      vname(2,indxDSI)='Biogenic silica                           '
      vname(3,indxDSI)='umol Si L-1                               '
      vname(4,indxDSI)='DSI, scalar, series                       '

      vname(1,indxNFE)='NFE                                       '
      vname(2,indxNFE)='Iron in small particles                   '
      vname(3,indxNFE)='umol Fe L-1                               '
      vname(4,indxNFE)='NFE, scalar, series                       '

      vname(1,indxNCH)='NCHL                                      '
      vname(2,indxNCH)='Chlorophyll in nano                       '
      vname(3,indxNCH)='mg Chl m-3                                '
      vname(4,indxNCH)='NCHL, scalar, series                      '

      vname(1,indxDCH)='DCHL                                      '
      vname(2,indxDCH)='Chlorophyll in diatoms                    '
      vname(3,indxDCH)='mg Chl m-3                                '
      vname(4,indxDCH)='DCHL, scalar, series                      '

      vname(1,indxNO3)='NO3                                       '
      vname(2,indxNO3)='Nitrate                                   '
      vname(3,indxNO3)='umol N L-1                                '
      vname(4,indxNO3)='Nitrate, scalar, series                   '

      vname(1,indxNH4)='NH4                                       '
      vname(2,indxNH4)='Ammonium                                  '
      vname(3,indxNH4)='umol N L-1                                '
      vname(4,indxNH4)='Ammonium, scalar, series                  '

#  elif defined BIO_NChlPZD
CR      write(stdout,'(1x,A,I3)') 'indxNO3 =',indxNO3
      vname(1,indxNO3)='NO3                                       '
      vname(2,indxNO3)='NO3 Nutrient                              '
      vname(3,indxNO3)='mMol N m-3                                '
      vname(4,indxNO3)='NO3, scalar, series                       '

CR      write(stdout,'(1x,A,I3)') 'indxChla =',indxChla
      vname(1,indxChla)='CHLA                                     '
      vname(2,indxChla)='Chlorophyll A                            '
      vname(3,indxChla)='mg Chla m-3                              '
      vname(4,indxChla)='Chlorophyl, scalar, series               '

CR      write(stdout,'(1x,A,I3)') 'indxPhy1 =',indxPhy1
      vname(1,indxPhy1)='PHYTO                                    '
      vname(2,indxPhy1)='Phytoplankton                            '
      vname(3,indxPhy1)='mMol N m-3                               '
      vname(4,indxPhy1)='Phytoplankton, scalar, series            '

CR      write(stdout,'(1x,A,I3)') 'indxZoo1 =',indxZoo1
      vname(1,indxZoo1)='ZOO                                      '
      vname(2,indxZoo1)='Zooplankton                              '
      vname(3,indxZoo1)='mMol N m-3                               '
      vname(4,indxZoo1)='Zooplankton, scalar, series              '

CR      write(stdout,'(1x,A,I3)') 'indxDet1 =',indxDet1
      vname(1,indxDet1)='DET                                      '
      vname(2,indxDet1)='Detritus Nutrient                        '
      vname(3,indxDet1)='mMol N m-3                               '
      vname(4,indxDet1)='Detritus, scalar, series                 '

#   ifdef OXYGEN
CR      write(stdout,'(1x,A,I3)') 'indxO2 =',indxO2
      vname(1,indxO2)='O2                                         '
      vname(2,indxO2)='Dissolved Oxygen	                          '
      vname(3,indxO2)='mMol O2 m-3                                '
      vname(4,indxO2)='Dissolved Oxygen	, scalar, series          '
#   endif

CR      write(stdout,'(1x,A,I3)') 'ntrc_salt=',ntrc_salt
CR      write(stdout,'(1x,A,I3)') 'ntrc_pas =',ntrc_pas
CR      write(stdout,'(1x,A,I3)') 'ntrc_bio =',ntrc_bio
CR      write(stdout,'(1x,A,I3)') 'ntrc_sed =',ntrc_sed
CR      write(stdout,'(1x,A,I3)') 'ntrc_diabio=',ntrc_diabio

#  elif defined BIO_N2ChlPZD2

CR      write(stdout,'(1x,A,I3)') 'indxNO3 =',indxNO3
      vname(1,indxNO3)='NO3                                       '
      vname(2,indxNO3)='NO3 Nutrient                              '
      vname(3,indxNO3)='mMol N m-3                                '
      vname(4,indxNO3)='NO3, scalar, series                       '

CR      write(stdout,'(1x,A,I3)') 'indxNH4 =',indxNH4
      vname(1,indxNH4)='NH4                                       '
      vname(2,indxNH4)='NH4 Nutrient                              '
      vname(3,indxNH4)='mMol N m-3                                '
      vname(4,indxNH4)='NH4, scalar, series                       '

CR      write(stdout,'(1x,A,I3)') 'indxChla =',indxChla
      vname(1,indxChla)='CHLA                                     '
      vname(2,indxChla)='Chlorophyll A                            '
      vname(3,indxChla)='mg Chla m-3                              '
      vname(4,indxChla)='Chlorophyl, scalar, series               '

CR      write(stdout,'(1x,A,I3)') 'indxPhy1 =',indxPhy1
      vname(1,indxPhy1)='PHYTO                                    '
      vname(2,indxPhy1)='Phytoplankton                            '
      vname(3,indxPhy1)='mMol N m-3                               '
      vname(4,indxPhy1)='Phytoplankton, scalar, series            '

CR      write(stdout,'(1x,A,I3)') 'indxZoo1 =',indxZoo1
      vname(1,indxZoo1)='ZOO                                      '
      vname(2,indxZoo1)='Zooplankton                              '
      vname(3,indxZoo1)='mMol N m-3                               '
      vname(4,indxZoo1)='Zooplankton, scalar, series              '

CR      write(stdout,'(1x,A,I3)') 'indxDet1 =',indxDet1
      vname(1,indxDet1)='SDET                                     '
      vname(2,indxDet1)='Small Detritus Nutrient                  '
      vname(3,indxDet1)='mMol N m-3                               '
      vname(4,indxDet1)='Small Detritus, scalar, series           '

CR      write(stdout,'(1x,A,I3)') 'indxDet2 =',indxDet2
      vname(1,indxDet2)='LDET                                     '
      vname(2,indxDet2)='Large Detritus Nutrient                  '
      vname(3,indxDet2)='mMol N m-3                               '
      vname(4,indxDet2)='Large Detritus, scalar, series           '

#  elif defined BIO_N2P2Z2D2

CR      write(stdout,'(1x,A,I3)') 'indxNO3 =',indxNO3
      vname(1,indxNO3)='NO3                                       '
      vname(2,indxNO3)='NO3 Nutrient                              '
      vname(3,indxNO3)='mMol N m-3                                '
      vname(4,indxNO3)='NO3, scalar, series                       '

CR      write(stdout,'(1x,A,I3)') 'indxNH4 =',indxNH4
      vname(1,indxNH4)='NH4                                       '
      vname(2,indxNH4)='NH4 Nutrient                              '
      vname(3,indxNH4)='mMol N m-3                                '
      vname(4,indxNH4)='NH4, scalar, series                       '

#   ifdef VAR_CHL_C
CR      write(stdout,'(1x,A,I3)') 'indxChl1 =',indxChl1
      vname(1,indxChl1)='CHLA-1                                   '
      vname(2,indxChl1)='Chlorophyll A                            '
      vname(3,indxChl1)='mg Chla m-3                              '
      vname(4,indxChl1)='Chlorophyl, scalar, series               '

CR      write(stdout,'(1x,A,I3)') 'indxChl2 =',indxChl2
      vname(1,indxChl2)='CHLA-2                                   '
      vname(2,indxChl2)='Chlorophyll A                            '
      vname(3,indxChl2)='mg Chla m-3                              '
      vname(4,indxChl2)='Chlorophyl, scalar, series               '
#   endif

CR      write(stdout,'(1x,A,I3)') 'indxPhy1 =',indxPhy1
      vname(1,indxPhy1)='SPHYTO                                   '
      vname(2,indxPhy1)='Small Phytoplankton                      '
      vname(3,indxPhy1)='mMol N m-3                               '
      vname(4,indxPhy1)='Small Phytoplankton, scalar, series      '

CR      write(stdout,'(1x,A,I3)') 'indxPhy2 =',indxPhy2
      vname(1,indxPhy2)='LPHYTO                                   '
      vname(2,indxPhy2)='Large Phytoplankton                      '
      vname(3,indxPhy2)='mMol N m-3                               '
      vname(4,indxPhy2)='Large Phytoplankton, scalar, series      '

CR      write(stdout,'(1x,A,I3)') 'indxZoo1 =',indxZoo1
      vname(1,indxZoo1)='SZOO                                     '
      vname(2,indxZoo1)='Small Zooplankton                        '
      vname(3,indxZoo1)='mMol N m-3                               '
      vname(4,indxZoo1)='Small Zooplankton, scalar, series        '

CR      write(stdout,'(1x,A,I3)') 'indxZoo2 =',indxZoo2
      vname(1,indxZoo2)='LZOO                                     '
      vname(2,indxZoo2)='Large Zooplankton                        '
      vname(3,indxZoo2)='mMol N m-3                               '
      vname(4,indxZoo2)='Large Zooplankton, scalar, series        '

CR      write(stdout,'(1x,A,I3)') 'indxDet1 =',indxDet1
      vname(1,indxDet1)='SDET                                     '
      vname(2,indxDet1)='Small Detritus Nutrient                  '
      vname(3,indxDet1)='mMol N m-3                               '
      vname(4,indxDet1)='Small Detritus, scalar, series           '

CR      write(stdout,'(1x,A,I3)') 'indxDet2 =',indxDet2
      vname(1,indxDet2)='LDET                                     '
      vname(2,indxDet2)='Large Detritus Nutrient                  '
      vname(3,indxDet2)='mMol N m-3                               '
      vname(4,indxDet2)='Large Detritus, scalar, series           '
#  endif
# endif
# ifdef SEDIMENT
CR      write(stdout,'(1x,A,I3)') 'indxsand =',indxsand
      vname(1,indxsand)='sand                                     '
      vname(2,indxsand)='sand sediment                            '
      vname(3,indxsand)='mg/l                                     '
      vname(4,indxsand)='sand, scalar, series                     '

CR      write(stdout,'(1x,A,I3)') 'indxsilt =',indxsilt
      vname(1,indxsilt)='silt                                     '
      vname(2,indxsilt)='silt sediment                            '
      vname(3,indxsilt)='mg/l                                     '
      vname(4,indxsilt)='silt, scalar, series                     '

CR      write(stdout,'(1x,A,I3)') 'indxBTHK =',indxBTHK
      vname(1,indxBTHK)  ='bed_thick                              '
      vname(2,indxBTHK)  ='Thickness of sediment bed layer        '
      vname(3,indxBTHK)  ='m                                      '
CR      write(stdout,'(1x,A,I3)') 'indxBPOR =',indxBPOR
      vname(1,indxBPOR)  ='bed_poros                              '
      vname(2,indxBPOR)  ='Porosity of sediment bed layer         '
      vname(3,indxBPOR)  ='no units                               '
CR      write(stdout,'(1x,A,I3)') 'indxBFRA1 =',indxBFRA
      vname(1,indxBFRA)  ='bed_frac_sand                          '
      vname(2,indxBFRA)  ='volume fraction of sand in bed layer   '
      vname(3,indxBFRA)  ='no units                               '
CR      write(stdout,'(1x,A,I3)') 'indxBFRA2 =',indxBFRA+1
      vname(1,indxBFRA+1)='bed_frac_silt                          '
      vname(2,indxBFRA+1)='Volume fraction of silt in bed layer   '
      vname(3,indxBFRA+1)='no units                               '
# endif
# ifdef BBL
CR      write(stdout,'(1x,A,I3)') 'indxAbed=',indxBBL
      vname(1,indxBBL)  ='Abed                                    '
      vname(2,indxBBL)  ='Bed wave excursion amplitude            '
      vname(3,indxBBL)  ='m                                       '
CR      write(stdout,'(1x,A,I3)') 'indxHrip =',indxBBL+1
      vname(1,indxHrip)='Hripple                                  '
      vname(2,indxHrip)='Bed ripple height                        '
      vname(3,indxHrip)='m                                        '
CR      write(stdout,'(1x,A,I3)') 'indxLrip =',indxBBL+2
      vname(1,indxLrip)='Lripple                                  '
      vname(2,indxLrip)='Bed ripple length                        '
      vname(3,indxLrip)='m                                        '
CR      write(stdout,'(1x,A,I3)') 'indxZbnot=',indxBBL+3
      vname(1,indxZbnot)='Zbnot                                   '
      vname(2,indxZbnot)='Physical hydraulic bottom roughness     '
      vname(3,indxZbnot)='m                                       '
CR      write(stdout,'(1x,A,I3)') 'indxZbapp=',indxBBL+4
      vname(1,indxZbapp)='Zbapp                                   '
      vname(2,indxZbapp)='Apparent hydraulic bottom roughness     '
      vname(3,indxZbapp)='m                                       '
CR      write(stdout,'(1x,A,I3)') 'indxBostrw=',indxBBL+5
      vname(1,indxBostrw)='bostrw                                 '
      vname(2,indxBostrw)='Wave-induced kinematic bottom stress   '
      vname(3,indxBostrw)='N/m2                                   '
# endif
# ifdef BULK_FLUX
CR      write(stdout,'(1x,A,I3)') 'indxShflx_rsw =',indxShflx_rsw
      vname(1,indxShflx_rsw)='shflx_rsw                           '
      vname(2,indxShflx_rsw)='Short-wave surface radiation        '
      vname(3,indxShflx_rsw)='Celsius meter second-1              '
CR      write(stdout,'(1x,A,I3)') 'indxShflx_rlw =',indxShflx_rlw
      vname(1,indxShflx_rlw)='shflx_rlw                           '
      vname(2,indxShflx_rlw)='Long-wave surface radiation         '
      vname(3,indxShflx_rlw)='Celsius meter second-1              '
CR      write(stdout,'(1x,A,I3)') 'indxShflx_lat =',indxShflx_lat
      vname(1,indxShflx_lat)='shflx_lat                           '
      vname(2,indxShflx_lat)='Latent surface heat flux            '
      vname(3,indxShflx_lat)='Celsius meter second-1              '
CR      write(stdout,'(1x,A,I3)') 'indxShflx_sen =',indxShflx_sen
      vname(1,indxShflx_sen)='shflx_sen                           '
      vname(2,indxShflx_sen)='Sensible surface heat flux          '
      vname(3,indxShflx_sen)='Celsius meter second-1              '
# endif

CR      write(stdout,'(1x,A,I3)') 'indxO =',indxO
      vname(1,indxO)='omega                                       '
      vname(2,indxO)='S-coordinate vertical momentum component    '
      vname(3,indxO)='meter second-1                              '
      vname(4,indxO)='omega, scalar, series                       '

CR      write(stdout,'(1x,A,I3)') 'indxW =',indxW
      vname(1,indxW)='w                                           '
      vname(2,indxW)='vertical momentum component                 '
      vname(3,indxW)='meter second-1                              '
      vname(4,indxW)='w-velocity, scalar, series                  '

CR      write(stdout,'(1x,A,I3)') 'indxR =',indxR
      vname(1,indxR)='rho                                         '
      vname(2,indxR)='density anomaly                             '
      vname(3,indxR)='kilogram meter-3                            '
      vname(4,indxR)='density, scalar, series                     '

# ifdef VIS_COEF_3D
CR      write(stdout,'(1x,A,I3)') 'indxVisc =',indxVisc
      vname(1,indxVisc)='visc3d                                   '
      vname(2,indxVisc)='horizontal viscosity coefficient         '
#  ifdef UV_VIS2
      vname(3,indxVisc)='meter2 second-1                          '
#  else
      vname(3,indxVisc)='meter4 second-1                          '
#  endif
      vname(4,indxVisc)='visc3d, scalar, series                   '
# endif
# ifdef DIF_COEF_3D
CR      write(stdout,'(1x,A,I3)') 'indxDiff =',indxDiff
      vname(1,indxDiff)='diff3d                                   '
      vname(2,indxDiff)='horizontal diffusivity coefficient       '
#  ifdef TS_DIF2
      vname(3,indxDiff)='meter2 second-1                          '
#  else
      vname(3,indxDiff)='meter4 second-1                          '
#  endif
      vname(4,indxDiff)='diff3d, scalar, series                   '
# endif

CR      write(stdout,'(1x,A,I3)') 'indxAkv =',indxAkv
      vname(1,indxAkv)='AKv                                       '
      vname(2,indxAkv)='vertical viscosity coefficient            '
      vname(3,indxAkv)='meter2 second-1                           '
      vname(4,indxAkv)='AKv, scalar, series                       '

CR      write(stdout,'(1x,A,I3)') 'indxAkt =',indxAkt
      vname(1,indxAkt)='AKt                                       '
      vname(2,indxAkt)='temperature vertical diffusion coefficient'
      vname(3,indxAkt)='meter2 second-1                           '
      vname(4,indxAkt)='AKt, scalar, series                       '
# ifdef SALINITY
CR      write(stdout,'(1x,A,I3)') 'indxAks =',indxAks
      vname(1,indxAks)='AKs                                       '
      vname(2,indxAks)='salinity vertical diffusion coefficient   '
      vname(3,indxAks)='meter2 second-1                           '
      vname(4,indxAks)='AKs, scalar, series                       '
# endif
# ifdef LMD_SKPP
CR      write(stdout,'(1x,A,I3)') 'indxHbl =',indxHbl
      vname(1,indxHbl)='hbl                                       '
      vname(2,indxHbl)='depth of planetary boundary layer         '
      vname(3,indxHbl)='meter                                     '
      vname(4,indxHbl)='hbl, scalar, series                       '
# endif
# ifdef LMD_BKPP
CR      write(stdout,'(1x,A,I3)') 'indxHbbl =',indxHbbl
      vname(1,indxHbbl)='hbbl                                     '
      vname(2,indxHbbl)='depth of bottom boundary layer           '
      vname(3,indxHbbl)='meter                                    '
      vname(4,indxHbbl)='hbbl, scalar, series                     '
# endif
# ifdef BIOLOGY
CR      write(stdout,'(1x,A,I3)') 'indxHel =',indxHel
      vname(1,indxHel)='hel                                       '
      vname(2,indxHel)='depth of euphotic layer                   '
      vname(3,indxHel)='meter                                     '
      vname(4,indxHel)='hel, scalar, series                       '
#  ifdef BIO_NChlPZD
CR      write(stdout,'(1x,A,I3)') 'indxChC =',indxChC
      vname(1,indxChC)='theta                                     '
      vname(2,indxChC)='Chlorophyll to Carbon ratio               '
      vname(3,indxChC)='mgChl/mgC                                 '
      vname(4,indxChC)='theta, scalar, series                     '
#   ifdef OXYGEN
CR      write(stdout,'(1x,A,I3)') 'indxU10 =',indxU10
      vname(1,indxU10)='U10                                       '
      vname(2,indxU10)='surface wind speed 10 m                   '
      vname(3,indxU10)='meter second-1                            '
      vname(4,indxU10)='surface wind speed, scalar, series        '

CR      write(stdout,'(1x,A,I3)') 'indxKvO2 =',indxKvO2
      vname(1,indxKvO2)='KvO2                                     '
      vname(2,indxKvO2)='gas transfer coef for O2                 '
      vname(3,indxKvO2)='meter second-1                           '
      vname(4,indxKvO2)='gas transfer coef for O2, scalar, series '

CR      write(stdout,'(1x,A,I3)') 'indxO2sat =',indxO2sat
      vname(1,indxO2sat)='O2sat                                   '
      vname(2,indxO2sat)='saturation oxygen                       '
      vname(3,indxO2sat)='mMol O2 m-3                             '
      vname(4,indxO2sat)='saturation oxygen, scalar, series       '
#   endif
#  elif defined BIO_N2P2Z2D2 && defined VAR_CHL_C
CR      write(stdout,'(1x,A,I3)') 'indxChC1 =',indxChC1
      vname(1,indxChC1)='theta1                                    '
      vname(2,indxChC1)='Chlorophyll to Carbon ratio for Phy1      '
      vname(3,indxChC1)='mgChl/mgC                                 '
      vname(4,indxChC1)='theta, scalar, series                     '

CR      write(stdout,'(1x,A,I3)') 'indxChC2 =',indxChC2
      vname(1,indxChC2)='theta2                                    '
      vname(2,indxChC2)='Chlorophyll to Carbon ratio for Phy2      '
      vname(3,indxChC2)='mgChl/mgC                                 '
      vname(4,indxChC2)='theta, scalar, series                     '
#  endif
# endif
#endif
CR      write(stdout,'(1x,A,I3)') 'indxSSH =',indxSSH
      vname(1,indxSSH)='SSH                                       '
      vname(2,indxSSH)='sea surface height                        '
      vname(3,indxSSH)='meter                                     '
      vname(4,indxSSH)='SSH, scalar, series                       '

CR      write(stdout,'(1x,A,I3)') 'indxSUSTR =',indxSUSTR
      vname(1,indxSUSTR)='sustr                                   '
      vname(2,indxSUSTR)='surface u-momentum stress               '
      vname(3,indxSUSTR)='Newton meter-2                          '
      vname(4,indxSUSTR)='surface u-mom stress, scalar, series    '

CR      write(stdout,'(1x,A,I3)') 'indxSVSTR =',indxSVSTR
      vname(1,indxSVSTR)='svstr                                   '
      vname(2,indxSVSTR)='surface v-momentum stress               '
      vname(3,indxSVSTR)='Newton meter-2                          '
      vname(4,indxSVSTR)='surface v-mom stress, scalar, series    '

#ifdef SOLVE3D
CR      write(stdout,'(1x,A,I3)') 'indxSHFl =',indxSHFl
      vname(1,indxSHFl)='shflux                                   '
      vname(2,indxSHFl)='surface net heat flux                    '
      vname(3,indxSHFl)='Watts meter-2                            '
      vname(4,indxSHFl)='surface heat flux, scalar, series        '
# ifdef SALINITY
CR      write(stdout,'(1x,A,I3)') 'indxSSFl=',indxSSFl
      vname(1,indxSSFl)='swflux                                   '
      vname(2,indxSSFl)='surface freshwater flux (E-P)            '
      vname(3,indxSSFl)='centimeter day-1                         '
      vname(4,indxSSFl)='surface freshwater flux, scalar, series  '
# endif
CR      write(stdout,'(1x,A,I3)') 'indxSWRad =',indxSWRad
      vname(1,indxSWRad)='swrad                                   '
      vname(2,indxSWRad)='solar shortwave radiation               '
      vname(3,indxSWRad)='Watts meter-2                           '
      vname(4,indxSWRad)='shortwave radiation, scalar, series     '

CR      write(stdout,'(1x,A,I3)') 'indxSST =',indxSST
      vname(1,indxSST)='SST                                       '
      vname(2,indxSST)='sea surface temperature                   '
      vname(3,indxSST)='Celsius                                   '
      vname(4,indxSST)='SST, scalar, series                       '

CR      write(stdout,'(1x,A,I3)') 'indxdQdSST =',indxdQdSST
      vname(1,indxdQdSST)='dQdSST                                 '
      vname(2,indxdQdSST)='surface heat flux sensitivity to SST   '
      vname(3,indxdQdSST)='Watts meter-2 Celsius-1                '
      vname(4,indxdQdSST)='dQdSST, scalar, series                 '
# if defined SALINITY && defined SFLX_CORR
CR      write(stdout,'(1x,A,I3)') 'indxSSS =',indxSSS
      vname(1,indxSSS)='SSS                                       '
      vname(2,indxSSS)='sea surface salinity                      '
      vname(3,indxSSS)='PSU                                       '
      vname(4,indxSSS)='SSS, scalar, series                       '
# endif
# ifdef BBL
#  ifndef ANA_WWAVE
CR      write(stdout,'(1x,A,I3)') 'indxWWA',indxWWA
      vname(1,indxWWA)='Awave                                     '
      vname(2,indxWWA)='wind induced wave amplitude               '
      vname(3,indxWWA)='meter                                     '
      vname(4,indxWWA)='Awave, scalar, series                     '

CR      write(stdout,'(1x,A,I3)') 'indxWWD =',indxWWD
      vname(1,indxWWD)='Dwave                                     '
      vname(2,indxWWD)='wind induced wave direction               '
      vname(3,indxWWD)='degrees                                   '
      vname(4,indxWWD)='Dwave, scalar, series                     '

CR      write(stdout,'(1x,A,I3)') 'indxWWP =',indxWWP
      vname(1,indxWWP)='Pwave                                     '
      vname(2,indxWWP)='wind induced wave Period                  '
      vname(3,indxWWP)='second                                    '
      vname(4,indxWWP)='Pwave, scalar, series                     '
#  endif
#  if (!defined ANA_BSEDIM && !defined SEDIMENT)
CR      write(stdout,'(1x,A,I3)') 'indxBSS =',indxBSS
      vname(1,indxBSS)='Ssize                                     '
      vname(2,indxBSS)='bottom sediment grain diameter size       '
      vname(3,indxBSS)='meter                                     '
      vname(4,indxBSS)='Ssize, scalar, series                     '

CR      write(stdout,'(1x,A,I3)') 'indxBSD =',indxBSD
      vname(1,indxBSD)='Sdens                                     '
      vname(2,indxBSD)='bottom sediment grain density             '
      vname(3,indxBSD)='kilogram meter-3                          '
      vname(4,indxBSD)='Sdens, scalar, series                     '
#  endif
# endif
#endif
#ifdef ICE
      vname(1,indxAi)='aice                                       '
      vname(2,indxAi)='fraction of cell covered by ice            '
      vname(3,indxAi)='nondimensional                             '
      vname(4,indxAi)='aice, scalar, series                       '

      vname(1,indxUi)='uice                                       '
      vname(2,indxUi)='u-component of ice velocity                '
      vname(3,indxUi)='meter sec-1                                '
      vname(4,indxUi)='uice, scalar, series                       '

      vname(1,indxVi)='vice                                       '
      vname(2,indxVi)='v-component of ice velocity                '
      vname(3,indxVi)='meter sec-1                                '
      vname(4,indxVi)='vice, scalar, series                       '

      vname(1,indxHi)='hice                                       '
      vname(2,indxHi)='depth of ice cover                         '
      vname(3,indxHi)='meter                                      '
      vname(4,indxHi)='hice, scalar, series                       '

      vname(1,indxHS)='snow_thick                                 '
      vname(2,indxHS)='depth of snow cover                        '
      vname(3,indxHS)='meter                                      '
      vname(4,indxHS)='snow_thick, scalar, series                 '

      vname(1,indxTIsrf)='tisrf                                   '
      vname(2,indxTIsrf)='temperature of ice surface              '
      vname(3,indxTIsrf)='Kelvin                                  '
      vname(4,indxTIsrf)='tsrf, scalar, series                    '
#endif
#ifdef BULK_FLUX
#ifdef ONLINE /* JG ONLINE */
      do blkvar_id=1,nblkvrs
        itbulkO(blkvar_id)=0
	newbulk(blkvar_id)=.true.
        bulk_timeO(1:2,blkvar_id)=-1.E+20
        bulk_recO(blkvar_id)=0
      enddo
      call get_bulkvar_names
#else
CR      write(stdout,'(1x,A,I3)') 'indxWSPD =',indxWSPD
      vname(1,indxWSPD)='wspd                                     '
      vname(2,indxWSPD)='surface wind speed 10 m                  '
      vname(3,indxWSPD)='meter second-1                           '
      vname(4,indxWSPD)='surface wind speed, scalar, series       '

CR      write(stdout,'(1x,A,I3)') 'indxTAIR =',indxTAIR
      vname(1,indxTAIR)='tair                                     '
      vname(2,indxTAIR)='surface air temperature 2m               '
      vname(3,indxTAIR)='Celsius                                  '
      vname(4,indxTAIR)='surface air temperature, scalar, series  '

CR      write(stdout,'(1x,A,I3)') 'indxRHUM =',indxRHUM
      vname(1,indxRHUM)='rhum                                     '
      vname(2,indxRHUM)='surface air relative humidity 2m         '
      vname(3,indxRHUM)='fraction                                 '
      vname(4,indxRHUM)='surface relative humidity, scalar, series'

#ifndef BULK_LW
CR      write(stdout,'(1x,A,I3)') 'indxRADLW =',indxRADLW
      vname(1,indxRADLW)='radlw                                   '
      vname(2,indxRADLW)='net terrestrial longwave radiation      '
      vname(3,indxRADLW)='Watts meter-2                           '
      vname(4,indxRADLW)='terrestrial longwave, scalar, series    '
#else
CR      write(stdout,'(1x,A,I3)') 'indxRADLW =',indxRADLW
      vname(1,indxRADLW)='radlw_in                                '
      vname(2,indxRADLW)='downward longwave radiation             '
      vname(3,indxRADLW)='Watts meter-2                           '
      vname(4,indxRADLW)='downward longwave, scalar, series       '
#endif  /* BULK_LW */ 
CR      write(stdout,'(1x,A,I3)') 'indxRADSW =',indxRADSW
      vname(1,indxRADSW)='radsw                                   '
      vname(2,indxRADSW)='net solar shortwave radiation           '
      vname(3,indxRADSW)='Watts meter-2                           '
      vname(4,indxRADSW)='solar shortwave, scalar, series         '

CR      write(stdout,'(1x,A,I3)') 'indxPRATE =',indxPRATE
      vname(1,indxPRATE)='prate                                   '
      vname(2,indxPRATE)='surface precipitation rate              '
      vname(3,indxPRATE)='Kg meter-2 second-1                     '
      vname(4,indxPRATE)='precipitation rate, scalar, series      '

CR      write(stdout,'(1x,A,I3)') 'indxUWND =',indxUWND
      vname(1,indxUWND)='uwnd                                     '
      vname(2,indxUWND)='surface u-wind speed 10 m            '
      vname(3,indxUWND)='meter second-1                           '
      vname(4,indxUWND)='surface wind speed, scalar, series       '

CR      write(stdout,'(1x,A,I3)') 'indxVWND =',indxVWND
      vname(1,indxVWND)='vwnd                                     '
      vname(2,indxVWND)='surface v-wind speed 10 m       '
      vname(3,indxVWND)='meter second-1                           '
      vname(4,indxVWND)='surface wind speed, scalar, series       '
#endif /* ONLINE */
#endif /* BULK_FLUX */

# ifdef DIAGNOSTICS_TS
      do itrc=1,NT
CR       write(stdout,'(1x,A,I3)') 'indxTXadv = ',indxTXadv+itrc-1
       lvar=lenstr(vname(1,indxT+itrc-1))
       nametrc=vname(1,indxT+itrc-1)(1:lvar)
       lvar=lenstr(nametrc)
       unitt=vname(3,indxT+itrc-1)
       write(vname1,*) trim(nametrc),'_xadv                       '
       vname(1,indxTXadv+itrc-1)=vname1
CR       write(stdout,'(20x,A)') vname(1,indxTXadv+itrc-1)
       vname(2,indxTXadv+itrc-1)='Horizontal (xi) advection term  '
       write(vname3,*) trim(unitt),' second-1                     '
       vname(3,indxTXadv+itrc-1)=vname3

CR       write(stdout,'(1x,A,I3)') 'indxTYadv = ',indxTYadv+itrc-1
       nametrc=vname(1,indxT+itrc-1)
       unitt=vname(3,indxT+itrc-1)
       write(vname1,*) trim(nametrc),'_yadv                       '
       vname(1,indxTYadv+itrc-1)=vname1
CR       write(stdout,'(20x,A)') vname(1,indxTYadv+itrc-1)
       vname(2,indxTYadv+itrc-1)='Horizontal (eta) advection term '
       write(vname3,*) trim(unitt),' second-1                     '
       vname(3,indxTYadv+itrc-1)=vname3

CR       write(stdout,'(1x,A,I3)') 'indxTVadv = ',indxTVadv+itrc-1
       nametrc=vname(1,indxT+itrc-1)
       unitt=vname(3,indxT+itrc-1)
       write(vname1,*) trim(nametrc),'_vadv                       ' 
       vname(1,indxTVadv+itrc-1)=vname1
CR       write(stdout,'(20x,A)') vname(1,indxTVadv+itrc-1)
       vname(2,indxTVadv+itrc-1)='Vertical advection term         '
       write(vname3,*) trim(unitt),' second-1                     '
       vname(3,indxTVadv+itrc-1)=vname3

CR       write(stdout,'(1x,A,I3)') 'indxTHmix = ',indxTHmix+itrc-1
       nametrc=vname(1,indxT+itrc-1)
       unitt=vname(3,indxT+itrc-1)
       write(vname1,*) trim(nametrc),'_hmix                       '
       vname(1,indxTHmix+itrc-1)=vname1
CR       write(stdout,'(20x,A)') vname(1,indxTHmix+itrc-1)
       vname(2,indxTHmix+itrc-1)='Horizontal mixing term          '
       write(vname3,*) trim(unitt),' second-1                     '
       vname(3,indxTHmix+itrc-1)=vname3

CR       write(stdout,'(1x,A,I3)') 'indxTVmix = ',indxTVmix+itrc-1
       nametrc=vname(1,indxT+itrc-1)
       unitt=vname(3,indxT+itrc-1)
       write(vname1,*) trim(nametrc),'_vmix                       ' 
       vname(1,indxTVmix+itrc-1)=vname1
CR       write(stdout,'(20x,A)') vname(1,indxTVmix+itrc-1)
       vname(2,indxTVmix+itrc-1)='Vertical mixing term            '
       write(vname3,*) trim(unitt),' second-1                     '
       vname(3,indxTVmix+itrc-1)=vname3

CR       write(stdout,'(1x,A,I3)') 'indxTForc = ',indxTForc+itrc-1
       nametrc=vname(1,indxT+itrc-1)
       unitt=vname(3,indxT+itrc-1)
       write(vname1,*) trim(nametrc),'_forc                       '
       vname(1,indxTForc+itrc-1)=vname1
CR       write(stdout,'(20x,A)') vname(1,indxTForc+itrc-1)
       vname(2,indxTForc+itrc-1)='Forcing term (Q & Nudging)      '
       write(vname3,*) trim(unitt),' second-1                     '
       vname(3,indxTForc+itrc-1)=vname3

CR       write(stdout,'(1x,A,I3)') 'indxTrate = ',indxTrate+itrc-1
       nametrc=vname(1,indxT+itrc-1)
       unitt=vname(3,indxT+itrc-1)
       write(vname1,*) trim(nametrc),'_rate                       '
       vname(1,indxTrate+itrc-1)=vname1
CR       write(stdout,'(20x,A)') vname(1,indxTrate+itrc-1)
       vname(2,indxTrate+itrc-1)='Time rate of change             '
       write(vname3,*) trim(unitt),' second-1                     '
       vname(3,indxTrate+itrc-1)=vname3

!
! Tracer diagnostics variables averaged over the MLD
!
#  ifdef DIAGNOSTICS_TS_MLD
CR       write(stdout,'(1x,A,I3)') 'indxTXadv_mld = ',indxTXadv_mld+itrc-1
       lvar=lenstr(vname(1,indxT+itrc-1))
       nametrc=vname(1,indxT+itrc-1)(1:lvar)
       lvar=lenstr(nametrc)
       unitt=vname(3,indxT+itrc-1)
       write(vname1,*) trim(nametrc),'_xadv_mld                   '
       vname(1,indxTXadv_mld+itrc-1)=vname1
CR       write(stdout,'(20x,A)') vname(1,indxTXadv_mld+itrc-1)
       vname(2,indxTXadv_mld+itrc-1)='MLD xi advection term       '
       write(vname3,*) trim(unitt),' second-1                     '
       vname(3,indxTXadv_mld+itrc-1)=vname3

CR       write(stdout,'(1x,A,I3)') 'indxTYadv_mld = ',indxTYadv_mld+itrc-1
       nametrc=vname(1,indxT+itrc-1)
       unitt=vname(3,indxT+itrc-1)
       write(vname1,*) trim(nametrc),'_yadv_mld                   '
       vname(1,indxTYadv_mld+itrc-1)=vname1
CR       write(stdout,'(20x,A)') vname(1,indxTYadv_mld+itrc-1)
       vname(2,indxTYadv_mld+itrc-1)='MLD eta advection term      '
       write(vname3,*) trim(unitt),' second-1                     '
       vname(3,indxTYadv_mld+itrc-1)=vname3

CR       write(stdout,'(1x,A,I3)') 'indxTVadv_mld = ',indxTVadv_mld+itrc-1
       nametrc=vname(1,indxT+itrc-1)
       unitt=vname(3,indxT+itrc-1)
       write(vname1,*) trim(nametrc),'_vadv_mld                   ' 
       vname(1,indxTVadv_mld+itrc-1)=vname1
CR       write(stdout,'(20x,A)') vname(1,indxTVadv+itrc-1)
       vname(2,indxTVadv_mld+itrc-1)='MLD vertical advection term '
       write(vname3,*) trim(unitt),' second-1                     '
       vname(3,indxTVadv_mld+itrc-1)=vname3

CR       write(stdout,'(1x,A,I3)') 'indxTHmix_mld = ',indxTHmix_mld+itrc-1
       nametrc=vname(1,indxT+itrc-1)
       unitt=vname(3,indxT+itrc-1)
       write(vname1,*) trim(nametrc),'_hmix_mld                   '
       vname(1,indxTHmix_mld+itrc-1)=vname1
CR       write(stdout,'(20x,A)') vname(1,indxTHmix_mld+itrc-1)
       vname(2,indxTHmix_mld+itrc-1)='MLD xi mixing term          '
       write(vname3,*) trim(unitt),' second-1                     '
       vname(3,indxTHmix_mld+itrc-1)=vname3

CR       write(stdout,'(1x,A,I3)') 'indxTVmix_mld = ',indxTVmix_mld+itrc-1
       nametrc=vname(1,indxT+itrc-1)
       unitt=vname(3,indxT+itrc-1)
       write(vname1,*) trim(nametrc),'_vmix_mld                   ' 
       vname(1,indxTVmix_mld+itrc-1)=vname1
CR       write(stdout,'(20x,A)') vname(1,indxTVmix_mld+itrc-1)
       vname(2,indxTVmix_mld+itrc-1)='MLD vertical mixing term    '
       write(vname3,*) trim(unitt),' second-1                     '
       vname(3,indxTVmix_mld+itrc-1)=vname3

CR       write(stdout,'(1x,A,I3)') 'indxTForc_mld = ',indxTForc_mld+itrc-1
       nametrc=vname(1,indxT+itrc-1)
       unitt=vname(3,indxT+itrc-1)
       write(vname1,*) trim(nametrc),'_forc_mld                   '
       vname(1,indxTForc_mld+itrc-1)=vname1
CR       write(stdout,'(20x,A)') vname(1,indxTForc_mld+itrc-1)
       vname(2,indxTForc_mld+itrc-1)='MLD Forcing term (Q & Nudg) '
       write(vname3,*) trim(unitt),' second-1                     '
       vname(3,indxTForc_mld+itrc-1)=vname3

CR       write(stdout,'(1x,A,I3)') 'indxTrate_mld = ',indxTrate_mld+itrc-1
       nametrc=vname(1,indxT+itrc-1)
       unitt=vname(3,indxT+itrc-1)
       write(vname1,*) trim(nametrc),'_rate_mld                   '
       vname(1,indxTrate_mld+itrc-1)=vname1
CR       write(stdout,'(20x,A)') vname(1,indxTrate_mld+itrc-1)
       vname(2,indxTrate_mld+itrc-1)='MLD time rate of change     '
       write(vname3,*) trim(unitt),' second-1                     '
       vname(3,indxTrate_mld+itrc-1)=vname3

CR       write(stdout,'(1x,A,I3)') 'indxTentr_mld = ',indxTentr_mld+itrc-1
       nametrc=vname(1,indxT+itrc-1)
       unitt=vname(3,indxT+itrc-1)
       write(vname1,*) trim(nametrc),'_entr_mld                   '
       vname(1,indxTentr_mld+itrc-1)=vname1
CR       write(stdout,'(20x,A)') vname(1,indxTentr_mld+itrc-1)
       vname(2,indxTentr_mld+itrc-1)='MLD entrainment rate        '
       write(vname3,*) trim(unitt),' second-1                     '
       vname(3,indxTentr_mld+itrc-1)=vname3
#  endif

      enddo
# endif  /* DIAGNOSTICS_TS */

# ifdef DIAGNOSTICS_UV
      do itrc=1,2
CR       write(stdout,'(1x,A,I3)') 'indxMXadv = ',indxMXadv+itrc-1
       lvar=lenstr(vname(1,indxU+itrc-1))
       nametrc=vname(1,indxU+itrc-1)(1:lvar)
       lvar=lenstr(nametrc)
       unitt=vname(3,indxU+itrc-1)
       write(vname1,*) trim(nametrc),'_xadv                       '
       vname(1,indxMXadv+itrc-1)=vname1
CR       write(stdout,'(20x,A)') vname(1,indxMxadv+itrc-1)
       vname(2,indxMXadv+itrc-1)='Horizontal (xi) advection term  '
       write(vname3,*) trim(unitt),' second-1                     '
       vname(3,indxMXadv+itrc-1)=vname3

CR       write(stdout,'(1x,A,I3)') 'indxMYadv = ',indxMYadv+itrc-1
       nametrc=vname(1,indxU+itrc-1)
       unitt=vname(3,indxU+itrc-1)
       write(vname1,*) trim(nametrc),'_yadv                       '
       vname(1,indxMYadv+itrc-1)=vname1
CR       write(stdout,'(20x,A)') vname(1,indxMYadv+itrc-1)
       vname(2,indxMYadv+itrc-1)='Horizontal (eta) advection term '
       write(vname3,*) trim(unitt),' second-1                     '
       vname(3,indxMYadv+itrc-1)=vname3

CR       write(stdout,'(1x,A,I3)') 'indxMVadv = ',indxMVadv+itrc-1
       nametrc=vname(1,indxU+itrc-1)
       unitt=vname(3,indxU+itrc-1)
       write(vname1,*) trim(nametrc),'_vadv                       ' 
       vname(1,indxMVadv+itrc-1)=vname1
CR       write(stdout,'(20x,A)') vname(1,indxMVadv+itrc-1)
       vname(2,indxMVadv+itrc-1)='Vertical advection term         '
       write(vname3,*) trim(unitt),' second-1                     '
       vname(3,indxMVadv+itrc-1)=vname3

CR       write(stdout,'(1x,A,I3)') 'indxMCor = ',indxMCor+itrc-1
       nametrc=vname(1,indxU+itrc-1)
       unitt=vname(3,indxU+itrc-1)
       write(vname1,*) trim(nametrc),'_cor                        '
       vname(1,indxMCor+itrc-1)=vname1
CR       write(stdout,'(20x,A)') vname(1,indxMcor+itrc-1)
       vname(2,indxMCor+itrc-1)='Coriolis term                    '
       write(vname3,*) trim(unitt),' second-1                     '
       vname(3,indxMCor+itrc-1)=vname3

CR       write(stdout,'(1x,A,I3)') 'indxMPrsgrd = ',indxMPrsgrd+itrc-1
       nametrc=vname(1,indxU+itrc-1)
       unitt=vname(3,indxU+itrc-1)
       write(vname1,*) trim(nametrc),'_Prsgrd                     '
       vname(1,indxMPrsgrd+itrc-1)=vname1
CR       write(stdout,'(20x,A)') vname(1,indxMPrsgrd+itrc-1)
       vname(2,indxMPrsgrd+itrc-1)='Pressure gradient term        '
       write(vname3,*) trim(unitt),' second-1                     '
       vname(3,indxMPrsgrd+itrc-1)=vname3

CR       write(stdout,'(1x,A,I3)') 'indxMHmix = ',indxMHmix+itrc-1
       nametrc=vname(1,indxU+itrc-1)
       unitt=vname(3,indxU+itrc-1)
       write(vname1,*) trim(nametrc),'_hmix                       '
       vname(1,indxMHmix+itrc-1)=vname1
CR       write(stdout,'(20x,A)') vname(1,indxMHmix+itrc-1)
       vname(2,indxMHmix+itrc-1)='Horizontal mixing term          '
       write(vname3,*) trim(unitt),' second-1                     '
       vname(3,indxMHmix+itrc-1)=vname3

CR       write(stdout,'(1x,A,I3)') 'indxMVmix = ',indxMVmix+itrc-1
       nametrc=vname(1,indxU+itrc-1)
       unitt=vname(3,indxU+itrc-1)
       write(vname1,*) trim(nametrc),'_vmix                       ' 
       vname(1,indxMVmix+itrc-1)=vname1
CR       write(stdout,'(20x,A)') vname(1,indxMVmix+itrc-1)
       vname(2,indxMVmix+itrc-1)='Vertical mixing term            '
       write(vname3,*) trim(unitt),' second-1                     '
       vname(3,indxMVmix+itrc-1)=vname3
       
CR       write(stdout,'(1x,A,I3)') 'indxMrate = ',indxMrate+itrc-1
       nametrc=vname(1,indxU+itrc-1)
       unitt=vname(3,indxU+itrc-1)
       write(vname1,*) trim(nametrc),'_rate                       '
       vname(1,indxMrate+itrc-1)=vname1
CR       write(stdout,'(20x,A)') vname(1,indxMrate+itrc-1)
       vname(2,indxMrate+itrc-1)='Time rate of change             '
       write(vname3,*) trim(unitt),' second-1                     '
       vname(3,indxMrate+itrc-1)=vname3
      enddo
# endif /* DIAGNOSTICS_UV */

# ifdef DIAGNOSTICS_BIO
#   ifdef BIO_NChlPZD
CR      write(stdout,'(1x,A,I3)') 'indxbioFlux(NFlux_NewProd)='
CR     &                          ,indxbioFlux+NFlux_NewProd-1
      vname(1,indxbioFlux+NFlux_NewProd-1)=
     &     'NFlux_NewProd                             '
      vname(2,indxbioFlux+NFlux_NewProd-1)=
     &     'Flux of N due to new production           '
      vname(3,indxbioFlux+NFlux_NewProd-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioFlux+NFlux_NewProd-1)=
     &     'NFlux_NewProd, scalar, series             '

      vname(1,indxbioFlux+NFlux_Grazing-1)=
     &     'NFlux_Grazing                             '
      vname(2,indxbioFlux+NFlux_Grazing-1)=
     &     'Flux of N due to grazing                  '
      vname(3,indxbioFlux+NFlux_Grazing-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioFlux+NFlux_Grazing-1)=
     &     'NFlux_Grazing, scalar, series             '

      vname(1,indxbioFlux+NFlux_SlopFeed-1)=
     &     'NFlux_SlopFeed                            '
      vname(2,indxbioFlux+NFlux_SlopFeed-1)=
     &     'Flux of N due to sloppy feeding           '
      vname(3,indxbioFlux+NFlux_SlopFeed-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioFlux+NFlux_SlopFeed-1)=
     &     'NFlux_SlopFeed, scalar, series            '

      vname(1,indxbioFlux+NFlux_Pmort-1)=
     &     'NFlux_Pmort                               '
      vname(2,indxbioFlux+NFlux_Pmort-1)=
     &     'Flux of N due to phytoplankton mortality  '
      vname(3,indxbioFlux+NFlux_Pmort-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioFlux+NFlux_Pmort-1)=
     &     'NFlux_Pmort, scalar, series               '

      vname(1,indxbioFlux+NFlux_Zmetab-1)=
     &     'NFlux_Zmetab                              '
      vname(2,indxbioFlux+NFlux_Zmetab-1)=
     &     'Flux of N due to zooplankton metabolism   '
      vname(3,indxbioFlux+NFlux_Zmetab-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioFlux+NFlux_Zmetab-1)=
     &     'NFlux_Zmetab, scalar, series              '

      vname(1,indxbioFlux+NFlux_Zmort-1)=
     &     'NFlux_Zmort                               '
      vname(2,indxbioFlux+NFlux_Zmort-1)=
     &     'Flux of N due to zooplankton mortality    '
      vname(3,indxbioFlux+NFlux_Zmort-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioFlux+NFlux_Zmort-1)=
     &     'NFlux_Zmort, scalar, series               '

CR      write(stdout,'(1x,A,I3)') 'indxbioFlux(NFlux_ReminD)='
CR     &                          ,indxbioFlux+NFlux_ReminD-1
      vname(1,indxbioFlux+NFlux_ReminD-1)=
     &     'NFlux_ReminD                                   '
      vname(2,indxbioFlux+NFlux_ReminD-1)=
     &     'Flux of N due to remin. of detritus            '
      vname(3,indxbioFlux+NFlux_ReminD-1)=
     &     'mmol N m-3 s-1                                 '
      vname(4,indxbioFlux+NFlux_ReminD-1)=
     &     'NFlux_ReminD, scalar, series                   '

#    ifdef OXYGEN
CR      write(stdout,'(1x,A,I3)') 'indxbioFlux(OGain_NewProd)='
CR     &                          ,indxbioFlux+OGain_NewProd-1
      vname(1,indxbioFlux+OGain_NewProd-1)=
     &     'OGain_NewProd                              '
      vname(2,indxbioFlux+OGain_NewProd-1)=
     &     'Flux of O2 due to new production           '
      vname(3,indxbioFlux+OGain_NewProd-1)=
     &     'mmol N m-3 s-1                             '
      vname(4,indxbioFlux+OGain_NewProd-1)=
     &     'OGain_NewProd, scalar, series              '

CR      write(stdout,'(1x,A,I3)') 'indxbioFlux(OLoss_Zmetab)='
CR     &                          ,indxbioFlux+OLoss_Zmetab-1
      vname(1,indxbioFlux+OLoss_Zmetab-1)=
     &     'OLoss_Zmetab                               '
      vname(2,indxbioFlux+OLoss_Zmetab-1)=
     &     'Flux of O2 due to zooplankton metabolism   '
      vname(3,indxbioFlux+OLoss_Zmetab-1)=
     &     'mmol O2 m-3 s-1                            '
      vname(4,indxbioFlux+OLoss_Zmetab-1)=
     &     'OLoss_Zmetab, scalar, series               '

CR      write(stdout,'(1x,A,I3)') 'indxbioFlux(OLoss_ReminD)='
CR     &                          ,indxbioFlux+OLoss_ReminD-1
      vname(1,indxbioFlux+OLoss_ReminD-1)=
     &     'OLoss_ReminD                               '
      vname(2,indxbioFlux+OLoss_ReminD-1)=
     &     'Flux of O2 due to remin. of small detritus '
      vname(3,indxbioFlux+OLoss_ReminD-1)=
     &     'mmol O2 m-3 s-1                            '
      vname(4,indxbioFlux+OLoss_ReminD-1)=
     &     'OLoss_ReminD, scalar, series               '
#    endif /* OXYGEN */

! vertical sinking terms
CR      write(stdout,'(1x,A,I3)') 'indxbioVSink(NFlux_VSinkP1)='
CR     &                          ,indxbioVSink+NFlux_VSinkP1-1
      vname(1,indxbioVSink+NFlux_VSinkP1-1)=
     &     'NFlux_VSinkP1                                    '
      vname(2,indxbioVSink+NFlux_VSinkP1-1)=
     &     'Vertical sinking flux of phytoplankton           '
      vname(3,indxbioVSink+NFlux_VSinkP1-1)=
     &     'mmol N m-3 s-1                                   '
      vname(4,indxbioVSink+NFlux_VSinkP1-1)=
     &     'NFlux_VSinkP1, scalar, series                    '

CR      write(stdout,'(1x,A,I3)') 'indxbioVSink(NFlux_VSinkD1)='
CR     &                          ,indxbioVSink+NFlux_VSinkD1-1
      vname(1,indxbioVSink+NFlux_VSinkD1-1)=
     &     'NFlux_VSinkD1                                    '
      vname(2,indxbioVSink+NFlux_VSinkD1-1)=
     &     'Vertical sinking flux of detritus N              '
      vname(3,indxbioVSink+NFlux_VSinkD1-1)=
     &     'mmol N m-3 s-1                                   '
      vname(4,indxbioVSink+NFlux_VSinkD1-1)=
     &     'NFlux_VSinkD1, scalar, series                     '

#    ifdef OXYGEN
! gas exchange terms
CR      write(stdout,'(1x,A,I3)') 'indxGasExcFlux='
CR     &                          ,indxGasExcFlux
      vname(1,indxGasExcFlux)=
     &     'GasExchangeO2                              '
      vname(2,indxGasExcFlux)=
     &     'Gas exchange of oxygen at the surface      '
      vname(3,indxGasExcFlux)=
     &     'mmol O2 m-2 s-1                            '
      vname(4,indxGasExcFlux)=
     &     'GasExchangeO2, scalar, series              '
#    endif /* OXYGEN */
#   elif defined BIO_N2P2Z2D2
CR      write(stdout,'(1x,A,I3)') 'indxbioFlux(NFlux_NewProd1)='
CR     &                          ,indxbioFlux+NFlux_NewProd1-1
      vname(1,indxbioFlux+NFlux_NewProd1-1)=
     &     'NFlux_NewProd1                            '
      vname(2,indxbioFlux+NFlux_NewProd1-1)=
     &     'Flux of N due to new production           '
      vname(3,indxbioFlux+NFlux_NewProd1-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioFlux+NFlux_NewProd1-1)=
     &     'NFlux_NewProd1, scalar, series            '

      vname(1,indxbioFlux+NFlux_NewProd2-1)=
     &     'NFlux_NewProd2                            '
      vname(2,indxbioFlux+NFlux_NewProd2-1)=
     &     'Flux of N due to new production           '
      vname(3,indxbioFlux+NFlux_NewProd2-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioFlux+NFlux_NewProd2-1)=
     &     'NFlux_NewProd2, scalar, series            '

      vname(1,indxbioFlux+NFlux_RegProd1-1)=
     &     'NFlux_RegProd1                            '
      vname(2,indxbioFlux+NFlux_RegProd1-1)=
     &     'Flux of N due to reg production           '
      vname(3,indxbioFlux+NFlux_RegProd1-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioFlux+NFlux_RegProd1-1)=
     &     'NFlux_RegProd1, scalar, series            '

      vname(1,indxbioFlux+NFlux_RegProd2-1)=
     &     'NFlux_RegProd2                            '
      vname(2,indxbioFlux+NFlux_RegProd2-1)=
     &     'Flux of N due to reg production           '
      vname(3,indxbioFlux+NFlux_RegProd2-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioFlux+NFlux_RegProd2-1)=
     &     'NFlux_RegProd2, scalar, series            '

      vname(1,indxbioFlux+NFlux_Nitrific-1)=
     &     'NFlux_Nitrific                            '
      vname(2,indxbioFlux+NFlux_Nitrific-1)=
     &     'Flux of N due to Nitrification            '
      vname(3,indxbioFlux+NFlux_Nitrific-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioFlux+NFlux_Nitrific-1)=
     &     'NFlux_Nitrific, scalar, series            '

      vname(1,indxbioFlux+NFlux_P1Z1Grazing-1)=
     &     'NFlux_P1Z1Grazing                         '
      vname(2,indxbioFlux+NFlux_P1Z1Grazing-1)=
     &     'Flux of N due to grazing of Z1 on P1      '
      vname(3,indxbioFlux+NFlux_P1Z1Grazing-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioFlux+NFlux_P1Z1Grazing-1)=
     &     'NFlux_P1Z1Grazing, scalar, series         '

      vname(1,indxbioFlux+NFlux_P2Z1Grazing-1)=
     &     'NFlux_P2Z1Grazing                         '
      vname(2,indxbioFlux+NFlux_P2Z1Grazing-1)=
     &     'Flux of N due to grazing of Z1 on P2      '
      vname(3,indxbioFlux+NFlux_P2Z1Grazing-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioFlux+NFlux_P2Z1Grazing-1)=
     &     'NFlux_P2Z1Grazing, scalar, series         '

      vname(1,indxbioFlux+NFlux_P1Z2Grazing-1)=
     &     'NFlux_P1Z2Grazing                         '
      vname(2,indxbioFlux+NFlux_P1Z2Grazing-1)=
     &     'Flux of N due to grazing of Z2 on P1      '
      vname(3,indxbioFlux+NFlux_P1Z2Grazing-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioFlux+NFlux_P1Z2Grazing-1)=
     &     'NFlux_P1Z2Grazing, scalar, series         '

      vname(1,indxbioFlux+NFlux_P2Z2Grazing-1)=
     &     'NFlux_P2Z2Grazing                         '
      vname(2,indxbioFlux+NFlux_P2Z2Grazing-1)=
     &     'Flux of N due to grazing of Z2 on P2      '
      vname(3,indxbioFlux+NFlux_P2Z2Grazing-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioFlux+NFlux_P2Z2Grazing-1)=
     &     'NFlux_P2Z2Grazing, scalar, series         '

      vname(1,indxbioFlux+NFlux_Z1Z2Grazing-1)=
     &     'NFlux_Z1Z2Grazing                         '
      vname(2,indxbioFlux+NFlux_Z1Z2Grazing-1)=
     &     'Flux of N due to grazing of Z2 on Z1      '
      vname(3,indxbioFlux+NFlux_Z1Z2Grazing-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioFlux+NFlux_Z1Z2Grazing-1)=
     &     'NFlux_Z1Z2Grazing, scalar, series         '

      vname(1,indxbioFlux+NFlux_SlopFeedZ1-1)=
     &     'NFlux_SlopFeedZ1                          '
      vname(2,indxbioFlux+NFlux_SlopFeedZ1-1)=
     &     'Flux of N due to sloppy feeding for Z1    '
      vname(3,indxbioFlux+NFlux_SlopFeedZ1-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioFlux+NFlux_SlopFeedZ1-1)=
     &     'NFlux_SlopFeedZ1, scalar, series          '

      vname(1,indxbioFlux+NFlux_SlopFeedZ2-1)=
     &     'NFlux_SlopFeedZ2                          '
      vname(2,indxbioFlux+NFlux_SlopFeedZ2-1)=
     &     'Flux of N due to sloppy feeding for Z2    '
      vname(3,indxbioFlux+NFlux_SlopFeedZ2-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioFlux+NFlux_SlopFeedZ2-1)=
     &     'NFlux_SlopFeedZ2, scalar, series          '

      vname(1,indxbioFlux+NFlux_P1mort-1)=
     &     'NFlux_P1mort                              '
      vname(2,indxbioFlux+NFlux_P1mort-1)=
     &     'Flux of N due to P1 mortality             '
      vname(3,indxbioFlux+NFlux_P1mort-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioFlux+NFlux_P1mort-1)=
     &     'NFlux_P1mort, scalar, series              '

      vname(1,indxbioFlux+NFlux_P2mort-1)=
     &     'NFlux_P2mort                              '
      vname(2,indxbioFlux+NFlux_P2mort-1)=
     &     'Flux of N due to P2 mortality             '
      vname(3,indxbioFlux+NFlux_P2mort-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioFlux+NFlux_P2mort-1)=
     &     'NFlux_P2mort, scalar, series              '

      vname(1,indxbioFlux+NFlux_Z1metab-1)=
     &     'NFlux_Z1metab                             '
      vname(2,indxbioFlux+NFlux_Z1metab-1)=
     &     'Flux of N due to Z1 metabolism            '
      vname(3,indxbioFlux+NFlux_Z1metab-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioFlux+NFlux_Z1metab-1)=
     &     'NFlux_Z1metab, scalar, series             '

      vname(1,indxbioFlux+NFlux_Z2metab-1)=
     &     'NFlux_Z2metab                             '
      vname(2,indxbioFlux+NFlux_Z2metab-1)=
     &     'Flux of N due to Z2 metabolism            '
      vname(3,indxbioFlux+NFlux_Z2metab-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioFlux+NFlux_Z2metab-1)=
     &     'NFlux_Z2metab, scalar, series             '

      vname(1,indxbioFlux+NFlux_Z1mort-1)=
     &     'NFlux_Z1mort                              '
      vname(2,indxbioFlux+NFlux_Z1mort-1)=
     &     'Flux of N due to Z1 mortality             '
      vname(3,indxbioFlux+NFlux_Z1mort-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioFlux+NFlux_Z1mort-1)=
     &     'NFlux_Z1mort, scalar, series              '

      vname(1,indxbioFlux+NFlux_Z2mort-1)=
     &     'NFlux_Z2mort                              '
      vname(2,indxbioFlux+NFlux_Z2mort-1)=
     &     'Flux of N due to Z2 mortality             '
      vname(3,indxbioFlux+NFlux_Z2mort-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioFlux+NFlux_Z2mort-1)=
     &     'NFlux_Z2mort, scalar, series              '

      vname(1,indxbioFlux+NFlux_ReminD1-1)=
     &     'NFlux_ReminD1                             '
      vname(2,indxbioFlux+NFlux_ReminD1-1)=
     &     'Flux of N due to remin. of D1             '
      vname(3,indxbioFlux+NFlux_ReminD1-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioFlux+NFlux_ReminD1-1)=
     &     'NFlux_ReminD1, scalar, series             '

CR      write(stdout,'(1x,A,I3)') 'indxbioFlux(NFlux_ReminD2)='
CR     &                          ,indxbioFlux+NFlux_ReminD2-1
      vname(1,indxbioFlux+NFlux_ReminD2-1)=
     &     'NFlux_ReminD2                                   '
      vname(2,indxbioFlux+NFlux_ReminD2-1)=
     &     'Flux of N due to remin. of D2                   '
      vname(3,indxbioFlux+NFlux_ReminD2-1)=
     &     'mmol N m-3 s-1                                  '
      vname(4,indxbioFlux+NFlux_ReminD2-1)=
     &     'NFlux_ReminD2, scalar, series                   '

! vertical sinking terms
CR      write(stdout,'(1x,A,I3)') 'indxbioVSink(NFlux_VSinkP2)='
CR     &                          ,indxbioVSink+NFlux_VSinkP2-1
      vname(1,indxbioVSink+NFlux_VSinkP2-1)=
     &     'NFlux_VSinkP2                             '
      vname(2,indxbioVSink+NFlux_VSinkP2-1)=
     &     'Flux of N due to Vert. Sink. of P2        '
      vname(3,indxbioVSink+NFlux_VSinkP2-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioVSink+NFlux_VSinkP2-1)=
     &     'NFlux_VSinkP2, scalar, series             '

      vname(1,indxbioVSink+NFlux_VSinkD1-1)=
     &     'NFlux_VSinkD1                             '
      vname(2,indxbioVSink+NFlux_VSinkD1-1)=
     &     'Flux of N due to Vert. Sink. of D1        '
      vname(3,indxbioVSink+NFlux_VSinkD1-1)=
     &     'mmol N m-3 s-1                            '
      vname(4,indxbioVSink+NFlux_VSinkD1-1)=
     &     'NFlux_VSinkD1, scalar, series             '

CR      write(stdout,'(1x,A,I3)') 'indxbioVSink(NFlux_VSinkD2)='
CR     &                          ,indxbioVSink+NFlux_VSinkD2-1
      vname(1,indxbioVSink+NFlux_VSinkD2-1)=
     &     'NFlux_VSinkD2                                    '
      vname(2,indxbioVSink+NFlux_VSinkD2-1)=
     &     'Flux of N due to Vert. Sink. of D2               '
      vname(3,indxbioVSink+NFlux_VSinkD2-1)=
     &     'mmol N m-3 s-1                                   '
      vname(4,indxbioVSink+NFlux_VSinkD2-1)=
     &     'NFlux_VSinkD2, scalar, series                    '
#   elif defined PISCES
#    ifdef key_trc_dia3d
        vname(1,indxbioFlux+Nhi-1)=
     &     'PH                                        '
        vname(2,indxbioFlux+Nhi-1)=
     &     'PH                                        '
        vname(3,indxbioFlux+Nhi-1)=
     &     '-                                         '
        vname(4,indxbioFlux+Nhi-1)=
     &     'PH            , scalar, series            '

        vname(1,indxbioFlux+Nco3-1)=
     &     'CO3                                       '
        vname(2,indxbioFlux+Nco3-1)=
     &     'Bicarbonates                              '
        vname(3,indxbioFlux+Nco3-1)=
     &     'mol/L                                     '
        vname(4,indxbioFlux+Nco3-1)=
     &     'CO3           , scalar, series            '

        vname(1,indxbioFlux+Naksp-1)=
     &     'CO3sat                                    '
        vname(2,indxbioFlux+Naksp-1)=
     &     'Saturation CO3                            '
        vname(3,indxbioFlux+Naksp-1)=
     &     'mol/L                                     '
        vname(4,indxbioFlux+Naksp-1)=
     &     'CO3sat        , scalar, series            '

        vname(1,indxbioFlux+Netot-1)=
     &     'PAR                                       '
        vname(2,indxbioFlux+Netot-1)=
     &     'Photosynthetic Available radiation        '
        vname(3,indxbioFlux+Netot-1)=
     &     'W/m2                                      '
        vname(4,indxbioFlux+Netot-1)=
     &     'PAR           , scalar, series            '

        vname(1,indxbioFlux+Nprorca-1)=
     &     'PPN                                       '
        vname(2,indxbioFlux+Nprorca-1)=
     &     'Primary production Nanoplankton           '
        vname(3,indxbioFlux+Nprorca-1)=
     &     'molC/m3/s                                 '
        vname(4,indxbioFlux+Nprorca-1)=
     &     'PPN           , scalar, series            '


        vname(1,indxbioFlux+Nprorca2-1)=
     &     'PPD                                       '
        vname(2,indxbioFlux+Nprorca2-1)=
     &     'Primary production Diatoms                '
        vname(3,indxbioFlux+Nprorca2-1)=
     &     'molC/m3/s                                 '
        vname(4,indxbioFlux+Nprorca2-1)=
     &     'PPD           , scalar, series            '

        vname(1,indxbioFlux+Npronew-1)=
     &     'PN                                        '
        vname(2,indxbioFlux+Npronew-1)=
     &     'New Production Nanoplankton               '
        vname(3,indxbioFlux+Npronew-1)=
     &     'molC/m3/s                                 '
        vname(4,indxbioFlux+Npronew-1)=
     &     'PN            , scalar, series            '


        vname(1,indxbioFlux+Npronew2-1)=
     &     'PND                                       '
        vname(2,indxbioFlux+Npronew2-1)=
     &     'New Production Diatoms                    '
        vname(3,indxbioFlux+Npronew2-1)=
     &     'molC/m3/s                                 '
        vname(4,indxbioFlux+Npronew2-1)=
     &     'PND           , scalar, series            '


        vname(1,indxbioFlux+Nprorca3-1)=
     &     'PBSi                                       '
        vname(2,indxbioFlux+Nprorca3-1)=
     &     'Silicate Production Diatoms                '
        vname(3,indxbioFlux+Nprorca3-1)=
     &     'molSi/m3/s                                 '
        vname(4,indxbioFlux+Nprorca3-1)=
     &     'PBSi          , scalar, series             '

        vname(1,indxbioFlux+Nprorca4-1)=
     &     'PFeN                                       '
        vname(2,indxbioFlux+Nprorca4-1)=
     &     'Iron Production Nano                       '
        vname(3,indxbioFlux+Nprorca4-1)=
     &     'molFe/m3/s                                 '
        vname(4,indxbioFlux+Nprorca4-1)=
     &     'PFeN          , scalar, series             '

        vname(1,indxbioFlux+Nprorca5-1)=
     &     'PFeD                                       '
        vname(2,indxbioFlux+Nprorca5-1)=
     &     'Iron Production Diatoms                    '
        vname(3,indxbioFlux+Nprorca5-1)=
     &     'molFe/m3/s                                 '
        vname(4,indxbioFlux+Nprorca5-1)=
     &     'PFeD          , scalar, series             '

#    endif

#    ifdef key_trc_diaadd
        vname(1,indxbioVSink+Nfld-1)=
     &     'Cflx                                      '
        vname(2,indxbioVSink+Nfld-1)=
     &     'DIC Flux                                  '
        vname(3,indxbioVSink+Nfld-1)=
     &     'molC/m2/s                                 '
        vname(4,indxbioVSink+Nfld-1)=
     &     'Cflx            , scalar, series          '

        vname(1,indxbioVSink+Nflu16-1)=
     &     'Oflx                                      '
        vname(2,indxbioVSink+Nflu16-1)=
     &     'O2 Flux                                  '
        vname(3,indxbioVSink+Nflu16-1)=
     &     'molO/m2/s                                 '
        vname(4,indxbioVSink+Nflu16-1)=
     &     'Oflx            , scalar, series          '

        vname(1,indxbioVSink+Nkgco2-1)=
     &     'Kg                                        '
        vname(2,indxbioVSink+Nkgco2-1)=
     &     'Gaz Transfer                              '
        vname(3,indxbioVSink+Nkgco2-1)=
     &     'mol/m2/s/uatm                             '
        vname(4,indxbioVSink+Nkgco2-1)=
     &     'Kg              , scalar, series          '

        vname(1,indxbioVSink+Natcco2-1)=
     &     'DelC                                      '
        vname(2,indxbioVSink+Natcco2-1)=
     &     'Deltaco2                                  '
        vname(3,indxbioVSink+Natcco2-1)=
     &     'uatm                                      '
        vname(4,indxbioVSink+Natcco2-1)=
     &     'DelC            , scalar, series          '

        vname(1,indxbioVSink+Nsinking-1)=
     &     'ExpPOC1                                   '
        vname(2,indxbioVSink+Nsinking-1)=
     &     'small POC export                          '
        vname(3,indxbioVSink+Nsinking-1)=
     &     'molC/m2/s                                 '
        vname(4,indxbioVSink+Nsinking-1)=
     &     'ExpPOC1         , scalar, series          '

        vname(1,indxbioVSink+Nsinking2-1)=
     &     'ExpPOC2                                   '
        vname(2,indxbioVSink+Nsinking2-1)=
     &     'big POC export                            '
        vname(3,indxbioVSink+Nsinking2-1)=
     &     'molC/m2/s                                 '
        vname(4,indxbioVSink+Nsinking2-1)=
     &     'ExpPOC2         , scalar, series          '

        vname(1,indxbioVSink+Nsinkfer-1)=
     &     'ExpFe1                                    '
        vname(2,indxbioVSink+Nsinkfer-1)=
     &     'Fe export Nano                            '
        vname(3,indxbioVSink+Nsinkfer-1)=
     &     'molFe/m2/s                                '
        vname(4,indxbioVSink+Nsinkfer-1)=
     &     'ExpFe1          , scalar, series          '

        vname(1,indxbioVSink+Nsinkfer2-1)=
     &     'ExpFe2                                    '
        vname(2,indxbioVSink+Nsinkfer2-1)=
     &     'Fe export Diatoms                         '
        vname(3,indxbioVSink+Nsinkfer2-1)=
     &     'molFe/m2/s                                '
        vname(4,indxbioVSink+Nsinkfer2-1)=
     &     'ExpFe2          , scalar, series          '

        vname(1,indxbioVSink+Nsinksil-1)=
     &     'ExpSi                                     '
        vname(2,indxbioVSink+Nsinksil-1)=
     &     'BSi export                                '
        vname(3,indxbioVSink+Nsinksil-1)=
     &     'molSi/m2/s                                '
        vname(4,indxbioVSink+Nsinksil-1)=
     &     'ExpSi           , scalar, series          '

        vname(1,indxbioVSink+Nsinkcal-1)=
     &     'ExpCaco3                                  '
        vname(2,indxbioVSink+Nsinkcal-1)=
     &     'Calcite export                            '
        vname(3,indxbioVSink+Nsinkcal-1)=
     &     'molC/m2/s                                 '
        vname(4,indxbioVSink+Nsinkcal-1)=
     &     'ExpCaCo3        , scalar, series          '

        vname(1,indxbioVSink+Nzmeu-1)=
     &     'Zmeu                                      '
        vname(2,indxbioVSink+Nzmeu-1)=
     &     'Euphotic Depth (1%)                       '
        vname(3,indxbioVSink+Nzmeu-1)=
     &     'm                                         '
        vname(4,indxbioVSink+Nzmeu-1)=
     &     'Zmeu            , scalar, series          '

        vname(1,indxbioVSink+Nirondep-1)=
     &     'Fedep                                     '
        vname(2,indxbioVSink+Nirondep-1)=
     &     'Iron surface deposition flux              '
        vname(3,indxbioVSink+Nirondep-1)=
     &     'molFe/m2/s                                '
        vname(4,indxbioVSink+Nirondep-1)=
     &     'Fedep           , scalar, series          '

        vname(1,indxbioVSink+Nnitrpot-1)=
     &     'Nfix                                      '
        vname(2,indxbioVSink+Nnitrpot-1)=
     &     'Nitrogen Fixation                         '
        vname(3,indxbioVSink+Nnitrpot-1)=
     &     'molN/m2/s                                 '
        vname(4,indxbioVSink+Nnitrpot-1)=
     &     'Nfix            , scalar, series          '

#    endif

#   endif /* PISCES */
# endif /* DIAGNOSTICS_BIO */
#if defined TCLIMATOLOGY && !defined ANA_TCLIMA
!
! flag to read in climatology for tracer type variables.
!
      do itrc=1,NT
        got_tclm(itrc)=.false.
      enddo
#endif /* TCLIMATOLOGY */
      return
      end
