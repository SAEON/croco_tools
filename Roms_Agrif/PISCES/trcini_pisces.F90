#include "cppdefs.h"

MODULE trcini_pisces
   !!======================================================================
   !!                         ***  MODULE trcini_pisces  ***
   !! TOP :   initialisation of the PISCES biochemical model
   !!======================================================================
   !! History :    -   !  1988-07  (E. Maier-Reiner) Original code
   !!              -   !  1999-10  (O. Aumont, C. Le Quere)
   !!              -   !  2002     (O. Aumont)  PISCES
   !!             1.0  !  2005-03  (O. Aumont, A. El Moussaoui) F90
   !!             2.0  !  2007-12  (C. Ethe, G. Madec) from trcini.pisces.h90
   !!----------------------------------------------------------------------
#if defined key_pisces
   !!----------------------------------------------------------------------
   !!   'key_pisces'                                       PISCES bio-model
   !!----------------------------------------------------------------------
   !! trc_ini_pisces   : PISCES biochemical model initialisation
   !!----------------------------------------------------------------------
   USE sms_pisces      ! Source Minus Sink variables
   USE trcsms_pisces   !
   USE p4zche          !  Chemical model
   USE p4zsink         !  vertical flux of particulate matter due to sinking
   USE p4zopt          !  optical model
   USE p4zrem          !  Remineralisation of organic matter
   USE p4zflx          !  Gas exchange
   USE p4zlim          !  Co-limitations of differents nutrients
   USE p4zprod         !  Growth rate of the 2 phyto groups
   USE p4zmicro        !  Sources and sinks of microzooplankton
   USE p4zmeso         !  Sources and sinks of mesozooplankton
   USE p4zmort         !  Mortality terms for phytoplankton
   USE p4zlys          !  Calcite saturation
   USE p4zsed          !  Sedimentation & burial

   !!* Substitution
#  include "ocean2pisces.h90"
#  include "top_substitute.h90"


   IMPLICIT NONE
   PRIVATE

   PUBLIC   trc_ini_pisces   ! called by trcini.F90 module
   PUBLIC   trc_nam_pisces   ! called by trcini.F90 module

   !! * Module variables
   REAL(wp) :: &
      sco2   =  2.312e-3         , &
      alka0  =  2.423e-3         , &
      oxyg0  =  177.6e-6         , &
      po4    =  2.174e-6         , &
      bioma0 =  1.000e-8         , &
      silic1 =  91.65e-6         , &
      no3    =  31.04e-6 * 7.6

   !!----------------------------------------------------------------------
   !! NEMO/TOP 2.0 , LOCEAN-IPSL (2007) 
   !! $Id: trcini_pisces.F90 1808 2010-03-11 09:17:56Z cetlod $ 
   !! Software governed by the CeCILL licence (modipsl/doc/NEMO_CeCILL.txt)
   !!----------------------------------------------------------------------

CONTAINS

    SUBROUTINE trc_ini_pisces

      !!----------------------------------------------------------------------
      !!                   ***  ROUTINE trc_ini_pisces ***
      !!
      !! ** Purpose :   Initialisation of the PISCES biochemical model
      !!----------------------------------------------------------------------
      INTEGER  ::  ji, jj, jk, jn, ierr
      REAL(wp) ::  zcaralk, zbicarb, zco3, zdic, zalk
      REAL(wp) ::  ztmas, ztmas1
      REAL(wp), DIMENSION(jptra)       :: trai
      REAL(wp)                         :: areatot



      IF(lwp) WRITE(numout,*)
      IF(lwp) WRITE(numout,*) ' trc_ini_pisces :   PISCES biochemical model initialisation'
      IF(lwp) WRITE(numout,*) ' ~~~~~~~~~~~~~~'


      ierr =         sms_pisces_alloc()          
      ierr = ierr +  trc_alloc()          ! Start of PISCES-related alloc routines...
      ierr = ierr +  p4z_sink_alloc()
      ierr = ierr +  p4z_opt_alloc()
      ierr = ierr +  p4z_prod_alloc()
      ierr = ierr +  p4z_rem_alloc()
      ierr = ierr +  p4z_flx_alloc()
      ierr = ierr +  p4z_sed_alloc()
      !
      IF( lk_mpp    )   CALL mpp_sum( ierr )
      IF( ierr /= 0 )   CALL ctl_stop( 'STOP in trc_ini_pisces : unable to allocate PISCES arrays' )

      IF( ln_ctl )  CALL prt_ctl_trc_ini

      CALL trc_sms_pisces_init
      !                                            ! Time-step
      rfact   = rdt                                ! ---------
      rfactr  = 1. / rfact
      rfact2  = rfact / FLOAT( nrdttrc )
      rfact2r = 1. / rfact2
      xstep  = rfact2 / rday      ! Timestep duration for biology


      IF(lwp) WRITE(numout,*) 
      IF(lwp) WRITE(numout,*) '    Tracer  time step    rfact  = ', rfact, ' rdt = ', rdt
      IF(lwp) write(numout,*) '    Biology time step    rfact2 = ', rfact2
      IF(lwp) WRITE(numout,*) 



      ! Set biological ratios
      ! ---------------------
      rno3   =   16.   / 122.
      po4r   =   1.e0  / 122.
      o2nit  =  32.    / 122.
      rdenit =  97.6   /  16.
      o2ut   = 140.    / 122.

      CALL p4z_che        ! initialize the chemical constants

      ndayflxtr = nday_year      !  Initialize a counter for the computation of chemistry

      ! Initialization of tracer concentration in case of  no restart 
      !--------------------------------------------------------------
      ln_rsttr = ( nrrec /= 0 ) 
      !
      IF( .NOT. ln_rsttr ) THEN  
         DO jk = KRANGE
            DO jj = JRANGE
               DO ji = IRANGE
                  trn(ji,jj,K,jpdic) = sco2
                  trn(ji,jj,K,jpdoc) = bioma0
                  trn(ji,jj,K,jptal) = alka0
                  trn(ji,jj,K,jpoxy) = oxyg0
                  trn(ji,jj,K,jpcal) = bioma0
                  trn(ji,jj,K,jppo4) = po4 / po4r
                  trn(ji,jj,K,jppoc) = bioma0
#  if ! defined key_kriest
                  trn(ji,jj,K,jpgoc) = bioma0
                  trn(ji,jj,K,jpbfe) = bioma0 * 5.e-6
#  else
                  trn(ji,jj,K,jpnum) = bioma0 / ( 6. * xkr_massp )
#  endif
                  trn(ji,jj,K,jpsil) = silic1
                  trn(ji,jj,K,jpbsi) = bioma0 * 0.15
                  trn(ji,jj,K,jpdsi) = bioma0 * 5.e-6
                  trn(ji,jj,K,jpphy) = bioma0
                  trn(ji,jj,K,jpdia) = bioma0
                  trn(ji,jj,K,jpzoo) = bioma0
                  trn(ji,jj,K,jpmes) = bioma0
                  trn(ji,jj,K,jpfer) = 0.6E-9
                  trn(ji,jj,K,jpsfe) = bioma0 * 5.e-6
                  trn(ji,jj,K,jpdfe) = bioma0 * 5.e-6
                  trn(ji,jj,K,jpnfe) = bioma0 * 5.e-6
                  trn(ji,jj,K,jpnch) = bioma0 * 12. / 55.
                  trn(ji,jj,K,jpdch) = bioma0 * 12. / 55.
                  trn(ji,jj,K,jpno3) = no3
                  trn(ji,jj,K,jpnh4) = bioma0
               ENDDO
            ENDDO
         ENDDO

      ENDIF

      
      ! initialize the half saturation constant for silicate
      ! ----------------------------------------------------
      DO jj = JRANGE
         DO ji = IRANGE
            xksi(:,:)    = 2.e-6
            xksimax(:,:) = xksi(:,:)
         ENDDO
      ENDDO

      ! Initialization of chemical variables of the carbon cycle
      ! --------------------------------------------------------
      DO jk = KRANGE
         DO jj = JRANGE
            DO ji = IRANGE
               zdic    = trn(ji,jj,K,jpdic) * 1e-6 
               zalk    = trn(ji,jj,K,jptal) * 1e-6 
               ztmas   = tmask(ji,jj,K)
               ztmas1  = 1. - tmask(ji,jj,K)
               zcaralk = zalk - borat(ji,jj,jk) / (  1. + 1.E-8 / ( rtrn + akb3(ji,jj,jk) )  )
               zco3    = ( zcaralk - zdic ) * ztmas + 0.5e-3 * ztmas1
               zbicarb = ( 2. * zdic - zcaralk )
               hi(ji,jj,jk) = ( ak23(ji,jj,jk) * zbicarb / zco3 ) * ztmas + 1.e-9 * ztmas1
!               hi(ji,jj,jk) = 1.e-8
            END DO
         END DO
      END DO

      !  
      IF(lwp) THEN               ! control print
         WRITE(numout,*)
         WRITE(numout,*)
         WRITE(numout,*) '          *** Total number of passive tracer jptra = ', jptra
      ENDIF

      CALL tracer_stat( nit000 )

      IF(lwp) WRITE(numout,*) 'Initialization of PISCES tracers done'
      IF(lwp) WRITE(numout,*) ' '

      !
   END SUBROUTINE trc_ini_pisces

   SUBROUTINE trc_nam_pisces             ! Empty routine

      CALL p4z_opt_init       !  Optic: PAR in the water column
      CALL p4z_lim_init       !  co-limitations by the various nutrients
      CALL p4z_prod_init      !  phytoplankton growth rate over the global ocean.
      CALL p4z_rem_init       !  remineralisation
      CALL p4z_mort_init      !  phytoplankton mortality 
      CALL p4z_micro_init     !  microzooplankton
      CALL p4z_meso_init      !  mesozooplankton
      CALL p4z_lys_init       !  calcite saturation
      CALL p4z_flx_init       !  gas exchange 
      CALL p4z_sed_init       !  sedimentation 


   END SUBROUTINE trc_nam_pisces

#else
   !!----------------------------------------------------------------------
   !!   Dummy module                            No PISCES biochemical model
   !!----------------------------------------------------------------------
CONTAINS
   SUBROUTINE trc_ini_pisces             ! Empty routine
   END SUBROUTINE trc_ini_pisces
#endif

   !!======================================================================
END MODULE trcini_pisces
