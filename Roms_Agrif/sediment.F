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
#ifdef SEDIMENT
      subroutine sediment (tile)
!
!===================================== Blaas / Shchepetkin / Warner ==
!  Copyright (c) 2004 Rutgers/UCLA                                   !
!================================================ Hernan G. Arango ===
!                                                                    !
!  This  routine computes the sediment sources and sinks and adds    !
!  then the global sediment tracer fields. Currently, it computes    !
!  the following terms:                                              !
!                                                                    !
!  * Vertical settling of sediment in the water column.              !
!  * Correction of near-bed concentration acc. to Rouse profile      !
!  * Armoring of bed surface depending on grain-size distribution    !
!  * Erosive and depositional flux interactions of sediment          !
!    between water column and the bed.                               !
!                                                                    !
!=====================================================================
!
      implicit none
#include "param.h"
#include "private_scratch.h"
      integer tile, trd, omp_get_thread_num
# include "compute_tile_bounds.h"
!
      trd=omp_get_thread_num()
      call sediment_tile (Istr,Iend,Jstr,Jend, 
     &                               A2d(1,1,trd), A2d(1,2,trd),
     &                               A2d(1,3,trd), A2d(1,4,trd),
     &                               A2d(1,5,trd), A2d(1,6,trd),
     &                               A2d(1,7,trd), A2d(1,8,trd),
     &                               A2d(1,9,trd), A2d(1,10,trd),
     &                               A2d(1,11,trd),A2d(1,12,trd),
     &                               A2d(1,13,trd),A2d(1,14,trd),
     &                               A2d(1,15,trd),B2d(1,trd))
      return
      end
!***********************************************************************
      subroutine sediment_tile (
     & Istr, Iend, Jstr, Jend, FC, qc, qR, qL, WR, WL,
     & Hz_inv, Hz_inv2, Hz_inv3, inthk, thpos,
     & thneg, deltot, hidexp, znotc, ksource)
!***********************************************************************
      implicit none
#include "param.h"
#include "bbl.h"
#include "forces.h"
#include "grid.h"
#include "ocean3d.h"
#include "scalars.h"
#include "sediment.h"
      integer Istr,Iend,Jstr,Jend, ised,indx, i,j,k,ks

      real FC(PRIVATE_1D_SCRATCH_ARRAY,0:N),   cff,
     &       qc(PRIVATE_1D_SCRATCH_ARRAY,N),   labcf,  
     &       qR(PRIVATE_1D_SCRATCH_ARRAY,N),   cff1, cff2,
     &       qL(PRIVATE_1D_SCRATCH_ARRAY,N),   Cb, cu, 
     &       WR(PRIVATE_1D_SCRATCH_ARRAY,N),   cffR, dltR,
     &       WL(PRIVATE_1D_SCRATCH_ARRAY,N),   cffL, dltL,
     &   Hz_inv(PRIVATE_1D_SCRATCH_ARRAY,N),   tcs_inv,
     &  Hz_inv2(PRIVATE_1D_SCRATCH_ARRAY,N),   tau_water,
     &  Hz_inv3(PRIVATE_1D_SCRATCH_ARRAY,N),   dep_flux,
     &    inthk(PRIVATE_1D_SCRATCH_ARRAY,NST), ero_flux, eps,
     &    thpos(PRIVATE_1D_SCRATCH_ARRAY),     masstot,
     &    thneg(PRIVATE_1D_SCRATCH_ARRAY),     thick,
     &   deltot(PRIVATE_1D_SCRATCH_ARRAY),     invlog2,
     &   hidexp(PRIVATE_1D_SCRATCH_ARRAY),     phimn,
     &   znotc(PRIVATE_1D_SCRATCH_ARRAY),   
     &      phi(NST), Zr 
#ifdef ROUSE
     &     ,Rouse
#endif         
      integer ksource(PRIVATE_1D_SCRATCH_ARRAY,N)

      parameter (labcf=0.29, eps=1.D-20)

      invlog2=1./LOG(2.)
      
      do j=Jstr,Jend                        ! Auxiliary step: save   
        do k=1,N                            ! inverses of grid box 
          do i=Istr,Iend                    ! heights to avoid
            Hz_inv(i,k)=1./Hz(i,j,k)        ! repeated divisions
          enddo
        enddo

        do i=Istr,Iend 
          phimn=0.
          cff=0.
          deltot(i)=0.
          thpos(i)=0.
          thneg(i)=0.

          do ised=1,NST
             phi(ised)=-LOG(Sd(ised))*invlog2
             phimn=phimn+phi(ised)*bed_frac(i,j,1,ised)
          enddo
          znotc(i)=(2.**(-phimn))/12.0

#ifdef BED_ARMOR          
          do ised=1,NST
             cff=cff+((phi(ised)-phimn)**2)*bed_frac(i,j,1,ised)
          enddo
     
          cff=SQRT(cff)
          
          hidexp(i)=(2.**(phimn))*((1.-labcf*cff)**5.)
!         hidexp = (d_i/d_50)*labE^5 !multipl. by d_i in ero_flux
!          with d_50 = 2^(-phimn)
!               labE = 1-labcf*cff
!                cff = sigma_phi
#endif
        enddo ! --> i (retain deltot, thpos|neg, hidexp)
        
        do k=1,N-1
          do i=Istr,Iend
            Hz_inv2(i,k)=1./(Hz(i,j,k)+Hz(i,j,k+1))
          enddo
        enddo
        do k=2,N-1
          do i=Istr,Iend
            Hz_inv3(i,k)=1./(Hz(i,j,k-1)+Hz(i,j,k)+Hz(i,j,k+1))
          enddo
        enddo
!
! Vertical sinking of suspended particles:   Copy concentration of
!========= ======= == ========= ==========   suspended sediment into
! scratch array "qc" (q-central, restrict it to be positive) which
! are hereafter interpreted as a set of grid-box averaged values for
! concentration. Then reconstruct vertical profile of "qc" in terms
! of a set of parabolic segments within each grid box; and, finally,
! compute semi-Lagrangian flux due to sinking.
!
        do ised=1,NST
          indx=itrc_sed+ised-1
          do k=1,N
            do i=Istr,Iend
              qc(i,k)=t(i,j,k,nnew,indx)
            enddo
          enddo

          do k=N-1,1,-1
            do i=Istr,Iend
              FC(i,k)=(qc(i,k+1)-qc(i,k))*Hz_inv2(i,k)
            enddo
          enddo
          do k=2,N-1
            do i=Istr,Iend
              dltR=Hz(i,j,k)*FC(i,k)
              dltL=Hz(i,j,k)*FC(i,k-1)
              cff=Hz(i,j,k-1)+2.*Hz(i,j,k)+Hz(i,j,k+1)
              cffR=cff*FC(i,k)
              cffL=cff*FC(i,k-1)            ! Apply PPM monotonicity
                                            ! constraint to prevent
              if (dltR*dltL .le. 0.) then   ! oscillation within the
                dltR=0.                     ! grid box
                dltL=0.
              elseif (abs(dltR) .gt. abs(cffL)) then
                dltR=cffL
              elseif (abs(dltL) .gt. abs(cffR)) then
                dltL=cffR
              endif                         ! Compute right and left
                                            ! side values qR,qL of 
              cff=(dltR-dltL)*Hz_inv3(i,k)  ! parabolic segments
              dltR=dltR-cff*Hz(i,j,k+1)     ! within grid box Hz(k)
              dltL=dltL+cff*Hz(i,j,k-1)     ! (WR,WL are measures of 
              qR(i,k)=qc(i,k)+dltR          ! quadratic variations).
              qL(i,k)=qc(i,k)-dltL
              WR(i,k)=( 2.*dltR-dltL )**2   ! NOTE: Although each 
              WL(i,k)=( dltR-2.*dltL )**2   ! parabolic segment is
            enddo                           ! monotone within its
          enddo          !--> discard FC    ! grid box, monotonicity 
                                            ! of the whole profile is
          cff=1.0E-14                       ! not guaranteed, because 
          do k=2,N-2                        ! qL(k+1)-qR(k) may still 
            do i=Istr,Iend                  ! have different sign 
              dltL=max(WL(i,k),   cff)      ! than qc(k+1)-qc(k)...
              dltR=max(WR(i,k+1), cff)
              qR(i,k)=(dltR*qR(i,k)+dltL*qL(i,k+1))/(dltR+dltL)
              qL(i,k+1)=qR(i,k)
            enddo                           !   ...this possibility
          enddo      !--> discard WR,WL     ! is excluded, after qL
                                            ! and qR are reconciled
          do i=Istr,Iend                    ! using WENO procedure.
            FC(i,N)=0.     !<-- no-flux BC
# if defined LINEAR_CONTINUATION
            qL(i,N)=qR(i,N-1)
            qR(i,N)=2.*qc(i,N)-qL(i,N)
# elif defined NEUMANN
            qL(i,N)=qR(i,N-1)
            qR(i,N)=1.5*qc(i,N)-0.5*qL(i,N)
# else
            qR(i,N)=qc(i,N)                 ! Strictly monotone
            qL(i,N)=qc(i,N)                 ! version as the default:
            qR(i,N-1)=qc(i,N)               ! distributions at top...
# endif
# if defined LINEAR_CONTINUATION 
            qR(i,1)=qL(i,2)
            qL(i,1)=2.*qc(i,1)-qR(i,1)
# elif defined NEUMANN
            qR(i,1)=qL(i,2)
            qL(i,1)=1.5*qc(i,1)-0.5*qR(i,1)
# elif defined ROUSE   
            qR(i,1)=qL(i,2)
            qL(i,1)=qc(i,1)*(z_r(i,j,1)/znotc(i))**Rouse 
! TEMP remarks
!   Assume qc-profile~Rouse profile C(z)=C(z_r(1))*(z/z_r(1))^-Rouse
!   Rouse = Wsed(ised)/(vonKar*ustar), 
!   ustar=max wc stress or mean (Grant&Madsen)
!   qL('bottom')=C(znotc)
!   znotc=d50/12 (Nikuradse length) 
!   No distinction yet whether w or w/o wave boundary layer 
! TEMP remarks
# else  
            qL(i,2)=qc(i,1)                 !     ...and bottom grid
            qR(i,1)=qc(i,1)                 ! boxes re assumed to be
            qL(i,1)=qc(i,1)                 ! piecewise constant.
# endif
          enddo

          do k=1,N                          ! Since the reconciled 
            do i=Istr,Iend                  ! interfacial values may
              dltR=qR(i,k)-qc(i,k)          ! cause non-monotonic
              dltL=qc(i,k)-qL(i,k)          ! behavior of parabolic
              cffR=2.*dltR                  ! segments inside grid
              cffL=2.*dltL                  ! box apply monotonicity
                                            ! constraint again.
              if (dltR*dltL .lt. 0.) then
                dltR=0.
                dltL=0.
              elseif (abs(dltR) .gt. abs(cffL)) then
                dltR=cffL
              elseif (abs(dltL) .gt. abs(cffR)) then
                dltL=cffR
              endif

              qR(i,k)=qc(i,k)+dltR
              qL(i,k)=qc(i,k)-dltL
            enddo
          enddo     !--> discard everything, except qR,qL
!
! After this moment reconstruction is considered complete. The next
! stage is to compute vertical advective fluxes FC. It is expected
! that sinking may occurs relatively fast, the algorithm is designed
! to be free of CFL criterion, which is achieved by allowing
! integration bounds for semi-Lagrangian advective flux to use as
! many grid boxes in upstream direction as necessary.
!
          cff=dt*abs(Wsed(ised))          ! In the two code segments
          do k=1,N                        ! WL is z-coordinate of the
            do i=Istr,Iend                ! departure point for grid
              FC(i,k-1)=0.                ! box interface z_w with
              WL(i,k)=z_w(i,j,k-1)+cff    ! the same indices;
              WR(i,k)=Hz(i,j,k)*qc(i,k)   ! FC is finite volume flux;
              ksource(i,k)=k              ! ksource(:,k) is index of
            enddo                         ! vertical grid box which
          enddo                           ! contains the departure
          do k=1,N                        ! point (restricted by N);
            do ks=k,N-1                   ! During the search: also
              do i=Istr,Iend
                if (WL(i,k) .gt. z_w(i,j,ks)) then
                  ksource(i,k)=ks+1
                  FC(i,k-1)=FC(i,k-1)+WR(i,ks)
                endif
              enddo                       ! add in content of whole
            enddo                         ! grid boxes participating
          enddo       !--> discard WR     ! in FC.

          do k=1,N                        ! Finalize computation of
            do i=Istr,Iend                ! flux: add fractional part
              ks=ksource(i,k)
              cu=min(1.,(WL(i,k)-z_w(i,j,ks-1))*Hz_inv(i,ks))
              FC(i,k-1)=FC(i,k-1) + Hz(i,j,ks)*cu*( qL(i,ks)
     &                             +cu*( 0.5*(qR(i,ks)-qL(i,ks))
     &             -(1.5-cu)*(qR(i,ks)+qL(i,ks)-2.*qc(i,ks))  ))
            enddo
          enddo
          do k=1,N,+1
            do i=Istr,Iend
              qc(i,k)=qc(i,k) + (FC(i,k)-FC(i,k-1))*Hz_inv(i,k)
            enddo
          enddo    !--> discard everything, except qc,FC(:,0)
!
! Deposition and resuspension near the bottom:  Update thickness of
!============ === =========== ==== === =======  top sediment layer on
! the bottom "bed" (meters), which changes due to precepitation flux
! FC(:,0) which is already computed and resuspension (erosion, hence
! called ero_flux), which is also applied to the bottom-most grid box
! value qc(:,1) (hence total mass is conserved; also restrict
! "ero_flux" so that thickness >= 0 after both fluxes are applied).
!
         tcs_inv= 1./tau_ce(ised) 

          do i=Istr,Iend
             Cb = 1-bed_poros(i,j,1)        ! Vsed_tot/(Vsed_tot+Vwater) 
                                            ! active layer
             cff     = dt*Erate(ised)*Cb
     &                   *bed_frac(i,j,1,ised)
#ifdef BED_ARMOR
     &                   *hidexp(i)*Sd(ised)
#endif

             masstot=Cb*Srho(ised)*(bed_frac(i,j,1,ised)*bed_thick(i,j,1)
     &              + bed_frac(i,j,2,ised)*bed_thick(i,j,2))

# ifdef BBL
            tau_water=0.5*sqrt( (bustrw(i,j)+bustrw(i+1,j))**2
     &                         +(bvstrw(i,j)+bvstrw(i,j+1))**2)
# else
            tau_water=0.5*sqrt( (bustr(i,j)+bustr(i+1,j))**2
     &                         +(bvstr(i,j)+bvstr(i,j+1))**2)

! Determine actual skin friction on sediments
     	    Zr=z_r(i,j,1)-z_w(i,j,0)
     	    if (Zob.ne.0.) then
     	      cff1 = vonKar/LOG(Zr/Zob) 
              cff2 = MIN(Cdb_max,MAX(Cdb_min,cff1*cff1))
              tau_water=tau_water/cff2   ! Umag^2 corresponding to bstr(u,v)
            elseif (rdrg2.gt.0) then
              tau_water=tau_water/rdrg2  
            else
              tau_water=tau_water*tau_water/(rdrg*rdrg) 
            endif
       	    cff1 = vonKar/LOG(Zr/znotc(i))
            tau_water=tau_water*cff1*cff1
# endif
            dep_flux=0.001*FC(i,0)
            ero_flux=min( max(0., cff*(tcs_inv*tau_water-1.)),
     &                    masstot+dep_flux)

            cff=(dep_flux-ero_flux)/(Srho(ised)*Cb)  ! net change of thick/class
            deltot(i)=deltot(i)+cff
            inthk(i,ised)=cff+bed_thick(i,j,1)*bed_frac(i,j,1,ised)
            thpos(i)=thpos(i)+max(inthk(i,ised),0.) 
            thneg(i)=thneg(i)+min(inthk(i,ised),0.)
            qc(i,1)=qc(i,1) + 1000.*ero_flux*Hz_inv(i,1)
          enddo
                                 ! Write concentration of suspended
          do k=1,N               ! sediments back to global arrays:
            do i=Istr,Iend
                t(i,j,k,nnew,indx)=qc(i,k)
# ifdef MASKING
     &                                   *rmask(i,j)
# endif
            enddo
          enddo
        enddo    ! <-- ised

! update bed fraction and thickness per size class

      do i=Istr,Iend
      
      if (deltot(i).ge.0.) then !net deposition
        cff=min(Bthk(1),bed_thick(i,j,1)+bed_thick(i,j,2)+deltot(i)) 
        thick=max(bed_thick(i,j,1)+bed_thick(i,j,2)+deltot(i)-cff,0.)

         do ised=1,NST
            bed_frac(i,j,1,ised)=(max(inthk(i,ised),0.)
     &                           /max(thpos(i),max(cff,eps)))
#           ifdef MASKING
     &                           *rmask(i,j)
#           endif

            bed_frac(i,j,2,ised)=((bed_frac(i,j,1,ised)
     &                         *(deltot(i)-(cff-bed_thick(i,j,1)))
     &                         *REAL(INT(cff/Bthk(1)))
     &                         +bed_frac(i,j,2,ised)
     &                         *max(bed_thick(i,j,2),0.))
     &                          /max(thick,eps))
#         ifdef MASKING
     &                          *rmask(i,j)
#         endif
        enddo ! -> ised
      else        ! net erosion
         thick=max(bed_thick(i,j,2)+deltot(i),0.)
         cff=max(bed_thick(i,j,1)+bed_thick(i,j,2)
     &           +deltot(i)-thick,0.)                ! delt_a_new

C        if (thick.le.0.) then
C           cff=max(bed_thick(i,j,1)+bed_thick(i,j,2)+deltot(i),0.)
C        endif

         do ised=1,NST

            bed_frac(i,j,2,ised)=((bed_frac(i,j,2,ised)*
     &             bed_thick(i,j,2)+min(inthk(i,ised),0.))
     &                   /max(bed_thick(i,j,2)+thneg(i),eps))
#           ifdef MASKING
     &                          *rmask(i,j)
#           endif 

            bed_frac(i,j,1,ised)=((max(inthk(i,ised),0.)
     &          +bed_frac(i,j,2,ised)*(cff-thpos(i)))
     &                           /max(cff,eps))
#           ifdef MASKING
     &                           *rmask(i,j)
#           endif
          enddo
      endif
      
      bed_thick(i,j,1)=cff
      bed_thick(i,j,2)=max(thick,0.)
#     ifdef MASKING
     &                                     *rmask(i,j)
#     endif
        enddo  ! <-- i
      enddo    ! <-- j
      return
      end
#else
      subroutine sediment_empty
      end
#endif
!
#ifdef SEDIMENT
       subroutine init_sediment
!====================================================================
!                                                                    !
!  This routine reads in and processes sediment parameters and       !
!  some initial data from sediments.in file.                         !
!=====================================================================
!
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "ncscrum.h"
# include "scalars.h"
# include "sediment.h"
!
      integer Nclass, i, icard, iunit, lstr, nl
      integer lenstr
      parameter (iunit=50)
!
!---------------------------------------------------------------------
!  Read in initial float locations.
!---------------------------------------------------------------------
!
      lstr=lenstr(sedname)

      open(iunit,file=sedname(1:lstr),form='formatted',
     &  status='old', err=195)
!
!  Read input parameters according to their input card number.
!
      icard=0
      do while (icard.lt.99)
!
!  Read in sediment case title.
!
        if (icard.eq.1) then
          read(iunit,'(a)',err=60) Stitle
          lstr=lenstr(Stitle)
          write(stdout,10) Stitle(1:lstr)
  10      format(1x,'(',a,')',/)
!
!  Read in parameteramd initial values per size class.
!
        elseif (icard.eq.2) then
          i=0
          Nclass=0
          do i=1,NST
           read(iunit,*,err=40) Sd(i), Csed(i), Srho(i), Wsed(i),
     &                     Erate(i), tau_ce(i), tau_cd(i), 
     &                     Bfr(1,i),Bfr(2,i)
     
           MPI_master_only write(stdout,'(/A,2x,i2)')
     &     'Sediment parameters for grain-size class:', i
     
           MPI_master_only write(stdout,'(7(/f10.5,2x,A))')
     &     Sd(i),    'Sd      Median sediment grain diameter (mm).',
     &     Csed(i),  'Csed    Analyt. init. of sed. conc. (mg/l).',
     &     Srho(i),  'Srho    Sediment grain density (kg/m3).',
     &     Wsed(i),  'Wsed    Particle settling velocity (mm/s).',
     &     Erate(i), 'Erate   Surface erosion rate (kg/(m2s)).',
     &     tau_ce(i),'tau_ce  Critical shear for erosion (N/m2).',
     &     tau_cd(i),'tau_cd  Critical shear for deposition (N/m2).'
          
           do nl=1,NLAY
            MPI_master_only write(stdout,'(f10.5,2x,A,2x,i2)')
     &      Bfr(nl,i), 'bed_frac   Volume fraction, layer:',nl
           enddo
 
           Sd(i)=Sd(i)/1000.         ! mm --> m
           Wsed(i)=Wsed(i)/1000.     ! mm/s --> m/s
           tau_ce(i)=tau_ce(i)/rho0  ! N/m2 --> m2/s2
           tau_cd(i)=tau_cd(i)/rho0  ! N/m2 --> m2/s2
           Nclass=Nclass+1
          enddo

        elseif (icard.eq.3) then
           MPI_master_only write(stdout,'(/A)')
     &     'Bed parameters for all bed levels'

          read(iunit,*,err=60) (Bthk(nl),nl=1,NLAY)
           do nl=1,NLAY
            MPI_master_only write(stdout,'(f10.5,2x,A,2x,i2,2x,A)')
     &      Bthk(nl), 'Bthk   Init. bed thickness, layer:',nl,'(m).'
           enddo

        elseif (icard.eq.4) then

          read(iunit,*,err=60) (Bpor(nl),nl=1,NLAY)
           do nl=1,NLAY
            MPI_master_only write(stdout,'(f10.5,2x,A,2x,i2,2x,A)')
     &      Bpor(nl), 'Bpor   Init. bed porosity, layer:',nl,'(m).'
           enddo

        elseif (icard.eq.5) then
          read(iunit,*,err=60) Hrip                     
            MPI_master_only write(stdout,'(f10.5,2x,A)')
     &      Hrip, 'Hrip   Init. ripple roughness (m).'
        
        elseif (icard.eq.6) then
          read(iunit,*,err=60) Lrip                     
            MPI_master_only write(stdout,'(f10.5,2x,A)')
     &      Lrip, 'Lrip   Init. riple height (m).'

        endif ! icard 
!
!  Read until last input card ID.
!
        read(iunit,*,err=60) icard
      enddo
      goto 90
!
!  Error while reading input parameters.
!
  40  write(stdout,50) icard, i, Nclass, sedname
  50  format(/,' INIT_SEDIMENTS - error reading variables in card: ',
     &             i2, ', entry: ',i3,/,15x,
     &             'nclass:',i3, 'input script: ',a)
  
  60  write(stdout,80) icard, sedname
  80  format(/,' INIT_SEDIMENTS - error while reading input card: ',
     &       i2,15x,'in input script: ',a)

 195  write(stdout,205) sedname
 205  format(/,'sediment inputfile ',A,/, 
     &   ' not found sediment initialization', ' from restart file')

  90  close(iunit)
      write(stdout,100) Nclass
 100  format(/,
     &       'Number of size classes in sediment computation:'i2)

      return
      end
#else       
      subroutine init_sediment_empty
      end
#endif
