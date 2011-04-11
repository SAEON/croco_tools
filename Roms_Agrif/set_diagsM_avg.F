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
#if (defined DIAGNOSTICS_UV && defined AVERAGES)

      subroutine set_diagsM_avg(tile)

!      USE param

      implicit none
      integer tile
# include "param.h"
# include "compute_tile_bounds.h"
      call set_diagsM_avg_tile(Istr,Iend,Jstr,Jend)
      return
      end

      subroutine set_diagsM_avg_tile(Istr,Iend,Jstr,Jend)
!
! Compute time-averaged fields within a tile.
! ------- ------------- ------ ------ - -----
! Because of syncronization issues, the delayed mode averaging
! procedure is used. This procedure implies that all fields to be
! averaged are sampled during the next time step, rather than at
! the end of the time step when they were computed.
!
! Thought this algorithm results in somewhat ackwad controlling
! logic it has the advantage that that all fields to be sampled
! correspond to exactly the same time, which is time step "n".
! Particularly, this is done this way because vertical velocity
! corresponding to the newly computed horizontal velocities
! becomes available only during the following time step.
! The same applies to the density field.
!
! The algorithm consists of three logical blocks: (1) initialization
! of the averages arrays: when mod(ilc-1,navg).eq.1 the target arrays
! are set to the first contribution; (2) accumulation of averaged
! data, when mod(ilc-1,navg).gt.1; and (3) adding the last
! contribution and scaling.
!
!      USE scalars
!      USE grid
!      USE averages
!      USE ocean2d
!      USE physflux
!      USE ncscrum

      implicit none
      integer Istr,Iend,Jstr,Jend, i,j, iflux, k, ilc
      real cff
# include "param.h"
# include "scalars.h"
# include "ncscrum.h"
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "averages.h"
# include "diagnostics.h"
!
# include "compute_auxiliary_bounds.h"
!
      ilc=1+iic-ntstart   ! number of time step since restart
!
! calculation of averaged fluxes will only be performed if ntsdiaM_avg
! is a positive number
!
      if (ilc.gt.ntsdiaM_avg) then
        if (mod(ilc-1,nwrtdiaM_avg).eq.1) then
          do iflux=1,2
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  MXadv_avg(i,j,k,iflux)=MXadv(i,j,k,iflux)  
                enddo
              enddo
            enddo
          enddo
          do iflux=1,2
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  MYadv_avg(i,j,k,iflux)=MYadv(i,j,k,iflux)
                enddo
              enddo
            enddo
          enddo
          do iflux=1,2
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  MVadv_avg(i,j,k,iflux)=MVadv(i,j,k,iflux)
                enddo
              enddo
            enddo
          enddo
          do iflux=1,2
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  MCor_avg(i,j,k,iflux)=MCor(i,j,k,iflux)
                enddo
              enddo
            enddo
          enddo
          do iflux=1,2
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  MPrsgrd_avg(i,j,k,iflux)=MPrsgrd(i,j,k,iflux)
                enddo
              enddo
            enddo
          enddo
          do iflux=1,2
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  MHmix_avg(i,j,k,iflux)=MHmix(i,j,k,iflux)
                enddo
              enddo
            enddo
          enddo
          do iflux=1,2
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  MVmix_avg(i,j,k,iflux)=MVmix(i,j,k,iflux)
                end do
              end do
            end do
          end do
          do iflux=1,2
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  Mrate_avg(i,j,k,iflux)=Mrate(i,j,k,iflux)
                enddo
              enddo
            enddo
          enddo

        elseif (mod(ilc-1,nwrtdiaM_avg).gt.1) then
          do iflux=1,2
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  MXadv_avg(i,j,k,iflux) = 
     &                       MXadv_avg(i,j,k,iflux) +
     &                       MXadv(i,j,k,iflux)
                enddo
              enddo
            enddo
          enddo
          do iflux=1,2
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  MYadv_avg(i,j,k,iflux) = 
     &                       MYadv_avg(i,j,k,iflux) +
     &                       MYadv(i,j,k,iflux)
                enddo
              enddo
            enddo
          enddo
          do iflux=1,2
            do k =1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  MVadv_avg(i,j,k,iflux) = 
     &                       MVadv_avg(i,j,k,iflux) +
     &                       MVadv(i,j,k,iflux)
                enddo
              enddo
            enddo
          enddo
          do iflux=1,2
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  MCor_avg(i,j,k,iflux) =
     &                       MCor_avg(i,j,k,iflux) +
     &                       MCor(i,j,k,iflux)
                enddo
              enddo
            enddo
          enddo
          do iflux=1,2
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  MPrsgrd_avg(i,j,k,iflux) =
     &                       MPrsgrd_avg(i,j,k,iflux) +
     &                       MPrsgrd(i,j,k,iflux)
                enddo
              enddo
            enddo
          enddo
          do iflux=1,2
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  MHmix_avg(i,j,k,iflux) =
     &                       MHmix_avg(i,j,k,iflux) +
     &                       MHmix(i,j,k,iflux)
                enddo
              enddo
            enddo
          enddo
          do iflux=1,2
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  MVmix_avg(i,j,k,iflux) = 
     &                       MVmix_avg(i,j,k,iflux) +
     &                       MVmix(i,j,k,iflux)
                enddo
              enddo
            enddo
          enddo
          do iflux=1,2
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  Mrate_avg(i,j,k,iflux) = 
     &                    Mrate_avg(i,j,k,iflux) + 
     &                    Mrate(i,j,k,iflux)  
                enddo
              enddo
            enddo
          enddo


        elseif (mod(ilc-1,nwrtdiaM_avg).eq.0) then

          cff=1./float(nwrtdiaM_avg)
          if (ZEROTH_TILE) timediaM_avg=timediaM_avg+
     &                                       float(nwrtdiaM_avg)*dt

          do iflux=1,2
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  MXadv_avg(i,j,k,iflux) = cff *
     &                       ( MXadv_avg(i,j,k,iflux) +
     &                         MXadv(i,j,k,iflux) )
                   enddo
                enddo
             enddo
          enddo
          do iflux=1,2
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  MYadv_avg(i,j,k,iflux) = cff *
     &                       ( MYadv_avg(i,j,k,iflux) +
     &                         MYadv(i,j,k,iflux) )
                enddo
              enddo
            enddo
          enddo
          do iflux=1,2
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  MVadv_avg(i,j,k,iflux) = cff *
     &                       ( MVadv_avg(i,j,k,iflux) +
     &                         MVadv(i,j,k,iflux) )
                enddo
              enddo
            enddo
          enddo
          do iflux=1,2
            do k =1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  MCor_avg(i,j,k,iflux) = cff *
     &                       ( MCor_avg(i,j,k,iflux) +
     &                         MCor(i,j,k,iflux) )
                enddo
              enddo
            enddo
          enddo
          do iflux=1,2
            do k =1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  MPrsgrd_avg(i,j,k,iflux) = cff *
     &                       ( MPrsgrd_avg(i,j,k,iflux) +
     &                         MPrsgrd(i,j,k,iflux) )
                enddo
              enddo
            enddo
          enddo
          do iflux=1,2
            do k =1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  MHmix_avg(i,j,k,iflux) = cff *
     &                       ( MHmix_avg(i,j,k,iflux) +
     &                         MHmix(i,j,k,iflux) )
                enddo
              enddo
            enddo
          enddo
          do iflux=1,2
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  MVmix_avg(i,j,k,iflux) = cff *
     &                       ( MVmix_avg(i,j,k,iflux) +
     &                         MVmix(i,j,k,iflux) )
                enddo
              enddo
            enddo
          enddo
          do iflux=1,2
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  Mrate_avg(i,j,k,iflux) = cff *
     &                    ( Mrate_avg(i,j,k,iflux) + 
     &                      Mrate(i,j,k,iflux) )
                enddo
              enddo
            enddo
          enddo
        endif
      endif
      return
      end
#else /* DIAGNOSTICS_UV && AVERAGES */
      subroutine set_diagsM_avg_empty
      end
#endif /* DIAGNOSTICS_UV && AVERAGES */
