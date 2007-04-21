!
! $Id: set_diags_avg.F,v 1.4 2005/10/10 13:40:18 pmarches Exp $
!
#include "cppdefs.h"
#if (defined DIAGNOSTICS_TS && defined AVERAGES)

      subroutine set_diags_avg(tile)

!      USE param

      implicit none
      integer tile
# include "param.h"
# include "compute_tile_bounds.h"
      call set_diags_avg_tile(Istr,Iend,Jstr,Jend)
      return
      end

      subroutine set_diags_avg_tile(Istr,Iend,Jstr,Jend)
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
! calculation of averaged fluxes will only be performed if ntsdia_avg
! is a positive number
!
      if (ilc.gt.ntsdia_avg) then
        if (mod(ilc-1,nwrtdia_avg).eq.1) then

          if (ZEROTH_TILE) timedia_avg=time

          do iflux=1,NT
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  TXadv_avg(i,j,k,iflux)=TXadv(i,j,k,iflux)  
                enddo
              enddo
            enddo
          enddo
          do iflux=1,NT
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  TYadv_avg(i,j,k,iflux)=TYadv(i,j,k,iflux)
                enddo
              enddo
            enddo
          enddo
          do iflux=1,NT
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  TVadv_avg(i,j,k,iflux)=TVadv(i,j,k,iflux)
                enddo
              enddo
            enddo
          enddo
          do iflux=1,NT
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  THmix_avg(i,j,k,iflux)=THmix(i,j,k,iflux)
                enddo
              enddo
            enddo
          enddo
          do iflux=1,NT
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  TVmix_avg(i,j,k,iflux)=TVmix(i,j,k,iflux)
                end do
              end do
            end do
          end do
          do iflux=1,NT
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  Tbody_avg(i,j,k,iflux)=Tbody(i,j,k,iflux)
                enddo
              enddo
            enddo
          enddo
          do iflux=1,NT
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  Trate_avg(i,j,k,iflux)=Trate(i,j,k,iflux)
                enddo
              enddo
            enddo
          enddo

        elseif (mod(ilc-1,nwrtdia_avg).gt.1) then

          if (ZEROTH_TILE) timedia_avg=timedia_avg+time

          do iflux=1,NT
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  TXadv_avg(i,j,k,iflux) = 
     &                       TXadv_avg(i,j,k,iflux) +
     &                       TXadv(i,j,k,iflux)
                enddo
              enddo
            enddo
          enddo
          do iflux=1,NT
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  TYadv_avg(i,j,k,iflux) = 
     &                       TYadv_avg(i,j,k,iflux) +
     &                       TYadv(i,j,k,iflux)
                enddo
              enddo
            enddo
          enddo
          do iflux=1,NT
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  TVadv_avg(i,j,k,iflux) = 
     &                       TVadv_avg(i,j,k,iflux) +
     &                       TVadv(i,j,k,iflux)
                enddo
              enddo
            enddo
          enddo
          do iflux=1,NT
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  THmix_avg(i,j,k,iflux) =
     &                       THmix_avg(i,j,k,iflux) +
     &                       THmix(i,j,k,iflux)
                enddo
              enddo
            enddo
          enddo
          do iflux=1,NT
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  TVmix_avg(i,j,k,iflux) = 
     &                       TVmix_avg(i,j,k,iflux) +
     &                       TVmix(i,j,k,iflux)
                enddo
              enddo
            enddo
          enddo
          do iflux=1,NT
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  Tbody_avg(i,j,k,iflux) = 
     &                    Tbody_avg(i,j,k,iflux) + 
     &                    Tbody(i,j,k,iflux)  
                enddo
              enddo
            enddo
          enddo
          do iflux=1,NT
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  Trate_avg(i,j,k,iflux) = 
     &                    Trate_avg(i,j,k,iflux) + 
     &                    Trate(i,j,k,iflux)  
                enddo
              enddo
            enddo
          enddo

        elseif (mod(ilc-1,nwrtdia_avg).eq.0) then

          cff=1./float(nwrtdia_avg)
          if (ZEROTH_TILE) timedia_avg=timedia_avg+float(nwrtdia_avg)*dt

          do iflux=1,NT
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  TXadv_avg(i,j,k,iflux) = cff *
     &                       ( TXadv_avg(i,j,k,iflux) +
     &                         TXadv(i,j,k,iflux) )
                   enddo
                enddo
             enddo
          enddo
          do iflux=1,NT
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  TYadv_avg(i,j,k,iflux) = cff *
     &                       ( TYadv_avg(i,j,k,iflux) +
     &                         TYadv(i,j,k,iflux) )
                enddo
              enddo
            enddo
          enddo
          do iflux=1,NT
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  TVadv_avg(i,j,k,iflux) = cff *
     &                       ( TVadv_avg(i,j,k,iflux) +
     &                         TVadv(i,j,k,iflux) )
                enddo
              enddo
            enddo
          enddo
          do iflux=1,NT
            do k =1, N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  THmix_avg(i,j,k,iflux) = cff *
     &                       ( THmix_avg(i,j,k,iflux) +
     &                         THmix(i,j,k,iflux) )
                enddo
              enddo
            enddo
          enddo
          do iflux=1,NT
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  TVmix_avg(i,j,k,iflux) = cff *
     &                       ( TVmix_avg(i,j,k,iflux) +
     &                         TVmix(i,j,k,iflux) )
                enddo
              enddo
            enddo
          enddo
          do iflux=1,NT
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  Tbody_avg(i,j,k,iflux) = cff *
     &                    ( Tbody_avg(i,j,k,iflux) + 
     &                      Tbody(i,j,k,iflux) )
                enddo
              enddo
            enddo
          enddo 
          do iflux=1,NT
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  Trate_avg(i,j,k,iflux) = cff *
     &                    ( Trate_avg(i,j,k,iflux) + 
     &                      Trate(i,j,k,iflux) )
                enddo
              enddo
            enddo
          enddo
        endif
      endif
      return
      end
#else 
      subroutine set_diags_avg_empty
      end
#endif /*DIAGNOSTICS_TS && defined AVERAGES*/
