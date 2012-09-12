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
#if defined DIAGNOSTICS_BIO && defined AVERAGES

      subroutine set_bio_diags_avg(tile)

!      USE param

      implicit none
      integer tile
# include "param.h"
# include "compute_tile_bounds.h"
      call set_bio_diags_avg_tile(Istr,Iend,Jstr,Jend)
      return
      end

      subroutine set_bio_diags_avg_tile(Istr,Iend,Jstr,Jend)
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
#include "compute_auxiliary_bounds.h"
!
      ilc=1+iic-ntstart   ! number of time step since restart
!
! calculation of averaged fluxes will only be performed if n_phys_flux_avg
! is a positive number
      if (ilc.gt.ntsdiabio_avg) then
         if (mod(ilc-1,nwrtdiabio_avg).eq.1) then

            if (ZEROTH_TILE) timediabio_avg=time

#ifdef PISCES
# ifdef key_trc_dia3d
            do iflux = 1, NumFluxTerms
              do k = 1, N
                do j = JstrR, JendR
                  do i = IstrR, IendR
                    bioFlux_avg(i,j,k,iflux)=bioFlux(i,j,k,iflux)
                  end do
                end do
              end do
            end do
# endif
# ifdef key_trc_diaadd
            do iflux = 1, NumVSinkTerms
               do j = JstrR, JendR
                  do i = IstrR, IendR
                     bioVSink_avg(i,j,iflux)=bioVSink(i,j,iflux)
                  end do
               end do
            end do
# endif
#else
            do iflux = 1, NumFluxTerms
              do k = 1, N
                do j = JstrR, JendR
                  do i = IstrR, IendR
                    bioFlux_avg(i,j,k,iflux)=bioFlux(i,j,k,iflux)
                  end do
                end do
              end do
            end do
            do iflux = 1, NumVSinkTerms
              do k = 0, N
                do j = JstrR, JendR
                  do i = IstrR, IendR
                    bioVSink_avg(i,j,k,iflux)=bioVSink(i,j,k,iflux)
                  end do
                end do
              end do
            end do
# if (defined BIO_NChlPZD && defined OXYGEN)
            do iflux = 1, NumGasExcTerms
              do j = JstrR, JendR
                do i = IstrR, IendR
                  GasExcFlux_avg(i,j,iflux) = GasExcFlux(i,j,iflux)
                end do
              end do
            end do
# endif /* BIO_NChlPZD && OXYGEN */
#endif

         elseif (mod(ilc-1,nwrtdiabio_avg).gt.1) then

            if (ZEROTH_TILE) timediabio_avg=timediabio_avg+time

#ifdef PISCES
# ifdef key_trc_dia3d
            do iflux = 1, NumFluxTerms
              do k = 1, N
                do j = JstrR, JendR
                  do i = IstrR, IendR
                    bioFlux_avg(i,j,k,iflux)=bioFlux_avg(i,j,k,iflux) +
     &                                       bioFlux(i,j,k,iflux)
                  end do
                end do
              end do
            end do
# endif
# ifdef key_trc_diaadd
            do iflux = 1, NumVSinkTerms
               do j = JstrR, JendR
                  do i = IstrR, IendR
                     bioVSink_avg(i,j,iflux)=bioVSink_avg(i,j,iflux) +
     &                                       bioVSink(i,j,iflux)
                  end do
               end do
            end do
# endif
#else
            do iflux = 1, NumFluxTerms
              do k = 1, N
                do j = JstrR, JendR
                  do i = IstrR, IendR
                    bioFlux_avg(i,j,k,iflux)=bioFlux_avg(i,j,k,iflux) +
     &                                       bioFlux(i,j,k,iflux)
                  end do
                end do
              end do
            end do
            do iflux = 1, NumVSinkTerms
              do k = 0, N
                do j = JstrR, JendR
                  do i = IstrR, IendR
                    bioVSink_avg(i,j,k,iflux)=bioVSink_avg(i,j,k,iflux)
     &                                       +bioVSink(i,j,k,iflux)
                  end do
                end do
              end do
            end do
# if (defined BIO_NChlPZD && defined OXYGEN)
            do iflux = 1, NumGasExcTerms
              do j = JstrR, JendR
                do i = IstrR, IendR
                  GasExcFlux_avg(i,j,iflux) =GasExcFlux_avg(i,j,iflux) +
     &                                       GasExcFlux(i,j,iflux)
                end do
              end do
            end do
# endif /* BIO_NChlPZD && OXYGEN */
#endif
         elseif (mod(ilc-1,nwrtdiabio_avg).eq.0) then

            cff=1./float(nwrtdiabio_avg)
            if (ZEROTH_TILE) timediabio_avg=timediabio_avg+
     &                                      float(nwrtdiabio_avg)*dt
#ifdef PISCES
# ifdef key_trc_dia3d
            do iflux = 1, NumFluxTerms
              do k = 1, N
                do j = JstrR, JendR
                  do i = IstrR, IendR
                    bioFlux_avg(i,j,k,iflux)=cff*
     &                    ( bioFlux_avg(i,j,k,iflux) +
     &                      bioFlux(i,j,k,iflux) )
                  end do
                end do
              end do
            end do
# endif
# ifdef key_trc_diaadd
            do iflux = 1, NumVSinkTerms
               do j = JstrR, JendR
                  do i = IstrR, IendR
                     bioVSink_avg(i,j,iflux)=cff*
     &                    ( bioVSink_avg(i,j,iflux) +
     &                      bioVSink(i,j,iflux) )
                     
                  end do
               end do
            end do
# endif
#else

            do iflux=1,NumFluxTerms
              do k=1,N
                do j=JstrR,JendR
                  do i = IstrR, IendR
                    bioFlux_avg(i,j,k,iflux)=cff*
     &                    ( bioFlux_avg(i,j,k,iflux) +
     &                      bioFlux(i,j,k,iflux) )
                  end do
                end do
              end do
            end do
            do iflux=1,NumVSinkTerms
              do k=0,N
                do j=JstrR,JendR
                  do i=IstrR, IendR
                    bioVSink_avg(i,j,k,iflux)=cff*
     &                     ( bioVSink_avg(i,j,k,iflux) +
     &                       bioVSink(i,j,k,iflux) )

                  end do
                end do
              end do
            end do
# if (defined BIO_NChlPZD && defined OXYGEN)
            do iflux = 1, NumGasExcTerms
              do j = JstrR, JendR
                do i = IstrR, IendR
                  GasExcFlux_avg(i,j,iflux) =cff*
     &                    ( GasExcFlux_avg(i,j,iflux) +
     &                     GasExcFlux(i,j,iflux) )
                end do
              end do
            end do
# endif /* BIO_NChlPZD && OXYGEN */
#endif
         endif
      endif

      return
      end 
#else /* DIAGNOSTICS_BIO && AVERAGES */
      subroutine set_bio_diags_avg_empty
      end
#endif /* DIAGNOSTICS_BIO && AVERAGES */
