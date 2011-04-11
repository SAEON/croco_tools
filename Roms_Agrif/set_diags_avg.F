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
      integer Istr,Iend,Jstr,Jend, i,j, itrc, k, ilc
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
          do itrc=1,NT
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  TXadv_avg(i,j,k,itrc)=TXadv(i,j,k,itrc)  
                  TYadv_avg(i,j,k,itrc)=TYadv(i,j,k,itrc)
                  TVadv_avg(i,j,k,itrc)=TVadv(i,j,k,itrc)
                  THmix_avg(i,j,k,itrc)=THmix(i,j,k,itrc)
                  TVmix_avg(i,j,k,itrc)=TVmix(i,j,k,itrc)
                  TForc_avg(i,j,k,itrc)=TForc(i,j,k,itrc)
                  Trate_avg(i,j,k,itrc)=Trate(i,j,k,itrc)
                enddo
              enddo
            enddo
          enddo

#ifdef DIAGNOSTICS_TS_MLD
!
! Diagnostics averaged over MLD
!
          do itrc=1,NT
            do j=JstrR,JendR
              do i=IstrR,IendR 
                TXadv_mld_avg(i,j,itrc)=TXadv_mld(i,j,itrc)  
                TYadv_mld_avg(i,j,itrc)=TYadv_mld(i,j,itrc)
                TVadv_mld_avg(i,j,itrc)=TVadv_mld(i,j,itrc)
                THmix_mld_avg(i,j,itrc)=THmix_mld(i,j,itrc)
                TVmix_mld_avg(i,j,itrc)=TVmix_mld(i,j,itrc)
                TForc_mld_avg(i,j,itrc)=TForc_mld(i,j,itrc)
                Trate_mld_avg(i,j,itrc)=Trate_mld(i,j,itrc)
                Tentr_mld_avg(i,j,itrc)=Tentr_mld(i,j,itrc)
              enddo
            enddo
          enddo
#endif /* DIAGNOSTICS_TS_MLD  */
        elseif (mod(ilc-1,nwrtdia_avg).gt.1) then
          do itrc=1,NT
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  TXadv_avg(i,j,k,itrc) = 
     &                       TXadv_avg(i,j,k,itrc) +
     &                       TXadv(i,j,k,itrc)
                  TYadv_avg(i,j,k,itrc) = 
     &                       TYadv_avg(i,j,k,itrc) +
     &                       TYadv(i,j,k,itrc)
                  TVadv_avg(i,j,k,itrc) = 
     &                       TVadv_avg(i,j,k,itrc) +
     &                       TVadv(i,j,k,itrc)
                  THmix_avg(i,j,k,itrc) =
     &                       THmix_avg(i,j,k,itrc) +
     &                       THmix(i,j,k,itrc)
                  TVmix_avg(i,j,k,itrc) = 
     &                       TVmix_avg(i,j,k,itrc) +
     &                       TVmix(i,j,k,itrc)
                  TForc_avg(i,j,k,itrc) = 
     &                    TForc_avg(i,j,k,itrc) + 
     &                    TForc(i,j,k,itrc)  
                  Trate_avg(i,j,k,itrc) = 
     &                    Trate_avg(i,j,k,itrc) + 
     &                    Trate(i,j,k,itrc)  
                enddo
              enddo
            enddo
          enddo


#ifdef DIAGNOSTICS_TS_MLD
!
! Diagnostics averaged over MLD
!
          do itrc=1,NT
            do j=JstrR,JendR
              do i=IstrR,IendR 
                TXadv_mld_avg(i,j,itrc)=TXadv_mld(i,j,itrc) +
     &                                  TXadv_mld_avg(i,j,itrc)
                TYadv_mld_avg(i,j,itrc)=TYadv_mld(i,j,itrc) +
     &                                  TYadv_mld_avg(i,j,itrc)
                TVadv_mld_avg(i,j,itrc)=TVadv_mld(i,j,itrc) +
     &                                  TVadv_mld_avg(i,j,itrc)
                THmix_mld_avg(i,j,itrc)=THmix_mld(i,j,itrc) +
     &                                  THmix_mld_avg(i,j,itrc)
                TVmix_mld_avg(i,j,itrc)=TVmix_mld(i,j,itrc) +
     &                                  TVmix_mld_avg(i,j,itrc)
                TForc_mld_avg(i,j,itrc)=TForc_mld(i,j,itrc) +
     &                                  TForc_mld_avg(i,j,itrc)
                Trate_mld_avg(i,j,itrc)=Trate_mld(i,j,itrc) +
     &                                  Trate_mld_avg(i,j,itrc)
                Tentr_mld_avg(i,j,itrc)=Tentr_mld(i,j,itrc) +
     &                                  Tentr_mld_avg(i,j,itrc)
              enddo
            enddo
          enddo
#endif /* DIAGNOSTICS_TS_MLD  */
        elseif (mod(ilc-1,nwrtdia_avg).eq.0) then

          cff=1./float(nwrtdia_avg)
          if (ZEROTH_TILE) timedia_avg=timedia_avg+float(nwrtdia_avg)*dt
          do itrc=1,NT
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR 
                  TXadv_avg(i,j,k,itrc) = cff *
     &                       ( TXadv_avg(i,j,k,itrc) +
     &                         TXadv(i,j,k,itrc) )
                  TYadv_avg(i,j,k,itrc) = cff *
     &                       ( TYadv_avg(i,j,k,itrc) +
     &                         TYadv(i,j,k,itrc) )
                  TVadv_avg(i,j,k,itrc) = cff *
     &                       ( TVadv_avg(i,j,k,itrc) +
     &                         TVadv(i,j,k,itrc) )
                  THmix_avg(i,j,k,itrc) = cff *
     &                       ( THmix_avg(i,j,k,itrc) +
     &                         THmix(i,j,k,itrc) )
                  TVmix_avg(i,j,k,itrc) = cff *
     &                       ( TVmix_avg(i,j,k,itrc) +
     &                         TVmix(i,j,k,itrc) )
                  TForc_avg(i,j,k,itrc) = cff *
     &                    ( TForc_avg(i,j,k,itrc) + 
     &                      TForc(i,j,k,itrc) )
                  Trate_avg(i,j,k,itrc) = cff *
     &                    ( Trate_avg(i,j,k,itrc) + 
     &                      Trate(i,j,k,itrc) )
                enddo
              enddo
            enddo
          enddo

#ifdef DIAGNOSTICS_TS_MLD
!
! Diagnostics averaged over MLD
!
          do itrc=1,NT
            do j=JstrR,JendR
              do i=IstrR,IendR 
                TXadv_mld_avg(i,j,itrc)=cff*( TXadv_mld(i,j,itrc) +
     &                                        TXadv_mld_avg(i,j,itrc) )
                TYadv_mld_avg(i,j,itrc)=cff*( TYadv_mld(i,j,itrc) +
     &                                        TYadv_mld_avg(i,j,itrc) )
                TVadv_mld_avg(i,j,itrc)=cff*( TVadv_mld(i,j,itrc) +
     &                                        TVadv_mld_avg(i,j,itrc) )
                THmix_mld_avg(i,j,itrc)=cff*( THmix_mld(i,j,itrc) +
     &                                        THmix_mld_avg(i,j,itrc) )
                TVmix_mld_avg(i,j,itrc)=cff*( TVmix_mld(i,j,itrc) +
     &                                        TVmix_mld_avg(i,j,itrc) )
                TForc_mld_avg(i,j,itrc)=cff*( TForc_mld(i,j,itrc) +
     &                                        TForc_mld_avg(i,j,itrc) )
                Trate_mld_avg(i,j,itrc)=cff*( Trate_mld(i,j,itrc) +
     &                                        Trate_mld_avg(i,j,itrc) )
                Tentr_mld_avg(i,j,itrc)=cff*( Tentr_mld(i,j,itrc) +
     &                                        Tentr_mld_avg(i,j,itrc) )
              enddo
            enddo
          enddo
#endif /* DIAGNOSTICS_TS_MLD  */
        endif
      endif
      return
      end
#else 
      subroutine set_diags_avg_empty
      end
#endif /*DIAGNOSTICS_TS && defined AVERAGES*/
