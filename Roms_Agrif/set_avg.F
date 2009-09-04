!
! $Id: set_avg.F,v 1.8 2005/09/23 15:56:31 pmarches Exp $
!
#include "cppdefs.h"
#ifdef AVERAGES
      subroutine set_avg (tile)
      implicit none
# include "param.h"
# ifdef SOLVE3D
#  include "work.h"
#  include "ncscrum.h"
# endif
      integer tile, trd
C$    integer omp_get_thread_num
# include "compute_tile_bounds.h"

# ifdef SOLVE3D
      if (wrtavg(indxW)) then
        call Wvlcty (tile, work)
      endif
# endif
      call set_avg_tile (Istr,Iend,Jstr,Jend)

      return
      end

      subroutine set_avg_tile (Istr,Iend,Jstr,Jend)
!
! Compute time-averaged fields within a tile.
! ------- ------------- ------ ------ - -----
! Because of syncronization issues, the delayed mode averaging
! procedure is used. This procedure implies that all fields to be
! averaged are sampled during the next time step, rather than at
! the end of the time step when they were computed.
!
! Though this algorithm results in somewhat ackward controlling
! logic it has the advantage that all fields to be sampled
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
      implicit none
# include "param.h"
      integer Istr,Iend,Jstr,Jend, i,j, ilc
      real cff
# ifdef SOLVE3D
      integer itrc,k
#  include "work.h"
# endif
# include "scalars.h"
# include "ncscrum.h"
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "averages.h"
# include "mixing.h"
# include "forces.h"
!
#include "compute_auxiliary_bounds.h"
!
      ilc=1+iic-ntstart   ! number of time step since restart
!
      if (ilc.gt.ntsavg) then 
        if (mod(ilc-1,navg).eq.1) then
          if (wrtavg(indxZ)) then
            do j=JstrR,JendR
              do i=IstrR,IendR
                zeta_avg(i,j)=zeta(i,j,fast_indx_out)
              enddo
            enddo
          endif
          if (wrtavg(indxUb)) then
            do j=JstrR,JendR
              do i=IstrR,IendR
                ubar_avg(i,j)=ubar(i,j,fast_indx_out)
              enddo
            enddo
          endif
          if (wrtavg(indxVb)) then
            do j=JstrR,JendR
              do i=IstrR,IendR
                vbar_avg(i,j)=vbar(i,j,fast_indx_out)
              enddo
            enddo
          endif
          if (wrtavg(indxBostr)) then
            do j=Jstr,Jend
              do i=Istr,Iend
                bostr_avg(i,j)=
     &                    0.5*sqrt((bustr(i,j)+bustr(i+1,j))**2
     &                            +(bvstr(i,j)+bvstr(i,j+1))**2)
     &                                                     *rho0
              enddo
            enddo
          endif
          if (wrtavg(indxWstr)) then
            do j=Jstr,Jend
              do i=Istr,Iend
                wstr_avg(i,j)=
     &                    0.5*sqrt((sustr(i,j)+sustr(i+1,j))**2
     &                            +(svstr(i,j)+svstr(i,j+1))**2)
     &                                                     *rho0
              enddo
            enddo
          endif
          if (wrtavg(indxUWstr)) then
            do j=Jstr,Jend
              do i=Istr,Iend
                sustr_avg(i,j)=sustr(i,j)*rho0
              enddo
            enddo
          endif
          if (wrtavg(indxVWstr)) then
            do j=Jstr,Jend
              do i=Istr,Iend
                svstr_avg(i,j)=svstr(i,j)*rho0
              enddo
            enddo
          endif
# ifdef SOLVE3D
          if (wrtavg(indxU)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  u_avg(i,j,k)=u(i,j,k,nstp)
                enddo
              enddo
            enddo
          endif
          if (wrtavg(indxV)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  v_avg(i,j,k)=v(i,j,k,nstp)
                enddo
              enddo
            enddo
          endif
          do itrc=1,NT
            if (wrtavg(indxT+itrc-1)) then
              do k=1,N
                do j=JstrR,JendR
                  do i=IstrR,IendR
                    t_avg(i,j,k,itrc)=t(i,j,k,nstp,itrc)
                  enddo
                enddo
              enddo
            endif
          enddo
          if (wrtavg(indxR)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  rho_avg(i,j,k)=rho(i,j,k)
                enddo
              enddo
            enddo
          endif
          if (wrtavg(indxO)) then
            do k=0,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  omega_avg(i,j,k)=w(i,j,k)
                enddo
              enddo
            enddo
          endif
          if (wrtavg(indxW)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  w_avg(i,j,k)=work(i,j,k)
                enddo
              enddo
            enddo
          endif
#  ifdef LMD_SKPP
          if (wrtavg(indxHbl)) then
            do j=JstrR,JendR
              do i=IstrR,IendR
                hbl_avg(i,j)=hbl(i,j)
              enddo
            enddo
          endif
#  endif
#  ifdef LMD_BKPP
          if (wrtavg(indxHbbl)) then
            do j=JstrR,JendR
              do i=IstrR,IendR
                hbbl_avg(i,j)=hbbl(i,j)
              enddo
            enddo
          endif
#  endif
#  ifdef BULK_FLUX
            do j=Jstr,Jend
              do i=Istr,Iend
                shflx_rsw_avg(i,j)=shflx_rsw(i,j)
              enddo
            enddo
            do j=Jstr,Jend
              do i=Istr,Iend
                shflx_rlw_avg(i,j)=shflx_rlw(i,j)
              enddo
            enddo
            do j=Jstr,Jend
              do i=Istr,Iend
                shflx_lat_avg(i,j)=shflx_lat(i,j)
              enddo
            enddo
            do j=Jstr,Jend
              do i=Istr,Iend
                shflx_sen_avg(i,j)=shflx_sen(i,j)
              enddo
            enddo
# endif
#  ifdef VIS_COEF_3D
          if (wrtavg(indxVisc)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  visc3d_avg(i,j,k)=visc3d_r(i,j,k)
                enddo
              enddo
            enddo
          endif
#  endif
#  ifdef DIF_COEF_3D
          if (wrtavg(indxDiff)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  diff3d_avg(i,j,k)=diff3d(i,j,k)
                enddo
              enddo
            enddo
          endif
#  endif
#  ifdef AVERAGES_K
          if (wrtavg(indxAkv)) then
            do k=0,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  Akv_avg(i,j,k)=Akv(i,j,k)
                enddo
              enddo
            enddo
          endif
          if (wrtavg(indxAkt)) then
            do k=0,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  Akt_avg(i,j,k,itemp)=Akt(i,j,k,itemp)
                enddo
              enddo
            enddo
          endif
#   ifdef SALINITY
          if (wrtavg(indxAks)) then
            do k=0,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  Akt_avg(i,j,k,isalt)=Akt(i,j,k,isalt)
                enddo
              enddo
            enddo
          endif
#   endif
#  endif
#  ifdef BIOLOGY
          if (wrtavg(indxHel)) then
            do j=JstrR,JendR
              do i=IstrR,IendR
                hel_avg(i,j)=hel(i,j)
              enddo
            enddo
          endif
#   ifdef BIO_NChlPZD
          if (wrtavg(indxChC)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  theta_avg(i,j,k)=theta(i,j,k)
                enddo
              enddo
            enddo
          endif
#    ifdef OXYGEN
          if (wrtavg(indxU10)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   u10_avg(i,j)=u10(i,j)
                enddo
             enddo
          endif
          if (wrtavg(indxKvO2)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   Kv_O2_avg(i,j)=Kv_O2(i,j)
                enddo
             enddo
          endif
          if (wrtavg(indxO2sat)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   O2satu_avg(i,j)=O2satu(i,j)
                enddo
             enddo
          endif
#    endif /* OXYGEN */
#   elif defined BIO_N2P2Z2D2 && defined VAR_CHL_C
          if (wrtavg(indxChC1)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  theta1_avg(i,j,k)=theta1(i,j,k)
                enddo
              enddo
            enddo
          endif

          if (wrtavg(indxChC2)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  theta2_avg(i,j,k)=theta2(i,j,k)
                enddo
              enddo
            enddo
          endif
#    endif
#   endif /* BIOLOGY */
#  endif /* SOLVE3D */

        elseif (mod(ilc-1,navg).gt.1) then
          if (wrtavg(indxZ)) then
            do j=JstrR,JendR
              do i=IstrR,IendR
                zeta_avg(i,j)=zeta_avg(i,j)+zeta(i,j,fast_indx_out)
              enddo
            enddo
          endif
          if (wrtavg(indxUb)) then
            do j=JstrR,JendR
              do i=IstrR,IendR
                ubar_avg(i,j)=ubar_avg(i,j)+ubar(i,j,fast_indx_out)
              enddo
            enddo
          endif
          if (wrtavg(indxVb)) then
            do j=JstrR,JendR
              do i=IstrR,IendR
                vbar_avg(i,j)=vbar_avg(i,j)+vbar(i,j,fast_indx_out)
              enddo
            enddo
          endif
          if (wrtavg(indxBostr)) then
            do j=Jstr,Jend
              do i=Istr,Iend
                bostr_avg(i,j)=bostr_avg(i,j)+
     &                         0.5*sqrt((bustr(i,j)+bustr(i+1,j))**2
     &                                 +(bvstr(i,j)+bvstr(i,j+1))**2)
     &                                                          *rho0
              enddo
            enddo
          endif
          if (wrtavg(indxWstr)) then
            do j=Jstr,Jend
              do i=Istr,Iend
                wstr_avg(i,j)=wstr_avg(i,j)+
     &                         0.5*sqrt((sustr(i,j)+sustr(i+1,j))**2
     &                                 +(svstr(i,j)+svstr(i,j+1))**2)
     &                                                          *rho0
              enddo
            enddo
          endif
          if (wrtavg(indxUWstr)) then
            do j=JstrR,JendR
              do i=IstrR,IendR
                sustr_avg(i,j)=sustr_avg(i,j)+
     &                         sustr(i,j)*rho0
              enddo
            enddo
          endif
          if (wrtavg(indxVWstr)) then
            do j=JstrR,JendR
              do i=IstrR,IendR
                svstr_avg(i,j)=svstr_avg(i,j)+
     &                         svstr(i,j)*rho0
              enddo
            enddo
          endif


# ifdef SOLVE3D
          if (wrtavg(indxU)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  u_avg(i,j,k)=u_avg(i,j,k)+u(i,j,k,nstp)
                enddo
              enddo
            enddo
          endif
          if (wrtavg(indxV)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  v_avg(i,j,k)=v_avg(i,j,k)+v(i,j,k,nstp)
                enddo
              enddo
            enddo
          endif
          do itrc=1,NT
            if (wrtavg(indxT+itrc-1)) then
              do k=1,N
                do j=JstrR,JendR
                  do i=IstrR,IendR
                    t_avg(i,j,k,itrc)=t_avg(i,j,k,itrc)
     &                               +t(i,j,k,nstp,itrc)
                  enddo
                enddo
              enddo
            endif
          enddo
          if (wrtavg(indxR)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  rho_avg(i,j,k)=rho_avg(i,j,k)+rho(i,j,k)
                enddo
              enddo
            enddo
          endif
          if (wrtavg(indxO)) then
            do k=0,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  omega_avg(i,j,k)=omega_avg(i,j,k)+w(i,j,k)
                enddo
              enddo
            enddo
          endif
          if (wrtavg(indxW)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  w_avg(i,j,k)=w_avg(i,j,k)+work(i,j,k)
                enddo
              enddo
            enddo
          endif
#  ifdef LMD_SKPP
          if (wrtavg(indxHbl)) then
            do j=JstrR,JendR
              do i=IstrR,IendR
                hbl_avg(i,j)=hbl_avg(i,j)+hbl(i,j)
              enddo
            enddo
          endif
#  endif
#  ifdef LMD_BKPP
          if (wrtavg(indxHbbl)) then
            do j=JstrR,JendR
              do i=IstrR,IendR
                hbbl_avg(i,j)=hbbl_avg(i,j)+hbbl(i,j)
              enddo
            enddo
          endif
#  endif
#  ifdef BULK_FLUX
            do j=Jstr,Jend
              do i=Istr,Iend
                shflx_rsw_avg(i,j)=shflx_rsw_avg(i,j)+shflx_rsw(i,j)
              enddo
            enddo
            do j=Jstr,Jend
              do i=Istr,Iend
                shflx_rlw_avg(i,j)=shflx_rlw_avg(i,j)+shflx_rlw(i,j)
              enddo
            enddo
            do j=Jstr,Jend
              do i=Istr,Iend
                shflx_lat_avg(i,j)=shflx_lat_avg(i,j)+shflx_lat(i,j)
              enddo
            enddo
            do j=Jstr,Jend
              do i=Istr,Iend
                shflx_sen_avg(i,j)=shflx_sen_avg(i,j)+shflx_sen(i,j)
              enddo
            enddo
#  endif
#  ifdef VIS_COEF_3D
          if (wrtavg(indxVisc)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  visc3d_avg(i,j,k)=visc3d_avg(i,j,k)+visc3d_r(i,j,k)
                enddo
              enddo
            enddo
          endif
#  endif
#  ifdef DIF_COEF_3D
          if (wrtavg(indxDiff)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  diff3d_avg(i,j,k)=diff3d_avg(i,j,k)+diff3d(i,j,k)
                enddo
              enddo
            enddo
          endif
#  endif
#  ifdef AVERAGES_K
          if (wrtavg(indxAkv)) then
            do k=0,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  Akv_avg(i,j,k)=Akv_avg(i,j,k)+Akv(i,j,k)
                enddo
              enddo
            enddo
          endif
          if (wrtavg(indxAkt)) then
            do k=0,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  Akt_avg(i,j,k,itemp)=Akt_avg(i,j,k,itemp)
     &                                    +Akt(i,j,k,itemp)
                enddo
              enddo
            enddo
          endif
#   ifdef SALINITY
          if (wrtavg(indxAks)) then
            do k=0,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  Akt_avg(i,j,k,isalt)=Akt_avg(i,j,k,isalt)
     &                                    +Akt(i,j,k,isalt)
                enddo
              enddo
            enddo
          endif
#   endif
#  endif /* AVERAGES_K */
#  ifdef BIOLOGY
          if (wrtavg(indxHel)) then
            do j=JstrR,JendR
              do i=IstrR,IendR
                hel_avg(i,j)=hel_avg(i,j)+hel(i,j)
              enddo
            enddo
          endif
#   ifdef BIO_NChlPZD
          if (wrtavg(indxChC)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  theta_avg(i,j,k)=theta_avg(i,j,k)+theta(i,j,k)
                enddo
              enddo
            enddo
          endif
#    ifdef OXYGEN
          if (wrtavg(indxU10)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   u10_avg(i,j)=u10_avg(i,j)+u10(i,j)
                enddo
             enddo
          endif
          if (wrtavg(indxKvO2)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   Kv_O2_avg(i,j)=Kv_O2_avg(i,j)+Kv_O2(i,j)
                enddo
             enddo
          endif
          if (wrtavg(indxO2sat)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   O2satu_avg(i,j)=O2satu_avg(i,j)+O2satu(i,j)
                enddo
             enddo
          endif
#    endif /* OXYGEN */
#   elif defined BIO_N2P2Z2D2 && defined VAR_CHL_C
          if (wrtavg(indxChC1)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  theta1_avg(i,j,k)=theta1_avg(i,j,k)+theta1(i,j,k)
                enddo
              enddo
            enddo
          endif

          if (wrtavg(indxChC2)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  theta2_avg(i,j,k)=theta2_avg(i,j,k)+theta2(i,j,k)
                enddo
              enddo
            enddo
          endif
#   endif
#  endif /* BIOLOGY */
# endif /* SOLVE3D */

        elseif (mod(ilc-1,navg).eq.0) then
          cff=1./float(navg)
          if (ZEROTH_TILE) time_avg=time_avg+float(navg)*dt
          if (wrtavg(indxZ)) then
            do j=JstrR,JendR
              do i=IstrR,IendR
                zeta_avg(i,j)=cff*( zeta_avg(i,j)
     &                   +zeta(i,j,fast_indx_out))
              enddo
            enddo
          endif
          if (wrtavg(indxUb)) then
            do j=JstrR,JendR
              do i=IstrR,IendR
                ubar_avg(i,j)=cff*( ubar_avg(i,j)
     &                   +ubar(i,j,fast_indx_out))
              enddo
            enddo
          endif
          if (wrtavg(indxVb)) then
            do j=JstrR,JendR
              do i=IstrR,IendR
                vbar_avg(i,j)=cff*( vbar_avg(i,j)
     &                   +vbar(i,j,fast_indx_out))
              enddo
            enddo
          endif
          if (wrtavg(indxBostr)) then
            do j=Jstr,Jend
              do i=Istr,Iend
                bostr_avg(i,j)=cff*( bostr_avg(i,j)+
     &                         0.5*sqrt((bustr(i,j)+bustr(i+1,j))**2
     &                                 +(bvstr(i,j)+bvstr(i,j+1))**2)
     &                                                          *rho0)
              enddo
            enddo
          endif
          if (wrtavg(indxWstr)) then
            do j=Jstr,Jend
              do i=Istr,Iend
                wstr_avg(i,j)=cff*( wstr_avg(i,j)+
     &                         0.5*sqrt((sustr(i,j)+sustr(i+1,j))**2
     &                                 +(svstr(i,j)+svstr(i,j+1))**2)
     &                                                          *rho0)
              enddo
            enddo
          endif
          if (wrtavg(indxUWstr)) then
            do j=JstrR,JendR
              do i=IstrR,IendR
                sustr_avg(i,j)=cff*(sustr_avg(i,j)+
     &                         sustr(i,j)*rho0)
              enddo
            enddo
          endif
          if (wrtavg(indxVWstr)) then
            do j=JstrR,JendR
              do i=IstrR,IendR
                svstr_avg(i,j)=cff*(svstr_avg(i,j)+
     &                         svstr(i,j)*rho0)
              enddo
            enddo
          endif
# ifdef SOLVE3D
          if (wrtavg(indxU)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  u_avg(i,j,k)=cff*(u_avg(i,j,k)+u(i,j,k,nstp))
                enddo
              enddo
            enddo
          endif
          if (wrtavg(indxV)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  v_avg(i,j,k)=cff*(v_avg(i,j,k)+v(i,j,k,nstp))
                enddo
              enddo
            enddo
          endif
          do itrc=1,NT
            if (wrtavg(indxT+itrc-1)) then
              do k=1,N
                do j=JstrR,JendR
                  do i=IstrR,IendR
                    t_avg(i,j,k,itrc)=cff*( t_avg(i,j,k,itrc)
     &                                    +t(i,j,k,nstp,itrc))
                  enddo
                enddo
              enddo
            endif
          enddo
          if (wrtavg(indxR)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  rho_avg(i,j,k)=cff*(rho_avg(i,j,k)+rho(i,j,k))
                enddo
              enddo
            enddo
          endif
          if (wrtavg(indxO)) then
            do k=0,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  omega_avg(i,j,k)=cff*pm(i,j)*pn(i,j)*
     &                             (omega_avg(i,j,k)+w(i,j,k))
                enddo
              enddo
            enddo
          endif
          if (wrtavg(indxW)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  w_avg(i,j,k)=cff*(w_avg(i,j,k)+work(i,j,k))
                enddo
              enddo
            enddo
          endif
#  ifdef LMD_SKPP
          if (wrtavg(indxHbl)) then
            do j=JstrR,JendR
              do i=IstrR,IendR
                hbl_avg(i,j)=cff*( hbl_avg(i,j)
     &                               +hbl(i,j))
              enddo
            enddo
          endif
#  endif
#  ifdef LMD_BKPP
          if (wrtavg(indxHbbl)) then
            do j=JstrR,JendR
              do i=IstrR,IendR
                hbbl_avg(i,j)=cff*( hbbl_avg(i,j)
     &                                +hbbl(i,j))
              enddo
            enddo
          endif
#  endif
#  ifdef BULK_FLUX
            do j=Jstr,Jend
              do i=Istr,Iend
                shflx_rsw_avg(i,j)=cff*( shflx_rsw_avg(i,j)+
     &                                   shflx_rsw(i,j) )
              enddo
            enddo
            do j=Jstr,Jend
              do i=Istr,Iend
                shflx_rlw_avg(i,j)=cff*( shflx_rlw_avg(i,j)+
     &                                   shflx_rlw(i,j) )
              enddo
            enddo
            do j=Jstr,Jend
              do i=Istr,Iend
                shflx_lat_avg(i,j)=cff*( shflx_lat_avg(i,j)+
     &                                   shflx_lat(i,j) )
              enddo
            enddo
            do j=Jstr,Jend
              do i=Istr,Iend
                shflx_sen_avg(i,j)=cff*( shflx_sen_avg(i,j)+
     &                                   shflx_sen(i,j) )
              enddo
            enddo
#  endif
#  ifdef VIS_COEF_3D
          if (wrtavg(indxVisc)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  visc3d_avg(i,j,k)=cff*(visc3d_avg(i,j,k)
     &                                  +visc3d_r(i,j,k))
                enddo
              enddo
            enddo
          endif
#  endif
#  ifdef DIF_COEF_3D
          if (wrtavg(indxDiff)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  diff3d_avg(i,j,k)=cff*(diff3d_avg(i,j,k)
     &                                  +diff3d(i,j,k))
                enddo
              enddo
            enddo
          endif
#  endif
#  ifdef AVERAGES_K
          if (wrtavg(indxAkv)) then
            do k=0,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  Akv_avg(i,j,k)=cff*(Akv_avg(i,j,k)+Akv(i,j,k))
                enddo
              enddo
            enddo
          endif
          if (wrtavg(indxAkt)) then
            do k=0,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  Akt_avg(i,j,k,itemp)=cff*(Akt_avg(i,j,k,itemp)
     &                                         +Akt(i,j,k,itemp))
                enddo
              enddo
            enddo
          endif
#   ifdef SALINITY
          if (wrtavg(indxAks)) then
            do k=0,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  Akt_avg(i,j,k,isalt)=cff*(Akt_avg(i,j,k,isalt)
     &                                         +Akt(i,j,k,isalt))
                enddo
              enddo
            enddo
          endif
#   endif
#  endif
#  ifdef BIOLOGY
          if (wrtavg(indxHel)) then
            do j=JstrR,JendR
              do i=IstrR,IendR
                hel_avg(i,j)=cff*( hel_avg(i,j)
     &                            +hel(i,j)     )
              enddo
            enddo
          endif
#   ifdef BIO_NChlPZD
          if (wrtavg(indxChC)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  theta_avg(i,j,k)=cff*( theta_avg(i,j,k)
     &                                  +theta(i,j,k)     )
                enddo
              enddo
            enddo
          endif
#    ifdef OXYGEN
          if (wrtavg(indxU10)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   u10_avg(i,j)=cff*( u10_avg(i,j)+u10(i,j) )
                enddo
             enddo
          endif
          if (wrtavg(indxKvO2)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   Kv_O2_avg(i,j)=cff*( Kv_O2_avg(i,j)+Kv_O2(i,j) )
                enddo
             enddo
          endif
          if (wrtavg(indxO2sat)) then
             do j=JstrR,JendR
                do i=IstrR,IendR
                   O2satu_avg(i,j)=cff*( O2satu_avg(i,j)+O2satu(i,j) )
                enddo
             enddo
          endif
#    endif /* OXYGEN */
#   elif defined BIO_N2P2Z2D2 && defined VAR_CHL_C
          if (wrtavg(indxChC1)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  theta1_avg(i,j,k)=cff*( theta1_avg(i,j,k)
     &                                   +theta1(i,j,k)     )
                enddo
              enddo
            enddo
          endif

          if (wrtavg(indxChC2)) then
            do k=1,N
              do j=JstrR,JendR
                do i=IstrR,IendR
                  theta2_avg(i,j,k)=cff*( theta2_avg(i,j,k)
     &                                   +theta2(i,j,k)     )
                enddo
              enddo
            enddo
          endif
#   endif
#  endif /* BIOLOGY */
# endif /* SOLVE3D */
        endif
      endif
      return
      end
#else
      subroutine set_avg_empty
      end
#endif /* AVERAGES */

