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
#if defined SOLVE3D && defined SPONGE_DIF2
!
# ifdef TCLIMATOLOGY
#  define CLIMAT_TS_SPONGE
# endif
!
# ifndef CHILD_SPG
       subroutine t3dmix_spg (tile)
      implicit none
        integer tile, itrc, trd, omp_get_thread_num
#  include "param.h"
#  include "private_scratch.h"
#  include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
#  ifdef AGRIF
       do itrc=1,NT
       if (AGRIF_Root()) then
         call t3dmix_spg_tile(Istr,Iend,Jstr,Jend, itrc, 
     &                     A2d(1, 1,trd), A2d(1, 2,trd),
     &                     A2d(1, 3,trd), A2d(1, 5,trd), A2d(1, 7,trd),
     &                     A2d(1, 9,trd), A2d(1,11,trd), A2d(1,13,trd))
       else
         call t3dmix_spg_child_tile(Istr,Iend,Jstr,Jend, itrc, 
     &                     A2d(1, 1,trd), A2d(1, 2,trd),
     &                     A2d(1, 3,trd), A2d(1, 5,trd), A2d(1, 7,trd),
     &                     A2d(1, 9,trd), A2d(1,11,trd), A2d(1,13,trd))
       endif
        enddo
        return
        end
#  else 
      do itrc=1,NT
       call t3dmix_spg_tile(Istr,Iend,Jstr,Jend, itrc, 
     &                     A2d(1, 1,trd), A2d(1, 2,trd),
     &                     A2d(1, 3,trd), A2d(1, 5,trd), A2d(1, 7,trd),
     &                     A2d(1, 9,trd), A2d(1,11,trd), A2d(1,13,trd))
      enddo
       return
       end
#  endif   /* AGRIF */
!
!---------------------------------------------------------------------
!*********************************************************************
!---------------------------------------------------------------------
!
!PARENT
!
      subroutine t3dmix_spg_tile(Istr,Iend,Jstr,Jend, itrc,
     &                         FX,FE, FC,dTdz, dTdx,dTde,dZdx,dZde)
#  undef CLIMAT_TS_SPONGE_FINE
!
!
! Compute laplacien diffusion in the parent grid sponge
! Diffusion applied on T-TCLM
!
# else
!
! CHILD
!
      subroutine t3dmix_spg_child_tile(Istr,Iend,Jstr,Jend, itrc,
     &                     FX,FE, FC,dTdz, dTdx,dTde,dZdx,dZde)

! Compute laplacien diffusion in the child sponge using 
! t3dmix_fine.F. Diffusion always applied applied on T-TCLM in fine grids
! (cpp keys :CLIMAT_TS_SPONGE_FINE)

#  define CLIMAT_TS_SPONGE_FINE
      
# endif /* CHILD_SPG */
!
!---------------------------------------------------------------------
! ******************************Common Code***************************
!---------------------------------------------------------------------
!!
      implicit none
# include "param.h"
      integer istr,iend,jstr,jend, itrc, i,j,k,k1,k2, kmld
      real   FX(PRIVATE_2D_SCRATCH_ARRAY),
     &       FE(PRIVATE_2D_SCRATCH_ARRAY),
     &       FC(PRIVATE_2D_SCRATCH_ARRAY,2),
     &     dTdz(PRIVATE_2D_SCRATCH_ARRAY,2),   cff,
     &     dTdx(PRIVATE_2D_SCRATCH_ARRAY,2),   cff1,
     &     dTde(PRIVATE_2D_SCRATCH_ARRAY,2),   cff2,
     &     dZdx(PRIVATE_2D_SCRATCH_ARRAY,2),   cff3,
     &     dZde(PRIVATE_2D_SCRATCH_ARRAY,2),   cff4

# include "grid.h"
# include "ocean3d.h"
# include "mixing.h"
# ifdef CLIMAT_TS_SPONGE
#  include "climat.h"
# endif
# include "scalars.h"
# ifdef DIAGNOSTICS_TS
#  include "diagnostics.h"
# endif
!
# ifdef AGRIF
#  include "zoom.h"
# endif
# include "compute_auxiliary_bounds.h"
!
# ifdef CHILD_SPG
#  define TCLM tsponge
# else
#  define TCLM tclm
# endif
!
# ifdef MASKING
#  define SWITCH *
# else
#  define SWITCH !
# endif
!
# ifdef TS_MIX_S
!--------------------------------------------------------------------
! Add in horizontal Laplacian diffusion along constant S-surfaces.
!--------------------------------------------------------------------
      do k=1,N
!
!  Compute XI- and ETA-components of diffusive tracer flux.
!
        do j=Jstr,Jend
          do i=Istr,Iend+1
          FX(i,j)=0.25*(diff2_sponge(i,j)+diff2_sponge(i-1,j))
     &                   *pmon_u(i,j)*(Hz(i,j,k)+Hz(i-1,j,k))*(
     &                t(i,j,k,nrhs,itrc)-t(i-1,j,k,nrhs,itrc)
# if defined CLIMAT_TS_SPONGE || defined CLIMAT_TS_SPONGE_FINE 
     &                   -TCLM(i,j,k,itrc)+TCLM(i-1,j,k,itrc)
# endif
     &                                      ) SWITCH umask(i,j)
          enddo
        enddo
        do j=Jstr,Jend+1
          do i=Istr,Iend
          FE(i,j)=0.25*(diff2_sponge(i,j)+diff2_sponge(i,j-1))
     &                     *pnom_v(i,j)*(Hz(i,j,k)+Hz(i,j-1,k))*(
     &                 t(i,j,k,nrhs,itrc)-t(i,j-1,k,nrhs,itrc)
# if defined CLIMAT_TS_SPONGE || defined CLIMAT_TS_SPONGE_FINE 
     &                    -TCLM(i,j,k,itrc)+TCLM(i,j-1,k,itrc)
# endif
     &                                      ) SWITCH vmask(i,j)
          enddo
        enddo
!
!  Add in horizontal diffusion of tracer [T m^3/s].
!
        do j=Jstr,Jend
          do i=Istr,Iend
            cff1=pm(i,j)*pn(i,j)
            t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)+dt*cff1
     &                 *(FX(i+1,j)-FX(i,j)+FE(i,j+1)-FE(i,j))
     &                                             /Hz(i,j,k)
          enddo
        enddo
      enddo

#else /* TS_MIX_S */

!
!-----------------------------------------------------------------------
! Compute horizontal (along geopotential     !!!   WARNING: RECURSIVE
! surfaces) Laplacian diffusion terms.       !!!   BLOCKING SEQUENCE
!-----------------------------------------------------------------------
!
      k2=1            ! vertical   dTdx,dTde(:,:,k2) k+1   rho-points 
      do k=0,N,+1     ! placement    dTdz,FC(:,:,k2) k+1/2   W-points 
        k1=k2         ! of scratch dTdx,dTde(:,:,k1) k     rho-points 
        k2=3-k1       ! variables    dTdz,FC(:,:,k1) k-1/2   W-points

        if (k.lt.N) then
          do j=Jstr,Jend
            do i=Istr,Iend+1
              cff=0.5*(pm(i,j)+pm(i-1,j)) SWITCH umask(i,j)
              dZdx(i,j,k2)=cff*(z_r(i,j,k+1) -z_r(i-1,j,k+1))
              dTdx(i,j,k2)=cff*(t(i,j,k+1,nrhs,itrc)
     &                               -t(i-1,j,k+1,nrhs,itrc)
# if defined CLIMAT_TS_SPONGE || defined CLIMAT_TS_SPONGE_FINE
     &               -TCLM(i,j,k+1,itrc)+TCLM(i-1,j,k+1,itrc)
#  endif
     &                                                  )
            enddo
          enddo
          do j=Jstr,Jend+1
            do i=Istr,Iend
              cff=0.5*(pn(i,j)+pn(i,j-1)) SWITCH vmask(i,j)
              dZde(i,j,k2)=cff*(z_r(i,j,k+1) -z_r(i,j-1,k+1))
              dTde(i,j,k2)=cff*(t(i,j,k+1,nrhs,itrc)
     &                               -t(i,j-1,k+1,nrhs,itrc)
# if defined CLIMAT_TS_SPONGE || defined CLIMAT_TS_SPONGE_FINE
     &               -TCLM(i,j,k+1,itrc)+TCLM(i,j-1,k+1,itrc)
#  endif
     &                                                  )
            enddo
          enddo
        endif
 
        if (k.eq.0 .or. k.eq.N) then
          do j=Jstr-1,Jend+1
            do i=Istr-1,Iend+1
              dTdz(i,j,k2)=0.0     ! Set no-flux bottom or
              FC(i,j,k2)=0.0       ! top boundary conditions.
            enddo
          enddo
        else
          do j=Jstr-1,Jend+1
            do i=Istr-1,Iend+1
              dTdz(i,j,k2)=(t(i,j,k+1,nrhs,itrc)-t(i,j,k,nrhs,itrc)
# if defined CLIMAT_TS_SPONGE || defined CLIMAT_TS_SPONGE_FINE
     &                          -TCLM(i,j,k+1,itrc)+TCLM(i,j,k,itrc)
# endif
     &                                   )/(z_r(i,j,k+1)-z_r(i,j,k))
            enddo
          enddo
        endif
 
        if (k.gt.0) then

          do j=Jstr,Jend
            do i=Istr,Iend+1
              FX(i,j)=0.25*(diff2_sponge(i,j)+diff2_sponge(i-1,j))
     &                     *(Hz(i,j,k)+Hz(i-1,j,k))*on_u(i,j)*(
     &           dTdx(i  ,j,k1)-0.5*(
     &             min(dZdx(i,j,k1),0.)*(dTdz(i-1,j,k1)+dTdz(i,j,k2))
     &            +max(dZdx(i,j,k1),0.)*(dTdz(i-1,j,k2)+dTdz(i,j,k1))
     &                                                             ))
            enddo
          enddo
          do j=Jstr,Jend+1
            do i=Istr,Iend
              FE(i,j)=0.25*(diff2_sponge(i,j)+diff2_sponge(i,j-1))
     &                     *(Hz(i,j,k)+Hz(i,j-1,k))*om_v(i,j)*(
     &           dTde(i,j,k1)-0.5*(
     &             min(dZde(i,j,k1),0.)*(dTdz(i,j-1,k1)+dTdz(i,j,k2))
     &            +max(dZde(i,j,k1),0.)*(dTdz(i,j-1,k2)+dTdz(i,j,k1))
     &                                                             ))
            enddo
          enddo
          if (k.lt.N) then
            do j=Jstr,Jend
              do i=Istr,Iend
                cff1=min(dZdx(i  ,j,k1),0.)
                cff2=min(dZdx(i+1,j,k2),0.)
                cff3=max(dZdx(i  ,j,k2),0.)
                cff4=max(dZdx(i+1,j,k1),0.)
                FC(i,j,k2)=0.5*diff2_sponge(i,j)*(
     &            cff1*(cff1*dTdz(i,j,k2)-dTdx(i  ,j,k1))
     &           +cff2*(cff2*dTdz(i,j,k2)-dTdx(i+1,j,k2))
     &           +cff3*(cff3*dTdz(i,j,k2)-dTdx(i  ,j,k2))
     &           +cff4*(cff4*dTdz(i,j,k2)-dTdx(i+1,j,k1))
     &                                         )
                cff1=min(dZde(i,j  ,k1),0.)
                cff2=min(dZde(i,j+1,k2),0.)
                cff3=max(dZde(i,j  ,k2),0.)
                cff4=max(dZde(i,j+1,k1),0.)
 
                FC(i,j,k2)=FC(i,j,k2)+0.5*diff2_sponge(i,j)*(
     &            cff1*(cff1*dTdz(i,j,k2)-dTde(i,j  ,k1))
     &           +cff2*(cff2*dTdz(i,j,k2)-dTde(i,j+1,k2))
     &           +cff3*(cff3*dTdz(i,j,k2)-dTde(i,j  ,k2))
     &           +cff4*(cff4*dTdz(i,j,k2)-dTde(i,j+1,k1))
     &                                                    )
              enddo
            enddo
          endif
          do j=Jstr,Jend
            do i=Istr,Iend
              cff1=pm(i,j)*pn(i,j)
               t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)+dt*(
     &                                 cff1*( FX(i+1,j)-FX(i,j)
     &                                       +FE(i,j+1)-FE(i,j) )
     &                                     +FC(i,j,k2)-FC(i,j,k1)
     &                                                  )/Hz(i,j,k)
            enddo
          enddo
          
#if defined AGRIF && defined AGRIF_CONSERV_TRA
          MYFX(IstrR:IendR,JstrR:JendR,k,itrc)=
     &          MYFX(IstrR:IendR,JstrR:JendR,k,itrc)
     &          +dt*FX(IstrR:IendR,JstrR:JendR)
          MYFY(IstrR:IendR,JstrR:JendR,k,itrc)=
     &          MYFY(IstrR:IendR,JstrR:JendR,k,itrc)
     &          +dt*FE(IstrR:IendR,JstrR:JendR)
#endif

# ifdef DIAGNOSTICS_TS
!
! Tracer diagnostics
!
          do j=Jstr,Jend
            do i=Istr,Iend
              cff1=pm(i,j)*pn(i,j)
              THmix(i,j,k,itrc)=THmix(i,j,k,itrc)*Hz(i,j,k)/cff1+ 
     &                                         FX(i+1,j)-FX(i,j)
     &                                        +FE(i,j+1)-FE(i,j)
#  ifndef TS_MIX_S
     &                             +(FC(i,j,k2)-FC(i,j,k1))/cff1
#  endif

              Trate(i,j,k,itrc)=(Hz(i,j,k)*t(i,j,k,nnew,itrc)
     &                      -Hz_bak(i,j,k)*t(i,j,k,nstp,itrc))
     &                                              /(dt*cff1)
!
! Divide all diagnostic terms by the cell volume
! (Hz(i,j,k,itrc)/(pm(i,j).*pn(i,j)). There after the unit
! of diag terms are: (unit of tracers)* s-1.
!
              THmix(i,j,k,itrc)=THmix(i,j,k,itrc)*cff1/Hz(i,j,k) 
     &                                         SWITCH rmask(i,j)
              Trate(i,j,k,itrc)=Trate(i,j,k,itrc)*cff1/Hz(i,j,k)
     &                                         SWITCH rmask(i,j)
            enddo
          enddo
# endif /* DIAGNOSTICS_TS */

        endif  ! <-- k.gt.0
      enddo   ! --> k
!
# endif /* TS_MIX_S */
!
!---------------------------------------------------------------------
!
# ifdef DIAGNOSTICS_TS
#  if defined DIAGNOSTICS_TS_MLD
!==========================================================
! Tracer diagnostics averaged over the MLD
!==========================================================
!
#   define T_mld_nnew FX
#   define T_mld_nstp FE
      do j=Jstr,Jend
        do i=Istr,Iend
           THmix_mld(i,j,itrc)=0.
#  ifdef TS_MIX_IMP
            TVmix_mld(i,j,itrc)=0.
#  endif
           T_mld_nnew(i,j)=0.
           T_mld_nstp(i,j)=0.
        enddo
      enddo
      do j=Jstr,Jend
        do i=Istr,Iend
#   ifdef LMD_SKPP
          kmld=kbl(i,j)
#   else
          kmld=N-5
#   endif
          do k=N,kmld,-1
            cff=Hz(i,j,k)/(z_w(i,j,N)-z_w(i,j,kmld-1))
            THmix_mld(i,j,itrc)=THmix_mld(i,j,itrc)+
     &                          THmix(i,j,k,itrc)*cff
#  ifdef TS_MIX_IMP
             TVmix_mld(i,j,itrc)=TVmix_mld(i,j,itrc)+
     &                           TVmix(i,j,k,itrc)*cff
#  endif
            T_mld_nnew(i,j)=T_mld_nnew(i,j)+
     &                      t(i,j,k,nnew,itrc)*cff
          enddo
        enddo
      enddo

      do j=Jstr,Jend
        do i=Istr,Iend
#   ifdef LMD_SKPP
          if (kbl_nstp(i,j).eq.0) kbl_nstp(i,j)=kbl(i,j)
          kmld=kbl_nstp(i,j)
#   else
          kmld=N-5
#   endif
          do k=N,kmld,-1
            cff=Hz_bak(i,j,k)/(z_w(i,j,N)-z_w(i,j,kmld-1))
            T_mld_nstp(i,j)=T_mld_nstp(i,j)+
     &                      t(i,j,k,nstp,itrc)*cff
          enddo
          if (itrc .eq. NT) kbl_nstp(i,j)=kbl(i,j)
        enddo
      enddo
      do j=Jstr,Jend
        do i=Istr,Iend
           Trate_mld(i,j,itrc)=(T_mld_nnew(i,j)-T_mld_nstp(i,j))/dt
           Tentr_mld(i,j,itrc)=Trate_mld(i,j,itrc)-
     &                         TXadv_mld(i,j,itrc)-
     &                         TYadv_mld(i,j,itrc)-
     &                         TVadv_mld(i,j,itrc)-
     &                         TVmix_mld(i,j,itrc)-
     &                         THmix_mld(i,j,itrc)-
     &                         TForc_mld(i,j,itrc)
        enddo
      enddo
#   undef T_mld_nnew 
#   undef T_mld_nstp
#  endif /* DIAGNOSTICS_TS_MLD */

#  ifdef DIAGNOSTICS_DEBUG
      if (istr.eq.1 .and. jstr.eq.1 .and. itrc.eq.itemp) then
        i=5
        j=5
#   if defined DIAGNOSTICS_TS_MLD
        cff=Trate_mld(i,j,itrc)-
     &      TXadv_mld(i,j,itrc)-
     &      TYadv_mld(i,j,itrc)-
     &      TVadv_mld(i,j,itrc)-
     &      TVmix_mld(i,j,itrc)-
     &      THmix_mld(i,j,itrc)-
     &      Tentr_mld(i,j,itrc)-
     &      TForc_mld(i,j,itrc)
           MPI_master_only write(stdout,'(A,2x,1pe10.3)')
     &     ' T3DMIX_SPG: T budget closure MLD     :', cff

#   endif /* DIAGNOSTICS_TS_MLD */
        cff=Trate(i,j,N-5,itrc)-
     &       TXadv(i,j,N-5,itrc)-
     &       TYadv(i,j,N-5,itrc)-
     &       TVadv(i,j,N-5,itrc)-
     &       TVmix(i,j,N-5,itrc)-
     &       THmix(i,j,N-5,itrc)-
     &       TForc(i,j,N-5,itrc)
           MPI_master_only write(stdout,'(A,2x,1pe10.3)')
     &     ' T3DMIX_SPG: T budget closure at k=N-5:', cff
      endif
#  endif /* DIAGNOSTICS_DEBUG */
# endif /* DIAGNOSTICS_TS */
!
!==========================================================
! Data exchange at the boundaries or interfaces
!==========================================================
!
# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
      call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                        t(START_2D_ARRAY,1,nnew,itrc))
# endif

      return
      end

#ifndef CHILD_SPG
# undef TCLM
# define CHILD_SPG
# ifdef AGRIF
#  include "t3dmix_spg.F"
# endif
# undef CHILD_SPG
#endif  /* !CHILD_SPG */

#else
      subroutine t3dmix_spg_empty
      end
#endif /* SOLVE3D */
