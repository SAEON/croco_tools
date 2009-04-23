#include "cppdefs.h"
#ifndef CHILD_SPG
      subroutine t3dmix (tile)
      implicit none
      integer tile, itrc, trd, omp_get_thread_num
#include "param.h"
#include "private_scratch.h"
#include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
#ifdef AGRIF
      do itrc=1,NT
        if (AGRIF_Root()) then
            call t3dmix_tile (istr,iend,jstr,jend, itrc, 
     &          A2d(1,1,trd),A2d(1,2,trd), A2d(1,3,trd))
        else
            call t3dmix_child_tile (istr,iend,jstr,jend, itrc,
     &          A2d(1,1,trd), A2d(1,2,trd), A2d(1,3,trd))
        endif
      enddo
      return
      end
#else 
      do itrc=1,NT
       call t3dmix_tile(istr,iend,jstr,jend, itrc, A2d(1,1,trd),
     &                                 A2d(1,2,trd), A2d(1,3,trd))
      enddo
       return
       end
#endif   /* AGRIF */
!
!---------------------------------------------------------------------
!*********************************************************************
!---------------------------------------------------------------------
!
!PARENT
! 
      subroutine t3dmix_tile (istr,iend,jstr,jend, itrc,
     &                    FX,FE, LapT)
#undef CLIMAT_TS_MIXH_FINE
!
#else
!
! CHILD
!
      subroutine t3dmix_child_tile(istr,iend,jstr,jend, itrc,
     &                     FX,FE, LapT)
#if !defined TS_SPLIT_UP3
#define CLIMAT_TS_MIXH_FINE
#endif
!
#endif /* CHILD_SPG */
!
!---------------------------------------------------------------------
! ******************************Common Code***************************
!---------------------------------------------------------------------
!!   
      implicit none
#include "param.h"
      integer itrc, istr,iend,jstr,jend, i,j,k, kmld,
     &        imin,imax,jmin,jmax
      real    FX(PRIVATE_2D_SCRATCH_ARRAY),     cff,
     &        FE(PRIVATE_2D_SCRATCH_ARRAY),     cff1,
     &        LapT(PRIVATE_2D_SCRATCH_ARRAY),   cff2
#include "grid.h"
#include "ocean3d.h"
#include "mixing.h"
# ifdef CLIMAT_TS_MIXH
#include "climat.h"
#endif
#include "scalars.h"
#ifdef DIAGNOSTICS_TS
# include "diagnostics.h"
#endif
!
#include "compute_auxiliary_bounds.h"
!
#ifdef AGRIF
#include "zoom.h"
#endif
!
#ifdef CHILD_SPG
#define TCLM tsponge
#else
#define TCLM tclm
#endif
!
      do k=1,N
 
#ifdef TS_DIF2
!
!--------------------------------------------------------------------
!  Add in horizontal Laplacian diffusion along constant S-surfaces.
!--------------------------------------------------------------------
!
!  Compute XI- and ETA-components of diffusive tracer flux.
!
        do j=jstr,jend
          do i=istr,iend+1
# ifdef DIF_COEF_3D
              FX(i,j)=0.25*(diff3d(i,j,k)+diff3d(i-1,j,k))
# else
              FX(i,j)=0.25*(diff2(i,j,itrc)+diff2(i-1,j,itrc))
#endif
     &                   *pmon_u(i,j)*(Hz(i,j,k)+Hz(i-1,j,k))*(
     &                     t(i,j,k,nrhs,itrc)-t(i-1,j,k,nrhs,itrc)
# if defined CLIMAT_TS_MIXH || defined CLIMAT_TS_MIXH_FINE
     &                          -TCLM(i,j,k,itrc)+TCLM(i,j-1,k,itrc)
# endif
     &                                              ) 
#    ifdef MASKING
     &                                            * umask(i,j)
#    endif
          enddo
        enddo
        do j=jstr,jend+1
          do i=istr,iend
# ifdef DIF_COEF_3D
              FE(i,j)=0.25*(diff3d(i,j,k)+diff3d(i,j-1,k))
# else
              FE(i,j)=0.25*(diff2(i,j,itrc)+diff2(i,j-1,itrc))
# endif
     &                     *pnom_v(i,j)*(Hz(i,j,k)+Hz(i,j-1,k))*(
     &                      t(i,j,k,nrhs,itrc)-t(i,j-1,k,nrhs,itrc)
# if defined CLIMAT_TS_MIXH || defined CLIMAT_TS_MIXH_FINE
     &                          -TCLM(i,j,k,itrc)+TCLM(i,j-1,k,itrc)
# endif
     &                                              ) 
#    ifdef MASKING
     &                                              * vmask(i,j)
#    endif
          enddo
        enddo
!
!  Add in horizontal diffusion of tracer [T m^3/s].
!
        do j=jstr,jend
          do i=istr,iend
            cff1=pm(i,j)*pn(i,j)
            t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)+dt*cff1
     &                 *(FX(i+1,j)-FX(i,j)+FE(i,j+1)-FE(i,j))
     &                                             /Hz(i,j,k)
          enddo
        enddo

#endif /* TS_DIF2 */

#ifdef TS_DIF4
!
!--------------------------------------------------------------------
!  Compute/Add in horizontal biharmonic diffusion along constant
!  S-surfaces.
!--------------------------------------------------------------------
!  The biharmonic operator is computed by applying the Laplacian
!  operator twice.
!
# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then
        imin=istr
      else
        imin=istr-1
      endif
      if (EASTERN_EDGE) then
        imax=iend
      else
        imax=iend+1
      endif
# else
      imin=istr-1
      imax=iend+1
# endif
# ifndef NS_PERIODIC
      if (SOUTHERN_EDGE) then
        jmin=jstr
      else
        jmin=jstr-1
      endif
      if (NORTHERN_EDGE) then
        jmax=jend
      else
        jmax=jend+1
      endif
# else
      jmin=jstr-1
      jmax=jend+1
# endif

!
!  Compute horizontal tracer flux in the XI-direction at U-points.
!
          do j=jmin,jmax
            do i=imin,imax+1
            FX(i,j)=pmon_u(i,j)*0.5*(Hz(i,j,k)+Hz(i-1,j,k))*(
     &                      t(i,j,k,nrhs,itrc)-t(i-1,j,k,nrhs,itrc)
# if defined CLIMAT_TS_MIXH || defined CLIMAT_TS_MIXH_FINE
     &                          -TCLM(i,j,k,itrc)+TCLM(i-1,j,k,itrc)
# endif
     &                                              ) 
#    ifdef MASKING
     &                                             * umask(i,j)
#    endif
          enddo
        enddo
!
!  Compute horizontal tracer flux in the ETA-direction at V-points.
!
        do j=jmin,jmax+1
          do i=imin,imax
            FE(i,j)=pnom_v(i,j)*0.5*(Hz(i,j,k)+Hz(i,j-1,k))*(
     &                     t(i,j,k,nrhs,itrc)-t(i,j-1,k,nrhs,itrc)
# if defined CLIMAT_TS_MIXH || defined CLIMAT_TS_MIXH_FINE
     &                          -TCLM(i,j,k,itrc)+TCLM(i,j-1,k,itrc)
# endif
     &                                              ) 
#    ifdef MASKING
     &                                              * vmask(i,j)
#    endif
          enddo
        enddo
!
!  Compute first Laplacian, without mixing coefficient.
!  Multiply by the metrics of the second Laplacian.
!  Save into work array "LapT".
!
         do j=jmin,jmax
            do i=imin,imax
            LapT(i,j)=(FX(i+1,j)-FX(i,j)+FE(i,j+1)-FE(i,j))
     &                          *pm(i,j)*pn(i,j)/Hz(i,j,k)
          enddo
        enddo
!
!  Apply boundary conditions (except periodic; closed or gradient)
!  to the first Laplacian.
!
# ifndef EW_PERIODIC
        if (WESTERN_EDGE) then
          do j=jmin,jmax
#  ifndef OBC_WEST
            LapT(istr-1,j)=0.
#  else
            LapT(istr-1,j)=LapT(istr,j)
#  endif
          enddo
        endif
        if (EASTERN_EDGE) then
          do j=jmin,jmax
#  ifndef OBC_EAST
            LapT(iend+1,j)=0.
#  else
            LapT(iend+1,j)=LapT(iend,j)
#  endif
          enddo
        endif
# endif /* !EW_PERIODIC */
# ifndef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          do i=imin,imax
#  ifndef OBC_SOUTH
            LapT(i,jstr-1)=0.
#  else
            LapT(i,jstr-1)=LapT(i,jstr)
#  endif
          enddo
        endif
        if (NORTHERN_EDGE) then
          do i=imin,imax
#  ifndef OBC_NORTH
            LapT(i,jend+1)=0.
#  else
            LapT(i,jend+1)=LapT(i,jend)
#  endif
          enddo
        endif
# endif /* !NS_PERIODIC */
!
!  Compute FX=d(LapT)/d(xi) and FE=d(LapT)/d(eta) terms
!  After that cmpute and add in biharmonic mixing [T m^3/s].
!  Multiply by mixing coefficient.
!
        do j=jstr,jend
          do i=istr,iend+1
# ifdef DIF_COEF_3D
#  ifdef TS_SPLIT_UP3
              FX(i,j)=-0.5*diff3d_u(i,j,k)
#  else
              FX(i,j)=-0.25*(diff3d(i,j,k)+diff3d(i-1,j,k))
#  endif
# else
              FX(i,j)=-0.25*(diff4(i,j,itrc)+diff4(i-1,j,itrc))
# endif
     &                     *pmon_u(i,j)*(Hz(i,j,k)+Hz(i-1,j,k))
     &                                 *(LapT(i,j)-LapT(i-1,j)) 
#    ifdef MASKING
     &                                        * umask(i,j)
#    endif
          enddo
        enddo
        do j=jstr,jend+1
          do i=istr,iend
# ifdef DIF_COEF_3D
#  ifdef TS_SPLIT_UP3
              FE(i,j)=-0.5*diff3d_v(i,j,k)
#  else
              FE(i,j)=-0.25*(diff3d(i,j,k)+diff3d(i,j-1,k))
#  endif
# else
              FE(i,j)=-0.25*(diff4(i,j,itrc)+diff4(i,j-1,itrc))
# endif
     &                     *pnom_v(i,j)*(Hz(i,j,k)+Hz(i,j-1,k))
     &                                 *(LapT(i,j)-LapT(i,j-1))
#    ifdef MASKING
     &                                         * vmask(i,j)
#    endif
          enddo
        enddo
!
!  Add in horizontal diffusion of tracer [T m^3/s].
!
        do j=jstr,jend
          do i=istr,iend
            cff1=pm(i,j)*pn(i,j)
            t(i,j,k,nnew,itrc)=t(i,j,k,nnew,itrc)+dt*cff1
     &                 *(FX(i+1,j)-FX(i,j)+FE(i,j+1)-FE(i,j))
     &                                             /Hz(i,j,k)
          enddo
        enddo

#endif /* TS_DIF4 */
      enddo   ! --> k
!------------------------------------------------------------------
# ifdef DIAGNOSTICS_TS
!
!==========================================================
! Tracer diagnostics
!==========================================================
!
      do k=1,N
        do j=jstr,jend
          do i=istr,iend
            cff1=pm(i,j)*pn(i,j)
            THmix(i,j,k,itrc)=FX(i+1,j)-FX(i,j)
     &                       +FE(i,j+1)-FE(i,j)
#  ifndef MIX_S_TS
     &                       +(FC(i,j,k2)-FC(i,j,k1))/cff1
# endif
#  ifdef MASKING
     &                                        * rmask(i,j)
#  endif
            Trate(i,j,k,itrc)=(Hz(i,j,k)*t(i,j,k,nnew,itrc)
     &                    -Hz_bak(i,j,k)*t(i,j,k,nstp,itrc))
     &                                            /(dt*cff1)
#  ifdef MASKING
     &                                        * rmask(i,j)
#  endif
!
! Divide all diagnostic terms by the cell volume
! (Hz(i,j,k,itrc)/(pm(i,j).*pn(i,j)). There after the unit
! of diag terms are: (unit of tracers)* s-1.
!
            THmix(i,j,k,itrc)=THmix(i,j,k,itrc)*cff1/Hz(i,j,k)
            Trate(i,j,k,itrc)=Trate(i,j,k,itrc)*cff1/Hz(i,j,k)
          enddo
        enddo
      enddo 
!
! Compute tracer diagnostics averaged over the MLD
!
#  ifdef DIAGNOSTICS_TS_MLD
       do j=Jstr,Jend
         do i=Istr,Iend
            THmix_mld(i,j,itrc)=0.
            Trate_mld(i,j,itrc)=0.
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
     &                           THmix(i,j,k,itrc)*cff
             Trate_mld(i,j,itrc)=Trate_mld(i,j,itrc)+
     &                           Trate(i,j,k,itrc)*cff
           enddo
         enddo
       enddo
#  endif
# endif /* DIAGNOSTICS_TS */
!------------------------------------------------------------------
#  if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
        call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                          t(START_2D_ARRAY,1,nnew,itrc))
#  endif
!
       return
      end

#ifndef CHILD_SPG
# define CHILD_SPG
# ifdef AGRIF
#  include "t3dmix_S.F"
# endif
# undef CHILD_SPG
#endif  /* !CHILD_SPG */
