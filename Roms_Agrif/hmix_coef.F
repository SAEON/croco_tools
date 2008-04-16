!
! $Id: visc_rescale.F,v 1.4 2005/05/11 15:08:29 pmarches Exp $
!
#include "cppdefs.h"
#if defined VIS_GRID || defined DIF_GRID ||\
    defined VIS_COEF_3D || defined DIF_COEF_3D

      subroutine hmix_coef (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      call hmix_coef_tile (Istr,Iend,Jstr,Jend,A2d(1,1,trd))
      return
      end
!
      subroutine hmix_coef_tile (Istr,Iend,Jstr,Jend, grd_scale)
!
!==================================================================
!
! Compute or rescale horizontal viscosity and mixing coefficients
!
! Patrick Marchesiello and Pierrick Penven, IRD 2007
!
!==================================================================
!

      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k,itrc
      real grd_scale(PRIVATE_2D_SCRATCH_ARRAY)
      real cff, surf, defrate, horcon, 
     &     uscale, cff_peclet, diff_clip
      parameter (uscale = 0.1, horcon=0.1,
     &           cff_peclet=1./12.) 
# include "param.h"
# include "grid.h"
# include "mixing.h"
# include "ocean3d.h"
# include "scalars.h"
!
# include "compute_auxiliary_bounds.h"
!
# define FULL_RESCALE

      if (iic.eq.0) then

# if defined VIS_GRID || defined DIF_GRID
!
!----------------------------------------------------------------
! Compute or rescale permanent horizontal viscosity and mixing 
! coefficients according to the local grid box size. The maximum 
! grid size "grdmax" is determined in metrics and it is assumed 
! known in this subroutine.
!
! Full estimation (FULL_RESCALE) is done using:
! A = 1/12 * uscale * grd_scale      harmonic
! B = 1/12 * uscale * grd_scale**3   bi-harmonic
!
! see Holland et al. (1998); Webb et al. (1998);
!     Griffies et al. (2000ab); Lee et al. (2002)
!-----------------------------------------------------------------
!
        do j=JstrR,JendR
          do i=IstrR,IendR
            grd_scale(i,j)=1./sqrt(pm(i,j)*pn(i,j))
          enddo
        enddo

#  ifdef VIS_GRID
#   ifdef UV_VIS2
#    ifdef FULL_RESCALE
        visc2=cff_peclet*uscale*grdmax
#    endif
        cff=visc2/grdmax
        do j=JstrR,JendR
          do i=IstrR,IendR
            visc2_r(i,j)=cff*grd_scale(i,j)
          enddo
        enddo
        cff=0.25*cff
        do j=Jstr,JendR
          do i=Istr,IendR
            visc2_p(i,j)=cff*(grd_scale(i,j  )+grd_scale(i-1,j  )+
     &                        grd_scale(i,j-1)+grd_scale(i-1,j-1))
          enddo
        enddo
#   endif
#   ifdef UV_VIS4
#    ifdef FULL_RESCALE
        visc4=cff_peclet*uscale*grdmax**3
#    endif
        cff=visc4/(grdmax**3)
        do j=JstrR,JendR
          do i=IstrR,IendR
            visc4_r(i,j)=cff*grd_scale(i,j)**3
          enddo
        enddo
        cff=0.25*cff
        do j=Jstr,JendR
          do i=Istr,IendR
            visc4_p(i,j)=cff*(grd_scale(i,j  )**3+grd_scale(i-1,j  )**3+
     &                        grd_scale(i,j-1)**3+grd_scale(i-1,j-1)**3)
          enddo
        enddo
#   endif
#  endif
#  ifdef DIF_GRID
#   ifdef TS_DIF2
        do itrc=1,NT
#    ifdef FULL_RESCALE
          tnu2(itrc)=cff_peclet*uscale*grdmax
#    endif
          cff=tnu2(itrc)/grdmax
          do j=JstrR,JendR
            do i=IstrR,IendR
              diff2(i,j,itrc)=cff*grd_scale(i,j)
            enddo
          enddo
        enddo
#   endif
#   ifdef TS_DIF4
        do itrc=1,NT
#    ifdef FULL_RESCALE
          tnu4(itrc)=cff_peclet*uscale*grdmax**3
#    endif
          cff=tnu4(itrc)/(grdmax**3)
          do j=JstrR,JendR
            do i=IstrR,IendR
              diff4(i,j,itrc)=cff*grd_scale(i,j)**3
            enddo
          enddo
        enddo
#   endif
#  endif

# endif /* VIS_GRID || DIF_GRID */

       else       ! iic.eq.0

# if defined VIS_COEF_3D || defined DIF_COEF_3D

#  if defined VIS_SMAGO || defined DIF_SMAGO || defined UV_SPLIT_UP3
!
!---------------------------------------------------------------
!  Smagorinsky diffusion coefficient
!
!  For Laplacian diffusion, the POM formulation is used:
!    A = CA*DX*DY*DEFRATE
!        with DEFRATE=sqrt[du/dx^2 +dvdy^2 + 0.5(dvdx+dudy)^2]
!        DEFRATE is the deformation rate
!
!  For Biharmonic diffusion, Griffies et al (2000) formulation
!  is used:
!
!    B = CB*(DX*DY)**2*DEFRATE
!   where CA is a constant parameter = 1/12 (Peclet constraint)
!
!---------------------------------------------------------------
!
!  Mixing at RHO points
!
      diff_clip=1.e-2*grdmax**3
      do k=1,N
        do j=JstrV-1,Jend
          do i=IstrU-1,Iend
            surf=(1./(pm(i,j)*pn(i,j)))
            defrate=sqrt( ((u(i+1,j,k,nrhs)-u(i,j,k,nrhs))*pm(i,j))**2
     &                   +((v(i,j+1,k,nrhs)-v(i,j,k,nrhs))*pn(i,j))**2
     &               +0.5*(0.25*pn(i,j)*(
     &                     u(i,j+1,k,nrhs)+u(i+1,j+1,k,nrhs)
     &                    -u(i,j-1,k,nrhs)-u(i+1,j-1,k,nrhs))
     &                   + 0.25*pm(i,j)*(
     &                     v(i+1,j,k,nrhs)+v(i+1,j+1,k,nrhs)
     &                    -v(i-1,j,k,nrhs)-v(i-1,j+1,k,nrhs)) )**2)

#   ifdef VIS_SMAGO
#    ifdef UV_VIS2
            visc3d_r(i,j,k)=horcon*surf*defrate+visc2_r(i,j)
#    elif defined UV_VIS4
            visc3d_r(i,j,k)=cff_peclet*surf**2*defrate+visc4_r(i,j)
#     ifdef MIX_GP_UV
            visc3d_r(i,j,k)=min(visc3d_r(i,j,k),diff_clip)
#     endif
#    endif
#   elif defined UV_SPLIT_UP3
            viscU_r(i,j,k)=cff_peclet*surf**2*max( defrate,
     &      0.5*abs(u(i,j,k,nrhs)+u(i+1,j,k,nrhs))*pm(i,j) )+visc4_r(i,j)
            viscV_r(i,j,k)=cff_peclet*surf**2*max( defrate,
     &      0.5*abs(v(i,j,k,nrhs)+v(i,j+1,k,nrhs))*pn(i,j) )+visc4_r(i,j)
#    ifdef MIX_GP_UV
            viscU_r(i,j,k)=min(viscU_r(i,j,k),diff_clip)
            viscV_r(i,j,k)=min(viscV_r(i,j,k),diff_clip)
#    endif
            visc3d_r(i,j,k)=sqrt(viscU_r(i,j,k)**2+viscV_r(i,j,k)**2)
#   endif

#   ifdef DIF_SMAGO
#    ifdef TS_DIF2
            diff3d(i,j,k)=horcon*surf*defrate+diff2(i,j,itemp) 
#    elif defined TS_DIF4
            diff3d(i,j,k)=cff_peclet*surf**2*defrate+diff4(i,j,itemp)
#     ifdef MIX_GP_TS
            diff3d(i,j,k)=min(diff3d(i,j,k),diff_clip)
#     endif
#    endif
#   endif
          enddo
        enddo
      enddo
#  endif /* VIS_SMAGO || DIF_SMAGO || UV_SPLIT_UP3 */

#  if defined TS_SPLIT_UP3
!
!--------------------------------------------------------------------
! Tracer mixing coefficient based on Peclet number
!
! Results from splitting the 3rd order upstream biased 
! scheme advection as in Holland et al. (1998) and Webb et al. (1998)
!
!  B = 1/12 * abs(u) * grd_scale**3
!
!--------------------------------------------------------------------
!
!!      diff_clip=1.e-2*grdmax**3
      do k=1,N
        do j=Jstr-1,Jend+1
          do i=IstrU-1,Iend+1
            diff3d_u(i,j,k)=cff_peclet*abs(u(i,j,k,nrhs))*om_u(i,j)**3
            diff_clip=.5*(Hz(i,j,k)+Hz(i-1,j,k))/
     &                   (z_r(i,j,k+1)-z_r(i-1,j,k+1))
            diff_clip=min(1.,3*diff_clip**2)*diff3d_u(i,j,k)
            diff3d_u(i,j,k)=diff3d_u(i,j,k)+
     &              0.5*(diff4(i,j,itemp)+diff4(i-1,j,itemp))
#   ifdef MIX_GP_TS
            diff3d_u(i,j,k)=min(diff3d_u(i,j,k),diff_clip)
#   endif
          enddo
        enddo
        do j=JstrV-1,Jend+1
          do i=Istr-1,Iend+1
            diff3d_v(i,j,k)=cff_peclet*abs(v(i,j,k,nrhs))*on_v(i,j)**3
            diff_clip=.5*(Hz(i,j,k)+Hz(i,j-1,k))/
     &                   (z_r(i,j,k+1)-z_r(i,j-1,k+1)) 
            diff_clip=min(1.,3*diff_clip**2)*diff3d_v(i,j,k)
            diff3d_v(i,j,k)=diff3d_v(i,j,k)+
     &                         0.5*(diff4(i,j,itemp)+diff4(i,j-1,itemp))
#   ifdef MIX_GP_TS
            diff3d_v(i,j,k)=min(diff3d_v(i,j,k),diff_clip)
#   endif
          enddo
        enddo
      enddo

      do k=1,N
        do j=JstrV-1,Jend
          do i=IstrU-1,Iend
            diff3d(i,j,k)=0.25*(diff3d_u(i,j,k)+diff3d_u(i+1,j,k)
     &                         +diff3d_v(i,j,k)+diff3d_v(i,j+1,k))
          enddo
        enddo
      enddo
#  endif /* TS_SPLIT_UP3 */

# endif /* VIS_COEF_3D || DIF_COEF_3D */

!
!------------------------------------------------------------
! Boundary conditions
!------------------------------------------------------------
!
# ifdef VIS_COEF_3D
      if (WESTERN_EDGE) then
        do k=1,N
          do j=JstrV-1,Jend
           visc3d_r(Istr-1,j,k)=visc3d_r(Istr,j,k)
#  ifdef UV_SPLIT_UP3
            viscU_r(Istr-1,j,k)=viscU_r(Istr,j,k)
            viscV_r(Istr-1,j,k)=viscV_r(Istr,j,k)
#  endif
          enddo
        enddo
      endif
      if (EASTERN_EDGE) then
        do k=1,N
          do j=JstrV-1,Jend
            visc3d_r(Iend+1,j,k)=visc3d_r(Iend,j,k)
#  ifdef UV_SPLIT_UP3
            viscU_r(Iend+1,j,k)=viscU_r(Iend,j,k)
            viscV_r(Iend+1,j,k)=viscV_r(Iend,j,k)
#  endif
          enddo
        enddo
      endif
      if (SOUTHERN_EDGE) then
        do k=1,N
          do i=IstrU-1,Iend
            visc3d_r(i,Jstr-1,k)=visc3d_r(i,Jstr,k)
#  ifdef UV_SPLIT_UP3
            viscU_r(i,Jstr-1,k)=viscU_r(i,Jstr,k)
            viscV_r(i,Jstr-1,k)=viscV_r(i,Jstr,k)
#  endif
          enddo
        enddo
      endif
      if (NORTHERN_EDGE) then
        do k=1,N
          do i=IstrU-1,Iend
            visc3d_r(i,Jend+1,k)=visc3d_r(i,Jend,k)
#  ifdef UV_SPLIT_UP3
            viscU_r(i,Jend+1,k)=viscU_r(i,Jend,k)
            viscV_r(i,Jend+1,k)=viscV_r(i,Jend,k)
#  endif
          enddo
        enddo
      endif
# endif
# ifdef DIF_COEF_3D
      if (WESTERN_EDGE) then
        do k=1,N
          do j=JstrV-1,Jend
            diff3d(Istr-1,j,k)=diff3d(Istr,j,k)
#  ifdef TS_SPLIT_UP3
            diff3d_u(Istr-1,j,k)=diff3d_u(Istr,j,k)
            diff3d_v(Istr-1,j,k)=diff3d_v(Istr,j,k)
#  endif
          enddo
        enddo
      endif
      if (EASTERN_EDGE) then
        do k=1,N
          do j=JstrV-1,Jend
            diff3d(Iend+1,j,k)=diff3d(Iend,j,k)
#  ifdef TS_SPLIT_UP3
            diff3d_u(Iend+1,j,k)=diff3d_u(Iend,j,k)
            diff3d_v(Iend+1,j,k)=diff3d_v(Iend,j,k)
#  endif
          enddo
        enddo
      endif
      if (SOUTHERN_EDGE) then
        do k=1,N
          do i=IstrU-1,Iend
            diff3d(i,Jstr-1,k)=diff3d(i,Jstr,k)
#  ifdef TS_SPLIT_UP3
            diff3d_u(i,Jstr-1,k)=diff3d_u(i,Jstr,k)
            diff3d_v(i,Jstr-1,k)=diff3d_v(i,Jstr,k)
#  endif
          enddo
        enddo
      endif
      if (NORTHERN_EDGE) then
        do k=1,N
          do i=IstrU-1,Iend
            diff3d(i,Jend+1,k)=diff3d(i,Jend,k)
#  ifdef TS_SPLIT_UP3
            diff3d_u(i,Jend+1,k)=diff3d_u(i,Jend,k)
            diff3d_v(i,Jend+1,k)=diff3d_v(i,Jend,k)
#  endif
          enddo
        enddo
      endif
# endif

!
! Corners
!
# ifdef VIS_COEF_3D
      if (SOUTHERN_EDGE .and. WESTERN_EDGE) then
        do k=1,N
          visc3d_r(Istr-1,Jstr-1,k)=0.5*
     &                          ( visc3d_r(Istr,Jstr-1,k)
     &                           +visc3d_r(Istr-1,Jstr,k))
#  ifdef UV_SPLIT_UP3
          viscU_r(Istr-1,Jstr-1,k)=0.5*
     &                          ( viscU_r(Istr,Jstr-1,k)
     &                           +viscU_r(Istr-1,Jstr,k))
          viscV_r(Istr-1,Jstr-1,k)=0.5*
     &                          ( viscV_r(Istr,Jstr-1,k)
     &                           +viscV_r(Istr-1,Jstr,k))
#  endif
        enddo
      endif
      if (SOUTHERN_EDGE .and. EASTERN_EDGE) then
        do k=1,N
          visc3d_r(Iend+1,Jstr-1,k)=0.5*
     &                          (visc3d_r(Iend,Jstr-1,k)
     &                          +visc3d_r(Iend+1,Jstr,k))
#  ifdef UV_SPLIT_UP3
          viscU_r(Iend+1,Jstr-1,k)=0.5*
     &                          (viscU_r(Iend,Jstr-1,k)
     &                          +viscU_r(Iend+1,Jstr,k))
          viscV_r(Iend+1,Jstr-1,k)=0.5*
     &                          (viscV_r(Iend,Jstr-1,k)
     &                          +viscV_r(Iend+1,Jstr,k))
#  endif
        enddo
      endif
      if (NORTHERN_EDGE .and. WESTERN_EDGE) then
        do k=1,N
          visc3d_r(Istr-1,Jend+1,k)=0.5*
     &                          ( visc3d_r(Istr,Jend+1,k)
     &                           +visc3d_r(Istr-1,Jend,k))
#  ifdef UV_SPLIT_UP3
          viscU_r(Istr-1,Jend+1,k)=0.5*
     &                          ( viscU_r(Istr,Jend+1,k)
     &                           +viscU_r(Istr-1,Jend,k))
          viscV_r(Istr-1,Jend+1,k)=0.5*
     &                          ( viscV_r(Istr,Jend+1,k)
     &                           +viscV_r(Istr-1,Jend,k))
#  endif
        enddo
      endif
      if (NORTHERN_EDGE .and. EASTERN_EDGE) then
        do k=1,N
          visc3d_r(Iend+1,Jend+1,k)=0.5*
     &                          ( visc3d_r(Iend,Jend+1,k)
     &                           +visc3d_r(Iend+1,Jend,k))
#  ifdef UV_SPLIT_UP3
          viscU_r(Iend+1,Jend+1,k)=0.5*
     &                          ( viscU_r(Iend,Jend+1,k)
     &                           +viscU_r(Iend+1,Jend,k))
          viscV_r(Iend+1,Jend+1,k)=0.5*
     &                          ( viscV_r(Iend,Jend+1,k)
     &                           +viscV_r(Iend+1,Jend,k))
#  endif
        enddo
      endif
# endif
# ifdef DIF_COEF_3D
      if (SOUTHERN_EDGE .and. WESTERN_EDGE) then
        do k=1,N
          diff3d(Istr-1,Jstr-1,k)=0.5*
     &                          ( diff3d(Istr,Jstr-1,k)
     &                           +diff3d(Istr-1,Jstr,k))
#  ifdef TS_SPLIT_UP3
          diff3d_u(Istr-1,Jstr-1,k)=0.5*
     &                          ( diff3d_u(Istr,Jstr-1,k)
     &                           +diff3d_u(Istr-1,Jstr,k))
          diff3d_v(Istr-1,Jstr-1,k)=0.5*
     &                          ( diff3d_v(Istr,Jstr-1,k)
     &                           +diff3d_v(Istr-1,Jstr,k))
#  endif
        enddo
      endif
      if (SOUTHERN_EDGE .and. EASTERN_EDGE) then
        do k=1,N
          diff3d(Iend+1,Jstr-1,k)=0.5*
     &                          (diff3d(Iend,Jstr-1,k)
     &                          +diff3d(Iend+1,Jstr,k))
#  ifdef TS_SPLIT_UP3
          diff3d_u(Iend+1,Jstr-1,k)=0.5*
     &                          (diff3d_u(Iend,Jstr-1,k)
     &                          +diff3d_u(Iend+1,Jstr,k))
          diff3d_v(Iend+1,Jstr-1,k)=0.5*
     &                          (diff3d_v(Iend,Jstr-1,k)
     &                          +diff3d_v(Iend+1,Jstr,k))
#  endif
        enddo
      endif
      if (NORTHERN_EDGE .and. WESTERN_EDGE) then
        do k=1,N
          diff3d(Istr-1,Jend+1,k)=0.5*
     &                          ( diff3d(Istr,Jend+1,k)
     &                           +diff3d(Istr-1,Jend,k))
#  ifdef TS_SPLIT_UP3
          diff3d_u(Istr-1,Jend+1,k)=0.5*
     &                          ( diff3d_u(Istr,Jend+1,k)
     &                           +diff3d_u(Istr-1,Jend,k))
          diff3d_v(Istr-1,Jend+1,k)=0.5*
     &                          ( diff3d_v(Istr,Jend+1,k)
     &                           +diff3d_v(Istr-1,Jend,k))
#  endif
        enddo
      endif
      if (NORTHERN_EDGE .and. EASTERN_EDGE) then
        do k=1,N
          diff3d(Iend+1,Jend+1,k)=0.5*
     &                          ( diff3d(Iend,Jend+1,k)
     &                           +diff3d(Iend+1,Jend,k))
#  ifdef TS_SPLIT_UP3
          diff3d_u(Iend+1,Jend+1,k)=0.5*
     &                          ( diff3d_u(Iend,Jend+1,k)
     &                           +diff3d_u(Iend+1,Jend,k))
          diff3d_v(Iend+1,Jend+1,k)=0.5*
     &                          ( diff3d_v(Iend,Jend+1,k)
     &                           +diff3d_v(Iend+1,Jend,k))
#  endif
        enddo
      endif
# endif

# ifdef VIS_COEF_3D
!
!  Viscosity at PSI points
!
      do k=1,N
        do j=Jstr,JendR
          do i=Istr,IendR
#  ifdef UV_SPLIT_UP3
            viscU_p(i,j,k)=0.25*
     &                  ( viscU_r(i,j  ,k)+viscU_r(i-1,j  ,k)
     &                   +viscU_r(i,j-1,k)+viscU_r(i-1,j-1,k))
            viscV_p(i,j,k)=0.25*
     &                  ( viscV_r(i,j  ,k)+viscV_r(i-1,j  ,k)
     &                   +viscV_r(i,j-1,k)+viscV_r(i-1,j-1,k))
#  else
            visc3d_p(i,j,k)=0.25*
     &                  ( visc3d_r(i,j  ,k)+visc3d_r(i-1,j  ,k)
     &                   +visc3d_r(i,j-1,k)+visc3d_r(i-1,j-1,k))
#  endif
          enddo
        enddo
      enddo
# endif

# if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
#  ifdef VIS_COEF_3D
      call exchange_r3d_tile (Istr,Iend,Jstr,Jend,visc3d_r)
#   ifdef UV_SPLIT_UP3
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend,viscU_r)
      call exchange_v3d_tile (Istr,Iend,Jstr,Jend,viscV_r)
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend,viscU_p)
      call exchange_v3d_tile (Istr,Iend,Jstr,Jend,viscV_p)
#   else
      call exchange_p3d_tile (Istr,Iend,Jstr,Jend,visc3d_p)
#   endif
#  endif
#  ifdef DIF_COEF_3D
      call exchange_r3d_tile (Istr,Iend,Jstr,Jend,diff3d)
#   ifdef TS_SPLIT_UP3
      call exchange_u3d_tile (Istr,Iend,Jstr,Jend,diff3d_u)
      call exchange_v3d_tile (Istr,Iend,Jstr,Jend,diff3d_v)
#   endif
#  endif
# endif

      endif      ! iic.eq.0

#else
      subroutine hmix_coef_empty
#endif
      return
      end
