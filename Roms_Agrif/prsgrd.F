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
#ifdef SOLVE3D

      subroutine prsgrd (tile)
c********************************************************************
c Subroutine prsgrd32AC1 from Alex Shchepetkin
c Density Jacobian (3), Cubic Polynomial fit (2), Alternative 
c formulation (A), compressible (C), Revision 1 (adiabatic differences)
c********************************************************************
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h" 
      trd=omp_get_thread_num()
      call prsgrd_tile (Istr,Iend,Jstr,Jend, 
     &                        A3d(1,1,trd), A3d(1,2,trd), A3d(1,3,trd),
     &                                      A2d(1,1,trd), A2d(1,2,trd),
     &          A2d(1,1,trd), A2d(1,2,trd), A2d(1,3,trd), A2d(1,4,trd))
      return
      end

      subroutine prsgrd_tile (Istr,Iend,Jstr,Jend, ru,rv, P,
     &                                       dR,dZ, FC,dZx,rx,dRx)
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k, imin,imax,jmin,jmax
# include "param.h"
# ifdef DIAGNOSTICS_UV
#  include "diagnostics.h"
# endif
      real ru(PRIVATE_2D_SCRATCH_ARRAY,N),   OneFifth,
     &     rv(PRIVATE_2D_SCRATCH_ARRAY,N),   OneTwelfth,
     &      P(PRIVATE_2D_SCRATCH_ARRAY,N),   epsil,dpth,
     &     dR(PRIVATE_1D_SCRATCH_ARRAY,0:N), cff, GRho,
     &     dZ(PRIVATE_1D_SCRATCH_ARRAY,0:N), cfr, HalfGRho, 
     &     FC(PRIVATE_2D_SCRATCH_ARRAY),
     &    dZx(PRIVATE_2D_SCRATCH_ARRAY),
     &     rx(PRIVATE_2D_SCRATCH_ARRAY),
     &    dRx(PRIVATE_2D_SCRATCH_ARRAY)

      parameter (OneFifth=0.2, OneTwelfth=1./12., epsil=0.) !1.E-10) 
!
#ifdef MASKING
# define SWITCH *
#else
# define SWITCH !
#endif
!
# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
# ifdef INNERSHELF_APG
! Impose alongshore pressure gradient balancing
! a constant cross-shore geostrophic flow UG (m/s)
       real UG
       parameter (UG=0.02)
# endif
!
# include "compute_auxiliary_bounds.h"
!
! Non-conservative Density-Jacobian scheme, based on cubic polynomial
! fits for rho and z_r as functions of nondimensianal coordinates xi,
! eta, and s (basically their respective fortran indices). The cubic
! polynomials are constructed by specifying first derivatives of
! interpolated fields on co-located (non-staggered) grid. These
! derivatives are computed using harmonic (rather that algebraic)
! averaging of elementary differences, which guarantees monotonicity
! of the resultant interpolant. 
!
! In the code below, if CPP-switch SPLIT_EOS is defined, the Equation
! of State (EOS) is assumed to have form 
!
!           rho(T,S,z) = rho1(T,S) + (zeta-z) * qp1(T,S)
!
! where rho1 is potential density at 1 atm and qp1 is compressibility
! coefficient, which does not depend on z (or weakly dependens on z).
! In this case 
!
!          d rho     d rho1              d qp1            d z
!         ------- = -------- + (zeta-z) -------  -  qp1 -------
!          d s,x      d s,x              d s,x          d s,x
!
!                   !<---- adiabatic part ---->|  !<- compress ->|
!
! where the first two terms constitute "adiabatic derivative" of 
! density, which is subject to harmonic averaging, while the last
! term is added in later. This approach quarantees that density
! profile reconstructed by cubic polynomial maintains its positive
! statification in physical sense as long as discrete values of
! density are positively stratified.
!
! This scheme retains exact antisymmetry J(rho,z_r)=-J(z_r,rho)
! [with the exception of harmonic averaging algorithm in the case
! when CPP-switch SPLIT_EOS is defined, see above]. If parameter
! OneFifth (see above) is set to zero, the scheme becomes identical
! to standard Jacobian.
!
! NOTE: This routine is an alternative form of prsgrd32 and it
!       produces results identical to that if its prototype. 
!
! Preliminary step (same for XI- and ETA-components:
!------------ ---- ----- --- --- --- ---------------
!

      GRho=g/rho0
      HalfGRho=0.5*GRho

      do j=JstrV-1,Jend
        do k=1,N-1
          do i=IstrU-1,Iend
            dZ(i,k)=z_r(i,j,k+1)-z_r(i,j,k)
# ifdef SPLIT_EOS 
            dpth=z_w(i,j,N)-0.5*(z_r(i,j,k+1)+z_r(i,j,k))

            dR(i,k)=rho1(i,j,k+1)-rho1(i,j,k)            ! Elementary
     &              +(qp1(i,j,k+1)-qp1(i,j,k))           ! adiabatic
     &                     *dpth*(1.-qp2*dpth)           ! difference
# else
            dR(i,k)=rho(i,j,k+1)-rho(i,j,k)
# endif
          enddo
        enddo
        do i=IstrU-1,Iend
          dR(i,N)=dR(i,N-1)
          dR(i,0)=dR(i,1)
          dZ(i,N)=dZ(i,N-1)
          dZ(i,0)=dZ(i,1)
        enddo
        do k=N,1,-1               !--> irreversible
          do i=IstrU-1,Iend
            cff=2.*dZ(i,k)*dZ(i,k-1)
            dZ(i,k)=cff/(dZ(i,k)+dZ(i,k-1))

            cfr=2.*dR(i,k)*dR(i,k-1)
            if (cfr.gt.epsil) then
              dR(i,k)=cfr/(dR(i,k)+dR(i,k-1))
            else
              dR(i,k)=0.
            endif
# ifdef SPLIT_EOS
            dpth=z_w(i,j,N)-z_r(i,j,k)
            dR(i,k)=dR(i,k)  -qp1(i,j,k)*dZ(i,k)*(1.-2.*qp2*dpth)
# endif
          enddo
        enddo


        do i=IstrU-1,Iend
          P(i,j,N)=g*z_w(i,j,N) + GRho*( rho(i,j,N)
     &       +0.5*(rho(i,j,N)-rho(i,j,N-1))*(z_w(i,j,N)-z_r(i,j,N))
     &          /(z_r(i,j,N)-z_r(i,j,N-1)) )*(z_w(i,j,N)-z_r(i,j,N))
        enddo
        do k=N-1,1,-1
          do i=IstrU-1,Iend
            P(i,j,k)=P(i,j,k+1)+HalfGRho*( (rho(i,j,k+1)+rho(i,j,k))
     &                                     *(z_r(i,j,k+1)-z_r(i,j,k))

     &     -OneFifth*( (dR(i,k+1)-dR(i,k))*( z_r(i,j,k+1)-z_r(i,j,k)
     &                              -OneTwelfth*(dZ(i,k+1)+dZ(i,k)) )

     &                -(dZ(i,k+1)-dZ(i,k))*( rho(i,j,k+1)-rho(i,j,k)
     &                              -OneTwelfth*(dR(i,k+1)+dR(i,k)) )
     &                                                             ))
          enddo
        enddo
      enddo   !<-- j

!
! Compute XI-component of pressure gradient term:
!-------- ------------ -- -------- -------- -----
!
# ifndef EW_PERIODIC
      if (WESTERN_EDGE) then     ! Restrict extended ranges one
        imin=IstrU               ! point inward near the physical
      else                       ! boundary. Note that this version
        imin=IstrU-1             ! of code works in MPI configuration
      endif                      ! too, while a more straightforward
      if (EASTERN_EDGE) then     ! loop range setting
        imax=Iend                ! 
      else                       !   i=max(2,IstrU-1),min(Iend+1,Lm)
        imax=Iend+1              !
      endif                      ! does not.
# else
      imin=Istr-1
      imax=Iend+1
# endif

      do k=N,1,-1
        do j=Jstr,Jend
          do i=imin,imax
            FC(i,j)=(z_r(i,j,k)-z_r(i-1,j,k))
# ifdef MASKING
     &                              *umask(i,j)
# endif
# ifdef SPLIT_EOS
            dpth=0.5*( z_w(i,j,N)+z_w(i-1,j,N)
     &                -z_r(i,j,k)-z_r(i-1,j,k))

            rx(i,j)=( rho1(i,j,k)-rho1(i-1,j,k)          ! Elementary
     &                +(qp1(i,j,k)-qp1(i-1,j,k))         ! adiabatic
     &                     *dpth*(1.-qp2*dpth) )         ! difference
# else
            rx(i,j)=(rho(i,j,k)-rho(i-1,j,k)) 
# endif
# ifdef MASKING
     &                              *umask(i,j)
# endif
          enddo
        enddo

# ifndef EW_PERIODIC
        if (WESTERN_EDGE) then         ! Extrapolate elementary
          do j=Jstr,Jend               ! differences near physical
            FC(imin-1,j)=FC(imin,j)    ! boundaries to compencate.
            rx(imin-1,j)=rx(imin,j)    ! for reduced loop ranges.
          enddo
        endif
        if (EASTERN_EDGE) then
          do j=Jstr,Jend
            FC(imax+1,j)=FC(imax,j)
            rx(imax+1,j)=rx(imax,j)
          enddo
        endif
# endif

        do j=Jstr,Jend
          do i=IstrU-1,Iend
            cff=2.*FC(i,j)*FC(i+1,j)
            if (cff.gt.epsil) then
              dZx(i,j)=cff/(FC(i,j)+FC(i+1,j))
            else
              dZx(i,j)=0.
            endif

            cfr=2.*rx(i,j)*rx(i+1,j)
            if (cfr.gt.epsil) then
              dRx(i,j)=cfr/(rx(i,j)+rx(i+1,j))
            else
              dRx(i,j)=0.
            endif
# ifdef SPLIT_EOS
            dRx(i,j)=dRx(i,j) -qp1(i,j,k)*dZx(i,j)
     &         *(1.-2.*qp2*(z_w(i,j,N)-z_r(i,j,k)))
# endif
          enddo   
        enddo                 !--> discard FC, rx

        do j=Jstr,Jend
          do i=IstrU,Iend
            ru(i,j,k)=0.5*(Hz(i,j,k)+Hz(i-1,j,k))*on_u(i,j)*(
     &      P(i-1,j,k)-P(i,j,k)-HalfGRho*( (rho(i,j,k)+rho(i-1,j,k))
     &                                     *(z_r(i,j,k)-z_r(i-1,j,k))

     &   -OneFifth*( (dRx(i,j)-dRx(i-1,j))*( z_r(i,j,k)-z_r(i-1,j,k)
     &                            -OneTwelfth*(dZx(i,j)+dZx(i-1,j)) ) 

     &              -(dZx(i,j)-dZx(i-1,j))*( rho(i,j,k)-rho(i-1,j,k)
     &                            -OneTwelfth*(dRx(i,j)+dRx(i-1,j)) )
     &                                                            )))
!
# ifdef DIAGNOSTICS_UV
            if (nnew.ne.3) then
              MPrsgrd(i,j,k,1) = ru(i,j,k) SWITCH umask(i,j)
            endif
# endif
          enddo
        enddo
!
! ETA-component of pressure gradient term:
!-------------- -- -------- -------- -----
!
# ifndef NS_PERIODIC
        if (SOUTHERN_EDGE) then
          jmin=JstrV
        else
          jmin=JstrV-1
        endif
        if (NORTHERN_EDGE) then
          jmax=Jend
        else
          jmax=Jend+1
        endif
# else
        jmin=Jstr-1
        jmax=Jend+1
# endif

        do j=jmin,jmax
          do i=Istr,Iend
            FC(i,j)=(z_r(i,j,k)-z_r(i,j-1,k))
# ifdef MASKING
     &                              *vmask(i,j)
# endif
# ifdef SPLIT_EOS
            dpth=0.5*( z_w(i,j,N)+z_w(i,j-1,N)
     &                -z_r(i,j,k)-z_r(i,j-1,k))
            
            rx(i,j)=( rho1(i,j,k)-rho1(i,j-1,k)          ! Elementary
     &                +(qp1(i,j,k)-qp1(i,j-1,k))         ! adiabatic
     &                     *dpth*(1.-qp2*dpth) )         ! difference
# else
            rx(i,j)=(rho(i,j,k)-rho(i,j-1,k))
# endif
# ifdef MASKING
     &                              *vmask(i,j)
# endif
          enddo
        enddo
 
        if (SOUTHERN_EDGE) then
          do i=Istr,Iend
            FC(i,jmin-1)=FC(i,jmin)
            rx(i,jmin-1)=rx(i,jmin)
          enddo
        endif
        if (NORTHERN_EDGE) then
          do i=Istr,Iend
            FC(i,jmax+1)=FC(i,jmax)
            rx(i,jmax+1)=rx(i,jmax)
          enddo
        endif


        do j=JstrV-1,Jend
          do i=Istr,Iend
            cff=2.*FC(i,j)*FC(i,j+1)
            if (cff.gt.epsil) then      
              dZx(i,j)=cff/(FC(i,j)+FC(i,j+1))
            else
              dZx(i,j)=0.
            endif

            cfr=2.*rx(i,j)*rx(i,j+1)
            if (cfr.gt.epsil) then    
              dRx(i,j)=cfr/(rx(i,j)+rx(i,j+1))
            else
              dRx(i,j)=0.
            endif
# ifdef SPLIT_EOS
            dRx(i,j)=dRx(i,j) -qp1(i,j,k)*dZx(i,j)
     &         *(1.-2.*qp2*(z_w(i,j,N)-z_r(i,j,k)))
# endif
          enddo
        enddo                 !--> discard FC, rx

        do j=JstrV,Jend
          do i=Istr,Iend
            rv(i,j,k)=0.5*(Hz(i,j,k)+Hz(i,j-1,k))*om_v(i,j)*(
# ifdef INNERSHELF_APG
     &      on_v(i,j)*f(i,j)*UG +    ! specified alongshore PG
# endif
     &      P(i,j-1,k)-P(i,j,k)-HalfGRho*( (rho(i,j,k)+rho(i,j-1,k))
     &                                     *(z_r(i,j,k)-z_r(i,j-1,k))

     &   -OneFifth*( (dRx(i,j)-dRx(i,j-1))*( z_r(i,j,k)-z_r(i,j-1,k)
     &                            -OneTwelfth*(dZx(i,j)+dZx(i,j-1)) )

     &              -(dZx(i,j)-dZx(i,j-1))*( rho(i,j,k)-rho(i,j-1,k)
     &                            -OneTwelfth*(dRx(i,j)+dRx(i,j-1)) )
     &                                                            )))
!
#  ifdef DIAGNOSTICS_UV
            if (nnew.ne.3) then
              MPrsgrd(i,j,k,2) = rv(i,j,k) SWITCH vmask(i,j)
            endif
#  endif
          enddo
        enddo
      enddo
      return
      end
#else
      subroutine prsgrd_empty
      end
#endif /* SOLVE3D */
