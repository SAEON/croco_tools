!
! $Id: ana_grid.F,v 1.5 2005/10/07 15:32:38 pmarches Exp $
!
#include "cppdefs.h"
#ifdef ANA_GRID
      subroutine ana_grid (tile)
      implicit none
      integer tile
# include "param.h"
# include "compute_tile_bounds.h"
      call ana_grid_tile (Istr,Iend,Jstr,Jend)
      return
      end

      subroutine ana_grid_tile (Istr,Iend,Jstr,Jend)
!
!  Set up model grid using an analytical expressions:
!-------------------------------------------------------------------
!  Output: stored in common blocks, see files "scalars.h" "grid.h"
!
!  xl,el      Physical dimensions of the computational domain 
!                                    [usually measured in  meters];  
!  h          Model bathymetry [meters, positive] at RHO-points.
!  hmin,hmax  Minimum and maximum values of depth of bathymetry [m].
!  f          Coriolis parameter (1/seconds) at RHO-points.
!  pm,pn      Coordinate transformation metric "m" [1/meters]
!                    associated with the differential distances in 
!                    XI- and ETA-directions, both are at RHO-points.
!  xp,xr      XI-coordinates [m] at PSI- and RHO-points.
!  yp,yr      ETA-coordinates [m] at PSI- and RHO-points.
!
# ifdef AGRIF
      use Agrif_Util
# endif
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,imax,jchn
# include "param.h"
# include "grid.h"
# include "scalars.h"
!
! Grid configuration parameters:
!-------------------------------------------------------------------
! Length_XI, Length_ETA  are the physical dimensions of the computational
!                               domain [usually measured in  meters];
! depth      is the maximum depth [meters, positive];
! f0,beta    are coriolis parameters which set up a beta-plane
!                                                     [1/s, 1/(m*s)].
!
      real Length_XI,Length_ETA, depth, f0,beta, 
     &     cff,y, x0,y0,dx,dy, NSUB

!
# include "compute_extended_bounds.h"
!
# if defined BASIN
                     depth=5000.
                     f0=1.E-4 
                     beta=2.E-11
# elif defined CANYON_A || defined CANYON_B
                     depth=4000.
                     f0=1.E-4
                     beta=0.
# elif defined EQUATOR
                     depth=5000.
                     f0=0.
                     beta=2.2829e-11
# elif defined GRAV_ADJ
                     depth=20.
                     f0=0.
                     beta=0.
# elif defined OVERFLOW
                     depth=40.
                     f0=0.
                     beta=0.
# elif defined SEAMOUNT
                     depth=4500.
                     beta=0.
                     f0=1.E-4
# elif defined SHELFRONT
                     depth=1660.
                     f0=1.E-4
                     beta=0.
# elif defined SOLITON
                     depth=1.
                     f0=0.
                     beta=1.
# elif defined UPWELLING
                     depth=150.
                     f0=-8.26E-5
                     beta=0.
# elif defined SED_TEST2
                     depth=10.0    
                     f0=1.E-4
                     beta=0.
# elif defined INNERSHELF
                     depth=5000.
                     f0=5.e-5
                     beta=0.
# elif defined RIVER
                     depth=150.
                     f0=8.26E-5
                     beta=0.
# else
                     depth=???
                     f0=???
                     beta=???
# endif
# ifdef AGRIF
      if (Agrif_Root()) then
# endif
# if defined BASIN
                     Length_XI =3600.0e+3
                     Length_ETA=2800.0e+3
# elif defined CANYON_A || defined CANYON_B
                     Length_XI =128.0e+3
                     Length_ETA=96.0e+3
# elif defined EQUATOR
                     Length_ETA=3200.0E+3
                     Length_XI =4000.0E+3
# elif defined GRAV_ADJ
                     Length_XI =64.0E+3
                     Length_ETA=2.0E+3
# elif defined OVERFLOW
                     Length_XI =2.0E+3
                     Length_ETA=64.0E+3
# elif defined SEAMOUNT
                     Length_XI =512.0e+3
                     Length_ETA=512.0e+3
# elif defined SHELFRONT
                     Length_XI =20.0e+3
                     Length_ETA=200.0e+3
# elif defined SOLITON
                     Length_XI =48.
                     Length_ETA=16.
# elif defined UPWELLING
                     Length_ETA=8.E+4
                     Length_XI =LLm*Length_ETA/MMm
# elif defined SED_TEST2
                     Length_XI =25000.0
                     Length_ETA=2500.0
# elif defined INNERSHELF
                     Length_XI =320.0e+3
                     Length_ETA=15.0e+3
# elif defined RIVER
                     Length_ETA=8.E+4
                     Length_XI =LLm*Length_ETA/MMm
# else
                     Length_XI =???
                     Length_ETA=??? 
# endif
# ifdef AGRIF
      else
!
! Child grid dimension
!
!        Length_XI=AMR_CURGRID%xp%parent_var%var%array2((AMR_ix()-1+
!     &     AMR_CURGRID%nbx)/AMR_coeffrefx,(AMR_iy()-1)/AMR_coeffrefy)-
!     &                          AMR_CURGRID%xp%parent_var%var%array2
!     &        ((AMR_ix()-1)/AMR_coeffrefy,(AMR_iy()-1)/AMR_coeffrefy)
!        Length_ETA=AMR_CURGRID%yp%parent_var%var%array2((AMR_ix()-1)/
!     &      AMR_coeffrefy,(AMR_iy()-1+AMR_CURGRID%nby)/AMR_coeffrefy)-
!     &                          AMR_CURGRID%yp%parent_var%var%array2
!     &        ((AMR_ix()-1)/AMR_coeffrefy,(AMR_iy()-1)/AMR_coeffrefy)
!
        Length_XI=
     &       Agrif_Parent_xp(Agrif_Ix()+Lm/Agrif_IRhox(),Agrif_Iy())
     &      -Agrif_Parent_xp(Agrif_Ix(),Agrif_Iy())
        Length_ETA=
     &       Agrif_Parent_yp(Agrif_Ix(),Agrif_Iy()+Mm/Agrif_IRhoy())
     &      -Agrif_Parent_yp(Agrif_Ix(),Agrif_Iy())
      endif
# endif /* AGRIF */
                                            ! Copy physical dimensions
      xl=Length_XI                          ! of the grid into glabally
      el=Length_ETA                         ! vizible  variables

      dx=Length_XI/float(LLm)               ! Set grid spacings
      dy=Length_ETA/float(MMm)              ! for rectangular grids

# ifdef MPI
      x0=float(ii)*Length_XI/float(NP_XI)   ! Coordinate of south-west
      y0=float(jj)*Length_ETA/float(NP_ETA) ! corner of the PI subdomain
# else
      x0=0.
      y0=0.
# endif
!CR# ifdef EQUATOR
!CR      y0=y0-0.5*el
!CR# endif

CR      write(*,'(4(A3,pe15.9,1x),I3)') 'dx=',dx, 'dy=',dy,
CR     &                                'x0=',x0, 'y0=',y0, mynode
 
                  
      do j=JstrR,JendR
        do i=IstrR,IendR
          xp(i,j)=x0+dx*float(i-1)        ! Setup rectangulag grid:
          xr(i,j)=x0+dx*(float(i-1)+0.5)  ! coordinates (XI,ETA) at
          yp(i,j)=y0+dy*float(j-1)        ! PSI- and RHO-points.
          yr(i,j)=y0+dy*(float(j-1)+0.5)
# ifdef MPI
          yr(i,j)=dy*(float(j+jj*Mm)-0.5)
# endif
# ifdef EQUATOR
          yr(i,j)=yr(i,j)-0.5*el
          yp(i,j)=yp(i,j)-0.5*el
# endif
        enddo
      enddo
!
! Compute coordinate transformation metrics at RHO-points "pm" and
! "pn"  (1/m) associated with the differential distances in XI and
! ETA, respectively.
!
      do j=JstrR,JendR
        do i=IstrR,IendR
          pm(i,j)=1./dx
          pn(i,j)=1./dy
        enddo
      enddo
!
!  Set Coriolis parameter [1/s] at RHO-points.
!
      do j=JstrR,JendR
        do i=IstrR,IendR
#  ifdef EQUATOR
          f(i,j)=f0+beta*yr(i,j)
#  else
          f(i,j)=f0+beta*(yr(i,j)-(0.5*el))
#  endif
        enddo
      enddo   
!
!  Set bathymetry [meters; positive] at RHO-points.
!
# if defined BASIN || defined EQUATOR || defined GRAV_ADJ || defined SOLITON
      do j=JstrR,JendR
        do i=IstrR,IendR
          h(i,j)=depth
        enddo
      enddo
# elif defined CANYON_A || defined CANYON_B
      do j=JstrR,JendR
        do i=IstrR,IendR
          cff=32000.-16000.*(sin(pi*xr(i,j)/Length_XI))**24
          h(i,j)=20.+0.5*(depth-20.)*(1.+tanh((yr(i,j)-cff)/10000.))
        enddo
      enddo
# elif defined OVERFLOW
      do j=JstrR,JendR
        do i=IstrR,IendR
          h(i,j)=20.+0.5*(depth-20.)*(1.+
     &                         tanh((yr(i,j)-40000.)/5000.))
        enddo
      enddo
# elif defined SEAMOUNT
      cff=(1./50.e3)**2
      do j=JstrR,JendR
        do i=IstrR,IendR
          h(i,j)=depth*(1-0.6*exp(-cff*((xr(i,j)-0.5*xl)**2+
     &                                  (yr(i,j)-0.5*el)**2))) 
        enddo
      enddo
# elif defined SHELFRONT
      do j=JstrR,JendR
        do i=IstrR,IendR
          cff=yr(i,j)/1000.
          if (cff.lt.50.) then
            h(i,j)=50.+2.*cff
          elseif (cff.lt.60.) then
            h(i,j)=160.+1.5*(cff-50.)**2-0.1*(cff-60.0)**2
          elseif (cff.lt.100.) then
            h(i,j)=310.+30.*(cff-60.)
          elseif (cff.lt.110.) then
            h(i,j)=1660.-1.5*(cff-110.)**2
          else
            h(i,j)=1660.
          endif
        enddo
      enddo
# elif defined SED_TEST2
      do j=Jstr,Jend
        do i=Istr,Iend
          val1=user(3)            !ocean-side depth
          val2=user(4)            !nearshore depth
          val3=user(5)/3.0        ! ~ width of slope
          h(i,j)=0.5*(val1+
     &     (val1-val2)*TANH((7000.0-xr(i,j))/val3))  
        enddo
      enddo
# elif defined UPWELLING
      do j=JstrR,JendR
#  ifdef MPI
        y=dy*float(j+jj*Mm)
#  else
        y=dy*float(j)
#  endif
        if (y.gt.Length_ETA/2.) y=Length_ETA-y+dy
        cff=min(depth,84.5+66.526*tanh(0.00015*(y-0.125*Length_ETA)))
        do i=IstrR,IendR
          h(i,j)=cff
        enddo
      enddo
# elif defined INNERSHELF
      do j=JstrR,JendR
        do i=IstrR,IendR
          h(i,j)=30.+0.5*1000.*(1+sinh((150.e3-xr(i,j))/40.e3)
     &               /cosh((150.e3-xr(i,j))/40.e3))
        enddo
      enddo
# elif defined RIVER
      do i=IstrR,IendR
# ifdef MPI
        cff=(float(i +ii*Lm)-0.5)/float(LLm)
# else
        cff=(float(i       )-0.5)/float(LLm)
# endif
        if (cff.lt.0.05) then
          h(i,JstrR)=15.
        elseif (cff.lt.0.15) then
          h(i,JstrR)=15.+843.75*(cff-0.05)**2
        elseif (cff.lt.0.85) then
          h(i,JstrR)=15.+168.75*(cff-0.1)
        elseif (cff.lt.0.95) then
          h(i,JstrR)=150.-843.75*(cff-0.95)**2
        else
          h(i,JstrR)=150.
        endif
      enddo
      do j=JstrR+1,JendR
        do i=IstrR,IendR
          h(i,j)=h(i,JstrR)
        enddo
      enddo
# else
      do j=JstrR,JendR
        do i=IstrR,IendR
          h(i,j)=???
        enddo
      enddo
# endif
# ifdef MASKING
#  if defined RIVER
      do j=JstrR,JendR             ! Set mask to all-water status,
        do i=IstrR,IendR           ! then mask out 5-point wide strip
          rmask(i,j)=1.            ! of land on the west, and finally
        enddo                      ! carve a 1-point wide channel
      enddo                        ! through that strip.
      imax=8
      jchn=60
      do j=JstrR,JendR
        do i=IstrR,imax
          rmask(i,j)=0.            ! <-- strip of land
        enddo
      enddo
      j=jchn
        if (j.ge.JstrR .and. j.le.JendR) then
          do i=IstrR+3,imax
            rmask(i,jchn-20)=1.
            rmask(i,jchn)=1.                   ! <-- channel
          enddo
        endif
      do j=jchn-6,jchn
        if (j.ge.JstrR .and. j.le.JendR) then
            rmask(3,j)=1.                      ! <-- channel
        endif
      enddo
#  endif
#endif
#else
      subroutine ana_grid_empty
#endif /* ANA_GRID */
      return
      end

