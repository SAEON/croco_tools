! $Id$
!======================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
! 
! ROMS_AGRIF website : http://www.romsagrif.org
!======================================================================
      subroutine plot
      implicit none
#include "cppdefs.h"
#ifdef NCARG
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "mixing.h"
# include "scalars.h"

      character daylabel*11, label*6
      real*4 daysize, size, angle0, cntr0, cntr1
      parameter (daysize=0.0175, size=0.012, angle0=0., cntr0=0.,
     &                                                  cntr1=-1.)
      integer white, grey, red_violet, violet, blue, light_blue,
     &        grass_green, green, yellow_green, yellow, orange, red
      parameter (white=1, grey=21, red_violet=32, violet=34,
     &           blue=37, light_blue=44, grass_green=47, green=49,
     &           yellow_green=52,  yellow=55,  orange=59,  red=62)

      real*4 VL1,VR1, VL2,VR2, VL3,VR3, VB,VT, WL,WR,WB,WT
# ifdef BIOLOGY
     &                                             , VL4,VR4
      parameter (VL1=0.01,  VR1=0.20,    VL2=0.21,  VR2=0.40,
     &           VL3=0.41,  VR3=0.60,    VL4=0.61,  VR4=0.99,
     &                                              WB=-120.,
# else
      parameter (VL1=0.01,  VR1=0.32,    VL2=0.34,  VR2=0.66,
     &                                   VL3=0.67,  VR3=0.99,
     &                                               WB=-80.,
# endif
     &   VB=0.075, VT=0.9875, WL=0., WR=1.0,  WT=0.)

      real Umin,Umax,cffM, Tmin,Tmax,cffT,  Smin,Smax,cffS,
     &                                         Akmax,cffA, cff
      parameter (Umin=-0.09,  Umax=0.02,  cffM=1./(Umax-Umin),
     &           Tmin=9.,     Tmax=19.,   cffT=1./(Tmax-Tmin),
     &           Smin=32.,    Smax=35., cffS=1./(Smax-Smin),
c    &           Akmax=0.03,                    cffA=1./Akmax)
     &           Akmax=0.01,   cffA=1./Akmax)
# ifdef BIOLOGY
      real maxNO3,cffNO3, maxNH4,cffNH4,   maxPhyt,cffPhyt,
     &     maxZoo,cffZoo, maxSDet,cffSDet, maxLDet,cffLDet,
     &                                     maxChla,cffChla
      parameter (maxNO3=15., maxPhyt=1.0,  maxZoo=0.75,
     &           maxNH4=3.,  maxChla=2.5,  maxSDet=2.0,
     &                                     maxLDet=0.5,

     &           cffNO3=1./maxNO3,     cffNH4=1./maxNH4,
     &           cffPhyt=1./maxPhyt,   cffZoo=1./maxZoo,
     &           cffSDet=1./maxSDet,   cffLDet=1./maxLDet,
     &                                 cffChla=1./maxChla)
# endif



      integer k,k_r,k_w,N_r,N_w
      real*4 z(0:N), x(0:N)
!-
      call set (VL1,VR1,VB,VT, WL,WR,WB,WT, 1)
      call gsclip(0)
      call gsplci(white)
      call line (WL,WT, WR,WT)
      x(0)=0.5*(WL+WR)
      x(1)=-cffM*Umin
      call line (x(1),WB, x(1),WT)
      x(1)=WL
      x(2)=WL+0.025*(WR-WL)
      do k=N-1,1,-1
        z(k)=z_w(k)
        if(z(k).gt.WB) call line (x(1),z(k), x(2),z(k))
      enddo

      cff=WB/4.
      if (cff.lt.25.) then
        cff=25.
      elseif (cff.lt.50.) then
        cff=50.
      elseif (cff.lt.100.) then
        cff=100.
      endif
 
      do k=1,int(2500./cff)
        z(0)=-25.*k
        if (z(0).gt.WB) then
          write(label,'(I4,1x,A1)') int(z(0)),'m'
          call plchhq(WL,z(0),label, size,angle0,cntr1)
        endif
      enddo

      k_r=N
  1    continue
       if (k_r.gt.0 .and. z_r(k_r).gt.WB) then
         k_r=k_r-1
         goto 1 
       endif
      k_w=k_r
      if (k_w.ge.0 .and. z_w(k_w).gt.WB) k_w=k_w-1
      N_r=N-k_r+1
      N_w=N-k_w+1

      call gsplci(blue)
      call plot_scale ('U,V', Umin,Umax, 0.00,WB,WT)
      do k=k_r,N
        z(k)=z_r(k)
        x(k)=cffM*(u(k,nnew)-Umin)
      enddo
      call curved (x(k_r),z(k_r),N_r)

      call gsplci(green)
      do k=k_r,N
        x(k)=cffM*(v(k,nnew)-Umin)
      enddo
      call curved (x(k_r),z(k_r),N_r)

      call set (VL2,VR2,VB,VT, WL,WR,WB,WT, 1)
      call gsclip(0)
      call gsplci(white)
      call line (WL,WT, WR,WT)
      call line (WL,WB, WL,WT)

      x(0)=0.5*(WL+WR)
      z(0)=WB-0.025*(WT-WB)
      write(daylabel,'(A4,F7.2)')  'day=',tdays
      write(*,*), 'Plotting ',daylabel
      call plchhq (x(0),z(0),daylabel, daysize,angle0,cntr0)

      call gsplci(red)
      call plot_scale ('Tem', Tmin,Tmax, 0.08,WB,WT)
      do k=k_r,N
        x(k)=cffT*(t(k,nnew,itemp)-Tmin)
      enddo
      call curved (x(k_r),z(k_r),N_r)

      call gsplci(orange)
      call plot_scale ('Sal', Smin,Smax, 0.02,WB,WT)
      do k=k_r,N
        x(k)=cffS*(t(k,nnew,isalt)-Smin)
      enddo
      call curved (x(k_r),z(k_r),N_r)
!--
      call set (VL3,VR3,VB,VT, WL,WR,WB,WT, 1)
      call gsclip(0)
      call gsplci(white)
      call line (WL,WT, WR,WT)
      call line (WL,WB, WL,WT)

# ifdef LMD_KPP
      call gsplci(yellow)
      z(0)=z_bl
      x(0)=WL+0.75*(WR-WL)
      call line (WL,z(0),x(0),z(0))
      x(0)=WL+0.8*(WR-WL)
      call plchhq(x(0),z(0),'hbl', size,angle0,cntr1)
# endif

      call gsplci(light_blue)
      call plot_scale ('Kv', 0.,Akmax, 0.08,WB,WT)
      do k=k_w,N
        z(k)=z_w(k)
        x(k)=cffA*Kv(k)
      enddo
      call curved (x(k_r),z(k_r),N_r)
/*
      do k=N-1,k_w,-3
        call line (x(k+1),z(k+1), x(k),z(k))
      enddo

      call gsplci(red)
      call plot_scale ('Akt', 0.,Akmax, 0.04,WB,WT)
      do k=k_w,N
        x(k)=cffA* Akt(k,itemp)
      enddo
      do k=N-2,k_w,-3
        call line (x(k+1),z(k+1), x(k),z(k))
      enddo

      call gsplci(orange)
      call plot_scale ('Aks', 0.,Akmax, 0.0,WB,WT)
      do k=k_w,N
        x(k)=cffA* Akt(k,isalt)
      enddo
      do k=N-3,k_w,-3
        call line (x(k+1),z(k+1), x(k),z(k))
      enddo
*/

# ifdef BIOLOGY
      call set (VL4,VR4,VB,VT, WL,WR,WB,WT, 1)
      call gsclip(0)
      call gsplci(white)
      call line (WL,WT, WR,WT)
      call line (WL,WB, WL,WT)

      call gsplci(violet)
      call plot_scale ('NO3', 0.,maxNO3, 0.15,WB,WT)
      do k=k_r,N
        z(k)=z_r(k)
        x(k)=cffNO3*t(k,nnew,iNO3_)
      enddo
      call curved (x(k_r),z(k_r),N_r)
      
      call gsplci(light_blue)
      call plot_scale ('NH4', 0.,maxNH4, 0.12,WB,WT)
      do k=k_r,N
        x(k)=cffNH4*t(k,nnew,iNH4_)
      enddo
      call curved (x(k_r),z(k_r),N_r)

      call gsplci(yellow_green)
      call plot_scale ('Phy', 0.,maxPhyt, 0.09,WB,WT)
      do k=k_r,N
        x(k)=cffPhyt*t(k,nnew,iPhyt)
      enddo
      call curved (x(k_r),z(k_r),N_r)

      call gsplci(grass_green)
      call plot_scale ('Chl', 0.,maxChla, 0.06,WB,WT)
      do k=k_r,N
        x(k)=cffChla*t(k,nnew,iChla)
      enddo
      call curved (x(k_r),z(k_r),N_r)


      call gsplci(red)
      call plot_scale ('Zoo', 0.,maxZoo, 0.03,WB,WT)
      do k=k_r,N
        x(k)=cffZoo*t(k,nnew,iZoo_)
      enddo
      call curved (x(k_r),z(k_r),N_r)

      call gsplci(orange)
      call plot_scale ('SmD', 0.,maxSDet, 0.00,WB,WT)
      do k=k_r,N
        x(k)=cffSDet*t(k,nnew,iSDet)
      enddo
      call curved (x(k_r),z(k_r),N_r)

      call gsplci(grey)
      call plot_scale ('LrD', 0.,maxLDet, -0.03,WB,WT)
      do k=k_r,N
        x(k)=cffLDet*t(k,nnew,iLDet)
      enddo
      call curved (x(k_r),z(k_r),N_r)

# endif

      call frame
      return
      end

      subroutine plot_scale (title, LFT,RHT,FR, WB,WT)
      implicit none
      integer i,i1,i2, nlab
      character title*3, label*5
      real LFT,RHT,FR, stride,cff,ticks 
      real*4 WB,WT,x1,x2,z1,z2,z3, labsize,titsize,angle0,cntr0,cntr1
      parameter (titsize=0.015, labsize=0.0085, angle0=0., cntr0=0.,
     &                                                     cntr1=-1.)
      z1=WB+FR*(WT-WB)
      z2=z1+0.01*(WT-WB)
      z3=z2+0.01*(WT-WB)

      x1=0.
      x2=1.
      call line (x1,z1, x2,z1)
      x2=0.8
      call plchhq (x2,z3,title, titsize,angle0,cntr1)

      cff=0.1
      x1=RHT-LFT
   1  continue
       if (x1.lt.1.) then
         x1=x1*10.
         cff=cff*0.1
         goto 1
       endif
   2  continue
       if (x1.gt.10.) then
         x1=x1*0.1
         cff=cff*10.
         goto 2 
       endif
      i=int(x1)
   3  continue
       stride=cff*i
       ticks=(RHT-LFT)/stride
       if (ticks.gt.4.) then
         i=i+1
         if (i.gt.5 .and. i.lt.10) i=10
         if (i.gt.10 .and. i.lt.15) i=15
         if (i.gt.15 .and. i.lt.20) i=20
         if (i.gt.20 .and. i.lt.20) i=25
         goto 3
       endif

      i1=LFT/stride
      if (stride*i1.lt.LFT) i1=i1+1
      i2=RHT/stride
      if (stride*i2.gt.RHT) i2=i2-1

      cff=1./(RHT-LFT)
      nlab=i2-i1+1
      do i=i1,i2
        x1=stride*i
        x2=cff*(x1-LFT)
        write(label,'(F5.2)') x1
        call line (x2,z1, x2,z2)
        if (x2.gt.0.05 .and. x2.lt.0.7) call plchhq
     &           (x2,z3,label, labsize,angle0,cntr0)
      enddo

#endif /* NCARG */
      return
      end 
