! $Id$
!======================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
! 
! ROMS_AGRIF website : http://www.romsagrif.org
!======================================================================
      subroutine SET_COLOR
      implicit none
#include "cppdefs.h"
#ifdef NCARG
      logical FIRST
      integer i,ii,j,m,n,nn
      parameter (nn=32,m=6)
      real H,S,V,R,G,B,x,CENTER,WIDTH,COEFF
      common /COLORS/ n
!--                                 It must be called ONE TIME ONLY,
!--                                 supressing the following calls...
      data FIRST /.TRUE./
      if(FIRST.eqv..FALSE.) return
      FIRST=.FALSE.

      n=nn
      COEFF=0.5/float(nn-1)
      call GSCR(1,0, .05, .05, .05)
      call GSCR(1,1,  1.,  1.,  1.)
       do j=1,2
         do i=2,nn
              x=COEFF*float(2*i-1-nn)
              if(j.eq.1) then
                 CENTER=112.
                 WIDTH=330.
                 S=0.4+0.6*(4.0*x*x)
                 V=0.6+0.4*(x+0.5)
              else
                 CENTER=128.
                 WIDTH=310.
                 V=1.0
                 S=1.0
              end if
              x=x+0.25*sin(12.56*x)/12.56
              H=CENTER-WIDTH*x
              if(H.lt.0.)   H=H+360.
              if(H.gt.360.) H=H-360.0
              call HSVRGB(H,S,V,R,G,B)
              ii=i+(j-1)*nn 
              call GSCR(1,ii, R,G,B)
            end do
       end do
!--                         Selection of the internal storage index...
      FIRST=.FALSE.
      return
      end
!--------------------------------------------------------------------------
      subroutine COLOR_TABLE
      implicit none
      character MES*1
      integer i,ii,j,jj,jjj,n
      real x,dx,y,dy
      common /COLORS/ n
      y=0.75
      dy=+0.040
      dx=+0.010

      call GSELNT(0)
      call PLCHHQ(0.5,0.8,'THIS IS THE COLOR INDEX TABLE',.025,.0,.0)
      do 2 jj=1,2
        y=y-0.175
        do 2 i=1,n
           jjj=i+n*(jj-1)
           call GSPLCI(jjj)
           x=0.05+0.9*(i-1)/float(n)
           ii=jjj/10
           if(ii.gt.0) then
              write(MES,3) ii
              call PLCHHQ(x+dx,y+dy,MES, 0.015,0.0,-1.0)
              j=jjj-10*ii
           else
              j=jjj
           end if
           write(MES,3) j
           call PLCHHQ(x,y,MES, 0.03,0.0,-1.0)
           x=x+0.003
  2        call PLCHHQ(x,y,MES, 0.03,0.0,-1.0)
  3   format(i1)

      call GSPLCI(1)
      call PLCHHQ(0.05,0.2,'SET OF POLYLINE COLOR = WHITE',.02,.0,-1.)
      call PLCHHQ(0.95,0.125,'SET_COLOR=OK!',.0275,.0,+1.)
      call FRAME

#endif /*NCARG */
      return  
      end

