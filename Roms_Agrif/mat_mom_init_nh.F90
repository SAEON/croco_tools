#include "cppdefs.h"
#ifdef NBQ

      subroutine mat_mom_init_nh 
!*******************************************************************
!*******************************************************************
!*******************************************************************
!      Matrix MOM for momentum equations: initialization
!*******************************************************************
!*******************************************************************
!*******************************************************************

      use module_nh
      use module_nbq       ! #NBQ#

      implicit none

# include "param_F90.h"
# include "scalars_F90.h"
# include "grid.h"
# include "nbq.h"

      integer :: l1_m,i,j,k,i_m

!*******************************************************************
!     Various Initializations:
!*******************************************************************

      nzmimp_nbq   = 1
      nzmom_nh     = 1
      nindkun_nbq  = 0
      neqmimp_nbq = 0
      
      momi_nh = 1
      momj_nh = 1
      momv_nh = 0.

      mimpj_nbq = 1
      mimpv_nbq = 0.

!*******************************************************************
!     Momentum Equation: X-direction
!******************************************************************* 
      
#ifdef OBC_WEST
!-------------------------------------------------------------------
!     Boundary condition (istru_nh-1):
!-------------------------------------------------------------------
      do l_nh=nequ_nh(1)+1,nequ_nh(2)
         i = l2imom_nh(l_nh)
         j = l2jmom_nh(l_nh)
         k = l2kmom_nh(l_nh)
         momi_nh  (l_nh)      = nzmom_nh 
         momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k)
         nzmom_nh             = nzmom_nh + mijk2lq_nh(i,j,k) 
      enddo
#endif

!-------------------------------------------------------------------
!     Inner domain, all layers: (i,j,k)
!-------------------------------------------------------------------
      do i_m=0,2
      do l_nh=nequ_nh(2+i_m)+1,nequ_nh(3+i_m)
         i = l2imom_nh(l_nh)
         j = l2jmom_nh(l_nh)
         k = l2kmom_nh(l_nh)
         momi_nh  (l_nh)     = nzmom_nh 

!-----------------------------
!.......point p(i,j,k+1):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k+1)
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i,j,k+1)

!-----------------------------
!.......point p(i,j,k):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k)
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i,j,k)

!-----------------------------
!.......point p(i,j,k-1):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k-1)
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i,j,k-1)

!-----------------------------
!.......point p(i-1,j,k+1):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i-1,j,k+1)
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i-1,j,k+1)

!-----------------------------
!.......point p(i-1,j,k):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i-1,j,k)
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i-1,j,k)

!-----------------------------
!.......point p(i-1,j,k-1):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i-1,j,k-1)
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i-1,j,k-1) 
      enddo 
      enddo 

#ifdef OBC_EAST
!-------------------------------------------------------------------
!.....Boundary condition (iendu_nh+1):
!-------------------------------------------------------------------
      do l_nh=nequ_nh(5)+1,nequ_nh(6)
         i = l2imom_nh(l_nh)
         j = l2jmom_nh(l_nh)
         k = l2kmom_nh(l_nh)
         momi_nh  (l_nh)     = nzmom_nh 
         momj_nh(nzmom_nh)   = ijk2lq_nh (i-1,j,k)
         nzmom_nh            = nzmom_nh + mijk2lq_nh(i-1,j,k) 
      enddo
#endif

#ifdef MPI
!-------------------------------------------------------------------
!.....MOM matrix structure when points are added at the eastearn boundary!
!-------------------------------------------------------------------
      momi_nh(nequ_nh(6)+1:nequ_nh(7))=nzmom_nh
#endif

!*******************************************************************
!     Momentum Equation: Y-direction
!******************************************************************* 

#ifdef OBC_SOUTH
!-------------------------------------------------------------------
!     Boundary condition (jstrv_nh-1):
!-------------------------------------------------------------------
      do l_nh=neqv_nh(1)+1,neqv_nh(2)
         i = l2imom_nh(l_nh)
         j = l2jmom_nh(l_nh)
         k = l2kmom_nh(l_nh)
        momi_nh  (l_nh)     = nzmom_nh
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k)
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i,j,k) 
      enddo
#endif
    
!-------------------------------------------------------------------
!     Inner domain, all layers: (i,j,k)
!-------------------------------------------------------------------
      do i_m=0,2
      do l_nh=neqv_nh(2+i_m)+1,neqv_nh(3+i_m)
         i = l2imom_nh(l_nh)
         j = l2jmom_nh(l_nh)
         k = l2kmom_nh(l_nh)
         momi_nh  (l_nh)     = nzmom_nh 

!-----------------------------
!.......point p(i,j,k+1):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k+1)
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i,j,k+1) 

!-----------------------------
!.......point p(i,j,k):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k)
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i,j,k) 

!-----------------------------
!.......point p(i,j,k-1):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k-1)
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i,j,k-1) 

!-----------------------------
!.......point p(i,j-1,k+1):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j-1,k+1)
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i,j-1,k+1) 

!-----------------------------
!.......point p(i,j-1,k):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j-1,k)
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i,j-1,k) 

!-----------------------------
!.......point p(i,j-1,k-1):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j-1,k-1)
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i,j-1,k-1) 
       enddo
       enddo
      
#ifdef OBC_NORTH
!-------------------------------------------------------------------
!.....Boundary condition (jendv_nh):
!-------------------------------------------------------------------
      do l_nh=neqv_nh(5)+1,neqv_nh(6)
         i = l2imom_nh(l_nh)
         j = l2jmom_nh(l_nh)
         k = l2kmom_nh(l_nh)
         momi_nh  (l_nh)      = nzmom_nh 
         momj_nh(nzmom_nh)    = ijk2lq_nh (i,j-1,k)
         nzmom_nh             = nzmom_nh + mijk2lq_nh(i,j-1,k) 
       enddo
#endif

#ifdef MPI
!-------------------------------------------------------------------
!.....MOM matrix structure when points are added at the northern boundary !
!-------------------------------------------------------------------
      momi_nh(neqv_nh(6)+1:neqw_nh(1))=nzmom_nh
#endif

!*******************************************************************
! Momentum Equation: Z-direction
!******************************************************************* 
      do l_nh=neqw_nh(1)+1,neqw_nh(2)

        i = l2imom_nh(l_nh)
        j = l2jmom_nh(l_nh)
        k = l2kmom_nh(l_nh)
        momi_nh  (l_nh)     = nzmom_nh

!-----------------------------
!.......point p(i,j,k+1):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k+1)
        momv_nh(nzmom_nh)    = -1. * float(mijk2lq_nh(i,j,k+1)) &
                                   * float(mijk2lq_nh(i,j,k))
        nzmom_nh             = nzmom_nh  + mijk2lq_nh(i,j,k+1)  &
                                   * float(mijk2lq_nh(i,j,k))

!-----------------------------
!.......point p(i,j,k):
!-----------------------------
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k)
        momv_nh(nzmom_nh)    = 1. * float(mijk2lq_nh(i,j,k))
        nzmom_nh             = nzmom_nh + mijk2lq_nh(i,j,k) 

      enddo

#ifdef MPI
!-------------------------------------------------------------------
!.....MOM matrix structure when points are added at the northern boundary !
!-------------------------------------------------------------------
      momi_nh(neqw_nh(2)+1:neqw_nh(3))=nzmom_nh
#endif


      if (ifl_imp_nbq.eq.1) then
!*******************************************************************
!.......Partie implicite:
!*******************************************************************

      neqmimp_nbq = 0

!-------------------------------------------------------------------
!     Inner domain, bottom layer: (i,j,k=0)
!-------------------------------------------------------------------

     do l_nh=neqmom_nh(1)+neqmom_nh(2)+1,neqmom_nh(0)
!!      do l_nh=neqw_nh(1)+1,neqw_nh(2)

        i = l2imom_nh(l_nh)
        j = l2jmom_nh(l_nh)
        k = l2kmom_nh(l_nh)

!-----------------------------
        if (k.eq.0) then
!-----------------------------
        neqmimp_nbq             = neqmimp_nbq  + 1
        mimpi_nbq (neqmimp_nbq) = nzmimp_nbq

        mimpj_nbq(nzmimp_nbq)   = ijk2lq_nh (i,j,k+1)
        mimpv_nbq(nzmimp_nbq)   = 0.
        nzmimp_nbq              = nzmimp_nbq + mijk2lq_nh(i,j,k+1)
        nindkun_nbq             = nindkun_nbq +1
        indkun_nbq(nindkun_nbq) = neqmimp_nbq

!        mimpj_nbq(nzmimp_nbq)   = ijk2lq_nh (i,j,k)
!        mimpv_nbq(nzmimp_nbq)   = 0.
!        nzmimp_nbq              = nzmimp_nbq + mijk2lq_nh(i,j,k)
!-----------------------------
        else
!-----------------------------
        neqmimp_nbq             = neqmimp_nbq  + 1
        mimpi_nbq (neqmimp_nbq) = nzmimp_nbq

        mimpj_nbq(nzmimp_nbq)   = ijk2lq_nh (i,j,k+1)
        mimpv_nbq(nzmimp_nbq)   = - ( soundspeed_nbq**2 - visc2_nbq ) * dtnbq 
        nzmimp_nbq              = nzmimp_nbq + mijk2lq_nh(i,j,k+1)

        mimpj_nbq(nzmimp_nbq)   = ijk2lq_nh (i,j,k)
        mimpv_nbq(nzmimp_nbq)   =   ( soundspeed_nbq**2 - visc2_nbq ) * dtnbq 
        nzmimp_nbq              = nzmimp_nbq + mijk2lq_nh(i,j,k)
!-----------------------------
       endif
!-----------------------------

      enddo
      endif


!*******************************************************************
!.....Correction temporelle:
!XA   http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages/Couplage_Numerique.htm
!     on stocke la matrice qui sert dans l equation pour rho_nbq (M_corr ! dans le document)
!     coherence car c est une matrice qui va etre multipliee par un rho
!******************************************************************* 
      neqcorrt_nbq=neqmom_nh(0) 

#ifdef NBQ_DRHODT
      do l_nh = 1,neqcont_nh
       i = l2iq_nh (l_nh)
       j = l2jq_nh (l_nh)
       k = l2kq_nh (l_nh)

!......caracteristiques de l equation:
       neqcorrt_nbq            = neqcorrt_nbq+1
       ijk2lmom_nh(i,j,k,4)    = neqcorrt_nbq
       mijk2lmom_nh(i,j,k,4)   = 1
       l2imom_nh(neqcorrt_nbq) = i
       l2jmom_nh(neqcorrt_nbq) = j
       l2kmom_nh(neqcorrt_nbq) = k
       momi_nh  (neqcorrt_nbq) = nzmom_nh

!......Point rh(i,j,k+1)
       momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,min(N,k+1))
       nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i,j,min(N,k+1)))
 
!......Point rh(i,j,k-1)
       momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,max(1,k-1))
       nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i,j,max(1,k-1)))

      enddo
#endif

!.....End of last line:
      momi_nh (neqcorrt_nbq+1) = nzmom_nh 
      mimpi_nbq(neqmimp_nbq+1) = nzmimp_nbq    ! Matrice implicite
      
!.....Tests matrix size:
      if (neqmom_nh(0).gt.nmv_nh) then
         write (6,*) 'nmv_nh trop petit!'
         stop
      endif

      if (nzmom_nh.gt.nmmom_nh) then
         write (6,*) 'nmmom_nh trop petit!'
         stop
      endif

      return
      end subroutine mat_mom_init_nh
#else
      subroutine mat_mom_init_empty
      return
      end 
#endif
