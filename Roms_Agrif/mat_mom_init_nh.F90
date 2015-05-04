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

      real :: dte_lp
      integer                                                         &
       i1_n                                                           &
      ,i2_n                                                           &
      ,j1_n                                                           &
      ,j2_n                                                           &
      ,i1o_n                                                          &
      ,i2o_n                                                          &
      ,j1o_n                                                          &
      ,j2o_n
      integer :: l1_m,i,j,k,kmax,imax,jmax

#ifdef MPI
#define LOCALLM Lmmpi
#define LOCALMM Mmmpi
#else
#define LOCALLM Lm
#define LOCALMM Mm
#endif    

!*******************************************************************
!     Various Initializations:
!*******************************************************************

      dte_lp=2*dtfast   !CXA
      imax=LOCALLM
      jmax=LOCALMM

      nzmimp_nbq   = 1
      nzmom_nh     = 1
      nindkun_nbq  = 0

      neqmom_nh(0:4) = 0

      neqmimp_nbq = 0
      
      momj_nh = 1
      momv_nh = 0.

      mimpj_nbq = 1
      mimpv_nbq = 0.


!*******************************************************************
!     MPI domain
!*******************************************************************

!......X-Direction:

! CXA  conditions ouvertes doivent etre prises en compte dans la matrice
! CXA  => on augmente i2o_n et on reduit i1o_n pour prendre ces points
!
!      SNBQ
!
!      j1_n = 2
!      if (par%tvoisin(sud).eq.mpi_proc_null.or.ifl_nbq.eq.1) j1_n = 1
!      j2_n = jmax - 1
!      if (par%tvoisin(nord).eq.mpi_proc_null.or.ifl_nbq.eq.1) j2_n = jmax
!      i1o_n = 2
!      if (par%tvoisin(ouest).eq.mpi_proc_null.or.ifl_nbq.eq.1) i1o_n = 1
!      i2o_n = imax
!      if (par%tvoisin(est).eq.mpi_proc_null.or.ifl_nbq.eq.1) i2o_n = imax+1

      i1o_n=0
      i2o_n=LOCALLM + 1
      j1_n=0
      j2_n=LOCALMM+1     

!......Y-Direction:
!
!      SNBQ
!
!      i1_n = 2
!      if (par%tvoisin(ouest).eq.mpi_proc_null.or.ifl_nbq.eq.1) i1_n = 1
!      i2_n = imax - 1
!      if (par%tvoisin(est).eq.mpi_proc_null.or.ifl_nbq.eq.1) i2_n = imax
!      j1o_n = 2
!      if (par%tvoisin(sud).eq.mpi_proc_null.or.ifl_nbq.eq.1) j1o_n = 1
!      j2o_n = jmax

      i1_n=0
      i2_n=LOCALLM+1  
      j1o_n=0
      j2o_n=LOCALMM+1

!......Z-Direction:   obtained from X- and Y-directions.

      kmax=N
      

!*******************************************************************
!     Momentum Equation: X-direction
!******************************************************************* 
      
      do j=j1_n,j2_n
      do i=i1o_n,i2o_n
      do k=1,kmax 

#ifdef MASKING
      if (umask(i,j).ne.0) then
#endif
        neqmom_nh(0)              = neqmom_nh(0) + 1  !!! <=> nump_nh
        ijk2lmom_nh(i,j,k,1)      = neqmom_nh(0)
        mijk2lmom_nh(i,j,k,1)     = 1 
        l2imom_nh(neqmom_nh(0))   = i
        l2jmom_nh(neqmom_nh(0))   = j
        l2kmom_nh(neqmom_nh(0))   = k
        momi_nh  (neqmom_nh(0))   = nzmom_nh 

        if (i.eq.imax+1) then
!.......point p(i,j,k): condition aux limites a l est
        momj_nh(nzmom_nh)    = ijk2lq_nh (i-1,j,k)
        momjg_nh(nzmom_nh)   = ijk2lqg_nh(i-1,j,k)
        nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i-1,j,k))

        elseif (i.eq.0) then
!.......point p(i,j,k): condition aux limites a l ouest
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k)
        momjg_nh(nzmom_nh)   = ijk2lqg_nh(i,j,k)
        nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i,j,k))

        else   ! point courant   
!!!     XA bottom et surface conditions traitees implicitement car 
!!!     alors ijk2lq_nh vaut 0 et rien n est fait. 
!.......point p(i,j,k+1):
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k+1)
        momjg_nh(nzmom_nh)   = ijk2lqg_nh(i,j,k+1)
        nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i,j,k+1))

!.......point p(i,j,k):
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k)
        momjg_nh(nzmom_nh)   = ijk2lqg_nh(i,j,k)
        nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i,j,k))

!.......point p(i,j,k-1):
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k-1)
        momjg_nh(nzmom_nh)   = ijk2lqg_nh(i,j,k-1)
        nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i,j,k-1))

!.......point p(i-1,j,k+1):
        momj_nh(nzmom_nh)    = ijk2lq_nh (i-1,j,k+1)
        momjg_nh(nzmom_nh)   = ijk2lqg_nh(i-1,j,k+1)
        nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i-1,j,k+1))

!.......point p(i-1,j,k):
        momj_nh(nzmom_nh)    = ijk2lq_nh (i-1,j,k)
        momjg_nh(nzmom_nh)   = ijk2lqg_nh(i-1,j,k)
        nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i-1,j,k))

!.......point p(i-1,j,k-1):
        momj_nh(nzmom_nh)    = ijk2lq_nh (i-1,j,k-1)
        momjg_nh(nzmom_nh)   = ijk2lqg_nh(i-1,j,k-1)
        nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i-1,j,k-1))

!.......Lignes a largeur fixe:
!        momj_nh(nzmom_nh) =max(1,momj_nh(nzmom_nh))
!        if (nzmom_nh-momi_nh(neqmom_nh(0)).gt.nmlmom_nh) then
!              write(6,*) MYID,"MOM===>bande trop etroite!"
!              stop 
!        endif
!        nzmom_nh = momi_nh(neqmom_nh(0)) + nmlmom_nh

        endif
#ifdef MASKING
      endif
#endif
      enddo
      
      enddo
      enddo

!.....Number of equations in X-direction:
      neqmom_nh(1) = neqmom_nh(0)

!*******************************************************************
!     Momentum Equation: Y-direction
!******************************************************************* 

      do j=j1o_n,j2o_n
      do i=i1_n,i2_n
      do k=1,kmax 

#ifdef MASKING
      if (vmask(i,j).ne.0) then
#endif
        neqmom_nh(0)              = neqmom_nh(0) + 1
        ijk2lmom_nh(i,j,k,2)      = neqmom_nh(0)
        mijk2lmom_nh(i,j,k,2)     = 1 
        l2imom_nh(neqmom_nh(0))   = i
        l2jmom_nh(neqmom_nh(0))   = j
        l2kmom_nh(neqmom_nh(0))   = k
        momi_nh  (neqmom_nh(0))   = nzmom_nh 
    
        if (j.eq.jmax+1) then
!.......point p(i,j-1,k): condition aux limites au nord
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j-1,k)
        momjg_nh(nzmom_nh)   = ijk2lqg_nh(i,j-1,k)
        nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i,j-1,k))

        elseif (j.eq.0) then
!.......point p(i,j,k): concdition aux limites au sud
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k)
        momjg_nh(nzmom_nh)   = ijk2lqg_nh(i,j,k)
        nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i,j,k))

        else ! point courant
!.......point p(i,j,k+1):
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k+1)
        momjg_nh(nzmom_nh)   = ijk2lqg_nh(i,j,k+1)
        nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i,j,k+1))

!.......point p(i,j,k):
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k)
        momjg_nh(nzmom_nh)   = ijk2lqg_nh(i,j,k)
        nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i,j,k))

!.......point p(i,j,k-1):
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k-1)
        momjg_nh(nzmom_nh)   = ijk2lqg_nh(i,j,k-1)
        nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i,j,k-1))

!.......point p(i,j-1,k+1):
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j-1,k+1)
        momjg_nh(nzmom_nh)   = ijk2lqg_nh(i,j-1,k+1)
        nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i,j-1,k+1))

!.......point p(i,j-1,k):
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j-1,k)
        momjg_nh(nzmom_nh)   = ijk2lqg_nh(i,j-1,k)
        nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i,j-1,k))

!.......point p(i,j-1,k-1):
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j-1,k-1)
        momjg_nh(nzmom_nh)   = ijk2lqg_nh(i,j-1,k-1)
        nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i,j-1,k-1))

!.......Lignes a largeur fixe:
!       momj_nh(nzmom_nh) =max(1,momj_nh(nzmom_nh))
!       if (nzmom_nh-momi_nh(neqmom_nh(0)).gt.nmlmom_nh) then
!             write(6,*) MYID,"MOM===>bande trop etroite!"
!             stop 
!       endif
!       nzmom_nh = momi_nh(neqmom_nh(0)) + nmlmom_nh

        endif
#ifdef MASKING
      endif
#endif
      enddo
      enddo
      enddo
      
!.....Number of equations in Y-direction:
      neqmom_nh(2) = neqmom_nh(0) - neqmom_nh(1)

!*******************************************************************
! Momentum Equation: Z-direction
!******************************************************************* 
      do j=j1_n,j2_n
      do i=i1_n,i2_n
      do k=0,N

#ifdef MASKING
      if (rmask(i,j).ne.0) then
#endif
        neqmom_nh(0)            = neqmom_nh(0) + 1
        ijk2lmom_nh(i,j,k,3)    = neqmom_nh(0)
        mijk2lmom_nh(i,j,k,3)   = 1 
        l2imom_nh(neqmom_nh(0)) = i
        l2jmom_nh(neqmom_nh(0)) = j
        l2kmom_nh(neqmom_nh(0)) = k
        momi_nh  (neqmom_nh(0)) = nzmom_nh 

!.......point p(i,j,k+1):
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k+1)
        momjg_nh(nzmom_nh)   = ijk2lqg_nh(i,j,k+1)
        if (k.ne.0) then
          momv_nh(nzmom_nh)  = -1. 
        else
          momv_nh(nzmom_nh)       = 0.
        endif
        nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i,j,k+1))

        if (k.ne.0) then
!.......point p(i,j,k):
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k)
        momjg_nh(nzmom_nh)   = ijk2lqg_nh(i,j,k)
        momv_nh(nzmom_nh)    = 1. 
        nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i,j,k))
        endif

!.......Lignes a largeur fixe:
!       momj_nh(nzmom_nh) =max(1,momj_nh(nzmom_nh))
!       if (nzmom_nh-momi_nh(neqmom_nh(0)).gt.nmlmom_nh) then
!             write(6,*) MYID,"MOM===>bande trop etroite!"
!             stop
!       endif
!       nzmom_nh = momi_nh(neqmom_nh(0)) + nmlmom_nh

#ifdef MASKING
      endif
#endif
      enddo
      enddo
      enddo

!.....Number of equation in Z-direction:
      neqmom_nh(3) = neqmom_nh(0) -(neqmom_nh(1)+neqmom_nh(2)) 
      neqmom_nh(4) = neqmom_nh(0) -(neqmom_nh(1)+neqmom_nh(2)        &
                                   +neqmom_nh(3)) 
      neqmom_nh(5) = neqmom_nh(0)

!*******************************************************************
!.....Correction temporelle:
!XA   http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages/Couplage_Numerique.htm
!     on stocke la matrice qui sert dans l equation pour rho_nbq (M_corr ! dans le document)
!     coherence car c est une matrice qui va etre multipliee par un rho
!******************************************************************* 
      neqcorrt_nbq=neqmom_nh(0) 

#ifdef NBQ_DRHODT
      do l_nh = 1,nzq_nh
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
       momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,min(kmax,k+1))
       momjg_nh(nzmom_nh)   = ijk2lqg_nh(i,j,min(kmax,k+1))
       nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i,j,min(kmax,k+1)))
 
!......Point rh(i,j,k-1)
       momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,max(1,k-1))
       momjg_nh(nzmom_nh)   = ijk2lqg_nh(i,j,max(1,k-1))
       nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i,j,max(1,k-1)))

!.......Lignes a largeur fixe:
!       momj_nh(nzmom_nh) =max(1,momj_nh(nzmom_nh))
!       if (nzmom_nh-momi_nh(neqcorrt_nbq).gt.nmlmom_nh) then
!             write(6,*) MYID,"MOM===>bande trop etroite!"
!             stop 
!       endif
!       nzmom_nh = momi_nh(neqcorrt_nbq) + nmlmom_nh

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
