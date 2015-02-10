#include "cppdefs.h"
#ifdef NBQ
      subroutine mat_mom_init_nh 

!__________________________________________________________________________
!
!                               SNH2012.14      
!                 Non-Hydrostatic & Non-Boussinesq Kernel Version  
! Laboratoire d Aerologie, 14 Avenue Edouard Belin, F-31400 Toulouse
! http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm  
!
!__________________________________________________________________________
!CXA MODULES A INTEGRER
!CXA      use module_parallele !#MPI
      use module_nh
      use module_nbq       ! #NBQ#
      implicit none
# include "param_F90.h"
# include "grid_F90.h"
# include "scalars_F90.h"
!CXA MODULES A INTEGRER

!*******************************************************************
! Matrice des equations du mouvement:
!*******************************************************************
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


      dte_lp=2*dtfast   !CXA
      imax=LOCALLM+1
      jmax=LOCALMM+1

!---->initialisation:     
      nzmimp_nbq   = 1
      nzmom_nh     = 1
      nindkun_nbq  = 0

      do l_nh = 0,4
         neqmom_nh(l_nh) = 0
      enddo

      neqmimp_nbq = 0
      
      momj_nh = 1
      momv_nh = 0.

      mimpj_nbq(:) = 1
      mimpv_nbq = 0.

!---->equation du mouvement suivant x:
!CXA serial
#ifdef TOTO
!.....conditions ouvertes doivent etre prises en compte dans la matrice
! !CXA => on augmente i2o_n et on reduit i1o_n pour prendre ces points
      j1_n = 2
      if (par%tvoisin(sud).eq.mpi_proc_null.or.ifl_nbq.eq.1) j1_n = 1
      j2_n = jmax - 1
      if (par%tvoisin(nord).eq.mpi_proc_null.or.ifl_nbq.eq.1) j2_n = jmax
      i1o_n = 2
      if (par%tvoisin(ouest).eq.mpi_proc_null.or.ifl_nbq.eq.1) i1o_n = 1
      i2o_n = imax
      if (par%tvoisin(est).eq.mpi_proc_null.or.ifl_nbq.eq.1) i2o_n = imax+1
#else
      i1o_n=1
      i2o_n=LOCALLM+1
      j1_n=0
      j2_n=LOCALMM+1
      kmax=N
#endif


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
!!!     partie de construction de la matrice
        momi_nh  (neqmom_nh(0))   = nzmom_nh 

        if (i.eq.imax+1) then
!.......point p(i,j,k): condition aux limites a l est
        momj_nh(nzmom_nh)    = ijk2lq_nh (i-1,j,k)
        momjg_nh(nzmom_nh)   = ijk2lqg_nh(i-1,j,k)
        nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i-1,j,k))

        elseif (i.eq.1) then
!.......point p(i,j,k): condition aux limites a l ouest
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k)
        momjg_nh(nzmom_nh)   = ijk2lqg_nh(i,j,k)
        nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i,j,k))

        else   ! point courant   
!!! XA bottom et surface conditions traitees implicitement car 
!!!    alors ijk2lq_nh vaut 0 et rien n est fait. 
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
        momj_nh(nzmom_nh) =max(1,momj_nh(nzmom_nh))
        if (nzmom_nh-momi_nh(neqmom_nh(0)).gt.nmlmom_nh) then
              write(6,*) MYID,"MOM===>bande trop etroite!"
              stop 'coucou'
        endif
        nzmom_nh = momi_nh(neqmom_nh(0)) + nmlmom_nh

        endif
#ifdef MASKING
      endif
#endif
      enddo
      
      enddo
      enddo
      neqmom_nh(1) = neqmom_nh(0)

!---->equation du mouvement suivant y:
#ifdef TOTO
      i1_n = 2
      if (par%tvoisin(ouest).eq.mpi_proc_null.or.ifl_nbq.eq.1) i1_n = 1
      i2_n = imax - 1
      if (par%tvoisin(est).eq.mpi_proc_null.or.ifl_nbq.eq.1) i2_n = imax
      j1o_n = 2
      if (par%tvoisin(sud).eq.mpi_proc_null.or.ifl_nbq.eq.1) j1o_n = 1
      j2o_n = jmax
      if (par%tvoisin(nord).eq.mpi_proc_null.or.ifl_nbq.eq.1) j2o_n = jmax+1
#else
      i1_n=0
      i2_n=LOCALLM+1
      j1o_n=1
      j2o_n=LOCALMM+1
      kmax=N
#endif

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

        elseif (j.eq.1) then
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
        momj_nh(nzmom_nh) =max(1,momj_nh(nzmom_nh))
        if (nzmom_nh-momi_nh(neqmom_nh(0)).gt.nmlmom_nh) then
              write(6,*) MYID,"MOM===>bande trop etroite!"
              stop 'coucou'
        endif
        nzmom_nh = momi_nh(neqmom_nh(0)) + nmlmom_nh

        endif
#ifdef MASKING
      endif
#endif
      enddo
      enddo
      enddo
      
      neqmom_nh(2) = neqmom_nh(0) - neqmom_nh(1)

!---->equation du mouvement suivant z:

      do j=j1_n,j2_n
      do i=i1_n,i2_n
!CXA      do k=1,kmax+1
      do k=0,N
#ifdef MASKING
      if (rmask(i,j).ne.0) then
#endif
!CXA  !!! ici car tableaux a declarer a partir de k=0 ci dessous
        neqmom_nh(0)            = neqmom_nh(0) + 1
        ijk2lmom_nh(i,j,k,3)    = neqmom_nh(0)
        mijk2lmom_nh(i,j,k,3)   = 1 
        l2imom_nh(neqmom_nh(0)) = i
        l2jmom_nh(neqmom_nh(0)) = j
        l2kmom_nh(neqmom_nh(0)) = k
        momi_nh  (neqmom_nh(0)) = nzmom_nh 

!.......point p(i,j,k+1):
!CXA        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k)
!CXA        momjg_nh(nzmom_nh)   = ijk2lqg_nh(i,j,k)
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k+1)
        momjg_nh(nzmom_nh)   = ijk2lqg_nh(i,j,k+1)
!CXA        if (k.ne.1) then
        if (k.ne.0) then
          momv_nh(nzmom_nh)  = -1. 
        else
          momv_nh(nzmom_nh)       = 0.
        endif
!CXA        nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i,j,k))
        nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i,j,k+1))

!CXA        if (k.ne.1) then
        if (k.ne.0) then
!.......point p(i,j,k):
!CXA        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k-1)
!CXA        momjg_nh(nzmom_nh)   = ijk2lqg_nh(i,j,k-1)
        momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,k)
        momjg_nh(nzmom_nh)   = ijk2lqg_nh(i,j,k)
        momv_nh(nzmom_nh)    = 1. 
!CXA        nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i,j,k-1))
        nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i,j,k))
        endif

!.......Lignes a largeur fixe:
        momj_nh(nzmom_nh) =max(1,momj_nh(nzmom_nh))
        if (nzmom_nh-momi_nh(neqmom_nh(0)).gt.nmlmom_nh) then
              write(6,*) MYID,"MOM===>bande trop etroite!"
              stop 'coucou'
        endif
        nzmom_nh = momi_nh(neqmom_nh(0)) + nmlmom_nh
#ifdef MASKING
      endif
#endif
      enddo
      enddo
      enddo

#ifdef TOTO_IMP
!............................
!.......Partie implicite:
!............................


      neqmimp_nbq = 0 

      do j=j1_n,j2_n
      do i=i1_n,i2_n
      do k=1,kmax+1
#ifdef MASKING
      if (rmask(i,j).ne.0) then
#endif
        neqmimp_nbq              = neqmimp_nbq  + 1
        mimpi_nbq (neqmimp_nbq)  = nzmimp_nbq

        if (k.gt.1) then
         mimpj_nbq(nzmimp_nbq)   = ijk2lq_nh (i,j,k)
         mimpv_nbq(nzmimp_nbq)   = -1. * coefimp_nbq
         nzmimp_nbq              = nzmimp_nbq + min(1,ijk2lq_nh(i,j,k))

         mimpj_nbq(nzmimp_nbq)   = ijk2lq_nh (i,j,k-1)
         mimpv_nbq(nzmimp_nbq)   = 1. * coefimp_nbq
         nzmimp_nbq              = nzmimp_nbq + min(1,ijk2lq_nh(i,j,k-1))
        else
         mimpj_nbq(nzmimp_nbq)   = ijk2lq_nh (i,j,k)
         mimpv_nbq(nzmimp_nbq)   = 0.
         nzmimp_nbq              = nzmimp_nbq + min(1,ijk2lq_nh(i,j,k))
         nindkun_nbq             = nindkun_nbq +1
         indkun_nbq(nindkun_nbq) = neqmimp_nbq
        endif
#ifdef MASKING
      endif
#endif
      enddo
      enddo
      enddo

#endif

      neqmom_nh(3) = neqmom_nh(0) -(neqmom_nh(1)+neqmom_nh(2)) 
      neqmom_nh(4) = neqmom_nh(0) -(neqmom_nh(1)+neqmom_nh(2)        &
                                   +neqmom_nh(3)) 
      neqmom_nh(5) = neqmom_nh(0)

!.....Correction temporelle:
!XA http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages/Couplage_Numerique.htm
! on stocke la matrice qui sert dans l equation pour rho_nbq (M_corr ! dans le document)
!! coherence car c est une matrice qui va etre multipliee par un rho
!  comme les autres plus haut
      neqcorrt_nbq=neqmom_nh(0) 

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
       momvg_nh(nzmom_nh)    = 1./dte_lp
       nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i,j,min(kmax,k+1)))
 
!......Point rh(i,j,k-1)
       momj_nh(nzmom_nh)    = ijk2lq_nh (i,j,max(1,k-1))
       momjg_nh(nzmom_nh)   = ijk2lqg_nh(i,j,max(1,k-1))
       momvg_nh(nzmom_nh)    = -1./dte_lp
       nzmom_nh             = nzmom_nh + min(1,ijk2lq_nh(i,j,max(1,k-1)))

!.......Lignes a largeur fixe:
        momj_nh(nzmom_nh) =max(1,momj_nh(nzmom_nh))
        if (nzmom_nh-momi_nh(neqcorrt_nbq).gt.nmlmom_nh) then
              write(6,*) MYID,"MOM===>bande trop etroite!"
              stop 'coucou'
        endif
        nzmom_nh = momi_nh(neqcorrt_nbq) + nmlmom_nh

      enddo

!.....Dernière ligne de la matrice:
      momi_nh (neqcorrt_nbq+1) = nzmom_nh 
      mimpi_nbq(neqmimp_nbq+1) = nzmimp_nbq    ! Matrice implicite
      
!.....test de la taille de la matrice:

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
