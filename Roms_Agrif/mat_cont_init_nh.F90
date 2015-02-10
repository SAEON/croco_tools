#include "cppdefs.h"
#ifdef NBQ
      subroutine mat_cont_init_nh 

!__________________________________________________________________________
!
!                               SNH2012.14      
!                 Non-Hydrostatic & Non-Boussinesq Kernel Version  
! Laboratoire d Aerologie, 14 Avenue Edouard Belin, F-31400 Toulouse
! http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm  
!
!__________________________________________________________________________
!CXA MODULES TO ADD 
      use module_nh
      use module_nbq
      implicit none
# include "param_F90.h"
# include "scalars_F90.h"


!CXA      use module_principal
!CXA      use module_parallele !#MPI
!CXA MODULES TO ADD 
      
      integer :: l1_m,i,j,k,kmax

! CF http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages/Algebrique_Formats_CSR.htm

!*******************************************************************
! matrice de l'equation de continuite
!*******************************************************************

!.....Initialisation des indicateurs de position,
!     qui serviront de variable taille par la suite:
      nzcont_nh     = 1

!.....initialisations:

      do l_nh = 1,4
        neqcont_nh(l_nh) = 0
      enddo

      contj_nh  = 1
      contv_nh  = 0.

!.......................................
! construction de la matrice:
! http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Pub/NH-NBQ/Html_maps/Schemas_Num_map.html
!.......................................
!CXA on redefinit kmax pour croco
      kmax=N
!CXA on redefinit kmax pour croco

      l1_nh    = 0
      cont_nnz_nh(1)=1

      do l_nh = 1,neqcont_nh(0)    !! l_nh = lignes | l1_nh = no de 

!......caracteristiques de l equation:
       i = l2iq_nh (l_nh) 
       j = l2jq_nh (l_nh) 
       k = l2kq_nh (l_nh)      
       conti_nh (l_nh) = nzcont_nh  !! matrice indice lignes pointer
       l1_m = l1_nh


!.......point u(i,j,k):
        if (ijk2lmom_nh(i,j,k,1).ne.0) then 
        l1_nh = l1_nh + 1
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k,1)
         nzcont_nh           = nzcont_nh + 1
        endif

!.......point v(i,j,k):
        if (ijk2lmom_nh(i,j,k,2).ne.0) then 
        l1_nh = l1_nh + 1
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k,2)
         nzcont_nh           = nzcont_nh + 1
        endif

        if (k.ne.kmax) then

!.......point u(i,j,k+1):
        if (ijk2lmom_nh(i,j,k+1,1).ne.0) then 
        l1_nh = l1_nh + 1
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k+1,1)
         nzcont_nh           = nzcont_nh + 1
        endif

!.......point v(i,j,k+1):
        if (ijk2lmom_nh(i,j,k+1,2).ne.0) then 
        l1_nh = l1_nh + 1
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k+1,2)
         nzcont_nh           = nzcont_nh + 1
        endif

        endif

        if (k.ne.1) then

!.......point u(i,j,k-1):
        if (ijk2lmom_nh(i,j,k-1,1).ne.0) then 
        l1_nh = l1_nh + 1
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k-1,1)
         nzcont_nh           = nzcont_nh + 1
        endif

!.......point v(i,j,k-1):
        if (ijk2lmom_nh(i,j,k-1,2).ne.0) then 
        l1_nh = l1_nh + 1
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k-1,2)
         nzcont_nh           = nzcont_nh + 1
        endif

        endif

!.......point u(i+1,j,k):
        if (ijk2lmom_nh(i+1,j,k,1).ne.0) then 
        l1_nh = l1_nh + 1
         contj_nh(nzcont_nh) = ijk2lmom_nh(i+1,j,k,1)
         nzcont_nh           = nzcont_nh + 1
        endif

!.......point v(i,j+1,k):
        if (ijk2lmom_nh(i,j+1,k,2).ne.0) then 
        l1_nh = l1_nh + 1
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j+1,k,2)
         nzcont_nh           = nzcont_nh + 1
        endif

        if (k.ne.kmax) then

!.......point u(i+1,j,k+1):
        if (ijk2lmom_nh(i+1,j,k+1,1).ne.0) then 
        l1_nh = l1_nh + 1
         contj_nh(nzcont_nh) = ijk2lmom_nh(i+1,j,k+1,1)
         nzcont_nh           = nzcont_nh + 1
        endif

!.......point v(i,j+1,k+1):
        if (ijk2lmom_nh(i,j+1,k+1,2).ne.0) then 
        l1_nh = l1_nh + 1
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j+1,k+1,2)
         nzcont_nh           = nzcont_nh + 1
        endif

        endif

        if (k.ne.1) then

!.......point u(i+1,j,k-1):
        if (ijk2lmom_nh(i+1,j,k-1,1).ne.0) then 
        l1_nh = l1_nh + 1
         contj_nh(nzcont_nh) = ijk2lmom_nh(i+1,j,k-1,1)
         nzcont_nh           = nzcont_nh + 1
        endif

!.......point v(i,j+1,k-1):
        if (ijk2lmom_nh(i,j+1,k-1,2).ne.0) then 
        l1_nh = l1_nh + 1
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j+1,k-1,2)
         nzcont_nh           = nzcont_nh + 1
        endif

        endif

!.......point w(i,j,k):
!CXA        if (ijk2lmom_nh(i,j,k+1,3).ne.0) then 
        if (ijk2lmom_nh(i,j,k,3).ne.0) then 
        l1_nh = l1_nh + 1
!CXA         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k+1,3)
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k,3)
         nzcont_nh           = nzcont_nh + 1
        endif

!.......point w(i,j,k-1):
!      if (k.ne.1) then
!CXA         if (ijk2lmom_nh(i,j,k,3).ne.0) then 
         if (ijk2lmom_nh(i,j,k-1,3).ne.0) then 
          l1_nh = l1_nh + 1
!CXA          contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k,3)
          contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k-1,3)
          nzcont_nh           = nzcont_nh + 1
         endif
!       endif

!... ....Lignes a largeur fixe: conti_nh(l_nh)
         contj_nh(nzcont_nh)=max(1,contj_nh(nzcont_nh))        
         if (nzcont_nh-conti_nh(l_nh).gt.nmlcont_nh) then
	      write(6,*) MYID,"CONT===>bande trop etroite!",nzcont_nh-conti_nh(l_nh)
              stop 
         endif
         nzcont_nh = conti_nh(l_nh) + nmlcont_nh
         l1_nh = l1_m + nmlcont_nh

!!! fermeture matrice
         cont_nnz_nh(l_nh+1)=l1_nh+1

      enddo

#ifdef TOTO
      if (ifl_nbq.eq.1) then
!.......................................
! Matrice schema implicit: pour (rho_nb w) et rho_nb
! on aura le tridiagonal en faisant le produit M_imp x Cont_imp
!  points interieurs et points de frontiere
! http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages/Schemas_Vz_Implicite.htm
!.......................................
     
      nzcimp_nbq     = 1
      if (iteration3d.eq.0) then
       cimpj_nbq = 1
       cimpv_nbq = 0.
      endif

      do l_nh = 1,nzq_nh

!......caracteristiques de l'equation:
       i = l2iq_nh (l_nh)
       j = l2jq_nh (l_nh)
       k = l2kq_nh (l_nh)
       cimpi_nbq(l_nh) = nzcimp_nbq

!.......point w(i,j,k+1):
!CXA       if (ijk2lmom_nh(i,j,k+1,3).ne.0) then
       if (ijk2lmom_nh(i,j,k,3).ne.0) then
!CXA         cimpj_nbq(nzcimp_nbq) = ijk2lmom_nh(i,j,k+1,3)-neqmom_nh(1)-neqmom_nh(2)
         cimpj_nbq(nzcimp_nbq) = ijk2lmom_nh(i,j,k,3)-neqmom_nh(1)-neqmom_nh(2)
         nzcimp_nbq            = nzcimp_nbq + 1
       endif

!.......point w(i,j,k):
!CXA       if (ijk2lmom_nh(i,j,k,3).ne.0) then
       if (ijk2lmom_nh(i,j,k-1,3).ne.0) then
!CXA         cimpj_nbq(nzcimp_nbq) = ijk2lmom_nh(i,j,k,3)-neqmom_nh(1)-neqmom_nh(2)
         cimpj_nbq(nzcimp_nbq) = ijk2lmom_nh(i,j,k-1,3)-neqmom_nh(1)-neqmom_nh(2)
         nzcimp_nbq            = nzcimp_nbq + 1
       endif

      enddo

      endif
#endif
!.......................................
! traitement du dernier point des
!     matrices
!.......................................
      conti_nh (neqcont_nh(0)+1) = nzcont_nh
      neqcimp_nbq = nzq_nh 
      cimpi_nbq(neqcimp_nbq+1) = nzcimp_nbq

!.....test de la taille de la matrice:

      if (nzcont_nh.gt.nmcont_nh) then
         write (6,*) 'nmcont_nh trop petit!'
         stop
      endif
      
      return
      end subroutine mat_cont_init_nh
#else
      subroutine mat_cont_init_nh_empty
      return
      end
#endif
