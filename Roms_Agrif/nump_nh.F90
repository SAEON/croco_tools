#include "cppdefs.h"
#ifdef NBQ
      subroutine nump_nh
!__________________________________________________________________________
!
!                               SNH2012.14      
!                 Non-Hydrostatic & Non-Boussinesq Kernel Version  
! Laboratoire d Aerologie, 14 Avenue Edouard Belin, F-31400 Toulouse
! http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm  
!
!__________________________________________________________________________

!!! manque modules + codes en dur a plusieurs endroits
!!! g de global a transformer en e pour tableau ijk2lqg_nh

!CXA MODULES TO ADD 
!CXA      use module_principal
!CXA      use module_parallele !#MPI
      use module_nh
      implicit none
# include "param_F90.h"
# include "grid_F90.h"
!CXA MODULES TO ADD 

      integer                                                         &
         i,j,k,kmax,                                                  &
         i1_n                                                         &
        ,i2_n                                                         &
        ,j1_n                                                         &
        ,j2_n                                                         &
        ,j1c_n                                                        &
        ,j2c_n                                                        &
        ,offsetvoisin_se                                              &
        ,offsetvoisin_so                                              &
        ,i_n                                                          &
        ,offsetvoisin_ne                                              &
        ,offsetvoisin_no                                              

#ifdef MPI
#define LOCALLM Lmmpi
#define LOCALMM Mmmpi
#else
#define LOCALLM Lm
#define LOCALMM Mm
#endif    




!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!
!  ATTENTION:
!
!     la numerotation doit etre coherente avec celle de l equations
!     de continuite et de ses conditionx aux limites:
!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!CXA PAS UTILISE - OPTIMISATION EN COURS
!******************************************************************************************
! Nombre de colonnes max par lignes des matrices MOM et CONT
!******************************************************************************************
!     nmlmat_nh = 0
!C     if ((iglb.le.3.and.(.not.iperiodicboundary)).or.(jglb.le.3.and.(.not.jperiodicboundary))) then
#ifdef MASKING
       if (LOCALLM.eq.1 .or. LOCALMM.eq.1) then
           nmlmom_nh  = 6 
           nmlcont_nh = 8 
       else
#endif
           nmlmom_nh  = 6 
           nmlcont_nh = 14 
#ifdef MASKING
       endif
#endif
!C     endif
!     write(6,*) par%rank,nmlmom_nh,nmlcont_nh

!CXA PAS UTILISE - OPTIMISATION EN COURS

 
!******************************************************************************************
! numerotation de l interieure du domaine
!******************************************************************************************
!CXA serial
#ifdef TOTO
      nzq_nh   = 0

!!! on étend le domaine au maximum en prenant tous les points dispos
      i1_n = 2
      if (par%tvoisin(ouest).eq.mpi_proc_null) i1_n = 1
      i2_n = imax - 1
      if (par%tvoisin(est).eq.mpi_proc_null) i2_n = imax
      j1_n = 2
      if (par%tvoisin(sud).eq.mpi_proc_null) j1_n = 1
      j2_n = jmax - 1
      if (par%tvoisin(nord).eq.mpi_proc_null) j2_n = jmax
#else
      kmax=N  !CXA
      i1_n=1
      i2_n=LOCALLM
      j1_n=1
      j2_n=LOCALMM
#endif

!.....numerotation: 
! cf: http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages/Algebrique_Numerotation_Base.htm
      do k=1,kmax 
      do i = i1_n , i2_n
      do j = j1_n , j2_n
!      do k=1,kmax                                   ! autre numerotation possible (inversion des boucles)
!CXA         if (mask_t(i,j,k).ne.0) then
#ifdef MASKING
          if (rmask(i,j).ne.0) then
#endif
           nzq_nh            = nzq_nh + 1            ! non zero
           ijk2lq_nh(i,j,k)  = nzq_nh                ! 
           mijk2lq_nh(i,j,k) = 1 
           l2iq_nh  (nzq_nh) = i
           l2jq_nh  (nzq_nh) = j
           l2kq_nh  (nzq_nh) = k
#ifdef MASKING
          endif
#endif
      enddo
      enddo
      enddo
  
!---->equation du mouvement suivant x:

!******************************************************************************************
!.....mise a jour du nombre total de points du domaine local:
!******************************************************************************************

      neqcont_nh(0) = nzq_nh

#ifdef TOTO

!.....nombre de points interieurs  !!!XA pour hips ou autre solveur:
!combien de points sur chaque domaine a broadcaster

      neqint_nh(par%rank) = neqcont_nh(0)



      call mpi_allgather(  ijk2lq_nh                                  &
                           ,(imax+2)*(jmax+2)*(kmax+3)                &  ! nombre d'elements (envoi)
                           ,mpi_integer                               &
                           ,ijk2lg_nh                                 &
                           ,(imax+2)*(jmax+2)*(kmax+3)                &   ! nombre d'elements (reception)
                           ,mpi_integer                               &
!                          ,par%comm2d                                &
                           ,par%comm2d                                &
                           ,ierr                                      &
                          )



!******************************************************************************************
!.....numerotation locale etendue
!******************************************************************************************

!-----> calcul de l'offset du domaine local:
      offsetloc = 0
      do i=0,par%rank-1
         offsetloc = offsetloc + neqint_nh(i)
      enddo

!-----> transformation de ijk2lq en numerotation globale:
!      do l=1,neqcont_nh(0)
!        ijk2lqg_nh(l) = ijk2lq_nh(l) + offsetloc
!      enddo
       ijk2lqg_nh = ijk2lq_nh + offsetloc  

!-----> traitement de la frontiere ouest avec voisin:

!.....calcul de l'offset pour le domaine se trouvant a l'ouest:
      if (par%tvoisin(ouest).ne.mpi_proc_null) then 

      offsetvoisin = 0
      do i_n=0,par%tvoisin(ouest)-1
         offsetvoisin = offsetvoisin + neqint_nh(i_n)
      enddo

!.......ajout d'une bande de points de pnh a l'ouest:

        if (par%tvoisin(nord).ne.mpi_proc_null) then
         offsetvoisin_no= 0
         do i_n=0,par%tvoisin(nordouest)-1
          offsetvoisin_no = offsetvoisin_no + neqint_nh(i_n)
         enddo
         j2c_n=jmax
        else
         j2c_n=j2_n
        endif
        if (par%tvoisin(sud).ne.mpi_proc_null) then
         offsetvoisin_so= 0
         do i_n=0,par%tvoisin(sudouest)-1
          offsetvoisin_so = offsetvoisin_so + neqint_nh(i_n)
         enddo
         j1c_n=1
        else
         j1c_n=j1_n
        endif

        i    = 1
        i1   = imax - 1   ! tous les domaines ont la meme taille 
        do k=1,kmax
        do j = j1c_n , j2c_n    
!         do k=1,kmax
          if (mask_t(i,j,k).ne.0) then
            nzq_nh = nzq_nh + 1
            if (j.gt.j2_n) then
            ijk2lqg_nh(i,j,k) = ijk2lg_nh(i1,2,k ,par%tvoisin(nordouest))   + offsetvoisin_no
            elseif (j.lt.j1_n) then
            ijk2lqg_nh(i,j,k) = ijk2lg_nh(i1,jmax-1,k,par%tvoisin(sudouest))+ offsetvoisin_so
            else
            ijk2lqg_nh(i,j,k) = ijk2lg_nh(i1,j,k,par%tvoisin(ouest))        + offsetvoisin
	    endif
            fqg_nh(nzq_nh-neqcont_nh(0))= ijk2lqg_nh(i,j,k)
            ijk2lq_nh(i,j,k)  = nzq_nh 
            mijk2lq_nh(i,j,k) = min(1,ijk2lq_nh(i,j,k))
            l2iq_nh  (nzq_nh) = i
            l2jq_nh  (nzq_nh) = j
            l2kq_nh  (nzq_nh) = k
          endif
         enddo
        enddo
      endif

       
      
!.....calcul de l'offset pour le domaine se trouvant a l'est:
      if (par%tvoisin(est).ne.mpi_proc_null) then 
        offsetvoisin = 0
        do i_n=0,par%tvoisin(est)-1
         offsetvoisin = offsetvoisin + neqint_nh(i_n)
        enddo

!.......ajout d'une bande de points de pnh a l'est:
        if (par%tvoisin(nord).ne.mpi_proc_null) then
         offsetvoisin_ne= 0
         do i_n=0,par%tvoisin(nordest)-1
          offsetvoisin_ne = offsetvoisin_ne + neqint_nh(i_n)
         enddo
         j2c_n=jmax
        else
         j2c_n=j2_n
        endif
        if (par%tvoisin(sud).ne.mpi_proc_null) then
         offsetvoisin_se= 0
         do i_n=0,par%tvoisin(sudest)-1
          offsetvoisin_se = offsetvoisin_se + neqint_nh(i_n)
         enddo
         j1c_n=1
        else
         j1c_n=j1_n
        endif
        i    = imax
        i1   = 2
        do k=1,kmax
        do j = j1c_n , j2c_n   
!         do k=1,kmax
          if (mask_t(i,j,k).ne.0) then
            nzq_nh            = nzq_nh + 1 
            if (j.gt.j2_n) then
            ijk2lqg_nh(i,j,k) = ijk2lg_nh(i1,2,k,par%tvoisin(nordest))+ offsetvoisin_ne
            elseif (j.lt.j1_n) then
            ijk2lqg_nh(i,j,k) = ijk2lg_nh(i1,jmax-1,k,par%tvoisin(sudest))+ offsetvoisin_se
            else
   
            ijk2lqg_nh(i,j,k) = ijk2lg_nh(i1,j,k,par%tvoisin(est)) + offsetvoisin
            endif
            fqg_nh(nzq_nh-neqcont_nh(0)) = ijk2lqg_nh(i,j,k)
            ijk2lq_nh(i,j,k)  = nzq_nh 
            mijk2lq_nh(i,j,k) = min(1,ijk2lq_nh(i,j,k))
            l2iq_nh  (nzq_nh) = i
            l2jq_nh  (nzq_nh) = j
            l2kq_nh  (nzq_nh) = k
          endif
         enddo
        enddo
      endif
     
!.....calcul de l'offset pour le domaine se trouvant au sud:
      if (par%tvoisin(sud).ne.mpi_proc_null) then 

        offsetvoisin = 0
        do i=0,par%tvoisin(sud)-1
         offsetvoisin = offsetvoisin + neqint_nh(i)
        enddo
         
!.......ajout d'une bande de points de pnh au sud:
        j    = 1
        j1   = jmax-1    ! tous les domaines ont la meme taille:
        do k=1,kmax
        do i = i1_n , i2_n     
!         do k=1,kmax
          if (mask_t(i,j,k).ne.0) then
            nzq_nh = nzq_nh + 1
            ijk2lqg_nh(i,j,k) = ijk2lg_nh(i,j1,k,par%tvoisin(sud))+ offsetvoisin
            fqg_nh(nzq_nh-neqcont_nh(0)) = ijk2lqg_nh(i,j,k)
            ijk2lq_nh(i,j,k)  = nzq_nh 
            mijk2lq_nh(i,j,k) = min(1,ijk2lq_nh(i,j,k))
            l2iq_nh  (nzq_nh) = i
            l2jq_nh  (nzq_nh) = j
            l2kq_nh  (nzq_nh) = k
          endif
         enddo
        enddo
        
      endif

!.....calcul de l'offset pour le domaine se trouvant au nord:
      if (par%tvoisin(nord).ne.mpi_proc_null) then 
        
        offsetvoisin = 0
        do i=0,par%tvoisin(nord)-1
         offsetvoisin = offsetvoisin + neqint_nh(i)
        enddo
         
!.......ajout d'une bande de points de pnh au nord:
        j    = jmax
        j1   = 2     ! tous les domaines ont la meme taille:
        do k=1,kmax
        do i = i1_n , i2_n     
!         do k=1,kmax
          if (mask_t(i,j,k).ne.0) then
            nzq_nh            = nzq_nh + 1
            ijk2lqg_nh(i,j,k) = ijk2lg_nh(i,j1,k,par%tvoisin(nord))+ offsetvoisin
            fqg_nh(nzq_nh-neqcont_nh(0)) = ijk2lqg_nh(i,j,k)
            ijk2lq_nh(i,j,k)  = nzq_nh 
            mijk2lq_nh(i,j,k) = min(1,ijk2lq_nh(i,j,k))
            l2iq_nh  (nzq_nh) = i
            l2jq_nh  (nzq_nh) = j
            l2kq_nh  (nzq_nh) = k
          endif
         enddo
        enddo
        
      endif

#else

!!! on recopie le tableau local -> tableau étendu 
!!! cf http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages/Algebrique_Numerotations_Speciales.htm
# ifdef MPI
      ijk2lg_nh(:,:,:,mynode)=ijk2lqg_nh(:,:,:)
# else
      ijk2lg_nh(:,:,:,0)=ijk2lqg_nh(:,:,:)
# endif
#endif

      return

      end subroutine nump_nh
 
#else
        subroutine nump_nh_empty
        return
        end 
#endif
