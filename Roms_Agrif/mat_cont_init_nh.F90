#include "cppdefs.h"
#ifdef NBQ
      subroutine mat_cont_init_nh 

!*******************************************************************
!*******************************************************************
!*******************************************************************
! Matrix CONT for continuity equation: initialization
!
! http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages/Algebrique_Formats_CSR.htm
!
! Matrix construction:
! http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Pub/NH-NBQ/Html_maps/Schemas_Num_map.html
!
!*******************************************************************
!*******************************************************************
!*******************************************************************

      use module_nh
      use module_nbq
      implicit none

# include "param_F90.h"
# include "scalars_F90.h"
      
      integer :: l1_m,i,j,k,kmax

!*******************************************************************
!     Various Initializations:
!*******************************************************************

!.....Initialize position indices, which will be used
!     as size variable afterwards:
      nzcont_nh     = 1

!.....initializations:
      neqcont_nh(1:4) = 0

      contj_nh  = 0
      contv_nh  = 0.

      kmax=N

      l1_nh    = 0

      cont_nnz_nh(1)=1

!*******************************************************************
!     Continuity Equation:
!*******************************************************************
      do l_nh = 1,neqcont_nh(0)   

!......Equation characteristics:
       i = l2iq_nh (l_nh) 
       j = l2jq_nh (l_nh) 
       k = l2kq_nh (l_nh)      
       conti_nh (l_nh) = nzcont_nh  !! matrix line pointer
       l1_m = l1_nh

!.......u(i,j,k):
        if (ijk2lmom_nh(i,j,k,1).ne.0) then 
         l1_nh = l1_nh + 1
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k,1)
         nzcont_nh           = nzcont_nh + 1
        endif

!.......v(i,j,k):
        if (ijk2lmom_nh(i,j,k,2).ne.0) then 
         l1_nh = l1_nh + 1
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k,2)
         nzcont_nh           = nzcont_nh + 1
        endif

        if (k.ne.kmax) then

!.......u(i,j,k+1):
        if (ijk2lmom_nh(i,j,k+1,1).ne.0) then 
         l1_nh = l1_nh + 1
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k+1,1)
         nzcont_nh           = nzcont_nh + 1
        endif

!.......v(i,j,k+1):
        if (ijk2lmom_nh(i,j,k+1,2).ne.0) then 
        l1_nh = l1_nh + 1
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k+1,2)
         nzcont_nh           = nzcont_nh + 1
        endif

        endif

        if (k.ne.1) then ! If not bottom 
!........u(i,j,k-1):
         if (ijk2lmom_nh(i,j,k-1,1).ne.0) then 
          l1_nh = l1_nh + 1
          contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k-1,1)
          nzcont_nh           = nzcont_nh + 1
         endif

!........v(i,j,k-1):
         if (ijk2lmom_nh(i,j,k-1,2).ne.0) then 
          l1_nh = l1_nh + 1
          contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k-1,2)
          nzcont_nh           = nzcont_nh + 1
         endif
        endif

!.......u(i+1,j,k):
        if (ijk2lmom_nh(i+1,j,k,1).ne.0) then 
         l1_nh = l1_nh + 1
         contj_nh(nzcont_nh) = ijk2lmom_nh(i+1,j,k,1)
         nzcont_nh           = nzcont_nh + 1
        endif

!.......v(i,j+1,k):
        if (ijk2lmom_nh(i,j+1,k,2).ne.0) then 
         l1_nh = l1_nh + 1
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j+1,k,2)
         nzcont_nh           = nzcont_nh + 1
        endif

        if (k.ne.kmax) then ! If not surface
!........u(i+1,j,k+1):
         if (ijk2lmom_nh(i+1,j,k+1,1).ne.0) then 
          l1_nh = l1_nh + 1
          contj_nh(nzcont_nh) = ijk2lmom_nh(i+1,j,k+1,1)
          nzcont_nh           = nzcont_nh + 1
         endif

!........v(i,j+1,k+1):
         if (ijk2lmom_nh(i,j+1,k+1,2).ne.0) then 
          l1_nh = l1_nh + 1
          contj_nh(nzcont_nh) = ijk2lmom_nh(i,j+1,k+1,2)
          nzcont_nh           = nzcont_nh + 1
         endif
        endif

        if (k.ne.1) then ! If not bottom 
!........u(i+1,j,k-1):
         if (ijk2lmom_nh(i+1,j,k-1,1).ne.0) then 
          l1_nh = l1_nh + 1
          contj_nh(nzcont_nh) = ijk2lmom_nh(i+1,j,k-1,1)
          nzcont_nh           = nzcont_nh + 1
         endif

!........v(i,j+1,k-1):
         if (ijk2lmom_nh(i,j+1,k-1,2).ne.0) then 
          l1_nh = l1_nh + 1
          contj_nh(nzcont_nh) = ijk2lmom_nh(i,j+1,k-1,2)
          nzcont_nh           = nzcont_nh + 1
         endif

        endif

!.......w(i,j,k):
        if (ijk2lmom_nh(i,j,k,3).ne.0) then 
         l1_nh = l1_nh + 1
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k,3)
         nzcont_nh           = nzcont_nh + 1
        endif

!.......w(i,j,k-1):
        if (ijk2lmom_nh(i,j,k-1,3).ne.0) then 
         l1_nh = l1_nh + 1
         contj_nh(nzcont_nh) = ijk2lmom_nh(i,j,k-1,3)
         nzcont_nh           = nzcont_nh + 1
        endif

!.......Lines with fixed width: conti_nh(l_nh)
!         contj_nh(nzcont_nh)=max(1,contj_nh(nzcont_nh))        
!         if (nzcont_nh-conti_nh(l_nh).gt.nmlcont_nh) then
!	    write(6,*) MYID,"CONT ==> the band is too narrow!", &
!                                      nzcont_nh-conti_nh(l_nh)
!           stop 
!         endif
!        nzcont_nh = conti_nh(l_nh) + nmlcont_nh
!        l1_nh = l1_m + nmlcont_nh

!       Last point...
        cont_nnz_nh(l_nh+1)=l1_nh+1

      enddo

!*******************************************************************
!     Last line treatment:
!*******************************************************************
      conti_nh (neqcont_nh(0)+1) = nzcont_nh
      neqcimp_nbq = nzq_nh 
      cimpi_nbq(neqcimp_nbq+1) = nzcimp_nbq

!.....Test matrix size:
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
