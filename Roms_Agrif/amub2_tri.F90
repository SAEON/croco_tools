#include "cppdefs.h"
#ifdef NBQ
!
!======================================================================
!                      NBQ-Mode for NH-modeling
!                            Main Routine
!======================================================================
!
!> @note Main web documentation:
!  http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/index_snh_home.htm
!
! DESCRIPTION: 
!
!> @brief SNBQ driver : Non-hydrostatic algorithm with the 
!                       Non-boussinesq solver.
!
!> @details NBQ time step. See SNBQ web pages (step3d_nbq.F90) 
! REVISION HISTORY:
!
!> @authors
!> @date 2015 October
!> @todo
!
!======================================================================
!

subroutine amub2_tri ( nrow, ncol, job, a, ja, ia, b, jb, ib, jc, ic, nzmax, &
  iw, ierr_a )

!**********************************************************************
!
!     AMUB performs the matrix product C = A * B.
!
!  Discussion:
!
!    The column dimension of B is not needed.
!
!  Modified:
!
!    08 January 2004
!
!  Author:
!
!    Youcef Saad
!
!  Parameters:
!
!    Input, integer NROW, the row dimension of the matrix.
!
!    Input, integer NCOL, the column dimension of the matrix.
!
!    Input, integer JOB, job indicator.  When JOB = 0, only the structure
!    is computed, that is, the arrays JC and IC, but the real values
!    are ignored.
!
!    Input, real A(*), integer JA(*), IA(NROW+1), the matrix in CSR
!    Compressed Sparse Row format.
!
!    Input, b, jb, ib, matrix B in compressed sparse row format.
!
!    Input, integer NZMAX, the length of the arrays c and jc.
!    The routine will stop if the result matrix C  has a number
!    of elements that exceeds exceeds NZMAX.
!
! on return:
!
! c,
! jc,
! ic    = resulting matrix C in compressed sparse row sparse format.
!
! ierr_a      = integer. serving as error message.
!         ierr_a = 0 means normal return,
!         ierr_a > 0 means that amub stopped while computing the
!         i-th row  of C with i = ierr_a, because the number
!         of elements in C exceeds nzmax.
!
! work arrays:
!
!  iw      = integer work array of length equal to the number of
!         columns in A.
!
!**********************************************************************
!
!      use module_principal  
!      use module_parallele
      use module_nh 
      use module_nbq
!      use module_exp 

  implicit none

# include "param_F90.h"
# include "scalars_F90.h"
# include "ocean3d.h"
# include "grid.h"
# include "nbq.h"

  integer ncol
  integer nrow
  integer nzmax

  real ( kind = 8 ) a(*)
  real ( kind = 8 ) b(*)
!  real ( kind = 8 ) c(nzmax)
!  integer ia(nrow+1)
!  integer ib(ncol+1)
!  integer ic(ncol+1)
  integer k
  integer ia(*)
  integer ib(*)
  integer ic(*)
  integer ierr_a
  integer iii
  integer iw(*)
  integer ja(*)
  integer jb(*)
  integer jc(nzmax)
  integer jcol
  integer jjj
  integer job
  integer jpos
!  integer k
  integer ka
  integer kb
  integer len
  real ( kind = 8 ) scal
  logical values

  pdv_nbq(1:neqmimp_nbq)=1.d0
  plv_nbq(1:neqmimp_nbq)=0.d0
  puv_nbq(1:neqmimp_nbq)=0.d0

  values = ( job /= 0 )
  len = 0
  ic(1) = 1
  ierr_a = 0
!
!  Initialize IW.
!
   iw(1:ncol) = 0

   do iii = 1, nrow
!
!  Row I.
!
    do ka = ia(iii), ia(iii+1)-1

      if ( values ) then
        scal = a(ka)
      end if

      jjj = ja(ka)

      do kb = ib(jjj), ib(jjj+1)-1

           jcol = jb(kb)
           jpos = iw(jcol)

           if ( jpos == 0 ) then
              len = len + 1
              if ( nzmax < len ) then
                 ierr_a = iii
                 return
              end if
              jc(len) = jcol
              iw(jcol)= len
              if ( values ) then
        !       c(len) = scal * b(kb)
                if (jcol.eq.iii) pdv_nbq(iii)=pdv_nbq(iii)     - scal * b(kb)
                if (jcol.lt.iii) plv_nbq(iii-1)=plv_nbq(iii-1) - scal * b(kb)
                if (jcol.gt.iii) puv_nbq(iii)=puv_nbq(iii)     - scal * b(kb)
              end if
           else
              if ( values ) then
        !        c(jpos) = c(jpos) + scal * b(kb)
                if (jcol.eq.iii) pdv_nbq(iii)=pdv_nbq(iii)     - scal * b(kb)
                if (jcol.lt.iii) plv_nbq(iii-1)=plv_nbq(iii-1) - scal * b(kb)
                if (jcol.gt.iii) puv_nbq(iii)=puv_nbq(iii)     - scal * b(kb)
              end if
           end if

         end do

    end do

    do k = ic(iii), len
      iw(jc(k)) = 0
    end do

    ic(iii+1) = len + 1

  end do

  return
end

#else
      subroutine amub2_tri_empty
      end subroutine amub2_tri_empty
#endif
