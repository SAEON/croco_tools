#include "cppdefs.h"
#ifdef NBQ

      subroutine grid_def_nh

!******************************************************************************************
!                   Numbering of Mass Points
!
!******************************************************************************************

      use module_nh
      use module_nbq

      implicit none

# include "param_F90.h"
# include "scalars_F90.h"
# include "grid.h"

#include "def_bounds.h"

!******************************************************************************************
! Inner MPI domain:
! cf: http://poc.obs-mip.fr/auclair/WOcean.fr/SNH/Restricted/NH-NBQ/Html_pages/Algebrique_Numerotation_Base.htm
!******************************************************************************************


!*******************************************************************
!     NH domain:  mass grid-points
!*******************************************************************

       istr_nh  = 1
       iend_nh  = LOCALLM
       jstr_nh  = 1
       jend_nh  = LOCALMM 

!*******************************************************************
!     NH domain:  velocity grid-points
!*******************************************************************

       istru_nh = 2
       iendu_nh = LOCALLM 
       jstrv_nh = 2
       jendv_nh = LOCALMM 

#ifdef MPI 
      if (WEST_INTER) then
       istru_nh = 1
      endif

      if (SOUTH_INTER) then
       jstrv_nh = 1
      endif
#endif

      return

      end subroutine grid_def_nh
 
#else
        subroutine grid_def_nh_empty
        return
        end 
#endif
