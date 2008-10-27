!
! $Id: uv3dmix_S.F,v 1.3 2005/10/10 13:40:19 pmarches Exp $
!
#include "cppdefs.h"
# if defined SOLVE3D && defined AGRIF
 
      subroutine uv3dmix_fine (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()

      call uv3dmix_fine_tile (Istr,Iend,Jstr,Jend)
      return
      end
 
      subroutine uv3dmix_fine_tile (Istr,Iend,Jstr,Jend)
!
! Computes harmonic mixing of momentum, along constant S-surfaces
! as horizontal divergence of the stress tensor. Components of the
! stress tensor are:
!                         du     dv
!         s_xx = -s_yy = ---- - -----
!                         dx     dy
!
!                         du     dv
!         s_xy =  s_yx = ---- + ----
!                         dy     dx
!
! Reference:
!
! Wajsowicz, R.C, 1993: A consistent formulation of the anisotropic
!     stress tensor for use in models of the large-scale ocean
!     circulation, JCP, 105, 333-338.
!
! Sadourny, R. and K. Maynard, 1997: Formulations of lateral
!     diffusion in geophysical fluid dynamics models, In "Numerical
!     Methods of Atmospheric and Oceanic Modelling". Lin, Laprise,
!     and Ritchie, Eds., NRC Research Press, 547-556.
!
! Griffies, S.M. and R.W. Hallberg, 2000: Biharmonic friction with
!     a Smagorinsky-like viscosity for use in large-scale eddy-
!     permitting ocean models, Mon. Wea. Rev., 128, 8, 2935-2946.
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k, indx
      real UFe(PRIVATE_2D_SCRATCH_ARRAY),
     &     UFx(PRIVATE_2D_SCRATCH_ARRAY), cff,
     &     VFe(PRIVATE_2D_SCRATCH_ARRAY),
     &     VFx(PRIVATE_2D_SCRATCH_ARRAY)
     
# include "param.h"
# include "scalars.h"
# include "grid.h"
# include "ocean3d.h"
# include "coupling.h"
# include "mixing.h"
# include "zoom.h"
# ifdef DIAGNOSTICS_UV
#  include "diagnostics.h"
# endif
!
# ifdef MPI
#  define LOCALLM Lmmpi
#  define LOCALMM Mmmpi
# else
#  define LOCALLM Lm
#  define LOCALMM Mm
# endif
       integer decal
       real tinterp,onemtinterp, rrhot
       integer :: nold
       integer :: irhot
       external interpspongeu, interpspongev
!
# include "compute_auxiliary_bounds.h"

      irhot = Agrif_Irhot()
      rrhot = real(irhot)
      decal = 2*max(Agrif_Irhox(),Agrif_Irhoy())+1
                       
      If (nbcoarse == 1) THEN
      do k=1,N
      do j=JstrR,JendR
      do i=IstrR,IendR
      usponge(i,j,k) = 0.
      vsponge(i,j,k) = 0.
      enddo
      enddo
      enddo
      
C$OMP BARRIER
C$OMP MASTER

      Call Agrif_Set_bc(usponge,(/-decal,0/),
     &         InterpolationShouldbemade=.TRUE.)
      Call Agrif_Set_bc(vsponge,(/-decal,0/),
     &         InterpolationShouldbemade=.TRUE.)

       tinterp = 1.
               
      Call Agrif_Bc_Variable(usponge,usponge,calledweight=tinterp,
     &                         procname=interpspongeu)
      Call Agrif_Bc_Variable(vsponge,vsponge,calledweight=tinterp,
     &                         procname=interpspongev)

      UTimesponge = 3 - UTimesponge
C$OMP END MASTER
C$OMP BARRIER

       if (SOUTHERN_EDGE) then  
         do k=1,N
         do j=0,decal
         do i=Istr-1,Iend+1
          U_sponge_south(i,j,k,UTimesponge)=
     &              usponge(i,j,k)       
          V_sponge_south(i,j,k,UTimesponge)=
     &              vsponge(i,j,k)         
         enddo
         enddo
         enddo
       endif
                     
      if (NORTHERN_EDGE) then
         do k=1,N
         do j=Jend-decal,Jend+1
         do i=Istr-1,Iend+1
          U_sponge_north(i,j,k,UTimesponge)=
     &              usponge(i,j,k)          
          V_sponge_north(i,j,k,UTimesponge)=
     &              vsponge(i,j,k)     
         enddo
         enddo
         enddo
       endif               


      if (WESTERN_EDGE) then
         do k=1,N
         do j=Jstr-1,Jend+1
         do i=0,decal      
          U_sponge_west(i,j,k,UTimesponge)=
     &              usponge(i,j,k)       
          V_sponge_west(i,j,k,UTimesponge)=
     &              vsponge(i,j,k)        
         enddo
         enddo
         enddo
       endif
                    
      if (EASTERN_EDGE) then  
         do k=1,N
         do j=Jstr-1,Jend+1
         do i=Iend-decal,Iend+1    
          U_sponge_east(i,j,k,UTimesponge)=
     &              usponge(i,j,k)    
          V_sponge_east(i,j,k,UTimesponge)=
     &              vsponge(i,j,k)             
         enddo
         enddo
         enddo  
       endif
      
C$OMP BARRIER 
 
      ENDIF    


       tinterp = real(nbcoarse-1)/rrhot
       IF (nbstep3d .LT. irhot) tinterp = 0.
       onemtinterp = -tinterp
       tinterp = 1.+tinterp
       
       nold = 3 - UTimesponge

       if (SOUTHERN_EDGE) then
       do k=1,N
       do j=0,decal
       do i=Istr-1,Iend+1
          usponge(i,j,k) =
     &          onemtinterp*U_sponge_south(i,j,k,nold)
     &       +tinterp*U_sponge_south(i,j,k,UTimesponge)

          vsponge(i,j,k) =
     &   onemtinterp*V_sponge_south(i,j,k,nold)
     &       +tinterp*V_sponge_south(i,j,k,UTimesponge)  
       enddo
       enddo
       enddo
       endif
       
       if (NORTHERN_EDGE) then
       do k=1,N
       do j=Jend-decal,Jend+1
       do i=Istr-1,Iend+1
          usponge(i,j,k) =
     &       onemtinterp*U_sponge_north(i,j,k,nold)
     &       +tinterp*U_sponge_north(i,j,k,UTimesponge)
          vsponge(i,j,k) =
     &       onemtinterp*V_sponge_north(i,j,k,nold)
     &       +tinterp*V_sponge_north(i,j,k,UTimesponge)       
       enddo
       enddo
       enddo           
       endif
       
      if (WESTERN_EDGE) then
         do k=1,N
         do j=Jstr-1,Jend+1
         do i=0,decal
          usponge(i,j,k) =
     &       onemtinterp*U_sponge_west(i,j,k,nold)
     &       +tinterp*U_sponge_west(i,j,k,UTimesponge)
          vsponge(i,j,k) =
     &       onemtinterp*V_sponge_west(i,j,k,nold)
     &       +tinterp*V_sponge_west(i,j,k,UTimesponge)
         enddo
         enddo
         enddo
       endif      
       
      if (EASTERN_EDGE) then    
         do k=1,N
         do j=Jstr-1,Jend+1
         do i=Iend-decal,Iend+1    
          usponge(i,j,k) =
     &       onemtinterp*U_sponge_east(i,j,k,nold)
     &       +tinterp*U_sponge_east(i,j,k,UTimesponge)
          vsponge(i,j,k) =
     &       onemtinterp*V_sponge_east(i,j,k,nold)
     &       +tinterp*V_sponge_east(i,j,k,UTimesponge)       
         enddo
         enddo
         enddo 
       endif
                          
      indx=3-nstp    !--> time index for target arrays;

      return
      end
      
      subroutine interpspongeu(tabres,i1,i2,j1,j2,k1,k2)
      implicit none
# include "param.h"
# include "ocean3d.h"
# include "scalars.h"

       integer i1,i2,j1,j2,k1,k2
       real tabres(i1:i2,j1:j2,k1:k2)
       
       tabres(i1:i2,j1:j2,k1:k2) = u(i1:i2,j1:j2,k1:k2,nstp)
             
      return
      end
      
      subroutine interpspongev(tabres,i1,i2,j1,j2,k1,k2)
      implicit none
# include "param.h"
# include "ocean3d.h"
# include "scalars.h"

       integer i1,i2,j1,j2,k1,k2
       real tabres(i1:i2,j1:j2,k1:k2)
       
       tabres(i1:i2,j1:j2,k1:k2) = v(i1:i2,j1:j2,k1:k2,nstp)
             
      return
      end      
            
#else
      subroutine nestingvisc3d_empty
      end
#endif
 
