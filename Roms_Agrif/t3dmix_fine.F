!
! $Id: t3dmix_S.F,v 1.4 2005/10/10 13:40:19 pmarches Exp $
!
#include "cppdefs.h"
#if defined SOLVE3D && defined AGRIF
      subroutine t3dmix_fine (tile)
      implicit none
      integer tile, trd, omp_get_thread_num
# include "param.h"
# include "private_scratch.h"
# include "compute_tile_bounds.h"
      trd=omp_get_thread_num()
      
        call t3dmix_fine_tile (Istr,Iend,Jstr,Jend)
      
      return
      end

      subroutine t3dmix_fine_tile (Istr,Iend,Jstr,Jend)

      implicit none
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "mixing.h"
# include "climat.h"
# include "scalars.h"
# include "zoom.h"
      integer itrc, Istr,Iend,Jstr,Jend, i,j,k
!
# ifdef MPI
#  define LOCALLM Lmmpi
#  define LOCALMM Mmmpi
# else
#  define LOCALLM Lm
#  define LOCALMM Mm
# endif
!
       integer decal
       real maxdiff
       real tinterp,onemtinterp, rrhot
       integer :: nold
       integer :: irhot
       external interpsponget
!
# include "compute_auxiliary_bounds.h"
!

      irhot = Agrif_Irhot()
      rrhot = real(irhot)
      decal = 2*max(Agrif_Irhox(),Agrif_Irhoy())+1
      If (nbcoarse == 1) THEN
      do itrc=1,NT
      do k=1,N
      do j=JstrR,JendR
      do i=IstrR,IendR
      tsponge(i,j,k,itrc) = 0.
      enddo
      enddo
      enddo
      enddo
C$OMP BARRIER
C$OMP MASTER
      
      Call Agrif_Set_bc(tsponge,(/-decal,0/),
     &     InterpolationShouldbemade=.TRUE.)
#ifdef MASKING
        Agrif_UseSpecialvalue=.true.
#endif
      Agrif_Specialvalue=0.

       tinterp = 1.

      Call Agrif_Bc_Variable(tsponge,tsponge,calledweight=tinterp,
     &                           procname=interpsponget)
      Agrif_UseSpecialvalue=.false.

       TTimesponge = 3 - TTimesponge
       
C$OMP END MASTER
C$OMP BARRIER
       
       if (SOUTHERN_EDGE) then  
         do itrc=1,NT
         do k=1,N
         do j=0,decal
         do i=Istr-1,Iend+1 
          T_sponge_south(i,j,k,TTimesponge,itrc)=
     &              tsponge(i,j,k,itrc)          
         enddo
         enddo
         enddo
         enddo
       endif
                     
      if (NORTHERN_EDGE) then
         do itrc=1,NT
         do k=1,N
         do j=Jend-decal,Jend+1
         do i=Istr-1,Iend+1     
          T_sponge_north(i,j,k,TTimesponge,itrc)=
     &              tsponge(i,j,k,itrc)          
         enddo
         enddo
         enddo
         enddo
       endif               


      if (WESTERN_EDGE) then
         do itrc=1,NT
         do k=1,N
         do j=Jstr-1,Jend+1
         do i=0,decal      
          T_sponge_west(i,j,k,TTimesponge,itrc)=
     &              tsponge(i,j,k,itrc)          
         enddo
         enddo
         enddo
         enddo
       endif
                    
      if (EASTERN_EDGE) then      
         do itrc=1,NT
         do k=1,N
         do j=Jstr-1,Jend+1
         do i=Iend-decal,Iend+1    
          T_sponge_east(i,j,k,TTimesponge,itrc)=
     &              tsponge(i,j,k,itrc)          
         enddo
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
       
       nold = 3 - TTimesponge

       if (SOUTHERN_EDGE) then
       do itrc=1,NT
       do k=1,N
       do j=0,decal
       do i=Istr-1,Iend+1
          tsponge(i,j,k,itrc) =
     &       onemtinterp*T_sponge_south(i,j,k,nold,itrc)
     &       +tinterp*T_sponge_south(i,j,k,TTimesponge,itrc)
       enddo
       enddo
       enddo
       enddo
       endif
       
       if (NORTHERN_EDGE) then
       do itrc=1,NT
       do k=1,N
       do j=Jend-decal,Jend+1
       do i=Istr-1,Iend+1
           tsponge(i,j,k,itrc) =
     &       onemtinterp*T_sponge_north(i,j,k,nold,itrc)
     &       +tinterp*T_sponge_north(i,j,k,TTimesponge,itrc)
       enddo
       enddo
       enddo
       enddo             
       endif
       
      if (WESTERN_EDGE) then
         do itrc=1,NT
         do k=1,N
         do j=Jstr-1,Jend+1
         do i=0,decal
            tsponge(i,j,k,itrc) =
     &                onemtinterp*T_sponge_west(i,j,k,nold,itrc)
     &                + tinterp*T_sponge_west(i,j,k,TTimesponge,itrc)
         enddo
         enddo
         enddo
         enddo
       endif      
       
      if (EASTERN_EDGE) then      
         do itrc=1,NT
         do k=1,N
         do j=Jstr-1,Jend+1
         do i=Iend-decal,Iend+1   
           tsponge(i,j,k,itrc)=
     &               onemtinterp*T_sponge_east(i,j,k,nold,itrc)
     &               + tinterp*T_sponge_east(i,j,k,TTimesponge,itrc)  
         enddo
         enddo
         enddo
         enddo  
       endif

      return
      end
      
      subroutine interpsponget(tabres,i1,i2,j1,j2,k1,k2,m1,m2)
      implicit none
# include "param.h"
# include "ocean3d.h"
# include "scalars.h"

       integer i1,i2,j1,j2,k1,k2,m1,m2
       real tabres(i1:i2,j1:j2,k1:k2,m1:m2)
       
       tabres(i1:i2,j1:j2,k1:k2,m1:m2) = 
     &               t(i1:i2,j1:j2,k1:k2,nstp,m1:m2)
      
      return
      end  
#else
      subroutine t3dmix_fine_empty
      end
# endif /* SOLVE3D && TS_DIF2 && MIX_S_TS */
