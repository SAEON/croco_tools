!
! $Id: t3dmix.F,v 1.3 2005/09/30 15:11:47 pmarches Exp $
!
#include "cppdefs.h"
#if defined SOLVE3D &&\
 (defined MIX_EN_TS || defined MIX_GP_TS || defined MIX_S_TS)

# if defined SOLVE3D && defined MIX_EN_TS
#  include "t3dmix_EN.F"
# elif defined SOLVE3D && defined MIX_GP_TS
#  include "t3dmix_GP.F"
# elif defined SOLVE3D && defined MIX_S_TS
#  include "t3dmix_S.F"
# endif

# ifdef DIAGNOSTICS_TS
!
!==========================================================
! Tracer diagnostics
!==========================================================
!
      do k=1,N
        do j=jstr,jend
          do i=istr,iend
            cff1=pm(i,j)*pn(i,j)
            THmix(i,j,k,itrc)=FX(i+1,j)-FX(i,j)
     &                       +FE(i,j+1)-FE(i,j)
#  ifndef MIX_S_TS
     &                       +(FC(i,j,k2)-FC(i,j,k1))/cff1
# endif
#  ifdef MASKING
     &                                        * rmask(i,j)
#  endif
            Trate(i,j,k,itrc)=(Hz(i,j,k)*t(i,j,k,nnew,itrc)
     &                    -Hz_bak(i,j,k)*t(i,j,k,nstp,itrc))
     &                                            /(dt*cff1)
#  ifdef MASKING
     &                                        * rmask(i,j)
#  endif
!
! Divide all diagnostic terms by the cell volume
! (Hz(i,j,k,itrc)/(pm(i,j).*pn(i,j)). There after the unit
! of diag terms are: (unit of tracers)* s-1.
!
            THmix(i,j,k,itrc)=THmix(i,j,k,itrc)*cff1/Hz(i,j,k)
            Trate(i,j,k,itrc)=Trate(i,j,k,itrc)*cff1/Hz(i,j,k)
          enddo
        enddo
      enddo 
!
! Compute tracer diagnostics averaged over the MLD
!
       do j=Jstr,Jend
         do i=Istr,Iend
            THmix_mld(i,j,itrc)=0.
            Trate_mld(i,j,itrc)=0.
         enddo
       enddo
       do j=Jstr,Jend
         do i=Istr,Iend
#  ifdef LMD_SKPP
           kmld=kbl(i,j)
#  else
           kmld=N-5
#  endif
           do k=N,kmld,-1
             cff=Hz(i,j,k)/(z_w(i,j,N)-z_w(i,j,kmld-1))
             THmix_mld(i,j,itrc)=THmix_mld(i,j,itrc)+
     &                           THmix(i,j,k,itrc)*cff
             Trate_mld(i,j,itrc)=Trate_mld(i,j,itrc)+
     &                           Trate(i,j,k,itrc)*cff
           enddo
         enddo
       enddo
# endif /* DIAGNOSTICS_TS */

!
!==========================================================
! Data exchange at the boundaries or interfaces
!==========================================================
!
! If diffusion in sponge layers is performed in the separate 
! sponge routine (t3dmix_spg) then message passing is 
! performed there
!
# if !(defined SPONGE_DIF2 && !defined TS_DIF2)
#  if defined EW_PERIODIC || defined NS_PERIODIC || defined MPI
        call exchange_r3d_tile (Istr,Iend,Jstr,Jend,
     &                          t(START_2D_ARRAY,1,nnew,itrc))
#  endif
# endif

       return
       end

#else
      subroutine t3dmix_empty
      end
#endif

