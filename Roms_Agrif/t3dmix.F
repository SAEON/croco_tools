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

c# ifdef DIAGNOSTICS_TS
c!
c!==========================================================
c! Tracer diagnostics
c!==========================================================
c
c      do k=1,N
c        do j=jstr,jend
c          do i=istr,iend
c            cff1=pm(i,j)*pn(i,j)
c            THmix(i,j,k,itrc)=FX(i+1,j)-FX(i,j)
c     &                       +FE(i,j+1)-FE(i,j)
c#  ifndef MIX_S_TS
c     &                       +(FC(i,j,k2)-FC(i,j,k1))/cff1
c# endif
c#  ifdef MASKING
c     &                                        * rmask(i,j)
c#  endif
c            Trate(i,j,k,itrc)=(Hz(i,j,k)*t(i,j,k,nnew,itrc)
c     &                    -Hz_bak(i,j,k)*t(i,j,k,nstp,itrc))
c     &                                            /(dt*cff1)
c#  ifdef MASKING
c     &                                        * rmask(i,j)
c#  endif
c!
c! Divide all diagnostic terms by the cell volume
c! (Hz(i,j,k,itrc)/(pm(i,j).*pn(i,j)). There after the unit
c! of diag terms are: (unit of tracers)* s-1.
c!
c            THmix(i,j,k,itrc)=THmix(i,j,k,itrc)*cff1/Hz(i,j,k)
c            Trate(i,j,k,itrc)=Trate(i,j,k,itrc)*cff1/Hz(i,j,k)
c          enddo
c        enddo
c      enddo 
c!
c! Compute tracer diagnostics averaged over the MLD
c!
c#  ifdef DIAGNOSTICS_TS_MLD
c       do j=Jstr,Jend
c         do i=Istr,Iend
c            THmix_mld(i,j,itrc)=0.
c            Trate_mld(i,j,itrc)=0.
c         enddo
c       enddo
c       do j=Jstr,Jend
c         do i=Istr,Iend
c#   ifdef LMD_SKPP
c           kmld=kbl(i,j)
c#   else
c           kmld=N-5
c#   endif
c           do k=N,kmld,-1
c             cff=Hz(i,j,k)/(z_w(i,j,N)-z_w(i,j,kmld-1))
c             THmix_mld(i,j,itrc)=THmix_mld(i,j,itrc)+
c     &                           THmix(i,j,k,itrc)*cff
c             Trate_mld(i,j,itrc)=Trate_mld(i,j,itrc)+
c     &                           Trate(i,j,k,itrc)*cff
c           enddo
c         enddo
c       enddo
c#  endif
c# endif /* DIAGNOSTICS_TS */


#else
      subroutine t3dmix_empty
      end
#endif

