# include "cppdefs.h"
# if defined SOLVE3D && defined NBQ
      subroutine w3dbc_tile(Istr,Iend,Jstr,Jend,grad)

# include "set_obc_definitions.h"
!
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "climat.h"
# include "scalars.h"
# include "boundary.h"
      integer Istr,Iend,Jstr,Jend,i,j,k
      real    grad(PRIVATE_2D_SCRATCH_ARRAY)
      real    eps, cff,
     &        cx,cy, dft,dfx,dfy, tau,tau_in,tau_out
      parameter (eps=1.E-20)
!
# include "compute_auxiliary_bounds.h"
!
! Interpolations of the parent values to get wbry_east or wclm
!
# if defined W_FRC_BRY || defined WNUDGING
      tau_in=dt*tauT_in
      tau_out=dt*tauT_out
# endif
!
# ifndef EW_COM_PERIODIC
!
!====================================================================
!                            WESTERN BC
!====================================================================
      if (WESTERN_EDGE) then
#  if defined OBC_COM_WEST && defined OBC_COM_TORLANSKI
!                                           Western edge radiation BC
!                                           ======= ==== ========= ==
        do k=1,N
          do j=Jstr,Jend+1
            grad(Istr-1,j)=( wz(Istr-1,j  ,k,nnew)
     &                      -wz(Istr-1,j-1,k,nnew))
#   ifdef MASKING
     &                                 *umask(Istr-1,j)
#   endif
            grad(Istr  ,j)=( wz(Istr  ,j  ,k,nnew)
     &                      -we(Istr  ,j-1,k,nnew))
#   ifdef MASKING
     &                                   *umask(Istr,j)
#   endif
          enddo
          do j=Jstr,Jend
            dft=wz(Istr,j,k,nstp)-wz(Istr  ,j,k,nnew)
            dfx=wz(Istr,j,k,nnew)-wz(Istr+1,j,k,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                  ! <-- cancel, if inflow 
#   if defined W_FRC_BRY || defined WNUDGING
              tau=tau_in
            else
              tau=tau_out
#   endif
            endif
 
            if (dft*(grad(Istr,j)+grad(Istr,j+1)) .gt. 0.) then
              dfy=grad(Istr,j)
            else
              dfy=grad(Istr,j+1)
            endif
 
#   ifdef OBC_COM_RAD_NORMAL
            dfy=0.
#   endif
            cff=max(dfx*dfx+dfy*dfy, eps)
            cx=dft*dfx
#   ifdef OBC_COM_RAD_NPO
            cy=0.
#   else
            cy=min(cff,max(dft*dfy,-cff))
#   endif
            wz(Istr-1,j,k,nnew)=( cff*wz(Istr-1,j,k,nstp)
     &                            +cx*wz(Istr  ,j,k,nnew)
     &                                -max(cy,0.)*grad(Istr-1,j  )
     &                                -min(cy,0.)*grad(Istr-1,j+1)
     &                                                 )/(cff+cx)
#   if defined W_FRC_BRY || defined WNUDGING
            wz(Istr-1,j,k,nnew)=(1.-tau)*wz(Istr-1,j,k,nnew)
#    ifdef W_FRC_BRY
     &                                    +tau*wbry_weswz(j,k)     
#    else
     &                                    +tau*wclm(Istr-1,j,k)
#    endif
#   endif
#   ifdef MASKING
            wz(Istr-1,j,k,nnew)=wz(Istr-1,j,k,nnew)
     &                                      *rmask(Istr-1,j)
#   endif
          enddo
        enddo
#  elif defined OBC_COM_WEST && defined OBC_COM_WUPWIND
!                                               Western edge Upwind BC
!                                               ======= ==== ====== ==
        do k=1,N
          do j=Jstr,Jend
            if (u(Istr+1,j,k,nstp).lt.0.) then
              wz(Istr-1,j,k,nnew)=wz(Istr,j,k,nnew)
            else
#   ifdef W_FRC_BRY
              wz(Istr-1,j,k,nnew)=wbry_weswz(j,k)
#   else
              wz(Istr-1,j,k,nnew)=wclm(Istr-1,j,k)
#   endif
           endif
#   ifdef MASKING
              wz(Istr-1,j,k,nnew)=wz(Istr-1,j,k,nnew)
     &                                       *rmask(Istr-1,j)
#   endif
         enddo
        enddo
#  elif defined OBC_COM_WEST && defined OBC_COM_WSPECIFIED
!                                            Western edge Specified BC
!                                            ======= ==== ========= ==
        do k=1,N
          do j=Jstr,Jend
#   ifdef W_FRC_BRY
              wz(Istr-1,j,k,nnew)=wbry_weswz(j,k)
#   else
              wz(Istr-1,j,k,nnew)=wclm(Istr-1,j,k)
#   endif
#   ifdef MASKING
     &                                       *rmask(Istr-1,j)
#   endif
         enddo
        enddo
#  else
        do k=1,N
          do j=Jstr,Jend
!                                            Western edge gradient BC
!                                            ======= ==== ======== ==
            wz(Istr-1,j,k,nnew)=wz(Istr,j,k,nnew)
#   ifdef MASKING
     &                              *rmask(Istr-1,j)
#   endif
          enddo
        enddo
#  endif
      endif     ! <-- WESTERN_EDGE
!
!====================================================================
!                            EASTERN BC
!====================================================================
      if (EASTERN_EDGE) then
#  if defined OBC_COM_EAST && defined OBC_COM_WORLANSKI
!                                           Eastern edge radiation BC
!                                           ======= ==== ========= ==
        do k=1,N
          do j=Jstr,Jend+1
           grad(Iend  ,j)=( wz(Iend  ,j  ,k,nstp)
     &                     -wz(Iend  ,j-1,k,nstp))
#   ifdef MASKING
     &                                  *vmask(Iend,j)
#   endif
           grad(Iend+1,j)=( wz(Iend+1,j  ,k,nstp)
     &                     -wz(Iend+1,j-1,k,nstp))
#   ifdef MASKING
     &                                *vmask(Iend+1,j)
#   endif
          enddo
          do j=Jstr,Jend
            dft=wz(Iend,j,k,nstp)-wz(Iend  ,j,k,nnew)
            dfx=wz(Iend,j,k,nnew)-wz(Iend-1,j,k,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                  ! <-- cancel, if inflow 
#   if defined W_FRC_BRY || defined WNUDGING
              tau=tau_in
            else
              tau=tau_out
#   endif
            endif
 
            if (dft*(grad(Iend,j)+grad(Iend,j+1)) .gt. 0.) then
              dfy=grad(Iend,j)
            else
              dfy=grad(Iend,j+1)
            endif
 
#   ifdef OBC_COM_RAD_NORMAL
            dfy=0.
#   endif
            cff=max(dfx*dfx+dfy*dfy, eps)
            cx=dft*dfx
#   ifdef OBC_COM_RAD_NPO
            cy=0.
#   else
            cy=min(cff,max(dft*dfy,-cff))
#   endif
 
            wz(Iend+1,j,k,nnew)=( cff*wz(Iend+1,j,k,nstp)
     &                                   +cx*wz(Iend,j,k,nnew)
     &                                -max(cy,0.)*grad(Iend+1,j  )
     &                                -min(cy,0.)*grad(Iend+1,j+1)
     &                                                 )/(cff+cx)
#   if defined W_FRC_BRY || defined WNUDGING
            wz(Iend+1,j,k,nnew)=(1.-tau)*wz(Iend+1,j,k,nnew)
#    ifdef T_FRC_BRY
     &                                      +tau*wbry_easwz(j,k)
#    else
     &                                    +tau*wclm(Iend+1,j,k)
#    endif
#   endif
#   ifdef MASKING
            wz(Iend+1,j,k,nnew)=wz(Iend+1,j,k,nnew)
     &                                     *rmask(Iend+1,j)
#   endif
          enddo
        enddo
#  elif defined OBC_COM_EAST && defined OBC_COM_WUPWIND
!                                            Eastern edge Upwind BC
!                                            ======= ==== ====== ==
        do k=1,N
          do j=Jstr,Jend
            if (u(Iend-1,j,k,nstp).gt.0.) then
              wz(Iend+1,j,k,nnew)=wz(Iend,j,k,nnew)
            else
#   ifdef T_FRC_BRY
              wz(Iend+1,j,k,nnew)=wbry_easwz(j,k)
#   else
              wz(Iend+1,j,k,nnew)=wclm(Iend+1,j,k)
#   endif
            endif
#   ifdef MASKING
              wz(Iend+1,j,k,nnew)=wz(Iend+1,j,k,nnew)
     &                                       *rmask(Iend+1,j)
#   endif
          enddo
        enddo
#  elif defined OBC_COM_EAST && defined OBC_COM_WSPECIFIED
!                                            Eastern edge Specified BC
!                                            ======= ==== ========= ==
        do k=1,N
          do j=Jstr,Jend
#   ifdef T_FRC_BRY
            wz(Iend+1,j,k,nnew)=wbry_easwz(j,k) 
#   else
            wz(Iend+1,j,k,nnew)=wclm(Iend+1,j,k)
#   endif
#   ifdef MASKING
     &                                   *rmask(Iend+1,j)
#   endif
          enddo
        enddo
#  else 
!                                            Eastern edge gradient BC
!                                            ======= ==== ======== ==
        do k=1,N
          do j=Jstr,Jend
            wz(Iend+1,j,k,nnew)=wz(Iend,j,k,nnew)
#   ifdef MASKING
     &                                   *rmask(Iend+1,j)
#   endif
          enddo
        enddo
#  endif
      endif    ! <-- EASTERN_EDGE
# endif        /* !EW_COM_PERIODIC */

# ifndef NS_COM_PERIODIC
!
!====================================================================
!                            SOUTHERN BC
!====================================================================
      if (SOUTHERN_EDGE) then
#  if defined OBC_COM_SOUTH && defined OBC_COM_WORLANSKI
!                                          Southern edge radiation BC
!                                          ======== ==== ========= ==
        do k=1,N
          do i=Istr,Iend+1
            grad(i,Jstr  )=( wz(i  ,Jstr  ,k,nstp)
     &                      -wz(i-1,Jstr  ,k,nstp))
#   ifdef MASKING
     &                                   *umask(i,Jstr)
#   endif
            grad(i,Jstr-1)=( wz(i  ,Jstr-1,k,nstp)
     &                      -wz(i-1,Jstr-1,k,nstp))
#   ifdef MASKING
     &                                *umask(i,Jstr-1)
#   endif
          enddo
          do i=Istr,Iend
            dft=wz(i,Jstr,k,nstp)-wz(i,Jstr  ,k,nnew)
            dfx=wz(i,Jstr,k,nnew)-wz(i,Jstr+1,k,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                   ! <-- cancel, if inflow 
#   if defined T_FRC_BRY || defined TNUDGING
              tau=tau_in
            else
              tau=tau_out
#   endif
            endif
 
            if (dft*(grad(i,Jstr)+grad(i+1,Jstr)) .gt. 0.) then
              dfy=grad(i,Jstr)
            else
              dfy=grad(i+1,Jstr)
            endif
 
#   ifdef OBC_COM_RAD_NORMAL
            dfy=0.
#   endif
            cff=max(dfx*dfx+dfy*dfy, eps)
            cx=dft*dfx
#   ifdef OBC_COM_RAD_NPO
            cy=0.
#   else
            cy=min(cff,max(dft*dfy,-cff))
#   endif
 
            wz(i,Jstr-1,k,nnew)=( cff*wz(i,Jstr-1,k,nstp)
     &                                   +cx*wz(i,Jstr,k,nnew)
     &                                -max(cy,0.)*grad(i  ,Jstr-1)
     &                                -min(cy,0.)*grad(i+1,Jstr-1)
     &                                                 )/(cff+cx)
#   if defined W_FRC_BRY || defined WNUDGING
            wz(i,Jstr-1,k,nnew)=(1.-tau)*wz(i,Jstr-1,k,nnew)
#    ifdef T_FRC_BRY
     &                                     +tau*wbry_south(i,k)
#    else
     &                                    +tau*wclm(i,Jstr-1,k)
#    endif
#   endif
#   ifdef MASKING
            wz(i,Jstr-1,k,nnew)=wz(i,Jstr-1,k,nnew)
     &                                     *rmask(i,Jstr-1)
#   endif
          enddo
        enddo
#  elif defined OBC_COM_SOUTH && defined OBC_COM_WUPWIND
!                                           Southern edge Upwind BC
!                                           ======== ==== ====== ==
        do k=1,N
          do i=Istr,Iend
            if (v(i,Jstr+1,k,nstp).lt.0.) then
              wz(i,Jstr-1,k,nnew)=wz(i,Jstr,k,nnew)
            else
#   ifdef T_FRC_BRY
              wz(i,Jstr-1,k,nnew)=wbry_south(i,k)
#   else
              wz(i,Jstr-1,k,nnew)=wclm(i,Jstr-1,k)
#   endif
            endif
#   ifdef MASKING
              wz(i,Jstr-1,k,nnew)=wz(i,Jstr-1,k,nnew)
     &                                       *rmask(i,Jstr-1)
#   endif
          enddo
        enddo
#  elif defined OBC_COM_SOUTH && defined OBC_COM_WSPECIFIED
!                                           Southern edge Specified BC
!                                           ======== ==== ========= ==
        do k=1,N
          do i=Istr,Iend
#   ifdef T_FRC_BRY
            wz(i,Jstr-1,k,nnew)=wbry_south(i,k)
#   else
            wz(i,Jstr-1,k,nnew)=wclm(i,Jstr-1,k)
#   endif
#   ifdef MASKING
     &                                   *rmask(i,Jstr-1)
#   endif
          enddo
        enddo
#  else
!                                           Southern edge gradient BC
!                                           ======== ==== ======== ==
        do k=1,N
          do i=Istr,Iend
            wz(i,Jstr-1,k,nnew)=wz(i,Jstr,k,nnew)
#   ifdef MASKING
     &                                   *rmask(i,Jstr-1)
#   endif
          enddo
        enddo
#  endif
      endif    ! <-- SOUTHERN_EDGE
!
!====================================================================
!                            NORTHERN BC
!====================================================================
      if (NORTHERN_EDGE) then
#  if defined OBC_COM_NORTH && defined OBC_COM_WORLANSKI
!                                          Northern edge radiation BC
!                                          ======== ==== ========= ==
        do k=1,N
          do i=Istr,Iend+1
            grad(i,Jend  )=( wz(i  ,Jend  ,k,nstp)
     &                      -wz(i-1,Jend  ,k,nstp))
#   ifdef MASKING
     &                                   *umask(i,Jend)
#   endif
            grad(i,Jend+1)=( wz(i  ,Jend+1,k,nstp)
     &                      -wz(i-1,Jend+1,k,nstp))
#   ifdef MASKING
     &                                 *umask(i,Jend+1)
#   endif
          enddo
          do i=Istr,Iend
            dft=wz(i,Jend,k,nstp)-wz(i,Jend  ,k,nnew)
            dfx=wz(i,Jend,k,nnew)-wz(i,Jend-1,k,nnew)
 
            if (dfx*dft .lt. 0.) then
              dft=0.                          ! <-- cancel, if inflow 
#   if defined W_FRC_BRY || defined WNUDGING
              tau=tau_in
            else
              tau=tau_out
#   endif
            endif
 
            if (dft*(grad(i,Jend)+grad(i+1,Jend)) .gt. 0.) then
              dfy=grad(i,Jend)
            else
              dfy=grad(i+1,Jend)
            endif
 
#   ifdef OBC_COM_RAD_NORMAL
            dfy=0.
#   endif
            cff=max(dfx*dfx+dfy*dfy, eps)
            cx=dft*dfx
#   ifdef OBC_COM_RAD_NPO
            cy=0.
#   else
            cy=min(cff,max(dft*dfy,-cff))
#   endif
            wz(i,Jend+1,k,nnew)=( cff*wz(i,Jend+1,k,nstp)
     &                                 +cx*wz(i,Jend  ,k,nnew)
     &                                -max(cy,0.)*grad(i  ,Jend+1)
     &                                -min(cy,0.)*grad(i+1,Jend+1)
     &                                                 )/(cff+cx)
#   if defined W_FRC_BRY || defined WNUDGING
            wz(i,Jend+1,k,nnew)=(1.-tau)*wz(i,Jend+1,k,nnew)
#    ifdef W_FRC_BRY
     &                                     +tau*wbry_north(i,k)
#    else
     &                                    +tau*wclm(i,Jend+1,k)
#    endif
#   endif
#   ifdef MASKING
            wz(i,Jend+1,k,nnew)=wz(i,Jend+1,k,nnew)
     &                                     *rmask(i,Jend+1)
#   endif
          enddo
        enddo
#  elif defined OBC_COM_NORTH && defined OBC_COM_WUPWIND
!                                           Northern edge Upwind BC
!                                           ======== ==== ====== ==
        do k=1,N
          do i=Istr,Iend
            if (v(i,Jend-1,k,nstp).gt.0.) then
              wz(i,Jend+1,k,nnew)=wz(i,Jend,k,nnew)
            else
#   ifdef T_FRC_BRY
              wz(i,Jend+1,k,nnew)=wbry_north(i,k)
#   else
              wz(i,Jend+1,k,nnew)=wclm(i,Jend+1,k)
#   endif
            endif
#   ifdef MASKING
              wz(i,Jend+1,k,nnew)=wz(i,Jend+1,k,nnew)
     &                                       *rmask(i,Jend+1)
#   endif
          enddo
        enddo
#  elif defined OBC_COM_NORTH && defined OBC_COM_WSPECIFIED
!                                           Northern edge Specified BC
!                                           ======== ==== ========= ==
        do k=1,N
          do i=Istr,Iend
#   ifdef T_FRC_BRY
            wz(i,Jend+1,k,nnew)=wbry_north(i,k)
#   else
            wz(i,Jend+1,k,nnew)=wclm(i,Jend+1,k)
#   endif
#   ifdef MASKING
     &                                   *rmask(i,Jend+1)
#   endif
          enddo
        enddo
#  else
!                                           Northern edge gradient BC
!                                           ======== ==== ======== ==
        do k=1,N
          do i=Istr,Iend
            wz(i,Jend+1,k,nnew)=wz(i,Jend,k,nnew)
#   ifdef MASKING
     &                                   *rmask(i,Jend+1)
#   endif
          enddo
        enddo
#  endif
      endif    ! <-- NORTHERN_EDGE
# endif /* ! NS_COM_PERIODIC */

                           ! Corners between adjacent open boundaries
                           ! ======= ======= ======== ==== ==========

# if defined OBC_COM_SOUTH && defined OBC_COM_WEST
      if (SOUTHERN_EDGE .and. WESTERN_EDGE) then
        do k=1,N
          wz(Istr-1,Jstr-1,k,nnew)=0.5*
     &                          ( wz(Istr,Jstr-1,k,nnew)
     &                           +wz(Istr-1,Jstr,k,nnew))
#   ifdef MASKING
     &                                 *rmask(Istr-1,Jstr-1)
#   endif
        enddo
      endif
# endif
# if defined OBC_COM_SOUTH && defined OBC_COM_EAST
      if (SOUTHERN_EDGE .and. EASTERN_EDGE) then
        do k=1,N
          wz(Iend+1,Jstr-1,k,nnew)=0.5*
     &                          (wz(Iend,Jstr-1,k,nnew)
     &                          +wz(Iend+1,Jstr,k,nnew))
#   ifdef MASKING
     &                                 *rmask(Iend+1,Jstr-1)
#   endif
        enddo
      endif
# endif
# if defined OBC_COM_NORTH && defined OBC_COM_WEST
      if (NORTHERN_EDGE .and. WESTERN_EDGE) then
        do k=1,N
          wz(Istr-1,Jend+1,k,nnew)=0.5*
     &                          ( wz(Istr,Jend+1,k,nnew)
     &                           +wz(Istr-1,Jend,k,nnew))
#   ifdef MASKING
     &                                 *rmask(Istr-1,Jend+1)
#   endif
        enddo
      endif
# endif
# if defined OBC_COM_NORTH && defined OBC_COM_EAST
      if (NORTHERN_EDGE .and. EASTERN_EDGE) then
        do k=1,N
          wz(Iend+1,Jend+1,k,nnew)=0.5*
     &                          ( wz(Iend,Jend+1,k,nnew)
     &                           +wz(Iend+1,Jend,k,nnew))
#   ifdef MASKING
     &                                 *rmask(Iend+1,Jend+1)
#   endif
        enddo
      endif
# endif
      return
      end
#else
      subroutine w3dbc_empty
      end
#endif /* SOLVE3D && NBQ */
