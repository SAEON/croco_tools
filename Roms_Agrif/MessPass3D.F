!
! $Id: MessPass3D.F,v 1.2 2003/12/17 13:55:58 pmarches Exp $
!
#include "cppdefs.h"
#if defined MPI && defined SOLVE3D

      subroutine MessPass3D_tile (Istr,Iend,Jstr,Jend, A, nmax)
!
! This subroutine is designed for ROMS-MPI code. It exchanges domain
! boundary information, including 2 ghost-cells in each direction.
! Ping Wang 9/20/99.
!
!     implicit none
# include "param.h"
# include "scalars.h"
      include 'mpif.h'

      integer nmax
      real A(GLOBAL_2D_ARRAY,nmax)
CSDISTRIBUTE_RESHAPE A(BLOCK_PATTERN) BLOCK_CLAUSE
      integer Istr,Iend,Jstr,Jend, i,j,k, isize,jsize,ksize,
     &        req(8), status(MPI_STATUS_SIZE,8), ierr
      integer iter, mdii, mdjj

      integer sub_X,size_X, sub_E,size_E, size_Z
#if ! defined AGRIF      
      parameter (size_Z=4*(N+1),
     &     sub_X=(Lm+NSUB_X-1)/NSUB_X,  size_X=2*(N+1)*(4+sub_X),
     &     sub_E=(Mm+NSUB_E-1)/NSUB_E,  size_E=2*(N+1)*(4+sub_E))

      real buf_snd4(size_Z),  ibuf_sndN(size_X),  buf_snd2(size_Z),
     &     buf_rev4(size_Z),  ibuf_revN(size_X),  buf_rev2(size_Z),
     &    jbuf_sndW(size_E),                      jbuf_sndE(size_E),
     &    jbuf_revW(size_E),                      jbuf_revE(size_E),
     &     buf_snd1(size_Z),  ibuf_sndS(size_X),  buf_snd3(size_Z),
     &     buf_rev1(size_Z),  ibuf_revS(size_X),  buf_rev3(size_Z)
#else
      real, dimension(:), allocatable ::
     &     buf_snd4,  ibuf_sndN,  buf_snd2,
     &     buf_rev4,  ibuf_revN,  buf_rev2,
     &    jbuf_sndW,                      jbuf_sndE,
     &    jbuf_revW,                      jbuf_revE,
     &     buf_snd1,  ibuf_sndS,  buf_snd3,
     &     buf_rev1,  ibuf_revS,  buf_rev3
#endif     

c**
c      common /buffers_3D/
c     &     buf_snd4,     ibuf_sndN,     buf_snd2,
c     &     buf_rev4,     ibuf_revN,     buf_rev2,
c
c     &    jbuf_sndW,                    jbuf_sndE,
c     &    jbuf_revW,                    jbuf_revE,
c
c     &     buf_snd1,     ibuf_sndS,     buf_snd3,
c     &     buf_rev1,     ibuf_revS,     buf_rev3
c**
!
#include "compute_message_bounds.h"

#if defined AGRIF
           size_Z=4*(N+1)
           sub_X=(Lm+NSUB_X-1)/NSUB_X
           size_X=2*(N+1)*(4+sub_X)
           sub_E=(Mm+NSUB_E-1)/NSUB_E
           size_E=2*(N+1)*(4+sub_E)
     
        Allocate(buf_snd4(size_Z),  ibuf_sndN(size_X),  buf_snd2(size_Z),
     &     buf_rev4(size_Z),  ibuf_revN(size_X),  buf_rev2(size_Z),
     &    jbuf_sndW(size_E),                      jbuf_sndE(size_E),
     &    jbuf_revW(size_E),                      jbuf_revE(size_E),
     &     buf_snd1(size_Z),  ibuf_sndS(size_X),  buf_snd3(size_Z),
     &     buf_rev1(size_Z),  ibuf_revS(size_X),  buf_rev3(size_Z))
#endif
! 
      ksize=4*nmax                    ! message sizes for
      isize=2*ishft*nmax              ! corner messages and sides
      jsize=2*jshft*nmax              ! in XI and ETA directions 
!
! Prepare to receive and send: sides....
!
                            !  Message passing split into two stages
                            !  in order to optimize Send-Recv pairing
                            !  in such a way that if one subdomain
      do iter=0,1           !  sends message to, say, its WESTERN
       mdii=mod(ii+iter,2)  !  neighbor, that neighbor is preparing
       mdjj=mod(jj+iter,2)  !  to receive this message first (i.e.
                            !  message coming from its EASTERN side),
                            !  rather than! rather than send his WEST
                            !  bound message, similarly to the first
                            !  subdomain.

!
! Prepare to receive and send: sides....
        if (mdii.eq.0) then

      if ((WEST_INTER)) then
        do k=1,nmax
          do j=jmin,jmax
            jbuf_sndW(k+nmax*(j-jmin      ))=A(1,j,k)
            jbuf_sndW(k+nmax*(j-jmin+jshft))=A(2,j,k)
          enddo
        enddo
        
        call MPI_Irecv (jbuf_revW, jsize, MPI_DOUBLE_PRECISION,
     &                     p_W, 2, MPI_COMM_WORLD, req(1), ierr)
     
        call MPI_Send  (jbuf_sndW, jsize, MPI_DOUBLE_PRECISION,
     &                     p_W, 1, MPI_COMM_WORLD,         ierr)
      endif

       else
       
      if (EAST_INTER) then
        do k=1,nmax
          do j=jmin,jmax
            jbuf_sndE(k+nmax*(j-jmin      ))=A(Lmmpi-1,j,k)
            jbuf_sndE(k+nmax*(j-jmin+jshft))=A(Lmmpi  ,j,k)
          enddo
        enddo
        
        call MPI_Irecv (jbuf_revE, jsize, MPI_DOUBLE_PRECISION,
     &                     p_E, 1, MPI_COMM_WORLD, req(2), ierr)
     
        call MPI_Send  (jbuf_sndE, jsize, MPI_DOUBLE_PRECISION,
     &                     p_E, 2, MPI_COMM_WORLD,         ierr)
      endif
      endif
       
      if (mdjj.eq.0) then
      if (SOUTH_INTER) then
      ibuf_snds = 0.
      
        do k=1,nmax
          do i=imin,imax
            ibuf_sndS(k+nmax*(i-imin      ))=A(i,1,k)
            ibuf_sndS(k+nmax*(i-imin+ishft))=A(i,2,k)
          enddo
        enddo
        call MPI_Irecv (ibuf_revS, isize, MPI_DOUBLE_PRECISION,
     &                     p_S, 4, MPI_COMM_WORLD, req(3), ierr)
        call MPI_Send  (ibuf_sndS, isize, MPI_DOUBLE_PRECISION,
     &                     p_S, 3, MPI_COMM_WORLD,         ierr)
      endif
      else
      if (NORTH_INTER) then
      
      ibuf_sndn = 0.
        do k=1,nmax
          do i=imin,imax
            ibuf_sndN(k+nmax*(i-imin      ))=A(i,Mmmpi-1,k)
            ibuf_sndN(k+nmax*(i-imin+ishft))=A(i,Mmmpi  ,k)
          enddo
        enddo
          
        call MPI_Irecv (ibuf_revN, isize, MPI_DOUBLE_PRECISION,
     &                     p_N, 3, MPI_COMM_WORLD, req(4), ierr)
        call MPI_Send  (ibuf_sndN, isize, MPI_DOUBLE_PRECISION,
     &                     p_N, 4, MPI_COMM_WORLD,         ierr)
      endif
      endif
       
!
! ...corners:
!
      if (mdii.eq.0) then
      if (SOUTH_INTER .and. WEST_INTER) then
        do k=1,nmax
          buf_snd1(k       )=A(1,1,k)
          buf_snd1(k+nmax  )=A(2,1,k)
          buf_snd1(k+2*nmax)=A(1,2,k)
          buf_snd1(k+3*nmax)=A(2,2,k)
        enddo
        call MPI_Irecv (buf_rev1, ksize, MPI_DOUBLE_PRECISION, p_SW,
     &                                6, MPI_COMM_WORLD, req(5),ierr)
        call MPI_Send  (buf_snd1, ksize, MPI_DOUBLE_PRECISION, p_SW,
     &                                5, MPI_COMM_WORLD,        ierr)
      endif
      else
      if (NORTH_INTER .and. EAST_INTER) then
      
        do k=1,nmax
          buf_snd2(k       )=A(Lmmpi-1,Mmmpi-1,k)
          buf_snd2(k+nmax  )=A(Lmmpi  ,Mmmpi-1,k)
          buf_snd2(k+2*nmax)=A(Lmmpi-1,Mmmpi  ,k)
          buf_snd2(k+3*nmax)=A(Lmmpi  ,Mmmpi  ,k)
        enddo
        call MPI_Irecv (buf_rev2, ksize, MPI_DOUBLE_PRECISION, p_NE,
     &                                5, MPI_COMM_WORLD, req(6),ierr)
        call MPI_Send  (buf_snd2, ksize, MPI_DOUBLE_PRECISION, p_NE,
     &                                6, MPI_COMM_WORLD,        ierr)
      endif
      endif

      if (mdii.eq.1) then
      if (SOUTH_INTER .and. EAST_INTER) then
      
        do k=1,nmax
          buf_snd3(k       )=A(Lmmpi-1,1,k)
          buf_snd3(k+nmax  )=A(Lmmpi  ,1,k)
          buf_snd3(k+2*nmax)=A(Lmmpi-1,2,k)
          buf_snd3(k+3*nmax)=A(Lmmpi  ,2,k)
        enddo
        call MPI_Irecv (buf_rev3, ksize, MPI_DOUBLE_PRECISION, p_SE,
     &                                8, MPI_COMM_WORLD, req(7),ierr)
        call MPI_Send  (buf_snd3, ksize, MPI_DOUBLE_PRECISION, p_SE,
     &                                7, MPI_COMM_WORLD,        ierr)
      endif
      else
      if (NORTH_INTER .and. WEST_INTER) then
      
        do k=1,nmax
          buf_snd4(k       )=A(1,Mmmpi-1,k)
          buf_snd4(k+nmax  )=A(2,Mmmpi-1,k)
          buf_snd4(k+2*nmax)=A(1,Mmmpi  ,k)
          buf_snd4(k+3*nmax)=A(2,Mmmpi  ,k)
        enddo
        call MPI_Irecv (buf_rev4, ksize, MPI_DOUBLE_PRECISION, p_NW,
     &                                7, MPI_COMM_WORLD, req(8),ierr)
        call MPI_Send  (buf_snd4, ksize, MPI_DOUBLE_PRECISION, p_NW,
     &                                8, MPI_COMM_WORLD,        ierr)
      endif
      endif
      enddo   !<-- iter

!
! Wait for completion of receive and fill ghost points: sides...
!
       
      if (WEST_INTER) then
      
        call MPI_Wait (req(1),status(1,1),ierr)
        
        do k=1,nmax
          do j=jmin,jmax
            A(-1,j,k)=jbuf_revW(k+nmax*(j-jmin      ))
            A( 0,j,k)=jbuf_revW(k+nmax*(j-jmin+jshft))
          enddo
        enddo
      endif

      if (EAST_INTER) then
      
        call MPI_Wait (req(2),status(1,2),ierr)
        
        do k=1,nmax
          do j=jmin,jmax
            A(Lmmpi+1,j,k)=jbuf_revE(k+nmax*(j-jmin      ))
            A(Lmmpi+2,j,k)=jbuf_revE(k+nmax*(j-jmin+jshft))
          enddo
        enddo
      endif

      if (SOUTH_INTER) then

        call MPI_Wait (req(3),status(1,3),ierr)
        
        do k=1,nmax
          do i=imin,imax
            A(i,-1,k)=ibuf_revS(k+nmax*(i-imin      ))
            A(i, 0,k)=ibuf_revS(k+nmax*(i-imin+ishft))
          enddo
        enddo
      endif

      if (NORTH_INTER) then

        call MPI_Wait (req(4),status(1,4),ierr)
        
        do k=1,nmax
          do i=imin,imax
            A(i,Mmmpi+1,k)=ibuf_revN(k+nmax*(i-imin      ))
            A(i,Mmmpi+2,k)=ibuf_revN(k+nmax*(i-imin+ishft))
          enddo
        enddo
      endif
!
! ...corners:
!
      if (SOUTH_INTER .and. WEST_INTER) then
        call MPI_Wait (req(5),status(1,5),ierr)
        
        do k=1,nmax
          A(-1,-1,k)=buf_rev1(k   )
          A( 0,-1,k)=buf_rev1(k+nmax )
          A(-1, 0,k)=buf_rev1(k+2*nmax)
          A( 0, 0,k)=buf_rev1(k+3*nmax)
        enddo
      endif

      if (NORTH_INTER.and.EAST_INTER) then
        call MPI_Wait (req(6),status(1,6),ierr)
        
        do k=1,nmax
          A(Lmmpi+1,Mmmpi+1,k)=buf_rev2(k   )
          A(Lmmpi+2,Mmmpi+1,k)=buf_rev2(k+nmax )
          A(Lmmpi+1,Mmmpi+2,k)=buf_rev2(k+2*nmax)
          A(Lmmpi+2,Mmmpi+2,k)=buf_rev2(k+3*nmax)
        enddo
      endif

      if (SOUTH_INTER .and. EAST_INTER) then
        call MPI_Wait (req(7),status(1,7),ierr)
        
        do k=1,nmax
          A(Lmmpi+1,-1,k)=buf_rev3(k   )
          A(Lmmpi+2,-1,k)=buf_rev3(k+nmax )
          A(Lmmpi+1, 0,k)=buf_rev3(k+2*nmax)
          A(Lmmpi+2, 0,k)=buf_rev3(k+3*nmax)
        enddo
      endif

      if (NORTH_INTER .and. WEST_INTER) then
        call MPI_Wait (req(8),status(1,8),ierr)
        
        do k=1,nmax
          A(-1,Mmmpi+1,k)=buf_rev4(k   )
          A( 0,Mmmpi+1,k)=buf_rev4(k+nmax )
          A(-1,Mmmpi+2,k)=buf_rev4(k+2*nmax)
          A( 0,Mmmpi+2,k)=buf_rev4(k+3*nmax)
        enddo
      endif

#if defined AGRIF
     
        DeAllocate(buf_snd4,  ibuf_sndN,  buf_snd2,
     &     buf_rev4,  ibuf_revN,  buf_rev2,
     &    jbuf_sndW,                      jbuf_sndE,
     &    jbuf_revW,                      jbuf_revE,
     &     buf_snd1,  ibuf_sndS,  buf_snd3,
     &     buf_rev1,  ibuf_revS,  buf_rev3)
#endif

      return
      end
#else
      subroutine MessPass3D_empty
      return
      end
#endif

