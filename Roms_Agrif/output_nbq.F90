#include "cppdefs.h"
#ifdef NBQ

      subroutine output_nbq(ichoix)
!-------------------------------------------------------------------
!
!                   Several Outputs for MPI testings
!
!-------------------------------------------------------------------
      use module_nh 
      use module_nbq

      implicit none

# include "param_F90.h"
# include "scalars_F90.h"
# include "ocean3d.h"
# include "ocean2d.h"
# include "grid.h"
# include "nbq.h"

      integer :: i,j,k,ichoix
      integer::p1_nbq,p2_nbq,p3_nbq,p4_nbq,p5_nbq

       if (ichoix.eq.0) then
!
!-------------------------------------------------------------------
! Time steps & grid size
!-------------------------------------------------------------------
!
        MPI_master_only write(stdout,'(A/,5(E12.4,2x,A/),A/)')   &
          ' ======================= NBQ ==================== ',  &
         dt,        '  dt (int)  Internal timestep [s]',         &
         dtfast,    '  dt (ext)  External timestep [s]',         &
         dtnbq,     '  dt (nbq)  Acoustic timestep [s]',         &
         om_u(2,2), '  dx        XI  grid step     [m]',         &
         on_u(2,2), '  dy        ETA grid step     [m]',         &
          ' ======================= NBQ ==================== '

       endif
!
!-------------------------------------------------------------------
! Output qdm to debug MPI (along x)
!-------------------------------------------------------------------
!
        if (ichoix.eq.10) then
#ifdef MPI
         if (mynode.eq.0) then
#endif
          open(unit=10,file='mpi_nbq0.dat',access='append')
          i=25
          j=1
          k=N
          p1_nbq=ijk2lq_nh(i,j,k)
          p4_nbq=ijk2lq_nh(i+1,j,k)
          p5_nbq=ijk2lq_nh(i+2,j,k)
          p2_nbq=ijk2lmom_nh(i,j,k,1)
          p3_nbq=ijk2lmom_nh(i+1,j,k,1)
          write(10,*) iic,iif
          write(10,*) iteration_nbq,'test0',qdm_nbq_a(p2_nbq,0),qdm_nbq_a(p3_nbq,0)
          write(10,*) iteration_nbq,'testd',qdm_nbq_a(p2_nbq,1),qdm_nbq_a(p3_nbq,1)
          write(10,*) iteration_nbq,'teste',qdm_nbq_a(p2_nbq,2),qdm_nbq_a(p3_nbq,2)
          close(10)
#ifdef MPI
         endif
#endif

#ifdef MPI
         if (mynode.eq.1) then
#endif
          open(unit=10,file='mpi_nbq1.dat',access='append')
          i=0
          j=1
          k=N
          p1_nbq=ijk2lq_nh(i,j,k)
          p4_nbq=ijk2lq_nh(i+1,j,k)
          p2_nbq=ijk2lmom_nh(i,j,k,1)
          p3_nbq=ijk2lmom_nh(i+1,j,k,1)
          p5_nbq=ijk2lmom_nh(i+2,j,k,1)
          write(10,*) iic,iif
          write(10,*) iteration_nbq,'test0',qdm_nbq_a(p2_nbq,0),qdm_nbq_a(p3_nbq,0)
          write(10,*) iteration_nbq,'testd',qdm_nbq_a(p2_nbq,1),qdm_nbq_a(p3_nbq,1)
          write(10,*) iteration_nbq,'teste',qdm_nbq_a(p2_nbq,2),qdm_nbq_a(p3_nbq,2)
          close(10)
#ifdef MPI
         endif
#endif
        endif

!-------------------------------------------------------------------
! Output rho_nbq and div_nbq to debug MPI (along x)
!-------------------------------------------------------------------
!
        if (ichoix.eq.11) then
#ifdef MPI
         if (mynode.eq.0) then
#endif
          open(unit=10,file='mpi_nbq0.dat',access='append')
          i=25
          j=1
          k=N
          p1_nbq=ijk2lq_nh(i,j,k)
          p4_nbq=ijk2lq_nh(i+1,j,k)
          p5_nbq=ijk2lq_nh(i+2,j,k)
          p2_nbq=ijk2lmom_nh(i,j,k,1)
          p3_nbq=ijk2lmom_nh(i+1,j,k,1)
          write(10,*) iteration_nbq,'testz',rhp_nbq_a(p1_nbq,0),rhp_nbq_a(p4_nbq,0)
          write(10,*) iteration_nbq,'testa',rhp_nbq_a(p1_nbq,1),rhp_nbq_a(p4_nbq,1)
          write(10,*) iteration_nbq,'testb',rhp_nbq_a(p1_nbq,2),rhp_nbq_a(p4_nbq,2)
          write(10,*) iteration_nbq,'testc',div_nbq_a(p1_nbq,1),div_nbq_a(p4_nbq,1)
          close(10)
#ifdef MPI
         endif
#endif
#ifdef MPI
         if (mynode.eq.1) then
#endif
          open(unit=10,file='mpi_nbq1.dat',access='append')
          i=0
          j=1
          k=N
          p1_nbq=ijk2lq_nh(i,j,k)
          p4_nbq=ijk2lq_nh(i+1,j,k)
          p2_nbq=ijk2lmom_nh(i,j,k,1)
          p3_nbq=ijk2lmom_nh(i+1,j,k,1)
          p5_nbq=ijk2lmom_nh(i+2,j,k,1)
          write(10,*) iteration_nbq,'testz',rhp_nbq_a(p1_nbq,0),rhp_nbq_a(p4_nbq,0)
          write(10,*) iteration_nbq,'testa',rhp_nbq_a(p1_nbq,1),rhp_nbq_a(p4_nbq,1)
          write(10,*) iteration_nbq,'testb',rhp_nbq_a(p1_nbq,2),rhp_nbq_a(p4_nbq,2)
          write(10,*) iteration_nbq,'testc',div_nbq_a(p1_nbq,1),div_nbq_a(p4_nbq,1)
          close(10)
#ifdef MPI
         endif
#endif
        endif

!
!-------------------------------------------------------------------
! Output qdm to debug MPI (along y)
!-------------------------------------------------------------------
!
        if (ichoix.eq.20) then
#ifdef MPI
         if (mynode.eq.0) then
#endif
          open(unit=10,file='mpi_nbq0.dat',access='append')
          i=1
          j=25
          k=N
          p1_nbq=ijk2lq_nh(i,j,k)
          p4_nbq=ijk2lq_nh(i,j+1,k)
          p5_nbq=ijk2lq_nh(i,j+2,k)
          p2_nbq=ijk2lmom_nh(i,j,k,1)
          p3_nbq=ijk2lmom_nh(i,j+1,k,1)
          write(10,*) iic,iif
          write(10,*) iteration_nbq,'test0',qdm_nbq_a(p2_nbq,0),qdm_nbq_a(p3_nbq,0)
          write(10,*) iteration_nbq,'testd',qdm_nbq_a(p2_nbq,1),qdm_nbq_a(p3_nbq,1)
          write(10,*) iteration_nbq,'teste',qdm_nbq_a(p2_nbq,2),qdm_nbq_a(p3_nbq,2)
          close(10)
#ifdef MPI
         endif
#endif

#ifdef MPI
         if (mynode.eq.1) then
#endif
          open(unit=10,file='mpi_nbq1.dat',access='append')
          i=1
          j=0
          k=N
          p1_nbq=ijk2lq_nh(i,j,k)
          p4_nbq=ijk2lq_nh(i,j+1,k)
          p2_nbq=ijk2lmom_nh(i,j,k,1)
          p3_nbq=ijk2lmom_nh(i,j+1,k,1)
          p5_nbq=ijk2lmom_nh(i,j+2,k,1)
          write(10,*) iic,iif
          write(10,*) iteration_nbq,'test0',qdm_nbq_a(p2_nbq,0),qdm_nbq_a(p3_nbq,0)
          write(10,*) iteration_nbq,'testd',qdm_nbq_a(p2_nbq,1),qdm_nbq_a(p3_nbq,1)
          write(10,*) iteration_nbq,'teste',qdm_nbq_a(p2_nbq,2),qdm_nbq_a(p3_nbq,2)
          close(10)
#ifdef MPI
         endif
#endif
        endif


!-------------------------------------------------------------------
! Output rho_nbq and div_nbq to debug MPI (along y)
!-------------------------------------------------------------------
!
        if (ichoix.eq.21) then
#ifdef MPI
         if (mynode.eq.0) then
#endif
          open(unit=10,file='mpi_nbq0.dat',access='append')
          i=1
          j=25
          k=N
          p1_nbq=ijk2lq_nh(i,j,k)
          p4_nbq=ijk2lq_nh(i,j+1,k)
          p5_nbq=ijk2lq_nh(i,j+2,k)
          p2_nbq=ijk2lmom_nh(i,j,k,1)
          p3_nbq=ijk2lmom_nh(i,j+1,k,1)
          write(10,*) iteration_nbq,'testz',rhp_nbq_a(p1_nbq,0),rhp_nbq_a(p4_nbq,0)
          write(10,*) iteration_nbq,'testa',rhp_nbq_a(p1_nbq,1),rhp_nbq_a(p4_nbq,1)
          write(10,*) iteration_nbq,'testb',rhp_nbq_a(p1_nbq,2),rhp_nbq_a(p4_nbq,2)
          write(10,*) iteration_nbq,'testc',div_nbq_a(p1_nbq,1),div_nbq_a(p4_nbq,1)
          close(10)
#ifdef MPI
         endif
#endif
#ifdef MPI
         if (mynode.eq.1) then
#endif
          open(unit=10,file='mpi_nbq1.dat',access='append')
          i=1
          j=0
          k=N
          p1_nbq=ijk2lq_nh(i,j,k)
          p4_nbq=ijk2lq_nh(i,j+1,k)
          p2_nbq=ijk2lmom_nh(i,j,k,1)
          p3_nbq=ijk2lmom_nh(i,j+1,k,1)
          p5_nbq=ijk2lmom_nh(i,j+2,k,1)
          write(10,*) iteration_nbq,'testz',rhp_nbq_a(p1_nbq,0),rhp_nbq_a(p4_nbq,0)
          write(10,*) iteration_nbq,'testa',rhp_nbq_a(p1_nbq,1),rhp_nbq_a(p4_nbq,1)
          write(10,*) iteration_nbq,'testb',rhp_nbq_a(p1_nbq,2),rhp_nbq_a(p4_nbq,2)
          write(10,*) iteration_nbq,'testc',div_nbq_a(p1_nbq,1),div_nbq_a(p4_nbq,1)
          close(10)
#ifdef MPI
         endif
#endif
        endif


!-------------------------------------------------------------------
! Outputs QDM
!-------------------------------------------------------------------
!
        if (ichoix.eq.30) then

#ifdef MPI
         if (mynode == 0) then
#endif
          if (mod(iteration_nbq,1) == 0) then
           i=4
           j=1
           k=int(N/2)
           l_nbq =ijk2lmom_nh(i,j,k,1)
           l1_nbq=ijk2lmom_nh(i,j,k,3)
           open(unit=10,file='qdm.dat' ,access='append')
             write(10,*) qdm_nbq_a(l_nbq,2)
             write(10,*) qdm_nbq_a(l1_nbq,2)
             write(10,*) u(i,j,k,nnew)
             write(10,*) wz(i,j,k,nnew)
           close(10)
          endif
#ifdef MPI
         endif
#endif
        endif


!-------------------------------------------------------------------
! Outputs for TANK: SSA
!-------------------------------------------------------------------
!
        if (ichoix.eq.40) then

#ifdef MPI
         if (mynode == 0) then
#endif
          if (mod(iif,10).eq.0.and.iif.le.ndtfast) then
           open(unit=10,file='zta.dat',access='append')
            i=1
            j=1
            write(10,*) (zeta(i,j,knew)/rhobar_nbq(i,j))-h(i,j)
            close(10)
          endif

#ifdef MPI
         endif
#endif
        endif

      end subroutine output_nbq

#else
      subroutine output_nbq_empty
      end subroutine output_nbq_empty
#endif

