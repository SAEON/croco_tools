! $Id$
!
!======================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
! 
! ROMS_AGRIF website : http://roms.mpl.ird.fr
!======================================================================
!
#include "cppdefs.h"
#ifdef BIOLOGY
      subroutine bio_diag (tile)
      implicit none
# include "param.h"
      integer tile
# include "compute_tile_bounds.h"
      call bio_diag_tile  (Istr,Iend,Jstr,Jend)
      return
      end

      subroutine bio_diag_tile (Istr,Iend,Jstr,Jend)
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
      integer Istr,Iend,Jstr,Jend, i,j,k,itrc, iocheck,
     &                             trd,omp_get_thread_num
!
!
      real cff1, NSUB
      real*QUAD cff, my_sum(0:2*NT+1)
#ifdef MPI
      include 'mpif.h'
      integer size, step, status(MPI_STATUS_SIZE), ierr
      real*QUAD buff(0:2*NT+1)
      common /xyz/ buff
#endif
# undef  ETALON_CHECK
# ifdef ETALON_CHECK
      integer ncheck, nparam
      parameter (ncheck=10, nparam=4)
      integer icheck, check_point(ncheck)
      character*57 check_line, etalon_line(ncheck)
      character*1 symbol(57)
      real    A0(nparam), A1(nparam)
      integer P0(nparam), P1(nparam)
# include "etalon_data.h"
# endif
      if (mod(iic-1,ninfo).eq.0) then
        do itrc=0,NT
          my_sum(2*itrc  )=QuadZero          ! <-- Reset local sums
          my_sum(2*itrc+1)=QuadZero
        enddo
        do j=Jstr,Jend
         do i=Istr,Iend
# ifdef MASKING
           cff1=rmask(i,j)/(pm(i,j)*pn(i,j))  ! <-- grid box area
# else
           cff1=1./(pm(i,j)*pn(i,j))
# endif
           do k=1,N
            cff=cff1*Hz(i,j,k)       ! <-- volume of grid box(i,j,k) 
            my_sum(1)=my_sum(1)+cff  ! <-- accumulate volume
             do itrc=1,NT
              my_sum(2*itrc  )=my_sum(2*itrc  )+cff*t(i,j,k,nstp,itrc)
              my_sum(2*itrc+1)=my_sum(2*itrc+1)+cff*t(i,j,k,nstp,itrc)
     &                                             *t(i,j,k,nstp,itrc) 
             enddo
           enddo
         enddo
        enddo
!
! Perform global summation: whoever gets first to the critical region
! resets global sums before global summation starts; after the global
! summation is completed, thread, which is the last one to enter the
! critical region, finalizes the computation of diagnostics and
! prints them out. 
!
        if (SINGLE_TILE_MODE) then
          NSUB=1
        else
          NSUB=NSUB_X*NSUB_E
        endif

C$OMP CRITICAL (bio_cr_rgn)
          if (bio_count.eq.0) then
            do itrc=1,2*NT+1
              global_sum(itrc)=QuadZero   ! <-- Reset global sums
            enddo
          endif 
                                       !-->  Perform global summation
          do itrc=1,2*NT+1
            global_sum(itrc)=global_sum(itrc)+my_sum(itrc)
          enddo

          bio_count=bio_count+1             ! This counter identifies
          if (bio_count.eq.NSUB) then      ! the last thread, whoever
            bio_count=0                    ! it is, not always master.
# ifdef MPI
            if (NNODES.gt.1) then         ! Perform global summation 
              size=NNODES                 ! among MPI processes
   10          step=(size+1)/2
                if (mynode.ge.step .and. mynode.lt.size) then
                  do itrc=1,2*NT+1
                    buff(itrc)=global_sum(itrc)  ! This is MPI_Reduce
                  enddo
                  call MPI_Send (buff,  6, MPI_DOUBLE_PRECISION,
     &                 mynode-step, 17, MPI_COMM_WORLD,      ierr)
                elseif (mynode .lt. size-step) then
                  call MPI_Recv (buff,  6, MPI_DOUBLE_PRECISION,
     &                 mynode+step, 17, MPI_COMM_WORLD, status, ierr)
                  do itrc=1,2*NT+1
                    global_sum(itrc)=global_sum(itrc)+buff(itrc)
                  enddo
                endif
               size=step
              if (size.gt.1) goto 10
            endif
            if (mynode.eq.0) then
# endif
            trd=omp_get_thread_num()

            cff=1./global_sum(1)
            do itrc=1,NT
              global_sum(2*itrc  )=cff*global_sum(2*itrc )
              global_sum(2*itrc+1)=cff*global_sum(2*itrc+1)
     &                                -global_sum(2*itrc)**2
            enddo
            if (first_time.eq.0) then
              first_time=1
# ifdef BIO_NChlPZD
              write(stdout,1) 'STEP','time[DAYS]','       NO3',
     &                   '     PHYTO','       ZOO','    DETRIT',
     &                   'trd'
!#  ifdef OXYGEN
!     &                  ,'        O2'
!#  endif
   1          format(1x,A4,3x,A10,1x,A10,4x,A10,4x,A10,4x,A10,3x,A3)
# elif defined BIO_N2ChlPZD2
              write(stdout,1) 'STEP','time[DAYS]','       NO3',
     &                   '     NH4 ','     PHYTO','       ZOO',
     &                   '   DET1+2','trd'
   1          format(1x,A4,3x,A10,1x,A10,4x,A10,4x,A10,4x,A10,4x,A10,3x,A3)
# endif

            endif

            do itrc=1,NT
              global_sum(2*itrc+1)=max(0.,global_sum(2*itrc+1))**0.5
            enddo

# ifdef BIO_NChlPZD
            write(stdout,2) iic-1,tdays, 
     &                      global_sum(2*iNO3_),  
     &                      global_sum(2*iPhy1), 
     &                      global_sum(2*iZoo1),  
     &                      global_sum(2*iDet1) 
!#  ifdef OXYGEN
!     &                    , global_sum(2*iO2)
!#  endif
     &                    , trd
   2        format(I6, F12.5, 4(1PE14.7), I3)
# elif defined BIO_N2ChlPZD2
            write(stdout,2) iic-1,tdays, 
     &                      global_sum(2*iNO3_),  
     &                      global_sum(2*iNH4_),  
     &                      global_sum(2*iPhy1), 
     &                      global_sum(2*iZoo1),  
     &                      global_sum(2*iDet1)+global_sum(2*iDet2), 
     &                      trd
   2        format(I6, F12.5, 5(1PE14.7), I3)
# endif

# ifdef  ETALON_CHECK
            do icheck=1,ncheck
              if (iic-1.eq.check_point(icheck)) then
                write(check_line,10,iostat=iocheck) cff,global_sum(7),
     &                                  global_sum( 9),global_sum(11)
  10            format(1PE15.9,3(1pe14.7))
                if (check_line .eq. etalon_line(icheck)) then
                  write(stdout,*) 'PASSED_ETALON_CHECK'
                else
                  read(check_line         ,11,iostat=iocheck) (A1(i),
     &                                             P1(i), i=1,nparam)
                  read(etalon_line(icheck),11,iostat=iocheck) (A0(i),
     &                                             P0(i), i=1,nparam)
  11              format(f11.9,1x,i3,3(f10.7,1x,i3))
                  do i=1,nparam
                    A1(i)=A1(i)-A0(i)*10.**float(P0(i)-P1(i))
                  enddo
                  write(check_line        ,12,iostat=iocheck) (A1(i)
     &                                                  ,i=1,nparam)
  12              format(f14.9,3(1x,f13.7))
                  j=0
                  do i=2,56
                    if (symbol(i).eq.'.'.and.symbol(i-1).eq.'0') then
                      symbol(i)=':'
                      j=1
                    else if (j.eq.1 .and. symbol(i).eq.'0') then
                      symbol(i)='.'
                    else
                      j=0
                    endif
                  enddo
                  write(stdout,15) ' difference:    ',check_line
  15              format(A16,A56)
                  write(stdout,*) 'ETALON_CHECK: SOMETHING_IS_WRONG'
                endif
              endif
            enddo
# endif
# ifdef MPI
            endif    ! <-- mynode.eq.0
# endif
          endif
C$OMP END CRITICAL (bio_cr_rgn)
      endif
      return
      end
#else
      subroutine bio_diag_empty
      return
      end
#endif /* BIOLOGY */

