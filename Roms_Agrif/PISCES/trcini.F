! $Id$
!
!=========================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France.
! The two other branches, from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al), are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
!
! ROMS_AGRIF website : http://roms.mpl.ird.fr
!=========================================================================
!
#include "cppdefs.h"
#ifdef PISCES

      SUBROUTINE trcini(Istr,Iend,Jstr,Jend)

      Implicit NONE
      INTEGER Istr,Jstr,Iend,Jend

CCC---------------------------------------------------------------------
CCC
CCC                       ROUTINE trcini
CCC                     ******************
CCC
CCC  PURPOSE :
CCC  ---------
CCC     initialization for passive tracer
CCC     for restart or not
CCC
CC   METHOD :
CC   -------
CC
CC   INPUT :
CC   -----
CC      argument		: no
CC      COMMON
CC
CC   OUTPUT :
CC   ------
CC      argument                : no
CC      COMMON
CC
CC   WORKSPACE :
CC   ---------
CC      i j k
CC
CC
CC   EXTERNAL :
CC   --------
CC
CC   MODIFICATIONS:
CC   --------------
CC                 04/00 (O. Aumont, M.A. Foujols) HAMOCC3 and P3ZD
CC                 04/06 (C. Menkes) HAMOCC3 and P3ZD
CC----------------------------------------------------------------------
CC parameters and commons
CC ======================
# include "param.h"
# include "parameter.h"
# include "common.h"
CC----------------------------------------------------------------------
CC local declarations
CC ==================
# if defined key_passivetrc

      INTEGER i,j,k,itrc
CCC---------------------------------------------------------------------
CCC  OPA8, LODYC (15/11/96)
CCC---------------------------------------------------------------------
#    if defined key_trc_pisces
#    include "trcini.pisces.h"
#    else
C
C Specific initialization to do
C
#    endif
C
C 1. initialization of passives tracers field
C -------------------------------------------
      DO itrc=1,jptra
        DO k=1,jpk
          DO j=Jstr,Jend
            DO i=Istr,Iend
              trn(i,j,k,itrc)=0.e0
            END DO
          END DO
        END DO
      END DO
      WRITE(numout,*) ' '
      WRITE(numout,*) ' trcini: generic initialisation done '
      WRITE(numout,*) ' '
# else
C
C no passive tracers
C
# endif
C
      RETURN
      END

!==================================================================
!
!==================================================================

      subroutine get_dust 
 
      Implicit none 
# include "param.h"
# include "parameter.h"
# include "common.h"
# include "netcdf.inc"
# include "ncscrum.h"
# include "scalars.h"
      integer i, j, ncid, varid, dimid, ierr,
     &        lstr, lenstr, nf_fread, nrec_dust, irec
      REAL dustmp(GLOBAL_2D_ARRAY,12)

#ifdef MPI
#define LOCALLM Lmmpi
#define LOCALMM Mmmpi
#else
#define LOCALLM Lm
#define LOCALMM Mm
#endif

!
!    READ DUST INPUT FROM ATMOSPHERE
!    -------------------------------------
!
      IF (bdustfer) THEN
        lstr=lenstr(bioname)
        ierr=nf_open (bioname(1:lstr), nf_nowrite, ncid)
        if (ierr .ne. nf_noerr) then
           write(stdout,4) bioname
        endif
        ierr=nf_inq_varid (ncid,"dust",varid)
        if (ierr .ne. nf_noerr) then
          write(stdout,5) "dust", bioname
        endif
        ierr=nf_inq_dimid(ncid,"dust_time",dimid)
        ierr=nf_inq_dimlen(ncid,dimid,nrec_dust)
!        write(*,*)'NREC_DUST=',nrec_dust
!        write(*,*)'-----------------------------'
        do irec=1,nrec_dust
          ierr=nf_fread(dustmp(START_2D_ARRAY,irec), ncid, varid,
     &                                              irec, r2dvar)
          if (ierr .ne. nf_noerr) then
            write(stdout,6) "dust", irec 
          endif
        enddo
        ierr=nf_close(ncid)
        write(stdout,'(6x,A,1x,I4)') 
     &                   'TRCINI_PISCES -- Read dust deposition '
#ifdef MPI
     &                                                   , mynode
#endif
  4     format(/,' TRCINI_PISCES - unable to open forcing netCDF ',1x,A)
  5     format(/,' TRCINI_PISCES - unable to find forcing variable: ',A,
     &                               /,14x,'in forcing netCDF  ',A)
  6     format(/,' TRCINI_PISCES - error while reading variable: ',A,2x,
     &                                           ' at TIME index = ',i4)


        do irec=1,nrec_dust
          do j=1,LOCALMM
            do i=1,LOCALLM
              dustmo(i,j,irec)=dustmp(i,j,irec)
            enddo
          enddo
        enddo

      ELSE
!        write(*,*)'----------------------------'
!        write(*,*)'BDUSTFER=',bdustfer
!        write(*,*)'-----------------------------'
        do irec=1,12
          do j=1,LOCALMM
            do i=1,LOCALLM
              dustmo(i,j,irec)=0.
            enddo
          enddo
        enddo

      ENDIF

      return
      end

#else /* PISCES */
!
!====================================================================
!                   nothing...
!====================================================================
!
      SUBROUTINE trcini_empty
      return
      end
#endif /* PISCES */

