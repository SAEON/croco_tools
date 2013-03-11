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
#ifdef AVERAGES
                                     ! Write time-averaged 
      subroutine wrt_avg             ! fields into averages
                                     ! netCDF file.
!     implicit none
      integer ierr, record, lstr, lvar, lenstr
     &  , start(2), count(2), ibuff(4), nf_fwrite, type
# ifdef SOLVE3D
     &            , itrc         
#  ifdef SEDIMENT
     &            , indxWrk
#  endif
# endif
# if defined MPI & !defined PARALLEL_FILES
      include 'mpif.h'
      integer status(MPI_STATUS_SIZE), blank
# endif
# include "param.h"
# include "scalars.h"
# include "averages.h"
# include "ncscrum.h"
# include "grid.h"
# include "ocean2d.h"
# include "ocean3d.h"
# include "work.h"
# include "netcdf.inc"
# ifdef SEDIMENT
#  include "sediment.h"
# endif
#ifdef MASKING
# define SWITCH *
#else
# define SWITCH !
#endif
#if defined MPI & !defined PARALLEL_FILES
      if (mynode.gt.0) then
        call MPI_Recv (blank, 1, MPI_INTEGER, mynode-1,
     &                 1, MPI_COMM_WORLD, status, ierr)
      endif
#endif
!
! Create/open averages file; write grid arrays, if so needed.
!
      call def_avg (ncidavg, nrecavg, ierr)
      if (ierr .ne. nf_noerr) goto 99
      lstr=lenstr(avgname)
!                                            !!! WARNING: Here it is
! Set record within the file.                !!! assumed that global
!                                            !!! restart record index
      nrecavg=max(nrecavg,1)                 !!! nrecrst is already
      if (nrpfavg.eq.0) then                 !!! advanced by main.
        record=nrecavg
      else
        record=1+mod(nrecavg-1, nrpfavg)
      endif
!
! Write out time-averaged variables:
! ----- --- ------------- ----------
!
!     Time step and record indices.
!
      type=filetype_avg

      ibuff(1)=iic
      ibuff(2)=nrecrst
      ibuff(3)=nrechis
      ibuff(4)=nrecavg

      start(1)=1
      start(2)=record
      count(1)=4
      count(2)=1
      ierr=nf_put_vara_int (ncidavg, avgTstep, start, count, ibuff)
      if (ierr .ne. nf_noerr) then
         write(stdout,1) 'time_step', record,ierr MYID
         goto 99                !--> ERROR
      endif
!     
!     Averaged time
!     
      ierr=nf_put_var1_FTYPE (ncidavg, avgTime, record, time_avg)
      if (ierr .ne. nf_noerr) then
         lvar=lenstr(vname(1,indxTime))
         write(stdout,1) vname(1,indxTime)(1:lvar), record, ierr
     &        MYID
         goto 99                !--> ERROR
      endif
!     
!     Averaged time2
!     
      ierr=nf_put_var1_FTYPE (ncidavg, avgTime2, record, time_avg)
      if (ierr .ne. nf_noerr) then
         lvar=lenstr(vname(1,indxTime2))
         write(stdout,1) vname(1,indxTime2)(1:lvar), record, ierr
     &        MYID
         goto 99                !--> ERROR
      endif

!     
!     Barotropic mode variables: free-surface and 2D momentum
!     components in XI-,ETA-directions.
!     
      if (wrtavg(indxZ)) then
         work2d=zeta(:,:,fast_indx_out)
         call fillvalue2d(work2d,ncidavg,avgZ,indxZ,
     &                   record,r2dvar,type)
      endif

      if (wrtavg(indxUb)) then
         work2d(:,:)=ubar(:,:,fast_indx_out)
         call fillvalue2d(work2d,ncidavg,avgUb,indxUb,
     &                      record,u2dvar,type)
      endif

      if (wrtavg(indxVb)) then
         work2d(:,:)=vbar(:,:,fast_indx_out)
         call fillvalue2d(work2d,ncidavg,avgVb,indxVb,
     &                   record,v2dvar,type)
      endif
!     
!     Write out kinematic bottom stress (N/m2).
!     
      if (wrtavg(indxBostr)) then
         work2d=bostr_avg
         call fillvalue2d(work2d,ncidavg,avgBostr,indxBostr,
     &        record,r2dvar,type)
      endif
!     
!--   Atmospheric forcing : no mask, no fill value
!     
!     
!     Write out kinematic surface stress (N/m2).
!     
      if (wrtavg(indxWstr)) then
         ierr=nf_fwrite(wstr_avg, ncidavg, avgWstr, record, r2dvar)
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxWstr))
            write(stdout,1) vname(1,indxWstr)(1:lvar), record, ierr
     &           MYID
            goto 99             !--> ERROR
         endif
CR      write(*,*) 'wrt_avg:  Wstr' MYID
      endif
!     
!     Write out kinematic U surface stress component (N/m2).
!     
      if (wrtavg(indxUWstr)) then
         ierr=nf_fwrite(sustr_avg, ncidavg, avgUWstr, record, u2dvar)
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxUWstr))
            write(stdout,1) vname(1,indxUWstr)(1:lvar), record, ierr
     &           MYID
            goto 99             !--> ERROR
         endif
      endif
!     
!     Write out kinematic V surface stress component (N/m2).
!     
      if (wrtavg(indxVWstr)) then
         ierr=nf_fwrite(svstr_avg, ncidavg, avgVWstr, record, v2dvar)
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxVWstr))
            write(stdout,1) vname(1,indxVWstr)(1:lvar), record, ierr
     &           MYID
            goto 99             !--> ERROR
         endif
      endif
!     
!     Write out total heat flux [W/m2]
!     
      if (wrtavg(indxShflx)) then
         work2d=stflx_avg(:,:,itemp)
     &          SWITCH rmask
!
!        Mult by mask to avoid erroneous data 
!        over land if Qcorrection cppkeys applied
!
         ierr=nf_fwrite(work2d, ncidavg, avgShflx, record, r2dvar)
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxShflx))
            write(stdout,1) vname(1,indxShflx)(1:lvar), record, ierr
     &           MYID
            goto 99             !--> ERROR
         endif
CR      write(*,*) 'wrt_avg:  stflx(:,:,itemp)' MYID
      endif
#ifdef SALINITY      
!     Write out surface E-P flux [cm/d]
!     
      if (wrtavg(indxSwflx)) then
         work2d=stflx_avg(:,:,isalt)
     &         SWITCH rmask  
!
!        Mult by mask to avoid erroneous data over land 
!        if SFlux cppkeys correction applied and also 
!        to remove enormous wrong value diciding by eps.    
!
         ierr=nf_fwrite(work2d, ncidavg, avgSwflx, record, r2dvar)
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxSwflx))
            write(stdout,1) vname(1,indxSwflx)(1:lvar), record, ierr
     &           MYID
            goto 99             !--> ERROR
         endif
CR      write(*,*) 'wrt_avg:  stflx(:,:,isalt)' MYID
      endif
#endif
!     Write out solar radiation flux [W/m2]
!     
      if (wrtavg(indxShflx_rsw)) then
# ifdef BULK_FLUX
         ierr=nf_fwrite(shflx_rsw_avg, ncidavg, avgShflx_rsw, 
     &                  record, r2dvar)
# else
         ierr=nf_fwrite(srflx_avg, ncidavg, avgShflx_rsw, record, r2dvar)
# endif
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxShflx_rsw))
            write(stdout,1) vname(1,indxShflx_rsw)(1:lvar), record, ierr
     &           MYID
            goto 99             !--> ERROR
         endif
CR      write(*,*) 'wrt_avg:  srflx' MYID
      endif
#  ifdef BULK_FLUX
      if (wrtavg(indxShflx_rlw)) then
         ierr=nf_fwrite(shflx_rlw_avg, ncidavg, avgShflx_rlw, record,
     &        r2dvar)
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxShflx_rlw))
            write(stdout,1) vname(1,indxShflx_rlw)(1:lvar), record, ierr
     &           MYID
            goto 99             !--> ERROR
         endif
      endif
      
      if (wrtavg(indxShflx_lat)) then
         ierr=nf_fwrite(shflx_lat_avg, ncidavg, avgShflx_lat, record,
     &        r2dvar)
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxShflx_lat))
            write(stdout,1) vname(1,indxShflx_lat)(1:lvar), record, ierr
     &           MYID
            goto 99             !--> ERROR
         endif
      endif
      
      if (wrtavg(indxShflx_sen)) then
         ierr=nf_fwrite(shflx_sen_avg, ncidavg, avgShflx_sen, record,
     &        r2dvar)
         if (ierr .ne. nf_noerr) then
            lvar=lenstr(vname(1,indxShflx_sen))
            write(stdout,1) vname(1,indxShflx_sen)(1:lvar), record, ierr
     &           MYID
            goto 99             !--> ERROR
         endif
      endif
#  endif
!        
!--
!     
# ifdef SOLVE3D
!     
!     3D momentum components in XI- and ETA-directions.
!     
      if (wrtavg(indxU)) then
         workr=u_avg
         call fillvalue3d(workr,ncidavg,avgU,indxU,record,u3dvar,type)   
      endif
!     
      if (wrtavg(indxV)) then
         workr=v_avg
         call fillvalue3d(workr,ncidavg,avgV,indxV,record,v3dvar,type)   
      endif
!     
!     Tracer variables.
!     
      do itrc=1,NT
         if (wrtavg(indxT+itrc-1)) then
            workr=t_avg(:,:,:,itrc)
            call fillvalue3d(workr,ncidavg,avgT(itrc),indxT+itrc-1,
     &           record,r3dvar,type)           
         endif
      enddo
!     
!     Density anomaly.
!     
      if (wrtavg(indxR)) then
         workr=rho_avg
         call fillvalue3d(workr,ncidavg,avgR,indxR,record,r3dvar,type) 
      endif
!     
!     Write out S-coordinate omega vertical velocity (m/s).
!     
      if (wrtavg(indxO)) then
         work=omega_avg
         call fillvalue3d_w(work,ncidavg,avgO,indx0,record,w3dvar,type) 
      endif
!     
!     Write out true vertical velocity (m/s).
!     
      if (wrtavg(indxW)) then
         workr=w_avg
         call fillvalue3d(workr,ncidavg,avgW,indxW,record,r3dvar,type)
      endif   
!     
#  ifdef SST_SKIN
!     
!     Write out skin temperature (degC)
!     
      if (wrtavg(indxT)) then
         work2d=sst_skin_avg
         call fillvalue2d(work2d,ncidavg,avgSST_skin,indxSST_skin,
     &                    record,r2dvar,type)
      endif
#  endif 
!
!     
#  ifdef VIS_COEF_3D
!     
!     Write out Horizontal viscosity coefficient.
!     
      if (wrtavg(indxVisc)) then
         workr=visc3d_avg
         call fillvalue3d(workr,ncidavg,avgVisc,indxVisc,
     &        record,r3dvar,type)
      endif
#  endif
#  ifdef DIF_COEF_3D
!     
!     Write out Horizontal diffusivity coefficient.
!     
      if (wrtavg(indxDiff)) then
         workr=diff3d_avg
         call fillvalue3d(workr,ncidavg,avgDiff,indxDiff,
     &        record,r3dvar,type)
      endif
#  endif
#  ifdef AVERAGES_K
!     
!     Write out vertical viscosity coefficient.
!     
      if (wrtavg(indxAkv)) then
         work=Akv_avg
         call fillvalue3d_w(work,ncidavg,avgAkv,indxAkv,
     &        record,w3dvar,type)
CR      write(*,*) 'wrt_avg:  wrtAkt' MYID
      endif
!     
!     Write out vertical diffusion coefficient for potential temperature.
!     
      if (wrtavg(indxAkt)) then
         work=Akt_avg(:,:,:,itemp)
         call fillvalue3d_w(work,ncidavg,avgAkt,indxAkt,
     &        record,w3dvar,type)
CR      write(*,*) 'wrt_avg:  wrtAkt' MYID
      endif
#    ifdef SALINITY
!     
!     Write out vertical diffusion coefficient for salinity.
!     
      if (wrtavg(indxAks)) then
         work=Akt_avg(:,:,:,isalt)
         call fillvalue3d_w(work,ncidavg,avgAks,indxAks,
     &        record,w3dvar,type)
CR       write(*,*) 'wrt_avg:  wrtAks' MYID
      endif
#    endif /* SALINITY */
#  endif  /* AVERAGES_K */
!     
#  ifdef LMD_SKPP
!     
!     Write out depth of planetary boundary layer (m).
!     
      if (wrtavg(indxHbl)) then
         work2d=hbl_avg
         call fillvalue2d(work2d,ncidavg,avgHbl,indxHbl,
     &        record,r2dvar,type)
      endif
#  endif
#  ifdef LMD_BKPP
!     
!     Write out depth of planetary boundary layer (m).
!     
      if (wrtavg(indxHbbl)) then
         work2d=hbbl_avg
         call fillvalue2d(work2d,ncidavg,avgHbbl,indxHbbl,
     &        record,r2dvar,type)
      endif
#  endif
#  if defined BIOLOGY && !defined PISCES
!     
!     Write out depth of the euphotic layer (m).
!     
      if (wrtavg(indxHel)) then
         work2d=hel_avg
         call fillvalue2d(work2d,ncidavg,avgHbl,indxHbl,
     &        record,r2dvar,type)
      endif
!     
!     Write out Chlorophyll to Carbon ratio (m).
!     
#   ifdef BIO_NChlPZD
      if (wrtavg(indxChC)) then
         workr=theta_avg
         call fillvalue3d(workr,ncidavg,avgChC,indxChC,
     &        record,r3dvar,type)
      endif
#     ifdef OXYGEN
      if (wrtavg(indxU10)) then
         work2d=u10_avg
         call fillvalue2d(work2d,ncidavg,avgU10,indxU10,
     &        record,r2dvar,type)
      endif
      if (wrtavg(indxKvO2)) then
         work2d=Kv_02_avg
         call fillvalue2d(work2d,ncidavg,avgKv02,indxKv02,
     &        record,r2dvar,type)
      endif
      if (wrtavg(indxO2sat)) then
         work2d=O2satu_avg
         call fillvalue2d(work2d,ncidavg,avgO2sat,indx02sat,
     &        record,r2dvar,type)
      endif
#     endif /* OXYGEN */
!     
#   endif
#  endif /* BIOLOGY */
!
# ifdef SEDIMENT
!     
!     Write out sediment bed layer thickness, porosity, volume 
!     fraction of size class in sediment bed (2+2*NST b3dgrd variables)
!     
      do itrc=1,NST
         indxWrk=indxBFRA+itrc-1
!         if (wrthis(indxWrk)) then
            worksed_frac=bed_frac_avg(:,:,:,itrc)
            call fillvalue3d(worksed_frac,ncidavg,avgSed(itrc+2),
     &           indxWrk,record,b3dvar,type)
!         endif
      enddo
# endif /* SEDIMENT */
!
# endif /* SOLVE3D */
!
 1    format(/' WRT_AVG - ERROR while writing variable(',1x,a,1x,
     &     ')into averages file.',/,11x,'Time record:',
     &     i6,3x,'netCDF error code',i4,3x,a,i4)
      goto 100
 99   may_day_flag=3
 100  continue

!     
!     Synchronize netCDF file to disk to allow other processes
!     to access data immediately after it is written.
!     
# if defined MPI &              !defined PARALLEL_FILES
      ierr=nf_close(ncidavg)
      if (nrpfavg.gt.0 .and. record.ge.nrpfavg) ncidavg=-1
# else
      if (nrpfavg.gt.0 .and. record.ge.nrpfavg) then
         ierr=nf_close(ncidavg)
         ncidavg=-1
      else
         ierr=nf_sync(ncidavg)
      endif
# endif
      if (ierr .eq. nf_noerr) then
         write(stdout,'(6x,A,2(A,I4,1x),A,I3)') 'WRT_AVG -- wrote ',
     &        'averaged fields into time record =', record, '/',
     &        nrecavg  MYID
      else
         write(stdout,'(/1x,2A/)') 'WRT_AVG ERROR: Cannot ',
     &        'synchronize/close averages netCDF file.'
         may_day_flag=3
      endif

# if defined MPI &              !defined PARALLEL_FILES
      if (mynode .lt. NNODES-1) then
         call MPI_Send (blank, 1, MPI_INTEGER, mynode+1,
     &        1, MPI_COMM_WORLD,  ierr)
      endif
# endif
      return
      end
#else
      subroutine wrt_avg_empty
      end
#endif /* AVERAGES */
