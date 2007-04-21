!
! $Id$
!
#include "cppdefs.h"
#if defined SOLVE3D && defined T_FRC_BRY && defined BIOLOGY 

      subroutine get_bry_bio            ! Read side boundary forcing 
      implicit none                     ! fields from boundary file
# include "param.h"
# include "scalars.h"
# include "ncscrum.h"
# include "netcdf.inc"
# include "boundary.h"
      real cff
      integer  lstr,lvar,lenstr, ierr, ierr_all, itrc
      integer  nf_read_bry_EW, nf_read_bry_NS 
!
! Initialization: Check, whether boundary forcing file is already
!================ opened, and if not, open it.  Find and save netCDF
! IDs for relevant variables.  Determine whether there is cycling to
! reuse the input data and find cycling period "bry_cycle", set
! initial cycling index "bry_ncycle" and record index "bry_rec",
! time index "itbry" and both time record bounds to large negative
! artificial values, so that it will trigger the logic in reading
! part below.
!
      ierr=nf_noerr
      lstr=lenstr(bry_file)
      if (iic.eq.0 ) then
         do itrc=1,NT
            got_tbry(itrc)=.FALSE.
         enddo
         if (bry_id.eq.-1) then
            ierr=nf_open (bry_file(1:lstr), nf_nowrite, bry_id)
            if (ierr.ne.nf_noerr) write(stdout,'(/1x,4A/)') 'ERROR ',
     &           'in get_all_bry: can not open netCDF file ''',
     &           bry_file(1:lstr),   '''.'
         endif
         if (ierr.eq.nf_noerr) then
            ierr_all=0 
            do itrc=1,NT
               if (itrc.eq.itemp) then
                  got_tbry(itemp)=.true.
# ifdef SALINITY
                  got_tbry(isalt)=.true.
# endif
                  ierr=nf_inq_varid (bry_id, 'bry_time',  bry_time_id)
                  if (ierr.ne.nf_noerr) then
                     got_tbry(itemp)=.false.
                     ierr_all=ierr_all+ierr
# ifdef SALINITY
                     got_tbry(isalt)=.false.
# endif
                     write(stdout,3) 'bry_time', bry_file(1:lstr)
C     goto 99      !--> ERROR
                  endif
                  
# ifdef PISCES
               elseif (itrc.eq.iDIC_) then
                  got_tbry(itrc)=.true.
                  ierr=nf_inq_varid (bry_id, 'dic_time', bry_tid(itrc))
                  ierr_all=ierr_all+ierr
                  if (ierr .ne. nf_noerr) then
                     got_tbry(itrc)=.false.
                     write(stdout,3) 'dic_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
                  endif
               elseif (itrc.eq.iTAL_) then
                  got_tbry(itrc)=.true.
                  ierr=nf_inq_varid (bry_id, 'talk_time', bry_tid(itrc))
                  ierr_all=ierr_all+ierr
                  if (ierr .ne. nf_noerr) then
                     got_tbry(itrc)=.false.
                     write(stdout,3) 'talk_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
                  endif
               elseif (itrc.eq.iOXY_) then
                  got_tbry(itrc)=.true.
                  ierr=nf_inq_varid (bry_id, 'o2_time', bry_tid(itrc))
                  ierr_all=ierr_all+ierr
                  if (ierr .ne. nf_noerr) then
                     got_tbry(itrc)=.false.
                     write(stdout,3) 'o2_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
                  endif
               elseif (itrc.eq.iCAL_) then
                  got_tbry(itrc)=.true.
                  ierr=nf_inq_varid (bry_id, 'cal_time', bry_tid(itrc))
                  ierr_all=ierr_all+ierr
                  if (ierr .ne. nf_noerr) then
                     got_tbry(itrc)=.false.
                     write(stdout,3) 'cal_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
                  endif
               elseif (itrc.eq.iPO4_) then
                  got_tbry(itrc)=.true.
                  ierr=nf_inq_varid (bry_id, 'po4_time', bry_tid(itrc))
                  ierr_all=ierr_all+ierr
                  if (ierr .ne. nf_noerr) then
                     got_tbry(itrc)=.false.
                     write(stdout,3) 'po4_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
                  endif
               elseif (itrc.eq.iPOC_) then
                  got_tbry(itrc)=.true.
                  ierr=nf_inq_varid (bry_id, 'poc_time', bry_tid(itrc))
                  ierr_all=ierr_all+ierr
                  if (ierr .ne. nf_noerr) then
                     got_tbry(itrc)=.false.
                     write(stdout,3) 'poc_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
                  endif
               elseif (itrc.eq.iSIL_) then
                  got_tbry(itrc)=.true.
                  ierr=nf_inq_varid (bry_id, 'si_time', bry_tid(itrc))
                  ierr_all=ierr_all+ierr
                  if (ierr .ne. nf_noerr) then
                     got_tbry(itrc)=.false.
                     write(stdout,3) 'si_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
                  endif
               elseif (itrc.eq.iPHY_) then
                  got_tbry(itrc)=.true.
                  ierr=nf_inq_varid (bry_id, 'phy_time', bry_tid(itrc))
                  ierr_all=ierr_all+ierr
                  if (ierr .ne. nf_noerr) then
                     got_tbry(itrc)=.false.
                     write(stdout,3) 'phy_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
                  endif
               elseif (itrc.eq.iZOO_) then
                  got_tbry(itrc)=.true.
                  ierr=nf_inq_varid (bry_id, 'zoo_time', bry_tid(itrc))
                  ierr_all=ierr_all+ierr
                  if (ierr .ne. nf_noerr) then
                     got_tbry(itrc)=.false.
                     write(stdout,3) 'zoo_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
                  endif
               elseif (itrc.eq.iDOC_) then
                  got_tbry(itrc)=.true.
                  ierr=nf_inq_varid (bry_id, 'doc_time', bry_tid(itrc))
                  ierr_all=ierr_all+ierr
                  if (ierr .ne. nf_noerr) then
                     got_tbry(itrc)=.false.
                     write(stdout,3) 'doc_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
                  endif
               elseif (itrc.eq.iDIA_) then
                  got_tbry(itrc)=.true.
                  ierr=nf_inq_varid (bry_id, 'dia_time', bry_tid(itrc))
                  ierr_all=ierr_all+ierr
                  if (ierr .ne. nf_noerr) then
                     got_tbry(itrc)=.false.
                     write(stdout,3) 'dia_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
                  endif
               elseif (itrc.eq.iMES_) then
                  got_tbry(itrc)=.true.
                  ierr=nf_inq_varid (bry_id, 'mes_time', bry_tid(itrc))
                  ierr_all=ierr_all+ierr
                  if (ierr .ne. nf_noerr) then
                     got_tbry(itrc)=.false.
                     write(stdout,3) 'mes_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
                  endif
               elseif (itrc.eq.iBSI_) then
                  got_tbry(itrc)=.true.
                  ierr=nf_inq_varid (bry_id, 'bsi_time', bry_tid(itrc))
                  ierr_all=ierr_all+ierr
                  if (ierr .ne. nf_noerr) then
                     got_tbry(itrc)=.false.
                     write(stdout,3) 'bsi_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
                  endif
               elseif (itrc.eq.iFER_) then
                  got_tbry(itrc)=.true.
                  ierr=nf_inq_varid (bry_id, 'fer_time', bry_tid(itrc))
                  ierr_all=ierr_all+ierr
                  if (ierr .ne. nf_noerr) then
                     got_tbry(itrc)=.false.
                     write(stdout,3) 'fer_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
                  endif
               elseif (itrc.eq.iBFE_) then
                  got_tbry(itrc)=.true.
                  ierr=nf_inq_varid (bry_id, 'bfe_time', bry_tid(itrc))
                  ierr_all=ierr_all+ierr
                  if (ierr .ne. nf_noerr) then
                     got_tbry(itrc)=.false.
                     write(stdout,3) 'bfe_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
                  endif
               elseif (itrc.eq.iGOC_) then
                  got_tbry(itrc)=.true.
                  ierr=nf_inq_varid (bry_id, 'goc_time', bry_tid(itrc))
                  ierr_all=ierr_all+ierr
                  if (ierr .ne. nf_noerr) then
                     got_tbry(itrc)=.false.
                     write(stdout,3) 'goc_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
                  endif
               elseif (itrc.eq.iSFE_) then
                  got_tbry(itrc)=.true.
                  ierr=nf_inq_varid (bry_id, 'sfe_time', bry_tid(itrc))
                  ierr_all=ierr_all+ierr
                  if (ierr .ne. nf_noerr) then
                     got_tbry(itrc)=.false.
                     write(stdout,3) 'sfe_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
                  endif
               elseif (itrc.eq.iDFE_) then
                  got_tbry(itrc)=.true.
                  ierr=nf_inq_varid (bry_id, 'dfe_time', bry_tid(itrc))
                  ierr_all=ierr_all+ierr
                  if (ierr .ne. nf_noerr) then
                     got_tbry(itrc)=.false.
                     write(stdout,3) 'dfe_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
                  endif
                  ierr_all=ierr_all+ierr
               elseif (itrc.eq.iDSI_) then
                  got_tbry(itrc)=.true.
                  ierr=nf_inq_varid (bry_id, 'dsi_time', bry_tid(itrc))
                  ierr_all=ierr_all+ierr
                  if (ierr .ne. nf_noerr) then
                     got_tbry(itrc)=.false.
                     write(stdout,3) 'dsi_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
                  endif
               elseif (itrc.eq.iNFE_) then
                  got_tbry(itrc)=.true.
                  ierr=nf_inq_varid (bry_id, 'nfe_time', bry_tid(itrc))
                  ierr_all=ierr_all+ierr
                  if (ierr .ne. nf_noerr) then
                     got_tbry(itrc)=.false.
                     write(stdout,3) 'nfe_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
                  endif
               elseif (itrc.eq.iNCH_) then
                  got_tbry(itrc)=.true.
                  ierr=nf_inq_varid (bry_id, 'nch_time', bry_tid(itrc))
                  ierr_all=ierr_all+ierr
                  if (ierr .ne. nf_noerr) then
                     got_tbry(itrc)=.false.
                     write(stdout,3) 'nch_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
                  endif
               elseif (itrc.eq.iDCH_) then
                  got_tbry(itrc)=.true.
                  ierr=nf_inq_varid (bry_id, 'dch_time', bry_tid(itrc))
                  ierr_all=ierr_all+ierr
                  if (ierr .ne. nf_noerr) then
                     got_tbry(itrc)=.false.
                     write(stdout,3) 'dch_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
                  endif
               elseif (itrc.eq.iNO3_) then
                  got_tbry(itrc)=.true.
                  ierr=nf_inq_varid (bry_id, 'no3_time', bry_tid(itrc))
                  ierr_all=ierr_all+ierr
                  if (ierr .ne. nf_noerr) then
                     got_tbry(itrc)=.false.
                     write(stdout,3) 'no3_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
                  endif
               elseif (itrc.eq.iNH4_) then
                  got_tbry(itrc)=.true.
                  ierr=nf_inq_varid (bry_id, 'nh4_time', bry_tid(itrc))
                  ierr_all=ierr_all+ierr
                  if (ierr .ne. nf_noerr) then
                     got_tbry(itrc)=.false.
                     write(stdout,3) 'nh4_time', bry_file(1:lstr)
c     goto 99                                     !--> ERROR
                  endif
               endif   ! condition on itrc value
c$$$               write(*,*) 'LOGICAL:',got_tbry(itrc),itrc
            enddo      ! loop on itrc
# endif                          
!            if (ierr_all.ne.nf_noerr) write(stdout,'(/1x,4A/)') 'ERROR ',
!     &           'in get_all_bry TIME ''',bry_file(1:lstr),   '''.'
            
            ierr_all=0
            
# ifdef PISCES
#  ifdef OBC_WEST
            if (got_tbry(iNO3_)) then
               ierr=nf_inq_varid (bry_id, 'NO3_west', 
     &              tbry_west_id(iNO3_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'NO3_west'
               ierr_all=ierr_all+ierr
            endif
            
            if (got_tbry(iPO4_)) then
               ierr=nf_inq_varid (bry_id, 'PO4_west', 
     &              tbry_west_id(iPO4_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'PO4_west'
               ierr_all=ierr_all+ierr
            endif
            
            if (got_tbry(iSIL_)) then
               ierr=nf_inq_varid (bry_id, 'Si_west', 
     &              tbry_west_id(iSIL_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'Si_west'
               ierr_all=ierr_all+ierr
            endif
            
            if (got_tbry(iOXY_)) then
               ierr=nf_inq_varid (bry_id, 'O2_west', 
     &              tbry_west_id(iOXY_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'O2_west'
               ierr_all=ierr_all+ierr
            endif

            if (got_tbry(iDIC_)) then
               ierr=nf_inq_varid (bry_id, 'DIC_west', 
     &              tbry_west_id(iDIC_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'DIC_west'
               ierr_all=ierr_all+ierr
            endif
            
            if (got_tbry(iDOC_)) then
               ierr=nf_inq_varid (bry_id, 'DOC_west', 
     &              tbry_west_id(iDOC_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'DOC_west'
               ierr_all=ierr_all+ierr
            endif
            
            if (got_tbry(iTAL_)) then
               ierr=nf_inq_varid (bry_id, 'TALK_west', 
     &              tbry_west_id(iTAL_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'TALK_west'
               ierr_all=ierr_all+ierr
            endif
            
            if (got_tbry(iFER_)) then
               ierr=nf_inq_varid (bry_id, 'FER_west', 
     &              tbry_west_id(iFER_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'FER_west'
               ierr_all=ierr_all+ierr
            endif
#  endif /* OBC_WEST */
            
#  ifdef OBC_EAST
            if (got_tbry(iNO3_)) then
               ierr=nf_inq_varid (bry_id, 'NO3_east', 
     &              tbry_east_id(iNO3_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'NO3_east'
               ierr_all=ierr_all+ierr
            endif
            
            if (got_tbry(iPO4_)) then
               ierr=nf_inq_varid (bry_id, 'PO4_east', 
     &              tbry_east_id(iPO4_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'PO4_east'
               ierr_all=ierr_all+ierr
            endif
            
            if (got_tbry(iSIL_)) then
               ierr=nf_inq_varid (bry_id, 'Si_east', 
     &              tbry_east_id(iSIL_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'Si_east'
               ierr_all=ierr_all+ierr
            endif
            
            if (got_tbry(iOXY_)) then
               ierr=nf_inq_varid (bry_id, 'O2_east', 
     &              tbry_east_id(iOXY_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'O2_east'
               ierr_all=ierr_all+ierr
            endif
            
            if (got_tbry(iDIC_)) then
               ierr=nf_inq_varid (bry_id, 'DIC_east', 
     &              tbry_east_id(iDIC_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'DIC_east'
               ierr_all=ierr_all+ierr
            endif
            
            if (got_tbry(iDOC_)) then
               ierr=nf_inq_varid (bry_id, 'DOC_east', 
     &            tbry_east_id(iDOC_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'DOC_east'
               ierr_all=ierr_all+ierr
            endif
            
            if (got_tbry(iTAL_)) then
               ierr=nf_inq_varid (bry_id, 'TALK_east', 
     &              tbry_east_id(iTAL_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'TALK_east'
               ierr_all=ierr_all+ierr
            endif
            
            if (got_tbry(iFER_)) then
               ierr=nf_inq_varid (bry_id, 'FER_east', 
     &              tbry_east_id(iFER_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'FER_east'
               ierr_all=ierr_all+ierr
            endif
#  endif /* OBC_EAST */

#  ifdef OBC_SOUTH
            if (got_tbry(iNO3_)) then
               ierr=nf_inq_varid (bry_id, 'NO3_south', 
     &              tbry_south_id(iNO3_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'NO3_south'
               ierr_all=ierr_all+ierr
            endif
            
            if (got_tbry(iPO4_)) then
               ierr=nf_inq_varid (bry_id, 'PO4_south', 
     &              tbry_south_id(iPO4_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'PO4_south'
               ierr_all=ierr_all+ierr
            endif
            
            if (got_tbry(iSIL_)) then
               ierr=nf_inq_varid (bry_id, 'Si_south', 
     &              tbry_south_id(iSIL_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'Si_south'
               ierr_all=ierr_all+ierr
            endif
            
            if (got_tbry(iOXY_)) then
               ierr=nf_inq_varid (bry_id, 'O2_south', 
     &              tbry_south_id(iOXY_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'O2_south'
               ierr_all=ierr_all+ierr
            endif
            
            if (got_tbry(iDIC_)) then
               ierr=nf_inq_varid (bry_id, 'DIC_south', 
     &              tbry_south_id(iDIC_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'DIC_south'
               ierr_all=ierr_all+ierr
            endif
            
            if (got_tbry(iDOC_)) then
               ierr=nf_inq_varid (bry_id, 'DOC_south', 
     &              tbry_south_id(iDOC_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'DOC_south'
               ierr_all=ierr_all+ierr
            endif
            
            if (got_tbry(iTAL_)) then
               ierr=nf_inq_varid (bry_id, 'TALK_south', 
     &              tbry_south_id(iTAL_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'TALK_south'
               ierr_all=ierr_all+ierr
            endif
            
            if (got_tbry(iFER_)) then
               ierr=nf_inq_varid (bry_id, 'FER_south', 
     &              tbry_south_id(iFER_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'FER_south'
               ierr_all=ierr_all+ierr
            endif
#  endif /* OBC_SOUTH */

#  ifdef OBC_NORTH
            if (got_tbry(iNO3_)) then
               ierr=nf_inq_varid (bry_id, 'NO3_north', 
     &              tbry_north_id(iNO3_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'NO3_north'
               ierr_all=ierr_all+ierr
            endif
            
            if (got_tbry(iPO4_)) then
               ierr=nf_inq_varid (bry_id, 'PO4_north', 
     &              tbry_north_id(iPO4_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'PO4_north'
               ierr_all=ierr_all+ierr
            endif
            
            if (got_tbry(iSIL_)) then
               ierr=nf_inq_varid (bry_id, 'Si_north', 
     &              tbry_north_id(iSIL_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'Si_north'
               ierr_all=ierr_all+ierr
            endif
            
            if (got_tbry(iOXY_)) then
               ierr=nf_inq_varid (bry_id, 'O2_north', 
     &              tbry_north_id(iOXY_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'O2_north'
               ierr_all=ierr_all+ierr
            endif

            if (got_tbry(iDIC_)) then
               ierr=nf_inq_varid (bry_id, 'DIC_north', 
     &              tbry_north_id(iDIC_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'DIC_north'
               ierr_all=ierr_all+ierr
            endif
            
            if (got_tbry(iDOC_)) then
               ierr=nf_inq_varid (bry_id, 'DOC_north', 
     &              tbry_north_id(iDOC_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'DOC_north'
               ierr_all=ierr_all+ierr
            endif
            
            if (got_tbry(iTAL_)) then
               ierr=nf_inq_varid (bry_id, 'TALK_north', 
     &              tbry_north_id(iTAL_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'TALK_north'
               ierr_all=ierr_all+ierr
            endif
            
            if (got_tbry(iFER_)) then
               ierr=nf_inq_varid (bry_id, 'FER_north', 
     &              tbry_north_id(iFER_))
               if (ierr.ne.nf_noerr) write(stdout,1) 'FER_north'
               ierr_all=ierr_all+ierr
            endif
#  endif /* OBC_NORTH */
# else
      ERROR ###  CPP-keys 'T_FRC_BRY' and 'BIOLOGY' are defined,
      ERROR ###  but no code is provided to set up boundary arrays.
# endif /* PISCES */
            
            ierr=ierr_all
            if (ierr.eq.nf_noerr) then
              do itrc=3,NT
                if(got_tbry(itrc)) then
                  call set_cycle (bry_id, bry_tid(itrc), 
     &            ntbry1(itrc),bry_cycle1(itrc), bry_ncycle1(itrc), 
     $                    bry_rec1(itrc))
                  bry_time1(1,itrc)=-1.E+20
                  bry_time1(2,itrc)=-1.E+20
                  itbry1(itrc)=1
                endif
              enddo
            else
              write(stdout,'(8x,4A)') 'ERROR(s) occur while examining',
     &           ' content of netCDF file ''', bry_file(1:lstr), '''.'
            endif
         endif      ! test on ierr.no_err 
      endif         ! test on iic
 1    format(' ERROR in get_all_bry: cannot find variable ''',A,'''')!

! Read data from the file:  Check if model time is bounded by past
!===== ==== ==== === =====  and future data times: if not, increment
! record and cycling indices, flip time index and read a new portion
! of data. Repeat until model time falls between the two data times.
!     
      do itrc=3,NT              ! loop on biology
        if(got_tbry(itrc)) then
          do while (bry_time1(itbry1(itrc),itrc).lt.time+dt .and. 
     $             ierr.eq.nf_noerr)
            call advance_cycle (bry_cycle1(itrc),ntbry1(itrc),
     $           bry_ncycle1(itrc),bry_rec1(itrc))
            if (ierr.eq.nf_noerr) then
            ierr=nf_get_var1_FTYPE 
     $           (bry_id, bry_tid(itrc), bry_rec1(itrc), cff)
             if (ierr.eq.nf_noerr) then
               itbry1(itrc)=min(3-itbry1(itrc),ntbry1(itrc))
               bry_time1(itbry1(itrc),itrc)=cff*day2sec + 
     $              bry_cycle1(itrc)*bry_ncycle1(itrc)
               ierr_all=0
# ifdef OBC_WEST

               ierr=nf_read_bry_EW (tbry_west_dt(0,1,itbry1(itrc),itrc),
     &              bry_id,tbry_west_id(itrc),  bry_rec1(itrc), r3dvar)
               if (ierr.ne.nf_noerr)  write(stdout,2)  'trc_west'
               ierr_all=ierr_all+ierr
# endif
# ifdef OBC_EAST

              ierr=nf_read_bry_EW (tbry_east_dt(0,1,itbry1(itrc),itrc), 
     &              bry_id,tbry_east_id(itrc),  bry_rec1(itrc), r3dvar)
               if (ierr.ne.nf_noerr)  write(stdout,2)  'trc_east'
               ierr_all=ierr_all+ierr
# endif
# ifdef OBC_SOUTH
              ierr=nf_read_bry_NS (tbry_south_dt(0,1,itbry1(itrc),itrc),
     &              bry_id,tbry_south_id(itrc),  bry_rec1(itrc), r3dvar)
               if (ierr.ne.nf_noerr)  write(stdout,2)  'trc_south'
               ierr_all=ierr_all+ierr
# endif
# ifdef OBC_NORTH
              ierr=nf_read_bry_NS (tbry_north_dt(0,1,itbry1(itrc),itrc),
     &              bry_id,tbry_north_id(itrc),  bry_rec1(itrc), r3dvar)
               if (ierr.ne.nf_noerr)  write(stdout,2)  'trc_north'
               ierr_all=ierr_all+ierr
# endif
               ierr=ierr_all
               
               if (ierr.eq.0) then
                  write(stdout,'(6x,A,9x,A,1x,G12.4,1x,I4)')
     &                 'GET_BRY_BIO    -- Read all boundary data',
     &                 'for time =', cff 
#ifdef MPI
     &                 , mynode
#endif
               else
                  write(stdout,'(1x,2A,I4/8x,3A)')
     &                 'ERROR(s) occur while trying to read record ',
     &                 bry_rec,  'in file ''',bry_file(1:lstr),'''.'
               endif 
             else
               write(stdout,2) 'bry_time'
             endif
            else
              write(stdout,'(/1x,A,I4,1x,A,I4/7x,4A/7x,2(A,G12.4)/)')
     &        'ERROR in get_bry_bio: requested time record ',  bry_rec,
     &         'exeeds the last record', ntbry,  'available in netCDF ',
     &         'file ''',   bry_file(1:lstr),  '''',  'tdays = ', tdays,
     &           '  but the last available  bry_time =',
     &           bry_time(itbry1(itrc))*sec2day
            endif

          enddo  !do while
        endif    !got_tbry(itrc)
      enddo      ! itrc
         

  2   format(' ERROR in get_bry_bio: cannot read variable ''',A,'''')
  3   format(/,' GET_BRY_BIO - unable to find climatology variable: ',
     &       a,/,15x,'in climatology NetCDF file: ',a)
      if (ierr.ne.nf_noerr) may_day_flag=2
      return
      end 

      subroutine set_bry_bio (tile)
      implicit none
      integer tile
# include "param.h"
# include "compute_tile_bounds.h"
      call set_bry_bio_tile (Istr,Iend,Jstr,Jend)
      return
      end


      subroutine set_bry_bio_tile (Istr,Iend,Jstr,Jend)
!
!--------------------------------------------------------------------
!  Set-up biology boundary forcing fields
!--------------------------------------------------------------------
!
      implicit none
      integer Istr,Iend,Jstr,Jend, i,j,k, it1,it2, itrc
      real cff, cff1,cff2
# include "param.h"
# include "scalars.h"
# include "boundary.h"
!
# include "compute_extended_bounds.h"

      do itrc=3,NT
         if(got_tbry(itrc)) then
         it1=3-itbry1(itrc)
         it2=itbry1(itrc)
         cff1=bry_time1(it2,itrc)-time
         cff2=time-bry_time1(it1,itrc)
         if (ZEROTH_TILE .and. cff1.lt.dt) synchro_flag=.true.
         
         if (cff1.ge.0. .and. cff2.ge.0.) then
            cff=1./(cff1+cff2)  ! interpolate
            cff1=cff1*cff       ! boundary values 
            cff2=cff2*cff       ! in time 

# ifdef OBC_WEST
            if (WESTERN_EDGE) then
               do k=1,N
                  do j=JstrR,JendR
                  tbry_west(j,k,itrc)=cff1*tbry_west_dt(j,k,it1,itrc)
     &                    +cff2*tbry_west_dt(j,k,it2,itrc)
                  enddo
               enddo
            endif
# endif

# ifdef OBC_EAST
            if (EASTERN_EDGE) then
               do k=1,N
                  do j=JstrR,JendR
                  tbry_east(j,k,itrc)=cff1*tbry_east_dt(j,k,it1,itrc)
     &                    +cff2*tbry_east_dt(j,k,it2,itrc)
                  enddo
               enddo
            endif
# endif


# ifdef OBC_SOUTH
            if (SOUTHERN_EDGE) then
               do k=1,N
                  do j=IstrR,IendR
                  tbry_south(j,k,itrc)=cff1*tbry_south_dt(j,k,it1,itrc)
     &                    +cff2*tbry_south_dt(j,k,it2,itrc)
                  enddo
               enddo
            endif
# endif


# ifdef OBC_NORTH
            if (NORTHERN_EDGE) then
               do k=1,N
                  do j=IstrR,IendR
                  tbry_north(j,k,itrc)=cff1*tbry_north_dt(j,k,it1,itrc)
     &                    +cff2*tbry_north_dt(j,k,it2,itrc)
                  enddo
               enddo
            endif
# endif


         elseif (ZEROTH_TILE) then
            write(stdout,'(/2(1x,A)/3(1x,A,F16.10)/)')
     &      'SET_BRY_ALL_TILE - current model time is out of bounds of',
     &      '''bry_time''.',     'BRY_TSTART=',   bry_time(it1)*sec2day,
     &      'TDAYS=',  tdays,    'BRY_TEND=',     bry_time(it2)*sec2day
            may_day_flag=2
         endif
         endif
      enddo

      return
      end
#else
      subroutine get_bry_bio_empty
      end
#endif
