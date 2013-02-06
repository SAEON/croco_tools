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
CC----------------------------------------------------------------
CC
CC                     ROUTINE trcini.pisces.h
CC                     ************************
CC
CC  PURPOSE :
CC  ---------
CC     Initialisation of PISCES biological and chemical variables
CC
CC   INPUT :
CC   -----
CC      common
CC              all the common defined in opa 
CC
CC
CC   OUTPUT :                   : no
CC   ------
CC
CC   EXTERNAL :
CC   ----------
CC         p4zche
CC
CC   MODIFICATIONS:
CC   --------------
CC      original  : 1988-07  E. MAIER-REIMER      MPI HAMBURG
CC      additions : 1999-10  O. Aumont and C. Le Quere
CC      additions : 2002     O. Aumont (PISCES)
CC
CC      changes for ROMS:
CC                  2007     P. Marchesiello
CC
CC---------------------------------------------------------------------
CC local declarations
CC ==================
      INTEGER ichl,iband,mo
      INTEGER jpmois,jpan
      PARAMETER (jpan=1, jpmois=12)

      CHARACTER*34 clname

      INTEGER ipi,ipj,ipk,istep(jpmois),itime,istep0(jpan)
      INTEGER numriv,numdust,numbath,numdep

      REAL xtoto,expide,denitide
      REAL ndepo(jpi,jpj),river(jpi,jpj)
      REAL riverdoc(jpi,jpj)
      REAL dustmp(GLOBAL_2D_ARRAY,12)
      REAL zsecond, zlon(jpi,jpj),zlat(jpi,jpj),zlev(jpk),zdate0
      REAL zmaskt

c      INCLUDE 'netcdf.inc'
# include "netcdf.inc"
# include "ncscrum.h"
# include "scalars.h"
      integer ncid, varid, dimid, ierr,
     &        lstr, lenstr, nf_fread, nrec_dust, irec
C
C Compute record length for direct access FILE
C (this length depends on 512 for the t3d machine)
C
      rfact = rdt 
      rfactr = 1./rfact
      WRITE(numout,*) ' Tracer time step=',rfact,' rdt=',rdt
      rfact2= rfact 
      rfact2r = 1./rfact2
      write(numout,*) ' Biology time step=',rfact2
C
C    INITIALISE DUST INPUT FROM ATMOSPHERE
C    -------------------------------------
C
!      IF (bdustfer) THEN
!        lstr=lenstr(bioname)
!        ierr=nf_open (bioname(1:lstr), nf_nowrite, ncid)
!        if (ierr .ne. nf_noerr) then
!           write(stdout,4) bioname
!        endif
!        ierr=nf_inq_varid (ncid,"dust",varid)
!        if (ierr .ne. nf_noerr) then
!          write(stdout,5) "dust", bioname
 !       endif
!        ierr=nf_inq_dimid(ncid,"dust_time",dimid)
!        ierr=nf_inq_dimlen(ncid,dimid,nrec_dust)
!        do irec=1,nrec_dust
!          ierr=nf_fread(dustmp(START_2D_ARRAY,irec), ncid, varid,
!     &                                              irec, r2dvar)
!          if (ierr .ne. nf_noerr) then
!            write(stdout,6) "dust", irec 
!          endif
!        enddo
!        ierr=nf_close(ncid)
!        write(stdout,'(6x,A,1x,I4)') 
!     &                   'TRCINI_PISCES -- Read dust deposition '
!#ifdef MPI
!     &                                                   , mynode
!#endif
!  4     format(/,' TRCINI_PISCES - unable to open forcing netCDF ',1x,A)
!  5     format(/,' TRCINI_PISCES - unable to find forcing variable: ',A,
!     &                               /,14x,'in forcing netCDF file: ',A)
!  6     format(/,' TRCINI_PISCES - error while reading variable: ',A,2x,
!     &                                           ' at TIME index = ',i4)
!
!        do irec=1,nrec_dust
!          do j=Jstr,Jend
!	    do i=Istr,Iend
 !             dustmo(i,j,irec)=dustmp(i,j,irec)
!            enddo
!          enddo
!        enddo
!
!      ELSE
!
!        do irec=1,nrec_dust
!          do j=Jstr,Jend
!	    do i=Istr,Iend
!	      dustmo(i,j,irec)=0.
!	    enddo
!	  enddo
!        enddo
!
!      ENDIF
!C
C    INITIALISE THE NUTRIENT INPUT BY RIVERS
C    ---------------------------------------
C
C       IF (briver) THEN
C         clname='river.orca.nc'
C         CALL flinopen(clname,nizoom,jpi,njzoom,jpj,.false.,ipi,ipj,0
C     $        ,zlon,zlat,zlev,itime,istep0,zdate0,zsecond,numriv)
C         CALL flinget(numriv,'riverdic',jpidta,jpjdta,0,jpan,1,
C     $          1,nizoom,jpi,njzoom,jpj,river)
C         CALL flinget(numriv,'riverdoc',jpidta,jpjdta,0,jpan,1,
C     $          1,nizoom,jpi,njzoom,jpj,riverdoc)
C         CALL flinclo(numriv)
C       ELSE
          river(:,:)=0.
          riverdoc(:,:)=0.
C       ENDIF
       
C
C    INITIALISE THE N INPUT BY DUST
C    -------------------------------
C
C       IF (bndepo) THEN
C         clname='ndeposition.orca.nc'
C         CALL flinopen(clname,nizoom,jpi,njzoom,jpj,.false.,ipi,ipj,0
C     $        ,zlon,zlat,zlev,itime,istep0,zdate0,zsecond,numdep)
C         CALL flinget(numdep,'ndep',jpidta,jpjdta,0,jpan,1,
C     $          1,nizoom,jpi,njzoom,jpj,ndepo)
C         CALL flinclo(numdep)
C       ELSE
          ndepo(:,:)=0.
C       ENDIF
C
C    Computation of the coastal mask.
C    Computation of an island mask to enhance coastal supply
C    of iron
C    -------------------------------------------------------
C
      IF (bsedinput) THEN
        do j=Jstr,Jend
          do i=Istr,Iend
            cmask(i,j,jpk)=1.
          end do
        end do
c        do k=1,jpk-1
c          do j=Jstr,Jend
c            do i=Istr,Iend
c                zmaskt=tmask(i+1,j,k)*tmask(i-1,j,k)
c     &                *tmask(i,j+1,k)*tmask(i,j-1,k)
c                if (zmaskt.eq.0) then
c                  cmask(i,j,k)=0.1
c                endif
c            enddo
c          enddo
c        enddo
        do k=1,jpk
          do j=Jstr,Jend
            do i=Istr,Iend
              expide=min(8.,(fsdept(i,j,k)/500.)**(-1.5))
              denitide=-0.9543+0.7662*log(expide)-
     &                                0.235*log(expide)**2
              cmask(i,j,k)=cmask(i,j,k)*min(1.,exp(denitide)/0.5)
            enddo
          enddo
        enddo
      ELSE
        cmask(:,:,:)=0.
      ENDIF
C
C     Computation of the total atmospheric supply of Si
C     -------------------------------------------------
C
C      sumdepsi=0.
C      DO mo=1,12
C        DO j=2,jpjm1
C          DO i=2,jpim1
C            sumdepsi=sumdepsi+dustmo(i,j,mo)/(12.*rmoss)*8.8
C     &         *0.075/28.1*e1t(i,j)*e2t(i,j)*tmask(i,j,1)
C          END DO
C        END DO
C      END DO
C
C    COMPUTATION OF THE N/P RELEASE DUE TO COASTAL RIVERS
C    COMPUTATION OF THE Si RELEASE DUE TO COASTAL RIVERS 
C    ---------------------------------------------------
C
      DO j=1,jpj
        DO i=1,jpi
          cotdep(i,j)=river(i,j)*1E9/(12.*raass*e1t(i,j)
     &              *e2t(i,j)*fse3t(i,j,1))*tmask(i,j,1)
          rivinp(i,j)=(river(i,j)+riverdoc(i,j))*1E9
     &      /(31.6*raass*e1t(i,j)*e2t(i,j)*fse3t(i,j,1))
     &                                         *tmask(i,j,1)
          nitdep(i,j)=7.6*ndepo(i,j)*tmask(i,j,1)/
     &                             (14E6*raass*fse3t(i,j,1))
        END DO
      END DO

C      rivpo4input=0.
C      rivalkinput=0.
C      nitdepinput=0.
C      DO j=2,jpjm1
C        DO i=2,jpim1
C          rivpo4input=rivpo4input+rivinp(i,j)*(e1t(i,j)*e2t(i,j)
C     &                           *fse3t(i,j,1))*tmask(i,j,1)*raass
C          rivalkinput=rivalkinput+cotdep(i,j)*(e1t(i,j)*e2t(i,j)
C     &                           *fse3t(i,j,1))*tmask(i,j,1)*raass
C          nitdepinput=nitdepinput+nitdep(i,j)*(e1t(i,j)*e2t(i,j)
C     &                           *fse3t(i,j,1))*tmask(i,j,1)*raass
C        END DO
C      END DO
C
C    Coastal supply of iron
C    ----------------------
C
      DO k=1,jpk
        DO j=1,jpj
          DO i=1,jpi
            ironsed(i,j,k)=sedfeinput*cmask(i,j,k)
     &                          /(fse3t(i,j,k)*rjjss)
          END DO
        END DO
      END DO
C
CC----------------------------------------------------------------------
CC
CC Initialize biological variables 
CC
CC----------------------------------------------------------------------
C
C Set biological ratios
C ---------------------
C
      rno3   = (16.)/122.
      po4r   = 1./122.
      o2nit  = 32./122.
      rdenit = 97.6/16.
      o2ut   = 131./122.
C
CC----------------------------------------------------------------------
CC
CC Initialize chemical variables 
CC
CC----------------------------------------------------------------------
C
C set atmospheric [o2] (atm) 
C --------------------------
C
      atcox = 0.20946
      hi(:,:,:)=1.E-8
C
C Set coefficients for chlorinity and calcite
C -------------------------------------------
C
      salchl = 1./1.80655
      calcon = 1.03E-2
      rtrn=1.E-15
C
C Set coefficients for apparent solubility equilibrium
C   of calcite (Ingle, 1800, eq. 6)
C ----------------------------------------------------
C
      akcc1 = -34.452
      akcc2 = -39.866
      akcc3 = 110.21
      akcc4 = -7.5752E-6
C
C
C Set coefficients for seawater pressure correction
C -------------------------------------------------
C
      devk1(1) = -25.5
      devk2(1) = 0.1271
      devk3(1) = 0. 
      devk4(1) = -3.08E-3 
      devk5(1) = 0.0877E-3 

      devk1(2) = -15.82
      devk2(2) = -0.0219
      devk3(2) = 0. 
      devk4(2) = 1.13E-3 
      devk5(2) = -0.1475E-3 

      devk1(3) = -29.48
      devk2(3) = 0.1622
      devk3(3) = 2.608E-3 
      devk4(3) = -2.84E-3 
      devk5(3) = 0. 

      devk1(4) = -14.51
      devk2(4) = 0.1211
      devk3(4) = -0.321E-3
      devk4(4) = -2.67E-3
      devk5(4) = 0.0427E-3

      devk1(5) = -23.12
      devk2(5) = 0.1758
      devk3(5) = -2.647E-3 
      devk4(5) = -5.15E-3 
      devk5(5) = 0.09E-3

      devk1(6) = -26.57
      devk2(6) = 0.2020
      devk3(6) = -3.042E-3
      devk4(6) = -4.08E-3
      devk5(6) = 0.0714E-3 

      devk1(7) = -25.60
      devk2(7) = 0.2324
      devk3(7) = -3.6246E-3 
      devk4(7) = -5.13E-3 
      devk5(7) = 0.0794E-3
C
      devkst = 0.23
      devks  = 35.4
C
C Set universal gas constants
C ---------------------------
C
      rgas = 83.131
      oxyco = 1./22.4144
C
C Set boron constants
C -------------------
      bor1 = 0.000232
      bor2 = 1./10.811
C
C Set volumetric solubility constants for co2 in mol/kg 
C from Weiss and Price (1980)
C -----------------------------------------------------
C
      c00 = -60.2409
      c01 = 93.4517
      c02 = 23.3585
      c03 = 0.023517
      c04 = -0.023656
      c05 = 0.0047036

      ca0 = -162.8301
      ca1 = 218.2968
      ca2 = 90.9241
      ca3 = -1.47696
      ca4 = 0.025695
      ca5 = -0.025225
      ca6 = 0.0049867
C
C Set coeff. for 1. dissoc. of carbonic acid (Millero, 1995)
C ---------------------------------------------------------------------
C
      c10 = -3670.7
      c11 =  62.008
      c12 = -9.7944
      c13 = 0.0118
      c14 = -0.000116
C
C Set coeff. for 2. dissoc. of carbonic acid (Millero, 1995)
C ---------------------------------------------------------------------
C
      c20 = -1394.7
      c21 = -4.777
      c22 = 0.0184
      c23 = -0.000118
C
C Set coeff. for 1. dissoc. of boric acid (Dickson and Goyet, 1994)
C ------------------------------------------------------------------
C
      cb0  = -8966.90
      cb1  = -2890.53
      cb2  = -77.942
      cb3  = 1.728
      cb4  = -0.0996
      cb5  = 148.0248
      cb6  = 137.1942
      cb7  = 1.62142
      cb8  = -24.4344
      cb9  = -25.085
      cb10 = -0.2474
      cb11 = 0.053105
C
C Set coeff. for dissoc. of water (Dickson and Riley, 1979, 
C   eq. 7, coefficient cw2 corrected from 0.9415 to 0.09415 
C   after pers. commun. to B. Bacastow, 1988)
C ---------------------------------------------------------
C
      cw0 = -13847.26
      cw1 = 148.9652
      cw2 = -23.6521
      cw3 = 118.67
      cw4 = -5.977
      cw5 = 1.0495
      cw6 = -0.01615
C
C Set coeff. for dissoc. of phosphate (Millero (1974)
C ---------------------------------------------------
C
      cp10 = 115.525
      cp11 = -4576.752
      cp12 = -18.453
      cp13 = -106.736
      cp14 = 0.69171
      cp15 = -0.65643
      cp16 = -0.01844

      cp20 = 172.0883
      cp21 = -8814.715
      cp22 = -27.927
      cp23 = -160.340
      cp24 = 1.3566
      cp25 = 0.37335
      cp26 = -0.05778

      cp30 = -18.141
      cp31 = -3070.75
      cp32 = 17.27039
      cp33 = 2.81197
      cp34 = -44.99486
      cp35 = -0.09984
C
C Set coeff. for dissoc. of phosphate (Millero (1974)
C ---------------------------------------------------
C
      cs10 = 117.385
      cs11 = -8904.2
      cs12 = -19.334
      cs13 = -458.79
      cs14 =  3.5913
      cs15 = 188.74
      cs16 = -1.5998
      cs17 = -12.1652
      cs18 = 0.07871
      cs19 = -0.001005
C
C Set volumetric solubility constants for o2 in ml/l (Weiss, 1970)
C ----------------------------------------------------------------
C
      ox0 = -58.3877
      ox1 = 85.8079
      ox2 = 23.8439
      ox3 = -0.034892
      ox4 = 0.015568
      ox5 = -0.0019387
C
C  FROM THE NEW BIOOPTIC MODEL PROPOSED JM ANDRE, WE READ HERE
C  A PRECOMPUTED ARRAY CORRESPONDING TO THE ATTENUATION COEFFICIENT
C  ----------------------------------------------------------------
C
c         open(49,file='kRGB61.txt',form='formatted')
c         do ichl=1,61
c           READ(49,*) xtoto,(xkrgb(iband,ichl),iband = 1,3)
c         end do
c         close(49)
C      
C
C  Call p4zche to initialize the chemical constants
C  ------------------------------------------------
C
      CALL p4zche(Istr,Iend,Jstr,Jend)
C
C
C  Initialize a counter for the computation of chemistry
C  -----------------------------------------------------
C
      ndayflxtr=0
C
      WRITE(numout,*) ' Initialisation of PISCES done'
