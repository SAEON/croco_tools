! $Id$
#ifdef PSOURCE
!
! Nsrc       Number of point Sources/Sinks.
! Dsrc       Direction of point Sources/Sinks:  0 = along XI-;
!                                               1 = along ETA-direction.
! Isrc,Jsrc  i,j-grid location of point Sources/Sinks,
!              0 <= Isrc <= Lm+1;   0 =<- Jsrc <= Mm+1.
! Lsrc       Logical switch indicating which tracer field to apply
!                                         the point Sources/Sinks.
! Qsrc       Mass transport profile (m3/s) of point Sources/Sinks.
! Qbar       Vertically integrated Qsrc (m3/s) of point
! QbarG      Latest two-time snapshots of vertically integrated
!              mass transport (m3/s) of point Sources/Sinks.
! Tsrc       Tracer (tracer units) point Sources/Sinks.
! TsrcG      Latest two-time snapshots of tracer (tracer units)
!              point Sources/Sinks.
! Qshape     Nondimensional shape function to distribute mass
!             ass point Sources/Sinks vertically.
!
      real Qbar(Msrc)
      common /sources_Qbar/ Qbar

      real Qsrc(Msrc,N)
      common /source_Qsrc/ Qsrc

      real Qshape(Msrc,N)
      common /source_Qshape/ Qshape

      real Tsrc(Msrc,N,NT)
      common /source_Tsrc/ Tsrc

      real Tsrc0(Msrc,NT)
      common /source_Tsrc0/ Tsrc0

      real lasrc(Msrc)
      common /source_lasrc/ lasrc

      real losrc(Msrc)
      common /source_losrc/ losrc

# ifndef ANA_PSOURCE
      real QbarG(Msrc,2)
      common /source_QbarG/ QbarG

      real TsrcG(Msrc,N,2,NT)
      common /source_TsrcG/ TsrcG
# endif

      integer Nsrc
      common /source_Nsrc/ Nsrc

      integer Dsrc(Msrc)
      common /source_Dsrc/ Dsrc

      integer Isrc(Msrc)
      common /source_Isrc/ Isrc

      integer Jsrc(Msrc)
      common /source_Jsrc/ Jsrc

      logical Lsrc(Msrc,NT)
      common /source_Lsrc/ Lsrc

#ifdef PSOURCE_NCFILE
      real rivdir(Msrc)
      common /source_rivdir/ rivdir
#endif

# ifdef MPI
      integer Isrc_mpi(Msrc,0:NNODES-1)
      common /source_Isrc_mpi/ Isrc_mpi
      integer Jsrc_mpi(Msrc,0:NNODES-1)
      common /source_Jsrc_mpi/ Jsrc_mpi
# endif

#endif

