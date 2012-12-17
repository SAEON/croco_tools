! $Id$
!======================================================================
! ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France
! The two other branches from UCLA (Shchepetkin et al) 
! and Rutgers University (Arango et al) are under MIT/X style license.
! ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
! 
! ROMS_AGRIF website : http://www.romsagrif.org
!======================================================================
!
! Other model parameters
!
! time      Model time since initialization (s).  
! tdays     Model time since initialization (days).
! iic       Timestep counter.
! nstp      Time index corresponding to the actual time step.
! nnew      Time index corresponding to the newest time step.
! dt        Size of timestep (s).  
! R0        Background constant density anomaly (kg/m^3) used in
!             linear equation of state.
! Tcoef     Thermal expansion coefficient in linear equation of
!             state. 
! Scoef     Saline contraction coefficient in linear equation of
!             state.
! twrite    Number of days before starting to write and to plot
! ntstart   Ntarting time step
! ntimes    Total number time-steps in current run
! nwrite    Number of time-steps between making a plot
! noutput   Number of time-steps between writing in output file
!            
      real time,tdays
      integer iic,nstp,nnew
      common /time_indices/ time,tdays,iic,nstp,nnew

      real dt, f, R0,Tcoef,Scoef, twrite
      integer ntstart,ntimes,nwrite,noutput,nplot
      common /scalars/ ntstart,ntimes,nwrite,noutput,nplot,
     &                 dt,f,twrite,R0,Tcoef,Scoef

