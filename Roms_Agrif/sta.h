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
#ifdef STATIONS
!  Include file "sta.h".
!  =====================
! NSTVARS    Number of stations variables.   
! stainfo    Station initial information.   
! istagrd    Index for station grid (embedding) location
! istastr    Index for station recording start time.           
! istaxgrd   Index for station x-grid location.               
! istaygrd   Index for station y-grid location.              
! istazgrd   Index for station z-grid location.             
! istalat    Index for station latitude location.            
! istalon    Index for station longitude location.          
! istadpt    Index for station depth.                      
! istatem    Index for station potential temperature.     
! istasal    Index for station salinity.                 
! istaden    Index for station density anomaly.          
! istav      Index for station v-component velocity.    
! istav      Index for station v-component velocity.          
! nstas      Number of stations.                              
! stagrd     Station/grid embedding correspondance array.   
! stainfo    Station data at input.                        
! stadata    Station variables data collected for output   
! staSigm    Station data at all sigma levels
! diagsta    Flag taht determines if it is time step station 

      integer NSTAVARS,
     &        istagrd,           istatstr,
     &        istaxgrd,          istaygrd,        istazgrd,
     &        istalon,           istalat,         istadpt,
     &        istatem,           istasal,         istaden, 
     &        istav,		 istau
      parameter (NSTAVARS=11,
     &        istagrd=-1,        istatstr=0,  
     &        istaxgrd=1,        istaygrd=2,      istazgrd=3, 
     &        istalon=4,         istalat=5,       istadpt=6,
     &        istatem=7,         istasal=8,       istaden=9, 
     &        istav=10,          istau=11                    ) 

      logical diagsta
      integer nstas, stagrd(Msta)
      common /stan/ nstas, diagsta

      real stainfo(istagrd:istazgrd,Msta)
      common /sta_info/ stainfo

      real staspval, stadeltap2c
      common /sta_scalars/ staspval, stadeltap2c

# ifdef ALL_SIGMA
      real stadata(1:NSTAVARS,Msta), staSigm(istadpt:istau,Msta,N)
c     common /sta_data/ stadata,stagrd
      common /sta_data/ stadata, staSigm
# else
      real stadata(1:NSTAVARS,Msta)
c     common /sta_data/ stadata,stagrd
      common /sta_data/ stadata
# endif

#endif /*STATIONS*/
