! $Id: init_sta.F 697 2011-04-11 12:35:17Z gcambon $
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


#ifdef XIOS

      module xios_module
      use xios
      implicit none

      public set_grid
      public set_domain_attr
      public set_axis_attr
      public set_field_attr
      public set_scalar

      contains

!!----------------------------------------------------------------------
!!----------------------------------------------------------------------
      SUBROUTINE set_grid( cdgrd, plon, plat)
!
!! ** Purpose :   define horizontal grids
!
# include "param.h"
      CHARACTER(LEN=1)            , INTENT(in) ::   cdgrd
      real plon(GLOBAL_2D_ARRAY)
      real plat(GLOBAL_2D_ARRAY)
      integer vtype, horiz_type, vert_type

      integer imin, imax, jmin, jmax, ni, nj
      integer nx_glo, ny_glo 
      integer istart, jstart
      integer data_ni,data_nj
!
# include "ncscrum.h"
# include "scalars.h"

! See ncscrum.h for r2dvar ...
      select case (cdgrd)
       case ("T")
        vtype=r2dvar
        nx_glo=LLm0+2
        ny_glo=MMm0+2
       case ("U")
        vtype=u2dvar
        nx_glo=LLm0+1
        ny_glo=MMm0+2
       case ("V")
        vtype=v2dvar
        nx_glo=LLm0+2
        ny_glo=MMm0+1
      end select

! From nf_fread.F
      vert_type=vtype/4                ! vertical and horizontal
      horiz_type=vtype-4*vert_type     ! grid types, then calculate
      jmin=horiz_type/2               ! starting indices indices
      imin=horiz_type-2*jmin          ! in horizontal directions.
!
      istart=1
      jstart=1

#ifdef MPI
      if (ii.gt.0) then
        istart=1-imin+iminmpi
        imin=1
      endif
      if (ii.eq.NP_XI-1) then
        imax=Lmmpi+1
      else
        imax=Lmmpi
      endif
      if (jj.gt.0) then
        jstart=1-jmin+jminmpi
        jmin=1
      endif
      if (jj.eq.NP_ETA-1) then
        jmax=Mmmpi+1
      else
        jmax=Mmmpi
      endif
#else
      imin=1
      imax=Lm+1
      jmin=1
      jmax=Mm+1
#endif

!     there is probably already a variable for ni and nj 
      ni=imax-imin+1
      nj=jmax-jmin+1

! from set_global_definitions.h
#ifdef THREE_GHOST_POINTS
# ifdef MPI
      data_ni = Lm+3+padd_X+2+1
      data_nj = Mm+3+padd_E+2+1
# endif
#else
# ifdef MPI
      data_ni = Lm+2+padd_X+1+1
      data_nj = Mm+2+padd_E+1+1
# endif
#endif


! notes iminmpi, jminmpi are computed in MPI_Setup.F

      CALL set_domain_attr("grid_"//cdgrd,ni_glo=nx_glo, nj_glo=ny_glo,
     &    ibegin=istart, ni=ni,jbegin=jstart,nj=nj)
! to do: below should have be rewritten to match all configs (periodic, number of ghost points, ...) see set_global_definitions.h
      CALL set_domain_attr("grid_"//cdgrd,data_dim=2,
     &    data_ibegin=START_1D_ARRAYXI-imin,
     &    data_ni=data_ni,
     &    data_jbegin=START_1D_ARRAYETA-jmin, 
     &    data_nj=data_nj)

      CALL set_domain_attr("grid_"//cdgrd,
     &    lonvalue=RESHAPE(plon(imin:imax,jmin:jmax), (/ni*nj/)),
     &    latvalue=RESHAPE(plat(imin:imax,jmin:jmax), (/ni*nj/)))

      END SUBROUTINE set_grid


!!----------------------------------------------------------------------
!!----------------------------------------------------------------------
      SUBROUTINE set_domain_attr( cdid, ni_glo, nj_glo, ibegin, jbegin,
     & ni, nj, zoom_ibegin, zoom_jbegin, zoom_ni, zoom_nj,
     & data_dim, data_ibegin, data_ni, data_jbegin, data_nj, 
     & lonvalue, latvalue, mask )

      CHARACTER(LEN=*) , INTENT(in) :: cdid
      INTEGER , OPTIONAL, INTENT(in) :: ni_glo, nj_glo, ibegin, jbegin 
      INTEGER , OPTIONAL, INTENT(in) :: ni, nj
      INTEGER , OPTIONAL, INTENT(in) :: data_dim, data_ibegin, data_ni
      INTEGER , OPTIONAL, INTENT(in) :: data_jbegin, data_nj
      INTEGER , OPTIONAL, INTENT(in) :: zoom_ibegin, zoom_jbegin
      INTEGER , OPTIONAL, INTENT(in) :: zoom_ni, zoom_nj
      REAL , DIMENSION(:) , OPTIONAL, INTENT(in) :: lonvalue, latvalue
      LOGICAL, DIMENSION(:,:), OPTIONAL, INTENT(in) :: mask

      IF ( xios_is_valid_domain(cdid) ) THEN
        CALL xios_set_domain_attr(cdid, ni_glo=ni_glo, nj_glo=nj_glo, 
     & ibegin=ibegin, jbegin=jbegin, ni=ni, nj=nj,
     & data_dim=data_dim, data_ibegin=data_ibegin, data_ni=data_ni, 
     & data_jbegin=data_jbegin, data_nj=data_nj, 
     & zoom_ibegin=zoom_ibegin, zoom_jbegin=zoom_jbegin, 
     & zoom_ni=zoom_ni, zoom_nj=zoom_nj,
     &    lonvalue=lonvalue, latvalue=latvalue,mask=mask )
      ENDIF

      IF ( xios_is_valid_domaingroup(cdid) ) THEN
         CALL xios_set_domaingroup_attr( cdid,
     &  ni_glo=ni_glo, nj_glo=nj_glo, 
     &  ibegin=ibegin, jbegin=jbegin, ni=ni, nj=nj,
     &  data_dim=data_dim, data_ibegin=data_ibegin, data_ni=data_ni, 
     &  data_jbegin=data_jbegin, data_nj=data_nj ,
     &  zoom_ibegin=zoom_ibegin, zoom_jbegin=zoom_jbegin, 
     &  zoom_ni=zoom_ni, zoom_nj=zoom_nj,
     &  lonvalue=lonvalue, latvalue=latvalue,mask=mask )
      ENDIF
      CALL xios_solve_inheritance()

      END SUBROUTINE set_domain_attr



!!----------------------------------------------------------------------
!!----------------------------------------------------------------------
      SUBROUTINE set_axis_attr( cdid, paxis )
      CHARACTER(LEN=*)      , INTENT(in) ::   cdid
      REAL, DIMENSION(:), INTENT(in) ::   paxis

      if ( xios_is_valid_axis (cdid) ) then
       CALL xios_set_axis_attr(cdid, size=size(paxis),value=paxis )
      endif
      if ( xios_is_valid_axisgroup(cdid) ) then
       CALL xios_set_axisgroup_attr(cdid, size=size(paxis),value=paxis )
      endif
      CALL xios_solve_inheritance()

      END SUBROUTINE set_axis_attr



!!----------------------------------------------------------------------
!!----------------------------------------------------------------------
      SUBROUTINE set_field_attr( cdid, freq_op, freq_offset )
      CHARACTER(LEN=*)          , INTENT(in) ::   cdid
      CHARACTER(LEN=*),OPTIONAL , INTENT(in) ::   freq_op
      CHARACTER(LEN=*),OPTIONAL , INTENT(in) ::   freq_offset
      IF ( xios_is_valid_field     (cdid) )  then
       CALL xios_set_field_attr(cdid, freq_op=freq_op, 
     &     freq_offset=freq_offset )
      endif
      IF ( xios_is_valid_fieldgroup(cdid) ) then
       CALL xios_set_fieldgroup_attr( cdid, freq_op=freq_op, 
     &     freq_offset=freq_offset )
      endif
      CALL xios_solve_inheritance()
      END SUBROUTINE set_field_attr


!!----------------------------------------------------------------------
!!----------------------------------------------------------------------
      SUBROUTINE set_scalar
!
!! ** Purpose :   define fake grids for scalar point
!
      REAL, DIMENSION(1,1) ::   zz = 1.
      !!----------------------------------------------------------------------

      CALL set_domain_attr('scalarpoint', ni_glo=10, nj_glo=1, 
     &  ibegin=1, jbegin=1, ni=1, nj=1)
      CALL set_domain_attr('scalarpoint', data_dim=1)
      CALL set_domain_attr('scalarpoint', lonvalue=(/ zz /), 
     &  latvalue=(/ zz /))

      END SUBROUTINE set_scalar



      end module xios_module



!=====================================================================

#include "cppdefs.h"
       subroutine init_xios(tile)

      implicit none
# include "param.h"
      integer tile
# include "compute_tile_bounds.h"
      call init_xios_tile (Istr,Iend,Jstr,Jend)
      return
      end


       subroutine init_xios_tile(Istr,Iend,Jstr,Jend)


!
!=====================================================================
!
! aponte xios (
          USE xios     ! XIOS io
          use xios_module
! aponte xios )
      implicit none
# include "param.h"
# include "grid.h"
# include "ocean3d.h"
# include "scalars.h"
# include "mpi_roms.h"
!

      integer tile
      integer ilocal_comm
      TYPE(xios_time) :: dtime 
      TYPE(xios_context) :: ctx_hdl
      TYPE(xios_field) :: field_hdl
      TYPE(xios_fieldgroup) :: fieldgroup_hdl
      TYPE(xios_file) :: file_hdl
      logical ok
!
      CHARACTER(len=10) :: clname
      CHARACTER(len=10) :: cdname
      integer Istr,Iend,Jstr,Jend

# include "compute_auxiliary_bounds.h"
!
# ifdef EW_PERIODIC
#  define IU_RANGE Istr,Iend
#  define IV_RANGE Istr,Iend
# else
#  define IU_RANGE Istr,IendR
#  define IV_RANGE IstrR,IendR
# endif

# ifdef NS_PERIODIC
#  define JU_RANGE Jstr,Jend
#  define JV_RANGE Jstr,Jend
# else
#  define JU_RANGE JstrR,JendR
#  define JV_RANGE Jstr,JendR
# endif
      cdname='roms'
      clname = cdname
# ifdef AGRIF
      IF( TRIM(Agrif_CFixed()) /= '0' )
!     & clname = TRIM(Agrif_CFixed())//"_"//TRIM(cdname)
     & clname =TRIM(cdname)//"."//TRIM(Agrif_CFixed())
# endif
# if defined MPI
      CALL xios_context_initialize(TRIM(clname), MPI_COMM_WORLD)
# else
      CALL xios_context_initialize(TRIM(clname), 0)
# endif
      CALL iom_swap( cdname )

! calendar parameters
      CALL xios_set_context_attr(TRIM(clname), calendar_type= 
     &      "Gregorian")
# ifdef START_DATE
! Should do with the start date variable
# else
      CALL xios_set_context_attr(TRIM(clname),
     &            start_date="1900-01-01 00:00:00" )  
# endif

      ! horizontal grid definition
#ifdef SPHERICAL
      CALL set_grid( "T", lonr, latr )
      CALL set_grid( "U", lonu, latu )
      CALL set_grid( "V", lonv, latv )
#else
      CALL set_grid( "T", xr, yr )
      CALL set_grid( "U", xp, yr )
      CALL set_grid( "V", xr, yp )
#endif

      ! vertical grid definition
      CALL set_axis_attr( "s_r", sc_r )
      CALL set_axis_attr( "s_w", sc_w )

      dtime%second = dt
      CALL xios_set_timestep(dtime)

      ! end definition phase
      CALL xios_close_context_definition()


      return
      end

      SUBROUTINE iom_swap( cdname )
      !!---------------------------------------------------------------------
      !!                   ***  SUBROUTINE  iom_swap  ***
      !!
      !! ** Purpose :  swap context between different agrif grid for xmlio_server
      !!---------------------------------------------------------------------
      USE xios
      CHARACTER(len=*), INTENT(in) :: cdname
      TYPE(xios_context) :: roms_hdl
#ifdef AGRIF
      IF( TRIM(Agrif_CFixed()) == '0' ) THEN
        CALL xios_get_handle(TRIM(cdname),roms_hdl)
      ELSE
        CALL xios_get_handle(TRIM(cdname)//"."//TRIM(Agrif_CFixed()),roms_hdl)
      ENDIF
#else
      CALL xios_get_handle(TRIM(cdname),roms_hdl)

#endif
      !
      CALL xios_set_current_context(roms_hdl)
      !
      END SUBROUTINE iom_swap

      SUBROUTINE iom_context_finalize( cdname )
      USE xios
      CHARACTER(LEN=*), INTENT(in) :: cdname
      TYPE(xios_context) :: roms_hdl
      !
      IF( xios_is_valid_context(cdname) ) THEN
         CALL iom_swap( cdname )   ! swap to cdname context
         CALL xios_context_finalize() ! finalize the context
         IF( cdname /= "roms" ) CALL iom_swap( "roms" )   ! return back to roms context
      ENDIF
      !
      END SUBROUTINE iom_context_finalize

#endif /* XIOS */





