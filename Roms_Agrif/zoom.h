!
! $Id: zoom.h,v 1.4 2005/09/23 15:56:31 pmarches Exp $
!
#  ifdef MPI
#   define LOCALLM Lmmpi
#   define LOCALMM Mmmpi
#  else
#   define LOCALLM Lm
#   define LOCALMM Mm
#  endif      

#ifdef AGRIF
# ifdef AGRIF_OBC_WEST
      real Zeta_west1(0:1,-1:Mm+2+padd_E,2)
      real Zeta_west2(0:1,-1:Mm+2+padd_E,0:NWEIGHT)
      real Zeta_west3(-1:7,-1:Mm+2+padd_E,0:NWEIGHT)
      real Zeta_west4(0:1,-1:Mm+2+padd_E,0:NWEIGHT)
      common/zoom2D_ZetaW/Zeta_west1, Zeta_west2, Zeta_west3,
     & Zeta_west4
      real DU_west(-1:Mm+2+padd_E),
     &     DU_west1(-1:Mm+2+padd_E,2),
     &     DU_west2(-1:Mm+2+padd_E,0:NWEIGHT),
     &     DU_west3(-1:7,-1:Mm+2+padd_E,0:NWEIGHT),
     &     DU_west4(-1:Mm+2+padd_E,0:NWEIGHT)
      common/zoom2D_UW/DU_west, DU_west1,DU_west2, DU_west3,
     &         DU_west4
      real DV_west(-1:Mm+2+padd_E),
     &     DV_west1(-1:Mm+2+padd_E,2),
     &     DV_west2(-1:Mm+2+padd_E,0:NWEIGHT),
     &     DV_west3(-1:7,-1:Mm+2+padd_E,0:NWEIGHT),
     &     DV_west4(-1:Mm+2+padd_E,0:NWEIGHT)    
      common/zoom2D_VW/DV_west, DV_west1,DV_west2,DV_west3,
     & DV_west4

#  ifdef SOLVE3D
      real T_west(0:1,-1:Mm+2+padd_E,N,4,NT)
      common/zoom3D_TW/T_west
      real U_west(1:1,-1:Mm+2+padd_E,N,4)
      common/zoom3D_UW/U_west 
      real V_west(0:0,-1:Mm+2+padd_E,N,4)
      common/zoom3D_VW/V_west
#  endif
# endif
# ifdef AGRIF_OBC_EAST
      real Zeta_east1(LOCALLM:LOCALLM+1,-1:Mm+2+padd_E,2)
      real Zeta_east2(LOCALLM:LOCALLM+1,-1:Mm+2+padd_E,0:NWEIGHT)
      real Zeta_east3(Lm-7:Lm+2+padd_X,-1:Mm+2+padd_E,0:NWEIGHT)
      real Zeta_east4(LOCALLM:LOCALLM+1,-1:Mm+2+padd_E,0:NWEIGHT)
      common/zoom2D_ZetaE/Zeta_east1, Zeta_east2, Zeta_east3,
     & Zeta_east4
      real DU_east(-1:Mm+2+padd_E),
     &     DU_east1(-1:Mm+2+padd_E,2),
     &     DU_east2(-1:Mm+2+padd_E,0:NWEIGHT),
     &     DU_east3(Lm-7:Lm+2+padd_X,-1:Mm+2+padd_E,0:NWEIGHT),
     &     DU_east4(-1:Mm+2+padd_E,0:NWEIGHT)
      common/zoom2D_UE/DU_east, DU_east1, DU_east2, DU_east3,
     &       DU_east4
      real DV_east(-1:Mm+2+padd_E),
     &     DV_east1(-1:Mm+2+padd_E,2),
     &     DV_east2(-1:Mm+2+padd_E,0:NWEIGHT),
     &     DV_east3(Lm-7:Lm+2+padd_X,-1:Mm+2+padd_E,0:NWEIGHT),
     &     DV_east4(-1:Mm+2+padd_E,0:NWEIGHT)
      common/zoom2D_VE/DV_east, DV_east1, DV_east2, DV_east3,
     &      DV_east4
#  ifdef SOLVE3D
      real T_east(LOCALLM:LOCALLM+1,-1:Mm+2+padd_E,N,4,NT)
      common/zoom3D_TE/T_east
      real U_east(LOCALLM+1:LOCALLM+1,-1:Mm+2+padd_E,N,4)
      common/zoom3D_UE/U_east 
      real V_east(LOCALLM+1:LOCALLM+1,-1:Mm+2+padd_E,N,4)
      common/zoom3D_VE/V_east
#  endif
# endif
# ifdef AGRIF_OBC_SOUTH
      real Zeta_south1(-1:Lm+2+padd_X,0:1,2)
      real Zeta_south2(-1:Lm+2+padd_X,0:1,0:NWEIGHT)   
      real Zeta_south3(-1:Lm+2+padd_X,-1:7,0:NWEIGHT)  
      real Zeta_south4(-1:Lm+2+padd_X,0:1,0:NWEIGHT)         
      common/zoom2D_ZetaS/Zeta_south1, Zeta_south2, Zeta_south3,
     & Zeta_south4
      real DU_south(-1:Lm+2+padd_X),
     &     DU_south1(-1:Lm+2+padd_X,2),
     &     DU_south2(-1:Lm+2+padd_X,0:NWEIGHT),
     &     DU_south3(-1:Lm+2+padd_X,-1:7,0:NWEIGHT),
     &     DU_south4(-1:Lm+2+padd_X,0:NWEIGHT)  
      common/zoom2D_US/DU_south, DU_south1, DU_south2, DU_south3,
     &     DU_south4
      real DV_south(-1:Lm+2+padd_X),
     &     DV_south1(-1:Lm+2+padd_X,2),
     &     DV_south2(-1:Lm+2+padd_X,0:NWEIGHT),
     &     DV_south3(-1:Lm+2+padd_X,-1:7,0:NWEIGHT),
     &     DV_south4(-1:Lm+2+padd_X,0:NWEIGHT)   
      common/zoom2D_VS/DV_south, DV_south1, DV_south2, DV_south3,
     &     DV_south4
#  ifdef SOLVE3D
      real T_south(-1:Lm+2+padd_X,0:1,N,4,NT)
      common/zoom3D_TS/T_south
      real U_south(-1:Lm+2+padd_X,0:0,N,4)
      common/zoom3D_US/U_south 
      real V_south(-1:Lm+2+padd_X,1:1,N,4)
      common/zoom3D_VS/V_south
#  endif
# endif
# ifdef AGRIF_OBC_NORTH
      real Zeta_north1(-1:Lm+2+padd_X,LOCALMM:LOCALMM+1,2)
      real Zeta_north2(-1:Lm+2+padd_X,LOCALMM:LOCALMM+1,0:NWEIGHT)   
      real Zeta_north3(-1:Lm+2+padd_X,Mm-7:Mm+2+padd_E,0:NWEIGHT)   
      real Zeta_north4(-1:Lm+2+padd_X,LOCALMM:LOCALMM+1,0:NWEIGHT)
      common/zoom2D_ZetaN/Zeta_north1, Zeta_north2, Zeta_north3,
     &  Zeta_north4
      real DU_north(-1:Lm+2+padd_X),
     &     DU_north1(-1:Lm+2+padd_X,2),
     &     DU_north2(-1:Lm+2+padd_X,0:NWEIGHT),
     &     DU_north3(-1:Lm+2+padd_X,Mm-7:Mm+2+padd_E,0:NWEIGHT),
     &     DU_north4(-1:Lm+2+padd_X,0:NWEIGHT)
      common/zoom2D_UN/DU_north, DU_north1, DU_north2, DU_north3,
     &     DU_north4
      real DV_north(-1:Lm+2+padd_X),
     &     DV_north1(-1:Lm+2+padd_X,2),
     &     DV_north2(-1:Lm+2+padd_X,0:NWEIGHT),
     &     DV_north3(-1:Lm+2+padd_X,Mm-7:Mm+2+padd_E,0:NWEIGHT),
     &     DV_north4(-1:Lm+2+padd_X,0:NWEIGHT)
      common/zoom2D_VN/DV_north, DV_north1, DV_north2, DV_north3,
     &     DV_north4
#  ifdef SOLVE3D
      real T_north(-1:Lm+2+padd_X,LOCALMM:LOCALMM+1,N,4,NT)     
      common/zoom3D_TN/T_north
      real U_north(-1:Lm+2+padd_X,LOCALMM+1:LOCALMM+1,N,4)
      common/zoom3D_UN/U_north 
      real V_north(-1:Lm+2+padd_X,LOCALMM+1:LOCALMM+1,N,4)
      common/zoom3D_VN/V_north
#  endif
# endif
      integer Zetatimeindex
      common/zoom2D_ZetaT/Zetatimeindex
      integer U2DTimeindex
      common/zoom2D_UT/U2DTimeindex    
      integer V2DTimeindex
      common/zoom2D_VT/V2DTimeindex 
# ifdef SOLVE3D
      integer Ttimeindex
      common/zoom3D_TT/Ttimeindex
      integer Utimeindex
      common/zoom3D_UT/Utimeindex
      integer Vtimeindex
      common/zoom3D_VT/Vtimeindex
# endif

      real weight2(0:NWEIGHT,0:NWEIGHT)
      common/weighting/weight2

      real updateTprof(GLOBAL_2D_ARRAY,N,NT)
      common/updateTprofile/updateTprof
      integer indupdate
      real myvalues(3*7*(2*(Lm+3)+2*(Mm+3)),0:NWEIGHT)
      common/updatevalues/myvalues,indupdate

      integer nbcoarse
      common/nestingmanag/nbcoarse
      
      real myfx(GLOBAL_2D_ARRAY,N,NT)
      real myfy(GLOBAL_2D_ARRAY,N,NT)
      common/myfluxes/myfx,myfy
       
      real Zt_avg3(GLOBAL_2D_ARRAY,4)
      common/averagebaro/Zt_avg3
      
      logical Alreadyupdated(GLOBAL_2D_ARRAY,3)
      common/updateprestep/Alreadyupdated

       real usponge(GLOBAL_2D_ARRAY,N)
       real vsponge(GLOBAL_2D_ARRAY,N)
       real tsponge(GLOBAL_2D_ARRAY,N,NT)
       common/sponge_com/usponge, vsponge, tsponge

            
#endif
