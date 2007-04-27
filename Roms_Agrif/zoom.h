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
      real Zeta_west3(-1:7,-1:Mm+2+padd_E,0:NWEIGHT)
      common/zoom2D_ZetaW/Zeta_west3
      real DU_west3(-1:7,-1:Mm+2+padd_E,0:NWEIGHT)
      common/zoom2D_UW/DU_west3
      real DV_west3(-1:7,-1:Mm+2+padd_E,0:NWEIGHT)    
      common/zoom2D_VW/DV_west3

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
      real Zeta_east3(Lm-7:Lm+2+padd_X,-1:Mm+2+padd_E,0:NWEIGHT)
      common/zoom2D_ZetaE/Zeta_east3
      real DU_east3(Lm-7:Lm+2+padd_X,-1:Mm+2+padd_E,0:NWEIGHT)
      common/zoom2D_UE/DU_east3
      real DV_east3(Lm-7:Lm+2+padd_X,-1:Mm+2+padd_E,0:NWEIGHT)
      common/zoom2D_VE/DV_east3
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
      real Zeta_south3(-1:Lm+2+padd_X,-1:7,0:NWEIGHT)       
      common/zoom2D_ZetaS/Zeta_south3
      real DU_south3(-1:Lm+2+padd_X,-1:7,0:NWEIGHT)
      common/zoom2D_US/DU_south3
      real DV_south3(-1:Lm+2+padd_X,-1:7,0:NWEIGHT)
      common/zoom2D_VS/DV_south3
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
      real Zeta_north3(-1:Lm+2+padd_X,Mm-7:Mm+2+padd_E,0:NWEIGHT)
      common/zoom2D_ZetaN/Zeta_north3
      real DU_north3(-1:Lm+2+padd_X,Mm-7:Mm+2+padd_E,0:NWEIGHT)
      common/zoom2D_UN/DU_north3
      real DV_north3(-1:Lm+2+padd_X,Mm-7:Mm+2+padd_E,0:NWEIGHT)
      common/zoom2D_VN/DV_north3
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
      
      real dinterp(GLOBAL_2D_ARRAY)
      common/zoombc2D/dinterp
      
      logical Alreadyupdated(GLOBAL_2D_ARRAY,3)
      common/updateprestep/Alreadyupdated

       real usponge(GLOBAL_2D_ARRAY,N)
       real vsponge(GLOBAL_2D_ARRAY,N)
       real tsponge(GLOBAL_2D_ARRAY,N,NT)
       common/sponge_com/usponge, vsponge, tsponge

#  ifdef SOLVE3D
      real T_sponge_west(0:10,-1:Mm+2+padd_E,N,2,NT)
      common/zoom3D_sponge_TW/T_sponge_west
      real U_sponge_west(1:11,-1:Mm+2+padd_E,N,2)
      common/zoom3D_sponge_UW/U_sponge_west 
      real V_sponge_west(0:10,-1:Mm+2+padd_E,N,2)
      common/zoom3D_sponge_VW/V_sponge_west
      
      real T_sponge_east(LOCALLM-9:LOCALLM+1,-1:Mm+2+padd_E,N,2,NT)
      common/zoom3D_sponge_TE/T_sponge_east
      real U_sponge_east(LOCALLM-9:LOCALLM+1,-1:Mm+2+padd_E,N,2)
      common/zoom3D_sponge_UE/U_sponge_east 
      real V_sponge_east(LOCALLM-9:LOCALLM+1,-1:Mm+2+padd_E,N,2)
      common/zoom3D_sponge_VE/V_sponge_east
      
      real T_sponge_south(-1:Lm+2+padd_X,0:10,N,2,NT)
      common/zoom3D_sponge_TS/T_sponge_south
      real U_sponge_south(-1:Lm+2+padd_X,0:10,N,2)
      common/zoom3D_sponge_US/U_sponge_south 
      real V_sponge_south(-1:Lm+2+padd_X,1:11,N,2)
      common/zoom3D_sponge_VS/V_sponge_south
            
      real T_sponge_north(-1:Lm+2+padd_X,LOCALMM-9:LOCALMM+1,N,2,NT)     
      common/zoom3D_sponge_TN/T_sponge_north
      real U_sponge_north(-1:Lm+2+padd_X,LOCALMM-9:LOCALMM+1,N,2)
      common/zoom3D_sponge_UN/U_sponge_north 
      real V_sponge_north(-1:Lm+2+padd_X,LOCALMM-9:LOCALMM+1,N,2)
      common/zoom3D_sponge_VN/V_sponge_north
      
      integer TTimesponge, UTimesponge, VTimesponge
      common/zoom3D_sponge_times/TTimesponge, UTimesponge, VTimesponge
            
#  endif
            
#endif
