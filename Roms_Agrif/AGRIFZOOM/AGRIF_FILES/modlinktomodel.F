C     AGRIF (Adaptive Grid Refinement In Fortran)
C
C     Copyright (C) 2003 Laurent Debreu (Laurent.Debreu@imag.fr)
C                        Christophe Vouland (Christophe.Vouland@imag.fr)    
C
C     This program is free software; you can redistribute it and/or modify
C     it under the terms of the GNU General Public License as published by
C     the Free Software Foundation; either version 2 of the License, or
C     (at your option) any later version.
C
C     This program is distributed in the hope that it will be useful,
C     but WITHOUT ANY WARRANTY; without even the implied warranty of
C     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C     GNU General Public License for more details.
C
C     You should have received a copy of the GNU General Public License
C     along with this program; if not, write to the Free Software
C     Foundation, Inc., 59 Temple Place -  Suite 330, Boston, MA 02111-1307, USA.
C
C
      Module Agrif_link
C
CCC   Description:
CCC   This module is used to link AGRIF files to the model
C
C     Modules used: 
C
c        external Agrif_InitValues
c        external Agrif_InitWorkspace
c        external Agrif_detect
C
        external Agrif_probdim_modtype_def
        external Agrif_clustering_def
C     Interface                       
      Interface
         Subroutine Agrif_Set_numberofcells(Agrif_Gr)
         Use Agrif_Types, Only : Agrif_grid
         TYPE(Agrif_Grid), Pointer :: Agrif_Gr   ! Pointer on the current grid  
         End Subroutine Agrif_Set_numberofcells
      End interface
      Interface
         Subroutine Agrif_Get_numberofcells(Agrif_Gr)
         Use Agrif_Types, Only : Agrif_grid
         TYPE(Agrif_Grid), Pointer :: Agrif_Gr   ! Pointer on the current grid  
         End Subroutine Agrif_Get_numberofcells
      End interface
      Interface
         Subroutine Agrif_Allocationcalls(Agrif_Gr)
         Use Agrif_Types, Only : Agrif_grid
         TYPE(Agrif_Grid), Pointer :: Agrif_Gr   ! Pointer on the current grid  
         End Subroutine Agrif_Allocationcalls
      End interface
C   
      End Module Agrif_link
C     **************************************************************************
CCC   Function Agrif_parent
C        modify by conv. To use : un_parent = Agrif_Parent(un)
C     **************************************************************************
CCC   Function Agrif_Get_Coarse_Grid
C        modify by conv. To use : un_Mygrid = Agrif_Get_Coarse_grid(un)
C     **************************************************************************
CCC   Function Agrif_Rhox
C        modify by conv. To use : var = Agrif_Rhox()
C                    REAL(Agrif_Curgrid % spaceref(1))
C     **************************************************************************
CCC   Function Agrif_Parent_Rhox
C        modify by conv. To use : var = Agrif_Parent_Rhox()
C                    REAL(Agrif_Curgrid % parent % spaceref(1))
C     **************************************************************************
CCC   Function Agrif_Irhox
C        modify by conv. To use : var = Agrif_Parent_IRhox()
C                    Agrif_Curgrid % spaceref(1)
C     **************************************************************************
CCC   Function Agrif_Parent_Irhox
C        modify by conv. To use : var = Agrif_Parent_IRhox()
C                    Agrif_Curgrid % parent % spaceref(1)
C     **************************************************************************
CCC   Function Agrif_Rhoy
C        modify by conv. To use : var = Agrif_Rhoy()
C                    REAL(Agrif_Curgrid % spaceref(2))
C     **************************************************************************
CCC   Function Agrif_Parent_Rhoy
C        modify by conv. To use : var = Agrif_Parent_Rhoy()
C                    REAL(Agrif_Curgrid % parent % spaceref(2))
C     **************************************************************************
CCC   Function Agrif_Irhoy
C        modify by conv. To use : var = Agrif_Parent_IRhoy()
C                    Agrif_Curgrid % spaceref(2)
C     **************************************************************************
CCC   Function Agrif_Parent_Irhoy
C        modify by conv. To use : var = Agrif_Parent_IRhoy()
C                    Agrif_Curgrid % parent % spaceref(2)
C     **************************************************************************
CCC   Function Agrif_Rhoz
C        modify by conv. To use : var = Agrif_Rhoz()
C                    REAL(Agrif_Curgrid % spaceref(3))
C     **************************************************************************
CCC   Function Agrif_Parent_Rhoz
C        modify by conv. To use : var = Agrif_Parent_Rhoz()
C                    REAL(Agrif_Curgrid % parent % spaceref(3))
C     **************************************************************************
CCC   Function Agrif_Irhoz
C        modify by conv. To use : var = Agrif_Parent_IRhoz()
C                    Agrif_Curgrid % spaceref(3)
C     **************************************************************************
CCC   Function Agrif_Parent_Irhoz
C        modify by conv. To use : var = Agrif_Parent_IRhoz()
C                    Agrif_Curgrid % parent % spaceref(3)
C     **************************************************************************
CCC   Function Agrif_NearCommonBorderX
C        modify by conv. To use : var = Agrif_NearCommonBorderX()
C                       AGRIF_CURGRID % NearRootBorder(1)
C     **************************************************************************
CCC   Function Agrif_NearCommonBorderY
C        modify by conv. To use : var = Agrif_NearCommonBorderY()
C                       AGRIF_CURGRID % NearRootBorder(2)
C     **************************************************************************
CCC   Function Agrif_NearCommonBorderZ
C        modify by conv. To use : var = Agrif_NearCommonBorderZ()
C                       AGRIF_CURGRID % NearRootBorder(3)
C     **************************************************************************
CCC   Function Agrif_DistantCommonBorderX
C        modify by conv. To use : var = Agrif_DistantCommonBorderX()
C                       AGRIF_CURGRID % DistantRootBorder(1)
C     **************************************************************************
CCC   Function Agrif_DistantCommonBorderY
C        modify by conv. To use : var = Agrif_DistantCommonBorderY()
C                       AGRIF_CURGRID % DistantRootBorder(2)
C     **************************************************************************
CCC   Function Agrif_DistantCommonBorderZ
C        modify by conv. To use : var = Agrif_DistantCommonBorderZ()
C                       AGRIF_CURGRID % DistantRootBorder(3)
C     **************************************************************************
CCC   Function Agrif_Nb_Step
C        modify by conv. To use : var = Agrif_Nb_Step()
C                          AGRIF_CURGRID % ngridstep
C     **************************************************************************
CCC   Function Agrif_Nb_Fine_Grids
C        modify by conv. To use : var = Agrif_Nb_Fine_Grids()
C                         Agrif_nbfixedgrids 
C     **************************************************************************
CCC   Function Agrif_Ix
C        modify by conv. To use : var = Agrif_Ix()
C                         Agrif_CURGRID % ix(1) 
C     **************************************************************************
CCC   Function Agrif_Parent_Ix
C        modify by conv. To use : var = Agrif_Parent_Ix()
C                        Agrif_CURGRID % parent % ix(1) 
C     **************************************************************************
CCC   Function Agrif_Iy
C        modify by conv. To use : var = Agrif_Iy()
C                        AGRIF_CURGRID % ix(2) 
C     **************************************************************************
CCC   Function Agrif_Parent_Iy
C        modify by conv. To use : var = Agrif_Parent_Iy()
C                       Agrif_CURGRID % parent % ix(2) 
C     **************************************************************************
CCC   Function Agrif_Iz
C        modify by conv. To use : var = Agrif_Iz()
C                      AGRIF_CURGRID % ix(3) 
C     **************************************************************************
CCC   Function Agrif_Parent_Iz
C        modify by conv. To use : var = Agrif_Parent_Iz()
C                     Agrif_CURGRID % parent % ix(3) 
C     **************************************************************************
CCC   Function Agrif_Get_grid_id
C        modify by conv. To use : var = Agrif_Get_grid_id()
C                    Agrif_CURGRID % grid_id
C     **************************************************************************
CCC   Function Agrif_Get_parent_id
C        modify by conv. To use : var = Agrif_Get_parent_id()
C                    Agrif_CURGRID % parent % grid_id
C     **************************************************************************
