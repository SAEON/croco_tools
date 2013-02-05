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
#ifdef SEDIMENT
/*
** Include file "sediment.h".
************************************* Meinte Blaas, John C. Warner ***
** Copyright (c) 2002/2004 Rutgers, UCLA                            **
************************************************* Hernan G. Arango ***
**                                                                  **
**  Bthk(NLAY)     User-defined initial bed layer thickness (m)     **
**  Bpor(NLAY)     User-defined initial porosity of bed layer       **
**  Bfr(NLAY,NST)  User-defined initial vol.fraction of layer, class *
**                                                                  **
**  Hrip      User-defined initial ripple height from file          **
**  Lrip      User-defined initial ripple length from file          **
**                                                                  **
**  bed_thick Sediment bed layer thickness (m)                      **
**  bed_poros Sediment bed layer porosity (void/total bed vol.)     **
**  bed_frac  Volume fraction of size class in bed layer            **
**                                                                  **
** Parameters for sediment model:                                   **
**                                                                  **
**  Csed     Sediment concentration (mg/l), used during analytical  **
**             initialization.                                      **
**  Erate    Surface erosion rate (kg/m2/s).                        **
**  Sd       Sediment grain diameter per size class (m).            **
**  Srho     Sediment grain density (kg/m3).                        **
**  Wsed     Particle settling velocity (m/s).                      **
**  tau_ce   Kinematic critical shear for erosion (m2/s2).          **
**  tau_cd   Kinematic critical shear for deposition (m2/s2).       **
**                                                                  **
** Sediment tracers identification indices:                         **
**                                                                  **
**  idsed    Cohesive and noncohesive sediment indices.             **
**  idmud    Cohesive sediment indices.                             **
**  isand    Noncohesive sediment indices.                          **
**                                                                  **
**  Stitle   Name of sediment.in inputfile                          **
**********************************************************************
*/  
      real Csed(NST), Erate(NST), Sd(NST), Srho(NST),
     &     Wsed(NST), tau_ce(NST), tau_cd(NST)
      common /ssediment/
     &        Csed, Erate, Sd,
     &        Srho, Wsed, tau_ce, tau_cd

      real Bthk(NLAY), Bpor(NLAY) 
      common /sediment_bedthk/ Bthk, Bpor
      
       
      real Bfr(NLAY,NST)
      common /sediment_bedfrc/ Bfr      
      
      real Hrip, Lrip
      common /sediment_bedrip/ Hrip, Lrip
      
      real bed_thick(GLOBAL_2D_ARRAY,NLAY), 
     &     bed_poros(GLOBAL_2D_ARRAY,NLAY),
     &     worksed_bed(GLOBAL_2D_ARRAY,NLAY)
      common /sediment_bed/ bed_thick, bed_poros,
     &                      worksed_bed
       
      real bed_frac(GLOBAL_2D_ARRAY,NLAY,NST),
     &     worksed_frac(GLOBAL_2D_ARRAY,NLAY)
      common /sediment_frac/ bed_frac, worksed_frac     
      
# ifdef AVERAGES
      real bed_frac_avg(GLOBAL_2D_ARRAY,NLAY,NST)
      common /sediment_frac_avg/ bed_frac_avg
# endif

      character*80 Stitle
      common /charseds/ Stitle

#endif /* SEDIMENT */

