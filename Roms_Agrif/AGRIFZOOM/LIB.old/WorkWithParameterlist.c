/******************************************************************************/
/*                                                                            */
/*     CONV (converter) for Agrif (Adaptive Grid Refinement In Fortran)       */
/*                                                                            */
/* Copyright or © or Copr. Laurent Debreu (Laurent.Debreu@imag.fr)            */
/*                        Cyril Mazauric (Cyril.Mazauric@imag.fr)             */
/* This software is governed by the CeCILL-C license under French law and     */
/* abiding by the rules of distribution of free software.  You can  use,      */
/* modify and/ or redistribute the software under the terms of the CeCILL-C   */
/* license as circulated by CEA, CNRS and INRIA at the following URL          */
/* "http://www.cecill.info".                                                  */
/*                                                                            */
/* As a counterpart to the access to the source code and  rights to copy,     */
/* modify and redistribute granted by the license, users are provided only    */
/* with a limited warranty  and the software's author,  the holder of the     */
/* economic rights,  and the successive licensors  have only  limited         */
/* liability.                                                                 */
/*                                                                            */
/* In this respect, the user's attention is drawn to the risks associated     */
/* with loading,  using,  modifying and/or developing or reproducing the      */
/* software by the user in light of its specific status of free software,     */
/* that may mean  that it is complicated to manipulate,  and  that  also      */
/* therefore means  that it is reserved for developers  and  experienced      */
/* professionals having in-depth computer knowledge. Users are therefore      */
/* encouraged to load and test the software's suitability as regards their    */
/* requirements in conditions enabling the security of their systems and/or   */
/* data to be ensured and,  more generally, to use and operate it in the      */
/* same conditions as regards security.                                       */
/*                                                                            */
/* The fact that you are presently reading this means that you have had       */
/* knowledge of the CeCILL-C license and that you accept its terms.           */
/******************************************************************************/
/* version 1.0                                                                */
/******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "decl.h"

/******************************************************************************/
/*                       AddvartoParamlist_1                                  */
/******************************************************************************/
/*  This subroutines is used to add the variable defined in common in the     */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void AddvartoParamlist_1(listvar *listin)
{
   listvar *parcours;
   
   if ( firstpass == 1 )
   {
      if ( !parameterlist )
      {
         parameterlist = listin;
      }
      else
      {
         parcours = parameterlist;
         while (parcours->suiv) parcours=parcours->suiv;
      
         parcours->suiv = listin;
      }
   }
}

/******************************************************************************/
/*                   UpdateparameterlistWithlistvarindoloop_1                 */
/******************************************************************************/
/*  This subroutines is used to add the variable defined in common in the     */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void UpdateparameterlistWithlistvarindoloop_1()
{
   listvar *parcours;
   listvar *parcours2;
   listvar *parcours3;
   listvar *parcoursprec;
   int out;
   
   parcoursprec = (listvar * )NULL;
   parcours = parameterlist;
   while ( parcours )
   {
   if ( !strcasecmp(parcours->var->subroutinename,subroutinename) )
   { 
      /* We should look in the listvarindoloop if this variable is present    */
      parcours2=listvarindoloop;
      out=0;
      while( parcours2 && out == 0 )
      {
         if ( !strcasecmp(parcours->var->nomvar,parcours2->var->nomvar) &&
              !strcasecmp(parcours->var->subroutinename,
                                            parcours2->var->modulename) 
            )
         {
            parcours->var->VariableIsParameter = 1;
            /* we should find in the globliste the type of this variable      */
            parcours3 = globliste;
            while ( parcours3 && out == 0 )
            {
               if ( !strcasecmp(parcours3->var->nomvar,parcours->var->nomvar) )
               {
                  out = 1 ;
                  strcpy(parcours->var->typevar,parcours3->var->typevar);
               }
               else
               {
                  parcours3 = parcours3->suiv;
               }
            }            
            out = 1 ;
         }
         else
         {
            parcours2 = parcours2->suiv;
         }
      }
      if ( out == 0 )
      {
         /* we did not find it                                                */
         /* we should remove the variable from the parameterlist              */
         if ( parcours ==  parameterlist)
         {
            parameterlist = parameterlist->suiv;
            parcours = parameterlist;
         }
         else
         {
            parcoursprec->suiv = parcours->suiv;
            parcours = parcoursprec->suiv;
         }
      }
      else
      {
         parcoursprec = parcours;
         parcours = parcours->suiv;
      }
   }
   else
   {
      parcoursprec = parcours;
      parcours = parcours->suiv;
   }
   }
}
