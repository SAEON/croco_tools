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
/*                         ajoutvarofsubroutine_1                             */
/******************************************************************************/
/* Firstpass 1                                                                */
/* We should complete the listvarofsubroutine                                 */
/******************************************************************************/
/*                                                                            */
/*                                                                            */
/******************************************************************************/
void ajoutvarofsubroutine_1(listvar *listtoadd)
{

   if ( firstpass == 1 && VariableIsParameter == 0 && SaveDeclare == 0)
   {
      listduplicated = (listvar *)NULL;
      duplicatelistvar(listtoadd);
      varofsubroutineliste = AddListvarToListvar
                                        (listduplicated,varofsubroutineliste,1);
   }
}

/******************************************************************************/
/*                 CleanThelistvarofsubroutineliste                           */
/******************************************************************************/
/* This subroutine is to remove from the varofsubroutineliste                 */
/* all variables which are not located in the subroutine argument             */
/******************************************************************************/
void CleanThelistvarofsubroutineliste()
{
  listvar *newvar;
  listvar *newvarprec;
  listvar *tmpglobvar;
  int out;

  newvarprec = (listvar *)NULL;
  newvar = varofsubroutineliste;
  while ( newvar )
  {

     out = 0;
     tmpglobvar = listargsubroutine;
     while ( tmpglobvar && out == 0 )
     {
        if ( !strcasecmp(newvar->var->nomvar,tmpglobvar->var->nomvar) &&
             !strcasecmp(newvar->var->modulename,subroutinename) )
        {
           out = 1;
	}
	else
	{
           tmpglobvar = tmpglobvar->suiv;	
	}	
     }
     /*  if the variable has not be found we should remove it                 */
     if ( out == 0 && !strcasecmp(newvar->var->modulename,subroutinename) )
     {
        /* remove the variable in the  varofsubroutineliste                   */
	if ( newvar == varofsubroutineliste )
	{
	   varofsubroutineliste = varofsubroutineliste->suiv;
	   newvar = varofsubroutineliste;
	}
	else
	{
	   newvarprec->suiv = newvar->suiv;
	   newvar = newvarprec->suiv;
	}
     }
     else
     {
         newvarprec= newvar;
	 newvar = newvar->suiv;
     }
  }
}


/******************************************************************************/
/*             UpdatevarofsubroutinelisteWithcommonlist                       */
/******************************************************************************/
/*  This subroutines is used to add the variable defined in common in the     */
/*    varofsubroutineliste                                                    */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void UpdatevarofsubroutinelisteWithcommonlist()
{
   listvarcommon *parcours;
   listvar *parcours2;
   listvar *parcoursvar;
   listvar *parcoursvarprec;
   int out;
   
   parcoursvar = varofsubroutineliste;
   parcoursvarprec = (listvar *)NULL;
   while ( parcoursvar )
   {
      /* We should look in the commonlist if this variable is present         */
      parcours=commonlist;
      out=0;
      while( parcours && out == 0 )
      {
         if ( !strcasecmp(parcoursvar->var->nomvar,parcours->nomvar) &&
              !strcasecmp(parcoursvar->var->subroutinename,
                                           parcours->subroutinename)
            )
         {
            out = 1 ;
         }
         else
         {
            parcours = parcours->suiv;
         }
      }
      if ( out == 1 )
      {
         /* we found it                                                       */
         /* we should remove the variable from the varofsubroutineliste       */
         if ( parcoursvar == varofsubroutineliste )
         {
            varofsubroutineliste = varofsubroutineliste->suiv;
            parcoursvar = varofsubroutineliste ;
         }
         else
         {
            parcoursvarprec->suiv = parcoursvar->suiv;
            parcoursvar = parcoursvarprec->suiv;
         }
      }
      else
      {
         parcoursvarprec = parcoursvar;
         parcoursvar = parcoursvar->suiv;
      }
   }
   
   /* now we should remove all parameters                                     */
   parcoursvar = varofsubroutineliste;
   while ( parcoursvar )
   {
      /* We should look in the commonlist if this variable is present         */
      parcours2=parameterlist;
      out=0;
      while( parcours2 && out == 0 )
      {
         if ( !strcasecmp(parcoursvar->var->nomvar,parcours2->var->nomvar) &&
              !strcasecmp(parcoursvar->var->subroutinename,
                                           parcours2->var->subroutinename) 
            )
         {
            out = 1 ;
            /*                                                                */
         }
         else
         {
            parcours2 = parcours2->suiv;
         }
      }
      if ( out == 1 )
      {
         /* we did find it                                                    */
         /* we should remove the variable from the varofsubroutineliste       */
         if ( parcoursvar == varofsubroutineliste )
         {
            varofsubroutineliste = varofsubroutineliste->suiv;
            parcoursvar = varofsubroutineliste;
         }
         else
         {
            parcoursvarprec->suiv = parcoursvar->suiv;
            parcoursvar = parcoursvarprec->suiv;
         }
      }
      else
      {
         parcoursvarprec = parcoursvar;
         parcoursvar = parcoursvar->suiv;
      }
   }
}


/******************************************************************************/
/*                UpdatevarsubroutineWithvarofsubroutinelist_1                */
/******************************************************************************/
/*  This subroutines is used to add the variable defined in common in the     */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void UpdatevarsubroutineWithvarofsubroutinelist_1()
{
   listvar *parcours;
   listvar *parcours2;
   listvar *parcoursprec;
   int out;
   
   parcoursprec = (listvar * )NULL;
   parcours = varsubroutine;
   while ( parcours )
   {
      /* We should look in the varofsubroutineliste if this variable is       */
      /*    present                                                           */
      parcours2=varofsubroutineliste;
      out=0;
      while( parcours2 && out == 0 )
      {
         if ( !strcasecmp(parcours->var->nomvar,parcours2->var->nomvar) &&
              !strcasecmp(parcours->var->subroutinename,
                                            parcours2->var->modulename) 
            )
         {
            out = 1 ;
         }
         else
         {
            parcours2 = parcours2->suiv;
         }
      }
      if ( out == 1 )
      {
         /* we did not find it                                                */
         /* we should remove the variable from the varsubroutine              */
         if ( parcours ==  varsubroutine)
         {
            varsubroutine = varsubroutine->suiv;
            parcours = varsubroutine;
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
}
