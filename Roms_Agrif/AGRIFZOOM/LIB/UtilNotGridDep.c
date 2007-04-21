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
/* version 1.3                                                                */
/******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "decl.h"

/******************************************************************************/
/*                        ajoutenotgriddep                                    */
/******************************************************************************/
/* This subroutine is used to add a record into listenotgriddepend            */
/*    This variable is add only if it is not present in the list              */
/*    This variable is add at the end of the list                             */
/******************************************************************************/
/*        _______     _______     _______     _______     _______             */
/*       + not  +    + not  +    +  not +    +  not +    +      +             */
/*       + grid +--->+ grid +--->+ grid +--->+ grid +--->+ NEW  +             */
/*       +______+    +______+    +______+    +______+    +______+             */
/*                                                                            */
/******************************************************************************/
void ajoutenotgriddep (char *name)
{
   /*                                                                         */
   variabletempo =(variable *)NULL;
   listvartempo=(listvar *)NULL;
   /* create the record                                                       */
   variabletempo=createvar(name,NULL);
   /* look in the listenotgriddepend if this variable exist                   */
   listvartempo = listenotgriddepend;
   while (listvartempo)
   {
     if (!strcasecmp(listvartempo->var->nomvar,name))
     {
        /* if this variable exist -> exit of the program                      */
        printf(" The variable %s\n",name);
        printf(" has been declared twice \n");
        printf(" as a non grid dependent variable \n");
        exit(1);
     }
      listvartempo= listvartempo->suiv;
   }
   /* if variable does not exist, we add it                                   */
   listvartempo = insertvar(listenotgriddepend,variabletempo);
   listenotgriddepend = listvartempo;
}

/******************************************************************************/
/*                           RemoveNotgriddependFromGlobliste                 */
/******************************************************************************/
/* This subroutine is used to remove from the globliste all variables         */
/* which are not grid dependent                                               */
/******************************************************************************/
/*        _______     _______     _______     _______     _______             */
/*       +      +    +      +    +      +    +      +    +      +             */
/*       + glob +--->+ glob +--->+ glob +--->+ glob +--->+ glob +             */
/*       +______+    +______+    +______+    +______+    +______+             */
/*                                          not grid                          */
/*                                            remove                          */
/*                                                                            */
/******************************************************************************/
void RemoveNotgriddependFromGlobliste()
{
   listvar  *newvarglobliste;
   listvar  *newvargloblistePrec;
   listvar  *newvarnotgriddepend;
   listvar  *newvarnotgriddependPrec;
   int      Out;
   int      Out1;
   
   if ( listenotgriddepend )
   {
      newvargloblistePrec = (listvar *)NULL;
      newvarnotgriddependPrec = (listvar *)NULL;
      /* Read of the globliste                                                */
      newvarglobliste = globliste;
      Out1 = 1 ;
      while ( newvarglobliste && Out1 == 1 )
      {
         newvarnotgriddepend = listenotgriddepend;
	 Out = 1 ;
         /* Read of the notgriddepend                                         */
	 while ( newvarnotgriddepend && Out == 1 )
	 {
            /* If the variable is in the notgriddepend list                   */
	    if ( ! strcasecmp(newvarglobliste->var->nomvar,
                                             newvarnotgriddepend->var->nomvar) )
	    {
               /* We should go out of the loop so Out = 0                     */
               Out = 0 ;
	       /* We remove the variable from the globliste                   */
	       /* If we are at the beginning of the globliste                 */
               if ( newvarglobliste == globliste )
               {
                  globliste = globliste->suiv;		     
               }
               else
               {
                  newvargloblistePrec->suiv = newvarglobliste->suiv;
                  newvarglobliste = newvargloblistePrec;
               }
               /* We remove the variable from the notgriddepend list          */
              if ( newvarnotgriddepend == listenotgriddepend )
              {
                 listenotgriddepend = listenotgriddepend->suiv;		     
              }
              else
              {
                 newvarnotgriddependPrec->suiv = newvarnotgriddepend->suiv;
                 newvarnotgriddepend = newvarnotgriddependPrec;
               }
	    }
            /* If the variable is not in the notgriddepend list               */
	    else
	    {
	       newvarnotgriddependPrec = newvarnotgriddepend;
	       newvarnotgriddepend = newvarnotgriddepend->suiv;
	    }
	 }
	 /* If the notgriddepend list is empty we go out of this subroutine   */
	 if ( !listenotgriddepend )
	 {
	    Out1 = 0;
	 }
	 newvargloblistePrec = newvarglobliste;
	 newvarglobliste = newvarglobliste->suiv;
      }
   }
}

/******************************************************************************/
/*                      VarIsNonGridDepend                                    */
/******************************************************************************/
/* This subroutine is used to know if a variable has been declared as non     */
/* grid dependent                                                             */
/******************************************************************************/
/*                                                                            */
/*  notgriddepend variable;    ----------->  VarIsNonGridDepend = 1           */
/*                                                                            */
/*                                                                            */
/******************************************************************************/
int VarIsNonGridDepend(char *name)
{
   listvar *newvar;
   int out;

   newvar = listenotgriddepend;
   out=0;
   while (newvar && out == 0 )
   {
      if ( !strcasecmp(newvar->var->nomvar,name) ) out = 1;
      else newvar = newvar->suiv;
   }
   return out;
}


/******************************************************************************/
/*                       NonGridDepDeclaration_0                              */
/******************************************************************************/
/* Firstpass 0                                                                */
/* We should modify this declaration in the file fortranout                   */
/******************************************************************************/
void NonGridDepDeclaration_0(listvar *listtomodify)
{
   if ( (aftercontainsdeclare == 0 && VariableIsParameter == 0) )
   {
      if (firstpass == 0)
      {
         pos_end = setposcur();
         RemoveWordSET_0(fortranout,pos_cur,
                               pos_end-pos_cur);
         /* Modifications of declarations */
         NonGridDepDeclaration(listtomodify);
      }
   }
}
