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
/*                          COMPLETEDATALIST                                  */
/******************************************************************************/
/* This subroutine is used to add a record to listdatavariable                */
/******************************************************************************/
/*        _______     _______     _______     _______     _______             */
/*       +      +    +      +    +      +    +      +    +      +             */
/*       + NEW  +--->+ data +--->+ data +--->+ data +--->+  data+             */
/*       +______+    +______+    +______+    +______+    +______+             */
/*                                                                            */
/******************************************************************************/
void CompleteDataList (char *name,char *values)
{
  listvar *newvar;
  
  newvar=(listvar *)malloc(sizeof(listvar));
  newvar->var=(variable *)malloc(sizeof(variable));
  strcpy(newvar->var->nomvar,name);  
  strcpy(newvar->var->initialvalue,values); 
  newvar->suiv = NULL; 

  if ( !listdatavariable )
  {
     listdatavariable  = newvar ;
  }
  else
  {
     newvar->suiv = listdatavariable;
     listdatavariable = newvar;
  }
}


/******************************************************************************/
/*                    COMPLETEGLOBLISTEWITHDATALIST_1                         */
/******************************************************************************/
/* This subroutine is used to complete the variable initialisation            */
/* in the globliste with the listdatavariable                                 */
/******************************************************************************/
/*        _______     _______     _______     _______     _______             */
/*       +      +    +      +    +      +    +      +    +      +             */
/*       + data +--->+ data +--->+ data +--->+ data +--->+ data +             */
/*       +______+    +______+    +______+    +______+    +______+             */
/*                                  ||                                        */
/*                                  ||                                        */
/*                                  ||                                        */
/*                                  ||                                        */
/*                            initialvalue                                    */
/*                                  ||                                        */
/*                                  ||                                        */
/*        _______     _______     __\/___     _______     _______             */
/*       +      +    +      +    +      +    +      +    +      +             */
/*       + glob +--->+ glob +--->+ glob +--->+ glob +--->+ glob +             */
/*       +______+    +______+    +______+    +______+    +______+             */
/*                                                                            */
/******************************************************************************/
void CompleteGlobListeWithDatalist_1()
{
  listvar *newvar;
  listvar *globlistetmp;
  int out;

  if ( firstpass == 1 )
  {  
  /* We are looking for each variable of the listdatavariable where           */
  /* are they located in the globliste                                        */
  newvar = listdatavariable;
  while ( newvar )
  {
     globlistetmp = globliste;
     out = 0 ;
     while( globlistetmp && out == 0 )
     {
        if ( !strcasecmp(newvar->var->nomvar,globlistetmp->var->nomvar) )
	{
           out = 1;
	   if ( strcasecmp(globlistetmp->var->initialvalue,"") )
	   {
              printf("The variable %s has ever a initial value \n"
                                                         , newvar->var->nomvar);
              printf("Error in the CompleteGlobListeWithDatalist routine \n");
              exit(0);	      
	   }
	   else
	   {
              strcpy(globlistetmp->var->initialvalue,newvar->var->initialvalue);
	   }
	}
	else
	{
        globlistetmp = globlistetmp->suiv;
	}
     }
     if ( !globlistetmp )
     {
        printf("The variable %s has not bee found in the globliste \n"
                                                         , newvar->var->nomvar);
        printf("Error in the CompleteGlobListeWithDatalist routine \n");
	exit(0);
     }
     newvar = newvar->suiv;
  }
  }
}

