/******************************************************************************/
/*                                                                            */
/*     CONV (converter) for Agrif (Adaptive Grid Refinement In Fortran)       */
/*                                                                            */
/* Copyright or   or Copr. Laurent Debreu (Laurent.Debreu@imag.fr)            */
/*                        Cyril Mazauric (Cyril_Mazauric@yahoo.fr)            */
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
/* version 1.7                                                                */
/******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "decl.h"
/******************************************************************************/
/*                          Add_Data_Var_1                                    */
/******************************************************************************/
/* This subroutine is used to add a record to List_Data_Var                   */
/******************************************************************************/
/*        _______     _______     _______     _______     _______             */
/*       +      +    +      +    +      +    +      +    +      +             */
/*       + NEW  +--->+ data +--->+ data +--->+ data +--->+  data+             */
/*       +______+    +______+    +______+    +______+    +______+             */
/*                                                                            */
/******************************************************************************/
void Add_Data_Var_1 (listvar **curlist,char *name,char *values)
{
  listvar *newvar;
  char ligne[LONG_C];

//  if ( firstpass == 1 )
//  {
     newvar=(listvar *)malloc(sizeof(listvar));
     newvar->var=(variable *)malloc(sizeof(variable));
     /*                                                                       */
     Init_Variable(newvar->var);
     /*                                                                       */
     if ( inmoduledeclare == 1 ) newvar->var->v_module=1;
     strcpy(newvar->var->v_nomvar,name);
     Save_Length(name,4);
     strcpy(newvar->var->v_subroutinename,subroutinename);
     Save_Length(subroutinename,11);
     strcpy(newvar->var->v_modulename,curmodulename);
     Save_Length(curmodulename,6);
     strcpy(newvar->var->v_commoninfile,mainfile);
     Save_Length(mainfile,10);
     if (strchr(values,',') && strncasecmp(values,"'",1))
            {
            sprintf(ligne,"(/%s/)",values);
            }
     else
       strcpy(ligne,values);
       
     strcpy(newvar->var->v_initialvalue,ligne);
     Save_Length(ligne,14);
     newvar->suiv = NULL;
     if ( ! (*curlist) )
     {
        *curlist  = newvar ;
     }
     else
     {
        newvar->suiv = *curlist;
        *curlist = newvar;
     }
//  }
}

void Add_Data_Var_Names_01 (listvar **curlist,listname *l1,listname *l2)
{
  listvar *newvar;
  listvar *tmpvar;
  listname *tmpvar1;
  listname *tmpvar2;  
  
  tmpvar1 = l1;
  tmpvar2 = l2;
  
  while (tmpvar1)
  {
     newvar=(listvar *)malloc(sizeof(listvar));
     newvar->var=(variable *)malloc(sizeof(variable));
     /*                                                                       */
     Init_Variable(newvar->var);
     /*                                                                       */
     if ( inmoduledeclare == 1 ) newvar->var->v_module=1;
     strcpy(newvar->var->v_nomvar,tmpvar1->n_name);
     Save_Length(tmpvar1->n_name,4);
     strcpy(newvar->var->v_subroutinename,subroutinename);
     Save_Length(subroutinename,11);
     strcpy(newvar->var->v_modulename,curmodulename);
     Save_Length(curmodulename,6);
     strcpy(newvar->var->v_commoninfile,mainfile);
     Save_Length(mainfile,10);
       
     strcpy(newvar->var->v_initialvalue,tmpvar2->n_name);
     Save_Length(tmpvar2->n_name,14);
     newvar->suiv = NULL;
     
     if ( ! (*curlist) )
     {
        *curlist  = newvar ;
     }
     else
     {
        tmpvar = *curlist;
        while (tmpvar->suiv)
          tmpvar=tmpvar->suiv;
        tmpvar->suiv = newvar;
     }
     
  tmpvar1 = tmpvar1->suiv;
  tmpvar2 = tmpvar2->suiv;  
  }
  return;


}
