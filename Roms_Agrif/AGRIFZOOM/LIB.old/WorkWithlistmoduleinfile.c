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
/*                       FillInlistmodule_1                                   */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void FillInlistmodule_1()
{
   listmodule *tmplist;
   
   
   if (firstpass == 1) 
   {   
      tmplist = (listmodule *)malloc(sizeof(listmodule));
      strcpy(tmplist->module,curmodulename);
      tmplist->AllocShouldMade = 0;
      tmplist->Alloc = 0;
      /*         */
      if ( !listmoduleinfile)
      {
         listmoduleinfile = tmplist;  
         tmplist->suiv = NULL;
      }
      else
      {
         tmplist->suiv = listmoduleinfile;
         listmoduleinfile = tmplist;      
      }
   }
}


/******************************************************************************/
/*                    AllocShouldMadeTo0InModule_1                            */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void AllocShouldMadeTo0InModule_1()
{
   listmodule *tmplist;
   
   
   if (firstpass == 1 && listmoduleinfile ) 
   {
      tmplist=listmoduleinfile;
      /* we should find the module in the listmoduleinfile                    */
      while ( strcasecmp(tmplist->module,curmodulename) ) tmplist=tmplist->suiv;
      /* and turn the flag to 0                                               */
      tmplist->AllocShouldMade = 0 ;
   }
}


/******************************************************************************/
/*                    AllocShouldMadeTo1InModule_1                            */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void AllocShouldMadeTo1InModule_1()
{
   listmodule *tmplist;
   
   
   if (firstpass == 1 && listmoduleinfile ) 
   {
      tmplist=listmoduleinfile;
      /* we should find the module in the listmoduleinfile                    */
      while ( strcasecmp(tmplist->module,curmodulename) ) tmplist=tmplist->suiv;
      /* and turn the flag to 0                                               */
      tmplist->AllocShouldMade = 1 ;
   }
}

/******************************************************************************/
/*                         AllocTo1InModule_1                                 */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void AllocTo1InModule_1()
{
   listmodule *tmplist;
   
   
   if (firstpass == 1 && listmoduleinfile ) 
   {
      tmplist=listmoduleinfile;
      /* we should find the module in the listmoduleinfile                    */
      while ( strcasecmp(tmplist->module,curmodulename) ) tmplist=tmplist->suiv;
      /* and turn the flag to 0                                               */
      tmplist->Alloc = 1 ;
   }
}

/******************************************************************************/
/*                           AllocShouldMadeInModule                          */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
int AllocShouldMadeInModule()
{
   listmodule *tmplist;
   
   
   if ( listmoduleinfile ) 
   {
      tmplist=listmoduleinfile;
      /* we should find the module in the listmoduleinfile                    */
      while ( strcasecmp(tmplist->module,curmodulename) ) tmplist=tmplist->suiv;
      /* and turn the flag to 0                                               */
      return tmplist->AllocShouldMade;
   }
   else
   {
      return 0;
   }
}

/******************************************************************************/
/*                                AllocInModule                               */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
int AllocInModule()
{
   listmodule *tmplist;
   
   
   if ( listmoduleinfile ) 
   {
      tmplist=listmoduleinfile;
      /* we should find the module in the listmoduleinfile                    */
      while ( strcasecmp(tmplist->module,curmodulename) ) tmplist=tmplist->suiv;
      /* and turn the flag to 0                                               */
      return tmplist->Alloc;
   }
   else
   {
      return 0;
   }
}
