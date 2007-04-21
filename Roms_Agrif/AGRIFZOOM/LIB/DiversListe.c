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
/*                           Addtolistvarcommon                               */
/******************************************************************************/
/*  This subroutines is used to add the variable defined in common in the     */
/*     commonlist                                                             */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void Addtolistvarcommon()
{
   listvarcommon *newvar;
   
   newvar = (listvarcommon *)malloc(sizeof(listvarcommon));
   strcpy(newvar->nomvar,commonvar);
   strcpy(newvar->commonname,commonblockname);
   strcpy(newvar->subroutinename,subroutinename);
   newvar->positioninblock= positioninblock;
   newvar->suiv = NULL;

   if ( !commonlist )
   {
      commonlist = newvar;
   }
   else
   {
      newvar->suiv = commonlist;
      commonlist = newvar;
   }
}

/******************************************************************************/
/*                           Addtolistnom                                     */
/******************************************************************************/
/* This subroutine is used to add a variable to the list                      */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
listnom *Addtolistnom(char *nom, listnom *listin)
{
   listnom *newnom;
   listnom *parcours;
   int out;

   newnom=(listnom *) malloc (sizeof (listnom));
   strcpy(newnom->nom,nom);
   newnom->suiv = NULL;

   if ( !listin ) listin = newnom;
   else
   {
      parcours = listin;
      out = 0 ;
      while ( parcours && out == 0 )
      {
         if ( !strcasecmp(parcours->nom,nom) ) out = 1 ;
         else parcours=parcours->suiv;
      }
      if ( out == 0 ) 
      {
          newnom->suiv = listin;
          listin = newnom;
      }
      else
      {
         free(newnom);
      }
   }
   return listin;
}

/******************************************************************************/
/*                           Addtolistname                                     */
/******************************************************************************/
/* This subroutine is used to add a        variable to the list               */
/******************************************************************************/
/*        _______     _______     _______     _______     _______             */
/*       +      +    +      +    +      +    +      +    +      +             */
/*       + NEW  +--->+ glob +--->+ glob +--->+ glob +--->+ glob +             */
/*       +______+    +______+    +______+    +______+    +______+             */
/*                                                                            */
/*                                                                            */
/*                                                                            */
/******************************************************************************/
listname *Addtolistname(char *nom,listname *input)
{
   listname *newnom;
   listname *parcours;
   int out;

   if ( !input )
   {
      newnom=(listname *) malloc (sizeof (listname));
      strcpy(newnom->name,nom);
      newnom->suiv = NULL;
      input = newnom;
   }
   else
   {
      parcours = input;
      out = 0 ;
      while ( parcours && out == 0 )
      {
         if ( !strcasecmp(parcours->name,nom) ) out = 1;
         else parcours=parcours->suiv;         
      }
      if ( out == 0 )
      {
         newnom=(listname *) malloc (sizeof (listname));
         strcpy(newnom->name,nom);
         newnom->suiv = input;
         input = newnom;
      }
   }
   return input;
}

/******************************************************************************/
/*                    ModuleIsDefineInInputFile                               */
/******************************************************************************/
/* This subroutine is used to know if the module is defined in the input file */
/******************************************************************************/
/*                                                                            */
/*                                                                            */
/******************************************************************************/
int ModuleIsDefineInInputFile(char *name)
{
   listnom *newnom;
   int out;
   
   out = 0;
   if ( listofmodules ) 
   {
      newnom = listofmodules;
      while( newnom && out == 0 )
      {
         if ( !strcasecmp(newnom->nom,name) ) out = 1 ;
         else newnom=newnom->suiv;
      }
   }
   return out;
}

/******************************************************************************/
/*                       AddNameToListNamelist_1                              */
/******************************************************************************/
/* This subroutine is used to add a listvar l at the end of a listvar         */
/* glob.                                                                      */
/*                                                                            */
/******************************************************************************/
/*        _______     _______     _______     _______     _______             */
/*       +      +    +      +    +      +    +      +    +      +             */
/*       + glob +--->+ glob +--->+ glob +--->+ glob +--->+  l   +             */
/*       +______+    +______+    +______+    +______+    +______+             */
/*                                                                            */
/******************************************************************************/
void AddNameToListNamelist_1(char * name)
{
   listnamelist *newvar;
   
   if ( firstpass == 1 )
   {
      if ( strcasecmp(name,"") )
      {
         newvar =(listnamelist*)malloc(sizeof(listnamelist));
         strcpy(newvar->name,name);
         newvar->suiv = listenamelist;
         listenamelist = newvar;
      }
   }
}

/******************************************************************************/
/*                      Addmoduletothelisttmp                                 */
/******************************************************************************/
/* This subroutine is used to add a record to a list of struct                */
/* listusemodule                                                              */
/******************************************************************************/
/*                                                                            */
/*       subroutine sub ... USE mod1 ===> insert in list                      */
/*        _______     _______     _______     _______     _______             */
/*       +      +    +      +    +      +    +      +    +      +             */
/*       + NEW  +--->+ list +--->+ list +--->+ list +--->+ list +             */
/*       +______+    +______+    +______+    +______+    +______+             */
/*                                                                            */
/*       list =  listofmoduletmp                                              */
/*                                                                            */
/******************************************************************************/
void Addmoduletothelisttmp(char *name)
{
  listusemodule *newmodule;
  listusemodule *parcours;
  int out;

  if ( !listofmoduletmp)
  {
    newmodule =(listusemodule *)malloc(sizeof(listusemodule));
    strcpy(newmodule->usemodule,name);
    strcpy(newmodule->cursubroutine,subroutinename);  
    newmodule->suiv = NULL;
    listofmoduletmp = newmodule ;
  }
  else
  {
    parcours = listofmoduletmp;
    out = 0;
    while( parcours && out == 0 )
    {
       if ( !strcasecmp(parcours->usemodule,name) ) out = 1;
       else parcours = parcours->suiv;
    }
    if ( out == 0 )
    {
       newmodule =(listusemodule *)malloc(sizeof(listusemodule));
       strcpy(newmodule->usemodule,name);
       strcpy(newmodule->cursubroutine,subroutinename);  
       newmodule->suiv = listofmoduletmp;
       listofmoduletmp = newmodule;
    }
  }
}

/******************************************************************************/
/*                   Add_ModuleTo_Modulelist_1                                */
/******************************************************************************/
/* This subroutine is used to add a        variable to the list               */
/******************************************************************************/
/*        _______     _______     _______     _______     _______             */
/*       +      +    +      +    +      +    +      +    +      +             */
/*       + NEW  +--->+ glob +--->+ glob +--->+ glob +--->+ glob +             */
/*       +______+    +______+    +______+    +______+    +______+             */
/*                                                                            */
/*                                                                            */
/*                                                                            */
/******************************************************************************/
void Add_ModuleTo_Modulelist_1(char *nom)
{
   listnom *newnom;

   if ( firstpass == 1 )
   {
      newnom=(listnom *) malloc (sizeof (listnom));
      strcpy(newnom->nom,nom);
      newnom->suiv = modulelist;
      modulelist = newnom;   
   }
}

/******************************************************************************/
/*                 completelistvarpointtovar_1                                */
/******************************************************************************/
/* Firstpass 1                                                                */
/* We should complete the listvarpointtovar                                   */ 
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void completelistvarpointtovar_1(char *namemodule,listcouple *couple)
{
   listvarpointtovar *pointtmp;
   
   if ( firstpass == 1 ) 
   {
      /* we should complete the Listofvarpointtovar                           */
      pointtmp=(listvarpointtovar *)malloc(sizeof(listvarpointtovar));
      strcpy(pointtmp->usemodule,namemodule);
      strcpy(pointtmp->cursubroutine,subroutinename);
      pointtmp->couple = couple;
      if ( Listofvarpointtovar )
      {
         pointtmp->suiv = Listofvarpointtovar;
         Listofvarpointtovar = pointtmp;
      }
      else
      {
         pointtmp->suiv = NULL;
         Listofvarpointtovar = pointtmp;    
      }
   }
}

/******************************************************************************/
/*                      Addincludetothelist_1                                 */
/******************************************************************************/
/* This subroutine is used to add a record to a list of struct                */
/*  listofincludebysubroutine                                                 */
/******************************************************************************/
/*                                                                            */
/*       subroutine sub ... USE mod1 ===> insert in list                      */
/*        _______     _______     _______     _______     _______             */
/*       +      +    +      +    +      +    +      +    +      +             */
/*       + NEW  +--->+ list +--->+ list +--->+ list +--->+ list +             */
/*       +______+    +______+    +______+    +______+    +______+             */
/*                                                                            */
/*       list =  listofmodulebysubroutine                                     */
/*                                                                            */
/******************************************************************************/
void Addincludetothelist_1(char *name)
{
  listusemodule *newinclude;

  if ( firstpass == 1 )
  {
  newinclude =(listusemodule *)malloc(sizeof(listusemodule));
  strcpy(newinclude->usemodule,name);
  strcpy(newinclude->cursubroutine,subroutinename);  
  newinclude->suiv = NULL;

  if ( !listofincludebysubroutine)
  {
     listofincludebysubroutine  = newinclude ;
  }
  else
  {
    newinclude->suiv = listofincludebysubroutine;
    listofincludebysubroutine = newinclude;
  }
  }
}


/******************************************************************************/
/*                        WriteIncludeDeclaration                             */
/******************************************************************************/
/* Firstpass 0                                                                */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void WriteIncludeDeclaration()
{
  listusemodule *newinclude;

  newinclude = listofincludebysubroutine;
  fprintf(fortranout,"\n");
  while ( newinclude )
  {
     if ( !strcasecmp(newinclude->cursubroutine,subroutinename) )
     {
        fprintf(fortranout,"      INCLUDE %s \n",newinclude->usemodule);
     }
     newinclude = newinclude ->suiv;  
  }
}
