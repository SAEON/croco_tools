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
/*                    RecordUseModulesVariables                               */
/******************************************************************************/
/******************************************************************************/
void RecordUseModulesVariables()
{
  listusemodule *tmplistmodule;

  /* we should record all variables defined in modules used in this file      */
  if ( listofmodulebysubroutine )
  {
     listofvarofusemodulecreated = 1;
     tmplistmodule = listofmodulebysubroutine;
     while ( tmplistmodule )
     {
        if ( tmplistmodule->firstuse == 1 )
        {
           /* check if the file .depend<usemodule> exist                      */
           globalvarofusefile = Readthedependfile
                                  (tmplistmodule->usemodule,globalvarofusefile);

           tmpparameterlocallist = ReaddependParameterList
                               (tmplistmodule->usemodule,tmpparameterlocallist);

        }

        tmplistmodule = tmplistmodule->suiv;
     }
  }
}

/******************************************************************************/
/*                RecordUseModulesUseModulesVariables                         */
/******************************************************************************/
/******************************************************************************/
void  RecordUseModulesUseModulesVariables()
{
  listusemodule *tmplistmodule;
  listusemodule *tmplistmodule1;

  /* we should record all variables defined in modules used in this file      */
  if ( listofmodulebysubroutine )
  {
     /* and we should read the .depend of the module used by the module used  */
     tmplistmodule = listofmodulebysubroutine;
     while ( tmplistmodule )
     {
        Readthedependlistofmoduleused(tmplistmodule->usemodule);
        while( tmpuselocallist )
        {
           Addmoduletothelisttmp(tmpuselocallist->usemodule);
	   tmplistmodule1 = tmpuselocallist->suiv;
	   free(tmpuselocallist);
           tmpuselocallist = tmplistmodule1;
        }
        tmplistmodule = tmplistmodule->suiv;
     }
           
     tmplistmodule = listofmoduletmp;
     while ( tmplistmodule )
     {
        Readthedependlistofmoduleused(tmplistmodule->usemodule);
        while( tmpuselocallist )
        {
           Addmoduletothelisttmp(tmpuselocallist->usemodule);
           tmplistmodule1 = tmpuselocallist->suiv;
           free(tmpuselocallist);
           tmpuselocallist = tmplistmodule1;
        }
	tmplistmodule = tmplistmodule->suiv;
     }



     tmplistmodule = listofmoduletmp;
     while ( tmplistmodule )
     {
        /* check if the file .depend<usemodule> exist                         */
        globalvarofusefile2 = Readthedependfile
                                 (tmplistmodule->usemodule,globalvarofusefile2);

        tmpparameterlocallist2 = ReaddependParameterList
                              (tmplistmodule->usemodule,tmpparameterlocallist2);
        
        tmplistmodule = tmplistmodule->suiv;
     }
  }
}

/******************************************************************************/
/*                      Addmoduletothelist_1                                  */
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
/*       list =  listofmodulebysubroutine                                     */
/*                                                                            */
/******************************************************************************/
void Addmoduletothelist_1(char *name)
{
  listusemodule *newmodule;
  listusemodule *parcours;
  int out;

  if ( firstpass == 1 )
  {
     newmodule =(listusemodule *)malloc(sizeof(listusemodule));
     strcpy(newmodule->usemodule,name);
     strcpy(newmodule->charusemodule,charusemodule);
     strcpy(newmodule->cursubroutine,subroutinename);  
     newmodule->firstuse = 1 ;  
     newmodule->suiv = NULL;

     if ( !listofmodulebysubroutine)
     {
         listofmodulebysubroutine = newmodule ;
     }
     else
     {
    parcours = listofmodulebysubroutine;
    while ( parcours && newmodule->firstuse == 1 )
    {
       if ( !strcasecmp(name,parcours->usemodule) ) 
       {
          newmodule->firstuse = 0 ;
       }
       parcours=parcours->suiv;
    }
    /* we can not add the same module twice for the same subroutine           */
    parcours = listofmodulebysubroutine;
    out = 0 ;
    while ( parcours && out == 0 )
    {
       if ( !strcasecmp(name,parcours->usemodule) &&
            !strcasecmp(subroutinename,parcours->cursubroutine)
           )
       {
          out = 1 ;
          free(newmodule);
       }
       else parcours=parcours->suiv;
    }
    if ( out == 0 )
    {
       newmodule->suiv = listofmodulebysubroutine;
       listofmodulebysubroutine = newmodule;
    }
  }
  }
}


/******************************************************************************/
/*                        Addmoduletothelist                                  */
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
/*       list =  listofmodulebysubroutine                                     */
/*                                                                            */
/******************************************************************************/
void Addmoduletothelist(char *name)
{
  listusemodule *newmodule;
  listusemodule *parcours;
  int out;

     newmodule =(listusemodule *)malloc(sizeof(listusemodule));
     strcpy(newmodule->usemodule,name);
     strcpy(newmodule->charusemodule,charusemodule);
     strcpy(newmodule->cursubroutine,subroutinename);  
     newmodule->firstuse = 1 ;  
     newmodule->suiv = NULL;

     if ( !listofmodulebysubroutine)
     {
         listofmodulebysubroutine = newmodule ;
     }
     else
     {
    parcours = listofmodulebysubroutine;
    while ( parcours && newmodule->firstuse == 1 )
    {
       if ( !strcasecmp(name,parcours->usemodule) ) 
       {
          newmodule->firstuse = 0 ;
       }
       parcours=parcours->suiv;
    }
    /* we can not add the same module twice for the same subroutine           */
    parcours = listofmodulebysubroutine;
    out = 0 ;
    while ( parcours && out == 0 )
    {
       if ( !strcasecmp(name,parcours->usemodule) &&
            !strcasecmp(subroutinename,parcours->cursubroutine)
           )
       {
          out = 1 ;
          free(newmodule);
       }
       else parcours=parcours->suiv;
    }
    if ( out == 0 )
    {
       newmodule->suiv = listofmodulebysubroutine;
       listofmodulebysubroutine = newmodule;
    }
  }
}


/******************************************************************************/
/*                        WriteUsemoduleDeclaration                           */
/******************************************************************************/
/* Firstpass 0                                                                */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void WriteUsemoduleDeclaration()
{
  listusemodule *newmodule;

  newmodule = listofmodulebysubroutine;
  fprintf(fortranout,"\n");
  while ( newmodule )
  {
     if ( !strcasecmp(newmodule->cursubroutine,subroutinename) )
     {
        if ( strcasecmp(newmodule->charusemodule,"Agrif_Util") ||
            adduseagrifutil != 1 ) fprintf(fortranout,"      USE %s \n"
                                                     ,newmodule->charusemodule);
     }
        newmodule = newmodule ->suiv;  
  }
}
