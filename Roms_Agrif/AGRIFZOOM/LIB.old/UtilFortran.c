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
/*                            initdimprob                                     */
/******************************************************************************/
/* This subroutine is used to initialized grid dimension variable             */
/******************************************************************************/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/******************************************************************************/
void initdimprob(int dimprobmod, char * nx, char * ny,char* nz)
{
  dimprob = dimprobmod;

  strcpy(nbmaillesX,nx);
  strcpy(nbmaillesY,ny);
  strcpy(nbmaillesZ,nz);
}

/******************************************************************************/
/*                      Variableshouldberemove                                */
/******************************************************************************/
/* Firstpass 0                                                                */
/******************************************************************************/
/*                                                                            */
/*               Agrif_<toto>(variable) ====>     Agrif_<toto>(variable)      */
/*                                                                            */
/******************************************************************************/
int Variableshouldberemove(char *nom)
{

   int remove;
   
   remove = 0 ; 
   
   if ( remove == 0 && !strcasecmp(nom,"RESHAPE") ) remove = 1 ;  
   if ( remove == 0 && Agrif_in_Tok_NAME(nom) == 1 ) remove = 1 ;  

   return remove;   
}

/******************************************************************************/
/*                          variableisglobal                                  */
/******************************************************************************/
/* This subroutine is to know if a variable is global                         */
/******************************************************************************/
int variableisglobal(listvar *curvar, listvar *listin)
{
  int Globalite;
  listvar *newvar;


  Globalite = 0;
  newvar = listin;
  while ( newvar && Globalite == 0 )
  {
     if ( !strcasecmp(newvar->var->nomvar,curvar->var->nomvar) )
     {
        Globalite = 1;
        /* Now we should give the definition of the variable in the           */
        /* table listvarindoloop                                              */
        strcpy(curvar->var->typevar,newvar->var->typevar);
        strcpy(curvar->var->dimchar,newvar->var->dimchar);
        curvar->var->nbdim = newvar->var->nbdim;
        curvar->var->dimensiongiven = newvar->var->dimensiongiven;
        curvar->var->allocatable = newvar->var->allocatable;
        curvar->var->pointerdeclare = newvar->var->pointerdeclare;
        curvar->var->indicetabvars = newvar->var->indicetabvars;
        strcpy(curvar->var->precision,newvar->var->precision);
        strcpy(curvar->var->readedlistdimension,
                                              newvar->var->readedlistdimension);
     }
     else
     {
         newvar = newvar->suiv;
     }
  }

  return Globalite ;
}

/******************************************************************************/
/*                      variableisglobalinmodule                                      */
/******************************************************************************/
/* This subroutine is to know if a variable is global                         */
/******************************************************************************/
void variableisglobalinmodule(listcouple *listin, char *module, FILE *fileout)
{
  int Globalite;
  listcouple *newvar;
  listcouple *newvarprec;
  listvar *tempo;
  listvar *newvar2;
  int out;

  Globalite = 1;
  tempo = (listvar *)NULL;
  tempo = Readthedependfile(module,tempo);
  newvar = listin;
  while ( newvar )
  {
     out = 0;
     newvar2 = tempo;
     while ( newvar2 && out == 0 )
     {
        if ( !strcasecmp(newvar2->var->nomvar,newvar->namevar) ) out = 1;
	else newvar2 = newvar2 ->suiv;
     }
     if ( out == 1 )
     {
        /* remove from the listin                                             */
	if ( newvar == listin )
	{
	   listin = listin->suiv;
           newvar = listin;
	}
	else
	{
           newvarprec->suiv = newvar->suiv;
	   newvar = newvar->suiv;
	}
     }
     else
     {
         newvarprec = newvar;
         newvar = newvar->suiv;
         Globalite = 0;
     }
  }
  if ( Globalite == 0 || !newvar)
  {
     pos_end = setposcur();
     RemoveWordSET_0(fileout,pos_curuse,
                                pos_end-pos_curuse);
     newvar = listin;
     while ( newvar )
     {
        fprintf(fileout,"      USE %s, ONLY : %s \n",module,newvar->namevar);
	newvar = newvar->suiv;
     }
  }
}

/******************************************************************************/
/*                     variableisparameterglobal                              */
/******************************************************************************/
/* This subroutine is to know if a variable is global                         */
/******************************************************************************/
int variableisparameterglobal(listvar *curvar, listparameter *listin)
{
  int Globalite;
  listparameter *newvar;

  Globalite = 0;
  newvar = listin;
  while ( newvar && Globalite == 0 )
  {
     if ( !strcasecmp(newvar->name,curvar->var->nomvar) ) Globalite = 1;
     else newvar = newvar->suiv;
  }

  return Globalite ;
}


/******************************************************************************/
/*                           addsubroutine_alloc_0                            */
/******************************************************************************/
/* Firstpass 0                                                                */
/* We should add subroutine of allocation                                     */
/* if moduleorcontains = 1 we are at the end module keyword                   */
/* if moduleorcontains = 0 we are at the contains   keyword                   */
/******************************************************************************/
void addsubroutine_alloc_0(int moduleorcontains)
{
   char ligne[LONGNOM];
   int Allocisempty;
   listvar *newvar;
   

   if ( firstpass == 0)
   {
     /* It is necessary to know if this subroutine is not empty               */
     Allocisempty = 0;
     newvar = globliste;
     while ( newvar && Allocisempty == 0 )
     {
        if ( !strcasecmp(newvar->var->modulename,curmodulename)) Allocisempty=1;
        else newvar = newvar->suiv;
     }
     if ( Allocisempty == 1 )
     {
         while ( newvar &&
                 !strcasecmp(newvar->var->modulename,curmodulename) &&
                 Allocisempty == 1 )
         {
            if ( (newvar->var->nbdim !=0          &&
                  newvar->var->allocatable != 1 ) ||
                 (newvar->var->nbdim == 0         &&
                  strcasecmp(newvar->var->initialvalue,"")) ) Allocisempty = 0;
            else newvar = newvar->suiv;
         }
     }
     if ( Allocisempty == 0 )
     {
      if ( AllocInModule() == 1)
      {
         /* we should remove end module <name>                                */
         if ( moduleorcontains == 1 )
         {
            RemoveWordCUR_0(fortranout,(long)(-strlen(curmodulename)-12),
                                          strlen(curmodulename)+11);
         }
         /* we should remove contains                                         */
         if ( moduleorcontains == 0 )
         {
            RemoveWordCUR_0(fortranout,(long)(-9),9);
         }
         strcpy (ligne, "\n      PUBLIC Alloc_agrif_");
         strcat (ligne, curmodulename);
         strcat (ligne, "\n");
         fprintf(fortranout,ligne);
      }
      if (AllocInModule() == 1)
      {      
         fprintf(fortranout,"\n      contains\n"); 
         strcpy (ligne, "\n#include \"alloc_agrif_");
         strcat (ligne, curmodulename);
         strcat (ligne, ".h\"\n");
         fprintf(fortranout,ligne);
         /* On reecrit la mot cle end module qui a ete efface du fichier      */
         /*    d'origine                                                      */
         if ( moduleorcontains == 1 ) fprintf(fortranout,"\n      end module %s"
                                                                ,curmodulename);
      }
     }
   }
}


/******************************************************************************/
/*                          IsTabvarsUseInArgument_0                          */
/******************************************************************************/
/* Firstpass 1                                                                */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
int IsTabvarsUseInArgument_0()
{
   int out;
   int doloopout;
   listvar *parcours;   

   out=1;
  
   if ( listvarindoloop )
   {
      doloopout = 0;
      parcours = listvarindoloop;
      while ( parcours && doloopout == 0 )   
      {
         if ( !strcasecmp(parcours->var->modulename,subroutinename) ) 
                                                                  doloopout = 1;
         else parcours = parcours->suiv;
      }
      if (  doloopout == 0 ) out = 0;
      else out = 1 ;
   }
   else out = 0;

   return out;
}


/******************************************************************************/
/*                        ImplicitNoneInSubroutine                            */
/******************************************************************************/
/* Firstpass 0                                                                */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
int ImplicitNoneInSubroutine()
{
  listname *parcours;
  int out;

  parcours= listimplicitnone;
  out = 0 ;
  while ( parcours && out == 0 )
  {
     if ( !strcasecmp(parcours->name,subroutinename) ) out = 1;
     else parcours = parcours->suiv;
  
  }
  return out;
}

/******************************************************************************/
/*                          varispointer_0                                    */
/******************************************************************************/
/* Firstpass 0                                                                */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
int varispointer_0(char *ident)
{
   listvar *newvar;
   int out;

   out =0;
   if (firstpass == 0 )
   {
      newvar = globalvarofusefile;
      while ( newvar && out == 0 )
      {
         if ( !strcasecmp(ident,newvar->var->nomvar) && 
              newvar->var->pointerdeclare == 1 )  out = 1;
         else newvar = newvar->suiv;
      }
   }
   return out;
}


/******************************************************************************/
/*                          VariableIsNotFunction                             */
/******************************************************************************/
/* 
/******************************************************************************/
int VariableIsNotFunction(char *ident)
{
   int out;
   listvar *newvar;

   out =0;

   if ( !strcasecmp(ident,"size") ||
        !strcasecmp(ident,"if")   ||
        !strcasecmp(ident,"max")  ||
        !strcasecmp(ident,"min") 
      )
   {
      newvar = varofsubroutineliste;
      while ( newvar && out == 0 )
      {
         if ( !strcasecmp(subroutinename, newvar->var->subroutinename) &&
	      !strcasecmp(ident, newvar->var->nomvar) ) out = 1;
         newvar = newvar -> suiv ;
      }
      if ( out == 1 ) out = 0;
      else out = 1;
      /* if it has not been found                                             */
      if ( out == 1 )
      {
         out = 0;
         newvar = globliste;
         while ( newvar && out == 0 )
         {
            if ( !strcasecmp(ident, newvar->var->nomvar) ) out = 1;
            newvar = newvar -> suiv ;
         }
         if ( out == 1 ) out = 0;
         else out = 1;
      }
   }
   /*                                                                         */
   return out;
}
