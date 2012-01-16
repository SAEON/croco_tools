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
    return Agrif_in_Tok_NAME(nom);
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
     if ( !strcasecmp(newvar->var->v_nomvar,curvar->var->v_nomvar) )
     {
        Globalite = 1;
        /* Now we should give the definition of the variable in the           */
        /* table List_UsedInSubroutine_Var                                    */
        strcpy(curvar->var->v_typevar,newvar->var->v_typevar);
        strcpy(curvar->var->v_dimchar,newvar->var->v_dimchar);
        curvar->var->v_nbdim = newvar->var->v_nbdim;
        curvar->var->v_dimensiongiven = newvar->var->v_dimensiongiven;
        curvar->var->v_allocatable = newvar->var->v_allocatable;
        curvar->var->v_target = newvar->var->v_target;
        curvar->var->v_pointerdeclare = newvar->var->v_pointerdeclare;
        curvar->var->v_indicetabvars = newvar->var->v_indicetabvars;
        strcpy(curvar->var->v_nameinttypename,newvar->var->v_nameinttypename);
        strcpy(curvar->var->v_precision,newvar->var->v_precision);
        strcpy(curvar->var->v_readedlistdimension,
                                            newvar->var->v_readedlistdimension);
        strcpy(curvar->var->v_commoninfile,newvar->var->v_commoninfile);
     }
     else
     {
         newvar = newvar->suiv;
     }
  }

  return Globalite ;
}

int VariableIsInListCommon(listvar *curvar,listvar *listin)
{
  int present;
  listvar *newvar;

  present = 0;
  newvar = listin;
  while ( newvar && present == 0 )
  {
     if ( !strcasecmp(newvar->var->v_nomvar,curvar->var->v_nomvar) &&
          !strcasecmp(newvar->var->v_subroutinename,
                                    curvar->var->v_subroutinename)
        )
     {
        strcpy(curvar->var->v_commoninfile,newvar->var->v_commoninfile);
        CopyRecord(curvar->var,newvar->var);
        present = 1;
     }
     else newvar = newvar->suiv;
  }

  return present;
}

int VariableIsInList(listvar *curvar,listvar *listin)
{
  int present;
  listvar *newvar;

  present = 0;
  newvar = listin;
  while ( newvar && present == 0 )
  {
     if ( !strcasecmp(newvar->var->v_nomvar,curvar->var->v_nomvar) )
     {
        CopyRecord(curvar->var,newvar->var);
        present = 1;
     }
     else newvar = newvar->suiv;
  }

  return present;
}

/******************************************************************************/
/*                      variableisglobalinmodule                              */
/******************************************************************************/
/* This subroutine is to know if a variable is global                         */
/******************************************************************************/
void variableisglobalinmodule(listcouple *listin, char *module, FILE *fileout, long int oldposcuruse)
{
  int Globalite;
  listcouple *newvar;
  listcouple *newvarprec;
  listvar *tempo;
  listvar *newvar2;
  int out;
  char truename[LONG_C];  

  Globalite = 1;
  newvarprec = (listcouple *)NULL;
  tempo = (listvar *)NULL;
  tempo = Readthedependfile(module,tempo);
  newvar = listin;

  while ( newvar )
  {
     if (!strcmp(newvar->c_namepointedvar,"")) {
       strcpy(truename,newvar->c_namevar);
     }
     else
     {
       strcpy(truename,newvar->c_namepointedvar);
     }
     
     out = 0;
     newvar2 = tempo;
     while ( newvar2 && out == 0 )
     {
        if ( !strcasecmp(newvar2->var->v_nomvar,truename) ) out = 1;
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
     pos_end = setposcurname(fileout);
     RemoveWordSET_0(fileout,oldposcuruse,
                                pos_end-oldposcuruse);
                                  
     newvar = listin;
     while ( newvar )
     {
        fprintf(fileout,"      USE %s, ONLY : %s \n",module,newvar->c_namevar);
        newvar = newvar->suiv;
     }
  }
}


void Remove_Word_Contains_0()
{
   if ( firstpass == 0 )
   {
      RemoveWordCUR_0(fortran_out,(long)(-9),9);
   }
}

void Remove_Word_end_module_0(int modulenamelength)
{
   if ( firstpass == 0 )
   {
      RemoveWordCUR_0(fortran_out,(long)(-modulenamelength-12), modulenamelength+11);
   }
}

void Write_Word_Contains_0()
{
   if ( firstpass == 0 )
   {
      fprintf(fortran_out,"\n      contains\n");
   }
}


void Write_Word_end_module_0()
{
   if ( firstpass == 0 )
   {
      fprintf(fortran_out,"\n      end module %s",curmodulename);
   }
}

void Add_Subroutine_For_Alloc(char *nom)
{
   listnom *parcours;
   listnom *newvar;
   int out;

   newvar = (listnom *)malloc(sizeof(listnom));
   strcpy(newvar->o_nom,nom);
   Save_Length(nom,23);
   newvar->suiv = NULL;

   if ( !List_Subroutine_For_Alloc )
   {
      List_Subroutine_For_Alloc = newvar;
   }
   else
   {
      parcours = List_Subroutine_For_Alloc;
      out = 0 ;
      while ( parcours->suiv && out == 0 )
      {
         if ( !strcasecmp(parcours->o_nom,nom) ) out = 1 ;
         else parcours = parcours ->suiv;
      }
      /*                                                                      */
      if ( out == 0 )
      {
         if ( strcasecmp(parcours->o_nom,nom) ) parcours->suiv = newvar;
      }
   }
}


void Write_Alloc_Subroutine_0()
{
   listnom *parcours_nom;
   listnom *parcours_nomprec;
   int out;
   char ligne[LONG_C];

   if ( firstpass == 0 )
   {
      parcours_nomprec = (listnom *)NULL;
      parcours_nom = List_NameOfModule;
      out = 0 ;
      while ( parcours_nom && out == 0 )
      {
         /*                                                                   */
         if ( !strcasecmp(curmodulename,parcours_nom->o_nom) ) out = 1;
         else parcours_nom = parcours_nom -> suiv;
      }
      if ( out == 1 )
      {
         if ( parcours_nom->o_val == 1 )
         {
            strcpy (ligne, "\n      PUBLIC Alloc_agrif_");
            strcat (ligne, curmodulename);
            strcat (ligne, "\n");
            convert2lower(ligne);
            fprintf(fortran_out,ligne);
         }
      }
      Write_Word_Contains_0();
      if ( out == 1 )
      {
         if ( parcours_nom->o_val == 1 )
         {
            sprintf (ligne, "Subroutine Alloc_agrif_%s(Agrif_Gr)",
                                                                 curmodulename);
            tofich(fortran_out,ligne,1);
            strcpy(ligne,"Use Agrif_Util");
            tofich(fortran_out,ligne,1);
            strcpy (ligne, "Type(Agrif_grid), Pointer :: Agrif_Gr");
            tofich(fortran_out,ligne,1);
            strcpy(ligne, "INTEGER :: i");
            tofich (fortran_out, ligne,1);
            strcpy (ligne, "\n#include \"alloc_agrif_");
            strcat (ligne, curmodulename);
            strcat (ligne, ".h\"\n");
            convert2lower(ligne);
            fprintf(fortran_out,ligne);
            strcpy (ligne, "Return");
            tofich(fortran_out,ligne,1);
            sprintf (ligne, "End Subroutine Alloc_agrif_%s",curmodulename);
            tofich(fortran_out,ligne,1);
            /* List all Call Alloc_agrif_                                     */
            Add_Subroutine_For_Alloc(curmodulename);
         }
         else
         {
            parcours_nom = List_Subroutine_For_Alloc;
            out = 0;
            while ( parcours_nom && out == 0 )
            {
               if ( !strcasecmp(parcours_nom->o_nom,curmodulename) ) out = 1;
               else
               {
                  parcours_nomprec = parcours_nom;
                  parcours_nom = parcours_nom->suiv;
               }
            }
            if ( out == 1 )
            {
               if ( parcours_nom == List_Subroutine_For_Alloc)
               {
                  List_Subroutine_For_Alloc = List_Subroutine_For_Alloc->suiv;
               }
               else
               {
                  parcours_nomprec->suiv = parcours_nom->suiv;
                  parcours_nom = parcours_nomprec->suiv ;
               }
            }
         }
      }
   }
}


void Write_Alloc_Subroutine_For_End_0()
{
   listnom *parcours_nom;
   listnom *parcours_nomprec;
   int out;
   char ligne[LONG_C];

   if ( firstpass == 0 )
   {
      parcours_nomprec = (listnom *)NULL;
      parcours_nom = List_NameOfModule;
      out = 0 ;
      while ( parcours_nom && out == 0 )
      {
         /*                                                                   */
         if ( !strcasecmp(curmodulename,parcours_nom->o_nom) ) out = 1;
         else parcours_nom = parcours_nom -> suiv;
      }
      if ( out == 1 )
      {
         if ( parcours_nom->o_val == 1 )
         {
            strcpy (ligne, "\n      PUBLIC Alloc_agrif_");
            strcat (ligne, curmodulename);
            strcat (ligne, "\n");
            convert2lower(ligne);
            fprintf(fortran_out,ligne);
            strcpy (ligne, "\n      contains\n");
            fprintf(fortran_out,ligne);
            sprintf (ligne, "Subroutine Alloc_agrif_%s(Agrif_Gr)",
                                                                 curmodulename);
            tofich(fortran_out,ligne,1);
            strcpy(ligne,"Use Agrif_Util");
            tofich(fortran_out,ligne,1);
            strcpy (ligne, "Type(Agrif_grid), Pointer :: Agrif_Gr");
            tofich(fortran_out,ligne,1);
            strcpy(ligne, "INTEGER :: i");
            tofich (fortran_out, ligne,1);
            strcpy (ligne, "\n#include \"alloc_agrif_");
            strcat (ligne, curmodulename);
            strcat (ligne, ".h\"\n");
            convert2lower(ligne);
            fprintf(fortran_out,ligne);
            strcpy (ligne, "Return");
            tofich(fortran_out,ligne,1);
            sprintf (ligne, "End Subroutine Alloc_agrif_%s",curmodulename);
            tofich(fortran_out,ligne,1);
            /* List all Call Alloc_agrif                                      */
            Add_Subroutine_For_Alloc(parcours_nom->o_nom);
         }
         else
         {
            parcours_nom = List_Subroutine_For_Alloc;
            out = 0;
            while ( parcours_nom && out == 0 )
            {
               if ( !strcasecmp(parcours_nom->o_nom,curmodulename) ) out = 1;
               else
               {
                  parcours_nomprec = parcours_nom;
                  parcours_nom = parcours_nom->suiv;
               }
            }
            if ( out == 1 )
            {
               if ( parcours_nom == List_Subroutine_For_Alloc)
               {
                  List_Subroutine_For_Alloc = List_Subroutine_For_Alloc->suiv;
               }
               else
               {
                  parcours_nomprec->suiv = parcours_nom->suiv;
                  parcours_nom = parcours_nomprec->suiv ;
               }
            }
         }
      }
   }
}

void Write_GlobalParameter_Declaration_0()
{
   listvar *parcours;

   if ( firstpass == 0 )
   {
      parcours = List_GlobalParameter_Var;
      while( parcours )
      {
         if ( !strcasecmp(parcours->var->v_modulename,curmodulename) )
         {
            writevardeclaration(parcours,module_declar,0,1);
         }
         parcours = parcours -> suiv;
      }
   }
}

void Write_GlobalType_Declaration_0()
{
   listvar *parcours;
   int out = 0;
   int headtypewritten = 0;
   char ligne[LONGNOM];
   int changeval;

   if ( firstpass == 0 )
   {
      parcours = List_Global_Var;
      while( parcours )
      {
         if ( !strcasecmp(parcours->var->v_modulename,curmodulename) )
         {
           if (!strcasecmp(parcours->var->v_typevar,"type"))
           {
            out = 1;
            if (headtypewritten == 0)
              {
                sprintf(ligne,"TYPE :: Agrif_%s",curmodulename);
                tofich(module_declar,ligne,1);
                headtypewritten = 1;
              }
            changeval = 0;
            if (parcours->var->v_allocatable == 1)
             {
               changeval = 1;
               parcours->var->v_allocatable = 0;
               parcours->var->v_pointerdeclare = 1;
             }
            writevardeclaration(parcours,module_declar,0,0);
            if (changeval == 1)
              {
               parcours->var->v_allocatable = 1;
               parcours->var->v_pointerdeclare = 0;
              }
            }
         }
         parcours = parcours -> suiv;
      }
      if (out == 1)
        {
                sprintf(ligne,"END TYPE Agrif_%s",curmodulename);
                tofich(module_declar,ligne,1);
                sprintf(ligne,"TYPE(Agrif_%s), DIMENSION(:), ALLOCATABLE :: Agrif_%s_var",curmodulename,curmodulename); 
                tofich(module_declar,ligne,1);
                sprintf(ligne,"PUBLIC :: Agrif_%s",curmodulename); 
                tofich(module_declar,ligne,1);
                sprintf(ligne,"PUBLIC :: Agrif_%s_var",curmodulename); 
                tofich(module_declar,ligne,1);
        }
   }
}

void Write_NotGridDepend_Declaration_0()
{
   listvar *parcours;

   if ( firstpass == 0 )
   {
      parcours = List_NotGridDepend_Var;
      while( parcours )
      {
         if ( !strcasecmp(parcours->var->v_modulename,curmodulename) )
         {
            writevardeclaration(parcours,fortran_out,0,1);
         }
         parcours = parcours -> suiv;
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

   if ( List_UsedInSubroutine_Var )
   {
      doloopout = 0;
      parcours = List_UsedInSubroutine_Var;
      while ( parcours && doloopout == 0 )
      {
         if ( !strcasecmp(parcours->var->v_subroutinename,subroutinename) )
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

  parcours= List_ImplicitNoneSubroutine;
  out = 0 ;
  while ( parcours && out == 0 )
  {
     if ( !strcasecmp(parcours->n_name,subroutinename) ) out = 1;
     else parcours = parcours->suiv;
  }
  return out;
}

/******************************************************************************/
/*                            Add_Pointer_Var_From_List_1                     */
/******************************************************************************/
/* Firstpass 1                                                                */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void Add_Pointer_Var_From_List_1(listvar *listin)
{
   listvar *parcours;

   if ( firstpass == 1 )
   {
       parcours = listin;
       while ( parcours )
       {
          Add_Pointer_Var_1(parcours->var->v_nomvar);
          parcours = parcours -> suiv ;
       }
   }
}

/******************************************************************************/
/*                            Add_Pointer_Var_1                               */
/******************************************************************************/
/* Firstpass 1                                                                */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void Add_Pointer_Var_1(char *nom)
{
   listname *newvar;
   listname *parcours;
   int out;

   if ( firstpass == 1 )
   {
      if ( !List_Pointer_Var )
      {
         newvar = (listname *)malloc(sizeof(listname));
         strcpy(newvar->n_name,nom);
         Save_Length(nom,20);
         newvar->suiv = NULL;
         List_Pointer_Var = newvar;
      }
      else
      {
         parcours = List_Pointer_Var;
         out = 0 ;
         while ( parcours->suiv && out == 0 )
         {
            if (  !strcasecmp(parcours->n_name,nom) ) out = 1;
            else
               parcours=parcours->suiv;
         }
         if ( out == 0 )
         {
            if (  !strcasecmp(parcours->n_name,nom) ) out = 1;
            else
            {
               /* add the record                                              */
              newvar = (listname *)malloc(sizeof(listname));
              strcpy(newvar->n_name,nom);
              Save_Length(nom,20);
              newvar->suiv = NULL;
              parcours->suiv = newvar;
            }
         }
      }
   }
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
   listname *newname;
   int out;

   out =0;
   if ( firstpass == 0 )
   {
      newname = List_Pointer_Var;
      while( newname && out == 0 )
      {
         if ( !strcasecmp(ident,newname->n_name) ) out = 1 ;
         else newname = newname->suiv;
      }
   }
   return out;
}

/******************************************************************************/
/*                          varistyped_0                                    */
/******************************************************************************/
/* Firstpass 0                                                                */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
int varistyped_0(char *ident)
{
   listvar *parcours;
   int out;

   out =0;
   if ( firstpass == 0 )
   {
      parcours = List_Global_Var;
      while( parcours && out == 0 )
      {
         if ( !strcasecmp(ident,parcours->var->v_nomvar) ) 
             {
             if (!strcasecmp(parcours->var->v_typevar,"type")) out = 1;
             }
         parcours = parcours->suiv;
      }
   }
   return out;
}


/******************************************************************************/
/*                          VariableIsNotFunction                             */
/******************************************************************************/
/*                                                                            */
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
      newvar = List_SubroutineDeclaration_Var;
      while ( newvar && out == 0 )
      {
         if ( !strcasecmp(subroutinename, newvar->var->v_subroutinename) &&
              !strcasecmp(ident, newvar->var->v_nomvar) ) out = 1;
         newvar = newvar -> suiv ;
      }
      if ( out == 1 ) out = 0;
      else out = 1;
      /* if it has not been found                                             */
      if ( out == 1 )
      {
         out = 0;
         newvar = List_Global_Var;
         while ( newvar && out == 0 )
         {
            if ( !strcasecmp(ident, newvar->var->v_nomvar) ) out = 1;
            newvar = newvar -> suiv ;
         }
         if ( out == 1 ) out = 0;
         else out = 1;
      }
   }
   /*                                                                         */
   return out;
}
