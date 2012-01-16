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
/*                         WriteBeginDeclaration                              */
/******************************************************************************/
/* This subroutine is used to write the begin of a declaration                */
/* taken in a variable record                                                 */
/*                                                                            */
/******************************************************************************/
/*                                                                            */
/*       integer variable ----------->   INTEGER                              */
/*                                                                            */
/******************************************************************************/
void WriteBeginDeclaration(variable *v,char ligne[LONG_4C], int visibility)
{
  char tmpligne[LONG_4C];

  if ( !strcasecmp(v->v_typevar,"") )
  {
     printf("WARNING : The type of the variable %s \n",v->v_nomvar);
     printf("          is unknown. CONV should define a type\n");
  }
  
  sprintf (ligne, "%s", v->v_typevar);
  if ( v->v_c_star == 1 ) strcat(ligne,"*");
  
  /* We should give the precision of the variable if it has been given        */
  if ( strcasecmp(v->v_precision,"") )
  {
     sprintf(tmpligne,"(%s)",v->v_precision);
     Save_Length(tmpligne,49);
     strcat(ligne,tmpligne);
  }
  
  if (strcasecmp(v->v_dimchar,""))
  {
     sprintf(tmpligne,"(%s)",v->v_dimchar);
     Save_Length(tmpligne,49);
     strcat(ligne,tmpligne);
  }
  
  if ( strcasecmp(v->v_nameinttypename,"") )
  {
     sprintf(tmpligne,"*%s",v->v_nameinttypename);
     Save_Length(tmpligne,49);
     strcat(ligne,tmpligne);
  }
  if (strcasecmp (v->v_IntentSpec, ""))
  {
     sprintf(tmpligne,",INTENT(%s) ",v->v_IntentSpec);
     Save_Length(tmpligne,49);
     strcat(ligne,tmpligne);
  }
  if ( v->v_VariableIsParameter == 1 ) strcat(ligne, ", PARAMETER");
  if (visibility == 1)
  {
  if ( v->v_PublicDeclare       == 1 ) strcat(ligne, ", PUBLIC");
  if ( v->v_PrivateDeclare      == 1 ) strcat(ligne, ", PRIVATE");
  }
  if ( v->v_ExternalDeclare     == 1 ) strcat(ligne, ", EXTERNAL");
  if ( v->v_allocatable         == 1)
       {strcat(ligne,", ALLOCATABLE");
       }
  if ( v->v_target         == 1)
       {strcat(ligne,", TARGET");
       }
  if ( v->v_optionaldeclare     == 1 ) strcat(ligne,", OPTIONAL");
  if ( v->v_pointerdeclare      == 1 ) strcat(ligne,", POINTER");
  Save_Length(ligne,45);
}


/******************************************************************************/
/*                         WriteScalarDeclaration                             */
/******************************************************************************/
/* This subroutine is used to write a scalar declaration                      */
/* taken in a variable record                                                 */
/*                                                                            */
/******************************************************************************/
/*                                                                            */
/*       integer variable ----------->   INTEGER :: VARIABLE                  */
/*                                                                            */
/******************************************************************************/
void WriteScalarDeclaration(variable *v,char ligne[LONG_4C])
{

  strcat (ligne, " :: ");
  strcat (ligne, v->v_nomvar);
  if ( strcasecmp(v->v_vallengspec,"") ) strcat(ligne,v->v_vallengspec);
  if ( v->v_VariableIsParameter == 1 )
  {
     strcat(ligne," = ");
     strcat(ligne,v->v_initialvalue);
  }
  Save_Length(ligne,45);
}


/******************************************************************************/
/*                         WriteTableDeclaration                              */
/******************************************************************************/
/* This subroutine is used to write a Table declaration                       */
/* taken in a variable record                                                 */
/*                                                                            */
/******************************************************************************/
/*                                                                            */
/*  integer variable(nb) ----------->                                         */
/*                      INTEGER, DIMENSION(1:nb) :: variable                  */
/*                                                                            */
/******************************************************************************/
void WriteTableDeclaration(variable * v,char ligne[LONG_4C],int tmpok)
{
  char newname[LONG_4C];

  strcat (ligne, ", Dimension(");

  if ( v->v_dimensiongiven == 1 && tmpok == 1 )
  {
                                         strcat(ligne,v->v_readedlistdimension);
                                         }
  if ( v->v_dimensiongiven == 1 && tmpok == 0 )
  {
     strcpy(newname,ChangeTheInitalvaluebyTabvarsName
                                  (v->v_readedlistdimension,List_Global_Var,0));

     if ( !strcasecmp(newname,"") ) strcat(newname,v->v_readedlistdimension);

        strcpy(newname,ChangeTheInitalvaluebyTabvarsName
                                 (newname,List_Common_Var,0));

     if ( !strcasecmp(newname,"") ) strcat(newname,v->v_readedlistdimension); 
     
        strcpy(newname,ChangeTheInitalvaluebyTabvarsName
                              (newname,List_ModuleUsed_Var,0));
        if ( !strcasecmp(newname,"") ) strcat(newname,v->v_readedlistdimension);

     Save_Length(newname,47);
     strcat(ligne,newname);
  }
  strcat (ligne, ")");
  strcat (ligne, " :: ");
  strcat (ligne, v->v_nomvar);
  if ( strcasecmp(vallengspec,"") ) strcat(ligne,v->v_vallengspec);

  if ( v->v_VariableIsParameter == 1 )
  {
     strcat(ligne," = ");
     strcat(ligne,v->v_initialvalue);
  }
  Save_Length(ligne,45);
}

/******************************************************************************/
/*                        writevardeclaration                                 */
/******************************************************************************/
/* This subroutine is used to write the initial declaration in the file       */
/* fileout of a variable                                                      */
/*                                                                            */
/******************************************************************************/
/*                                                                            */
/*  integer variable(nb) ----------->                                         */
/*                      INTEGER, DIMENSION(1:nb),Pointer :: variable          */
/*                                                                            */
/******************************************************************************/
void writevardeclaration (listvar * var_record, FILE *fileout, int value, int visibility)
{
  FILE *filecommon;
  listvar *newvar;
  variable *v;
  char ligne[LONG_4C];

  filecommon=fileout;
  newvar = var_record;
  
  if ( newvar->var->v_save == 0 || inmodulemeet == 0 )
  {
     v = newvar->var;
     WriteBeginDeclaration(v,ligne,visibility);

     if ( v->v_nbdim == 0 ) WriteScalarDeclaration(v,ligne);
     else WriteTableDeclaration(v,ligne,value);

     if ( v->v_VariableIsParameter != 1 && strcasecmp(v->v_initialvalue,"") )
     {
        strcat(ligne," = ");
        strcat(ligne,v->v_initialvalue);
     }
     
     tofich (filecommon, ligne,1);
  }
  Save_Length(ligne,45);
  
}


void WriteLocalParamDeclaration()
{
   listvar *parcours;

   parcours = List_Parameter_Var;
   while ( parcours )
   {
      if ( !strcasecmp(parcours->var->v_subroutinename,subroutinename) )
      {
         writevardeclaration(parcours,fortran_out,0,1);
      }
      parcours = parcours -> suiv;
   }
}

void WriteFunctionDeclaration(int value)
{
   listvar *parcours;

   parcours = List_FunctionType_Var;
   while ( parcours )
   {
      if ( !strcasecmp(parcours->var->v_subroutinename,subroutinename) &&
            strcasecmp(parcours->var->v_typevar,"")
         )
      {
         writevardeclaration(parcours,fortran_out,value,1);
      }
      parcours = parcours -> suiv;
   }
}

void WriteSubroutineDeclaration(int value)
{
   listvar *parcours;

   parcours = List_SubroutineDeclaration_Var;
   while ( parcours )
   {
      if ( !strcasecmp(parcours->var->v_subroutinename,subroutinename) &&
           parcours->var->v_save == 0                                  &&
           parcours->var->v_pointerdeclare == 0                        &&
           parcours->var->v_VariableIsParameter == 0                   &&
           parcours->var->v_common == 0
         )
      {
         writevardeclaration(parcours,fortran_out,value,1);

      }
      else if ( !strcasecmp(parcours->var->v_subroutinename,subroutinename) &&
           parcours->var->v_save == 0                                  &&
           parcours->var->v_VariableIsParameter == 0                   &&
           parcours->var->v_common == 0
              )
      {
         writevardeclaration(parcours,fortran_out,value,1);

      }
      parcours = parcours -> suiv;
   }
}

void WriteArgumentDeclaration_beforecall()
{
   int position;
   listnom *neededparameter;
   FILE *paramtoamr;
   listvar *newvar;
   char ligne[LONG_4C];

   fprintf(fortran_out,"#include \"Param_BeforeCall_%s.h\" \n",subroutinename);
   /*                                                                         */
   sprintf(ligne,"Param_BeforeCall_%s.h",subroutinename);
   paramtoamr = open_for_write(ligne);
   /*                                                                         */
   neededparameter = (listnom * )NULL;
   /*                                                                         */
   position = 1;
   newvar = List_SubroutineArgument_Var;
   while ( newvar )
   {
      if ( !strcasecmp(newvar->var->v_subroutinename,subroutinename) &&
                       newvar->var->v_positioninblock == position
         )
      {
         position = position + 1;

         writevardeclaration(newvar,fortran_out,0,1);
         neededparameter = writedeclarationintoamr(List_Parameter_Var,
                   paramtoamr,newvar->var,newvar->var->v_subroutinename,
                   neededparameter,subroutinename);

         newvar = List_SubroutineArgument_Var;
      }
      else newvar = newvar -> suiv;
   }
   Save_Length(ligne,45);
   fclose(paramtoamr);
}

void WriteArgumentDeclaration_Sort()
{
   int position;
   listvar *newvar;

   /*                                                                         */
   position = 1;
   newvar = List_SubroutineArgument_Var;
   while ( newvar )
   {
      if ( !strcasecmp(newvar->var->v_subroutinename,subroutinename) &&
                       newvar->var->v_positioninblock == position
         )
      {
         position = position + 1;

         writevardeclaration(newvar,fortran_out,1,1);
         /*                                                                   */
         newvar = List_SubroutineArgument_Var;
      }
      else newvar = newvar -> suiv;
   }
   /*                                                                         */
   newvar = List_SubroutineArgument_Var;
   while ( newvar )
   {
      if ( !strcasecmp(newvar->var->v_subroutinename,subroutinename) &&
                       newvar->var->v_positioninblock == 0           &&
                       newvar->var->v_nbdim == 0
         )
      {

         writevardeclaration(newvar,fortran_out,1,1);
      }
      newvar = newvar -> suiv;
   }
   /*                                                                         */
   newvar = List_SubroutineArgument_Var;
   while ( newvar )
   {
      if ( !strcasecmp(newvar->var->v_subroutinename,subroutinename) &&
                       newvar->var->v_positioninblock == 0           &&
                       newvar->var->v_nbdim != 0
         )
      {
         writevardeclaration(newvar,fortran_out,1,1);
      }
      newvar = newvar -> suiv;
   }
}

/******************************************************************************/
/*                      writedeclarationintoamr                               */
/******************************************************************************/
/* This subroutine is used to write the declaration of parameters needed in   */
/*    allocation subroutines creates in toamr.c                               */
/******************************************************************************/
/*                                                                            */
/*                                                                            */
/******************************************************************************/
listnom *writedeclarationintoamr (listvar * deb_common, FILE *fileout,
                              variable *var , char commonname[LONG_C],
                           listnom *neededparameter, char name_common[LONG_C])
{
  listvar *newvar;
  variable *v;
  char ligne[LONG_4C];
  int changeval;
  int out;
  int writeit;
  listnom *parcours;
  listnom *parcoursprec;

  parcoursprec = (listnom * )NULL;

  /* we should list the needed parameter                                      */
  if ( !strcasecmp(name_common,commonname) )
     neededparameter = DecomposeTheNameinlistnom(var->v_readedlistdimension,
                                                               neededparameter);
  /*                                                                          */
  parcours = neededparameter;
  while (parcours)
  {
     newvar = deb_common;

     out = 0 ;
     while ( newvar && out == 0 )
     {
     
        if ( !strcasecmp(parcours->o_nom,newvar->var->v_nomvar) && !strcasecmp(var->v_subroutinename,newvar->var->v_subroutinename))
        {
           out=1;
        /* add the name to the list of needed parameter                       */
           neededparameter = DecomposeTheNameinlistnom(
                 newvar->var->v_initialvalue,
                 neededparameter );
        }
        else newvar=newvar->suiv;
     }
     parcours=parcours->suiv;
   }
  /*                                                                          */
  parcours = neededparameter;
  while (parcours)
  {
     newvar = deb_common;
     out = 0 ;
     while ( newvar && out == 0 )
     {
        if ( !strcasecmp(parcours->o_nom,newvar->var->v_nomvar) && !strcasecmp(var->v_subroutinename,newvar->var->v_subroutinename))        
        {
           out=1;
        /* add the name to the list of needed parameter                       */
           neededparameter = DecomposeTheNameinlistnom(
                 newvar->var->v_initialvalue,
                 neededparameter );
        }
        else newvar=newvar->suiv;
     }
     parcours=parcours->suiv;
   }
  /*                                                                          */
  tofich (fileout, "",1);
  parcours = neededparameter;
  while (parcours)
  {
     writeit = 0;
     newvar = deb_common;
     while ( newvar && writeit == 0 )
     {
        if ( !strcasecmp(parcours->o_nom,newvar->var->v_nomvar) &&
            !strcasecmp(var->v_subroutinename,newvar->var->v_subroutinename) && parcours->o_val == 0 )
        {
           writeit=1;
           parcours->o_val = 1;
        }
        else newvar = newvar->suiv;
     }

     if ( writeit == 1  )
     {
        changeval = 0;
        v = newvar->var;
//        if ( v->v_allocatable == 1 && strcasecmp(v->v_typevar,"type") )
//        {
//           changeval = 1;
//           v->v_allocatable = 0;
//        }
        WriteBeginDeclaration(v,ligne,1);
        if ( v->v_nbdim == 0 ) WriteScalarDeclaration(v,ligne);
        else WriteTableDeclaration(v,ligne,1);

        tofich (fileout, ligne,1);
        if ( changeval == 1 )
        {
           v->v_allocatable = 1;
        }
     }
     else
     {
        if (  strncasecmp(parcours->o_nom,"mpi_",4) == 0 &&
              shouldincludempif                     == 1 )
        {
           shouldincludempif = 0;
           fprintf(fileout,"      include \'mpif.h\' \n");
        }
     }
     parcours=parcours->suiv;
  }
  Save_Length(ligne,45);
  return neededparameter;
}


/******************************************************************************/
/*                       writesub_loopdeclaration_scalar                      */
/******************************************************************************/
/* This subroutine is used to write the declaration part of subloop           */
/*    subroutines                                                             */
/******************************************************************************/
/*                                                                            */
/*  integer variable(nb) ----------->                                         */
/*                                                                            */
/*          INTEGER, DIMENSION(1:nb)         :: variable                      */
/*                                                                            */
/******************************************************************************/
void writesub_loopdeclaration_scalar (listvar * deb_common, FILE *fileout)
{
  listvar *newvar;
  variable *v;
  char ligne[LONG_4C];

  tofich (fileout, "",1);
  newvar = deb_common;

  while (newvar)
  {
     if ( newvar->var->v_nbdim == 0 &&
          !strcasecmp(newvar->var->v_subroutinename,subroutinename)  &&
           newvar->var->v_pointerdeclare == 0
         )
     {
        v = newvar->var;

        WriteBeginDeclaration(v,ligne,1);
        WriteScalarDeclaration(v,ligne);
        tofich (fileout, ligne,1);
     }
     newvar = newvar->suiv;
  }
  Save_Length(ligne,45);
}

/******************************************************************************/
/*                       writesub_loopdeclaration_tab                         */
/******************************************************************************/
/* This subroutine is used to write the declaration part of subloop           */
/*    subroutines                                                             */
/******************************************************************************/
/*                                                                            */
/*  integer variable(nb) ----------->                                         */
/*                                                                            */
/*          INTEGER, DIMENSION(1:nb)         :: variable                      */
/*                                                                            */
/******************************************************************************/
void writesub_loopdeclaration_tab (listvar * deb_common, FILE *fileout)
{
  listvar *newvar;
  variable *v;
  char ligne[LONG_4C];
  int changeval;

  tofich (fileout, "",1);
  newvar = deb_common;
  while (newvar)
  {
//  printf("newvar = %s %d %s\n",newvar->var->v_nomvar,newvar->var->v_pointerdeclare,newvar->var->v_typevar);
     if ( newvar->var->v_nbdim != 0                                 &&
          !strcasecmp(newvar->var->v_subroutinename,subroutinename) &&
          (newvar->var->v_pointerdeclare == 0 || !strcasecmp(newvar->var->v_typevar,"type"))
        )
     {
        changeval = 0;
        v = newvar->var;
        if ( v->v_allocatable == 1)
        {
          if (strcasecmp(v->v_typevar,"type"))
           {
      //     changeval = 1;
      //     v->v_allocatable = 0;
           }
          else
           {
           changeval = 2;
           v->v_allocatable = 0;
           v->v_pointerdeclare = 1;
           }
        }

        WriteBeginDeclaration(v,ligne,1);
        WriteTableDeclaration(v,ligne,1);
        tofich (fileout, ligne,1);
        if ( changeval >= 1 ) v->v_allocatable = 1;
        if ( changeval == 2 ) v->v_pointerdeclare = 0;
     }
     newvar = newvar->suiv;
  }
  Save_Length(ligne,45);
}

void ReWriteDeclarationAndAddTosubroutine_01(listvar *listdecl)
{
    listvar *parcours;
    int out;

    if (insubroutinedeclare == 1)
    {
        parcours = listdecl;
        while (parcours)
        {
            out = LookingForVariableInList(List_SubroutineArgument_Var,parcours->var);
            if (out == 0) out = VariableIsInListCommon(parcours,List_Common_Var);
            if (out == 0) out = LookingForVariableInList(List_Parameter_Var,parcours->var);
            if (out == 0) out = LookingForVariableInList(List_FunctionType_Var,parcours->var);
            if (out == 0) out = LookingForVariableInListGlobal(List_Global_Var,parcours->var);

            if (firstpass == 0 && out == 0 && VariableIsParameter == 0 && SaveDeclare == 0) 
            {
                writevardeclaration(parcours,fortran_out,1,1);
            }
            if (firstpass == 1)
            {
                if (VariableIsParameter == 0 && SaveDeclare == 0)
                {
                    List_SubroutineDeclaration_Var = insertvar(List_SubroutineDeclaration_Var,parcours->var);
                }
            }
            parcours = parcours->suiv;
        }
    }
}

void ReWriteDataStatement_0(FILE * filout)
{
    listvar *parcours;
    int out;
    char ligne[LONG_C];
    char initialvalue[LONG_C];

    if (insubroutinedeclare == 1)
    {
        parcours = List_Data_Var_Cur ;
        while (parcours)
        {
            out = VariableIsInListCommon(parcours,List_Common_Var);
            if (out == 0) out = LookingForVariableInListGlobal(List_Global_Var,parcours->var);
            if (out == 0)
            {
                if (strncasecmp(parcours->var->v_initialvalue,"(/",2))
                {
                    strcpy(initialvalue,parcours->var->v_initialvalue);
                }
                else
                {
                    strncpy(initialvalue,&parcours->var->v_initialvalue[2],strlen(parcours->var->v_initialvalue)-4);
                    strcpy(&initialvalue[strlen(parcours->var->v_initialvalue)-4],"\0");
                }
                sprintf(ligne,"data %s/%s/",parcours->var->v_nomvar,initialvalue);
                tofich(filout,ligne,1);
            }
            parcours = parcours->suiv;
        }
    }
}