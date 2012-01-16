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
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "decl.h"
char lvargridname[LONG_4C];
char lvargridname2[LONG_4C];


/******************************************************************************/
/*                       variablenameroottabvars                              */
/******************************************************************************/
/* This subroutine is used to create the string                               */
/******************************************************************************/
/*                                                                            */
/*  ----------->  Agrif_Mygrid % tabvars (i) % var                            */
/*                                                                            */
/******************************************************************************/
char *variablenameroottabvars (variable * var)
{
  char *ligne;

  ligne = (char *) malloc (LONG_C * sizeof (char));
  sprintf (ligne, "Agrif_Mygrid %% tabvars(%d) %% var ", var->v_indicetabvars);
  return ligne;
}


/******************************************************************************/
/*                        variablenametabvars                                 */
/******************************************************************************/
/* This subroutine is used to create the string                               */
/******************************************************************************/
/*                                                                            */
/*  if iorindice = 0 ---------->  Agrif_Gr % tabvars (i) % var                */
/*                                                                            */
/*  if iorindice = 1 ---------->  Agrif_Gr % tabvars (12) % var               */
/*                                                                            */
/******************************************************************************/
char *variablenametabvars (variable * var, int iorindice)
{
  char *ligne;

  ligne = (char *) malloc (LONG_C * sizeof (char));
  if ( iorindice == 0 ) sprintf (ligne, " Agrif_Gr %% tabvars(%d)%% var",
                                 var->v_indicetabvars);
  else sprintf (ligne, " Agrif_Gr %% tabvars(i)%% var");
  return ligne;
}

/******************************************************************************/
/*                        variablecurgridtabvars                              */
/******************************************************************************/
/* This subroutine is used to create the string                               */
/******************************************************************************/
/*                                                                            */
/*  ----------->  Agrif_Curgrid % tabvars (i) % var                           */
/*                                                                            */
/******************************************************************************/
char *variablecurgridtabvars (variable * var,int ParentOrCurgrid)
{
  char *ligne;

  ligne = (char *) malloc (LONG_C * sizeof (char));
  if ( ParentOrCurgrid == 0 ) sprintf (ligne, " Agrif_tabvars(%d) %% var",
                              var->v_indicetabvars);
  else if ( ParentOrCurgrid == 1 ) sprintf (ligne,
                              " Agrif_tabvars(%d) %% parent_var %% var",
                               var->v_indicetabvars);
  else if ( ParentOrCurgrid == 2 ) sprintf (ligne,
                              " Agrif_Mygrid %% tabvars(%d) %% var",
                               var->v_indicetabvars);
  else if ( ParentOrCurgrid == 3 ) sprintf (ligne,
                              " Agrif_Curgrid %% tabvars(%d) %% var",
                               var->v_indicetabvars);
  else sprintf (ligne, " AGRIF_Mygrid %% tabvars(%d) %% var",
                               var->v_indicetabvars);
  return ligne;
}

void WARNING_CharSize(variable *var)
{
   if ( var->v_nbdim == 0 )
   {
      if ( convert2int(var->v_dimchar) > 2050 )
      {
         printf("WARNING : The dimension of the character  %s   \n",
                                                              var->v_nomvar);
         printf("   is upper than 2050. You must change         \n");
         printf("   the dimension of carray0                    \n");
         printf("   in the file AGRIF/AGRIF_FILES/modtypes.F    \n");
         printf("   line 247. Replace 300 with %d.              \n",
                                            convert2int(var->v_dimchar)+100);
      }
      Save_Length_int(convert2int(var->v_dimchar),1);
   }
   else if ( var->v_nbdim == 1 )
   {
      if ( convert2int(var->v_dimchar) > 300 )
      {
         printf("WARNING : The dimension of the character  %s   \n",
                                                              var->v_nomvar);
         printf("   is upper than 300. You must change          \n");
         printf("   the dimension of carray1                    \n");
         printf("   in the file AGRIF/AGRIF_FILES/modtypes.F    \n");
         printf("   line 247. Replace 300 with %d.              \n",
                                            convert2int(var->v_dimchar)+100);
      }
      Save_Length_int(convert2int(var->v_dimchar),2);
   }
   else if ( var->v_nbdim == 2 )
   {
      if ( convert2int(var->v_dimchar) > 300 )
      {
         printf("WARNING : The dimension of the character  %s   \n",
                                                              var->v_nomvar);
         printf("   is upper than 300. You must change          \n");
         printf("   the dimension of carray2                    \n");
         printf("   in the file AGRIF/AGRIF_FILES/modtypes.F    \n");
         printf("   line 247. Replace 300 with %d.              \n",
                                            convert2int(var->v_dimchar)+100);
      }
      Save_Length_int(convert2int(var->v_dimchar),3);
   }
   else if ( var->v_nbdim == 3 )
   {
      if ( convert2int(var->v_dimchar) > 300 )
      {
         printf("WARNING : The dimension of the character  %s   \n",
                                                              var->v_nomvar);
         printf("   is upper than 300. You must change          \n");
         printf("   the dimension of carray3                    \n");
         printf("   in the file AGRIF/AGRIF_FILES/modtypes.F    \n");
         printf("   line 247. Replace 300 with %d.              \n",
                                            convert2int(var->v_dimchar)+100);
      }
      Save_Length_int(convert2int(var->v_dimchar),4);
   }
}
/******************************************************************************/
/*                           vargridnametabvars                               */
/******************************************************************************/
/* This subroutine is used to create the string                               */
/******************************************************************************/
/*                                                                            */
/*  if iorindice == 0 ----------->  Agrif_Gr % tabvars (i) % var % array1     */
/*                                                                            */
/*  if iorindice == 1 ----------->  Agrif_Gr % tabvars (12) % var % array1    */
/*                                                                            */
/******************************************************************************/
char *vargridnametabvars (variable * var,int iorindice)
{
  char *tmp;
  char tmp1[LONG_C];

  tmp = variablenametabvars (var,iorindice);
  strcpy(tmp1,tmp);
  if ( todebugfree == 1 ) free(tmp);

  sprintf (lvargridname, "%s", tmp1);
  if (!strcasecmp (var->v_typevar, "REAL"))
    {
      if ( !strcasecmp(var->v_nameinttypename,"8") )
                           sprintf (lvargridname2, "%% darray%d", var->v_nbdim);
      else if ( !strcasecmp(var->v_nameinttypename,"4") )
                           sprintf (lvargridname2, "%% sarray%d", var->v_nbdim);
      else sprintf (lvargridname2, "%% array%d", var->v_nbdim);
    }
  else if (!strcasecmp (var->v_typevar, "INTEGER"))
    {
      sprintf (lvargridname2, "%% iarray%d", var->v_nbdim);
    }
  else if (!strcasecmp (var->v_typevar, "LOGICAL"))
    {
      sprintf (lvargridname2, "%% larray%d", var->v_nbdim);
    }
  else if (!strcasecmp (var->v_typevar, "CHARACTER"))
    {
      WARNING_CharSize(var);
      sprintf (lvargridname2, "%% carray%d", var->v_nbdim);
    }

  strcat (lvargridname, lvargridname2);

  Save_Length(lvargridname,42);
  Save_Length(lvargridname2,42);
  return lvargridname;
}

/******************************************************************************/
/*                           vargridcurgridtabvars                            */
/******************************************************************************/
/* This subroutine is used to create the string                               */
/******************************************************************************/
/*                                                                            */
/* if ParentOrCurgrid == 0 -->  Agrif_Curgrid % tabvars (i) % var % array1    */
/*                                                                            */
/* if ParentOrCurgrid == 1 -->  Agrif_tabvars (i) % parent_var %var % array1  */
/*                                                                            */
/* if ParentOrCurgrid == 2 -->  Agrif_Gr % tabvars (i) % var % array1         */
/*                                                                            */
/******************************************************************************/
char *vargridcurgridtabvars (variable * var,int ParentOrCurgrid)
{
  char *tmp;
  char tmp1[LONG_C];

 if (!strcasecmp(var->v_typevar,"type"))
  {
  strcpy(lvargridname2,"");
  sprintf(lvargridname,"Agrif_%s_var(Agrif_Curgrid%%fixedrank)%%%s",var->v_modulename,var->v_nomvar);
  printf("modulename = %s %s\n",var->v_nomvar, var->v_modulename);
  }
  else
  {
  tmp = variablecurgridtabvars (var,ParentOrCurgrid);
  strcpy(tmp1,tmp);
  if ( todebugfree == 1 ) free(tmp);

  sprintf (lvargridname, "%s", tmp1);
  if (!strcasecmp (var->v_typevar, "REAL"))
    {
      if ( !strcasecmp(var->v_nameinttypename,"8") )
                           sprintf (lvargridname2, "%% darray%d", var->v_nbdim);
      else if ( !strcasecmp(var->v_nameinttypename,"4") )
                           sprintf (lvargridname2, "%% sarray%d", var->v_nbdim);
      else sprintf (lvargridname2, "%% array%d", var->v_nbdim);
    }
  else if (!strcasecmp (var->v_typevar, "INTEGER"))
    {
      sprintf (lvargridname2, "%% iarray%d", var->v_nbdim);
    }
  else if (!strcasecmp (var->v_typevar, "LOGICAL"))
    {
      sprintf (lvargridname2, "%% larray%d", var->v_nbdim);
    }
  else if (!strcasecmp (var->v_typevar, "CHARACTER"))
    {
      WARNING_CharSize(var);
      sprintf (lvargridname2, "%% carray%d", var->v_nbdim);
    }
  }

  strcat (lvargridname, lvargridname2);

  Save_Length(lvargridname,42);
  Save_Length(lvargridname2,42);
  return lvargridname;
}

/******************************************************************************/
/*                  vargridcurgridtabvarswithoutAgrif_Gr                      */
/******************************************************************************/
/* This subroutine is used to create the string                               */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
char *vargridcurgridtabvarswithoutAgrif_Gr (variable * var)
{

  sprintf (lvargridname, "(%d) %% var", var->v_indicetabvars);

  if (!strcasecmp (var->v_typevar, "REAL"))
    {
      if ( !strcasecmp(var->v_nameinttypename,"8") )
                           sprintf (lvargridname2, "%% darray%d", var->v_nbdim);
      else if ( !strcasecmp(var->v_nameinttypename,"4") )
                           sprintf (lvargridname2, "%% sarray%d", var->v_nbdim);
      else sprintf (lvargridname2, "%% array%d", var->v_nbdim);
    }
  else if (!strcasecmp (var->v_typevar, "INTEGER"))
    {
      sprintf (lvargridname2, "%% iarray%d", var->v_nbdim);
    }
  else if (!strcasecmp (var->v_typevar, "LOGICAL"))
    {
      sprintf (lvargridname2, "%% larray%d", var->v_nbdim);
    }
  else if (!strcasecmp (var->v_typevar, "CHARACTER"))
    {
      WARNING_CharSize(var);
      sprintf (lvargridname2, "%% carray%d", var->v_nbdim);
    }

  strcat (lvargridname, lvargridname2);

  Save_Length(lvargridname,42);
  Save_Length(lvargridname2,42);
  return lvargridname;
}

/******************************************************************************/
/*                               vargridparam                                 */
/******************************************************************************/
/* This subroutine is used to create the string which contains                */
/* dimension list                                                             */
/******************************************************************************/
/*                                                                            */
/*  DIMENSION(jpi,0:jpj) ----------->"1:jpi,0:jpj"                            */
/*                                                                            */
/******************************************************************************/
char *vargridparam (variable * v, int whichone)
{
  typedim dim;
  listdim *newdim;
  char newname[LONG_4C];

  newdim = v->v_dimension;
  if (!newdim) return "";

  strcpy (tmpvargridname, "(");
  while (newdim)
  {
     dim = newdim->dim;

     strcpy(newname,"");
     strcpy(newname,
            ChangeTheInitalvaluebyTabvarsName(dim.first,List_Global_Var,
                                                                     whichone));
                                                                     
        strcpy(newname,ChangeTheInitalvaluebyTabvarsName(newname,
                       List_Common_Var,whichone));

        strcpy(newname,ChangeTheInitalvaluebyTabvarsName(newname,
                       List_ModuleUsed_Var,whichone));

     strcat (tmpvargridname, newname);
     strcat (tmpvargridname, " : ");

     strcpy(newname,"");
     strcpy(newname,ChangeTheInitalvaluebyTabvarsName
                        (dim.last,List_Global_Var,whichone));
                        
        strcpy(newname,ChangeTheInitalvaluebyTabvarsName
                       (newname, List_Common_Var,whichone));   
                       
        strcpy(newname,ChangeTheInitalvaluebyTabvarsName
                       (newname, List_ModuleUsed_Var,whichone));                                            
                        
     Save_Length(tmpvargridname,46);
     strcat (tmpvargridname, newname);
     newdim = newdim->suiv;
     if (newdim) strcat (tmpvargridname, ",");
  }
  strcat (tmpvargridname, ")");
  strcat (tmpvargridname, "\0");
  Save_Length(tmpvargridname,40);
  return tmpvargridname;
}

/******************************************************************************/
/*                        write_probdimagrif_file                             */
/******************************************************************************/
/* This subroutine is used to create the file probdim_agrif.h                 */
/******************************************************************************/
/*                                                                            */
/*               probdim_agrif.h                                              */
/*                                                                            */
/*               Agrif_probdim = <number>                                     */
/*                                                                            */
/******************************************************************************/
void write_probdimagrif_file()
{
  FILE *probdim;
  char ligne[LONG_C];

  probdim = open_for_write("probdim_agrif.h");
  sprintf (ligne, "Agrif_Probdim = %d", dimprob);
  tofich (probdim, ligne,1);
  fclose (probdim);
}

/******************************************************************************/
/*                             write_keysagrif_file                           */
/******************************************************************************/
/* This subroutine is used to create the file keys_agrif.h                    */
/******************************************************************************/
/*                                                                            */
/*               keys_agrif.h                                                 */
/*                                                                            */
/*               AGRIF_USE_FIXED_GRIDS = 0                                    */
/*               AGRIF_USE_ONLY_FIXED_GRIDS = 0                               */
/*               AGRIF_USE_(ONLY)_FIXED_GRIDS = 1                             */
/*                                                                            */
/******************************************************************************/
void write_keysagrif_file()
{
  FILE *keys;

  keys = open_for_write("keys_agrif.h");
  fprintf(keys,"      AGRIF_USE_FIXED_GRIDS = 0\n");
  fprintf(keys,"      AGRIF_USE_ONLY_FIXED_GRIDS = 0\n");
  if (fixedgrids     == 1) fprintf(keys,"      AGRIF_USE_FIXED_GRIDS = 1\n");
  if (onlyfixedgrids == 1)
                         fprintf(keys,"      AGRIF_USE_ONLY_FIXED_GRIDS = 1\n");

  fclose(keys);
}

/******************************************************************************/
/*                      write_modtypeagrif_file                               */
/******************************************************************************/
/* This subroutine is used to create the file typedata                        */
/******************************************************************************/
/*                                                                            */
/*               modtype_agrif.h                                              */
/*                                                                            */
/*               Agrif_NbVariables =                                          */
/*                                                                            */
/******************************************************************************/
void write_modtypeagrif_file()
{
  char ligne[LONG_C];
  FILE *typedata;

  typedata = open_for_write("modtype_agrif.h");
  /* AGRIF_NbVariables : number of variables                                  */
  sprintf (ligne, "AGRIF_NbVariables = %d",indicemaxtabvars);
  tofich(typedata,ligne,1);
  fclose (typedata);
}

/******************************************************************************/
/*                   write_createvarnameagrif_file                            */
/******************************************************************************/
/* This subroutine is used to create the file  createvarname                  */
/******************************************************************************/
/*                                                                            */
/*    Agrif_Gr % tabvars (i) % var % namevar = "variable"                     */
/*                                                                            */
/******************************************************************************/
void write_createvarnameagrif_file(variable *v,FILE *createvarname,
                                                       int *InitEmpty)
{
  char ligne[LONG_C];
  char *tmp;
  char temp1[LONG_C];

  tmp =  variablenametabvars(v,0);
  strcpy (temp1, tmp);
  if ( todebugfree == 1 ) free(tmp);

  *InitEmpty = 0 ;
  sprintf(ligne, "%s %% namevar = \"%s\"",temp1,v->v_nomvar);
  tofich(createvarname,ligne,1);
}

/******************************************************************************/
/*                        write_Setnumberofcells_file                         */
/******************************************************************************/
/* This subroutine is used to create the file  setnumberofcells               */
/******************************************************************************/
/*                                                                            */
/*              Agrif_Gr % n(i) = nbmailles                                   */
/*                                                                            */
/******************************************************************************/
void write_Setnumberofcells_file(char *name)
{
  char ligne[LONG_C];
  FILE *setnumberofcells;

  if ( IndicenbmaillesX != 0 )
  {
  setnumberofcells = open_for_write(name);

  if (onlyfixedgrids != 1 )
  {
  sprintf (ligne,
           "Agrif_Gr %% nb(1) = Agrif_Gr %% tabvars(%d) %% var %% iarray0",
           IndicenbmaillesX);
  }
  else
  {
  sprintf (ligne,
           "Agrif_Gr %% nb(1) = Agrif_Curgrid %% tabvars(%d) %% var %% iarray0",
           IndicenbmaillesX);
  }
  tofich (setnumberofcells, ligne,1);
  if (dimprob > 1)
  {
     if (onlyfixedgrids != 1 )
     {
     sprintf (ligne,
           "Agrif_Gr %% nb(2) = Agrif_Gr %% tabvars(%d) %% var %% iarray0",
           IndicenbmaillesY);
     }
     else
     {
     sprintf (ligne,
           "Agrif_Gr %% nb(2) = Agrif_Curgrid %% tabvars(%d) %% var %% iarray0",
           IndicenbmaillesY);
     }

     tofich (setnumberofcells, ligne,1);
  }
  if (dimprob > 2)
  {
     if (onlyfixedgrids != 1 )
     {
     sprintf (ligne,
           "Agrif_Gr %% nb(3) = Agrif_Gr %% tabvars(%d) %% var %% iarray0",
           IndicenbmaillesZ);
     }
     else
     {
     sprintf (ligne,
           "Agrif_Gr %% nb(3) = Agrif_Curgrid %% tabvars(%d) %% var %% iarray0",
           IndicenbmaillesZ);
     }
     tofich (setnumberofcells, ligne,1);
  }

  fclose (setnumberofcells);
  }
}

/******************************************************************************/
/*                       write_Getnumberofcells_file                          */
/******************************************************************************/
/* This subroutine is used to create the file  getnumberofcells               */
/******************************************************************************/
/*                                                                            */
/*              nbmailles = Agrif_Gr % n(i)                                   */
/*                                                                            */
/******************************************************************************/
void write_Getnumberofcells_file(char *name)
{
  char ligne[LONG_C];
  FILE *getnumberofcells;

  if ( IndicenbmaillesX != 0 )
  {
  getnumberofcells = open_for_write(name);
  sprintf (ligne,
           "Agrif_Curgrid %% tabvars(%d) %% var %% iarray0 = Agrif_Gr %% nb(1)",
           IndicenbmaillesX);
  tofich (getnumberofcells, ligne,1);
  if (dimprob > 1)
    {
      sprintf (ligne,
           "Agrif_Curgrid %% tabvars(%d) %% var %% iarray0 = Agrif_Gr %% nb(2)",
           IndicenbmaillesY);
      tofich (getnumberofcells, ligne,1);
    }
  if (dimprob > 2)
    {
      sprintf (ligne,
           "Agrif_Curgrid %% tabvars(%d) %% var %% iarray0 = Agrif_Gr %% nb(3)",
           IndicenbmaillesZ);
      tofich (getnumberofcells, ligne,1);
    }
  fclose (getnumberofcells);
  }
}


/******************************************************************************/
/*                      write_initialisationsagrif_file                       */
/******************************************************************************/
/* This subroutine is used to create the file initproc                        */
/******************************************************************************/
/*                                                                            */
/*              ! variable                                                    */
/*              Agrif_Gr % tabvars(i) % var % nbdim = 1                       */
/*                                                                            */
/******************************************************************************/
void write_initialisationsagrif_file(variable *v,FILE *initproc,
                                     int *VarnameEmpty)
{
  char ligne[LONG_C];
  char temp1[LONG_C];
  char *tmp;

  tmp = variablenameroottabvars (v);
  strcpy (temp1, tmp);
  if ( todebugfree == 1 ) free(tmp);

  if ( v->v_nbdim != 0 )
  {
     *VarnameEmpty = 0 ;
     sprintf (ligne, "%s %% nbdim = %d", temp1, v->v_nbdim);
     tofich (initproc, ligne,1);
  }
}


void Write_Alloc_Agrif_Files()
{
   listnom *parcours;
   FILE *alloccalls;
   FILE *AllocUSE;

   AllocUSE= open_for_write("include_use_Alloc_agrif.h");
   alloccalls = open_for_write("allocations_calls_agrif.h");

   parcours = List_Subroutine_For_Alloc;
   while ( parcours )
   {
      fprintf(AllocUSE,"      USE %s\n", parcours -> o_nom );
      fprintf (alloccalls,"      Call Alloc_agrif_%s(Agrif_Gr)\n",
                                                            parcours -> o_nom );
      parcours = parcours -> suiv;
   }

   fclose (AllocUSE);
   fclose (alloccalls);
}

int IndiceInlist(int indic, listindice *listin)
{
   listindice *parcoursindic;
   int out;

   out = 0 ;

   parcoursindic = listin;
   while ( parcoursindic && out == 0 )
   {
      if ( parcoursindic->i_indice == indic ) out = 1;
      else parcoursindic = parcoursindic -> suiv;
   }

   return out;
}
void write_allocation_Common_0()
{
   listnom *parcours_nom;
   listnom *neededparameter;
   listvar *parcours;
   listvar *parcoursprec;
   listvar *parcours1;
   FILE *allocationagrif;
   FILE *paramtoamr;
   char ligne[LONGNOM];
   char ligne2[LONGNOM];   
   variable *v;
   int IndiceMax;
   int IndiceMin;
   int compteur;
   int out;
   int indiceprec;
   int ValeurMax;
   char initialvalue[LONG_4C];
   listindice *list_indic;
   listindice *parcoursindic;
   int i;

   parcoursprec = (listvar *)NULL;
   parcours_nom = List_NameOfCommon;
   ValeurMax = 2;
   while ( parcours_nom  )
   {
      /*                                                                      */
      if ( parcours_nom->o_val == 1 )
      {
         /* Open the file to create the Alloc_agrif subroutine                */
         sprintf(ligne,"alloc_agrif_%s.h",parcours_nom->o_nom);
         allocationagrif = open_for_write(ligne);
         /*                                                                   */
         fprintf(allocationagrif,"#include \"Param_toamr_%s.h\" \n",
                                                           parcours_nom->o_nom);
         /*                                                                   */
         sprintf(ligne,"Param_toamr_%s.h",parcours_nom->o_nom);
         paramtoamr = open_for_write(ligne);
         neededparameter = (listnom * )NULL;
         /*                                                                   */
         list_indic = (listindice *)NULL;
         /*                                                                   */
         shouldincludempif = 1 ;
         parcours = List_Common_Var;
         while ( parcours )
         {
            if ( !strcasecmp(parcours->var->v_commonname,parcours_nom->o_nom) &&
                  IndiceInlist(parcours->var->v_indicetabvars,list_indic) == 0
               )
            {
               /***************************************************************/
               /***************************************************************/
               /***************************************************************/
               v = parcours->var;
               IndiceMax = 0;
               IndiceMin = indicemaxtabvars;
  /* body of the file                                                         */
  if ( !strcasecmp(v->v_commoninfile,mainfile) )
  {
     if (onlyfixedgrids != 1 && v->v_nbdim!=0)
     {
//        strcpy (ligne, "If (.not. associated(");
        strcpy (ligne, "If (.not. allocated(");
        strcat (ligne, vargridnametabvars(v,0));
        strcat (ligne, "))                       then");
        Save_Length(ligne,48);
        tofich (allocationagrif, ligne,1);
     }
     if ( v->v_allocatable != 1 && ( v->v_dimsempty != 1) )
     {
        /*                ALLOCATION                                          */
        if ( v->v_dimension != 0  )
        {
           if ( v->v_indicetabvars < IndiceMin ||
                v->v_indicetabvars > IndiceMax )
           {
              parcours1 = parcours;
              compteur = -1;
              out = 0;
              indiceprec = parcours->var->v_indicetabvars -1 ;
              while ( parcours1 && out == 0 &&
                      !strcasecmp(  parcours->var->v_readedlistdimension,
                                  parcours1->var->v_readedlistdimension) &&
                      !strcasecmp(  parcours->var->v_typevar,
                                  parcours1->var->v_typevar) &&
                            ( parcours1->var->v_indicetabvars == indiceprec+1 )
                     )
              {

               if ( !strcasecmp(parcours1->var->v_modulename,
                                parcours_nom->o_nom) ||
                    !strcasecmp(parcours1->var->v_commonname,
                                parcours_nom->o_nom) )
                 {
                      compteur = compteur +1 ;
                      indiceprec = parcours1->var->v_indicetabvars;
                      parcoursprec = parcours1;
                      parcours1 = parcours1->suiv;
                 }
                 else out = 1;
              }

              if ( compteur > ValeurMax )
              {
                 fprintf(allocationagrif,"      DO i = %d , %d\n",
                                         parcours->var->v_indicetabvars,
                                       parcours->var->v_indicetabvars+compteur);
                 IndiceMin = parcours->var->v_indicetabvars;
                 IndiceMax = parcours->var->v_indicetabvars+compteur;
                 strcpy (ligne, "allocate ");
                 strcat (ligne, "(");
                 strcat (ligne, vargridnametabvars(v,1));
                 strcat (ligne, vargridparam(v,0));
                 strcat (ligne, ")");
                 Save_Length(ligne,48);
                 tofich (allocationagrif, ligne,1);
                 fprintf(allocationagrif,"      end do\n");
                 i=parcours->var->v_indicetabvars;
                 do
                 {
                    parcoursindic =  (listindice *)malloc(sizeof(listindice));
                    parcoursindic -> i_indice = i;
                    parcoursindic -> suiv = list_indic;
                    list_indic = parcoursindic;
                    i = i + 1;
                 } while ( i <= parcours->var->v_indicetabvars+compteur );
                 parcours = parcoursprec;
                 /*                                                           */
              }
              else
              {
                 strcpy (ligne, "allocate ");
                 strcat (ligne, "(");
                 strcat (ligne, vargridnametabvars(v,0));
                 strcat (ligne, vargridparam(v,0));
                 strcat (ligne, ")");
                 Save_Length(ligne,48);
                 tofich (allocationagrif, ligne,1);
                 /*                                                           */
                 parcoursindic =  (listindice *)malloc(sizeof(listindice));
                 parcoursindic -> i_indice = parcours->var->v_indicetabvars;
                 parcoursindic -> suiv = list_indic;
                 list_indic = parcoursindic;
              }
                neededparameter = writedeclarationintoamr(List_Parameter_Var,
                              paramtoamr,v,parcours_nom->o_nom,neededparameter,
                                                               v->v_commonname);
              /*                                                              */
           }
        } /* end of the allocation part                                       */
        /*                INITIALISATION                                      */
        if ( strcasecmp(v->v_initialvalue,"") )
        {
           strcpy (ligne, "");
           strcat (ligne, vargridnametabvars(v,0));
           /* We should modify the initialvalue in the case of variable has   */
           /*    been defined with others variables                           */
                      
           strcpy(initialvalue,
                  ChangeTheInitalvaluebyTabvarsName
                                      (v->v_initialvalue,List_Global_Var,0));
           if ( !strcasecmp(initialvalue,v->v_initialvalue) )
           {
              strcpy(initialvalue,"");
              strcpy(initialvalue,ChangeTheInitalvaluebyTabvarsName
                                      (v->v_initialvalue,List_Common_Var,0));
           }
           if ( !strcasecmp(initialvalue,v->v_initialvalue) )
           {
              strcpy(initialvalue,"");
              strcpy(initialvalue,ChangeTheInitalvaluebyTabvarsName
                                     (v->v_initialvalue,List_ModuleUsed_Var,0));
           }
           strcat (ligne," = ");

           if (v->v_nbdim == 0)
           {
           strcpy(ligne2,initialvalue);
           }
           else
           {
           sprintf(ligne2,"RESHAPE(%s,SHAPE(%s))",initialvalue,vargridnametabvars(v,0));
           }
           strcat (ligne,ligne2);
           /*                                                                 */
           Save_Length(ligne,48);
           tofich (allocationagrif, ligne,1);
        }
     }
     if (onlyfixedgrids != 1 && v->v_nbdim!=0)
     {
        strcpy (ligne, "   End if");
        tofich (allocationagrif, ligne,1);
     }
  }
               /***************************************************************/
               /***************************************************************/
               /***************************************************************/
            }
            parcours = parcours -> suiv;
         }
         /* Close the file Alloc_agrif                                        */
         fclose(allocationagrif);
         fclose(paramtoamr);
      }
      /*                                                                      */
      parcours_nom = parcours_nom -> suiv;
   }

}



void write_allocation_Global_0()
{
   listnom *parcours_nom;
   listvar *parcours;
   listvar *parcoursprec;
   listvar *parcours1;
   FILE *allocationagrif;
   char ligne[LONGNOM];
   variable *v;
   int IndiceMax;
   int IndiceMin;
   int compteur;
   int out;
   int indiceprec;
   int ValeurMax;
   char initialvalue[LONG_4C];
   int typeiswritten ;

   parcoursprec = (listvar *)NULL;
   parcours_nom = List_NameOfModule;
   ValeurMax = 2;
   while ( parcours_nom  )
   {
      /*                                                                      */
      if ( parcours_nom->o_val == 1 )
      {
         IndiceMax = 0;
         IndiceMin = indicemaxtabvars;
         /* Open the file to create the Alloc_agrif subroutine                */
         sprintf(ligne,"alloc_agrif_%s.h",parcours_nom->o_nom);
         allocationagrif = open_for_write(ligne);
         /*                                                                   */
         if ( ModuleIsDefineInInputFile(parcours_nom->o_nom) == 1 )
         {
             /* add the call to initworkspace                                 */
            tofich(allocationagrif,"if ( .NOT. Agrif_Root() ) then ",1);
            fprintf(allocationagrif,"#include \"GetNumberofcells.h\" \n");
            tofich(allocationagrif,"else ",1);
            fprintf(allocationagrif,"#include \"SetNumberofcells.h\" \n");
            tofich(allocationagrif,"endif ",1);
            tofich(allocationagrif,"Call Agrif_InitWorkspace ",1);
         }

         typeiswritten = 0;

         parcours = List_Global_Var;
         while ( parcours )
         {
            if ( !strcasecmp(parcours->var->v_modulename,parcours_nom->o_nom) &&
                 parcours->var->v_VariableIsParameter == 0                  &&
                 parcours->var->v_notgrid == 0                              &&
                 !strcasecmp(parcours->var->v_modulename,parcours_nom->o_nom)  )
            {
               /***************************************************************/
               /***************************************************************/
               /***************************************************************/
               v = parcours->var;
               IndiceMax = 0;
               IndiceMin = indicemaxtabvars;
  /* body of the file                                                         */
  if ( !strcasecmp(v->v_commoninfile,mainfile) )
  {
     if (onlyfixedgrids != 1 && v->v_nbdim!=0)
     {
//        strcpy (ligne, "If (.not. associated(");
        strcpy (ligne, "If (.not. allocated(");
        strcat (ligne, vargridnametabvars(v,0));
        strcat (ligne, "))                       then");
        Save_Length(ligne,48);
        tofich (allocationagrif, ligne,1);
     }
     if ( v->v_allocatable != 1 && ( v->v_dimsempty != 1) )
     {
        /*                ALLOCATION                                          */
        if ( v->v_dimension != 0  )
        {
           if ( v->v_indicetabvars < IndiceMin ||
                v->v_indicetabvars > IndiceMax )
           {
              parcours1 = parcours;
              compteur = -1;
              out = 0;
              indiceprec = parcours->var->v_indicetabvars -1 ;
              while ( parcours1 && out == 0 &&
                      !strcasecmp(  parcours->var->v_readedlistdimension,
                                  parcours1->var->v_readedlistdimension) &&
                      !strcasecmp(  parcours->var->v_typevar,
                                  parcours1->var->v_typevar) &&
                             ( parcours1->var->v_indicetabvars == indiceprec+1 )
                     )
              {

               if ( !strcasecmp(parcours1->var->v_modulename,
                                parcours_nom->o_nom) ||
                    !strcasecmp(parcours1->var->v_commonname,
                                parcours_nom->o_nom) )
                 {
                      compteur = compteur +1 ;
                      indiceprec = parcours1->var->v_indicetabvars;
                      parcoursprec = parcours1;
                      parcours1 = parcours1->suiv;
                 }
                 else out = 1;
              }
              if ( compteur > ValeurMax )
              {
                 fprintf(allocationagrif,"      DO i = %d , %d\n",
                                          parcours->var->v_indicetabvars,
                                       parcours->var->v_indicetabvars+compteur);
                 IndiceMin = parcours->var->v_indicetabvars;
                 IndiceMax = parcours->var->v_indicetabvars+compteur;
                 strcpy (ligne, "allocate ");
                 strcat (ligne, "(");
                 strcat (ligne, vargridnametabvars(v,1));
                 strcat (ligne, vargridparam(v,0));
                 strcat (ligne, ")");
                 Save_Length(ligne,48);
                 tofich (allocationagrif, ligne,1);
                 fprintf(allocationagrif,"      end do\n");
                 parcours = parcoursprec;
              }
              else
              {
                 strcpy (ligne, "allocate ");
                 strcat (ligne, "(");
                 strcat (ligne, vargridnametabvars(v,0));
                 strcat (ligne, vargridparam(v,0));
                 strcat (ligne, ")");
                 Save_Length(ligne,48);
                 tofich (allocationagrif, ligne,1);
              }
           }
        } /* end of the allocation part                                       */

        /*                INITIALISATION                                      */
        if ( strcasecmp(v->v_initialvalue,"") )
        {
           strcpy (ligne, "");
           strcat (ligne, vargridnametabvars(v,0));
           /* We should modify the initialvalue in the case of variable has   */
           /*    been defined with others variables                           */

           strcpy(initialvalue,
                  ChangeTheInitalvaluebyTabvarsName
                                      (v->v_initialvalue,List_Global_Var,0));
           if ( !strcasecmp(initialvalue,v->v_initialvalue) )
           {
              strcpy(initialvalue,"");
              strcpy(initialvalue,ChangeTheInitalvaluebyTabvarsName
                                      (v->v_initialvalue,List_Common_Var,0));
           }
           if ( !strcasecmp(initialvalue,v->v_initialvalue) )
           {
              strcpy(initialvalue,"");
              strcpy(initialvalue,ChangeTheInitalvaluebyTabvarsName
                                     (v->v_initialvalue,List_ModuleUsed_Var,0));
           }
           strcat (ligne," = ");
           strcat (ligne,initialvalue);
           /*                                                                 */
           Save_Length(ligne,48);
           tofich (allocationagrif, ligne,1);
        }
     }
/* Case of structure types */
        if ((typeiswritten == 0) && !strcasecmp(v->v_typevar,"type"))
        {
        sprintf(ligne,"If (.Not.Allocated(Agrif_%s_var)) Then",v->v_modulename);
        tofich(allocationagrif, ligne, 1);
        sprintf(ligne,"Allocate(Agrif_%s_var(0:Agrif_NbMaxGrids))",v->v_modulename);
        tofich(allocationagrif, ligne, 1);
        strcpy(ligne,"End If");
        tofich(allocationagrif, ligne, 1);
        typeiswritten = 1;
        }
     if (onlyfixedgrids != 1 && v->v_nbdim!=0)
     {
        strcpy (ligne, "   End if");
        tofich (allocationagrif, ligne,1);
     }
  }
               /***************************************************************/
               /***************************************************************/
               /***************************************************************/
            }
            parcours = parcours -> suiv;
         }
         /*                                                                   */
         if ( ModuleIsDefineInInputFile(parcours_nom->o_nom) == 1 )
         {
            /* add the call to initworkspace                                  */
            tofich(allocationagrif,"if ( .NOT. Agrif_Root() ) then ",1);
            fprintf(allocationagrif,"#include \"GetNumberofcells.h\" \n");
            tofich(allocationagrif,"else ",1);
            fprintf(allocationagrif,"#include \"SetNumberofcells.h\" \n");
            tofich(allocationagrif,"endif ",1);
            tofich(allocationagrif,"Call Agrif_InitWorkspace ",1);
         }
         /* Close the file Alloc_agrif                                        */
         fclose(allocationagrif);
      } /* end parcours_nom == 1                                              */
      /*                                                                      */
      parcours_nom = parcours_nom -> suiv;
   }
}

/******************************************************************************/
/*                           creefichieramr                                   */
/******************************************************************************/
/* This subroutine is the main one to create AGRIF_INC files                  */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void creefichieramr (const char *NameTampon)
{
  listvar *newvar;
  variable *v;
  int erreur;
  char filefich[LONG_C];
  char ligne[LONG_C];
  int IndiceMax;
  int IndiceMin;
  int InitEmpty;
  int VarnameEmpty;
  int donotwrite;

  FILE *initproc;
  FILE *initglobal;
  FILE *createvarname;
  FILE *createvarnameglobal;

  if ( todebug == 1 ) printf("Enter in creefichieramr\n");
  strcpy (filefich, "cd ");
  strcat (filefich, nomdir);
  erreur = system (filefich);
  if (erreur)
  {
     strcpy (filefich, "mkdir ");
     strcat (filefich, nomdir);
     system (filefich);
     printf ("%s: Directory created\n", nomdir);
  }

/******************************************************************************/
/******************** Creation of AGRIF_INC files *****************************/
/******************************************************************************/

/*----------------------------------------------------------------------------*/
  if ( todebug == 1 )
  {
     strcpy(ligne,"initialisations_agrif_");
     strcat(ligne,NameTampon);
     strcat(ligne,".h");
     initproc = open_for_write(ligne);
/*----------------------------------------------------------------------------*/
     strcpy(ligne,"createvarname_agrif_");
     strcat(ligne,NameTampon);
     strcat(ligne,".h");
     createvarname = open_for_write(ligne);
/*----------------------------------------------------------------------------*/
     InitEmpty = 1 ;
     VarnameEmpty = 1 ;

     newvar = List_Global_Var;
     while ( newvar && todebug == 1 )
     {
        donotwrite = 0;
        v = newvar->var;

        if ( ( v->v_common == 1 || v->v_module == 1 ) && donotwrite == 0 )
        {
          write_createvarnameagrif_file(v,createvarname,&VarnameEmpty);
          write_initialisationsagrif_file(v,initproc,&InitEmpty);
        }
        newvar = newvar->suiv;
     }
  /*                                                                          */
     fclose (createvarname);
     fclose (initproc);
  /*--------------------------------------------------------------------------*/
     if ( Did_filetoparse_readed(curmodulename) == 0 )
     {
        if ( InitEmpty != 1  )
        {
           initglobal = open_for_append("initialisations_agrif.h");
           strcpy(ligne,"#include \"initialisations_agrif_");
           strcat(ligne,NameTampon);
           strcat(ligne,".h\"\n");
           fprintf(initglobal,ligne);
           fclose(initglobal);
        }
  /*--------------------------------------------------------------------------*/
        if ( VarnameEmpty != 1 )
        {
           createvarnameglobal= open_for_append("createvarname_agrif.h");
           strcpy(ligne,"#include \"createvarname_agrif_");
           strcat(ligne,NameTampon);
           strcat(ligne,".h\"\n");
           fprintf(createvarnameglobal,ligne);
           fclose(createvarnameglobal);
        }
     }
  }
/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/
  IndiceMax = 0;
  IndiceMin = 0;

  write_allocation_Common_0();
  write_allocation_Global_0();

  Write_Alloc_Agrif_Files();
  write_probdimagrif_file();
  write_keysagrif_file();
  write_modtypeagrif_file();
  if ( NbMailleXDefined == 1 )
                             write_Setnumberofcells_file("SetNumberofcells.h");
  if ( NbMailleXDefined == 1 )
                             write_Getnumberofcells_file("GetNumberofcells.h");
  retour77 = 0;
  if ( NbMailleXDefined == 1 )
                          write_Setnumberofcells_file("SetNumberofcellsFree.h");
  if ( NbMailleXDefined == 1 )
                          write_Getnumberofcells_file("GetNumberofcellsFree.h");
  retour77 = 1;
  if ( NbMailleXDefined == 1 )
                         write_Setnumberofcells_file("SetNumberofcellsFixed.h");
  if ( NbMailleXDefined == 1 )
                         write_Getnumberofcells_file("GetNumberofcellsFixed.h");
  if ( todebug == 1 ) printf("Out of creefichieramr\n");
}
