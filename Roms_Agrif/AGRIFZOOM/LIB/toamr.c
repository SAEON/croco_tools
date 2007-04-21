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
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "decl.h"
char lvargridname[LONGNOM];
char lvargridname2[LONGNOM];


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

  ligne = (char *) malloc (LONGLIGNE * sizeof (char));
  sprintf (ligne, "Agrif_Mygrid %% tabvars(%d) %% var ", var->indicetabvars);
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

  ligne = (char *) malloc (LONGLIGNE * sizeof (char));
  if ( iorindice == 0 ) sprintf (ligne, " Agrif_Gr %% tabvars(%d)%% var",
                                 var->indicetabvars);
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

  ligne = (char *) malloc (LONGLIGNE * sizeof (char));
  if ( ParentOrCurgrid == 0 ) sprintf (ligne, " Agrif_tabvars(%d) %% var",
                              var->indicetabvars);
  else if ( ParentOrCurgrid == 1 ) sprintf (ligne, 
                              " Agrif_tabvars(%d) %% parent_var %% var",
                               var->indicetabvars);
  else if ( ParentOrCurgrid == 2 ) sprintf (ligne, 
                              " Agrif_Mygrid %% tabvars(%d) %% var",
                               var->indicetabvars);
  else if ( ParentOrCurgrid == 3 ) sprintf (ligne, 
                              " Agrif_Curgrid %% tabvars(%d) %% var",
                               var->indicetabvars);
  else sprintf (ligne, " AGRIF_Mygrid %% tabvars(%d) %% var",
                               var->indicetabvars);
  return ligne;
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
  char tmp1[LONGNOM];
  
  tmp = variablenametabvars (var,iorindice);
  strcpy(tmp1,tmp);
  if ( todebugfree == 1 ) free(tmp);
  
  sprintf (lvargridname, "%s", tmp1);
  if (!strcasecmp (var->typevar, "REAL"))
    {
      if ( !strcasecmp(var->nameinttypename,"8") ) sprintf (lvargridname2, "%% darray%d", var->nbdim);
      else sprintf (lvargridname2, "%% array%d", var->nbdim);
    }
  else if (!strcasecmp (var->typevar, "INTEGER"))
    {
      sprintf (lvargridname2, "%% iarray%d", var->nbdim);
    }
  else if (!strcasecmp (var->typevar, "LOGICAL"))
    {
      sprintf (lvargridname2, "%% larray%d", var->nbdim);
    }
  else if (!strcasecmp (var->typevar, "CHARACTER"))
    {
      sprintf (lvargridname2, "%% carray%d", var->nbdim);
    }

  strcat (lvargridname, lvargridname2);

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
  char tmp1[LONGNOM];
  
  tmp = variablecurgridtabvars (var,ParentOrCurgrid);
  strcpy(tmp1,tmp);
  if ( todebugfree == 1 ) free(tmp);
  
  sprintf (lvargridname, "%s", tmp1);
  if (!strcasecmp (var->typevar, "REAL"))
    {
      if ( !strcasecmp(var->nameinttypename,"8") ) sprintf (lvargridname2, "%% darray%d", var->nbdim);
      else sprintf (lvargridname2, "%% array%d", var->nbdim);
    }
  else if (!strcasecmp (var->typevar, "INTEGER"))
    {
      sprintf (lvargridname2, "%% iarray%d", var->nbdim);
    }
  else if (!strcasecmp (var->typevar, "LOGICAL"))
    {
      sprintf (lvargridname2, "%% larray%d", var->nbdim);
    }
  else if (!strcasecmp (var->typevar, "CHARACTER"))
    {
      sprintf (lvargridname2, "%% carray%d", var->nbdim);
    }

  strcat (lvargridname, lvargridname2);

  return lvargridname;
}

/******************************************************************************/
/*                           vargridcurgridtabvarswithoutAgrif_Gr                            */
/******************************************************************************/
/* This subroutine is used to create the string                               */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
char *vargridcurgridtabvarswithoutAgrif_Gr (variable * var)
{
    
  sprintf (lvargridname, "(%d) %% var", var->indicetabvars);

  if (!strcasecmp (var->typevar, "REAL"))
    {
      if ( !strcasecmp(var->nameinttypename,"8") ) sprintf (lvargridname2, "%% darray%d", var->nbdim);
      else sprintf (lvargridname2, "%% array%d", var->nbdim);
    }
  else if (!strcasecmp (var->typevar, "INTEGER"))
    {
      sprintf (lvargridname2, "%% iarray%d", var->nbdim);
    }
  else if (!strcasecmp (var->typevar, "LOGICAL"))
    {
      sprintf (lvargridname2, "%% larray%d", var->nbdim);
    }
  else if (!strcasecmp (var->typevar, "CHARACTER"))
    {
      sprintf (lvargridname2, "%% carray%d", var->nbdim);
    }

  strcat (lvargridname, lvargridname2);

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
  char newname[LONGNOM];
    
  newdim = v->dimension;
  if (!newdim) return "";

  strcpy (tmpvargridname, "(");
  while (newdim)
  {
     dim = newdim->dim;

     strcpy(newname,"");
     strcpy(newname, 
            ChangeTheInitalvaluebyTabvarsName(dim.first,globliste,whichone));   
     if ( !strcasecmp(newname,dim.first))
     {
        strcpy(newname,"");     
        /* la liste des use de cette subroutine                               */
        if ( !globalvarofusefile ) RecordUseModulesVariables();
        strcpy(newname,ChangeTheInitalvaluebyTabvarsName(dim.first,
                       globalvarofusefile,whichone));
     }
     strcat (tmpvargridname, newname);
     strcat (tmpvargridname, " : ");
     strcpy(newname,"");
     strcpy(newname,ChangeTheInitalvaluebyTabvarsName
                        (dim.last,globliste,whichone));   
     if ( !strcasecmp(newname,dim.last))
     {
        strcpy(newname,"");     
        /* la liste des use de cette subroutine                               */
        if ( !globalvarofusefile ) RecordUseModulesVariables();
        strcpy(newname,ChangeTheInitalvaluebyTabvarsName
                       (dim.last, globalvarofusefile,whichone));
     }
     strcat (tmpvargridname, newname);
     newdim = newdim->suiv;
     if (newdim) strcat (tmpvargridname, ",");
  }
  strcat (tmpvargridname, ")");
  strcat (tmpvargridname, "\0");
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
  char ligne[LONGLIGNE*100];
  
  probdim = associate("probdim_agrif.h");
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

  keys = associate ("keys_agrif.h");
  fprintf(keys,"      AGRIF_USE_FIXED_GRIDS = 0\n");
  fprintf(keys,"      AGRIF_USE_ONLY_FIXED_GRIDS = 0\n");
  if (fixedgrids     == 1) fprintf(keys,"      AGRIF_USE_FIXED_GRIDS = 1\n");
  if (onlyfixedgrids == 1) fprintf(keys,"      AGRIF_USE_ONLY_FIXED_GRIDS = 1\n");

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
  char ligne[LONGLIGNE*100];
  FILE *typedata;

  typedata = associate ("modtype_agrif.h");
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
  char ligne[LONGLIGNE*100];
  char *tmp;
  char temp1[LONGLIGNE];
  
  tmp =  variablenametabvars(v,0);
  strcpy (temp1, tmp);
  if ( todebugfree == 1 ) free(tmp);

  *InitEmpty = 0 ;
  sprintf(ligne, "%s %% namevar = \"%s\"",temp1,v->nomvar);
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
void write_Setnumberofcells_file()
{
  char ligne[LONGLIGNE*100];
  FILE *setnumberofcells;

  setnumberofcells=associate("SetNumberofcells.h");
  
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

/******************************************************************************/
/*                       write_Getnumberofcells_file                          */
/******************************************************************************/
/* This subroutine is used to create the file  getnumberofcells               */
/******************************************************************************/
/*                                                                            */
/*              nbmailles = Agrif_Gr % n(i)                                   */
/*                                                                            */
/******************************************************************************/
void write_Getnumberofcells_file()
{
  char ligne[LONGLIGNE*100];
  FILE *getnumberofcells;

  getnumberofcells=associate("GetNumberofcells.h");
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
  char ligne[LONGLIGNE*100];
  char temp1[LONGLIGNE];
  char *tmp;

  tmp = variablenameroottabvars (v);
  strcpy (temp1, tmp);
  if ( todebugfree == 1 ) free(tmp);

  if ( v->nbdim != 0 ) 
  {
     *VarnameEmpty = 0 ;
     sprintf (ligne, "%s %% nbdim = %d", temp1, v->nbdim);
     tofich (initproc, ligne,1);
  }
}

/******************************************************************************/
/*                        write_allocation                                    */
/******************************************************************************/
/* This subroutine is used to create the file allocationagrif                 */
/******************************************************************************/
/*                                                                            */
/*                 allocations_calls_agrif.h                                  */
/*                 Call Alloc_agrif_module (Agrif_Gr)                         */
/*                                                                            */
/*                 alloc_agrif_module.h                                       */
/*                 Subroutine Alloc_agrif_module (Agrif_Gr)                   */
/*                 allocate(Agrif_Gr%tabvars(i)%var%array1(jpi)               */
/*                 variable =>Agrif_Gr%tabvars(i)%var%array1                  */
/*                                                                            */
/******************************************************************************/
listnom *write_allocation(listvar *newvar,variable *v,
                          listnom *listedesnoms,
                          FILE *alloccalls,
                          FILE *AllocUSE,
                          FILE *modulealloc,
                          int *IndiceMax)
{
  char ligne[LONGLIGNE*100];
  char curname[LONGNOM];
  char initialvalue[LONGNOM];
  char name1[LONGNOM];
  listvar *parcours;
  listnom *parcoursnom;
  int compteur;
  int ValeurMax;
  int donotwrite=0;
  int out;
  FILE *alloc_agrif;
  
  ValeurMax = 2;
  if (v->common == 1) strcpy(curname,v->commonname);     
  if (v->module == 1) strcpy(curname,v->modulename);  
  
  if ( strcasecmp(curname,Alloctreatedname) )
  {
     strcpy(Alloctreatedname,curname);     

/******************************************************************************/
/*                 alloc_agrif_module.h                                       */
/*                 Subroutine Alloc_agrif_module (Agrif_Gr)                   */
/******************************************************************************/
     if ( v->common == 1 ) strcpy(name1,v->commonname);
     else if ( v->module == 1 )  strcpy(name1,v->modulename);
     else exit(1);

     sprintf(ligne,"alloc_agrif_%s.h",name1);
     allocationagrif = associate (ligne);

     *IndiceMax = 0;
     AllocEmpty = 1;

     sprintf (ligne, "Subroutine Alloc_agrif_%s(Agrif_Gr)", curname);
     tofich(allocationagrif,ligne,1);
     if ( ModuleIsDefineInInputFile(curname) == 1 )
     {
        strcpy(ligne,"Use Agrif_Util");
        tofich(allocationagrif,ligne,1);
     }
     else
     {
        if ( fortran77 == 1 )
        {
           strcpy(ligne,"Use Agrif_Types, ONLY : Agrif_tabvars");
           tofich(allocationagrif,ligne,1);
           strcpy(ligne,"Use Agrif_Types, ONLY : Agrif_grid");
           tofich(allocationagrif,ligne,1);
        }
     }
     strcpy (ligne, "Type(Agrif_grid), Pointer :: Agrif_Gr");
     tofich(allocationagrif,ligne,1);
     /* Add the declaration of I into the allocationagrif file                */
     strcpy(ligne, "INTEGER :: i");
     tofich (allocationagrif, ligne,1);

     if ( fortran77 == 1 )
     {
        writedeclarationintoamr(parameterlist,allocationagrif,newvar,curname);
     }
     if ( ModuleIsDefineInInputFile(curname) == 1 )
     {
         /* add the call to initworkspace                                     */
         tofich(allocationagrif,"if ( .NOT. Agrif_Root() ) then ",1);
         fprintf(allocationagrif,"#include \"GetNumberofcells.h\" \n");
         tofich(allocationagrif,"else ",1);
         fprintf(allocationagrif,"#include \"SetNumberofcells.h\" \n");
         tofich(allocationagrif,"endif ",1);
         tofich(allocationagrif,"Call Agrif_InitWorkspace ",1);
     }
  }
  /* body of the file                                                         */
  if ( !strcasecmp(v->commonname,Alloctreatedname) ||
       !strcasecmp(v->modulename,Alloctreatedname) )
  {
     if (onlyfixedgrids != 1 && v->nbdim!=0) 
     {
        strcpy (ligne, "If (.not. associated(");
        strcat (ligne, vargridnametabvars(v,0));
        strcat (ligne, "))                       then");
        tofich (allocationagrif, ligne,1);
        AllocEmpty = 0;
     }
     if ( v->allocatable != 1 && ( v->dimsempty != 1) )
     {
        /*                ALLOCATION                                          */
        if ( v->dimension != 0  )
        {
           if ( v->indicetabvars > *IndiceMax )
           {
              parcours = newvar;
              compteur = -1;
	      out = 0;
              while ( parcours && out == 0 &&
                      !strcasecmp(  newvar->var->readedlistdimension,
                                  parcours->var->readedlistdimension) &&
                      !strcasecmp(  newvar->var->typevar,
		                  parcours->var->typevar) )
              {
	         if ( fortran77 == 1 )
		 {
		    if ( !strcasecmp(parcours->var->commonname,curname) ) compteur = compteur +1 ;
		    else out = 1;
		 }
		 else
		 {
		    if ( !strcasecmp(parcours->var->modulename,curname) ) compteur = compteur +1 ;		 
		    else out = 1;
		 }
                 parcours = parcours->suiv;
              }
              if ( compteur > ValeurMax )
              {
                 fprintf(allocationagrif,"      DO i = %d , %d\n", 
                                          newvar->var->indicetabvars,
                                          newvar->var->indicetabvars+compteur);
                *IndiceMax = newvar->var->indicetabvars+compteur;
                 strcpy (ligne, "allocate ");
                 strcat (ligne, "(");
                 strcat (ligne, vargridnametabvars(v,1));
                 strcat (ligne, vargridparam(v,0));
                 strcat (ligne, ")");
                 tofich (allocationagrif, ligne,1);
                 fprintf(allocationagrif,"      end do\n");           
                 AllocEmpty = 0;
              }
              else
              {
                 strcpy (ligne, "allocate ");
                 strcat (ligne, "(");
                 strcat (ligne, vargridnametabvars(v,0));
                 strcat (ligne, vargridparam(v,0));
                 strcat (ligne, ")");
                 tofich (allocationagrif, ligne,1); 
                 AllocEmpty = 0;
              }
           }
        } /* end of the allocation part                                       */
        /*                INITIALISATION                                      */
        if ( strcasecmp(v->initialvalue,"") ) 
        {
           strcpy (ligne, "");
           strcat (ligne, vargridnametabvars(v,0));
           /* We should modify the initialvalue in the case of variable has   */
           /*    benn defined with others variables                           */
           strcpy(initialvalue,
                  ChangeTheInitalvaluebyTabvarsName
                                      (v->initialvalue,globliste,0));
           if ( !strcasecmp(initialvalue,v->initialvalue) )
           {
              strcpy(initialvalue,"");     
              /* la liste des use de cette subroutine                         */
              if ( !globalvarofusefile ) RecordUseModulesVariables();
              strcpy(initialvalue,ChangeTheInitalvaluebyTabvarsName
                                      (v->initialvalue,globalvarofusefile,0));
           }
           strcat (ligne," = "); 
           strcat (ligne,initialvalue); 
           /*                                                                 */
           tofich (allocationagrif, ligne,1);
           AllocEmpty = 0;
        }
     }
     if (onlyfixedgrids != 1 && v->nbdim!=0) 
     {
        strcpy (ligne, "   End if");
        tofich (allocationagrif, ligne,1);
     }     
  } 
  /* closing of the file                                                      */
  if ( newvar->suiv == NULL ||
       ( v->common == 1 &&
         strcasecmp(newvar->suiv->var->commonname,Alloctreatedname) ) ||
       ( v->module == 1 &&
         strcasecmp(newvar->suiv->var->modulename,Alloctreatedname) ) 
     )
  {
     if ( ModuleIsDefineInInputFile(curname) == 1 )
     {
         /* add the call to initworkspace                                     */
         tofich(allocationagrif,"if ( .NOT. Agrif_Root() ) then ",1);
         fprintf(allocationagrif,"#include \"GetNumberofcells.h\" \n");
         tofich(allocationagrif,"else ",1);
         fprintf(allocationagrif,"#include \"SetNumberofcells.h\" \n");
         tofich(allocationagrif,"endif ",1);
         tofich(allocationagrif,"Call Agrif_InitWorkspace ",1);
     }
     strcpy (ligne, "Return");
     tofich(allocationagrif,ligne,1);
     sprintf (ligne, "End Subroutine Alloc_agrif_%s",curname);
     tofich(allocationagrif,ligne,1);
     fclose(allocationagrif);
     allocationagrif = (FILE *)NULL;

/******************************************************************************/
/*                 NewModule_module.h                                         */
/*                 module <module> ETC ...                                    */
/******************************************************************************/
     if ( fortran77 == 1 )
     {
        donotwrite = 0 ;
        if ( strcasecmp(v->commoninfile,mainfile)) donotwrite = 1 ;
        else 
        {
           /* we should verify that this module has not been write in this fil*/
           parcoursnom = listedesnoms;
           sprintf(ligne,"USE %s",curname);
           while ( parcoursnom && donotwrite == 0 )
           {
              if ( !strcasecmp(parcoursnom->nom,ligne) ) donotwrite = 1;
              else parcoursnom = parcoursnom ->suiv;
           }
        }

        if ( donotwrite == 0 ) 
        {
           if ( ModuleIsDefineInInputFile(curname) == 1 ) AllocEmpty = 0;
           if ( AllocEmpty == 1 )
           {
            sprintf (ligne, "\n#include \"agrif_alloc_%s.h\"\n", curname);
            fprintf(alloccalls,ligne);
            sprintf (ligne, "agrif_alloc_%s.h", curname);
            alloc_agrif = associate(ligne);
            sprintf (ligne, "!", curname);
            tofich (alloc_agrif, ligne,1);

            fprintf(modulealloc,"! empty module alloc %s \n",curname );
	    fclose(alloc_agrif);
           }
           else
           {
/******************************************************************************/
/*                 include_use_Alloc_agrif.h                               */
/*                 USE mod                                                    */
/******************************************************************************/
            sprintf (ligne, "USE %s", curname);
            tofich (AllocUSE, ligne,1);
/******************************************************************************/
/*                 allocations_calls_agrif.h                                  */
/*                 Call Alloc_agrif_module (Agrif_Gr)                         */
/******************************************************************************/
            sprintf (ligne, "\n#include \"agrif_alloc_%s.h\"\n", curname);
            fprintf(alloccalls,ligne);
            sprintf (ligne, "agrif_alloc_%s.h", curname);
            alloc_agrif = associate(ligne);
            sprintf (ligne, "Call Alloc_agrif_%s(Agrif_Gr)", curname);
            tofich (alloc_agrif, ligne,1);
	    fclose(alloc_agrif);
/******************************************************************************/
/******************************************************************************/
            fprintf(modulealloc,"      module %s \n",curname);
            fprintf(modulealloc,"      IMPLICIT NONE \n");
            fprintf(modulealloc,"      PUBLIC Alloc_agrif_%s \n",curname);
            fprintf(modulealloc,"      CONTAINS \n");
            fprintf(modulealloc,"#include \"alloc_agrif_%s.h\" \n",curname);
            fprintf(modulealloc,"      end module %s \n",curname);
/******************************************************************************/
/******************************************************************************/
            sprintf(ligne,"USE %s",curname);
            listedesnoms = Addtolistnom(ligne,listedesnoms);
           }
        }
     }
        else
        {
           if ( Did_filetoparse_treated == 0 && AllocEmpty == 0 )
           {
/******************************************************************************/
/*                 include_use_Alloc_agrif.h                                  */
/*                 USE mod                                                    */
/******************************************************************************/
              sprintf (ligne, "USE %s", curname);
              tofich (AllocUSE, ligne,1);
/******************************************************************************/
/*                 allocations_calls_agrif.h                                  */
/*                 Call Alloc_agrif_module (Agrif_Gr)                         */
/******************************************************************************/
              sprintf (ligne, "#include \"agrif_alloc_%s.h\" \n", curname);
              fprintf (alloccalls, ligne);
              sprintf (ligne, "agrif_alloc_%s.h", curname);
              alloc_agrif = associate(ligne);
              sprintf (ligne, "Call Alloc_agrif_%s(Agrif_Gr)", curname);
              tofich (alloc_agrif, ligne,1);
	      fclose(alloc_agrif);
           }
        }
  }
  return listedesnoms;
}

/******************************************************************************/
/*                           creefichieramr                                   */
/******************************************************************************/
/* This subroutine is the main one to create AGRIF_INC files                  */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void creefichieramr (char *NameTampon)
{
  listvar *newvar;
  variable *v;
  int erreur;
  char filefich[LONGNOM];
  char ligne[LONGNOM];
  int IndiceMax;
  int InitEmpty;
  int VarnameEmpty;
  int donotwrite;
  listnom *listedesnoms;
  
  FILE *initproc;
  FILE *initglobal;
  FILE *createvarname;
  FILE *createvarnameglobal;
  FILE *alloccalls;
  FILE *AllocUSE;
  FILE *modulealloc;
  
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
  /*--------------------------------------------------------------------------*/
  /*   Record the list of module used in the file include_use_Alloc_agrif     */
  listedesnoms = (listnom *)NULL;

  strcpy(Alloctreatedname,"");     
/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/
/*----------------------------------------------------------------------------*/
  if ( todebug == 1 )
  {
     strcpy(ligne,"initialisations_agrif_");
     strcat(ligne,NameTampon);
     strcat(ligne,".h");
     initproc = associate (ligne);
/*----------------------------------------------------------------------------*/
     strcpy(ligne,"createvarname_agrif_");
     strcat(ligne,NameTampon);
     strcat(ligne,".h");
     createvarname = associate (ligne);
/*----------------------------------------------------------------------------*/
     InitEmpty = 1 ;
     VarnameEmpty = 1 ;

     newvar = globliste;
     while ( newvar && todebug == 1 )
     {
        donotwrite = 0;
        v = newvar->var;

        if ( fortran77 == 1 )
        {
           if ( v->indicetabvars <= oldindicemaxtabvars ) donotwrite = 1;
        }

        if ( ( v->common == 1 || v->module == 1 ) && donotwrite == 0 )
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
     if ( Did_filetoparse_treated == 0 )  
     {
        if ( InitEmpty != 1  )
        {
           initglobal = associateaplus("initialisations_agrif.h");
           strcpy(ligne,"#include \"initialisations_agrif_");
           strcat(ligne,NameTampon);
           strcat(ligne,".h\"\n");
           fprintf(initglobal,ligne);
           fclose(initglobal);     
        }
  /*--------------------------------------------------------------------------*/
        if ( VarnameEmpty != 1 )
        {
           createvarnameglobal= associateaplus("createvarname_agrif.h");
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
  AllocUSE= associateaplus("include_use_Alloc_agrif.h");
  alloccalls = associateaplus("allocations_calls_agrif.h");
  convert2lower(NameTampon);
  sprintf(ligne,"NewModule_%s.h",NameTampon);
  modulealloc=associate(ligne);
  /*--------------------------------------------------------------------------*/
  IndiceMax = 0;

  newvar = globliste;
  while (newvar)
  {
     v = newvar->var;
     if ( (v->common == 1) || (v->module == 1) )
     {
        listedesnoms = write_allocation(newvar,v,
                                       listedesnoms,
                                       alloccalls,
                                       AllocUSE,
                                       modulealloc,
                                       &IndiceMax);
     }
     newvar = newvar->suiv;
  }
  
  fclose (AllocUSE);
  fclose (alloccalls);
  fclose (modulealloc);
  if ( agrif2modelf77 == 0 ) retour77 = 0 ;
  write_probdimagrif_file();
  write_keysagrif_file();
  write_modtypeagrif_file();     
  write_Setnumberofcells_file();
  write_Getnumberofcells_file();     
  if ( todebug == 1 ) printf("Out of creefichieramr\n");
}
