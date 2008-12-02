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
%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "decl.h"
%}

%union {
       int ival;
       char na[LONG_C];
       listnom * ln;
       }

%token TOK_SEP
%token TOK_USE
%token TOK_MODULEMAIN      /* name of the module                              */
%token TOK_NOTGRIDDEP      /* Variable which are not grid dependent           */
%token <na> TOK_USEITEM
%token <na> TOK_NAME
%token <na> TOK_PROBTYPE   /* dimension of the problem                        */
%token ','
%token ';'
%token ':'
%token '('
%token ')'
%token '['
%token ']'
%%
input :
      | input line
;
line :'\n'
      | TOK_PROBTYPE TOK_NAME ';'                   {initdimprob(1,$2,"0","0");}
      | TOK_PROBTYPE TOK_NAME ',' TOK_NAME ';'      {initdimprob(2,$2, $4,"0");}
      | TOK_PROBTYPE TOK_NAME ',' TOK_NAME ',' TOK_NAME ';'
                                                    {initdimprob(3,$2, $4, $6);}
      | TOK_MODULEMAIN TOK_NAME ';'
                             {listofmodules = Addtolistnom($2,listofmodules,0);
                                                        Addmoduletothelist($2);}
      | TOK_NOTGRIDDEP TOK_SEP TOK_NAME ';'       {Add_NotGridDepend_Var_1($3);}
      | TOK_USE TOK_USEITEM ';'  {
                                    if (!strcasecmp($2,"FIXED_GRIDS"))
                                                                 fixedgrids=1;
                                    if (!strcasecmp($2,"ONLY_FIXED_GRIDS"))
                                                             onlyfixedgrids=1;
                                 }
      ;
%%

int main(int argc,char *argv[])
{
   extern FILE * yyin ;
   FILE *dependglobaloutput;
   int i;
   listnom *parcours;
   listvar *newvar;
   int stylegiven = 0;
   int infreegiven ;
   int infixedgiven ;
   int lengthmainfile;

   if (argc < 2)
   {
       printf("usage : conv <file> [-rm] [-incdir <directory>] \n");
       printf(" [-comdirin   <directory>] [-comdirout <directory>]\n");
       printf(" [-convfile  <FILENAME >] -SubloopScalar -SubloopScalar1 \n");
       printf(" [-free|-fixed]\n");
       exit(0);
   }
/******************************************************************************/
/*  1-  Variables initialization                                              */
/******************************************************************************/
   List_Global_Var=(listvar *)NULL;
   List_GlobalParameter_Var=(listvar *)NULL;
   List_Allocate_Var=(listallocate *)NULL;
   List_Common_Var=(listvar *)NULL;
   List_SubroutineWhereAgrifUsed=(listnom *)NULL;
   List_Subroutine_For_Alloc=(listnom *)NULL;
   List_Include=(listusemodule *)NULL;
   List_NameOfModuleUsed=(listusemodule *)NULL;
   listofmoduletmp=(listusemodule *)NULL;
   List_SubroutineDeclaration_Var=(listvar *)NULL;
   List_UsedInSubroutine_Var=(listvar *)NULL;
   List_NotGridDepend_Var=(listvar *)NULL;
   Listofavailableindices=(listindice *)NULL;
   List_CouplePointed_Var=(listvarpointtovar *)NULL;
   List_ModuleUsed_Var = (listvar *)NULL;
   List_ModuleUsedInModuleUsed_Var = (listvar *)NULL;
   List_GlobParamModuleUsed_Var = (listparameter *)NULL;
   List_GlobParamModuleUsedInModuleUsed_Var = (listparameter *)NULL;
   List_SubroutineArgument_Var = (listvar *)NULL;
   List_FunctionType_Var = (listvar *)NULL;
   tmpuselocallist = (listusemodule *)NULL;
   List_ContainsSubroutine = (listnom *)NULL;
   oldfortranout = (FILE *)NULL;

   strcpy(mainfile,argv[1]);
   strcpy(nomdir,"AGRIF_INC");
   strcpy(commondirin,".");
   strcpy(commondirout,".");
   strcpy(filetoparse," ");
   strcpy(subofagrifinitgrids,"");
   strcpy(meetagrifinitgrids,"");
   strcpy(mpiinitvar,"");

   length_last = 0 ;
   length_first = 0 ;
   length_v_typevar = 0 ;
   length_v_nomvar = 0 ;
   length_v_dimchar = 0 ;
   length_v_modulename = 0 ;
   length_v_commonname = 0 ;
   length_v_vallengspec = 0 ;
   length_v_nameinttypename = 0 ;
   length_v_commoninfile = 0 ;
   length_v_subroutinename = 0 ;
   length_v_precision = 0 ;
   length_v_IntentSpec = 0 ;
   length_v_initialvalue = 0 ;
   length_v_readedlistdimension = 0 ;
   length_u_usemodule = 0 ;
   length_u_charusemodule = 0 ;
   length_u_cursubroutine = 0 ;
   length_u_modulename = 0 ;
   length_n_name = 0 ;
   length_c_namevar = 0 ;
   length_c_namepointedvar = 0 ;
   length_o_nom = 0 ;
   length_o_module = 0 ;
   length_a_nomvar = 0 ;
   length_a_subroutine = 0 ;
   length_a_module = 0 ;
   length_t_usemodule = 0 ;
   length_t_cursubroutine = 0 ;
   length_curfilename = 0 ;
   length_nomfileoutput = 0 ;
   length_motparse = 0 ;
   length_mainfile = 0 ;
   length_nomdir = 0 ;
   length_commondirout = 0 ;
   length_commondirin = 0 ;
   length_filetoparse = 0 ;
   length_curbuf = 0 ;
   length_toprintglob = 0 ;
   length_tmpvargridname = 0 ;
   length_ligne_Subloop = 0 ;
   length_lvargridname_toamr = 0 ;
   length_toprint_utilagrif = 0 ;
   length_toprinttmp_utilchar = 0 ;
   length_ligne_writedecl = 0 ;
   length_newname_toamr = 0 ;
   length_newname_writedecl = 0 ;
   length_ligne_toamr = 0 ;
   length_tmpligne_writedecl = 0 ;
   value_char_size = 0 ;
   value_char_size1 = 0 ;
   value_char_size2 = 0 ;
   value_char_size3 = 0 ;
   inallocate = 0;
   infixed = 1;
   infree  = 0;

   checkexistcommon=1;
   todebug=0;
   onlyfixedgrids=0;
   fixedgrids=0;
   InAgrifParentDef = 0;
   IndicenbmaillesX=0;
   IndicenbmaillesY=0;
   IndicenbmaillesZ=0;
   created_dimensionlist = 1;
   indicemaxtabvars = 0;   /* current indice in the table tabvars             */
   SubloopScalar = 0;
   todebug = 0;
   todebugfree = 0;
   retour77 = 1 ;
   mark = 0 ;
   shouldincludempif = 0 ;
   Read_val_max();
/******************************************************************************/
/*  2-  Program arguments                                                     */
/******************************************************************************/

   if ((yyin=fopen(argv[1],"r"))==NULL)
   {
      printf("the file %s doesn't exist \n",argv[1]);
      exit(0);
   }

   i=2;
   while (i<argc)
   {
      if (!strcasecmp(argv[i],"-incdir"))
      {
         strcpy(nomdir,argv[i+1]);
         i++;
      }
      else if (!strcasecmp(argv[i],"-comdirin")) /* input directory           */
      {
         strcpy(commondirin,argv[i+1]);
         i++;
      }
      else if (!strcasecmp(argv[i],"-comdirout")) /* output directory         */
      {
         strcpy(commondirout,argv[i+1]);
         i++;
      }
      else if (!strcasecmp(argv[i],"-convfile")) /* file to parse             */
      {
         strcpy(filetoparse,argv[i+1]);
         i++;
         lengthmainfile = strlen(filetoparse);
         if (!strcasecmp(&filetoparse[lengthmainfile-4],".f90"))
         {
         infixed = 0;
         infree = 1;
         }
         else
         {
         infixed = 1;
         infree = 0;
         }
      }
      else if (!strcasecmp(argv[i],"-free")) /* file to parse        */
      {
         stylegiven = 1;
         infreegiven  = 1 ;
         infixedgiven = 0;
      }   
      else if (!strcasecmp(argv[i],"-fixed")) /* file to parse        */
      {
         stylegiven = 1;
         infreegiven  = 0;
         infixedgiven = 1;
      }         
      else if (!strcasecmp(argv[i],"-SubloopScalar")) /* file to parse        */
      {
         SubloopScalar = 1 ;
      }
      else if (!strcasecmp(argv[i],"-SubloopScalar1")) /* file to parse       */
      {
         SubloopScalar = 2 ;
      }
      else if (!strcasecmp(argv[i],"-todebug")) /* file to parse       */
      {
         todebug = 1 ;
      }
      else if (!strcasecmp(argv[i],"-mark")) /* file to parse       */
      {
         mark = 1 ;
      }
      else if (!strcasecmp(argv[i],"-todebugfree")) /* file to parse       */
      {
         todebugfree = 1 ;
      }
      else if (!strcasecmp(argv[i],"-rm"))
      {
         checkexistcommon=0;
      }
      else
      {
         printf("Unkwon option : %s\n",argv[i]);
         exit(0);
      }
      i++;
   }

   if (stylegiven == 1) 
   {
   infree = infreegiven;
   infixed = infixedgiven;   
   }
   Save_Length(nomdir,34);
   Save_Length(commondirout,35);
   Save_Length(commondirin,36);
   Save_Length(filetoparse,37);

/******************************************************************************/
/*  3-  Parsing of the  conv file <name>.in                                   */
/******************************************************************************/

   if ((yyin=fopen(argv[1],"r"))==NULL)
   {
       printf("the file %s doesn't exist \n",argv[1]);
       exit(0);
   }
   strcpy(mainfile,argv[1]);
   Save_Length(mainfile,33);

   if ( strstr(filetoparse,".f90") ||
        strstr(filetoparse,".F90") ) retour77 = 0;

   yyparse();

/******************************************************************************/
/*  4-  Preparation of the file parsing                                       */
/******************************************************************************/
   if ((yyin=fopen(filetoparse,"r"))==NULL) /* Is the file to parse exist ?   */
   {
      printf("the file %s doesn't exist \n",filetoparse);
      exit(0);
   }
   /* mainfile : the name of the file to parse                                */
   strcpy(mainfile,filetoparse);
   /*                                                                         */
   if ((dependglobaloutput=fopen(".dependglobal_agrif","r"))!=NULL)
   {
      fscanf(dependglobaloutput,"%d\n",&indicemaxtabvars);
      fclose(dependglobaloutput);
   }
   Readthedependavailablefile();
   /* Read the .dependnbxnby file which contains indices of nbmaillsX,        */
   /*    nbmailleY and nbmailleZ                                              */
   Readthedependnbxnbyfile();
   Read_Subroutine_For_Alloc();
/******************************************************************************/
/*  5-  Parsing of the input file (2 times)                                   */
/******************************************************************************/
   /* Record all variable in list                                             */
   firstpass = 1;
   processfortran(filetoparse);
   /*                                                                         */
   CompleteThelistvarindoloop();
   /* Read list of module used                                                */
   RecordUseModulesVariables();
   /* Read list of module used in module used                                 */
   RecordUseModulesUseModulesVariables();
   /* Save variables are considered as globals ones                           */
   Update_List_Global_Var_From_List_Save_Var();
   /* Update all lists                                                        */
   ListUpdate();
   /*                                                                         */
   Clean_List_Global_Var();
   /* Indice tabvars identification                                           */
   IndiceTabvarsIdentification();
   /* Update all lists                                                        */
   ListUpdate();
   /* The allocation subroutine is necessary ????                             */
   New_Allocate_Subroutine_Is_Necessary();
   /* The allocation subroutine is necessary for common list                  */
   New_Allocate_Subroutine_For_Common_Is_Necessary();
   /* Sort List_SubroutineArgument_Var                                        */
   Sort_List_SubroutineArgument_Var();
   /* Clean all lists                                                         */
   ListClean();
   /* Update Indice of List_UsedInSubroutine_Var from module used             */
   List_UsedInSubroutine_Var_Update_From_Module_Used();
   /* Update List_SubroutineWhereAgrifUsed                                    */
   UpdateList_SubroutineWhereAgrifUsed();
   /* Update List_UsedInSubroutine_Var with v_readedlistdimension             */
   UpdateList_UsedInSubroutine_With_dimension();;
   /*                                                                         */
   ModifyThelistvarindoloop();
   /*                                                                         */
   UpdateListDeclarationWithDimensionList();
   /*                                                                         */
   GiveTypeOfVariables();
   Affiche();
   /* Build new subroutines                                                   */
   firstpass = 0;
   processfortran(filetoparse);

   newvar = (listvar *)NULL;
/*newvar = List_Global_Var; */
   while ( newvar )
   {
      printf("++++ %s %d %s %s %s\n",
      newvar->var->v_nomvar,
      newvar->var->v_nbdim,
      newvar->var->v_subroutinename,
      newvar->var->v_modulename,
      newvar->var->v_typevar
             );
      newvar = newvar->suiv;
   }
/******************************************************************************/
/*  6-  Write informations in output files                                    */
/******************************************************************************/

   /* Write the .dependglobal_agrif file which contain the max indice         */
   /*    of the tabvars table                                                 */
   dependglobaloutput = fopen(".dependglobal_agrif","w");
   fprintf(dependglobaloutput,"%d\n",indicemaxtabvars);
   fclose(dependglobaloutput);
   /* Write the list of available indice                                      */
   Writethedependavailablefile();
   /* Write the .dependnbxnby file which contains indices of nbmaillsX,       */
   /*    nbmailleY and nbmailleZ                                              */
   Writethedependnbxnbyfile();
   /* Write the .depend<namefile> file which contain general informations     */
   /*    about variable of this file                                          */
   parcours = List_NameOfModule;
   while( parcours )
   {
      Writethedependlistofmoduleused(parcours->o_nom);
      WritedependParameterList(parcours->o_nom);
      Writethedependfile(parcours->o_nom,List_Global_Var);
      parcours=parcours->suiv;
   }
   parcours = List_NameOfCommon;
   while( parcours )
   {
      Writethedependfile(parcours->o_nom,List_Common_Var);
      parcours=parcours->suiv;
   }
   Write_Subroutine_For_Alloc();
/******************************************************************************/
/*  7-  Create files in AGRIF_INC directory                                   */
/******************************************************************************/
   creefichieramr(NameTamponfile);

   Write_val_max();

   if ( todebug == 1 ) printf("Out of CONV \n");
   return 0;
}
