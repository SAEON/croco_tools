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
/* version 1.2                                                                */
/******************************************************************************/
%{
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "decl.h"
%}

%union {
       int ival;
       char na[LONGNOM];
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
      | TOK_PROBTYPE TOK_NAME ';'                  {initdimprob(1,$2,"0","0");}
      | TOK_PROBTYPE TOK_NAME ',' TOK_NAME ';'     {initdimprob(2,$2, $4,"0");} 
      | TOK_PROBTYPE TOK_NAME ',' TOK_NAME ',' TOK_NAME ';'
                                                   {initdimprob(3,$2, $4, $6);}
      | TOK_MODULEMAIN TOK_NAME ';'            
                               {listofmodules = Addtolistnom($2,listofmodules);
                                Addmoduletothelist($2);}
      | TOK_NOTGRIDDEP TOK_SEP TOK_NAME ';'             {ajoutenotgriddep($3);}
      | TOK_USE TOK_USEITEM ';'  {
                                    if (!strcasecmp($2,"FIXED_GRIDS"))
                                                                 fixedgrids=1;
                                    if (!strcasecmp($2,"ONLY_FIXED_GRIDS"))
                                                             onlyfixedgrids=1;
                                    if (!strcasecmp($2,"F77"))
                                                             agrif2modelf77=1;
                                 }
      ;
%%

int main(int argc,char *argv[])
{
   extern FILE * yyin ;
   FILE *dependglobaloutput;
   char *tmp;
   int i;
   listnom *parcours;

   if (argc < 2) 
   {
       printf("usage : conv <file> [-rm] [-incdir <directory>] \n");
       printf(" [-comdirin   <directory>] [-comdirout <directory>]\n");
       printf(" [-convfile  <FILENAME >] -SubloopScalar -SubloopScalar1 \n"); 
       printf(" -f77\n"); 
       exit(0);
   }
/******************************************************************************/
/*  1-  Variables initialization                                              */
/******************************************************************************/
   globliste=(listvar *)NULL;
   listenamelist=(listnamelist *)NULL;
   globparam=(listvar *)NULL;
   AllocateList=(listallocate *)NULL;
   commonlist=(listvarcommon *)NULL;
   listofsubroutinewhereagrifisused=(listnom *)NULL;
   listofincludebysubroutine=(listusemodule *)NULL;
   listofmodulebysubroutine=(listusemodule *)NULL;
   listofmoduletmp=(listusemodule *)NULL;
   listmoduleinfile=(listmodule *)NULL;
   varofsubroutineliste=(listvar *)NULL;
   varsubroutine=(listvar *)NULL;
   listvarindoloop=(listvar *)NULL;
   listenotgriddepend=(listvar *)NULL;
   Listofavailableindices=(listindice *)NULL;
   Listofvarpointtovar=(listvarpointtovar *)NULL;
   globalvarofusefile = (listvar *)NULL;
   globalvarofusefile2 = (listvar *)NULL;
   tmpparameterlocallist = (listparameter *)NULL;
   tmpparameterlocallist2 = (listparameter *)NULL;
   
   strcpy(mainfile,argv[1]);    
   strcpy(nomdir,"AGRIF_INC");
   strcpy(commondirin,".");
   strcpy(commondirout,".");
   strcpy(filetoparse," "); 
   strcpy(subofagrifinitgrids,""); 
   strcpy(meetagrifinitgrids,"");
   strcpy(meetmpiinit,"");
   strcpy(mpiinitvar,"");

   listofvarofusemodulecreated=0;
   checkexistcommon=1;
   fortran77 = 0 ;
   Did_filetoparse_treated = 0 ;
   todebug=0;
   onlyfixedgrids=0;
   fixedgrids=0;
   InAgrifParentDef = 0;
   IndicenbmaillesX=0;
   IndicenbmaillesY=0;
   IndicenbmaillesZ=0;
   created_dimensionlist = 1;
   indicemaxtabvars = 0;   /* current indice in the table tabvars             */
   oldindicemaxtabvars = 0;/* current indice in the table tabvars             */
   SubloopScalar = 0;
   todebug = 0;
   todebugfree = 0;
   retour77 = 1 ;
   agrif2modelf77 = 0;
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
      }   
      else if (!strcasecmp(argv[i],"-f77")) /* fortran 77 file to parse       */
      {     
         fortran77 = 1 ;
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

/******************************************************************************/
/*  3-  Parsing of the  conv file <name>.in                                   */
/******************************************************************************/

   if ((yyin=fopen(argv[1],"r"))==NULL) 
   {
       printf("the file %s doesn't exist \n",argv[1]);
       exit(0);    
   }
   strcpy(mainfile,argv[1]);    

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
   /* NameTamponfile : the name of the model file extract from the name       */
   /*    of agrif_module_<NameTamponfile>                                     */
   tmp = strchr(filetoparse, '.');
   NameTamponfile=(char *)malloc(
                              (strlen(filetoparse)-strlen(tmp)+1)*sizeof(char));
   strncpy(NameTamponfile,filetoparse,strlen(filetoparse)-strlen(tmp)+1);
   strcpy (&NameTamponfile[strlen(filetoparse)-strlen(tmp)], "\0");
   /* mainfile : the name of the file to parse                                */
   strcpy(mainfile,filetoparse);    
   /* We should verify that this file has not been read before                */
   /* if it is the case we record the old globliste in the tmplocallist       */
   tmplocallist = (listvar *)NULL;
   tmpuselocallist = (listusemodule *)NULL;
   Did_filetoparse_treated = Did_filetoparse_readed(NameTamponfile);
   /* if  Did_filetoparse_treated = 1 then the file to parse has been treated */
   if ( Did_filetoparse_treated == 0 ) 
   {
     /* if the filetoparse has not been treated, we should know the last      */
     /*    tabvars indices which has been used                                */
     if ((dependglobaloutput=fopen(".dependglobal","r"))!=NULL) 
     {
        fscanf(dependglobaloutput,"%d\n",&indicemaxtabvars);
        fclose(dependglobaloutput);
        oldindicemaxtabvars = indicemaxtabvars;
     }
   }   
   /* Read the .dependnbxnby file which contains indices of nbmaillsX,       */
   /*    nbmailleY and nbmailleZ                                              */
   Readthedependnbxnbyfile();

/******************************************************************************/
/*  4-  Parsing of the input file (2 times)                                   */
/******************************************************************************/

   firstpass = 1; 
   processfortran(filetoparse); 
   firstpass = 0; 
   processfortran(filetoparse);

/******************************************************************************/
/*  5-  Write informations in output files                                    */
/******************************************************************************/

   if ( Did_filetoparse_treated == 0 ) /* if the file has never been treated  */
   {
      /* Write the .dependglobal file which contain the max indice            */
      /*    of the tabvars table                                              */
      dependglobaloutput = fopen(".dependglobal","w");
      fprintf(dependglobaloutput,"%d\n",indicemaxtabvars);
      fclose(dependglobaloutput);
      /* Write the .depend<namefile> file which contain general informations  */
      /*    about variable of this file                                       */
      parcours = modulelist;
      while( parcours )
      {
         Writethedependfile(parcours->nom,globliste);
         parcours=parcours->suiv;
      }
   }

/******************************************************************************/
/*  7-  Remove the non grid dependent variables                               */
/******************************************************************************/

   /* we should remove from the globliste the non grid dependent variables    */
   RemoveNotgriddependFromGlobliste();

/******************************************************************************/
/*  8-  Write informations in output files                                    */
/******************************************************************************/

   /* if this file has been treated in past called,                           */
   /*    we should compare the old parsing (record in the tmplocallist)       */
   /*    and the new one contained in the globliste                           */
   if ( Did_filetoparse_treated == 1 ) 
   {
      parcours = modulelist;
      while( parcours )
      {
         tmplocallist= Readthedependfile( parcours->nom  ,tmplocallist );
         parcours=parcours->suiv;
      }
      /* if the filetoparse has not been treated, we should know              */
      /*    the last tabvars indices which has been used                      */
     if ((dependglobaloutput=fopen(".dependglobal","r"))!=NULL) 
     {
        fscanf(dependglobaloutput,"%d\n",&indicemaxtabvars);
        fclose(dependglobaloutput);
        oldindicemaxtabvars = indicemaxtabvars;
     }
     /* Read the list of available indice                                     */
     Readthedependavailablefile();
     /* the old treatement has been recorded in the tmplocallist              */
     /* Now we should compare the old treatement with the new one             */
/*mazauric for each module */
     CompareNewparsingandoldone();
     /* Write the .dependglobal file which contain general informations       */
     /*    about globlist                                                     */
     dependglobaloutput = fopen(".dependglobal","w");
     fprintf(dependglobaloutput,"%d\n",indicemaxtabvars);
     fclose(dependglobaloutput);
     /* Write the list of available indice                                    */
     Writethedependavailablefile();  
     /* Write the .depend<namefile> file which contain general                */
     /*    informations about variable of this file                           */
     parcours = modulelist;
     while( parcours )
     {
        Writethedependfile(parcours->nom,globliste);
        parcours=parcours->suiv;
     }
     /* Write the .dependnbxnby file which contains indices of nbmaillsX,     */
     /*    nbmailleY and nbmailleZ                                            */
     Writethedependnbxnbyfile();
   }
   /* Write the .dependnbxnby file which contains indices of nbmaillsX,       */
   /*    nbmailleY and nbmailleZ                                              */
   Writethedependnbxnbyfile();
/******************************************************************************/
/*  8-  Create files in AGRIF_INC directory                                   */
/******************************************************************************/
   creefichieramr(NameTamponfile);
   if ( todebugfree == 1 ) deallocation_all();
   if ( todebug == 1 ) printf("Out of CONV \n");
   return 0;
}
