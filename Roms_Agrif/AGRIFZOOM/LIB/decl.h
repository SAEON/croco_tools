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
#define LONGNOM 800
#define LONGLIGNE 800

/******************************************************************************/
/*********** Declaration of structures used in conv ***************************/
/******************************************************************************/

typedef struct 
{
   char first[LONGNOM];
   char last[LONGNOM];
} typedim ;                /* fortran dimension as 'ndeb:nfin'                */

typedef struct listdim 
{   
   typedim dim;
   struct listdim *suiv;
} listdim;                 /* list of the dimensions of a variable            */
      
typedef struct variable 
{
   char typevar[LONGNOM];
   char nomvar[LONGNOM] ;
   char oldname[LONGNOM] ;
   char dimchar[LONGNOM];
   listdim *dimension;
   int nbdim;
   int common;
   int positioninblock;
   int module; 
   int save;
   int VariableIsParameter;
   int PublicDeclare;
   int PrivateDeclare;
   int ExternalDeclare;
   char modulename[LONGNOM]; 
   char commonname[LONGNOM];
   char vallengspec[LONGNOM];
   char nameinttypename[LONGNOM];
   int pointedvar;
   char commoninfile[LONGNOM];
   char subroutinename[LONGNOM];
   int dimensiongiven;
   int c_star;
   int typegiven;
   char precision[LONGNOM]; 
   char initialvalue[LONGNOM]; 
   int indicetabvars; 
   int pointerdeclare; 
   int optionaldeclare;
   int allocatable; 
   char IntentSpec[LONGNOM];    
   int dimsempty;
   char readedlistdimension[LONGNOM];    
} variable ;               /* type of a variable                              */
                           /* typevar : type (integer, real, ...)             */
                           /* nomvar : name of the variable                   */
                           /* dimension : list of dimensions of the variable  */ 
                           /* nbdim: 1 if the variable is 1d, etc ...         */
                           /* precision : Name of the variable which          */
                           /* determine the precision. example : wp in the    */
                           /* case where REAL(wp)                             */
         
typedef struct listvar
{
   variable *var ;
   struct listvar * suiv;
} listvar ;                /* list of variables                               */
  

typedef struct listvarcommon
{
   char nomvar[LONGNOM] ;
   char commonname[LONGNOM];
   char subroutinename[LONGNOM];
   int dimensiongiven;
   int nbdim;
   int indicetabvars;
   int positioninblock;
   listdim *dimension;
   char readedlistdimension[LONGNOM];    
   struct listvarcommon * suiv;
} listvarcommon ;          /* list of variables in common block               */
  

typedef struct listusemodule 
{
   char usemodule[LONGNOM];
   char charusemodule[LONGNOM];
   char cursubroutine[LONGNOM];
   int firstuse;
   struct listusemodule * suiv;
} listusemodule;           /* list of names                                   */

typedef struct listparameter
{
   char name[LONGNOM];
   char modulename[LONGNOM];
   struct listparameter * suiv;
} listparameter ;           /* list of names                                  */

typedef struct listnamelist
{
   char name[LONGNOM];
   struct listnamelist * suiv;
} listnamelist ;            /* list of names                                  */

typedef struct listname
{
   char name[LONGNOM];
   struct  listname* suiv;
} listname ;            /* list of names                                  */


typedef struct listmodule 
{
   char module[LONGNOM];
   int AllocShouldMade;
   int Alloc;
   struct listmodule * suiv;
} listmodule;              /* list of names                                   */



typedef struct listcouple 
{
   char namevar[LONGNOM];
   char namepointedvar[LONGNOM];
   struct listcouple * suiv;
} listcouple;              /* list of names                                   */


typedef struct listnom 
{
   char nom[LONGNOM];
   listcouple *couple;
   struct listnom * suiv;
} listnom;                 /* list of names                                   */


typedef struct listallocate 
{
   char nomvar[LONGNOM];
   char subroutine[LONGNOM];
   char module[LONGNOM];
   struct listallocate * suiv;
} listallocate ;

 
typedef struct listvarpointtovar 
{
   char usemodule[LONGNOM];
   char cursubroutine[LONGNOM];
   listcouple *couple;
   struct  listvarpointtovar* suiv;
}listvarpointtovar ;       /* list of names                                   */


typedef struct listindice 
{
   int indice;
   struct  listindice * suiv;
} listindice;              /* list of indiced                                 */
 

 int fortran77;            /* = 1; the code has been writen in                */
                           /*    fortran77 else in fortran 90                 */
 int retour77;
 int agrif2modelf77;
/******************************************************************************/
/****************   *** COMMON Variables ***  *********************************/
/******************************************************************************/

 int positioninblock;
 char commonvar[LONGNOM];
 char commonblockname[LONGNOM];

/******************************************************************************/
/****************   *** AGRIF Variables ***   *********************************/
/******************************************************************************/
 int inagrifcallargument;
 int adduseagrifutil;
 int sameagrifargument;
 int InAgrifParentDef;
 char sameagrifname[LONGNOM];
/******************************************************************************/
/****************   *** VAR DEF Variables ***   *******************************/
/******************************************************************************/
 int oldindicemaxtabvars;  /* Number of variables in the model i.e. last      */
 int indicemaxtabvars;     /* Number of variables in the model i.e. last      */
                           /*    indice used in  the tabvars table            */
 int PublicDeclare;        /* Variable has been declared as PUBLIC */ 
 int PrivateDeclare;       /* Variable has been declared as PRIVATE */ 
 int ExternalDeclare;      /* Variable has been declared as EXTERNAL */ 
 int InitialValueGiven;    /* An initial value has been given */ 
 int formatdeclare;
 int Allocatabledeclare;
 int SaveDeclare;
 int pointerdeclare;
 int optionaldeclare;
 int VariableIsParameter; 
 int dimsgiven;
 int c_star;
 char DeclType[LONGNOM]; 
 char nameinttypename[LONGNOM]; 
 char InitValue[LONGNOM*2]; 
 char IntentSpec[LONGNOM];
 char NamePrecision[LONGNOM]; 
 char CharacterSize[LONGNOM]; 
 char curmodulename[LONGNOM];
 char vallengspec[LONGNOM];
 char subroutinename[LONGNOM];

/******************************************************************************/
/****************   *** TOAMR Variables ***   *********************************/
/******************************************************************************/
 char Alloctreatedname[LONGNOM];

/******************************************************************************/
/****************   *** CONV Variables ***   **********************************/
/******************************************************************************/
 int dimprob ;             /* dimension of the problem : 1 for 1D,2 for 2D,   */
                           /*    3 for 3D                                     */
 int onlyfixedgrids;       /* = 1 if onlyfixedgrids is true                   */
 int todebug;
 int todebugfree;
 int fixedgrids;           /* = 1 if fixedgrids is true                       */
 char nbmaillesX[LONGNOM]; /* number of cells in the x direction              */
 char nbmaillesY[LONGNOM]; /* number of cells in the y direction              */
 char nbmaillesZ[LONGNOM]; /* number of cells in the z direction              */
 int IndicenbmaillesX;
 int IndicenbmaillesY;
 int IndicenbmaillesZ;

 listvar *globliste;
 listvar *globparam;
 listvar *listdatavariable;
 listvar *listargsubroutine;
 listvar *varofsubroutineliste;
 listvar *varsubroutine;
 listvar *listvarindoloop;
 listvar *finglobliste;
 listvar *tmplocallist;
 listvar *parameterlist;
 listvar *globalvarofusefile2;
 listvar *globalvarofusefile;
 listvar *functionlistvar;
 listvar *listenotgriddepend; /* List of the variables which are not grid dependent */
 listvar *listvartempo;
 listvar *listduplicated;

 listvarcommon *commonlist;

 listname *listimplicitnone;
 listname *listpointer;

 listusemodule *listofmodulebysubroutine;
 listusemodule *listofincludebysubroutine;
 listusemodule *listofmoduletmp;
 listusemodule *tmpuselocallist;

 listparameter *tmpparameterlocallist2;
 listparameter *tmpparameterlocallist;

 listmodule *listmoduleinfile;

 listnamelist *listenamelist;

 listnom *NewModuleList;
 listnom *listofmodules;
 listnom *modulelist;
 listnom *Listofvariableinagriffunction;
 listnom *listofsubroutinewhereagrifisused;

 listallocate *AllocateList;

 listvarpointtovar *Listofvarpointtovar; 
                           /*  variables which are pointed to an other one    */

 listindice *Listofavailableindices; 
                           /* List of available indices in the tabvars table  */

 int indeclarationvar;
 int inmodulemeet;
 int incalldeclare;
 int Did_filetoparse_treated;
 int aftercontainsdeclare; /* Signale si l'on vient d'un contains ou non */
 int colnum;
 int callagrifinitgrids;
 int callmpiinit;
 int firstpass;
 int listofvarofusemodulecreated;
 int couldaddvariable;
 int Savemeet;
 int pointedvar;
 int agrif_parentcall;
 int didvariableadded;
 int infunctiondeclare;
 int SubloopScalar;        /* =1 we should put in argument of sub_loop        */
                           /*    only                                         */
                           /*    scalar and not table u(1,1,1) in place of u  */
 int checkexistcommon;
 int insubroutinedeclare;
 int tmpdeclaration_everdone;
 int paramdeclaration_everdone;
 int inmoduledeclare;
 int AllocEmpty;
 int dimsempty;
 int created_dimensionlist;

 char meetagrifinitgrids[LONGNOM];
 char meetmpiinit[LONGNOM];
 char mpiinitvar[LONGNOM];
 char *NameTamponfile;
 char toprintglob[LONGNOM];
 char recorddimension[LONGNOM];
 char tmpvargridname[LONGLIGNE];
 char curdimchar[10];
 char OriginalFileName[LONGNOM]; /* Name of the parsing file*/ 
 char EmptyChar[LONGNOM];        /* An empty char */ 
 char curfilename[LONGNOM];
 char nomfileoutput[LONGNOM];
 char curbuf[100*LONGNOM];
 char motparse[LONGNOM];
 char charusemodule[LONGNOM];
 char subofagrifinitgrids[LONGNOM];
 char motparse1[LONGNOM];
 char curfile[LONGNOM];         /* name of the current file */
 char mainfile[LONGNOM];        /* name of the configuration file */
 char nomdir[LONGNOM];          /* name of the directory where include files are put */
 char commondirout[LONGNOM];    /* name of the directory where comon files are put */
 char commondirin[LONGNOM];     /* name of the directory containing the common files */
 char filetoparse[LONGNOM];     /* name of the file where all the module file are listed */ 

 FILE *fortranout; /* Output File */
 FILE *fortranin; /* Input File */
 FILE *oldfortranout;
 FILE *subloop;
 FILE *commontomoduleout;
 FILE *paramtomoduleout;
 FILE *inputmodule;
 FILE *allocationagrif;

 long int pos_cur;         /* current position in the output file             */
 long int pos_curagrifparent;
                           /* current position in the output file             */
 long int pos_curcall;     /* current position in the output file             */
 long int pos_curuse;      /* current position in the output file             */
 long int pos_cur_decl;    /* current position in the output file             */
 long int pos_curdata;     /* current position in the output file             */
 long int pos_curparameter;/* current position in the output file             */
 long int pos_curcommon;   /* current position in the output file             */
 long int pos_curinit;     /* current position in the output file             */
 long int pos_curinclude;  /* final position of a line in file                */
 long int pos_end;         /* final position of a line in file                */
 long int pos_endsubroutine;
                           /* final position of a line in file                */

 variable *variabletempo;
 variable *curvar;

 listdim *curdim;
 listdim *commondim;

/******************************************************************************/
/*********** Declaration of externals subroutines *****************************/
/***************************************************** ************************/

/******************************************************************************/
/*********** UtilNotGridDep.c *************************************************/
/******************************************************************************/
extern void ajoutenotgriddep (char *name);
extern void RemoveNotgriddependFromGlobliste();
extern int VarIsNonGridDepend(char *name);
extern void NonGridDepDeclaration_0(listvar *listtomodify);
/******************************************************************************/
/*********** WriteInFile.c ****************************************************/
/******************************************************************************/
extern void tofich_reste (FILE * filout, char *s,int returnlineornot);
extern void tofich (FILE * filout, char *s,int returnlineornot);
extern void tofich_blanc (FILE * filout, int size);
extern void tofich_line (FILE * filout, int size, long int position);
extern void RemoveWordCUR_0(FILE * filout, long int position, 
                                           long int sizetoremove);
extern void RemoveWordSET_0(FILE * filout, long int position, 
                                           long int sizetoremove);
/******************************************************************************/
/*********** Writedeclarations.c **********************************************/
/******************************************************************************/
extern void WriteBeginDeclaration(variable *v,char ligne[LONGLIGNE]);
extern void WriteScalarDeclaration(variable *v,char ligne[LONGLIGNE]);
extern void WriteTableDeclaration(variable * v,char ligne[LONGLIGNE],int tmpok);
extern void writevardeclaration (listvar * var_record, FILE *fileout);
extern void NonGridDepDeclaration(listvar * deb_common);
extern void writedeclaration (listvar * deb_common, FILE *fileout, 
                                                    listvar *presentinthislist);
extern void writesub_loopdeclaration (listvar * deb_common, FILE *fileout);
extern void writedeclarationintoamr (listvar * deb_common, FILE *fileout,
                                    listvar *listin , char commonname[LONGNOM]);
extern void  writedeclarationsubroutinedeclaration(listvar * deb_common,
                                                 FILE *fileout,listvar *listin);
/******************************************************************************/
/*********** WorkWithvarofsubroutineliste.c ***********************************/
/******************************************************************************/
extern void CleanThelistvarofsubroutineliste();
extern void UpdatevarofsubroutinelisteWithcommonlist();
extern void ajoutvarofsubroutine_1(listvar *listtoadd);
extern void UpdatevarsubroutineWithvarofsubroutinelist_1();
/******************************************************************************/
/*********** toamr.c **********************************************************/
/******************************************************************************/
extern char *variablenameroottabvars (variable * var);
extern char *variablenametabvars (variable * var, int iorindice);
extern char *variablecurgridtabvars (variable * var,int ParentOrCurgrid);
extern char *vargridnametabvars (variable * var,int iorindice);
extern char *vargridcurgridtabvars (variable * var,int ParentOrCurgrid);
extern char *vargridparam (variable * v, int whichone);
extern char *vargridcurgridtabvarswithoutAgrif_Gr (variable * var);
extern void write_probdimagrif_file();
extern void write_keysagrif_file();
extern void write_clusteringagrif_file();
extern void write_modtypeagrif_file();
extern void write_createvarnameagrif_file(variable *v,FILE *createvarname,
                                                                int *InitEmpty);
extern void write_Setnumberofcells_file();
extern void write_Getnumberofcells_file();
extern void write_initialisationsagrif_file(variable *v,FILE *initproc,
                                     int *VarnameEmpty);
extern listnom *write_allocation(listvar *newvar,variable *v,
                          listnom *listedesnoms,
                          FILE *alloccalls,
                          FILE *AllocUSE,
                          FILE *modulealloc,
                          int *IndiceMax);
extern void creefichieramr (char *NameTampon);
/******************************************************************************/
/*********** dependfile.c *****************************************************/
/******************************************************************************/
extern void Writethedependnbxnbyfile();
extern void Readthedependnbxnbyfile();
extern void Writethedependlistofmoduleused(char *NameTampon );
extern void Readthedependlistofmoduleused(char *NameTampon);
extern void WritedependParameterList(char *NameTampon );
extern listparameter *ReaddependParameterList(char *NameTampon, 
                                                        listparameter *listout);
extern void Writethedependfile(char *NameTampon, listvar *input );
extern listvar *Readthedependfile( char *NameTampon , listvar *listout);
extern void Writethedependavailablefile();
extern void Readthedependavailablefile();
extern int Did_filetoparse_readed(char *NameTampon);
/******************************************************************************/
/*********** SubLoopCreation.c ************************************************/
/******************************************************************************/
extern void writeheadnewsub_0();
extern void writesubroutinedeclaration_0(listvar *listtomodify);
extern void WriteVariablelist_subloop(FILE *outputfile);
extern void WriteVariablelist_subloop_Call(FILE *outputfile);
extern void WriteVariablelist_subloop_Def(FILE *outputfile);
extern void WriteHeadofSubroutineLoop();
extern void closeandcallsubloopandincludeit_0(int suborfun, 
                                   char endsub[LONGNOM], char optname[LONGNOM]);
/******************************************************************************/
/*********** WorkWithglobliste.c **********************************************/
/******************************************************************************/
extern void CompareNewparsingandoldone();
extern void ajoutevar_1(listvar *listtoadd);
extern void ajoutevarsave_1(listvar *listtoadd);
extern void UpdateIndiceTabvarsofGlobliste();
extern void UpdateIndiceTabvarsofGloblisteFromCommon();
extern void UpdateGloblisteWithcommonlist_1();
/******************************************************************************/
/*********** WorkWithlistvarindoloop.c ****************************************/
/******************************************************************************/
extern void cleanlistvarfordoloop_1(int endsuborfunc);
extern void ajoutevarindoloop_1(char *ident);
extern void ajoutevarindoloop_definedimension (char *name);
extern void CleanFromThelistvarindoloopTheAgrifSubArguments();
extern void CleanThelistvarindoloop ();
extern void ModifyThelistvarindoloop();
extern void CompleteThelistvarindoloop();
/******************************************************************************/
/*********** WorkWithlistdatavariable.c ***************************************/
/******************************************************************************/
extern void CompleteDataList (char *name,char *values);
extern void CompleteGlobListeWithDatalist_1();
/******************************************************************************/
/*********** UtilAgrif.c ******************************************************/
/******************************************************************************/
extern void Instanciation_0(char *ident);
extern int Vartonumber(char *tokname);
extern int Agrif_in_Tok_NAME(char *tokname);
extern void completeListofvariableinagriffunction_1(char *ident);
extern void ModifyTheVariableName_0(char *ident);
extern void AddsubroutineTolistsubwhereagrifused();
extern void AddUseAgrifUtil_0();
extern void NotifyAgrifFunction_0(char *ident);
extern void ModifyTheAgrifFunction_0(char *ident);
extern void AgriffunctionModify_0(char *ident,int whichone);
extern void AddUseAgrifInModuleDeclaration_0();
/******************************************************************************/
/*********** WorkWithParameterlist.c ******************************************/
/******************************************************************************/
extern void AddvartoParamlist_1(listvar *listin);
extern void UpdateparameterlistWithlistvarindoloop_1();
/******************************************************************************/
/*********** WorkWithAllocatelist.c *******************************************/
/******************************************************************************/
extern void AddIdentToTheAllocateList_1(char *nom);
extern int IsAllocateInThisSubroutine_0();
extern int IsVarAllocatable_0(char *ident);
extern int varisallocatable_0(char *ident);
extern void AddNameToTheAllocateList_1(char *nom,char *nommodule);
/******************************************************************************/
/*********** UtilCharacter.c **************************************************/
/******************************************************************************/
extern void FindAndChangeNameToTabvars(char name[LONGNOM],
                char toprint[LONGNOM],listvar * listtosee, int ParentOrCurgrid);
extern char *ChangeTheInitalvaluebyTabvarsName(char *nom,listvar *listtoread,
                                                                  int whichone);
extern int IsVariableReal(char *nom);
extern void IsVarInUseFile(char *nom);
extern listnom *DecomposeTheNameinlistnom(char *nom, listnom * listout);
extern void DecomposeTheName(char *nom);
extern void convert2lower(char *name);
extern int stringblanc(char *name);
/******************************************************************************/
/*********** UtilListe.c ******************************************************/
/******************************************************************************/
extern listvar *AddListvarToListvar(listvar *l,listvar *glob,
                                                            int ValueFirstpass);
extern void CreateAndFillin_Curvar(char *type,char *tokname,
                                                listdim *dims,variable *curvar);
extern void duplicatelistvar(listvar * orig);
extern listdim *insertdim(listdim *lin,typedim nom);
extern void change_dim_char(listdim *lin,listvar * l);
extern int num_dims(listdim *d);
extern variable *createvar(char *nom,listdim *d);
extern listvar *insertvar(listvar *lin,variable *v);
extern listvar *settype(char *nom,listvar *lin);
/******************************************************************************/
/*********** UtilFile.c *******************************************************/
/******************************************************************************/
extern FILE * associate (char *filename);
extern FILE * associateaplus (char *filename);
extern long int setposcur();
extern long int setposcurinoldfortranout();
extern void copyuse_0(char *namemodule);
extern void copyuseonly_0(char *namemodule);
/******************************************************************************/
/*********** WorkWithlistofmodulebysubroutine.c *******************************/
/******************************************************************************/
extern void RecordUseModulesVariables();
extern void RecordUseModulesUseModulesVariables();
extern void Addmoduletothelist_1(char *name);
extern void Addmoduletothelist(char *name);
extern void WriteUsemoduleDeclaration();
/******************************************************************************/
/*********** WorkWithlistmoduleinfile.c ***************************************/
/******************************************************************************/
extern void FillInlistmodule_1();
extern void AllocShouldMadeTo0InModule_1();
extern void AllocShouldMadeTo1InModule_1();
extern void AllocTo1InModule_1();
extern int AllocShouldMadeInModule();
extern int AllocInModule();
/******************************************************************************/
/*********** UtilFortran.c ****************************************************/
/******************************************************************************/
extern void initdimprob(int dimprobmod, char * nx, char * ny,char* nz);
extern int Variableshouldberemove(char *nom);
extern int variableisglobal(listvar *curvar, listvar *listin);
extern void variableisglobalinmodule(listcouple *listin, char *module, 
                                                                 FILE *fileout);
extern int variableisparameterglobal(listvar *curvar, listparameter *listin);
extern void addsubroutine_alloc_0(int moduleorcontains);
extern int IsTabvarsUseInArgument_0();
extern int ImplicitNoneInSubroutine();
extern void AddNameToThelistpointer_1(char *nom);
extern int varispointer_0(char *ident);
extern int VariableIsNotFunction(char *ident);
/******************************************************************************/
/*********** DiversListe.c ****************************************************/
/******************************************************************************/
extern void Addtolistvarcommon();
extern listnom *Addtolistnom(char *nom, listnom *listin);
extern listname *Addtolistname(char *nom,listname *input);
extern int ModuleIsDefineInInputFile(char *name);
extern void AddNameToListNamelist_1(char * name);
extern void Addmoduletothelisttmp(char *name);
extern void Add_ModuleTo_Modulelist_1(char *nom);
extern void completelistvarpointtovar_1(char *namemodule,
                                                            listcouple *couple);
extern void Addincludetothelist_1(char *name);
extern void WriteIncludeDeclaration();
/******************************************************************************/
extern void processfortran(char *fichier_entree);
