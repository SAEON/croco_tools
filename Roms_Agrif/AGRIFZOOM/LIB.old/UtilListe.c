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
/*                            AddListvartolistvar                             */
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
listvar * AddListvarToListvar(listvar *l,listvar *glob,int ValueFirstpass)
{
   listvar *newvar;

   if ( firstpass == ValueFirstpass )
   {
      if ( !glob) glob = l ;
      else
      {
         newvar=glob;
         while (newvar->suiv) newvar = newvar->suiv;
         newvar->suiv = l;
      }
   }
   return glob;
}

/******************************************************************************/
/*                       CreateAndFillin_Curvar                               */
/******************************************************************************/
/* This subroutine is used to create the record corresponding to the          */
/* list of declaration                                                        */
/******************************************************************************/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*                                                                            */
/******************************************************************************/
void CreateAndFillin_Curvar(char *type,char *tokname,
                            listdim *dims,variable *curvar)
{
   if (!strcasecmp(type,"character") && strcasecmp(CharacterSize,"") )
                            strcpy(curvar->dimchar,CharacterSize);

  /* On donne la precision de la variable si elle a ete donnee                */
  curvar->c_star = 0;
  if ( c_star == 1 ) curvar->c_star = 1;
  /*                                                                          */
  if ( strcasecmp(vallengspec,"") ) strcpy(curvar->vallengspec,vallengspec);

  if ( strcasecmp(NamePrecision,"") ) strcpy(curvar->precision,NamePrecision);
  /* Si cette variable a ete declaree dans un module on met curvar->module=1  */
  if ( inmoduledeclare == 1 || SaveDeclare == 1)
  {
      curvar->module = 1;
      /* Puis on donne le nom du module dans curvar->modulename               */
      strcpy(curvar->modulename,curmodulename);
   }
   else if (insubroutinedeclare == 1 )
   /* we give the name of the subroutine to the modulename                    */
   {
      strcpy(curvar->modulename,subroutinename);
   }
   /* Si cette variable a ete initialisee                                     */
   if (InitialValueGiven == 1 ) strcpy(curvar->initialvalue,InitValue); 
   /* Si cette variable est declaree en save                                  */
   if (SaveDeclare == 1 ) curvar->save = 1;
   /* Si cette variable est allocatable                                       */
   if (Allocatabledeclare == 1 ) curvar->allocatable=1;
   /* if INTENT spec has been given                                           */
   if ( strcasecmp(IntentSpec,"") ) strcpy(curvar->IntentSpec,IntentSpec);
}


/******************************************************************************/
/*                        duplicatelistvar                                    */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void duplicatelistvar(listvar * orig)
{
   listvar *parcours;
   listvar *tmplistvar;
   listvar *tmplistvarprec;
   listdim *tmplistdim;
   variable *tmpvar;

   tmplistvarprec = (listvar *)NULL;
   parcours = orig;
   while ( parcours )
   {
      tmplistvar = (listvar *)malloc(sizeof(listvar));
      tmpvar = (variable *)malloc(sizeof(variable));
      /*                                                                      */
      strcpy(tmpvar->typevar,parcours->var->typevar);      
      strcpy(tmpvar->nomvar,parcours->var->nomvar);      
      strcpy(tmpvar->oldname,parcours->var->oldname);      
      strcpy(tmpvar->dimchar,parcours->var->dimchar);      
      if ( parcours->var->dimension )
      {
         tmplistdim = (listdim *)malloc(sizeof(listdim));
         tmplistdim = parcours->var->dimension;
         tmpvar->dimension = tmplistdim;
      }
      tmpvar->nbdim=parcours->var->nbdim;
      tmpvar->common=parcours->var->common;
      tmpvar->positioninblock=parcours->var->positioninblock;
      tmpvar->module=parcours->var->module;
      tmpvar->save=parcours->var->save;
      tmpvar->VariableIsParameter=parcours->var->VariableIsParameter;
      strcpy(tmpvar->modulename,parcours->var->modulename);      
      strcpy(tmpvar->commonname,parcours->var->commonname);      
      strcpy(tmpvar->vallengspec,parcours->var->vallengspec);
      strcpy(tmpvar->nameinttypename,parcours->var->nameinttypename);
      tmpvar->pointedvar=parcours->var->pointedvar;
      strcpy(tmpvar->commoninfile,parcours->var->commoninfile);      
      strcpy(tmpvar->subroutinename,parcours->var->subroutinename);      
      tmpvar->dimensiongiven=parcours->var->dimensiongiven;
      tmpvar->c_star=parcours->var->c_star;
      strcpy(tmpvar->precision,parcours->var->precision);
      strcpy(tmpvar->initialvalue,parcours->var->initialvalue);
      tmpvar->pointerdeclare=parcours->var->pointerdeclare;
      tmpvar->optionaldeclare=parcours->var->optionaldeclare;
      tmpvar->allocatable=parcours->var->allocatable;
      strcpy(tmpvar->IntentSpec,parcours->var->IntentSpec);
      tmpvar->dimsempty=parcours->var->dimsempty;
      strcpy(tmpvar->readedlistdimension,parcours->var->readedlistdimension);
      /*                                                                      */
      tmplistvar->var = tmpvar;
      tmplistvar->suiv = NULL;
      /*                                                                      */
      if ( !listduplicated )
      {
         listduplicated = tmplistvar;
         tmplistvarprec = listduplicated;
      }
      else
      {
         tmplistvarprec->suiv = tmplistvar;
         tmplistvarprec = tmplistvar;
      }
      /*                                                                      */
      parcours = parcours->suiv;
   }
}

/******************************************************************************/
/*                           insertdim                                        */
/******************************************************************************/
/* This subroutine is used to insert a record in a list of                    */
/* struct : listdim                                                           */
/******************************************************************************/
/*        _______     _______     _______     _______     _______             */
/*       +      +    +      +    +      +    +      +    +      +             */
/*       + NEW  +--->+ lin  +--->+ lin  +--->+ lin  +--->+  lin +             */
/*       +______+    +______+    +______+    +______+    +______+             */
/*                                                                            */
/******************************************************************************/
listdim * insertdim(listdim *lin,typedim nom)
{
   listdim *newdim ;
   listdim *parcours ;

   newdim=(listdim *) malloc (sizeof (listdim));
   newdim->dim=nom;
   newdim->suiv=NULL;
   
   if ( ! lin )
   {
      lin = newdim;
   }
   else
   {
      parcours = lin;
      while ( parcours->suiv ) parcours=parcours->suiv;
      parcours->suiv = newdim;
   }
   
   return lin;
}

/******************************************************************************/
/*                            change_dim_char                                 */
/******************************************************************************/
/* This subroutine is used to change the dimension in the list lin            */
/******************************************************************************/
/*        _______     _______                 _______     _______             */
/*       +  l   +    +  l   +                +  l   +    +   l  +             */
/*       + old  +--->+ old  +--------------->+ lin  +--->+  lin +             */
/*       +______+    +______+                +______+    +______+             */
/*                                                                            */
/******************************************************************************/
void change_dim_char(listdim *lin,listvar * l)
{
   listvar *parcours_var;
   variable *v;
  
   
   parcours_var=l;
   while(parcours_var)
   { 
      v=parcours_var->var;
      strcpy(v->dimchar,(lin->dim).last);
      parcours_var=parcours_var->suiv;
   }
}


/******************************************************************************/
/*                                num_dims                                    */
/******************************************************************************/
/* This subroutine is used to know the dimension of a table                   */
/******************************************************************************/
/*                                                                            */
/*             Dimension(jpi,jpj,jpk) ----------> num_dims = 3                */
/*                                                                            */
/******************************************************************************/
int num_dims(listdim *d)
{
   listdim *parcours;
   int compteur = 0;

   parcours = d;
   while(parcours)
   {
     compteur++;
     parcours=parcours->suiv;
   }
   return compteur;  
}


/******************************************************************************/
/*                          CREATEVAR                                         */
/******************************************************************************/
/* This subroutine is used to create and initialized a record of the          */
/*      struct : variable                                                     */
/******************************************************************************/
variable * createvar(char *nom,listdim *d)
{
  variable *var;
  listdim *dims;
  char ligne[LONGNOM];
  char listdimension[LONGNOM];

   var=(variable *) malloc(sizeof(variable));
   strcpy(var->nomvar,nom);
   /* Definition of the number of this variable in the table tabvars          */
   var->indicetabvars = 0;
   if ( firstpass == 1 && ( aftercontainsdeclare == 0 || 
                            SaveDeclare == 1          ||
                            fortran77 == 1 ) 
      )
   {
      indicemaxtabvars = indicemaxtabvars + 1;
      var->indicetabvars = indicemaxtabvars;
   }
   /*                                                                         */
   var->pointerdeclare=0;
   var->dimsempty=0;
   var->optionaldeclare=0;
   var->dimensiongiven=0;
   var->positioninblock=0;
   var->VariableIsParameter = 0;
   var->PublicDeclare = 0;
   var->PrivateDeclare = 0;
   var->ExternalDeclare = 0;
   var->common=0;
   var->allocatable=0;
   var->module=0; 
   var->save=0;
   /*                                                                         */
   strcpy(var->nameinttypename,"");
   strcpy(listdimension,"");
   strcpy(var->modulename,"");
   strcpy(var->commonname,"");
   strcpy(var->commoninfile,mainfile);
   strcpy(var->subroutinename,subroutinename);
   strcpy(var->dimchar,"");
   strcpy(var->oldname,"");
   strcpy(var->precision,""); 
   strcpy(var->initialvalue,""); 
   strcpy(var->IntentSpec,""); 
   /*                                                                         */
   if ( strcasecmp(nameinttypename,"") ) 
                                   strcpy(var->nameinttypename,nameinttypename);
   if ( optionaldeclare     == 1 ) var->optionaldeclare = 1;
   if ( pointerdeclare      == 1 ) var->pointerdeclare = 1;
   if ( VariableIsParameter == 1 ) var->VariableIsParameter = 1 ;
   if ( PublicDeclare       == 1 ) var->PublicDeclare = 1 ;
   if ( PrivateDeclare      == 1 ) var->PrivateDeclare = 1;
   if ( ExternalDeclare     == 1 ) var->ExternalDeclare = 1; 
   /*                                                                         */
   var->dimension=d;
   /* Creation of the string for the dimension of this variable               */
   dimsempty = 1;
   if ( d )
   {
      var->dimensiongiven=1;
      dims = d;
      while (dims)
      {
         if ( strcasecmp(dims->dim.first,"") || strcasecmp(dims->dim.last,""))
                                                                  dimsempty = 0;
         sprintf(ligne,"%s:%s",dims->dim.first,dims->dim.last);
         strcat(listdimension,ligne);
         if ( dims->suiv )
         {
            strcat(listdimension,",");	     
         }
         dims = dims->suiv;
      }
      if ( dimsempty == 1 ) var->dimsempty=1;
   }
   strcpy(var->readedlistdimension,listdimension);
   /*                                                                         */
   var->nbdim=num_dims(d);
   /*                                                                         */
   return var;
}

/******************************************************************************/
/*                            INSERTVAR                                       */
/******************************************************************************/
/* This subroutine is used to insert a record in a list of the                */
/*      struct : listvar                                                      */
/******************************************************************************/
/*        _______     _______     _______     _______     _______             */
/*       +      +    +      +    +      +    +      +    +      +             */
/*       +  lin +--->+  lin +--->+ lin  +--->+ lin  +--->+ NEW  +             */
/*       +______+    +______+    +______+    +______+    +______+             */
/*                                                                            */
/*                                                                            */
/******************************************************************************/
listvar * insertvar(listvar *lin,variable *v)
{
   listvar *newvar ;
   listvar *tmpvar ;

   newvar=(listvar *) malloc (sizeof (listvar));
   newvar->var=v;
   newvar->suiv = NULL;
   if (!lin)
   {
      newvar->suiv=NULL;
      lin = newvar;
   }
   else
   {
      tmpvar = lin ;
      while (tmpvar->suiv)
      {
         tmpvar = tmpvar ->suiv ;
      }
      tmpvar -> suiv = newvar;   
   }
   return lin;
}

/******************************************************************************/
/*                             SETTYPE                                        */
/******************************************************************************/
/* This subroutine is used to give the same variable type at each             */
/*      record of the list of the struct : listvar                            */
/******************************************************************************/
/*        _______     _______     _______     _______     _______             */
/*       + REAL +    + REAL +    + REAL +    + REAL +    + REAL +             */
/*       +  lin +--->+  lin +--->+ lin  +--->+ lin  +--->+ lin  +             */
/*       +______+    +______+    +______+    +______+    +______+             */
/*                                                                            */
/*                                                                            */
/******************************************************************************/
listvar *settype(char *nom,listvar *lin)
{
   listvar *newvar;
   variable *v;

   newvar=lin;
   while (newvar)
   {
      v=newvar->var;
      strcpy(v->typevar,nom);
      newvar=newvar->suiv;
   }
   newvar=lin;
   return newvar ;
}

/******************************************************************************/
/*                          deallocation_all                                  */
/******************************************************************************/
/* This subroutine is used to deallocate every strucuture                     */
/******************************************************************************/
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*                                                                            */
/*                                                                            */
/******************************************************************************/
void deallocation_all()
{
   int compt;
   listparameter *newparam;
   listvar *newlistvar;
   variable *newvar;
   listdim *newdim;
   listvarcommon *newvarcommon;
   listname *newname;
   listusemodule *newusemodule;
   listmodule *newmodule;
   listnamelist *newnamelist;
   listnom *newnom;
   listallocate *newallocate;
   listvarpointtovar *newvarpointtovar; 
   listindice *newindice; 
  
   /* deallocation globliste                                                  */
   compt = 0;
   while ( globliste )
   {
      if ( todebug == 1 && compt == 0 ) 
      {
         printf("Deallocation globliste\n");
	 compt = 1;
      }
      newlistvar =  globliste -> suiv;
      free(globliste->var->dimension);
      free(globliste->var);
      free(globliste);
      globliste = newlistvar;
   }
   /* deallocation globparam                                                  */
   compt = 0;
   while ( globparam )
   {
      if ( todebug == 1 && compt == 0 ) 
      {
         printf("Deallocation globparam\n");
	 compt = 1;
      }
      newlistvar =  globparam -> suiv;
      free(globparam->var->dimension);
      free(globparam->var);
      free(globparam);
      globparam = newlistvar;
   }
   /* deallocation listdatavariable                                           */
   compt = 0;
   while ( listdatavariable )
   {
      if ( todebug == 1 && compt == 0 ) 
      {
         printf("Deallocation listdatavariable\n");
	 compt = 1;
      }
      newlistvar =  listdatavariable -> suiv;
      free(listdatavariable->var->dimension);
      free(listdatavariable->var);
      free(listdatavariable);
      listdatavariable = newlistvar;
   }
   /* deallocation listargsubroutine                                          */
   compt = 0;
   while ( listargsubroutine )
   {
      if ( todebug == 1 && compt == 0 ) 
      {
         printf("Deallocation listargsubroutine\n");
	 compt = 1;
      }
      newlistvar =  listargsubroutine -> suiv;
      free(listargsubroutine->var->dimension);
      free(listargsubroutine->var);
      free(listargsubroutine);
      listargsubroutine = newlistvar;
   }
   /* deallocation varofsubroutineliste                                       */
   compt = 0;
   while ( varofsubroutineliste )
   {
      if ( todebug == 1 && compt == 0 ) 
      {
         printf("Deallocation varofsubroutineliste\n");
	 compt = 1;
      }
      newlistvar =  varofsubroutineliste -> suiv;
      free(varofsubroutineliste->var->dimension);
      free(varofsubroutineliste->var);
      free(varofsubroutineliste);
      varofsubroutineliste = newlistvar;
   }
   /* deallocation varsubroutine                                              */
   compt = 0;
   while ( varsubroutine )
   {
      if ( todebug == 1 && compt == 0 ) 
      {
         printf("Deallocation varsubroutine\n");
	 compt = 1;
      }
      newlistvar =  varsubroutine -> suiv;
      free(varsubroutine->var->dimension);
      free(varsubroutine->var);
      free(varsubroutine);
      varsubroutine = newlistvar;
   }
   /* deallocation listvarindoloop                                            */
   compt = 0;
   while ( listvarindoloop )
   {
      if ( todebug == 1 && compt == 0 ) 
      {
         printf("Deallocation listvarindoloop\n");
	 compt = 1;
      }
      newlistvar =  listvarindoloop -> suiv;
      free(listvarindoloop->var->dimension);
      free(listvarindoloop->var);
      free(listvarindoloop);
      listvarindoloop = newlistvar;
   }
   /* deallocation finglobliste                                               */
   compt = 0;
   while ( finglobliste )
   {
      if ( todebug == 1 && compt == 0 ) 
      {
         printf("Deallocation finglobliste\n");
	 compt = 1;
      }
      newlistvar =  finglobliste -> suiv;
      free(finglobliste->var->dimension);
      free(finglobliste->var);
      free(finglobliste);
      finglobliste = newlistvar;
   }
   /* deallocation tmplocallist                                               */
   compt = 0;
   while ( tmplocallist )
   {
      if ( todebug == 1 && compt == 0 ) 
      {
         printf("Deallocation tmplocallist\n");
	 compt = 1;
      }
      newlistvar =  tmplocallist -> suiv;
      free(tmplocallist->var->dimension);
      free(tmplocallist->var);
      free(tmplocallist);
      tmplocallist = newlistvar;
   }
   /* deallocation parameterlist                                              */
   compt = 0;
   while ( parameterlist )
   {
      if ( todebug == 1 && compt == 0 ) 
      {
         printf("Deallocation parameterlist\n");
	 compt = 1;
      }
      newlistvar =  parameterlist -> suiv;
      free(parameterlist->var->dimension);
      free(parameterlist->var);
      free(parameterlist);
      parameterlist = newlistvar;
   }
   /* deallocation globalvarofusefile2                                        */
   compt = 0;
   while ( globalvarofusefile2 )
   {
      if ( todebug == 1 && compt == 0 ) 
      {
         printf("Deallocation globalvarofusefile2\n");
	 compt = 1;
      }
      newlistvar =  globalvarofusefile2 -> suiv;
/*      free(globalvarofusefile2->var->dimension);*/
      free(globalvarofusefile2->var);
      free(globalvarofusefile2);
      globalvarofusefile2 = newlistvar;
   }
   /* deallocation globalvarofusefile                                         */
   compt = 0;
   while ( globalvarofusefile )
   {
      if ( todebug == 1 && compt == 0 ) 
      {
         printf("Deallocation globalvarofusefile\n");
	 compt = 1;
      }
      newlistvar =  globalvarofusefile -> suiv;
      free(globalvarofusefile->var->dimension);
      free(globalvarofusefile->var);
      free(globalvarofusefile);
      globalvarofusefile = newlistvar;
   }
   /* deallocation functionlistvar                                            */
   compt = 0;
   while ( functionlistvar )
   {
      if ( todebug == 1 && compt == 0 ) 
      {
         printf("Deallocation functionlistvar\n");
	 compt = 1;
      }
      newlistvar =  functionlistvar -> suiv;
      free(functionlistvar->var->dimension);
      free(functionlistvar->var);
      free(functionlistvar);
      functionlistvar = newlistvar;
   }
   /* deallocation listenotgriddepend                                         */
   compt = 0;
   while ( listenotgriddepend )
   {
      if ( todebug == 1 && compt == 0 ) 
      {
         printf("Deallocation listenotgriddepend\n");
	 compt = 1;
      }
      newlistvar =  listenotgriddepend -> suiv;
      free(listenotgriddepend->var->dimension);
      free(listenotgriddepend->var);
      free(listenotgriddepend);
      listenotgriddepend = newlistvar;
   }
   /* deallocation commonlist                                                 */
   compt = 0;
   while ( commonlist )
   {
      if ( todebug == 1 && compt == 0 ) 
      {
         printf("Deallocation commonlist\n");
	 compt = 1;
      }
      newvarcommon =  commonlist -> suiv;
      free(commonlist->dimension);
      free(commonlist);
      commonlist = newvarcommon;
   }
   /* deallocation listimplicitnone                                           */
   compt = 0;
   while ( listimplicitnone )
   {
      if ( todebug == 1 && compt == 0 ) 
      {
         printf("Deallocation listimplicitnone\n");
	 compt = 1;
      }
      newname =  listimplicitnone -> suiv;
      free(listimplicitnone);
      listimplicitnone = newname;
   }
   /* deallocation listofmodulebysubroutine                                   */
   compt = 0;
   while ( listofmodulebysubroutine )
   {
      if ( todebug == 1 && compt == 0 ) 
      {
         printf("Deallocation listofmodulebysubroutine\n");
	 compt = 1;
      }
      newusemodule =  listofmodulebysubroutine -> suiv;
      free(listofmodulebysubroutine);
      listofmodulebysubroutine = newusemodule;
   }
   /* deallocation listofincludebysubroutine                                  */
   compt = 0;
   while ( listofincludebysubroutine )
   {
      if ( todebug == 1 && compt == 0 ) 
      {
         printf("Deallocation listofincludebysubroutine\n");
	 compt = 1;
      }
      newusemodule =  listofincludebysubroutine -> suiv;
      free(listofincludebysubroutine);
      listofincludebysubroutine = newusemodule;
   }
   /* deallocation listofmoduletmp                                            */
   compt = 0;
   while ( listofmoduletmp )
   {
      if ( todebug == 1 && compt == 0 ) 
      {
         printf("Deallocation listofmoduletmp\n");
	 compt = 1;
      }
      newusemodule =  listofmoduletmp -> suiv;
      free(listofmoduletmp);
      listofmoduletmp = newusemodule;
   }
   /* deallocation tmpuselocallist                                            */
   compt = 0;
   while ( tmpuselocallist )
   {
      if ( todebug == 1 && compt == 0 ) 
      {
         printf("Deallocation tmpuselocallist\n");
	 compt = 1;
      }
      newusemodule =  tmpuselocallist -> suiv;
      free(tmpuselocallist);
      tmpuselocallist = newusemodule;
   }
   /* deallocation tmpparameterlocallist2                                     */
   compt = 0;
   while ( tmpparameterlocallist2 )
   {
      if ( todebug == 1 && compt == 0 )
      {
         printf("Deallocation tmpparameterlocallist2\n");
	 compt = 1;
      }
      newparam = tmpparameterlocallist2 -> suiv;
      free(tmpparameterlocallist2);
      tmpparameterlocallist2 = newparam;
   }
   /* deallocation tmpparameterlocallist                                      */
   compt = 0;
   while ( tmpparameterlocallist )
   {
      if ( todebug == 1 && compt == 0 )
      {
         printf("Deallocation tmpparameterlocallist\n");
	 compt = 1;
      }
      newparam = tmpparameterlocallist -> suiv;
      free(tmpparameterlocallist);
      tmpparameterlocallist = newparam;
   }
   /* deallocation listmoduleinfile                                           */
   compt = 0;
   while ( listmoduleinfile )
   {
      if ( todebug == 1 && compt == 0 )
      {
         printf("Deallocation listmoduleinfile\n");
	 compt = 1;
      }
      newmodule = listmoduleinfile -> suiv;
      free(listmoduleinfile);
      listmoduleinfile = newmodule;
   }
   /* deallocation listenamelist                                              */
   compt = 0;
   while ( listenamelist )
   {
      if ( todebug == 1 && compt == 0 )
      {
         printf("Deallocation listenamelist\n");
	 compt = 1;
      }
      newnamelist = listenamelist -> suiv;
      free(listenamelist);
      listenamelist = newnamelist;
   }
   /* deallocation NewModuleList                                              */
   compt = 0;
   while ( NewModuleList )
   {
      if ( todebug == 1 && compt == 0 )
      {
         printf("Deallocation NewModuleList\n");
	 compt = 1;
      }
      newnom = NewModuleList -> suiv;
      free(NewModuleList);
      NewModuleList = newnom;
   }
   /* deallocation listofmodules                                              */
   compt = 0;
   while ( listofmodules )
   {
      if ( todebug == 1 && compt == 0 )
      {
         printf("Deallocation listofmodules\n");
	 compt = 1;
      }
      newnom = listofmodules -> suiv;
      free(listofmodules);
      listofmodules = newnom;
   }
   /* deallocation modulelist                                                 */
   compt = 0;
   while ( modulelist )
   {
      if ( todebug == 1 && compt == 0 )
      {
         printf("Deallocation modulelist\n");
	 compt = 1;
      }
      newnom = modulelist -> suiv;
      free(modulelist);
      modulelist = newnom;
   }
   /* deallocation Listofvariableinagriffunction                              */
   compt = 0;
   while ( Listofvariableinagriffunction )
   {
      if ( todebug == 1 && compt == 0 )
      {
         printf("Deallocation Listofvariableinagriffunction\n");
	 compt = 1;
      }
      newnom = Listofvariableinagriffunction -> suiv;
      free(Listofvariableinagriffunction);
      Listofvariableinagriffunction = newnom;
   }
   /* deallocation listofsubroutinewhereagrifisused                           */
   compt = 0;
   while ( listofsubroutinewhereagrifisused )
   {
      if ( todebug == 1 && compt == 0 )
      {
         printf("Deallocation listofsubroutinewhereagrifisused\n");
	 compt = 1;
      }
      newnom = listofsubroutinewhereagrifisused -> suiv;
      free(listofsubroutinewhereagrifisused);
      listofsubroutinewhereagrifisused = newnom;
   }
   /* deallocation AllocateList                                               */
   compt = 0;
   while ( AllocateList )
   {
      if ( todebug == 1 && compt == 0 )
      {
         printf("Deallocation AllocateList\n");
	 compt = 1;
      }
      newallocate = AllocateList -> suiv;
      free(AllocateList);
      AllocateList = newallocate;
   }
   /* deallocation Listofvarpointtovar                                        */
   compt = 0;
   while ( Listofvarpointtovar )
   {
      if ( todebug == 1 && compt == 0 )
      {
         printf("Deallocation Listofvarpointtovar\n");
	 compt = 1;
      }
      newvarpointtovar = Listofvarpointtovar -> suiv;
      free(Listofvarpointtovar);
      Listofvarpointtovar = newvarpointtovar;
   }
   /* deallocation Listofavailableindices                                     */
   compt = 0;
   while ( Listofavailableindices )
   {
      if ( todebug == 1 && compt == 0 )
      {
         printf("Deallocation Listofavailableindices\n");
	 compt = 1;
      }
      newindice = Listofavailableindices -> suiv;
      free(Listofavailableindices);
      Listofavailableindices = newindice;
   }

   
}



void deallocation_curdim()
{
   int compt;
   listdim *newdim;
   
   /* deallocation listdim                                                    */
   compt = 0;
   while ( curdim )
   {
      if ( todebug == 1 && compt == 0 )
      {
         printf("Deallocation curdim\n");
	 compt = 1;
      }
      newdim = curdim -> suiv;
      free(curdim);
      curdim = newdim;
   }

}
