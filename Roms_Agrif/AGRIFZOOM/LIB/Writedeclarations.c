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
void WriteBeginDeclaration(variable *v,char ligne[LONGLIGNE])
{
  char tmpligne[LONGLIGNE];

  sprintf (ligne, "%s", v->typevar);
  if ( v->c_star == 1 ) strcat(ligne,"*");
  /* We should give the precision of the variable if it has been given        */
  if ( strcasecmp(v->precision,"") )
  {
     sprintf(tmpligne,"(%s)",v->precision);
     strcat(ligne,tmpligne);
  }
  if (strcasecmp(v->dimchar,""))
  {
     sprintf(tmpligne,"(%s)",v->dimchar);
     strcat(ligne,tmpligne);
  }
  if ( strcasecmp(v->nameinttypename,"") )
  {
     sprintf(tmpligne,"*%s",v->nameinttypename);
     strcat(ligne,tmpligne);
  }
  if (strcasecmp (v->IntentSpec, ""))
  {
     sprintf(tmpligne,",INTENT(%s) ",v->IntentSpec);
     strcat(ligne,tmpligne);
  }   
  if ( v->VariableIsParameter == 1 ) strcat(ligne, ", PARAMETER");
  if ( v->PublicDeclare       == 1 ) strcat(ligne, ", PUBLIC");  
  if ( v->PrivateDeclare      == 1 ) strcat(ligne, ", PRIVATE"); 
  if ( v->ExternalDeclare     == 1 ) strcat(ligne, ", EXTERNAL");  
  if ( v->allocatable == 1 && v->save ==0 ) strcat(ligne,", ALLOCATABLE");
  if ( v->optionaldeclare == 1 ) strcat(ligne,", OPTIONAL");
  if ( v->pointerdeclare == 1 ) strcat(ligne,", POINTER");
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
void  WriteScalarDeclaration(variable *v,char ligne[LONGLIGNE])
{

  strcat (ligne, " :: ");
  strcat (ligne, v->nomvar);
  if ( strcasecmp(v->vallengspec,"") ) strcat(ligne,v->vallengspec);
  if ( v->VariableIsParameter == 1 ) 
  {
     strcat(ligne," = ");
     strcat(ligne,v->initialvalue);
  }
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
void  WriteTableDeclaration(variable * v,char ligne[LONGLIGNE],int tmpok)
{
  char newname[LONGNOM];

  strcat (ligne, ", Dimension(");
  if ( v->dimensiongiven == 1 && tmpok == 1 )
                                           strcat(ligne,v->readedlistdimension);
  if ( v->dimensiongiven == 1 && tmpok == 0 )
  {
     strcpy(newname,ChangeTheInitalvaluebyTabvarsName
                                          (v->readedlistdimension,globliste,0));
     if ( !strcasecmp(newname,v->readedlistdimension) )
     {
        strcpy(newname,"");     
        /* la liste des use de cette subroutine                               */
        strcpy(newname,ChangeTheInitalvaluebyTabvarsName
                                 (v->readedlistdimension,globalvarofusefile,0));
        if ( !strcasecmp(newname,"") ) strcat(newname,v->readedlistdimension);
     }
     strcat(ligne,newname);
  }
  strcat (ligne, ")");
  strcat (ligne, " :: ");
  strcat (ligne, v->nomvar);  
  if ( strcasecmp(vallengspec,"") ) strcat(ligne,v->vallengspec);
  if ( !strcasecmp (v->typevar, "character") ) strcat(ligne,vargridparam(v,0));
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
void writevardeclaration (listvar * var_record, FILE *fileout)
{
  FILE *filecommon;
  listvar *newvar;
  variable *v;
  char ligne[LONGNOM];

  filecommon=fileout;
  newvar = var_record;

  if ( newvar->var->save == 0 || inmodulemeet == 0 )
  {
     v = newvar->var;
     WriteBeginDeclaration(v,ligne);
     if ( v->nbdim == 0 ) WriteScalarDeclaration(v,ligne);
     else WriteTableDeclaration(v,ligne,0);

     if ( strcasecmp(v->initialvalue,"") )
     {
        strcat(ligne," = ");
        strcat(ligne,v->initialvalue);
     }  
     tofich (filecommon, ligne,1);
  }
}


/******************************************************************************/
/*                      NonGridDepDeclaration                                 */
/******************************************************************************/
/* This subroutine is used to change the variables declaration                */
/*                                                                            */
/******************************************************************************/
/*                                                                            */
/*  integer variable(nb) ----------->                                         */
/*                      INTEGER, DIMENSION(:),Pointer :: variable             */
/*                                                                            */
/******************************************************************************/
void NonGridDepDeclaration(listvar * deb_common)
{
  listvar *newvar;

  if ( ( SaveDeclare == 0 || aftercontainsdeclare == 0 ) && listenotgriddepend ) 
  {
     newvar = deb_common;
     while (newvar)
     {
        if ( VarIsNonGridDepend(newvar->var->nomvar) == 1 ) 
                                       writevardeclaration (newvar, fortranout);
        newvar = newvar->suiv;
     }
  }
}


/******************************************************************************/
/*                       writedeclaration                                     */
/******************************************************************************/
/* This subroutine is used to write the declaration if variable present in    */
/*    the deb_common and also in the presentinthislist list file              */
/******************************************************************************/
/*                                                                            */
/*  integer variable(nb) ----------->                                         */
/*                      INTEGER, DIMENSION(1:nb),Pointer :: variable          */
/*                                                                            */
/******************************************************************************/
void writedeclaration (listvar * deb_common, FILE *fileout, listvar *presentinthislist)
{
  FILE *filecommon;
  listvar *newvar;
  listvar *parcours;
  variable *v;
  char ligne[LONGLIGNE];
  int out;

  filecommon=fileout;

  newvar = deb_common;
  while (newvar)
  {
     if ( newvar->var->save == 0 || inmodulemeet == 0 )
     {
        parcours = presentinthislist;
        /* we should write declaration of variable present in the list        */
        /* presentinthislist                                                  */
        /* if presentinthislist is empty we should write all declarations     */
        out = 0 ;
        while ( parcours && out == 0 )
        {
            /* if we find this variable in the presentinthislist, we          */
            /* could write it                                                 */
           if ( !strcasecmp(parcours->var->nomvar,newvar->var->nomvar) &&
                !strcasecmp(parcours->var->subroutinename,
                                          newvar->var->subroutinename) 
               ) out = 1;
           else parcours =parcours ->suiv;
        }
        if ( out == 0 || !presentinthislist)
        {
           /* if the variable has not been found or if the                    */
           /* presentinthislist is empty, we do not write the declaration     */
        }
        else
        {
           /* else we could write it                                          */
           v = newvar->var;
           WriteBeginDeclaration(v,ligne);
           if ( v->nbdim == 0 ) WriteScalarDeclaration(v,ligne);
           else WriteTableDeclaration(v,ligne,0);
            
           if ( strcasecmp(v->initialvalue,"") )
           {
              strcat(ligne, "=");
              strcat(ligne, v->initialvalue);
           }
           tofich (filecommon, ligne,1);
        }
     }
     newvar = newvar->suiv;
  }
}

/******************************************************************************/
/*                       writesub_loopdeclaration                             */
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
void writesub_loopdeclaration (listvar * deb_common, FILE *fileout)
{
  listvar *newvar;
  variable *v;
  char ligne[LONGLIGNE];
  int changeval;

  tofich (fileout, "",1);
  newvar = deb_common;
  while (newvar)
  {
     if ( !strcasecmp(newvar->var->modulename,subroutinename) )
     {
        changeval = 0;
        v = newvar->var;
        if ( v->allocatable == 1 && fortran77 == 0 ) 
        {
           changeval = 1;
           v->allocatable = 0; 
        }
        WriteBeginDeclaration(v,ligne);
        if ( v->nbdim == 0 ) WriteScalarDeclaration(v,ligne);
        else WriteTableDeclaration(v,ligne,1);

        tofich (fileout, ligne,1);
        if ( changeval == 1 ) 
        {
           v->allocatable = 1;
        }
     }
     newvar = newvar->suiv;
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
void writedeclarationintoamr (listvar * deb_common, FILE *fileout,
                              listvar *listin , char commonname[LONGNOM])
{
  listvar *newvar;
  variable *v;
  char ligne[LONGLIGNE];
  int changeval;
  char firstmodule[LONGNOM];
  int out;
  listnom *neededparameter;
  int writeit;
  listnom *parcours;
  listnom *parcoursprec;
  
  parcoursprec = (listnom * )NULL;
  neededparameter = (listnom * )NULL;
  /* we should list the needed parameter                                      */
  newvar = listin;
  out = 0 ;
  while ( newvar && out == 0 )
  {
     if ( strcasecmp(newvar->var->commonname,commonname) ) out = 1;
     else 
     {
        /* add the name to the list of needed parameter                       */
        neededparameter = DecomposeTheNameinlistnom(
                 newvar->var->readedlistdimension,
                 neededparameter );
        newvar = newvar->suiv;
     }
  }
  /*                                                                          */
  parcours = neededparameter;
  while (parcours)
  {
     newvar = deb_common;
     out = 0 ;
     while ( newvar && out == 0 )
     {
        if ( !strcasecmp(parcours->nom,newvar->var->nomvar) ) 
        {
           out=1; 
        /* add the name to the list of needed parameter                       */
           neededparameter = DecomposeTheNameinlistnom(
                 newvar->var->initialvalue,
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
        if ( !strcasecmp(parcours->nom,newvar->var->nomvar) ) 
        {
           out=1; 
        /* add the name to the list of needed parameter                       */
           neededparameter = DecomposeTheNameinlistnom(
                 newvar->var->initialvalue,
                 neededparameter );
        }
        else newvar=newvar->suiv;
     }
     parcours=parcours->suiv;
   }     
  /*                                                                          */
  strcpy(firstmodule,"");
  tofich (fileout, "",1);
  newvar = deb_common;
  while (newvar)
  {
     writeit = 0;
     parcours = neededparameter;
     while ( parcours && writeit == 0 )
     {
        if ( !strcasecmp(parcours->nom,newvar->var->nomvar) )
        {
           writeit=1;
           if ( parcours == neededparameter )
           {
              neededparameter = neededparameter->suiv;
           }
           else
           {
              parcoursprec->suiv= parcours->suiv;           
           }
        }
        else
        {
           parcoursprec=parcours;
           parcours=parcours->suiv;
        }
     }
     
     if ( writeit == 1  )
     {
        changeval = 0;
        v = newvar->var;
        if ( v->allocatable == 1 && fortran77 == 0 ) 
        {
           changeval = 1;
           v->allocatable = 0; 
        }
        WriteBeginDeclaration(v,ligne);
        if ( v->nbdim == 0 ) WriteScalarDeclaration(v,ligne);
        else WriteTableDeclaration(v,ligne,1);

        tofich (fileout, ligne,1);
        if ( changeval == 1 ) 
        {
           v->allocatable = 1;
        }
     }
     newvar = newvar->suiv;
  }
}



/******************************************************************************/
/*                     writedeclarationsubroutinedeclaration                  */
/******************************************************************************/
/* This subroutine is used to write the declaration of parameters needed in   */
/*    in the table definition. This subroutine is used for the declaration    */
/*    part of original subroutines                                            */
/******************************************************************************/
/*                                                                            */
/*                                                                            */
/******************************************************************************/
void  writedeclarationsubroutinedeclaration(listvar * deb_common, FILE *fileout,
                              listvar *listin)
{
  listvar *newvar;
  variable *v;
  char ligne[LONGLIGNE];
  int changeval;
  char firstmodule[LONGNOM];
  int out;
  listnom *neededparameter;
  int writeit;
  listnom *parcours;
  listnom *parcoursprec;
  
  parcoursprec = (listnom * )NULL;
  neededparameter = (listnom * )NULL;
  /* we should list the needed parameter                                      */
  newvar = listin;
  while ( newvar )
  {
     if ( !strcasecmp(newvar->var->subroutinename,subroutinename) )
     {
        /* add the name to the list of needed parameter                       */
        neededparameter = DecomposeTheNameinlistnom(
                 newvar->var->readedlistdimension,
                 neededparameter );
     }
     newvar = newvar->suiv;
  }
  /*                                                                          */
  parcours = neededparameter;
  while (parcours)
  {
     newvar = deb_common;
     out = 0 ;
     while ( newvar && out == 0 )
     {
        if ( !strcasecmp(parcours->nom,newvar->var->nomvar) ) 
        {
           out=1; 
        /* add the name to the list of needed parameter                       */
           neededparameter = DecomposeTheNameinlistnom(
                 newvar->var->initialvalue,
                 neededparameter );
        }
        else newvar=newvar->suiv;
     }
     parcours=parcours->suiv;
   }     
   /*                                                                         */
  strcpy(firstmodule,"");
  tofich (fileout, "",1);
  newvar = deb_common;
  while (newvar)
  {
     writeit = 0;
     parcours = neededparameter;
     while ( parcours && writeit == 0 )
     {
        if ( !strcasecmp(parcours->nom,newvar->var->nomvar) )
        {
           writeit=1;
           if ( parcours == neededparameter )
           {
              neededparameter = neededparameter->suiv;
           }
           else
           {
              parcoursprec->suiv= parcours->suiv;           
           }
        }
        else
        {
           parcoursprec=parcours;
           parcours=parcours->suiv;
        }
     }
     
     if ( writeit == 1  )
     {
        changeval = 0;
        v = newvar->var;
        if ( v->allocatable == 1 && fortran77 == 0 ) 
        {
           changeval = 1;
           v->allocatable = 0; 
        }
        WriteBeginDeclaration(v,ligne);
        if ( v->nbdim == 0 ) WriteScalarDeclaration(v,ligne);
        else WriteTableDeclaration(v,ligne,1);

        tofich (fileout, ligne,1);
        if ( changeval == 1 ) 
        {
           v->allocatable = 1;
        }
     }
     newvar = newvar->suiv;
  }
}
