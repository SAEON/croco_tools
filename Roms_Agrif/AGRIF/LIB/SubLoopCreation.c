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
/*     preparation and write of the argument list of a subroutine             */
/******************************************************************************/


/******************************************************************************/
/*                             writeheadnewsub_0                              */
/******************************************************************************/
/* Firstpass 0                                                                */
/* We should write the head of the subroutine sub_loop_<subroutinename>       */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void writeheadnewsub_0()
{
   char ligne[LONG_C];

   if ( firstpass == 0 && IsTabvarsUseInArgument_0() == 1 )
   {
      if ( todebug == 1 ) printf("Enter in writeheadnewsub_0\n");
      /* we should add the use agrif_uti l if it is necessary                 */
      WriteHeadofSubroutineLoop();
      WriteUsemoduleDeclaration(subroutinename);
      if ( ImplicitNoneInSubroutine() == 1 ) fprintf(fortran_out,
                                                       "      IMPLICIT NONE\n");
      WriteIncludeDeclaration();
      /*                                                                      */
      /* We should write once the declaration of tables (extract              */
      /*    from pointer) in the new subroutine                               */
      if ( SubInList_ContainsSubroutine() == 0 ) WriteLocalParamDeclaration();

      sprintf(ligne,"\n#include \"ParamFile%s.h\" \n",subroutinename);
      tofich(fortran_out,ligne,1);

      WriteArgumentDeclaration_Sort();

      writesub_loopdeclaration_tab(List_UsedInSubroutine_Var,fortran_out);
      writesub_loopdeclaration_scalar(List_UsedInSubroutine_Var,paramout);
      /* now we should write the function declaration                         */
      /*    case if it is the                                                 */
      WriteFunctionDeclaration(1);

//      if ( SubInList_ContainsSubroutine() == 0 ) WriteSubroutineDeclaration(1);
  
      if ( todebug == 1 ) printf("Out of writeheadnewsub_0\n");
   }
   else if ( firstpass == 0 )
   {
      AddUseAgrifUtil_0(fortran_out);
      WriteUsemoduleDeclaration(subroutinename);
      WriteIncludeDeclaration();
      if ( ImplicitNoneInSubroutine() == 1 ) fprintf(fortran_out,"      IMPLICIT NONE\n");
      WriteLocalParamDeclaration();
      if ( functiondeclarationisdone == 0 ) WriteFunctionDeclaration(1);
      WriteArgumentDeclaration_beforecall();
/*    writesub_loopdeclaration_scalar(List_SubroutineArgument_Var,fortran_out);
      writesub_loopdeclaration_tab(List_SubroutineArgument_Var,fortran_out);*/
   }
}


/******************************************************************************/
/*                    WriteVariablelist_subloop                               */
/******************************************************************************/
/* This subroutine is used to write the list of the variable which            */
/* should be called by the sub_loop_<name> subroutine                         */
/* The first part is composed by the list of the local variables              */
/******************************************************************************/
/*                                                                            */
/*    List_SubroutineDeclaration_Var    a,b,c,  &                             */
/*                                      d,e,f,  &                             */
/*     a,b,c,d,e,f,g,h     ========>    g,h                                   */
/*                                                                            */
/******************************************************************************/
void WriteVariablelist_subloop(char *ligne)
{
   listvar *parcours;
   int compteur;

   if ( todebug == 1 ) printf("Enter in WriteVariablelist_subloop\n");
   parcours = List_SubroutineArgument_Var;
   didvariableadded = 0;
   compteur = 0 ;

   while ( parcours )
   {
      /* if the readed variable is a variable of the subroutine               */
      /*    subroutinename we should write the name of this variable          */
      /*    in the output file                                                */
      if ( !strcasecmp(parcours->var->v_subroutinename,subroutinename) )
      {
         if ( didvariableadded == 1 )   strcat(ligne,",");
         strcat(ligne,parcours->var->v_nomvar);
         didvariableadded = 1;
      }
      parcours = parcours -> suiv;
   }
   parcours = List_FunctionType_Var;
   while ( parcours )
   {
      if ( !strcasecmp(parcours->var->v_subroutinename,subroutinename) )
      {
         if ( didvariableadded == 1 )   strcat(ligne,",");
         strcat(ligne,parcours->var->v_nomvar);
         didvariableadded = 1;
      }
      parcours = parcours -> suiv;
   }
   if ( todebug == 1 ) printf("Out of WriteVariablelist_subloop\n");
}


/******************************************************************************/
/*                     WriteVariablelist_subloop_Call                         */
/******************************************************************************/
/* This subroutine is used to write the list of the variable which            */
/* should be called by the sub_loop_<name> subroutine into the called         */
/* The second part is composed by the list of the global table                */
/******************************************************************************/
/*                                                                            */
/*   List_UsedInSubroutine_Var SubloopScalar = 0 | SubloopScalar = 1          */
/*                                a,b,c,  &      |  a,b(1,1),c,      &        */
/*     a,b,c,d,e,f,g,h  =====>    d,e,f,  &      |  d(1),e(1,1,1),f, &        */
/*                                g,h            |  g,h(1,1)                  */
/*                                                                            */
/******************************************************************************/
void WriteVariablelist_subloop_Call(char *ligne)
{
   listvar *parcours;
   char ligne2[10];
   int i;
   int compteur ;

   if ( todebug == 1 ) printf("Enter in WriteVariablelist_subloop_Call\n");
   parcours = List_UsedInSubroutine_Var;
   compteur = 0 ;
   while ( parcours )
   {
      /* if the readed variable is a variable of the subroutine               */
      /*    subroutinename we should write the name of this variable          */
      /*    in the output file                                                */
      if ( !strcasecmp(parcours->var->v_subroutinename,subroutinename)  &&
           (parcours->var->v_pointerdeclare == 0 || !strcasecmp(parcours->var->v_typevar,"type"))
         )
      {
         if ( didvariableadded == 1 )   strcat(ligne," , ");
         strcat(ligne,vargridcurgridtabvars(parcours->var,0));
         /* if it is asked in the call of the conv we should give             */
         /* scalar in argument, so we should put (1,1,1) after the            */
         /* the name of the variable                                          */
         if (  SubloopScalar != 0 &&
               (
               (parcours->var->v_pointerdeclare == 0 || !strcasecmp(parcours->var->v_typevar,"type"))) &&
               parcours->var->v_nbdim != 0 )
         {
             i = 1;
             while ( i <=  parcours->var->v_nbdim )
             {
                if ( i == 1 ) strcat(ligne,"( ");
                if ( SubloopScalar == 2 )
                {
                   strcat(ligne,":");
                   if ( i != parcours->var->v_nbdim ) strcat(ligne,",");
                }
                else
                {
                   strcat(ligne," lbound( ");
                   strcat(ligne,vargridcurgridtabvars(parcours->var,0));
                   strcat(ligne,",");
                   strcpy(ligne2,"");
                   sprintf(ligne2,"%d",i);
                   strcat(ligne,ligne2);
                   if ( i != parcours->var->v_nbdim ) strcat(ligne,"),");
                }
                if ( i == parcours->var->v_nbdim ) strcat(ligne,"))");
                i++;
             }
         }
         didvariableadded = 1;
         compteur = compteur +1 ;
      }
      parcours = parcours -> suiv;
   }
   if ( todebug == 1 ) printf("Out of WriteVariablelist_subloop_Call\n");
}


/******************************************************************************/
/*                       WriteVariablelist_subloop_Def                        */
/******************************************************************************/
/* This subroutine is used to write the list of the variable which            */
/* should be called by the sub_loop_<name> subroutine into the def            */
/* The second part is composed by the list of the global table                */
/* <name>_tmp                                                                 */
/******************************************************************************/
/*                                                                            */
/*       List_UsedInSubroutine_Var                                            */
/*                                a-tmp,b-tmp,c_tmp, &                        */
/*     a,b,c,d,e,f,g,h  =====>    d_tmp,e_tmp,f_tmp, &                        */
/*                                g_tmp,h_tmp                                 */
/*                                                                            */
/******************************************************************************/
void WriteVariablelist_subloop_Def(char *ligne)
{
   listvar *parcours;
   int compteur;

   if ( todebug == 1 ) printf("Enter in WriteVariablelist_subloop_Def\n");
   parcours = List_UsedInSubroutine_Var;
   compteur = 0 ;
   while ( parcours )
   {
      /* if the readed variable is a variable of the subroutine               */
      /*    subrotinename we should write the name of this variable           */
      /*    in the output file                                                */
      if ( !strcasecmp(parcours->var->v_subroutinename,subroutinename)  &&
           (parcours->var->v_pointerdeclare == 0 || !strcasecmp(parcours->var->v_typevar,"type"))
         )
      {
         if ( didvariableadded == 1 )   strcat(ligne,",");
         strcat(ligne,parcours->var->v_nomvar);
         didvariableadded = 1;
      }
      parcours = parcours -> suiv;
   }
   Save_Length(ligne,41);
   if ( todebug == 1 ) printf("Out of WriteVariablelist_subloop_Def\n");
}



/******************************************************************************/
/*                      WriteHeadofSubroutineLoop                             */
/******************************************************************************/
/* This subroutine is used to write the head of the subroutine                */
/* Sub_Loop_<name>                                                            */
/******************************************************************************/
/*                 Sub_loop_subroutine.h                                      */
/*                                                                            */
/*                 subroutine Sub_Loop_subroutine ( &                         */
/*                 a,b,c, &                                                   */
/* SubLoopScalar   d,e(1,1),f(1,1,1), &                                       */
/*                 g,h  &                                                     */
/*                 )                                                          */
/******************************************************************************/
void WriteHeadofSubroutineLoop()
{
   char ligne[LONG_C];
   FILE * subloop;

   if ( todebug == 1 ) printf("Enter in WriteHeadofSubroutineLoop\n");
   tofich(fortran_out,"\n",1);
   /* Open this newfile                                                       */
   sprintf(ligne,"Sub_Loop_%s.h",subroutinename);
   subloop = open_for_write(ligne);
   /*                                                                         */
   if (isrecursive) sprintf(ligne,"      recursive subroutine Sub_Loop_%s(",subroutinename);
   else             sprintf(ligne,"      subroutine Sub_Loop_%s(",subroutinename);
   /*                                                                         */
   WriteVariablelist_subloop(ligne);
   WriteVariablelist_subloop_Def(ligne);
   /*                                                                         */
   strcat(ligne,")");
   tofich(subloop,ligne,1);
   /* if USE agrif_Uti l should be add                                        */
   AddUseAgrifUtil_0(subloop);
   /*                                                                         */
   oldfortran_out = fortran_out;
   fortran_out = subloop;
   if ( todebug == 1 ) printf("Out of WriteHeadofSubroutineLoop\n");
}

/******************************************************************************/
/*                closeandcallsubloopandincludeit_0                           */
/******************************************************************************/
/* Firstpass 0                                                                */
/* We should close the sub_loop subroutine, call it and close the             */
/* function (suborfun = 0)                                                    */
/* subroutine (suborfun = 1)                                                  */
/* end (suborfun = 2)                                                         */
/* end program (suborfun = 3)                                                 */
/* and include the sub_loop subroutine after                                  */
/******************************************************************************/
/*                                                                            */
/******************************************************************************/
void closeandcallsubloopandincludeit_0(int suborfun)
{
   char ligne[LONG_10M];

   if ( firstpass == 0 )
   {

   if ( todebug == 1 ) printf("Enter in closeandcallsubloopandincludeit_0\n");
   if ( IsTabvarsUseInArgument_0() == 1 )
   {
      /* We should remove the key word end subroutine                         */
      RemoveWordCUR_0(fortran_out,(long)(-(pos_cur-pos_endsubroutine)),
                                           pos_cur-pos_endsubroutine);
      /* We should close the loop subroutine                                  */
      sprintf(ligne,"\n      end subroutine Sub_Loop_%s",subroutinename);
      tofich(fortran_out,ligne,1);
      fclose(fortran_out);
      fortran_out = oldfortran_out;

      AddUseAgrifUtilBeforeCall_0(fortran_out);
      if ( functiondeclarationisdone == 0 ) WriteFunctionDeclaration(0);
      WriteArgumentDeclaration_beforecall();
      if ( !strcasecmp(subofagrifinitgrids,subroutinename) )
                     fprintf(oldfortran_out,"      Call Agrif_Init_Grids () \n");
      /* Now we add the call af the new subroutine                            */
      sprintf(ligne,"\n      Call Sub_Loop_%s( ",subroutinename);
      /* Write the list of the local variables used in this new subroutine    */
      WriteVariablelist_subloop(ligne);
      /* Write the list of the global tables used in this new subroutine      */
      /*    in doloop                                                         */
      WriteVariablelist_subloop_Call(ligne);
      /* Close the parenthesis of the new subroutine called                   */
       strcat(ligne,")");
      tofich(fortran_out,ligne,1);
      /* we should include the above file in the original code                */
      sprintf(ligne,"\n       contains\n");
      tofich(fortran_out,ligne,1);
      sprintf(ligne,"\n#include \"Sub_Loop_%s.h\" \n",subroutinename);
      tofich(fortran_out,ligne,1);

      /* We should close the original subroutine                              */
      if ( suborfun == 3 ) sprintf(ligne,"\n      end program %s"
                                                               ,subroutinename);
      if ( suborfun == 2 ) sprintf(ligne,"\n      end");
      if ( suborfun == 1 ) sprintf(ligne,"\n      end subroutine %s"
                                                               ,subroutinename);
      if ( suborfun == 0 ) sprintf(ligne,"\n      end function %s"
                                                               ,subroutinename);
      tofich(fortran_out,ligne,1);
      }
    oldfortran_out = (FILE *)NULL;      
   if ( todebug == 1 ) printf("Out of closeandcallsubloopandincludeit_0\n");
   }
   
}




void closeandcallsubloop_contains_0()
{
   char ligne[LONG_C];

   if ( firstpass == 0 )
   {
   if ( todebug == 1 ) printf("Enter in closeandcallsubloopandincludeit_0\n");
   if ( IsTabvarsUseInArgument_0() == 1 )
   {
      Remove_Word_Contains_0();
      sprintf(ligne,"\n      end subroutine Sub_Loop_%s",subroutinename);
      tofich(fortran_out,ligne,1);
      fclose(fortran_out);
      fortran_out = oldfortran_out;

      AddUseAgrifUtilBeforeCall_0(fortran_out);
      if ( ImplicitNoneInSubroutine() == 1 ) fprintf(fortran_out,
                                                       "      IMPLICIT NONE\n");
      WriteLocalParamDeclaration();
      if ( functiondeclarationisdone == 0 ) WriteFunctionDeclaration(0);
      WriteArgumentDeclaration_beforecall();
      WriteSubroutineDeclaration(0);
      if ( !strcasecmp(subofagrifinitgrids,subroutinename) )
                     fprintf(oldfortran_out,"      Call Agrif_Init_Grids () \n");
      /* Now we add the call af the new subroutine                            */
      if ( retour77 == 0 ) sprintf(ligne,"\n      Call Sub_Loop_%s( &"
                                                               ,subroutinename);
      else sprintf(ligne,"\n      Call Sub_Loop_%s( ",subroutinename);
      fprintf(fortran_out,ligne);
      /* Write the list of the local variables used in this new subroutine    */
      WriteVariablelist_subloop(ligne);
      /* Write the list of the global tables used in this new subroutine      */
      /*    in doloop                                                         */
      WriteVariablelist_subloop_Call(ligne);
      /* Close the parenthesis of the new subroutine called                   */
      sprintf(ligne,")");
      tofich(fortran_out,ligne,1);
      /* We should close the original subroutine                              */
      sprintf(ligne,"\n      contains");
      tofich(fortran_out,ligne,1);
      /* we should include the above file in the original code                */
      sprintf(ligne,"\n#include \"Sub_Loop_%s.h\" \n",subroutinename);
      tofich(fortran_out,ligne,1);
      }
   oldfortran_out = (FILE *)NULL;
   if ( todebug == 1 ) printf("Out of closeandcallsubloopandincludeit_0\n");
   }
}
