/* $Id$ */
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
/* "http ://www.cecill.info".                                                  */
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
extern int line_num_fortran;
extern int line_num_fortran_common;
char *tmp;
char c_selectorname[LONG_C];
char ligne[LONG_C];
char truename[LONGNOM];
char identcopy[LONG_C];
int c_selectorgiven=0;
int incom;
listvar *curlistvar;
typedim c_selectordim;
listcouple *coupletmp;
listdim *parcoursdim;
int removeline=0;
listvar *test;
%}

%union {
       char      nac[LONG_C];
       char      na[LONGNOM];
       listdim  *d;
       listvar  *l;
       listnom  *ln;
       listcouple  *lc;
       listname *lnn;
       typedim   dim1;
       variable *v;
       }

%left ','
%nonassoc ':'
%right '='
%left TOK_BINARY_OP
%left TOK_EQV TOK_NEQV
%left TOK_OR TOK_XOR
%left TOK_AND
%left TOK_NOT
%nonassoc TOK_LT TOK_GT TOK_LE TOK_GE TOK_EQ TOK_NE
%nonassoc TOK_UNARY_OP
%left TOK_DSLASH
%left '+' '-'
%left '*' TOK_SLASH
%right TOK_DASTER

%token TOK_SEP
%token TOK_SEMICOLON
%token TOK_NEXTLINE
%token TOK_PARAMETER
%token TOK_RESULT
%token TOK_ONLY
%token TOK_INCLUDE
%token TOK_SUBROUTINE
%token TOK_PROGRAM
%token TOK_FUNCTION
%token TOK_OMP
%token TOK_DOLLAR
%token TOK_FORMAT
%token TOK_MAX
%token TOK_TANH
%token TOK_WHERE
%token TOK_ELSEWHERE
%token TOK_ENDWHERE
%token TOK_MAXVAL
%token TOK_TRIM
%token TOK_SUM
%token TOK_SQRT
%token TOK_CASE
%token TOK_SELECTCASE
%token TOK_FILE
%token TOK_END
%token TOK_ERR
%token TOK_DONOTTREAT
%token TOK_ENDDONOTTREAT
%token TOK_EXIST
%token TOK_MIN
%token TOK_FLOAT
%token TOK_EXP
%token TOK_COS
%token TOK_COSH
%token TOK_ACOS
%token TOK_NINT
%token TOK_CYCLE
%token TOK_SIN
%token TOK_SINH
%token TOK_ASIN
%token TOK_EQUIVALENCE
%token TOK_BACKSPACE
%token TOK_LOG
%token TOK_TAN
%token TOK_ATAN
%token TOK_RECURSIVE
%token TOK_ABS
%token TOK_MOD
%token TOK_SIGN
%token TOK_MINLOC
%token TOK_MAXLOC
%token TOK_EXIT
%token TOK_MINVAL
%token TOK_PUBLIC
%token TOK_PRIVATE
%token TOK_ALLOCATABLE
%token TOK_RETURN
%token TOK_THEN
%token TOK_ELSEIF
%token TOK_ELSE
%token TOK_ENDIF
%token TOK_PRINT
%token TOK_PLAINGOTO
%token TOK_CONSTRUCTID
%token TOK_LOGICALIF
%token TOK_PLAINDO
%token TOK_CONTAINS
%token TOK_ENDDO
%token TOK_MODULE
%token TOK_ENDMODULE
%token TOK_DOWHILE
%token TOK_ALLOCATE
%token TOK_OPEN
%token TOK_CLOSE
%token TOK_INQUIRE
%token TOK_WRITE
%token TOK_READ
%token TOK_REWIND
%token TOK_DEALLOCATE
%token TOK_NULLIFY
%token TOK_FIN
%token TOK_DEBUT
%token TOK_DIMENSION
%token TOK_ENDSELECT
%token TOK_EXTERNAL
%token TOK_INTENT
%token TOK_INTRINSIC
%token TOK_NAMELIST
%token TOK_CASEDEFAULT
%token TOK_OPTIONAL
%token TOK_POINTER
%token TOK_CONTINUE
%token TOK_SAVE
%token TOK_TARGET
%token TOK_QUOTE
%token TOK_IMPLICIT
%token TOK_NONE
%token TOK_CALL
%token TOK_STAT
%token TOK_POINT_TO
%token TOK_COMMON
%token TOK_GLOBAL
%token TOK_INTERFACE
%token TOK_ENDINTERFACE
%token TOK_LEFTAB
%token TOK_RIGHTAB
%token TOK_PAUSE
%token TOK_PROCEDURE
%token TOK_STOP
%token TOK_NAMEEQ
%token TOK_REAL8
%token <nac> TOK_OUT
%token <nac> TOK_INOUT
%token <nac> TOK_IN
%token <nac> TOK_USE
%token <nac> TOK_DSLASH
%token <nac> TOK_DASTER
%token <nac> TOK_EQ
%token <nac> TOK_EQV
%token <nac> TOK_GT
%token <nac> TOK_LT
%token <nac> TOK_GE
%token <nac> TOK_NE
%token <nac> TOK_NEQV
%token <nac> TOK_LE
%token <nac> TOK_OR
%token <nac> TOK_XOR
%token <nac> TOK_NOT
%token <nac> TOK_AND
%token <nac> TOK_TRUE
%token <nac> TOK_FALSE
%token <nac> TOK_LABEL
%token <nac> TOK_TYPE
%token <nac> TOK_TYPEPAR
%token <nac> TOK_ENDTYPE
%token <nac> TOK_REAL
%token <nac> TOK_INTEGER
%token <nac> TOK_LOGICAL
%token <nac> TOK_DOUBLEPRECISION
%token <nac> TOK_DOUBLEREAL
%token <nac> TOK_ENDSUBROUTINE
%token <nac> TOK_ENDFUNCTION
%token <nac> TOK_ENDPROGRAM
%token <nac> TOK_ENDUNIT
%token <nac> TOK_CHARACTER
%token <nac> TOK_CHAR_CONSTANT
%token <nac> TOK_CHAR_CUT
%token <nac> TOK_DATA
%token <nac> TOK_CHAR_INT
%token <nac> TOK_CHAR_MESSAGE
%token <nac> TOK_CSTREAL
%token <nac> TOK_CSTREALDP
%token <nac> TOK_CSTREALQP
%token <nac> TOK_SFREAL
%token <nac> TOK_COMPLEX
%token <nac> TOK_DOUBLECOMPLEX
%token <nac> TOK_NAME
%token <nac> TOK_NAME_CHAR
%token <nac> TOK_PROBTYPE  /* dimension of the problem                        */
%token <nac> TOK_INTERPTYPE/* kind of interpolation                           */
%token <nac> TOK_VARTYPE   /* posit ion of the grid variable on the cells of  */
                          /*     the mesh                                     */
%token <nac> TOK_SLASH
%token <nac> TOK_BC        /* calculation of the boundary conditions           */
%token <nac> TOK_OP
%token <nac> TOK_CSTINT
%token <nac> TOK_COMMENT
%token <nac> TOK_FILENAME
%token ','
%token ':'
%token '('
%token ')'
%token '['
%token ']'
%token '!'
%token '_'
%token '<'
%token '>'
%type <l> dcl
%type <l> after_type
%type <l> dimension
%type <l> paramlist
%type <l> args
%type <l> arglist
%type <lc> only_list
%type <lc> only_name
%type <lc> rename_list
%type <lc> rename_name
%type <d> dims
%type <d> dimlist
%type <dim1> dim
%type <v> paramitem
%type <nac> comblock
%type <nac> name_routine
%type <nac> module_name
%type <nac> opt_name
%type <nac> type
%type <nac> word_endsubroutine
%type <nac> word_endfunction
%type <nac> word_endprogram
%type <nac> word_endunit
%type <nac> typename
%type <nac> typespec
%type <nac> string_constant
%type <nac> simple_const
%type <nac> ident
%type <nac> do_var
%type <nac> intent_spec
%type <nac> signe
%type <nac> opt_signe
%type <nac> filename
%type <na> attribute
%type <na> complex_const
%type <na> begin_array
%type <na> clause
%type <na> arg
%type <na> uexpr
%type <na> minmaxlist
%type <na> lhs
%type <na> vec
%type <na> outlist
%type <na> out2
%type <na> other
%type <na> dospec
%type <na> expr_data
%type <na> structure_component
%type <na> array_ele_substring_func_ref
%type <na> funarglist
%type <na> funarg
%type <na> funargs
%type <na> triplet
%type <na> substring
%type <na> opt_substring
%type <na> opt_expr
%type <na> optexpr
%type <na> datavallist
%type <na> after_slash
%type <na> after_equal
%type <na> predefinedfunction
%type <na> expr
%type <na> ubound
%type <na> operation
%type <na> proper_lengspec
%type <lnn> use_name_list
%type <lnn> public

%left TOK_OP
%%
input :
      | input line
      ;
line :  '\n' position
      | thislabel suite_line_list
      | TOK_COMMENT
      | keyword cmnt writedeclar
      | error writedeclar nulcurbuf
                   {yyerrok;yyclearin;}
      ;
suite_line_list : suite_line
      |   suite_line_list TOK_SEMICOLON suite_line
      ;
suite_line : entry fin_line  /* subroutine, function, module                    */
      | spec fin_line      /* declaration                                     */
      | before_include filename fin_line
                  {
                     if (inmoduledeclare == 0 )
                     {
                        pos_end = setposcur();
                        RemoveWordSET_0(fortranout,pos_curinclude,
                                              pos_end-pos_curinclude);
                     }
                  }
      | exec cmnt writedeclar /* if, do etc ...                               */
      | instr fin_line    /* instruction ident  : do i = 1 ...                */
      ;
instr : ident ':'
      ;
fin_line : position cmnt
      ;
keyword : TOK_DONOTTREAT
         {
            /* we should ignore the declaration until the keyword             */
            /*    TOK_ENDDONOTTREAT                                           */
            couldaddvariable = 0 ;
            RemoveWordCUR_0(fortranout,-20,20);
         }
      | TOK_ENDDONOTTREAT
         {
             couldaddvariable = 1 ;
             RemoveWordCUR_0(fortranout,-24,24);
          }
      | TOK_OMP
      | TOK_DOLLAR
      ;
position : {pos_cur = setposcur();}
      ;
thislabel :
      | TOK_LABEL nulcurbuf
      ;
cmnt :
      | TOK_COMMENT
      ;
incomment :
                   {incom = 1;}
      ;
nulcurbuf :
                   {if (incom !=1) {strcpy(curbuf,"");incom=0;}}
      ;
opt_recursive :
      | TOK_RECURSIVE
      ;
entry :
      | opt_recursive TOK_SUBROUTINE name_routine arglist
                   {
                      if ( couldaddvariable == 1 )
                      {
                      /* open param file                                      */
                      if ( firstpass == 0 )
                      {
                         sprintf(ligne,"%s/ParamFile%s.h",nomdir,$3);
                         paramout=fopen(ligne,"w");
                         if ( retour77 == 0 ) fprintf(paramout,"!\n");
                         else fprintf(paramout,"C\n");

                      }
                      Add_SubroutineArgument_Var_1($4);
                      if ( inmodulemeet == 1 )
                      {
                         insubroutinedeclare = 1;
                         /* in the second step we should write the head of    */
                         /*    the subroutine sub_loop_<subroutinename>       */
                         writeheadnewsub_0(1);
                      }
                      else
                      {
                            insubroutinedeclare = 1;
                            writeheadnewsub_0(1);
                      }
                      }
                   }
      | TOK_PROGRAM name_routine
                   {
                      /* open param file                                      */
                      if ( firstpass == 0 )
                      {
                         sprintf(ligne,"%s/ParamFile%s.h",nomdir,$2);
                         paramout=fopen(ligne,"w");
                         if ( retour77 == 0 ) fprintf(paramout,"!\n");
                         else fprintf(paramout,"C\n");

                      }
                      strcpy(subroutinename,$2);
                      /* Common case                                          */
                      insubroutinedeclare = 1;
                      /* in the second step we should write the head of       */
                      /*    the subroutine sub_loop_<subroutinename>          */
                      writeheadnewsub_0(1);
                   }
      | TOK_FUNCTION name_routine arglist TOK_RESULT arglist1
                   {
                      /* open param file                                      */
                      if ( firstpass == 0 )
                      {
                         sprintf(ligne,"%s/ParamFile%s.h",nomdir,$2);
                         paramout=fopen(ligne,"w");
                         if ( retour77 == 0 ) fprintf(paramout,"!\n");
                         else fprintf(paramout,"C\n");
                      }
                      strcpy(subroutinename,$2);
                      if ( inmodulemeet == 1 )
                      {
                         insubroutinedeclare = 1;
                         /* we should to list of the subroutine argument the  */
                         /*    name of the function which has to be defined   */
                         Add_SubroutineArgument_Var_1($3);
                         strcpy(DeclType,"");
                         /* in the second step we should write the head of    */
                         /*    the subroutine sub_loop_<subroutinename>       */
                         writeheadnewsub_0(2);
                      }
                      else
                      {
                            insubroutinedeclare = 1;
                            /* we should to list of the subroutine argument   */
                            /* name of the function which has to be defined   */
                            Add_SubroutineArgument_Var_1($3);
                            strcpy(DeclType,"");
                            Add_FunctionType_Var_1($2);
                            writeheadnewsub_0(2);
                      }
                   }
      | TOK_FUNCTION name_routine arglist
                   {
                      /* open param file                                      */
                      if ( firstpass == 0 )
                      {
                         sprintf(ligne,"%s/ParamFile%s.h",nomdir,$2);
                         paramout=fopen(ligne,"w");
                         if ( retour77 == 0 ) fprintf(paramout,"!\n");
                         else fprintf(paramout,"C\n");
                      }
                      strcpy(subroutinename,$2);
                      if ( inmodulemeet == 1 )
                      {
                         insubroutinedeclare = 1;
                         /* we should to list of the subroutine argument the  */
                         /*    name of the function which has to be defined   */
                         Add_SubroutineArgument_Var_1($3);
                         strcpy(DeclType,"");
                         Add_FunctionType_Var_1($2);
                         /* in the second step we should write the head of    */
                         /*    the subroutine sub_loop_<subroutinename>       */
                         writeheadnewsub_0(2);
                      }
                      else
                      {
                            insubroutinedeclare = 1;
                            /* we should to list of the subroutine argument   */
                            /* name of the function which has to be defined   */
                            Add_SubroutineArgument_Var_1($3);
                            strcpy(DeclType,"");
                            Add_FunctionType_Var_1($2);
                            writeheadnewsub_0(2);
                      }
                   }
      | TOK_MODULE TOK_NAME
                   {
                      GlobalDeclaration = 0;
                      strcpy(curmodulename,$2);
                      strcpy(subroutinename,"");
                      Add_NameOfModule_1($2);
                      if ( inmoduledeclare == 0 )
                      {
                         /* To know if there are in the module declaration    */
                         inmoduledeclare = 1;
                         /* to know if a module has been met                  */
                         inmodulemeet = 1;
                         /* to know if we are after the keyword contains      */
                         aftercontainsdeclare = 0 ;
                      }
                   }
      ;
name_routine : TOK_NAME
                   {
                      if ( couldaddvariable == 1 )
                      {
                         strcpy($$,$1);strcpy(subroutinename,$1);
                      }
                   }
writedeclar :
      ;
before_include : TOK_INCLUDE
                   {
                      pos_curinclude = setposcur()-9;
                   }
filename : TOK_CHAR_CONSTANT
                   {
                      if ( couldaddvariable == 1 ) Add_Include_1($1);
                   }
      ;
arglist :           {
                      if ( firstpass == 1 && couldaddvariable == 1) $$=NULL;
                   }
      | '(' ')'    {
                      if ( firstpass == 1 && couldaddvariable == 1 ) $$=NULL;
                   }
      | '(' args ')'
                   {
                       if ( firstpass == 1 && couldaddvariable == 1 ) $$=$2;
                   }
      ;
arglist1:
      | '(' ')'
      | '(' args ')'
                   {
                      if ( couldaddvariable == 1 )
                      {
                         Add_SubroutineArgument_Var_1($2);
                      }
                   }
      ;
args :arg           {
                      if ( firstpass == 1  && couldaddvariable == 1)
                      {
                         strcpy(nameinttypenameback,nameinttypename);
                         strcpy(nameinttypename,"");
                         curvar=createvar($1,NULL);
                        strcpy(nameinttypename,nameinttypenameback);
                         curlistvar=insertvar(NULL,curvar);
                         $$=settype("",curlistvar);
                      }
                   }
      | args ',' arg
                   {
                      if ( firstpass == 1  && couldaddvariable == 1)
                      {
                         strcpy(nameinttypenameback,nameinttypename);
                         strcpy(nameinttypename,"");                      
                         curvar=createvar($3,NULL);
                         strcpy(nameinttypename,nameinttypenameback);                         
                         $$=insertvar($1,curvar);
                      }
                   }
      ;
arg : TOK_NAME      {if ( couldaddvariable == 1 ) strcpy($$,$1);}
      | '*'        {if ( couldaddvariable == 1 ) strcpy($$,"*");}
      ;
spec : type after_type
                   {
                      if ( VarTypepar == 1 )
                      {
                         couldaddvariable = 1 ;
                         VarTypepar = 0;
                      }
                   }
      | TOK_TYPE opt_spec opt_sep opt_name
                   {
                      if ( couldaddvariable == 1 )
                      {
                         VarType = 1;
                         couldaddvariable = 0 ;
                      }
                   }
      | TOK_ENDTYPE opt_name
                   {
                      if ( VarType == 1 ) couldaddvariable = 1 ;
                      VarType = 0;
                      VarTypepar = 0;
                   }
      | TOK_POINTER list_couple
      | before_parameter  '(' paramlist ')'
                   {
                      if ( couldaddvariable == 1 )
                      {
                         if ( insubroutinedeclare == 0 )
                                                  Add_GlobalParameter_Var_1($3);
                         else Add_Parameter_Var_1($3);
                         pos_end = setposcur();
                        RemoveWordSET_0(fortranout,pos_cur_decl,
                                                    pos_end-pos_cur_decl);
                      }
                      VariableIsParameter =  0 ;
                   }
      | before_parameter  paramlist
                   {
                     if ( couldaddvariable == 1 )
                     {
                        if ( insubroutinedeclare == 0 )
                                                  Add_GlobalParameter_Var_1($2);
                         else Add_Parameter_Var_1($2);
                         pos_end = setposcur();
                         RemoveWordSET_0(fortranout,pos_cur_decl,
                                                    pos_end-pos_cur_decl);
                      }
                      VariableIsParameter =  0 ;
                   }
      | common
      | save
                  {
                     pos_end = setposcur();
                     RemoveWordSET_0(fortranout,pos_cursave,
                                                pos_end-pos_cursave);
                  }
      | implicit
      | dimension
                  {
                   /* if the variable is a parameter we can suppose that is   */
                   /*    value is the same on each grid. It is not useless to */
                   /*    create a copy of it on each grid                     */
                      if ( couldaddvariable == 1 )
                      {
                         Add_Globliste_1($1);
                         /* if variableparamlists has been declared in a      */
                         /*    subroutine                                     */
                         if ( insubroutinedeclare == 1 )
                         {
                            Add_Dimension_Var_1($1);
                         }
                         pos_end = setposcur();
                         RemoveWordSET_0(fortranout,pos_curdimension,
                                                pos_end-pos_curdimension);
                      }
                      /*                                                      */
                      PublicDeclare = 0;
                      PrivateDeclare = 0;
                      ExternalDeclare = 0;
                      strcpy(NamePrecision,"");
                      c_star = 0;
                      InitialValueGiven = 0 ;
                      strcpy(IntentSpec,"");
                      VariableIsParameter =  0 ;
                      Allocatabledeclare = 0 ;
                      SaveDeclare = 0;
                      pointerdeclare = 0;
                      optionaldeclare = 0 ;
                      dimsgiven=0;
                      c_selectorgiven=0;
                      strcpy(nameinttypename,"");
                      strcpy(c_selectorname,"");
                   }
      | public
      {
      if (firstpass == 0)
      {
      if ($1)
      {
      removeglobfromlist(&($1));
      pos_end = setposcur();
           RemoveWordSET_0(fortranout,pos_cur,pos_end-pos_cur);
      writelistpublic($1);
      }
      }
      }
      | private
      | use_stat
      | module_proc_stmt
      | interface
      | namelist
      | TOK_BACKSPACE '(' expr ')'
      | TOK_EXTERNAL opt_sep use_name_list
      | TOK_INTRINSIC opt_sep use_intrinsic_list
      | TOK_EQUIVALENCE list_expr_equi
      | before_data data '\n'
                   {
                      /* we should remove the data declaration                */
                      if ( couldaddvariable == 1 && aftercontainsdeclare == 0 )
                      {
                        pos_end = setposcur();
                        RemoveWordSET_0(fortranout,pos_curdata,
                                              pos_end-pos_curdata);
                      }
                  }
      ;
opt_spec :
      | access_spec
      {
         PublicDeclare = 0 ;
         PrivateDeclare = 0 ;
      }
      ;
name_intrinsic : TOK_SUM
      | TOK_TANH
      | TOK_MAXVAL
      | TOK_MIN
      | TOK_MINVAL
      | TOK_TRIM
      | TOK_SQRT
      | TOK_NINT
      | TOK_FLOAT
      | TOK_EXP
      | TOK_COS
      | TOK_COSH
      | TOK_ACOS
      | TOK_SIN
      | TOK_SINH
      | TOK_ASIN
      | TOK_LOG
      | TOK_TAN
      | TOK_ATAN
      | TOK_MOD
      | TOK_SIGN
      | TOK_MINLOC
      | TOK_MAXLOC
      | TOK_NAME
      ;
use_intrinsic_list : name_intrinsic
      | use_intrinsic_list ',' name_intrinsic
      ;
list_couple : '(' list_expr ')'
      | list_couple ',' '(' list_expr ')'
      ;
list_expr_equi : expr_equi
      | list_expr_equi ',' expr_equi
      ;
expr_equi : '(' list_expr_equi1 ')'
      ;
list_expr_equi1 : ident dims
      | list_expr_equi1 ',' ident dims
      ;
list_expr : expr
      | list_expr ',' expr
      ;
opt_sep :
      | ':' ':'
      ;
after_type : dcl nodimsgiven
                   {
                      /* if the variable is a parameter we can suppose that is*/
                      /*    value is the same on each grid. It is not useless */
                      /*    to create a copy of it on each grid               */
                      if ( couldaddvariable == 1 )
                      {
                      pos_end = setposcur();
                      /*if (insubroutinedeclare == 0)
                        {   */
                         RemoveWordSET_0(fortranout,pos_cur_decl,
                                                 pos_end-pos_cur_decl);
                                         
                       /* }
                      else
                       {*/
                        ReWriteDeclarationAndAddTosubroutine_01($1);
                        pos_cur_decl = setposcur();
                        
                       /*}*/
                      if ( firstpass == 0 &&
                           GlobalDeclaration == 0 &&
                           insubroutinedeclare == 0 )
                      {
                         sprintf(ligne,"\n#include \"Module_Declar_%s.h\"\n"
                                                                ,curmodulename);
                         tofich(fortranout,ligne,1);
                         sprintf (ligne, "Module_Declar_%s.h",curmodulename);
                         module_declar = associate(ligne);
                         sprintf (ligne, " ");
                         tofich (module_declar, ligne,1);
                         GlobalDeclaration = 1 ;
                      }
                         $$ = $1;
                         Add_Globliste_1($1);
                                                  
                         if ( insubroutinedeclare == 0 )
                                                  Add_GlobalParameter_Var_1($1);
                         else
                         {
                            if ( pointerdeclare == 1 )
                                                Add_Pointer_Var_From_List_1($1);
                            Add_Parameter_Var_1($1);
                         }

                         /* if variables has been declared in a subroutine    */
                         if ( insubroutinedeclare == 1 )
                         { 
                       /*    Add_SubroutineDeclaration_Var_1($1);*/
                         }
                         /* If there are a SAVE declarations in module's      */
                         /*    subroutines we should remove it from the       */
                         /*    subroutines declaration and add it in the      */
                         /*    global declarations                            */
                         if ( aftercontainsdeclare == 1 &&
                              SaveDeclare == 1 && firstpass == 1 )
                         {
                              if ( inmodulemeet == 0 ) Add_Save_Var_dcl_1($1);
                              else  Add_SubroutineDeclarationSave_Var_1($1);
                         }
                      }
                      /*                                                      */
                      PublicDeclare = 0;
                      PrivateDeclare = 0;
                      ExternalDeclare = 0;
                      strcpy(NamePrecision,"");
                      c_star = 0;
                      InitialValueGiven = 0 ;
                      strcpy(IntentSpec,"");
                      VariableIsParameter =  0 ;
                      Allocatabledeclare = 0 ;
                      SaveDeclare = 0;
                      pointerdeclare = 0;
                      optionaldeclare = 0 ;
                      dimsgiven=0;
                      c_selectorgiven=0;
                      strcpy(nameinttypename,"");
                      strcpy(c_selectorname,"");
                   }
      | before_function name_routine arglist
                   {
                      /* open param file                                      */
                      if ( firstpass == 0 )
                      {
                         sprintf(ligne,"%s/ParamFile%s.h",nomdir,$2);
                         paramout=fopen(ligne,"w");
                         if ( retour77 == 0 ) fprintf(paramout,"!\n");
                         else fprintf(paramout,"C\n");
                      }
                      strcpy(subroutinename,$2);
                      if ( inmodulemeet == 1 )
                      {
                         insubroutinedeclare = 1;
                         /* we should to list of the subroutine argument the  */
                         /*    name of the function which has to be defined   */
                         Add_SubroutineArgument_Var_1($3);
                         Add_FunctionType_Var_1($2);
                         /* in the second step we should write the head of    */
                         /*    the subroutine sub_loop_<subroutinename>       */
                         writeheadnewsub_0(2);
                      }
                      else
                      {
                         insubroutinedeclare = 1;
                         /* we should to list of the subroutine argument the  */
                         /*    name of the function which has to be defined   */
                         Add_SubroutineArgument_Var_1($3);
                         Add_FunctionType_Var_1($2);
                         /* in the second step we should write the head of    */
                         /*    the subroutine sub_loop_<subroutinename>       */
                         writeheadnewsub_0(2);
                      }
                      strcpy(nameinttypename,"");

                   }
      ;
before_function : TOK_FUNCTION
                   {
                       functiondeclarationisdone = 1;
                   }
                   ;

before_parameter : TOK_PARAMETER
                   {
                      VariableIsParameter = 1;
                      pos_curparameter = setposcur()-9;
                   }
before_data : TOK_DATA
                   {
                      pos_curdata = setposcur()-strlen($1);
                   }
data : TOK_NAME TOK_SLASH datavallist TOK_SLASH
                   {
                      if ( couldaddvariable == 1 )
                      {
                      if ( aftercontainsdeclare == 1 ) strcpy(ligne,"");
                      else sprintf(ligne,"(/ %s /)",$3);
                      Add_Data_Var_1($1,ligne);
                      }
                   }
      | data opt_comma TOK_NAME TOK_SLASH datavallist TOK_SLASH
                   {
                      if ( couldaddvariable == 1 )
                      {
                      if ( aftercontainsdeclare == 1 ) strcpy(ligne,"");
                      else sprintf(ligne,"(/ %s /)",$5);
                      Add_Data_Var_1($3,ligne);
                      }
                   }
      | datanamelist TOK_SLASH datavallist TOK_SLASH
                   {
                       /*******************************************************/
                       /*******************************************************/
                       /*******************************************************/
                       /*******************************************************/
                       /*******************************************************/
                       /*******************************************************/
                       /*******************************************************/
                   }
      ;
datavallist : expr_data
                   {
                      if ( couldaddvariable == 1 )
                      {
                         strcpy($$,$1);
                      }
                   }
      | expr_data ',' datavallist
                   {
                      if ( couldaddvariable == 1 )
                      {
                         sprintf($$,"%s,%s",$1,$3);
                      }
                   }
      ;

save :  before_save varsave
      | before_save  comblock varsave
      | save opt_comma comblock opt_comma varsave
      | save ',' varsave
      ;
before_save : TOK_SAVE
                  {
                     pos_cursave = setposcur()-4;
                  }
      ;
varsave :
      | TOK_NAME dims
                  {
                     if ( couldaddvariable == 1 ) Add_Save_Var_1($1,$2);
                  }
      ;
datanamelist : TOK_NAME
      | TOK_NAME '(' expr ')'
      | datanamelist ',' datanamelist
      ;
expr_data : opt_signe simple_const
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s%s",$1,$2);}
      | expr_data '+' expr_data
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s+%s",$1,$3);}
      | expr_data '-' expr_data
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s-%s",$1,$3);}
      | expr_data '*' expr_data
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s*%s",$1,$3);}
      | expr_data '/' expr_data
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s/%s",$1,$3);}
      ;
opt_signe :
                   {if ( couldaddvariable == 1 ) strcpy($$,"");}
      | signe
                   {if ( couldaddvariable == 1 ) strcpy($$,$1);}
      ;
namelist :  namelist_action after_namelist
      ;
namelist_action : TOK_NAMELIST  ident
      | TOK_NAMELIST  comblock ident
      | namelist_action opt_comma comblock opt_comma ident
      | namelist_action ',' ident
      ;
after_namelist :
      ;
interface : TOK_INTERFACE opt_name
      | TOK_ENDINTERFACE opt_name
      ;
before_dimension : TOK_DIMENSION
                   {
                      positioninblock=0;
                      pos_curdimension = setposcur()-9;
                   }

dimension :  before_dimension opt_comma TOK_NAME dims lengspec
      {
         if ( couldaddvariable == 1 )
         {
            /*                                                                */
            curvar=createvar($3,$4);
            /*                                                                */
            CreateAndFillin_Curvar("",curvar);
            /*                                                                */
            curlistvar=insertvar(NULL,curvar);
            /*                                                                */
            $$=settype("",curlistvar);
            /*                                                                */
            strcpy(vallengspec,"");
         }
      }
      | dimension ',' TOK_NAME dims lengspec
      {
         if ( couldaddvariable == 1 )
         {
            /*                                                                */
            curvar=createvar($3,$4);
            /*                                                                */
            CreateAndFillin_Curvar("",curvar);
            /*                                                                */
            curlistvar=insertvar($1,curvar);
            /*                                                                */
            $$=curlistvar;
            /*                                                                */
            strcpy(vallengspec,"");
         }
      }
      ;
private : TOK_PRIVATE '\n'
      | TOK_PRIVATE opt_sep use_name_list
      ;
public : TOK_PUBLIC '\n'
        {
        $$=(listname *)NULL;
        }
      | TOK_PUBLIC opt_sep use_name_list
         {
          $$=$3;
         }
      ;
use_name_list : TOK_NAME
           {
           $$ = Insertname(NULL,$1);
           }
      | use_name_list ',' TOK_NAME
          {
          $$ = Insertname($1,$3);
          }
      ;
common : before_common var_common_list
                   {
                         pos_end = setposcur();
                         RemoveWordSET_0(fortranout,pos_curcommon,
                                                  pos_end-pos_curcommon);
                   }
      | before_common comblock var_common_list
                   {
                         if ( couldaddvariable == 1 )
                         {
                            sprintf(charusemodule,"%s",$2);
                            Add_NameOfCommon_1($2);
                            pos_end = setposcur();
                            RemoveWordSET_0(fortranout,pos_curcommon,
                                                       pos_end-pos_curcommon);
                         }
                   }
      | common opt_comma comblock opt_comma var_common_list
                   {
                         if ( couldaddvariable == 1 )
                         {
                            sprintf(charusemodule,"%s",$3);
                            Add_NameOfCommon_1($3);
                            pos_end = setposcur();
                            RemoveWordSET_0(fortranout,pos_curcommon,
                                                       pos_end-pos_curcommon);
                         }
                   }
      ;
before_common : TOK_COMMON
                   {
                      positioninblock=0;
                      pos_curcommon = setposcur()-6;
                   }
      | TOK_GLOBAL TOK_COMMON
                   {
                      positioninblock=0;
                      pos_curcommon = setposcur()-6-7;
                   }
      ;
var_common_list : var_common
                   {
                      if ( couldaddvariable == 1 ) Add_Common_var_1();
                   }

     | var_common_list ',' var_common
                   {
                      if ( couldaddvariable == 1 ) Add_Common_var_1();
                   }
var_common : TOK_NAME dims
                   {
                      if ( couldaddvariable == 1 )
                      {
                         positioninblock = positioninblock + 1 ;
                         strcpy(commonvar,$1);
                         commondim = $2;
                      }
                   }
      ;
comblock : TOK_DSLASH
                   {
                      if ( couldaddvariable == 1 )
                      {
                         strcpy($$,"");
                         positioninblock=0;
                         strcpy(commonblockname,"");
                      }
                   }
      | TOK_SLASH TOK_NAME TOK_SLASH
                   {
                      if ( couldaddvariable == 1 )
                      {
                         strcpy($$,$2);
                         positioninblock=0;
                         strcpy(commonblockname,$2);
                      }
                   }
      ;
opt_comma :
      | ','
      ;
paramlist : paramitem
                   {
                      if ( couldaddvariable == 1 ) $$=insertvar(NULL,$1);
                   }
      | paramlist ',' paramitem
                   {
                      if ( couldaddvariable == 1 ) $$=insertvar($1,$3);
                   }
      ;
paramitem : TOK_NAME '=' expr
                   {
                     if ( couldaddvariable == 1 )
                     {
                         curvar=(variable *) malloc(sizeof(variable));
                         /*                                                   */
                         Init_Variable(curvar);
                         /*                                                   */
                         curvar->v_VariableIsParameter=1;
                         strcpy(curvar->v_nomvar,$1);
                         Save_Length($1,4);
                         strcpy(curvar->v_subroutinename,subroutinename);
                         Save_Length(subroutinename,11);
                         strcpy(curvar->v_modulename,curmodulename);
                         Save_Length(curmodulename,6);
                         strcpy(curvar->v_initialvalue,$3);
                         Save_Length($3,14);
                         strcpy(curvar->v_commoninfile,mainfile);
                         Save_Length(mainfile,10);
                         $$=curvar;
                      }
                   }
      ;
module_proc_stmt : TOK_PROCEDURE proc_name_list
      ;
proc_name_list : TOK_NAME
      | proc_name_list ',' TOK_NAME
      ;
implicit : TOK_IMPLICIT TOK_NONE
                    {
                       if ( insubroutinedeclare == 1 )
                       {
                          Add_ImplicitNoneSubroutine_1();
                          pos_end = setposcur();
                          RemoveWordSET_0(fortranout,pos_end-13,
                                                             13);
                       }
                    }
      | TOK_IMPLICIT TOK_REAL8
      ;
opt_retour :
      ;
dcl : options opt_retour TOK_NAME dims lengspec initial_value
                   {
                      if ( couldaddvariable == 1 )
                      {
                         /*                                                   */
                         if (dimsgiven == 1)
                         {
                            curvar=createvar($3,curdim);
                         }
                         else
                         {
                            curvar=createvar($3,$4);
                         }
                         /*                                                   */
                         CreateAndFillin_Curvar(DeclType,curvar);
                         /*                                                   */
                         curlistvar=insertvar(NULL,curvar);
                         if (!strcasecmp(DeclType,"character"))
                         {
                            if (c_selectorgiven == 1)
                            {
                               strcpy(c_selectordim.first,"1");
                               strcpy(c_selectordim.last,c_selectorname);
                               Save_Length(c_selectorname,1);
                               change_dim_char
                                     (insertdim(NULL,c_selectordim),curlistvar);
                            }
                         }
                         $$=settype(DeclType,curlistvar);
                      }
                      strcpy(vallengspec,"");
                   }
      | dcl ',' opt_retour TOK_NAME dims lengspec initial_value
                   {
                      if ( couldaddvariable == 1 )
                      {
                         if (dimsgiven == 1)
                         {
                            curvar=createvar($4,curdim);
                         }
                         else
                         {
                            curvar=createvar($4,$5);
                         }
                         /*                                                   */
                         CreateAndFillin_Curvar($1->var->v_typevar,curvar);
                         /*                                                   */
                         strcpy(curvar->v_typevar,($1->var->v_typevar));
                         Save_Length($1->var->v_typevar,3);
                         /*                                                   */
                         curlistvar=insertvar($1,curvar);
                         if (!strcasecmp(DeclType,"character"))
                         {
                            if (c_selectorgiven == 1)
                            {
                               strcpy(c_selectordim.first,"1");
                               strcpy(c_selectordim.last,c_selectorname);
                               Save_Length(c_selectorname,1);
                               change_dim_char
                                     (insertdim(NULL,c_selectordim),curlistvar);
                            }
                         }
                         $$=curlistvar;
                      }
                      strcpy(vallengspec,"");
                   }
      ;
nodimsgiven :       {dimsgiven=0;}
      ;
type : typespec selector
                   {strcpy(DeclType,$1);}
      | before_character c_selector
                   {
                      strcpy(DeclType,"CHARACTER");
                   }
      | typename '*' TOK_CSTINT
                   {
                      strcpy(DeclType,$1);
                      strcpy(nameinttypename,$3);
                   }
      | before_typepar attribute ')'
                   {
                      strcpy(DeclType,"TYPE");
                   }
      ;
before_typepar : TOK_TYPEPAR
                   {
                 /*     if ( couldaddvariable == 1 ) VarTypepar = 1 ;
                      couldaddvariable = 0 ;
                      pos_cur_decl = setposcur()-5;*/
                   pos_cur_decl = setposcur()-5;
                   }
      ;
c_selector :
      | '*' TOK_CSTINT
                   {c_selectorgiven=1;strcpy(c_selectorname,$2);}
      | '*' '(' c_attribute ')' {c_star = 1;}
      | '(' c_attribute ')'
      ;
c_attribute : TOK_NAME clause opt_clause
      | TOK_NAME '=' clause opt_clause
      | clause opt_clause
      ;
before_character : TOK_CHARACTER
                   {
                      pos_cur_decl = setposcur()-9;
                   }
      ;
typespec : typename {strcpy($$,$1);}
      ;
typename : TOK_INTEGER
                   {
                      strcpy($$,"INTEGER");
                      pos_cur_decl = setposcur()-7;
                   }
      | TOK_REAL   {
                      strcpy($$,"REAL");
                      pos_cur_decl = setposcur()-4;
                   }
      | TOK_COMPLEX
                   {strcpy($$,"COMPLEX");
                   pos_cur_decl = setposcur()-7;}
      | TOK_DOUBLEPRECISION
                   {
                      pos_cur_decl = setposcur()-16;
                      strcpy($$,"REAL");
                      strcpy(nameinttypename,"8");
                   }
      | TOK_DOUBLECOMPLEX
                   {strcpy($$,"DOUBLE COMPLEX");}
      | TOK_LOGICAL
                   {
                      strcpy($$,"LOGICAL");
                      pos_cur_decl = setposcur()-7;
                   }
      ;
lengspec :
      | '*' proper_lengspec {strcpy(vallengspec,$2);}
      ;
proper_lengspec : expr {sprintf($$,"*%s",$1);}
      | '(' '*' ')'{strcpy($$,"*(*)");}
      ;
selector :
      | '*' proper_selector
      | '(' attribute ')'
      ;
proper_selector : expr
      | '(' '*' ')'
      ;
attribute : TOK_NAME clause
      | TOK_NAME '=' clause
                   {
                      if ( strstr($3,"0.d0") )
                      {
                         strcpy(nameinttypename,"8");
                         sprintf(NamePrecision,"");
                      }
                      else sprintf(NamePrecision,"%s = %s",$1,$3);
                   }
      | TOK_NAME
                   {
                      strcpy(NamePrecision,$1);
                   }
      | TOK_CSTINT
                   {
                      strcpy(NamePrecision,$1);
                   }
      ;
clause : expr       {strcpy(CharacterSize,$1);
                    strcpy($$,$1);}
      | '*'        {strcpy(CharacterSize,"*");
                    strcpy($$,"*");}
      ;
opt_clause :
      | ',' TOK_NAME clause
      ;
options :
      | ':' ':'
      | ',' attr_spec_list ':' ':'
      ;
attr_spec_list : attr_spec
      | attr_spec_list ',' attr_spec
      ;
attr_spec : TOK_PARAMETER
                   {
                      VariableIsParameter = 1;
                   }
      | access_spec
      | TOK_ALLOCATABLE
                   {Allocatabledeclare = 1;}
      | TOK_DIMENSION dims
                   {
                      dimsgiven=1;
                      curdim=$2;
                   }
      | TOK_EXTERNAL
                   {ExternalDeclare = 1;}
      | TOK_INTENT '(' intent_spec ')'
                   {strcpy(IntentSpec,$3);}
      | TOK_INTRINSIC
      | TOK_OPTIONAL{optionaldeclare = 1 ;}
      | TOK_POINTER {pointerdeclare = 1 ;}
      | TOK_SAVE    {
/*                       if ( inmodulemeet == 1 )
                       {*/
                          SaveDeclare = 1 ;
                     /*  }*/
                    }
      | TOK_TARGET
      ;
intent_spec : TOK_IN {strcpy($$,$1);}
      | TOK_OUT     {strcpy($$,$1);}
      | TOK_INOUT   {strcpy($$,$1); }
      ;
access_spec : TOK_PUBLIC
                   {PublicDeclare = 1;}
      | TOK_PRIVATE
                   {PrivateDeclare = 1;}
      ;
dims :              {if ( created_dimensionlist == 1 )
                       {
                           $$=(listdim *)NULL;
                       }
                   }
      | '(' dimlist ')'
                   {if ( created_dimensionlist == 1 ||
                         agrif_parentcall      == 1 ) $$=$2;}
      ;
dimlist :   dim     {if ( created_dimensionlist == 1 ||
                         agrif_parentcall      == 1 ) $$=insertdim(NULL,$1);}
      | dimlist ',' dim
                   {if ( couldaddvariable == 1 )
                         if ( created_dimensionlist == 1 ) $$=insertdim($1,$3);}
      ;
dim : ubound         {
                      strcpy($$.first,"1");
                      strcpy($$.last,$1);
                      Save_Length($1,1);
                   }
      | ':'        {
                      strcpy($$.first,"");
                      strcpy($$.last,"");
                   }
      | expr ':'   {
                      strcpy($$.first,$1);
                      Save_Length($1,2);
                      strcpy($$.last,"");
                   }
      | ':' expr   {
                      strcpy($$.first,"");
                      strcpy($$.last,$2);
                      Save_Length($2,1);
                   }
      | expr ':' ubound
                   {
                      strcpy($$.first,$1);
                      Save_Length($1,2);
                      strcpy($$.last,$3);
                      Save_Length($3,1);
                   }
      ;
ubound :  '*'       {strcpy($$,"*");}
      | expr       {strcpy($$,$1);}
      ;
expr :  uexpr       {if ( couldaddvariable == 1 ) strcpy($$,$1);}
      | '(' expr ')'
                   {if ( couldaddvariable == 1 ) sprintf($$,"(%s)",$2);}
      | complex_const
                   {if ( couldaddvariable == 1 ) strcpy($$,$1);}
      | predefinedfunction
                   {if ( couldaddvariable == 1 ) strcpy($$,$1);}
      ;

predefinedfunction : TOK_SUM minmaxlist ')'
                   {sprintf($$,"SUM(%s)",$2);}
      | TOK_MAX minmaxlist ')'
                   {sprintf($$,"MAX(%s)",$2);}
      | TOK_TANH '(' minmaxlist ')'
                   {sprintf($$,"TANH(%s)",$3);}
      | TOK_MAXVAL '(' minmaxlist ')'
                   {sprintf($$,"MAXVAL(%s)",$3);}
      | TOK_MIN minmaxlist ')'
                   {sprintf($$,"MIN(%s)",$2);}
      | TOK_MINVAL '(' minmaxlist ')'
                   {sprintf($$,"MINVAL(%s)",$3);}
      | TOK_TRIM '(' expr ')'
                   {sprintf($$,"TRIM(%s)",$3);}
      | TOK_SQRT expr ')'
                   {sprintf($$,"SQRT(%s)",$2);}
      | TOK_REAL '(' minmaxlist ')'
                   {sprintf($$,"REAL(%s)",$3);}
      | TOK_NINT '(' expr ')'
                   {sprintf($$,"NINT(%s)",$3);}
      | TOK_FLOAT '(' expr ')'
                   {sprintf($$,"FLOAT(%s)",$3);}
      | TOK_EXP '(' expr ')'
                   {sprintf($$,"EXP(%s)",$3);}
      | TOK_COS '(' expr ')'
                   {sprintf($$,"COS(%s)",$3);}
      | TOK_COSH '(' expr ')'
                   {sprintf($$,"COSH(%s)",$3);}
      | TOK_ACOS '(' expr ')'
                   {sprintf($$,"ACOS(%s)",$3);}
      | TOK_SIN '(' expr ')'
                   {sprintf($$,"SIN(%s)",$3);}
      | TOK_SINH '(' expr ')'
                   {sprintf($$,"SINH(%s)",$3);}
      | TOK_ASIN '(' expr ')'
                   {sprintf($$,"ASIN(%s)",$3);}
      | TOK_LOG '(' expr ')'
                   {sprintf($$,"LOG(%s)",$3);}
      | TOK_TAN '(' expr ')'
                   {sprintf($$,"TAN(%s)",$3);}
      | TOK_ATAN '(' expr ')'
                   {sprintf($$,"ATAN(%s)",$3);}
      | TOK_ABS expr ')'
                   {sprintf($$,"ABS(%s)",$2);}
      | TOK_MOD '(' minmaxlist ')'
                   {sprintf($$,"MOD(%s)",$3);}
      | TOK_SIGN '(' minmaxlist ')'
                   {sprintf($$,"SIGN(%s)",$3);}
      | TOK_MINLOC '(' minmaxlist ')'
                   {sprintf($$,"MINLOC(%s)",$3);}
      | TOK_MAXLOC '(' minmaxlist ')'
                   {sprintf($$,"MAXLOC(%s)",$3);}
      ;
minmaxlist : expr {strcpy($$,$1);}
      | minmaxlist ',' expr
                   {if ( couldaddvariable == 1 )
                   { strcpy($$,$1);strcat($$,",");strcat($$,$3);}}
      ;
uexpr :  lhs        {if ( couldaddvariable == 1 ) strcpy($$,$1);}
      | simple_const
                   {if ( couldaddvariable == 1 ) strcpy($$,$1);}
      | vec
                   {if ( couldaddvariable == 1 ) strcpy($$,$1);}
      | expr operation
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s%s",$1,$2);}
      | signe expr %prec '* ' 
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s%s",$1,$2);}
      | TOK_NOT expr
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s%s",$1,$2);}
      ;
signe : '+'        {if ( couldaddvariable == 1 ) strcpy($$,"+");}
      | '-'        {if ( couldaddvariable == 1 ) strcpy($$,"-");}
      ;
operation : '+' expr %prec '+'
                   {if ( couldaddvariable == 1 ) sprintf($$,"+%s",$2);}
      |  '-' expr %prec '+'
                   {if ( couldaddvariable == 1 ) sprintf($$,"-%s",$2);}
      |  '*' expr
                   {if ( couldaddvariable == 1 ) sprintf($$,"*%s",$2);}
      |  TOK_DASTER expr
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s%s",$1,$2);}
      |  TOK_EQ expr %prec TOK_EQ
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s%s",$1,$2);}
      |  TOK_EQV expr %prec TOK_EQV
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s%s",$1,$2);}                   
      |  TOK_GT expr %prec TOK_EQ
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s%s",$1,$2);}
      |  '>' expr %prec TOK_EQ
                   {if ( couldaddvariable == 1 ) sprintf($$," > %s",$2);}
      |  TOK_LT expr %prec TOK_EQ
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s%s",$1,$2);}
      |  '<' expr %prec TOK_EQ
                   {if ( couldaddvariable == 1 ) sprintf($$," < %s",$2);}
      |  TOK_GE expr %prec TOK_EQ
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s%s",$1,$2);}
      |  '>''=' expr %prec TOK_EQ
                   {if ( couldaddvariable == 1 ) sprintf($$," >= %s",$3);}
      |  TOK_LE expr %prec TOK_EQ
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s%s",$1,$2);}
      |  '<''=' expr %prec TOK_EQ
                   {if ( couldaddvariable == 1 ) sprintf($$," <= %s",$3);}
      |  TOK_NE expr %prec TOK_EQ
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s%s",$1,$2);}
      |  TOK_NEQV expr %prec TOK_EQV
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s%s",$1,$2);}                   
      |  TOK_XOR expr
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s%s",$1,$2);}
      |  TOK_OR expr
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s%s",$1,$2);}
      |  TOK_AND expr
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s%s",$1,$2);}
      |  TOK_SLASH after_slash
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s",$2);}
      |  '=' after_equal
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s",$2);}

after_slash : {strcpy($$,"");}
      | expr
                   {sprintf($$,"/%s",$1);}
      | '=' expr %prec TOK_EQ
                   {sprintf($$,"/= %s",$2);}
      | TOK_SLASH expr
                   {sprintf($$,"//%s",$2);}
      ;
after_equal : '=' expr %prec TOK_EQ
                   {if ( couldaddvariable == 1 ) sprintf($$,"==%s",$2);}
      | expr
                   {if ( couldaddvariable == 1 ) sprintf($$,"= %s",$1);}
      ;

lhs : ident         {if ( couldaddvariable == 1 ) strcpy($$,$1);}
      | structure_component
                   {if ( couldaddvariable == 1 ) strcpy($$,$1);}
      | array_ele_substring_func_ref
                   {if ( couldaddvariable == 1 ) strcpy($$,$1);}
      ;
beforefunctionuse : {
                      agrif_parentcall =0;
                      if (!strcasecmp(identcopy,"Agrif_Parent") )
                                                            agrif_parentcall =1;
                      if ( Agrif_in_Tok_NAME(identcopy) == 1 )
                      {
                         inagrifcallargument = 1;
                         Add_SubroutineWhereAgrifUsed_1(subroutinename,
                                                        curmodulename);
                      }
                   }
      ;
array_ele_substring_func_ref : begin_array
                   {
                     strcpy($$,$1);
                     if ( incalldeclare == 0 ) inagrifcallargument = 0;
                   }
      | begin_array substring
                   {if ( couldaddvariable == 1 ) sprintf($$," %s %s ",$1,$2);}
      | structure_component '(' funarglist ')'
                   {if ( couldaddvariable == 1 )
                                                sprintf($$," %s ( %s )",$1,$3);}
      | structure_component '(' funarglist ')' substring
                   {if ( couldaddvariable == 1 )
                                         sprintf($$," %s ( %s ) %s ",$1,$3,$5);}
      ;
begin_array : ident '(' funarglist ')'
                   {
                      if ( couldaddvariable == 1 )
                      {
                         sprintf($$," %s ( %s )",$1,$3);
                         ModifyTheAgrifFunction_0($3);
                         agrif_parentcall =0;
                      }
                   }
      ;
structure_component : lhs '%' lhs
                   {
                      sprintf($$," %s %% %s ",$1,$3);
                      if ( incalldeclare == 0 ) inagrifcallargument = 0;
                   }
      ;
vec :  TOK_LEFTAB outlist TOK_RIGHTAB
                   {sprintf($$,"(/%s/)",$2);}
      ;
funarglist : beforefunctionuse    {strcpy($$," ");}
      | beforefunctionuse funargs
                   {strcpy($$,$2);}
      ;
funargs : funarg     {if ( couldaddvariable == 1 ) strcpy($$,$1);}
      | funargs ',' funarg
                    {if ( couldaddvariable == 1 ) sprintf($$,"%s,%s",$1,$3);}
      ;
funarg : expr       {strcpy($$,$1);}
      | triplet    {strcpy($$,$1);}
      ;
triplet : expr ':' expr
                    {if ( couldaddvariable == 1 ) sprintf($$,"%s :%s",$1,$3);}
      | expr ':' expr ':' expr
                    {if ( couldaddvariable == 1 )
                                               sprintf($$,"%s :%s :%s",$1,$3,$5);}
      | ':' expr ':' expr
                    {if ( couldaddvariable == 1 ) sprintf($$,":%s :%s",$2,$4);}
      | ':' ':' expr{if ( couldaddvariable == 1 ) sprintf($$,": : %s",$3);}
      | ':' expr    {if ( couldaddvariable == 1 ) sprintf($$,":%s",$2);}
      | expr ':'    {if ( couldaddvariable == 1 ) sprintf($$,"%s :",$1);}
      | ':'         {if ( couldaddvariable == 1 ) sprintf($$,":");}
      ;
ident : TOK_NAME    {
                       if ( couldaddvariable == 1 )
                       {
                       if ( Vartonumber($1) == 1 )
                       {
                          Add_SubroutineWhereAgrifUsed_1(subroutinename,
                                                        curmodulename);
                       }
                       if (!strcasecmp($1,"Agrif_Parent") )
                                                            agrif_parentcall =1;
                       if ( VariableIsNotFunction($1) == 0 )
                       {
                          if ( inagrifcallargument == 1 )
                          {
                             if ( !strcasecmp($1,identcopy) )
                             {
                                strcpy(sameagrifname,identcopy);
                                sameagrifargument = 1;
                             }
                          }
                          strcpy(identcopy,$1);
                          pointedvar=0;
                          strcpy(truename,$1);
                          if (variscoupled_0($1)) strcpy(truename,getcoupledname_0($1));
/*
                          if ( VarIsNonGridDepend($1) == 0 &&
                               Variableshouldberemove($1) == 0 )
                          {
                             if ( inagrifcallargument == 1 ||
                                  varisallocatable_0($1) == 1 ||
                                  varispointer_0($1) == 1 )
                             {
                            if ((IsinListe(List_UsedInSubroutine_Var,$1) == 1) || (inagrifcallargument == 1))
                             {
                              if (varistyped_0($1) == 0)
                                 {
                                 ModifyTheVariableName_0($1);
                                 }
                                 }
                                 else
                                 {
                                 }
                             }
                             if (variscoupled_0($1) == 1)
                             {
      printf("mla variable %s est couplee %s\n",$1,getcoupledname_0($1));
                             ModifyTheVariableNamecoupled_0($1,getcoupledname_0($1));
                             }
                             if ( inagrifcallargument != 1 ||
                                  sameagrifargument ==1 )
                                  Add_UsedInSubroutine_Var_1($1);
                          }
                          NotifyAgrifFunction_0($1);
*/
                          if ( VarIsNonGridDepend(truename) == 0 &&
                               Variableshouldberemove(truename) == 0 )
                          {
                             if ( inagrifcallargument == 1 ||
                                  varisallocatable_0(truename) == 1 ||
                                  varispointer_0(truename) == 1 )
                             {
                            if ((IsinListe(List_UsedInSubroutine_Var,$1) == 1) || (inagrifcallargument == 1))
                             {
                              if (varistyped_0(truename) == 0)
                                 {
                                 ModifyTheVariableName_0(truename,strlen($1));
                                 }
                                 }
                             }
                             if ( inagrifcallargument != 1 ||
                                  sameagrifargument ==1 )
                                  Add_UsedInSubroutine_Var_1(truename);
                          }
                          NotifyAgrifFunction_0(truename);
                       }
                       }
                    }
      ;
simple_const : TOK_TRUE
                     {if ( couldaddvariable == 1 ) strcpy($$,".TRUE.");}
      | TOK_FALSE    {if ( couldaddvariable == 1 ) strcpy($$,".FALSE.");}
      | TOK_CSTINT   {if ( couldaddvariable == 1 ) strcpy($$,$1);}
      | TOK_CSTREAL  {if ( couldaddvariable == 1 ) strcpy($$,$1);}
      | TOK_CSTREALDP{if ( couldaddvariable == 1 ) strcpy($$,$1);}
      | TOK_CSTREALQP{if ( couldaddvariable == 1 ) strcpy($$,$1);}
      | simple_const TOK_NAME
                     {if ( couldaddvariable == 1 ) sprintf($$,"%s%s",$1,$2);}
      | string_constant opt_substring
      ;
string_constant : TOK_CHAR_CONSTANT
                     {if ( couldaddvariable == 1 ) strcpy($$,$1);}
      | string_constant TOK_CHAR_CONSTANT
      | TOK_CHAR_MESSAGE
                     {if ( couldaddvariable == 1 ) strcpy($$,$1);}
      | TOK_CHAR_CUT
                     {if ( couldaddvariable == 1 ) strcpy($$,$1);}
      ;
opt_substring :      {if ( couldaddvariable == 1 ) strcpy($$," ");}
      | substring   {if ( couldaddvariable == 1 ) strcpy($$,$1);}
      ;
substring : '(' optexpr ':' optexpr ')'
                    {if ( couldaddvariable == 1 ) sprintf($$,"(%s :%s)",$2,$4);}
      ;
optexpr :           {if ( couldaddvariable == 1 ) strcpy($$," ");}
      | expr        {if ( couldaddvariable == 1 ) strcpy($$,$1);}
      ;
opt_expr : '\n'          {if ( couldaddvariable == 1 ) strcpy($$," ");}
      | expr        {if ( couldaddvariable == 1 ) strcpy($$,$1);}
      ;
initial_value :      {InitialValueGiven = 0;}
      | before_initial '=' expr
                    {
                       if ( couldaddvariable == 1 )
                       {
                          strcpy(InitValue,$3);
                          InitialValueGiven = 1;
                       }
                    }
      ;
before_initial : {pos_curinit = setposcur();}
      ;
complex_const : '(' uexpr ',' uexpr ')'
                    {sprintf($$,"(%s,%s)",$2,$4);}
      ;
use_stat : word_use  module_name
                    {
                      if ( couldaddvariable == 1 )
                      {
                      /* if variables has been declared in a subroutine       */
                      if (insubroutinedeclare == 1)
                      {
                         copyuse_0($2);
                      }
                      sprintf(charusemodule,"%s",$2);
                      Add_NameOfModuleUsed_1($2);

                      if ( inmoduledeclare == 0 )
                      {
                         pos_end = setposcur();
                         RemoveWordSET_0(fortranout,pos_curuse,
                                               pos_end-pos_curuse);
                      }
                      }
                    }
      | word_use  module_name ',' rename_list
                    {
                       if ( couldaddvariable == 1 )
                       {
                      if (insubroutinedeclare == 1)
                      {
                         Add_CouplePointed_Var_1($2,$4);
                      }
                      if ( firstpass == 1 )
                      {
                         if ( insubroutinedeclare == 1 )
                         {
                            coupletmp = $4;
                            strcpy(ligne,"");
                            while ( coupletmp )
                            {
                               strcat(ligne,coupletmp->c_namevar);
                               strcat(ligne," => ");
                               strcat(ligne,coupletmp->c_namepointedvar);
                               coupletmp = coupletmp->suiv;
                               if ( coupletmp ) strcat(ligne,",");
                            }
                            sprintf(charusemodule,"%s",$2);
                         }
                         Add_NameOfModuleUsed_1($2);
                      }
                      if ( inmoduledeclare == 0 )
                      {
                         pos_end = setposcur();
                         RemoveWordSET_0(fortranout,pos_curuse,
                                               pos_end-pos_curuse);
                      }
                      }
                    }
      | word_use  module_name ',' TOK_ONLY ':' '\n'
                    {
                       if ( couldaddvariable == 1 )
                       {
                      /* if variables has been declared in a subroutine       */
                      if (insubroutinedeclare == 1)
                      {
                         copyuseonly_0($2);
                      }
                      sprintf(charusemodule,"%s",$2);
                      Add_NameOfModuleUsed_1($2);

                       if ( inmoduledeclare == 0 )
                       {
                          pos_end = setposcur();
                          RemoveWordSET_0(fortranout,pos_curuse,
                                                pos_end-pos_curuse);
                       }
                       }
                    }
      | word_use  module_name ',' TOK_ONLY ':' only_list
                    {
                       if ( couldaddvariable == 1 )
                       {
                       /* if variables has been declared in a subroutine      */
                       if (insubroutinedeclare == 1)
                       {
                          Add_CouplePointed_Var_1($2,$6);
                       }
                       if ( firstpass == 1 )
                       {
                         if ( insubroutinedeclare == 1 )
                         {
                             coupletmp = $6;
                             strcpy(ligne,"");
                             while ( coupletmp )
                             {
                                strcat(ligne,coupletmp->c_namevar);
                               if ( strcasecmp(coupletmp->c_namepointedvar,"") )
                                                           strcat(ligne," => ");
                                strcat(ligne,coupletmp->c_namepointedvar);
                                coupletmp = coupletmp->suiv;
                                if ( coupletmp ) strcat(ligne,",");
                             }
                             sprintf(charusemodule,"%s",$2);
                          }
                          Add_NameOfModuleUsed_1($2);
                       }
                       if ( firstpass == 0 )
                       {
                          if ( inmoduledeclare == 0 )
                          {

                            pos_end = setposcur();
                             RemoveWordSET_0(fortranout,pos_curuse,
                                                   pos_end-pos_curuse);
                       if (oldfortranout) 
                         variableisglobalinmodule($6,$2,oldfortranout,pos_curuseold);
                        
                          }
                          else
                          {

                             /* if we are in the module declare and if the    */
                             /* onlylist is a list of global variable         */
                             variableisglobalinmodule($6, $2, fortranout,pos_curuse);
                          }
                       }
                       }
                    }
      ;
word_use : TOK_USE
                   {
                      pos_curuse = setposcur()-strlen($1);
                     if (firstpass == 0 && oldfortranout) {
                     pos_curuseold = setposcurname(oldfortranout);
                     }
                   }
      ;
module_name : TOK_NAME
                    {strcpy($$,$1);}
      ;
rename_list : rename_name
                    {
                       if ( couldaddvariable == 1 ) $$ = $1;
                    }
      | rename_list ',' rename_name
                    {
                        if ( couldaddvariable == 1 )
                        {
                        /* insert the variable in the list $1                 */
                        $3->suiv = $1;
                        $$ = $3;
                        }
                    }
      ;
rename_name : TOK_NAME TOK_POINT_TO TOK_NAME
                    {
                       coupletmp =(listcouple *)malloc(sizeof(listcouple));
                       strcpy(coupletmp->c_namevar,$1);
                       Save_Length($1,21);
                       strcpy(coupletmp->c_namepointedvar,$3);
                       Save_Length($3,22);
                       coupletmp->suiv = NULL;
                       $$ = coupletmp;
                     }
      ;
only_list : only_name
                    {
                       if ( couldaddvariable == 1 ) $$ = $1;
                    }
      | only_list ',' only_name
                    {
                        if ( couldaddvariable == 1 )
                        {
                        /* insert the variable in the list $1                 */
                        $3->suiv = $1;
                        $$ = $3;
                        }
                    }
      ;
only_name : TOK_NAME TOK_POINT_TO TOK_NAME
                    {
                       coupletmp =(listcouple *)malloc(sizeof(listcouple));
                       strcpy(coupletmp->c_namevar,$1);
                       Save_Length($1,21);
                       strcpy(coupletmp->c_namepointedvar,$3);
                       Save_Length($3,22);
                       coupletmp->suiv = NULL;
                       $$ = coupletmp;
                       pointedvar=1;
                       Add_UsedInSubroutine_Var_1($1);
                    }
      | TOK_NAME    {
                       coupletmp =(listcouple *)malloc(sizeof(listcouple));
                       strcpy(coupletmp->c_namevar,$1);
                       Save_Length($1,21);
                       strcpy(coupletmp->c_namepointedvar,"");
                       coupletmp->suiv = NULL;
                       $$ = coupletmp;
                     }
      ;
exec : iffable
      | TOK_ALLOCATE '(' allocation_list opt_stat_spec ')'
                     {
                         Add_SubroutineWhereAgrifUsed_1(subroutinename,
                                                        curmodulename);
                                                        inallocate = 0;
                     }
      | TOK_DEALLOCATE '(' allocate_object_list opt_stat_spec ')'
                     {
                          Add_SubroutineWhereAgrifUsed_1(subroutinename,
                                                         curmodulename);
                                                         inallocate = 0;
                     }
      | TOK_NULLIFY '(' pointer_name_list ')'
      | word_endunit /* end                                                   */
                    {
                       GlobalDeclaration = 0 ;
                       if ( firstpass == 0 &&
                            strcasecmp(subroutinename,"") )
                       {
                          if ( module_declar && insubroutinedeclare == 0 )
                          {
                              fclose(module_declar);
                          }
                       }
                       if ( couldaddvariable == 1 &&
                            strcasecmp(subroutinename,"") )
                       {
                       if ( inmodulemeet == 1 )
                       {
                         /* we are in a module                                */
                         if ( insubroutinedeclare == 1 )
                         {
                            /* it is like an end subroutine <name>            */
                            insubroutinedeclare = 0 ;
                            /*                                                */
                            pos_cur = setposcur();
                            closeandcallsubloopandincludeit_0(1);
                            functiondeclarationisdone = 0;
                         }
                         else
                         {
                            /* it is like an end module <name>                */
                            inmoduledeclare = 0 ;
                            inmodulemeet = 0 ;
                         }
                       }
                       else
                       {
                          insubroutinedeclare = 0;
                          /*                                                  */
                          pos_cur = setposcur();                        
                          closeandcallsubloopandincludeit_0(2);
                            functiondeclarationisdone = 0;
                          if ( firstpass == 0 )
                          {
                             if ( retour77 == 0 ) fprintf(paramout,"!\n");
                             else fprintf(paramout,"C\n");
                             fclose(paramout);
                           }
                        }
                      }
                      strcpy(subroutinename,"");
                    }
      | word_endprogram opt_name
                    {
                       if ( couldaddvariable == 1 )
                       {
                       insubroutinedeclare = 0;
                       /*                                                     */
                       pos_cur = setposcur();                      
                       closeandcallsubloopandincludeit_0(3);
                            functiondeclarationisdone = 0;
                      if ( firstpass == 0 )
                      {
                         if ( retour77 == 0 ) fprintf(paramout,"!\n");
                         else fprintf(paramout,"C\n");
                         fclose(paramout);
                      }
                      strcpy(subroutinename,"");
                      }
                    }
      | word_endsubroutine opt_name
                    {
                       if ( couldaddvariable == 1 &&
                            strcasecmp(subroutinename,"") )
                       {
                       insubroutinedeclare = 0;
                       /*                                                     */
                       pos_cur = setposcur();
                                             
                       closeandcallsubloopandincludeit_0(1);
                            functiondeclarationisdone = 0;
                      if ( firstpass == 0 )
                      {
                         if ( retour77 == 0 ) fprintf(paramout,"!\n");
                         else fprintf(paramout,"C\n");
                         fclose(paramout);
                      }
                      strcpy(subroutinename,"");
                      }
                    }
      | word_endfunction opt_name
                    {
                       if ( couldaddvariable == 1 )
                       {
                       insubroutinedeclare = 0;
                       /*                                                     */
                       pos_cur = setposcur();

                       closeandcallsubloopandincludeit_0(0);
                            functiondeclarationisdone = 0;
                      if ( firstpass == 0 )
                      {
                         if ( retour77 == 0 ) fprintf(paramout,"!\n");
                         else fprintf(paramout,"C\n");
                         fclose(paramout);
                      }
                      strcpy(subroutinename,"");
                      }
                    }
      | TOK_ENDMODULE opt_name
                    {
                       if ( couldaddvariable == 1 )
                       {
                       /* if we never meet the contains keyword               */
                      Remove_Word_end_module_0(strlen($2));
                       if ( inmoduledeclare == 1 )
                       {
                          if ( aftercontainsdeclare == 0 )
                          {
                             Write_GlobalParameter_Declaration_0();
                             Write_NotGridDepend_Declaration_0();
                             Write_GlobalType_Declaration_0();
                             Write_Alloc_Subroutine_For_End_0();
                          }
                       }
                                           
                       inmoduledeclare = 0 ;
                       inmodulemeet = 0 ;

                      Write_Word_end_module_0();
                      strcpy(curmodulename,"");
                      aftercontainsdeclare = 1;
                      if ( firstpass == 0 )
                      {
                         if ( module_declar && insubroutinedeclare == 0)
                         {
                           fclose(module_declar);
                         }
                      }
                      GlobalDeclaration = 0 ;
                      }
                  }
      | boucledo
      | logif iffable
      | TOK_WHERE '(' expr ')' opt_expr
      | TOK_ELSEWHERE
      | TOK_ENDWHERE
      | logif TOK_THEN
      | TOK_ELSEIF  '(' expr ')' TOK_THEN
      | TOK_ELSE
      | TOK_ENDIF opt_name
      | TOK_CASE caselist ')'
      | TOK_SELECTCASE '(' expr ')'
      | TOK_CASEDEFAULT
      | TOK_ENDSELECT
      | TOK_CONTAINS
                   {
                      if (inmoduledeclare == 1 )
                      {
                         Remove_Word_Contains_0();
                         Write_GlobalParameter_Declaration_0();
                         Write_GlobalType_Declaration_0();
                         Write_NotGridDepend_Declaration_0();
                         Write_Alloc_Subroutine_0();
                         inmoduledeclare = 0 ;
                         aftercontainsdeclare = 1;
                      }
                      else
                      {
                       if ( couldaddvariable == 1 )
                       {
                          if ( firstpass == 1 ) List_ContainsSubroutine =
                                                Addtolistnom(subroutinename,
                                                     List_ContainsSubroutine,0);
                          insubroutinedeclare = 0;
                          /*                                                  */

                          closeandcallsubloop_contains_0();
                            functiondeclarationisdone = 0;
                         if ( firstpass == 0 )
                         {
                            if ( retour77 == 0 ) fprintf(paramout,"!\n");
                            else fprintf(paramout,"C\n");
                            fclose(paramout);
                         }
                         }
                         strcpy(subroutinename,"");
                      }
                   }
      ;
word_endsubroutine : TOK_ENDSUBROUTINE
                    {
                      if ( couldaddvariable == 1 )
                      {
                       strcpy($$,$1);
                       pos_endsubroutine = setposcur()-strlen($1);
                       functiondeclarationisdone = 0;
                       }
                    }
      ;
word_endunit : TOK_ENDUNIT
                    {
                      if ( couldaddvariable == 1 )
                      {
                       strcpy($$,$1);
                       pos_endsubroutine = setposcur()-strlen($1);
                       }
                    }
      ;
word_endprogram :  TOK_ENDPROGRAM
                    {
                      if ( couldaddvariable == 1 )
                      {
                       strcpy($$,$1);
                       pos_endsubroutine = setposcur()-strlen($1);
                       }
                    }
      ;
word_endfunction : TOK_ENDFUNCTION
                    {
                      if ( couldaddvariable == 1 )
                      {
                       strcpy($$,$1);
                       pos_endsubroutine = setposcur()-strlen($1);
                       }
                    }
      ;
caselist : expr
      | caselist ',' expr
      | caselist ':' expr
      ;
boucledo : worddo opt_int do_arg
      | wordwhile expr
      | TOK_ENDDO optname
      ;
do_arg :
      | do_var '=' expr ',' expr
      | do_var '=' expr ',' expr ',' expr
opt_int :
      | TOK_CSTINT opt_comma
      ;
opt_name : '\n'  {strcpy($$,"");}
      | TOK_NAME {strcpy($$,$1);}
      ;
optname :
      | TOK_NAME
      ;
worddo :  TOK_PLAINDO
      ;
wordwhile :TOK_DOWHILE
      ;

dotarget :
      | TOK_CSTINT
      ;

iffable : TOK_CONTINUE
      | ident_dims after_ident_dims
      | goto
      | io
      | call
      | TOK_ALLOCATE '(' allocation_list opt_stat_spec ')'
                     {
                          Add_SubroutineWhereAgrifUsed_1(subroutinename,
                                                        curmodulename);
                                                        inallocate = 0;
                     }
      | TOK_DEALLOCATE '(' allocate_object_list opt_stat_spec ')'
                     {
                          Add_SubroutineWhereAgrifUsed_1(subroutinename,
                                                        curmodulename);
                                                        inallocate = 0;
                     }
      | TOK_EXIT optexpr
      | TOK_RETURN opt_expr
      | TOK_CYCLE opt_expr
      | stop opt_expr
      | int_list
      ;
before_dims : {if ( couldaddvariable == 1 ) created_dimensionlist = 0;}
ident_dims : ident before_dims dims dims
              {
                  created_dimensionlist = 1;
                  if  ( agrif_parentcall == 1 )
                  {
                      ModifyTheAgrifFunction_0($3->dim.last);
                      agrif_parentcall =0;
                      fprintf(fortranout," = ");
                  }
              }
      | ident_dims '%' ident before_dims dims dims
      {created_dimensionlist = 1;}
int_list : TOK_CSTINT
      | int_list ',' TOK_CSTINT
      ;
after_ident_dims : '=' expr
      | TOK_POINT_TO expr
      ;
call : keywordcall opt_call
                   {
                      inagrifcallargument = 0 ;
                      incalldeclare=0;
                      if ( oldfortranout &&
                           !strcasecmp(meetagrifinitgrids,subroutinename) &&
                           firstpass == 0 &&
                           callmpiinit == 1)
                      {
                      /*   pos_end = setposcur();
                         RemoveWordSET_0(fortranout,pos_curcall,
                                               pos_end-pos_curcall);
                         fprintf(oldfortranout,"      Call MPI_Init (%s) \n"
                                                                   ,mpiinitvar);*/
                      }
                      if ( oldfortranout           &&
                           callagrifinitgrids == 1 &&
                           firstpass == 0 )
                      {
                         pos_end = setposcur();
                         RemoveWordSET_0(fortranout,pos_curcall,
                                               pos_end-pos_curcall);

                         strcpy(subofagrifinitgrids,subroutinename);
                      }
                      Instanciation_0(sameagrifname);
                   }
      ;
opt_call :
      | '(' opt_callarglist  ')'
      ;
opt_callarglist :
      | callarglist
      ;
keywordcall : before_call TOK_NAME
                    {
                       if (!strcasecmp($2,"MPI_Init") )
                       {
                          callmpiinit = 1;
                       }
                       else
                       {
                          callmpiinit = 0;
                       }
                       if (!strcasecmp($2,"Agrif_Init_Grids") )
                       {
                          callagrifinitgrids = 1;
                          strcpy(meetagrifinitgrids,subroutinename);
                       }
                       else callagrifinitgrids = 0;
                       if ( !strcasecmp($2,"Agrif_Open_File") )
                       {
                          Add_SubroutineWhereAgrifUsed_1(subroutinename,
                                                        curmodulename);
                       }
                       if ( Vartonumber($2) == 1 )
                       {
                          incalldeclare=1;
                          inagrifcallargument = 1 ;
                          Add_SubroutineWhereAgrifUsed_1(subroutinename,
                                                        curmodulename);
                       }
                    }
      ;
before_call : TOK_CALL
                    {pos_curcall=setposcur()-4;}
callarglist :  callarg
      | callarglist ',' callarg
      ;

callarg :  expr {
                  if ( callmpiinit == 1 )
                  {
                     strcpy(mpiinitvar,$1);
                     if ( firstpass == 1 )
                     {
                        Add_UsedInSubroutine_Var_1 (mpiinitvar);
/*                        curvar=createvar($1,NULL);
                        curlistvar=insertvar(NULL,curvar);
                        List_Subr outineArgument_Var = AddListvarToListvar
                         (curlistvar,List_SubroutineAr gument_Var,1);*/
                     }
                  }
               }
      | '*' label
      ;

stop : TOK_PAUSE
      | TOK_STOP
      ;

io : iofctl ioctl
      | read option_read
      | write ioctl
      | write ioctl outlist
      | TOK_REWIND after_rewind
      | TOK_FORMAT
      ;
opt_CHAR_INT :
      | TOK_CSTINT TOK_NAME
      ;
idfile : '*'
      | TOK_CSTINT
      | ident
      ;
option_print :
      | ',' outlist
      ;
option_inlist :
      | inlist
      ;
option_read : ioctl option_inlist
      | infmt opt_inlist
      ;
opt_outlist :
      | outlist
      ;
opt_inlist :
      | ',' inlist
      ;
ioctl :  '(' ctllist ')'
      | '(' fexpr ')'
      ;
after_rewind :  '(' ident ')'
      | '(' TOK_CSTINT ')'
      | TOK_CSTINT
      | '(' uexpr ')'
      | TOK_NAME
      ;
ctllist : ioclause
      | ctllist ',' ioclause
      ;
ioclause : fexpr
      | '*'
      | TOK_DASTER
      | TOK_NAME expr
      | TOK_NAME expr '%' ident_dims
      | TOK_NAME '(' triplet ')'
      | TOK_NAME '*'
      | TOK_NAME TOK_DASTER
      ;
iofctl : TOK_OPEN
      | TOK_CLOSE
      ;
infmt :  unpar_fexpr
      | '*'
      ;

read :TOK_READ
      | TOK_INQUIRE
      | TOK_PRINT
      ;

write : TOK_WRITE
      ;

fexpr : unpar_fexpr
      | '(' fexpr ')'
      ;
unpar_fexpr : lhs
      | simple_const
      | fexpr addop fexpr %prec '+'
      | fexpr '*' fexpr
      | fexpr TOK_SLASH fexpr
      | fexpr TOK_DASTER fexpr
      | addop fexpr %prec '*'
      | fexpr TOK_DSLASH fexpr
      | TOK_FILE expr
      | TOK_EXIST expr
      | TOK_ERR expr
      | TOK_END expr
      | TOK_NAME '=' expr
      ;
addop : '+'
      | '-'
      ;
inlist : inelt
      | inlist ',' inelt
      ;
opt_lhs :
      | lhs
      ;
inelt : opt_lhs opt_operation
      | '(' inlist ')' opt_operation
      | predefinedfunction opt_operation
      | simple_const opt_operation
      | '(' inlist ',' dospec ')'
      ;
opt_operation :
      | operation
      | opt_operation operation
      ;
outlist : uexpr    {if ( couldaddvariable == 1 ) strcpy($$,$1);}
      | other      {if ( couldaddvariable == 1 ) strcpy($$,$1);}
      | out2       {if ( couldaddvariable == 1 ) strcpy($$,$1);}
      ;
out2: uexpr ',' expr
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s,%s",$1,$3);}
      | uexpr ',' other
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s,%s",$1,$3);}
      | other ',' expr
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s,%s",$1,$3);}
      | other ',' other
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s,%s",$1,$3);}
      | out2 ',' expr
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s,%s",$1,$3);}
      | out2 ',' other
                   {if ( couldaddvariable == 1 ) sprintf($$,"%s,%s",$1,$3);}
      | uexpr     {if ( couldaddvariable == 1 ) strcpy($$,$1);}
      | predefinedfunction {if ( couldaddvariable == 1 ) strcpy($$,$1);}
      ;
other :  complex_const
                   {if ( couldaddvariable == 1 ) strcpy($$,$1);}
      | '(' expr ')'
                   {if ( couldaddvariable == 1 ) sprintf($$," (%s)",$2);}
      | '(' uexpr ',' dospec ')'
                   {if ( couldaddvariable == 1 ) sprintf($$,"(%s,%s)",$2,$4);}
      | '(' other ',' dospec ')'
                   {if ( couldaddvariable == 1 ) sprintf($$,"(%s,%s)",$2,$4);}
      | '(' out2 ',' dospec ')'
                   {if ( couldaddvariable == 1 ) sprintf($$,"(%s,%s)",$2,$4);}
      ;

dospec : TOK_NAME '=' expr ',' expr
                   {if ( couldaddvariable == 1 )
                                              sprintf($$,"%s=%s,%s)",$1,$3,$5);}
      | TOK_NAME '=' expr ',' expr ',' expr
                   {if ( couldaddvariable == 1 )
                                        sprintf($$,"%s=%s,%s,%s)",$1,$3,$5,$7);}
      ;
labellist : label
      | labellist ',' label
      ;
label : TOK_CSTINT
      ;
goto : TOK_PLAINGOTO '(' expr ',' expr ')' ',' expr
      | TOK_PLAINGOTO label
      ;
allocation_list : allocate_object
      | ident_dims
      | allocation_list ',' allocate_object
      ;
allocate_object : ident
                   {Add_Allocate_Var_1($1,curmodulename);}
      | structure_component
      | array_element
      ;
array_element : ident '(' funargs ')'
                   {Add_Allocate_Var_1($1,curmodulename);}
      ;
subscript_list : expr
      | subscript_list ',' expr
      ;

allocate_object_list :allocate_object
      | allocate_object_list ',' allocate_object
      ;
opt_stat_spec :
      | ',' TOK_STAT '=' ident
      ;
pointer_name_list : ident
      | pointer_name_list ',' ident
      ;
opt_construct_name :
      | TOK_NAME
      ;
opt_construct_name_colon :
      | TOK_CONSTRUCTID ':'
      ;
logif : TOK_LOGICALIF expr ')'
      ;
do_var : ident {strcpy($$,$1);}
      ;
%%

void processfortran(char *fichier_entree)
{
   extern FILE *fortranin;
   extern FILE *fortranout;
   char nomfile[LONG_C];
   int c;
   int confirmyes;

   /*fortrandebug = 1;*/
   if ( mark == 1 ) printf("Firstpass == %d \n",firstpass);
/******************************************************************************/
/*  1-  Open input and output files                                           */
/******************************************************************************/
   strcpy(nomfile,commondirin);
   strcat(nomfile,"/");
   strcat(nomfile,fichier_entree);
   fortranin=fopen( nomfile,"r");
   if (! fortranin)
   {
      printf("Error : File %s does not exist\n",nomfile);
      exit(1);
   }

   strcpy(curfile,nomfile);
   strcpy(nomfile,commondirout);
   strcat(nomfile,"/");
   strcat(nomfile,fichier_entree);
   strcpy(nomfileoutput,nomfile);
   Save_Length(nomfileoutput,31);
   if (firstpass == 1)
   {
      if (checkexistcommon == 1)
      {
         if (fopen(nomfile,"r"))
         {
            printf("Warning : file %s already exist\n",nomfile);
            confirmyes = 0;
            while (confirmyes==0)
            {
               printf("Override file %s ? [Y/N]\n",nomfile);
               c=getchar();
               getchar();
               if (c==79 || c==110)
               {
                  printf("We stop\n");
                  exit(1);
               }
               if (c==89 || c==121)
               {
                  confirmyes=1;
               }
            }
         }
      }
   }

/******************************************************************************/
/*  2-  Variables initialization                                              */
/******************************************************************************/

   line_num_fortran_common=1;
   line_num_fortran=1;
   PublicDeclare = 0;
   PrivateDeclare = 0;
   ExternalDeclare = 0;
   SaveDeclare = 0;
   pointerdeclare = 0;
   optionaldeclare = 0;
   incalldeclare = 0;
   VarType = 0;
   VarTypepar = 0;
   Allocatabledeclare = 0 ;
   strcpy(NamePrecision," ");
   VariableIsParameter =  0 ;
   strcpy(NamePrecision,"");
   c_star = 0 ;
   functiondeclarationisdone = 0;
   insubroutinedeclare = 0 ;
   strcpy(subroutinename," ");
   InitialValueGiven = 0 ;
   strcpy(EmptyChar," ");
   inmoduledeclare = 0;
   colnum=0;
   incom=0;
   couldaddvariable=1;
   aftercontainsdeclare = 1;
   strcpy(nameinttypename,"");
   /* Name of the file without format                                         */
   tmp = strchr(fichier_entree, '.');
   strncpy(curfilename,fichier_entree,strlen(fichier_entree)-strlen(tmp));
   Save_Length(curfilename,30);
/******************************************************************************/
/*  3-  Parsing of the input file (1 time)                                    */
/******************************************************************************/
   if (firstpass == 0 )
   {
      fortranout=fopen(nomfileoutput,"w");

      NewModule_Creation_0();
   }

   fortranparse();

   strcpy(curfile,mainfile);

   if (firstpass == 0 ) fclose(fortranout);
}
