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
extern int line_num_fortran;
extern int line_num_fortran_common;
char *tmp;
char c_selectorname[LONGNOM];
char ligne[LONGNOM];
char identcopy[LONGNOM];
int c_selectorgiven=0;
int incom;
listvar *curlistvar;
typedim c_selectordim;
listcouple *coupletmp;
int removeline=0;
%}

%union {
       char      na[LONGNOM];
       listdim  *d;
       listvar  *l;
       listvarcommon *lcom;
       listnom  *ln;
       listcouple  *lc;
       typedim   dim1;
       variable *v;
       }

%left ','
%nonassoc ':'
%right '='
%left TOK_BINARY_OP
%left EQV NEQV
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
%token TOK_INT
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
%token TOK_ABS
%token TOK_MOD
%token TOK_SIGN
%token TOK_MINLOC
/*%token TOK_REC*/
%token TOK_MAXLOC
%token TOK_EXIT
%token TOK_MINVAL
%token TOK_PUBLIC
%token TOK_PRIVATE
%token TOK_ALLOCATABLE
%token TOK_IN
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
%token TOK_OUT
%token TOK_INOUT
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
%token TOK_POINT
%token TOK_DATA 
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
%token <na> TOK_USE
%token <na> TOK_DSLASH
%token <na> TOK_DASTER
%token <na> TOK_EQ
%token <na> TOK_GT
%token <na> TOK_LT
%token <na> TOK_GE
%token <na> TOK_NE
%token <na> TOK_LE
%token <na> TOK_OR
%token <na> TOK_XOR
%token <na> TOK_NOT
%token <na> TOK_AND
%token <na> TOK_TRUE 
%token <na> TOK_FALSE 
%token <na> TOK_LABEL
%token <na> TOK_TYPE
%token <na> TOK_TYPEPAR
%token <na> TOK_ENDTYPE
%token <na> TOK_REAL
%token <na> TOK_INTEGER
%token <na> TOK_LOGICAL
%token <na> TOK_DOUBLEPRECISION
%token <na> TOK_DOUBLEREAL
%token <na> TOK_ENDSUBROUTINE
%token <na> TOK_ENDFUNCTION
%token <na> TOK_ENDPROGRAM
%token <na> TOK_ENDUNIT
%token <na> TOK_CHARACTER
%token <na> TOK_CHAR_CONSTANT
%token <na> TOK_CHAR_CUT
%token <na> TOK_CHAR_INT
%token <na> TOK_CHAR_MESSAGE 
%token <na> TOK_CSTREAL
%token <na> TOK_CSTREALDP
%token <na> TOK_CSTREALQP
%token <na> TOK_SFREAL 
%token <na> TOK_COMPLEX
%token <na> TOK_DOUBLECOMPLEX
%token <na> TOK_NAME
%token <na> TOK_NAME_CHAR
%token <na> TOK_PROBTYPE  /* dimension of the problem                         */
%token <na> TOK_INTERPTYPE/* kind of interpolation                            */
%token <na> TOK_VARTYPE   /* posit ion of the grid variable on the cells of   */
                          /*     the mesh                                     */
%token <na> TOK_SLASH
%token <na> TOK_BC        /* calculation of the boundary conditions           */
%token <na> TOK_OP
%token <na> TOK_CSTINT
%token <na> TOK_COMMENT 
%token <na> TOK_FILENAME
%token ','
%token ';'
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
%type <l> dimension
%type <l> paramlist
%type <l> args  
%type <lc> only_list
%type <lc> only_name
%type <lc> rename_list
%type <lc> rename_name
%type <lcom> common
%type <lcom> var_common
%type <lcom> var_common_list
%type <d> dims
%type <d> dimlist
%type <dim1> dim
%type <v> paramitem
%type <na> comblock
%type <na> name_routine
%type <na> begin_array
%type <na> module_name
%type <na> opt_name
%type <na> clause
%type <na> type
%type <na> arg
%type <na> typename
%type <na> typespec
%type <na> uexpr
%type <na> minmaxlist
%type <na> complex_const
%type <na> lhs
%type <na> simple_const
%type <na> vec
%type <na> outlist 
%type <na> out2 
%type <na> other 
%type <na> dospec 
%type <na> expr_data 
%type <na> beforefunctionuse 
%type <na> ident 
%type <na> structure_component 
%type <na> array_ele_substring_func_ref  
%type <na> funarglist  
%type <na> funarg  
%type <na> funargs  
%type <na> triplet  
%type <na> substring  
%type <na> string_constant  
%type <na> opt_substring  
%type <na> opt_expr  
%type <na> optexpr  
%type <na> datavallist  
%type <na> after_slash  
%type <na> after_equal
%type <na> predefinedfunction
%type <na> do_var  
%type <na> expr
%type <na> word_endsubroutine
%type <na> word_endfunction
%type <na> word_endprogram
%type <na> word_endunit
%type <na> intent_spec
%type <na> ubound
%type <na> signe
%type <na> opt_signe
%type <na> operation
%type <na> filename
%type <na> proper_lengspec

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
      |   suite_line ';' suite_line_list
      ;
suite_line : entry fin_line/* subroutine, function, module                    */
      | spec fin_line      /* declaration                                     */
      | before_include filename fin_line
                  {
                     if (inmoduledeclare == 0 && 
                         couldaddvariable == 1 )
                     {
                        pos_end = setposcur();
                        RemoveWordSET_0(fortranout,pos_curinclude,
                                              pos_end-pos_curinclude);
                     }
                  }
      | exec cmnt writedeclar /* if, do etc ...                               */
      | instr fin_line    /* instruction ident : do i = 1 ...                 */
      ;
instr : ident ':'
      ;
fin_line : position cmnt
      ;
keyword : TOK_DONOTTREAT 
         {
            /* we should ignore the declaration until the keyword   */
            /*    TOK_ENDDONOTTREAT                                 */
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
position: {pos_cur = setposcur();}
      ;
thislabel: 
      | TOK_LABEL nulcurbuf
      ;
cmnt:
      | TOK_COMMENT
      ;
incomment:
                   {incom = 1;}
      ;
nulcurbuf:
                   {if (incom !=1) {strcpy(curbuf,"");incom=0;}}
      ;
entry: 
      | TOK_SUBROUTINE name_routine arglist
                   {
                      Listofvariableinagriffunction=(listnom *)NULL;
                      strcpy(subroutinename,$2);
                      if ( inmodulemeet == 1 )
                      {
                         tmpdeclaration_everdone = 0;
                         paramdeclaration_everdone = 0;
                         insubroutinedeclare = 1;
                         AddUseAgrifUtil_0();
                         /* in the second step we should write the head of    */
                         /*    the subroutine sub_loop_<subroutinename>       */
                         writeheadnewsub_0(1);
                         adduseagrifutil = 0 ; 
                      }
                      else
                      {
                            tmpdeclaration_everdone = 0;
                            paramdeclaration_everdone = 0;
                            insubroutinedeclare = 1;
                            AddUseAgrifUtil_0();
                            writeheadnewsub_0(1);
                            adduseagrifutil = 0 ; 
                      }
                   }
      | TOK_PROGRAM name_routine
                   {
                      Listofvariableinagriffunction=(listnom *)NULL;
                      strcpy(subroutinename,$2);
                      /* Common case                                          */
                         tmpdeclaration_everdone = 0;
                         paramdeclaration_everdone = 0;
                         insubroutinedeclare = 1;
                         AddUseAgrifUtil_0();
                         /* in the second step we should write the head of    */
                         /*    the subroutine sub_loop_<subroutinename>       */
                         writeheadnewsub_0(1);
                         adduseagrifutil = 0 ;                       
                   }
      | TOK_FUNCTION name_routine arglist TOK_RESULT arglist1
                   {
                      Listofvariableinagriffunction=(listnom *)NULL;
                      strcpy(subroutinename,$2);
                      if ( inmodulemeet == 1 )
                      {
                         tmpdeclaration_everdone = 0;
                         paramdeclaration_everdone = 0;
                         insubroutinedeclare = 1;
                         AddUseAgrifUtil_0();
                         /* we should to list of the subroutine argument the  */
                         /*    name of the function which has to be defined   */
                         if ( firstpass == 1 ) 
                         {
                            curvar=createvar($2,NULL);
                            curlistvar=insertvar(NULL,curvar);
                            listargsubroutine = AddListvarToListvar(curlistvar,listargsubroutine,1);
                         }
                         /* in the second step we should write the head of    */
                         /*    the subroutine sub_loop_<subroutinename>       */
                         writeheadnewsub_0(2);
                         adduseagrifutil = 0 ; 
                      }
                      else
                      {
                            tmpdeclaration_everdone = 0;
                            paramdeclaration_everdone = 0;
                            insubroutinedeclare = 1;
                            AddUseAgrifUtil_0();
                            /* we should to list of the subroutine argument   */
                            /* name of the function which has to be defined   */
                            if ( firstpass == 1 ) 
                            {
                               curvar=createvar($2,NULL);
                               curlistvar=insertvar(NULL,curvar);
                               listargsubroutine = AddListvarToListvar
                                               (curlistvar,listargsubroutine,1);
                            }
                            writeheadnewsub_0(2);
                            adduseagrifutil = 0 ; 
                      }
                   }
      | TOK_FUNCTION name_routine arglist
                   {
                      Listofvariableinagriffunction=(listnom *)NULL;
                      strcpy(subroutinename,$2);
                      if ( inmodulemeet == 1 )
                      {
                         tmpdeclaration_everdone = 0;
                         paramdeclaration_everdone = 0;
                         insubroutinedeclare = 1;
                         AddUseAgrifUtil_0();
                         /* we should to list of the subroutine argument the  */
                         /*    name of the function which has to be defined   */
                         if ( firstpass == 1 ) 
                         {
                            curvar=createvar($2,NULL);
                            curlistvar=insertvar(NULL,curvar);
                            listargsubroutine = AddListvarToListvar
                                               (curlistvar,listargsubroutine,1);
                         }
                         /* in the second step we should write the head of    */
                         /*    the subroutine sub_loop_<subroutinename>       */
                         writeheadnewsub_0(2);
                         adduseagrifutil = 0 ; 
                      }
                      else
                      {
                            tmpdeclaration_everdone = 0;
                            paramdeclaration_everdone = 0;
                            insubroutinedeclare = 1;
                            AddUseAgrifUtil_0();
                            /* we should to list of the subroutine argument   */
                            /* name of the function which has to be defined   */
                            if ( firstpass == 1 ) 
                            {
                               curvar=createvar($2,NULL);
                               curlistvar=insertvar(NULL,curvar);
                               listargsubroutine = AddListvarToListvar
                                               (curlistvar,listargsubroutine,1);
                            }
                            writeheadnewsub_0(2);
                            adduseagrifutil = 0 ; 
                      }
                   }
      | TOK_MODULE TOK_NAME
                   {
                      strcpy(curmodulename,$2);
                      Add_ModuleTo_Modulelist_1($2);
                      if ( inmoduledeclare == 0 )
                      { 
                         /* Alloc should be create ?                          */
                         FillInlistmodule_1();   
                         /* To know if there are in the module declaration    */
                         inmoduledeclare = 1;
                         /* to know if a module has been met                  */
                         inmodulemeet = 1;
                         /* to know if we are after the keyword contains      */
                         aftercontainsdeclare = 0 ;
                      }      
                      /* WE should use Agrif_Util if it is necessary          */
                      AddUseAgrifInModuleDeclaration_0();
                   }
      ;
name_routine : TOK_NAME {strcpy($$,$1);strcpy(subroutinename,$1);}
writedeclar :
      ;
before_include : TOK_INCLUDE
                   {
                      pos_curinclude = setposcur()-9;
                   }      
filename: TOK_CHAR_CONSTANT
                   {
                      if ( couldaddvariable == 1 )
                      {
                         Addincludetothelist_1($1);
                      }                   
                   }
      ;
arglist: 
      | '(' ')'    {
                      if ( firstpass == 1 ) listargsubroutine=NULL;
                   }
      | '(' args ')'
                   {
                       if ( firstpass == 1 ) listargsubroutine=$2;
                   }
      ;
arglist1: 
      | '(' ')'
      | '(' args ')'
                   {
                       listargsubroutine = AddListvarToListvar
                                                       ($2,listargsubroutine,1);
                   }
      ;
args:arg           {
                      if ( firstpass == 1 )
                      {
                         curvar=createvar($1,curdim);
                         curlistvar=insertvar(NULL,curvar);
                         $$=settype($1,curlistvar);
                      }
                   }
      | args ',' arg 
                   {
                      if ( firstpass == 1 )
                      {
                         curvar=createvar($3,curdim);
                         $$=insertvar($1,curvar);
                      }
                   }
      ;
arg: TOK_NAME      {strcpy($$,$1);}
      | '*'        {strcpy($$,"*");}
      ; 
spec: type after_type
                   {
                      /* remove declaration                                   */
                      if ( fortran77 == 1                       && 
                           infunctiondeclare == 0               && 
                           IsTabvarsUseInArgument_0() == 1      &&
                           couldaddvariable == 1 ) 
/*                           commonlist                           && */
                      {
                         pos_end = setposcur();
                         RemoveWordSET_0(fortranout,pos_cur_decl,
                                               pos_end-pos_cur_decl);
                      }
                      infunctiondeclare = 0 ;
                   }
      | TOK_TYPE opt_sep opt_name
/*      {
         couldaddvariable = 0;
      }*/
      | TOK_ENDTYPE opt_name
/*      {
         couldaddvariable = 1;
      }*/
      | TOK_POINTER list_couple
      | before_parameter  '(' paramlist ')' 
                   {
                      AddvartoParamlist_1($3);
                      if ( fortran77 == 1  && 
                           commonlist      && 
                           listvarindoloop && 
                           IsTabvarsUseInArgument_0() == 1 )
                      {
                         pos_end = setposcur();
                         RemoveWordSET_0(fortranout,pos_curparameter,
                                               pos_end-pos_curparameter);
                      }
                   }
      | before_parameter  paramlist 
                   {
                      AddvartoParamlist_1($2);
                      if ( fortran77 == 1  && 
                           commonlist      && 
                           listvarindoloop && 
                           IsTabvarsUseInArgument_0() == 1 )
                      {
                         pos_end = setposcur();
                         RemoveWordSET_0(fortranout,pos_curparameter,
                                               pos_end-pos_curparameter);
                      }
                   }
      | common
      | save
      | implicit
      | dimension
                  {
                   /* if the variable is a parameter we can suppose that is   */
                   /*    value is the same on each grid. It is not useless to */
                   /*    create a copy of it on each grid                     */
                      if ( couldaddvariable == 1 )
                      {
                         ajoutevar_1($1);
                         NonGridDepDeclaration_0($1);
                         /* if variables has been declared in a subroutine    */
                         if ( insubroutinedeclare == 1 )
                         {
                           ajoutvarofsubroutine_1($1);
                           writesubroutinedeclaration_0($1);
                         }
                      }
                      /* Case of common block                                 */
                      indeclarationvar=0;
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
                   }
      | public
      | private
      | use_stat
      | module_proc_stmt
      | interface 
      | namelist
      | TOK_BACKSPACE '(' expr ')'
      | TOK_EXTERNAL opt_sep use_name_list
      | TOK_INTRINSIC opt_sep use_intrinsic_list
      | TOK_EQUIVALENCE '(' list_expr ')'
      | before_data data '\n'
                   {
                      /* we should remove the data declaration                */
                     if ( aftercontainsdeclare == 0 )
                     {
                        pos_end = setposcur();
                        RemoveWordSET_0(fortranout,pos_curdata,
                                              pos_end-pos_curdata);
                     }
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
list_couple : '(' list_expr ')' 
      | list_couple ',' '(' list_expr ')' 
list_expr : expr
      | list_expr ',' expr
opt_sep :
      | ':' ':'
      ;
after_type : dcl nodimsgiven 
                   {
                   /* if the variable is a parameter we can suppose that is   */
                   /*    value is the same on each grid. It is not useless to */
                   /*    create a copy of it on each grid                     */
                      if ( couldaddvariable == 1 )
                      {
                         ajoutevar_1($1);
                         if ( VariableIsParameter == 1 ) globparam =
                                            AddListvarToListvar($1,globparam,1);
                         NonGridDepDeclaration_0($1);
                         /* if variables has been declared in a subroutine    */
                         if ( insubroutinedeclare == 1 )
                         {
                           ajoutvarofsubroutine_1($1);
                           writesubroutinedeclaration_0($1);
                         }
                         /* If there are a SAVE declarations in module's      */
                         /*    subroutines we should remove it from the       */
                         /*    subroutines declaration and add it in the      */
                         /*    global declarations                            */
                         if ( aftercontainsdeclare == 1 ) 
                         {
                            ajoutevarsave_1($1);
                            if ( VariableIsParameter == 0 && SaveDeclare == 1) 
                            {
                               pos_end = setposcur();
                               RemoveWordSET_0(fortranout,pos_cur,
                                               pos_end-pos_cur);
                            }
                         }
                      }
                      /* Case of common block                                 */
                      indeclarationvar=0;
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
                   }
      | TOK_FUNCTION TOK_NAME arglist
                   {
                      infunctiondeclare = 1 ;
                      Listofvariableinagriffunction=(listnom *)NULL;
                      strcpy(subroutinename,$2);
                      if ( inmodulemeet == 1 )
                      {
                         tmpdeclaration_everdone = 0;
                         paramdeclaration_everdone = 0;
                         insubroutinedeclare = 1;
                         AddUseAgrifUtil_0();
                         /* we should to list of the subroutine argument the  */
                         /*    name of the function which has to be defined   */
                         if ( firstpass == 1 ) 
                         {
                            curvar=createvar($2,NULL);
                            strcpy(curvar->typevar,DeclType);
                            curlistvar=insertvar(NULL,curvar);
                            listargsubroutine = AddListvarToListvar
                                               (curlistvar,listargsubroutine,1);
                            curvar=createvar($2,NULL);
                            strcpy(curvar->typevar,DeclType);
                            strcpy(curvar->modulename,subroutinename);
                            curlistvar=insertvar(NULL,curvar);        
                            varofsubroutineliste = AddListvarToListvar
                                            (curlistvar,varofsubroutineliste,1);
                         }
                         if ( firstpass == 0 )
                         {
                            curvar=createvar($2,NULL);
                            strcpy(curvar->typevar,DeclType);
                            functionlistvar=insertvar(NULL,curvar);
                         }
                         /* in the second step we should write the head of    */
                         /*    the subroutine sub_loop_<subroutinename>       */
                         writeheadnewsub_0(2);
                         adduseagrifutil = 0 ; 
                      }
                      else
                      {
                         tmpdeclaration_everdone = 0;
                         paramdeclaration_everdone = 0;
                         insubroutinedeclare = 1;
                         AddUseAgrifUtil_0();
                         /* we should to list of the subroutine argument the  */
                         /*    name of the function which has to be defined   */
                         if ( firstpass == 1 ) 
                         {
                            curvar=createvar($2,NULL);
                            strcpy(curvar->typevar,DeclType);
                            curlistvar=insertvar(NULL,curvar);
                            listargsubroutine = AddListvarToListvar
                                               (curlistvar,listargsubroutine,1);
                            curvar=createvar($2,NULL);
                            strcpy(curvar->typevar,DeclType);
                            strcpy(curvar->modulename,subroutinename);
                            curlistvar=insertvar(NULL,curvar);        
                            varofsubroutineliste = AddListvarToListvar
                                            (curlistvar,varofsubroutineliste,1);
                         }
                         /* in the second step we should write the head of    */
                         /*    the subroutine sub_loop_<subroutinename>       */
                         writeheadnewsub_0(2);
                         adduseagrifutil = 0 ; 
                      }
                   }
      ;
before_parameter : TOK_PARAMETER
                   {
                      pos_curparameter = setposcur()-9;
                   }      
before_data : TOK_DATA
                   {
                      pos_curdata = setposcur()-4;
                   }
data: TOK_NAME TOK_SLASH datavallist TOK_SLASH
                   {
                      sprintf(ligne,"(/ %s /)",$3);
                      CompleteDataList($1,ligne);
                   }
      | data opt_comma TOK_NAME TOK_SLASH datavallist TOK_SLASH
                   {
                      sprintf(ligne,"(/ %s /)",$5);
                      CompleteDataList($3,ligne);
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
datanamelist : TOK_NAME
      | datanamelist ',' TOK_NAME
      ;
datavallist : expr_data
                   {
                      strcpy($$,$1);
                   }
      | expr_data ',' datavallist 
                   {
                      sprintf($$,"%s,%s",$1,$3);
                   }
      ;
expr_data : opt_signe simple_const
                   {sprintf($$,"%s%s",$1,$2);}
      | expr_data '+' expr_data
                   {sprintf($$,"%s+%s",$1,$3);}
      | expr_data '-' expr_data
                   {sprintf($$,"%s-%s",$1,$3);}
      | expr_data '*' expr_data
                   {sprintf($$,"%s*%s",$1,$3);}
      | expr_data '/' expr_data
                   {sprintf($$,"%s/%s",$1,$3);}
      ;
opt_signe : 
                   {strcpy($$,"");}
      | signe
                   {strcpy($$,$1);}
      ;
namelist:  namelist_action after_namelist
      ;
namelist_action : TOK_NAMELIST  ident 
      | TOK_NAMELIST  comblock ident
      {
         AddNameToListNamelist_1($2);
      }
      | namelist_action opt_comma comblock opt_comma ident
      {
         AddNameToListNamelist_1($3);
      }
      | namelist_action ',' ident
      ;
after_namelist :
      ;
interface: TOK_INTERFACE opt_name
      | TOK_ENDINTERFACE opt_name
      ;
dimension: TOK_DIMENSION  opt_comma TOK_NAME dims lengspec
      {
         if ( couldaddvariable == 1 )
         {
            if ( inmoduledeclare == 1 || SaveDeclare == 1 )
            {
               if ( AllocShouldMadeInModule() == 1 ) 
               {
                 AllocTo1InModule_1();
               }
            }      
            /*                                                                */
            curvar=createvar($3,$4);
            /*                                                                */
            if ( IsVariableReal($3) == 1 )
            {
               /*                                                             */
               CreateAndFillin_Curvar("REAL",$3,$4,curvar);
               /*                                                             */
               curlistvar=insertvar(NULL,curvar);
               /*                                                             */
               $$=settype("REAL",curlistvar);
            }
            else
            {
               /*                                                             */
               CreateAndFillin_Curvar("INTEGER",$3,$4,curvar);
               /*                                                             */
               curlistvar=insertvar(NULL,curvar);
               /*                                                             */
               $$=settype("INTEGER",curlistvar);
            }
            strcpy(vallengspec,"");
         }
         else
         {
            /* mazauric*/
         }
      }
      | dimension ',' TOK_NAME dims lengspec
      {
         if ( couldaddvariable == 1 )
         {
            /*                                                                */
            curvar=createvar($3,$4);
            /*                                                                */
            if ( IsVariableReal($3) == 1 )
            {
               /*                                                             */
               CreateAndFillin_Curvar("REAL",$3,$4,curvar);
               /*                                                             */
               curlistvar=insertvar($1,curvar);
               /*                                                             */
               $$=curlistvar;
            }
            else
            {
               /*                                                             */
               CreateAndFillin_Curvar("INTEGER",$3,$4,curvar);
               /*                                                             */
               curlistvar=insertvar($1,curvar);
               /*                                                             */
               $$=curlistvar;
            }
            strcpy(vallengspec,"");
         }         
         else
         {
            /* mazauric*/
         }
      }
      ;
private: TOK_PRIVATE '\n'
      | TOK_PRIVATE opt_sep use_name_list
      ;
public: TOK_PUBLIC '\n'
      | TOK_PUBLIC opt_sep use_name_list 
      ;
use_name_list: TOK_NAME 
      | use_name_list ',' TOK_NAME
      ;
common: before_common var_common_list
                   {
                         if (fortran77 == 1 &&
                             couldaddvariable == 1 )
                         {
                            pos_end = setposcur();
                            RemoveWordSET_0(fortranout,pos_curcommon,
                                                  pos_end-pos_curcommon);
                         }
                   }
      | before_common comblock var_common_list
                   {
                         if ( couldaddvariable == 1 )
                         {
                            sprintf(charusemodule,"%s",$2);
                            Add_ModuleTo_Modulelist_1($2);
                            if ( fortran77 == 1 )
                            {
                               pos_end = setposcur();
                               RemoveWordSET_0(fortranout,pos_curcommon,
                                                     pos_end-pos_curcommon);
                            }
                         }
                   }
      | common opt_comma comblock opt_comma var_common_list
                   {
                         if ( couldaddvariable == 1 )
                         {
                            sprintf(charusemodule,"%s",$3);
                            Add_ModuleTo_Modulelist_1($3);
                            if ( fortran77 == 1 )
                            {
                               pos_end = setposcur();
                               RemoveWordSET_0(fortranout,pos_curcommon,
                                                     pos_end-pos_curcommon);
                            }
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
                      if ( couldaddvariable == 1 ) Addtolistvarcommon();
                   }

     | var_common_list ',' var_common
                   {
                      if ( couldaddvariable == 1 ) Addtolistvarcommon();
                   }
var_common: TOK_NAME dims  
                   {
                      if ( couldaddvariable == 1 ) 
                      {
                         positioninblock = positioninblock + 1 ;
                         strcpy(commonvar,$1);
                         commondim = $2;
                      }
                   }
      ;
comblock: TOK_DSLASH 
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
save: TOK_SAVE varsave
      | TOK_SAVE  comblock varsave
      | save opt_comma comblock opt_comma varsave
      | save ',' varsave
      ;
varsave: 
      | TOK_NAME before_dims dims 
      {created_dimensionlist = 1;}
      ;
      
opt_comma:
      | ','
      ;
paramlist: paramitem
                   {
                      if ( firstpass == 1 ) $$=insertvar(NULL,$1);
                   }
      | paramlist ',' paramitem
                   {
                      if ( firstpass == 1 ) $$=insertvar($1,$3);
                   }
      ;
paramitem : TOK_NAME '=' expr
                   {
                      if ( firstpass == 1 )
                      {
                         curvar=(variable *) malloc(sizeof(variable));
                         curvar->dimension=(listdim *)NULL;
                         curvar->nbdim=0;
                         curvar->common=0;
                         curvar->positioninblock=0;
                         curvar->module=0; 
                         curvar->save=0;
                         curvar->VariableIsParameter=0;
                         curvar->PublicDeclare=0;
                         curvar->PrivateDeclare=0;
                         curvar->ExternalDeclare=0;
                         curvar->pointedvar=0;
                         curvar->dimensiongiven=0;
                         curvar->c_star=0;
                         curvar->typegiven=0;
                         curvar->indicetabvars=0; 
                         curvar->pointerdeclare=0; 
                         curvar->optionaldeclare=0;
                         curvar->allocatable=0; 
                         curvar->dimsempty=0;
                         strcpy(curvar->nomvar,$1);
                         strcpy(curvar->subroutinename,subroutinename);
                         strcpy(curvar->modulename,subroutinename);
                         strcpy(curvar->vallengspec,"");
                         strcpy(curvar->precision,"");
                         strcpy(curvar->nameinttypename,"");
                         strcpy(curvar->IntentSpec,"");
                         strcpy(curvar->dimchar,"");
                         strcpy(curvar->initialvalue,$3);
                         $$=curvar;
                      }
                   }
      ;
module_proc_stmt: TOK_PROCEDURE proc_name_list
      ;
proc_name_list: TOK_NAME
      | proc_name_list ',' TOK_NAME
      ;
implicit: TOK_IMPLICIT TOK_NONE
                    {
                       if ( firstpass == 1 && insubroutinedeclare == 1 )
                       {
                           listimplicitnone = Addtolistname
                                              (subroutinename,listimplicitnone);
                       }
                       if ( tmpdeclaration_everdone == 1 && 
                            inmoduledeclare == 0 )
                       {
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
                         if ( inmoduledeclare == 1 || SaveDeclare == 1 )
                         {
                            if ( AllocShouldMadeInModule() == 1 ) 
                            {
                               AllocTo1InModule_1();
                            }
                         }      
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
                         CreateAndFillin_Curvar(DeclType,$3,$4,curvar);
                         /*                                                   */
                         curlistvar=insertvar(NULL,curvar);
                         if (!strcasecmp(DeclType,"character")) 
                         {
                            if (c_selectorgiven == 1) 
                            {
                               strcpy(c_selectordim.first,"1");
                               strcpy(c_selectordim.last,c_selectorname);
                               change_dim_char
                                     (insertdim(NULL,c_selectordim),curlistvar);
                            }
                         }
                         $$=settype(DeclType,curlistvar);
                      }
                      else
                      {
                         /* mazauric*/
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
                         CreateAndFillin_Curvar($1->var->typevar,$4,$5,curvar);
                         /*                                                   */
                         strcpy(curvar->typevar,($1->var->typevar));
                         /*                                                   */
                         curlistvar=insertvar($1,curvar);
                         if (!strcasecmp(DeclType,"character")) 
                         {
                            if (c_selectorgiven == 1) 
                            {
                               strcpy(c_selectordim.first,"1");
                               strcpy(c_selectordim.last,c_selectorname);
                               change_dim_char
                                     (insertdim(NULL,c_selectordim),curlistvar);
                            }
                         }
                         $$=curlistvar;
                      }
                      else
                      {
                         /* mazauric*/
                      }
                      strcpy(vallengspec,"");
                   }
      ;      
nodimsgiven:       {dimsgiven=0;}
      ;
type:typespec selector 
                   {strcpy(DeclType,$1);indeclarationvar=1;}
      | before_character c_selector 
                   {
                      indeclarationvar=1;
                      strcpy(DeclType,"CHARACTER");
                      if (inmoduledeclare == 1 ) 
                      {
                         AllocShouldMadeTo1InModule_1();
                      }
                   }
      | typename '*' TOK_CSTINT
                   {
                      indeclarationvar=1;
                      strcpy(DeclType,$1);
                      strcpy(nameinttypename,$3);
                   }
      | TOK_TYPEPAR attribute ')' 
                   {
                      pos_cur_decl = setposcur()-5;
                      strcpy(DeclType,"TYPE");
		      indeclarationvar=1;
                   }
      ;
c_selector:
      | '*' TOK_CSTINT 
                   {c_selectorgiven=1;strcpy(c_selectorname,$2);}
      | '*' '(' c_attribute ')' {c_star = 1;}
      | '(' c_attribute ')' 
      ;
c_attribute: TOK_NAME clause opt_clause 
      | TOK_NAME '=' clause opt_clause 
      | clause opt_clause 
      ;
before_character : TOK_CHARACTER
                   {
                      pos_cur_decl = setposcur()-9;
                   }
      ;
typespec: typename {strcpy($$,$1);}
      ;
typename: TOK_INTEGER 
                   {
                      strcpy($$,"INTEGER");
                      pos_cur_decl = setposcur()-7;
                      if (inmoduledeclare == 1 ) 
                      {
                         AllocShouldMadeTo1InModule_1();
                       }
                   }
      | TOK_REAL   {
                      strcpy($$,"REAL"); 
                      pos_cur_decl = setposcur()-4;
                      if (inmoduledeclare == 1 ) 
                      {
                         AllocShouldMadeTo1InModule_1();
                      }
                   }
      | TOK_COMPLEX      
                   {strcpy($$,"COMPLEX");}
      | TOK_DOUBLEPRECISION 
                   {
		      strcpy($$,"REAL");
                      strcpy(nameinttypename,"8");
                   }
      | TOK_DOUBLECOMPLEX 
                   {strcpy($$,"DOUBLE COMPLEX");}
      | TOK_LOGICAL      
                   {
                      strcpy($$,"LOGICAL");
                      pos_cur_decl = setposcur()-7;
                      if (inmoduledeclare == 1 ) 
                      {
                         AllocShouldMadeTo1InModule_1();
                      }
                   }
/*      | TOK_TYPE      
                   {
                      pos_cur_decl = setposcur()-5;
                      strcpy($$,"TYPE");
                   }*/
      ;
lengspec:
      | '*' proper_lengspec {strcpy(vallengspec,$2);}
      ;
proper_lengspec: expr {sprintf($$,"*%s",$1);}
      | '(' '*' ')'{strcpy($$,"*(*)");}
      ;
selector:
      | '*' proper_selector
      | '(' attribute ')' 
      ;
proper_selector: expr
      | '(' '*' ')'
      ;
attribute: TOK_NAME clause 
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
clause: expr       {strcpy(CharacterSize,$1);
                    strcpy($$,$1);}
      | '*'        {strcpy(CharacterSize,"*");
                    strcpy($$,"*");} 
      ;
opt_clause: 
      | ',' TOK_NAME clause
      ;
options:
      | ':' ':'
      | ',' attr_spec_list ':' ':'
      ;
attr_spec_list: attr_spec
      | attr_spec_list ',' attr_spec
      ;
attr_spec: TOK_PARAMETER 
                   {
                      VariableIsParameter = 1;
                      if (inmoduledeclare == 1 ) 
                      {
                         AllocShouldMadeTo0InModule_1();
                      }
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
      | TOK_INTENT intent_spec
                   {strcpy(IntentSpec,$2);}
      | TOK_INTRINSIC
      | TOK_OPTIONAL{optionaldeclare = 1 ;}
      | TOK_POINTER {pointerdeclare = 1 ;}
      | TOK_SAVE    {
                       if ( inmodulemeet == 1 )
                       {
                          SaveDeclare = 1 ;
                          Savemeet = 1;
                          AllocShouldMadeTo1InModule_1();
                       }
                    }
      | TOK_TARGET
      ;
intent_spec: TOK_IN {sprintf($$,"in");}
      | TOK_OUT     {sprintf($$,"out");}
      | TOK_INOUT   {sprintf($$,"inout");}
      ; 
access_spec: TOK_PUBLIC 
                   {PublicDeclare = 1;} 
      | TOK_PRIVATE 
                   {PrivateDeclare = 1;} 
      ;
dims:              {if ( created_dimensionlist == 1 ) $$=(listdim *)NULL;}
      | '(' dimlist ')'  
                   {if ( created_dimensionlist == 1 || 
		         agrif_parentcall      == 1 ) $$=$2;}
      ;
dimlist:   dim     {if ( created_dimensionlist == 1 ||
                         agrif_parentcall      == 1 ) $$=insertdim(NULL,$1);}
      | dimlist ',' dim 
                   {if ( created_dimensionlist == 1 ) $$=insertdim($1,$3);}
      ;
dim:ubound         {strcpy($$.first,"1");strcpy($$.last,$1);}
      | ':'        {strcpy($$.first,"");strcpy($$.last,"");}
      | expr ':'   {strcpy($$.first,$1);strcpy($$.last,"");}
      | ':' expr   {strcpy($$.first,"");strcpy($$.last,$2);}
      | expr ':' ubound
                   {strcpy($$.first,$1);strcpy($$.last,$3);}
      ;
ubound:  '*'       {strcpy($$,"*");}
      | expr       {strcpy($$,$1);}
      ;
expr:  uexpr       {strcpy($$,$1);}
      | '(' expr ')' 
                   {sprintf($$,"(%s)",$2);}
      | complex_const 
                   {strcpy($$,$1);}
      | predefinedfunction
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
      | TOK_INT '(' expr ')'
                   {sprintf($$,"INT(%s)",$3);}                   
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
                   {strcpy($$,$1);strcat($$,",");strcat($$,$3);}
      ;
uexpr:  lhs        {strcpy($$,$1);} 
      | simple_const 
                   {strcpy($$,$1);} 
      | vec 
                   {strcpy($$,$1);} 
      | expr operation
                   {sprintf($$,"%s%s",$1,$2);}
      | signe expr %prec '* ' 
                   {sprintf($$,"%s%s",$1,$2);}
      | TOK_NOT expr 
                   {sprintf($$,"%s%s",$1,$2);}
      ;
signe : '+'        {strcpy($$,"+");}
      | '-'        {strcpy($$,"-");}
      ;
operation : '+' expr %prec '+' 
                   {sprintf($$,"+%s",$2);}
      |  '-' expr %prec '+' 
                   {sprintf($$,"-%s",$2);}
      |  '*' expr 
                   {sprintf($$,"*%s",$2);}
      |  TOK_DASTER expr 
                   {sprintf($$,"%s%s",$1,$2);}
      |  TOK_EQ expr %prec TOK_EQ 
                   {sprintf($$,"%s%s",$1,$2);}
      |  TOK_GT expr %prec TOK_EQ 
                   {sprintf($$,"%s%s",$1,$2);}
      |  '>' expr %prec TOK_EQ 
                   {sprintf($$," > %s",$2);}
      |  TOK_LT expr %prec TOK_EQ 
                   {sprintf($$,"%s%s",$1,$2);}
      |  '<' expr %prec TOK_EQ 
                   {sprintf($$," < %s",$2);}
      |  TOK_GE expr %prec TOK_EQ 
                   {sprintf($$,"%s%s",$1,$2);}
      |  '>''=' expr %prec TOK_EQ 
                   {sprintf($$," >= %s",$3);}
      |  TOK_LE expr %prec TOK_EQ
                   {sprintf($$,"%s%s",$1,$2);}
      |  '<''=' expr %prec TOK_EQ 
                   {sprintf($$," <= %s",$3);}
      |  TOK_NE expr %prec TOK_EQ 
                   {sprintf($$,"%s%s",$1,$2);}
      |  TOK_XOR expr 
                   {sprintf($$,"%s%s",$1,$2);}
      |  TOK_OR expr 
                   {sprintf($$,"%s%s",$1,$2);}
      |  TOK_AND expr 
                   {sprintf($$,"%s%s",$1,$2);}
      |  TOK_SLASH after_slash
                   {sprintf($$,"%s",$2);}
      |  '=' after_equal
                   {sprintf($$,"%s",$2);}

after_slash : {strcpy($$,"");}
      | expr 
                   {sprintf($$,"/%s",$1);}
      | '=' expr %prec TOK_EQ 
                   {sprintf($$,"/= %s",$2);}
      | TOK_SLASH expr
                   {sprintf($$,"//%s",$2);}
      ;
after_equal : '=' expr %prec TOK_EQ 
                   {sprintf($$,"==%s",$2);}
      | expr
                   {sprintf($$,"= %s",$1);}
      ;
      
lhs: ident         {strcpy($$,$1);} 
      | structure_component 
                   {strcpy($$,$1);} 
      | array_ele_substring_func_ref 
                   {strcpy($$,$1);} 
      ;
beforefunctionuse : {
                      agrif_parentcall =0;
                      if (!strcasecmp(identcopy,"Agrif_Parent") )
                                                            agrif_parentcall =1;
                      if ( Agrif_in_Tok_NAME(identcopy) == 1 )
                      { 
                         inagrifcallargument = 1;
                         AddsubroutineTolistsubwhereagrifused();
                      }
                   }
      ;
array_ele_substring_func_ref: begin_array
                   {
                     strcpy($$,$1);
                     if ( incalldeclare == 0 ) inagrifcallargument = 0;
                   }
      | begin_array substring 
                   {sprintf($$," %s %s ",$1,$2);}
      | structure_component '(' funarglist ')' 
                   {sprintf($$," %s ( %s )",$1,$3);}
      | structure_component '(' funarglist ')' substring 
                   {sprintf($$," %s ( %s ) %s ",$1,$3,$5);}
      ;
begin_array : ident '(' funarglist ')'
                   {
                      sprintf($$," %s ( %s )",$1,$3);
                      ModifyTheAgrifFunction_0($3);
                      agrif_parentcall =0; 
                   }
      ;
structure_component: lhs '%' lhs 
                   {
                      sprintf($$," %s %% %s ",$1,$3);
                     if ( incalldeclare == 0 ) inagrifcallargument = 0;
                   }
      ;
vec:  TOK_LEFTAB outlist TOK_RIGHTAB 
                   {sprintf($$,"(/%s/)",$2);} 
      ;
funarglist: beforefunctionuse    {strcpy($$," ");}
      | beforefunctionuse funargs  
                   {strcpy($$,$2);}
      ;
funargs: funarg     {strcpy($$,$1);}
      | funargs ',' funarg 
                    {sprintf($$,"%s,%s",$1,$3);} 
      ;
funarg: expr       {strcpy($$,$1);}
      | triplet    {strcpy($$,$1);}
      ;
triplet: expr ':' expr 
                    {sprintf($$,"%s:%s",$1,$3);} 
      | expr ':' expr ':' expr 
                    {sprintf($$,"%s:%s:%s",$1,$3,$5);} 
      | ':' expr ':' expr 
                    {sprintf($$,":%s:%s",$2,$4);} 
      | ':' ':' expr{sprintf($$,": : %s",$3);} 
      | ':' expr    {sprintf($$,":%s",$2);} 
      | expr ':'     {sprintf($$,"%s:",$1);} 
      | ':'         {sprintf($$,":");} 
      ;
ident : TOK_NAME    {  
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
                          if ( VarIsNonGridDepend($1) == 0 &&
                               formatdeclare == 0 
                             )
                          {
                             if ( inagrifcallargument == 1 ||
                                  varisallocatable_0($1) == 1 ||
                                  varispointer_0($1) == 1 )
                             {
                                ModifyTheVariableName_0($1);
                             }
                             if ( inagrifcallargument != 1 || sameagrifargument ==1 )
                                  ajoutevarindoloop_1($1);
                          }
                          NotifyAgrifFunction_0($1);
                       }
		    }
      ;
simple_const: TOK_TRUE 
                     {strcpy($$,".TRUE.");}
      | TOK_FALSE    {strcpy($$,".FALSE.");}
      | TOK_CSTINT   {strcpy($$,$1);}
      | TOK_CSTREAL  {strcpy($$,$1);}
      | TOK_CSTREALDP{strcpy($$,$1);}
      | TOK_CSTREALQP{strcpy($$,$1);}
      | simple_const TOK_NAME 
                     {sprintf($$,"%s%s",$1,$2);}
      | string_constant opt_substring
      ;
string_constant: TOK_CHAR_CONSTANT 
                     {strcpy($$,$1);}
      | string_constant TOK_CHAR_CONSTANT 
      | TOK_CHAR_MESSAGE 
                     {strcpy($$,$1);}
      | TOK_CHAR_CUT
                     {strcpy($$,$1);}
      ;
opt_substring:      {strcpy($$," ");}
      | substring   {strcpy($$,$1);}
      ;
substring: '(' optexpr ':' optexpr ')' 
                    {sprintf($$,"(%s:%s)",$2,$4);} 
      ;
optexpr:           {strcpy($$," ");}
      | expr        {strcpy($$,$1);}
      ;
opt_expr: '\n'          {strcpy($$," ");}
      | expr        {strcpy($$,$1);}
      ;
initial_value:      {InitialValueGiven = 0;}
      | before_initial '=' expr    
                    {
                       strcpy(InitValue,$3);
                       InitialValueGiven = 1;
                    } 
      ;
before_initial : {pos_curinit = setposcur();}
      ;
complex_const: '(' uexpr ',' uexpr ')' 
                    {sprintf($$,"(%s,%s)",$2,$4);} 
      ;
use_stat: word_use  module_name
                    {
                      /* if variables has been declared in a subroutine       */
                      if (insubroutinedeclare == 1)
                      {
                         copyuse_0($2);
                      }
                      sprintf(charusemodule,"%s",$2);
                      Addmoduletothelist_1($2);

                      if ( inmoduledeclare == 0 )
                      {
                         pos_end = setposcur();
                         RemoveWordSET_0(fortranout,pos_curuse,
                                               pos_end-pos_curuse);
                      }
                    }    
      | word_use  module_name ',' rename_list
                    {
                      if (insubroutinedeclare == 1)
                      {
                         completelistvarpointtovar_1($2,$4);
                      }
                      if ( firstpass == 1 ) 
                      {
                         if ( insubroutinedeclare == 1 )
                         {
                            coupletmp = $4;
                            strcpy(ligne,"");
                            while ( coupletmp )
                            {
                               strcat(ligne,coupletmp->namevar);
                               strcat(ligne," => ");
                               strcat(ligne,coupletmp->namepointedvar);
                               coupletmp = coupletmp->suiv;
                               if ( coupletmp ) strcat(ligne,",");
                            }
                            sprintf(charusemodule,"%s",$2);
                         }
                         Addmoduletothelist_1($2);
                      }
                      if ( inmoduledeclare == 0 )
                      {
                         pos_end = setposcur();
                         RemoveWordSET_0(fortranout,pos_curuse,
                                               pos_end-pos_curuse);
                      }
                    }    
      | word_use  module_name ',' TOK_ONLY ':' '\n'
                    {
                      /* if variables has been declared in a subroutine       */
                      if (insubroutinedeclare == 1)
                      {
                         copyuseonly_0($2);
                      }
                      sprintf(charusemodule,"%s",$2);
                      Addmoduletothelist_1($2);

                       if ( inmoduledeclare == 0 )
                       {
                          pos_end = setposcur();
                          RemoveWordSET_0(fortranout,pos_curuse,
                                                pos_end-pos_curuse);
                       }
                    }    
      | word_use  module_name ',' TOK_ONLY ':' only_list
                    {
                       /* if variables has been declared in a subroutine      */
                       if (insubroutinedeclare == 1)
                       {
                          completelistvarpointtovar_1($2,$6);
                       }
                       if ( firstpass == 1 ) 
                       {
                         if ( insubroutinedeclare == 1 )
                         {
                             coupletmp = $6;
                             strcpy(ligne,"");
                             while ( coupletmp )
                             {
                                strcat(ligne,coupletmp->namevar);
                                if ( strcasecmp(coupletmp->namepointedvar,"") )
                                                           strcat(ligne," => ");
                                strcat(ligne,coupletmp->namepointedvar);
                                coupletmp = coupletmp->suiv;
                                if ( coupletmp ) strcat(ligne,",");
                             }
                             sprintf(charusemodule,"%s",$2);
                          }
                          Addmoduletothelist_1($2);
                       }
                       if ( firstpass == 0 )
		       {
                          if ( inmoduledeclare == 0 )
                          {
                             pos_end = setposcur();
                             RemoveWordSET_0(fortranout,pos_curuse,
                                                   pos_end-pos_curuse);
                          }
		          else
		          {
   		             /* if we are in the module declare and if the    */
	   	             /* onlylist is a list of global variable         */
		             variableisglobalinmodule($6, $2, fortranout);
		          }
                       }
                    }    
      ;
word_use : TOK_USE
                   {
                      pos_curuse = setposcur()-strlen($1);
                   }
      ;
module_name: TOK_NAME 
                    {strcpy($$,$1);}
      ;
rename_list: rename_name
                    {
                       $$ = $1;
                    }                    
      | rename_list ',' rename_name
                    {
                        /* insert the variable in the list $1                 */
                        $3->suiv = $1;
                        $$ = $3;
                    }
      ;
rename_name: TOK_NAME TOK_POINT_TO TOK_NAME
                    {
                       coupletmp =(listcouple *)malloc(sizeof(listcouple));
                       strcpy(coupletmp->namevar,$1);
                       strcpy(coupletmp->namepointedvar,$3);
                       coupletmp->suiv = NULL;
                       $$ = coupletmp;
                     }
      ;
only_list: only_name 
                    {
                       $$ = $1;
                    }                    
      | only_list ',' only_name
                    {
                        /* insert the variable in the list $1                 */
                        $3->suiv = $1;
                        $$ = $3;
                    }
      ;
only_name: TOK_NAME TOK_POINT_TO TOK_NAME 
                    {
                       coupletmp =(listcouple *)malloc(sizeof(listcouple));
                       strcpy(coupletmp->namevar,$1);
                       strcpy(coupletmp->namepointedvar,$3);
                       coupletmp->suiv = NULL;
                       $$ = coupletmp;
                       pointedvar=1;
                       ajoutevarindoloop_1($1);
                    }
      | TOK_NAME    {
                       coupletmp =(listcouple *)malloc(sizeof(listcouple));
                       strcpy(coupletmp->namevar,$1);
                       strcpy(coupletmp->namepointedvar,"");
                       coupletmp->suiv = NULL;
                       $$ = coupletmp;
                     }
      ;
exec: iffable
      | TOK_ALLOCATE '(' allocation_list opt_stat_spec ')'
      | TOK_DEALLOCATE '(' allocate_object_list opt_stat_spec ')'
      | TOK_NULLIFY '(' pointer_name_list ')'
      | word_endunit /* end                                                   */
                    {
                       if ( inmodulemeet == 1 )
                       {
                         /* we are in a module                                */
                         if ( insubroutinedeclare == 1 )
                         {
                            /* it is like an end subroutine <name>            */
                            insubroutinedeclare = 0 ;
                            paramdeclaration_everdone = 0;
                            tmpdeclaration_everdone = 0;
                            /*                                                */
                            pos_cur = setposcur();		       
                            closeandcallsubloopandincludeit_0(1,$1,"");
                            /* at the end of the firstpas  we should remove   */
                            /*    from the listvarindoloop all variables      */
                            /*    which has not been declared as table in the */
                            /*    globliste                                   */
                            cleanlistvarfordoloop_1(1);
                         }
                         else
                         {
                            /* if we never meet the contains keyword          */
                            if ( inmoduledeclare == 1 )
                            {
                               if ( aftercontainsdeclare == 0 )
                               {
                                 CompleteGlobListeWithDatalist_1();
                                 addsubroutine_alloc_0(1);
                               }
                            }
                            /* it is like an end module <name>                */
                            inmoduledeclare = 0 ; 
                            inmodulemeet = 0 ; 
                         }
                       }
                       else
                       {
                          paramdeclaration_everdone = 0;
                          tmpdeclaration_everdone = 0;
                          insubroutinedeclare = 0;
                          /*                                                  */
                          pos_cur = setposcur();		       
                          closeandcallsubloopandincludeit_0(2,$1,"");
                          /* it is like end subroutine or end program         */
                          /*  Common case                                     */
                          /* at the end of the firstpas  we should remove     */
                          /*    from the listvarindoloop all variables which  */
                          /*    has not been declared as table in the         */
                          /*    globliste                                     */
                          cleanlistvarfordoloop_1(1);
                       }
                    }
      | word_endprogram opt_name
                    {
                       tmpdeclaration_everdone = 0;
                         paramdeclaration_everdone = 0;
                       insubroutinedeclare = 0;
                       /*                                                     */
                       pos_cur = setposcur();		       
                       closeandcallsubloopandincludeit_0(3,$1,$2);
                       /*  Common case                                        */
                       /* at the end of the firstpas  we should remove from   */
                       /*    the listvarindoloop all variables which has not  */
                       /*    been declared as table in the globliste          */
                       cleanlistvarfordoloop_1(3);
                    }
      | word_endsubroutine opt_name
                    {
                       tmpdeclaration_everdone = 0;
                       paramdeclaration_everdone = 0;
                       insubroutinedeclare = 0;
                       /*                                                     */
                       pos_cur = setposcur();		       
                       closeandcallsubloopandincludeit_0(1,$1,$2);
                       /*  Common case                                        */
                       /* at the end of the firstpas  we should remove from   */
                       /*    the listvarindoloop all variables which has not  */
                       /*    been declared as table in the globliste          */
                       cleanlistvarfordoloop_1(1);
                    }
      | word_endfunction opt_name
                    {
                       tmpdeclaration_everdone = 0;
                       paramdeclaration_everdone = 0;
                       insubroutinedeclare = 0;
                       /*                                                     */
                       pos_cur = setposcur();		       
                       closeandcallsubloopandincludeit_0(0,$1,$2);
                       /*  Common case                                        */
                       /* at the end of the firstpas  we should remove from   */
                       /*    the listvarindoloop all variables which has not  */
                       /*    been declared as table in the globliste          */
                       cleanlistvarfordoloop_1(0);
                    }
      | TOK_ENDMODULE opt_name
                    {
                       /* if we never meet the contains keyword               */
                       if ( inmoduledeclare == 1 )
                       {
                          if ( aftercontainsdeclare == 0 )
                          {
                             CompleteGlobListeWithDatalist_1();
                             addsubroutine_alloc_0(1);
                          }
                       }
                       inmoduledeclare = 0 ; 
                       inmodulemeet = 0 ; 
                       /* Write the list of module used in this file          */
		       if ( firstpass == 0 )
		       {
                          Writethedependlistofmoduleused(curmodulename);
                          WritedependParameterList(curmodulename);
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
      | TOK_CASE '(' caselist ')'
      | TOK_SELECTCASE '(' expr ')'
      | TOK_CASEDEFAULT
      | TOK_ENDSELECT
      | TOK_CONTAINS
                   {
                      if (inmoduledeclare == 1 )
                      {
                         CompleteGlobListeWithDatalist_1();
                         addsubroutine_alloc_0(0);
                      }
                      inmoduledeclare = 0 ; 
                      aftercontainsdeclare = 1;
                   }
      ;
      
word_endsubroutine: TOK_ENDSUBROUTINE
                    {
		       strcpy($$,$1);
                       pos_endsubroutine = setposcur()-strlen($1);
		    }
      ;
word_endunit: TOK_ENDUNIT
                    {
		       strcpy($$,$1);
                       pos_endsubroutine = setposcur()-strlen($1);
		    }
      ;
word_endprogram:  TOK_ENDPROGRAM
                    {
		       strcpy($$,$1);
                       pos_endsubroutine = setposcur()-strlen($1);
		    }
      ;
word_endfunction: TOK_ENDFUNCTION
                    {
		       strcpy($$,$1);
                       pos_endsubroutine = setposcur()-strlen($1);
		    }
      ;
caselist: expr
      | caselist ',' expr
      | caselist ':' expr
      ;
boucledo : worddo opt_int do_var '=' expr ',' expr
      | worddo opt_int do_var '=' expr ',' expr ',' expr
      | wordwhile expr
      | TOK_ENDDO optname
      ;
opt_int : 
      | TOK_CSTINT
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

dotarget:
      | TOK_CSTINT
      ;

iffable: TOK_CONTINUE
      | ident_dims after_ident_dims
      | goto
      | io
      | call 
      | TOK_ALLOCATE '(' allocation_list opt_stat_spec ')'
      | TOK_DEALLOCATE '(' allocate_object_list opt_stat_spec ')'
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
call: keywordcall opt_call 
                   {
                      inagrifcallargument = 0 ;
                      incalldeclare=0;
                      if ( oldfortranout && 
                           !strcasecmp(meetagrifinitgrids,subroutinename) && 
                           firstpass == 0 &&
                           callmpiinit == 1)
                      {
                         pos_end = setposcur();
                         RemoveWordSET_0(fortranout,pos_curcall,
                                               pos_end-pos_curcall);
                         fprintf(oldfortranout,"      Call MPI_Init (%s) \n"
                                                                   ,mpiinitvar);
                      }
                      if ( oldfortranout           && 
                           callagrifinitgrids == 1 && 
                           firstpass == 0 )
                      {
                         pos_end = setposcur();
                         RemoveWordSET_0(fortranout,pos_curcall,
                                               pos_end-pos_curcall);
                         fprintf(oldfortranout,
                                           "      Call Agrif_Init_Grids () \n");
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
                          strcpy(meetmpiinit,subroutinename);
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
                       if ( Vartonumber($2) == 1 ) 
                       {
                          incalldeclare=1;
                          inagrifcallargument = 1 ;
                          AddsubroutineTolistsubwhereagrifused();
                       }
                    }
      ;
before_call : TOK_CALL
                    {pos_curcall=setposcur()-4;}
callarglist:  callarg
      | callarglist ',' callarg
      ;

callarg:  expr {
                  if ( callmpiinit == 1 ) 
                  {
                     strcpy(mpiinitvar,$1);
                     if ( firstpass == 1 ) 
                     {
                        curvar=createvar($1,NULL);
                        curlistvar=insertvar(NULL,curvar);
                        listargsubroutine = AddListvarToListvar
                                               (curlistvar,listargsubroutine,1);
                     }
                  }
               }
      | '*' label
      ;

stop: TOK_PAUSE
      | TOK_STOP
      ;

io: iofctl ioctl
      | read option_read
      | TOK_REWIND after_rewind
      | wordformat debut_format ioctl_format fin_format
                    {formatdeclare = 0;}
      ;
wordformat : TOK_FORMAT
                    {formatdeclare = 1;}
opt_ioctlformat : 
      | ioctl_format
      ;
opt_ioctl_format : 
      | ',' ioctl_format
      | ',' '*'
      ;
debut_format : TOK_LEFTAB opt_comma
      | '('
      ;
ioctl_format : format_expr
      |   ioctl_format ',' format_expr
      ;
format_expr : 
      | uexpr
      | TOK_CSTINT TOK_CHAR_INT
      | TOK_CSTINT debut_format ioctl_format fin_format
      | TOK_SLASH opt_CHAR_INT
      | TOK_CHAR_INT TOK_SLASH format_expr
      | TOK_SLASH TOK_SLASH
      | TOK_CHAR_INT
      | '(' format_expr ')'
      | '(' uexpr ')'
      ;
opt_CHAR_INT : 
      | TOK_CSTINT TOK_NAME
      ;
fin_format : opt_comma TOK_RIGHTAB opt_comma
      | ')'
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
ioctl:  '(' ctllist ')'
      | '(' fexpr ')' 
      ;
after_rewind:  '(' ident ')'
      | TOK_NAME
      ;
ctllist: ioclause
      | ctllist ',' ioclause
      ;
ioclause: fexpr 
      | '*'
      | TOK_DASTER
      | TOK_NAME expr 
      | TOK_NAME expr '%' ident_dims
      | TOK_NAME '(' triplet ')'
      | TOK_NAME '*' 
      | TOK_NAME TOK_DASTER 
      ;
iofctl: TOK_OPEN
      | TOK_CLOSE
      ;
infmt:  unpar_fexpr
      | '*'
      ;

read:TOK_READ
      | TOK_INQUIRE
      | TOK_WRITE
      | TOK_PRINT
      ;
fexpr: unpar_fexpr
      | '(' fexpr ')'
      ;
unpar_fexpr: lhs
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
addop: '+'
      | '-'
      ;
inlist: inelt
      | inlist ',' inelt
      ;
opt_lhs : 
      | lhs
      ;
inelt: opt_lhs opt_operation
      | '(' inlist ')' opt_operation
      | predefinedfunction opt_operation
      | simple_const opt_operation 
      | '(' inlist ',' dospec ')'
      ;
opt_operation :
      | operation
      | opt_operation operation
      ;      
outlist: other      {strcpy($$,$1);} 
      | out2       {strcpy($$,$1);} 
      ;
out2: uexpr ',' expr
                   {sprintf($$,"%s,%s",$1,$3);} 
      | uexpr ',' other 
                   {sprintf($$,"%s,%s",$1,$3);} 
      | other ',' expr 
                   {sprintf($$,"%s,%s",$1,$3);} 
      | other ',' other 
                   {sprintf($$,"%s,%s",$1,$3);} 
      | out2 ',' expr 
                   {sprintf($$,"%s,%s",$1,$3);} 
      | out2 ',' other 
                   {sprintf($$,"%s,%s",$1,$3);} 
      | uexpr     {strcpy($$,$1);} 
      | predefinedfunction {strcpy($$,$1);} 
      ;
other:  complex_const 
                   {strcpy($$,$1);} 
      | '(' expr ')' 
                   {sprintf($$," (%s)",$2);} 
      | '(' uexpr ',' dospec ')'
                   {sprintf($$,"(%s,%s)",$2,$4);} 
      | '(' other ',' dospec ')'
                   {sprintf($$,"(%s,%s)",$2,$4);} 
      | '(' out2 ',' dospec ')'
                   {sprintf($$,"(%s,%s)",$2,$4);} 
      ;

dospec: TOK_NAME '=' expr ',' expr 
                   {sprintf($$,"%s=%s,%s)",$1,$3,$5);} 
      | TOK_NAME '=' expr ',' expr ',' expr 
                   {sprintf($$,"%s=%s,%s,%s)",$1,$3,$5,$7);} 
      ;
labellist: label
      | labellist ',' label
      ;
label: TOK_CSTINT
      ;
goto: TOK_PLAINGOTO label
      ;
allocation_list: allocate_object
      | ident_dims
      | allocation_list ',' allocate_object
      ;
allocate_object: ident
                   {AddIdentToTheAllocateList_1($1);}
      | structure_component
      | array_element
      ;
array_element: ident '(' funargs ')' 
                   {AddIdentToTheAllocateList_1($1);}
      ;
subscript_list: expr 
      | subscript_list ',' expr 
      ;

allocate_object_list:allocate_object
      | allocate_object_list ',' allocate_object
      ;
opt_stat_spec:
      | ',' TOK_STAT '=' ident
      ;
pointer_name_list: ident
      | pointer_name_list ',' ident
      ;
opt_construct_name:
      | TOK_NAME
      ;
opt_construct_name_colon:
      | TOK_CONSTRUCTID ':'
      ;
logif: TOK_LOGICALIF expr ')'
      ;
do_var: ident {strcpy($$,$1);}
      ;
%%

void processfortran(char *fichier_entree)
{
   extern FILE *fortranin;
   extern FILE *fortranout;
   char nomfile[LONGNOM];
   int c;
   int confirmyes;

   /*fortrandebug = 1;*/
   if ( todebug == 1 ) printf("Firstpass == %d \n",firstpass);
/******************************************************************************/
/*  1-  Open input and output files                                           */
/******************************************************************************/
   strcpy(OriginalFileName,fichier_entree); 
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
   formatdeclare = 0;
   ExternalDeclare = 0; 
   SaveDeclare = 0;
   indeclarationvar=0;
   pointerdeclare = 0;
   optionaldeclare = 0;
   incalldeclare = 0;
   infunctiondeclare = 0 ;
   Allocatabledeclare = 0 ;
   strcpy(NamePrecision," "); 
   VariableIsParameter =  0 ; 
   strcpy(NamePrecision,"");  
   c_star = 0 ; 
   insubroutinedeclare = 0 ;
   strcpy(subroutinename," "); 
   InitialValueGiven = 0 ; 
   strcpy(EmptyChar," "); 
   inmoduledeclare = 0;
   colnum=0;
   incom=0;
   couldaddvariable=1;
   aftercontainsdeclare = 1;
   /* Name of the file without format                                         */
   tmp = strchr(fichier_entree, '.');
   strncpy(curfilename,fichier_entree,strlen(fichier_entree)-strlen(tmp)); 
/******************************************************************************/
/*  2-  Parsing of the input file (1 time)                                    */
/******************************************************************************/
   if (firstpass == 0 ) 
   {
      fortranout=fopen(nomfileoutput,"w");
      /* we should add the new module comes from common block                 */
      if (fortran77 == 1 ) 
      {
         convert2lower(curfilename);
         fprintf
                      (fortranout,"#include \"NewModule_%s.h\" \n",curfilename);
      }
   }

   fortranparse();

   strcpy(curfile,mainfile);

   if (firstpass == 0 ) fclose(fortranout);
}
