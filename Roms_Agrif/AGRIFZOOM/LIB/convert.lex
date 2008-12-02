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
%s character
%{
#include <math.h>
#include <stdlib.h>
#include <string.h>
int line_num=1;
extern FILE * yyin;
#define MAX_INCLUDE_DEPTH 30
YY_BUFFER_STATE include_stack[MAX_INCLUDE_DEPTH];
%}

COMMENT "%"
SEPARATEUR "::"
NIMPORTEQUOI .
COMMENTAIRES1 {COMMENT}{NIMPORTEQUOI}*{COMMENT}
PROBTYPE "1D"|"2D"|"3D"
USEITEM "FIXED_GRIDS"|"ONLY_FIXED_GRIDS"|"F77"
NAME [a-zA-Z\_][a-zA-Z0-9\_]*
DIGIT [0-9]+
NEXTLINE \n+[ \t]+"$"|\n+[ \t]+"&"
%%
parammodule return TOK_MODULEMAIN; /* name of the module                      */
notgriddep  return TOK_NOTGRIDDEP; /* variable which are not grid dependent   */
use         return TOK_USE;
{COMMENTAIRES1}    {}
{SEPARATEUR}        return TOK_SEP;
{USEITEM}          {strcpy(yylval.na,yytext); return TOK_USEITEM;}
{PROBTYPE}         {strcpy(yylval.na,yytext); return TOK_PROBTYPE;}
                                   /* dimension of the problem                */
{NAME}             {strcpy(yylval.na,yytext); return TOK_NAME;}
;|\,|\(|\)|:|\[|\] {return (int) *yytext;}
\n                 {line_num++;return (int) *yytext;}
[ \t]+ ;
%%


int yywrap()
{
}


yyerror(char *s)
{
if (!strcasecmp(curfile,mainfile))
{
   printf("Dans convert %s line %d, fichier %s\n",s,line_num,curfile);
}
else
{
   printf("Dans convert %s line %d, fichier %s\n",s,line_num,curfile);
}
exit(0);
}
