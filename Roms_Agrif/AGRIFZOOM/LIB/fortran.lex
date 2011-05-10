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
%x parameter
%s character
%x donottreat
%s fortran77style
%s fortran90style
%{
#include <math.h>
#include <stdlib.h>
#include <string.h>
extern FILE * yyin;
#define MAX_INCLUDE_DEPTH 30
#define tabsize 6
YY_BUFFER_STATE include_stack[MAX_INCLUDE_DEPTH];
int line_num_fortran=1;
int line_num_fortran_common=1;
int newlinef90 = 0;
char *tmp;
char tmpc;
/******************************************************************************/
/**************PETITS PB NON PREVUS *******************************************/
/******************************************************************************/
/* NEXTLINF77 un ligne fortran 77 peut commencer par -      &a=b or on        */
/*            a prevu seulement       & a=b avec l'espace entre le symbole    */
/*            de la 7eme et le debut de la ligne de commande                  */
/*            le ! est aussi interdit comme symbole de la 7 eme colonne       */
/*            Normalement NEXTLINEF77 \n+[ ]{5}[^ ]                           */
/******************************************************************************/
#define YY_USER_ACTION \
        {\
           if (firstpass == 0) \
           {\
              strcat(curbuf,yytext); \
              Save_Length(curbuf,38); \
              strcpy(motparse,yytext);\
              Save_Length(motparse,32); \
              colnum = colnum + strlen(motparse);\
              ECHO; \
           }\
           strcpy(motparse1,yytext);\
           /*printf("yytext = %s\n",yytext);*/\
        /*if ( firstpass == 1 ) 
                      printf("yytext = %s %d\n",yytext,strlen(yytext));*/\
        }
%}
AGRIFDEB "Agrif_debut"
AGRIFFIN "Agrif_fin"
REAL8 "real*8"[ \t]*"(a-h,o-z)"
NOTTREAT Agrif_do_not_treat
ENDNOTTREAT Agrif_end_do_not_treat

NIMPORTEQUOI .
SLASH "/"
DSLASH "/"[ \t]*"/"
NAME [a-zA-Z\_][a-zA-Z0-9\_]*
DIGIT [0-9]+
INT {DIGIT}
EXPONENT e[-+]?{DIGIT}
DEXPONENT d[-+]?{DIGIT}
QEXPONENT q[-+]?{DIGIT}
REAL (({DIGIT}\.[0-9]+|[0-9]*\.{DIGIT}){EXPONENT}?)|{DIGIT}\.{EXPONENT}
REALDP (({DIGIT}\.[0-9]+|[0-9]*\.{DIGIT}){DEXPONENT}?)|{DIGIT}\.{DEXPONENT}
REALQP (({DIGIT}\.[0-9]+|[0-9]*\.{DIGIT}){QEXPONENT}?)|{DIGIT}\.{QEXPONENT}
ENDFUNCTION end[ \t]*function
DOUBLEPRECISION double[ \t]*precision
DOUBLECOMPLEX double[ \t]*complex

COMMENTAIRESFORTRAN77 ^([Cc*](([ \t]*\n)|([^AaHhOo\n]{NIMPORTEQUOI}*\n)))
COMMENTAIRESFORTRAN90 ^([ \t]*!{NIMPORTEQUOI}*\n)
COMMENTAIRESFORTRAN90_2 (!{NIMPORTEQUOI}*)
NEXTLINEF90 "&"{NIMPORTEQUOI}*[\n]*
NEXTLINEF77 \n[ \t]{5}("&"|"+"|"$"|"*"|"1"|"2"|"3"|"4"|"5"|"6"|"7"|"8"|"."|"#")
%%
  if (infixed) BEGIN(fortran77style) ;
  if (infree) BEGIN(fortran90style)  ;

^C${AGRIFDEB}            return TOK_DEBUT;
^C${AGRIFFIN}            return TOK_FIN;
^C$OMP[ \t]*{NIMPORTEQUOI}* return TOK_OMP;
^C$[ \t]*{NIMPORTEQUOI}* return TOK_DOLLAR;

{REAL8}                 {return TOK_REAL8;}
subroutine              {return TOK_SUBROUTINE;}
program                 {return TOK_PROGRAM;}
allocate                {inallocate = 1; return TOK_ALLOCATE;}
deallocate              {inallocate = 1; return TOK_DEALLOCATE;}
result                  {return TOK_RESULT;}
function                {return TOK_FUNCTION;}
end[ \t]*subroutine     {strcpy(yylval.na,yytext);return TOK_ENDSUBROUTINE;}
end[ \t]*program        {strcpy(yylval.na,yytext);return TOK_ENDPROGRAM;}
end[ \t]*function       {strcpy(yylval.na,yytext);return TOK_ENDFUNCTION;}
end                     {strcpy(yylval.na,yytext);return TOK_ENDUNIT;}
include                  return TOK_INCLUDE;
^[ \t]*use[ ]+           {
                            strcpy(yylval.na,yytext);
                            tmpc = input();
                            unput(tmpc);
                            if ( (
                               tmpc >= 'a' && tmpc <= 'z'
                                  ) || (
                               tmpc >= 'A' && tmpc <= 'Z'
                               )  )
                               {
                                  return TOK_USE;
                               }
                               else
                               {
                                  return TOK_NAME;
                               }
                         }
rewind                  {return TOK_REWIND;}
implicit                 return TOK_IMPLICIT;
none                     return TOK_NONE;
call                     return TOK_CALL;
.true.                   return TOK_TRUE;
.false.                  return TOK_FALSE;
\=\>                    {return TOK_POINT_TO;}
\*\*                    {strcpy(yylval.na,yytext);return TOK_DASTER;}
\.[ \t]*eqv\.            {strcpy(yylval.na,yytext);return TOK_EQV;}
\.[ \t]*eq\.            {strcpy(yylval.na,yytext);return TOK_EQ;}
\.[ \t]*gt\.            {strcpy(yylval.na,yytext);return TOK_GT;}
\.[ \t]*ge\.            {strcpy(yylval.na,yytext);return TOK_GE;}
\.[ \t]*lt\.            {strcpy(yylval.na,yytext);return TOK_LT;}
\.[ \t]*le\.            {strcpy(yylval.na,yytext);return TOK_LE;}
\.[ \t]*neqv\.          {strcpy(yylval.na,yytext);return TOK_NEQV;}
\.[ \t]*ne\.            {strcpy(yylval.na,yytext);return TOK_NE;}
\.[ \t]*not\.           {strcpy(yylval.na,yytext);return TOK_NOT;}
\.[ \t]*or\.            {strcpy(yylval.na,yytext);return TOK_OR;}
\.[ \t]*xor\.           {strcpy(yylval.na,yytext);return TOK_XOR;}
\.[ \t]*and\.           {strcpy(yylval.na,yytext);return TOK_AND;}
module                  {return TOK_MODULE;}
do[ 0-9\t]*while           {return TOK_DOWHILE;}
end[ \t]*module          return TOK_ENDMODULE;
end[ \t]*do              return TOK_ENDDO;
do                      {return TOK_PLAINDO;}
real                    {strcpy(yylval.na,yytext);return TOK_REAL;}
integer                 {strcpy(yylval.na,yytext);return TOK_INTEGER;}
logical                 {strcpy(yylval.na,yytext);return TOK_LOGICAL;}
character               {strcpy(yylval.na,yytext);return TOK_CHARACTER;}
allocatable             {return TOK_ALLOCATABLE;}
close                    return TOK_CLOSE;
inquire                  return TOK_INQUIRE;
dimension               {return TOK_DIMENSION;}
pause                    return TOK_PAUSE;
equivalence              return TOK_EQUIVALENCE;
stop                     return TOK_STOP;
where                    return TOK_WHERE;
end[ \t]*where           return TOK_ENDWHERE;
else[ \t]*where          return TOK_ELSEWHERE;
complex                 {return TOK_COMPLEX;}
^[ \t]*contains         {return TOK_CONTAINS;}
only                    {return TOK_ONLY;}
parameter               {return TOK_PARAMETER;}
recursive               {return TOK_RECURSIVE;}
common                  {return TOK_COMMON;}
^[ \t]*global[ \t]+     {return TOK_GLOBAL;}
external                {return TOK_EXTERNAL;}
intent                  {return TOK_INTENT;}
pointer                 {return TOK_POINTER;}
optional                {return TOK_OPTIONAL;}
save                    {return TOK_SAVE;}
^[ \t]*type[ \t\,]+       {return TOK_TYPE;}
^[ \t]*type[ \t]*\(     {return TOK_TYPEPAR;}
stat                    {if (inallocate == 1) return TOK_STAT; else {strcpy(yylval.na,yytext);return TOK_NAME;}}
end[ \t]*type           {return TOK_ENDTYPE;}
open                     return TOK_OPEN;
return                   return TOK_RETURN;
exit[^(]                 return TOK_EXIT;
print                    return TOK_PRINT;
module[ \t]*procedure   {return TOK_PROCEDURE;}
read                    {return TOK_READ;}
namelist                {return TOK_NAMELIST;}
write                   {return TOK_WRITE;}
target                  {return TOK_TARGET;}
public                  {return TOK_PUBLIC;}
private                 {return TOK_PRIVATE;}
in                      {strcpy(yylval.nac,yytext);return TOK_IN;}
^[ \t]*data[ \t]+       {strcpy(yylval.na,yytext);return TOK_DATA;}
continue                 return TOK_CONTINUE;
go[ \t]*to              {return TOK_PLAINGOTO;}
out                     {strcpy(yylval.nac,yytext);return TOK_OUT;}
inout                   {strcpy(yylval.nac,yytext);return TOK_INOUT;}
intrinsic               {return TOK_INTRINSIC;}
then                    {return TOK_THEN;}
else[ \t]*if            {return TOK_ELSEIF;}
else                    {return TOK_ELSE;}
end[ \t]*if             {return TOK_ENDIF;}
if[ \t]*\(              {return TOK_LOGICALIF;}
sum[ \t]*\(             {return TOK_SUM;}
max[ \t]*\(             {return TOK_MAX;}
tanh                    {return TOK_TANH;}
maxval                  {return TOK_MAXVAL;}
trim                    {return TOK_TRIM;}
sqrt\(                  {return TOK_SQRT;}
select[ \t]*case        {return TOK_SELECTCASE;}
^[ \t]*case[ \t]*\(     {return TOK_CASE;}
^[ \t]*case[ \t]*default       {return TOK_CASEDEFAULT;}
end[ \t]*select         {return TOK_ENDSELECT;}
file[ \t]*\=            {return TOK_FILE;}
end[ \t]*\=             {return TOK_END;}
err[ \t]*\=             {return TOK_ERR;}
exist[ \t]*\=           {return TOK_EXIST;}
min[ \t]*\(             {return TOK_MIN;}
nint                    {return TOK_NINT;}
float                   {return TOK_FLOAT;}
exp                     {return TOK_EXP;}
cos                     {return TOK_COS;}
cosh                    {return TOK_COSH;}
acos                    {return TOK_ACOS;}
sin                     {return TOK_SIN;}
sinh                    {return TOK_SINH;}
asin                    {return TOK_ASIN;}
log                     {return TOK_LOG;}
tan                     {return TOK_TAN;}
atan                    {return TOK_ATAN;}
cycle                   {return TOK_CYCLE;}
abs\(                   {return TOK_ABS;}
mod                     {return TOK_MOD;}
sign                    {return TOK_SIGN;}
minloc                  {return TOK_MINLOC;}
maxloc                  {return TOK_MAXLOC;}
minval                  {return TOK_MINVAL;}
interface               {return TOK_INTERFACE;}
backspace               {return TOK_BACKSPACE;}
end[ \t]*interface      {return TOK_ENDINTERFACE;}
\({SLASH}               {return TOK_LEFTAB;}
{SLASH}\)               {return TOK_RIGHTAB;}
format[ \t]*\(({NIMPORTEQUOI}|{NEXTLINEF90}|{NEXTLINEF77})*\)  {return TOK_FORMAT;}
{DOUBLEPRECISION}       {strcpy(yylval.na,yytext);return TOK_DOUBLEPRECISION;}
{DOUBLECOMPLEX}         {strcpy(yylval.na,yytext);return TOK_DOUBLECOMPLEX;}
{SLASH}                 {strcpy(yylval.na,yytext);return TOK_SLASH;}
DSLASH                  {strcpy(yylval.na,yytext);return TOK_DSLASH;}
(\')[^']*&{0,1}\n[ \t]*&{0,1}[^']*(\')         {strcpy(yylval.na,yytext);return TOK_CHAR_CUT;}
(\')[^\n']*(\')         {strcpy(yylval.na,yytext);return TOK_CHAR_CONSTANT;}
(\")[^\n]*(\")          {strcpy(yylval.na,yytext);return TOK_CHAR_MESSAGE;}
({NAME}{REAL})          {strcpy(yylval.na,yytext);return TOK_CHAR_INT;}
{NAME}                  {strcpy(yylval.na,yytext);return TOK_NAME;}
{REAL}                  {strcpy(yylval.na,yytext);return TOK_CSTREAL;}
{REALDP}                {strcpy(yylval.na,yytext);return TOK_CSTREALDP;}
{REALQP}                {strcpy(yylval.na,yytext);return TOK_CSTREALQP;}
({DIGIT}\.)/[^{NAME}|"and."|"false."|"true."|"eq."|"or."|"gt."|"ge."|"lt."|"le."|"not."|"ne."] {strcpy(yylval.na,yytext);return TOK_CSTREAL;}
{INT}                   {strcpy(yylval.na,yytext);return TOK_CSTINT;}
\$ {}
\'|\"                   {return TOK_QUOTE;}
\.                      {}
\(|\)|:|\[|\]|\+|\-|\*|\% {strcpy(yylval.na,yytext);return (int) *yytext;}
\;                      {return TOK_SEMICOLON;}
\,                      {return (int) *yytext;}
\=                      {return (int) *yytext;}
\<                      {return (int) *yytext;}
\>                      {return (int) *yytext;}
\n                      {colnum=0;line_num_fortran++;line_num_fortran_common++; return (int) *yytext;}
^[ ]*$
^(((" "|[0-9]){1,5})|([ \t]{1,5}))[ &]+ {if (newlinef90 == 0) return TOK_LABEL; else newlinef90 = 0;}
[ ]+
[\t]+                   {colnum=colnum-1+tabsize;}
[ \t]+ ;
{NEXTLINEF90}           {line_num_fortran++;line_num_fortran_common++;newlinef90=1;colnum=0;}
{NEXTLINEF77}           {line_num_fortran++;line_num_fortran_common++;colnum=0;}
<fortran77style>{COMMENTAIRESFORTRAN77} {
                           convert2lower(motparse1);
                           if ( strncasecmp(motparse1,"contains",8) == 0 )
                           {
                              return TOK_CONTAINS;
                           }
                           else
                           {
                              colnum=0;line_num_fortran++;line_num_fortran_common++;
                             if ( !strcasecmp(motparse1,"C$AGRIF_DO_NOT_TREAT\n")) return TOK_DONOTTREAT;
                             if ( !strcasecmp(motparse1,"C$AGRIF_END_DO_NOT_TREAT\n")) return TOK_ENDDONOTTREAT;
                           }
                         }
^"!$AGRIF_DO_NOT_TREAT\n" BEGIN(donottreat);
<donottreat>^"!$AGRIF_END_DO_NOT_TREAT\n" BEGIN(INITIAL);
<donottreat>.*\n
{COMMENTAIRESFORTRAN90}   {
                             colnum = 0;
                             if ( !strcasecmp(motparse1,"!$AGRIF_DO_NOT_TREAT\n")) return TOK_DONOTTREAT;
                             if ( !strcasecmp(motparse1,"!$AGRIF_END_DO_NOT_TREAT\n")) return TOK_ENDDONOTTREAT;
                          }
{COMMENTAIRESFORTRAN90_2} {
                             colnum = 0;
                             if ( !strcasecmp(motparse1,"!$AGRIF_DO_NOT_TREAT\n")) return TOK_DONOTTREAT;
                             if ( !strcasecmp(motparse1,"!$AGRIF_END_DO_NOT_TREAT\n")) return TOK_ENDDONOTTREAT;
                          }
%%

fortranerror(char *s)
{
   if (!strcasecmp(curfile,mainfile))
   {
      printf("%s line %d, file %s\n",s,line_num_fortran,curfile);
   }
   else
   {
      printf("%s line %d, file %s\n",s,line_num_fortran_common,curfile);
   }
   /*exit(0);*/
}

int fortranwrap()
{
}
