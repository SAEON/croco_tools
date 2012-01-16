/* A Bison parser, made by GNU Bison 2.5.  */

/* Bison implementation for Yacc-like parsers in C
   
      Copyright (C) 1984, 1989-1990, 2000-2011 Free Software Foundation, Inc.
   
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.
   
   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "2.5"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1

/* Using locations.  */
#define YYLSP_NEEDED 0

/* Substitute the variable and function names.  */
#define yyparse         fortran_parse
#define yylex           fortran_lex
#define yyerror         fortran_error
#define yylval          fortran_lval
#define yychar          fortran_char
#define yydebug         fortran_debug
#define yynerrs         fortran_nerrs


/* Copy the first part of user declarations.  */

/* Line 268 of yacc.c  */
#line 36 "fortran.y"

#define YYMAXDEPTH 1000
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "decl.h"

extern int line_num_input;
extern char *fortran_text;

char *tmp;
char c_selectorname[LONG_C];
char ligne[LONG_C];
char truename[LONGNOM];
char identcopy[LONG_C];
int c_selectorgiven=0;
listvar *curlistvar;
typedim c_selectordim;
listcouple *coupletmp;
listdim *parcoursdim;
int removeline=0;
listvar *test;

int fortran_error(const char *s)
{
    if (!strcasecmp(curfile,mainfile))
        printf("%s line %d, file %s motclef = |%s|\n",s,line_num_input,curfile,fortran_text);
    else
        printf("%s line %d, file %s motclef = |%s| curbuf = |%s|\n",s,line_num_input,curfile,fortran_text,curbuf);
    exit(0);
}



/* Line 268 of yacc.c  */
#line 114 "fortran.tab.c"

/* Enabling traces.  */
#ifndef YYDEBUG
# define YYDEBUG 1
#endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 0
#endif

/* Enabling the token table.  */
#ifndef YYTOKEN_TABLE
# define YYTOKEN_TABLE 0
#endif


/* Tokens.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
   /* Put the tokens into the symbol table, so that GDB and other debuggers
      know about them.  */
   enum yytokentype {
     TOK_NEQV = 258,
     TOK_EQV = 259,
     TOK_XOR = 260,
     TOK_OR = 261,
     TOK_AND = 262,
     TOK_NOT = 263,
     TOK_NE = 264,
     TOK_EQ = 265,
     TOK_GE = 266,
     TOK_LE = 267,
     TOK_GT = 268,
     TOK_LT = 269,
     TOK_DSLASH = 270,
     TOK_SLASH = 271,
     TOK_DASTER = 272,
     TOK_SEMICOLON = 273,
     TOK_PARAMETER = 274,
     TOK_RESULT = 275,
     TOK_ONLY = 276,
     TOK_INCLUDE = 277,
     TOK_SUBROUTINE = 278,
     TOK_PROGRAM = 279,
     TOK_FUNCTION = 280,
     TOK_FORMAT = 281,
     TOK_MAX = 282,
     TOK_TANH = 283,
     TOK_WHERE = 284,
     TOK_ELSEWHERE = 285,
     TOK_ENDWHERE = 286,
     TOK_MAXVAL = 287,
     TOK_TRIM = 288,
     TOK_SUM = 289,
     TOK_SQRT = 290,
     TOK_CASE = 291,
     TOK_SELECTCASE = 292,
     TOK_FILE = 293,
     TOK_END = 294,
     TOK_ERR = 295,
     TOK_EXIST = 296,
     TOK_MIN = 297,
     TOK_FLOAT = 298,
     TOK_EXP = 299,
     TOK_COS = 300,
     TOK_COSH = 301,
     TOK_ACOS = 302,
     TOK_NINT = 303,
     TOK_CYCLE = 304,
     TOK_SIN = 305,
     TOK_SINH = 306,
     TOK_ASIN = 307,
     TOK_EQUIVALENCE = 308,
     TOK_BACKSPACE = 309,
     TOK_LOG = 310,
     TOK_TAN = 311,
     TOK_ATAN = 312,
     TOK_RECURSIVE = 313,
     TOK_ABS = 314,
     TOK_MOD = 315,
     TOK_SIGN = 316,
     TOK_MINLOC = 317,
     TOK_MAXLOC = 318,
     TOK_EXIT = 319,
     TOK_MINVAL = 320,
     TOK_PUBLIC = 321,
     TOK_PRIVATE = 322,
     TOK_ALLOCATABLE = 323,
     TOK_RETURN = 324,
     TOK_THEN = 325,
     TOK_ELSEIF = 326,
     TOK_ELSE = 327,
     TOK_ENDIF = 328,
     TOK_PRINT = 329,
     TOK_PLAINGOTO = 330,
     TOK_LOGICALIF = 331,
     TOK_PLAINDO = 332,
     TOK_CONTAINS = 333,
     TOK_ENDDO = 334,
     TOK_MODULE = 335,
     TOK_ENDMODULE = 336,
     TOK_DOWHILE = 337,
     TOK_ALLOCATE = 338,
     TOK_OPEN = 339,
     TOK_CLOSE = 340,
     TOK_INQUIRE = 341,
     TOK_WRITE = 342,
     TOK_FLUSH = 343,
     TOK_READ = 344,
     TOK_REWIND = 345,
     TOK_DEALLOCATE = 346,
     TOK_NULLIFY = 347,
     TOK_DIMENSION = 348,
     TOK_ENDSELECT = 349,
     TOK_EXTERNAL = 350,
     TOK_INTENT = 351,
     TOK_INTRINSIC = 352,
     TOK_NAMELIST = 353,
     TOK_CASEDEFAULT = 354,
     TOK_OPTIONAL = 355,
     TOK_POINTER = 356,
     TOK_CONTINUE = 357,
     TOK_SAVE = 358,
     TOK_TARGET = 359,
     TOK_IMPLICIT = 360,
     TOK_NONE = 361,
     TOK_CALL = 362,
     TOK_STAT = 363,
     TOK_POINT_TO = 364,
     TOK_COMMON = 365,
     TOK_GLOBAL = 366,
     TOK_LEFTAB = 367,
     TOK_RIGHTAB = 368,
     TOK_PAUSE = 369,
     TOK_PROCEDURE = 370,
     TOK_STOP = 371,
     TOK_REAL8 = 372,
     TOK_OUT = 373,
     TOK_INOUT = 374,
     TOK_IN = 375,
     TOK_USE = 376,
     TOK_TRUE = 377,
     TOK_FALSE = 378,
     TOK_LABEL = 379,
     TOK_TYPE = 380,
     TOK_TYPEPAR = 381,
     TOK_ENDTYPE = 382,
     TOK_REAL = 383,
     TOK_INTEGER = 384,
     TOK_LOGICAL = 385,
     TOK_DOUBLEPRECISION = 386,
     TOK_ENDSUBROUTINE = 387,
     TOK_ENDFUNCTION = 388,
     TOK_ENDPROGRAM = 389,
     TOK_ENDUNIT = 390,
     TOK_CHARACTER = 391,
     TOK_CHAR_CONSTANT = 392,
     TOK_CHAR_CUT = 393,
     TOK_DATA = 394,
     TOK_CHAR_MESSAGE = 395,
     TOK_CSTREAL = 396,
     TOK_COMPLEX = 397,
     TOK_DOUBLECOMPLEX = 398,
     TOK_NAME = 399,
     TOK_CSTINT = 400
   };
#endif



#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE
{

/* Line 293 of yacc.c  */
#line 70 "fortran.y"

    char        nac[LONG_C];
    char        na[LONGNOM];
    listdim     *d;
    listvar     *l;
    listcouple  *lc;
    listname    *lnn;
    typedim     dim1;
    variable    *v;



/* Line 293 of yacc.c  */
#line 308 "fortran.tab.c"
} YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define yystype YYSTYPE /* obsolescent; will be withdrawn */
# define YYSTYPE_IS_DECLARED 1
#endif


/* Copy the second part of user declarations.  */


/* Line 343 of yacc.c  */
#line 320 "fortran.tab.c"

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#elif (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
typedef signed char yytype_int8;
#else
typedef short int yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(msgid) dgettext ("bison-runtime", msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(msgid) msgid
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(e) ((void) (e))
#else
# define YYUSE(e) /* empty */
#endif

/* Identity function, used to suppress warnings about constant conditions.  */
#ifndef lint
# define YYID(n) (n)
#else
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static int
YYID (int yyi)
#else
static int
YYID (yyi)
    int yyi;
#endif
{
  return yyi;
}
#endif

#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's `empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (YYID (0))
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
	     && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS && (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
	 || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)				\
    do									\
      {									\
	YYSIZE_T yynewbytes;						\
	YYCOPY (&yyptr->Stack_alloc, Stack, yysize);			\
	Stack = &yyptr->Stack_alloc;					\
	yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
	yyptr += yynewbytes / sizeof (*yyptr);				\
      }									\
    while (YYID (0))

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from FROM to TO.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(To, From, Count) \
      __builtin_memcpy (To, From, (Count) * sizeof (*(From)))
#  else
#   define YYCOPY(To, From, Count)		\
      do					\
	{					\
	  YYSIZE_T yyi;				\
	  for (yyi = 0; yyi < (Count); yyi++)	\
	    (To)[yyi] = (From)[yyi];		\
	}					\
      while (YYID (0))
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  2
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   5282

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  159
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  164
/* YYNRULES -- Number of rules.  */
#define YYNRULES  520
/* YYNRULES -- Number of states.  */
#define YYNSTATES  939

/* YYTRANSLATE(YYLEX) -- Bison symbol number corresponding to YYLEX.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   400

#define YYTRANSLATE(YYX)						\
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[YYLEX] -- Bison symbol number corresponding to YYLEX.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
     156,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,   158,     2,     2,
     152,   153,    21,    19,     3,    20,     2,   157,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     4,     2,
     154,     5,   155,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    17,
      18,    22,    23,    24,    25,    26,    27,    28,    29,    30,
      31,    32,    33,    34,    35,    36,    37,    38,    39,    40,
      41,    42,    43,    44,    45,    46,    47,    48,    49,    50,
      51,    52,    53,    54,    55,    56,    57,    58,    59,    60,
      61,    62,    63,    64,    65,    66,    67,    68,    69,    70,
      71,    72,    73,    74,    75,    76,    77,    78,    79,    80,
      81,    82,    83,    84,    85,    86,    87,    88,    89,    90,
      91,    92,    93,    94,    95,    96,    97,    98,    99,   100,
     101,   102,   103,   104,   105,   106,   107,   108,   109,   110,
     111,   112,   113,   114,   115,   116,   117,   118,   119,   120,
     121,   122,   123,   124,   125,   126,   127,   128,   129,   130,
     131,   132,   133,   134,   135,   136,   137,   138,   139,   140,
     141,   142,   143,   144,   145,   146,   147,   148,   149,   150,
     151
};

#if YYDEBUG
/* YYPRHS[YYN] -- Index of the first RHS symbol of rule number YYN in
   YYRHS.  */
static const yytype_uint16 yyprhs[] =
{
       0,     0,     3,     4,     7,    10,    12,    15,    17,    19,
      23,    26,    29,    33,    35,    39,    40,    41,    43,    48,
      51,    57,    61,    64,    66,    68,    69,    72,    76,    77,
      80,    84,    86,    90,    92,    94,    97,   102,   105,   108,
     113,   116,   118,   120,   122,   124,   126,   128,   130,   132,
     134,   139,   143,   147,   150,   154,   155,   157,   159,   161,
     163,   165,   167,   169,   171,   173,   175,   177,   179,   181,
     183,   185,   187,   189,   191,   193,   195,   197,   199,   201,
     203,   205,   207,   211,   215,   221,   223,   227,   231,   234,
     239,   241,   245,   246,   249,   252,   256,   258,   260,   262,
     267,   274,   279,   281,   285,   288,   292,   298,   302,   304,
     305,   308,   310,   315,   319,   322,   326,   330,   334,   338,
     339,   341,   344,   348,   354,   358,   360,   366,   372,   375,
     379,   382,   386,   388,   392,   395,   399,   405,   407,   410,
     412,   416,   419,   421,   425,   426,   428,   430,   434,   438,
     441,   443,   447,   450,   453,   459,   466,   467,   470,   473,
     477,   481,   482,   485,   490,   494,   498,   503,   506,   508,
     510,   512,   514,   516,   518,   520,   521,   524,   526,   530,
     531,   534,   538,   540,   544,   547,   551,   553,   555,   557,
     559,   560,   564,   565,   568,   573,   575,   579,   581,   583,
     585,   588,   590,   595,   597,   599,   601,   603,   605,   607,
     609,   611,   613,   615,   616,   620,   622,   626,   628,   630,
     633,   636,   640,   642,   644,   646,   648,   650,   654,   658,
     662,   667,   672,   676,   681,   686,   690,   695,   700,   705,
     710,   715,   720,   725,   730,   735,   740,   745,   750,   755,
     759,   764,   769,   774,   779,   781,   785,   787,   789,   791,
     794,   797,   800,   802,   804,   807,   810,   813,   816,   819,
     822,   825,   828,   831,   834,   837,   841,   844,   848,   851,
     854,   857,   860,   863,   866,   869,   870,   872,   875,   878,
     881,   883,   885,   887,   889,   890,   892,   895,   900,   906,
     911,   915,   919,   921,   924,   926,   930,   932,   934,   938,
     944,   949,   953,   956,   959,   961,   963,   965,   967,   969,
     971,   974,   977,   979,   982,   984,   986,   987,   989,   995,
     996,   998,  1000,  1002,  1003,  1007,  1008,  1014,  1017,  1022,
    1029,  1036,  1038,  1040,  1044,  1048,  1050,  1054,  1058,  1060,
    1062,  1068,  1074,  1079,  1081,  1084,  1087,  1090,  1093,  1095,
    1098,  1104,  1106,  1108,  1111,  1117,  1119,  1122,  1126,  1131,
    1133,  1135,  1137,  1139,  1141,  1143,  1145,  1147,  1151,  1155,
    1159,  1162,  1165,  1166,  1172,  1180,  1181,  1184,  1186,  1188,
    1189,  1191,  1193,  1196,  1198,  1200,  1202,  1208,  1214,  1217,
    1220,  1223,  1226,  1228,  1229,  1234,  1241,  1243,  1247,  1250,
    1253,  1256,  1257,  1261,  1262,  1264,  1267,  1269,  1271,  1275,
    1277,  1280,  1282,  1284,  1287,  1290,  1293,  1297,  1300,  1302,
    1303,  1305,  1308,  1311,  1312,  1315,  1319,  1323,  1327,  1331,
    1333,  1337,  1339,  1341,  1345,  1347,  1349,  1351,  1354,  1359,
    1364,  1367,  1370,  1372,  1374,  1376,  1378,  1380,  1382,  1384,
    1386,  1388,  1392,  1394,  1396,  1400,  1404,  1408,  1412,  1415,
    1419,  1422,  1425,  1428,  1431,  1435,  1437,  1439,  1441,  1445,
    1446,  1448,  1451,  1456,  1459,  1462,  1468,  1469,  1471,  1474,
    1476,  1478,  1480,  1482,  1486,  1490,  1494,  1498,  1502,  1506,
    1512,  1518,  1524,  1530,  1538,  1547,  1550,  1552,  1554,  1558,
    1560,  1562,  1564,  1569,  1571,  1575,  1576,  1581,  1583,  1587,
    1591
};

/* YYRHS -- A `-1'-separated list of the rules' RHS.  */
static const yytype_int16 yyrhs[] =
{
     160,     0,    -1,    -1,   160,   161,    -1,   156,   164,    -1,
     162,    -1,   130,   162,    -1,     1,    -1,   163,    -1,   162,
      24,   163,    -1,   166,   164,    -1,   173,   164,    -1,    28,
     168,   164,    -1,   269,    -1,   253,     4,   164,    -1,    -1,
      -1,    64,    -1,   165,    29,   167,   169,    -1,    30,   167,
      -1,    31,   167,   169,    26,   170,    -1,    31,   167,   169,
      -1,    86,   150,    -1,   150,    -1,   143,    -1,    -1,   152,
     153,    -1,   152,   171,   153,    -1,    -1,   152,   153,    -1,
     152,   171,   153,    -1,   172,    -1,   171,     3,   172,    -1,
     150,    -1,    21,    -1,   214,   183,    -1,   131,   174,   182,
     278,    -1,   133,   278,    -1,   107,   177,    -1,   185,   152,
     207,   153,    -1,   185,   207,    -1,   201,    -1,   189,    -1,
     211,    -1,   197,    -1,   199,    -1,   198,    -1,   263,    -1,
     209,    -1,   195,    -1,    60,   152,   235,   153,    -1,   101,
     182,   200,    -1,   103,   182,   176,    -1,    59,   178,    -1,
     186,   187,   156,    -1,    -1,   230,    -1,    40,    -1,    34,
      -1,    38,    -1,    48,    -1,    71,    -1,    39,    -1,    41,
      -1,    54,    -1,    49,    -1,    50,    -1,    51,    -1,    52,
      -1,    53,    -1,    56,    -1,    57,    -1,    58,    -1,    61,
      -1,    62,    -1,    63,    -1,    66,    -1,    67,    -1,    68,
      -1,    69,    -1,   150,    -1,   175,    -1,   176,     3,   175,
      -1,   152,   181,   153,    -1,   177,     3,   152,   181,   153,
      -1,   179,    -1,   178,     3,   179,    -1,   152,   180,   153,
      -1,   253,   231,    -1,   180,     3,   253,   231,    -1,   235,
      -1,   181,     3,   235,    -1,    -1,     4,     4,    -1,   212,
     213,    -1,   184,   167,   169,    -1,    31,    -1,    25,    -1,
     145,    -1,   150,    22,   188,    22,    -1,   187,   206,   150,
      22,   188,    22,    -1,   192,    22,   188,    22,    -1,   193,
      -1,   193,     3,   188,    -1,   190,   191,    -1,   190,   205,
     191,    -1,   189,   206,   205,   206,   191,    -1,   189,     3,
     191,    -1,   109,    -1,    -1,   150,   231,    -1,   150,    -1,
     150,   152,   235,   153,    -1,   192,     3,   192,    -1,   194,
     254,    -1,   193,    19,   193,    -1,   193,    20,   193,    -1,
     193,    21,   193,    -1,   193,   157,   193,    -1,    -1,   239,
      -1,   104,   253,    -1,   104,   205,   253,    -1,   195,   206,
     205,   206,   253,    -1,   195,     3,   253,    -1,    99,    -1,
     196,   206,   150,   231,   219,    -1,   197,     3,   150,   231,
     219,    -1,    73,   156,    -1,    73,   182,   200,    -1,    72,
     156,    -1,    72,   182,   200,    -1,   150,    -1,   200,     3,
     150,    -1,   202,   203,    -1,   202,   205,   203,    -1,   201,
     206,   205,   206,   203,    -1,   116,    -1,   117,   116,    -1,
     204,    -1,   203,     3,   204,    -1,   150,   231,    -1,    18,
      -1,    22,   150,    22,    -1,    -1,     3,    -1,   208,    -1,
     207,     3,   208,    -1,   150,     5,   235,    -1,   121,   210,
      -1,   150,    -1,   210,     3,   150,    -1,   111,   112,    -1,
     111,   123,    -1,   226,   150,   231,   219,   260,    -1,   212,
       3,   150,   231,   219,   260,    -1,    -1,   218,   221,    -1,
     217,   215,    -1,   218,    21,   151,    -1,   132,   223,   153,
      -1,    -1,    21,   151,    -1,    21,   152,   216,   153,    -1,
     152,   216,   153,    -1,   150,   224,   225,    -1,   150,     5,
     224,   225,    -1,   224,   225,    -1,   142,    -1,   135,    -1,
     136,    -1,   134,    -1,   148,    -1,   149,    -1,   137,    -1,
      -1,    21,   220,    -1,   235,    -1,   152,    21,   153,    -1,
      -1,    21,   222,    -1,   152,   223,   153,    -1,   235,    -1,
     152,    21,   153,    -1,   150,   224,    -1,   150,     5,   224,
      -1,   150,    -1,   151,    -1,   235,    -1,    21,    -1,    -1,
       3,   150,   224,    -1,    -1,     4,     4,    -1,     3,   227,
       4,     4,    -1,   228,    -1,   227,     3,   228,    -1,    25,
      -1,   230,    -1,    74,    -1,    99,   231,    -1,   101,    -1,
     102,   152,   229,   153,    -1,   103,    -1,   106,    -1,   107,
      -1,   109,    -1,   110,    -1,   126,    -1,   124,    -1,   125,
      -1,    72,    -1,    73,    -1,    -1,   152,   232,   153,    -1,
     233,    -1,   232,     3,   233,    -1,   234,    -1,     4,    -1,
     235,     4,    -1,     4,   235,    -1,   235,     4,   234,    -1,
      21,    -1,   235,    -1,   238,    -1,   262,    -1,   236,    -1,
     152,   235,   153,    -1,    40,   237,   153,    -1,    33,   237,
     153,    -1,    34,   152,   237,   153,    -1,    38,   152,   237,
     153,    -1,    48,   237,   153,    -1,    71,   152,   237,   153,
      -1,    39,   152,   235,   153,    -1,    41,   235,   153,    -1,
     134,   152,   237,   153,    -1,    54,   152,   235,   153,    -1,
      49,   152,   235,   153,    -1,    50,   152,   235,   153,    -1,
      51,   152,   235,   153,    -1,    52,   152,   235,   153,    -1,
      53,   152,   235,   153,    -1,    56,   152,   235,   153,    -1,
      57,   152,   235,   153,    -1,    58,   152,   235,   153,    -1,
      61,   152,   235,   153,    -1,    62,   152,   235,   153,    -1,
      63,   152,   235,   153,    -1,    65,   235,   153,    -1,    66,
     152,   237,   153,    -1,    67,   152,   237,   153,    -1,    68,
     152,   237,   153,    -1,    69,   152,   237,   153,    -1,   235,
      -1,   237,     3,   235,    -1,   243,    -1,   254,    -1,   248,
      -1,   235,   240,    -1,   239,   235,    -1,    11,   235,    -1,
      19,    -1,    20,    -1,    19,   235,    -1,    20,   235,    -1,
      21,   235,    -1,    23,   235,    -1,    13,   235,    -1,     7,
     235,    -1,    16,   235,    -1,   155,   235,    -1,    17,   235,
      -1,   154,   235,    -1,    14,   235,    -1,   155,     5,   235,
      -1,    15,   235,    -1,   154,     5,   235,    -1,    12,   235,
      -1,     6,   235,    -1,     8,   235,    -1,     9,   235,    -1,
      10,   235,    -1,    22,   241,    -1,     5,   242,    -1,    -1,
     235,    -1,     5,   235,    -1,    22,   235,    -1,     5,   235,
      -1,   235,    -1,   253,    -1,   247,    -1,   245,    -1,    -1,
     246,    -1,   246,   257,    -1,   247,   152,   249,   153,    -1,
     247,   152,   249,   153,   257,    -1,   253,   152,   249,   153,
      -1,   243,   158,   243,    -1,   118,   311,   119,    -1,   244,
      -1,   244,   250,    -1,   251,    -1,   250,     3,   251,    -1,
     235,    -1,   252,    -1,   235,     4,   235,    -1,   235,     4,
     235,     4,   235,    -1,     4,   235,     4,   235,    -1,     4,
       4,   235,    -1,     4,   235,    -1,   235,     4,    -1,     4,
      -1,   150,    -1,   128,    -1,   129,    -1,   151,    -1,   147,
      -1,   254,   150,    -1,   255,   256,    -1,   143,    -1,   255,
     143,    -1,   146,    -1,   144,    -1,    -1,   257,    -1,   152,
     258,     4,   258,   153,    -1,    -1,   235,    -1,   156,    -1,
     235,    -1,    -1,   261,     5,   235,    -1,    -1,   152,   238,
       3,   238,   153,    -1,   264,   150,    -1,   264,   150,     3,
     265,    -1,   264,   150,     3,    27,     4,   156,    -1,   264,
     150,     3,    27,     4,   267,    -1,   127,    -1,   266,    -1,
     265,     3,   266,    -1,   150,   115,   150,    -1,   268,    -1,
     267,     3,   268,    -1,   150,   115,   150,    -1,   150,    -1,
     280,    -1,    89,   152,   315,   319,   153,    -1,    97,   152,
     318,   319,   153,    -1,    98,   152,   320,   153,    -1,   271,
      -1,   272,   278,    -1,   270,   278,    -1,   273,   278,    -1,
      87,   278,    -1,   275,    -1,   321,   280,    -1,    35,   152,
     235,   153,   259,    -1,    36,    -1,    37,    -1,   321,    76,
      -1,    77,   152,   235,   153,    76,    -1,    78,    -1,    79,
     278,    -1,    42,   274,   153,    -1,    43,   152,   235,   153,
      -1,   105,    -1,   100,    -1,    84,    -1,   138,    -1,   141,
      -1,   140,    -1,   139,    -1,   235,    -1,   274,     3,   235,
      -1,   274,     4,   235,    -1,    83,   277,   276,    -1,    88,
     235,    -1,    85,   279,    -1,    -1,   322,     5,   235,     3,
     235,    -1,   322,     5,   235,     3,   235,     3,   235,    -1,
      -1,   151,   206,    -1,   156,    -1,   150,    -1,    -1,   150,
      -1,   108,    -1,   282,   284,    -1,   314,    -1,   293,    -1,
     285,    -1,    89,   152,   315,   319,   153,    -1,    97,   152,
     318,   319,   153,    -1,    70,   258,    -1,    75,   259,    -1,
      55,   259,    -1,   292,   259,    -1,   283,    -1,    -1,   253,
     281,   231,   231,    -1,   282,   158,   253,   281,   231,   231,
      -1,   151,    -1,   283,     3,   151,    -1,     5,   235,    -1,
     115,   235,    -1,   288,   286,    -1,    -1,   152,   287,   153,
      -1,    -1,   290,    -1,   289,   150,    -1,   113,    -1,   291,
      -1,   290,     3,   291,    -1,   235,    -1,    21,   151,    -1,
     120,    -1,   122,    -1,   301,   297,    -1,   303,   295,    -1,
      93,   297,    -1,    93,   297,   311,    -1,    96,   298,    -1,
      32,    -1,    -1,   307,    -1,   297,   294,    -1,   302,   296,
      -1,    -1,     3,   307,    -1,   152,   299,   153,    -1,   152,
     304,   153,    -1,   152,   253,   153,    -1,   152,   151,   153,
      -1,   151,    -1,   152,   238,   153,    -1,   150,    -1,   300,
      -1,   299,     3,   300,    -1,   304,    -1,    21,    -1,    23,
      -1,   253,   235,    -1,   253,   235,   158,   282,    -1,   253,
     152,   252,   153,    -1,   253,    21,    -1,   253,    23,    -1,
      90,    -1,    91,    -1,    94,    -1,   305,    -1,    21,    -1,
      95,    -1,    92,    -1,    80,    -1,   305,    -1,   152,   304,
     153,    -1,   243,    -1,   254,    -1,   304,   306,   304,    -1,
     304,    21,   304,    -1,   304,    22,   304,    -1,   304,    23,
     304,    -1,   306,   304,    -1,   304,    18,   304,    -1,    44,
     235,    -1,    47,   235,    -1,    46,   235,    -1,    45,   235,
      -1,   150,     5,   235,    -1,    19,    -1,    20,    -1,   309,
      -1,   307,     3,   309,    -1,    -1,   243,    -1,   308,   310,
      -1,   152,   307,   153,   310,    -1,   236,   310,    -1,   254,
     310,    -1,   152,   307,     3,   313,   153,    -1,    -1,   240,
      -1,   310,   240,    -1,   262,    -1,   236,    -1,   238,    -1,
     312,    -1,   238,     3,   235,    -1,   238,     3,   312,    -1,
     312,     3,   235,    -1,   312,     3,   312,    -1,   311,     3,
     235,    -1,   311,     3,   312,    -1,   152,   238,     3,   313,
     153,    -1,   152,   311,     3,   313,   153,    -1,   152,   312,
       3,   313,   153,    -1,   150,     5,   235,     3,   235,    -1,
     150,     5,   235,     3,   235,     3,   235,    -1,    81,   152,
     235,     3,   235,   153,     3,   235,    -1,    81,   151,    -1,
     316,    -1,   282,    -1,   315,     3,   316,    -1,   253,    -1,
     247,    -1,   317,    -1,   253,   152,   250,   153,    -1,   316,
      -1,   318,     3,   316,    -1,    -1,     3,   114,     5,   253,
      -1,   253,    -1,   320,     3,   253,    -1,    82,   235,   153,
      -1,   253,    -1
};

/* YYRLINE[YYN] -- source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   310,   310,   311,   313,   314,   315,   316,   319,   320,
     323,   324,   325,   333,   334,   336,   338,   339,   341,   369,
     387,   420,   454,   471,   473,   475,   476,   477,   479,   480,
     481,   483,   495,   507,   508,   510,   518,   526,   532,   533,
     544,   555,   556,   561,   562,   594,   607,   608,   609,   610,
     611,   612,   613,   614,   615,   633,   634,   641,   642,   643,
     644,   645,   646,   647,   648,   649,   650,   651,   652,   653,
     654,   655,   656,   657,   658,   659,   660,   661,   662,   663,
     664,   666,   667,   669,   670,   672,   673,   675,   677,   678,
     680,   681,   683,   684,   687,   747,   783,   785,   787,   789,
     798,   807,   814,   815,   818,   819,   820,   821,   823,   825,
     826,   829,   830,   831,   834,   835,   836,   837,   838,   840,
     841,   844,   845,   846,   847,   850,   857,   868,   881,   882,
     885,   886,   889,   890,   893,   898,   908,   920,   921,   924,
     925,   927,   938,   947,   957,   958,   960,   964,   969,   992,
     994,   995,   997,  1006,  1008,  1030,  1055,  1057,  1058,  1059,
    1060,  1062,  1063,  1064,  1065,  1067,  1068,  1069,  1071,  1074,
    1075,  1076,  1077,  1078,  1079,  1081,  1082,  1084,  1085,  1087,
    1088,  1089,  1091,  1092,  1095,  1096,  1106,  1107,  1110,  1111,
    1113,  1114,  1116,  1117,  1118,  1120,  1121,  1124,  1125,  1126,
    1127,  1128,  1129,  1131,  1132,  1133,  1134,  1135,  1138,  1139,
    1140,  1143,  1144,  1146,  1147,  1150,  1151,  1153,  1154,  1155,
    1156,  1157,  1160,  1161,  1163,  1164,  1165,  1166,  1170,  1171,
    1172,  1173,  1174,  1175,  1176,  1177,  1178,  1179,  1180,  1181,
    1182,  1183,  1184,  1185,  1186,  1187,  1188,  1189,  1190,  1191,
    1192,  1193,  1194,  1195,  1197,  1198,  1200,  1201,  1202,  1203,
    1204,  1205,  1207,  1208,  1211,  1213,  1215,  1217,  1219,  1221,
    1223,  1225,  1227,  1229,  1231,  1233,  1235,  1237,  1239,  1241,
    1243,  1245,  1247,  1249,  1251,  1254,  1255,  1257,  1259,  1262,
    1264,  1268,  1269,  1270,  1274,  1285,  1286,  1287,  1288,  1291,
    1302,  1309,  1312,  1313,  1316,  1317,  1320,  1321,  1324,  1325,
    1326,  1327,  1328,  1329,  1330,  1332,  1373,  1375,  1376,  1377,
    1378,  1380,  1382,  1384,  1385,  1387,  1390,  1391,  1393,  1396,
    1397,  1399,  1400,  1402,  1403,  1412,  1415,  1417,  1437,  1471,
    1491,  1542,  1548,  1552,  1562,  1574,  1575,  1586,  1598,  1607,
    1608,  1614,  1620,  1621,  1671,  1689,  1709,  1728,  1761,  1762,
    1763,  1764,  1765,  1766,  1767,  1768,  1769,  1770,  1771,  1772,
    1773,  1774,  1811,  1821,  1830,  1839,  1848,  1849,  1850,  1853,
    1854,  1855,  1857,  1858,  1859,  1860,  1861,  1863,  1864,  1866,
    1867,  1871,  1872,  1873,  1874,  1875,  1876,  1881,  1886,  1887,
    1888,  1889,  1890,  1892,  1894,  1904,  1909,  1910,  1913,  1914,
    1916,  1944,  1945,  1947,  1948,  1950,  1980,  1982,  1983,  1987,
    1995,  1998,  1999,  2002,  2003,  2004,  2005,  2006,  2007,  2009,
    2010,  2013,  2014,  2016,  2017,  2019,  2020,  2023,  2024,  2025,
    2026,  2027,  2030,  2031,  2034,  2035,  2036,  2037,  2038,  2039,
    2040,  2041,  2044,  2045,  2046,  2048,  2049,  2052,  2053,  2054,
    2057,  2058,  2060,  2061,  2062,  2063,  2064,  2065,  2066,  2067,
    2068,  2069,  2070,  2071,  2072,  2074,  2075,  2077,  2078,  2080,
    2081,  2083,  2084,  2085,  2086,  2087,  2089,  2090,  2091,  2094,
    2095,  2096,  2097,  2098,  2099,  2100,  2101,  2102,  2103,  2106,
    2107,  2108,  2110,  2111,  2113,  2114,  2116,  2117,  2118,  2120,
    2122,  2123,  2125,  2128,  2129,  2131,  2132,  2134,  2135,  2137,
    2139
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || YYTOKEN_TABLE
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "','", "':'", "'='", "TOK_NEQV",
  "TOK_EQV", "TOK_XOR", "TOK_OR", "TOK_AND", "TOK_NOT", "TOK_NE", "TOK_EQ",
  "TOK_GE", "TOK_LE", "TOK_GT", "TOK_LT", "TOK_DSLASH", "'+'", "'-'",
  "'*'", "TOK_SLASH", "TOK_DASTER", "TOK_SEMICOLON", "TOK_PARAMETER",
  "TOK_RESULT", "TOK_ONLY", "TOK_INCLUDE", "TOK_SUBROUTINE", "TOK_PROGRAM",
  "TOK_FUNCTION", "TOK_FORMAT", "TOK_MAX", "TOK_TANH", "TOK_WHERE",
  "TOK_ELSEWHERE", "TOK_ENDWHERE", "TOK_MAXVAL", "TOK_TRIM", "TOK_SUM",
  "TOK_SQRT", "TOK_CASE", "TOK_SELECTCASE", "TOK_FILE", "TOK_END",
  "TOK_ERR", "TOK_EXIST", "TOK_MIN", "TOK_FLOAT", "TOK_EXP", "TOK_COS",
  "TOK_COSH", "TOK_ACOS", "TOK_NINT", "TOK_CYCLE", "TOK_SIN", "TOK_SINH",
  "TOK_ASIN", "TOK_EQUIVALENCE", "TOK_BACKSPACE", "TOK_LOG", "TOK_TAN",
  "TOK_ATAN", "TOK_RECURSIVE", "TOK_ABS", "TOK_MOD", "TOK_SIGN",
  "TOK_MINLOC", "TOK_MAXLOC", "TOK_EXIT", "TOK_MINVAL", "TOK_PUBLIC",
  "TOK_PRIVATE", "TOK_ALLOCATABLE", "TOK_RETURN", "TOK_THEN", "TOK_ELSEIF",
  "TOK_ELSE", "TOK_ENDIF", "TOK_PRINT", "TOK_PLAINGOTO", "TOK_LOGICALIF",
  "TOK_PLAINDO", "TOK_CONTAINS", "TOK_ENDDO", "TOK_MODULE",
  "TOK_ENDMODULE", "TOK_DOWHILE", "TOK_ALLOCATE", "TOK_OPEN", "TOK_CLOSE",
  "TOK_INQUIRE", "TOK_WRITE", "TOK_FLUSH", "TOK_READ", "TOK_REWIND",
  "TOK_DEALLOCATE", "TOK_NULLIFY", "TOK_DIMENSION", "TOK_ENDSELECT",
  "TOK_EXTERNAL", "TOK_INTENT", "TOK_INTRINSIC", "TOK_NAMELIST",
  "TOK_CASEDEFAULT", "TOK_OPTIONAL", "TOK_POINTER", "TOK_CONTINUE",
  "TOK_SAVE", "TOK_TARGET", "TOK_IMPLICIT", "TOK_NONE", "TOK_CALL",
  "TOK_STAT", "TOK_POINT_TO", "TOK_COMMON", "TOK_GLOBAL", "TOK_LEFTAB",
  "TOK_RIGHTAB", "TOK_PAUSE", "TOK_PROCEDURE", "TOK_STOP", "TOK_REAL8",
  "TOK_OUT", "TOK_INOUT", "TOK_IN", "TOK_USE", "TOK_TRUE", "TOK_FALSE",
  "TOK_LABEL", "TOK_TYPE", "TOK_TYPEPAR", "TOK_ENDTYPE", "TOK_REAL",
  "TOK_INTEGER", "TOK_LOGICAL", "TOK_DOUBLEPRECISION", "TOK_ENDSUBROUTINE",
  "TOK_ENDFUNCTION", "TOK_ENDPROGRAM", "TOK_ENDUNIT", "TOK_CHARACTER",
  "TOK_CHAR_CONSTANT", "TOK_CHAR_CUT", "TOK_DATA", "TOK_CHAR_MESSAGE",
  "TOK_CSTREAL", "TOK_COMPLEX", "TOK_DOUBLECOMPLEX", "TOK_NAME",
  "TOK_CSTINT", "'('", "')'", "'<'", "'>'", "'\\n'", "'/'", "'%'",
  "$accept", "input", "line", "suite_line_list", "suite_line", "fin_line",
  "opt_recursive", "entry", "name_routine", "filename", "arglist",
  "arglist1", "args", "arg", "spec", "opt_spec", "name_intrinsic",
  "use_intrinsic_list", "list_couple", "list_expr_equi", "expr_equi",
  "list_expr_equi1", "list_expr", "opt_sep", "after_type",
  "before_function", "before_parameter", "before_data", "data",
  "datavallist", "save", "before_save", "varsave", "datanamelist",
  "expr_data", "opt_signe", "namelist", "before_dimension", "dimension",
  "private", "public", "use_name_list", "common", "before_common",
  "var_common_list", "var_common", "comblock", "opt_comma", "paramlist",
  "paramitem", "module_proc_stmt", "proc_name_list", "implicit", "dcl",
  "nodimsgiven", "type", "c_selector", "c_attribute", "before_character",
  "typespec", "lengspec", "proper_lengspec", "selector", "proper_selector",
  "attribute", "clause", "opt_clause", "options", "attr_spec_list",
  "attr_spec", "intent_spec", "access_spec", "dims", "dimlist", "dim",
  "ubound", "expr", "predefinedfunction", "minmaxlist", "uexpr", "signe",
  "operation", "after_slash", "after_equal", "lhs", "beforefunctionuse",
  "array_ele_substring_func_ref", "begin_array", "structure_component",
  "vec", "funarglist", "funargs", "funarg", "triplet", "ident",
  "simple_const", "string_constant", "opt_substring", "substring",
  "optexpr", "opt_expr", "initial_value", "before_initial",
  "complex_const", "use_stat", "word_use", "rename_list", "rename_name",
  "only_list", "only_name", "exec", "word_endsubroutine", "word_endunit",
  "word_endprogram", "word_endfunction", "caselist", "boucledo", "do_arg",
  "opt_int", "opt_name", "optname", "iffable", "before_dims", "ident_dims",
  "int_list", "after_ident_dims", "call", "opt_call", "opt_callarglist",
  "keywordcall", "before_call", "callarglist", "callarg", "stop", "io",
  "option_inlist", "option_read", "opt_inlist", "ioctl", "after_rewind",
  "ctllist", "ioclause", "iofctl", "infmt", "read", "fexpr", "unpar_fexpr",
  "addop", "inlist", "opt_lhs", "inelt", "opt_operation", "outlist",
  "other", "dospec", "goto", "allocation_list", "allocate_object",
  "array_element", "allocate_object_list", "opt_stat_spec",
  "pointer_name_list", "logif", "do_var", 0
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[YYLEX-NUM] -- Internal token number corresponding to
   token YYLEX-NUM.  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,    44,    58,    61,   258,   259,   260,   261,
     262,   263,   264,   265,   266,   267,   268,   269,   270,    43,
      45,    42,   271,   272,   273,   274,   275,   276,   277,   278,
     279,   280,   281,   282,   283,   284,   285,   286,   287,   288,
     289,   290,   291,   292,   293,   294,   295,   296,   297,   298,
     299,   300,   301,   302,   303,   304,   305,   306,   307,   308,
     309,   310,   311,   312,   313,   314,   315,   316,   317,   318,
     319,   320,   321,   322,   323,   324,   325,   326,   327,   328,
     329,   330,   331,   332,   333,   334,   335,   336,   337,   338,
     339,   340,   341,   342,   343,   344,   345,   346,   347,   348,
     349,   350,   351,   352,   353,   354,   355,   356,   357,   358,
     359,   360,   361,   362,   363,   364,   365,   366,   367,   368,
     369,   370,   371,   372,   373,   374,   375,   376,   377,   378,
     379,   380,   381,   382,   383,   384,   385,   386,   387,   388,
     389,   390,   391,   392,   393,   394,   395,   396,   397,   398,
     399,   400,    40,    41,    60,    62,    10,    47,    37
};
# endif

/* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   159,   160,   160,   161,   161,   161,   161,   162,   162,
     163,   163,   163,   163,   163,   164,   165,   165,   166,   166,
     166,   166,   166,   167,   168,   169,   169,   169,   170,   170,
     170,   171,   171,   172,   172,   173,   173,   173,   173,   173,
     173,   173,   173,   173,   173,   173,   173,   173,   173,   173,
     173,   173,   173,   173,   173,   174,   174,   175,   175,   175,
     175,   175,   175,   175,   175,   175,   175,   175,   175,   175,
     175,   175,   175,   175,   175,   175,   175,   175,   175,   175,
     175,   176,   176,   177,   177,   178,   178,   179,   180,   180,
     181,   181,   182,   182,   183,   183,   184,   185,   186,   187,
     187,   187,   188,   188,   189,   189,   189,   189,   190,   191,
     191,   192,   192,   192,   193,   193,   193,   193,   193,   194,
     194,   195,   195,   195,   195,   196,   197,   197,   198,   198,
     199,   199,   200,   200,   201,   201,   201,   202,   202,   203,
     203,   204,   205,   205,   206,   206,   207,   207,   208,   209,
     210,   210,   211,   211,   212,   212,   213,   214,   214,   214,
     214,   215,   215,   215,   215,   216,   216,   216,   217,   218,
     218,   218,   218,   218,   218,   219,   219,   220,   220,   221,
     221,   221,   222,   222,   223,   223,   223,   223,   224,   224,
     225,   225,   226,   226,   226,   227,   227,   228,   228,   228,
     228,   228,   228,   228,   228,   228,   228,   228,   229,   229,
     229,   230,   230,   231,   231,   232,   232,   233,   233,   233,
     233,   233,   234,   234,   235,   235,   235,   235,   236,   236,
     236,   236,   236,   236,   236,   236,   236,   236,   236,   236,
     236,   236,   236,   236,   236,   236,   236,   236,   236,   236,
     236,   236,   236,   236,   237,   237,   238,   238,   238,   238,
     238,   238,   239,   239,   240,   240,   240,   240,   240,   240,
     240,   240,   240,   240,   240,   240,   240,   240,   240,   240,
     240,   240,   240,   240,   240,   241,   241,   241,   241,   242,
     242,   243,   243,   243,   244,   245,   245,   245,   245,   246,
     247,   248,   249,   249,   250,   250,   251,   251,   252,   252,
     252,   252,   252,   252,   252,   253,   254,   254,   254,   254,
     254,   254,   255,   255,   255,   255,   256,   256,   257,   258,
     258,   259,   259,   260,   260,   261,   262,   263,   263,   263,
     263,   264,   265,   265,   266,   267,   267,   268,   268,   269,
     269,   269,   269,   269,   269,   269,   269,   269,   269,   269,
     269,   269,   269,   269,   269,   269,   269,   269,   269,   269,
     269,   269,   270,   271,   272,   273,   274,   274,   274,   275,
     275,   275,   276,   276,   276,   277,   277,   278,   278,   279,
     279,   280,   280,   280,   280,   280,   280,   280,   280,   280,
     280,   280,   280,   281,   282,   282,   283,   283,   284,   284,
     285,   286,   286,   287,   287,   288,   289,   290,   290,   291,
     291,   292,   292,   293,   293,   293,   293,   293,   293,   294,
     294,   295,   295,   296,   296,   297,   297,   298,   298,   298,
     298,   298,   299,   299,   300,   300,   300,   300,   300,   300,
     300,   300,   301,   301,   301,   302,   302,   303,   303,   303,
     304,   304,   305,   305,   305,   305,   305,   305,   305,   305,
     305,   305,   305,   305,   305,   306,   306,   307,   307,   308,
     308,   309,   309,   309,   309,   309,   310,   310,   310,   311,
     311,   311,   311,   311,   311,   311,   311,   311,   311,   312,
     312,   312,   313,   313,   314,   314,   315,   315,   315,   316,
     316,   316,   317,   318,   318,   319,   319,   320,   320,   321,
     322
};

/* YYR2[YYN] -- Number of symbols composing right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     2,     1,     2,     1,     1,     3,
       2,     2,     3,     1,     3,     0,     0,     1,     4,     2,
       5,     3,     2,     1,     1,     0,     2,     3,     0,     2,
       3,     1,     3,     1,     1,     2,     4,     2,     2,     4,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       4,     3,     3,     2,     3,     0,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     3,     3,     5,     1,     3,     3,     2,     4,
       1,     3,     0,     2,     2,     3,     1,     1,     1,     4,
       6,     4,     1,     3,     2,     3,     5,     3,     1,     0,
       2,     1,     4,     3,     2,     3,     3,     3,     3,     0,
       1,     2,     3,     5,     3,     1,     5,     5,     2,     3,
       2,     3,     1,     3,     2,     3,     5,     1,     2,     1,
       3,     2,     1,     3,     0,     1,     1,     3,     3,     2,
       1,     3,     2,     2,     5,     6,     0,     2,     2,     3,
       3,     0,     2,     4,     3,     3,     4,     2,     1,     1,
       1,     1,     1,     1,     1,     0,     2,     1,     3,     0,
       2,     3,     1,     3,     2,     3,     1,     1,     1,     1,
       0,     3,     0,     2,     4,     1,     3,     1,     1,     1,
       2,     1,     4,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     0,     3,     1,     3,     1,     1,     2,
       2,     3,     1,     1,     1,     1,     1,     3,     3,     3,
       4,     4,     3,     4,     4,     3,     4,     4,     4,     4,
       4,     4,     4,     4,     4,     4,     4,     4,     4,     3,
       4,     4,     4,     4,     1,     3,     1,     1,     1,     2,
       2,     2,     1,     1,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     3,     2,     3,     2,     2,
       2,     2,     2,     2,     2,     0,     1,     2,     2,     2,
       1,     1,     1,     1,     0,     1,     2,     4,     5,     4,
       3,     3,     1,     2,     1,     3,     1,     1,     3,     5,
       4,     3,     2,     2,     1,     1,     1,     1,     1,     1,
       2,     2,     1,     2,     1,     1,     0,     1,     5,     0,
       1,     1,     1,     0,     3,     0,     5,     2,     4,     6,
       6,     1,     1,     3,     3,     1,     3,     3,     1,     1,
       5,     5,     4,     1,     2,     2,     2,     2,     1,     2,
       5,     1,     1,     2,     5,     1,     2,     3,     4,     1,
       1,     1,     1,     1,     1,     1,     1,     3,     3,     3,
       2,     2,     0,     5,     7,     0,     2,     1,     1,     0,
       1,     1,     2,     1,     1,     1,     5,     5,     2,     2,
       2,     2,     1,     0,     4,     6,     1,     3,     2,     2,
       2,     0,     3,     0,     1,     2,     1,     1,     3,     1,
       2,     1,     1,     2,     2,     2,     3,     2,     1,     0,
       1,     2,     2,     0,     2,     3,     3,     3,     3,     1,
       3,     1,     1,     3,     1,     1,     1,     2,     4,     4,
       2,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     3,     1,     1,     3,     3,     3,     3,     2,     3,
       2,     2,     2,     2,     3,     1,     1,     1,     3,     0,
       1,     2,     4,     2,     2,     5,     0,     1,     2,     1,
       1,     1,     1,     3,     3,     3,     3,     3,     3,     5,
       5,     5,     5,     7,     8,     2,     1,     1,     3,     1,
       1,     1,     4,     1,     3,     0,     4,     1,     3,     3,
       1
};

/* YYDEFACT[STATE-NAME] -- Default reduction number in state STATE-NUM.
   Performed when YYTABLE doesn't specify something else to do.  Zero
   means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,     1,     7,    97,     0,     0,     0,   428,     0,
     361,   362,     0,     0,     0,     0,     0,    17,   329,    92,
      92,     0,     0,   365,     0,   459,     0,     0,   385,   371,
     389,     0,     0,     0,     0,   452,   453,   458,     0,   454,
     457,     0,     0,     0,   125,   370,    92,    92,     0,   369,
       0,   391,   108,     0,   416,   137,     0,   421,     0,   422,
     341,    16,    55,     0,     0,   171,   169,   170,   174,   372,
     375,   374,   373,   168,    98,   172,   173,   315,   406,    15,
       3,     5,     8,     0,    15,    15,     0,     0,    42,   109,
      49,   144,    44,    46,    45,    41,     0,    48,    43,   192,
     161,   179,   403,    47,     0,    13,     0,   353,     0,     0,
     358,   349,     0,   402,   395,   411,     0,     0,   394,     0,
       0,   393,     0,    24,    15,    23,    19,    25,     0,     0,
     262,   263,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   316,   317,
       0,   322,   325,   324,   319,   318,     0,   376,   226,   224,
       0,   256,   293,   295,   292,   258,   291,   257,   326,   225,
       0,     0,   331,   332,   400,     0,    53,    85,     0,   330,
     398,     0,   130,     0,   128,     0,   399,     0,   388,   387,
     366,   505,     0,     0,   144,   382,   390,   381,    22,   357,
     380,     0,     0,   425,   441,   439,     0,   427,     0,     0,
       0,     0,   142,     0,     0,   121,     0,    38,   152,   153,
     138,   150,   149,     6,   211,   212,    92,    56,   186,   187,
       0,    37,     4,    16,     0,    10,    11,     0,     0,    40,
     146,   111,   144,     0,   109,     0,   213,   104,   109,   145,
       0,   145,     0,     0,     0,   213,   134,   139,     0,     0,
       0,    96,    35,     0,   156,     0,     0,     0,   158,     0,
       0,   157,    15,   213,   337,   355,   354,   356,     0,     0,
       0,   392,     0,   413,   410,   415,   401,   423,   475,   476,
     456,     0,     0,     0,     0,   315,     0,   462,   463,   424,
     429,   433,     0,   455,     0,   363,     0,     0,   403,   359,
      12,     0,    21,     0,   261,   254,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   490,   491,   489,     0,   492,     0,     0,
     224,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   285,     0,     0,     0,
     259,   260,     0,   329,   296,   294,   294,   320,   323,   321,
     327,     0,     0,   367,     0,     0,   213,     0,     0,    93,
     132,   131,   129,     0,     0,   519,   386,   520,   379,     0,
       0,   510,   403,   507,   515,   506,   511,   445,   446,     0,
     291,     0,   442,   444,   460,   426,   318,   224,   291,   509,
     513,   515,   517,     0,    51,    58,    59,    62,    57,    63,
      60,    65,    66,    67,    68,    69,    64,    70,    71,    72,
      73,    74,    75,    76,    77,    78,    79,    61,    80,    81,
      52,     0,   122,     0,    90,     0,     0,     0,     0,   189,
     184,   188,   160,     9,    25,     0,     0,     0,   119,     0,
      54,     0,     0,   119,   107,   144,     0,   110,   105,   124,
     144,   213,   213,   144,   141,     0,   135,   197,   199,   213,
     201,     0,   203,   204,   205,   206,   207,     0,   195,   198,
     193,    25,     0,    94,   213,   162,     0,   315,     0,   190,
     318,     0,   180,   182,     0,    14,   213,     0,   408,   409,
     403,   407,     0,   419,     0,   414,   417,   470,   473,   472,
     471,     0,   444,   479,   486,   480,   486,   431,   430,   486,
     477,   479,   432,     0,     0,     0,     0,     0,   468,     0,
       0,    34,    33,    26,     0,    31,    28,     0,     0,   229,
       0,     0,     0,   228,   235,   232,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   249,     0,
       0,     0,     0,     0,   224,     0,     0,     0,     0,   301,
       0,     0,   227,     0,     0,   290,   284,   279,   269,   280,
     281,   282,   278,   268,   274,   276,   270,   272,   264,   265,
     266,     0,     0,   286,   283,   267,     0,   273,     0,   271,
     300,     0,   302,     0,     0,   377,   378,   368,     0,    87,
      88,    86,    50,     0,     0,     0,     0,   294,     0,     0,
       0,   450,   451,   294,   447,     0,   435,   436,   438,   440,
     437,     0,     0,     0,   352,     0,   143,     0,    83,     0,
     151,    36,   185,    18,   148,    39,   147,     0,   102,     0,
     120,     0,     0,   111,   113,     0,   109,   218,   222,     0,
     215,   217,   223,     0,   175,   175,     0,   140,   200,     0,
       0,     0,    95,   213,   175,     0,     0,   190,   164,     0,
     167,     0,   181,   404,     0,     0,   338,   342,   213,   420,
     412,     0,   474,   436,     0,   487,   483,   484,   479,   481,
     434,   469,   465,   466,   467,   464,   515,   515,     0,    27,
       0,    20,   360,   255,   230,   231,   234,   238,   239,   240,
     241,   242,   237,   243,   244,   245,   246,   247,   248,   250,
     251,   252,   253,   233,     0,     0,     0,   493,   494,   497,
     498,   495,   496,   236,   224,   289,   287,   288,   277,   275,
     329,   314,   306,   303,   304,   307,   297,   299,   213,   133,
     364,     0,     0,     0,     0,   508,   350,   461,     0,     0,
       0,   443,   444,   514,   351,   518,    82,    91,     0,    99,
     119,   119,   119,   119,   119,   114,   112,   119,   101,   106,
     220,     0,   214,   219,   123,     0,   126,   127,   136,   209,
     210,   208,     0,   196,   194,   175,   333,   163,   190,   165,
       0,   183,     0,     0,     0,   213,   418,   479,   486,   488,
     478,     0,     0,    32,    29,     0,   315,     0,     0,     0,
     336,     0,     0,   312,   313,     0,   298,    89,     0,     0,
     512,     0,   449,   448,    84,   103,   115,   116,   117,   118,
       0,   216,   221,   223,     0,   176,   177,   202,   333,   154,
       0,   166,   191,   348,   339,   340,   345,   344,   343,   405,
       0,   482,   396,   397,    30,     0,   499,   500,   501,   328,
     311,     0,   308,   305,     0,   383,   516,   100,     0,   155,
       0,     0,     0,   485,     0,   310,     0,   504,     0,   178,
     334,   347,   346,     0,   309,   384,   502,     0,   503
};

/* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,    80,    81,    82,   242,    83,    84,   126,   124,
     322,   741,   564,   565,    85,   236,   459,   460,   227,   186,
     187,   395,   463,   193,   272,   273,    86,    87,   252,   677,
      88,    89,   257,   253,   678,   679,    90,    91,    92,    93,
      94,   401,    95,    96,   266,   267,   224,   255,   249,   250,
      97,   232,    98,   274,   513,    99,   278,   518,   100,   101,
     826,   885,   281,   522,   240,   519,   710,   275,   507,   508,
     832,   509,   487,   689,   690,   691,   325,   168,   326,   169,
     170,   380,   624,   606,   171,   632,   172,   173,   174,   175,
     634,   783,   784,   785,   176,   177,   178,   389,   384,   190,
     184,   889,   890,   179,   103,   104,   716,   717,   895,   896,
     105,   106,   107,   108,   109,   180,   110,   408,   205,   200,
     207,   111,   283,   112,   113,   291,   114,   294,   534,   115,
     116,   535,   536,   117,   118,   547,   309,   552,   213,   217,
     421,   422,   119,   311,   120,   312,   424,   314,   548,   549,
     550,   726,   356,   357,   857,   121,   414,   415,   416,   431,
     649,   433,   122,   409
};

/* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
   STATE-NUM.  */
#define YYPACT_NINF -670
static const yytype_int16 yypact[] =
{
    -670,   903,  -670,  -670,  -670,   -79,   -56,   -56,  -670,   -46,
    -670,  -670,  4244,   -19,  3270,   -11,    16,  -670,  4244,    32,
      39,  3270,   103,  -670,   -22,  -670,   192,  4244,    45,  -670,
       5,    36,   -22,  4244,   123,  -670,  -670,  -670,   144,  -670,
    -670,   254,   154,   164,  -670,  -670,   225,   225,   134,  -670,
     168,  -670,  -670,   -14,  -670,  -670,   170,  -670,   187,  -670,
    -670,  4972,   275,   227,   -22,  -670,  -670,  -670,  -670,  -670,
    -670,  -670,  -670,  -670,  -670,  -670,  -670,  -670,  -670,  -670,
    -670,   317,  -670,   339,  -670,  -670,   188,   251,   475,   155,
     488,   413,   423,  -670,  -670,   493,   157,  -670,  -670,   128,
      67,   106,   439,  -670,   298,  -670,   -22,  -670,   -22,   -22,
    -670,  -670,    25,   451,  -670,   314,   342,  3270,  -670,   144,
    2374,  -670,  4848,  -670,  -670,  -670,  -670,   351,  4244,  4244,
    -670,  -670,  4244,   356,   362,   366,  4244,  4244,  4244,   369,
     370,   374,   381,   382,   385,   386,   387,   389,   394,   396,
     397,  4244,   398,   400,   401,   402,   414,  4364,  -670,  -670,
     415,  -670,  -670,  -670,  -670,  -670,  4244,  2152,  -670,  -670,
    4244,   336,  -670,   416,   418,  -670,   426,   432,   -80,  -670,
      44,  4244,  -670,  2152,  -670,   433,   529,  -670,  4244,  2152,
    -670,   532,  -670,   440,  -670,   440,  -670,  4244,  -670,  -670,
    -670,  -670,  4244,  1273,   413,   433,  -670,  -670,  -670,  -670,
    2152,   433,  2254,  4364,  -670,  -670,  4484,  -670,   433,   433,
     440,  2590,  -670,   443,   433,  -670,  4244,   562,  -670,  -670,
    -670,  -670,   586,   317,  -670,  -670,   225,  -670,  2550,  -670,
     441,  -670,  -670,  4972,   -56,  -670,  -670,   590,   446,   601,
    -670,    79,    35,    90,   455,   135,   456,  -670,   455,   433,
     135,  -670,   457,   462,   135,   456,   612,  -670,   467,   470,
     615,  -670,  -670,   -56,   618,   472,   289,  3524,  -670,  4604,
     227,  -670,  -670,   456,   620,  -670,  -670,  -670,  4244,  4244,
     433,  -670,   479,  3644,  -670,  -670,  -670,  -670,  -670,  -670,
    -670,  4244,  4244,  4244,  4244,   626,  2254,   336,   432,  -670,
    1055,   629,   539,   580,   373,  -670,   484,   485,  -670,  -670,
    -670,   159,   613,  1359,   743,  2152,    65,  4244,  4244,  4244,
      68,  1424,    72,  4244,  4244,  4244,  4244,  4244,  4244,  4244,
    4244,  4244,  4244,  4244,  4244,  1486,  4244,  4244,  4244,  4244,
    4244,  4364,  2152,  1510,   705,  1529,    48,   635,  4244,  1552,
     637,  2910,  4244,  4244,  4244,  4244,  4244,  4244,  4244,  4244,
    4244,  4244,  4244,  4244,  4244,  4244,  2670,  4244,  3030,  3150,
    -670,   290,   433,  4244,  -670,  -670,  -670,  -670,  -670,  -670,
    -670,  4244,  4244,  -670,  1575,    77,   456,   -11,  1594,  -670,
    -670,   640,   640,  1613,   769,  -670,  -670,  -670,  -670,   639,
     336,   -16,   184,   487,   643,  -670,  -670,  -670,  -670,   373,
    3404,    84,  -670,   142,  -670,   644,   495,   496,   312,   184,
    -670,   647,  -670,    88,   640,  -670,  -670,  -670,  -670,  -670,
    -670,  -670,  -670,  -670,  -670,  -670,  -670,  -670,  -670,  -670,
    -670,  -670,  -670,  -670,  -670,  -670,  -670,  -670,  -670,  -670,
     648,   631,  -670,    89,  2152,   504,   508,   -22,  3764,  -670,
    -670,  2152,  -670,  -670,   351,  4244,    92,   446,   468,  4244,
    -670,   510,   511,   468,  -670,   413,  2190,  -670,  -670,  -670,
     413,   456,   456,   413,  -670,   467,   612,  -670,  -670,   456,
    -670,   512,  -670,  -670,  -670,  -670,  -670,   486,  -670,  -670,
    -670,   351,   513,  -670,   456,  -670,  3524,  2790,   509,   663,
     143,  3884,  -670,  2152,   515,  -670,   456,    34,  2152,  2152,
    -670,  -670,   531,  2152,   535,   686,  -670,  2152,  2152,  2152,
    2152,  4244,   279,  5091,  2152,   336,  1637,  -670,   687,  2152,
    -670,  5091,  -670,   373,   373,   373,   373,   373,   668,   433,
     433,  -670,  -670,  -670,    93,  -670,   540,  3270,  4244,  -670,
      96,    97,  1680,  -670,  -670,  -670,  1703,  1788,  1807,  1831,
    1854,  1873,  1892,  1911,  1958,  1982,  2062,  2081,  -670,    99,
     100,   104,   108,   111,   690,   692,   693,  4364,  4364,  -670,
    4364,   117,  -670,  4244,  4244,  2152,  -670,   817,   817,   867,
     867,   743,  2036,  2036,  2036,  2036,  2036,  2036,   149,   149,
     290,  4244,  4244,  2152,  -670,   290,  4244,  2036,  4244,  2036,
     336,   694,  2310,   546,   547,  2152,  2152,  -670,   433,  -670,
    -670,  -670,  -670,   551,   628,  4244,  4244,  2310,   -29,   549,
     392,  -670,  -670,  2310,   664,  2254,  -670,  -670,  -670,  -670,
    -670,   -29,   552,   433,  -670,  2590,  -670,  4244,  -670,  4244,
    -670,  -670,  -670,  -670,  2152,  -670,  -670,   684,    37,   384,
    -670,  2109,   685,   564,  -670,   701,   455,  4244,  -670,   119,
    -670,  -670,  1295,   433,   708,   708,   467,  -670,  -670,    74,
     470,   726,  -670,   456,   708,   578,  3764,   663,  -670,   582,
    -670,   581,  -670,  -670,   729,   621,   732,  -670,   456,  -670,
    -670,  3644,  2152,   719,   120,  -670,  2152,  2152,  5091,  2152,
     687,   606,   668,   668,   668,   438,   643,   647,    83,  -670,
     161,  -670,  -670,  2152,  -670,  -670,  -670,  -670,  -670,  -670,
    -670,  -670,  -670,  -670,  -670,  -670,  -670,  -670,  -670,  -670,
    -670,  -670,  -670,  -670,  4724,  4724,  4724,  2152,  -670,  2152,
    -670,  2152,  -670,  -670,   587,  2036,  2036,   290,  2036,  2036,
    4244,  2430,  1315,   736,  -670,  -670,   416,  -670,   456,  -670,
    -670,  2133,  1122,   121,   737,  -670,  -670,  -670,  1249,   588,
     433,  -670,   539,  -670,  -670,  -670,  -670,  2152,   126,  -670,
     468,   468,   468,   468,   468,   432,  -670,   468,  -670,  -670,
    2152,  2190,  -670,  4004,  -670,  4844,  -670,  -670,   612,  -670,
    -670,  -670,   591,  -670,  -670,   708,   740,  -670,   663,  -670,
    3764,  -670,   132,   596,   597,   456,  -670,  5130,  2152,  -670,
    -670,   595,   598,  -670,  -670,   127,   744,   608,   614,   617,
    -670,   627,  4244,  1335,  4244,  2310,  -670,  -670,   747,  4244,
    -670,   433,  -670,   487,  -670,  -670,    24,    24,   630,   130,
     749,  -670,  -670,  2152,  4124,  -670,  2152,  -670,   740,  -670,
     788,  -670,  -670,   679,  -670,   792,  -670,  -670,  -670,  -670,
     645,  2152,  -670,  -670,  -670,  4244,  -670,  -670,  -670,  -670,
    2152,  4244,  1401,  -670,  4244,  1143,  -670,  -670,   646,  -670,
    4244,   650,   651,  -670,  1208,  2152,  4244,  2152,  4244,  -670,
    2152,  -670,  -670,  4244,  2152,  2152,  1229,  4244,  2152
};

/* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -670,  -670,  -670,   735,   554,   -58,  -670,  -670,    12,  -670,
    -398,  -670,    62,    69,  -670,  -670,   138,  -670,  -670,  -670,
     408,  -670,   137,     3,  -670,  -670,  -670,  -670,  -670,  -441,
    -670,  -670,  -240,   326,  -416,  -670,  -670,  -670,  -670,  -670,
    -670,  -105,  -670,  -670,  -258,   315,   -52,   -17,   566,   335,
    -670,  -670,  -670,  -670,  -670,  -670,  -670,   299,  -670,  -670,
    -642,  -670,  -670,  -670,   536,  -234,  -621,  -670,  -670,   124,
    -670,   755,  -248,  -670,    -1,    -2,   -12,  -144,    75,  -146,
    -423,  -467,  -670,  -670,   -74,  -670,  -670,  -670,  -186,  -670,
     450,   176,   -24,   189,     4,  -104,  -670,  -670,  -175,  -371,
     -20,   -45,  -670,  -124,  -670,  -670,  -670,     0,  -670,   -77,
    -670,  -670,  -670,  -670,  -670,  -670,  -670,  -670,  -670,   -25,
    -670,   724,   318,  -187,  -670,  -670,  -670,  -670,  -670,  -670,
    -670,  -670,   133,  -670,  -670,  -670,  -670,  -670,   380,  -670,
    -670,   194,  -670,  -670,  -670,  -171,   727,  -304,  -433,  -670,
    -669,  -515,  -159,  -329,  -447,  -670,   292,  -190,  -670,   295,
    -402,  -670,  -670,  -670
};

/* YYTABLE[YYPACT[STATE-NUM]].  What to do in state STATE-NUM.  If
   positive, shift that token.  If negative, reduce the rule which
   number is the opposite.  If YYTABLE_NINF, syntax error.  */
#define YYTABLE_NINF -480
static const yytype_int16 yytable[] =
{
     167,   196,   183,   390,   470,   102,   189,   209,   557,   183,
     496,   354,   631,   353,   484,   203,   308,   494,   488,   127,
     360,   210,   596,   195,   413,   411,   245,   246,   430,   662,
     288,   727,   411,   355,   729,   526,   191,   258,   261,   241,
     810,   423,   685,   191,   268,   813,   307,   391,   392,   220,
     221,   598,   225,   827,   425,   680,   811,   812,   813,   850,
     680,   714,   836,   388,   123,   102,   320,   354,   568,   353,
     427,   568,   383,   260,   262,   568,   673,   725,   264,   725,
     638,   285,   725,   286,   287,   794,   839,   655,   276,   355,
     402,   663,   667,   482,   125,   477,   738,   296,   228,   568,
     568,   478,   568,   568,   561,   183,   128,   568,   308,   229,
     724,   568,   483,   702,   568,   434,   323,   324,   730,   557,
     568,    77,   821,   847,   865,   331,   318,   279,   198,   667,
     738,   269,   270,   181,   199,   542,   385,   410,   307,   345,
     289,   185,  -292,   558,   410,   352,  -159,  -159,   640,   811,
     812,   813,   222,   222,   359,   206,   223,   223,   381,   271,
     553,   298,   299,   554,   555,   556,   544,   599,   188,   394,
     375,   376,   377,   222,  -159,   222,   398,   223,   850,   223,
     561,   814,   561,   290,   715,   403,   208,   406,   192,   396,
     404,   480,   595,   888,   814,   194,   204,   393,   829,   830,
     831,   352,   308,   485,   352,   594,   546,   353,   490,   407,
     308,   330,   493,   332,   464,   412,   420,   891,   569,   277,
     428,   573,   429,   432,   525,   575,   471,   355,   462,   191,
     639,   479,   307,   562,   672,   481,   545,   656,   557,   467,
     307,   664,   668,   694,   695,   675,   739,   102,   650,   744,
     745,   698,   759,   760,   557,   197,   474,   761,   280,   849,
     849,   762,   849,   489,   763,   471,   704,   523,   768,   770,
     773,   772,   822,   848,   870,   211,   528,   529,   713,   874,
     904,   533,   893,   707,    77,   511,   230,   814,   894,   537,
     538,   539,   540,  -159,   530,   657,   212,   553,   298,   299,
     554,   555,   556,   378,   379,   256,   218,   265,   630,   562,
     420,   562,   563,   377,   854,   308,   219,   572,   858,   859,
     226,   576,   577,   578,   579,   580,   581,   582,   583,   584,
     585,   586,   587,   901,   851,   852,   647,   231,   247,   359,
     248,   243,  -291,   201,   202,   307,   557,   234,   235,   605,
     607,   608,   609,   610,   611,   612,   613,   614,   615,   616,
     617,   618,   619,   620,   623,   625,   627,   629,   244,   875,
     430,   189,   413,   411,   411,   360,   880,   238,   239,   635,
     636,   725,   731,   732,   733,   734,   735,   680,   680,   680,
     680,   680,   298,   299,   680,   876,   877,   878,   879,   544,
     900,   251,   570,   571,   214,   215,   216,   544,   654,   861,
     553,   298,   299,   554,   555,   556,   261,   301,   302,   303,
     304,   589,   590,   591,   592,   593,   263,   557,   557,   557,
     557,   557,   723,   601,   849,   768,   770,   772,   828,   546,
     515,   516,   671,   282,   378,   379,   819,   546,   284,   308,
     308,   308,   308,   308,   292,   835,   471,   774,   795,   554,
     555,   556,   411,   674,   386,   660,   293,   681,   686,   545,
     845,   803,   838,   693,   692,   411,   696,   545,   254,   307,
     307,   307,   307,   307,   802,   410,   410,   130,   131,   700,
     701,   259,   295,  -144,   382,   497,   261,  -144,   557,   297,
     310,   158,   159,   321,   471,   471,  -144,   360,   327,   359,
    -144,  -144,   158,   159,   328,  -144,   161,   162,   329,   163,
     164,   333,   334,   305,   165,   419,   335,   161,   162,   722,
     163,   164,   397,   336,   337,   165,   399,   338,   339,   340,
     867,   341,   234,   235,   498,   797,   342,   742,   343,   344,
     346,   308,   347,   348,   349,   183,   743,   553,   298,   299,
     554,   555,   556,   412,   429,   465,   350,   358,   383,   499,
     385,   500,   501,   502,   410,   815,   503,   504,   386,   505,
     506,   307,   387,    77,   544,   767,   769,   410,   771,   466,
     400,   352,   775,   461,   472,   475,   247,   899,  -460,  -460,
    -460,  -460,  -460,  -460,   477,   256,   892,   491,   486,   776,
     777,   866,   492,   873,   778,   495,   779,   265,   774,   510,
     782,   512,   514,   527,   546,   298,   299,   554,   555,   556,
     531,   541,   551,   791,   792,   782,   559,   560,   600,   566,
     603,   798,   788,   643,   646,   290,   648,   598,   658,   659,
     661,   665,   429,   666,   545,   807,   669,   464,   670,   420,
     682,   683,   708,   703,   699,   429,   709,   805,   712,   361,
     362,   363,   364,   365,   366,   820,   367,   368,   369,   370,
     371,   372,   719,   373,   374,   375,   376,   377,   720,   721,
     728,   556,   740,   764,   471,   765,   766,   824,   780,   786,
     787,   789,   796,   544,   790,   804,   809,   817,   597,   533,
    -224,  -224,  -224,  -224,  -224,  -224,   479,  -224,  -224,  -224,
    -224,  -224,  -224,   818,  -224,  -224,  -224,  -224,  -224,   825,
     834,   837,   840,   842,   841,   844,   843,  -461,   360,   865,
     860,   872,   871,   546,   887,  -335,   897,   715,   902,   905,
     914,   903,   767,   769,   771,   367,   368,   369,   370,   371,
     372,   906,   373,   374,   375,   376,   377,   907,   189,   863,
     908,   917,   645,   545,   361,   362,   363,   364,   365,   366,
     909,   367,   368,   369,   370,   371,   372,   814,   373,   374,
     375,   376,   377,   920,   921,   922,   233,   473,   923,   929,
     931,   893,   855,   806,   318,   641,   808,   853,   684,   692,
     697,   883,   676,   886,   476,   705,   524,   237,   378,   379,
     881,   882,   800,   793,   833,   364,   365,   366,   471,   367,
     368,   369,   370,   371,   372,   633,   373,   374,   375,   376,
     377,   913,   799,   919,   898,   932,   319,   313,   718,   801,
     910,   736,   912,   782,   846,   737,     0,   915,     0,  -224,
    -224,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   359,     0,     0,   916,     0,   366,     0,   367,
     368,   369,   370,   371,   372,     0,   373,   374,   375,   376,
     377,     0,     0,   924,     0,     0,     0,   378,   379,   925,
       0,     0,   927,     2,     3,     0,     0,     0,   930,     0,
       0,     0,     0,     0,   934,     0,   935,     0,     0,     0,
       0,   936,     0,   378,   379,   938,     0,     0,     4,     0,
       0,     5,   -16,     6,     7,     8,     0,     0,     9,    10,
      11,     0,     0,     0,     0,    12,    13,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,    14,     0,
       0,     0,    15,    16,     0,     0,     0,    17,     0,     0,
       0,   378,   379,    18,     0,    19,    20,     0,    21,     0,
      22,    23,    24,    25,    26,    27,    28,    29,    30,    31,
      32,    33,    34,    35,    36,    37,    38,    39,    40,    41,
      42,    43,    44,    45,    46,     0,    47,    48,    49,     0,
      50,    51,    52,     0,    53,     0,    54,     0,     0,    55,
      56,   378,   379,    57,    58,    59,     0,     0,     0,     0,
      60,     0,     0,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,    71,    72,    73,     0,     0,    74,     0,
       0,    75,    76,    77,    78,     0,     0,     0,  -479,    79,
    -479,  -479,  -479,  -479,  -479,  -479,     0,  -479,  -479,  -479,
    -479,  -479,  -479,     0,  -479,  -479,  -479,  -479,  -479,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   132,   133,
       0,     0,     0,   134,   135,   136,   137,     0,     0,     0,
       0,     0,     0,   138,   139,   140,   141,   142,   143,   144,
       0,   145,   146,   147,     0,     0,   148,   149,   150,     0,
     151,   152,   153,   154,   155,   869,   156,   361,   362,   363,
     364,   365,   366,     0,   367,   368,   369,   370,   371,   372,
       0,   373,   374,   375,   376,   377,   928,     0,   361,   362,
     363,   364,   365,   366,     0,   367,   368,   369,   370,   371,
     372,     0,   373,   374,   375,   376,   377,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   158,   159,     0,     0,     0,     0,   160,
       0,     0,     0,     0,     0,     0,     0,     0,   161,   162,
       0,   163,   164,     0,     0,    77,   165,   543,     0,  -479,
    -479,   933,     0,   361,   362,   363,   364,   365,   366,     0,
     367,   368,   369,   370,   371,   372,     0,   373,   374,   375,
     376,   377,   937,     0,   361,   362,   363,   364,   365,   366,
       0,   367,   368,   369,   370,   371,   372,     0,   373,   374,
     375,   376,   377,   864,   361,   362,   363,   364,   365,   366,
       0,   367,   368,   369,   370,   371,   372,     0,   373,   374,
     375,   376,   377,     0,     0,     0,   378,   379,   361,   362,
     363,   364,   365,   366,     0,   367,   368,   369,   370,   371,
     372,     0,   373,   374,   375,   376,   377,   378,   379,   823,
     361,   362,   363,   364,   365,   366,     0,   367,   368,   369,
     370,   371,   372,     0,   373,   374,   375,   376,   377,   864,
     361,   362,   363,   364,   365,   366,     0,   367,   368,   369,
     370,   371,   372,     0,   373,   374,   375,   376,   377,   911,
     361,   362,   363,   364,   365,   366,     0,   367,   368,   369,
     370,   371,   372,     0,   373,   374,   375,   376,   377,     0,
       0,     0,   378,   379,   361,   362,   363,   364,   365,   366,
       0,   367,   368,   369,   370,   371,   372,     0,   373,   374,
     375,   376,   377,   378,   379,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   602,   378,   379,   926,   361,   362,   363,   364,
     365,   366,     0,   367,   368,   369,   370,   371,   372,     0,
     373,   374,   375,   376,   377,     0,   405,   378,   379,   361,
     362,   363,   364,   365,   366,     0,   367,   368,   369,   370,
     371,   372,     0,   373,   374,   375,   376,   377,     0,   378,
     379,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   378,
     379,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   378,
     379,   361,   362,   363,   364,   365,   366,     0,   367,   368,
     369,   370,   371,   372,     0,   373,   374,   375,   376,   377,
       0,     0,   567,   378,   379,  -226,  -226,  -226,  -226,  -226,
    -226,     0,  -226,  -226,  -226,  -226,  -226,  -226,     0,  -226,
    -226,  -226,  -226,  -226,  -225,  -225,  -225,  -225,  -225,  -225,
       0,  -225,  -225,  -225,  -225,  -225,  -225,     0,  -225,  -225,
    -225,  -225,  -225,     0,     0,   378,   379,   361,   362,   363,
     364,   365,   366,     0,   367,   368,   369,   370,   371,   372,
       0,   373,   374,   375,   376,   377,     0,   574,   378,   379,
     361,   362,   363,   364,   365,   366,     0,   367,   368,   369,
     370,   371,   372,     0,   373,   374,   375,   376,   377,   361,
     362,   363,   364,   365,   366,     0,   367,   368,   369,   370,
     371,   372,     0,   373,   374,   375,   376,   377,   361,   362,
     363,   364,   365,   366,     0,   367,   368,   369,   370,   371,
     372,     0,   373,   374,   375,   376,   377,     0,     0,   588,
     378,   379,   361,   362,   363,   364,   365,   366,     0,   367,
     368,   369,   370,   371,   372,     0,   373,   374,   375,   376,
     377,     0,     0,  -226,  -226,  -226,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -225,  -225,  -225,   361,   362,   363,   364,   365,
     366,     0,   367,   368,   369,   370,   371,   372,     0,   373,
     374,   375,   376,   377,     0,   602,   378,   379,   361,   362,
     363,   364,   365,   366,     0,   367,   368,   369,   370,   371,
     372,     0,   373,   374,   375,   376,   377,     0,   637,   378,
     379,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   642,   378,   379,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   644,   378,   379,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   387,     0,     0,
       0,   378,   379,   361,   362,   363,   364,   365,   366,     0,
     367,   368,   369,   370,   371,   372,     0,   373,   374,   375,
     376,   377,   361,   362,   363,   364,   365,   366,     0,   367,
     368,   369,   370,   371,   372,     0,   373,   374,   375,   376,
     377,     0,     0,   746,   378,   379,   361,   362,   363,   364,
     365,   366,     0,   367,   368,   369,   370,   371,   372,     0,
     373,   374,   375,   376,   377,     0,   747,   378,   379,   361,
     362,   363,   364,   365,   366,     0,   367,   368,   369,   370,
     371,   372,     0,   373,   374,   375,   376,   377,   361,   362,
     363,   364,   365,   366,     0,   367,   368,   369,   370,   371,
     372,     0,   373,   374,   375,   376,   377,   361,   362,   363,
     364,   365,   366,     0,   367,   368,   369,   370,   371,   372,
       0,   373,   374,   375,   376,   377,   361,   362,   363,   364,
     365,   366,     0,   367,   368,   369,   370,   371,   372,     0,
     373,   374,   375,   376,   377,     0,     0,     0,     0,     0,
       0,   748,   378,   379,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     749,   378,   379,   361,   362,   363,   364,   365,   366,     0,
     367,   368,   369,   370,   371,   372,     0,   373,   374,   375,
     376,   377,     0,     0,   750,   378,   379,   361,   362,   363,
     364,   365,   366,     0,   367,   368,   369,   370,   371,   372,
       0,   373,   374,   375,   376,   377,     0,   751,   378,   379,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   752,   378,   379,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   753,   378,   379,  -480,  -480,
    -480,  -480,  -480,  -480,     0,   373,   374,   375,   376,   377,
       0,     0,     0,     0,   754,   378,   379,   361,   362,   363,
     364,   365,   366,     0,   367,   368,   369,   370,   371,   372,
       0,   373,   374,   375,   376,   377,   361,   362,   363,   364,
     365,   366,     0,   367,   368,   369,   370,   371,   372,     0,
     373,   374,   375,   376,   377,     0,     0,     0,     0,     0,
       0,   755,   378,   379,   361,   362,   363,   364,   365,   366,
       0,   367,   368,   369,   370,   371,   372,     0,   373,   374,
     375,   376,   377,     0,     0,   756,   378,   379,   361,   362,
     363,   364,   365,   366,     0,   367,   368,   369,   370,   371,
     372,     0,   373,   374,   375,   376,   377,   361,   362,   363,
     364,   365,   366,     0,   367,   368,   369,   370,   371,   372,
       0,   373,   374,   375,   376,   377,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     378,   379,     0,     0,   687,     0,     0,     0,     0,     0,
       0,   129,     0,     0,     0,     0,     0,     0,     0,   130,
     131,   688,     0,     0,     0,   757,   378,   379,     0,     0,
       0,     0,     0,   132,   133,     0,     0,     0,   134,   135,
     136,   137,     0,     0,   758,   378,   379,     0,   138,   139,
     140,   141,   142,   143,   144,     0,   145,   146,   147,     0,
       0,   148,   149,   150,     0,   151,   152,   153,   154,   155,
       0,   156,   816,   378,   379,     0,     0,     0,     0,     0,
       0,     0,     0,   298,   299,   417,     0,   418,     0,     0,
       0,     0,     0,     0,     0,     0,   868,   378,   379,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   301,   302,
     303,   304,     0,     0,     0,     0,   378,   379,   157,     0,
       0,     0,     0,     0,   781,     0,     0,     0,   158,   159,
       0,   129,     0,     0,   160,     0,     0,     0,     0,   130,
     131,     0,     0,   161,   162,     0,   163,   164,     0,     0,
      77,   165,   166,   132,   133,     0,     0,     0,   134,   135,
     136,   137,     0,     0,     0,     0,     0,     0,   138,   139,
     140,   141,   142,   143,   144,     0,   145,   146,   147,     0,
       0,   148,   149,   150,     0,   151,   152,   153,   154,   155,
       0,   156,   158,   159,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   298,   299,   300,     0,   161,   162,     0,
     163,   164,     0,     0,   305,   165,   419,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   301,   302,
     303,   304,     0,     0,     0,     0,     0,     0,   157,     0,
       0,     0,     0,     0,   862,     0,     0,     0,   158,   159,
       0,   129,     0,     0,   160,     0,     0,     0,     0,   130,
     131,     0,     0,   161,   162,     0,   163,   164,     0,     0,
      77,   165,   166,   132,   133,     0,     0,     0,   134,   135,
     136,   137,     0,     0,     0,     0,     0,     0,   138,   139,
     140,   141,   142,   143,   144,     0,   145,   146,   147,     0,
       0,   148,   149,   150,     0,   151,   152,   153,   154,   155,
       0,   156,   158,   159,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   161,   162,     0,
     163,   164,     0,     0,   305,   165,   306,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   157,     0,
       0,     0,     0,     0,     0,   468,     0,     0,   158,   159,
       0,   129,     0,     0,   160,     0,     0,     0,     0,   130,
     131,   469,     0,   161,   162,     0,   163,   164,     0,     0,
      77,   165,   166,   132,   133,     0,     0,     0,   134,   135,
     136,   137,     0,     0,     0,     0,     0,     0,   138,   139,
     140,   141,   142,   143,   144,     0,   145,   146,   147,     0,
       0,   148,   149,   150,     0,   151,   152,   153,   154,   155,
       0,   156,     0,     0,   435,     0,     0,     0,   436,   437,
     438,   439,     0,     0,     0,     0,     0,     0,   440,   441,
     442,   443,   444,   445,   446,     0,   447,   448,   449,     0,
       0,   450,   451,   452,     0,     0,   453,   454,   455,   456,
       0,   457,     0,     0,     0,     0,     0,     0,   157,     0,
       0,     0,     0,     0,     0,   621,     0,     0,   158,   159,
       0,   129,     0,     0,   160,     0,     0,     0,     0,   130,
     131,     0,   622,   161,   162,     0,   163,   164,     0,     0,
      77,   165,   166,   132,   133,     0,     0,     0,   134,   135,
     136,   137,     0,     0,     0,     0,     0,     0,   138,   139,
     140,   141,   142,   143,   144,     0,   145,   146,   147,     0,
       0,   148,   149,   150,     0,   151,   152,   153,   154,   155,
     458,   156,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   157,     0,
       0,     0,     0,     0,     0,   706,     0,     0,   158,   159,
       0,   129,     0,     0,   160,     0,     0,     0,     0,   130,
     131,   469,     0,   161,   162,     0,   163,   164,     0,     0,
      77,   165,   166,   132,   133,     0,     0,     0,   134,   135,
     136,   137,     0,     0,     0,     0,     0,     0,   138,   139,
     140,   141,   142,   143,   144,     0,   145,   146,   147,     0,
       0,   148,   149,   150,     0,   151,   152,   153,   154,   155,
       0,   156,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   157,     0,
       0,     0,     0,     0,     0,   604,     0,     0,   158,   159,
       0,   129,     0,     0,   160,     0,     0,     0,     0,   130,
     131,     0,     0,   161,   162,     0,   163,   164,     0,     0,
      77,   165,   166,   132,   133,     0,     0,     0,   134,   135,
     136,   137,     0,     0,     0,     0,     0,     0,   138,   139,
     140,   141,   142,   143,   144,     0,   145,   146,   147,     0,
       0,   148,   149,   150,     0,   151,   152,   153,   154,   155,
       0,   156,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   157,     0,
       0,     0,     0,     0,     0,   626,     0,     0,   158,   159,
       0,   129,     0,     0,   160,     0,     0,     0,     0,   130,
     131,     0,     0,   161,   162,     0,   163,   164,     0,     0,
      77,   165,   166,   132,   133,     0,     0,     0,   134,   135,
     136,   137,     0,     0,     0,     0,     0,     0,   138,   139,
     140,   141,   142,   143,   144,     0,   145,   146,   147,     0,
       0,   148,   149,   150,     0,   151,   152,   153,   154,   155,
       0,   156,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   157,     0,
       0,     0,     0,     0,     0,   628,     0,     0,   158,   159,
       0,   129,     0,     0,   160,     0,     0,     0,     0,   130,
     131,     0,     0,   161,   162,     0,   163,   164,     0,     0,
      77,   165,   166,   132,   133,     0,     0,     0,   134,   135,
     136,   137,     0,     0,     0,     0,     0,     0,   138,   139,
     140,   141,   142,   143,   144,     0,   145,   146,   147,     0,
       0,   148,   149,   150,     0,   151,   152,   153,   154,   155,
       0,   156,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   157,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   158,   159,
       0,   129,     0,     0,   160,     0,     0,     0,     0,   130,
     131,     0,     0,   161,   162,     0,   163,   164,     0,     0,
      77,   165,   166,   132,   133,     0,     0,     0,   134,   135,
     136,   137,     0,     0,     0,     0,     0,     0,   138,   139,
     140,   141,   142,   143,   144,     0,   145,   146,   147,     0,
       0,   148,   149,   150,     0,   151,   152,   153,   154,   155,
       0,   156,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   157,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   158,   159,
       0,     0,     0,     0,   160,     0,     0,     0,     0,     0,
       0,     0,     0,   161,   162,   129,   163,   164,     0,     0,
      77,   165,   166,   130,   131,   651,   182,   652,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   132,   133,     0,
       0,     0,   134,   135,   136,   137,     0,     0,     0,     0,
       0,     0,   138,   139,   140,   141,   142,   143,   144,     0,
     145,   146,   147,     0,     0,   148,   149,   150,     0,   151,
     152,   153,   154,   155,     0,   156,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   157,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   158,   159,     0,   129,     0,     0,   160,     0,
       0,     0,     0,   130,   131,   469,     0,   161,   162,     0,
     163,   164,     0,     0,    77,   165,   653,   132,   133,     0,
       0,     0,   134,   135,   136,   137,     0,     0,     0,     0,
       0,     0,   138,   139,   140,   141,   142,   143,   144,     0,
     145,   146,   147,     0,     0,   148,   149,   150,     0,   151,
     152,   153,   154,   155,     0,   156,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   157,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   158,   159,     0,   129,     0,     0,   160,     0,
       0,     0,     0,   130,   131,   532,     0,   161,   162,     0,
     163,   164,     0,     0,   517,   165,   166,   132,   133,     0,
       0,     0,   134,   135,   136,   137,     0,     0,     0,     0,
       0,     0,   138,   139,   140,   141,   142,   143,   144,     0,
     145,   146,   147,     0,     0,   148,   149,   150,     0,   151,
     152,   153,   154,   155,     0,   156,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   157,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   158,   159,     0,   129,     0,     0,   160,     0,
       0,     0,     0,   130,   131,   469,     0,   161,   162,     0,
     163,   164,     0,     0,    77,   165,   166,   132,   133,     0,
       0,     0,   134,   135,   136,   137,     0,     0,     0,     0,
       0,     0,   138,   139,   140,   141,   142,   143,   144,     0,
     145,   146,   147,     0,     0,   148,   149,   150,     0,   151,
     152,   153,   154,   155,     0,   156,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   157,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   158,   159,     0,   129,     0,     0,   160,     0,
       0,     0,     0,   130,   131,   711,     0,   161,   162,     0,
     163,   164,     0,     0,    77,   165,   166,   132,   133,     0,
       0,     0,   134,   135,   136,   137,     0,     0,     0,     0,
       0,     0,   138,   139,   140,   141,   142,   143,   144,     0,
     145,   146,   147,     0,     0,   148,   149,   150,     0,   151,
     152,   153,   154,   155,     0,   156,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   157,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   158,   159,     0,   129,     0,     0,   160,     0,
       0,     0,     0,   130,   131,   688,     0,   161,   162,     0,
     163,   164,     0,     0,    77,   165,   166,   132,   133,     0,
       0,     0,   134,   135,   136,   137,     0,     0,     0,     0,
       0,     0,   138,   139,   140,   141,   142,   143,   144,     0,
     145,   146,   147,     0,     0,   148,   149,   150,     0,   151,
     152,   153,   154,   155,     0,   156,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   157,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   158,   159,     0,   129,     0,     0,   160,     0,
       0,     0,     0,   130,   131,   918,     0,   161,   162,     0,
     163,   164,     0,     0,    77,   165,   166,   132,   133,     0,
       0,     0,   134,   135,   136,   137,     0,     0,     0,     0,
       0,     0,   138,   139,   140,   141,   142,   143,   144,     0,
     145,   146,   147,     0,     0,   148,   149,   150,     0,   151,
     152,   153,   154,   155,     0,   156,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   157,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   158,   159,     0,   129,     0,     0,   160,     0,
       0,     0,     0,   130,   131,     0,     0,   161,   162,     0,
     163,   164,     0,     0,    77,   165,   166,   132,   133,     0,
       0,     0,   134,   135,   136,   137,     0,     0,     0,     0,
       0,     0,   138,   139,   140,   141,   142,   143,   144,     0,
     145,   146,   147,     0,     0,   148,   149,   150,     0,   151,
     152,   153,   154,   155,     0,   156,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   157,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   158,   159,     0,   129,     0,     0,   160,     0,
       0,     0,     0,   130,   131,     0,     0,   161,   162,     0,
     163,   164,     0,     0,    77,   165,   166,   132,   133,     0,
       0,     0,   134,   135,   136,   137,     0,     0,     0,     0,
       0,     0,   138,   139,   140,   141,   142,   143,   144,     0,
     145,   146,   147,     0,     0,   148,   149,   150,     0,   151,
     152,   153,   154,   155,     0,   156,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   157,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   158,   159,     0,   129,     0,     0,   160,     0,
       0,     0,     0,   130,   131,     0,     0,   161,   162,     0,
     163,   164,     0,     0,    77,   165,   351,   132,   133,     0,
       0,     0,   134,   135,   136,   137,     0,     0,     0,     0,
       0,     0,   138,   139,   140,   141,   142,   143,   144,     0,
     145,   146,   147,     0,     0,   148,   149,   150,     0,   151,
     152,   153,   154,   155,     0,   156,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   157,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   158,   159,     0,   129,     0,     0,   160,     0,
       0,     0,     0,   130,   131,     0,     0,   161,   162,     0,
     163,   164,     0,     0,    77,   426,   166,   132,   133,     0,
       0,     0,   134,   135,   136,   137,     0,     0,     0,     0,
       0,     0,   138,   139,   140,   141,   142,   143,   144,     0,
     145,   146,   147,     0,     0,   148,   149,   150,     0,   151,
     152,   153,   154,   155,     0,   156,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   157,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   158,   159,     0,   129,     0,     0,   160,     0,
       0,     0,     0,   130,   131,     0,     0,   161,   162,     0,
     163,   164,     0,     0,    77,   520,   521,   132,   133,     0,
       0,     0,   134,   135,   136,   137,     0,     0,     0,     0,
       0,     0,   138,   139,   140,   141,   142,   143,   144,     0,
     145,   146,   147,     0,     0,   148,   149,   150,     0,   151,
     152,   153,   154,   155,     0,   156,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   157,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   158,   159,     0,   129,     0,     0,   160,     0,
       0,     0,     0,   130,   131,     0,     0,   161,   162,     0,
     163,   164,     0,     0,   856,   165,   351,   132,   133,     0,
       8,     0,   134,   135,   136,   137,     0,     0,     0,     0,
       0,     0,   138,   139,   140,   141,   142,   143,   144,     0,
     145,   146,   147,    14,     0,   148,   149,   150,     0,   151,
     152,   153,   154,   155,     0,   156,     0,     0,    18,     0,
       0,     0,     0,    21,   315,     0,     0,     0,    25,    26,
       0,     0,     0,     0,     0,     0,     0,   316,    35,    36,
      37,    38,    39,    40,    41,   317,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,    51,     0,     0,     0,
       0,    54,   157,     0,     0,     0,     0,     0,    57,     0,
      59,     0,   158,   159,     0,     0,     0,     0,   160,     0,
       0,     0,     0,     0,     0,     0,     0,   161,   162,     0,
     163,   164,     0,     0,    77,   165,   884,     4,    77,    78,
       5,     0,     6,     7,     8,     0,     0,     9,    10,    11,
       0,     0,     0,     0,    12,    13,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,    14,     0,     0,
       0,    15,    16,     0,     0,     0,    17,     0,     0,     0,
       0,     0,    18,     0,    19,    20,     0,    21,     0,    22,
      23,    24,    25,    26,    27,    28,    29,    30,    31,    32,
      33,    34,    35,    36,    37,    38,    39,    40,    41,    42,
      43,    44,    45,    46,     0,    47,    48,    49,     0,    50,
      51,    52,     0,    53,     0,    54,     0,     0,    55,    56,
       0,     0,    57,    58,    59,     0,     0,     0,     0,    60,
       0,     0,     0,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    72,    73,     0,     0,    74,     0,     0,
      75,    76,    77,    78,   132,   133,     0,     0,     0,   134,
     135,   136,   137,     0,     0,     0,     0,     0,     0,   138,
     139,   140,   141,   142,   143,   144,     0,   145,   146,   147,
       0,     0,   148,   149,   150,     0,   151,   152,   153,   154,
     155,     0,   156,   132,   133,     0,     0,     0,   134,   135,
     136,   137,     0,     0,     0,     0,     0,     0,   138,   139,
     140,   141,   142,   143,   144,     0,   145,   146,   147,     0,
       0,   148,   149,   150,     0,   151,   152,   153,   154,   155,
       0,   156,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   158,
     159,     0,     0,     0,     0,   160,     0,     0,     0,     0,
       0,     0,     0,     0,   161,   162,     0,   163,   164,     0,
       0,    77,   165,   543,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   158,   159,
       0,     0,     0,     0,   160,     0,     0,     0,     0,     0,
       0,     0,     0,   161,   162,     0,   163,   164,     0,     0,
     856,   165,   543
};

#define yypact_value_is_default(yystate) \
  ((yystate) == (-670))

#define yytable_value_is_error(yytable_value) \
  ((yytable_value) == (-480))

static const yytype_int16 yycheck[] =
{
      12,    21,    14,   178,   238,     1,    18,    32,   312,    21,
     268,   157,   383,   157,   254,    27,   120,   265,   258,     7,
     166,    33,   351,    20,   211,   211,    84,    85,   218,   431,
       5,   546,   218,   157,   549,   283,     4,    89,     3,    64,
       3,   212,   483,     4,    96,    21,   120,     3,     4,    46,
      47,     3,    48,   695,   213,   478,    19,    20,    21,   728,
     483,    27,   704,   143,   143,    61,   124,   213,     3,   213,
     216,     3,   152,    90,    91,     3,   474,   544,    95,   546,
       3,   106,   549,   108,   109,   114,   707,     3,    21,   213,
     195,     3,     3,     3,   150,     3,     3,   117,   112,     3,
       3,    22,     3,     3,    21,   117,   152,     3,   212,   123,
     543,     3,    22,   511,     3,   220,   128,   129,   551,   423,
       3,   150,     3,     3,     3,   137,   122,    21,   150,     3,
       3,     3,     4,   152,   156,   306,   152,   211,   212,   151,
     115,   152,   158,   314,   218,   157,     3,     4,   396,    19,
      20,    21,    18,    18,   166,   150,    22,    22,   170,    31,
      18,    19,    20,    21,    22,    23,   310,   119,   152,   181,
      21,    22,    23,    18,    31,    18,   188,    22,   847,    22,
      21,   157,    21,   158,   150,   197,   150,   204,   156,   185,
     202,   156,   351,   835,   157,   156,   151,   153,   124,   125,
     126,   213,   306,   255,   216,   351,   310,   351,   260,   205,
     314,   136,   264,   138,   226,   211,   212,   838,   153,   152,
     216,   153,   218,   219,   282,   153,   238,   351,   224,     4,
     153,   152,   306,   150,   468,   252,   310,   153,   542,   236,
     314,   153,   153,   491,   492,   153,   153,   243,   419,   153,
     153,   499,   153,   153,   558,   152,   244,   153,   152,   726,
     727,   153,   729,   259,   153,   277,   514,   279,   597,   598,
     153,   600,   153,   153,   153,   152,   288,   289,   526,   153,
     153,   293,   150,   517,   150,   273,   116,   157,   156,   301,
     302,   303,   304,   150,   290,   153,   152,    18,    19,    20,
      21,    22,    23,   154,   155,   150,   152,   150,   382,   150,
     306,   150,   153,    23,   153,   419,   152,   329,   765,   766,
     152,   333,   334,   335,   336,   337,   338,   339,   340,   341,
     342,   343,   344,   848,   736,   737,   152,   150,   150,   351,
     152,    24,   158,   151,   152,   419,   650,    72,    73,   361,
     362,   363,   364,   365,   366,   367,   368,   369,   370,   371,
     372,   373,   374,   375,   376,   377,   378,   379,    29,   810,
     560,   383,   559,   559,   560,   521,   817,   150,   151,   391,
     392,   848,   553,   554,   555,   556,   557,   810,   811,   812,
     813,   814,    19,    20,   817,   811,   812,   813,   814,   543,
     847,   150,   327,   328,   150,   151,   152,   551,   420,   780,
      18,    19,    20,    21,    22,    23,     3,    44,    45,    46,
      47,   346,   347,   348,   349,   350,     3,   731,   732,   733,
     734,   735,   153,   358,   901,   764,   765,   766,   696,   543,
     151,   152,   467,     4,   154,   155,   686,   551,   150,   553,
     554,   555,   556,   557,     3,   703,   468,   603,   648,    21,
      22,    23,   648,   475,   152,   153,   152,   479,   485,   543,
     718,   661,   706,   490,   486,   661,   493,   551,     3,   553,
     554,   555,   556,   557,   655,   559,   560,    19,    20,     3,
       4,     3,   150,    18,   158,    25,     3,    22,   802,   119,
     120,   128,   129,   152,   516,   517,    18,   653,   152,   521,
      22,    18,   128,   129,   152,    22,   143,   144,   152,   146,
     147,   152,   152,   150,   151,   152,   152,   143,   144,   541,
     146,   147,     3,   152,   152,   151,     4,   152,   152,   152,
     788,   152,    72,    73,    74,   153,   152,   567,   152,   152,
     152,   655,   152,   152,   152,   567,   568,    18,    19,    20,
      21,    22,    23,   559,   560,     3,   152,   152,   152,    99,
     152,   101,   102,   103,   648,   679,   106,   107,   152,   109,
     110,   655,   150,   150,   728,   597,   598,   661,   600,     3,
     150,   603,   604,   150,   153,     5,   150,   845,    18,    19,
      20,    21,    22,    23,     3,   150,   840,   150,   152,   621,
     622,   786,   150,   800,   626,     3,   628,   150,   764,     4,
     632,     3,   150,     3,   728,    19,    20,    21,    22,    23,
     151,     5,     3,   645,   646,   647,   152,   152,     3,    26,
       3,   653,   638,     3,     5,   158,     3,     3,   153,   153,
       3,     3,   648,    22,   728,   667,   152,   669,   150,   655,
     150,   150,   153,   150,   152,   661,     3,   663,   153,     5,
       6,     7,     8,     9,    10,   687,    12,    13,    14,    15,
      16,    17,   151,    19,    20,    21,    22,    23,   153,     3,
       3,    23,   152,     3,   706,     3,     3,   693,     4,   153,
     153,   150,   153,   847,    76,   153,    22,    22,     3,   721,
       5,     6,     7,     8,     9,    10,   152,    12,    13,    14,
      15,    16,    17,    22,    19,    20,    21,    22,    23,    21,
       4,   153,   150,     4,   153,     3,   115,    18,   884,     3,
     153,   153,     5,   847,   153,     5,   150,   150,   153,     5,
       3,   153,   764,   765,   766,    12,    13,    14,    15,    16,
      17,   153,    19,    20,    21,    22,    23,   153,   780,   781,
     153,    22,     3,   847,     5,     6,     7,     8,     9,    10,
     153,    12,    13,    14,    15,    16,    17,   157,    19,    20,
      21,    22,    23,     5,   115,     3,    61,   243,   153,   153,
     150,   150,   740,   665,   800,   397,   669,   738,   482,   821,
     495,   823,   477,   825,   248,   516,   280,    62,   154,   155,
     821,   823,   158,   647,   700,     8,     9,    10,   840,    12,
      13,    14,    15,    16,    17,   385,    19,    20,    21,    22,
      23,   865,   653,   888,   844,   922,   122,   120,   530,   655,
     862,   559,   864,   865,   721,   560,    -1,   869,    -1,   154,
     155,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   884,    -1,    -1,   871,    -1,    10,    -1,    12,
      13,    14,    15,    16,    17,    -1,    19,    20,    21,    22,
      23,    -1,    -1,   905,    -1,    -1,    -1,   154,   155,   911,
      -1,    -1,   914,     0,     1,    -1,    -1,    -1,   920,    -1,
      -1,    -1,    -1,    -1,   926,    -1,   928,    -1,    -1,    -1,
      -1,   933,    -1,   154,   155,   937,    -1,    -1,    25,    -1,
      -1,    28,    29,    30,    31,    32,    -1,    -1,    35,    36,
      37,    -1,    -1,    -1,    -1,    42,    43,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,    -1,
      -1,    -1,    59,    60,    -1,    -1,    -1,    64,    -1,    -1,
      -1,   154,   155,    70,    -1,    72,    73,    -1,    75,    -1,
      77,    78,    79,    80,    81,    82,    83,    84,    85,    86,
      87,    88,    89,    90,    91,    92,    93,    94,    95,    96,
      97,    98,    99,   100,   101,    -1,   103,   104,   105,    -1,
     107,   108,   109,    -1,   111,    -1,   113,    -1,    -1,   116,
     117,   154,   155,   120,   121,   122,    -1,    -1,    -1,    -1,
     127,    -1,    -1,   130,   131,   132,   133,   134,   135,   136,
     137,   138,   139,   140,   141,   142,    -1,    -1,   145,    -1,
      -1,   148,   149,   150,   151,    -1,    -1,    -1,     3,   156,
       5,     6,     7,     8,     9,    10,    -1,    12,    13,    14,
      15,    16,    17,    -1,    19,    20,    21,    22,    23,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    33,    34,
      -1,    -1,    -1,    38,    39,    40,    41,    -1,    -1,    -1,
      -1,    -1,    -1,    48,    49,    50,    51,    52,    53,    54,
      -1,    56,    57,    58,    -1,    -1,    61,    62,    63,    -1,
      65,    66,    67,    68,    69,     3,    71,     5,     6,     7,
       8,     9,    10,    -1,    12,    13,    14,    15,    16,    17,
      -1,    19,    20,    21,    22,    23,     3,    -1,     5,     6,
       7,     8,     9,    10,    -1,    12,    13,    14,    15,    16,
      17,    -1,    19,    20,    21,    22,    23,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   128,   129,    -1,    -1,    -1,    -1,   134,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   143,   144,
      -1,   146,   147,    -1,    -1,   150,   151,   152,    -1,   154,
     155,     3,    -1,     5,     6,     7,     8,     9,    10,    -1,
      12,    13,    14,    15,    16,    17,    -1,    19,    20,    21,
      22,    23,     3,    -1,     5,     6,     7,     8,     9,    10,
      -1,    12,    13,    14,    15,    16,    17,    -1,    19,    20,
      21,    22,    23,     4,     5,     6,     7,     8,     9,    10,
      -1,    12,    13,    14,    15,    16,    17,    -1,    19,    20,
      21,    22,    23,    -1,    -1,    -1,   154,   155,     5,     6,
       7,     8,     9,    10,    -1,    12,    13,    14,    15,    16,
      17,    -1,    19,    20,    21,    22,    23,   154,   155,     4,
       5,     6,     7,     8,     9,    10,    -1,    12,    13,    14,
      15,    16,    17,    -1,    19,    20,    21,    22,    23,     4,
       5,     6,     7,     8,     9,    10,    -1,    12,    13,    14,
      15,    16,    17,    -1,    19,    20,    21,    22,    23,     4,
       5,     6,     7,     8,     9,    10,    -1,    12,    13,    14,
      15,    16,    17,    -1,    19,    20,    21,    22,    23,    -1,
      -1,    -1,   154,   155,     5,     6,     7,     8,     9,    10,
      -1,    12,    13,    14,    15,    16,    17,    -1,    19,    20,
      21,    22,    23,   154,   155,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   153,   154,   155,     4,     5,     6,     7,     8,
       9,    10,    -1,    12,    13,    14,    15,    16,    17,    -1,
      19,    20,    21,    22,    23,    -1,   153,   154,   155,     5,
       6,     7,     8,     9,    10,    -1,    12,    13,    14,    15,
      16,    17,    -1,    19,    20,    21,    22,    23,    -1,   154,
     155,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,
     155,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,
     155,     5,     6,     7,     8,     9,    10,    -1,    12,    13,
      14,    15,    16,    17,    -1,    19,    20,    21,    22,    23,
      -1,    -1,   153,   154,   155,     5,     6,     7,     8,     9,
      10,    -1,    12,    13,    14,    15,    16,    17,    -1,    19,
      20,    21,    22,    23,     5,     6,     7,     8,     9,    10,
      -1,    12,    13,    14,    15,    16,    17,    -1,    19,    20,
      21,    22,    23,    -1,    -1,   154,   155,     5,     6,     7,
       8,     9,    10,    -1,    12,    13,    14,    15,    16,    17,
      -1,    19,    20,    21,    22,    23,    -1,   153,   154,   155,
       5,     6,     7,     8,     9,    10,    -1,    12,    13,    14,
      15,    16,    17,    -1,    19,    20,    21,    22,    23,     5,
       6,     7,     8,     9,    10,    -1,    12,    13,    14,    15,
      16,    17,    -1,    19,    20,    21,    22,    23,     5,     6,
       7,     8,     9,    10,    -1,    12,    13,    14,    15,    16,
      17,    -1,    19,    20,    21,    22,    23,    -1,    -1,   153,
     154,   155,     5,     6,     7,     8,     9,    10,    -1,    12,
      13,    14,    15,    16,    17,    -1,    19,    20,    21,    22,
      23,    -1,    -1,   153,   154,   155,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   153,   154,   155,     5,     6,     7,     8,     9,
      10,    -1,    12,    13,    14,    15,    16,    17,    -1,    19,
      20,    21,    22,    23,    -1,   153,   154,   155,     5,     6,
       7,     8,     9,    10,    -1,    12,    13,    14,    15,    16,
      17,    -1,    19,    20,    21,    22,    23,    -1,   153,   154,
     155,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   153,   154,   155,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   153,   154,   155,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   150,    -1,    -1,
      -1,   154,   155,     5,     6,     7,     8,     9,    10,    -1,
      12,    13,    14,    15,    16,    17,    -1,    19,    20,    21,
      22,    23,     5,     6,     7,     8,     9,    10,    -1,    12,
      13,    14,    15,    16,    17,    -1,    19,    20,    21,    22,
      23,    -1,    -1,   153,   154,   155,     5,     6,     7,     8,
       9,    10,    -1,    12,    13,    14,    15,    16,    17,    -1,
      19,    20,    21,    22,    23,    -1,   153,   154,   155,     5,
       6,     7,     8,     9,    10,    -1,    12,    13,    14,    15,
      16,    17,    -1,    19,    20,    21,    22,    23,     5,     6,
       7,     8,     9,    10,    -1,    12,    13,    14,    15,    16,
      17,    -1,    19,    20,    21,    22,    23,     5,     6,     7,
       8,     9,    10,    -1,    12,    13,    14,    15,    16,    17,
      -1,    19,    20,    21,    22,    23,     5,     6,     7,     8,
       9,    10,    -1,    12,    13,    14,    15,    16,    17,    -1,
      19,    20,    21,    22,    23,    -1,    -1,    -1,    -1,    -1,
      -1,   153,   154,   155,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     153,   154,   155,     5,     6,     7,     8,     9,    10,    -1,
      12,    13,    14,    15,    16,    17,    -1,    19,    20,    21,
      22,    23,    -1,    -1,   153,   154,   155,     5,     6,     7,
       8,     9,    10,    -1,    12,    13,    14,    15,    16,    17,
      -1,    19,    20,    21,    22,    23,    -1,   153,   154,   155,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   153,   154,   155,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   153,   154,   155,    12,    13,
      14,    15,    16,    17,    -1,    19,    20,    21,    22,    23,
      -1,    -1,    -1,    -1,   153,   154,   155,     5,     6,     7,
       8,     9,    10,    -1,    12,    13,    14,    15,    16,    17,
      -1,    19,    20,    21,    22,    23,     5,     6,     7,     8,
       9,    10,    -1,    12,    13,    14,    15,    16,    17,    -1,
      19,    20,    21,    22,    23,    -1,    -1,    -1,    -1,    -1,
      -1,   153,   154,   155,     5,     6,     7,     8,     9,    10,
      -1,    12,    13,    14,    15,    16,    17,    -1,    19,    20,
      21,    22,    23,    -1,    -1,   153,   154,   155,     5,     6,
       7,     8,     9,    10,    -1,    12,    13,    14,    15,    16,
      17,    -1,    19,    20,    21,    22,    23,     5,     6,     7,
       8,     9,    10,    -1,    12,    13,    14,    15,    16,    17,
      -1,    19,    20,    21,    22,    23,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     154,   155,    -1,    -1,     4,    -1,    -1,    -1,    -1,    -1,
      -1,    11,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    19,
      20,    21,    -1,    -1,    -1,   153,   154,   155,    -1,    -1,
      -1,    -1,    -1,    33,    34,    -1,    -1,    -1,    38,    39,
      40,    41,    -1,    -1,   153,   154,   155,    -1,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    58,    -1,
      -1,    61,    62,    63,    -1,    65,    66,    67,    68,    69,
      -1,    71,   153,   154,   155,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    19,    20,    21,    -1,    23,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   153,   154,   155,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    44,    45,
      46,    47,    -1,    -1,    -1,    -1,   154,   155,   118,    -1,
      -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,   128,   129,
      -1,    11,    -1,    -1,   134,    -1,    -1,    -1,    -1,    19,
      20,    -1,    -1,   143,   144,    -1,   146,   147,    -1,    -1,
     150,   151,   152,    33,    34,    -1,    -1,    -1,    38,    39,
      40,    41,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    58,    -1,
      -1,    61,    62,    63,    -1,    65,    66,    67,    68,    69,
      -1,    71,   128,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    19,    20,    21,    -1,   143,   144,    -1,
     146,   147,    -1,    -1,   150,   151,   152,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    44,    45,
      46,    47,    -1,    -1,    -1,    -1,    -1,    -1,   118,    -1,
      -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,   128,   129,
      -1,    11,    -1,    -1,   134,    -1,    -1,    -1,    -1,    19,
      20,    -1,    -1,   143,   144,    -1,   146,   147,    -1,    -1,
     150,   151,   152,    33,    34,    -1,    -1,    -1,    38,    39,
      40,    41,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    58,    -1,
      -1,    61,    62,    63,    -1,    65,    66,    67,    68,    69,
      -1,    71,   128,   129,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   143,   144,    -1,
     146,   147,    -1,    -1,   150,   151,   152,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,    -1,
      -1,    -1,    -1,    -1,    -1,     5,    -1,    -1,   128,   129,
      -1,    11,    -1,    -1,   134,    -1,    -1,    -1,    -1,    19,
      20,    21,    -1,   143,   144,    -1,   146,   147,    -1,    -1,
     150,   151,   152,    33,    34,    -1,    -1,    -1,    38,    39,
      40,    41,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    58,    -1,
      -1,    61,    62,    63,    -1,    65,    66,    67,    68,    69,
      -1,    71,    -1,    -1,    34,    -1,    -1,    -1,    38,    39,
      40,    41,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    58,    -1,
      -1,    61,    62,    63,    -1,    -1,    66,    67,    68,    69,
      -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,   118,    -1,
      -1,    -1,    -1,    -1,    -1,     5,    -1,    -1,   128,   129,
      -1,    11,    -1,    -1,   134,    -1,    -1,    -1,    -1,    19,
      20,    -1,    22,   143,   144,    -1,   146,   147,    -1,    -1,
     150,   151,   152,    33,    34,    -1,    -1,    -1,    38,    39,
      40,    41,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    58,    -1,
      -1,    61,    62,    63,    -1,    65,    66,    67,    68,    69,
     150,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,    -1,
      -1,    -1,    -1,    -1,    -1,     5,    -1,    -1,   128,   129,
      -1,    11,    -1,    -1,   134,    -1,    -1,    -1,    -1,    19,
      20,    21,    -1,   143,   144,    -1,   146,   147,    -1,    -1,
     150,   151,   152,    33,    34,    -1,    -1,    -1,    38,    39,
      40,    41,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    58,    -1,
      -1,    61,    62,    63,    -1,    65,    66,    67,    68,    69,
      -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,    -1,
      -1,    -1,    -1,    -1,    -1,     5,    -1,    -1,   128,   129,
      -1,    11,    -1,    -1,   134,    -1,    -1,    -1,    -1,    19,
      20,    -1,    -1,   143,   144,    -1,   146,   147,    -1,    -1,
     150,   151,   152,    33,    34,    -1,    -1,    -1,    38,    39,
      40,    41,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    58,    -1,
      -1,    61,    62,    63,    -1,    65,    66,    67,    68,    69,
      -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,    -1,
      -1,    -1,    -1,    -1,    -1,     5,    -1,    -1,   128,   129,
      -1,    11,    -1,    -1,   134,    -1,    -1,    -1,    -1,    19,
      20,    -1,    -1,   143,   144,    -1,   146,   147,    -1,    -1,
     150,   151,   152,    33,    34,    -1,    -1,    -1,    38,    39,
      40,    41,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    58,    -1,
      -1,    61,    62,    63,    -1,    65,    66,    67,    68,    69,
      -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,    -1,
      -1,    -1,    -1,    -1,    -1,     5,    -1,    -1,   128,   129,
      -1,    11,    -1,    -1,   134,    -1,    -1,    -1,    -1,    19,
      20,    -1,    -1,   143,   144,    -1,   146,   147,    -1,    -1,
     150,   151,   152,    33,    34,    -1,    -1,    -1,    38,    39,
      40,    41,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    58,    -1,
      -1,    61,    62,    63,    -1,    65,    66,    67,    68,    69,
      -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,   129,
      -1,    11,    -1,    -1,   134,    -1,    -1,    -1,    -1,    19,
      20,    -1,    -1,   143,   144,    -1,   146,   147,    -1,    -1,
     150,   151,   152,    33,    34,    -1,    -1,    -1,    38,    39,
      40,    41,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    58,    -1,
      -1,    61,    62,    63,    -1,    65,    66,    67,    68,    69,
      -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   118,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,   129,
      -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   143,   144,    11,   146,   147,    -1,    -1,
     150,   151,   152,    19,    20,    21,   156,    23,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    33,    34,    -1,
      -1,    -1,    38,    39,    40,    41,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    -1,
      56,    57,    58,    -1,    -1,    61,    62,    63,    -1,    65,
      66,    67,    68,    69,    -1,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   128,   129,    -1,    11,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    19,    20,    21,    -1,   143,   144,    -1,
     146,   147,    -1,    -1,   150,   151,   152,    33,    34,    -1,
      -1,    -1,    38,    39,    40,    41,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    -1,
      56,    57,    58,    -1,    -1,    61,    62,    63,    -1,    65,
      66,    67,    68,    69,    -1,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   128,   129,    -1,    11,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    19,    20,    21,    -1,   143,   144,    -1,
     146,   147,    -1,    -1,   150,   151,   152,    33,    34,    -1,
      -1,    -1,    38,    39,    40,    41,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    -1,
      56,    57,    58,    -1,    -1,    61,    62,    63,    -1,    65,
      66,    67,    68,    69,    -1,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   128,   129,    -1,    11,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    19,    20,    21,    -1,   143,   144,    -1,
     146,   147,    -1,    -1,   150,   151,   152,    33,    34,    -1,
      -1,    -1,    38,    39,    40,    41,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    -1,
      56,    57,    58,    -1,    -1,    61,    62,    63,    -1,    65,
      66,    67,    68,    69,    -1,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   128,   129,    -1,    11,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    19,    20,    21,    -1,   143,   144,    -1,
     146,   147,    -1,    -1,   150,   151,   152,    33,    34,    -1,
      -1,    -1,    38,    39,    40,    41,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    -1,
      56,    57,    58,    -1,    -1,    61,    62,    63,    -1,    65,
      66,    67,    68,    69,    -1,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   128,   129,    -1,    11,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    19,    20,    21,    -1,   143,   144,    -1,
     146,   147,    -1,    -1,   150,   151,   152,    33,    34,    -1,
      -1,    -1,    38,    39,    40,    41,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    -1,
      56,    57,    58,    -1,    -1,    61,    62,    63,    -1,    65,
      66,    67,    68,    69,    -1,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   128,   129,    -1,    11,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    19,    20,    21,    -1,   143,   144,    -1,
     146,   147,    -1,    -1,   150,   151,   152,    33,    34,    -1,
      -1,    -1,    38,    39,    40,    41,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    -1,
      56,    57,    58,    -1,    -1,    61,    62,    63,    -1,    65,
      66,    67,    68,    69,    -1,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   128,   129,    -1,    11,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    19,    20,    -1,    -1,   143,   144,    -1,
     146,   147,    -1,    -1,   150,   151,   152,    33,    34,    -1,
      -1,    -1,    38,    39,    40,    41,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    -1,
      56,    57,    58,    -1,    -1,    61,    62,    63,    -1,    65,
      66,    67,    68,    69,    -1,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   128,   129,    -1,    11,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    19,    20,    -1,    -1,   143,   144,    -1,
     146,   147,    -1,    -1,   150,   151,   152,    33,    34,    -1,
      -1,    -1,    38,    39,    40,    41,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    -1,
      56,    57,    58,    -1,    -1,    61,    62,    63,    -1,    65,
      66,    67,    68,    69,    -1,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   128,   129,    -1,    11,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    19,    20,    -1,    -1,   143,   144,    -1,
     146,   147,    -1,    -1,   150,   151,   152,    33,    34,    -1,
      -1,    -1,    38,    39,    40,    41,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    -1,
      56,    57,    58,    -1,    -1,    61,    62,    63,    -1,    65,
      66,    67,    68,    69,    -1,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   128,   129,    -1,    11,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    19,    20,    -1,    -1,   143,   144,    -1,
     146,   147,    -1,    -1,   150,   151,   152,    33,    34,    -1,
      -1,    -1,    38,    39,    40,    41,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    -1,
      56,    57,    58,    -1,    -1,    61,    62,    63,    -1,    65,
      66,    67,    68,    69,    -1,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   128,   129,    -1,    11,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    19,    20,    -1,    -1,   143,   144,    -1,
     146,   147,    -1,    -1,   150,   151,   152,    33,    34,    -1,
      -1,    -1,    38,    39,    40,    41,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    -1,
      56,    57,    58,    -1,    -1,    61,    62,    63,    -1,    65,
      66,    67,    68,    69,    -1,    71,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   118,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   128,   129,    -1,    11,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    19,    20,    -1,    -1,   143,   144,    -1,
     146,   147,    -1,    -1,   150,   151,   152,    33,    34,    -1,
      32,    -1,    38,    39,    40,    41,    -1,    -1,    -1,    -1,
      -1,    -1,    48,    49,    50,    51,    52,    53,    54,    -1,
      56,    57,    58,    55,    -1,    61,    62,    63,    -1,    65,
      66,    67,    68,    69,    -1,    71,    -1,    -1,    70,    -1,
      -1,    -1,    -1,    75,    76,    -1,    -1,    -1,    80,    81,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    89,    90,    91,
      92,    93,    94,    95,    96,    97,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   108,    -1,    -1,    -1,
      -1,   113,   118,    -1,    -1,    -1,    -1,    -1,   120,    -1,
     122,    -1,   128,   129,    -1,    -1,    -1,    -1,   134,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   143,   144,    -1,
     146,   147,    -1,    -1,   150,   151,   152,    25,   150,   151,
      28,    -1,    30,    31,    32,    -1,    -1,    35,    36,    37,
      -1,    -1,    -1,    -1,    42,    43,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    55,    -1,    -1,
      -1,    59,    60,    -1,    -1,    -1,    64,    -1,    -1,    -1,
      -1,    -1,    70,    -1,    72,    73,    -1,    75,    -1,    77,
      78,    79,    80,    81,    82,    83,    84,    85,    86,    87,
      88,    89,    90,    91,    92,    93,    94,    95,    96,    97,
      98,    99,   100,   101,    -1,   103,   104,   105,    -1,   107,
     108,   109,    -1,   111,    -1,   113,    -1,    -1,   116,   117,
      -1,    -1,   120,   121,   122,    -1,    -1,    -1,    -1,   127,
      -1,    -1,    -1,   131,   132,   133,   134,   135,   136,   137,
     138,   139,   140,   141,   142,    -1,    -1,   145,    -1,    -1,
     148,   149,   150,   151,    33,    34,    -1,    -1,    -1,    38,
      39,    40,    41,    -1,    -1,    -1,    -1,    -1,    -1,    48,
      49,    50,    51,    52,    53,    54,    -1,    56,    57,    58,
      -1,    -1,    61,    62,    63,    -1,    65,    66,    67,    68,
      69,    -1,    71,    33,    34,    -1,    -1,    -1,    38,    39,
      40,    41,    -1,    -1,    -1,    -1,    -1,    -1,    48,    49,
      50,    51,    52,    53,    54,    -1,    56,    57,    58,    -1,
      -1,    61,    62,    63,    -1,    65,    66,    67,    68,    69,
      -1,    71,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,
     129,    -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   143,   144,    -1,   146,   147,    -1,
      -1,   150,   151,   152,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   128,   129,
      -1,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   143,   144,    -1,   146,   147,    -1,    -1,
     150,   151,   152
};

/* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
   symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   160,     0,     1,    25,    28,    30,    31,    32,    35,
      36,    37,    42,    43,    55,    59,    60,    64,    70,    72,
      73,    75,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   103,   104,   105,
     107,   108,   109,   111,   113,   116,   117,   120,   121,   122,
     127,   130,   131,   132,   133,   134,   135,   136,   137,   138,
     139,   140,   141,   142,   145,   148,   149,   150,   151,   156,
     161,   162,   163,   165,   166,   173,   185,   186,   189,   190,
     195,   196,   197,   198,   199,   201,   202,   209,   211,   214,
     217,   218,   253,   263,   264,   269,   270,   271,   272,   273,
     275,   280,   282,   283,   285,   288,   289,   292,   293,   301,
     303,   314,   321,   143,   168,   150,   167,   167,   152,    11,
      19,    20,    33,    34,    38,    39,    40,    41,    48,    49,
      50,    51,    52,    53,    54,    56,    57,    58,    61,    62,
      63,    65,    66,    67,    68,    69,    71,   118,   128,   129,
     134,   143,   144,   146,   147,   151,   152,   235,   236,   238,
     239,   243,   245,   246,   247,   248,   253,   254,   255,   262,
     274,   152,   156,   235,   259,   152,   178,   179,   152,   235,
     258,     4,   156,   182,   156,   182,   259,   152,   150,   156,
     278,   151,   152,   235,   151,   277,   150,   279,   150,   278,
     235,   152,   152,   297,   150,   151,   152,   298,   152,   152,
     182,   182,    18,    22,   205,   253,   152,   177,   112,   123,
     116,   150,   210,   162,    72,    73,   174,   230,   150,   151,
     223,   278,   164,    24,    29,   164,   164,   150,   152,   207,
     208,   150,   187,   192,     3,   206,   150,   191,   205,     3,
     206,     3,   206,     3,   206,   150,   203,   204,   205,     3,
       4,    31,   183,   184,   212,   226,    21,   152,   215,    21,
     152,   221,     4,   281,   150,   278,   278,   278,     5,   115,
     158,   284,     3,   152,   286,   150,   259,   297,    19,    20,
      21,    44,    45,    46,    47,   150,   152,   243,   254,   295,
     297,   302,   304,   305,   306,    76,    89,    97,   253,   280,
     164,   152,   169,   235,   235,   235,   237,   152,   152,   152,
     237,   235,   237,   152,   152,   152,   152,   152,   152,   152,
     152,   152,   152,   152,   152,   235,   152,   152,   152,   152,
     152,   152,   235,   236,   238,   262,   311,   312,   152,   235,
     238,     5,     6,     7,     8,     9,    10,    12,    13,    14,
      15,    16,    17,    19,    20,    21,    22,    23,   154,   155,
     240,   235,   158,   152,   257,   152,   152,   150,   143,   256,
     257,     3,     4,   153,   235,   180,   253,     3,   235,     4,
     150,   200,   200,   235,   235,   153,   206,   253,   276,   322,
     243,   247,   253,   282,   315,   316,   317,    21,    23,   152,
     253,   299,   300,   304,   305,   311,   151,   238,   253,   253,
     316,   318,   253,   320,   200,    34,    38,    39,    40,    41,
      48,    49,    50,    51,    52,    53,    54,    56,    57,    58,
      61,    62,    63,    66,    67,    68,    69,    71,   150,   175,
     176,   150,   253,   181,   235,     3,     3,   182,     5,    21,
     224,   235,   153,   163,   167,     5,   207,     3,    22,   152,
     156,   206,     3,    22,   191,   205,   152,   231,   191,   253,
     205,   150,   150,   205,   231,     3,   203,    25,    74,    99,
     101,   102,   103,   106,   107,   109,   110,   227,   228,   230,
       4,   167,     3,   213,   150,   151,   152,   150,   216,   224,
     151,   152,   222,   235,   223,   164,   231,     3,   235,   235,
     253,   151,    21,   235,   287,   290,   291,   235,   235,   235,
     235,     5,   304,   152,   236,   243,   254,   294,   307,   308,
     309,     3,   296,    18,    21,    22,    23,   306,   304,   152,
     152,    21,   150,   153,   171,   172,    26,   153,     3,   153,
     237,   237,   235,   153,   153,   153,   235,   235,   235,   235,
     235,   235,   235,   235,   235,   235,   235,   235,   153,   237,
     237,   237,   237,   237,   238,   311,   312,     3,     3,   119,
       3,   237,   153,     3,     5,   235,   242,   235,   235,   235,
     235,   235,   235,   235,   235,   235,   235,   235,   235,   235,
     235,     5,    22,   235,   241,   235,     5,   235,     5,   235,
     243,   258,   244,   249,   249,   235,   235,   153,     3,   153,
     231,   179,   153,     3,   153,     3,     5,   152,     3,   319,
     304,    21,    23,   152,   235,     3,   153,   153,   153,   153,
     153,     3,   319,     3,   153,     3,    22,     3,   153,   152,
     150,   278,   224,   169,   235,   153,   208,   188,   193,   194,
     239,   235,   150,   150,   192,   188,   206,     4,    21,   232,
     233,   234,   235,   206,   231,   231,   206,   204,   231,   152,
       3,     4,   169,   150,   231,   216,     5,   224,   153,     3,
     225,    21,   153,   231,    27,   150,   265,   266,   281,   151,
     153,     3,   235,   153,   307,   240,   310,   310,     3,   310,
     307,   304,   304,   304,   304,   304,   315,   318,     3,   153,
     152,   170,   259,   235,   153,   153,   153,   153,   153,   153,
     153,   153,   153,   153,   153,   153,   153,   153,   153,   153,
     153,   153,   153,   153,     3,     3,     3,   235,   312,   235,
     312,   235,   312,   153,   238,   235,   235,   235,   235,   235,
       4,     4,   235,   250,   251,   252,   153,   153,   253,   150,
      76,   235,   235,   250,   114,   316,   153,   153,   235,   252,
     158,   300,   304,   316,   153,   253,   175,   235,   181,    22,
       3,    19,    20,    21,   157,   254,   153,    22,    22,   191,
     235,     3,   153,     4,   253,    21,   219,   219,   203,   124,
     125,   126,   229,   228,     4,   231,   219,   153,   224,   225,
     150,   153,     4,   115,     3,   231,   291,     3,   153,   240,
     309,   319,   319,   172,   153,   171,   150,   313,   313,   313,
     153,   258,     4,   235,     4,     3,   257,   231,   153,     3,
     153,     5,   153,   282,   153,   188,   193,   193,   193,   193,
     188,   233,   234,   235,   152,   220,   235,   153,   219,   260,
     261,   225,   224,   150,   156,   267,   268,   150,   266,   231,
     313,   310,   153,   153,   153,     5,   153,   153,   153,   153,
     235,     4,   235,   251,     3,   235,   253,    22,    21,   260,
       5,   115,     3,   153,   235,   235,     4,   235,     3,   153,
     235,   150,   268,     3,   235,   235,   235,     3,   235
};

#define yyerrok		(yyerrstatus = 0)
#define yyclearin	(yychar = YYEMPTY)
#define YYEMPTY		(-2)
#define YYEOF		0

#define YYACCEPT	goto yyacceptlab
#define YYABORT		goto yyabortlab
#define YYERROR		goto yyerrorlab


/* Like YYERROR except do call yyerror.  This remains here temporarily
   to ease the transition to the new meaning of YYERROR, for GCC.
   Once GCC version 2 has supplanted version 1, this can go.  However,
   YYFAIL appears to be in use.  Nevertheless, it is formally deprecated
   in Bison 2.4.2's NEWS entry, where a plan to phase it out is
   discussed.  */

#define YYFAIL		goto yyerrlab
#if defined YYFAIL
  /* This is here to suppress warnings from the GCC cpp's
     -Wunused-macros.  Normally we don't worry about that warning, but
     some users do, and we want to make it easy for users to remove
     YYFAIL uses, which will produce warnings from Bison 2.5.  */
#endif

#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)					\
do								\
  if (yychar == YYEMPTY && yylen == 1)				\
    {								\
      yychar = (Token);						\
      yylval = (Value);						\
      YYPOPSTACK (1);						\
      goto yybackup;						\
    }								\
  else								\
    {								\
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;							\
    }								\
while (YYID (0))


#define YYTERROR	1
#define YYERRCODE	256


/* YYLLOC_DEFAULT -- Set CURRENT to span from RHS[1] to RHS[N].
   If N is 0, then set CURRENT to the empty location which ends
   the previous symbol: RHS[0] (always defined).  */

#define YYRHSLOC(Rhs, K) ((Rhs)[K])
#ifndef YYLLOC_DEFAULT
# define YYLLOC_DEFAULT(Current, Rhs, N)				\
    do									\
      if (YYID (N))                                                    \
	{								\
	  (Current).first_line   = YYRHSLOC (Rhs, 1).first_line;	\
	  (Current).first_column = YYRHSLOC (Rhs, 1).first_column;	\
	  (Current).last_line    = YYRHSLOC (Rhs, N).last_line;		\
	  (Current).last_column  = YYRHSLOC (Rhs, N).last_column;	\
	}								\
      else								\
	{								\
	  (Current).first_line   = (Current).last_line   =		\
	    YYRHSLOC (Rhs, 0).last_line;				\
	  (Current).first_column = (Current).last_column =		\
	    YYRHSLOC (Rhs, 0).last_column;				\
	}								\
    while (YYID (0))
#endif


/* This macro is provided for backward compatibility. */

#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


/* YYLEX -- calling `yylex' with the right arguments.  */

#ifdef YYLEX_PARAM
# define YYLEX yylex (YYLEX_PARAM)
#else
# define YYLEX yylex ()
#endif

/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)			\
do {						\
  if (yydebug)					\
    YYFPRINTF Args;				\
} while (YYID (0))

# define YY_SYMBOL_PRINT(Title, Type, Value, Location)			  \
do {									  \
  if (yydebug)								  \
    {									  \
      YYFPRINTF (stderr, "%s ", Title);					  \
      yy_symbol_print (stderr,						  \
		  Type, Value); \
      YYFPRINTF (stderr, "\n");						  \
    }									  \
} while (YYID (0))


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_value_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# else
  YYUSE (yyoutput);
# endif
  switch (yytype)
    {
      default:
	break;
    }
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
#else
static void
yy_symbol_print (yyoutput, yytype, yyvaluep)
    FILE *yyoutput;
    int yytype;
    YYSTYPE const * const yyvaluep;
#endif
{
  if (yytype < YYNTOKENS)
    YYFPRINTF (yyoutput, "token %s (", yytname[yytype]);
  else
    YYFPRINTF (yyoutput, "nterm %s (", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
#else
static void
yy_stack_print (yybottom, yytop)
    yytype_int16 *yybottom;
    yytype_int16 *yytop;
#endif
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)				\
do {								\
  if (yydebug)							\
    yy_stack_print ((Bottom), (Top));				\
} while (YYID (0))


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yy_reduce_print (YYSTYPE *yyvsp, int yyrule)
#else
static void
yy_reduce_print (yyvsp, yyrule)
    YYSTYPE *yyvsp;
    int yyrule;
#endif
{
  int yynrhs = yyr2[yyrule];
  int yyi;
  unsigned long int yylno = yyrline[yyrule];
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
	     yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr, yyrhs[yyprhs[yyrule] + yyi],
		       &(yyvsp[(yyi + 1) - (yynrhs)])
		       		       );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)		\
do {					\
  if (yydebug)				\
    yy_reduce_print (yyvsp, Rule); \
} while (YYID (0))

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef	YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static YYSIZE_T
yystrlen (const char *yystr)
#else
static YYSIZE_T
yystrlen (yystr)
    const char *yystr;
#endif
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static char *
yystpcpy (char *yydest, const char *yysrc)
#else
static char *
yystpcpy (yydest, yysrc)
    char *yydest;
    const char *yysrc;
#endif
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
	switch (*++yyp)
	  {
	  case '\'':
	  case ',':
	    goto do_not_strip_quotes;

	  case '\\':
	    if (*++yyp != '\\')
	      goto do_not_strip_quotes;
	    /* Fall through.  */
	  default:
	    if (yyres)
	      yyres[yyn] = *yyp;
	    yyn++;
	    break;

	  case '"':
	    if (yyres)
	      yyres[yyn] = '\0';
	    return yyn;
	  }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (0, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  YYSIZE_T yysize1;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = 0;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - Assume YYFAIL is not used.  It's too flawed to consider.  See
       <http://lists.gnu.org/archive/html/bison-patches/2009-12/msg00024.html>
       for details.  YYERROR is fine as it does not invoke this
       function.
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                yysize1 = yysize + yytnamerr (0, yytname[yyx]);
                if (! (yysize <= yysize1
                       && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                  return 2;
                yysize = yysize1;
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  yysize1 = yysize + yystrlen (yyformat);
  if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
    return 2;
  yysize = yysize1;

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

/*ARGSUSED*/
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
#else
static void
yydestruct (yymsg, yytype, yyvaluep)
    const char *yymsg;
    int yytype;
    YYSTYPE *yyvaluep;
#endif
{
  YYUSE (yyvaluep);

  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  switch (yytype)
    {

      default:
	break;
    }
}


/* Prevent warnings from -Wmissing-prototypes.  */
#ifdef YYPARSE_PARAM
#if defined __STDC__ || defined __cplusplus
int yyparse (void *YYPARSE_PARAM);
#else
int yyparse ();
#endif
#else /* ! YYPARSE_PARAM */
#if defined __STDC__ || defined __cplusplus
int yyparse (void);
#else
int yyparse ();
#endif
#endif /* ! YYPARSE_PARAM */


/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;

/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

#ifdef YYPARSE_PARAM
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void *YYPARSE_PARAM)
#else
int
yyparse (YYPARSE_PARAM)
    void *YYPARSE_PARAM;
#endif
#else /* ! YYPARSE_PARAM */
#if (defined __STDC__ || defined __C99__FUNC__ \
     || defined __cplusplus || defined _MSC_VER)
int
yyparse (void)
#else
int
yyparse ()

#endif
#endif
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       `yyss': related to states.
       `yyvs': related to semantic values.

       Refer to the stacks thru separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yytoken = 0;
  yyss = yyssa;
  yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */

  /* Initialize stack pointers.
     Waste one element of value and location stack
     so that they stay on the same level as the state stack.
     The wasted elements are never initialized.  */
  yyssp = yyss;
  yyvsp = yyvs;

  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
	/* Give user a chance to reallocate the stack.  Use copies of
	   these so that the &'s don't force the real ones into
	   memory.  */
	YYSTYPE *yyvs1 = yyvs;
	yytype_int16 *yyss1 = yyss;

	/* Each stack pointer address is followed by the size of the
	   data in use in that stack, in bytes.  This used to be a
	   conditional around just the two extra args, but that might
	   be undefined if yyoverflow is a macro.  */
	yyoverflow (YY_("memory exhausted"),
		    &yyss1, yysize * sizeof (*yyssp),
		    &yyvs1, yysize * sizeof (*yyvsp),
		    &yystacksize);

	yyss = yyss1;
	yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
	goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
	yystacksize = YYMAXDEPTH;

      {
	yytype_int16 *yyss1 = yyss;
	union yyalloc *yyptr =
	  (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
	if (! yyptr)
	  goto yyexhaustedlab;
	YYSTACK_RELOCATE (yyss_alloc, yyss);
	YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
	if (yyss1 != yyssa)
	  YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
		  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
	YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = YYLEX;
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  *++yyvsp = yylval;

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     `$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 7:

/* Line 1806 of yacc.c  */
#line 316 "fortran.y"
    {yyerrok;yyclearin;}
    break;

  case 12:

/* Line 1806 of yacc.c  */
#line 326 "fortran.y"
    {
            if (inmoduledeclare == 0 )
            {
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_curinclude,pos_end-pos_curinclude);
            }
        }
    break;

  case 15:

/* Line 1806 of yacc.c  */
#line 336 "fortran.y"
    { pos_cur = setposcur(); }
    break;

  case 16:

/* Line 1806 of yacc.c  */
#line 338 "fortran.y"
    { isrecursive = 0; }
    break;

  case 17:

/* Line 1806 of yacc.c  */
#line 339 "fortran.y"
    { isrecursive = 1; }
    break;

  case 18:

/* Line 1806 of yacc.c  */
#line 342 "fortran.y"
    {
                      if ( couldaddvariable == 1)
                      {
                      /* open param file                                      */
                      if ( firstpass == 0 )
                      {
                         sprintf(ligne,"%s/ParamFile%s.h",nomdir,(yyvsp[(3) - (4)].nac));
                         paramout=fopen(ligne,"w");
                         if ( retour77 == 0 ) fprintf(paramout,"!\n");
                         else fprintf(paramout,"C\n");

                      }
                      Add_SubroutineArgument_Var_1((yyvsp[(4) - (4)].l));
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
    break;

  case 19:

/* Line 1806 of yacc.c  */
#line 370 "fortran.y"
    {
                      /* open param file                                      */
                      if ( firstpass == 0 )
                      {
                         sprintf(ligne,"%s/ParamFile%s.h",nomdir,(yyvsp[(2) - (2)].nac));
                         paramout=fopen(ligne,"w");
                         if ( retour77 == 0 ) fprintf(paramout,"!\n");
                         else fprintf(paramout,"C\n");

                      }
                      strcpy(subroutinename,(yyvsp[(2) - (2)].nac));
                      /* Common case                                          */
                      insubroutinedeclare = 1;
                      /* in the second step we should write the head of       */
                      /*    the subroutine sub_loop_<subroutinename>          */
                      writeheadnewsub_0(1);
                   }
    break;

  case 20:

/* Line 1806 of yacc.c  */
#line 388 "fortran.y"
    {
                      /* open param file                                      */
                      if ( firstpass == 0 )
                      {
                         sprintf(ligne,"%s/ParamFile%s.h",nomdir,(yyvsp[(2) - (5)].nac));
                         paramout=fopen(ligne,"w");
                         if ( retour77 == 0 ) fprintf(paramout,"!\n");
                         else fprintf(paramout,"C\n");
                      }
                      strcpy(subroutinename,(yyvsp[(2) - (5)].nac));
                      if ( inmodulemeet == 1 )
                      {
                         insubroutinedeclare = 1;
                         /* we should to list of the subroutine argument the  */
                         /*    name of the function which has to be defined   */
                         Add_SubroutineArgument_Var_1((yyvsp[(3) - (5)].l));
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
                            Add_SubroutineArgument_Var_1((yyvsp[(3) - (5)].l));
                            strcpy(DeclType,"");
                            Add_FunctionType_Var_1((yyvsp[(2) - (5)].nac));
                            writeheadnewsub_0(2);
                      }
                   }
    break;

  case 21:

/* Line 1806 of yacc.c  */
#line 421 "fortran.y"
    {
                      /* open param file                                      */
                      if ( firstpass == 0 )
                      {
                         sprintf(ligne,"%s/ParamFile%s.h",nomdir,(yyvsp[(2) - (3)].nac));
                         paramout=fopen(ligne,"w");
                         if ( retour77 == 0 ) fprintf(paramout,"!\n");
                         else fprintf(paramout,"C\n");
                      }
                      strcpy(subroutinename,(yyvsp[(2) - (3)].nac));
                      if ( inmodulemeet == 1 )
                      {
                         insubroutinedeclare = 1;
                         /* we should to list of the subroutine argument the  */
                         /*    name of the function which has to be defined   */
                         Add_SubroutineArgument_Var_1((yyvsp[(3) - (3)].l));
                         strcpy(DeclType,"");
                         Add_FunctionType_Var_1((yyvsp[(2) - (3)].nac));
                         /* in the second step we should write the head of    */
                         /*    the subroutine sub_loop_<subroutinename>       */
                         writeheadnewsub_0(2);
                      }
                      else
                      {
                            insubroutinedeclare = 1;
                            /* we should to list of the subroutine argument   */
                            /* name of the function which has to be defined   */
                            Add_SubroutineArgument_Var_1((yyvsp[(3) - (3)].l));
                            strcpy(DeclType,"");
                            Add_FunctionType_Var_1((yyvsp[(2) - (3)].nac));
                            writeheadnewsub_0(2);
                      }
                   }
    break;

  case 22:

/* Line 1806 of yacc.c  */
#line 455 "fortran.y"
    {
                      GlobalDeclaration = 0;
                      strcpy(curmodulename,(yyvsp[(2) - (2)].nac));
                      strcpy(subroutinename,"");
                      Add_NameOfModule_1((yyvsp[(2) - (2)].nac));
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
    break;

  case 23:

/* Line 1806 of yacc.c  */
#line 471 "fortran.y"
    { if ( couldaddvariable == 1 ) strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));strcpy(subroutinename,(yyvsp[(1) - (1)].nac)); }
    break;

  case 24:

/* Line 1806 of yacc.c  */
#line 473 "fortran.y"
    { if ( couldaddvariable == 1 ) Add_Include_1((yyvsp[(1) - (1)].nac)); }
    break;

  case 25:

/* Line 1806 of yacc.c  */
#line 475 "fortran.y"
    { if ( firstpass == 1 && couldaddvariable == 1 ) (yyval.l)=NULL; }
    break;

  case 26:

/* Line 1806 of yacc.c  */
#line 476 "fortran.y"
    { if ( firstpass == 1 && couldaddvariable == 1 ) (yyval.l)=NULL; }
    break;

  case 27:

/* Line 1806 of yacc.c  */
#line 477 "fortran.y"
    { if ( firstpass == 1 && couldaddvariable == 1 ) (yyval.l)=(yyvsp[(2) - (3)].l); }
    break;

  case 30:

/* Line 1806 of yacc.c  */
#line 481 "fortran.y"
    { if ( couldaddvariable == 1 ) Add_SubroutineArgument_Var_1((yyvsp[(2) - (3)].l)); }
    break;

  case 31:

/* Line 1806 of yacc.c  */
#line 484 "fortran.y"
    {
            if ( firstpass == 1  && couldaddvariable == 1)
            {
                strcpy(nameinttypenameback,nameinttypename);
                strcpy(nameinttypename,"");
                curvar = createvar((yyvsp[(1) - (1)].na),NULL);
                strcpy(nameinttypename,nameinttypenameback);
                curlistvar = insertvar(NULL,curvar);
                (yyval.l) = settype("",curlistvar);
            }
        }
    break;

  case 32:

/* Line 1806 of yacc.c  */
#line 496 "fortran.y"
    {
            if ( firstpass == 1  && couldaddvariable == 1)
            {
                strcpy(nameinttypenameback,nameinttypename);
                strcpy(nameinttypename,"");                      
                curvar = createvar((yyvsp[(3) - (3)].na),NULL);
                strcpy(nameinttypename,nameinttypenameback);                         
                (yyval.l) = insertvar((yyvsp[(1) - (3)].l),curvar);
            }
        }
    break;

  case 33:

/* Line 1806 of yacc.c  */
#line 507 "fortran.y"
    { if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].nac)); }
    break;

  case 34:

/* Line 1806 of yacc.c  */
#line 508 "fortran.y"
    { if ( couldaddvariable == 1 ) strcpy((yyval.na),"*");}
    break;

  case 35:

/* Line 1806 of yacc.c  */
#line 511 "fortran.y"
    {
            if ( VarTypepar == 1 )
            {
                couldaddvariable = 1 ;
                VarTypepar = 0;
            }
        }
    break;

  case 36:

/* Line 1806 of yacc.c  */
#line 519 "fortran.y"
    {
            if ( couldaddvariable == 1 )
            {
                VarType = 1;
                couldaddvariable = 0 ;
            }
        }
    break;

  case 37:

/* Line 1806 of yacc.c  */
#line 527 "fortran.y"
    {
            if ( VarType == 1 ) couldaddvariable = 1 ;
            VarType = 0;
            VarTypepar = 0;
        }
    break;

  case 39:

/* Line 1806 of yacc.c  */
#line 534 "fortran.y"
    {
            if ( couldaddvariable == 1 )
            {
                if ( insubroutinedeclare == 0 )    Add_GlobalParameter_Var_1((yyvsp[(3) - (4)].l));
                else                               Add_Parameter_Var_1((yyvsp[(3) - (4)].l));
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_cur_decl,pos_end-pos_cur_decl);
            }
            VariableIsParameter =  0 ;
        }
    break;

  case 40:

/* Line 1806 of yacc.c  */
#line 545 "fortran.y"
    {
            if ( couldaddvariable == 1 )
            {
                if ( insubroutinedeclare == 0 )     Add_GlobalParameter_Var_1((yyvsp[(2) - (2)].l));
                else                                Add_Parameter_Var_1((yyvsp[(2) - (2)].l));
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_cur_decl,pos_end-pos_cur_decl);
            }
            VariableIsParameter =  0 ;
        }
    break;

  case 42:

/* Line 1806 of yacc.c  */
#line 557 "fortran.y"
    {
            pos_end = setposcur();
            RemoveWordSET_0(fortran_out,pos_cursave,pos_end-pos_cursave);
        }
    break;

  case 44:

/* Line 1806 of yacc.c  */
#line 563 "fortran.y"
    {
            /* if the variable is a parameter we can suppose that is   */
            /*    value is the same on each grid. It is not useless to */
            /*    create a copy of it on each grid                     */
            if ( couldaddvariable == 1 )
            {
                Add_Globliste_1((yyvsp[(1) - (1)].l));
                /* if variableparamlists has been declared in a      */
                /*    subroutine                                     */
                if ( insubroutinedeclare == 1 )     Add_Dimension_Var_1((yyvsp[(1) - (1)].l));
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_curdimension,pos_end-pos_curdimension);
            }
            PublicDeclare = 0;
            PrivateDeclare = 0;
            ExternalDeclare = 0;
            strcpy(NamePrecision,"");
            c_star = 0;
            InitialValueGiven = 0 ;
            strcpy(IntentSpec,"");
            VariableIsParameter =  0 ;
            Allocatabledeclare = 0 ;
            Targetdeclare = 0 ;
            SaveDeclare = 0;
            pointerdeclare = 0;
            optionaldeclare = 0 ;
            dimsgiven=0;
            c_selectorgiven=0;
            strcpy(nameinttypename,"");
            strcpy(c_selectorname,"");
        }
    break;

  case 45:

/* Line 1806 of yacc.c  */
#line 595 "fortran.y"
    {
            if (firstpass == 0)
            {
                if ((yyvsp[(1) - (1)].lnn))
                {
                    removeglobfromlist(&((yyvsp[(1) - (1)].lnn)));
                    pos_end = setposcur();
                    RemoveWordSET_0(fortran_out,pos_cur,pos_end-pos_cur);
                    writelistpublic((yyvsp[(1) - (1)].lnn));
                }
            }
        }
    break;

  case 54:

/* Line 1806 of yacc.c  */
#line 616 "fortran.y"
    {
            /* we should remove the data declaration                */
            if ( couldaddvariable == 1 )
            {
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_curdata,pos_end-pos_curdata);
            }
            if ( couldaddvariable == 1 && aftercontainsdeclare == 1 )
            {
                if (firstpass == 0)
                {
                    ReWriteDataStatement_0(fortran_out);
                    pos_end = setposcur();
                }
            }
        }
    break;

  case 56:

/* Line 1806 of yacc.c  */
#line 635 "fortran.y"
    {
            PublicDeclare = 0 ;
            PrivateDeclare = 0 ;
        }
    break;

  case 94:

/* Line 1806 of yacc.c  */
#line 688 "fortran.y"
    {
            /* if the variable is a parameter we can suppose that is*/
            /*    value is the same on each grid. It is not useless */
            /*    to create a copy of it on each grid               */
            if ( couldaddvariable == 1 )
            {
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_cur_decl,pos_end-pos_cur_decl);
                ReWriteDeclarationAndAddTosubroutine_01((yyvsp[(1) - (2)].l));
                pos_cur_decl = setposcur();
                if ( firstpass == 0 && GlobalDeclaration == 0
                                    && insubroutinedeclare == 0 )
                {
                    sprintf(ligne,"\n#include \"Module_Declar_%s.h\"\n",curmodulename);
                    tofich(fortran_out,ligne,1);
                    sprintf (ligne, "Module_Declar_%s.h",curmodulename);
                    module_declar = open_for_write(ligne);
                    sprintf (ligne, " ");
                    tofich (module_declar, ligne,1);
                    GlobalDeclaration = 1 ;
                    pos_cur_decl = setposcur();
                }
                (yyval.l) = (yyvsp[(1) - (2)].l);
                Add_Globliste_1((yyvsp[(1) - (2)].l));
                                                  
                if ( insubroutinedeclare == 0 )     Add_GlobalParameter_Var_1((yyvsp[(1) - (2)].l));
                else
                {
                    if ( pointerdeclare == 1 )      Add_Pointer_Var_From_List_1((yyvsp[(1) - (2)].l));
                    Add_Parameter_Var_1((yyvsp[(1) - (2)].l));
                }
                /* If there are a SAVE declarations in module's subroutines we should */
                /*    remove it from the subroutines declaration and add it in the    */
                /*    global declarations                                             */
                if ( firstpass == 1 && aftercontainsdeclare == 1
                                    && SaveDeclare == 1 )
                {
                    if ( inmodulemeet == 0 ) Add_Save_Var_dcl_1((yyvsp[(1) - (2)].l));
                    else  Add_SubroutineDeclarationSave_Var_1((yyvsp[(1) - (2)].l));
                }
            }
            PublicDeclare = 0;
            PrivateDeclare = 0;
            ExternalDeclare = 0;
            strcpy(NamePrecision,"");
            c_star = 0;
            InitialValueGiven = 0 ;
            strcpy(IntentSpec,"");
            VariableIsParameter =  0 ;
            Allocatabledeclare = 0 ;
            Targetdeclare = 0 ;
            SaveDeclare = 0;
            pointerdeclare = 0;
            optionaldeclare = 0 ;
            dimsgiven=0;
            c_selectorgiven=0;
            strcpy(nameinttypename,"");
            strcpy(c_selectorname,"");
        }
    break;

  case 95:

/* Line 1806 of yacc.c  */
#line 748 "fortran.y"
    {
            /* open param file                                      */
            if ( firstpass == 0 )
            {
                sprintf(ligne,"%s/ParamFile%s.h",nomdir,(yyvsp[(2) - (3)].nac));
                paramout = fopen(ligne,"w");
                if ( retour77 == 0 )    fprintf(paramout,"!\n");
                else                    fprintf(paramout,"C\n");
            }
            strcpy(subroutinename,(yyvsp[(2) - (3)].nac));
            if ( inmodulemeet == 1 )
            {
                insubroutinedeclare = 1;
                /* we should to list of the subroutine argument the  */
                /*    name of the function which has to be defined   */
                Add_SubroutineArgument_Var_1((yyvsp[(3) - (3)].l));
                Add_FunctionType_Var_1((yyvsp[(2) - (3)].nac));
                /* in the second step we should write the head of    */
                /*    the subroutine sub_loop_<subroutinename>       */
                writeheadnewsub_0(2);
            }
            else
            {
                insubroutinedeclare = 1;
                /* we should to list of the subroutine argument the  */
                /*    name of the function which has to be defined   */
                Add_SubroutineArgument_Var_1((yyvsp[(3) - (3)].l));
                Add_FunctionType_Var_1((yyvsp[(2) - (3)].nac));
                /* in the second step we should write the head of    */
                /*    the subroutine sub_loop_<subroutinename>       */
                writeheadnewsub_0(2);
            }
            strcpy(nameinttypename,"");
        }
    break;

  case 96:

/* Line 1806 of yacc.c  */
#line 783 "fortran.y"
    { functiondeclarationisdone = 1; }
    break;

  case 97:

/* Line 1806 of yacc.c  */
#line 785 "fortran.y"
    { VariableIsParameter = 1; pos_curparameter = setposcur()-9; }
    break;

  case 98:

/* Line 1806 of yacc.c  */
#line 787 "fortran.y"
    { pos_curdata = setposcur()-strlen((yyvsp[(1) - (1)].nac)); Init_List_Data_Var(); }
    break;

  case 99:

/* Line 1806 of yacc.c  */
#line 790 "fortran.y"
    {
            if ( couldaddvariable == 1 )
            {
                createstringfromlistname(ligne,(yyvsp[(3) - (4)].lnn));
                if (firstpass == 1) Add_Data_Var_1(&List_Data_Var,(yyvsp[(1) - (4)].nac),ligne);
                else                Add_Data_Var_1(&List_Data_Var_Cur,(yyvsp[(1) - (4)].nac),ligne);
            }
        }
    break;

  case 100:

/* Line 1806 of yacc.c  */
#line 799 "fortran.y"
    {
            if ( couldaddvariable == 1 )
            {
                createstringfromlistname(ligne,(yyvsp[(5) - (6)].lnn));                      
                if (firstpass == 1) Add_Data_Var_1(&List_Data_Var,(yyvsp[(3) - (6)].nac),ligne);
                else                Add_Data_Var_1(&List_Data_Var_Cur,(yyvsp[(3) - (6)].nac),ligne);                      
            }
        }
    break;

  case 101:

/* Line 1806 of yacc.c  */
#line 808 "fortran.y"
    {
            if (firstpass == 1)  Add_Data_Var_Names_01(&List_Data_Var,(yyvsp[(1) - (4)].lnn),(yyvsp[(3) - (4)].lnn));
            else                 Add_Data_Var_Names_01(&List_Data_Var_Cur,(yyvsp[(1) - (4)].lnn),(yyvsp[(3) - (4)].lnn));
        }
    break;

  case 102:

/* Line 1806 of yacc.c  */
#line 814 "fortran.y"
    { if ( couldaddvariable == 1 )  (yyval.lnn) = Insertname(NULL,(yyvsp[(1) - (1)].na),0); }
    break;

  case 103:

/* Line 1806 of yacc.c  */
#line 815 "fortran.y"
    { if ( couldaddvariable == 1 )  (yyval.lnn) = Insertname((yyvsp[(3) - (3)].lnn),(yyvsp[(1) - (3)].na),1); }
    break;

  case 108:

/* Line 1806 of yacc.c  */
#line 823 "fortran.y"
    { pos_cursave = setposcur()-4; }
    break;

  case 110:

/* Line 1806 of yacc.c  */
#line 826 "fortran.y"
    { if ( couldaddvariable == 1 ) Add_Save_Var_1((yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].d)); }
    break;

  case 111:

/* Line 1806 of yacc.c  */
#line 829 "fortran.y"
    { (yyval.lnn)=Insertname(NULL,(yyvsp[(1) - (1)].nac),0); }
    break;

  case 112:

/* Line 1806 of yacc.c  */
#line 830 "fortran.y"
    { printf("INSTRUCTION NON TRAITEE : INITIALISATION DE DATA AVEC EXPRESSION\n"); exit(0); }
    break;

  case 113:

/* Line 1806 of yacc.c  */
#line 831 "fortran.y"
    { (yyval.lnn) = concat_listname((yyvsp[(1) - (3)].lnn),(yyvsp[(3) - (3)].lnn)); }
    break;

  case 114:

/* Line 1806 of yacc.c  */
#line 834 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].nac));  }
    break;

  case 115:

/* Line 1806 of yacc.c  */
#line 835 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s+%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na)); }
    break;

  case 116:

/* Line 1806 of yacc.c  */
#line 836 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s-%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na)); }
    break;

  case 117:

/* Line 1806 of yacc.c  */
#line 837 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s*%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na)); }
    break;

  case 118:

/* Line 1806 of yacc.c  */
#line 838 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s/%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na)); }
    break;

  case 119:

/* Line 1806 of yacc.c  */
#line 840 "fortran.y"
    { if ( couldaddvariable == 1 ) strcpy((yyval.nac),""); }
    break;

  case 120:

/* Line 1806 of yacc.c  */
#line 841 "fortran.y"
    { if ( couldaddvariable == 1 ) strcpy((yyval.nac),(yyvsp[(1) - (1)].nac)); }
    break;

  case 125:

/* Line 1806 of yacc.c  */
#line 851 "fortran.y"
    {
            positioninblock = 0;
            pos_curdimension = setposcur()-9;
        }
    break;

  case 126:

/* Line 1806 of yacc.c  */
#line 858 "fortran.y"
    {
            if ( couldaddvariable == 1 )
            {
                curvar=createvar((yyvsp[(3) - (5)].nac),(yyvsp[(4) - (5)].d));
                CreateAndFillin_Curvar("",curvar);
                curlistvar=insertvar(NULL,curvar);
                (yyval.l)=settype("",curlistvar);
                strcpy(vallengspec,"");
            }
        }
    break;

  case 127:

/* Line 1806 of yacc.c  */
#line 869 "fortran.y"
    {
            if ( couldaddvariable == 1 )
            {
                curvar=createvar((yyvsp[(3) - (5)].nac),(yyvsp[(4) - (5)].d));
                CreateAndFillin_Curvar("",curvar);
                curlistvar=insertvar((yyvsp[(1) - (5)].l),curvar);
                (yyval.l)=curlistvar;
                strcpy(vallengspec,"");
            }
        }
    break;

  case 130:

/* Line 1806 of yacc.c  */
#line 885 "fortran.y"
    { (yyval.lnn) = (listname *) NULL; }
    break;

  case 131:

/* Line 1806 of yacc.c  */
#line 886 "fortran.y"
    { (yyval.lnn) = (yyvsp[(3) - (3)].lnn); }
    break;

  case 132:

/* Line 1806 of yacc.c  */
#line 889 "fortran.y"
    { (yyval.lnn) = Insertname(NULL,(yyvsp[(1) - (1)].nac),0); }
    break;

  case 133:

/* Line 1806 of yacc.c  */
#line 890 "fortran.y"
    { (yyval.lnn) = Insertname((yyvsp[(1) - (3)].lnn),(yyvsp[(3) - (3)].nac),0);   }
    break;

  case 134:

/* Line 1806 of yacc.c  */
#line 894 "fortran.y"
    {
            pos_end = setposcur();
            RemoveWordSET_0(fortran_out,pos_curcommon,pos_end-pos_curcommon);
        }
    break;

  case 135:

/* Line 1806 of yacc.c  */
#line 899 "fortran.y"
    {
            if ( couldaddvariable == 1 )
            {
                sprintf(charusemodule,"%s",(yyvsp[(2) - (3)].nac));
                Add_NameOfCommon_1((yyvsp[(2) - (3)].nac),subroutinename);
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_curcommon,pos_end-pos_curcommon);
            }
        }
    break;

  case 136:

/* Line 1806 of yacc.c  */
#line 909 "fortran.y"
    {
            if ( couldaddvariable == 1 )
            {
                sprintf(charusemodule,"%s",(yyvsp[(3) - (5)].nac));
                Add_NameOfCommon_1((yyvsp[(3) - (5)].nac),subroutinename);
                pos_end = setposcur();
                RemoveWordSET_0(fortran_out,pos_curcommon,pos_end-pos_curcommon);
            }
        }
    break;

  case 137:

/* Line 1806 of yacc.c  */
#line 920 "fortran.y"
    { positioninblock = 0; pos_curcommon = setposcur()-6;   }
    break;

  case 138:

/* Line 1806 of yacc.c  */
#line 921 "fortran.y"
    { positioninblock = 0; pos_curcommon = setposcur()-6-7; }
    break;

  case 139:

/* Line 1806 of yacc.c  */
#line 924 "fortran.y"
    { if ( couldaddvariable == 1 ) Add_Common_var_1(); }
    break;

  case 140:

/* Line 1806 of yacc.c  */
#line 925 "fortran.y"
    { if ( couldaddvariable == 1 ) Add_Common_var_1(); }
    break;

  case 141:

/* Line 1806 of yacc.c  */
#line 928 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
                         positioninblock = positioninblock + 1 ;
                         strcpy(commonvar,(yyvsp[(1) - (2)].nac));
                         commondim = (yyvsp[(2) - (2)].d);
                      }
                   }
    break;

  case 142:

/* Line 1806 of yacc.c  */
#line 939 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
                         strcpy((yyval.nac),"");
                         positioninblock=0;
                         strcpy(commonblockname,"");
                      }
                   }
    break;

  case 143:

/* Line 1806 of yacc.c  */
#line 948 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
                         strcpy((yyval.nac),(yyvsp[(2) - (3)].nac));
                         positioninblock=0;
                         strcpy(commonblockname,(yyvsp[(2) - (3)].nac));
                      }
                   }
    break;

  case 146:

/* Line 1806 of yacc.c  */
#line 961 "fortran.y"
    {
                      if ( couldaddvariable == 1 ) (yyval.l)=insertvar(NULL,(yyvsp[(1) - (1)].v));
                   }
    break;

  case 147:

/* Line 1806 of yacc.c  */
#line 965 "fortran.y"
    {
                      if ( couldaddvariable == 1 ) (yyval.l)=insertvar((yyvsp[(1) - (3)].l),(yyvsp[(3) - (3)].v));
                   }
    break;

  case 148:

/* Line 1806 of yacc.c  */
#line 970 "fortran.y"
    {
                     if ( couldaddvariable == 1 )
                     {
                         curvar=(variable *) malloc(sizeof(variable));
                         /*                                                   */
                         Init_Variable(curvar);
                         /*                                                   */
                         curvar->v_VariableIsParameter=1;
                         strcpy(curvar->v_nomvar,(yyvsp[(1) - (3)].nac));
                         Save_Length((yyvsp[(1) - (3)].nac),4);
                         strcpy(curvar->v_subroutinename,subroutinename);
                         Save_Length(subroutinename,11);
                         strcpy(curvar->v_modulename,curmodulename);
                         Save_Length(curmodulename,6);
                         strcpy(curvar->v_initialvalue,(yyvsp[(3) - (3)].na));
                         Save_Length((yyvsp[(3) - (3)].na),14);
                         strcpy(curvar->v_commoninfile,mainfile);
                         Save_Length(mainfile,10);
                         (yyval.v)=curvar;
                      }
                   }
    break;

  case 152:

/* Line 1806 of yacc.c  */
#line 998 "fortran.y"
    {
                       if ( insubroutinedeclare == 1 )
                       {
                          Add_ImplicitNoneSubroutine_1();
                          pos_end = setposcur();
                          RemoveWordSET_0(fortran_out,pos_end-13,13);
                       }
                    }
    break;

  case 154:

/* Line 1806 of yacc.c  */
#line 1009 "fortran.y"
    {
            if ( couldaddvariable == 1 )
            {
                if (dimsgiven == 1)    curvar = createvar((yyvsp[(2) - (5)].nac),curdim);
                else                   curvar = createvar((yyvsp[(2) - (5)].nac),(yyvsp[(3) - (5)].d));
                CreateAndFillin_Curvar(DeclType,curvar);
                curlistvar=insertvar(NULL,curvar);
                if (!strcasecmp(DeclType,"character"))
                {
                    if (c_selectorgiven == 1)
                    {
                        strcpy(c_selectordim.first,"1");
                        strcpy(c_selectordim.last,c_selectorname);
                        Save_Length(c_selectorname,1);
                        change_dim_char(insertdim(NULL,c_selectordim),curlistvar);
                    }
                }
                (yyval.l)=settype(DeclType,curlistvar);
            }
            strcpy(vallengspec,"");
        }
    break;

  case 155:

/* Line 1806 of yacc.c  */
#line 1031 "fortran.y"
    {
            if ( couldaddvariable == 1 )
            {
                if (dimsgiven == 1)    curvar = createvar((yyvsp[(3) - (6)].nac),curdim);
                else                   curvar = createvar((yyvsp[(3) - (6)].nac),(yyvsp[(4) - (6)].d));
                CreateAndFillin_Curvar((yyvsp[(1) - (6)].l)->var->v_typevar,curvar);
                strcpy(curvar->v_typevar,((yyvsp[(1) - (6)].l)->var->v_typevar));
                Save_Length((yyvsp[(1) - (6)].l)->var->v_typevar,3);
                curlistvar=insertvar((yyvsp[(1) - (6)].l),curvar);
                if (!strcasecmp(DeclType,"character"))
                {
                    if (c_selectorgiven == 1)
                    {
                        strcpy(c_selectordim.first,"1");
                        strcpy(c_selectordim.last,c_selectorname);
                        Save_Length(c_selectorname,1);
                        change_dim_char(insertdim(NULL,c_selectordim),curlistvar);
                    }
                }
                (yyval.l)=curlistvar;
            }
            strcpy(vallengspec,"");
        }
    break;

  case 156:

/* Line 1806 of yacc.c  */
#line 1055 "fortran.y"
    { dimsgiven = 0; }
    break;

  case 157:

/* Line 1806 of yacc.c  */
#line 1057 "fortran.y"
    { strcpy(DeclType,(yyvsp[(1) - (2)].nac));}
    break;

  case 158:

/* Line 1806 of yacc.c  */
#line 1058 "fortran.y"
    { strcpy(DeclType,"CHARACTER"); }
    break;

  case 159:

/* Line 1806 of yacc.c  */
#line 1059 "fortran.y"
    { strcpy(DeclType,(yyvsp[(1) - (3)].nac)); strcpy(nameinttypename,(yyvsp[(3) - (3)].nac)); }
    break;

  case 160:

/* Line 1806 of yacc.c  */
#line 1060 "fortran.y"
    { strcpy(DeclType,"TYPE"); }
    break;

  case 162:

/* Line 1806 of yacc.c  */
#line 1063 "fortran.y"
    { c_selectorgiven = 1; strcpy(c_selectorname,(yyvsp[(2) - (2)].nac)); }
    break;

  case 163:

/* Line 1806 of yacc.c  */
#line 1064 "fortran.y"
    { c_star = 1;}
    break;

  case 168:

/* Line 1806 of yacc.c  */
#line 1071 "fortran.y"
    { pos_cur_decl = setposcur()-9; }
    break;

  case 169:

/* Line 1806 of yacc.c  */
#line 1074 "fortran.y"
    { strcpy((yyval.nac),"INTEGER"); pos_cur_decl = setposcur()-7; }
    break;

  case 170:

/* Line 1806 of yacc.c  */
#line 1075 "fortran.y"
    { strcpy((yyval.nac),"LOGICAL"); pos_cur_decl = setposcur()-7; }
    break;

  case 171:

/* Line 1806 of yacc.c  */
#line 1076 "fortran.y"
    { strcpy((yyval.nac),"REAL");    pos_cur_decl = setposcur()-4; }
    break;

  case 172:

/* Line 1806 of yacc.c  */
#line 1077 "fortran.y"
    { strcpy((yyval.nac),"COMPLEX"); pos_cur_decl = setposcur()-7; }
    break;

  case 173:

/* Line 1806 of yacc.c  */
#line 1078 "fortran.y"
    { strcpy((yyval.nac),"DOUBLE COMPLEX"); pos_cur_decl = setposcur()-14; }
    break;

  case 174:

/* Line 1806 of yacc.c  */
#line 1079 "fortran.y"
    { pos_cur_decl = setposcur()-16; strcpy((yyval.nac),"REAL"); strcpy(nameinttypename,"8"); }
    break;

  case 176:

/* Line 1806 of yacc.c  */
#line 1082 "fortran.y"
    {strcpy(vallengspec,(yyvsp[(2) - (2)].na));}
    break;

  case 177:

/* Line 1806 of yacc.c  */
#line 1084 "fortran.y"
    {sprintf((yyval.na),"*%s",(yyvsp[(1) - (1)].na));}
    break;

  case 178:

/* Line 1806 of yacc.c  */
#line 1085 "fortran.y"
    {strcpy((yyval.na),"*(*)");}
    break;

  case 185:

/* Line 1806 of yacc.c  */
#line 1097 "fortran.y"
    {
            if ( strstr((yyvsp[(3) - (3)].na),"0.d0") )
            {
                strcpy(nameinttypename,"8");
                sprintf(NamePrecision,"");
            }
            else
                sprintf(NamePrecision,"%s = %s",(yyvsp[(1) - (3)].nac),(yyvsp[(3) - (3)].na));
        }
    break;

  case 186:

/* Line 1806 of yacc.c  */
#line 1106 "fortran.y"
    { strcpy(NamePrecision,(yyvsp[(1) - (1)].nac)); }
    break;

  case 187:

/* Line 1806 of yacc.c  */
#line 1107 "fortran.y"
    { strcpy(NamePrecision,(yyvsp[(1) - (1)].nac)); }
    break;

  case 188:

/* Line 1806 of yacc.c  */
#line 1110 "fortran.y"
    { strcpy(CharacterSize,(yyvsp[(1) - (1)].na));  strcpy((yyval.na),(yyvsp[(1) - (1)].na));  }
    break;

  case 189:

/* Line 1806 of yacc.c  */
#line 1111 "fortran.y"
    { strcpy(CharacterSize,"*"); strcpy((yyval.na),"*"); }
    break;

  case 197:

/* Line 1806 of yacc.c  */
#line 1124 "fortran.y"
    { VariableIsParameter = 1; }
    break;

  case 199:

/* Line 1806 of yacc.c  */
#line 1126 "fortran.y"
    { Allocatabledeclare = 1; }
    break;

  case 200:

/* Line 1806 of yacc.c  */
#line 1127 "fortran.y"
    { dimsgiven=1; curdim=(yyvsp[(2) - (2)].d); }
    break;

  case 201:

/* Line 1806 of yacc.c  */
#line 1128 "fortran.y"
    { ExternalDeclare = 1; }
    break;

  case 202:

/* Line 1806 of yacc.c  */
#line 1130 "fortran.y"
    { strcpy(IntentSpec,(yyvsp[(3) - (4)].nac)); }
    break;

  case 204:

/* Line 1806 of yacc.c  */
#line 1132 "fortran.y"
    { optionaldeclare = 1 ; }
    break;

  case 205:

/* Line 1806 of yacc.c  */
#line 1133 "fortran.y"
    { pointerdeclare = 1 ; }
    break;

  case 206:

/* Line 1806 of yacc.c  */
#line 1134 "fortran.y"
    { SaveDeclare = 1 ; }
    break;

  case 207:

/* Line 1806 of yacc.c  */
#line 1135 "fortran.y"
    { Targetdeclare = 1; }
    break;

  case 208:

/* Line 1806 of yacc.c  */
#line 1138 "fortran.y"
    { strcpy((yyval.nac),(yyvsp[(1) - (1)].nac)); }
    break;

  case 209:

/* Line 1806 of yacc.c  */
#line 1139 "fortran.y"
    { strcpy((yyval.nac),(yyvsp[(1) - (1)].nac)); }
    break;

  case 210:

/* Line 1806 of yacc.c  */
#line 1140 "fortran.y"
    { strcpy((yyval.nac),(yyvsp[(1) - (1)].nac)); }
    break;

  case 211:

/* Line 1806 of yacc.c  */
#line 1143 "fortran.y"
    { PublicDeclare = 1;  }
    break;

  case 212:

/* Line 1806 of yacc.c  */
#line 1144 "fortran.y"
    { PrivateDeclare = 1; }
    break;

  case 213:

/* Line 1806 of yacc.c  */
#line 1146 "fortran.y"
    { if ( created_dimensionlist == 1 ) (yyval.d) = (listdim*) NULL; }
    break;

  case 214:

/* Line 1806 of yacc.c  */
#line 1147 "fortran.y"
    { if ( created_dimensionlist == 1 || agrif_parentcall == 1 ) (yyval.d)=(yyvsp[(2) - (3)].d); }
    break;

  case 215:

/* Line 1806 of yacc.c  */
#line 1150 "fortran.y"
    { if ( created_dimensionlist == 1 || agrif_parentcall == 1 )  (yyval.d)=insertdim(NULL,(yyvsp[(1) - (1)].dim1)); }
    break;

  case 216:

/* Line 1806 of yacc.c  */
#line 1151 "fortran.y"
    { if ( couldaddvariable == 1 && created_dimensionlist == 1 )  (yyval.d)=insertdim((yyvsp[(1) - (3)].d),(yyvsp[(3) - (3)].dim1));   }
    break;

  case 217:

/* Line 1806 of yacc.c  */
#line 1153 "fortran.y"
    { strcpy((yyval.dim1).first,"1"); strcpy((yyval.dim1).last,(yyvsp[(1) - (1)].na)); Save_Length((yyvsp[(1) - (1)].na),1); }
    break;

  case 218:

/* Line 1806 of yacc.c  */
#line 1154 "fortran.y"
    { strcpy((yyval.dim1).first,"");  strcpy((yyval.dim1).last,"");                    }
    break;

  case 219:

/* Line 1806 of yacc.c  */
#line 1155 "fortran.y"
    { strcpy((yyval.dim1).first,(yyvsp[(1) - (2)].na));  Save_Length((yyvsp[(1) - (2)].na),2); strcpy((yyval.dim1).last,""); }
    break;

  case 220:

/* Line 1806 of yacc.c  */
#line 1156 "fortran.y"
    { strcpy((yyval.dim1).first,"");  strcpy((yyval.dim1).last,(yyvsp[(2) - (2)].na)); Save_Length((yyvsp[(2) - (2)].na),1); }
    break;

  case 221:

/* Line 1806 of yacc.c  */
#line 1157 "fortran.y"
    { strcpy((yyval.dim1).first,(yyvsp[(1) - (3)].na));  Save_Length((yyvsp[(1) - (3)].na),2); strcpy((yyval.dim1).last,(yyvsp[(3) - (3)].na)); Save_Length((yyvsp[(3) - (3)].na),1); }
    break;

  case 222:

/* Line 1806 of yacc.c  */
#line 1160 "fortran.y"
    { strcpy((yyval.na),"*"); }
    break;

  case 223:

/* Line 1806 of yacc.c  */
#line 1161 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na));  }
    break;

  case 224:

/* Line 1806 of yacc.c  */
#line 1163 "fortran.y"
    { if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 225:

/* Line 1806 of yacc.c  */
#line 1164 "fortran.y"
    { if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 226:

/* Line 1806 of yacc.c  */
#line 1165 "fortran.y"
    { if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 227:

/* Line 1806 of yacc.c  */
#line 1166 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),"(%s)",(yyvsp[(2) - (3)].na)); }
    break;

  case 228:

/* Line 1806 of yacc.c  */
#line 1170 "fortran.y"
    {sprintf((yyval.na),"SUM(%s)",(yyvsp[(2) - (3)].na));}
    break;

  case 229:

/* Line 1806 of yacc.c  */
#line 1171 "fortran.y"
    {sprintf((yyval.na),"MAX(%s)",(yyvsp[(2) - (3)].na));}
    break;

  case 230:

/* Line 1806 of yacc.c  */
#line 1172 "fortran.y"
    {sprintf((yyval.na),"TANH(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 231:

/* Line 1806 of yacc.c  */
#line 1173 "fortran.y"
    {sprintf((yyval.na),"MAXVAL(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 232:

/* Line 1806 of yacc.c  */
#line 1174 "fortran.y"
    {sprintf((yyval.na),"MIN(%s)",(yyvsp[(2) - (3)].na));}
    break;

  case 233:

/* Line 1806 of yacc.c  */
#line 1175 "fortran.y"
    {sprintf((yyval.na),"MINVAL(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 234:

/* Line 1806 of yacc.c  */
#line 1176 "fortran.y"
    {sprintf((yyval.na),"TRIM(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 235:

/* Line 1806 of yacc.c  */
#line 1177 "fortran.y"
    {sprintf((yyval.na),"SQRT(%s)",(yyvsp[(2) - (3)].na));}
    break;

  case 236:

/* Line 1806 of yacc.c  */
#line 1178 "fortran.y"
    {sprintf((yyval.na),"REAL(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 237:

/* Line 1806 of yacc.c  */
#line 1179 "fortran.y"
    {sprintf((yyval.na),"NINT(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 238:

/* Line 1806 of yacc.c  */
#line 1180 "fortran.y"
    {sprintf((yyval.na),"FLOAT(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 239:

/* Line 1806 of yacc.c  */
#line 1181 "fortran.y"
    {sprintf((yyval.na),"EXP(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 240:

/* Line 1806 of yacc.c  */
#line 1182 "fortran.y"
    {sprintf((yyval.na),"COS(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 241:

/* Line 1806 of yacc.c  */
#line 1183 "fortran.y"
    {sprintf((yyval.na),"COSH(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 242:

/* Line 1806 of yacc.c  */
#line 1184 "fortran.y"
    {sprintf((yyval.na),"ACOS(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 243:

/* Line 1806 of yacc.c  */
#line 1185 "fortran.y"
    {sprintf((yyval.na),"SIN(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 244:

/* Line 1806 of yacc.c  */
#line 1186 "fortran.y"
    {sprintf((yyval.na),"SINH(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 245:

/* Line 1806 of yacc.c  */
#line 1187 "fortran.y"
    {sprintf((yyval.na),"ASIN(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 246:

/* Line 1806 of yacc.c  */
#line 1188 "fortran.y"
    {sprintf((yyval.na),"LOG(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 247:

/* Line 1806 of yacc.c  */
#line 1189 "fortran.y"
    {sprintf((yyval.na),"TAN(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 248:

/* Line 1806 of yacc.c  */
#line 1190 "fortran.y"
    {sprintf((yyval.na),"ATAN(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 249:

/* Line 1806 of yacc.c  */
#line 1191 "fortran.y"
    {sprintf((yyval.na),"ABS(%s)",(yyvsp[(2) - (3)].na));}
    break;

  case 250:

/* Line 1806 of yacc.c  */
#line 1192 "fortran.y"
    {sprintf((yyval.na),"MOD(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 251:

/* Line 1806 of yacc.c  */
#line 1193 "fortran.y"
    {sprintf((yyval.na),"SIGN(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 252:

/* Line 1806 of yacc.c  */
#line 1194 "fortran.y"
    {sprintf((yyval.na),"MINLOC(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 253:

/* Line 1806 of yacc.c  */
#line 1195 "fortran.y"
    {sprintf((yyval.na),"MAXLOC(%s)",(yyvsp[(3) - (4)].na));}
    break;

  case 254:

/* Line 1806 of yacc.c  */
#line 1197 "fortran.y"
    {strcpy((yyval.na),(yyvsp[(1) - (1)].na));}
    break;

  case 255:

/* Line 1806 of yacc.c  */
#line 1198 "fortran.y"
    { if ( couldaddvariable == 1 ) { strcpy((yyval.na),(yyvsp[(1) - (3)].na));strcat((yyval.na),",");strcat((yyval.na),(yyvsp[(3) - (3)].na));}}
    break;

  case 256:

/* Line 1806 of yacc.c  */
#line 1200 "fortran.y"
    { if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 257:

/* Line 1806 of yacc.c  */
#line 1201 "fortran.y"
    { if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].nac)); }
    break;

  case 258:

/* Line 1806 of yacc.c  */
#line 1202 "fortran.y"
    { if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 259:

/* Line 1806 of yacc.c  */
#line 1203 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].na),(yyvsp[(2) - (2)].na)); }
    break;

  case 260:

/* Line 1806 of yacc.c  */
#line 1204 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na)); }
    break;

  case 261:

/* Line 1806 of yacc.c  */
#line 1205 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na)); }
    break;

  case 262:

/* Line 1806 of yacc.c  */
#line 1207 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.nac),"+");}
    break;

  case 263:

/* Line 1806 of yacc.c  */
#line 1208 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.nac),"-");}
    break;

  case 264:

/* Line 1806 of yacc.c  */
#line 1212 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"+%s",(yyvsp[(2) - (2)].na));}
    break;

  case 265:

/* Line 1806 of yacc.c  */
#line 1214 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"-%s",(yyvsp[(2) - (2)].na));}
    break;

  case 266:

/* Line 1806 of yacc.c  */
#line 1216 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"*%s",(yyvsp[(2) - (2)].na));}
    break;

  case 267:

/* Line 1806 of yacc.c  */
#line 1218 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na));}
    break;

  case 268:

/* Line 1806 of yacc.c  */
#line 1220 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na));}
    break;

  case 269:

/* Line 1806 of yacc.c  */
#line 1222 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na));}
    break;

  case 270:

/* Line 1806 of yacc.c  */
#line 1224 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na));}
    break;

  case 271:

/* Line 1806 of yacc.c  */
#line 1226 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na)," > %s",(yyvsp[(2) - (2)].na));}
    break;

  case 272:

/* Line 1806 of yacc.c  */
#line 1228 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na));}
    break;

  case 273:

/* Line 1806 of yacc.c  */
#line 1230 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na)," < %s",(yyvsp[(2) - (2)].na));}
    break;

  case 274:

/* Line 1806 of yacc.c  */
#line 1232 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na));}
    break;

  case 275:

/* Line 1806 of yacc.c  */
#line 1234 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na)," >= %s",(yyvsp[(3) - (3)].na));}
    break;

  case 276:

/* Line 1806 of yacc.c  */
#line 1236 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na));}
    break;

  case 277:

/* Line 1806 of yacc.c  */
#line 1238 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na)," <= %s",(yyvsp[(3) - (3)].na));}
    break;

  case 278:

/* Line 1806 of yacc.c  */
#line 1240 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na));}
    break;

  case 279:

/* Line 1806 of yacc.c  */
#line 1242 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na));}
    break;

  case 280:

/* Line 1806 of yacc.c  */
#line 1244 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na));}
    break;

  case 281:

/* Line 1806 of yacc.c  */
#line 1246 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na));}
    break;

  case 282:

/* Line 1806 of yacc.c  */
#line 1248 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].na));}
    break;

  case 283:

/* Line 1806 of yacc.c  */
#line 1250 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s",(yyvsp[(2) - (2)].na));}
    break;

  case 284:

/* Line 1806 of yacc.c  */
#line 1252 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s",(yyvsp[(2) - (2)].na));}
    break;

  case 285:

/* Line 1806 of yacc.c  */
#line 1254 "fortran.y"
    {strcpy((yyval.na),"");}
    break;

  case 286:

/* Line 1806 of yacc.c  */
#line 1256 "fortran.y"
    {sprintf((yyval.na),"/%s",(yyvsp[(1) - (1)].na));}
    break;

  case 287:

/* Line 1806 of yacc.c  */
#line 1258 "fortran.y"
    {sprintf((yyval.na),"/= %s",(yyvsp[(2) - (2)].na));}
    break;

  case 288:

/* Line 1806 of yacc.c  */
#line 1260 "fortran.y"
    {sprintf((yyval.na),"//%s",(yyvsp[(2) - (2)].na));}
    break;

  case 289:

/* Line 1806 of yacc.c  */
#line 1263 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"==%s",(yyvsp[(2) - (2)].na));}
    break;

  case 290:

/* Line 1806 of yacc.c  */
#line 1265 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"= %s",(yyvsp[(1) - (1)].na));}
    break;

  case 291:

/* Line 1806 of yacc.c  */
#line 1268 "fortran.y"
    { if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].nac)); }
    break;

  case 292:

/* Line 1806 of yacc.c  */
#line 1269 "fortran.y"
    { if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 293:

/* Line 1806 of yacc.c  */
#line 1270 "fortran.y"
    { if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 294:

/* Line 1806 of yacc.c  */
#line 1274 "fortran.y"
    {
            agrif_parentcall = 0;
            if (!strcasecmp(identcopy,"Agrif_Parent") )     agrif_parentcall = 1;
            if ( Agrif_in_Tok_NAME(identcopy) == 1 )
            {
                inagrifcallargument = 1;
                Add_SubroutineWhereAgrifUsed_1(subroutinename, curmodulename);
            }
        }
    break;

  case 295:

/* Line 1806 of yacc.c  */
#line 1285 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(1) - (1)].na)); if ( incalldeclare == 0 ) inagrifcallargument = 0;   }
    break;

  case 296:

/* Line 1806 of yacc.c  */
#line 1286 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na)," %s %s ",(yyvsp[(1) - (2)].na),(yyvsp[(2) - (2)].na));           }
    break;

  case 297:

/* Line 1806 of yacc.c  */
#line 1287 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na)," %s ( %s )",(yyvsp[(1) - (4)].na),(yyvsp[(3) - (4)].na));        }
    break;

  case 298:

/* Line 1806 of yacc.c  */
#line 1288 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na)," %s ( %s ) %s ",(yyvsp[(1) - (5)].na),(yyvsp[(3) - (5)].na),(yyvsp[(5) - (5)].na)); }
    break;

  case 299:

/* Line 1806 of yacc.c  */
#line 1292 "fortran.y"
    {
            if ( couldaddvariable == 1 )
            {
                sprintf((yyval.na)," %s ( %s )",(yyvsp[(1) - (4)].nac),(yyvsp[(3) - (4)].na));
                ModifyTheAgrifFunction_0((yyvsp[(3) - (4)].na));
                agrif_parentcall =0;
            }
        }
    break;

  case 300:

/* Line 1806 of yacc.c  */
#line 1303 "fortran.y"
    {
            sprintf((yyval.na)," %s %% %s ",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na));
            if ( incalldeclare == 0 ) inagrifcallargument = 0;
        }
    break;

  case 301:

/* Line 1806 of yacc.c  */
#line 1309 "fortran.y"
    { sprintf((yyval.na),"(/%s/)",(yyvsp[(2) - (3)].na)); }
    break;

  case 302:

/* Line 1806 of yacc.c  */
#line 1312 "fortran.y"
    { strcpy((yyval.na)," "); }
    break;

  case 303:

/* Line 1806 of yacc.c  */
#line 1313 "fortran.y"
    { strcpy((yyval.na),(yyvsp[(2) - (2)].na)); }
    break;

  case 304:

/* Line 1806 of yacc.c  */
#line 1316 "fortran.y"
    { if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 305:

/* Line 1806 of yacc.c  */
#line 1317 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s,%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na)); }
    break;

  case 306:

/* Line 1806 of yacc.c  */
#line 1320 "fortran.y"
    {strcpy((yyval.na),(yyvsp[(1) - (1)].na));}
    break;

  case 307:

/* Line 1806 of yacc.c  */
#line 1321 "fortran.y"
    {strcpy((yyval.na),(yyvsp[(1) - (1)].na));}
    break;

  case 308:

/* Line 1806 of yacc.c  */
#line 1324 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s :%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na));}
    break;

  case 309:

/* Line 1806 of yacc.c  */
#line 1325 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s :%s :%s",(yyvsp[(1) - (5)].na),(yyvsp[(3) - (5)].na),(yyvsp[(5) - (5)].na));}
    break;

  case 310:

/* Line 1806 of yacc.c  */
#line 1326 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),":%s :%s",(yyvsp[(2) - (4)].na),(yyvsp[(4) - (4)].na));}
    break;

  case 311:

/* Line 1806 of yacc.c  */
#line 1327 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),": : %s",(yyvsp[(3) - (3)].na));}
    break;

  case 312:

/* Line 1806 of yacc.c  */
#line 1328 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),":%s",(yyvsp[(2) - (2)].na));}
    break;

  case 313:

/* Line 1806 of yacc.c  */
#line 1329 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s :",(yyvsp[(1) - (2)].na));}
    break;

  case 314:

/* Line 1806 of yacc.c  */
#line 1330 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),":");}
    break;

  case 315:

/* Line 1806 of yacc.c  */
#line 1333 "fortran.y"
    {
            if ( couldaddvariable == 1 && afterpercent == 0 )
            {
                if ( Vartonumber((yyvsp[(1) - (1)].nac)) == 1 ) Add_SubroutineWhereAgrifUsed_1(subroutinename, curmodulename);
                if ( !strcasecmp((yyvsp[(1) - (1)].nac),"Agrif_Parent") )   agrif_parentcall =1;
                if ( VariableIsNotFunction((yyvsp[(1) - (1)].nac)) == 0 )
                {
                    if ( inagrifcallargument == 1 )
                    {
                        if ( !strcasecmp((yyvsp[(1) - (1)].nac),identcopy) )
                        {
                            strcpy(sameagrifname,identcopy);
                            sameagrifargument = 1;
                        }
                    }
                    strcpy(identcopy,(yyvsp[(1) - (1)].nac));
                    pointedvar=0;
                    strcpy(truename,(yyvsp[(1) - (1)].nac));
                    if (variscoupled_0((yyvsp[(1) - (1)].nac))) strcpy(truename,getcoupledname_0((yyvsp[(1) - (1)].nac)));

                    if ( VarIsNonGridDepend(truename) == 0 && Variableshouldberemove(truename) == 0 )
                    {
                        if ( inagrifcallargument == 1 || varispointer_0(truename) == 1 )
                        {
                            if ( (IsinListe(List_UsedInSubroutine_Var,(yyvsp[(1) - (1)].nac)) == 1) || (inagrifcallargument == 1) )
                            {
                                if (varistyped_0(truename) == 0)    ModifyTheVariableName_0(truename,strlen((yyvsp[(1) - (1)].nac)));
                            }
                        }
                        if ( inagrifcallargument != 1 || sameagrifargument ==1 )    Add_UsedInSubroutine_Var_1(truename);
                    }
                    NotifyAgrifFunction_0(truename);
                }
            }
            else
            {
                afterpercent = 0;
            }
        }
    break;

  case 316:

/* Line 1806 of yacc.c  */
#line 1374 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.nac),".TRUE.");}
    break;

  case 317:

/* Line 1806 of yacc.c  */
#line 1375 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.nac),".FALSE.");}
    break;

  case 318:

/* Line 1806 of yacc.c  */
#line 1376 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));}
    break;

  case 319:

/* Line 1806 of yacc.c  */
#line 1377 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));}
    break;

  case 320:

/* Line 1806 of yacc.c  */
#line 1379 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.nac),"%s%s",(yyvsp[(1) - (2)].nac),(yyvsp[(2) - (2)].nac));}
    break;

  case 322:

/* Line 1806 of yacc.c  */
#line 1383 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));}
    break;

  case 324:

/* Line 1806 of yacc.c  */
#line 1386 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));}
    break;

  case 325:

/* Line 1806 of yacc.c  */
#line 1388 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));}
    break;

  case 326:

/* Line 1806 of yacc.c  */
#line 1390 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na)," ");}
    break;

  case 327:

/* Line 1806 of yacc.c  */
#line 1391 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na));}
    break;

  case 328:

/* Line 1806 of yacc.c  */
#line 1394 "fortran.y"
    {if ( couldaddvariable == 1 ) sprintf((yyval.na),"(%s :%s)",(yyvsp[(2) - (5)].na),(yyvsp[(4) - (5)].na));}
    break;

  case 329:

/* Line 1806 of yacc.c  */
#line 1396 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na)," ");}
    break;

  case 330:

/* Line 1806 of yacc.c  */
#line 1397 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na));}
    break;

  case 331:

/* Line 1806 of yacc.c  */
#line 1399 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na)," ");}
    break;

  case 332:

/* Line 1806 of yacc.c  */
#line 1400 "fortran.y"
    {if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na));}
    break;

  case 333:

/* Line 1806 of yacc.c  */
#line 1402 "fortran.y"
    {InitialValueGiven = 0;}
    break;

  case 334:

/* Line 1806 of yacc.c  */
#line 1404 "fortran.y"
    {
                       if ( couldaddvariable == 1 )
                       {
                          strcpy(InitValue,(yyvsp[(3) - (3)].na));
                          InitialValueGiven = 1;
                       }
                    }
    break;

  case 335:

/* Line 1806 of yacc.c  */
#line 1412 "fortran.y"
    {pos_curinit = setposcur();}
    break;

  case 336:

/* Line 1806 of yacc.c  */
#line 1415 "fortran.y"
    {sprintf((yyval.na),"(%s,%s)",(yyvsp[(2) - (5)].na),(yyvsp[(4) - (5)].na)); }
    break;

  case 337:

/* Line 1806 of yacc.c  */
#line 1418 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
                      /* if variables has been declared in a subroutine       */
                      if (insubroutinedeclare == 1)
                      {
                         copyuse_0((yyvsp[(2) - (2)].nac));
                      }
                      sprintf(charusemodule,"%s",(yyvsp[(2) - (2)].nac));
                      Add_NameOfModuleUsed_1((yyvsp[(2) - (2)].nac));

                      if ( inmoduledeclare == 0 )
                      {
                         pos_end = setposcur();
                         RemoveWordSET_0(fortran_out,pos_curuse,
                                               pos_end-pos_curuse);
                      }
                      }
                    }
    break;

  case 338:

/* Line 1806 of yacc.c  */
#line 1438 "fortran.y"
    {
                       if ( couldaddvariable == 1 )
                       {
                      if (insubroutinedeclare == 1)
                      {
                         Add_CouplePointed_Var_1((yyvsp[(2) - (4)].nac),(yyvsp[(4) - (4)].lc));
                      }
                      if ( firstpass == 1 )
                      {
                         if ( insubroutinedeclare == 1 )
                         {
                            coupletmp = (yyvsp[(4) - (4)].lc);
                            strcpy(ligne,"");
                            while ( coupletmp )
                            {
                               strcat(ligne,coupletmp->c_namevar);
                               strcat(ligne," => ");
                               strcat(ligne,coupletmp->c_namepointedvar);
                               coupletmp = coupletmp->suiv;
                               if ( coupletmp ) strcat(ligne,",");
                            }
                            sprintf(charusemodule,"%s",(yyvsp[(2) - (4)].nac));
                         }
                         Add_NameOfModuleUsed_1((yyvsp[(2) - (4)].nac));
                      }
                      if ( inmoduledeclare == 0 )
                      {
                         pos_end = setposcur();
                         RemoveWordSET_0(fortran_out,pos_curuse,
                                               pos_end-pos_curuse);
                      }
                      }
                    }
    break;

  case 339:

/* Line 1806 of yacc.c  */
#line 1472 "fortran.y"
    {
                       if ( couldaddvariable == 1 )
                       {
                      /* if variables has been declared in a subroutine       */
                      if (insubroutinedeclare == 1)
                      {
                         copyuseonly_0((yyvsp[(2) - (6)].nac));
                      }
                      sprintf(charusemodule,"%s",(yyvsp[(2) - (6)].nac));
                      Add_NameOfModuleUsed_1((yyvsp[(2) - (6)].nac));

                       if ( inmoduledeclare == 0 )
                       {
                          pos_end = setposcur();
                          RemoveWordSET_0(fortran_out,pos_curuse,
                                                pos_end-pos_curuse);
                       }
                       }
                    }
    break;

  case 340:

/* Line 1806 of yacc.c  */
#line 1492 "fortran.y"
    {
                       if ( couldaddvariable == 1 )
                       {
                       /* if variables has been declared in a subroutine      */
                       if (insubroutinedeclare == 1)
                       {
                          Add_CouplePointed_Var_1((yyvsp[(2) - (6)].nac),(yyvsp[(6) - (6)].lc));
                       }
                       if ( firstpass == 1 )
                       {
                         if ( insubroutinedeclare == 1 )
                         {
                             coupletmp = (yyvsp[(6) - (6)].lc);
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
                             sprintf(charusemodule,"%s",(yyvsp[(2) - (6)].nac));
                          }
                          Add_NameOfModuleUsed_1((yyvsp[(2) - (6)].nac));
                       }
                       if ( firstpass == 0 )
                       {
                          if ( inmoduledeclare == 0 )
                          {

                            pos_end = setposcur();
                             RemoveWordSET_0(fortran_out,pos_curuse,
                                                   pos_end-pos_curuse);
                       if (oldfortran_out) 
                         variableisglobalinmodule((yyvsp[(6) - (6)].lc),(yyvsp[(2) - (6)].nac),oldfortran_out,pos_curuseold);
                        
                          }
                          else
                          {

                             /* if we are in the module declare and if the    */
                             /* onlylist is a list of global variable         */
                             variableisglobalinmodule((yyvsp[(6) - (6)].lc), (yyvsp[(2) - (6)].nac), fortran_out,pos_curuse);
                          }
                       }
                       }
                    }
    break;

  case 341:

/* Line 1806 of yacc.c  */
#line 1543 "fortran.y"
    {
                pos_curuse = setposcur()-strlen((yyvsp[(1) - (1)].nac));
                if (firstpass == 0 && oldfortran_out) pos_curuseold = setposcurname(oldfortran_out);
            }
    break;

  case 342:

/* Line 1806 of yacc.c  */
#line 1549 "fortran.y"
    {
                       if ( couldaddvariable == 1 ) (yyval.lc) = (yyvsp[(1) - (1)].lc);
                    }
    break;

  case 343:

/* Line 1806 of yacc.c  */
#line 1553 "fortran.y"
    {
                        if ( couldaddvariable == 1 )
                        {
                        /* insert the variable in the list $1                 */
                        (yyvsp[(3) - (3)].lc)->suiv = (yyvsp[(1) - (3)].lc);
                        (yyval.lc) = (yyvsp[(3) - (3)].lc);
                        }
                    }
    break;

  case 344:

/* Line 1806 of yacc.c  */
#line 1563 "fortran.y"
    {
                       coupletmp =(listcouple *)malloc(sizeof(listcouple));
                       strcpy(coupletmp->c_namevar,(yyvsp[(1) - (3)].nac));
                       Save_Length((yyvsp[(1) - (3)].nac),21);
                       strcpy(coupletmp->c_namepointedvar,(yyvsp[(3) - (3)].nac));
                       Save_Length((yyvsp[(3) - (3)].nac),22);
                       coupletmp->suiv = NULL;
                       (yyval.lc) = coupletmp;
                     }
    break;

  case 345:

/* Line 1806 of yacc.c  */
#line 1574 "fortran.y"
    { if ( couldaddvariable == 1 ) (yyval.lc) = (yyvsp[(1) - (1)].lc); }
    break;

  case 346:

/* Line 1806 of yacc.c  */
#line 1576 "fortran.y"
    {
            if ( couldaddvariable == 1 )
            {
                /* insert the variable in the list $1                 */
                (yyvsp[(3) - (3)].lc)->suiv = (yyvsp[(1) - (3)].lc);
                (yyval.lc) = (yyvsp[(3) - (3)].lc);
            }
        }
    break;

  case 347:

/* Line 1806 of yacc.c  */
#line 1587 "fortran.y"
    {
                       coupletmp =(listcouple *)malloc(sizeof(listcouple));
                       strcpy(coupletmp->c_namevar,(yyvsp[(1) - (3)].nac));
                       Save_Length((yyvsp[(1) - (3)].nac),21);
                       strcpy(coupletmp->c_namepointedvar,(yyvsp[(3) - (3)].nac));
                       Save_Length((yyvsp[(3) - (3)].nac),22);
                       coupletmp->suiv = NULL;
                       (yyval.lc) = coupletmp;
                       pointedvar=1;
                       Add_UsedInSubroutine_Var_1((yyvsp[(1) - (3)].nac));
                    }
    break;

  case 348:

/* Line 1806 of yacc.c  */
#line 1598 "fortran.y"
    {
                       coupletmp =(listcouple *)malloc(sizeof(listcouple));
                       strcpy(coupletmp->c_namevar,(yyvsp[(1) - (1)].nac));
                       Save_Length((yyvsp[(1) - (1)].nac),21);
                       strcpy(coupletmp->c_namepointedvar,"");
                       coupletmp->suiv = NULL;
                       (yyval.lc) = coupletmp;
                     }
    break;

  case 350:

/* Line 1806 of yacc.c  */
#line 1609 "fortran.y"
    {
                         Add_SubroutineWhereAgrifUsed_1(subroutinename,
                                                        curmodulename);
                                                        inallocate = 0;
                     }
    break;

  case 351:

/* Line 1806 of yacc.c  */
#line 1615 "fortran.y"
    {
                          Add_SubroutineWhereAgrifUsed_1(subroutinename,
                                                         curmodulename);
                                                         inallocate = 0;
                     }
    break;

  case 353:

/* Line 1806 of yacc.c  */
#line 1622 "fortran.y"
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
    break;

  case 354:

/* Line 1806 of yacc.c  */
#line 1672 "fortran.y"
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
    break;

  case 355:

/* Line 1806 of yacc.c  */
#line 1690 "fortran.y"
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
    break;

  case 356:

/* Line 1806 of yacc.c  */
#line 1710 "fortran.y"
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
    break;

  case 357:

/* Line 1806 of yacc.c  */
#line 1729 "fortran.y"
    {
                       if ( couldaddvariable == 1 )
                       {
                       /* if we never meet the contains keyword               */
                      Remove_Word_end_module_0(strlen((yyvsp[(2) - (2)].nac)));
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
    break;

  case 371:

/* Line 1806 of yacc.c  */
#line 1775 "fortran.y"
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
                      incontainssubroutine = 1;
                      strcpy(previoussubroutinename,subroutinename);
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
                            if ( retour77 == 0 )    fprintf(paramout,"!\n");
                            else                    fprintf(paramout,"C\n");
                            fclose(paramout);
                         }
                         }
                         strcpy(subroutinename,"");
                      }
                   }
    break;

  case 372:

/* Line 1806 of yacc.c  */
#line 1812 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
                       strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));
                       pos_endsubroutine = setposcur()-strlen((yyvsp[(1) - (1)].nac));
                       functiondeclarationisdone = 0;
                       }
                    }
    break;

  case 373:

/* Line 1806 of yacc.c  */
#line 1822 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
                       strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));
                       pos_endsubroutine = setposcur()-strlen((yyvsp[(1) - (1)].nac));
                       }
                    }
    break;

  case 374:

/* Line 1806 of yacc.c  */
#line 1831 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
                       strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));
                       pos_endsubroutine = setposcur()-strlen((yyvsp[(1) - (1)].nac));
                       }
                    }
    break;

  case 375:

/* Line 1806 of yacc.c  */
#line 1840 "fortran.y"
    {
                      if ( couldaddvariable == 1 )
                      {
                       strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));
                       pos_endsubroutine = setposcur()-strlen((yyvsp[(1) - (1)].nac));
                       }
                    }
    break;

  case 387:

/* Line 1806 of yacc.c  */
#line 1863 "fortran.y"
    {strcpy((yyval.nac),"");}
    break;

  case 388:

/* Line 1806 of yacc.c  */
#line 1864 "fortran.y"
    {strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));}
    break;

  case 396:

/* Line 1806 of yacc.c  */
#line 1877 "fortran.y"
    {
            Add_SubroutineWhereAgrifUsed_1(subroutinename,curmodulename);
            inallocate = 0;
        }
    break;

  case 397:

/* Line 1806 of yacc.c  */
#line 1882 "fortran.y"
    {
            Add_SubroutineWhereAgrifUsed_1(subroutinename,curmodulename);
            inallocate = 0;
        }
    break;

  case 403:

/* Line 1806 of yacc.c  */
#line 1892 "fortran.y"
    { if ( couldaddvariable == 1 ) created_dimensionlist = 0; }
    break;

  case 404:

/* Line 1806 of yacc.c  */
#line 1895 "fortran.y"
    {
            created_dimensionlist = 1;
            if  ( agrif_parentcall == 1 )
            {
                ModifyTheAgrifFunction_0((yyvsp[(3) - (4)].d)->dim.last);
                agrif_parentcall = 0;
                fprintf(fortran_out," = ");
            }
        }
    break;

  case 405:

/* Line 1806 of yacc.c  */
#line 1905 "fortran.y"
    {
            created_dimensionlist = 1;
        }
    break;

  case 410:

/* Line 1806 of yacc.c  */
#line 1917 "fortran.y"
    {
                      inagrifcallargument = 0 ;
                      incalldeclare=0;
                      if ( oldfortran_out &&
                           !strcasecmp(meetagrifinitgrids,subroutinename) &&
                           firstpass == 0 &&
                           callmpiinit == 1)
                      {
                      /*   pos_end = setposcur();
                         RemoveWordSET_0(fortran_out,pos_curcall,
                                               pos_end-pos_curcall);
                         fprintf(oldfortran_out,"      Call MPI_Init (%s) \n"
                                                                   ,mpiinitvar);*/
                      }
                      if ( oldfortran_out           &&
                           callagrifinitgrids == 1 &&
                           firstpass == 0 )
                      {
                         pos_end = setposcur();
                         RemoveWordSET_0(fortran_out,pos_curcall,
                                               pos_end-pos_curcall);

                         strcpy(subofagrifinitgrids,subroutinename);
                      }
                      Instanciation_0(sameagrifname);
                   }
    break;

  case 415:

/* Line 1806 of yacc.c  */
#line 1951 "fortran.y"
    {
                       if (!strcasecmp((yyvsp[(2) - (2)].nac),"MPI_Init") )
                       {
                          callmpiinit = 1;
                       }
                       else
                       {
                          callmpiinit = 0;
                       }
                       if (!strcasecmp((yyvsp[(2) - (2)].nac),"Agrif_Init_Grids") )
                       {
                          callagrifinitgrids = 1;
                          strcpy(meetagrifinitgrids,subroutinename);
                       }
                       else callagrifinitgrids = 0;
                       if ( !strcasecmp((yyvsp[(2) - (2)].nac),"Agrif_Open_File") )
                       {
                          Add_SubroutineWhereAgrifUsed_1(subroutinename,
                                                        curmodulename);
                       }
                       if ( Vartonumber((yyvsp[(2) - (2)].nac)) == 1 )
                       {
                          incalldeclare=1;
                          inagrifcallargument = 1 ;
                          Add_SubroutineWhereAgrifUsed_1(subroutinename,
                                                        curmodulename);
                       }
                    }
    break;

  case 416:

/* Line 1806 of yacc.c  */
#line 1980 "fortran.y"
    { pos_curcall=setposcur()-4; }
    break;

  case 419:

/* Line 1806 of yacc.c  */
#line 1988 "fortran.y"
    {
            if ( callmpiinit == 1 )
            {
                strcpy(mpiinitvar,(yyvsp[(1) - (1)].na));
                if ( firstpass == 1 )  Add_UsedInSubroutine_Var_1 (mpiinitvar);
            }
        }
    break;

  case 489:

/* Line 1806 of yacc.c  */
#line 2094 "fortran.y"
    { if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 490:

/* Line 1806 of yacc.c  */
#line 2095 "fortran.y"
    { if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 491:

/* Line 1806 of yacc.c  */
#line 2096 "fortran.y"
    { if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 492:

/* Line 1806 of yacc.c  */
#line 2097 "fortran.y"
    { if ( couldaddvariable == 1 ) strcpy((yyval.na),(yyvsp[(1) - (1)].na)); }
    break;

  case 493:

/* Line 1806 of yacc.c  */
#line 2098 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s,%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na)); }
    break;

  case 494:

/* Line 1806 of yacc.c  */
#line 2099 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s,%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na)); }
    break;

  case 495:

/* Line 1806 of yacc.c  */
#line 2100 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s,%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na)); }
    break;

  case 496:

/* Line 1806 of yacc.c  */
#line 2101 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s,%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na)); }
    break;

  case 497:

/* Line 1806 of yacc.c  */
#line 2102 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s,%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na)); }
    break;

  case 498:

/* Line 1806 of yacc.c  */
#line 2103 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s,%s",(yyvsp[(1) - (3)].na),(yyvsp[(3) - (3)].na)); }
    break;

  case 499:

/* Line 1806 of yacc.c  */
#line 2106 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),"(%s,%s)",(yyvsp[(2) - (5)].na),(yyvsp[(4) - (5)].na)); }
    break;

  case 500:

/* Line 1806 of yacc.c  */
#line 2107 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),"(%s,%s)",(yyvsp[(2) - (5)].na),(yyvsp[(4) - (5)].na)); }
    break;

  case 501:

/* Line 1806 of yacc.c  */
#line 2108 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),"(%s,%s)",(yyvsp[(2) - (5)].na),(yyvsp[(4) - (5)].na)); }
    break;

  case 502:

/* Line 1806 of yacc.c  */
#line 2110 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s=%s,%s)",(yyvsp[(1) - (5)].nac),(yyvsp[(3) - (5)].na),(yyvsp[(5) - (5)].na));}
    break;

  case 503:

/* Line 1806 of yacc.c  */
#line 2111 "fortran.y"
    { if ( couldaddvariable == 1 ) sprintf((yyval.na),"%s=%s,%s,%s)",(yyvsp[(1) - (7)].nac),(yyvsp[(3) - (7)].na),(yyvsp[(5) - (7)].na),(yyvsp[(7) - (7)].na));}
    break;

  case 509:

/* Line 1806 of yacc.c  */
#line 2121 "fortran.y"
    {Add_Allocate_Var_1((yyvsp[(1) - (1)].nac),curmodulename);}
    break;

  case 512:

/* Line 1806 of yacc.c  */
#line 2126 "fortran.y"
    {Add_Allocate_Var_1((yyvsp[(1) - (4)].nac),curmodulename);}
    break;

  case 520:

/* Line 1806 of yacc.c  */
#line 2139 "fortran.y"
    {strcpy((yyval.nac),(yyvsp[(1) - (1)].nac));}
    break;



/* Line 1806 of yacc.c  */
#line 6340 "fortran.tab.c"
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now `shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*------------------------------------.
| yyerrlab -- here on detecting error |
`------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
	 error, discard it.  */

      if (yychar <= YYEOF)
	{
	  /* Return failure if at end of input.  */
	  if (yychar == YYEOF)
	    YYABORT;
	}
      else
	{
	  yydestruct ("Error: discarding",
		      yytoken, &yylval);
	  yychar = YYEMPTY;
	}
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule which action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;	/* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
	{
	  yyn += YYTERROR;
	  if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
	    {
	      yyn = yytable[yyn];
	      if (0 < yyn)
		break;
	    }
	}

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
	YYABORT;


      yydestruct ("Error: popping",
		  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  *++yyvsp = yylval;


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined(yyoverflow) || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule which action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
		  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  /* Make sure YYID is used.  */
  return YYID (yyresult);
}



/* Line 2067 of yacc.c  */
#line 2141 "fortran.y"


void process_fortran(char *fichier_entree)
{
    extern FILE *fortran_in;
    extern FILE *fortran_out;

    char nomfile[LONG_C];
    int c;
    int confirmyes;

    if ( todebug == 1 ) printf("Firstpass == %d \n",firstpass);

/******************************************************************************/
/*  1-  Open input and output files                                           */
/******************************************************************************/

    strcpy(nomfile,commondirin);
    strcat(nomfile,"/");
    strcat(nomfile,fichier_entree);
    fortran_in = fopen( nomfile,"r");
    if (! fortran_in)
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
                    c = getchar();
                    getchar();
                    if (c==79 || c==110)
                    {
                        printf("We stop\n");
                        exit(1);
                    }
                    if (c==89 || c==121)
                    {
                        confirmyes = 1;
                    }
                }
            }
        }
    }

/******************************************************************************/
/*  2-  Variables initialization                                              */
/******************************************************************************/

    line_num_input = 1;
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
    Targetdeclare = 0 ;
    strcpy(NamePrecision," ");
    VariableIsParameter =  0 ;
    strcpy(NamePrecision,"");
    c_star = 0 ;
    functiondeclarationisdone = 0;
    insubroutinedeclare = 0 ;
    strcpy(subroutinename," ");
    isrecursive = 0;
    InitialValueGiven = 0 ;
    strcpy(EmptyChar," ");
    inmoduledeclare = 0;
    incontainssubroutine = 0;
    couldaddvariable = 1;
    afterpercent = 0;
    aftercontainsdeclare = 1;
    strcpy(nameinttypename,"");
    /* Name of the file without format                                         */
    tmp = strchr(fichier_entree, '.');
    strncpy(curfilename,fichier_entree,strlen(fichier_entree)-strlen(tmp));
    Save_Length(curfilename,30);

/******************************************************************************/
/*  3-  Parsing of the input file (1 time)                                    */
/******************************************************************************/

    if (firstpass == 0) fortran_out = fopen(nomfileoutput,"w");

    fortran_parse();

    if (firstpass == 0) NewModule_Creation_0();
   
    strcpy(curfile,mainfile);

    if (firstpass == 0) fclose(fortran_out);
}

#line 2 "fortran.yy.c"

#line 4 "fortran.yy.c"

#define  YY_INT_ALIGNED short int

/* A lexical scanner generated by flex */

#define yy_create_buffer fortran__create_buffer
#define yy_delete_buffer fortran__delete_buffer
#define yy_flex_debug fortran__flex_debug
#define yy_init_buffer fortran__init_buffer
#define yy_flush_buffer fortran__flush_buffer
#define yy_load_buffer_state fortran__load_buffer_state
#define yy_switch_to_buffer fortran__switch_to_buffer
#define yyin fortran_in
#define yyleng fortran_leng
#define yylex fortran_lex
#define yylineno fortran_lineno
#define yyout fortran_out
#define yyrestart fortran_restart
#define yytext fortran_text
#define yywrap fortran_wrap
#define yyalloc fortran_alloc
#define yyrealloc fortran_realloc
#define yyfree fortran_free

#define FLEX_SCANNER
#define YY_FLEX_MAJOR_VERSION 2
#define YY_FLEX_MINOR_VERSION 5
#define YY_FLEX_SUBMINOR_VERSION 35
#if YY_FLEX_SUBMINOR_VERSION > 0
#define FLEX_BETA
#endif

/* First, we deal with  platform-specific or compiler-specific issues. */

/* begin standard C headers. */
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>

/* end standard C headers. */

/* flex integer type definitions */

#ifndef FLEXINT_H
#define FLEXINT_H

/* C99 systems have <inttypes.h>. Non-C99 systems may or may not. */

#if defined (__STDC_VERSION__) && __STDC_VERSION__ >= 199901L

/* C99 says to define __STDC_LIMIT_MACROS before including stdint.h,
 * if you want the limit (max/min) macros for int types. 
 */
#ifndef __STDC_LIMIT_MACROS
#define __STDC_LIMIT_MACROS 1
#endif

#include <inttypes.h>
typedef int8_t flex_int8_t;
typedef uint8_t flex_uint8_t;
typedef int16_t flex_int16_t;
typedef uint16_t flex_uint16_t;
typedef int32_t flex_int32_t;
typedef uint32_t flex_uint32_t;
#else
typedef signed char flex_int8_t;
typedef short int flex_int16_t;
typedef int flex_int32_t;
typedef unsigned char flex_uint8_t; 
typedef unsigned short int flex_uint16_t;
typedef unsigned int flex_uint32_t;
#endif /* ! C99 */

/* Limits of integral types. */
#ifndef INT8_MIN
#define INT8_MIN               (-128)
#endif
#ifndef INT16_MIN
#define INT16_MIN              (-32767-1)
#endif
#ifndef INT32_MIN
#define INT32_MIN              (-2147483647-1)
#endif
#ifndef INT8_MAX
#define INT8_MAX               (127)
#endif
#ifndef INT16_MAX
#define INT16_MAX              (32767)
#endif
#ifndef INT32_MAX
#define INT32_MAX              (2147483647)
#endif
#ifndef UINT8_MAX
#define UINT8_MAX              (255U)
#endif
#ifndef UINT16_MAX
#define UINT16_MAX             (65535U)
#endif
#ifndef UINT32_MAX
#define UINT32_MAX             (4294967295U)
#endif

#endif /* ! FLEXINT_H */

#ifdef __cplusplus

/* The "const" storage-class-modifier is valid. */
#define YY_USE_CONST

#else	/* ! __cplusplus */

/* C99 requires __STDC__ to be defined as 1. */
#if defined (__STDC__)

#define YY_USE_CONST

#endif	/* defined (__STDC__) */
#endif	/* ! __cplusplus */

#ifdef YY_USE_CONST
#define yyconst const
#else
#define yyconst
#endif

/* Returned upon end-of-file. */
#define YY_NULL 0

/* Promotes a possibly negative, possibly signed char to an unsigned
 * integer for use as an array index.  If the signed char is negative,
 * we want to instead treat it as an 8-bit unsigned char, hence the
 * double cast.
 */
#define YY_SC_TO_UI(c) ((unsigned int) (unsigned char) c)

/* Enter a start condition.  This macro really ought to take a parameter,
 * but we do it the disgusting crufty way forced on us by the ()-less
 * definition of BEGIN.
 */
#define BEGIN (yy_start) = 1 + 2 *

/* Translate the current start state into a value that can be later handed
 * to BEGIN to return to the state.  The YYSTATE alias is for lex
 * compatibility.
 */
#define YY_START (((yy_start) - 1) / 2)
#define YYSTATE YY_START

/* Action number for EOF rule of a given start state. */
#define YY_STATE_EOF(state) (YY_END_OF_BUFFER + state + 1)

/* Special action meaning "start processing a new file". */
#define YY_NEW_FILE fortran_restart(fortran_in  )

#define YY_END_OF_BUFFER_CHAR 0

/* Size of default input buffer. */
#ifndef YY_BUF_SIZE
#define YY_BUF_SIZE 16384
#endif

/* The state buf must be large enough to hold one state per character in the main buffer.
 */
#define YY_STATE_BUF_SIZE   ((YY_BUF_SIZE + 2) * sizeof(yy_state_type))

#ifndef YY_TYPEDEF_YY_BUFFER_STATE
#define YY_TYPEDEF_YY_BUFFER_STATE
typedef struct yy_buffer_state *YY_BUFFER_STATE;
#endif

extern int fortran_leng;

extern FILE *fortran_in, *fortran_out;

#define EOB_ACT_CONTINUE_SCAN 0
#define EOB_ACT_END_OF_FILE 1
#define EOB_ACT_LAST_MATCH 2

    #define YY_LESS_LINENO(n)
    
/* Return all but the first "n" matched characters back to the input stream. */
#define yyless(n) \
	do \
		{ \
		/* Undo effects of setting up fortran_text. */ \
        int yyless_macro_arg = (n); \
        YY_LESS_LINENO(yyless_macro_arg);\
		*yy_cp = (yy_hold_char); \
		YY_RESTORE_YY_MORE_OFFSET \
		(yy_c_buf_p) = yy_cp = yy_bp + yyless_macro_arg - YY_MORE_ADJ; \
		YY_DO_BEFORE_ACTION; /* set up fortran_text again */ \
		} \
	while ( 0 )

#define unput(c) yyunput( c, (yytext_ptr)  )

#ifndef YY_TYPEDEF_YY_SIZE_T
#define YY_TYPEDEF_YY_SIZE_T
typedef size_t yy_size_t;
#endif

#ifndef YY_STRUCT_YY_BUFFER_STATE
#define YY_STRUCT_YY_BUFFER_STATE
struct yy_buffer_state
	{
	FILE *yy_input_file;

	char *yy_ch_buf;		/* input buffer */
	char *yy_buf_pos;		/* current position in input buffer */

	/* Size of input buffer in bytes, not including room for EOB
	 * characters.
	 */
	yy_size_t yy_buf_size;

	/* Number of characters read into yy_ch_buf, not including EOB
	 * characters.
	 */
	int yy_n_chars;

	/* Whether we "own" the buffer - i.e., we know we created it,
	 * and can realloc() it to grow it, and should free() it to
	 * delete it.
	 */
	int yy_is_our_buffer;

	/* Whether this is an "interactive" input source; if so, and
	 * if we're using stdio for input, then we want to use getc()
	 * instead of fread(), to make sure we stop fetching input after
	 * each newline.
	 */
	int yy_is_interactive;

	/* Whether we're considered to be at the beginning of a line.
	 * If so, '^' rules will be active on the next match, otherwise
	 * not.
	 */
	int yy_at_bol;

    int yy_bs_lineno; /**< The line count. */
    int yy_bs_column; /**< The column count. */
    
	/* Whether to try to fill the input buffer when we reach the
	 * end of it.
	 */
	int yy_fill_buffer;

	int yy_buffer_status;

#define YY_BUFFER_NEW 0
#define YY_BUFFER_NORMAL 1
	/* When an EOF's been seen but there's still some text to process
	 * then we mark the buffer as YY_EOF_PENDING, to indicate that we
	 * shouldn't try reading from the input source any more.  We might
	 * still have a bunch of tokens to match, though, because of
	 * possible backing-up.
	 *
	 * When we actually see the EOF, we change the status to "new"
	 * (via fortran_restart()), so that the user can continue scanning by
	 * just pointing fortran_in at a new input file.
	 */
#define YY_BUFFER_EOF_PENDING 2

	};
#endif /* !YY_STRUCT_YY_BUFFER_STATE */

/* Stack of input buffers. */
static size_t yy_buffer_stack_top = 0; /**< index of top of stack. */
static size_t yy_buffer_stack_max = 0; /**< capacity of stack. */
static YY_BUFFER_STATE * yy_buffer_stack = 0; /**< Stack as an array. */

/* We provide macros for accessing buffer states in case in the
 * future we want to put the buffer states in a more general
 * "scanner state".
 *
 * Returns the top of the stack, or NULL.
 */
#define YY_CURRENT_BUFFER ( (yy_buffer_stack) \
                          ? (yy_buffer_stack)[(yy_buffer_stack_top)] \
                          : NULL)

/* Same as previous macro, but useful when we know that the buffer stack is not
 * NULL or when we need an lvalue. For internal use only.
 */
#define YY_CURRENT_BUFFER_LVALUE (yy_buffer_stack)[(yy_buffer_stack_top)]

/* yy_hold_char holds the character lost when fortran_text is formed. */
static char yy_hold_char;
static int yy_n_chars;		/* number of characters read into yy_ch_buf */
int fortran_leng;

/* Points to current character in buffer. */
static char *yy_c_buf_p = (char *) 0;
static int yy_init = 0;		/* whether we need to initialize */
static int yy_start = 0;	/* start state number */

/* Flag which is used to allow fortran_wrap()'s to do buffer switches
 * instead of setting up a fresh fortran_in.  A bit of a hack ...
 */
static int yy_did_buffer_switch_on_eof;

void fortran_restart (FILE *input_file  );
void fortran__switch_to_buffer (YY_BUFFER_STATE new_buffer  );
YY_BUFFER_STATE fortran__create_buffer (FILE *file,int size  );
void fortran__delete_buffer (YY_BUFFER_STATE b  );
void fortran__flush_buffer (YY_BUFFER_STATE b  );
void fortran_push_buffer_state (YY_BUFFER_STATE new_buffer  );
void fortran_pop_buffer_state (void );

static void fortran_ensure_buffer_stack (void );
static void fortran__load_buffer_state (void );
static void fortran__init_buffer (YY_BUFFER_STATE b,FILE *file  );

#define YY_FLUSH_BUFFER fortran__flush_buffer(YY_CURRENT_BUFFER )

YY_BUFFER_STATE fortran__scan_buffer (char *base,yy_size_t size  );
YY_BUFFER_STATE fortran__scan_string (yyconst char *yy_str  );
YY_BUFFER_STATE fortran__scan_bytes (yyconst char *bytes,int len  );

void *fortran_alloc (yy_size_t  );
void *fortran_realloc (void *,yy_size_t  );
void fortran_free (void *  );

#define yy_new_buffer fortran__create_buffer

#define yy_set_interactive(is_interactive) \
	{ \
	if ( ! YY_CURRENT_BUFFER ){ \
        fortran_ensure_buffer_stack (); \
		YY_CURRENT_BUFFER_LVALUE =    \
            fortran__create_buffer(fortran_in,YY_BUF_SIZE ); \
	} \
	YY_CURRENT_BUFFER_LVALUE->yy_is_interactive = is_interactive; \
	}

#define yy_set_bol(at_bol) \
	{ \
	if ( ! YY_CURRENT_BUFFER ){\
        fortran_ensure_buffer_stack (); \
		YY_CURRENT_BUFFER_LVALUE =    \
            fortran__create_buffer(fortran_in,YY_BUF_SIZE ); \
	} \
	YY_CURRENT_BUFFER_LVALUE->yy_at_bol = at_bol; \
	}

#define YY_AT_BOL() (YY_CURRENT_BUFFER_LVALUE->yy_at_bol)

/* Begin user sect3 */

#define fortran_wrap(n) 1
#define YY_SKIP_YYWRAP

typedef unsigned char YY_CHAR;

FILE *fortran_in = (FILE *) 0, *fortran_out = (FILE *) 0;

typedef int yy_state_type;

extern int fortran_lineno;

int fortran_lineno = 1;

extern char *fortran_text;
#define yytext_ptr fortran_text

static yy_state_type yy_get_previous_state (void );
static yy_state_type yy_try_NUL_trans (yy_state_type current_state  );
static int yy_get_next_buffer (void );
static void yy_fatal_error (yyconst char msg[]  );

/* Done after the current pattern has been matched and before the
 * corresponding action - sets up fortran_text.
 */
#define YY_DO_BEFORE_ACTION \
	(yytext_ptr) = yy_bp; \
	fortran_leng = (size_t) (yy_cp - yy_bp); \
	(yy_hold_char) = *yy_cp; \
	*yy_cp = '\0'; \
	(yy_c_buf_p) = yy_cp;

#define YY_NUM_RULES 166
#define YY_END_OF_BUFFER 167
/* This struct is not used in this scanner,
   but its presence is necessary. */
struct yy_trans_info
	{
	flex_int32_t yy_verify;
	flex_int32_t yy_nxt;
	};
static yyconst flex_int16_t yy_accept[1100] =
    {   0,
        0,    0,    0,    0,    0,    0,    0,    0,    0,    0,
        0,    0,  167,  166,  156,  154,  165,  166,  145,  148,
      158,  166,  147,  147,  147,  150,  146,  134,  144,  149,
      152,  151,  153,  141,  141,  141,  141,  141,  141,  141,
      141,  141,  141,  141,  141,  141,  141,  141,  141,  141,
      141,  156,  154,  156,  165,  144,  141,  141,  141,  141,
      141,  141,  166,  166,  162,  166,  166,  166,  147,  141,
        0,    0,  156,    0,    0,  165,  165,  165,    0,  138,
        0,    0,  158,  158,  158,  158,    0,    0,    0,  137,
        0,    0,  131,   22,    0,  143,    0,    0,    0,    0,

        0,    0,    0,  132,    0,  144,    0,   21,  141,  141,
      141,  141,  141,  141,  141,  141,  141,  141,  141,  141,
      141,  141,  141,  141,   39,  141,  141,  141,  141,  141,
      141,  141,  141,  141,  141,  141,  141,  141,   84,  141,
      141,  141,  141,  141,  141,  141,  141,  141,  141,  141,
      141,  141,  141,  141,  141,  141,  141,  141,  141,  141,
      141,  141,  141,  141,  141,  141,  156,  156,    0,  157,
        0,    0,    0,    0,    0,    0,  155,  156,    0,  165,
      164,  165,  165,  165,  157,  144,    0,  141,  141,  141,
      141,   84,  141,  141,    0,  162,    0,    0,    0,    0,

        0,    0,  163,   22,    0,    0,    0,  141,  141,  141,
      141,  141,    0,    0,    0,  165,  165,    0,    0,  158,
      158,    0,    0,    0,    0,  136,    0,    0,    0,    0,
        0,    0,    0,    0,    0,    0,    0,    0,    0,  142,
      142,    0,  143,  141,  141,  141,  141,  141,  141,  141,
      141,  141,  141,  141,  141,  141,  114,  141,  141,  141,
        0,  141,  141,  141,  141,  141,   12,  141,  141,  141,
      113,  141,  141,  141,  141,  141,  141,    0,  141,    0,
       95,  141,  141,  141,  141,  141,  120,  141,  141,  125,
      141,  141,  141,  141,  141,  141,  141,   88,  141,  141,

      141,  141,  141,  141,  141,  141,  141,  141,  141,  141,
      141,  141,  117,  141,  141,  141,  141,  141,  121,  141,
      141,  141,  141,  141,  156,  156,    0,    0,    0,    0,
        0,    0,    0,    0,    0,  156,    0,  157,  165,  165,
      165,  144,    0,  141,  141,  141,  141,  141,  141,  141,
        0,    0,    0,    0,  163,    0,    0,    0,  141,  141,
      141,  141,  141,    0,    0,    0,  165,  165,    0,    0,
      158,  158,    0,    0,    0,    0,  143,    0,   24,    0,
       26,   25,   28,   27,   30,    0,    0,   32,    0,  143,
      124,  116,  141,  141,  119,  122,  141,  141,   18,  141,

      141,  141,  141,  141,  115,  141,  141,  141,    0,  141,
      141,  141,   93,    0,  107,  141,  141,  141,  141,  141,
      141,  141,  141,  141,    0,  108,  141,  141,  141,  141,
      141,  141,  141,  141,    0,   87,  141,  141,  141,  141,
      141,  141,  141,    0,   97,  141,  141,    0,  110,  141,
      141,  141,  141,  111,   17,  141,   58,   72,  141,  141,
      141,  141,  141,  141,  141,  141,   77,   40,  141,  141,
      141,  141,   67,  141,  126,  118,  141,   70,   53,  141,
        0,   96,   98,  141,   91,  100,  141,  141,  156,  156,
        0,    0,    0,    0,    0,    0,    0,  156,    0,  157,

      165,  165,  165,  144,    0,  141,  141,  141,  141,  141,
      141,   14,    0,    0,    0,    0,    0,    0,  141,  141,
      141,  141,    0,    0,    0,  165,  165,    0,    0,  158,
      158,    0,    0,   34,   23,    0,   31,   33,  141,  141,
      141,  141,  141,   48,  141,  141,  141,  123,  141,  141,
        0,  141,  141,  141,    0,  141,  141,    0,    0,    0,
        0,    0,    0,    0,    0,   38,  141,   94,  141,  141,
      141,  141,  141,  141,  141,  141,   74,   74,  141,    0,
      106,  112,   80,  141,  141,   87,  141,  141,   89,  141,
      141,  141,  141,  141,  141,  141,  141,  141,  141,  141,

      141,  141,  141,   51,  141,   75,  141,  141,  141,    0,
      141,  141,  141,  141,  141,  101,  141,  141,   54,   79,
      156,  156,    0,    0,    0,    0,    0,    0,  156,    0,
      157,  165,  165,  165,  144,    0,    0,  103,  141,  141,
       85,  141,  141,   68,   69,   68,    0,    0,    0,    0,
        0,  141,   48,  141,  123,    0,   19,    0,  165,   19,
        0,   19,   19,  158,   19,    0,   19,   19,   19,   29,
      141,  141,   19,  141,  141,   61,  141,  141,  141,  141,
        0,  141,  141,  135,    0,    0,   92,  141,   38,    0,
       94,    0,    0,    0,    0,    0,    0,  141,  141,  141,

      141,  141,  141,  141,  141,    0,  109,  141,  141,  141,
      141,  141,  141,  141,   64,  141,  141,  128,   99,  127,
      129,   35,  141,  141,  141,  141,  141,  141,  141,   82,
        0,  141,    7,   73,   15,  141,  141,   81,  156,  156,
        0,    0,    0,    0,  156,  165,  165,   19,    0,  141,
      141,  141,  141,    0,    0,    0,   19,    0,  141,   19,
       20,    0,  159,   20,   20,   20,   20,   20,   20,   20,
       20,  141,  141,  141,   46,  141,  141,  141,    0,    0,
      141,  141,   36,   92,    0,  141,    0,    0,    0,    0,
        0,    0,    0,  141,  141,  141,  141,  141,   71,  141,

      141,  141,    0,    0,  141,  141,   13,   49,   41,  141,
       42,    0,  141,  141,    5,  141,  141,   65,   83,    3,
        0,    0,  141,    0,  141,  141,    0,    0,    0,    0,
      165,   20,    0,  141,  141,   62,  141,    0,    0,   20,
        0,   20,  141,    4,  141,  141,   86,  141,  141,   36,
        0,    0,  141,  141,    0,  141,    0,    0,    0,    0,
        0,   71,    0,  141,  141,  141,  141,  141,   55,  141,
       63,    0,    0,    0,    0,  133,    8,   16,  141,    0,
      141,   78,   66,  141,    0,  141,    0,  141,  141,    0,
        0,    0,  165,    0,  141,   57,  141,    0,    0,    0,

      141,  130,   43,  141,   50,    0,    0,  141,  141,    0,
       56,    0,    0,    0,    0,    0,   55,  141,   37,  141,
      105,  141,  141,    0,    0,    0,    0,    0,  133,   90,
        0,  141,   59,    0,   60,    0,  141,  141,    0,   57,
        0,  165,    0,  141,  139,    0,    0,    0,  141,    6,
        0,    0,  141,  141,   56,    0,   37,    0,  105,    0,
      141,   10,  141,  141,    0,    0,    0,  141,    0,    0,
      102,    2,    0,  139,  165,    0,  141,    0,    0,    0,
       47,    0,    0,  141,  141,    0,   10,    0,   11,  141,
       52,    0,    0,    0,  141,    0,  102,    0,  165,    0,

      104,    0,    0,    0,    0,    0,  141,  141,   11,    0,
      141,    0,    0,    0,  141,    0,  104,  165,    0,    0,
        0,    0,    0,    0,   45,  141,    0,    9,    0,    0,
        0,  141,    0,  165,    0,    0,    0,  140,    0,   45,
        0,  141,    9,    0,    0,    0,    0,  141,    0,  165,
        0,    0,    0,    0,   44,    0,    0,    0,   76,    1,
      165,    0,    0,    0,   44,   76,  165,    0,    0,    0,
      165,    0,    0,    0,  165,    0,    0,    0,  165,    0,
        0,    0,  165,    0,    0,    0,  165,  160,    0,  160,
        0,    0,  160,    0,    0,    0,    0,  161,    0

    } ;

static yyconst flex_int32_t yy_ec[256] =
    {   0,
        1,    1,    1,    1,    1,    1,    1,    1,    2,    3,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    4,    5,    6,    7,    8,    9,   10,   11,   12,
       13,   14,   15,   16,   17,   18,   19,   20,   21,   22,
       23,   24,   25,   26,   27,   28,   29,   30,   31,   32,
       33,   34,    1,    1,   35,   36,   37,   38,   39,   40,
       41,   42,   43,   44,   45,   46,   47,   48,   49,   50,
       51,   52,   53,   54,   55,   56,   57,   58,   59,   60,
       61,    1,   62,    1,   63,    1,   64,   65,   66,   67,

       68,   69,   70,   71,   72,   44,   73,   74,   75,   76,
       77,   78,   79,   80,   81,   82,   83,   84,   85,   86,
       87,   88,   89,   89,   89,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,

        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1
    } ;

static yyconst flex_int32_t yy_meta[90] =
    {   0,
        1,    2,    3,    2,    1,    4,    1,    1,    1,    1,
        1,    5,    1,    1,    1,    1,    1,    4,    1,    6,
        6,    6,    6,    6,    6,    6,    6,    6,    6,    1,
        1,    1,    1,    1,    7,    8,    8,    9,    9,   10,
       10,   11,    8,    8,    8,   10,   10,   10,    7,    8,
        9,   10,   10,   10,   10,    8,   12,    8,    8,    8,
        1,    1,    8,    7,    8,    8,    9,    9,   10,   10,
       11,    8,    8,   10,   10,   10,    7,    8,    9,   10,
       10,   10,   10,    8,   12,    8,    8,    8,    4
    } ;

static yyconst flex_int16_t yy_base[1140] =
    {   0,
        0,   88,    0,    0,    0,    0,  824,   93,    0,   85,
        0,    0,  811,   64,   98,  103,   80,  129,   96,   99,
      134,  137,  145,  133,  168,  135,  249,  180,  318,  136,
      156,  212,  172,  246,  297,  366,  341,  414,  415,  247,
      239,  323,  334,  464,  466,  497,  522,  471,  571,  577,
      516,  658,  226,  453,  612,  738,  340,  786,  515,  635,
      667,  555, 3884,  805, 3884,  241,  116,  118,  730,  868,
       59,   76,  229,  253,  256,    0,  122,   92,  767, 3884,
      157,  298,  750,  726,  204,  282,  237,  667,  349, 3884,
      681,  439, 3884, 3884,  956,  730,  147,  184,  314,  539,

      446,  184,  253, 3884, 1023,  462,  944, 3884,    0,  252,
      316,  149,  345,  330,  372,  397,  379,  393,  468,  473,
      704,  468,  519,  513, 1101,  526,  537,  527,  561,  571,
      636,  581,  733,  576,  582,  701,  789,  595,  782,  562,
      607,  624,  575,  640,  674,  695,  700,  726,  736,  732,
      782,  595,  779,  611,  946,  731,  749,  937,  750,  957,
      943,  977,  667,  760,  774,  939, 1015,    0,  720,  257,
     1019,  951,  792,  940, 1006,  941,  424, 1091, 1139,  717,
     3884, 1035, 1036,  984, 1167, 1193, 1021, 1027, 1094,  981,
     1010, 1175, 1010, 1036,  694, 3884, 1133, 1103, 1074, 1107,

      685,  442, 3884,  682, 1141, 1128, 1143, 1272, 1361, 1174,
     1175, 1117, 1033, 1102,  841, 1154, 1147, 1219, 1237, 1196,
     1179, 1129, 1297, 1307, 1237, 3884, 1311, 1326, 1323, 1166,
     1180,  662,  656,  646,  631, 1217, 1212,  616, 1215, 3884,
     1274, 1377, 1390,  621, 1216, 1252, 1278, 1280, 1282, 1276,
     1287, 1308, 1355, 1355, 1374, 1366, 1384, 1382, 1383, 1391,
     1375, 1378, 1398, 1395, 1403, 1400, 1467, 1398, 1440, 1393,
        0, 1404, 1412, 1418, 1405, 1415, 1417, 1474, 1415,  845,
     3884, 1419, 1435, 1429, 1430, 1450, 1434, 1521, 1525, 1432,
     1440, 1434, 1458, 1445, 1435, 1455, 1468,    0, 1497, 1482,

     1490, 1498, 1500, 1498, 1509, 1495, 1496, 1502, 1515, 1520,
     1521, 1514, 1522, 1514, 1515, 1523, 1523,  972, 1523, 1543,
     1532, 1539, 1539, 1538,  522,  525, 1539,  598, 1547, 1554,
     1550, 1557, 1553, 1560, 1572, 1621, 1647, 1673, 1612, 1613,
     1609, 1699, 1586, 1590, 1570, 1596, 1597, 1666, 1597,  578,
     1614, 1686,  578, 1384, 3884, 1634, 1658, 1663, 1643, 1696,
     1695, 1680, 1697, 1610, 1642, 1749, 1633, 1672, 1730, 1700,
     1709, 1742, 1313, 1744, 1746, 1778, 1795,  549, 3884,  545,
     3884, 3884, 3884, 3884, 3884, 1708,  541, 3884,  524, 1788,
     3884,    0, 1712, 1742,    0,    0, 1741, 1731,    0, 1778,

     1789, 1781, 1785, 1789,    0, 1794, 1789, 1788, 1795, 1794,
     1798, 1790, 1842, 1926, 3884, 1796, 1792, 1808, 1801, 1799,
     1814, 1793, 1812, 1804, 1870, 3884, 1811, 2011, 1824, 1882,
     1827, 1845, 1854, 1837, 1843,    0, 1851, 1841, 1844, 1857,
     1864, 1859, 1869, 1770, 3884, 1859, 1875, 1784, 3884, 1864,
     1882, 1875, 1876,    0,    0, 1882,    0,    0, 1883, 1886,
     1899, 1883, 1888, 1908, 1895, 1902,    0,  503, 1901, 1910,
     1905, 1910,    0, 1911,    0,    0,  500,    0,    0, 1913,
     1985, 3884,    0, 1924,    0,    0, 1932, 1955,  735,  792,
     1963, 1914, 1968, 1969, 1967, 1970,  504, 2035, 2061, 2087,

     1988, 1996, 2007, 2113, 1996, 2090, 2031,  973, 2015, 2078,
     2143,  482, 2044, 2050, 2030, 2048, 2117, 2066, 2097, 2127,
     2130, 2132, 2031,    0, 2170, 2059,    0, 2137,  465, 2150,
      448, 2172, 1784, 3884, 3884,  429, 3884, 3884, 2129, 2135,
     2240, 2071, 2140,    0, 2112, 2140, 2136,    0, 2137, 2134,
     2147, 2154, 2155, 2160, 2219, 2163, 2162, 2158, 2154, 2170,
     2187, 2186, 2222, 2201, 2192,    0, 2217,    0, 2229, 2220,
     2229, 2242, 2230, 2242, 2247, 2281, 3884,    0, 2236, 2285,
     3884,    0,    0, 2238, 2243, 3884, 2257, 2257,    0, 2246,
     2260, 2252, 2261, 2278, 2279, 2273, 2280, 2275, 2283, 2282,

     2287, 2282, 2292,    0, 2293,    0, 2281, 2298, 2299,  413,
     2285, 2285, 2292, 2303, 2289, 3884, 2289, 2294,    0,    0,
      994, 1074, 2371, 2315, 1146, 2317, 2305, 2376, 2222,  345,
      428, 2350, 2352,  430, 2435, 2326, 2385, 3884, 2335, 2334,
     1151, 2336, 2344, 2392, 3884, 1773, 2356, 2361, 2397,  426,
     2398, 2382, 2355, 2514, 2360,    0, 3884, 2602,    0,    0,
      415,  370, 3884,  364,  361, 1941, 2217, 2415, 3884, 3884,
     2631, 2332,    0, 2367, 2351,    0, 2349, 2357, 2400, 2399,
     2370, 2430, 2408,    0, 2408, 2407,    0, 2415, 3884, 2403,
     3884, 2430, 2423, 2435, 2433, 2432, 2436, 2447, 2451, 2466,

     2472, 2483, 2473, 2485, 2492, 2547, 3884, 2504, 2483, 2493,
     2500, 2511, 2513, 2502,    0, 2502, 2510,    0,    0,    0,
        0, 2609, 2504, 2499, 2524, 2507, 2510, 2525, 2520,    0,
     2487, 2526,    0,    0,    0, 2617, 2517,    0, 1494, 1254,
     2534, 2535, 2541, 2582, 2490, 2615,  319,  311, 2612, 2613,
     2609, 1497, 2625, 2631, 2655,  307,  303, 2663, 2732, 2632,
     3884, 2666, 3884,    0,  285, 3884,  262, 2669, 2687, 3884,
        0, 2638, 2638, 2637,    0, 2639, 2651, 2639, 2656, 2717,
     2650, 2649,    0, 3884, 2687, 2673, 2691, 2675, 2690, 2713,
     2704, 2720, 2709, 2714, 2723, 2718, 2734, 2724,    0, 2735,

     2736, 2731, 2706, 2804, 2734, 2731,    0,    0,    0, 2737,
        0, 2774, 2735, 2736,    0, 2745, 2760,    0,    0,    0,
     2710, 2767, 2736, 2821, 2776, 2769, 2776, 2774, 2425, 2791,
      138,  239,  161, 2792, 2776, 2486, 2793,  351, 2783,  206,
      960, 2797, 2797,    0, 2797, 2786,    0, 2785, 2794, 3884,
     2794, 2792, 2799, 2808, 2797, 2812, 2799, 2808, 2808, 2827,
     2819, 3884, 2830, 2830, 2836, 2843, 2829, 2829,    0, 2837,
        0, 2887, 2906, 2918, 2921, 2925,    0,    0, 2849, 2853,
     2865,    0,    0, 2865,  183, 2862, 2868, 2865, 2878, 2901,
     2886, 2900, 2903, 2902, 2888,    0, 2905, 2944, 2945, 2946,

     2904,    0,    0, 2912,    0, 2906, 2918, 2909, 2923, 2922,
        0, 2919, 2924, 2929, 2914, 2917, 3884, 2925,    0, 2928,
        0, 2922, 2940, 2995, 3005, 3017, 3021, 3033, 3037,    0,
     2933, 2951,    0, 2974,    0, 2941, 2987, 2989, 2946, 3884,
     2993, 3002, 2989, 2996,    0, 3008, 3030, 3041, 3009,    0,
     3002, 3012, 3007, 3011, 3884, 3009, 3884, 3012, 3884, 3006,
     3015,    0, 3021, 3028, 3069, 3072, 3031, 3030,  181, 3053,
        0,    0, 3043, 3884,  977,  116, 3041, 3096, 3098, 1256,
        0, 3054, 3059, 3064, 3052, 3060, 3884, 3066,    0, 3063,
        0, 3110, 3138, 3076, 3078, 3070, 3884, 3068, 3101, 3076,

        0, 1766, 3104, 3107, 3086, 3076, 3072, 3084, 3884, 3089,
     3107, 3157, 3166, 3115, 3107,  128, 3884, 3117, 3118, 3168,
     3182, 3170, 3116, 3135,    0, 3131, 3142,    0, 3246, 3201,
     3132, 3136, 3129, 3197, 3138, 3188, 3194, 3884, 3198, 3884,
     3144, 3151, 3884, 3220, 3226, 3279, 3160, 3170,  114, 1892,
       40, 2593, 2611, 3165,    0, 3307, 3311, 3187,    0, 3884,
     3230, 3164, 3240, 3241, 3884, 3884, 3239, 3179, 3273, 3274,
     3259, 3195, 3275, 3293, 3296, 3283, 2831, 3327, 3282, 3271,
     3287, 3325, 3331, 3335, 3337, 3340, 3363, 3884, 3368, 3884,
     3342, 3371, 3884, 3345, 3343, 3374, 3380, 3884, 3884, 3425,

     3437, 3449, 3461, 3473, 3485, 3492, 3504, 3516, 3528, 3540,
     3547, 3556, 3568, 3580, 3592, 3604, 3611, 3619, 3631, 3643,
     3655, 3667, 3679, 3691, 3703, 3715, 3727, 3739, 3751, 3763,
     3775, 3787, 3799, 3811, 3823, 3835, 3847, 3859, 3871
    } ;

static yyconst flex_int16_t yy_def[1140] =
    {   0,
     1099,    1, 1100, 1100,    1,    2, 1101, 1101,    1,    2,
        1,    2, 1099, 1099, 1099, 1099, 1102, 1103, 1099, 1099,
     1104, 1105, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
     1099, 1099, 1099, 1106, 1106, 1106, 1106, 1106, 1106, 1106,
     1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106,
     1106, 1099, 1099,   52, 1107, 1099,   36, 1106, 1106, 1106,
     1106, 1106, 1099, 1108, 1099, 1108, 1108, 1108, 1109, 1099,
     1099, 1099, 1099, 1099, 1099, 1102, 1102, 1102, 1103, 1099,
     1103, 1103, 1104, 1099, 1104, 1104, 1105, 1110, 1105, 1099,
     1105, 1105, 1099, 1099, 1099, 1111, 1099, 1099, 1099, 1099,

     1099, 1099, 1099, 1099, 1112,   29, 1099, 1099, 1106, 1106,
     1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106,
     1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106,
     1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106,
     1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106,
     1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106,
     1106, 1106, 1106, 1106, 1106, 1106,   52,  167, 1113, 1099,
     1099, 1099, 1099, 1099, 1099, 1099, 1099,  167, 1099, 1114,
     1099, 1114, 1114, 1114, 1099, 1099, 1099, 1106, 1106, 1106,
     1106, 1106, 1106, 1106, 1108, 1099, 1108, 1108, 1108, 1108,

     1115, 1115, 1099, 1115, 1115, 1115, 1115, 1116, 1116,  209,
      209,  209, 1099, 1099, 1099, 1102, 1102, 1103, 1103, 1104,
     1104, 1110, 1110, 1110, 1110, 1099, 1105, 1105, 1099, 1099,
     1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
     1117, 1099, 1099, 1106, 1106, 1106, 1106, 1106, 1106, 1106,
     1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106,
     1118,  125, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106,
     1106, 1106, 1106, 1106, 1106, 1106, 1106, 1099, 1106, 1099,
     1099, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106,
     1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106,

     1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106,
     1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106,
     1106, 1106, 1106, 1106,  167,  167, 1099, 1113, 1099, 1099,
     1099, 1099, 1099, 1099, 1099,  178, 1099, 1099, 1114, 1114,
     1114, 1099, 1099, 1106, 1106, 1106, 1106, 1106, 1106, 1106,
     1108, 1108, 1115, 1115, 1099, 1115, 1115, 1115,  209,  209,
      209,  209,  209, 1099, 1099, 1099, 1102, 1102, 1103, 1103,
     1104, 1104, 1110, 1105, 1105, 1099, 1099, 1099, 1099, 1099,
     1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1117,
     1099, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106,

     1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1099, 1106,
     1106, 1106, 1106, 1099, 1099, 1106, 1106, 1106, 1106, 1106,
     1106, 1106, 1106, 1106, 1099, 1099, 1106, 1119, 1106, 1106,
     1106, 1106, 1106, 1106, 1099, 1106, 1106, 1106, 1106, 1106,
     1106, 1106, 1106, 1099, 1099, 1106, 1106, 1099, 1099, 1106,
     1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106,
     1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106,
     1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106,
     1099, 1099, 1106, 1106, 1106, 1106, 1106, 1106,  167,  167,
     1099, 1099, 1099, 1099, 1099, 1099, 1099,  178, 1099, 1099,

     1114, 1114, 1114, 1099, 1099, 1106, 1106, 1106, 1106, 1106,
     1106, 1099, 1108, 1108, 1108, 1115, 1115, 1115,  209,  209,
      209,  209, 1099, 1120, 1099, 1102, 1121, 1103, 1122, 1104,
     1123, 1105, 1124, 1099, 1099, 1099, 1099, 1099, 1106, 1106,
     1125, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106,
     1099, 1106, 1106, 1106, 1099, 1106, 1106, 1099, 1099, 1099,
     1099, 1099, 1099, 1099, 1099, 1106, 1106, 1106, 1106, 1106,
     1106, 1106, 1106, 1106, 1106, 1106, 1099, 1106, 1106, 1099,
     1099, 1106, 1106, 1106, 1106, 1099, 1106, 1106, 1106, 1106,
     1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106,

     1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1099,
     1106, 1106, 1106, 1106, 1106, 1099, 1106, 1106, 1106, 1106,
      167,  167, 1099, 1099, 1099, 1099, 1099, 1099,  167, 1099,
     1099, 1114, 1114, 1126, 1099, 1099, 1099, 1099, 1106, 1106,
     1099, 1106, 1106, 1099, 1099, 1099, 1108, 1108, 1115, 1127,
     1115,  209,  209, 1128,  209, 1129, 1099, 1099, 1130, 1102,
     1131, 1103, 1099, 1132, 1104, 1133, 1105, 1105, 1099, 1099,
     1134, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106,
     1099, 1106, 1106, 1106, 1099, 1099, 1106, 1106, 1099, 1099,
     1099, 1099, 1099, 1099, 1099, 1099, 1099, 1106, 1106, 1106,

     1106, 1106, 1106, 1106, 1106, 1099, 1099, 1106, 1106, 1106,
     1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106,
     1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106,
     1099, 1106, 1106, 1106, 1106, 1106, 1106, 1106,  167,  167,
     1099, 1099, 1099, 1099,  167, 1114, 1135, 1114, 1099, 1106,
     1106, 1106, 1106, 1108, 1108, 1136, 1115, 1115, 1137,  209,
     1099, 1099, 1099, 1102, 1103, 1099, 1104, 1105, 1105, 1099,
     1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1099, 1099,
     1106, 1106, 1106, 1099, 1099, 1106, 1099, 1099, 1099, 1099,
     1099, 1099, 1099, 1106, 1106, 1106, 1106, 1106, 1106, 1106,

     1106, 1106, 1099, 1138, 1106, 1106, 1106, 1106, 1106, 1106,
     1106, 1099, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1106,
     1099, 1099, 1106, 1099, 1106, 1106, 1099, 1099, 1099, 1099,
     1114, 1114, 1099, 1106, 1106, 1099, 1106, 1108, 1108, 1115,
     1115,  209, 1106, 1106, 1106, 1106, 1106, 1106, 1106, 1099,
     1099, 1099, 1106, 1106, 1099, 1106, 1099, 1099, 1099, 1099,
     1099, 1099, 1099, 1106, 1106, 1106, 1106, 1106, 1106, 1106,
     1106, 1138, 1138, 1099, 1139, 1138, 1106, 1106, 1106, 1099,
     1106, 1106, 1106, 1106, 1099, 1106, 1099, 1106, 1106, 1099,
     1099, 1099, 1114, 1099, 1106, 1106, 1106, 1108, 1108, 1115,

     1106, 1106, 1106, 1106, 1106, 1099, 1099, 1106, 1106, 1099,
     1106, 1099, 1099, 1099, 1099, 1099, 1099, 1106, 1106, 1106,
     1106, 1106, 1106, 1099, 1139, 1139, 1138, 1139, 1139, 1106,
     1099, 1106, 1106, 1099, 1106, 1099, 1106, 1106, 1099, 1099,
     1099, 1114, 1099, 1106, 1106, 1108, 1108, 1115, 1106, 1106,
     1099, 1099, 1106, 1106, 1099, 1099, 1099, 1099, 1099, 1099,
     1106, 1106, 1106, 1106, 1099, 1138, 1099, 1106, 1099, 1099,
     1106, 1106, 1099, 1099, 1114, 1099, 1106, 1108, 1108, 1115,
     1106, 1099, 1099, 1106, 1106, 1099, 1099, 1099, 1106, 1106,
     1106, 1099, 1138, 1099, 1106, 1099, 1099, 1099, 1114, 1099,

     1106, 1108, 1108, 1115, 1099, 1099, 1106, 1106, 1099, 1099,
     1106, 1099, 1138, 1099, 1106, 1099, 1099, 1114, 1099, 1108,
     1108, 1115, 1099, 1099, 1106, 1106, 1099, 1106, 1099, 1138,
     1099, 1106, 1099, 1114, 1099, 1108, 1108, 1099, 1115, 1099,
     1099, 1106, 1099, 1099, 1138, 1138, 1099, 1106, 1099, 1114,
     1099, 1108, 1115, 1099, 1106, 1138, 1139, 1099, 1106, 1099,
     1114, 1099, 1108, 1115, 1099, 1099, 1114, 1099, 1108, 1115,
     1114, 1099, 1108, 1115, 1114, 1099, 1108, 1115, 1114, 1099,
     1108, 1115, 1114, 1099, 1108, 1115, 1114, 1099, 1099, 1099,
     1108, 1115, 1099, 1108, 1108, 1108, 1108, 1099,    0, 1099,

     1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
     1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
     1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
     1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099
    } ;

static yyconst flex_int16_t yy_nxt[3974] =
    {   0,
       14,   15,   16,   15,   17,   18,   14,   19,   20,   21,
       22,   23,   24,   25,   24,   26,   24,   27,   28,   29,
       29,   29,   29,   29,   29,   29,   29,   29,   29,   24,
       30,   31,   32,   33,   34,   35,   36,   37,   38,   39,
       40,   41,   42,   41,   41,   43,   44,   45,   46,   47,
       41,   48,   49,   50,   41,   41,   51,   41,   41,   41,
       24,   24,   41,   34,   35,   36,   37,   38,   39,   40,
       41,   42,   41,   43,   44,   45,   46,   47,   41,   48,
       49,   50,   41,   41,   51,   41,   41,   41,   14,   52,
       53,   54,   55,  213,   66,   65,   66,   67,   69,   73,

       74,   73, 1062,   71,   75,   74,   75,   56,   56,   56,
       56,   56,   56,   56,   56,   56,   56,   72,  196,   77,
      196,   70,  213,  199,   57,   58, 1060,  214,   59,   67,
       60,   68,   71,   78,   80,   71,   84,   71,   71,   88,
      181,   61,   62,  217, 1033,   72,   89,   90,   77,   72,
       70,   72,   72,   57,   58,  214,  216,   59,   67,   60,
       68,   78,   80,   93,   71,  200,   71,   71,   81,   61,
       62,  217,   71,   85,   71,   71,   91,   72, 1000,   72,
       72,   94,   82,  246,   71,  216,   72,   86,   72,   72,
       92,  218,  104,  200,  230,   71,  996,   81,   72,  934,

      893,   71,   85,   71,   71,   91,   84,   71,  203,   72,
       82,   71,  246,   71,   72,   86,   72,   72,   92,   71,
      218,   72,  230,  894,   71,   72,   72,   75,   74,   75,
       73,   74,   73,   72,  231,  238,   71,   72,  220,   88,
       71,  181,  197,  196,  197,  108,   89,   90,   71,   72,
       95,   71,   95,   72,   75,   74,   75,  215,   74,  215,
      170,   72,  231,  238,   84,   72,  170,  220,   96,   96,
       96,   96,   96,   96,   96,   96,   96,   96,  112,  198,
       71,  110,  111,   97,   84,  112,  112,   98,   71,   99,
       80,  113,  117,   72,  100,  136,  101,  102,  114,  115,

      117,  239,   72,   80,  244,  203,  103,  112,  198,  203,
      110,  111,   97,  181,  112,  112,   98,   71,   99,  113,
      117,  181,  100,  136,  101,  102,  114,  115,  117,  239,
       72,  116,  244,  221,  103,  105,  112,  106,  106,  106,
      106,  106,  106,  106,  106,  106,  106,  187,  170,  219,
      117,   88,  232,  196,  170,  107,  107,   71,   89,   90,
      116,  221,  137,   84,  245,  112,   84,  233,  107,  138,
      139,   72,  248,  112,  188,   80,  117,  219,  117,  123,
      112,  232,  140,  124,  107,  107,   71,  117,  189,  125,
      247,  137,  245,  126,  117,  233,  107,  138,  139,   72,

      118,  248,  112,  188,  117,  112,  249,  119,  123,  112,
      140,  120,  124,  898,  121,  117,  189,  125,  247,  117,
      766,  126,  117,  250,  122,   75,   74,   75,  203,  118,
      250,  170,  181,  251,  112,  249,  119,  170,  252,  120,
      731,   88,  121,  354,  355,  354,  670,  117,   89,   90,
       84,  250,  122,  112,  112,  177,  178,  132,  250,  127,
      133,  128,  251,  134,  129,  130,  252,  117,  117,  135,
      663,  131,  179,  179,  179,  179,  179,  179,  179,  179,
      179,  179,  112,  112,  236,  512,  132,  127,  133,  128,
      228,  134,  129,  130,  237,  117,  117,  135,  141,  131,

      144, 1099,  253,  112,  258,  112,  142,  512,  145,  155,
      112,  616,  143,  236,  146, 1099,  610,  117,  228,  117,
      147,  254,  237,  489,  117,  490,  489,  141,  490,  144,
     1099,  253,  112,  258,  112,  142,  112,  145,  155,  112,
      143,  538,  146, 1099,  148,  117,  149,  117,  147,  254,
      117,  150,  117,  259,  112,  112,  151,  165,  537,  260,
      191,  112,  535,  136,  267,  112,  534,  166,  117,  117,
      152,  265,  148,  153,  149,  117,  154,  234,  117,  150,
      203,  512,  259,  112,  112,  151,  165,  260,  191,  266,
      112,  136,  235,  267,  112,  166,  117,  117,  152,  265,

      181,  153,  287,  117,  154,  156,  234,  194,  117,  157,
      112,  162,  290,  158,  181,  268,  112,  266,  163,  182,
      235,  159,  269,  112,  160,  161,  273,  276,  164,  277,
      117,  287,  391,  388,  156,  194,  117,  301,  157,  112,
      162,  290,  158,  268,  282,  112,  304,  163,  384,  159,
      269,  183,  160,  161,  273,  276,  164,  277,  117,  167,
       74,  168,  169,  383,  288,  184,  301,  170,  223,  224,
      223,  289,  282,  382,  137,  304,  225,  226,  270,  381,
      183,  138,  192,   88,  203,  271,  291,  203,  117,  272,
       89,   90,  288,  184,  171,  172,  196,   71,  173,  289,

      174,  162,  278,  137,  278,  321,  112,  270,  163,  138,
      192,  175,  176,  271,  291,  227,  117,  272,  164,  181,
      117,  292,  181,  171,  172,  193,   71,  173,   84,  174,
      162,  202,  203,  202,  321,  112,  621,  163,  622,  175,
      176,  185,  293,  204,  227,  294,  164,  170,  117,  292,
      255,  256,   84,  193,  279,  105,  257,  186,  186,  186,
      186,  186,  186,  186,  186,  186,  186,  229,  229,  205,
      293,  295,   80,  294,  296,  107,  107,   71,  255,  256,
      229,  274,  279,  206,  257,  298,  310,  275,  107,  297,
      280,   72,  280,  621,  311,  622,  229,  229,  205,  295,

      281,  314,  322,  296,  107,  107,   71,  196,  229,  274,
     1099,  206,  323,  298,  310,  275,  107,  297,  283,   72,
      190,  302,  311,  246,  123,  112,   65,  303,  124,  314,
      284,  322,  285,  299,  125,  286,  300,  332,  126,  117,
     1099,  323,  366,   74,  366, 1099,  280,  283,  280,  190,
      302, 1099,  246,  123,  112,  303,  281,  124,  284, 1099,
      285,  299,  125,  286,  300,  332,  126,  117,  201,  202,
      203,  202,  201,  201,  201,  207,  201,  201,  201,  201,
      201,  201,  201,  201,  201,  201,  201,  208,  208,  208,
      208,  208,  208,  208,  208,  208,  208,  201,  201,  201,

      201,  201,  188,  208,  208,  208,  208,  209,  208,  119,
      208,  208,  208,  210,  208,  208,  189,  208,  208,  208,
      208,  211,  208,  208,  208,  208,  212,  208,  201,  201,
      208,  188,  208,  208,  208,  208,  209,  208,  119,  208,
      208,  210,  208,  208,  189,  208,  208,  208,  208,  211,
      208,  208,  208,  208,  212,  208,  201,   95,  242,   95,
      242, 1099,  203,  243,  243,  243,  243,  243,  243,  243,
      243,  243,  243,  481,  641,  481,  641,  312,  317,  181,
      305,  324,  306,  482,  313,  331,  181,  333, 1099,  318,
       97,  315, 1099,  335,   98,  739,   99,  740,  307,  308,

     1099,  100,  309,  101,  102,  316,  312,  317,  250,  305,
      324,  306,  313,  103,  331,  333,  325,  318,  326,   97,
      315,  335,  900,   98,  319,   99,  307,  308,  320,  100,
      309,  101,  102,  316,  346,  341,  250,  181,  181,  999,
     1099,  103,  241,  241,  241,  241,  241,  241,  241,  241,
      241,  241,  319,  329, 1099,  343,  320,  214,  347,  349,
      107,  107,  346,  341,  334, 1099, 1099,  330,  327,  339,
      340, 1099,  252,  107,  350,  739,  196,  740,  364,  344,
     1099, 1099,  329, 1099,  343,  214,  347,  349, 1099,  107,
      107, 1099,  334,  177,  336,  330,  327, 1099,  339,  340,

      252,  107,  261,  350,  261,  196,  364,  344,  351,  196,
      337,  337,  337,  337,  337,  337,  337,  337,  337,  337,
      262,  262,  262,  262,  262,  262,  262,  262,  262,  262,
      203,  224, 1099, 1099,  197,  196,  197,  351,  373,  226,
      255,  345,  338,  203,  352,  203,  257,  641,  170,  641,
      200,  359,  641,  363,  641,  263,  365,  264,  337,  337,
      337,  337,  337,  337,  337,  337,  337,  337,  255,  345,
      338,  198, 1099,  352,  257,  356,  170,  358,  200,  357,
      359,   84,  363,  263,  365,  264,  337,  337,  337,  337,
      337,  337,  337,  337,  337,  337,  338,  379,   84,  367,

      198,  368,  170,  378,  356, 1099,  358,  357,  359,  359,
      105,  283,  342,  342,  342,  342,  342,  342,  342,  342,
      342,  342,  361,  284,   80,  285,  362,  367,  348,  368,
      107,  107,  378,  372,  385,  380, 1099,  359,  359,  224,
      283,  371,   80,  107, 1099, 1099,  373,  226, 1099, 1099,
      361,  284, 1099,  285,  362,  739,  348,  740,  203,  107,
      107,  372, 1099,  380,  369,  387,  389,  386,  392,  371,
     1099,  107,  353,  353,  203,  353,  353,  353,  353,  353,
      353,  353,  353,  353,  353,  353,  353,  353,  353,  353,
      353,  370,  369,  387,  389,  386,  392,  393,  223,  224,

      223,  353,  353,  353,  353,  353,  225,  226,  223,  224,
      223,  229,  229,   88, 1099,  224,  225,  226, 1004,  370,
       89,   90,  373,  226,  229,  393,  394,  395,   88,  396,
      397,  398,  353,  353, 1099,   89,   90,  376, 1099,  376,
      229,  229,  377,  377,  377,  377,  377,  377,  377,  377,
      377,  377,  229,  399,  394,  395,  374,  396,  397,  398,
      353,  353,  353,  203,  353,  353,  353,  353,  353,  353,
      353,  353,  353,  353,  353,  353,  353,  353,  353,  353,
      375,  399, 1099, 1099,  374,  354,  355,  354, 1099, 1099,
      353,  353,  353,  353,  353,  360,  243,  243,  243,  243,

      243,  243,  243,  243,  243,  243,  400,  401,  375,  243,
      243,  243,  243,  243,  243,  243,  243,  243,  243,  404,
      402,  353,  353,  403,  360,  405, 1099,  406,  407,  408,
     1099,  409,  109,  410,  400,  401,  411,  412,  413, 1099,
      424,  425,  429,  425, 1099,  427,  428,  404,  402,  353,
      430,  403,  431,  434,  405,  406,  407,  432,  408,  409,
      109,  433,  410,  436,  437,  411,  412,  413,  414,  424,
      414,  429,  426,  427,  428,  278,  443,  278,  453,  430,
      438,  431,  434,  439,  440,  432,  452,  454,  441,  433,
      456,  436,  437,  457, 1099,  739,  455,  739,  836,  415,

      836,  442,  458, 1099,  416,  443,  417,  453,  438,  418,
      459,  439,  440,  419,  452,  454,  420,  441,  456,  421,
      422,  457,  444,  423,  444,  455,  448,  435,  448,  442,
      458,  460,  445,  416,  461,  417,  449,  462,  418,  459,
      465,  419, 1099,  466,  420,  463,  467,  421,  422,  469,
      470,  423, 1099,  464,  468,  435,  471,  472,  473,  474,
      460,  475,  461,  476,  483,  462,  446,  477,  478,  465,
      450,  466,  479,  463,  480,  467,  447,  469,  470,  485,
      451,  464,  468,  484,  471,  486,  472,  473,  474,  475,
      487,  488,  476,  483,  446,  477,  478,  334,  450,  491,

      479,  492,  480,  493,  447,  494,  495,  485,  451,  496,
      497,  181,  484,  486,  181,  181,  196, 1099,  487,  488,
     1099, 1099,  489,  507,  498,  334,  505,  491,  506,  492,
      508,  493,  509,  494,  495,  511,  203,  496, 1099,  497,
      499,  499,  499,  499,  499,  499,  499,  499,  499,  499,
      500,  507,  501, 1099,  513,  505,  170,  506,  502,  508,
      203,  509,  523,  503,  511,  203,  499,  499,  499,  499,
      499,  499,  499,  499,  499,  499,  500,  359, 1099,  516,
      524,  501,  170,  513, 1099,  526,  502,  514,  196,  514,
      523,  503,  499,  499,  499,  499,  499,  499,  499,  499,

      499,  499,  500,  518,  510,   80,  359,  516,  170,  524,
      527,   84,  517,  526,  359, 1099,  105,  442,  504,  504,
      504,  504,  504,  504,  504,  504,  504,  504,  515,  359,
      359,  359,  518,  510,  521,   80,  107,  107,  529,  527,
      517,  519,  522,  359,   84,  442,   88,  520,   88,  107,
      525,   74,  525,   89,   90,   89,   90,  515,  359,  359,
      359,  530,  521,  536,  539,  107,  107,  529,  196,  519,
      522,  444, 1099,  444,  646,  520,  646,  107,  540,  541,
      531,  445,  528,  542,  533,  448,   88,  448,  646,  530,
     1099,  536,  539,  668,  669,  449,  532,  377,  377,  377,

      377,  377,  377,  377,  377,  377,  377,  540,  541,  531,
      528,  542,  543,  533,  377,  377,  377,  377,  377,  377,
      377,  377,  377,  377,  532,  229,  229,  544, 1020,  545,
      546,  547,  548, 1099,  549,  550,  551, 1099,  229,  552,
      553,  543,  554,  555,  566,  555,  567,  568, 1099,  569,
      570,  573,  571,  574,  229,  229,  544,  545,  546,  575,
      547,  548,  549,  550,  576,  551,  229,  552,  572,  553,
      554,  425,  566,  425,  567,  579,  568,  569,  570,  573,
      582,  571,  574,  580,  556,  580,  583,  575,  584, 1099,
      585,  586,  576,  587,  181,  588,  572,  589,  557,  590,

     1099,  593,  426,  579,  591,  594, 1099,  595,  582,  596,
     1099,  592,  597,  556,  581,  583,  598,  584,  585,  586,
      599,  600,  587,  588,  601,  589,  557,  414,  590,  414,
      593,  602,  603,  591,  594,  595,  605,  604,  596,  592,
      597,  606,  607,   88,  609,  598,  608,  615,  599,  600,
      769,  770,  611,  601, 1061,  612,  613,  614,  415,  602,
      603,  617,  618,  558,  605,  559,  604,  624,  560,  606,
      619,  607,  561,  609,  608,  562,  615, 1099,  563,  564,
      611, 1099,  565,  612,  613,  614,  481, 1099,  481,  617,
      181,  618,  558,  620,  559,  624,  482,  560,  181,  619,

      561,  623,  625,  562,  626,  627,  563,  564,  628,  181,
      565,  577,  577,  577,  577,  577,  577,  577,  577,  577,
      577,  577,  620,  577,  577,  577,  577,  577,  577,  577,
      623,  625,  196,  626,  627, 1099,  621,  628,  629,  632,
      577,  577,  577,  577,  577,  634,  196,  636,  633,  642,
      203,  514,  196,  514,  630,  630,  630,  630,  630,  630,
      630,  630,  630,  630,  631,  640, 1099,  632,  203,  656,
      170,  577,  577,  547,  634,  636,  633,  648,  642, 1099,
      630,  630,  630,  630,  630,  630,  630,  630,  630,  630,
      631,  637,  515,  637,  640,  647,  170,  659,  656,  577,

      649,  638,  547, 1099, 1099,  648,  630,  630,  630,  630,
      630,  630,  630,  630,  630,  630,  631,  651,  591,  203,
      674,  515,  170,  647, 1099,  592,  659,  639,  649,  643,
      105,  359,  635,  635,  635,  635,  635,  635,  635,  635,
      635,  635,   80, 1099,  644,  651,  644,  591,  674,  652,
      107,  107,   84,  592,  645,  650,  639,  643,  646,  676,
      359,  359, 1099,  107,  359,  653,  359,  671,  654,  672,
      655,  658,   74,  658,   88,  661,  675,  652,  677,  107,
      107,   89,   90,  678,  650,  679,  680,  676,  664,  681,
      359,  107,  682,  359,  653,  359,  671,  654,  672,  655,

      683,  684,  687,  688,  661,  675,  689,  677,  690,  691,
      666,  678, 1099,  679,  680, 1099, 1099,  664,  681,   88,
      555,  682,  555,  739,  177,  745,   89,   90,  683, 1099,
      684,  687,  688,  697,  689,  692,  690,  693,  691,  666,
      657,  657, 1099,  657,  657,  657,  657,  657,  657,  657,
      657,  657,  657,  657,  657,  657,  657,  657,  657,  696,
      694,  685,  697,  692,  698,  693,  699, 1099,  700,  657,
      657,  657,  657,  657,  701,  686,  695,  702, 1099,  703,
      704,  705,  706,  708,  706,  710,  580,  696,  580,  694,
      685,  709,  698,  711,  712,  699,  700,  713,  714, 1099,

      657,  657,  701,  686,  695,  715,  702,  703,  716,  704,
      705,  708,  717,  707,  710,  718,  720,  581,  719,  709,
      721,  722,  711,  712,  723,  713,  724,  714,  657,  725,
      726,  727,  729,  715,  728,  730,  716,  732,  733,  734,
      735,  717,  736,  737,  718,  720,  719,  738,  721,  742,
      722,  743,  181,  723,  181,  724,  744,  725,  196,  726,
      727,  729,  728,  196,  730,  732,  733,  734,  749,  735,
      736,  737,  637,  750,  637,  738,  751,  644,  742,  644,
      743,  752,  638,  753,  744,  772,  637,  645,  637,  359,
      747,  646,  746,  644,  359,  644,  638,  749,  754,  203,

      203,  773,  750,  645,  774,  751,  775,  646,  741,  752,
     1099,  776,  753,  772,  755,  779,  359,   88,  359,  747,
      759,  746,  741,  359,   89,   90,  836,  754,  836, 1099,
      773,  780,  774,  780,  775,  756,  777,  741,  170,  776,
      758,  778,  755,  779,  170,  359,  783,  784,  785,  759,
      787,  741,  105,  786,  106,  106,  106,  106,  106,  106,
      106,  106,  106,  106,  756,  777,  781,  788,  791,  758,
      778,  789,  107,  107,  793,  783,  784,  785,  787,  782,
      790,  792,  786,  794,  803,  107,  803,  836,  821,  836,
      821,  739,  177,  745,  804,  781,  788,  791,  822,  789,

     1099,  107,  107,  793, 1099,  795,  796,  782,  790,  792,
      797,  799,  794,  107,  757,  757,  203,  757,  757,  757,
      757,  757,  757,  757,  757,  757,  757,  757,  757,  757,
      757,  757,  757,  795,  798,  796,  800,  801,  802,  797,
      799,  805,  806,  757,  757,  757,  757,  757,  706,  807,
      706,  808, 1099,  809,  810,  811,  814,  815,  816, 1099,
      817,  818,  798,  819,  800,  801,  820,  802,  823,  805,
      826,  806,  827, 1099,  757,  757, 1099,  828,  807,  707,
      808,  809,  810,  811,  814,  815,  829,  816,  817,  818,
     1099, 1099,  819, 1099,  820,  196, 1099,  823,  826, 1099,

     1099,  827,  757,  762,   74,  762,  828, 1099,  763,  763,
      812,  763,  812,  203,  829,  763,  763,  181,  824,  763,
      824,  830,  763,  763,  763,  763,  763,  763,  763,  763,
      763,  761,  761,  196,  761,  761,  761,  761,  761,  761,
      761,  761,  761,  761,  761,  761,  761,  761,  761,  761,
      830,  833,  834,  825,  831, 1063,  835,  196,  813,  837,
      761,  761,  761,  761,  761,  203,  359,  762,   74,  762,
      838,   88,  843, 1064,  845,  846,  844,  847,   89,   90,
      833,  834,  825,  831,  835,  848,  813,  849,  837,   88,
     1099,  761,  761,  839,  850,  359,   89,   90,  853,  838,

      854,  843,  841,  845,  846,  844,  847,  803, 1099,  803,
     1099,  821, 1099,  821,  848,  849, 1099,  804,  780,  761,
      780,  822,  839,  850,  856,  855,  853,  857,  854,  858,
      859,  841,  840,  840,  203,  840,  840,  840,  840,  840,
      840,  840,  840,  840,  840,  840,  840,  840,  840,  840,
      840,  860,  856,  851,  855,  861,  857,  858,  862,  859,
      863,  840,  840,  840,  840,  840,  852,  864,  865,  866,
      867, 1099,  868,  869,  870,  812,  871,  812, 1099,  879,
      860,  877,  851,  861,  878,  196,  881,  862,  863,  882,
      883,  886,  840,  840,  852,  864,  865,  866,  884,  867,

      868,  885,  869,  870,  871,  873,  874,  873,  879,  877,
      888,  889,  878,  875,  881,  890,  876,  882,  883,  886,
      840,  891,  824,  880,  824,  892,  895,  884,  896,  897,
      885,  359,  901,  196,  899,  902, 1099,  903,  904,  888,
      889,  905,  906,  907,  890,  908,  909, 1099,  910,  891,
      911,  880,  912,  913,  892,  895,  896,  887,  897,  914,
      359,  901,  899,  915,  902,  903,  904,  916,  917,  905,
      906,  907,  918,  908,  919,  909,  910,  920, 1099,  911,
      912,  913,  921,  922,  923,  930,  887,  914,  873,  874,
      873, 1099,  915, 1081, 1099,  916,  875,  917, 1099,  876,

      935,  918,  936,  919,  931,  181,  920,  873,  874,  873,
      921,  922,  923,  932,  930,  875,  933,  937,  876,  924,
      874,  924,  926,  927,  926,  938,  873,  874,  873,  935,
      928,  936,  931,  929,  875,  939,  941,  876,  940,  943,
      942,  932,  944,  945,  933,  937,  196,  196,  203,  949,
      950, 1099,  951,  938, 1099, 1099,  952, 1099,  953,  954,
      955,  956,  957,  958,  939,  941,  940,  959,  943,  942,
      944,  960,  945,  961,  962,  963,  964,  949, 1099,  950,
      951,  967,  946,  948,  947,  952,  953,  968,  954,  955,
      956,  957,  958,  970, 1099,  959,  965,  874,  965,  960,

      973,  961,  962,  963,  181,  964,  926,  927,  926,  967,
      196,  946,  948,  947,  928,  969,  968,  929,  926,  927,
      926,  970,  966,  927,  966,  971,  928,  972,  973,  929,
      875,  974,  196,  876,  926,  927,  926,  976,  926,  927,
      926,  977,  928,  203,  969,  929,  928,  981,  983,  929,
      975,  982,  984,  985,  971,  978,  972,  986,  987,  988,
      974, 1099,  989,  990,  979,  976,  991,  994,  995,  977,
      992,  874,  992,  993,  874,  993,  981,  983,  975,  982,
      984,  875,  985,  978,  876,  986,  987,  988,  998,  980,
      989,  997,  990,  979, 1001,  991,  994,  995,  196, 1005,

      196, 1006, 1007,  181, 1008, 1099,  196, 1009, 1010,  203,
     1011, 1012,  874, 1012, 1014, 1015,  998,  980, 1016,  181,
      997, 1017, 1001, 1019, 1023, 1099, 1026, 1005, 1024, 1025,
     1006, 1007, 1008, 1002, 1003, 1009, 1027, 1010, 1011, 1013,
      874, 1013, 1021, 1014, 1015, 1028, 1016,  875, 1018, 1017,
      876, 1019, 1031, 1023, 1022, 1026, 1024, 1025, 1029,  874,
     1029, 1032, 1002, 1003, 1027, 1034, 1035, 1030,  874, 1030,
      196, 1021,  203, 1040, 1028,  875, 1018, 1041,  876, 1042,
     1043, 1031, 1022, 1037, 1038, 1037, 1047, 1048, 1049, 1032,
      196, 1051, 1054, 1034, 1035, 1037, 1038, 1037, 1055,  181,

      203, 1040, 1046,  874, 1046, 1036, 1041, 1042, 1059, 1043,
      875, 1058, 1065,  876, 1047, 1048, 1049, 1068, 1039, 1051,
     1054, 1044,  874, 1044, 1099, 1066, 1055,  873,  874,  873,
     1072, 1099,  181, 1076, 1036,  875, 1052, 1059,  876, 1058,
     1065,  181,  196,  203, 1099, 1068, 1039, 1044,  874, 1044,
     1050, 1053, 1045, 1045, 1066, 1045, 1099, 1099, 1072, 1045,
     1045,  181, 1076, 1045, 1052, 1099, 1045, 1045, 1045, 1045,
     1045, 1045, 1045, 1045, 1045,  196,  203,  196, 1050, 1053,
      873,  874,  873, 1067,  181, 1056, 1056, 1069, 1057,  196,
     1071,  876, 1056, 1056, 1070,  203, 1056, 1075,  181, 1056,

     1056, 1056, 1056, 1056, 1056, 1056, 1056, 1056,  873,  874,
      873, 1067,  926,  927,  926, 1069,  875, 1080, 1071,  876,
      928, 1073, 1070,  929, 1084, 1074, 1075,  203, 1077,  203,
     1079, 1078, 1087, 1088, 1087, 1083, 1089, 1090, 1089,  196,
     1085, 1092, 1093, 1092,  196,  196, 1080,  196, 1099, 1073,
     1099, 1099, 1084, 1074, 1099, 1099, 1077, 1099, 1099, 1079,
     1078, 1082, 1099, 1083, 1087, 1088, 1087, 1099, 1085, 1089,
     1090, 1089, 1092, 1093, 1092, 1097, 1098, 1097, 1086, 1095,
     1094, 1097, 1098, 1097, 1099, 1099, 1099, 1099, 1091, 1099,
     1082, 1099, 1099, 1099, 1099, 1099, 1096, 1099, 1099, 1099,

     1099, 1099, 1099, 1099, 1099, 1099, 1086, 1099, 1095, 1094,
     1099, 1099, 1099, 1099, 1099, 1099, 1091, 1099, 1099, 1099,
     1099, 1099, 1099, 1099, 1096,   63,   63,   63,   63,   63,
       63,   63,   63,   63,   63,   63,   63,   64,   64,   64,
       64,   64,   64,   64,   64,   64,   64,   64,   64,   76,
       76, 1099,   76,   76,   76,   76,   76,   76,   76,   76,
       76,   79,   79, 1099,   79,   79,   79,   79,   79,   79,
       79,   79,   79,   83,   83,   83,   83,   83,   83,   83,
       83,   83,   83,   83,   83,   87,   87,   87,   87,   87,
       87,   87,   87,   87,   87,   87,   87,  109,  109,  109,

      109,  109,  109,  109,  180,  180,  180,  180,  180,  180,
      180,  180,  180,  180,  180,  180,  195,  195,  195,  195,
      195,  195,  195,  195,  195,  195,  195,  195,  201,  201,
      201,  201,  201,  201, 1099,  201,  201,  201, 1099,  201,
      222,  222,  222,  222,  222,  222,  222,  222,  222,  222,
      222,  222,   96, 1099, 1099,   96,  240,  240,  240, 1099,
      240,  240, 1099,  240,  240, 1099,  240,  240,  328,  328,
      328,  328,  328,  328,  328,  328,  328,  328,  328,  328,
      180,  180,  180,  180,  180,  180,  180,  180,  180,  180,
      180,  180,  353,  353,  353,  353,  353,  353,  353,  353,

      353,  353,  353,  353,  359,  359,  359,  359,  359,  359,
      359,  359,  359,  359,  359,  359,  390, 1099, 1099,  390,
      261, 1099, 1099, 1099,  261, 1099, 1099, 1099, 1099, 1099,
      261,  578,  578,  578,  578, 1099,  578,  578,  578,  578,
      578,  578,  578,  657,  657, 1099,  657,  657,  657,  657,
      657,  657,  657,  657,  657,  660,  660, 1099,  660,  660,
      660,  660,  660,  660,  660,  660,  660,  662,  662, 1099,
      662,  662,  662,  662,  662,  662,  662,  662,  662,  665,
      665,  665,  665,  665,  665,  665,  665,  665,  665,  665,
      665,  667,  667,  667,  667,  667,  667,  667,  667,  667,

      667,  667,  667,  673,  673, 1099,  673,  673,  673,  673,
      673,  673,  673,  673,  673,  748,  748,  748,  748,  748,
      748,  748,  748,  748,  748,  748,  748,  757,  757,  757,
      757,  757,  757,  757,  757,  757,  757,  757,  757,  760,
      760,  760,  760,  760,  760,  760,  760,  760,  760,  760,
      760,  761,  761, 1099,  761,  761,  761,  761,  761,  761,
      761,  761,  761,  764,  764, 1099,  764,  764,  764,  764,
      764,  764,  764,  764,  764,  765,  765, 1099,  765,  765,
      765,  765,  765,  765,  765,  765,  765,  767,  767,  767,
      767,  767,  767,  767,  767,  767,  767,  767,  767,  768,

      768,  768,  768,  768,  768,  768,  768,  768,  768,  768,
      768,  771,  771, 1099,  771,  771,  771,  771,  771,  771,
      771,  771,  771,  832,  832,  832,  832,  832,  832,  832,
      832,  832,  832,  832,  832,  840,  840,  840,  840,  840,
      840,  840,  840,  840,  840,  840,  840,  842,  842,  842,
      842,  842,  842,  842,  842,  842,  842,  842,  842,  872,
      872,  872,  872,  872,  872,  872,  872,  872,  872,  872,
      872,  925,  925,  925,  925,  925,  925,  925,  925,  925,
      925,  925,  925,   13, 1099, 1099, 1099, 1099, 1099, 1099,
     1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,

     1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
     1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
     1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
     1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
     1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
     1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
     1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
     1099, 1099, 1099
    } ;

static yyconst flex_int16_t yy_chk[3974] =
    {   0,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    1,
        1,    1,    1,    1,    1,    1,    1,    1,    1,    2,
        2,    2,    2,   71,    8,    8,    8,    8,   10,   15,

       15,   15, 1051,   14,   16,   16,   16,    2,    2,    2,
        2,    2,    2,    2,    2,    2,    2,   14,   67,   17,
       68,   10,   71,   67,    2,    2, 1049,   72,    2,    8,
        2,    8,   14,   17,   18,   19,   21,   15,   20,   22,
      831,    2,    2,   78, 1016,   14,   22,   22,   17,   19,
       10,   15,   20,    2,    2,   72,   77,    2,    8,    2,
        8,   17,   81,   23,   19,   68,   15,   20,   18,    2,
        2,   78,   24,   21,   26,   30,   22,   19,  976,   15,
       20,   25,   18,  112,   23,   77,   24,   21,   26,   30,
       22,   81,   28,   68,   97,   31,  969,   18,   23,  885,

      831,   24,   21,   26,   30,   22,   85,   25,  840,   31,
       18,   33,  112,   23,   24,   21,   26,   30,   22,   28,
       81,   25,   97,  833,   31,   33,   23,   53,   53,   53,
       73,   73,   73,   28,   98,  102,   25,   31,   85,   87,
       33,  832,   66,   66,   66,   32,   87,   87,   28,   25,
       27,   32,   27,   33,   74,   74,   74,   75,   75,   75,
      170,   28,   98,  102,  767,   32,  170,   85,   27,   27,
       27,   27,   27,   27,   27,   27,   27,   27,   41,   66,
       32,   34,   34,   27,   86,   34,   40,   27,   27,   27,
      765,   34,   41,   32,   27,   40,   27,   27,   34,   34,

       40,  103,   27,   82,  110,  757,   27,   41,   66,  756,
       34,   34,   27,  748,   34,   40,   27,   27,   27,   34,
       41,  747,   27,   40,   27,   27,   34,   34,   40,  103,
       27,   35,  110,   86,   27,   29,   35,   29,   29,   29,
       29,   29,   29,   29,   29,   29,   29,   57,  630,   82,
       35,   89,   99,  838,  630,   29,   29,   29,   89,   89,
       35,   86,   42,  665,  111,   35,  664,   99,   29,   42,
       42,   29,  114,   43,   57,  662,   42,   82,   35,   37,
       37,   99,   43,   37,   29,   29,   29,   43,   57,   37,
      113,   42,  111,   37,   37,   99,   29,   42,   42,   29,

       36,  114,   43,   57,   42,   36,  115,   36,   37,   37,
       43,   36,   37,  838,   36,   43,   57,   37,  113,   36,
      661,   37,   37,  115,   36,  177,  177,  177,  650,   36,
      117,  631,  634,  116,   36,  115,   36,  631,  118,   36,
      610,   92,   36,  202,  202,  202,  536,   36,   92,   92,
      531,  115,   36,   38,   39,   54,   54,   39,  117,   38,
       39,   38,  116,   39,   38,   38,  118,   38,   39,   39,
      529,   38,   54,   54,   54,   54,   54,   54,   54,   54,
       54,   54,   38,   39,  101,  512,   39,   38,   39,   38,
       92,   39,   38,   38,  101,   38,   39,   39,   44,   38,

       45,  106,  119,   44,  122,   45,   44,  497,   45,   48,
       48,  477,   44,  101,   45,  106,  468,   44,   92,   45,
       45,  120,  101,  325,   48,  325,  326,   44,  326,   45,
      106,  119,   44,  122,   45,   44,   46,   45,   48,   48,
       44,  389,   45,  106,   46,   44,   46,   45,   45,  120,
       46,   46,   48,  123,   59,   51,   47,   51,  387,  124,
       59,   47,  380,   59,  128,   46,  378,   51,   59,   51,
       47,  126,   46,   47,   46,   47,   47,  100,   46,   46,
      353,  350,  123,   59,   51,   47,   51,  124,   59,  127,
       47,   59,  100,  128,   62,   51,   59,   51,   47,  126,

      328,   47,  140,   47,   47,   49,  100,   62,   62,   49,
       49,   50,  143,   49,   55,  129,   50,  127,   50,   55,
      100,   49,  130,   62,   49,   49,  132,  134,   50,  135,
       50,  140,  244,  238,   49,   62,   62,  152,   49,   49,
       50,  143,   49,  129,  138,   50,  154,   50,  235,   49,
      130,   55,   49,   49,  132,  134,   50,  135,   50,   52,
       52,   52,   52,  234,  141,   55,  152,   52,   88,   88,
       88,  142,  138,  233,   60,  154,   88,   88,  131,  232,
       55,   60,   60,   91,  204,  131,  144,  201,   60,  131,
       91,   91,  141,   55,   52,   52,  195,   52,   52,  142,

       52,   61,  136,   60,  136,  163,   61,  131,   61,   60,
       60,   52,   52,  131,  144,   91,   60,  131,   61,  180,
       61,  145,  169,   52,   52,   61,   52,   52,   84,   52,
       61,   69,   69,   69,  163,   61,  489,   61,  489,   52,
       52,   56,  146,   69,   91,  147,   61,   56,   61,  145,
      121,  121,   83,   61,  136,   56,  121,   56,   56,   56,
       56,   56,   56,   56,   56,   56,   56,   96,   96,   69,
      146,  148,   79,  147,  149,   56,   56,   56,  121,  121,
       96,  133,  136,   69,  121,  150,  156,  133,   56,  149,
      137,   56,  137,  490,  157,  490,   96,   96,   69,  148,

      137,  159,  164,  149,   56,   56,   56,   64,   96,  133,
       13,   69,  165,  150,  156,  133,   56,  149,  139,   56,
       58,  153,  157,  137,   58,   58,    7,  153,   58,  159,
      139,  164,  139,  151,   58,  139,  151,  173,   58,   58,
        0,  165,  215,  215,  215,    0,  280,  139,  280,   58,
      153,    0,  137,   58,   58,  153,  280,   58,  139,    0,
      139,  151,   58,  139,  151,  173,   58,   58,   70,   70,
       70,   70,   70,   70,   70,   70,   70,   70,   70,   70,
       70,   70,   70,   70,   70,   70,   70,   70,   70,   70,
       70,   70,   70,   70,   70,   70,   70,   70,   70,   70,

       70,   70,   70,   70,   70,   70,   70,   70,   70,   70,
       70,   70,   70,   70,   70,   70,   70,   70,   70,   70,
       70,   70,   70,   70,   70,   70,   70,   70,   70,   70,
       70,   70,   70,   70,   70,   70,   70,   70,   70,   70,
       70,   70,   70,   70,   70,   70,   70,   70,   70,   70,
       70,   70,   70,   70,   70,   70,   70,   95,  107,   95,
      107,    0,  841,  107,  107,  107,  107,  107,  107,  107,
      107,  107,  107,  318,  508,  318,  508,  158,  161,  975,
      155,  166,  155,  318,  158,  172,  184,  174,    0,  161,
       95,  160,    0,  176,   95,  621,   95,  621,  155,  155,

        0,   95,  155,   95,   95,  160,  158,  161,  160,  155,
      166,  155,  158,   95,  172,  174,  167,  161,  167,   95,
      160,  176,  841,   95,  162,   95,  155,  155,  162,   95,
      155,   95,   95,  160,  190,  184,  160,  182,  183,  975,
        0,   95,  105,  105,  105,  105,  105,  105,  105,  105,
      105,  105,  162,  171,  167,  187,  162,  175,  191,  193,
      105,  105,  190,  184,  175,    0,    0,  171,  167,  182,
      183,    0,  188,  105,  194,  622,  199,  622,  213,  188,
        0,    0,  171,  167,  187,  175,  191,  193,    0,  105,
      105,    0,  175,  178,  178,  171,  167,    0,  182,  183,

      188,  105,  125,  194,  125,  198,  213,  188,  199,  200,
      178,  178,  178,  178,  178,  178,  178,  178,  178,  178,
      125,  125,  125,  125,  125,  125,  125,  125,  125,  125,
      206,  222,    0,    0,  197,  197,  197,  199,  222,  222,
      189,  189,  179,  205,  200,  207,  189,  625,  179,  625,
      198,  212,  641,  212,  641,  125,  214,  125,  179,  179,
      179,  179,  179,  179,  179,  179,  179,  179,  189,  189,
      185,  197,    0,  200,  189,  205,  185,  207,  198,  206,
      212,  221,  212,  125,  214,  125,  185,  185,  185,  185,
      185,  185,  185,  185,  185,  185,  186,  231,  220,  216,

      197,  217,  186,  230,  205,    0,  207,  206,  210,  211,
      186,  192,  186,  186,  186,  186,  186,  186,  186,  186,
      186,  186,  210,  192,  218,  192,  211,  216,  192,  217,
      186,  186,  230,  221,  236,  231,    0,  210,  211,  225,
      192,  220,  219,  186,    0,    0,  225,  225,    0,    0,
      210,  192,    0,  192,  211,  740,  192,  740,  980,  186,
      186,  221,    0,  231,  218,  237,  239,  236,  245,  220,
        0,  186,  208,  208,  208,  208,  208,  208,  208,  208,
      208,  208,  208,  208,  208,  208,  208,  208,  208,  208,
      208,  219,  218,  237,  239,  236,  245,  246,  223,  223,

      223,  208,  208,  208,  208,  208,  223,  223,  224,  224,
      224,  241,  241,  227,    0,  373,  224,  224,  980,  219,
      227,  227,  373,  373,  241,  246,  247,  248,  228,  249,
      250,  251,  208,  208,    0,  228,  228,  229,    0,  229,
      241,  241,  229,  229,  229,  229,  229,  229,  229,  229,
      229,  229,  241,  252,  247,  248,  227,  249,  250,  251,
      208,  209,  209,  209,  209,  209,  209,  209,  209,  209,
      209,  209,  209,  209,  209,  209,  209,  209,  209,  209,
      228,  252,    0,    0,  227,  354,  354,  354,    0,    0,
      209,  209,  209,  209,  209,  209,  242,  242,  242,  242,

      242,  242,  242,  242,  242,  242,  253,  254,  228,  243,
      243,  243,  243,  243,  243,  243,  243,  243,  243,  256,
      255,  209,  209,  255,  209,  257,    0,  258,  259,  260,
        0,  261,  262,  263,  253,  254,  264,  265,  266,    0,
      268,  269,  272,  269,    0,  270,  270,  256,  255,  209,
      273,  255,  274,  277,  257,  258,  259,  275,  260,  261,
      262,  276,  263,  279,  282,  264,  265,  266,  267,  268,
      267,  272,  269,  270,  270,  278,  287,  278,  291,  273,
      283,  274,  277,  284,  285,  275,  290,  292,  286,  276,
      294,  279,  282,  295,    0,  739,  293,  739,  752,  267,

      752,  286,  296,  739,  267,  287,  267,  291,  283,  267,
      297,  284,  285,  267,  290,  292,  267,  286,  294,  267,
      267,  295,  288,  267,  288,  293,  289,  278,  289,  286,
      296,  299,  288,  267,  300,  267,  289,  301,  267,  297,
      303,  267,    0,  304,  267,  302,  305,  267,  267,  306,
      307,  267,    0,  302,  305,  278,  308,  309,  310,  311,
      299,  312,  300,  313,  319,  301,  288,  314,  315,  303,
      289,  304,  316,  302,  317,  305,  288,  306,  307,  321,
      289,  302,  305,  320,  308,  322,  309,  310,  311,  312,
      323,  324,  313,  319,  288,  314,  315,  327,  289,  329,

      316,  330,  317,  331,  288,  332,  333,  321,  289,  334,
      335,  341,  320,  322,  339,  340,  351,    0,  323,  324,
        0,    0,  336,  345,  336,  327,  343,  329,  344,  330,
      346,  331,  347,  332,  333,  349,  356,  334,    0,  335,
      336,  336,  336,  336,  336,  336,  336,  336,  336,  336,
      337,  345,  339,    0,  351,  343,  337,  344,  340,  346,
      357,  347,  364,  341,  349,  358,  337,  337,  337,  337,
      337,  337,  337,  337,  337,  337,  338,  359,    0,  356,
      365,  339,  338,  351,    0,  367,  340,  352,  352,  352,
      364,  341,  338,  338,  338,  338,  338,  338,  338,  338,

      338,  338,  342,  358,  348,  370,  359,  356,  342,  365,
      368,  371,  357,  367,  362,    0,  342,  348,  342,  342,
      342,  342,  342,  342,  342,  342,  342,  342,  352,  361,
      360,  363,  358,  348,  362,  369,  342,  342,  370,  368,
      357,  360,  363,  362,  372,  348,  374,  361,  375,  342,
      366,  366,  366,  374,  374,  375,  375,  352,  361,  360,
      363,  371,  362,  386,  393,  342,  342,  370, 1002,  360,
      363,  444,    0,  444,  646,  361,  646,  342,  394,  397,
      372,  444,  369,  398,  375,  448,  533,  448,  646,  371,
        0,  386,  393,  533,  533,  448,  374,  376,  376,  376,

      376,  376,  376,  376,  376,  376,  376,  394,  397,  372,
      369,  398,  400,  375,  377,  377,  377,  377,  377,  377,
      377,  377,  377,  377,  374,  390,  390,  401, 1002,  402,
      403,  404,  406,    0,  407,  408,  409,    0,  390,  410,
      411,  400,  412,  413,  416,  413,  417,  418,    0,  419,
      420,  422,  421,  423,  390,  390,  401,  402,  403,  424,
      404,  406,  407,  408,  427,  409,  390,  410,  421,  411,
      412,  425,  416,  425,  417,  429,  418,  419,  420,  422,
      431,  421,  423,  430,  413,  430,  432,  424,  433,    0,
      434,  435,  427,  437, 1050,  438,  421,  439,  413,  440,

        0,  442,  425,  429,  441,  443,    0,  446,  431,  447,
        0,  441,  450,  413,  430,  432,  451,  433,  434,  435,
      452,  453,  437,  438,  456,  439,  413,  414,  440,  414,
      442,  459,  460,  441,  443,  446,  462,  461,  447,  441,
      450,  463,  464,  666,  466,  451,  465,  474,  452,  453,
      666,  666,  469,  456, 1050,  470,  471,  472,  414,  459,
      460,  480,  484,  414,  462,  414,  461,  492,  414,  463,
      487,  464,  414,  466,  465,  414,  474,    0,  414,  414,
      469,    0,  414,  470,  471,  472,  481,    0,  481,  480,
      501,  484,  414,  488,  414,  492,  481,  414,  502,  487,

      414,  491,  493,  414,  494,  495,  414,  414,  496,  503,
      414,  428,  428,  428,  428,  428,  428,  428,  428,  428,
      428,  428,  488,  428,  428,  428,  428,  428,  428,  428,
      491,  493,  515,  494,  495,    0,  498,  496,  498,  501,
      428,  428,  428,  428,  428,  503,  513,  505,  502,  509,
      516,  514,  514,  514,  498,  498,  498,  498,  498,  498,
      498,  498,  498,  498,  499,  507,    0,  501,  518,  523,
      499,  428,  428,  507,  503,  505,  502,  515,  509,    0,
      499,  499,  499,  499,  499,  499,  499,  499,  499,  499,
      500,  506,  514,  506,  507,  513,  500,  526,  523,  428,

      516,  506,  507,    0,    0,  515,  500,  500,  500,  500,
      500,  500,  500,  500,  500,  500,  504,  518,  510,  517,
      542,  514,  504,  513,    0,  510,  526,  506,  516,  510,
      504,  519,  504,  504,  504,  504,  504,  504,  504,  504,
      504,  504,  528,    0,  511,  518,  511,  510,  542,  519,
      504,  504,  530,  510,  511,  517,  506,  510,  511,  545,
      519,  520,    0,  504,  521,  520,  522,  539,  521,  540,
      522,  525,  525,  525,  532,  528,  543,  519,  546,  504,
      504,  532,  532,  547,  517,  549,  550,  545,  530,  551,
      520,  504,  552,  521,  520,  522,  539,  521,  540,  522,

      553,  554,  556,  557,  528,  543,  558,  546,  559,  560,
      532,  547,    0,  549,  550,    0,    0,  530,  551,  667,
      555,  552,  555,  629,  629,  629,  667,  667,  553,    0,
      554,  556,  557,  565,  558,  561,  559,  562,  560,  532,
      541,  541,    0,  541,  541,  541,  541,  541,  541,  541,
      541,  541,  541,  541,  541,  541,  541,  541,  541,  564,
      563,  555,  565,  561,  567,  562,  569,    0,  570,  541,
      541,  541,  541,  541,  571,  555,  563,  572,    0,  573,
      574,  575,  576,  579,  576,  585,  580,  564,  580,  563,
      555,  584,  567,  587,  588,  569,  570,  590,  591,    0,

      541,  541,  571,  555,  563,  592,  572,  573,  593,  574,
      575,  579,  594,  576,  585,  595,  597,  580,  596,  584,
      598,  599,  587,  588,  600,  590,  601,  591,  541,  602,
      603,  605,  608,  592,  607,  609,  593,  611,  612,  613,
      614,  594,  615,  617,  595,  597,  596,  618,  598,  624,
      599,  626,  632,  600,  633,  601,  627,  602,  647,  603,
      605,  608,  607,  648,  609,  611,  612,  613,  636,  614,
      615,  617,  623,  639,  623,  618,  640,  628,  624,  628,
      626,  642,  623,  643,  627,  672,  637,  628,  637,  653,
      633,  628,  632,  644,  655,  644,  637,  636,  647,  649,

      651,  674,  639,  644,  675,  640,  677,  644,  623,  642,
        0,  678,  643,  672,  648,  681,  652,  668,  653,  633,
      652,  632,  637,  655,  668,  668,  829,  647,  829,    0,
      674,  682,  675,  682,  677,  649,  679,  623,  635,  678,
      651,  680,  648,  681,  635,  652,  683,  685,  686,  652,
      690,  637,  635,  688,  635,  635,  635,  635,  635,  635,
      635,  635,  635,  635,  649,  679,  682,  692,  695,  651,
      680,  693,  635,  635,  697,  683,  685,  686,  690,  682,
      694,  696,  688,  698,  709,  635,  709,  836,  731,  836,
      731,  745,  745,  745,  709,  682,  692,  695,  731,  693,

        0,  635,  635,  697,    0,  699,  700,  682,  694,  696,
      701,  703,  698,  635,  654,  654,  654,  654,  654,  654,
      654,  654,  654,  654,  654,  654,  654,  654,  654,  654,
      654,  654,  654,  699,  702,  700,  704,  705,  708,  701,
      703,  710,  711,  654,  654,  654,  654,  654,  706,  712,
      706,  713,    0,  714,  716,  717,  723,  724,  725,    0,
      726,  727,  702,  728,  704,  705,  729,  708,  732,  710,
      737,  711,  741,    0,  654,  654,    0,  742,  712,  706,
      713,  714,  716,  717,  723,  724,  743,  725,  726,  727,
        0,    0,  728,    0,  729, 1052,    0,  732,  737,    0,

        0,  741,  654,  658,  658,  658,  742,    0,  658,  658,
      722,  658,  722, 1053,  743,  658,  658,  746,  736,  658,
      736,  744,  658,  658,  658,  658,  658,  658,  658,  658,
      658,  671,  671,  754,  671,  671,  671,  671,  671,  671,
      671,  671,  671,  671,  671,  671,  671,  671,  671,  671,
      744,  749,  750,  736,  746, 1052,  751,  755,  722,  753,
      671,  671,  671,  671,  671,  758,  760,  762,  762,  762,
      754,  768,  772, 1053,  773,  774,  772,  776,  768,  768,
      749,  750,  736,  746,  751,  777,  722,  778,  753,  769,
        0,  671,  671,  755,  779,  760,  769,  769,  781,  754,

      782,  772,  758,  773,  774,  772,  776,  803,    0,  803,
        0,  821,    0,  821,  777,  778,    0,  803,  780,  671,
      780,  821,  755,  779,  786,  785,  781,  787,  782,  788,
      789,  758,  759,  759,  759,  759,  759,  759,  759,  759,
      759,  759,  759,  759,  759,  759,  759,  759,  759,  759,
      759,  790,  786,  780,  785,  791,  787,  788,  792,  789,
      793,  759,  759,  759,  759,  759,  780,  794,  795,  796,
      797,    0,  798,  800,  801,  812,  802,  812,    0,  810,
      790,  805,  780,  791,  806,  839,  813,  792,  793,  814,
      816,  823,  759,  759,  780,  794,  795,  796,  817,  797,

      798,  822,  800,  801,  802,  804,  804,  804,  810,  805,
      825,  826,  806,  804,  813,  827,  804,  814,  816,  823,
      759,  828,  824,  812,  824,  830,  834,  817,  835,  837,
      822,  842,  843, 1077,  839,  845,    0,  846,  848,  825,
      826,  849,  851,  852,  827,  853,  854,    0,  855,  828,
      856,  812,  857,  858,  830,  834,  835,  824,  837,  859,
      842,  843,  839,  860,  845,  846,  848,  861,  863,  849,
      851,  852,  864,  853,  865,  854,  855,  866,    0,  856,
      857,  858,  867,  868,  870,  879,  824,  859,  872,  872,
      872,    0,  860, 1077,    0,  861,  872,  863,    0,  872,

      886,  864,  887,  865,  880,  893,  866,  873,  873,  873,
      867,  868,  870,  881,  879,  873,  884,  888,  873,  874,
      874,  874,  875,  875,  875,  889,  876,  876,  876,  886,
      875,  887,  880,  875,  876,  890,  892,  876,  891,  894,
      893,  881,  895,  897,  884,  888,  898,  899,  900,  901,
      904,    0,  906,  889,    0,    0,  907,    0,  908,  909,
      910,  912,  913,  914,  890,  892,  891,  915,  894,  893,
      895,  916,  897,  918,  920,  922,  923,  901,    0,  904,
      906,  931,  898,  900,  899,  907,  908,  932,  909,  910,
      912,  913,  914,  936,    0,  915,  924,  924,  924,  916,

      939,  918,  920,  922,  942,  923,  925,  925,  925,  931,
      946,  898,  900,  899,  925,  934,  932,  925,  926,  926,
      926,  936,  927,  927,  927,  937,  926,  938,  939,  926,
      927,  941,  947,  927,  928,  928,  928,  943,  929,  929,
      929,  944,  928,  948,  934,  928,  929,  949,  952,  929,
      942,  951,  953,  954,  937,  946,  938,  956,  958,  960,
      941,    0,  961,  963,  947,  943,  964,  967,  968,  944,
      965,  965,  965,  966,  966,  966,  949,  952,  942,  951,
      953,  966,  954,  946,  966,  956,  958,  960,  973,  948,
      961,  970,  963,  947,  977,  964,  967,  968,  978,  982,

      979,  983,  984,  999,  985,    0, 1003,  986,  988, 1004,
      990,  992,  992,  992,  994,  995,  973,  948,  996, 1018,
      970,  998,  977, 1000, 1005,    0, 1008,  982, 1006, 1007,
      983,  984,  985,  978,  979,  986, 1010,  988,  990,  993,
      993,  993, 1003,  994,  995, 1011,  996,  993,  999,  998,
      993, 1000, 1014, 1005, 1004, 1008, 1006, 1007, 1012, 1012,
     1012, 1015,  978,  979, 1010, 1018, 1019, 1013, 1013, 1013,
     1020, 1003, 1022, 1023, 1011, 1013,  999, 1024, 1013, 1026,
     1027, 1014, 1004, 1021, 1021, 1021, 1031, 1032, 1033, 1015,
     1036, 1035, 1041, 1018, 1019, 1037, 1037, 1037, 1042, 1034,

     1039, 1023, 1030, 1030, 1030, 1020, 1024, 1026, 1048, 1027,
     1030, 1047, 1054, 1030, 1031, 1032, 1033, 1062, 1022, 1035,
     1041, 1044, 1044, 1044,    0, 1058, 1042, 1045, 1045, 1045,
     1068,    0, 1061, 1072, 1020, 1045, 1036, 1048, 1045, 1047,
     1054, 1067, 1063, 1064,    0, 1062, 1022, 1029, 1029, 1029,
     1034, 1039, 1029, 1029, 1058, 1029,    0,    0, 1068, 1029,
     1029, 1071, 1072, 1029, 1036,    0, 1029, 1029, 1029, 1029,
     1029, 1029, 1029, 1029, 1029, 1069, 1070, 1073, 1034, 1039,
     1046, 1046, 1046, 1061, 1079, 1046, 1046, 1063, 1046, 1081,
     1067, 1046, 1046, 1046, 1064, 1074, 1046, 1071, 1075, 1046,

     1046, 1046, 1046, 1046, 1046, 1046, 1046, 1046, 1056, 1056,
     1056, 1061, 1057, 1057, 1057, 1063, 1056, 1076, 1067, 1056,
     1057, 1069, 1064, 1057, 1080, 1070, 1071, 1082, 1073, 1078,
     1075, 1074, 1083, 1083, 1083, 1079, 1084, 1084, 1084, 1085,
     1081, 1086, 1086, 1086, 1091, 1095, 1076, 1094,    0, 1069,
        0,    0, 1080, 1070,    0,    0, 1073,    0,    0, 1075,
     1074, 1078,    0, 1079, 1087, 1087, 1087,    0, 1081, 1089,
     1089, 1089, 1092, 1092, 1092, 1096, 1096, 1096, 1082, 1094,
     1091, 1097, 1097, 1097,    0,    0,    0,    0, 1085,    0,
     1078,    0,    0,    0,    0,    0, 1095,    0,    0,    0,

        0,    0,    0,    0,    0,    0, 1082,    0, 1094, 1091,
        0,    0,    0,    0,    0,    0, 1085,    0,    0,    0,
        0,    0,    0,    0, 1095, 1100, 1100, 1100, 1100, 1100,
     1100, 1100, 1100, 1100, 1100, 1100, 1100, 1101, 1101, 1101,
     1101, 1101, 1101, 1101, 1101, 1101, 1101, 1101, 1101, 1102,
     1102,    0, 1102, 1102, 1102, 1102, 1102, 1102, 1102, 1102,
     1102, 1103, 1103,    0, 1103, 1103, 1103, 1103, 1103, 1103,
     1103, 1103, 1103, 1104, 1104, 1104, 1104, 1104, 1104, 1104,
     1104, 1104, 1104, 1104, 1104, 1105, 1105, 1105, 1105, 1105,
     1105, 1105, 1105, 1105, 1105, 1105, 1105, 1106, 1106, 1106,

     1106, 1106, 1106, 1106, 1107, 1107, 1107, 1107, 1107, 1107,
     1107, 1107, 1107, 1107, 1107, 1107, 1108, 1108, 1108, 1108,
     1108, 1108, 1108, 1108, 1108, 1108, 1108, 1108, 1109, 1109,
     1109, 1109, 1109, 1109,    0, 1109, 1109, 1109,    0, 1109,
     1110, 1110, 1110, 1110, 1110, 1110, 1110, 1110, 1110, 1110,
     1110, 1110, 1111,    0,    0, 1111, 1112, 1112, 1112,    0,
     1112, 1112,    0, 1112, 1112,    0, 1112, 1112, 1113, 1113,
     1113, 1113, 1113, 1113, 1113, 1113, 1113, 1113, 1113, 1113,
     1114, 1114, 1114, 1114, 1114, 1114, 1114, 1114, 1114, 1114,
     1114, 1114, 1115, 1115, 1115, 1115, 1115, 1115, 1115, 1115,

     1115, 1115, 1115, 1115, 1116, 1116, 1116, 1116, 1116, 1116,
     1116, 1116, 1116, 1116, 1116, 1116, 1117,    0,    0, 1117,
     1118,    0,    0,    0, 1118,    0,    0,    0,    0,    0,
     1118, 1119, 1119, 1119, 1119,    0, 1119, 1119, 1119, 1119,
     1119, 1119, 1119, 1120, 1120,    0, 1120, 1120, 1120, 1120,
     1120, 1120, 1120, 1120, 1120, 1121, 1121,    0, 1121, 1121,
     1121, 1121, 1121, 1121, 1121, 1121, 1121, 1122, 1122,    0,
     1122, 1122, 1122, 1122, 1122, 1122, 1122, 1122, 1122, 1123,
     1123, 1123, 1123, 1123, 1123, 1123, 1123, 1123, 1123, 1123,
     1123, 1124, 1124, 1124, 1124, 1124, 1124, 1124, 1124, 1124,

     1124, 1124, 1124, 1125, 1125,    0, 1125, 1125, 1125, 1125,
     1125, 1125, 1125, 1125, 1125, 1126, 1126, 1126, 1126, 1126,
     1126, 1126, 1126, 1126, 1126, 1126, 1126, 1127, 1127, 1127,
     1127, 1127, 1127, 1127, 1127, 1127, 1127, 1127, 1127, 1128,
     1128, 1128, 1128, 1128, 1128, 1128, 1128, 1128, 1128, 1128,
     1128, 1129, 1129,    0, 1129, 1129, 1129, 1129, 1129, 1129,
     1129, 1129, 1129, 1130, 1130,    0, 1130, 1130, 1130, 1130,
     1130, 1130, 1130, 1130, 1130, 1131, 1131,    0, 1131, 1131,
     1131, 1131, 1131, 1131, 1131, 1131, 1131, 1132, 1132, 1132,
     1132, 1132, 1132, 1132, 1132, 1132, 1132, 1132, 1132, 1133,

     1133, 1133, 1133, 1133, 1133, 1133, 1133, 1133, 1133, 1133,
     1133, 1134, 1134,    0, 1134, 1134, 1134, 1134, 1134, 1134,
     1134, 1134, 1134, 1135, 1135, 1135, 1135, 1135, 1135, 1135,
     1135, 1135, 1135, 1135, 1135, 1136, 1136, 1136, 1136, 1136,
     1136, 1136, 1136, 1136, 1136, 1136, 1136, 1137, 1137, 1137,
     1137, 1137, 1137, 1137, 1137, 1137, 1137, 1137, 1137, 1138,
     1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138, 1138,
     1138, 1139, 1139, 1139, 1139, 1139, 1139, 1139, 1139, 1139,
     1139, 1139, 1139, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
     1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,

     1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
     1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
     1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
     1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
     1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
     1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
     1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099, 1099,
     1099, 1099, 1099
    } ;

static yy_state_type yy_last_accepting_state;
static char *yy_last_accepting_cpos;

extern int fortran__flex_debug;
int fortran__flex_debug = 0;

/* The intent behind this definition is that it'll catch
 * any uses of REJECT which flex missed.
 */
#define REJECT reject_used_but_not_detected
#define yymore() yymore_used_but_not_detected
#define YY_MORE_ADJ 0
#define YY_RESTORE_YY_MORE_OFFSET
char *fortran_text;
#line 1 "fortran.lex"
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





#line 44 "fortran.lex"
#include <math.h>
#include <stdlib.h>
#include <string.h>
extern FILE * fortran_in;
#define MAX_INCLUDE_DEPTH 30
#define tabsize 6
YY_BUFFER_STATE include_stack[MAX_INCLUDE_DEPTH];
int line_num_input = 1;
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
#define YY_USER_ACTION                                                  \
    {                                                                   \
        if (firstpass == 0) {                                           \
            strcat(curbuf,fortran_text);   Save_Length(curbuf,38);      \
            strcpy(motparse,fortran_text); Save_Length(motparse,32);    \
            ECHO;                                                       \
        }                                                               \
        strcpy(motparse1,fortran_text);                                 \
    }
    
void out_of_donottreat(void);

#line 1792 "fortran.yy.c"

#define INITIAL 0
#define parameter 1
#define character 2
#define donottreat 3
#define fortran77style 4
#define fortran90style 5

#ifndef YY_NO_UNISTD_H
/* Special case for "unistd.h", since it is non-ANSI. We include it way
 * down here because we want the user's section 1 to have been scanned first.
 * The user has a chance to override it with an option.
 */
#include <unistd.h>
#endif

#ifndef YY_EXTRA_TYPE
#define YY_EXTRA_TYPE void *
#endif

static int yy_init_globals (void );

/* Accessor methods to globals.
   These are made visible to non-reentrant scanners for convenience. */

int fortran_lex_destroy (void );

int fortran_get_debug (void );

void fortran_set_debug (int debug_flag  );

YY_EXTRA_TYPE fortran_get_extra (void );

void fortran_set_extra (YY_EXTRA_TYPE user_defined  );

FILE *fortran_get_in (void );

void fortran_set_in  (FILE * in_str  );

FILE *fortran_get_out (void );

void fortran_set_out  (FILE * out_str  );

int fortran_get_leng (void );

char *fortran_get_text (void );

int fortran_get_lineno (void );

void fortran_set_lineno (int line_number  );

/* Macros after this point can all be overridden by user definitions in
 * section 1.
 */

#ifndef YY_SKIP_YYWRAP
#ifdef __cplusplus
extern "C" int fortran_wrap (void );
#else
extern int fortran_wrap (void );
#endif
#endif

    static void yyunput (int c,char *buf_ptr  );
    
#ifndef yytext_ptr
static void yy_flex_strncpy (char *,yyconst char *,int );
#endif

#ifdef YY_NEED_STRLEN
static int yy_flex_strlen (yyconst char * );
#endif

#ifndef YY_NO_INPUT

#ifdef __cplusplus
static int yyinput (void );
#else
static int input (void );
#endif

#endif

/* Amount of stuff to slurp up with each read. */
#ifndef YY_READ_BUF_SIZE
#define YY_READ_BUF_SIZE 8192
#endif

/* Copy whatever the last rule matched to the standard output. */
#ifndef ECHO
/* This used to be an fputs(), but since the string might contain NUL's,
 * we now use fwrite().
 */
#define ECHO fwrite( fortran_text, fortran_leng, 1, fortran_out )
#endif

/* Gets input and stuffs it into "buf".  number of characters read, or YY_NULL,
 * is returned in "result".
 */
#ifndef YY_INPUT
#define YY_INPUT(buf,result,max_size) \
	if ( YY_CURRENT_BUFFER_LVALUE->yy_is_interactive ) \
		{ \
		int c = '*'; \
		int n; \
		for ( n = 0; n < max_size && \
			     (c = getc( fortran_in )) != EOF && c != '\n'; ++n ) \
			buf[n] = (char) c; \
		if ( c == '\n' ) \
			buf[n++] = (char) c; \
		if ( c == EOF && ferror( fortran_in ) ) \
			YY_FATAL_ERROR( "input in flex scanner failed" ); \
		result = n; \
		} \
	else \
		{ \
		errno=0; \
		while ( (result = fread(buf, 1, max_size, fortran_in))==0 && ferror(fortran_in)) \
			{ \
			if( errno != EINTR) \
				{ \
				YY_FATAL_ERROR( "input in flex scanner failed" ); \
				break; \
				} \
			errno=0; \
			clearerr(fortran_in); \
			} \
		}\
\

#endif

/* No semi-colon after return; correct usage is to write "yyterminate();" -
 * we don't want an extra ';' after the "return" because that will cause
 * some compilers to complain about unreachable statements.
 */
#ifndef yyterminate
#define yyterminate() return YY_NULL
#endif

/* Number of entries by which start-condition stack grows. */
#ifndef YY_START_STACK_INCR
#define YY_START_STACK_INCR 25
#endif

/* Report a fatal error. */
#ifndef YY_FATAL_ERROR
#define YY_FATAL_ERROR(msg) yy_fatal_error( msg )
#endif

/* end tables serialization structures and prototypes */

/* Default declaration of generated scanner - a define so the user can
 * easily add parameters.
 */
#ifndef YY_DECL
#define YY_DECL_IS_OURS 1

extern int fortran_lex (void);

#define YY_DECL int fortran_lex (void)
#endif /* !YY_DECL */

/* Code executed at the beginning of each rule, after fortran_text and fortran_leng
 * have been set up.
 */
#ifndef YY_USER_ACTION
#define YY_USER_ACTION
#endif

/* Code executed at the end of each rule. */
#ifndef YY_BREAK
#define YY_BREAK break;
#endif

#define YY_RULE_SETUP \
	if ( fortran_leng > 0 ) \
		YY_CURRENT_BUFFER_LVALUE->yy_at_bol = \
				(fortran_text[fortran_leng - 1] == '\n'); \
	YY_USER_ACTION

/** The main scanner function which does all the work.
 */
YY_DECL
{
	register yy_state_type yy_current_state;
	register char *yy_cp, *yy_bp;
	register int yy_act;
    
#line 106 "fortran.lex"

  if (infixed) BEGIN(fortran77style) ;
  if (infree)  BEGIN(fortran90style) ;

#line 1987 "fortran.yy.c"

	if ( !(yy_init) )
		{
		(yy_init) = 1;

#ifdef YY_USER_INIT
		YY_USER_INIT;
#endif

		if ( ! (yy_start) )
			(yy_start) = 1;	/* first start state */

		if ( ! fortran_in )
			fortran_in = stdin;

		if ( ! fortran_out )
			fortran_out = stdout;

		if ( ! YY_CURRENT_BUFFER ) {
			fortran_ensure_buffer_stack ();
			YY_CURRENT_BUFFER_LVALUE =
				fortran__create_buffer(fortran_in,YY_BUF_SIZE );
		}

		fortran__load_buffer_state( );
		}

	while ( 1 )		/* loops until end-of-file is reached */
		{
		yy_cp = (yy_c_buf_p);

		/* Support of fortran_text. */
		*yy_cp = (yy_hold_char);

		/* yy_bp points to the position in yy_ch_buf of the start of
		 * the current run.
		 */
		yy_bp = yy_cp;

		yy_current_state = (yy_start);
		yy_current_state += YY_AT_BOL();
yy_match:
		do
			{
			register YY_CHAR yy_c = yy_ec[YY_SC_TO_UI(*yy_cp)];
			if ( yy_accept[yy_current_state] )
				{
				(yy_last_accepting_state) = yy_current_state;
				(yy_last_accepting_cpos) = yy_cp;
				}
			while ( yy_chk[yy_base[yy_current_state] + yy_c] != yy_current_state )
				{
				yy_current_state = (int) yy_def[yy_current_state];
				if ( yy_current_state >= 1100 )
					yy_c = yy_meta[(unsigned int) yy_c];
				}
			yy_current_state = yy_nxt[yy_base[yy_current_state] + (unsigned int) yy_c];
			++yy_cp;
			}
		while ( yy_base[yy_current_state] != 3884 );

yy_find_action:
		yy_act = yy_accept[yy_current_state];
		if ( yy_act == 0 )
			{ /* have to back up */
			yy_cp = (yy_last_accepting_cpos);
			yy_current_state = (yy_last_accepting_state);
			yy_act = yy_accept[yy_current_state];
			}

		YY_DO_BEFORE_ACTION;

do_action:	/* This label is used only to access EOF actions. */

		switch ( yy_act )
	{ /* beginning of action switch */
			case 0: /* must back up */
			/* undo the effects of YY_DO_BEFORE_ACTION */
			*yy_cp = (yy_hold_char);
			yy_cp = (yy_last_accepting_cpos);
			yy_current_state = (yy_last_accepting_state);
			goto yy_find_action;

case 1:
YY_RULE_SETUP
#line 110 "fortran.lex"
{ return TOK_REAL8; }
	YY_BREAK
case 2:
YY_RULE_SETUP
#line 111 "fortran.lex"
{ return TOK_SUBROUTINE; }
	YY_BREAK
case 3:
YY_RULE_SETUP
#line 112 "fortran.lex"
{ return TOK_PROGRAM; }
	YY_BREAK
case 4:
YY_RULE_SETUP
#line 113 "fortran.lex"
{ inallocate = 1; return TOK_ALLOCATE; }
	YY_BREAK
case 5:
YY_RULE_SETUP
#line 114 "fortran.lex"
{ return TOK_NULLIFY; }
	YY_BREAK
case 6:
YY_RULE_SETUP
#line 115 "fortran.lex"
{ inallocate = 1; return TOK_DEALLOCATE; }
	YY_BREAK
case 7:
YY_RULE_SETUP
#line 116 "fortran.lex"
{ return TOK_RESULT; }
	YY_BREAK
case 8:
YY_RULE_SETUP
#line 117 "fortran.lex"
{ return TOK_FUNCTION; }
	YY_BREAK
case 9:
YY_RULE_SETUP
#line 118 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_ENDSUBROUTINE;}
	YY_BREAK
case 10:
YY_RULE_SETUP
#line 119 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_ENDPROGRAM;}
	YY_BREAK
case 11:
YY_RULE_SETUP
#line 120 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_ENDFUNCTION;}
	YY_BREAK
case 12:
YY_RULE_SETUP
#line 121 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_ENDUNIT;}
	YY_BREAK
case 13:
YY_RULE_SETUP
#line 122 "fortran.lex"
{ pos_curinclude = setposcur()-9; return TOK_INCLUDE;}
	YY_BREAK
case 14:
YY_RULE_SETUP
#line 123 "fortran.lex"
{ strcpy(yylval.na,fortran_text);
                              tmpc = input(); unput(tmpc);
                              if ( ( tmpc >= 'a' && tmpc <= 'z' ) ||
                                   ( tmpc >= 'A' && tmpc <= 'Z' )  )  return TOK_USE;
                              else                                    return TOK_NAME;
                            }
	YY_BREAK
case 15:
YY_RULE_SETUP
#line 129 "fortran.lex"
{ return TOK_REWIND; }
	YY_BREAK
case 16:
YY_RULE_SETUP
#line 130 "fortran.lex"
{ return TOK_IMPLICIT; }
	YY_BREAK
case 17:
YY_RULE_SETUP
#line 131 "fortran.lex"
{ return TOK_NONE; }
	YY_BREAK
case 18:
YY_RULE_SETUP
#line 132 "fortran.lex"
{ return TOK_CALL; }
	YY_BREAK
case 19:
YY_RULE_SETUP
#line 133 "fortran.lex"
{ return TOK_TRUE; }
	YY_BREAK
case 20:
YY_RULE_SETUP
#line 134 "fortran.lex"
{ return TOK_FALSE; }
	YY_BREAK
case 21:
YY_RULE_SETUP
#line 135 "fortran.lex"
{ return TOK_POINT_TO; }
	YY_BREAK
case 22:
YY_RULE_SETUP
#line 136 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_DASTER; }
	YY_BREAK
case 23:
YY_RULE_SETUP
#line 137 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_EQV; }
	YY_BREAK
case 24:
YY_RULE_SETUP
#line 138 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_EQ;  }
	YY_BREAK
case 25:
YY_RULE_SETUP
#line 139 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_GT;  }
	YY_BREAK
case 26:
YY_RULE_SETUP
#line 140 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_GE;  }
	YY_BREAK
case 27:
YY_RULE_SETUP
#line 141 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_LT;  }
	YY_BREAK
case 28:
YY_RULE_SETUP
#line 142 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_LE;  }
	YY_BREAK
case 29:
YY_RULE_SETUP
#line 143 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_NEQV;}
	YY_BREAK
case 30:
YY_RULE_SETUP
#line 144 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_NE;  }
	YY_BREAK
case 31:
YY_RULE_SETUP
#line 145 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_NOT; }
	YY_BREAK
case 32:
YY_RULE_SETUP
#line 146 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_OR;  }
	YY_BREAK
case 33:
YY_RULE_SETUP
#line 147 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_XOR; }
	YY_BREAK
case 34:
YY_RULE_SETUP
#line 148 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_AND; }
	YY_BREAK
case 35:
YY_RULE_SETUP
#line 149 "fortran.lex"
{ return TOK_MODULE; }
	YY_BREAK
case 36:
YY_RULE_SETUP
#line 150 "fortran.lex"
{ return TOK_DOWHILE; }
	YY_BREAK
case 37:
YY_RULE_SETUP
#line 151 "fortran.lex"
{ return TOK_ENDMODULE; }
	YY_BREAK
case 38:
YY_RULE_SETUP
#line 152 "fortran.lex"
{ return TOK_ENDDO; }
	YY_BREAK
case 39:
YY_RULE_SETUP
#line 153 "fortran.lex"
{ return TOK_PLAINDO;}
	YY_BREAK
case 40:
YY_RULE_SETUP
#line 154 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_REAL; }
	YY_BREAK
case 41:
YY_RULE_SETUP
#line 155 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_INTEGER; }
	YY_BREAK
case 42:
YY_RULE_SETUP
#line 156 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_LOGICAL; }
	YY_BREAK
case 43:
YY_RULE_SETUP
#line 157 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_CHARACTER; }
	YY_BREAK
case 44:
YY_RULE_SETUP
#line 158 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_DOUBLEPRECISION; }
	YY_BREAK
case 45:
YY_RULE_SETUP
#line 159 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_DOUBLECOMPLEX; }
	YY_BREAK
case 46:
YY_RULE_SETUP
#line 160 "fortran.lex"
{ return TOK_COMPLEX; }
	YY_BREAK
case 47:
YY_RULE_SETUP
#line 161 "fortran.lex"
{ return TOK_ALLOCATABLE; }
	YY_BREAK
case 48:
YY_RULE_SETUP
#line 162 "fortran.lex"
{ return TOK_CLOSE; }
	YY_BREAK
case 49:
YY_RULE_SETUP
#line 163 "fortran.lex"
{ return TOK_INQUIRE; }
	YY_BREAK
case 50:
YY_RULE_SETUP
#line 164 "fortran.lex"
{ return TOK_DIMENSION; }
	YY_BREAK
case 51:
YY_RULE_SETUP
#line 165 "fortran.lex"
{ return TOK_PAUSE; }
	YY_BREAK
case 52:
YY_RULE_SETUP
#line 166 "fortran.lex"
{ return TOK_EQUIVALENCE; }
	YY_BREAK
case 53:
YY_RULE_SETUP
#line 167 "fortran.lex"
{ return TOK_STOP; }
	YY_BREAK
case 54:
YY_RULE_SETUP
#line 168 "fortran.lex"
{ return TOK_WHERE; }
	YY_BREAK
case 55:
YY_RULE_SETUP
#line 169 "fortran.lex"
{ return TOK_ENDWHERE; }
	YY_BREAK
case 56:
YY_RULE_SETUP
#line 170 "fortran.lex"
{ return TOK_ELSEWHERE; }
	YY_BREAK
case 57:
YY_RULE_SETUP
#line 171 "fortran.lex"
{ return TOK_CONTAINS; }
	YY_BREAK
case 58:
YY_RULE_SETUP
#line 172 "fortran.lex"
{ return TOK_ONLY; }
	YY_BREAK
case 59:
YY_RULE_SETUP
#line 173 "fortran.lex"
{ return TOK_PARAMETER; }
	YY_BREAK
case 60:
YY_RULE_SETUP
#line 174 "fortran.lex"
{ return TOK_RECURSIVE; }
	YY_BREAK
case 61:
YY_RULE_SETUP
#line 175 "fortran.lex"
{ return TOK_COMMON; }
	YY_BREAK
case 62:
YY_RULE_SETUP
#line 176 "fortran.lex"
{ return TOK_GLOBAL; }
	YY_BREAK
case 63:
YY_RULE_SETUP
#line 177 "fortran.lex"
{ return TOK_EXTERNAL; }
	YY_BREAK
case 64:
YY_RULE_SETUP
#line 178 "fortran.lex"
{ return TOK_INTENT; }
	YY_BREAK
case 65:
YY_RULE_SETUP
#line 179 "fortran.lex"
{ return TOK_POINTER; }
	YY_BREAK
case 66:
YY_RULE_SETUP
#line 180 "fortran.lex"
{ return TOK_OPTIONAL; }
	YY_BREAK
case 67:
YY_RULE_SETUP
#line 181 "fortran.lex"
{ return TOK_SAVE; }
	YY_BREAK
case 68:
YY_RULE_SETUP
#line 182 "fortran.lex"
{ return TOK_TYPE; }
	YY_BREAK
case 69:
YY_RULE_SETUP
#line 183 "fortran.lex"
{ pos_cur_decl = setposcur()-5; return TOK_TYPEPAR; }
	YY_BREAK
case 70:
YY_RULE_SETUP
#line 184 "fortran.lex"
{ if (inallocate == 1) return TOK_STAT; else { strcpy(yylval.na,fortran_text); return TOK_NAME; } }
	YY_BREAK
case 71:
YY_RULE_SETUP
#line 185 "fortran.lex"
{ return TOK_ENDTYPE; }
	YY_BREAK
case 72:
YY_RULE_SETUP
#line 186 "fortran.lex"
{ return TOK_OPEN; }
	YY_BREAK
case 73:
YY_RULE_SETUP
#line 187 "fortran.lex"
{ return TOK_RETURN; }
	YY_BREAK
case 74:
/* rule 74 can match eol */
YY_RULE_SETUP
#line 188 "fortran.lex"
{ return TOK_EXIT; }
	YY_BREAK
case 75:
YY_RULE_SETUP
#line 189 "fortran.lex"
{ return TOK_PRINT; }
	YY_BREAK
case 76:
YY_RULE_SETUP
#line 190 "fortran.lex"
{ return TOK_PROCEDURE; }
	YY_BREAK
case 77:
YY_RULE_SETUP
#line 191 "fortran.lex"
{ return TOK_READ; }
	YY_BREAK
case 78:
YY_RULE_SETUP
#line 192 "fortran.lex"
{ return TOK_NAMELIST; }
	YY_BREAK
case 79:
YY_RULE_SETUP
#line 193 "fortran.lex"
{ return TOK_WRITE; }
	YY_BREAK
case 80:
YY_RULE_SETUP
#line 194 "fortran.lex"
{ return TOK_FLUSH; }
	YY_BREAK
case 81:
YY_RULE_SETUP
#line 195 "fortran.lex"
{ return TOK_TARGET; }
	YY_BREAK
case 82:
YY_RULE_SETUP
#line 196 "fortran.lex"
{ return TOK_PUBLIC; }
	YY_BREAK
case 83:
YY_RULE_SETUP
#line 197 "fortran.lex"
{ return TOK_PRIVATE; }
	YY_BREAK
case 84:
YY_RULE_SETUP
#line 198 "fortran.lex"
{ strcpy(yylval.nac,fortran_text); return TOK_IN; }
	YY_BREAK
case 85:
YY_RULE_SETUP
#line 199 "fortran.lex"
{ strcpy(yylval.na, fortran_text); return TOK_DATA; }
	YY_BREAK
case 86:
YY_RULE_SETUP
#line 200 "fortran.lex"
{ return TOK_CONTINUE; }
	YY_BREAK
case 87:
YY_RULE_SETUP
#line 201 "fortran.lex"
{ return TOK_PLAINGOTO; }
	YY_BREAK
case 88:
YY_RULE_SETUP
#line 202 "fortran.lex"
{ strcpy(yylval.nac,fortran_text); return TOK_OUT; }
	YY_BREAK
case 89:
YY_RULE_SETUP
#line 203 "fortran.lex"
{ strcpy(yylval.nac,fortran_text); return TOK_INOUT; }
	YY_BREAK
case 90:
YY_RULE_SETUP
#line 204 "fortran.lex"
{ return TOK_INTRINSIC; }
	YY_BREAK
case 91:
YY_RULE_SETUP
#line 205 "fortran.lex"
{ return TOK_THEN; }
	YY_BREAK
case 92:
YY_RULE_SETUP
#line 206 "fortran.lex"
{ return TOK_ELSEIF; }
	YY_BREAK
case 93:
YY_RULE_SETUP
#line 207 "fortran.lex"
{ return TOK_ELSE; }
	YY_BREAK
case 94:
YY_RULE_SETUP
#line 208 "fortran.lex"
{ return TOK_ENDIF; }
	YY_BREAK
case 95:
YY_RULE_SETUP
#line 209 "fortran.lex"
{ return TOK_LOGICALIF; }
	YY_BREAK
case 96:
YY_RULE_SETUP
#line 210 "fortran.lex"
{ return TOK_SUM; }
	YY_BREAK
case 97:
YY_RULE_SETUP
#line 211 "fortran.lex"
{ return TOK_MAX; }
	YY_BREAK
case 98:
YY_RULE_SETUP
#line 212 "fortran.lex"
{ return TOK_TANH; }
	YY_BREAK
case 99:
YY_RULE_SETUP
#line 213 "fortran.lex"
{ return TOK_MAXVAL; }
	YY_BREAK
case 100:
YY_RULE_SETUP
#line 214 "fortran.lex"
{ return TOK_TRIM; }
	YY_BREAK
case 101:
YY_RULE_SETUP
#line 215 "fortran.lex"
{ return TOK_SQRT; }
	YY_BREAK
case 102:
YY_RULE_SETUP
#line 216 "fortran.lex"
{ return TOK_SELECTCASE; }
	YY_BREAK
case 103:
YY_RULE_SETUP
#line 217 "fortran.lex"
{ return TOK_CASE; }
	YY_BREAK
case 104:
YY_RULE_SETUP
#line 218 "fortran.lex"
{ return TOK_CASEDEFAULT; }
	YY_BREAK
case 105:
YY_RULE_SETUP
#line 219 "fortran.lex"
{ return TOK_ENDSELECT; }
	YY_BREAK
case 106:
YY_RULE_SETUP
#line 220 "fortran.lex"
{ return TOK_FILE; }
	YY_BREAK
case 107:
YY_RULE_SETUP
#line 221 "fortran.lex"
{ return TOK_END; }
	YY_BREAK
case 108:
YY_RULE_SETUP
#line 222 "fortran.lex"
{ return TOK_ERR; }
	YY_BREAK
case 109:
YY_RULE_SETUP
#line 223 "fortran.lex"
{ return TOK_EXIST; }
	YY_BREAK
case 110:
YY_RULE_SETUP
#line 224 "fortran.lex"
{ return TOK_MIN; }
	YY_BREAK
case 111:
YY_RULE_SETUP
#line 225 "fortran.lex"
{ return TOK_NINT; }
	YY_BREAK
case 112:
YY_RULE_SETUP
#line 226 "fortran.lex"
{ return TOK_FLOAT; }
	YY_BREAK
case 113:
YY_RULE_SETUP
#line 227 "fortran.lex"
{ return TOK_EXP; }
	YY_BREAK
case 114:
YY_RULE_SETUP
#line 228 "fortran.lex"
{ return TOK_COS; }
	YY_BREAK
case 115:
YY_RULE_SETUP
#line 229 "fortran.lex"
{ return TOK_COSH; }
	YY_BREAK
case 116:
YY_RULE_SETUP
#line 230 "fortran.lex"
{ return TOK_ACOS; }
	YY_BREAK
case 117:
YY_RULE_SETUP
#line 231 "fortran.lex"
{ return TOK_SIN; }
	YY_BREAK
case 118:
YY_RULE_SETUP
#line 232 "fortran.lex"
{ return TOK_SINH; }
	YY_BREAK
case 119:
YY_RULE_SETUP
#line 233 "fortran.lex"
{ return TOK_ASIN; }
	YY_BREAK
case 120:
YY_RULE_SETUP
#line 234 "fortran.lex"
{ return TOK_LOG; }
	YY_BREAK
case 121:
YY_RULE_SETUP
#line 235 "fortran.lex"
{ return TOK_TAN; }
	YY_BREAK
case 122:
YY_RULE_SETUP
#line 236 "fortran.lex"
{ return TOK_ATAN; }
	YY_BREAK
case 123:
YY_RULE_SETUP
#line 237 "fortran.lex"
{ return TOK_CYCLE; }
	YY_BREAK
case 124:
YY_RULE_SETUP
#line 238 "fortran.lex"
{ return TOK_ABS; }
	YY_BREAK
case 125:
YY_RULE_SETUP
#line 239 "fortran.lex"
{ return TOK_MOD; }
	YY_BREAK
case 126:
YY_RULE_SETUP
#line 240 "fortran.lex"
{ return TOK_SIGN; }
	YY_BREAK
case 127:
YY_RULE_SETUP
#line 241 "fortran.lex"
{ return TOK_MINLOC; }
	YY_BREAK
case 128:
YY_RULE_SETUP
#line 242 "fortran.lex"
{ return TOK_MAXLOC; }
	YY_BREAK
case 129:
YY_RULE_SETUP
#line 243 "fortran.lex"
{ return TOK_MINVAL; }
	YY_BREAK
case 130:
YY_RULE_SETUP
#line 244 "fortran.lex"
{ return TOK_BACKSPACE; }
	YY_BREAK
case 131:
YY_RULE_SETUP
#line 245 "fortran.lex"
{ return TOK_LEFTAB; }
	YY_BREAK
case 132:
YY_RULE_SETUP
#line 246 "fortran.lex"
{ return TOK_RIGHTAB; }
	YY_BREAK
case 133:
/* rule 133 can match eol */
YY_RULE_SETUP
#line 247 "fortran.lex"
{
                              return TOK_FORMAT; }
	YY_BREAK
case 134:
YY_RULE_SETUP
#line 249 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_SLASH; }
	YY_BREAK
case 135:
YY_RULE_SETUP
#line 250 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_DSLASH; }
	YY_BREAK
case 136:
/* rule 136 can match eol */
YY_RULE_SETUP
#line 251 "fortran.lex"
{
                              strcpy(yylval.na,fortran_text); return TOK_CHAR_CUT; }
	YY_BREAK
case 137:
YY_RULE_SETUP
#line 253 "fortran.lex"
{ strcpy(yylval.na,fortran_text);return TOK_CHAR_CONSTANT; }
	YY_BREAK
case 138:
YY_RULE_SETUP
#line 254 "fortran.lex"
{ strcpy(yylval.na,fortran_text);return TOK_CHAR_MESSAGE; }
	YY_BREAK
case 139:
YY_RULE_SETUP
#line 255 "fortran.lex"
{ BEGIN(donottreat); }
	YY_BREAK
case 140:
/* rule 140 can match eol */
YY_RULE_SETUP
#line 256 "fortran.lex"
{ out_of_donottreat(); return '\n'; }
	YY_BREAK
case 141:
YY_RULE_SETUP
#line 257 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_NAME; }
	YY_BREAK
case 142:
/* rule 142 can match eol */
YY_RULE_SETUP
#line 258 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_CSTREAL; }
	YY_BREAK
case 143:
YY_RULE_SETUP
#line 259 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_CSTREAL; }
	YY_BREAK
case 144:
YY_RULE_SETUP
#line 260 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return TOK_CSTINT; }
	YY_BREAK
case 145:
YY_RULE_SETUP
#line 261 "fortran.lex"
{}
	YY_BREAK
case 146:
YY_RULE_SETUP
#line 262 "fortran.lex"
{}
	YY_BREAK
case 147:
YY_RULE_SETUP
#line 263 "fortran.lex"
{ strcpy(yylval.na,fortran_text); return (int) *fortran_text; }
	YY_BREAK
case 148:
YY_RULE_SETUP
#line 264 "fortran.lex"
{ afterpercent = 1; strcpy(yylval.na,fortran_text); return (int) *fortran_text; }
	YY_BREAK
case 149:
YY_RULE_SETUP
#line 265 "fortran.lex"
{ return TOK_SEMICOLON; }
	YY_BREAK
case 150:
YY_RULE_SETUP
#line 266 "fortran.lex"
{ return (int) *fortran_text; }
	YY_BREAK
case 151:
YY_RULE_SETUP
#line 267 "fortran.lex"
{ return (int) *fortran_text; }
	YY_BREAK
case 152:
YY_RULE_SETUP
#line 268 "fortran.lex"
{ return (int) *fortran_text; }
	YY_BREAK
case 153:
YY_RULE_SETUP
#line 269 "fortran.lex"
{ return (int) *fortran_text; }
	YY_BREAK
case 154:
/* rule 154 can match eol */
YY_RULE_SETUP
#line 270 "fortran.lex"
{ line_num_input++; return (int) *fortran_text; }
	YY_BREAK
case 155:
*yy_cp = (yy_hold_char); /* undo effects of setting up fortran_text */
(yy_c_buf_p) = yy_cp -= 1;
YY_DO_BEFORE_ACTION; /* set up fortran_text again */
YY_RULE_SETUP
#line 271 "fortran.lex"
{}
	YY_BREAK
case 156:
YY_RULE_SETUP
#line 272 "fortran.lex"
{}
	YY_BREAK
case 157:
YY_RULE_SETUP
#line 273 "fortran.lex"
{ if (newlinef90 == 0) return TOK_LABEL; else newlinef90 = 0; }
	YY_BREAK
case 158:
/* rule 158 can match eol */
YY_RULE_SETUP
#line 274 "fortran.lex"
{ line_num_input++; newlinef90=1; }
	YY_BREAK
case 159:
/* rule 159 can match eol */
YY_RULE_SETUP
#line 275 "fortran.lex"
{ line_num_input++; }
	YY_BREAK
case 160:
/* rule 160 can match eol */
YY_RULE_SETUP
#line 277 "fortran.lex"
{ line_num_input++; BEGIN(donottreat); }
	YY_BREAK
case 161:
/* rule 161 can match eol */
YY_RULE_SETUP
#line 278 "fortran.lex"
{ out_of_donottreat(); return '\n'; }
	YY_BREAK
case 162:
/* rule 162 can match eol */
YY_RULE_SETUP
#line 279 "fortran.lex"
{ line_num_input++; }
	YY_BREAK
case 163:
/* rule 163 can match eol */
YY_RULE_SETUP
#line 280 "fortran.lex"
{ line_num_input++; }
	YY_BREAK
case 164:
/* rule 164 can match eol */
YY_RULE_SETUP
#line 281 "fortran.lex"
{ line_num_input++; }
	YY_BREAK
case 165:
YY_RULE_SETUP
#line 282 "fortran.lex"
{ }
	YY_BREAK
case 166:
YY_RULE_SETUP
#line 283 "fortran.lex"
ECHO;
	YY_BREAK
#line 2924 "fortran.yy.c"
case YY_STATE_EOF(INITIAL):
case YY_STATE_EOF(parameter):
case YY_STATE_EOF(character):
case YY_STATE_EOF(donottreat):
case YY_STATE_EOF(fortran77style):
case YY_STATE_EOF(fortran90style):
	yyterminate();

	case YY_END_OF_BUFFER:
		{
		/* Amount of text matched not including the EOB char. */
		int yy_amount_of_matched_text = (int) (yy_cp - (yytext_ptr)) - 1;

		/* Undo the effects of YY_DO_BEFORE_ACTION. */
		*yy_cp = (yy_hold_char);
		YY_RESTORE_YY_MORE_OFFSET

		if ( YY_CURRENT_BUFFER_LVALUE->yy_buffer_status == YY_BUFFER_NEW )
			{
			/* We're scanning a new file or input source.  It's
			 * possible that this happened because the user
			 * just pointed fortran_in at a new source and called
			 * fortran_lex().  If so, then we have to assure
			 * consistency between YY_CURRENT_BUFFER and our
			 * globals.  Here is the right place to do so, because
			 * this is the first action (other than possibly a
			 * back-up) that will match for the new input source.
			 */
			(yy_n_chars) = YY_CURRENT_BUFFER_LVALUE->yy_n_chars;
			YY_CURRENT_BUFFER_LVALUE->yy_input_file = fortran_in;
			YY_CURRENT_BUFFER_LVALUE->yy_buffer_status = YY_BUFFER_NORMAL;
			}

		/* Note that here we test for yy_c_buf_p "<=" to the position
		 * of the first EOB in the buffer, since yy_c_buf_p will
		 * already have been incremented past the NUL character
		 * (since all states make transitions on EOB to the
		 * end-of-buffer state).  Contrast this with the test
		 * in input().
		 */
		if ( (yy_c_buf_p) <= &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars)] )
			{ /* This was really a NUL. */
			yy_state_type yy_next_state;

			(yy_c_buf_p) = (yytext_ptr) + yy_amount_of_matched_text;

			yy_current_state = yy_get_previous_state(  );

			/* Okay, we're now positioned to make the NUL
			 * transition.  We couldn't have
			 * yy_get_previous_state() go ahead and do it
			 * for us because it doesn't know how to deal
			 * with the possibility of jamming (and we don't
			 * want to build jamming into it because then it
			 * will run more slowly).
			 */

			yy_next_state = yy_try_NUL_trans( yy_current_state );

			yy_bp = (yytext_ptr) + YY_MORE_ADJ;

			if ( yy_next_state )
				{
				/* Consume the NUL. */
				yy_cp = ++(yy_c_buf_p);
				yy_current_state = yy_next_state;
				goto yy_match;
				}

			else
				{
				yy_cp = (yy_c_buf_p);
				goto yy_find_action;
				}
			}

		else switch ( yy_get_next_buffer(  ) )
			{
			case EOB_ACT_END_OF_FILE:
				{
				(yy_did_buffer_switch_on_eof) = 0;

				if ( fortran_wrap( ) )
					{
					/* Note: because we've taken care in
					 * yy_get_next_buffer() to have set up
					 * fortran_text, we can now set up
					 * yy_c_buf_p so that if some total
					 * hoser (like flex itself) wants to
					 * call the scanner after we return the
					 * YY_NULL, it'll still work - another
					 * YY_NULL will get returned.
					 */
					(yy_c_buf_p) = (yytext_ptr) + YY_MORE_ADJ;

					yy_act = YY_STATE_EOF(YY_START);
					goto do_action;
					}

				else
					{
					if ( ! (yy_did_buffer_switch_on_eof) )
						YY_NEW_FILE;
					}
				break;
				}

			case EOB_ACT_CONTINUE_SCAN:
				(yy_c_buf_p) =
					(yytext_ptr) + yy_amount_of_matched_text;

				yy_current_state = yy_get_previous_state(  );

				yy_cp = (yy_c_buf_p);
				yy_bp = (yytext_ptr) + YY_MORE_ADJ;
				goto yy_match;

			case EOB_ACT_LAST_MATCH:
				(yy_c_buf_p) =
				&YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars)];

				yy_current_state = yy_get_previous_state(  );

				yy_cp = (yy_c_buf_p);
				yy_bp = (yytext_ptr) + YY_MORE_ADJ;
				goto yy_find_action;
			}
		break;
		}

	default:
		YY_FATAL_ERROR(
			"fatal flex scanner internal error--no action found" );
	} /* end of action switch */
		} /* end of scanning one token */
} /* end of fortran_lex */

/* yy_get_next_buffer - try to read in a new buffer
 *
 * Returns a code representing an action:
 *	EOB_ACT_LAST_MATCH -
 *	EOB_ACT_CONTINUE_SCAN - continue scanning from current position
 *	EOB_ACT_END_OF_FILE - end of file
 */
static int yy_get_next_buffer (void)
{
    	register char *dest = YY_CURRENT_BUFFER_LVALUE->yy_ch_buf;
	register char *source = (yytext_ptr);
	register int number_to_move, i;
	int ret_val;

	if ( (yy_c_buf_p) > &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars) + 1] )
		YY_FATAL_ERROR(
		"fatal flex scanner internal error--end of buffer missed" );

	if ( YY_CURRENT_BUFFER_LVALUE->yy_fill_buffer == 0 )
		{ /* Don't try to fill the buffer, so this is an EOF. */
		if ( (yy_c_buf_p) - (yytext_ptr) - YY_MORE_ADJ == 1 )
			{
			/* We matched a single character, the EOB, so
			 * treat this as a final EOF.
			 */
			return EOB_ACT_END_OF_FILE;
			}

		else
			{
			/* We matched some text prior to the EOB, first
			 * process it.
			 */
			return EOB_ACT_LAST_MATCH;
			}
		}

	/* Try to read more data. */

	/* First move last chars to start of buffer. */
	number_to_move = (int) ((yy_c_buf_p) - (yytext_ptr)) - 1;

	for ( i = 0; i < number_to_move; ++i )
		*(dest++) = *(source++);

	if ( YY_CURRENT_BUFFER_LVALUE->yy_buffer_status == YY_BUFFER_EOF_PENDING )
		/* don't do the read, it's not guaranteed to return an EOF,
		 * just force an EOF
		 */
		YY_CURRENT_BUFFER_LVALUE->yy_n_chars = (yy_n_chars) = 0;

	else
		{
			int num_to_read =
			YY_CURRENT_BUFFER_LVALUE->yy_buf_size - number_to_move - 1;

		while ( num_to_read <= 0 )
			{ /* Not enough room in the buffer - grow it. */

			/* just a shorter name for the current buffer */
			YY_BUFFER_STATE b = YY_CURRENT_BUFFER;

			int yy_c_buf_p_offset =
				(int) ((yy_c_buf_p) - b->yy_ch_buf);

			if ( b->yy_is_our_buffer )
				{
				int new_size = b->yy_buf_size * 2;

				if ( new_size <= 0 )
					b->yy_buf_size += b->yy_buf_size / 8;
				else
					b->yy_buf_size *= 2;

				b->yy_ch_buf = (char *)
					/* Include room in for 2 EOB chars. */
					fortran_realloc((void *) b->yy_ch_buf,b->yy_buf_size + 2  );
				}
			else
				/* Can't grow it, we don't own it. */
				b->yy_ch_buf = 0;

			if ( ! b->yy_ch_buf )
				YY_FATAL_ERROR(
				"fatal error - scanner input buffer overflow" );

			(yy_c_buf_p) = &b->yy_ch_buf[yy_c_buf_p_offset];

			num_to_read = YY_CURRENT_BUFFER_LVALUE->yy_buf_size -
						number_to_move - 1;

			}

		if ( num_to_read > YY_READ_BUF_SIZE )
			num_to_read = YY_READ_BUF_SIZE;

		/* Read in more data. */
		YY_INPUT( (&YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[number_to_move]),
			(yy_n_chars), (size_t) num_to_read );

		YY_CURRENT_BUFFER_LVALUE->yy_n_chars = (yy_n_chars);
		}

	if ( (yy_n_chars) == 0 )
		{
		if ( number_to_move == YY_MORE_ADJ )
			{
			ret_val = EOB_ACT_END_OF_FILE;
			fortran_restart(fortran_in  );
			}

		else
			{
			ret_val = EOB_ACT_LAST_MATCH;
			YY_CURRENT_BUFFER_LVALUE->yy_buffer_status =
				YY_BUFFER_EOF_PENDING;
			}
		}

	else
		ret_val = EOB_ACT_CONTINUE_SCAN;

	if ((yy_size_t) ((yy_n_chars) + number_to_move) > YY_CURRENT_BUFFER_LVALUE->yy_buf_size) {
		/* Extend the array by 50%, plus the number we really need. */
		yy_size_t new_size = (yy_n_chars) + number_to_move + ((yy_n_chars) >> 1);
		YY_CURRENT_BUFFER_LVALUE->yy_ch_buf = (char *) fortran_realloc((void *) YY_CURRENT_BUFFER_LVALUE->yy_ch_buf,new_size  );
		if ( ! YY_CURRENT_BUFFER_LVALUE->yy_ch_buf )
			YY_FATAL_ERROR( "out of dynamic memory in yy_get_next_buffer()" );
	}

	(yy_n_chars) += number_to_move;
	YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars)] = YY_END_OF_BUFFER_CHAR;
	YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars) + 1] = YY_END_OF_BUFFER_CHAR;

	(yytext_ptr) = &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[0];

	return ret_val;
}

/* yy_get_previous_state - get the state just before the EOB char was reached */

    static yy_state_type yy_get_previous_state (void)
{
	register yy_state_type yy_current_state;
	register char *yy_cp;
    
	yy_current_state = (yy_start);
	yy_current_state += YY_AT_BOL();

	for ( yy_cp = (yytext_ptr) + YY_MORE_ADJ; yy_cp < (yy_c_buf_p); ++yy_cp )
		{
		register YY_CHAR yy_c = (*yy_cp ? yy_ec[YY_SC_TO_UI(*yy_cp)] : 1);
		if ( yy_accept[yy_current_state] )
			{
			(yy_last_accepting_state) = yy_current_state;
			(yy_last_accepting_cpos) = yy_cp;
			}
		while ( yy_chk[yy_base[yy_current_state] + yy_c] != yy_current_state )
			{
			yy_current_state = (int) yy_def[yy_current_state];
			if ( yy_current_state >= 1100 )
				yy_c = yy_meta[(unsigned int) yy_c];
			}
		yy_current_state = yy_nxt[yy_base[yy_current_state] + (unsigned int) yy_c];
		}

	return yy_current_state;
}

/* yy_try_NUL_trans - try to make a transition on the NUL character
 *
 * synopsis
 *	next_state = yy_try_NUL_trans( current_state );
 */
    static yy_state_type yy_try_NUL_trans  (yy_state_type yy_current_state )
{
	register int yy_is_jam;
    	register char *yy_cp = (yy_c_buf_p);

	register YY_CHAR yy_c = 1;
	if ( yy_accept[yy_current_state] )
		{
		(yy_last_accepting_state) = yy_current_state;
		(yy_last_accepting_cpos) = yy_cp;
		}
	while ( yy_chk[yy_base[yy_current_state] + yy_c] != yy_current_state )
		{
		yy_current_state = (int) yy_def[yy_current_state];
		if ( yy_current_state >= 1100 )
			yy_c = yy_meta[(unsigned int) yy_c];
		}
	yy_current_state = yy_nxt[yy_base[yy_current_state] + (unsigned int) yy_c];
	yy_is_jam = (yy_current_state == 1099);

	return yy_is_jam ? 0 : yy_current_state;
}

    static void yyunput (int c, register char * yy_bp )
{
	register char *yy_cp;
    
    yy_cp = (yy_c_buf_p);

	/* undo effects of setting up fortran_text */
	*yy_cp = (yy_hold_char);

	if ( yy_cp < YY_CURRENT_BUFFER_LVALUE->yy_ch_buf + 2 )
		{ /* need to shift things up to make room */
		/* +2 for EOB chars. */
		register int number_to_move = (yy_n_chars) + 2;
		register char *dest = &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[
					YY_CURRENT_BUFFER_LVALUE->yy_buf_size + 2];
		register char *source =
				&YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[number_to_move];

		while ( source > YY_CURRENT_BUFFER_LVALUE->yy_ch_buf )
			*--dest = *--source;

		yy_cp += (int) (dest - source);
		yy_bp += (int) (dest - source);
		YY_CURRENT_BUFFER_LVALUE->yy_n_chars =
			(yy_n_chars) = YY_CURRENT_BUFFER_LVALUE->yy_buf_size;

		if ( yy_cp < YY_CURRENT_BUFFER_LVALUE->yy_ch_buf + 2 )
			YY_FATAL_ERROR( "flex scanner push-back overflow" );
		}

	*--yy_cp = (char) c;

	(yytext_ptr) = yy_bp;
	(yy_hold_char) = *yy_cp;
	(yy_c_buf_p) = yy_cp;
}

#ifndef YY_NO_INPUT
#ifdef __cplusplus
    static int yyinput (void)
#else
    static int input  (void)
#endif

{
	int c;
    
	*(yy_c_buf_p) = (yy_hold_char);

	if ( *(yy_c_buf_p) == YY_END_OF_BUFFER_CHAR )
		{
		/* yy_c_buf_p now points to the character we want to return.
		 * If this occurs *before* the EOB characters, then it's a
		 * valid NUL; if not, then we've hit the end of the buffer.
		 */
		if ( (yy_c_buf_p) < &YY_CURRENT_BUFFER_LVALUE->yy_ch_buf[(yy_n_chars)] )
			/* This was really a NUL. */
			*(yy_c_buf_p) = '\0';

		else
			{ /* need more input */
			int offset = (yy_c_buf_p) - (yytext_ptr);
			++(yy_c_buf_p);

			switch ( yy_get_next_buffer(  ) )
				{
				case EOB_ACT_LAST_MATCH:
					/* This happens because yy_g_n_b()
					 * sees that we've accumulated a
					 * token and flags that we need to
					 * try matching the token before
					 * proceeding.  But for input(),
					 * there's no matching to consider.
					 * So convert the EOB_ACT_LAST_MATCH
					 * to EOB_ACT_END_OF_FILE.
					 */

					/* Reset buffer status. */
					fortran_restart(fortran_in );

					/*FALLTHROUGH*/

				case EOB_ACT_END_OF_FILE:
					{
					if ( fortran_wrap( ) )
						return EOF;

					if ( ! (yy_did_buffer_switch_on_eof) )
						YY_NEW_FILE;
#ifdef __cplusplus
					return yyinput();
#else
					return input();
#endif
					}

				case EOB_ACT_CONTINUE_SCAN:
					(yy_c_buf_p) = (yytext_ptr) + offset;
					break;
				}
			}
		}

	c = *(unsigned char *) (yy_c_buf_p);	/* cast for 8-bit char's */
	*(yy_c_buf_p) = '\0';	/* preserve fortran_text */
	(yy_hold_char) = *++(yy_c_buf_p);

	YY_CURRENT_BUFFER_LVALUE->yy_at_bol = (c == '\n');

	return c;
}
#endif	/* ifndef YY_NO_INPUT */

/** Immediately switch to a different input stream.
 * @param input_file A readable stream.
 * 
 * @note This function does not reset the start condition to @c INITIAL .
 */
    void fortran_restart  (FILE * input_file )
{
    
	if ( ! YY_CURRENT_BUFFER ){
        fortran_ensure_buffer_stack ();
		YY_CURRENT_BUFFER_LVALUE =
            fortran__create_buffer(fortran_in,YY_BUF_SIZE );
	}

	fortran__init_buffer(YY_CURRENT_BUFFER,input_file );
	fortran__load_buffer_state( );
}

/** Switch to a different input buffer.
 * @param new_buffer The new input buffer.
 * 
 */
    void fortran__switch_to_buffer  (YY_BUFFER_STATE  new_buffer )
{
    
	/* TODO. We should be able to replace this entire function body
	 * with
	 *		fortran_pop_buffer_state();
	 *		fortran_push_buffer_state(new_buffer);
     */
	fortran_ensure_buffer_stack ();
	if ( YY_CURRENT_BUFFER == new_buffer )
		return;

	if ( YY_CURRENT_BUFFER )
		{
		/* Flush out information for old buffer. */
		*(yy_c_buf_p) = (yy_hold_char);
		YY_CURRENT_BUFFER_LVALUE->yy_buf_pos = (yy_c_buf_p);
		YY_CURRENT_BUFFER_LVALUE->yy_n_chars = (yy_n_chars);
		}

	YY_CURRENT_BUFFER_LVALUE = new_buffer;
	fortran__load_buffer_state( );

	/* We don't actually know whether we did this switch during
	 * EOF (fortran_wrap()) processing, but the only time this flag
	 * is looked at is after fortran_wrap() is called, so it's safe
	 * to go ahead and always set it.
	 */
	(yy_did_buffer_switch_on_eof) = 1;
}

static void fortran__load_buffer_state  (void)
{
    	(yy_n_chars) = YY_CURRENT_BUFFER_LVALUE->yy_n_chars;
	(yytext_ptr) = (yy_c_buf_p) = YY_CURRENT_BUFFER_LVALUE->yy_buf_pos;
	fortran_in = YY_CURRENT_BUFFER_LVALUE->yy_input_file;
	(yy_hold_char) = *(yy_c_buf_p);
}

/** Allocate and initialize an input buffer state.
 * @param file A readable stream.
 * @param size The character buffer size in bytes. When in doubt, use @c YY_BUF_SIZE.
 * 
 * @return the allocated buffer state.
 */
    YY_BUFFER_STATE fortran__create_buffer  (FILE * file, int  size )
{
	YY_BUFFER_STATE b;
    
	b = (YY_BUFFER_STATE) fortran_alloc(sizeof( struct yy_buffer_state )  );
	if ( ! b )
		YY_FATAL_ERROR( "out of dynamic memory in fortran__create_buffer()" );

	b->yy_buf_size = size;

	/* yy_ch_buf has to be 2 characters longer than the size given because
	 * we need to put in 2 end-of-buffer characters.
	 */
	b->yy_ch_buf = (char *) fortran_alloc(b->yy_buf_size + 2  );
	if ( ! b->yy_ch_buf )
		YY_FATAL_ERROR( "out of dynamic memory in fortran__create_buffer()" );

	b->yy_is_our_buffer = 1;

	fortran__init_buffer(b,file );

	return b;
}

/** Destroy the buffer.
 * @param b a buffer created with fortran__create_buffer()
 * 
 */
    void fortran__delete_buffer (YY_BUFFER_STATE  b )
{
    
	if ( ! b )
		return;

	if ( b == YY_CURRENT_BUFFER ) /* Not sure if we should pop here. */
		YY_CURRENT_BUFFER_LVALUE = (YY_BUFFER_STATE) 0;

	if ( b->yy_is_our_buffer )
		fortran_free((void *) b->yy_ch_buf  );

	fortran_free((void *) b  );
}

#ifndef __cplusplus
extern int isatty (int );
#endif /* __cplusplus */
    
/* Initializes or reinitializes a buffer.
 * This function is sometimes called more than once on the same buffer,
 * such as during a fortran_restart() or at EOF.
 */
    static void fortran__init_buffer  (YY_BUFFER_STATE  b, FILE * file )

{
	int oerrno = errno;
    
	fortran__flush_buffer(b );

	b->yy_input_file = file;
	b->yy_fill_buffer = 1;

    /* If b is the current buffer, then fortran__init_buffer was _probably_
     * called from fortran_restart() or through yy_get_next_buffer.
     * In that case, we don't want to reset the lineno or column.
     */
    if (b != YY_CURRENT_BUFFER){
        b->yy_bs_lineno = 1;
        b->yy_bs_column = 0;
    }

        b->yy_is_interactive = file ? (isatty( fileno(file) ) > 0) : 0;
    
	errno = oerrno;
}

/** Discard all buffered characters. On the next scan, YY_INPUT will be called.
 * @param b the buffer state to be flushed, usually @c YY_CURRENT_BUFFER.
 * 
 */
    void fortran__flush_buffer (YY_BUFFER_STATE  b )
{
    	if ( ! b )
		return;

	b->yy_n_chars = 0;

	/* We always need two end-of-buffer characters.  The first causes
	 * a transition to the end-of-buffer state.  The second causes
	 * a jam in that state.
	 */
	b->yy_ch_buf[0] = YY_END_OF_BUFFER_CHAR;
	b->yy_ch_buf[1] = YY_END_OF_BUFFER_CHAR;

	b->yy_buf_pos = &b->yy_ch_buf[0];

	b->yy_at_bol = 1;
	b->yy_buffer_status = YY_BUFFER_NEW;

	if ( b == YY_CURRENT_BUFFER )
		fortran__load_buffer_state( );
}

/** Pushes the new state onto the stack. The new state becomes
 *  the current state. This function will allocate the stack
 *  if necessary.
 *  @param new_buffer The new state.
 *  
 */
void fortran_push_buffer_state (YY_BUFFER_STATE new_buffer )
{
    	if (new_buffer == NULL)
		return;

	fortran_ensure_buffer_stack();

	/* This block is copied from fortran__switch_to_buffer. */
	if ( YY_CURRENT_BUFFER )
		{
		/* Flush out information for old buffer. */
		*(yy_c_buf_p) = (yy_hold_char);
		YY_CURRENT_BUFFER_LVALUE->yy_buf_pos = (yy_c_buf_p);
		YY_CURRENT_BUFFER_LVALUE->yy_n_chars = (yy_n_chars);
		}

	/* Only push if top exists. Otherwise, replace top. */
	if (YY_CURRENT_BUFFER)
		(yy_buffer_stack_top)++;
	YY_CURRENT_BUFFER_LVALUE = new_buffer;

	/* copied from fortran__switch_to_buffer. */
	fortran__load_buffer_state( );
	(yy_did_buffer_switch_on_eof) = 1;
}

/** Removes and deletes the top of the stack, if present.
 *  The next element becomes the new top.
 *  
 */
void fortran_pop_buffer_state (void)
{
    	if (!YY_CURRENT_BUFFER)
		return;

	fortran__delete_buffer(YY_CURRENT_BUFFER );
	YY_CURRENT_BUFFER_LVALUE = NULL;
	if ((yy_buffer_stack_top) > 0)
		--(yy_buffer_stack_top);

	if (YY_CURRENT_BUFFER) {
		fortran__load_buffer_state( );
		(yy_did_buffer_switch_on_eof) = 1;
	}
}

/* Allocates the stack if it does not exist.
 *  Guarantees space for at least one push.
 */
static void fortran_ensure_buffer_stack (void)
{
	int num_to_alloc;
    
	if (!(yy_buffer_stack)) {

		/* First allocation is just for 2 elements, since we don't know if this
		 * scanner will even need a stack. We use 2 instead of 1 to avoid an
		 * immediate realloc on the next call.
         */
		num_to_alloc = 1;
		(yy_buffer_stack) = (struct yy_buffer_state**)fortran_alloc
								(num_to_alloc * sizeof(struct yy_buffer_state*)
								);
		if ( ! (yy_buffer_stack) )
			YY_FATAL_ERROR( "out of dynamic memory in fortran_ensure_buffer_stack()" );
								  
		memset((yy_buffer_stack), 0, num_to_alloc * sizeof(struct yy_buffer_state*));
				
		(yy_buffer_stack_max) = num_to_alloc;
		(yy_buffer_stack_top) = 0;
		return;
	}

	if ((yy_buffer_stack_top) >= ((yy_buffer_stack_max)) - 1){

		/* Increase the buffer to prepare for a possible push. */
		int grow_size = 8 /* arbitrary grow size */;

		num_to_alloc = (yy_buffer_stack_max) + grow_size;
		(yy_buffer_stack) = (struct yy_buffer_state**)fortran_realloc
								((yy_buffer_stack),
								num_to_alloc * sizeof(struct yy_buffer_state*)
								);
		if ( ! (yy_buffer_stack) )
			YY_FATAL_ERROR( "out of dynamic memory in fortran_ensure_buffer_stack()" );

		/* zero only the new slots.*/
		memset((yy_buffer_stack) + (yy_buffer_stack_max), 0, grow_size * sizeof(struct yy_buffer_state*));
		(yy_buffer_stack_max) = num_to_alloc;
	}
}

/** Setup the input buffer state to scan directly from a user-specified character buffer.
 * @param base the character buffer
 * @param size the size in bytes of the character buffer
 * 
 * @return the newly allocated buffer state object. 
 */
YY_BUFFER_STATE fortran__scan_buffer  (char * base, yy_size_t  size )
{
	YY_BUFFER_STATE b;
    
	if ( size < 2 ||
	     base[size-2] != YY_END_OF_BUFFER_CHAR ||
	     base[size-1] != YY_END_OF_BUFFER_CHAR )
		/* They forgot to leave room for the EOB's. */
		return 0;

	b = (YY_BUFFER_STATE) fortran_alloc(sizeof( struct yy_buffer_state )  );
	if ( ! b )
		YY_FATAL_ERROR( "out of dynamic memory in fortran__scan_buffer()" );

	b->yy_buf_size = size - 2;	/* "- 2" to take care of EOB's */
	b->yy_buf_pos = b->yy_ch_buf = base;
	b->yy_is_our_buffer = 0;
	b->yy_input_file = 0;
	b->yy_n_chars = b->yy_buf_size;
	b->yy_is_interactive = 0;
	b->yy_at_bol = 1;
	b->yy_fill_buffer = 0;
	b->yy_buffer_status = YY_BUFFER_NEW;

	fortran__switch_to_buffer(b  );

	return b;
}

/** Setup the input buffer state to scan a string. The next call to fortran_lex() will
 * scan from a @e copy of @a str.
 * @param yystr a NUL-terminated string to scan
 * 
 * @return the newly allocated buffer state object.
 * @note If you want to scan bytes that may contain NUL values, then use
 *       fortran__scan_bytes() instead.
 */
YY_BUFFER_STATE fortran__scan_string (yyconst char * yystr )
{
    
	return fortran__scan_bytes(yystr,strlen(yystr) );
}

/** Setup the input buffer state to scan the given bytes. The next call to fortran_lex() will
 * scan from a @e copy of @a bytes.
 * @param bytes the byte buffer to scan
 * @param len the number of bytes in the buffer pointed to by @a bytes.
 * 
 * @return the newly allocated buffer state object.
 */
YY_BUFFER_STATE fortran__scan_bytes  (yyconst char * yybytes, int  _yybytes_len )
{
	YY_BUFFER_STATE b;
	char *buf;
	yy_size_t n;
	int i;
    
	/* Get memory for full buffer, including space for trailing EOB's. */
	n = _yybytes_len + 2;
	buf = (char *) fortran_alloc(n  );
	if ( ! buf )
		YY_FATAL_ERROR( "out of dynamic memory in fortran__scan_bytes()" );

	for ( i = 0; i < _yybytes_len; ++i )
		buf[i] = yybytes[i];

	buf[_yybytes_len] = buf[_yybytes_len+1] = YY_END_OF_BUFFER_CHAR;

	b = fortran__scan_buffer(buf,n );
	if ( ! b )
		YY_FATAL_ERROR( "bad buffer in fortran__scan_bytes()" );

	/* It's okay to grow etc. this buffer, and we should throw it
	 * away when we're done.
	 */
	b->yy_is_our_buffer = 1;

	return b;
}

#ifndef YY_EXIT_FAILURE
#define YY_EXIT_FAILURE 2
#endif

static void yy_fatal_error (yyconst char* msg )
{
    	(void) fprintf( stderr, "%s\n", msg );
	exit( YY_EXIT_FAILURE );
}

/* Redefine yyless() so it works in section 3 code. */

#undef yyless
#define yyless(n) \
	do \
		{ \
		/* Undo effects of setting up fortran_text. */ \
        int yyless_macro_arg = (n); \
        YY_LESS_LINENO(yyless_macro_arg);\
		fortran_text[fortran_leng] = (yy_hold_char); \
		(yy_c_buf_p) = fortran_text + yyless_macro_arg; \
		(yy_hold_char) = *(yy_c_buf_p); \
		*(yy_c_buf_p) = '\0'; \
		fortran_leng = yyless_macro_arg; \
		} \
	while ( 0 )

/* Accessor  methods (get/set functions) to struct members. */

/** Get the current line number.
 * 
 */
int fortran_get_lineno  (void)
{
        
    return fortran_lineno;
}

/** Get the input stream.
 * 
 */
FILE *fortran_get_in  (void)
{
        return fortran_in;
}

/** Get the output stream.
 * 
 */
FILE *fortran_get_out  (void)
{
        return fortran_out;
}

/** Get the length of the current token.
 * 
 */
int fortran_get_leng  (void)
{
        return fortran_leng;
}

/** Get the current token.
 * 
 */

char *fortran_get_text  (void)
{
        return fortran_text;
}

/** Set the current line number.
 * @param line_number
 * 
 */
void fortran_set_lineno (int  line_number )
{
    
    fortran_lineno = line_number;
}

/** Set the input stream. This does not discard the current
 * input buffer.
 * @param in_str A readable stream.
 * 
 * @see fortran__switch_to_buffer
 */
void fortran_set_in (FILE *  in_str )
{
        fortran_in = in_str ;
}

void fortran_set_out (FILE *  out_str )
{
        fortran_out = out_str ;
}

int fortran_get_debug  (void)
{
        return fortran__flex_debug;
}

void fortran_set_debug (int  bdebug )
{
        fortran__flex_debug = bdebug ;
}

static int yy_init_globals (void)
{
        /* Initialization is the same as for the non-reentrant scanner.
     * This function is called from fortran_lex_destroy(), so don't allocate here.
     */

    (yy_buffer_stack) = 0;
    (yy_buffer_stack_top) = 0;
    (yy_buffer_stack_max) = 0;
    (yy_c_buf_p) = (char *) 0;
    (yy_init) = 0;
    (yy_start) = 0;

/* Defined in main.c */
#ifdef YY_STDINIT
    fortran_in = stdin;
    fortran_out = stdout;
#else
    fortran_in = (FILE *) 0;
    fortran_out = (FILE *) 0;
#endif

    /* For future reference: Set errno on error, since we are called by
     * fortran_lex_init()
     */
    return 0;
}

/* fortran_lex_destroy is for both reentrant and non-reentrant scanners. */
int fortran_lex_destroy  (void)
{
    
    /* Pop the buffer stack, destroying each element. */
	while(YY_CURRENT_BUFFER){
		fortran__delete_buffer(YY_CURRENT_BUFFER  );
		YY_CURRENT_BUFFER_LVALUE = NULL;
		fortran_pop_buffer_state();
	}

	/* Destroy the stack itself. */
	fortran_free((yy_buffer_stack) );
	(yy_buffer_stack) = NULL;

    /* Reset the globals. This is important in a non-reentrant scanner so the next time
     * fortran_lex() is called, initialization will occur. */
    yy_init_globals( );

    return 0;
}

/*
 * Internal utility routines.
 */

#ifndef yytext_ptr
static void yy_flex_strncpy (char* s1, yyconst char * s2, int n )
{
	register int i;
	for ( i = 0; i < n; ++i )
		s1[i] = s2[i];
}
#endif

#ifdef YY_NEED_STRLEN
static int yy_flex_strlen (yyconst char * s )
{
	register int n;
	for ( n = 0; s[n]; ++n )
		;

	return n;
}
#endif

void *fortran_alloc (yy_size_t  size )
{
	return (void *) malloc( size );
}

void *fortran_realloc  (void * ptr, yy_size_t  size )
{
	/* The cast to (char *) in the following accommodates both
	 * implementations that use char* generic pointers, and those
	 * that use void* generic pointers.  It works with the latter
	 * because both ANSI C and C++ allow castless assignment from
	 * any pointer type to void*, and deal with argument conversions
	 * as though doing an assignment.
	 */
	return (void *) realloc( (char *) ptr, size );
}

void fortran_free (void * ptr )
{
	free( (char *) ptr );	/* see fortran_realloc() for (char *) cast */
}

#define YYTABLES_NAME "yytables"

#line 283 "fortran.lex"



void out_of_donottreat ( void )
{
    BEGIN(INITIAL);
    if (infixed) BEGIN(fortran77style) ;
    if (infree)  BEGIN(fortran90style) ;
    line_num_input++;
}
