# Compilation:
CC		= cc -g -O
LEX		= flex

# option de flex et pas de lex
LEXFLAGS=-i
YACC = byacc -t -v


OBJS = main.o WriteInFile.o toamr.o fortran.o  \
       dependfile.o SubLoopCreation.o WorkWithlistvarindoloop.o \
       WorkWithvarofsubroutineliste.o WorkWithParameterlist.o \
       Writedeclarations.o WorkWithglobliste.o UtilFortran.o \
       UtilNotGridDep.o WorkWithlistdatavariable.o \
       DiversListe.o UtilAgrif.o WorkWithAllocatelist.o \
       UtilCharacter.o UtilListe.o UtilFile.o \
       WorkWithlistofmodulebysubroutine.o WorkWithlistmoduleinfile.o

.SUFFIXES:
.SUFFIXES: .c .o

all : conv

conv :  $(OBJS)
	$(CC) $(OBJS)  $(LEXLIB) -o ../$@

main.o : main.c
main.c : convert.tab.c  convert.yy.c
	rm -f main.c
	cat convert.tab.c  convert.yy.c > main.c
	rm -f convert.yy.c convert.tab.c
fortran.o : fortran.c	
fortran.c : fortran.tab.c fortran.yy.c
	rm -f fortran.c
	cat fortran.tab.c  fortran.yy.c > fortran.c	
	rm -f fortran.yy.c fortran.tab.c
convert.tab.c : convert.y decl.h
	$(YACC) convert.y
	mv -f y.tab.c convert.tab.c
fortran.tab.c : fortran.y decl.h
	$(YACC) -p fortran fortran.y
	mv -f y.tab.c fortran.tab.c	
convert.yy.c : convert.lex
	$(LEX) $(LEXFLAGS) -oconvert.yy.c convert.lex 
fortran.yy.c : fortran.lex
	$(LEX) $(LEXFLAGS) -Pfortran -ofortran.yy.c fortran.lex
	
toamr.o : toamr.c decl.h
WriteInFile.o : WriteInFile.c decl.h
dependfile.o : dependfile.c decl.h
SubLoopCreation.o : SubLoopCreation.c decl.h
WorkWithglobliste.o : WorkWithglobliste.c decl.h
WorkWithlistvarindoloop.o : WorkWithlistvarindoloop.c decl.h
WorkWithvarofsubroutineliste.o : WorkWithvarofsubroutineliste.c decl.h
Writedeclarations.o : Writedeclarations.c decl.h
UtilFortran.o : UtilFortran.c decl.h
WorkWithParameterlist.o : WorkWithParameterlist.c decl.h
UtilNotGridDep.o : UtilNotGridDep.c decl.h
WorkWithlistdatavariable.o : WorkWithlistdatavariable.c decl.h
DiversListe.o : DiversListe.c decl.h
UtilAgrif.o : UtilAgrif.c decl.h
WorkWithAllocatelist.o : WorkWithAllocatelist.c decl.h
UtilCharacter.o : UtilCharacter.c decl.h
UtilListe.o : UtilListe.c decl.h
UtilFile.o : UtilFile.c decl.h
WorkWithlistofmodulebysubroutine.o : WorkWithlistofmodulebysubroutine.c decl.h
WorkWithlistmoduleinfile.o : WorkWithlistmoduleinfile.c decl.h
clean : 
	/bin/rm -f *.o y.tab.c main.c lex.yy.c fortran.c \
	fortran.tab.c fortran.yy.c convert.tab.c convert.yy.c \
	y.output
