#- option to debug
C_D = -g # -g -Wall
# Compilation:
CC		= cc -O
#-
OBJS = main.o WriteInFile.o toamr.o fortran.o  \
       dependfile.o SubLoopCreation.o WorkWithlistvarindoloop.o \
       WorkWithvarofsubroutineliste.o WorkWithParameterlist.o \
       Writedeclarations.o WorkWithglobliste.o UtilFortran.o \
       UtilNotGridDep.o WorkWithlistdatavariable.o \
       DiversListe.o UtilAgrif.o WorkWithAllocatelist.o \
       UtilCharacter.o UtilListe.o UtilFile.o \
       WorkWithlistofmodulebysubroutine.o WorkWithlistmoduleinfile.o \
       WorkWithlistofcoupled.o


.SUFFIXES:
.SUFFIXES: .c .o

all : conv
	@echo CONV is ok

conv :  $(OBJS)
	@$(CC) $(OBJS) -o ../$@

main.o : main.c
fortran.o : fortran.c	
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
WorkWithlistofcoupled.o : WorkWithlistofcoupled.c decl.h
clean : 
	/bin/rm -f *.o y.output
