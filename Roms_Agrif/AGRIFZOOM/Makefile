# $Id$
SRM=/bin/rm -f

#CPP = cpp
#CPPFLAGS = -traditional

AGRIFOBJS=AGRIF_OBJS
AGRIFINC=AGRIF_INC
AGRIFFILES=AGRIF_FILES
AGRIFYOURFILES=AGRIF_YOURFILES

FILENAMES= modbc modcluster modinit modinitvars modinterp modinterpbasic \
	   modtypes modutil modbcfunction modcurgridfunctions \
	   modmask modsauv modupdate \
	   modupdatebasic modlinktomodel modmpp modarrays modflux \
	modvariables
	   
OBJS1=$(addprefix $(AGRIFOBJS)/,$(FILENAMES))
OBJS=$(addsuffix .o,$(OBJS1))

AGRIF_FILES1=$(addprefix $(AGRIFFILES)/,$(FILENAMES))
AGRIF_FILES=$(addsuffix .F,$(AGRIF_FILES1))

all :   conv_exe libagrif.a
	echo AGRIF is OK

conv_exe : 
	$(MAKE) -C LIB

include rules.make


