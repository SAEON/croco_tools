#- Creation des elements relatifs a AGRIF (lib, config)
#---------------------------------------------------------------------
SHELL = /bin/sh
#---------------------------------------------------------------------

DIR_OBJS      = AGRIF_OBJS
DIR_FILES     = AGRIF_FILES
DIR_YOURFILES = AGRIF_YOURFILES

FILENAMES = 	modbc modcluster modinit modinitvars modinterp modinterpbasic \
				modtypes modbcfunction modutil modcurgridfunctions \
				modmask modsauv modupdate modmpp \
				modupdatebasic modlinktomodel modarrays modflux modvariables
           
OBJS=$(addsuffix .o,$(addprefix $(DIR_OBJS)/,$(FILENAMES)))
FILES=$(addsuffix .F90,$(addprefix $(DIR_FILES)/,$(FILENAMES)))

all: conv libagrif.a
	@echo
	@echo ===================================================
	@echo   AGRIF is OK
	@echo ===================================================
	@echo
	
conv: 
	$(MAKE) -C LIB conv
	mv -f LIB/conv .
	
libagrif.a : prep_lib $(OBJS)
	$(AR) -r $@ $(OBJS)
	ranlib $@

prep_lib:
	mkdir -p $(DIR_YOURFILES)
	mkdir -p $(DIR_OBJS)

$(DIR_OBJS)/%.o : $(DIR_FILES)/%.F90
	$(RM) $(DIR_YOURFILES)/$(*F).f90
	$(CPP) $(CPPFLAGS) $(DIR_FILES)/$(*F).F90 > $(DIR_YOURFILES)/$(*F).f90
	$(FC) $(FFLAGS) -I.. -c $(DIR_YOURFILES)/$(*F).f90 -o $(DIR_OBJS)/$(*F).o

$(DIR_OBJS)/modarrays.o: $(DIR_OBJS)/modtypes.o

$(DIR_OBJS)/modbc.o: $(DIR_OBJS)/modinterp.o

$(DIR_OBJS)/modbcfunction.o: $(DIR_OBJS)/modupdate.o 	\
							 $(DIR_OBJS)/modbc.o 		\
							 $(DIR_OBJS)/modinterp.o 	\
							 $(DIR_OBJS)/modtypes.o 	\
							 $(DIR_OBJS)/modflux.o

$(DIR_OBJS)/modcluster.o: $(DIR_OBJS)/modtypes.o 		\
						  $(DIR_OBJS)/modlinktomodel.o 	\
						  $(DIR_OBJS)/modsauv.o 		\
						  $(DIR_OBJS)/modinitvars.o 	\
						  $(DIR_OBJS)/modcurgridfunctions.o

$(DIR_OBJS)/modcurgridfunctions.o: $(DIR_OBJS)/modinit.o \
								   $(DIR_OBJS)/modtypes.o

$(DIR_OBJS)/modinit.o: $(DIR_OBJS)/modlinktomodel.o \
					   $(DIR_OBJS)/modtypes.o

$(DIR_OBJS)/modinitvars.o: $(DIR_OBJS)/modlinktomodel.o \
						   $(DIR_OBJS)/modtypes.o

$(DIR_OBJS)/modinterpbasic.o: $(DIR_OBJS)/modtypes.o

$(DIR_OBJS)/modinterp.o: $(DIR_OBJS)/modcurgridfunctions.o \
						 $(DIR_OBJS)/modmask.o \
						 $(DIR_OBJS)/modarrays.o \
						 $(DIR_OBJS)/modmpp.o \
						 $(DIR_OBJS)/modinterpbasic.o

$(DIR_OBJS)/modlinktomodel.o: $(DIR_OBJS)/modtypes.o

$(DIR_OBJS)/modmask.o: $(DIR_OBJS)/modtypes.o

$(DIR_OBJS)/modsauv.o: $(DIR_OBJS)/modarrays.o \
			$(DIR_OBJS)/modlinktomodel.o \
			$(DIR_OBJS)/modtypes.o $(DIR_OBJS)/modvariables.o

$(DIR_OBJS)/modupdatebasic.o: $(DIR_OBJS)/modtypes.o

$(DIR_OBJS)/modupdate.o: $(DIR_OBJS)/modmask.o \
			$(DIR_OBJS)/modcurgridfunctions.o \
			$(DIR_OBJS)/modarrays.o \
			$(DIR_OBJS)/modmpp.o \
			$(DIR_OBJS)/modupdatebasic.o

$(DIR_OBJS)/modutil.o: $(DIR_OBJS)/modtypes.o \
			$(DIR_OBJS)/modcurgridfunctions.o \
			$(DIR_OBJS)/modlinktomodel.o \
			$(DIR_OBJS)/modsauv.o \
			$(DIR_OBJS)/modcluster.o

$(DIR_OBJS)/modmpp.o: $(DIR_OBJS)/modtypes.o \
			$(DIR_OBJS)/modarrays.o

.PHONY: doc

doc:
	(cd doc ; doxygen Doxyfile )

clean: clean-conv
	$(RM) $(OBJS) libagrif.a *.mod $(DIR_YOURFILES)/*

clean-all: clean
	$(MAKE) -C LIB clean-all

clean-conv:
	$(MAKE) -C LIB clean-all
	$(RM) conv
