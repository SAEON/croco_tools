libagrif.a : $(OBJS)
	$(AR) -r $@ $(OBJS)      
        
$(AGRIFOBJS)/%.o : $(AGRIFFILES)/%.F
	$(SRM) $(AGRIFYOURFILES)/$(*F).f
	$(CPP) -I$(AGRIFINC) -I../$(COMDIROUT) -I.. $(AGRIFFILES)/$(*F).F | ../mpc > $(AGRIFYOURFILES)/$(*F).f
	$(F90) $(FFLAGS_EXTEND) -I../$(COMDIROUT) -I.. -c $(AGRIFYOURFILES)/$(*F).f -o $(AGRIFOBJS)/$(*F).o		


$(AGRIFOBJS)/modbc.o: $(AGRIFOBJS)/modinterp.o

$(AGRIFOBJS)/modbcfunction.o: $(AGRIFOBJS)/modtypes.o 
$(AGRIFOBJS)/modbcfunction.o: $(AGRIFOBJS)/modinterp.o 
$(AGRIFOBJS)/modbcfunction.o: $(AGRIFOBJS)/modbc.o 
$(AGRIFOBJS)/modbcfunction.o: $(AGRIFOBJS)/modupdate.o 
$(AGRIFOBJS)/modbcfunction.o: $(AGRIFOBJS)/modflux.o

$(AGRIFOBJS)/modcluster.o: $(AGRIFOBJS)/modcurgridfunctions.o $(AGRIFOBJS)/modinitvars.o
$(AGRIFOBJS)/modcluster.o: $(AGRIFOBJS)/modsauv.o $(AGRIFOBJS)/modlinktomodel.o
$(AGRIFOBJS)/modcluster.o: $(AGRIFOBJS)/modtypes.o

$(AGRIFOBJS)/modcurgridfunctions.o: $(AGRIFOBJS)/modinit.o
$(AGRIFOBJS)/modcurgridfunctions.o: $(AGRIFOBJS)/modtypes.o

$(AGRIFOBJS)/modinit.o: $(AGRIFOBJS)/modtypes.o
$(AGRIFOBJS)/modinit.o: $(AGRIFOBJS)/modlinktomodel.o

$(AGRIFOBJS)/modinitvars.o: $(AGRIFOBJS)/modtypes.o
$(AGRIFOBJS)/modinitvars.o: $(AGRIFOBJS)/modlinktomodel.o

$(AGRIFOBJS)/modinterp.o: $(AGRIFOBJS)/modcurgridfunctions.o $(AGRIFOBJS)/modinterpbasic.o
$(AGRIFOBJS)/modinterp.o: $(AGRIFOBJS)/modmask.o $(AGRIFOBJS)/modarrays.o $(AGRIFOBJS)/modmpp.o

$(AGRIFOBJS)/modmask.o: $(AGRIFOBJS)/modtypes.o

$(AGRIFOBJS)/modsauv.o: $(AGRIFOBJS)/modtypes.o
$(AGRIFOBJS)/modsauv.o: $(AGRIFOBJS)/modlinktomodel.o

$(AGRIFOBJS)/modutil.o: $(AGRIFOBJS)/modcluster.o $(AGRIFOBJS)/modsauv.o 
$(AGRIFOBJS)/modutil.o: $(AGRIFOBJS)/modlinktomodel.o $(AGRIFOBJS)/modtypes.o 
$(AGRIFOBJS)/modutil.o: $(AGRIFOBJS)/modbcfunction.o

$(AGRIFOBJS)/modupdate.o: $(AGRIFOBJS)/modupdatebasic.o $(AGRIFOBJS)/modinterp.o

clean : FORCE
	$(SRM) $(OBJS) libagrif.a *.mod
	
FORCE :
