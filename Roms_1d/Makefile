# Universal machine independent makefile for SCRUM/ROMS model 
#====================================================================
# Set machine dependent definitions and rules.

include Makedefs

# Configuration
#
 SRCS = main.F		setup_grid.F	setup_initial.F\
	set_forces.F	step3d.F	rho_eos.F	alfabeta.F\
	lmd_vmix.F	lmd_kpp.F	lmd_wscale.F	lmd_swfrac.F\
        bulk_flux.F     sst_skin.F	biology.F	o2sat.F\
        plot.F		set_color.F

 OBJS = $(SRCS:.F=.o)

#
# Target for executable.
#
roms1d: $(OBJS)
	$(LDR) $(FFLAGS) $(LDFLAGS) -o a.out $(OBJS) $(LIBNCAR)
	mv a.out roms1d
#
# Target to create list of dependecies: Make.depend
#
depend: cross_matrix
	cross_matrix *.F
#
# Auxiliary utilities cross_matrix and mpc
#
#
tools: cross_matrix mpc
#
mpc: mpc.F
	$(CPP) -P $(CPPFLAGS) mpc.F > mpc_.f
	$(LDR) $(FFLAGS) $(LDFLAGS) -o mpc mpc_.f
cross_matrix: cross_matrix.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o cross_matrix cross_matrix.o
#
# Auxiliary subroutine set_color: it must be compiled with
# single precision, so a special rule is needed.
#
set_color.o: set_color.F
	$(CFT) -c set_color.F
#
#  Cleaning targets.
#
clean:
	/bin/rm -f core *.o *.i *.s *.f *.trace

clobber: clean
	 /bin/rm -f mpc cross_matrix roms1d *.dat fort.* gmeta
#
#  All
#
all: tools  depend roms1d
#
# Include automatically generated dependency list:
#
include Make.depend

