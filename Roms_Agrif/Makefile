# $Id$
#
#======================================================================
# ROMS_AGRIF is a branch of ROMS developped at IRD and INRIA, in France
# The two other branches from UCLA (Shchepetkin et al) 
# and Rutgers University (Arango et al) are under MIT/X style license.
# ROMS_AGRIF specific routines (nesting) are under CeCILL-C license.
# 
# ROMS_AGRIF website : http://roms.mpl.ird.fr
#======================================================================
#
# Universal machine independent makefile for ROMS model 
#====================================================================
# Set machine dependent definitions and rules.

include Makedefs

# Configuration for SCRUM/ROMS model:
#====================================================================
# All source codes files are sorted into eight groups, separated by
# blanc lines:
#   1) main driving part;	 2) 2D time stepping engine;
#   3) 3D time stepping engine;	 4) sea-water EOS routines;
#   5) vertical mixing schemes;	 6) on-fly model diagnostic routines;
#   7) netCDF I/O routines;	 8) model forcing routines;
#   9) MPI routines;		10) Modules routines;
#  11) Floats routines;		12) Stations routines;
#  13) AGRIF routines;          14) PISCES routines
#--------------------------------------------------------------------

 SRCS = main.F		step.F		read_inp.F\
	timers.F	init_scalars.F	init_arrays.F	set_weights.F\
	set_scoord.F	ana_grid.F	setup_grid1.F	setup_grid2.F\
	set_nudgcof.F	ana_initial.F	analytical.F	zonavg.F\
	setup_kwds.F    check_kwds.F	check_srcs.F\
        check_switches1.F		check_switches2.F\
\
	step2d.F	u2dbc.F		v2dbc.F		zetabc.F\
	obc_volcons.F\
\
	pre_step3d.F	step3d_t.F	step3d_uv1.F	step3d_uv2.F\
	prsgrd.F	rhs3d.F		set_depth.F	omega.F\
	uv3dmix.F	uv3dmix_spg.F	t3dmix.F	t3dmix_spg.F\
	hmix_coef.F\
	u3dbc.F		v3dbc.F		t3dbc.F\
\
	rho_eos.F	ab_ratio.F	alfabeta.F\
\
	ana_vmix.F	bvf_mix.F	lmd_vmix.F\
	lmd_skpp.F	lmd_bkpp.F	lmd_swfrac.F	lmd_wscale.F\
\
	diag.F		wvlcty.F	checkdims.F	grid_stiffness.F\
	bio_diag.F\
\
	get_date.F	lenstr.F	closecdf.F	ana_initracer.F\
	insert_node.F	nf_fread.F	get_grid.F	get_initial.F\
	def_grid_2d.F   def_grid_3d.F   def_his.F       def_rst.F\
        set_avg.F       wrt_grid.F      wrt_his.F       wrt_rst.F\
        wrt_avg.F       output.F        put_global_atts.F\
	nf_fread_x.F 	nf_fread_y.F\
	def_diags.F			wrt_diags.F\
        wrt_diags_avg.F 		set_diags_avg.F\
        def_diagsM.F			wrt_diagsM.F\
        wrt_diagsM_avg.F 		set_diagsM_avg.F\
        def_bio_diags.F			wrt_bio_diags.F\
	wrt_bio_diags_avg.F 		set_bio_diags_avg.F\
\
        get_vbc.F       set_cycle.F     get_wwave.F\
        get_tclima.F    get_uclima.F    get_ssh.F       get_sss.F\
        get_smflux.F    get_stflux.F    get_srflux.F    get_sst.F\
        get_tides.F     clm_tides.F     get_bulk.F      bulk_flux.F\
        get_bry.F       get_bry_bio.F	nf_read_bry.F	sstskin.F\
        get_psource.F   nf_add_attribute.F fillvalue.F\
\
        online_bulk_var.F		online_get_bulk.F\
	online_interp.F			online_interpolate_bulk.F\
	online_set_bulk.F\
\
	MPI_Setup.F	MessPass2D.F	MessPass3D.F	exchange.F\
\
	biology.F	o2sato.F	sediment.F	bbl.F\
\
	init_floats.F	wrt_floats.F	step_floats.F	rhs_floats.F\
	interp_rho.F	def_floats.F	init_arrays_floats.F\
	random_walk.F	get_initial_floats.F\
\
	init_sta.F	wrt_sta.F	step_sta.F	interp_sta.F\
	def_sta.F	init_arrays_sta.F\
\
        zoom.F		update2D.F	set_nudgcof_fine.F\
	zoombc_2D.F	zoombc_3D.F	uv3dpremix.F\
        t3dpremix.F     update3D.F\
\
	ropigrid.F	ropistep.F	pirostep.F\
	trcini.F	trclsm.F	pisces_ini.F\
	p4zday.F	p4znano.F	p4zlys.F\
        p4zdiat.F       p4zopt.F        p4zsink.F\
        p4zflx.F        p4zprg.F        p4zslow.F\
        p4zint.F        p4zprod.F	p4zmicro.F\
        p4zlim.F        p4zrem.F	p4zche.F\
        p4zbio.F        p4zmeso.F       p4zsink2.F\
        p4zsed.F\
	autotiling.F debug.F Agrif2Model.F

AMRDIR = AGRIF/AGRIF_YOURFILES

#--------------------------------------------------------------------

 RCS = $(SRCS:.F=.f)

 OBJS = $(RCS:.f=.o)

 SBIN = roms

 AMRRCS=$(AMRSRCS:.F=.f)

 AMROBJS=$(AMRRCS:.f=.o)

#
# Eecutable file.
# ========= =====
#
$(SBIN): $(OBJS)
	 $(LDR) $(FFLAGS) $(LDFLAGS) -o a.out $(OBJS) $(LCDF) $(LMPI) 
	mv a.out $(SBIN)
#
# Everything
# ==========
all: tools depend $(SBIN)

#
# Auxiliary utility programs and List of Dependecies:
# ========= ======= ======== === ==== == ============
#
  TOOLS = mpc cross_matrix cppcheck srcscheck checkkwds partit ncjoin ncrename

tools: $(TOOLS)

mpc: mpc.F
	$(CPP) -P $(CPPFLAGS) mpc.F > mpc_.f
	$(LDR) $(FFLAGS) $(LDFLAGS) -o mpc mpc_.f
cross_matrix: cross_matrix.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o cross_matrix cross_matrix.o
cppcheck: cppcheck.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o cppcheck cppcheck.o
srcscheck: srcscheck.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o srcscheck srcscheck.o
checkkwds: checkkwds.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o checkkwds checkkwds.o
checkdefs: check_switches1.F setup_kwds.F

check_switches1.F: cppcheck cppdefs.h
	cat cppdefs.h set_global_definitions.h > mergcpp.txt
	./cppcheck
check_srcs.F: srcscheck Makefile
	./srcscheck
setup_kwds.F: checkkwds read_inp.F
	./checkkwds
partit: partit.o insert_node.o lenstr.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o partit partit.o insert_node.o lenstr.o $(LCDF)
ncjoin: ncjoin.o lenstr.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o ncjoin ncjoin.o lenstr.o $(LCDF)
ncrename: ncrename.o lenstr.o
	$(LDR) $(FFLAGS) $(LDFLAGS) -o ncrename ncrename.o lenstr.o $(LCDF)

depend: checkdefs cross_matrix
	./cross_matrix *.F

mymodules: $(MOBJS) $(AMROBJS)
#
# Target to create tar file.
# ====== == ====== === =====
#
tarfile:
	tar cvf roms.tar Make* *.h *.F *.README README.*  *.in *.in.*
#
# Cleaning:
# =========
#
rmtools:
	/bin/rm -f $(TOOLS)
clean:
	/bin/rm -rf core *.o $(AMRDIR)/*.o *.i *.s *.f *.trace *.mod ${COMP_FILES}

clobber: clean
	/bin/rm -rf $(SBIN) $(TOOLS) ./rii_files


plotter: plotter.F
	f77 -n32 -o plotter plotter.F $(LIBNCAR)

#
# Special treatment for barrier function:
# THERE SHALL BE NO OPTIMIZATION HERE!!!!
#
my_barrier.o: my_barrier.f
	$(CFT) -c -O0 my_barrier.f
#
# Include automatically generated dependency list:
#

include Make.depend

