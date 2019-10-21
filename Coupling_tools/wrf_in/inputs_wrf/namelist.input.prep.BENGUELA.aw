 &time_control
 run_days                            = 0,
 run_hours                           = 0,
 run_minutes                         = 0,
 run_seconds                         = 0,
 start_year                          = <yr1>,<yr1>,<yr1>,
 start_month                         = <mo1>,<mo1>,<mo1>,
 start_day                           = <dy1>,<dy1>,<dy1>,
 start_hour                          = <hr1>,<hr1>,<hr1>,
 start_minute                        = 00,00,00,
 start_second                        = 00,00,00,
 end_year                            = <yr2>,<yr2>,<yr2>,
 end_month                           = <mo2>,<mo2>,<mo2>,
 end_day                             = <dy2>,<dy2>,<dy2>,
 end_hour                            = <hr2>,<hr2>,<hr2>,
 end_minute                          = 00,00,00,
 end_second                          = 00,00,00,
 interval_seconds                    = 21600
 input_from_file                     = .true.,.true.,.true.,
 history_interval_h                  = <his_int_h>,<his_int_h>,<his_int_h>,
 frames_per_outfile                  = <his_nb_out>,<his_nb_out>,<his_nb_out>,
 restart                             = .<rst>.,
 restart_interval_h                  = <rst_int_h>,
 override_restart_timers             = .true.
 write_hist_at_0h_rst                = .true.,
 io_form_history                     = 2
 io_form_restart                     = 2
 io_form_input                       = 2
 io_form_boundary                    = 2
 io_form_auxinput4                   = 2
 auxinput4_inname                    = "wrflowinp_d<domain>"
 auxinput4_interval                  = 360,360,360,
 auxinput4_end_h                     = 0,0,0, 
 output_diagnostics                  = 1
 auxhist3_outname                    = "wrfxtrm_d<domain>_<date>"
 io_form_auxhist3                    = 2
 auxhist3_interval                   = <xtrm_int_m>,<xtrm_int_m>,<xtrm_int_m>,
 frames_per_auxhist3                 = <xtrm_nb_out>,<xtrm_nb_out>,<xtrm_nb_out>,
 iofields_filename                   = "myoutfields.txt", "myoutfields.txt", "myoutfields.txt"
 debug_level                         = 10
 /

 &domains
 time_step                           = 150,
 time_step_fract_num                 = 0,
 time_step_fract_den                 = 1,
 max_dom                             = 1,
 e_we                                = 56,456,277,
 e_sn                                = 50,476,349,
 e_vert                              = 60,60,60,
 eta_levels                          =  1.000, 0.9977 , 0.9953 , 0.9930 , 0.9897 , 0.9863 , 0.9830 , 0.9787 , 0.9743 , 0.9700 , 0.9647 , 0.9593 , 0.9540 , 0.9473 , 0.9407 , 0.9340 , 0.9257 , 0.9173 , 0.9090 , 0.8993 , 0.8897 , 0.8800 , 0.8664 , 0.8528 , 0.8256 , 0.7984 , 0.7712 , 0.7209 , 0.6731 , 0.6277 , 0.5846 , 0.5439 , 0.5052 , 0.4686 , 0.4339 , 0.4011 , 0.3701 , 0.3408 , 0.3132 , 0.2871 , 0.2625 , 0.2393 , 0.2174 , 0.1969 , 0.1775 , 0.1594 , 0.1423 , 0.1263 , 0.1114 , 0.0973 , 0.0842 , 0.0719 , 0.0605 , 0.0498 , 0.0399 , 0.0307 , 0.0221 , 0.0142 , 0.0068 , 0.0000,
 p_top_requested                     = 5000,
 num_metgrid_levels                  = 38,
 num_metgrid_soil_levels             = 4,
 dx                                  = 30000,6000,2000,
 dy                                  = 30000,6000,2000,
 grid_id                             = 1,     2,     3,
 parent_id                           = 0,     1,     2,
 i_parent_start                      = 1,32,35,
 j_parent_start                      = 1,153,24,
 parent_grid_ratio                   = 1,5,3,
 parent_time_step_ratio              = 1,5,3,
 feedback                            = 1,
 smooth_option                       = 2
 num_ext_model_couple_dom            = 1,
 nproc_x                             = <nproc_x>,
 nproc_y                             = <nproc_y>,
 numtiles                            = 1
 /

 &physics
 mp_physics                          = 6,     6,     6,
 mp_zero_out                         = 2,
 mp_zero_out_thresh                  = 1.e-8,
 progn                               = 0,0,0,
 ra_lw_physics                       = 1,     1,     1,
 ra_sw_physics                       = 1,     1,     1,
 radt                                = 20,    5,     5,
 sf_sfclay_physics                   = 1,     1,     1,
 sf_surface_physics                  = 1,     1,     1,
 sf_urban_physics                    = 0,     0,     0,
 bl_pbl_physics                      = 1,     1,     1,
 bldt                                = 0,     0,     0,
 cu_physics                          = 2,     0,     0,
 cudt                                = 5,     5,     5,
 shcu_physics                        = 0,
 ishallow                            = 0,
 cu_diag                             = 0,
 cu_rad_feedback                     = .false.,.false.,.false.,
 isfflx                              = 1,
 ifsnow                              = 0,
 icloud                              = 1,
 surface_input_source                = 1,
 num_soil_layers                     = 4,
 sst_update                          = 1,
 usemonalb                           = .false.,
 tmn_update                          = 0,
 lagday                              = 150,
 sst_skin                            = 0,
 isftcflx                            = 5,
 prec_acc_dt                         = 360.,360.,
 /

 &dynamics
 rk_ord                              = 3,
 diff_opt                            = 1,1,1,
 km_opt                              = 4,4,4,
 diff_6th_opt                        = 0,0,0,
 diff_6th_factor                     = 0.12,0.12,0.12,
 damp_opt                            = 0,
 dampcoef                            = 0.2,0.2,0.2
 zdamp                               = 5000.,5000.,5000.,
 w_damping                           = 0,
 base_temp                           = 290.
 khdif                               = 0,0,0,
 kvdif                               = 0,0,0,
 smdiv                               = 0.1,0.1,0.1,
 emdiv                               = 0.01,0.01,0.01,
 epssm                               = 0.1,0.1,0.1,
 non_hydrostatic                     = .true., .true., .true.,
 h_mom_adv_order                     = 5,5,5,
 v_mom_adv_order                     = 3,3,3,
 h_sca_adv_order                     = 5,5,5,
 v_sca_adv_order                     = 3,3,3,
 moist_adv_opt                       = 1,1,1,     
 scalar_adv_opt                      = 2,2,2,     
 tke_adv_opt                         = 2,2,2,
 tracer_adv_opt                      = 0,0,0,
 tracer_opt                          = 5,5,5,
 dyn_opt                             = 2,
 gwd_opt                             = 1,
 time_step_sound                     = 4,
 /

 &bdy_control
 spec_bdy_width                      = 5,
 spec_zone                           = 1,
 relax_zone                          = 4,
 specified                           = .true., .false.,.false.,
 nested                              = .false., .true., .true.,
 spec_exp                            = 0.33,
 periodic_x                          = .false.,
 symmetric_xs                        = .false.,
 symmetric_xe                        = .false.,
 open_xs                             = .false.,
 open_xe                             = .false.,
 periodic_y                          = .false.,
 symmetric_ys                        = .false.,
 symmetric_ye                        = .false.,
 open_ys                             = .false.,
 open_ye                             = .false.,
 /

 &grib2
 /

 &namelist_quilt
 nio_tasks_per_group = <niotaskpg>,
 nio_groups = <niogp>,
 /

 &fdda
 grid_fdda                           = 0,0,0,
 gfdda_inname                        = "wrffdda_d<domain>",
 gfdda_end_h                         = 144, 144, 144,
 gfdda_interval_m                    = 360,360,360,
 fgdt                                = 0,     0,     0,
 fgdtzero                            = 0,     0,     0,
 if_no_pbl_nudging_uv                = 1,     1,     1,
 if_no_pbl_nudging_t                 = 1,     1,     1,
 if_no_pbl_nudging_ph                = 1,     1,     1,
 if_no_pbl_nudging_q                 = 1,     1,     1,
 if_zfac_uv                          = 1,     1,     1,
 k_zfac_uv                           = 9,     9,     9,
 if_zfac_t                           = 1,     1,     1,
 k_zfac_t                            = 9,     9,     9,
 if_zfac_ph                          = 1,     1,     1,
 k_zfac_ph                           = 9,     9,     9,
 if_zfac_q                           = 1,     1,     1,
 k_zfac_q                            = 9,     9,     9,
 dk_zfac_uv                          = 1,     1,     1,
 dk_zfac_t                           = 1,     1,     1,
 dk_zfac_ph                          = 1,     1,     1,
 guv                                 = 0.0003,     0,     0,
 gt                                  = 0.0003,     0,     0,
 gph                                 = 0.0003,     0,     0,
 gq                                  = 0,     0,     0,
 xwavenum                            = 3,0,0
 ywavenum                            = 3,0,0
 if_ramping                          = 0,
 dtramp_min                          = 60.0,
 io_form_gfdda                       = 2,
 /
