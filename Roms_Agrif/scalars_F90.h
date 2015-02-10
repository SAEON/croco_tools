      real dt, dtfast, time, time2, time_start, tdays                       
      integer ndtfast, iic, kstp, krhs, knew, next_kstp                             
#ifdef SOLVE3D
      integer iif, nstp, nrhs, nnew, nbstep3d                              
#endif
#ifdef FLOATS
      integer nfp1, nf, nfm1, nfm2, nfm3                                   
#endif
#ifdef WKB_WWAVE
      integer wstp, wnew
#endif
      logical PREDICTOR_2D_STEP
      common /time_indices/  dt,dtfast, time, time2,time_start, tdays,    &
                             ndtfast, iic, kstp, krhs, knew, next_kstp,            &
#ifdef SOLVE3D
                             iif, nstp, nrhs, nnew, nbstep3d,             &
#endif
#ifdef FLOATS
                             nfp1, nf, nfm1, nfm2, nfm3,                  &
#endif
#ifdef WKB_WWAVE
                             wstp, wnew,                                  &
#endif
                             PREDICTOR_2D_STEP 




      real time_avg, time2_avg, rho0                                        &
                     , rdrg, rdrg2, Cdb_min, Cdb_max, Zob                   &
                     , xl, el, visc2, visc4, gamma2
#ifdef SOLVE3D
      real  theta_s,   theta_b,   Tcline,  hc
      real  sc_w(0:N), Cs_w(0:N), sc_r(N), Cs_r(N)
      real  rx0, rx1
      real  tnu2(NT),tnu4(NT)
# ifndef NONLIN_EOS
      real R0,T0,S0, Tcoef, Scoef
# endif
      real weight(6,0:NWEIGHT)

#endif
      common /scalars_main/                                                 &
                   time_avg, time2_avg,  rho0,      rdrg,    rdrg2          &
                 , Zob,       Cdb_min,   Cdb_max                            &
                 , xl, el,    visc2,     visc4,   gamma2                    &
#ifdef SOLVE3D
                 , theta_s,   theta_b,   Tcline,  hc                        &
                 , sc_w,      Cs_w,      sc_r,    Cs_r                      &
                 , rx0,       rx1,       tnu2,    tnu4                      &                
# ifndef NONLIN_EOS
                            , R0,T0,S0,  Tcoef,   Scoef                     &
# endif
                            , weight
#endif



#ifdef MPI
!
! MPI rlated variables
! === ====== =========
!
      logical EAST_INTER, WEST_INTER, NORTH_INTER, SOUTH_INTER
      integer mynode, ii,jj, p_W,p_E,p_S,p_N, p_SW,p_SE, p_NW,p_NE
      common /comm_setup/ mynode, ii,jj, p_W,p_E,p_S,p_N, p_SW,p_SE,
     &  p_NW,p_NE, EAST_INTER, WEST_INTER, NORTH_INTER, SOUTH_INTER
          
#endif


      real Eradius, g, day2sec,sec2day, jul_off,                        &
           year2day,day2year                                            
      parameter (Eradius=6371315.0,  day2sec=86400.,                   &
                 sec2day=1./86400., jul_off=2440000.,                  &
                 year2day=365.25, day2year=1./365.25)                   
!
! Acceleration of gravity (nondimensional for Soliton problem)
!
#ifdef SOLITON
      parameter (g=1.)
#else
      parameter (g=9.81)
#endif
!

