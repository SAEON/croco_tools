function  create_grid(nx,ny,grdname,title,new_scoord)
%
% This is part of Easy Grid
%  (c) 2008, Jeroen Molemaker, UCLA
%
% Modified to create a grid as croco_tools: QB 21/05/2019
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

nw = netcdf(grdname, 'clobber');
result = redef(nw);

%
%  Create dimensions
%
L  = nx - 1
M  = ny - 1
Lp = L + 1
Mp = M + 1

nw('xi_u') = L;
nw('eta_u') = Mp;
nw('xi_v') = Lp;
nw('eta_v') = M;
nw('xi_rho') = Lp;
nw('eta_rho') = Mp;
nw('xi_psi') = L;
nw('eta_psi') = M;

%nw('xi_rho')  = nx;
%nw('eta_rho') = ny;
%nw('xi_u')  = nx-1;
%nw('eta_u') = ny-1;
%nw('xi_psi')  = nx-1;
%nw('eta_psi') = ny-1;
nw('one') = 1;

%
%  Create variables and attributes
%

nw{'spherical'} = ncchar('one');
nw{'spherical'}.long_name = 'Grid type logical switch';
nw{'spherical'}.option_T = 'spherical';

nw{'angle'} = ncdouble('eta_rho', 'xi_rho');
nw{'angle'}.long_name = 'angle between xi axis and east';
nw{'angle'}.units = 'radians';

nw{'h'} = ncdouble('eta_rho', 'xi_rho');
nw{'h'}.long_name = 'Final bathymetry at RHO-points';
nw{'h'}.units = 'meter';

nw{'hraw'} = ncdouble('eta_rho', 'xi_rho');
nw{'hraw'}.long_name = 'Working bathymetry at RHO-points';
nw{'hraw'}.units = 'meter';

nw{'f'} = ncdouble('eta_rho', 'xi_rho');
nw{'f'}.long_name = 'Coriolis parameter at RHO-points';
nw{'f'}.units = 'second-1';

nw{'pm'} = ncdouble('eta_rho', 'xi_rho');
nw{'pm'}.long_name = 'curvilinear coordinate metric in XI';
nw{'pm'}.units = 'meter-1';

nw{'pn'} = ncdouble('eta_rho', 'xi_rho');
nw{'pn'}.long_name = 'curvilinear coordinate metric in ETA';
nw{'pn'}.units = 'meter-1';

nw{'lon_rho'} = ncdouble('eta_rho', 'xi_rho');
nw{'lon_rho'}.long_name = 'longitude of RHO-points';
nw{'lon_rho'}.units = 'degree_east';

nw{'lat_rho'} = ncdouble('eta_rho', 'xi_rho');
nw{'lat_rho'}.long_name = 'latitude of RHO-points';
nw{'lat_rho'}.units = 'degree_north';

nw{'mask_rho'} = ncdouble('eta_rho', 'xi_rho');
nw{'mask_rho'}.long_name = 'mask on RHO-points';
nw{'mask_rho'}.option_0 = 'land';
nw{'mask_rho'}.option_1 = 'water';

nw{'lon_psi'} = ncdouble('eta_psi', 'xi_psi');
nw{'lon_psi'}.long_name = 'longitude of PSI-points';
nw{'lon_psi'}.units = 'degree_east';

nw{'lat_psi'} = ncdouble('eta_psi', 'xi_psi');
nw{'lat_psi'}.long_name = 'latitude of PSI-points';
nw{'lat_psi'}.units = 'degree_north';

nw{'mask_psi'} = ncdouble('eta_psi', 'xi_psi');
nw{'mask_psi'}.long_name = 'mask on PSI-points';
nw{'mask_psi'}.option_0 = 'land';
nw{'mask_psi'}.option_1 = 'water';

nw{'lon_u'} = ncdouble('eta_u', 'xi_u');
nw{'lon_u'}.long_name = 'longitude of U-points';
nw{'lon_u'}.units = 'degree_east';

nw{'lat_u'} = ncdouble('eta_u', 'xi_u');
nw{'lat_u'}.long_name = 'latitude of U-points';
nw{'lat_u'}.units = 'degree_north';

nw{'mask_u'} = ncdouble('eta_u', 'xi_u');
nw{'mask_u'}.long_name = 'mask on U-points';
nw{'mask_u'}.option_0 = 'land';
nw{'mask_u'}.option_1 = 'water';

nw{'lon_v'} = ncdouble('eta_v', 'xi_v');
nw{'lon_v'}.long_name = 'longitude of V-points';
nw{'lon_v'}.units = 'degree_east';

nw{'lat_v'} = ncdouble('eta_v', 'xi_v');
nw{'lat_v'}.long_name = 'latitude of V-points';
nw{'lat_v'}.units = 'degree_north';

nw{'mask_v'} = ncdouble('eta_v', 'xi_v');
nw{'mask_v'}.long_name = 'mask on V-points';
nw{'mask_v'}.option_0 = 'land';
nw{'mask_rho'}.option_1 = 'water';

result = endef(nw);

nw.title = title;
nw.date = date;
nw.type = 'ROMS grid produced by Easy Grid';
new_scoord = 1;
if new_scoord==1
  nw.VertCoordType = 'NEW';
end

result = close(nw);
