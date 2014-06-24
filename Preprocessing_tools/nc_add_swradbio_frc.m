function nc_add_swradbio_frc(fname)
nw=netcdf(fname,'write');
redef(nw)
%
% Add variables and attributes
%
nw{'swradbio'}= ncdouble('srf_time', 'eta_rho', 'xi_rho');
nw{'swradbio'}.long_name = ncchar('solar shortwave radiation - no night');
nw{'swradbio'}.long_name = 'solar shortwave radiation - no night';
nw{'swradbio'}.units = ncchar('Watts meter-2');
nw{'swradbio'}.units = 'Watts meter-2';
nw{'swradbio'}.positive = ncchar('downward flux, heating');
nw{'swradbio'}.positive = 'downward flux, heating';
nw{'swradbio'}.negative = ncchar('upward flux, cooling');
nw{'swradbio'}.negative = 'upward flux, cooling';
result = endef(nw);
close(nw)