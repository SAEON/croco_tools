function nc_add_swradbio_blk(fname)
disp(['Add radswbio: daily averaged solar short wave radiation'])
nw=netcdf(fname,'write');
redef(nw)
%
% Add variables and attributes
%
nw{'radswbio'}= ncdouble('bulk_time', 'eta_rho', 'xi_rho');
nw{'radswbio'}.long_name = ncchar('solar shortwave radiation - no night');
nw{'radswbio'}.long_name = 'solar shortwave radiation - no night';
nw{'radswbio'}.units = ncchar('Watts meter-2');
nw{'radswbio'}.units = 'Watts meter-2';
nw{'radswbio'}.positive = ncchar('downward flux, heating');
nw{'radswbio'}.positive = 'downward flux, heating';
nw{'radswbio'}.negative = ncchar('upward flux, cooling');
nw{'radswbio'}.negative = 'upward flux, cooling';
result = endef(nw);
close(nw)