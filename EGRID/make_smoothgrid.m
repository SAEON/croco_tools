grd_name = '/net/libra/local/tmp/1/cvic/ROMS/Simulations/WEBUS/webus_grd.nc';
hmin    = 15.;
hmax_coast = 3000.;
rmax    = 0.2;
n_filter_deep_topo  = 4;
n_filter_final      = 2;



nc = netcdf.open(grd_name,'nowrite');

hrawid = netcdf.inqVarID(nc,'hraw');
hraw   = netcdf.getVar(nc,hrawid);

mskid = netcdf.inqVarID(nc,'mask_rho');
maskr = netcdf.getVar(nc,mskid);

h = smoothgrid(hraw,maskr,hmin,hmax_coast,rmax,n_filter_deep_topo,n_filter_final);

%hid = netcdf.inqVarID(nc,'h');
%netcdf.putVar(nc,hid,h);

netcdf.close(nc);


ncwrite(grd_name,'h',h);

