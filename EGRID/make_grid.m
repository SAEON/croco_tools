function make_grid(nx,ny,lon,lat,pn,pm,hraw,angle,xsize,ysize,rot,tra_lon,tra_lat,lone,late);
% 
% This is part of Easy Grid
%  (c) 2008, Jeroen Molemaker, UCLA
%
%
% Modified to create a grid as croco_tools: QB 21/05/2019
%

grdname = 'roms_grd.nc';
ROMS_title = ['ROMS grid by Easy Grid. Settings:', ...
    ' nx: '   ,num2str(nx),    ' ny: '   , num2str(ny), ...
    ' xsize: ', num2str(xsize/1e3),' ysize: ',num2str(ysize/1e3), ...
    ' rotate: ',num2str(rot),' Lon: ',num2str(tra_lon),' Lat: ',num2str(tra_lat) ];
%
nxp= nx+2;
nyp= ny+2;
%
% Create the grid file
%
create_grid(nxp,nyp,grdname,ROMS_title)
%
%
f=4*pi*sin(lat)/(24*3600);
%fmax = max(max(f));
%fmin = min(min(f));
%
% Compute the mask
%
mask_r = 0*hraw + 1;
mask_r(hraw > 0) = 0;

lon_rho = lon*180/pi;
lat_rho = lat*180/pi;
[lon_u,lon_v,lon_p]=rho2uvp(lon_rho);
[lat_u,lat_v,lat_p]=rho2uvp(lat_rho);

[mask_u,mask_v,mask_p]=uvp_mask(mask_r);


%
% Fill the grid file
%
nc=netcdf(grdname,'write');
nc{'pm'}(:)   = pm;
nc{'pn'}(:)   = pn;
nc{'angle'}(:)= angle*pi/180;
nc{'hraw'}(:) = hraw;
nc{'f'}(:)    = f;
nc{'lon_rho'}(:) = lon_rho;  %% (degrees)
nc{'lat_rho'}(:) = lat_rho;  %% (degrees)
nc{'mask_rho'}(:) = mask_r;
nc{'lon_psi'}(:) = lon_p;  %% (degrees)
nc{'lat_psi'}(:) = lat_p;  %% (degrees)
nc{'mask_psi'}(:) = mask_p;
nc{'lon_u'}(:) = lon_u;
nc{'lat_u'}(:) = lat_u;
nc{'mask_u'}(:) = mask_u;
nc{'lon_v'}(:) = lon_v;
nc{'lat_v'}(:) = lat_v;
nc{'mask_v'}(:) = mask_v;
nc{'spherical'}(:)='T';
close(nc);
%
