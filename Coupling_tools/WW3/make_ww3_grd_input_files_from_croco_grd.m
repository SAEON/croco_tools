%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% 
%  Create WW3 input files (bathy, mask, ...) from CROCO grid file
%
%  Further Information:  
%  http://www.croco-ocean.org
%  
%  This file is part of CROCOTOOLS
%
%  CROCOTOOLS is free software; you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published
%  by the Free Software Foundation; either version 2 of the License,
%  or (at your option) any later version.
%
%  CROCOTOOLS is distributed in the hope that it will be useful, but
%  WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program; if not, write to the Free Software
%  Foundation, Inc., 59 Temple Place, Suite 330, Boston,
%  MA  02111-1307  USA
%
%  Copyright (c) 2018 by Swen Jullien 
%  e-mail:swen.jullien@ifremer.fr  
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all
close all
%%%%%%%%%%%%%%%%%%%%% USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%%
%
run( '../CROCO/start' );
run( '../CROCO/crocotools_param' );
WW3_files_dir = [getenv('WAV_FILES_DIR') '/'];
eval(['!mkdir ',WW3_files_dir])
ww3bathyfile   = [WW3_files_dir,'bottom.inp'];
ww3lonfile     = [WW3_files_dir,'x.inp'];
ww3latfile     = [WW3_files_dir,'y.inp'];
ww3maskfile    = [WW3_files_dir,'mask.inp'];
ww3maskbdyfile = [WW3_files_dir,'mask_and_bdy.inp'];
ww3obstfile    = [WW3_files_dir,'obst.inp'];
%
%%%%%%%%%%%%%%%%%%% END USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%
warning off
isoctave=exist('octave_config_info');
%
% Open CROCO grid file and read lon, lat, mask, depth
%
disp(' ')
disp(' Read CROCO grid...')
%
nc = netcdf(grdname,'r');
lon = nc{'lon_rho'}(:);
lat = nc{'lat_rho'}(:);
mask = nc{'mask_rho'}(:);
depth = nc{'h'}(:);
close(nc);
%
% Write WW3 bathymetry file
%
disp(' ')
disp([' Write WW3 bathymetry file ',ww3bathyfile])
%
d = depth.*(-1);
[Ny,Nx] = size(d);
file = fopen(ww3bathyfile,'w');
for i = 1:Ny
 fprintf(file,' %d ',d(i,:));
 fprintf(file,'\n');
end;
fclose(file);
%
% Write WW3 longitudes file
%
disp(' ')
disp([' Write WW3 longitudes file ',ww3lonfile])
%
l = lon;
file = fopen(ww3lonfile,'w');
for i = 1:Ny
 fprintf(file,' %d ',l(i,:));
 fprintf(file,'\n');
end;
fclose(file);
%
% Write WW3 latitudes file
%
disp(' ')
disp([' Write WW3 latitudes file ',ww3latfile])
%
l = lat;
file = fopen(ww3latfile,'w');
for i = 1:Ny
 fprintf(file,' %d ',l(i,:));
 fprintf(file,'\n');
end;
fclose(file);
%
% Write WW3 obstructions file
%
disp(' ')
disp([' Write WW3 obstructions file ',ww3obstfile])
disp(' with no obstruction ')
%
ox=zeros(size(d));
oy=zeros(size(d));
file = fopen(ww3obstfile,'w');
for i = 1:Ny
 fprintf(file,' %d ',ox(i,:));
 fprintf(file,'\n');
end;
fprintf(file,'\n');
for i = 1:Ny
 fprintf(file,' %d ',oy(i,:));
 fprintf(file,'\n');
end
fclose(file);
%
% Write WW3 mask files
%
disp(' ')
disp([' Write WW3 mask file ',ww3maskfile])
disp(' As CROCO mask and without open boundary points')
%
m = mask;
file = fopen(ww3maskfile,'w');
for i = 1:Ny
 fprintf(file,' %d ',m(i,:));
 fprintf(file,'\n');
end;
fclose(file);
%
disp(' ')
disp([' Write WW3 mask file ',ww3maskbdyfile])
disp(' As CROCO mask and with open boundaries')
%
m(1,:) = 2 * mask(1,:);
m(end,:) = 2 * mask(end,:);
m(:,1) = 2 * mask(:,1);
m(:,end) = 2 * mask(:,end);
file = fopen(ww3maskbdyfile,'w');
for i = 1:Ny
 fprintf(file,' %d ',m(i,:));
 fprintf(file,'\n');
end;
fclose(file);
%
% End
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
