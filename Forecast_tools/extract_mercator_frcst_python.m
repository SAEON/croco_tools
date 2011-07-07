function extract_mercator_frcst_python(FRCST_dir,FRCST_prefix,url,...
                      time,Yorig)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Extract a subset from Marcator using python motu client (cls)
% Write it in a local file (keeping the classic SODA netcdf format)
% 
%  Further Information:  
%  http://www.brest.ird.fr/Roms_tools/
%  
%  This file is part of ROMSTOOLS
%
%  ROMSTOOLS is free software; you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published
%  by the Free Software Foundation; either version 2 of the License,
%  or (at your option) any later version.
%
%  ROMSTOOLS is distributed in the hope that it will be useful, but
%  WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program; if not, write to the Free Software
%  Foundation, Inc., 59 Temple Place, Suite 330, Boston,
%  MA  02111-1307  USA
%
%  Copyright (c) 2006 by Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr  
%
%  Updated    9-Sep-2006 by Pierrick Penven
%  Updated    19-May-2011 by Andres Sepulveda & Gildas Cambon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
disp(['    Download MERCATOR'])
%
nc = netcdf(url);
lon = nc{'longitude'}(:);
lat = nc{'latitude'}(:);
depth = nc{'depth'}(:);
time = nc{'time'}(:);
time = time / 24 + datenum(1950,1,1) - datenum(Yorig,1,1);

% Get SSH
%
%missval = -32767;
disp('    ...SSH')
%vname='sea_surface_height_above_geoid';
vname='ssh';
ncc=nc{vname};
ssh=ncc(:);
missval=ncc.FillValue_(:);
scale_factor=ncc.scale_factor(:);
add_offset=ncc.add_offset(:);
ssh(ssh<=missval)=NaN;
ssh = ssh.*scale_factor + add_offset;
%
%
% Get U
%
disp('    ...U')
%vname='sea_water_x_velocity';
vname='u';
ncc=nc{vname};
u=ncc(:);
missval=ncc.FillValue_(:);
scale_factor=ncc.scale_factor(:);
add_offset=ncc.add_offset(:);
u(u<=missval)=NaN;
u = u.*scale_factor + add_offset;
%
% Get V
%
disp('    ...V')
%vname='sea_water_y_velocity';
vname='v';
ncc=nc{vname};
v=ncc(:);
missval=ncc.FillValue_(:);
scale_factor=ncc.scale_factor(:);
add_offset=ncc.add_offset(:);
v(v<=missval)=NaN;
v = v.*scale_factor + add_offset;
%
% Get TEMP
%
disp('    ...TEMP')
%vname='sea_water_potential_temperature';
vname='temperature';
ncc=nc{vname};
temp=ncc(:);
missval=ncc.FillValue_(:);
scale_factor=ncc.scale_factor(:);
add_offset=ncc.add_offset(:);
ktoc=272.15;

temp(temp<=missval)=NaN;
temp = temp.*scale_factor + add_offset - ktoc;
%
% Get SALT
%
disp('    ...SALT')
%vname='sea_water_salinity';
vname='salinity';
ncc=nc{vname};
salt=ncc(:);
missval=ncc.FillValue_(:);
scale_factor=ncc.scale_factor(:);
add_offset=ncc.add_offset(:);
salt(salt<=missval)=NaN;
salt = salt.*scale_factor + add_offset;
%
%
% Create the Mercator file
rundate_str=date;
rundate=datenum(rundate_str)-datenum(Yorig,1,1);
close(nc)

create_OGCM([FRCST_dir,FRCST_prefix,num2str(rundate),'.cdf'],...
             lon,lat,lon-0.5,lat,lon,lat-0.5,depth,time,...
             squeeze(temp),squeeze(salt),squeeze(u),...
             squeeze(v),squeeze(ssh),Yorig)
%
return
