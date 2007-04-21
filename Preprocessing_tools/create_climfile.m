function create_climfile(clmname,grdname,title,...
                         theta_s,theta_b,hc,N,...
                         time,cycle,clobber);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% function create_climfile(clmname,grdname,title,...
%                          theta_s,theta_b,hc,N,...
%                          time,cycle,clobber);
%
%   This function create the header of a Netcdf climatology 
%   file.
%
%   Input:
%
%   clmname      Netcdf climatology file name (character string).
%   grdname      Netcdf grid file name (character string).
%   theta_s      S-coordinate surface control parameter.(Real) 
%   theta_b      S-coordinate bottom control parameter.(Real)
%   hc           Width (m) of surface or bottom boundary layer
%                where higher vertical resolution is required
%                during stretching.(Real) 
%   N            Number of vertical levels.(Integer) 
%   time        Temperature climatology time.(vector) 
%   time        Salinity climatology time.(vector)
%   time        Velocity climatology time.(vector)
%   cycle        Length (days) for cycling the climatology.(Real)
%   clobber      Switch to allow or not writing over an existing 
%                file.(character string)
%
%   Output
%
%   nc       Output netcdf object.
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
%  Copyright (c) 2001-2006 by Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr  
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
disp(' ')
disp([' Creating the file : ',clmname])
disp(' ')
%
%  Read the grid file
%
nc = netcdf(grdname, 'nowrite');
h=nc{'h'}(:);
maskr=nc{'mask_rho'}(:);
Lp=length(nc('xi_rho'));
Mp=length(nc('eta_rho'));
status=close(nc);
hmin=min(min(h(maskr==1)));
if hc > hmin
  error([' hc (',num2str(hc),' m) > hmin (',num2str(hmin),' m)'])
end
L=Lp-1;
M=Mp-1;
Np=N+1;
%
%  Create the climatology file
%
type = 'CLIMATOLOGY file' ; 
history = 'ROMS' ;
nc = netcdf(clmname,clobber);
result = redef(nc);
%
%  Create dimensions
%
nc('xi_u') = L;
nc('xi_v') = Lp;
nc('xi_rho') = Lp;
nc('eta_u') = Mp;
nc('eta_v') = M;
nc('eta_rho') = Mp;
nc('s_rho') = N;
nc('s_w') = Np;
nc('tracer') = 2;
nc('tclm_time') = length(time);
nc('temp_time') = length(time);
nc('sclm_time') = length(time);
nc('salt_time') = length(time);
nc('uclm_time') = length(time);
nc('vclm_time') = length(time);
nc('v2d_time')  = length(time);
nc('v3d_time')  = length(time);
nc('ssh_time')  = length(time);
nc('zeta_time') = length(time);
nc('one') = 1;
%
%  Create variables
%
nc{'tstart'} = ncdouble('one') ;
nc{'tend'} = ncdouble('one') ;
nc{'theta_s'} = ncdouble('one') ;
nc{'theta_b'} = ncdouble('one') ;
nc{'Tcline'} = ncdouble('one') ;
nc{'hc'} = ncdouble('one') ;
nc{'sc_r'} = ncdouble('s_rho') ;
nc{'Cs_r'} = ncdouble('s_rho') ;
nc{'tclm_time'} = ncdouble('tclm_time') ;
nc{'temp_time'} = ncdouble('temp_time') ;
nc{'sclm_time'} = ncdouble('sclm_time') ;
nc{'salt_time'} = ncdouble('salt_time') ;
nc{'uclm_time'} = ncdouble('uclm_time') ;
nc{'vclm_time'} = ncdouble('vclm_time') ;
nc{'v2d_time'} = ncdouble('v2d_time') ;
nc{'v3d_time'} = ncdouble('v3d_time') ;
nc{'ssh_time'} = ncdouble('ssh_time') ;
nc{'zeta_time'} = ncdouble('zeta_time') ;
nc{'temp'} = ncdouble('tclm_time','s_rho','eta_rho','xi_rho') ;
nc{'salt'} = ncdouble('sclm_time','s_rho','eta_rho','xi_rho') ;
nc{'u'} = ncdouble('uclm_time','s_rho','eta_u','xi_u') ;
nc{'v'} = ncdouble('vclm_time','s_rho','eta_v','xi_v') ;
nc{'ubar'} = ncdouble('uclm_time','eta_u','xi_u') ;
nc{'vbar'} = ncdouble('vclm_time','eta_v','xi_v') ;
nc{'SSH'} = ncdouble('ssh_time','eta_rho','xi_rho') ;
nc{'zeta'} = ncdouble('zeta_time','eta_rho','xi_rho') ;
%
%  Create attributes
%
nc{'tstart'}.long_name = ncchar('start processing day');
nc{'tstart'}.long_name = 'start processing day';
nc{'tstart'}.units = ncchar('day');
nc{'tstart'}.units = 'day';
%
nc{'tend'}.long_name = ncchar('end processing day');
nc{'tend'}.long_name = 'end processing day';
nc{'tend'}.units = ncchar('day');
nc{'tend'}.units = 'day';
%
nc{'theta_s'}.long_name = ncchar('S-coordinate surface control parameter');
nc{'theta_s'}.long_name = 'S-coordinate surface control parameter';
nc{'theta_s'}.units = ncchar('nondimensional');
nc{'theta_s'}.units = 'nondimensional';
%
nc{'theta_b'}.long_name = ncchar('S-coordinate bottom control parameter');
nc{'theta_b'}.long_name = 'S-coordinate bottom control parameter';
nc{'theta_b'}.units = ncchar('nondimensional');
nc{'theta_b'}.units = 'nondimensional';
%
nc{'Tcline'}.long_name = ncchar('S-coordinate surface/bottom layer width');
nc{'Tcline'}.long_name = 'S-coordinate surface/bottom layer width';
nc{'Tcline'}.units = ncchar('meter');
nc{'Tcline'}.units = 'meter';
%
nc{'hc'}.long_name = ncchar('S-coordinate parameter, critical depth');
nc{'hc'}.long_name = 'S-coordinate parameter, critical depth';
nc{'hc'}.units = ncchar('meter');
nc{'hc'}.units = 'meter';
%
nc{'sc_r'}.long_name = ncchar('S-coordinate at RHO-points');
nc{'sc_r'}.long_name = 'S-coordinate at RHO-points';
nc{'sc_r'}.units = ncchar('nondimensional');
nc{'sc_r'}.units = 'nondimensional';
nc{'sc_r'}.valid_min = -1;
nc{'sc_r'}.valid_max = 0;
%
nc{'Cs_r'}.long_name = ncchar('S-coordinate stretching curves at RHO-points');
nc{'Cs_r'}.long_name = 'S-coordinate stretching curves at RHO-points';
nc{'Cs_r'}.units = ncchar('nondimensional');
nc{'Cs_r'}.units = 'nondimensional';
nc{'Cs_r'}.valid_min = -1;
nc{'Cs_r'}.valid_max = 0;
%
nc{'tclm_time'}.long_name = ncchar('time for temperature climatology');
nc{'tclm_time'}.long_name = 'time for temperature climatology';
nc{'tclm_time'}.units = ncchar('day');
nc{'tclm_time'}.units = 'day';
nc{'tclm_time'}.cycle_length = cycle;
%
nc{'temp_time'}.long_name = ncchar('time for temperature climatology');
nc{'temp_time'}.long_name = 'time for temperature climatology';
nc{'temp_time'}.units = ncchar('day');
nc{'temp_time'}.units = 'day';
nc{'temp_time'}.cycle_length = cycle;
%
nc{'sclm_time'}.long_name = ncchar('time for salinity climatology');
nc{'sclm_time'}.long_name = 'time for salinity climatology';
nc{'sclm_time'}.units = ncchar('day');
nc{'sclm_time'}.units = 'day';
nc{'sclm_time'}.cycle_length = cycle;
%
nc{'salt_time'}.long_name = ncchar('time for salinity climatology');
nc{'salt_time'}.long_name = 'time for salinity climatology';
nc{'salt_time'}.units = ncchar('day');
nc{'salt_time'}.units = 'day';
nc{'salt_time'}.cycle_length = cycle;
%
nc{'uclm_time'}.long_name = ncchar('time climatological u');
nc{'uclm_time'}.long_name = 'time climatological u';
nc{'uclm_time'}.units = ncchar('day');
nc{'uclm_time'}.units = 'day';
nc{'uclm_time'}.cycle_length = cycle;
%
nc{'vclm_time'}.long_name = ncchar('time climatological v');
nc{'vclm_time'}.long_name = 'time climatological v';
nc{'vclm_time'}.units = ncchar('day');
nc{'vclm_time'}.units = 'day';
nc{'vclm_time'}.cycle_length = cycle;
%
nc{'v2d_time'}.long_name = ncchar('time for 2D velocity climatology');
nc{'v2d_time'}.long_name = 'time for 2D velocity climatology';
nc{'v2d_time'}.units = ncchar('day');
nc{'v2d_time'}.units = 'day';
nc{'v2d_time'}.cycle_length = cycle;
%
nc{'v3d_time'}.long_name = ncchar('time for 3D velocity climatology');
nc{'v3d_time'}.long_name = 'time for 3D velocity climatology';
nc{'v3d_time'}.units = ncchar('day');
nc{'v3d_time'}.units = 'day';
nc{'v3d_time'}.cycle_length = cycle;
%
nc{'ssh_time'}.long_name = ncchar('time for sea surface height');
nc{'ssh_time'}.long_name = 'time for sea surface height';
nc{'ssh_time'}.units = ncchar('day');
nc{'ssh_time'}.units = 'day';
nc{'ssh_time'}.cycle_length = cycle;
%
nc{'zeta_time'}.long_name = ncchar('time for sea surface height');
nc{'zeta_time'}.long_name = 'time for sea surface height';
nc{'zeta_time'}.units = ncchar('day');
nc{'zeta_time'}.units = 'day';
nc{'zeta_time'}.cycle_length = cycle;
%
nc{'temp'}.long_name = ncchar('potential temperature');
nc{'temp'}.long_name = 'potential temperature';
nc{'temp'}.units = ncchar('Celsius');
nc{'temp'}.units = 'Celsius';
%
nc{'salt'}.long_name = ncchar('salinity');
nc{'salt'}.long_name = 'salinity';
nc{'salt'}.units = ncchar('PSU');
nc{'salt'}.units = 'PSU';
%
nc{'u'}.long_name = ncchar('u-momentum component');
nc{'u'}.long_name = 'u-momentum component';
nc{'u'}.units = ncchar('meter second-1');
nc{'u'}.units = 'meter second-1';
%
nc{'v'}.long_name = ncchar('v-momentum component');
nc{'v'}.long_name = 'v-momentum component';
nc{'v'}.units = ncchar('meter second-1');
nc{'v'}.units = 'meter second-1';
%
nc{'ubar'}.long_name = ncchar('vertically integrated u-momentum component');
nc{'ubar'}.long_name = 'vertically integrated u-momentum component';
nc{'ubar'}.units = ncchar('meter second-1');
nc{'ubar'}.units = 'meter second-1';
%
nc{'vbar'}.long_name = ncchar('vertically integrated v-momentum component');
nc{'vbar'}.long_name = 'vertically integrated v-momentum component';
nc{'vbar'}.units = ncchar('meter second-1');
nc{'vbar'}.units = 'meter second-1';
%
nc{'SSH'}.long_name = ncchar('sea surface height');
nc{'SSH'}.long_name = 'sea surface height';
nc{'SSH'}.units = ncchar('meter');
nc{'SSH'}.units = 'meter';
%
nc{'zeta'}.long_name = ncchar('sea surface height');
nc{'zeta'}.long_name = 'sea surface height';
nc{'zeta'}.units = ncchar('meter');
nc{'zeta'}.units = 'meter';
%
% Create global attributes
%
nc.title = ncchar(title);
nc.title = title;
nc.date = ncchar(date);
nc.date = date;
nc.clim_file = ncchar(clmname);
nc.clim_file = clmname;
nc.grd_file = ncchar(grdname);
nc.grd_file = grdname;
nc.type = ncchar(type);
nc.type = type;
nc.history = ncchar(history);
nc.history = history;
%
% Leave define mode
%
result = endef(nc);
%
% Compute S coordinates
%
cff1=1./sinh(theta_s);
cff2=0.5/tanh(0.5*theta_s);
sc=((1:N)-N-0.5)/N;
Cs=(1.-theta_b)*cff1*sinh(theta_s*sc)...
    +theta_b*(cff2*tanh(theta_s*(sc+0.5))-0.5);
%
% Write variables
%
nc{'tstart'}(:) =  min([min(time) min(time) min(time)]); 
nc{'tend'}(:) =  max([max(time) max(time) max(time)]); 
nc{'theta_s'}(:) =  theta_s; 
nc{'theta_b'}(:) =  theta_b; 
nc{'Tcline'}(:) =  hc; 
nc{'hc'}(:) =  hc; 
nc{'sc_r'}(:) =  sc; 
nc{'Cs_r'}(:) =  Cs; 
nc{'tclm_time'}(:) =  time; 
nc{'temp_time'}(:) =  time; 
nc{'sclm_time'}(:) =  time; 
nc{'salt_time'}(:) =  time; 
nc{'uclm_time'}(:) =  time; 
nc{'vclm_time'}(:) =  time; 
nc{'v2d_time'}(:) =   time; 
nc{'v3d_time'}(:) =   time; 
nc{'ssh_time'}(:) =   time;
nc{'zeta_time'}(:) =  time;
nc{'u'}(:) =  0; 
nc{'v'}(:) =  0; 
nc{'ubar'}(:) =  0; 
nc{'vbar'}(:) =  0; 
nc{'SSH'}(:) =  0; 
nc{'zeta'}(:) =  0; 
nc{'temp'}(:) =  0; 
nc{'salt'}(:) =  0; 
close(nc)
return


