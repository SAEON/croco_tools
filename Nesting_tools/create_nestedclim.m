function ncclim=create_nestedclim(climfile,gridfile,parentfile,title,...
                                theta_s,theta_b,Tcline,N,...
                                ttime,stime,utime,vtime,sshtime,...
                                tcycle,scycle,ucycle,vcycle,sshcycle,clobber)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  function ncclim=create_climfile(climfile,gridfile,theta_s,...
%                  theta_b,Tcline,N,...
%            ttime,stime,utime,vtime,sshtime,...
%            tcycle,scycle,ucycle,vcycle,sshcycle,...
%                  clobber) 
%
%   This function create the header of a Netcdf climatology 
%   file.
%
%   Input: 
%
%   climfile     Netcdf climatology file name (character string)
%   gridfile     Netcdf grid file name (character string).
%   theta_s      S-coordinate surface control parameter.(Real)
%   theta_b      S-coordinate bottom control parameter.(Real)
%   Tcline       Width (m) of surface or bottom boundary layer
%                where higher vertical resolution is required 
%                during stretching.(Real)
%   N            Number of vertical levels.(Integer)
%   ttime        Temperature climatology time.(vector) 
%   stime        Salinity climatology time.(vector)
%   utime        Velocity climatology time.(vector)
%   cycle        Length (days) for cycling the climatology.(Real)
%   clobber      Switch to allow or not writing over an existing
%                file.(character string)
%
%   Output
%
%   ncclim       Output netcdf object.
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
%  Copyright (c) 2004-2006 by Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr  
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
disp(' ')
disp(' ')
disp(['Creating the file : ',climfile])
disp(' ')
%
%  Read the grid file
%
nc = netcdf(gridfile, 'nowrite');
h = nc{'h'}(:);
close(nc);
[Mp,Lp]=size(h);
L=Lp-1;
M=Mp-1;
Np=N+1;
%
%  Create the climatology file
%
type = 'CLIMATOLOGY file' ; 
history = 'ROMS' ;
ncclim = netcdf(climfile,clobber);
result = redef(ncclim);
%
%  Create dimensions
%
ncclim('xi_u') = L;
ncclim('xi_v') = Lp;
ncclim('xi_rho') = Lp;
ncclim('eta_u') = Mp;
ncclim('eta_v') = M;
ncclim('eta_rho') = Mp;
ncclim('s_rho') = N;
ncclim('s_w') = Np;
ncclim('tracer') = 2;
ncclim('tclm_time') = size(ttime);
ncclim('sclm_time') = size(stime);
ncclim('uclm_time') = size(utime);
ncclim('vclm_time') = size(vtime);
ncclim('ssh_time') = size(sshtime);
ncclim('one') = 1;
%
%  Create variables
%
ncclim{'tstart'} = ncdouble('one') ;
ncclim{'tend'} = ncdouble('one') ;
ncclim{'theta_s'} = ncdouble('one') ;
ncclim{'theta_b'} = ncdouble('one') ;
ncclim{'Tcline'} = ncdouble('one') ;
ncclim{'hc'} = ncdouble('one') ;
ncclim{'sc_r'} = ncdouble('s_rho') ;
ncclim{'Cs_r'} = ncdouble('s_rho') ;
ncclim{'tclm_time'} = ncdouble('tclm_time') ;
ncclim{'sclm_time'} = ncdouble('sclm_time') ;
ncclim{'uclm_time'} = ncdouble('uclm_time') ;
ncclim{'vclm_time'} = ncdouble('vclm_time') ;
ncclim{'ssh_time'} = ncdouble('ssh_time') ;
ncclim{'temp'} = ncdouble('tclm_time','s_rho','eta_rho','xi_rho') ;
ncclim{'salt'} = ncdouble('sclm_time','s_rho','eta_rho','xi_rho') ;
ncclim{'u'} = ncdouble('uclm_time','s_rho','eta_u','xi_u') ;
ncclim{'v'} = ncdouble('vclm_time','s_rho','eta_v','xi_v') ;
ncclim{'ubar'} = ncdouble('uclm_time','eta_u','xi_u') ;
ncclim{'vbar'} = ncdouble('vclm_time','eta_v','xi_v') ;
ncclim{'SSH'} = ncdouble('ssh_time','eta_rho','xi_rho') ;
%
%  Create attributes
%
ncclim{'tstart'}.long_name = ncchar('start processing day');
ncclim{'tstart'}.long_name = 'start processing day';
ncclim{'tstart'}.units = ncchar('day');
ncclim{'tstart'}.units = 'day';
%
ncclim{'tend'}.long_name = ncchar('end processing day');
ncclim{'tend'}.long_name = 'end processing day';
ncclim{'tend'}.units = ncchar('day');
ncclim{'tend'}.units = 'day';
%
ncclim{'theta_s'}.long_name = ncchar('S-coordinate surface control parameter');
ncclim{'theta_s'}.long_name = 'S-coordinate surface control parameter';
ncclim{'theta_s'}.units = ncchar('nondimensional');
ncclim{'theta_s'}.units = 'nondimensional';
%
ncclim{'theta_b'}.long_name = ncchar('S-coordinate bottom control parameter');
ncclim{'theta_b'}.long_name = 'S-coordinate bottom control parameter';
ncclim{'theta_b'}.units = ncchar('nondimensional');
ncclim{'theta_b'}.units = 'nondimensional';
%
ncclim{'Tcline'}.long_name = ncchar('S-coordinate surface/bottom layer width');
ncclim{'Tcline'}.long_name = 'S-coordinate surface/bottom layer width';
ncclim{'Tcline'}.units = ncchar('meter');
ncclim{'Tcline'}.units = 'meter';
%
ncclim{'hc'}.long_name = ncchar('S-coordinate parameter, critical depth');
ncclim{'hc'}.long_name = 'S-coordinate parameter, critical depth';
ncclim{'hc'}.units = ncchar('meter');
ncclim{'hc'}.units = 'meter';
%
ncclim{'sc_r'}.long_name = ncchar('S-coordinate at RHO-points');
ncclim{'sc_r'}.long_name = 'S-coordinate at RHO-points';
ncclim{'sc_r'}.units = ncchar('nondimensional');
ncclim{'sc_r'}.units = 'nondimensional';
ncclim{'sc_r'}.valid_min = -1;
ncclim{'sc_r'}.valid_max = 0;
ncclim{'sc_r'}.field = ncchar('sc_r, scalar');
ncclim{'sc_r'}.field = 'sc_r, scalar';
%
ncclim{'Cs_r'}.long_name = ncchar('S-coordinate stretching curves at RHO-points');
ncclim{'Cs_r'}.long_name = 'S-coordinate stretching curves at RHO-points';
ncclim{'Cs_r'}.units = ncchar('nondimensional');
ncclim{'Cs_r'}.units = 'nondimensional';
ncclim{'Cs_r'}.valid_min = -1;
ncclim{'Cs_r'}.valid_max = 0;
ncclim{'Cs_r'}.field = ncchar('Cs_r, scalar');
ncclim{'Cs_r'}.field = 'Cs_r, scalar';
%
ncclim{'tclm_time'}.long_name = ncchar('time for temperature climatology');
ncclim{'tclm_time'}.long_name = 'time for temperature climatology';
ncclim{'tclm_time'}.units = ncchar('day');
ncclim{'tclm_time'}.units = 'day';
ncclim{'tclm_time'}.cycle_length = tcycle;
ncclim{'tclm_time'}.field = ncchar('tclm_time, scalar, series');
ncclim{'tclm_time'}.field = 'tclm_time, scalar, series'  ;
%
ncclim{'sclm_time'}.long_name = ncchar('time for salinity climatology');
ncclim{'sclm_time'}.long_name = 'time for salinity climatology';
ncclim{'sclm_time'}.units = ncchar('day');
ncclim{'sclm_time'}.units = 'day';
ncclim{'sclm_time'}.cycle_length = scycle;
ncclim{'sclm_time'}.field = ncchar('sclm_time, scalar, serie');
ncclim{'sclm_time'}.field = 'sclm_time, scalar, serie';
%
ncclim{'uclm_time'}.long_name = ncchar('time climatological u');
ncclim{'uclm_time'}.long_name = 'time climatological u';
ncclim{'uclm_time'}.units = ncchar('day');
ncclim{'uclm_time'}.units = 'day';
ncclim{'uclm_time'}.cycle_length = ucycle;
ncclim{'uclm_time'}.field = ncchar('uclm_time, scalar, serie');
ncclim{'uclm_time'}.field = 'uclm_time, scalar, serie';
%
ncclim{'vclm_time'}.long_name = ncchar('time climatological v');
ncclim{'vclm_time'}.long_name = 'time climatological v';
ncclim{'vclm_time'}.units = ncchar('day');
ncclim{'vclm_time'}.units = 'day';
ncclim{'vclm_time'}.cycle_length = vcycle;
ncclim{'vclm_time'}.field = ncchar('vclm_time, scalar, serie');
ncclim{'vclm_time'}.field = 'vclm_time, scalar, serie';
%
ncclim{'ssh_time'}.long_name = ncchar('time for sea surface height');
ncclim{'ssh_time'}.long_name = 'time for sea surface height';
ncclim{'ssh_time'}.units = ncchar('day');
ncclim{'ssh_time'}.units = 'day';
ncclim{'ssh_time'}.cycle_length = sshcycle;
ncclim{'ssh_time'}.field = ncchar('ssh_time, scalar, serie');
ncclim{'ssh_time'}.field = 'ssh_time, scalar, serie';
%
ncclim{'temp'}.long_name = ncchar('potential temperature');
ncclim{'temp'}.long_name = 'potential temperature';
ncclim{'temp'}.units = ncchar('Celsius');
ncclim{'temp'}.units = 'Celsius';
ncclim{'temp'}.field = ncchar('temperature, scalar, series');
ncclim{'temp'}.field = 'temperature, scalar, series';
%
ncclim{'salt'}.long_name = ncchar('salinity');
ncclim{'salt'}.long_name = 'salinity';
ncclim{'salt'}.units = ncchar('PSU');
ncclim{'salt'}.units = 'PSU';
ncclim{'salt'}.field = ncchar('salinity, scalar, series');
ncclim{'salt'}.field = 'salinity, scalar, series';
%
ncclim{'u'}.long_name = ncchar('u-momentum component');
ncclim{'u'}.long_name = 'u-momentum component';
ncclim{'u'}.units = ncchar('meter second-1');
ncclim{'u'}.units = 'meter second-1';
ncclim{'u'}.field = ncchar('u-velocity, scalar, series');
ncclim{'u'}.field = 'u-velocity, scalar, series';
%
ncclim{'v'}.long_name = ncchar('v-momentum component');
ncclim{'v'}.long_name = 'v-momentum component';
ncclim{'v'}.units = ncchar('meter second-1');
ncclim{'v'}.units = 'meter second-1';
ncclim{'v'}.field = ncchar('v-velocity, scalar, series');
ncclim{'v'}.field = 'v-velocity, scalar, series';
%
ncclim{'ubar'}.long_name = ncchar('vertically integrated u-momentum component');
ncclim{'ubar'}.long_name = 'vertically integrated u-momentum component';
ncclim{'ubar'}.units = ncchar('meter second-1');
ncclim{'ubar'}.units = 'meter second-1';
ncclim{'ubar'}.field = ncchar('ubar-velocity, scalar, series');
ncclim{'ubar'}.field = 'ubar-velocity, scalar, series';
%
ncclim{'vbar'}.long_name = ncchar('vertically integrated v-momentum component');
ncclim{'vbar'}.long_name = 'vertically integrated v-momentum component';
ncclim{'vbar'}.units = ncchar('meter second-1');
ncclim{'vbar'}.units = 'meter second-1';
ncclim{'vbar'}.field = ncchar('vbar-velocity, scalar, series');
ncclim{'vbar'}.field = 'vbar-velocity, scalar, series';
%
ncclim{'SSH'}.long_name = ncchar('sea surface height');
ncclim{'SSH'}.long_name = 'sea surface height';
ncclim{'SSH'}.units = ncchar('meter');
ncclim{'SSH'}.units = 'meter';
ncclim{'SSH'}.field = ncchar('SSH, scalar, series');
ncclim{'SSH'}.field = 'SSH, scalar, series';
%
% Create global attributes
%
ncclim.title = ncchar(title);
ncclim.title = title;
ncclim.date = ncchar(date);
ncclim.date = date;
ncclim.clim_file = ncchar(climfile);
ncclim.clim_file = climfile;
ncclim.grd_file = ncchar(gridfile);
ncclim.grd_file = gridfile;
ncclim.parent_file = ncchar(parentfile);
ncclim.parent_file = parentfile;
ncclim.type = ncchar(type);
ncclim.type = type;
ncclim.history = ncchar(history);
ncclim.history = history;
%
% Leave define mode
%
result = endef(ncclim);
%
% Compute S coordinates
%
ds=1.0/N;
hmin=min(min(h));
hc=min(hmin,Tcline);
lev=1:N;
sc=-1+(lev-0.5).*ds;
Ptheta=sinh(theta_s.*sc)./sinh(theta_s);
Rtheta=tanh(theta_s.*(sc+0.5))./(2*tanh(0.5*theta_s))-0.5;
Cs=(1-theta_b).*Ptheta+theta_b.*Rtheta;
%
% Write variables
%
ncclim{'tstart'}(:) =  min([min(ttime) min(stime) min(utime)]); 
ncclim{'tend'}(:) =  max([max(ttime) max(stime) max(utime)]); 
ncclim{'theta_s'}(:) =  theta_s; 
ncclim{'theta_b'}(:) =  theta_b; 
ncclim{'Tcline'}(:) =  Tcline; 
ncclim{'hc'}(:) =  hc; 
ncclim{'sc_r'}(:) =  sc; 
ncclim{'Cs_r'}(:) =  Cs; 
ncclim{'tclm_time'}(:) =  ttime; 
ncclim{'sclm_time'}(:) =  stime; 
ncclim{'uclm_time'}(:) = utime ; 
ncclim{'vclm_time'}(:) = vtime ; 
ncclim{'ssh_time'}(:) = sshtime;
ncclim{'u'}(:) =  0; 
ncclim{'v'}(:) =  0; 
ncclim{'ubar'}(:) =  0; 
ncclim{'vbar'}(:) =  0; 
ncclim{'SSH'}(:) =  0; 
ncclim{'temp'}(:) =  0; 
ncclim{'salt'}(:) =  0; 
%
% Synchronize on disk
%
sync(ncclim);
return


