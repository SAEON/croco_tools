function create_bryfile(bryname,grdname,title,obc,...
                        theta_s,theta_b,hc,N,...
                        time,cycle,clobber);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% function create_bryfile(bryname,grdname,title,obc...
%                          theta_s,theta_b,hc,N,...
%                          time,cycle,clobber);
%
%   This function create the header of a Netcdf climatology 
%   file.
%
%   Input:
%
%   bryname      Netcdf climatology file name (character string).
%   grdname      Netcdf grid file name (character string).
%   obc          open boundaries flag (1=open , [S E N W]).
%   theta_s      S-coordinate surface control parameter.(Real)
%   theta_b      S-coordinate bottom control parameter.(Real)
%   hc           Width (m) of surface or bottom boundary layer 
%                where higher vertical resolution is required 
%                during stretching.(Real)
%   N            Number of vertical levels.(Integer)
%   time         time.(vector)
%   cycle        Length (days) for cycling the climatology.(Real)
%   clobber      Switch to allow or not writing over an existing
%                file.(character string)
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
disp([' Creating the file : ',bryname])
disp(' ')
%
%  Read the grid file and check the topography
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
%  Create the boundary file
%
type = 'BOUNDARY file' ; 
history = 'ROMS' ;
nc = netcdf(bryname,clobber);
result = redef(nc);
%
%  Create dimensions
%
nc('xi_u') = L;
nc('xi_rho') = Lp;
nc('eta_v') = M;
nc('eta_rho') = Mp;
nc('s_rho') = N;
nc('bry_time') = length(time);
nc('one') = 1;
%
%  Create variables and attributes
%
nc{'theta_s'} = ncdouble('one') ;
nc{'theta_s'}.long_name = ncchar('S-coordinate surface control parameter');
nc{'theta_s'}.long_name = 'S-coordinate surface control parameter';
nc{'theta_s'}.units = ncchar('nondimensional');
nc{'theta_s'}.units = 'nondimensional';
%
nc{'theta_b'} = ncdouble('one') ;
nc{'theta_b'}.long_name = ncchar('S-coordinate bottom control parameter');
nc{'theta_b'}.long_name = 'S-coordinate bottom control parameter';
nc{'theta_b'}.units = ncchar('nondimensional');
nc{'theta_b'}.units = 'nondimensional';
%
nc{'Tcline'} = ncdouble('one') ;
nc{'Tcline'}.long_name = ncchar('S-coordinate surface/bottom layer width');
nc{'Tcline'}.long_name = 'S-coordinate surface/bottom layer width';
nc{'Tcline'}.units = ncchar('meter');
nc{'Tcline'}.units = 'meter';
%
nc{'hc'} = ncdouble('one') ;
nc{'hc'}.long_name = ncchar('S-coordinate parameter, critical depth');
nc{'hc'}.long_name = 'S-coordinate parameter, critical depth';
nc{'hc'}.units = ncchar('meter');
nc{'hc'}.units = 'meter';
%
nc{'sc_r'} = ncdouble('s_rho') ;
nc{'sc_r'}.long_name = ncchar('S-coordinate at RHO-points');
nc{'sc_r'}.long_name = 'S-coordinate at RHO-points';
nc{'sc_r'}.units = ncchar('nondimensional');
nc{'sc_r'}.units = 'nondimensional';
nc{'sc_r'}.valid_min = -1;
nc{'sc_r'}.valid_max = 0;
%
nc{'Cs_r'} = ncdouble('s_rho') ;
nc{'Cs_r'}.long_name = ncchar('S-coordinate stretching curves at RHO-points');
nc{'Cs_r'}.long_name = 'S-coordinate stretching curves at RHO-points';
nc{'Cs_r'}.units = ncchar('nondimensional');
nc{'Cs_r'}.units = 'nondimensional';
nc{'Cs_r'}.valid_min = -1;
nc{'Cs_r'}.valid_max = 0;
%
nc{'bry_time'} = ncdouble('bry_time') ;
nc{'bry_time'}.long_name = ncchar('time for temperature climatology');
nc{'bry_time'}.long_name = 'time for temperature climatology';
nc{'bry_time'}.units = ncchar('day');
nc{'bry_time'}.units = 'day';
nc{'bry_time'}.cycle_length = cycle;%
%
if obc(1)==1
%
%   Southern boundary
%
  nc{'temp_south'} = ncdouble('bry_time','s_rho','xi_rho') ;
  nc{'temp_south'}.long_name = ncchar('southern boundary potential temperature');
  nc{'temp_south'}.long_name = 'southern boundary potential temperature';
  nc{'temp_south'}.units = ncchar('Celsius');
  nc{'temp_south'}.units = 'Celsius';
%
  nc{'salt_south'} = ncdouble('bry_time','s_rho','xi_rho') ;
  nc{'salt_south'}.long_name = ncchar('southern boundary salinity');
  nc{'salt_south'}.long_name = 'southern boundary salinity';
  nc{'salt_south'}.units = ncchar('PSU');
  nc{'salt_south'}.units = 'PSU';
%
  nc{'u_south'} = ncdouble('bry_time','s_rho','xi_u') ;
  nc{'u_south'}.long_name = ncchar('southern boundary u-momentum component');
  nc{'u_south'}.long_name = 'southern boundary u-momentum component';
  nc{'u_south'}.units = ncchar('meter second-1');
  nc{'u_south'}.units = 'meter second-1';
%
  nc{'v_south'} = ncdouble('bry_time','s_rho','xi_rho') ;
  nc{'v_south'}.long_name = ncchar('southern boundary v-momentum component');
  nc{'v_south'}.long_name = 'southern boundary v-momentum component';
  nc{'v_south'}.units = ncchar('meter second-1');
  nc{'v_south'}.units = 'meter second-1';
%
  nc{'ubar_south'} = ncdouble('bry_time','xi_u') ;
  nc{'ubar_south'}.long_name = ncchar('southern boundary vertically integrated u-momentum component');
  nc{'ubar_south'}.long_name = 'southern boundary vertically integrated u-momentum component';
  nc{'ubar_south'}.units = ncchar('meter second-1');
  nc{'ubar_south'}.units = 'meter second-1';
%
  nc{'vbar_south'} = ncdouble('bry_time','xi_rho') ;
  nc{'vbar_south'}.long_name = ncchar('southern boundary vertically integrated v-momentum component');
  nc{'vbar_south'}.long_name = 'southern boundary vertically integrated v-momentum component';
  nc{'vbar_south'}.units = ncchar('meter second-1');
  nc{'vbar_south'}.units = 'meter second-1';
%
  nc{'zeta_south'} = ncdouble('bry_time','xi_rho') ;
  nc{'zeta_south'}.long_name = ncchar('southern boundary sea surface height');
  nc{'zeta_south'}.long_name = 'southern boundary sea surface height';
  nc{'zeta_south'}.units = ncchar('meter');
  nc{'zeta_south'}.units = 'meter';
%
end
%
if obc(2)==1
%
%   Eastern boundary
%
  nc{'temp_east'} = ncdouble('bry_time','s_rho','eta_rho') ;
  nc{'temp_east'}.long_name = ncchar('eastern boundary potential temperature');
  nc{'temp_east'}.long_name = 'eastern boundary potential temperature';
  nc{'temp_east'}.units = ncchar('Celsius');
  nc{'temp_east'}.units = 'Celsius';
%
  nc{'salt_east'} = ncdouble('bry_time','s_rho','eta_rho') ;
  nc{'salt_east'}.long_name = ncchar('eastern boundary salinity');
  nc{'salt_east'}.long_name = 'eastern boundary salinity';
  nc{'salt_east'}.units = ncchar('PSU');
  nc{'salt_east'}.units = 'PSU';
%
  nc{'u_east'} = ncdouble('bry_time','s_rho','eta_rho') ;
  nc{'u_east'}.long_name = ncchar('eastern boundary u-momentum component');
  nc{'u_east'}.long_name = 'eastern boundary u-momentum component';
  nc{'u_east'}.units = ncchar('meter second-1');
  nc{'u_east'}.units = 'meter second-1';
%
  nc{'v_east'} = ncdouble('bry_time','s_rho','eta_v') ;
  nc{'v_east'}.long_name = ncchar('eastern boundary v-momentum component');
  nc{'v_east'}.long_name = 'eastern boundary v-momentum component';
  nc{'v_east'}.units = ncchar('meter second-1');
  nc{'v_east'}.units = 'meter second-1';
%
  nc{'ubar_east'} = ncdouble('bry_time','eta_rho') ;
  nc{'ubar_east'}.long_name = ncchar('eastern boundary vertically integrated u-momentum component');
  nc{'ubar_east'}.long_name = 'eastern boundary vertically integrated u-momentum component';
  nc{'ubar_east'}.units = ncchar('meter second-1');
  nc{'ubar_east'}.units = 'meter second-1';
%
  nc{'vbar_east'} = ncdouble('bry_time','eta_v') ;
  nc{'vbar_east'}.long_name = ncchar('eastern boundary vertically integrated v-momentum component');
  nc{'vbar_east'}.long_name = 'eastern boundary vertically integrated v-momentum component';
  nc{'vbar_east'}.units = ncchar('meter second-1');
  nc{'vbar_east'}.units = 'meter second-1';
%
  nc{'zeta_east'} = ncdouble('bry_time','eta_rho') ;
  nc{'zeta_east'}.long_name = ncchar('eastern boundary sea surface height');
  nc{'zeta_east'}.long_name = 'eastern boundary sea surface height';
  nc{'zeta_east'}.units = ncchar('meter');
  nc{'zeta_east'}.units = 'meter';
%
end
%
if obc(3)==1
%
%   Northern boundary
%
  nc{'temp_north'} = ncdouble('bry_time','s_rho','xi_rho') ;
  nc{'temp_north'}.long_name = ncchar('northern boundary potential temperature');
  nc{'temp_north'}.long_name = 'northern boundary potential temperature';
  nc{'temp_north'}.units = ncchar('Celsius');
  nc{'temp_north'}.units = 'Celsius';
%
  nc{'salt_north'} = ncdouble('bry_time','s_rho','xi_rho') ;
  nc{'salt_north'}.long_name = ncchar('northern boundary salinity');
  nc{'salt_north'}.long_name = 'northern boundary salinity';
  nc{'salt_north'}.units = ncchar('PSU');
  nc{'salt_north'}.units = 'PSU';
%
  nc{'u_north'} = ncdouble('bry_time','s_rho','xi_u') ;
  nc{'u_north'}.long_name = ncchar('northern boundary u-momentum component');
  nc{'u_north'}.long_name = 'northern boundary u-momentum component';
  nc{'u_north'}.units = ncchar('meter second-1');
  nc{'u_north'}.units = 'meter second-1';
%
  nc{'v_north'} = ncdouble('bry_time','s_rho','xi_rho') ;
  nc{'v_north'}.long_name = ncchar('northern boundary v-momentum component');
  nc{'v_north'}.long_name = 'northern boundary v-momentum component';
  nc{'v_north'}.units = ncchar('meter second-1');
  nc{'v_north'}.units = 'meter second-1';
%
  nc{'ubar_north'} = ncdouble('bry_time','xi_u') ;
  nc{'ubar_north'}.long_name = ncchar('northern boundary vertically integrated u-momentum component');
  nc{'ubar_north'}.long_name = 'northern boundary vertically integrated u-momentum component';
  nc{'ubar_north'}.units = ncchar('meter second-1');
  nc{'ubar_north'}.units = 'meter second-1';
%
  nc{'vbar_north'} = ncdouble('bry_time','xi_rho') ;
  nc{'vbar_north'}.long_name = ncchar('northern boundary vertically integrated v-momentum component');
  nc{'vbar_north'}.long_name = 'northern boundary vertically integrated v-momentum component';
  nc{'vbar_north'}.units = ncchar('meter second-1');
  nc{'vbar_north'}.units = 'meter second-1';
%
  nc{'zeta_north'} = ncdouble('bry_time','xi_rho') ;
  nc{'zeta_north'}.long_name = ncchar('northern boundary sea surface height');
  nc{'zeta_north'}.long_name = 'northern boundary sea surface height';
  nc{'zeta_north'}.units = ncchar('meter');
  nc{'zeta_north'}.units = 'meter';
%
end
%
if obc(4)==1
%
%   Western boundary
%
  nc{'temp_west'} = ncdouble('bry_time','s_rho','eta_rho') ;
  nc{'temp_west'}.long_name = ncchar('western boundary potential temperature');
  nc{'temp_west'}.long_name = 'western boundary potential temperature';
  nc{'temp_west'}.units = ncchar('Celsius');
  nc{'temp_west'}.units = 'Celsius';
%
  nc{'salt_west'} = ncdouble('bry_time','s_rho','eta_rho') ;
  nc{'salt_west'}.long_name = ncchar('western boundary salinity');
  nc{'salt_west'}.long_name = 'western boundary salinity';
  nc{'salt_west'}.units = ncchar('PSU');
  nc{'salt_west'}.units = 'PSU';
%
  nc{'u_west'} = ncdouble('bry_time','s_rho','eta_rho') ;
  nc{'u_west'}.long_name = ncchar('western boundary u-momentum component');
  nc{'u_west'}.long_name = 'western boundary u-momentum component';
  nc{'u_west'}.units = ncchar('meter second-1');
  nc{'u_west'}.units = 'meter second-1';
%
  nc{'v_west'} = ncdouble('bry_time','s_rho','eta_v') ;
  nc{'v_west'}.long_name = ncchar('western boundary v-momentum component');
  nc{'v_west'}.long_name = 'western boundary v-momentum component';
  nc{'v_west'}.units = ncchar('meter second-1');
  nc{'v_west'}.units = 'meter second-1';
%
  nc{'ubar_west'} = ncdouble('bry_time','eta_rho') ;
  nc{'ubar_west'}.long_name = ncchar('western boundary vertically integrated u-momentum component');
  nc{'ubar_west'}.long_name = 'western boundary vertically integrated u-momentum component';
  nc{'ubar_west'}.units = ncchar('meter second-1');
  nc{'ubar_west'}.units = 'meter second-1';
%
  nc{'vbar_west'} = ncdouble('bry_time','eta_v') ;
  nc{'vbar_west'}.long_name = ncchar('western boundary vertically integrated v-momentum component');
  nc{'vbar_west'}.long_name = 'western boundary vertically integrated v-momentum component';
  nc{'vbar_west'}.units = ncchar('meter second-1');
  nc{'vbar_west'}.units = 'meter second-1';
%
  nc{'zeta_west'} = ncdouble('bry_time','eta_rho') ;
  nc{'zeta_west'}.long_name = ncchar('western boundary sea surface height');
  nc{'zeta_west'}.long_name = 'western boundary sea surface height';
  nc{'zeta_west'}.units = ncchar('meter');
  nc{'zeta_west'}.units = 'meter';
%
end
%
%
% Create global attributes
%
nc.title = ncchar(title);
nc.title = title;
nc.date = ncchar(date);
nc.date = date;
nc.clim_file = ncchar(bryname);
nc.clim_file = bryname;
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
nc{'theta_s'}(:) =  theta_s; 
nc{'theta_b'}(:) =  theta_b; 
nc{'Tcline'}(:) =  hc; 
nc{'hc'}(:) =  hc; 
nc{'sc_r'}(:) =  sc; 
nc{'Cs_r'}(:) =  Cs; 
nc{'bry_time'}(:) =  time; 
if obc(1)==1
  nc{'u_south'}(:) =  0; 
  nc{'v_south'}(:) =  0; 
  nc{'ubar_south'}(:) =  0; 
  nc{'vbar_south'}(:) =  0; 
  nc{'zeta_south'}(:) =  0; 
  nc{'temp_south'}(:) =  0; 
  nc{'salt_south'}(:) =  0;
end 
if obc(2)==1
  nc{'u_east'}(:) =  0; 
  nc{'v_east'}(:) =  0; 
  nc{'ubar_east'}(:) =  0; 
  nc{'vbar_east'}(:) =  0; 
  nc{'zeta_east'}(:) =  0; 
  nc{'temp_east'}(:) =  0; 
  nc{'salt_east'}(:) =  0;
end 
if obc(3)==1
  nc{'u_north'}(:) =  0; 
  nc{'v_north'}(:) =  0; 
  nc{'ubar_north'}(:) =  0; 
  nc{'vbar_north'}(:) =  0; 
  nc{'zeta_north'}(:) =  0; 
  nc{'temp_north'}(:) =  0; 
  nc{'salt_north'}(:) =  0;
end 
if obc(4)==1
  nc{'u_west'}(:) =  0; 
  nc{'v_west'}(:) =  0; 
  nc{'ubar_west'}(:) =  0; 
  nc{'vbar_west'}(:) =  0; 
  nc{'zeta_west'}(:) =  0; 
  nc{'temp_west'}(:) =  0; 
  nc{'salt_west'}(:) =  0;
end 
close(nc)
return


