function ncini=create_nestedinitial(inifile,gridfile,parentfile,title,...
				    theta_s,theta_b,Tcline,N,time,clobber,...
				    biol,pisces,namebiol,namepisces,unitbiol,unitpisces)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  function ncini=create_inifile(inifile,gridfile,theta_s,... 
%                  theta_b,Tcline,N,time,clobber) 
%
%   This function create the header of a Netcdf initial 
%   file.
%
%   Input: 
%
%   inifile     Netcdf initial file name (character string)
%   gridfile     Netcdf grid file name (character string).
%   parentfile  Netcdf parent initial file name (character string).
%   theta_s      S-coordinate surface control parameter.(Real)
%   theta_b      S-coordinate bottom control parameter.(Real)
%   Tcline       Width (m) of surface or bottom boundary layer
%                where higher vertical resolution is required 
%                during stretching.(Real)
%   N            Number of vertical levels.(Integer)
%   time        
%   clobber      Switch to allow or not writing over an existing
%                file.(character string)
%
%   Output
%
%   ncini       Output netcdf object.
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
disp(['Creating the file : ',inifile])
disp(' ')
%
%  Read the grid file
%
nc=netcdf(gridfile, 'nowrite');
h=nc{'h'}(:); 
status=close(nc);
[Mp,Lp]=size(h);
L=Lp-1;
M=Mp-1;
Np=N+1;
%
%  Create the initial file
%
type = 'INITIAL file' ; 
history = 'ROMS' ;
ncini = netcdf(inifile,clobber);
result = redef(ncini);
%
%  Create dimensions
%
ncini('xi_u') = L;
ncini('xi_v') = Lp;
ncini('xi_rho') = Lp;
ncini('eta_u') = Mp;
ncini('eta_v') = M;
ncini('eta_rho') = Mp;
ncini('s_rho') = N;
ncini('s_w') = Np;
ncini('tracer') = 2;
ncini('time') = 0;
ncini('one') = 1;
%
%  Create variables
%
ncini{'tstart'} = ncdouble('one') ;
ncini{'tend'} = ncdouble('one') ;
ncini{'theta_s'} = ncdouble('one') ;
ncini{'theta_b'} = ncdouble('one') ;
ncini{'Tcline'} = ncdouble('one') ;
ncini{'hc'} = ncdouble('one') ;
ncini{'sc_r'} = ncdouble('s_rho') ;
ncini{'Cs_r'} = ncdouble('s_rho') ;
ncini{'scrum_time'} = ncdouble('time') ;
ncini{'u'} = ncdouble('time','s_rho','eta_u','xi_u') ;
ncini{'v'} = ncdouble('time','s_rho','eta_v','xi_v') ;
ncini{'ubar'} = ncdouble('time','eta_u','xi_u') ;
ncini{'vbar'} = ncdouble('time','eta_v','xi_v') ;
ncini{'zeta'} = ncdouble('time','eta_rho','xi_rho') ;
ncini{'temp'} = ncdouble('time','s_rho','eta_rho','xi_rho') ;
ncini{'salt'} = ncdouble('time','s_rho','eta_rho','xi_rho') ;
if biol==1
  ncini{'NO3'} = ncdouble('time','s_rho','eta_rho','xi_rho') ;
  ncini{'CHLA'} = ncdouble('time','s_rho','eta_rho','xi_rho') ;
  ncini{'PHYTO'} = ncdouble('time','s_rho','eta_rho','xi_rho') ;
  ncini{'ZOO'} = ncdouble('time','s_rho','eta_rho','xi_rho') ;
end
if pisces==1
  ncini{'NO3'} = ncdouble('time','s_rho','eta_rho','xi_rho') ;
  ncini{'PO4'} = ncdouble('time','s_rho','eta_rho','xi_rho') ;
  ncini{'Si'}  = ncdouble('time','s_rho','eta_rho','xi_rho') ;
  ncini{'O2'}  = ncdouble('time','s_rho','eta_rho','xi_rho') ;
  ncini{'DIC'} = ncdouble('time','s_rho','eta_rho','xi_rho') ;
  ncini{'TALK'} = ncdouble('time','s_rho','eta_rho','xi_rho') ;
  ncini{'DOC'} = ncdouble('time','s_rho','eta_rho','xi_rho') ;
  ncini{'FER'} = ncdouble('time','s_rho','eta_rho','xi_rho') ;
end

%
%  Create attributes
%
ncini{'tstart'}.long_name = ncchar('start processing day');
ncini{'tstart'}.long_name = 'start processing day';
ncini{'tstart'}.units = ncchar('day');
ncini{'tstart'}.units = 'day';
%
ncini{'tend'}.long_name = ncchar('end processing day');
ncini{'tend'}.long_name = 'end processing day';
ncini{'tend'}.units = ncchar('day');
ncini{'tend'}.units = 'day';
%
ncini{'theta_s'}.long_name = ncchar('S-coordinate surface control parameter');
ncini{'theta_s'}.long_name = 'S-coordinate surface control parameter';
ncini{'theta_s'}.units = ncchar('nondimensional');
ncini{'theta_s'}.units = 'nondimensional';
%
ncini{'theta_b'}.long_name = ncchar('S-coordinate bottom control parameter');
ncini{'theta_b'}.long_name = 'S-coordinate bottom control parameter';
ncini{'theta_b'}.units = ncchar('nondimensional');
ncini{'theta_b'}.units = 'nondimensional';
%
ncini{'Tcline'}.long_name = ncchar('S-coordinate surface/bottom layer width');
ncini{'Tcline'}.long_name = 'S-coordinate surface/bottom layer width';
ncini{'Tcline'}.units = ncchar('meter');
ncini{'Tcline'}.units = 'meter';
%
ncini{'hc'}.long_name = ncchar('S-coordinate parameter, critical depth');
ncini{'hc'}.long_name = 'S-coordinate parameter, critical depth';
ncini{'hc'}.units = ncchar('meter');
ncini{'hc'}.units = 'meter';
%
ncini{'sc_r'}.long_name = ncchar('S-coordinate at RHO-points');
ncini{'sc_r'}.long_name = 'S-coordinate at RHO-points';
ncini{'sc_r'}.units = ncchar('nondimensional');
ncini{'sc_r'}.units = 'nondimensional';
ncini{'sc_r'}.valid_min = -1;
ncini{'sc_r'}.valid_max = 0;
ncini{'sc_r'}.field = ncchar('sc_r, scalar');
ncini{'sc_r'}.field = 'sc_r, scalar';
%
ncini{'Cs_r'}.long_name = ncchar('S-coordinate stretching curves at RHO-points');
ncini{'Cs_r'}.long_name = 'S-coordinate stretching curves at RHO-points';
ncini{'Cs_r'}.units = ncchar('nondimensional');
ncini{'Cs_r'}.units = 'nondimensional';
ncini{'Cs_r'}.valid_min = -1;
ncini{'Cs_r'}.valid_max = 0;
ncini{'Cs_r'}.field = ncchar('Cs_r, scalar');
ncini{'Cs_r'}.field = 'Cs_r, scalar';
%
ncini{'scrum_time'}.long_name = ncchar('time since initialization');
ncini{'scrum_time'}.long_name = 'time since initialization';
ncini{'scrum_time'}.units = ncchar('second');
ncini{'scrum_time'}.units = 'second';
ncini{'scrum_time'}.field = ncchar('time, scalar, series');
ncini{'scrum_time'}.field = 'time, scalar, series';
%
ncini{'u'}.long_name = ncchar('u-momentum component');
ncini{'u'}.long_name = 'u-momentum component';
ncini{'u'}.units = ncchar('meter second-1');
ncini{'u'}.units = 'meter second-1';
ncini{'u'}.field = ncchar('u-velocity, scalar, series');
ncini{'u'}.field = 'u-velocity, scalar, series';
%
ncini{'v'}.long_name = ncchar('v-momentum component');
ncini{'v'}.long_name = 'v-momentum component';
ncini{'v'}.units = ncchar('meter second-1');
ncini{'v'}.units = 'meter second-1';
ncini{'v'}.field = ncchar('v-velocity, scalar, series');
ncini{'v'}.field = 'v-velocity, scalar, series';
%
ncini{'ubar'}.long_name = ncchar('vertically integrated u-momentum component');
ncini{'ubar'}.long_name = 'vertically integrated u-momentum component';
ncini{'ubar'}.units = ncchar('meter second-1');
ncini{'ubar'}.units = 'meter second-1';
ncini{'ubar'}.field = ncchar('ubar-velocity, scalar, series');
ncini{'ubar'}.field = 'ubar-velocity, scalar, series';
%
ncini{'vbar'}.long_name = ncchar('vertically integrated v-momentum component');
ncini{'vbar'}.long_name = 'vertically integrated v-momentum component';
ncini{'vbar'}.units = ncchar('meter second-1');
ncini{'vbar'}.units = 'meter second-1';
ncini{'vbar'}.field = ncchar('vbar-velocity, scalar, series');
ncini{'vbar'}.field = 'vbar-velocity, scalar, series';
%
ncini{'zeta'}.long_name = ncchar('free-surface');
ncini{'zeta'}.long_name = 'free-surface';
ncini{'zeta'}.units = ncchar('meter');
ncini{'zeta'}.units = 'meter';
ncini{'zeta'}.field = ncchar('free-surface, scalar, series');
ncini{'zeta'}.field = 'free-surface, scalar, series';
%
ncini{'temp'}.long_name = ncchar('potential temperature');
ncini{'temp'}.long_name = 'potential temperature';
ncini{'temp'}.units = ncchar('Celsius');
ncini{'temp'}.units = 'Celsius';
ncini{'temp'}.field = ncchar('temperature, scalar, series');
ncini{'temp'}.field = 'temperature, scalar, series';
%
ncini{'salt'}.long_name = ncchar('salinity');
ncini{'salt'}.long_name = 'salinity';
ncini{'salt'}.units = ncchar('PSU');
ncini{'salt'}.units = 'PSU';
ncini{'salt'}.field = ncchar('salinity, scalar, series');
ncini{'salt'}.field = 'salinity, scalar, series';
%
if biol
  ncini{'NO3'}.long_name = ncchar('Nitrate');
  ncini{'NO3'}.long_name = 'Nitrate';
  ncini{'NO3'}.units = ncchar('mMol N m-3');
  ncini{'NO3'}.units = 'mMol N m-3';
  ncini{'NO3'}.field = ncchar('NO3, scalar, series');
  ncini{'NO3'}.field = 'NO3, scalar, series';
  %
  ncini{'CHLA'}.long_name = ncchar('Chlorophyll');
  ncini{'CHLA'}.long_name = 'Chlorophyll';
  ncini{'CHLA'}.units = ncchar('mg C l-1');
  ncini{'CHLA'}.units = 'mg C l-1';
  ncini{'CHLA'}.field = ncchar('CHLA, scalar, series');
  ncini{'CHLA'}.field = 'CHLA, scalar, series';
  %
  ncini{'PHYTO'}.long_name = ncchar('Phytoplankton');
  ncini{'PHYTO'}.long_name = 'Phytoplankton';
  ncini{'PHYTO'}.units = ncchar('mMol N m-3');
  ncini{'PHYTO'}.units = 'mMol N m-3';
  ncini{'PHYTO'}.field = ncchar('PHYTO, scalar, series');
  ncini{'PHYTO'}.field = 'PHYTO, scalar, series';
  %
  ncini{'ZOO'}.long_name = ncchar('Zooplankton');
  ncini{'ZOO'}.long_name = 'Zooplankton';
  ncini{'ZOO'}.units = ncchar('mMol N m-3');
  ncini{'ZOO'}.units = 'mMol N m-3';
  ncini{'ZOO'}.field = ncchar('ZOO, scalar, series');
  ncini{'ZOO'}.field = 'ZOO, scalar, series';
end;
%
if pisces
  for k=1:8
    disp(['K=',num2str(k)])
    ncini{char(namepisces(k))}.long_name = ncchar(char(namepisces(k)));
    ncini{char(namepisces(k))}.long_name = char(namepisces(k));
    ncini{char(namepisces(k))}.units = ncchar(char(unitpisces(k)));
    ncini{char(namepisces(k))}.units = char(unitpisces(k));
    ncini{char(namepisces(k))}.field = ncchar([char(namepisces(k)),', scalar, series']);
    ncini{char(namepisces(k))}.field = [char(namepisces(k)),', scalar, series'];
  end
end;
%
% Create global attributes
%
disp('Create global attribute')
ncini.title = ncchar(title);
ncini.title = title;
ncini.date = ncchar(date);
ncini.date = date;
ncini.clim_file = ncchar(inifile);
ncini.clim_file = inifile;
ncini.grd_file = ncchar(gridfile);
ncini.grd_file = gridfile;
ncini.parent_file = ncchar(parentfile);
ncini.parent_file = parentfile;
ncini.type = ncchar(type);
ncini.type = type;
ncini.history = ncchar(history);
ncini.history = history;
%
% Leave define mode
%
result = endef(ncini);
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
ncini{'tstart'}(:)=time/(24*3600); 
ncini{'tend'}(:)=time/(24*3600); 
ncini{'theta_s'}(:)=theta_s; 
ncini{'theta_b'}(:)=theta_b; 
ncini{'Tcline'}(:)=Tcline; 
ncini{'hc'}(:)=hc; 
ncini{'sc_r'}(:)=sc; 
ncini{'Cs_r'}(:)=Cs; 
ncini{'scrum_time'}(1)=time; 
ncini{'u'}(:)=0; 
ncini{'v'}(:)=0; 
ncini{'zeta'}(:)=0; 
ncini{'ubar'}(:)=0; 
ncini{'vbar'}(:)=0; 
ncini{'temp'}(:)=0; 
ncini{'salt'}(:)=0; 
%
if biol==1
  disp('Write variable biology')
  ncini{'NO3'}(:)=0;
  ncini{'CHLA'}(:)=0;
  ncini{'PHYTO'}(:)=0;
  ncini{'ZOO'}(:)=0;
end
%
if pisces==1
  disp('Write variable pisces')
  ncini{'NO3'}(:)=0;
  ncini{'PO4'}(:)=0;
  ncini{'Si'}(:)=0;
  ncini{'O2'}(:)=0;
  ncini{'DIC'}(:)=0;
  ncini{'TALK'}(:)=0;
  ncini{'DOC'}(:)=0;
  ncini{'FER'}(:)=0;
end
%
%
% Synchronize on disk
%
sync(ncini);
return


