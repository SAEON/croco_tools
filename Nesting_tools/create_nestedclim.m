function ncclim=create_nestedclim(climfile,gridfile,parentfile,title,...
				  theta_s,theta_b,Tcline,N,...
				  ttime,stime,utime,vtime,sshtime,...
				  tcycle,scycle,ucycle,vcycle,sshcycle,...
				  no3p_time,po4_time,si_time,o2_time,dic_time,talk_time, doc_time,fer_time,...
				  no3p_cycle,po4_cycle,si_cycle,o2_cycle,dic_cycle,talk_cycle,doc_cycle,fer_cycle,...
				  no3_time,chla_time,phyto_time,zoo_time,...
				  no3_cycle,chla_cycle,phyto_cycle,zoo_cycle,...     
				  clobber,...    
				  biol,pisces,namebiol,namepisces,unitbiol,unitpisces)
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
% $$$ biol
% $$$ pisces
% $$$ namebiol
% $$$ namepisces
% $$$ unitbiol
% $$$ unitpisces
% $$$ no3_time
% $$$ chla_time
% $$$ phyto_time
% $$$ zoo_time
% $$$ no3_cycle
% $$$ chla_cycle
% $$$ phyto_cycle
% $$$ zoo_cycle

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

if biol
  ncclim('no3_time')= length(no3_time);
  ncclim('chla_time') = length(chla_time);
  ncclim('phyto_time') = length(phyto_time);
  ncclim('zoo_time') = length(zoo_time);
end
%
if pisces
  ncclim('no3_time') = length(no3p_time);
  ncclim('po4_time') = length(po4_time);
  ncclim('si_time') = length(si_time);
  ncclim('o2_time') = length(o2_time);
  ncclim('dic_time') = length(dic_time);
  ncclim('talk_time') = length(talk_time);
  ncclim('doc_time') = length(doc_time);
  ncclim('fer_time') = length(fer_time);
end
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
if biol
  ncclim{'no3_time'} = ncdouble('no3_time') ;
  ncclim{'chla_time'} = ncdouble('chla_time') ;
  ncclim{'phyto_time'} = ncdouble('phyto_time') ;
  ncclim{'zoo_time'} = ncdouble('zoo_time') ;
  %
  ncclim{'NO3'} = ncdouble('no3_time','s_rho','eta_rho','xi_rho') ;
  ncclim{'CHLA'} = ncdouble('chla_time','s_rho','eta_rho','xi_rho') ;
  ncclim{'PHYTO'} = ncdouble('phyto_time','s_rho','eta_rho','xi_rho') ;
  ncclim{'ZOO'} = ncdouble('zoo_time','s_rho','eta_rho','xi_rho') ;
end
%
if pisces
  ncclim{'no3_time'} = ncdouble('no3_time') ;
  ncclim{'po4_time'} = ncdouble('po4_time') ;
  ncclim{'si_time'} = ncdouble('si_time') ;
  ncclim{'o2_time'} = ncdouble('o2_time') ;
  ncclim{'dic_time'} = ncdouble('dic_time') ;
  ncclim{'talk_time'} = ncdouble('talk_time') ;
  ncclim{'doc_time'} = ncdouble('doc_time') ;
  ncclim{'fer_time'} = ncdouble('fer_time') ;
  % 
  ncclim{'NO3'} = ncdouble('no3_time','s_rho','eta_rho','xi_rho') ;
  ncclim{'PO4'} = ncdouble('po4_time','s_rho','eta_rho','xi_rho') ;
  ncclim{'Si'} = ncdouble('si_time','s_rho','eta_rho','xi_rho') ;
  ncclim{'O2'} = ncdouble('o2_time','s_rho','eta_rho','xi_rho') ;
  ncclim{'DIC'} = ncdouble('dic_time','s_rho','eta_rho','xi_rho') ;
  ncclim{'TALK'} = ncdouble('talk_time','s_rho','eta_rho','xi_rho') ;
  ncclim{'DOC'} = ncdouble('doc_time','s_rho','eta_rho','xi_rho') ;
  ncclim{'FER'} = ncdouble('fer_time','s_rho','eta_rho','xi_rho') ;
end
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
if biol    
  ncclim{'no3_time'}.long_name = ncchar('time for Nitrate');
  ncclim{'no3_time'}.long_name = 'time for Nitrate';
  ncclim{'no3_time'}.units = ncchar('day');
  ncclim{'no3_time'}.units = 'day';
  ncclim{'no3_time'}.cycle_length = no3_cycle;
  ncclim{'no3_time'}.field = ncchar('no3_time, scalar, serie');
  ncclim{'no3_time'}.field = 'no3_time, scalar, serie';
  %
  ncclim{'chla_time'}.long_name = ncchar('time for Chlorophyll');
  ncclim{'chla_time'}.long_name = 'time for Chlorophyll';
  ncclim{'chla_time'}.units = ncchar('day');
  ncclim{'chla_time'}.units = 'day';
  ncclim{'chla_time'}.cycle_length = chla_cycle;
  ncclim{'chla_time'}.field = ncchar('chla_time, scalar, serie');
  ncclim{'chla_time'}.field = 'chla_time, scalar, serie';
  %
  ncclim{'phyto_time'}.long_name = ncchar('time for Phytoplankton');
  ncclim{'phyto_time'}.long_name = 'time for Phytoplankton';
  ncclim{'phyto_time'}.units = ncchar('day');
  ncclim{'phyto_time'}.units = 'day';
  ncclim{'phyto_time'}.cycle_length = phyto_cycle;
  ncclim{'phyto_time'}.field = ncchar('phyto_time, scalar, serie');
  ncclim{'phyto_time'}.field = 'phyto_time, scalar, serie';
  %
  ncclim{'zoo_time'}.long_name = ncchar('time for Zooplankton');
  ncclim{'zoo_time'}.long_name = 'time for Zooplankton';
  ncclim{'zoo_time'}.units = ncchar('day');
  ncclim{'zoo_time'}.units = 'day';
  ncclim{'zoo_time'}.cycle_length = zoo_cycle;
  ncclim{'zoo_time'}.field = ncchar('zoo_time, scalar, serie');
  ncclim{'zoo_time'}.field = 'zoo_time, scalar, serie';
  %
  ncclim{'NO3'}.long_name = ncchar('Nitrate');
  ncclim{'NO3'}.long_name = 'Nitrate';
  ncclim{'NO3'}.units = ncchar('mMol N m-3');
  ncclim{'NO3'}.units = 'mMol N m-3';
  ncclim{'NO3'}.field = ncchar('NO3, scalar, series');
  ncclim{'NO3'}.field = 'NO3, scalar, series';
  %
  ncclim{'CHLA'}.long_name = ncchar('Chlorophyll');
  ncclim{'CHLA'}.long_name = 'Chlorophyll';
  ncclim{'CHLA'}.units = ncchar('mg C l-1');
  ncclim{'CHLA'}.units = 'mg C l-1';
  ncclim{'CHLA'}.field = ncchar('CHLA, scalar, series');
  ncclim{'CHLA'}.field = 'CHLA, scalar, series';
  %
  ncclim{'PHYTO'}.long_name = ncchar('Phytoplankton');
  ncclim{'PHYTO'}.long_name = 'Phytoplankton';
  ncclim{'PHYTO'}.units = ncchar('mMol N m-3');
  ncclim{'PHYTO'}.units = 'mMol N m-3';
  ncclim{'PHYTO'}.field = ncchar('PHYTO, scalar, series');
  ncclim{'PHYTO'}.field = 'PHYTO, scalar, series';
  %  
  ncclim{'ZOO'}.long_name = ncchar('Zooplankton');
  ncclim{'ZOO'}.long_name = 'Zooplankton';
  ncclim{'ZOO'}.units = ncchar('mMol N m-3');
  ncclim{'ZOO'}.units = 'mMol N m-3';
  ncclim{'ZOO'}.field = ncchar('ZOO, scalar, series');
  ncclim{'ZOO'}.field = 'ZOO, scalar, series';
end;
%
if pisces
  ncclim{'no3_time'}.long_name = ncchar('time for nitrate');
  ncclim{'no3_time'}.long_name = 'time for Nitrate';
  ncclim{'no3_time'}.units = ncchar('day');
  ncclim{'no3_time'}.units = 'day';
  ncclim{'no3_time'}.cycle_length = no3p_cycle;
  ncclim{'no3_time'}.field = ncchar('no3_time, scalar, serie');
  ncclim{'no3_time'}.field = 'no3_time, scalar, serie'; 
  %    
  ncclim{'po4_time'}.long_name = ncchar('time for phosphate');
  ncclim{'po4_time'}.long_name = 'time for Phosphate';
  ncclim{'po4_time'}.units = ncchar('day');
  ncclim{'po4_time'}.units = 'day';
  ncclim{'po4_time'}.cycle_length = po4_cycle;
  ncclim{'po4_time'}.field = ncchar('po4_time, scalar, serie');
  ncclim{'po4_time'}.field = 'po4_time, scalar, serie'; 
  %
  ncclim{'si_time'}.long_name = ncchar('time for silicate ');
  ncclim{'si_time'}.long_name = 'time for Nitrate';
  ncclim{'si_time'}.units = ncchar('day');
  ncclim{'si_time'}.units = 'day';
  ncclim{'si_time'}.cycle_length = si_cycle;
  ncclim{'si_time'}.field = ncchar('si_time, scalar, serie');
  ncclim{'si_time'}.field = 'si_time, scalar, serie'; 
  %
  ncclim{'o2_time'}.long_name = ncchar('time for oxygen');
  ncclim{'o2_time'}.long_name = 'time for Nitrate';
  ncclim{'o2_time'}.units = ncchar('day');
  ncclim{'o2_time'}.units = 'day';
  ncclim{'o2_time'}.cycle_length = o2_cycle;
  ncclim{'o2_time'}.field = ncchar('o2_time, scalar, serie');
  ncclim{'o2_time'}.field = 'o2_time, scalar, serie'; 
  %
  ncclim{'dic_time'}.long_name = ncchar('time for DIC');
  ncclim{'dic_time'}.long_name = 'time for Nitrate';
  ncclim{'dic_time'}.units = ncchar('day');
  ncclim{'dic_time'}.units = 'day';
  ncclim{'dic_time'}.cycle_length = dic_cycle;
  ncclim{'dic_time'}.field = ncchar('dic_time, scalar, serie');
  ncclim{'dic_time'}.field = 'dic_time, scalar, serie'; 
  %
  ncclim{'talk_time'}.long_name = ncchar('time for TALK');
  ncclim{'talk_time'}.long_name = 'time for Nitrate';
  ncclim{'talk_time'}.units = ncchar('day');
  ncclim{'talk_time'}.units = 'day';
  ncclim{'talk_time'}.cycle_length = talk_cycle;
  ncclim{'talk_time'}.field = ncchar('talk_time, scalar, serie');
  ncclim{'talk_time'}.field = 'talk_time, scalar, serie'; 
  %
  ncclim{'doc_time'}.long_name = ncchar('time for DOC');
  ncclim{'doc_time'}.long_name = 'time for Nitrate';
  ncclim{'doc_time'}.units = ncchar('day');
  ncclim{'doc_time'}.units = 'day';
  ncclim{'doc_time'}.cycle_length = doc_cycle;
  ncclim{'doc_time'}.field = ncchar('doc_time, scalar, serie');
  ncclim{'doc_time'}.field = 'doc_time, scalar, serie'; 
  %
  ncclim{'fer_time'}.long_name = ncchar('time for iron');
  ncclim{'fer_time'}.long_name = 'time for Nitrate';
  ncclim{'fer_time'}.units = ncchar('day');
  ncclim{'fer_time'}.units = 'day';
  ncclim{'fer_time'}.cycle_length = fer_cycle;
  ncclim{'fer_time'}.field = ncchar('fer_time, scalar, serie');
  ncclim{'fer_time'}.field = 'fer_time, scalar, serie'; 
  %
  for k=1:length(namepisces)
    disp(['K=',num2str(k)])
    ncclim{char(namepisces(k))}.long_name = ncchar(char(namepisces(k)));
    ncclim{char(namepisces(k))}.long_name = char(namepisces(k));
    ncclim{char(namepisces(k))}.units = ncchar(char(unitpisces(k)));
    ncclim{char(namepisces(k))}.units = char(unitpisces(k));
    ncclim{char(namepisces(k))}.field = ncchar([char(namepisces(k)),', scalar, series']);
    ncclim{char(namepisces(k))}.field = [char(namepisces(k)),', scalar, series'];
  end
end;
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
if biol==1
  disp('Write variable biology')
  % size(no3_time)
  % chla_time
  ncclim{'chla_time'}(:)=squeeze(chla_time);
  ncclim{'no3_time'}(:)=no3_time;
  ncclim{'phyto_time'}(:)=phyto_time;
  ncclim{'zoo_time'}(:)=zoo_time;
  % 
  ncclim{'NO3'}(:)=0;
  ncclim{'CHLA'}(:)=0;
  ncclim{'PHYTO'}(:)=0;
  ncclim{'ZOO'}(:)=0;
end

%
if pisces==1
  disp('Write variable pisces')
  ncclim{'no3_time'}(:)=no3p_time;
  ncclim{'po4_time'}(:)=po4_time;
  ncclim{'si_time'}(:)=si_time;
  ncclim{'o2_time'}(:)=o2_time;
  ncclim{'dic_time'}(:)=dic_time;
  ncclim{'talk_time'}(:)=talk_time;
  ncclim{'doc_time'}(:)=doc_time;
  ncclim{'fer_time'}(:)=fer_time;
  %
  %%
  %
  ncclim{'NO3'}(:)=0;
  ncclim{'PO4'}(:)=0;
  ncclim{'Si'}(:)=0;
  ncclim{'O2'}(:)=0;
  ncclim{'DIC'}(:)=0;
  ncclim{'TALK'}(:)=0;
  ncclim{'DOC'}(:)=0;
  ncclim{'FER'}(:)=0;
end
%
%
% Synchronize on disk
%
sync(ncclim);
return


