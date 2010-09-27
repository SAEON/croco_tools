%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Add the tides to the ROMS forcing file for tidal forcing
%  by the boundary conditions.
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
%  Copyright (c) 2003-2006 by Patrick Marchesiello and Meinte Blass
%
%  Updated   1-Sep-2006 by Pierrick Penven (generalisation of romstools_param.m)
%  Updated   3-Oct-2006 by Pierrick Penven (cleaning + phase lag for Yorig time)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all
close all
%%%%%%%%%%%%%%%%%%%%% USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%%
%
% Common parameters
%
romstools_param
%
%%%%%%%%%%%%%%%%%%% END USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%
%
% Get start time of simulation in fractional mjd for nodal correction
%
date_mjd=mjd(Ymin,Mmin,Dmin);
[pf,pu,t0,phase_mkB]=egbert_correc(date_mjd,Hmin,Min_min,Smin);
deg=180.0/pi;
%
% Add a phase correction to be consistent with the 'Yorig' time
%
t0=t0-24*mjd(Yorig,1,1);
%
%  Read in ROMS grid.
%
disp('Reading ROMS grid parameters ...');
nc=netcdf(grdname);
lonr=nc{'lon_rho'}(:);
latr=nc{'lat_rho'}(:);
lonu=nc{'lon_u'}(:);
latu=nc{'lat_u'}(:);
lonv=nc{'lon_v'}(:);
latv=nc{'lat_v'}(:);
rangle=nc{'angle'}(:); % devrait etre utilise....
h=nc{'h'}(:);
rmask=nc{'mask_rho'}(:);
close(nc)
%
% Read in TPX file
%
nctides=netcdf(tidename);
periods=nctides{'periods'}(:);
cmpt=nctides.components(:);
close(nctides)
Nmax=length(periods);
Ntides=min([Nmax Ntides]);
%
% Prepare the forcing file
%
for i=1:Ntides
  components(3*i-2:3*i)=[cmpt(3*tidalrank(i)-2:3*tidalrank(i)-1),' '];
end
disp(['Tidal components : ',components])
nc_add_tides(frcname,Ntides,date_mjd,components)
ncfrc=netcdf(frcname,'write');
%
% Loop on periods
%
for itide=1:Ntides
  it=tidalrank(itide);
  disp(['Processing tide : ',num2str(itide),' of ',num2str(Ntides)])
  ncfrc{'tide_period'}(itide)=periods(it);
%
% Get the phase corrections
%
  correc_amp=pf(it);
  correc_phase=-phase_mkB(it)-pu(it)+360.*t0./periods(it);	   
%
% Process the surface elevation
%
  disp('  ssh...')
  ur=ext_data_tpxo(tidename,'ssh_r',it,lonr,latr,'r',Roa);
  ui=ext_data_tpxo(tidename,'ssh_i',it,lonr,latr,'r',Roa);
  ei=complex(ur,ui);
  ncfrc{'tide_Ephase'}(itide,:,:)=mod(-deg*angle(ei)+correc_phase,360.0);     
  ncfrc{'tide_Eamp'}(itide,:,:)=abs(ei)*correc_amp;
%
% Process U
%
  disp('  u...')
  ur=ext_data_tpxo(tidename,'u_r',it,lonr,latr,'u',Roa);
  ui=ext_data_tpxo(tidename,'u_i',it,lonr,latr,'u',Roa);
  ei=complex(ur,ui);
  upha=mod(-deg*angle(ei)+correc_phase,360.0); 
  uamp=abs(ei)*correc_amp;
%
% Process V
%
  disp('  v...')
  ur=ext_data_tpxo(tidename,'v_r',it,lonr,latr,'v',Roa);
  ui=ext_data_tpxo(tidename,'v_i',it,lonr,latr,'v',Roa);
  ei=complex(ur,ui);
  vpha=mod(-deg*angle(ei)+correc_phase,360.0); 
  vamp=abs(ei)*correc_amp;
%
% Convert to tidal ellipses
%
  disp('  Convert to tidal ellipse parameters...')
  [major,eccentricity,inclination,phase]=ap2ep(uamp,upha,vamp,vpha);
  ncfrc{'tide_Cmin'}(itide,:,:)=major.*eccentricity;
  ncfrc{'tide_Cmax'}(itide,:,:)=major;
  ncfrc{'tide_Cangle'}(itide,:,:)=inclination;
  ncfrc{'tide_Cphase'}(itide,:,:)=phase;
%
end
%
% Close the file
%
close(ncfrc)
%
% Plot
%
if makeplot==1
  warning off
  figure(1)
  plot_tide(grdname,frcname,1,0.5,2,coastfileplot)
  
  figure(2)
  plot_tide(grdname,frcname,2,0.5,2,coastfileplot)
  
  figure(3)
  plot_tide(grdname,frcname,3,0.5,2,coastfileplot)
  
  figure(4)
  plot_tide(grdname,frcname,4,0.5,2,coastfileplot)
  
  figure(5)
  plot_tide(grdname,frcname,5,0.5,2,coastfileplot)
  
  figure(6)
  plot_tide(grdname,frcname,6,0.5,2,coastfileplot)
  
  figure(7)
  plot_tide(grdname,frcname,7,0.5,2,coastfileplot)
  
  figure(8)
  plot_tide(grdname,frcname,8,0.5,2,coastfileplot)
  
  figure(9)
  plot_tide(grdname,frcname,9,0.5,2,coastfileplot)
  
  figure(10)
  plot_tide(grdname,frcname,10,0.5,2,coastfileplot)
 
  figure(11)
  clm_tides(grdname,frcname,Ntides,Ymin,Mmin,Dmin,...
            Hmin,Min_min,Smin,Yorig,lon0,lat0,Z0)
   warning on     
end
