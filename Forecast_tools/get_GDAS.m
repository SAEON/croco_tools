function [t,tx,ty,tair,rhum,prate,wspd,radlw,radsw]=...
         get_GDAS(fname,mask,tndx,jrange,i1min,i1max,...
	 i2min,i2max,i3min,i3max,missvalue)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Download one full subset of GFS for ROMS bulk for 1 time step
% Put them in the ROMS units
%
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
%  Updated    20-Aug-2008 by Matthieu Caillaud & P. Marchesiello
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
trange=['[',num2str(min(tndx)),':',num2str(max(tndx)),']'];
%
% Get GFS variables for 1 time step
%
disp(' ')
disp('===========================================')
%disp('time...')
t=readdap(fname,'time',trange);
%t=t+365; % put it in "matlab" time.
t=t+364.75; % put it in "matlab" time. PM
disp(['GDAS: ',datestr(t)])
disp('===========================================')
%disp('u...')
u=mask.*getdap('',fname,'ugrd10m',trange,'',jrange,...
                i1min,i1max,i2min,i2max,i3min,i3max);
u(abs(u)>=missvalue)=NaN;
%disp('v...')
v=mask.*getdap('',fname,'vgrd10m',trange,'',jrange,...
                i1min,i1max,i2min,i2max,i3min,i3max);
v(abs(v)>=missvalue)=NaN;
%disp('skt...')
skt=mask.*getdap('',fname,'tmpsfc',trange,'',jrange,...
                  i1min,i1max,i2min,i2max,i3min,i3max);
skt(abs(skt)>=missvalue)=NaN;
%disp('tair...')
tair=mask.*getdap('',fname,'tmp2m',trange,'',jrange,...
                i1min,i1max,i2min,i2max,i3min,i3max);
tair(abs(tair)>=missvalue)=NaN;
%disp('rhum...')
rhum=mask.*getdap('',fname,'rh2m',trange,'',jrange,...
                i1min,i1max,i2min,i2max,i3min,i3max);
rhum(abs(rhum)>=missvalue)=NaN;
%disp('prate...')
prate=mask.*getdap('',fname,'pratesfc',trange,'',jrange,...
                i1min,i1max,i2min,i2max,i3min,i3max);
prate(abs(prate)>=missvalue)=NaN;
%disp('radlw...')
radlw=mask.*getdap('',fname,'dlwrfsfc',trange,'',jrange,...
                i1min,i1max,i2min,i2max,i3min,i3max);
radlw(abs(radlw)>=missvalue)=NaN;
%disp('radsw...')
radsw=mask.*getdap('',fname,'dswrfsfc',trange,'',jrange,...
                i1min,i1max,i2min,i2max,i3min,i3max);
radsw(abs(radsw)>=missvalue)=NaN;
%
% Transform the variables
%
%
% 1: Air temperature: Convert from Kelvin to Celsius
%
tair=tair-273.15;
%
% 2: Relative humidity: Convert from % to fraction
%
rhum=rhum/100;
%
% 3: Precipitation rate: Convert from [kg/m^2/s] to cm/day
%
prate=prate*0.1*(24*60*60.0);
prate(abs(prate)<1.e-4)=0;
%
% 4: Net shortwave flux: [W/m^2]
%    ROMS convention: positive downward: same as GFS
% ?? albedo ??
%
radsw(radsw<1.e-10)=0;
%
% 5: Net outgoing Longwave flux:  [W/m^2]
%    ROMS convention: positive upward (opposite to nswrs)
%    GFS convention: positive downward --> * (-1)
%    input: downward longwave rad. and
%    skin temperature.
%
skt=skt-273.15; 
radlw=-lwhf(skt,radlw); 
%
% 6: Wind speed
%
wspd=sqrt(u.^2+v.^2);
%
% 7: Compute the stress following large and pond
%
[Cd,uu]=cdnlp(wspd,10.);
rhoa=air_dens(tair,rhum*100);
tx=Cd.*rhoa.*u.*wspd;
ty=Cd.*rhoa.*v.*wspd;
%
return
