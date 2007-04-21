function getpot_bry(bryname,grdname,obcndx);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Get potential temperature of seawater from insitu
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

%
% open the grid file  
% 
ng=netcdf(grdname);
L=length(ng('xi_rho'));
M=length(ng('eta_rho'));
if obcndx==1
  h=ng{'h'}(1,:);
  suffix='_south';
elseif obcndx==2
  h=ng{'h'}(:,L);
  suffix='_east';
elseif obcndx==3
  h=ng{'h'}(M,:);
  suffix='_north';
elseif obcndx==4
  h=ng{'h'}(:,1);
  suffix='_west';
end
close(ng);
%
% open the clim file  
% 
nc=netcdf(bryname,'write');
theta_s = nc{'theta_s'}(:);
theta_b =  nc{'theta_b'}(:);
hc  =  nc{'hc'}(:);
N =  length(nc('s_rho'));
tlen =  length(nc('bry_time'));
if tlen==0
  tlen=1;
end
%
% Get the sigma depths
%
P=-1e-4*1025*9.81*squeeze(zlevs(h,0.*h,theta_s,theta_b,hc,N,'r'));
%
% loop on time
%
for l=1:tlen
%  disp([' Time index: ',num2str(l),' of total: ',num2str(tlen)])
  T=squeeze(nc{['temp',suffix]}(l,:,:));
  S=squeeze(nc{['salt',suffix]}(l,:,:));
  nc{'temp'}(l,:,:)=theta(S,T,P);
end
close(nc);
return
