%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Make 2 graphics from the results of the INNER SHELF test case
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
%  Copyright (c) 2002-2006 by Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr  
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all
close all

tndx=30;
%
% Read data
%
nc=netcdf('inner_avg.nc');
h=nc{'h'}(:);
x=squeeze(nc{'x_rho'}(2,:));
zeta=squeeze(nc{'zeta'}(tndx,:,:));
t=squeeze(nc{'temp'}(tndx,:,2,:));
[N,L]=size(t);
u=squeeze(nc{'u'}(tndx,:,2,:));
v=squeeze(nc{'v'}(tndx,:,2,:));
w=squeeze(nc{'w'}(tndx,:,2,:));
theta_s=nc.theta_s(:);
theta_b=nc.theta_b(:);
hc=nc.hc(:);
close(nc);
zr = zlevs(h,zeta,theta_s,theta_b,hc,N,'r');
zr=squeeze(zr(:,1,:));
zw = zlevs(h,zeta,theta_s,theta_b,hc,N,'w');
zw=squeeze(zw(:,1,:));

xr=reshape(x,1,L);
xr=repmat(xr,[N 1])/1000;
xw=reshape(x,1,L);
xw=repmat(xw,[N+1 1])/1000;

%
% First plot
%
figure(1)
contourf(xr,zr,t,[6:0.5:20])
shading flat
caxis([7 19])
colorbar
title('Inner shelf temperature [^oC] vertical section')

%
% Second plot
%
figure(2)
contourf(xr,zr,24*3600*w,[-20:2:40])
shading flat
caxis([-10 30])
colorbar
title('Inner shelf vertical velocities [m.day^{-1}] vertical section')


