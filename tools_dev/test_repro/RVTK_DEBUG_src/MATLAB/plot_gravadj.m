%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Make 1 plot from the results of the GRAV_ADJ test case
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

tndx=69;
plot_psi=0;
%
% Read data
%
nc=netcdf('gravadj_his.nc');
h=nc{'h'}(:);
x=squeeze(nc{'x_rho'}(2,:));
zeta=squeeze(nc{'zeta'}(tndx,:,:));
t=squeeze(nc{'temp'}(tndx,:,2,:));
w=1000*squeeze(nc{'w'}(tndx,:,2,:));
[N,M]=size(t);
theta_s=nc.theta_s(:);
theta_b=nc.theta_b(:);
hc=nc.hc(:);
close(nc);

zr = zlevs(h,zeta,theta_s,theta_b,hc,N,'r');
zr=squeeze(zr(:,2,:));
xr=reshape(x,1,M);
xr=repmat(xr,[N 1])/1000 - 32;

psi=zeros(size(w));
for i=2:M;
  psi(:,i)=psi(:,i-1)-w(:,i).*(xr(:,i)-xr(:,i-1));
end


%
% Plot
%
hFig = figure;
set(hFig, 'Position', [700 700 700 200])
contourf(xr,zr,t,[15:1:40]);
if plot_psi
 hold on
 contour(xr,zr,psi,[-5:0.5:5],'k');
 hold off
end
caxis([15 38])
xlabel('X [km]')
ylabel('Depth [m]')
shading flat
colorbar
title('Gravitational adjustment')
export_fig -transparent -pdf gravadj.pdf



