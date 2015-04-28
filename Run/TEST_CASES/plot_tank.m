%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Make plot from the results of the TANK test case
% 
%  Further Information:  
%  http://www.romsagrif.org/
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
%  Ref: Penven, P., L. Debreu, P. Marchesiello and J.C. McWilliams,
%       Application of the ROMS embedding procedure for the Central 
%      California Upwelling System,  Ocean Modelling, 2006.
%
%  Patrick Marchesiello, IRD 2015
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all
close all
%================== User defined parameters ===========================
%
% --- model params ---
%
fname     = 'tank_his.nc';  % roms file name
g         = 9.81;           % gravity acceleration (m^2/s)
yindex    = 2;              % y index
makemovie = 0;              % make movie using QTWriter
makepdf   = 1;              % make pdf file

eta       = 0.1;            % nondimensional periodic amplitude 
D0        = 10;             % tank depth
Lt        = 9.9;            % tank length
%
%======================================================================

% ---------------------------------------------------------------------
% --- get grid from numerical model ---
% ---------------------------------------------------------------------

nc=netcdf(fname);
tindex=length(nc{'scrum_time'}(:)); % reads last record

if makemovie,
 movObj = QTWriter('tank.mov');
 tstr=1;
 tend=tindex;
else,
 tstr=tindex;
 tend=tindex;
end

% horizontal grid
hr=squeeze(nc{'h'}(yindex,:));
L=length(hr);
xr=squeeze(nc{'x_rho'}(yindex,:));
yr=squeeze(nc{'y_rho'}(yindex,:));
dx=xr(2)-xr(1);
% vertical grid parameters
N=length(nc('s_rho'));
theta_s=nc.theta_s(:); 
theta_b=nc.theta_b(:); 
hc=nc.hc(:); 

%============================================================
%              --- plot time series ---
% zeta: upper right
% u: upper middle
% w: center right
%=============================================================
kk=26;
nc=netcdf(fname);
t0    = nc{'scrum_time'}(1:tindex);
zeta01= 100*squeeze(nc{'zeta'}(1:tindex,yindex,L-1));
u01   = 100*squeeze(nc{'u'}(1:tindex,end,yindex,L/2));
w01   = 100*squeeze(nc{'w'}(1:tindex,kk,yindex,L-1)); % z=-4.95m
% vertical grid
zr0=zeros(tend,N,L);
for i=1:tend
 zeta0     = squeeze(nc{'zeta'}(i,yindex,:));
 zr0(i,:,:)= squeeze(zlevs(hr,zeta0,theta_s,theta_b,hc,N,'r',2));
end
close(nc)
zru0=0.5*(zr0(:,:,1:end-1)+zr0(:,:,2:end));

% --- Analytical solutions ---
x      = xr(L-1);
xu     = 0.5*(xr(L/2)+xr(L/2+1));
z0     = squeeze(zru0(:,end,L-1)); 
zz     = squeeze(zr0(:,kk,L-1)); %-4.95;

% Non-hydrostatic case
k      = pi/Lt;
sig    = sqrt(g*k*tanh(k*D0));
zeta02 =  100*eta*cos(k*x-sig*t0);
u02    =  100*eta*sig*(sin(sig*t0)./sinh(k*D0))*sin(k*xu).*cosh(k*(D0+z0));
w02    = -100*eta*sig*(sin(sig*t0)./sinh(k*D0))*cos(k*x).*sinh(k*(D0+zz));

% Hydrostatic case
sig    = k*sqrt(g*D0);
T_lw   = 2*pi/sig;
zeta03 =  100*eta*cos(k*x)*cos(sig*t0);
u03    =  100*g*eta*k/sig*sin(k*xu)*sin(sig*t0);
w03    = -100*g*eta*k^2/sig*cos(k*x)*sin(sig*t0).*(D0+zz);

t_per=t0/T_lw;

figure('pos',[100 500 600 600])
subplot(3,1,1)
plot(t_per,zeta03,'k',t_per,zeta02,'k--',t_per,zeta01,'r')
legend('Analytical hydro','Analytical N-hydro','Numerical hydro')
axis([0 5 -11 11])
xlabel('time (periods)')
ylabel('zeta (cm)')
grid on
title(['TANK test case'])

subplot(3,1,2)
plot(t_per,u03,'k',t_per,u02,'k--',t_per,u01,'r')
axis([0 5 -20 20])
xlabel('time (periods)')
ylabel('u (cm/s)')
grid on

subplot(3,1,3)
plot(t_per,w03,'k',t_per,w02,'k--',t_per,w01,'r')
axis([0 5 -20 20])
xlabel('time (periods)')
ylabel('w (cm/s)')
grid on

if makepdf
 print -dpdf tank.pdf
 eval('!pdfcrop tank.pdf tank_timeseries.pdf')
end

%============================================================
% --- make animation ---
%=============================================================

if makemovie || makepdf,

nc=netcdf(fname);
hf = figure;
axis tight; set(hf,'DoubleBuffer','on');
set(gca,'nextplot','replacechildren');

for tindex=tstr:tend % ---------------------------------------------

 disp(['Time index: ',num2str(tindex)])

 % vertical grid
 zeta=squeeze(nc{'zeta'}(tindex,yindex,:));
 zr=squeeze(zlevs(hr,zeta,theta_s,theta_b,hc,N,'r',2));
 zru=0.5*(zr(:,1:end-1)+zr(:,2:end));
 zw=squeeze(zlevs(hr,zeta,theta_s,theta_b,hc,N,'w',2));
 zwu=0.5*(zw(:,1:end-1)+zw(:,2:end));

 xr2d=repmat(xr,[N 1]);
 D=hr+zeta;
 D2d=repmat(D,[N 1]);

 % ---------------------------------------------------------------------
 % --- read/compute numerical model fields (index 1) ---
 % ---------------------------------------------------------------------
 time=nc{'scrum_time'}(tindex);

 zeta1=zeta;

 % ... num zonal velocity ...                         ---> xu,zru
 u1=squeeze(nc{'u'}(tindex,:,yindex,:));

 % ... num vertical velocity ...                      ---> xr,zw
 w1=squeeze(nc{'w'}(tindex,:,yindex,:));

 % ---------------------------------------------------------------------
 % --- compute analytical solutions (index 2) ---
 %
 % Chen, X.J., 2003. A fully hydrodynamic model for three-dimensional, 
 % free-surface flows. Int. J. Numer. Methods Fluids 42, 929–952.
 % ---------------------------------------------------------------------
 
 k     = pi/Lt;
 sig   = sqrt(g*k*tanh(k*D0));

 zeta2 =  eta*cos(k*xr2d)*cos(sig*time);
 u2    =  eta*sig*(sin(sig*time)/sinh(k*D0))*sin(k*xr2d).*cosh(k*(D0+zr));
 w2    = -eta*sig*(sin(sig*time)/sinh(k*D0))*cos(k*xr2d).*sinh(k*(D0+zr));

 % --- Hydrostatic solution ---
 k     = pi/Lt;
 sig   = k*sqrt(g*D0);

 zeta3 =  eta*cos(k*xr2d-sig*time);
 u3    =  g*eta*k/sig*sin(k*xr2d)*sin(sig*time);
 w3    =  -g*eta*k^2/sig*cos(k*xr2d)*sin(sig*time).*(D0+zr);

 %============================================================
 % --- plot ---
 %=============================================================

 u1=100*u1;
 u2=100*u2;
 u3=100*u3;
 w1=100*w1;
 w2=100*w2;
 w3=100*w3;
 u1(:,L)=u1(:,L-1);

 cmin=-20; cmax=20; nbcol=20;
 cint=(cmax-cmin)/nbcol;
 map=colormap(jet(nbcol));
 map(nbcol/2  ,:)=[1 1 1];
 map(nbcol/2+1,:)=[1 1 1];
 colormap(map);

 contourf(xr2d,zr,u1-u3,[cmin:cint:cmax]); hold on
 shading flat; colorbar;
 ha=plot(xr,zeta3,'color','g','LineWidth',2);
 hn=plot(xr,zeta1,'color','r','LineWidth',2);
 %legend([ha,hn],'Analytical','Numerical')
 grid on
 axis([0 10 -10 1])
 caxis([cmin cmax])
 time2=time/T_lw;
 title(['U error at Time ',num2str(time2),' periods'])
 hold off

 if makemovie,  
  % Write each frame to the file
  movObj.FrameRate =  5;
  writeMovie(movObj,getframe(hf));
  clf('reset')
 end
%----------------------------------

end % time loop

if makemovie, 
  movObj.Loop = 'loop'; % Set looping flag
  close(movObj);        % Finish writing movie and close file
end

if makepdf
 print -dpdf tank.pdf
 eval('!pdfcrop tank.pdf tank_40s.pdf')
end

close(nc)

end % makemovie

return






