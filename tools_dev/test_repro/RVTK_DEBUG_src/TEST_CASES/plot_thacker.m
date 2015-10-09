%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Make plot from the results of the SHOREFACE test case
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
%  Patrick Marchesiello, IRD 2013
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all
close all
%================== User defined parameters ===========================
%
% --- model params ---
%
fname     = 'thacker_his.nc';  % roms file name
g         = 9.81;                  % gravity acceleration (m^2/s)
yindex    = 101;                   % y index
makemovie = 0;                     % make movie using QTWriter
makepdf   = 0;                     % make pdf file
%
%======================================================================

% ---------------------------------------------------------------------
% --- get grid from numerical model ---
% ---------------------------------------------------------------------

nc=netcdf(fname);
tindex=length(nc{'scrum_time'}(:)); % reads last record

if makemovie,
 movObj = QTWriter('thacker.mov');
 tstr=1;
 tend=tindex;
else,
 tstr=tindex;
 tend=tstr;
end

hf = figure;
axis tight; set(hf,'DoubleBuffer','on');
set(gca,'nextplot','replacechildren');

for tindex=tstr:tend % ---------------------------------------------

%
% horizontal grid
 hr=squeeze(nc{'h'}(yindex,:));
 xindex=1;
 hr=hr(xindex:end);
 L=length(hr);
 xr=squeeze(nc{'x_rho'}(yindex,xindex:end));
 yr=squeeze(nc{'y_rho'}(yindex,xindex:end));
 dx=xr(2)-xr(1);
%
% vertical grid
 N=length(nc('s_rho'));
 theta_s=nc.theta_s(:); 
 theta_b=nc.theta_b(:); 
 hc=nc.hc(:); 
 zeta=squeeze(nc{'zeta'}(tindex,yindex,xindex:end));
 zr=squeeze(zlevs(hr,zeta,theta_s,theta_b,hc,N,'r',2));
 dzr=zr(2:end,:)-zr(1:end-1,:);               % ---> zw(2:N,:)
 zru=0.5*(zr(:,1:end-1)+zr(:,2:end));
 dzru=zru(2:end,:)-zru(1:end-1,:);            % ---> zwu(2:N,:)
 zw=squeeze(zlevs(hr,zeta,theta_s,theta_b,hc,N,'w',2));
 dzw=zw(2:end,:)-zw(1:end-1,:);               % ---> zr
 zwu=0.5*(zw(:,1:end-1)+zw(:,2:end));
 dzwu=zwu(2:end,:)-zwu(1:end-1,:);            % ---> zru
%
 xr2d=repmat(xr,[N 1]);
 D=hr+zeta;
 D2d=repmat(D,[N 1]);

 % ---------------------------------------------------------------------
 % --- read/compute numerical model fields (index 1) ---
 % ---------------------------------------------------------------------
 time=nc{'scrum_time'}(tindex)/86400;

 zeta1=zeta;

 % ... num zonal velocity ...                         ---> xu,zru
 u1=squeeze(nc{'u'}(tindex,:,yindex,xindex:end));

 % ... num meridional velocity ...                    ---> xr,zr
 v1=squeeze(nc{'v'}(tindex,:,yindex,xindex:end));

 % ... num vertical velocity ...                      ---> xr,zw
 w1=squeeze(nc{'w'}(tindex,:,yindex,xindex:end));

 % ... num temperature ...                            ---> xr,zr
 t1=squeeze(nc{'temp'}(tindex,:,yindex,xindex:end));

 % ---------------------------------------------------------------------
 % --- compute analytical solutions (index 2) ---
 % ---------------------------------------------------------------------
 eta=0.1;
 D0=max(max(hr));       % --> 10
 f=nc{'f'}(yindex,1);   % --> 1.e-4;
 Lt=79.5e3;             % --> 80.e3-0.5: distance at u points
 omega=0.5*f + sqrt(0.25*f^2 +2*g*D0/Lt^2);

 u2=-eta*omega*Lt*sin(omega*time*86400)*ones(size(u1));
 v2=-eta*omega*Lt*cos(omega*time*86400)*ones(size(v1));
 zeta2=2*eta*D0*(xr./Lt*cos(omega*time*86400) ...
                -yr./Lt*cos(omega*time*86400) ...
                                - 0.5*eta/Lt) ...
                  .*ones(size(zeta1));
 
 %============================================================
 % --- plot ---
 %=============================================================
 Dcrit=0.11;
 xr=1.e-3*xr;
 xr2d=1.e-3*xr2d;
 u1(:,L)=u1(:,L-1);
 u2(:,L)=u2(:,L-1);
 zeta1(D<Dcrit)=NaN;
 u1(D2d<Dcrit)=NaN;
 zeta2(zeta2<-hr)=NaN;

 cmin=-2; cmax=2; nbcol=20;
 cint=(cmax-cmin)/nbcol;
 map=colormap(jet(nbcol));
 map(nbcol/2  ,:)=[1 1 1];
 map(nbcol/2+1,:)=[1 1 1];
 colormap(map);
 contourf(xr2d,zr,u1-u2,[cmin:cint:cmax]); hold on
 shading flat; colorbar;
 plot(xr,-hr,'color','k','LineWidth',3);
 plot(xr,zeta1,'color','g','LineWidth',3);
 plot(xr,zeta2,'color','r','LineWidth',2);
 grid on
 axis([-100 100 -10 5])
 caxis([cmin cmax])
 thour=floor(time*24);
 title(['THACKER: U Err at Time ',num2str(thour),' hour'])
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

%============================================================
% --- plot time series at center point ---
%=============================================================

t0=nc{'scrum_time'}(1:tindex)./86400;
u10=squeeze(nc{'u'}(1:tindex,3,yindex,yindex));
u20=-eta*omega*Lt*sin(omega*t0*86400);

figure
plot(t0,u20,'k',t0,u10,'r')
if makepdf
 print -dpdf thacker.pdf
 eval('!pdfcrop thacker.pdf thacker.pdf')
end

close(nc);




