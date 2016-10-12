function clm_tides(grdname,frcname,Ntides,Ymin,Mmin,Dmin,Hmin,...
                   Min_min,Smin,Yorig,lon0,lat0,Z0)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Reproduce CROCO clm_tides.F for tidal SSH. 
%  Takes care of 'Yorig' time 
%
%  Further Information:  
%  http://www.croco-ocean.org
%  
%  This file is part of CROCOTOOLS
%
%  CROCOTOOLS is free software; you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published
%  by the Free Software Foundation; either version 2 of the License,
%  or (at your option) any later version.
%
%  CROCOTOOLS is distributed in the hope that it will be useful, but
%  WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program; if not, write to the Free Software
%  Foundation, Inc., 59 Temple Place, Suite 330, Boston,
%  MA  02111-1307  USA
%
%  Adadpted from time_series_tide.m made by Patrick Marchesiello
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Reproduce CROCO 'Yorig' time
%
date0=datenum(Ymin,Mmin,Dmin,Hmin,Min_min,Smin)-datenum(Yorig,1,1);
time=date0*24*3600;
dt=3600;
ntimes=480;
%
deg2rad=pi/180.;
%
% Read the grid
%
nc=netcdf(grdname,'r');
lon=nc{'lon_rho'}(:);
lat=nc{'lat_rho'}(:);
close(nc)
%
% Find j,i indices for the tidegauge
%
[J,I]=find((lat(1:end-1,1:end-1)<lat0 & lat(2:end,2:end)>=lat0 &...
            lon(1:end-1,1:end-1)<lon0 & lon(2:end,2:end)>=lon0)==1);
if isempty(I) |  isempty(J)
  disp('Warning: tide gauge place not found')
  [M,L]=size(lon);
  I=round(L/2);
  J=round(M/2);
end 
disp(['I = ',int2str(I),' J = ',int2str(J)])
lon1=lon(J,I);
lat1=lat(J,I);
disp(['lon1 = ',num2str(lon1),' lat1 = ',num2str(lat1)])
%
nc=netcdf(frcname,'r');
Tperiod=nc{'tide_period'}(:);
SSH_Tamp=nc{'tide_Eamp'}(:,J,I);
SSH_Tphase=nc{'tide_Ephase'}(:,J,I);
close(nc)
%
Tperiod=Tperiod*3600.;
SSH_Tphase=SSH_Tphase*deg2rad;
%
ramp=1.;
%
%---------------------------------------------------------------------
%  Add tidal elevation (m) to sea surface height climatology.
%---------------------------------------------------------------------
%
for nstep=1:ntimes
  Etide=0;
  for itide=1:Ntides
    if (Tperiod(itide)>0.)
      omega=2.*pi*time/Tperiod(itide);
      Etide=Etide+ramp*SSH_Tamp(itide).*...
                  cos(omega-SSH_Tphase(itide));
    end
  end
%
  mytime(nstep)=time;
  ssh(nstep)=Etide;
  time=time+dt;
end
%
% Plot
%
days=mytime/(24*3600);
day0=min(min(days));
disp(datestr(day0+datenum(Yorig,1,1)))
plot(days-day0,ssh+Z0)
axis([0 20 0 2*Z0])
set(gca,'Xtick',[0:5:20])
ylabel('Sea level [m]')
xlabel('Days since initialisation')
title(['lon=',num2str(lon1),...
       ', lat=',num2str(lat1),', Initialisation: ',...
       datestr(day0+datenum(Yorig,1,1))])
grid on
