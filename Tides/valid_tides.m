%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% valid_tides: compares CROCO tidal time series with tide-gauge data
%              and CROCO forcing data (TPXO), then computes harmonic
%              constituants using Rich Pawlowicz's T_TIDE package 
%              and intercompare model/data/forcing for the 
%              strongest model tidal constituents.
%
%  T_TIDE  -->  http://www.eos.ubc.ca/~rich/#T_Tide
%  R. Pawlowicz, B. Beardsley, and S. Lentz, 2002: Classical tidal harmonic 
%  analysis including error estimates in MATLAB using T_TIDE, Computers and 
%  Geosciences. 28, 929-937.

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
%  Copyright (c) 2005-2006 by Patrick Marchesiello and Pierrick Penven
%  e-mail:Pierrick.Penven@ird.fr
%
%  Updated    12-Aug-2011 by Patrick Marchesiello
%  Updated    29-Mar-2022 by Patrick Marchesiello
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all; 
close all;

crocotools_param

%==================== USER DEFINED VARIABLES ============================
%
% time series : start and duration ........
%
year=2004; month=1; day=3; % Start date of analysis
ndays=24;                  % Duration in days
%
% Output  ................
%
fid=1;                     % ouput type: 1=screen ; 2=outfile 
outfile='valid_tides.txt'; % output file name
t_tide_out='none';         % t_tide output: 'none' 'screen' or outfile
%
% MODEL ....................................
%
hisname =[CROCO_files_dir,'croco_his.nc'];
%
% TIDAL STATIONS .............................
%
nbstations=3;
%
% files
%
dir='Tides_Minh/';
stafile(1,:)=[dir,'HD2004.dat     '];
stafile(2,:)=[dir,'BL2004_orig.dat'];
stafile(3,:)=[dir,'HN2004.dat     '];
%
% positions (lat,lon)
%
staname(1,:)='Hon Dau     ';
lonsta(1)=106.80; latsta(1)=20.67; 
%
staname(2,:)='Bach Long Vi';
lonsta(2)=107.72; latsta(2)=20.13;
%
staname(3,:)='Hon Ngu     ';
lonsta(3)=105.77; latsta(3)=18.80;
% 
%================ END USERS DEFINED VARIABLES =========================
%
%........................................................................
% Set time references and new time array for interpolations
%........................................................................
%
torig=mjd(Yorig,1,1);               % origin time
t0=mjd(year,month,day)-torig;       % start time
interval = 1.;                      % time interval (hours)
timeref=[t0:interval/24:t0+ndays];  % new time array
%
%........................................................................
% Read tidal gauge data
%
% The columns are: year month day hours (min) SSH
%........................................................................
%
for k=1:nbstations
 tides=load(strtrim(stafile(k,:)));
 y_data=tides(:,1);
 m_data=tides(:,2);
 d_data=tides(:,3);
 h_data=tides(:,4)-timezone;  % convert to Greenwich hour
 if size(tides,2)==5,
  ssh_d(:,k)=tides(:,5);
  time_d(:,k)=mjd(y_data,m_data,d_data,h_data) - torig;
 else
  min_data=tides(:,5);
  ssh_d(:,k)=tides(:,6);
  time_d(:,k)=mjd(y_data,m_data,d_data,h_data) ...
                       + min_data/(60*24) - torig;
 end
 ssh_d(:,k)=ssh_d(:,k)-mean(ssh_d(:,k));
end
%
%........................................................................
% Read CROCO time array and grid
%........................................................................
%
nc=netcdf(hisname);
time=nc{'scrum_time'}(:)/86400;
close(nc)
T=length(time);
%
nc=netcdf(grdname);
lon=nc{'lon_rho'}(:);
lat=nc{'lat_rho'}(:);
h=nc{'h'}(:);
mask=nc{'mask_rho'}(:);
close(nc);
latmid=mean(mean(lat));
lonmin=min(min(lon));
lonmax=max(max(lon));
latmin=min(min(lat));
latmax=max(max(lat));
%
%....................................................................
% Find j,i indices for tide stations
%....................................................................
for k=1:nbstations
 I(k)=0;J(k)=0;ii=1;
 [j0 i0]=find(lat(1:end-5,1:end-5)< latsta(k) & ...
              lat(6:end  ,6:end  )>=latsta(k) & ...
              lon(1:end-5,1:end-5)< lonsta(k) & ...
              lon(6:end  ,6:end  )>=lonsta(k));
 for j=j0(1):j0(end); for i=i0(1):i0(end);
  if ii==1 & mask(j,i)==1, 
   J(k)=j; I(k)=i; ii=0;
  end
 end; end;
 if I(k)==0 | J(k)==0
  disp(' Warning: location of station not found, use model middle point ...')
  [M,L]=size(lon);
  I(k)=round(L/2);
  J(k)=round(M/2);
 end 
 if mask(J(k),I(k))==0,
  disp(' Warning: location of tide station in model land mask ...')
 end
 lonsta_m(k)=lon(J(k),I(k));
 latsta_m(k)=lat(J(k),I(k));
%
% disp(' ')
% disp(['Station ',staname(k,:),' position : '])
% disp(['model (i,j)     = ',int2str(I(k)),'  ',int2str(J(k))])
% disp(['model (lon,lat) = ',num2str(lonsta_m(k)),'  ',num2str(latsta_m(k))])
% disp(['real  (lon,lat) = ',num2str(lonsta(k)),'  ',num2str(latsta(k))])
end
%
%....................................................................
% Model tides: read CROCO surface elevation 
%....................................................................
%
nc=netcdf(hisname);
for k=1:nbstations
 ssh_m(:,k)=squeeze(nc{'zeta'}(:,J(k),I(k)));
 ssh_m(:,k)=ssh_m(:,k)-mean(ssh_m(:,k));
end
close(nc);
%
%.....................................................................
% Forcing tides (e;g. TPXO) 
% Rebuild from harmonics stored in croco forcing file
%.....................................................................
%
nc=netcdf(frcname);
for k=1:nbstations
 Tperiod(:,k)=squeeze(nc{'tide_period'}(:,J(k),I(k))./24);     % hours-->days
 Ephase(:,k) =squeeze(nc{'tide_Ephase'}(:,J(k),I(k))*pi/180);  % deg-->rad
 Eamp(:,k)   =squeeze(nc{'tide_Eamp'}(:,J(k),I(k)));           % m
end
cmpt=nc.components(:);
close(nc);
Nmax=length(Eamp); 
Ntides=min([Nmax Ntides]);
for i=1:Ntides
  components(3*i-2:3*i)=[cmpt(3*i-2:3*i-1),' '];
end
disp(' ')
disp(['Model forcing tidal components : ',components])
disp(' ')

ssh_f=zeros(T,nbstations);
for k=1:nbstations
 for itime=1:T
  for itide=1:Ntides
    omega=2*pi/Tperiod(itide,k)*time(itime); 
    ssh_f(itime,k)=ssh_f(itime,k) + ...
                   Eamp(itide,k).*cos(omega - Ephase(itide,k));
  end
 end
 ssh_f(:,k)=ssh_f(:,k)-mean(ssh_f(:,k));
end
%
%  Check harmonics
%
plotmap=0;
if plotmap,
 nc=netcdf(frcname);
 Tper=squeeze(nc{'tide_period'}(1)./24);     % hours-->days
 Epha0 =squeeze(nc{'tide_Ephase'}(1,:,:));   % deg
 Eamp0 =squeeze(nc{'tide_Eamp'}(1,:,:));     % m
 close(nc);
 mask(mask==0)=NaN;

 figure
 pcolor(lon,lat,Eamp0.*mask)
 shading flat
 colorbar
 hold on
 text(lonsta,latsta,'*','fontsize',30)
 hold off
 title('amplitude M2 (m)')
 axis([lonmin lonmax latmin latmax])
 caxis([0 2.5]);

 figure
 pcolor(lon,lat,Epha0.*mask)
 shading flat
 colorbar
 hold on
 text(lonsta,latsta,'*','fontsize',30)
 hold off
 title('Phase M2 (deg)')
 axis([lonmin lonmax latmin latmax])
 caxis([0 360]);
 return
end
%
%.....................................................................
% Interpolate time series on the same time vector timeref
%.....................................................................
%
method='linear';
for k=1:nbstations
 sshd(:,k)=interp1(time_d(:,k),ssh_d(:,k),timeref,method);
 sshf(:,k)=interp1(time,ssh_f(:,k),timeref,method);
 sshm(:,k)=interp1(time,ssh_m(:,k),timeref,method);
end
%
%==============================================================
% Make a few plots
%==============================================================
%
%------------------------------------------
%               Plot domain
%------------------------------------------
f=figure(1);
set(f,'Position',[200 300 500 500]);
themask=ones(size(mask));
themask(mask==0)=NaN;
domaxis=[min(min(lon)) max(max(lon)) min(min(lat)) max(max(lat))];
colaxis=[min(min(h)) max(max(h))];
colormap(cool)
fixcolorbar([0.25 0.05 0.5 0.03],colaxis,...
            'Topography',10)
width=1; height=0.8;
subplot('position',[0. 0.14 width height])
m_proj('mercator',...
       'lon',[domaxis(1) domaxis(2)],...
       'lat',[domaxis(3) domaxis(4)]);
m_pcolor(lon,lat,h.*themask); hold on;
shading flat; caxis(colaxis)
[C1,h1]=m_contour(lon,lat,h, ...
         [5 10 20 30 40 50 70 100 500 1000 2000 4000],'k');
clabel(C1,h1,'LabelSpacing',1000,'Rotation',0,'Color','r')
if ~isempty(coastfileplot)
  m_usercoast(coastfileplot,'color','r');
else
  m_gshhs_l('color','r');
  m_gshhs_l('speckle','color','r');
end
for k=1:nbstations
  m_text(lonsta_m(k),latsta_m(k),sprintf('+ %s', ...
         staname(k,:)),'fontsize',18);
end
m_grid('box','fancy','tickdir','in');
hold off
%export_fig -transparent valid_tide_domain.pdf
%
%-------------------------------------------
% Plot tidal series: model, data and forcing
%-------------------------------------------
f=figure(2);
nsta=min(4,nbstations);  % max of 4 stations
set(f,'Position',[200 400 1000 min(1000,nsta*400)]);
%
for k=1:nsta  
 subplot(nsta,1,k)
 a=plot(timeref,sshf(:,k),'k','linewidth',2); hold on;
 b=plot(timeref,sshd(:,k),'b','linewidth',2); hold on;
 c=plot(timeref,sshm(:,k),'r','linewidth',2); hold on;
 h_legend=legend([a b c],'TPXO','Tidal Gauge','Model');
 set(h_legend,'FontSize',15,'Location','Southeast');
 title(staname(k,:));
 ylabel('Tidal level [m]')
 xlabel('Time [days]')
 grid on
 hold off;
 axis([t0 t0+ndays -3.5 3.5]);
 set(gca,'fontsize',15)
end
%export_fig -transparent valid_tide_series.pdf
%
%===================================================================
% Tidal harminic analysis using T_TIDE
%===================================================================
%
disp(' ')
disp(' ====== TIDAL ANALYSIS ======')
disp(' ')
%
type_const=1; % 1 --> take constituents in forcing file 
              % 2 --> sort out significant model constituents
%
ianalysis=1; % 1
while ianalysis>0

 disp(' ')
 disp('Station for tidal analysis:')
 disp(['   EXIT PROGRAM : ',num2str(0)])
 disp(['   ALL Stations : ',num2str(nbstations+1)])
 for k=1:nbstations
   disp(['   Station ',staname(k,:),' : ',num2str(k)]) 
 end
 fstation=input(' .................. choose a number: ');
 disp(' ')
 if fstation==0, return; end
 if fstation==nbstations+1, 
   istation1=1;
   istation2=nbstations;
 else
   istation1=fstation;
   istation2=fstation;
 end
 A_errmd=zeros(nbstations,Ntides);
 P_errmd=zeros(nbstations,Ntides);
 A_errmf=zeros(nbstations,Ntides);
 P_errmf=zeros(nbstations,Ntides);
 
 clear Itides

 for istation=istation1:istation2;  % station loop ------

  sshm0=sshm(:,istation);
  sshd0=sshd(:,istation);
  sshf0=sshf(:,istation);
 
  disp(' ')
  disp('---------------------------------------------------------')
  disp('           Harmonic analysis : MODEL DATA                ')
  disp('---------------------------------------------------------')
  disp(' ')
 
  [tidestruc,xout]=t_tide(sshm0,'interval',interval,'start',datenum(Yorig,1,1), ...
                                'latitude',latmid,'output',t_tide_out);

  if type_const==2,
   % sort significant tidal constituents by order of intensity
   fsig=tidestruc.tidecon(:,1)>tidestruc.tidecon(:,2); % Significant peaks
   tmp=tidestruc.tidecon(fsig,1);
   [Atides,Itides]=sort(tmp,'descend'); 
   Nbtides=length(Atides);
   tmp=tidestruc.tidecon(fsig,3); Ptides=tmp(Itides);
   tmp=tidestruc.name(fsig,:); Ttides=tmp(Itides,:);
   Atides_m=Atides; Ptides_m=Ptides; Ttides_m=Ttides;
  elseif type_const==1,
   % take constituents of forcing file
   Itides(1:Ntides)=0;
   nconst=length(tidestruc.name);
   for i=1:nconst;
     for k=1:Ntides;
       if tidestruc.name(i,1:3)==components(3*k-2:3*k), 
         Itides(k)=i; 
       end
     end
   end
   Itides=Itides(Itides~=0);
   Atides_m=tidestruc.tidecon(Itides,1);
   Ptides_m=tidestruc.tidecon(Itides,3);
   Ttides_m=tidestruc.name(Itides,:);
   Nbtides=length(Atides_m);
   Ptides_m=zero22pi(Ptides_m+180)-180;
  end
%
% Plot frequency spectrum of tidal amplitude
%
  f=figure(3);
  set(f,'Units','normalized','Position',[0. 0. 0.4 0.4]);
  fsig=tidestruc.tidecon(:,1)>tidestruc.tidecon(:,2); % Significant peaks
  semilogy([tidestruc.freq(~fsig),tidestruc.freq(~fsig)]', ...
       [.0005*ones(sum(~fsig),1),tidestruc.tidecon(~fsig,1)]','.-r');
  line([tidestruc.freq(fsig),tidestruc.freq(fsig)]', ...
       [.0005*ones(sum(fsig),1),tidestruc.tidecon(fsig,1)]','marker','.','color','b');
  line(tidestruc.freq,tidestruc.tidecon(:,2),'linestyle',':','color',[0 .5 0]);
  set(gca,'ylim',[.0005 5],'xlim',[0 .5]);
  xlabel('frequency (cph)');
  text(tidestruc.freq,tidestruc.tidecon(:,1),tidestruc.name,'rotation',45,'vertical','base');
  ylabel('Amplitude (m)');
  text(.27,.4,'Analyzed lines with 95% significance level');
  text(.35,.2,'Significant Constituents','color','b');
  text(.35,.1,'Insignificant Constituents','color','r');
  text(.35,.05,'95% Significance Level','color',[0 .5 0]);
  title('MODEL')
 
  disp(' ')
  disp('---------------------------------------------------------')
  disp('          Hamonic analysis: TIDE-GAUGE DATA              ')
  disp('---------------------------------------------------------')
  disp(' ')

  [tidestruc,xout]=t_tide(sshd0,'interval',interval,'start',datenum(Yorig,1,1), ...
                                'latitude',latmid,'output',t_tide_out);

% Select model constituents
  Itides(1:Nbtides)=0;
  nconst=length(tidestruc.name);
  for i=1:nconst;
   for k=1:Nbtides;
     if tidestruc.name(i,1:3)==Ttides_m(k,1:3), Itides(k)=i; end
   end
  end
  Atides_d=tidestruc.tidecon(Itides,1);
  Ptides_d=tidestruc.tidecon(Itides,3);
  Ttides_d=tidestruc.name(Itides,:);
  Ptides_d=zero22pi(Ptides_d+180)-180;
%
  f=figure(4);
  set(f,'Units','normalized','Position',[0. 0. 0.4 0.4]);
  fsig=tidestruc.tidecon(:,1)>tidestruc.tidecon(:,2); % Significant peaks
  semilogy([tidestruc.freq(~fsig),tidestruc.freq(~fsig)]', ...
       [.0005*ones(sum(~fsig),1),tidestruc.tidecon(~fsig,1)]','.-r');
  line([tidestruc.freq(fsig),tidestruc.freq(fsig)]', ...
       [.0005*ones(sum(fsig),1),tidestruc.tidecon(fsig,1)]','marker','.','color','b');
  line(tidestruc.freq,tidestruc.tidecon(:,2),'linestyle',':','color',[0 .5 0]);
  set(gca,'ylim',[.0005 5],'xlim',[0 .5]);
  xlabel('frequency (cph)');
  text(tidestruc.freq,tidestruc.tidecon(:,1),tidestruc.name,'rotation',45,'vertical','base');
  ylabel('Amplitude (m)');
  text(.27,.4,'Analyzed lines with 95% significance level');
  text(.35,.2,'Significant Constituents','color','b');
  text(.35,.1,'Insignificant Constituents','color','r');
  text(.35,.05,'95% Significance Level','color',[0 .5 0]);
  title('DATA')

  disp(' ')
  disp('---------------------------------------------------------')
  disp('      Harmonic analysis : MODEL FORCING DATA (TPXO)      ')
  disp('---------------------------------------------------------')
  disp(' ')

  [tidestruc,xout]=t_tide(sshf0,'interval',interval,'start',datenum(Yorig,1,1), ...
                                'latitude',latmid,'output',t_tide_out);

% Select model constituents
  Itides(1:Nbtides)=0;
  nconst=length(tidestruc.name);
  for i=1:nconst;
   for k=1:Nbtides;
     if tidestruc.name(i,1:3)==Ttides_m(k,1:3), Itides(k)=i; end
   end
  end
  Atides_f=tidestruc.tidecon(Itides,1);
  Ptides_f=tidestruc.tidecon(Itides,3);
  Ttides_f=tidestruc.name(Itides,:);
  Ptides_f=zero22pi(Ptides_f+180)-180;

  f=figure(5);
  set(f,'Units','normalized','Position',[0. 0. 0.4 0.4]);
  fsig=tidestruc.tidecon(:,1)>tidestruc.tidecon(:,2); % Significant peaks
  semilogy([tidestruc.freq(~fsig),tidestruc.freq(~fsig)]', ...
           [.0005*ones(sum(~fsig),1),tidestruc.tidecon(~fsig,1)]','.-r');
  line([tidestruc.freq(fsig),tidestruc.freq(fsig)]', ...
       [.0005*ones(sum(fsig),1),tidestruc.tidecon(fsig,1)]','marker','.','color','b');
  line(tidestruc.freq,tidestruc.tidecon(:,2),'linestyle',':','color',[0 .5 0]);
  set(gca,'ylim',[.0005 5],'xlim',[0 .5]);
  xlabel('frequency (cph)');
  text(tidestruc.freq,tidestruc.tidecon(:,1),tidestruc.name,'rotation',45,'vertical','base');
  ylabel('Amplitude (m)');
  text(.27,.4,'Analyzed lines with 95% significance level');
  text(.35,.2,'Significant Constituents','color','b');
  text(.35,.1,'Insignificant Constituents','color','r');
  text(.35,.05,'95% Significance Level','color',[0 .5 0]);
  title('FORCING (TPXO)')

  disp(' ')
  disp('------------------------------------------------------------------')
  disp(['  Statistical comparison of principal constituants at : ',staname(istation,:)])
  disp('------------------------------------------------------------------')
  disp(['  STD  data          = ',num2str(std(sshd0))]);
  disp(['  RMSE model/data    = ',num2str(rmse(sshm0,sshd0))]);
  disp(['  RMSE model/forcing = ',num2str(rmse(sshm0,sshf0))]);
  disp(' ')

  Atides_errmd=Atides_m-Atides_d;
  Ptides_errmd=Ptides_m-Ptides_d;
  Ptides_errmd=zero22pi(Ptides_errmd+180)-180;
  Atides_errmf=Atides_m-Atides_f;
  Ptides_errmf=Ptides_m-Ptides_f;
  Ptides_errmf=zero22pi(Ptides_errmf+180)-180;
 
  if fid>1, fid = fopen(outfile,'w'); end;

  fprintf(fid,'\n           Model       model/data err         model/forcing err');
  fprintf(fid,'\n tide     Amp[cm]    Amp[cm]  Phase[deg]    Amp[cm]  Phase[deg]\n');
  for i=1:Nbtides
   fprintf(fid,' ');
   fprintf(fid,'%s %10.1f %10.1f %10.1f %12.1f %10.1f\n',Ttides_m(i,:),100*Atides_m(i), ...
            100*Atides_errmd(i),Ptides_errmd(i),100*Atides_errmf(i),Ptides_errmf(i));
  end
  disp(' ')
%
%------------------------------------------
%  TEST OF T_TIDE ON FORCING DATA TPXO
%------------------------------------------
%
% get t-tide constituents
% then recover original TPXO tidal amp and phase and refer phase to
% central time of time-series as in t-tide (when no start-time is given)
%
  testforcing=1;
  if type_const==1 & testforcing,

    [tidestruc,xout]=t_tide(sshf0,'interval',interval,'output',t_tide_out);

    Itides(1:Nbtides)=0;
    nconst=length(tidestruc.name);
    for k=1:Nbtides;
      for i=1:nconst;
        if tidestruc.name(i,1:3)==Ttides_m(k,1:3), Itides(k)=i; end
      end
    end
    Atides_f=tidestruc.tidecon(Itides,1);
    Ptides_f=tidestruc.tidecon(Itides,3);
    Ptides_f=zero22pi(Ptides_f+180)-180;

    for k=1:Nbtides;
     for i=1:Ntides;
       if Ttides_m(k,1:2)==components(3*i-2:3*i-1),
         Eampsta(k)=Eamp(i,istation);
         Ephasesta(k)=Ephase(i,istation)*180/pi;
         central_time=(timeref(end)-timeref(1))/2+timeref(1);
         omega=mod(360*central_time/Tperiod(i,istation),360);
         Ephasesta(k)=Ephasesta(k)-omega;
       end
     end
    end
    Ephasesta=zero22pi(Ephasesta+180)-180;
    disp(' ')
    disp(' Test T_TIDE decomposition on TPXO: ')
    disp(' ================================== ')
    fprintf(fid,'\n           Amplitude [cm]         Phase [deg]');
    fprintf(fid,'\n tide     Original  T_tide     Original  T_tide\n');
    for i=1:Nbtides
     fprintf(fid,' ');
     fprintf(fid,'%s %10.1f %8.1f %12.1f %8.1f\n', ...
         Ttides_f(i,:),100*Eampsta(i),100*Atides_f(i),Ephasesta(i),Ptides_f(i));
    end
    disp(' ')
  end
%
% Store errors for each station to compute RMSE
%
  A_errmd(istation,1:Nbtides)=Atides_errmd;
  P_errmd(istation,1:Nbtides)=Ptides_errmd;
  A_errmf(istation,1:Nbtides)=Atides_errmf;
  P_errmf(istation,1:Nbtides)=Ptides_errmf;

 end % istation
% 
 if fstation==nbstations+1,
  disp(' ')
  disp('===================================================================')
  disp('        RMSE of tidal phase and amplitude over all stations :      ')
  disp('===================================================================')
  fprintf(fid,'\n            RMSE model/data        RMSE model/forcing');
  fprintf(fid,'\n tide      Amp[cm]  Phase[deg]    Amp[cm]  Phase[deg]\n');
  for i=1:Nbtides
    RMSE_Amd=sqrt(mean(A_errmd(:,i).^2));
    RMSE_Pmd=sqrt(mean(P_errmd(:,i).^2));
    RMSE_Amf=sqrt(mean(A_errmf(:,i).^2));
    RMSE_Pmf=sqrt(mean(P_errmf(:,i).^2));
    fprintf(fid,' ');
    fprintf(fid,'%s %10.1f %10.1f %12.1f %10.1f\n',Ttides_m(i,:), ...
            100*RMSE_Amd,RMSE_Pmd,100*RMSE_Amf,RMSE_Pmf);
  end
  disp(' ')
 end

 if fid>1, fclose(fid); end

end % ianalysis



