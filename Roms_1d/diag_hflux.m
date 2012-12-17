%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  plot 1dv heat fluxes from ROMS-1D
%
%  j: time in days
%  z: vertical coordinate
%  var: 1DV variable
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all
close all

romstools_param

INDIR = FORDIR;
ROMS_FILES = 'OUT_CONTROL_VOCALS/';

dstr = 2; dend = 360;
windowSize = 8;
%===========================================
%
% call air-sea constants
as_consts

%
%===========================================
% READ DATA
%===========================================
%
% reads ascii station file
%
fid=fopen('forces.data','r');
  text0 = fgets(fid);
  text0 = fgets(fid);
  text0 = fgets(fid);
lon        = fscanf(fid,'%f',1);
lat        = fscanf(fid,'%f\n',1);
  text0 = fgets(fid);
sustr_clm  = fscanf(fid,'%f\n',12);
  text0 = fgets(fid);
svstr_clm  = fscanf(fid,'%f\n',12);
  text0 = fgets(fid);
shflux_clm = fscanf(fid,'%f\n',12);
  text0 = fgets(fid);
swflux_clm = fscanf(fid,'%f\n',12);
  text0 = fgets(fid);
sst_clm    = fscanf(fid,'%f\n',12);
  text0 = fgets(fid);
dqdsst_clm = fscanf(fid,'%f\n',12);
  text0 = fgets(fid);
swrad_clm  = fscanf(fid,'%f\n',12);
  text0 = fgets(fid);
upwi_clm   = fscanf(fid,'%f\n',12);
  text0 = fgets(fid);
radsw_clm  = fscanf(fid,'%f\n',12);
  text0 = fgets(fid);
radlw_clm  = fscanf(fid,'%f\n',12);
  text0 = fgets(fid);
prate_clm  = fscanf(fid,'%f\n',12);
  text0 = fgets(fid);
tair_clm   = fscanf(fid,'%f\n',12);
  text0 = fgets(fid);
rhum_clm   = fscanf(fid,'%f\n',12);
  text0 = fgets(fid);
uwnd_clm   = fscanf(fid,'%f\n',12);
  text0 = fgets(fid);
vwnd_clm   = fscanf(fid,'%f\n',12);
fclose(fid);
%
% reads Oberhuber files
%
fname='/Users/pmarches/Roms_tools/OBERHUBER/lhfx.cdf';
[J,I]=find_station(fname,lon,lat);
nc=netcdf(fname);
lh_clm=nc{'lhfx'}(:,J,I);
tclm=nc{'T'}(:);
close(nc);
%
fname='/Users/pmarches/Roms_tools/OBERHUBER/shfx.cdf';
[J,I]=find_station(fname,lon,lat);
nc=netcdf(fname);
sh_clm=nc{'shfx'}(:,J,I);
close(nc);
%
fname='/Users/pmarches/Roms_tools/OBERHUBER/sst.cdf';
[J,I]=find_station(fname,lon,lat);
nc=netcdf(fname);
sst_clm2=nc{'sst'}(:,J,I);
close(nc);

%
% reads COADS files
%
fname='/Users/pmarches/Roms_tools/COADS05/latent3.cdf';
[J,I]=find_station(fname,lon,lat);
nc=netcdf(fname);
scale=0.00626;
offset=97.64;
lh_clm2=scale*nc{'latent3'}(:,J,I)+offset;
lh_clm2=-lh_clm2;
close(nc);
%
fname='/Users/pmarches/Roms_tools/COADS05/sensib3.cdf';
[J,I]=find_station(fname,lon,lat);
nc=netcdf(fname);
scale=0.0036573;
offset=57.16;
sh_clm2=scale*nc{'sensib3'}(:,J,I)+offset;
sh_clm2=-sh_clm2;
close(nc);

%
% ROMS grid
%
j=load([ROMS_FILES,'days.dat']);
z=load([ROMS_FILES,'z.dat']);
zbl=load([ROMS_FILES,'zbl.dat']);
tstr=min(find(j>dstr));
tend=min(find(j>dend));
j=j./30;
dstr=dstr/30; dend=dend/30;

%
% Complete fluxes
%
radlw_clm = radlw_clm - emiss_lw*sigmaSB*(sst_clm+273.16).^4;
wstr_clm = sqrt(sustr_clm.^2 + svstr_clm.^2);
shflux_clm2 = swrad_clm+radlw_clm+lh_clm+sh_clm;
%
%===========================================
% PLOT HEAT FLUXES
%===========================================
figure('position',[5 5 700 500]);
%
% TSK
%
fname=[ROMS_FILES,'tsk.dat'];
var=load(fname);
tsk=var';
mtsk=filter(ones(1,windowSize)/windowSize,1,tsk);
stdtsk=std(tsk-mtsk);
fname=[ROMS_FILES,'t01.dat'];
var=load(fname);
temp=var';
sst=temp(end,:);
msst=filter(ones(1,windowSize)/windowSize,1,sst);
stdsst=std(sst-msst);
%
subplot(2,2,1)
h=plot(j,mtsk,'b',j,msst,'g',tclm,sst_clm,'r*',tclm,sst_clm2,'g*'); hold on
legend('mean TSK','mean SST','sst COADS','sst OBERHUBER')
set(h,'LineWidth',2)
plot(j,mtsk-stdtsk,'b--',j,mtsk+stdtsk,'b--'); hold on
plot(j,msst-stdsst,'g--',j,msst+stdsst,'g--'); hold off
xlabel('Time [Month]')
title('SST SKIN [^{\circ} C]')
axis([dstr dend min(sst_clm)-1 max(sst_clm)+1]);
%
% Total HFlux
%
fname=[ROMS_FILES,'sen.dat'];
var=load(fname);
fname=[ROMS_FILES,'lat.dat'];
var=var+load(fname);
fname=[ROMS_FILES,'rsw.dat'];
var=var+load(fname);
fname=[ROMS_FILES,'rlw.dat'];
var=var+load(fname);
var=var';
mvar=filter(ones(1,windowSize)/windowSize,1,var);
stdvar=std(var-mvar);
%
subplot(2,2,2)
h1=plot(j,mvar,'k'); hold on
h2=plot(tclm,shflux_clm,'r*',tclm,shflux_clm2,'g*'); hold on
set(h1,'LineWidth',2)
plot(j,mvar-stdvar,'k--',j,mvar+stdvar,'k--'); hold off
grid on
xlabel('Time [Month]')
title('Total HF [W/m^2]')
axis([dstr dend -Inf Inf]);
%
% WSTR
%
fname=[ROMS_FILES,'sus.dat'];
sus=load(fname);
fname=[ROMS_FILES,'svs.dat'];
svs=load(fname);
var=sqrt(sus.^2+svs.^2);
var=var';
mvar=filter(ones(1,windowSize)/windowSize,1,var);
stdvar=std(var-mvar);
%
subplot(2,2,3)
h1=plot(j,mvar,'k'); hold on
h2=plot(tclm,wstr_clm,'r*'); hold on
set(h1,'LineWidth',2)
plot(j,mvar-stdvar,'k--',j,mvar+stdvar,'k--'); hold off
grid on
xlabel('Time [Month]')
title('WSTR [N/m^2]')
axis([dstr dend -Inf Inf]);
%
% TAIR
%
subplot(2,2,4)
h2=plot(tclm,tair_clm-sst_clm,'r*'); hold on
grid on
xlabel('Time [Days]')
title('TAIR-SST [degC]')
axis([dstr dend -Inf Inf]);

%----------------------------------------------
figure('position',[5 5 700 500]);
%
%
% LH
%
fname=[ROMS_FILES,'lat.dat'];
var=load(fname);
var=var';
mvar=filter(ones(1,windowSize)/windowSize,1,var);
stdvar=std(var-mvar);
%
subplot(2,2,1)
h1=plot(j,mvar,'k'); hold on
h2=plot(tclm,lh_clm,'r*',tclm,lh_clm2,'g*'); hold on
set(h1,'LineWidth',2)
plot(j,mvar-stdvar,'k--',j,mvar+stdvar,'k--'); hold off
grid on
xlabel('Time [Month]')
title('LH [W/m^2]')
axis([dstr dend -Inf Inf]);
%
% SH
%
fname=[ROMS_FILES,'sen.dat'];
var=load(fname);
var=var';
mvar=filter(ones(1,windowSize)/windowSize,1,var);
stdvar=std(var-mvar);
%
subplot(2,2,2)
h1=plot(j,mvar,'k'); hold on
h2=plot(tclm,sh_clm,'r*',tclm,sh_clm2,'g*'); hold on
set(h1,'LineWidth',2)
plot(j,mvar-stdvar,'k--',j,mvar+stdvar,'k--'); hold off
grid on
xlabel('Time [Month]')
title('SH [W/m^2]')
axis([dstr dend -Inf Inf]);
%
% SW
%
fname=[ROMS_FILES,'rsw.dat'];
var=load(fname);
var=var';
mvar=filter(ones(1,windowSize)/windowSize,1,var);
stdvar=std(var-mvar);
%
subplot(2,2,3)
h1=plot(j,mvar,'k'); hold on
h2=plot(tclm,radsw_clm,'r*'); hold on
set(h1,'LineWidth',2)
plot(j,mvar-stdvar,'k--',j,mvar+stdvar,'k--'); hold off
grid on
xlabel('Time [Month]')
title('SW [W/m^2]')
axis([dstr dend -Inf Inf]);
%
% LW
%
fname=[ROMS_FILES,'rlw.dat'];
var=load(fname);
var=var';
mvar=filter(ones(1,windowSize)/windowSize,1,var);
stdvar=std(var-mvar);
%
subplot(2,2,4)
h1=plot(j,mvar,'k'); hold on
h2=plot(tclm,radlw_clm,'r*'); hold on
set(h1,'LineWidth',2)
plot(j,mvar-stdvar,'k--',j,mvar+stdvar,'k--'); hold off
grid on
xlabel('Time [Month]')
title('LW [W/m^2]')
axis([dstr dend -Inf Inf]);

return

