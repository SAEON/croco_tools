function [] = add_tidal_data(tidename,gname,fname,Ntides,tidalrank,...
                             year,month,day,hr,minute,second,coastfileplot)
% 
% Add tidal forcing in interannual forcing file 
% of the type roms_frc_NCEP2_Y--M--.nc
%
%   tidename : TPXO file name
%   gname : roms grid file
%   fname : roms forcing (frc) file
%   Ntides : number of tidala waves
%   tidal rank : order from the rank in the TPXO file
%   year,month,day,hr,minute,second : start time of simulation
%                                     for nodal correction
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Get start time of simulation in fractional mjd for nodal correction
%
date_mjd=mjd(year,month,day);
[pf,pu,t0,phase_mkB]=egbert_correc(date_mjd,hr,minute,second);
deg=180.0/pi;
%
%  Read in ROMS grid.
%
disp('Reading ROMS grid parameters ...');
nc=netcdf(gname);
rlon=nc{'lon_rho'}(:);
rlat=nc{'lat_rho'}(:);
rangle=nc{'angle'}(:);
h=nc{'h'}(:);
rmask=nc{'mask_rho'}(:);
close(nc)
minlat=(min(min(rlat)));
maxlat=(max(max(rlat)));
minlon=(min(min(rlon)));
maxlon=(max(max(rlon)));
%
% Read TPX grid
%
nctides=netcdf(tidename);
periods=nctides{'periods'}(:);
xr=nctides{'lon_r'}(:);
yr=nctides{'lat_r'}(:);
xu=nctides{'lon_u'}(:);
yu=nctides{'lat_u'}(:);
xv=nctides{'lon_v'}(:);
yv=nctides{'lat_v'}(:);
Lr=length(xr);
Mr=length(yr);
Lu=length(xu);
Mu=length(yu);
Lv=length(xv);
Mv=length(yv);
Nmax=length(periods);
Ntides=min([Nmax Ntides]);
cmpt=nc.components(:);
%
% Prepare the forcing file
%
for i=1:Ntides
  components(3*i-2:3*i)=[cmpt(3*tidalrank(i)-2:3*tidalrank(i)-1),' '];
end
disp(['Tidal components : ',components])
nc_add_tides(fname,Ntides,date_mjd,components)
ncfrc=netcdf(fname,'write');
%
% Get the subgrids
%
iminr=max([max(find(xr<minlon))-1 1]);
imaxr=min([min(find(xr>maxlon))+1 Lr]);
jminr=max([max(find(yr<minlat))-1 1]);
jmaxr=min([min(find(yr>maxlat))+1 Mr]);
iminu=max([max(find(xu<minlon))-1 1]);
imaxu=min([min(find(xu>maxlon))+1 Lu]);
jminu=max([max(find(yu<minlat))-1 1]);
jmaxu=min([min(find(yu>maxlat))+1 Mu]);
iminv=max([max(find(xv<minlon))-1 1]);
imaxv=min([min(find(xv>maxlon))+1 Lv]);
jminv=max([max(find(yv<minlat))-1 1]);
jmaxv=min([min(find(yv>maxlat))+1 Mv]);
%
xr=xr(iminr:imaxr);
yr=yr(jminr:jmaxr);
xu=xu(iminu:imaxu);
yu=yu(jminu:jmaxu);
xv=xv(iminv:imaxv);
yv=yv(jminv:jmaxv);
[Xr,Yr]=meshgrid(xr,yr);
[Xu,Yu]=meshgrid(xu,yu);
[Xv,Yv]=meshgrid(xv,yv);
%
% Get TPXO topography
%
hz=nctides{'h'}(:);
hu=NaN*hz;
hv=NaN*hz;
hu(:,2:end)=0.5*(hz(:,2:end)+hz(:,1:end-1));
hv(2:end,:)=0.5*(hz(2:end,:)+hz(1:end-1,:));
hz=hz(jminr:jmaxr,iminr:imaxr);
hu=hu(jminu:jmaxu,iminu:imaxu);
hv=hv(jminv:jmaxv,iminv:imaxv);
hz(hz==0)=NaN;
hu(hu==0)=NaN;
hv(hv==0)=NaN;
%
%
for itide=1:Ntides
  it=tidalrank(itide);
  disp(['Processing tide : ',num2str(itide),' of ',num2str(Ntides)])
  ncfrc{'tide_period'}(it)=periods(itide);
%
% Get the phase corrections
%
  correc_amp=pf(itide);
  correc_phase=-phase_mkB(itide)-pu(itide)+360.*t0./periods(itide);           
%
% Process the surface elevation
%
  disp('  ssh...')
  ur=nctides{'ssh_r'}(itide,jminr:jmaxr,iminr:imaxr);
  ur(ur==0)=NaN;  
  ur(isnan(ur))=griddata(Xr(isfinite(ur)),Yr(isfinite(ur)),...
                         ur(isfinite(ur)),...
                         Xr(isnan(ur)),Yr(isnan(ur)),'nearest');
  ur=interp2(xr,yr,ur,rlon,rlat,'cubic');
  ui=nctides{'ssh_i'}(itide,jminr:jmaxr,iminr:imaxr);
  ui(ui==0)=NaN;
  ui(isnan(ui))=griddata(Xr(isfinite(ui)),Yr(isfinite(ui)),...
                         ui(isfinite(ui)),...
                         Xr(isnan(ui)),Yr(isnan(ui)),'nearest');
  ui=interp2(xr,yr,ui,rlon,rlat,'cubic');
  ei=complex(ur,ui);
  ncfrc{'tide_Ephase'}(it,:,:)=mod(-deg*angle(ei)+correc_phase,360.0);     
  ncfrc{'tide_Eamp'}(it,:,:)=abs(ei)*correc_amp;
%
% Process U
%
  disp('  u...')
  ur=nctides{'u_r'}(itide,jminu:jmaxu,iminu:imaxu);
  ur(ur==0)=NaN;
  ur=ur./hu;
  ur(isnan(ur))=griddata(Xu(isfinite(ur)),Yu(isfinite(ur)),...
                         ur(isfinite(ur)),...
                         Xu(isnan(ur)),Yu(isnan(ur)),'nearest');
  ur=interp2(xu,yu,ur,rlon,rlat,'cubic');
  ui=nctides{'u_i'}(itide,jminu:jmaxu,iminu:imaxu);
  ui(ui==0)=NaN;
  ui=ui./hu;
  ui(isnan(ui))=griddata(Xu(isfinite(ui)),Yu(isfinite(ui)),...
                         ui(isfinite(ui)),...
                         Xu(isnan(ui)),Yu(isnan(ui)),'nearest');
  ui=interp2(xu,yu,ui,rlon,rlat,'cubic');
  ei=complex(ur,ui);
  upha=mod(-deg*angle(ei)+correc_phase,360.0); 
  uamp=abs(ei)*correc_amp;
%
% Process V
%
  disp('  v...')
  vr=nctides{'v_r'}(itide,jminv:jmaxv,iminv:imaxv);
  vr(vr==0)=NaN;
  vr=vr./hv;
  vr(isnan(vr))=griddata(Xv(isfinite(vr)),Yv(isfinite(vr)),...
                         vr(isfinite(vr)),...
                         Xv(isnan(vr)),Yv(isnan(vr)),'nearest');
  vr=interp2(xv,yv,vr,rlon,rlat,'cubic');
  vi=nctides{'v_i'}(itide,jminv:jmaxv,iminv:imaxv);
  vi(vi==0)=NaN;
  vi=vi./hv;
  vi(isnan(vi))=griddata(Xv(isfinite(vi)),Yv(isfinite(vi)),...
                         vi(isfinite(vi)),...
                         Xv(isnan(vi)),Yv(isnan(vi)),'nearest');
  vi=interp2(xv,yv,vi,rlon,rlat,'cubic');
  ei=complex(vr,vi);
  vpha=mod(-deg*angle(ei)+correc_phase,360.0); 
  vamp=abs(ei)*correc_amp;
%
% Convert to tidal ellipses
%
  disp('  Convert to tidal ellipse parameters...')
  [major,eccentricity,inclination,phase]=ap2ep(uamp,upha,vamp,vpha);
  ncfrc{'tide_Cmin'}(it,:,:)=major.*eccentricity;
  ncfrc{'tide_Cmax'}(it,:,:)=major;
  ncfrc{'tide_Cangle'}(it,:,:)=inclination;
  ncfrc{'tide_Cphase'}(it,:,:)=phase;
%
end
%
% Close the files
%
close(nctides)
close(ncfrc)
%
% Plot
%
plot_tide(gname,fname,1,1,3,coastfileplot)
