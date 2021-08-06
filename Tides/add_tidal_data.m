function [] = add_tidal_data(tidename,grdname,frcname,Ntides,tidalrank,...
                             Yorig,year,month,coastfileplot, ...
                             sal_tides,salname)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Add tidal forcing in interannual forcing file 
% of the type croco_frc_NCEP2_Y--M--.nc
%
%   tidename : TPXO file name
%   grdname : croco grid file
%   frcname : croco forcing (frc) file
%   Ntides : number of tidala waves
%   tidal rank : order from the rank in the TPXO file
%   Yorig : time origin of simulation for phase correction
%   year,month : current time of simulation for nodal correction
%
%   last modified: P. Marchesiello, Oct 2020 (lon<0 + fix pot. tides)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
interp_method = 'linear';
makeplot      = 1;

pot_tides=1;            % Potential tidal forcing
if nargin < 10          % if Self-Attraction/Loading not  
  sal_tides=0;          % given, set to none
end
%
% Get time of simulation in fractional mjd for nodal correction
%
date_mjd=mjd(year,month,1);
[pf,pu,t0,phase_mkB]=egbert_correc(date_mjd,0.,0.,0.);
deg=180.0/pi;

rad=pi/180.0;
%
% Add a phase correction to be consistent with the 'Yorig' time
%
t0=t0-24.*mjd(Yorig,1,1);
%
%  Read in CROCO grid.
%
disp('Reading CROCO grid parameters ...');
nc=netcdf(grdname,'r');
rlon=nc{'lon_rho'}(:);
rlat=nc{'lat_rho'}(:);
rangle=nc{'angle'}(:);
h=nc{'h'}(:);
rmask=nc{'mask_rho'}(:);
close(nc)
rlon(rlon<0)=rlon(rlon<0)+360;
minlat=(min(min(rlat)));
maxlat=(max(max(rlat)));
minlon=(min(min(rlon)));
maxlon=(max(max(rlon)));
%
% Read TPX grid
%
nctides=netcdf(tidename,'r');
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
cmpt=nctides.components(:);
%
% Prepare the forcing file
%
for i=1:Ntides
  components(3*i-2:3*i)=[cmpt(3*tidalrank(i)-2:3*tidalrank(i)-1),' '];
end
disp(['Tidal components : ',components])
nc_add_tides(frcname,Ntides,date_mjd,components)
ncfrc=netcdf(frcname,'write');
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
hz(hz<10)=NaN;
hu(hu<10)=NaN;
hv(hv<10)=NaN;
%
% Tidal potential:
%
%    amplitudes and elasticity factors for:
%        M2 S2 N2 K2 K1 O1 P1 Q1 Mf Mm
%
A=[0.242334 0.113033 0.046398 0.030704 ...
   0.141565 0.100514 0.046843 0.019256 ...
   0.041742 0.022026];                       % Amplitude (Schwiderski, 1978)
B=[0.693 0.693 0.693 0.693 ...
   0.736 0.695 0.706 0.695 ...
   0.693 0.693];                             % Elasticity factor
%
coslat2=cos(rad*rlat).^2;                    % Phase arrays
sin2lat=sin(2.*rad*rlat);
%
%----------------------------------------------------------
%               Loop on tidal components
%----------------------------------------------------------
%
for itide=1:Ntides
  it=tidalrank(itide);
  disp(['Processing tide : ',num2str(itide),' of ',num2str(Ntides)])
  ncfrc{'tide_period'}(itide)=periods(it);
%
% Get the phase corrections
%
  correc_amp=pf(it);
  correc_phase=-phase_mkB(it)-pu(it)+360.*t0./periods(it);
%
% Process the surface elevation
%
  disp('  ssh...')
  ur=nctides{'ssh_r'}(it,jminr:jmaxr,iminr:imaxr);
  ur(ur==0)=NaN;  
  ur(isnan(ur))=griddata(Xr(isfinite(ur)),Yr(isfinite(ur)),...
                         ur(isfinite(ur)),...
                         Xr(isnan(ur)),Yr(isnan(ur)),'nearest');
  ur=interp2(xr,yr,ur,rlon,rlat,interp_method);
  ui=nctides{'ssh_i'}(it,jminr:jmaxr,iminr:imaxr);
  ui(ui==0)=NaN;
  ui(isnan(ui))=griddata(Xr(isfinite(ui)),Yr(isfinite(ui)),...
                         ui(isfinite(ui)),...
                         Xr(isnan(ui)),Yr(isnan(ui)),'nearest');
  ui=interp2(xr,yr,ui,rlon,rlat,interp_method);
  ei=complex(ur,ui);
  ncfrc{'tide_Ephase'}(itide,:,:)=mod(-deg*angle(ei)+correc_phase,360.0);
  ncfrc{'tide_Eamp'}(itide,:,:)=abs(ei)*correc_amp;
%
% Process U
%
  disp('  u...')
  ur=nctides{'u_r'}(it,jminu:jmaxu,iminu:imaxu);
  ur(ur==0)=NaN;
  ur=ur./hu;
  ur(isnan(ur))=griddata(Xu(isfinite(ur)),Yu(isfinite(ur)),...
                         ur(isfinite(ur)),...
                         Xu(isnan(ur)),Yu(isnan(ur)),'nearest');
  ur=interp2(xu,yu,ur,rlon,rlat,interp_method);
  ui=nctides{'u_i'}(it,jminu:jmaxu,iminu:imaxu);
  ui(ui==0)=NaN;
  ui=ui./hu;
  ui(isnan(ui))=griddata(Xu(isfinite(ui)),Yu(isfinite(ui)),...
                         ui(isfinite(ui)),...
                         Xu(isnan(ui)),Yu(isnan(ui)),'nearest');
  ui=interp2(xu,yu,ui,rlon,rlat,interp_method);
  ei=complex(ur,ui);
  upha=mod(-deg*angle(ei)+correc_phase,360.0); 
  uamp=abs(ei)*correc_amp;
%
% Process V
%
  disp('  v...')
  vr=nctides{'v_r'}(it,jminv:jmaxv,iminv:imaxv);
  vr(vr==0)=NaN;
  vr=vr./hv;
  vr(isnan(vr))=griddata(Xv(isfinite(vr)),Yv(isfinite(vr)),...
                         vr(isfinite(vr)),...
                         Xv(isnan(vr)),Yv(isnan(vr)),'nearest');
  vr=interp2(xv,yv,vr,rlon,rlat,interp_method);
  vi=nctides{'v_i'}(it,jminv:jmaxv,iminv:imaxv);
  vi(vi==0)=NaN;
  vi=vi./hv;
  vi(isnan(vi))=griddata(Xv(isfinite(vi)),Yv(isfinite(vi)),...
                         vi(isfinite(vi)),...
                         Xv(isnan(vi)),Yv(isnan(vi)),'nearest');
  vi=interp2(xv,yv,vi,rlon,rlat,interp_method);
  ei=complex(vr,vi);
  vpha=mod(-deg*angle(ei)+correc_phase,360.0); 
  vamp=abs(ei)*correc_amp;
%
% Convert to tidal ellipses
%
  disp('  Convert to tidal ellipse parameters...')
  [major,eccentricity,inclination,phase]=ap2ep(uamp,upha,vamp,vpha);
  ncfrc{'tide_Cmin'}(itide,:,:)=major.*eccentricity;
  ncfrc{'tide_Cmax'}(itide,:,:)=major;
  ncfrc{'tide_Cangle'}(itide,:,:)=inclination;
  ncfrc{'tide_Cphase'}(itide,:,:)=phase;
%
  if pot_tides,
%
% Process equilibrium tidal potential
%
   disp('Process equilibrium tidal potential...')
   if periods(it)<13.0                 % semidiurnal
     Pamp=correc_amp*A(it)*B(it)*coslat2;
     Ppha=mod(-2.*rlon+correc_phase,360.0);
   elseif periods(it)<26.0             % diurnal
     Pamp=correc_amp*A(it)*B(it)*sin2lat;
     Ppha=mod(-rlon+correc_phase,360.0);
   else                                % long-term
     Pamp=correc_amp*A(it)*B(it)*(1-1.5*coslat2);
     Ppha=mod(correc_phase,360.0);
   end
%
% Process tidal loading and self-attraction potential
%            from GOT99.2b model
%
   if sal_tides & it<9
    disp('Process tidal loading and self-attraction potential...')
    [SALamp,SALpha]=ext_data_sal(grdname,salname, ...
                                 'tide_SALamp','tide_SALpha',it);
    SALamp=SALamp*correc_amp;
    SALpha=mod(SALpha+correc_phase,360.0);
%
% --> Get total tidal potential = Eq + load
%
    disp('Get total tidal potential...')
    Ptot=Pamp.*exp(1i*Ppha*rad) + SALamp.*exp(1i*SALpha*rad);
    Pamp=abs(Ptot);
    Ppha=deg*angle(Ptot);
    Ppha(Pamp<0.0001)=0.;
    Ppha=mod(Ppha,360.0);
   end
%
% Write tidal potential into file
%
   ncfrc{'tide_Pamp'}(itide,:,:)=Pamp;
   ncfrc{'tide_Pphase'}(itide,:,:)=Ppha;

  end % <-- if pot_tides

end % <-- itide ----
%
% Close the files
%
close(nctides)
close(ncfrc)
%
%------------------------------------------------
%                 Make plots 
%------------------------------------------------
%
% Plot tidal harmonics
%
if makeplot==1

  warning off
  if Ntides>=5, 
   tiderg=[1 5];
  else
   tiderg=1;
  end
  for itide=tiderg; %1:Ntides
    figure
    plot_tide(grdname,frcname,itide,0.5,2,coastfileplot)
  end
  warning on 

  if pot_tides
%
%  Plot tidal potential
%
    ncfrc=netcdf(frcname);
    domaxis=[minlon maxlon minlat maxlat];
% M2 AMP
    Pamp=squeeze(ncfrc{'tide_Pamp'}(1,:,:));
    figure
    m_proj('mercator',...
           'lon',[domaxis(1) domaxis(2)],...
           'lat',[domaxis(3) domaxis(4)]);
    m_pcolor(rlon,rlat,Pamp);
    shading flat;
    colorbar
    m_gshhs_i('color','r');
    m_gshhs_i('patch','k');
    m_grid('box','fancy','xtick',5,'ytick',5,'fontsize',7);
    frcname=['Potential Tides: M2 Amplitude [m]'];
    title(frcname,'fontsize',16)
% M2 PHASE
    Ppha=squeeze(ncfrc{'tide_Pphase'}(1,:,:));
    figure
    m_proj('mercator',...
           'lon',[domaxis(1) domaxis(2)],...
           'lat',[domaxis(3) domaxis(4)]);
    m_pcolor(rlon,rlat,Ppha);
    shading flat;
    colorbar
    m_gshhs_i('color','r');
    m_gshhs_i('patch','k');
    m_grid('box','fancy','xtick',5,'ytick',5,'fontsize',7);
    frcname=['Potential Tides: M2 Phase [m]'];
    title(frcname,'fontsize',16)
    
    if Ntides>=5
% K1 AMP
    Pamp=squeeze(ncfrc{'tide_Pamp'}(5,:,:));
    figure
    m_proj('mercator',...
           'lon',[domaxis(1) domaxis(2)],...
           'lat',[domaxis(3) domaxis(4)]);
    m_pcolor(rlon,rlat,Pamp);
    shading flat;
    colorbar
    m_gshhs_i('color','r');
    m_gshhs_i('patch','k');
    m_grid('box','fancy','xtick',5,'ytick',5,'fontsize',7);
    frcname=['Potential Tides: K1 Amplitude [m]'];
    title(frcname,'fontsize',16)
% K1 PHASE
    Ppha=squeeze(ncfrc{'tide_Pphase'}(5,:,:));
    figure
    m_proj('mercator',...
           'lon',[domaxis(1) domaxis(2)],...
           'lat',[domaxis(3) domaxis(4)]);
    m_pcolor(rlon,rlat,Ppha);
    shading flat;
    colorbar
    m_gshhs_i('color','r');
    m_gshhs_i('patch','k');
    m_grid('box','fancy','xtick',5,'ytick',5,'fontsize',7);
    frcname=['Potential Tides: K1 Phase [m]'];
    title(frcname,'fontsize',16)
    close(ncfrc)
    end
  end  % <-- pot_tides

end  % <-- makeplot
