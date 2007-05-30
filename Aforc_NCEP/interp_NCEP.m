function interp_NCEP(NCEP_version,NCEP_dir,Y,M,Roa,interp_method,...
                     lon1,lat1,mask1,lon2,lat2,mask2,tin,...
		     nc_frc,nc_blk,lon,lat,angle,tout)

%
% Read the local NCEP files and perform the interpolations
%
% Pierrick 2005
%


%
% 1: Air temperature: Convert from Kelvin to Celsius
%
nc=netcdf([NCEP_dir,'air.2m.gauss.Y',num2str(Y),'M',num2str(M),'.nc']);
tair=squeeze(nc{'air'}(tin,:,:));
close(nc)
tair=get_missing_val(lon1,lat1,mask1.*tair,nan,Roa,nan);
tair=tair-273.15;
tair=interp2(lon1,lat1,tair,lon,lat,interp_method);
%
% 2: Relative humidity: Convert from % to fraction
%
if NCEP_version==1
  nc=netcdf([NCEP_dir,'rhum.sig995.Y',num2str(Y),'M',num2str(M),'.nc']);
  rhum=squeeze(nc{'rhum'}(tin,:,:));
  close(nc)
  rhum=get_missing_val(lon2,lat2,mask2.*rhum,nan,Roa,nan);
  rhum=rhum/100;
  rhum=interp2(lon2,lat2,rhum,lon,lat,interp_method);
elseif NCEP_version==2
  nc=netcdf([NCEP_dir,'shum.2m.gauss.Y',num2str(Y),'M',num2str(M),'.nc']);
  shum=squeeze(nc{'shum'}(tin,:,:));
  close(nc)
  shum=get_missing_val(lon1,lat1,mask1.*shum,nan,Roa,nan);
  shum=interp2(lon1,lat1,shum,lon,lat,interp_method);
  rhum=shum./qsat(tair);
end
%
% 3: Precipitation rate: Convert from [kg/m^2/s] to cm/day
%
nc=netcdf([NCEP_dir,'prate.sfc.gauss.Y',num2str(Y),'M',num2str(M),'.nc']);
prate=squeeze(nc{'prate'}(tin,:,:));
close(nc)
prate=get_missing_val(lon1,lat1,mask1.*prate,nan,Roa,nan);
prate=prate*0.1*(24*60*60.0);
prate=interp2(lon1,lat1,prate,lon,lat,interp_method);
prate(abs(prate)<1.e-4)=0;
%
% 4: Net shortwave flux: [W/m^2]
%      ROMS convention: positive downward
%           opposite to NCEP -->*(-1)
%           but same as NCEP2...
%
if NCEP_version==1
  nc=netcdf([NCEP_dir,'nswrs.sfc.gauss.Y',num2str(Y),'M',num2str(M),'.nc']);
  nswrs=squeeze(nc{'nswrs'}(tin,:,:));
  close(nc)
  nswrs=get_missing_val(lon1,lat1,mask1.*nswrs,nan,Roa,nan);
  nswrs=nswrs*-1;
  radsw=interp2(lon1,lat1,nswrs,lon,lat,interp_method);
  radsw(radsw<1.e-10)=0;
elseif NCEP_version==2
  nc=netcdf([NCEP_dir,'dswrf.sfc.gauss.Y',num2str(Y),'M',num2str(M),'.nc']);
  dswrf=squeeze(nc{'dswrf'}(tin,:,:));
  close(nc)
  dswrf=get_missing_val(lon1,lat1,mask1.*dswrf,nan,Roa,nan);
  radsw=interp2(lon1,lat1,dswrf,lon,lat,interp_method);
  radsw(radsw<1.e-10)=0;
end
%
% 5: Net outgoing Longwave flux:  [W/m^2]
%      ROMS convention: positive upward (opposite to nswrs)
%
if NCEP_version==1
  nc=netcdf([NCEP_dir,'nlwrs.sfc.gauss.Y',num2str(Y),'M',num2str(M),'.nc']);
  nlwrs=squeeze(nc{'nlwrs'}(tin,:,:));
  close(nc)
  nlwrs=get_missing_val(lon1,lat1,mask1.*nlwrs,nan,Roa,nan);
  radlw=interp2(lon1,lat1,nlwrs,lon,lat,interp_method);
elseif NCEP_version==2
%
%   lwhf convention: positive downward --> * (-1)
%   input: downward longwave rad. and sst from
%   respectively NCEP2 downward rad. (same conv.) and
%   skin temperature.
%
  nc=netcdf([NCEP_dir,'skt.sfc.gauss.Y',num2str(Y),'M',num2str(M),'.nc']);
  skt=squeeze(nc{'skt'}(tin,:,:));
  close(nc)
  skt=get_missing_val(lon1,lat1,mask1.*skt,nan,Roa,nan);
  skt=skt-273.15; 
%
  nc=netcdf([NCEP_dir,'dlwrf.sfc.gauss.Y',num2str(Y),'M',num2str(M),'.nc']);
  dlwrf=squeeze(nc{'dlwrf'}(tin,:,:));
  close(nc)
  dlwrf=get_missing_val(lon1,lat1,mask1.*dlwrf,nan,Roa,nan);
  nlwf=-lwhf(skt,dlwrf); 
  radlw=interp2(lon1,lat1,nlwf,lon,lat,interp_method);
end
%
% 6: Wind & Wind stress
%
nc=netcdf([NCEP_dir,'uwnd.10m.gauss.Y',num2str(Y),'M',num2str(M),'.nc']);
uwnd=squeeze(nc{'uwnd'}(tin,:,:));
close(nc)
uwnd=get_missing_val(lon1,lat1,mask1.*uwnd,nan,Roa,nan);
uwnd=interp2(lon1,lat1,uwnd,lon,lat,interp_method);
%
nc=netcdf([NCEP_dir,'vwnd.10m.gauss.Y',num2str(Y),'M',num2str(M),'.nc']);
vwnd=squeeze(nc{'vwnd'}(tin,:,:));
close(nc)
vwnd=get_missing_val(lon1,lat1,mask1.*vwnd,nan,Roa,nan);
vwnd=interp2(lon1,lat1,vwnd,lon,lat,interp_method);
%
% Compute the stress
%
wspd=sqrt(uwnd.^2+vwnd.^2);
[Cd,uu]=cdnlp(wspd,10.);
rhoa=air_dens(tair,rhum*100);
tx=Cd.*rhoa.*uwnd.*wspd;
ty=Cd.*rhoa.*vwnd.*wspd;
%
% Rotations on the ROMS grid
%
cosa=cos(angle);
sina=sin(angle);
%
sustr=rho2u_2d(tx.*cosa+ty.*sina);
svstr=rho2v_2d(ty.*cosa-tx.*sina);
%
% uwnd et vwnd sont aux points 'rho'
%
u10=uwnd.*cosa+vwnd.*sina;
v10=vwnd.*cosa-uwnd.*sina;
%
% Fill the ROMS files
%
if ~isempty(nc_frc)
  nc_frc{'sustr'}(tout,:,:)=sustr;
  nc_frc{'svstr'}(tout,:,:)=svstr;
end
if ~isempty(nc_blk)
  nc_blk{'tair'}(tout,:,:)=tair;
  nc_blk{'rhum'}(tout,:,:)=rhum;
  nc_blk{'prate'}(tout,:,:)=prate;
  nc_blk{'wspd'}(tout,:,:)=wspd;
  nc_blk{'radlw'}(tout,:,:)=radlw;
  nc_blk{'radsw'}(tout,:,:)=radsw;
  nc_blk{'uwnd'}(tout,:,:)=u10;
  nc_blk{'vwnd'}(tout,:,:)=v10;
%  nc_blk{'sustr'}(tout,:,:)=sustr;
%  nc_blk{'svstr'}(tout,:,:)=svstr;
end
