%===========================================
%
% make_forcing
%
% BUILD ASCII forcing file for ROMS-1D
%
% P. Marchesiello June 2012
%===========================================
close all
clear all

romstools_param

INDIR   = FORDIR;
outfile = frc_file;
%===========================================
%
% call air-sea constants
as_consts

outfile=[OUTDIR,outfile];
fid=fopen(outfile,'w');

% TITLE
fprintf(fid,'ROMS-1D FORCING DATA \n');
fprintf(fid,'\n');

% LON LAT
fprintf(fid,'LONGITUDE  -  LATITUDE \n');
fprintf(fid,'%8.2f%12.2f \n',lon,lat);
fprintf(fid,'\n');

% SUSTR
fname=[INDIR,'taux.cdf'];
[J,I]=find_station(fname,lon,lat);
nc=netcdf(fname);
var=nc{'taux'}(:,J,I);
if var==-1.e10,
 disp('STATION ON LAND - ABORT')
 return
end
close(nc);
fprintf(fid,'SUSTR "surface u-momentum stress" "Newton meter-2" \n');
format='%10.4f%10.4f%10.4f%10.4f%10.4f%10.4f \n';
for n=1:6;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n');
for n=7:12;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n\n');

% SVSTR
fname=[INDIR,'tauy.cdf'];
nc=netcdf(fname);
var=nc{'tauy'}(:,J,I);
close(nc);
fprintf(fid,'SVSTR "surface v-momentum stress" "Newton meter-2" \n');
format='%10.4f%10.4f%10.4f%10.4f%10.4f%10.4f \n';
for n=1:6;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n');
for n=7:12;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n\n');

% SHFLUX
fname=[INDIR,'netheat.cdf'];
nc=netcdf(fname);
var=nc{'netheat'}(:,J,I);
close(nc);
fprintf(fid,'SHFLUX "surface net heat flux" "Watts meter-2" \n');
format='%10.4f%10.4f%10.4f%10.4f%10.4f%10.4f \n';
for n=1:6;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n');
for n=7:12;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n\n');

% SWFLUX
fname=[INDIR,'emp.cdf'];
nc=netcdf(fname);
var=0.8*nc{'emp'}(:,J,I); % coeff = mm/(3hour) -> centimeter day-1
close(nc);
fprintf(fid,'SWFLUX "surface freshwater flux (E-P)" "centimeter day-1" \n');
format='%10.4f%10.4f%10.4f%10.4f%10.4f%10.4f \n';
for n=1:6;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n');
for n=7:12;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n\n');

% SST
fname=[INDIR,'sst.cdf'];
nc=netcdf(fname);
var=nc{'sst'}(:,J,I);
close(nc);
fprintf(fid,'SST "sea surface temperature" "Celsius" \n');
format='%10.4f%10.4f%10.4f%10.4f%10.4f%10.4f \n';
for n=1:6;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n');
for n=7:12;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n\n');

% DQSST
fname=[INDIR,'sst.cdf'];
nc=netcdf(fname);
sst=nc{'sst'}(:,J,I);
close(nc);
fname=[INDIR,'sat.cdf'];
nc=netcdf(fname);
sat=nc{'sat'}(:,J,I);
close(nc);
fname=[INDIR,'airdens.cdf'];
nc=netcdf(fname);
airdens=nc{'airdens'}(:,J,I);
fname=[INDIR,'w3.cdf'];
close(nc);
nc=netcdf(fname);
w3=nc{'w3'}(:,J,I);
close(nc);
fname=[INDIR,'qsea.cdf'];
nc=netcdf(fname);
qsea=0.001*nc{'qsea'}(:,J,I);
close(nc);
var=get_dqdsst(sst,sat,airdens,w3,qsea);
fprintf(fid,'DQSST "surface net heat flux sensitivity to SST" "Watts m-2 Cels-1" \n');
format='%10.4f%10.4f%10.4f%10.4f%10.4f%10.4f \n';
for n=1:6;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n');
for n=7:12;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n\n');

% SWRAD
fname=[INDIR,'shortrad.cdf'];
nc=netcdf(fname);
var=nc{'shortrad'}(:,J,I);
close(nc);
fprintf(fid,'SWRAD "solar shortwave radiation" "Watts meter-2" \n');
format='%10.4f%10.4f%10.4f%10.4f%10.4f%10.4f \n';
for n=1:6;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n');
for n=7:12;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n\n');

% UPWI
fprintf(fid,'UPWI "upwelling indice" "m3 s-1" \n');
format='%10.4f%10.4f%10.4f%10.4f%10.4f%10.4f \n';
var(1:12)=0.0;
for n=1:6;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n');
for n=7:12;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n\n');

%
% BULK FLUXES VARIABLES
%

% RADSW
fname=[INDIR,'shortrad.cdf'];
[J,I]=find_station(fname,lon,lat);
nc=netcdf(fname);
var=nc{'shortrad'}(:,J,I);
close(nc);
fprintf(fid,'RADSW "solar shortwave radiation" "Watts meter-2" \n');
format='%10.4f%10.4f%10.4f%10.4f%10.4f%10.4f \n';
for n=1:6;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n');
for n=7:12;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n\n');

% RADLW
fname=[INDIR,'longrad.cdf'];
[J,I]=find_station(fname,lon,lat);
nc=netcdf(fname);
radlw=nc{'longrad'}(:,J,I);
close(nc);
% substract upward gray-body longwave flux 
% and make it positive downward;
lwup=emiss_lw.*sigmaSB.*((sst+CtoK).^4);
var=-(radlw-lwup);
fprintf(fid,'RADLW "Downward longwave radiation" "Watts meter-2" \n');
format='%10.4f%10.4f%10.4f%10.4f%10.4f%10.4f \n';
for n=1:6;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n');
for n=7:12;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n\n');

% PRATE
fname=[INDIR,'precip.cdf'];
[J,I]=find_station(fname,lon,lat);
nc=netcdf(fname);
var=0.8*nc{'precip'}(:,J,I); % mm/(3hour) -> centimeter day-1 
close(nc);
fprintf(fid,'PRATE "precipitaion rate"  "centimeter day-1" \n');
format='%10.4f%10.4f%10.4f%10.4f%10.4f%10.4f \n';
for n=1:6;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n');
for n=7:12;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n\n');

% TAIR
fname=[INDIR,'sat.cdf'];
[J,I]=find_station(fname,lon,lat);
nc=netcdf(fname);
var=nc{'sat'}(:,J,I);
close(nc);
fprintf(fid,'TAIR "sea level air temperature" "Celsius" \n');
format='%10.4f%10.4f%10.4f%10.4f%10.4f%10.4f \n';
for n=1:6;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n');
for n=7:12;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n\n');

% RHUM
fname=[INDIR,'rh.cdf'];
[J,I]=find_station(fname,lon,lat);
nc=netcdf(fname);
var=0.01*nc{'rh'}(:,J,I);  % percent -> fraction
close(nc);
fprintf(fid,'RHUM "relative humidity" "fraction" \n');
format='%10.4f%10.4f%10.4f%10.4f%10.4f%10.4f \n';
for n=1:6;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n');
for n=7:12;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n\n');

% UWND
fname=[INDIR,'u3.cdf'];
[J,I]=find_station(fname,lon,lat);
nc=netcdf(fname);
var=nc{'u3'}(:,J,I);
close(nc);
fprintf(fid,'UWND "zonal wind" "m/s" \n');
format='%10.4f%10.4f%10.4f%10.4f%10.4f%10.4f \n';
for n=1:6;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n');
for n=7:12;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n\n');

% VWND
fname=[INDIR,'v3.cdf'];
[J,I]=find_station(fname,lon,lat);
nc=netcdf(fname);
var=nc{'v3'}(:,J,I);
close(nc);
fprintf(fid,'VWND "zonal wind" "m/s" \n');
format='%10.4f%10.4f%10.4f%10.4f%10.4f%10.4f \n';
for n=1:6;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n');
for n=7:12;
 fprintf(fid,format,var(n));
end
fprintf(fid,'\n\n');


fclose(fid);
return
%=======================================================

