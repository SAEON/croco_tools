%===========================================
%
% make_clim
%
% Build ASCII climatology file for ROMS-1D
% with interpolated WOA T,S,NO3 profiles
%
% P. Marchesiello - June 2012
%
%===========================================
close all
clear all

romstools_param

INDIR   = CLMDIR;
outfile = clm_file;
%===========================================
%
outfile=[OUTDIR,outfile];
fid=fopen(outfile,'w');

% TITLE
fprintf(fid,'ROMS-1D CLIMATOLOGY PROFILE DATA \n');
% LON LAT
fprintf(fid,'LONGITUDE %8.2f   - LATITUDE %8.2f \n',lon,lat);
fprintf(fid,'\n');

% TEMP
fname=[INDIR,'temp_month.cdf'];
[J,I]=find_station(fname,lon,lat);
nc=netcdf(fname);
z=-nc{'Z'}(1:24);
temp=nc{'temperature'}(6,1:24,J,I);
close(nc);
figure(1)
plot(temp,z)
title(['TEMPERATURE PROFILE - LON=',num2str(lon),' LAT=',num2str(lat)])

% SALT
fname=[INDIR,'salt_month.cdf'];
[J,I]=find_station(fname,lon,lat);
nc=netcdf(fname);
z=-nc{'Z'}(1:24);
salt=nc{'salinity'}(6,1:24,J,I);
close(nc);
figure(2)
plot(salt,z)
title(['SALINITY PROFILE - LON=',num2str(lon),' LAT=',num2str(lat)])

% NO3
fname=[INDIR,'no3_ann.cdf'];
[J,I]=find_station(fname,lon,lat);
nc=netcdf(fname);
z=-nc{'Z'}(1:24);
no3=nc{'nitrate'}(1:24,J,I);
close(nc);
figure(3)
plot(no3,z)
title(['NITRATE PROFILE - LON=',num2str(lon),' LAT=',num2str(lat)])

% WRITE TO FILE
%
fprintf(fid,'WOA DEPTH (m) \n');
format='%8.0f%8.0f%8.0f%8.0f%8.0f%8.0f%8.0f%8.0f%8.0f%8.0f%8.0f \n';
for n=24:-1:13;
 fprintf(fid,format,z(n));
end
fprintf(fid,'\n');
for n=12:-1:1;
 fprintf(fid,format,z(n));
end
fprintf(fid,'\n\n');
%
fprintf(fid,'WOA TEMP \n');
format='%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f \n';
for n=24:-1:13;
 fprintf(fid,format,temp(n));
end
fprintf(fid,'\n');
for n=12:-1:1;
 fprintf(fid,format,temp(n));
end
fprintf(fid,'\n\n');
%
fprintf(fid,'WOA SALT \n');
format='%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f \n';
for n=24:-1:13;
 fprintf(fid,format,salt(n));
end
fprintf(fid,'\n');
for n=12:-1:1;
 fprintf(fid,format,salt(n));
end
fprintf(fid,'\n\n');
%
fprintf(fid,'WOA NO3 \n');
format='%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f%8.2f \n';
for n=24:-1:13;
 fprintf(fid,format,no3(n));
end
fprintf(fid,'\n');
for n=12:-1:1;
 fprintf(fid,format,no3(n));
end
fprintf(fid,'\n\n');

return
