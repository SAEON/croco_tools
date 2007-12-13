function extract_ECCO(OGCM_dir,OGCM_prefix,url,Y,M,...
                      lon,lon_u,lat,lat_v,depth,...
                      krange,jrange,jrange_v,...
                      i1min,i1max,i2min,i2max,i3min,i3max,...
                      i1min_u,i1max_u,i2min_u,i2max_u,i3min_u,i3max_u,...
                      Yorig)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Extract a subset from ECCO using DODS
% Write it in a local file (keeping the classic
% SODA netcdf format)
% 
%
%  Further Information:  
%  http://www.brest.ird.fr/Roms_tools/
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
%  Copyright (c) 2006 by Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr  
%
%  Updated    6-Sep-2006 by Pierrick Penven
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

disp(['    Download ECCO for ',num2str(Y),...
      ' - ',num2str(M)])
%
if M <= 3
  prefix=[url,num2str(Y),'/n10day_01_09/'];
  suffix=['_08_08.00001_02160_240.cdf'];
elseif M <= 6
  prefix=[url,num2str(Y),'/n10day_10_18/'];
  suffix=['_08_08.02160_04320_240.cdf'];
elseif M <= 9
  prefix=[url,num2str(Y),'/n10day_19_27/'];
  suffix=['_08_08.04320_06480_240.cdf'];
elseif M <= 12
  prefix=[url,num2str(Y),'/n10day_28_37/'];
  suffix=['_08_08.06480_08880_240.cdf'];
end
%
% Get the dataset attributes
%
vname='Have';
fname=[prefix,vname,suffix];
x=readattribute(fname);		
missval=x.Have.missing_value;
%
% Get the time range
%
time=readdap(fname,'time',[]);
%
% Process the time
%
time=time/24+datenum(1970,1,1);
disp(datestr(time))
c=datevec(time);
y0=c(:,1);
m0=c(:,2);
oktime=1;
if Y~=mean(y0)
  disp(['Warning: dates are not all for year: ',num2str(Y)])
  oktime=0;
end
tndx=find(m0==M);
if isempty(tndx)
  error('Time problem: did not find the corresponding month')
end
%
if oktime==0 & M==12 % to get the value of 1 jan of next year %
  tndx=[tndx;tndx(end)+1];
end
trange=['[',num2str(min(tndx)-1),':',num2str(max(tndx)-1),']'];
%
% Put the time into "Yorig time"
%
time=time(tndx)-datenum(Yorig,1,1);
%
% Get SSH
%
disp('    ...SSH')
vname='Have';
fname=[vname,suffix];
ssh=getdap(prefix,fname,vname,...
            trange,'',jrange,...
            i1min,i1max,i2min,i2max,i3min,i3max);
ssh=shiftdim(ssh,2);
ssh(ssh<=missval)=NaN;
%
% Get U
%
disp('    ...U')
vname='Uave';
fname=[vname,suffix];
u=getdap(prefix,fname,vname,...
          trange,krange,jrange,...
          i1min_u,i1max_u,i2min_u,i2max_u,i3min_u,i3max_u);
u=permute(u,[4 3 1 2]);
u(u<=missval)=NaN;
%
% Get V
%
disp('    ...V')
vname='Vave';
fname=[vname,suffix];
v=getdap(prefix,fname,vname,...
          trange,krange,jrange_v,...
          i1min,i1max,i2min,i2max,i3min,i3max);
v=permute(v,[4 3 1 2]);
v(v<=missval)=NaN;
%
% Get TEMP
%
disp('    ...TEMP')
vname='Tave';
fname=[vname,suffix];
temp=getdap(prefix,fname,vname,...
             trange,krange,jrange,...
             i1min,i1max,i2min,i2max,i3min,i3max);
temp=permute(temp,[4 3 1 2]);
temp(temp<=missval)=NaN;
%
% Get SALT
%
disp('    ...SALT')
vname='Save';
fname=[vname,suffix];
salt=getdap(prefix,fname,vname,...
             trange,krange,jrange,...
             i1min,i1max,i2min,i2max,i3min,i3max);
salt=permute(salt,[4 3 1 2]);
salt(salt<=missval)=NaN;
%
% Create the ECCO file
%
create_OGCM([OGCM_dir,OGCM_prefix,'Y',num2str(Y),'M',num2str(M),'.cdf'],...
             lon,lat,lon_u,lat,lon,lat_v,depth,time,...
             temp,salt,u,v,ssh,Yorig)
%
return
