function extract_SODA(SODA_dir,SODA_prefix,url,year,month,...
                      lon,lat,depth,time,...
                      trange,krange,jrange,...
                      i1min,i1max,i2min,i2max,i3min,i3max,...
                      Yorig)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Extract a subset from SODA using OPENDAP
% Write it in a local file (keeping the classic
% SODA netcdf format) 
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
%  Copyright (c) 2005-2006 by Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr  
%
%  Updated   6-Sep-2006 by Pierrick Penven
%  Updated : 23-Oct-2009 by Gildas Cambon Adapatation for the special tratment 
%            for the bry file
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
disp(['    Download SODA for ',num2str(year),...
      ' - ',num2str(month)])
%
% Get the dataset attributes
%
disp('Get the dataset attributes')
x=readattribute(url);		
%
% Get SSH
%
disp('    ...SSH')
ssh=getdap(url,'',...
	       'ssh',trange,'',jrange,...
	       i1min,i1max,i2min,i2max,i3min,i3max);
missval=x.ssh.missing_value;
ssh(ssh<=missval)=NaN;
%
% Get TAUX
%
disp('    ...TAUX')
taux=getdap(url,'',...
		'taux',trange,'',jrange,...
		i1min,i1max,i2min,i2max,i3min,i3max);
missval=x.taux.missing_value;
taux(taux<=missval)=NaN;
%
% Get TAUY
%
disp('    ...TAUY')
tauy=getdap(url,'',...
		'tauy',trange,'',jrange,...
		i1min,i1max,i2min,i2max,i3min,i3max);
missval=x.tauy.missing_value;
tauy(tauy<=missval)=NaN;
%
% Get U
%
disp('    ...U')
u=getdap(url,'',...
	     'u',trange,krange,jrange,...
	     i1min,i1max,i2min,i2max,i3min,i3max);
u=shiftdim(u,2);
missval=x.u.missing_value;
u(u<=missval)=NaN;
%
% Get V
%
disp('    ...V')
v=getdap(url,'',...
	     'v',trange,krange,jrange,...
	     i1min,i1max,i2min,i2max,i3min,i3max);
v=shiftdim(v,2);
missval=x.v.missing_value;
v(v<=missval)=NaN;
%
% Get TEMP
%
disp('    ...TEMP')
temp=getdap(url,'',...
		'temp',trange,krange,jrange,...
		i1min,i1max,i2min,i2max,i3min,i3max);
temp=shiftdim(temp,2);
missval=x.temp.missing_value;
temp(temp<=missval)=NaN;
%
% Get SALT
%
disp('    ...SALT')
salt=getdap(url,'',...
		'salt',trange,krange,jrange,...
		i1min,i1max,i2min,i2max,i3min,i3max);
salt=shiftdim(salt,2);
missval=x.salt.missing_value;
salt(salt<=missval)=NaN;
%
% Create the SODA file
%
% $$$ create_OGCM([SODA_dir,SODA_prefix,'Y',num2str(year),'M',num2str(month),'.cdf'],...
% $$$             lon,lat,lon,lat,lon,lat,depth,time,...
% $$$             temp,salt,u,v,ssh,taux,tauy,Yorig)

create_SODA([SODA_dir,SODA_prefix,'Y',num2str(year),'M',num2str(month),'.cdf'],...
            lon,lat,lon,lat,lon,lat,depth,time,...
            temp,salt,u,v,ssh,taux,tauy,Yorig)
%
return
