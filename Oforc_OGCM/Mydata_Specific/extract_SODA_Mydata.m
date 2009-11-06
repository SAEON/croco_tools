function [u,v,temp,salt,taux,tauy,ssh]=extract_SODA_Mydata(SODA_dir,SODA_prefix,...
                      url_mydata,...
		      year,month,...
                      lon,lat,depth,tndx,tndx3,SODA_time,...
                      krange,jmin,jmax,...
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
%  Updated    6-Sep-2006 by Pierrick Penven
%  Updated : 13-Sep-2009 by Gildas Cambon Adapatation for the 
%  case without OpenDap. Take care, of the vertical axis. 
%  Here no flipdim
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
disp(['    Compute SODA forcing 5d mean from my data for ',num2str(year),...
      ' - ',num2str(month)])

% Get SSH
%
disp('    ...SSH')
myimin=i1min; 
myimax=i1max; 
%
%nc=netcdf([url_mydata,'Coastal_SA_',num2str(year),'.cdf'])
nc=netcdf([url_mydataogcm,'/,'prefix_mydataogcm,'_',num2str(year),'.cdf'])
ssh=nc{'SSH'}(tndx3,jmin:jmax,myimin:myimax); 
missval=nc{'SSH'}.missing_value(:);
ssh(ssh==missval)=NaN;
%
% Get TAUX
%
disp('    ...TAUX')
taux=nc{'TAUX'}(tndx3,jmin:jmax,myimin:myimax); 
missval=nc{'TAUX'}.missing_value(:);
taux(taux==missval)=NaN;
%
% Get TAUY
%
disp('    ...TAUY')
tauy=nc{'TAUY'}(tndx3,jmin:jmax,myimin:myimax); 
missval=nc{'TAUY'}.missing_value(:);
tauy(tauy==missval)=NaN;
%
% Get U
%
disp('    ...U')
u=nc{'U'}(tndx3,:,jmin:jmax,myimin:myimax);
missval=nc{'U'}.missing_value(:);
u(u==missval)=NaN;
%
% Get V
%
disp('    ...V')
v=nc{'V'}(tndx3,:,jmin:jmax,myimin:myimax); 
missval=nc{'V'}.missing_value(:);
v(v==missval)=NaN;
%
% Get TEMP
%
disp('    ...TEMP')
temp=nc{'TEMP'}(tndx3,:,jmin:jmax,myimin:myimax);
missval=nc{'TEMP'}.missing_value(:);
temp(temp==missval)=NaN;
%
% Get SALT
%
disp('    ...SALT')
salt=nc{'SALT'}(tndx3,:,jmin:jmax,myimin:myimax);
missval=nc{'SALT'}.missing_value(:);
salt(salt==missval)=NaN;
close(nc)
%
%
% Create the SODA file
%
disp(['Create_SODA'])
create_SODA([SODA_dir,SODA_prefix,'Y',num2str(year),'M',num2str(month),'.cdf'],...
            lon,lat,lon,lat,lon,lat,depth,SODA_time(tndx),...
            temp,salt,u,v,ssh,taux,tauy,Yorig)

%----------------------------------------------------------------------
return
