function vinterp(clmname,grdname,oaname,vname,tname,zname,tini,...
                 type,isinitialval);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Vertical interpolation from a Z-grid to a sigma-grid in the
%  case of climatology files.
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
%  Copyright (c) 2003-2006 by Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr  
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% open the grid file  
% 
ng=netcdf(grdname);
h=ng{'h'}(:);
close(ng);
%
% open the clim file  
% 
nc=netcdf(clmname,'write');
theta_s = nc{'theta_s'}(:);
theta_b =  nc{'theta_b'}(:);
hc  =  nc{'hc'}(:);
N =  length(nc('s_rho'));
%
% open the oa file  
% 
noa=netcdf(oaname);
z=-noa{zname}(:);
t=noa{tname}(:);
tlen=length(t);
%
% Get the sigma depths
%
zroms=zlevs(h,0.*h,theta_s,theta_b,hc,N,'r');
if type=='u'
  zroms=rho2u_3d(zroms);
end
if type=='v'
  zroms=rho2v_3d(zroms);
end
zmin=min(min(min(zroms)));
zmax=max(max(max(zroms)));
%
% Check if the min z level is below the min sigma level 
%    (if not add a deep layer)
%
addsurf=max(z)<zmax;
addbot=min(z)>zmin;
if addsurf
 z=[100;z];
end
if addbot
 z=[z;-100000];
end
Nz=min(find(z<zmin));
z=z(1:Nz);
%
% loop on time
%
% Are we processing initial file ?
isinitial = 0;
if (nargin > 8)
isinitial = isinitialval;
end

if (isinitial == 1)  % initial file 
  tlen=1;
end
for l=1:tlen
%for l=1:1
  if (isinitial == 0)
    disp([' Time index: ',num2str(l),' of total: ',num2str(tlen)])
  else
    ll=find(t<=tini);
    if (size(ll,1) ~= 0)
    l=ll(size(ll,1));
    else
    l=1;
    end
    disp([' Time index: ',num2str(l)])
  end
  var=squeeze(noa{vname}(l,:,:,:));
  if addsurf
    var=cat(1,var(1,:,:),var);
  end
  if addbot
    var=cat(1,var,var(end,:,:));
  end
  var=var(1:Nz,:,:);
  if (isinitial == 0)
    nc{vname}(l,:,:,:)=ztosigma(flipdim(var,1),zroms,flipud(z));
  else
    nc{vname}(1,:,:,:)=ztosigma(flipdim(var,1),zroms,flipud(z));
  end
end
close(nc);
close(noa);
return
