function nested_restart(child_grd,parent_rst,child_rst,...
                        vertical_correc,extrapmask,biol)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Compute the restart file of the embedded grid
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
%  Copyright (c) 2004-2006 by Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr  
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Title
%
title=['restart file for the embedded grid :',child_rst,...
       ' using parent restart file: ',parent_rst];
disp(' ')
disp(title)
%
if vertical_correc==1
 disp('Vertical corrections: on')
end
if extrapmask==1
 disp('Under mask extrapolations: on')
end
if biol==1
 disp('Biology: on')
end
%
% Read in the embedded grid
%
disp(' ')
disp(' Read in the embedded grid...')
nc=netcdf(child_grd);
parent_grd=nc.parent_grid(:);
imin=nc{'grd_pos'}(1);
imax=nc{'grd_pos'}(2);
jmin=nc{'grd_pos'}(3);
jmax=nc{'grd_pos'}(4);
refinecoeff=nc{'refine_coef'}(:);
result=close(nc);
nc=netcdf(parent_grd);
Lp=length(nc('xi_rho'));
Mp=length(nc('eta_rho'));
if extrapmask==1
  mask=nc{'mask_rho'}(:);
else
  mask=[];
end
result=close(nc);
%
% Read in the parent restart file
%
disp(' ')
disp(' Read in the parent restart file...')
nc = netcdf(parent_rst);
N=length(nc('s_rho'));
thetime = nc{'scrum_time'}(:);
result=close(nc);
%
% Create the restart file
%
disp(' ')
disp(' Create the restart file...')
ncrst=create_nestedrestart(child_rst,child_grd,parent_rst,title,'clobber');
%
% Get the parent indices
%
[igrd_r,jgrd_r]=meshgrid((1:1:Lp),(1:1:Mp));
[igrd_p,jgrd_p]=meshgrid((1:1:Lp-1),(1:1:Mp-1));
[igrd_u,jgrd_u]=meshgrid((1:1:Lp-1),(1:1:Mp));
[igrd_v,jgrd_v]=meshgrid((1:1:Lp),(1:1:Mp-1));
%
% Get the children indices
%
ipchild=(imin:1/refinecoeff:imax);
jpchild=(jmin:1/refinecoeff:jmax);
irchild=(imin+0.5-0.5/refinecoeff:1/refinecoeff:imax+0.5+0.5/refinecoeff);
jrchild=(jmin+0.5-0.5/refinecoeff:1/refinecoeff:jmax+0.5+0.5/refinecoeff);
[ichildgrd_p,jchildgrd_p]=meshgrid(ipchild,jpchild);
[ichildgrd_r,jchildgrd_r]=meshgrid(irchild,jrchild);
[ichildgrd_u,jchildgrd_u]=meshgrid(ipchild,jrchild);
[ichildgrd_v,jchildgrd_v]=meshgrid(irchild,jpchild);
%
% interpolations
% 
disp(' ')
disp(' Do the interpolations...')                     
np=netcdf(parent_rst);
for tindex=1:length(thetime)
  disp([' Time index : ',num2str(tindex),' of ',num2str(length(thetime))])                     
  ncrst{'scrum_time'}(tindex)=thetime(tindex);
  ncrst{'time_step'}(tindex,:)= np{'time_step'}(tindex,:);
%
  disp('zeta...')
  interpvar3d(np,ncrst,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'zeta',mask,tindex)
  disp('ubar...')
  interpvar3d(np,ncrst,igrd_u,jgrd_u,ichildgrd_u,jchildgrd_u,'ubar',mask,tindex)
  disp('vbar...')
  interpvar3d(np,ncrst,igrd_v,jgrd_v,ichildgrd_v,jchildgrd_v,'vbar',mask,tindex)
  disp('u...')
  interpvar4d(np,ncrst,igrd_u,jgrd_u,ichildgrd_u,jchildgrd_u,'u',mask,tindex,N)
  disp('v...')
  interpvar4d(np,ncrst,igrd_v,jgrd_v,ichildgrd_v,jchildgrd_v,'v',mask,tindex,N)
  disp('temp...')
  interpvar4d(np,ncrst,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'temp',mask,tindex,N)
  disp('salt...')
  interpvar4d(np,ncrst,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'salt',mask,tindex,N)
  if (biol==1)
    disp('NO3...')
    interpvar4d(np,ncrst,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'NO3',mask,tindex,N)
    disp('CHLA...')
    interpvar4d(np,ncrst,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'CHLA',mask,tindex,N)
    disp('PHYTO...')
    interpvar4d(np,ncrst,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'PHYTO',mask,tindex,N)
  end
end
  result=close(np);
  result=close(ncrst);
%
%  Vertical corrections
%
if (vertical_correc==1)
  for tindex=1:length(thetime)
    disp([' Time index : ',num2str(tindex),' of ',num2str(length(thetime))])                     
    vert_correc(child_rst,tindex,biol)
  end
end
%
% Make a plot
%
if ~isempty(thetime)
  disp(' ')
  disp(' Make a plot...')
  figure(1)
  plot_nestclim(child_rst,child_grd,'temp',1)
else
  disp(' ')
  disp(' Warning : no restart variable to plot...')
end

return
