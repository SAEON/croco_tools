function nested_initial(child_grd,parent_ini,child_ini,...
                        vertical_correc,extrapmask,biol,pisces)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Compute the initial file of the embedded grid
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
%tindex=1;

%Check the number of variables in the clim parents file
a= netcdf(parent_ini);
Vars = var(a);
Varnames = [ncnames(Vars)];
nvar=length(Varnames);
isbiolfiles=0;
ispiscesfiles=0;
%
namebiol={''};
unitbiol={''};
namepisces={''};
unitpisces={''};
% pisces
% biol
if biol
  %Name, units etc .. of the variables
  namebiol={'NO3';'CHLA';'PHYTO';'ZOO'};
  unitbiol={'mMol N m-3';'mg C l-1';'mMol N m-3'};
  for i=1:length(namebiol)
	aa=sum(strcmp(Varnames,namebiol(i)));
	isbiolfiles=isbiolfiles+aa;
  end
  if isbiolfiles==length(namebiol)
	disp('Compute Bio NPZD variables')
	disp('==================')
  else
	disp(sprintf(['ERROR in NPZD Processing : ... \n', ...
	  'You don not have the neccesary variables in the clim file \n',...
	  'or you didn''t choose the right bio. model. \n', ...
	  'Check roms_ini.nc parent file and make_ini.m']))
	return
  end
end
 if pisces
   namepisces={'NO3';'PO4';'Si';'O2';'DIC';'TALK';'DOC';'FER'};
   unitpisces={'mMol N m-3';'mMol P m-3';'mMol Si m-3';'mMol O m-3';'mMol C m-3';'mMol C m-3';'mMol C m-3';'uMol Fe m-3'};
   %
   for i=1:length(namepisces)
	 aa=sum(strcmp(Varnames,namepisces(i)));
	 ispiscesfiles=ispiscesfiles+aa;
   end
   if ispiscesfiles==length(namepisces)
	 disp('Compute Pisces biogeochemical variables')
	 disp('==================')
   else
	 disp(sprintf(['ERROR in  PISCES Processing :  \n', ...
	   'You don''t have the neccesary variables in the clim file \n',...
	   'or you didn''t choose the right bio. model. \n', ...
	   'Check roms_ini.nc parent file and make_ini.m']))
	 return
   end
 end
if extrapmask==1
  disp('Extrapolation under mask is on')
  disp('====================')
end
%
if vertical_correc==1
  disp('Vertical correction is on')
  disp('===============')
end
%
if pisces & biol 
  error(['Both Biol NPZD and Pisces are ON, not possible yet... !'])
end


%
% Title
%
title=['Initial file for the embedded grid :',child_ini,...
       ' using parent initial file: ',parent_ini];
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
if pisces==1
  disp('Pisces: on')
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
% Read in the parent initial file
%
disp(' ')
disp(' Read in the parent initial file...')
nc = netcdf(parent_ini);
theta_s = nc{'theta_s'}(:);
theta_b = nc{'theta_b'}(:);
Tcline = nc{'Tcline'}(:);
N = length(nc('s_rho'));
thetime = nc{'scrum_time'}(:);
result=close(nc);
%
% Create the initial file
%
disp(' ')
disp(' Create the initial file...')
ncini=create_nestedinitial(child_ini,child_grd,parent_ini,title,...
                           theta_s,theta_b,Tcline,N,thetime,'clobber',...
                           biol,pisces,namebiol,namepisces,unitbiol,unitpisces);
%
% parent indices
%
[igrd_r,jgrd_r]=meshgrid((1:1:Lp),(1:1:Mp));
[igrd_p,jgrd_p]=meshgrid((1:1:Lp-1),(1:1:Mp-1));
[igrd_u,jgrd_u]=meshgrid((1:1:Lp-1),(1:1:Mp));
[igrd_v,jgrd_v]=meshgrid((1:1:Lp),(1:1:Mp-1));
%
% the children indices
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
np=netcdf(parent_ini);
for tindex=1:length(thetime)
  disp('zeta...')
  interpvar3d(np,ncini,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'zeta',mask,tindex)
  disp('ubar...')
  interpvar3d(np,ncini,igrd_u,jgrd_u,ichildgrd_u,jchildgrd_u,'ubar',mask,tindex)
  disp('vbar...')
  interpvar3d(np,ncini,igrd_v,jgrd_v,ichildgrd_v,jchildgrd_v,'vbar',mask,tindex)
  disp('u...')
  interpvar4d(np,ncini,igrd_u,jgrd_u,ichildgrd_u,jchildgrd_u,'u',mask,tindex,N)
  disp('v...')
  interpvar4d(np,ncini,igrd_v,jgrd_v,ichildgrd_v,jchildgrd_v,'v',mask,tindex,N)
  disp('temp...')
  interpvar4d(np,ncini,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'temp',mask,tindex,N)
  disp('salt...')
  interpvar4d(np,ncini,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'salt',mask,tindex,N)
  %
  if (biol==1)
    disp('NO3...')
    interpvar4d(np,ncini,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'NO3',mask,tindex,N)
    disp('CHLA...')
    interpvar4d(np,ncini,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'CHLA',mask,tindex,N)
    disp('PHYTO...')
    interpvar4d(np,ncini,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'PHYTO',mask,tindex,N)
    disp('ZOO...')
    interpvar4d(np,ncini,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'ZOO',mask,tindex,N)
  end
  %
  if (pisces==1)
    disp('NO3...')
    interpvar4d(np,ncini,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'NO3',mask,tindex,N)
    disp('PO4...')
    interpvar4d(np,ncini,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'PO4',mask,tindex,N)
    disp('Si...')
    interpvar4d(np,ncini,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'Si',mask,tindex,N)
    disp('O2...')
    interpvar4d(np,ncini,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'O2',mask,tindex,N)
    disp('DIC...')
    interpvar4d(np,ncini,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'DIC',mask,tindex,N)
    disp('TALK...')
    interpvar4d(np,ncini,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'TALK',mask,tindex,N)
    disp('DOC...')
    interpvar4d(np,ncini,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'DOC',mask,tindex,N)
    disp('FER...')
    interpvar4d(np,ncini,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'FER',mask,tindex,N)
  end
  %
end
result=close(np);
result=close(ncini);
%
%  Vertical corrections
%
if (vertical_correc==1)
  for tindex=1:length(thetime)
    disp([' Time index : ',num2str(tindex),' of ',num2str(length(thetime))])                     
    vert_correc(child_ini,tindex,biol,pisces,namebiol,namepisces)
  end
end
%
% Make a plot
%
disp(' ')
disp(' Make a plot...')
figure(1)
plot_nestclim(child_ini,child_grd,'temp',1)
return

