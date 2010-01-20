function nested_clim(child_grd,parent_clim,child_clim,...
                     vertical_correc,extrapmask,biol,pisces)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  compute the climatology of the embedded grid
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

%Check the number of variables in the clim parents file
a= netcdf(parent_clim);
Vars = var(a);
Varnames = [ncnames(Vars)];
nvar =length(Varnames);
%
% Initialisation
%
% biol
% pisces

namebiol={''};
unitbiol={''};
% timebiol={''};
% Utilisation de char et str2num !!
% A faire pour simplifier le code et le rendre plus modulable.
no3_time=[];
no3_cycle=[];
chla_time=[];
chla_cycle=[];
phyto_time=[];
phyto_cycle=[];
zoo_time=[];
zoo_cycle=[];
% biol_time=(15:30:345); % days: middle of each month
% biol_cycle=360; % repetition of a typical year of 360 days
%--
namepisces={''};
unitpisces={''};
%timepisces={''};
% Utilisation de char et str2num !!
% A faire pour simplifier le code et le rendre plus modulable.
no3p_time=[];
no3p_cycle=[];
po4_time=[];
po4_cycle=[];
si_time=[];
si_cycle=[];
o2_time=[];
o2_cycle=[];
dic_time=[];
dic_cycle=[];
talk_time=[];
talk_cycle=[];
doc_time=[];
doc_cycle=[];
fer_time=[];
fer_cycle=[];
%--
% Chech type of clim file
%
if nvar <= 26
elseif (nvar <=  34 & biol)
  %Name, units etc .. of the variables
  namebiol={'NO3';'CHLA';'PHYTO';'ZOO'};
  unitbiol={'mMol N m-3';'mg C l-1';'mMol N m-3'};
  %timebiol={'no3_time';'chla_time';'phyto_time';'zoo_time'};
  disp(['Compute Biological variables type NPZD : '])
  disp(['NchlPZD or N2ChlZD2 or N2P2Z2D2         '])
  disp('==========================')
elseif (pisces & nvar>=35)
  %Name, units etc .. of the variables
  namepisces={'NO3';'PO4';'Si';'O2';'DIC';'TALK';'DOC';'FER'};
  unitpisces={'mMol N m-3';'mMol P m-3';'mMol Si m-3';'mMol O m-3';'mMol C m-3';'mMol C m-3';'mMol C m-3';'uMol Fe m-3'};
  %timepisces={'no3_time';'po4_time';'si_time';'o2_time';'dic_time';'talk_time';'doc_time';'fer_time'};
  disp('Compute Pisces biogeochemical variables')
  disp('=========================')
else
  error(sprintf(['You don''t have the neccesary variables in the clim file. \n',...
		 'or you didn''t choose the right bio. model. \n',...
		 'Check roms_ini.nc parent file and make_clim.m']))
end
%
% Title
%
title=['Climatology file for the embedded grid :',child_clim,...
       ' using parent forcing file: ',parent_clim];
disp(' ')
disp(title)
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
  error(['Both Biol NPZD and Pisces are ON, no possible yet...!'])
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
% Read in the parent climatology file
%
disp(' ')
disp(' Read in the parent climatology file...')
nc = netcdf(parent_clim);
theta_s = nc{'theta_s'}(:);
theta_b = nc{'theta_b'}(:);
Tcline = nc{'Tcline'}(:);
N = length(nc('s_rho'));
ttime = nc{'tclm_time'}(:);
tcycle = nc{'tclm_time'}.cycle_length(:);
stime = nc{'sclm_time'}(:);
scycle = nc{'sclm_time'}.cycle_length(:);
utime = nc{'ssh_time'}(:);
ucycle = nc{'ssh_time'}.cycle_length(:);
vtime = nc{'ssh_time'}(:);
vcycle = nc{'ssh_time'}.cycle_length(:);
sshtime = nc{'ssh_time'}(:);
sshcycle = nc{'ssh_time'}.cycle_length(:);
%
if biol==1
  % for k=1:length(namebiol)
  % timebio(k)= nc{char(timebiol(k))}(:);
  % cyclebio(k)= nc{char(timebiol(k))}.cycle_length(:);
  no3_time = nc{'no3_time'}(:);
  no3_cycle = nc{'no3_time'}.cycle_length(:)
  chla_time = nc{'chla_time'}(:);
  chla_cycle = nc{'chla_time'}.cycle_length(:);
  phyto_time = nc{'phyto_time'}(:);
  phyto_cycle = nc{'phyto_time'}.cycle_length(:);
  zoo_time = nc{'zoo_time'}(:);
  zoo_cycle = nc{'zoo_time'}.cycle_length(:);
end
%
if pisces==1
  no3p_time = nc{'no3_time'}(:);
  no3p_cycle = nc{'no3_time'}.cycle_length(:);
  po4_time = nc{'po4_time'}(:);
  po4_cycle = nc{'po4_time'}.cycle_length(:);
  si_time = nc{'si_time'}(:);
  si_cycle = nc{'si_time'}.cycle_length(:);
  o2_time = nc{'o2_time'}(:);
  o2_cycle = nc{'o2_time'}.cycle_length(:);
  dic_time = nc{'dic_time'}(:);
  dic_cycle = nc{'dic_time'}.cycle_length(:);
  talk_time = nc{'talk_time'}(:);
  talk_cycle = nc{'talk_time'}.cycle_length(:);
  doc_time = nc{'doc_time'}(:);
  doc_cycle = nc{'doc_time'}.cycle_length(:);
  fer_time = nc{'fer_time'}(:);
  fer_cycle = nc{'fer_time'}.cycle_length(:);
end
%
result=close(nc);

climtime=ttime;
if stime~=climtime | utime~=climtime | vtime~=climtime | sshtime~=climtime
  error('Nested_clim: different times... ')
end
%
% Create the climatology file
%
disp(' ')
disp(' Create the climatology file...')
ncclim=create_nestedclim(child_clim,child_grd,parent_clim,title,...
			 theta_s,theta_b,Tcline,N,...
			 ttime,stime,utime,vtime,sshtime,...
			 tcycle,scycle,ucycle,vcycle,sshcycle,...
			 no3p_time,po4_time,si_time,o2_time,dic_time,talk_time, doc_time,fer_time,...
			 no3p_cycle,po4_cycle,si_cycle,o2_cycle,dic_cycle,talk_cycle, doc_cycle,fer_cycle,...
			 no3_time,chla_time,phyto_time,zoo_time, ...
			 no3_cycle,chla_cycle,phyto_cycle,zoo_cycle, ...
			 'clobber',...
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
np=netcdf(parent_clim);
disp('u...')
for tindex=1:length(climtime)
  disp([' Time index : ',num2str(tindex),' of ',num2str(length(climtime))]) 
  interpvar4d(np,ncclim,igrd_u,jgrd_u,ichildgrd_u,jchildgrd_u,'u',mask,tindex,N)
end
disp('v...')
for tindex=1:length(climtime)
  disp([' Time index : ',num2str(tindex),' of ',num2str(length(climtime))]) 
  interpvar4d(np,ncclim,igrd_v,jgrd_v,ichildgrd_v,jchildgrd_v,'v',mask,tindex,N)
end
disp('zeta...')
for tindex=1:length(climtime)
  disp([' Time index : ',num2str(tindex),' of ',num2str(length(climtime))]) 
  interpvar3d(np,ncclim,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'SSH',mask,tindex)
end
disp('ubar...')
for tindex=1:length(climtime)
  disp([' Time index : ',num2str(tindex),' of ',num2str(length(climtime))]) 
  interpvar3d(np,ncclim,igrd_u,jgrd_u,ichildgrd_u,jchildgrd_u,'ubar',mask,tindex)
end
disp('vbar...')
for tindex=1:length(climtime)
  disp([' Time index : ',num2str(tindex),' of ',num2str(length(climtime))]) 
  interpvar3d(np,ncclim,igrd_v,jgrd_v,ichildgrd_v,jchildgrd_v,'vbar',mask,tindex)
end
disp('temp...')
for tindex=1:length(climtime)
  disp([' Time index : ',num2str(tindex),' of ',num2str(length(climtime))]) 
  interpvar4d(np,ncclim,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'temp',mask,tindex,N)
end
disp('salt...')
for tindex=1:length(climtime)
  disp([' Time index : ',num2str(tindex),' of ',num2str(length(climtime))]) 
  interpvar4d(np,ncclim,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'salt',mask,tindex,N)
end
%
%%
%
if biol
  disp('NO3...')   
  for tindex=1:length(no3_time)
    disp([' Time index : ',num2str(tindex),' of ',num2str(length(no3_time))]) 
    interpvar4d(np,ncclim,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'NO3',mask,tindex,N)
  end
  disp('CHLA...')  
  for tindex=1:length(chla_time)
    disp([' Time index : ',num2str(tindex),' of ',num2str(length(chla_time))]) 
    interpvar4d(np,ncclim,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'CHLA',mask,tindex,N)
  end
  disp('PYTHO...')  
  for tindex=1:length(phyto_time)
    disp([' Time index : ',num2str(tindex),' of ',num2str(length(phyto_time))]) 
    interpvar4d(np,ncclim,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'PHYTO',mask,tindex,N)
  end
  disp('ZOO...')  
  for tindex=1:length(zoo_time)
    disp([' Time index : ',num2str(tindex),' of ',num2str(length(zoo_time))]) 
    interpvar4d(np,ncclim,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'ZOO',mask,tindex,N)
  end
end
%
%%
%
if pisces
  disp('NO3...')   
  for tindex=1:length(no3p_time)
    disp([' Time index : ',num2str(tindex),' of ',num2str(length(no3p_time))]) 
    interpvar4d(np,ncclim,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'NO3',mask,tindex,N)
  end
  disp('PO4...')  
  for tindex=1:length(po4_time)
    disp([' Time index : ',num2str(tindex),' of ',num2str(length(po4_time))]) 
    interpvar4d(np,ncclim,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'PO4',mask,tindex,N)
  end
  disp('Si...')  
  for tindex=1:length(si_time)
    disp([' Time index : ',num2str(tindex),' of ',num2str(length(si_time))]) 
    interpvar4d(np,ncclim,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'Si',mask,tindex,N)
  end
  disp('O2...')  
  for tindex=1:length(o2_time)
    disp([' Time index : ',num2str(tindex),' of ',num2str(length(o2_time))]) 
    interpvar4d(np,ncclim,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'O2',mask,tindex,N)
  end
  disp('DIC...')  
  for tindex=1:length(dic_time)
    disp([' Time index : ',num2str(tindex),' of ',num2str(length(dic_time))]) 
    interpvar4d(np,ncclim,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'DIC',mask,tindex,N)
  end
  disp('TALK...')  
  for tindex=1:length(talk_time)
    disp([' Time index : ',num2str(tindex),' of ',num2str(length(talk_time))]) 
    interpvar4d(np,ncclim,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'TALK',mask,tindex,N)
  end
  disp('DOC...')  
  for tindex=1:length(doc_time)
    disp([' Time index : ',num2str(tindex),' of ',num2str(length(doc_time))]) 
    interpvar4d(np,ncclim,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'DOC',mask,tindex,N)
  end
  disp('FER...')  
  for tindex=1:length(fer_time)
    disp([' Time index : ',num2str(tindex),' of ',num2str(length(fer_time))]) 
    interpvar4d(np,ncclim,igrd_r,jgrd_r,ichildgrd_r,jchildgrd_r,'FER',mask,tindex,N)
  end
end
%
%%
%
close(np);
close(ncclim);
%
%  Vertical corrections
%
if (vertical_correc==1)
  disp('Process variable physical variables')
  for tindex=1:length(climtime)
    disp([' Time index : ',num2str(tindex),' of ',num2str(length(climtime))])                     
    vert_correc(child_clim,tindex,0,0,namebiol,namepisces)
  end
  %%
  if biol
    disp('Process variable NPZD')
    for tindex=1:length(no3_time)
      disp([' Time index : ',num2str(tindex),' of ',num2str(length(no3_time))]) 
      vert_correc_onefield(child_clim,tindex,'NO3')
    end
    for tindex=1:length(chla_time)
      disp([' Time index : ',num2str(tindex),' of ',num2str(length(chla_time))]) 
      vert_correc_onefield(child_clim,tindex,'CHLA')
    end      
    for tindex=1:length(phyto_time)
      disp([' Time index : ',num2str(tindex),' of ',num2str(length(phyto_time))]) 
      vert_correc_onefield(child_clim,tindex,'PHYTO')
    end    
    for tindex=1:length(zoo_time)
      disp([' Time index : ',num2str(tindex),' of ',num2str(length(zoo_time))]) 
      vert_correc_onefield(child_clim,tindex,'ZOO')
    end         
  end
  %%
  if pisces
    disp('Process variable PISCES')
    %to be finished  ....
    for tindex=1:length(no3_time)
      disp([' Time index : ',num2str(tindex),' of ',num2str(length(no3_time))]) 
      vert_correc_onefield(child_clim,tindex,'NO3')
    end
    for tindex=1:length(po4_time)
      disp([' Time index : ',num2str(tindex),' of ',num2str(length(po4_time))]) 
      vert_correc_onefield(child_clim,tindex,'PO4')
    end
    for tindex=1:length(si_time)
      disp([' Time index : ',num2str(tindex),' of ',num2str(length(si_time))]) 
      vert_correc_onefield(child_clim,tindex,'Si')
    end
    for tindex=1:length(o2_time)
      disp([' Time index : ',num2str(tindex),' of ',num2str(length(o2_time))]) 
      vert_correc_onefield(child_clim,tindex,'O2')
    end
    for tindex=1:length(dic_time)
      disp([' Time index : ',num2str(tindex),' of ',num2str(length(dic_time))]) 
      vert_correc_onefield(child_clim,tindex,'DIC')
    end
    for tindex=1:length(talk_time)
      disp([' Time index : ',num2str(tindex),' of ',num2str(length(talk_time))]) 
      vert_correc_onefield(child_clim,tindex,'TALK')
    end
    for tindex=1:length(doc_time)
      disp([' Time index : ',num2str(tindex),' of ',num2str(length(doc_time))]) 
      vert_correc_onefield(child_clim,tindex,'DOC')
    end
    for tindex=1:length(fer_time)
      disp([' Time index : ',num2str(tindex),' of ',num2str(length(fer_time))]) 
      vert_correc_onefield(child_clim,tindex,'FER')
    end       
  end
end
%
% Make a plot
%
disp(' ')
disp(' Make a plot...')
figure(1)
plot_nestclim(child_clim,child_grd,'temp',4)
return
