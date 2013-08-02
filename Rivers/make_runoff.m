clear all
close all
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Create a ROMS run-off forcing file
%
%
%  Further Information:
%  http://www.romsagrif.org
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
%
%  July 2013: G.Cambon (IRD/LEGOS) & M. Herrmann (IRD/LEGOS)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%% USERS DEFINED VARIABLES %%%%%%%%%
clear all
close all
%%%%%%%%%%%%%%%%%%%%% USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%%
%
romstools_param
disp(['Create runoff forcing from Dai and Trenberth''s global monthly climatological run-off dataset'])
title_name='runoff forcing file (Dai and Trenberth, 2002 dataset)';
%
plotting=1;
define_dir=0 ;  %->flag to define directly the dir/sen of the runoff
if define_dir
    % Define direction/sense of the flow. First column is the u- (0) or v- (1)
    % direction. Second column is the sense of the flow in the choosen direction
    dir(1,:)=[0, -1];
    dir(2,:)=[0, -1];
    dir(3,:)=[1, -1];
end
if psource_ts
    % Define manually (and uncomment) the tracer (t, s, and eventually biogeochemical tracer
    % concentration
    %     temp_src0=[11 9 9 12 20 20 24 25 21 18 13 12];
    %     temp_src(:,:)=[temp_src0;temp_src0+2;temp_src0+2.8];
    %     %
    %     salt_src0=[2 3 5 1 5 3 2 1 4 2 1 2];
    %     salt_src(:,:)=[salt_src0;salt_src0;salt_src0];
    %     %
    %     no3_src0=[0 0 0 0 0 0 0 0 0 0 0 0];
    %     no3_src(:,:)=[no3_src0;no3_src0+2;no3_src0+2.8];
end
%
%%%%%%%%%%%%%%%%%%% END USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%
%
[latriv,lonriv,my_flow,myrivername,rivernumber]=runoff_glob_extract(grdname,global_clim_rivername);
latriv=latriv';
lonriv=lonriv';

if psource_ts
    my_temp_src = temp_src;
    my_salt_src = salt_src;
    my_no3_src  = no3_src;
end
%
rivername=strvcat(myrivername);
rivernumber=size(rivername,1);
rivname_StrLen=size(rivername,2);
%
% Determine the positions of the river, from its lon/lat position
% extract j and i to put in roms.in / roms.in.1 for that use of roms_grd.nc
%
[lat,lon,mask]=read_latlonmask(grdname,'r');
[latu,lonu,masku]=read_latlonmask(grdname,'u');
[latv,lonv,maskv]=read_latlonmask(grdname,'v');
%
disp(['First guess:'])
disp(['============'])

for k= 1:rivernumber
    disp([' '])
    disp(['- Process river #',num2str(k),': ',char(myrivername(k,:))])
    indomain(k)=check_domain_runoff(lon,lat,lonriv(k),latriv(k));
%    if indomain(k)
    [j,i]=runoff_grid_pos(lon,lat,lonriv(k),latriv(k));
    J(k)=j;     I(k)=i;
    disp(['  Position is approximetly  J=',num2str(J(k)),' and I=',num2str(I(k))])
%     else
%     disp(['The river is not in the domain...'])
%     disp(['it will not be considered...'])
%     J(k)=1;     I(k)=1;
%     end
        disp(['lon src in grid (rho point) ~',num2str(lon(J(k),I(k)))])
        disp(['lat src in grid (rho point) ~',num2str(lat(J(k),I(k)))])
end
%
% Check the river you really have to process.
% Remove rivers out of the domain, if any...
%
rivertoprocess=find(indomain==1);
number_rivertoprocess=length(rivertoprocess);
%
rivername=strvcat(myrivername(rivertoprocess,:));
rivernumber=number_rivertoprocess;
rivname_StrLen=size(rivername,2);
%
plotting=1;
if plotting
    figure
    pcolor(lonv,latv,maskv);
    hold on;
    
    for k0=1:number_rivertoprocess
        k=rivertoprocess(k0);
        aa=plot(lon(J(k),I(k)),lat(J(k),I(k)),'or');
        legend(aa,'first guess position');
        cc=text(lon(J(k),I(k))-0.5,lat(J(k),I(k))-0.5,myrivername(k,:));
        set(cc,'fontweight','demi','fontsize',13);
    end
end
%
% Choose the river you really want to process...
%
for k = 1 : number_rivertoprocess
   indomain_last=indomain;
   indomain_last(k)=input(['Do you want to use river (Yes[1], No[0]) ?  ', rivername(k,:)]);
end
rivertoprocess=find(indomain_last==1);
number_rivertoprocess=length(rivertoprocess);
%
rivername=strvcat(myrivername(rivertoprocess,:));
rivernumber=number_rivertoprocess;
rivname_StrLen=size(rivername,2);
my_flow=my_flow(:,find(indomain_last==1));
myrivername=myrivername(find(indomain_last==1),:);
if define_dir==0
%
% Define the direction/sense of the flow. First column is the u- (0) or v- (1) 
% direction. Second column is the sense of the flow in the choosen direction
%    
    for k0=1:number_rivertoprocess
        k=rivertoprocess(k0);
        disp(['====='])
        disp(['River ',rivername(k,:)])
        disp(['Choose the orientation of the flow'])
        dir11 = input('0=zonal or 1=meridional. ');
        if (dir11~=0 && dir11~=1)
            error([num2str(dir11),' : Wrong choice ...'])
        end
        disp(['Choose the sense of the flow. '])
        dir12= input('0 is positive [S-N or W-E], -1 negative [E-W or N-S]. ');
        if (dir12~=0 && dir12~=-1)
            error([num2str(dir12),' : Wrong choice ...'])
        end
        dir(k,:)=[dir11 dir12];
    end
end
%
% Create the runoff forcing file
%
disp(' ')
disp(' Create the runoff file...')
create_runoff(rivname,grdname,title_name,...
    qbar_time,qbar_cycle, ...
    temp_src_time,temp_src_cycle,...
    salt_src_time,salt_src_cycle,...
    rivername,rivernumber,rivname_StrLen,dir,psource_ts,makebio)
%
% Determine where is the rivers
%
disp(['Find the real positions of the rivers in the grid: '])
disp(['========================================'])
for k0=1:rivernumber
    k=k0+0;
    disp(['Process final position for river ',rivername(k,:)])
        disp(['Choose the orientation'])  
    jj=J(k); ii=I(k);dir2=dir(k,:);
    [jj2,ii2]=locate_runoff(dir2,jj,ii,mask,masku,maskv);
    J2(k)=jj2; I2(k)=ii2;
    disp([char(myrivername(k,:)),' is J=',num2str(J2(k)),' and I=',num2str(I2(k))])
    disp([' '])
end
if plotting
    hold on;
    for k=1:rivernumber
        bb=plot(lonv(J2(k),I2(k)),latv(J2(k),I2(k)),'ok');
    end
    legend([aa,bb],{'Approximative first guess river location','final adjusted river location'});
    title({'\bf Location of river in the roms grid';'(from Dai and Trenberth dataset)'});
end
%
% Fill the river discharge and eventually
% t/s concentration, no3 concentration
%
nw=netcdf(rivname,'w');
disp(['Write in runoff file'])
nw{'river_position'}(:,:)=[I2 J2];
nw{'river_direction'}(:,:)=dir;
disp(['... river positions'])
% Write qbar, temp,salt and bgc varibales conc.
cff=150;
disp(['================== WARNING ============================'])
disp(['Use a cff coef to increase the discharge= ',num2str(cff)])
disp(['======================================================='])
my_flow=cc.*my_flow;
nw{'Qbar'}(:) = my_flow;
disp(['... discharges'])
if psource_ts
    nw{'temp_src'}(:) = my_temp_src(rivertoprocess,:);
    disp(['... temperature concentration'])
    nw{'salt_src'}(:) = my_salt_src(rivertoprocess,:);
    disp(['... salt concentration'])
    if makebio
        nw{'NO3_src'}(:) = my_no3_src(rivertoprocess,:);
        disp(['... NO3 concentration'])
    end
end
close(nw)
%
% Line to enter in the roms.in file in the psource section
%
disp([' '])
disp(['Line to enter in the roms.in file in the psource_ncfile section :'])
disp(['-----------------------------------------------------------------'])
disp(['psource_ncfile:   Nsrc  Isrc  Jsrc  Dsrc qbardir  Lsrc  Tsrc   runoff file name'])
disp(['                           ROMS_FILES/roms_runoff.nc'])
disp(['                 ',num2str(rivernumber)'])
for k=1:rivernumber
    if dir(k,1) == 1     %flow meridien
        disp(['                        ',num2str(I2(k)-1),'  ',num2str(J2(k)),'  ',num2str(dir(k,1)),'  ',num2str(dir(k,2)),'   30*T   5.0  0.0'])
    elseif dir(k,1) == 0 %flow zonal
        disp(['                        ',num2str(I2(k)),'  ',num2str(J2(k)-1),'  ',num2str(dir(k,1)),'  ',num2str(dir(k,2)),'   30*T   5.0  0.0'])
    end
end
disp(['-----------------------------------------------------------------'])
   
%
% Plot the seasonal cycle
%
figure
for k=1: rivernumber
hold on
plot([1:12],my_flow)
legend(myrivername)
box on, grid on
title(['\bf Monthly clim of the domain run off']) 
xlabel(['\bf Month']);ylabel(['\bf Discharge in m3/s'])
set(gca,'Xtick',[0.5:11.5],'XtickLabel',['J';'F';'M';'A';'M';'J';'J';'A';'S';'O';'N';'D']);
end