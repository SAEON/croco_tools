%clear all
%close all
function [my_latriv,my_lonriv,my_flow,my_rivername,number_river]=runoff_glob_extract(grdfile,global_clim_river);
% grdfile=['ROMS_FILES/roms_grd.nc'];
% global_clim_river='runoff_global_clim.nc';
[latr,lonr,maskr]=read_latlonmask(grdfile,'r');
dl=0.0;
minlat=min(min(latr))-dl;
maxlat=max(max(latr))+dl;
minlon=min(min(lonr))-dl;
maxlon=max(max(lonr))+dl;
ncriv=netcdf(global_clim_river,'r');
time=ncriv{'time'}(:);
lonriv_mou=ncriv{'lon_mou'}(:);
latriv_mou=ncriv{'lat_mou'}(:);
ct_name=ncriv{'ct_name'}(:);
cn_name=ncriv{'cn_name'}(:);
riv_name=ncriv{'riv_name'}(:);
ocn_name=ncriv{'ocn_name'}(:);
stn_name=ncriv{'stn_name'}(:);
FLOW_clm=ncriv{'FLOW_clm'}(:);
close(ncriv)

% Find the river in the domain grid
%----------------------------------
my_riv=find(latriv_mou>=minlat & latriv_mou<=maxlat & lonriv_mou >= minlon & lonriv_mou <= maxlon);

disp(['River in the domain :'])
disp(['Number of river= ',num2str(length(my_riv))])
disp(['Domain contains river= '])
for k=1:length(my_riv)
    disp(['- ',riv_name(my_riv(k),:),' flowing in ocean ',ocn_name(my_riv(k),:)])
end
my_flow=FLOW_clm(:,my_riv);
my_rivername=riv_name(my_riv,:);
number_river=length(my_riv);
my_latriv=latriv_mou(my_riv);
my_lonriv=lonriv_mou(my_riv);

figure
plot([1:12],my_flow)
legend(my_rivername)
box on, grid on
title(['\bf Monthly clim of the domain run off']) 
xlabel(['\bf Month']);ylabel(['\bf Discharge in m3/s'])
set(gca,'Xtick',[0.5:11.5],'XtickLabel',['J';'F';'M';'A';'M';'J';'J';'A';'S';'O';'N';'D']);
end
