function [my_latriv,my_lonriv,my_flow,my_rivername,number_river]=runoff_glob_extract(grdfile,global_clim_river);
%
%  [my_latriv,my_lonriv,my_flow,my_rivername,number_river]=runoff_glob_extract(grdfile,global_clim_river);
%  Read all the rivers from the netcdf global climatology file
%  Select the ones located into the model grid
%
disp(' ')
disp(['Reading the global monthly climatological run-off dataset... '])
disp(' ')
%
%  Read the grid
%
[latr,lonr,maskr]=read_latlonmask(grdfile,'r');
dl=0.0;
minlat=min(min(latr))-dl;
maxlat=max(max(latr))+dl;
minlon=min(min(lonr))-dl;
maxlon=max(max(lonr))+dl;
%
%  Read the global rivers
%
ncriv=netcdf(global_clim_river,'r');
time=ncriv{'time'}(:);
lonriv_mou=ncriv{'lon_mou'}(:);
latriv_mou=ncriv{'lat_mou'}(:);
ct_name=ncriv{'ct_name'}(:);
cn_name=ncriv{'cn_name'}(:);
warning off
riv_name=ncriv{'riv_name'}(:);
ocn_name=ncriv{'ocn_name'}(:);
stn_name=ncriv{'stn_name'}(:);
warning on
FLOW_clm=ncriv{'FLOW_clm'}(:);
close(ncriv)
%size(FLOW_clm)
%
% Select the rivers in the domain grid
%-------------------------------------
%
%========================================
%River selection criteria
maxmaxflow=(max(max(FLOW_clm)));
maxflow=(max(FLOW_clm)');  % attention transpose

meanflowmax=mean(FLOW_clm(:,1));    % pour amazon
meanflow=nanmean(FLOW_clm(:,:),1)'; % pour les autres

%size(meanflow)
%plot(meanflow)
%size(maxflow)

%==========================================================================================================
%% => by default : all rivers in the domain 
rivdetectype='DEFAULT';
my_riv=find(latriv_mou>=minlat & latriv_mou<=maxlat & lonriv_mou >= minlon & lonriv_mou <= maxlon);

%==

% => megatl : rivers in the boxes + max value >= 20%*max val of the flow amazonia peak)
%rivdetectype='MEGATL';
%thold=0.1;
%thold=0.03;
%my_riv=find(latriv_mou>=minlat & latriv_mou<=maxlat & lonriv_mou >= minlon & lonriv_mou <= maxlon & meanflow >= thold.*meanflowmax);
%========================================
%
disp(['There are ',num2str(length(my_riv)),' rivers in the domain : '])
disp(['===='])
disp(' ')
disp([' => Rivers detection type : ',rivdetectype,' (see in runoff_glob_extract.m)'])
disp(' ')
disp(['===='])
disp(['There are ',num2str(length(my_riv)),' rivers in the domain : '])
disp(['Domain contains rivers :'])
for k=1:length(my_riv)
   disp([num2str(k),' - ',riv_name(my_riv(k),:),' flowing in ocean ',ocn_name(my_riv(k),1:4)])
end
my_flow=FLOW_clm(:,my_riv);
my_rivername=riv_name(my_riv,:);
number_river=length(my_riv);
my_latriv=latriv_mou(my_riv);
my_lonriv=lonriv_mou(my_riv);
%
% make a figure
%
if ~isempty(my_flow)
    figure(100)
    plot([1:12],my_flow)
    legend(my_rivername,'location','northeastoutside')
    box on, grid on
    title(['\bf Monthly clim of the domain run off'])
    xlabel(['\bf Month']);ylabel(['\bf Discharge in m3/s'])
    set(gca,'Xtick',[0.5:11.5],'XtickLabel',['J';'F';'M';'A';'M';'J';'J';'A';'S';'O';'N';'D']);
end
%
return
