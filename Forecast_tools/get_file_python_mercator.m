function outname = get_file_python_mercator(pathmotu,vars,geom,date,info,outname)

% Usage: 
%=======
% get_file_python_mercator(pathMotu,{'varname1' 'varname2'},[lonmin lonmax latmin latmax
% depthmin depthmax],{'startdate' 'enddate'},{user password},'outname.nc');
% 
% Version 0.0.0,1
%=================
eval(['! rm ',outname])
command = {'! '
	   sprintf('%s',pathmotu)
	   'motu-client-python/motu-client.py '
	   sprintf('-u %s -p %s ',info{1},info{2}) 
	   '-m http://atoll.mercator-ocean.fr/mfcglo-mercator-gateway-servlet/Motu? '
	   '-s http://purl.org/myocean/ontology/service/database%23GLOBAL_ANALYSIS_FORECAST_PHYS_001_001_a-TDS '
	   '-d dataset-psy3v3-pgs-glo-myocean-bestestimate '
	   sprintf('-o ./ ')
	   sprintf('-t %s -T %s ',date{1},date{2})
	   sprintf('-x %f -X %f ',geom(1),geom(2))
	   sprintf('-y %f -Y %f ',geom(3),geom(4))
	   sprintf('-z %f -Z %f ',geom(5),geom(6))
	   sprintf('--out-name %s ',outname)};
	
for k =1:length(vars)
  command{end+1}=sprintf('-v %s ',vars{k});
  %command{end+2}=sprintf('--proxy-server=http://proxy.legos.obs-mip.fr:3128');
end
%command{end+2}=sprintf('--proxy-server=http://www.legos.obs-mip.fr/protect/proxy.pac');
disp([command{:}])
eval([command{:}])
