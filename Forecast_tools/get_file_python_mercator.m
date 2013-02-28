function outname = get_file_python_mercator(pathmotu,vars,geom,date,info,outname)

% Usage: 
%=======
% get_file_python_mercator(pathMotu,{'varname1' 'varname2'},[lonmin lonmax latmin latmax
% depthmin depthmax],{'startdate' 'enddate'},{user password},'outname.nc');
% 
% Version 0.0.0,2
%=================
eval(['! rm ',outname])
command = {'! '
	   sprintf('%s',pathmotu)
	   'motu-client-python/motu-client.py '
	   sprintf('-u %s -p %s ',info{1},info{2}) 
	   '-m http://atoll.mercator-ocean.fr/mfcglo-mercator-gateway-servlet/Motu '
	   '-s http://purl.org/myocean/ontology/service/database#GLOBAL_ANALYSIS_FORECAST_PHYS_001_001_c-TDS '
	   '-d global-analysis-forecast-phys-001-001-c-glo '
	   sprintf('-o ./ ')
	   sprintf('-t %s -T %s ',date{1},date{2})
	   sprintf('-x %f -X %f ',geom(1),geom(2))
	   sprintf('-y %f -Y %f ',geom(3),geom(4))
	   sprintf('-z %f -Z %f ',geom(5),geom(6))
	   sprintf('--out-name %s ',outname)};
	
for k =1:length(vars)
  command{end+1}=sprintf('-v %s ',vars{k});
end
%===
% If you use proxy server, uncomment next line and fill it correctly
% Take care need a SPACE between "--proxy-server=" and "the proxy-server-name" !
%command{end+2}=sprintf('--proxy-server= http://your_proxy_server:your_proxy_port');
%===
disp([command{:}])
eval([command{:}])
