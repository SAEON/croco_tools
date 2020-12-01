function outname = get_file_python_mercator(pathmotu,mercator_type,...
                                            motu_url,service_id,product_id, ...
                                            vars, ...
                                            geom,date,info,outname)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Extract a subgrid from MERCATOR to get a CROCO forcing
%     (Store that into monthly files.
%      Take care of the Greenwitch Meridian.)
% 
%  Further Information:  
%  http://www.croco-ocean.org
%  
%  This file is part of CROCOTOOLS
%
%  CROCOTOOLS is free software; you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published
%  by the Free Software Foundation; either version 2 of the License,
%  or (at your option) any later version.
%
%  CROCOTOOLS is distributed in the hope that it will be useful, but
%  WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program; if not, write to the Free Software
%  Foundation, Inc., 59 Temple Place, Suite 330, Boston,
%  MA  02111-1307  USA
%
%  Copyright (c) 2006 by Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr
%
%  Updated   12-Feb-2016 by P. Marchesiello
%  Updated   26-Nov-2020 by G. Cambon
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Usage: 
%=======
% get_file_python_mercator(pathMotu,mercator_type,...
%                          motu_url,service_id,product_id, ...
%                          {'varname1' 'varname2'}, ...
%                          [lonmin lonmax latmin latmax, depthmin depthmax], ...
%                          {'startdate' 'enddate'},{user password},
%                          'outname.nc');
% 
% For GLORYS 12 reanalysis
% ========================
% motu_url='http://my.cmems-du.eu/motu-web/Motu';
% service_id='GLOBAL_REANALYSIS_PHY_001_030-TDS';
% product_id='global-reanalysis-phy-001-030-daily';
%
% For Mercator 1/12 forecast
% ==========================
% motu_url='http://nrt.cmems-du.eu/motu-web/Motu';
% service_id='GLOBAL_ANALYSIS_FORECAST_PHY_001_024-TDS';
% product_id='global-analysis-forecast-phy-001-024';
%
% For Met-Office 1/4 forecast
% ===========================
% motu_url='http://nrtcmems.mercator-ocean.fr/motu-web/Motu';
% service_id='GLOBAL_ANALYSIS_FORECAST_PHYS_001_015-TDS';
% product_id='global-analysis-forecast-phys-001-015';
%
% Check http://marine.copernicus.eu/web/34-products-and-services-faq.php 
% for changes in the command line: addresses, file names, variable names ...
%
% Currently needs Python2.7.X (with X >= 10) or Python 3.X (with >=4) 
% and motuclient.py v.1.8.X (where "X" is equal or higher to "4") 
% from https://marine.copernicus.eu/services-portfolio/technical-faq/#motu-client
%
% python
% ======
% Type:
%   python --version
% It should return: "Python 2.7.10+" or "Python 3.4+".
%
% motuclient
% ==========
% 1- Use croco's motuclient (pathMotu => Forecast_tools) or ...
% 2- Install your own motuclient:
%   To update/upgrade and get the latest version of motuclient 
%   from a previous version (<= v1.8.3), type in the following:
%      $ python -m pip install --upgrade motuclient
%   Otherwise (if there is no previous installation of motuclient), 
%   type in the following:
%      $ python -m pip install motuclient
%   It should install and display motuclient package v1.8.4 (Oct. 2019). 
%   To make sure, display the version:
%      $ python -m motuclient --version
%   If it does not return: "motuclient-python v1.8.X" ("X" >= "4"), 
%   then type in the following:
%      $ python -m pip install motuclient==1.8.4
%=======
%
eval(['! rm ',outname])

if isempty(pathmotu),  % use your own motuclient

    command = {'!python -m motuclient'
               sprintf(' -u %s -p %s',info{1},info{2}) 
               sprintf(' -m %s',motu_url)
               sprintf(' -s %s',service_id)
               sprintf(' -d %s',product_id)
               sprintf(' -t %s -T %s',date{1},date{2})
               sprintf(' -x %f -X %f',geom(1),geom(2))
               sprintf(' -y %f -Y %f',geom(3),geom(4))
               sprintf(' -z %f -Z %f',geom(5),geom(6))
               sprintf(' -o ./')
               sprintf(' --out-name %s',outname)};

else                  % use croco's motuclient

    command = {'!'
    	       sprintf('%s',pathmotu)
    	      'motuclient-python/motuclient.py'
    	       sprintf(' -u %s -p %s',info{1},info{2})
               sprintf(' -m %s',motu_url)
               sprintf(' -s %s',service_id)
               sprintf(' -d %s',product_id)
               sprintf(' -t %s -T %s',date{1},date{2})
               sprintf(' -x %f -X %f',geom(1),geom(2))
               sprintf(' -y %f -Y %f',geom(3),geom(4))
               sprintf(' -z %f -Z %f',geom(5),geom(6))
               sprintf(' -o ./')
               sprintf(' --out-name %s',outname)};
end
    
for k =1:length(vars)
    command{end+1}=sprintf(' -v %s ',vars{k});
end

%===
%  If using a proxy server, uncomment the following line and replace by your 
%  proxy url and port server. Beware that a SPACE is needed between 
%  "--proxy-server=" and "the proxy-server-name" !
%
%command{end+2}=sprintf('--proxy-server= http://your_proxy_server:your_proxy_port');
%===
disp([command{:}])
eval([command{:}])

%
% example of a python motuclient commands
%
%python -m motuclient --motu http://my.cmems-du.eu/motu-web/Motu --service-id GLOBAL_REANALYSIS_PHY_001_030-TDS --product-id global-reanalysis-phy-001-030-daily --longitude-min 15 --longitude-max 20 --latitude-min -40 --latitude-max -25 --date-min "2004-12-31 12:00:00" --date-max "2005-02-01 12:00:00" --depth-min 0.493 --depth-max 5727.918 --variable so --variable thetao --variable uo --variable vo --variable zos --out-dir <OUTPUT_DIRECTORY> --out-name <OUTPUT_FILENAME> --user <USERNAME> --pwd <PASSWORD>
end

