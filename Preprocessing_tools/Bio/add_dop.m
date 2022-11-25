function add_dop(oafile,climfile,inifile,gridfile,seas_datafile,...
                 ann_datafile,cycle,makeoa,makeclim);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  function [longrd,latgrd,dop]=add_dop(climfile,gridfile,...
%                                       seas_datafile,ann_datafile,...
%                                       cycle);
%
%  Add DOP (mMol C m-3) in a CROCO climatology file
%  take seasonal data for the upper levels and annual data for the
%  lower levels
%
%  input:
%    
%    climfile      : croco climatology file to process (netcdf)
%    gridfile      : croco grid file (netcdf)
%    seas_datafile : regular longitude - latitude - z seasonal data 
%                    file used for the upper levels  (netcdf)
%    ann_datafile  : regular longitude - latitude - z annual data 
%                    file used for the lower levels  (netcdf)
%    cycle         : time length (days) of climatology cycle (ex:360 for
%                    annual cycle) - 0 if no cycle.
%
%   output:
%
%    [longrd,latgrd,dop] : surface field to plot (as an illustration)
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
%  Copyright (c) 2001-2006 by Pierrick Penven
%  e-mail:Pierrick.Penven@ird.fr
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Read in the grid
%
nc=netcdf(gridfile,'r');
hmax=max(max(nc{'h'}(:)));
close(nc);
%
% read in the datafiles 
%
nc=netcdf(seas_datafile,'r');
t=nc{'T'}(:);
t;
close(nc)
nc=netcdf(ann_datafile,'r');
zdop=nc{'Z'}(:);
kmax=max(find(zdop<hmax))-1;
zdop=zdop(1:kmax);
%disp('Size zdop=')
size(zdop);
close(nc)
%
% open the OA file  
% 
if (makeoa)
  disp('Add_dop: creating variables and attributes for the OA file')
  nc=netcdf(oafile,'write');
%  redef(nc);

  %Create Dimensions
  nc('dop_time') = length(t);
  %Create Variable
  nc{'dop_time'} = ncdouble('dop_time') ;
%
%%
%
  %Create Dimensions
  nc('Zdop') = length(zdop);
  %Create Variable
  nc{'Zdop'} = ncdouble('Zdop') ;
%
%%
%
  %Create Variable
  nc{'DOP'} = ncdouble('dop_time','Zdop','eta_rho','xi_rho') ;
%
%%
%
  %Create Attribute

  nc{'dop_time'}.long_name = ncchar('time for dop');
  nc{'dop_time'}.long_name = 'time for dop';
  nc{'dop_time'}.units = ncchar('day');
  nc{'dop_time'}.units = 'day';
  if cycle~=0
    nc{'dop_time'}.cycle_length = cycle;
  end
%
  nc{'Zdop'}.long_name = ncchar('Depth for DOP');
  nc{'Zdop'}.long_name = 'Depth for DOP';
  nc{'Zdop'}.units = ncchar('m');
  nc{'Zdop'}.units = 'm';
%
  nc{'DOP'}.long_name = ncchar('DOP');
  nc{'DOP'}.long_name = 'DOP';
  nc{'DOP'}.units = ncchar('mMol C m-3');
  nc{'DOP'}.units = 'mMol C m-3';
  nc{'DOP'}.fields = ncchar('DOP, scalar, series');
  nc{'DOP'}.fields = 'DOP, scalar, series';
%
%%  endef(nc);
%
%% Write variables
% record deth and time and close
%
  nc{'dop_time'}(:)=t*30; % if time in month in the dataset !!!
  nc{'Zdop'}(:)=zdop;
  close(nc)
end
%
% Same thing for the Clim file
%
if (makeclim)
  disp('Add_dop: creating variables and attributes for the Climatology file')
%
% open the clim file  
% 
  nc=netcdf(climfile,'write');
%%  redef(nc);
  nc('dop_time') = length(t);;
  nc{'dop_time'} = ncdouble('dop_time') ;
  nc{'DOP'} = ncdouble('dop_time','s_rho','eta_rho','xi_rho') ;
%
  nc{'dop_time'}.long_name = ncchar('time for dop');
  nc{'dop_time'}.long_name = 'time for dop';
  nc{'dop_time'}.units = ncchar('day');
  nc{'dop_time'}.units = 'day';
  if cycle~=0
    nc{'dop_time'}.cycle_length = cycle;
  end
%
  nc{'DOP'}.long_name = ncchar('DOP');
  nc{'DOP'}.long_name = 'DOP';
  nc{'DOP'}.units = ncchar('mMol C m-3');
  nc{'DOP'}.units = 'mMol C m-3';
  nc{'DOP'}.fields = ncchar('DOP, scalar, series');
  nc{'DOP'}.fields = 'DOP, scalar, series';
%
%%  endef(nc);
%
% record the time and close
%
  nc{'dop_time'}(:)=t*30; % if time in month in the dataset !!!
  close(nc)
end
%
% Same thing for the Initial file
%
%disp('Add_dop: creating variables and attributes for the Initial file')
%
% open the clim file  
% 
%nc=netcdf(inifile,'write');
%redef(nc);
%nc{'DOP'} = ncdouble('time','s_rho','eta_rho','xi_rho') ;
%
%nc{'DOP'}.long_name = ncchar('Nitrate');
%nc{'DOP'}.long_name = 'Nitrate';
%nc{'DOP'}.units = ncchar('mMol N m-3');
%nc{'DOP'}.units = 'mMol N m-3';
%
%endef(nc);
%close(nc)

return
