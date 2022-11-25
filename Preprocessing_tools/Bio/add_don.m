function add_don(oafile,climfile,inifile,gridfile,seas_datafile,...
                 ann_datafile,cycle,makeoa,makeclim);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  function [longrd,latgrd,doc]=add_doc(climfile,gridfile,...
%                                       seas_datafile,ann_datafile,...
%                                       cycle);
%
%  Add DOC (mMol C m-3) in a CROCO climatology file
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
%    [longrd,latgrd,doc] : surface field to plot (as an illustration)
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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
zdon=nc{'Z'}(:);
kmax=max(find(zdon<hmax))-1;
zdon=zdon(1:kmax);
%disp('Size zdon=')
size(zdon);
close(nc)
%
% open the OA file  
% 
if (makeoa)
  disp('Add_don: creating variables and attributes for the OA file')
  nc=netcdf(oafile,'write');
 % redef(nc);

  %Create Dimensions
  nc('don_time') = length(t);
  %Create Variable
  nc{'don_time'} = ncdouble('don_time') ;
%
%%
%
  %Create Dimensions
  nc('Zdon') = length(zdon);
  %Create Variable
  nc{'Zdon'} = ncdouble('Zdon') ;
%
%%
%
  %Create Variable
  nc{'DON'} = ncdouble('don_time','Zdon','eta_rho','xi_rho') ;
%
%%
%
  %Create Attribute

  nc{'don_time'}.long_name = ncchar('time for don');
  nc{'don_time'}.long_name = 'time for don';
  nc{'don_time'}.units = ncchar('day');
  nc{'don_time'}.units = 'day';
  if cycle~=0
    nc{'don_time'}.cycle_length = cycle;
  end
%%
  nc{'Zdon'}.long_name = ncchar('Depth for DON');
  nc{'Zdon'}.long_name = 'Depth for DON';
  nc{'Zdon'}.units = ncchar('m');
  nc{'Zdon'}.units = 'm';
%%%
  nc{'DON'}.long_name = ncchar('DON');
  nc{'DON'}.long_name = 'DON';
  nc{'DON'}.units = ncchar('mMol C m-3');
  nc{'DON'}.units = 'mMol C m-3';
  nc{'DON'}.fields = ncchar('DON, scalar, series');
  nc{'DON'}.fields = 'DON, scalar, series';
%%%
%%  endef(nc);
%
%% Write variables
%% record deth and time and close
%
  nc{'don_time'}(:)=t*30; % if time in month in the dataset !!!
  nc{'Zdon'}(:)=zdon;
  close(nc)
end
%
% Same thing for the Clim file
%
if (makeclim)
  disp('Add_don: creating variables and attributes for the Climatology file')
%
% open the clim file  
% 
  nc=netcdf(climfile,'write');
%  redef(nc);
  nc('don_time') = length(t);;
  nc{'don_time'} = ncdouble('don_time') ;
  nc{'DON'} = ncdouble('don_time','s_rho','eta_rho','xi_rho') ;
%
  nc{'don_time'}.long_name = ncchar('time for don');
  nc{'don_time'}.long_name = 'time for don';
  nc{'don_time'}.units = ncchar('day');
  nc{'don_time'}.units = 'day';
  if cycle~=0
    nc{'don_time'}.cycle_length = cycle;
  end
%
  nc{'DON'}.long_name = ncchar('DON');
  nc{'DON'}.long_name = 'DON';
  nc{'DON'}.units = ncchar('mMol C m-3');
  nc{'DON'}.units = 'mMol C m-3';
  nc{'DON'}.fields = ncchar('DON, scalar, series');
  nc{'DON'}.fields = 'DON, scalar, series';
%
%%  endef(nc);
%
% record the time and close
%
  nc{'don_time'}(:)=t*30; % if time in month in the dataset !!!
  close(nc)
end
%
% Same thing for the Initial file
%
%disp('Add_doc: creating variables and attributes for the Initial file')
%
% open the clim file  
% 
%nc=netcdf(inifile,'write');
%redef(nc);
%nc{'DOC'} = ncdouble('time','s_rho','eta_rho','xi_rho') ;
%
%nc{'DOC'}.long_name = ncchar('Nitrate');
%nc{'DOC'}.long_name = 'Nitrate';
%nc{'DOC'}.units = ncchar('mMol N m-3');
%nc{'DOC'}.units = 'mMol N m-3';
%
%endef(nc);
%close(nc)

return
