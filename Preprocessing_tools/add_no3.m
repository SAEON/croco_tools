function add_no3(oafile,climfile,inifile,gridfile,seas_datafile,...
                 ann_datafile,cycle,makeoa,makeclim);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  function [longrd,latgrd,no3]=add_no3(climfile,gridfile,...
%                                       seas_datafile,ann_datafile,...
%                                       cycle);
%
%  pierrick 2001
%
%  Add nitrate (mMol N m-3) in a ROMS climatology file
%  take seasonal data for the upper levels and annual data for the
%  lower levels
%
%  input:
%    
%    climfile      : roms climatology file to process (netcdf)
%    gridfile      : roms grid file (netcdf)
%    seas_datafile : regular longitude - latitude - z seasonal data 
%                    file used for the upper levels  (netcdf)
%    ann_datafile  : regular longitude - latitude - z annual data 
%                    file used for the lower levels  (netcdf)
%    cycle         : time length (days) of climatology cycle (ex:360 for
%                    annual cycle) - 0 if no cycle.
%
%   output:
%
%    [longrd,latgrd,no3] : surface field to plot (as an illustration)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Read in the grid
%
nc=netcdf(gridfile);
hmax=max(max(nc{'h'}(:)));
close(nc);
%
% read in the datafiles 
%
nc=netcdf(seas_datafile);
t=nc{'T'}(:);
close(nc)
nc=netcdf(ann_datafile);
zno3=nc{'Z'}(:)
kmax=max(find(zno3<hmax))-1;
zno3=zno3(1:kmax);
close(nc)
%
% open the OA file  
% 
if (makeoa)
  disp('Add_no3: creating variables and attributes for the OA file')
  nc=netcdf(oafile,'write');
  redef(nc);
  nc('no3_time') = length(t);
  nc{'no3_time'} = ncdouble('no3_time'); 
  nc('Zno3') = length(zno3);
  nc{'Zno3'} = ncdouble('Zno3'); 
  nc{'NO3'} = ncdouble('no3_time','Zno3','eta_rho','xi_rho') ;
%
  nc{'no3_time'}.long_name = ncchar('time for nitrate');
  nc{'no3_time'}.long_name = 'time for nitrate';
  nc{'no3_time'}.units = ncchar('day');
  nc{'no3_time'}.units = 'day';
  if cycle~=0
    nc{'no3_time'}.cycle_length = cycle;
  end
%
  nc{'Zno3'}.long_name = ncchar('Depth for NO3');
  nc{'Zno3'}.long_name = 'Depth for NO3';
  nc{'Zno3'}.units = ncchar('m');
  nc{'Zno3'}.units = 'm';
%
  nc{'NO3'}.long_name = ncchar('Nitrate');
  nc{'NO3'}.long_name = 'Nitrate';
  nc{'NO3'}.units = ncchar('mMol N m-3');
  nc{'NO3'}.units = 'mMol N m-3';
  nc{'NO3'}.fields = ncchar('NO3, scalar, series');
  nc{'NO3'}.fields = 'NO3, scalar, series';
%
  endef(nc);
%
% record deth and time and close
%
  nc{'Zno3'}(:)=zno3;
  nc{'no3_time'}(:)=t*30; % if time in month in the dataset !!!
  close(nc)
end
%
% Same thing for the Clim file
%
if (makeclim)
  disp('Add_no3: creating variables and attributes for the Climatology file')
%
% open the clim file  
% 
  nc=netcdf(climfile,'write');
  redef(nc);
  nc('no3_time') = length(t);;
  nc{'no3_time'} = ncdouble('no3_time') ;
  nc{'NO3'} = ncdouble('no3_time','s_rho','eta_rho','xi_rho') ;
%
  nc{'no3_time'}.long_name = ncchar('time for nitrate');
  nc{'no3_time'}.long_name = 'time for nitrate';
  nc{'no3_time'}.units = ncchar('day');
  nc{'no3_time'}.units = 'day';
  if cycle~=0
    nc{'no3_time'}.cycle_length = cycle;
  end
%
  nc{'NO3'}.long_name = ncchar('Nitrate');
  nc{'NO3'}.long_name = 'Nitrate';
  nc{'NO3'}.units = ncchar('mMol N m-3');
  nc{'NO3'}.units = 'mMol N m-3';
  nc{'NO3'}.fields = ncchar('NO3, scalar, series');
  nc{'NO3'}.fields = 'NO3, scalar, series';
%
  endef(nc);
%
% record the time and close
%
  nc{'no3_time'}(:)=t*30; % if time in month in the dataset !!!
  close(nc)
end
%
% Same thing for the Initial file
%
%disp('Add_no3: creating variables and attributes for the Initial file')
%
% open the clim file  
% 
%nc=netcdf(inifile,'write');
%redef(nc);
%nc{'NO3'} = ncdouble('time','s_rho','eta_rho','xi_rho') ;
%
%nc{'NO3'}.long_name = ncchar('Nitrate');
%nc{'NO3'}.long_name = 'Nitrate';
%nc{'NO3'}.units = ncchar('mMol N m-3');
%nc{'NO3'}.units = 'mMol N m-3';
%
%endef(nc);
%close(nc)

return
