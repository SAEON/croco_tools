function add_zoo(climfile);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  function add_zoo(climfile);
%
%  pierrick 2001  update patrick 2005
%
%  Add zooplancton (mMol N m-3) in a ROMS climatology file.
%  take the chlorophyll (mg C) from the climatology file and
%  multiply by the ratio chlorophyll / phytoplancton derived
%  from previous simulations (Gruber et al., 2005)
%
%  zoo = 0.2 * chla
%
%  input:
%    
%    climfile      : roms climatology file to process (netcdf)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
theta=0.2;
%
disp('Add_zoo: creating variable and attribute')
%
% open the clim file  
% 
nc=netcdf(climfile,'write');
time= nc{'chla_time'}(:);
cycle= nc{'chla_time'}.cycle_length(:);
tlen=length(time);
redef(nc);
nc('zoo_time') = tlen;
nc{'zoo_time'} = ncdouble('zoo_time') ;
nc{'ZOO'} = ncdouble('zoo_time','s_rho','eta_rho','xi_rho') ;
%
nc{'zoo_time'}.long_name = ncchar('time for zooplankton');
nc{'zoo_time'}.long_name = 'time for zooplankton';
nc{'zoo_time'}.units = ncchar('day');
nc{'zoo_time'}.units = 'day';
if cycle~=0
  nc{'zoo_time'}.cycle_length = cycle;
end
%
nc{'ZOO'}.long_name = ncchar('Zooplankton');
nc{'ZOO'}.long_name = 'Zooplankton';
nc{'ZOO'}.units = ncchar('mMol N m-3');
nc{'ZOO'}.units = 'mMol N m-3';
nc{'ZOO'}.fields = ncchar('ZOO, scalar, series');
nc{'ZOO'}.fields = 'ZOO, scalar, series';
%
endef(nc);
%
% record the time
%
nc{'zoo_time'}(:)=time;
%
% loop on time
%
for l=1:tlen
  disp(['time index: ',num2str(l),' of total: ',num2str(tlen)])
  nc{'ZOO'}(l,:,:,:)=theta*squeeze(nc{'CHLA'}(l,:,:,:));
end
close(nc);
return
