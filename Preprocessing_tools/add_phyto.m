function add_phyto(climfile);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  function add_phyto(climfile);
%
%  pierrick 2001
%
%  Add phytoplancton (mMol N m-3)  in a ROMS climatology file.
%  take the chlorophyll (mg C) from the climatology file and
%  multiply by the ratio chlorophyll / phytoplancton derived
%  from previous simulations.
%
%  phyto = 0.5 * chla
%
%  input:
%    
%    climfile      : roms climatology file to process (netcdf)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
theta=0.5;
%
disp('Add_phyto: creating variable and attribute')
%
% open the clim file  
% 
nc=netcdf(climfile,'write');
time= nc{'chla_time'}(:);
cycle= nc{'chla_time'}.cycle_length(:);
tlen=length(time);
redef(nc);
nc('phyto_time') = tlen;
nc{'phyto_time'} = ncdouble('phyto_time') ;
nc{'PHYTO'} = ncdouble('phyto_time','s_rho','eta_rho','xi_rho') ;
%
nc{'phyto_time'}.long_name = ncchar('time for phytoplankton');
nc{'phyto_time'}.long_name = 'time for phytoplankton';
nc{'phyto_time'}.units = ncchar('day');
nc{'phyto_time'}.units = 'day';
if cycle~=0
  nc{'phyto_time'}.cycle_length = cycle;
end
%
nc{'PHYTO'}.long_name = ncchar('Phytoplankton');
nc{'PHYTO'}.long_name = 'Phytoplankton';
nc{'PHYTO'}.units = ncchar('mMol N m-3');
nc{'PHYTO'}.units = 'mMol N m-3';
nc{'PHYTO'}.fields = ncchar('PHYTO, scalar, series');
nc{'PHYTO'}.fields = 'PHYTO, scalar, series';
%
endef(nc);
%
% record the time
%
nc{'phyto_time'}(:)=time;
%
% loop on time
%
for l=1:tlen
  disp(['time index: ',num2str(l),' of total: ',num2str(tlen)])
  nc{'PHYTO'}(l,:,:,:)=theta*squeeze(nc{'CHLA'}(l,:,:,:));
end
close(nc);
return
