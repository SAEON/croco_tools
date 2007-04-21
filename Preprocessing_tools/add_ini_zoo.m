function add_ini_zoo(inifile);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  function add_ini_zoo(inifile);
%
%  pierrick 2001 update patrick 2005
%
%  Add zooplancton (mMol N m-3) in a ROMS climatology file.
%  take the chlorophyll (mg C) from the initial file and
%  multiply by the ratio chlorophyll / phytoplancton derived
%  from previous simulations (Gruber et al., 2005)
%
%  zoo = 0.2 * chla

%  input:
%    
%    inifile      : roms initial file to process (netcdf)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
theta=0.2;
%
disp('Add_ini_zoo: creating variable and attribute')
%
% open the initial file  
% 
nc=netcdf(inifile,'write');
time= nc{'scrum_time'}(:);
tlen=length(time);
redef(nc);
nc{'ZOO'} = ncdouble('time','s_rho','eta_rho','xi_rho') ;
nc{'ZOO'}.long_name = ncchar('Zooplankton');
nc{'ZOO'}.long_name = 'Zooplankton';
nc{'ZOO'}.units = ncchar('mMol N m-3');
nc{'ZOO'}.units = 'mMol N m-3';
nc{'ZOO'}.fields = ncchar('ZOO, scalar, series');
nc{'ZOO'}.fields = 'ZOO, scalar, series';
endef(nc);
%
% loop on time
%
for l=1:tlen
  disp(['time index: ',num2str(l),' of total: ',num2str(tlen)])
  nc{'ZOO'}(l,:,:,:)=theta*squeeze(nc{'CHLA'}(l,:,:,:));
end
close(nc);
return
