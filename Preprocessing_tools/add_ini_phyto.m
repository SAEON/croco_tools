function add_ini_phyto(inifile);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  function add_ini_phyto(inifile);
%
%  pierrick 2001
%
%  Add phytoplancton (mMol N m-3)  in a ROMS initial file.
%  take the chlorophyll (mg C) from the initial file and
%  multiply by the ratio chlorophyll / phytoplancton derived
%  from previous simulations.
%
%  phyto = 0.5 * chla
%
%  input:
%    
%    inifile      : roms initial file to process (netcdf)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
theta=0.5;
%
disp('Add_ini_phyto: creating variable and attribute')
%
% open the initial file  
% 
nc=netcdf(inifile,'write');
time= nc{'scrum_time'}(:);
tlen=length(time);
redef(nc);
nc{'PHYTO'} = ncdouble('time','s_rho','eta_rho','xi_rho') ;
nc{'PHYTO'}.long_name = ncchar('Phytoplankton');
nc{'PHYTO'}.long_name = 'Phytoplankton';
nc{'PHYTO'}.units = ncchar('mMol N m-3');
nc{'PHYTO'}.units = 'mMol N m-3';
nc{'PHYTO'}.fields = ncchar('PHYTO, scalar, series');
nc{'PHYTO'}.fields = 'PHYTO, scalar, series';
endef(nc);
%
% loop on time
%
for l=1:tlen
  disp(['time index: ',num2str(l),' of total: ',num2str(tlen)])
  nc{'PHYTO'}(l,:,:,:)=theta*squeeze(nc{'CHLA'}(l,:,:,:));
end
close(nc);
return
