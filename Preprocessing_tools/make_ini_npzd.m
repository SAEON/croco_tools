%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Build a ROMS initial file from Global Atlas (WOA or CARS)
%
%  Extrapole and interpole fields from a
%  Climatology to get initial conditions for
%  ROMS (initial netcdf files) .
%
%  Data input format (netcdf):
%     variable(T, Z, Y, X)
%     T : time [Months]
%     Z : Depth [m]
%     Y : Latitude [degree north]
%     X : Longitude [degree east]
%
%  Data source : IRI/LDEO Climate Data Library (World Ocean Atlas 1998)
%    http://ingrid.ldgo.columbia.edu/
%    http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NODC/.WOA98/
%
%  Pierrick Penven, IRD, 2005.
%  Olivier Aumont, IRD, 2006.
%  Patricio Marchesiello, IRD 2007
%  Christophe Eugene Raoul Menkes, IRD 2013
%  Elodie Gutknecht, 2013
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all
close all
%%%%%%%%%%%%%%%%%%%%% USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%%
%
%  Title 
%
romstools_param
%
%  Data climatologies file names:
%
no3_seas_data=[climato_dir,'no3_seas.cdf'];
no3_ann_data=[climato_dir,'no3_ann.cdf'];
o2_seas_data=[climato_dir,'o2_seas.cdf'];
o2_ann_data=[climato_dir,'o2_ann.cdf'];
chla_seas_data=[chla_dir,'chla_seas.cdf'];

NO3min=0.01;
O2min=0.01;
%
%
%%%%%%%%%%%%%%%%%%% END USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%


%
% Add variables no3 and o2 in the oa and clm files 
%
add_no3(oaname,clmname,ininame,grdname,no3_seas_data,...
        no3_ann_data,woa_cycle,makeoa,makeclim)
add_o2(oaname,clmname,ininame,grdname,o2_seas_data,...
        o2_ann_data,woa_cycle,makeoa,makeclim)	
%
% Horizontal extrapolation
%
if (makeoa)

  ext_tracers(oaname,no3_seas_data,no3_ann_data,...
              'nitrate','NO3','no3_time','Zno3',Roa);
  ext_tracers(oaname,o2_seas_data,o2_ann_data,...
              'oxygen','O2','o2_time','Zo2',Roa);

end

%-----------------------------------------------------------------
% Initial file
%-----------------------------------------------------------------

if (makeini)
  disp(' Add initial conditions ')
%
%  O2		 
  disp(' ')
  disp(' O2...')
  add_ini_o2(ininame,grdname,oaname,woa_cycle,O2min);
%
%  NO3		 
  disp(' ')
  disp(' NO3...')
  add_ini_no3(ininame,grdname,oaname,woa_cycle,NO3min);
%
%  CHla		 
  disp(' ')
  disp(' CHla...')
  add_ini_chla(ininame,grdname,chla_seas_data,woa_cycle,Roa);
%
%  Phyto		 
  disp(' ')
  disp(' Phyto...')
  add_ini_phyto(ininame);
%
%  Zoo
  disp(' ')
  disp(' Zoo...')
  add_ini_zoo(ininame);

end



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Make a few plots
%

if (makeplot)
  disp(' ')
  disp(' Make a few plots...')
  test_clim(ininame,grdname,'O2',1,coastfileplot)
  figure
  test_clim(ininame,grdname,'NO3',1,coastfileplot)
  figure
  test_clim(ininame,grdname,'CHLA',1,coastfileplot)
  figure
  test_clim(ininame,grdname,'PHYTO',1,coastfileplot)
end 

%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



