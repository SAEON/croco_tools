%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Build a ROMS initial file from Levitus Data
%
%  Extrapole and interpole temperature and salinity from a
%  Climatology to get initial conditions for
%  ROMS (initial netcdf files) .
%  Get the velocities and sea surface elevation via a 
%  geostrophic computation.
%
%  Data input format (netcdf):
%     temperature(T, Z, Y, X)
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
%  Christophe Eugene Raoul Menkes, IRD 2007
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
no3_month_data  = '../WOAPISCES/no3_seas.cdf';
no3_ann_data    = '../WOAPISCES/no3_ann.cdf';
po4_month_data  = '../WOAPISCES/po4_seas.cdf';
po4_ann_data    = '../WOAPISCES/po4_ann.cdf';
o2_month_data   = '../WOAPISCES/o2_seas.cdf';
o2_ann_data     = '../WOAPISCES/o2_ann.cdf';
sio3_month_data = '../WOAPISCES/sio3_seas.cdf';
sio3_ann_data   = '../WOAPISCES/sio3_ann.cdf';
dic_month_data  = '../WOAPISCES/dic_seas.cdf';
dic_ann_data    = '../WOAPISCES/dic_ann.cdf';
talk_month_data = '../WOAPISCES/talk_seas.cdf';
talk_ann_data   = '../WOAPISCES/talk_ann.cdf';
doc_month_data  = '../WOAPISCES/doc_seas.cdf';
doc_ann_data    = '../WOAPISCES/doc_ann.cdf';
fer_month_data  = '../WOAPISCES/fer_seas.cdf';
fer_ann_data    = '../WOAPISCES/fer_ann.cdf';
dust_month_data = '../WOAPISCES/dust_seas.cdf';
dust_ann_data   = '../WOAPISCES/dust_ann.cdf';
%
%
%%%%%%%%%%%%%%%%%%% END USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%
%
% Title
%
disp(' ')
disp([' Adding PISCES data into initial file: ',ininame])
%
% Initial file
%
add_ini_pisces(ininame,'write');
%
% Horizontal and vertical interp/extrapolations 
%
disp(' ')
disp(' Interpolations / extrapolations')
disp(' ')
disp('nitrate ...')
ext_tracers_ini(ininame,grdname,no3_month_data,no3_ann_data,...
             'nitrate','NO3','r',tini);
disp(' ')
disp('phosphate ...')
ext_tracers_ini(ininame,grdname,po4_month_data,po4_ann_data,...
             'phosphate','PO4','r',tini);
disp(' ')
disp('Silicate ...')
ext_tracers_ini(ininame,grdname,sio3_month_data,sio3_ann_data,...
             'silicate','Si','r',tini);
disp(' ')
disp(' Oxygen ...')
ext_tracers_ini(ininame,grdname,o2_month_data,o2_ann_data,...
             'oxygen','O2','r',tini);
disp(' ')
disp('Dissolved Inorganic Carbon ...')
ext_tracers_ini(ininame,grdname,dic_month_data,dic_ann_data,...
             'dic','DIC','r',tini);
disp(' ')
disp('Total alkalinity ...')
ext_tracers_ini(ininame,grdname,talk_month_data,talk_ann_data,...
             'talk','TALK','r',tini);
disp(' ')
disp('Dissolved Organic Carbon...')
ext_tracers_ini(ininame,grdname,doc_month_data,doc_ann_data,...
             'doc','DOC','r',tini);
disp(' ')
disp('Iron ...')
ext_tracers_ini(ininame,grdname,fer_month_data,fer_ann_data,...
             'fer','FER','r',tini);
%
% Make a few plots
%
disp(' ')
disp(' Make a few plots...')
test_clim(clmname,grdname,'NO3',1,coastfileplot)
figure
test_clim(clmname,grdname,'PO4',1,coastfileplot)
figure
test_clim(clmname,grdname,'Si',1,coastfileplot)
figure
test_clim(clmname,grdname,'O2',1,coastfileplot)
figure
test_clim(clmname,grdname,'DIC',1,coastfileplot)
figure
test_clim(clmname,grdname,'TALK',1,coastfileplot)
figure
test_clim(clmname,grdname,'DOC',1,coastfileplot)
figure
test_clim(clmname,grdname,'FER',1,coastfileplot)
%
% End
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
