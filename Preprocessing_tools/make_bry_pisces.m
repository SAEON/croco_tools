%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Build a ROMS boundary file
%
%  Extrapole and interpole temperature and salinity from a
%  climatology to get boundary conditions for
%  ROMS (boundary netcdf file) .
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
%  Data source : IRI/LDEO climate Data Library (World Ocean Atlas 1998)
%    http://ingrid.ldgo.columbia.edu/
%    http://iridl.ldeo.columbia.edu/SOURCES/.NOAA/.NODC/.WOA98/
%
%  Pierrick Penven, IRD, 2005.                                    %
%  Olivier Aumont the master, IRD, 2006.                          %
%  Patricio Marchesiello, chief, IRD, 2007.                       %
%  Christophe Eugene Raoul Menkes, the slave, IRD, 2007.          %
%
%  WARNING !!!!!! THIS ASSUMES THAT THE TIME FOR PISCES INITIAL.
%  IS THE SAME AS THE CLIM T AND S. ELSE, CHANGE THE PROGRAM
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all
close all
%%%%%%%%%%%%%%%%%%%%% USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%%
%
% Common parameters
%
romstools_param
%
% Set times and cycles: monthly climatology for all data
%
time=woa_time;             % time 
cycle=woa_cycle;           % cycle 
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
% Read in the grid
%
disp(' ')
disp(' Read in the grid...')
nc=netcdf(grdname);
lon=nc{'lon_rho'}(:);
lat=nc{'lat_rho'}(:);
Lp=length(nc('xi_rho'));
Mp=length(nc('eta_rho'));
hmax=max(max(nc{'h'}(:)));
result=close(nc);
%
% Redefine the boundary file
%
if (makebry)
  disp(' ')
  disp(' Redefine the boundary file...')
  add_bry_pisces(bryname,obc,time,cycle,'write');
end
%
% Redefine the boundary file in Z-coordinates
%
if (makeZbry)
  disp(' ')
  disp(' Redefine the boundary Z-file...')
%
% get Z
%
  nc=netcdf(no3_ann_data);
  Z=nc{'Z'}(:);
  kmax=max(find(Z<hmax))-1;
  Z=Z(1:kmax);
  close(nc)
  add_bry_pisces_Z(Zbryname,obc,Z,time,cycle,'write');
  disp(' ')
  disp(' Horizontal extrapolations')
%
% Loop on the lateral boundaries 
%
  for obcndx=1:4
    if obc(obcndx)==1
      if obcndx==1
        disp(' Processing southern boundary...')
	suffix='_south';
      elseif obcndx==2
        disp(' Processing eastern boundary...')
	suffix='_east';
      elseif obcndx==3
        disp(' Processing northern boundary...')
	suffix='_north';
      elseif obcndx==4
        disp(' Processing western boundary...')
	suffix='_west';
      end
  
      disp('  Nitrate...')
      bry_interp_pisces(Zbryname,lon,lat,no3_month_data,no3_ann_data,...
               'nitrate',['NO3',suffix],obcndx);        
      disp('  Phosphate...')
      bry_interp_pisces(Zbryname,lon,lat,po4_month_data,po4_ann_data,...
               'phosphate',['PO4',suffix],obcndx);        
      disp('  Silica...')
      bry_interp_pisces(Zbryname,lon,lat,sio3_month_data,sio3_ann_data,...
               'silicate',['Si',suffix],obcndx);        
      disp('  Oxygen for life...')
      bry_interp_pisces(Zbryname,lon,lat,o2_month_data,o2_ann_data,...
               'oxygen',['O2',suffix],obcndx);        
      disp('  Dissolved Inorganic Carbon...')
      bry_interp_pisces(Zbryname,lon,lat,dic_month_data,dic_ann_data,...
               'dic',['DIC',suffix],obcndx);        
      disp('  Total Alkalinity...')
      bry_interp_pisces(Zbryname,lon,lat,talk_month_data,talk_ann_data,...
               'talk',['TALK',suffix],obcndx);        
      disp('  Dissolved Organic Carbon...')
      bry_interp_pisces(Zbryname,lon,lat,doc_month_data,doc_ann_data,...
               'doc',['DOC',suffix],obcndx);        
      disp('  Iron...')
      bry_interp_pisces(Zbryname,lon,lat,fer_month_data,fer_ann_data,...
               'fer',['FER',suffix],obcndx);        
    end
  end
end
%
% Vertical interpolations 
%
if (makebry)
  disp(' ')
  disp(' Vertical interpolations')
%
% Loop on the lateral boundaries 
%
  for obcndx=1:4
    if obc(obcndx)==1
      if obcndx==1
        disp(' Processing southern boundary...')
	suffix='_south';
      elseif obcndx==2
        disp(' Processing eastern boundary...')
	suffix='_east';
      elseif obcndx==3
        disp(' Processing northern boundary...')
	suffix='_north';
      elseif obcndx==4
        disp(' Processing western boundary...')
	suffix='_west';
      end
      disp(' ')
      disp('  Nitrate...')
      vinterp_bry(bryname,grdname,Zbryname,['NO3',suffix],obcndx);
      disp(' ')
      disp('  Phosphate...')
      vinterp_bry(bryname,grdname,Zbryname,['PO4',suffix],obcndx);
      disp(' ')
      disp('  Silica...')
      vinterp_bry(bryname,grdname,Zbryname,['Si',suffix],obcndx);
      disp(' ')
      disp('  Oxygen for life...')
      vinterp_bry(bryname,grdname,Zbryname,['O2',suffix],obcndx);
      disp(' ')
      disp('  Dissolved Inorganic Carbon...')
      vinterp_bry(bryname,grdname,Zbryname,['DIC',suffix],obcndx);
      disp(' ')
      disp('  Total Alkalinity...')
      vinterp_bry(bryname,grdname,Zbryname,['TALK',suffix],obcndx);
      disp(' ')
      disp('  Dissolved Organic Carbon...')
      vinterp_bry(bryname,grdname,Zbryname,['DOC',suffix],obcndx);
      disp(' ')
      disp('  Iron...')
      vinterp_bry(bryname,grdname,Zbryname,['FER',suffix],obcndx);
    end
  end
end
%
% Make a few plots
%
disp(' ')
disp(' Make a few plots...')
test_bry(bryname,grdname,'NO3',1,obc)
figure
test_bry(bryname,grdname,'PO4',1,obc)
figure
test_bry(bryname,grdname,'Si',1,obc)
figure
test_bry(bryname,grdname,'O2',1,obc)
figure
test_bry(bryname,grdname,'DIC',6,obc)
figure
test_bry(bryname,grdname,'TALK',6,obc)
figure
test_bry(bryname,grdname,'DOC',6,obc)
figure
test_bry(bryname,grdname,'FER',6,obc)
%
% End
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
