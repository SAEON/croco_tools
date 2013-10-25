%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Add N2O data in input ROMS files from Global Atlas (WOA or CARS)
%
%  N2O distributuion from Nevison et al. (2003) formulation
%
%  Data input format (netcdf):
%     variable(T, Z, Y, X)
%     T : time [Months]
%     Z : Depth [m]
%     Y : Latitude [degree north]
%     X : Longitude [degree east]
%
%
%  Elodie Gutknecht, 2013
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all
close all
%%%%%%%%%%%%%%%%%%%%% USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%%
%
%  Title 
%
romstools_param
%
%
%%%%%%%%%%%%%%%%%%% END USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%


nc=netcdf(grdname);
h = nc{'h'}(:);
close(nc)

%------------------------------------------------------------------
% O2 initial conditions
nc=netcdf(ininame);
O2_ini   = nc{'O2'}(:);
close(nc)

%------------------------------------------------------------------
% O2 climatological conditions
nc=netcdf(clmname);
theta_s = nc{'theta_s'}(:);
theta_b = nc{'theta_b'}(:);
Tcline = nc{'Tcline'}(:);
N = length(nc('s_rho'));
O2_clm   = nc{'O2'}(:);
O2_time   = nc{'o2_time'}(:);
O2_cycle   = nc{'o2_time'}.cycle_length(:);
zeta = nc{'zeta'}(:);
close(nc)

zw_ini=zlevs(h,0.,theta_s,theta_b,Tcline,N,'w',vtransform); 

for itps=1:length(O2_time)
    zw_clm(itps,:,:,:)=zlevs(h,squeeze(zeta(itps,:,:)),theta_s,theta_b,Tcline,N,'w',vtransform); 
end 

%
% N2O based on Nevison et al (2003)
% 

N2O_ini(1:size(O2_ini,1),1:size(O2_ini,2),1:size(O2_ini,3))=NaN;

for i=1:size(O2_ini,1)
  for j=1:size(O2_ini,2)
    for k=1:size(O2_ini,3)
  
      if (zw_ini(i,j,k) <= (-50)) 
	cff=(16./170.)*((0.26/O2_ini(i,j,k))-0.0004)*exp(zw_ini(i,j,k)/3000.);    %mmolN2O/mmolO2
	N2O_ini(i,j,k)=O2_ini(i,j,k)*cff;
      else
        N2O_ini(i,j,k)=0.0076;                                     	
      end

    end
  end
end

%f Find NaN
find(isnan(N2O_ini)==1);
N2O_clm(1:size(O2_clm,1),1:size(O2_clm,2),1:size(O2_clm,3),1:size(O2_clm,4))=NaN;

for i=1:size(O2_clm,1)
  for j=1:size(O2_clm,2)
    for k=1:size(O2_clm,3)
      for l=1:size(O2_clm,4)

	if (zw_clm(i,j,k,l) <= (-50)) 
	  cff=(16./170.)*((0.26/O2_clm(i,j,k,l))-0.0004)*exp(zw_clm(i,j,k,l)/3000.);    %mmolN2O/mmolO2
   	  N2O_clm(i,j,k,l)=O2_clm(i,j,k,l)*cff;
        else
          N2O_clm(i,j,k,l)=0.0076;                                     	
        end

      end
    end
  end
end

% Find NaN'
find(isnan(N2O_clm)==1);
T= O2_time ;% time in days
cycle=O2_cycle;

if makeini
%
% add N20 in initial file
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

type = 'initial conditions file' ; 
history = 'ROMS' ;

%
% open the ini file  
% 
nc = netcdf(ininame,'write');
%
% new variable
%
redef(nc);
nc('n2o_time') = 1;
nc{'n2o_time'} = ncdouble('time') ;

nc{'N2O'} = ncdouble('time','s_rho','eta_rho','xi_rho') ;
nc{'N2O'}.long_name = ncchar('Nitrous oxide');
nc{'N2O'}.long_name = 'Nitrous oxide';
nc{'N2O'}.units = ncchar('mMol N2O m-3'); 
nc{'N2O'}.units = 'mMol N2O m-3';
nc{'N2O'}.fields = ncchar('NO2, scalar, series');
nc{'N2O'}.fields = 'NO2, scalar, series';
  
endef(nc);

%
% write new variable
%
nc{'N2O'}(:,:,:) =  N2O_ini; 

%
% Synchronize on disk
%
close(nc);
end
 

if makeclim
%
% add N20 in climatological file
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%

type = 'climatological conditions file' ; 
history = 'ROMS' ;

%
% open the clm file  
% 
nc = netcdf(clmname,'write');
%
% new variable
%
redef(nc);

nc('n2o_time') = length(T);
nc{'n2o_time'} = ncdouble('n2o_time') ;

nc{'n2o_time'}.long_name = ncchar('time climatological N2O');
nc{'n2o_time'}.long_name = 'time climatological N2O';
nc{'n2o_time'}.units = ncchar('day');
nc{'n2o_time'}.units = 'day';
nc{'n2o_time'}.cycle_length = cycle;

nc{'N2O'} = ncdouble('n2o_time','s_rho','eta_rho','xi_rho') ;
nc{'N2O'}.long_name = ncchar('Nitrous oxide');
nc{'N2O'}.long_name = 'Nitrous oxide';
nc{'N2O'}.units = ncchar('mMol N2O m-3'); 
nc{'N2O'}.units = 'mMol N2O m-3';
nc{'N2O'}.fields = ncchar('NO2, scalar, series');
nc{'N2O'}.fields = 'NO2, scalar, series';
  
endef(nc);

%
% write new variable
%

nc{'n2o_time'}(:) =  T; 
nc{'N2O'}(:,:,:,:) =  N2O_clm; 

%
% Synchronize on disk
%
close(nc);

end

%% IL RESTE LES BRY A FAIRE pour N20 !
%% IL MANQUE LA GESTION DES BRY DANS LE CODE pour BIOEBUS !





