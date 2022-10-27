clear all
close all

%%%%%%%%%%%%%%%%%%%%% USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%%
%
% Common parameters
%
crocotools_param                      
frc_prefix=[frc_prefix,'_TIDETPXO_'];
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% end of user input  parameters
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
if level==0
  nc_suffix='.nc';
else
  nc_suffix=['.nc.',num2str(level)];
  grdname=[grdname,'.',num2str(level)];
end
%
% Get the model grid
%
disp(' ')
disp([' Read the grid in ',grdname])
nc=netcdf(grdname);
Lp=length(nc('xi_rho'));
Mp=length(nc('eta_rho'));
lon=nc{'lon_rho'}(:);
lat=nc{'lat_rho'}(:);
lonu=nc{'lon_u'}(:);
latu=nc{'lat_u'}(:);
lonv=nc{'lon_v'}(:);
latv=nc{'lat_v'}(:);
angle=nc{'angle'}(:);
close(nc);

for Y=Ymin:Ymax
    if Y==Ymin 
        mo_min=Mmin;
    else
        mo_min=1;
    end
    if Y==Ymax
      mo_max=Mmax;
    else
        mo_max=12;
    end
    for M=mo_min:mo_max
        disp(' ')
        disp(['Processing  year ',num2str(Y),...
              ' - month ',num2str(M)])
        disp(' ')
        %-------------------------------------------------------------------%
        %
        % Create the CROCO forcing files
        %
        % ------------------------------------------------------------------%
        %
        disp(['====================='])
        disp('Create the tide-only frc netcdf file')
        disp(['====================='])
        %
        frcname=[frc_prefix,'Y',num2str(Y),...
                 'M',num2str(sprintf(Mth_format,M)),nc_suffix];
        disp(['Create a new only tide forcing file: ' frcname])
	      disp([' '])
        create_forcing_tideonly(frcname,grdname,CROCO_title)
        %
        % Add the tides
        %
        makeplot_tides=0;
        pot_tides=1;
        add_tidal_data(tidename,grdname,frcname,Ntides,tidalrank,...
                           Yorig,Y,M,coastfileplot,makeplot_tides,pot_tides)
                         
    end
end
