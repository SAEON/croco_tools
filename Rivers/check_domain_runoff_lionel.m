function indomain=check_domain_runoff(lon_grid,lat_grid,lon_real,lat_real)
%
% Function that detect that the river is in the croco domain
%
% lat_real
% lon_real

if ( lon_real >= max(lon_grid(:)) | lon_real <= min(lon_grid(:)) ...
        | lat_real >= max(lat_grid(:)) | lat_real <= min(lat_grid(:)) )
    indomain=0;
else
    indomain=1;
end
return
