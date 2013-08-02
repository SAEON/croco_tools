function [i,j]=grid_pos(lon_grid,lat_grid,lon_real,lat_real);

%Purpose :
% find the i and j index of a real position on the ROMS grid
% lon_grid and lat_grid are 2D matrix : longitude and latitude of the ROMS grid
% lon_real,lat_real : are scalars for the real position point
%
%
    mini=min( (lon_grid-lon_real).^2 + (lat_grid-lat_real).^2 );
    [m j]=min(mini);
    [m i]=min( ( lon_grid(:,j)-lon_real ).^2 + ( lat_grid(:,j)-lat_real ).^2 );
return