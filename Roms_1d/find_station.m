function [J,I]=find_station(fname,lon,lat)

nc=netcdf(fname);
X=nc{'X'}(:);
Y=nc{'Y'}(:);
if isempty(X)
  X=nc{'lon'}(:);
end
if isempty(X)
  X=nc{'longitude'}(:);
end
if isempty(X)
  error(['Empty longitude in ',fname])
end
if isempty(Y)
  Y=nc{'lat'}(:);
end
if isempty(Y)
  Y=nc{'latitude'}(:);
end
if isempty(Y)
  error(['Empty latitude in ',fname])
end
dl=X(2)-X(1);
lonmin=lon-dl;
lonmax=lon+dl;
latmin=lat-dl;
latmax=lat+dl;
J=min(find(Y>=latmin & Y<=latmax));
I=min(find(X-360>=lonmin & X-360<=lonmax));
if isempty(I),
 I=min(find(X>=lonmin & X<=lonmax));
 if isempty(I),
  I=min(find(X+360>=lonmin & X+360<=lonmax));
 end
end
close(nc);

return
