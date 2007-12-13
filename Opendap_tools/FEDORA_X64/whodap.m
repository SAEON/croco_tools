function text = whodap(url)
%--------------------------------------------------------------------
%     Copyright (C) URI/MIT
%     Revision: 1.0
%
%  function text = whodap(URL)
%
% DESCRIPTION:
%  Get the Dataset Descriptor Structure (DDS) for a data set 
%  given the URL for that data set. The DDS is an object used by DAP2 
%  to encode information about a data set's variable's names and 
%  data types. `whodap' accesses the data set, requests this object
%  and writes it to standard output. 
%  
%  Because DAP2 can represent a wide range of data types some of the 
%  variables in a data set may not be read directly into Matlab.
%  The DDS can help in choosing constraints for variables so they 
%  can be interned by Matlab. See EXAMPLE for an example; See loaddods
%  for information about a Matlab function which may be used to load
%  variables from DODS data sets into Matlab.
%  
% INPUT:
%  A DAP2 URL.
%
% OUTPUT:
%  A DAP2-accessible Dataset descriptor structure, as text.
%
% EXAMPLE:
%  whodap('http://test.opendap.org/opendap/nph-dods/data/coads.nc')
%	-> Returns structured text describing the names and types
%	   of variables in the dataset `coads.nc' in directory 'data'
%	   on `dods.gso.uri.edu'.
%
%  whodap('http://test.opendap.org/opendap/nph-dods/data/fnoc1.nc')
%       -> Returns:
%	Dataset {
%	    Int32 u[time_a = 16][lat = 17][lon = 21];
%	    Int32 v[time_a = 16][lat = 17][lon = 21];
%	    Float64 lat[lat = 17];
%	    Float64 lon[lon = 21];
%	    Float64 time[time = 16];
%	} fnoc1;
%
%	Indicating that the data set contains five arrays: U and V
%	have three dimensions while LAT, LON and TIME have one.
%  
% CALLER: general purpose
% CALLEE: whodap.m
%
% AUTHOR: James Gallagher, OPeNDAP
%---------------------------------------------------------------------

eval(['!writedap -D -- ', url])


