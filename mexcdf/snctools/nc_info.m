function fileinfo = nc_info ( ncfile )
% NC_INFO:  information about a NetCDF 2 or 3 file
%
% USAGE:  fileinfo = nc_info ( ncfile );
%
% PARAMETERS:
% Input:
%    ncfile:  
%        a string that specifies the name of the NetCDF file
% Output:
%    fileinfo:
%        A structure whose fields contain information about the contants
%        of the NetCDF file.  The set of fields return in "fileinfo" are:
%
%        Filename:  
%            a string containing the name of the file.
%        Dimension:  
%            an array of structures describing the NetCDF dimensions.
%        Dataset:  
%            an array of structures describing the NetCDF datasets.
%        Attributes:  
%            An array of structures These correspond to the global attributes.
%
%
%        Each "Dimension" element contains the following fields:
%       
%        Name:
%            a string containing the name of the dimension.
%        Length:
%            a scalar value, the size of this dimension
%        Unlimited:
%            Set to 1 if the dimension is the record dimension, set to
%            0 otherwise.
%
%
%        Each "Dataset" element contains the following structures.
%
%        Name:  
%            a string containing the name of the variable.
%        Nctype:  
%            a string specifying the NetCDF datatype of this variable.
%        Dimensions:  
%            a cell array with the names of the dimensions upon which
%            this variable depends.
%        Unlimited:  
%            Flag, either 1 if the variable has an unlimited dimension
%            or 0 if not.
%        Rank:  
%            Array that describes the size of each dimension upon which 
%            this dataset depends.
%        DataAttributes:  
%            Same as "Attributes" above, but here they are the variable 
%            attributes.
%                         
%        Each "Attribute" or "DataAttribute" element contains the following 
%        fields.
%
%        Name:  
%            a string containing the name of the attribute.
%        Nctype:  
%            a string specifying the NetCDF datatype of this attribute.
%        Attnum:  
%            a scalar specifying the attribute id
%        Value: 
%            either a string or a double precision value corresponding to
%            the value of the attribute
%
%
% The "Dataset" elements are not populated with the actual data values.
%
% This routine purposefully mimics that of Mathwork's hdfinfo.
%
%
%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% $Id: nc_info.m 2559 2008-11-28 21:53:27Z johnevans007 $
% $LastChangedDate: 2008-11-28 16:53:27 -0500 (Fri, 28 Nov 2008) $
% $LastChangedRevision: 2559 $
% $LastChangedBy: johnevans007 $
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



nargchk(1,1,nargin);
nargoutchk(1,1,nargout);

nc_method = determine_retrieval_method(ncfile);

fileinfo = nc_method(ncfile);

return











%--------------------------------------------------------------------------
function retrieval_method = determine_retrieval_method(ncfile)

retrieval_method = [];

switch ( version('-release') )
	case { '11', '12', '13', '14', '2006a', '2006b', '2007a', '2007b', '2008a' }
		can_use_tmw = false;
	otherwise
		can_use_tmw = true;
end

file_is_nc3 = exist(ncfile,'file') && isnc3(ncfile);
file_is_nc4 = exist(ncfile,'file') && isnc4(ncfile);
file_is_url = ~isempty(regexp(ncfile,'\<http[s]*'));
mexnc_available = (exist('mexnc') == 2);
java_available = (exist('NetcdfFile') == 8);

% Need this in order to determine if we can use java.
import ucar.nc2.*

if file_is_nc3
    % If the version is R2008b or later, use native matlab
    if can_use_tmw
        retrieval_method = @nc_info_tmw;
    elseif mexnc_available
        retrieval_method = @nc_info_mexnc;
    elseif java_available
        retrieval_method = @nc_info_java;
    else
        error('SNCTOOLS:nc_info:noRetrievalMethodAvailable', ...
              'Neither MATLAB, MEXNC, nor JAVA is available to retrieve data from your netcdf-3 file.');
    end

elseif file_is_url

    if java_available
        retrieval_method = @nc_info_java;
    else % just try mexnc
        retrieval_method = @nc_info_mexnc;
    end

elseif file_is_nc4

	can_use_mexnc = false;
	try 
		[ncid, status] = mexnc('open',ncfile,'nowrite');
		if ( status == 0 )
			can_use_mexnc = true;
			mexnc('close',ncid);
		else
			can_use_mexnc = false;
		end
	catch 
		can_use_mexnc = false;
	end

    if can_use_mexnc;
        retrieval_method = @nc_info_mexnc;
    elseif java_available 
        retrieval_method = @nc_info_java;
    else
        error('SNCTOOLS:nc_info:javaRetrievalMethodNotAvailable', ...
              'You currently neeed either JAVA or a netcdf-4 capable version of mexnc to read data from your netcdf-4 file.');
    end

end

if isempty(retrieval_method)
    error('SNCTOOLS:nc_varget:noRetrievalMethodAvailable', ...
          'Neither MATLAB, MEXNC, nor JAVA is available to retrieve data from your file.');
end

%--------------------------------------------------------------------------
function fileinfo = nc_info_tmw ( ncfile )


fileinfo.Filename = ncfile;

ncid=netcdf.open(ncfile, nc_nowrite_mode );
[ndims, nvars, ngatts, record_dimension] = netcdf.inq(ncid);


%
% Get the dimensions
if ndims == 0
	Dimension = struct ( [] );
else
	if ndims > 0
		Dimension(1)=nc_getdiminfo ( ncid, 0 );
	end
	Dimension = repmat ( Dimension, ndims,1 );
	for dimid = 1:ndims-1
		Dimension(dimid+1)=nc_getdiminfo ( ncid, dimid );
	end
end



%
% Get the global attributes.
if ngatts == 0
	fileinfo.Attribute = struct([]);
else
	if ngatts > 0
		Attribute(1) = nc_get_attribute_struct_tmw ( ncid, nc_global, 0 );
	end
	Attribute = repmat ( Attribute, ngatts, 1 );
	for attnum = 1:ngatts-1
		Attribute(attnum+1) = nc_get_attribute_struct_tmw ( ncid, nc_global, attnum );
	end
	fileinfo.Attribute = Attribute;
end





%
% Get the variable information.
if nvars == 0
	Dataset = struct([]);
else
	if ( nvars > 0 )
		Dataset(1) = nc_getvarinfo ( ncid, 0 );
	end
	Dataset = repmat ( Dataset, nvars, 1 );
	for varid=1:nvars-1
		Dataset(varid+1) = nc_getvarinfo ( ncid, varid );
	end
end

fileinfo.Dimension = Dimension;
fileinfo.Dataset = Dataset;


netcdf.close(ncid);


return








%--------------------------------------------------------------------------
function fileinfo = nc_info_mexnc ( ncfile )


fileinfo.Filename = ncfile;

[ncid, status]=mexnc('open', ncfile, nc_nowrite_mode );
if status ~= 0
    ncerr = mexnc('strerror', status);
    error ( 'SNCTOOLS:NC_INFO:MEXNC:OPEN', ncerr );
end



[ndims, nvars, ngatts, record_dimension, status] = mexnc('INQ', ncid);
if status ~= 0
    ncerr = mexnc('strerror', status);
    mexnc('close',ncid);
    error ( 'SNCTOOLS:NC_INFO:MEXNC:INQ', ncerr );
end


%
% Get the dimensions
if ndims == 0
	Dimension = struct ( [] );
else
	if ndims > 0
		Dimension(1)=nc_getdiminfo ( ncid, 0 );
	end
	Dimension = repmat ( Dimension, ndims,1 );
	for dimid = 1:ndims-1
		Dimension(dimid+1)=nc_getdiminfo ( ncid, dimid );
	end
end



%
% Get the global attributes.
if ngatts == 0
	fileinfo.Attribute = struct([]);
else
	if ngatts > 0
		Attribute(1) = nc_get_attribute_struct ( ncid, nc_global, 0 );
	end
	Attribute = repmat ( Attribute, ngatts, 1 );
	for attnum = 1:ngatts-1
		Attribute(attnum+1) = nc_get_attribute_struct ( ncid, nc_global, attnum );
	end
	fileinfo.Attribute = Attribute;
end





%
% Get the variable information.
if nvars == 0
	Dataset = struct([]);
else
	if ( nvars > 0 )
		Dataset(1) = nc_getvarinfo ( ncid, 0 );
	end
	Dataset = repmat ( Dataset, nvars, 1 );
	for varid=1:nvars-1
		Dataset(varid+1) = nc_getvarinfo ( ncid, varid );
	end
end

fileinfo.Dimension = Dimension;
fileinfo.Dataset = Dataset;


mexnc('close',ncid);


return









