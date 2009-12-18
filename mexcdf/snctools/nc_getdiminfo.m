function dinfo = nc_getdiminfo ( arg1, arg2 )
% NC_GETDIMINFO:  returns metadata about a specific NetCDF dimension
%
% DINFO = NC_GETDIMINFO(NCFILE,DIMNAME) returns information about the
% dimension DIMNAME in the netCDF file NCFILE.
%
% DINFO = NC_GETDIMINFO(NCID,DIMID) returns information about the
% dimension with numeric ID DIMID in the already-opened netCDF file
% with file ID NCID.  This form is not recommended for use from the
% command line.
%
% Upon output, DINFO will have the following fields.
%
%    Name:  
%        a string containing the name of the dimension.
%    Length:  
%        a scalar equal to the length of the dimension
%    Unlimited:  
%        A flag, either 1 if the dimension is an unlimited dimension
%        or 0 if not.
%
% In case of an error, an exception is thrown.
%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% $Id: nc_getdiminfo.m 2559 2008-11-28 21:53:27Z johnevans007 $
% $LastChangedDate: 2008-11-28 16:53:27 -0500 (Fri, 28 Nov 2008) $
% $LastChangedRevision: 2559 $
% $LastChangedBy: johnevans007 $
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


nargchk(2,2,nargin);
nargoutchk(1,1,nargout);

nc_method = determine_retrieval_method(arg1,arg2);

dinfo = nc_method(arg1,arg2);



%-------------------------------------------------------------------------
function nc_method = determine_retrieval_method(arg1,arg2)

% Need this in order to determine if we can use java.
import ucar.nc2.*

switch ( version('-release') )
	case { '11', '12', '13', '14', '2006a', '2006b', '2007a', '2007b', '2008a' }
		can_use_tmw = false;
	otherwise
		can_use_tmw = true;
end


file_is_nc3 = ischar(arg1) && exist(arg1,'file') && isnc3(arg1);
file_is_nc4 = ischar(arg1) && exist(arg1,'file') && isnc4(arg1);
file_is_url = ischar(arg1) && ~isempty(regexp(arg1,'\<http[s]*'));
mexnc_available = (exist('mexnc') == 2);
java_available = (exist('NetcdfFile') == 8);


if file_is_nc3
    % If the version is R2008b or later, use native matlab
    if can_use_tmw
        nc_method = @nc_getdiminfo_tmw;
    elseif mexnc_available
        nc_method = @nc_getdiminfo_mexnc;
    elseif java_available
        nc_method = @nc_getdiminfo_java;
    else
        error('SNCTOOLS:nc_varget:noRetrievalMethodAvailable', ...
              'Neither MATLAB, MEXNC, nor JAVA is available to retrieve data from your netcdf-3 file.');
    end

elseif file_is_url

    if java_available
        nc_method = @nc_getdiminfo_java;
    else % just try mexnc, hope that it is opendap-enabled
        nc_method = @nc_getdiminfo_mexnc;
    end

elseif isa(arg1,'ucar.nc2.NetcdfFile')

    nc_method = @nc_getdiminfo_java;

elseif isa(arg1,'double')

	if can_use_tmw
        nc_method = @nc_getdiminfo_tmw;
	else
        nc_method = @nc_getdiminfo_mexnc;
	end

elseif file_is_nc4

	can_use_mexnc = false;
	try 
		[ncid, status] = mexnc('open',arg1,'nowrite');
		if ( status == 0 )
			can_use_mexnc = true;
			mexnc('close',ncid);
		end
	catch
		can_use_mexnc = false;
	end

	if can_use_mexnc
        nc_method = @nc_getdiminfo_mexnc;
	elseif java_available
        nc_method = @nc_getdiminfo_java;
    else
        error('SNCTOOLS:nc_varget:noRetrievalMethodAvailable', ...
              'No retrieval method is available for your netcdf-4 file.');
    end

end





%===============================================================================
function dinfo = nc_getdiminfo_java ( arg1, arg2 )

import ucar.nc2.dods.*  ;
import ucar.nc2.*       ;

if isa(arg1,'char') && isa(arg2,'char')
	if exist(arg1,'file')
		jncid = NetcdfFile.open(arg1);
	else
		jncid = DODSNetcdfFile(arg1);
	end
	dim = jncid.findDimension(arg2);
elseif isa(arg1,'ucar.nc2.NetcdfFile') && isa(arg2,'ucar.nc2.Dimension')
	jncid = arg1;
	dim = arg2;
elseif isa(arg1,'ucar.nc2.dods.DODSNetcdfFile') && isa(arg2,'ucar.nc2.Dimension')
	jncid = arg1;
	dim = arg2;
else
	eid = 'SNCTOOLS:nc_getdiminfo:java:badDatatypes';
	msg = sprintf ( 'For a java retrieval, datatypes must be either both char, or one must be a file ID and the other a dimension ID.' );
	error ( eid, msg );
end

dinfo.Name = char(dim.getName());
dinfo.Length = dim.getLength();
dinfo.Unlimited = dim.isUnlimited();

if isa(arg1,'char') && isa(arg2,'char')
	jncid.close();	
end

return

%===============================================================================
function dinfo = nc_getdiminfo_tmw ( arg1, arg2 )

%
% If we are here, then we must have been given something local.
if ischar(arg1) && ischar(arg2)
    dinfo = handle_char_nc_getdiminfo_tmw(arg1,arg2);

elseif isnumeric ( arg1 ) && isnumeric ( arg2 )
	dinfo = handle_numeric_nc_getdiminfo_tmw(arg1,arg2);
else
	error ( 'SNCTOOLS:NC_GETDIMINFO_TMW:badInputDatatypes', ...
	            'Must supply either two character or two numeric arguments.' );
end



return





%===============================================================================
function dinfo = nc_getdiminfo_mexnc ( arg1, arg2 )

%
% If we are here, then we must have been given something local.
if ischar(arg1) && ischar(arg2)
    dinfo = handle_char_nc_getdiminfo(arg1,arg2);
elseif isnumeric ( arg1 ) && isnumeric ( arg2 )
	dinfo = handle_numeric_nc_getdiminfo(arg1,arg2);
else
	error ( 'SNCTOOLS:NC_GETDIMINFO_MEX:badInputDatatypes', ...
	            'Must supply either two character or two numeric arguments.' );
end



return





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function dinfo = handle_char_nc_getdiminfo ( ncfile, dimname )

[ncid,status ]=mexnc('open', ncfile, nc_nowrite_mode );
if status ~= 0
	ncerror = mexnc ( 'strerror', status );
	error ( 'SNCTOOLS:NC_GETDIMINFO:handle_char_nc_getdiminfo:openFailed', ncerror );
end


[dimid, status] = mexnc('INQ_DIMID', ncid, dimname);
if ( status ~= 0 )
	mexnc('close',ncid);
	ncerror = mexnc ( 'strerror', status );
	error ( 'SNCTOOLS:NC_GETDIMINFO:handle_char_nc_getdiminfo:inq_dimidFailed', ncerror );
end


dinfo = handle_numeric_nc_getdiminfo ( ncid,  dimid );

mexnc('close',ncid);






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function dinfo = handle_numeric_nc_getdiminfo ( ncid, dimid )


[unlimdim, status] = mexnc ( 'inq_unlimdim', ncid );
if status ~= 0
	mexnc('close',ncid);
	ncerror = mexnc ( 'strerror', status );
	error ( 'SNCTOOLS:NC_GETDIMINFO:MEXNC:inq_ulimdimFailed', ncerror );
end



[dimname, dimlength, status] = mexnc('INQ_DIM', ncid, dimid);
if status ~= 0
	mexnc('close',ncid);
	ncerror = mexnc ( 'strerror', status );
	error ( 'SNCTOOLS:NC_GETDIMINFO:MEXNC:inq_dimFailed', ncerror );
end

dinfo.Name = dimname;
dinfo.Length = dimlength;

if dimid == unlimdim
	dinfo.Unlimited = true;
else
	dinfo.Unlimited = false;
end


return






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function dinfo = handle_char_nc_getdiminfo_tmw ( ncfile, dimname )

ncid=netcdf.open(ncfile,nc_nowrite_mode);
dimid = netcdf.inqDimID(ncid, dimname);
dinfo = handle_numeric_nc_getdiminfo_tmw ( ncid,  dimid );
netcdf.close(ncid);






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function dinfo = handle_numeric_nc_getdiminfo_tmw ( ncid, dimid )


[dud,dud,dud,unlimdim] = netcdf.inq(ncid );
[dimname, dimlength] = netcdf.inqDim(ncid, dimid);


dinfo.Name = dimname;
dinfo.Length = dimlength;

if dimid == unlimdim
	dinfo.Unlimited = true;
else
	dinfo.Unlimited = false;
end


return
