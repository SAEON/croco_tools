function [varargout] = mexnc ( varargin )
%  MEXNC MATLAB gateway to NetCDF interface.
%    MEXNC is a gateway to the NetCDF interface. To use this 
%    function, you should be familiar with the information about 
%    NetCDF contained in the User's Guide for NetCDF.  This 
%    documentation may be obtained from Unidata at 
%    <http://my.unidata.ucar.edu/content/software/netcdf/docs.html>;.
% 
%    The general syntax for MEXNC is 
%    mexnc(funcstr,param1,param2,...). There is a one-to-one 
%    correspondence between functions in the NetCDF 4.0 and 3.0 API and 
%    valid values for funcstr.  For example, mexnc('close',ncid) 
%    corresponds to the C library call nc_close (ncid).  MEXNC was
%    originally built on top of the NetCDF 2.4 API, however, and the 
%    correspondence between the NetCDF 2.4 API and values of funcstr 
%    is somewhat fuzzier.  It's recommended that new code use the 3.0 
%    or higher API.
% 
%    Syntax conventions
%    ------------------ 
%    The return status of a NetCDF-3 MEXNC operation will correspond
%    exactly to the return status of the corresponding NetCDF API 
%    function.  You can use the 'STRERROR' function below to get a string 
%    description of return status.  The return status values of NetCDF-2 
%    MEXCDF operations are not as well defined.  Usually they are -1  
%    in case of an error.
% 
%    "ncid" refers to the NetCDF file ID, "dimid" refers to a NetCDF
%    dimension ID, and "varid" refers to a NetCDF variable ID.
% 
%    NetCDF files use C-style ordering for multidimensional arrays, 
%    while MATLAB uses FORTRAN-style ordering.  This means that the size 
%    of the MATLAB array must be flipped relative to the defined 
%    dimension sizes of the NetCDF data set.  For example, if the NetCDF 
%    data set has dimensions 3-by-4-by-5, then the equivalent MATLAB 
%    array has size 5-by-4-by-3.  The PERMUTE command is useful for 
%    making any necessary conversions when reading from or writing to 
%    NetCDF data sets.
% 
%    Dataset functions
%    --------------
%      error_message = mexnc ( 'strerror', error_code );
%          Returns a reference to an error message string corresponding 
%          to an integer netCDF error status or to a system error 
%          number, presumably returned by a previous call to some other 
%          netCDF function. 
% 
%      lib_version = mexnc ( 'inq_libvers' );
%          Returns a string identifying the version of the netCDF library 
%          and when it was built.
% 
%      [ncid, status] = mexnc ( 'create', filename, access_mode );
%          The access mode can be a string such as 'clobber' or 
%          'noclobber', but it is preferable to use the helper functions
% 
%              nc_clobber_mode
%              nc_noclobber_mode
%              nc_share_mode
%              nc_64bit_offset_mode (new in NetCDF 3.6)
%          
%          These correspond to named constants in the <netcdf.h> file.  
%          Check the NetCDF User's Guide for more information.  You may 
%          also combine any of these with the bitor function, e.g.
%              >> access_mode = bitor ( nc_write_mode, nc_share_mode );
%          The access_mode is a required argument.
% 
%      [chunksize, ncid, status] = mexnc ( '_create', filename, access_mode, initialsize );
%          More advanced versionce of 'create'.  The 'initialsize' 
%          parameter sets the initial size of the file at creation
%          time.  See the netcdf man page for a description of 
%          chunksize.
%
%
%      [ncid, status] = mexnc ( 'open', filename, access_mode );
%          Opens an existing netCDF dataset for access.  Access modes 
%          available are
% 
%              nc_nowrite_mode
%              nc_write_mode
%              nc_share_mode
%        
%          If the access_mode is not given, the default is assumed to
%          be nc_nowrite_mode.
% 
%      [ncid, chunksizehint, status] 
%              = mexnc ( '_open', filename, access_mode, chunksizehint );
%
%          Same as usual OPEN operation with additional performance 
%          tuning parameter.  See the netcdf documentation for 
%          additional information.
% 
%      status = mexnc ( 'close', ncid );
%          Closes a previously-opened NetCDF file.
% 
%      status = mexnc ( 'redef', ncid );
%          Puts an open NetCDF dataset into define mode, so dimensions, 
%          variables, and attributes can be added or renamed and 
%          attributes can be deleted.  This function is not needed if the
%          file is opened in nc_hdf5_mode. 
% 
%      status = mexnc ( 'enddef', ncid );
%          Takes an open NetCDF file out of define mode.
% 
%      status = mexnc ( '_enddef', ncid, h_minfree, v_align, v_minfree, r_align );
%          Same as ENDDEF, but with enhanced performance tuning 
%          parameters.  See the man page for netcdf for details.
% 
%      status = mexnc ( 'sync', ncid );
%          Unless  the NC_SHARE bit is set in OPEN or CREATE, accesses 
%          to the underlying netCDF dataset are buffered by the 
%          library.  This function  synchronizes the state of the 
%          underlying dataset and the library.  This is done 
%          automatically by CLOSE and END_DEF.
% 
%      [ndims, nvars, ngatts, unlimdim, status] = mexnc ( 'inq', ncid );
%          Inquires as to the number of dimensions, number of variables,
%          number of global attributes, and the unlimited dimension (if
%          any).  
% 
%      [ndims, status] = mexnc ( 'inq_ndims', ncid );
%          Inquires as to the number of dimensions only. 
% 
%      [nvars, status] = mexnc ( 'inq_nvars', ncid );
%          Inquires as to the number of variables only. 
% 
%      [natts, status] = mexnc ( 'inq_natts', ncid );
%          Inquires as to the number of global attributes only. 
% 
%      [unlimdim, status] = mexnc ( 'inq_unlimdim', ncid );
%          Inquire as to the unlimited dimension.  As of NetCDF 4.0, this
%          will return just the first unlimited dimension.
% 
%      [status] = mexnc ( 'sync', ncid );
%          Offers a way to synchronize the disk copy of a netCDF dataset 
%          with in-memory buffers
%          
%      [status] = mexnc ( 'abort', ncid );
%          One does not really need this function.  Just ignore it.
% 
%      [old_fill_mode, status] = mexnc ( 'set_fill', ncid, new_fill_mode );
%          Determines whether or not variable prefilling will be done.  
%          The NetCDF dataset shall be writable.  new_fill_mode is
%          either nc_fill_mode to enable prefilling (the default) or 
%          nc_nofill_mode to disable  prefilling.  This function 
%          returns the previous setting in old_fill_mode.
% 
%    Dimension functions
%    --------------
%      [dimid, status] = mexnc ( 'def_dim', ncid, name, length );
%          Adds a new dimension to an open netCDF dataset in define 
%          mode. It returns a dimension ID, given the netCDF ID, the 
%          dimension name, and the dimension length.
% 
%      [dimid, status] = mexnc ( 'inq_dimid', ncid, name );
%          Returns (as an argument) the ID of a netCDF dimension, 
%          given the name of the dimension. If ndims is the number 
%          of dimensions defined for a netCDF dataset, each dimension 
%          has an ID between 0 and ndims-1.
% 
%      [name, length, status] = mexnc ( 'inq_dim', ncid, dimid );
%          Returns information about a netCDF dimension including its 
%          name and its length. The length for the unlimited dimension, 
%          if any, is the number of records written so far.
% 
%      [name, status] = mexnc ( 'inq_dimname', ncid, dimid );
%          Returns the name of a dimension given the dimid.
% 
%      [dimlength, status] = mexnc ( 'inq_dimlen', ncid, dimid );
%          Returns the length of a dimension given the dimid.  The 
%          length for the unlimited dimension is the number of records
%          written so far.
% 
%      [status] = mexnc ( 'rename_dim', ncid, dimid, name );
%          Renames an existing dimension in a netCDF dataset open for 
%          writing.
% 
%    General Variable functions
%    --------------------------
%      [varid, status] = mexnc ( 'def_var', ncid, name, datatype, ndims, dimids );
%          Adds a new variable to a netCDF dataset.  In order to define 
%          a singleton variable (one with one element and no defined 
%          dimensions, set ndims to 1 and dimids = [].
% 
%      [varid, status] = mexnc ( 'inq_varid', ncid, varname );
%          Returns the ID of a netCDF variable, given its name.
% 
%      [varname, datatype, ndims, dimids, natts, status] = mexnc ( 'inq_var', ncid, varid );
%          Returns other information about a NetCDF variable given its ID.
% 
%      [varname, status] = mexnc ( 'inq_varname', ncid, varid );
%          Returns variable name given its ID.
% 
%      [vartype, status] = mexnc ( 'inq_vartype', ncid, varid );
%          Returns numeric datatype given its ID.
% 
%      [varndims, status] = mexnc ( 'inq_varndims', ncid, varid );
%          Returns number of dimensions given the varid.
% 
%      [dimids, status] = mexnc ( 'inq_vardimid', ncid, varid );
%          Returns dimension identifiers given the varid.
% 
%      [varnatts, status] = mexnc ( 'inq_varnatts', ncid, varid );
%          Returns number of variable attributes given the varid.
% 
%      status = mexnc ( 'rename_var', ncid, varid, new_varname );
%          Changes  the  name  of  a  netCDF  variable.
% 
%   Variable I/O functions
%   ----------------------
%     Any routines marked "*XXX" constitute a suite of routines
%     that are specialized for various datatypes.  Possibilities
%     for XXX include "text", "uchar", "schar", "short", "int", 
%     "float", and "double".  The data is automatically converted 
%     to the external type of the specified variable.    Since 
%     MATLAB's default datatype is double precision, most of the 
%     time you would want XXX to be "double"
%
%     Because of the difference between row-major order (C) and 
%     column-major order (matlab), you should transpose or permute 
%     your data before passing it into or after receiving it from 
%     these I/O routines.  See the User's Guide for examples.
%
%     MAJOR DIFFERENCE BETWEEN THESE FUNCTIONS AND MexCDF(netcdf-2).
%         These functions do not make use of the add_offset and
%         scale_factor attributes.  That job is left to any user
%         routines written as a wrapper to MexCDF.
%
%         The varid must be the actual varid, substituting the name 
%         of the variable is not allowed.
%
%     status = mexnc ( 'put_var_XXX', ncid, varid, data );
%         PUT_VAR_XXX writes an entire variable.  
%
%     [data, status] = mexnc ( 'get_var_XXX', ncid, varid );
%         PUT_VAR_XXX's alter ego.  Retrieves all the data from a
%         variable and casts it into the requested datatype.
%
%     status = mexnc ( 'put_vara_XXX', ncid, varid, start_coord, 
%                                         count_coord, data );
%         Writes a hyperslab of data to a netcdf variable.  The
%         start_coord specifies the indices where the write operation
%         is to begin, and the count_coord specifies the extent 
%         of the write along each dimension.
%
%     [data, status] = mexnc ( 'get_vara_XXX', ncid, varid, start_coord, count_coord );
%         Retrieves a hyperslab from a variable, starting at a specific
%         index coordinate and with a count extent along each dimension.
%
%     status = mexnc ( 'put_vars_XXX', ncid, varid, 
%                          start_coord, count_coord, stride_coord, data );
%         Writes a hyperslab of data to a netcdf variable.  The "stride"
%         or interval between accessed values) must be specified for
%         each dimension.
%
%     [data, status] = mexnc ( 'get_vars_XXX', ncid, varid, start_coord, 
%                                 count_coord, stride_coord );
%         Retrieves a strided hyperslab from a variable, given a starting 
%         coordinate, a given count of elements to read along each dimension,
%         and the stride between accessed elements along each dimension.
%
%     status = mexnc ( 'put_var1_XXX', ncid, varid, start_coord, data );
%         Only a single value is to be written.  The coordinate of that 
%         value must be specified.
%
%     [data, status] = mexnc ( 'get_var1_XXX', ncid, varid, index );
%         Only a single value is to be read.  The index of that value 
%         must be specified.
%
%     status = mexnc ( 'put_varm_XXX', ncid, varid, start_coord, 
%                         count_coord, stride_coord, imap_coord, data );
%         Writes a hyperslab of data to a netcdf variable.  Similar to
%         stride I/O, except that an additional index mapping vector 
%         is provided to specify the in-memory arrangement of the  
%         data values.  
%
%     [data, status] = mexnc ( 'get_varm_XXX', ncid, varid, start_coord, 
%                                 count_coord, stride_coord, imap_coord );
%         Retrieves a memory-mapped strided hyperslab from a variable, 
%         given a starting coordinate, a given count of elements to 
%         read along each dimension, the stride between accessed 
%         elements along each dimension, and the in-memory arrangement
%         of the data values.  This routine is severly complicated by
%         the row-major-order-column-major-order problem.  Please see 
%         the User Guide for examples, and then remember that the 
%         answer you get back is transposed.  
%
%   Attribute functions
%   -------------------
%     Any routines marked "*XXX" constitute a suite of routines
%     that are specialized for various datatypes.  Possibilities
%     for XXX include "uchar", "schar", "short", "int", "float", 
%     and "double".  The data is automatically converted to the 
%     external type of the specified attribute.    
%
%     status = mexnc ( 'copy_att', ncid_in, varid_in, attname, ncid_out, varid_out );
%         Copies an attribute from one variable to another, possibly
%         within the same netcdf file.
%
%     status = mexnc ( 'del_att', ncid, varid, attname );
%         Deletes an attribute.
%
%     [att_value, status] = mexnc ( 'get_att_XXX', ncid, varid, attname );
%         Retrieves an attribute value. 
%
%     [datatype, attlen, status] = mexnc ( 'inq_att', ncid, varid, attname );
%         Retrieves the datatype and length of an attribute given its
%         name.
%
%     [attid, status] = mexnc ( 'inq_attid', ncid, varid, attname );
%         Retrieves the numeric id of an attribute given its name.
%
%     [att_len, status] = mexnc ( 'inq_attlen', ncid, varid, attname );
%         Retrieves the length of an attribute given the name.
%
%     [attname, status] = mexnc ( 'inq_attname', ncid, varid, attid );
%         Retrieves the name of an attribute given its numeric attribute id.
%
%     [att_type, status] = mexnc ( 'inq_atttype', ncid, varid, attname );
%         Retrieves the numeric id of the datatype of an attribute
%
%     status = mexnc ( 'put_att_XXX', ncid, varid, attname, datatype, attlen, attvalue );
%         Writes an attribute value.  The "datatype" attribute can be 
%         either the numeric value of the external datatype (e.g. 6 
%         for nc_double), or just the string equivalent (e.g. "double").
%
%     status = mexnc ( 'rename_att', ncid, varid, old_attname, new_attname );
%         Renames an attribute.
%
%
%  NetCDF 2.4 API
%  --------------
% 
%  These functions constitute the time-tested mexcdf that was build on 
%  top of the NetCDF 2.4 API.  They continue to work, but in some cases operate
%  somewhat differently than the MexCDF(netcdf-3) functions.
% 
%      status = mexnc('ENDEF', cdfid)
%      [ndims, nvars, natts, recdim, status] = mexnc('INQUIRE', cdfid)
% 
%      status = mexnc('DIMDEF', cdfid, 'name', length)
%      [dimid, rcode] = mexnc('DIMID', cdfid, 'name')
%      [name, length, status] = mexnc('DIMINQ', cdfid, dimid)
%      status = mexnc('DIMRENAME', cdfid, 'name')
% 
%      status = mexnc('VARDEF', cdfid, 'name', datatype, ndims, [dim])
%      [varid, rcode] = mexnc('VARID', cdfid, 'name')
%      [name, datatype, ndims, dimids, natts, status] = mexnc('VARINQ', cdfid, varid)
%      status = mexnc('VARPUT1', cdfid, varid, coords, value, autoscale)
%      [value, status] = mexnc('VARGET1', cdfid, varid, coords, autoscale)
%      status = mexnc('VARPUT', cdfid, varid, start, count, value, autoscale)
%      [value, status] = mexnc('VARGET', cdfid, varid, start, count, autoscale)
%      status = mexnc('VARPUTG', cdfid, varid, start, count, stride, [], value, autoscale)
%      [value, status] = mexnc('VARGETG', cdfid, varid, start, count, stride, [], autoscale)
%      status = mexnc('VARRENAME', cdfid, varid, 'name')
% 
%      status = mexnc('ATTPUT', cdfid, varid, 'name', datatype, len, value) 
%          
%          A negative value on the length will cause the mexfile to 
%          try to figure out the length itself.
%
%      [datatype, len, status] = mexnc('ATTINQ', cdfid, varid, 'name')
%      [value, status] = mexnc('ATTGET', cdfid, varid, 'name')
%      status = mexnc('ATTCOPY', incdf, invar, 'name', outcdf, outvar)
%      [name, status] = mexnc('ATTNAME', cdfid, varid, attnum)
%      status = mexnc('ATTRENAME', cdfid, varid, 'name', 'newname')
%      status = mexnc('ATTDEL', cdfid, varid, 'name')
% 
%      status = mexnc('RECPUT', cdfid, recnum, [data], autoscale, recdim)
%      [data, status] = mexnc('RECGET', cdfid, recnum, autoscale, recdim)
%      [recvarids, recsizes, status] = mexnc('RECINQ', cdfid, recdim)
% 
%      len = mexnc('TYPELEN', datatype)
%      old_fillmode = mexnc('SETFILL', cdfid, fillmode)
% 
%      old_ncopts = mexnc('SETOPTS', ncopts)
%      ncerr = mexnc('ERR')
%      code = mexnc('PARAMETER', 'NC_...')
%
%
%  Non-NetCDF related functions
%  ----------------------------
%     [release_name, release_date] = mexnc ( 'get_mexnc_info' );
%         Return information about this particular release of mexnc.  If
%         the release version is "2.0.19" and the release was made on 
%         January 1, 2005, then
% 
%         "release_name" would be "$Name$"
%         "release_date" would be "$Date: 2006-05-25 11:36:51 -0400 (Thu, 25 May 2006) $"
% 
% 
%    The general framework of this document was shamelessly lifted from
%    the help information for the HDFSD interface.  Many of the function
%    descriptions were lifted nearly verbatim from the NetCDF online
%    documentation.
% 
%  AUTHOR:
%     John Evans (johnevans@acm.org) wrote the MexNC(netcdf-3) portion.
%     Chuck Denham wrote the original MexCDF(netcdf-2). 
%
