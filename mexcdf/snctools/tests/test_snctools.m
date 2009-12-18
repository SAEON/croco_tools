function test_snctools()
% TEST_SNCTOOLS
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% $Id: test_snctools.m 2559 2008-11-28 21:53:27Z johnevans007 $
% $LastChangedDate: 2008-11-28 16:53:27 -0500 (Fri, 28 Nov 2008) $
% $LastChangedRevision: 2559 $
% $LastChangedBy: johnevans007 $
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% switch off some warnings
mver = version('-release');
switch mver
    case {'11', '12'}
        error ( 'This version of MATLAB is too old, SNCTOOLS will not run.' );
    case {'13'}
        error ( 'R13 is not supported in this release of SNCTOOLS');
    otherwise
        warning('off', 'SNCTOOLS:nc_archive_buffer:deprecatedMessage' );
        warning('off', 'SNCTOOLS:nc_datatype_string:deprecatedMessage' );
        warning('off', 'SNCTOOLS:nc_diff:deprecatedMessage' );
        warning('off', 'SNCTOOLS:nc_getall:deprecatedMessage' );
        warning('off', 'SNCTOOLS:snc2mat:deprecatedMessage' );
end


switch mver
    case {'14', '2006a', '2006b', '2007a', '2007b', '2008a'}
		fprintf ( 1, 'Your version of MATLAB is %s, SNCTOOLS will require MEXNC in order to run local I/O tests.\n', mver );
		pause(1);
		% Do we have mexnc?
		if exist('mexnc') == 2
			fprintf ( 1, 'Good, looks like we found mexnc.\n\n\n' );
			pause(1);
		else
			fprintf ( 1, 'Cannot find mexnc, you will need java in order to accomplish anything.\n\n\n' );
			pause(1);
		end

    otherwise
		fprintf ( 1, 'Your version of MATLAB is %s, SNCTOOLS will use the MATLAB native netCDF package to run all local I/O tests.\n\n\n', mver );
		pause(1);
		
end

if snctools_use_java
	fprintf ( 1, 'Good, looks like you have java support ready to go, we can test OPeNDAP URLs.\n\n\n' );
	pause(1);
end


run_backend_neutral_tests;
run_backend_mex_tests;

fprintf ( 1, '\nAll  possible tests for your configuration have been run.  Bye.\n\n' );

return





%
% Save any old settings.
old_settings.test_remote_mexnc = getpref('SNCTOOLS','TEST_REMOTE_MEXNC',false);
old_settings.test_remote_java = getpref('SNCTOOLS','TEST_REMOTE_JAVA',false);

for pref_i = [0:1]
	setpref('SNCTOOLS','USE_JAVA',pref_i);


	% Go thru all possible settins of USE_TMW.
	% Only allow [0 1] if R2008b or later.
	if strcmp(mver,'2008b')
		preflist = [0 1];
	else
		preflist = 0;
	end

	for pref_j = preflist
		setpref('SNCTOOLS','USE_TMW',pref_j);

		% Go thru all possible settings of PRESERVE_FVD
		for pref_k = [0:1]
			setpref('SNCTOOLS','PRESERVE_FVD',pref_k);

			p = getpref ( 'SNCTOOLS' );
			if ~isempty(p)
			    fprintf ( 1, '\nYour current SNCTOOLS preferences are set to \n' );
			    p
			end

			run_mexnc_tests;
			run_java_tests;
			run_tmw_tests;
		end
	end
end




fprintf ( 1, '\nRestoring old settings...\n' );
setpref('SNCTOOLS','TEST_REMOTE_JAVA',old_settings.test_remote_java);
setpref('SNCTOOLS','TEST_REMOTE_MEXNC',old_settings.test_remote_mexnc);
return


%----------------------------------------------------------------------
function run_mexnc_tests()

% Is mexnc ok?
mexnc_loc = which ( 'mexnc' );
mexnc_ok = ~isempty(which('mexnc'));

pause_duration = 3;
if ~mexnc_ok
    fprintf ( 1, 'MEXNC was not found, so the tests requiring mexnc\n' );
    fprintf ( 1, 'will not be run.\n\n' );
    return
end

fprintf ( 1, '\n' );
fprintf ( 1, 'Ok, we found mexnc.  ' );
fprintf ( 1, 'Remote OPeNDAP/mexnc tests ' );
if getpref('SNCTOOLS','TEST_REMOTE_MEXNC',false)
    fprintf ( 1, 'will ' );
    setpref('SNCTOOLS','TEST_REMOTE',true)
else
    fprintf ( 1, 'will NOT ' );
    setpref('SNCTOOLS','TEST_REMOTE',false)
end
fprintf ( 1, 'be run.\n  Starting tests in ' );
for j = 1:pause_duration
    fprintf ( 1, '%d... ', pause_duration - j + 1 );
    pause(1);
end
fprintf ( 1, '\n' );

run_backend_neutral_tests;
run_backend_mexnc_tests;


return


%----------------------------------------------------------------------
function run_java_tests()

% figure out how the user has set things up
% Is java access ok?
java_ok = usejava('jvm') && getpref('SNCTOOLS','USE_JAVA',false);
mexnc_ok = ~isempty(which('mexnc'));
toolsUI_ok = false;
if java_ok
    import ucar.nc2.* ;
    toolsUI_ok = ~isempty(which('NetcdfFile'));
end


if ~java_ok
    fprintf ( 1, '\n' );
    fprintf ( 1, 'Looks like java is not enabled and/or SNCTOOLS is not \n' );
    fprintf ( 1, 'enabled to use java, so the java backend will not be \n' );
    fprintf ( 1, 'tested.  \n' );
    return
end

if ~toolsUI_ok
    fprintf ( 1, '\n' );
    fprintf ( 1, 'Looks like java is enabled, but I cannot find the toolsUI \n' );
    fprintf ( 1, 'jar file on your path.  If you wish to test the java backend, \n' );
    fprintf ( 1, 'you will need to fix your javaclasspath.\n' );
    return
end

pause_duration = 3;
fprintf ( 1, 'Ok, the java setup looks good to go.  ' );
if mexnc_ok
    fprintf ( 1, 'Mexnc will be run on those m-files that cannot use java.\n' );
else
    fprintf ( 1, 'The number of tests is reduced since mexnc cannot be found.\n' );
end
fprintf ( 1, 'Remote OPeNDAP/java tests ' );
if getpref('SNCTOOLS','TEST_REMOTE_JAVA',false)
    fprintf ( 1, 'will ' );
    setpref('SNCTOOLS','TEST_REMOTE',true)
else
    fprintf ( 1, 'will NOT ' );
    setpref('SNCTOOLS','TEST_REMOTE',false)
end
fprintf ( 1, 'be run.\n  Starting tests in ' );
for j = 1:pause_duration
    fprintf ( 1, '%d... ', pause_duration - j + 1 );
    pause(1);
end
fprintf ( 1, '\n' );

run_backend_neutral_tests;

return




%----------------------------------------------------------------------
function run_all_tests()

fprintf ( 1, 'Ok, about to start testing in  ' );
pause_duration = 3;
for j = 1:pause_duration
    fprintf ( 1, '%d... ', pause_duration - j + 1 );
    pause(1);
end
fprintf ( 1, '\n' );

test_nc_attget;
test_nc_datatype_string;
test_nc_iscoordvar;
test_nc_isunlimitedvar;
test_nc_dump;
test_nc_getlast;
test_nc_isvar;
test_nc_varsize;
test_nc_getvarinfo;
test_nc_info;
test_nc_getbuffer;
test_nc_varget;
test_nc_getdiminfo;

test_nc_varput           ( 'test.nc' );
test_nc_add_dimension    ( 'test.nc' );
test_nc_addhist          ( 'test.nc' );
test_nc_addvar           ( 'test.nc' );
test_nc_attput           ( 'test.nc' );
test_nc_create_empty     ( 'test.nc' );
test_nc_varrename        ( 'test.nc' );
test_nc_addnewrecs       ( 'test.nc' );
test_nc_add_recs         ( 'test.nc' );
test_nc_archive_buffer   ( 'test.nc' );

test_snc2mat             ( 'test.nc' );
test_nc_getall           ( 'test.nc' );
test_nc_diff             ( 'test1.nc', 'test2.nc' );
test_nc_cat_a;


return




%----------------------------------------------------------------------
function run_tmw_tests()

% Is use_tmw ok?
tmw_ok = strcmp(version('-release'),'2008b') && getpref('SNCTOOLS','USE_TMW',false);
if ~tmw_ok
    return
end

fprintf ( 1, 'Ok, about to start TMW testing in  ' );
pause_duration = 3;
for j = 1:pause_duration
    fprintf ( 1, '%d... ', pause_duration - j + 1 );
    pause(1);
end
fprintf ( 1, '\n' );

run_backend_neutral_tests;
run_backend_mexnc_tests;

return





%----------------------------------------------------------------------
function cleanup(old_settings)
fprintf ( 1, '\n' );
answer = input ( 'Done with this series of tests. Do you wish to remove all test NetCDF and *.mat files that were created? [y/n]\n', 's' );
if strcmp ( lower(answer), 'y' )
    delete ( '*.nc' );
    delete ( '*.mat' );
end
fprintf ( 1, '\nRestoring old settings...\n' );
setpref('SNCTOOLS','USE_JAVA',old_settings.use_java);
setpref('SNCTOOLS','TEST_REMOTE_MEXNC',old_settings.test_remote_mexnc);
setpref('SNCTOOLS','TEST_REMOTE_JAVA',old_settings.test_remote_java);
rmpref ('SNCTOOLS','TEST_REMOTE');
return




function run_backend_neutral_tests()

test_nc_attget;
test_nc_datatype_string;
test_nc_iscoordvar;
test_nc_isunlimitedvar;
test_nc_dump;
test_nc_getlast;
test_nc_isvar;
test_nc_varsize;
test_nc_getvarinfo;
test_nc_info;
test_nc_getbuffer;
test_nc_varget;
test_nc_getdiminfo;


return




%----------------------------------------------------------------------
function run_backend_mex_tests()

if ~(snctools_use_tmw || snctools_use_mexnc)
	fprintf ( 1, 'Cannot use native netcdf support or mexnc, no tests requiring netcdf output can be run.\n' );	
	return
end

test_nc_varput           ( 'test.nc' );
test_nc_add_dimension    ( 'test.nc' );
test_nc_addhist          ( 'test.nc' );
test_nc_addvar           ( 'test.nc' );
test_nc_attput           ( 'test.nc' );
test_nc_create_empty     ( 'test.nc' );
test_nc_varrename        ( 'test.nc' );
test_nc_addnewrecs       ( 'test.nc' );
test_nc_add_recs         ( 'test.nc' );
test_nc_archive_buffer   ( 'test.nc' );

test_snc2mat             ( 'test.nc' );
test_nc_getall           ( 'test.nc' );
test_nc_diff             ( 'test1.nc', 'test2.nc' );
test_nc_cat_a;



return

