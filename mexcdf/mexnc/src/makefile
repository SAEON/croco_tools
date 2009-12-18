R2008b:
	mex -g -v -f ./mexopts.R2008b.sh -output vanilla_mexnc mexgateway.c netcdf3.c netcdf2.c common.c 
	mv *.mex*  ../private

R2008a:
	mex -g -v -f ./mexopts.R2008a.sh -output vanilla_mexnc mexgateway.c netcdf3.c netcdf2.c common.c 
	mv *.mex*  ../private

R2007b:
	mex -g -v -f ./mexopts.R2007b.sh -output vanilla_mexnc mexgateway.c netcdf2.c netcdf3.c common.c 
	mv *.mex*  ../private

R2007b_opendap:
	mex -g -v -f ./mexopts.R2007b_opendap.sh -output opendap_mexnc mexgateway.c netcdf2.c netcdf3.c common.c 
	mv *.mexmaci ../private

clean:
	rm -f *.o mexnc.mex*
