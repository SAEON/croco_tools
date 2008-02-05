/**********************************************************************
 *
 * common.c
 *
 * This file contains routines that are used or are intended for use
 * by the other source files of mexnc.
 *
 *********************************************************************/

# include <ctype.h>
# include <errno.h>
# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# include "netcdf.h"

# include "mex.h"

# include "mexnc.h"




/*
 * mexncCreateDoubleScalar:
 *
 * The netcdf-3 mexnc was originally written on an R13 platform.  
 * Backporting to R11 created a conflict with the mxCreateDoubleScalar
 * routine, which was not available to the R11.  Since the equivalent 
 * code would be cumbersome, it was decided to just stub out all calls
 * to mxCreateDoubleScalar and handle all the differences in just one
 * place.
 * */
mxArray *mexncCreateDoubleScalar ( double value ) {

	mxArray *mx;

#ifdef MEXNC_R11
	/*
	 * number of dimensions in a NetCDF file and their IDs.
	 * */
	int             mx_count_coord[1]; 

	/*
	 * Pointer to data part of the matrix.
	 * */
	double          *pr;

	mx_count_coord[0] = 1;

	mx = mxCreateNumericArray ( 1, mx_count_coord, mxDOUBLE_CLASS, mxREAL );
	pr = mxGetPr ( mx );
	pr[0] = value;

#else

	mx = mxCreateDoubleScalar ( value );

#endif

	return ( mx );

}











void Usage ( ) {
	mexPrintf ( "You must have at least one input argument.  Try\n\n" );
	mexPrintf ( "    >> help mexcdf53\n\n" );
}






/*
 * Mat2Size_t:  
 *
 * Turns values in an mxArray into a size_t array.
 * */
size_t * Mat2Size_t ( const mxArray *mat )
{
	double	*	pr;
	size_t		*	pint;
	size_t		*	p;
	int			len;
	int			i;

	len = mxGetM(mat) * mxGetN(mat);
	
	pint = (size_t *) mxCalloc(len, sizeof(size_t));
	p = pint;
	pr = mxGetPr(mat);
	
	for (i = 0; i < len; i++)	{
		*p++ = (size_t) *pr++;
	}
	
	return (pint);
}









/*
 * Mat2Ptrdiff_t:  
 *
 * Turns values in an mxArray into a ptrdiff_t array.
 * */
ptrdiff_t * Mat2Ptrdiff_t ( const mxArray *mat )
{
	double	*	pr;
	ptrdiff_t		*	pint;
	ptrdiff_t		*	p;
	int			len;
	int			i;

	len = mxGetM(mat) * mxGetN(mat);
	
	pint = (ptrdiff_t *) mxCalloc(len, sizeof(ptrdiff_t));
	p = pint;
	pr = mxGetPr(mat);
	
	for (i = 0; i < len; i++)	{
		*p++ = (size_t) *pr++;
	}
	
	return (pint);
}

