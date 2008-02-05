/**********************************************************************
 *
 * netcdf3.c
 *
 * This file contains code to handle the mexfile interface to NetCDF-3
 * API calls.
 *
 *********************************************************************/

/*
 * $Id: netcdf3.c 1289 2006-05-05 13:23:03Z johnevans007 $
 * */

# include <ctype.h>
# include <errno.h>
# include <stdio.h>
# include <stdlib.h>
# include <string.h>

# include "netcdf.h"

# include "mex.h"

# include "mexnc.h"



/*
 * These prototypes are specific to handling NetCDF 3 stuff.
 * */
void   handle_nc_abort        ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_close        ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc__create      ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_create       ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_def_dim      ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_def_var      ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_del_att      ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc__enddef      ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_enddef       ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_copy_att     ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_get_att      ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_get_var      ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_get_var1     ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_get_vara     ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_get_vars     ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_get_varm     ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq          ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_ndims    ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_nvars    ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_natts    ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_att      ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_attid    ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_attlen   ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_attname  ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_atttype  ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_dim      ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_dimid    ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_dimlen   ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_dimname  ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_unlimdim ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_var      ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_varid    ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_varname  ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_vartype  ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_varndims ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_vardimid ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_inq_varnatts ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc__open        ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_open         ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_redef        ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_rename_att   ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_rename_dim   ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_rename_var   ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_put_att      ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_put_var      ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_put_var1     ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_put_vara     ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_put_vars     ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_put_varm     ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_set_fill     ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_strerror     ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );
void   handle_nc_sync         ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *opcode );


void 	determine_varm_output_size ( 
		int        ndims, 
		int        num_requested_elements, 
		size_t    *nc_count_coord, 
		ptrdiff_t *nc_stride_coord, 
		ptrdiff_t *nc_imap_coord, 
		int       *result_size ) ;


void handle_netcdf3_api    (
	int             nlhs,
	mxArray        *plhs[],
	int             nrhs,
	const mxArray  *prhs[],
	op             *nc_op         )
{


	/*
	 * Loop index
	 * */
	int     j;

	char	error_message[1000];






	
	switch ( nc_op->opcode )	{
		
		case ABORT:
			handle_nc_abort ( nlhs, plhs, nrhs, prhs, nc_op );
			break;

			
		case CLOSE:
			handle_nc_close ( nlhs, plhs, nrhs, prhs, nc_op );
			break;
			

		case COPY_ATT:  
			handle_nc_copy_att ( nlhs, plhs, nrhs, prhs, nc_op );
			break;


		case _CREATE: 
			handle_nc__create ( nlhs, plhs, nrhs, prhs, nc_op );
			break;
			

		case CREATE: 
			handle_nc_create ( nlhs, plhs, nrhs, prhs, nc_op );
			break;
			

		case DEF_DIM:
			handle_nc_def_dim ( nlhs, plhs, nrhs, prhs, nc_op );
			break;


		case DEF_VAR:
			handle_nc_def_var ( nlhs, plhs, nrhs, prhs, nc_op );
			break;


		case DEL_ATT:
			handle_nc_del_att ( nlhs, plhs, nrhs, prhs, nc_op );
			break;


		case _ENDDEF:
			handle_nc__enddef ( nlhs, plhs, nrhs, prhs, nc_op );
			break;
			

		case END_DEF:
		case ENDDEF:
		case ENDEF:
			handle_nc_enddef ( nlhs, plhs, nrhs, prhs, nc_op );
			break;
			

		case GET_ATT_DOUBLE:
		case GET_ATT_FLOAT:
		case GET_ATT_INT:
		case GET_ATT_SHORT:
		case GET_ATT_SCHAR:
		case GET_ATT_UCHAR:
		case GET_ATT_TEXT:
			handle_nc_get_att ( nlhs, plhs, nrhs, prhs, nc_op );
			break;


		case GET_VAR_DOUBLE:
		case GET_VAR_FLOAT:
		case GET_VAR_INT:
		case GET_VAR_SHORT:
		case GET_VAR_SCHAR:
		case GET_VAR_UCHAR:
		case GET_VAR_TEXT:
			handle_nc_get_var ( nlhs, plhs, nrhs, prhs, nc_op );
			break;


		case GET_VAR1_DOUBLE:
		case GET_VAR1_FLOAT:
		case GET_VAR1_INT:
		case GET_VAR1_SHORT:
		case GET_VAR1_SCHAR:
		case GET_VAR1_UCHAR:
		case GET_VAR1_TEXT:
			handle_nc_get_var1 ( nlhs, plhs, nrhs, prhs, nc_op );
			break;


		case GET_VARA_DOUBLE:
		case GET_VARA_FLOAT:
		case GET_VARA_INT:
		case GET_VARA_SHORT:
		case GET_VARA_SCHAR:
		case GET_VARA_UCHAR:
		case GET_VARA_TEXT:
			handle_nc_get_vara ( nlhs, plhs, nrhs, prhs, nc_op );
			break;


		case GET_VARS_DOUBLE:
		case GET_VARS_FLOAT:
		case GET_VARS_INT:
		case GET_VARS_SHORT:
		case GET_VARS_SCHAR:
		case GET_VARS_UCHAR:
		case GET_VARS_TEXT:
			handle_nc_get_vars ( nlhs, plhs, nrhs, prhs, nc_op );
			break;


		case GET_VARM_DOUBLE:
		case GET_VARM_FLOAT:
		case GET_VARM_INT:
		case GET_VARM_SHORT:
		case GET_VARM_SCHAR:
		case GET_VARM_UCHAR:
		case GET_VARM_TEXT:
			handle_nc_get_varm ( nlhs, plhs, nrhs, prhs, nc_op );
			break;


		case INQ: 
			handle_nc_inq ( nlhs, plhs, nrhs, prhs, nc_op );
			break;


		case INQ_NDIMS: 
			handle_nc_inq_ndims ( nlhs, plhs, nrhs, prhs, nc_op );
			break;


		case INQ_NVARS: 
			handle_nc_inq_nvars ( nlhs, plhs, nrhs, prhs, nc_op );
			break;


		case INQ_NATTS: 
			handle_nc_inq_natts ( nlhs, plhs, nrhs, prhs, nc_op );
			break;


		case INQ_ATT:
			handle_nc_inq_att ( nlhs, plhs, nrhs, prhs, nc_op );
			break;


		case INQ_ATTID: 
			handle_nc_inq_attid ( nlhs, plhs, nrhs, prhs, nc_op ); 
			break;


		case INQ_ATTLEN: 
			handle_nc_inq_attlen ( nlhs, plhs, nrhs, prhs, nc_op ); 
			break;


		case INQ_ATTNAME: 
			handle_nc_inq_attname ( nlhs, plhs, nrhs, prhs, nc_op );
			break;


		case INQ_ATTTYPE: 
			handle_nc_inq_atttype ( nlhs, plhs, nrhs, prhs, nc_op );
			break;


		case INQ_DIM:
			handle_nc_inq_dim ( nlhs, plhs, nrhs, prhs, nc_op );
			break;
			

		case INQ_DIMID:
			handle_nc_inq_dimid ( nlhs, plhs, nrhs, prhs, nc_op );
			break;
		

		case INQ_DIMLEN:
			handle_nc_inq_dimlen ( nlhs, plhs, nrhs, prhs, nc_op );
			break;
			

		case INQ_DIMNAME:
			handle_nc_inq_dimname ( nlhs, plhs, nrhs, prhs, nc_op );
			break;
			

		case INQ_LIBVERS:
			plhs[0] = mxCreateString ( nc_inq_libvers() );
			break;


		case INQ_VAR:
			handle_nc_inq_var ( nlhs, plhs, nrhs, prhs, nc_op );
			break;
		

		case INQ_VARNAME:
			handle_nc_inq_varname ( nlhs, plhs, nrhs, prhs, nc_op );
			break;
		

		case INQ_VARTYPE:
			handle_nc_inq_vartype ( nlhs, plhs, nrhs, prhs, nc_op );
			break;
		

		case INQ_VARNDIMS:
			handle_nc_inq_varndims ( nlhs, plhs, nrhs, prhs, nc_op );
			break;

		
		case INQ_VARDIMID:
			handle_nc_inq_vardimid ( nlhs, plhs, nrhs, prhs, nc_op );
			break;


		case INQ_VARNATTS:
			handle_nc_inq_varnatts ( nlhs, plhs, nrhs, prhs, nc_op );
			break;
		

		case INQ_UNLIMDIM: 
			handle_nc_inq_unlimdim ( nlhs, plhs, nrhs, prhs, nc_op );
			break;
		

		case INQ_VARID:
			handle_nc_inq_varid ( nlhs, plhs, nrhs, prhs, nc_op );
			break;
		

		case _OPEN: 
			handle_nc__open ( nlhs, plhs, nrhs, prhs, nc_op );
			break;
			

		case OPEN: 
			handle_nc_open ( nlhs, plhs, nrhs, prhs, nc_op );
			break;
			

		case PUT_ATT_DOUBLE:
		case PUT_ATT_FLOAT:
		case PUT_ATT_INT:
		case PUT_ATT_SHORT:
		case PUT_ATT_SCHAR:
		case PUT_ATT_UCHAR:
		case PUT_ATT_TEXT:
			handle_nc_put_att ( nlhs, plhs, nrhs, prhs, nc_op );
			break;


		case PUT_VAR_DOUBLE:
		case PUT_VAR_FLOAT:
		case PUT_VAR_INT:
		case PUT_VAR_SHORT:
		case PUT_VAR_SCHAR:
		case PUT_VAR_UCHAR:
		case PUT_VAR_TEXT:
			handle_nc_put_var ( nlhs, plhs, nrhs, prhs, nc_op );
			break;


		case PUT_VARA_DOUBLE:
		case PUT_VARA_FLOAT:
		case PUT_VARA_INT:
		case PUT_VARA_SHORT:
		case PUT_VARA_SCHAR:
		case PUT_VARA_UCHAR:
		case PUT_VARA_TEXT:
			handle_nc_put_vara ( nlhs, plhs, nrhs, prhs, nc_op );
			break;


		case PUT_VARS_DOUBLE:
		case PUT_VARS_FLOAT:
		case PUT_VARS_INT:
		case PUT_VARS_SHORT:
		case PUT_VARS_SCHAR:
		case PUT_VARS_UCHAR:
		case PUT_VARS_TEXT:
			handle_nc_put_vars ( nlhs, plhs, nrhs, prhs, nc_op );
			break;


		case PUT_VARM_DOUBLE:
		case PUT_VARM_FLOAT:
		case PUT_VARM_INT:
		case PUT_VARM_SHORT:
		case PUT_VARM_SCHAR:
		case PUT_VARM_UCHAR:
		case PUT_VARM_TEXT:
			handle_nc_put_varm ( nlhs, plhs, nrhs, prhs, nc_op );
			break;


		case PUT_VAR1_DOUBLE:
		case PUT_VAR1_FLOAT:
		case PUT_VAR1_INT:
		case PUT_VAR1_SHORT:
		case PUT_VAR1_SCHAR:
		case PUT_VAR1_UCHAR:
		case PUT_VAR1_TEXT:
			handle_nc_put_var1 ( nlhs, plhs, nrhs, prhs, nc_op );
			break;


		case REDEF:
			handle_nc_redef ( nlhs, plhs, nrhs, prhs, nc_op );
			break;
			

		case RENAME_ATT: 
			handle_nc_rename_att ( nlhs, plhs, nrhs, prhs, nc_op );
			break;
			

		case RENAME_DIM:
			handle_nc_rename_dim ( nlhs, plhs, nrhs, prhs, nc_op );
			break;
			

		case RENAME_VAR:
			handle_nc_rename_var ( nlhs, plhs, nrhs, prhs, nc_op );
			break;
			

		case SET_FILL: 
			handle_nc_set_fill ( nlhs, plhs, nrhs, prhs, nc_op );
			break;

			
		case STRERROR:
			handle_nc_strerror ( nlhs, plhs, nrhs, prhs, nc_op );
			break;

			
		case SYNC:
			handle_nc_sync ( nlhs, plhs, nrhs, prhs, nc_op );
			break;
		
			
			
		default:
		
			sprintf ( error_message, "unhandled opcode %d, %s, line %d, file %s\n", nc_op->opcode, nc_op->opname, __LINE__, __FILE__ );
			mexPrintf ( error_message );
			for ( j = 0; j < nlhs; ++j ) {
				plhs[j] = mexncCreateDoubleScalar ( mxGetNaN () );
			}
			break;
	}
	
	return;
}













/*
 * HANDLE_GET_VARA:
 *
 * code for handling the nc_get_vara_xxx family of function calls.
 * */
void handle_nc_get_vara ( 
        int            nlhs, 
        mxArray       *plhs[], 
        int            nrhs, 
        const mxArray *prhs[], 
        op            *the_op ) 
{

	/*
	 * Constitutes the position index where the data write is to begin.
	 * */
	size_t		  *nc_start_coord;
	size_t		  *nc_count_coord;
	
	int			  mx_count_coord[MAX_NC_DIMS];

	/*
	 * NetCDF File ID
	 * */
	int	  ncid;
	
	/*
	 * NetCDF variable ID
	 * */
	int	  varid;

	/*
	 * Short cut pointer to matlab array data.
	 * */
	double  *pr;

	/*
	 * Generic space for character data.
	 * */
	char   *char_buffer;

	/*
	 * Return status from netcdf operation.
	 * */
	int	  status;

	char	error_message[1000];

	/*
	 * number of dimensions in a NetCDF file and their IDs.
	 * */
	int			 ndims;
	int			 dimids[NC_MAX_DIMS]; 

	/*
	 * number of elements in a variable hyperslab
	 * */
	int			 varsize;

	/*
	 * loop index
	 * */
	int			  j;

	/*
	 * Generic ptr for accessing mxChar data.
	 * */
	mxChar *mxchar_ptr;

	/*
	 * Generic ptr for accessing char data.
	 * */
	char *char_ptr;

	OPCODE  opcode = the_op->opcode;
	char   *opname = the_op->opname;  



	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
		sprintf ( error_message, "ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", opname, __LINE__, __FILE__ );
		mexErrMsgTxt ( error_message );
		return;
	}



	if ( mxIsDouble(prhs[2]) == false ) {
		sprintf ( error_message, "varid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", opname, __LINE__, __FILE__ );
		mexErrMsgTxt ( error_message );
		return;
	}



	if ( mxIsDouble(prhs[3]) == false ) {
		sprintf ( error_message, "start coord index argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", opname, __LINE__, __FILE__ );
		mexErrMsgTxt ( error_message );
		return;
	}



	if ( mxIsDouble(prhs[4]) == false ) {
		sprintf ( error_message, "count coord index argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", opname, __LINE__, __FILE__ );
		mexErrMsgTxt ( error_message );
		return;
	}




	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	varid = (int)(pr[0]);
	nc_start_coord = Mat2Size_t ( prhs[3] );
	nc_count_coord = Mat2Size_t ( prhs[4] );

	/*
	 * Get the dimensions that define the variable.
	 * */
	status = nc_inq_var ( ncid, varid, NULL, NULL, &ndims, dimids, NULL );
	if ( status != NC_NOERR ) {
		sprintf ( error_message, "nc_inq_var failed, line %d, file %s\n", __LINE__, __FILE__ );
		mexErrMsgTxt ( error_message );
	}


	/*
	 * Figure out how much space.
	 * */
	varsize = 1;
	for ( j = 0; j < ndims; ++j ) {
		varsize *= nc_count_coord[j];

		/*
		 * We need to set the dimensions of the matrix as the reverse
		 * of how it is defined by the user.  This makes the
		 * size of the matrix seem transposed (upon return to matlab), 
		 * but otherwise the data gets layed out incorrectly due to 
		 * the difference between row-major order (C) and column-major 
		 * order (matlab).
		 * */
		mx_count_coord[ndims - j - 1] = nc_count_coord[j];
	}

	switch ( opcode ) {

		case GET_VARA_DOUBLE:
			plhs[0] = mxCreateNumericArray ( ndims, mx_count_coord, mxDOUBLE_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_vara_double ( ncid, varid, nc_start_coord, nc_count_coord, (double *)pr );
			break;

		case GET_VARA_FLOAT:
			plhs[0] = mxCreateNumericArray ( ndims, mx_count_coord, mxSINGLE_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_vara_float ( ncid, varid, nc_start_coord, nc_count_coord, (float *)pr );
			break;

		case GET_VARA_INT:
			plhs[0] = mxCreateNumericArray ( ndims, mx_count_coord, mxINT32_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_vara_int ( ncid, varid, nc_start_coord, nc_count_coord, (int *)pr );
			break;

		case GET_VARA_SHORT:
			plhs[0] = mxCreateNumericArray ( ndims, mx_count_coord, mxINT16_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_vara_short ( ncid, varid, nc_start_coord, nc_count_coord, (short int *)pr );
			break;

		case GET_VARA_SCHAR:
			plhs[0] = mxCreateNumericArray ( ndims, mx_count_coord, mxINT8_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_vara_schar ( ncid, varid, nc_start_coord, nc_count_coord, (signed char *)pr );
			break;

		case GET_VARA_UCHAR:
			plhs[0] = mxCreateNumericArray ( ndims, mx_count_coord, mxUINT8_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_vara_uchar ( ncid, varid, nc_start_coord, nc_count_coord, (unsigned char *)pr );
			break;

		case GET_VARA_TEXT:
			plhs[0] = mxCreateCharArray ( ndims, mx_count_coord );
			pr = mxGetPr ( plhs[0] );

			char_buffer = mxCalloc ( varsize + 1, sizeof(mxChar) );
			status = nc_get_vara_text ( ncid, varid, nc_start_coord, nc_count_coord, char_buffer );

			/*
			 * Copy them into the mxChar array one char at a time.
			 * We apparently need to do it this way because there is
			 * a mismatch between the datatypes char and mxChar.
			 * */
			char_ptr = char_buffer;
			mxchar_ptr = (mxChar *)pr;
			for ( j = 0; j < varsize; ++j ) {
				mxchar_ptr[j] = (mxChar) ( char_ptr[j] );
			}

			break;

		default:
			sprintf ( error_message, "unhandled opcode %d, %s, line %d file %s\n", opcode, opname, __LINE__, __FILE__ );
			mexErrMsgTxt ( error_message );

	}
	plhs[1] = mexncCreateDoubleScalar ( status );


	return;

}









/*
 * HANDLE_NC_GET_VARS:
 *
 * code for handling the nc_get_vars_xxx family of function calls.
 * */
void handle_nc_get_vars ( 
        int            nlhs, 
        mxArray       *plhs[], 
        int            nrhs, 
        const mxArray *prhs[], 
        op            *the_op ) 
{

	/*
	 * Constitutes the position index where the data write is to begin.
	 * */
	size_t		  *nc_start_coord;
	size_t		  *nc_count_coord;
	/*
	size_t		  *nc_stride_coord;
	*/
	ptrdiff_t		  *nc_stride_coord;
	
	int			  mx_count_coord[MAX_NC_DIMS];

	/*
	 * NetCDF File ID
	 * */
	int	  ncid;
	
	/*
	 * NetCDF variable ID
	 * */
	int	  varid;

	/*
	 * Short cut pointer to matlab array data.
	 * */
	double  *pr;

	/*
	 * Generic space for character data.
	 * */
	char   *char_buffer;

	/*
	 * Return status from netcdf operation.
	 * */
	int	  status;

	char	error_message[1000];

	/*
	 * number of dimensions in a NetCDF file and their IDs.
	 * */
	int			 ndims;
	int			 dimids[NC_MAX_DIMS]; 

	/*
	 * number of elements in a variable hyperslab
	 * */
	int			 varsize;

	/*
	 * loop index
	 * */
	int			  j;

	/*
	 * Generic ptr for accessing mxChar data.
	 * */
	mxChar *mxchar_ptr;

	/*
	 * Generic ptr for accessing char data.
	 * */
	char *char_ptr;



	OPCODE  opcode = the_op->opcode;
	char   *opname = the_op->opname;  


	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
		sprintf ( error_message, "ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", opname, __LINE__, __FILE__ );
		mexErrMsgTxt ( error_message );
		return;
	}



	if ( mxIsDouble(prhs[2]) == false ) {
		sprintf ( error_message, "varid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", opname, __LINE__, __FILE__ );
		mexErrMsgTxt ( error_message );
		return;
	}



	if ( mxIsDouble(prhs[3]) == false ) {
		sprintf ( error_message, "index argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", opname, __LINE__, __FILE__ );
		mexErrMsgTxt ( error_message );
		return;
	}



	if ( mxIsDouble(prhs[4]) == false ) {
		sprintf ( error_message, "count argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", opname, __LINE__, __FILE__ );
		mexErrMsgTxt ( error_message );
		return;
	}



	if ( mxIsDouble(prhs[5]) == false ) {
		sprintf ( error_message, "stride argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", opname, __LINE__, __FILE__ );
		mexErrMsgTxt ( error_message );
		return;
	}



	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	varid = (int)(pr[0]);
	nc_start_coord = Mat2Size_t ( prhs[3] );
	nc_count_coord = Mat2Size_t ( prhs[4] );
	nc_stride_coord = Mat2Ptrdiff_t ( prhs[5] );


	/*
	 * Get the dimensions that define the variable.
	 * */
	status = nc_inq_var ( ncid, varid, NULL, NULL, &ndims, dimids, NULL );
	if ( status != NC_NOERR ) {
		sprintf ( error_message, "nc_inq_var failed, line %d, file %s\n", __LINE__, __FILE__ );
		mexErrMsgTxt ( error_message );
	}


	/*
	 * Figure out how much space.
	 * */
	varsize = 1;
	for ( j = 0; j < ndims; ++j ) {
		varsize *= nc_count_coord[j];

		/*
		 * We need to set the dimensions of the matrix as the reverse
		 * of how it is defined by the user.  This makes the
		 * size of the matrix seem transposed (upon return to matlab), 
		 * but otherwise the data gets layed out incorrectly due to 
		 * the difference between row-major order (C) and column-major 
		 * order (matlab).
		 * */
		mx_count_coord[ndims - j - 1] = nc_count_coord[j];
	}

	switch ( opcode ) {

		case GET_VARS_DOUBLE:
			plhs[0] = mxCreateNumericArray ( ndims, mx_count_coord, mxDOUBLE_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_vars_double ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, (double *)pr );
			break;

		case GET_VARS_FLOAT:
			plhs[0] = mxCreateNumericArray ( ndims, mx_count_coord, mxSINGLE_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_vars_float ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, (float *)pr );
			break;

		case GET_VARS_INT:
			plhs[0] = mxCreateNumericArray ( ndims, mx_count_coord, mxINT32_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_vars_int ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, (int *)pr );
			break;

		case GET_VARS_SHORT:
			plhs[0] = mxCreateNumericArray ( ndims, mx_count_coord, mxINT16_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_vars_short ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, (short int *)pr );
			break;

		case GET_VARS_SCHAR:
			plhs[0] = mxCreateNumericArray ( ndims, mx_count_coord, mxINT8_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_vars_schar ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, (signed char *)pr );
			break;

		case GET_VARS_UCHAR:
			plhs[0] = mxCreateNumericArray ( ndims, mx_count_coord, mxUINT8_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_vars_uchar ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, (unsigned char *)pr );
			break;

		case GET_VARS_TEXT:
			plhs[0] = mxCreateCharArray ( ndims, mx_count_coord );
			pr = mxGetPr ( plhs[0] );

			char_buffer = mxCalloc ( varsize + 1, sizeof(mxChar) );
			status = nc_get_vars_text ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, char_buffer );

			/*
			 * Copy them into the mxChar array one char at a time.
			 * We apparently need to do it this way because there is
			 * a mismatch between the datatypes char and mxChar.
			 * */
			char_ptr = char_buffer;
			mxchar_ptr = (mxChar *)pr;
			for ( j = 0; j < varsize; ++j ) {
				mxchar_ptr[j] = (mxChar) ( char_ptr[j] );
			}

			break;

		default:
			sprintf ( error_message, "unhandled opcode %d, %s, line %d file %s\n", opcode, opname, __LINE__, __FILE__ );
			mexErrMsgTxt ( error_message );

	}
	plhs[1] = mexncCreateDoubleScalar ( status );


	return;

}







/*
 * HANDLE_NC_GET_VARM:
 *
 * code for handling the nc_get_varm_xxx family of function calls.
 * */
void handle_nc_get_varm ( 
        int            nlhs, 
        mxArray       *plhs[], 
        int            nrhs, 
        const mxArray *prhs[], 
        op            *the_op ) 
{

	/*
	 * Constitutes the position index where the data write is to begin.
	 * */
	size_t		  *nc_start_coord;
	size_t		  *nc_count_coord;
	ptrdiff_t	   *nc_stride_coord;
	ptrdiff_t	   *nc_imap_coord;
	
	/*
	 * We have to determine ourselves how big the resulting matrix will be.
	 * */
	int			  result_size[MAX_NC_DIMS];

	/*
	 * This is actually used by matlab to create the matrix.  It's different
	 * than result_size because of the row-major-order-column-major-order issue.
	 * */
	int			  mx_result_size[MAX_NC_DIMS];

	/*
	 * NetCDF File ID
	 * */
	int	  ncid;
	
	/*
	 * NetCDF variable ID
	 * */
	int	  varid;

	/*
	 * Short cut pointer to matlab array data.
	 * */
	double  *pr;


	/*
	 * Generic space for character data.
	 * */
	char   *char_buffer;

	/*
	 * Return status from netcdf operation.
	 * */
	int	  status;

	char	error_message[1000];

	/*
	 * number of dimensions in a NetCDF file and their IDs.
	 * */
	int			 ndims;
	int			 dimids[NC_MAX_DIMS]; 
	size_t		  netcdf_dimension_sizes[NC_MAX_DIMS]; 

	/*
	 * number of elements in a variable hyperslab
	 * */
	int			 num_requested_elements;
	int			 total_num_elements;

	/*
	 * loop index
	 * */
	int			  j;

	/*
	 * Generic ptr for accessing mxChar data.
	 * */
	mxChar *mxchar_ptr;

	/*
	 * Generic ptr for accessing char data.
	 * */
	char *char_ptr;


	/*
	 * Since this function is so hard to figure out, lots of debugging
	 * code is left in place.  Activate it by turning this switch to 1.
	 * */
	int verbose = 0;

	OPCODE  opcode = the_op->opcode;
	char   *opname = the_op->opname;  


	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	varid = (int)(pr[0]);

	nc_start_coord = Mat2Size_t ( prhs[3] );
	nc_count_coord = Mat2Size_t ( prhs[4] );
	nc_stride_coord = Mat2Ptrdiff_t ( prhs[5] );
	nc_imap_coord = Mat2Ptrdiff_t ( prhs[6] );


	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
		sprintf ( error_message, "ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", opname, __LINE__, __FILE__ );
		mexErrMsgTxt ( error_message );
		return;
	}



	if ( mxIsDouble(prhs[2]) == false ) {
		sprintf ( error_message, "varid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", opname, __LINE__, __FILE__ );
		mexErrMsgTxt ( error_message );
		return;
	}



	if ( mxIsDouble(prhs[3]) == false ) {
		sprintf ( error_message, "index argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", opname, __LINE__, __FILE__ );
		mexErrMsgTxt ( error_message );
		return;
	}



	if ( mxIsDouble(prhs[4]) == false ) {
		sprintf ( error_message, "count argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", opname, __LINE__, __FILE__ );
		mexErrMsgTxt ( error_message );
		return;
	}



	if ( mxIsDouble(prhs[5]) == false ) {
		sprintf ( error_message, "stride argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", opname, __LINE__, __FILE__ );
		mexErrMsgTxt ( error_message );
		return;
	}



	if ( mxIsDouble(prhs[6]) == false ) {
		sprintf ( error_message, "imap argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", opname, __LINE__, __FILE__ );
		mexErrMsgTxt ( error_message );
		return;
	}



	/*
	 * Get the dimensions that define the variable.
	 * */
	status = nc_inq_var ( ncid, varid, NULL, NULL, &ndims, dimids, NULL );
	if ( status != NC_NOERR ) {
		sprintf ( error_message, "nc_inq_var failed, line %d, file %s\n", __LINE__, __FILE__ );
		mexErrMsgTxt ( error_message );
	}

	/*
	 * Check that the lengths of the input "start", "count", "stride", and "imap" 
	 * matrices match the rank of the netcdf variable.
	 */
	for ( j = 3; j < 7; ++j ) {

		/*
		 * number of rows, columns of "start", "count", "stride", and "imap" coord arrays.
		 * */
		int m, n;

		/*
		 * length of "start", "count", "stride", and "imap" coord arrays
		 * */
		int vector_length;

		/*
		 * Check that the input coordinates are 1-D arrays.
		 */
		m = mxGetM ( prhs[j] );
		n = mxGetN ( prhs[j] );
		if ( ( m == 1 ) || ( n == 1 ) ) {
			;	
		} else {
			sprintf ( error_message, "input array %d must be a 1D vector, not %dx%d\n", j, m, n );
			mexErrMsgTxt ( error_message );
		}



		/*
		 * Check that the vector length is ok.  Must be the same as netcdf variable rank.
		 */
		vector_length = m*n;

		if ( vector_length != ndims ) {
			sprintf ( error_message, "input array %d  must have a length equal to the netcdf variable rank, %d\n", j, ndims );
			mexErrMsgTxt ( error_message );
		}

	}






	/*
	 * Get the size of each dimension.  
	 * */
	for ( j = 0; j < ndims; ++j ) {

		status = nc_inq_dimlen ( ncid, dimids[j], &netcdf_dimension_sizes[j] );
		if ( status != NC_NOERR ) {
			sprintf ( error_message, "nc_inq_dimlen failed on dimid %d \n", dimids[j] );
			mexErrMsgTxt ( error_message );
		}


	}


	/*
	 * Figure out the total number of elements the user thinks they
	 * are asking for.  Also compute the total number of elements.
	 * */
	num_requested_elements = 1;
	total_num_elements = 1;
	for ( j = 0; j < ndims; ++j ) {
		num_requested_elements *= nc_count_coord[j];
		total_num_elements *= netcdf_dimension_sizes[j];
	}


	determine_varm_output_size ( ndims, num_requested_elements, 
			nc_count_coord, nc_stride_coord, nc_imap_coord, result_size );




	/*
	 * We need to set the dimensions of the matrix as the reverse
	 * of how it is defined by the user.  This makes the
	 * size of the matrix seem transposed (upon return to matlab), 
	 * but otherwise the data gets layed out incorrectly due to 
	 * the difference between row-major order (C) and column-major 
	 * order (matlab).
	 * */
	for ( j = 0; j < ndims; ++j ) {
		mx_result_size[ndims - j - 1] = result_size[j];
	}

	if ( verbose ) {
			mexPrintf ( "start " );
			for ( j = 0; j < ndims; ++j ) {
				printf ( "[%d]", nc_start_coord[j] );
			}
			mexPrintf ( "\n\n" );
	
	
			mexPrintf ( "count " );
			for ( j = 0; j < ndims; ++j ) {
				printf ( "[%d]", nc_count_coord[j] );
			}
			mexPrintf ( "\n\n" );
	
	
			mexPrintf ( "stride " );
			for ( j = 0; j < ndims; ++j ) {
				printf ( "[%d]", nc_stride_coord[j] );
			}
			mexPrintf ( "\n\n" );
	
	
			mexPrintf ( "imap " );
			for ( j = 0; j < ndims; ++j ) {
				printf ( "[%d]", nc_imap_coord[j] );
			}
			mexPrintf ( "\n\n" );
	
			mexPrintf ( "result_size " );
			for ( j = 0; j < ndims; ++j ) {
				printf ( "[%d]", result_size[j] );
			}
			mexPrintf ( "\n\n" );
	
			mexPrintf ( "mx_result_size " );
			for ( j = 0; j < ndims; ++j ) {
				printf ( "[%d]", mx_result_size[j] );
			}
			mexPrintf ( "\n\n" );
	}

	/*
	 * Check that no elements in mx_result_size are non-positive.
	 * That can crash matlab hard.
	 * */
	for ( j = 0; j < ndims; ++j ) {
		if  ( mx_result_size[j] < 1 ) {

			sprintf ( error_message, "Requested data extent is invalid.\n\n" );
			mexPrintf ( error_message );

			mexPrintf ( "start " );
			for ( j = 0; j < ndims; ++j ) {
				printf ( "[%d]", nc_start_coord[j] );
			}
			mexPrintf ( "\n\n" );
	
	
			mexPrintf ( "count " );
			for ( j = 0; j < ndims; ++j ) {
				printf ( "[%d]", nc_count_coord[j] );
			}
			mexPrintf ( "\n\n" );
	
	
			mexPrintf ( "stride " );
			for ( j = 0; j < ndims; ++j ) {
				printf ( "[%d]", nc_stride_coord[j] );
			}
			mexPrintf ( "\n\n" );
	
	
			mexPrintf ( "imap " );
			for ( j = 0; j < ndims; ++j ) {
				printf ( "[%d]", nc_imap_coord[j] );
			}
			mexPrintf ( "\n\n" );
	
			mexPrintf ( "result_size " );
			for ( j = 0; j < ndims; ++j ) {
				printf ( "[%d]", result_size[j] );
			}
			mexPrintf ( "\n\n" );
	
			mexPrintf ( "mx_result_size " );
			for ( j = 0; j < ndims; ++j ) {
				printf ( "[%d]", mx_result_size[j] );
			}
			mexErrMsgTxt ( "\n\n" );

		}
	}


	switch ( opcode ) {

		case GET_VARM_DOUBLE:
			plhs[0] = mxCreateNumericArray ( ndims, mx_result_size, mxDOUBLE_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_varm_double ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, (double *)pr );
			break;

		case GET_VARM_FLOAT:
			plhs[0] = mxCreateNumericArray ( ndims, mx_result_size, mxSINGLE_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_varm_float ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, (float *)pr );
			break;

		case GET_VARM_INT:
			plhs[0] = mxCreateNumericArray ( ndims, mx_result_size, mxINT32_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_varm_int ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, (int *)pr );
			break;

		case GET_VARM_SHORT:
			plhs[0] = mxCreateNumericArray ( ndims, mx_result_size, mxINT16_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_varm_short ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, (short int *)pr );
			break;

		case GET_VARM_SCHAR:
			plhs[0] = mxCreateNumericArray ( ndims, mx_result_size, mxINT8_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_varm_schar ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, (signed char *)pr );
			break;

		case GET_VARM_UCHAR:
			plhs[0] = mxCreateNumericArray ( ndims, mx_result_size, mxUINT8_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_varm_uchar ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, (unsigned char *)pr );
			break;

		case GET_VARM_TEXT:
			plhs[0] = mxCreateCharArray ( ndims, mx_result_size );
			pr = mxGetPr ( plhs[0] );

			char_buffer = mxCalloc ( num_requested_elements + 1, sizeof(mxChar) );
			status = nc_get_varm_text ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, char_buffer );

			/*
			 * Copy them into the mxChar array one char at a time.
			 * We apparently need to do it this way because there is
			 * a mismatch between the datatypes char and mxChar.
			 * */
			char_ptr = char_buffer;
			mxchar_ptr = (mxChar *)pr;
			for ( j = 0; j < num_requested_elements; ++j ) {
				mxchar_ptr[j] = (mxChar) ( char_ptr[j] );
			}

			break;

		default:
			sprintf ( error_message, "unhandled opcode %d, %s, line %d file %s\n", opcode, opname, __LINE__, __FILE__ );
			mexErrMsgTxt ( error_message );

	}
	plhs[1] = mexncCreateDoubleScalar ( status );


	return;

}










/*
 * HANDLE_NC_PUT_ATT:
 *
 * code for handling the nc_put_att_xxx family of function calls.
 * */
void handle_nc_put_att ( 
        int            nlhs, 
        mxArray       *plhs[], 
        int            nrhs, 
        const mxArray *prhs[], 
        op            *the_op ) 
{

    /*
     * NetCDF File ID
     * */
    int      ncid;
    
    /*
     * NetCDF variable ID
     * */
    int      varid;

    /*
     * Short cut pointer to matlab array data.
     * */
    double  *pr;

    /*
     * This points to actual attribute data.
     * */
    double  *attribute_value;

    /*
     * Generic space for character data.
     * */
    char   *char_buffer;

    /*
     * NCTYPE of the attribute
     * */
    int datatype;


    /*
     * length of the attribute
     * */
    size_t att_len;

    /*
     * Return status from netcdf operation.
     * */
    int      status;

    /*
     * name of attribute
     * */
    char     *name;

    char    error_message[1000];

    OPCODE  opcode = the_op->opcode;
    char   *opname = the_op->opname;  




    pr = mxGetPr ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetPr ( prhs[2] );
    varid = (int)(pr[0]);
    name = Mat2Str(prhs[3]);
    datatype = Parameter ( prhs[4] );
    pr = mxGetPr ( prhs[5] );
    att_len = (size_t)(pr[0]);
    attribute_value = mxGetPr ( prhs[6] );


    switch ( opcode ) {

        case PUT_ATT_DOUBLE:
            status = nc_put_att_double ( ncid, varid, name, datatype, att_len, attribute_value );
            break;

        case PUT_ATT_FLOAT:
            status = nc_put_att_float ( ncid, varid, name, datatype, att_len, (float *)attribute_value );
            break;

        case PUT_ATT_INT:
            status = nc_put_att_int ( ncid, varid, name, datatype, att_len, (int *)attribute_value );
            break;

        case PUT_ATT_SHORT:
            status = nc_put_att_short ( ncid, varid, name, datatype, att_len, (short int *)attribute_value );
            break;

        case PUT_ATT_SCHAR:
            status = nc_put_att_schar ( ncid, varid, name, datatype, att_len, (signed char *)attribute_value );
            break;

        case PUT_ATT_UCHAR:
            status = nc_put_att_uchar ( ncid, varid, name, datatype, att_len, (unsigned char *)attribute_value );
            break;

        case PUT_ATT_TEXT:
            char_buffer = mxArrayToString ( prhs[6] );
            status = nc_put_att_text ( ncid, varid, name, att_len, char_buffer );
            mxFree ( char_buffer );
            break;

        default:
            sprintf ( error_message, "unhandled opcode %d, %s, line %d file %s\n", opcode, opname, __LINE__, __FILE__ );
            mexErrMsgTxt ( error_message );
            return;
            
    }
    plhs[0] = mexncCreateDoubleScalar ( status );

    return;
}







/*
 * HANDLE_NC_PUT_VAR:
 *
 * code for handling the nc_put_var_xxx family of function calls.
 * */
void handle_nc_put_var ( 
        int            nlhs, 
        mxArray       *plhs[], 
        int            nrhs, 
        const mxArray *prhs[], 
	op            *nc_op ) 
{

	/*
	 * NetCDF File ID
	 * */
	int	  ncid;
	
	/*
	 * NetCDF variable ID
	 * */
	int	  varid;

	/*
	 * Short cut pointer to matlab array data.
	 * */
	double  *pr;

	/*
	 * This points to actual matrix data.
	 * */
	double  *data_buffer;

	/*
	 * Generic space for character data.
	 * */
	char   *char_buffer;

	/*
	 * Return status from netcdf operation.
	 * */
	int	  status;


	char	error_message[1000];

	OPCODE  opcode = nc_op->opcode;
	char   *opname = nc_op->opname;  



	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
		sprintf ( error_message, 
			"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
			nc_op->opname, __LINE__, __FILE__ );
		mexErrMsgTxt ( error_message );
		return;
	}
	
	if ( mxIsDouble(prhs[2]) == false ) {
		sprintf ( error_message, 
			"varid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
			nc_op->opname, __LINE__, __FILE__ );
		mexErrMsgTxt ( error_message );
		return;
	}
	
	
	



	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	varid = (int)(pr[0]);
	data_buffer = mxGetPr ( prhs[3] );

	switch ( opcode ) {

		case PUT_VAR_DOUBLE:
			status = nc_put_var_double ( ncid, varid, data_buffer );
			break;

		case PUT_VAR_FLOAT:
			status = nc_put_var_float ( ncid, varid, (float *)data_buffer );
			break;

		case PUT_VAR_INT:
			status = nc_put_var_int ( ncid, varid, (int *)data_buffer );
			break;

		case PUT_VAR_SHORT:
			status = nc_put_var_short ( ncid, varid, (short int *)data_buffer );
			break;

		case PUT_VAR_SCHAR:
			status = nc_put_var_schar ( ncid, varid, (signed char *)data_buffer );
			break;

		case PUT_VAR_UCHAR:
			status = nc_put_var_uchar ( ncid, varid, (unsigned char *)data_buffer );
			break;

		case PUT_VAR_TEXT:
			char_buffer = mxArrayToString ( prhs[3] );
			status = nc_put_var_text ( ncid, varid, char_buffer );
			mxFree ( char_buffer );
			break;

		default:
			sprintf ( error_message, "unhandled opcode %d, %s, line %d file %s\n", opcode, opname, __LINE__, __FILE__ );
			mexErrMsgTxt ( error_message );
			return;
			
	}
	plhs[0] = mexncCreateDoubleScalar ( status );

	return;
}








/*
 * HANDLE_NC_PUT_VARA:
 *
 * code for handling the nc_put_vara_xxx family of function calls.
 * */
void handle_nc_put_vara ( 
        int            nlhs, 
        mxArray       *plhs[], 
        int            nrhs, 
        const mxArray *prhs[], 
        op            *nc_op ) 
{

	/*
	 * Constitutes the position index where the data write is to begin.
	 * */
	size_t		  *nc_start_coord;
	size_t		  *nc_count_coord;
	
	/*
	 * NetCDF File ID
	 * */
	int	  ncid;
	
	/*
	 * NetCDF variable ID
	 * */
	int	  varid;

	/*
	 * Short cut pointer to matlab array data.
	 * */
	double  *pr;

	/*
	 * This points to actual matrix data.
	 * */
	double  *data_buffer;

	/*
	 * Generic space for character data.
	 * */
	char   *char_buffer;

	/*
	 * Return status from netcdf operation.
	 * */
	int	  status;


	char	error_message[1000];

	OPCODE  opcode = nc_op->opcode;
	char   *opname = nc_op->opname;  


	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"varid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[3]) == false ) {
	        sprintf ( error_message, 
				"start index argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[4]) == false ) {
	        sprintf ( error_message, 
				"count index argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	



	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	varid = (int)(pr[0]);
	nc_start_coord = Mat2Size_t ( prhs[3] );
	nc_count_coord = Mat2Size_t ( prhs[4] );
	data_buffer = mxGetPr ( prhs[5] );


	switch ( opcode ) {

		case PUT_VARA_DOUBLE:
			status = nc_put_vara_double ( ncid, varid, nc_start_coord, nc_count_coord, data_buffer );
			break;

		case PUT_VARA_FLOAT:
			status = nc_put_vara_float ( ncid, varid, nc_start_coord, nc_count_coord, (float *)data_buffer );
			break;

		case PUT_VARA_INT:
			status = nc_put_vara_int ( ncid, varid, nc_start_coord, nc_count_coord, (int *)data_buffer );
			break;

		case PUT_VARA_SHORT:
			status = nc_put_vara_short ( ncid, varid, nc_start_coord, nc_count_coord, (short int *)data_buffer );
			break;

		case PUT_VARA_SCHAR:
			status = nc_put_vara_schar ( ncid, varid, nc_start_coord, nc_count_coord, (signed char *)data_buffer );
			break;

		case PUT_VARA_UCHAR:
			status = nc_put_vara_uchar ( ncid, varid, nc_start_coord, nc_count_coord, (unsigned char *)data_buffer );
			break;

		case PUT_VARA_TEXT:
			char_buffer = mxArrayToString ( prhs[5] );
			status = nc_put_vara_text ( ncid, varid, nc_start_coord, nc_count_coord, char_buffer );
			mxFree ( char_buffer );
			break;

		default:
			sprintf ( error_message, "unhandled opcode %d, %s, line %d file %s\n", opcode, opname, __LINE__, __FILE__ );
			mexErrMsgTxt ( error_message );
			return;
			
	}

	plhs[0] = mexncCreateDoubleScalar ( status );
	return;

}








/*
 * HANDLE_NC_PUT_VARS:
 *
 * code for handling the nc_put_vars_xxx family of function calls.
 * */
void handle_nc_put_vars ( 
        int            nlhs, 
        mxArray       *plhs[], 
        int            nrhs, 
        const mxArray *prhs[], 
        op            *nc_op ) 
{

	/*
	 * Constitutes the position index where the data write is to begin.
	 * */
	size_t		  *nc_start_coord;
	size_t		  *nc_count_coord;
	ptrdiff_t	   *nc_stride_coord;
	
	/*
	 * NetCDF File ID
	 * */
	int	  ncid;
	
	/*
	 * NetCDF variable ID
	 * */
	int	  varid;

	/*
	 * Short cut pointer to matlab array data.
	 * */
	double  *pr;

	/*
	 * This points to actual matrix data.
	 * */
	double  *data_buffer;

	/*
	 * Generic space for character data.
	 * */
	char   *char_buffer;

	/*
	 * Return status from netcdf operation.
	 * */
	int	  status;


	char	error_message[1000];

	OPCODE  opcode = nc_op->opcode;
	char   *opname = nc_op->opname;  


	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"varid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[3]) == false ) {
	        sprintf ( error_message, 
				"start index argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[4]) == false ) {
	        sprintf ( error_message, 
				"count index argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[5]) == false ) {
	        sprintf ( error_message, 
				"stride index argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	




	/*
	 * ... now extract the inputs ...
	 * */
	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	varid = (int)(pr[0]);
	nc_start_coord = Mat2Size_t ( prhs[3] );
	nc_count_coord = Mat2Size_t ( prhs[4] );
	nc_stride_coord = Mat2Ptrdiff_t ( prhs[5] );
	data_buffer = mxGetPr ( prhs[6] );


	/*
	 * ... and do that voodoo, that you do, so well ...
	 * */
	switch ( opcode ) {

		case PUT_VARS_DOUBLE:
			status = nc_put_vars_double ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, data_buffer );
			break;

		case PUT_VARS_FLOAT:
			status = nc_put_vars_float ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, (float *)data_buffer );
			break;

		case PUT_VARS_INT:
			status = nc_put_vars_int ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, (int *)data_buffer );
			break;

		case PUT_VARS_SHORT:
			status = nc_put_vars_short ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, (short int *)data_buffer );
			break;

		case PUT_VARS_SCHAR:
			status = nc_put_vars_schar ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, (signed char *)data_buffer );
			break;

		case PUT_VARS_UCHAR:
			status = nc_put_vars_uchar ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, (unsigned char *)data_buffer );
			break;

		case PUT_VARS_TEXT:
			char_buffer = mxArrayToString ( prhs[6] );
			status = nc_put_vars_text ( ncid, varid, nc_start_coord, nc_count_coord, nc_stride_coord, char_buffer );
			mxFree ( char_buffer );
			break;

		default:
			sprintf ( error_message, "unhandled opcode %d, %s, line %d file %s\n", opcode, opname, __LINE__, __FILE__ );
			mexErrMsgTxt ( error_message );
			return;
			
	}

	plhs[0] = mexncCreateDoubleScalar ( status );
	return;

}








/*
 * HANDLE_NC_PUT_VARM:
 *
 * code for handling the nc_put_varm_xxx family of function calls.
 * */
void handle_nc_put_varm ( 
        int            nlhs, 
        mxArray       *plhs[], 
        int            nrhs, 
        const mxArray *prhs[], 
        op            *nc_op ) 
{

	/*
	 * Constitutes the position index where the data write is to begin.
	 * */
	size_t		  *nc_start_coord;
	size_t		  *nc_count_coord;
	ptrdiff_t	   *nc_stride_coord;
	ptrdiff_t	   *nc_imap_coord;
	
	/*
	 * NetCDF File ID
	 * */
	int	  ncid;
	
	/*
	 * NetCDF variable ID
	 * */
	int	  varid;

	/*
	 * Short cut pointer to matlab array data.
	 * */
	double  *pr;

	/*
	 * This points to actual matrix data.
	 * */
	double  *data_buffer;

	/*
	 * Generic space for character data.
	 * */
	char   *char_buffer;

	/*
	 * Return status from netcdf operation.
	 * */
	int	  status;


	char	error_message[1000];

	OPCODE  opcode = nc_op->opcode;
	char   *opname = nc_op->opname;  




	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"varid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[3]) == false ) {
	        sprintf ( error_message, 
				"start index argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[4]) == false ) {
	        sprintf ( error_message, 
				"count index argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[5]) == false ) {
	        sprintf ( error_message, 
				"stride index argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[6]) == false ) {
	        sprintf ( error_message, 
				"imap index argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	




	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	varid = (int)(pr[0]);
	nc_start_coord = Mat2Size_t ( prhs[3] );
	nc_count_coord = Mat2Size_t ( prhs[4] );
	nc_stride_coord = Mat2Ptrdiff_t ( prhs[5] );
	nc_imap_coord = Mat2Ptrdiff_t ( prhs[6] );
	data_buffer = mxGetPr ( prhs[7] );


	switch ( opcode ) {

		case PUT_VARM_DOUBLE:
			status = nc_put_varm_double ( ncid, varid, 
					nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, 
					data_buffer );
			break;

		case PUT_VARM_FLOAT:
			status = nc_put_varm_float ( ncid, varid, 
					nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, 
					(float *)data_buffer );
			break;

		case PUT_VARM_INT:
			status = nc_put_varm_int ( ncid, varid, 
					nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, 
					(int *)data_buffer );
			break;

		case PUT_VARM_SHORT:
			status = nc_put_varm_short ( ncid, varid, 
					nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, 
					(short int *)data_buffer );
			break;

		case PUT_VARM_SCHAR:
			status = nc_put_varm_schar ( ncid, varid, 
					nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, 
					(signed char *)data_buffer );
			break;

		case PUT_VARM_UCHAR:
			status = nc_put_varm_uchar ( ncid, varid, 
					nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, 
					(unsigned char *)data_buffer );
			break;

		case PUT_VARM_TEXT:
			char_buffer = mxArrayToString ( prhs[7] );
			status = nc_put_varm_text ( ncid, varid, 
					nc_start_coord, nc_count_coord, nc_stride_coord, nc_imap_coord, 
					char_buffer );
			mxFree ( char_buffer );
			break;

		default:
			sprintf ( error_message, "unhandled opcode %d, %s, line %d file %s\n", opcode, opname, __LINE__, __FILE__ );
			mexErrMsgTxt ( error_message );
			
	}

	plhs[0] = mexncCreateDoubleScalar ( status );
	return;

}









/*
 * HANDLE_GET_ATT:  retrieve an attribute
 *
 * code for handling the nc_get_att_xxx family of function calls.
 * */
void handle_nc_get_att ( 
        int            nlhs, 
        mxArray       *plhs[], 
        int            nrhs, 
        const mxArray *prhs[], 
        op            *nc_op ) 
{

	/*
	 * NetCDF File ID
	 * */
	int	  ncid;
	
	/*
	 * NetCDF variable ID
	 * */
	int	  varid;

	/*
	 * Short cut pointer to matlab array data.
	 * */
	double  *pr;

	/*
	 * Generic space for character data.
	 * */
	char   *char_buffer;

	/*
	 * Return status from netcdf operation.
	 * */
	int	  status;


	/*
	 * loop index
	 * */
	int			  j;

	/*
	 * number of elements in an attribute
	 * */
	size_t			 att_len;

	/*
	 * Generic ptr for accessing mxChar data.
	 * */
	mxChar *mxchar_ptr;

	/*
	 * Generic ptr for accessing char data.
	 * */
	char *char_ptr;

	/*
	 * attribute name
	 * */
	char *att_name;


	char	error_message[1000];

	OPCODE  opcode = nc_op->opcode;
	char   *opname = nc_op->opname;  


	/*
	 * rank of attribute array
	 * */
	int	mx_size[2];


	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"varid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsChar(prhs[3]) == false ) {
	        sprintf ( error_message, 
				"attname argument must be matlab char, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	


	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	varid = (int)(pr[0]);
	att_name = Mat2Str ( prhs[3] );

	/* 
	 * Get the length that define the variable.  
	 * */
	status = nc_inq_attlen ( ncid, varid, att_name, &att_len );
	if ( status != NC_NOERR ) {
		sprintf ( error_message, "nc_inq_attlen failed, line %d, file %s\n", __LINE__, __FILE__ );
		mexErrMsgTxt ( error_message );
	}

	mx_size[0] = 1;
	mx_size[1] = att_len;


	/*
	 * And finally retrieve the data.
	 * */
	switch ( opcode ) {
		case GET_ATT_DOUBLE:
			plhs[0] = mxCreateNumericArray ( 2, mx_size, mxDOUBLE_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_att_double ( ncid, varid, att_name, pr );
			break;

		case GET_ATT_FLOAT:
			plhs[0] = mxCreateNumericArray ( 2, mx_size, mxSINGLE_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_att_float ( ncid, varid, att_name, (float *)pr );
			break;

		case GET_ATT_INT:
			plhs[0] = mxCreateNumericArray ( 2, mx_size, mxINT32_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_att_int ( ncid, varid, att_name, (int *)pr );
			break;

		case GET_ATT_SHORT:
			plhs[0] = mxCreateNumericArray ( 2, mx_size, mxINT16_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_att_short ( ncid, varid, att_name, (short int *)pr );
			break;

		case GET_ATT_SCHAR:
			plhs[0] = mxCreateNumericArray ( 2, mx_size, mxINT8_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_att_schar ( ncid, varid, att_name, (signed char *)pr );
			break;

		case GET_ATT_UCHAR:
			plhs[0] = mxCreateNumericArray ( 2, mx_size, mxUINT8_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_att_uchar ( ncid, varid, att_name, (unsigned char *)pr );
			break;

		case GET_ATT_TEXT:
			plhs[0] = mxCreateCharArray ( 2, mx_size );
			pr = mxGetPr ( plhs[0] );
			char_buffer = mxCalloc ( mx_size[1], sizeof(mxChar) );
			status = nc_get_att_text ( ncid, varid, att_name, char_buffer );

			/*
			 * Copy them into the mxChar array one char at a time.
			 * We apparently need to do it this way because there is
			 * a mismatch between the datatypes char and mxChar.
			 * */
			char_ptr = char_buffer;
			mxchar_ptr = (mxChar *)pr;
			for ( j = 0; j < mx_size[1]; ++j ) {
				mxchar_ptr[j] = (mxChar) ( char_ptr[j] );
			}


			break;

		default:
			sprintf ( error_message, "unhandled opcode %d, %s, line %d file %s\n", opcode, opname, __LINE__, __FILE__ );
			mexErrMsgTxt ( error_message );
			

	}
	plhs[1] = mexncCreateDoubleScalar ( status );

	return;

}






/*
 * HANDLE_NC_GET_VAR:
 *
 * code for handling the nc_get_var_xxx family of function calls.
 * */
void handle_nc_get_var ( 
        int            nlhs, 
        mxArray       *plhs[], 
        int            nrhs, 
        const mxArray *prhs[], 
        op            *nc_op ) 
{

	int	 mx_count_coord[MAX_NC_DIMS];
	
	/*
	 * NetCDF File ID
	 * */
	int	  ncid;
	
	/*
	 * NetCDF variable ID
	 * */
	int	  varid;

	/*
	 * Short cut pointer to matlab array data.
	 * */
	double  *pr;

	/*
	 * Generic space for character data.
	 * */
	char   *char_buffer;

	/*
	 * Return status from netcdf operation.
	 * */
	int	  status;

	/*
	 * number of dimensions in a NetCDF file and their IDs.
	 * */
	int	  ndims;
	int	  dimids[NC_MAX_DIMS]; 

	/*
	 * Length of a dimension.
	 * */
	size_t		  dimlen;


	/*
	 * loop index
	 * */
	int			  j;

	/*
	 * number of elements in a variable hyperslab
	 * */
	int			 varsize;

	/*
	 * Generic ptr for accessing mxChar data.
	 * */
	mxChar *mxchar_ptr;

	/*
	 * Generic ptr for accessing char data.
	 * */
	char *char_ptr;


	char	error_message[1000];

	OPCODE  opcode = nc_op->opcode;
	char   *opname = nc_op->opname;  


	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"varid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	
	
	

	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	varid = (int)(pr[0]);

	/*
	 * Get the dimensions that define the variable.
	 * */
	status = nc_inq_var ( ncid, varid, NULL, NULL, &ndims, dimids, NULL );
	if ( status != NC_NOERR ) {
		sprintf ( error_message, "nc_inq_var failed, line %d, file %s, \"%s\"\n", 
				__LINE__, __FILE__, nc_strerror(status) );
		mexErrMsgTxt ( error_message );
	}



	/*
	 * Allocate the space.
	 * */
	for ( j = 0; j < ndims; ++j ) {
		status = nc_inq_dimlen ( ncid, dimids[j], &dimlen );
		if ( status != NC_NOERR ) {
			sprintf ( error_message, "nc_inq_dimlen failed, line %d, file %s\n", __LINE__, __FILE__ );
			mexErrMsgTxt ( error_message );
		}

		/*
		 * We need to set the dimensions of the matrix as the reverse
		 * of how it is defined in the netcdf file.  This makes the
		 * size of the matrix seem transposed (upon return to matlab), 
		 * but otherwise the data gets layed out incorrectly due to 
		 * the difference between row-major order (C) and column-major 
		 * order (matlab).
		 * */
		mx_count_coord[ndims - j - 1] = dimlen;
	}


	/*
	 * If we are dealing with a singleton, then just set the number of dimensions to be 1.
	 * Seems like the simplest solution.
	 * */
	if ( ndims == 0 ) {
		ndims = 1;
		mx_count_coord[0] = 1;
	}

	/*
	 * And finally retrieve the data.
	 * */
	switch ( opcode ) {
		case GET_VAR_DOUBLE:
			plhs[0] = mxCreateNumericArray ( ndims, mx_count_coord, mxDOUBLE_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_var_double ( ncid, varid, pr );
			break;

		case GET_VAR_FLOAT:
			plhs[0] = mxCreateNumericArray ( ndims, mx_count_coord, mxSINGLE_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_var_float ( ncid, varid, (float *)pr );
			break;

		case GET_VAR_INT:
			plhs[0] = mxCreateNumericArray ( ndims, mx_count_coord, mxINT32_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_var_int ( ncid, varid, (int *)pr );
			break;

		case GET_VAR_SHORT:
			plhs[0] = mxCreateNumericArray ( ndims, mx_count_coord, mxINT16_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_var_short ( ncid, varid, (short int *)pr );
			break;

		case GET_VAR_SCHAR:
			plhs[0] = mxCreateNumericArray ( ndims, mx_count_coord, mxINT8_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_var_schar ( ncid, varid, (signed char *)pr );
			break;

		case GET_VAR_UCHAR:
			plhs[0] = mxCreateNumericArray ( ndims, mx_count_coord, mxUINT8_CLASS, mxREAL );
			pr = mxGetPr ( plhs[0] );
			status = nc_get_var_uchar ( ncid, varid, (unsigned char *)pr );
			break;

		case GET_VAR_TEXT:
			plhs[0] = mxCreateCharArray ( ndims, mx_count_coord );
			pr = mxGetPr ( plhs[0] );
			/*
			status = nc_get_var_text ( ncid, varid, (mxChar *)pr );
			*/
			varsize = 1;
			for ( j = 0; j < ndims; ++j ) {
				varsize *= mx_count_coord[j];
			}
			char_buffer = mxCalloc ( varsize + 1, sizeof(mxChar) );
			status = nc_get_var_text ( ncid, varid, char_buffer );

			/*
			 * Copy them into the mxChar array one char at a time.
			 * We apparently need to do it this way because there is
			 * a mismatch between the datatypes char and mxChar.
			 * */
			char_ptr = char_buffer;
			mxchar_ptr = (mxChar *)pr;
			for ( j = 0; j < varsize; ++j ) {
				mxchar_ptr[j] = (mxChar) ( char_ptr[j] );
			}


			break;

		default:
			sprintf ( error_message, "unhandled opcode %d, %s, line %d file %s\n", opcode, opname, __LINE__, __FILE__ );
			mexErrMsgTxt ( error_message );
			

	}
	plhs[1] = mexncCreateDoubleScalar ( status );

    return;

}








/*
 * HANDLE_NC_GET_VAR1:
 *
 * code for handling the nc_get_var1_xxx family of function calls.
 * */
void handle_nc_get_var1 ( 
        int            nlhs, 
        mxArray       *plhs[], 
        int            nrhs, 
        const mxArray *prhs[], 
        op            *the_op ) 
{

    /*
     * Constitutes the position index where the data write is to begin.
     * */
    size_t          *nc_start_coord;
    
    /*
     * NetCDF File ID
     * */
    int      ncid;
    
    /*
     * NetCDF variable ID
     * */
    int      varid;

    /*
     * Short cut pointer to matlab array data.
     * */
    double  *pr;

    /*
     * Return status from netcdf operation.
     * */
    int      status;

    /*
     * Values returned by "get_var1_xxx" calls.
     * */
    double          double_value;
    float           float_value;
    int             int_value;
    short int       short_value;
    signed char     schar_value;
    unsigned char   uchar_value;

    int              mx_count_coord[1] = { 1 };

    char    error_message[1000];
    char    tmp_buffer[1000];

    OPCODE  opcode = the_op->opcode;
    char   *opname = the_op->opname;  

    /*
     * Make sure that the inputs are the right type.
     * */
    if ( mxIsDouble(prhs[1]) == false ) {
            sprintf ( error_message, "ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", opname, __LINE__, __FILE__ );
            mexErrMsgTxt ( error_message );
            return;
    }



    if ( mxIsDouble(prhs[2]) == false ) {
            sprintf ( error_message, "varid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", opname, __LINE__, __FILE__ );
            mexErrMsgTxt ( error_message );
            return;
    }



    if ( mxIsDouble(prhs[3]) == false ) {
            sprintf ( error_message, "index argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", opname, __LINE__, __FILE__ );
            mexErrMsgTxt ( error_message );
            return;
    }




    pr = mxGetPr ( prhs[1] );
    ncid = (int)(pr[0]);
    pr = mxGetPr ( prhs[2] );
    varid = (int)(pr[0]);
    nc_start_coord = Mat2Size_t ( prhs[3] );

    switch ( opcode ) {
        case GET_VAR1_DOUBLE:
            status = nc_get_var1_double ( ncid, varid, nc_start_coord, &double_value );
            plhs[0] = mexncCreateDoubleScalar ( double_value );
            break;

        case GET_VAR1_FLOAT:
            status = nc_get_var1_float ( ncid, varid, nc_start_coord, &float_value );
            plhs[0] = mxCreateNumericArray ( 1, mx_count_coord, mxSINGLE_CLASS, mxREAL );
            pr = mxGetPr ( plhs[0] );
            ((float *)pr)[0] = float_value;
            break;

        case GET_VAR1_INT:
            status = nc_get_var1_int ( ncid, varid, nc_start_coord, &int_value );
            plhs[0] = mxCreateNumericArray ( 1, mx_count_coord, mxINT32_CLASS, mxREAL );
            pr = mxGetPr ( plhs[0] );
            ((int *)pr)[0] = int_value;
            break;

        case GET_VAR1_SHORT:
            status = nc_get_var1_short ( ncid, varid, nc_start_coord, &short_value );
            plhs[0] = mxCreateNumericArray ( 1, mx_count_coord, mxINT16_CLASS, mxREAL );
            pr = mxGetPr ( plhs[0] );
            ((short int *)pr)[0] = short_value;
            break;

        case GET_VAR1_SCHAR:
            status = nc_get_var1_schar ( ncid, varid, nc_start_coord, &schar_value );
            plhs[0] = mxCreateNumericArray ( 1, mx_count_coord, mxINT8_CLASS, mxREAL );
            pr = mxGetPr ( plhs[0] );
            ((signed char*)pr)[0] = schar_value;
            break;

        case GET_VAR1_UCHAR:
            status = nc_get_var1_uchar ( ncid, varid, nc_start_coord, &uchar_value );
            plhs[0] = mxCreateNumericArray ( 1, mx_count_coord, mxUINT8_CLASS, mxREAL );
            pr = mxGetPr ( plhs[0] );
            ((unsigned char*)pr)[0] = uchar_value;
            break;

        case GET_VAR1_TEXT:
            status = nc_get_var1_text ( ncid, varid, nc_start_coord, tmp_buffer );
            tmp_buffer[1] = '\0';
            plhs[0] = mxCreateString ( tmp_buffer );
            plhs[1] = mexncCreateDoubleScalar ( status );
            break;

        default:
            sprintf ( error_message, "unhandled opcode %d, %s, line %d file %s\n", opcode, opname, __LINE__, __FILE__ );
            mexErrMsgTxt ( error_message );
            return;

    }
    plhs[1] = mexncCreateDoubleScalar ( status );



    return;

}











/*
 * HANDLE_NC_PUT_VAR1:
 *
 * code for handling the nc_put_var1_xxx family of function calls.
 * */
void handle_nc_put_var1 ( 
        int            nlhs, 
        mxArray       *plhs[], 
        int            nrhs, 
        const mxArray *prhs[], 
        op            *nc_op ) 
{

	/*
	 * Constitutes the position index where the data write is to begin.
	 * */
	size_t		  *nc_start_coord;
	
	/*
	 * NetCDF File ID
	 * */
	int	  ncid;
	
	/*
	 * NetCDF variable ID
	 * */
	int	  varid;

	/*
	 * Short cut pointer to matlab array data.
	 * */
	double  *pr;

	/*
	 * This points to actual matrix data.
	 * */
	double  *data_buffer;

	/*
	 * Return status from netcdf operation.
	 * */
	int	  status;


	char	error_message[1000];

	OPCODE  opcode = nc_op->opcode;
	char   *opname = nc_op->opname;  


	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"varid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[3]) == false ) {
	        sprintf ( error_message, 
				"start index argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	
	

	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	varid = (int)(pr[0]);
	nc_start_coord = Mat2Size_t ( prhs[3] );
	data_buffer = mxGetPr ( prhs[4] );

	switch ( opcode ) {

		case PUT_VAR1_DOUBLE:
			status = nc_put_var1_double ( ncid, varid, nc_start_coord, (double *)data_buffer );
			break;

		case PUT_VAR1_FLOAT:
			status = nc_put_var1_float ( ncid, varid, nc_start_coord, (float *)data_buffer );
			break;

		case PUT_VAR1_INT:
			status = nc_put_var1_int ( ncid, varid, nc_start_coord, (int *)data_buffer );
			break;

		case PUT_VAR1_SHORT:
			status = nc_put_var1_short ( ncid, varid, nc_start_coord, (short int *)data_buffer );
			break;

		case PUT_VAR1_SCHAR:
			status = nc_put_var1_schar ( ncid, varid, nc_start_coord, (signed char*)data_buffer );
			break;

		case PUT_VAR1_UCHAR:
			status = nc_put_var1_uchar ( ncid, varid, nc_start_coord, (unsigned char*)data_buffer );
			break;

		case PUT_VAR1_TEXT:
			status = nc_put_var1_text ( ncid, varid, nc_start_coord, (char *)data_buffer );
			break;

		default:
			sprintf ( error_message, "unhandled opcode %d, %s, line %d file %s\n", opcode, opname, __LINE__, __FILE__ );
			mexErrMsgTxt ( error_message );
			return;

	}
	plhs[0] = mexncCreateDoubleScalar ( status );

	return;

}










/*
 * HANDLE_COPY_ATT:
 *
 * code for handling the nc_copy_att routine.
 *
 * */
void   handle_nc_copy_att ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid_in;
        int varid_in;
        int ncid_out;
        int varid_out;

	/* 
	 * name of netcdf attribute name
	 * */
	char    *attname;

	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;



	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid_in argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	
	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"varid_in argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	
	if ( mxIsChar(prhs[3]) == false ) {
	        sprintf ( error_message, 
				"attname argument must be character, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	if ( mxIsDouble(prhs[4]) == false ) {
	        sprintf ( error_message, 
				"ncid_out argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	
	if ( mxIsDouble(prhs[5]) == false ) {
	        sprintf ( error_message, 
				"varid_out argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	



        pr = mxGetPr ( prhs[1] );
        ncid_in = (int)(pr[0]);
        pr = mxGetPr ( prhs[2] );
        varid_in = (int)(pr[0]);
        attname = Mat2Str(prhs[3]);
        pr = mxGetPr ( prhs[4] );
        ncid_out = (int)(pr[0]);
        pr = mxGetPr ( prhs[5] );
        varid_out = (int)(pr[0]);

        status = nc_copy_att ( ncid_in, varid_in, attname, ncid_out, varid_out );
        plhs[0] = mexncCreateDoubleScalar ( status );
	return;

}







            
/*
 * HANDLE_NC_INQ_ATTID:
 *
 * code for handling the nc_inq_attid routine.
 *
 * */
void   handle_nc_inq_attid ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;
        int varid;

	/* 
	 * name of netcdf variable 
	 * */
	char    *attname;

	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;


	/*
	* numeric id of an attribute
	* */
	int     attribute_id;


	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	
	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"varid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	
	if ( mxIsChar(prhs[3]) == false ) {
	        sprintf ( error_message, 
				"attname argument must be character, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	


	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	varid = (int)(pr[0]);

	attname = Mat2Str(prhs[3]);
            
	status = nc_inq_attid ( ncid, varid, attname, &attribute_id );
	plhs[0] = mexncCreateDoubleScalar (attribute_id);
	plhs[1] = mexncCreateDoubleScalar (status);
        
            


	return;

}


            
/*
 * HANDLE_NC_INQ_ATTNAME:
 *
 * code for handling the nc_inq_attname routine.
 *
 * */
void   handle_nc_inq_attname ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;
        int varid;

	/* 
	 * name of netcdf variable 
	 * */
	char    *attname;

	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;

	/*
	 * numeric id of an attribute
	 * */
	int     attribute_id;



	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	
	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"varid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	
	if ( mxIsDouble(prhs[3]) == false ) {
	        sprintf ( error_message, 
				"attid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	varid = (int)(pr[0]);
	pr = mxGetPr ( prhs[3] );
	attribute_id = (int)(pr[0]);

	attname = (char *) mxCalloc(MAX_NC_NAME, sizeof(char));

	status = nc_inq_attname ( ncid, varid, attribute_id, attname );
	if ( status < 0 ) {
		plhs[0] = mxCreateString ("");
		plhs[1] = mexncCreateDoubleScalar (status);
	} else {
		plhs[0] = mxCreateString (attname);
		plhs[1] = mexncCreateDoubleScalar (status);
	}


	return;

}










/*
 * HANDLE_NC_ABORT:
 *
 * code for handling the nc_abort routine.
 *
 * */
void   handle_nc_abort ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;

	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;



	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	status = nc_abort(ncid);
	plhs[0] = mexncCreateDoubleScalar(status);
	


	return;

}










            
/*
 * HANDLE_NC_CLOSE:
 *
 * code for handling the nc_close routine.
 *
 * */
void   handle_nc_close ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;

	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;



	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	status = nc_close(ncid);
	plhs[0] = mexncCreateDoubleScalar ( status );




	return;

}











/*
 * HANDLE_NC__CREATE:
 *
 * code for handling the nc__create routine.
 *
 * */
void   handle_nc__create ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;

	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;

	/*
	 * NetCDF file creation mode.
	 * */
	int cmode;

	/*
	 * sets the initial size of the file at creation time.
	 * */
	size_t initialsize;


	/*
	 * See the man page for a description of chunksize.
	 * */
	size_t chunksize;


	/*
	 * path of NetCDF file
	 * */
	char    *path;


	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsChar(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"operation \"%s\":  netcdf file argument must be character, line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( !((mxIsChar(prhs[2]) == true) || (mxIsDouble(prhs[2]) == true )) ) {
	        sprintf ( error_message, 
				"operation \"%s\":  mode argument must be either character or numeric, line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	}
	if ( mxIsDouble(prhs[3]) == false ) {
	        sprintf ( error_message, 
				"operation \"%s\":  initial size argument must be double precision, line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	

	
	path = Mat2Str(prhs[1]);
	pr = mxGetPr ( prhs[2] );
	cmode = (int) pr[0];
	pr = mxGetPr ( prhs[3] );
	initialsize = (int)(pr[0]);
            
            
	status = nc__create ( path, cmode, initialsize, &chunksize, &ncid );
	plhs[0] = mexncCreateDoubleScalar ( chunksize );
	plhs[1] = mexncCreateDoubleScalar ( ncid );
	plhs[2] = mexncCreateDoubleScalar ( status );
            
	return;

}








/*
 * HANDLE_NC_CREATE:
 *
 * code for handling the nc_create routine.
 *
 * */
void   handle_nc_create ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;

	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;

	/*
	 * NetCDF file creation mode.
	 * */
	int cmode;

	/*
	 * path of NetCDF file
	 * */
	char    *path;


	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsChar(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"operation \"%s\":  netcdf file argument must be character, line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	/*
	 * The mode argument has historically been something like 'clobber', which is
	 * a character data type.  I'd prefer it just a mnemonic numeric value, like 
	 * that which is returned by "nc_clobber_mode.m", but
	 * too much time has passed for that.
	 * */
	if ( nrhs == 3 ) {
		if ( !((mxIsChar(prhs[2]) == true) || (mxIsDouble(prhs[2]) == true )) ) {
		        sprintf ( error_message, 
					"operation \"%s\":  mode argument must be either character or numeric, line %d file \"%s\"\n", 
					nc_op->opname, __LINE__, __FILE__ );
		        mexErrMsgTxt ( error_message );
		        return;
		}
		pr = mxGetPr ( prhs[2] );
		cmode = (int) pr[0];
	} else {
		cmode = NC_NOCLOBBER;
	}


	
	path = Mat2Str(prhs[1]);
            
            
	status = nc_create ( path, cmode, &ncid );
	plhs[0] = mexncCreateDoubleScalar ( ncid );
	plhs[1] = mexncCreateDoubleScalar ( status );
            
	return;

}









/*
 * HANDLE_NC_DEF_DIM:
 *
 * code for handling the nc_def_dim routine.
 *
 * */
void   handle_nc_def_dim ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;

	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;


	/* 
	 * name of netcdf variable 
	 * */
	char    *dimension_name;


	/*
	 * Length of the dimension being defined.
	 * */
	size_t  dim_length;


	/*
	 * NetCDF identifier for newly defined dimension.
	 * */
	int     dimid;


	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	if ( mxIsChar(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"dimname argument must be character, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	if ( mxIsDouble(prhs[3]) == false ) {
	        sprintf ( error_message, 
				"dim_length argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	dimension_name = Mat2Str(prhs[2]);
	pr = mxGetPr ( prhs[3] );
	dim_length = (int) pr[0];
	status = nc_def_dim ( ncid, dimension_name, dim_length, &dimid );
	plhs[0] = mexncCreateDoubleScalar ( dimid );
	plhs[1] = mexncCreateDoubleScalar ( status );




	return;

}











/*
 * HANDLE_NC_DEF_VAR:
 *
 * code for handling the nc_def_var routine.
 *
 * */
void   handle_nc_def_var ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;

	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;


	/* 
	 * name of netcdf variable 
	 * */
	char    *name;



	/* 
	 * UNIDATA defined type of a variable.
	 * */
	nc_type  datatype;

	/* 
	 * number of dimensions in a NetCDF file and their IDs.
	 * */
	int             ndims;
	int             dimids[NC_MAX_DIMS]; 

	/* 
	 * NetCDF variable ID
	 * */
	int      varid;


	/*
	 * Loop index
	 * */
	int j;

	/*
	 * Sizes of input matrices.
	 * */
	int m, n;


	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	if ( mxIsChar(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"name argument must be character, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	if ( !((mxIsChar(prhs[3]) == false) || (mxIsDouble(prhs[3]) == false )) ) {
	        sprintf ( error_message, 
				"datatype argument must be matlab native double precision (<== that one, please) or character, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	if ( mxIsDouble(prhs[4]) == false ) {
	        sprintf ( error_message, 
				"ndims argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	if ( mxIsDouble(prhs[5]) == false ) {
	        sprintf ( error_message, 
				"dimids argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	name = Mat2Str(prhs[2]);
	/*
	pr = mxGetPr ( prhs[3] );
	datatype = (nc_type) pr[0];
	*/
	datatype = Parameter ( prhs[3] );
	pr = mxGetPr ( prhs[4] );
	ndims = (int) (pr[0]);

	/*
	 * Make sure the user didn't do something really stupid like give too many dimensions.
	 * */
	if ( ndims > NC_MAX_VAR_DIMS ) {
	        sprintf ( error_message, 
				"given number of dimensions (%d) exceeds preset maximum of %d, operation \"%s\", line %d file \"%s\"\n", 
				ndims, NC_MAX_VAR_DIMS, nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}



	/*
	 * And check that the given number of dimensions matches what the dimension array really has.
	 * */
	pr = mxGetPr ( prhs[5] );
	m = mxGetM ( prhs[5] );
	n = mxGetN ( prhs[5] );
	if ( ndims != (m*n) ) {
	        sprintf ( error_message, 
				"given number of dimensions (%d) does not equal the size of the dimid array actually passed (%d), operation \"%s\", line %d file \"%s\"\n", 
				ndims, m*n, nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}


	/*
	 * Copy the dimension ids on over.
	 * */
	for ( j = 0; j < ndims; ++j ) {
		dimids[j] = (int) (pr[j]);
	}
	status = nc_def_var ( ncid, name, datatype, ndims, dimids, &varid );
	plhs[0] = mexncCreateDoubleScalar ( varid );
	plhs[1] = mexncCreateDoubleScalar ( status );


	return;

}











/*
 * HANDLE_NC_DEL_ATT:
 *
 * code for handling the nc_del_att routine.
 *
 * */
void   handle_nc_del_att ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;

	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;


	/* 
	 * name of netcdf variable 
	 * */
	char    *name;



	/* 
	 * NetCDF variable ID
	 * */
	int      varid;



	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"varid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	if ( mxIsChar(prhs[3]) == false ) {
	        sprintf ( error_message, 
				"name argument must be character, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	varid = (int)(pr[0]);
	name = Mat2Str(prhs[3]);
	status = nc_del_att ( ncid, varid, name );
	plhs[0] = mexncCreateDoubleScalar ( status );

	return;
            
}











/*
 * HANDLE_NC__ENDDEF:
 *
 * code for handling the nc__enddef routine.
 *
 * */
void   handle_nc__enddef ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;

	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;


	/*
	 * performance tuning parameters.
	 * See the netcdf man page for details.
	 * */
	size_t h_minfree, v_align, v_minfree, r_align;


	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"h_minfree argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[3]) == false ) {
	        sprintf ( error_message, 
				"v_align argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[4]) == false ) {
	        sprintf ( error_message, 
				"v_minfree argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[5]) == false ) {
	        sprintf ( error_message, 
				"r_align argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	h_minfree = (size_t)(pr[0]);
	pr = mxGetPr ( prhs[3] );
	v_align = (size_t)(pr[0]);
	pr = mxGetPr ( prhs[4] );
	v_minfree = (size_t)(pr[0]);
	pr = mxGetPr ( prhs[5] );
	r_align = (size_t)(pr[0]);
	status = nc__enddef(ncid, h_minfree, v_align, v_minfree, r_align);
	plhs[0] = mexncCreateDoubleScalar ( status );
	return;
            
}










/*
 * HANDLE_NC_ENDDEF:
 *
 * code for handling the nc_end_def routine.
 *
 * */
void   handle_nc_enddef ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;

	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;


	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	status = nc_enddef(ncid);
	plhs[0] = mexncCreateDoubleScalar ( status );
	return;
            
}










/*
 * HANDLE_NC_INQ:
 *
 * code for handling the nc_inq routine.
 *
 * */
void   handle_nc_inq ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;

	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;


	/* 
	 * Return arguments.
	 * */
	int      ndims;     /* number of dimensions in the netcdf file */
	int      nvars;     /* number of variables in the netcdf file */
	int      natts;     /* number of global attributes in the netcdf file */
	int      recdim;    /* ID of the record dimension in the netcdf file */


	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	status = nc_inq (ncid, &ndims, &nvars, &natts, &recdim);
	plhs[0] = mexncCreateDoubleScalar(ndims);
	plhs[1] = mexncCreateDoubleScalar(nvars);
	plhs[2] = mexncCreateDoubleScalar(natts);
	plhs[3] = mexncCreateDoubleScalar(recdim);
	plhs[4] = mexncCreateDoubleScalar(status);
	return;
            

}










/*
 * HANDLE_NC_INQ_NDIMS:
 *
 * code for handling the nc_inq_ndims routine.
 *
 * */
void   handle_nc_inq_ndims ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;

	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;


	/* 
	 * Return arguments.
	 * */
	int      ndims;     /* number of dimensions in the netcdf file */


	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	status = nc_inq_ndims (ncid, &ndims);
	plhs[0] = mexncCreateDoubleScalar(ndims);
	plhs[1] = mexncCreateDoubleScalar(status);
	return;
            


}









/*
 * HANDLE_NC_INQ_NVARS:
 *
 * code for handling the nc_inq_nvars routine.
 *
 * */
void   handle_nc_inq_nvars ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;

	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;


	/*
	 * Number of variables in a netcdf file
	 * */
	int	  nvars;


	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	status = nc_inq_nvars (ncid, &nvars);
	plhs[0] = mexncCreateDoubleScalar(nvars);
	plhs[1] = mexncCreateDoubleScalar(status);
	return;
			




}









/*
 * HANDLE_NC_INQ_NATTS:
 *
 * code for handling the nc_inq_natts routine.
 *
 * */
void   handle_nc_inq_natts ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;

	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;


	/*
	 * Number of global attributes in a netcdf file
	 * */
	int	  natts;


	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	
	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	status = nc_inq_natts (ncid, &natts);
	plhs[0] = mexncCreateDoubleScalar(natts);
	plhs[1] = mexncCreateDoubleScalar(status);
	return;
			

}









/*
 * HANDLE_NC_INQ_ATT:
 *
 * code for handling the nc_inq_att routine.
 *
 * */
void   handle_nc_inq_att ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;
        int varid;

	/*
	 * name of attribute
	 * */
	char	*name;


	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;


	/*
	 * NetCDF attribute datatype and attribute length
	 * */
	nc_type	  datatype;
	size_t	  attribute_length;


	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"varid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsChar(prhs[3]) == false ) {
	        sprintf ( error_message, 
				"attname argument must be character, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
			

		
	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	varid = (int)(pr[0]);

	name = Mat2Str(prhs[3]);
			
	status = nc_inq_att ( ncid, varid, name, &datatype, &attribute_length );
	if ( status < 0 ) {
		plhs[0] = mexncCreateDoubleScalar ( mxGetNaN() );
		plhs[1] = mexncCreateDoubleScalar ( mxGetNaN() );
	} else {
		plhs[0] = mexncCreateDoubleScalar (datatype);
		plhs[1] = mexncCreateDoubleScalar (attribute_length);
	}
	plhs[2] = mexncCreateDoubleScalar (status);
		
	return;
			




}










/*
 * HANDLE_NC_INQ_ATTLEN:
 *
 * code for handling the nc_inq_attlen routine.
 *
 * */
void   handle_nc_inq_attlen ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;
        int varid;

	/*
	 * name of attribute
	 * */
	char	*name;


	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;


	/*
	 * NetCDF attribute datatype and attribute length
	 * */
	size_t	  attribute_length;



	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"varid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsChar(prhs[3]) == false ) {
	        sprintf ( error_message, 
				"attname argument must be character, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
			
	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	varid = (int)(pr[0]);

	name = Mat2Str(prhs[3]);
	
	status = nc_inq_attlen ( ncid, varid, name, &attribute_length );
	plhs[0] = mexncCreateDoubleScalar ( attribute_length );
	plhs[1] = mexncCreateDoubleScalar (status);
		
			
	return;

}











/*
 * HANDLE_NC_INQ_ATTTYPE:
 *
 * code for handling the nc_inq_atttype routine.
 *
 * */
void   handle_nc_inq_atttype ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and variable IDs 
         * */
        int ncid;
        int varid;

	/*
	 * name of attribute
	 * */
	char	*name;


	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;



	/*
	 * Enumerated NetCDF datatype.
	 * */
	nc_type datatype;



	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"varid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsChar(prhs[3]) == false ) {
	        sprintf ( error_message, 
				"attname argument must be character, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
			

	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	varid = (int)(pr[0]);
	name = Mat2Str(prhs[3]);

	status = nc_inq_atttype ( ncid, varid, name, &datatype );
	plhs[0] = mexncCreateDoubleScalar (datatype);
	plhs[1] = mexncCreateDoubleScalar (status);
		

	return;

}













/*
 * HANDLE_NC_INQ_DIM:
 *
 * code for handling the nc_inq_dim routine.
 *
 * */
void   handle_nc_inq_dim ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and dimension IDs 
         * */
        int ncid;
        int dimid;

	/*
	 * name of attribute
	 * */
	char	*name;


	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;


	/*
	 * length of a dimension
	 * */
	size_t  dim_length;



	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"dimid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
			

	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	dimid = (int)(pr[0]);
	name = (char *) mxCalloc(MAX_NC_NAME, sizeof(char));

	status = nc_inq_dim ( ncid, dimid, name, & dim_length);
	plhs[0] = mxCreateString ( name );
	plhs[1] = mexncCreateDoubleScalar ( (double) dim_length );
	plhs[2] = mexncCreateDoubleScalar (status);

	return;

}













/*
 * HANDLE_NC_INQ_DIMID:
 *
 * code for handling the nc_inq_dimid routine.
 *
 * */
void   handle_nc_inq_dimid ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and dimension IDs 
         * */
        int ncid;
        int dimid;

	/*
	 * name of attribute
	 * */
	char	*name;


	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;




	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsChar(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"dimension name argument must be matlab character, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
			

	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	name = Mat2Str(prhs[2]);
	status = nc_inq_dimid( ncid, name, &dimid);
	plhs[0] = mexncCreateDoubleScalar(dimid);
	plhs[1] = mexncCreateDoubleScalar(status);

	return;


}













/*
 * HANDLE_NC_INQ_DIMLEN:
 *
 * code for handling the nc_inq_dimlen routine.
 *
 * */
void   handle_nc_inq_dimlen ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and dimension IDs 
         * */
        int ncid;
        int dimid;


	/*
	 * length of a dimension
	 * */
	size_t  dim_length;



	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;




	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"dimid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
			
	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	dimid = (int)(pr[0]);
	status = nc_inq_dimlen ( ncid, dimid, & dim_length);
	plhs[0] = mexncCreateDoubleScalar ( (double) dim_length );
	plhs[1] = mexncCreateDoubleScalar (status);


	return;


}













/*
 * HANDLE_NC_INQ_DIMNAME:
 *
 * code for handling the nc_inq_dimname routine.
 *
 * */
void   handle_nc_inq_dimname ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and dimension IDs 
         * */
        int ncid;
        int dimid;




	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;


	/*
	 * name of dimension
	 * */
	char	*name;



	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"dimid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
			
	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	dimid = (int)(pr[0]);
	name = (char *) mxCalloc(MAX_NC_NAME, sizeof(char));
	status = nc_inq_dimname ( ncid, dimid, name );
	plhs[0] = mxCreateString ( name );
	plhs[1] = mexncCreateDoubleScalar (status);


	return;

}













/*
 * HANDLE_NC_INQ_VAR:
 *
 * code for handling the nc_inq_var routine.
 *
 * */
void   handle_nc_inq_var ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and dimension IDs 
         * */
        int ncid;
        int varid;


	/*
	 * UNIDATA defined type of a variable.
	 * */
	nc_type  datatype;


	/*
	 * Loop index 
	 * */
	int j;

	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;


	/*
	 * name of dimension
	 * */
	char	*name;


	/*
	 * number of dimensions in a NetCDF file and their IDs.
	 * */
	int	 ndims;
	int	 dimids[NC_MAX_DIMS]; 


	/*
	 * Number of attributes for a netcdf variable.
	 * */
	int	  natts;



	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"varid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
			

	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	varid = (int)(pr[0]);

	name = (char *) mxCalloc(MAX_NC_NAME, sizeof(char));
	
	status = nc_inq_var ( ncid, varid, name, &datatype, &ndims, dimids, &natts );
	if ( status < 0 ) {
		plhs[0] = mxCreateString ("");
		plhs[1] = mexncCreateDoubleScalar (-1);
		plhs[2] = mexncCreateDoubleScalar (-1);
		plhs[3] = mexncCreateDoubleScalar (-1);
		plhs[4] = mexncCreateDoubleScalar (-1);
		plhs[5] = mexncCreateDoubleScalar (status);
	} else {
		plhs[0] = mxCreateString (name);
		plhs[1] = mexncCreateDoubleScalar (datatype);
		plhs[2] = mexncCreateDoubleScalar (ndims);

		/*
		 * Copy the dimension ids into the matrix
		 * */
		plhs[3] = mxCreateDoubleMatrix ( 1, ndims, mxREAL );
		pr = mxGetPr ( plhs[3] );
		for ( j = 0; j < ndims; ++j ) {
			pr[j] = dimids[j];
		}


		plhs[4] = mexncCreateDoubleScalar (natts);
		plhs[5] = mexncCreateDoubleScalar (status);
	}
		


	return;

}













/*
 * HANDLE_NC_INQ_VARNAME:
 *
 * code for handling the nc_inq_varname routine.
 *
 * */
void   handle_nc_inq_varname ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and dimension IDs 
         * */
        int ncid;
        int varid;


	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;


	/*
	 * name of dimension
	 * */
	char	*name;


	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"varid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
			

	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	varid = (int)(pr[0]);

	name = (char *) mxCalloc(MAX_NC_NAME, sizeof(char));
			
	status = nc_inq_varname ( ncid, varid, name );
	if ( status < 0 ) {
		plhs[0] = mxCreateString ("");
		plhs[1] = mexncCreateDoubleScalar (status);
	} else {
		plhs[0] = mxCreateString (name);
		plhs[1] = mexncCreateDoubleScalar (status);
	}
		

	return;

}













/*
 * HANDLE_NC_INQ_VARTYPE:
 *
 * code for handling the nc_inq_varname routine.
 *
 * */
void   handle_nc_inq_vartype ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and dimension IDs 
         * */
        int ncid;
        int varid;


	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;


	/*
	 * UNIDATA defined type of a variable.
	 * */
	nc_type  datatype;


	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"varid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
			

	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	varid = (int)(pr[0]);

	status = nc_inq_vartype ( ncid, varid, &datatype );
	plhs[0] = mexncCreateDoubleScalar (datatype);
	plhs[1] = mexncCreateDoubleScalar (status);



	return;

}













/*
 * HANDLE_NC_INQ_VARNDIMS:
 *
 * code for handling the nc_inq_varndims routine.
 *
 * */
void   handle_nc_inq_varndims ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and dimension IDs 
         * */
        int ncid;
        int varid;


	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;


	/*
	 * number of dimensions in a NetCDF file and their IDs.
	 * */
	int	 ndims;




	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"varid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
			

	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	varid = (int)(pr[0]);

	status = nc_inq_varndims ( ncid, varid, &ndims );
	plhs[0] = mexncCreateDoubleScalar (ndims);
	plhs[1] = mexncCreateDoubleScalar (status);

	return;

}













/*
 * HANDLE_NC_INQ_VARDIMID:
 *
 * code for handling the nc_inq_vardimid routine.
 *
 *
 * This is a strange case.  Rather than call nc_inq_vardimid
 * we just call nc_inq_var with NULL in place of certain
 * arguments.  We need to have ndims in order to properly
 * size the dimids matrix.  Otherwise we'd make two calls
 * instead of one.
 * 
 * */
void   handle_nc_inq_vardimid ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;

	/*
	 * Loop index.
	 * */
	int j;

        /*
         * file and dimension IDs 
         * */
        int ncid;
        int varid;


	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;


	/*
	 * number of dimensions in a NetCDF file and their IDs.
	 * */
	int	 ndims;
	int	 dimids[NC_MAX_DIMS]; 






	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"varid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
			

	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	varid = (int)(pr[0]);

	status = nc_inq_var ( ncid, varid, NULL, NULL, &ndims, dimids, NULL );
	if ( status < 0 ) {

		plhs[0] = mexncCreateDoubleScalar (-1);
		plhs[1] = mexncCreateDoubleScalar (status);

	} else {

		/*
		 * Copy the dimension ids into the matrix
		 * */
		plhs[0] = mxCreateDoubleMatrix ( 1, ndims, mxREAL );
		pr = mxGetPr ( plhs[0] );
		for ( j = 0; j < ndims; ++j ) {
			pr[j] = dimids[j];
		}


		plhs[1] = mexncCreateDoubleScalar (status);

	}
		

	return;

}













/*
 * HANDLE_NC_INQ_VARNATTS:
 *
 * code for handling the nc_inq_varnatts routine.
 *
 * */
void   handle_nc_inq_varnatts ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;

        /*
         * file and dimension IDs 
         * */
        int ncid;
        int varid;


	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;


	/*
	 * number of variable attributes
	 * */
	int	 natts;






	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"varid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
			
	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	varid = (int)(pr[0]);

	status = nc_inq_varnatts ( ncid, varid, &natts );
	plhs[0] = mexncCreateDoubleScalar (natts);
	plhs[1] = mexncCreateDoubleScalar (status);

	return;

}










/*
 * HANDLE_NC_INQ_UNLIMDIM:
 *
 * code for handling the nc_inq_unlimdim routine.
 *
 * */
void   handle_nc_inq_unlimdim ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;

        /*
         * file id 
         * */
        int ncid;


	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;


	/*
	 * dimension id for unlimited dimension
	 * */
	int	  recdim;





	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
			
	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	status = nc_inq_unlimdim ( ncid, &recdim );
	plhs[0] = mexncCreateDoubleScalar(recdim);
	plhs[1] = mexncCreateDoubleScalar(status);

	return;

}










/*
 * HANDLE_NC_INQ_VARID:
 *
 * code for handling the nc_inq_varid routine.
 *
 * */
void   handle_nc_inq_varid ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;

        /*
         * file and dimension IDs 
         * */
        int ncid;
        int varid;


	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;


	/*
	 * name of variable
	 * */
	char	*name;








	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( mxIsChar(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"variable name argument must be of type matlab char, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
			
	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	name = Mat2Str(prhs[2]);
	status = nc_inq_varid ( ncid, name, &varid );
	plhs[0] = mexncCreateDoubleScalar(varid);
	plhs[1] = mexncCreateDoubleScalar(status);
	return;

}











/*
 * HANDLE_NC_OPEN:
 *
 * code for handling the nc_open routine.
 *
 * */
void   handle_nc_open ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;

        /*
         * file and dimension IDs 
         * */
        int ncid;


	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;


	/*
	 * path of netcdf file
	 * */
	char	*netcdf_filename;



	/*
	 * NetCDF file opening mode
	 * */
	int	 mode;






	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsChar(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"NetCDF file name argument must be of type matlab char, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}

	


	/*
	 * Unfortunately, the mode argument has historically been optional, 
	 * defaulting to NC_NOCLOBBER.
	 * */
	if ( nrhs == 3 ) {
		if ( mxIsChar(prhs[2]) ) {
			mode = Parameter ( prhs[2] ) ;
		} else if ( mxIsDouble(prhs[2]) ) {
			pr = mxGetPr ( prhs[2] );
			mode = (int) pr[0];
		} else {
		        sprintf ( error_message, 
					"operation \"%s\":  mode argument must be either character or matlab double precision, line %d file \"%s\"\n", 
					nc_op->opname, __LINE__, __FILE__ );
		        mexErrMsgTxt ( error_message );
		        return;
		}
	} else {
		mode = NC_NOCLOBBER;
	}


			
	netcdf_filename = Mat2Str(prhs[1]);
	
	status = nc_open(netcdf_filename, mode, &ncid);
	plhs[0] = mexncCreateDoubleScalar ( ncid );
	plhs[1] = mexncCreateDoubleScalar ( status );

			
	return;

}









/*
 * HANDLE_NC_REDEF:
 *
 * code for handling the nc_redef routine.
 *
 * */
void   handle_nc_redef ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;

        /*
         * file and variable id
         * */
        int ncid;


	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;




	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be of type matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}

	

	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	status = nc_redef ( ncid );
	plhs[0] = mexncCreateDoubleScalar ( status );


	return;

}










/*
 * HANDLE_NC_RENAME_ATT:
 *
 * code for handling the nc_rename_att routine.
 *
 * */
void   handle_nc_rename_att ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;

        /*
         * file and dimension IDs 
         * */
        int ncid;
        int varid;


	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;


	/*
	 * used to rename attributes
	 * */
	char	*old_att_name;
	char	*new_att_name;




	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be of type matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}

	

	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"varid argument must be of type matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}

	

	if ( mxIsChar(prhs[3]) == false ) {
	        sprintf ( error_message, 
				"old attribute name argument must be of type matlab char, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}

	

	if ( mxIsChar(prhs[4]) == false ) {
	        sprintf ( error_message, 
				"new attribute name argument must be of type matlab char, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}

	



	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	varid = (int)(pr[0]);
	old_att_name = Mat2Str(prhs[3]);
	new_att_name = Mat2Str(prhs[4]);
	status = nc_rename_att ( ncid, varid, old_att_name, new_att_name );
	plhs[0] = mexncCreateDoubleScalar ( status );


	return;

}













/*
 * HANDLE_NC_RENAME_DIM:
 *
 * code for handling the nc_rename_dim routine.
 *
 * */
void   handle_nc_rename_dim ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;

        /*
         * file and dimension IDs 
         * */
        int ncid;
        int dimid;


	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;


	char	*new_dimension_name;





	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be of type matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}

	

	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"dimid argument must be of type matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}

	

	if ( mxIsChar(prhs[3]) == false ) {
	        sprintf ( error_message, 
				"dimension name argument must be of type matlab native character, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}

	

	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	dimid = (int)(pr[0]);
	new_dimension_name = Mat2Str(prhs[3]);
	status = nc_rename_dim ( ncid, dimid, new_dimension_name );
	plhs[0] = mexncCreateDoubleScalar ( status );
	return;

}







/*
 * HANDLE_NC_RENAME_VAR:
 *
 * code for handling the nc_rename_var routine.
 *
 * */
void   handle_nc_rename_var ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;

        /*
         * file and variable IDs 
         * */
        int ncid;
        int varid;


	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;


	char	*new_variable_name;





	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be of type matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}

	

	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"dimid argument must be of type matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}

	

	if ( mxIsChar(prhs[3]) == false ) {
	        sprintf ( error_message, 
				"variable name argument must be of type matlab native character, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}

	

	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	pr = mxGetPr ( prhs[2] );
	varid = (int)(pr[0]);

	new_variable_name = Mat2Str(prhs[3]);
	status = nc_rename_var(ncid, varid, new_variable_name);
	plhs[0] = mexncCreateDoubleScalar(status);


	return;







}






/*
 * HANDLE_NC_SET_FILL:
 *
 * code for handling the nc_set_fill routine.
 *
 * */
void   handle_nc_set_fill ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;

        /*
         * file and variable IDs 
         * */
        int ncid;


	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;



	int new_fill_mode;
	int old_fill_mode;




	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be of type matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}

	

	if ( mxIsDouble(prhs[2]) == false ) {
	        sprintf ( error_message, 
				"new fill mode argument must be of type matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}

	

	

	pr = mxGetPr(prhs[1]);
	ncid = (int)(pr[0]);
	pr = mxGetPr(prhs[2]);
	new_fill_mode = (int) pr[0];
	status = nc_set_fill ( ncid, new_fill_mode, &old_fill_mode );
	plhs[0] = mexncCreateDoubleScalar(old_fill_mode);
	plhs[1] = mexncCreateDoubleScalar(status);
			
	return;







}







/*
 * HANDLE_NC_STRERROR:
 *
 * code for handling the nc_strerror routine.
 *
 * */
void   handle_nc_strerror ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;





	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"status argument must be of type matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}

	


	pr = mxGetPr(prhs[1]);
	status = (int) pr[0];
	plhs[0] = mxCreateString ( nc_strerror( status ) );
	return;







}








/*
 * HANDLE_NC_SYNC:
 *
 * code for handling the nc_sync routine.
 *
 * */
void   handle_nc_sync ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


	/* 
	 * netcdf file id
	 * */
	int      ncid;

	/*
	 * Return value 
	 * */
	int status;





	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsDouble(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"ncid argument must be of type matlab native double precision, operation \"%s\", line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}

	


	pr = mxGetPr ( prhs[1] );
	ncid = (int)(pr[0]);
	status = nc_sync(ncid);
	plhs[0] = mexncCreateDoubleScalar(status);
			
			
	return;







}








/*
 * HANDLE_NC__OPEN:
 *
 * code for handling the nc__open routine.
 *
 * */
void   handle_nc__open ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], op *nc_op ) {

	char    error_message[1000];

	/*
	 * Pointer shortcut to matrix data.
	 * */
	double *pr;


        /*
         * file and variable IDs used in nc_copy_att
         * */
        int ncid;

	/* 
	 * Return status from netcdf operation.  
	 * */
	int      status;

	/*
	 * NetCDF file creation mode.
	 * */
	int cmode;


	/*
	 * See the man page for a description of chunksize.
	 * */
	size_t chunksizehint;


	/*
	 * path of NetCDF file
	 * */
	char    *path;


	/*
	 * Make sure that the inputs are the right type.
	 * */
	if ( mxIsChar(prhs[1]) == false ) {
	        sprintf ( error_message, 
				"operation \"%s\":  netcdf file argument must be character, line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	
	if ( !((mxIsChar(prhs[2]) == true) || (mxIsDouble(prhs[2]) == true )) ) {
	        sprintf ( error_message, 
				"operation \"%s\":  mode argument must be either character or numeric, line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	}
	if ( mxIsDouble(prhs[3]) == false ) {
	        sprintf ( error_message, 
				"operation \"%s\":  chunksize argument must be double precision, line %d file \"%s\"\n", 
				nc_op->opname, __LINE__, __FILE__ );
	        mexErrMsgTxt ( error_message );
	        return;
	}
	

	

	
	path = Mat2Str(prhs[1]);
	pr = mxGetPr ( prhs[2] );
	cmode = (int) pr[0];
	pr = mxGetPr ( prhs[3] );
	chunksizehint = (size_t)(pr[0]);
            
            
	status = nc__open ( path, cmode, &chunksizehint, &ncid );
	plhs[0] = mexncCreateDoubleScalar ( ncid );
	plhs[1] = mexncCreateDoubleScalar ( chunksizehint );
	plhs[2] = mexncCreateDoubleScalar ( status );
            
	return;

}







/*
 * DETERMINE_VARM_OUTPUT_SIZE
 *
 * The get_varm and put_varm routines are unusual in that the size of 
 * the resulting output array do not necessarily match what that of
 * the nc_count_coord array.
 *
 * The size of each edge depends upon computing the ratio of the two
 * smallest edges of the imap vector.  It has to be that way, if you think
 * about the definition of the imap vector.  For example, suppose that the
 * netcdf variable is 8x6x4 in size and that the imap vector is [24 4 1].
 * This trivially maps the output into an 8x6x4 output array, but it's a
 * good example, nevertheless.
 *
 * There are 192 elements to be retrieved, but the first imap element of
 * 24 means that there is a distance of 
 * So the output size is [8 6 4].
 *
 * As a second example, suppose we have the same 8x6x4 array, but the
 * imap vector is [1 8 48].  
 *
 * Ex. #2 NetCDF size = [5 4 3 2].
 *        Want the trivial matlab size of [5 4 3 2].
 *        Think in terms of the transpose, [2 3 4 5].
 *        Imap vector is [1 5 20 60]
 *
 * Not sure why this works, exactly.  It makes sense for the trivial
 * mapping, but hard to figure otherwise.
 *
 * */
void 	determine_varm_output_size ( 
		int        ndims, 
		int        num_requested_elements, 
		size_t    *nc_count_coord, 
		ptrdiff_t *nc_stride_coord, 
		ptrdiff_t *nc_imap_coord, 
		int       *result_size )
{


	/*
	 * Loop index
	 * */
	int j, k;


	char	error_message[1000];


	/*
	 * If an element in this array is flagged, it means that we 
	 * have already figured its contribution.
	 * */
	int still_unused[MAX_NC_DIMS];


	/*
	 * Keeps track of the largest remaining imap coordinate for
	 * each dimension.
	 * */
	ptrdiff_t max_imap_element_size;
	ptrdiff_t max_imap_element_index;


	/*
	 * Initialize the flag array.
	 * */
	for ( j = 0; j < MAX_NC_DIMS; ++j ) {
		still_unused[j] = 1;
	}


	/*
	 * For each dimension, figure the contribution.
	 * */
	for ( j = 0; j < ndims; ++j ) {

		/*
		 * Find the largest remaining imap element.
		 * */
		max_imap_element_size = -1; 
		for ( k = 0; k < ndims; ++k ) { 
			if ( (nc_imap_coord[k] > max_imap_element_size) && ( still_unused[k] ) ) {
				max_imap_element_index = k;
				max_imap_element_size = nc_imap_coord[k];
			}
		}


		/*
		 * Figure the contribution for this dimension.
		 * Remember to reverse the order, otherwise the row-major order
		 * and column-major order issue will get us.:W
		 * */
		result_size[j] = num_requested_elements / nc_imap_coord [ max_imap_element_index ];

		/*
		 * Reduce the dimensionality.
		 * */
		num_requested_elements = nc_imap_coord [ max_imap_element_index ];


		/*
		 * We are done with this imap coordinate.  Mark it as used.
		 * */
		still_unused[max_imap_element_index] = 0;

	}


	/*
	 * Check that no elements in result_size are non-positive.
	 * That can crash matlab hard.
	 * */
	for ( j = 0; j < ndims; ++j ) {
		if  ( result_size[j] < 1 ) {

			sprintf ( error_message, "Requested data extent is invalid.\n\n" );
			mexPrintf ( error_message );

			mexPrintf ( "count " );
			for ( j = 0; j < ndims; ++j ) {
				printf ( "[%d]", nc_count_coord[j] );
			}
			mexPrintf ( "\n\n" );
	
	
			mexPrintf ( "stride " );
			for ( j = 0; j < ndims; ++j ) {
				printf ( "[%d]", nc_stride_coord[j] );
			}
			mexPrintf ( "\n\n" );
	
	
			mexPrintf ( "imap " );
			for ( j = 0; j < ndims; ++j ) {
				printf ( "[%d]", nc_imap_coord[j] );
			}
			mexPrintf ( "\n\n" );
	
			mexPrintf ( "result_size " );
			for ( j = 0; j < ndims; ++j ) {
				printf ( "[%d]", result_size[j] );
			}
			mexPrintf ( "\n\n" );
	
			mexErrMsgTxt ( "\n\n" );

		}
	}


}







