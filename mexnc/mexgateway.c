/**********************************************************************
 *
 * mexgateway.c
 *
 * This file functions as the mex-file entry point.  The intended mexnc
 * operation is gleaned from the first argument, and then we transfer
 * control to the source file that handles either the NetCDF-2 or
 * NetCDF-3 API.
 *
 *********************************************************************/

/*
 * $Id: mexgateway.c 1325 2006-05-25 15:34:26Z johnevans007 $
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
 * The Name tag tells which release of mexnc was checked out via CVS
 * and compiled (but only if the release was specifically named).
 * By running the command
 *
 * strings mexnc.mex* | grep "Name:"
 *
 * one should clearly see the release number.
 *
 * */
static char *mexnc_date_id="$Date: 2006-05-25 11:34:26 -0400 (Thu, 25 May 2006) $";
static char *mexnc_release_id="$Name$";



op    *opname2opcode ( const mxArray *, int, int );
void   get_mexnc_info ( int nlhs, mxArray *plhs[], int nrhs, const mxArray *prhs[], OPCODE );	


void mexFunction ( int nlhs, mxArray *plhs[], int nrhs, const mxArray	*prhs[] ) {

	op   *nc_op;

	/*
	 * loop index
	 * */
	int              j;



	char	error_message[1000];





	/*	Disable the NC_FATAL option from ncopts.	*/
	
	if (ncopts & NC_FATAL)	{
		ncopts -= NC_FATAL;
	}
	
	/*	Display usage if less than one input argument.	*/
	
	if (nrhs < 1)	{
	
		Usage();
		
		return;
	}


	/*
	 * Make sure the first argument is not the empty set.
	 * */
	if ( single_matrix_input_is_empty_set ( prhs[0] ) ) {
		mexErrMsgTxt ( "cannot have empty set in first input position.\n\0" );
	}

	
	nc_op = opname2opcode ( prhs[0], nlhs, nrhs );
	

	/*
	 * Now make sure that none of the other arguments are the
	 * empty set.  We need to know the name of the netcdf operation
	 * before we can do this, since a few of the netcdf-2 functions
	 * do actually allow for the empty set.  If there are any illegal 
	 * empty set arguments, then an exception is thrown.
	 *
	 * */
	check_other_args_for_empty_set ( nc_op, prhs, nrhs );


	
	/*	Perform the NetCDF operation.	*/
	
	switch ( nc_op->opcode)	{

		case GET_MEXNC_INFO:
			plhs[0] = mxCreateString ( mexnc_release_id );
			plhs[1] = mxCreateString ( mexnc_date_id );
			break;

		
		/*
		 * Handle the NetCDF-3 operations
		 * */
		case ABORT:
		case CLOSE:
		case COPY_ATT: 
		case _CREATE:
		case CREATE:
		case DEF_DIM:
		case DEF_VAR:
		case DEL_ATT:
		case _ENDDEF:
		case END_DEF:
		case ENDDEF:
		case GET_ATT_DOUBLE:
		case GET_ATT_FLOAT:
		case GET_ATT_INT:
		case GET_ATT_SHORT:
		case GET_ATT_SCHAR:
		case GET_ATT_UCHAR:
		case GET_ATT_TEXT:
		case GET_VAR_DOUBLE:
		case GET_VAR_FLOAT:
		case GET_VAR_INT:
		case GET_VAR_SHORT:
		case GET_VAR_SCHAR:
		case GET_VAR_UCHAR:
		case GET_VAR_TEXT:
		case GET_VAR1_DOUBLE:
		case GET_VAR1_FLOAT:
		case GET_VAR1_INT:
		case GET_VAR1_SHORT:
		case GET_VAR1_SCHAR:
		case GET_VAR1_UCHAR:
		case GET_VAR1_TEXT:
		case GET_VARA_DOUBLE:
		case GET_VARA_FLOAT:
		case GET_VARA_INT:
		case GET_VARA_SHORT:
		case GET_VARA_SCHAR:
		case GET_VARA_UCHAR:
		case GET_VARA_TEXT:
		case GET_VARS_DOUBLE:
		case GET_VARS_FLOAT:
		case GET_VARS_INT:
		case GET_VARS_SHORT:
		case GET_VARS_SCHAR:
		case GET_VARS_UCHAR:
		case GET_VARS_TEXT:
		case GET_VARM_DOUBLE:
		case GET_VARM_FLOAT:
		case GET_VARM_INT:
		case GET_VARM_SHORT:
		case GET_VARM_SCHAR:
		case GET_VARM_UCHAR:
		case GET_VARM_TEXT:
		case INQ:
		case INQ_NDIMS:
		case INQ_NVARS:
		case INQ_NATTS:
		case INQ_ATT:
		case INQ_ATTID:
		case INQ_ATTLEN:
		case INQ_ATTNAME:
		case INQ_ATTTYPE:
		case INQ_DIM:
		case INQ_DIMID:
		case INQ_DIMNAME:
		case INQ_DIMLEN:
		case INQ_LIBVERS:
		case INQ_VAR:
		case INQ_VARNAME:
		case INQ_VARTYPE:
		case INQ_VARNDIMS:
		case INQ_VARDIMID:
		case INQ_VARNATTS:
		case INQ_UNLIMDIM:
		case INQ_VARID:
		case _OPEN:
		case OPEN:
		case PUT_ATT_DOUBLE:
		case PUT_ATT_FLOAT:
		case PUT_ATT_INT:
		case PUT_ATT_SHORT:
		case PUT_ATT_SCHAR:
		case PUT_ATT_UCHAR:
		case PUT_ATT_TEXT:
		case PUT_VAR_DOUBLE:
		case PUT_VAR_FLOAT:
		case PUT_VAR_INT:
		case PUT_VAR_SHORT:
		case PUT_VAR_SCHAR:
		case PUT_VAR_UCHAR:
		case PUT_VAR_TEXT:
		case PUT_VARA_DOUBLE:
		case PUT_VARA_FLOAT:
		case PUT_VARA_INT:
		case PUT_VARA_SHORT:
		case PUT_VARA_SCHAR:
		case PUT_VARA_UCHAR:
		case PUT_VARA_TEXT:
		case PUT_VARS_DOUBLE:
		case PUT_VARS_FLOAT:
		case PUT_VARS_INT:
		case PUT_VARS_SHORT:
		case PUT_VARS_SCHAR:
		case PUT_VARS_UCHAR:
		case PUT_VARS_TEXT:
		case PUT_VARM_DOUBLE:
		case PUT_VARM_FLOAT:
		case PUT_VARM_INT:
		case PUT_VARM_SHORT:
		case PUT_VARM_SCHAR:
		case PUT_VARM_UCHAR:
		case PUT_VARM_TEXT:
		case PUT_VAR1_DOUBLE:
		case PUT_VAR1_FLOAT:
		case PUT_VAR1_INT:
		case PUT_VAR1_SHORT:
		case PUT_VAR1_SCHAR:
		case PUT_VAR1_UCHAR:
		case PUT_VAR1_TEXT:
		case REDEF:
		case RENAME_ATT:
		case RENAME_DIM:
		case RENAME_VAR:
		case SET_FILL:
		case STRERROR:
		case SYNC:
			handle_netcdf3_api ( nlhs, plhs, nrhs, prhs, nc_op );	
			break;
			
			
			
			

			
		/*
		 * Ok these are all the NetCDF 2.4 API calls.  Keep'em locked 
		 * away in the attic.
		 * */
		case ATTCOPY:
		case ATTDEL:
		case ATTGET:
		case ATTINQ:
		case ATTNAME:
		case ATTRENAME:
		case ATTPUT:
		case DIMDEF:
		case DIMID:
		case DIMINQ:
		case DIMRENAME:
		case ENDEF:
		case ERR:
		case INQUIRE:
		case PARAMETER:
		case RECPUT:
		case RECGET:
		case RECINQ:
		case SETFILL:
		case SETOPTS:
		case TYPELEN:
		case VARCOPY:
		case VARDEF:
		case VARGET:
		case VARGET1:
		case VARGETG:
		case VARID:
		case VARINQ:
		case VARPUT:
		case VARPUT1:
		case VARPUTG:
		case VARRENAME:

			handle_netcdf2_api ( nlhs, plhs, nrhs, prhs, nc_op );	
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
 * This function transforms the implied name of a netcdf function 
 * (such as "nc_open") into an enumerated type (such as OPEN) that
 * is more readily handled by switch statements.  We also check to 
 * make sure that the right number of inputs and outputs are specified.
 * */
op *opname2opcode ( const mxArray *opname_matrix, int nlhs, int nrhs ) {

	static op ops[] =	{
		{ ABORT,            "abort",            2, 1 },
		{ CLOSE,            "close",            2, 1 },
		{ COPY_ATT,         "copy_att",         6, 1 }, 
		{ _CREATE,          "_create",          4, 3 }, 
		{ CREATE,           "create",           2, 2 }, 
		{ DEF_DIM,          "def_dim",          4, 1 }, 
		{ DEF_VAR,          "def_var",          6, 1 }, 
		{ DEL_ATT,          "del_att",          4, 1 }, 
		{ _ENDDEF,          "_enddef",          5, 1 }, 
		{ END_DEF,          "end_def",          1, 1 }, 
		{ ENDDEF,           "enddef",           1, 1 }, 
		{ GET_ATT_DOUBLE,   "get_att_double",   4, 2 }, 
		{ GET_ATT_FLOAT,    "get_att_float",    4, 2 }, 
		{ GET_ATT_INT,      "get_att_int",      4, 2 }, 
		{ GET_ATT_SHORT,    "get_att_short",    4, 2 }, 
		{ GET_ATT_SCHAR,    "get_att_schar",    4, 2 }, 
		{ GET_ATT_UCHAR,    "get_att_uchar",    4, 2 }, 
		{ GET_ATT_TEXT,     "get_att_text",     4, 2 }, 
		{ GET_MEXNC_INFO,   "get_mexnc_info",   1, 2 }, 
		{ GET_VAR_DOUBLE,   "get_var_double",   3, 2 }, 
		{ GET_VAR_FLOAT,    "get_var_float",    3, 2 }, 
		{ GET_VAR_INT,      "get_var_int",      3, 2 }, 
		{ GET_VAR_SHORT,    "get_var_short",    3, 2 }, 
		{ GET_VAR_SCHAR,    "get_var_schar",    3, 2 }, 
		{ GET_VAR_UCHAR,    "get_var_uchar",    3, 2 }, 
		{ GET_VAR_TEXT,     "get_var_text",     3, 2 }, 
		{ GET_VAR1_DOUBLE,  "get_var1_double",  4, 2 }, 
		{ GET_VAR1_FLOAT,   "get_var1_float",   4, 2 }, 
		{ GET_VAR1_INT,     "get_var1_int",     4, 2 }, 
		{ GET_VAR1_SHORT,   "get_var1_short",   4, 2 }, 
		{ GET_VAR1_SCHAR,   "get_var1_schar",   4, 2 }, 
		{ GET_VAR1_UCHAR,   "get_var1_uchar",   4, 2 }, 
		{ GET_VAR1_TEXT,    "get_var1_text",    4, 2 }, 
		{ GET_VARA_DOUBLE,  "get_vara_double",  5, 2 }, 
		{ GET_VARA_FLOAT,   "get_vara_float",   5, 2 }, 
		{ GET_VARA_INT,     "get_vara_int",     5, 2 }, 
		{ GET_VARA_SHORT,   "get_vara_short",   5, 2 }, 
		{ GET_VARA_SCHAR,   "get_vara_schar",   5, 2 }, 
		{ GET_VARA_UCHAR,   "get_vara_uchar",   5, 2 }, 
		{ GET_VARA_TEXT,    "get_vara_text",    5, 2 }, 
		{ GET_VARS_DOUBLE,  "get_vars_double",  6, 2 }, 
		{ GET_VARS_FLOAT,   "get_vars_float",   6, 2 }, 
		{ GET_VARS_INT,     "get_vars_int",     6, 2 }, 
		{ GET_VARS_SHORT,   "get_vars_short",   6, 2 }, 
		{ GET_VARS_SCHAR,   "get_vars_schar",   6, 2 }, 
		{ GET_VARS_UCHAR,   "get_vars_uchar",   6, 2 }, 
		{ GET_VARS_TEXT,    "get_vars_text",    6, 2 }, 
		{ GET_VARM_DOUBLE,  "get_varm_double",  7, 2 }, 
		{ GET_VARM_FLOAT,   "get_varm_float",   7, 2 }, 
		{ GET_VARM_INT,     "get_varm_int",     7, 2 }, 
		{ GET_VARM_SHORT,   "get_varm_short",   7, 2 }, 
		{ GET_VARM_SCHAR,   "get_varm_schar",   7, 2 }, 
		{ GET_VARM_UCHAR,   "get_varm_uchar",   7, 2 }, 
		{ GET_VARM_TEXT,    "get_varm_text",    7, 2 }, 
		{ INQ,              "inq",              2, 5 }, 
		{ INQ_ATT,          "inq_att",          4, 3 }, 
		{ INQ_ATTID,        "inq_attid",        4, 2 }, 
		{ INQ_ATTLEN,       "inq_attlen",       4, 2 }, 
		{ INQ_ATTNAME,      "inq_attname",      4, 2 }, 
		{ INQ_ATTTYPE,      "inq_atttype",      4, 2 }, 
		{ INQ_DIM,          "inq_dim",          3, 3 }, 
		{ INQ_DIMID,        "inq_dimid",        3, 1 }, 
		{ INQ_DIMLEN,       "inq_dimlen",       3, 2 }, 
		{ INQ_DIMNAME,      "inq_dimname",      3, 2 }, 
		{ INQ_LIBVERS,      "inq_libvers",      1, 1 }, 
		{ INQ_NDIMS,        "inq_ndims",        2, 2 }, 
		{ INQ_NVARS,        "inq_nvars",        2, 2 }, 
		{ INQ_NATTS,        "inq_natts",        2, 2 }, 
		{ INQ_UNLIMDIM,     "inq_unlimdim",     1, 2 }, 
		{ INQ_VARID,        "inq_varid",        3, 1 }, 
		{ INQ_VAR,          "inq_var",          3, 5 }, 
		{ INQ_VARNAME,      "inq_varname",      3, 2 }, 
		{ INQ_VARTYPE,      "inq_vartype",      3, 2 }, 
		{ INQ_VARNDIMS,     "inq_varndims",     3, 2 }, 
		{ INQ_VARDIMID,     "inq_vardimid",     3, 2 }, 
		{ INQ_VARNATTS,     "inq_varnatts",     3, 2 }, 
		{ _OPEN,            "_open",            4, 3 }, 
		{ OPEN,             "open",             2, 2 }, 
		{ PUT_ATT_DOUBLE,   "put_att_double",   7, 1 }, 
		{ PUT_ATT_FLOAT,    "put_att_float",    7, 1 }, 
		{ PUT_ATT_INT,      "put_att_int",      7, 1 }, 
		{ PUT_ATT_SHORT,    "put_att_short",    7, 1 }, 
		{ PUT_ATT_SCHAR,    "put_att_schar",    7, 1 }, 
		{ PUT_ATT_UCHAR,    "put_att_uchar",    7, 1 }, 
		{ PUT_ATT_TEXT,     "put_att_text",     7, 1 }, 
		{ PUT_VAR_DOUBLE,   "put_var_double",   4, 1 }, 
		{ PUT_VAR_FLOAT,    "put_var_float",    4, 1 }, 
		{ PUT_VAR_INT,      "put_var_int",      4, 1 }, 
		{ PUT_VAR_SHORT,    "put_var_short",    4, 1 }, 
		{ PUT_VAR_SCHAR,    "put_var_schar",    4, 1 }, 
		{ PUT_VAR_UCHAR,    "put_var_uchar",    4, 1 }, 
		{ PUT_VAR_TEXT,     "put_var_text",     4, 1 }, 
		{ PUT_VARA_DOUBLE,  "put_vara_double",  6, 1 }, 
		{ PUT_VARA_FLOAT,   "put_vara_float",   6, 1 }, 
		{ PUT_VARA_INT,     "put_vara_int",     6, 1 }, 
		{ PUT_VARA_SHORT,   "put_vara_short",   6, 1 }, 
		{ PUT_VARA_SCHAR,   "put_vara_schar",   6, 1 }, 
		{ PUT_VARA_UCHAR,   "put_vara_uchar",   6, 1 }, 
		{ PUT_VARA_TEXT,    "put_vara_text",    6, 1 }, 
		{ PUT_VARS_DOUBLE,  "put_vars_double",  7, 1 }, 
		{ PUT_VARS_FLOAT,   "put_vars_float",   7, 1 }, 
		{ PUT_VARS_INT,     "put_vars_int",     7, 1 }, 
		{ PUT_VARS_SHORT,   "put_vars_short",   7, 1 }, 
		{ PUT_VARS_SCHAR,   "put_vars_schar",   7, 1 }, 
		{ PUT_VARS_UCHAR,   "put_vars_uchar",   7, 1 }, 
		{ PUT_VARS_TEXT,    "put_vars_text",    7, 1 }, 
		{ PUT_VARM_DOUBLE,  "put_varm_double",  8, 1 }, 
		{ PUT_VARM_FLOAT,   "put_varm_float",   8, 1 }, 
		{ PUT_VARM_INT,     "put_varm_int",     8, 1 }, 
		{ PUT_VARM_SHORT,   "put_varm_short",   8, 1 }, 
		{ PUT_VARM_SCHAR,   "put_varm_schar",   8, 1 }, 
		{ PUT_VARM_UCHAR,   "put_varm_uchar",   8, 1 }, 
		{ PUT_VARM_TEXT,    "put_varm_text",    8, 1 }, 
		{ PUT_VAR1_DOUBLE,  "put_var1_double",  5, 1 }, 
		{ PUT_VAR1_FLOAT,   "put_var1_float",   5, 1 }, 
		{ PUT_VAR1_INT,     "put_var1_int",     5, 1 }, 
		{ PUT_VAR1_SHORT,   "put_var1_short",   5, 1 }, 
		{ PUT_VAR1_SCHAR,   "put_var1_schar",   5, 1 }, 
		{ PUT_VAR1_UCHAR,   "put_var1_uchar",   5, 1 }, 
		{ PUT_VAR1_TEXT,    "put_var1_text",    5, 1 }, 
		{ REDEF,            "redef",            1, 1 }, 
		{ RENAME_ATT,       "rename_att",       5, 1 }, 
		{ RENAME_DIM,       "rename_dim",       4, 1 }, 
		{ RENAME_VAR,       "rename_var",       4, 1 }, 
		{ SET_FILL,         "set_fill",         3, 2 }, 
		{ STRERROR,         "strerror",         1, 1 }, 
		{ SYNC,             "sync",             2, 1 }, 
	
		/*
		 * Deprecated Netcdf 2.4
		 * */
		{ DIMDEF,           "dimdef",           4, 1 }, 
		{ DIMID,            "dimid",            3, 1 }, 
		{ DIMINQ,           "diminq",           3, 3 }, 
		{ DIMRENAME,        "dimrename",        4, 1 }, 
		{ ENDEF,            "endef",            1, 1 }, 
		{ INQUIRE,          "inquire",          2, 5 }, 
		{ VARDEF,           "vardef",           6, 1 }, 
		{ VARID,            "varid",            3, 1 }, 
		{ VARINQ,           "varinq",           3, 5 }, 
	
		{ VARPUT1,          "varput1",          5, 1 }, 
		{ VARGET1,          "varget1",          4, 2 }, 
		{ VARPUT,           "varput",           6, 1 }, 
		{ VARGET,           "varget",           5, 2 }, 
		{ VARPUTG,          "varputg",          7, 1 }, 
		{ VARGETG,          "vargetg",          6, 2 }, 
		{ VARRENAME,        "varrename",        4, 1 }, 
		{ VARCOPY,          "varcopy",          3, 2 }, 
		{ ATTPUT,           "attput",           7, 1 }, 
		{ ATTINQ,           "attinq",           4, 3 }, 
		{ ATTGET,           "attget",           4, 2 }, 
		{ ATTCOPY,          "attcopy",          6, 1 }, 
		{ ATTNAME,          "attname",          4, 2 }, 
		{ ATTRENAME,        "attrename",        5, 1 }, 
		{ ATTDEL,           "attdel",           4, 1 }, 
		{ RECPUT,           "recput",           4, 1 }, 
		{ RECGET,           "recget",           3, 2 }, 
		{ RECINQ,           "recinq",           2, 3 }, 
		{ TYPELEN,          "typelen",          2, 2 }, 
		{ SETFILL,          "setfill",          3, 2 }, 
		{ SETOPTS,          "setopts",          2, 1 }, 
		{ ERR,              "err",              1, 1 }, 
		{ PARAMETER,        "parameter",        1, 1 }, 
		{ NONE,             "none",             0, 0 }
	};

	OPCODE   opcode;
	char    *opname;
	int      j;

	/*
	 * Used to access the opcode portions.
	 * */
	char		*	p;

	char	error_message[1000];

	opname = Mat2Str(opname_matrix);

	/*	Convert the operation name to its opcode.	*/
	for (j = 0; j < strlen(opname); j++)	{
		opname[j] = (char) tolower((int) opname[j]);
	}
	p = opname;
	if (strncmp(p, "nc", 2) == 0)	{	/*	Trim away "nc".	*/
		p += 2;
	}
	
	j = 0;
	opcode = NONE;
	while (ops[j].opcode != NONE)	{
		if (!strcmp(p, ops[j].opname))	{
			opcode = ops[j].opcode;
			if (ops[j].nrhs > nrhs)	{
				sprintf ( error_message, "MEXNC: opname = %s, too few input arguments (%d), we need at least %d.\n", opname, nrhs, ops[j].nrhs );
				mexErrMsgTxt(error_message);
			}
			else if (0 && ops[j].nlhs > nlhs)	{	/*	Disabled.	*/
				sprintf ( error_message, "MEXNC: opname = %s, too few output arguments (%d), we need at least %d.\n", opname, nlhs, ops[j].nlhs );
				mexErrMsgTxt(error_message);
			}
			break;
		}
		else	{
			j++;
		}
	}
	
	if (opcode == NONE)	{
		sprintf ( error_message, "MEXNC: opname = %s, no such operation.\n", opname );
		mexErrMsgTxt(error_message);
	}
	
	return ( & ops[j] );


}










/*
 * single_matrix_input_is_empty_set
 * 
 * Passing in the empty set as an input will generally cause 
 * segmentation faults.  This can actually arise more than 
 * one would think, especially as matlab guis fall into the hands of
 * the unwise.
 * */
int single_matrix_input_is_empty_set ( const mxArray *input ) {

	/*
	 * These are the number of rows and columns of the input.
	 * If it is zero, then we know we have the empty set.
	 * */
	int m, n;

	m = mxGetM ( input );
	n = mxGetN ( input );

	if ( (m*n) == 0 ) {

		/*
		 * Yeah it's the empty set.
		 * */
		return ( 1 );

	} else {

		/*
		 * No it is not the empty set.
		 * */
		return ( 0 );

	}
}







/*
 * After the first argument has been checked to see that it is not
 * the empty set, the op name can be extracted.  If the op name is
 * known, then we can intelligently check the other input arguments
 * to make sure that none of them are the empty set except where
 * specifically allowed.  The reason for the allowed cases aren't 
 * really explained anywhere.  And unfortunately there's already
 * code out there that makes use of this.  What a terrific 
 * situation.  Sloppy.  Damned sloppy.
 * 
 * The empty string is considered different than [], I suppose, so
 * we skip that.
 *
 * */
void check_other_args_for_empty_set ( op *nc_op, const mxArray *mx_input[], int num_inputs ) {

	/*
	 * If we encounter the empty set where it is illegal, say so here.
	 * */
	char error_msg[500];

	/*
	 * Shortcut to the matrix data
	 * */
	double *pr;



	/*
	 * Loop index for matlab array inputs.
	 * */
	int i;

	for ( i = 1; i < num_inputs; ++i ) {


		/*
		 * An empty string is sometimes allowed.
		 * */
		if ( mxIsChar ( mx_input[i] ) ) {
			continue;
		}


		if ( ( nc_op->opcode == ATTPUT ) && ( i == 6 ) ) {
			continue;
		}





		if ( ( nc_op->opcode == DEF_VAR ) && ( i == 5 ) ) {
			continue;
		}

		if ( ( nc_op->opcode == VARDEF ) && ( i == 5 ) ) {
			continue;
		}






		if ( ( nc_op->opcode == VARGETG ) && ( i == 6 ) ) {
			continue;
		}
		if ( ( nc_op->opcode == VARPUTG ) && ( i == 6 ) ) {
			continue;
		}
		if ( ( nc_op->opcode == VARPUTG ) && ( i == 7 ) ) {
			continue;
		}

		if ( ( nc_op->opcode == VARPUT1 ) && ( i == 4 ) ) {
			continue;
		}





		if ( ( nc_op->opcode == VARPUT ) && ( i == 5 ) ) {
			continue;
		}

		/*
		 * The test we use is whether the matrix size is zero.
		 * If so, then assume that we've got [] as an input.
		 * */
		pr = mxGetPr ( mx_input[i] );
		if ( pr == NULL ) {
			/*
			 * Yeah it's the empty set.
			 * */
			sprintf ( error_msg, "%s:  cannot have empty set in input position %d.\n", nc_op->opname, i+1 );
			mexErrMsgTxt ( error_msg );
		}

	}




}


