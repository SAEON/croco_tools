/******************************************************************************/
/*                                                                            */
/*     CONV (converter) for Agrif (Adaptive Grid Refinement In Fortran)       */
/*                                                                            */
/* Copyright or   or Copr. Laurent Debreu (Laurent.Debreu@imag.fr)            */
/*                        Cyril Mazauric (Cyril_Mazauric@yahoo.fr)            */
/* This software is governed by the CeCILL-C license under French law and     */
/* abiding by the rules of distribution of free software.  You can  use,      */
/* modify and/ or redistribute the software under the terms of the CeCILL-C   */
/* license as circulated by CEA, CNRS and INRIA at the following URL          */
/* "http://www.cecill.info".                                                  */
/*                                                                            */
/* As a counterpart to the access to the source code and  rights to copy,     */
/* modify and redistribute granted by the license, users are provided only    */
/* with a limited warranty  and the software's author,  the holder of the     */
/* economic rights,  and the successive licensors  have only  limited         */
/* liability.                                                                 */
/*                                                                            */
/* In this respect, the user's attention is drawn to the risks associated     */
/* with loading,  using,  modifying and/or developing or reproducing the      */
/* software by the user in light of its specific status of free software,     */
/* that may mean  that it is complicated to manipulate,  and  that  also      */
/* therefore means  that it is reserved for developers  and  experienced      */
/* professionals having in-depth computer knowledge. Users are therefore      */
/* encouraged to load and test the software's suitability as regards their    */
/* requirements in conditions enabling the security of their systems and/or   */
/* data to be ensured and,  more generally, to use and operate it in the      */
/* same conditions as regards security.                                       */
/*                                                                            */
/* The fact that you are presently reading this means that you have had       */
/* knowledge of the CeCILL-C license and that you accept its terms.           */
/******************************************************************************/
/* version 1.7                                                                */
/******************************************************************************/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "decl.h"

void Save_Length(char *nom, int whichone)
{
    size_t len_nom = strlen(nom);
    
   if ( whichone == 1  && len_nom > length_last )
   {
      length_last = len_nom;
      if ( length_last > LONG_C )
            printf("WARNING 1 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_last+100);
   }
   if ( whichone == 2  && len_nom > length_first )
   {
      length_first = len_nom;
      if ( length_first > LONG_C )
           printf("WARNING 2 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_first+100);
   }
   if ( whichone == 3  && len_nom > length_v_typevar )
   {
      length_v_typevar = len_nom;
      if ( length_v_typevar > LONG_C )
           printf("WARNING 3 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_v_typevar+100);
   }
   if ( whichone == 4  && len_nom > length_v_nomvar )
   {
      length_v_nomvar = len_nom;
      if ( length_v_nomvar > LONG_C )
           printf("WARNING 4 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_v_nomvar+100);
   }
   if ( whichone == 5  && len_nom > length_v_dimchar )
   {
      length_v_dimchar = len_nom;
      if ( length_v_dimchar > LONG_C )
           printf("WARNING 5 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_v_dimchar+100);
   }
   if ( whichone == 6  && len_nom > length_v_modulename )
   {
      length_v_modulename = len_nom;
      if ( length_v_modulename > LONG_C )
           printf("WARNING 6 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_v_modulename+100);
   }
   if ( whichone == 7  && len_nom > length_v_commonname )
   {
      length_v_commonname = len_nom;
      if ( length_v_commonname > LONG_C )
           printf("WARNING 7 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_v_commonname+100);
   }
   if ( whichone == 8  && len_nom > length_v_vallengspec )
   {
      length_v_vallengspec = len_nom;
      if ( length_v_vallengspec > LONG_C )
           printf("WARNING 8 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_v_vallengspec+100);
   }
   if ( whichone == 9  && len_nom > length_v_nameinttypename )
   {
      length_v_nameinttypename = len_nom;
      if ( length_v_nameinttypename > LONG_C )
           printf("WARNING 9 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_v_nameinttypename+100);
   }
   if ( whichone == 10 && len_nom > length_v_commoninfile )
   {
      length_v_commoninfile = len_nom;
      if ( length_v_commoninfile > LONG_C )
           printf("WARNING 10 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_v_commoninfile+100);
   }
   if ( whichone == 11 && len_nom > length_v_subroutinename )
   {
      length_v_subroutinename = len_nom;
      if ( length_v_subroutinename > LONG_C )
           printf("WARNING 11 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_v_subroutinename+100);
   }
   if ( whichone == 12 && len_nom > length_v_precision )
   {
      length_v_precision = len_nom;
      if ( length_v_precision > LONG_C )
           printf("WARNING 12 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_v_precision+100);
   }
   if ( whichone == 13 && len_nom > length_v_IntentSpec )
   {
      length_v_IntentSpec = len_nom;
      if ( length_v_IntentSpec > LONG_C )
           printf("WARNING 13 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_v_IntentSpec+100);
   }
   if ( whichone == 14 && len_nom > length_v_initialvalue )
   {
      length_v_initialvalue = len_nom;
      if ( length_v_initialvalue > LONG_4C )
           printf("WARNING 14 : The value of LONG_4C (defined in decl.h) should be upgrated to %lu\n", length_v_initialvalue+100);
   }
   if ( whichone == 15 && len_nom > length_v_readedlistdimension )
   {
      length_v_readedlistdimension = len_nom;
      if ( length_v_readedlistdimension > LONG_C )
           printf("WARNING 15 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_v_readedlistdimension+100);
   }
   if ( whichone == 16 && len_nom > length_u_usemodule )
   {
      length_u_usemodule = len_nom;
      if ( length_u_usemodule > LONG_C )
           printf("WARNING 16 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_u_usemodule+100);
   }
   if ( whichone == 17 && len_nom > length_u_charusemodule )
   {
      length_u_charusemodule = len_nom;
      if ( length_u_charusemodule > LONG_C )
           printf("WARNING 17 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_u_charusemodule+100);
   }
   if ( whichone == 18 && len_nom > length_u_cursubroutine )
   {
      length_u_cursubroutine = len_nom;
      if ( length_u_cursubroutine > LONG_C )
           printf("WARNING 18 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_u_cursubroutine+100);
   }
   if ( whichone == 19 && len_nom > length_u_modulename )
   {
      length_u_modulename = len_nom;
      if ( length_u_modulename > LONG_C )
           printf("WARNING 19 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_u_modulename+100);
   }
   if ( whichone == 20 && len_nom > length_n_name )
   {
      length_n_name = len_nom;
      if ( length_n_name > LONG_C )
           printf("WARNING 20 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_n_name+100);
   }
   if ( whichone == 21 && len_nom > length_c_namevar )
   {
      length_c_namevar = len_nom;
      if ( length_c_namevar > LONG_C )
           printf("WARNING 21 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_c_namevar+100);
   }
   if ( whichone == 22 && len_nom > length_c_namepointedvar )
   {
      length_c_namepointedvar = len_nom;
      if ( length_c_namepointedvar > LONG_C )
           printf("WARNING 22 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_c_namepointedvar+100);
   }
   if ( whichone == 23 && len_nom > length_o_nom )
   {
      length_o_nom = len_nom;
      if ( length_o_nom > LONG_C )
           printf("WARNING 23 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_o_nom+100);
   }
   if ( whichone == 24 && len_nom > length_o_module )
   {
      length_o_module = len_nom;
      if ( length_o_module > LONG_C )
           printf("WARNING 24 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_o_module+100);
   }
   if ( whichone == 25 && len_nom > length_a_nomvar )
   {
      length_a_nomvar = len_nom;
      if ( length_a_nomvar > LONG_C )
           printf("WARNING 25 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_a_nomvar+100);
   }
   if ( whichone == 26 && len_nom > length_a_subroutine )
   {
      length_a_subroutine = len_nom;
      if ( length_a_subroutine > LONG_C )
           printf("WARNING 26 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_a_subroutine+100);
   }
   if ( whichone == 27 && len_nom > length_a_module )
   {
      length_a_module = len_nom;
      if ( length_a_module > LONG_C )
           printf("WARNING 27 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_a_module+100);
   }
   if ( whichone == 28 && len_nom > length_t_usemodule )
   {
      length_t_usemodule = len_nom;
      if ( length_t_usemodule > LONG_C )
           printf("WARNING 28 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_t_usemodule+100);
   }
   if ( whichone == 29 && len_nom > length_t_cursubroutine )
   {
      length_t_cursubroutine = len_nom;
      if ( length_t_cursubroutine > LONG_C )
           printf("WARNING 29 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_t_cursubroutine+100);
   }
   if ( whichone == 30 && len_nom > length_curfilename )
   {
      length_curfilename = len_nom;
      if (length_curfilename  > LONG_C )
           printf("WARNING 30 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_curfilename+100);
   }
   if ( whichone == 31 && len_nom > length_nomfileoutput )
   {
      length_nomfileoutput = len_nom;
      if ( length_nomfileoutput > LONG_C )
           printf("WARNING 31 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_nomfileoutput+100);
   }
   if ( whichone == 32 && len_nom > length_motparse )
   {
      length_motparse = len_nom;
      if ( length_motparse > LONG_4C )
           printf("WARNING 32 : The value of LONG_4C (defined in decl.h) should be upgrated to %lu\n", length_motparse+100);
   }
   if ( whichone == 33 && len_nom > length_mainfile )
   {
      length_mainfile = len_nom;
      if ( length_mainfile > LONG_C )
           printf("WARNING 33 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_mainfile+100);
   }
   if ( whichone == 34 && len_nom > length_nomdir )
   {
      length_nomdir = len_nom;
      if ( length_nomdir > LONG_C )
           printf("WARNING 34 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_nomdir+100);
   }
   if ( whichone == 35 && len_nom > length_commondirout )
   {
      length_commondirout = len_nom;
      if ( length_commondirout > LONG_C )
           printf("WARNING 35 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_commondirout+100);
   }
   if ( whichone == 36 && len_nom > length_commondirin )
   {
      length_commondirin = len_nom;
      if ( length_commondirin > LONG_C )
           printf("WARNING 36 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_commondirin+100);
   }
   if ( whichone == 37 && len_nom > length_filetoparse )
   {
      length_filetoparse = len_nom;
      if ( length_filetoparse > LONG_C )
           printf("WARNING 37 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_filetoparse+100);
   }
   if ( whichone == 38 && len_nom > length_curbuf )
   {
      length_curbuf = len_nom;
      if ( length_curbuf > LONG_40M )
           printf("WARNING 38 : The value of LONG_40M (defined in decl.h) should be upgrated to %lu\n", length_curbuf+100);
   }
   if ( whichone == 39 && len_nom > length_toprintglob )
   {
      length_toprintglob = len_nom;
      if ( length_toprintglob > LONG_4C )
           printf("WARNING 39 : The value of LONG_4C (defined in decl.h) should be upgrated to %lu\n", length_toprintglob+100);
   }
   if ( whichone == 40 && len_nom > length_tmpvargridname )
   {
      length_tmpvargridname = len_nom;
      if ( length_tmpvargridname > LONG_4C )
           printf("WARNING 40 : The value of LONG_4C (defined in decl.h) should be upgrated to %lu\n", length_tmpvargridname+100);
   }
   if ( whichone == 41 && len_nom > length_ligne_Subloop )
   {
       length_ligne_Subloop = len_nom;
      if ( length_ligne_Subloop > LONG_40M )
           printf("WARNING 41 : The value of LONG_40M (defined in decl.h) should be upgrated to %lu\n",length_ligne_Subloop+100);
   }
   if ( whichone == 42 && len_nom > length_lvargridname_toamr )
   {
      length_lvargridname_toamr = len_nom;
      if ( length_lvargridname_toamr > LONG_4C )
           printf("WARNING 42 : The value of LONG_4C (defined in decl.h) should be upgrated to %lu\n", length_lvargridname_toamr+100);
   }
   if ( whichone == 43 && len_nom > length_toprint_utilagrif )
   {
      length_toprint_utilagrif = len_nom;
      if ( length_toprint_utilagrif > LONG_C )
           printf("WARNING 43 : The value of LONG_C (defined in decl.h) should be upgrated to %lu\n", length_toprint_utilagrif+100);
   }
   if ( whichone == 44 && len_nom > length_toprinttmp_utilchar )
   {
      length_toprinttmp_utilchar = len_nom;
      if ( length_toprinttmp_utilchar > LONG_4C)
           printf("WARNING 44 : The value of LONG_4C (defined in decl.h) should be upgrated to %lu\n", length_toprinttmp_utilchar+100);
   }
   if ( whichone == 45 && len_nom > length_ligne_writedecl )
   {
      length_ligne_writedecl = len_nom;
      if ( length_ligne_writedecl > LONG_4C )
           printf("WARNING 45 : The value of LONG_4C (defined in decl.h) should be upgrated to %lu\n", length_ligne_writedecl+100);
   }
   if ( whichone == 46 && len_nom > length_newname_toamr )
   {
      length_newname_toamr = len_nom;
      if ( length_newname_toamr > LONG_4C )
           printf("WARNING 46 : The value of LONG_4C (defined in decl.h) should be upgrated to %lu\n", length_newname_toamr+100);
   }
   if ( whichone == 47 && len_nom > length_newname_writedecl )
   {
      length_newname_writedecl = len_nom;
      if ( length_newname_writedecl > LONG_4C )
           printf("WARNING 47 : The value of LONG_4C (defined in decl.h) should be upgrated to %lu\n", length_newname_writedecl +100);
   }
   if ( whichone == 48 && len_nom > length_ligne_toamr )
   {
      length_ligne_toamr = len_nom;
      if ( length_ligne_toamr > LONGNOM )
           printf("WARNING 48 : The value of LONGNOM (defined in decl.h) should be upgrated to %lu\n", length_ligne_toamr +100);
   }
   if ( whichone == 49 && len_nom > length_tmpligne_writedecl )
   {
      length_tmpligne_writedecl = len_nom;
      if ( length_tmpligne_writedecl > LONG_4C )
           printf("WARNING 49 : The value of LONG_4C (defined in decl.h) should be upgrated to %lu\n", length_tmpligne_writedecl+100);
   }
}

void Save_Length_int(int val, int whichone)
{
   if ( whichone == 1 && val > value_char_size )    value_char_size  = val;
   if ( whichone == 2 && val > value_char_size1 )   value_char_size1 = val;
   if ( whichone == 3 && val > value_char_size2 )   value_char_size2 = val;
   if ( whichone == 4 && val > value_char_size3 )   value_char_size3 = val;
}
