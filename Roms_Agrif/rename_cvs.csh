#!/bin/tcsh

set old_file=t3dbc_im.F
set new_file=t3dbc.F

rm -f $old_file
cvs remove $old_file
cvs add $new_file
