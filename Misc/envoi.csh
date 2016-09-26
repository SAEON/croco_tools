#!/bin/csh
if ($#argv != 1) then
   echo Usage: $0 directory file
   exit 127
endif
#
set file = $1
set REMOTE_HOST = XXX
set REMOTE_DIR = XXX
ftp -n $REMOTE_HOST << EOF
user anonymous toto@
cd $REMOTE_DIR
put $file
bye
