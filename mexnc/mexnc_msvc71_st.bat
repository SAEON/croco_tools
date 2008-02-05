@echo off
rem MEXNC_MSVC71_ST.BAT
rem
rem    Compile and link options used for building mexnc.dll 
rem    using the Microsoft Visual C++ compiler version 7.1 and Matlab 7 (sp1)
rem    and single threaded NetCDF 3.6.1 library (w/ large file support)  
rem    
rem    
rem    Rich Signell (rsignell@usgs.gov) 15-Jan-2005
rem
rem ********************************************************************
rem NETCDF STUFF
rem We downloaded the precompiled NetCDF 3.6.1 libraries from 
rem ftp://ftp.unidata.ucar.edu/pub/netcdf/contrib/win32/netcdf-3.6.1-beta1-win32dll.zip
rem For some reason the netcdf.h was not included in this distribution, so we downloaded
rem the netcdf 3.6.1 beta2 source code and used the netcdf.h file found there.  It was also
rem necesary to define DLL_NETCDF so that the right info was selected from the netcdf.h file
********************************************************************
set DODS_INC=c:\netcdf36
set DODS_LIB=c:\netcdf36
set DODS_LIBS=netcdf.lib
rem ********************************************************************
rem General parameters
rem ********************************************************************

set MATLAB=%MATLAB%
set MSVCDir=%MSVCDir%
set MSDevDir=%MSVCDir%
set PATH=%MSDevDir%\bin;%PATH%
set INCLUDE=%DODS_INC%;%MSVCDir%\INCLUDE;%INCLUDE%
set LIB=%MSVCDir%\LIB;%MSVCDir%\PlatformSDK\LIB;%LIB%

rem ********************************************************************
rem Compiler parameters
rem ********************************************************************
set COMPILER=cl
set COMPFLAGS= -DMATLAB_MEX_FILE -DDLL_NETCDF  /nologo /W3 /GR /GX /c
set OPTIMFLAGS=/ML 
set DEBUGFLAGS=/MDd -Zi -Fd"%OUTDIR%%MEX_NAME%.pdb"
set NAME_OBJECT=/Fo

rem ********************************************************************
rem Linker parameters
rem ********************************************************************
set LIBLOC=%MATLAB%\extern\lib\win32\microsoft\msvc71
set LINKER=link
set LINKFLAGS=/dll /NODEFAULTLIB:msvcrt.lib /VERBOSE:lib /export:%ENTRYPOINT% /MAP /LIBPATH:"%DODS_LIB%" %DODS_LIBS% /LIBPATH:"%LIBLOC%" libmx.lib libmex.lib libmat.lib /implib:%LIB_NAME%.x
set LINKOPTIMFLAGS=
set LINKDEBUGFLAGS=/debug
set LINK_FILE=
set LINK_LIB=
set NAME_OUTPUT=/out:"%OUTDIR%%MEX_NAME%.dll"
set RSP_FILE_INDICATOR=@

rem ********************************************************************
rem Resource compiler parameters
rem ********************************************************************
set RC_COMPILER=rc /fo "%OUTDIR%mexversion.res"
set RC_LINKER=

set POSTLINK_CMDS=del "%OUTDIR%%MEX_NAME%.map"
set POSTLINK_CMDS1=del %LIB_NAME%.x
