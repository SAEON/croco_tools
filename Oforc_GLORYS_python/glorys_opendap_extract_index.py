import numpy as np
import matplotlib.pyplot as plt
import mpl_toolkits.basemap as bm
from mpl_toolkits.basemap import cm
from pydap.client import open_url
from datetime import date
import netCDF4 as nc
import pandas as pd
import os
import time
import sys

os.system("unset http_proxy")
os.system("unset https_proxy")
os.system("unset HTTP_PROXY")
os.system("unset HTTPS_PROXY")
os.system("export http_proxy=''")
os.system("export HTTP_PROXY=''")

#
# URLS for each variable
#

urlT="http://tds.mercator-ocean.fr/thredds/dodsC/glorys12v1-daily-gridT"   # votemper
urlS="http://tds.mercator-ocean.fr/thredds/dodsC/glorys12v1-daily-gridS"   # vosaline
urlU="http://tds.mercator-ocean.fr/thredds/dodsC/glorys12v1-daily-gridU"   # vozocrtx (zonal velocity), sozotaux (zonal wind stress)
urlV="http://tds.mercator-ocean.fr/thredds/dodsC/glorys12v1-daily-gridV"   # vomecrty (meridional velocity), sometauy (meridional windstress)
url2D="http://tds.mercator-ocean.fr/thredds/dodsC/glorys12v1-daily-grid2D" # sossheig

tout=3000 #time out
#
# Directories and names
#

missval=np.nan
out_name='GLORYS_12V1_SAFE'
Yorig=1990

#
# choose domain to subset: !! don't go north of 23N !!
#

big=1

if big==1:
  out_dir='/media/ppenven/GLORYS12/NC/'
  tmp_dir='/media/ppenven/GLORYS12/TMP/'
  latmin=-60 
  latmax=  6 
  lonmin=-20
  lonmax= 70 
else:
  out_dir='/home/ppenven/Download_GLORYS/TEST/'
  tmp_dir='/home/ppenven/Download_GLORYS/TEST/'
  latmin=-38
  latmax=-33
  lonmin=17
  lonmax=23

#
# choose the time frame to extract
#

batch_mod=1

if batch_mod==1:
  print ("This is the name of the script: "), sys.argv[0]
  print ("Number of arguments: "), len(sys.argv)
  print ("The arguments are: ") , str(sys.argv)
  if len(sys.argv) != 2:
    raise ValueError('Usage: python ./glorys_opendap_extract_batch.py T')
  else:
    T=int(float(sys.argv[1]))
    print (' T days since Yorig:  '), T
else:
  T=date.toordinal(date(2018,12,25))-date.toordinal(date(Yorig,1,1))
#
# Get the time in days since Yorig,1,1 
#

T=T+0.5 #12H

Tstr="%06d" %(T)

#
# specify names of variables and coordinates
#

Tname='votemper' 
Sname='vosaline' 
Uname='vozocrtx' 
Vname='vomecrty' 
SSHname='sossheig'

latname='nav_lat'
lonname='nav_lon'
depname='deptht'
tname='time_counter'

#
# Output file names
#

tmp_file=tmp_dir+'tmp_'+out_name+'_'+Tstr+'.nc'
output_file=out_dir+out_name+'_'+Tstr+'.nc'

#
# Test if the output file is already here
#

if not(os.path.exists(output_file)):

#
# Open the URLs
#
  print('Open URLS...')

  print('Open URL: ' +urlT)
  datasetT=open_url(urlT,timeout=tout)
  print('Open URL: ' +urlS)
  datasetS=open_url(urlS,timeout=tout)
  print('Open URL: ' +urlU)
  datasetU=open_url(urlU,timeout=tout)
  print('Open URL: ' +urlV)
  datasetV=open_url(urlV,timeout=tout)
  print('Open URL: ' +url2D)
  dataset2D=open_url(url2D,timeout=tout)

#
# Read the horizontal grid from dataset2D
#

#
# !!! WARNING : NORTH of 23N the grid is no more rectangular !!! don't use it north of 23N
#
#

  print('Read LatT and LonT...')

  nlat = dataset2D[latname][:,1]
  nlon = dataset2D[lonname][1,:]
  lonT=np.array(nlon[lonname])
  latT=np.array(nlat[latname])

  print('Read LatU and LonU...')

  nlat = datasetU[latname][:,1]
  nlon = datasetU[lonname][1,:]
  lonU=np.array(nlon[lonname])
  latU=np.array(nlat[latname])

  print('Read LatV and LonV...')

  nlat = datasetV[latname][:,1]
  nlon = datasetV[lonname][1,:]
  lonV=np.array(nlon[lonname])
  latV=np.array(nlat[latname])

#
# find the indices of the lon and lat ranges specified above
#
# !!! WARNING : the dataset is in the form -180 ; 180 (prb for Pacific)
#
#

  print('Get a subset...')

  LAT_indx=np.where( (latT > latmin) & (latT < latmax) )
  LON_indx=np.where( (lonT > lonmin) & (lonT < lonmax) )

  iminT=LON_indx[1].min()
  imaxT=LON_indx[1].max()
  jminT=LAT_indx[0].min()
  jmaxT=LAT_indx[0].max()

  lonT=lonT[0,iminT:imaxT]
  latT=latT[jminT:jmaxT,0]

  LT=np.size(lonT)
  MT=np.size(latT)

  LAT_indx=np.where( (latU > latmin) & (latU < latmax) )
  LON_indx=np.where( (lonU > lonmin) & (lonU < lonmax) )

  iminU=LON_indx[1].min()
  imaxU=LON_indx[1].max()
  jminU=LAT_indx[0].min()
  jmaxU=LAT_indx[0].max()

  lonU=lonU[0,iminU:imaxU]
  latU=latU[jminU:jmaxU,0]

  LU=np.size(lonU)
  MU=np.size(latU)

  LAT_indx=np.where( (latV > latmin) & (latV < latmax) )
  LON_indx=np.where( (lonV > lonmin) & (lonV < lonmax) )

  iminV=LON_indx[1].min()
  imaxV=LON_indx[1].max()
  jminV=LAT_indx[0].min()
  jmaxV=LAT_indx[0].max()

  lonV=lonV[0,iminV:imaxV]
  latV=latV[jminV:jmaxV,0]

  LV=np.size(lonV)
  MV=np.size(latV)

#
# Get time indices (they are different due to a bug in the datasets)
#

  T2d=(np.squeeze(dataset2D['time_counter'][:])/24)+date.toordinal(date(1950,1,1))-date.toordinal(date(Yorig,1,1))
  tndx2D=np.int(np.squeeze(np.where(T2d==T)))

  TT=(np.squeeze(datasetT['time_counter'][:])/24)+date.toordinal(date(1950,1,1))-date.toordinal(date(Yorig,1,1))
  tndxT=np.int(np.squeeze(np.where(TT==T)))

  TS=(np.squeeze(datasetS['time_counter'][:])/24)+date.toordinal(date(1950,1,1))-date.toordinal(date(Yorig,1,1))
  tndxS=np.int(np.squeeze(np.where(TS==T)))

  TU=(np.squeeze(datasetU['time_counter'][:])/24)+date.toordinal(date(1950,1,1))-date.toordinal(date(Yorig,1,1))
  tndxU=np.int(np.squeeze(np.where(TU==T)))

  TV=(np.squeeze(datasetV['time_counter'][:])/24)+date.toordinal(date(1950,1,1))-date.toordinal(date(Yorig,1,1))
  tndxV=np.int(np.squeeze(np.where(TV==T)))


#
# Get the vertical levels
#

  depths=np.squeeze(datasetT[depname][:])
  N=max(np.shape(depths))

#
# Create the file
#

  if os.path.exists(tmp_file):
    os.remove(tmp_file)

  print('Create: ' +tmp_file)
  nc_out=nc.Dataset(tmp_file,'w',format='NETCDF4')

#
# set global attributes
#

  nc_out.description='GLORYS 12V1 extraction'
  nc_out.history = 'Created '+str(time.ctime(time.time()))
  nc_out.source=url2D

#
# Create dimensions
#

  nc_dim_time=nc_out.createDimension('time',None)

  nc_dim_lonT=nc_out.createDimension('lonT',LT)
  nc_dim_latT=nc_out.createDimension('latT',MT)

  nc_dim_lonU=nc_out.createDimension('lonU',LU)
  nc_dim_latU=nc_out.createDimension('latU',MU)

  nc_dim_lonV=nc_out.createDimension('lonV',LV)
  nc_dim_latV=nc_out.createDimension('latV',MV)

  nc_dim_depth=nc_out.createDimension('depth',N)

#
# Create variables
#


### create coordinate variables
  nc_time=nc_out.createVariable('time',np.float64, ('time',))

  nc_time.units='days since '+str(Yorig)+'-01-01 00:00:00'
#nc_time.units='hours since 1950-01-01 00:00:00'
  nc_time.calendar='gregorian'

  nc_depth=nc_out.createVariable('depth',np.float32, ('depth',))
  nc_depth.units='meter'

  nc_latT=nc_out.createVariable('latT',np.float32, ('latT',))
  nc_latT.units='degree_north'

  nc_lonT=nc_out.createVariable('lonT',np.float32, ('lonT',))
  nc_lonT.units='degree_east'

  nc_latU=nc_out.createVariable('latU',np.float32, ('latU',))
  nc_latU.units='degree_north'

  nc_lonU=nc_out.createVariable('lonU',np.float32, ('lonU',))
  nc_lonU.units='degree_east'

  nc_latV=nc_out.createVariable('latV',np.float32, ('latV',))
  nc_latV.units='degree_north'

  nc_lonV=nc_out.createVariable('lonV',np.float32, ('lonV',))
  nc_lonV.units='degree_east'

  nc_ssh=nc_out.createVariable('ssh',np.float32,('time','latT','lonT'))
  nc_ssh.long_name='SEA LEVEL HEIGHT'
  nc_ssh.units='m'
  nc_ssh.missing_value=missval

  nc_temp=nc_out.createVariable('temp',np.float32,('time','depth','latT','lonT'))
  nc_temp.long_name='TEMPERATURE'
  nc_temp.units='deg. C'
  nc_temp.missing_value=missval


  nc_salt=nc_out.createVariable('salt',np.float32,('time','depth','latT','lonT'))
  nc_salt.long_name='SALINITY'
  nc_salt.units='ppt'
  nc_salt.missing_value=missval

  nc_U=nc_out.createVariable('u',np.float32,('time','depth','latU','lonU'))
  nc_U.long_name='ZONAL VELOCITY'
  nc_U.units='m/sec'
  nc_U.missing_value=missval

  nc_V=nc_out.createVariable('v',np.float32,('time','depth','latV','lonV'))
  nc_V.long_name='MERIDIONAL VELOCITY'
  nc_V.units='m/sec'
  nc_V.missing_value=missval

#
# Load lon, lat and depths
#

  nc_latT[:]=latT[:]
  nc_lonT[:]=lonT[:]

  nc_latU[:]=latU[:]
  nc_lonU[:]=lonU[:]

  nc_latV[:]=latV[:]
  nc_lonV[:]=lonV[:]

  nc_depth[:]=depths

#
# Check time indices
#

  var1=dataset2D[tname][tndx2D,1]
  dt=nc.num2date(var1.data[0], units='hours since 1950-01-01 00:00:00')
  print ('2D: tndx2D: '+str(tndx2D)+' Year: '+str(dt.year)+', Month: '+str(dt.month)+', Day: '+str(dt.day)+', H: '+str(dt.hour))

  var1=datasetT[tname][tndxT,1]
  dt=nc.num2date(var1.data[0], units='hours since 1950-01-01 00:00:00')
  print ('T: tndxT: '+str(tndxT)+' Year: '+str(dt.year)+', Month: '+str(dt.month)+', Day: '+str(dt.day)+', H: '+str(dt.hour))

  var1=datasetS[tname][tndxS,1]
  dt=nc.num2date(var1.data[0], units='hours since 1950-01-01 00:00:00')
  print ('S: tndxS: '+str(tndxS)+' Year: '+str(dt.year)+', Month: '+str(dt.month)+', Day: '+str(dt.day)+', H: '+str(dt.hour))

  var1=datasetU[tname][tndxU,1]
  dt=nc.num2date(var1.data[0], units='hours since 1950-01-01 00:00:00')
  print ('U: tndxU: '+str(tndxU)+' Year: '+str(dt.year)+', Month: '+str(dt.month)+', Day: '+str(dt.day)+', H: '+str(dt.hour))

  var1=datasetV[tname][tndxV,1]
  dt=nc.num2date(var1.data[0], units='hours since 1950-01-01 00:00:00')
  print ('V: tndxV: '+str(tndxV)+' Year: '+str(dt.year)+', Month: '+str(dt.month)+', Day: '+str(dt.day)+', H: '+str(dt.hour))

#
# Load variables
#

  count=0

  nc_time[count]=T

  print(' SSH...')
  var1=dataset2D[SSHname]
  var2=var1[SSHname].data[tndx2D,jminT:jmaxT,iminT:imaxT]
  nc_ssh[count,:,:]=var2[0,:,:]

  print(' temp...')
  var1=datasetT[Tname]
  var2=var1[Tname].data[tndxT,:,jminT:jmaxT,iminT:imaxT]
  nc_temp[count,:,:,:]=var2[0,:,:,:]

  print(' salt...')
  var1=datasetS[Sname]
  var2=var1[Sname].data[tndxS,:,jminT:jmaxT,iminT:imaxT]
  nc_salt[count,:,:,:]=var2[0,:,:,:]
 
  print(' U...')
  var1=datasetU[Uname]
  var2=var1[Uname].data[tndxU,:,jminU:jmaxU,iminU:imaxU]
  nc_U[count,:,:,:]=var2[0,:,:,:]
 
  print(' V...')
  var1=datasetV[Vname]
  var2=var1[Vname].data[tndxV,:,jminV:jmaxV,iminV:imaxV]
  nc_V[count,:,:,:]=var2[0,:,:,:]

  nc_out.close()

#
# Move the file in the final directory
#

  os.rename(tmp_file,output_file)

  print('Done')

else:

  print(output_file+' already exists - stop')


