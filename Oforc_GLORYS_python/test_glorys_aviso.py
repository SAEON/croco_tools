#!/usr/bin/python
import numpy as np
import matplotlib.pyplot as plt
import mpl_toolkits.basemap as bm
#from mpl_toolkits.axes_grid1 import AxesGrid
from netCDF4 import Dataset     as netcdf
from datetime import date
import sys
#sys.path.insert(0,'/XXX/') 
import my_plots                 as my
from interp_Cgrid import  * 
from croco_vgrid import  * 
#from streamfunction import  * 
#import matplotlib._cntr as cntr
import tpx_tools as tpx
from datetime import date
import os
#from scipy import interpolate




moviname='test_glorys_moz.mpg'
#
# Directory and file names
#
#
AVISO_DIR='/u/DATA/AVISO/MADT_RENAME/'
AVISO_TYPE='madt'
name='agu'
#
alti_prefix=AVISO_DIR+'/'+AVISO_TYPE+'_'
alti_suffix='.nc';
#
Yorig=1990
#
# First date to process
#
Ymin=1993
Mmin=01
Dmin=01
#
# Last date to process
#
#
Ymax=2017
Mmax=05
Dmax=10
#
#
dt=3 # Interval between AVISO maps [days]
#
nc=netcdf('glorys_moz_sst.nc')
lon_glo=nc.variables['lon'][:]
lat_glo=nc.variables['lat'][:]
T_glo=nc.variables['time'][:]
#
# choose domain to subset: !! don't go north of 23N !!
#

latmin=-30 
latmax=-10 
lonmin=32
lonmax=55 

#
# Time in days since 1/1/Yorig
# romsdate=date.fromordinal(np.int(date.toordinal(date(Yorig,1,1))+t/(24*3600)))
#  print 'reading date : ',romsdate.isoformat()
#
tstart=date.toordinal(date(Ymin,Mmin,Dmin))-date.toordinal(date(Yorig,1,1))
tend  =date.toordinal(date(Ymax,Mmax,Dmax))-date.toordinal(date(Yorig,1,1))
#
#
# Get the grid
#
a=date.fromordinal(np.int(date.toordinal(date(Yorig,1,1))+tstart))
alti_date="%04d%02d%02d" %(a.year,a.month,a.day)
alti_fname=alti_prefix+alti_date+alti_suffix
#
x,y,zeta,rmask,time=tpx.get_topex2014(lonmin,lonmax,latmin,latmax,alti_fname)
#
[umask,vmask,pmask]=uvp_mask(rmask)
#
lon,lat,f,pm,pn=tpx.get_tpx_grid(x,y)
#
#
#
# Main Loop
#
#
#
lwidth=0.5
time_steps=np.arange(tstart,tend+dt,dt)
nindx=-1
files = []
for t in time_steps:
#
# Get the date
#
  a=date.fromordinal(np.int(date.toordinal(date(Yorig,1,1))+t))
  alti_date="%04d%02d%02d" %(a.year,a.month,a.day)
  alti_fname=alti_prefix+alti_date+alti_suffix
#
# read Topex SSH
#
  x,y,zeta,mask,time=tpx.get_topex2014(lonmin,lonmax,latmin,latmax,alti_fname)
  zeta=100*zeta
  zeta[zeta<-200]=np.nan
#
# read Glorys
#
  tndx=np.int(np.squeeze(np.array(np.where(T_glo-0.5==t))))
  print tndx
  ssh=100*nc.variables['ssh'][tndx,:,:]
  ssh[np.where(ssh.mask)]=np.nan
  ssh=np.array(ssh)
  
  fig = plt.figure(figsize=(12,10))
  [Long,Latg]=np.meshgrid(lon_glo,lat_glo)
  m_glo,x_glo,y_glo=my.figmap2(Long,Latg,lonmin,lonmax,latmin,latmax,'i')
  m_avi = bm.Basemap(projection='merc',
               llcrnrlat=lat.min(), llcrnrlon=lon.min(),
               urcrnrlat=lat.max(), urcrnrlon=lon.max(),
               resolution='l')
  x_avi, y_avi = m_avi(lon,lat)
  clevs = np.arange(-200,200,20)
#  cs1 = m_glo.contourf(x_glo,y_glo,100*ssh_glo,np.arange(0,150,20),cmap='bwr',extend='both')
  cs2 = m_glo.contour(x_glo,y_glo,ssh,clevs,linestyles='solid',colors='r',linewidths=lwidth)
  cs3 = m_avi.contour(x_avi,y_avi,zeta,clevs,linestyles='solid',colors='k',linewidths=lwidth)
  plt.title(alti_date)

  nindx=nindx+1
  fname = '_tmp%05d' %nindx
  print('Saving frame', fname)
  plt.savefig(fname,dpi=50)
  plt.close()
  files.append(fname)

nc.close()
print('Convert tmp files')
for fname in files:
  os.system('convert '+fname+'.png '+fname+'.ppm')
print('Making movie  - this make take a while')
os.system("ppmtompeg ./inp_ppm2mpeg")
# cleanup files
print('Cleaning tmp files')
for fname in files:
  os.system('rm '+fname+'.png '+fname+'.ppm')

os.system('mv movie.mpg '+moviname)
