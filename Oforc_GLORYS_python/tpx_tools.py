import numpy as np
import numpy.ma as ma
from datetime import date
from netCDF4 import Dataset as netcdf
import tpx_tools as tpx
from scipy import interpolate
#import matplotlib._cntr as cntr

#
# --------------- get a netcdf AVISO file name from a time ---------------
#
def get_tpx_name(T,Yorig,alti_prefix,alti_suffix):
#
# T: time in days since Yorig/1/1
#
  a=date.fromordinal(np.int(T+date.toordinal(date(Yorig,1,1))))
  alti_date="%04d%02d%02d" %(a.year,a.month,a.day)
  alti_fname=alti_prefix+alti_date+alti_suffix
#
  return alti_fname
#
# --------------- read a netcdf AVISO map ---------------
#

def get_topex2020(lonmin,lonmax,latmin,latmax,fname):
#
# Read an AVISO netcdf file and return lon,lat,zeta
#
  dataname='adt'
#
  ncdat=netcdf(fname,mode='r')
  T=ncdat.variables['time'][:]
  X=ncdat.variables['longitude'][:]
  Y=ncdat.variables['latitude'][:]
#
  a=date.fromordinal(T+date.toordinal(date(1950,1,1)))
  print('AVISO - ' + a.isoformat())
#
# get a subgrid in y
#
  j = np.where(( Y>=latmin ) & ( Y<=latmax ))
  j = np.array(j)
  j = j.flatten()
  y=Y[j.min():j.max()]
#
# get 3 subgrids in x 
#
  i1 = np.where(( X-360.>=lonmin) & ( X-360.<=lonmax ))
  i1 = np.array(i1)
  i1 = i1.flatten()
  i2 = np.where(( X>=lonmin) & ( X<=lonmax ))
  i2 = np.array(i2)
  i2 = i2.flatten()
  i3 = np.where(( X+360.>=lonmin) & ( X+360.<=lonmax ))
  i3 = np.array(i3)
  i3 = i3.flatten()
#
#  Read data for the 3 subgrids and concatenate
#
  if len(i2)>0:
    d2=ncdat.variables['adt'][0,j.min():j.max(),i2.min():i2.max()]
    m2=1. + np.zeros(d2.shape)
    try:
      m2[d2.mask]=0.
      d2.mask=ma.nomask
    except:
      print('no mask d2')
      
    x2=X[i2.min():i2.max()]
  
  if len(i1)>0:
    d1=ncdat.variables['adt'][0,j.min():j.max(),i1.min():i1.max()]
    m1=1. + np.zeros(d1.shape)
    try:
      m1[d1.mask]=0.
      d1.mask=ma.nomask
    except:
      print('no mask d1')
      
    x1=X[i1.min():i1.max()]-360
 
  if len(i3)>0:
    d3=ncdat.variables['adt'][0,j.min():j.max(),i3.min():i3.max()]
    m3=1. + np.zeros(d3.shape)
    try:
      m3[d3.mask]=0.
      d3.mask=ma.nomask
    except:
      print('no mask d3')
      
    x3=X[i3.min():i3.max()]+360
#
#  Close and return
#
  ncdat.close()


  if len(i2)>0:
    data=d2
    mask=m2
    x=x2
    
  if len(i1)>0:
    if len(data) > 0:
      data=np.concatenate((d1,data),axis=1)
      mask=np.concatenate((m1,mask),axis=1)
      x=np.concatenate((x1,x),axis=0)
    else:
      data=d1
      mask=m1
      x=x1
 
  if len(i3)>0:
    if len(data) > 0:
      data=np.concatenate((data,d3),axis=1)
      mask=np.concatenate((mask,m3),axis=1)
      x=np.concatenate((x,x3),axis=0)
    else:
      data=d3
      mask=m3
      x=x3


  [Mp,Lp]=data.shape
  dout=np.zeros((Mp,Lp))
  dout[:]=data[:]

  mask=np.zeros((Mp,Lp)) +1.
  mask[np.where(dout<=-1000)]=0.
  dout[np.where(dout<=-1000)]=np.nan


#
#
#
  return (x,y,dout,mask,T)
#
# --------------- read a netcdf AVISO map ---------------
#

def get_topex2014(lonmin,lonmax,latmin,latmax,fname):
#
# Read an AVISO netcdf file and return lon,lat,zeta
#
  dataname='adt'
#
  ncdat=netcdf(fname,mode='r')
  T=ncdat.variables['time'][:]
  X=ncdat.variables['lon'][:]
  Y=ncdat.variables['lat'][:]
#
  a=date.fromordinal(T+date.toordinal(date(1950,1,1)))
  print('AVISO - ' + a.isoformat())
#
# get a subgrid in y
#
  j = np.where(( Y>=latmin ) & ( Y<=latmax ))
  j = np.array(j)
  j = j.flatten()
  y=Y[j.min():j.max()]
#
# get 3 subgrids in x 
#
  i1 = np.where(( X-360.>=lonmin) & ( X-360.<=lonmax ))
  i1 = np.array(i1)
  i1 = i1.flatten()
  i2 = np.where(( X>=lonmin) & ( X<=lonmax ))
  i2 = np.array(i2)
  i2 = i2.flatten()
  i3 = np.where(( X+360.>=lonmin) & ( X+360.<=lonmax ))
  i3 = np.array(i3)
  i3 = i3.flatten()
#
#  Read data for the 3 subgrids and concatenate
#
  if len(i2)>0:
    d2=ncdat.variables['adt'][0,j.min():j.max(),i2.min():i2.max()]
    m2=1. + np.zeros(d2.shape)
    try:
      m2[d2.mask]=0.
      d2.mask=ma.nomask
    except:
      print('no mask d2')
      
    x2=X[i2.min():i2.max()]
  
  if len(i1)>0:
    d1=ncdat.variables['adt'][0,j.min():j.max(),i1.min():i1.max()]
    m1=1. + np.zeros(d1.shape)
    try:
      m1[d1.mask]=0.
      d1.mask=ma.nomask
    except:
      print('no mask d1')
      
    x1=X[i1.min():i1.max()]-360
 
  if len(i3)>0:
    d3=ncdat.variables['adt'][0,j.min():j.max(),i3.min():i3.max()]
    m3=1. + np.zeros(d3.shape)
    try:
      m3[d3.mask]=0.
      d3.mask=ma.nomask
    except:
      print('no mask d3')
      
    x3=X[i3.min():i3.max()]+360
#
#  Close and return
#
  ncdat.close()


  if len(i2)>0:
    data=d2
    mask=m2
    x=x2
    
  if len(i1)>0:
    if len(data) > 0:
      data=np.concatenate((d1,data),axis=1)
      mask=np.concatenate((m1,mask),axis=1)
      x=np.concatenate((x1,x),axis=0)
    else:
      data=d1
      mask=m1
      x=x1
 
  if len(i3)>0:
    if len(data) > 0:
      data=np.concatenate((data,d3),axis=1)
      mask=np.concatenate((mask,m3),axis=1)
      x=np.concatenate((x,x3),axis=0)
    else:
      data=d3
      mask=m3
      x=x3



  [Mp,Lp]=data.shape
  dout=np.zeros((Mp,Lp))
  dout[:]=data[:]
  
 

#
#
#
  return (x,y,dout,mask,T)
  
  
#
# --------------- spheric distance ---------------
#

def spheric_dist(lat1,lat2,lon1,lon2):
#####################################################################
#
#  function dist=spheric_dist(lat1,lat2,lon1,lon2)
#
# compute distances for a simple spheric earth
#
#   input:
#
#  lat1 : latitude of first point (matrix)
#  lon1 : longitude of first point (matrix)
#  lat2 : latitude of second point (matrix)
#  lon2 : longitude of second point (matrix)
#
#   output:
#  dist : distance from first point to second point (matrix)
  R=6367442.76
#
#  Determine proper longitudinal shift.
#
  l=np.abs(lon2-lon1)
#
  if l.size==1:
   if l>180.:
     l=360-l
  else:
   l[l>=180.]=360.-l[l>=180.]
#                  
#  Convert Decimal degrees to radians.
#
  deg2rad=np.pi/180.
  lat1=lat1*deg2rad
  lat2=lat2*deg2rad
  l=l*deg2rad
#
#  Compute the distances
#
  dist=R*np.arcsin(np.sqrt(((np.sin(l)*np.cos(lat2))**2)+ \
                            (((np.sin(lat2)*np.cos(lat1))- \
			      (np.sin(lat1)*np.cos(lat2)*np.cos(l)))**2)))
  return dist

#
# --------------- Get a grid from Topex ---------------
#
def get_tpx_grid(x,y):
#
# Get pm,pn,f from Topex
#
  [lon,lat]=np.meshgrid(x,y)
#
  f=4*np.pi*np.sin(np.pi*lat/180)*366.25/(24*3600*365.25)
#
# Get the metrics
#
  lonu=0.5*(lon[ :  , :-1]+lon[ : ,1: ])
  latu=0.5*(lat[ :  , :-1]+lat[ : ,1: ])
  lonv=0.5*(lon[ :-1, :  ]+lon[1: , : ])
  latv=0.5*(lat[ :-1, :  ]+lat[1: , : ])
#
# pm and pn
#
  dx=0*f
  dy=0*f
#
  dx[:,1:-1]=tpx.spheric_dist(latu[:,:-1],latu[:,1:], \
                        lonu[:,:-1],lonu[:,1:])
  dx[:,0]=dx[:,2]
  dx[:,-1]=dx[:,-2]
#
  dy[1:-1,:]=tpx.spheric_dist(latv[:-1,:],latv[1:,:], \
                        lonv[:-1,:],lonv[1:,:])
  dy[0,:]=dy[1,:]
  dy[-1,:]=dy[-2,:]
#
  pm=1/dx
  pn=1/dy
#
  return (lon,lat,f,pm,pn)
#
#
# --------------- Get a grid from Topex ---------------
#
def interp_topo(lon,lat,dx,dy,toponame):
#
# add a topography (here etopo2) to a ROMS grid
#
# the topogaphy matrix is coarsened prior
# to the interpolation on the ROMS grid tp
# prevent the generation of noise due to 
# subsampling. this procedure ensure a better
# general volume conservation.
#
#
  dx_grd=0.5*(dx.mean()+dy.mean())
  print('   Grid resolution : '+ str(np.int(dx_grd/1000))+ ' km')
#
  dl=np.max([1,2*(dx_grd/(60*1852))])
  lonmin=lon.min()-dl
  lonmax=lon.max()+dl
  latmin=lat.min()-dl
  latmax=lat.max()+dl
#
#  open the topo file
#
  nc=netcdf(toponame,mode='r')
  X=nc.variables['lon'][:]
  Y=nc.variables['lat'][:]
#
# get a subgrid in y
#
  j = np.where(( Y>=latmin ) & ( Y<=latmax ))
  j = np.array(j)
  j = j.flatten()
  y=Y[j.min():j.max()]
#
# get 3 subgrids in x 
#
  i1 = np.where(( X-360>=lonmin) & ( X-360<=lonmax ))
  i1 = np.array(i1)
  i1 = i1.flatten()
  i2 = np.where(( X>=lonmin) & ( X<=lonmax ))
  i2 = np.array(i2)
  i2 = i2.flatten()
  i3 = np.where(( X+360>=lonmin) & ( X+360<=lonmax ))
  i3 = np.array(i3)
  i3 = i3.flatten()
#
#  Read topo for the 3 subgrids and concatenate
#
  if len(i2)>0:
    topo=nc.variables['topo'][j.min():j.max(),i2.min():i2.max()]
    x=X[i2.min():i2.max()]
  
  if len(i1)>0:
    if len(topo) > 0:
      topo=np.concatenate((nc.variables['topo'][j.min():j.max(),i1.min():i1.max()],topo),axis=1)
      x=np.concatenate((X[i1.min():i1.max()]-360,x),axis=0)
    else:
      topo=nc.variables['topo'][j.min():j.max(),i1.min():i1.max()]
      x=X[i1.min():i1.max()]-360
 
  if len(i3)>0:
    if len(topo) > 0:
      topo=np.concatenate((topo,nc.variables['topo'][j.min():j.max(),i3.min():i3.max()]),axis=1)
      x=np.concatenate((x,X[i3.min():i3.max()]+360),axis=0)
    else:
      topo=nc.variables['topo'][j.min():j.max(),i3.min():i3.max()]
      x=X[i3.min():i3.max()]+360
#
#  Close and return
#
  nc.close()
  topo=-topo
#
# Get TOPO averaged resolution
#
  R=6367442.76
  deg2rad=np.pi/180
  dg=x[1:]-x[:-1]
  dg=dg.mean()
  dphi=y[1:]-y[:-1]
  dphi=dphi.mean()
  dy=R*deg2rad*dphi
  dx=R*deg2rad*dg*np.cos(deg2rad*y)
  dx_topo=0.5*(dx.mean()+dy.mean())
  print('   Topography data resolution : '+ str(dx_topo/1000)+ ' km')
#
# Degrade TOPO resolution
#
  n=0
  while (dx_grd>dx_topo):
    n=n+1
    
    x=0.5*(x[1:]+x[:-1])
    x=x[::2]
    y=0.5*(y[1:]+y[:-1])
    y=y[::2]

    topo=0.25*(topo[1: ,:-1]+topo[1: ,1:]+ \
               topo[:-1,:-1]+topo[:-1,1:])
    topo=topo[::2,::2]
    print(n, topo.shape)

    dg=x[1:]-x[:-1]
    dg=dg.mean()
    dphi=y[1:]-y[:-1]
    dphi=dphi.mean()
    dy=R*deg2rad*dphi
    dx=R*deg2rad*dg*np.cos(deg2rad*y)
    dx_topo=0.5*(dx.mean()+dy.mean())

  print('   Topography resolution halved '+str(n)+' times')
  print('   New topography resolution : '+str(dx_topo/1000)+' km')
#
#  interpolate the topo
#
  finterp = interpolate.interp2d(x,y,topo,kind='cubic')
  h= finterp(lon, lat)
#[X,Y]=np.meshgrid(x,y)
#finterp = interpolate.Rbf(X,Y,topo,epsilon=2)
#h= finterp(Lon, Lat)
  h[h<0]=0
  return h
#
#
# ---------------  contour_agulhas ---------------
#
def contour_agulhas(Lon,Lat,zeta,isagulhasshelf):
#
# Get the westernmost position of the Agulhas retroflection
#

#
# Get the value of zeta inshore of the Agulhas of the Natal coast
#

  lon0=Lon[isagulhasshelf==1]
  lon0=lon0.mean()
  lat0=Lat[isagulhasshelf==1]
  lat0=lat0.mean()
  
  zetaagul=zeta[isagulhasshelf==1]
  zetaagul=zetaagul.mean()

  c = cntr.Cntr(Lon,Lat,zeta)
  nlist = c.trace(zetaagul,zetaagul, 0)
  segs = nlist[:len(nlist)//2]
  distmin=5000e3;
  llmax=0
  for seg in segs:
    x=seg[:,0]
    ll=x.shape[0]
    if ll>llmax:
      llmax=ll
  
  print(str(llmax)) 
  llmax=llmax/2
  
  for seg in segs:
    x=seg[:,0]
    ll=x.shape[0]
    y=seg[:,1]
    linedist=tpx.spheric_dist(y,lat0,x,lon0)
    linedist=linedist.min()
    if ( linedist<distmin ) & ( ll>=llmax):
      distmin=linedist
      x0=x
      y0=y
  
  x_retro=x0.min()
  y_retro=y0[x0==x_retro]
  
  x_retro=x_retro.mean()
  y_retro=y_retro.mean()
  
  return x0,y0,x_retro,y_retro
#
#
# ---------------  ---------------
#
