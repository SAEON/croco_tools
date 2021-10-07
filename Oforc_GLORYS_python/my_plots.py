#!/usr/bin/python
import numpy                as np
import matplotlib.pyplot    as plt
import mpl_toolkits.basemap as bm
from mpl_toolkits.basemap import cm
from netCDF4 import Dataset as netcdf

#
# --------------- Quick pcolor for debugging ---------------
#
def pcol(varin):
  plt.pcolor(varin)
  plt.colorbar()
  plt.show()
  return

#
# --------------- Quick contourf for debugging ---------------
#
def cnf(varin):
  plt.contourf(varin)
  plt.colorbar()
  plt.show()
  return


# --------------- read a netcdf ROMS grid ---------------

def get_grd(gname):
  ncgrd = netcdf(gname,mode='r')
  mask=ncgrd.variables['mask_rho'][:]
  lon=ncgrd.variables['lon_rho'][:]
  lat=ncgrd.variables['lat_rho'][:]
  pm=ncgrd.variables['pm'][:]
  pn=ncgrd.variables['pn'][:]
  f=ncgrd.variables['f'][:]
  ncgrd.close()
  
  return mask,lon,lat,pm,pn,f


# --------------- read a netcdf ROMS grid ---------------

def get_grd2(gname):
  ncgrd = netcdf(gname,mode='r')
  mask=ncgrd.variables['mask_rho'][:]
  lon=ncgrd.variables['lon_rho'][:]
  lat=ncgrd.variables['lat_rho'][:]
  pm=ncgrd.variables['pm'][:]
  pn=ncgrd.variables['pn'][:]
  f=ncgrd.variables['f'][:]
  h=ncgrd.variables['h'][:]
  ncgrd.close()
  
  return mask,lon,lat,pm,pn,f,h


# --------------- simple eke plot / no show / no save / no colorbar ---------------

def plt_eke_nocbar(fname):

  data=np.load(fname)
  lon = data['lon']
  lat = data['lat']
  eke = data['eke']
  zeta = data['avgzeta']
  cs1 = ekesshmap_nocbar(lon,lat,eke,zeta)

  return cs1

# --------------- simple eke plot ---------------

def plt_eke(fname, outname):

  data=np.load(fname)
  lon = data['lon']
  lat = data['lat']
  eke = data['eke']
  zeta = data['avgzeta']
  plt.figure()
  ekesshmap(lon,lat,eke,zeta)
  plt.savefig(outname)
  plt.show()

  return
# --------------- eke + ssh map no colorbar ---------------

def ekesshmap_nocbar(lon,lat,eke,zeta):

  m,x,y =figmap(lon,lat)

  clevs1 = np.arange(0,3100,100)
  cs1 = m.contourf(x,y,1e4*eke,clevs1,cmap=plt.cm.afmhot_r)

  clevs2 = np.arange(-200,200,20)
  cs2 = m.contour(x,y,100*zeta,clevs2,linestyles='solid',colors='k',linewidths=0.2)
  plt.clabel(cs2, inline=True, fmt='%1.0f', fontsize=7, colors='k')
  
  return cs1
# --------------- eke + ssh map ---------------

def ekesshmap(lon,lat,eke,zeta):

  m,x,y =figmap(lon,lat)

  clevs1 = np.arange(0,3100,100)
  cs1 = m.contourf(x,y,1e4*eke,clevs1,cmap=plt.cm.afmhot_r)

  cbar = m.colorbar(cs1)
  cbar.ax.xaxis.set_label_position('top') 
  cbar.ax.set_xlabel('cm$^2$.s$^{-2}$')

  clevs2 = np.arange(-200,200,20)
  cs2 = m.contour(x,y,100*zeta,clevs2,linestyles='solid',colors='k',linewidths=0.2)
  plt.clabel(cs2, inline=True, fmt='%1.0f', fontsize=7, colors='k')
  
  return

# --------------- simple figure with a map ---------------

def figmap(lon,lat):

  m = bm.Basemap(projection='merc',
               llcrnrlat=lat.min(), llcrnrlon=lon.min(),
               urcrnrlat=lat.max(), urcrnrlon=lon.max(),
               resolution='l')
  x, y = m(lon,lat)
  m.drawparallels(np.arange(10*np.floor(lat.min()/10),10*np.ceil(lat.max()/10),10.),
                      labels=[True,False,False,False],
                      fontsize='5',linewidth=0.18)
  m.drawmeridians(np.arange(10*np.floor(lon.min()/10),10*np.ceil(lon.max()/10),10.),
                      labels=[False,False,False,True],
                      fontsize='5',linewidth=0.18)
  m.fillcontinents(color='grey')

  #plt.xlabel('Longitude',labelpad=20)
  #plt.ylabel('Latitude',labelpad=40)
  
  return m,x,y

# --------------- second figure with a map ---------------

def figmap2(lon,lat,lonmin,lonmax,latmin,latmax,res,fsize):

  m = bm.Basemap(projection='merc',
               llcrnrlat=latmin, llcrnrlon=lonmin,
               urcrnrlat=latmax, urcrnrlon=lonmax,
               resolution=res)
  x, y = m(lon,lat)
  m.drawparallels(np.arange(10*np.floor(lat.min()/10),10*np.ceil(lat.max()/10),10.),
                      labels=[True,False,False,False],
                      fontsize=fsize,linewidth=0.15)
  m.drawmeridians(np.arange(10*np.floor(lon.min()/10),10*np.ceil(lon.max()/10),10.),
                      labels=[False,False,False,True],
                      fontsize=fsize,linewidth=0.15)
  m.fillcontinents(color='grey')

  #plt.xlabel('Longitude',labelpad=20)
  #plt.ylabel('Latitude',labelpad=40)
  
  return m,x,y
