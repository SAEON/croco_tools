#
#
#########################################################################
#
#
# croco_glorys.py
#
#
# A collection of functions for interpolation of GLORYS ocean reanalysis 
# data to create CROCO initial and boundary condition files
#
#
# Gustav Rautenbach, Steven Herbette, Pierrick Penven, 2021
#
#########################################################################
#
#
#  geo_idx = geo_idx(dd, dd_array)
#
#    Get the closest index of a particular coordinate
#
#
#  (elem,coef) = get_tri_coef(X, Y, newX, newY, verbose=0)
#
#    Get Delaunay linear interpolation pointers and coefficients
#
#
#  varnew =  horiz_interp_delaunay(lonold,latold,varold,lonnew,latnew,elem=0,coef=0)
#
#    Do an horizontal interpolation
#
#
#  Vout,NzGood = interp_tracers(nc,vname,l,k,imin,imax,jmin,jmax,Lon,Lat,coef,elem)
#
#    Remove the missing values from a gridded 2D field from a Netcdf file
#    and do an horizontal interpolation using Delaunay matrices (coef and elem)
#
#
#  vout=interp3d(nc,vname,tndx_glo,Nzgoodmin,depth,z_rho,imin,imax,jmin,jmax,Lon,Lat,coef,elem)
#
#    Do a full interpolation of a 3d variable from GLORYS to a CROCO sigma grid
#    1 - Horizontal interpolation on each GLORYS levels
#    2 - Vertical Interpolation from z to CROCO sigma levels
#
#
#  uout,vout=interp3d_uv(nc,tndx_glo,Nzgoodmin,depth,z_rho,cosa,sina,\
#                iminU,imaxU,jminU,jmaxU,LonU,LatU,coefU,elemU,\
#                iminV,imaxV,jminV,jmaxV,LonV,LatV,coefV,elemV)
#    Do a full interpolation of a 3d U and V from GLORYS to a CROCO sigma grid
#    Do a rotation to align with CROCO grid (if not rectangular)
#
#
#
#  create_inifile(ininame,grdname,title,theta_s,theta_b,hc,N,ini_time,vtransform)
#
#    This function create a Netcdf initial file
#
#
#  create_bryfile(bryname,grdname,title,obc...
#                          theta_s,theta_b,hc,N,...
#                          bry_time,cycle,clobber)
#
#    This function create a Netcdf boundary file
#
#
#  (LonT_bry,LatT_bry,iminT_bry,imaxT_bry,jminT_bry,jmaxT_bry,elemT_bry,coefT_bry,\
#   LonU_bry,LatU_bry,iminU_bry,imaxU_bry,jminU_bry,jmaxU_bry,elemU_bry,coefU_bry,\
#   LonV_bry,LatV_bry,iminV_bry,imaxV_bry,jminV_bry,jmaxV_bry,elemV_bry,coefV_bry)\
#   = get_delaunay_bry(lon_bry,lat_bry,dl,ncglo)
#
#     Get Delaunay linear interpolation pointers and coefficients for each boundary
#
#  ncbry = interp_bry(obctype,ncglo,tndx_glo,ncbry,tndx_bry,\
#               h_bry,theta_s,theta_b,hc,N,vtransform,\
#	        Nzgoodmin,depth,angle_bry,\
#               LonT_bry,LatT_bry,iminT_bry,imaxT_bry,jminT_bry,jmaxT_bry,elemT_bry,coefT_bry,\
#               LonU_bry,LatU_bry,iminU_bry,imaxU_bry,jminU_bry,jmaxU_bry,elemU_bry,coefU_bry,\
#	        LonV_bry,LatV_bry,iminV_bry,imaxV_bry,jminV_bry,jmaxV_bry,elemV_bry,coefV_bry)
#
#
#    Do a full interpolation of a 3d variable from GLORYS to a CROCO sigma grid
#    for each boundary
#
#
########################################################################
#
#
#  This is free software; you can redistribute it and/or modify
#  it under the terms of the GNU General Public License as published
#  by the Free Software Foundation; either version 2 of the License,
#  or (at your option) any later version.
#
#  CROCOTOOLS is distributed in the hope that it will be useful, but
#  WITHOUT ANY WARRANTY; without even the implied warranty of
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#  GNU General Public License for more details.
#
#  You should have received a copy of the GNU General Public License
#  along with this program; if not, write to the Free Software
#  Foundation, Inc., 59 Temple Place, Suite 330, Boston,
#  MA  02111-1307  USA
#
#
######################################################################
#
#

import numpy as np
from netCDF4 import Dataset  as netcdf
from scipy.interpolate import griddata
import os
import sys
import time
#sys.path.insert(0,'/XXX/')
import croco_vgrid as vgrd
import croco_glorys as glor
from interp_Cgrid import *
from scipy.spatial import Delaunay
from   progressbar import *


#
#
#########################################################################
#
#
# get index of particular coordinate
#
#

def geo_idx(dd, dd_array):

    """
     - dd - the decimal degree (latitude or longitude)
     - dd_array - the list of decimal degrees to search.
     search for nearest decimal degree in an array of decimal degrees and return the index.
     np.argmin returns the indices of minium value along an axis.
     so subtract dd from all values in dd_array, take absolute value and find index of minium.
   """
    geo_idx = (np.abs(dd_array - dd)).argmin()
    return geo_idx

#
#
#########################################################################
#
#



#
#
#########################################################################
#
#

def get_tri_coef(X, Y, newX, newY, verbose=0):

    """
    Inputs:
        origin lon and lat 2d arrays (X,Y)
        child lon and lat 2d arrays (newX,newY)
    Ouputs:
        elem - pointers to 2d gridded data (at lonp,latp locations) from
            which the interpolation is computed (3 for each child point)
        coef - linear interpolation coefficients
    Use:
        To subsequently interpolate data from Fp to Fc, the following
        will work:      Fc  = sum(coef.*Fp(elem),3);  This line  should come in place of all
        griddata calls. Since it avoids repeated triangulations and tsearches (that are done
        with every call to griddata) it should be much faster.
    """

    Xp = np.array([X.ravel(), Y.ravel()]).T
    Xc = np.array([newX.ravel(), newY.ravel()]).T


    #Compute Delaunay triangulation
    if verbose==1: tstart = tm.time()
    tri = Delaunay(Xp)
    if verbose==1: print('Delaunay Triangulation', tm.time()-tstart)

    #Compute enclosing simplex and barycentric coordinate (similar to tsearchn in MATLAB)
    npts = Xc.shape[0]
    p = np.zeros((npts,3))

    points = tri.points[tri.vertices[tri.find_simplex(Xc)]]
    if verbose==1: tstart = tm.time()
    for i in progressbar(range(npts),'  Get_tri_coef: ', 40):

        if verbose==1: print(np.float(i)/npts)

        if tri.find_simplex(Xc[i])==-1:  #Point outside triangulation
             p[i,:] = p[i,:] * np.nan

        else:

            if verbose==1: tstart = tm.time()
            A = np.append(np.ones((3,1)),points[i] ,axis=1)
            if verbose==1: print('append A', tm.time()-tstart)

            if verbose==1: tstart = tm.time()
            B = np.append(1., Xc[i])
            if verbose==1: print('append B', tm.time()-tstart)

            if verbose==1: tstart = tm.time()
            p[i,:] = np.linalg.lstsq(A.T,B.T)[0]
            if verbose==1: print('solve', tm.time()-tstart)


    if verbose==1: print('Coef. computation 1', tm.time()-tstart)

    if verbose==1: tstart = tm.time()
    elem = np.reshape(tri.vertices[tri.find_simplex(Xc)],(newX.shape[0],newY.shape[1],3))
    coef = np.reshape(p,(newX.shape[0],newY.shape[1],3))
    if verbose==1: print('Coef. computation 2', tm.time()-tstart)

    return(elem,coef)

#
#
#########################################################################
#
#



#
#
#########################################################################
#
#

def horiz_interp_delaunay(lonold,latold,varold,lonnew,latnew,elem=0,coef=0):

#
# horizontal interpolation
#
    """
    lonold: 2D original longitude matrix
    latold: 2D original longitude matrix
    varold: 2D original datra matrix
    lonnew: 2D new longitude matrix
    latnew: 2D new longitude matrix
    print(len(args*))
    """


    ## Horizontal Interpolation from croco varoables'grid to new grid
    ##: get interpolation coefficients
    if np.all(elem==0):
        [elem,coef] = get_tri_coef(lonold, latold,lonnew, latnew)
        coefnorm=np.sum(coef,axis=2)
        coef=coef/coefnorm[:,:,np.newaxis]
        varnew = np.sum(coef*varold.ravel()[elem],2)
        return elem,coef,varnew
    else:
        varnew = np.sum(coef*varold.ravel()[elem],2)
        return varnew

#
#
#########################################################################
#
#



#
#
######################################################################
###### FUNCTION INTERP_TRACERS #######################################
######################################################################
#
#

def interp_tracers(nc,vname,l,k,imin,imax,jmin,jmax,Lon,Lat,coef,elem):

#
#
#  Remove the missing values from a gridded 2D field
#  and do an horizontal interpolation using Delaunay matrices (coef and elem)
#
#
######################################################################
#
#
#

#
# 1: Read data
#

  if k==-1:
    Vin = np.array(nc[vname][l,jmin:jmax,imin:imax]) 
  else:
    Vin = np.array(nc[vname][l,k,jmin:jmax,imin:imax]) 
  
#
# 2: Remove bad values (using nearest values)
#

  igood=np.where(Vin<1000.)
  ibad=np.where(Vin>=1000.) 
  NzGood=np.size(igood) 
  Nbad=np.size(ibad)
  
  if NzGood==0:

    print('Warning: no good data')
    Vin[:]=np.nan

  elif NzGood<10:

    print('Warning: less than 10 good values')
    Vin[:]=np.mean(Vin[igood])

  else:

    Vin[ibad] = griddata((Lon[igood],Lat[igood]),Vin[igood],(Lon[ibad],Lat[ibad]),method='nearest')

#
# 3: 2D interpolation
#

  Vout = np.sum(coef*Vin.ravel()[elem],2)
  
  return Vout,NzGood


#
#
#
######################################################################
###### END FUNCTION INTERP_TRACERS ###################################
######################################################################
#
#



#
#
######################################################################
##### FUNCTION INTERP3D ##############################################
######################################################################
#
#

def interp3d(nc,vname,tndx_glo,Nzgoodmin,depth,z_rho,imin,imax,jmin,jmax,Lon,Lat,coef,elem):

#
#  Do a full interpolation of a 3d variable from GLORYS to a CROCO sigma grid
#
#  1 - Horizontal interpolation on each GLORYS levels
#  2 - Vertical Interpolation from z to CROCO sigma levels
# 
#
######################################################################
#
#
#

  [N,M,L]=np.shape(z_rho)
  [Nz]=np.shape(depth)

  comp_horzinterp = 1 # if 1 compute horizontal interpolations - 0 use saved matrices (for debugging)

  if comp_horzinterp==1:

    print('Horizontal interpolation over z levels')

    t3d=np.zeros((Nz,M,L))
    kgood=-1

    for k in progressbar(range(Nz),vname+': ', 40):    
 
      (t2d,Nzgood) = interp_tracers(nc,vname,tndx_glo,k,imin,imax,jmin,jmax,Lon,Lat,coef,elem)

      if Nzgood>Nzgoodmin:
        kgood=kgood+1
        t3d[kgood,:,:]=t2d

    t3d=t3d[0:kgood,:,:]
    depth=depth[0:kgood]
    
    np.savez('t3d.npz',t3d=t3d,depth=depth)

  else:

    print('Load matrix...')
    data=np.load('t3d.npz')
    t3d = data['t3d']
    depth = data['depth']

  [Nz]=np.shape(depth)
  Z=-depth

#
#----------------------------------------------------
#  Vertical interpolation
#----------------------------------------------------
#

  print('Vertical interpolation')

#
# Add a layer below the bottom and above the surface to avoid vertical extrapolations 
# and flip the matrices upside down (Z[Nz]=surface) 
#  

  Z=np.flipud(np.concatenate(([100.],Z,[-10000.])))
  [Nz]=np.shape(Z)

  t3d=np.flipud(vgrd.add2layers(t3d))

#
# Do the vertical interpolations 
#

  vout=vgrd.ztosigma(t3d,Z,z_rho)
  
  return vout

#
#
######################################################################
##### END FUNCTION INTERP3D ##########################################
######################################################################
#
#

#
#
######################################################################
##### FUNCTION INTERP3D_UV ###########################################
######################################################################
#
#


def interp3d_uv(nc,tndx_glo,Nzgoodmin,depth,z_rho,cosa,sina,\
                iminU,imaxU,jminU,jmaxU,LonU,LatU,coefU,elemU,\
                iminV,imaxV,jminV,jmaxV,LonV,LatV,coefV,elemV):

#
#  Do a full interpolation of a 3d variable from GLORYS to a CROCO sigma grid
#
#  1 - Horizontal interpolation on each GLORYS levels
#  2 - Vertical Interpolation from z to CROCO sigma levels
# 
#
######################################################################
#
#
#

  [N,M,L]=np.shape(z_rho)
  [Nz]=np.shape(depth)

  comp_horzinterp = 1 # if 1 compute horizontal interpolations - 0 use saved matrices (for debugging)

  if comp_horzinterp==1:

    print('Horizontal interpolation of u and v over z levels')

    u3d=np.zeros((Nz,M,L-1))
    v3d=np.zeros((Nz,M-1,L))
    kgood=-1

    for k in progressbar(range(Nz),' uv : ', 40):    
 
      (u2d,Nzgood_u) = interp_tracers(nc,'u',tndx_glo,k,iminU,imaxU,jminU,jmaxU,LonU,LatU,coefU,elemU)
      (v2d,Nzgood_v) = interp_tracers(nc,'v',tndx_glo,k,iminV,imaxV,jminV,jmaxV,LonV,LatV,coefV,elemV)

      Nzgood=np.min((Nzgood_u,Nzgood_v))

      if Nzgood>Nzgoodmin:
        kgood=kgood+1

#
# Rotation and put to u-points and v-points 
#

        u3d[kgood,:,:]=rho2u_2d(u2d*cosa+v2d*sina)
        v3d[kgood,:,:]=rho2v_2d(v2d*cosa-u2d*sina)

    u3d=u3d[0:kgood,:,:]
    v3d=v3d[0:kgood,:,:]
    depth=depth[0:kgood]
    
    np.savez('u3d.npz',u3d=u3d,v3d=v3d,depth=depth)

  else:

    print('Load matrices...')
    data=np.load('u3d.npz')
    u3d = data['u3d']
    v3d = data['v3d']
    depth = data['depth']

  [Nz]=np.shape(depth)
  Z=-depth

#
#----------------------------------------------------
#  Vertical interpolation
#----------------------------------------------------
#

  print('Vertical interpolation')


#
# Add a layer below the bottom and above the surface to avoid vertical extrapolations 
# and flip the matrices upside down (Z[Nz]=surface) 
#  

  Z=np.flipud(np.concatenate(([100.],Z,[-10000.])))
  [Nz]=np.shape(Z)

  u3d=np.flipud(vgrd.add2layers(u3d))
  v3d=np.flipud(vgrd.add2layers(v3d))

#
# Do the vertical interpolations 
#

  uout=vgrd.ztosigma(u3d,Z,rho2u_3d(z_rho))
  vout=vgrd.ztosigma(v3d,Z,rho2v_3d(z_rho))
  
  return uout,vout

#
#
#
######################################################################
###### END FUNCTION INTERP3D_UV ######################################
######################################################################
#
#



#
#
######################################################################
##### FUNCTION CREATE_INIFILE ########################################
######################################################################
#
#

def create_inifile(ininame,grdname,title,theta_s,theta_b,hc,N,ini_time,vtransform):

#
#
################################################################
#
#
#   This function create the header of a Netcdf climatology 
#   file.
#
#   Input: 
# 
#   ininame      Netcdf initial file name (character string).
#   grdname      Netcdf grid file name (character string).
#   theta_s      S-coordinate surface control parameter.(Real)
#   theta_b      S-coordinate bottom control parameter.(Real)
#   hc           Width (m) of surface or bottom boundary layer
#                where higher vertical resolution is required 
#                during stretching.(Real)
#   N            Number of vertical levels.(Integer)  
#   ini_time     Initial time.(Real) 
#   clobber      Switch to allow or not writing over an existing
#                file.(character string) 
#
# 
#
#
################################################################
#
#

  print(' ')
  print(' Creating the file : '+ininame)
  print(' VTRANSFORM = '+str(vtransform))

#
#  Read the grid file and check the topography
#

  nc=netcdf(grdname, 'r')
  h=nc.variables['h'][:]
  maskr=nc.variables['mask_rho'][:]
  [Mp,Lp]=np.shape(h)
  nc.close()
#
#
#
  hmin=np.min(h[np.where(maskr==1)])
  if vtransform == 1:
    if hc > hmin:
      print('Error: hc ('+hc+' m) > hmin ('+hmin+' m)')


  L=Lp-1
  M=Mp-1
  Np=N+1

#
#  Create the initial file
#

  type = 'INITIAL file'  
  history = 'CROCO' 
  if os.path.exists(ininame):
      os.remove(ininame)

  print('Create: ' +ininame)
  nc=netcdf(ininame,'w',format='NETCDF3_CLASSIC')

#
# Create global attributes
#

  nc.title = title
  nc.history = 'Created '+str(time.ctime(time.time()))
  nc.ini_file = ininame
  nc.grd_file = grdname
  nc.type='CROCO initial file'

#
#  Create dimensions
#

  nc_dim_xi_u=nc.createDimension('xi_u',L)
  nc_dim_xi_v=nc.createDimension('xi_v',Lp)
  nc_dim_xi_rho=nc.createDimension('xi_rho',Lp)
  nc_dim_eta_u=nc.createDimension('eta_u',Mp)
  nc_dim_eta_v=nc.createDimension('eta_v',M)
  nc_dim_eta_rho=nc.createDimension('eta_rho',Mp)
  nc_dim_s_rho=nc.createDimension('s_rho',N)
  nc_dim_s_w=nc.createDimension('s_w',Np)
  nc_dim_tracer=nc.createDimension('tracer',2)
  nc_dim_ini_time=nc.createDimension('ini_time',0)
  nc_dim_one=nc.createDimension('one',1)

#
#  Create variables and attributes
#

  nc_spherical=nc.createVariable('spherical','S1', ('one',))
  nc_spherical.long_name = 'grid type logical switch'
  nc_spherical.flag_values = 'T, F'
  nc_spherical.flag_meanings = 'spherical Cartesian'
#
  nc_Vtransform=nc.createVariable('Vtransform','i4', ('one',))
  nc_Vtransform.long_name = 'vertical terrain-following transformation equation'
#
  nc_Vstretching=nc.createVariable('Vstretching','i4', ('one',))
  nc_Vstretching.long_name = 'vertical terrain-following stretching function'
#
  nc_tstart=nc.createVariable('tstart',np.float64, ('one',))
  nc_tstart.long_name = 'start processing day'
  nc_tstart.units = 'day'
#
  nc_tend=nc.createVariable('tend',np.float64, ('one',))
  nc_tend.long_name = 'end processing day'
  nc_tend.units = 'day'
#
  nc_theta_s=nc.createVariable('theta_s',np.float64, ('one',))
  nc_theta_s.long_name = 'S-coordinate surface control parameter'
  nc_theta_s.units = 'nondimensional'
#
  nc_theta_b=nc.createVariable('theta_b',np.float64, ('one',))
  nc_theta_b.long_name = 'S-coordinate bottom control parameter'
  nc_theta_b.units = 'nondimensional'
#
  nc_Tcline=nc.createVariable('Tcline',np.float64, ('one',))
  nc_Tcline.long_name = 'S-coordinate surface/bottom layer width'
  nc_Tcline.units = 'meter'
#
  nc_hc=nc.createVariable('hc',np.float64, ('one',))
  nc_hc.long_name = 'S-coordinate parameter, critical depth'
  nc_hc.units = 'meter'
#
  nc_sc_r=nc.createVariable('sc_r',np.float64, ('s_rho',))
  nc_sc_r.long_name = 'S-coordinate at RHO-points'
  nc_sc_r.valid_min = -1.
  nc_sc_r.valid_max = 0.
  nc_sc_r.positive = 'up'

  if vtransform == 1:
    nc_sc_r.standard_name = 'ocean_s_coordinate_g1'
  elif vtransform == 2:
    nc_sc_r.standard_name = 'ocean_s_coordinate_g2'     

  nc_sc_r.formula_terms = 's: s_rho C: Cs_r eta: zeta depth: h depth_c: hc'
#
  nc_sc_w=nc.createVariable('sc_w',np.float64, ('s_w',))
  nc_sc_w.long_name = 'S-coordinate at W-points'
  nc_sc_w.valid_min = -1. 
  nc_sc_w.valid_max = 0. 
  nc_sc_w.positive = 'up'

  if vtransform == 1:
    nc_sc_w.standard_name = 'ocean_s_coordinate_g1'
  elif vtransform == 2:
    nc_sc_w.standard_name = 'ocean_s_coordinate_g2'

  nc_sc_w.formula_terms = 's: s_w C: Cs_w eta: zeta depth: h depth_c: hc'
#
  nc_Cs_r=nc.createVariable('Cs_r',np.float64, ('s_rho',))
  nc_Cs_r.long_name = 'S-coordinate stretching curves at RHO-points'
  nc_Cs_r.units = 'nondimensional'
  nc_Cs_r.valid_min = -1
  nc_Cs_r.valid_max = 0
#
  nc_Cs_w=nc.createVariable('Cs_w',np.float64, ('s_w',))
  nc_Cs_w.long_name = 'S-coordinate stretching curves at W-points'
  nc_Cs_w.units = 'nondimensional'
  nc_Cs_w.valid_min = -1
  nc_Cs_w.valid_max = 0
#
  nc_scrum_time=nc.createVariable('scrum_time',np.float64, ('ini_time',))
  nc_scrum_time.long_name = 'ini_time since initialization'
  nc_scrum_time.units = 'second'
#
  nc_ocean_time=nc.createVariable('ocean_time',np.float64, ('ini_time',))
  nc_ocean_time.long_name = 'ini_time since initialization'
  nc_ocean_time.units = 'second'
#
  nc_u=nc.createVariable('u',np.float64, ('ini_time','s_rho','eta_u','xi_u',)) 
  nc_u.long_name = 'u-momentum component'
  nc_u.units = 'meter second-1'
#
  nc_v=nc.createVariable('v',np.float64, ('ini_time','s_rho','eta_v','xi_v',)) 
  nc_v.long_name = 'v-momentum component'
  nc_v.units = 'meter second-1'
#
  nc_ubar=nc.createVariable('ubar',np.float64, ('ini_time','eta_u','xi_u',)) 
  nc_ubar.long_name = 'vertically integrated u-momentum component'
  nc_ubar.units = 'meter second-1'
#
  nc_vbar=nc.createVariable('vbar',np.float64, ('ini_time','eta_v','xi_v',)) 
  nc_vbar.long_name = 'vertically integrated v-momentum component'
  nc_vbar.units = 'meter second-1'
#
  nc_zeta=nc.createVariable('zeta',np.float64, ('ini_time','eta_rho','xi_rho',)) 
  nc_zeta.long_name = 'free-surface'
  nc_zeta.units = 'meter'
#
  nc_temp=nc.createVariable('temp',np.float64, ('ini_time','s_rho','eta_rho','xi_rho',)) 
  nc_temp.long_name = 'potential temperature'
  nc_temp.units = 'Celsius'
#
  nc_salt=nc.createVariable('salt',np.float64, ('ini_time','s_rho','eta_rho','xi_rho',))
  nc_salt.long_name = 'salinity'
  nc_salt.units = 'PSU'

#
# Compute S coordinates
#

  (sc_r,Cs_r,sc_w,Cs_w) = vgrd.scoordinate(theta_s,theta_b,N,hc,vtransform)
  print('vtransform = '+str(vtransform))

#
# Write variables
#

  nc_spherical[:]='T'
  nc_Vtransform[:]=vtransform
  nc_Vstretching[:]=1
  nc_tstart[:] =  ini_time 
  nc_tend[:] =  ini_time 
  nc_theta_s[:] =  theta_s 
  nc_theta_b[:] =  theta_b 
  nc_Tcline[:] =  hc 
  nc_hc[:] =  hc 
  nc_sc_r[:] =  sc_r 
  nc_Cs_r[:] =  Cs_r 
  nc_scrum_time[0] = ini_time*24.*3600. 
  nc_ocean_time[0] = ini_time*24.*3600. 
  nc_u[:] =  0 
  nc_v[:] =  0 
  nc_zeta[:] =  0 
  nc_ubar[:] =  0 
  nc_vbar[:] =  0 
  nc_temp[:] =  0 
  nc_salt[:] =  0 
#
  nc.close()
#  
  return


#
#
#
######################################################################
###### END FUNCTION CREATE_INIFILE ###################################
######################################################################
#
#



#
#
######################################################################
##### FUNCTION CREATE_BRYFILE ########################################
######################################################################
#
#

def create_bryfile(bryname,grdname,title,obc,\
                        theta_s,theta_b,hc,N,\
                        bry_time,cycle,vtransform):

#
#
################################################################
#
# function create_bryfile(bryname,grdname,title,obc...
#                          theta_s,theta_b,hc,N,...
#                          bry_time,cycle,clobber)
#
#   This function create the header of a Netcdf climatology 
#   file.
#
#   Input:
#
#   bryname      Netcdf climatology file name (character string).
#   grdname      Netcdf grid file name (character string).
#   obc          open boundaries flag (1=open , [S E N W]).
#   theta_s      S-coordinate surface control parameter.(Real)
#   theta_b      S-coordinate bottom control parameter.(Real)
#   hc           Width (m) of surface or bottom boundary layer 
#                where higher vertical resolution is required 
#                during stretching.(Real)
#   N            Number of vertical levels.(Integer)
#   bry_time     time.(vector)
#   cycle        Length (days) for cycling the climatology.(Real)
#
#
################################################################
#
#

  print(' ')
  print(' Creating the file : '+bryname)
  print(' ')
  print(' VTRANSFORM = '+str(vtransform))

#
#  Read the grid file and check the topography
#

  nc=netcdf(grdname, 'r')
  h=nc.variables['h'][:]
  maskr=nc.variables['mask_rho'][:]
  [Mp,Lp]=np.shape(h)
  nc.close()

#
#
#

  hmin=np.min(h[np.where(maskr==1)])
  if vtransform == 1:
    if hc > hmin:
      print('Error: hc ('+hc+' m) > hmin ('+hmin+' m)')


  L=Lp-1
  M=Mp-1
  Np=N+1

  Nt=0

#
#  Create the boundary file
#
  type = 'BOUNDARY file'
  history = 'CROCO' 

  if os.path.exists(bryname):
    os.remove(bryname)

  print('Create: ' +bryname)
  nc=netcdf(bryname,'w',format='NETCDF4')

#
# set global attributes
#

  nc.type='CROCO boundary file'
  nc.history = 'Created '+str(time.ctime(time.time()))
  nc.title = title
  nc.bry_file = bryname
  nc.grd_file = grdname

#
#  Create dimensions
#

  nc_dim_xi_u=nc.createDimension('xi_u',L)
  nc_dim_xi_v=nc.createDimension('xi_v',Lp)
  nc_dim_xi_rho=nc.createDimension('xi_rho',Lp)
  nc_dim_eta_u=nc.createDimension('eta_u',Mp)
  nc_dim_eta_v=nc.createDimension('eta_v',M)
  nc_dim_eta_rho=nc.createDimension('eta_rho',Mp)
  nc_dim_s_rho=nc.createDimension('s_rho',N)
  nc_dim_s_w=nc.createDimension('s_w',Np)
  nc_dim_tracer=nc.createDimension('tracer',2)
  nc_dim_bry_time=nc.createDimension('bry_time',Nt)
  nc_dim_tclm_time=nc.createDimension('tclm_time',Nt)
  nc_dim_temp_time=nc.createDimension('temp_time',Nt)
  nc_dim_sclm_time=nc.createDimension('sclm_time',Nt)
  nc_dim_salt_time=nc.createDimension('salt_time',Nt)
  nc_dim_uclm_time=nc.createDimension('uclm_time',Nt)
  nc_dim_vclm_time=nc.createDimension('vclm_time',Nt)
  nc_dim_v2d_time=nc.createDimension('v2d_time',Nt)
  nc_dim_v3d_time=nc.createDimension('v3d_time',Nt)
  nc_dim_ssh_time=nc.createDimension('ssh_time',Nt)
  nc_dim_zeta_time=nc.createDimension('zeta_time',Nt)
  nc_dim_one=nc.createDimension('one',1)

#
#  Create variables and attributes
#

  nc_spherical=nc.createVariable('spherical','S1', ('one',))
  nc_spherical.long_name = 'grid type logical switch'
  nc_spherical.flag_values = 'T, F'
  nc_spherical.flag_meanings = 'spherical Cartesian'
#
  nc_Vtransform=nc.createVariable('Vtransform','i4', ('one',))
  nc_Vtransform.long_name = 'vertical terrain-following transformation equation'
#
  nc_Vstretching=nc.createVariable('Vstretching','i4', ('one',))
  nc_Vstretching.long_name = 'vertical terrain-following stretching function'
#
  nc_tstart=nc.createVariable('tstart',np.float64, ('one',))
  nc_tstart.long_name = 'start processing day'
  nc_tstart.units = 'day'
#
  nc_tend=nc.createVariable('tend',np.float64, ('one',))
  nc_tend.long_name = 'end processing day'
  nc_tend.units = 'day'
#
  nc_theta_s=nc.createVariable('theta_s',np.float64, ('one',))
  nc_theta_s.long_name = 'S-coordinate surface control parameter'
  nc_theta_s.units = 'nondimensional'
#
  nc_theta_b=nc.createVariable('theta_b',np.float64, ('one',))
  nc_theta_b.long_name = 'S-coordinate bottom control parameter'
  nc_theta_b.units = 'nondimensional'
#
  nc_Tcline=nc.createVariable('Tcline',np.float64, ('one',))
  nc_Tcline.long_name = 'S-coordinate surface/bottom layer width'
  nc_Tcline.units = 'meter'
#
  nc_hc=nc.createVariable('hc',np.float64, ('one',))
  nc_hc.long_name = 'S-coordinate parameter, critical depth'
  nc_hc.units = 'meter'
#
  nc_sc_r=nc.createVariable('sc_r',np.float64, ('s_rho',))
  nc_sc_r.long_name = 'S-coordinate at RHO-points'
  nc_sc_r.valid_min = -1.
  nc_sc_r.valid_max = 0.
  nc_sc_r.positive = 'up'

  if vtransform == 1:
    nc_sc_r.standard_name = 'ocean_s_coordinate_g1'
  elif vtransform == 2:
    nc_sc_r.standard_name = 'ocean_s_coordinate_g2'     

  nc_sc_r.formula_terms = 's: s_rho C: Cs_r eta: zeta depth: h depth_c: hc'
#
  nc_sc_w=nc.createVariable('sc_w',np.float64, ('s_w',))
  nc_sc_w.long_name = 'S-coordinate at W-points'
  nc_sc_w.valid_min = -1. 
  nc_sc_w.valid_max = 0. 
  nc_sc_w.positive = 'up'

  if vtransform == 1:
    nc_sc_w.standard_name = 'ocean_s_coordinate_g1'
  elif vtransform == 2:
    nc_sc_w.standard_name = 'ocean_s_coordinate_g2'

  nc_sc_w.formula_terms = 's: s_w C: Cs_w eta: zeta depth: h depth_c: hc'
#
  nc_Cs_r=nc.createVariable('Cs_r',np.float64, ('s_rho',))
  nc_Cs_r.long_name = 'S-coordinate stretching curves at RHO-points'
  nc_Cs_r.units = 'nondimensional'
  nc_Cs_r.valid_min = -1
  nc_Cs_r.valid_max = 0
#
  nc_Cs_w=nc.createVariable('Cs_w',np.float64, ('s_w',))
  nc_Cs_w.long_name = 'S-coordinate stretching curves at W-points'
  nc_Cs_w.units = 'nondimensional'
  nc_Cs_w.valid_min = -1
  nc_Cs_w.valid_max = 0
#
  nc_bry_time=nc.createVariable('bry_time',np.float64, ('bry_time',))
  nc_bry_time.long_name = 'time for boundary climatology'
  nc_bry_time.units = 'day'
  nc_bry_time.calendar = 'XXX days in every year'
  nc_bry_time.cycle_length = cycle
#
  nc_tclm_time=nc.createVariable('tclm_time',np.float64, ('tclm_time',))
  nc_tclm_time.long_name = 'time for temperature climatology'
  nc_tclm_time.units = 'day'
  nc_tclm_time.calendar = 'XXX days in every year'
  nc_tclm_time.cycle_length = cycle
#
  nc_temp_time=nc.createVariable('temp_time',np.float64, ('temp_time',))
  nc_temp_time.long_name = 'time for temperature climatology'
  nc_temp_time.units = 'day'
  nc_temp_time.calendar = 'XXX days in every year'
  nc_temp_time.cycle_length = cycle
#
  nc_sclm_time=nc.createVariable('sclm_time',np.float64, ('sclm_time',))
  nc_sclm_time.long_name = 'time for salinity climatology'
  nc_sclm_time.units = 'day'
  nc_sclm_time.calendar = 'XXX days in every year'
  nc_sclm_time.cycle_length = cycle
#
  nc_salt_time=nc.createVariable('salt_time',np.float64, ('salt_time',))
  nc_salt_time.long_name = 'time for salinity climatology'
  nc_salt_time.units = 'day'
  nc_salt_time.calendar = 'XXX days in every year'
  nc_salt_time.cycle_length = cycle
#
  nc_uclm_time=nc.createVariable('uclm_time',np.float64, ('uclm_time',))
  nc_uclm_time.long_name = 'time climatological u'
  nc_uclm_time.units = 'day'
  nc_uclm_time.calendar = 'XXX days in every year'
  nc_uclm_time.cycle_length = cycle
#
  nc_vclm_time=nc.createVariable('vclm_time',np.float64, ('vclm_time',))
  nc_vclm_time.long_name = 'time climatological v'
  nc_vclm_time.units = 'day'
  nc_vclm_time.calendar = 'XXX days in every year'
  nc_vclm_time.cycle_length = cycle
#
  nc_v2d_time=nc.createVariable('v2d_time',np.float64, ('v2d_time',))
  nc_v2d_time.long_name = 'time for 2D velocity climatology'
  nc_v2d_time.units = 'day'
  nc_v2d_time.calendar = 'XXX days in every year'
  nc_v2d_time.cycle_length = cycle
#
  nc_v3d_time=nc.createVariable('v3d_time',np.float64, ('v3d_time',))
  nc_v3d_time.long_name = 'time for 3D velocity climatology'
  nc_v3d_time.units = 'day'
  nc_v3d_time.calendar = 'XXX days in every year'
  nc_v3d_time.cycle_length = cycle
#
  nc_ssh_time=nc.createVariable('ssh_time',np.float64, ('ssh_time',))
  nc_ssh_time.long_name = 'time for sea surface height'
  nc_ssh_time.units = 'day'
  nc_ssh_time.calendar = 'XXX days in every year'
  nc_ssh_time.cycle_length = cycle
#
  nc_zeta_time=nc.createVariable('zeta_time',np.float64, ('zeta_time',))
  nc_zeta_time.long_name = 'time for sea surface height'
  nc_zeta_time.units = 'day'
  nc_zeta_time.calendar = 'XXX days in every year'
  nc_zeta_time.cycle_length = cycle
#
  if obc[0]==1:
#
#   Southern boundary
#
    nc_temp_south=nc.createVariable('temp_south',np.float64, ('temp_time','s_rho','xi_rho',))
    nc_temp_south.long_name = 'southern boundary potential temperature'
    nc_temp_south.units = 'Celsius'
    nc_temp_south.coordinates = 'lon_rho s_rho temp_time'
#
    nc_salt_south=nc.createVariable('salt_south',np.float64, ('salt_time','s_rho','xi_rho',))
    nc_salt_south.long_name = 'southern boundary salinity'
    nc_salt_south.units = 'PSU'
    nc_salt_south.coordinates = 'lon_rho s_rho salt_time'
#
    nc_u_south=nc.createVariable('u_south',np.float64, ('v3d_time','s_rho','xi_u',))
    nc_u_south.long_name = 'southern boundary u-momentum component'
    nc_u_south.units = 'meter second-1'
    nc_u_south.coordinates = 'lon_u s_rho u_bry_time'
#
    nc_v_south=nc.createVariable('v_south',np.float64, ('v3d_time','s_rho','xi_rho',))
    nc_v_south.long_name = 'southern boundary v-momentum component'
    nc_v_south.units = 'meter second-1'
    nc_v_south.coordinates = 'lon_v s_rho vclm_time'
#
    nc_ubar_south=nc.createVariable('ubar_south',np.float64, ('v2d_time','xi_u',))
    nc_ubar_south.long_name = 'southern boundary vertically integrated u-momentum component'
    nc_ubar_south.units = 'meter second-1'
    nc_ubar_south.coordinates = 'lon_u uclm_time'
#
    nc_vbar_south=nc.createVariable('vbar_south',np.float64, ('v2d_time','xi_rho',))
    nc_vbar_south.long_name = 'southern boundary vertically integrated v-momentum component'
    nc_vbar_south.units = 'meter second-1'
    nc_vbar_south.coordinates = 'lon_v vclm_time'
#
    nc_zeta_south=nc.createVariable('zeta_south',np.float64, ('zeta_time','xi_rho',))
    nc_zeta_south.long_name = 'southern boundary sea surface height'
    nc_zeta_south.units = 'meter'
    nc_zeta_south.coordinates = 'lon_rho zeta_time'
#

  if obc[1]==1:
#
#   Eastern boundary
#
    nc_temp_east=nc.createVariable('temp_east',np.float64, ('temp_time','s_rho','eta_rho',))
    nc_temp_east.long_name = 'eastern boundary potential temperature'
    nc_temp_east.units = 'Celsius'
    nc_temp_east.coordinates = 'lat_rho s_rho temp_time'
#
    nc_salt_east=nc.createVariable('salt_east',np.float64, ('salt_time','s_rho','eta_rho',))
    nc_salt_east.long_name = 'eastern boundary salinity'
    nc_salt_east.units = 'PSU'
    nc_salt_east.coordinates = 'lat_rho s_rho salt_time'
#
    nc_u_east=nc.createVariable('u_east',np.float64, ('v3d_time','s_rho','eta_rho',))
    nc_u_east.long_name = 'eastern boundary u-momentum component'
    nc_u_east.units = 'meter second-1'
    nc_u_east.coordinates = 'lat_u s_rho u_bry_time'
#
    nc_v_east=nc.createVariable('v_east',np.float64, ('v3d_time','s_rho','eta_v',))
    nc_v_east.long_name = 'eastern boundary v-momentum component'
    nc_v_east.units = 'meter second-1'
    nc_v_east.coordinates = 'lat_v s_rho vclm_time'
#
    nc_ubar_east=nc.createVariable('ubar_east',np.float64, ('v2d_time','eta_rho',))
    nc_ubar_east.long_name = 'eastern boundary vertically integrated u-momentum component'
    nc_ubar_east.units = 'meter second-1'
    nc_ubar_east.coordinates = 'lat_u uclm_time'
#
    nc_vbar_east=nc.createVariable('vbar_east',np.float64, ('v2d_time','eta_v',))
    nc_vbar_east.long_name = 'eastern boundary vertically integrated v-momentum component'
    nc_vbar_east.units = 'meter second-1'
    nc_vbar_east.coordinates = 'lat_v vclm_time'
#
    nc_zeta_east=nc.createVariable('zeta_east',np.float64, ('zeta_time','eta_rho',))
    nc_zeta_east.long_name = 'eastern boundary sea surface height'
    nc_zeta_east.units = 'meter'
    nc_zeta_east.coordinates = 'lat_rho zeta_time'
#

  if obc[2]==1:
#
#   Northern boundary
#
    nc_temp_north=nc.createVariable('temp_north',np.float64, ('temp_time','s_rho','xi_rho',))
    nc_temp_north.long_name = 'northern boundary potential temperature'
    nc_temp_north.units = 'Celsius'
    nc_temp_north.coordinates = 'lon_rho s_rho temp_time'
#
    nc_salt_north=nc.createVariable('salt_north',np.float64, ('salt_time','s_rho','xi_rho',))
    nc_salt_north.long_name = 'northern boundary salinity'
    nc_salt_north.units = 'PSU'
    nc_salt_north.coordinates = 'lon_rho s_rho salt_time'
#
    nc_u_north=nc.createVariable('u_north',np.float64, ('v3d_time','s_rho','xi_u',))
    nc_u_north.long_name = 'northern boundary u-momentum component'
    nc_u_north.units = 'meter second-1'
    nc_u_north.coordinates = 'lon_u s_rho u_bry_time'
#
    nc_v_north=nc.createVariable('v_north',np.float64, ('v3d_time','s_rho','xi_rho',))
    nc_v_north.long_name = 'northern boundary v-momentum component'
    nc_v_north.units = 'meter second-1'
    nc_v_north.coordinates = 'lon_v s_rho vclm_time'
#
    nc_ubar_north=nc.createVariable('ubar_north',np.float64, ('v2d_time','xi_u',))
    nc_ubar_north.long_name = 'northern boundary vertically integrated u-momentum component'
    nc_ubar_north.units = 'meter second-1'
    nc_ubar_north.coordinates = 'lon_u uclm_time'
#
    nc_vbar_north=nc.createVariable('vbar_north',np.float64, ('v2d_time','xi_rho',))
    nc_vbar_north.long_name = 'northern boundary vertically integrated v-momentum component'
    nc_vbar_north.units = 'meter second-1'
    nc_vbar_north.coordinates = 'lon_v vclm_time'

    nc_zeta_north=nc.createVariable('zeta_north',np.float64, ('zeta_time','xi_rho',))
    nc_zeta_north.long_name = 'northern boundary sea surface height'
    nc_zeta_north.units = 'meter'
    nc_zeta_north.coordinates = 'lon_rho zeta_time'
#

  if obc[3]==1:
#
#   Western boundary
#
    nc_temp_west=nc.createVariable('temp_west',np.float64, ('temp_time','s_rho','eta_rho',))
    nc_temp_west.long_name = 'western boundary potential temperature'
    nc_temp_west.units = 'Celsius'
    nc_temp_west.coordinates = 'lat_rho s_rho temp_time'
#
    nc_salt_west=nc.createVariable('salt_west',np.float64, ('salt_time','s_rho','eta_rho',))
    nc_salt_west.long_name = 'western boundary salinity'
    nc_salt_west.units = 'PSU'
    nc_salt_west.coordinates = 'lat_rho s_rho salt_time'
#
    nc_u_west=nc.createVariable('u_west',np.float64, ('v3d_time','s_rho','eta_rho',))
    nc_u_west.long_name = 'western boundary u-momentum component'
    nc_u_west.units = 'meter second-1'
    nc_u_west.coordinates = 'lat_u s_rho u_bry_time'
#
    nc_v_west=nc.createVariable('v_west',np.float64, ('v3d_time','s_rho','eta_v',))
    nc_v_west.long_name = 'western boundary v-momentum component'
    nc_v_west.units = 'meter second-1'
    nc_v_west.coordinates = 'lat_v s_rho vclm_time'
#
    nc_ubar_west=nc.createVariable('ubar_west',np.float64, ('v2d_time','eta_rho',))
    nc_ubar_west.long_name = 'western boundary vertically integrated u-momentum component'
    nc_ubar_west.units = 'meter second-1'
    nc_ubar_west.coordinates = 'lat_u uclm_time'
#
    nc_vbar_west=nc.createVariable('vbar_west',np.float64, ('v2d_time','eta_v',))
    nc_vbar_west.long_name = 'western boundary vertically integrated v-momentum component'
    nc_vbar_west.units = 'meter second-1'
    nc_vbar_west.coordinates = 'lat_v vclm_time'
#
    nc_zeta_west=nc.createVariable('zeta_west',np.float64, ('zeta_time','eta_rho',))
    nc_zeta_west.long_name = 'western boundary sea surface height'
    nc_zeta_west.units = 'meter'
    nc_zeta_west.coordinates = 'lat_rho zeta_time'
#

#
# Compute S coordinates
#

  (sc_r,Cs_r,sc_w,Cs_w) = vgrd.scoordinate(theta_s,theta_b,N,hc,vtransform)
  print('vtransform = '+str(vtransform))

#
# Write variables
#
  nc_spherical[:]='T'
  nc_Vtransform[:]=vtransform
  nc_Vstretching[:]=1
  nc_tstart[:] =   np.min(bry_time) 
  nc_tend[:] =     np.max(bry_time) 
  nc_theta_s[:] =  theta_s 
  nc_theta_b[:] =  theta_b 
  nc_Tcline[:] =  hc 
  nc_hc[:] =  hc 
  nc_sc_r[:] = sc_r
  nc_sc_w[:] = sc_w
  nc_Cs_r[:] = Cs_r  
  nc_Cs_w[:] = Cs_w
  nc_tclm_time[:] =  bry_time 
  nc_temp_time[:] =  bry_time 
  nc_sclm_time[:] =  bry_time 
  nc_salt_time[:] =  bry_time 
  nc_uclm_time[:] =  bry_time 
  nc_vclm_time[:] =  bry_time 
  nc_v2d_time[:]  =  bry_time 
  nc_v3d_time[:]  =  bry_time 
  nc_ssh_time[:]  =  bry_time
  nc_zeta_time[:] =  bry_time
  nc_bry_time[:]  =  bry_time 

  if obc[0]==1:
    nc_u_south[:] =  0. 
    nc_v_south[:] =  0. 
    nc_ubar_south[:] =  0. 
    nc_vbar_south[:] =  0. 
    nc_zeta_south[:] =  0. 
    nc_temp_south[:] =  0. 
    nc_salt_south[:] =  0.

  if obc[1]==1:
    nc_u_east[:] =  0. 
    nc_v_east[:] =  0. 
    nc_ubar_east[:] =  0. 
    nc_vbar_east[:] =  0. 
    nc_zeta_east[:] =  0. 
    nc_temp_east[:] =  0. 
    nc_salt_east[:] =  0.

  if obc[2]==1:
    nc_u_north[:] =  0. 
    nc_v_north[:] =  0. 
    nc_ubar_north[:] =  0. 
    nc_vbar_north[:] =  0. 
    nc_zeta_north[:] =  0. 
    nc_temp_north[:] =  0. 
    nc_salt_north[:] =  0.

  if obc[3]==1:
    nc_u_west[:] =  0. 
    nc_v_west[:] =  0. 
    nc_ubar_west[:] =  0. 
    nc_vbar_west[:] =  0. 
    nc_zeta_west[:] =  0. 
    nc_temp_west[:] =  0. 
    nc_salt_west[:] =  0.
  
  nc.close()

  return

#
#
#
######################################################################
###### END FUNCTION CREATE_BRYFILE ###################################
######################################################################
#
#



#
#
######################################################################
##### FUNCTION GET_DELAUNAY_BRY ######################################
######################################################################
#
#

def get_delaunay_bry(lon_bry,lat_bry,dl,ncglo):

#
#
################################################################
#
# function get_delaunay_bry(lon_bry,lat_bry,dl,ncglo)
#
#   This function computes the delaunay matrices for the interpolations 
#   at the boundaies
#
#   Input:
#
#   lon_bry      Longitudes of the boundary (vector).
#   lat_bry      Latitudes of the boundary (vector).
#   dl           extra extension in deg (real).
#   ncglo        netcdf structure poiting to the GLORYS file
#
#   Output:
#
#   LonT_bry,LatT_bry,iminT_bry,imaxT_bry,jminT_bry,jmaxT_bry,elemT_bry,coefT_bry,
#   LonU_bry,LatU_bry,iminU_bry,imaxU_bry,jminU_bry,jmaxU_bry,elemU_bry,coefU_bry,
#   LonV_bry,LatV_bry,iminV_bry,imaxV_bry,jminV_bry,jmaxV_bry,elemV_bry,coefV_bry
#
################################################################
#
#

    comp_delaunay=1

    lonmin=np.min(lon_bry)-dl
    lonmax=np.max(lon_bry)+dl
    latmin=np.min(lat_bry)-dl
    latmax=np.max(lat_bry)+dl
  

#
# get GLORYS positions and indices at T-points
#

    latT    = np.array(ncglo['latT'][:])
    lonT    = np.array(ncglo['lonT'][:])

    iminT_bry   = glor.geo_idx(lonmin, lonT)
    imaxT_bry   = glor.geo_idx(lonmax, lonT)
    jminT_bry   = glor.geo_idx(latmin, latT)
    jmaxT_bry   = glor.geo_idx(latmax, latT) 
  
    lonT=lonT[iminT_bry:imaxT_bry]
    latT=latT[jminT_bry:jmaxT_bry]
    (LonT_bry,LatT_bry)=np.meshgrid(lonT,latT)

#
# get GLORYS positions and indices at U-points
#

    latU    = np.array(ncglo['latU'][:])
    lonU    = np.array(ncglo['lonU'][:])

    iminU_bry   = glor.geo_idx(lonmin, lonU)
    imaxU_bry   = glor.geo_idx(lonmax, lonU)
    jminU_bry   = glor.geo_idx(latmin, latU)
    jmaxU_bry   = glor.geo_idx(latmax, latU) 

    lonU=lonU[iminU_bry:imaxU_bry]
    latU=latU[jminU_bry:jmaxU_bry]
    (LonU_bry,LatU_bry)=np.meshgrid(lonU,latU)

#
# get GLORYS positions and indices at V-points
#

    latV    = np.array(ncglo['latV'][:])
    lonV    = np.array(ncglo['lonV'][:])

    iminV_bry   = glor.geo_idx(lonmin, lonV)
    imaxV_bry   = glor.geo_idx(lonmax, lonV)
    jminV_bry   = glor.geo_idx(latmin, latV)
    jmaxV_bry   = glor.geo_idx(latmax, latV) 
  
    lonV=lonV[iminV_bry:imaxV_bry]
    latV=latV[jminV_bry:jmaxV_bry]
    (LonV_bry,LatV_bry)=np.meshgrid(lonV,latV)

#
# Horizontal and vertical interp/extrapolations 
#

    print(' ')
    print(' Interpolations / extrapolations')
    print(' ')

#
# Get the 2D interpolation coefficients
#

    if comp_delaunay==1:
 
      print('Compute Delaunay triangulation from GLORYS T-points to CROCO rho_points...')
      [elemT_bry,coefT_bry] = glor.get_tri_coef(LonT_bry,LatT_bry,lon_bry,lat_bry)
      coefnorm=np.sum(coefT_bry,axis=2)
      coefT_bry=coefT_bry/coefnorm[:,:,np.newaxis]

      print('Compute Delaunay triangulation from GLORYS U-points to CROCO rho_points...')
      [elemU_bry,coefU_bry] = glor.get_tri_coef(LonU_bry,LatU_bry,lon_bry,lat_bry)
      coefnorm=np.sum(coefU_bry,axis=2)
      coefU_bry=coefU_bry/coefnorm[:,:,np.newaxis]

      print('Compute Delaunay triangulation from GLORYS V-points to CROCO rho_points...')
      [elemV_bry,coefV_bry] = glor.get_tri_coef(LonV_bry,LatV_bry,lon_bry,lat_bry)
      coefnorm=np.sum(coefV_bry,axis=2)
      coefV_bry=coefV_bry/coefnorm[:,:,np.newaxis]

#
# Save the Delaunay triangulation matrices
#

      np.savez('coeffs_bry.npz',\
             coefT_bry=coefT_bry,elemT_bry=elemT_bry,\
             coefU_bry=coefU_bry,elemU_bry=elemU_bry,\
	     coefV_bry=coefV_bry,elemV_bry=elemV_bry)

    else:

#
# Load the Delaunay triangulation matrices
#

      print('Load Delaunay triangulation...')
      data=np.load('coeffs_bry.npz')
      coefT_bry = data['coefT_bry']
      elemT_bry = data['elemT_bry']
      coefU_bry = data['coefU_bry']
      elemU_bry = data['elemU_bry']
      coefV_bry = data['coefV_bry']
      elemV_bry = data['elemV_bry']

    print('Delaunay triangulation done')

    return(LonT_bry,LatT_bry,iminT_bry,imaxT_bry,jminT_bry,jmaxT_bry,elemT_bry,coefT_bry,\
           LonU_bry,LatU_bry,iminU_bry,imaxU_bry,jminU_bry,jmaxU_bry,elemU_bry,coefU_bry,\
	   LonV_bry,LatV_bry,iminV_bry,imaxV_bry,jminV_bry,jmaxV_bry,elemV_bry,coefV_bry)



#
#
#
######################################################################
###### END FUNCTION GET_DELAUNAY_BRY #################################
######################################################################
#
#



#
#
######################################################################
##### FUNCTION INTERP_BRY ############################################
######################################################################
#
#

def interp_bry(obctype,ncglo,tndx_glo,ncbry,tndx_bry,\
               h_bry,theta_s,theta_b,hc,N,vtransform,\
	       Nzgoodmin,depth,angle_bry,\
               LonT_bry,LatT_bry,iminT_bry,imaxT_bry,jminT_bry,jmaxT_bry,elemT_bry,coefT_bry,\
               LonU_bry,LatU_bry,iminU_bry,imaxU_bry,jminU_bry,jmaxU_bry,elemU_bry,coefU_bry,\
	       LonV_bry,LatV_bry,iminV_bry,imaxV_bry,jminV_bry,jmaxV_bry,elemV_bry,coefV_bry):


#
#
################################################################
# function interp_bry(obctype,ncglo,tndx_glo,ncbry,tndx_bry,\
#              h_bry,theta_s,theta_b,hc,N,vtransform,\
#	       Nzgoodmin,depth,angle_bry,\
#              LonT_bry,LatT_bry,iminT_bry,imaxT_bry,jminT_bry,jmaxT_bry,elemT_bry,coefT_bry,\
#              LonU_bry,LatU_bry,iminU_bry,imaxU_bry,jminU_bry,jmaxU_bry,elemU_bry,coefU_bry,\
#	       LonV_bry,LatV_bry,iminV_bry,imaxV_bry,jminV_bry,jmaxV_bry,elemV_bry,coefV_bry)
#
#
#   Interpolates all the variable for one boundary
#
#   Input:
#
#              obctype,ncglo,tndx_glo,ncbry,tndx_bry,
#              h_bry,theta_s,theta_b,hc,N,vtransform,
#	       Nzgoodmin,depth,angle_bry,
#              LonT_bry,LatT_bry,iminT_bry,imaxT_bry,jminT_bry,jmaxT_bry,elemT_bry,coefT_bry,
#              LonU_bry,LatU_bry,iminU_bry,imaxU_bry,jminU_bry,jmaxU_bry,elemU_bry,coefU_bry,
#	       LonV_bry,LatV_bry,iminV_bry,imaxV_bry,jminV_bry,jmaxV_bry,elemV_bry,coefV_bry
#
#   Output:
#
#   ncbry    Netcdf file structure
#
################################################################
#
#

  
#
#
# 1: SSH 
#
#

    print('Interpolate SSH...')
  
    (zeta_bry,NzGood) = glor.interp_tracers(ncglo,'ssh',tndx_glo,-1,\
                                        iminT_bry,imaxT_bry,jminT_bry,jmaxT_bry,\
				        LonT_bry,LatT_bry,coefT_bry,elemT_bry)

      

#
# Get CROCO sigma coordinate at rho and w points (using zeta)
#

    z_rho=vgrd.zlevs(h_bry,zeta_bry,theta_s,theta_b,hc,N,'r',vtransform)

    z_w=vgrd.zlevs(h_bry,zeta_bry,theta_s,theta_b,hc,N,'w',vtransform)

#
#
# 2: Temperature
#
#

    print('Interpolate Temperature...')

    temp_bry=glor.interp3d(ncglo,'temp',tndx_glo,Nzgoodmin,depth,z_rho,\
                             iminT_bry,imaxT_bry,jminT_bry,jmaxT_bry,\
			     LonT_bry,LatT_bry,coefT_bry,elemT_bry)

#
#
# 3: Salinity 
#
#

    print('Interpolate Salinity...')

    salt_bry=glor.interp3d(ncglo,'salt',tndx_glo,Nzgoodmin,depth,z_rho,\
                             iminT_bry,imaxT_bry,jminT_bry,jmaxT_bry,\
			     LonT_bry,LatT_bry,coefT_bry,elemT_bry)

#
#
# 4: U and V 
#
# (interpolate on z levels at rho points - rotate to align with the grid - 
#  put to u and v points - vertical interpolation to sigma grid)
#
#

    cosa=np.cos(angle_bry)
    sina=np.sin(angle_bry)

    [u_bry,v_bry]=glor.interp3d_uv(ncglo,tndx_glo,Nzgoodmin,depth,z_rho,cosa,sina,\
                    iminU_bry,imaxU_bry,jminU_bry,jmaxU_bry,\
		    LonU_bry,LatU_bry,coefU_bry,elemU_bry,\
                    iminV_bry,imaxV_bry,jminV_bry,jmaxV_bry,\
		    LonV_bry,LatV_bry,coefV_bry,elemV_bry)


#
#
# 5: UBAR and VBAR 
#
# Here it could be nice to get the barotropic transport from GLORYS, to put it on CROCO grid,
# and to correct u and v accordingly...
# But let's start simple and just integrate u and v on CROCO grid.
#
#

    (ubar_bry,h0)=vgrd.vintegr(u_bry,rho2u_3d(z_w),rho2u_3d(z_rho),np.nan,np.nan)/rho2u_2d(h_bry)
    (vbar_bry,h0)=vgrd.vintegr(v_bry,rho2v_3d(z_w),rho2v_3d(z_rho),np.nan,np.nan)/rho2v_2d(h_bry)


    if obctype=='s':

      ncbry['zeta_south'][tndx_bry,:]=zeta_bry[0,:]
      ncbry['temp_south'][tndx_bry,:,:]=temp_bry[:,0,:]
      ncbry['salt_south'][tndx_bry,:,:]=salt_bry[:,0,:]
      ncbry['u_south'][tndx_bry,:,:]=u_bry[:,0,:]
      ncbry['v_south'][tndx_bry,:,:]=v_bry[:,0,:]
      ncbry['ubar_south'][tndx_bry,:]=ubar_bry[0,:]
      ncbry['vbar_south'][tndx_bry,:]=vbar_bry[0,:]
      
    elif obctype=='n':

      ncbry['zeta_north'][tndx_bry,:]=zeta_bry[-1,:]
      ncbry['temp_north'][tndx_bry,:,:]=temp_bry[:,-1,:]
      ncbry['salt_north'][tndx_bry,:,:]=salt_bry[:,-1,:]
      ncbry['u_north'][tndx_bry,:,:]=u_bry[:,-1,:]
      ncbry['v_north'][tndx_bry,:,:]=v_bry[:,-1,:]
      ncbry['ubar_north'][tndx_bry,:]=ubar_bry[-1,:]
      ncbry['vbar_north'][tndx_bry,:]=vbar_bry[-1,:]

    elif obctype=='e':

      ncbry['zeta_east'][tndx_bry,:]=zeta_bry[:,0]
      ncbry['temp_east'][tndx_bry,:,:]=temp_bry[:,:,0]
      ncbry['salt_east'][tndx_bry,:,:]=salt_bry[:,:,0]
      ncbry['u_east'][tndx_bry,:,:]=u_bry[:,:,0]
      ncbry['v_east'][tndx_bry,:,:]=v_bry[:,:,0]
      ncbry['ubar_east'][tndx_bry,:]=ubar_bry[:,0]
      ncbry['vbar_east'][tndx_bry,:]=vbar_bry[:,0]


    elif obctype=='w':

      ncbry['zeta_west'][tndx_bry,:]=zeta_bry[:,-1]
      ncbry['temp_west'][tndx_bry,:,:]=temp_bry[:,:,-1]
      ncbry['salt_west'][tndx_bry,:,:]=salt_bry[:,:,-1]
      ncbry['u_west'][tndx_bry,:,:]=u_bry[:,:,-1]
      ncbry['v_west'][tndx_bry,:,:]=v_bry[:,:,-1]
      ncbry['ubar_west'][tndx_bry,:]=ubar_bry[:,-1]
      ncbry['vbar_west'][tndx_bry,:]=vbar_bry[:,-1]

    return(ncbry)






