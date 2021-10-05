'''
CV 2014/05/21 : This file gathers all interpolation functions on a C-grid. 
'''
from numpy import zeros
from numpy import nan
import numpy as np

# --------------- from u-point to rho-point ---------------
def u2rho_2d(var_u):
        [Mp,L]                  = var_u.shape
        Lp                      = L+1
        Lm                      = L-1
        var_rho                 = zeros((Mp,Lp))
        var_rho[:,1:L]          = 0.5*(var_u[:,0:Lm] + var_u[:,1:L])
        var_rho[:,0]            = var_rho[:,1]
        var_rho[:,Lp-1]         = var_rho[:,L-1]
        return var_rho

def u2rho_3d(var_u):
        [N,Mp,L]                = var_u.shape
        Lp                      = L+1
        Lm                      = L-1
        var_rho                 = zeros((N,Mp,Lp))
        var_rho[:,:,1:L]        = 0.5*(var_u[:,:,0:Lm] + var_u[:,:,1:L])
        var_rho[:,:,0]          = var_rho[:,:,1]
        var_rho[:,:,Lp-1]       = var_rho[:,:,L-1]
        return var_rho

def u2rho_4d(var_u):
        [nt,N,Mp,L]             = var_u.shape
        Lp                      = L+1
        Lm                      = L-1
        var_rho                 = zeros((nt,N,Mp,Lp))
        var_rho[:,:,:,1:L]      = 0.5*(var_u[:,:,:,0:Lm] + var_u[:,:,:,1:L])
        var_rho[:,:,:,0]        = var_rho[:,:,:,1]
        var_rho[:,:,:,Lp-1]     = var_rho[:,:,:,L-1]
        return var_rho

def u2v_2d(var_u):
        [Mp,L]                  = var_u.shape
        M                              = Mp-1
        Mm                      = M-1
        Lp                      = L+1
        Lm                      = L-1
        var_v                          = zeros((M,Lp))
        var_v[0:M,1:L]          = 0.25*(var_u[:-1,:-1]+var_u[:-1,1:]+\
                                        var_u[1: ,:-1]+var_u[1: ,1:])
        var_v[:,0]              = var_v[:,1]
        var_v[:,L]              = var_v[:,Lm]
        return var_v

# --------------- from v-point to rho-point ---------------
def v2rho_2d(var_v):
        [M,Lp]                  = var_v.shape
        Mp                      = M+1
        Mm                      = M-1
        var_rho                 = zeros((Mp,Lp))
        var_rho[1:M,:]          = 0.5*(var_v[0:Mm,:] + var_v[1:M,:])
        var_rho[0,:]            = var_rho[1,:]
        var_rho[Mp-1,:]         = var_rho[M-1,:]
        return var_rho

def v2rho_3d(var_v):
        [N,M,Lp]                = var_v.shape
        Mp                      = M+1
        Mm                      = M-1
        var_rho                 = zeros((N,Mp,Lp))
        var_rho[:,1:M,:]        = 0.5*(var_v[:,0:Mm,:] + var_v[:,1:M,:])
        var_rho[:,0,:]          = var_rho[:,1,:]
        var_rho[:,Mp-1,:]       = var_rho[:,M-1,:]
        return var_rho

def v2rho_4d(var_v):
        [nt,N,M,Lp]             = var_v.shape
        Mp                      = M+1
        Mm                      = M-1
        var_rho                 = zeros((nt,N,Mp,Lp))
        var_rho[:,:,1:M,:]      = 0.5*(var_v[:,:,0:Mm,:] + var_v[:,:,1:M,:])
        var_rho[:,:,0,:]        = var_rho[:,:,1,:]
        var_rho[:,:,Mp-1,:]     = var_rho[:,:,M-1,:]
        return var_rho

def v2u_2d(var_v):
        [M,Lp]                  = var_v.shape
        Mp                      = M+1
        Mm                      = M-1
        L                       = Lp-1
        Lm                      = L-1
        var_u                          = zeros((Mp,L))
        var_u[1:M,0:L]          = 0.25*(var_v[:-1,:-1]+var_v[:-1,1:]+\
                                        var_v[1: ,:-1]+var_v[1: ,1:])
        var_u[0,:]              = var_u[1,:]
        var_u[M,:]              = var_u[Mm,:]
        return var_u

# --------------- from rho-point to u,v,psi-point ---------------

def rho2uvp(rfield):                                 # often used for a mask
        [Mp,Lp]         = rfield.shape
        M               = Mp-1
        L               = Lp-1
        vfield          = 0.5*(rfield[0:M,:] + rfield[1:Mp,:])
        ufield          = 0.5*(rfield[:,0:L] + rfield[:,1:Lp])
        pfield          = 0.5*(ufield[0:M,:] + ufield[1:Mp,:])
        return ufield,vfield,pfield

def rho2u_2d(var_rho):
        [Mp,Lp]         = var_rho.shape
        L               = Lp-1
        var_u           = 0.5*(var_rho[:,0:L]+var_rho[:,1:Lp])
        return var_u

def rho2u_3d(var_rho):
        [N,Mp,Lp]       = var_rho.shape
        L               = Lp-1
        var_u           = 0.5*(var_rho[:,:,0:L]+var_rho[:,:,1:Lp])
        return var_u

def rho2u_4d(var_rho):
        [nt,N,Mp,Lp]    = var_rho.shape
        L               = Lp-1
        var_u           = 0.5*(var_rho[:,:,:,0:L]+var_rho[:,:,:,1:Lp])
        return var_u

def rho2v_2d(var_rho):
        [Mp,Lp]         = var_rho.shape
        M               = Mp-1
        var_v           = 0.5*(var_rho[0:M,:]+var_rho[1:Mp,:])
        return var_v

def rho2v_3d(var_rho):
        [N,Mp,Lp]       = var_rho.shape
        M               = Mp-1
        var_v           = 0.5*(var_rho[:,0:M,:]+var_rho[:,1:Mp,:])
        return var_v

def rho2v_4d(var_rho):
        [nt,N,Mp,Lp]    = var_rho.shape
        M               = Mp-1
        var_v           = 0.5*(var_rho[:,:,0:M,:]+var_rho[:,:,1:Mp,:])
        return var_v


def rho2p_2d(var_rho):
        [Mp,Lp]         = var_rho.shape
        M               = Mp-1
        L               = Lp-1
        var_p           = 0.25*(var_rho[0:M,0:L]+var_rho[0:M,1:Lp]+
                                var_rho[1:Mp,0:L]+var_rho[1:Mp,1:Lp])
        return var_p

def rho2p_3d(var_rho):
        [N,Mp,Lp]         = var_rho.shape
        M               = Mp-1
        L               = Lp-1
        var_p           = 0.25*(var_rho[:,0:M,0:L]+var_rho[:,0:M,1:Lp]+
                                var_rho[:,1:Mp,0:L]+var_rho[:,1:Mp,1:Lp])
        return var_p

# --------------- from psi-point to rho-point ---------------
def psi2rho_2d(var_psi):                                # this is for a 2D variable only
        [M,L]           = var_psi.shape
        Mp              = M+1
        Lp              = L+1
        Mm              = M-1
        Lm              = L-1
        var_rho         = zeros((Mp,Lp))
        var_rho[1:M,1:L]=0.25*(var_psi[0:Mm,0:Lm]+var_psi[0:Mm,1:L]+var_psi[1:M,0:Lm]+var_psi[1:M,1:L])
        var_rho[0,:]    = var_rho[1,:]
        var_rho[Mp-1,:] = var_rho[M-1,:]
        var_rho[:,0]    = var_rho[:,1]
        var_rho[:,Lp-1] = var_rho[:,L-1]
        return var_rho

def psi2rho_3d(var_psi):                                # this is for a 3D variable only
        [N,M,L]         = var_psi.shape
        Mp              = M+1
        Lp              = L+1
        Mm              = M-1
        Lm              = L-1
        var_rho         = zeros((N,Mp,Lp))
        var_rho[:,1:M,1:L] = 0.25*(var_psi[:,0:Mm,0:Lm] + var_psi[:,0:Mm,1:L]\
                                  +var_psi[:,1:M,0:Lm]  + var_psi[:,1:M,1:L])
        var_rho[:,0,:]    = var_rho[:,1,:]
        var_rho[:,Mp-1,:] = var_rho[:,M-1,:]
        var_rho[:,:,0]    = var_rho[:,:,1]
        var_rho[:,:,Lp-1] = var_rho[:,:,L-1]
        return var_rho

# --------------- get u- v- and psi- masks from maks at rho point ---------------

def uvp_mask(rfield):
        [Mp,Lp]         = rfield.shape
        M               = Mp-1
        L               = Lp-1
        vfield          = rfield[0:M,:] * rfield[1:Mp,:]
        ufield          = rfield[:,0:L] * rfield[:,1:Lp]
        pfield          = ufield[0:M,:] * ufield[1:Mp,:]
        return ufield,vfield,pfield


# --------------- 9 points avg ---------------

def nine_pts_avg(var,mask):
        var = var * mask

        num=var[2:  :3, :-2:3]+var[2:  :3,1:-1:3]+var[2:  :3,2::3]+\
            var[1:-1:3, :-2:3]+var[1:-1:3,1:-1:3]+var[1:-1:3,2::3]+\
            var[ :-2:3, :-2:3]+var[ :-2:3,1:-1:3]+var[ :-2:3,2::3]

        den=mask[2:  :3, :-2:3]+mask[2:  :3,1:-1:3]+mask[2:  :3,2::3]+\
            mask[1:-1:3, :-2:3]+mask[1:-1:3,1:-1:3]+mask[1:-1:3,2::3]+\
            mask[ :-2:3, :-2:3]+mask[ :-2:3,1:-1:3]+mask[ :-2:3,2::3]

        den[den==0]=nan
        var=num/den
        
        return var


# --------------- vorticity ---------------

def vort(ubar,vbar,pm,pn):

        [Mp,Lp] = pm.shape
        L=Lp-1
        M=Mp-1
        Lm=L-1
        Mm=M-1
        xi=zeros((M,L))
        mn_p=zeros((M,L))
        uom=zeros((M,Lp))
        von=zeros((Mp,L))
        uom=2*ubar/(pm[:,0:L]+pm[:,1:Lp])
        von=2*vbar/(pn[0:M,:]+pn[1:Mp,:])
        mn=pm*pn
        mn_p=(mn[0:M ,0:L ]+mn[0:M ,1:Lp]+\
              mn[1:Mp,1:Lp]+mn[1:Mp,0:L ])/4
        xi=mn_p*(von[:,1:Lp]-von[:,0:L]-uom[1:Mp,:]+uom[0:M,:])

        return xi


# --------------- vorticity ---------------

def vort3d(u,v,pm,pn):

        [N,Mp,L] = u.shape
        Lp=L+1
        M=Mp-1
        
        om=2/(pm[:,0:L]+pm[:,1:Lp])
        on=2/(pn[0:M,:]+pn[1:Mp,:])
        uom=u*np.tile(om,(N,1,1))
        von=v*np.tile(on,(N,1,1))
        mn=pm*pn
        mn_p=(mn[0:M ,0:L ]+mn[0:M ,1:Lp]+\
              mn[1:Mp,1:Lp]+mn[1:Mp,0:L ])/4
        xi=np.tile(mn_p,(N,1,1))*(von[:,:,1:Lp]-von[:,:,0:L]-uom[:,1:Mp,:]+uom[:,0:M,:])

        return xi

#
# --------------- corrected and stable arange function ---------------
#
# Function arange like matlab
#
def matarange(start,step,stop):
  out=np.arange(start,stop+step,step+1.123456789123456789e-20)
  return out


#
# --------------- ^2 more efficient ---------------
#
# Function square
#
def square(varin):
  varout=varin*varin
  return varout

#
# --------------- put nan on masked values ---------------
#
# Function nanmask
#
def nanmask(varin,maskin):
  
  varin[np.where(maskin==0.)]=np.nan
  varout=varin+0.
  return varout

# --------------- vertical grid ---------------
#
#
def zlevs(zeta,h,sc_r,sc_w,Cs_r,Cs_w,hc,Vtransform):
#
# Get CROCO vertical grid
#
#
  N=np.size(sc_r)
  hdims = np.shape(h)
  if hdims==2:
    (Mp,Lp)=hdims
  elif hdims==1:
    Mp=hdims
    Lp=1
  else:
    Mp=1
    Lp=1
    
  hinv=1./h
#
# get z_r and z_w: depths of vertical levels at rho and w points
#
  z_w=np.zeros((N+1,Mp,Lp,), dtype=float, order='C')
  z_w[0,:,:]=-h
  z_r=np.zeros((N,Mp,Lp,), dtype=float, order='C')
  if Vtransform==2.0:
    print('NEW_S_COORD')
    for k in np.arange(0,N,1):

      cff_w =hc*sc_w[k+1]
      cff_r =hc*sc_r[k]
      cff1_w=Cs_w[k+1]
      cff1_r=Cs_r[k]

      z_w0=cff_w+cff1_w*h
      z_r0=cff_r+cff1_r*h  
         
      z_w[k+1,:,:]=z_w0*h*hinv+zeta*(1.+z_w0*hinv) 
      z_r[k,:,:]=z_r0*h*hinv+zeta*(1.+z_r0*hinv)

  else:
    print('OLD_S_COORD')
    for k in np.arange(0,N,1):

      cff_w =hc*(sc_w[k+1]-Cs_w[k+1])
      cff_r =hc*(sc_r[k]-Cs_r[k])
      cff1_w=Cs_w[k+1]
      cff1_r=Cs_r[k]

      z_w0=cff_w+cff1_w*h
      z_r0=cff_r+cff1_r*h  
         
      z_w[k+1,:,:]=z_w0+zeta*(1.+z_w0*hinv)      
      z_r[k,:,:]=z_r0+zeta*(1.+z_r0*hinv)
#
# get Hz height of cells
#
  Hz=z_w[1:,:,:]-z_w[:-1,:,:]

  return z_r, z_w, Hz


 
