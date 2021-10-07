#!/usr/bin/python
import numpy as np
import sys
#sys.path.insert(0,'/XXX/') 
import my_plots                 as my
from interp_Cgrid import *


#import python_scripts_pierrick as pier


#
#
#
# --------------- Get_streamfunc: Compute a stream function ---------------
#
# inverse Laplacian(psi)=xi in presence of coastal mask
#
# can work with 1 island
#
# 
#
def get_streamfunc(UU,VV,pm,pn,rmask):
#
# 1 enforce mass conservation at lateral boundaries
#
  [UU,VV]=get_obcvolcons(UU,VV,pm,pn,rmask)
#
# 2 get psi0 (laplacian(psi0)=xi without the island) and psi1 (laplacian(psi1)=1 for the island)
# 
  [psi0,psi1,island]=get_psi0(UU,VV,pm,pn,rmask)
#
# 3 get the circulation around the island and merge psi0 and psi1
# 
  if np.sum(island)==0.:
    A=0.
  else:
    A=get_a(UU,VV,psi0,psi1,island,pm,pn)

  psi=psi0+A*psi1

  return psi

#
#
#
# --------------- Get_obcvolcons: enforce mass conservation at lateral boundaries ---------------
#
#
#
def get_obcvolcons(UU,VV,pm,pn,rmask):
######################################################################
#
# Enforce integral flux conservation around the domain
#
######################################################################

  umask=rmask[:,1:]*rmask[:,:-1]
  vmask=rmask[1:,:]*rmask[:-1,:]
#
  dy_u=2.*umask/(pn[:,1:]+pn[:,:-1])
  dx_v=2.*vmask/(pm[1:,:]+pm[:-1,:])
#  
  udy=UU*dy_u
  vdx=VV*dx_v
#
  Flux =sum(vdx[0 ,1:-1])-sum(udy[1:-1,-1])- \
        sum(vdx[-1,1:-1])+sum(udy[1:-1, 0])
  Cross=sum(dx_v[0 ,1:-1])+sum(dy_u[1:-1,-1])+ \
        sum(dx_v[-1,1:-1])+sum(dy_u[1:-1, 0])
#
  vcorr=Flux/Cross
  print('Flux correction : '+str(vcorr))
#
  VV[0,:]=VV[0,:]-vcorr
  UU[:,-1]=UU[:,-1]+vcorr
  VV[-1,:]=VV[-1,:]+vcorr
  UU[:,0]=UU[:,0]-vcorr
#
  UU=UU*umask
  VV=VV*vmask
#
  return UU,VV


#
#
#
# --------------- Get PSI0: first step for computing a stream function ---------------
#
#
def get_psi0(UU,VV,pm,pn,rmask):
######################################################################
#
#  Compute a stream function from a ROMS vector
#  field (velocity or transport)
#
#  1 - get boundary conditions for psi
#  2 - diffuse these boundary conditions on the land points
#  3 - get Xi the vorticity
#  4 - inverse laplacian(psi)=Xi
#
#
######################################################################
  epsil=0.1
  (M,L)=np.shape(pm) 
  pmask=rmask[1: ,1: ]*rmask[1: ,:-1]* \
        rmask[:-1,1: ]*rmask[:-1,:-1]
#
# Get lateral boundary conditions for psi
#
# psi: initial and  dirichlet boundary conditions, 2D matrix
# bcpsi: dirichlet boundary conditions for the mask, 2D matrix 
#
  [psi,bcpsi]=get_bcpsi(UU,VV,pm,pn,pmask)
  island_bool=(pmask==0.) & (bcpsi==0.)
#  island_bool=(pmask==10.) 
  island=island_bool.astype(float)

   
  if np.sum(island)==0.:
    print 'No island'
    bc0=bcpsi+0.
    bc0[np.where(bcpsi==0.)]=np.nan
  else:
    print 'Island'
    bc0=bcpsi+0.
    bc0[np.where((bcpsi==0.) & (pmask!=0.))]=np.nan
    bc1=0.*bc0
    bc1[np.where(island==1.)]=1.

#
# get the vorticity
#
  xi=vort(UU,VV,pm,pn);
#
# get psi0
#
  psi0=inv_elliptic(psi,bc0,xi,pm,pn,pmask,epsil)
#
# get psi1
#
  if np.sum(island)==0.:
    psi1=0*psi0
  else:
    psi1=inv_elliptic(0.*psi,bc1,0.*xi,pm,pn,pmask,epsil);

#
  return psi0,psi1,island

#
#
#
# --------------- GET_BCPSI: get the lateral Dirichlet boundary conditions in presence of land mask  ---------------
# ---------------                (propagate the lateral psi values in the mask)
#
#
#
def get_bcpsi(UU,VV,pm,pn,pmask):
#
# Get lateral boundary conditions for psi
#
  (M,L)=np.shape(pm) 
  psi=0*pmask
  uon=2.*UU/(pn[:,:-1]+pn[:,1:])
  vom=2.*VV/(pm[:-1,:]+pm[1:,:])
#
# Quick fix for a bug: stepsor cherche bcpsi~=0 pour ne
# pas appliquer de condition aux limites sur les iles.
# or si psi(1,1)=0 et si c'est masque, alors c'est considere
# comme une ile. 
# le fixer a 1e7 devrait prevenir le probleme, mais ce n'est
# pas fiable a 100#.
#
  psi[0,0]=1e7
  epsil=1e-5
  nmax=500
#
  for j in range(1,M-1):
    psi[j,0]=psi[j-1,0]-uon[j,0]

  for i in range(1,L-1):
    psi[-1,i]=psi[-1,i-1]+vom[-1,i]
    
  psiend=psi[-1,-1]

  for i in range(1,L-1):
    psi[0,i]=psi[0,i-1]+vom[0,i]

  for j in range(1,M-1):
    psi[j,-1]=psi[j-1,-1]-uon[j,-1]
  
  if psiend!=0:
    deltapsi=abs((psi[-1,-1]-psiend)/psiend)
  else:
    deltapsi=abs(psi[-1,-1])

  if deltapsi>1e-10:
    print('Warning : no mass conservation : deltapsi='+str(deltapsi))

#
# Diffuse the psi boundaries condition on land to get 
# a constant psi on the mask. 
# WARNING !!! This does not work for islands
#
  land=1.-pmask
  bcpsi=land*psi
  mean1=np.nanmean(bcpsi)
  delta_mean=epsil+1.
  n=1
  while (delta_mean>epsil)&(n<nmax):
  
    lmask=0.*bcpsi + 0.
    lmask[np.where(np.abs(bcpsi)>0.)]=1.
    
    denom=(land[:-2,1:-1]*lmask[:-2,1:-1]+ \
           land[2:,1:-1] *lmask[2:,1:-1]+   \
           land[1:-1,:-2]*lmask[1:-1,:-2]+ \
           land[1:-1,2:] *lmask[1:-1,2:])

    denom[np.where(denom==0.)]=1.

    rhs=(land[:-2,1:-1]*bcpsi[:-2,1:-1]+ \
         land[2:,1:-1] *bcpsi[2:,1:-1]+  \
         land[1:-1,:-2]*bcpsi[1:-1,:-2]+ \
         land[1:-1,2:] *bcpsi[1:-1,2:])/denom
 
    bcpsi[1:-1,1:-1]=land[1:-1,1:-1]*rhs
     
    mean2=np.nanmean(bcpsi)
    delta_mean=abs((mean1-mean2)/mean2)
    mean1=mean2+0.
    n=n+1
    if n>=nmax:
      print 'Mask: no convergence'
      
  print 'Mask: '+str(n)+' iterations'
  
  return psi,bcpsi

#
#
#
# --------------- inv_elliptic: inverse laplacian psi = xi using surrelaxation (SOR) ---------------
#
#
#
def inv_elliptic(psi,bc,xi,pm,pn,pmask,epsil):
#
# Inversion of the elliptic equation (SOR)
#

#
# Prepare for psi integration div(psi)=xi
#
  (M,L)=np.shape(pm) 
  mn=pm*pn
  mon=pm/pn
  nom=pn/pm
  a1=0.25*(mn[:-1,:-1]+mn[:-1,1:]+\
           mn[1:,:-1] +mn[1:,1:])
  a2=0.5*(mon[:-1,1:]+mon[1:,1:])
  a3=0.5*(mon[:-1,:-1]+mon[1:,:-1])
  a4=0.5*(nom[1:,:-1]+nom[1:,1:])
  a5=0.5*(nom[:-1,:-1]+nom[:-1,1:])
  b1=-(a2+a3+a4+a5)
  b2=-1./a1
  J2=square(np.max((np.cos(np.pi/L)+square(nom)*np.cos(np.pi/M))/(1+square(nom))))
#
# Initial value of surrelaxation
#
  W=1
#
# Get a matrix of parity (i.e. i+j = odd or even)
#   
  myi=matarange(0,1,L-2)
  myj=matarange(0,1,M-2)
  [MYI,MYJ]=np.meshgrid(myi,myj)
  ijeven=np.mod(MYJ+MYI,2.)
#
# First step (odd and even)
#
  rhs=0.*psi
  parity=1. #(i+j) even
  (psi,W,rhs)=step_sor(rhs,a2,a3,a4,a5,b1,b2,psi,parity,ijeven,W,J2,bc,xi)
  parity=0. #(i+j) odd
  (psi,W,rhs)=step_sor(rhs,a2,a3,a4,a5,b1,b2,psi,parity,ijeven,W,J2,bc,xi)

#
# Integration until convergence
#
  meanrhs=np.mean(np.abs(rhs[np.where(pmask==1)]))
  Norm0=meanrhs+0.
  n=1
#  nmax=M*L
  nmax=1000
  
  while (meanrhs>epsil*Norm0):
    n=n+1
    parity=1. #(i+j) even
    (psi,W,rhs)=step_sor(rhs,a2,a3,a4,a5,b1,b2,psi,parity,ijeven,W,J2,bc,xi)
    parity=0. #(i+j) odd
    (psi,W,rhs)=step_sor(rhs,a2,a3,a4,a5,b1,b2,psi,parity,ijeven,W,J2,bc,xi)
    meanrhs=np.mean(np.abs(rhs[np.where(pmask==1)]))
#    print n,meanrhs
    if (n > nmax):
      meanrhs=0.
      print 'PSI: No convergence'

  print 'PSI: '+str(n)+' iterations '
# 
  return psi

#
#
#
# --------------- STEP_SOR: 1 step of the successive over surrelaxation (on a black/white grid) ---------------
#
#
#
def step_sor(rhs,a2,a3,a4,a5,b1,b2,psi,parity,ijeven,W,J2,bc,xi):
######################################################################
#
#  1 Step of the elliptic solver (SOR) used in get_psi.m
#
#
######################################################################
#
# Get the right hand side terms of the equations
#
  rhs[1:-1,1:-1]= \
     a2[1:-1,1:-1]*psi[1:-1,2:]+ \
     a3[1:-1,1:-1]*psi[1:-1,0:-2]+ \
     a4[1:-1,1:-1]*psi[2:,1:-1]+ \
     a5[1:-1,1:-1]*psi[0:-2,1:-1]+ \
     b1[1:-1,1:-1]*psi[1:-1,1:-1]+ \
     b2[1:-1,1:-1]*xi[1:-1,1:-1];
#
# Step PSI
#
  psi[np.where(ijeven==parity)]=psi[np.where(ijeven==parity)]- \
                    W*rhs[np.where(ijeven==parity)]/ \
                    b1[np.where(ijeven==parity)]
#
# Advance the surrelaxation parameter
#
  W=1/(1-J2*W/4)
#
# Apply the mask (not the islands bcpsi~=0)
#
  psi[np.where(np.isfinite(bc))]=bc[np.where(np.isfinite(bc))]
#
#
#
  return psi,W,rhs


#
#
#
# --------------- GET_A: get the circulation around the island ---------------
#
#
#
def get_a(UU,VV,psi0,psi1,island,pm,pn):
#
# calcul de A : psi=psi0+A*psi1
#
  pm_u=rho2u_2d(pm)
  pm_v=rho2v_2d(pm)
  pn_u=rho2u_2d(pn)
  pn_v=rho2v_2d(pn)
#
# Get the circulation around the island due to psi0
#
  psi_r=psi2rho_2d(psi0)
  u_v=-pn_v*(psi_r[1:,:]-psi_r[:-1,:])
  v_u= pm_u*(psi_r[:,1:]-psi_r[:,:-1])
#
  circ=(v_u[1:-1,:-1]/pn_u[1:-1,:-1]) +  \
       (u_v[1:,1:-1]/pm_v[1:,1:-1]) -  \
       (v_u[1:-1,1:]/pn_u[1:-1,1:]) -  \
       (u_v[:-1,1:-1]/pm_v[:-1,1:-1])
  A0=np.sum(circ[np.where(island==1.)])     
#
# Get the circulation around the island due to psi1
#
  psi_r=psi2rho_2d(psi1)
  u_v=-pn_v*(psi_r[1:,:]-psi_r[:-1,:])
  v_u= pm_u*(psi_r[:,1:]-psi_r[:,:-1])
#
  circ=(v_u[1:-1,:-1]/pn_u[1:-1,:-1]) +  \
       (u_v[1:,1:-1]/pm_v[1:,1:-1]) -  \
       (v_u[1:-1,1:]/pn_u[1:-1,1:]) -  \
       (u_v[:-1,1:-1]/pm_v[:-1,1:-1])
  A1=np.sum(circ[np.where(island==1.)]) 
#
# Get the total circulation around the island
#
  v_u=v2u_2d(VV)
  u_v=u2v_2d(UU)
  circ=(v_u[1:-1,:-1]/pn_u[1:-1,:-1]) +  \
       (u_v[1:,1:-1]/pm_v[1:,1:-1]) -  \
       (v_u[1:-1,1:]/pn_u[1:-1,1:]) -  \
       (u_v[:-1,1:-1]/pm_v[:-1,1:-1])
  Atotal=np.sum(circ[np.where(island==1.)])
#
# Get A
#
  A=(Atotal-A0)/A1
#
  return A

