import numpy                as np
import croco_scripts as croc
import interp_Cgrid  as cgrd

def rho_eos_v1(zeta,z_r,Tt,Ts,rho0,DUKO_2001,SPLIT_EOS):
#
#======================================================================
# Compute density anomaly via Equation Of State (EOS) for seawater.
# Following Jackett and McDougall, 1995, physical EOS is assumed to
# have form 
#
#                           rho0 + rho1(T,S)
#           rho(T,S,z) = ------------------------                 (1)
#                         1 - 0.1*|z|/K(T,S,|z|)
#
# where rho1(T,S) is sea-water density perturbation [kg/m^3] at
# standard pressure of 1 Atm (sea surface); |z| is absolute depth,
# i.e. distance from free-surface to the point at which density is
# computed, and
#
#     K(T,S,|z|) = K00 + K01(T,S) + K1(T,S)*|z| + K2(T,S)*|z|^2.  (2)
#
# To reduce errors of pressure-gradient scheme associated with
# nonlinearity of compressibility effects, as well as to reduce
# roundoff errors, the dominant part of density profile,
#
#                           rho0
#                     ----------------                            (3)
#                      1 - 0.1|z|/K00
#
# is removed from from (1). [Since (3) is purely a function of z,
# it does not contribute to pressure gradient.]  This results in
#
#                   rho1 - rho0*[K01+K1*|z|+K2*|z|^2]/[K00-0.1|z|] 
#    rho1 + 0.1|z| -----------------------------------------------
#                        K00 + K01 + (K1-0.1)*|z| + K2*|z|^2
#                                                                 (4)
# which is suitable for pressure-gradient calculation. 
#
# Optionally, if CPP-switch SPLIT_EOS is defined, term proportional 
# to |z| is linearized using smallness 0.1|z|/[K00 + K01] << 1 and
# the resultant EOS has form
# 
#              rho(T,S.z) = rho1(T,S) + qp1(T,S)*|z|               (5)
#
# where 
#                            rho1 - rho0*K01(T,S)/K00
#              qp1(T,S)= 0.1 --------------------------            (6)
#                                K00 + K01(T,S)
#
# is stored in a special array.
#
# This splitting allows representation of spatial derivatives (and
# also differences) of density as sum of adiabatic derivatives and
# compressible part according to 
#
#         d rho     d rho1           d qp1             d |z|
#        ------- = -------- + |z| * -------  +  qp1 * -------      (7)
#         d x,s      d x,s           d x,s            d x,s
#
#                  |<----- adiabatic ----->|   |<- compress ->|
#
# so that constraining of adiabatic derivative for monotonicity is
# equivalent to enforcement of physically stable stratification.
# [This separation and constraining algorithm is subsequently used
# in computation of pressure gradient within prsgrd32-family
# schemes.] 
#
# If so prescribed compute the Brunt-Vaisala frequency [1/s^2] at
# horizontal RHO-points and vertical W-points,
#
#                          g    d rho  |
#             bvf^2 = - ------ ------- |                          (8)
#                        rho0    d z   | adiabatic
#
# where density anomaly difference is computed by adiabatically
# rising/lowering the water parcel from RHO point above/below to
# the W-point depth at "z_w".
#
# References:
# ----------
#  Shchepetkin, A.F., McWilliams, J.C., 2003: A method for computing 
#  horizontal pressure-gradient force in an oceanic model with a 
#  non-aligned vertical coordinate. J. Geophys. Res. 108 (C3), 3090.
#
#====================================================================== 
#

      qp2=0.0000172

      (N,M,L)=np.shape(Tt)
      dpth=np.tile(zeta,(N,1,1))-z_r     

      r00=999.842594
      r01=6.793952E-2
      r02=-9.095290E-3
      r03=1.001685E-4
      r04=-1.120083E-6
      r05=6.536332E-9
      r10=0.824493
      r11=-4.08990E-3
      r12=7.64380E-5
      r13=-8.24670E-7
      r14=5.38750E-9
      rS0=-5.72466E-3
      rS1=1.02270E-4
      rS2=-1.65460E-6
      r20=4.8314E-4
      K00=19092.56
      K01=209.8925
      K02=-3.041638
      K03=-1.852732e-3
      K04=-1.361629e-5
      K10=104.4077
      K11=-6.500517
      K12=0.1553190
      K13=2.326469e-4
      KS0=-5.587545
      KS1=+0.7390729
      KS2=-1.909078e-2
      B00=0.4721788
      B01=0.01028859
      B02=-2.512549e-4
      B03=-5.939910e-7
      B10=-0.01571896
      B11=-2.598241e-4
      B12=7.267926e-6
      BS1=2.042967e-3
      E00=+1.045941e-5
      E01=-5.782165e-10
      E02=+1.296821e-7
      E10=-2.595994e-7
      E11=-1.248266e-9
      E12=-3.508914e-9
         
      if DUKO_2001:
        Tt0=3.8e0
        Ts0=34.5e0
        sqrtTs0=np.sqrt(Ts0)
        K0_Duk= Tt0*( K01+Tt0*( K02+Tt0*( K03+Tt0*K04 )))  \
               +Ts0*( K10+Tt0*( K11+Tt0*( K12+Tt0*K13 ))   \
                    +sqrtTs0*( KS0+Tt0*( KS1+Tt0*KS2 )))

#
#  compute rho as a perturbation to rho0 (at the surface)
#

      dr00=r00-rho0

      sqrtTs=np.sqrt(Ts)

      rho1=( dr00 +Tt*( r01+Tt*( r02+Tt*( r03+Tt*(          \
                                            r04+Tt*r05 )))) \
                         +Ts*( r10+Tt*( r11+Tt*( r12+Tt*(   \
                                            r13+Tt*r14 )))  \
                              +sqrtTs*(rS0+Tt*(             \
                                    rS1+Tt*rS2 ))+Ts*r20 ))
     
      K0= Tt*( K01+Tt*( K02+Tt*( K03+Tt*K04 )))             \
         +Ts*( K10+Tt*( K11+Tt*( K12+Tt*K13 ))              \
              +sqrtTs*( KS0+Tt*( KS1+Tt*KS2 ))) 

      if  SPLIT_EOS:
        if DUKO_2001:
            qp1=0.1*(rho0+rho1)*(K0_Duk-K0)                 \
                               /((K00+K0)*(K00+K0_Duk)) 						
        else:
            qp1=0.1*(K00*rho1-rho0*K0)/(K00*(K00+K0))

        rho=rho1+qp1*dpth*(1.-qp2*dpth)

      else:

        K1=B00+Tt*(B01+Tt*(B02+Tt*B03)) +Ts*( B10+Tt*( B11  \
                                    +Tt*B12 )+sqrtTs*BS1 )

        K2=E00+Tt*(E01+Tt*E02) +Ts*(E10+Tt*(E11+Tt*E12))


        cff=K00-0.1*dpth
        cff1=K0+dpth*(K1+K2*dpth)
        rho=( rho1*cff*(K00+cff1)                           \
                           -0.1*dpth*rho0*cff1              \
                            )/(cff*(cff+cff1))

      return rho, rho1, qp1, qp2




def get_buyancy_gradients(zeta,z_r,Tt,Ts,rho0,pm,pn,DUKO_2001,SPLIT_EOS):
#
#====================================================================== 
#
# get_buyancy_gradients
#
#====================================================================== 
#
#
# Adapted from Jonathan's R_tools_fort.F
#

      (N,M,L)=np.shape(Tt)

      g=9.81 
      cff=g/rho0
      
      (rho, rho1, qp1, qp2)=croc.rho_eos_v1(zeta,z_r,Tt,Ts,rho0,DUKO_2001,SPLIT_EOS)

      if  SPLIT_EOS:


#---------------------------------------------------------------------------------------
#
#  compute buyancy gradients -  1 db/dz 
#


        dpth=np.tile(zeta,(N-1,1,1))-0.5*(z_r[1:,:,:]+z_r[:-1,:,:]) 

        cff2=( rho1[1:,:,:]-rho1[:-1,:,:]                 \
             +(qp1[1:,:,:]-qp1[:-1,:,:])                  \
               *dpth*(1.-2.*qp2*dpth)                       \
                     )

        dbdz=-cff*cff2 / (z_r[1:,:,:]-z_r[:-1,:,:])

#---------------------------------------------------------------------------------------
#
#  compute buyancy gradients -  2 db/dx 
#

        dpth=cgrd.rho2u_3d(np.tile(zeta,(N,1,1))-z_r)

        dbdx=-cff*( rho1[:,:,1:]-rho1[:,:,:-1]            \
                      +(qp1[:,:,1:]-qp1[:,:,:-1])         \
                           *dpth*(1.-qp2*dpth) )            \
                 *cgrd.rho2u_3d(np.tile(pm,(N,1,1)))

#---------------------------------------------------------------------------------------
#
#  compute buyancy gradients -  3 db/dy 
#

        dpth=cgrd.rho2v_3d(np.tile(zeta,(N,1,1))-z_r)
	
	dbdy=-cff*( rho1[:,1:,:]-rho1[:,:-1,:]            \
                      +(qp1[:,1:,:]-qp1[:,:-1,:])         \
                           *dpth*(1.-qp2*dpth) )            \
                 *cgrd.rho2v_3d(np.tile(pn,(N,1,1)))

#---------------------------------------------------------------------------------------
      else:

         print 'no implementation in non split eos'

      return dbdx, dbdy, dbdz

def PV(dbdx,dbdy,dbdz,u,v,z_r,f,pm,pn):
#
#====================================================================== 
#
# Compute Ertel PV using buyancy 
#
# Adapted from Jonathan's R_tools.py
#
# PV is returned on psi-w grid
#
#====================================================================== 
#

    (N,M,L)=np.shape(z_r)

    dz_r = z_r[1:,:,:]- z_r[:-1,:,:]
    dz_r[dz_r==0] = np.nan
    dz_p=cgrd.rho2p_3d(dz_r)
#
    del dz_r 
#---------------------------------------------------------------------------------------
#
#  Ertel potential vorticity, term 1: [f + (dv/dx - du/dy)]*db/dz
#
    dbdz = cgrd.rho2p_3d(dbdz)
#
# vrt on psi-rho grid
#
    vrt = cgrd.vort3d(u,v,pm,pn)
#
# PV1 on psi-w grid
#    
    pv =  (np.tile(cgrd.rho2p_2d(f),(N-1,1,1)) + 0.5*(vrt[1:,:,:] + vrt[:-1,:,:])) * dbdz
#
    del vrt, dbdz, f
#---------------------------------------------------------------------------------------
#
#  Ertel potential vorticity, term 2: (dv/dz)*(db/dx)
#
# dvdz on psi-w grid
#
    dvdz = cgrd.rho2u_3d(v[1:,:,:]-v[:-1,:,:])/dz_p
#
# dbdx on psi-rho grid
#
    dbdx = cgrd.rho2v_3d(dbdx)
#
# PV2 on psi-w grid
#
    pv = pv -1*dvdz*0.5*(dbdx[1:,:,:] + dbdx[:-1,:,:])
#
    del dbdx,dvdz
#---------------------------------------------------------------------------------------
#
#  Ertel potential vorticity, term 3: (du/dz)*(db/dy)
#
# dudz on psi-w grid
#
    dudz = cgrd.rho2v_3d(u[1:,:,:,]-u[:-1,:,:])/dz_p
#
# dbdy on psi-rho grid
#
    dbdy = cgrd.rho2u_3d(dbdy)
#
# PV3 on psi-w grid
#
    pv = pv + dudz*0.5*(dbdy[1:,:,:] + dbdy[:-1,:,:])
#
    del dbdy,dudz
 
    return pv




