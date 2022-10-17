import numpy as np

def square2quadrilateral( x0, y0, x1, y1, x2, y2, x3, y3, *args):

# init A matrix     
    a = np.zeros([8,len(x0)]) 

    delx3 = x0-x1+x2-x3
    dely3 = y0-y1+y2-y3

####
    affinemap = np.argwhere( (delx3==0)&(dely3==0))
    if len(affinemap)!=0:
        xx0 = x0[affinemap]
        xx1 = x1[affinemap]
        xx2 = x2[affinemap]
        yy0 = y0[affinemap]
        yy1 = y1[affinemap]
        yy2 = y2[affinemap]

        a[0, affinemap] = xx1-xx0
        a[1, affinemap] = xx2-xx1
        a[2, affinemap] = xx0
        a[3, affinemap] = yy1-yy0
        a[4, affinemap] = yy2-yy1
        a[5, affinemap] = yy0
        a[6, affinemap] = 0
        a[7, affinemap] = 0
####
    projectivemap = np.argwhere((delx3!=0)or(dely3!=0))
    if len(projectivemap)!=0:
        xx0 = x0[projectivemap]
        xx1 = x1[projectivemap]
        xx2 = x2[projectivemap]
        xx3 = x3[projectivemap]
        yy0 = y0[projectivemap]
        yy1 = y1[projectivemap]
        yy2 = y2[projectivemap]
        yy3 = y3[projectivemap]

        delx1 = xx1-xx2
        dely1 = yy1-yy2
        delx2 = xx3-xx2
        dely2 = yy3-yy2
        delx3 = delx3[projectivemap]
        dely3 = dely3[projectivemap]

        div = delx1*dely2-dely1*delx2
        zero = np.argwhere(div==0)

        if len(zero)!=0:
           exit()
        a13 = (delx3*dely2-dely3*delx2)/div
        a23 = (delx1*dely3-dely1*delx3)/div

        a[0, projectivemap] = xx1-xx0+a13*xx1
        a[1, projectivemap] = xx3-xx0+a23*xx3
        a[2, projectivemap] = xx0
        a[3, projectivemap] = yy1-yy0+a13*yy1
        a[4, projectivemap] = yy3-yy0+a23*yy3
        a[5, projectivemap] = yy0
        a[6, projectivemap] = a13
        a[7, projectivemap] = a23

    if len(args)==0: return a

    if len(args)!=0:
        if (len(args)!=2): print('Error in number of arg put'); exit()
        
        xin=args[0]
        yin=args[1]
        if len(xin)==1:
            xin = xin*np.ones(len(x0))
            yin = yin*np.ones(len(x0))
        res = np.zeros([2,len(xin)]) 
        if len(x0)==1:
            div = a[6]*xin[:] + a[7]*yin[:] + 1
            zero = np.argwhere(div==0)
            if len(zero)==0: exit()
            res[0, :] = (a[0]*xin[:] + a[1]*yin[:] + a[2])/div
            res[1, :] = (a[3]*xin[:] + a[4]*yin[:] + a[5])/div
        else:
            div = a[6,:]*xin +a[7,:]*yin + 1
            zero = np.argwhere(div==0)
            if len(zero)!=0: exit()
            res[0, :] = (a[0, :]*xin[:] + a[1, :]*yin[:] + a[2, :])/div
            res[1, :] = (a[3, :]*xin[:] + a[4, :]*yin[:] + a[5, :])/div
        return res


def quadrilateral2square(x0,y0,x1,y1,x2,y2,x3,y3,xin,yin):

    a = square2quadrilateral(x0,y0,x1,y1,x2,y2,x3,y3)

    adj = np.zeros([9, len(x0)])

    adj[0,:] = a[4,:]       -a[7,:]*a[5,:]
    adj[1,:] = a[7,:]*a[2,:]-a[1,:]
    adj[2,:] = a[1,:]*a[5,:]-a[4,:]*a[2,:]
    adj[3,:] = a[6,:]*a[5,:]-a[3,:]
    adj[4,:] = a[0,:]       -a[6,:]*a[2,:]
    adj[5,:] = a[3,:]*a[2,:]-a[0,:]*a[5,:]
    adj[6,:] = a[3,:]*a[7,:]-a[6,:]*a[4,:]
    adj[7,:] = a[6,:]*a[1,:]-a[0,:]*a[7,:]
    adj[8,:] = a[0,:]*a[4,:]-a[3,:]*a[1,:]
 
    if len(xin)==1:
        xin = xin*np.ones(len(x0))
        yin = yin*np.ones(len(x0))
#
# compute xprime, yprime and wprime
#
    if len(x0)==1:
        wpr = 1./(adj[6]*xin + adj[7]*yin + adj[8])
    else:
        wpr = 1./(adj[6,:]*xin + adj[7,:]*yin + adj[8,:])
  
    xpr = xin*wpr
    ypr = yin*wpr
#
    res = np.zeros([2,len(xin)])
#
    if len(x0)==1:
        res[0,:] = xpr*adj[0] + ypr*adj[1] +wpr*adj[2]
        res[1,:] = xpr*adj[3] + ypr*adj[4] +wpr*adj[5]
    else:
        res[0,:] = xpr*adj[0,:] + ypr*adj[1,:] +wpr*adj[2,:]
        res[1,:] = xpr*adj[3,:] + ypr*adj[4,:] +wpr*adj[5,:]
#
    return res



