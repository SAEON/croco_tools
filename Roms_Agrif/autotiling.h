         integer nbsampling, lastiic
         common/dyndistribution/nbsampling, lastiic


      integer, parameter :: MAX_NSUB_X=Lm/6
      integer, parameter :: MAX_NSUB_E=Mm/6
      
      integer, parameter :: nbvalid = 3
      integer, parameter :: nbdistrib = 4
      
      real cpu_domain(0:2)
      real times(MAX_NSUB_X,MAX_NSUB_E,nbdistrib*nbvalid)
      integer nbinst(MAX_NSUB_X,MAX_NSUB_E)
      integer nsub_emin,nsub_emax, nsub_xmin,nsub_xmax
      integer nsub_decalx, nsub_decale
      integer curdistrib
      common/dyndistribution2/times,cpu_domain,
     &       nbinst,nsub_xmin, nsub_xmax,
     &  nsub_emin,nsub_emax,nsub_decalx,nsub_decale,
     &  curdistrib

      integer, parameter :: nbx=4
      integer, parameter :: nbe=4

