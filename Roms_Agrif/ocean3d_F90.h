#ifdef SOLVE3D
      real u(GLOBAL_2D_ARRAY,N,3)
      real v(GLOBAL_2D_ARRAY,N,3)
      real t(GLOBAL_2D_ARRAY,N,3,NT)
      common /ocean_u/u /ocean_v/v /ocean_t/t

      real wz(GLOBAL_2D_ARRAY,0:N,3)
      common /ocean_wz/ wz

      real Hz(GLOBAL_2D_ARRAY,N)
      real Hz_bak(GLOBAL_2D_ARRAY,N)
      real z_r(GLOBAL_2D_ARRAY,N)
      real z_w(GLOBAL_2D_ARRAY,0:N)
      real Huon(GLOBAL_2D_ARRAY,N)
      real Hvom(GLOBAL_2D_ARRAY,N)
      real W(GLOBAL_2D_ARRAY,0:N)
      common /grid_Hz/Hz    /grid_zr/z_r  /grid_W/W         &
        /grid_Hz_bak/Hz_bak /grid_zw/z_w  /grid_Huon/Huon   &
                                          /grid_Hvom/Hvom

      real Hzr(GLOBAL_2D_ARRAY,N)
      common /grid_Hzr/Hzr
      real Hz_half(GLOBAL_2D_ARRAY,N)
      common /grid_Hz_half/Hz_half

#endif
