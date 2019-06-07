use netcdf

implicit none

integer err,i,id,idh,idu,idt,ido
integer idri,idrj,idui,iduj,idvi,idvj,idp
integer idrx,idry,idux,iduy,idvx,idvy
integer idpp,idhh,idhr,idhi,idzr,idzi,idmr,idmi
integer nx,ny
integer, dimension(:,:), allocatable    :: idata,zmask,umask,vmask
real(4), dimension(:,:), allocatable    :: data
real(4), dimension(:), allocatable      :: lon_r,lat_r
real(4), dimension(:), allocatable      :: lon_u,lat_u
real(4), dimension(:), allocatable      :: lon_v,lat_v
real(4), dimension(:,:), allocatable    :: h
real(4), dimension(:), allocatable      :: periods
character(len=80) name

! ... Input data
! ...
err = NF90_OPEN('grid_tpxo9.nc',0,idt)
err = NF90_OPEN('h_tpxo9.v1.nc',0,idh)
err = NF90_OPEN('u_tpxo9.v1.nc',0,idu)

err = NF90_INQUIRE_DIMENSION(idt,1,name,nx)
err = NF90_INQUIRE_DIMENSION(idt,2,name,ny)

print*, 'nx, ny = ', nx, ny
allocate(lon_r(nx))
allocate(lat_r(ny))
allocate(lon_u(nx))
allocate(lat_u(ny))
allocate(lon_v(nx))
allocate(lat_v(ny))
allocate(data(ny,nx))
allocate(h(nx,ny))
allocate(periods(10))

periods = [12.4206, 12.0, 12.6583, 11.9672, 23.9345, 25.8193, 24.0659, &
           26.8684, 327.8599, 661.31]

allocate(idata(ny,nx))
allocate(zmask(nx,ny))
allocate(umask(nx,ny))
allocate(vmask(nx,ny))

! ... Longitude
! ...
err = NF90_INQ_VARID(idt,'lon_z',id)
err = NF90_GET_VAR(idt,id,data)
print*, trim(nf90_strerror(err))
print*, data(1,1), data(1,nx)
print*, data(ny,1), data(ny,nx)
lon_r(:) = data(1,:)

err = NF90_INQ_VARID(idt,'lon_u',id)
err = NF90_GET_VAR(idt,id,data)
print*, trim(nf90_strerror(err))
print*, data(1,1), data(1,nx)
print*, data(ny,1), data(ny,nx)
lon_u(:) = data(1,:)

err = NF90_INQ_VARID(idt,'lon_v',id)
err = NF90_GET_VAR(idt,id,data)
print*, trim(nf90_strerror(err))
print*, data(1,1), data(1,nx)
print*, data(ny,1), data(ny,nx)
lon_v(:) = data(1,:)

! ... Latitude
! ...
err = NF90_INQ_VARID(idt,'lat_z',id)
err = NF90_GET_VAR(idt,id,data)
print*, trim(nf90_strerror(err))
print*, data(1,1), data(ny,1)
print*, data(1,nx), data(ny,nx)
lat_r(:) = data(:,1)

err = NF90_INQ_VARID(idt,'lat_u',id)
err = NF90_GET_VAR(idt,id,data)
print*, trim(nf90_strerror(err))
print*, data(1,1), data(ny,1)
print*, data(1,nx), data(ny,nx)
lat_u(:) = data(:,1)

err = NF90_INQ_VARID(idt,'lat_v',id)
err = NF90_GET_VAR(idt,id,data)
print*, trim(nf90_strerror(err))
print*, data(1,1), data(ny,1)
print*, data(1,nx), data(ny,nx)
lat_v(:) = data(:,1)

! ... Land-sea masks
! ...
err = NF90_INQ_VARID(idt,'mz',id)
err = NF90_GET_VAR(idt,id,idata)
print*, trim(nf90_strerror(err))
zmask = transpose(idata)

err = NF90_INQ_VARID(idt,'mu',id)
err = NF90_GET_VAR(idt,id,idata)
print*, trim(nf90_strerror(err))
umask = transpose(idata)

err = NF90_INQ_VARID(idt,'mv',id)
err = NF90_GET_VAR(idt,id,idata)
print*, trim(nf90_strerror(err))
vmask = transpose(idata)


! ... Topography
! ...
err = NF90_INQ_VARID(idt,'hz',id)
err = NF90_GET_VAR(idt,id,data)
print*, trim(nf90_strerror(err))
h = transpose(data)


! ... Output file:
! ...
err = NF90_CREATE('TPXO9.nc',NF90_WRITE,ido)
err = NF90_DEF_DIM(ido,'lon_r',nx,idri)
err = NF90_DEF_DIM(ido,'lat_r',ny,idrj)
err = NF90_DEF_DIM(ido,'lon_u',nx,idui)
err = NF90_DEF_DIM(ido,'lat_u',ny,iduj)
err = NF90_DEF_DIM(ido,'lon_v',nx,idvi)
err = NF90_DEF_DIM(ido,'lat_v',ny,idvj)
err = NF90_DEF_DIM(ido,'periods',10,idp)

err = NF90_DEF_VAR(ido,'lon_r',NF90_FLOAT,(/idri/),idrx)
err = NF90_DEF_VAR(ido,'lat_r',NF90_FLOAT,(/idrj/),idry)
err = NF90_DEF_VAR(ido,'lon_u',NF90_FLOAT,(/idui/),idux)
err = NF90_DEF_VAR(ido,'lat_u',NF90_FLOAT,(/iduj/),iduy)
err = NF90_DEF_VAR(ido,'lon_v',NF90_FLOAT,(/idvi/),idvx)
err = NF90_DEF_VAR(ido,'lat_v',NF90_FLOAT,(/idvj/),idvy)
err = NF90_DEF_VAR(ido,'periods',NF90_FLOAT,(/idp/),idpp)
err = NF90_DEF_VAR(ido,'h',NF90_FLOAT,(/idri,idrj/),idhh)
err = NF90_DEF_VAR(ido,'ssh_r',NF90_FLOAT,(/idri,idrj,idp/),idhr)
err = NF90_DEF_VAR(ido,'ssh_i',NF90_FLOAT,(/idri,idrj,idp/),idhi)
err = NF90_DEF_VAR(ido,'u_r',NF90_FLOAT,(/idri,idrj,idp/),idzr)
err = NF90_DEF_VAR(ido,'u_i',NF90_FLOAT,(/idri,idrj,idp/),idzi)
err = NF90_DEF_VAR(ido,'v_r',NF90_FLOAT,(/idri,idrj,idp/),idmr)
err = NF90_DEF_VAR(ido,'v_i',NF90_FLOAT,(/idri,idrj,idp/),idmi)

err = NF90_PUT_ATT(ido,idrx,'long_name','Longitude at Z points')
err = NF90_PUT_ATT(ido,idrx,'units','degree_east')
err = NF90_PUT_ATT(ido,idry,'long_name','Latitude at Z points')
err = NF90_PUT_ATT(ido,idry,'units','degree_north')
err = NF90_PUT_ATT(ido,idux,'long_name','Longitude at U points')
err = NF90_PUT_ATT(ido,iduy,'long_name','Latitude at U points')
err = NF90_PUT_ATT(ido,idvx,'long_name','Longitude at V points')
err = NF90_PUT_ATT(ido,idvy,'long_name','Latitude at V points')
err = NF90_PUT_ATT(ido,idpp,'long_name','Tide periods')
err = NF90_PUT_ATT(ido,idhh,'long_name','Topography at Z points')
err = NF90_PUT_ATT(ido,idhh,'units','meter')
err = NF90_PUT_ATT(ido,idhr,'long_name','Tidal elevation, Real part')
err = NF90_PUT_ATT(ido,idhr,'units','meter')
err = NF90_PUT_ATT(ido,idhr,'_FillValue',NF90_FILL_FLOAT)
err = NF90_PUT_ATT(ido,idhi,'long_name','Tidal elevation, Imag part')
err = NF90_PUT_ATT(ido,idhi,'units','meter')
err = NF90_PUT_ATT(ido,idhi,'_FillValue',NF90_FILL_FLOAT)
err = NF90_PUT_ATT(ido,idzr,'long_name','Tidal U-transport, Real part')
err = NF90_PUT_ATT(ido,idzr,'units','meter^2/s')
err = NF90_PUT_ATT(ido,idzr,'_FillValue',NF90_FILL_FLOAT)
err = NF90_PUT_ATT(ido,idzi,'long_name','Tidal U-transport, Imag part')
err = NF90_PUT_ATT(ido,idzi,'units','meter^2/s')
err = NF90_PUT_ATT(ido,idzi,'_FillValue',NF90_FILL_FLOAT)
err = NF90_PUT_ATT(ido,idmr,'long_name','Tidal V-transport, Real part')
err = NF90_PUT_ATT(ido,idmr,'units','meter^2/s')
err = NF90_PUT_ATT(ido,idmr,'_FillValue',NF90_FILL_FLOAT)
err = NF90_PUT_ATT(ido,idmi,'long_name','Tidal V-transport, Imag part')
err = NF90_PUT_ATT(ido,idmi,'units','meter^2/s')
err = NF90_PUT_ATT(ido,idmi,'_FillValue',NF90_FILL_FLOAT)
err = NF90_PUT_ATT(ido,0,'title','TPXO9.v1 2018 tidal elevation file')
err = NF90_PUT_ATT(ido,0,'date','13-May-2019')
err = NF90_PUT_ATT(ido,0,'components','M2 S2 N2 K2 K1 O1 P1 Q1 Mf Mm')
err = NF90_ENDDEF(ido)

print*, 'Writing lon_r ...'
err = NF90_PUT_VAR(ido,idrx,lon_r)
print*, trim(nf90_strerror(err))

print*, 'Writing lat_r ...'
err = NF90_PUT_VAR(ido,idry,lat_r)
print*, trim(nf90_strerror(err))

print*, 'Writing lon_u ...'
err = NF90_PUT_VAR(ido,idux,lon_u)
print*, trim(nf90_strerror(err))

print*, 'Writing lat_r ...'
err = NF90_PUT_VAR(ido,iduy,lat_u)
print*, trim(nf90_strerror(err))

print*, 'Writing lon_v ...'
err = NF90_PUT_VAR(ido,idvx,lon_v)
print*, trim(nf90_strerror(err))

print*, 'Writing lat_v ...'
err = NF90_PUT_VAR(ido,idvy,lat_v)
print*, trim(nf90_strerror(err))

print*, 'Writing periods ...'
err = NF90_PUT_VAR(ido,idpp,periods)
print*, trim(nf90_strerror(err))

print*, 'Writing topography ...'
err = NF90_PUT_VAR(ido,idhh,h)
print*, trim(nf90_strerror(err))

! ... SSH_R
! ...
print*, 'Writing SSR Real part ...'
err = NF90_INQ_VARID(idh,'hRe',id)
do i=1,8
   err = NF90_GET_VAR(idh,id,data,[1,1,i],[ny,nx,1])
   print*, trim(nf90_strerror(err))
   h = transpose(data)
   !where(zmask.eq.0) h = NF90_FILL_FLOAT
   where(zmask.eq.0) h = 0.0
   err = NF90_PUT_VAR(ido,idhr,h,[1,1,i],[nx,ny,1])
enddo

err = NF90_GET_VAR(idh,id,data,[1,1,10],[ny,nx,1])
print*, trim(nf90_strerror(err))
h = transpose(data)
!where(zmask.eq.0) h = NF90_FILL_FLOAT
where(zmask.eq.0) h = 0.0
err = NF90_PUT_VAR(ido,idhr,h,[1,1,9],[nx,ny,1])

err = NF90_GET_VAR(idh,id,data,[1,1,9],[ny,nx,1])
print*, trim(nf90_strerror(err))
h = transpose(data)
!where(zmask.eq.0) h = NF90_FILL_FLOAT
where(zmask.eq.0) h = 0.0
err = NF90_PUT_VAR(ido,idhr,h,[1,1,10],[nx,ny,1])


! ... SSH_I
! ...
print*, 'Writing SSR Imag part ...'
err = NF90_INQ_VARID(idh,'hIm',id)
do i=1,8
   err = NF90_GET_VAR(idh,id,data,[1,1,i],[ny,nx,1])
   print*, trim(nf90_strerror(err))
   h = transpose(data)
   !where(zmask.eq.0) h = NF90_FILL_FLOAT
   where(zmask.eq.0) h = 0.0
   err = NF90_PUT_VAR(ido,idhi,h,[1,1,i],[nx,ny,1])
enddo

err = NF90_GET_VAR(idh,id,data,[1,1,10],[ny,nx,1])
print*, trim(nf90_strerror(err))
h = transpose(data)
!where(zmask.eq.0) h = NF90_FILL_FLOAT
where(zmask.eq.0) h = 0.0
err = NF90_PUT_VAR(ido,idhi,h,[1,1,9],[nx,ny,1])

err = NF90_GET_VAR(idh,id,data,[1,1,9],[ny,nx,1])
print*, trim(nf90_strerror(err))
h = transpose(data)
!where(zmask.eq.0) h = NF90_FILL_FLOAT
where(zmask.eq.0) h = 0.0
err = NF90_PUT_VAR(ido,idhi,h,[1,1,10],[nx,ny,1])

! ... UR
! ...
print*, 'Writing U Re part ...'
err = NF90_INQ_VARID(idu,'URe',id)
do i=1,8
   err = NF90_GET_VAR(idu,id,data,[1,1,i],[ny,nx,1])
   print*, trim(nf90_strerror(err))
   h = transpose(data)
   !where(umask.eq.0) h = NF90_FILL_FLOAT
   where(umask.eq.0) h = 0.0
   err = NF90_PUT_VAR(ido,idzr,h,[1,1,i],[nx,ny,1])
enddo

err = NF90_GET_VAR(idu,id,data,[1,1,10],[ny,nx,1])
print*, trim(nf90_strerror(err))
h = transpose(data)
!where(umask.eq.0) h = NF90_FILL_FLOAT
where(umask.eq.0) h = 0.0
err = NF90_PUT_VAR(ido,idzr,h,[1,1,9],[nx,ny,1])

err = NF90_GET_VAR(idu,id,data,[1,1,9],[ny,nx,1])
print*, trim(nf90_strerror(err))
h = transpose(data)
!where(umask.eq.0) h = NF90_FILL_FLOAT
where(umask.eq.0) h = 0.0
err = NF90_PUT_VAR(ido,idzr,h,[1,1,10],[nx,ny,1])

! ... UI
! ...
print*, 'Writing U Im part ...'
err = NF90_INQ_VARID(idu,'UIm',id)
do i=1,8
   err = NF90_GET_VAR(idu,id,data,[1,1,i],[ny,nx,1])
   print*, trim(nf90_strerror(err))
   h = transpose(data)
   !where(umask.eq.0) h = NF90_FILL_FLOAT
   where(umask.eq.0) h = 0.0
   err = NF90_PUT_VAR(ido,idzi,h,[1,1,i],[nx,ny,1])
enddo

err = NF90_GET_VAR(idu,id,data,[1,1,10],[ny,nx,1])
print*, trim(nf90_strerror(err))
h = transpose(data)
!where(umask.eq.0) h = NF90_FILL_FLOAT
where(umask.eq.0) h = 0.0
err = NF90_PUT_VAR(ido,idzi,h,[1,1,9],[nx,ny,1])

err = NF90_GET_VAR(idu,id,data,[1,1,9],[ny,nx,1])
print*, trim(nf90_strerror(err))
h = transpose(data)
!where(umask.eq.0) h = NF90_FILL_FLOAT
where(umask.eq.0) h = 0.0
err = NF90_PUT_VAR(ido,idzi,h,[1,1,10],[nx,ny,1])


! ... VR
! ...
print*, 'Writing V Re part ...'
err = NF90_INQ_VARID(idu,'VRe',id)
do i=1,8
   err = NF90_GET_VAR(idu,id,data,[1,1,i],[ny,nx,1])
   print*, trim(nf90_strerror(err))
   h = transpose(data)
   !where(vmask.eq.0) h = NF90_FILL_FLOAT
   where(vmask.eq.0) h = 0.0
   err = NF90_PUT_VAR(ido,idmr,h,[1,1,i],[nx,ny,1])
enddo

err = NF90_GET_VAR(idu,id,data,[1,1,10],[ny,nx,1])
print*, trim(nf90_strerror(err))
h = transpose(data)
!where(vmask.eq.0) h = NF90_FILL_FLOAT
where(vmask.eq.0) h = 0.0
err = NF90_PUT_VAR(ido,idmr,h,[1,1,9],[nx,ny,1])

err = NF90_GET_VAR(idu,id,data,[1,1,9],[ny,nx,1])
print*, trim(nf90_strerror(err))
h = transpose(data)
!where(vmask.eq.0) h = NF90_FILL_FLOAT
where(vmask.eq.0) h = 0.0
err = NF90_PUT_VAR(ido,idmr,h,[1,1,10],[nx,ny,1])

! ... VI
! ...
print*, 'Writing V Im part ...'
err = NF90_INQ_VARID(idu,'VIm',id)
do i=1,8
   err = NF90_GET_VAR(idu,id,data,[1,1,i],[ny,nx,1])
   print*, trim(nf90_strerror(err))
   h = transpose(data)
   !where(vmask.eq.0) h = NF90_FILL_FLOAT
   where(vmask.eq.0) h = 0.0
   err = NF90_PUT_VAR(ido,idmi,h,[1,1,i],[nx,ny,1])
enddo

err = NF90_GET_VAR(idu,id,data,[1,1,10],[ny,nx,1])
print*, trim(nf90_strerror(err))
h = transpose(data)
!where(vmask.eq.0) h = NF90_FILL_FLOAT
where(vmask.eq.0) h = 0.0
err = NF90_PUT_VAR(ido,idmi,h,[1,1,9],[nx,ny,1])

err = NF90_GET_VAR(idu,id,data,[1,1,9],[ny,nx,1])
print*, trim(nf90_strerror(err))
h = transpose(data)
!where(vmask.eq.0) h = NF90_FILL_FLOAT
where(vmask.eq.0) h = 0.0
err = NF90_PUT_VAR(ido,idmi,h,[1,1,10],[nx,ny,1])


err = NF90_CLOSE(ido)

end

