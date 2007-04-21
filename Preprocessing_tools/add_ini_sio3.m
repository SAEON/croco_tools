function add_ini_sio3(ininame,grdname,oaname,cycle);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  function [longrd,latgrd,sio3]=add_ini_sio3(ininame,grdname,...
%                                           seas_datafile,ann_datafile,...
%                                           cycle);
%
%  pierrick 2001
%
%  Add phosphate (mMol P m-3) in a ROMS initial file.
%  take seasonal data for the upper levels and annual data for the
%  lower levels.
%  do a temporal interpolation to have values at initial
%  time.
%
%  input:
%    
%    ininame       : roms initial file to process (netcdf)
%    grdname      : roms grid file (netcdf)
%    seas_datafile : regular longitude - latitude - z seasonal data 
%                    file used for the upper levels  (netcdf)
%    ann_datafile  : regular longitude - latitude - z annual data 
%                    file used for the lower levels  (netcdf)
%    cycle         : time length (days) of climatology cycle (ex:360 for
%                    annual cycle) - 0 if no cycle.
%
%   output:
%
%    [longrd,latgrd,no3] : surface field to plot (as an illustration)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
disp('Add_ini_sio3: creating variable and attribute')
%
% open the grid file  
% 
nc=netcdf(grdname);
h=nc{'h'}(:);
close(nc);
%
% open the initial file  
% 
nc=netcdf(ininame,'write');
theta_s = nc{'theta_s'}(:);
theta_b =  nc{'theta_b'}(:);
hc  =  nc{'hc'}(:);
N =  length(nc('s_rho'));
%
% open the oa file  
% 
noa=netcdf(oaname);
z=-noa{'Zsi'}(:);
oatime=noa{'si_time'}(:);
tlen=length(oatime);
%
% Get the sigma depths
%
zroms=zlevs(h,0.*h,theta_s,theta_b,hc,N,'r');
zmin=min(min(min(zroms)));
zmax=max(max(max(zroms)));
%
% Check if the min z level is below the min sigma level 
%    (if not add a deep layer)
%
addsurf=max(z)<zmax;
addbot=min(z)>zmin;
if addsurf
 z=[100;z];
end
if addbot
 z=[z;-100000];
end
Nz=min(find(z<zmin));
z=z(1:Nz);
%
% open the initial file  
% 
nc=netcdf(ininame,'write');
theta_s = nc{'theta_s'}(:);
theta_b =  nc{'theta_b'}(:);
hc  =  nc{'hc'}(:);
N =  length(nc('s_rho'));
scrum_time = nc{'scrum_time'}(:);
scrum_time = scrum_time / (24*3600);
tinilen = length(scrum_time);
redef(nc);
nc{'Si'} = ncdouble('time','s_rho','eta_rho','xi_rho');
nc{'Si'}.long_name = ncchar('Silicate');
nc{'Si'}.long_name = 'Silicate';
nc{'Si'}.units = ncchar('mMol Si m-3');
nc{'Si'}.units = 'mMol Si m-3';
nc{'Si'}.fields = ncchar('Si, scalar, series');
nc{'Si'}.fields = 'Si, scalar, series';
endef(nc);
%
%  loop on initial time
%
for l=1:tinilen
  disp(['time index: ',num2str(l),' of total: ',num2str(tinilen)])
%
%  get data time indices and weights for temporal interpolation
%
  if cycle~=0
    modeltime=mod(scrum_time(l),cycle);
  else
    modeltime=scrum_time;
  end
  l1=find(modeltime==oatime);
  if isempty(l1)
    disp('temporal interpolation')
    l1=max(find(oatime<modeltime));
    time1=oatime(l1);
    if isempty(l1)
      if cycle~=0
        l1=tlen;
        time1=oatime(l1)-cycle;
      else
        error('No previous time in the dataset')
      end
    end
    l2=min(find(oatime>modeltime));
    time2=oatime(l2);
    if isempty(l2)
      if cycle~=0
        l2=1;
        time2=oatime(l2)+cycle;
      else
        error('No posterious time in the dataset')
      end
    end
    cff1=(modeltime-time2)/(time1-time2);
    cff2=(time1-modeltime)/(time1-time2);
  else
    cff1=1;
    l2=l1;
    cff2=0;
  end
%
% interpole the seasonal dataset on the horizontal roms grid
%
  disp(['Add_ini_sio3: vertical interpolation'])
  var=squeeze(noa{'Si'}(l1,:,:,:));
  if addsurf
    var=cat(1,var(1,:,:),var);
  end
  if addbot
    var=cat(1,var,var(end,:,:));
  end
  var2=squeeze(noa{'Si'}(l2,:,:,:));
  if addsurf
    var2=cat(1,var2(1,:,:),var2);
  end
  if addbot
    var2=cat(1,var2,var2(end,:,:));
  end
  var=cff1*var + cff2*var2;
  nc{'Si'}(l,:,:,:)=ztosigma(flipdim(var,1),zroms,flipud(z));
end
close(nc);
close(noa);
return
