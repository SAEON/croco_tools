function add_ini_fer(ininame,grdname,oaname,cycle);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  function [longrd,latgrd,fer]=add_ini_fer(ininame,grdname,...
%                                           seas_datafile,ann_datafile,...
%                                           cycle);
%
%  pierrick 2001
%
%  Add nitrate (mMol N m-3) in a ROMS initial file.
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
%    [longrd,latgrd,fer] : surface field to plot (as an illustration)
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
disp('Add_ini_fer: creating variable and attribute')
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
z=-noa{'Zfer'}(:);
oatime=noa{'fer_time'}(:);
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
nc{'FER'} = ncdouble('time','s_rho','eta_rho','xi_rho');
nc{'FER'}.long_name = ncchar('Iron');
nc{'FER'}.long_name = 'Iron';
nc{'FER'}.units = ncchar('uMol Fe m-3');
nc{'FER'}.units = 'uMol Fe m-3';
nc{'FER'}.fields = ncchar('FER, scalar, series');
nc{'FER'}.fields = 'FER, scalar, series';
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
  disp(['Add_ini_fer: vertical interpolation'])
  var=squeeze(noa{'FER'}(l1,:,:,:));
  if addsurf
    var=cat(1,var(1,:,:),var);
  end
  if addbot
    var=cat(1,var,var(end,:,:));
  end
  var2=squeeze(noa{'FER'}(l2,:,:,:));
  if addsurf
    var2=cat(1,var2(1,:,:),var2);
  end
  if addbot
    var2=cat(1,var2,var2(end,:,:));
  end
  var=cff1*var + cff2*var2;
  nc{'FER'}(l,:,:,:)=ztosigma(flipdim(var,1),zroms,flipud(z));
end
close(nc);
close(noa);
return
