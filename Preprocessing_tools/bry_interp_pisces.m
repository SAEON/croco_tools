function bry_interp(zbryname,lon,lat,seas_datafile,ann_datafile,...
                    dataname,vname,obcndx);
[M,L]=size(lon);
%
% set the value of ro (oa decorrelation scale [m]) 
% and default (value if no data)
%
ro=1e8;
default=NaN;
%
% Read in the datafile
%
ncseas=netcdf(seas_datafile);
X=ncseas{'X'}(:);
Y=ncseas{'Y'}(:);
Zseas=-ncseas{'Z'}(:);
T=ncseas{'T'}(:);
tlen=length(T);
Nzseas=length(Zseas);
%
% get the boundary position
%
if obcndx==1
%
% Southern boundary
% 
  iroms=(1:L);
  jroms=1;
elseif obcndx==2
%
% Eastern boundary
% 
  iroms=L;
  jroms=(1:M);
elseif obcndx==3
%
% Northern boundary
% 
  iroms=(1:L);
  jroms=M;
elseif obcndx==4
%
% Western boundary
% 
  iroms=1;
  jroms=(1:M);
end
%
lon=lon(jroms,iroms);
lat=lat(jroms,iroms);
%
% get a data subgrid (dependant of the OBC used)
%
dl=1.6;
lonmin=min(min(lon))-dl;
lonmax=max(max(lon))+dl;
latmin=min(min(lat))-dl;
latmax=max(max(lat))+dl;
%
j=find(Y>=latmin & Y<=latmax);
i1=find(X-360>=lonmin & X-360<=lonmax);
i2=find(X>=lonmin & X<=lonmax);
i3=find(X+360>=lonmin & X+360<=lonmax);
x=cat(1,X(i1)-360,X(i2),X(i3)+360);
y=Y(j);
%
% Open the Z-boundary file
%
nc=netcdf(zbryname,'write');
Z=-nc{'Z'}(:);
Nz=length(Z);
%
% Check the time
%
tbry=nc{'bry_time'}(:); 
T=T*30; % if time in month in the dataset !!!
if (tbry~=T)
  num2str(tbry)
  num2str(T)
  error(['time mismatch  tbry = ',num2str(tbry),...
                              '  t = ',num2str(T)])
end
%
% Read the annual dataset
%
if Nz > Nzseas
  ncann=netcdf(ann_datafile);
  zann=-ncann{'Z'}(1:Nz);
  if (Z~=zann)
    error('Vertical levels mismatch')
  end
%
% Interpole the annual dataset on the horizontal ROMS grid
%
  disp(' Ext tracers: horizontal interpolation of the annual data')
  if Zseas~=zann(1:length(Zseas)) 
    error('vertical levels dont match')
  end
  dims=size(lon);
  datazgrid=zeros(Nz,length(lon));
  missval=ncann{dataname}.missing_value(:);
  for k=Nzseas+1:Nz
    if ~isempty(i2)
      data=squeeze(ncann{dataname}(k,j,i2));
    else
      data=[];
    end
    if ~isempty(i1)
      data=cat(2,squeeze(ncann{dataname}(k,j,i1)),data);
    end
    if ~isempty(i3)
      data=cat(2,data,squeeze(ncann{dataname}(k,j,i3)));
    end
    data=get_missing_val(x,y,data,missval,ro,default);
    if dims(1)==1
      datazgrid(k,:)=interp2(x,y,data,lon,lat,'linear');
    else
      datazgrid(k,:)=(interp2(x,y,data,lon,lat,'linear'))';    
    end
  end
  close(ncann);
end
%
% interpole the seasonal dataset on the horizontal roms grid
%
disp([' Ext tracers: horizontal interpolation of the seasonal data'])
%
% loop on time
%

missval=ncseas{dataname}.missing_value(:);
for l=1:tlen
%for l=1:1
  disp(['time index: ',num2str(l),' of total: ',num2str(tlen)])
  dims=size(lon);
  if Nz <= Nzseas
%    datazgrid=zeros(Nz,M,L);
    datazgrid=zeros(Nz,length(lon));
  end
  for k=1:min([Nz Nzseas])
    if ~isempty(i2)
      data=squeeze(ncseas{dataname}(l,k,j,i2));
    else
      data=[];
    end
    if ~isempty(i1)
      data=cat(2,squeeze(ncseas{dataname}(l,k,j,i1)),data);
    end
    if ~isempty(i3)
      data=cat(2,data,squeeze(ncseas{dataname}(l,k,j,i3)));
    end
    data=get_missing_val(x,y,data,missval,ro,default);
    if dims(1)==1
      datazgrid(k,:)=interp2(x,y,data,lon,lat,'linear');
    else
      datazgrid(k,:)=(interp2(x,y,data,lon,lat,'linear'))';    
    end
  end
  nc{vname}(l,:,:,:)=datazgrid;
end
close(nc);
close(ncseas);
return

