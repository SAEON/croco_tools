function download_QSCAT(Ymin,Ymax,Mmin,Mmax,lonmin,lonmax,latmin,latmax,...
                        QSCAT_dir,Yorig,QSCAT_blk)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Extract a subgrid from QSCAT to get a ROMS forcing
% Store that into monthly files (to limit the problems
% of bandwith...).
% Take care of the Greenwitch Meridian.
% 
% 
%  Further Information:  
%  http://www.brest.ird.fr/Roms_tools/
%  
%  This file is part of ROMSTOOLS
%
%  ROMSTOOLS is free software; you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published
%  by the Free Software Foundation; either version 2 of the License,
%  or (at your option) any later version.
%
%  ROMSTOOLS is distributed in the hope that it will be useful, but
%  WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program; if not, write to the Free Software
%  Foundation, Inc., 59 Temple Place, Suite 330, Boston,
%  MA  02111-1307  USA
%
%  Copyright (c) 2007 by Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr  
% 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
if nargin < 1
  Ymin=2000;
  Ymax=2000;
  Yorig=1900;
  Mmin=1;
  Mmax=3;
  lonmin=12.3;
  lonmax=20.45;
  latmin=-35.5;
  latmax=-26.5;
  QSCAT_dir='DATA/QSCAT_Benguela/';
  QSCAT_blk    = 0;  
end
%
url='http://www.ifremer.fr/dodsG/CERSAT/quikscat_daily';
%
% start
%
disp([' '])
disp(['Get QSCAT wind from ',num2str(Ymin),' to ',num2str(Ymax)])
disp(['Minimum Longitude: ',num2str(lonmin)])
disp(['Maximum Longitude: ',num2str(lonmax)])
disp(['Minimum Latitude: ',num2str(latmin)])
disp(['Maximum Latitude: ',num2str(latmax)])
disp([' '])
%
% Create the directory
%
disp(['Making output data directory ',QSCAT_dir])
eval(['!mkdir ',QSCAT_dir])
%
% Find a subset of the QSCAT grid
%
[i1min,i1max,i2min,i2max,i3min,i3max,jrange,lon,lat]=...
get_QSCAT_grid(url,lonmin,lonmax,latmin,latmax);


disp(['...DOWNLOAD.....'])
disp(['...INFO.....'])
disp(['url= ',url])
disp(['lonmin= ',num2str(lonmin)])
disp(['lonmax= ',num2str(lonmax)])
disp(['latmin= ',num2str(latmin)])
disp(['latmax= ',num2str(latmax)])
disp(['SIZE LON=',num2str(size(lon))])
disp(['SIZE LAT=',num2str(size(lat))])
disp(['........'])
%
% Get the time
%
time=readdap(url,'time',[]);
%
% Convert the time into "Yorig" time (i.e in days since Yorig/1/1 00:00:0.0)
%

%disp('TIME is=')
%time(1:10)

time=time+datenum(1,1,1)-datenum(Yorig,1,1)-2; %-2 to match with CERSAT dates%
[year,month,days,hour,min,sec]=datevec(time+datenum(Yorig,1,1));

%disp(['TIME is='])
%time(1:10)
%year(1:10)
%month(1:10)
%days(1:10)
%time(end-9:end)
%year(end-9:end)
%month(end-9:end)
%days(end-9:end)

%
% Loop on the years
%
for Y=Ymin:Ymax
  disp(['Processing year: ',num2str(Y)])
%
% Loop on the months
%
  if Y==Ymin
    mo_min=Mmin;
  else
    mo_min=1;
  end
  if Y==Ymax
    mo_max=Mmax;
  else
    mo_max=12;
  end
  for M=mo_min:mo_max
    disp(['  Processing month: ',num2str(M)])
%
% Get the time indices for this month
%
    tndx=find(month==M & year==Y);
    taux=zeros(length(tndx),length(lat),length(lon));
    tauy=0*taux;
    n=0; clear good_time;clear taux; clear tauy
    for i=tndx(1):tndx(end)   
      disp(['    Processing day: ',num2str(n)])
      trange=['[',num2str(i-1),':',num2str(i-1),']'];
      x=getdap(url,[],'zwst',trange,[],jrange,...
                         i1min,i1max,i2min,i2max,i3min,i3max);
      y=getdap(url,[],'mwst',trange,[],jrange,...
                           i1min,i1max,i2min,i2max,i3min,i3max);
      
      
      if QSCAT_blk
	xu=getdap(url,[],'zws',trange,[],jrange,...
                         i1min,i1max,i2min,i2max,i3min,i3max);
	yv=getdap(url,[],'mws',trange,[],jrange,...
                         i1min,i1max,i2min,i2max,i3min,i3max);
	ws=getdap(url,[],'ws',trange,[],jrange,...
                         i1min,i1max,i2min,i2max,i3min,i3max);
	
      end
      
      x(x==-32767)=NaN;
      y(y==-32767)=NaN;
      if QSCAT_blk
      xu(xu==-32767)=NaN;
      yv(yv==-32767)=NaN;
      ws(ws==-32767)=NaN;
      end
 
      if (isnan(max(max(x))) | isnan(max(max(y))))
        disp('Warning : nan value')
      elseif ((max(max(x))==0) | (max(max(y))==0))
        disp('Warning : 0 value')
      else
        n=n+1;
        good_time(n)=time(i);
        taux(n,:,:)=x;
        tauy(n,:,:)=y;
	if QSCAT_blk
	  uwnd(n,:,:)=xu;
	  vwnd(n,:,:)=yv;
	  wnds(n,:,:)=ws;
        end
      end  
    end

    %  
    disp('Checking filling of the maps...')
disp(['...INFO.....'])
size(lon)
size(lat)
disp(['........'])    
    
    tot=length(lat)*length(lon);
    
    nbmask=max(sum(sum(squeeze(floor(mean(isnan(taux(1:n,:,:)),1))))),1);
    size( sum(sum(squeeze(floor(mean(isnan(taux(1:n,:,:)),1))))) ) 
    to_keep=[];
    for k=1:n
       tab=squeeze(taux(k,:,:));
       per=(sum(sum(isnan(tab)))-nbmask)/tot*100.;
       if per >= 5.
         disp([''])
         disp(['***********************************************'])
         disp(['More than 5% bad values -> map ',num2str(k), ' removed'])
         disp(['For your info -> per=  ',num2str(per),'%']) 
         disp(['***********************************************'])         
         disp([''])
       else
         to_keep=[to_keep,k];
       end 
    end
    %
    good_time=good_time(to_keep);
    taux=taux(to_keep,:,:);
    tauy=tauy(to_keep,:,:); 
    
    if QSCAT_blk
	uwnd=uwnd(to_keep,:,:);
	vwnd=vwnd(to_keep,:,:);
	wnds=wnds(to_keep,:,:);
    end
    
%
% Check for erroneous data values abs>10*max(median)
    disp('Checking erroneous data values...')
    nt=length(to_keep);
    
%
    med=median(taux,1);
%    x_ind=find( abs(taux-med(ones(nt,1),:,:)) >= 5*max(max(abs(med))) );
    x_ind=find( abs(taux-repmat(med,[nt,1,1])) >= 5*max(max(abs(med))) );
    taux(x_ind)=NaN;
%
    med=median(tauy,1);
%    y_ind=find( abs(tauy-med(ones(nt,1),:,:)) >= 5*max(max(abs(med))) );
    y_ind=find( abs(tauy-repmat(med,[nt,1,1])) >= 5*max(max(abs(med))) );
    tauy(y_ind)=NaN;
%
if QSCAT_blk
%
    med=median(uwnd,1);
%    x_ind=find( abs(taux-med(ones(nt,1),:,:)) >= 5*max(max(abs(med))) );
    x_ind=find( abs(uwnd-repmat(med,[nt,1,1])) >= 5*max(max(abs(med))) );
    uwnd(x_ind)=NaN;
%   
    med=median(vwnd,1);
%    y_ind=find( abs(tauy-med(ones(nt,1),:,:)) >= 5*max(max(abs(med))) );
    y_ind=find( abs(vwnd-repmat(med,[nt,1,1])) >= 5*max(max(abs(med))) );
    vwnd(y_ind)=NaN;
  
    med=median(wnds,1);
%    y_ind=find( abs(tauy-med(ones(nt,1),:,:)) >= 5*max(max(abs(med))) );
    y_ind=find( abs(wnds-repmat(med,[nt,1,1])) >= 5*max(max(abs(med))) );
    wnds(y_ind)=NaN;
end

    
    write_NCEP([QSCAT_dir,'taux','Y',num2str(Y),'M',num2str(M),'.nc'],...
                'taux',lon,lat,good_time,taux,Yorig)
    write_NCEP([QSCAT_dir,'tauy','Y',num2str(Y),'M',num2str(M),'.nc'],...
                'tauy',lon,lat,good_time,tauy,Yorig)
    
    if QSCAT_blk
    write_NCEP([QSCAT_dir,'uwnd','Y',num2str(Y),'M',num2str(M),'.nc'],...
                'uwnd',lon,lat,good_time,uwnd,Yorig)
    write_NCEP([QSCAT_dir,'vwnd','Y',num2str(Y),'M',num2str(M),'.nc'],...
                'vwnd',lon,lat,good_time,vwnd,Yorig)
    write_NCEP([QSCAT_dir,'wnds','Y',num2str(Y),'M',num2str(M),'.nc'],...
                'wnds',lon,lat,good_time,wnds,Yorig)
    end
  end
end
return
 

