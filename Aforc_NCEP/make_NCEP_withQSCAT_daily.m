%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Build a ROMS forcing file
%
%  The program take the NCEP forcing [and bulk (optional)] file[s] 
%  and replace tx, ty[, u, w, w] with the QSCAT data 
%  
%  Usage : First use the make_NCEP, then do the
%  make_NCEP_withQSCAT_daily.m

%  Extrapole and interpole surface data to get surface boundary
%  conditions for ROMS (forcing netcdf file)
%
%  Data input format (netcdf):
%     taux(T, Y, X)
%     T : time [Months]
%     Y : Latitude [degree north]
%     X : Longitude [degree east]
%
%  Data source : IRI/LDEO Climate Data Library 
%                (Atlas of Surface Marine Data 1994)
%
%    http://ingrid.ldgo.columbia.edu/
%    http://iridl.ldeo.columbia.edu/SOURCES/.DASILVA/
%
%  Pierrick Penven, IRD, 2002. 
%  Modified by Marchesiello, 2005, to use 
%             IFREMER/CERSAT MWF QuikSCAT daily winds.
%  Modified by Penven, jan 2007, to use OPENDAP.
%  Modified by S.Illig, March 2008
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
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all
close all
%%%%%%%%%%%%%%%%%%%%% USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%%
%
% Common parameters
%
romstools_param
%
%
if NCEP_version==1
  ncep_frc_prefix=[frc_prefix,'_NCEP1_'];                           
  ncep_blk_prefix=[blk_prefix,'_NCEP1_'];
elseif NCEP_version==2
  ncep_frc_prefix=[frc_prefix,'_NCEP2_'];                           
  ncep_blk_prefix=[blk_prefix,'_NCEP2_'];
end
%
%
%
QSCAT_frc_prefix=[ncep_frc_prefix,'wQS_'];
QSCAT_blk_prefix=[ncep_blk_prefix,'wQS_'];
%
%
%
taux_name='taux';
tauy_name='tauy';


if QSCAT_blk==1
  uwnd_name='uwnd';
  vwnd_name='vwnd';
  wnds_name='wnds';
end
%
%
%%%%%%%%%%%%%%%%%%% END USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%%
%
% Title
%
disp(' ')
disp(ROMS_title)
%
if level==0
  nc_suffix='.nc';
else
  nc_suffix=['.nc.',num2str(level)];
  grdname=[grdname,'.',num2str(level)];
end
%
% Get the model grid
%
nc=netcdf(grdname);
lon=nc{'lon_rho'}(:);
lat=nc{'lat_rho'}(:);
angle=nc{'angle'}(:);
close(nc)
cosa = cos(angle);
sina = sin(angle);
%
[MMp,LLp]=size(lon);
%
% Extract data over the internet
%
if Download_data==1
%
% Get the model limits
%
  lonmin=min(min(lon));
  lonmax=max(max(lon));
  latmin=min(min(lat));
  latmax=max(max(lat));
%
% Download QSCAT
% 
  disp('Download QSCAT data with OPENDAP')
  download_QSCAT(Ymin,Ymax,Mmin,Mmax,lonmin,lonmax,latmin,latmax,...
                 QSCAT_dir,Yorig,QSCAT_blk)
end
%
% Loop on the years and the months
%
for Y=Ymin:Ymax
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
    disp('====================================================== ')
    disp(' ')
    disp(['Processing  year ',num2str(Y),' - month ',num2str(M)])
    disp(' ')
    
%
%Copy forcing and bulk NCEP files to NCEP modified by  QSCAT wind and
%stress forcing and bulk file
%    
    file_frc_ncep=[ncep_frc_prefix,'Y',num2str(Y),'M',num2str(M),'.nc'];
    file_frc_qscat=[QSCAT_frc_prefix,'Y',num2str(Y),'M',num2str(M),'.nc'];
    disp(['Copying NCEP file ',file_frc_ncep,' in NCEP_wQS file : ',file_frc_qscat])     
    copyfile(file_frc_ncep,file_frc_qscat,'f')
    
    if QSCAT_blk==1 
      file_blk_ncep=[ncep_blk_prefix,'Y',num2str(Y),'M',num2str(M),'.nc'];
      file_blk_qscat=[QSCAT_blk_prefix,'Y',num2str(Y),'M',num2str(M),'.nc'];    
      disp(['Copying NCEP file ',file_blk_ncep,' in NCEP_wQS file : ',file_blk_qscat])     
      copyfile(file_blk_ncep,file_blk_qscat,'f')
    end
    
    
%
% Process the QuickSCAT time (here in days)
%
    nc=netcdf([QSCAT_dir,'tauxY',num2str(Y),'M',num2str(M),'.nc']);
    QSCAT_time=nc{'time'}(:);
    close(nc);
    dt=round(mean(gradient(QSCAT_time)));  
%
% Add 2 times step in the ROMS files: 1 at the beginning and 2 at the end 
% (otherwise.. ROMS crashes)
%
    tlen=length(QSCAT_time)+2;
    time=0*(1:tlen);
    time(2:end-1)=QSCAT_time;
    time(1)=datenum(Y,M,1)-datenum(Yorig,1,1)-1+dt/2;  
    time(end)=datenum(Y,M,eomday(Y,M))-datenum(Yorig,1,1)+1+dt/2; %%%% Modified by Serena
%    time(end)=datenum(Y,M,eomday(Y,M))-datenum(Yorig,1,1)+2+dt/2; %%%% Modified by Serena
%
% Initialize array 
%
    u=zeros(MMp,LLp,tlen);
    v=u;
    if QSCAT_blk==1  
      xu=u;    
      yv=u;
      ws=u;
    end
%
% Create a ROMS forcing/bulk file for each month
%
    nc_frc=netcdf(file_frc_qscat,'w');
    sms_time_ncep=nc_frc{'sms_time'}(:);
    
    if QSCAT_blk==1  
      nc_blk=netcdf(file_blk_qscat,'w');
      blk_time_ncep=nc_blk{'bulk_time'}(:); 
    end
%
% Add the wind
%
% 1. Check if there are QSCAT files for the previous Month
%
    Mm=M-1;
    Ym=Y;
    if Mm==0
      Mm=12;
      Ym=Y-1;
    end
    taux_file=[QSCAT_dir,'tauxY',num2str(Ym),'M',num2str(Mm),'.nc'];
    if exist(taux_file)==0
      disp(['   No data for the previous month: using current month'])
      tindex=1;
      Mm=M;
      Ym=Y;
    else
      nc=netcdf(taux_file);
      tindex=length(nc('time'));
      close(nc)
    end
%
% 2. Perform the interpolations for the previous month
%
    disp(['Perform the interpolations of QSCAT data ...'])
    disp(['...'])
    disp('First step')
    taux_file=[QSCAT_dir,'tauxY',num2str(Ym),'M',num2str(Mm),'.nc'];
    tauy_file=[QSCAT_dir,'tauyY',num2str(Ym),'M',num2str(Mm),'.nc'];
    u(:,:,1)=ext_data(taux_file,taux_name,tindex,...
               lon,lat,[],Roa,2);
    v(:,:,1)=ext_data(tauy_file,tauy_name,tindex,...
               lon,lat,[],Roa,2);
    
    if QSCAT_blk==1   
      uwnd_file=[QSCAT_dir,'uwndY',num2str(Ym),'M',num2str(Mm),'.nc'];
      vwnd_file=[QSCAT_dir,'vwndY',num2str(Ym),'M',num2str(Mm),'.nc']; 
      wnds_file=[QSCAT_dir,'wndsY',num2str(Ym),'M',num2str(Mm),'.nc'];       
      xu(:,:,1)=ext_data(uwnd_file,uwnd_name,tindex,...
               lon,lat,[],Roa,2);
      yv(:,:,1)=ext_data(vwnd_file,vwnd_name,tindex,...
               lon,lat,[],Roa,2); 
      ws(:,:,1)=ext_data(wnds_file,wnds_name,tindex,...
               lon,lat,[],Roa,2); 
    end
%
% 3. Perform the interpolations for the current month
%
    taux_file=[QSCAT_dir,'tauxY',num2str(Y),'M',num2str(M),'.nc'];
    tauy_file=[QSCAT_dir,'tauyY',num2str(Y),'M',num2str(M),'.nc'];
    for tindex=2:tlen-1
      u(:,:,tindex)=ext_data(taux_file,taux_name,tindex-1,...
                 lon,lat,[],Roa,2);
      v(:,:,tindex)=ext_data(tauy_file,tauy_name,tindex-1,...
                 lon,lat,[],Roa,2);
    end
    
    if QSCAT_blk==1 
      uwnd_file=[QSCAT_dir,'uwndY',num2str(Y),'M',num2str(M),'.nc'];
      vwnd_file=[QSCAT_dir,'vwndY',num2str(Y),'M',num2str(M),'.nc']; 
      wnds_file=[QSCAT_dir,'wndsY',num2str(Y),'M',num2str(M),'.nc'];    
      for tindex=2:tlen-1
        xu(:,:,tindex)=ext_data(uwnd_file,uwnd_name,tindex-1,...
               lon,lat,[],Roa,2);
        yv(:,:,tindex)=ext_data(vwnd_file,vwnd_name,tindex-1,...
               lon,lat,[],Roa,2); 
        ws(:,:,tindex)=ext_data(wnds_file,wnds_name,tindex-1,...
               lon,lat,[],Roa,2); 
      end
    end
%
% 4. Read the QSCAT file for the next month
%
    Mp=M+1;
    Yp=Y;
    if Mp==13
      Mp=1;
      Yp=Y+1;
    end
    taux_file=[QSCAT_dir,'tauxY',num2str(Yp),'M',num2str(Mp),'.nc'];
    if exist(taux_file)==0
      disp(['   No data for the next month: using current month'])
      tindex=tlen-2;
      Mp=M;
      Yp=Y;
    else
      nc=netcdf(taux_file);
      tindex=1;
      nc_frc{'sms_time'}(tlen)=nc{'time'}(tindex);
      close(nc)
    end
%
% 5. Perform the interpolations for the next month
%
    disp('Last step')
    taux_file=[QSCAT_dir,'tauxY',num2str(Yp),'M',num2str(Mp),'.nc'];
    tauy_file=[QSCAT_dir,'tauyY',num2str(Yp),'M',num2str(Mp),'.nc'];
    u(:,:,tlen)=ext_data(taux_file,taux_name,tindex,...
               lon,lat,[],Roa,2);
    v(:,:,tlen)=ext_data(tauy_file,tauy_name,tindex,...
               lon,lat,[],Roa,2);
 
    
    if QSCAT_blk==1 
      uwnd_file=[QSCAT_dir,'uwndY',num2str(Yp),'M',num2str(Mp),'.nc'];
      vwnd_file=[QSCAT_dir,'vwndY',num2str(Yp),'M',num2str(Mp),'.nc']; 
      wnds_file=[QSCAT_dir,'wndsY',num2str(Yp),'M',num2str(Mp),'.nc'];       
      xu(:,:,tlen)=ext_data(uwnd_file,uwnd_name,tindex,...
               lon,lat,[],Roa,2);
      yv(:,:,tlen)=ext_data(vwnd_file,vwnd_name,tindex,...
               lon,lat,[],Roa,2); 
      ws(:,:,tlen)=ext_data(wnds_file,wnds_name,tindex,...
               lon,lat,[],Roa,2);         
    end    
    %

    ny=size(u,1);
    nx=size(u,2); 
    nt=size(u,3); 
    nt2=size(sms_time_ncep,1);
    %
    uu=zeros(ny,nx,nt2);
    vv=zeros(ny,nx,nt2); 
    
    if QSCAT_blk==1 
      uuwnd=zeros(ny,nx,nt2);
      vvwnd=zeros(ny,nx,nt2); 
      wwnds=zeros(ny,nx,nt2); 
    end
%
%
% Proceed the time interpolation of QSCAT-modified NCEP frc/bulk file
% (u,v,w for bulk and ustr and vstr for frc) on the NCEP time axis
% 4X daily generaly
%

disp('--------------')
disp(['Proceed time interpolation on NCEP time axis...'])    
disp('--------------')

tic
    for ii=1:nx
      for jj=1:ny
        uu(jj,ii,:)=interp1(time,squeeze(u(jj,ii,:)),sms_time_ncep,'cubic');  
        vv(jj,ii,:)=interp1(time,squeeze(v(jj,ii,:)),sms_time_ncep,'cubic'); 
	
        if QSCAT_blk==1   
          uuwnd(jj,ii,:)=interp1(time,squeeze(xu(jj,ii,:)),blk_time_ncep,'cubic');  
          vvwnd(jj,ii,:)=interp1(time,squeeze(yv(jj,ii,:)),blk_time_ncep,'cubic');  
          wwnds(jj,ii,:)=interp1(time,squeeze(ws(jj,ii,:)),blk_time_ncep,'cubic');
        end
	
      end
    end
toc
%
% Fill the frc and/or bulk file 
%
disp('--------------')
disp('Fill the frc and/or bulk file')
disp('--------------')

    for ll=1:nt2
      u=squeeze(uu(:,:,ll));
      v=squeeze(vv(:,:,ll));      
      nc_frc{'sustr'}(ll,:,:)=rho2u_2d(u.*cosa + v.*sina);
      nc_frc{'svstr'}(ll,:,:)=rho2v_2d(v.*cosa - u.*sina);
      
      if QSCAT_blk==1  

        nc_blk{'sustr'}(ll,:,:)=rho2u_2d(u.*cosa + v.*sina);
        nc_blk{'svstr'}(ll,:,:)=rho2v_2d(v.*cosa - u.*sina);

        u=squeeze(uuwnd(:,:,ll));
        v=squeeze(vvwnd(:,:,ll));      
        nc_blk{'uwnd'}(ll,:,:)=rho2u_2d(u.*cosa + v.*sina);
        nc_blk{'vwnd'}(ll,:,:)=rho2v_2d(v.*cosa - u.*sina); 
        nc_blk{'wspd'}(ll,:,:)=squeeze(wwnds(:,:,ll));     
      end
      
    end
    close(nc_frc)
%
    if QSCAT_blk==1
      close(nc_blk)
    end
  %
  end
end
disp('--------------')
disp(['Finish, you have now NCEP file (frc and/or bulk files) with',...
     ' wind speed and wind stress from QSCAT'])
disp('--------------')	

%
% Spin-up: (reproduce the first year 'SPIN_Long' times)
% just copy the files for the first year and change the time
%
if SPIN_Long>0
  M=Mmin-1;
  Y=Ymin-SPIN_Long;
  for month=1:12*SPIN_Long
    M=M+1;
    if M==13
      M=1; 
      Y=Y+1;
    end
%
% Forcing files
%
    if makefrc==1
%
% Copy the file
%
      frcname=[QSCAT_frc_prefix,'Y',num2str(Ymin),'M',num2str(M),nc_suffix];
      frcname2=[QSCAT_frc_prefix,'Y',num2str(Y),'M',num2str(M),nc_suffix];
      disp(['Create ',frcname2]) 
      eval(['!cp ',frcname,' ',frcname2]) 
%
% Change the time
%
      nc=netcdf(frcname2,'write');
      time=nc{'sms_time'}(:)+datenum(Yorig,1,1);
      [y,m,d,h,mi,s]=datevec(time);
      dy=Ymin-Y;
      y=y-dy;
      time=datenum(y,m,d,h,mi,s)-datenum(Yorig,1,1);
%      disp(datestr(time+datenum(Yorig,1,1)))
      nc{'sms_time'}(:)=time;
      close(nc)
    end
%
% Bulk files
%
    if makeblk==1
%
% Copy the file
%
      blkname=[QSCAT_blk_prefix,'Y',num2str(Ymin),'M',num2str(M),nc_suffix];
      blkname2=[QSCAT_blk_prefix,'Y',num2str(Y),'M',num2str(M),nc_suffix];
      disp(['Create ',blkname2]) 
      eval(['!cp ',blkname,' ',blkname2]) 
%
% Change the time
%
      nc=netcdf(blkname2,'write');
      time=nc{'bulk_time'}(:)+datenum(Yorig,1,1);
      [y,m,d,h,mi,s]=datevec(time);
      dy=Ymin-Y;
      y=y-dy;
      time=datenum(y,m,d,h,mi,s)-datenum(Yorig,1,1);
%      disp(datestr(time+datenum(Yorig,1,1)))
      nc{'bulk_time'}(:)=time;
      close(nc)
    end
  end
end


%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%---------------------------------------------------------------
% Make a few plots
%---------------------------------------------------------------
if makeplot==1
  disp(' ')
  disp(' Make a few plots...')
  slides=[1 25 50 75];
  test_forcing(file_frc_qscat,grdname,'svstr',slides,3,coastfileplot)
  figure
  test_forcing(file_frc_qscat,grdname,'sustr',slides,3,coastfileplot)
  if QSCAT_blk==1  
    figure
    test_forcing(file_blk_qscat,grdname,'uwnd',slides,3,coastfileplot)
    figure
    test_forcing(file_blk_qscat,grdname,'vwnd',slides,3,coastfileplot)
    figure
    test_forcing(file_blk_qscat,grdname,'wspd',slides,3,coastfileplot)
  end
end
