

clear all
close all


%%%%%%%%%%%%%%%%%%%%% USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%%
%
crocotools_param
prt_grd=[RUN_dir,'/CROCO_FILES/croco_grd.nc'];
chd_grd=[RUN_dir,'/CROCO_FILES/croco_grd.nc.1'];
%
%%%%%%%%%%%%%%%%%%% END USERS DEFINED VARIABLES %%%%%%%%%%%%%%%%%%%%%%%
warning off
%
% Title
%
disp(' ')
disp([' Find ', chd_grd, ' in ',prt_grd])
disp(' ')
%
% Load data
%
nc=netcdf(prt_grd);
Plonp=squeeze(nc{'lon_psi'}(2,:));
Platp=squeeze(nc{'lat_psi'}(:,2));
close(nc)

nc=netcdf(chd_grd);
Clonp=squeeze(nc{'lon_psi'}(2,:));
Clatp=squeeze(nc{'lat_psi'}(:,2));
close(nc)
%
% Test if correct
%
if Clonp(1)<Plonp(1)
    error(['Child Western boundary not in parent'])
end

if Clonp(end)>Plonp(end)
    error(['Child Eastern boundary not in parent'])
end

if Clatp(1)<Platp(1)
    error(['Child Southern boundary not in parent'])
end

if Clatp(end)>Platp(end)
    error(['Child Northern boundary not in parent'])
end
%
% Find index
%
[val,i_min] = min(abs(Plonp(:)-Clonp(1)));
[val,i_max] = min(abs(Plonp(:)-Clonp(end)));
[val,j_min] = min(abs(Platp(:)-Clatp(1)));
[val,j_max] = min(abs(Platp(:)-Clatp(end)));
%
%some change in some specific cases
disp([sprintf('%0.8f',Clonp(1)),' ',sprintf('%0.8f',Plonp(i_min))])
disp([sprintf('%0.8f',Clonp(end)),' ',sprintf('%0.8f',Plonp(i_max))])
disp([sprintf('%0.8f',Clatp(1)),' ',sprintf('%0.8f',Platp(j_min))])
disp([sprintf('%0.8f',Clatp(end)),' ',sprintf('%0.8f',Platp(j_max))])

%

%
% Find refinement level
%
refinecoeff=0;
for k = 1:(size(Clonp,2)-2)
    if abs(Plonp(i_min+1)-Clonp(k))>min(abs(Plonp(i_min+1)-Clonp(:)))
        refinecoeff=refinecoeff+1;
    else
        break
    end

    if k == (size(Clonp,2)-2)
        error(['End of child grid was reached without finding second parent point --> please check your grids'])
    end
end
%
disp(' ')
disp(['The refinement coefficient between Prt and Chd is ',num2str(refinecoeff)])
disp(' ')

%
% Rewrite netcdf
%
nc=netcdf(chd_grd);
latp=nc{'lat_psi'}(:);
lonp=nc{'lon_psi'}(:);
xp=nc{'x_psi'}(:);
yp=nc{'y_psi'}(:);
maskp=nc{'mask_psi'}(:);
%
latu=nc{'lat_u'}(:);
lonu=nc{'lon_u'}(:);
xu=nc{'x_u'}(:);
yu=nc{'y_u'}(:);
masku=nc{'mask_u'}(:);
%
latv=nc{'lat_v'}(:);
lonv=nc{'lon_v'}(:);
xv=nc{'x_v'}(:);
yv=nc{'y_v'}(:);
maskv=nc{'mask_v'}(:);
%
latr=nc{'lat_rho'}(:);
lonr=nc{'lon_rho'}(:);
xr=nc{'x_rho'}(:);
yr=nc{'y_rho'}(:);
maskr=nc{'mask_rho'}(:);
%
h=nc{'h'}(:);
f=nc{'f'}(:);
alpha=nc{'alpha'}(:);
dndx=nc{'dndx'}(:);
dmde=nc{'dmde'}(:);
angle=nc{'angle'}(:);
pm=nc{'pm'}(:);
pn=nc{'pn'}(:);
%
[Mp,Lp]=size(lonp);
close(nc)
%
create_nestedgrid(Lp,Mp,chd_grd,prt_grd,'title')
%
nw=netcdf(chd_grd,'write');
nw{'refine_coef'}(:)=refinecoeff;
nw{'grd_pos'}(:) = [i_min,i_max,j_min,j_max];
nw{'lat_u'}(:)=latu;
nw{'lon_u'}(:)=lonu;
nw{'x_u'}(:)=xu;
nw{'y_u'}(:)=yu;
nw{'mask_u'}(:)=masku;
%
nw{'lat_v'}(:)=latv;
nw{'lon_v'}(:)=lonv;
nw{'x_v'}(:)=xv;
nw{'y_v'}(:)=yv;
nw{'mask_v'}(:)=maskv;
%
nw{'lat_rho'}(:)=latr;
nw{'lon_rho'}(:)=lonr;
nw{'x_rho'}(:)=xr;
nw{'y_rho'}(:)=yr;
nw{'mask_rho'}(:)=maskr;
%
nw{'lat_psi'}(:)=latp;
nw{'lon_psi'}(:)=lonp;
nw{'x_psi'}(:)=xp;
nw{'y_psi'}(:)=yp;
nw{'mask_psi'}(:)=maskp;
%
nw{'h'}(:)=h;
nw{'pm'}(:)=pm;
nw{'pn'}(:)=pn;
nw{'hraw'}(1,:,:)=h;
nw{'angle'}(:)=angle;
nw{'f'}(:)=f;
nw{'spherical'}(:)='T';
nw{'dndx'}(:)=dndx;
nw{'dmde'}(:)=dmde;
%
%Create global attributes
%
nw.title = ncchar(CROCO_title);
nw.title = CROCO_title;
nw.date = ncchar(date);
nw.date = date;
nw.parent_grid = ncchar(grdname);
nw.parent_grid = grdname;

close(nw)
