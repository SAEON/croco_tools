%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  plot ROMS-1d variables
%
%  j: time in days
%  z: vertical coordinate
%  var: 1DV variable
%
%  Patrick Marchesiello, IRD, Dec 2012
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all
close all

romstools_param

OUTDIR='./';

dstr=260; dend=266;

biology=0;

%=============================================
if (lat<0),
 dstr=dstr-180; dend=dend-180;
end
%
fname=[OUTDIR,'t01.dat'];
var=load(fname)';
var=wextend('addrow','sp0',var,1,'d');
temp=var;
%
fname=[OUTDIR,'tsk.dat'];
tsk=load(fname)';
temp(end,:)=tsk;
%
fname=[OUTDIR,'days.dat'];
j=load(fname);
NT=length(j);
%
fname=[OUTDIR,'z.dat'];
var=load(fname);
var=wextend('addcol','sp0',var,1,'d');
z=var;
z(end)=0.;
N=length(z);
%
fname=[OUTDIR,'zbl.dat'];
zbl=load(fname);
windowSize = 32;
mzbl=filter(ones(1,windowSize)/windowSize,1,zbl);
%
fname=[OUTDIR,'t02.dat'];
var=load(fname)';
var=wextend('addrow','sp0',var,1,'d');
salt=var;
%
fname=[OUTDIR,'akt.dat'];
var=load(fname)';
var=wextend('addrow','sp0',var,1,'d');
akt=var;
%
if biology,
 fname=[OUTDIR,'t03.dat'];
 var=load(fname)';
 var=wextend('addrow','sp0',var,1,'d');
 no3=var;
%
 fname=[OUTDIR,'t05.dat'];
 var=load(fname)';
 var=wextend('addrow','sp0',var,1,'d');
 chla=var;
end
%
tstr=min(find(j>dstr))-1;
tend=min(find(j>dend))+1;
if isempty(tend), tend=length(j); end;
%
[z_r,z_w,Hz]=get_grid(N-1,theta_s,hmax);
z_r=wextend('addcol','sp0',z_r,1,'d');
z_r2d=repmat(z_r,NT,1)';
z_w2d=repmat(z_w,NT,1)';
[rho,bvf]=rho_eos(temp,salt,z_r2d,z_w2d,9.8,1025);
zmax=mean(zbl)-30;
%
%-----------------------------------------------------------------
% MAKE PLOTS
%-----------------------------------------------------------------
%
% seasonal cycle of T
%
figure(3)
zmax=-140.;
K=min(find(z>zmax))-1;
cmin=min(min(temp(K:end,:))); 
cmax=max(max(temp(K:end,:))); cint=0.25;
contourf(j(4:8:end),z,temp(:,4:8:end),[cmin:cint:cmax]);
colorbar
hold on
h=plot(j,mzbl,'k');
hold off
set(h,'LineWidth',2)
xlabel('Time [Days]')
ylabel('Depth [Meters]')
title('T [^{\circ} C]')
caxis([cmin cmax]);
axis([min(j) max(j) zmax 0]);
export_fig -pdf -transparent temp_sea.pdf

%
% Summer T
%
zmax=-80.;
K=min(find(z>zmax))-1;
j=j(tstr:tend);
z=z(K:end);
zbl=zbl(tstr:tend);
%
figure(4)
temp=temp(K:end,tstr:tend);
cmin=min(min(temp)); 
cmax=max(max(temp)); cint=(cmax-cmin)/20;
contourf(j,z,temp,[cmin:cint:cmax]);
colorbar
hold on
h=plot(j,zbl,'k');
hold off
set(h,'LineWidth',2)
xlabel('Time [Days]')
ylabel('Depth [Meters]')
title('T [^{\circ} C]')
caxis([cmin cmax]);
axis([dstr dend zmax 0]);
set(gcf, 'PaperPositionMode', 'auto');
export_fig -pdf -transparent temp.pdf

%
% Summer T anomalies
%
figure(5)
mtemp=mean(temp,2);
mtemp=repmat(mtemp,[1 (tend-tstr+1)]);
atemp=temp-mtemp;
cmin=min(min(atemp)); 
cmax=max(max(atemp)); cint=(cmax-cmin)/20;
contourf(j,z,atemp,[cmin:cint:cmax]);
colorbar
hold on
h=plot(j,zbl,'k');
hold off
set(h,'LineWidth',2)
xlabel('Time [Days]')
ylabel('Depth [Meters]')
title('T [^{\circ} C]')
caxis([cmin cmax]);
axis([dstr dend zmax 0]);
set(gcf, 'PaperPositionMode', 'auto');
export_fig -pdf -transparent temp_ano.pdf

return

%
% Summer S
%
figure(6)
salt=salt(K:end,tstr:tend);
cmin=min(min(salt)); 
cmax=max(max(salt)); cint=0.005;
contourf(j,z,salt,[cmin:cint:cmax]);
colorbar
hold on
plot(j,zbl,'k');
hold off
set(h,'LineWidth',2)
xlabel('Time [Days]')
ylabel('Depth [Meters]')
title('S [psu]')
caxis([cmin cmax]);
axis([dstr dend zmax 0]);
export_fig -pdf -transparent salt.pdf

%
% Summer rho
%
figure(7)
rho=rho(K:end,tstr:tend);
cmin=min(min(rho)); 
cmax=max(max(rho)); 
pcolor(j,z,rho); shading flat;
colorbar
hold on
h=plot(j,zbl,'k');
hold off
set(h,'LineWidth',2)
xlabel('Time [Days]')
ylabel('Depth [Meters]')
title('RHO [kg/m^3]')
caxis([cmin cmax]);
axis([dstr dend zmax 0]);

%
% Summer bvf
%
figure(8)
bvf=bvf(K:end,tstr:tend);
z_w=z_w(K:end);
cmin=min(min(bvf)); 
cmax=max(max(bvf));
%cmin=min(cmin,-cmax);cmax=max(cmax,-cmin);
pcolor(j,z_w,bvf); shading flat
colorbar
hold on
h=plot(j,zbl,'k');
hold off
set(h,'LineWidth',2)
xlabel('Time [Days]')
ylabel('Depth [Meters]')
title('BVF [1/s^2]')
caxis([cmin cmax]);
axis([dstr dend zmax 0]);
export_fig -pdf -transparent bvf.pdf

%
% Summer Akt
%
figure(9)
akt=akt(K:end,tstr:tend);
cmin=0; cmax=4; cint=1;
pcolor(j,z,log10(akt))
shading flat
colorbar
hold on
h=plot(j,zbl,'k');
hold off
set(h,'LineWidth',2)
xlabel('Time [Days]')
ylabel('Depth [Meters]')
title('LOG(AKt) [cm^2/s]')
caxis([cmin cmax]);
axis([dstr dend zmax 0]);
export_fig -pdf -transparent akt.pdf

if biology, %--------------------------------

%
% Summer NO3
%
figure(10)
var=log10(no3);
var=var(K:end,tstr:tend);
cmin=min(min(var)); 
cmax=max(max(var)); cint=0.05;
contourf(j,z,var,[cmin:cint:cmax]);
colorbar
hold on
h=plot(j,zbl,'k');
hold off
set(h,'LineWidth',2)
xlabel('Time [Days]')
ylabel('Depth [Meters]')
title('LOG(NO3) [mMol N m-3]')
caxis([cmin cmax]);
axis([dstr dend zmax 0]);
export_fig -pdf -transparent no3.pdf
%

% Summer Chla
%
figure(11)
var=log10(chla);
var=var(K:end,tstr:tend);
cmin=min(min(var)); 
cmax=max(max(var)); cint=0.01;
contourf(j,z,var,[cmin:cint:cmax]);
%pcolor(j,z,var); shading flat
colorbar
hold on
h=plot(j,zbl,'k');
hold off
set(h,'LineWidth',2)
xlabel('Time [Days]')
ylabel('Depth [Meters]')
title('LOG(CHLa) [mg Chl m-3]')
caxis([cmin cmax]);
axis([dstr dend zmax 0]);
export_fig -pdf -transparent chl.pdf

end % biology -----------------------------------
