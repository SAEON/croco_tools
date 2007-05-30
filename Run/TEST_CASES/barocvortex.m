%
% Compute zeta,ubar,vbar,u,v,t for the vortex
%
%
% Pierrick Penven, IRD, 2005
%

%
%
% P1 : pressure at z=0
%
% P1=P0*exp(-r2/lambda2)
%
% P0 such as u(r0)=umax
%
% => P0=rho0 f0 umax lambda sqrt(e/2)
%
P0=rho0*f0*umax*lambda*sqrt(exp(1)/2);
P1=Pa+P0*exp(-(X.^2+Y.^2)/lambda^2);
%
% Calcul de rho at z=0
%
a=-P0*(1-exp(-H))/(g*(H-1+exp(-H)));
rho1=rho0+a*exp(-(X.^2+Y.^2)/lambda^2);
%
% Surface elevation 
%
zeta=(P1-Pa)./(g.*rho1);
%
% Vertical grid 
%
zw=zlevs(h0,zeta,theta_s,theta_b,hc,N,'w');
zr=zlevs(h0,zeta,theta_s,theta_b,hc,N,'r');
%
% Density
%
%
xr=tridim(X,N);
yr=tridim(Y,N);
%
rho=rho0.*(1-N2.*zr./g);
rhodyn=-P0.*(1-exp(-zr-H)).*exp(-(xr.^2+yr.^2)/lambda^2)./(g*(H-1+exp(-H)));
rho(zr>-H)=rho(zr>-H)+rhodyn(zr>-H);
%
% Temperature
%
% rho=rho0+R0-TCOEF*T
%
R0=30;
TCOEF=0.28;
t=(-rho+1000+R0)/TCOEF;
%
% U and V
%
%
a=2*P0/(f0*rho0*lambda^2);
zu=0.5*(zr(:,:,1:end-1)+zr(:,:,2:end));
xu=0.5*(xr(:,:,1:end-1)+xr(:,:,2:end));
yu=0.5*(yr(:,:,1:end-1)+yr(:,:,2:end));
F=(H-1+zu+exp(-zu-H))./(H-1+exp(-H));
F(zu<-H)=0;
u=a.*F.*yu.*exp(-(xu.^2+yu.^2)/lambda^2);
zv=0.5*(zr(:,1:end-1,:)+zr(:,2:end,:));
xv=0.5*(xr(:,1:end-1,:)+xr(:,2:end,:));
yv=0.5*(yr(:,1:end-1,:)+yr(:,2:end,:));
F=(H-1+zv+exp(-zv-H))./(H-1+exp(-H));
F(zv<-H)=0;
v=-a.*F.*xv.*exp(-(xv.^2+yv.^2)/lambda^2);
%
% Vitesses barotropes
%
dz=zw(2:end,:,:)-zw(1:end-1,:,:);
dzu=0.5*(dz(:,:,1:end-1)+dz(:,:,2:end));
dzv=0.5*(dz(:,1:end-1,:)+dz(:,2:end,:));
hu=squeeze(sum(dzu.*u));
hv=squeeze(sum(dzv.*v));
D_u=squeeze(sum(dzu));
D_v=squeeze(sum(dzv));
ubar=squeeze(hu./D_u);
vbar=squeeze(hv./D_v);
%
return
