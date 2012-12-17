%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Check vertical grid size of ROMS-1D
%
% Patrick Marchesiello - June 2012
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
clear all
close all

N=100;      % number of model levels
theta_s=2;  % surface stretching parameter
hmax=200;   % model depth

ods=N;ds=1/ods;
cff=hmax/sinh(theta_s);
for k=N:-1:1
  sc_w=ds*(k-N);
  z_w(k+1)=-cff*sinh(-theta_s*sc_w);
  sc_r=ds*(k-N-0.5);
  z_r(k)=-cff*sinh(-theta_s*sc_r);
end
z_w(1)=-hmax;
for k=1:N
  Hz(k)=z_w(k+1)-z_w(k);
end

figure
plot(Hz,z_r,'*');
title('Vertical grid size dz (m)')
xlabel('dz (m)')
ylabel('depth (m)')

