function z = zlevs(h,zeta,theta_s,theta_b,hc,N,type);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  function z = zlevs(h,zeta,theta_s,theta_b,hc,N,type);
%
%  this function compute the depth of rho or w points for ROMS
%
%  On Input:
%
%    type    'r': rho point 'w': w point 
%
%  On Output:
%
%    z       Depths (m) of RHO- or W-points (3D matrix).
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
%  Copyright (c) 2002-2006 by Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr  
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[M,L]=size(h);
%
% Set S-Curves in domain [-1 < sc < 0] at vertical W- and RHO-points.
%
cff1=1./sinh(theta_s);
cff2=0.5/tanh(0.5*theta_s);
if type=='w'
  sc=((0:N)-N)/N;
  N=N+1;
else
  sc=((1:N)-N-0.5)/N;
end
Cs=(1.-theta_b)*cff1*sinh(theta_s*sc)...
    +theta_b*(cff2*tanh(theta_s*(sc+0.5))-0.5);
%
% Create S-coordinate system: based on model topography h(i,j),
% fast-time-averaged free-surface field and vertical coordinate
% transformation metrics compute evolving depths of of the three-
% dimensional model grid.
%    
hinv=1./h;
cff=hc*(sc-Cs);
cff1=Cs;
cff2=sc+1;
z=zeros(N,M,L);
for k=1:N
  z0=cff(k)+cff1(k)*h;
  z(k,:,:)=z0+zeta.*(1.+z0.*hinv);
end

%if type=='w'
%  hmin=min(min(h));
%  hmax=max(max(h));
%  for k=N:-1:1
%    cff1=sc(k)*hc+(hmin-hc)*Cs(k);
%    cff2=sc(k)*hc+(0.5*(hmin+hmax)-hc)*Cs(k);
%    cff3=sc(k)*hc+(hmax-hc)*Cs(k);
%    disp([num2str(k,6),' | ',num2str(sc(k),6),' | ',num2str(Cs(k)),' | ',...
%         num2str(cff1),' | ',num2str(cff2),' | ',num2str(cff3)])
%  end
%end

return

