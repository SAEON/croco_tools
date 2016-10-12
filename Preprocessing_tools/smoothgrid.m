function h = smoothgrid(h,maskr,hmin,hmax_coast,hmax,...
                        rmax,n_filter_deep_topo,n_filter_final)
%
%  Smooth the topography to get a maximum r factor = rmax
%
%  n_filter_deep_topo:
%  Number of pass of a selective filter to reduce the isolated
%  seamounts on the deep ocean.
%
%  n_filter_final:
%  Number of pass of a single hanning filter at the end of the
%  procedure to ensure that there is no 2DX noise in the 
%  topography.
%
%  Further Information:  
%  http://www.croco-ocean.org
%  
%  This file is part of CROCOTOOLS
%
%  CROCOTOOLS is free software; you can redistribute it and/or modify
%  it under the terms of the GNU General Public License as published
%  by the Free Software Foundation; either version 2 of the License,
%  or (at your option) any later version.
%
%  CROCOTOOLS is distributed in the hope that it will be useful, but
%  WITHOUT ANY WARRANTY; without even the implied warranty of
%  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%  GNU General Public License for more details.
%
%  You should have received a copy of the GNU General Public License
%  along with this program; if not, write to the Free Software
%  Foundation, Inc., 59 Temple Place, Suite 330, Boston,
%  MA  02111-1307  USA
%
%  Copyright (c) 2005-2006 by Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr  
%
%  Contributions of A. Shchepetkin (UCLA), P. Marchesiello (IRD)
%                   and X. Capet (UCLA)
%
%  Updated    Aug-2006 by Pierrick Penven
%  Updated    Dec-2013 by Patrick Marchesiello for wetting-drying
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%
% Cut topography and flood dry cells momentarily
%
h(h<hmin)=hmin;
h(h>hmax)=hmax;
%
%
% 1: Deep Ocean Filter
%
if n_filter_deep_topo>=1
  disp(' Apply a filter on the Deep Ocean to remove the isolated seamounts :')
  disp(['   ',num2str(n_filter_deep_topo),' pass of a selective filter.'])
%
%  Build a smoothing coefficient that is a linear function 
%  of a smooth topography.
%
  coef=h;
  for i=1:8
    coef=hanning_smoother(coef);             % coef is a smoothed bathy
  end 
  coef=0.125*(coef./max(max(coef)));         % rescale the smoothed bathy
%  
  for i=1:n_filter_deep_topo;
    h=hanning_smoother_coef2d(h,coef);       % smooth with avariable coef
    h(maskr==0 & h>hmax_coast)=hmax_coast;
  end
end
%
%  Apply a selective filter on log(h) to reduce grad(h)/h.
%
disp(' Apply a selective filter on log(h) to reduce grad(h)/h :')
if hmin<0, h=h-hmin+1; end
h=rotfilter(h,maskr,hmax_coast,rmax);
if hmin<0, h=h+hmin-1; end
%
%  Smooth the topography again to prevent 2D noise
%
if n_filter_final>1
  disp(' Smooth the topography a last time to prevent 2DX noise:')
  disp(['   ',num2str(n_filter_final),' pass of a hanning smoother.'])
  for i=1:n_filter_final
    h=hanning_smoother(h);
    h(maskr==0 & h>hmax_coast)=hmax_coast;
  end
end
%
h(h<hmin)=hmin;
%
return
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function h=rotfilter(h,maskr,hmax_coast,rmax)
%
% Apply a selective filter on log(h) to reduce grad(h)/h.
% 
[M,L]=size(h);
Mm=M-1;
Mmm=M-2;
Lm=L-1;
Lmm=L-2;
cff=0.8;
nu=3/16;
[rx,ry]=rfact(h);
r=max(max(max(rx)),max(max(ry)));
h=log(h);
hmax_coast=log(hmax_coast);
i=0;
while r>rmax
  i=i+1;
  cx=double(rx>cff*rmax);
  cx=hanning_smoother(cx);
  cy=double(ry>cff*rmax);
  cy=hanning_smoother(cy);
  fx=cx.*FX(h);
  fy=cy.*FY(h);
  h(2:Mm,2:Lm)=h(2:Mm,2:Lm)+nu*...
             ((fx(2:Mm,2:Lm)-fx(2:Mm,1:Lmm))+...
              (fy(2:Mm,2:Lm)-fy(1:Mmm,2:Lm)));
  h(1,:)=h(2,:);
  h(M,:)=h(Mm,:);
  h(:,1)=h(:,2);
  h(:,L)=h(:,Lm);
  h(maskr==0 & h>hmax_coast)=hmax_coast;
  [rx,ry]=rfact(exp(h));
  r=max(max(max(rx)),max(max(ry)));
end

disp(['   ',num2str(i),' iterations - rmax = ',num2str(r)]) 
h=exp(h);

return
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function [rx,ry]=rfact(h);
[M,L]=size(h);
Mm=M-1;
Mmm=M-2;
Lm=L-1;
Lmm=L-2;
rx=abs(h(1:M,2:L)-h(1:M,1:Lm))./(h(1:M,2:L)+h(1:M,1:Lm));
ry=abs(h(2:M,1:L)-h(1:Mm,1:L))./(h(2:M,1:L)+h(1:Mm,1:L));

return
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function h=hanning_smoother(h);
[M,L]=size(h);
Mm=M-1;
Mmm=M-2;
Lm=L-1;
Lmm=L-2;

h(2:Mm,2:Lm)=0.125*(h(1:Mmm,2:Lm)+h(3:M,2:Lm)+...
                       h(2:Mm,1:Lmm)+h(2:Mm,3:L)+...
                       4*h(2:Mm,2:Lm));

h(1,:)=h(2,:);
h(M,:)=h(Mm,:);
h(:,1)=h(:,2);
h(:,L)=h(:,Lm);

return
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function h=hanning_smoother_coef2d(h,coef);
[M,L]=size(h);
Mm=M-1;
Mmm=M-2;
Lm=L-1;
Lmm=L-2;
h(2:Mm,2:Lm)=coef(2:Mm,2:Lm).*(h(1:Mmm,2:Lm)+h(3:M,2:Lm)+...
                               h(2:Mm,1:Lmm)+h(2:Mm,3:L))...
            +(1-4.*coef(2:Mm,2:Lm)).*h(2:Mm,2:Lm);
h(1,:)=h(2,:);
h(M,:)=h(Mm,:);
h(:,1)=h(:,2);
h(:,L)=h(:,Lm);

return
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function fx=FX(h);
[M,L]=size(h);
Mm=M-1;
Mmm=M-2;
Lm=L-1;
Lmm=L-2;

fx(2:Mm,:)=(h(2:Mm,2:L)-h(2:Mm,1:Lm))*5/6 +...
   (h(1:Mmm,2:L)-h(1:Mmm,1:Lm)+h(3:M,2:L)-h(3:M,1:Lm))/12;
   
fx(1,:)=fx(2,:);
fx(M,:)=fx(Mm,:);

return
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
function fy=FY(h);
[M,L]=size(h);
Mm=M-1;
Mmm=M-2;
Lm=L-1;
Lmm=L-2;

fy(:,2:Lm)=(h(2:M,2:Lm)-h(1:Mm,2:Lm))*5/6 +...
           (h(2:M,1:Lmm)-h(1:Mm,1:Lmm)+h(2:M,3:L)-h(1:Mm,3:L))/12;
	   
fy(:,1)=fy(:,2);
fy(:,L)=fy(:,Lm);

return

