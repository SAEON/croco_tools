function z = zlevs(h,zeta,theta_s,theta_b,hc,N,type,vtransform,vstretching)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  function z = zlevs(h,zeta,theta_s,theta_b,hc,N,type,vtransform,vstretching)
%
%  this function compute the depth of rho or w points for CROCO/ROMS
%
%  On Input:
%
%    type         'r': rho point 'w': w point
%    vtransform   1=> old v transform (Song, 1994);
%                 2=> new v transform (Shchepetkin, 2006)
%    vstretching  Vertical stretching function (optional, integer 1-5):
%                   1 => Song and Haidvogel (1994)
%                   2 => A. Shchepetkin (UCLA-ROMS, 2005)
%                   3 => R. Geyer BBL refinement
%                   4 => A. Shchepetkin (UCLA-ROMS, 2010)
%                   5 => Souza et al. (2015) quadratic Legendre polynomial
%                 If omitted: defaults to 1 for vtransform=1, 4 for vtransform=2.
%                 See: https://www.myroms.org/wiki/Vertical_S-coordinate
%
%  On Output:
%
%    z       Depths (m) of RHO- or W-points (3D matrix).
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
%  Copyright (c) 2002-2006 by Pierrick Penven
%  e-mail:Pierrick.Penven@ird.fr
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
[M,L]=size(h);
%
% Handle vtransform default
%
if nargin < 8
    disp(['WARNING no vtransform defined'])
    vtransform = 1;
    disp(['Default S-coordinate system use : Vtransform=1 (old one)'])
end
%
% Handle vstretching default
%
if nargin < 9
    if vtransform == 1
        vstretching = 1;
    else
        vstretching = 4;
        warning('zlevs:defaultVstretching', ...
            ['Vstretching not specified for Vtransform=2, defaulting to 4 ', ...
             '(Shchepetkin 2010). If your ROMS/CROCO file uses a different ', ...
             'Vstretching (e.g. 5), pass it as the 9th argument.']);
    end
end
%
% Step 1: Compute S-coordinates (sc) and stretching function (Cs)
%
% Compute fractional sigma coordinate
%
if vstretching == 5
    % Quadratic Legendre polynomial (Souza et al., 2015)
    if type=='w'
        lev = 0:N;
        sc = -(lev.*lev - 2.0.*lev.*N + lev + N*N - N) ./ (N*N - N) - ...
              0.01.*(lev.*lev - lev.*N) ./ (1.0 - N);
        sc(1) = -1.0;
        N = N + 1;
    else
        lev = (1:N) - 0.5;
        sc = -(lev.*lev - 2.0.*lev.*N + lev + N*N - N) ./ (N*N - N) - ...
              0.01.*(lev.*lev - lev.*N) ./ (1.0 - N);
    end
else
    % Standard linear sigma for Vstretching 1-4
    if type=='w'
        sc = ((0:N) - N) ./ N;
        N = N + 1;
    else
        sc = ((1:N) - N - 0.5) ./ N;
    end
end
%
% Compute stretching function C(sigma)
%
if vstretching == 1
    % Song and Haidvogel (1994)
    if theta_s > 0
        Cs = (1.0-theta_b) .* sinh(theta_s.*sc) ./ sinh(theta_s) + ...
             theta_b .* (tanh(theta_s.*(sc+0.5)) ./ (2.0*tanh(0.5*theta_s)) - 0.5);
    else
        Cs = sc;
    end
elseif vstretching == 2
    % A. Shchepetkin (UCLA-ROMS, 2005)
    alfa = 1.0;
    beta = 1.0;
    if theta_s > 0
        Csur = (1.0-cosh(theta_s.*sc)) ./ (cosh(theta_s)-1.0);
        if theta_b > 0
            Cbot = -1.0 + sinh(theta_b.*(sc+1.0)) ./ sinh(theta_b);
            weight = (sc+1.0).^alfa .* (1.0+(alfa/beta).*(1.0-(sc+1.0).^beta));
            Cs = weight.*Csur + (1.0-weight).*Cbot;
        else
            Cs = Csur;
        end
    else
        Cs = sc;
    end
elseif vstretching == 3
    % R. Geyer BBL refinement
    if theta_s > 0
        exp_s = theta_s;
        exp_b = theta_b;
        alpha = 3;
        Cbot = log(cosh(alpha.*(sc+1).^exp_b)) ./ log(cosh(alpha)) - 1;
        Csur = -log(cosh(alpha.*abs(sc).^exp_s)) ./ log(cosh(alpha));
        weight = (1 - tanh(alpha.*(sc+0.5))) ./ 2;
        Cs = weight.*Cbot + (1-weight).*Csur;
    else
        Cs = sc;
    end
elseif vstretching == 4 || vstretching == 5
    % A. Shchepetkin (UCLA-ROMS, 2010) / Souza et al. (2015)
    % Same C(sigma) formula; Vstretching=5 differs only in sigma above
    if theta_s > 0
        Cs = (1.0-cosh(theta_s.*sc)) ./ (cosh(theta_s)-1.0);
    else
        Cs = -sc.^2;
    end
    if theta_b > 0
        Cs = (exp(theta_b.*Cs)-1.0) ./ (1.0-exp(-theta_b));
    end
else
    error('zlevs:badVstretching', ...
        'Vstretching=%d is not supported. Use 1-5.', vstretching);
end
%
% Step 2: Compute depths using Vtransform
%
% Adjust for dry cells
%
h(h==0) = 1.e-2;
Dcrit = 0.01;
zeta(zeta<(Dcrit-h)) = Dcrit - h(zeta<(Dcrit-h));
%
z = zeros(N,M,L);
if vtransform == 1
    hinv = 1./h;
    cff = hc*(sc-Cs);
    cff1 = Cs;
    for k = 1:N
        z0 = cff(k) + cff1(k)*h;
        z(k,:,:) = z0 + zeta.*(1.0+z0.*hinv);
    end
elseif vtransform == 2
    h2 = h + hc;
    h2inv = 1./h2;
    cff = hc*sc;
    for k = 1:N
        z0 = cff(k) + Cs(k)*h;
        z(k,:,:) = z0.*h./h2 + zeta.*(1.0+z0.*h2inv);
    end
else
    error('zlevs:badVtransform', ...
        'Vtransform=%d is not supported. Use 1 or 2.', vtransform);
end

return
