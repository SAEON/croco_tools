function extract_NCEP(NCEP_dir,url,fname,vname,year,month,...
                      lon,lat,time,...
                      trange,level,jrange,...
                      i1min,i1max,i2min,i2max,i3min,i3max,...
                      Yorig)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Extract a subset from NCEP using OPENDAP
% Write it in a local file
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
%  Copyright (c) 2005-2006 by Pierrick Penven 
%  e-mail:Pierrick.Penven@ird.fr  
%
%  Updated    6-Sep-2006 by Pierrick Penven
%  Updated    22-Sep-2006 by Pierrick Penven (readattribute)
%  Updated    Jul-2007 to use Nomads3 server, J. Lefevre.  
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Get the variable name
%
disp(['Get ',vname,' for year ',num2str(year),...
      ' - month ',num2str(month)])
%
% Get the variable 2D subset (take care of greenwitch)
%
var=getdap(url,fname,vname,...
            trange,level,jrange,...
            i1min,i1max,i2min,i2max,i3min,i3max);	    
%
% Get the dataset attributes
%
x=readattribute([url,fname]);
%eval(['add_offset=x.',vname,'.add_offset;']);
%eval(['scale_factor=x.',vname,'.scale_factor;']);  %no scale scale factor and offset ??
eval(['missing_value=x.',vname,'.missing_value;']);

%
% Correct the variable
%
var=shiftdim(var,2);
var(var==missing_value)=NaN;
%
% Write it in a file
%
write_NCEP([NCEP_dir,vname,'_Y',num2str(year),'M',num2str(month),'.nc'],...
            vname,lon,lat,time,var,Yorig)
%
return

