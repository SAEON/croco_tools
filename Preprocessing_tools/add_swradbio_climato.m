clear all
close all

disp(['Add swradbio: daily averaged solar short wave radiation     '])
disp(['Creating variables and attributes for forcing and bulk ' ...
      'files'])

romstools_param
%
% Open the frc file
% 
if (makefrc)
disp(['Add swradbio: daily averaged solar short wave radiation'])
disp(['No change needed in this climato case                  '])
disp(['Creating variables and attributes for frc files        '])
end
add_swradbio_frc(frcname);
%
nw=netcdf(frcname,'write');
swrad1=nw{'swrad'}(:);
nw{'swradbio'}(:) = swrad1;
close(nw)
%
% Open the bulk time
%
if (makeblk)
disp(['Add radswbio: daily averaged solar short wave radiation'])
disp(['No change needed in this climato case                  '])
disp(['Creating variables and attributes for bulk files       '])
end
add_swradbio_blk(blkname);
%
nw=netcdf(blkname,'write');
radsw1=nw{'radsw'}(:);
nw{'radswbio'}(:) = radsw1;
close(nw)
%