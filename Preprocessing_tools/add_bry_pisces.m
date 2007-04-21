function add_bry_pisces(bryname,obc,time,cycle,clobber);
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                                 %
% function add_bry_pisces(bryname,grdname,title,obc...            %
%                         theta_s,theta_b,hc,N,...                %
%                         time,cycle,clobber);                    %
%                                                                 %
%   This function create the header of a Netcdf climatology       %
%   file.                                                         %
%                                                                 %
%   Input:                                                        %
%                                                                 %
%   bryname      Netcdf climatology file name (character string). %
%   obc          open boundaries flag (1=open , [S E N W]).       %
%   time         time.(vector)                                    %
%   cycle        Length (days) for cycling the climatology.(Real) %
%   clobber      Switch to allow or not writing over an existing  %
%                file.(character string)                          %
%                                                                 %
%  Pierrick Penven, IRD, 2005.                                    %
%  Olivier Aumont the master, IRD, 2006.                          %
%  Patricio Marchesiello, chief, IRD, 2007.                       %
%  Christophe Eugene Raoul Menkes, the slave, IRD, 2007.          %
%                                                                 %
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
%  Data climatologies file names:
%
no3_month_data  = '../WOAPISCES/no3_seas.cdf';
no3_ann_data    = '../WOAPISCES/no3_ann.cdf';
po4_month_data  = '../WOAPISCES/po4_seas.cdf';
po4_ann_data    = '../WOAPISCES/po4_ann.cdf';
o2_month_data   = '../WOAPISCES/o2_seas.cdf';
o2_ann_data     = '../WOAPISCES/o2_ann.cdf';
sio3_month_data = '../WOAPISCES/sio3_seas.cdf';
sio3_ann_data   = '../WOAPISCES/sio3_ann.cdf';
dic_month_data  = '../WOAPISCES/dic_seas.cdf';
dic_ann_data    = '../WOAPISCES/dic_ann.cdf';
talk_month_data = '../WOAPISCES/talk_seas.cdf';
talk_ann_data   = '../WOAPISCES/talk_ann.cdf';
doc_month_data  = '../WOAPISCES/doc_seas.cdf';
doc_ann_data    = '../WOAPISCES/doc_ann.cdf';
fer_month_data  = '../WOAPISCES/fer_seas.cdf';
fer_ann_data    = '../WOAPISCES/fer_ann.cdf';
dust_month_data = '../WOAPISCES/dust_seas.cdf';
dust_ann_data   = '../WOAPISCES/dust_ann.cdf';

disp(' ')
disp([' Adding PISCES data into file : ',bryname])
disp(' ')
%
%  Redefine the boundary file
%
nc = netcdf(bryname,clobber);
result = redef(nc);
%
%  Create dimensions
%
nc('DIC_time') = length(time);
nc('DOC_time') = length(time);
nc('NO3_time') = length(time);
nc('PO4_time') = length(time);
nc('TALK_time') = length(time);
nc('Si_time') = length(time);
nc('FER_time') = length(time);
nc('O2_time') = length(time);
nc('one') = 1;
%
%  Create variables and attributes
%
nc{'DIC_time'} = ncdouble('DIC_time') ;
nc{'DIC_time'}.long_name = ncchar('time for DIC climatology');
nc{'DIC_time'}.long_name = 'time for DIC climatology';
nc{'DIC_time'}.units = ncchar('day');
nc{'DIC_time'}.units = 'day';
nc{'DIC_time'}.cycle_length = cycle;%
%
nc{'TALK_time'} = ncdouble('TALK_time') ;
nc{'TALK_time'}.long_name = ncchar('time for TALK climatology');
nc{'TALK_time'}.long_name = 'time for TALK climatology';
nc{'TALK_time'}.units = ncchar('day');
nc{'TALK_time'}.units = 'day';
nc{'TALK_time'}.cycle_length = cycle;%
%
nc{'NO3_time'} = ncdouble('NO3_time') ;
nc{'NO3_time'}.long_name = ncchar('time for NO3 climatology')
nc{'NO3_time'}.long_name = 'time for NO3 climatology';
nc{'NO3_time'}.units = ncchar('day');
nc{'NO3_time'}.units = 'day';
nc{'NO3_time'}.cycle_length = cycle;%
%
nc{'PO4_time'} = ncdouble('PO4_time') ;
nc{'PO4_time'}.long_name = ncchar('time for PO4 climatology');
nc{'PO4_time'}.long_name = 'time for PO4 climatology';
nc{'PO4_time'}.units = ncchar('day');
nc{'PO4_time'}.units = 'day';
nc{'PO4_time'}.cycle_length = cycle;%
%
nc{'Si_time'} = ncdouble('Si_time') ;
nc{'Si_time'}.long_name = ncchar('time for Si climatology');
nc{'Si_time'}.long_name = 'time for Si climatology';
nc{'Si_time'}.units = ncchar('day');
nc{'Si_time'}.units = 'day';
nc{'Si_time'}.cycle_length = cycle;%
%
nc{'O2_time'} = ncdouble('O2_time') ;
nc{'O2_time'}.long_name = ncchar('time for O2 climatology');
nc{'O2_time'}.long_name = 'time for O2 climatology';
nc{'O2_time'}.units = ncchar('day');
nc{'O2_time'}.units = 'day';
nc{'O2_time'}.cycle_length = cycle;%
%
nc{'DOC_time'} = ncdouble('DOC_time') ;
nc{'DOC_time'}.long_name = ncchar('time for DOC climatology');
nc{'DOC_time'}.long_name = 'time for DOC climatology';
nc{'DOC_time'}.units = ncchar('day');
nc{'DOC_time'}.units = 'day';
nc{'DOC_time'}.cycle_length = cycle;%
%
nc{'FER_time'} = ncdouble('FER_time') ;
nc{'FER_time'}.long_name = ncchar('time for FER climatology');
nc{'FER_time'}.long_name = 'time for FER climatology';
nc{'FER_time'}.units = ncchar('day');
nc{'FER_time'}.units = 'day';
nc{'FER_time'}.cycle_length = cycle;%
%
if obc(1)==1
%
%   Southern boundary
%
  nc{'NO3_south'} = ncdouble('NO3_time','s_rho','xi_rho') ;
  nc{'NO3_south'}.long_name = ncchar('southern boundary NO3');
  nc{'NO3_south'}.long_name = 'southern boundary NO3';
  nc{'NO3_south'}.units = ncchar('mMol N m-3');
  nc{'NO3_south'}.units = 'mMol N m-3';
%
  nc{'PO4_south'} = ncdouble('PO4_time','s_rho','xi_rho') ;
  nc{'PO4_south'}.long_name = ncchar('southern boundary PO4');
  nc{'PO4_south'}.long_name = 'southern boundary PO4';
  nc{'PO4_south'}.units = ncchar('mMol N m-3');
  nc{'PO4_south'}.units = 'mMol N m-3';
%
  nc{'Si_south'} = ncdouble('Si_time','s_rho','xi_rho') ;
  nc{'Si_south'}.long_name = ncchar('southern boundary Si');
  nc{'Si_south'}.long_name = 'southern boundary Si';
  nc{'Si_south'}.units = ncchar('mMol N m-3');
  nc{'Si_south'}.units = 'mMol N m-3';
%
  nc{'O2_south'} = ncdouble('O2_time','s_rho','xi_rho') ;
  nc{'O2_south'}.long_name = ncchar('southern boundary O2');
  nc{'O2_south'}.long_name = 'southern boundary O2';
  nc{'O2_south'}.units = ncchar('mMol N m-3');
  nc{'O2_south'}.units = 'mMol N m-3';
%
  nc{'DIC_south'} = ncdouble('DIC_time','s_rho','xi_rho') ;
  nc{'DIC_south'}.long_name = ncchar('southern boundary DIC');
  nc{'DIC_south'}.long_name = 'southern boundary DIC';
  nc{'DIC_south'}.units = ncchar('mMol N m-3');
  nc{'DIC_south'}.units = 'mMol N m-3';
%
  nc{'TALK_south'} = ncdouble('TALK_time','s_rho','xi_rho') ;
  nc{'TALK_south'}.long_name = ncchar('southern boundary TALK');
  nc{'TALK_south'}.long_name = 'southern boundary TALK';
  nc{'TALK_south'}.units = ncchar('mMol N m-3');
  nc{'TALK_south'}.units = 'mMol N m-3';
%
  nc{'DOC_south'} = ncdouble('DOC_time','s_rho','xi_rho') ;
  nc{'DOC_south'}.long_name = ncchar('southern boundary DOC');
  nc{'DOC_south'}.long_name = 'southern boundary DOC';
  nc{'DOC_south'}.units = ncchar('mMol N m-3');
  nc{'DOC_south'}.units = 'mMol N m-3';
%
  nc{'FER_south'} = ncdouble('FER_time','s_rho','xi_rho') ;
  nc{'FER_south'}.long_name = ncchar('southern boundary Iron');
  nc{'FER_south'}.long_name = 'southern boundary Iron';
  nc{'FER_south'}.units = ncchar('mMol N m-3');
  nc{'FER_south'}.units = 'mMol N m-3';
%
end
%
if obc(2)==1
%
%   Eastern boundary
%
  nc{'NO3_east'} = ncdouble('NO3_time','s_rho','eta_rho') ;
  nc{'NO3_east'}.long_name = ncchar('eastern boundary NO3');
  nc{'NO3_east'}.long_name = 'eastern boundary NO3';
  nc{'NO3_east'}.units = ncchar('mMol N m-3');
  nc{'NO3_east'}.units = 'mMol N m-3';
%
  nc{'PO4_east'} = ncdouble('PO4_time','s_rho','eta_rho') ;
  nc{'PO4_east'}.long_name = ncchar('eastern boundary PO4');
  nc{'PO4_east'}.long_name = 'eastern boundary PO4';
  nc{'PO4_east'}.units = ncchar('mMol N m-3');
  nc{'PO4_east'}.units = 'mMol N m-3';
%
  nc{'Si_east'} = ncdouble('Si_time','s_rho','eta_rho') ;
  nc{'Si_east'}.long_name = ncchar('eastern boundary Si');
  nc{'Si_east'}.long_name = 'eastern boundary Si';
  nc{'Si_east'}.units = ncchar('mMol N m-3');
  nc{'Si_east'}.units = 'mMol N m-3';
%
  nc{'O2_east'} = ncdouble('O2_time','s_rho','eta_rho') ;
  nc{'O2_east'}.long_name = ncchar('eastern boundary O2');
  nc{'O2_east'}.long_name = 'eastern boundary O2';
  nc{'O2_east'}.units = ncchar('mMol N m-3');
  nc{'O2_east'}.units = 'mMol N m-3';
%
  nc{'DIC_east'} = ncdouble('DIC_time','s_rho','eta_rho') ;
  nc{'DIC_east'}.long_name = ncchar('eastern boundary DIC');
  nc{'DIC_east'}.long_name = 'eastern boundary DIC';
  nc{'DIC_east'}.units = ncchar('mMol N m-3');
  nc{'DIC_east'}.units = 'mMol N m-3';
%
  nc{'TALK_east'} = ncdouble('TALK_time','s_rho','eta_rho') ;
  nc{'TALK_east'}.long_name = ncchar('eastern boundary TALK');
  nc{'TALK_east'}.long_name = 'eastern boundary TALK';
  nc{'TALK_east'}.units = ncchar('mMol N m-3');
  nc{'TALK_east'}.units = 'mMol N m-3';
%
  nc{'DOC_east'} = ncdouble('DOC_time','s_rho','eta_rho') ;
  nc{'DOC_east'}.long_name = ncchar('eastern boundary DOC');
  nc{'DOC_east'}.long_name = 'eastern boundary DOC';
  nc{'DOC_east'}.units = ncchar('mMol N m-3');
  nc{'DOC_east'}.units = 'mMol N m-3';
%
  nc{'FER_east'} = ncdouble('FER_time','s_rho','eta_rho') ;
  nc{'FER_east'}.long_name = ncchar('eastern boundary Iron');
  nc{'FER_east'}.long_name = 'eastern boundary Iron';
  nc{'FER_east'}.units = ncchar('mMol N m-3');
  nc{'FER_east'}.units = 'mMol N m-3';
%
end
%
if obc(3)==1
%
%   Northern boundary
%
  nc{'NO3_north'} = ncdouble('NO3_time','s_rho','xi_rho') ;
  nc{'NO3_north'}.long_name = ncchar('northern boundary NO3');
  nc{'NO3_north'}.long_name = 'northern boundary NO3';
  nc{'NO3_north'}.units = ncchar('mMol N m-3');
  nc{'NO3_north'}.units = 'mMol N m-3';
%
  nc{'PO4_north'} = ncdouble('PO4_time','s_rho','xi_rho') ;
  nc{'PO4_north'}.long_name = ncchar('northern boundary PO4');
  nc{'PO4_north'}.long_name = 'northern boundary PO4';
  nc{'PO4_north'}.units = ncchar('mMol N m-3');
  nc{'PO4_north'}.units = 'mMol N m-3';
%
  nc{'Si_north'} = ncdouble('Si_time','s_rho','xi_rho') ;
  nc{'Si_north'}.long_name = ncchar('northern boundary Si');
  nc{'Si_north'}.long_name = 'northern boundary Si';
  nc{'Si_north'}.units = ncchar('mMol N m-3');
  nc{'Si_north'}.units = 'mMol N m-3';
%
  nc{'O2_north'} = ncdouble('O2_time','s_rho','xi_rho') ;
  nc{'O2_north'}.long_name = ncchar('northern boundary O2');
  nc{'O2_north'}.long_name = 'northern boundary O2';
  nc{'O2_north'}.units = ncchar('mMol N m-3');
  nc{'O2_north'}.units = 'mMol N m-3';
%
  nc{'DIC_north'} = ncdouble('DIC_time','s_rho','xi_rho') ;
  nc{'DIC_north'}.long_name = ncchar('northern boundary DIC');
  nc{'DIC_north'}.long_name = 'northern boundary DIC';
  nc{'DIC_north'}.units = ncchar('mMol N m-3');
  nc{'DIC_north'}.units = 'mMol N m-3';
%
  nc{'TALK_north'} = ncdouble('TALK_time','s_rho','xi_rho') ;
  nc{'TALK_north'}.long_name = ncchar('northern boundary TALK');
  nc{'TALK_north'}.long_name = 'northern boundary TALK';
  nc{'TALK_north'}.units = ncchar('mMol N m-3');
  nc{'TALK_north'}.units = 'mMol N m-3';
%
  nc{'DOC_north'} = ncdouble('DOC_time','s_rho','xi_rho') ;
  nc{'DOC_north'}.long_name = ncchar('northern boundary DOC');
  nc{'DOC_north'}.long_name = 'northern boundary DOC';
  nc{'DOC_north'}.units = ncchar('mMol N m-3');
  nc{'DOC_north'}.units = 'mMol N m-3';
%
  nc{'FER_north'} = ncdouble('FER_time','s_rho','xi_rho') ;
  nc{'FER_north'}.long_name = ncchar('northern boundary Iron');
  nc{'FER_north'}.long_name = 'northern boundary Iron';
  nc{'FER_north'}.units = ncchar('mMol N m-3');
  nc{'FER_north'}.units = 'mMol N m-3';
%
end
%
if obc(4)==1
%
%   Western boundary
%
  nc{'NO3_west'} = ncdouble('NO3_time','s_rho','eta_rho') ;
  nc{'NO3_west'}.long_name = ncchar('western boundary NO3');
  nc{'NO3_west'}.long_name = 'western boundary NO3';
  nc{'NO3_west'}.units = ncchar('mMol N m-3');
  nc{'NO3_west'}.units = 'mMol N m-3';
%
  nc{'PO4_west'} = ncdouble('PO4_time','s_rho','eta_rho') ;
  nc{'PO4_west'}.long_name = ncchar('western boundary PO4');
  nc{'PO4_west'}.long_name = 'western boundary PO4';
  nc{'PO4_west'}.units = ncchar('mMol N m-3');
  nc{'PO4_west'}.units = 'mMol N m-3';
%
  nc{'Si_west'} = ncdouble('Si_time','s_rho','eta_rho') ;
  nc{'Si_west'}.long_name = ncchar('western boundary Si');
  nc{'Si_west'}.long_name = 'western boundary Si';
  nc{'Si_west'}.units = ncchar('mMol N m-3');
  nc{'Si_west'}.units = 'mMol N m-3';
%
  nc{'O2_west'} = ncdouble('O2_time','s_rho','eta_rho') ;
  nc{'O2_west'}.long_name = ncchar('western boundary O2');
  nc{'O2_west'}.long_name = 'western boundary O2';
  nc{'O2_west'}.units = ncchar('mMol N m-3');
  nc{'O2_west'}.units = 'mMol N m-3';
%
  nc{'DIC_west'} = ncdouble('DIC_time','s_rho','eta_rho') ;
  nc{'DIC_west'}.long_name = ncchar('western boundary DIC');
  nc{'DIC_west'}.long_name = 'western boundary DIC';
  nc{'DIC_west'}.units = ncchar('mMol N m-3');
  nc{'DIC_west'}.units = 'mMol N m-3';
%
  nc{'TALK_west'} = ncdouble('TALK_time','s_rho','eta_rho') ;
  nc{'TALK_west'}.long_name = ncchar('western boundary TALK');
  nc{'TALK_west'}.long_name = 'western boundary TALK';
  nc{'TALK_west'}.units = ncchar('mMol N m-3');
  nc{'TALK_west'}.units = 'mMol N m-3';
%
  nc{'DOC_west'} = ncdouble('DOC_time','s_rho','eta_rho') ;
  nc{'DOC_west'}.long_name = ncchar('western boundary DOC');
  nc{'DOC_west'}.long_name = 'western boundary DOC';
  nc{'DOC_west'}.units = ncchar('mMol N m-3');
  nc{'DOC_west'}.units = 'mMol N m-3';
%
  nc{'FER_west'} = ncdouble('FER_time','s_rho','eta_rho') ;
  nc{'FER_west'}.long_name = ncchar('western boundary Iron');
  nc{'FER_west'}.long_name = 'western boundary Iron';
  nc{'FER_west'}.units = ncchar('mMol N m-3');
  nc{'FER_west'}.units = 'mMol N m-3';
%
end
%
% Leave define mode
%
result = endef(nc);
%
% Write variables
%
nc{'DIC_time'}(:) = time;
nc{'DOC_time'}(:) = time;
nc{'NO3_time'}(:) = time;
nc{'PO4_time'}(:) = time;
nc{'TALK_time'}(:) = time;
nc{'Si_time'}(:) = time;
nc{'FER_time'}(:) = time;
nc{'O2_time'}(:) = time;
if obc(1)==1
  nc{'NO3_south'}(:)  =  0;
  nc{'PO4_south'}(:)  =  0;
  nc{'Si_south'}(:) =  0;
  nc{'O2_south'}(:)   =  0;
  nc{'DIC_south'}(:)  =  0;
  nc{'TALK_south'}(:) =  0;
  nc{'DOC_south'}(:)  =  0;
  nc{'FER_south'}(:)  =  0;
end 
if obc(2)==1
  nc{'NO3_east'}(:)  =  0;
  nc{'PO4_east'}(:)  =  0;
  nc{'Si_east'}(:) =  0;
  nc{'O2_east'}(:)   =  0;
  nc{'DIC_east'}(:)  =  0;
  nc{'TALK_east'}(:) =  0;
  nc{'DOC_east'}(:)  =  0;
  nc{'FER_east'}(:)  =  0;
end 
if obc(3)==1
  nc{'NO3_north'}(:)  =  0;
  nc{'PO4_north'}(:)  =  0;
  nc{'Si_north'}(:) =  0;
  nc{'O2_north'}(:)   =  0;
  nc{'DIC_north'}(:)  =  0;
  nc{'TALK_north'}(:) =  0;
  nc{'DOC_north'}(:)  =  0;
  nc{'FER_north'}(:)  =  0;
end 
if obc(4)==1
  nc{'NO3_west'}(:)  =  0;
  nc{'PO4_west'}(:)  =  0;
  nc{'Si_west'}(:) =  0;
  nc{'O2_west'}(:)   =  0;
  nc{'DIC_west'}(:)  =  0;
  nc{'TALK_west'}(:) =  0;
  nc{'DOC_west'}(:)  =  0;
  nc{'FER_west'}(:)  =  0;
end 
close(nc)
return


