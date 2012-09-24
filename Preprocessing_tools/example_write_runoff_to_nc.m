clear all
close all

% In this programm the monthly data for river runoff are written into a
% netcdf file

% data from the Mekong River Commission (2005): mean monthly value btw 1960
% and 2004, m3/s
debit_at_kratie=[3620  2730 2290 2220 3640 11200 22200 35500 36700 22000 10900 5710]
debit_mekong=100/91*debit_at_kratie

% data from Annales de Géographie, Pardé, 1938: average over 1912-1935.
% mean : 3917m3/s
debit_redriv=[1229 1125 1052 1089 1735 4802 8557 11243 6573 5065 2875 1657]

% data from wikipedia for Xi at wuzhou
debit_wuzhou= [1761 1743 2216 4527 9384 14211 14835 14410 8853 5346 3687 2605]
debit_wuzhou*9500/mean(debit_wuzhou)

riv_name= ['R1';'R2';'R3'];
A=[debit_mekong;debit_redriv;debit_wuzhou]

figure
plot(A')
legend(riv_name)

nw=netcdf('roms_runoff_monthly.nc','clobber');
% Dimension 
nw('riv_time') = 12;
nw('n_riv') = 3;
nw('RivnameStrLen') = 2;

% Variable and Attribute
nw{'riv_time'} = ncdouble('riv_time');
nw{'riv_time'}.long_name = ncchar('river runoff time');
nw{'riv_time'}.units = ncchar('days');
nw{'riv_time'}.cycle_length = 360;

nw{'RIV'} = ncdouble('n_riv','riv_time');
nw{'RIV'}.long_name = ncchar('river runoff time');
nw{'RIV'}.units = ncchar('days');

nw{'riv_name'} = ncchar('n_riv','RivnameStrLen');
nw{'riv_name'}.long_name = ncchar('river runoff time');
result = endef(nw);

% Fill th value
%
nw{'riv_time'} (:) = [15:30:365];
nw{'RIV'}(:) = A.*100';
nw{'riv_name'}(:) = riv_name;
close (nw)
  
%Localisation des points source :

R_i= [32 36 28];
R_j= [20 14 33];
grdfile='ROMS_FILES/roms_grd.nc';
[lat, lon, mask]=read_latlonmask(grdfile,'r');

figure(1)
pcolor(mask)
hold on
plot(R_i,R_j,'or')




