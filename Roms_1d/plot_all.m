a=4;
b=3;
i=1;
subplot(a,b,i)
tzplot('u');
title('u [m.s^{-1}]')
ylabel('Depth [m]')
set(gca,'XTickLabel',{' '})

i=i+1;
subplot(a,b,i)
tzplot('v');
title('v [m.s^{-1}]')
set(gca,'XTickLabel',{' '})
set(gca,'YTickLabel',{' '})

i=i+1;
subplot(a,b,i)
tzplot('t01');
title('T [^{\circ} C]')
set(gca,'XTickLabel',{' '})
set(gca,'YTickLabel',{' '})
[amin amax]=caxis;
amin=max(0,amin);
caxis([amin amax]);

i=i+1;
subplot(a,b,i)
tzplot('t02');
title('S [PSU]')
ylabel('Depth [m]')
set(gca,'XTickLabel',{' '})
[amin amax]=caxis;
amin=max(0,amin);
caxis([amin amax])

i=i+1;
subplot(a,b,i)
tzplot('t03');
title('NO_3 [mMol N_2 m^{-3}]')
set(gca,'XTickLabel',{' '})
set(gca,'YTickLabel',{' '})
[amin amax]=caxis;
amin=max(0,amin);
caxis([amin amax])

i=i+1;
subplot(a,b,i)
tzplot('t04');
title('NH_4 [mMol N_2 m^{-3}]')
set(gca,'XTickLabel',{' '})
set(gca,'YTickLabel',{' '})
[amin amax]=caxis;
amin=max(0,amin);
caxis([amin amax])

i=i+1;
subplot(a,b,i)
tzplot('t05');
title('Chl_a [mg Chl_a m^{-3}]')
ylabel('Depth [m]')
set(gca,'XTickLabel',{' '})
[amin amax]=caxis;
amin=max(0,amin);
caxis([amin amax])

i=i+1;
subplot(a,b,i)
tzplot('t06');
title('Phyt [mMol N_2 m^{-3}]')
set(gca,'XTickLabel',{' '})
set(gca,'YTickLabel',{' '})
[amin amax]=caxis;
amin=max(0,amin);
caxis([amin amax])

i=i+1;
subplot(a,b,i)
tzplot('t07');
title('Zoo [mMol N_2 m^{-3}]')
set(gca,'XTickLabel',{' '})
set(gca,'YTickLabel',{' '})
[amin amax]=caxis;
amin=max(0,amin);
caxis([amin amax])

i=i+1;
subplot(a,b,i)
tzplot('t08');
title('SDet [mMol N_2 m^{-3}]')
xlabel('Time [Days]')
ylabel('Depth [m]')
[amin amax]=caxis;
amin=max(0,amin);
caxis([amin amax])

i=i+1;
subplot(a,b,i)
tzplot('t09');
title('LDet [mMol N_2 m^{-3}]')
xlabel('Time [Days]')
set(gca,'YTickLabel',{' '})
[amin amax]=caxis;
amin=max(0,amin);
caxis([amin amax])

i=i+1;
subplot(a,b,i)
tzplot('t10');
title('Oxy [mMol O_2 m^{-3}]')
xlabel('Time [Days]')
set(gca,'YTickLabel',{' '})
[amin amax]=caxis;
amin=max(0,amin);
caxis([amin amax])

