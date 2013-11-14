function n2o=nevis_2003(zw,o2);

if ( zw <= -50)
    cff=16./170.*0.26/o2-0.0004*exp(zw./3000.);    %mmolN2O/mmolO2
    n2o=cff*o2;
else
    n2o=0.0076;
end
return