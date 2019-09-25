function moy = nanmean_lpo(tmp,dim)
 
if nargin == 1
    idx = isnan(tmp);
    tmp(idx) = 0;%Mise a zero des valeurs egales a NaN
    ntot = sum(~idx);%Nbre de valeurs non egales a NaN
    ntot(ntot==0) = NaN;%Traitement pour nombre de valeurs = 0
    moy = sum(tmp)./ntot;
else
    idx = isnan(tmp);
    tmp(idx) = 0;%Mise a zero des valeurs egales a NaN
    ntot = sum(~idx,dim);%Nbre de valeurs non egales a NaN
    ntot(ntot==0) = NaN;;%Traitement pour nombre de valeurs = 0
    moy = sum(tmp,dim)./ntot;
end


