
NMD = load(...
'/home/jason/Dropbox/wee/dsf/NMDavg.mat');
SED = load(...
'/home/jason/Dropbox/wee/dsf/SEDavg.mat');

for ikpt=1:size(SED.sed,2)
    ikpt
semilogy(SED.omega,SED.sed(:,ikpt),'.')
pause
end