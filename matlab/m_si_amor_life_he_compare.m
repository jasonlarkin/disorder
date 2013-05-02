clear

con = m_constant;

sed = load('/home/jason/disorder2/si/amor/normand/perf4096/anneal_1100K/emin/SEDfit_fixed.mat');

he = load('/home/jason/disorder/si/he_si_amor_life.txt');
loglog(...
    sed.HLDfreq,sed.life,'.',...
    2*pi*1e12*he(:,1),he(:,2),'.'...
    )
