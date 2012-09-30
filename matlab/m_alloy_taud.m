clear
str.NMD = '/home/jason/disorder2/lj/alloy/10K/0.05/10x/nmd_vc/work/1/';
SED=load(strcat(str.NMD,'SEDdata.mat'));
NMD=load(strcat(str.NMD,'NMDdata.mat'));

SED = nmd_convert_data(NMD,SED);

ALLOY = m_ld_defect_life(NMD,SED, 0.05 , 1 , 3 , 1.1 , 200.0);

%--------------------------------------------------------------------------

loglog(...
    SED.freq,SED.life,'.',...
    ALLOY.freq, ALLOY.life,'.',...
    ALLOY.freq, 1E4*ALLOY.freq.^(-4)...
    )

save(strcat(str.NMD,'ALLOY.mat'), '-struct', 'ALLOY');



