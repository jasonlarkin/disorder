
str.NMD = '/home/jason/disorder/lj/alloy/10K/0.5/10x/NMD/1/work/';
SED=load(strcat(str.NMD,'SEDdata.mat'));
NMD=load(strcat(str.NMD,'NMDdata.mat'));

SED = nmd_convert_data(NMD,SED);

ALLOY = m_ld_defect_life(NMD,SED, 0.5 , 1 , 3 , 2.0 );

%SHOULD THE EIGENVECTORS/FREQS FROM FROM VC, OR PERFECT M=1?


loglog(...
    SED.freq*NMD.LJ.tau,SED.life/NMD.LJ.tau,'.',...
    ALLOY.freq, ALLOY.life,'.',...
    ALLOY.freq, 1E4*ALLOY.freq.^(-4)...
    )


save(strcat(str.NMD,'ALLOY.mat'), '-struct', 'ALLOY');



