clear
str.NMD ='/home/jason/disorder2/lj/alloy/10K/0.5/10x/nmd_vc/work/1/';
%str.NMD = '/home/jason/disorder2/lj/alloy/10K/0.05/10x/NMD/1/work/';
SED=load(strcat(str.NMD,'SEDdata.mat'));
NMD=load(strcat(str.NMD,'NMDdata.mat'));
SED = nmd_convert_data(NMD,SED);



ALLOY = m_ld_defect_life(NMD, 0.15 , 1 , 3 , 1.3 , 100 , 0.45 , 1);

%--------------------------------------------------------------------------
%figure
loglog(...
    SED.freq,SED.life,'.',...
    ALLOY.freq, ALLOY.life(:,1),'.',...
    ALLOY.freq, 1E4*ALLOY.freq.^(-4)...
    )

% loglog(...
%     ALLOY.freq, ALLOY.life(:,1),'.',...
%     ALLOY.freq, 1E4*ALLOY.freq.^(-4)...
%     )

save(strcat(str.NMD,'ALLOY.mat'), '-struct', 'ALLOY');



