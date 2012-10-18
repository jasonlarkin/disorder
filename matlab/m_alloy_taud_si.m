clear
%str.NMD ='/home/jason/disorder2/lj/alloy/10K/0.5/10x/nmd_vc/work/1/';
%str.NMD = '/home/jason/disorder2/lj/alloy/10K/0.05/10x/NMD/1/work/';

str.nmd = '/home/jason/disorder2/si/alloy/0.05/8x/';
NMD=load(strcat(str.nmd,'nmd.mat'));



ALLOY = m_ld_defect_life_si(NMD, 0.05 , 1 , 2.6 , 1.08 , 100 , 1.0 , 1);

%--------------------------------------------------------------------------
%figure
% loglog(...
%     SED.freq,SED.life,'.',...
%     ALLOY.freq, ALLOY.life(:,1),'.',...
%     ALLOY.freq, 1E4*ALLOY.freq.^(-4)...
%     )

loglog(...
    ALLOY.freq, ALLOY.life(:,1),'.',...
    ALLOY.freq, 1E40*ALLOY.freq.^(-4)...
    )

save(strcat(str.nmd,'ALLOY.mat'), '-struct', 'ALLOY');



