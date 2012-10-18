clear
%str.NMD ='/home/jason/disorder2/lj/alloy/10K/0.5/10x/nmd_vc/work/1/';
%str.NMD = '/home/jason/disorder2/lj/alloy/10K/0.05/10x/NMD/1/work/';

str.nmd = '/home/jason/disorder2/si/alloy/0.0/8x/';

NMD=load(strcat(str.nmd,'nmd.mat'));


ALLOY = m_ld_defect_life(NMD, 0.05 , 1 , 3 , 1.3 , 100 , 0.45 , 1);

%--------------------------------------------------------------------------
%figure
% loglog(...
%     SED.freq,SED.life,'.',...
%     ALLOY.freq, ALLOY.life(:,1),'.',...
%     ALLOY.freq, 1E4*ALLOY.freq.^(-4)...
%     )

loglog(...
    ALLOY.freq, ALLOY.life(:,1),'.',...
    ALLOY.freq, 1E4*ALLOY.freq.^(-4)...
    )

save(strcat(str.NMD,'ALLOY.mat'), '-struct', 'ALLOY');



