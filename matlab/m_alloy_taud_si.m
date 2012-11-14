clear
%str.NMD ='/home/jason/disorder2/lj/alloy/10K/0.5/10x/nmd_vc/work/1/';
%str.NMD = '/home/jason/disorder2/lj/alloy/10K/0.05/10x/NMD/1/work/';

str.nmd = '/home/jason/disorder2/si/alloy/0.5/20x/';
NMD=load(strcat(str.nmd,'nmd.mat'));

m1 = 1; m2 = 2.6; c = 0.5; vm = (1-c)*m1 + c*m2

ALLOY = m_ld_defect_life_si(NMD, c , m1 , m2 , vm , 100 , 0.45 , 1);

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



