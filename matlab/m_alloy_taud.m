clear

% str.NMD = '/home/jason/disorder2/lj/alloy/10K/0.01/10x/NMD/1/work/';
% NMD=load(strcat(str.nmd,'nmd.mat'));

str.NMD = '/home/jason/disorder2/lj/alloy/10K/0.01/10x/NMD/1/work/';
NMD=load(strcat(str.NMD,'NMDdata.mat'));
str.save = '/home/jason/disorder2/lj/alloy/10K/diff_mass/m1_m1.1_c5/';

m1 = 1; m2 = 1.1; c = 0.5; vm = (1-c)*m1 + c*m2

pause

ALLOY = m_ld_defect_life(NMD, c , m1 , m2 , vm , 100 , 0.45 , 1);

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

save(strcat(str.save,'ALLOY.mat'), '-struct', 'ALLOY');



