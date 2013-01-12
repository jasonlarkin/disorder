clear

icnt=1;
str.NMD = '/home/jason/disorder2/lj/amor/4x/XCORR_AF/10K/2^19_2^16/';
NMD(icnt).NMD = load([str.NMD 'NMDfit.mat']);
SED(icnt).SED = load([str.NMD 'SEDfit.mat']);
icnt=2;
str.NMD = '/home/jason/disorder2/lj/amor/4x/XCORR_AF/5K/2^19_2^16/';
NMD(icnt).NMD = load([str.NMD 'NMDfit.mat']);
SED(icnt).SED = load([str.NMD 'SEDfit.mat']);
icnt=3;
str.NMD = '/home/jason/disorder2/lj/amor/4x/XCORR_AF/1K/2^19_2^16/';
NMD(icnt).NMD = load([str.NMD 'NMDfit.mat']);
SED(icnt).SED = load([str.NMD 'SEDfit.mat']);
icnt=4;
str.NMD = '/home/jason/disorder2/lj/amor/4x/XCORR_AF/0.1K/2^19_2^16/';
NMD(icnt).NMD = load([str.NMD 'NMDfit.mat']);
SED(icnt).SED = load([str.NMD 'SEDfit.mat']);

loglog(...
    SED(1).SED.HLDfreq(:),SED(1).SED.life(:),'.',...
    SED(2).SED.HLDfreq(:),SED(2).SED.life(:),'.',...
    SED(3).SED.HLDfreq(:),SED(3).SED.life(:),'.',...
    SED(4).SED.HLDfreq(:),SED(4).SED.life(:),'.',...
    SED(1).SED.HLDfreq(:),(2*pi)./SED(1).SED.HLDfreq(:)...
    )


