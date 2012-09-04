
%4x
str.nmd = '/home/jason/disorder/lj/alloy/10K/0.0/4x/XCORR_AF/1/work';
nmdx(1).nmd = load(strcat(str.nmd,'/NMDfit.mat'));
nmdx(1).sed = load(strcat(str.nmd,'/SEDfit.mat'));
%4x_2
str.nmd = '/home/jason/disorder/lj/alloy/10K/0.0/4x/XCORR_AF/tmp/1';
nmdx(2).nmd = load(strcat(str.nmd,'/NMDfit.mat'));
nmdx(2).sed = load(strcat(str.nmd,'/SEDfit.mat'));
%4x_3
str.nmd = '/home/jason/disorder/lj/alloy/10K/0.0/4x/XCORR_AF/work/1';
nmdx(3).nmd = load(strcat(str.nmd,'/NMDfit.mat'));
nmdx(3).sed = load(strcat(str.nmd,'/SEDfit.mat'));
%4x_4
str.nmd = '/home/jason/disorder/lj/alloy/10K/0.0/4x/XCORR_AF/219/1';
nmdx(4).nmd = load(strcat(str.nmd,'/NMDfit.mat'));
nmdx(4).sed = load(strcat(str.nmd,'/SEDfit.mat'));

loglog(...
    nmdx(1).sed.HLDfreq,nmdx(1).sed.life,'.',...
    nmdx(2).sed.HLDfreq,nmdx(2).sed.life,'.',...
    nmdx(3).sed.HLDfreq,nmdx(3).sed.life,'.',...
    nmdx(4).sed.HLDfreq,nmdx(4).sed.life,'.'...
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------

str.NMD = '/home/jason/disorder/lj/alloy/10K/0.0/4x/NMD/1/work';
NMD=load(strcat(str.NMD,'/NMDdata.mat'));
SED=load(strcat(str.NMD,'/SEDdata.mat'));
SED = nmd_convert_data(NMD,SED);

str.NMD_AF = '/home/jason/disorder/lj/alloy/10K/0.0/4x/NMD_AF/1/work';
NMD_AF=load(strcat(str.NMD_AF,'/NMDfit.mat'));
SED_AF=load(strcat(str.NMD_AF,'/SEDfit.mat'));

%str.NMD_X = '/home/jason/disorder/lj/alloy/10K/0.0/4x/XCORR_AF/1/work';
str.NMD_X = '/home/jason/disorder/lj/alloy/10K/0.0/4x/XCORR_AF/tmp/1';
NMD_X=load(strcat(str.NMD_X,'/NMDfit.mat'));
SED_X=load(strcat(str.NMD_X,'/SEDfit.mat'));

loglog(...
    SED.freq,SED.life,'.',...
    SED_X.HLDfreq,SED_X.life,'.'...
    )
pause
loglog(...
    SED.freq,SED.life,'.',...
    SED_AF.HLDfreq,SED_AF.life,'.'...
    )
pause
loglog(...
    SED_X.HLDfreq,SED_X.life,'.',...
    SED_AF.HLDfreq,SED_AF.life,'.'...
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
loglog(...
    SED.freq,SED.life,'.',...
    nmdx(3).sed.HLDfreq,nmdx(3).sed.life,'.'...
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
loglog(...
    SED_AF.HLDfreq,SED_AF.life,'.',...
    nmdx(3).sed.HLDfreq,nmdx(3).sed.life,'.'...
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
loglog(...
    SED_AF.HLDfreq,SED_AF.life,'.',...
    nmdx(4).sed.HLDfreq,nmdx(4).sed.life,'.'...
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------


