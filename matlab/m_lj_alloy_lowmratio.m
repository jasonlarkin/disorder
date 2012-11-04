clear

str.nmd = ['/home/jason/disorder2/lj/alloy/10K/0.0/10x/NMD/1/work/'];
nmd=load(strcat(str.nmd,'NMDdata.mat'));
SED=load(strcat(str.nmd,'SEDdata.mat'));
sed = nmd_convert_data(nmd,SED);

str.ald = '/home/jason/disorder2/lj/ald/m1/10x/Data_fullBZ.xls';
ald = m_joe_read_data_si(str.ald);
aldsed =...
    m_joe_ald2nmd( nmd , ald );

str.alloy = '/home/jason/disorder2/lj/alloy/10K/diff_mass/m1_m1.1_c01/';
alloy(1).alloy = load([str.alloy 'ALLOY.mat']);
str.alloy = '/home/jason/disorder2/lj/alloy/10K/diff_mass/m1_m1.1_c05/';
alloy(2).alloy = load([str.alloy 'ALLOY.mat']);
str.alloy = '/home/jason/disorder2/lj/alloy/10K/diff_mass/m1_m1.1_c5/';
alloy(3).alloy = load([str.alloy 'ALLOY.mat']);
str.alloy = '/home/jason/disorder2/lj/alloy/10K/diff_mass/m1_m3_c01/';
alloy(4).alloy = load([str.alloy 'ALLOY.mat']);
str.alloy = '/home/jason/disorder2/lj/alloy/10K/diff_mass/m1_m3_c05/';
alloy(5).alloy = load([str.alloy 'ALLOY.mat']);



loglog(...
    aldsed.freq*nmd.LJ.tau,aldsed.life/nmd.LJ.tau,'.',...
    alloy(1).alloy.freq,alloy(1).alloy.life,'.',...
    alloy(2).alloy.freq,alloy(2).alloy.life,'.',...
    alloy(3).alloy.freq,alloy(3).alloy.life,'.',...
    alloy(4).alloy.freq,alloy(4).alloy.life,'.',...
    alloy(5).alloy.freq,alloy(5).alloy.life,'.'...
    )