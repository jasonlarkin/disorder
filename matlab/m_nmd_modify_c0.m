%function m_ald_taud_nmd_mix(str_ald,str_nmd_per,str_nmd_vc,str_alloy)
%--------------------------------------------------------------------------
%m_ald_taud(str_ald,str_nmd_perfect,str_nmd_vc,str_alloy)
% m_ald_taud(...
% '/home/jason/disorder/lj/ald/m1.1/10x/Data_fullBZ.xls',...
% '/home/jason/disorder2/lj/alloy/10K/0.05/10x/nmd_vc/work/1/',...
% '/home/jason/disorder2/lj/alloy/10K/0.05/10x/NMD/1/work/',...
% '/home/jason/disorder2/lj/alloy/10K/0.05/10x/nmd_vc/work/1/')
%--------------------------------------------------------------------------

str_ald = '/home/jason/disorder/lj/ald/m1.1/10x/Data_fullBZ.xls';
str_nmd_per = '/home/jason/disorder2/lj/alloy/10K/0.05/10x/nmd_vc/work/1/';
str_nmd_vc = '/home/jason/disorder2/lj/alloy/10K/0.05/10x/NMD/1/work/';
str_alloy = '/home/jason/disorder2/lj/alloy/10K/0.05/10x/nmd_vc/work/1/';

lj = m_lj; constant = m_constant;

ald = m_joe_read_data(str_ald);

nmd_per=load(strcat(str_nmd_per,'NMDdata.mat'));
sed_per=load(strcat(str_nmd_per,'SEDdata.mat'));
sed_per = nmd_convert_data(nmd_per,sed_per)

nmd_vc=load(strcat(str_nmd_vc,'NMDdata.mat'));
sed_vc=load(strcat(str_nmd_vc,'SEDdata.mat'));
sed_vc = nmd_convert_data(nmd_vc,sed_vc)

sed_per.ald = m_joe_ald2nmd( nmd_per , sed_per , ald );

alloy = load(strcat(str_alloy,'ALLOY.mat'));


loglog(...
    alloy.freq,alloy.life,'.'...
    )

%nmd_per vs nmd_vc
loglog(...
    sed_per.freq,sed_per.life,'.'...
    )

%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 


%end