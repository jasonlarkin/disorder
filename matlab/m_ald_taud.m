function m_ald_taud(str_ald,str_nmd_per,str_nmd_vc,str_alloy)
%--------------------------------------------------------------------------
%m_ald_taud(str_ald,str_nmd_perfect,str_nmd_vc,str_alloy)
% m_ald_taud(...
% '/home/jason/disorder/lj/ald/m1.1/10x/Data_fullBZ.xls',...
% '/home/jason/disorder2/lj/alloy/10K/0.05/10x/nmd_vc/work/1/',...
% '/home/jason/disorder2/lj/alloy/10K/0.05/10x/NMD/1/work/',...
% '/home/jason/disorder2/lj/alloy/10K/0.05/10x/nmd_vc/work/1/')
%--------------------------------------------------------------------------

lj = m_lj; constant = m_constant;

ald = m_joe_read_data(str_ald);

nmd_per=load(strcat(str_nmd_per,'NMDdata.mat'));
sed_per=load(strcat(str_nmd_per,'SEDdata.mat'));
sed_per = nmd_convert_data(nmd_per,sed_per);

nmd_vc=load(strcat(str_nmd_vc,'NMDdata.mat'));
sed_vc=load(strcat(str_nmd_vc,'SEDdata.mat'));
sed_vc = nmd_convert_data(nmd_vc,sed_vc);

sed_per.ald = m_joe_ald2nmd( nmd_per , sed_per , ald );

alloy = load(strcat(str_alloy,'ALLOY.mat'))
alloy.life_d = alloy.life_d/nmd_per.LJ.tau;
alloy.life_pp = alloy.life_pp/nmd_per.LJ.tau;


loglog(...
    alloy.freq,alloy.life,'.',...
    alloy.freq,alloy.life_pp,'.',...
    alloy.freq,alloy.life_d,'.'...
    )

%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 

loglog(...
    sed_per.ald.freq,sed_per.ald.life,'.',...
    alloy.freq,alloy.life,'.'...
    )

%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 

loglog(...
    sed_per.ald.freq,sed_per.ald.life,'.',...
    sed_per.ald.freq,...
    1./( (1./sed_per.ald.life) + (1./alloy.life') ),'.',...
    alloy.freq,alloy.life,'.'...
    )

size(alloy.life)
size(sed_per.ald.life)

%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 

loglog(...
    sed_vc.freq,sed_vc.life,'.',...
    sed_per.ald.freq,...
    1./( (1./sed_per.ald.life) + (1./alloy.life') ),'.'...
    )

sed_per.ald
sed_per

cond =...
    m_ald_cond(...
    (1./( (1./sed_per.ald.life) + (1./alloy.life') ))*lj.tau,...
    sed_per.ald.vel(:,1)*(lj.sigma/lj.tau),...
    nmd_per.VOLUME)

max(sed_per.ald.vel(:,1)*(lj.sigma/lj.tau))
nmd_per.VOLUME

end