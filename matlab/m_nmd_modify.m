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
    sed_per.freq,sed_per.life,'.',...
    sed_vc.freq,sed_vc.life,'.',...
    sed_vc.freq,1./(1./sed_vc.life - 1./sed_per.life),'.',...
    sed_per.ald.freq , 2*pi./sed_per.ald.freq...
    )

%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 

loglog(...
    sed_vc.freq,sed_vc.life,'.',...
    sed_per.ald.freq,sed_per.ald.life,'.',...
    0.97*sed_per.ald.freq,...
    1./( (1./sed_per.ald.life) + (1./alloy.life(:,1)) ),'.',...
    sed_per.ald.freq , 2*pi./sed_per.ald.freq...
    )

%--------------------------------------------------------------------------
pause
%-------------------------------------------------------------------------- 

freq_cut = 9.24;

I = find( sed_per.ald.freq <= freq_cut );
mix.freq(I,1) = sed_per.ald.freq(I);
mix.life(I,1) = 1./( (1./sed_per.ald.life(I)) + (1./alloy.life(I,1)) );

I = find( sed_per.ald.freq > freq_cut );
mix.freq(I,1) = sed_vc.freq(I);
mix.life(I,1) = sed_vc.life(I);

loglog(...
    sed_vc.freq,sed_vc.life,'.',...
    sed_per.ald.freq,sed_per.ald.life,'.',...
    0.97*sed_per.ald.freq,...
    1./( (1./sed_per.ald.life) + (1./alloy.life(:,1)) ),'.',...
    mix.freq,mix.life,'.'...
    )

mix

cond_mix =...
    m_ald_cond(...
    mix.life*lj.tau,...
    sed_per.ald.vel(:,1)*(lj.sigma/lj.tau),...
    nmd_per.VOLUME)




%end