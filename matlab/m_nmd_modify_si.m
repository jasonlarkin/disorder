%function m_ald_taud_si(str_ald,str_nmd_per,str_nmd_vc,str_alloy)
%--------------------------------------------------------------------------
%m_ald_taud(str_ald,str_nmd_perfect,str_nmd_vc,str_alloy)
% m_ald_taud(...
% '/home/jason/disorder/lj/ald/m1.1/10x/Data_fullBZ.xls',...
% '/home/jason/disorder2/lj/alloy/10K/0.05/10x/nmd_vc/work/1/',...
% '/home/jason/disorder2/lj/alloy/10K/0.05/10x/NMD/1/work/',...
% '/home/jason/disorder2/lj/alloy/10K/0.05/10x/nmd_vc/work/1/')
%--------------------------------------------------------------------------

clear

constant = m_constant;

sys_size = '8x';

%--------------------------------------------------------------------------
%load
%-------------------------------------------------------------------------- 
ipt = 1;
str_ald = ['/home/jason/disorder2/si/ald/conv/' sys_size '/Data_fullBZ.xls'];
str_alloy = ['/home/jason/disorder2/si/alloy/0.05/' sys_size '/'];
str_nmd = ['/home/jason/disorder2/si/alloy/0.0/' sys_size '/'];
nmd=load(strcat(str_nmd,'nmd.mat'));
ald = m_joe_read_data_si(str_ald);
alloy = load(strcat(str_alloy,'ALLOY.mat'));

aldsed =...
    m_joe_ald2nmd_si(...
    nmd.NUM_KPTS, nmd.kptmodelist , nmd.NUM_MODES , ald );

nmdnew.kpt = aldsed.kpt; nmdnew.freq = aldsed.freq;
nmdnew.vel = aldsed.vel;
%hori
str_hori = ['/home/jason/Documents/sed/hori/' sys_size '_TA.txt'];
A = load(str_hori); hori.TA = sortrows(A);
str_hori = ['/home/jason/Documents/sed/hori/' sys_size '_LA.txt'];
A = load(str_hori); hori.LA = sortrows(A);
str_hori = ['/home/jason/Documents/sed/hori/' sys_size '_TO.txt'];
A = load(str_hori); hori.TO = sortrows(A);
str_hori = ['/home/jason/Documents/sed/hori/' sys_size '_LO.txt'];
A = load(str_hori); hori.LO = sortrows(A);

hori.A(:,1) = [hori.TA(:,1);hori.LA(:,1);hori.TO(:,1);hori.LO(:,1)];
hori.A(:,2) = [hori.TA(:,2);hori.LA(:,2);hori.TO(:,2);hori.LO(:,2)];

hori.B = sortrows(hori.A);
[B,I,J] = unique(hori.B(:,1),'rows');
hori.C(:,1) = hori.B(I,1); hori.C(:,2) = hori.B(I,2); 
hori.freq = hori.C(:,1); hori.life = hori.C(:,2);

loglog(...
    ald.freq,ald.life,'.',...
    hori.freq/constant.s2ps*2*pi,hori.life*constant.s2ps,'.'...
    )
%interpolate
yi = interp1(...
    hori.freq/constant.s2ps*2*pi,...
    hori.life*constant.s2ps,...
    nmdnew.freq,'cubic');

%make < 0 life =0
[I,J] = find(yi < 0); yi(I) = 0.0;
%plot interpoalte lifetimes
loglog(...
    ald.freq,ald.life,'.',...
    hori.freq/constant.s2ps*2*pi,hori.life*constant.s2ps,'.',...
    nmdnew.freq,yi,'.'...
    )

nmdnew.life = yi; nmdnew.VOLUME = nmd.x0.VOLUME;
%make 0-freq life =0
[I,J] = sort(nmdnew.life,'descend');
nmdnew.life(J(1:3)) = 0.0;

%boost
nmdnew.life = nmdnew.life*1.5;
%conductivity
cond_nmdnew =...
    m_ald_cond(nmdnew.life,nmdnew.vel(:,1),nmd.x0.VOLUME*constant.ang2m^3)
cond_aldsed =...
    m_ald_cond(aldsed.life,aldsed.vel(:,1),nmd.x0.VOLUME*constant.ang2m^3)
%plot final
loglog(...
    ald.freq,ald.life,'.',...
    hori.freq/constant.s2ps*2*pi,hori.life*constant.s2ps,'.',...
    nmdnew.freq,nmdnew.life,'.'...
    )


save([str_nmd 'SEDdata.mat'], '-struct', 'nmdnew');

pause
plot(aldsed.life,nmdnew.life,'.')


