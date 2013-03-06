clear
ipt=1
VIRTUAL_MASS = 1.1;
freq_cutoff = 0;
dph_scaling = 0.5;
vg_scaling = 0.75;
constant = m_constant; lj = m_lj;
%vcnmd
str.nmd = '/home/jason/disorder2/lj/alloy/10K/0.05/10x/NMD/1/work/';
vcnmd(ipt).nmd = load([str.nmd 'NMDdata.mat']);
vcnmd(ipt).sed = load([str.nmd 'SEDdata.mat']);
%vcald
str.alloy = '/home/jason/disorder2/lj/alloy/10K/0.05/10x/NMD/1/work/';
alloy(ipt).alloy = load([str.alloy 'ALLOY.mat']);
str.ald = '/home/jason/disorder2/lj/ald/m1.1/10x/Data_fullBZ.xls';
vcald(ipt).ald = m_joe_read_data([str.ald]);
vcald(ipt).sedald = m_joe_ald2nmd( vcnmd(ipt).nmd , vcald(ipt).ald );

% amorphous
% af(ipt).sound =...
%     ( (2/3)*(623.27) + (1/3)*(1412.11) )*(1/vg_scaling)...
%     *vcnmd(1).nmd.LJ.tau/vcnmd(1).nmd.LJ.sigma...
%     /sqrt(VIRTUAL_MASS);

af(ipt).sound =...
    ( (2/3)*(860.5162) + (1/3)*(1581.3) )...
    *vcnmd(1).nmd.LJ.tau/vcnmd(1).nmd.LJ.sigma...
    /sqrt(VIRTUAL_MASS);

vcald(ipt).cond =...
    m_ald_cond(...
    1./(1./alloy(ipt).alloy.life +...
    1./vcald(ipt).sedald.life)*vcnmd(ipt).nmd.LJ.tau,...
    vcald(ipt).sedald.vel(:,1)*vcnmd(ipt).nmd.LJ.sigma/vcnmd(ipt).nmd.LJ.tau,...
    vcnmd(ipt).nmd.VOLUME);

vcnmd(ipt).cond =...
    m_ald_cond(...
    vcnmd(ipt).sed.life*vcnmd(ipt).nmd.LJ.tau,...
    vcnmd(ipt).sed.groupvel(:,1)*vcnmd(ipt).nmd.LJ.sigma/vcnmd(ipt).nmd.LJ.tau,...
    vcnmd(ipt).nmd.VOLUME);
vcald(ipt).cond
vcnmd(ipt).cond

loglog(...
    vcald(ipt).sedald.freq,...
    1./(1./alloy(ipt).alloy.life + 1./vcald(ipt).sedald.life),'.',...
    vcnmd(ipt).sed.freq,...
    vcnmd(ipt).sed.life,'.'...
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
%nmd DHS
%--------------------------------------------------------------------------
%adjust for D_ph
vcnmd(ipt).sed.Dphi =...
    vcnmd(ipt).sed.life.*...
    (vcnmd(ipt).sed.groupvel(:,1).^2);
vcnmd(ipt).sed.Dphf = vcnmd(ipt).sed.Dphi;
[Idiff Jdiff] =...
    find(...
    vcnmd(ipt).sed.freq > freq_cutoff & ...
    vcnmd(ipt).sed.Dphi < ...
    (1/3)*af(ipt).sound*vcnmd(ipt).nmd.alat*dph_scaling ...
    );
%swap values
vcnmd(ipt).sed.Dphf(Idiff) = ...
    (1/3)*af(ipt).sound*vcnmd(ipt).nmd.alat*dph_scaling;
old_cond =...
    (constant.kb/vcnmd(ipt).nmd.VOLUME)*...
    sum(...
    vcnmd(ipt).sed.Dphi*...
    vcnmd(ipt).nmd.LJ.sigma^2/vcnmd(ipt).nmd.LJ.tau...
    )
new_cond =...
    (constant.kb/vcnmd(ipt).nmd.VOLUME)*...
    sum(...
    vcnmd(ipt).sed.Dphf*...
    vcnmd(ipt).nmd.LJ.sigma^2/vcnmd(ipt).nmd.LJ.tau...
    )
%--------------------------------------------------------------------------
loglog(...
    vcnmd(ipt).sed.freq,...
    vcnmd(ipt).sed.Dphi,'.',...
    vcnmd(ipt).sed.freq,...
    vcnmd(ipt).sed.Dphf,'.'...
    )
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------


%--------------------------------------------------------------------------
%nmd DIR
%--------------------------------------------------------------------------
%adjust for D_ph
vcnmd(ipt).sed.Dphi =...
    vcnmd(ipt).sed.life.*...
    (vcnmd(ipt).sed.groupvel(:,1).^2);
vcnmd(ipt).sed.DphIR = vcnmd(ipt).sed.Dphi;
[Idiff Jdiff] =...
    find(...
    vcnmd(ipt).sed.freq > freq_cutoff & ...
    vcnmd(ipt).sed.Dphi < ...
    (1/3)*af(ipt).sound^2*(2*pi./vcnmd(ipt).sed.freq) ...
    );
%swap values
vcnmd(ipt).sed.DphIR(Idiff) = ...
    (1/3)*af(ipt).sound^2*(2*pi./vcnmd(ipt).sed.freq(Idiff));
old_cond =...
    (constant.kb/vcnmd(ipt).nmd.VOLUME)*...
    sum(...
    vcnmd(ipt).sed.Dphi*...
    vcnmd(ipt).nmd.LJ.sigma^2/vcnmd(ipt).nmd.LJ.tau...
    )
new_cond =...
    (constant.kb/vcnmd(ipt).nmd.VOLUME)*...
    sum(...
    vcnmd(ipt).sed.DphIR*...
    vcnmd(ipt).nmd.LJ.sigma^2/vcnmd(ipt).nmd.LJ.tau...
    )
%--------------------------------------------------------------------------
loglog(...
    vcnmd(ipt).sed.freq,...
    vcnmd(ipt).sed.Dphi,'.',...
    vcnmd(ipt).sed.freq,...
    vcnmd(ipt).sed.Dphf,'.',...
    vcnmd(ipt).sed.freq,...
    vcnmd(ipt).sed.DphIR,'.'...
    )
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------



%--------------------------------------------------------------------------
%ald
%--------------------------------------------------------------------------
%adjust for D_ph
vcald(ipt).sedald.Dphi =...
    1./(1./alloy(ipt).alloy.life + 1./vcald(ipt).sedald.life).*...
    (vcald(ipt).sedald.vel(:,1).^2);
vcald(ipt).sedald.Dphf = vcald(ipt).sedald.Dphi;
[Idiff Jdiff] =...
    find(...
    vcald(ipt).sedald.freq > freq_cutoff & ...
    vcald(ipt).sedald.Dphi < ...
    (1/3)*af(ipt).sound*vcnmd(ipt).nmd.alat*dph_scaling ...
    );
%swap values
vcald(ipt).sedald.Dphf(Idiff) = ...
    (1/3)*af(ipt).sound*vcnmd(ipt).nmd.alat*dph_scaling;
old_cond =...
    (constant.kb/vcnmd(ipt).nmd.VOLUME)*...
    sum(...
    vcald(ipt).sedald.Dphi*...
    vcnmd(ipt).nmd.LJ.sigma^2/vcnmd(ipt).nmd.LJ.tau...
    )
new_cond =...
    (constant.kb/vcnmd(ipt).nmd.VOLUME)*...
    sum(...
    vcald(ipt).sedald.Dphf*...
    vcnmd(ipt).nmd.LJ.sigma^2/vcnmd(ipt).nmd.LJ.tau...
    )





%--------------------------------------------------------------------------
loglog(...
    vcald(ipt).sedald.freq,...
    vcald(ipt).sedald.Dphi,'.',...
    vcald(ipt).sedald.freq,...
    vcald(ipt).sedald.Dphf,'.'...
    )
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------

loglog(...
    vcnmd(ipt).sed.freq,...
    vcnmd(ipt).sed.Dphi,'.',...
    vcnmd(ipt).sed.freq,...
    vcnmd(ipt).sed.Dphf,'.',...
    vcald(ipt).sedald.freq,...
    vcald(ipt).sedald.Dphi,'.',...
    vcald(ipt).sedald.freq,...
    vcald(ipt).sedald.Dphf,'.'...
    )


