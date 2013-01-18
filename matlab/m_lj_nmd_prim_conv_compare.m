clear

lj = m_lj;
c = m_constant;

str_nmd = '/home/jason/disorder2/lj/alloy/10K/0.0/10x/NMD/1/work/';

%nmd.nmdfit_o = load([str_nmd 'NMDfit_orig.mat']);
%nmd.sedfit_o = load([str_nmd 'SEDfit_orig.mat']);

nmd.nmddata_o = load([str_nmd 'NMDdata_messedup.mat']);
nmd.seddata_o = load([str_nmd 'SEDdata_messedup.mat']);

nmd.seddata_o = nmd_convert_data(nmd.nmddata_o,nmd.seddata_o);

nmd.nmddata = load([str_nmd 'NMDdata.mat']);
nmd.seddata = load([str_nmd 'SEDdata.mat']);

nmd.seddata = nmd_convert_data(nmd.nmddata,nmd.seddata);

sed.conv =...
    dlmread('/home/jason/sed/data/LJ/sed_conv_10x_20K.dat');
sed.prim =...
    dlmread('/home/jason/sed/data/LJ/sed_prim_10x_20K.dat');

sed.conv6x =...
    dlmread('/home/jason/sed/data/LJ/sed_conv_6x_20K.dat');
sed.prim6x =...
    dlmread('/home/jason/sed/data/LJ/sed_prim_6x_20K.dat');

loglog(...
    sed.prim6x(:,1),sed.prim6x(:,3),'.',...
    sed.conv6x(:,1),sed.conv6x(:,3),'.'...
    )

%plot tau fs freq
loglog(...
    nmd.seddata.freq,nmd.seddata.life,'.',...
    nmd.seddata.freq,(1E2)*nmd.seddata.freq.^(-1.9),...
    nmd.seddata.freq,(2*pi)./nmd.seddata.freq...
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
loglog(...
    sed.prim(:,1),sed.prim(:,3),'.',...
    sed.conv(:,1),sed.conv(:,3),'.'...
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
%filter
%--------------------------------------------------------------------------
% I = find(...
%     
% loglog(...
%     sed.conv(:,1),sed.conv(:,3),'.')
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------


%--------------------------------------------------------------------------
%prim rand
%--------------------------------------------------------------------------
NUM_MODES=12;
for ikpt=1:size(nmd.seddata.irrkpt.life,2)
    prim.freq( (ikpt-1)*NUM_MODES +1 : ikpt*NUM_MODES , 1) =...
       nmd.seddata.irrkpt.HLDfreq(:,ikpt);
prim.life( (ikpt-1)*NUM_MODES +1 : ikpt*NUM_MODES , 1) =...
    nmd.seddata.irrkpt.life(:,ikpt) + ...
    0.05*nmd.seddata.irrkpt.life(:,ikpt).*...
    2.*(0.5 - rand(NUM_MODES,1));
end

loglog(...
    nmd.seddata.freq,nmd.seddata.life,'.',...
    prim.freq*lj.tau,prim.life/lj.tau,'.',...
    nmd.seddata.freq,(1E2)*nmd.seddata.freq.^(-1.9),...
    nmd.seddata.freq,(2*pi)./nmd.seddata.freq...
    )

%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------

gamma.freq = nmd.seddata.freq;
gamma.life = nmd.seddata.life + ...
    0.2*nmd.seddata.life.*...
    2.*(0.5 - rand(length(nmd.seddata.life),1));

subplot(2,1,1),...
loglog(...
    nmd.seddata.freq,nmd.seddata.life,'.',...
    prim.freq*lj.tau,prim.life/lj.tau,'.',...
    nmd.seddata.freq,(1E2)*nmd.seddata.freq.^(-1.9),...
    nmd.seddata.freq,(2*pi)./nmd.seddata.freq...
    )
subplot(2,1,2),...
loglog(...
    gamma.freq,gamma.life,'.',...
    nmd.seddata.freq,(1E2)*nmd.seddata.freq.^(-1.9),...
    nmd.seddata.freq,(2*pi)./nmd.seddata.freq...
    )




