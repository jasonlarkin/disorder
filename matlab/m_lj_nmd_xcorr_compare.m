clear

lj = m_lj;
c = m_constant;

str_nmd = '/home/jason/disorder2/lj/alloy/10K/0.5/10x/NMD/1/work/';

nmd.nmdfit_o = load([str_nmd 'NMDfit_orig.mat']);
nmd.sedfit_o = load([str_nmd 'SEDfit_orig.mat']);

nmd.nmddata_o = load([str_nmd 'NMDdata_orig.mat']);
nmd.seddata_o = load([str_nmd 'SEDdata_orig.mat']);

nmd.seddata_o = nmd_convert_data(nmd.nmddata_o,nmd.seddata_o);

nmd.nmddata = load([str_nmd 'NMDdata.mat']);
nmd.seddata = load([str_nmd 'SEDdata.mat']);



% for ikpt=50:size(nmd.seddata.irrkpt.sedavg,3)
%     for imode=1:size(nmd.seddata.irrkpt.sedavg,2)
% semilogy(...
%     nmd.seddata.omega(1:5000),nmd.seddata.irrkpt.sedavg(1:5000,imode,ikpt)...
%     )
% %pause
% imode
% ikpt
%     end
% end



[I,J] = find(...
    nmd.seddata.kpt(:,1)==1 & ...
    nmd.seddata.kpt(:,2)==0 & ...
    nmd.seddata.kpt(:,3)==0 )

loglog(...
    nmd.seddata.irrkpt.HLDfreq*lj.tau,nmd.seddata.irrkpt.life/lj.tau,'.',...
    nmd.seddata.freq,nmd.seddata.life,'.')

nmd.seddata.irrkpt.life(:,50)/lj.tau
nmd.seddata.life( I ) 

nmd.seddata.freq( I ) 
nmd.seddata.sedfreq( I ) 


%100
kpt(1) = 50; mode(1) = 3; kpt(2) = 50; mode(2) = 4;
semilogy(...
    nmd.seddata.omega(1:5000),nmd.seddata.irrkpt.sedavg(1:5000,mode(1),kpt(1)),...
    nmd.seddata.omega(1:5000),nmd.seddata.irrkpt.sedavg(1:5000,mode(2),kpt(2))...
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
dt = 0.002; tf = 100; N = ceil(tf/dt);
t = linspace(0,2*tf,N);
%get lifetimes
life(1) = nmd.seddata.irrkpt.life(3,50)/lj.tau;
freq(1) = nmd.seddata.sedfreq( I(3) );
life(2) = nmd.seddata.irrkpt.life(4,50)/lj.tau;
freq(2) = nmd.seddata.sedfreq( I(4) );



%1
q(1).q = exp(-abs(t - tf)/life(1)).*( cos(freq(1)*abs(t - tf)) );
q(1).qd = exp(-abs(t - tf)/life(1)).*( sin(freq(1)*abs(t - tf)) );
%2
q(2).q = exp(-abs(t- tf)/life(2)).*...
    (cos(freq(2)*abs(t- tf)) + cos(1.1*freq(2)*abs(t- tf)) );
q(2).qd = exp(-abs(t- tf)/life(2)).*...
    (sin(freq(2)*abs(t- tf)) + sin(1.1*freq(2)*abs(t- tf)) );

plot(...
    t, q(1).q , t , q(1).qd,...
    t, q(2).q, t , q(2).qd...
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
energy(1).V = q(1).q.^2; energy(1).T = q(1).qd.^2; 
energy(1).E = energy(1).V/max(energy(1).V) + energy(1).T/max(energy(1).T) 
energy(2).V = q(2).q.^2; energy(2).T = q(2).qd.^2; 
energy(2).E = energy(2).V/max(energy(2).V) + energy(2).T/max(energy(2).T) 

EE1 = xcorr(energy(1).E - mean(energy(1).E),'coeff');
TT1 = xcorr(energy(1).T/max(energy(1).T)  ,'coeff');
TV1 = xcorr(energy(1).T,energy(1).V ,'coeff');
plot(...
    t , TT1(ceil(length(t):length(TT1))),...
    t , EE1(ceil(length(t):length(EE1))),...
    t , TV1(ceil(length(t):length(TV1)))...
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
EE2 = xcorr(energy(2).E - mean(energy(2).E),'coeff');
TT2 = xcorr(energy(2).T/max(energy(2).T) - mean(energy(2).T/max(energy(2).T))  ,'coeff');
TV2 = xcorr(energy(2).T,energy(2).V ,'coeff');
plot(...
    t , TT2(ceil(length(t):length(TT2))),...
    t , EE2(ceil(length(t):length(EE2))),...
    t , TV2(ceil(length(t):length(TV2)))...
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------




pause

w = 0:0.0001:100;
w0(1) = 10; w0(2) = 50;



lor1 = 1 ./ ( (w - w0(1)).^2 + 1);
lor2 = 1 ./ ( (w - w0(2)).^2 + 1);

semilogy( w , lor1 + lor2 )

plot(1:length(lor1), real(fft(lor1+lor2)))


% exp(-t./life(1)).*exp(-1i*freq(1)*t).*...
%     exp(-1i*(freq(1)+freq(1)*0.05)*t)...


% str_nmd = '/home/jason/disorder2/lj/alloy/10K/0.5/10x/NMD/1/work/';
% nmd.nmdfit = load([str_nmd 'NMDfit_orig.mat']);
% nmd.sedfit = load([str_nmd 'SEDfit_orig.mat']);
% 
% str_nmdaf = '/home/jason/disorder2/lj/alloy/10K/0.5/10x/NMD_AF/1/work/';
% nmdaf.nmdfit = load([str_nmdaf 'NMDfit_orig.mat']);
% nmdaf.sedfit = load([str_nmdaf 'SEDfit_orig.mat']);
% str_xcorr = '/home/jason/disorder2/lj/alloy/10K/0.5/10x/XCORR_AF/1/work/';
% xcorr.nmdfit = load([str_xcorr 'NMDfit.mat']);
% xcorr.sedfit = load([str_xcorr 'SEDfit.mat']);
% 
% loglog(...
%     nmdaf.sedfit.HLDfreq,nmdaf.sedfit.life,'.',...
%     xcorr.sedfit.HLDfreq,xcorr.sedfit.life,'.'...
%     )
% 
% pause
% 
% 
% nmd.nmdavg = load([str_nmd 'NMDavg_orig.mat']);
% nmd.sedavg = load([str_nmd 'SEDavg_orig.mat']);
% 
% nmdaf.nmdavg = load([str_nmdaf 'NMDavg_orig.mat']);
% nmdaf.sedavg = load([str_nmdaf 'SEDavg_orig.mat']);
% 
% xcorr.nmdavg = load([str_xcorr 'NMDavg.mat']);
% xcorr.sedavg = load([str_xcorr 'SEDavg.mat']);
% 
% for imode=1000:1:size(nmdaf.sedavg.sed(:,:),2)
% subplot(3,1,1),...
% semilogy(...
%     nmdaf.sedavg.omega(1:5000),nmd.sedavg.irrkpt.sedavg(1:5000,7,8)...
%     )
% subplot(3,1,2),...
% semilogy(...
%     nmdaf.sedavg.omega(1:2500),nmdaf.sedavg.sed(1:2500,imode)...
%     )
% subplot(3,1,3),...
%     plot(...
%     xcorr.sedavg.omega(1:200),xcorr.sedavg.sed(1:200,imode)...
%     )
% nmdaf.sedfit.HLDfreq(imode)
% nmdaf.sedfit.life(imode)
% imode
% pause
% end
% 
% %create
% 
% for ikpt=1:size(nmd.sedavg.irrkpt.sedavg,3)
%     for imode=1:size(nmd.sedavg.irrkpt.sedavg,2)
% 
% semilogy(...
%     nmdaf.sedavg.omega(1:5000),nmd.sedavg.irrkpt.sedavg(1:5000,imode,ikpt)...
%     )
% pause
% 
% imode
% ikpt
%     end
% end

% for imode=1000:1:size(nmdaf.sedavg.sed(:,:),2)
% subplot(3,1,1),...
% semilogy(...
%     nmdaf.sedavg.omega(1:5000),nmd.sedavg.irrkpt.sedavg(1:5000,7,8)...
%     )
% subplot(3,1,2),...
% semilogy(...
%     nmdaf.sedavg.omega(1:2500),nmdaf.sedavg.sed(1:2500,imode)...
%     )
% subplot(3,1,3),...
%     plot(...
%     xcorr.sedavg.omega(1:200),xcorr.sedavg.sed(1:200,imode)...
%     )
% nmdaf.sedfit.HLDfreq(imode)
% nmdaf.sedfit.life(imode)
% imode
% pause
% end



