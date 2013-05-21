clear
con = m_constant; si = m_si;

%si
DSF(1).DSF =...
    load('/home/jason/disorder2/si/amor/normand/perf4096/anneal_1100K/emin/DSF_FIT_b10.mat');
DSF(2).DSF =...
    load('/home/jason/disorder2/si/amor/normand/perf4096/anneal_1100K/emin/dsf/nmd/DSF_FIT.mat');

SED(1).SED =...
    load('/home/jason/disorder2/si/amor/normand/perf4096/anneal_1100K/emin/SEDfit_fixed.mat');
SED(2).SED =...
    load('/home/jason/disorder2/si/amor/normand/perf4096/anneal_1100K/emin/SEDfit_fixed.mat');

Di(8,1).Di =...
    m_gulp_af_si_readDi(...
    '/home/jason/disorder2/si/amor/normand/perf4096/anneal_1100K/emin/','Di_b5.gout');
Di(8,1).Di(:,3) = Di(8,1).Di(:,3)*(3/4)/10;
VOLUME = (8*5.43E-10)^3;
cond(8,1) = (con.kb / VOLUME)*sum(Di(8,1).Di(:,3));
[Di(8,1).dosx Di(8,1).dosy] = m_dos(Di(8,1).Di(:,2), 55 , 1, VOLUME);

Di(8,5).Di =...
    m_gulp_af_si_readDi(...
    '/home/jason/disorder2/si/amor/normand/perf4096/anneal_1100K/emin/','Di_b25.gout');
Di(8,5).Di(:,3) = Di(8,5).Di(:,3)*(3/4)/10;
VOLUME = (8*5.43E-10)^3;
cond(8,5) = (con.kb / VOLUME)*sum(Di(8,5).Di(:,3));
[Di(8,5).dosx Di(8,5).dosy] = m_dos(Di(8,5).Di(:,2), 100 , 1, VOLUME);

Di(8,10).Di =...
    m_gulp_af_si_readDi(...
    '/home/jason/disorder2/si/amor/normand/perf4096/anneal_1100K/emin/','Di_b75.gout');
Di(8,10).Di(:,3) = Di(8,10).Di(:,3)*(3/4)/10;
VOLUME = (8*5.43E-10)^3;
cond(8,10) = (con.kb / VOLUME)*sum(Di(8,10).Di(:,3));
[Di(8,10).dosx Di(8,10).dosy] = m_dos(Di(8,10).Di(:,2), 150 , 1, VOLUME);

Di(8,11).Di =...
    m_gulp_af_si_readDi(...
    '/home/jason/disorder2/si/amor/normand/perf4096/anneal_1100K/emin/','Di_b100.gout');
Di(8,11).Di(:,3) = Di(8,11).Di(:,3)*(3/4)/10;
VOLUME = (8*5.43E-10)^3;
cond(8,11) = (con.kb / VOLUME)*sum(Di(8,11).Di(:,3));

Di(8,12).Di =...
    m_gulp_af_si_readDi(...
    '/home/jason/disorder2/si/amor/normand/perf4096/anneal_1100K/emin/','Di_b200.gout');
Di(8,12).Di(:,3) = Di(8,12).Di(:,3)*(3/4)/10;
VOLUME = (8*5.43E-10)^3;
cond(8,12) = (con.kb / VOLUME)*sum(Di(8,12).Di(:,3));

Di(8,13).Di =...
    m_gulp_af_si_readDi(...
    '/home/jason/disorder2/si/amor/normand/perf4096/anneal_1100K/emin/','Di_b300.gout');
Di(8,13).Di(:,3) = Di(8,13).Di(:,3)*(3/4)/10;
VOLUME = (8*5.43E-10)^3;
cond(8,13) = (con.kb / VOLUME)*sum(Di(8,13).Di(:,3));

Di(8,14).Di =...
    m_gulp_af_si_readDi(...
    '/home/jason/disorder2/si/amor/normand/perf4096/anneal_1100K/emin/','Di_b400.gout');
Di(8,14).Di(:,3) = Di(8,14).Di(:,3)*(3/4)/10;
VOLUME = (8*5.43E-10)^3;
cond(8,14) = (con.kb / VOLUME)*sum(Di(8,14).Di(:,3));

%--------------------------------------------------------------------------
%sio2
%--------------------------------------------------------------------------

siO2 = m_sio2

sio2.D(1).D = m_gulp_af_si_readDi('/home/jason/disorder2/sio2/alan/a972/emin/','Di_b1.gout');
sio2.D(1).D(:,3) = sio2.D(1).D(:,3)*(3/4)/10;
sio2.VOLUME = (24.5E-10)^3;
sio2.cond(1) = (con.kb / sio2.VOLUME)*sum(sio2.D(1).D(:,3));
[sio2.D(1).dosx sio2.D(1).dosy] = m_dos(sio2.D(1).D(:,2), 100 , 1, sio2.VOLUME);
sio2.D(2).D = m_gulp_af_si_readDi('/home/jason/disorder2/sio2/alan/a972/emin/','Di_b2.gout');
sio2.D(2).D(:,3) = sio2.D(2).D(:,3)*(3/4)/10;
sio2.cond(2) = (con.kb / sio2.VOLUME)*sum(sio2.D(2).D(:,3));
sio2.D(3).D = m_gulp_af_si_readDi('/home/jason/disorder2/sio2/alan/a972/emin/','Di_b3.gout');
sio2.D(3).D(:,3) = sio2.D(3).D(:,3)*(3/4)/10;
sio2.cond(3) = (con.kb / sio2.VOLUME)*sum(sio2.D(3).D(:,3));
sio2.D(4).D = m_gulp_af_si_readDi('/home/jason/disorder2/sio2/alan/a972/emin/','Di_b4.gout');
sio2.D(4).D(:,3) = sio2.D(4).D(:,3)*(3/4)/10;
sio2.cond(4) = (con.kb / sio2.VOLUME)*sum(sio2.D(4).D(:,3));
sio2.D(5).D = m_gulp_af_si_readDi('/home/jason/disorder2/sio2/alan/a972/emin/','Di_b5.gout');
sio2.D(5).D(:,3) = sio2.D(5).D(:,3)*(3/4)/10;
sio2.cond(5) = (con.kb / sio2.VOLUME)*sum(sio2.D(5).D(:,3));

loglog(...
sio2.D(1).D(:,2),sio2.D(1).D(:,3),'.',...
sio2.D(4).D(:,2),sio2.D(4).D(:,3),'.'...
)

%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------

sio2.D(1,2).D = m_gulp_af_si_readDi('/home/jason/disorder2/sio2/alan/a288/tile/emin/','Di_b1.gout');
sio2.D(1,2).D(:,3) = sio2.D(1,2).D(:,3)*(3/4)/10;
sio2.cond(1,2) = (con.kb / sio2.VOLUME)*sum(sio2.D(1,2).D(:,3));

loglog(...
    sio2.D(1).D(:,2),sio2.D(1).D(:,3),'.',...
    sio2.D(1,2).D(:,2),sio2.D(1,2).D(:,3),'.'...
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
sio2.SED(1).SED =...
    load('/home/jason/disorder2/sio2/alan/a288/tile/emin/SEDfit_inv1.mat');
sio2.SED(2).SED =...
    load('/home/jason/disorder2/sio2/alan/a288/tile/emin/SEDfit_inv2.mat');
sio2.SED(4).SED =...
    load('/home/jason/disorder2/sio2/alan/a288/tile/emin/SEDfit_inv4.mat');

sio2.SED(1,2).SED =...
    load('/home/jason/disorder2/sio2/alan/a972/emin/SEDfit.mat');

loglog(...
    sio2.SED(1).SED.HLDfreq(12:end),sio2.SED(1).SED.life(12:end)*1e-12,'.',...
    sio2.SED(2).SED.HLDfreq(12:end),sio2.SED(2).SED.life(12:end)*1e-12,'.',...
    sio2.SED(4).SED.HLDfreq(12:end),sio2.SED(4).SED.life(12:end)*1e-12,'.',...
    sio2.SED(1,2).SED.HLDfreq(12:end),sio2.SED(1,2).SED.life(12:end)*1e-12,'.',...
    sio2.SED(4).SED.HLDfreq(12:end),2*pi./sio2.SED(4).SED.HLDfreq(12:end)...
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
loglog(...
    sio2.SED(1).SED.HLDfreq(12:end),sio2.SED(1).SED.life(12:end)*1e-12,'.',...
    sio2.D(1).D(:,2),sio2.D(1).D(:,3),'.'...
    )


%--------------------------------------------------------------------------
%debye
%--------------------------------------------------------------------------

% k = \int_{0}^{w_{max}} \frac{d\omega DOS(\omega)C(\omega/T)D(\omega)}{V}
% units: W/m-K = [ rads/s ] [ () ] [ J/K ] [ m^2/s ][ 1/m^3 ]
% units: DOS = [ 1 / rads/s ]

debye.num = 100;
debye.A=1E-29;
debye.B=1.3E21;
debye.c = (1/3)*si.amor.vs_long + (2/3)*si.amor.vs_tran;
debye.wmin = min(Di(8,1).Di(:,2)); %4.113E12
debye.wcut = 1.1625E13; 
debye.dw = debye.wcut/debye.num;
debye.freq_range = linspace(debye.wcut/debye.num,debye.wcut,debye.num);
debye.dos = 3*(debye.freq_range.^2)/(2*(0.93*si.amor.vs_tran)^3*pi^2)*VOLUME;
debye.cond = (con.kb/VOLUME)*(debye.dos*debye.dw)*debye.B.*(debye.freq_range.^(-2));
debye.mfp = 3*debye.B.*(debye.freq_range.^(-2))/si.amor.vs_tran;
[Y debye.Isort] = sort(debye.mfp);
sum(debye.cond)

%--------------------------------------------------------------------------
T = 300;
%--------------------------------------------------------------------------

%matth
matth.T0 = 20; matth.dres = 1.7E-4; matth.drel = 2.5E-4;
matth.Dres = 1./(   (1/matth.dres) * (con.hbar*Di(8,1).Di(:,2) / (con.kb * matth.T0) ) .*...
    tanh( con.hbar*Di(8,1).Di(:,2) / (2 * con.kb * T) )   );
matth.Drel = 1./(   (1/matth.drel) * ( (T/matth.T0)^3 ) ./...
    ( 1 + (con.kb*matth.T0 ./ (con.hbar * Di(8,1).Di(:,2) )) * ( (T/matth.T0)^3 ) )   ); 

%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------

% %Di with SF, DSF
% subplot(2,1,1),...
% loglog(...
%     Di(8,1).Di(:,2),Di(8,1).Di(:,3),...
%     SED.HLDfreq,((1/3))*si.amor.vs_tran^2*(SED.life*1E-12),...
%     DSF(1).DSF.freq(1:8)*1E12,((1/3))*si.amor.vs_tran^2*(DSF(1).DSF.life(1:8)*1E-12),...
%     DSF(1).DSF.freq(9:16)*1E12,((1/3))*si.amor.vs_tran^2*(DSF(1).DSF.life(9:16)*1E-12),...
%     DSF(2).DSF.freq*1E12,((1/3))*si.amor.vs_tran^2*(DSF(2).DSF.life*1E-12),...
%     linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100),...
%     1E21*linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100).^(-2),...
%     linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100),...
%     1E12*2*pi./linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100)...
%     )
% axis([ 3E12 4E14 1.0E-8 6.0E-5 ])
% subplot(2,1,2),...
%     loglog(...
%     Di(8,10).dosx,Di(8,10).dosy...
%     )
% axis([ 3E12 4E14 0 0.1 ])
%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------

% %si vs sio2
% subplot(2,1,1),...
% loglog(...
%     Di(8,1).Di(:,2),Di(8,1).Di(:,3),...
%     D(1).D(:,2),D(1).D(:,3),...
%     linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100),...
%     1E21*linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100).^(-2),...
%     linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100),...
%     1E12*2*pi./linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100)...
%     )
% axis([ 3E12 4E14 1.0E-8 6.0E-5 ])
% subplot(2,1,2),...
%     loglog(...
%     Di(8,10).dosx,Di(8,10).dosy,...
%     D(1).dosx,D(1).dosy....
%     )
% axis([ 3E12 4E14 0 0.1 ])
% %--------------------------------------------------------------------------
% %pause
% %--------------------------------------------------------------------------


%sed
SF_long=load('/home/jason/disorder2/si/amor/normand/perf4096/anneal_1100K/emin/DSF_tran_b10.mat');
DSF_long = load('/home/jason/disorder2/si/amor/normand/perf4096/anneal_1100K/emin/dsf/nmd/DSF.mat');
SEDavg = load('/home/jason/disorder2/si/amor/normand/perf4096/anneal_1100K/emin/SEDavg.mat');
% for ikpt =1:8
% semilogy(...
%     SF_long.freq_range/1e12,SF_long.SL(:,ikpt),...
%     DSF_long.freq_range,smooth(DSF_long.SL(:,ikpt),7)...
%     )
% pause
% end
semilogy(...
    SF_long.freq_range(100:700)/1e12,SF_long.SL(100:700,1),...
    DSF_long.freq_range(50:300),1E2*smooth(DSF_long.SL(50:300,1),7),...
    SEDavg.omega,1.5E-13*smooth(SEDavg.sed(:,10),7),...
    SF_long.freq_range(100:end)/1e12,SF_long.SL(100:end,2),...
    DSF_long.freq_range(50:end),3.5E2*smooth(DSF_long.SL(50:end,2),7),...
    SEDavg.omega,8E-13*smooth(SEDavg.sed(:,96),7)...
    )
%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------

% %subplot(2,1,1),...
% loglog(...
%     Di(8,1).Di(:,2),matth.Dres,...
%     Di(8,1).Di(:,2),matth.Drel,...
%     Di(8,1).Di(:,2),Di(8,1).Di(:,3),...
%     SED.HLDfreq,((1/3))*debye.c^2*(SED.life*1E-12),...
%     DSF(1).DSF.freq(1:8)*1E12,((1/3))*si.amor.vs_long^2*(DSF(1).DSF.life(1:8)*1E-12),...
%     DSF(1).DSF.freq(9:16)*1E12,((1/3))*si.amor.vs_tran^2*(DSF(1).DSF.life(9:16)*1E-12),...
%     linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100),...
%     debye.B*linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100).^(-2),...
%     linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100),...
%     1E12*2*pi./linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100),...
%     linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100),...
%     (1/3)*debye.c*(5.43E-10/2)*ones(1,100)...
%     )
% %axis([ 3E12 4E14 1.0E-8 6.0E-5 ])
% %--------------------------------------------------------------------------
% pause
% %--------------------------------------------------------------------------

%DOS
%subplot(3,1,3),...
    loglog(...
    Di(8,5).dosx,Di(8,5).dosy*(4096*3)/(Di(8,5).dosx(end)/length(Di(8,5).dosx)),...
    Di(8,10).dosx,Di(8,10).dosy*(4096*3)/(Di(8,10).dosx(end)/length(Di(8,10).dosx)),...
    debye.freq_range,debye.dos...
    )
axis([ 3E12 1.3E14 1E-12 1E-9])
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------

%tau
%subplot(2,1,1),...
loglog(...
    SED.HLDfreq,(SED.life*1E-12),...
    DSF(1).DSF.freq(1:8)*1E12,(DSF(1).DSF.life(1:8)*1E-12),...
    DSF(1).DSF.freq(9:16)*1E12,(DSF(1).DSF.life(9:16)*1E-12),...
    linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100),...
    2*pi./linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100)...
    )
axis([ 3E12 1.3E14 3E-14 1E-11])
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
%subplot(2,1,2),...
%Di
loglog(...
    SED.HLDfreq,((1/3))*si.amor.vs_tran^2*(1.0*SED.life*1E-12),...
    Di(8,1).Di(:,2),Di(8,1).Di(:,3),...
    linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100),...
    (1/3)*(5.43E-10/2)*si.amor.vs_tran*ones(100,1),...
    linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100),...
    debye.B*linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100).^(-2)...
    )
axis([ 3E12 1.3E14 1E-8 1E-4])
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
clf
%subplot(2,1,2),...
%Di
loglog(...
    SED.HLDfreq,((1/3))*si.amor.vs_tran^2*(1.0*SED.life*1E-12),...
    Di(8,1).Di(:,2),Di(8,1).Di(:,3),...
    linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100),...
    (1/3)*(5.43E-10/2)*si.amor.vs_tran*ones(100,1),...
    linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100),...
    debye.B*linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100).^(-2)...
    )
axis([ 3E12 1.3E14 1E-8 1E-4])
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
clf

% %SED/AF
% %subplot(2,1,1),...
% loglog(...
%     Di(8,1).Di(:,2),sqrt(3*Di(8,1).Di(:,3)'./(SED.life(5:end)*1E-12)),...
%     linspace(min(Di(8,1).Di(:,2)),max(Di(8,1).Di(:,2)),100),...
%     si.amor.vs_tran*ones(1,100)...
%     )
% %subplot(3,1,2),...
% loglog(...
%     Di(8,1).Di(:,2),Di(8,1).Di(:,3)./(1.0*((1/3))*si.amor.vs_tran^2*(SED.life(5:end)'*1E-12))...
%     )
% %--------------------------------------------------------------------------
% %pause
% %--------------------------------------------------------------------------

%mfp
%subplot(3,1,1),...
    loglog(...
    Di(8,1).Di(:,2),3*Di(8,1).Di(:,3)/si.amor.vs_tran,...
    SED.HLDfreq,si.amor.vs_tran*(1.0*SED.life*1E-12),...
    linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100),...
    (8*5.43E-10)*ones(100,1),...
    linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100),...
    (5.43E-10/2)*ones(100,1)...
    )
axis([ 3E12 1.3E14 1E-10 1E-7])
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
%vAF
%subplot(3,1,1),...
    loglog(...
    Di(8,1).Di(:,2),3*Di(8,1).Di(:,3)/si.amor.vs_tran,...
    SED.HLDfreq,si.amor.vs_tran*(1.0*SED.life*1E-12),...
    linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100),...
    (8*5.43E-10)*ones(100,1),...
    linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100),...
    (5.43E-10/2)*ones(100,1)...
    )
axis([ 3E12 1.3E14 1E-10 1E-7])
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------



%wcut
[debye.Icut1 debye.Jcut1] = find( Di(8,1).Di(:,2) < debye.wcut + 0.0001E12);
[debye.Icut2 debye.Jcut2] = find( Di(8,1).Di(:,2) > debye.wcut - 0.0001E12);
[SED.Icut1 SED.Jcut1] = find( SED.HLDfreq < 0.0 + 0.0001E12);
[SED.Icut2 SED.Jcut2] = find( SED.HLDfreq > 0.0 - 0.0001E12);
VOLUME = (8*5.43E-10)^3;
debye.cut(1).cond = (con.kb / VOLUME)*Di(8,1).Di(debye.Icut1,3);
debye.cut(2).mfp = 3*Di(8,1).Di(debye.Icut1,3)/si.amor.vs_tran;
sum(debye.cut(1).cond)
debye.cut(2).cond = (con.kb / VOLUME)*Di(8,1).Di(debye.Icut2,3);
debye.cut(2).mfp = 3*Di(8,1).Di(debye.Icut2,3)/si.amor.vs_tran;
[Y debye.cut(2).Isort] = sort(debye.cut(2).mfp);
sum(debye.cut(2).cond)

SED.cut(1).cond = (con.kb / VOLUME)*((1/3))*si.amor.vs_tran^2*(1.0*SED.life(SED.Jcut1)*1E-12)';
SED.cut(1).mfp = si.amor.vs_tran*SED.life(SED.Jcut1)*1E-12';
[Y SED.cut(1).Isort] = sort(SED.cut(1).mfp);

SED.cut(2).cond = (con.kb / VOLUME)*((1/3))*si.amor.vs_tran^2*(1.0*SED.life(SED.Jcut2)*1E-12)';
SED.cut(2).mfp = si.amor.vs_tran*SED.life(SED.Jcut2)*1E-12';
[Y SED.cut(2).Isort] = sort(SED.cut(2).mfp);

sum(SED.cut(1).cond)
sum(SED.cut(2).cond)

%semilogx(Di(8,1).Di(:,2),cumtrapz((con.kb / VOLUME)*Di(8,1).Di(:,3) ),'.')
%semilogx(flipdim(debye.mfp,2),sum(debye.cond) + sum(SED.cut(1).cond)+cumtrapz(debye.cond))
%semilogx( flipdim(SED.cut(1).mfp'), cumtrapz(SED.cut(1).cond))

%Di
loglog(...
    SED.HLDfreq,((1/3))*si.amor.vs_tran^2*(1.0*SED.life*1E-12),...
    Di(8,1).Di(debye.Icut2,2),Di(8,1).Di(debye.Icut2,3),...
    linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100),...
    (1/3)*(5.43E-10/2)*si.amor.vs_tran*ones(100,1),...
    linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100),...
    debye.B*linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100).^(-2)...
    )
axis([ 3E12 1.3E14 1E-8 1E-4])
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------

regner.cond(1).cond = load('/home/jason/disorder/si/amor/regner/500nm.dat');
regner.cond(2).cond = load('/home/jason/disorder/si/amor/regner/1000nm.dat');
regner.cond(3).cond = load('/home/jason/disorder/si/amor/regner/2000nm.dat');

semilogx(...
    debye.cut(2).mfp(debye.cut(2).Isort) , cumtrapz(debye.cut(2).cond(debye.cut(2).Isort)),...
    SED.cut(1).mfp(SED.cut(1).Isort) , sum(debye.cut(2).cond) + cumtrapz(SED.cut(1).cond(SED.cut(1).Isort)),...
    debye.mfp(debye.Isort) , sum(debye.cut(2).cond) + sum(SED.cut(1).cond)+cumtrapz(debye.cond(debye.Isort)),...
    (8*5.43E-10)*ones(1,100) , linspace(0,2,100),...
    ((5.43/2)*1E-10)*ones(1,100) , linspace(0,2,100),...
    regner.cond(1).cond(:,1)*1e-6 , regner.cond(1).cond(:,2)*1.7,...
    regner.cond(2).cond(:,1)*1e-6 , regner.cond(2).cond(:,2)*1.7,...
    regner.cond(3).cond(:,1)*1e-6 , regner.cond(3).cond(:,2)*1.7... 
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
