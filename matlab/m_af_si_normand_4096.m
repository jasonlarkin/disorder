clear
con = m_constant; si = m_si;

%si
DSF(1).DSF =...
    load('/home/jason/disorder2/si/amor/normand/perf4096/anneal_1100K/emin/DSF_FIT_b10.mat');
DSF(2).DSF =...
    load('/home/jason/disorder2/si/amor/normand/perf4096/anneal_1100K/emin/dsf/nmd/DSF_FIT.mat');
SED =...
    load('/home/jason/disorder2/si/amor/normand/perf4096/anneal_1100K/emin/SEDfit_fixed.mat');

Di(8,1).Di =...
    m_gulp_af_si_readDi(...
    '/home/jason/disorder2/si/amor/normand/perf4096/anneal_1100K/emin/','Di_b10.gout');
Di(8,1).Di(:,3) = Di(8,1).Di(:,3)*(3/4)/10;
VOLUME = (8*5.43E-10)^3;
cond(8,1) = (con.kb / VOLUME)*sum(Di(8,1).Di(:,3));
[Di(8,1).dosx Di(8,1).dosy] = m_dos(Di(8,1).Di(:,2), 220 , 1);

Di(8,5).Di =...
    m_gulp_af_si_readDi(...
    '/home/jason/disorder2/si/amor/normand/perf4096/anneal_1100K/emin/','Di_b25.gout');
Di(8,5).Di(:,3) = Di(8,5).Di(:,3)*(3/4)/10;
VOLUME = (8*5.43E-10)^3;
cond(8,5) = (con.kb / VOLUME)*sum(Di(8,5).Di(:,3));
[Di(8,5).dosx Di(8,5).dosy] = m_dos(Di(8,5).Di(:,2), 220 , 1);

Di(8,10).Di =...
    m_gulp_af_si_readDi(...
    '/home/jason/disorder2/si/amor/normand/perf4096/anneal_1100K/emin/','Di_b75.gout');
Di(8,10).Di(:,3) = Di(8,10).Di(:,3)*(3/4)/10;
VOLUME = (8*5.43E-10)^3;
cond(8,10) = (con.kb / VOLUME)*sum(Di(8,10).Di(:,3));
[Di(8,10).dosx Di(8,10).dosy] = m_dos(Di(8,10).Di(:,2), 220 , 1);

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

%sio2
D(1).D = m_gulp_af_si_readDi('/home/jason/disorder2/sio2/alan/a972/','Di.gout');
D(1).D(:,3) = D(1).D(:,3)*(3/4)/10;
VOLUME = (24.5E-10)^3;
sio2.cond(1) = (con.kb / VOLUME)*sum(D(1).D(:,3));
[D(1).dosx D(1).dosy] = m_dos(D(1).D(:,2), 100 , 1);
D(2).D = m_gulp_af_si_readDi('/home/jason/disorder2/sio2/alan/a972/','Di_b10.gout');
D(2).D(:,3) = D(2).D(:,3)*(3/4)/10;
VOLUME = (24.5E-10)^3;
sio2.cond(2) = (con.kb / VOLUME)*sum(D(2).D(:,3));

%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------

%Di
%subplot(2,1,1),...
% clf
% plotyy(...
%     SED.HLDfreq , ((1/3))*si.amor.vs_tran^2*(SED.life*1E-12),...
%     Di(8,1).dosx,Di(8,1).dosy./((Di(8,1).dosx*1E-12).^2),...
%     'loglog') 
% %axis([3E12 2E14 1E-8 1E-4])
% hold on
% plotyy(...
%     Di(8,1).Di(:,2),Di(8,1).Di(:,3),...
%     Di(8,1).dosx,Di(8,1).dosy./((Di(8,1).dosx*1E-12).^2),...
%     'loglog')
% hold on
% plotyy(...
%     DSF(1).DSF.freq(1:8)*1E12,((1/3))*si.amor.vs_tran^2*(DSF(1).DSF.life(1:8)*1E-12),...
%     Di(8,1).dosx,Di(8,1).dosy./((Di(8,1).dosx*1E-12).^2),...
%     'loglog') 
% hold on
% plotyy(...
%     DSF(1).DSF.freq(9:end)*1E12,((1/3))*si.amor.vs_tran^2*(DSF(1).DSF.life(9:end)*1E-12),...
%     Di(8,1).dosx,Di(8,1).dosy./((Di(8,1).dosx*1E-12).^2),...
%     'loglog') 
% hold on
% plotyy(...
%     DSF(2).DSF.freq*1E12,((1/3))*si.amor.vs_tran^2*(DSF(2).DSF.life*1E-12),...
%     Di(8,1).dosx,Di(8,10).dosy./((Di(8,1).dosx*1E-12).^2),...
%     'loglog') 
% pause

subplot(2,1,1),...
loglog(...
    Di(8,1).Di(:,2),Di(8,1).Di(:,3),...
    SED.HLDfreq,((1/3))*si.amor.vs_tran^2*(SED.life*1E-12),...
    DSF(1).DSF.freq(1:8)*1E12,((1/3))*si.amor.vs_tran^2*(DSF(1).DSF.life(1:8)*1E-12),...
    DSF(1).DSF.freq(9:16)*1E12,((1/3))*si.amor.vs_tran^2*(DSF(1).DSF.life(9:16)*1E-12),...
    DSF(2).DSF.freq*1E12,((1/3))*si.amor.vs_tran^2*(DSF(2).DSF.life*1E-12),...
    linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100),...
    1E21*linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100).^(-2),...
    linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100),...
    1E12*2*pi./linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100)...
    )
axis([ 3E12 4E14 1.0E-8 6.0E-5 ])
subplot(2,1,2),...
    loglog(...
    Di(8,10).dosx,Di(8,10).dosy...
    )
axis([ 3E12 4E14 0 0.1 ])
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
subplot(2,1,1),...
loglog(...
    Di(8,1).Di(:,2),Di(8,1).Di(:,3),...
    D(1).D(:,2),D(1).D(:,3),...
    linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100),...
    1E21*linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100).^(-2),...
    linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100),...
    1E12*2*pi./linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100)...
    )
axis([ 3E12 4E14 1.0E-8 6.0E-5 ])
subplot(2,1,2),...
    loglog(...
    Di(8,10).dosx,Di(8,10).dosy,...
    D(1).dosx,D(1).dosy....
    )
axis([ 3E12 4E14 0 0.1 ])
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------


%tau
%subplot(2,1,1),...
loglog(...
    Di(8,1).Di(:,2),3*Di(8,1).Di(:,3)/(si.amor.vs^2),...
    SED.HLDfreq,(SED.life*1E-12),...
    DSF(1).DSF.freq*1E12,(DSF(1).DSF.life*1E-12),...
    DSF(2).DSF.freq*1E12,(DSF(2).DSF.life*1E-12),...
    linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100),...
    1E14*linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100).^(-2),...
    linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100),...
    2*pi./linspace(min(Di(8,10).Di(:,2)),max(Di(8,10).Di(:,2)),100)...
    )
%axis([ 4E12 2E14 1.0E-13 3.0E-10 ])
%subplot(2,1,2),...
%    loglog(...
%    Di(8,10).dosx,Di(8,10).dosy./((Di(8,10).dosx*1E-12).^2)...
%    )
%ax;is([ 4E12 2E14 0 0.01 ])
pause

%tau
%subplot(2,1,1),...
loglog(...
    Di(8,1).Di(:,2),sqrt(3*Di(8,1).Di(:,3)'./(SED.life(5:end)*1E-12)),...
    linspace(min(Di(8,1).Di(:,2)),max(Di(8,1).Di(:,2)),100),...
    si.amor.vs_tran*ones(1,100)...
    )
%axis([ 4E12 2E14 1.0E-13 3.0E-10 ])
%subplot(2,1,2),...
%    loglog(...
%    Di(8,10).dosx,Di(8,10).dosy./((Di(8,10).dosx*1E-12).^2)...
%    )
%ax;is([ 4E12 2E14 0 0.01 ])

pause

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



