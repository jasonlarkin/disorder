clear
constant = m_constant; lj = m_lj;

str.main = '/home/jason/disorder2/lj/alloy/10K/0.05/12x/NMD/1/work/';
tamura(1).tamura = load( [str.main 'tamura_1000_m1.1_c0.01.mat'] );
tamura(2).tamura = load( [str.main 'tamura_1000.mat'] );
str.main = '/home/jason/disorder2/lj/alloy/10K/0.15/12x/NMD/1/work/';
tamura(3).tamura = load( [str.main 'tamura_1000.mat'] );
str.main = '/home/jason/disorder2/lj/alloy/10K/0.5/12x/NMD/1/work/';
tamura(4).tamura = load( [str.main 'tamura_1000.mat'] );

str.main = '/home/jason/disorder2/si/alloy/0.05/24x/';
tamura(5).tamura = load( [str.main 'tamura_1000_m1.1_c0.01.mat'] );
tamura(6).tamura = load( [str.main 'tamura_1000.mat'] );
str.main = '/home/jason/disorder2/si/alloy/0.5/24x/';
tamura(7).tamura = load( [str.main 'tamura_1000.mat'] );

%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
subplot(3,1,1),...
semilogx(...
    tamura(1).tamura.dosx*sqrt(1.1),tamura(1).tamura.dosy,...
    tamura(2).tamura.dosx,tamura(2).tamura.dosy,...
    tamura(4).tamura.dosx,tamura(4).tamura.dosy...
    )
subplot(3,1,2),...
loglog(...
    tamura(1).tamura.freq_range*sqrt(1.1),abs(tamura(1).tamura.dw),'.',...
    tamura(2).tamura.freq_range,abs(tamura(2).tamura.dw),'.',...
    tamura(4).tamura.freq_range,abs(tamura(4).tamura.dw),'.'...
    )
subplot(3,1,3),...
loglog(...    
    tamura(1).tamura.freq_range*sqrt(1.1),abs(tamura(1).tamura.g3Iwg2),'.',...
    tamura(1).tamura.freq_range*sqrt(1.1),abs(tamura(1).tamura.g4Iwg2),'.',...
    tamura(2).tamura.freq_range,abs(tamura(2).tamura.g3Iwg2),'.',...
    tamura(2).tamura.freq_range,abs(tamura(2).tamura.g4Iwg2),'.',...
    tamura(4).tamura.freq_range,abs(tamura(4).tamura.g3Iwg2),'.',...
    tamura(4).tamura.freq_range,abs(tamura(4).tamura.g4Iwg2),'.'...
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
subplot(3,1,1),...
semilogx(...
    tamura(5).tamura.dosx*sqrt(1.08),tamura(5).tamura.dosy,...
    tamura(6).tamura.dosx,tamura(6).tamura.dosy,...
    tamura(7).tamura.dosx,tamura(7).tamura.dosy...
    )
subplot(3,1,2),...
loglog(...
    tamura(5).tamura.freq_range,abs(tamura(5).tamura.dw),'.',...
    tamura(6).tamura.freq_range,abs(tamura(6).tamura.dw),'.',...
    tamura(7).tamura.freq_range,abs(tamura(7).tamura.dw),'.'...
    )
subplot(3,1,3),...
loglog(...    
    tamura(5).tamura.freq_range,abs(tamura(5).tamura.g3Iwg2),'.',...
    tamura(5).tamura.freq_range,abs(tamura(5).tamura.g4Iwg2),'.',...
    tamura(6).tamura.freq_range,abs(tamura(6).tamura.g3Iwg2),'.',...
    tamura(6).tamura.freq_range,abs(tamura(6).tamura.g4Iwg2),'.',...
    tamura(7).tamura.freq_range,abs(tamura(7).tamura.g3Iwg2),'.',...
    tamura(7).tamura.freq_range,abs(tamura(7).tamura.g4Iwg2),'.'...
    )

% m_alloy_tamura_highorder_compare_createfigure(...
%     X1, Y1, X2, X3, X4, Y2, X5, Y3, X6, Y4, YMatrix1, YMatrix2, YMatrix3)
% 
% m_alloy_tamura_highorder_compare_createfigure(...
%     tamura(5).tamura.dosx*sqrt(1.08),...
%     tamura(5).tamura.dosy,...
%     tamura(6).tamura.dosx,...
%     tamura(6).tamura.dosy,...
%     tamura(7).tamura.dosx,...
%     tamura(7).tamura.dosy,...
%     X5,...
%     Y3,...
%     X6,...
%     Y4,...
%     YMatrix1,...
%     YMatrix2,... 
%     YMatrix3...
%     )

