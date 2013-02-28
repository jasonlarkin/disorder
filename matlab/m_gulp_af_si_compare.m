clear
lj = m_lj; con = m_constant;

AF = m_af_load( '/home/jason/disorder2/lj/amor/4x/AF/tmp/' )

Di = m_gulp_af_lj_readDi(...
    '/home/jason/disorder2/lj/amor/4x/AF/gulp/4.0.7/units/','Dij.gout');









plot(AF.freq/max(AF.freq),AF.Di(:,2)/max(AF.Di(:,2)),'.')

[AF.dosx AF.dosy] = m_dos(AF.freq(4:end)/max(AF.freq(4:end)),15,1);

[dosx dosy] = m_dos(Di(:,2)/max(Di(:,2)),15,1);




subplot(3,1,1),...
    plot(...
    AF.freq(4:end)/max(AF.freq(4:end)), Di(:,2)/max(Di(:,2)),'.',...
    AF.freq(4:end)/max(AF.freq(4:end)),AF.freq(4:end)/max(AF.freq(4:end)))
subplot(3,1,2),...
plot(...
    AF.freq/max(AF.freq),AF.Di(:,2)/max(AF.Di(:,2)),'.',...
    Di(:,2)/max(Di(:,2)),Di(:,3)/max(Di(:,3)),'.'...
    )
subplot(3,1,3),...
plot(...
    AF.Di(4:end,2)/max(AF.Di(4:end,2)),Di(:,3)/max(Di(:,3)),'.',...
    AF.Di(4:end,2)/max(AF.Di(4:end,2)),AF.Di(4:end,2)/max(AF.Di(4:end,2))...
    )

pause

plot(AF.freq,AF.Di(:,2),'.',Di(:,2),Di(:,3),'.')

