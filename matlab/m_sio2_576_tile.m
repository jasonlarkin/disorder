clear
con = m_constant;


tile = load('/home/jason/disorder2/sio2/288.txt');



sio2.SED(1).SED =...
    load('/home/jason/disorder2/sio2/alan/a288/tile/anneal/emin/SEDfit.mat');
sio2.SED(1).SED.life(4:7) = sio2.SED(1).SED.life(4:7)*1.5;

sio2.VOLUME(4,3) = (40.26E-10)^3;
sio2.D(4,3).D = m_gulp_af_si_readDi('/home/jason/disorder2/sio2/alan/576a/tile/','Di.gout');
sio2.D(4,3).D(:,3) = sio2.D(4,3).D(:,3)*(3/4)/10;
sio2.cond(4,3) = (con.kb / sio2.VOLUME(4,3))*sum(sio2.D(4,3).D(:,3));
[sio2.D(4,3).dosx sio2.D(4,3).dosy] = m_dos(sio2.D(4,3).D(:,2), 175 , 1, sio2.VOLUME(4,3));

loglog(...
    sio2.SED(1).SED.HLDfreq(50:end),sio2.SED(1).SED.life(50:end)*1e-12,'.',...
    tile(:,1),tile(:,2),'.')

fit(:,1) = [ tile(:,1) ; sio2.SED(1).SED.HLDfreq(50:end)' ];
fit(:,2) = [ tile(:,2) ; (sio2.SED(1).SED.life(50:end)*1e-12)' ];

[C,IA,IC] = unique(fit(:,1));

%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
yi = interp1(...
     fit(IA,1),fit(IA,2),...
     sio2.D(4,3).D(:,2) );
loglog(...
    sio2.SED(1).SED.HLDfreq(4:end),sio2.SED(1).SED.life(4:end)*1e-12,'.',...
    tile(:,1),tile(:,2),'.',...
    sio2.D(4,3).D(:,2) , yi , '.',...
    sio2.D(4,3).D(:,2) , 2*pi./sio2.D(4,3).D(:,2)...
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------


SED.freq = sio2.D(4,3).D(:,2);  SED.life = yi; 

save('/home/jason/disorder2/sio2/alan/576a/tile/SED_fit.mat','-struct', 'SED')
