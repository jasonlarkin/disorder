clear

str.NMD = '/home/jason/disorder2/lj/amor/4x/XCORR_AF/1K/2^20_2^19/';


NMD=load(strcat(str.NMD,'NMDavg.mat'));

SED=load(strcat(str.NMD,'SEDavg.mat'));

[tmp,str.main]=system('pwd');

XCORR_RANGE = 4000; XCORR_RANGE_1 = 1; XCORR_RANGE_2 = 1000;

for imode=1:1:size(SED.modemaster,2)      

    imode
    
    xcorr_mean = mean(SED.sed(XCORR_RANGE_1:XCORR_RANGE,imode));
    xcorr_mean = 0;
    
    I = find( SED.sed(1:XCORR_RANGE,imode) < 0);
    
    [Imin,Jmin] = min(SED.sed(XCORR_RANGE_1:XCORR_RANGE,imode));
    
    plot(...
        SED.omega(1:XCORR_RANGE),...
        SED.sed(1:XCORR_RANGE,imode) ,...
        SED.omega(1:XCORR_RANGE),...
        cumtrapz((SED.sed(1:XCORR_RANGE,imode))*SED.omega(1) - ...
        0.0 ) ...
        )
    
    SED.HLDfreq(imode)

    SED.life(imode) =...
        max(cumtrapz(SED.sed(:,imode) - ...
        0.0 ))*SED.omega(1) ;
    
%     semilogy(...
%         SED.omega(1:XCORR_RANGE),...
%         SED.sed(1:XCORR_RANGE,imode),'.'...
%         )
    
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
end

%save only the fit properties
SEDfit.HLDfreq = SED.HLDfreq;
SEDfit.life = SED.life;
    
save(strcat(str.NMD,'NMDfit.mat'), '-struct', 'NMD');
save(strcat(str.NMD,'SEDfit.mat'), '-struct', 'SEDfit');

figure

loglog(...
    SED.HLDfreq(:),SED.life(:),'.',...
    SED.HLDfreq(:),(2*pi)./SED.HLDfreq(:)...
    )



