
%--------------------------------------------------------------------------
%0.0
%--------------------------------------------------------------------------
%4x
%str.NMD = '/home/jason/disorder/lj/alloy/10K/0.0/4x/XCORR_AF/1/work';
% %4x_2
%str.NMD = '/home/jason/disorder/lj/alloy/10K/0.0/4x/XCORR_AF/tmp/1';
%4x_3
%str.NMD = '/home/jason/disorder/lj/alloy/10K/0.0/4x/XCORR_AF/work/1';
%4x_4
%str.NMD = '/home/jason/disorder/lj/alloy/10K/0.0/4x/XCORR_AF/219/1';

%str.NMD = '/home/jason/disorder/lj/alloy/10K/0.05/10x/XCORR_AF/220/1/work';

%--------------------------------------------------------------------------
%amor
%--------------------------------------------------------------------------
%4x
str.NMD = '/home/jason/disorder/lj/amor/4x/XCORR_AF/220/1/work';
%8x
%str.NMD = '/home/jason/lammps/LJ/amorphous/8x/XCORR_AF/1';
%10x
%str.NMD = '/home/jason/disorder/lj/amor/10x/XCORR_AF/220/1/work';




NMD=load(strcat(str.NMD,'/NMDavg.mat'));

SED=load(strcat(str.NMD,'/SEDavg.mat'));

[tmp,str.main]=system('pwd');

XCORR_RANGE = 4096;

for imode=1:size(SED.modemaster,2)      

    imode

%subtract long time average
    SED.sed(:,imode) =...
        SED.sed(:,imode) - mean(SED.sed(250:1000,imode));
    
    plot(...
        SED.omega(1:XCORR_RANGE),...
        SED.sed(1:XCORR_RANGE,imode),...
        SED.omega(1:XCORR_RANGE),...
        cumtrapz((SED.sed(1:XCORR_RANGE,imode))*SED.omega(1))...
        )
    
    SED.HLDfreq(imode)

    SED.life(imode) = max(cumtrapz(SED.sed(:,imode))*SED.omega(1))
    
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
end

%save only the fit properties
SEDfit.HLDfreq = SED.HLDfreq;
SEDfit.life = SED.life;
    
save(strcat(str.NMD,'/NMDfit.mat'), '-struct', 'NMD');
save(strcat(str.NMD,'/SEDfit.mat'), '-struct', 'SEDfit');

figure

loglog(...
    SED.HLDfreq(:),SED.life(:),'.',...
    SED.HLDfreq(:),1E3*SED.HLDfreq(:).^(-2),...
    SED.HLDfreq(:),1E3*SED.HLDfreq(:).^(-4)...
    )



