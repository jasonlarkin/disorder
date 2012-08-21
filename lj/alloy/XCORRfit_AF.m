
%--------------------------------------------------------------------------
%0.0
%--------------------------------------------------------------------------
%4x
str.NMD = '/home/jason/orderdisorder/lj/alloy/10K/0.0/8x/XCORR_AF/1';


%--------------------------------------------------------------------------
%amor
%--------------------------------------------------------------------------
%4x
%str.NMD = '/home/jason/lammps/LJ/amorphous/4x/XCORR_AF/1';
%8x
%str.NMD = '/home/jason/lammps/LJ/amorphous/8x/XCORR_AF/1';
%10x
% str.NMD = '/home/jason/lammps/LJ/amorphous/10x/XCORR_AF/1';




NMD=load(strcat(str.NMD,'/NMDavg.mat'));

SED=load(strcat(str.NMD,'/SEDavg.mat'));

[tmp,str.main]=system('pwd');

XCORR_RANGE = 4000;

for imode=1:size(SED.modemaster,2)      

    imode

    I = find( SED.sed(1:XCORR_RANGE,imode) < 0);
    
    plot(...
        SED.omega(1:XCORR_RANGE),...
        SED.sed(1:XCORR_RANGE,imode),...
        SED.omega(1:XCORR_RANGE),...
        cumtrapz((SED.sed(1:XCORR_RANGE,imode))*SED.omega(1))...
        )
    
    SED.HLDfreq(imode)

    SED.life(imode) = max(cumtrapz(SED.sed(:,imode))*SED.omega(1))
    
%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------
end

%save only the fit properties
SEDfit.HLDfreq = SED.HLDfreq;
SEDfit.life = SED.life;
    
save(strcat(str.NMD,'/NMDfit.mat'), '-struct', 'NMD');
save(strcat(str.NMD,'/SEDfit.mat'), '-struct', 'SEDfit');

figure

loglog(SED.HLDfreq(:),SED.life(:),'.')



