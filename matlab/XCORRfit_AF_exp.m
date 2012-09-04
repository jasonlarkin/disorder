
%--------------------------------------------------------------------------
%0.0
%--------------------------------------------------------------------------
%4x
%str.NMD = '/home/jason/disorder/lj/alloy/10K/0.0/4x/XCORR_AF/1/work';
% %4x_2
%str.NMD = '/home/jason/disorder/lj/alloy/10K/0.0/4x/XCORR_AF/tmp/1';
%4x_3
str.NMD = '/home/jason/disorder/lj/alloy/10K/0.0/4x/XCORR_AF/work/1';

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

for imode=4:size(SED.modemaster,2)      

    imode
    SED.HLDfreq(imode)

    plot(...
        SED.omega(1:XCORR_RANGE),...
        SED.sed(1:XCORR_RANGE,imode),...
        SED.omega(1:XCORR_RANGE),...
        cumtrapz((SED.sed(1:XCORR_RANGE,imode))*SED.omega(1))...
        )
    SED.life(imode) = max(cumtrapz(SED.sed(:,imode))*SED.omega(1));
    SED.life(imode)
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
    I = find( SED.sed(1:XCORR_RANGE,imode) < 0);
    semilogy(SED.omega(1:I(1)),SED.sed(1:I(1),imode),'.')
    weights = ones(I(1),1);
    exp_func = @(tau,t)weights.*( exp(-t/tau) );
    options =...
        optimset(...
        'MaxIter',5000,'MaxFunEvals',5000,'TolFun',1e-6,'TolX',1e-6); 
    c0 = [ SED.life(imode) ];
    lb(1) = 0.0; ub(1) = 100000.0;
    [c_fit] =...
        lsqcurvefit( exp_func,c0,SED.omega(1:I(1)),...
        SED.sed(1:I(1),imode).*weights,...
        lb,ub,options);
    c_fit
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

loglog(SED.HLDfreq(:),SED.life(:),'.')



