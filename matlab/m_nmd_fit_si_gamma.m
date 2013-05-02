clear
str = '/home/jason/disorder2/si/amor/normand/perf4096/anneal_1100K/emin/';
gamma_guess = 1;
PT_PERC = 1;
INV_PERC = 1;
HLD_SCALING_PCT = 1;
%m_nmd_fit_lj(str,gamma_guess,PT_PERC,INV_PERC)

str.NMD = str;

% gamma_guess = 3;
% 
% str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.05/10x/NMD/1';
% PT_PERC=0.05; INV_PERC = 0.0; HLD_SCALING_PCT = 0.95


NMD=load(strcat(str.NMD,'NMDavg.mat'));

SED=load(strcat(str.NMD,'SEDavg.mat'));

[tmp,str.main]=system('pwd');


for imode=1:size(SED.modemaster,2)      
%PRINT CURRENT KPT
    imode
%scaling
    HLD_SCALING_PCT=0.95;
%              

if imode<150
    PT_PERC=0.01; INV_PERC = 0.4; gamma_guess = 10;
else
    PT_PERC=0.01; INV_PERC = 0.4; gamma_guess = 4;
end

SED.HLDfreq(imode)

w_guess =...
    ceil(...
    SED.HLDfreq(imode)/NMD.w_step*HLD_SCALING_PCT);

%semilogy(1:length(SED.irrkpt.sedavg(:,kpt_list(kpt_cnt))),SED.irrkpt.sedavg(:,kpt_list(kpt_cnt)),'.') 
    semilogy(1:NMD.NUM_OMEGAS,SED.sed(1:NMD.NUM_OMEGAS,imode),'.')
%Set freq to 1:sample_size
    w(:,1)=(1:length(SED.sed(:,imode)));   
%LORENTZIAN FUNCTION FOR SINGLE AND MULTIPLE PEAKS 
    [Ipeak,Jpeak]=max(SED.sed(:,imode));
    [Imin,Jmin] = min(SED.sed(:,imode));
%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------

if imode<5
    
%Store 
    SED.sedfreq( imode ) = 0;
%Store 
    SED.life( imode ) = 0;
%Store    
    SED.height( imode ) = 0;
%Store
    SED.wleft(imode) = 0;
    SED.wright(imode) = 0;
    
else

%Find wleft    
    [I,J] =...
        find(...
        SED.sed(...
        1:Jpeak,imode)<PT_PERC*SED.sed(Jpeak,imode));
    wleft = w(I(length(I)));
%Find wright
    [I,J] = find(...
        SED.sed(...
        Jpeak:length(w),imode)>PT_PERC*SED.sed(Jpeak,imode));
%
    wright = Jpeak + I(length(I));
%     wright = w_guess(imode) + I(length(I));

%
    [Imax,Jmax] = max(SED.sed(wleft:wright,imode));          
%
    weights = ones(length(w(wleft:wright)),1);
%
    weights(1) = INV_PERC/PT_PERC;
    weights(length(weights)) =...
        INV_PERC/PT_PERC;

    lor_func = @(c,w)weights.*(c(1))./(1 + ( (w - c(3))./ c(2) ).^2 );
%
    options =...
        optimset(...
        'MaxIter',5000,'MaxFunEvals',5000,'TolFun',1e-6,'TolX',1e-6);  
%
    [Ipeak,Jpeak]=max(SED.sed(:,imode));
%Initial Guess
    c0 = [ 1*Ipeak, gamma_guess, Jpeak ]; 
%    c0 = [ 2*Ipeak, gamma_guess, w_guess(imode) ]; 
%FIT THE LORENTZIAN(S)
    lb(1:length(c0)) = 0.0; ub(1:3:length(c0)) =...
        max(SED.sed(:,imode))*10; 
    ub(2:3:length(c0)) = 1000; ub(3:3:length(c0)) = w(length(w));
    [c_fit] =...
        lsqcurvefit(...
        lor_func,c0,w(wleft:wright),...
        SED.sed(wleft:wright,imode).*weights,...
        lb,ub,options);
    NMD.w_step
%Store separate liftimes and frequencies for single and MULTIPLE FITS
    center=c_fit(3)*NMD.w_step; lifetime=1/(2*c_fit(2))/NMD.w_step;
%Store 
    SED.sedfreq( imode ) = center;
%Store 
    SED.life( imode ) = lifetime;
%Store    
    SED.height( imode ) = c_fit(1);
%Store
    SED.wleft(imode) = wleft;
    SED.wright(imode) = wright;
%Plot each fit, single and multiple
    semilogy(w(wleft:wright),...
        SED.sed(wleft:wright,imode),'.',...
        w(wleft:wright),lor_func(c_fit,w(wleft:wright)),...
        w(wleft:wright),lor_func(c0,w(wleft:wright)))
    axis([wleft wright min(...
        SED.sed(:,imode))...
        max(SED.sed(:,imode))]);
    
SED.R2(imode) =...
       1 - ...
       sum(...
        (SED.sed(wleft:wright,imode) -...
        lor_func(c_fit,w(wleft:wright))).^2)...
       /...
       sum(...
        (SED.sed(wleft:wright,imode) -...
        mean(lor_func(c_fit,w(wleft:wright)))).^2);
  SED.R2(imode)   

end
   

    disp(sprintf('%s', 'SED FREQ:'));
    SED.sedfreq(imode)
    disp(sprintf('%s', 'SED LIFE:'));
    SED.life(imode)
%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------
clf
hold off
clear I J buffer c0 pks loc str_func lifetime center
end

%save only the fit properties
SEDfit.HLDfreq = SED.HLDfreq;
SEDfit.sedfreq = SED.sedfreq;
SEDfit.life = SED.life;
SEDfit.height = SED.height;
SEDfit.wleft = SED.wleft;
SEDfit.wright = SED.wright;

loglog(SEDfit.HLDfreq,SEDfit.life,'.',...
    SEDfit.HLDfreq,2*pi./SEDfit.HLDfreq*1E12)
    
save(strcat(str.NMD,'/NMDfit.mat'), '-struct', 'NMD');
save(strcat(str.NMD,'/SEDfit.mat'), '-struct', 'SEDfit');


   

