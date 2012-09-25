function m_nmd_fit_lj(str,gamma_guess,PT_PERC,INV_PERC,HLD_SCALING_PCT)
%m_nmd_fit_lj(str,gamma_guess,PT_PERC,INV_PERC)

str.NMD = str;

% gamma_guess = 3;
% 
% str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.05/10x/NMD/1';
% PT_PERC=0.05; INV_PERC = 0.0; HLD_SCALING_PCT = 0.95


NMD=load(strcat(str.NMD,'NMDavg.mat'));

SED=load(strcat(str.NMD,'SEDavg.mat'));

[tmp,str.main]=system('pwd');

for kpt_cnt=1:size(SED.irrkpt.kpt,1)      
%PRINT CURRENT KPT
    SED.irrkpt.kpt(kpt_cnt,1:3)
% scaling
%     HLD_SCALING_PCT=0.95;
%              
w_guess =...
    ceil(...
    SED.irrkpt.HLDfreq(1:NMD.NUM_MODES,kpt_cnt)/NMD.w_step*HLD_SCALING_PCT)
%
if SED.irrkpt.kpt(kpt_cnt,1)==0 &...
        SED.irrkpt.kpt(kpt_cnt,1)==0 &...
            SED.irrkpt.kpt(kpt_cnt,1)==0
        
    SED.irrkpt.sedfreq( 1:NMD.NUM_MODES ,kpt_cnt) = 0;
    %Store in ps
    SED.irrkpt.life( 1:NMD.NUM_MODES ,kpt_cnt) = 0; 
    SED.irrkpt.flag(kpt_cnt)=0;
else
    for imode = 1:NMD.NUM_MODES
%semilogy(1:length(SED.irrkpt.sedavg(:,kpt_list(kpt_cnt))),SED.irrkpt.sedavg(:,kpt_list(kpt_cnt)),'.') 
    semilogy(1:NMD.NUM_OMEGAS,SED.irrkpt.sedavg(1:NMD.NUM_OMEGAS,imode,kpt_cnt),'.')
%Set freq to 1:sample_size
    w(:,1)=(1:length(SED.irrkpt.sedavg(:,kpt_cnt)));   
%LORENTZIAN FUNCTION FOR SINGLE AND MULTIPLE PEAKS 
    [Ipeak,Jpeak]=max(SED.irrkpt.sedavg(:,imode,kpt_cnt));
    [Imin,Jmin] = min(SED.irrkpt.sedavg(:,imode,kpt_cnt));
%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------

%Find wleft    
    [I,J] =...
        find(...
        SED.irrkpt.sedavg(1:Jpeak,imode,kpt_cnt)...
        <...
        PT_PERC*SED.irrkpt.sedavg(Jpeak,imode,kpt_cnt)     );
    
    wleft = w(I(length(I)));
    
%     wleft = 1;

%Find wright
    [I,J] = find(...
        SED.irrkpt.sedavg(Jpeak:length(w),imode,kpt_cnt)...
        <...
        PT_PERC*SED.irrkpt.sedavg(Jpeak,imode,kpt_cnt)      );

      wright = Jpeak + I(1);
     
%     wright = max(w);
%    wright = Jpeak + I(length(I));
    
%
    [Imax,Jmax]=max(SED.irrkpt.sedavg(wleft:wright,imode,kpt_cnt));          
%
    weights = ones(length(w(wleft:wright)),1);
%
    weights(1:1) = INV_PERC/PT_PERC;
    weights(length(weights)-1:length(weights)) = INV_PERC/PT_PERC;
%
    lor_func = @(c,w)weights.*(c(1))./(1 + ( (w - c(3))./ c(2) ).^2 );
%
    options =...
        optimset(...
        'MaxIter',5000,'MaxFunEvals',5000,'TolFun',1e-5,'TolX',1e-5);  
%
    [Ipeak,Jpeak]=max(SED.irrkpt.sedavg(:,imode,kpt_cnt));
%Initial Guess
    c0 = [ 2*Ipeak, gamma_guess, Jpeak ]; 
%    c0 = [ 2*Ipeak, gamma_guess, w_guess(imode) ]; 
%FIT THE LORENTZIAN(S)
    lb(1:length(c0)) = 0.0; ub(1:3:length(c0)) =...
        max(SED.irrkpt.sedavg(:,imode,kpt_cnt))*10; 
    ub(2:3:length(c0)) = 10000; ub(3:3:length(c0)) = w(length(w));
    [c_fit] =...
        lsqcurvefit(...
        lor_func,c0,w(wleft:wright),...
        SED.irrkpt.sedavg(wleft:wright,imode,kpt_cnt).*weights,...
        lb,ub,options);
     
%Store separate liftimes and frequencies for single and MULTIPLE FITS
    center=c_fit(3)*NMD.w_step; lifetime=1/(2*c_fit(2))/NMD.w_step;
%Store 
    SED.irrkpt.sedfreq( imode ,kpt_cnt) = center;
%Store 
    SED.irrkpt.life( imode ,kpt_cnt) = lifetime;
%Store    
    SED.irrkpt.height( imode, kpt_cnt) = c_fit(1);
%Store
    SED.irrkpt.wleft(imode,kpt_cnt) = wleft;
    SED.irrkpt.wright(imode,kpt_cnt) = wright;
%Plot each fit, single and multiple
    semilogy(w(wleft:wright),...
        SED.irrkpt.sedavg(wleft:wright,imode,kpt_cnt),'.',...
        w(wleft:wright),lor_func(c_fit,w(wleft:wright)),...
        w(wleft:wright),lor_func(c0,w(wleft:wright)))
    axis([wleft wright min(...
        SED.irrkpt.sedavg(:,imode,kpt_cnt))...
        max(SED.irrkpt.sedavg(:,imode,kpt_cnt))]);
    
   SED.irrkpt.R2(imode,kpt_cnt) =...
       1 - ...
       sum(...
        (SED.irrkpt.sedavg(wleft:wright,imode,kpt_cnt) -...
        lor_func(c_fit,w(wleft:wright))).^2)...
       /...
       sum(...
        (SED.irrkpt.sedavg(wleft:wright,imode,kpt_cnt) -...
        mean(lor_func(c_fit,w(wleft:wright)))).^2);
  SED.irrkpt.R2(imode,kpt_cnt)      
    
%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------
    end
%Calculate integral of SED = 2TE
    SED.irrkpt.sedintegral(kpt_cnt) =...
        trapz(w*(NMD.w_step),SED.irrkpt.sedavg(:,kpt_cnt));
%
    disp(sprintf('%s', 'SED FREQ:'));
    SED.irrkpt.sedfreq(:,kpt_cnt)
    disp(sprintf('%s', 'SED LIFE:'));
    SED.irrkpt.life(:,kpt_cnt)
%
kpt_cnt
%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------
clf
hold off
clear I J buffer c0 pks loc str_func lifetime center
end
end %END KPTLSIT
   

%FILTER 0.0
% I = find(SED.irrkpt.sedfreq(:,:) > 10.0 & SED.irrkpt.life(:,:) > 40.0);
% SED.irrkpt.life(I) = mean(mean(SED.irrkpt.life));

%FILTER 0.5
% I = find(SED.irrkpt.sedfreq(:,:) > 7.0 & SED.irrkpt.life(:,:) > 20.0);
% SED.irrkpt.life(I) = mean(mean(SED.irrkpt.life));

loglog(SED.irrkpt.sedfreq(:,:),SED.irrkpt.life(:,:),'.')

pause

plot(SED.irrkpt.HLDfreq,SED.irrkpt.sedfreq,'.')

pause

plot(SED.irrkpt.sedfreq,SED.irrkpt.R2(:,:),'.')

I = find(SED.irrkpt.R2<0.8);

plot(SED.irrkpt.sedfreq,SED.irrkpt.life,'.')

SED = nmd_convert_data(NMD,SED);

save(strcat(str.NMD,'NMDfit.mat'), '-struct', 'NMD');
save(strcat(str.NMD,'SEDfit.mat'), '-struct', 'SED');

end
    
   

