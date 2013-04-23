clear
lj = m_lj; constant = m_constant;
str_af = '/home/jason/disorder2/si/amor/normand/perf4096/anneal_1100K/emin/';

%.eigvec = load(strcat(str_af,'AF_eigvec_1.dat'));
AF.freq = load(strcat(str_af,'AF_freq.dat'));
AF.x0 = m_x0_read(strcat(str_af,'x0.data'));

% str_nmd = '/home/jason/disorder2/lj/alloy/10K/0.15/10x/NMD/1/work/';
% nmd = load([str_nmd 'NMDdata.mat']);
% sed = load([str_nmd 'SEDdata.mat']);

DSF(1).DSF = load([str_af 'DSF_long_b2.5.mat']);
DSF(2).DSF = load([str_af 'DSF_tran_b2.5.mat']);

imode=0;

for idir = 1:2
    for ikpt = 1:length(DSF(1).DSF.kpt)
        imode = imode+1
        if ikpt<=1
            PT_PERC = 0.01;
            INV_PERC = 1.0;
        elseif ikpt<=3
            PT_PERC = 0.01;
            INV_PERC = 0.01;
        else
            PT_PERC = 0.01;
            INV_PERC = 0.01;
        end
        
%--------------------------------------------------------------------------
%tran
%--------------------------------------------------------------------------
        [Imax,Jmax] = max(...
            DSF(idir).DSF.SL(:,ikpt)...
            );
%Find wleft    
        [I,J] =...
            find(...
            DSF(idir).DSF.SL(1:Jmax,ikpt) ...
            <...
            PT_PERC*DSF(idir).DSF.SL(Jmax,ikpt) );
        wleft = I(length(I))
%Find wright
        [I,J] = find(...
            DSF(idir).DSF.SL(Jmax:length(DSF(idir).DSF.SL),ikpt) ...
            <...
            PT_PERC*DSF(idir).DSF.SL(Jmax,ikpt) );
        wright = Jmax + I(1)
%FIT THE LORENTZIAN(S)
c0 = [ 2*Imax, 1, DSF(idir).DSF.freq_range(Jmax)/1e12 ];

    lb(1:length(c0)) = 0.0; ub(1:3:length(c0)) = 100000*Imax; 
    ub(2:3:length(c0)) = 1000*1e15; 
    ub(3:3:length(c0)) =....
        1000*DSF(idir).DSF.freq_range(length(DSF(idir).DSF.freq_range));
    weights = ones(length(wleft:wright),1);
    weights(1:30) = INV_PERC/PT_PERC;
    weights(length(weights)-30:length(weights)) = INV_PERC/PT_PERC;
    lor_func = @(c,w)(c(1))./(1 + ( (w - c(3))./ c(2) ).^2 );
    options =...
        optimset(...
        'MaxIter',5000,'MaxFunEvals',5000,'TolFun',1e-20,'TolX',1e-6); 
    [c_fit] =...
        lsqcurvefit(...
        lor_func,c0,...
        DSF(idir).DSF.freq_range(wleft:wright)'/1e12,...
        DSF(idir).DSF.SL(wleft:wright,ikpt),...
        lb,ub,options);
%Store separate liftimes and frequencies for single and MULTIPLE FITS
center=c_fit(3); lifetime=1/(2*c_fit(2));
    semilogy(...
        DSF(idir).DSF.freq_range/1e12,DSF(idir).DSF.SL(:,ikpt),...
        DSF(idir).DSF.freq_range(wleft:wright)'/1e12,...
        lor_func(c0,DSF(idir).DSF.freq_range(wleft:wright)'/1e12)...
        )
pause
    semilogy(...
        DSF(idir).DSF.freq_range/1e12,DSF(idir).DSF.SL(:,ikpt),...
        DSF(idir).DSF.freq_range(wleft:wright)'/1e12,...
        lor_func(c_fit,DSF(idir).DSF.freq_range(wleft:wright)'/1e12)...
        )
    DSF_FIT.freq(imode) = center;
    DSF_FIT.life(imode) = lifetime;
    
    center
    lifetime
    
%     DSF_FIT.freq_sed(imode) = sed.freq(Ikpt(JfindT));
%     DSF_FIT.life_sed(imode) = sed.life(Ikpt(JfindT));
pause
    end
end
%pause
%end correction
% reso1 = 0.1; reso2 = 0.1;
% [I,J] = find(...
%     10 - reso1 < DSF_FIT.life & DSF_FIT.life < 70 + reso1 &...
%     15.0 - reso2 < DSF_FIT.freq & DSF_FIT.freq < 26.0 + reso2 ...
%     );
% DSF_FIT.life(J) = 12.43234; 

loglog(...
    DSF_FIT.freq, DSF_FIT.life,'.',...
    DSF_FIT.freq,2*pi./DSF_FIT.freq,...
    DSF_FIT.freq,(1E4)*1./(DSF_FIT.freq.^4),...
    DSF_FIT.freq,(1E3)*1./(DSF_FIT.freq.^2)...
    )

save(strcat(str_af,'DSF_FIT.mat'), '-struct', 'DSF_FIT');

