function ALLOY =...
    m_ld_defect_life_si(NMD,alloy_conc,m1,m2,vm,DW_SCALE,PEAK_HEIGHT,NUM_ITERATIONS)
%ALLOY =...
%    m_ld_defect_life(NMD,alloy_conc,m1,m2,vm,DW_SCALE,NUM_ITERATIONS)
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
tic
%--------------------------------------------------------------------------

[I,J] =sort(NMD.freq);
NMD.freq(J(1:3))
NMD.freq(J(1:3)) = 0.0;
NMD.freq(J(1:3))

%re-sort eigvec and transpose freq
for ikpt = 1:NMD.NUM_KPTS
        eig(...
            (ikpt-1)*NMD.x0.NUM_MODES+1:(ikpt)*NMD.x0.NUM_MODES,...
            1:3*NMD.x0.NUM_ATOMS_UCELL)...
            =...
        NMD.eigvec( (NMD.x0.NUM_ATOMS_UCELL*3)*(ikpt-1)+1 ...
            :...
            ((NMD.x0.NUM_ATOMS_UCELL*3)*ikpt),   1:NMD.x0.NUM_MODES      )';  
%with kpt indexing
%         ALLOY.freq(...
%             (ikpt-1)*NMD.x0.NUM_MODES+1:(ikpt)*NMD.x0.NUM_MODES,1)...
%             =...
%             NMD.freq(ikpt,:)';  
%wihtout kpt indexing
        ALLOY.freq(...
            (ikpt-1)*NMD.x0.NUM_MODES+1:(ikpt)*NMD.x0.NUM_MODES,1)...
            =...
        NMD.freq(...
            (ikpt-1)*NMD.x0.NUM_MODES+1:(ikpt)*NMD.x0.NUM_MODES,1);  
        
        
end

[I,J] =sort(ALLOY.freq);
ALLOY.freq(J(1:3))
min(ALLOY.freq)
hist(ALLOY.freq)

ALLOY.life(1:NMD.x0.NUM_MODES,1:NUM_ITERATIONS) = 0.0;
%calculate coupling strength
g(1) =...
    (1-alloy_conc)* ((1 - (m1/vm) )^2) + (alloy_conc)* ((1 - (m2/vm) )^2);
%calculate average level spacing
freq_sorted = sort(ALLOY.freq);
ALLOY.dw_avg =...
    real(...
    mean(...
    freq_sorted(2:length(freq_sorted))...
    -...
    freq_sorted(1:length(freq_sorted)-1)));
%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------
for imode = 1:size(eig,1)
%find all e*(k'v',b) dot e(kv,b), sum over b
    SUMb =...
        g(1)*...
        abs(...
            sum(...
                bsxfun(...
                @times, conj(eig(:,:))  ,  eig(imode,:)...
                )...
            ,2)...
        ).^2;
%set self-term 0
    SUMb(imode) = 0;
%find lorentzian broadenings
delwij = ...
    ALLOY.freq - ALLOY.freq(imode);
%scale the mean level spacing DW_SCALE
lor =...
    (PEAK_HEIGHT/pi)*(ALLOY.dw_avg*DW_SCALE./...
    ( delwij.^2 + (ALLOY.dw_avg*DW_SCALE)^2 ) );
%evaluate Eq. ()
    ALLOY.life(imode,1) =...
        ( pi*(ALLOY.freq(imode)^2) ) / ( 2*NMD.x0.Nx*NMD.x0.Ny*NMD.x0.Nz )*...
        sum(lor.*SUMb,1);
%convert 1/life to life
ALLOY.life(imode,1) = 1/ALLOY.life(imode,1);
end
%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
toc
%--------------------------------------------------------------------------

end








%--------------------------------------------------------------------------
%debug tools
%--------------------------------------------------------------------------

%     if ALLOY.freq(imode)<20
%     plot(ALLOY.freq,lor.*SUMb,'.')
% %--------------------------------------------------------------------------
% pause
% %--------------------------------------------------------------------------
%     end


%--------------------------------------------------------------------------
%iteration
%--------------------------------------------------------------------------

% %re-iterate using calculated lifetimes above
% for iiter = 2:NUM_ITERATIONS
%     iiter
% for imode = 1:size(eig,1)
% %find all e*(k',v') dot e(k,v), sum over b
%     SUMb =...
%         g(1)*...
%         abs(...
%         sum(...
%         bsxfun(...
%         @times, eig(:,:)  ,  conj(eig(imode,:)) ),2) ...
%         ).^2 ;
% %set self-term 0
%     SUMb(imode) = 0;
% %find lorentzian broadenings
% delwij = ...
%     ALLOY.freq - ALLOY.freq(imode);
% ALLOY.dw =...
%     1./(2*ALLOY.life(:,iiter-1)) + 1./(2*ALLOY.life(imode,iiter-1));
% 
% lor =...
%     (1.0/pi)*(ALLOY.dw./( delwij.^2 + ALLOY.dw.^2 ) );
% % max(lor)
% % plot(ALLOY.freq,ALLOY.dw,'.',ALLOY.freq,ALLOY.dw_avg)
% %--------------------------------------------------------------------------
% %pause
% %--------------------------------------------------------------------------
% %evaluate Eq. ()
%     ALLOY.life(imode,iiter) =...
%         ( pi*(ALLOY.freq(imode)^2) ) / ( 2*NMD.Nx*NMD.Ny*NMD.Nz )*...
%         sum(lor.*SUMb,1);
% %convert linewidth to lifetime
% ALLOY.life(imode,iiter) = 1/ALLOY.life(imode,iiter);
% end
% loglog(ALLOY.freq,ALLOY.life(:,iiter),'.')
% %--------------------------------------------------------------------------
% pause
% %--------------------------------------------------------------------------
% % loglog(ALLOY.freq,ALLOY.dw_avg,'.')
% % %--------------------------------------------------------------------------
% % pause
% % %--------------------------------------------------------------------------
% end
