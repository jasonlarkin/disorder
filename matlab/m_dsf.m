clear

%AF.str = '/home/jason/disorder2/lj/alloy/10K/0.05/10x/work/';

AF.str = '/home/jason/disorder2/lj/amor/10x/work/';
%100
AF.kpt(:,1) = [0.05 0.15 0.2 0.25 0.3]'; 
AF.kpt(:,2) = 0;
AF.kpt(:,3) = 0;


lj = m_lj; constant = m_constant;
T = 10*(constant.kb/lj.eps);

NMD.Nx = 10; NMD.Ny = 10; NMD.Nz = 10; 

AF.eigvec = load(strcat(AF.str,'AF_eigvec_1.dat'));
AF.freq = load(strcat(AF.str,'AF_freq_1.dat'));
AF.x0 = m_x0_read(AF.str,'x0.data');

NMD.alat = 1.5636;
%NMD.alat = AF.x0.Lx;

%100
% AF.kpt(:,1) = [0.1 0.2 0.3 0.4 0.5]'; 
% AF.kpt(:,2) = 0;
% AF.kpt(:,3) = 0;
%110
% AF.kpt(:,1) = [0.1 0.2 0.3 0.4 0.5]'; 
% AF.kpt(:,2) = [0.1 0.2 0.3 0.4 0.5]';
% AF.kpt(:,3) = 0;
%111
% AF.kpt(:,1) = [0.1 0.2 0.3 0.4 0.5]'; 
% AF.kpt(:,2) = [0.1 0.2 0.3 0.4 0.5]';
% AF.kpt(:,3) = [0.1 0.2 0.3 0.4 0.5]';

% DSF =...
%     m_dsf_long(...
%     AF.str , AF.kpt , AF.x0 , AF.freq, AF.eigvec, NMD.alat , 5.0 , 1.1 );

DSF =...
    m_dsf_tran(...
    AF.str , AF.kpt , AF.x0 , AF.freq, AF.eigvec, NMD.alat , 5.0 , 1.1 );

semilogx(...
    DSF.freq_range , DSF.EpL(:,1)/max(DSF.EpL(:,1)),...
    DSF.freq_range , DSF.EpL(:,3)/max(DSF.EpL(:,3)),...
    DSF.freq_range , DSF.EpL(:,5)/max(DSF.EpL(:,5))...
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
semilogy(...
    DSF.freq_range,DSF.SL(:,1),...
    DSF.freq_range,DSF.SL(:,3),...
    DSF.freq_range,DSF.SL(:,5)...
    )








