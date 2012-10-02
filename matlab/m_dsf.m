clear

lj = m_lj; constant = m_constant;

T = 10*(constant.kb/lj.eps);

%amor
%AF.str = '/home/jason/disorder2/lj/amor/4x/work/';
%AF.str = '/home/jason/disorder/lj/amor/8x/work/';
%AF.str = '/home/jason/disorder/lj/amor/10x/work/';
%AF.str = '/home/jason/disorder/lj/amor/12x/work/';

AF.str = '/home/jason/disorder2/lj/alloy/10K/0.5/8x/work/';

NMD.Nx = 10; NMD.Ny = 10; NMD.Nz = 10; 


AF.eigvec = load(strcat(AF.str,'AF_eigvec_1.dat'));
AF.freq = load(strcat(AF.str,'AF_freq_1.dat'));
AF.x0 = m_x0_read(AF.str,'x0.data');

NMD.alat = 1.5636;
%NMD.alat = AF.x0.Lx;

AF.kpt(:,1) = [0.1 0.2 0.3 0.4 0.5]'; 
AF.kpt(:,2) = [0.1 0.2 0.3 0.4 0.5]';
AF.kpt(:,3) = [0.1 0.2 0.3 0.4 0.5]';
%convert to expected form
AF.kpt(:,1) = AF.kpt(:,1) ;
AF.kpt(:,2) = AF.kpt(:,2) ;
AF.kpt(:,3) = AF.kpt(:,3) ;

DSF =...
    m_dsf_long(...
    AF.str , AF.kpt , AF.x0 , AF.freq, AF.eigvec, NMD.alat , 0.1 , T, lj.mass/lj.mass );

semilogx(...
    DSF.freq_range , DSF.EpL(:,1)/max(DSF.EpL(:,1)),...
    DSF.freq_range,...
    DSF.EpL(:,ceil(length(DSF.kpt)/2))/max(DSF.EpL(:,ceil(length(DSF.kpt)/2))),...
    DSF.freq_range,...
    DSF.EpL(:,length(DSF.kpt))/max(DSF.EpL(:,length(DSF.kpt)))...
    )
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------
semilogy(...
    DSF.freq_range,DSF.SL(:,1),...
    DSF.freq_range,DSF.SL(:,ceil(length(DSF.kpt)/2)),...
    DSF.freq_range,DSF.SL(:,length(DSF.kpt))...
    )








