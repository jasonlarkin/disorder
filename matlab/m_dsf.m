clear
lj = m_lj; constant = m_constant;
str_af = '/home/jason/disorder2/lj/alloy/10K/0.5/10x/work/';

AF.eigvec = load(strcat(str_af,'AF_eigvec_1.dat'));
AF.freq = load(strcat(str_af,'AF_freq_1.dat'));
AF.x0 = m_x0_read(str_af,'x0.data');

str_nmd = '/home/jason/disorder2/lj/alloy/10K/0.5/10x/NMD/1/work/';
nmd = load([str_nmd 'NMDdata.mat']);
sed = load([str_nmd 'SEDdata.mat']);

%full
% AF.kpt(:,1) = nmd.kptmaster(:,1); 
% AF.kpt(:,2) = nmd.kptmaster(:,2);
% AF.kpt(:,3) = nmd.kptmaster(:,3);
%AK.kpt = [AF.kpt(:,1)/nmd.Nx AF.kpt(:,2)/nmd.Ny AF.kpt(:,3)/nmd.Nz]
%100
% AF.kpt(:,1) = [0.1 0.2 0.3 0.4 0.5]'; 
% AF.kpt(:,2) = 0;
% AF.kpt(:,3) = 0;
%110
% AF.kpt(:,1) = [0.1 0.2 0.3 0.4 0.5]'; 
% AF.kpt(:,2) = [0.1 0.2 0.3 0.4 0.5]';
% AF.kpt(:,3) = 0;
%111
AF.kpt(:,1) = [0.1 0.2 0.3 0.4 0.5]'; 
AF.kpt(:,2) = [0.1 0.2 0.3 0.4 0.5]';
AF.kpt(:,3) = [0.1 0.2 0.3 0.4 0.5]';

tic
DSF =...
    m_dsf_long(...
    str_af , [AF.kpt(:,1) AF.kpt(:,2) AF.kpt(:,3)], ...
    AF.x0 , AF.freq, AF.eigvec, nmd.alat , 20.0 , 1.1 );
toc

% for ikpt = 1:length(DSF.kpt)
%     AF.kpt(ikpt,:)
% semilogy(...
%     DSF.freq_range,DSF.SL(:,ikpt),...
%     DSF.freq_range,DSF.SL(:,ikpt),...
%     DSF.freq_range,DSF.SL(:,ikpt)...
%     )
% pause
% end

tic
DSF =...
    m_dsf_tran(...
    str_af , [AF.kpt(:,1) AF.kpt(:,2) AF.kpt(:,3)], ...
    AF.x0 , AF.freq, AF.eigvec, nmd.alat , 20.0 , 1.1);
toc

% for ikpt = 1:length(DSF.kpt)
%     AF.kpt(ikpt,:)
% semilogy(...
%     DSF.freq_range,DSF.SL(:,ikpt)...
%     )
% pause
% end

