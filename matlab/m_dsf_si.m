clear
lj = m_lj; constant = m_constant;
str_af = '/home/jason/disorder2/sio2/alan/a972/emin/';

AF.eigvec = load(strcat(str_af,'eigvec.dat'));
AF.freq = load(strcat(str_af,'freq.dat'))';
AF.x0 = m_x0_read([str_af 'x0.data']);

nmd.alat = 4.75;

%silica
%100
AF.kpt(:,1) = [0.1 0.2 0.3 0.4 0.5]'; 
AF.kpt(:,2) = 0;
AF.kpt(:,3) = 0;


%100
% AF.kpt(:,1) = [0.125 0.25 0.375 0.5 0.625 0.75 0.875 1.0]'; 
% AF.kpt(:,2) = 0;
% AF.kpt(:,3) = 0;
%111
% AF.kpt(:,1) = [0.05 0.075 0.1 0.125 0.15 0.175 0.2 0.25 0.3 0.35 0.4 0.45 0.5]'; 
% AF.kpt(:,2) = [0.05 0.075 0.1 0.125 0.15 0.175 0.2 0.25 0.3 0.35 0.4 0.45 0.5]';
% AF.kpt(:,3) = 0;
%111
% AF.kpt(:,1) = [0.05 0.075 0.1 0.125 0.15 0.175 0.2 0.25 0.3 0.35 0.4 0.45 0.5]'; 
% AF.kpt(:,2) = [0.05 0.075 0.1 0.125 0.15 0.175 0.2 0.25 0.3 0.35 0.4 0.45 0.5]';
% AF.kpt(:,3) = [0.05 0.075 0.1 0.125 0.15 0.175 0.2 0.25 0.3 0.35 0.4 0.45 0.5]';


BROADEN = 1.0;
rng(11111);

DSF_long_avg.SL = zeros(length(AF.freq), size(AF.kpt,1));
DSF_tran_avg.SL = zeros(length(AF.freq), size(AF.kpt,1));

for ikpt = 1:size(AF.kpt,1)
    tic
    long =...
        m_dsf_long(...
        str_af , AF.kpt(ikpt,:), ...
        AF.x0 , AF.freq, AF.eigvec, nmd.alat , BROADEN , 1.1 );
    toc
    tic
    tran =...
        m_dsf_tran(...
        str_af , AF.kpt(ikpt,:), ...
        AF.x0 , AF.freq, AF.eigvec, nmd.alat , BROADEN , 1.1);
    toc
    
    DSF_long.kpt = AF.kpt;
    DSF_tran.kpt = AF.kpt;
    DSF_long.freq_range = long.freq_range;
    DSF_tran.freq_range = tran.freq_range;
    DSF_long.SL(:,ikpt) = long.SL;
    DSF_tran.SL(:,ikpt) = tran.SL;
        
    semilogy(...
        DSF_tran.freq_range,DSF_tran.SL(:,ikpt),'.',...
        DSF_long.freq_range,4*DSF_long.SL(:,ikpt),'.'...
        )
%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------
end
 
for ikpt=1:size(AF.kpt,1)
    ikpt
semilogy(...
        DSF_tran.freq_range,smooth(DSF_tran.SL(:,ikpt),7),'.',...
        DSF_long.freq_range,4*smooth(DSF_long.SL(:,ikpt),7),'.'...
        )
pause
end
        
save(strcat(str_af,'DSF_long.mat'), '-struct', 'DSF_long');
save(strcat(str_af,'DSF_tran.mat'), '-struct', 'DSF_tran');



