clear
lj = m_lj; constant = m_constant;
str_af = '/home/jason/disorder2/si/amor/normand/perfect3/anneal/';

AF.eigvec = load(strcat(str_af,'eigvec.dat'));
AF.freq = load(strcat(str_af,'freq.dat'))';
AF.x0 = m_x0_read([str_af 'x0_nve.data']);

nmd.alat = 5.43;

%4x: 2s 6x: 20 s 8x: 100 s

% str_nmd = '/home/jason/disorder2/lj/alloy/10K/0.5/10x/NMD/1/work/';
% nmd = load([str_nmd 'NMDdata.mat']);
% sed = load([str_nmd 'SEDdata.mat']);

%full
% AF.kpt(:,1) = nmd.kptmaster(:,1); 
% AF.kpt(:,2) = nmd.kptmaster(:,2);
% AF.kpt(:,3) = nmd.kptmaster(:,3);
%AK.kpt = [AF.kpt(:,1)/nmd.Nx AF.kpt(:,2)/nmd.Ny AF.kpt(:,3)/nmd.Nz]
%100
AF.kpt(:,1) = [0.05 0.075 0.1 0.125 0.15 0.175 0.2 0.25 0.3 0.35 0.4 0.45 0.5]'; 
AF.kpt(:,2) = 0;
AF.kpt(:,3) = 0;
% %010
% AF.kpt(:,2) = [0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5]'; 
% AF.kpt(:,1) = 0;
% AF.kpt(:,3) = 0;
% %001
% AF.kpt(:,3) = [0.05 0.1 0.15 0.2 0.25 0.3 0.35 0.4 0.45 0.5]'; 
% AF.kpt(:,1) = 0;
% AF.kpt(:,2) = 0;

%110
% AF.kpt(:,1) = [0.1 0.2 0.3 0.4 0.5]'; 
% AF.kpt(:,2) = [0.1 0.2 0.3 0.4 0.5]';
% AF.kpt(:,3) = 0;
%111
% AF.kpt(:,1) = [0.1 0.2 0.3 0.4 0.5]'; 
% AF.kpt(:,2) = [0.1 0.2 0.3 0.4 0.5]';
% AF.kpt(:,3) = [0.1 0.2 0.3 0.4 0.5]';

BROADEN = 20;
NUM_RAND = 5;
rng(11111);

DSF_long_avg.SL = zeros(length(AF.freq), size(AF.kpt,1));
DSF_tran_avg.SL = zeros(length(AF.freq), size(AF.kpt,1));

for ikpt = 1:size(AF.kpt,1)
    r = AF.kpt(ikpt,1); theta = 2*pi*rand(NUM_RAND,1); phi = 2*pi*rand(NUM_RAND,1);
    x = r*sin(theta).*cos(phi);
    y = r*sin(theta).*sin(phi);
    z = r*cos(theta);
    kpt_rand = [x y z];
    tic
    DSF_long_rand =...
        m_dsf_long(...
        str_af , kpt_rand, ...
        AF.x0 , AF.freq, AF.eigvec, nmd.alat , BROADEN , 1.1 );
    toc
    DSF_long_avg.SL(:,ikpt) = DSF_long_avg.SL(:,ikpt) + mean(DSF_long_rand.SL(:,:),2);
    tic
    DSF_tran_rand =...
        m_dsf_tran(...
        str_af , kpt_rand, ...
        AF.x0 , AF.freq, AF.eigvec, nmd.alat , BROADEN , 1.1);
    toc
    DSF_tran_avg.SL(:,ikpt) = DSF_tran_avg.SL(:,ikpt) + mean(DSF_tran_rand.SL(:,:),2);

    DSF_long_avg.freq_range = DSF_long_rand.freq_range;
    DSF_tran_avg.freq_range = DSF_tran_rand.freq_range;
    DSF_long_avg.kpt = AF.kpt;
    DSF_tran_avg.kpt = AF.kpt;
    
AF.kpt(ikpt,:)
        
    semilogy(...
        DSF_tran_avg.freq_range,DSF_tran_avg.SL(:,ikpt),'.',...
        DSF_long_avg.freq_range,4*DSF_long_avg.SL(:,ikpt),'.'...
        )
%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------
end
 
for ikpt=1:size(AF.kpt,1)
    ikpt
semilogy(...
        DSF_tran_avg.freq_range,DSF_tran_avg.SL(:,ikpt),'.',...
        DSF_long_avg.freq_range,4*DSF_long_avg.SL(:,ikpt),'.'...
        )
pause
end
        
save(strcat(str_af,'DSF_long_avg.mat'), '-struct', 'DSF_long_avg');
save(strcat(str_af,'DSF_tran_avg.mat'), '-struct', 'DSF_tran_avg');



