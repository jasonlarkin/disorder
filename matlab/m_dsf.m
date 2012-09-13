
%alloy
%AF.str = '/home/jason/disorder/lj/amor/4x/work/10K/';
%AF.str = '/home/jason/disorder/lj/amor/8x/work/';
%AF.str = '/home/jason/disorder/lj/amor/10x/work/';
AF.str = '/home/jason/disorder/lj/amor/12x/work/';

NMD.Nx = 10; NMD.Ny = 10; NMD.Nz = 10; 
NMD.alat = 1.5636;

AF.eigvec = load(strcat(AF.str,'AF_eigvec_1.dat'));
AF.freq = load(strcat(AF.str,'AF_freq_1.dat'));
AF.x0 = load(strcat(AF.str,'x0.data'));
 
AF.NUM_ATOMS = AF.x0(1,1); AF.NUM_ATOMS_UCELL = AF.x0(1,2); 
AF.Lx = AF.x0(1,3); AF.Ly = AF.x0(1,4); AF.Lz = AF.x0(1,5);
AF.VOLUME = AF.Lx*AF.Ly*AF.Lz; AF.dr = (AF.VOLUME/AF.NUM_ATOMS)^(1/3);
 
AF.x0 = AF.x0(2:size(AF.x0,1),:);



AF.kpt(:,1) = (0:0.01:1/2)'; 
AF.kpt(:,2) = 0;AF.kpt(:,3) = 0;
%convert to expected form
AF.kpt(:,1) = AF.kpt(:,1) ;
AF.kpt(:,2) = AF.kpt(:,2) ;
AF.kpt(:,3) = AF.kpt(:,3) ;

DSF = m_dsf_long( AF.str , AF.kpt , AF.x0 , AF.freq, AF.eigvec, NMD.alat );

plot( DSF.kpt(:,1),DSF.dsf(1,:),'.' )

plot(...
DSF.freq,DSF.dsf(:,5)/max(DSF.dsf(:,5)),'.',...
DSF.freq,DSF.dsf(:,11)/max(DSF.dsf(:,11)),'.',...
DSF.freq,DSF.dsf(:,31)/max(DSF.dsf(:,31)),'.',...
DSF.freq,DSF.dsf(:,41)/max(DSF.dsf(:,41)),'.',...
DSF.freq,DSF.dsf(:,41)/max(DSF.dsf(:,41)),'.')






