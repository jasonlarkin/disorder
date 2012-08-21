
%alloy
AF.str = '/home/jason/orderdisorder/lj/alloy/10K/0.5/8x/';

%alloy
AF.eigvec = load(strcat(AF.str,'/work/AF_eigvec_1.dat'));
AF.freq = load(strcat(AF.str,'AF_freq_1.dat'));
AF.x0 = load(strcat(AF.str,'x0.data'));
 
AF.NUM_ATOMS = AF.x0(1,1); AF.NUM_ATOMS_UCELL = AF.x0(1,2); 
AF.Lx = AF.x0(1,3); AF.Ly = AF.x0(1,4); AF.Lz = AF.x0(1,5);
AF.VOLUME = AF.Lx*AF.Ly*AF.Lz; AF.dr = (AF.VOLUME/AF.NUM_ATOMS)^(1/3);
 
AF.x0 = AF.x0(2:size(AF.x0,1),:);

%amorphous
%str.AF = '/home/jason/lammps/LJ/amorphous/8x/AF/';
%AF.eigvec = load(strcat(str.AF,'AF_eigvec_1.dat'));
%AF.freq = load(strcat(str.AF,'AF_freq_1.dat'));
%AF.x0 = load(strcat(str.AF,'LJ_amor_1.pos'));
%AF.Di = load(strcat(str.AF,'Di(wi)_1.dat'));
%AF.NUM_ATOMS = AF.x0(1,1); AF.NUM_ATOMS_UCELL = AF.x0(1,2); 
%AF.Lx = AF.x0(1,3); AF.Ly = AF.x0(1,4); AF.Lz = AF.x0(1,5);
%AF.VOLUME = AF.Lx*AF.Ly*AF.Lz; AF.dr = (AF.VOLUME/AF.NUM_ATOMS)^(1/3);
%AF.x0 = AF.x0(2:size(AF.x0,1),:);


NMD.Nx = 8;

AF.kpt(:,1) = (1:0.1:NMD.Nx)'; 
AF.kpt(:,2) = 0;AF.kpt(:,3) = 0;
%convert to expected form
AF.kpt(:,1) = AF.kpt(:,1) / (AF.Lx);
AF.kpt(:,2) = AF.kpt(:,2) / (AF.Ly);
AF.kpt(:,3) = AF.kpt(:,3) / (AF.Lz);

DSF = m_dsf_long( AF.str , AF.kpt , AF.x0 , AF.freq, AF.eigvec );

plot( DSF.kpt(:,1),DSF.dsf(1,:),'.' )

plot(...
AF.freq,DSF.dsf(:,5)/max(DSF.dsf(:,5)),'.',...
AF.freq,DSF.dsf(:,11)/max(DSF.dsf(:,11)),'.',...
AF.freq,DSF.dsf(:,31)/max(DSF.dsf(:,31)),'.',...
AF.freq,DSF.dsf(:,41)/max(DSF.dsf(:,41)),'.',...
AF.freq,DSF.dsf(:,51)/max(DSF.dsf(:,51)),'.')






