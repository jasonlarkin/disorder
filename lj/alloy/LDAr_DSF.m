% %amorphous
% AF.eigvec = load('/home/jason/lammps/LJ/amorphous/8x/prepare/eigvec_1.dat');
% AF.x0 = load('/home/jason/lammps/LJ/amorphous/8x/prepare/LJ_amor_1.pos');


%alloy
str.AF = '/home/jason/lammps/LJ/alloy/10K/0.5/8x/NMD_AF/';
%amorphous
str.AF = '/home/jason/lammps/LJ/amorphous/8x/AF/';

%alloy
% AF.eigvec = load(strcat(str.AF,'AF_eigvec_1.dat'));
% AF.freq = load(strcat(str.AF,'AF_freq_1.dat'));
% AF.x0 = load(strcat(str.AF,'x0_0.5_1.data'));
% AF.Di = load(strcat(str.AF,'Di(wi)_1.dat'));
% 
% AF.NUM_ATOMS = AF.x0(1,1); AF.NUM_ATOMS_UCELL = AF.x0(1,2); 
% AF.Lx = AF.x0(1,3); AF.Ly = AF.x0(1,4); AF.Lz = AF.x0(1,5);
% AF.VOLUME = AF.Lx*AF.Ly*AF.Lz; AF.dr = (AF.VOLUME/AF.NUM_ATOMS)^(1/3);
% 
% AF.x0 = AF.x0(2:size(AF.x0,1),:);

%amorphous
AF.eigvec = load(strcat(str.AF,'AF_eigvec_1.dat'));
AF.freq = load(strcat(str.AF,'AF_freq_1.dat'));
AF.x0 = load(strcat(str.AF,'LJ_amor_1.pos'));
AF.Di = load(strcat(str.AF,'Di(wi)_1.dat'));

AF.NUM_ATOMS = AF.x0(1,1); AF.NUM_ATOMS_UCELL = AF.x0(1,2); 
AF.Lx = AF.x0(1,3); AF.Ly = AF.x0(1,4); AF.Lz = AF.x0(1,5);
AF.VOLUME = AF.Lx*AF.Ly*AF.Lz; AF.dr = (AF.VOLUME/AF.NUM_ATOMS)^(1/3);

AF.x0 = AF.x0(2:size(AF.x0,1),:);


str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.5/8x/NMD/1/';
NMD=load(strcat(str.NMD,'NMDdata.mat'));
SED=load(strcat(str.NMD,'SEDdata.mat'));
%convert freq to cols
SED = nmd_convert_data(NMD,SED);
%convert to LJ units
SED.freq = SED.freq*NMD.LJ.tau;


AF.kpt(:,1) = (1:0.1:NMD.Nx)'; 
AF.kpt(:,2) = 0;AF.kpt(:,3) = 0;


for imode = 1:size(AF.freq,1)
tic
AF = lj_eigvec_fft(AF, NMD, SED, imode);

AF.DSF(imode,:) =...
    real(AF.eig_fftx(imode,:)).^2 + imag(AF.eig_fftx(imode,:)).^2 +...
    real(AF.eig_ffty(imode,:)).^2 + imag(AF.eig_ffty(imode,:)).^2 +...
    real(AF.eig_fftz(imode,:)).^2 + imag(AF.eig_fftz(imode,:)).^2 ; 


%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------
toc
end




plot(...
AF.freq,AF.DSF(:,5)/max(AF.DSF(:,5)),'.',...
AF.freq,AF.DSF(:,11)/max(AF.DSF(:,11)),'.',...
AF.freq,AF.DSF(:,31)/max(AF.DSF(:,31)),'.',...
AF.freq,AF.DSF(:,41)/max(AF.DSF(:,41)),'.',...
AF.freq,AF.DSF(:,51)/max(AF.DSF(:,51)),'.')

AF_DSF = AF;
AF_DSF = rmfield(AF_DSF,'eigvec');

save(strcat(str.AF,'AF_DSF.mat'), '-struct', 'AF_DSF');

