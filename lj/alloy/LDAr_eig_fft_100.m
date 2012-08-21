% %amorphous
% AF.eigvec = load('/home/jason/lammps/LJ/amorphous/8x/prepare/eigvec_1.dat');
% AF.x0 = load('/home/jason/lammps/LJ/amorphous/8x/prepare/LJ_amor_1.pos');

str.AF = '/home/jason/lammps/LJ/alloy/10K/0.0/8x/NMD/';
tic
%alloy
AF.eigvec = load(strcat(str.AF,'AF_eigvec_1.dat'));
toc
AF.freq = load(strcat(str.AF,'AF_freq_1.dat'));
AF.x0 = load(strcat(str.AF,'x0_0_1.data'));
AF.Di = load(strcat(str.AF,'Di(wi)_1.dat'));

AF.NUM_ATOMS = AF.x0(1,1); AF.NUM_ATOMS_UCELL = AF.x0(1,2); 
AF.Lx = AF.x0(1,3); AF.Ly = AF.x0(1,4); AF.Lz = AF.x0(1,5);
AF.VOLUME = AF.Lx*AF.Ly*AF.Lz; AF.dr = (AF.VOLUME/AF.NUM_ATOMS)^(1/3);

AF.x0 = AF.x0(2:size(AF.x0,1),:);

str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.0/8x/NMD/1/';

NMD=load(strcat(str.NMD,'NMDdata.mat'));
SED=load(strcat(str.NMD,'SEDdata.mat'));

%convert freq to cols
SED = nmd_convert_data(NMD,SED);
%convert to LJ units
SED.freq = SED.freq*NMD.LJ.tau;

%PICK OUT 100

AF.NUM_dk = 50; 

AF.kpt(:,1) = 0:((NMD.Nx/2)/AF.NUM_dk):(NMD.Nx/2) ; 
AF.kpt(:,2) = 0; AF.kpt(:,3)=0;

%find
AF.kpt_norm =...
    sqrt(...
    AF.kpt(:,1).^2+AF.kpt(:,2).^2+AF.kpt(:,3).^2);
%find plot.dk
AF.kpt_norm_dk =...
    (max(AF.kpt_norm) - min(AF.kpt_norm))/...
    (size(AF.kpt_norm,1)-1);
AF.rangek = 0:AF.kpt_norm_dk:max(AF.kpt_norm);
%find plot.dw
AF.dw = (max(AF.freq) - min(AF.freq))/size(AF.freq,1);
AF.rangew = 0:AF.dw:max(AF.freq);


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


AF_DSF = AF;
AF_DSF = rmfield(AF_DSF,'eigvec');

save(strcat(str.AF,'AF_DSF.mat'), '-struct', 'AF_DSF');


% ismember(abs(SED.irrkpt.kpt(:,1)),KPT.kpt2(:,1))
% 
% [Ikpt,J] = find(...
%     ismember(abs(SED.irrkpt.kpt(:,1)),KPT.kpt2(:,1))==1 &...
%     (SED.irrkpt.kpt(:,2) == 0) ==1 &...
%     (SED.irrkpt.kpt(:,3) == 0) ==1  ) ;

% IKPT(1:size(KPT.kpt2(Ikpt,:))) = 0;



% [Ikpt,J] = find(...
%     abs(SED.irrkpt.kpt(:,1))==KPT.kpt2(1,1) &...
%     abs(SED.irrkpt.kpt(:,2))==KPT.kpt2(1,2) &...
%     abs(SED.irrkpt.kpt(:,3))==KPT.kpt2(1,3) );
% IKPT(1,1) = Ikpt;
% for ikpt = 2:size(KPT.kpt2,1)
% [Ikpt,J] = find(...
%     abs(SED.irrkpt.kpt(:,1))==KPT.kpt2(ikpt,1) &...
%     abs(SED.irrkpt.kpt(:,2))==KPT.kpt2(ikpt,2) &...
%     abs(SED.irrkpt.kpt(:,3))==KPT.kpt2(ikpt,3) )
% IKPT(ikpt,1) = Ikpt;
% end
% [Ikpt,J] = find(...
%     abs(SED.irrkpt.kpt(:,3))==KPT.kpt2(size(KPT.kpt2,1),1) &...
%     abs(SED.irrkpt.kpt(:,2))==KPT.kpt2(size(KPT.kpt2,1),2) &...
%     abs(SED.irrkpt.kpt(:,1))==KPT.kpt2(size(KPT.kpt2,1),3) )
% IKPT(ikpt,1) = Ikpt;
% 
% 
% plot3(repmat(KPT.kpt2(:,1)',size(AF.DSF(:,IKPT'),1),1) , AF.DSF(:,IKPT),'.')


% AF.eig
% 
% plot3(SED.irrkpt.kpt(IKPT,1),

%[Y X] = hist(AF.eig_fftz(imode,1),100);



