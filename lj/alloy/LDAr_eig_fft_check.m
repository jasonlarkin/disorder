% %amorphous
% AF.eigvec = load('/home/jason/lammps/LJ/amorphous/8x/prepare/eigvec_1.dat');
% AF.x0 = load('/home/jason/lammps/LJ/amorphous/8x/prepare/LJ_amor_1.pos');

str.AF = '/home/jason/lammps/LJ/alloy/10K/0.5/12x/NMD/';

%alloy
AF.eigvec = load(strcat(str.AF,'AF_eigvec_1.dat'));
AF.freq = load(strcat(str.AF,'AF_freq_1.dat'));
AF.x0 = load(strcat(str.AF,'x0_0.5_1.data'));
AF.Di = load(strcat(str.AF,'Di(wi)_1.dat'));

AF.NUM_ATOMS = AF.x0(1,1); AF.NUM_ATOMS_UCELL = AF.x0(1,2); 
AF.Lx = AF.x0(1,3); AF.Ly = AF.x0(1,4); AF.Lz = AF.x0(1,5);
AF.VOLUME = AF.Lx*AF.Ly*AF.Lz; AF.dr = (AF.VOLUME/AF.NUM_ATOMS)^(1/3);

AF.x0 = AF.x0(2:size(AF.x0,1),:);


str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.5/12x/NMD/1/';

NMD=load(strcat(str.NMD,'NMDdata.mat'));
SED=load(strcat(str.NMD,'SEDdata.mat'));

%convert freq to cols
SED = nmd_convert_data(NMD,SED);
%convert to LJ units
SED.freq = SED.freq*NMD.LJ.tau;



% KPT.kpt = [         0   0   0
%                     1   0   0
%                     2   0   0       ];

AF.kpt = SED.irrkpt.kpt;

% FREQ.freq = 

% KPT.kpt = NMD.kptmaster;
                           


tic
for imode = 1:size(AF.freq,1)
    
% quiver3(...
%     AF.x0(:,3),AF.x0(:,4),AF.x0(:,5),...
%     AF.eigvec(1:3:size(AF.eigvec,1),imode),...
%     AF.eigvec(2:3:size(AF.eigvec,1),imode),...
%     AF.eigvec(3:3:size(AF.eigvec,1),imode)     )

AF = lj_eigvec_fft(AF, NMD, SED, imode);

AF.DSF(imode,:) =...
    real(AF.eig_fftx(imode,:)).^2 + imag(AF.eig_fftx(imode,:)).^2 +...
    real(AF.eig_ffty(imode,:)).^2 + imag(AF.eig_ffty(imode,:)).^2 +...
    real(AF.eig_fftz(imode,:)).^2 + imag(AF.eig_fftz(imode,:)).^2 ; 

% subplot(3,1,1),...
%     plot(1:NMD.NUM_ATOMS,AF.eigvec(1:3:size(AF.eigvec,1),imode),...
% 1:NMD.NUM_ATOMS,AF.eigvec(2:3:size(AF.eigvec,1),imode),...
% 1:NMD.NUM_ATOMS,AF.eigvec(3:3:size(AF.eigvec,1),imode) )
% 
% subplot(3,1,2),...
%     plot(...
%     1:size(KPT.kpt,1) , abs(AF.eig_fftx(imode,:)),'.',...
%     1:size(KPT.kpt,1) , abs(AF.eig_ffty(imode,:)),'.',...
%     1:size(KPT.kpt,1) , abs(AF.eig_fftz(imode,:)),'.','markersize',30 )

% subplot(3,1,3),...
%     plot(KPT.kpt,SED.irrkpt.HLDfreq(:,:),'.')



% [Ix,Jx] =...
%     max( real(AF.eig_fftx(imode,:)).^2 + imag(AF.eig_fftx(imode,:)).^2 ); 
% [Iy,Jy] =...
%     max( real(AF.eig_ffty(imode,:)).^2 + imag(AF.eig_ffty(imode,:)).^2 ); 
% [Iz,Jz] =...
%     max( real(AF.eig_fftz(imode,:)).^2 + imag(AF.eig_fftz(imode,:)).^2 ); 

% Ix
% Iy
% Iz
% 
% KPT.kpt(Jx,:)
% KPT.kpt(Jy,:)
% KPT.kpt(Jz,:)
% imode
% AF.freq(imode)


% %attempt to find freq
% Ifreq =...
%     find(...
%     SED.freq <= AF.freq(imode)+1e-4 &...
%     SED.freq >= AF.freq(imode)-1e-4 );

%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------
end
toc



AF_DSF = AF;
AF_DSF = rmfield(AF_DSF,'eigvec');

save(strcat(str.AF,'AF_DSF.mat'), '-struct', 'AF_DSF');




