% %amorphous
% AF.eigvec = load('/home/jason/lammps/LJ/amorphous/8x/prepare/eigvec_1.dat');
% AF.x0 = load('/home/jason/lammps/LJ/amorphous/8x/prepare/LJ_amor_1.pos');

str.AF = '/home/jason/lammps/LJ/alloy/10K/0.0/8x/NMD_AF/';

%alloy
AF.eigvec = load(strcat(str.AF,'AF_eigvec_1.dat'));
AF.freq = load(strcat(str.AF,'AF_freq_1.dat'));
AF.x0 = load(strcat(str.AF,'x0_0_1.data'));

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




KPT.kpt = [         0   0   0
                    1   0   0
                    2   0   0       ];
                
KPT.kpt = NMD.kptmaster;
                
                
find( ismember(...
    ismember(NMD.kptmaster(:,1), KPT.kpt(:,1)'),...
    ismember(NMD.kptmaster(:,2), KPT.kpt(:,2)')' == 
                
find( ismember( a, [1 6] ) )
                
%ALL
LIST.MODE = 1:size(AF.eigvec,1);
            

for imode = 1:size(AF.eigvec,1)
quiver3(...
    AF.x0(:,3),AF.x0(:,4),AF.x0(:,5),...
    AF.eigvec(1:3:size(AF.eigvec,1),LIST.MODE(imode)),...
    AF.eigvec(2:3:size(AF.eigvec,1),LIST.MODE(imode)),...
    AF.eigvec(3:3:size(AF.eigvec,1),LIST.MODE(imode))     )

AF = lj_eigvec_fft(AF, NMD, SED);

plot(1:NMD.NUM_ATOMS,AF.eigvec(1:3:size(AF.eigvec,1),LIST.MODE(imode)),...
1:NMD.NUM_ATOMS,AF.eigvec(2:3:size(AF.eigvec,1),LIST.MODE(imode)),...
1:NMD.NUM_ATOMS,AF.eigvec(3:3:size(AF.eigvec,1),LIST.MODE(imode)) )

plot(...
    1:size(SED.irrkpt.kpt,1) , abs(AF.eig_fftx(LIST.MODE(imode),:)),'.',...
    1:size(SED.irrkpt.kpt,1) , abs(AF.eig_ffty(LIST.MODE(imode),:)),'.',...
    1:size(SED.irrkpt.kpt,1) , abs(AF.eig_fftz(LIST.MODE(imode),:)),'.' )

[Ix,Jx] = max( abs(AF.eig_fftx(LIST.MODE(imode),:)) ); 
[Iy,Jy] = max( abs(AF.eig_ffty(LIST.MODE(imode),:)) );
[Iz,Jz] = max( abs(AF.eig_fftz(LIST.MODE(imode),:)) );

SED.irrkpt.kpt(Jx,:)
SED.irrkpt.kpt(Jy,:)
SED.irrkpt.kpt(Jz,:)

%attempt to find freq
Ifreq =...
    find(...
    SED.freq <= AF.freq(LIST.MODE(imode))+1e-4 &...
    SED.freq >= AF.freq(LIST.MODE(imode))-1e-4 );

%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------
end



