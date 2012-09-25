


str.AF = '/home/jason/lammps/LJ/alloy/10K/0.05/12x/NMD_AF/';
af(1) = m_af_load( str.AF );

af(1).dos = 

str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.05/12x/NMD/1/';
NMD05=load(strcat(str.NMD,'NMDdata.mat'));
SED05=load(strcat(str.NMD,'SEDdata.mat'));
%convert freq to cols
SED05 = nmd_convert_data(NMD05,SED05);
%convert to LJ units
SED05.freq = SED05.freq*NMD05.LJ.tau;

str.AF = '/home/jason/lammps/LJ/alloy/10K/0.5/12x/NMD_AF/';
AF5.alloy.freq = load(strcat(str.AF,'AF_freq_1.dat'));
AF5.alloy.Di =...
    load(strcat(str.AF,'Di(wi)_1.dat'));
AF5.alloy.x0 = load(strcat(str.AF,'x0_0.5_1.data'));
AF5.alloy.NUM_ATOMS = AF5.alloy.x0(1,1); 
AF5.alloy.NUM_ATOMS_UCELL = AF5.alloy.x0(1,2); 
AF5.alloy.Lx = AF5.alloy.x0(1,3); 
AF5.alloy.Ly = AF5.alloy.x0(1,4); 
AF5.alloy.Lz = AF5.alloy.x0(1,5);
AF5.alloy.VOLUME = AF5.alloy.Lx*AF5.alloy.Ly*AF5.alloy.Lz; 
AF5.alloy.dr = (AF5.alloy.VOLUME/AF5.alloy.NUM_ATOMS)^(1/3);
AF5.alloy.x0 = AF5.alloy.x0(2:size(AF5.alloy.x0,1),:);

str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.5/12x/NMD/1/';
NMD5=load(strcat(str.NMD,'NMDdata.mat'));
SED5=load(strcat(str.NMD,'SEDdata.mat'));
%convert freq to cols
SED5 = nmd_convert_data(NMD5,SED5);
%convert to LJ units
SED5.freq = SED5.freq*NMD5.LJ.tau;


%AMOR
str.AF = '/home/jason/lammps/LJ/amorphous/12x/prepare/';

AF.amor.freq = load(strcat(str.AF,'freq_1.dat'));
AF.amor.Di =...
    load(strcat(str.AF,'Di(wi)_1.dat'));
AF.amor.x0 = load(strcat(str.AF,'LJ_amor_1.pos'));
AF.amor.NUM_ATOMS = AF.amor.x0(1,1); 
AF.amor.NUM_ATOMS_UCELL = AF.amor.x0(1,2); 
AF.amor.Lx = AF.amor.x0(1,3); 
AF.amor.Ly = AF.amor.x0(1,4); 
AF.amor.Lz = AF.amor.x0(1,5);
AF.amor.VOLUME = AF.amor.Lx*AF.amor.Ly*AF.amor.Lz; 
AF.amor.dr = (AF.amor.VOLUME/AF.amor.NUM_ATOMS)^(1/3);
AF.amor.x0 = AF.amor.x0(2:size(AF.amor.x0,1),:);

DOS_BIN = 400;
[AF.amor.DOSX AF.amor.DOSY] =...
    lj_DOS(AF.amor.freq, DOS_BIN, AF.amor.NUM_ATOMS);


[AF5.alloy.DOSX AF5.alloy.DOSY] =...
    lj_DOS(AF5.alloy.freq, DOS_BIN, AF5.alloy.NUM_ATOMS)
[SED5.DOSX SED5.DOSY] = lj_DOS(SED5.freq, DOS_BIN, NMD5.NUM_ATOMS);


[AF05.alloy.DOSX AF05.alloy.DOSY] =...
    lj_DOS(AF05.alloy.freq, DOS_BIN, AF05.alloy.NUM_ATOMS)
[SED05.DOSX SED05.DOSY] = lj_DOS(SED05.freq, DOS_BIN, NMD05.NUM_ATOMS);


% 
% AF_DSF = AF;
% AF_DSF = rmfield(AF_DSF,'eigvec');
% 
% save(strcat(str.AF,'AF_DSF.mat'), '-struct', 'AF_DSF');


plot(...
    AF.amor.DOSX ,...
    AF.amor.DOSY/sum(AF.amor.DOSY)/(NMD5.VOLUME/NMD5.LJ.sigma^3),...
    AF5.alloy.DOSX ,...
    AF5.alloy.DOSY/sum(AF5.alloy.DOSY)/(NMD5.VOLUME/NMD5.LJ.sigma^3),'.',...
    SED5.DOSX ,...
    SED5.DOSY/sum(SED5.DOSY)/(NMD5.VOLUME/NMD5.LJ.sigma^3),...
    AF05.alloy.DOSX ,...
    AF05.alloy.DOSY/sum(AF05.alloy.DOSY)/(NMD05.VOLUME/NMD05.LJ.sigma^3),'.',...
    SED05.DOSX ,...
    SED05.DOSY/sum(SED05.DOSY)/(NMD05.VOLUME/NMD05.LJ.sigma^3),...
    'linewidth',5,'markersize',20 )


