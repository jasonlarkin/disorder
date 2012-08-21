
hold on

%c=0.0
%str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.0/4x/NMD/1';
% str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.0/6x/NMD/1';
% str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.0/8x/NMD/1';
% str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.0/10x/NMD/1';
str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.0/12x/NMD/1';

NMD=load(strcat(str.NMD,'/NMDdata.mat'));
SED=load(strcat(str.NMD,'/SEDdata.mat'));
SED = nmd_convert_data(NMD,SED);
loglog(...
    2*pi./(SED.sedfreq(1:NMD.NUM_MODES:size(SED.sedfreq,1))),...
    SED.life(1:NMD.NUM_MODES:size(SED.sedfreq,1))/NMD.LJ.tau,'.',...
    2*pi./(SED.sedfreq(2:NMD.NUM_MODES:size(SED.sedfreq,1))),...
    SED.life(2:NMD.NUM_MODES:size(SED.sedfreq,1))/NMD.LJ.tau,'.',...
    2*pi./(SED.sedfreq(3:NMD.NUM_MODES:size(SED.sedfreq,1))),...
    SED.life(3:NMD.NUM_MODES:size(SED.sedfreq,1))/NMD.LJ.tau,'.',...
    ((0:10)/10)*3.5,...
    ((0:10)/10)*3.5 )

%c=0.05
%str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.05/4x/NMD/1';
%str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.05/6x/NMD/1';
%str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.05/8x/NMD/1';
%str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.05/10x/NMD/1';
str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.05/12x/NMD/1';

NMD=load(strcat(str.NMD,'/NMDdata.mat'));
SED=load(strcat(str.NMD,'/SEDdata.mat'));
SED = nmd_convert_data(NMD,SED);
loglog(...
    2*pi./(SED.sedfreq),SED.life/NMD.LJ.tau,'.',...
    ((0:10)/10)*3.5,...
    ((0:10)/10)*3.5 )

%c=0.15
%str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.15/4x/NMD/1';
%str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.15/6x/NMD/1';
%str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.15/8x/NMD/1';
%str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.15/10x/NMD/1';
str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.15/12x/NMD/1';

NMD=load(strcat(str.NMD,'/NMDdata.mat'));
SED=load(strcat(str.NMD,'/SEDdata.mat'));
SED = nmd_convert_data(NMD,SED);
loglog(...
    2*pi./(SED.sedfreq),SED.life/NMD.LJ.tau,'.',...
    ((0:10)/10)*3.5,...
    ((0:10)/10)*3.5 )

%c=0.5
% str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.5/4x/NMD/1';
%str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.5/6x/NMD/1';
% str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.5/8x/NMD/1';
%str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.5/10x/NMD/1';
str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.5/12x/NMD/1';

NMD=load(strcat(str.NMD,'/NMDdata.mat'));
SED=load(strcat(str.NMD,'/SEDdata.mat'));
SED = nmd_convert_data(NMD,SED);
loglog(...
    2*pi./(SED.sedfreq(1:NMD.NUM_MODES:size(SED.sedfreq,1))),...
    SED.life(1:NMD.NUM_MODES:size(SED.sedfreq,1))/NMD.LJ.tau,'.',...
    2*pi./(SED.sedfreq(2:NMD.NUM_MODES:size(SED.sedfreq,1))),...
    SED.life(2:NMD.NUM_MODES:size(SED.sedfreq,1))/NMD.LJ.tau,'.',...
    2*pi./(SED.sedfreq(3:NMD.NUM_MODES:size(SED.sedfreq,1))),...
    SED.life(3:NMD.NUM_MODES:size(SED.sedfreq,1))/NMD.LJ.tau,'.',...
    ((0:10)/10)*3.5,...
    ((0:10)/10)*3.5 )




