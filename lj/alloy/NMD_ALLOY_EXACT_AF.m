
%CONV
str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.0/8x/NMD/1';
NMD0=load(strcat(str.NMD,'/NMDdata.mat'));
SED0=load(strcat(str.NMD,'/SEDdata.mat'));

str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.05/8x/NMD/1';
NMD05=load(strcat(str.NMD,'/NMDdata.mat'));
SED05=load(strcat(str.NMD,'/SEDdata.mat'));

str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.15/8x/NMD/1';
NMD15=load(strcat(str.NMD,'/NMDdata.mat'));
SED15=load(strcat(str.NMD,'/SEDdata.mat'));

str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.5/8x/NMD/1';
NMD5=load(strcat(str.NMD,'/NMDdata.mat'));
SED5=load(strcat(str.NMD,'/SEDdata.mat'));


%SUPER
str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.0/8x/NMD_AF/1';
NMD0_AF=load(strcat(str.NMD,'/NMDfit.mat'));
SED0_AF=load(strcat(str.NMD,'/SEDfit.mat'));

str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.05/8x/NMD_AF/1';
NMD05_AF=load(strcat(str.NMD,'/NMDfit.mat'));
SED05_AF=load(strcat(str.NMD,'/SEDfit.mat'));

str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.15/8x/NMD_AF/1';
NMD15_AF=load(strcat(str.NMD,'/NMDfit.mat'));
SED15_AF=load(strcat(str.NMD,'/SEDfit.mat'));

str.NMD = '/home/jason/lammps/LJ/alloy/10K/0.5/8x/NMD_AF/1';
NMD5_AF=load(strcat(str.NMD,'/NMDfit.mat'));
SED5_AF=load(strcat(str.NMD,'/SEDfit.mat'));


SED0 = nmd_convert_data(NMD0,SED0);
SED05 = nmd_convert_data(NMD0,SED05);
SED15 = nmd_convert_data(NMD0,SED15);
SED5 = nmd_convert_data(NMD0,SED5);

pause


%LIFE

loglog(...
    SED0.sedfreq,SED5.life/NMD0.LJ.tau,'.',...
    SED5.sedfreq,SED5.life/NMD5.LJ.tau,'.',...
    SED5_AF.sedfreq,SED5_AF.life,'.')

loglog(...
    SED0.sedfreq,SED0.life/NMD0.LJ.tau,'.',...
    SED05.sedfreq,SED05.life/NMD5.LJ.tau,'.',...
    SED05_AF.sedfreq,SED05_AF.life,'.')

%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------

%DOS
[SED0.y SED0.x] = hist(SED0.sedfreq,20);
[SED0_AF.y SED0_AF.x] = hist(SED0_AF.sedfreq,20);

plot(SED0.x, SED0.y,'.',...
    SED0_AF.x, SED0_AF.y,'.')


loglog(...
    SED0_AF.sedfreq(:),SED0_AF.life(:),'.',...
    SED05_AF.sedfreq(:),SED05_AF.life(:),'.',...
    SED15_AF.sedfreq(:),SED15_AF.life(:),'.',...
    SED5_AF.sedfreq(:),SED5_AF.life(:),'.',...
    SED0_AF.sedfreq(:),1E5*SED0_AF.sedfreq(:).^(-4))

