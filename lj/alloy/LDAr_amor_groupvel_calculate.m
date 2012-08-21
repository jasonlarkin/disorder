clear

%--------------------------------------------------------------------------
iseed = 1;
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
LD.alloy_conc = 0.0;
%--------------------------------------------------------------------------

%MASSES
%--------------------------------------------------------------------------
LD.mass(1) = 1.0; LD.mass(2) = 3.0;
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
[tmp,str.main] = system('pwd'); str.main_write = str.main;
str.main = strcat('/home/jason/lammps/LJ/alloy/10K/0.5/4x/NMD/');

AF.AF = load(strcat(str.main,'AF_1_1.mat'));

AF.AFx = load(strcat(str.main,'AF_1_2.mat'));
AF.AFy = load(strcat(str.main,'AF_1_3.mat'));
AF.AFz = load(strcat(str.main,'AF_1_4.mat'));

AF.AF.groupvel(:,1) =...
    (AF.AFx.freq - AF.AF.freq)/AF.AFx.dk;
AF.AF.groupvel(:,2) =...
    (AF.AFy.freq - AF.AF.freq)/AF.AFy.dk;
AF.AF.groupvel(:,3) =...
    (AF.AFz.freq - AF.AF.freq)/AF.AFz.dk;

plot(...
    AF.AF.freq,...
    AF.AF.LJ.sigma/AF.AF.LJ.tau*sqrt(...
    AF.AF.groupvel(:,1).^2 + ...
    AF.AF.groupvel(:,1).^2 + ...
    AF.AF.groupvel(:,1).^2 ), ...
    '.')

AFsave = AF.AF;
    
save(...
    strcat(str.main,'AF_groupvel_1.mat')...
    , '-struct', 'AFsave');

