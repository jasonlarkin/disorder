clear
%--------------INPUT-------------------------------------------------------
%LJ Potential and Material Parameters
LJ.eps = 1.67E-21;              
LJ.sigma = 3.4E-10;                
LJ.mass = 6.6326E-26;               
LJ.tau = sqrt((LJ.mass*(LJ.sigma^2))/LJ.eps);
%potential cut off
LD.a2 = 2.5^2;
%Lorentzian cutoff
LD.deltaL = 1.e-2;

constant.kb = 1.3806E-23;                  
constant.hbar = 1.054E-34;      
constant.i = sqrt(-1);
constant.c = 29979245800.00019;      %cm/s
constant.s2ps = 1E-12;
constant.ang2m = 1E-10;
constant.eV2J = 1.60217646E-19;



%--------------------------------------------------------------------------
[tmp,str.main] = system('pwd'); str.main_write = str.main;
str.main = strcat(str.main,'/8x/AF/');
%--------------------------------------------------------------------------
iseed=1;
%--------------------------------------------------------------------------

tic
SED.SED = dlmread('/home/jason/Documents/SED/LJ/8x/LJ_NMD_8x_20K.txt');
toc

tic
amor.freq = dlmread(strcat(str.main,'freq_',int2str(iseed),'.dat'));
toc

[SED.DOS SED.freq_bin] = hist(SED.SED(:,2)*LJ.tau,20);

[amor.DOS amor.freq_bin] = hist(amor.freq,30);

SED.DOS_cum = cumtrapz(SED.DOS);
amor.DOS_cum = cumtrapz(amor.DOS);

plot(SED.freq_bin, SED.DOS_cum/sum(SED.DOS), ...
    amor.freq_bin,amor.DOS_cum/sum(amor.DOS))

plot(SED.freq_bin, SED.DOS/sum(SED.DOS), ...
    amor.freq_bin,amor.DOS/sum(amor.DOS))

LJAr_DOS_createfigure(SED.freq_bin, SED.DOS/sum(SED.DOS),...
     amor.freq_bin,amor.DOS/sum(amor.DOS))


    