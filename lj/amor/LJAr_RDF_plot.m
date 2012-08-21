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
str.main = strcat(str.main,'/8x/RDF/');


RDF.crys = dlmread(strcat(str.main,'RDF_avg_crys.dat'));

RDF.amor = dlmread(strcat(str.main,'RDF_avg.dat'));

LJAr_RDF_createfigure(RDF.crys(:,1),RDF.crys(:,2),...
    RDF.amor(:,1),RDF.amor(:,2))


    