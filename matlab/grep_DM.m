clear
%--------------INPUT-------------------------------------------------------
%LJ Potential and Material Parameters
LJ.eps = 1.67E-21;              %aJ (1.67E-21 Joules) aJ=1E-18 J
LJ.sigma = 3.4E-10;                 %Angstroms 3.4E-10 meters
LJ.mass = 6.6326E-26;               %1E-28 kg (6.6326E-26 kg)
LJ.tau = sqrt((LJ.mass*(LJ.sigma^2))/LJ.eps);
constant.kb = 1.3806E-23;                    %aJ/k (1.3806E-23 J/K)
constant.hbar = 1.054E-34;                %J/s
constant.i = sqrt(-1);
constant.c = 29979245800.00019;      %cm/s
constant.s2ps = 1E-12;
constant.ang2m = 1E-10;
constant.eV2J = 1.60217646E-19;
%--------------------------------------------------------------------------
[tmp,str.main] = system('pwd');
%str.main = strcat(str.main,'/amorphous/4x/prepare/');
%str.cmd = ['cd ' str.main];
%system(str.cmd);

%--------------------------------------------------------------------------
%This is required to get the kpt + dk to properly input to GULP
            format long
%--------------------------------------------------------------------------

%load the positions
iseed = 1;
x = dlmread(strcat(str.main,'/LJ_amor_1.pos'));
%x = dlmread(strcat(str.main,'/x0.data'));

%GREP out the freq and eigvec

NUM_ATOMS_UCELL = x(1,1);
NUM_MODES = x(1,1)*3;

%2) grep out phi

    phi = zeros(NUM_ATOMS_UCELL*3,NUM_ATOMS_UCELL*3);
    phi_length = 1 + (NUM_ATOMS_UCELL*3/12)*NUM_ATOMS_UCELL*3;
    
    str1 = 'grep -A ';
str2 = strcat(int2str(phi_length),' "  Real Dynamical matrix :" amor_dynam.gout > phi1.dat');
    str_cmd = [str1,str2];
    system(str_cmd);
    str_cmd = ('sed ''s/  Real Dynamical matrix ://g'' phi1.dat > phi2.dat');
    system(str_cmd);


%Read in phi to sort properly		
    str_read=strcat(str.main,'/phi2.dat');
    dummy = dlmread(str_read);

%Gamma has only real components	
phi = zeros(NUM_ATOMS_UCELL*3,NUM_ATOMS_UCELL*3); 
for jline =1:NUM_ATOMS_UCELL*3
% for iline = 1:size(dummy,1)/(NUM_ATOMS_UCELL*3)
  for iline = 1:(NUM_ATOMS_UCELL*3)/12
    %phi(1:NUM_ATOMS_UCELL*3,(iline-1)*12+1:(iline)*12) =...
        %dummy((iline-1)*3*NUM_ATOMS_UCELL+1:(iline)*3*NUM_ATOMS_UCELL,1:12);
     phi(jline,(iline-1)*12+1:(iline)*12) =...
        dummy((jline-1)*(NUM_ATOMS_UCELL*3)/12+iline,1:12);
    
%         phi(1:NUM_ATOMS_UCELL*3,(iline-1)*12+1:(iline)*12) =...
%         dummy((iline-1)*3*NUM_ATOMS_UCELL+1:(iline)*3*NUM_ATOMS_UCELL,1:12)*constant.eV2J/LJ.eps;

  end
end
    
    [eig,freq] = eig(phi);
    hist(diag(sqrt(freq)),50);   
    str.write = strcat(str.main,'/phi.dat');
    dlmwrite(str.write,phi,'delimiter',' ');
    

%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------
    

