function energy = gulp_si_energy(LD)

% NMD.LJ.eps = 1.67E-21;              %aJ (1.67E-21 Joules) aJ=1E-18 J
% NMD.LJ.sigma = 3.4E-10;                 %Angstroms 3.4E-10 meters
% NMD.LJ.mass = 6.6326E-26;               %1E-28 kg (6.6326E-26 kg)
% NMD.LJ.tau = sqrt((NMD.LJ.mass*(NMD.LJ.sigma^2))/NMD.LJ.eps);
% NMD.constant.kb = 1.3806E-23;                    %aJ/k (1.3806E-23 J/K)
% NMD.constant.hbar = 1.054E-34;                %J/s
% NMD.constant.i = sqrt(-1);
% NMD.constant.c = 29979245800.00019;      %cm/s
% NMD.constant.s2ps = 1E-12;
% NMD.constant.ang2m = 1E-10;
% NMD.constant.eV2J = 1.60217646E-19;
% 
% mass_Ar = 39.948;

%--------------------------------------------------------------------------
%This is required to get the kpt + dk to properly input to GULP
    format long
%--------------------------------------------------------------------------

%1) Input alat, atom mass, kpt
    str.orig = 'XPOS';
    str.change =[ num2str( LD.pos_tmp(1,1) )];
    str.cmd1 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
    str.orig = 'YPOS';
    str.change =[ num2str( LD.pos_tmp(1,2) )];
    str.cmd2 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
    str.orig = 'ZPOS';
    str.change =[ num2str( LD.pos_tmp(1,3) )];
    str.cmd3 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];

    str.cmd4 =...
        ['si_energy.gin > input.gin'];
    
    str.cmd = ['sed ' str.cmd1 str.cmd2 str.cmd3 str.cmd4];
    system(str.cmd);
    
    while exist('input.gin')==0 end
    
%run gulp
    str.cmd = ['gulp input input'];
    system(str.cmd);

%3) grep out frequencies
    str.cmd = 'grep "Total lattice energy  " input.gout > gulp_energy.dat';            
    system(str.cmd);
     
    str.remove = 'Total lattice energy       =';
    str.cmd1 = ['-e ''s/' str.remove ' //g'' '];
    str.remove = 'eV';
    str.cmd2 = ['-e ''s/' str.remove '//g'' '];
    str.remove = 'kJ\/(mole unit cells)';
    str.cmd3 = ['-e ''s/' str.remove '//g'' '];
    
    str.cmd4 =...
        ['gulp_energy.dat > energy.dat'];
    
    str.cmd = ['sed ' str.cmd1 str.cmd2 str.cmd3 str.cmd4 ];
    system(str.cmd);
    
while exist('energy.dat')==0 end

%Read in freq to sort properly		
    str.read=strcat('energy.dat');
    fid=fopen(str.read);
    dummy = textscan(fid,'%f%f%f','Delimiter','\t','commentStyle', '--'); 
    fclose(fid);
    
energy = dummy{1}(1);

system('rm energy.dat');
% system('rm input.gin');