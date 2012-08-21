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
%--------------------------------------------------------------------------
[tmp,str.main] = system('pwd');
%str.main = strcat(str.main,'/amorphous/4x/prepare/');
%str.cmd = ['cd ' str.main];
%system(str.cmd);

%load the positions
iseed = 1;
x = dlmread(strcat(str.main,'/LJ_amor_',int2str(iseed),'.pos'));

%GREP out the freq and eigvec

NUM_ATOMS_UCELL = x(1,1);
NUM_MODES = x(1,1)*3;

%2) grep out eigenvectors

    eigvec = zeros(NUM_ATOMS_UCELL*3,NUM_MODES);
    str1 = 'grep -A ';
    str2 = strcat(int2str(3*NUM_ATOMS_UCELL),' " 1 x" amor_eig.gout > eigvec1.dat');
    str_cmd = [str1,str2];
    system(str_cmd);
    str_cmd = ('sed ''s/x//g'' eigvec1.dat > eigvec2.dat');
    system(str_cmd);
    %system('rm eigvec.dat');
    str_cmd = ('sed ''s/y//g'' eigvec2.dat > eigvec3.dat');
    %system('rm eigvec2.dat');
    system(str_cmd);
    %system('rm eigvec3.dat');
    str_cmd = ('sed ''s/z//g'' eigvec3.dat > eigvec4.dat');
    system(str_cmd);

%Read in eigvec to sort properly		
        str_read=strcat(str.main,'/eigvec4.dat');
        fid=fopen(str_read);
        dummy = textscan(fid,'%f%f%f%f%f%f%f','Delimiter','\t','commentStyle', '--'); 
        fclose(fid);

%Gamma has only real components	
    eigvec = zeros(NUM_ATOMS_UCELL*3,NUM_MODES); 
    for imode = 1:(NUM_MODES/3/2)
        eigvec(:,(imode-1)*6+1) = dummy{2}((imode-1)*3*NUM_ATOMS_UCELL+1:(imode)*3*NUM_ATOMS_UCELL);
        eigvec(:,(imode-1)*6+2) = dummy{3}((imode-1)*3*NUM_ATOMS_UCELL+1:(imode)*3*NUM_ATOMS_UCELL);
        eigvec(:,(imode-1)*6+3) = dummy{4}((imode-1)*3*NUM_ATOMS_UCELL+1:(imode)*3*NUM_ATOMS_UCELL);
        eigvec(:,(imode-1)*6+4) = dummy{5}((imode-1)*3*NUM_ATOMS_UCELL+1:(imode)*3*NUM_ATOMS_UCELL);
        eigvec(:,(imode-1)*6+5) = dummy{6}((imode-1)*3*NUM_ATOMS_UCELL+1:(imode)*3*NUM_ATOMS_UCELL);
        eigvec(:,(imode-1)*6+6) = dummy{7}((imode-1)*3*NUM_ATOMS_UCELL+1:(imode)*3*NUM_ATOMS_UCELL);
    end

%Output formatted eigvec		
        str_write=strcat(str.main,'/eigvec.dat');
    dlmwrite(str_write,eigvec,'delimiter',' ');
   
%3) grep out frequencies
             str_cmd = 'grep "Frequency  " amor_eig.gout > freq1.dat';            
             system(str_cmd);
             str_cmd = 'sed "s/Frequency  //g" freq1.dat > freq2.dat';            
             system(str_cmd);
             %str_cmd = 'mv freq2.dat freq_gulp.dat';            
             %system(str_cmd);
	%Read in freq to sort properly		
        	str_read=strcat(str.main,'/freq2.dat');
        	fid=fopen(str_read);
        	dummy = textscan(fid,'%f%f%f%f%f%f','Delimiter','\t','commentStyle', '--'); 
        	fclose(fid);
		for imode = 1:(NUM_MODES/6)
			freq((imode-1)*6+1) = dummy{1}(imode)*constant.c*2*pi;
			freq((imode-1)*6+2) = dummy{2}(imode)*constant.c*2*pi;
			freq((imode-1)*6+3) = dummy{3}(imode)*constant.c*2*pi;
            freq((imode-1)*6+4) = dummy{4}(imode)*constant.c*2*pi;
			freq((imode-1)*6+5) = dummy{5}(imode)*constant.c*2*pi;
			freq((imode-1)*6+6) = dummy{6}(imode)*constant.c*2*pi;
        end
        
%Output formatted freqs
        str_write=strcat(str.main,'/freq.dat');
    dlmwrite(str_write,freq,'delimiter',' ');

%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------

