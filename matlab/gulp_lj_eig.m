function eigvec = gulp_lj_eig(kpt,NUM_ATOMS_UCELL)
%--------------------------------------------------------------------------
%eigvec = gulp_lj_eig(kpt,NUM_ATOMS_UCELL)
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
%This is required to get the kpt + dk to properly input to GULP
    format long
%--------------------------------------------------------------------------

str_orig = '0.0 0.0 0.0';
str_change =...
strcat( num2str(kpt(1)),'\t',num2str(kpt(2)),'\t',num2str(kpt(3)) );
str_cmd = strcat('sed ''s/',str_orig,'/',str_change,'/g'' BZ.gin > disp.gin');
system(str_cmd);



str.cmd = ['gulp disp disp']; system(str.cmd);

%grep out eigenvectors
    eigvec = zeros(3*NUM_ATOMS_UCELL,3*NUM_ATOMS_UCELL);
    str1 = 'grep -A ';
    str2 = strcat(int2str(3*NUM_ATOMS_UCELL),...
        ' " 1 x" disp.gout > eigvec_grep.dat');
    str.cmd = [str1,str2]; system(str.cmd);
    str.cmd = ('sed ''s/x//g'' eigvec_grep.dat > eigvec2.dat');
    system(str.cmd);
    system('rm eigvec_grep.dat');
    str.cmd = ('sed ''s/y//g'' eigvec2.dat > eigvec3.dat'); 
    system(str.cmd); system('rm eigvec2.dat');  
    str.cmd = ('sed ''s/z//g'' eigvec3.dat > eigvec4.dat'); 
    system(str.cmd); system('rm eigvec3.dat');

%read in eigvec to sort properly		
    str.read=strcat('eigvec4.dat');
    fid=fopen(str.read);
    dummy = textscan(fid,'%f%f%f%f%f%f%f','Delimiter','\t',...
        'commentStyle', '--'); 
    fclose(fid);
    system('rm eigvec4.dat'); 
    
if kpt(1) == 0 & kpt(2) == 0 & kpt(3) == 0 
%Gamma has only real components	
    eigvec = zeros(3*NUM_ATOMS_UCELL,3*NUM_ATOMS_UCELL); 
    for imode = 1:(3*NUM_ATOMS_UCELL/3/2)
    eigvec(:,(imode-1)*6+1) =...
        dummy{2}((imode-1)*3*NUM_ATOMS_UCELL+1:(imode)*3*NUM_ATOMS_UCELL);
    eigvec(:,(imode-1)*6+2) =...
        dummy{3}((imode-1)*3*NUM_ATOMS_UCELL+1:(imode)*3*NUM_ATOMS_UCELL);
    eigvec(:,(imode-1)*6+3) =...
        dummy{4}((imode-1)*3*NUM_ATOMS_UCELL+1:(imode)*3*NUM_ATOMS_UCELL);
    eigvec(:,(imode-1)*6+4) =...
        dummy{5}((imode-1)*3*NUM_ATOMS_UCELL+1:(imode)*3*NUM_ATOMS_UCELL);
    eigvec(:,(imode-1)*6+5) =...
        dummy{6}((imode-1)*3*NUM_ATOMS_UCELL+1:(imode)*3*NUM_ATOMS_UCELL);
    eigvec(:,(imode-1)*6+6) =...
        dummy{7}((imode-1)*3*NUM_ATOMS_UCELL+1:(imode)*3*NUM_ATOMS_UCELL);
    end
else
%Put Real and Imag in right place		
eigvec = zeros(3*NUM_ATOMS_UCELL,3*NUM_ATOMS_UCELL); 
    for imode = 1:(3*NUM_ATOMS_UCELL/3)
        eigvec(:,(imode-1)*3+1) =...
            dummy{2}((imode-1)*3*NUM_ATOMS_UCELL+1:(imode)*3*NUM_ATOMS_UCELL)...
            + i*dummy{3}((imode-1)*3*NUM_ATOMS_UCELL+1:(imode)*3*NUM_ATOMS_UCELL);
        eigvec(:,(imode-1)*3+2) =...
            dummy{4}((imode-1)*3*NUM_ATOMS_UCELL+1:(imode)*3*NUM_ATOMS_UCELL)...
            + i*dummy{5}((imode-1)*3*NUM_ATOMS_UCELL+1:(imode)*3*NUM_ATOMS_UCELL);
        eigvec(:,(imode-1)*3+3) =...
            dummy{6}((imode-1)*3*NUM_ATOMS_UCELL+1:(imode)*3*NUM_ATOMS_UCELL)...
            + i*dummy{7}((imode-1)*3*NUM_ATOMS_UCELL+1:(imode)*3*NUM_ATOMS_UCELL);
    end
end

    
end
