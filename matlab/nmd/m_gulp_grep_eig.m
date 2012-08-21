
function eigvec = gulp_grep_eig( gulp , x0)

%--------------------------------------------------------------------------
%This is required to get the kpt + dk to properly input to GULP
    format long
%--------------------------------------------------------------------------

str.cmd = [gulp.path '/' gulp.exe ' disp disp']; system(str.cmd);

str.cmd

%grep out eigenvectors
    eigvec = zeros(3*size(x0.ucell.cart,1),3*size(x0.ucell.cart,1));
    str1 = 'grep -A ';
    str2 = strcat(int2str(3*size(x0.ucell.cart,1)),...
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
    
if gulp.kpt(1) == 0 & gulp.kpt(2) == 0 & gulp.kpt(3) == 0 
%Gamma has only real components	
    eigvec = zeros(3*size(x0.ucell.cart,1),3*size(x0.ucell.cart,1)); 
    for imode = 1:(3*size(x0.ucell.cart,1)/3/2)
    eigvec(:,(imode-1)*6+1) =...
        dummy{2}((imode-1)*3*size(x0.ucell.cart,1)+1:(imode)*3*size(x0.ucell.cart,1));
    eigvec(:,(imode-1)*6+2) =...
        dummy{3}((imode-1)*3*size(x0.ucell.cart,1)+1:(imode)*3*size(x0.ucell.cart,1));
    eigvec(:,(imode-1)*6+3) =...
        dummy{4}((imode-1)*3*size(x0.ucell.cart,1)+1:(imode)*3*size(x0.ucell.cart,1));
    eigvec(:,(imode-1)*6+4) =...
        dummy{5}((imode-1)*3*size(x0.ucell.cart,1)+1:(imode)*3*size(x0.ucell.cart,1));
    eigvec(:,(imode-1)*6+5) =...
        dummy{6}((imode-1)*3*size(x0.ucell.cart,1)+1:(imode)*3*size(x0.ucell.cart,1));
    eigvec(:,(imode-1)*6+6) =...
        dummy{7}((imode-1)*3*size(x0.ucell.cart,1)+1:(imode)*3*size(x0.ucell.cart,1));
    end
else
%Put Real and Imag in right place		
eigvec = zeros(3*size(x0.ucell.cart,1),3*size(x0.ucell.cart,1)); 
    for imode = 1:(3*size(x0.ucell.cart,1)/3)
        eigvec(:,(imode-1)*3+1) =...
            dummy{2}((imode-1)*3*size(x0.ucell.cart,1)+1:(imode)*3*size(x0.ucell.cart,1))...
            + i*dummy{3}((imode-1)*3*size(x0.ucell.cart,1)+1:(imode)*3*size(x0.ucell.cart,1));
        eigvec(:,(imode-1)*3+2) =...
            dummy{4}((imode-1)*3*size(x0.ucell.cart,1)+1:(imode)*3*size(x0.ucell.cart,1))...
            + i*dummy{5}((imode-1)*3*size(x0.ucell.cart,1)+1:(imode)*3*size(x0.ucell.cart,1));
        eigvec(:,(imode-1)*3+3) =...
            dummy{6}((imode-1)*3*size(x0.ucell.cart,1)+1:(imode)*3*size(x0.ucell.cart,1))...
            + i*dummy{7}((imode-1)*3*size(x0.ucell.cart,1)+1:(imode)*3*size(x0.ucell.cart,1));
    end
end

    
end