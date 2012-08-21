function freq = gulp_grep_freq( gulp , x0 )
%--------------------------------------------------------------------------
%This is required to get the kpt + dk to properly input to GULP
    format long
%--------------------------------------------------------------------------

str.cmd = [gulp.path '/' gulp.exe ' disp disp']; system(str.cmd);

str.cmd

%grep out frequencies
     str.cmd = 'grep "Frequency  " disp.gout > freq_grep.dat';            
     system(str.cmd);
     str.cmd = 'sed "s/Frequency  //g" freq_grep.dat > freq_grep2.dat';            
     system(str.cmd);
%read in freq to sort properly		
    str.read=strcat('freq_grep2.dat');
    fid=fopen(str.read);
    dummy = textscan(fid,'%f%f%f','Delimiter','\t','commentStyle', '--'); 
    fclose(fid);
%remove temporary
system('rm freq_grep.dat');
system('rm freq_grep2.dat');
    
for imode = 1:(3*size(x0.ucell.cart,1)/3)
    freq((imode-1)*3+1) =...
        dummy{1}(imode)*x0.constant.c*2*pi*x0.LJ.tau;
    freq((imode-1)*3+2) =...
        dummy{2}(imode)*x0.constant.c*2*pi*x0.LJ.tau;
    freq((imode-1)*3+3) =...
        dummy{3}(imode)*x0.constant.c*2*pi*x0.LJ.tau;
end

end