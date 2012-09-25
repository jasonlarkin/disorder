function freq = gulp_lj_freq(kpt,NUM_ATOMS_UCELL)
%--------------------------------------------------------------------------
%freq = gulp_lj_freq( kpt , NUM_ATOMS_UCELL );
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
%This is required to get the kpt + dk to properly input to GULP
    format long
%--------------------------------------------------------------------------

constant = m_constant;
lj = m_lj;

str_orig = '0.0 0.0 0.0';
str_change =...
strcat( num2str(kpt(1)),'\t',num2str(kpt(2)),'\t',num2str(kpt(3)) );
str_cmd = strcat('sed ''s/',str_orig,'/',str_change,'/g'' BZ.gin > disp.gin');
system(str_cmd);



str.cmd = ['gulp disp disp']; system(str.cmd);


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
    
for imode = 1:(3*NUM_ATOMS_UCELL/3)
    freq((imode-1)*3+1) =...
        dummy{1}(imode)*constant.c*2*pi*lj.tau;
    freq((imode-1)*3+2) =...
        dummy{2}(imode)*constant.c*2*pi*lj.tau;
    freq((imode-1)*3+3) =...
        dummy{3}(imode)*constant.c*2*pi*lj.tau;
end

end
