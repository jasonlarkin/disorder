function m_gulp_disp( gulp, x0 ) 
%--------------------------------------------------------------------------
gulp.dk = 10E-5;
%--------------------------------------------------------------------------

if exist([NMD.str.main '/eigvec.dat'], 'file')~=0
    system(['rm -f ' str_main '/eigvec.dat']);
    system(['rm -f ' str_main '/freq.dat']);
    system(['rm -f ' str_main '/vel.dat']);
end    

for ikpt=1:size(x0.kptlist,1)       
%only works for orthogonal lattice vector systems!
    gulp.kpt = x0.kptlist(ikpt,:)./[x0.Nx x0.Ny x0.Nz];
    
    gulp = gulp_disp_create_script( gulp , x0 );

eigvec =...
    gulp_grep_eig( gulp );
freq =...
    gulp_grep_freq( gulp );
vel =...
    gulp_grep_vel( gulp );
%Output formatted eigvec		
    str.write=strcat(str_main,'/eigvec.dat');
    dlmwrite(str.write,eigvec,'-append','delimiter',' ');
%Output formatted freqs
    str.write=strcat(str_main,'/freq.dat');
    dlmwrite(str.write,freq,'-append','delimiter',' ');
%Output velocities
    str.write=strcat(str_main,'/vel.dat');
    dlmwrite(str.write,vel,'-append','delimiter',' ');
end

end