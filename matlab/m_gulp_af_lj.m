clear

str.main = '/home/jason/disorder2/lj/amor/4x/prepare/tmp/';
str.matlab = '/home/jason/disorder/matlab/';
str.gulp = '/home/jason/disorder/matlab/gulp/gulp-4.0.5/Src/';
str.x0 = '/home/jason/disorder2/lj/amor/4x/prepare/tmp/x0K_1.data';

str.gin = 'gulp_af_lj.tmp';
str.gulp.sh = 'gulp.sh.tmp.gilg'; 
str.gulp.submit = 'gulp.submit.sh.tmp.gilg';

gulp.walltime = 72;
gulp.cpu = 1;

lj = m_lj; constant=m_constant;

%--------------------------------------------------------------------------
%x0 load
%--------------------------------------------------------------------------
x0 = m_x0_read(str.x0); x0.atype(1).str = 'Ar1';

%--------------------------------------------------------------------------
%gulp_af_lj.tmp
%--------------------------------------------------------------------------
    str.orig = 'MASS';
    str.change = [num2str(lj.mass_au)];
    str.cmd1 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
    str.orig = 'ALAT';
    str.change = [num2str(x0.Lx*lj.sigma/constant.ang2m)];
    str.cmd2 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
	str.orig = 'KPT';
    str.change = ['0.0 0.0 0.0'];
    str.cmd3 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];

	str.orig = 'RUNPATH';
    str.change = strcat(str.main);
    str.temp = strcat('-e ''s|',str.orig,'|',str.change);
    str.cmd4 = [str.temp '|g'' '];        

    str.cmd5 =...
    [str.matlab str.gin ' > ' str.main 'gulp.gin'];
        
    str.cmd =...
        ['sed ' str.cmd1 str.cmd2 str.cmd3 str.cmd4 str.cmd5 ];       
    system(str.cmd);

gulp.coords = m_gulp_coord2gulp(x0);

str.write = strcat(str.main,'gulp.gin');
dlmwrite(str.write,gulp.coords,'-append','delimiter','');

%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
%gulp.sh
%--------------------------------------------------------------------------

%--------------------------------------------------------------------------
%gulp_submit.sh
%--------------------------------------------------------------------------
system(['cp ' str.matlab str.gulp.submit ' ' str.main 'gulp_submit.sh']);
output =...
        ['qsub -l walltime=' int2str(gulp.walltime)...
        ':00:00 -l nodes=1:ppn=' int2str(gulp.cpu) ' gulp.sh'];
str.write = strcat(str.main,'gulp_submit.sh');
dlmwrite(str.write,output,'-append','delimiter','');





