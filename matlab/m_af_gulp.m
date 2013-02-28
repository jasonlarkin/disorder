clear
path(path,'/home/jason/disorder/matlab/');
lj = m_lj; constant = m_constant;
str.main = '/home/jason/disorder2/lj/amor/4x/prepare/tmp/';
str.matlab = '/home/jason/disorder/matlab/';
str.gin = 'gulp_af_lj.tmp';
%--------------------------------------------------------------------------
%x0
%--------------------------------------------------------------------------
x0 = m_x0_read([str.main 'x0K_1.data']);
%--------------------------------------------------------------------------
%gulp
%--------------------------------------------------------------------------
orig(1).str = 'MASS';
change(1).str = num2str(lj.mass_au);
orig(2).str = 'LX';
change(2).str = num2str(x0.Lx*lj.sigma/constant.ang2m);
orig(3).str = 'LY';
change(3).str = num2str(x0.Ly*lj.sigma/constant.ang2m);
orig(4).str = 'LZ';
change(4).str = num2str(x0.Lz*lj.sigma/constant.ang2m);
orig(5).str = 'RUNPATH';
change(5).str = str.main;

m_change_file_strings(...
    [str.matlab str.gin],...
    orig,...
    [str.main 'gulp.gin'],...
    change)
clear orig change
          
formats = '%2.6f';

output = [x0.id x0.m nmd.x0.x nmd.x0.y nmd.x0.z];
str.write=...
    [str.main 'gulp.gin'];









