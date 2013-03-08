clear
path(path,'/home/jason/disorder/matlab/');
lj = m_lj; constant = m_constant;
str.main = '/home/jason/disorder2/si/amor/prepare/6x/annealDonadio/';
str.matlab = '/home/jason/disorder/matlab/';
str.gin = 'gulp_af_si.tmp';
%--------------------------------------------------------------------------
%x0
%--------------------------------------------------------------------------
x0 = m_x0_read([str.main 'x0_emin_1.data']);
%--------------------------------------------------------------------------
%gulp
%--------------------------------------------------------------------------
orig(1).str = 'MASS';
change(1).str = num2str(lj.mass_au);
orig(2).str = 'LX';
change(2).str = num2str(x0.Lx);
orig(3).str = 'LY';
change(3).str = num2str(x0.Ly);
orig(4).str = 'LZ';
change(4).str = num2str(x0.Lz);
orig(5).str = 'RUNPATH';
change(5).str = str.main;
orig(6).str = 'KPT';
change(6).str = '0.0 0.0 0.0';

m_change_file_strings(...
    [str.matlab str.gin],...
    orig,...
    [str.main 'gulp.gin'],...
    change)
clear orig change
for itype = 1:length(x0.x)
a_type(itype).str = 'Si';
end
coords = m_gulp_coord2gulp( x0 , a_type );
dlmwrite([str.main 'gulp.gin'],...
        coords ,'-append','delimiter','');










