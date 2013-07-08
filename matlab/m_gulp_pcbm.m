clear
path(path,'/home/jason/disorder/matlab/');
lj = m_lj; constant = m_constant;
str.main = '/home/jason/disorder/pcbm/pcbm_pdb/isolated/';
str.matlab = '/home/jason/disorder/matlab/';
str.gin = 'gulp_af_pcbm.tmp';
%--------------------------------------------------------------------------
%x0
%--------------------------------------------------------------------------
x0 = m_x0_read([str.main 'p1d1_1a1b1c.isolated.data']);
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

olps = m_pcbm_olps;

for itype = 1:length(x0.x)
    
    if x0.m(itype) ==olps.type.CF
        a_type(itype).str = 'C1';
    elseif x0.m(itype) ==olps.type.CA1
        a_type(itype).str = 'C2';
    elseif x0.m(itype) ==olps.type.CA2
        a_type(itype).str = 'C3';
    elseif x0.m(itype) ==olps.type.C
        a_type(itype).str = 'C4';
    elseif x0.m(itype) ==olps.type.CT1
        a_type(itype).str = 'C5';
    elseif x0.m(itype) ==olps.type.CT2
        a_type(itype).str = 'C6';
    elseif x0.m(itype) ==olps.type.CT3
        a_type(itype).str = 'C7';
    elseif x0.m(itype) ==olps.type.O
        a_type(itype).str = 'O1';
    elseif x0.m(itype) ==olps.type.OS
        a_type(itype).str = 'O2';
    elseif x0.m(itype) ==olps.type.HA
        a_type(itype).str = 'H1';
    elseif x0.m(itype) ==olps.type.HC
        a_type(itype).str = 'H2';
    end
end
coords = m_gulp_coord2gulp( x0 , a_type );
dlmwrite([str.main 'gulp.gin'],...
        coords ,'-append','delimiter','');
    