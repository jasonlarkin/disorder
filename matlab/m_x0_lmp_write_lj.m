function m_x0_lmp_write_lj(x0,str_mat,str_tmp,str_main,str_name)
% x0 = m_x0_lmp_write_lj(x0,str_mat,str_tmp,str_main,str_name)
%write an x0 file in lammps input format
%--------------------------------------------------------------------------
    
%-------------------------------------------------------------------------- 

%output lammps    
    str.orig = 'NUM_ATOMS';
    str.change = [int2str( x0.NUM_ATOMS )];
    str.cmd1 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
    str.orig = 'LX';
    str.change = [num2str( x0.Lx )];
    str.cmd2 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
    str.orig = 'LY';
    str.change = [num2str( x0.Ly )];
    str.cmd3 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
    str.orig = 'LZ';
    str.change = [num2str( x0.Lz )];
    str.cmd4 = ['-e ''s/\<' str.orig '\>/' str.change '/g'' '];
    
    str.cmd5 =...
        [str_mat str_tmp ' > ' str_main str_name ];
        
    str.cmd = ['sed ' str.cmd1 str.cmd2 str.cmd3 str.cmd4 str.cmd5 ];
        
system(str.cmd);

output = [ x0.id x0.m x0.x x0.y x0.z ];
str.write=...
    [str_main str_name];
dlmwrite(str.write,output,'-append','delimiter','\t');

end
%--------------------------------------------------------------------------	


