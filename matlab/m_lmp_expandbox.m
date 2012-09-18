function m_lmp_expandbox(str,name)
%m_lmp_expandbox(str,name)
%-------------------------------------------------------------------------- 

format long

str.main = str;
str.matlab = '/home/jason/disorder/matlab/';

cd(str.main);

x0 = m_x0_read(str.main,name);

m_x0_lmp_write_lj(x0,...
	str.matlab, 'lmp.in.x0.lj.tmp',...
	str.main,'lmp.in.x0');

plot3(x0.x,x0.y,x0.z,'.')
axis( [0 x0.Lx 0 x0.Ly 0 x0.Lz] );
view(2)
%--------------------------------------------------------------------------
pause
%--------------------------------------------------------------------------

str.cmd=...
    ['cp ' str.matlab 'lmp.in.relax.tmp ' str.main 'lmp.in.relax'];
system(str.cmd);

%m_lmp_write_input(str.main,'lmp.in.relax.tmp','lmp.in.relax');

    str.cmd =...
        ['lmp_serial < ' str.main 'lmp.in.relax' ];
    system(str.cmd);
%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------

relax = m_lmp_readdump_one(strcat(str.main,'lmp.dump.pos'),0,3);

relaxu = m_lmp_readdump_one(strcat(str.main,'lmp.dump.posu'),0,3);

plot3(...
    relaxu.atom_data(:,1),relaxu.atom_data(:,2),relaxu.atom_data(:,3),'.')
axis(...
    [relaxu.x_bound(1) relaxu.x_bound(2)...
     relaxu.y_bound(1) relaxu.y_bound(2)...
     relaxu.z_bound(1) relaxu.z_bound(2)]);
view(2)

diff(:,1) =x0.x - relaxu.atom_data(:,1);
diff(:,2) =x0.y - relaxu.atom_data(:,2);
diff(:,3) =x0.z - relaxu.atom_data(:,3);

max(...
    (...
    ( x0.x - relaxu.atom_data(:,1)).^2 +...
    ( x0.y - relaxu.atom_data(:,2)).^2 +...
    ( x0.z - relaxu.atom_data(:,3)).^2 ))

max(...
    (...
    ( x0.x - relax.atom_data(:,1)).^2 +...
    ( x0.y - relax.atom_data(:,2)).^2 +...
    ( x0.z - relax.atom_data(:,3)).^2 ))
%--------------------------------------------------------------------------
%pause
%--------------------------------------------------------------------------

x0_relax = m_x0_from_dump( relax , x0.id, x0.m , x0.NUM_ATOMS_UCELL);

m_x0_write_lj( x0_relax , str.main , 'x0_time_relax.data' );
    
%--------------------------------------------------------------------------	
%pause
%--------------------------------------------------------------------------	

end
