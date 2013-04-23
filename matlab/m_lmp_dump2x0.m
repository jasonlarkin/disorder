function m_lmp_dump2x0(str_dump,str_dump_name,str_x0_name)
%m_lmp_dump2x0(str_dump,str_dump_name,str_x0,str_x0_name)
%-------------------------------------------------------------------------- 

format long

str_x0 = str_dump;

%str_main = '/home/jason/disorder/lj/amor/4x/XCORR_AF/220/2_10K_relax/time/';

pos = m_lmp_readdump_one(strcat(str_dump,str_dump_name),-1,3);

pos

%dummy = m_lmp_readlog(strcat(str.main,'log_heat_1.lammps'));
%logdata = str2num(dummy.data{2});

%output relaxed
output =...
	[pos.Natoms pos.Natoms...
	 pos.x_bound(2) pos.y_bound(2) pos.z_bound(2)];
x =...
    [ (1:pos.Natoms)' ones(pos.Natoms,1)...
    pos.atom_data(:,1) pos.atom_data(:,2) pos.atom_data(:,3) ];
    
    dlmwrite(strcat(str_x0,str_x0_name),...
        output ,'-append','delimiter',' ');
    dlmwrite(strcat(str_x0,str_x0_name),...
        x ,'-append','delimiter',' ');

plot3(pos.atom_data(:,1),pos.atom_data(:,2),pos.atom_data(:,3),'.')
    
%--------------------------------------------------------------------------	


