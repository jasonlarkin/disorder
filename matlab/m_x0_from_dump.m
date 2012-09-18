
function x0 = m_x0_from_dump( data , id, m , NUM_ATOMS_UCELL)
% x0 = m_x0_from_dump( data , id, m , NUM_ATOMS_UCELL)
%write an x0 file in lammps input format
%--------------------------------------------------------------------------

x0.x = data.atom_data(:,1);
x0.y = data.atom_data(:,2);
x0.z = data.atom_data(:,3);
x0.m = m;
x0.id = id;

    x0.pos(:,1) = x0.id; x0.pos(:,2) = x0.m; 
    x0.pos(:,3) = x0.x; x0.pos(:,4) = x0.y; x0.pos(:,5) = x0.z;
    
    x0.NUM_ATOMS_UCELL = NUM_ATOMS_UCELL; x0.NUM_ATOMS = data.Natoms;
    x0.NUM_MODES = x0.NUM_ATOMS*3;
    
    x0.Lx = data.x_bound(2)-data.x_bound(1);
    x0.Ly = data.y_bound(2)-data.y_bound(1);
    x0.Lz = data.z_bound(2)-data.z_bound(1);
    x0.VOLUME = x0.Lx*x0.Ly*x0.Lz;

end