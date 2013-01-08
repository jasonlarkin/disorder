function x0 = m_x0_read(str)
% x0 = m_x0_read(str,name) reads a structure file x0
%--------------------------------------------------------------------------

dummy = load(strcat(str));

    x0.param = dummy(1,:);
    x0.id = dummy(2:size(dummy,1),1);
    x0.m = dummy(2:size(dummy,1),2);
    x0.x = dummy(2:size(dummy,1),3);
    x0.y = dummy(2:size(dummy,1),4);
    x0.z = dummy(2:size(dummy,1),5);
    x0.pos(:,1) = x0.id; x0.pos(:,2) = x0.m; 
    x0.pos(:,3) = x0.x; x0.pos(:,4) = x0.y; x0.pos(:,5) = x0.z;
    
    x0.NUM_ATOMS_UCELL = x0.param(2); x0.NUM_ATOMS = x0.param(1);
    x0.NUM_MODES = x0.param(2)*3;
    
    x0.Lx = x0.param(3); x0.Ly = x0.param(4); x0.Lz = x0.param(5);
    x0.VOLUME = x0.Lx*x0.Ly*x0.Lz;
  
end
%--------------------------------------------------------------------------
