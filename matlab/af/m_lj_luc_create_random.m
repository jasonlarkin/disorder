function x0 = m_lj_luc_create_random( x0 )

%create conv cell as building block
x0.x0 = m_lj_unit_cell( x0 ); 

x0.luc.LJ = x0.x0.LJ; x0.luc.constant = x0.x0.constant;

%create the luc building block from the unit cell above
x0.x0.pos =...
    m_build_supercell(...
    x0.x0.ucell.cart, x0.x0.latvec, x0.Nx, x0.Ny, x0.Nz );
%create the luc supercell from the luc building block
x0.luc.pos =...
    m_build_supercell(...
    x0.x0.pos,...
    bsxfun(@times,x0.x0.latvec,[x0.Nx x0.Ny x0.Nz ]),...    %increase latvec
    x0.luc.Nx/x0.Nx, x0.luc.Ny/x0.Ny, x0.luc.Nz/x0.Nz);
%set unique atom ids
    x0.luc.id = 1:size(x0.luc.pos,1);
%group is how many atoms form the unit cell
    x0.luc.group.size =  size(x0.x0.ucell.cart,1)*x0.Nx*x0.Ny*x0.Nz;
    x0.luc.group.num = size(x0.luc.pos,1)/x0.luc.group.size;
%set initial masses to all 1
    x0.luc.m(1:size(x0.luc.pos,1),1) = 1;
%use rng seed for repeatability
    rng( x0.luc.rand.seed );
%randomize group ids
    x0.luc.group.list = randperm(x0.luc.group.num);
%replace group masses randomly
for igroup=1:x0.luc.group.num
    if x0.luc.group.list(igroup)<=(x0.luc.group.num/2)
    x0.luc.m(...
        (igroup-1)*x0.luc.group.size+1:(igroup)*x0.luc.group.size,1) = ...
        1;
    else
    x0.luc.m(...
        (igroup-1)*x0.luc.group.size+1:(igroup)*x0.luc.group.size,1) = ...
        2;
    end
end

x0.luc.Lx = x0.luc.Nx * x0.x0.latvec(1,1);
x0.luc.Ly = x0.luc.Ny * x0.x0.latvec(2,2);
x0.luc.Lz = x0.luc.Nz * x0.x0.latvec(3,3);

end