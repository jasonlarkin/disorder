function x0 = m_lj_alloy_create( x0 )

    x0 = m_lj_unit_cell( x0 );
    x0.pos =...
        m_build_supercell(...
    x0.ucell.cart, x0.latvec, x0.Nx, x0.Ny, x0.Nz );
        
    [x0.id,x0.m] =...
        m_create_binaryalloy( x0.mass, x0.pos, x0.alloy );

end