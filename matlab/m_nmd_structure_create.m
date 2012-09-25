
function NMD = m_nmd_structure_create( NMD )

if NMD.system(1).str == 'LJ'
    if NMD.system(2).str == 'alloy'
        NMD.x0 = m_lj_alloy_create( NMD.x0 );
    elseif NMD.system(2).str == 'superlattice'
    end
elseif NMD.system(1).str == 'Si'
end

NMD.x0.kptlist =...
    m_create_kptlist( NMD.x0.latvec_int, NMD.x0.Nx, NMD.x0.Ny, NMD.x0.Nz );

x0_dummy = NMD.x0;

%output x0 structure
save(strcat(NMD.str.main,'/x0.mat'), '-struct', 'x0_dummy');


end


