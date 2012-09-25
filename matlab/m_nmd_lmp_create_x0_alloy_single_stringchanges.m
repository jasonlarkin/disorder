function NMD = nmd_lmp_create_x0_alloy_single_stringchanges( NMD )

NMD.str.x0.file = 'lmp.x0.alloy.single.temp';

NMD.str.x0.orig(1).str = 'NUM_ATOMS';
NMD.str.x0.change(1).str = int2str(size(NMD.x0.pos,1));
NMD.str.x0.orig(2).str = 'NUM_ATOMS_TYPE';
NMD.str.x0.orig(3).str = 'LX';
NMD.str.x0.orig(4).str = 'LY';
NMD.str.x0.orig(5).str = 'LZ';
NMD.str.x0.orig(6).str = 'ATOM_MASS_1';






end