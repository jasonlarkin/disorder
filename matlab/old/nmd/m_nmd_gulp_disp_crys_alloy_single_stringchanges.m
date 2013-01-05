function NMD = m_nmd_lmp_create_x0_alloy_single_stringchanges( NMD )

NMD.gulp.file = 'gulp_lj_disp_conv.gin';

NMD.gulp.x0.orig(1).str = 'ATOM_MASS_1';
NMD.gulp.x0.change(1).str = int2str(NMD.x0.mass(1));







end
